// Functions defining the evolution of GRMHD fluid

#include <memory>

// Until Parthenon gets a reduce()
#include "Kokkos_Core.hpp"

#include "mesh/mesh.hpp"
#include "coordinates/coordinates.hpp"

#include "decs.hpp"

#include "boundaries.hpp"
#include "coordinate_embedding.hpp"
#include "coordinate_systems.hpp"
#include "debug.hpp"
#include "fluxes.hpp"
#include "grmhd.hpp"
#include "phys.hpp"
#include "source.hpp"
#include "U_to_P.hpp"

using namespace parthenon;

namespace GRMHD
{

/**
 * Declare fields
 *
 * TODO:
 * Check metadata flags, esp ctop
 * Add pflag as an integer "field", or at least something sync-able
 * Split out B fields to option them face-centered
 */
std::shared_ptr<StateDescriptor> Initialize(ParameterInput *pin)
{
    auto fluid_state = std::make_shared<StateDescriptor>("GRMHD");
    Params &params = fluid_state->AllParams();

    // Add the problem name, so we can be C++ noobs and special-case on string contents
    std::string problem_name = pin->GetString("job", "problem_id");
    params.Add("problem", problem_name);

    // TODO do we need this?  Can we set it based on
    params.Add("order", 2);

    // There are only 2 parameters related to fluid evolution:
    // 1. Fluid gamma for EOS (TODO separate EOS class to make this broader)
    // 2. Proportion of courant condition for timesteps
    double gamma = pin->GetOrAddReal("GRMHD", "gamma", 4. / 3);
    params.Add("gamma", gamma);
    double cfl = pin->GetOrAddReal("GRMHD", "cfl", 0.9);
    params.Add("cfl", cfl);
    // Starting/minimum timestep, if something about the sound speed goes wonky
    // Parthenon allows up to 2x higher dt per step, so it climbs to CFL quite fast
    double dt_min = pin->GetOrAddReal("time", "dt_min", 1.e-5);
    params.Add("dt_min", dt_min);

    // Coordinate options for building Grids per-mesh
    // It is probably easier to create a global CoordinateEmbedding pointer
    // TODO defaults should probably be KS
    std::string base_str = pin->GetOrAddString("coordinates", "base", "cartesian_minkowski");
    params.Add("c_base", base_str);
    std::string transform_str = pin->GetOrAddString("coordinates", "transform", "cartesian_null");
    params.Add("c_transform", transform_str);
    GReal startx1 = pin->GetOrAddReal("mesh", "x1min", 0);
    params.Add("c_startx1", startx1);
    GReal a = pin->GetOrAddReal("coordinates", "a", 0.0);
    params.Add("c_a", a);
    GReal hslope = pin->GetOrAddReal("coordinates", "hslope", 0.3);
    params.Add("c_hslope", hslope);
    GReal mks_smooth = pin->GetOrAddReal("coordinates", "mks_smooth", 0.5);
    params.Add("c_mks_smooth", mks_smooth);
    GReal poly_xt = pin->GetOrAddReal("coordinates", "poly_xt", 0.82);
    params.Add("c_poly_xt", poly_xt);
    GReal poly_alpha = pin->GetOrAddReal("coordinates", "poly_alpha", 14.0);
    params.Add("c_poly_alpha", poly_alpha);

    cerr << "GRMHD using " << base_str << " base coordiantes with " << transform_str << " transform" << std::endl;

    // We generally carry around the conserved versions of varialbles, treating them as the fundamental ones
    // However, since most analysis tooling expects the primitives, we *output* those.
    Metadata m;
    std::vector<int> s_vector({3});
    std::vector<int> s_fourvector({4});
    std::vector<int> s_fluid({NPRIM-3});
    std::vector<int> s_prims({NPRIM});
    // TODO arrange names/metadata to more accurately reflect e.g. variable locations and relations
    // for now cells are too darn easy and I don't use any fancy face-centered features
    m = Metadata({m.Cell, m.FillGhost, m.Independent, m.Restart, m.Conserved}, s_prims);
    fluid_state->AddField("c.c.bulk.cons", m, DerivedOwnership::shared);
    // initialize metadata the same but length s_vector
    // fluid_state->AddField("c.c.bulk.cons_B", m, DerivedOwnership::shared);

    m = Metadata({m.Cell, m.Derived, m.OneCopy, m.Graphics, m.Intensive}, s_prims);
    fluid_state->AddField("c.c.bulk.prims", m, DerivedOwnership::shared);
    // metadata!
    // fluid_state->AddField("c.c.bulk.prims_B", m, DerivedOwnership::shared);

    // Max (i.e. positive) sound speed vector.  Easiest to keep here due to needing it for EstimateTimestep
    m = Metadata({m.Cell, m.Derived, m.OneCopy, m.Vector}, s_vector);
    fluid_state->AddField("c.c.bulk.ctop", m, DerivedOwnership::unique);

    // Fluxes. TODO coax parthenon to store these as U.flux[0,1,2]
    m = Metadata({m.Cell, m.Derived, m.OneCopy}, s_prims);
    fluid_state->AddField("c.c.bulk.F1", m, DerivedOwnership::shared);
    fluid_state->AddField("c.c.bulk.F2", m, DerivedOwnership::shared);
    fluid_state->AddField("c.c.bulk.F3", m, DerivedOwnership::shared);
    // TODO Add jcon as an output-only calculation. Is that a thing?


    // Flags for patching inverter errors
    // TODO integer fields in Parthenon? Flags need to be sync'd with FillGhost,
    // and would be nice to include in dumps/restarts as well
    // m = Metadata({m.Cell, m.OneCopy, m.FillGhost, m.Independent, m.Graphics, m.Restart});
    // fluid_state->AddField("bulk.pflag", m, DerivedOwnership::shared);

    fluid_state->FillDerived = GRMHD::FillDerived;
    fluid_state->CheckRefinement = nullptr;
    fluid_state->EstimateTimestep = GRMHD::EstimateTimestep;
    return fluid_state;
}

/**
 * Get the primitive variables, which in Parthenon's nomenclature are "derived"
 * Derived variables are updated before output and during the step so that we can work with them
 */
void FillDerived(Container<Real>& rc)
{
    FLAG("Filling Derived");
    MeshBlock *pmb = rc.pmy_block;

    GridVars U = rc.Get("c.c.bulk.cons").data;
    GridVars P = rc.Get("c.c.bulk.prims").data;

    //GridVars pflag = rc.Get("bulk.pflag").data; // TODO remember to switch to ints
    ParArrayND<int> pflag("pflag", pmb->ncells1, pmb->ncells2, pmb->ncells3);

    // TODO how do I carry this around per-block and just update it when needed?
    // (or if not the Grid, then at least a coordinate system and EOS...)
    Grid G(pmb);
    Real gamma = pmb->packages["GRMHD"]->Param<Real>("gamma");
    EOS* eos = new GammaLaw(gamma);

    // Get the primitives from our conserved versions
    // Note this covers ghost zones!  This is intentional, as primitives in
    // ghost zones are needed for reconstruction
    pmb->par_for("U_to_P", 0, pmb->ncells1-1, 0, pmb->ncells2-1, 0, pmb->ncells3-1,
        KOKKOS_LAMBDA_3D {
            pflag(i, j, k) = U_to_P(G, U, eos, i, j, k, Loci::center, P);
        }
    );
    FLAG("Filled");

#if DEBUG
    // TODO this is actually a lot easier to calculate in the conserved vars,
    // due to not needing metric.  But this was the routine I could copy.
    double maxDivB = max_divb(rc);
    fprintf(stderr, "Maximum divB: %g\n", maxDivB);

    count_print_flags(pmb, pflag);
#endif
}

/**
 * Calculate the LLF fluxes
 */
TaskStatus CalculateFluxes(Container<Real>& rc)
{
    FLAG("Calculating Fluxes");
    MeshBlock *pmb = rc.pmy_block;
    GridVars pl("pl", NPRIM, pmb->ncells1, pmb->ncells2, pmb->ncells3);
    GridVars pr("pr", NPRIM, pmb->ncells1, pmb->ncells2, pmb->ncells3);
    GridVars F1 = rc.Get("c.c.bulk.F1").data;
    GridVars F2 = rc.Get("c.c.bulk.F2").data;
    GridVars F3 = rc.Get("c.c.bulk.F3").data;

    // Reconstruct primitives at left and right sides of faces
    WENO5X1(rc, pl, pr);
    // Calculate flux from values at left & right of face
    LRToFlux(rc, pr, pl, 1, F1);

    WENO5X2(rc, pl, pr);
    LRToFlux(rc, pr, pl, 2, F2);

    WENO5X3(rc, pl, pr);
    LRToFlux(rc, pr, pl, 3, F3);

    // TODO necessary?  Definitely messes with Bondi problem currently, needs nuance
    //FixFlux(rc, F1, F2, F3);

    // Constrained transport for B must be applied after everything, including fixing boundary fluxes
    FluxCT(rc, F1, F2, F3);
    FLAG("Calculated fluxes");

    return TaskStatus::complete;
}

/**
 * Calculate dU/dt from a set of fluxes.
 * Needs prims and cons.flux components filled, by FillDerived and CalculateFluxes respectively
 *
 * @param rc is the current stage's container
 * @param base is the base container containing the global dUdt term
 */
TaskStatus ApplyFluxes(Container<Real>& rc, Container<Real>& dudt)
{
    FLAG("Applying fluxes");
    MeshBlock *pmb = rc.pmy_block;
    auto is = pmb->is, js = pmb->js, ks = pmb->ks;
    auto ie = pmb->ie, je = pmb->je, ke = pmb->ke;
    GridVars U = rc.Get("c.c.bulk.cons").data;
    GridVars F1 = rc.Get("c.c.bulk.F1").data;
    GridVars F2 = rc.Get("c.c.bulk.F2").data;
    GridVars F3 = rc.Get("c.c.bulk.F3").data;
    GridVars P = rc.Get("c.c.bulk.prims").data;

    // TODO *sigh*
    Grid G(pmb);
    Real gamma = pmb->packages["GRMHD"]->Param<Real>("gamma");
    EOS* eos = new GammaLaw(gamma);

    // Unpack a bunch of variables for the kernel below
    auto dUdt = dudt.Get("c.c.bulk.cons").data;
    // We don't otherwise support irregular grids...
    auto dx1v = pmb->pcoord->dx1v;
    auto dx2v = pmb->pcoord->dx2v;
    auto dx3v = pmb->pcoord->dx3v;

    pmb->par_for("uber_diff", is, ie, js, je, ks, ke,
        KOKKOS_LAMBDA_3D {
            // Calculate the source term and apply it in 1 go (since it's stencil-1)
            FourVectors Dtmp;
            Real dU[NPRIM] = {0};
            get_state(G, P, i, j, k, Loci::center, Dtmp);
            get_fluid_source(G, P, Dtmp, eos, i, j, k, dU);

            PLOOP dUdt(p, i, j, k) = (F1(p, i, j, k) - F1(p, i+1, j, k)) / dx1v(i) +
                                    (F2(p, i, j, k) - F2(p, i, j+1, k)) / dx2v(j) +
                                    (F3(p, i, j, k) - F3(p, i, j, k+1)) / dx3v(k) +
                                    dU[p];
        }
    );
    FLAG("Applied");

    return TaskStatus::complete;
}

/**
 * Returns the minimum CFL timestep among all zones in the block,
 * multiplied by a proportion "cfl" for safety.
 *
 * This is just for a particular MeshBlock/package, so don't rely on it
 * Parthenon will take the minimum and put it in pmy_mesh->dt
 */
Real EstimateTimestep(Container<Real>& rc)
{
    FLAG("Estimating timestep");
    MeshBlock *pmb = rc.pmy_block;
    auto is = pmb->is, js = pmb->js, ks = pmb->ks;
    auto ie = pmb->ie, je = pmb->je, ke = pmb->ke;
    auto dx1v = pmb->pcoord->dx1v;
    auto dx2v = pmb->pcoord->dx2v;
    auto dx3v = pmb->pcoord->dx3v;
    GridVector ctop = rc.Get("c.c.bulk.ctop").data;

    // TODO is there a parthenon par_reduce yet?
    double ndt;
    Kokkos::Min<double> min_reducer(ndt);
    Kokkos::parallel_reduce("ndt_min", MDRangePolicy<Rank<3>>({is, js, ks}, {ie+1, je+1, ke+1}),
        KOKKOS_LAMBDA (const int &i, const int &j, const int &k, double &local_min) {
            double ndt_zone = 1 / (1 / (dx1v(i) / ctop(0, i, j, k)) +
                                   1 / (dx2v(j) / ctop(1, i, j, k)) +
                                   1 / (dx3v(k) / ctop(2, i, j, k)));
            if (ndt_zone < local_min) local_min = ndt_zone;
        }
    , min_reducer);

    // Sometimes this is called before ctop is initialized.  Catch weird dts and play it safe.
    if (ndt <= 0.0 || isnan(ndt) || ndt > 1) {
        cerr << "ndt was unsafe: " << ndt << "! Using dt_min" << std::endl;
        ndt = pmb->packages["GRMHD"]->Param<Real>("dt_min");
    } else {
        ndt *= pmb->packages["GRMHD"]->Param<Real>("cfl");
    }
    FLAG("Estimated");
    //fprintf(stderr, "dt = %g\n", ndt);
    return ndt;
}



} // namespace GRMHD