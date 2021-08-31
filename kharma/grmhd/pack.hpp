/* 
 *  File: pack.hpp
 *  
 *  BSD 3-Clause License
 *  
 *  Copyright (c) 2020, AFD Group at UIUC
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  
 *  1. Redistributions of source code must retain the above copyright notice, this
 *     list of conditions and the following disclaimer.
 *  
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *  
 *  3. Neither the name of the copyright holder nor the names of its
 *     contributors may be used to endorse or promote products derived from
 *     this software without specific prior written permission.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#pragma once

#include "grmhd.hpp"

using namespace parthenon;

namespace GRMHD {

inline VariablePack<Real> PackMHDPrims(MeshBlockData<Real> *rc, PackIndexMap& prims_map, bool coarse=false)
{
    auto pmb = rc->GetBlockPointer();
    MetadataFlag isPrimitive = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("PrimitiveFlag");
    MetadataFlag isMHD = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("MHDFlag");
    return rc->PackVariables({isPrimitive, isMHD}, prims_map, coarse);
}

inline VariablePack<Real> PackMHDCons(MeshBlockData<Real> *rc, PackIndexMap& cons_map, bool coarse=false)
{
    auto pmb = rc->GetBlockPointer();
    MetadataFlag isMHD = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("MHDFlag");
    return rc->PackVariables({Metadata::Conserved, isMHD}, cons_map, coarse);
}

inline VariablePack<Real> PackHDPrims(MeshBlockData<Real> *rc, PackIndexMap& prims_map, bool coarse=false)
{
    auto pmb = rc->GetBlockPointer();
    MetadataFlag isPrimitive = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("PrimitiveFlag");
    MetadataFlag isHD = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("HDFlag");
    return rc->PackVariables({isPrimitive, isHD}, prims_map, coarse);
}

inline VariablePack<Real> PackHDCons(MeshBlockData<Real> *rc, PackIndexMap& cons_map, bool coarse=false)
{
    auto pmb = rc->GetBlockPointer();
    MetadataFlag isHD = pmb->packages.Get("GRMHD")->Param<MetadataFlag>("HDFlag");
    return rc->PackVariables({Metadata::Conserved, isHD}, cons_map, coarse);
}


} // namespace GRMHD