#TDLI Astro cluster
if [[ $(hostname -f) == "mgmt" ]]; then
  echo "We are on Astro"

  C_NATIVE=gcc
  CXX_NATIVE=g++

  NPROC=1

  MPI_EXE="mpirun"
fi
