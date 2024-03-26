# Macbook with M3 chip
if [[ $(hostname -f) == "haruhi.local" ]]; then
  echo "We are on haruhi"
  
  C_NATIVE=gcc
  CXX_NATIVE=g++

  NPROC=4

  MPI_EXE="mpirun"
fi
