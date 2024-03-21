# Macbook with intel CPU and no GPU
if [[ $(hostname -f) == "haruhi.local" ]]; then
  echo "We are on haruhi"
  
  C_NATIVE=gcc
  CXX_NATIVE=g++

  NPROC=4
#  HOST_ARCH="ICL"

  MPI_EXE="mpirun"
fi

if [[ $(hostname -f) == "mgmt" ]]; then
  echo "We are on Astro"

fi
