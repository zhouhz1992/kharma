#
#  This file sets the KHARMA_PATH environment variable if it
#  is not set yet, and adds paths to PATH.
#

if [ -z $KHARMA_PATH ]; then
  export KHARMA_PATH=`echo $PWD`
fi

export PATH=${PATH}:$KHARMA_PATH/utils
