#!/bin/sh
### Check WHIZARD eio_lhef module setup
echo "Running script $0"
if test -f STDHEP_FLAG; then
    exec ./run_whizard_ut.sh --check eio_stdhep
else
    echo "|=============================================================================|"
    echo "No StdHEP available, test skipped"
    exit 77
fi

