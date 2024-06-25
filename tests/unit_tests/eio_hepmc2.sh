#!/bin/sh
### Check WHIZARD eio_hepmc2 module setup
echo "Running script $0"
if test -f HEPMC2_FLAG; then
    exec ./run_whizard_ut.sh --check eio_hepmc2
else
    echo "|=============================================================================|"
    echo "No HepMC2 available, test skipped"
    exit 77
fi
