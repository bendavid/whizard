#!/bin/sh
### Check WHIZARD eio_hepmc module setup
echo "Running script $0"
if test -f HEPMC_FLAG; then
    exec ./run_whizard.sh @script@ --check eio_hepmc
else
    echo "|=============================================================================|"
    echo "No HepMC available, test skipped"
    exit 77
fi
