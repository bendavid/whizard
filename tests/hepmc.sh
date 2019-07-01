#!/bin/sh
### Check WHIZARD hepmc module setup
echo "Running script $0"
if test -f HEPMC_FLAG; then
    exec ./run_whizard.sh @script@ --check hepmc
else
    echo "|=============================================================================|"
    echo "No HepMC available, test skipped"
    exit 77
fi
