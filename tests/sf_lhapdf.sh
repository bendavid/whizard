#!/bin/sh
### Check WHIZARD module sf_base
echo "Running script $0"
if test -f LHAPDF_FLAG; then
    exec ./run_whizard.sh @script@ --check sf_lhapdf
else
    echo "|=============================================================================|"
    echo "No LHAPDF available, test skipped"
    exit 77
fi
