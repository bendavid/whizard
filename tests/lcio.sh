#!/bin/sh
### Check WHIZARD lcio module setup
echo "Running script $0"
if test -f LCIO_FLAG; then
    exec ./run_whizard.sh @script@ --check lcio
else
    echo "|=============================================================================|"
    echo "No LCIO available, test skipped"
    exit 77
fi
