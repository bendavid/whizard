#!/bin/sh
### Check WHIZARD jets handler
echo "Running script $0"
if test -f FASTJET_FLAG; then
    ./run_whizard.sh @script@ --check jets
else
    echo "|=============================================================================|"
    echo "No FastJet available, test skipped"
    exit 77
fi

