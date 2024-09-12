#!/bin/sh
### Check WHIZARD setup for gamma gamma -> low-pT hadrons
echo "Running script $0"
if test -f PYTHIA6_FLAG; then
    ./run_whizard.sh @script@ --no-logging
    script=`basename @script@`
    echo "Contents of ${script}.debug" >> $script.log
    cat ${script}.debug 
    diff ref-output/$script.ref $script.log
else
    echo "|=============================================================================|"
    echo "PYTHIA6 disabled, test skipped"
    exit 77
fi
