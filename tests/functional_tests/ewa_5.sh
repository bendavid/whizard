#!/bin/sh
### Check WHIZARD/EWA setup
echo "Running script $0"
if test -f OCAML_FLAG; then
    script=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model SM
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
