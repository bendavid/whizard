#!/bin/sh
### Check WHIZARD: different offshell strategies in threshold
echo "Running script $0"
if test -f OCAML_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    rc=$?; if [ $rc != 0 ]; then exit $rc; fi
    diff ref-output/${s}.ref ${s}.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
