#!/bin/sh
# time ?
echo "Running script $0"
if test -f OCAML_FLAG -a -f OPENLOOPS_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available"
    exit 77
fi
