#!/bin/sh
### Check WHIZARD for a simple test process
### (This test has no ref file, it just compares event output data)
echo "Running script $0"
if test -f OCAML_FLAG; then
    name=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model QCD
    head -1 ${name}a.mokka.evt | sed -e 's/.* \([0-9].[0-9]\+E+[0-9]\+\)/\1/' > ${name}a.red.evt
    head -1 ${name}b.mokka.evt | sed -e 's/.* \([0-9].[0-9]\+E+[0-9]\+\)/\1/' > ${name}b.red.evt
    grep '^ 1 ' ${name}a.mokka.evt >> ${name}a.red.evt
    grep '^ 1 ' ${name}b.mokka.evt >> ${name}b.red.evt
    diff ${name}a.red.evt ${name}b.red.evt
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi
    
