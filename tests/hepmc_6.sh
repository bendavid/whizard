#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
if test -f OCAML_FLAG -a -f HEPMC_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging --model QED
    echo "Output from running ${s}_rd:" > ${s}_rd.log
    LD_LIBRARY_PATH=@HEPMC_LD_LIBRARY_PATH@ ./${s}_rd >> ${s}_rd.log
    diff ref-output/$s.ref ${s}_rd.log
else
    echo "|=============================================================================|"
    echo "No HepMC available, test skipped"
    exit 77
fi

