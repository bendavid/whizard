#!/bin/sh
### Check WHIZARDs initial state shower/matching
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG; then
    ./run_whizard.sh @script@ --no-logging
    script=`basename @script@`
    ### Event files not compared, numerical noise!
    # echo "Contents of ${script}_1.evt:" >> $script.log
    # cat ${script}_1.evt >> $script.log
    # echo "Contents of ${script}_2.evt:" >> $script.log
    # cat ${script}_2.evt >> $script.log
    diff ref-output/$script.ref $script.log

else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available and/or PYTHIA6 disabled, test skipped"
    exit 77
fi
