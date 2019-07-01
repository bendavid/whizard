#!/bin/sh
# time ~2 min
echo "Running script $0"
if test -f OCAML_FLAG -a -f PYTHIA6_FLAG -a -f PYTHON_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
    @python_bin@ @share_dir@/check-debug-output-hadro.py ${s}_WHIZARD \
      7000 1>&2 &>> $s.run.log
    rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
    @python_bin@ @share_dir@/check-debug-output-hadro.py ${s}_PYTHIA6 \
      7000 1>&2 &>> $s.run.log
  else
    echo "|=============================================================================|"
    echo "O'Mega, PYTHIA6 and/or Python disabled/not available, test skipped"
    exit 77
fi
