#!/bin/sh
# time ~
echo "Running script $0"
if test -f OCAML_FLAG -a -f OPENLOOPS_FLAG; then
    s=`basename @script@`
    ./run_whizard.sh @script@ --no-logging
    rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
    @python_bin@ @share_dir@/compare-integrals.py $s \
      @share_dir@/extra_integration_results.dat &>> $s.run.log
  else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available"
    exit 77
fi
