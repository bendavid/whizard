#!/bin/sh
# Check stable result of ee -> bbbb at NLO
# time ~
echo "Running script $0"

name=`basename @script@`

if test -f ref-output/$name.ref; then
  if test -f OCAML_FLAG -a -f OPENLOOPS_FLAG; then
    ./run_whizard.sh @script@ --no-logging
    mv $name.log $name.log.tmp
    cat $name.log.tmp | sed -e 's/Loading library:.*/Loading library: [...]/' > $name.log
    rm $name.log.tmp
    diff ref-output/$name.ref $name.log
    diffrc=$?
    grep --quiet "\[OpenLoops\] Requested library not installed." $name.run.log
    greprc=$?
    if test $diffrc -gt 0 -a $greprc -eq 0; then
      echo "|=============================================================================|"
      echo "OpenLoops process library missing"
      exit 77
    fi
    exit $diffrc
  else
    echo "|=============================================================================|"
    echo "No O'Mega/OpenLoops matrix elements available"
    exit 77
  fi
else
  echo "|=============================================================================|"
  echo "$name.ref not found"
  exit 1
fi
