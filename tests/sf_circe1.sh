#!/bin/sh
### Check WHIZARD module sf_circe1
echo "Running script $0"
#if test -f CIRCE1_FLAG; then
    exec ./run_whizard.sh @script@ --check sf_circe1
#else
#    echo "|====================================================================#=========|"
#    echo "CIRCE1 not available, test skipped"
#    exit 77
#fi
