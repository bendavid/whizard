#!/bin/sh
### Check WHIZARD module sf_circe2
echo "Running script $0"
#if test -f CIRCE2_FLAG; then
    exec ./run_whizard.sh @script@ --check sf_circe2
#else
#    echo "|====================================================================#=========|"
#    echo "CIRCE2 not available, test skipped"
#    exit 77
#fi
