#!/bin/sh
### Check WHIZARD: process library handling
echo "Running script $0"
if test -f OCAML_FLAG; then
    ./run_whizard.sh @script@ -u --user-src=user_strfun --user-target=user_strfun --no-logging
    diff ref-output/`basename @script@`.ref `basename @script@`.log
else
    echo "|=============================================================================|"
    echo "No O'Mega matrix elements available, test skipped"
    exit 77
fi    



