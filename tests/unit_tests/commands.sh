#!/bin/sh
### Check WHIZARD command setup
echo "Running script $0"
if test -f GAMELAN_FLAG; then
    exec ./run_whizard_ut.sh --check commands
else
    echo "|=============================================================================|"
    echo "No MetaPost available, test skipped"
    exit 77
fi
