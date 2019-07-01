#!/bin/sh
### Check WHIZARD module sm_physics
echo "Running script $0"
exec ./run_whizard.sh @script@ --no-model --no-library --check sm_qcd
