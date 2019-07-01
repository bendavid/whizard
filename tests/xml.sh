#!/bin/sh
### Check WHIZARD module xml
echo "Running script $0"
exec ./run_whizard.sh @script@ --no-model --no-library --check xml
