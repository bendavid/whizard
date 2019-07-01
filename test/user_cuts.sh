#!/bin/sh
### Check WHIZARD: process library handling
echo "Running script $0"
exec ./run_whizard.sh @script@ -u --user-src=user_cuts --user-target=user_cuts
