#!/bin/sh
### Check WHIZARD event setup
echo "Running script $0"
exec ./run_whizard.sh @script@ --check subevt_expr

