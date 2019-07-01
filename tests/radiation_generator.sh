#!/bin/sh
### Check creation of N+1-particle flavor structers
echo "Running script $0"
exec ./run_whizard.sh @script@ --check radiation_generator
