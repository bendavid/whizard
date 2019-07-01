#!/bin/sh
### Check WHIZARD phase space generator for FKS regions
echo "Running script $0"
exec ./run_whizard.sh @script@ --check phs_fks_generator
