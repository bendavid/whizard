#!/bin/sh
### Check WHIZARD: process library handling (static case)
echo "Running script $0"
./run_whizard.sh @script@
exec ./whizard.static --logfile `basename @script@`2.log @script@2.sin


