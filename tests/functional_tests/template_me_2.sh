#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
rm -f @script@_lib.* @script$_p?.*
./run_whizard.sh @script@ --no-logging
diff ref-output/`basename @script@`.ref `basename @script@`.log
