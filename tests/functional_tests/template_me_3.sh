#!/bin/sh
### Check WHIZARD for a simple test process
echo "Running script $0"
rm -f @script@_lib.* @script$_p?.*
./run_whizard.sh @script@ --no-logging
name=`basename @script@`
echo "Contents of ${name}_p1.lhe:" >> $name.log
cat ${name}_p1.lhe | sed -e 's/^<generator version=.*$/<generator version=[...]>WHIZARD<\/generator>/' >> $name.log
diff ref-output/$name.ref $name.log
