#!/bin/sh
### Check WHIZARD/O'Mega interaction with SUSYHIT
echo "Running script $0"
PRG=susyhit
if (which $PRG >/dev/null 2>&1); then
  exec ./run_whizard.sh @script@
else
  echo "|=============================================================================|"
  echo "$PRG executable not found in PATH, test skipped"
  exit 77
fi
