#! /bin/sh -x
########################################################################

# Edited by tests/Makefile using $(SED)

EXOTIC_COLOR_TESTS="%%EXOTIC_COLOR_TESTS%%"
srcdir="%%srcdir%%"
SED="%%SED%%"
OMEGA_UFO="%%OMEGA_UFO%%"
OMEGA_UFO_MAJORANA="%%OMEGA_UFO_MAJORANA%%"
EXOTIC_COLOR_UFO_DIR="%%EXOTIC_COLOR_UFO_DIR%%"

########################################################################

# Run the tests:
for name in $EXOTIC_COLOR_TESTS; do
  file="$srcdir/$name"
  process="`$SED '/^#/d' $file | $SED -n 1p`"
  cascade="`$SED '/^#/d' $file | $SED -n 2p`"
  $SED '/^#/d' $file | $SED -n '3,$p' >$name.expected
  $OMEGA_UFO -model:UFO_dir "$EXOTIC_COLOR_UFO_DIR" -model:exec \
	     -scatter "$process" -cascade "$cascade" -quiet \
      | $SED -n '/flavor combinations/,$p' >$name.result
  diff $name.expected $name.result
  rc=$?
  if test "$rc" -ne 0; then
    exit $rc
  else
    rm -f $name.expected $name.result
  fi
done

exit 0
