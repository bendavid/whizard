#! /bin/sh
########################################################################
# This script is for developers only and needs not to be portable.
# This script assumes an opam installation with many versions of
# O'Caml available as switches.
########################################################################
# tl;dr : don't try this at home, kids ;)
########################################################################

src=$(dirname $(realpath $0))
root=$(dirname $(dirname $src))
build=$root/_build
log=$src/opam_versions.out

versions="$1"
if [ -z "$versions" ]; then
    versions="$(opam switch -s)"
fi

rm -f $log

for switch in $versions; do
  opam switch $switch >/dev/null || exit 2
  opam switch show
  eval $(opam env)
  mkdir -p $build/$switch
  cd $build/$switch
  if [ ! -e config.status ]; then
    cp -a $build/default/config.status .
    ./config.status --recheck
    ./config.status
  fi
  make -j $(getconf _NPROCESSORS_ONLN) -C omega && \
  make -j $(getconf _NPROCESSORS_ONLN) -C omega check
  if [ "$?" = 0 ]; then
    echo "$switch PASS" >> $log
  else
    echo "$switch FAIL" >> $log
  fi
done
