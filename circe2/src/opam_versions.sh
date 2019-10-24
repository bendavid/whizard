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
  cd $root
  ./build_master.sh CIRCE2
  rm -fr $build/$switch
  mkdir -p $build/$switch
  cd $build/$switch
  $root/configure --enable-distribution --disable-static
  make -j $(getconf _NPROCESSORS_ONLN) && \
  make -j $(getconf _NPROCESSORS_ONLN) check && \
  make -j $(getconf _NPROCESSORS_ONLN) dist && \
  make -j $(getconf _NPROCESSORS_ONLN) distcheck
  if [ "$?" = 0 ]; then
    echo "$switch PASS" >> $log
  else
    echo "$switch FAIL" >> $log
  fi
done

for dist_switch in $versions; do
  for build_switch in $versions; do
    cd $build/$dist_switch
    rm -fr whizard_circe2-[0-9].[0-9].[0-9]
    tar xzf whizard_circe2-[0-9].[0-9].[0-9].tar.gz
    cd whizard_circe2-[0-9].[0-9].[0-9]
    opam switch $build_switch >/dev/null || exit 2
    echo "Building with $(opam switch show) in $(pwd)"
    eval $(opam env)
    $root/configure --enable-distribution --disable-static
    make -j $(getconf _NPROCESSORS_ONLN) && \
    make -j $(getconf _NPROCESSORS_ONLN) check
    if [ "$?" = 0 ]; then
      echo "build: $build_switch dist: $dist_switch PASS" >> $log
    else
      echo "build: $build_switch dist: $dist_switch FAIL" >> $log
    fi
  done
done
