#! /bin/sh
jobs=12
UFO=$HOME/physics/SM/
root=$HOME/physics/whizard
build=$root/_build

make -j $jobs -C $build/omega/src || exit 1
make -j $jobs -C $build/omega/tests ufo_unit || exit 1
$build/omega/tests/ufo_unit "$@"
