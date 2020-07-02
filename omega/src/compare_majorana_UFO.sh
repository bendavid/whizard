#! /bin/sh
########################################################################
# This script is for developers only and needs not to be portable.
# This script takes TO's directory structure for granted.
########################################################################
# tl;dr : don't try this at home, kids ;)
########################################################################

case "$#" in
    2) mode="$1"
       process="$2"
       ;;
    *) echo "usage: $0 [-scatter|-decay] process" 1>&2
       exit 2
       ;;
esac
  
jobs=12
width=1000
width=80

root=$HOME/physics/whizard
build=$root/_build/default

OCAMLFLAGS="-w -D -warn-error +P"
make OCAMLFLAGS="$OCAMLFLAGS" -j $jobs -C $build/omega/src || exit 1
make -j $jobs -C $build/omega/bin \
   omega_UFO.opt omega_UFO_Majorana.opt || exit 1

omega_dirac="$build/omega/bin/omega_UFO.opt -model:exec -target:width $width"
omega_majorana="$build/omega/bin/omega_UFO_Majorana.opt -model:Majorana -model:exec -target:width $width"

$omega_dirac "$mode" "$process" > omega_amplitude_dirac.f90 2>/dev/null
$omega_majorana "$mode" "$process" > omega_amplitude_majorana.f90 2>/dev/null

if   grep -q 'integer, parameter :: n_prt = 0' omega_amplitude_dirac.f90; then
  echo "O'Mega Dirac empty: $mode $process" 1>&2;
elif grep -q 'integer, parameter :: n_prt = 0' omega_amplitude_majorana.f90; then
  echo "O'Mega Majorana empty: $mode $process" 1>&2;
else
  diff -u omega_amplitude_dirac.f90 omega_amplitude_majorana.f90
fi
