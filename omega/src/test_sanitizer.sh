#! /bin/sh
########################################################################
# This script is for developers only and needs not to be portable.
# This script takes TO's directory structure for granted.
########################################################################
# tl;dr : don't try this at home, kids ;)
########################################################################

sanitizer=$HOME/physics/whizard/_build/default/omega/scripts/ufo-sanitizer
omega=$HOME/physics/whizard/_build/default/omega/bin/omega_UFO.opt

if [ -z "$1" ]; then
    echo "usage: $0 UFO_dir" 1>&2
    exit 1
fi

ufodir="$1"
outdir="$ufodir.sanitized"
shift

python "$sanitizer" "$ufodir" "$outdir" || \
    python3 "$sanitizer" "$ufodir" "$outdir"

if [ -z "$1" ]; then
    diff -r -uwbBI '^\(#\|import  \+configuration\)' -x '*.pyc' \
	 "$ufodir" "$outdir"
else
    $omega -model:UFO_dir $ufodir  -model:exec -scatter "$1" > orig.f90
    $omega -model:UFO_dir $outdir  -model:exec -scatter "$1" > sanitized.f90
    diff -u orig.f90 sanitized.f90
fi
