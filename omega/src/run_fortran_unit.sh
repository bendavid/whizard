#! /bin/sh
########################################################################
# This script is for developers only and needs not to be portable.
# This script assumes an opam installation with many versions of
# O'Caml available as switches.
########################################################################
# tl;dr : don't try this at home, kids ;)
########################################################################

src=$(dirname $(realpath $0))
tmp=$(mktemp -d)

trap "rm -fr $tmp" 0 1 2 3 15

cd $tmp || exit 2

cp -a \
   $src/fortran_unit.ml \
   $src/format_Fortran.mli $src/format_Fortran*.ml \
   $src/OUnit.mli $src/OUnit.ml \
   .

compile_and_run () {
  switch=$1
  tag=$2
  flags="-w -D $3"
  opam switch $switch >/dev/null || exit 2
  opam switch show
  eval $(opam env)
  rm -f fortran_unit *.o *.cm[iox]
  ocamlopt OUnit.mli format_Fortran.mli
  ocamlopt $flags -o fortran_unit -I $src unix.cmxa \
    OUnit.ml format_Fortran$tag.ml fortran_unit.ml
  ./fortran_unit # -verbose
}

### Here we will loop over compiler/library versions
compile_and_run 4.02.3 "" -safe-string
compile_and_run 4.03.0 "" -safe-string
compile_and_run 4.05.0 "" -safe-string
compile_and_run 4.06.1 "" -safe-string
compile_and_run 4.07.1 "" -safe-string
compile_and_run 4.08.0 "" -safe-string

exit 0
