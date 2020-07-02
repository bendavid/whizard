#! /bin/sh
########################################################################
# This script is for developers only and needs not to be portable.
# This script takes TO's directory structure for granted.
########################################################################
# tl;dr : don't try this at home, kids ;)
########################################################################

jobs=12

UFO_SM=../tests/UFO/SM/
UFO_MSSM=../tests/UFO/MSSM/
UFO_SMEFT=$HOME/physics/SMEFTsim_A_U35_alphaScheme_UFO_v2_1/
UFO_SMEFT=$HOME/physics/SMEFT_mW_UFO/

root=$HOME/physics/whizard
build=$root/_build/default
omega=omega_UFO

case X"$1" in
   X"-SM")    UFO=$UFO_SM;    shift;;
   X"-SMEFT") UFO=$UFO_SMEFT; shift;;
   X"-MSSM")  UFO=$UFO_MSSM;  omega=omega_UFO_Majorana; shift;;
   X"-X")     UFO="$2";       shift 2;;
   *)         UFO=$UFO_SM;;
esac

OCAMLFLAGS="-w -D -warn-error +P"
make OCAMLFLAGS="$OCAMLFLAGS" -j $jobs -C $build/omega/src || exit 1
make -j $jobs -C $build/omega/bin $omega.opt || exit 1
$build/omega/bin/$omega.opt -model:UFO_dir $UFO -model:dump -model:exec "$@"
