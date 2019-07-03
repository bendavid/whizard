#! /bin/sh

jobs=12

UFO_SM=$HOME/physics/SM/
UFO_SMEFT=$HOME/physics/SMEFT_mW_UFO/
UFO_SMEFT=$HOME/physics/SMEFTsim_A_U35_alphaScheme_UFO_v2_1/

root=$HOME/physics/whizard
build=$root/_build

case X"$1" in
   X"-SM")    UFO=$UFO_SM;    shift;;
   X"-SMEFT") UFO=$UFO_SMEFT; shift;;
   *)         UFO=$UFO_SM;;
esac

OCAMLFLAGS="-w -D -warn-error +P"
make OCAMLFLAGS="$OCAMLFLAGS" -j $jobs -C $build/omega/src || exit 1
make -j $jobs -C $build/omega/bin omega_UFO.opt || exit 1
$build/omega/bin/omega_UFO.opt -model:UFO_dir $UFO -model:dump -model:exec "$@"
