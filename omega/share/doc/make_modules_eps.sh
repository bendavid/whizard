#! /bin/sh
# make_modules_eps.sh --

root=`pwd`

build_dir=$root/_build/src
src_dir=$root/src
doc_dir=$root/share/doc

modules="bundle.ml bundle.mli cache.ml
  cache.mli cascade.ml cascade.mli cascade_syntax.ml
  cascade_syntax.mli color.ml color.mli colorize.ml colorize.mli
  combinatorics.ml combinatorics.mli config.mli
  coupling.mli DAG.ml DAG.mli fusion.ml fusion.mli linalg.ml
  linalg.mli model.mli modellib_BSM.ml modellib_BSM.mli
  modellib_MSSM.ml modellib_MSSM.mli modellib_SM.ml modellib_SM.mli
  modellib_NoH.ml modellib_NoH.mli
  modeltools.ml modeltools.mli momentum.ml momentum.mli omega.ml
  omega.mli omega_MSSM.ml omega_Littlest.ml omega_QED.ml omega_SM.ml
  options.ml options.mli partition.ml partition.mli pmap.ml pmap.mli
  powSet.ml powSet.mli process.ml process.mli product.ml product.mli
  progress.ml progress.mli target.mli targets.ml
  targets.mli thoFilename.ml thoFilename.mli
  thoList.ml thoList.mli thoString.ml thoString.mli topology.ml
  topology.mli tree.ml tree.mli tree2.ml tree2.mli
  tuple.ml tuple.mli"

(cd $src_dir && ocamldoc -dot -dot-reduce -dot-colors lightgray \
   -o /tmp/modules.dot -I $build_dir $modules)
dot /tmp/modules.dot  -Tps >$doc_dir/modules.eps
rm -f /tmp/modules.dot
