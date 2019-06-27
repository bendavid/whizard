dnl f90.m4 --
divert(-1)dnl
undefine(`eval')
define(`pure',`')
define(`elemental',`')
define(`_specific_sv',`private :: $1_s, $1_v')
define(`_interface_sv',`interface $1
     module procedure $1_s, $1_v
  end interface`'define($1,$1_s)')
define(`_specific_sva',`private :: $1_s, $1_v, $1_a')
define(`_interface_sva',`interface $1
     module procedure $1_s, $1_v, $1_a
  end interface`'define($1,$1_s)')
define(`_begin_f90',`dnl')
define(`_end_f90',`dnl')
divert`'dnl
