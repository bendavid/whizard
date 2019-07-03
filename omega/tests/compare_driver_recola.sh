#! /bin/sh
# compare_driver_UFO.sh --
########################################################################

omega_template="$1"
shift 1

models="SM_Higgs"

modules=""

########################################################################
########################################################################
########################################################################

while read module threshold abs_threshold n roots model mode process; do

  case $module in

   '#'*) # skip comments
     ;;

   '')   # skip empty lines
     ;;

   '!'*) break
     ;;

    *)
      ########################################################################
      modules="$modules $module"
      eval threshold_$module=$threshold
      eval abs_threshold_$module=$abs_threshold
      eval n_$module=$n
      eval roots_$module=$roots
      eval process_$module="'$process'"
      flavors_omega="`echo \"$process\" | sed 's/ *|.*$//'`"
      flavors_recola="`echo \"$process\" | sed 's/^.*| *//'`"
      eval process_recola_$module="'$flavors_recola'"
      ######################################################################
      omega="`echo $omega_template | sed s/%%%/$model/g`"
      $omega  "$@" \
        -target:parameter_module parameters_${model}_recola \
        -target:module amplitude_compare_recola_${module} \
        -$mode "$flavors_omega" 2>/dev/null 
    ;;
  esac

done

for module in $modules; do

cat <<EOF
module interface_compare_recola_${module}
  use omega_interface
  use amplitude_compare_recola_${module}
  implicit none
  private
  public :: load
contains
  function load () result (p)
    type(omega_procedures) :: p
    p%number_particles_in => number_particles_in
    p%number_particles_out => number_particles_out
    p%number_spin_states => number_spin_states
    p%spin_states => spin_states
    p%number_flavor_states => number_flavor_states
    p%flavor_states => flavor_states
    p%number_color_indices => number_color_indices
    p%number_color_flows => number_color_flows
    p%color_flows => color_flows
    p%number_color_factors => number_color_factors
    p%color_factors => color_factors
    p%color_sum => color_sum
    p%new_event => new_event
    p%reset_helicity_selection => reset_helicity_selection
    p%is_allowed => is_allowed
    p%get_amplitude => get_amplitude
  end function load
end module interface_compare_recola_${module}

EOF

done

########################################################################

cat <<EOF
program compare_recola
  use kinds
  use recola
  use compare_lib_recola
EOF

for module in $modules; do
cat <<EOF
  use interface_compare_recola_${module}, load_${module} => load
EOF
done

for model in $models; do
cat <<EOF
  use parameters_${model}_recola, init_parameters_$model => init_parameters
EOF
done

cat <<EOF
  implicit none

  real(double) :: asq_sum_recola, asq_sum_omega
  integer :: failures, attempts, seed
  integer :: failed_processes, attempted_processes
  integer, dimension(8) :: date_time

  call date_and_time (values = date_time)
  seed = product (date_time)
  seed = 24

  failed_processes = 0
  attempted_processes = 0

EOF

for model in $models; do
cat <<EOF
  call init_parameters_${model} ()
EOF
done

cat <<EOF
  call set_recola_parameters_from_omega

EOF

for module in $modules; do

eval process="\${process_$module}"
eval n="\${n_$module}"
eval threshold="\${threshold_$module}"
eval abs_threshold="\${abs_threshold_$module}"
eval roots="\${roots_$module}"
eval flavors_recola="\${process_recola_$module}"

cat <<EOF
  print *, "checking process '$process'"
  call check_recola (load_$module (), '$flavors_recola', &
                     roots = real ($roots, kind=default), &
                     threshold = real ($threshold, kind=default), &
                     n = $n, seed = seed, &
                     abs_threshold = real ($abs_threshold, kind=default), &
                     failures = failures, attempts = attempts)
  if (failures > 0) then
     print *, failures, 'failures in', $n, 'attempts'
     failed_processes = failed_processes + 1
  end if

EOF
done

cat <<EOF
  if (failed_processes > 0) then
     print *, failed_processes, " failed processes in ", attempted_processes, " attempts"
     stop 1
  end if
end program compare_recola
EOF

exit 0

