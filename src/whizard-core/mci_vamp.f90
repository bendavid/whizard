! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module mci_vamp

  use kinds !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_12, FMT_14, FMT_17, FMT_19 !NODEP!
  use diagnostics !NODEP!
  use constants !NODEP!
  use unit_tests
  use md5

  use phs_base
  use rng_base
  use rng_tao
  use mci_base
  
  use vamp !NODEP!
  use exceptions !NODEP!
  
  implicit none
  private

  public :: grid_parameters_t
  public :: history_parameters_t
  public :: mci_vamp_t
  public :: mci_vamp_instance_t
  public :: mci_vamp_test

  type :: grid_parameters_t
     integer :: threshold_calls = 0
     integer :: min_calls_per_channel = 10
     integer :: min_calls_per_bin = 10
     integer :: min_bins = 3
     integer :: max_bins = 20
     logical :: stratified = .true.
     logical :: use_vamp_equivalences = .true.
     real(default) :: channel_weights_power = 0.25_default
     real(default) :: accuracy_goal = 0
     real(default) :: error_goal = 0
     real(default) :: rel_error_goal = 0
   contains
     procedure :: write => grid_parameters_write
  end type grid_parameters_t

  type :: history_parameters_t
     logical :: global = .true.
     logical :: global_verbose = .false.
     logical :: channel = .false.
     logical :: channel_verbose = .false.
   contains
     procedure :: write => history_parameters_write
  end type history_parameters_t

  type :: pass_t
     integer :: i_pass = 0
     integer :: i_first_it = 0
     integer :: n_it = 0
     integer :: n_calls = 0
     integer :: n_bins = 0
     logical :: adapt_grids = .false.
     logical :: adapt_weights = .false.
     logical :: is_final_pass = .false.
     logical :: integral_defined = .false.
     integer, dimension(:), allocatable :: calls
     real(default), dimension(:), allocatable :: integral
     real(default), dimension(:), allocatable :: error
     real(default), dimension(:), allocatable :: efficiency
     type(vamp_history), dimension(:), allocatable :: v_history
     type(vamp_history), dimension(:,:), allocatable :: v_histories
     type(pass_t), pointer :: next => null ()
   contains
     procedure :: final => pass_final
     procedure :: write => pass_write
     procedure :: read => pass_read
     procedure :: write_history => pass_write_history
     procedure :: configure => pass_configure
     procedure :: configure_history => pass_configure_history
     procedure :: update => pass_update
     procedure :: get_integration_index => pass_get_integration_index
     procedure :: get_calls => pass_get_calls
     procedure :: get_integral => pass_get_integral
     procedure :: get_error => pass_get_error
     procedure :: get_efficiency => pass_get_efficiency
  end type pass_t
     
  type, extends (mci_t) :: mci_vamp_t
     logical, dimension(:), allocatable :: dim_is_flat
     type(grid_parameters_t) :: grid_par
     type(history_parameters_t) :: history_par
     integer :: min_calls = 0
     type(pass_t), pointer :: first_pass => null ()
     type(pass_t), pointer :: current_pass => null ()
     type(vamp_equivalences_t) :: equivalences
     logical :: rebuild = .true.
     logical :: check_grid_file = .true.
     logical :: grid_filename_set = .false.
     logical :: negative_weights = .false.
     logical :: verbose = .false.
     type(string_t) :: grid_filename
     character(32) :: md5sum_adapted = ""
   contains
     procedure :: reset => mci_vamp_reset
     procedure :: final => mci_vamp_final
     procedure :: write => mci_vamp_write
     procedure :: write_history_parameters => mci_vamp_write_history_parameters
     procedure :: write_history => mci_vamp_write_history
     procedure :: compute_md5sum => mci_vamp_compute_md5sum
     procedure :: get_md5sum => mci_vamp_get_md5sum
     procedure :: startup_message => mci_vamp_startup_message
     procedure :: record_index => mci_vamp_record_index
     procedure :: set_grid_parameters => mci_vamp_set_grid_parameters
     procedure :: set_history_parameters => mci_vamp_set_history_parameters
     procedure :: set_rebuild_flag => mci_vamp_set_rebuild_flag
     procedure :: set_grid_filename => mci_vamp_set_grid_filename
     procedure :: declare_flat_dimensions => mci_vamp_declare_flat_dimensions
     procedure :: declare_equivalences => mci_vamp_declare_equivalences
     procedure :: allocate_instance => mci_vamp_allocate_instance
     procedure :: add_pass => mci_vamp_add_pass
     procedure :: update_from_ref => mci_vamp_update_from_ref
     procedure :: update => mci_vamp_update
     procedure :: write_grids => mci_vamp_write_grids
     procedure :: read_grids_header => mci_vamp_read_grids_header
     procedure :: read_grids_data => mci_vamp_read_grids_data
     procedure :: read_grids => mci_vamp_read_grids
     procedure :: integrate => mci_vamp_integrate
     procedure :: check_goals => mci_vamp_check_goals
     procedure :: error_reached => mci_vamp_error_reached
     procedure :: rel_error_reached => mci_vamp_rel_error_reached
     procedure :: accuracy_reached => mci_vamp_accuracy_reached
     procedure :: prepare_simulation => mci_vamp_prepare_simulation
     procedure :: generate_weighted_event => mci_vamp_generate_weighted_event
     procedure :: generate_unweighted_event => &
          mci_vamp_generate_unweighted_event
     procedure :: rebuild_event => mci_vamp_rebuild_event
     procedure :: pacify => mci_vamp_pacify
  end type mci_vamp_t
  
  type, extends (vamp_data_t) :: mci_workspace_t
     class(mci_sampler_t), pointer :: sampler => null ()
     class(mci_vamp_instance_t), pointer :: instance => null ()
  end type mci_workspace_t
  
  type, extends (mci_instance_t) :: mci_vamp_instance_t
     type(mci_vamp_t), pointer :: mci => null ()
     logical :: grids_defined = .false.
     logical :: grids_from_file = .false.
     integer :: n_it = 0
     integer :: it = 0
     logical :: pass_complete = .false.
     integer :: n_calls = 0
     integer :: calls = 0
     logical :: it_complete = .false.
     logical :: enable_adapt_grids = .false.
     logical :: enable_adapt_weights = .false.
     logical :: allow_adapt_grids = .false.
     logical :: allow_adapt_weights = .false.
     logical :: negative_weights = .false.
     integer :: n_adapt_grids = 0
     integer :: n_adapt_weights = 0
     logical :: generating_events = .false.
     real(default) :: safety_factor = 1
     type(vamp_grids) :: grids
     real(default) :: g = 0
     real(default), dimension(:), allocatable :: gi
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: efficiency = 0
     real(default), dimension(:), allocatable :: vamp_x
     logical :: vamp_weight_set = .false.
     real(default) :: vamp_weight = 0
     real(default) :: vamp_excess = 0
     logical :: allocate_global_history = .false.
     type(vamp_history), dimension(:), pointer :: v_history => null ()
     logical :: allocate_channel_history = .false.
     type(vamp_history), dimension(:,:), pointer :: v_histories => null ()
   contains
     procedure :: write => mci_vamp_instance_write
     procedure :: write_grids => mci_vamp_instance_write_grids
     procedure :: final => mci_vamp_instance_final
     procedure :: init => mci_vamp_instance_init
     procedure :: new_pass => mci_vamp_instance_new_pass
     procedure :: create_grids => mci_vamp_instance_create_grids
     procedure :: discard_integrals => mci_vamp_instance_discard_integrals
     procedure :: allow_adaptation => mci_vamp_instance_allow_adaptation
     procedure :: adapt_grids => mci_vamp_instance_adapt_grids
     procedure :: adapt_weights => mci_vamp_instance_adapt_weights
     procedure :: sample_grids => mci_vamp_instance_sample_grids
     procedure :: get_efficiency_array => mci_vamp_instance_get_efficiency_array
     procedure :: get_efficiency => mci_vamp_instance_get_efficiency
     procedure :: init_simulation => mci_vamp_instance_init_simulation
     procedure :: final_simulation => mci_vamp_instance_final_simulation
     procedure :: compute_weight => mci_vamp_instance_compute_weight
     procedure :: record_integrand => mci_vamp_instance_record_integrand
     procedure :: get_event_weight => mci_vamp_instance_get_event_weight
     procedure :: get_event_excess => mci_vamp_instance_get_event_excess
  end type mci_vamp_instance_t
  

  interface operator (.matches.)
     module procedure pass_matches
  end interface operator (.matches.)
  interface operator (.matches.)
     module procedure real_matches
  end interface operator (.matches.)

  type, extends (mci_sampler_t) :: test_sampler_1_t
     real(default), dimension(:), allocatable :: x
     real(default) :: val
     integer :: mode = 1
   contains
     procedure :: write => test_sampler_1_write
     procedure :: evaluate => test_sampler_1_evaluate
     procedure :: is_valid => test_sampler_1_is_valid
     procedure :: rebuild => test_sampler_1_rebuild
     procedure :: fetch => test_sampler_1_fetch
  end type test_sampler_1_t

  type, extends (mci_sampler_t) :: test_sampler_2_t
     real(default), dimension(:,:), allocatable :: x
     real(default), dimension(:), allocatable :: f
     real(default) :: val
   contains
     procedure :: write => test_sampler_2_write
     procedure :: compute => test_sampler_2_compute
     procedure :: evaluate => test_sampler_2_evaluate
     procedure :: is_valid => test_sampler_2_is_valid
     procedure :: rebuild => test_sampler_2_rebuild
     procedure :: fetch => test_sampler_2_fetch
  end type test_sampler_2_t

  type, extends (mci_sampler_t) :: test_sampler_3_t
     real(default), dimension(:,:), allocatable :: x
     real(default), dimension(:), allocatable :: f
     real(default) :: val
     real(default) :: a = 1
     real(default) :: b = 1
   contains
     procedure :: write => test_sampler_3_write
     procedure :: compute => test_sampler_3_compute
     procedure :: evaluate => test_sampler_3_evaluate
     procedure :: is_valid => test_sampler_3_is_valid
     procedure :: rebuild => test_sampler_3_rebuild
     procedure :: fetch => test_sampler_3_fetch
  end type test_sampler_3_t


contains
  
  subroutine grid_parameters_write (object, unit)
    class(grid_parameters_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A,I0)") "threshold_calls       = ", &
         object%threshold_calls 
    write (u, "(3x,A,I0)") "min_calls_per_channel = ", &
         object%min_calls_per_channel
    write (u, "(3x,A,I0)") "min_calls_per_bin     = ", &
         object%min_calls_per_bin
    write (u, "(3x,A,I0)") "min_bins              = ", &
         object%min_bins
    write (u, "(3x,A,I0)") "max_bins              = ", &
         object%max_bins
    write (u, "(3x,A,L1)") "stratified            = ", &
         object%stratified
    write (u, "(3x,A,L1)") "use_vamp_equivalences = ", &
         object%use_vamp_equivalences
    write (u, "(3x,A,F10.7)") "channel_weights_power = ", &
         object%channel_weights_power
    if (object%accuracy_goal > 0) then
       write (u, "(3x,A,F10.7)") "accuracy_goal         = ", &
            object%accuracy_goal
    end if
    if (object%error_goal > 0) then
       write (u, "(3x,A,F10.7)") "error_goal            = ", &
            object%error_goal
    end if
    if (object%rel_error_goal > 0) then
       write (u, "(3x,A,F10.7)") "rel_error_goal        = ", &
            object%rel_error_goal
    end if
  end subroutine grid_parameters_write

  subroutine history_parameters_write (object, unit)
    class(history_parameters_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A,L1)") "history(global)       = ", object%global
    write (u, "(3x,A,L1)") "history(global) verb. = ", object%global_verbose
    write (u, "(3x,A,L1)") "history(channels)     = ", object%channel
    write (u, "(3x,A,L1)") "history(chann.) verb. = ", object%channel_verbose
  end subroutine history_parameters_write

  subroutine pass_final (object)
    class(pass_t), intent(inout) :: object
    if (allocated (object%v_history)) then
       call vamp_delete_history (object%v_history)
    end if
    if (allocated (object%v_histories)) then
       call vamp_delete_history (object%v_histories)
    end if
  end subroutine pass_final
  
  subroutine pass_write (object, unit, pacify)
    class(pass_t), intent(in) :: object
    integer, intent(in) :: unit
    logical, intent(in), optional :: pacify
    integer :: u, i
    character(len=7) :: fmt
    call pac_fmt (fmt, FMT_17, FMT_14, pacify)
    u = output_unit (unit)
    write (u, "(3x,A,I0)")  "n_it          = ", object%n_it
    write (u, "(3x,A,I0)")  "n_calls       = ", object%n_calls
    write (u, "(3x,A,I0)")  "n_bins        = ", object%n_bins
    write (u, "(3x,A,L1)")  "adapt grids   = ", object%adapt_grids
    write (u, "(3x,A,L1)")  "adapt weights = ", object%adapt_weights
    if (object%integral_defined) then
       write (u, "(3x,A)")  "Results:  [it, calls, integral, error, efficiency]"
       do i = 1, object%n_it
          write (u, "(5x,I0,1x,I0,3(1x," // fmt // "))") &
               i, object%calls(i), object%integral(i), object%error(i), &
               object%efficiency(i)
       end do
    else
       write (u, "(3x,A)")  "Results: [undefined]"
    end if
  end subroutine pass_write
  
  subroutine pass_read (object, u, n_pass, n_it)
    class(pass_t), intent(out) :: object
    integer, intent(in) :: u, n_pass, n_it
    integer :: i, j
    character(80) :: buffer
    object%i_pass = n_pass + 1
    object%i_first_it = n_it + 1
    call read_ival (u, object%n_it)
    call read_ival (u, object%n_calls)
    call read_ival (u, object%n_bins)
    call read_lval (u, object%adapt_grids)
    call read_lval (u, object%adapt_weights)
    allocate (object%calls (object%n_it), source = 0)
    allocate (object%integral (object%n_it), source = 0._default)
    allocate (object%error (object%n_it), source = 0._default)
    allocate (object%efficiency (object%n_it), source = 0._default)
    read (u, "(A)")  buffer
    select case (trim (adjustl (buffer)))
    case ("Results:  [it, calls, integral, error, efficiency]")
       do i = 1, object%n_it
          read (u, *) &
               j, object%calls(i), object%integral(i), object%error(i), &
               object%efficiency(i)
       end do
       object%integral_defined = .true.
    case ("Results: [undefined]")
       object%integral_defined = .false.
    case default
       call msg_fatal ("Reading integration pass: corrupted file")
    end select
  end subroutine pass_read
  
  subroutine pass_write_history (pass, unit)
    class(pass_t), intent(in) :: pass
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    if (allocated (pass%v_history)) then
       call vamp_write_history (u, pass%v_history)
    else
       write (u, "(1x,A)")  "Global history: [undefined]"
    end if
    if (allocated (pass%v_histories)) then
       write (u, "(1x,A)")  "Channel histories:"
       call vamp_write_history (u, pass%v_histories)
    else
       write (u, "(1x,A)")  "Channel histories: [undefined]"
    end if
  end subroutine pass_write_history
    
  subroutine pass_configure (pass, n_it, n_calls, min_calls, &
       min_bins, max_bins, min_channel_calls)
    class(pass_t), intent(inout) :: pass
    integer, intent(in) :: n_it, n_calls, min_channel_calls
    integer, intent(in) :: min_calls, min_bins, max_bins
    pass%n_it = n_it    
    if (min_calls /= 0) then
       pass%n_bins =  max (min_bins, &
            min (n_calls / min_calls, max_bins))
    else
       pass%n_bins = max_bins
    end if
    pass%n_calls = max (n_calls, max (min_calls, min_channel_calls))
    if (pass%n_calls /= n_calls) then
       write (msg_buffer, "(A,I0)")  "VAMP: too few calls, resetting " &
            // "n_calls to ", pass%n_calls
       call msg_warning ()
    end if
    allocate (pass%calls (n_it), source = 0)
    allocate (pass%integral (n_it), source = 0._default)
    allocate (pass%error (n_it), source = 0._default)
    allocate (pass%efficiency (n_it), source = 0._default)
  end subroutine pass_configure
  
  subroutine pass_configure_history (pass, n_channels, par)
    class(pass_t), intent(inout) :: pass
    integer, intent(in) :: n_channels
    type(history_parameters_t), intent(in) :: par
    if (par%global) then
       allocate (pass%v_history (pass%n_it))
       call vamp_create_history (pass%v_history, &
            verbose = par%global_verbose)
    end if
    if (par%channel) then
       allocate (pass%v_histories (pass%n_it, n_channels))
       call vamp_create_history (pass%v_histories, &
            verbose = par%channel_verbose)
    end if
  end subroutine pass_configure_history
  
  function pass_matches (pass, ref) result (ok)
    type(pass_t), intent(in) :: pass, ref
    integer :: n
    logical :: ok
    ok = .true.
    if (ok)  ok = pass%i_pass == ref%i_pass
    if (ok)  ok = pass%i_first_it == ref%i_first_it
    if (ok)  ok = pass%n_it == ref%n_it
    if (ok)  ok = pass%n_calls == ref%n_calls
    if (ok)  ok = pass%n_bins == ref%n_bins
    if (ok)  ok = pass%adapt_grids .eqv. ref%adapt_grids
    if (ok)  ok = pass%adapt_weights .eqv. ref%adapt_weights
    if (ok)  ok = pass%integral_defined .eqv. ref%integral_defined
    if (pass%integral_defined) then
       n = pass%n_it
       if (ok)  ok = all (pass%calls(:n) == ref%calls(:n))
       if (ok)  ok = all (pass%integral(:n) .matches. ref%integral(:n))
       if (ok)  ok = all (pass%error(:n) .matches. ref%error(:n))
       if (ok)  ok = all (pass%efficiency(:n) .matches. ref%efficiency(:n))
    end if
  end function pass_matches
    
  subroutine pass_update (pass, ref, ok)
    class(pass_t), intent(inout) :: pass
    type(pass_t), intent(in) :: ref
    logical, intent(out) :: ok
    integer :: n, n_ref
    ok = .true.
    if (ok)  ok = pass%i_pass == ref%i_pass
    if (ok)  ok = pass%i_first_it == ref%i_first_it
    if (ok)  ok = pass%n_calls == ref%n_calls
    if (ok)  ok = pass%n_bins == ref%n_bins
    if (ok)  ok = pass%adapt_grids .eqv. ref%adapt_grids
    if (ok)  ok = pass%adapt_weights .eqv. ref%adapt_weights
    if (ok) then
       if (ref%integral_defined) then
          if (.not. allocated (pass%calls)) then
             allocate (pass%calls (pass%n_it), source = 0)
             allocate (pass%integral (pass%n_it), source = 0._default)
             allocate (pass%error (pass%n_it), source = 0._default)
             allocate (pass%efficiency (pass%n_it), source = 0._default)
          end if
          n = count (pass%calls /= 0)
          n_ref = count (ref%calls /= 0)
          ok = n <= n_ref .and. n_ref <= pass%n_it
          if (ok)  ok = all (pass%calls(:n) == ref%calls(:n))
          if (ok)  ok = all (pass%integral(:n) .matches. ref%integral(:n))
          if (ok)  ok = all (pass%error(:n) .matches. ref%error(:n))
          if (ok)  ok = all (pass%efficiency(:n) .matches. ref%efficiency(:n))
          if (ok) then
             pass%calls(n+1:n_ref) = ref%calls(n+1:n_ref)
             pass%integral(n+1:n_ref) = ref%integral(n+1:n_ref)
             pass%error(n+1:n_ref) = ref%error(n+1:n_ref)
             pass%efficiency(n+1:n_ref) = ref%efficiency(n+1:n_ref)
             pass%integral_defined = any (pass%calls /= 0)
          end if
       end if
    end if
  end subroutine pass_update

  elemental function real_matches (x, y) result (ok)
    real(default), intent(in) :: x, y
    logical :: ok
    real(default), parameter :: tolerance = 1.e-8_default
    ok = abs (x - y) <= tolerance * max (abs (x), abs (y))
  end function real_matches
  
  function pass_get_integration_index (pass) result (n)
    class (pass_t), intent(in) :: pass
    integer :: n
    integer :: i
    n = 0
    if (allocated (pass%calls)) then
       do i = 1, pass%n_it
          if (pass%calls(i) == 0)  exit
          n = i
       end do
    end if
  end function pass_get_integration_index

  function pass_get_calls (pass) result (calls)
    class(pass_t), intent(in) :: pass
    integer :: calls
    integer :: n
    n = pass%get_integration_index ()
    if (n /= 0) then
       calls = pass%calls(n)
    else
       calls = 0
    end if
  end function pass_get_calls

  function pass_get_integral (pass) result (integral)
    class(pass_t), intent(in) :: pass
    real(default) :: integral
    integer :: n
    n = pass%get_integration_index ()
    if (n /= 0) then
       integral = pass%integral(n)
    else
       integral = 0
    end if
  end function pass_get_integral

  function pass_get_error (pass) result (error)
    class(pass_t), intent(in) :: pass
    real(default) :: error
    integer :: n
    n = pass%get_integration_index ()
    if (n /= 0) then
       error = pass%error(n)
    else
       error = 0
    end if
  end function pass_get_error

  function pass_get_efficiency (pass) result (efficiency)
    class(pass_t), intent(in) :: pass
    real(default) :: efficiency
    integer :: n
    n = pass%get_integration_index ()
    if (n /= 0) then
       efficiency = pass%efficiency(n)
    else
       efficiency = 0
    end if
  end function pass_get_efficiency

  subroutine mci_vamp_reset (object)
    class(mci_vamp_t), intent(inout) :: object
    type(pass_t), pointer :: current_pass
    do while (associated (object%first_pass))
       current_pass => object%first_pass
       object%first_pass => current_pass%next
       call current_pass%final ()
       deallocate (current_pass)
    end do
    object%current_pass => null ()
  end subroutine mci_vamp_reset
  
  subroutine mci_vamp_final (object)
    class(mci_vamp_t), intent(inout) :: object
    call object%reset ()
    call vamp_equivalences_final (object%equivalences)
    call object%base_final ()
  end subroutine mci_vamp_final
  
  subroutine mci_vamp_write (object, unit, pacify, md5sum_version)
    class(mci_vamp_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    type(pass_t), pointer :: current_pass
    integer :: u, i
    u = output_unit (unit)
    write (u, "(1x,A)")  "VAMP integrator:"
    call object%base_write (u, pacify, md5sum_version)
    if (allocated (object%dim_is_flat)) then
       write (u, "(3x,A,999(1x,I0))")  "Flat dimensions    =", &
            pack ([(i, i = 1, object%n_dim)], object%dim_is_flat)
    end if
    write (u, "(1x,A)")  "Grid parameters:"
    call object%grid_par%write (u)
    write (u, "(3x,A,I0)") "min_calls             = ", object%min_calls
    write (u, "(3x,A,L1)") "negative weights      = ", &
         object%negative_weights
    write (u, "(3x,A,L1)") "verbose               = ", &
         object%verbose  
    if (object%grid_par%use_vamp_equivalences) then
       call vamp_equivalences_write (object%equivalences, u)
    end if
    current_pass => object%first_pass
    do while (associated (current_pass))
       write (u, "(1x,A,I0,A)")  "Integration pass:"
       call current_pass%write (u, pacify)
       current_pass => current_pass%next
    end do
    if (object%md5sum_adapted /= "") then
       write (u, "(1x,A,A,A)")  "MD5 sum (including results) = '", &
            object%md5sum_adapted, "'"
    end if
  end subroutine mci_vamp_write
  
  subroutine mci_vamp_write_history_parameters (mci, unit)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)")  "VAMP history parameters:"
    call mci%history_par%write (unit)
  end subroutine mci_vamp_write_history_parameters

  subroutine mci_vamp_write_history (mci, unit)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in), optional :: unit
    type(pass_t), pointer :: current_pass
    integer :: i_pass
    integer :: u
    u = output_unit (unit)
    if (associated (mci%first_pass)) then
       write (u, "(1x,A)")  "VAMP history (global):"
       i_pass = 0
       current_pass => mci%first_pass
       do while (associated (current_pass))
          i_pass = i_pass + 1
          write (u, "(1x,A,I0,':')")  "Pass #", i_pass
          call current_pass%write_history (u)
          current_pass => current_pass%next
       end do
    end if
  end subroutine mci_vamp_write_history
  
  subroutine mci_vamp_compute_md5sum (mci, pacify)
    class(mci_vamp_t), intent(inout) :: mci
    logical, intent(in), optional :: pacify
    integer :: u
    mci%md5sum_adapted = ""
    u = free_unit ()
    open (u, status = "scratch", action = "readwrite")
    write (u, "(A)")  mci%md5sum
    call mci%write (u, pacify, md5sum_version = .true.)
    rewind (u)
    mci%md5sum_adapted = md5sum (u)
    close (u)
  end subroutine mci_vamp_compute_md5sum
    
  function mci_vamp_get_md5sum (mci) result (md5sum)
    class(mci_vamp_t), intent(in) :: mci
    character(32) :: md5sum
    if (mci%md5sum_adapted /= "") then
       md5sum = mci%md5sum_adapted
    else
       md5sum = mci%md5sum
    end if
  end function mci_vamp_get_md5sum
  
  subroutine mci_vamp_startup_message (mci, unit, n_calls)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    integer :: num_calls, n_bins
    if (present (n_calls)) then
       num_calls = n_calls 
    else
       num_calls = 0
    end if
    if (mci%min_calls /= 0) then
       n_bins =  max (mci%grid_par%min_bins, &
            min (num_calls / mci%min_calls, &
            mci%grid_par%max_bins))
    else
       n_bins = mci%grid_par%max_bins  
    end if    
    call mci%base_startup_message (unit = unit, n_calls = n_calls)
    if (mci%grid_par%use_vamp_equivalences) then
       write (msg_buffer, "(A,2(1x,I0,1x,A))") &
            "Integrator: Using VAMP channel equivalences"    
       call msg_message (unit = unit)
    end if
    write (msg_buffer, "(A,2(1x,I0,1x,A),L1)") &
         "Integrator:", num_calls, &
         "initial calls,", n_bins, & 
         "bins, stratified = ", &
         mci%grid_par%stratified
    call msg_message (unit = unit)
    write (msg_buffer, "(A,2(1x,I0,1x,A))") &
         "Integrator: VAMP"
    call msg_message (unit = unit)
  end subroutine mci_vamp_startup_message
    
  subroutine mci_vamp_record_index (mci, i_mci)
    class(mci_vamp_t), intent(inout) :: mci
    integer, intent(in) :: i_mci
    type(string_t) :: basename, suffix
    character(32) :: buffer
    if (mci%grid_filename_set) then
       basename = mci%grid_filename
       call split (basename, suffix, ".", back=.true.)
       write (buffer, "(I0)")  i_mci
       if (basename /= "") then
          mci%grid_filename = basename // "_m" // trim (buffer) // "." // suffix
       else
          mci%grid_filename = suffix // "_m" // trim (buffer) // ".vg"
       end if
    end if
  end subroutine mci_vamp_record_index

  subroutine mci_vamp_set_grid_parameters (mci, grid_par)
    class(mci_vamp_t), intent(inout) :: mci
    type(grid_parameters_t), intent(in) :: grid_par
    mci%grid_par = grid_par
    mci%min_calls = grid_par%min_calls_per_bin * mci%n_channel
  end subroutine mci_vamp_set_grid_parameters
  
  subroutine mci_vamp_set_history_parameters (mci, history_par)
    class(mci_vamp_t), intent(inout) :: mci
    type(history_parameters_t), intent(in) :: history_par
    mci%history_par = history_par
  end subroutine mci_vamp_set_history_parameters
  
  subroutine mci_vamp_set_rebuild_flag (mci, rebuild, check_grid_file)
    class(mci_vamp_t), intent(inout) :: mci
    logical, intent(in) :: rebuild
    logical, intent(in) :: check_grid_file
    mci%rebuild = rebuild
    mci%check_grid_file = check_grid_file
  end subroutine mci_vamp_set_rebuild_flag
  
  subroutine mci_vamp_set_grid_filename (mci, name, run_id)
    class(mci_vamp_t), intent(inout) :: mci
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: run_id
    if (present (run_id)) then
       mci%grid_filename = name // "." // run_id // ".vg"
    else
       mci%grid_filename = name // ".vg"
    end if
    mci%grid_filename_set = .true.
  end subroutine mci_vamp_set_grid_filename
  
  subroutine mci_vamp_declare_flat_dimensions (mci, dim_flat)
    class(mci_vamp_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: dim_flat
    integer :: d
    allocate (mci%dim_is_flat (mci%n_dim), source = .false.)
    do d = 1, size (dim_flat)
       mci%dim_is_flat(dim_flat(d)) = .true.
    end do
  end subroutine mci_vamp_declare_flat_dimensions
  
  subroutine mci_vamp_declare_equivalences (mci, channel, dim_offset)
    class(mci_vamp_t), intent(inout) :: mci
    type(phs_channel_t), dimension(:), intent(in) :: channel
    integer, intent(in) :: dim_offset
    integer, dimension(:), allocatable :: perm, mode
    integer :: n_channels, n_dim, n_equivalences
    integer :: c, i, j, left, right
    n_channels = mci%n_channel
    n_dim = mci%n_dim
    n_equivalences = 0
    do c = 1, n_channels
       n_equivalences = n_equivalences + size (channel(c)%eq)
    end do
    call vamp_equivalences_init (mci%equivalences, &
         n_equivalences, n_channels, n_dim)
    allocate (perm (n_dim))
    allocate (mode (n_dim))
    perm(1:dim_offset) = [(i, i = 1, dim_offset)]
    mode(1:dim_offset) = VEQ_IDENTITY
    c = 1
    j = 0
    do i = 1, n_equivalences
       if (j < size (channel(c)%eq)) then
          j = j + 1
       else
          c = c + 1
          j = 1
       end if
       associate (eq => channel(c)%eq(j))
         left = c
         right = eq%c
         perm(dim_offset+1:) = eq%perm + dim_offset
         mode(dim_offset+1:) = eq%mode
         call vamp_equivalence_set (mci%equivalences, &
              i, left, right, perm, mode)
       end associate
    end do
    call vamp_equivalences_complete (mci%equivalences)
  end subroutine mci_vamp_declare_equivalences

  subroutine mci_vamp_allocate_instance (mci, mci_instance)
    class(mci_vamp_t), intent(in) :: mci
    class(mci_instance_t), intent(out), pointer :: mci_instance
    allocate (mci_vamp_instance_t :: mci_instance)
  end subroutine mci_vamp_allocate_instance
  
  subroutine mci_vamp_add_pass (mci, adapt_grids, adapt_weights, final)
    class(mci_vamp_t), intent(inout) :: mci
    logical, intent(in), optional :: adapt_grids, adapt_weights, final
    integer :: i_pass, i_it
    type(pass_t), pointer :: new
    allocate (new)
    if (associated (mci%current_pass)) then
       i_pass = mci%current_pass%i_pass + 1
       i_it   = mci%current_pass%i_first_it + mci%current_pass%n_it
       mci%current_pass%next => new
    else
       i_pass = 1
       i_it = 1
       mci%first_pass => new
    end if
    mci%current_pass => new
    new%i_pass = i_pass
    new%i_first_it = i_it
    if (present (adapt_grids)) then
       new%adapt_grids = adapt_grids
    else
       new%adapt_grids = .false.
    end if
    if (present (adapt_weights)) then
       new%adapt_weights = adapt_weights
    else
       new%adapt_weights = .false.
    end if
    if (present (final)) then
       new%is_final_pass = final
    else
       new%is_final_pass = .false.
    end if
  end subroutine mci_vamp_add_pass
  
  subroutine mci_vamp_update_from_ref (mci, mci_ref, success)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_t), intent(in) :: mci_ref
    logical, intent(out) :: success
    type(pass_t), pointer :: current_pass, ref_pass
    select type (mci_ref)
    type is (mci_vamp_t)
       current_pass => mci%first_pass
       ref_pass => mci_ref%first_pass
       success = .true.
       do while (success .and. associated (current_pass))
          if (associated (ref_pass)) then
             if (associated (current_pass%next)) then
                success = current_pass .matches. ref_pass
             else
                call current_pass%update (ref_pass, success)
                if (current_pass%integral_defined) then
                   mci%integral = current_pass%get_integral ()
                   mci%error = current_pass%get_error ()
                   mci%efficiency = current_pass%get_efficiency ()
                   mci%integral_known = .true.
                   mci%error_known = .true.
                   mci%efficiency_known = .true.
                end if
             end if
             current_pass => current_pass%next
             ref_pass => ref_pass%next
          else
             success = .false.
          end if
       end do
    end select
  end subroutine mci_vamp_update_from_ref
  
  subroutine mci_vamp_update (mci, u, success)
    class(mci_vamp_t), intent(inout) :: mci
    integer, intent(in) :: u
    logical, intent(out) :: success
    character(80) :: buffer
    character(32) :: md5sum_file
    type(mci_vamp_t) :: mci_file
    integer :: n_pass, n_it
    call read_sval (u, md5sum_file)
    if (mci%check_grid_file) then
       success = md5sum_file == mci%md5sum
    else
       success = .true.
    end if
    if (success) then
       read (u, *)
       read (u, "(A)")  buffer
       if (trim (adjustl (buffer)) == "VAMP integrator:") then
          n_pass = 0
          n_it = 0
          do
             read (u, "(A)")  buffer
             select case (trim (adjustl (buffer)))
             case ("")
                exit
             case ("Integration pass:")
                call mci_file%add_pass ()
                call mci_file%current_pass%read (u, n_pass, n_it)
                n_pass = n_pass + 1
                n_it = n_it + mci_file%current_pass%n_it
             end select
          end do
          call mci%update_from_ref (mci_file, success)
          call mci_file%final ()
       else
          call msg_fatal ("VAMP: reading grid file: corrupted data")
       end if
    end if
  end subroutine mci_vamp_update
  
  subroutine mci_vamp_write_grids (mci, instance)
    class(mci_vamp_t), intent(in) :: mci
    class(mci_instance_t), intent(inout) :: instance
    integer :: u
    select type (instance)
    type is (mci_vamp_instance_t)
       if (mci%grid_filename_set) then
          if (instance%grids_defined) then
             u = free_unit ()
             open (u, file = char (mci%grid_filename), &
                  action = "write", status = "replace")
             write (u, "(1x,A,A,A)")  "MD5sum = '", mci%md5sum, "'"
             write (u, *)
             call mci%write (u)
             write (u, *)
             write (u, "(1x,A)")  "VAMP grids:"
             call vamp_write_grids (instance%grids, u, &
                  write_integrals = .true.)
             close (u)
          else
             call msg_bug ("VAMP: write grids: grids undefined")
          end if
       else
          call msg_bug ("VAMP: write grids: filename undefined")
       end if
    end select
  end subroutine mci_vamp_write_grids

  subroutine mci_vamp_read_grids_header (mci, success)
    class(mci_vamp_t), intent(inout) :: mci
    logical, intent(out) :: success
    logical :: exist
    integer :: u
    success = .false.
    if (mci%grid_filename_set) then
       inquire (file = char (mci%grid_filename), exist = exist)
       if (exist) then
          u = free_unit ()
          open (u, file = char (mci%grid_filename), &
               action = "read", status = "old")
          call mci%update (u, success)
          close (u)
          if (.not. success) then
             write (msg_buffer, "(A,A,A)") &
                  "VAMP: parameter mismatch, discarding grid file '", &
                  char (mci%grid_filename), "'"
             call msg_message ()
          end if
       end if
    else
       call msg_bug ("VAMP: read grids: filename undefined")
    end if
  end subroutine mci_vamp_read_grids_header
  
  subroutine mci_vamp_read_grids_data (mci, instance, read_integrals)
    class(mci_vamp_t), intent(in) :: mci
    class(mci_instance_t), intent(inout) :: instance
    logical, intent(in), optional :: read_integrals
    integer :: u
    character(80) :: buffer
    select type (instance)
    type is (mci_vamp_instance_t)
       if (.not. instance%grids_defined) then
          u = free_unit ()
          open (u, file = char (mci%grid_filename), &
               action = "read", status = "old") 
          do
             read (u, "(A)")  buffer
             if (trim (adjustl (buffer)) == "VAMP grids:")  exit
          end do
          call vamp_read_grids (instance%grids, u, read_integrals)
          close (u)
          instance%grids_defined = .true.
       else
          call msg_bug ("VAMP: read grids: grids already defined")
       end if
    end select
  end subroutine mci_vamp_read_grids_data
  
  subroutine mci_vamp_read_grids (mci, instance, success)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    logical, intent(out) :: success
    logical :: exist
    integer :: u
    character(80) :: buffer
    select type (instance)
    type is (mci_vamp_instance_t)
       success = .false.
       if (mci%grid_filename_set) then
          if (.not. instance%grids_defined) then
             inquire (file = char (mci%grid_filename), exist = exist)
             if (exist) then
                u = free_unit ()
                open (u, file = char (mci%grid_filename), &
                     action = "read", status = "old")
                call mci%update (u, success)
                if (success) then
                   read (u, "(A)")  buffer
                   if (trim (adjustl (buffer)) == "VAMP grids:") then
                      call vamp_read_grids (instance%grids, u)
                   else
                      call msg_fatal ("VAMP: reading grid file: &
                           &corrupted grid data")
                   end if
                else
                   write (msg_buffer, "(A,A,A)") &
                        "VAMP: parameter mismatch, discarding grid file '", &
                        char (mci%grid_filename), "'"
                   call msg_message ()
                end if
                close (u)
                instance%grids_defined = success
             end if
          else
             call msg_bug ("VAMP: read grids: grids already defined")
          end if
       else
          call msg_bug ("VAMP: read grids: filename undefined")
       end if
    end select
  end subroutine mci_vamp_read_grids
  
  subroutine read_rval (u, rval)
    integer, intent(in) :: u
    real(default), intent(out) :: rval
    character(80) :: buffer
    read (u, "(A)")  buffer
    buffer = adjustl (buffer(scan (buffer, "=") + 1:))
    read (buffer, *)  rval
  end subroutine read_rval
    
  subroutine read_ival (u, ival)
    integer, intent(in) :: u
    integer, intent(out) :: ival
    character(80) :: buffer
    read (u, "(A)")  buffer
    buffer = adjustl (buffer(scan (buffer, "=") + 1:))
    read (buffer, *)  ival
  end subroutine read_ival
    
  subroutine read_sval (u, sval)
    integer, intent(in) :: u
    character(*), intent(out) :: sval
    character(80) :: buffer
    read (u, "(A)")  buffer
    buffer = adjustl (buffer(scan (buffer, "=") + 1:))
    read (buffer, *)  sval
  end subroutine read_sval
    
  subroutine read_lval (u, lval)
    integer, intent(in) :: u
    logical, intent(out) :: lval
    character(80) :: buffer
    read (u, "(A)")  buffer
    buffer = adjustl (buffer(scan (buffer, "=") + 1:))
    read (buffer, *)  lval
  end subroutine read_lval
    
  subroutine mci_vamp_integrate (mci, instance, sampler, &
       n_it, n_calls, results, pacify)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    class(mci_results_t), intent(inout), optional :: results
    logical, intent(in), optional :: pacify
    integer :: it
    logical :: reshape, from_file, success
    select type (instance)
    type is (mci_vamp_instance_t)
       if (associated (mci%current_pass)) then
          mci%current_pass%integral_defined = .false.
          call mci%current_pass%configure (n_it, n_calls, &
               mci%min_calls, mci%grid_par%min_bins, &
               mci%grid_par%max_bins, &
               mci%grid_par%min_calls_per_channel * mci%n_channel)
          call mci%current_pass%configure_history &
               (mci%n_channel, mci%history_par)
          instance%pass_complete = .false.
          instance%it_complete = .false.
          call instance%new_pass (reshape)
          if (.not. instance%grids_defined .or. instance%grids_from_file) then
             if (mci%grid_filename_set .and. .not. mci%rebuild) then
                call mci%read_grids_header (success)
                from_file = success
                if (.not. instance%grids_defined .and. success) then
                   call mci%read_grids_data (instance)
                end if
             else
                from_file = .false.
             end if
          else
             from_file = .false.
          end if
          if (from_file) then
             if (.not. mci%check_grid_file) &
                  call msg_warning ("Reading grid file: MD5 sum check disabled")
             call msg_message ("VAMP: " &
                  // "using grids and results from file '" &
                  // char (mci%grid_filename) // "'")
          else if (.not. instance%grids_defined) then
             call instance%create_grids ()
          end if
          do it = 1, instance%n_it
             if (signal_is_pending ())  return
             instance%grids_from_file = from_file .and. &
                  it <= mci%current_pass%get_integration_index ()
             if (.not. instance%grids_from_file) then
                instance%it_complete = .false.
                call instance%adapt_grids ()
                if (signal_is_pending ())  return
                call instance%adapt_weights ()
                if (signal_is_pending ())  return
                call instance%discard_integrals (reshape)
                if (mci%grid_par%use_vamp_equivalences) then
                   call instance%sample_grids (mci%rng, sampler, &
                        mci%equivalences)
                else
                   call instance%sample_grids (mci%rng, sampler)
                end if
                if (signal_is_pending ())  return
                instance%it_complete = .true.
                if (instance%integral /= 0) then
                   mci%current_pass%calls(it) = instance%calls
                   mci%current_pass%integral(it) = instance%integral
                   if (abs (instance%error / instance%integral) &
                        > epsilon (1._default)) then
                      mci%current_pass%error(it) = instance%error
                   end if
                   mci%current_pass%efficiency(it) = instance%efficiency
                end if
                mci%current_pass%integral_defined = .true.
             end if
             if (present (results)) then
                if (mci%has_chains ()) then
                   call mci%collect_chain_weights (instance%w)
                   call results%record (1, &
                        n_calls    = mci%current_pass%calls(it), &
                        integral   = mci%current_pass%integral(it), &
                        error      = mci%current_pass%error(it), &
                        efficiency = mci%current_pass%efficiency(it), &
                        chain_weights = mci%chain_weights, &
                        suppress = pacify)
                else
                   call results%record (1, &
                        n_calls    = mci%current_pass%calls(it), &
                        integral   = mci%current_pass%integral(it), &
                        error      = mci%current_pass%error(it), &
                        efficiency = mci%current_pass%efficiency(it), &
                        suppress = pacify)
                end if
             end if
             if (.not. instance%grids_from_file &
                  .and. mci%grid_filename_set) then
                call mci%write_grids (instance)
             end if
             call instance%allow_adaptation ()
             reshape = .false.
             if (.not. mci%current_pass%is_final_pass) then
                call mci%check_goals (it, success)
                if (success)  exit
             end if
          end do
          if (signal_is_pending ())  return
          instance%pass_complete = .true.
          mci%integral = mci%current_pass%get_integral()
          mci%error = mci%current_pass%get_error()
          mci%efficiency = mci%current_pass%get_efficiency()
          mci%integral_known = .true.
          mci%error_known = .true.
          mci%efficiency_known = .true.
          call mci%compute_md5sum (pacify)
       else
          call msg_bug ("MCI integrate: current_pass object not allocated")
       end if
    end select
  end subroutine mci_vamp_integrate

  subroutine mci_vamp_check_goals (mci, it, success)
    class(mci_vamp_t), intent(inout) :: mci
    integer, intent(in) :: it
    logical, intent(out) :: success
    success = .false.
    if (mci%error_reached (it)) then
       mci%current_pass%n_it = it
       call msg_message ("VAMP: error goal reached; &
            &skipping iterations")
       success = .true.
       return
    end if
    if (mci%rel_error_reached (it)) then
       mci%current_pass%n_it = it
       call msg_message ("VAMP: relative error goal reached; &
            &skipping iterations")
       success = .true.
       return
    end if
    if (mci%accuracy_reached (it)) then
       mci%current_pass%n_it = it
       call msg_message ("VAMP: accuracy goal reached; &
            &skipping iterations")
       success = .true.
       return
    end if
  end subroutine mci_vamp_check_goals
    
  function mci_vamp_error_reached (mci, it) result (flag)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in) :: it
    logical :: flag
    real(default) :: error_goal, error
    error_goal = mci%grid_par%error_goal
    if (error_goal > 0) then
       associate (pass => mci%current_pass)
         if (pass%integral_defined) then
            error = abs (pass%error(it))
            flag = error < error_goal
         else
            flag = .false.
         end if
       end associate
    else
       flag = .false.
    end if
  end function mci_vamp_error_reached
  
  function mci_vamp_rel_error_reached (mci, it) result (flag)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in) :: it
    logical :: flag
    real(default) :: rel_error_goal, rel_error
    rel_error_goal = mci%grid_par%rel_error_goal
    if (rel_error_goal > 0) then
       associate (pass => mci%current_pass)
         if (pass%integral_defined) then
            if (pass%integral(it) /= 0) then
               rel_error = abs (pass%error(it) / pass%integral(it))
               flag = rel_error < rel_error_goal
            else
               flag = .true.
            end if
         else
            flag = .false.
         end if
       end associate
    else
       flag = .false.
    end if
  end function mci_vamp_rel_error_reached
  
  function mci_vamp_accuracy_reached (mci, it) result (flag)
    class(mci_vamp_t), intent(in) :: mci
    integer, intent(in) :: it
    logical :: flag
    real(default) :: accuracy_goal, accuracy
    accuracy_goal = mci%grid_par%accuracy_goal
    if (accuracy_goal > 0) then
       associate (pass => mci%current_pass)
         if (pass%integral_defined) then
            if (pass%integral(it) /= 0) then
               accuracy = abs (pass%error(it) / pass%integral(it)) &
                    * sqrt (real (pass%calls(it), default))
               flag = accuracy < accuracy_goal
            else
               flag = .true.
            end if
         else
            flag = .false.
         end if
       end associate
    else
       flag = .false.
    end if
  end function mci_vamp_accuracy_reached
  
  subroutine mci_vamp_prepare_simulation (mci)
    class(mci_vamp_t), intent(inout) :: mci
    logical :: success
    if (mci%grid_filename_set) then
       call mci%read_grids_header (success)
       call mci%compute_md5sum ()
       if (.not. success) then
          call msg_fatal ("Simulate: " &
               // "reading integration grids from file '" &
               // char (mci%grid_filename) // "' failed")
       end if
    else
       call msg_bug ("VAMP: simulation: no grids, no grid filename")
    end if
  end subroutine mci_vamp_prepare_simulation
  
  subroutine mci_vamp_generate_weighted_event (mci, instance, sampler)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    class(vamp_data_t), allocatable :: data
    type(exception) :: vamp_exception
    select type (instance)
    type is (mci_vamp_instance_t)
       instance%vamp_weight_set = .false.
       allocate (mci_workspace_t :: data)
       select type (data)
       type is (mci_workspace_t)
          data%sampler => sampler
          data%instance => instance
       end select
       select type (rng => mci%rng)
       type is (rng_tao_t)
          if (instance%grids_defined) then
             call vamp_next_event ( &
                  instance%vamp_x, &
                  rng%state, &
                  instance%grids, &
                  vamp_sampling_function, &
                  data, &
                  phi = phi_trivial, &
                  weight = instance%vamp_weight, &
                  exc = vamp_exception)
             call handle_vamp_exception (vamp_exception, mci%verbose)
             instance%vamp_excess = 0
             instance%vamp_weight_set = .true.
          else
             call msg_bug ("VAMP: generate event: grids undefined")
          end if
       class default
         call msg_fatal ("VAMP event generation: &
               &random-number generator must be TAO")
       end select
    end select
  end subroutine mci_vamp_generate_weighted_event
       
  subroutine mci_vamp_generate_unweighted_event (mci, instance, sampler)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    class(vamp_data_t), allocatable :: data
    type(exception) :: vamp_exception
    select type (instance)
    type is (mci_vamp_instance_t)
       instance%vamp_weight_set = .false.
       allocate (mci_workspace_t :: data)
       select type (data)
       type is (mci_workspace_t)
          data%sampler => sampler
          data%instance => instance
       end select
       select type (rng => mci%rng)
       type is (rng_tao_t)
          if (instance%grids_defined) then
             REJECTION: do
                call vamp_next_event ( &
                     instance%vamp_x, &
                     rng%state, &
                     instance%grids, &
                     vamp_sampling_function, &
                     data, &
                     phi = phi_trivial, &
                     excess = instance%vamp_excess, &
                     exc = vamp_exception)
                if (signal_is_pending ())  return
                if (sampler%is_valid ())  exit REJECTION
             end do REJECTION
             call handle_vamp_exception (vamp_exception, mci%verbose)
             instance%vamp_weight = 1
             instance%vamp_weight_set = .true.
          else
             call msg_bug ("VAMP: generate event: grids undefined")
          end if
       class default
         call msg_fatal ("VAMP event generation: &
               &random-number generator must be TAO")
       end select
    end select
  end subroutine mci_vamp_generate_unweighted_event
    
  subroutine mci_vamp_rebuild_event (mci, instance, sampler, state)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
    call msg_bug ("MCI vamp rebuild event not implemented yet")
  end subroutine mci_vamp_rebuild_event
       
  subroutine mci_vamp_pacify (object, efficiency_reset, error_reset)
    class(mci_vamp_t), intent(inout) :: object
    logical, intent(in), optional :: efficiency_reset, error_reset
    logical :: err_reset
    type(pass_t), pointer :: current_pass
    err_reset = .false.
    if (present (error_reset))  err_reset = error_reset
    current_pass => object%first_pass    
    do while (associated (current_pass))
       if (allocated (current_pass%error) .and. err_reset) then
          current_pass%error = 0
       end if
       if (allocated (current_pass%efficiency) .and. err_reset) then
          current_pass%efficiency = 1
       end if
       current_pass => current_pass%next
    end do
  end subroutine mci_vamp_pacify
    
  subroutine mci_vamp_instance_write (object, unit, pacify)
    class(mci_vamp_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify    
    integer :: u, i
    character(len=7) :: fmt 
    call pac_fmt (fmt, FMT_17, FMT_14, pacify)
    u = output_unit (unit)
    write (u, "(3x,A," // FMT_19 // ")") "Integrand = ", object%integrand
    write (u, "(3x,A," // FMT_19 // ")") "Weight    = ", object%mci_weight
    if (object%vamp_weight_set) then
       write (u, "(3x,A," // FMT_19 // ")") "VAMP wgt  = ", object%vamp_weight
       if (object%vamp_excess /= 0) then
          write (u, "(3x,A," // FMT_19 // ")") "VAMP exc  = ", &
               object%vamp_excess
       end if
    end if
    write (u, "(3x,A,L1)")  "adapt grids   = ", object%enable_adapt_grids
    write (u, "(3x,A,L1)")  "adapt weights = ", object%enable_adapt_weights
    if (object%grids_defined) then
       if (object%grids_from_file) then
          write (u, "(3x,A)")  "VAMP grids: read from file"
       else
          write (u, "(3x,A)")  "VAMP grids: defined"
       end if
    else
       write (u, "(3x,A)")  "VAMP grids: [undefined]"
    end if
    write (u, "(3x,A,I0)")  "n_it          = ", object%n_it
    write (u, "(3x,A,I0)")  "it            = ", object%it
    write (u, "(3x,A,L1)")  "pass complete = ", object%it_complete
    write (u, "(3x,A,I0)")  "n_calls       = ", object%n_calls
    write (u, "(3x,A,I0)")  "calls         = ", object%calls
    write (u, "(3x,A,L1)")  "it complete   = ", object%it_complete
    write (u, "(3x,A,I0)")  "n adapt.(g)   = ", object%n_adapt_grids
    write (u, "(3x,A,I0)")  "n adapt.(w)   = ", object%n_adapt_weights
    write (u, "(3x,A,L1)")  "gen. events   = ", object%generating_events
    write (u, "(3x,A,L1)")  "neg. weights  = ", object%negative_weights
    if (object%safety_factor /= 1)  write &
          (u, "(3x,A," // fmt // ")")  "safety f = ", object%safety_factor
    write (u, "(3x,A," // fmt // ")")  "integral = ", object%integral
    write (u, "(3x,A," // fmt // ")")  "error    = ", object%error
    write (u, "(3x,A," // fmt // ")")  "eff.     = ", object%efficiency
    write (u, "(3x,A)")  "weights:"
    do i = 1, size (object%w)
       write (u, "(5x,I0,1x," // FMT_12 // ")")  i, object%w(i)
    end do
  end subroutine mci_vamp_instance_write
  
  subroutine mci_vamp_instance_write_grids (object, unit)
    class(mci_vamp_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    if (object%grids_defined) then
       call vamp_write_grids (object%grids, u, write_integrals = .true.)
    end if
  end subroutine mci_vamp_instance_write_grids
  
  subroutine mci_vamp_instance_init (mci_instance, mci)
    class(mci_vamp_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
    call mci_instance%base_init (mci)
    select type (mci)
    type is (mci_vamp_t)
       mci_instance%mci => mci
       allocate (mci_instance%gi (mci%n_channel))
       mci_instance%allocate_global_history = .not. mci%history_par%global
       mci_instance%allocate_channel_history = .not. mci%history_par%channel
       mci_instance%negative_weights = mci%negative_weights
    end select
  end subroutine mci_vamp_instance_init
    
  subroutine mci_vamp_instance_new_pass (instance, reshape)
    class(mci_vamp_instance_t), intent(inout) :: instance
    logical, intent(out) :: reshape
    type(pass_t), pointer :: current
    associate (mci => instance%mci)
      current => mci%current_pass
      instance%n_it = current%n_it
      if (instance%n_calls == 0) then
         reshape = .false.
         instance%n_calls = current%n_calls
      else if (instance%n_calls == current%n_calls) then
         reshape = .false.
      else
         reshape = .true.
         instance%n_calls = current%n_calls
      end if
      instance%it = 0
      instance%calls = 0
      instance%enable_adapt_grids = current%adapt_grids
      instance%enable_adapt_weights = current%adapt_weights
      instance%generating_events = .false.
      if (instance%allocate_global_history) then
         if (associated (instance%v_history)) then
            call vamp_delete_history (instance%v_history)
            deallocate (instance%v_history)
         end if
         allocate (instance%v_history (instance%n_it))
         call vamp_create_history (instance%v_history, verbose = .false.)
      else
         instance%v_history => current%v_history
      end if
      if (instance%allocate_channel_history) then
         if (associated (instance%v_histories)) then
            call vamp_delete_history (instance%v_histories)
            deallocate (instance%v_histories)
         end if
         allocate (instance%v_histories (instance%n_it, mci%n_channel))
         call vamp_create_history (instance%v_histories, verbose = .false.)
      else
         instance%v_histories => current%v_histories
      end if
    end associate
  end subroutine mci_vamp_instance_new_pass
  
  subroutine mci_vamp_instance_create_grids (instance)
    class(mci_vamp_instance_t), intent(inout) :: instance
    type (pass_t), pointer :: current
    integer, dimension(:), allocatable :: num_div
    real(default), dimension(:,:), allocatable :: region
    associate (mci => instance%mci)
      current => mci%current_pass
      allocate (num_div (mci%n_dim))
      allocate (region (2, mci%n_dim))
      region(1,:) = 0
      region(2,:) = 1
      num_div = current%n_bins
      instance%n_adapt_grids = 0
      instance%n_adapt_weights = 0
      if (.not. instance%grids_defined) then
         call vamp_create_grids (instance%grids, &
              region, &
              current%n_calls, &
              weights = instance%w, &
              num_div = num_div, &
              stratified = mci%grid_par%stratified)
         instance%grids_defined = .true.
      else
         call msg_bug ("VAMP: create grids: grids already defined")
      end if
    end associate
  end subroutine mci_vamp_instance_create_grids

  subroutine mci_vamp_instance_discard_integrals (instance, reshape)
    class(mci_vamp_instance_t), intent(inout) :: instance
    logical, intent(in) :: reshape
    instance%calls = 0
    instance%integral = 0
    instance%error = 0
    instance%efficiency = 0
    associate (mci => instance%mci)
      if (instance%grids_defined) then
         if (mci%grid_par%use_vamp_equivalences) then
            if (reshape) then
               call vamp_discard_integrals (instance%grids, &
                    num_calls = instance%n_calls, &
                    stratified = mci%grid_par%stratified, &
                    eq = mci%equivalences)
            else
               call vamp_discard_integrals (instance%grids, &
                    stratified = mci%grid_par%stratified, &
                    eq = mci%equivalences)
            end if
         else
            if (reshape) then
               call vamp_discard_integrals (instance%grids, &
                    num_calls = instance%n_calls, &
                    stratified = mci%grid_par%stratified)
            else
               call vamp_discard_integrals (instance%grids, &
                    stratified = mci%grid_par%stratified)
            end if
         end if
      else
         call msg_bug ("VAMP: discard integrals: grids undefined")
      end if
    end associate
  end subroutine mci_vamp_instance_discard_integrals
    
  subroutine mci_vamp_instance_allow_adaptation (instance)
    class(mci_vamp_instance_t), intent(inout) :: instance
    instance%allow_adapt_grids = .true.
    instance%allow_adapt_weights = .true.
  end subroutine mci_vamp_instance_allow_adaptation

  subroutine mci_vamp_instance_adapt_grids (instance)
    class(mci_vamp_instance_t), intent(inout) :: instance
    if (instance%enable_adapt_grids .and. instance%allow_adapt_grids) then
       if (instance%grids_defined) then
          call vamp_refine_grids (instance%grids)
          instance%n_adapt_grids = instance%n_adapt_grids + 1
      else
         call msg_bug ("VAMP: adapt grids: grids undefined")
      end if
    end if
  end subroutine mci_vamp_instance_adapt_grids
  
  subroutine mci_vamp_instance_adapt_weights (instance)
    class(mci_vamp_instance_t), intent(inout) :: instance
    real(default) :: w_sum, w_avg_ch, sum_w_underflow, w_min
    integer :: n_ch, ch, n_underflow
    logical, dimension(:), allocatable :: mask, underflow
    type(exception) :: vamp_exception
    if (instance%enable_adapt_weights .and. instance%allow_adapt_weights) then
       associate (mci => instance%mci)
         if (instance%grids_defined) then
            instance%w = instance%grids%weights &
                 * vamp_get_variance (instance%grids%grids) &
                 ** mci%grid_par%channel_weights_power
            w_sum = sum (instance%w)
            if (w_sum /= 0) then
               instance%w = instance%w / w_sum
               if (mci%n_chain /= 0) then
                  allocate (mask (mci%n_channel))
                  do ch = 1, mci%n_chain
                     mask = mci%chain == ch
                     n_ch = count (mask)
                     if (n_ch /= 0) then
                        w_avg_ch = sum (instance%w, mask) / n_ch
                        where (mask)  instance%w = w_avg_ch
                     end if
                  end do
               end if
               if (mci%grid_par%threshold_calls /= 0) then
                  w_min = &
                       real (mci%grid_par%threshold_calls, default) &
                       / instance%n_calls
                  allocate (underflow (mci%n_channel))
                  underflow = instance%w /= 0 .and. abs (instance%w) < w_min
                  n_underflow = count (underflow)
                  sum_w_underflow = sum (instance%w, mask=underflow)
                  if (sum_w_underflow /= 1) then
                     where (underflow)
                        instance%w = w_min
                     elsewhere
                        instance%w = instance%w &
                             * (1 - n_underflow * w_min) / (1 - sum_w_underflow)
                     end where
                  end if
               end if
            end if
            call vamp_update_weights (instance%grids, instance%w, &
                 exc = vamp_exception)
            call handle_vamp_exception (vamp_exception, mci%verbose)
         else
            call msg_bug ("VAMP: adapt weights: grids undefined")
         end if
       end associate
       instance%n_adapt_weights = instance%n_adapt_weights + 1
    end if
  end subroutine mci_vamp_instance_adapt_weights
  
  subroutine mci_vamp_instance_sample_grids (instance, rng, sampler, eq)
    class(mci_vamp_instance_t), intent(inout), target :: instance
    class(rng_t), intent(inout) :: rng
    class(mci_sampler_t), intent(inout), target :: sampler
    type(vamp_equivalences_t), intent(in), optional :: eq
    class(vamp_data_t), allocatable :: data
    type(exception) :: vamp_exception
    allocate (mci_workspace_t :: data)
    select type (data)
    type is (mci_workspace_t)
       data%sampler => sampler
       data%instance => instance
    end select
    select type (rng)
    type is (rng_tao_t)
       instance%it = instance%it + 1
       instance%calls = 0
       if (instance%grids_defined) then
          call vamp_sample_grids ( &
               rng%state, &
               instance%grids, &
               vamp_sampling_function, &
               data, &
               1, &
               eq = eq, &
               history = instance%v_history(instance%it:), &
               histories = instance%v_histories(instance%it:,:), &
               integral = instance%integral, &
               std_dev = instance%error, &
               exc = vamp_exception, &
               negative_weights = instance%negative_weights)
          call handle_vamp_exception (vamp_exception, instance%mci%verbose)
          instance%efficiency = instance%get_efficiency ()
       else
          call msg_bug ("VAMP: sample grids: grids undefined")
       end if
    class default
       call msg_fatal ("VAMP integration: random-number generator must be TAO")
    end select
  end subroutine mci_vamp_instance_sample_grids

  function mci_vamp_instance_get_efficiency_array (mci) result (efficiency)
    class(mci_vamp_instance_t), intent(in) :: mci
    real(default), dimension(:), allocatable :: efficiency
    allocate (efficiency (mci%mci%n_channel))
    where (mci%grids%grids%f_max /= 0)
       efficiency = mci%grids%grids%mu(1) / abs (mci%grids%grids%f_max)
    elsewhere
       efficiency = 0
    end where
  end function mci_vamp_instance_get_efficiency_array

  function mci_vamp_instance_get_efficiency (mci) result (efficiency)
    class(mci_vamp_instance_t), intent(in) :: mci
    real(default) :: efficiency
    real(default), dimension(:), allocatable :: weight
    real(default) :: norm
    allocate (weight (mci%mci%n_channel))
    weight = mci%grids%weights * abs (mci%grids%grids%f_max)
    norm = sum (weight)
    if (norm /= 0) then
       efficiency = dot_product (mci%get_efficiency_array (), weight) / norm
    else
       efficiency = 1
    end if
  end function mci_vamp_instance_get_efficiency

  subroutine mci_vamp_instance_init_simulation (instance, safety_factor)
    class(mci_vamp_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: safety_factor
    associate (mci => instance%mci)
      allocate (instance%vamp_x (mci%n_dim))
      instance%it = 0
      instance%calls = 0
      instance%generating_events = .true.
      if (present (safety_factor))  instance%safety_factor = safety_factor
      if (.not. instance%grids_defined) then
         if (mci%grid_filename_set) then
            if (.not. mci%check_grid_file) &
                 call msg_warning ("Reading grid file: MD5 sum check disabled")
            call msg_message ("Simulate: " &
                 // "using integration grids from file '" &
                 // char (mci%grid_filename) // "'")
            call mci%read_grids_data (instance)
            if (instance%safety_factor /= 1) then
               write (msg_buffer, "(A,ES10.3,A)")  "Simulate: &
                    &applying safety factor", instance%safety_factor, &
                    " to event rejection"
               call msg_message ()
               instance%grids%grids%f_max = &
                    instance%grids%grids%f_max * instance%safety_factor
            end if
         else
            call msg_bug ("VAMP: simulation: no grids, no grid filename")
         end if
      end if
    end associate
  end subroutine mci_vamp_instance_init_simulation
  
  subroutine mci_vamp_instance_final_simulation (instance)
    class(mci_vamp_instance_t), intent(inout) :: instance
    if (allocated (instance%vamp_x))  deallocate (instance%vamp_x)
  end subroutine mci_vamp_instance_final_simulation
  
  function vamp_sampling_function &
       (xi, data, weights, channel, grids) result (f)
    real(default) :: f
    real(default), dimension(:), intent(in) :: xi
    class(vamp_data_t), intent(in) :: data
    real(default), dimension(:), intent(in), optional :: weights
    integer, intent(in), optional :: channel
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(exception) :: exc
    class(mci_instance_t), pointer :: instance
    logical :: verbose
    character(*), parameter :: FN = "WHIZARD sampling function"
    select type (data)
    type is (mci_workspace_t)
       instance => data%instance
       select type (instance)
       class is (mci_vamp_instance_t)
          instance%calls = instance%calls + 1
          verbose = instance%mci%verbose
       end select
       call instance%evaluate (data%sampler, channel, xi)
       if (signal_is_pending ()) then
          call raise_exception (exc, EXC_FATAL, FN, "signal received")
          call handle_vamp_exception (exc, verbose)
          call terminate_now_if_signal ()
       end if
       f = instance%get_value ()
    end select
  end function vamp_sampling_function

  pure function phi_trivial (xi, channel_dummy) result (x)
    real(default), dimension(:), intent(in) :: xi
    integer, intent(in) :: channel_dummy
    real(default), dimension(size(xi)) :: x
    x = xi
  end function phi_trivial

  subroutine mci_vamp_instance_compute_weight (mci, c)
    class(mci_vamp_instance_t), intent(inout) :: mci
    integer, intent(in) :: c
    integer :: i
    mci%selected_channel = c
    !$OMP PARALLEL PRIVATE(i) SHARED(mci)
    !$OMP DO
    do i = 1, mci%mci%n_channel
       if (mci%w(i) /= 0) then
          mci%gi(i) = vamp_probability (mci%grids%grids(i), mci%x(:,i))
       else
          mci%gi(i) = 0
       end if
    end do
    !$OMP END DO
    !$OMP END PARALLEL
    mci%g = 0
    if (mci%gi(c) /= 0) then
       do i = 1, mci%mci%n_channel
          if (mci%w(i) /= 0 .and. mci%f(i) /= 0) then
             mci%g = mci%g + mci%w(i) * mci%gi(i) / mci%f(i)
          end if
       end do
    end if
    if (mci%g /= 0) then
       mci%mci_weight = mci%gi(c) / mci%g
    else
       mci%mci_weight = 0
    end if
  end subroutine mci_vamp_instance_compute_weight
    
  subroutine mci_vamp_instance_record_integrand (mci, integrand)
    class(mci_vamp_instance_t), intent(inout) :: mci
    real(default), intent(in) :: integrand
    mci%integrand = integrand
  end subroutine mci_vamp_instance_record_integrand
  
  function mci_vamp_instance_get_event_weight (mci) result (value)
    class(mci_vamp_instance_t), intent(in) :: mci
    real(default) :: value
    if (mci%vamp_weight_set) then
       value = mci%vamp_weight
    else
       call msg_bug ("VAMP: attempt to read undefined event weight")
    end if
  end function mci_vamp_instance_get_event_weight
   
  function mci_vamp_instance_get_event_excess (mci) result (value)
    class(mci_vamp_instance_t), intent(in) :: mci
    real(default) :: value
    if (mci%vamp_weight_set) then
       value = mci%vamp_excess
    else
       call msg_bug ("VAMP: attempt to read undefined event excess weight")
    end if
  end function mci_vamp_instance_get_event_excess
   
  subroutine handle_vamp_exception (exc, verbose)
    type(exception), intent(in) :: exc
    logical, intent(in) :: verbose
    integer :: exc_level
    if (verbose) then
       exc_level = EXC_INFO
    else
       exc_level = EXC_ERROR
    end if
    if (exc%level >= exc_level) then
       write (msg_buffer, "(A,':',1x,A)")  trim (exc%origin), trim (exc%message)
       select case (exc%level)
       case (EXC_INFO);  call msg_message ()
       case (EXC_WARN);  call msg_warning ()
       case (EXC_ERROR); call msg_error ()
       case (EXC_FATAL)
          if (signal_is_pending ()) then
             call msg_message ()
          else
             call msg_fatal ()
          end if
       end select
    end if
  end subroutine handle_vamp_exception
  

  subroutine mci_vamp_instance_final (object)
    class(mci_vamp_instance_t), intent(inout) :: object
    if (object%allocate_global_history) then
       if (associated (object%v_history)) then
          call vamp_delete_history (object%v_history)
          deallocate (object%v_history)
       end if
    end if
    if (object%allocate_channel_history) then
       if (associated (object%v_histories)) then
          call vamp_delete_history (object%v_histories)
          deallocate (object%v_histories)
       end if
    end if
    if (object%grids_defined) then
       call vamp_delete_grids (object%grids)
       object%grids_defined = .false.       
    end if
  end subroutine mci_vamp_instance_final
  
  subroutine mci_vamp_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (mci_vamp_1, "mci_vamp_1", &
         "one-dimensional integral", &
         u, results)
    call test (mci_vamp_2, "mci_vamp_2", &
         "multiple iterations", &
         u, results)
    call test (mci_vamp_3, "mci_vamp_3", &
         "grid adaptation", &
         u, results)
    call test (mci_vamp_4, "mci_vamp_4", &
         "two-dimensional integration", &
         u, results)
    call test (mci_vamp_5, "mci_vamp_5", &
         "two-dimensional integration", &
         u, results)
    call test (mci_vamp_6, "mci_vamp_6", &
         "weight adaptation", &
         u, results)
    call test (mci_vamp_7, "mci_vamp_7", &
         "use channel equivalences", &
         u, results)
    call test (mci_vamp_8, "mci_vamp_8", &
         "integration passes", &
         u, results)
    call test (mci_vamp_9, "mci_vamp_9", &
         "weighted event", &
         u, results)
    call test (mci_vamp_10, "mci_vamp_10", &
         "grids I/O", &
         u, results)
   call test (mci_vamp_11, "mci_vamp_11", &
        "weighted events with grid I/O", &
        u, results)
   call test (mci_vamp_12, "mci_vamp_12", &
        "unweighted events with grid I/O", &
        u, results)
    call test (mci_vamp_13, "mci_vamp_13", &
         "updating integration results", &
         u, results)
    call test (mci_vamp_14, "mci_vamp_14", &
         "accuracy goal", &
         u, results)
    call test (mci_vamp_15, "mci_vamp_15", &
         "VAMP history", &
         u, results)
  end subroutine mci_vamp_test
  
  subroutine test_sampler_1_write (object, unit, testflag)
    class(test_sampler_1_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    select case (object%mode)
    case (1)
       write (u, "(1x,A)") "Test sampler: f(x) = 3 x^2"
    case (2)
       write (u, "(1x,A)") "Test sampler: f(x) = 11 x^10"
    case (3)
       write (u, "(1x,A)") "Test sampler: f(x) = 11 x^10 * 2 * cos^2 (2 pi y)"
    end select
  end subroutine test_sampler_1_write
  
  subroutine test_sampler_1_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_1_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    if (allocated (sampler%x))  deallocate (sampler%x)
    allocate (sampler%x (size (x_in)))
    sampler%x = x_in
    select case (sampler%mode)
    case (1)
       sampler%val = 3 * x_in(1) ** 2
    case (2)
       sampler%val = 11 * x_in(1) ** 10
    case (3)
       sampler%val = 11 * x_in(1) ** 10 * 2 * cos (twopi * x_in(2)) ** 2
    end select
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_1_evaluate

  function test_sampler_1_is_valid (sampler) result (valid)
    class(test_sampler_1_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_1_is_valid
  
  subroutine test_sampler_1_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_1_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    if (allocated (sampler%x))  deallocate (sampler%x)
    allocate (sampler%x (size (x_in)))
    sampler%x = x_in
    sampler%val = val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_1_rebuild

  subroutine test_sampler_1_fetch (sampler, val, x, f)
    class(test_sampler_1_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_1_fetch
    
  subroutine test_sampler_2_write (object, unit, testflag)
    class(test_sampler_2_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)") "Two-channel test sampler 2"
  end subroutine test_sampler_2_write
  
  subroutine test_sampler_2_compute (sampler, c, x_in)
    class(test_sampler_2_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default) :: xx, yy, uu, vv
    if (.not. allocated (sampler%x)) &
         allocate (sampler%x (size (x_in), 2))
    if (.not. allocated (sampler%f)) &
         allocate (sampler%f (2))
    select case (c)
    case (1)
       xx = x_in(1)
       yy = x_in(2)
       uu = xx * yy
       vv = (1 + log (xx/yy) / log (xx*yy)) / 2
    case (2)
       uu = x_in(1)
       vv = x_in(2)
       xx = uu ** vv
       yy = uu ** (1 - vv)
    end select
    sampler%val = (2 * sin (pi * xx) * sin (pi * yy)) ** 2 &
         + 2 * sin (pi * vv) ** 2
    sampler%f(1) = 1
    sampler%f(2) = abs (log (uu))
    sampler%x(:,1) = [xx, yy]
    sampler%x(:,2) = [uu, vv]
  end subroutine test_sampler_2_compute

  subroutine test_sampler_2_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_2_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_2_evaluate

  function test_sampler_2_is_valid (sampler) result (valid)
    class(test_sampler_2_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_2_is_valid
  
  subroutine test_sampler_2_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_2_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_2_rebuild

  subroutine test_sampler_2_fetch (sampler, val, x, f)
    class(test_sampler_2_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%val
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_2_fetch
    
  subroutine test_sampler_3_write (object, unit, testflag)
    class(test_sampler_3_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)") "Two-channel test sampler 3"
    write (u, "(3x,A,F5.2)")  "a = ", object%a
    write (u, "(3x,A,F5.2)")  "b = ", object%b
  end subroutine test_sampler_3_write
  
  subroutine test_sampler_3_compute (sampler, c, x_in)
    class(test_sampler_3_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default) :: u, v, xx
    if (.not. allocated (sampler%x)) &
         allocate (sampler%x (size (x_in), 2))
    if (.not. allocated (sampler%f)) &
         allocate (sampler%f (2))
    select case (c)
    case (1)
       u = x_in(1)
       xx = u ** 0.2_default
       v = (1 - xx) ** 5._default
    case (2)
       v = x_in(1)
       xx = 1 - v ** 0.2_default
       u = xx ** 5._default
    end select
    sampler%val = sampler%a * 5 * xx ** 4 + sampler%b * 5 * (1 - xx) ** 4
    sampler%f(1) = 0.2_default * u ** (-0.8_default)
    sampler%f(2) = 0.2_default * v ** (-0.8_default)
    sampler%x(:,1) = [u]
    sampler%x(:,2) = [v]
  end subroutine test_sampler_3_compute

  subroutine test_sampler_3_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_3_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_3_evaluate

  function test_sampler_3_is_valid (sampler) result (valid)
    class(test_sampler_3_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_3_is_valid
  
  subroutine test_sampler_3_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_3_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_3_rebuild

  subroutine test_sampler_3_fetch (sampler, val, x, f)
    class(test_sampler_3_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%val
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_3_fetch
    
  subroutine mci_vamp_1 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_1"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(single channel)"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 1)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_1_t :: sampler)
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_calls = 1000"
    write (u, "(A)")  "   (lower precision to avoid"
    write (u, "(A)")  "      numerical noise)"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass ()
    end select
    call mci%integrate (mci_instance, sampler, 1, 1000, pacify = .true.)
    call mci%write (u, .true.)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u, .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_1"

  end subroutine mci_vamp_1

  subroutine mci_vamp_2 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_2"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(single channel)" 
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 1)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_1_t :: sampler)
    select type (sampler)
    type is (test_sampler_1_t)
       sampler%mode = 2
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 100"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .false.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 100)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_2"

  end subroutine mci_vamp_2

  subroutine mci_vamp_3 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_3"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(single channel)" 
    write (u, "(A)")  "*            and adapt grid"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 1)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_1_t :: sampler)
    select type (sampler)
    type is (test_sampler_1_t)
       sampler%mode = 2
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 100"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 100)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_3"

  end subroutine mci_vamp_3

  subroutine mci_vamp_4 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_4"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions &
         &(single channel)" 
    write (u, "(A)")  "*            and adapt grid"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 1)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_1_t :: sampler)
    select type (sampler)
    type is (test_sampler_1_t)
       sampler%mode = 3
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_4"

  end subroutine mci_vamp_4

  subroutine mci_vamp_5 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_5"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions &
         &(two channels)" 
    write (u, "(A)")  "*            and adapt grid"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_2_t :: sampler)
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_5"

  end subroutine mci_vamp_5

  subroutine mci_vamp_6 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_6"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(two channels)" 
    write (u, "(A)")  "*            and adapt weights"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_3_t :: sampler)
    select type (sampler)
    type is (test_sampler_3_t)
       sampler%a = 0.9_default
       sampler%b = 0.1_default
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_weights = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()
    deallocate (mci_instance)
    deallocate (mci)

    write (u, "(A)")
    write (u, "(A)")  "* Re-initialize with chained channels"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    call mci%declare_chains ([1,1])
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_weights = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_6"

  end subroutine mci_vamp_6

  subroutine mci_vamp_7 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    type(phs_channel_t), dimension(:), allocatable :: channel
    class(rng_t), allocatable :: rng
    real(default), dimension(:,:), allocatable :: x
    integer :: u_grid, iostat, i, div, ch
    character(16) :: buffer
    
    write (u, "(A)")  "* Test output: mci_vamp_7"
    write (u, "(A)")  "*   Purpose: check effect of channel equivalences"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_3_t :: sampler)
    select type (sampler)
    type is (test_sampler_3_t)
       sampler%a = 0.7_default
       sampler%b = 0.3_default
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 2 and n_calls = 1000, &
         &adapt grids"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 2, 1000)

    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)") "* Write grids and extract binning"
    write (u, "(A)")

    u_grid = free_unit ()
    open (u_grid, status = "scratch", action = "readwrite")
    select type (mci_instance)
    type is (mci_vamp_instance_t)
       call vamp_write_grids (mci_instance%grids, u_grid)
    end select
    rewind (u_grid)
    allocate (x (0:20, 2))
    do div = 1, 2
       FIND_BINS1: do
          read (u_grid, "(A)")  buffer
          if (trim (adjustl (buffer)) == "begin d%x") then
             do
                read (u_grid, *, iostat = iostat)  i, x(i,div)
                if (iostat /= 0)  exit FIND_BINS1
             end do
          end if
       end do FIND_BINS1
    end do
    close (u_grid)

    write (u, "(1x,A,L1)")  "Equal binning in both channels = ", &
         all (x(:,1) == x(:,2))
    deallocate (x)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()
    deallocate (mci_instance)
    deallocate (mci)

    write (u, "(A)")
    write (u, "(A)")  "* Re-initialize integrator, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .true.
       call mci%set_grid_parameters (grid_par)
    end select
    
    write (u, "(A)")  "* Define equivalences"
    write (u, "(A)")

    allocate (channel (2))
    do ch = 1, 2
       allocate (channel(ch)%eq (2))
       do i = 1, 2
          associate (eq => channel(ch)%eq(i))
            call eq%init (1)
            eq%c = i
            eq%perm = [1]
            eq%mode = [0]
          end associate
       end do
       write (u, "(1x,I0,':')", advance = "no")  ch
       call channel(ch)%write (u)
    end do
    call mci%declare_equivalences (channel, dim_offset = 0)

    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 2 and n_calls = 1000, &
         &adapt grids"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 2, 1000)
 
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)") "* Write grids and extract binning"
    write (u, "(A)")

    u_grid = free_unit ()
    open (u_grid, status = "scratch", action = "readwrite")
    select type (mci_instance)
    type is (mci_vamp_instance_t)
       call vamp_write_grids (mci_instance%grids, u_grid)
    end select
    rewind (u_grid)
    allocate (x (0:20, 2))
    do div = 1, 2
       FIND_BINS2: do
          read (u_grid, "(A)")  buffer
          if (trim (adjustl (buffer)) == "begin d%x") then
             do
                read (u_grid, *, iostat = iostat)  i, x(i,div)
                if (iostat /= 0)  exit FIND_BINS2
             end do
          end if
       end do FIND_BINS2
    end do
    close (u_grid)

    write (u, "(1x,A,L1)")  "Equal binning in both channels = ", &
         all (x(:,1) == x(:,2))
    deallocate (x)
    

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_7"

  end subroutine mci_vamp_7

  subroutine mci_vamp_8 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_8"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(two channels)" 
    write (u, "(A)")  "*            in three passes"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_3_t :: sampler)
    select type (sampler)
    type is (test_sampler_3_t)
       sampler%a = 0.9_default
       sampler%b = 0.1_default
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with grid and weight adaptation"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true., adapt_weights = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with grid adaptation"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate without adaptation"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass ()
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Contents of mci_instance:"
    write (u, "(A)")
    
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_8"

  end subroutine mci_vamp_8

  subroutine mci_vamp_9 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_9"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions &
         &(two channels)" 
    write (u, "(A)")  "*            and generate a weighted event"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_2_t :: sampler)
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    call mci%add_pass ()
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Generate a weighted event"
    write (u, "(A)")

    call mci_instance%init_simulation ()
    call mci%generate_weighted_event (mci_instance, sampler)

    write (u, "(1x,A)")  "MCI instance:"
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final_simulation ()
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_9"

  end subroutine mci_vamp_9

  subroutine mci_vamp_10 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    type(string_t) :: file1, file2
    character(80) :: buffer1, buffer2
    integer :: u1, u2, iostat1, iostat2
    logical :: equal, success
   
    write (u, "(A)")  "* Test output: mci_vamp_10"
    write (u, "(A)")  "*   Purpose: write and read VAMP grids"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    mci%md5sum = "1234567890abcdef1234567890abcdef"

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_2_t :: sampler)
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    call mci%add_pass ()
    call mci%integrate (mci_instance, sampler, 1, 1000)
 
    write (u, "(A)")  "* Write grids to file"
    write (u, "(A)")
 
    file1 = "mci_vamp_10.1"
    select type (mci)
    type is (mci_vamp_t)
       call mci%set_grid_filename (file1)
       call mci%write_grids (mci_instance)
    end select

    call mci_instance%final ()
    call mci%final ()
    deallocate (mci)
    
    write (u, "(A)")  "* Read grids from file"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    mci%md5sum = "1234567890abcdef1234567890abcdef"

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)

    select type (mci)
    type is (mci_vamp_t)
       call mci%set_grid_filename (file1)
       call mci%add_pass ()
       call mci%current_pass%configure (1, 1000, &
            mci%min_calls, &
            mci%grid_par%min_bins, mci%grid_par%max_bins, &
            mci%grid_par%min_calls_per_channel * mci%n_channel)
       call mci%read_grids_header (success)
       call mci%compute_md5sum ()
       call mci%read_grids_data (mci_instance, read_integrals = .true.)
    end select
    write (u, "(1x,A,L1)")  "success = ", success
    
    write (u, "(A)")
    write (u, "(A)")  "* Write grids again"
    write (u, "(A)")

    file2 = "mci_vamp_10.2"
    select type (mci)
    type is (mci_vamp_t)
       call mci%set_grid_filename (file2)
       call mci%write_grids (mci_instance)
    end select

    u1 = free_unit ()
    open (u1, file = char (file1) // ".vg", action = "read", status = "old")
    u2 = free_unit ()
    open (u2, file = char (file2) // ".vg", action = "read", status = "old")

    equal = .true.
    iostat1 = 0
    iostat2 = 0
    do while (equal .and. iostat1 == 0 .and. iostat2 == 0)
       read (u1, "(A)", iostat = iostat1)  buffer1
       read (u2, "(A)", iostat = iostat2)  buffer2
       equal = buffer1 == buffer2 .and. iostat1 == iostat2
    end do
    close (u1)
    close (u2)
    
    if (equal) then
       write (u, "(1x,A)")  "Success: grid files are identical"
    else
       write (u, "(1x,A)")  "Failure: grid files differ"
    end if

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_10"

  end subroutine mci_vamp_10

  subroutine mci_vamp_11 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_11"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions &
         &(two channels)" 
    write (u, "(A)")  "*            and generate a weighted event"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
       call mci%set_grid_filename (var_str ("mci_vamp_11"))
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_2_t :: sampler)

    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    call mci%add_pass ()
    call mci%integrate (mci_instance, sampler, 1, 1000)
 
    write (u, "(A)")  "* Reset instance"
    write (u, "(A)")

    call mci_instance%final ()
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)

    write (u, "(A)")  "* Generate a weighted event"
    write (u, "(A)")

    call mci_instance%init_simulation ()
    call mci%generate_weighted_event (mci_instance, sampler)

    write (u, "(A)")  "* Cleanup"

    call mci_instance%final_simulation ()
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_11"

  end subroutine mci_vamp_11

  subroutine mci_vamp_12 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_12"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions &
         &(two channels)" 
    write (u, "(A)")  "*            and generate an unweighted event"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
       call mci%set_grid_filename (var_str ("mci_vamp_12"))
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_2_t :: sampler)

    write (u, "(A)")  "* Integrate with n_it = 3 and n_calls = 1000"
    write (u, "(A)")
    
    call mci%add_pass ()
    call mci%integrate (mci_instance, sampler, 1, 1000)
 
    write (u, "(A)")  "* Reset instance"
    write (u, "(A)")

    call mci_instance%final ()
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)

    write (u, "(A)")  "* Generate an unweighted event"
    write (u, "(A)")

    call mci_instance%init_simulation ()
    call mci%generate_unweighted_event (mci_instance, sampler)

    write (u, "(1x,A)")  "MCI instance:"
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final_simulation ()
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_12"

  end subroutine mci_vamp_12

  subroutine mci_vamp_13 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci, mci_ref
    logical :: success
    
    write (u, "(A)")  "* Test output: mci_vamp_13"
    write (u, "(A)")  "*   Purpose: match and update integrators"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator with no passes"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
    end select
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize reference"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci_ref)
    call mci_ref%set_dimensions (2, 2)
    select type (mci_ref)
    type is (mci_vamp_t)
       call mci_ref%set_grid_parameters (grid_par)
    end select
    
    select type (mci_ref)
    type is (mci_vamp_t)
       call mci_ref%add_pass (adapt_grids = .true.)
       call mci_ref%current_pass%configure (2, 1000, 0, 1, 5, 0)
       mci_ref%current_pass%calls = [77, 77]
       mci_ref%current_pass%integral = [1.23_default, 3.45_default]
       mci_ref%current_pass%error = [0.23_default, 0.45_default]
       mci_ref%current_pass%efficiency = [0.1_default, 0.6_default]
       mci_ref%current_pass%integral_defined = .true.

       call mci_ref%add_pass ()
       call mci_ref%current_pass%configure (2, 2000, 0, 1, 7, 0)
       mci_ref%current_pass%calls = [99, 0]
       mci_ref%current_pass%integral = [7.89_default, 0._default]
       mci_ref%current_pass%error = [0.89_default, 0._default]
       mci_ref%current_pass%efficiency = [0.86_default, 0._default]
       mci_ref%current_pass%integral_defined = .true.
    end select

    call mci_ref%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Update integrator (no-op, should succeed)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Add pass to integrator"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
       call mci%current_pass%configure (2, 1000, 0, 1, 5, 0)
       mci%current_pass%calls = [77, 77]
       mci%current_pass%integral = [1.23_default, 3.45_default]
       mci%current_pass%error = [0.23_default, 0.45_default]
       mci%current_pass%efficiency = [0.1_default, 0.6_default]
       mci%current_pass%integral_defined = .true.
    end select

    write (u, "(A)")  "* Update integrator (no-op, should succeed)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Add pass to integrator, wrong parameters"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass ()
       call mci%current_pass%configure (2, 1000, 0, 1, 7, 0)
    end select

    write (u, "(A)")  "* Update integrator (should fail)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset and add passes to integrator"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%reset ()
       call mci%add_pass (adapt_grids = .true.)
       call mci%current_pass%configure (2, 1000, 0, 1, 5, 0)
       mci%current_pass%calls = [77, 77]
       mci%current_pass%integral = [1.23_default, 3.45_default]
       mci%current_pass%error = [0.23_default, 0.45_default]
       mci%current_pass%efficiency = [0.1_default, 0.6_default]
       mci%current_pass%integral_defined = .true.
       
       call mci%add_pass ()
       call mci%current_pass%configure (2, 2000, 0, 1, 7, 0)
    end select

    write (u, "(A)")  "* Update integrator (should succeed)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Update again (no-op, should succeed)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Add extra result to integrator"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       mci%current_pass%calls(2) = 1234
    end select

    write (u, "(A)")  "* Update integrator (should fail)"
    write (u, "(A)")

    select type (mci)
    type is (mci_vamp_t)
       call mci%update_from_ref (mci_ref, success)
    end select

    write (u, "(1x,A,L1)")  "success = ", success
    write (u, "(A)")
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci%final ()
    call mci_ref%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_13"

  end subroutine mci_vamp_13

  subroutine mci_vamp_14 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_14"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(single channel)" 
    write (u, "(A)")  "*            and check accuracy goal"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 1)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%use_vamp_equivalences = .false.
       grid_par%accuracy_goal = 5E-2_default
       call mci%set_grid_parameters (grid_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_1_t :: sampler)
    select type (sampler)
    type is (test_sampler_1_t)
       sampler%mode = 2
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_it = 5 and n_calls = 100"
    write (u, "(A)")
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 5, 100)
    call mci%write (u)
 
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_14"

  end subroutine mci_vamp_14

  subroutine mci_vamp_15 (u)
    integer, intent(in) :: u
    type(grid_parameters_t) :: grid_par
    type(history_parameters_t) :: history_par
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_vamp_15"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension &
         &(two channels)" 
    write (u, "(A)")  "*            in three passes, show history"
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize integrator, sampler, instance"
    write (u, "(A)")

    history_par%channel = .true.

    allocate (mci_vamp_t :: mci)
    call mci%set_dimensions (1, 2)
    select type (mci)
    type is (mci_vamp_t)
       grid_par%stratified = .false.
       grid_par%use_vamp_equivalences = .false.
       call mci%set_grid_parameters (grid_par)
       call mci%set_history_parameters (history_par)
    end select
    
    allocate (rng_tao_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)

    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_3_t :: sampler)
    select type (sampler)
    type is (test_sampler_3_t)
       sampler%a = 0.9_default
       sampler%b = 0.1_default
    end select
    call sampler%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Pass 1: grid and weight adaptation"
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true., adapt_weights = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
 
    write (u, "(A)")
    write (u, "(A)")  "* Pass 2: grid adaptation"
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass (adapt_grids = .true.)
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)
 
    write (u, "(A)")
    write (u, "(A)")  "* Pass 3: without adaptation"
    
    select type (mci)
    type is (mci_vamp_t)
       call mci%add_pass ()
    end select
    call mci%integrate (mci_instance, sampler, 3, 1000)

    write (u, "(A)")
    write (u, "(A)")  "* Contents of MCI record, with history"
    write (u, "(A)")
    
    call mci%write (u)
    select type (mci)
    type is (mci_vamp_t)
       call mci%write_history (u)
    end select
 
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_vamp_15"

  end subroutine mci_vamp_15


end module mci_vamp
