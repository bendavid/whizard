! WHIZARD 2.6.2 Dec 13 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module mci_vamp2

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: pac_fmt
  use format_utils, only: write_separator, write_indent
  use format_defs, only: FMT_12, FMT_14, FMT_17, FMT_19
  use diagnostics
  use md5
  use phs_base
  use rng_base

  use mci_base

  use vegas, only: VEGAS_MODE_IMPORTANCE, VEGAS_MODE_IMPORTANCE_ONLY
  use vamp2



  implicit none
  private

  public :: mci_vamp2_config_t
  public :: mci_vamp2_t
  public :: mci_vamp2_instance_t

  type, extends (vamp2_func_t) :: mci_vamp2_func_t
     private
     real(default) :: integrand = 0.
     class(mci_sampler_t), pointer :: sampler => null ()
     class(mci_vamp2_instance_t), pointer :: instance => null ()
   contains
     procedure, public :: set_workspace => mci_vamp2_func_set_workspace
     procedure, public :: get_probabilities => mci_vamp2_func_get_probabilities
     procedure, public :: get_weight => mci_vamp2_func_get_weight
     procedure, public :: set_integrand => mci_vamp2_func_set_integrand
     procedure, public :: evaluate_maps => mci_vamp2_func_evaluate_maps
     procedure, public :: evaluate_func => mci_vamp2_func_evaluate_func
  end type mci_vamp2_func_t

  type, extends (vamp2_config_t) :: mci_vamp2_config_t
     logical :: use_channel_equivalences = .false.
  end type mci_vamp2_config_t

  type :: list_pass_t
     type(pass_t), pointer :: first => null ()
     type(pass_t), pointer :: current => null ()
   contains
     procedure :: final => list_pass_final
     procedure :: add => list_pass_add
     procedure :: update_from_ref => list_pass_update_from_ref
     procedure :: write => list_pass_write
  end type list_pass_t

  type :: pass_t
     integer :: i_pass = 0
     integer :: i_first_it = 0
     integer :: n_it = 0
     integer :: n_calls = 0
     logical :: adapt_grids = .false.
     logical :: adapt_weights = .false.
     logical :: is_final_pass = .false.
     logical :: integral_defined = .false.
     integer, dimension(:), allocatable :: calls
     integer, dimension(:), allocatable :: calls_valid
     real(default), dimension(:), allocatable :: integral
     real(default), dimension(:), allocatable :: error
     real(default), dimension(:), allocatable :: efficiency
     type(pass_t), pointer :: next => null ()
   contains
     procedure :: write => pass_write
     procedure :: read => pass_read
     procedure :: configure => pass_configure
     procedure :: update => pass_update
     procedure :: get_integration_index => pass_get_integration_index
     procedure :: get_calls => pass_get_calls
     procedure :: get_calls_valid => pass_get_calls_valid
     procedure :: get_integral => pass_get_integral
     procedure :: get_error => pass_get_error
     procedure :: get_efficiency => pass_get_efficiency
  end type pass_t

  type, extends(mci_t) :: mci_vamp2_t
     type(mci_vamp2_config_t) :: config
     type(vamp2_t) :: integrator
     logical :: integrator_defined = .false.
     logical :: integrator_from_file = .false.
     logical :: adapt_grids = .false.
     logical :: adapt_weights = .false.
     integer :: n_adapt_grids = 0
     integer :: n_adapt_weights = 0
     integer :: n_calls = 0
     type(list_pass_t) :: list_pass
     logical :: rebuild = .true.
     logical :: check_grid_file = .true.
     logical :: integrator_filename_set = .false.
     logical :: negative_weights = .false.
     logical :: verbose = .false.
     logical :: pass_complete = .false.
     logical :: it_complete = .false.
     type(string_t) :: integrator_filename
     character(32) :: md5sum_adapted = ""
   contains
     procedure, public :: final => mci_vamp2_final
     procedure, public :: write => mci_vamp2_write
     procedure, public :: compute_md5sum => mci_vamp2_compute_md5sum
     procedure, public :: get_md5sum => mci_vamp2_get_md5sum
     procedure, public :: startup_message => mci_vamp2_startup_message
     procedure, public :: write_log_entry => mci_vamp2_write_log_entry
     procedure, public :: record_index => mci_vamp2_record_index
     procedure, public :: set_config => mci_vamp2_set_config
     procedure, public :: set_rebuild_flag => mci_vamp2_set_rebuild_flag
     procedure, public :: set_integrator_filename => mci_vamp2_set_integrator_filename
     procedure :: prepend_integrator_path => mci_vamp2_prepend_integrator_path
     procedure, public :: declare_flat_dimensions => mci_vamp2_declare_flat_dimensions
     procedure, public :: declare_equivalences => mci_vamp2_declare_equivalences
     procedure, public :: allocate_instance => mci_vamp2_allocate_instance
     procedure, public :: add_pass => mci_vamp2_add_pass
     procedure, public :: update_from_ref => mci_vamp2_update_from_ref
     procedure, public :: update => mci_vamp2_update
     procedure :: write_grids => mci_vamp2_write_grids
     procedure :: read_header => mci_vamp2_read_header
     procedure :: read_data => mci_vamp2_read_data
     procedure :: read_grids => mci_vamp2_read_grids
     procedure, public :: init_integrator => mci_vamp2_init_integrator
     procedure, public :: reset_result => mci_vamp2_reset_result
     procedure, public :: set_calls => mci_vamp2_set_calls
     procedure, private :: init_integration => mci_vamp2_init_integration
     procedure, public :: integrate => mci_vamp2_integrate
     procedure, public :: prepare_simulation => mci_vamp2_prepare_simulation
     procedure, public :: generate_weighted_event => mci_vamp2_generate_weighted_event
     procedure, public :: generate_unweighted_event => mci_vamp2_generate_unweighted_event
     procedure, public :: rebuild_event => mci_vamp2_rebuild_event
  end type mci_vamp2_t

  type, extends (mci_instance_t) :: mci_vamp2_instance_t
     class(mci_vamp2_func_t), allocatable :: func
     real(default), dimension(:), allocatable :: gi
     integer :: n_events = 0
     logical :: event_generated = .false.
     real(default) :: event_weight = 0.
     real(default) :: event_excess = 0.
     real(default) :: event_rescale_f_max = 1.
     real(default), dimension(:), allocatable :: event_x
   contains
     procedure, public :: write => mci_vamp2_instance_write
     procedure, public :: final => mci_vamp2_instance_final
     procedure, public :: init => mci_vamp2_instance_init
     procedure, public :: set_workspace => mci_vamp2_instance_set_workspace
     procedure, public :: compute_weight => mci_vamp2_instance_compute_weight
     procedure, public :: record_integrand => mci_vamp2_instance_record_integrand
     procedure, public :: init_simulation => mci_vamp2_instance_init_simulation
     procedure, public :: final_simulation => mci_vamp2_instance_final_simulation
     procedure, public :: get_event_weight => mci_vamp2_instance_get_event_weight
     procedure, public :: get_event_excess => mci_vamp2_instance_get_event_excess
  end type mci_vamp2_instance_t


  interface operator (.matches.)
     module procedure pass_matches
  end interface operator (.matches.)
  interface operator (.matches.)
     module procedure real_matches
  end interface operator (.matches.)

contains

  subroutine mci_vamp2_func_set_workspace (self, instance, sampler)
    class(mci_vamp2_func_t), intent(inout) :: self
    class(mci_vamp2_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    self%instance => instance
    self%sampler => sampler
  end subroutine mci_vamp2_func_set_workspace

  function mci_vamp2_func_get_probabilities (self) result (gi)
    class(mci_vamp2_func_t), intent(inout) :: self
    real(default), dimension(self%n_channel) :: gi
    gi = self%gi
  end function mci_vamp2_func_get_probabilities

  real(default) function mci_vamp2_func_get_weight (self) result (g)
    class(mci_vamp2_func_t), intent(in) :: self
    g = self%g
  end function mci_vamp2_func_get_weight

  subroutine mci_vamp2_func_set_integrand (self, integrand)
    class(mci_vamp2_func_t), intent(inout) :: self
    real(default), intent(in) :: integrand
    self%integrand = integrand
  end subroutine mci_vamp2_func_set_integrand

  subroutine mci_vamp2_func_evaluate_maps (self, x)
    class(mci_vamp2_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    select type (self)
    type is (mci_vamp2_func_t)
       call self%instance%evaluate (self%sampler, self%current_channel, x)
    end select
    self%valid_x = self%instance%valid
    self%xi = self%instance%x
    self%det = self%instance%f
  end subroutine mci_vamp2_func_evaluate_maps

  real(default) function mci_vamp2_func_evaluate_func (self, x) result (f)
    class(mci_vamp2_func_t), intent(in) :: self
    real(default), dimension(:), intent(in) :: x
    f = self%integrand
    if (signal_is_pending ()) then
       call msg_message ("MCI VAMP2: function evalutae func: signal received")
       call terminate_now_if_signal ()
    end if
    call terminate_now_if_single_event ()
  end function mci_vamp2_func_evaluate_func

  subroutine list_pass_final (self)
    class(list_pass_t), intent(inout) :: self
    type(pass_t), pointer :: current
    current => self%first
    do while (associated (current))
       self%first => current%next
       deallocate (current)
       current => self%first
    end do
  end subroutine list_pass_final

  subroutine list_pass_add (self, adapt_grids, adapt_weights, final_pass)
    class(list_pass_t), intent(inout) :: self
    logical, intent(in), optional :: adapt_grids, adapt_weights, final_pass
    type(pass_t), pointer :: new_pass
    allocate (new_pass)
    new_pass%i_pass = 1
    new_pass%i_first_it = 1
    new_pass%adapt_grids = .false.; if (present (adapt_grids)) &
         & new_pass%adapt_grids = adapt_grids
    new_pass%adapt_weights = .false.; if (present (adapt_weights)) &
         & new_pass%adapt_weights = adapt_weights
    new_pass%is_final_pass = .false.; if (present (final_pass)) &
         & new_pass%is_final_pass = final_pass
    if (.not. associated (self%first)) then
       self%first => new_pass
    else
       new_pass%i_pass = new_pass%i_pass + self%current%i_pass
       new_pass%i_first_it = self%current%i_first_it + self%current%n_it
       self%current%next => new_pass
    end if
    self%current => new_pass
  end subroutine list_pass_add

  subroutine list_pass_update_from_ref (self, ref, success)
    class(list_pass_t), intent(inout) :: self
    type(list_pass_t), intent(in) :: ref
    logical, intent(out) :: success
    type(pass_t), pointer :: current, ref_current
    current => self%first
    ref_current => ref%first
    success = .true.
    do while (success .and. associated (current))
       if (associated (ref_current)) then
          if (associated (current%next)) then
             success = current .matches. ref_current
          else
             call current%update (ref_current, success)
          end if
          current => current%next
          ref_current => ref_current%next
       else
          success = .false.
       end if
    end do
  end subroutine list_pass_update_from_ref

  subroutine list_pass_write (self, unit, pacify)
    class(list_pass_t), intent(in) :: self
    integer, intent(in) :: unit
    logical, intent(in), optional :: pacify
    type(pass_t), pointer :: current
    current => self%first
    do while (associated (current))
       write (unit, "(1X,A)") "Integration pass:"
       call current%write (unit, pacify)
       current => current%next
    end do
  end subroutine list_pass_write

  subroutine pass_write (self, unit, pacify)
    class(pass_t), intent(in) :: self
    integer, intent(in) :: unit
    logical, intent(in), optional :: pacify
    integer :: u, i
    character(len=7) :: fmt
    call pac_fmt (fmt, FMT_17, FMT_14, pacify)
    u = given_output_unit (unit)
    write (u, "(3X,A,I0)") "n_it          = ", self%n_it
    write (u, "(3X,A,I0)") "n_calls       = ", self%n_calls
    write (u, "(3X,A,L1)") "adapt grids   = ", self%adapt_grids
    write (u, "(3X,A,L1)") "adapt weights = ", self%adapt_weights
    if (self%integral_defined) then
       write (u, "(3X,A)") "Results:  [it, calls, valid, integral, error, efficiency]"
       do i = 1, self%n_it
          write (u, "(5x,I0,2(1x,I0),3(1x," // fmt // "))") &
               i, self%calls(i), self%calls_valid(i), self%integral(i), self%error(i), &
               self%efficiency(i)
       end do
    else
       write (u, "(3x,A)")  "Results: [undefined]"
    end if
  end subroutine pass_write

  subroutine pass_read (self, u, n_pass, n_it)
    class(pass_t), intent(out) :: self
    integer, intent(in) :: u, n_pass, n_it
    integer :: i, j
    character(80) :: buffer
    self%i_pass = n_pass + 1
    self%i_first_it = n_it + 1
    call read_ival (u, self%n_it)
    call read_ival (u, self%n_calls)
    call read_lval (u, self%adapt_grids)
    call read_lval (u, self%adapt_weights)
    allocate (self%calls (self%n_it), source = 0)
    allocate (self%calls_valid (self%n_it), source = 0)
    allocate (self%integral (self%n_it), source = 0._default)
    allocate (self%error (self%n_it), source = 0._default)
    allocate (self%efficiency (self%n_it), source = 0._default)
    read (u, "(A)")  buffer
    select case (trim (adjustl (buffer)))
    case ("Results:  [it, calls, valid, integral, error, efficiency]")
       do i = 1, self%n_it
          read (u, *) &
               j, self%calls(i), self%calls_valid(i), self%integral(i), self%error(i), &
               self%efficiency(i)
       end do
       self%integral_defined = .true.
    case ("Results: [undefined]")
       self%integral_defined = .false.
    case default
       call msg_fatal ("Reading integration pass: corrupted file")
    end select
  end subroutine pass_read

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

  subroutine pass_configure (pass, n_it, n_calls, n_calls_min)
    class(pass_t), intent(inout) :: pass
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    integer, intent(in) :: n_calls_min
    pass%n_it = n_it
    pass%n_calls = max (n_calls, n_calls_min)
    if (pass%n_calls /= n_calls) then
       write (msg_buffer, "(A,I0)")  "VAMP2: too few calls, resetting " &
            // "n_calls to ", pass%n_calls
       call msg_warning ()
    end if
    allocate (pass%calls (n_it), source = 0)
    allocate (pass%calls_valid (n_it), source = 0)
    allocate (pass%integral (n_it), source = 0._default)
    allocate (pass%error (n_it), source = 0._default)
    allocate (pass%efficiency (n_it), source = 0._default)
  end subroutine pass_configure

  function pass_matches (pass, ref) result (ok)
    type(pass_t), intent(in) :: pass, ref
    integer :: n
    logical :: ok
    ok = .true.
    if (ok)  ok = pass%i_pass == ref%i_pass
    if (ok)  ok = pass%i_first_it == ref%i_first_it
    if (ok)  ok = pass%n_it == ref%n_it
    if (ok)  ok = pass%n_calls == ref%n_calls
    if (ok)  ok = pass%adapt_grids .eqv. ref%adapt_grids
    if (ok)  ok = pass%adapt_weights .eqv. ref%adapt_weights
    if (ok)  ok = pass%integral_defined .eqv. ref%integral_defined
    if (pass%integral_defined) then
       n = pass%n_it
       if (ok)  ok = all (pass%calls(:n) == ref%calls(:n))
       if (ok)  ok = all (pass%calls_valid(:n) == ref%calls_valid(:n))
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
    if (ok)  ok = pass%adapt_grids .eqv. ref%adapt_grids
    if (ok)  ok = pass%adapt_weights .eqv. ref%adapt_weights
    if (ok) then
       if (ref%integral_defined) then
          if (.not. allocated (pass%calls)) then
             allocate (pass%calls (pass%n_it), source = 0)
             allocate (pass%calls_valid (pass%n_it), source = 0)
             allocate (pass%integral (pass%n_it), source = 0._default)
             allocate (pass%error (pass%n_it), source = 0._default)
             allocate (pass%efficiency (pass%n_it), source = 0._default)
          end if
          n = count (pass%calls /= 0)
          n_ref = count (ref%calls /= 0)
          ok = n <= n_ref .and. n_ref <= pass%n_it
          if (ok)  ok = all (pass%calls(:n) == ref%calls(:n))
          if (ok)  ok = all (pass%calls_valid(:n) == ref%calls_valid(:n))
          if (ok)  ok = all (pass%integral(:n) .matches. ref%integral(:n))
          if (ok)  ok = all (pass%error(:n) .matches. ref%error(:n))
          if (ok)  ok = all (pass%efficiency(:n) .matches. ref%efficiency(:n))
          if (ok) then
             pass%calls(n+1:n_ref) = ref%calls(n+1:n_ref)
             pass%calls_valid(n+1:n_ref) = ref%calls_valid(n+1:n_ref)
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
    calls = 0
    if (n /= 0) then
       calls = pass%calls(n)
    end if
  end function pass_get_calls

  function pass_get_calls_valid (pass) result (valid)
    class(pass_t), intent(in) :: pass
    integer :: valid
    integer :: n
    n = pass%get_integration_index ()
    valid = 0
    if (n /= 0) then
       valid = pass%calls_valid(n)
    end if
  end function pass_get_calls_valid

  function pass_get_integral (pass) result (integral)
    class(pass_t), intent(in) :: pass
    real(default) :: integral
    integer :: n
    n = pass%get_integration_index ()
    integral = 0
    if (n /= 0) then
       integral = pass%integral(n)
    end if
  end function pass_get_integral

  function pass_get_error (pass) result (error)
    class(pass_t), intent(in) :: pass
    real(default) :: error
    integer :: n
    n = pass%get_integration_index ()
    error = 0
    if (n /= 0) then
       error = pass%error(n)
    end if
  end function pass_get_error

  function pass_get_efficiency (pass) result (efficiency)
    class(pass_t), intent(in) :: pass
    real(default) :: efficiency
    integer :: n
    n = pass%get_integration_index ()
    efficiency = 0
    if (n /= 0) then
       efficiency = pass%efficiency(n)
    end if
  end function pass_get_efficiency

  subroutine mci_vamp2_final (object)
    class(mci_vamp2_t), intent(inout) :: object
    call object%list_pass%final ()
    call object%base_final ()
  end subroutine mci_vamp2_final

  subroutine mci_vamp2_write (object, unit, pacify, md5sum_version)
    class(mci_vamp2_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1X,A)") "VAMP2 integrator:"
    call object%base_write (u, pacify, md5sum_version)
    write (u, "(1X,A)") "Grid config:"
    call object%config%write (u)
    write (u, "(3X,A,L1)") "Integrator defined   = ", object%integrator_defined
    write (u, "(3X,A,L1)") "Integrator from file = ", object%integrator_from_file
    write (u, "(3X,A,L1)") "Adapt grids          = ", object%adapt_grids
    write (u, "(3X,A,L1)") "Adapt weights        = ", object%adapt_weights
    write (u, "(3X,A,I0)") "No. of adapt grids   = ", object%n_adapt_grids
    write (u, "(3X,A,I0)") "No. of adapt weights = ", object%n_adapt_weights
    write (u, "(3X,A,L1)") "Verbose              = ", object%verbose
    call object%list_pass%write (u, pacify)
    if (object%md5sum_adapted /= "") then
       write (u, "(1X,A,A,A)")  "MD5 sum (including results) = '", &
            & object%md5sum_adapted, "'"
    end if
  end subroutine mci_vamp2_write

  subroutine mci_vamp2_compute_md5sum (mci, pacify)
    class(mci_vamp2_t), intent(inout) :: mci
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
  end subroutine mci_vamp2_compute_md5sum

  pure function mci_vamp2_get_md5sum (mci) result (md5sum)
    class(mci_vamp2_t), intent(in) :: mci
    character(32) :: md5sum
    if (mci%md5sum_adapted /= "") then
       md5sum = mci%md5sum_adapted
    else
       md5sum = mci%md5sum
    end if
  end function mci_vamp2_get_md5sum

  subroutine mci_vamp2_startup_message (mci, unit, n_calls)
    class(mci_vamp2_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    integer :: num_calls, n_bins
    num_calls = 0; if (present (n_calls)) num_calls = n_calls
    n_bins = mci%config%n_bins_max
    call mci%base_startup_message (unit = unit, n_calls = n_calls)
    if (mci%config%use_channel_equivalences) then
       write (msg_buffer, "(A,2(1x,I0,1x,A))") &
            "Integrator: Using channel equivalences"
       call msg_message (unit = unit)
    end if
    write (msg_buffer, "(A,2(1x,I0,1x,A),L1)") &
         "Integrator:", num_calls, &
         "initial calls,", n_bins, &
         "max. bins, stratified = ", &
         mci%config%stratified
    call msg_message (unit = unit)
    write (msg_buffer, "(A,2(1x,I0,1x,A))") &
         "Integrator: VAMP2"
    call msg_message (unit = unit)
  end subroutine mci_vamp2_startup_message

  subroutine mci_vamp2_write_log_entry (mci, u)
    class(mci_vamp2_t), intent(in) :: mci
    integer, intent(in) :: u
    write (u, "(1x,A)")  "MC Integrator is VAMP2"
    call write_separator (u)
    if (mci%config%use_channel_equivalences) then
       ! TODO
       write (u, "(3x,A)") "No channel equivalences have not been implemented yet."
    else
       write (u, "(3x,A)") "No channel equivalences have been used."
    end if
    call write_separator (u)
    call mci%write_chain_weights (u)
  end subroutine mci_vamp2_write_log_entry

  subroutine mci_vamp2_record_index (mci, i_mci)
    class(mci_vamp2_t), intent(inout) :: mci
    integer, intent(in) :: i_mci
    type(string_t) :: basename, suffix
    character(32) :: buffer
    if (mci%integrator_filename_set) then
       basename = mci%integrator_filename
       call split (basename, suffix, ".", back=.true.)
       write (buffer, "(I0)") i_mci
       if (basename /= "") then
          mci%integrator_filename = basename // "_m" // trim (buffer) // "." // suffix
       else
          mci%integrator_filename = suffix // "_m" // trim (buffer) // ".vg2"
       end if
    end if
  end subroutine mci_vamp2_record_index

  subroutine mci_vamp2_set_config (mci, config)
    class(mci_vamp2_t), intent(inout) :: mci
    type(mci_vamp2_config_t), intent(in) :: config
    mci%config = config
  end subroutine mci_vamp2_set_config

  subroutine mci_vamp2_set_rebuild_flag (mci, rebuild, check_grid_file)
    class(mci_vamp2_t), intent(inout) :: mci
    logical, intent(in) :: rebuild
    logical, intent(in) :: check_grid_file
    mci%rebuild = rebuild
    mci%check_grid_file = check_grid_file
  end subroutine mci_vamp2_set_rebuild_flag

  subroutine mci_vamp2_set_integrator_filename (mci, name, run_id)
    class(mci_vamp2_t), intent(inout) :: mci
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: run_id
    mci%integrator_filename = name // ".vg2"
    if (present (run_id)) then
       mci%integrator_filename = name // "." // run_id // ".vg2"
    end if
    mci%integrator_filename_set = .true.
  end subroutine mci_vamp2_set_integrator_filename

  subroutine mci_vamp2_prepend_integrator_path (mci, prefix)
    class(mci_vamp2_t), intent(inout) :: mci
    type(string_t), intent(in) :: prefix
    if (.not. mci%integrator_filename_set) then
       call msg_warning ("Cannot add prefix to invalid integrator filename!")
    end if
    mci%integrator_filename = prefix // "/" // mci%integrator_filename
  end subroutine mci_vamp2_prepend_integrator_path

  subroutine mci_vamp2_declare_flat_dimensions (mci, dim_flat)
    class(mci_vamp2_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: dim_flat
  end subroutine mci_vamp2_declare_flat_dimensions

  subroutine mci_vamp2_declare_equivalences (mci, channel, dim_offset)
    class(mci_vamp2_t), intent(inout) :: mci
    type(phs_channel_t), dimension(:), intent(in) :: channel
    integer, intent(in) :: dim_offset
  end subroutine mci_vamp2_declare_equivalences

  subroutine mci_vamp2_allocate_instance (mci, mci_instance)
    class(mci_vamp2_t), intent(in) :: mci
    class(mci_instance_t), intent(out), pointer :: mci_instance
    allocate (mci_vamp2_instance_t :: mci_instance)
  end subroutine mci_vamp2_allocate_instance

  subroutine mci_vamp2_add_pass (mci, adapt_grids, adapt_weights, final_pass)
    class(mci_vamp2_t), intent(inout) :: mci
    logical, intent(in), optional :: adapt_grids, adapt_weights, final_pass
    call mci%list_pass%add (adapt_grids, adapt_weights, final_pass)
  end subroutine mci_vamp2_add_pass

  subroutine mci_vamp2_update_from_ref (mci, mci_ref, success)
    class(mci_vamp2_t), intent(inout) :: mci
    class(mci_t), intent(in) :: mci_ref
    logical, intent(out) :: success
    select type (mci_ref)
    type is (mci_vamp2_t)
       call mci%list_pass%update_from_ref (mci_ref%list_pass, success)
       if (mci%list_pass%current%integral_defined) then
          mci%integral = mci%list_pass%current%get_integral ()
          mci%error = mci%list_pass%current%get_error ()
          mci%efficiency = mci%list_pass%current%get_efficiency ()
          mci%integral_known = .true.
          mci%error_known = .true.
          mci%efficiency_known = .true.
       end if
    end select
  end subroutine mci_vamp2_update_from_ref

  subroutine mci_vamp2_update (mci, u, success)
    class(mci_vamp2_t), intent(inout) :: mci
    integer, intent(in) :: u
    logical, intent(out) :: success
    character(80) :: buffer
    character(32) :: md5sum_file
    type(mci_vamp2_t) :: mci_file
    integer :: n_pass, n_it
    call read_sval (u, md5sum_file)
    success = .true.; if (mci%check_grid_file) &
       & success = (md5sum_file == mci%md5sum)
    if (success) then
       read (u, *)
       read (u, "(A)") buffer
       if (trim (adjustl (buffer)) /= "VAMP2 integrator:") then
          call msg_fatal ("VAMP2: reading grid file: corrupted data")
       end if
       n_pass = 0
       n_it = 0
       do
          read (u, "(A)") buffer
          select case (trim (adjustl (buffer)))
          case ("")
             exit
          case ("Integration pass:")
             call mci_file%list_pass%add ()
             call mci_file%list_pass%current%read (u, n_pass, n_it)
             n_pass = n_pass + 1
             n_it = n_it + mci_file%list_pass%current%n_it
          end select
       end do
       call mci%update_from_ref (mci_file, success)
       call mci_file%final ()
    end if
  end subroutine mci_vamp2_update

  subroutine mci_vamp2_write_grids (mci)
    class(mci_vamp2_t), intent(in) :: mci
    integer :: u
    if (.not. mci%integrator_filename_set) then
       call msg_bug ("VAMP2: write grids: filename undefined")
    end if
    if (.not. mci%integrator_defined) then
       call msg_bug ("VAMP2: write grids: grids undefined")
    end if
    u = free_unit ()
    open (u, file = char (mci%integrator_filename), &
         action = "write", status = "replace")
    write (u, "(1X,A,A,A)") "MD5sum = '", mci%md5sum, "'"
    write (u, *)
    call mci%write (u)
    write (u, *)
    write (u, "(1X,A)") "VAMP2 grids:"
    call mci%integrator%write_grids (u)
    close (u)
  end subroutine mci_vamp2_write_grids

  subroutine mci_vamp2_read_header (mci, success)
    class(mci_vamp2_t), intent(inout) :: mci
    logical, intent(out) :: success
    logical :: exist
    integer :: u
    success = .false.
    if (.not. mci%integrator_filename_set) then
       call msg_bug ("VAMP2: read grids: filename undefined")
    end if
    inquire (file = char (mci%integrator_filename), exist = exist)
    if (exist) then
       u = free_unit ()
       open (u, file = char (mci%integrator_filename), &
            action = "read", status = "old")
       call mci%update (u, success)
       close (u)
       if (.not. success) then
          write (msg_buffer, "(A,A,A)") &
               "VAMP2: header: parameter mismatch, discarding grid file '", &
               char (mci%integrator_filename), "'"
          call msg_message ()
       end if
    end if
  end subroutine mci_vamp2_read_header

  subroutine mci_vamp2_read_data (mci)
    class(mci_vamp2_t), intent(inout) :: mci
    integer :: u
    character(80) :: buffer
    if (mci%integrator_defined) then
       call msg_bug ("VAMP2: read grids: grids already defined")
    end if
    u = free_unit ()
    open (u, file = char (mci%integrator_filename), &
         action = "read", status = "old")
    do
       read (u, "(A)")  buffer
       if (trim (adjustl (buffer)) == "VAMP2 grids:")  exit
    end do
    call mci%integrator%read_grids (u)
    close (u)
    mci%integrator_defined = .true.
  end subroutine mci_vamp2_read_data

  subroutine mci_vamp2_read_grids (mci, success)
    class(mci_vamp2_t), intent(inout) :: mci
    logical, intent(out) :: success
    logical :: exist
    integer :: u
    character(80) :: buffer
    success = .false.
    if (.not. mci%integrator_filename_set) then
       call msg_bug ("VAMP2: read grids: filename undefined")
    end if
    if (mci%integrator_defined) then
       call msg_bug ("VAMP2: read grids: grids already defined")
    end if
    inquire (file = char (mci%integrator_filename), exist = exist)
    if (exist) then
       u = free_unit ()
       open (u, file = char (mci%integrator_filename), &
            action = "read", status = "old")
       call mci%update (u, success)
       if (success) then
          read (u, "(A)")  buffer
          if (trim (adjustl (buffer)) /= "VAMP2 grids:") then
             call msg_fatal ("VAMP2: reading grid file: &
                  &corrupted grid data")
          end if
          call mci%integrator%read_grids (u)
       else
          write (msg_buffer, "(A,A,A)") &
               "VAMP2: read grids: parameter mismatch, discarding grid file '", &
               char (mci%integrator_filename), "'"
          call msg_message ()
       end if
       close (u)
       mci%integrator_defined = success
    end if
  end subroutine mci_vamp2_read_grids
  subroutine mci_vamp2_init_integrator (mci)
    class(mci_vamp2_t), intent(inout) :: mci
    type (pass_t), pointer :: current
    integer :: ch, vegas_mode
    current => mci%list_pass%current
    vegas_mode = merge (VEGAS_MODE_IMPORTANCE, VEGAS_MODE_IMPORTANCE_ONLY,&
         & mci%config%stratified)
    mci%n_adapt_grids = 0
    mci%n_adapt_weights = 0
    if (mci%integrator_defined) then
       call msg_bug ("[MCI VAMP2]: init integrator: &
            & integrator is already initialised.")
    end if
    mci%integrator = vamp2_t (mci%n_channel, mci%n_dim, &
         & n_bins_max = mci%config%n_bins_max, &
         & iterations = 1, &
         & mode = vegas_mode)
    if (mci%has_chains ()) call mci%integrator%set_chain (mci%n_chain, mci%chain)
    call mci%integrator%set_config (mci%config)
    mci%integrator_defined = .true.
  end subroutine mci_vamp2_init_integrator

  subroutine mci_vamp2_reset_result (mci)
    class(mci_vamp2_t), intent(inout) :: mci
    if (.not. mci%integrator_defined) then
       call msg_bug ("[MCI VAMP2] reset results: integrator undefined")
    end if
    call mci%integrator%reset_result ()
  end subroutine mci_vamp2_reset_result

  subroutine mci_vamp2_set_calls (mci, n_calls)
    class(mci_vamp2_t), intent(inout) :: mci
    integer :: n_calls
    if (.not. mci%integrator_defined) then
       call msg_bug ("[MCI VAMP2] set calls: grids undefined")
    end if
    call mci%integrator%set_calls (n_calls)
  end subroutine mci_vamp2_set_calls

  subroutine mci_vamp2_init_integration (mci, n_it, n_calls, instance)
    class(mci_vamp2_t), intent(inout) :: mci
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    class(mci_instance_t), intent(inout) :: instance
    logical :: from_file, success
    if (.not. associated (mci%list_pass%current)) then
       call msg_bug ("MCI integrate: current_pass object not allocated")
    end if
    associate (current_pass => mci%list_pass%current)
      current_pass%integral_defined = .false.
      mci%config%n_calls_min = mci%config%n_calls_min_per_channel * mci%config%n_channel
      call current_pass%configure (n_it, n_calls, mci%config%n_calls_min)
      mci%adapt_grids = current_pass%adapt_grids
      mci%adapt_weights = current_pass%adapt_weights
      mci%pass_complete = .false.
      mci%it_complete = .false.
      from_file = .false.
      if (.not. mci%integrator_defined .or. mci%integrator_from_file) then
         if (mci%integrator_filename_set .and. .not. mci%rebuild) then
            call mci%read_header (success)
            from_file = success
            if (.not. mci%integrator_defined .and. success) &
                 & call mci%read_data ()
         end if
      end if
      if (from_file) then
         if (.not. mci%check_grid_file) &
              & call msg_warning ("Reading grid file: MD5 sum check disabled")
         call msg_message ("VAMP2: " &
              // "using grids and results from file ’" &
              // char (mci%integrator_filename) // "’")
      else if (.not. mci%integrator_defined) then
         call mci%init_integrator ()
      end if
      mci%integrator_from_file = from_file
      if (.not. mci%integrator_from_file) then
         call mci%integrator%set_calls (current_pass%n_calls)
      end if
    end associate
  end subroutine mci_vamp2_init_integration

  subroutine mci_vamp2_integrate (mci, instance, sampler, &
       n_it, n_calls, results, pacify)
    class(mci_vamp2_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    class(mci_results_t), intent(inout), optional :: results
    logical, intent(in), optional :: pacify
    integer :: it
    logical :: from_file, success
    real(default) :: integral, error, efficiency
    integer :: calls, calls_valid
  
    call mci%init_integration (n_it, n_calls, instance)
    from_file = mci%integrator_from_file
    select type (instance)
    type is (mci_vamp2_instance_t)
       call instance%set_workspace (sampler)
    end select
    associate (current_pass => mci%list_pass%current)
      do it = 1, current_pass%n_it
         if (signal_is_pending ()) return
         mci%integrator_from_file = from_file .and. &
              it <= current_pass%get_integration_index ()
         if (.not. mci%integrator_from_file) then
            mci%it_complete = .false.
            select type (instance)
            type is (mci_vamp2_instance_t)
               call mci%integrator%integrate (instance%func, mci%rng, &
                    & iterations = 1, &
                    & opt_reset_result = .true., &
                    & opt_refine_grid = mci%adapt_grids, &
                    & opt_adapt_weight = mci%adapt_weights, &
                    & opt_verbose = mci%verbose)
            end select
            if (signal_is_pending ()) return
            mci%it_complete = .true.
            integral = mci%integrator%get_integral ()
            calls = mci%integrator%get_n_calls ()
            select type (instance)
            type is (mci_vamp2_instance_t)
               calls_valid = instance%func%get_n_calls ()
               call instance%func%reset_n_calls ()
            end select
            error = sqrt (mci%integrator%get_variance ())
            efficiency = mci%integrator%get_efficiency ()
          
            if (integral /= 0) then
               current_pass%integral(it) = integral
               current_pass%calls(it) = calls
               current_pass%calls_valid(it) = calls_valid
               current_pass%error(it) = error
               current_pass%efficiency(it) = efficiency
            end if
            current_pass%integral_defined = .true.
         end if
         if (present (results)) then
            if (mci%has_chains ()) then
               call mci%collect_chain_weights (instance%w)
               call results%record (1, &
                    n_calls = current_pass%calls(it), &
                    n_calls_valid = current_pass%calls_valid(it), &
                    integral = current_pass%integral(it), &
                    error = current_pass%error(it), &
                    efficiency = current_pass%efficiency(it), &
                    efficiency_pos = current_pass%efficiency(it), &
                    efficiency_neg = 0._default, &
                    chain_weights = mci%chain_weights, &
                    suppress = pacify)
            else
               call results%record (1, &
                    n_calls = current_pass%calls(it), &
                    n_calls_valid = current_pass%calls_valid(it), &
                    integral = current_pass%integral(it), &
                    error = current_pass%error(it), &
                    efficiency = current_pass%efficiency(it), &
                    efficiency_pos = current_pass%efficiency(it), &
                    efficiency_neg = 0._default, &
                    suppress = pacify)
            end if
         end if
         if (.not. mci%integrator_from_file &
              .and. mci%integrator_filename_set) then
             call mci%write_grids ()
         end if
         if (.not. current_pass%is_final_pass) then
            call check_goals (it, success)
            if (success) exit
         end if
      end do
      if (signal_is_pending ()) return
      mci%pass_complete = .true.
      mci%integral = current_pass%get_integral()
      mci%error = current_pass%get_error()
      mci%efficiency = current_pass%get_efficiency()
      mci%integral_known = .true.
      mci%error_known = .true.
      mci%efficiency_known = .true.
      call mci%compute_md5sum (pacify)
    end associate
  contains
      subroutine check_goals (it, success)
        integer, intent(in) :: it
        logical, intent(out) :: success
        success = .false.
        associate (current_pass => mci%list_pass%current)
          if (error_reached (it)) then
             current_pass%n_it = it
             call msg_message ("[MCI VAMP2] error goal reached; &
                  &skipping iterations")
             success = .true.
             return
          end if
          if (rel_error_reached (it)) then
             current_pass%n_it = it
             call msg_message ("[MCI VAMP2] relative error goal reached; &
                  &skipping iterations")
             success = .true.
             return
          end if
          if (accuracy_reached (it)) then
             current_pass%n_it = it
             call msg_message ("[MCI VAMP2] accuracy goal reached; &
                  &skipping iterations")
             success = .true.
             return
          end if
        end associate
      end subroutine check_goals

      function error_reached (it) result (flag)
        integer, intent(in) :: it
        logical :: flag
        real(default) :: error_goal, error
        error_goal = mci%config%error_goal
        flag = .false.
        associate (current_pass => mci%list_pass%current)
          if (error_goal > 0 .and. current_pass%integral_defined) then
             error = abs (current_pass%error(it))
             flag = error < error_goal
          end if
        end associate
      end function error_reached

      function rel_error_reached (it) result (flag)
        integer, intent(in) :: it
        logical :: flag
        real(default) :: rel_error_goal, rel_error
        rel_error_goal = mci%config%rel_error_goal
        flag = .false.
        associate (current_pass => mci%list_pass%current)
          if (rel_error_goal > 0 .and. current_pass%integral_defined) then
             rel_error = abs (current_pass%error(it) / current_pass%integral(it))
             flag = rel_error < rel_error_goal
          end if
        end associate
      end function rel_error_reached

      function accuracy_reached (it) result (flag)
        integer, intent(in) :: it
        logical :: flag
        real(default) :: accuracy_goal, accuracy
        accuracy_goal = mci%config%accuracy_goal
        flag = .false.
        associate (current_pass => mci%list_pass%current)
          if (accuracy_goal > 0 .and. current_pass%integral_defined) then
             if (current_pass%integral(it) /= 0) then
                accuracy = abs (current_pass%error(it) / current_pass%integral(it)) &
                     * sqrt (real (current_pass%calls(it), default))
                flag = accuracy < accuracy_goal
             else
                flag = .true.
             end if
          end if
        end associate
      end function accuracy_reached

  end subroutine mci_vamp2_integrate

  subroutine mci_vamp2_prepare_simulation (mci)
    class(mci_vamp2_t), intent(inout) :: mci
    logical :: success
    if (.not. mci%integrator_filename_set) then
       call msg_bug ("VAMP2: preapre simulation: integrator filename not set.")
    end if
    call mci%read_header (success)
    call mci%compute_md5sum ()
    if (.not. success) then
       call msg_fatal ("Simulate: " &
            // "reading integration grids from file ’" &
            // char (mci%integrator_filename) // "’ failed")
    end if
    if (.not. mci%integrator_defined) then
       call mci%read_data ()
    end if
  end subroutine mci_vamp2_prepare_simulation

  subroutine mci_vamp2_generate_weighted_event (mci, instance, sampler)
    class(mci_vamp2_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    if (.not. mci%integrator_defined) then
       call msg_bug ("VAMP2: generate weighted event: undefined integrator")
    end if
    select type (instance)
    type is (mci_vamp2_instance_t)
       instance%event_generated = .false.
       call instance%set_workspace (sampler)
       call mci%integrator%generate_weighted (&
            & instance%func, mci%rng, instance%event_x)
       instance%event_weight = mci%integrator%get_evt_weight ()
       instance%event_excess = 0
       instance%n_events = instance%n_events + 1
       instance%event_generated = .true.
    end select
  end subroutine mci_vamp2_generate_weighted_event

  subroutine mci_vamp2_generate_unweighted_event (mci, instance, sampler)
    class(mci_vamp2_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    if (.not. mci%integrator_defined) then
       call msg_bug ("VAMP2: generate unweighted event: undefined integrator")
    end if
    select type (instance)
    type is (mci_vamp2_instance_t)
       instance%event_generated = .false.
       call instance%set_workspace (sampler)
       generate: do
          call mci%integrator%generate_unweighted (&
               & instance%func, mci%rng, instance%event_x, &
               & opt_event_rescale = instance%event_rescale_f_max)
          instance%event_excess = mci%integrator%get_evt_weight_excess ()
          if (signal_is_pending ()) return
          if (sampler%is_valid ()) exit generate
       end do generate
       if (mci%integrator%get_evt_weight () < 0.) then
          if (.not. mci%negative_weights) then
             call msg_fatal ("MCI VAMP2 cannot sample negative weights!")
          end if
          instance%event_weight = -1._default
       else
          instance%event_weight = 1._default
       end if
       instance%event_weight = 1
       instance%n_events = instance%n_events + 1
       instance%event_generated = .true.
    end select
  end subroutine mci_vamp2_generate_unweighted_event

  subroutine mci_vamp2_rebuild_event (mci, instance, sampler, state)
    class(mci_vamp2_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
    call msg_bug ("MCI VAMP2 rebuild event not implemented yet.")
  end subroutine mci_vamp2_rebuild_event

  subroutine mci_vamp2_instance_write (object, unit, pacify)
    class(mci_vamp2_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    integer :: u, ch, j
    character(len=7) :: fmt
    call pac_fmt (fmt, FMT_17, FMT_14, pacify)
    u = given_output_unit (unit)
    write (u, "(1X,A)") "MCI VAMP2 instance:"
    write (u, "(1X,A,I0)") &
         & "Selected channel        = ", object%selected_channel
    write (u, "(1X,A25,1X," // fmt // ")") &
         & "Integrand               = ", object%integrand
    write (u, "(1X,A25,1X," // fmt // ")") &
         & "MCI weight              = ", object%mci_weight
    write (u, "(1X,A,L1)") &
         & "Valid                   = ", object%valid
    write (u, "(1X,A)") "MCI a-priori weight:"
    do ch = 1, size (object%w)
       write (u, "(3X,I25,1X," // fmt // ")") ch, object%w(ch)
    end do
    write (u, "(1X,A)") "MCI jacobian:"
    do ch = 1, size (object%w)
       write (u, "(3X,I25,1X," // fmt // ")") ch, object%f(ch)
    end do
    write (u, "(1X,A)") "MCI mapped x:"
    do ch = 1, size (object%w)
       do j = 1, size (object%x, 1)
          write (u, "(3X,2(1X,I8),1X," // fmt // ")") j, ch, object%x(j, ch)
       end do
    end do
    write (u, "(1X,A)") "MCI channel weight:"
    do ch = 1, size (object%w)
       write (u, "(3X,I25,1X," // fmt // ")") ch, object%gi(ch)
    end do
    write (u, "(1X,A,I0)") &
         & "Number of event         = ", object%n_events
    write (u, "(1X,A,L1)") &
         & "Event generated         = ", object%event_generated
    write (u, "(1X,A25,1X," // fmt // ")") &
         & "Event weight            = ", object%event_weight
    write (u, "(1X,A25,1X," // fmt // ")") &
         & "Event excess            = ", object%event_excess
    write (u, "(1X,A25,1X," // fmt // ")") &
         & "Event rescale f max     = ", object%event_rescale_f_max
    write (u, "(1X,A,L1)") &
         & "Negative (event) weight = ", object%negative_weights
    write (u, "(1X,A)") "MCI event"
    do j = 1, size (object%event_x)
       write (u, "(3X,I25,1X," // fmt // ")") j, object%event_x(j)
    end do
  end subroutine mci_vamp2_instance_write

  subroutine mci_vamp2_instance_final (object)
    class(mci_vamp2_instance_t), intent(inout) :: object
    !
  end subroutine mci_vamp2_instance_final

  subroutine mci_vamp2_instance_init (mci_instance, mci)
    class(mci_vamp2_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
    call mci_instance%base_init (mci)
    allocate (mci_instance%gi(mci%n_channel), source=0._default)
    allocate (mci_instance%event_x(mci%n_dim), source=0._default)
    allocate (mci_vamp2_func_t :: mci_instance%func)
    call mci_instance%func%init (n_dim = mci%n_dim, n_channel = mci%n_channel)
  end subroutine mci_vamp2_instance_init

  subroutine mci_vamp2_instance_set_workspace (instance, sampler)
    class(mci_vamp2_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    call instance%func%set_workspace (instance, sampler)
  end subroutine mci_vamp2_instance_set_workspace

  subroutine mci_vamp2_instance_compute_weight (mci, c)
    class(mci_vamp2_instance_t), intent(inout) :: mci
    integer, intent(in) :: c
    mci%gi = mci%func%get_probabilities ()
    mci%mci_weight = mci%func%get_weight ()
  end subroutine mci_vamp2_instance_compute_weight

  subroutine mci_vamp2_instance_record_integrand (mci, integrand)
    class(mci_vamp2_instance_t), intent(inout) :: mci
    real(default), intent(in) :: integrand
    mci%integrand = integrand
    call mci%func%set_integrand (integrand)
  end subroutine mci_vamp2_instance_record_integrand

  subroutine mci_vamp2_instance_init_simulation (instance, safety_factor)
    class(mci_vamp2_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: safety_factor
    if (present (safety_factor)) instance%event_rescale_f_max = safety_factor
    instance%n_events = 0
    instance%event_generated = .false.
    if (instance%event_rescale_f_max /= 1) then
       write (msg_buffer, "(A,ES10.3,A)") "Simulate: &
            &applying safety factor ", instance%event_rescale_f_max, &
            & " to event rejection."
       call msg_message ()
    end if
  end subroutine mci_vamp2_instance_init_simulation

  subroutine mci_vamp2_instance_final_simulation (instance)
    class(mci_vamp2_instance_t), intent(inout) :: instance
    !
  end subroutine mci_vamp2_instance_final_simulation

  function mci_vamp2_instance_get_event_weight (mci) result (weight)
    class(mci_vamp2_instance_t), intent(in) :: mci
    real(default) :: weight
    if (.not. mci%event_generated) then
       call msg_bug ("MCI VAMP2: get event weight: no event generated")
    end if
    weight = mci%event_weight
  end function mci_vamp2_instance_get_event_weight

  function mci_vamp2_instance_get_event_excess (mci) result (excess)
    class(mci_vamp2_instance_t), intent(in) :: mci
    real(default) :: excess
    if (.not. mci%event_generated) then
       call msg_bug ("MCI VAMP2: get event excess: no event generated")
    end if
    excess = mci%event_excess
  end function mci_vamp2_instance_get_event_excess


end module mci_vamp2
