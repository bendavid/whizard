! WHIZARD 2.2.4 Feb 06 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module mci_base

  use kinds
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_14, FMT_17
  use unit_tests
  use diagnostics
  use cputime
  use phs_base
  use rng_base

  implicit none
  private

  public :: mci_t
  public :: mci_instance_t
  public :: mci_state_t
  public :: mci_sampler_t
  public :: mci_results_t
  public :: mci_base_test
  public :: mci_test_t
  public :: mci_test_instance_t

  type, abstract :: mci_t
     integer :: n_dim = 0
     integer :: n_channel = 0
     integer :: n_chain = 0
     integer, dimension(:), allocatable :: chain
     real(default), dimension(:), allocatable :: chain_weights
     character(32) :: md5sum = ""
     logical :: integral_known = .false.
     logical :: error_known = .false.
     logical :: efficiency_known = .false.
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: efficiency = 0
     logical :: use_timer = .false.
     type(timer_t) :: timer
     class(rng_t), allocatable :: rng
   contains
     procedure :: base_final => mci_final
     procedure (mci_final), deferred :: final
     procedure :: base_write => mci_write
     procedure (mci_write), deferred :: write
     procedure (mci_startup_message), deferred :: startup_message
     procedure :: base_startup_message => mci_startup_message
     procedure :: record_index => mci_record_index
     procedure :: set_dimensions => mci_set_dimensions
     procedure :: base_set_dimensions => mci_set_dimensions
     procedure (mci_declare_flat_dimensions), deferred :: declare_flat_dimensions
     procedure (mci_declare_equivalences), deferred :: declare_equivalences
     procedure :: declare_chains => mci_declare_chains
     procedure :: collect_chain_weights => mci_collect_chain_weights
     procedure :: has_chains => mci_has_chains
     procedure :: write_chain_weights => mci_write_chain_weights
     procedure :: set_md5sum => mci_set_md5sum
     procedure :: add_pass => mci_add_pass
     procedure (mci_allocate_instance), deferred :: allocate_instance
     procedure :: import_rng => mci_import_rng
     procedure :: set_timer => mci_set_timer
     procedure :: start_timer => mci_start_timer
     procedure :: stop_timer => mci_stop_timer
     procedure :: sampler_test => mci_sampler_test
     procedure (mci_integrate), deferred :: integrate
     procedure (mci_prepare_simulation), deferred :: prepare_simulation
     procedure (mci_generate), deferred :: generate_weighted_event
     procedure (mci_generate), deferred :: generate_unweighted_event
     procedure (mci_rebuild), deferred :: rebuild_event
     procedure :: get_integral => mci_get_integral
     procedure :: get_error => mci_get_error
     procedure :: get_efficiency => mci_get_efficiency
     procedure :: get_time => mci_get_time
     procedure :: get_md5sum => mci_get_md5sum
  end type mci_t
  
  type, abstract :: mci_instance_t
     logical :: valid = .false.
     real(default), dimension(:), allocatable :: w
     real(default), dimension(:), allocatable :: f
     real(default), dimension(:,:), allocatable :: x
     integer :: selected_channel = 0
     real(default) :: mci_weight = 0
     real(default) :: integrand  = 0
   contains
     procedure (mci_instance_write), deferred :: write
     procedure (mci_instance_final), deferred :: final
     procedure (mci_instance_base_init), deferred :: init
     procedure :: base_init => mci_instance_base_init
     procedure (mci_instance_compute_weight), deferred :: compute_weight
     procedure (mci_instance_record_integrand), deferred :: record_integrand
     procedure :: evaluate => mci_instance_evaluate
     procedure (mci_instance_init_simulation), deferred :: init_simulation
     procedure (mci_instance_final_simulation), deferred :: final_simulation
     procedure :: fetch => mci_instance_fetch
     procedure :: get_value => mci_instance_get_value
     procedure :: get_event_weight => mci_instance_get_value
     procedure (mci_instance_get_event_excess), deferred :: get_event_excess
     procedure :: store => mci_instance_store
     procedure :: recall => mci_instance_recall
  end type mci_instance_t
  
  type :: mci_state_t
     integer :: selected_channel = 0
     real(default), dimension(:), allocatable :: x_in
     real(default) :: val
   contains
     procedure :: write => mci_state_write
  end type mci_state_t
  
  type, abstract :: mci_sampler_t
   contains
     procedure (mci_sampler_write), deferred :: write
     procedure (mci_sampler_evaluate), deferred :: evaluate
     procedure (mci_sampler_is_valid), deferred :: is_valid
     procedure (mci_sampler_rebuild), deferred :: rebuild
     procedure (mci_sampler_fetch), deferred :: fetch
  end type mci_sampler_t

  type, abstract :: mci_results_t
   contains
     procedure (mci_results_write), deferred :: write
     procedure (mci_results_record), deferred :: record
  end type mci_results_t
  

  abstract interface
     subroutine mci_declare_flat_dimensions (mci, dim_flat)
       import
       class(mci_t), intent(inout) :: mci
       integer, dimension(:), intent(in) :: dim_flat
     end subroutine mci_declare_flat_dimensions
  end interface
  
  abstract interface
     subroutine mci_declare_equivalences (mci, channel, dim_offset)
       import
       class(mci_t), intent(inout) :: mci
       type(phs_channel_t), dimension(:), intent(in) :: channel
       integer, intent(in) :: dim_offset
     end subroutine mci_declare_equivalences
  end interface
  
  abstract interface
     subroutine mci_allocate_instance (mci, mci_instance)
       import
       class(mci_t), intent(in) :: mci
       class(mci_instance_t), intent(out), pointer :: mci_instance
     end subroutine mci_allocate_instance
  end interface
       
  abstract interface
     subroutine mci_integrate (mci, instance, sampler, &
          n_it, n_calls, results, pacify)
       import
       class(mci_t), intent(inout) :: mci
       class(mci_instance_t), intent(inout) :: instance
       class(mci_sampler_t), intent(inout) :: sampler
       integer, intent(in) :: n_it
       integer, intent(in) :: n_calls
       logical, intent(in), optional :: pacify
       class(mci_results_t), intent(inout), optional :: results
     end subroutine mci_integrate
  end interface
  
  abstract interface
     subroutine mci_prepare_simulation (mci)
       import
       class(mci_t), intent(inout) :: mci
     end subroutine mci_prepare_simulation
  end interface
  
  abstract interface
     subroutine mci_generate (mci, instance, sampler)
       import
       class(mci_t), intent(inout) :: mci
       class(mci_instance_t), intent(inout), target :: instance
       class(mci_sampler_t), intent(inout), target :: sampler
     end subroutine mci_generate
  end interface
  
  abstract interface
     subroutine mci_rebuild (mci, instance, sampler, state)
       import
       class(mci_t), intent(inout) :: mci
       class(mci_instance_t), intent(inout) :: instance
       class(mci_sampler_t), intent(inout) :: sampler
       class(mci_state_t), intent(in) :: state
     end subroutine mci_rebuild
  end interface
  
  abstract interface
     subroutine mci_instance_write (object, unit, pacify)
       import
       class(mci_instance_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: pacify
     end subroutine mci_instance_write
  end interface
    
  abstract interface
     subroutine mci_instance_final (object)
       import
       class(mci_instance_t), intent(inout) :: object
     end subroutine mci_instance_final
  end interface
  
  abstract interface
     subroutine mci_instance_compute_weight (mci, c)
       import
       class(mci_instance_t), intent(inout) :: mci
       integer, intent(in) :: c
     end subroutine mci_instance_compute_weight
  end interface
    
  abstract interface
     subroutine mci_instance_record_integrand (mci, integrand)
       import
       class(mci_instance_t), intent(inout) :: mci
       real(default), intent(in) :: integrand
     end subroutine mci_instance_record_integrand
  end interface
  
  abstract interface
     subroutine mci_instance_init_simulation (instance, safety_factor)
       import
       class(mci_instance_t), intent(inout) :: instance
       real(default), intent(in), optional :: safety_factor
     end subroutine mci_instance_init_simulation
  end interface
  
  abstract interface
     subroutine mci_instance_final_simulation (instance)
       import
       class(mci_instance_t), intent(inout) :: instance
     end subroutine mci_instance_final_simulation
  end interface
  
  abstract interface
     function mci_instance_get_event_excess (mci) result (excess)
       import
       class(mci_instance_t), intent(in) :: mci
       real(default) :: excess
     end function mci_instance_get_event_excess
  end interface
  
  abstract interface
     subroutine mci_sampler_write (object, unit, testflag)
       import
       class(mci_sampler_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: testflag
     end subroutine mci_sampler_write
  end interface
  
  abstract interface
     subroutine mci_sampler_evaluate (sampler, c, x_in, val, x, f)
       import
       class(mci_sampler_t), intent(inout) :: sampler
       integer, intent(in) :: c
       real(default), dimension(:), intent(in) :: x_in
       real(default), intent(out) :: val
       real(default), dimension(:,:), intent(out) :: x
       real(default), dimension(:), intent(out) :: f
     end subroutine mci_sampler_evaluate
  end interface

  abstract interface
     function mci_sampler_is_valid (sampler) result (valid)
       import
       class(mci_sampler_t), intent(in) :: sampler
       logical :: valid
     end function mci_sampler_is_valid
  end interface

  abstract interface
     subroutine mci_sampler_rebuild (sampler, c, x_in, val, x, f)
       import
       class(mci_sampler_t), intent(inout) :: sampler
       integer, intent(in) :: c
       real(default), dimension(:), intent(in) :: x_in
       real(default), intent(in) :: val
       real(default), dimension(:,:), intent(out) :: x
       real(default), dimension(:), intent(out) :: f
     end subroutine mci_sampler_rebuild
  end interface
  
  abstract interface
     subroutine mci_sampler_fetch (sampler, val, x, f)
       import
       class(mci_sampler_t), intent(in) :: sampler
       real(default), intent(out) :: val
       real(default), dimension(:,:), intent(out) :: x
       real(default), dimension(:), intent(out) :: f
     end subroutine mci_sampler_fetch
  end interface
  
  abstract interface
     subroutine mci_results_write (object, unit, verbose, suppress)
       import
       class(mci_results_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: verbose, suppress
     end subroutine mci_results_write
  end interface
  
  abstract interface
     subroutine mci_results_record (object, n_it, &
          n_calls, integral, error, efficiency, chain_weights, suppress)
       import
       class(mci_results_t), intent(inout) :: object
       integer, intent(in) :: n_it
       integer, intent(in) :: n_calls
       real(default), intent(in) :: integral
       real(default), intent(in) :: error
       real(default), intent(in) :: efficiency
       real(default), dimension(:), intent(in), optional :: chain_weights
       logical, intent(in), optional :: suppress
     end subroutine mci_results_record
  end interface


  type, extends (mci_t) :: mci_test_t
     integer :: divisions = 0
     integer :: tries = 0
     real(default) :: max_factor = 1
   contains
     procedure :: final => mci_test_final
     procedure :: write => mci_test_write
     procedure :: startup_message => mci_test_startup_message
     procedure :: declare_flat_dimensions => mci_test_ignore_flat_dimensions
     procedure :: declare_equivalences => mci_test_ignore_equivalences
     procedure :: set_divisions => mci_test_set_divisions
     procedure :: set_max_factor => mci_test_set_max_factor
     procedure :: allocate_instance => mci_test_allocate_instance
     procedure :: integrate => mci_test_integrate
     procedure :: prepare_simulation => mci_test_ignore_prepare_simulation
     procedure :: generate_weighted_event => mci_test_generate_weighted_event
     procedure :: generate_unweighted_event => &
          mci_test_generate_unweighted_event
     procedure :: rebuild_event => mci_test_rebuild_event
  end type mci_test_t
  
  type, extends (mci_instance_t) :: mci_test_instance_t
     type(mci_test_t), pointer :: mci => null ()
     real(default) :: g = 0
     real(default), dimension(:), allocatable :: gi
     real(default) :: value = 0
     real(default) :: rel_value = 0
     real(default), dimension(:), allocatable :: max
   contains
     procedure :: write => mci_test_instance_write
     procedure :: final => mci_test_instance_final
     procedure :: init => mci_test_instance_init
     procedure :: compute_weight => mci_test_instance_compute_weight
     procedure :: record_integrand => mci_test_instance_record_integrand
     procedure :: init_simulation => mci_test_instance_init_simulation
     procedure :: final_simulation => mci_test_instance_final_simulation
     procedure :: get_event_excess => mci_test_instance_get_event_excess
  end type mci_test_instance_t

  type, extends (mci_sampler_t) :: test_sampler_t
     real(default) :: integrand = 0
     integer :: selected_channel = 0
     real(default), dimension(:,:), allocatable :: x
     real(default), dimension(:), allocatable :: f
   contains
     procedure :: init => test_sampler_init
     procedure :: write => test_sampler_write
     procedure :: compute => test_sampler_compute
     procedure :: is_valid => test_sampler_is_valid
     procedure :: evaluate => test_sampler_evaluate
     procedure :: rebuild => test_sampler_rebuild
     procedure :: fetch => test_sampler_fetch
  end type test_sampler_t
  
  type, extends (mci_results_t) :: mci_test_results_t
     integer :: n_it = 0
     integer :: n_calls = 0
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: efficiency = 0
   contains
     procedure :: write => mci_test_results_write
     procedure :: record => mci_test_results_record
  end type mci_test_results_t
  

contains
  
  subroutine mci_final (object)
    class(mci_t), intent(inout) :: object
    if (allocated (object%rng))  call object%rng%final ()
  end subroutine mci_final

  subroutine mci_write (object, unit, pacify, md5sum_version)
    class(mci_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    logical :: md5sum_ver
    integer :: u, i, j
    character(len=7) :: fmt 
    call pac_fmt (fmt, FMT_17, FMT_14, pacify)
    u = given_output_unit (unit)
    md5sum_ver = .false.
    if (present (md5sum_version))  md5sum_ver = md5sum_version
    if (object%use_timer .and. .not. md5sum_ver) then
       write (u, "(2x)", advance="no")
       call object%timer%write (u)
    end if
    if (object%integral_known) then
       write (u, "(3x,A," // fmt // ")") &
            "Integral             = ", object%integral
    end if
    if (object%error_known) then
       write (u, "(3x,A," // fmt // ")") &
            "Error                = ", object%error
    end if
    if (object%efficiency_known) then
       write (u, "(3x,A," // fmt // ")")  &
            "Efficiency           = ", object%efficiency
    end if
    write (u, "(3x,A,I0)")  "Number of channels   = ", object%n_channel
    write (u, "(3x,A,I0)")  "Number of dimensions = ", object%n_dim
    if (object%n_chain > 0) then
       write (u, "(3x,A,I0)")  "Number of chains     = ", object%n_chain
       write (u, "(3x,A)")  "Chains:"
       do i = 1, object%n_chain
          write (u, "(5x,I0,':')", advance = "no")  i
          do j = 1, object%n_channel
             if (object%chain(j) == i) &
                  write (u, "(1x,I0)", advance = "no")  j
          end do
          write (u, "(A)")
       end do
    end if
  end subroutine mci_write

  subroutine mci_startup_message (mci, unit, n_calls)
    class(mci_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    if (mci%n_chain > 0) then
       write (msg_buffer, "(A,3(1x,I0,1x,A))") &
            "Integrator:", mci%n_chain, "chains,", &
            mci%n_channel, "channels,", &
            mci%n_dim, "dimensions"
    else
       write (msg_buffer, "(A,3(1x,I0,1x,A))") &
            "Integrator:", &
            mci%n_channel, "channels,", &
            mci%n_dim, "dimensions"
    end if
    call msg_message (unit = unit)
  end subroutine mci_startup_message

  subroutine mci_record_index (mci, i_mci)
    class(mci_t), intent(inout) :: mci
    integer, intent(in) :: i_mci
  end subroutine mci_record_index

  subroutine mci_set_dimensions (mci, n_dim, n_channel)
    class(mci_t), intent(inout) :: mci
    integer, intent(in) :: n_dim
    integer, intent(in) :: n_channel
    mci%n_dim = n_dim
    mci%n_channel = n_channel
  end subroutine mci_set_dimensions
  
  subroutine mci_declare_chains (mci, chain)
    class(mci_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: chain
    allocate (mci%chain (size (chain)))
    mci%n_chain = maxval (chain)
    allocate (mci%chain_weights (mci%n_chain), source = 0._default)
    mci%chain = chain
  end subroutine mci_declare_chains
  
  subroutine mci_collect_chain_weights (mci, weight)
    class(mci_t), intent(inout) :: mci
    real(default), dimension(:), intent(in) :: weight
    integer :: i, c
    if (allocated (mci%chain)) then
       mci%chain_weights = 0
       do i = 1, size (mci%chain)
          c = mci%chain(i)
          mci%chain_weights(c) = mci%chain_weights(c) + weight(i)
       end do
    end if
  end subroutine mci_collect_chain_weights
    
  function mci_has_chains (mci) result (flag)
    class(mci_t), intent(in) :: mci
    logical :: flag
    flag = allocated (mci%chain)
  end function mci_has_chains

  subroutine mci_write_chain_weights (mci, unit)
    class(mci_t), intent(in) :: mci
    integer, intent(in), optional :: unit
    integer :: u, i, n, n_digits
    character(4) :: ifmt
    u = given_output_unit (unit)
    if (allocated (mci%chain_weights)) then
       write (u, "(1x,A)")  "Weights of channel chains (groves):"
       n_digits = 0
       n = size (mci%chain_weights)
       do while (n > 0)
          n = n / 10
          n_digits = n_digits + 1
       end do
       write (ifmt, "(A1,I1)") "I", n_digits
       do i = 1, size (mci%chain_weights)
          write (u, "(3x," // ifmt // ",F13.10)")  i, mci%chain_weights(i)
       end do
    end if
  end subroutine mci_write_chain_weights
  
  subroutine mci_set_md5sum (mci, md5sum)
    class(mci_t), intent(inout) :: mci
    character(32), intent(in) :: md5sum
    mci%md5sum = md5sum
  end subroutine mci_set_md5sum
  
  subroutine mci_add_pass (mci, adapt_grids, adapt_weights, final)
    class(mci_t), intent(inout) :: mci
    logical, intent(in), optional :: adapt_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: final
  end subroutine mci_add_pass
    
  subroutine mci_import_rng (mci, rng)
    class(mci_t), intent(inout) :: mci
    class(rng_t), intent(inout), allocatable :: rng
    call move_alloc (rng, mci%rng)
  end subroutine mci_import_rng
  
  subroutine mci_set_timer (mci, active)
    class(mci_t), intent(inout) :: mci
    logical, intent(in) :: active
    mci%use_timer = active
  end subroutine mci_set_timer
    
  subroutine mci_start_timer (mci)
    class(mci_t), intent(inout) :: mci
    if (mci%use_timer)  call mci%timer%start ()
  end subroutine mci_start_timer
  
  subroutine mci_stop_timer (mci)
    class(mci_t), intent(inout) :: mci
    if (mci%use_timer)  call mci%timer%stop ()
  end subroutine mci_stop_timer
  
  subroutine mci_sampler_test (mci, sampler, n_calls)
    class(mci_t), intent(inout) :: mci
    class(mci_sampler_t), intent(inout), target :: sampler
    integer, intent(in) :: n_calls
    real(default), dimension(:), allocatable :: x_in, f
    real(default), dimension(:,:), allocatable :: x_out
    real(default) :: val
    integer :: i, c
    allocate (x_in (mci%n_dim))
    allocate (f (mci%n_channel))
    allocate (x_out (mci%n_dim, mci%n_channel))
    do i = 1, n_calls
       c = mod (i, mci%n_channel) + 1
       call mci%rng%generate_array (x_in)
       call sampler%evaluate (c, x_in, val, x_out, f)
    end do
  end subroutine mci_sampler_test
  
  function mci_get_integral (mci) result (integral)
    class(mci_t), intent(in) :: mci
    real(default) :: integral
    if (mci%integral_known) then
       integral = mci%integral
    else
       call msg_bug ("The integral is unknown. This is presumably a" // &
            "WHIZARD bug.")
    end if
  end function mci_get_integral
  
  function mci_get_error (mci) result (error)
    class(mci_t), intent(in) :: mci
    real(default) :: error
    if (mci%error_known) then
       error = mci%error
    else
       error = 0
    end if
  end function mci_get_error
  
  function mci_get_efficiency (mci) result (efficiency)
    class(mci_t), intent(in) :: mci
    real(default) :: efficiency
    if (mci%efficiency_known) then
       efficiency = mci%efficiency
    else
       efficiency = 0
    end if
  end function mci_get_efficiency
  
  function mci_get_time (mci) result (time)
    class(mci_t), intent(in) :: mci
    real(default) :: time
    if (mci%use_timer) then
       time = mci%timer
    else
       time = 0
    end if
  end function mci_get_time
  
  function mci_get_md5sum (mci) result (md5sum)
    class(mci_t), intent(in) :: mci
    character(32) :: md5sum
    md5sum = mci%md5sum
  end function mci_get_md5sum
  
  subroutine mci_instance_base_init (mci_instance, mci)
    class(mci_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
    allocate (mci_instance%w (mci%n_channel))
    allocate (mci_instance%f (mci%n_channel))
    allocate (mci_instance%x (mci%n_dim, mci%n_channel))
    if (size (mci_instance%w) /= 0) then
       mci_instance%w = 1._default / size (mci_instance%w)
    end if
    mci_instance%f = 0
    mci_instance%x = 0
  end subroutine mci_instance_base_init
    
  subroutine mci_instance_evaluate (mci, sampler, c, x)
    class(mci_instance_t), intent(inout) :: mci
    class(mci_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x
    real(default) :: val
    call sampler%evaluate (c, x, val, mci%x, mci%f)
    mci%valid = sampler%is_valid ()
    if (mci%valid) then
       call mci%compute_weight (c)
       call mci%record_integrand (val)
    end if
  end subroutine mci_instance_evaluate
    
  subroutine mci_instance_fetch (mci, sampler, c)
    class(mci_instance_t), intent(inout) :: mci
    class(mci_sampler_t), intent(in) :: sampler
    integer, intent(in) :: c
    real(default) :: val
    mci%valid = sampler%is_valid ()
    if (mci%valid) then
       call sampler%fetch (val, mci%x, mci%f)
       call mci%compute_weight (c)
       call mci%record_integrand (val)
    end if
  end subroutine mci_instance_fetch

  function mci_instance_get_value (mci) result (value)
    class(mci_instance_t), intent(in) :: mci
    real(default) :: value
    if (mci%valid) then
       value = mci%integrand * mci%mci_weight
    else
       value = 0
    end if
  end function mci_instance_get_value

  subroutine mci_state_write (object, unit)
    class(mci_state_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "MCI state:"
    write (u, "(3x,A,I0)")  "Channel   = ", object%selected_channel
    write (u, "(3x,A,999(1x,F12.10))")  "x (in)    =", object%x_in
    write (u, "(3x,A,ES19.12)")  "Integrand = ", object%val
  end subroutine mci_state_write
  
  subroutine mci_instance_store (mci, state)
    class(mci_instance_t), intent(in) :: mci
    class(mci_state_t), intent(out) :: state
    state%selected_channel = mci%selected_channel
    allocate (state%x_in (size (mci%x, 1)))
    state%x_in = mci%x(:,mci%selected_channel)
    state%val = mci%integrand
  end subroutine mci_instance_store
    
  subroutine mci_instance_recall (mci, sampler, state)
    class(mci_instance_t), intent(inout) :: mci
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
    if (size (state%x_in) == size (mci%x, 1) &
         .and. state%selected_channel <= size (mci%x, 2)) then
       call sampler%rebuild (state%selected_channel, &
            state%x_in, state%val, mci%x, mci%f)
       call mci%compute_weight (state%selected_channel)
       call mci%record_integrand (state%val)
    else
       call msg_fatal ("Recalling event: mismatch in channel or dimension")
    end if
  end subroutine mci_instance_recall
    

  subroutine mci_base_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (mci_base_1, "mci_base_1", &
         "integrator configuration", &
         u, results)
    call test (mci_base_2, "mci_base_2", &
         "integration", &
         u, results)
    call test (mci_base_3, "mci_base_3", &
         "integration (two channels)", &
         u, results)
    call test (mci_base_4, "mci_base_4", &
         "event generation (two channels)", &
         u, results)
    call test (mci_base_5, "mci_base_5", &
         "store and recall", &
         u, results)
    call test (mci_base_6, "mci_base_6", &
         "chained channels", &
         u, results)
    call test (mci_base_7, "mci_base_7", &
         "recording results", &
         u, results)
    call test (mci_base_8, "mci_base_8", &
         "timer", &
         u, results)
  end subroutine mci_base_test
  
  subroutine mci_test_final (object)
    class(mci_test_t), intent(inout) :: object
    call object%base_final ()
  end subroutine mci_test_final
  
  subroutine mci_test_write (object, unit, pacify, md5sum_version)
    class(mci_test_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Test integrator:"
    call object%base_write (u, pacify, md5sum_version)
    if (object%divisions /= 0) then
       write (u, "(3x,A,I0)")  "Number of divisions  = ", object%divisions
    end if
    if (allocated (object%rng))  call object%rng%write (u)
  end subroutine mci_test_write
  
  subroutine mci_test_startup_message (mci, unit, n_calls)
    class(mci_test_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    call mci%base_startup_message (unit = unit, n_calls = n_calls)
    write (msg_buffer, "(A,1x,I0,1x,A)") &
         "Integrator: Test:", mci%divisions, "divisions"
    call msg_message (unit = unit)
  end subroutine mci_test_startup_message
  
  subroutine mci_test_ignore_flat_dimensions (mci, dim_flat)
    class(mci_test_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: dim_flat
  end subroutine mci_test_ignore_flat_dimensions
  
  subroutine mci_test_ignore_equivalences (mci, channel, dim_offset)
    class(mci_test_t), intent(inout) :: mci
    type(phs_channel_t), dimension(:), intent(in) :: channel
    integer, intent(in) :: dim_offset
  end subroutine mci_test_ignore_equivalences
  
  subroutine mci_test_set_divisions (object, divisions)
    class(mci_test_t), intent(inout) :: object
    integer, intent(in) :: divisions
    object%divisions = divisions
  end subroutine mci_test_set_divisions
  
  subroutine mci_test_set_max_factor (object, max_factor)
    class(mci_test_t), intent(inout) :: object
    real(default), intent(in) :: max_factor
    object%max_factor = max_factor
  end subroutine mci_test_set_max_factor
  
  subroutine mci_test_allocate_instance (mci, mci_instance)
    class(mci_test_t), intent(in) :: mci
    class(mci_instance_t), intent(out), pointer :: mci_instance
    allocate (mci_test_instance_t :: mci_instance)
  end subroutine mci_test_allocate_instance
  
  subroutine mci_test_integrate (mci, instance, sampler, &
       n_it, n_calls, results, pacify)
    class(mci_test_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: pacify
    class(mci_results_t), intent(inout), optional :: results
    real(default), dimension(:), allocatable :: integral
    real(default), dimension(:), allocatable :: x
    integer :: i, j, c
    select type (instance)
    type is (mci_test_instance_t)
       allocate (integral (mci%n_channel))
       integral = 0
       allocate (x (mci%n_dim))
       select case (mci%n_dim)
       case (1)
          do c = 1, mci%n_channel
             do i = 1, mci%divisions
                x(1) = (i - 0.5_default) / mci%divisions
                call instance%evaluate (sampler, c, x)
                integral(c) = integral(c) + instance%get_value ()
             end do
          end do
          mci%integral = dot_product (instance%w, integral) &
               / mci%divisions
          mci%integral_known = .true.
       case (2)
          do c = 1, mci%n_channel
             do i = 1, mci%divisions
                x(1) = (i - 0.5_default) / mci%divisions
                do j = 1, mci%divisions
                   x(2) = (j - 0.5_default) / mci%divisions
                   call instance%evaluate (sampler, c, x)
                   integral(c) = integral(c) + instance%get_value ()
                end do
             end do
          end do
          mci%integral = dot_product (instance%w, integral) &
               / mci%divisions / mci%divisions
          mci%integral_known = .true.
       end select
       if (present (results)) then
          call results%record (n_it, n_calls, &
               mci%integral, mci%error, &
               efficiency = 0._default)
       end if
    end select
  end subroutine mci_test_integrate
  
  subroutine mci_test_ignore_prepare_simulation (mci)
    class(mci_test_t), intent(inout) :: mci
  end subroutine mci_test_ignore_prepare_simulation
  
  subroutine mci_test_generate_weighted_event (mci, instance, sampler)
    class(mci_test_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    real(default) :: r
    real(default), dimension(:), allocatable :: x
    integer :: c
    select type (instance)
    type is (mci_test_instance_t)
       allocate (x (mci%n_dim))
       select case (mci%n_channel)
       case (1)
          c = 1
          call mci%rng%generate (x(1))
       case (2)
          call mci%rng%generate (r)
          if (r < instance%w(1)) then
             c = 1
          else
             c = 2
          end if
          call mci%rng%generate (x)
       end select
       call instance%evaluate (sampler, c, x)
    end select
  end subroutine mci_test_generate_weighted_event
       
  subroutine mci_test_generate_unweighted_event (mci, instance, sampler)
    class(mci_test_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    real(default) :: r
    integer :: i 
    select type (instance)
    type is (mci_test_instance_t)
       mci%tries = 0
       do i = 1, 10
          call mci%generate_weighted_event (instance, sampler)
          mci%tries = mci%tries + 1
          call mci%rng%generate (r)
          if (r < instance%rel_value)  exit
       end do
    end select
  end subroutine mci_test_generate_unweighted_event
    
  subroutine mci_test_rebuild_event (mci, instance, sampler, state)
    class(mci_test_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
    select type (instance)
    type is (mci_test_instance_t)
       call instance%recall (sampler, state)
    end select
  end subroutine mci_test_rebuild_event
    
  subroutine mci_test_instance_write (object, unit, pacify)
    class(mci_test_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    integer :: u, c
    u = given_output_unit (unit)
    write (u, "(1x,A,ES13.7)") "Result value = ", object%value
    write (u, "(1x,A,ES13.7)") "Rel. weight  = ", object%rel_value
    write (u, "(1x,A,ES13.7)") "Integrand    = ", object%integrand
    write (u, "(1x,A,ES13.7)") "MCI weight   = ", object%mci_weight
    write (u, "(3x,A,I0)")  "c = ", object%selected_channel
    write (u, "(3x,A,ES13.7)") "g = ", object%g
    write (u, "(1x,A)")  "Channel parameters:"
    do c = 1, object%mci%n_channel
       write (u, "(1x,I0,A,4(1x,ES13.7))")  c, ": w/f/g/m =", &
            object%w(c), object%f(c), object%gi(c), object%max(c)
       write (u, "(4x,A,9(1x,F9.7))")  "x =", object%x(:,c)
    end do
  end subroutine mci_test_instance_write
  
  subroutine mci_test_instance_final (object)
    class(mci_test_instance_t), intent(inout) :: object
  end subroutine mci_test_instance_final
  
  subroutine mci_test_instance_init (mci_instance, mci)
    class(mci_test_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
    call mci_instance%base_init (mci)
    select type (mci)
    type is (mci_test_t)
       mci_instance%mci => mci
    end select
    allocate (mci_instance%gi (mci%n_channel))
    mci_instance%gi = 0
    allocate (mci_instance%max (mci%n_channel))
    select case (mci%n_channel)
    case (1)
       mci_instance%max = 1._default
    case (2)
       mci_instance%max = 2._default
    end select
  end subroutine mci_test_instance_init
    
  subroutine mci_test_instance_compute_weight (mci, c)
    class(mci_test_instance_t), intent(inout) :: mci
    integer, intent(in) :: c
    integer :: i
    mci%selected_channel = c
    select case (mci%mci%n_dim)
    case (1)
       mci%gi(1) = 1
    case (2)
       mci%gi(1) = 1
       mci%gi(2) = 2 * mci%x(2,2)
    end select
    mci%g = 0
    do i = 1, mci%mci%n_channel
       mci%g = mci%g + mci%w(i) * mci%gi(i) / mci%f(i)
    end do
    mci%mci_weight = mci%gi(c) / mci%g
  end subroutine mci_test_instance_compute_weight
    
  subroutine mci_test_instance_record_integrand (mci, integrand)
    class(mci_test_instance_t), intent(inout) :: mci
    real(default), intent(in) :: integrand
    mci%integrand = integrand
    mci%value = mci%integrand * mci%mci_weight
    mci%rel_value = mci%value / mci%max(mci%selected_channel) &
         / mci%mci%max_factor
  end subroutine mci_test_instance_record_integrand
  
  subroutine mci_test_instance_init_simulation (instance, safety_factor)
    class(mci_test_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: safety_factor
  end subroutine mci_test_instance_init_simulation
  
  subroutine mci_test_instance_final_simulation (instance)
    class(mci_test_instance_t), intent(inout) :: instance
  end subroutine mci_test_instance_final_simulation
  
  function mci_test_instance_get_event_excess (mci) result (excess)
    class(mci_test_instance_t), intent(in) :: mci
    real(default) :: excess
    excess = 0
  end function mci_test_instance_get_event_excess
  
  subroutine test_sampler_init (sampler, n)
    class(test_sampler_t), intent(out) :: sampler
    integer, intent(in) :: n
    allocate (sampler%x (n, n))
    allocate (sampler%f (n))
  end subroutine test_sampler_init
  
  subroutine test_sampler_write (object, unit, testflag)
    class(test_sampler_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, c
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Test sampler:"
    write (u, "(3x,A,ES13.7)")  "Integrand = ", object%integrand
    write (u, "(3x,A,I0)")      "Channel   = ", object%selected_channel
    do c = 1, size (object%f)
       write (u, "(1x,I0,':',1x,A,ES13.7)") c, "f = ", object%f(c)
       write (u, "(4x,A,9(1x,F9.7))") "x =", object%x(:,c)
    end do
  end subroutine test_sampler_write
  
  subroutine test_sampler_compute (sampler, c, x_in)
    class(test_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    sampler%selected_channel = c
    select case (size (sampler%f))
    case (1)
       sampler%x(:,1) = x_in
       sampler%f = 1
    case (2)
       select case (c)
       case (1)
          sampler%x(:,1) = x_in
          sampler%x(1,2) = sqrt (x_in(1))
          sampler%x(2,2) = x_in(2)
       case (2)
          sampler%x(1,1) = x_in(1) ** 2
          sampler%x(2,1) = x_in(2)
          sampler%x(:,2) = x_in
       end select
       sampler%f(1) = 1
       sampler%f(2) = 2 * sampler%x(1,2)
    end select
  end subroutine test_sampler_compute

  function test_sampler_is_valid (sampler) result (valid)
    class(test_sampler_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_is_valid
  
  subroutine test_sampler_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    sampler%integrand = 1
    val = sampler%integrand
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_evaluate

  subroutine test_sampler_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%compute (c, x_in)
    sampler%integrand = val
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_rebuild

  subroutine test_sampler_fetch (sampler, val, x, f)
    class(test_sampler_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%integrand
    x = sampler%x
    f = sampler%f
  end subroutine test_sampler_fetch

  subroutine mci_test_results_write (object, unit, verbose, suppress)
    class(mci_test_results_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, suppress
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A,1x,I0)") "Iterations = ", object%n_it
    write (u, "(3x,A,1x,I0)") "Calls      = ", object%n_calls
    write (u, "(3x,A,1x,F12.10)")  "Integral   = ", object%integral
    write (u, "(3x,A,1x,F12.10)")  "Error      = ", object%error
    write (u, "(3x,A,1x,F12.10)")  "Efficiency = ", object%efficiency
  end subroutine mci_test_results_write
  
  subroutine mci_test_results_record (object, n_it, n_calls, &
       integral, error, efficiency, chain_weights, suppress)
    class(mci_test_results_t), intent(inout) :: object
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    real(default), intent(in) :: integral
    real(default), intent(in) :: error
    real(default), intent(in) :: efficiency
    real(default), dimension(:), intent(in), optional :: chain_weights
    logical, intent(in), optional :: suppress
    object%n_it = n_it
    object%n_calls = n_calls
    object%integral = integral
    object%error = error
    object%efficiency = efficiency
  end subroutine mci_test_results_record

  subroutine mci_base_1 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler

    real(default) :: integrand
    
    write (u, "(A)")  "* Test output: mci_base_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test integrator"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (2)
    end select

    write (u, "(A)")  "* Evaluate sampler for given point and channel"
    write (u, "(A)")
    
    call sampler%evaluate (1, [0.25_default, 0.8_default], &
         integrand, mci_instance%x, mci_instance%f)

    call sampler%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Compute MCI weight"
    write (u, "(A)")
    
    call mci_instance%compute_weight (1)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Get integrand and compute weight for another point"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 2, [0.5_default, 0.6_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Recall results, again"
    write (u, "(A)")
    
    call mci_instance%final ()
    deallocate (mci_instance)
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)

    call mci_instance%fetch (sampler, 2)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Retrieve value"
    write (u, "(A)")
    
    write (u, "(1x,A,ES13.7)")  "Weighted integrand = ", &
         mci_instance%get_value ()

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_1"

  end subroutine mci_base_1

  subroutine mci_base_2 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_base_2"
    write (u, "(A)")  "*   Purpose: perform a test integral"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (1, 1)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select
    
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (1)
    end select
    
    write (u, "(A)")  "* Integrate"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 0, 0)
    
    call mci%write (u)
    
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_2"

  end subroutine mci_base_2

  subroutine mci_base_3 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_base_3"
    write (u, "(A)")  "*   Purpose: perform a nontrivial test integral"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select

    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (2)
    end select
    
    write (u, "(A)")  "* Integrate"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 0, 0)
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with higher resolution"
    write (u, "(A)")

    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (100)
    end select

    call mci%integrate (mci_instance, sampler, 0, 0)
    call mci%write (u)

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_3"

  end subroutine mci_base_3

  subroutine mci_base_4 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: mci_base_4"
    write (u, "(A)")  "*   Purpose: generate events"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator, instance, sampler"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (2)
    end select
    
    allocate (rng_test_t :: rng)
    call mci%import_rng (rng)
    
    write (u, "(A)")  "* Generate weighted event"
    write (u, "(A)")
    
    call mci%generate_weighted_event (mci_instance, sampler)

    call sampler%write (u)
    write (u, *)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate unweighted event"
    write (u, "(A)")
    
    call mci%generate_unweighted_event (mci_instance, sampler)

    select type (mci)
    type is (mci_test_t)
       write (u, "(A,I0)")  " Success in try ", mci%tries
       write (u, "(A)")
    end select
    
    call sampler%write (u)
    write (u, *)
    call mci_instance%write (u)
    
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_4"

  end subroutine mci_base_4

  subroutine mci_base_5 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    class(mci_state_t), allocatable :: state
    
    write (u, "(A)")  "* Test output: mci_base_5"
    write (u, "(A)")  "*   Purpose: store and recall an event"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator, instance, sampler"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (2)
    end select
    
    allocate (rng_test_t :: rng)
    call mci%import_rng (rng)
    
    write (u, "(A)")  "* Generate weighted event"
    write (u, "(A)")
    
    call mci%generate_weighted_event (mci_instance, sampler)

    call sampler%write (u)
    write (u, *)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Store data"
    write (u, "(A)")
    
    allocate (state)
    call mci_instance%store (state)
    call mci_instance%final ()
    deallocate (mci_instance)
    
    call state%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Recall data and rebuild event"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    call mci%rebuild_event (mci_instance, sampler, state)

    call sampler%write (u)
    write (u, *)
    call mci_instance%write (u)
    
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_5"

  end subroutine mci_base_5

  subroutine mci_base_6 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    
    write (u, "(A)")  "* Test output: mci_base_6"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test integrator with chains"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (1, 5)
    
    write (u, "(A)")  "* Introduce chains"
    write (u, "(A)")
    
    call mci%declare_chains ([1, 2, 2, 1, 2])

    call mci%write (u)
    
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_6"

  end subroutine mci_base_6

  subroutine mci_base_7 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(mci_results_t), allocatable :: results
    
    write (u, "(A)")  "* Test output: mci_base_7"
    write (u, "(A)")  "*   Purpose: perform a nontrivial test integral &
         &and record results"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select

    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_t :: sampler)
    select type (sampler)
    type is (test_sampler_t)
       call sampler%init (2)
    end select
    
    allocate (mci_test_results_t :: results)

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 1, 1000, results)
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Display results"
    write (u, "(A)")

    call results%write (u)
    
    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_7"

  end subroutine mci_base_7

  subroutine mci_base_8 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    
    write (u, "(A)")  "* Test output: mci_base_8"
    write (u, "(A)")  "*   Purpose: check timer availability"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator with timer"
    write (u, "(A)")

    allocate (mci_test_t :: mci)
    call mci%set_dimensions (2, 2)
    select type (mci)
    type is (mci_test_t)
       call mci%set_divisions (10)
    end select

    call mci%set_timer (active = .true.)
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Start timer"
    write (u, "(A)")

    call mci%start_timer ()
    call mci%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Stop timer"
    write (u, "(A)")

    call mci%stop_timer ()
    call mci%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Readout"
    write (u, "(A)")

    write (u, "(1x,A,F6.3)")  "Time = ", mci%get_time ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Deactivate timer"
    write (u, "(A)")

    call mci%set_timer (active = .false.)
    call mci%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_base_8"

  end subroutine mci_base_8


end module mci_base
