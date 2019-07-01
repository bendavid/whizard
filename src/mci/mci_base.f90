! WHIZARD 2.3.1 Aug 25 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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
  use io_units
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_14, FMT_17
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
     procedure(mci_write_log_entry), deferred :: write_log_entry
     procedure(mci_compute_md5sum), deferred :: compute_md5sum
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
     procedure :: pacify => mci_pacify
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
     logical :: negative_weights = .false.
   contains
     procedure (mci_instance_write), deferred :: write
     procedure (mci_instance_final), deferred :: final
     procedure (mci_instance_base_init), deferred :: init
     procedure :: base_init => mci_instance_base_init
     procedure :: set_channel_weights => mci_instance_set_channel_weights
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
     subroutine mci_write_log_entry (mci, u)
       import
       class(mci_t), intent(in) :: mci
       integer, intent(in) :: u
     end subroutine mci_write_log_entry
  end interface
       
  abstract interface
     subroutine mci_compute_md5sum (mci, pacify)
       import
       class(mci_t), intent(inout) :: mci
       logical, intent(in), optional :: pacify
     end subroutine mci_compute_md5sum
  end interface

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
  
  subroutine mci_add_pass (mci, adapt_grids, adapt_weights, final_pass)
    class(mci_t), intent(inout) :: mci
    logical, intent(in), optional :: adapt_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: final_pass
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
  
  subroutine mci_pacify (object, efficiency_reset, error_reset)
    class(mci_t), intent(inout) :: object
    logical, intent(in), optional :: efficiency_reset, error_reset
  end subroutine mci_pacify
    
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
    if (mci%n_channel > 0) then
       call mci_instance%set_channel_weights &
            (spread (1._default, dim=1, ncopies=mci%n_channel))
    end if
    mci_instance%f = 0
    mci_instance%x = 0
  end subroutine mci_instance_base_init
    
  subroutine mci_instance_set_channel_weights (mci_instance, weights, sum_non_zero)
    class(mci_instance_t), intent(inout) :: mci_instance
    real(default), dimension(:), intent(in) :: weights
    logical, intent(out), optional :: sum_non_zero
    real(default) :: wsum
    wsum = sum (weights)
    if (wsum /= 0) then
       mci_instance%w = weights / wsum
       if (present (sum_non_zero)) sum_non_zero = .true.
    else
       if (present (sum_non_zero)) sum_non_zero = .false.
       call msg_warning ("MC sampler initialization:&
            & sum of channel weights is zero")
    end if
  end subroutine mci_instance_set_channel_weights
  
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
    

end module mci_base
