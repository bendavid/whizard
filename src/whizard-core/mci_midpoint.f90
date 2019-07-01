! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module mci_midpoint

  use kinds
  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use diagnostics

  use phs_base
  use rng_base
  use mci_base
  
  implicit none
  private

  public :: mci_midpoint_t
  public :: mci_midpoint_instance_t
  public :: mci_midpoint_test

  type, extends (mci_t) :: mci_midpoint_t
     integer :: n_dim_binned = 0
     logical, dimension(:), allocatable :: dim_is_binned
     logical :: calls_known = .false.
     integer :: n_calls = 0
     integer :: n_calls_pos = 0
     integer :: n_calls_nul = 0
     integer :: n_calls_neg = 0
     real(default) :: integral_pos = 0
     real(default) :: integral_neg = 0
     integer, dimension(:), allocatable :: n_bin
     logical :: max_known = .false.
     real(default) :: max = 0
     real(default) :: min = 0
     real(default) :: max_abs = 0
     real(default) :: min_abs = 0
   contains
     procedure :: final => mci_midpoint_final
     procedure :: write => mci_midpoint_write
     procedure :: startup_message => mci_midpoint_startup_message
     procedure :: set_dimensions => mci_midpoint_set_dimensions
     procedure :: declare_flat_dimensions => mci_midpoint_declare_flat_dimensions
     procedure :: declare_equivalences => mci_midpoint_ignore_equivalences
     procedure :: allocate_instance => mci_midpoint_allocate_instance
     procedure :: integrate => mci_midpoint_integrate
     procedure :: prepare_simulation => mci_midpoint_ignore_prepare_simulation
     procedure :: generate_weighted_event => mci_midpoint_generate_weighted_event
     procedure :: generate_unweighted_event => &
          mci_midpoint_generate_unweighted_event
     procedure :: rebuild_event => mci_midpoint_rebuild_event
end type mci_midpoint_t
  
  type, extends (mci_instance_t) :: mci_midpoint_instance_t
     type(mci_midpoint_t), pointer :: mci => null ()
     logical :: max_known = .false.
     real(default) :: max = 0
     real(default) :: min = 0
     real(default) :: max_abs = 0
     real(default) :: min_abs = 0
     real(default) :: safety_factor = 1
     real(default) :: excess_weight = 0
   contains
     procedure :: write => mci_midpoint_instance_write
     procedure :: final => mci_midpoint_instance_final
     procedure :: init => mci_midpoint_instance_init
     procedure :: get_max => mci_midpoint_instance_get_max
     procedure :: set_max => mci_midpoint_instance_set_max
     procedure :: compute_weight => mci_midpoint_instance_compute_weight
     procedure :: record_integrand => mci_midpoint_instance_record_integrand
     procedure :: init_simulation => mci_midpoint_instance_init_simulation
     procedure :: final_simulation => mci_midpoint_instance_final_simulation
     procedure :: get_event_excess => mci_midpoint_instance_get_event_excess
  end type mci_midpoint_instance_t
  

  type, extends (mci_sampler_t) :: test_sampler_1_t
     real(default), dimension(:), allocatable :: x
     real(default) :: val
   contains
     procedure :: write => test_sampler_1_write
     procedure :: evaluate => test_sampler_1_evaluate
     procedure :: is_valid => test_sampler_1_is_valid
     procedure :: rebuild => test_sampler_1_rebuild
     procedure :: fetch => test_sampler_1_fetch
  end type test_sampler_1_t

  type, extends (mci_sampler_t) :: test_sampler_2_t
     real(default) :: val
     real(default), dimension(2) :: x
   contains
     procedure :: write => test_sampler_2_write
     procedure :: evaluate => test_sampler_2_evaluate
     procedure :: is_valid => test_sampler_2_is_valid
     procedure :: rebuild => test_sampler_2_rebuild
     procedure :: fetch => test_sampler_2_fetch
  end type test_sampler_2_t

  type, extends (mci_sampler_t) :: test_sampler_4_t
     real(default) :: val
     real(default), dimension(:), allocatable :: x
   contains
     procedure :: write => test_sampler_4_write
     procedure :: evaluate => test_sampler_4_evaluate
     procedure :: is_valid => test_sampler_4_is_valid
     procedure :: rebuild => test_sampler_4_rebuild
     procedure :: fetch => test_sampler_4_fetch
  end type test_sampler_4_t


contains
  
  subroutine mci_midpoint_final (object)
    class(mci_midpoint_t), intent(inout) :: object
    call object%base_final ()
  end subroutine mci_midpoint_final
  
  subroutine mci_midpoint_write (object, unit, pacify, md5sum_version)
    class(mci_midpoint_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Single-channel midpoint rule integrator:"
    call object%base_write (u, pacify, md5sum_version)
    if (object%n_dim_binned < object%n_dim) then
       write (u, "(3x,A,99(1x,I0))")  "Flat dimensions      =", &
            pack ([(i, i = 1, object%n_dim)], mask = .not. object%dim_is_binned)
       write (u, "(3x,A,I0)")  "Number of binned dim = ", object%n_dim_binned
    end if
    if (object%calls_known) then
       write (u, "(3x,A,99(1x,I0))")  "Number of bins       =", object%n_bin
       write (u, "(3x,A,I0)")  "Number of calls      = ", object%n_calls
       if (object%n_calls_pos /= object%n_calls) then
          write (u, "(3x,A,I0)")  "  positive value     = ", object%n_calls_pos
          write (u, "(3x,A,I0)")  "  zero value         = ", object%n_calls_nul
          write (u, "(3x,A,I0)")  "  negative value     = ", object%n_calls_neg
          write (u, "(3x,A,ES17.10)") &
               "Integral (pos. part) = ", object%integral_pos
          write (u, "(3x,A,ES17.10)") &
               "Integral (neg. part) = ", object%integral_neg
       end if
    end if
    if (object%max_known) then
       write (u, "(3x,A,ES17.10)")  "Maximum of integrand = ", object%max
       write (u, "(3x,A,ES17.10)")  "Minimum of integrand = ", object%min
       if (object%min /= object%min_abs) then
          write (u, "(3x,A,ES17.10)")  "Maximum (abs. value) = ", object%max_abs
          write (u, "(3x,A,ES17.10)")  "Minimum (abs. value) = ", object%min_abs
       end if
    end if
    if (allocated (object%rng))  call object%rng%write (u)
  end subroutine mci_midpoint_write
    
  subroutine mci_midpoint_startup_message (mci, unit, n_calls)
    class(mci_midpoint_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    call mci%base_startup_message (unit = unit, n_calls = n_calls)
    if (mci%n_dim_binned < mci%n_dim) then
       write (msg_buffer, "(A,2(1x,I0,1x,A))") &
            "Integrator: Midpoint rule:", &
            mci%n_dim_binned, "binned dimensions"
    else
       write (msg_buffer, "(A,2(1x,I0,1x,A))") &
            "Integrator: Midpoint rule"
    end if
    call msg_message (unit = unit)
  end subroutine mci_midpoint_startup_message
    
  subroutine mci_midpoint_set_dimensions (mci, n_dim, n_channel)
    class(mci_midpoint_t), intent(inout) :: mci
    integer, intent(in) :: n_dim
    integer, intent(in) :: n_channel
    if (n_channel == 1) then
       mci%n_channel = n_channel
       mci%n_dim = n_dim
       allocate (mci%dim_is_binned (mci%n_dim))
       mci%dim_is_binned = .true.
       mci%n_dim_binned = count (mci%dim_is_binned)
       allocate (mci%n_bin (mci%n_dim))
       mci%n_bin = 0
    else
       call msg_fatal ("Attempt to initialize single-channel integrator &
            &for multiple channels")
    end if
  end subroutine mci_midpoint_set_dimensions
  
  subroutine mci_midpoint_declare_flat_dimensions (mci, dim_flat)
    class(mci_midpoint_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: dim_flat
    integer :: d
    mci%n_dim_binned = mci%n_dim - size (dim_flat)
    do d = 1, size (dim_flat)
       mci%dim_is_binned(dim_flat(d)) = .false.
    end do
    mci%n_dim_binned = count (mci%dim_is_binned)
  end subroutine mci_midpoint_declare_flat_dimensions
  
  subroutine mci_midpoint_ignore_equivalences (mci, channel, dim_offset)
    class(mci_midpoint_t), intent(inout) :: mci
    type(phs_channel_t), dimension(:), intent(in) :: channel
    integer, intent(in) :: dim_offset
  end subroutine mci_midpoint_ignore_equivalences
  
  subroutine mci_midpoint_allocate_instance (mci, mci_instance)
    class(mci_midpoint_t), intent(in) :: mci
    class(mci_instance_t), intent(out), pointer :: mci_instance
    allocate (mci_midpoint_instance_t :: mci_instance)
  end subroutine mci_midpoint_allocate_instance
  
  subroutine mci_midpoint_integrate (mci, instance, sampler, n_it, n_calls, &
       results, pacify)
    class(mci_midpoint_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: pacify
    class(mci_results_t), intent(inout), optional :: results
    real(default), dimension(:), allocatable :: x
    real(default) :: integral, integral_pos, integral_neg
    integer :: n_bin
    select type (instance)
    type is (mci_midpoint_instance_t)
       allocate (x (mci%n_dim))
       integral = 0
       integral_pos = 0
       integral_neg = 0
       select case (mci%n_dim_binned)
       case (1)
          n_bin = n_calls
       case (2:)
          n_bin = max (int (n_calls ** (1. / mci%n_dim_binned)), 1)
       end select
       where (mci%dim_is_binned)
          mci%n_bin = n_bin
       elsewhere
          mci%n_bin = 1
       end where
       mci%n_calls = product (mci%n_bin)
       mci%n_calls_pos = 0
       mci%n_calls_nul = 0
       mci%n_calls_neg = 0
       mci%calls_known = .true.
       call sample_dim (mci%n_dim)
       mci%integral = integral / mci%n_calls
       mci%integral_pos = integral_pos / mci%n_calls
       mci%integral_neg = integral_neg / mci%n_calls
       mci%integral_known = .true.
       call instance%set_max ()
       if (present (results)) then
          call results%record (1, mci%n_calls, &
               mci%integral, mci%error, mci%efficiency)
       end if
    end select
  contains
    recursive subroutine sample_dim (d)
      integer, intent(in) :: d
      integer :: i
      real(default) :: value
      do i = 1, mci%n_bin(d)
         x(d) = (i - 0.5_default) / mci%n_bin(d)
         if (d > 1) then
            call sample_dim (d - 1)
         else
            if (signal_is_pending ())  return
            call instance%evaluate (sampler, 1, x)
            value = instance%get_value ()
            if (value > 0) then
               mci%n_calls_pos = mci%n_calls_pos + 1
               integral = integral + value
               integral_pos = integral_pos + value
            else if (value == 0) then
               mci%n_calls_nul = mci%n_calls_nul + 1
            else
               mci%n_calls_neg = mci%n_calls_neg + 1
               integral = integral + value
               integral_neg = integral_neg + value
            end if
         end if
      end do
    end subroutine sample_dim
  end subroutine mci_midpoint_integrate

  subroutine mci_midpoint_ignore_prepare_simulation (mci)
    class(mci_midpoint_t), intent(inout) :: mci
  end subroutine mci_midpoint_ignore_prepare_simulation
  
  subroutine mci_midpoint_generate_weighted_event (mci, instance, sampler)
    class(mci_midpoint_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    real(default), dimension(mci%n_dim) :: x
    select type (instance)
    type is (mci_midpoint_instance_t)
       call mci%rng%generate (x)
       call instance%evaluate (sampler, 1, x)
       instance%excess_weight = 0
    end select
  end subroutine mci_midpoint_generate_weighted_event
       
  subroutine mci_midpoint_generate_unweighted_event (mci, instance, sampler)
    class(mci_midpoint_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    real(default) :: x, norm, int
    select type (instance)
    type is (mci_midpoint_instance_t)
       if (mci%max_known .and. mci%max_abs > 0) then
          norm = abs (mci%max_abs * instance%safety_factor)
          REJECTION: do
             call mci%generate_weighted_event (instance, sampler)
             if (sampler%is_valid ()) then
                call mci%rng%generate (x)
                int = abs (instance%integrand)
                if (x * norm <= int) then
                   if (norm > 0 .and. norm < int) then
                      instance%excess_weight = int / norm - 1
                   end if
                   exit REJECTION
                end if
             end if
             if (signal_is_pending ())  return
          end do REJECTION
       else
          call msg_fatal ("Unweighted event generation: &
               &maximum of integrand is zero or unknown")
       end if
    end select
  end subroutine mci_midpoint_generate_unweighted_event
    
  subroutine mci_midpoint_rebuild_event (mci, instance, sampler, state)
    class(mci_midpoint_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
    select type (instance)
    type is (mci_midpoint_instance_t)
       call instance%recall (sampler, state)
    end select
  end subroutine mci_midpoint_rebuild_event
       
  subroutine mci_midpoint_instance_write (object, unit, pacify)
    class(mci_midpoint_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A,9(1x,F12.10))")  "x =", object%x(:,1)
    write (u, "(1x,A,ES19.12)") "Integrand = ", object%integrand
    write (u, "(1x,A,ES19.12)") "Weight    = ", object%mci_weight
    if (object%safety_factor /= 1) then
       write (u, "(1x,A,ES19.12)") "Safety f  = ", object%safety_factor
    end if
    if (object%excess_weight /= 0) then
       write (u, "(1x,A,ES19.12)") "Excess    = ", object%excess_weight
    end if
    if (object%max_known) then
       write (u, "(1x,A,ES19.12)") "Maximum   = ", object%max
       write (u, "(1x,A,ES19.12)") "Minimum   = ", object%min
       if (object%min /= object%min_abs) then
          write (u, "(1x,A,ES19.12)") "Max.(abs) = ", object%max_abs
          write (u, "(1x,A,ES19.12)") "Min.(abs) = ", object%min_abs
       end if
    end if
  end subroutine mci_midpoint_instance_write
  
  subroutine mci_midpoint_instance_init (mci_instance, mci)
    class(mci_midpoint_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
    call mci_instance%base_init (mci)
    select type (mci)
    type is (mci_midpoint_t)
       mci_instance%mci => mci
       call mci_instance%get_max ()
       mci_instance%selected_channel = 1
    end select
  end subroutine mci_midpoint_instance_init
    
  subroutine mci_midpoint_instance_get_max (instance)
    class(mci_midpoint_instance_t), intent(inout) :: instance
    associate (mci => instance%mci)
      if (mci%max_known) then
         instance%max_known = .true.
         instance%max = mci%max
         instance%min = mci%min
         instance%max_abs = mci%max_abs
         instance%min_abs = mci%min_abs
      end if
    end associate
  end subroutine mci_midpoint_instance_get_max
  
  subroutine mci_midpoint_instance_set_max (instance)
    class(mci_midpoint_instance_t), intent(inout) :: instance
    associate (mci => instance%mci)
      if (instance%max_known) then
         if (mci%max_known) then
            mci%max = max (mci%max, instance%max)
            mci%min = min (mci%min, instance%min)
            mci%max_abs = max (mci%max_abs, instance%max_abs)
            mci%min_abs = min (mci%min_abs, instance%min_abs)
         else
            mci%max = instance%max
            mci%min = instance%min
            mci%max_abs = instance%max_abs
            mci%min_abs = instance%min_abs
            mci%max_known = .true.
         end if
         if (mci%max_abs /= 0) then
            if (mci%integral == mci%integral_pos) then
               mci%efficiency = mci%integral / mci%max_abs
               mci%efficiency_known = .true.
            else if (mci%n_calls /= 0) then
               mci%efficiency = &
                    (mci%n_calls_pos * mci%integral_pos &
                    - mci%n_calls_neg * mci%integral_neg) &
                    / mci%n_calls / mci%max_abs
               mci%efficiency_known = .true.
            end if
         end if
      end if
    end associate
  end subroutine mci_midpoint_instance_set_max
  
  subroutine mci_midpoint_instance_compute_weight (mci, c)
    class(mci_midpoint_instance_t), intent(inout) :: mci
    integer, intent(in) :: c
    select case (c)
    case (1)
       mci%mci_weight = mci%f(1)
    case default
       call msg_fatal ("MCI midpoint integrator: only single channel supported")
    end select
  end subroutine mci_midpoint_instance_compute_weight
    
  subroutine mci_midpoint_instance_record_integrand (mci, integrand)
    class(mci_midpoint_instance_t), intent(inout) :: mci
    real(default), intent(in) :: integrand
    mci%integrand = integrand
    if (mci%max_known) then
       mci%max = max (mci%max, integrand)
       mci%min = min (mci%min, integrand)
       mci%max_abs = max (mci%max_abs, abs (integrand))
       mci%min_abs = min (mci%min_abs, abs (integrand))
    else
       mci%max = integrand
       mci%min = integrand
       mci%max_abs = abs (integrand)
       mci%min_abs = abs (integrand)
       mci%max_known = .true.
    end if
  end subroutine mci_midpoint_instance_record_integrand
  
  subroutine mci_midpoint_instance_init_simulation (instance, safety_factor)
    class(mci_midpoint_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: safety_factor
    if (present (safety_factor))  instance%safety_factor = safety_factor
  end subroutine mci_midpoint_instance_init_simulation
  
  subroutine mci_midpoint_instance_final_simulation (instance)
    class(mci_midpoint_instance_t), intent(inout) :: instance
  end subroutine mci_midpoint_instance_final_simulation
  
  function mci_midpoint_instance_get_event_excess (mci) result (excess)
    class(mci_midpoint_instance_t), intent(in) :: mci
    real(default) :: excess
    excess = mci%excess_weight
  end function mci_midpoint_instance_get_event_excess
  
  subroutine test_sampler_1_write (object, unit, testflag)
    class(test_sampler_1_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Test sampler: f(x) = 3 x^2"
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
    sampler%val = 3 * x_in(1) ** 2
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_1_evaluate

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
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Test sampler: f(x) = 3 x^2 + 2 y"
  end subroutine test_sampler_2_write
  
  subroutine test_sampler_2_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_2_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    sampler%x = x_in
    sampler%val = 3 * x_in(1) ** 2 + 2 * x_in(2)
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_2_evaluate

  subroutine test_sampler_2_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_2_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    sampler%x = x_in
    sampler%val = val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_2_rebuild

  subroutine test_sampler_2_fetch (sampler, val, x, f)
    class(test_sampler_2_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_2_fetch
  
  subroutine test_sampler_4_write (object, unit, testflag)
    class(test_sampler_4_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Test sampler: f(x) = 1 - 3 x^2"
  end subroutine test_sampler_4_write
  
  subroutine test_sampler_4_evaluate (sampler, c, x_in, val, x, f)
    class(test_sampler_4_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    if (x_in(1) >= .5_default) then
       sampler%val = 1 - 3 * x_in(1) ** 2
    else
       sampler%val = 0
    end if
    if (.not. allocated (sampler%x))  allocate (sampler%x (size (x_in)))
    sampler%x = x_in
    call sampler%fetch (val, x, f)
  end subroutine test_sampler_4_evaluate

  subroutine test_sampler_4_rebuild (sampler, c, x_in, val, x, f)
    class(test_sampler_4_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    sampler%x = x_in
    sampler%val = val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_4_rebuild

  subroutine test_sampler_4_fetch (sampler, val, x, f)
    class(test_sampler_4_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    val = sampler%val
    x(:,1) = sampler%x
    f = 1
  end subroutine test_sampler_4_fetch
    

  subroutine mci_midpoint_instance_final (object)
    class(mci_midpoint_instance_t), intent(inout) :: object
  end subroutine mci_midpoint_instance_final
  
  subroutine mci_midpoint_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (mci_midpoint_1, "mci_midpoint_1", &
         "one-dimensional integral", &
         u, results)
    call test (mci_midpoint_2, "mci_midpoint_2", &
         "two-dimensional integral", &
         u, results)
    call test (mci_midpoint_3, "mci_midpoint_3", &
         "two-dimensional integral with flat dimension", &
         u, results)
    call test (mci_midpoint_4, "mci_midpoint_4", &
         "integrand with sign flip", &
         u, results)
    call test (mci_midpoint_5, "mci_midpoint_5", &
         "weighted events", &
         u, results)
    call test (mci_midpoint_6, "mci_midpoint_6", &
         "unweighted events", &
         u, results)
    call test (mci_midpoint_7, "mci_midpoint_7", &
         "excess weight", &
         u, results)
  end subroutine mci_midpoint_test
  
  function test_sampler_1_is_valid (sampler) result (valid)
    class(test_sampler_1_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_1_is_valid
  
  function test_sampler_2_is_valid (sampler) result (valid)
    class(test_sampler_2_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_2_is_valid
  
  function test_sampler_4_is_valid (sampler) result (valid)
    class(test_sampler_4_t), intent(in) :: sampler
    logical :: valid
    valid = .true.
  end function test_sampler_4_is_valid
  
  subroutine mci_midpoint_1 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_midpoint_1"
    write (u, "(A)")  "*   Purpose: integrate function in one dimension"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (1, 1)
    
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
    write (u, "(A)")  "* Evaluate for x = 0.8"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.8_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for x = 0.7"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.7_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for x = 0.9"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.9_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_calls = 1000"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_1"

  end subroutine mci_midpoint_1

  subroutine mci_midpoint_2 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_midpoint_2"
    write (u, "(A)")  "*   Purpose: integrate function in two dimensions"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (2, 1)
    
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_2_t :: sampler)
    call sampler%write (u)
     
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for x = 0.8, y = 0.2"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.8_default, 0.2_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_calls = 1000"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_2"

  end subroutine mci_midpoint_2

  subroutine mci_midpoint_3 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_midpoint_3"
    write (u, "(A)")  "*   Purpose: integrate function with one flat dimension"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    select type (mci)
    type is (mci_midpoint_t)
       call mci%set_dimensions (2, 1)
       call mci%declare_flat_dimensions ([2])
    end select
    
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
    write (u, "(A)")  "* Evaluate for x = 0.8, y = 0.2"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.8_default, 0.2_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_calls = 1000"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_3"

  end subroutine mci_midpoint_3

  subroutine mci_midpoint_4 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    
    write (u, "(A)")  "* Test output: mci_midpoint_4"
    write (u, "(A)")  "*   Purpose: integrate function with sign flip &
         &in one dimension"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (1, 1)
    
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_4_t :: sampler)
    call sampler%write (u)
     
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for x = 0.8"
    write (u, "(A)")
    
    call mci_instance%evaluate (sampler, 1, [0.8_default])
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Integrate with n_calls = 1000"
    write (u, "(A)")
    
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)

    call mci_instance%final ()
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_4"

  end subroutine mci_midpoint_4

  subroutine mci_midpoint_5 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng
    class(mci_state_t), allocatable :: state

    write (u, "(A)")  "* Test output: mci_midpoint_5"
    write (u, "(A)")  "*   Purpose: generate weighted events"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (2, 1)
    
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_2_t :: sampler)

    write (u, "(A)")  "* Initialize random-number generator"
    write (u, "(A)")
    
    allocate (rng_test_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)
    
    write (u, "(A)")  "* Generate weighted event"
    write (u, "(A)")
    
    call mci%generate_weighted_event (mci_instance, sampler)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate weighted event"
    write (u, "(A)")
    
    call mci%generate_weighted_event (mci_instance, sampler)
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

    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    deallocate (mci_instance)
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_5"

  end subroutine mci_midpoint_5
    
  subroutine mci_midpoint_6 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng

    write (u, "(A)")  "* Test output: mci_midpoint_6"
    write (u, "(A)")  "*   Purpose: generate unweighted events"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (1, 1)
    
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_4_t :: sampler)

    write (u, "(A)")  "* Initialize random-number generator"
    write (u, "(A)")
    
    allocate (rng_test_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)
    
    write (u, "(A)")  "* Integrate (determine maximum of integrand"
    write (u, "(A)")
    call mci%integrate (mci_instance, sampler, 1, 1000)
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate unweighted event"
    write (u, "(A)")
    
    call mci%generate_unweighted_event (mci_instance, sampler)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    deallocate (mci_instance)
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_6"

  end subroutine mci_midpoint_6
    
  subroutine mci_midpoint_7 (u)
    integer, intent(in) :: u
    class(mci_t), allocatable, target :: mci
    class(mci_instance_t), pointer :: mci_instance => null ()
    class(mci_sampler_t), allocatable :: sampler
    class(rng_t), allocatable :: rng

    write (u, "(A)")  "* Test output: mci_midpoint_7"
    write (u, "(A)")  "*   Purpose: generate unweighted event &
         &with excess weight"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize integrator"
    write (u, "(A)")

    allocate (mci_midpoint_t :: mci)
    call mci%set_dimensions (1, 1)
    
    write (u, "(A)")  "* Initialize instance"
    write (u, "(A)")
    
    call mci%allocate_instance (mci_instance)
    call mci_instance%init (mci)
    
    write (u, "(A)")  "* Initialize test sampler"
    write (u, "(A)")
    
    allocate (test_sampler_4_t :: sampler)

    write (u, "(A)")  "* Initialize random-number generator"
    write (u, "(A)")
    
    allocate (rng_test_t :: rng)
    call rng%init ()
    call mci%import_rng (rng)
    
    write (u, "(A)")  "* Integrate (determine maximum of integrand"
    write (u, "(A)")
    call mci%integrate (mci_instance, sampler, 1, 2)
    call mci%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate unweighted event"
    write (u, "(A)")
    
    call mci_instance%init_simulation ()
    call mci%generate_unweighted_event (mci_instance, sampler)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Use getter methods"
    write (u, "(A)")
    
    write (u, "(1x,A,1x,ES19.12)")  "weight =", mci_instance%get_event_weight ()
    write (u, "(1x,A,1x,ES19.12)")  "excess =", mci_instance%get_event_excess ()

    write (u, "(A)")
    write (u, "(A)")  "* Apply safety factor"
    write (u, "(A)")

    call mci_instance%init_simulation (safety_factor = 2.1_default)

    write (u, "(A)")  "* Generate unweighted event"
    write (u, "(A)")
    
    call mci%generate_unweighted_event (mci_instance, sampler)
    call mci_instance%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Use getter methods"
    write (u, "(A)")
    
    write (u, "(1x,A,1x,ES19.12)")  "weight =", mci_instance%get_event_weight ()
    write (u, "(1x,A,1x,ES19.12)")  "excess =", mci_instance%get_event_excess ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call mci_instance%final ()
    deallocate (mci_instance)
    call mci%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: mci_midpoint_7"

  end subroutine mci_midpoint_7
    

end module mci_midpoint
