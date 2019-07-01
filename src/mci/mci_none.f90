! WHIZARD 2.7.1 Mar 27 2019
!
! Copyright (C) 1999-2019 by
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

module mci_none

  use kinds, only: default
  use io_units, only: given_output_unit
  use diagnostics, only: msg_message, msg_fatal
  use phs_base, only: phs_channel_t

  use mci_base

  implicit none
  private

  public :: mci_none_t
  public :: mci_none_instance_t

  type, extends (mci_t) :: mci_none_t
   contains
     procedure :: final => mci_none_final
     procedure :: write => mci_none_write
     procedure :: startup_message => mci_none_startup_message
     procedure :: write_log_entry => mci_none_write_log_entry
     procedure :: compute_md5sum => mci_none_compute_md5sum
     procedure :: declare_flat_dimensions => mci_none_ignore_flat_dimensions
     procedure :: declare_equivalences => mci_none_ignore_equivalences
     procedure :: allocate_instance => mci_none_allocate_instance
     procedure :: integrate => mci_none_integrate
     procedure :: prepare_simulation => mci_none_ignore_prepare_simulation
     procedure :: generate_weighted_event => mci_none_generate_no_event
     procedure :: generate_unweighted_event => mci_none_generate_no_event
     procedure :: rebuild_event => mci_none_rebuild_event
end type mci_none_t

  type, extends (mci_instance_t) :: mci_none_instance_t
   contains
     procedure :: write => mci_none_instance_write
     procedure :: final => mci_none_instance_final
     procedure :: init => mci_none_instance_init
     procedure :: compute_weight => mci_none_instance_compute_weight
     procedure :: record_integrand => mci_none_instance_record_integrand
     procedure :: init_simulation => mci_none_instance_init_simulation
     procedure :: final_simulation => mci_none_instance_final_simulation
     procedure :: get_event_excess => mci_none_instance_get_event_excess
  end type mci_none_instance_t


contains

  subroutine mci_none_final (object)
    class(mci_none_t), intent(inout) :: object
  end subroutine mci_none_final

  subroutine mci_none_write (object, unit, pacify, md5sum_version)
    class(mci_none_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical, intent(in), optional :: md5sum_version
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Integrator: non-functional dummy"
  end subroutine mci_none_write

  subroutine mci_none_startup_message (mci, unit, n_calls)
    class(mci_none_t), intent(in) :: mci
    integer, intent(in), optional :: unit, n_calls
    call msg_message ("Integrator: none")
  end subroutine mci_none_startup_message

  subroutine mci_none_write_log_entry (mci, u)
    class(mci_none_t), intent(in) :: mci
    integer, intent(in) :: u
    write (u, "(1x,A)")  "MC Integrator is none (no-op)"
  end subroutine mci_none_write_log_entry

  subroutine mci_none_compute_md5sum (mci, pacify)
    class(mci_none_t), intent(inout) :: mci
    logical, intent(in), optional :: pacify
  end subroutine mci_none_compute_md5sum

  subroutine mci_none_ignore_flat_dimensions (mci, dim_flat)
    class(mci_none_t), intent(inout) :: mci
    integer, dimension(:), intent(in) :: dim_flat
  end subroutine mci_none_ignore_flat_dimensions

  subroutine mci_none_ignore_equivalences (mci, channel, dim_offset)
    class(mci_none_t), intent(inout) :: mci
    type(phs_channel_t), dimension(:), intent(in) :: channel
    integer, intent(in) :: dim_offset
  end subroutine mci_none_ignore_equivalences

  subroutine mci_none_allocate_instance (mci, mci_instance)
    class(mci_none_t), intent(in) :: mci
    class(mci_instance_t), intent(out), pointer :: mci_instance
    allocate (mci_none_instance_t :: mci_instance)
  end subroutine mci_none_allocate_instance

  subroutine mci_none_integrate (mci, instance, sampler, n_it, n_calls, &
       results, pacify)
    class(mci_none_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: pacify
    class(mci_results_t), intent(inout), optional :: results
    call msg_fatal ("Integration: attempt to integrate with the 'mci_none' method")
  end subroutine mci_none_integrate

  subroutine mci_none_ignore_prepare_simulation (mci)
    class(mci_none_t), intent(inout) :: mci
  end subroutine mci_none_ignore_prepare_simulation

  subroutine mci_none_generate_no_event (mci, instance, sampler)
    class(mci_none_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    call msg_fatal ("Integration: attempt to generate event with the 'mci_none' method")
  end subroutine mci_none_generate_no_event

  subroutine mci_none_rebuild_event (mci, instance, sampler, state)
    class(mci_none_t), intent(inout) :: mci
    class(mci_instance_t), intent(inout) :: instance
    class(mci_sampler_t), intent(inout) :: sampler
    class(mci_state_t), intent(in) :: state
  end subroutine mci_none_rebuild_event

  subroutine mci_none_instance_write (object, unit, pacify)
    class(mci_none_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Integrator instance: non-functional dummy"
  end subroutine mci_none_instance_write

  subroutine mci_none_instance_final (object)
    class(mci_none_instance_t), intent(inout) :: object
  end subroutine mci_none_instance_final

  subroutine mci_none_instance_init (mci_instance, mci)
    class(mci_none_instance_t), intent(out) :: mci_instance
    class(mci_t), intent(in), target :: mci
  end subroutine mci_none_instance_init

  subroutine mci_none_instance_compute_weight (mci, c)
    class(mci_none_instance_t), intent(inout) :: mci
    integer, intent(in) :: c
    call msg_fatal ("Integration: attempt to compute weight with the 'mci_none' method")
  end subroutine mci_none_instance_compute_weight

  subroutine mci_none_instance_record_integrand (mci, integrand)
    class(mci_none_instance_t), intent(inout) :: mci
    real(default), intent(in) :: integrand
  end subroutine mci_none_instance_record_integrand

  subroutine mci_none_instance_init_simulation (instance, safety_factor)
    class(mci_none_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: safety_factor
  end subroutine mci_none_instance_init_simulation

  subroutine mci_none_instance_final_simulation (instance)
    class(mci_none_instance_t), intent(inout) :: instance
  end subroutine mci_none_instance_final_simulation

  function mci_none_instance_get_event_excess (mci) result (excess)
    class(mci_none_instance_t), intent(in) :: mci
    real(default) :: excess
    excess = 0
  end function mci_none_instance_get_event_excess


end module mci_none
