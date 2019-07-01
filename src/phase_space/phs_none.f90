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

module phs_none

  use kinds, only: default
  use io_units, only: given_output_unit
  use diagnostics, only: msg_message, msg_fatal
  use phs_base, only: phs_config_t, phs_t

  implicit none
  private

  public :: phs_none_config_t
  public :: phs_none_t

  type, extends (phs_config_t) :: phs_none_config_t
  contains
    procedure :: final => phs_none_config_final
    procedure :: write => phs_none_config_write
    procedure :: configure => phs_none_config_configure
    procedure :: startup_message => phs_none_config_startup_message
    procedure, nopass :: allocate_instance => phs_none_config_allocate_instance
  end type phs_none_config_t

  type, extends (phs_t) :: phs_none_t
   contains
     procedure :: write => phs_none_write
     procedure :: final => phs_none_final
     procedure :: init => phs_none_init
     procedure :: evaluate_selected_channel => phs_none_evaluate_selected_channel
     procedure :: evaluate_other_channels => phs_none_evaluate_other_channels
     procedure :: inverse => phs_none_inverse
  end type phs_none_t


contains

  subroutine phs_none_config_final (object)
    class(phs_none_config_t), intent(inout) :: object
  end subroutine phs_none_config_final

  subroutine phs_none_config_write (object, unit, include_id)
    class(phs_none_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: include_id
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Partonic phase-space configuration: non-functional dummy"
  end subroutine phs_none_config_write

  subroutine phs_none_config_configure (phs_config, sqrts, &
       sqrts_fixed, cm_frame, azimuthal_dependence, rebuild, ignore_mismatch, &
       nlo_type)
    class(phs_none_config_t), intent(inout) :: phs_config
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: sqrts_fixed
    logical, intent(in), optional :: cm_frame
    logical, intent(in), optional :: azimuthal_dependence
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    integer, intent(in), optional :: nlo_type
  end subroutine phs_none_config_configure

  subroutine phs_none_config_startup_message (phs_config, unit)
    class(phs_none_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    call msg_message ("Phase space: none")
  end subroutine phs_none_config_startup_message

  subroutine phs_none_config_allocate_instance (phs)
    class(phs_t), intent(inout), pointer :: phs
    allocate (phs_none_t :: phs)
  end subroutine phs_none_config_allocate_instance

  subroutine phs_none_write (object, unit, verbose)
    class(phs_none_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A)")  "Partonic phase space: none"
  end subroutine phs_none_write

  subroutine phs_none_final (object)
    class(phs_none_t), intent(inout) :: object
  end subroutine phs_none_final

  subroutine phs_none_init (phs, phs_config)
    class(phs_none_t), intent(out) :: phs
    class(phs_config_t), intent(in), target :: phs_config
    call phs%base_init (phs_config)
  end subroutine phs_none_init

  subroutine phs_none_evaluate_selected_channel (phs, c_in, r_in)
    class(phs_none_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    real(default), intent(in), dimension(:) :: r_in
    call msg_fatal ("Phase space: attempt to evaluate with the 'phs_none' method")
  end subroutine phs_none_evaluate_selected_channel

  subroutine phs_none_evaluate_other_channels (phs, c_in)
    class(phs_none_t), intent(inout) :: phs
    integer, intent(in) :: c_in
  end subroutine phs_none_evaluate_other_channels

  subroutine phs_none_inverse (phs)
    class(phs_none_t), intent(inout) :: phs
    call msg_fatal ("Phase space: attempt to evaluate inverse with the 'phs_none' method")
  end subroutine phs_none_inverse


end module phs_none
