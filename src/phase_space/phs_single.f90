! WHIZARD 2.7.0 Jan 21 2019
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

module phs_single

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use numeric_utils
  use diagnostics
  use os_interface
  use lorentz
  use physics_defs
  use model_data
  use flavors
  use process_constants
  use phs_base

  implicit none
  private

  public :: phs_single_config_t
  public :: phs_single_t

  type, extends (phs_config_t) :: phs_single_config_t
  contains
    procedure :: final => phs_single_config_final
    procedure :: write => phs_single_config_write
    procedure :: configure => phs_single_config_configure
    procedure :: startup_message => phs_single_config_startup_message
    procedure, nopass :: allocate_instance => phs_single_config_allocate_instance
  end type phs_single_config_t

  type, extends (phs_t) :: phs_single_t
   contains
     procedure :: write => phs_single_write
     procedure :: final => phs_single_final
     procedure :: init => phs_single_init
     procedure :: compute_factor => phs_single_compute_factor
     procedure :: evaluate_selected_channel => phs_single_evaluate_selected_channel
     procedure :: evaluate_other_channels => phs_single_evaluate_other_channels
     procedure :: decay_p => phs_single_decay_p
     procedure :: inverse => phs_single_inverse
  end type phs_single_t


contains

  subroutine phs_single_config_final (object)
    class(phs_single_config_t), intent(inout) :: object
  end subroutine phs_single_config_final

  subroutine phs_single_config_write (object, unit, include_id)
    class(phs_single_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: include_id
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Partonic phase-space configuration (single-particle):"
    call object%base_write (unit)
  end subroutine phs_single_config_write

  subroutine phs_single_config_configure (phs_config, sqrts, &
       sqrts_fixed, cm_frame, azimuthal_dependence, rebuild, ignore_mismatch, &
       nlo_type, subdir)
    class(phs_single_config_t), intent(inout) :: phs_config
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: sqrts_fixed
    logical, intent(in), optional :: cm_frame
    logical, intent(in), optional :: azimuthal_dependence
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    integer, intent(in), optional :: nlo_type
    type(string_t), intent(in), optional :: subdir
    if (.not. present (nlo_type)) &
      phs_config%nlo_type = BORN
    if (phs_config%n_out == 2) then
       phs_config%n_channel = 1
       phs_config%n_par = 2
       phs_config%sqrts = sqrts
       if (present (sqrts_fixed))  phs_config%sqrts_fixed = sqrts_fixed
       if (present (cm_frame))  phs_config%cm_frame = cm_frame
       if (present (azimuthal_dependence)) then
          phs_config%azimuthal_dependence = azimuthal_dependence
          if (.not. azimuthal_dependence) then
             allocate (phs_config%dim_flat (1))
             phs_config%dim_flat(1) = 2
          end if
       end if
       if (allocated (phs_config%channel))  deallocate (phs_config%channel)
       allocate (phs_config%channel (1))
       call phs_config%compute_md5sum ()
    else
       call msg_fatal ("Single-particle phase space requires n_out = 2")
    end if
  end subroutine phs_single_config_configure

  subroutine phs_single_config_startup_message (phs_config, unit)
    class(phs_single_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    call phs_config%base_startup_message (unit)
    write (msg_buffer, "(A,2(1x,I0,1x,A))") &
         "Phase space: single-particle"
    call msg_message (unit = unit)
  end subroutine phs_single_config_startup_message

  subroutine phs_single_config_allocate_instance (phs)
    class(phs_t), intent(inout), pointer :: phs
    allocate (phs_single_t :: phs)
  end subroutine phs_single_config_allocate_instance

  subroutine phs_single_write (object, unit, verbose)
    class(phs_single_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit)
    call object%base_write (u)
  end subroutine phs_single_write

  subroutine phs_single_final (object)
    class(phs_single_t), intent(inout) :: object
  end subroutine phs_single_final

  subroutine phs_single_init (phs, phs_config)
    class(phs_single_t), intent(out) :: phs
    class(phs_config_t), intent(in), target :: phs_config
    call phs%base_init (phs_config)
    phs%volume = 1 / (4 * twopi5)
    call phs%compute_factor ()
  end subroutine phs_single_init

  subroutine phs_single_compute_factor (phs)
    class(phs_single_t), intent(inout) :: phs
    real(default) :: s_hat
    select case (phs%config%n_in)
    case (1)
       if (.not. phs%p_defined) then
          if (sum (phs%m_out) < phs%m_in(1)) then
             s_hat = phs%m_in(1) ** 2
             phs%f(1) = 1 / s_hat &
                  * sqrt (lambda (s_hat, phs%m_out(1)**2, phs%m_out(2)**2))
          else
             print *, "m_in  = ", phs%m_in
             print *, "m_out = ", phs%m_out
             call msg_fatal ("Decay is kinematically forbidden")
          end if
       end if
    case (2)
       if (phs%config%sqrts_fixed) then
          if (phs%p_defined)  return
          s_hat = phs%config%sqrts ** 2
       else
          if (.not. phs%p_defined)  return
          s_hat = sum (phs%p) ** 2
       end if
       if (sum (phs%m_in)**2 < s_hat .and. sum (phs%m_out)**2 < s_hat) then
          phs%f(1) = 1 / s_hat * &
               ( lambda (s_hat, phs%m_in (1)**2, phs%m_in (2)**2)   &
               * lambda (s_hat, phs%m_out(1)**2, phs%m_out(2)**2) ) &
               ** 0.25_default
       else
          phs%f(1) = 0
       end if
    end select
  end subroutine phs_single_compute_factor

  subroutine phs_single_evaluate_selected_channel (phs, c_in, r_in)
    class(phs_single_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    real(default), intent(in), dimension(:) :: r_in
    !!! !!! !!! Catching a gfortran bogus warning
    type(vector4_t), dimension(2) :: p_dum
    if (phs%p_defined) then
       call phs%select_channel (c_in)
       phs%r(:,c_in) = r_in
       select case (phs%config%n_in)
       case (2)
          if (all (phs%m_in == phs%m_out)) then
             call compute_kinematics_solid_angle (phs%p, phs%q, r_in)
          else
             call msg_bug ("PHS single: inelastic scattering not implemented")
          end if
       case (1)
          !!! !!! !!! Catching a gfortran bogus warning
          !!! call compute_kinematics_solid_angle (phs%decay_p (), phs%q, x)
          p_dum = phs%decay_p ()
          call compute_kinematics_solid_angle (p_dum, phs%q, r_in)
       end select
       call phs%compute_factor ()
       phs%q_defined = .true.
       phs%r_defined = .true.
    end if
  end subroutine phs_single_evaluate_selected_channel

  subroutine phs_single_evaluate_other_channels (phs, c_in)
    class(phs_single_t), intent(inout) :: phs
    integer, intent(in) :: c_in
  end subroutine phs_single_evaluate_other_channels

  function phs_single_decay_p (phs) result (p)
    class(phs_single_t), intent(in) :: phs
    type(vector4_t), dimension(2) :: p
    real(default) :: k
    real(default), dimension(2) :: E
    k = sqrt (lambda (phs%m_in(1) ** 2, phs%m_out(1) ** 2, phs%m_out(2) ** 2)) &
         / (2 * phs%m_in(1))
    E = sqrt (phs%m_out ** 2 + k ** 2)
    p(1) = vector4_moving (E(1), k, 3)
    p(2) = vector4_moving (E(2),-k, 3)
  end function phs_single_decay_p

  subroutine phs_single_inverse (phs)
    class(phs_single_t), intent(inout) :: phs
    real(default), dimension(:), allocatable :: x
    if (phs%p_defined .and. phs%q_defined) then
       call phs%select_channel ()
       allocate (x (phs%config%n_par))
       call inverse_kinematics_solid_angle (phs%p, phs%q, x)
       phs%r(:,1) = x
       call phs%compute_factor ()
       phs%r_defined = .true.
    end if
  end subroutine phs_single_inverse


end module phs_single
