! WHIZARD 2.2.7 Aug 11 2015
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
module prc_user_defined

  use, intrinsic :: iso_c_binding !NODEP!
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use lorentz
  use interactions

  use prc_core_def
  use prc_core
 
  implicit none
  private

  public :: prc_user_defined_base_t
  public :: prc_tt_threshold_t

  type, abstract, extends (prc_core_t) :: prc_user_defined_base_t
  contains
    procedure :: needs_mcset => prc_user_defined_base_needs_mcset
    procedure :: get_n_terms => prc_user_defined_base_get_n_terms
    procedure :: is_allowed => prc_user_defined_base_is_allowed
    procedure :: compute_hard_kinematics => prc_user_defined_base_compute_hard_kinematics
    procedure :: compute_eff_kinematics => prc_user_defined_base_compute_eff_kinematics
    procedure :: recover_kinematics => prc_user_defined_base_recover_kinematics
  end type prc_user_defined_base_t

  type, abstract, extends (prc_core_state_t) :: user_defined_state_t
    logical :: new_kinematics = .true.
    real(default) :: alpha_qcd = -1
  contains
  
  end type user_defined_state_t

  type, extends (prc_user_defined_base_t) :: prc_tt_threshold_t
  contains
    procedure :: write => prc_tt_threshold_write 
    procedure :: compute_amplitude => prc_tt_threshold_compute_amplitude
  end type prc_tt_threshold_t




contains

  function prc_user_defined_base_needs_mcset (object) result (flag)
    class(prc_user_defined_base_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function prc_user_defined_base_needs_mcset

  function prc_user_defined_base_get_n_terms (object) result (n)
    class(prc_user_defined_base_t), intent(in) :: object
    integer :: n
    n = 1
  end function prc_user_defined_base_get_n_terms

  function prc_user_defined_base_is_allowed (object, i_term, f, h, c) result (flag)
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_term, f, h, c
    logical :: flag
    logical(c_bool) :: cflag
    select type (driver => object%driver)
    class is (prc_core_driver_t)
!       call driver%is_allowed (f, h, c, cflag)
!       flag = cflag
       flag = .true.
    end select
  end function prc_user_defined_base_is_allowed

  subroutine prc_user_defined_base_compute_hard_kinematics &
       (object, p_seed, i_term, int_hard, core_state)
    class(prc_user_defined_base_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(in) :: p_seed
    integer, intent(in) :: i_term
    type(interaction_t), intent(inout) :: int_hard
    class(prc_core_state_t), intent(inout), allocatable :: core_state 
    call int_hard%set_momenta (p_seed)
    if (allocated (core_state)) then
      select type (core_state)
      class is (user_defined_state_t); core_state%new_kinematics = .true.
      end select
    end if
  end subroutine prc_user_defined_base_compute_hard_kinematics

  subroutine prc_user_defined_base_compute_eff_kinematics &
       (object, i_term, int_hard, int_eff, core_state)
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_term
    type(interaction_t), intent(in) :: int_hard
    type(interaction_t), intent(inout) :: int_eff
    class(prc_core_state_t), intent(inout), allocatable :: core_state
  end subroutine prc_user_defined_base_compute_eff_kinematics

  subroutine prc_user_defined_base_recover_kinematics &
       (object, p_seed, int_hard, int_eff, core_state)
    class(prc_user_defined_base_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(inout) :: p_seed
    type(interaction_t), intent(inout) :: int_hard, int_eff
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    integer :: n_in
    n_in = int_eff%get_n_in ()
    call int_eff%set_momenta (p_seed(1:n_in), outgoing = .false.)
    p_seed(n_in+1:) = int_eff%get_momenta (outgoing = .true.)
  end subroutine prc_user_defined_base_recover_kinematics

  subroutine prc_tt_threshold_write (object, unit)
    class(prc_tt_threshold_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_message ("tt-threshold")
  end subroutine prc_tt_threshold_write

  function prc_tt_threshold_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
       core_state)  result (amp)
    class(prc_tt_threshold_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: core_state
    complex(default) :: amp
    !!! Intentionally left empty
  end function prc_tt_threshold_compute_amplitude


end module prc_user_defined
