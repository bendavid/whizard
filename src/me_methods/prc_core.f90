! WHIZARD 2.6.0 Sep 08 2017
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
module prc_core

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use lorentz
  use interactions

  use process_constants
  use prc_core_def
  use process_libraries
  use sf_base

  implicit none
  private

  public :: prc_core_t
  public :: prc_core_state_t
  public :: helicity_selection_t

  type, abstract :: prc_core_t
     class(prc_core_def_t), pointer :: def => null ()
     logical :: data_known = .false.
     type(process_constants_t) :: data
     class(prc_core_driver_t), allocatable :: driver
     logical :: use_color_factors = .false.
     integer :: nc = 3
   contains
     procedure(prc_core_write), deferred :: write
     procedure(prc_core_write_name), deferred :: write_name
     procedure :: init => prc_core_init
     procedure :: base_init => prc_core_init
     procedure :: has_matrix_element => prc_core_has_matrix_element
     procedure(prc_core_is_allowed), deferred :: is_allowed
     procedure :: get_constants => prc_core_get_constants
     procedure :: get_alpha_s => prc_core_get_alpha_s
     procedure :: allocate_workspace => prc_core_ignore_workspace
     procedure(prc_core_compute_hard_kinematics), deferred :: &
          compute_hard_kinematics
     procedure(prc_core_compute_eff_kinematics), deferred :: &
          compute_eff_kinematics
     procedure(prc_core_compute_amplitude), deferred :: compute_amplitude
  end type prc_core_t

  type, abstract :: prc_core_state_t
   contains
     procedure(workspace_write), deferred :: write
     procedure(workspace_reset_new_kinematics), deferred :: reset_new_kinematics
  end type prc_core_state_t

  type :: helicity_selection_t
     logical :: active = .false.
     real(default) :: threshold = 0
     integer :: cutoff = 0
   contains
     procedure :: write => helicity_selection_write
  end type helicity_selection_t


  abstract interface
     subroutine prc_core_write (object, unit)
       import
       class(prc_core_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine prc_core_write
  end interface

  abstract interface
     subroutine prc_core_write_name (object, unit)
       import
       class(prc_core_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine prc_core_write_name
  end interface

  abstract interface
     function prc_core_is_allowed (object, i_term, f, h, c) result (flag)
       import
       class(prc_core_t), intent(in) :: object
       integer, intent(in) :: i_term, f, h, c
       logical :: flag
     end function prc_core_is_allowed
  end interface

  abstract interface
     subroutine prc_core_compute_hard_kinematics &
          (object, p_seed, i_term, int_hard, core_state)
       import
       class(prc_core_t), intent(in) :: object
       type(vector4_t), dimension(:), intent(in) :: p_seed
       integer, intent(in) :: i_term
       type(interaction_t), intent(inout) :: int_hard
       class(prc_core_state_t), intent(inout), allocatable :: core_state
     end subroutine prc_core_compute_hard_kinematics
  end interface

  abstract interface
     subroutine prc_core_compute_eff_kinematics &
          (object, i_term, int_hard, int_eff, core_state)
       import
       class(prc_core_t), intent(in) :: object
       integer, intent(in) :: i_term
       type(interaction_t), intent(in) :: int_hard
       type(interaction_t), intent(inout) :: int_eff
       class(prc_core_state_t), intent(inout), allocatable :: core_state
     end subroutine prc_core_compute_eff_kinematics
  end interface

  abstract interface
     function prc_core_compute_amplitude &
          (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
          core_state) result (amp)
       import
       complex(default) :: amp
       class(prc_core_t), intent(in) :: object
       integer, intent(in) :: j
       type(vector4_t), dimension(:), intent(in) :: p
       integer, intent(in) :: f, h, c
       real(default), intent(in) :: fac_scale, ren_scale
       real(default), intent(in), allocatable :: alpha_qcd_forced
       class(prc_core_state_t), intent(inout), allocatable, optional :: &
            core_state
     end function prc_core_compute_amplitude
  end interface

  abstract interface
     subroutine workspace_write (object, unit)
       import
       class(prc_core_state_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine workspace_write
  end interface

  abstract interface
    subroutine workspace_reset_new_kinematics (object)
      import
      class(prc_core_state_t), intent(inout) :: object
    end subroutine workspace_reset_new_kinematics
  end interface


contains

  subroutine prc_core_init (object, def, lib, id, i_component)
    class(prc_core_t), intent(inout) :: object
    class(prc_core_def_t), intent(in), target :: def
    type(process_library_t), intent(in), target :: lib
    type(string_t), intent(in) :: id
    integer, intent(in) :: i_component
    object%def => def
    call lib%connect_process (id, i_component, object%data, object%driver)
    object%data_known = .true.
  end subroutine prc_core_init

  function prc_core_has_matrix_element (object) result (flag)
    class(prc_core_t), intent(in) :: object
    logical :: flag
    flag = object%data%n_flv /= 0
  end function prc_core_has_matrix_element

  subroutine prc_core_get_constants (object, data, i_term)
    class(prc_core_t), intent(in) :: object
    type(process_constants_t), intent(out) :: data
    integer, intent(in) :: i_term
    data = object%data
  end subroutine prc_core_get_constants

  function prc_core_get_alpha_s (object, core_state) result (alpha)
    class(prc_core_t), intent(in) :: object
    class(prc_core_state_t), intent(in), allocatable :: core_state
    real(default) :: alpha
    alpha = -1
  end function prc_core_get_alpha_s

  subroutine prc_core_ignore_workspace (object, core_state)
    class(prc_core_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
  end subroutine prc_core_ignore_workspace

  subroutine helicity_selection_write (object, unit)
    class(helicity_selection_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (object%active) then
       write (u, "(3x,A)")  "Helicity selection data:"
       write (u, "(5x,A,ES17.10)") &
            "threshold =", object%threshold
       write (u, "(5x,A,I0)") &
            "cutoff    = ", object%cutoff
    end if
  end subroutine helicity_selection_write


end module prc_core
