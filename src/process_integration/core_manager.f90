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

module core_manager

  use format_utils, only: write_integer_array, write_separator
  use physics_defs, only: BORN, NLO_REAL
  use io_units
  use prc_core

  implicit none
  private

  public :: core_manager_t

  integer, parameter, public :: N_MAX_CORES = 100


  type :: generic_core_t
    class(prc_core_t), allocatable :: core
  end type  generic_core_t

  type :: core_manager_t
    integer, dimension(N_MAX_CORES) :: i_component_to_i_core = 0
    integer, dimension(N_MAX_CORES) :: i_core = 0
    logical, dimension(N_MAX_CORES) :: sub = .false.
    character(32), dimension(N_MAX_CORES) :: md5s = ""
    integer, dimension(N_MAX_CORES) :: nlo_type
    integer :: n_cores = 0
    integer, dimension(:), allocatable :: i_core_to_first_i_component
    type(generic_core_t), dimension(:), allocatable :: cores
    integer :: current_index = 1
  contains
    procedure :: register_new => cm_register_new
    procedure :: register_existing => cm_register_existing
    procedure :: allocate_core_array => cm_allocate_core_array
    procedure :: create_i_core_to_first_i_component &
       => core_manager_create_i_core_to_first_i_component
    procedure :: allocate_core => cm_allocate_core
    procedure :: get_core => cm_get_core
    procedure :: get_subtraction_core => cm_get_subtraction_core
    procedure :: get_flv_states => cm_get_flv_states
    procedure :: core_is_radiation => cm_core_is_radiation
    procedure :: write => cm_write
    procedure :: final => cm_final
  end type core_manager_t


contains

  subroutine cm_register_new (cm, nlo_type, i_component, md5sum)
    class(core_manager_t), intent(inout) :: cm
    integer, intent(in) :: nlo_type, i_component
    character(32), intent(in) :: md5sum
    cm%nlo_type(cm%current_index) = nlo_type
    cm%md5s(cm%current_index) = md5sum
    cm%i_component_to_i_core(i_component) = cm%current_index
    cm%i_core(cm%current_index) = cm%current_index
    cm%current_index = cm%current_index + 1
  end subroutine cm_register_new

  subroutine cm_register_existing (cm, i_existing, i_component)
    class(core_manager_t), intent(inout) :: cm
    integer, intent(in) :: i_existing, i_component
    integer :: i_core
    i_core = cm%i_component_to_i_core(i_existing)
    cm%i_component_to_i_core(i_component) = i_core
  end subroutine cm_register_existing

  subroutine cm_allocate_core_array (cm)
    class(core_manager_t), intent(inout) :: cm
    cm%n_cores = count (cm%i_core > 0)
    allocate (cm%cores (cm%n_cores))
  end subroutine cm_allocate_core_array

  subroutine core_manager_create_i_core_to_first_i_component (cm, n_components)
    class(core_manager_t), intent(inout) :: cm
    integer, intent(in) :: n_components
    integer :: i, i_core
    allocate (cm%i_core_to_first_i_component (cm%n_cores))
    cm%i_core_to_first_i_component = 0
    do i = 1, n_components
       if (.not. any (cm%i_core_to_first_i_component == i)) then
          i_core = cm%i_component_to_i_core (i)
          cm%i_core_to_first_i_component(i_core) = i
       end if
    end do
  end subroutine core_manager_create_i_core_to_first_i_component

  subroutine cm_allocate_core (cm, i_core, core_template)
    class(core_manager_t), intent(inout) :: cm
    integer, intent(in) :: i_core
    class(prc_core_t), intent(in) :: core_template
    allocate (cm%cores(i_core)%core, source = core_template)
  end subroutine cm_allocate_core

  function cm_get_core (cm, i_core) result (core)
    class(prc_core_t), pointer :: core
    class(core_manager_t), intent(in), target :: cm
    integer, intent(in) :: i_core
    core => cm%cores(i_core)%core
  end function cm_get_core

  function cm_get_subtraction_core (cm) result (core)
    class(prc_core_t), pointer :: core
    class(core_manager_t), intent(in), target :: cm
    integer :: i
    core => null ()
    do i = 1, cm%n_cores
       if (cm%sub(i)) then
          core => cm%cores(i)%core
          exit
       end if
    end do
  end function cm_get_subtraction_core

  pure subroutine cm_get_flv_states (cm, flv_born, flv_real, n_in)
    class(core_manager_t), intent(in) :: cm
    integer, dimension(:,:), allocatable, intent(out) :: flv_born, flv_real
    integer, intent(out) :: n_in
    integer :: i
    do i = 1, cm%n_cores
       if (cm%nlo_type(i) == BORN) then
          if (.not. allocated (flv_born)) &
             allocate (flv_born (size (cm%cores(i)%core%data%flv_state, 1), &
                size (cm%cores(i)%core%data%flv_state, 2)))
          flv_born = cm%cores(i)%core%data%flv_state
          n_in = cm%cores(i)%core%data%n_in
       else if (cm%nlo_type(i) == NLO_REAL) then
          if (.not. allocated (flv_real)) &
             allocate (flv_real (size (cm%cores(i)%core%data%flv_state, 1), &
                size (cm%cores(i)%core%data%flv_state, 2)))
          flv_real = cm%cores(i)%core%data%flv_state
          n_in = cm%cores(i)%core%data%n_in
       end if
    end do
  end subroutine cm_get_flv_states

  elemental function cm_core_is_radiation (cm, i_core) result (is_rad)
    logical :: is_rad
    class(core_manager_t), intent(in) :: cm
    integer, intent(in) :: i_core
    is_rad = cm%nlo_type(i_core) == NLO_REAL .and. .not. cm%sub(i_core)
  end function cm_core_is_radiation

  subroutine cm_write (cm, unit)
    class(core_manager_t), intent(in) :: cm
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u,"(1x,A,I1,A)") "component manager instance with ", cm%n_cores, " cores: "
    write (u,"(1x,A)") "(* denotes subtraction cores)"
    write (u,"(1x,A,L1)") 'Cores allocated? ', allocated (cm%cores)
    do i = 1, cm%n_cores
       write (u,"(1x,A,I1,A)", advance = "no") "Core nr. ", i, ": "
       if (cm%sub(i)) write (u,"(A)", advance = "no") "*"
       call cm%cores(i)%core%write_name (u)
    end do
    call write_separator (u, 1)
    write (u,"(1x,A)") "i_component -> i_core: "
    do i = 1, N_MAX_CORES
       if (cm%i_component_to_i_core(i) > 0) then
          write (u, "(I0, A)", advance = "no") cm%i_component_to_i_core(i), ", "
       else
          write (u, "(A)") ""
          exit
       end if
    end do
    call write_separator (u, 1)
    if (allocated (cm%i_core_to_first_i_component)) then
       write (u, "(1x,A)") "i_core -> i_component_first: "
       call write_integer_array (cm%i_core_to_first_i_component, &
          unit = u, n_max = cm%n_cores)
    else
       write (u, "(1X,A)") "cm%i_core_to_first_i_component: Not allocated."
    end if
    call write_separator (u, 1)
    write (u,"(1x,A)") "nlo type -> i_core: "
    call write_integer_array (cm%i_core, unit = u, n_max = cm%n_cores)
    call write_separator (u, 1)
    write (u, "(1x,A)") "Md5 sums: "
    do i = 1, cm%n_cores
       write (u, "(A,A)") cm%md5s(i)
    end do
    call write_separator (u, 1)
  end subroutine cm_write

  subroutine cm_final (cm)
    class(core_manager_t), intent(inout) :: cm
    cm%i_component_to_i_core = 0
    cm%n_cores = 0
    cm%i_core = 0
    cm%nlo_type = 0
    cm%md5s = ""
    cm%current_index = 1
    if (allocated (cm%cores)) deallocate (cm%cores)
    if (allocated (cm%i_core_to_first_i_component)) &
       deallocate (cm%i_core_to_first_i_component)
  end subroutine cm_final



end module core_manager
