! WHIZARD 2.6.3 Feb 10 2018
!
! Copyright (C) 1999-2018 by
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

module muli_dsigma
  use kinds, only: default
  use constants
  use muli_base
  use muli_momentum
  use muli_interactions
  use muli_cuba
  use muli_trapezium
  use muli_aq

  implicit none
  private

  integer, parameter :: dim_f = 17


  public :: muli_dsigma_t

  type, extends (aq_class) :: muli_dsigma_t
     private
     type(transverse_mom_t) :: pt
     type(cuba_divonne_t) :: cuba_int
   contains
     procedure :: write_to_marker => muli_dsigma_write_to_marker
     procedure :: read_from_marker => muli_dsigma_read_from_marker
     procedure :: print_to_unit => muli_dsigma_print_to_unit
     procedure, nopass :: get_type => muli_dsigma_get_type
     procedure :: generate => muli_dsigma_generate
     procedure :: evaluate => muli_dsigma_evaluate
     generic :: initialize => muli_dsigma_initialize
     procedure :: muli_dsigma_initialize
     ! procedure :: reset => muli_dsigma_reset
  end type muli_dsigma_t


contains

  subroutine muli_dsigma_write_to_marker (this, marker, status)
    class(muli_dsigma_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(ser_class_t), pointer :: ser
    call marker%mark_begin ("muli_dsigma_t")
    call this%basic_write_to_marker (marker, status)
    call this%cuba_int%serialize (marker, "cuba_int")
    call marker%mark_end ("muli_dsigma_t")
  end subroutine muli_dsigma_write_to_marker

  subroutine muli_dsigma_read_from_marker (this, marker, status)
    class(muli_dsigma_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%pick_begin ("muli_dsigma_t", status=status)
    call this%basic_read_from_marker (marker, status)
    call this%cuba_int%deserialize ("cuba_int", marker)
    call marker%pick_end ("muli_dsigma_t", status)
  end subroutine muli_dsigma_read_from_marker

  subroutine muli_dsigma_print_to_unit &
       (this, unit, parents, components, peers)
    class(muli_dsigma_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer :: ite
    if (parents > 0)  call this%basic_print_to_unit &
         (unit, parents-1, components, peers)
    write (unit, "(A)")  "Components of muli_dsigma_t"
    if (components > 0) then
       write (unit, "(A)")  "Printing components of cuba_int:"
       call this%cuba_int%print_to_unit (unit, parents, components-1, peers)
    else
       write (unit, "(A)")  "Skipping components of cuba_int:"
    end if
  end subroutine muli_dsigma_print_to_unit

  pure subroutine muli_dsigma_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="muli_dsigma_t")
  end subroutine muli_dsigma_get_type

  subroutine muli_dsigma_generate (this, gev2_scale_cutoff, gev2_s, int_tree)
    class(muli_dsigma_t), intent(inout) :: this
    real(default), intent(in) :: gev2_scale_cutoff, gev2_s
    type(muli_trapezium_tree_t), intent(out) :: int_tree
    real(default), dimension(ceiling (log (gev2_s/gev2_scale_cutoff)/two)) :: &
         initial_values
    integer :: n
    print *, gev2_s/gev2_scale_cutoff, &
         ceiling (log (gev2_s/gev2_scale_cutoff)/two)
    ! allocate (initial_values (ceiling (-log (gev2_scale_cutoff))/2))
    ! allocate (real(default), &
    !    dimension (ceiling (log(gev2_scale_cutoff))/2) :: initial_values)
    initial_values(1) = sqrt(gev2_scale_cutoff/gev2_s) * two
    do n = 2, size(initial_values) - 1
       initial_values(n) = initial_values(n-1) * euler
    end do
    initial_values(n) = one
    print *, initial_values
    ! stop
    call this%initialize (i_one, "dsigma")
    call this%pt%initialize (gev2_s)
    this%abs_error_goal = zero
    this%rel_error_goal = scale(one, -12) !-12
    this%max_nodes = 1000
    call this%cuba_int%set_common (dim_f=dim_f, dim_x=2, &
         eps_rel=scale(this%rel_error_goal,-8), flags = 0)
    call this%cuba_int%set_deferred (xgiven_flat = [1.E-2_default, &
         5.E-1_default + epsilon(1._default), 1.E-2_default, &
         5.E-1_default - epsilon(1._default)])
    print *, "muli_dsigma_generate:"
    ! print *, "Cuba Error Goal:    ", this%cuba_int%eps_rel
    print *, "Overall Error Goal: ", this%rel_error_goal
    call this%init_error_tree (dim_f, initial_values)
    call this%run ()
    call this%integrate (int_tree)
    call this%err_tree%deallocate_all ()
    deallocate (this%err_tree)
    nullify (this%int_list)
  end subroutine muli_dsigma_generate

  subroutine muli_dsigma_evaluate (this, x, y)
    class(muli_dsigma_t), intent(inout) :: this
    real(default), intent(in) :: x
    real(default), intent(out), dimension(:):: y
    call this%pt%set_unit_scale (x)
    ! print *, "muli_dsigma_evaluate x=", x
    ! call this%cuba_int%integrate_userdata &
    !       (interactions_proton_proton_integrand_param_17_reg, this%pt)
    ! if (this%cuba_int%fail == 0) then
    ! call this%cuba_int%print_all ()
    call this%cuba_int%get_integral_array (y)
    ! else
    !    print *, "muli_dsigma_evaluate: failed."
    !    stop
    ! end if
  end subroutine muli_dsigma_evaluate

  subroutine muli_dsigma_initialize &
       (this, id, name, goal, max_nodes, dim, cuba_goal)
    class(muli_dsigma_t), intent(inout) :: this
    integer(dik), intent(in) :: id, max_nodes
    integer, intent(in) :: dim
    character(*), intent(in) :: name
    real(default), intent(in) :: goal, cuba_goal
    call this%initialize (id,name)
    ! 1E-4
    this%rel_error_goal = goal
    this%max_nodes = max_nodes
    call this%cuba_int%set_common (dim_f=dim, dim_x=2, &
         ! 1E-6
         eps_rel=cuba_goal, flags = 0)
    call this%cuba_int%set_deferred (xgiven_flat = [1.E-2_default, &
         5.E-1_default + epsilon(1._default), &
         1.E-2_default, 5.E-1_default - epsilon(1._default)])
    ! call aq_initialize (this, id, name, d_goal, max_nodes, dim_f, &
    !        [8E-1_default/7E3_default, 2E-3_default, 1E-2_default, &
    !         1E-1_default, one])
    call this%init_error_tree (dim, [8.E-1_default/7.E3_default, &
         2.E-3_default, 1.E-2_default, 1.E-1_default, &
         1._default])
    this%is_deferred_initialised = .true.
  end subroutine muli_dsigma_initialize

  ! subroutine muli_dsigma_reset (this)
  !   class(muli_dsigma_t), intent(inout) :: this
  !   call aq_reset (this)
  !   call this%initialize &
  !      (id, name, d_goal, max_nodes, dim_f, init, cuba_goal)
  ! end subroutine muli_dsigma_reset


end module muli_dsigma

