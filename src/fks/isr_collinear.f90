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

module isr_collinear

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use constants, only: one, two
  use physics_defs, only: n_beam_structure_int
  use sf_base, only: sf_rescale_t

  implicit none
  private

  public :: sf_rescale_collinear_t
  public :: sf_rescale_real_t
  public :: sf_rescale_dglap_t

  type, extends (sf_rescale_t) :: sf_rescale_collinear_t
     real(default) :: xi_tilde
  contains
    procedure :: apply => sf_rescale_collinear_apply
    procedure :: set => sf_rescale_collinear_set
  end type sf_rescale_collinear_t

  type, extends (sf_rescale_t) :: sf_rescale_real_t
     real(default) :: xi, y
  contains
    procedure :: apply => sf_rescale_real_apply
    procedure :: set => sf_rescale_real_set
  end type sf_rescale_real_t

  type, extends(sf_rescale_t) :: sf_rescale_dglap_t
     real(default), dimension(:), allocatable :: z
   contains
     procedure :: apply => sf_rescale_dglap_apply
     procedure :: set => sf_rescale_dglap_set
  end type sf_rescale_dglap_t


contains

  subroutine sf_rescale_collinear_apply (func, x)
    class(sf_rescale_collinear_t), intent(in) :: func
    real(default), intent(inout) :: x
    real(default) :: xi
    if (debug2_active (D_BEAMS)) then
       print *, 'Rescaling function - Collinear: '
       print *, 'Input: ', x
       print *, 'xi_tilde: ', func%xi_tilde
    end if
    xi = func%xi_tilde * (one - x)
    x = x / (one - xi)
    if (debug2_active (D_BEAMS))  print *, 'scaled x: ', x
  end subroutine sf_rescale_collinear_apply

  subroutine sf_rescale_collinear_set (func, xi_tilde)
    class(sf_rescale_collinear_t), intent(inout) :: func
    real(default), intent(in) :: xi_tilde
    func%xi_tilde = xi_tilde
  end subroutine sf_rescale_collinear_set

  subroutine sf_rescale_real_apply (func, x)
    class(sf_rescale_real_t), intent(in) :: func
    real(default), intent(inout) :: x
    real(default) :: onepy, onemy
    if (debug2_active (D_BEAMS)) then
       print *, 'Rescaling function - Real: '
       print *, 'Input: ', x
       print *, 'Beam index: ', func%i_beam
       print *, 'xi: ', func%xi, 'y: ', func%y
    end if
    x = x / sqrt (one - func%xi)
    onepy = one + func%y; onemy = one - func%y
    if (func%i_beam == 1) then
       x = x * sqrt ((two - func%xi * onemy) / (two - func%xi * onepy))
    else if (func%i_beam == 2) then
       x = x * sqrt ((two - func%xi * onepy) / (two - func%xi * onemy))
    else
       call msg_fatal ("sf_rescale_real_apply - invalid beam index")
    end if
    if (debug2_active (D_BEAMS))  print *, 'scaled x: ', x
  end subroutine sf_rescale_real_apply

  subroutine sf_rescale_real_set (func, xi, y)
    class(sf_rescale_real_t), intent(inout) :: func
    real(default), intent(in) :: xi, y
    func%xi = xi; func%y = y
  end subroutine sf_rescale_real_set

  subroutine sf_rescale_dglap_apply (func, x)
    class(sf_rescale_dglap_t), intent(in) :: func
    real(default), intent(inout) :: x
    if (debug2_active (D_BEAMS))  then
       print *, "Rescaling function - DGLAP:"
       print *, "Input: ", x
       print *, "Beam index: ", func%i_beam
       print *, "z: ", func%z
    end if
    x = x / func%z(func%i_beam)
    if (debug2_active (D_BEAMS)) print *, "scaled x: ", x
  end subroutine sf_rescale_dglap_apply

  subroutine sf_rescale_dglap_set (func, z)
    class(sf_rescale_dglap_t), intent(inout) :: func
    real(default), dimension(:), intent(in) :: z
    ! allocate-on-assginment
    func%z = z
  end subroutine sf_rescale_dglap_set


end module isr_collinear

