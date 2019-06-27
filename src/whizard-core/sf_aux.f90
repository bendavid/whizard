! WHIZARD 2.0.1 Sun Apr 25 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module sf_aux

  use kinds, only: default !NODEP!
  use constants, only: twopi !NODEP!
  use file_utils !NODEP!
  use lorentz !NODEP!

  implicit none
  private

  public :: splitting_data_t
  public :: new_splitting_data
  public :: splitting_data_write
  public :: splitting_get_x_bounds
  public :: splitting_set_t_bounds
  public :: splitting_narrow_t_bounds
  public :: splitting_sample_t
  public :: splitting_set_collinear
  public :: splitting_sample_phi
  public :: split_momentum
  public :: on_shell

  integer, parameter, public :: KEEP_ENERGY = 0, KEEP_MOMENTUM = 1

  type :: splitting_data_t
     private
     real(default) :: x0 = 0
     real(default) :: x1
     real(default) :: t0
     real(default) :: t1
     real(default) :: phi0 = 0
     real(default) :: phi1 = twopi
     real(default) :: E, p, s, u, m2
     real(default) :: x, xb, pb
     real(default) :: t, phi
  end type splitting_data_t


contains

  function new_splitting_data (k, mk2, mr2, m) result (d)
    type(splitting_data_t) :: d
    type(vector4_t), intent(in) :: k
    real(default), intent(in) :: mk2, mr2, m
    d%E = energy (k)
    d%x1 = 1 - sqrt (max (mr2, 0._default)) / d%E
    d%p = sqrt (d%E**2 - mk2)
    d%s = mk2
    d%u = mr2
    d%m2 = m**2
  end function new_splitting_data

  subroutine splitting_data_write (d, unit)
    type(splitting_data_t), intent(in) :: d
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Splitting data:"
    write (u, *) " x0   =", d%x0
    write (u, *) " x    =", d%x
    write (u, *) " xb   =", d%xb
    write (u, *) " x1   =", d%x1
    write (u, *) " t0   =", d%t0
    write (u, *) " t    =", d%t
    write (u, *) " t1   =", d%t1
    write (u, *) " phi0 =", d%phi0
    write (u, *) " phi  =", d%phi
    write (u, *) " phi1 =", d%phi1
    write (u, *) " E    =", d%E
    write (u, *) " p    =", d%p
    write (u, *) " pb   =", d%pb
    write (u, *) " s    =", d%s
    write (u, *) " u    =", d%u
    write (u, *) " m2   =", d%m2
  end subroutine splitting_data_write

  function splitting_get_x_bounds (d) result (x)
    real(default), dimension(2) :: x
    type(splitting_data_t), intent(in) :: d
    x = (/ d%x0, d%x1 /)
  end function splitting_get_x_bounds

  subroutine splitting_set_t_bounds (d, x, xb)
    type(splitting_data_t), intent(inout) :: d
    real(default), intent(in) :: x, xb
    real(default) :: tp, tm
    d%x = x
    d%xb = xb
    d%pb = sqrt (max (d%E**2 - d%u / xb**2, 0._default))
    tp = -2 * xb * d%E**2 + d%s + d%u
    tm = -2 * xb * d%p * d%pb
    d%t0 = tp + tm
    d%t1 = tp - tm
  end subroutine splitting_set_t_bounds

  subroutine splitting_narrow_t_bounds (d, qmin, qmax)
    type(splitting_data_t), intent(inout) :: d
    real(default), intent(in), optional :: qmin, qmax
    if (present (qmax))  d%t0 = max (d%t0, - qmax ** 2)
    if (present (qmin))  d%t1 = min (d%t1, - qmin ** 2)
  end subroutine splitting_narrow_t_bounds

  subroutine splitting_sample_t (d, r, t0, t1)
    type(splitting_data_t), intent(inout) :: d
    real(default), intent(in) :: r
    real(default), intent(in), optional :: t0, t1
    real(default) :: tt0, tt1
    tt0 = d%t0;  if (present (t0))  tt0 = max (t0, tt0)
    tt1 = d%t1;  if (present (t1))  tt1 = min (t1, tt1)
    d%t = d%m2 + (tt0 - d%m2) * exp (r * log ((tt1 - d%m2) / (tt0 - d%m2)))
  end subroutine splitting_sample_t

  subroutine splitting_set_collinear (d)
    type(splitting_data_t), intent(inout) :: d
    d%t = d%t1
  end subroutine splitting_set_collinear

  subroutine splitting_sample_phi (d, r)
    type(splitting_data_t), intent(inout) :: d
    real(default), intent(in) :: r
    d%phi = (1-r) * d%phi0 + r * d%phi1
  end subroutine splitting_sample_phi

  function split_momentum (k, d) result (q)
    type(vector4_t), dimension(2) :: q
    type(vector4_t), intent(in) :: k
    type(splitting_data_t), intent(in) :: d
    real(default) :: ct, st, cp, sp
    type(lorentz_transformation_t) :: rot
    real(default) :: tt0, tt1, den
    type(vector3_t) :: kk, q1, q2
    if (d%t < d%t1) then
       tt0 = d%t - d%t0
       tt1 = d%t - d%t1
       den = 2 * d%xb * d%p * d%pb
       ct = (tt0 + tt1) / (2 * den)
       st = - (tt0 * tt1) / den**2
       cp = cos (d%phi)
       sp = sin (d%phi)
       rot = rotation_to_2nd (3, space_part (k))
       q1 = vector3_moving (d%xb * d%pb * (/ st * cp, st * sp, ct /))
       q2 = vector3_moving (d%p, 3) - q1
       q(1) = rot * vector4_moving (d%xb * d%E, q1)
       q(2) = rot * vector4_moving (d%x * d%E, q2)
    else if (d%s /= 0 .or. d%u /= 0) then
       kk = space_part (k)
       q1 = d%xb * (d%pb / d%p) * kk
       q2 = kk - q1
       q(1) = vector4_moving (d%xb * d%E, q1)
       q(2) = vector4_moving (d%x * d%E, q2)
    else
       q(1) = d%xb * k
       q(2) = d%x * k
    end if
  end function split_momentum
    
  elemental subroutine on_shell (p, mass, keep)
    type(vector4_t), intent(inout) :: p
    real(default), intent(in) :: mass
    integer, intent(in) :: keep
    real(default) :: E, pn
    select case (keep)
    case (KEEP_ENERGY)
       E = energy (p)
       pn = sqrt (max (E**2 - mass**2, 0._default))
       p = vector4_moving (E, pn * direction (space_part (p)))
    case (KEEP_MOMENTUM)
       E = sqrt (space_part (p) ** 2 + mass **2)
       p = vector4_moving (E, space_part (p))
    end select
  end subroutine on_shell


end module sf_aux

