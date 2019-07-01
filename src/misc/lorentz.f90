! WHIZARD 2.0.7 Mar 19 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module lorentz

  use kinds, only: default !NODEP!
  use constants, only: pi, twopi, degree !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use c_particles

  implicit none
  private

  public :: vector3_t
  public :: vector3_write
  public :: vector3_null
  public :: vector3_canonical
  public :: vector3_moving
  public :: vector3_get_component
  public :: vector3_get_components
  public :: vector4_t
  public :: vector4_write
  public :: vector4_write_raw
  public :: vector4_read_raw
  public :: vector4_null
  public :: vector4_canonical
  public :: vector4_at_rest
  public :: vector4_moving
  public :: vector4_set_component
  public :: vector4_get_component
  public :: vector4_get_components
  public :: vector4_from_c_prt
  public :: vector4_to_c_prt
  public :: lorentz_transformation_t
  public :: lorentz_transformation_write
  public :: lorentz_transformation_get_components
  public :: identity
  public :: space_reflection

  public :: operator(==), operator(/=)
  public :: operator(+), operator(-)
  public :: operator(*), operator(/)
  public :: operator(**)

  public :: cross_product
  public :: sum
  public :: direction
  public :: space_part
  public :: array_from_vector4
  public :: azimuthal_angle
  public :: azimuthal_angle_deg
  public :: azimuthal_distance
  public :: azimuthal_distance_deg
  public :: polar_angle
  public :: polar_angle_ct
  public :: polar_angle_deg
  public :: enclosed_angle
  public :: enclosed_angle_ct
  public :: enclosed_angle_deg
  public :: enclosed_angle_rest_frame
  public :: enclosed_angle_ct_rest_frame
  public :: enclosed_angle_deg_rest_frame
  public :: transverse_part
  public :: longitudinal_part
  public :: space_part_norm
  public :: energy
  public :: invariant_mass
  public :: invariant_mass_squared
  public :: transverse_mass
  public :: rapidity
  public :: pseudorapidity
  public :: rapidity_distance
  public :: pseudorapidity_distance
  public :: eta_phi_distance
  public :: inverse
  public :: boost
  public :: rotation
  public :: rotation_to_2nd
  public :: transformation
  public :: LT_compose_r3_r2_b3
  public :: LT_compose_r2_r3_b3
  public :: axis_from_p_r3_r2_b3, axis_from_p_b3
  public :: lambda
  public :: colliding_momenta

  type :: vector3_t
     private
     real(default), dimension(3) :: p
  end type vector3_t

  type :: vector4_t
     private
     real(default), dimension(0:3) :: p = &
        (/ 0._default, 0._default, 0._default, 0._default /)
  end type vector4_t
  type :: lorentz_transformation_t
     private
     real(default), dimension(0:3, 0:3) :: L
  end type lorentz_transformation_t


  type(vector3_t), parameter :: vector3_null = &
       vector3_t ((/ 0._default, 0._default, 0._default /))

  type(vector4_t), parameter :: vector4_null = &
       vector4_t ((/ 0._default, 0._default, 0._default, 0._default /))

  integer, dimension(3,3), parameter :: delta_three = &
       & reshape( source = (/ 1,0,0, 0,1,0, 0,0,1 /), &
       &          shape  = (/3,3/) )
  integer, dimension(3,3,3), parameter :: epsilon_three = &
       & reshape( source = (/ 0, 0,0,  0,0,-1,   0,1,0,&
       &                      0, 0,1,  0,0, 0,  -1,0,0,&
       &                      0,-1,0,  1,0, 0,   0,0,0 /),&
       &          shape = (/3,3,3/) )
  type(lorentz_transformation_t), parameter :: &
       & identity = &
       & lorentz_transformation_t ( &
       & reshape( source = (/ 1._default, 0._default, 0._default, 0._default, &
       &                      0._default, 1._default, 0._default, 0._default, &
       &                      0._default, 0._default, 1._default, 0._default, &
       &                      0._default, 0._default, 0._default, 1._default /),&
       &          shape = (/ 4,4 /) ) )
  type(lorentz_transformation_t), parameter :: &
       & space_reflection = &
       & lorentz_transformation_t ( &
       & reshape( source = (/ 1._default, 0._default, 0._default, 0._default, &
       &                      0._default,-1._default, 0._default, 0._default, &
       &                      0._default, 0._default,-1._default, 0._default, &
       &                      0._default, 0._default, 0._default,-1._default /),&
       &          shape = (/ 4,4 /) ) )

  interface vector3_moving
     module procedure vector3_moving_canonical
     module procedure vector3_moving_generic
  end interface
  interface operator(==)
     module procedure vector3_eq
  end interface
  interface operator(/=)
     module procedure vector3_neq
  end interface
  interface operator(+)
     module procedure add_vector3
  end interface
  interface operator(-)
     module procedure sub_vector3
  end interface
  interface operator(*)
     module procedure prod_integer_vector3, prod_vector3_integer
     module procedure prod_real_vector3, prod_vector3_real
  end interface
  interface operator(/)
     module procedure div_vector3_real, div_vector3_integer
  end interface
  interface operator(*)
     module procedure prod_vector3
  end interface
  interface cross_product
     module procedure vector3_cross_product
  end interface
  interface operator(**)
     module procedure power_vector3
  end interface
  interface operator(-)
     module procedure negate_vector3
  end interface
  interface sum
     module procedure sum_vector3
  end interface
  interface direction
     module procedure vector3_get_direction
  end interface
  interface vector4_moving
     module procedure vector4_moving_canonical
     module procedure vector4_moving_generic
  end interface
  interface operator(==)
     module procedure vector4_eq
  end interface
  interface operator(/=)
     module procedure vector4_neq
  end interface
  interface operator(+)
     module procedure add_vector4
  end interface
  interface operator(-)
     module procedure sub_vector4
  end interface
  interface operator(*)
     module procedure prod_real_vector4, prod_vector4_real
     module procedure prod_integer_vector4, prod_vector4_integer
  end interface
  interface operator(/)
     module procedure div_vector4_real
     module procedure div_vector4_integer
  end interface
  interface operator(*)
     module procedure prod_vector4
  end interface
  interface operator(**)
     module procedure power_vector4
  end interface
  interface operator(-)
     module procedure negate_vector4
  end interface
  interface sum
     module procedure sum_vector4
  end interface
  interface space_part
     module procedure vector4_get_space_part
  end interface
  interface direction
     module procedure vector4_get_direction
  end interface
  interface array_from_vector4
     module procedure array_from_vector4_1
     module procedure array_from_vector4_2
  end interface
  interface azimuthal_angle
     module procedure vector3_azimuthal_angle
     module procedure vector4_azimuthal_angle
  end interface
  interface azimuthal_angle_deg
     module procedure vector3_azimuthal_angle_deg
     module procedure vector4_azimuthal_angle_deg
  end interface
  interface azimuthal_distance
     module procedure vector3_azimuthal_distance
     module procedure vector4_azimuthal_distance
  end interface
  interface azimuthal_distance_deg
     module procedure vector3_azimuthal_distance_deg
     module procedure vector4_azimuthal_distance_deg
  end interface
  interface polar_angle
     module procedure polar_angle_vector3
     module procedure polar_angle_vector4
  end interface
  interface polar_angle_ct
     module procedure polar_angle_ct_vector3
     module procedure polar_angle_ct_vector4
  end interface
  interface polar_angle_deg
     module procedure polar_angle_deg_vector3
     module procedure polar_angle_deg_vector4
  end interface
  interface enclosed_angle
     module procedure enclosed_angle_vector3
     module procedure enclosed_angle_vector4
  end interface
  interface enclosed_angle_ct
     module procedure enclosed_angle_ct_vector3
     module procedure enclosed_angle_ct_vector4
  end interface
  interface enclosed_angle_deg
     module procedure enclosed_angle_deg_vector3
     module procedure enclosed_angle_deg_vector4
  end interface
  interface enclosed_angle_rest_frame
     module procedure enclosed_angle_rest_frame_vector4
  end interface
  interface enclosed_angle_ct_rest_frame
     module procedure enclosed_angle_ct_rest_frame_vector4
  end interface
  interface enclosed_angle_deg_rest_frame
     module procedure enclosed_angle_deg_rest_frame_vector4
  end interface
  interface transverse_part
     module procedure transverse_part_vector4
  end interface
  interface longitudinal_part
     module procedure longitudinal_part_vector4
  end interface
  interface space_part_norm
     module procedure space_part_norm_vector4
  end interface
  interface energy
     module procedure energy_vector4
     module procedure energy_vector3
     module procedure energy_real
  end interface
  interface invariant_mass
     module procedure invariant_mass_vector4
  end interface
  interface invariant_mass_squared
     module procedure invariant_mass_squared_vector4
  end interface
  interface transverse_mass
     module procedure transverse_mass_vector4
  end interface
  interface rapidity
     module procedure rapidity_vector4
  end interface
  interface pseudorapidity
     module procedure pseudorapidity_vector4
  end interface
  interface rapidity_distance
     module procedure rapidity_distance_vector4
  end interface
  interface pseudorapidity_distance
     module procedure pseudorapidity_distance_vector4
  end interface
  interface eta_phi_distance
     module procedure eta_phi_distance_vector4
  end interface
  interface inverse
     module procedure lorentz_transformation_inverse
  end interface
  interface boost
     module procedure boost_from_rest_frame
     module procedure boost_from_rest_frame_vector3
     module procedure boost_generic
     module procedure boost_canonical
  end interface
  interface rotation
     module procedure rotation_generic
     module procedure rotation_canonical
     module procedure rotation_generic_cs
     module procedure rotation_canonical_cs
  end interface
  interface rotation_to_2nd
     module procedure rotation_to_2nd_generic
     module procedure rotation_to_2nd_canonical
  end interface
  interface transformation
     module procedure transformation_rec_generic
     module procedure transformation_rec_canonical
  end interface
  interface operator(*)
     module procedure prod_LT_vector4
     module procedure prod_LT_LT
     module procedure prod_vector4_LT
  end interface

contains

  subroutine vector3_write (p, unit)
    type(vector3_t), intent(in) :: p
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write(u, *) 'P = ', p%p
  end subroutine vector3_write

  elemental function vector3_canonical (k) result (p)
    type(vector3_t) :: p
    integer, intent(in) :: k
    p = vector3_null
    p%p(k) = 1
  end function vector3_canonical

  elemental function vector3_moving_canonical (p, k) result(q)
    type(vector3_t) :: q
    real(default), intent(in) :: p
    integer, intent(in) :: k
    q = vector3_null
    q%p(k) = p
  end function vector3_moving_canonical
  pure function vector3_moving_generic (p) result(q)
    real(default), dimension(3), intent(in) :: p
    type(vector3_t) :: q
    q%p = p
  end function vector3_moving_generic

  elemental function vector3_eq (p, q) result (r)
    logical :: r
    type(vector3_t), intent(in) :: p,q
    r = all (p%p == q%p)
  end function vector3_eq
  elemental function vector3_neq (p, q) result (r)
    logical :: r
    type(vector3_t), intent(in) :: p,q
    r = any (p%p /= q%p)
  end function vector3_neq

  elemental function add_vector3 (p, q) result (r)
    type(vector3_t) :: r
    type(vector3_t), intent(in) :: p,q
    r%p = p%p + q%p
  end function add_vector3
  elemental function sub_vector3 (p, q) result (r)
    type(vector3_t) :: r
    type(vector3_t), intent(in) :: p,q
    r%p = p%p - q%p
  end function sub_vector3

  elemental function prod_real_vector3 (s, p) result (q)
    type(vector3_t) :: q 
    real(default), intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = s * p%p
  end function prod_real_vector3
  elemental function prod_vector3_real (p, s) result (q)
    type(vector3_t) :: q
    real(default), intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = s * p%p
  end function prod_vector3_real
  elemental function div_vector3_real (p, s) result (q)
    type(vector3_t) :: q
    real(default), intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = p%p/s
  end function div_vector3_real
  elemental function prod_integer_vector3 (s, p) result (q)
    type(vector3_t) :: q
    integer, intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = s * p%p
  end function prod_integer_vector3
  elemental function prod_vector3_integer (p, s) result (q)
    type(vector3_t) :: q
    integer, intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = s * p%p
  end function prod_vector3_integer
  elemental function div_vector3_integer (p, s) result (q)
    type(vector3_t) :: q
    integer, intent(in) :: s
    type(vector3_t), intent(in) :: p
    q%p = p%p/s
  end function div_vector3_integer

  elemental function prod_vector3 (p, q) result (s)
    real(default) :: s
    type(vector3_t), intent(in) :: p,q
    s = dot_product (p%p, q%p)
  end function prod_vector3

  elemental function vector3_cross_product (p, q) result (r)
    type(vector3_t) :: r
    type(vector3_t), intent(in) :: p,q
    integer :: i
    do i=1,3
       r%p(i) = dot_product (p%p, matmul(epsilon_three(i,:,:), q%p))
    end do
  end function vector3_cross_product

  elemental function power_vector3 (p, e) result (s)
    real(default) :: s
    type(vector3_t), intent(in) :: p
    integer, intent(in) :: e
    s = dot_product (p%p, p%p)
    if (e/=2) then
       if (mod(e,2)==0) then
          s = s**(e/2)
       else
          s = sqrt(s)**e
       end if
    end if
  end function power_vector3

  elemental function negate_vector3 (p) result (q)
    type(vector3_t) :: q
    type(vector3_t), intent(in) :: p
    q%p = -p%p
  end function negate_vector3

  pure function sum_vector3 (p) result (q)
    type(vector3_t) :: q
    type(vector3_t), dimension(:), intent(in) :: p
    integer :: i
    do i=1, 3
       q%p(i) = sum (p%p(i))
    end do
  end function sum_vector3
!   pure function sum_vector3_mask (p, mask) result (q)
!     type(vector3_t) :: q
!     type(vector3_t), dimension(:), intent(in) :: p
!     logical, dimension(:), intent(in) :: mask
!     integer :: i
!     do i=1, 3
!        q%p(i) = sum (p%p(i), mask=mask)
!     end do
!   end function sum_vector3_mask

  elemental function vector3_get_component (p, k) result (c)
    type(vector3_t), intent(in) :: p
    integer, intent(in) :: k
    real(default) :: c
    c = p%p(k)
  end function vector3_get_component

  pure function vector3_get_components (p) result (a)
    type(vector3_t), intent(in) :: p
    real(default), dimension(3) :: a
    a = p%p
  end function vector3_get_components

  elemental function vector3_get_direction (p) result (q)
    type(vector3_t) :: q
    type(vector3_t), intent(in) :: p
    q%p = p%p / p**1
  end function vector3_get_direction

  subroutine vector4_write (p, unit, show_mass)
    type(vector4_t), intent(in) :: p
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_mass
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write(u, *) 'E = ', p%p(0)
    write(u, *) 'P = ', p%p(1:)
    if (present (show_mass)) then
       if (show_mass) &
            write (u, *) 'M = ', p**1
    end if
  end subroutine vector4_write

  subroutine vector4_write_raw (p, u)
    type(vector4_t), intent(in) :: p
    integer, intent(in) :: u
    write (u) p%p
  end subroutine vector4_write_raw

  subroutine vector4_read_raw (p, u, iostat)
    type(vector4_t), intent(out) :: p
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    read (u, iostat=iostat) p%p
  end subroutine vector4_read_raw

  elemental function vector4_canonical (k) result (p)
    type(vector4_t) :: p
    integer, intent(in) :: k
    p = vector4_null
    p%p(k) = 1
  end function vector4_canonical

  elemental function vector4_at_rest (m) result (p)
    type(vector4_t) :: p
    real(default), intent(in) :: m
    p = vector4_t ((/ m, 0._default, 0._default, 0._default /))
  end function vector4_at_rest

  elemental function vector4_moving_canonical (E, p, k) result (q)
    type(vector4_t) :: q
    real(default), intent(in) :: E, p
    integer, intent(in) :: k
    q = vector4_at_rest(E)
    q%p(k) = p
  end function vector4_moving_canonical
  elemental function vector4_moving_generic (E, p) result (q)
    type(vector4_t) :: q
    real(default), intent(in) :: E
    type(vector3_t), intent(in) :: p
    q%p(0) = E
    q%p(1:) = p%p
  end function vector4_moving_generic

  elemental function vector4_eq (p, q) result (r)
    logical :: r
    type(vector4_t), intent(in) :: p,q
    r = all (p%p == q%p)
  end function vector4_eq
  elemental function vector4_neq (p, q) result (r)
    logical :: r
    type(vector4_t), intent(in) :: p,q
    r = any (p%p /= q%p)
  end function vector4_neq

  elemental function add_vector4 (p,q) result (r)
    type(vector4_t) :: r
    type(vector4_t), intent(in) :: p,q
    r%p = p%p + q%p
  end function add_vector4
  elemental function sub_vector4 (p,q) result (r)
    type(vector4_t) :: r
    type(vector4_t), intent(in) :: p,q
    r%p = p%p - q%p
  end function sub_vector4

  elemental function prod_real_vector4 (s, p) result (q)
    type(vector4_t) :: q
    real(default), intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = s * p%p
  end function prod_real_vector4
  elemental function prod_vector4_real (p, s) result (q)
    type(vector4_t) :: q
    real(default), intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = s * p%p
  end function prod_vector4_real
  elemental function div_vector4_real (p, s) result (q)
    type(vector4_t) :: q
    real(default), intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = p%p/s
  end function div_vector4_real
  elemental function prod_integer_vector4 (s, p) result (q)
    type(vector4_t) :: q
    integer, intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = s * p%p
  end function prod_integer_vector4
  elemental function prod_vector4_integer (p, s) result (q)
    type(vector4_t) :: q
    integer, intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = s * p%p
  end function prod_vector4_integer
  elemental function div_vector4_integer (p, s) result (q)
    type(vector4_t) :: q
    integer, intent(in) :: s
    type(vector4_t), intent(in) :: p
    q%p = p%p/s
  end function div_vector4_integer

  elemental function prod_vector4 (p, q) result (s)
    real(default) :: s
    type(vector4_t), intent(in) :: p,q
    s = p%p(0)*q%p(0) - dot_product(p%p(1:), q%p(1:))
  end function prod_vector4

  elemental function power_vector4 (p, e) result (s)
    real(default) :: s
    type(vector4_t), intent(in) :: p
    integer, intent(in) :: e
    s = p*p
    if (e/=2) then
       if (mod(e,2)==0) then
          s = s**(e/2)
       elseif (s>=0) then
          s = sqrt(s)**e
       else
          s = -(sqrt(abs(s))**e)
       end if
    end if
  end function power_vector4

  elemental function negate_vector4 (p) result (q)
    type(vector4_t) :: q
    type(vector4_t), intent(in) :: p
    q%p = -p%p
  end function negate_vector4

  pure function sum_vector4 (p) result (q)
    type(vector4_t) :: q
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: i
    do i=0, 3
       q%p(i) = sum (p%p(i))
    end do
  end function sum_vector4
!   pure function sum_vector4_mask (p, mask) result (q)
!     type(vector4_t) :: q
!     type(vector4_t), dimension(:), intent(in) :: p
!     logical, dimension(:), intent(in) :: mask
!     integer :: i
!     do i=0, 3
!        q%p(i) = sum (p%p(i), mask=mask)
!     end do
!   end function sum_vector4_mask

  subroutine vector4_set_component (p, k, c)
    type(vector4_t), intent(inout) :: p
    integer, intent(in) :: k
    real(default), intent(in) :: c
    p%p(k) = c
  end subroutine vector4_set_component

  elemental function vector4_get_component (p, k) result (c)
    real(default) :: c
    type(vector4_t), intent(in) :: p
    integer, intent(in) :: k
    c = p%p(k)
  end function vector4_get_component

  pure function vector4_get_components (p) result (a)
    real(default), dimension(0:3) :: a
    type(vector4_t), intent(in) :: p
    a = p%p
  end function vector4_get_components

  elemental function vector4_get_space_part (p) result (q)
    type(vector3_t) :: q
    type(vector4_t), intent(in) :: p
    q%p = p%p(1:)
  end function vector4_get_space_part

  elemental function vector4_get_direction (p) result (q)
    type(vector3_t) :: q
    type(vector4_t), intent(in) :: p
    q%p = p%p(1:)
    q = q / q**1
  end function vector4_get_direction

  pure function array_from_vector4_1 (p) result (a)
    type(vector4_t), intent(in) :: p
    real(default), dimension(0:3) :: a
    a = p%p
  end function array_from_vector4_1

  pure function array_from_vector4_2 (p) result (a)
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), dimension(0:3, size(p)) :: a
    integer :: i
    forall (i=1:size(p))
       a(0:3,i) = p(i)%p
    end forall
  end function array_from_vector4_2

  elemental function vector4_from_c_prt (c_prt) result (p)
    type(vector4_t) :: p
    type(c_prt_t), intent(in) :: c_prt
    p%p(0) = c_prt%pe
    p%p(1) = c_prt%px
    p%p(2) = c_prt%py
    p%p(3) = c_prt%pz
  end function vector4_from_c_prt

  elemental function vector4_to_c_prt (p, p2) result (c_prt)
    type(c_prt_t) :: c_prt
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    c_prt%pe = p%p(0)
    c_prt%px = p%p(1)
    c_prt%py = p%p(2)
    c_prt%pz = p%p(3)
    if (present (p2)) then
       c_prt%p2 = p2
    else
       c_prt%p2 = p ** 2
    end if
  end function vector4_to_c_prt

  elemental function vector3_azimuthal_angle (p) result (phi)
    real(default) :: phi
    type(vector3_t), intent(in) :: p
    if (any(p%p(1:2)/=0)) then
       phi = atan2(p%p(2), p%p(1))
       if (phi < 0) phi = phi + twopi
    else
       phi = 0
    end if
  end function vector3_azimuthal_angle
  elemental function vector4_azimuthal_angle (p) result (phi)
    real(default) :: phi
    type(vector4_t), intent(in) :: p
    phi = vector3_azimuthal_angle (space_part (p))
  end function vector4_azimuthal_angle

  elemental function vector3_azimuthal_angle_deg (p) result (phi)
    real(default) :: phi
    type(vector3_t), intent(in) :: p
    phi = vector3_azimuthal_angle (p) / degree
  end function vector3_azimuthal_angle_deg
  elemental function vector4_azimuthal_angle_deg (p) result (phi)
    real(default) :: phi
    type(vector4_t), intent(in) :: p
    phi = vector4_azimuthal_angle (p) / degree
  end function vector4_azimuthal_angle_deg

  elemental function vector3_azimuthal_distance (p, q) result (dphi)
    real(default) :: dphi
    type(vector3_t), intent(in) :: p,q
    dphi = vector3_azimuthal_angle (q) - vector3_azimuthal_angle (p)
    if (dphi <= -pi) then
       dphi = dphi + twopi
    else if (dphi > pi) then
       dphi = dphi - twopi
    end if
  end function vector3_azimuthal_distance
  elemental function vector4_azimuthal_distance (p, q) result (dphi)
    real(default) :: dphi
    type(vector4_t), intent(in) :: p,q
    dphi = vector3_azimuthal_distance &
         (space_part (p), space_part (q))
  end function vector4_azimuthal_distance

  elemental function vector3_azimuthal_distance_deg (p, q) result (dphi)
    real(default) :: dphi
    type(vector3_t), intent(in) :: p,q
    dphi = vector3_azimuthal_distance (p, q) / degree
  end function vector3_azimuthal_distance_deg
  elemental function vector4_azimuthal_distance_deg (p, q) result (dphi)
    real(default) :: dphi
    type(vector4_t), intent(in) :: p,q
    dphi = vector4_azimuthal_distance (p, q) / degree
  end function vector4_azimuthal_distance_deg

  elemental function polar_angle_vector3 (p) result (theta)
    real(default) :: theta
    type(vector3_t), intent(in) :: p
    if (any(p%p/=0)) then
       theta = atan2 (sqrt(p%p(1)**2 + p%p(2)**2), p%p(3))
    else
       theta = 0
    end if
  end function polar_angle_vector3
  elemental function polar_angle_vector4 (p) result (theta)
    real(default) :: theta
    type(vector4_t), intent(in) :: p
    theta = polar_angle (space_part (p))
  end function polar_angle_vector4

  elemental function polar_angle_ct_vector3 (p) result (ct)
    real(default) :: ct
    type(vector3_t), intent(in) :: p
    if (any(p%p/=0)) then
       ct = p%p(3) / p**1
    else
       ct = 1
    end if
  end function polar_angle_ct_vector3
  elemental function polar_angle_ct_vector4 (p) result (ct)
    real(default) :: ct
    type(vector4_t), intent(in) :: p
    ct = polar_angle_ct (space_part (p))
  end function polar_angle_ct_vector4

  elemental function polar_angle_deg_vector3 (p) result (theta)
    real(default) :: theta
    type(vector3_t), intent(in) :: p
    theta = polar_angle (p) / degree
  end function polar_angle_deg_vector3
  elemental function polar_angle_deg_vector4 (p) result (theta)
    real(default) :: theta
    type(vector4_t), intent(in) :: p
    theta = polar_angle (p) / degree
  end function polar_angle_deg_vector4

  elemental function enclosed_angle_vector3 (p, q) result (theta)
    real(default) :: theta
    type(vector3_t), intent(in) :: p, q
    theta = acos (enclosed_angle_ct (p, q))
  end function enclosed_angle_vector3
  elemental function enclosed_angle_vector4 (p, q) result (theta)
    real(default) :: theta
    type(vector4_t), intent(in) :: p, q
    theta = enclosed_angle (space_part (p), space_part (q))
  end function enclosed_angle_vector4

  elemental function enclosed_angle_ct_vector3 (p, q) result (ct)
    real(default) :: ct
    type(vector3_t), intent(in) :: p, q
    if (any(p%p/=0).and.any(q%p/=0)) then
       ct = p*q / (p**1 * q**1)
       if (ct>1) then
          ct = 1
       else if (ct<-1) then
          ct = -1
       end if
    else
       ct = 1
    end if
  end function enclosed_angle_ct_vector3
  elemental function enclosed_angle_ct_vector4 (p, q) result (ct)
    real(default) :: ct
    type(vector4_t), intent(in) :: p, q
    ct = enclosed_angle_ct (space_part (p), space_part (q))
  end function enclosed_angle_ct_vector4

  elemental function enclosed_angle_deg_vector3 (p, q) result (theta)
    real(default) :: theta
    type(vector3_t), intent(in) :: p, q
    theta = enclosed_angle (p, q) / degree
  end function enclosed_angle_deg_vector3
  elemental function enclosed_angle_deg_vector4 (p, q) result (theta)
    real(default) :: theta
    type(vector4_t), intent(in) :: p, q
    theta = enclosed_angle (p, q) / degree
  end function enclosed_angle_deg_vector4

  elemental function enclosed_angle_rest_frame_vector4 (p, q) result (theta)
    type(vector4_t), intent(in) :: p, q
    real(default) :: theta
    theta = acos (enclosed_angle_ct_rest_frame (p, q))
  end function enclosed_angle_rest_frame_vector4
  elemental function enclosed_angle_ct_rest_frame_vector4 (p, q) result (ct)
    type(vector4_t), intent(in) :: p, q
    real(default) :: ct
    if (invariant_mass(q) > 0) then
       ct = enclosed_angle_ct ( &
            space_part (boost(-q, invariant_mass (q)) * p), &
            space_part (q))
    else
       ct = 1
    end if
  end function enclosed_angle_ct_rest_frame_vector4
  elemental function enclosed_angle_deg_rest_frame_vector4 (p, q) &
       result (theta)
    type(vector4_t), intent(in) :: p, q
    real(default) :: theta
    theta = enclosed_angle_rest_frame (p, q) / degree
  end function enclosed_angle_deg_rest_frame_vector4

  elemental function transverse_part_vector4 (p) result (pT)
    real(default) :: pT
    type(vector4_t), intent(in) :: p
    pT = sqrt(p%p(1)**2 + p%p(2)**2)
  end function transverse_part_vector4

  elemental function longitudinal_part_vector4 (p) result (pL)
    real(default) :: pL
    type(vector4_t), intent(in) :: p
    pL = p%p(3)
  end function longitudinal_part_vector4

  elemental function space_part_norm_vector4 (p) result (p3)
    real(default) :: p3
    type(vector4_t), intent(in) :: p
    p3 = sqrt (p%p(1)**2 + p%p(2)**2 + p%p(3)**2)
  end function space_part_norm_vector4

  elemental function energy_vector4 (p) result (E)
    real(default) :: E
    type(vector4_t), intent(in) :: p
    E = p%p(0)
  end function energy_vector4

  elemental function energy_vector3 (p, mass) result (E)
    real(default) :: E
    type(vector3_t), intent(in) :: p
    real(default), intent(in), optional :: mass
    if (present (mass)) then
       E = sqrt (p**2 + mass**2)
    else
       E = p**1
    end if
  end function energy_vector3

  elemental function energy_real (p, mass) result (E)
    real(default) :: E
    real(default), intent(in) :: p
    real(default), intent(in), optional :: mass
    if (present (mass)) then
       E = sqrt (p**2 + mass**2)
    else
       E = abs (p)
    end if
  end function energy_real

  elemental function invariant_mass_vector4 (p) result (m)
    real(default) :: m
    type(vector4_t), intent(in) :: p
    real(default) :: msq
    msq = p*p
    if (msq >= 0) then
       m = sqrt (msq)
    else
       m = - sqrt (abs (msq))
    end if
  end function invariant_mass_vector4
  elemental function invariant_mass_squared_vector4 (p) result (msq)
    real(default) :: msq
    type(vector4_t), intent(in) :: p
    msq = p*p
  end function invariant_mass_squared_vector4

  elemental function transverse_mass_vector4 (p) result (m)
    real(default) :: m
    type(vector4_t), intent(in) :: p
    real(default) :: msq
    msq = p%p(0)**2 - p%p(1)**2 - p%p(2)**2
    if (msq >= 0) then
       m = sqrt (msq)
    else
       m = - sqrt (abs (msq))
    end if
  end function transverse_mass_vector4

  elemental function rapidity_vector4 (p) result (y)
    real(default) :: y
    type(vector4_t), intent(in) :: p
    y = .5 * log( (energy (p) + longitudinal_part (p)) &
         &       /(energy (p) - longitudinal_part (p)))
  end function rapidity_vector4

  elemental function pseudorapidity_vector4 (p) result (eta)
    real(default) :: eta
    type(vector4_t), intent(in) :: p
    eta = -log( tan (.5 * polar_angle (p)))
  end function pseudorapidity_vector4

  elemental function rapidity_distance_vector4 (p, q) result (dy)
    type(vector4_t), intent(in) :: p, q
    real(default) :: dy
    dy = rapidity (q) - rapidity (p)
  end function rapidity_distance_vector4

  elemental function pseudorapidity_distance_vector4 (p, q) result (deta)
    real(default) :: deta
    type(vector4_t), intent(in) :: p, q
    deta = pseudorapidity (q) - pseudorapidity (p)
  end function pseudorapidity_distance_vector4

  elemental function eta_phi_distance_vector4 (p, q) result (dr)
    type(vector4_t), intent(in) :: p, q
    real(default) :: dr
    dr = sqrt ( &
         pseudorapidity_distance (p, q)**2 &
         + azimuthal_distance (p, q)**2)
  end function eta_phi_distance_vector4

  subroutine lorentz_transformation_write (L, unit)
    type(lorentz_transformation_t), intent(in) :: L
    integer, intent(in), optional :: unit
    integer :: u
    integer :: i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) 'Lorentz transformation:'
    write (u, *) 'L00:'
    write (u, *) L%L(0,0)
    write (u, *) 'L0j:', L%L(0,1:)
    write (u, *) 'Li0, Lij:'
    do i = 1, 3
       write (u, *) L%L(i,0)
       write (u, *) '    ', L%L(i,1:)
    end do
  end subroutine lorentz_transformation_write

  pure function lorentz_transformation_get_components (L) result (a)
    type(lorentz_transformation_t), intent(in) :: L
    real(default), dimension(0:3,0:3) :: a
    a = L%L
  end function lorentz_transformation_get_components

  elemental function lorentz_transformation_inverse (L) result (IL)
    type(lorentz_transformation_t) :: IL
    type(lorentz_transformation_t), intent(in) :: L
    IL%L(0,0) = L%L(0,0)
    IL%L(0,1:) = -L%L(1:,0)
    IL%L(1:,0) = -L%L(0,1:)
    IL%L(1:,1:) = transpose(L%L(1:,1:))
  end function lorentz_transformation_inverse

  elemental function boost_from_rest_frame (p, m) result (L)
    type(lorentz_transformation_t) :: L
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: m
    L = boost_from_rest_frame_vector3 (space_part (p), m)
  end function boost_from_rest_frame
  elemental function boost_from_rest_frame_vector3 (p, m) result (L)
    type(lorentz_transformation_t) :: L
    type(vector3_t), intent(in) :: p
    real(default), intent(in) :: m
    type(vector3_t) :: beta_gamma
    real(default) :: bg2, g, c
    integer :: i,j
    if (m /= 0) then
       beta_gamma = p / m
       bg2 = beta_gamma**2
    else
       bg2 = 0
    end if
    if (bg2 /= 0) then
       g = sqrt(1 + bg2);  c = (g-1)/bg2
       L%L(0,0)  = g
       L%L(0,1:) = beta_gamma%p
       L%L(1:,0) = L%L(0,1:)
       do i=1,3
          do j=1,3
             L%L(i,j) = delta_three(i,j) + c*beta_gamma%p(i)*beta_gamma%p(j)
          end do
       end do
    else
       L = identity
    end if
  end function boost_from_rest_frame_vector3
  elemental function boost_canonical (beta_gamma, k) result (L)
    type(lorentz_transformation_t) :: L
    real(default), intent(in) :: beta_gamma
    integer, intent(in) :: k
    real(default) :: g
    g = sqrt(1 + beta_gamma**2)
    L = identity
    L%L(0,0) = g
    L%L(0,k) = beta_gamma
    L%L(k,0) = L%L(0,k)
    L%L(k,k) = L%L(0,0)
  end function boost_canonical
  elemental function boost_generic (beta_gamma, axis) result (L)
    type(lorentz_transformation_t) :: L
    real(default), intent(in) :: beta_gamma
    type(vector3_t), intent(in) :: axis
    if (any(axis%p/=0)) then
       L = boost_from_rest_frame_vector3 (beta_gamma * axis, axis**1)
    else
       L = identity
    end if
  end function boost_generic

  elemental function rotation_generic_cs (cp, sp, axis) result (R)
    type(lorentz_transformation_t) :: R
    real(default), intent(in) :: cp, sp
    type(vector3_t), intent(in) :: axis
    integer :: i,j
    R = identity
    do i=1,3
       do j=1,3
          R%L(i,j) = cp*delta_three(i,j) + (1-cp)*axis%p(i)*axis%p(j)  &
               &   - sp*dot_product(epsilon_three(i,j,:), axis%p)
       end do
    end do
  end function rotation_generic_cs
  elemental function rotation_generic (axis) result (R)
    type(lorentz_transformation_t) :: R
    type(vector3_t), intent(in) :: axis
    real(default) :: phi
    if (any(axis%p/=0)) then
       phi = abs(axis**1)
       R = rotation_generic_cs (cos(phi), sin(phi), axis/phi)
    else
       R = identity
    end if
  end function rotation_generic
  elemental function rotation_canonical_cs (cp, sp, k) result (R)
    type(lorentz_transformation_t) :: R
    real(default), intent(in) :: cp, sp
    integer, intent(in) :: k
    integer :: i,j
    R = identity
    do i=1,3
       do j=1,3
          R%L(i,j) = -sp*epsilon_three(i,j,k)
       end do
       R%L(i,i) = cp
    end do
    R%L(k,k) = 1
  end function rotation_canonical_cs
  elemental function rotation_canonical (phi, k) result (R)
    type(lorentz_transformation_t) :: R
    real(default), intent(in) :: phi
    integer, intent(in) :: k
    R = rotation_canonical_cs(cos(phi), sin(phi), k)
  end function rotation_canonical
  elemental function rotation_to_2nd_generic (p, q) result (R)
    type(lorentz_transformation_t) :: R
    type(vector3_t), intent(in) :: p, q
    type(vector3_t) :: a, b, ab
    real(default) :: ct, st
    if (any (p%p /= 0) .and. any (q%p /= 0)) then
       a = direction (p)
       b = direction (q)
       ab = cross_product(a,b)
       ct = a*b;  st = ab**1
       if (st /= 0) then
          R = rotation_generic_cs (ct, st, ab/st)
       else if (ct < 0) then
          R = space_reflection
       else
          R = identity
       end if
    else
       R = identity
    end if
  end function rotation_to_2nd_generic
  elemental function rotation_to_2nd_canonical (k, p) result (R)
    type(lorentz_transformation_t) :: R
    integer, intent(in) :: k
    type(vector3_t), intent(in) :: p
    type(vector3_t) :: b, ab
    real(default) :: ct, st
    integer :: i, j
    if (any (p%p /= 0)) then
       b = direction (p)
       ab%p = 0
       do i = 1, 3
          do j = 1, 3
             ab%p(j) = ab%p(j) + b%p(i) * epsilon_three(i,j,k)
          end do
       end do
       ct = b%p(k);  st = ab**1
       if (st /= 0) then
          R = rotation_generic_cs (ct, st, ab/st)
       else if (ct < 0) then
          R = space_reflection
       else
          R = identity
       end if
    else
       R = identity
    end if
  end function rotation_to_2nd_canonical

  elemental function transformation_rec_generic (axis, p1, p2, m) result (L)
    type(vector3_t), intent(in) :: axis
    type(vector4_t), intent(in) :: p1, p2
    real(default), intent(in) :: m
    type(lorentz_transformation_t) :: L
    L = boost (p1 + p2, m)
    L = L * rotation_to_2nd (axis, space_part (inverse (L) * p1))
  end function transformation_rec_generic
  elemental function transformation_rec_canonical (k, p1, p2, m) result (L)
    integer, intent(in) :: k
    type(vector4_t), intent(in) :: p1, p2
    real(default), intent(in) :: m
    type(lorentz_transformation_t) :: L
    L = boost (p1 + p2, m)
    L = L * rotation_to_2nd (k, space_part (inverse (L) * p1))
  end function transformation_rec_canonical
  elemental function prod_LT_vector4 (L, p) result (np)
    type(vector4_t) :: np
    type(lorentz_transformation_t), intent(in) :: L
    type(vector4_t), intent(in) :: p
    np%p = matmul (L%L, p%p)
  end function prod_LT_vector4
  elemental function prod_LT_LT (L1, L2) result (NL)
    type(lorentz_transformation_t) :: NL
    type(lorentz_transformation_t), intent(in) :: L1,L2
    NL%L = matmul (L1%L, L2%L)
  end function prod_LT_LT
  elemental function prod_vector4_LT (p, L) result (np)
    type(vector4_t) :: np
    type(vector4_t), intent(in) :: p
    type(lorentz_transformation_t), intent(in) :: L
    np%p = matmul (p%p, L%L)
  end function prod_vector4_LT

  elemental function LT_compose_r3_r2_b3 &
       (cp, sp, ct, st, beta_gamma) result (L)
    type(lorentz_transformation_t) :: L
    real(default), intent(in) :: cp, sp, ct, st, beta_gamma
    real(default) :: gamma
    if (beta_gamma==0) then
       L%L(0,0)  = 1
       L%L(1:,0) = 0
       L%L(0,1:) = 0
       L%L(1,1:) = (/  ct*cp, -ct*sp, st /)
       L%L(2,1:) = (/     sp,     cp,  0._default /)
       L%L(3,1:) = (/ -st*cp,  st*sp, ct /)
    else
       gamma = sqrt(1 + beta_gamma**2)
       L%L(0,0)  = gamma
       L%L(1,0)  = 0
       L%L(2,0)  = 0
       L%L(3,0)  = beta_gamma
       L%L(0,1:) = beta_gamma * (/ -st*cp,  st*sp, ct /)
       L%L(1,1:) =              (/  ct*cp, -ct*sp, st /)
       L%L(2,1:) =              (/     sp,     cp, 0._default /)
       L%L(3,1:) = gamma      * (/ -st*cp,  st*sp, ct /)
    end if
  end function LT_compose_r3_r2_b3

  elemental function LT_compose_r2_r3_b3 &
       (ct, st, cp, sp, beta_gamma) result (L)
    type(lorentz_transformation_t) :: L
    real(default), intent(in) :: ct, st, cp, sp, beta_gamma
    real(default) :: gamma
    if (beta_gamma==0) then
       L%L(0,0)  = 1
       L%L(1:,0) = 0
       L%L(0,1:) = 0
       L%L(1,1:) = (/  ct*cp,    -sp,     st*cp /)
       L%L(2,1:) = (/  ct*sp,     cp,     st*sp /)
       L%L(3,1:) = (/ -st   , 0._default, ct    /)
    else
       gamma = sqrt(1 + beta_gamma**2)
       L%L(0,0)  = gamma
       L%L(1,0)  = 0
       L%L(2,0)  = 0
       L%L(3,0)  = beta_gamma
       L%L(0,1:) = beta_gamma * (/ -st   , 0._default, ct    /)
       L%L(1,1:) =              (/  ct*cp,    -sp,     st*cp /)
       L%L(2,1:) =              (/  ct*sp,     cp,     st*sp /)
       L%L(3,1:) = gamma      * (/ -st   , 0._default, ct    /)
    end if
  end function LT_compose_r2_r3_b3

  elemental function axis_from_p_r3_r2_b3 &
       (p, cp, sp, ct, st, beta_gamma) result (n)
    type(vector3_t) :: n
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: cp, sp, ct, st, beta_gamma
    real(default) :: gamma, px, py
    px = cp * p%p(1) - sp * p%p(2)
    py = sp * p%p(1) + cp * p%p(2)
    n%p(1) =  ct * px + st * p%p(3)
    n%p(2) = py
    n%p(3) = -st * px + ct * p%p(3) 
    if (beta_gamma/=0) then
       gamma = sqrt(1 + beta_gamma**2)
       n%p(3) = n%p(3) * gamma + p%p(0) * beta_gamma
    end if
  end function axis_from_p_r3_r2_b3

  elemental function axis_from_p_b3 (p, beta_gamma) result (n)
    type(vector3_t) :: n
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: beta_gamma
    real(default) :: gamma
    n%p = p%p(1:3)
    if (beta_gamma/=0) then
       gamma = sqrt(1 + beta_gamma**2)
       n%p(3) = n%p(3) * gamma + p%p(0) * beta_gamma
    end if
  end function axis_from_p_b3

  elemental function lambda (m1sq, m2sq, m3sq)
    real(default) :: lambda
    real(default), intent(in) :: m1sq, m2sq, m3sq
    lambda = (m1sq - m2sq - m3sq)**2 - 4*m2sq*m3sq
  end function lambda

  function colliding_momenta (sqrts, m, p_cm) result (p)
    type(vector4_t), dimension(2) :: p
    real(default), intent(in) :: sqrts
    real(default), dimension(2), intent(in), optional :: m
    real(default), intent(in), optional :: p_cm
    real(default), dimension(2) :: dmsq
    real(default) :: ch, sh
    real(default), dimension(2) :: E0, p0
    integer, dimension(2), parameter :: sgn = (/1, -1/)
    if (sqrts == 0) then
       call msg_fatal (" Colliding beams: sqrts is zero (please set sqrts)")
       p = vector4_null;  return
    else if (sqrts <= 0) then
       call msg_fatal (" Colliding beams: sqrts is negative")
       p = vector4_null;  return
    end if
    if (present (m)) then
       dmsq = sgn * (m(1)**2-m(2)**2)
       E0 = (sqrts + dmsq/sqrts) / 2
       if (any (E0 < m)) then
          call msg_fatal &
               (" Colliding beams: beam energy is less than particle mass")
          p = vector4_null;  return
       end if
       p0 = sgn * sqrt (E0**2 - m**2)
    else
       E0 = sqrts / 2
       p0 = sgn * E0
    end if
    if (present (p_cm)) then
       sh = p_cm / sqrts
       ch = sqrt (1 + sh**2)
       p = vector4_moving (E0 * ch + p0 * sh, E0 * sh + p0 * ch, 3)
    else
       p = vector4_moving (E0, p0, 3)
    end if
  end function colliding_momenta

end module lorentz
