! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  use kinds, only: default
  use io_units
  use constants, only: twopi
  use unit_tests

  use lorentz

  implicit none
  private

  public :: splitting_data_t
  public :: on_shell
  public :: sf_aux_test

  integer, parameter, public :: KEEP_ENERGY = 0, KEEP_MOMENTUM = 1

  type :: splitting_data_t
     private
     logical :: collinear = .false.
     real(default) :: x0 = 0
     real(default) :: x1
     real(default) :: t0
     real(default) :: t1
     real(default) :: phi0 = 0
     real(default) :: phi1 = twopi
     real(default) :: E, p, s, u, m2
     real(default) :: x, xb, pb
     real(default) :: t = 0
     real(default) :: phi = 0
   contains
     procedure :: write => splitting_data_write
     procedure :: init => splitting_data_init
     procedure :: get_x_bounds => splitting_get_x_bounds
     procedure :: set_t_bounds => splitting_set_t_bounds
     procedure :: sample_t => splitting_sample_t
     procedure :: inverse_t => splitting_inverse_t
     procedure :: sample_phi => splitting_sample_phi
     procedure :: inverse_phi => splitting_inverse_phi
     procedure :: split_momentum => splitting_split_momentum
     procedure :: recover => splitting_recover
     procedure :: get_x => splitting_get_x
  end type splitting_data_t


contains

  subroutine splitting_data_write (d, unit)
    class(splitting_data_t), intent(in) :: d
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)") "Splitting data:"
    write (u, "(2x,A,L1)")  "collinear = ", d%collinear
1   format (2x,A,1x,ES15.8)
    write (u, 1) "x0   =", d%x0
    write (u, 1) "x    =", d%x
    write (u, 1) "xb   =", d%xb
    write (u, 1) "x1   =", d%x1
    write (u, 1) "t0   =", d%t0
    write (u, 1) "t    =", d%t
    write (u, 1) "t1   =", d%t1
    write (u, 1) "phi0 =", d%phi0
    write (u, 1) "phi  =", d%phi
    write (u, 1) "phi1 =", d%phi1
    write (u, 1) "E    =", d%E
    write (u, 1) "p    =", d%p
    write (u, 1) "pb   =", d%pb
    write (u, 1) "s    =", d%s
    write (u, 1) "u    =", d%u
    write (u, 1) "m2   =", d%m2
  end subroutine splitting_data_write

  subroutine splitting_data_init (d, k, mk2, mr2, mo2, collinear)
    class(splitting_data_t), intent(out) :: d
    type(vector4_t), intent(in) :: k
    real(default), intent(in) :: mk2, mr2, mo2
    logical, intent(in), optional :: collinear
    if (present (collinear))  d%collinear = collinear
    d%E = energy (k)
    d%x1 = 1 - sqrt (max (mr2, 0._default)) / d%E
    d%p = sqrt (d%E**2 - mk2)
    d%s = mk2
    d%u = mr2
    d%m2 = mo2
  end subroutine splitting_data_init

  function splitting_get_x_bounds (d) result (x)
    class(splitting_data_t), intent(in) :: d
    real(default), dimension(2) :: x
    x = [ d%x0, d%x1 ]
  end function splitting_get_x_bounds

  elemental subroutine splitting_set_t_bounds (d, x, xb)
    class(splitting_data_t), intent(inout) :: d
    real(default), intent(in), optional :: x, xb
    real(default) :: tp, tm
    if (present (x))  d%x = x
    if (present (xb)) d%xb = xb
    if (d%xb /= 0) then
       d%pb = sqrt (max (d%E**2 - d%u / d%xb**2, 0._default))
    else
       d%pb = 0
    end if
    tp = -2 * d%xb * d%E**2 + d%s + d%u
    tm = -2 * d%xb * d%p * d%pb
    d%t0 = tp + tm
    d%t1 = tp - tm
    d%t = d%t1
  end subroutine splitting_set_t_bounds

  subroutine splitting_sample_t (d, r, t0, t1)
    class(splitting_data_t), intent(inout) :: d
    real(default), intent(in) :: r
    real(default), intent(in), optional :: t0, t1
    real(default) :: tt0, tt1, tt0m, tt1m
    if (d%collinear) then
       d%t = d%t1
    else
       tt0 = d%t0;  if (present (t0))  tt0 = max (t0, tt0)
       tt1 = d%t1;  if (present (t1))  tt1 = min (t1, tt1)
       tt0m = tt0 - d%m2
       tt1m = tt1 - d%m2
       if (tt0m < 0 .and. tt1m < 0 .and. abs(tt0m) > &
            epsilon(tt0m) .and. abs(tt1m) > epsilon(tt0m)) then
          d%t = d%m2 + tt0m * exp (r * log (tt1m / tt0m))
       else
          d%t = tt1
       end if
    end if
  end subroutine splitting_sample_t

  subroutine splitting_inverse_t (d, r, t0, t1)
    class(splitting_data_t), intent(in) :: d
    real(default), intent(out) :: r
    real(default), intent(in), optional :: t0, t1
    real(default) :: tt0, tt1, tt0m, tt1m
    if (d%collinear) then
       r = 0
    else
       tt0 = d%t0;  if (present (t0))  tt0 = max (t0, tt0)
       tt1 = d%t1;  if (present (t1))  tt1 = min (t1, tt1)
       tt0m = tt0 - d%m2
       tt1m = tt1 - d%m2
       if (tt0m < 0 .and. tt1m < 0) then
          r = log ((d%t - d%m2) / tt0m) / log (tt1m / tt0m)
       else
          r = 0
       end if
    end if
  end subroutine splitting_inverse_t
    
  subroutine splitting_sample_phi (d, r)
    class(splitting_data_t), intent(inout) :: d
    real(default), intent(in) :: r
    if (d%collinear) then
       d%phi = 0
    else
       d%phi = (1-r) * d%phi0 + r * d%phi1
    end if
  end subroutine splitting_sample_phi

  subroutine splitting_inverse_phi (d, r)
    class(splitting_data_t), intent(in) :: d
    real(default), intent(out) :: r
    if (d%collinear) then
       r = 0
    else
       r = (d%phi - d%phi0) / (d%phi1 - d%phi0)
    end if
  end subroutine splitting_inverse_phi

  function splitting_split_momentum (d, k) result (q)
    class(splitting_data_t), intent(in) :: d
    type(vector4_t), dimension(2) :: q
    type(vector4_t), intent(in) :: k
    real(default) :: st2, ct2, st, ct, cp, sp
    type(lorentz_transformation_t) :: rot
    real(default) :: tt0, tt1, den
    type(vector3_t) :: kk, q1, q2
    if (d%collinear) then
       if (d%s == 0 .and. d%u == 0) then
          q(1) = d%xb * k
          q(2) = d%x * k
       else
          kk = space_part (k)
          q1 = d%xb * (d%pb / d%p) * kk
          q2 = kk - q1
          q(1) = vector4_moving (d%xb * d%E, q1)
          q(2) = vector4_moving (d%x * d%E, q2)
       end if
    else       
       den = 2 * d%xb * d%p * d%pb       
       tt0 = max (d%t - d%t0, 0._default)
       tt1 = min (d%t - d%t1, 0._default)
       if (den**2 <= epsilon(den)) then
          st2 = 1
       else
          st2 = - (tt0 * tt1) / den ** 2
       end if
       if (st2 > 1) then 
          st2 = 1
       end if
       ct2 = 1 - st2
       st = sqrt (max (st2, 0._default))
       ct = sqrt (max (ct2, 0._default))
       sp = sin (d%phi)
       cp = cos (d%phi)
       rot = rotation_to_2nd (3, space_part (k))
       q1 = vector3_moving (d%xb * d%pb * [st * cp, st * sp, ct])
       q2 = vector3_moving (d%p, 3) - q1
       q(1) = rot * vector4_moving (d%xb * d%E, q1)
       q(2) = rot * vector4_moving (d%x * d%E, q2)
    end if
  end function splitting_split_momentum
    
  elemental subroutine on_shell (p, m2, keep)
    type(vector4_t), intent(inout) :: p
    real(default), intent(in) :: m2
    integer, intent(in) :: keep
    real(default) :: E, E2, pn
    select case (keep)
    case (KEEP_ENERGY)
       E = energy (p)
       E2 = E ** 2
       if (E2 >= m2) then
          pn = sqrt (E2 - m2)
          p = vector4_moving (E, pn * direction (space_part (p)))
       else
          p = vector4_null
       end if
    case (KEEP_MOMENTUM)
       E = sqrt (space_part (p) ** 2 + m2)
       p = vector4_moving (E, space_part (p))
    end select
  end subroutine on_shell

  subroutine splitting_recover (d, k, q, keep)
    class(splitting_data_t), intent(inout) :: d
    type(vector4_t), intent(in) :: k, q
    integer, intent(in) :: keep
    type(lorentz_transformation_t) :: rot
    type(vector4_t) :: q0, k0
    real(default) :: p1, p2, p3, pt2, pp2, pl
    real(default) :: aux, den, norm
    real(default) :: st2, ct2, ct
    rot = inverse (rotation_to_2nd (3, space_part (k)))
    q0 = rot * q
    p1 = vector4_get_component (q0, 1)
    p2 = vector4_get_component (q0, 2)
    p3 = vector4_get_component (q0, 3)
    pt2 = p1 ** 2 + p2 ** 2
    pp2 = p1 ** 2 + p2 ** 2 + p3 ** 2
    pl = abs (p3)
    k0 = vector4_moving (d%E, d%p, 3)
    select case (keep)
    case (KEEP_ENERGY)
       d%x = energy (q0) / d%E
       d%xb = 1 - d%x
       call d%set_t_bounds ()
       if (.not. d%collinear) then
          aux = (d%xb * d%pb) ** 2 * pp2 - d%p ** 2 * pt2
          den = d%p ** 2 - (d%xb * d%pb) ** 2
          if (aux >= 0 .and. den > 0) then
             norm = (d%p * pl + sqrt (aux)) / den
          else
             norm = 1
          end if
       end if
    case (KEEP_MOMENTUM)
       d%xb = sqrt (space_part (k0 - q0) ** 2 + d%u) / d%E
       d%x = 1 - d%xb
       call d%set_t_bounds ()
       norm = 1
    end select
    if (d%collinear) then
       d%t = d%t1
       d%phi = 0
    else
       if ((d%xb * d%pb * norm)**2 < epsilon(d%xb)) then
          st2 = 1
       else
          st2 = pt2 / (d%xb * d%pb * norm ) ** 2
       end if
       if (st2 > 1) then
          st2 = 1
       end if
       ct2 = 1 - st2
       ct = sqrt (max (ct2, 0._default))
       if (ct /= -1) then
          d%t = d%t1 - 2 * d%xb * d%p * d%pb * st2 / (1 + ct)
       else
          d%t = d%t0
       end if
       if (p1 /= 0 .or. p2 /= 0) then
          d%phi = atan2 (-p2, -p1)
       else
          d%phi = 0
       end if
    end if
  end subroutine splitting_recover
       
  function splitting_get_x (sd) result (x)
    class(splitting_data_t), intent(in) :: sd
    real(default) :: x
    x = sd%x
  end function splitting_get_x


  subroutine sf_aux_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_aux_1, "sf_aux_1", &
         "massless radiation", &
         u, results)
    call test (sf_aux_2, "sf_aux_2", &
         "massless parton", &
         u, results)
    call test (sf_aux_3, "sf_aux_3", &
         "massless parton", &
         u, results)
  end subroutine sf_aux_test
  
  subroutine sf_aux_1 (u)
    integer, intent(in) :: u
    type(splitting_data_t) :: sd
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q, q0
    real(default) :: E, mk, mp, mq
    real(default) :: x, r1, r2, r1o, r2o
    real(default) :: k2, q0_2, q1_2, q2_2
    
    write (u, "(A)")  "* Test output: sf_aux_1"
    write (u, "(A)")  "*   Purpose: compute momentum splitting"
    write (u, "(A)")  "             (massless radiated particle)"
    write (u, "(A)")

    E = 1
    mk = 0.3_default
    mp = 0
    mq = mk

    k = vector4_moving (E, sqrt (E**2 - mk**2), 3)
    k2 = k ** 2;  call pacify (k2, 1e-10_default)

    x = 0.6_default
    r1 = 0.5_default
    r2 = 0.125_default
    
    write (u, "(A)")  "* (1) Non-collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%sample_t (r1)
    call sd%sample_phi (r2)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q0_2 = q0(2) ** 2;  call pacify (q0_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q0_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t
    

    call sd%inverse_t (r1o)
    
    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)
    
    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q0_2 = q0(2) ** 2;  call pacify (q0_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q0_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    call sd%inverse_t (r1o)

    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)

    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* (2) Collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2, collinear = .true.)
    call sd%set_t_bounds (x, 1 - x)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q0_2 = q0(2) ** 2;  call pacify (q0_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q0_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q0_2 = q0(2) ** 2;  call pacify (q0_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q0_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_aux_1"

  end subroutine sf_aux_1
  
  subroutine sf_aux_2 (u)
    integer, intent(in) :: u
    type(splitting_data_t) :: sd
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q, q0
    real(default) :: E, mk, mp, mq
    real(default) :: x, r1, r2, r1o, r2o
    real(default) :: k2, q02_2, q1_2, q2_2
    
    write (u, "(A)")  "* Test output: sf_aux_2"
    write (u, "(A)")  "*   Purpose: compute momentum splitting"
    write (u, "(A)")  "             (massless outgoing particle)"
    write (u, "(A)")

    E = 1
    mk = 0.3_default
    mp = mk
    mq = 0

    k = vector4_moving (E, sqrt (E**2 - mk**2), 3)
    k2 = k ** 2;  call pacify (k2, 1e-10_default)

    x = 0.6_default
    r1 = 0.5_default
    r2 = 0.125_default
    
    write (u, "(A)")  "* (1) Non-collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%sample_t (r1)
    call sd%sample_phi (r2)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t
    

    call sd%inverse_t (r1o)
    
    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)
    
    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    call sd%inverse_t (r1o)

    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)

    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* (2) Collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2, collinear = .true.)
    call sd%set_t_bounds (x, 1 - x)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_aux_2"

  end subroutine sf_aux_2
  
  subroutine sf_aux_3 (u)
    integer, intent(in) :: u
    type(splitting_data_t) :: sd
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q, q0
    real(default) :: E, mk, mp, mq, qmin, qmax
    real(default) :: x, r1, r2, r1o, r2o
    real(default) :: k2, q02_2, q1_2, q2_2
    
    write (u, "(A)")  "* Test output: sf_aux_3"
    write (u, "(A)")  "*   Purpose: compute momentum splitting"
    write (u, "(A)")  "             (all massless, q cuts)"
    write (u, "(A)")

    E = 1
    mk = 0
    mp = 0
    mq = 0
    qmin = 1e-2_default
    qmax = 1e0_default

    k = vector4_moving (E, sqrt (E**2 - mk**2), 3)
    k2 = k ** 2;  call pacify (k2, 1e-10_default)

    x = 0.6_default
    r1 = 0.5_default
    r2 = 0.125_default
    
    write (u, "(A)")  "* (1) Non-collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%sample_t (r1, t1 = - qmin ** 2, t0 = - qmax **2)
    call sd%sample_phi (r2)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t
    

    call sd%inverse_t (r1o, t1 = - qmin ** 2, t0 = - qmax **2)
    
    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)
    
    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    call sd%inverse_t (r1o, t1 = - qmin ** 2, t0 = - qmax **2)

    write (u, "(A)")  "Compare: r1"
    write (u, "(2(1x,F11.8))")  r1, r1o

    call sd%inverse_phi (r2o)

    write (u, "(A)")  "Compare: r2"
    write (u, "(2(1x,F11.8))")  r2, r2o

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* (2) Collinear setup"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2, collinear = .true.)
    call sd%set_t_bounds (x, 1 - x)

    call sd%write (u)
    
    q = sd%split_momentum (k)
    q1_2 = q(1) ** 2;  call pacify (q1_2, 1e-10_default)
    q2_2 = q(2) ** 2;  call pacify (q2_2, 1e-10_default)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: s"
    write (u, "(2(1x,F11.8))")  sd%s, k2
    
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  sd%t, q2_2
    
    write (u, "(A)")  "Compare: u"
    write (u, "(2(1x,F11.8))")  sd%u, q1_2
    
    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  sd%x, energy (q(2)) / energy (k)
    
    write (u, "(A)")  "Compare: 1-x"
    write (u, "(2(1x,F11.8))")  sd%xb, energy (q(1)) / energy (k)
    
    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep energy)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_ENERGY)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_ENERGY)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Project on-shell (keep momentum)"

    q0 = q
    call on_shell (q0, [mp**2, mq**2], KEEP_MOMENTUM)

    write (u, "(A)")
    write (u, "(A)")  "Incoming momentum k ="
    call vector4_write (k, u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum sum p + q ="
    call vector4_write (sum (q0), u)
    write (u, "(A)")
    write (u, "(A)")  "Radiated momentum p ="
    call vector4_write (q0(1), u)
    write (u, "(A)")
    write (u, "(A)")  "Outgoing momentum q ="
    call vector4_write (q0(2), u)
    write (u, "(A)")

    write (u, "(A)")  "Compare: mo^2"
    q02_2 = q0(2) ** 2;  call pacify (q02_2, 1e-10_default)
    write (u, "(2(1x,F11.8))")  sd%m2, q02_2
    write (u, "(A)")
    
    write (u, "(A)")  "* Recover parameters from outgoing momentum"
    write (u, "(A)")

    call sd%init (k, mk**2, mp**2, mq**2)
    call sd%set_t_bounds (x, 1 - x)
    call sd%recover (k, q0(2), KEEP_MOMENTUM)

    write (u, "(A)")  "Compare: x"
    write (u, "(2(1x,F11.8))")  x, sd%x
    write (u, "(A)")  "Compare: t"
    write (u, "(2(1x,F11.8))")  q2_2, sd%t

    write (u, "(A)")
    call sd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_aux_3"

  end subroutine sf_aux_3
  

end module sf_aux

