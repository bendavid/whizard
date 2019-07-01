! WHIZARD 2.2.6 May 02 2015
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

module sm_physics

  use kinds, only: default
  use io_units
  use constants
  use unit_tests
  use diagnostics
  use physics_defs
  use lorentz

  implicit none
  private

  public :: beta0, beta1, beta2, coeff_b0, coeff_b1, coeff_b2
  public :: running_as, running_as_lam
  public :: gamma_g, k_g
  public :: Li2
  public :: faux
  public :: fonehalf
  public :: fonehalf_pseudo
  public :: fone
  public :: gaux
  public :: tri_i1
  public :: tri_i2
  public :: run_b0
  public :: run_b1
  public :: run_aa
  public :: ff_dipole
  public :: fi_dipole
  public :: if_dipole
  public :: ii_dipole
  public :: delta
  public :: plus_distr
  public :: pqq
  public :: pgq
  public :: pqg
  public :: pgg
  public :: pqq_reg
  public :: pgg_reg
  public :: kbarqg
  public :: kbargq
  public :: kbarqq
  public :: kbargg
  public :: ktildeqq
  public :: ktildeqg
  public :: ktildegq
  public :: ktildegg
  public :: insert_q
  public :: insert_g
  public :: k_q_al, k_g_al
  public :: plus_distr_al
  public :: kbarqg_al
  public :: kbargq_al
  public :: kbarqq_al
  public :: kbargg_al
  public :: ktildeqq_al
  public :: log_plus_distr
  public :: log2_plus_distr
  public :: log2_plus_distr_al
  public :: p_qqg
  public :: p_gqq
  public :: p_ggg
  public :: integral_over_p_qqg
  public :: integral_over_p_gqq
  public :: integral_over_p_ggg
  public :: p_qqg_pol
  public :: sm_physics_test

  real(kind=default), parameter, public ::  gamma_q = three/two * CF, &
     k_q = (7.0_default/two - pi**2/6.0_default) * CF


contains

  pure function beta0 (nf)
    real(default), intent(in) :: nf
    real(default) :: beta0
    beta0 = 11.0_default - two/three * nf
  end function beta0

  pure function beta1 (nf)
    real(default), intent(in) :: nf
    real(default) :: beta1
    beta1 = 51.0_default - 19.0_default/three * nf
  end function beta1

  pure function beta2 (nf)
    real(default), intent(in) :: nf
    real(default) :: beta2
    beta2 = 2857.0_default - 5033.0_default / 9.0_default * &
                    nf + 325.0_default/27.0_default * nf**2
  end function beta2

  pure function coeff_b0 (nf)
    real(default), intent(in) :: nf
    real(default) :: coeff_b0
    coeff_b0 = (11.0_default * CA - two * nf) / (12.0_default * pi)
  end function coeff_b0

  pure function coeff_b1 (nf)
    real(default), intent(in) :: nf
    real(default) :: coeff_b1
    coeff_b1 = (17.0_default * CA**2 - five * CA * nf - three * CF * nf) / &
               (24.0_default * pi**2)
  end function coeff_b1

  pure function coeff_b2 (nf)
    real(default), intent(in) :: nf
    real(default) :: coeff_b2
    coeff_b2 = (2857.0_default/54.0_default * CA**3 - &
                    1415.0_default/54.0_default * &
                    CA**2 * nf - 205.0_default/18.0_default * CA*CF*nf &
                    + 79.0_default/54.0_default * CA*nf**2 + &
                    11.0_default/9.0_default * CF * nf**2) / (four*pi)**3
  end function coeff_b2

  pure function running_as (scale, al_mz, mz, order, nf) result (ascale)
    real(default), intent(in) :: scale
    real(default), intent(in), optional :: al_mz, nf, mz
    integer, intent(in), optional :: order
    integer :: ord
    real(default) :: az, m_z, as_log, n_f, b0, b1, b2, ascale
    real(default) :: as0, as1
    if (present (mz)) then
       m_z = mz
    else
       m_z = MZ_REF
    end if
    if (present (order)) then
       ord = order
    else
       ord = 0
    end if
    if (present (al_mz)) then
       az = al_mz
    else
       az = ALPHA_QCD_MZ_REF
    end if
    if (present (nf)) then
       n_f = nf
    else
       n_f = 5
    end if
    b0 = coeff_b0 (n_f)
    b1 = coeff_b1 (n_f)
    b2 = coeff_b2 (n_f)
    as_log = one + b0 * az * log(scale**2/m_z**2)
    as0 = az / as_log
    as1 = as0 - as0**2 * b1/b0 * log(as_log)
    select case (ord)
    case (0)
       ascale = as0
    case (1)
       ascale = as1
    case (2)
       ascale = as1 + as0**3 * (b1**2/b0**2 * ((log(as_log))**2 - &
            log(as_log) + as_log - one) - b2/b0 * (as_log - one))
    case default
       ascale = as0
    end select
  end function running_as

  pure function running_as_lam (nf, scale, lambda, order) result (ascale)
    real(default), intent(in) :: nf, scale
    real(default), intent(in), optional :: lambda
    integer, intent(in), optional :: order
    real(default) :: lambda_qcd
    real(default) :: as0, as1, logmul, b0, b1, b2, ascale
    integer :: ord
    if (present (lambda)) then
       lambda_qcd = lambda
    else
       lambda_qcd = LAMBDA_QCD_REF
    end if
    if (present (order)) then
       ord = order
    else
       ord = 0
    end if
    b0 = beta0(nf)
    logmul = log(scale**2/lambda_qcd**2)
    as0 = four*pi / b0 / logmul
    if (ord > 0) then
       b1 = beta1(nf)
       as1 = as0 * (one - two* b1 / b0**2 * log(logmul) / logmul)
    end if
    select case (ord)
    case (0)
       ascale = as0
    case (1)
       ascale = as1
    case (2)
       b2 = beta2(nf)
       ascale = as1 + as0 * four * b1**2/b0**4/logmul**2 * &
            ((log(logmul) - 0.5_default)**2 + &
             b2*b0/8.0_default/b1**2 - five/four)
    case default
       ascale = as0
    end select
  end function running_as_lam

  elemental function gamma_g (nf) result (gg)
     real(kind=default), intent(in) :: nf
     real(kind=default) :: gg
     gg = 11.0_default/6.0_default * CA - two/three * TR * nf
  end function gamma_g

  elemental function k_g (nf) result (kg)
     real(kind=default), intent(in) :: nf
     real(kind=default) :: kg
     kg = (67.0_default/18.0_default - pi**2/6.0_default) * CA - &
                10.0_default/9.0_default * TR * nf
  end function k_g

  function Li2 (x)
      use kinds, only: double
      real(default), intent(in) :: x
      real(default) :: Li2
      Li2 = real( Li2_double (real(x, kind=double)), kind=default)
  end function Li2

  function Li2_double (x)  result (Li2)
    use kinds, only: double
    real(kind=double), intent(in) :: x
    real(kind=double) :: Li2
    real(kind=double), parameter :: pi2_6 = pi**2/6
    if (abs(1-x) < 1.E-13_double) then
       Li2 = pi2_6
    else if (abs(1-x) <  0.5_double) then
       Li2 = pi2_6 - log(1-x) * log(x) - Li2_restricted (1-x)
    else if (abs(x) > 1.d0) then
       ! Li2 = 0
       ! call msg_bug (" Dilogarithm called outside of defined range.")
       !!! Reactivate Dilogarithm identity
        Li2 = -pi2_6 - 0.5_default * log(-x) * log(-x) - Li2_restricted (1/x)
    else
       Li2 = Li2_restricted (x)
    end if
  contains
    function Li2_restricted (x) result (Li2)
      real(kind=double), intent(in) :: x
      real(kind=double) :: Li2
      real(kind=double) :: tmp, z, z2
      z = - log (1-x)
      z2 = z**2
! Horner's rule for the powers z^3 through z^19
      tmp = 43867._double/798._double
      tmp = tmp * z2 /342._double - 3617._double/510._double
      tmp = tmp * z2 /272._double + 7._double/6._double
      tmp = tmp * z2 /210._double - 691._double/2730._double
      tmp = tmp * z2 /156._double + 5._double/66._double
      tmp = tmp * z2 /110._double - 1._double/30._double
      tmp = tmp * z2 / 72._double + 1._double/42._double
      tmp = tmp * z2 / 42._double - 1._double/30._double
      tmp = tmp * z2 / 20._double + 1._double/6._double
! The first three terms of the power series
      Li2 = z2 * z * tmp / 6._double - 0.25_double * z2 + z
    end function Li2_restricted
  end function Li2_double

  elemental function faux (x) result (y)
    real(default), intent(in) :: x
    complex(default) :: y
    if (1 <= x) then
       y = asin(sqrt(1/x))**2
    else
       y = - 1/4.0_default * (log((1 + sqrt(1 - x))/ &
            (1 - sqrt(1 - x))) - cmplx (0.0_default, pi, kind=default))**2
    end if
  end function faux

  elemental function fonehalf (x) result (y)
    real(default), intent(in) :: x
    complex(default) :: y
    if (abs(x) < eps0) then
       y = 0
    else
       y = - 2.0_default * x * (1 + (1 - x) * faux(x))
    end if
  end function fonehalf

  function fonehalf_pseudo (x) result (y)
    real(default), intent(in) :: x
    complex(default) :: y
    if (abs(x) < eps0) then
       y = 0
    else
       y = - 2.0_default * x * faux(x)
    end if
  end function fonehalf_pseudo

  elemental function fone (x) result  (y)
    real(default), intent(in) :: x
    complex(default) :: y
    if (abs(x) < eps0) then
       y = 2.0_default
    else
       y = 2.0_default + 3.0_default * x + &
            3.0_default * x * (2.0_default - x) * &
            faux(x)
    end if
  end function fone

  elemental function gaux (x) result (y)
    real(default), intent(in) :: x
    complex(default) :: y
    if (1 <= x) then
       y = sqrt(x - 1) * asin(sqrt(1/x))
    else
       y = sqrt(1 - x) * (log((1 + sqrt(1 - x)) / &
            (1 - sqrt(1 - x))) - &
            cmplx (0.0_default, pi, kind=default)) / 2.0_default
    end if
  end function gaux

  elemental function tri_i1 (a,b) result (y)
    real(default), intent(in) :: a,b
    complex(default) :: y
    if (a < eps0 .or. b < eps0) then
       y = 0
    else
       y = a*b/2.0_default/(a-b) + a**2 * b**2/2.0_default/(a-b)**2 * &
            (faux(a) - faux(b)) + &
            a**2 * b/(a-b)**2 * (gaux(a) - gaux(b))
    end if
  end function tri_i1

  elemental function tri_i2 (a,b) result (y)
    real(default), intent(in) :: a,b
    complex(default) :: y
    if (a < eps0 .or. b < eps0) then
       y = 0
    else
       y = - a * b / 2.0_default / (a-b) * (faux(a) - faux(b))
    end if
  end function tri_i2

  elemental function run_b0 (nf) result (bnull)
    integer, intent(in) :: nf
    real(default) :: bnull
    bnull = 33.0_default - 2.0_default * nf
  end function run_b0

  elemental function run_b1 (nf) result (bone)
      integer, intent(in) :: nf
      real(default) :: bone
      bone = 6.0_default * (153.0_default - 19.0_default * nf)/run_b0(nf)**2
  end function run_b1

  elemental function run_aa (nf) result (aaa)
      integer, intent(in) :: nf
      real(default) :: aaa
      aaa = 12.0_default * PI / run_b0(nf)
  end function run_aa

  elemental function run_bb (nf) result (bbb)
    integer, intent(in) :: nf
    real(default) :: bbb
    bbb = run_b1(nf) / run_aa(nf)
  end function run_bb

  pure subroutine ff_dipole (v_ijk,y_ijk,p_ij,pp_k,p_i,p_j,p_k)
      type(vector4_t), intent(in) :: p_i, p_j, p_k
      type(vector4_t), intent(out) :: p_ij, pp_k
      real(kind=default), intent(out) :: y_ijk
      real(kind=default) :: z_i
      real(kind=default), intent(out) :: v_ijk
      z_i   = (p_i*p_k) / ((p_k*p_j) + (p_k*p_i))
      y_ijk = (p_i*p_j) / ((p_i*p_j) + (p_i*p_k) + (p_j*p_k))
      p_ij  = p_i + p_j - y_ijk/(1.0_default - y_ijk) * p_k
      pp_k  = (1.0/(1.0_default - y_ijk)) * p_k
      !!! We don't multiply by alpha_s right here:
      v_ijk = 8.0_default * PI * CF * &
           (2.0 / (1.0 - z_i*(1.0 - y_ijk)) - (1.0 + z_i))
  end subroutine ff_dipole

  pure subroutine fi_dipole (v_ija,x_ija,p_ij,pp_a,p_i,p_j,p_a)
     type(vector4_t), intent(in) :: p_i, p_j, p_a
     type(vector4_t), intent(out) :: p_ij, pp_a
     real(kind=default), intent(out) :: x_ija
     real(kind=default) :: z_i
     real(kind=default), intent(out) :: v_ija
     z_i   = (p_i*p_a) / ((p_a*p_j) + (p_a*p_i))
     x_ija = ((p_i*p_a) + (p_j*p_a) - (p_i*p_j)) &
          / ((p_i*p_a) + (p_j*p_a))
     p_ij  = p_i + p_j - (1.0_default - x_ija) * p_a
     pp_a  = x_ija * p_a
     !!! We don't not multiply by alpha_s right here:
     v_ija = 8.0_default * PI * CF * &
          (2.0 / (1.0 - z_i + (1.0 - x_ija)) - (1.0 + z_i)) / x_ija
  end subroutine fi_dipole

  pure subroutine if_dipole (v_kja,u_j,p_aj,pp_k,p_k,p_j,p_a)
     type(vector4_t), intent(in) :: p_k, p_j, p_a
     type(vector4_t), intent(out) :: p_aj, pp_k
     real(kind=default), intent(out) :: u_j
     real(kind=default) :: x_kja
     real(kind=default), intent(out) :: v_kja
     u_j   = (p_a*p_j) / ((p_a*p_j) + (p_a*p_k))
     x_kja = ((p_a*p_k) + (p_a*p_j) - (p_j*p_k)) &
          / ((p_a*p_j) + (p_a*p_k))
     p_aj  = x_kja * p_a
     pp_k  = p_k + p_j - (1.0_default - x_kja) * p_a
     v_kja = 8.0_default * PI * CF * &
          (2.0 / (1.0 - x_kja + u_j) - (1.0 + x_kja)) / x_kja
  end subroutine if_dipole

  pure subroutine ii_dipole (v_jab,v_j,p_in,p_out,flag_1or2)
      type(vector4_t), dimension(:), intent(in) :: p_in
      type(vector4_t), dimension(size(p_in)-1), intent(out) :: p_out
      logical, intent(in) :: flag_1or2
      real(kind=default), intent(out) :: v_j
      real(kind=default), intent(out) :: v_jab
      type(vector4_t) :: p_a, p_b, p_j
      type(vector4_t) :: k, kk
      type(vector4_t) :: p_aj
      real(kind=default) :: x_jab
      integer :: i
      !!! flag_1or2 decides whether this a 12 or 21 dipole
      if (flag_1or2) then
         p_a = p_in(1)
         p_b = p_in(2)
      else
         p_b = p_in(1)
         p_a = p_in(2)
      end if
      !!! We assume that the unresolved particle has always the last
      !!! momentum
      p_j = p_in(size(p_in))
      x_jab = ((p_a*p_b) - (p_a*p_j) - (p_b*p_j)) / (p_a*p_b)
      v_j = (p_a*p_j) / (p_a * p_b)
      p_aj  = x_jab * p_a
      k     = p_a + p_b - p_j
      kk    = p_aj + p_b
      do i = 3, size(p_in)-1
         p_out(i) = p_in(i) - 2.0*((k+kk)*p_in(i))/((k+kk)*(k+kk)) * (k+kk) + &
              (2.0 * (k*p_in(i)) / (k*k)) * kk
      end do
      if (flag_1or2) then
         p_out(1) = p_aj
         p_out(2) = p_b
      else
         p_out(1) = p_b
         p_out(2) = p_aj
      end if
      v_jab = 8.0_default * PI * CF * &
           (2.0 / (1.0 - x_jab) - (1.0 + x_jab)) / x_jab
  end subroutine ii_dipole
  elemental function delta (x,eps) result (z)
     real(kind=default), intent(in) :: x, eps
     real(kind=default) :: z
     if (x > one - eps) then
        z = one / eps
     else
        z = 0
     end if
  end function delta

  elemental function plus_distr (x,eps) result (plusd)
     real(kind=default), intent(in) :: x, eps
     real(kind=default) :: plusd
     if (x > one - eps) then
        plusd = log(eps) / eps
     else
        plusd = one / (one - x)
     end if
  end function plus_distr

  elemental function pqq (x,eps) result (pqqx)
     real(kind=default), intent(in) :: x, eps
     real(kind=default) :: pqqx
     if (x > (1.0_default - eps)) then
        pqqx = (eps - one) / two + two * log(eps) / eps - &
             three * (eps - one) / eps / two
     else
        pqqx = (one + x**2) / (one - x)
     end if
     pqqx = CF * pqqx
  end function pqq

  elemental function pgq (x) result (pgqx)
     real(kind=default), intent(in) :: x
     real(kind=default) :: pgqx
     pgqx = TR * (x**2 + (one - x)**2)
  end function pgq

  elemental function pqg (x) result (pqgx)
     real(kind=default), intent(in) :: x
     real(kind=default) :: pqgx
     pqgx = CF * (one + (one - x)**2) / x
  end function pqg

  elemental function pgg (x, nf, eps) result (pggx)
    real(kind=default), intent(in) :: x, nf, eps
    real(kind=default) :: pggx
    pggx = two * CA * ( plus_distr (x, eps) + (one-x)/x - one + &
                   x*(one-x)) + delta (x, eps)  * gamma_g(nf)
  end function pgg

  elemental function pqq_reg (x) result (pqqregx)
     real(kind=default), intent(in) :: x
     real(kind=default) :: pqqregx
     pqqregx = - CF * (one + x)
  end function pqq_reg

  elemental function pgg_reg (x) result (pggregx)
     real(kind=default), intent(in) :: x
     real(kind=default) :: pggregx
     pggregx = two * CA * ((one - x)/x - one + x*(one - x))
  end function pgg_reg

  function kbarqg (x) result (kbarqgx)
    real(kind=default), intent(in) :: x
    real(kind=default) :: kbarqgx
    kbarqgx = pqg(x) * log((one-x)/x) + CF * x
  end function kbarqg

  function kbargq (x) result (kbargqx)
    real(kind=default), intent(in) :: x
    real(kind=default) :: kbargqx
    kbargqx = pgq(x) * log((one-x)/x) + two * TR * x * (one - x)
  end function kbargq

  function kbarqq (x,eps) result (kbarqqx)
    real(kind=default), intent(in) :: x, eps
    real(kind=default) :: kbarqqx
    kbarqqx = CF*(log_plus_distr(x,eps) - (one+x) * log((one-x)/x) + (one - &
         x) - (five - pi**2) * delta(x,eps))
  end function kbarqq

  function kbargg (x,eps,nf) result (kbarggx)
    real(kind=default), intent(in) :: x, eps, nf
    real(kind=default) :: kbarggx
    kbarggx = CA * (log_plus_distr(x,eps) + two * ((one-x)/x - one + &
                         x*(one-x) * log((1-x)/x))) - delta(x,eps) * &
                         ((50.0_default/9.0_default - pi**2) * CA - &
                         16.0_default/9.0_default * TR * nf)
  end function kbargg

  function ktildeqq (x,eps) result (ktildeqqx)
    real(kind=default), intent(in) :: x, eps
    real(kind=default) :: ktildeqqx
    ktildeqqx = pqq_reg (x) * log(one-x) + CF * ( - log2_plus_distr (x,eps) &
                          - pi**2/three * delta(x,eps))
  end function ktildeqq

  function ktildeqg (x,eps) result (ktildeqgx)
    real(kind=default), intent(in) :: x, eps
    real(kind=default) :: ktildeqgx
    ktildeqgx = pqg (x) * log(one-x)
  end function ktildeqg

  function ktildegq (x,eps) result (ktildegqx)
    real(kind=default), intent(in) :: x, eps
    real(kind=default) :: ktildegqx
    ktildegqx = pgq (x) * log(one-x)
  end function ktildegq

  function ktildegg (x,eps) result (ktildeggx)
    real(kind=default), intent(in) :: x, eps
    real(kind=default) :: ktildeggx
    ktildeggx = pgg_reg (x) * log(one-x) + CA * ( - &
       log2_plus_distr (x,eps) - pi**2/three * delta(x,eps))
  end function ktildegg

  pure function insert_q ()
    real(kind=default), dimension(0:2) :: insert_q
    insert_q(0) = gamma_q + k_q - pi**2/three * CF
    insert_q(1) = gamma_q
    insert_q(2) = CF
  end function insert_q

  pure function insert_g (nf)
    real(kind=default), intent(in) :: nf
    real(kind=default), dimension(0:2) :: insert_g
    insert_g(0) = gamma_g (nf) + k_g (nf) - pi**2/three * CA
    insert_g(1) = gamma_g (nf)
    insert_g(2) = CA
  end function insert_g

  pure function k_q_al (alpha)
    real(kind=default), intent(in) :: alpha
    real(kind=default) :: k_q_al
    k_q_al = k_q - CF * (log(alpha))**2 + gamma_q * &
                      (alpha - one - log(alpha))
  end function k_q_al

  pure function k_g_al (alpha, nf)
    real(kind=default), intent(in) :: alpha, nf
    real(kind=default) :: k_g_al
    k_g_al = k_g (nf) - CA * (log(alpha))**2 + gamma_g (nf) * &
                     (alpha - one - log(alpha))
  end function k_g_al

  function plus_distr_al (x,alpha,eps) result (plusd_al)
     real(kind=default), intent(in) :: x,  eps, alpha
     real(kind=default) :: plusd_al
     if ((one - alpha) >= (one - eps)) then
        plusd_al = zero
        call msg_fatal ('sm_physics, plus_distr_al: alpha and epsilon chosen wrongly')
     elseif (x < (1.0_default - alpha)) then
        plusd_al = 0
     else if (x > (1.0_default - eps)) then
        plusd_al = log(eps/alpha)/eps
     else
        plusd_al = one/(one-x)
     end if
   end function plus_distr_al

  function kbarqg_al (x,alpha,eps) result (kbarqgx)
    real(kind=default), intent(in) :: x, alpha, eps
    real(kind=default) :: kbarqgx
    kbarqgx = pqg (x) * log(alpha*(one-x)/x) + CF * x
  end function kbarqg_al
  function kbargq_al (x,alpha,eps) result (kbargqx)
    real(kind=default), intent(in) :: x, alpha, eps
    real(kind=default) :: kbargqx
    kbargqx = pgq (x) * log(alpha*(one-x)/x) + two * TR * x * (one-x)
  end function kbargq_al
  function kbarqq_al (x,alpha,eps) result (kbarqqx)
     real(kind=default), intent(in) :: x, alpha, eps
     real(kind=default) :: kbarqqx
     kbarqqx = CF * (one - x) + pqq_reg(x) * log(alpha*(one-x)/x) &
              + CF * log_plus_distr(x,eps) &
             - (gamma_q + k_q_al(alpha) - CF * &
              five/6.0_default  * pi**2 - CF * (log(alpha))**2) * &
              delta(x,eps) + &
              CF * two/(one -x)*log(alpha*(two-x)/(one+alpha-x))
     if (x < (one-alpha)) then
        kbarqqx = kbarqqx - CF * two/(one-x) * log((two-x)/(one-x))
     end if
  end function kbarqq_al

  function kbargg_al (x,alpha,eps,nf) result (kbarggx)
     real(kind=default), intent(in) :: x, alpha, eps, nf
     real(kind=default) :: kbarggx
     kbarggx = pgg_reg(x) * log(alpha*(one-x)/x) &
              + CA * log_plus_distr(x,eps) &
             - (gamma_g(nf) + k_g_al(alpha,nf) - CA * &
              five/6.0_default  * pi**2 - CA * (log(alpha))**2) * &
              delta(x,eps) + &
              CA * two/(one -x)*log(alpha*(two-x)/(one+alpha-x))
     if (x < (one-alpha)) then
        kbarggx = kbarggx - CA * two/(one-x) * log((two-x)/(one-x))
     end if
  end function kbargg_al

  function ktildeqq_al (x,alpha,eps) result (ktildeqqx)
    real(kind=default), intent(in) :: x, eps, alpha
    real(kind=default) :: ktildeqqx
    ktildeqqx = pqq_reg(x) * log((one-x)/alpha) + CF*( &
         - log2_plus_distr_al(x,alpha,eps) - Pi**2/three * delta(x,eps) &
         + (one+x**2)/(one-x) * log(min(one,(alpha/(one-x)))) &
         + two/(one-x) * log((one+alpha-x)/alpha))
    if (x > (one-alpha)) then
       ktildeqqx = ktildeqqx - CF*two/(one-x)*log(two-x)
    end if
  end function ktildeqq_al

  function log_plus_distr (x,eps) result (lpd)
     real(kind=default), intent(in) :: x, eps
     real(kind=default) :: lpd, eps2
     eps2 = min (eps, 0.1816_default)
     if (x > (1.0_default - eps2)) then
        lpd = ((log(eps2))**2 + two*Li2(eps2) - pi**2/three)/eps2
     else
        lpd = two*log((one-x)/x)/(one-x)
     end if
  end function log_plus_distr

  function log2_plus_distr (x,eps) result (lpd)
      real(kind=default), intent(in) :: x, eps
      real(kind=default) :: lpd
      if (x > (1.0_default - eps)) then
         lpd = - (log(eps))**2/eps
      else
         lpd = two*log(one/(one-x))/(one-x)
      end if
  end function log2_plus_distr

  function log2_plus_distr_al (x,alpha,eps) result (lpd_al)
    real(kind=default), intent(in) :: x, eps, alpha
    real(kind=default) :: lpd_al
    if ((one - alpha) >= (one - eps)) then
       lpd_al = zero
       call msg_fatal ('alpha and epsilon chosen wrongly')
    elseif (x < (one - alpha)) then
       lpd_al = 0
    elseif (x > (1.0_default - eps)) then
       lpd_al = - ((log(eps))**2 - (log(alpha))**2)/eps
    else
       lpd_al = two*log(one/(one-x))/(one-x)
    end if
  end function log2_plus_distr_al

  elemental function p_qqg (z) result (P)
    real(default), intent(in) :: z
    real(default) :: P
    P = CF * (one + z**2) / (one - z)
  end function p_qqg
  elemental function p_gqq (z) result (P)
    real(default), intent(in) :: z
    real(default) :: P
    P = TR * (z**2 + (one - z)**2)
  end function p_gqq
  elemental function p_ggg (z) result (P)
    real(default), intent(in) :: z
    real(default) :: P
    P = NC * ((one - z) / z + z / (one - z) + z * (one - z))
  end function p_ggg

  pure function integral_over_p_qqg (zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral
    integral = (two / three) * (- zmax**2 + zmin**2 - &
         two * (zmax - zmin) + four * log((one - zmin) / (one - zmax)))
  end function integral_over_p_qqg

  pure function integral_over_p_gqq (zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral
    integral = 0.5_default * ((two / three) * &
         (zmax**3 - zmin**3) - (zmax**2 - zmin**2) + (zmax - zmin))
  end function integral_over_p_gqq

  pure function integral_over_p_ggg (zmin, zmax) result (integral)
    real(default), intent(in) :: zmin, zmax
    real(default) :: integral
    integral = three * ((log(zmax) - two * zmax - &
         log(one - zmax) + zmax**2 / two - zmax**3 / three) - &
         (log(zmin) - zmin - zmin - log(one - zmin) + zmin**2 &
         / two - zmin**3 / three) )
  end function integral_over_p_ggg

  elemental function p_qqg_pol (z, l_a, l_b, l_c) result (P)
    real(default), intent(in) :: z
    integer, intent(in) :: l_a, l_b, l_c
    real(default) :: P
    if (l_a /= l_b) then
       P = zero
       return
    end if
    if (l_c == -1) then
       P = one - z
    else
       P = (one + z)**2 / (one - z)
    end if
    P = P * CF
  end function p_qqg_pol


  subroutine sm_physics_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sm_physics_1, "sm_physics_1", &
         "Splitting functions", &
         u, results)
  end subroutine sm_physics_test

  subroutine sm_physics_1 (u)
    integer, intent(in) :: u
    real(default) :: z = 0.75_default

    write (u, "(A)")  "* Test output: sm_physics_1"
    write (u, "(A)")  "*   Purpose: check analytic properties"
    write (u, "(A)")

    write (u, "(A)")  "* Splitting functions:"
    write (u, "(A)")

    call assert (u, vanishes (p_qqg_pol (z, +1, -1, +1)))
    call assert (u, vanishes (p_qqg_pol (z, +1, -1, -1)))
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, +1)))
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, -1)))

    call assert (u, nearly_equal ( &
         p_qqg_pol (z, +1, +1, -1) + p_qqg_pol (z, +1, +1, +1), &
         p_qqg (z)))

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sm_physics_1"

  end subroutine sm_physics_1


end module sm_physics
