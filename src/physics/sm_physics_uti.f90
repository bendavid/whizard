! WHIZARD 2.3.1 Aug 25 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module sm_physics_uti

  use kinds, only: default
  use numeric_utils
  use format_defs, only: FMT_15
  use constants

  use sm_physics

  implicit none
  private

  public :: sm_physics_1
  public :: sm_physics_2

contains

  subroutine sm_physics_1 (u)
    integer, intent(in) :: u
    real(default) :: z = 0.75_default

    write (u, "(A)")  "* Test output: sm_physics_1"
    write (u, "(A)")  "*   Purpose: check analytic properties"
    write (u, "(A)")

    write (u, "(A)")  "* Splitting functions:"
    write (u, "(A)")

    call assert (u, vanishes (p_qqg_pol (z, +1, -1, +1)), "+-+")
    call assert (u, vanishes (p_qqg_pol (z, +1, -1, -1)), "+--")
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, +1)), "-++")
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, -1)), "-+-")

    !call assert (u, nearly_equal ( &
         !p_qqg_pol (z, +1, +1, -1) + p_qqg_pol (z, +1, +1, +1), &
         !p_qqg (z)), "pol sum")

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sm_physics_1"

  end subroutine sm_physics_1

  subroutine sm_physics_2 (u)
    integer, intent(in) :: u
    real(default) :: mtop, mw, mz, mb, g_mu, sinthw, alpha, vtb, gamma0
    real(default) :: w2, alphas, alphas_mz, gamma1
    write (u, "(A)")  "* Test output: sm_physics_2"
    write (u, "(A)")  "*   Purpose: Check different top width computations"
    write (u, "(A)")

    write (u, "(A)")  "*   Values from [[1207.5018]] (massless b)"
    mtop = 172.0
    mw = 80.399
    mz = 91.1876
    mb = zero
    mb = 0.00001
    g_mu = 1.16637E-5
    sinthw = sqrt(one - mw**2 / mz**2)
    alpha = alpha_from_g_mu (g_mu, mw, sinthw)
    vtb = one
    w2 = mw**2 / mtop**2

    write (u, "(A)")  "*   Check Li2 implementation"
    call assert_equal (u, Li2(w2), 0.2317566263959552_default, &
         "Li2(w2)", rel_smallness=1.0E-6_default)
    call assert_equal (u, Li2(one - w2), 1.038200378935867_default, &
         "Li2(one - w2)", rel_smallness=1.0E-6_default)

    write (u, "(A)")  "*   Check LO Width"
    gamma0 = top_width_sm_lo (alpha, sinthw, vtb, mtop, mw, mb)
    call assert_equal (u, gamma0, 1.4655_default, &
         "top_width_sm_lo", rel_smallness=1.0E-5_default)
    alphas = zero
    gamma0 = top_width_sm_qcd_nlo_massless_b (alpha, sinthw, mtop, mw, alphas)
    call assert_equal (u, gamma0, 1.4655_default, &
         "top_width_sm_qcd_nlo_massless_b", rel_smallness=1.0E-5_default)
    gamma0 = top_width_sm_qcd_nlo (alpha, sinthw, mtop, mw, mb, alphas)
    call assert_equal (u, gamma0, 1.4655_default, &
         "top_width_sm_qcd_nlo", rel_smallness=1.0E-5_default)

    write (u, "(A)")  "*   Check NLO Width"
    alphas_mz = 0.1202      ! MSTW2008 NLO fit
    alphas = running_as (mtop, alphas_mz, mz, 1, 5.0_default)
    gamma1 = top_width_sm_qcd_nlo_massless_b (alpha, sinthw, mtop, mw, alphas)
    call assert_equal (u, gamma1, 1.3376_default, rel_smallness=1.0E-4_default)
    gamma1 = top_width_sm_qcd_nlo (alpha, sinthw, mtop, mw, mb, alphas)
    ! It would be nice to get one more significant digit but the
    ! expression is numerically rather unstable for mb -> 0
    call assert_equal (u, gamma1, 1.3376_default, rel_smallness=1.0E-3_default)

    write (u, "(A)")  "*   Values from threshold validation (massive b)"
    alpha = one / 125.924
    ! ee = 0.315901
    ! cw = 0.881903
    ! v = 240.024
    mtop = 172.0 ! This is the value for M1S !!!
    mb = 4.2
    sinthw = 0.47143
    mz = 91.188
    mw = 80.419
    call assert_equal (u, sqrt(one - mw**2 / mz**2), sinthw, "sinthw", rel_smallness=1.0E-6_default)

    write (u, "(A)")  "*   Check LO Width"
    gamma0 = top_width_sm_lo (alpha, sinthw, vtb, mtop, mw, mb)
    call assert_equal (u, gamma0, 1.5386446_default, "gamma0", rel_smallness=1.0E-7_default)
    alphas = zero
    gamma0 = top_width_sm_qcd_nlo (alpha, sinthw, mtop, mw, mb, alphas)
    call assert_equal (u, gamma0, 1.5386446_default, "gamma0", rel_smallness=1.0E-7_default)

    write (u, "(A)")  "*   Check NLO Width"
    alphas_mz = 0.118 !(Z pole, NLL running to mu_h)
    alphas = running_as (mtop, alphas_mz, mz, 1, 5.0_default)
    write (u, "(A," // FMT_15 // ")")  "*   alphas = ", alphas
    gamma1 = top_width_sm_qcd_nlo (alpha, sinthw, mtop, mw, mb, alphas)
    write (u, "(A," // FMT_15 // ")")  "*   Gamma1 = ", gamma1

    mb = zero
    gamma1 = top_width_sm_qcd_nlo_massless_b (alpha, sinthw, mtop, mw, alphas)
    alphas = running_as (mtop, alphas_mz, mz, 1, 5.0_default)
    write (u, "(A," // FMT_15 // ")")  "*   Gamma1(mb=0) = ", gamma1

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sm_physics_2"
  end subroutine sm_physics_2


end module sm_physics_uti
