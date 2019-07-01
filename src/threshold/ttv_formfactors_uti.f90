! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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

module ttv_formfactors_uti

  use kinds, only: default
  use constants
  use ttv_formfactors
  use diagnostics
  use sm_physics, only: running_as
  use unit_tests

  implicit none
  private

  public :: ttv_formfactors_1

contains

  subroutine ttv_formfactors_1 (u)
    integer, intent(in) :: u
    real(default) :: m1s, Vtb, wt_inv, alphaemi, sw, alphas_mz, mz, &
         mw, mb, sh, sf, nloop, FF, v1, v2, scan_sqrts_max, sqrts, &
         scan_sqrts_min, scan_sqrts_stepsize, test, gam_out, mpole
    type(phase_space_point_t) :: ps
    integer :: ff_mode
    logical :: mpole_fixed
    write (u, "(A)")  "* Test output: ttv_formfactors_1"
    write (u, "(A)")  "*   Purpose: Basic setup"
    write (u, "(A)")

    m1s = 172.0_default
    Vtb = one
    wt_inv = zero
    alphaemi = 125.0_default
    alphas_mz = 0.118_default
    mz = 91.1876_default
    mw = 80.399_default
    sw = sqrt(one - mw**2 / mz**2)
    mb = 4.2_default
    sh = one
    sf = one
    nloop = one
    !FF = RESUMMED_P0CONSTANT
    FF = MATCHED
    v1 = 0.3
    v2 = 0.5
    sqrts = 2 * m1s + 0.01_default
    scan_sqrts_min = sqrts
    scan_sqrts_max = sqrts
    scan_sqrts_stepsize = 0.1_default
    mpole_fixed = .true.
    test = - one
    !msg_level(D_THRESHOLD) = DEBUG2
    call init_parameters &
         (mpole, gam_out, m1s, Vtb, wt_inv, &
          alphaemi, sw, alphas_mz, mz, mw, &
          mb, sh, sf, nloop, FF, &
          v1, v2, scan_sqrts_min, scan_sqrts_max, &
          scan_sqrts_stepsize, mpole_fixed)
    call init_threshold_grids (test)

    write (u, "(A)") "Check that the mass is fixed"
    call ps%init (m1s**2, m1s**2, sqrts**2, mpole)
    call assert (u, m1s_to_mpole (350.0_default) == m1s, &
         "m1s_to_mpole (350.0_default) == m1s")
    call assert (u, m1s_to_mpole (550.0_default) == m1s, &
         "m1s_to_mpole (550.0_default) == m1s")
    write (u, "(A)") ""

    write (u, "(A)") "Check that the mass is not fixed"
    mpole_fixed = .false.
    call init_parameters &
         (mpole, gam_out, m1s, Vtb, wt_inv, &
          alphaemi, sw, alphas_mz, mz, mw, &
          mb, sh, sf, nloop, FF, &
          v1, v2, scan_sqrts_min, scan_sqrts_max, &
          scan_sqrts_stepsize, mpole_fixed)
    call init_threshold_grids (test)
    call assert (u, m1s_to_mpole (350.0_default) > m1s + 0.1_default, &
         "m1s_to_mpole (350.0_default) > m1s")
    write (u, "(A)")

    !!! care FF_master contains the tree level that we usually subtract again
    write (u, "(A)") "Check low energy behavior"
    mpole_fixed = .true.
    call init_parameters &
         (mpole, gam_out, m1s, Vtb, wt_inv, &
          alphaemi, sw, alphas_mz, mz, mw, &
          mb, sh, sf, nloop, FF, &
          v1, v2, scan_sqrts_min, scan_sqrts_max, &
          scan_sqrts_stepsize, mpole_fixed)
    call init_threshold_grids (test)
    call assert_equal (u, f_switch_off (v_matching (ps%sqrts)), one, "f_switch_off (v_matching (ps%sqrts))")
    !call assert_equal (u, &
         !abs (FF_master (ps, 1, EXPANDED_HARD_P0CONSTANT)), &
         !abs (FF_master (ps, 1, RESUMMED_P0CONSTANT)), &
         !"expansion should be smaller than resummed?")
    call assert_equal (u, &
         FF_master (ps, 1, EXPANDED_SOFT_SWITCHOFF_P0CONSTANT), &
         FF_master (ps, 1, EXPANDED_SOFT_P0CONSTANT), &
         "switchoff function should do nothing here")
    write (u, "(A)") ""

    write (u, "(A)") "Check high energy behavior"
    sqrts = 500.0_default
    scan_sqrts_min = sqrts
    scan_sqrts_max = sqrts
    call init_parameters &
         (mpole, gam_out, m1s, Vtb, wt_inv, &
          alphaemi, sw, alphas_mz, mz, mw, &
          mb, sh, sf, nloop, FF, &
          v1, v2, scan_sqrts_min, scan_sqrts_max, &
          scan_sqrts_stepsize, mpole_fixed)
    call init_threshold_grids (test)
    ! For simplicity we test on-shell back-to-back tops
    !p = sqrt(sqrts / 4 - mpole**2)
    call ps%init (m1s**2, m1s**2, sqrts**2, mpole)
    call assert_equal (u, f_switch_off (v_matching (ps%sqrts)), tiny_10, &
         "f_switch_off (v_matching (ps%sqrts))")
    call assert (u, &
         abs (FF_master (ps, 1, EXPANDED_HARD_P0CONSTANT)) > &
         abs (FF_master (ps, 1, RESUMMED_P0CONSTANT)), &
         "expansion with hard alphas should be larger " // &
         "than resummed (with switchoff)")
    call assert_equal (u, &
         abs (FF_master (ps, 1, RESUMMED_P0CONSTANT)), one, &
         "resummed (with switchoff) should be one (tree-level)")
    call assert_equal (u, &
         abs (FF_master (ps, 1, EXPANDED_SOFT_SWITCHOFF_P0CONSTANT)), one, &
         "expanded (with switchoff) should be one (tree-level)")
    write (u, "(A)") ""

    write (u, "(A)") "Check global variables"
    call assert_equal (u, AS_HARD, &
         running_as (m1s, alphas_mz, mz, 2, 5.0_default), "hard alphas")
    call assert_equal (u, AS_SOFT, zero, "soft alphas", abs_smallness=tiny_10)
    call assert_equal (u, AS_USOFT, zero, "ultrasoft alphas", abs_smallness=tiny_10)
    call assert_equal (u, AS_LL_SOFT, zero, "LL soft alphas", abs_smallness=tiny_10)

    write (u, "(A)")  "* Test output end: ttv_formfactors_1"
  end subroutine ttv_formfactors_1


end module ttv_formfactors_uti
