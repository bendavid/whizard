! $Id: external_Test.f90 2364 2010-04-20 12:47:06Z cnspeckn $
!
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     Fabian Bach <fabian.bach@desy.de> (only this file)
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
subroutine init_external_parameters (par) bind (C)
  use iso_c_binding
  use kinds
  use diagnostics
  use ttv_formfactors
  implicit none

  real(c_default_float), dimension(*), intent(inout) :: par
  real(default) :: m1s, Vtb, wt_inv, alphas, mZ, &
                   nrqcd_order, sh, sf, FF, offshell_strategy, v1, v2, mpole, &
                   aemi, sw, mW, mb, wtop, sqrts_min, sqrts_max, sqrts_it
  logical :: mpole_fixed
  call msg_debug (D_THRESHOLD, "init_external_parameters")
  mZ     = par(1)
  mW     = par(2)
  alphas = par(4)
  mb     = par(10)
  aemi   = par(19)
  m1s    = par(20)
  Vtb    = par(21)
  wt_inv = par(22)
  nrqcd_order  = par(23)
  sh     = par(24)
  sf     = par(25)
  FF     = par(26)
  offshell_strategy = par(27)
  v1     = par(28)
  v2     = par(29)
  sqrts_min = par(30)
  sqrts_max = par(31)
  sqrts_it = par(32)
  mpole_fixed = par(35) > 0.0_default
  sw     = par(38)
  call init_parameters (mpole, wtop, m1s, Vtb, wt_inv, aemi, &
                        sw, alphas, mZ, mW, mb, sh, sf, nrqcd_order, FF, &
                        offshell_strategy, v1, v2, &
                        sqrts_min, sqrts_max, sqrts_it, mpole_fixed)
  par(40) = mpole
  par(41) = wtop
end subroutine init_external_parameters
