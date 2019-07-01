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
  use ttv_formfactors
  implicit none

  real(c_default_float), dimension(*), intent(inout) :: par
  real(default) :: m1s_in, wtop, alphas, mZ, &
                   nloop, sh, sf, match, mpole, &
                   alphaemi, sw, mW, mb
  mZ     = par(1)
  mW     = par(2)
  alphas = par(4)
  mb     = par(10)
  alphaemi = par(19)
  m1s_in = par(20)
  wtop   = par(21)
  nloop  = par(22)
  sh     = par(23)
  sf     = par(24)
  sw     = par(27)
  call ttv_formfactors_init (mpole, m1s_in, wtop, alphaemi, sw, alphas, &
                mZ, mW, mb, sh, sf, nloop)
  par(21) = wtop
  par(29) = mpole
end subroutine init_external_parameters
