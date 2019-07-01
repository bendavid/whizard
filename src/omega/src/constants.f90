! WHIZARD <<Version>> <<Date>>

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

module constants

  use kinds, only: default

  implicit none
  private

  complex(default), parameter, public :: &
       imago = (0._default, 1._default)

  real(default), parameter, public :: &
       zero = 0.0_default, &
       one = 1.0_default, two = 2.0_default, three = 3.0_default, &
       four = 4.0_default, five = 5.0_default

  real(default), parameter, public :: &
       pi = 3.1415926535897932384626433832795028841972_default

  real(default), parameter, public :: &
       twopi = 2*pi, &
       twopi2 = twopi**2,  twopi3 = twopi**3,  twopi4 = twopi**4, &
       twopi5 = twopi**5,  twopi6 = twopi**6

  real(default), parameter, public :: &
       degree = pi/180

  real(default), parameter, public :: &
       conv = 0.38937966e12_default

  real(default), parameter, public :: &
       pb_per_fb = 1.e-3_default

  real(default), parameter, public :: &
       NC = three, CF = (NC**2 - one)/two/NC, CA = NC, &
       TR = one/two

  character(*), parameter, public :: &
       energy_unit = "GeV"

  character(*), parameter, public :: &
       cross_section_unit = "fb"

end module constants
