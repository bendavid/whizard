! $Id: parameters.QED.omega.f90,v 1.1 2004/03/11 04:21:17 kilian Exp $
!
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
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

module parameters_qed
  use kinds

  implicit none
  private
  public :: init_parameters

  real(default), dimension(22), public :: mass, width
  complex(default), public :: qlep

  integer, parameter :: qelep = -1
  complex(default), parameter :: e = 0.3_default

contains

  subroutine init_parameters
    mass(1:22) = 0
    width(1:22) = 0
    ! mass(11) = 511e-6_default
    ! mass(13) = 105.66e-3_default
    ! mass(15) = 1.777_default
    qlep = - e * qelep
  end subroutine init_parameters

end module parameters_qed
