! $Id: parameters.QCD.omega.f90,v 1.1 2005/05/19 16:00:08 kilian Exp $
!
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     with contributions from
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
module parameters_qcd
  use kinds 
  use constants
  implicit none
  private
  real(default), dimension(21), public :: mass, width
  real(default), public :: as
  complex(default), public :: gs, igs
  public :: import_from_whizard, model_update_alpha_s
contains
  subroutine import_from_whizard (par_array)
    real(default), dimension(6), intent(in) :: par_array
    type :: parameter_set
       real(default) :: alphas
       real(default) :: ms
       real(default) :: mc
       real(default) :: mb
       real(default) :: mtop
       real(default) :: wtop
    end type parameter_set
    type(parameter_set) :: par
    par%alphas = par_array(1)
    par%ms = par_array(2)
    par%mc = par_array(3)
    par%mb = par_array(4)
    par%mtop = par_array(5)
    par%wtop = par_array(6)
    mass(1:21) = 0
    width(1:21) = 0
    mass(3) = par%ms
    mass(4) = par%mc
    mass(5) = par%mb
    mass(6) = par%mtop
    width(6) = par%wtop    
    ! color-flow basis: gs is divided by sqrt2
    gs = sqrt(2.0_default*PI*par%alphas)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs    
  end subroutine import_from_whizard

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs        
  end subroutine model_update_alpha_s
end module parameters_qcd
