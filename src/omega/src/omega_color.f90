!  $Id: omegalib.nw 2453 2010-05-01 06:02:28Z jr_reuter $
!
!  Copyright (C) 1999-2009 by 
!      Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!      Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!      Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!
!  WHIZARD is free software; you can redistribute it and/or modify it
!  under the terms of the GNU General Public License as published by 
!  the Free Software Foundation; either version 2, or (at your option)
!  any later version.
!
!  WHIZARD is distributed in the hope that it will be useful, but
!  WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module omega_color
  use kinds
  implicit none
  private
  public :: omega_color_factor
  type omega_color_factor
     integer :: i1, i2
     complex(kind=default) :: factor
  end type omega_color_factor
  public :: omega_color_sum
  integer, parameter, public :: omega_color_2010_01_A = 0
contains
  pure function omega_color_sum (flv, hel, amp, cf) result (amp2)
    complex(kind=default) :: amp2
    integer, intent(in) :: flv, hel
    complex(kind=default), dimension(:,:,:), intent(in) :: amp
    type(omega_color_factor), dimension(:), intent(in) :: cf
    integer :: n
    amp2 = 0
    do n = 1, size (cf)
       amp2 = amp2 &
            + cf(n)%factor * amp(flv,hel,cf(n)%i1) * conjg (amp(flv,hel,cf(n)%i2))
    end do
  end function omega_color_sum
end module omega_color
