! circe2_sample.f90 -- testing circe2
! $Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $
! Copyright (C) 2001-2014 by 
!      Wolfgang Kilian <kilian@physik.uni-siegen.de>
!      Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!      Juergen Reuter <juergen.reuter@desy.de>
!      Christian Speckner <cnspeckn@googlemail.com>
!
! Circe2 is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! Circe2 is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  module circe2_sample_routines
    use kinds
    use circe2
    use tao_rng

    implicit none
    private

    public :: random
    public :: write3

contains

  subroutine random (u)
    real(kind=double), intent(out) :: u
    call taornu (u)
  end subroutine random

  subroutine write3 (x, y, w)
    real(kind=double), intent(in) :: x, y, w
    print *, x, y, w
  end subroutine write3

  end module circe2_sample_routines  

  program circe2_sample
    use kinds
    use circe2
    use circe2_sample_routines
    
  implicit none

    integer :: i, p1, h1, p2, h2, n, ierror
    character(len=256) :: file, design, mode
    real(kind=double) :: roots, x1, x2, w
    read *, file, design, roots, p1, h1, p2, h2, mode, n
    ierror = 0
    call cir2ld (file, design, roots, ierror)
    if (ierror .ne. 0) then
       print *, 'sample: cir2ld failed!'
       stop
    end if
    if ((mode(1:1) .eq. 'w') .or. (mode(1:1) .eq. 'W')) then
           do i = 1, n
              call random (x1)
              call random (x2)
              w = cir2dn (p1, h1, p2, h2, x1, x2)
              call write3 (x1, x2, w)
           end do
    else
           do i = 1, n    
              call cir2gn (p1, h1, p2, h2, x1, x2, random)
              call write3 (x1, x2, 1d0)
           end do
    end if
  end program circe2_sample

