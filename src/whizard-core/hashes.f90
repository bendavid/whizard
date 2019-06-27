! WHIZARD 2.0.2 Tue May 18 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module hashes

  use kinds, only: i8, i32 !NODEP!

  implicit none
  private

  public :: hash

contains

  function hash (key)
    integer(i8), dimension(:), intent(in) :: key
    integer(i32) :: hash
    integer :: i
    hash = 0
    do i = 1, size (key)
       hash = hash + key(i)
       hash = hash + ishft (hash, 10)
       hash = ieor (hash, ishft (hash, -6))
    end do
    hash = hash + ishft (hash, 3)
    hash = ieor (hash, ishft (hash, -11))
    hash = hash + ishft (hash, 15)
  end function hash
    

end module hashes
