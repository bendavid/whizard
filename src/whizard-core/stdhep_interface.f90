! WHIZARD 2.0.3 Tue Aug 10 2010
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

module stdhep_interface

  use kinds, only: i32, i64 !NODEP!

  implicit none
  private
 
  public :: stdhep_init, stdhep_write, stdhep_end

  integer, parameter, public :: &
       STDHEP_HEPEVT = 1, STDHEP_HEPEUP = 11, STDHEP_HEPRUP = 12

  integer, save :: istr, lok

contains

  subroutine stdhep_init (file, title, nevt)
    character(len=*), intent(in) :: file, title
    integer(i64), intent(in) :: nevt
    integer(i32) :: nevt32
    external stdxwinit, stdxwrt
    nevt32 = min (nevt, int (huge (1_i32), i64))
    call stdxwinit (file, title, nevt32, istr, lok)
    call stdxwrt (100, istr, lok)
  end subroutine stdhep_init

  subroutine stdhep_write (ilbl)
    integer, intent(in) :: ilbl
    external stdxwrt
    call stdxwrt (ilbl, istr, lok)
  end subroutine stdhep_write

  subroutine stdhep_end
    external stdxend
    call stdxend (istr)
  end subroutine stdhep_end

end module stdhep_interface
