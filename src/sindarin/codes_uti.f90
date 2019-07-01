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

module codes_uti
  
  use io_units

  use codes
    
  implicit none
  private

  public :: codes_1

contains
  
  subroutine codes_1 (u)
    integer, intent(in) :: u
    integer :: utmp, i
    type(code_t) :: code
    character(256) :: buffer

    write (u, "(A)")  "* Test output: codes_1"
    write (u, "(A)")  "*   Purpose: check code I/O"
    write (u, "(A)")      

    utmp = free_unit ()
    open (utmp, status="scratch", action="readwrite")

    write (utmp, "(1x,A)")  "4 0 0 0"
    write (utmp, "(1x,A)")  "5 2 1 3 5 6 7"
    write (utmp, "(1x,A)")  "foo"
    write (utmp, "(1x,A)")  "7 1 2 0"
    write (utmp, "(1x,A)")  "T"
    write (utmp, "(1x,A)")  "F"
    write (utmp, "(1x,A)")  "42 3 3 0"
    write (utmp, "(1x,A)")  "0"
    write (utmp, "(1x,A)")  "12345"
    write (utmp, "(1x,A)")  "-987654321"
    
    rewind (utmp)
    do
       read (utmp, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (utmp)
    write (u, *)

    do i = 1, 4
       call code%read (utmp)
       call code%write (u, verbose=.true.)
    end do

    rewind (utmp)
    write (u, *)

    do i = 1, 4
       call code%read (utmp)
       call code%write (u)
    end do

    close (utmp)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: codes_1"
    
  end subroutine codes_1


end module codes_uti
