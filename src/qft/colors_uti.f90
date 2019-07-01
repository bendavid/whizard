! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
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

module colors_uti

  use colors

  implicit none
  private

  public :: color_1

contains

  subroutine color_1 (u)
    integer, intent(in) :: u
    type(color_t), dimension(4) :: col1, col2, col
    type(color_t), dimension(:), allocatable :: col3
    type(color_t), dimension(:,:), allocatable :: col_array
    integer :: count, i
    call col1%init_col_acl ([1, 0, 2, 3], [0, 1, 3, 2])
    col2 = col1
    call color_write (col1, u)
    write (u, "(A)")
    call color_write (col2, u)
    write (u, "(A)")
    col = col1 .merge. col2
    call color_write (col, u)
    write (u, "(A)")
    count = count_color_loops (col)
    write (u, "(A,I1)") "Number of color loops (3): ", count
    call col2%init_col_acl ([1, 0, 2, 3], [0, 2, 3, 1])
    call color_write (col1, u)
    write (u, "(A)")
    call color_write (col2, u)
    write (u, "(A)")
    col = col1 .merge. col2
    call color_write (col, u)
    write (u, "(A)")
    count = count_color_loops (col)
    write (u, "(A,I1)")  "Number of color loops (2): ", count
    write (u, "(A)")
    allocate (col3 (4))
    call color_init_from_array (col3, &
         reshape ([1, 0,   0, -1,  2, -3,  3, -2], & 
                  [2, 4]))
    call color_write (col3, u)
    write (u, "(A)")
    call color_array_make_contractions (col3, col_array)
    write (u, "(A)")  "Contractions:"
    do i = 1, size (col_array, 2)
       call color_write (col_array(:,i), u)
       write (u, "(A)")
    end do
    deallocate (col3)
    write (u, "(A)")
    allocate (col3 (6))
    call color_init_from_array (col3, &
         reshape ([1, -2,   3, 0,  0, -1,  2, -4,  -3, 0,  4, 0], & 
                  [2, 6]))
    call color_write (col3, u)
    write (u, "(A)")
    call color_array_make_contractions (col3, col_array)
    write (u, "(A)")  "Contractions:"
    do i = 1, size (col_array, 2)
       call color_write (col_array(:,i), u)
       write (u, "(A)")
    end do
  end subroutine color_1


end module colors_uti
