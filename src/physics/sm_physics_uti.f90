! WHIZARD 2.2.7 Aug 11 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module sm_physics_uti

  use kinds, only: default
  use unit_tests, only: nearly_equal, vanishes, assert

  use sm_physics

  implicit none
  private

  public :: sm_physics_1

contains

  subroutine sm_physics_1 (u)
    integer, intent(in) :: u
    real(default) :: z = 0.75_default

    write (u, "(A)")  "* Test output: sm_physics_1"
    write (u, "(A)")  "*   Purpose: check analytic properties"
    write (u, "(A)")

    write (u, "(A)")  "* Splitting functions:"
    write (u, "(A)")

    call assert (u, vanishes (p_qqg_pol (z, +1, -1, +1)))
    call assert (u, vanishes (p_qqg_pol (z, +1, -1, -1)))
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, +1)))
    call assert (u, vanishes (p_qqg_pol (z, -1, +1, -1)))

    call assert (u, nearly_equal ( &
         p_qqg_pol (z, +1, +1, -1) + p_qqg_pol (z, +1, +1, +1), &
         p_qqg (z)))

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sm_physics_1"

  end subroutine sm_physics_1


end module sm_physics_uti
