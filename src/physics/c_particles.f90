! WHIZARD 2.2.5 Feb 27 2015
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

module c_particles

  use, intrinsic :: iso_c_binding !NODEP!

  use io_units
  use format_defs, only: FMT_14, FMT_19

  implicit none
  private

  public :: c_prt_t
  public :: c_prt_write

  type, bind(C) :: c_prt_t
     integer(c_int) :: type = 0
     integer(c_int) :: pdg = 0
     integer(c_int) :: polarized = 0
     integer(c_int) :: h = 0
     real(c_double) :: pe = 0
     real(c_double) :: px = 0
     real(c_double) :: py = 0
     real(c_double) :: pz = 0
     real(c_double) :: p2 = 0
  end type c_prt_t


contains

  subroutine c_prt_write (prt, unit)
    type(c_prt_t), intent(in) :: prt
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)", advance="no")  "prt("
    write (u, "(I0,':')", advance="no")  prt%type
    if (prt%polarized /= 0) then
       write (u, "(I0,'/',I0,'|')", advance="no")  prt%pdg, prt%h
    else
       write (u, "(I0,'|')", advance="no") prt%pdg
    end if
    write (u, "(" // FMT_14 // ",';'," // FMT_14 // ",','," // &
         FMT_14 // ",','," // FMT_14 // ")", advance="no") &
         prt%pe, prt%px, prt%py, prt%pz
    write (u, "('|'," // FMT_19 // ")", advance="no")  prt%p2
    write (u, "(A)")  ")"
  end subroutine c_prt_write

end module c_particles
