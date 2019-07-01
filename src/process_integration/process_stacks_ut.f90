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

module process_stacks_ut
  use unit_tests
  use process_stacks_uti
  
  implicit none
  private

  public :: process_stacks_test

contains
  
  subroutine process_stacks_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (process_stacks_1, "process_stacks_1", &
         "write an empty process stack", &
         u, results)
    call test (process_stacks_2, "process_stacks_2", &
         "fill a process stack", &
         u, results)
    call test (process_stacks_3, "process_stacks_3", &
         "process variables", &
         u, results)
    call test (process_stacks_4, "process_stacks_4", &
         "linked stacks", &
         u, results)
  end subroutine process_stacks_test
  

end module process_stacks_ut
