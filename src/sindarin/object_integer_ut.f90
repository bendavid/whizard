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

module object_integer_ut

  use unit_tests
  use object_integer_uti
  
  implicit none
  private

  public :: object_integer_test

contains
  
  subroutine object_integer_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (object_integer_1, "object_integer_1", &
         "values", &
         u, results)
    call test (object_integer_2, "object_integer_2", &
         "assignment", &
         u, results)
    call test (object_integer_3, "object_integer_3", &
         "composite assignment", &
         u, results)
    call test (object_integer_4, "object_integer_4", &
         "operator: minus", &
         u, results)
    call test (object_integer_5, "object_integer_5", &
         "operator: multiply", &
         u, results)
    call test (object_integer_6, "object_integer_6", &
         "operator: multiply and divide", &
         u, results)
    call test (object_integer_7, "object_integer_7", &
         "operator: add", &
         u, results)
    call test (object_integer_8, "object_integer_8", &
         "operator: add and subtract", &
         u, results)
    call test (object_integer_9, "object_integer_9", &
         "nested expressions", &
         u, results)
    call test (object_integer_10, "object_integer_10", &
         "build expressions from code", &
         u, results)  
  end subroutine object_integer_test
  

end module object_integer_ut
