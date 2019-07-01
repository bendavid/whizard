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

module object_base_ut

  use unit_tests
  use object_base_uti
  
  implicit none
  private

  public :: object_base_test

contains
  
  subroutine object_base_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (object_base_1, "object_base_1", &
         "object and prototype", &
         u, results)
    call test (object_base_2, "object_base_2", &
         "composite object", &
         u, results)
    call test (object_base_3, "object_base_3", &
         "object path search", &
         u, results)
    call test (object_base_4, "object_base_4", &
         "object references and copies", &
         u, results)
    call test (object_base_5, "object_base_5", &
         "object iterator", &
         u, results)
    call test (object_base_6, "object_base_6", &
         "prototype repository", &
         u, results)
    call test (object_base_7, "object_base_7", &
         "build composite using code", &
         u, results)
    call test (object_base_8, "object_base_8", &
         "named reference", &
         u, results)  
  end subroutine object_base_test
  

end module object_base_ut
