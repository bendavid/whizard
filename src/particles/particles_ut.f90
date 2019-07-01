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

module particles_ut
  use unit_tests
  use particles_uti
  
  implicit none
  private

  public :: particles_test

contains
  
  subroutine particles_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (particles_1, "particles_1", &
         "check particle_set routines", &
         u, results)
    call test (particles_2, "particles_2", &
         "reconstruct hard interaction", &
         u, results)
    call test (particles_3, "particles_3", &
         "reconstruct interaction with beam structure", &
         u, results)
    call test (particles_4, "particles_4", &
         "reconstruct interaction with missing beams", &
         u, results)
    call test (particles_5, "particles_5", &
         "reconstruct interaction with beams and duplicate entries", &
         u, results)
    call test (particles_6, "particles_6", &
         "reconstruct interaction with pair spectrum", &
         u, results)
    call test (particles_7, "particles_7", &
         "reconstruct decay interaction with reordering", &
         u, results)
    call test (particles_8, "particles_8", &
              "Test functions on particle sets", u, results)  
  end subroutine particles_test


end module particles_ut
