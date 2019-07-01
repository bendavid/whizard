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

module sf_lhapdf_ut
  use unit_tests
  use system_dependencies, only: LHAPDF5_AVAILABLE
  use system_dependencies, only: LHAPDF6_AVAILABLE  
  use sf_lhapdf_uti
  
  implicit none
  private

  public :: sf_lhapdf_test

contains
  
  subroutine sf_lhapdf_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    if (LHAPDF5_AVAILABLE) then  
       call test (sf_lhapdf_1, "sf_lhapdf5_1", &
            "structure function configuration", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_1, "sf_lhapdf6_1", &
            "structure function configuration", &
            u, results)
    end if
    if (LHAPDF5_AVAILABLE) then
       call test (sf_lhapdf_2, "sf_lhapdf5_2", &
            "structure function instance", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_2, "sf_lhapdf6_2", &
            "structure function instance", &
            u, results)     
    end if
    if (LHAPDF5_AVAILABLE) then
       call test (sf_lhapdf_3, "sf_lhapdf5_3", &
            "running alpha_s", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_3, "sf_lhapdf6_3", &
            "running alpha_s", &
            u, results)     
    end if
  end subroutine sf_lhapdf_test
  

end module sf_lhapdf_ut
