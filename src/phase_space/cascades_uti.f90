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

module cascades_uti

  use kinds, only: default
  use flavors
  use model_data
  use phs_forests, only: phs_parameters_t

  use cascades

  implicit none
  private

  public :: cascade_1

contains

  subroutine cascade_1 (u)  
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(5,2) :: flv
    type(cascade_set_t) :: cascade_set
    type(phs_parameters_t) :: phs_par

    write (u, "(A)")  "* Test output: Cascades"
    write (u, "(A)")  "*   Purpose: test cascade phase space functions"
    write (u, "(A)")  
    
    write (u, "(A)")  "* Initializing"
    write (u, "(A)")    
    
    call model%init_sm_test ()

    call flv(1,1)%init ( 2, model)
    call flv(2,1)%init (-2, model)
    call flv(3,1)%init ( 1, model)
    call flv(4,1)%init (-1, model)
    call flv(5,1)%init (21, model)
    call flv(1,2)%init ( 2, model)
    call flv(2,2)%init (-2, model)
    call flv(3,2)%init ( 2, model)
    call flv(4,2)%init (-2, model)
    call flv(5,2)%init (21, model)
    phs_par%sqrts = 1000._default
    phs_par%off_shell = 2
    
    write (u, "(A)")
    write (u, "(A)")  "* Generating the cascades"
    write (u, "(A)")
    
    call cascade_set_generate (cascade_set, model, 2, 3, flv, phs_par,.true.)
    call cascade_set_write (cascade_set, u)
    call cascade_set_write_file_format (cascade_set, u)

    write (u, "(A)")  "* Cleanup"
    write (u, "(A)")
    
    call cascade_set_final (cascade_set)
    call model%final ()
    
    write (u, *)
    write (u, "(A)")  "* Test output end: cascade_1"
        
  end subroutine cascade_1


end module cascades_uti
