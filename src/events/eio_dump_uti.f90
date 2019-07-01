! WHIZARD 2.3.1 Aug 25 2016
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

module eio_dump_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use event_base
  use eio_data
  use eio_base

  use eio_dump
  
  use eio_base_ut, only: eio_prepare_test, eio_cleanup_test

  implicit none
  private

  public :: eio_dump_1

contains

  subroutine eio_dump_1 (u)
    integer, intent(in) :: u
    class(generic_event_t), pointer :: event
    class(eio_t), allocatable :: eio
    integer :: u_file
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_dump_1"
    write (u, "(A)")  "*   Purpose: generate an event and dump to output"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call eio_prepare_test (event, unweighted = .false.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    allocate (eio_dump_t :: eio)
    select type (eio)
    type is (eio_dump_t)
       eio%unit = u
       eio%writing = .true.
       eio%weights = .true.
    end select
    
    call eio%init_out (var_str (""))
    call event%generate (1, [0._default, 0._default])

    call eio%output (event, i_prc = 42)

    write (u, "(A)")
    write (u, "(A)")  "* Contents of eio_dump object"
    write (u, "(A)")

    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    select type (eio)
    type is (eio_dump_t)
       eio%writing = .false.
    end select
    call eio%final ()

    call eio_cleanup_test (event) 

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_dump_1"
  end subroutine eio_dump_1
  

end module eio_dump_uti
