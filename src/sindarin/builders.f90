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

module builders

  use codes

  implicit none
  private

  public :: builder_t

  type, abstract :: builder_t
     private
   contains
     procedure(builder_final), deferred :: final
     procedure(builder_write), deferred :: write
     procedure(builder_decode), deferred :: decode
     procedure(builder_build), deferred :: build
     procedure :: copy => builder_copy
  end type builder_t
   

  abstract interface
     subroutine builder_final (builder)
       import
       class(builder_t), intent(inout) :: builder
     end subroutine builder_final
  end interface

  abstract interface
     subroutine builder_write (builder, unit)
       import
       class(builder_t), intent(in) :: builder
       integer, intent(in), optional :: unit
     end subroutine builder_write
  end interface

  abstract interface
     subroutine builder_decode (builder, code, success)
       import
       class(builder_t), intent(inout) :: builder
       type(code_t), intent(out) :: code
       logical, intent(out) :: success
     end subroutine builder_decode
  end interface

  abstract interface
     subroutine builder_build (builder, code, success)
       import
       class(builder_t), intent(inout) :: builder
       type(code_t), intent(in) :: code
       logical, intent(out) :: success
     end subroutine builder_build
  end interface
    

contains
  
  subroutine builder_copy (builder, builder_source, success)
    class(builder_t), intent(inout) :: builder
    class(builder_t), intent(inout) :: builder_source
    logical, intent(out) :: success
    type(code_t) :: code
    call builder_source%decode (code, success)
    if (.not. success)  return
    call builder%build (code, success)
  end subroutine builder_copy
    

end module builders
