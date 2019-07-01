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

module object_builder_uti
  
    use iso_varying_string, string_t => varying_string
    use io_units
    use codes
    use object_base

    use object_builder

  implicit none
  private

  public :: object_builder_1

contains
  
  subroutine object_builder_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype
    class(object_t), pointer :: object, member
    type(repository_t), allocatable :: repository
    type(object_builder_t) :: builder
    type(code_t) :: code
    logical :: success
    integer :: utmp, iostat

    write (u, "(A)")  "* Test output: object_builder_1"
    write (u, "(A)")  "*   Purpose: object building using builder"

    write (u, "(A)")      
    write (u, "(A)")  "* Create repository with tag prototype"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select
    
    allocate (repository)
    call repository%init (name = var_str ("repository"), n_members = 1)
    call repository%import_member (1, prototype)

    write (u, "(A)")
    call repository%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create composite"
    
    call repository%spawn (var_str ("tag"), member)
    select type (member)
    class is (composite_t)
       call member%init (name = var_str ("foo"))
    end select
    
    call repository%spawn (var_str ("tag"), object)
    select type (object)
    class is (composite_t)
       call object%init (name = var_str ("obj1"), n_members = 1)
       call object%import_member (1, member)
    end select
    
    write (u, "(A)")
    call object%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create builder"
    
    call builder%import_repository (repository)

    write (u, "(A)")
    call builder%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Initialize builder with object"
    
    call builder%init_object (object)

    write (u, "(A)")
    call builder%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Decode"
    write (u, "(A)")      
      
    utmp = free_unit ()
    open (utmp, status="scratch")

    do
       call builder%decode (code, success)
       if (.not. success)  exit
       call code%write (u, verbose=.true.)
       call code%write (utmp)
    end do

    call remove_object (object)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Reset builder"
    write (u, "(A)")     
 
    call builder%init_empty ()
    call builder%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Reconstruct object"
    write (u, "(A)")      

    rewind (utmp)
    do
       call code%read (utmp, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (utmp)

    call builder%export (object)
    call object%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"

    call builder%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_builder_1"
    
    end subroutine object_builder_1


end module object_builder_uti
