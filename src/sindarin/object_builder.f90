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

module object_builder

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units
  use diagnostics
  use codes
  use builders
  use object_base

  implicit none
  private

  public :: object_builder_t

  type, extends (builder_t) :: object_builder_t
     private
     type(repository_t), allocatable :: repository
     class(object_t), pointer :: main => null ()
     type(object_iterator_t) :: it
   contains
     procedure :: final => object_builder_final
     procedure :: write => object_builder_write
     procedure :: write_repository => object_builder_write_repository
     procedure :: write_object => object_builder_write_object
     procedure :: write_iterator => object_builder_write_iterator
     procedure :: import_repository => object_builder_import_repository
     procedure :: reset => object_builder_reset
     procedure :: init_object => object_builder_init_object
     procedure :: init_empty => object_builder_init_empty
     procedure :: decode => object_builder_decode
     procedure :: build => object_builder_build
     procedure :: export => object_builder_export
  end type object_builder_t
  

contains

  subroutine object_builder_final (builder)
    class(object_builder_t), intent(inout) :: builder
    call builder%reset ()
    if (allocated (builder%repository)) then
       call builder%repository%final ()
       deallocate (builder%repository)
    end if
  end subroutine object_builder_final

  subroutine object_builder_write (builder, unit)
    class(object_builder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A)")  "Object builder:"
    call builder%write_repository (u)
    call builder%write_object (u)
    call builder%write_iterator (u)
  end subroutine object_builder_write

  subroutine object_builder_write_repository (builder, unit)
    class(object_builder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x)", advance="no")
    if (allocated (builder%repository)) then
       call builder%repository%write (u)
    else
       write (u, "(A)")  "[no repository]"
    end if
  end subroutine object_builder_write_repository
  
  subroutine object_builder_write_object (builder, unit)
    class(object_builder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    class(object_t), pointer :: core
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x)", advance="no")
    if (associated (builder%main)) then
       select type (main => builder%main)
       class is (wrapper_t)
          call main%get_core_ptr (core)
          if (associated (core)) then
             call core%write (u)
          else
             write (u, "(A)")  "[Empty object]"
          end if
       end select
    else
       write (u, "(A)")  "[No object]"
    end if
  end subroutine object_builder_write_object
  
  subroutine object_builder_write_iterator (builder, unit)
    class(object_builder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (builder%it%is_valid ()) then
       write (u, "(1x,A)", advance="no")  "Iterator:"
       call builder%it%write (u)
       write (u, *)
    else
       write (u, "(1x,A)")  "[Null iterator]"
    end if
  end subroutine object_builder_write_iterator
  
  subroutine object_builder_import_repository (builder, repository)
    class(object_builder_t), intent(inout) :: builder
    type(repository_t), intent(inout), allocatable :: repository
    call move_alloc (from=repository, to=builder%repository)
  end subroutine object_builder_import_repository
  
  subroutine object_builder_reset (builder)
    class(object_builder_t), intent(inout) :: builder
    call builder%it%final ()
    if (associated (builder%main)) then
       call remove_object (builder%main)
    end if
  end subroutine object_builder_reset
    
  subroutine object_builder_init_object (builder, object)
    class(object_builder_t), intent(inout) :: builder
    class(object_t), intent(inout), target :: object
    call builder%reset ()
    call object%make_reference (builder%main)
    call builder%it%init (builder%main%dereference ())
  end subroutine object_builder_init_object
  
  subroutine object_builder_init_empty (builder)
    class(object_builder_t), intent(inout) :: builder
    call builder%reset ()
    allocate (wrapper_t :: builder%main)
    call builder%it%init (builder%main)
  end subroutine object_builder_init_empty
    
  subroutine object_builder_decode (builder, code, success)
    class(object_builder_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    class(object_t), pointer :: object
    success = builder%it%is_valid ()
    if (success) then
       call builder%it%get_object (object)
       code = object%get_code (builder%repository)
       call builder%it%advance ()
    end if
  end subroutine object_builder_decode
  
  subroutine object_builder_build (builder, code, success)
    class(object_builder_t), intent(inout) :: builder
    type(code_t), intent(in) :: code
    logical, intent(out) :: success
    class(object_t), pointer :: object
    logical, parameter :: DEBUG = .false.
!     logical, parameter :: DEBUG = .true.
    if (DEBUG)  print *
    if (DEBUG)  print *, "build object"
    if (DEBUG)  call code%write ()
    call build_object (object, code, builder%repository)
    if (associated (object)) then
       call builder%it%advance (import_object = object)
    else
       call builder%it%advance ()
    end if
    success = builder%it%is_valid ()
    if (success) then
       call builder%it%get_object (object)
       if (DEBUG)  call builder%it%write ()
       if (DEBUG)  print *
       if (DEBUG)  call object%write ()
       select type (object)
       class is (value_t)
          call object%init_from_code (code)
       class is (ref_array_t)
          call object%init_from_code (code)
       end select
    else
       call builder%write ()
       call code%write ()
       call msg_fatal ("Sindarin: error in byte code")
    end if
  end subroutine object_builder_build
    
  subroutine object_builder_export (builder, object)
    class(object_builder_t), intent(inout) :: builder
    class(object_t), intent(out), pointer :: object
    object => null ()
    if (associated (builder%main)) then
       select type (main => builder%main)
       class is (wrapper_t)
          call main%get_core_ptr (object)
       end select
       deallocate (builder%main)
    end if
    call builder%reset ()
  end subroutine object_builder_export
          

end module object_builder
