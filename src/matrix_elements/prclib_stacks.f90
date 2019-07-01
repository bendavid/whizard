! WHIZARD 2.2.4 Feb 06 2015
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

module prclib_stacks
  
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use unit_tests
  use os_interface
  use process_libraries

  implicit none
  private

  public :: prclib_entry_t
  public :: prclib_stack_t
  public :: prclib_stacks_test

  type, extends (process_library_t) :: prclib_entry_t
     type(prclib_entry_t), pointer :: next => null ()
  end type prclib_entry_t

  type :: prclib_stack_t
     integer :: n = 0
     type(prclib_entry_t), pointer :: first => null ()
   contains
     procedure :: final => prclib_stack_final
     procedure :: write => prclib_stack_write
     procedure :: push => prclib_stack_push
     procedure :: get_first_ptr => prclib_stack_get_first_ptr
     procedure :: get_names => prclib_stack_get_names
     procedure :: get_library_ptr => prclib_stack_get_library_ptr
  end type prclib_stack_t
  

contains
  
  subroutine prclib_stack_final (object)
    class(prclib_stack_t), intent(inout) :: object
    type(prclib_entry_t), pointer :: lib
    do while (associated (object%first))
       lib => object%first
       object%first => lib%next
       call lib%final ()
       deallocate (lib)
    end do
    object%n = 0
  end subroutine prclib_stack_final
  
  subroutine prclib_stack_write (object, unit, libpath)
    class(prclib_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: libpath
    type(prclib_entry_t), pointer :: lib
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    select case (object%n)
    case (0)
       write (u, "(1x,A)")  "Process library stack: [empty]"
    case default
       write (u, "(1x,A)")  "Process library stack:"
       lib => object%first
       do while (associated (lib))
          call write_separator (u)
          call lib%write (u, libpath)
          lib => lib%next
       end do
    end select
    call write_separator (u, 2)
  end subroutine prclib_stack_write

  subroutine prclib_stack_push (stack, lib)
    class(prclib_stack_t), intent(inout) :: stack
    type(prclib_entry_t), intent(inout), pointer :: lib
    lib%next => stack%first
    stack%first => lib
    lib => null ()
    stack%n = stack%n + 1
  end subroutine prclib_stack_push
  
  function prclib_stack_get_first_ptr (stack) result (ptr)
    class(prclib_stack_t), intent(in) :: stack
    type(process_library_t), pointer :: ptr
    if (associated (stack%first)) then
       ptr => stack%first%process_library_t
    else
       ptr => null ()
    end if
  end function prclib_stack_get_first_ptr

  subroutine prclib_stack_get_names (stack, libname)
    class(prclib_stack_t), intent(in) :: stack
    type(string_t), dimension(:), allocatable, intent(out) :: libname
    type(prclib_entry_t), pointer :: lib
    integer :: i
    allocate (libname (stack%n))
    i = stack%n
    lib => stack%first
    do while (associated (lib))
       libname(i) = lib%get_name ()
       i = i - 1
       lib => lib%next
    end do
  end subroutine prclib_stack_get_names

  function prclib_stack_get_library_ptr (stack, libname) result (ptr)
    class(prclib_stack_t), intent(in) :: stack
    type(string_t), intent(in) :: libname
    type(process_library_t), pointer :: ptr
    type(prclib_entry_t), pointer :: current
    current => stack%first
    do while (associated (current))
       if (current%get_name () == libname) then
          ptr => current%process_library_t
          return
       end if
       current => current%next
    end do
    ptr => null ()
  end function prclib_stack_get_library_ptr


  subroutine prclib_stacks_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (prclib_stacks_1, "prclib_stacks_1", &
         "write an empty process library stack", &
         u, results)
    call test (prclib_stacks_2, "prclib_stacks_2", &
         "fill a process library stack", &
         u, results)
  end subroutine prclib_stacks_test
  
  subroutine prclib_stacks_1 (u)
    integer, intent(in) :: u
    type(prclib_stack_t) :: stack

    write (u, "(A)")  "* Test output: prclib_stacks_1"
    write (u, "(A)")  "*   Purpose: display an empty process library stack"
    write (u, "(A)")

    call stack%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prclib_stacks_1"
    
  end subroutine prclib_stacks_1
  
  subroutine prclib_stacks_2 (u)
    integer, intent(in) :: u
    type(prclib_stack_t) :: stack
    type(prclib_entry_t), pointer :: lib

    write (u, "(A)")  "* Test output: prclib_stacks_2"
    write (u, "(A)")  "*   Purpose: fill a process library stack"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize two (empty) libraries &
         &and push them on the stack"
    write (u, "(A)")

    allocate (lib)
    call lib%init (var_str ("lib1"))
    call stack%push (lib)

    allocate (lib)
    call lib%init (var_str ("lib2"))
    call stack%push (lib)

    call stack%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call stack%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prclib_stacks_2"
    
  end subroutine prclib_stacks_2
  

end module prclib_stacks
