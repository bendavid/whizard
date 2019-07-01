! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module process_stacks
  
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use unit_tests
  use diagnostics
  use os_interface
  use sm_qcd
  use variables
  use model_data
  use rng_base

  use process_libraries
  use prc_test
  use processes

  implicit none
  private

  public :: process_entry_t
  public :: process_stack_t
  public :: process_stacks_test

  type, extends (process_t) :: process_entry_t
     type(process_entry_t), pointer :: next => null ()
  end type process_entry_t

  type :: process_stack_t
     integer :: n = 0
     type(process_entry_t), pointer :: first => null ()
     type(var_list_t), pointer :: var_list => null ()
     type(process_stack_t), pointer :: next => null ()
   contains
     procedure :: clear => process_stack_clear
     procedure :: final => process_stack_final
     procedure :: write => process_stack_write
     procedure :: write_var_list => process_stack_write_var_list
     procedure :: show => process_stack_show
     procedure :: link => process_stack_link
     procedure :: init_var_list => process_stack_init_var_list
     procedure :: link_var_list => process_stack_link_var_list
     procedure :: push => process_stack_push
     procedure :: init_result_vars => process_stack_init_result_vars
     procedure :: fill_result_vars => process_stack_fill_result_vars
     procedure :: exists => process_stack_exists
     procedure :: get_process_ptr => process_stack_get_process_ptr
  end type process_stack_t
  

contains
  
  subroutine process_stack_clear (stack)
    class(process_stack_t), intent(inout) :: stack
    type(process_entry_t), pointer :: process
    if (associated (stack%var_list)) then
       call var_list_final (stack%var_list)
    end if
    do while (associated (stack%first))
       process => stack%first
       stack%first => process%next
       call process%final ()
       deallocate (process)
    end do
    stack%n = 0
  end subroutine process_stack_clear
  
  subroutine process_stack_final (object)
    class(process_stack_t), intent(inout) :: object
    type(process_entry_t), pointer :: process
    call object%clear ()
    if (associated (object%var_list)) then
       deallocate (object%var_list)
    end if
  end subroutine process_stack_final
  
  recursive subroutine process_stack_write (object, unit, pacify)
    class(process_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    type(process_entry_t), pointer :: process
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    select case (object%n)
    case (0)
       write (u, "(1x,A)")  "Process stack: [empty]"
       call write_separator (u, 2)
    case default
       write (u, "(1x,A)")  "Process stack:"
       process => object%first
       do while (associated (process))
          call process%write (.false., u, pacify = pacify)
          process => process%next
       end do
    end select
    if (associated (object%next)) then
       write (u, "(1x,A)")  "[Processes from context environment:]"
       call object%next%write (u, pacify)
    end if
  end subroutine process_stack_write

  subroutine process_stack_write_var_list (object, unit)
    class(process_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    if (associated (object%var_list)) then
       call var_list_write (object%var_list, unit)
    end if
  end subroutine process_stack_write_var_list

  recursive subroutine process_stack_show (object, unit)
    class(process_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(process_entry_t), pointer :: process
    integer :: u
    u = given_output_unit (unit)
    select case (object%n)
    case (0)
    case default
       process => object%first
       do while (associated (process))
          call process%show (u, verbose=.false.)
          process => process%next
       end do
    end select
    if (associated (object%next))  call object%next%show ()
  end subroutine process_stack_show

  subroutine process_stack_link (local_stack, global_stack)
    class(process_stack_t), intent(inout) :: local_stack
    type(process_stack_t), intent(in), target :: global_stack
    local_stack%next => global_stack
  end subroutine process_stack_link

  subroutine process_stack_init_var_list (stack, var_list)
    class(process_stack_t), intent(inout) :: stack
    type(var_list_t), intent(inout), optional :: var_list
    allocate (stack%var_list)
    if (present (var_list))  call var_list_link (var_list, stack%var_list)
  end subroutine process_stack_init_var_list
  
  subroutine process_stack_link_var_list (stack, var_list)
    class(process_stack_t), intent(inout) :: stack
    type(var_list_t), intent(in), target :: var_list
    call var_list_link (stack%var_list, var_list)
  end subroutine process_stack_link_var_list
  
  subroutine process_stack_push (stack, process)
    class(process_stack_t), intent(inout) :: stack
    type(process_entry_t), intent(inout), pointer :: process
    process%next => stack%first
    stack%first => process
    process => null ()
    stack%n = stack%n + 1
  end subroutine process_stack_push
  
  subroutine process_stack_init_result_vars (stack, id)
    class(process_stack_t), intent(inout) :: stack
    type(string_t), intent(in) :: id
    call var_list_init_num_id (stack%var_list, id)
    call var_list_init_process_results (stack%var_list, id)
  end subroutine process_stack_init_result_vars
  
  subroutine process_stack_fill_result_vars (stack, id)
    class(process_stack_t), intent(inout) :: stack
    type(string_t), intent(in) :: id
    type(process_t), pointer :: process
    process => stack%get_process_ptr (id)
    if (associated (process)) then
       call var_list_init_num_id (stack%var_list, id, process%get_num_id ())
       if (process%has_integral ()) then
          call var_list_init_process_results (stack%var_list, id, &
               integral = process%get_integral (), &
               error = process%get_error ())
       end if
    else
       call msg_bug ("process_stack_fill_result_vars: unknown process ID")
    end if
  end subroutine process_stack_fill_result_vars

  function process_stack_exists (stack, id) result (flag)
    class(process_stack_t), intent(in) :: stack
    type(string_t), intent(in) :: id
    logical :: flag
    type(process_t), pointer :: process
    process => stack%get_process_ptr (id)
    flag = associated (process)
  end function process_stack_exists

  recursive function process_stack_get_process_ptr (stack, id) result (ptr)
    class(process_stack_t), intent(in) :: stack
    type(string_t), intent(in) :: id
    type(process_t), pointer :: ptr
    type(process_entry_t), pointer :: entry
    ptr => null ()
    entry => stack%first
    do while (associated (entry))
       if (entry%get_id () == id) then
          ptr => entry%process_t
          return
       end if
       entry => entry%next
    end do
    if (associated (stack%next))  ptr => stack%next%get_process_ptr (id)
  end function process_stack_get_process_ptr


  subroutine process_stacks_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (process_stacks_1, "process_stacks_1", &
         "write an empty process stack", &
         u, results)
    call test (process_stacks_2, "process_stacks_2", &
         "fill a process stack", &
         u, results)
    call test (process_stacks_3, "process_stacks_3", &
         "process variables", &
         u, results)
    call test (process_stacks_4, "process_stacks_4", &
         "linked stacks", &
         u, results)
  end subroutine process_stacks_test
  
  subroutine process_stacks_1 (u)
    integer, intent(in) :: u
    type(process_stack_t) :: stack

    write (u, "(A)")  "* Test output: process_stacks_1"
    write (u, "(A)")  "*   Purpose: display an empty process stack"
    write (u, "(A)")

    call stack%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_stacks_1"
    
  end subroutine process_stacks_1
  
  subroutine process_stacks_2 (u)
    integer, intent(in) :: u
    type(process_stack_t) :: stack
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_entry_t), pointer :: process => null ()

    write (u, "(A)")  "* Test output: process_stacks_2"
    write (u, "(A)")  "*   Purpose: fill a process stack"
    write (u, "(A)")

    write (u, "(A)")  "* Build, initialize and store two test processes"
    write (u, "(A)")

    libname = "process_stacks2"
    procname = libname
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    allocate (model)
    call model%init_test ()

    allocate (process)
    run_id = "run1"
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 
    call stack%push (process)
    
    allocate (model)
    call model%init_test ()

    allocate (process)
    run_id = "run2"
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 
    call stack%push (process)
    
    call stack%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call stack%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_stacks_2"
    
  end subroutine process_stacks_2
  
  subroutine process_stacks_3 (u)
    integer, intent(in) :: u
    type(process_stack_t) :: stack
    type(model_data_t), target :: model
    type(string_t) :: procname
    type(process_entry_t), pointer :: process => null ()
    type(process_instance_t), target :: process_instance

    write (u, "(A)")  "* Test output: process_stacks_3"
    write (u, "(A)")  "*   Purpose: setup process variables"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process variables"
    write (u, "(A)")

    procname = "processes_test"
    call model%init_test ()

    write (u, "(A)")  "* Initialize process variables"
    write (u, "(A)")

    call stack%init_var_list ()
    call stack%init_result_vars (procname)
    call stack%write_var_list (u)

    write (u, "(A)")
    write (u, "(A)")  "* Build and integrate a test process"
    write (u, "(A)")

    allocate (process)
    call prepare_test_process (process%process_t, process_instance, model)
    call process%integrate (process_instance, 1, 1, 1000)
    call process_instance%final ()
    call process%final_integration (1)
    call stack%push (process)
    
    write (u, "(A)")  "* Fill process variables"
    write (u, "(A)")

    call stack%fill_result_vars (procname)
    call stack%write_var_list (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call stack%final ()

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_stacks_3"
    
  end subroutine process_stacks_3
  
  subroutine process_stacks_4 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(process_stack_t), target :: stack1, stack2
    class(model_data_t), pointer :: model
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    type(process_entry_t), pointer :: process => null ()

    write (u, "(A)")  "* Test output: process_stacks_4"
    write (u, "(A)")  "*   Purpose: link process stacks"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process variables"
    write (u, "(A)")

    libname = "process_stacks_4_lib"
    procname = "process_stacks_4a"

    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)

    write (u, "(A)")  "* Initialize first process"
    write (u, "(A)")

    call prc_test_create_library (procname, lib)

    allocate (model)
    call model%init_test ()

    allocate (process)
    run_id = "run1"
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 
    call stack1%push (process)
    
    write (u, "(A)")  "* Initialize second process"
    write (u, "(A)")

    call stack2%link (stack1)

    procname = "process_stacks_4b"
    call prc_test_create_library (procname, lib)

    allocate (model)
    call model%init_test ()

    allocate (process)
    run_id = "run2"
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 
    call stack2%push (process)
    
    write (u, "(A)")  "* Show linked stacks"
    write (u, "(A)")

    call stack2%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call stack2%final ()
    call stack1%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_stacks_4"
    
  end subroutine process_stacks_4
  

end module process_stacks
