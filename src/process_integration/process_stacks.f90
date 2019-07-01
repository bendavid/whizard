! WHIZARD 2.7.0 Jan 21 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use diagnostics
  use os_interface
  use sm_qcd
  use model_data
  use rng_base
  use variables
  use observables
  use process_libraries
  use process

  implicit none
  private

  public :: process_entry_t
  public :: process_stack_t

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
     procedure :: pop_last => process_stack_pop_last
     procedure :: init_result_vars => process_stack_init_result_vars
     procedure :: fill_result_vars => process_stack_fill_result_vars
     procedure :: update_result_vars => process_stack_update_result_vars
     procedure :: exists => process_stack_exists
     procedure :: get_process_ptr => process_stack_get_process_ptr
  end type process_stack_t


contains

  subroutine process_stack_clear (stack)
    class(process_stack_t), intent(inout) :: stack
    type(process_entry_t), pointer :: process
    if (associated (stack%var_list)) then
       call stack%var_list%final ()
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

  recursive subroutine process_stack_show (object, unit, fifo)
    class(process_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: fifo
    type(process_entry_t), pointer :: process
    logical :: reverse
    integer :: u, i, j
    u = given_output_unit (unit)
    reverse = .false.;  if (present (fifo))  reverse = fifo
    select case (object%n)
    case (0)
    case default
       if (.not. reverse) then
          process => object%first
          do while (associated (process))
             call process%show (u, verbose=.false.)
             process => process%next
          end do 
       else
          do i = 1, object%n
             process => object%first
             do j = 1, object%n - i
                process => process%next
             end do
             call process%show (u, verbose=.false.)
          end do
       end if
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
    if (present (var_list))  call var_list%link (stack%var_list)
  end subroutine process_stack_init_var_list

  subroutine process_stack_link_var_list (stack, var_list)
    class(process_stack_t), intent(inout) :: stack
    type(var_list_t), intent(in), target :: var_list
    call stack%var_list%link (var_list)
  end subroutine process_stack_link_var_list

  subroutine process_stack_push (stack, process)
    class(process_stack_t), intent(inout) :: stack
    type(process_entry_t), intent(inout), pointer :: process
    process%next => stack%first
    stack%first => process
    process => null ()
    stack%n = stack%n + 1
  end subroutine process_stack_push

  subroutine process_stack_pop_last (stack, process)
    class(process_stack_t), intent(inout) :: stack
    type(process_entry_t), intent(inout), pointer :: process
    type(process_entry_t), pointer :: previous
    integer :: i
    select case (stack%n)
    case (:0)
       process => null ()
    case (1)
       process => stack%first
       stack%first => null ()
       stack%n = 0
    case (2:)
       process => stack%first
       do i = 2, stack%n
          previous => process
          process => process%next
       end do
       previous%next => null ()
       stack%n = stack%n - 1
    end select
  end subroutine process_stack_pop_last
    
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

  subroutine process_stack_update_result_vars (stack, id, var_list_local)
    class(process_stack_t), intent(inout) :: stack
    type(string_t), intent(in) :: id
    type(var_list_t), intent(inout) :: var_list_local
    call update ("integral(" // id // ")")
    call update ("error(" // id // ")")
  contains
    subroutine update (var_name)
      type(string_t), intent(in) :: var_name
      real(default) :: value
      if (var_list_local%contains (var_name, follow_link = .false.)) then
         value = stack%var_list%get_rval (var_name)
         call var_list_local%set_real (var_name, value, is_known = .true.)
      end if
    end subroutine update
  end subroutine process_stack_update_result_vars

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


end module process_stacks
