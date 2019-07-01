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

module object_container

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units
  use diagnostics

  use codes
  use object_base
  use object_builder
  use object_expr
  use object_logical
  use object_integer

  implicit none
  private

  public :: container_t

  type, extends (operator_binary_t) :: container_t
     private
     integer :: ctype = 0
     integer :: stype = 0
   contains
     procedure :: write_core => container_write_core
     procedure :: write_value => container_write_value
     procedure :: get_priority => container_get_priority
     procedure :: get_prototype => container_get_prototype
     procedure :: show_opname => container_show_opname
     procedure :: get_opname => container_get_opname
     procedure :: space_left => container_space_left
     procedure :: space_right => container_space_right
     procedure :: instantiate => container_instantiate
     generic :: init => container_init
     procedure, private :: container_init
     procedure :: setup_range => container_setup_range
     procedure :: get_code => container_get_code
     procedure :: init_from_code => container_init_from_code
     procedure :: resolve => container_resolve
end type container_t


contains

  recursive subroutine container_write_core (object, unit, indent)
    class(container_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    class(object_t), pointer :: core
    call object%get_core_ptr (core)
    call core%write_as_expression (unit, indent)
  end subroutine container_write_core
       
  recursive subroutine container_write_value (object, unit, indent)
    class(container_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    call object%write_expression (unit, indent)
  end subroutine container_write_value
    
  pure function container_get_priority (object) result (priority)
    class(container_t), intent(in) :: object
    integer :: priority
    select case (object%ctype)
    case (CT_TUPLE);    priority = PRIO_COLON
    case (CT_LIST);     priority = PRIO_COMMA
    case (CT_SEQUENCE); priority = PRIO_ARROW
    case default
       priority = 0
    end select
  end function container_get_priority
  
  recursive function container_get_prototype (object) result (prototype)
    class(container_t), intent(in) :: object
    type(string_t) :: prototype
    class(object_t), pointer :: core
    call object%get_core_ptr (core)
    if (associated (core)) then
       prototype = "container"
    else
       prototype = "object"
    end if
  end function container_get_prototype
  
  pure function container_show_opname (object, i) result (flag)
    class(container_t), intent(in) :: object
    integer, intent(in), optional :: i
    logical :: flag
    if (present (i)) then
       select case (object%stype)
       case (0)
          flag = i == 1
       case default
          flag = i == 2 .or. i == 3
       end select
    else
       flag = .false.
    end if
  end function container_show_opname

  pure function container_get_opname (object, i) result (name)
    class(container_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    select case (object%stype)
    case (0)
       select case (object%ctype)
       case (CT_TUPLE);  name = ":"
       case (CT_LIST);  name = ","
       case (CT_SEQUENCE);  name = "=>"
       case default
          name = "?"
       end select
    case default
       if (i == 2) then
          name = "=>"
       else if (i == 3) then
          select case (object%stype)
          case (CT_ADD);  name = "/+"
          case (CT_SUB);  name = "/-"
          case (CT_MUL);  name = "/*"
          case (CT_DIV);  name = "//"
          case (CT_LIN);  name = "+/+"
          case (CT_LOG);  name = "*/*"
          case default
             name = "?"
          end select
       else
          name = "?"
       end if
    end select
  end function container_get_opname

  function container_space_left (object) result (flag)
    class(container_t), intent(in) :: object
    logical :: flag
    select case (object%ctype)
    case (CT_TUPLE);    flag = .false.
    case (CT_LIST);     flag = .false.
    case (CT_SEQUENCE); flag = .true.
    case default
       flag = .false.
    end select
  end function container_space_left
  
  function container_space_right (object) result (flag)
    class(container_t), intent(in) :: object
    logical :: flag
    select case (object%ctype)
    case (CT_TUPLE);    flag = .false.
    case (CT_LIST);     flag = .true.
    case (CT_SEQUENCE); flag = .true.
    case default
       flag = .true.
    end select
  end function container_space_right
  
  subroutine container_instantiate (object, instance)
    class(container_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (container_t :: instance)
  end subroutine container_instantiate
  
  subroutine container_init (object, ctype, name, mode)
    class(container_t), intent(inout) :: object
    integer, intent(in) :: ctype
    type(string_t), intent(in), optional :: name
    integer, intent(in), optional :: mode
    if (present (name)) then
       call object%composite_t%init (name, mode)
    else
       call object%composite_t%init (var_str ("container"), mode)
    end if
    object%ctype = ctype
  end subroutine container_init
  
  subroutine container_setup_range (object, stype)
    class(container_t), intent(inout) :: object
    integer, intent(in) :: stype
    object%stype = stype
  end subroutine container_setup_range
  
  function container_get_code (object, repository) result (code)
    class(container_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call object%get_base_code (code, repository)
    if (object%stype /= 0) then
       call code%create_integer_val ([object%stype])
    else
       call code%create_integer_val ([object%ctype])
    end if
  end function container_get_code
  
  subroutine container_init_from_code (object, code)
    class(container_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    logical :: success
    integer :: t
    call code%get_integer (t, success)
    if (.not. success)  call msg_bug &
         ("Sindarin: error in byte code for container operator")
    select case (t)
    case (CT_TUPLE, CT_LIST, CT_SEQUENCE)
       call object%init (t, mode = code%get_att (2))
    case (CT_ADD, CT_SUB, CT_MUL, CT_DIV, CT_LIN, CT_LOG)
       call object%init (CT_SEQUENCE, mode = code%get_att (2))
       call object%setup_range (t)
    case default
       call msg_bug ("Sindarin decoder: container: unknown separator code")
    end select
    call object%init_args (n_arg = code%get_att (5))
    call object%set_intrinsic (intrinsic = code%get_att (3) == 0)
  end subroutine container_init_from_code

  recursive subroutine container_resolve (object, success)
    class(container_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: item, core
    integer :: i, n_args
    success = .false.
    n_args = object%get_n_members ()
    call object%get_core_ptr (core)
    call core%resolve (success);  if (.not. success)  return
    select type (core)
    type is (ref_array_t)
       do i = 1, n_args
          call object%get_member_ptr (i, item)
          call item%resolve (success);  if (.not. success)  return
          call core%associate (i, item)
       end do
    end select
  end subroutine container_resolve
  

end module object_container
