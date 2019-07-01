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

module object_integer

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units
  use diagnostics

  use codes
  use object_base
  use object_builder
  use object_expr
  use object_logical

  implicit none
  private

  public :: integer_t
  public :: integer_p
  public :: minus_t
  public :: integer_binary_t
  public :: multiply_t
  public :: add_t

  type, extends (value_t) :: integer_t
     private
     integer :: value = 0
   contains
     procedure :: final => integer_final
     procedure :: write_expression => integer_write_value
     procedure :: write_value => integer_write_value
     procedure :: get_name => integer_get_name
     procedure :: instantiate => integer_instantiate
     procedure :: get_code => integer_get_code
     procedure :: init_from_code => integer_init_from_code
     procedure :: init => integer_init
     procedure :: match_value => integer_match_value
     procedure :: assign_value => integer_assign_value
  end type integer_t
  
  type :: integer_p
     private
     type(integer_t), pointer :: p => null ()
   contains
     procedure :: associate => integer_p_associate
     procedure :: is_defined => integer_p_is_defined
     procedure :: get_value => integer_p_get_value
  end type integer_p
     
  type, extends (operator_unary_t), abstract :: integer_unary_t
     private
     type(integer_t), pointer :: res => null ()
     type(integer_t), pointer :: arg => null ()
   contains
     generic :: init => integer_unary_init
     procedure, private :: integer_unary_init
     procedure :: resolve => integer_unary_resolve
  end type integer_unary_t

  type, extends (integer_unary_t) :: minus_t
     private
   contains
     procedure :: write_expression => minus_write_expression
     procedure :: get_priority => minus_get_priority
     procedure :: get_opname => minus_get_opname
     procedure :: instantiate => minus_instantiate
     generic :: init => minus_init
     procedure, private :: minus_init
     procedure :: evaluate => minus_evaluate
  end type minus_t
  
  type, extends (operator_binary_t), abstract :: integer_binary_t
     private
     type(integer_t), pointer :: res => null ()
     type(integer_p), dimension(:), allocatable :: arg_ptr
     logical, dimension(:), allocatable :: inverse
   contains
     procedure :: write_expression => integer_binary_write_expression
     procedure :: show_opname => integer_binary_show_opname
     generic :: init => integer_binary_init
     procedure, private :: integer_binary_init
     procedure :: init_members => integer_binary_init_members
     procedure :: tag_inverse => integer_binary_tag_inverse
     procedure :: get_code => integer_binary_get_code
     procedure :: init_from_code => integer_binary_init_from_code
     procedure :: resolve => integer_binary_resolve
  end type integer_binary_t

  type, extends (integer_binary_t) :: multiply_t
     private
   contains
     procedure :: get_priority => multiply_get_priority
     procedure :: get_opname => multiply_get_opname
     procedure :: instantiate => multiply_instantiate
     generic :: init => multiply_init
     procedure, private :: multiply_init
     procedure :: evaluate => multiply_evaluate
  end type multiply_t
  
  type, extends (integer_binary_t) :: add_t
     private
   contains
     procedure :: get_priority => add_get_priority
     procedure :: get_opname => add_get_opname
     procedure :: instantiate => add_instantiate
     generic :: init => add_init
     procedure, private :: add_init
     procedure :: evaluate => add_evaluate
  end type add_t
  

contains

  subroutine integer_p_associate (object, target_object)
    class(integer_p), intent(inout) :: object
    type(integer_t), intent(in), target :: target_object
    object%p => target_object
  end subroutine integer_p_associate
  
  pure function integer_p_is_defined (object) result (flag)
    class(integer_p), intent(in) :: object
    logical :: flag
    if (associated (object%p)) then
       flag = object%p%is_defined ()
    else
       flag = .false.
    end if
  end function integer_p_is_defined
  
  pure function integer_p_get_value (object) result (value)
    class(integer_p), intent(in) :: object
    integer :: value
    value = object%p%value
  end function integer_p_get_value
  
  subroutine integer_final (object)
    class(integer_t), intent(inout) :: object
  end subroutine integer_final
 
  subroutine integer_write_value (object, unit, indent)
    class(integer_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u
    u = given_output_unit (unit)
    write (u, "(I0)", advance="no")  object%value
  end subroutine integer_write_value
       
  pure function integer_get_name (object) result (name)
    class(integer_t), intent(in) :: object
    type(string_t) :: name
    name = "integer"
  end function integer_get_name
  
  subroutine integer_instantiate (object, instance)
    class(integer_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (integer_t :: instance)
  end subroutine integer_instantiate
    
  function integer_get_code (object, repository) result (code)
    class(integer_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (CAT_VALUE)
    if (object%is_defined ()) then
       call code%create_integer_val ([object%value])
    end if
  end function integer_get_code
  
  subroutine integer_init_from_code (object, code)
    class(integer_t), intent(out) :: object
    type(code_t), intent(in) :: code
    integer :: value
    logical :: is_defined
    call code%get_integer (value, is_defined)
    if (is_defined) then
       call object%init (value)
    else
       call object%init ()
    end if
  end subroutine integer_init_from_code
    
  pure subroutine integer_init (object, value)
    class(integer_t), intent(inout) :: object
    integer, intent(in), optional :: value
    if (present (value)) then
       object%value = value
       call object%set_defined (.true.)
    else
       call object%set_defined (.false.)
    end if
  end subroutine integer_init
 
  subroutine integer_match_value (object, source, success)
    class(integer_t), intent(in) :: object
    class(value_t), intent(in) :: source
    logical, intent(out) :: success
    select type (source)
    class is (integer_t)
       success = .true.
    class default
       success = .false.
    end select
  end subroutine integer_match_value
       
  subroutine integer_assign_value (object, source)
    class(integer_t), intent(inout) :: object
    class(value_t), intent(in) :: source
    select type (source)
    class is (integer_t)
       object%value = source%value
    end select
  end subroutine integer_assign_value
       
  recursive subroutine minus_write_expression (object, unit, indent)
    class(minus_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: arg
    integer :: u, priority
    u = given_output_unit (unit)
    priority = object%get_priority ()
    write (u, "(A)", advance="no")  char (object%get_opname ())
    call object%get_member_ptr (1, arg)
    if (associated (arg)) then
       arg => arg%dereference ()
       if (arg%is_expression ()) then
          call arg%write_as_expression (unit, indent, priority=priority)
       else if (arg%has_literal ()) then
          write (u, "('(')", advance="no")
          call arg%write_as_expression (u, indent)
          write (u, "(')')", advance="no")
       else
          call arg%write_as_expression (u, indent)
       end if
    else
       write (u, "(A)", advance="no") "???"
    end if
  end subroutine minus_write_expression

  pure function minus_get_priority (object) result (priority)
    class(minus_t), intent(in) :: object
    integer :: priority
    priority = PRIO_MINUS
  end function minus_get_priority
  
  pure function minus_get_opname (object, i) result (name)
    class(minus_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    name = "-"
  end function minus_get_opname
    
  subroutine minus_instantiate (object, instance)
    class(minus_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (minus_t :: instance)
    select type (instance)
    class is (minus_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine minus_instantiate
  
  subroutine integer_unary_init (object, prototype, name, mode)
    class(integer_unary_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: mode
    class(object_t), pointer :: value, core
    call object%composite_t%init (name, mode = mode)
    select type (prototype)
    class is (composite_t)
       call prototype%get_core_ptr (value)
       call object%set_default_prototype (prototype)
    end select
    call value%instantiate (core)
    call object%import_core (core)
  end subroutine integer_unary_init
  
  subroutine minus_init (object, prototype, mode)
    class(minus_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call integer_unary_init (object, prototype, var_str ("minus"), mode)
  end subroutine minus_init
  
  recursive subroutine integer_unary_resolve (object, success)
    class(integer_unary_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: arg, core
    success = .false.
    object%res => null ()
    object%arg => null ()
    call object%get_core_ptr (core)
    call core%resolve (success);  if (.not. success)  return
    select type (core)
    type is (integer_t)
       object%res => core
    class default
       return
    end select
    if (object%has_value ()) then
       call object%get_member_ptr (1, arg)
       call arg%resolve (success);  if (.not. success)  return
       select type (arg)
       class is (wrapper_t)
          call arg%get_core_ptr (core)
          select type (core)
          type is (integer_t)
             object%arg => core
             success = .true.
          end select
       end select
    end if
  end subroutine integer_unary_resolve
  
  recursive subroutine minus_evaluate (object)
    class(minus_t), intent(inout), target :: object
    call object%composite_t%evaluate ()
    if (object%arg%is_defined ()) then
       call object%res%init (- object%arg%value)
    else
       call object%res%init ()
    end if
  end subroutine minus_evaluate
  
  recursive subroutine integer_binary_write_expression (object, unit, indent)
    class(integer_binary_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: arg, core
    integer :: u, priority, i
    logical :: paren
    u = given_output_unit (unit)
    priority = object%get_priority ()
    do i = 1, object%get_n_members ()
       if (i > 1) then
          write (u, "(1x,A,1x)", advance="no")  char (object%get_opname (i))
       end if
       call object%get_member_ptr (i, arg)
       if (associated (arg)) then
          paren = .false.
          arg => arg%dereference ()
          select type (arg)
          class is (minus_t)
             paren = i > 1
          class is (composite_t)
             call arg%get_core_ptr (core)
             select type (core)
             type is (integer_t);  paren = core%value < 0
             end select
          end select
          if (paren)  write (u, "('(')", advance="no")
          call arg%write_as_expression (unit, indent, priority=priority)
          if (paren)  write (u, "(')')", advance="no")
       else
          write (u, "(A)", advance="no") "???"
       end if
    end do
  end subroutine integer_binary_write_expression

  pure function multiply_get_priority (object) result (priority)
    class(multiply_t), intent(in) :: object
    integer :: priority
    priority = PRIO_MULTIPLY
  end function multiply_get_priority
  
  pure function add_get_priority (object) result (priority)
    class(add_t), intent(in) :: object
    integer :: priority
    priority = PRIO_ADD
  end function add_get_priority
  
  pure function integer_binary_show_opname (object, i) result (flag)
    class(integer_binary_t), intent(in) :: object
    integer, intent(in), optional :: i
    logical :: flag
    if (present (i)) then
       flag = i > 1
    else
       flag = .false.
    end if
  end function integer_binary_show_opname
  
  pure function multiply_get_opname (object, i) result (name)
    class(multiply_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    if (object%inverse(i)) then
       name = "/"
    else
       name = "*"
    end if
  end function multiply_get_opname
    
  pure function add_get_opname (object, i) result (name)
    class(add_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    if (object%inverse(i)) then
       name = "-"
    else
       name = "+"
    end if
  end function add_get_opname
    
  subroutine multiply_instantiate (object, instance)
    class(multiply_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (multiply_t :: instance)
    select type (instance)
    type is (multiply_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine multiply_instantiate
  
  subroutine add_instantiate (object, instance)
    class(add_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (add_t :: instance)
    select type (instance)
    type is (add_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine add_instantiate
  
  subroutine integer_binary_init (object, prototype, name, mode)
    class(integer_binary_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: mode
    class(object_t), pointer :: value, core
    call object%composite_t%init (name, mode)
    select type (prototype)
    class is (composite_t)
       call prototype%get_core_ptr (value)
       call object%set_default_prototype (prototype)
    end select
    call value%instantiate (core)
    call object%import_core (core)
  end subroutine integer_binary_init
  
  subroutine integer_binary_init_members (object, n_members, n_arguments)
    class(integer_binary_t), intent(inout), target :: object
    integer, intent(in) :: n_members
    integer, intent(in), optional :: n_arguments
    call object%composite_t%init_members (n_members, n_arguments)
    allocate (object%inverse (n_members), source = .false.)
  end subroutine integer_binary_init_members

  subroutine integer_binary_tag_inverse (object, i, inverse)
    class(integer_binary_t), intent(inout) :: object
    integer, intent(in) :: i
    logical, intent(in) :: inverse
    object%inverse(i) = inverse
  end subroutine integer_binary_tag_inverse
  
  subroutine multiply_init (object, prototype, mode)
    class(multiply_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call integer_binary_init (object, prototype, var_str ("multiply"), mode)
  end subroutine multiply_init
  
  subroutine add_init (object, prototype, mode)
    class(add_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call integer_binary_init (object, prototype, var_str ("add"), mode)
  end subroutine add_init
  
  function integer_binary_get_code (object, repository) result (code)
    class(integer_binary_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call object%get_base_code (code, repository)
    call code%create_logical_val (object%inverse)
  end function integer_binary_get_code
  
  subroutine integer_binary_init_from_code (object, code)
    class(integer_binary_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    logical :: success
    call object%set_mode (mode = code%get_att (2))
    call object%init_args (n_arg = code%get_att (5))
    call object%set_intrinsic (intrinsic = code%get_att (3) == 0)
    call code%get_logical_array (object%inverse, success)
    if (.not. success)  call msg_bug &
         ("Sindarin: error in byte code for integer binary operator")
  end subroutine integer_binary_init_from_code

  recursive subroutine integer_binary_resolve (object, success)
    class(integer_binary_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: arg, core
    integer :: i, n_args
    success = .false.
    object%res => null ()
    n_args = object%get_n_members ()
    call object%get_core_ptr (core)
    call core%resolve (success);  if (.not. success)  return
    select type (core)
    type is (integer_t)
       object%res => core
    class default
       return
    end select
    if (.not. allocated (object%arg_ptr)) &
         allocate (object%arg_ptr (n_args))
    if (object%has_value ()) then
       do i = 1, n_args
          call object%get_member_ptr (i, arg)
          call arg%resolve (success);  if (.not. success)  return
          select type (arg)
          class is (wrapper_t)
             call arg%get_core_ptr (core)
             select type (core)
             type is (integer_t)
                call object%arg_ptr(i)%associate (core)
                success = .true.
             end select
          end select
       end do
    end if
  end subroutine integer_binary_resolve
  
  recursive subroutine multiply_evaluate (object)
    class(multiply_t), intent(inout), target :: object
    integer :: i
    integer :: val, res
    call object%composite_t%evaluate ()
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%is_defined ()) then
          call object%res%init ()
          return
       end if
    end do
    res = object%arg_ptr(1)%get_value ()
    do i = 2, size (object%arg_ptr)
       val = object%arg_ptr(i)%get_value ()
       if (object%inverse(i)) then
          if (val == 0) then
             call object%res%init ()
             return
          else
             res = res / val
          end if
       else
          res = res * val
       end if
    end do
    call object%res%init (res)
  end subroutine multiply_evaluate
  
  recursive subroutine add_evaluate (object)
    class(add_t), intent(inout), target :: object
    integer :: i
    integer :: val, res
    call object%composite_t%evaluate ()
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%is_defined ()) then
          call object%res%init ()
          return
       end if
    end do
    res = object%arg_ptr(1)%get_value ()
    do i = 2, size (object%arg_ptr)
       val = object%arg_ptr(i)%get_value ()
       if (object%inverse(i)) then
          res = res - val
       else
          res = res + val
       end if
    end do
    call object%res%init (res)
  end subroutine add_evaluate
  

end module object_integer
