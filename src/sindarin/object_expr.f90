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

module object_expr

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units
  use diagnostics
  use codes
  use object_base

  implicit none
  private

  public :: statement_t
  public :: assignment_t
  public :: expression_t
  public :: operator_t
  public :: operator_unary_t
  public :: operator_binary_t

  type, extends (composite_t), abstract :: statement_t
     private
   contains
     procedure :: get_prototype => statement_get_prototype
     procedure :: is_statement => statement_is_statement
  end type statement_t
  
  type :: item_t
     private
     class(value_t), pointer :: lhs => null ()
     class(value_t), pointer :: rhs => null ()
     class(item_t), pointer :: next => null ()
  end type item_t
  
  type, extends (statement_t) :: assignment_t
     private
     class(object_t), pointer :: id => null ()
     class(object_t), pointer :: lhs => null ()
     class(item_t), pointer :: item => null ()
   contains
     procedure :: final => assignment_final
     procedure :: write_statement => assignment_write_statement
     procedure :: write_mantle => assignment_write_mantle
     procedure :: write_stack => assignment_write_stack
     procedure :: has_id => assignment_has_id
     procedure :: get_id_ptr => assignment_get_id_ptr
     procedure :: instantiate => assignment_instantiate
     procedure :: get_code => assignment_get_code
     procedure :: init_from_code => assignment_init_from_code
     generic :: init => assignment_init
     procedure, private :: assignment_init
     procedure :: set_path => assignment_set_path
     procedure :: import_id => assignment_import_id
     procedure :: set_lhs => assignment_set_lhs
     procedure :: set_rhs => assignment_set_rhs
     procedure :: push => assignment_push
     procedure :: resolve => assignment_resolve
     procedure :: evaluate => assignment_evaluate
     procedure :: next_position => assignment_next_position
  end type assignment_t
     
  type, extends (composite_t), abstract :: expression_t
     private
   contains
     procedure :: is_expression => expression_is_expression
     procedure :: get_prototype_index => expression_get_prototype_index
     procedure :: get_code => expression_get_code
     procedure :: init_from_code => expression_init_from_code
     procedure (expression_init_args), deferred :: init_args
  end type expression_t
  
  type, extends (expression_t), abstract :: operator_t
     private
   contains
     procedure :: get_prototype => operator_get_prototype
     procedure :: get_opname => operator_get_opname
     procedure :: space_left => operator_space_left
     procedure :: space_right => operator_space_right
     procedure :: init_args => operator_init_args
  end type operator_t
  
  type, extends (operator_t), abstract :: operator_unary_t
     private
   contains
     procedure :: write_expression => operator_unary_write_expression
     procedure :: get_signature => operator_unary_get_signature
  end type operator_unary_t
  
  type, extends (operator_t), abstract :: operator_binary_t
     private
   contains
     procedure :: write_expression => operator_binary_write_expression
     procedure :: get_signature => operator_binary_get_signature
  end type operator_binary_t
  

  abstract interface
     subroutine expression_init_args (object, n_arg, check)
       import
       class(expression_t), intent(inout) :: object
       integer, intent(in) :: n_arg
       logical, intent(in), optional :: check
     end subroutine expression_init_args
  end interface


contains

  function statement_get_prototype (object) result (prototype)
    class(statement_t), intent(in) :: object
    type(string_t) :: prototype
    prototype = "statement"
  end function statement_get_prototype
  
  pure function statement_is_statement (object) result (flag)
    class(statement_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function statement_is_statement
  
  recursive subroutine assignment_final (object)
    class(assignment_t), intent(inout) :: object
    class(item_t), pointer :: item
    if (associated (object%id)) then
       call object%id%final ()
       deallocate (object%id)
    end if
    do while (associated (object%item))
       item => object%item
       object%item => item%next
       deallocate (item)
    end do
    call object%composite_t%final ()
  end subroutine assignment_final
    
  recursive subroutine assignment_write_statement (object, unit, indent)
    class(assignment_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: rhs
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%id)) then
       if (object%id%is_defined ()) then
          call object%id%write_as_expression (unit, indent)
       else
          write (u, "('<LHS>')", advance="no")
       end if
    else
       write (u, "('<LHS>')", advance="no")
    end if
    write (u, "(1x,'=',1x)", advance="no")
    call object%get_member_ptr (1, rhs)
    if (associated (rhs)) then
       call rhs%write_as_expression (unit, indent)
    else
       write (u, "(A)", advance="no") "<???>"
    end if
  end subroutine assignment_write_statement
    
  recursive subroutine assignment_write_mantle (object, unit, indent, refcount)
    class(assignment_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    logical, intent(in), optional :: refcount
    class(object_t), pointer :: rhs
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(A,1x)", advance="no")  "LHS:"
    if (associated (object%id)) then
       if (object%id%is_defined ()) then
          call object%id%write_as_expression (unit, ind+1)
          write (u, *)
       else
          write (u, "('<LHS>')")
       end if
    else
       write (u, *)
    end if
    call write_indent (u, ind)
    write (u, "(A,1x)", advance="no")  "RHS:"
    call object%get_member_ptr (1, rhs)
    if (associated (rhs)) then
       call rhs%write (unit, ind+1)
    else
       write (u, "(A)") "<???>"
    end if
  end subroutine assignment_write_mantle
  
  subroutine assignment_write_stack (object, unit)
    class(assignment_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    class(item_t), pointer :: item
    u = given_output_unit (unit)
    call object%write (u)
    item => object%item
    do while (associated (item))
       write (u, "('+')", advance="no")
       write (u, "(1x,'LHS: ')", advance="no")
       if (associated (item%lhs)) then
          write (u, "(A,1x,'=',1x)", advance="no")  char (item%lhs%get_name ())
          if (item%lhs%is_defined ()) then
             call item%lhs%write_as_expression (u)
          else
             write (u, "('???')", advance="no")
          end if
       else
          write (u, "(1x,'?')", advance="no")
       end if
       write (u, "(2x,'RHS: ')", advance="no")
       if (associated (item%rhs)) then
          write (u, "(A,1x,'=',1x)", advance="no")  char (item%rhs%get_name ())
          if (item%rhs%is_defined ()) then
             call item%rhs%write_expression (u)
          else
             write (u, "('???')", advance="no")
          end if
       else
          write (u, "(1x,'?')", advance="no")
       end if
       write (u, *)
       item => item%next
    end do
  end subroutine assignment_write_stack
  
  pure function assignment_has_id (object) result (flag)
    class(assignment_t), intent(in) :: object
    logical :: flag
    flag = associated (object%id)
  end function assignment_has_id
  
  subroutine assignment_get_id_ptr (object, id)
    class(assignment_t), intent(in) :: object
    class(object_t), pointer, intent(out) :: id
    id => object%id
  end subroutine assignment_get_id_ptr
  
  recursive subroutine assignment_instantiate (object, instance)
    class(assignment_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (assignment_t :: instance)
    select type (instance)
    type is (assignment_t)
       call instance%register (object)
       call instance%init (mode = MODE_CONSTANT)
    end select
  end subroutine assignment_instantiate
    
  function assignment_get_code (object, repository) result (code)
    class(assignment_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    integer :: prototype_index
    type(code_t) :: code
    if (present (repository)) then
       prototype_index = object%get_prototype_index (repository)
    else
       prototype_index = 0
    end if
    call code%set (CAT_COMPOSITE, [prototype_index])
  end function assignment_get_code
  
  subroutine assignment_init_from_code (object, code)
    class(assignment_t), intent(inout) :: object
    type(code_t), intent(in) :: code
  end subroutine assignment_init_from_code

  subroutine assignment_init (object, mode)
    class(assignment_t), intent(inout) :: object
    integer, intent(in), optional :: mode
    call object%init (name=var_str ("assignment"), mode = mode, &
         n_members=1, n_arguments=1)
  end subroutine assignment_init
    
  subroutine assignment_set_path (object, lhs_path)
    class(assignment_t), intent(inout) :: object
    type(string_t), dimension(:), intent(in) :: lhs_path
    allocate (id_t :: object%id)
    select type (id => object%id)
    type is (id_t)
       call id%init (lhs_path)
    end select
  end subroutine assignment_set_path
    
  subroutine assignment_import_id (object, id)
    class(assignment_t), intent(inout) :: object
    class(object_t), intent(inout), pointer :: id
    if (associated (object%id)) then
       call object%id%final ()
       deallocate (object%id)
    end if
    object%id => id
    id => null ()
  end subroutine assignment_import_id
    
  subroutine assignment_set_lhs (object, lhs)
    class(assignment_t), intent(inout) :: object
    class(object_t), intent(in), pointer :: lhs
    object%lhs => lhs
  end subroutine assignment_set_lhs

  subroutine assignment_set_rhs (object, rhs, link)
    class(assignment_t), intent(inout) :: object
    class(object_t), intent(inout), pointer :: rhs
    logical, intent(in) :: link
    if (link) then
       call object%link_member (1, rhs)
    else
       call object%import_member (1, rhs)
    end if
  end subroutine assignment_set_rhs
    
  subroutine assignment_push (object, lhs, rhs)
    class(assignment_t), intent(inout) :: object
    class(value_t), intent(in), pointer :: lhs, rhs
    class(item_t), pointer :: item
    allocate (item)
    item%lhs => lhs
    item%rhs => rhs
    item%next => object%item
    object%item => item
  end subroutine assignment_push
  
  subroutine assignment_resolve (object, success)
    class(assignment_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: lhs, rhs
    logical :: mutable
    if (object%has_id ()) then
       select type (id => object%id)
       type is (id_t)
          call object%find (id%get_path (), object%lhs)
       end select
    end if
    success = associated (object%lhs)
    if (success) then
       select type (lhs => object%lhs)
       type is (composite_t);  call lhs%check_mode (mutable)
          success = mutable
       end select
    end if
    if (success) then
       lhs => object%lhs
       call object%get_member_ptr (1, rhs)
       if (associated (lhs) .and. associated (rhs)) then
          call lhs%match (rhs, success, object)
       else
          success = .false.
       end if
    end if
  end subroutine assignment_resolve
    
  subroutine assignment_evaluate (object)
    class(assignment_t), intent(inout), target :: object
    class(item_t), pointer :: item
    item => object%item
    do while (associated (item))
       call item%lhs%assign (item%rhs)
       item => item%next
    end do
  end subroutine assignment_evaluate
    
  subroutine assignment_next_position &
       (object, position, next_object, import_object)
    class(assignment_t), intent(inout), target :: object
    type(position_t), intent(inout) :: position
    class(object_t), intent(out), pointer, optional :: next_object
    class(object_t), intent(inout), pointer, optional :: import_object
    select case (position%part)
    case (POS_HERE)
       if (object%has_id ()) then
          position%part = POS_ID
          if (present (next_object))  next_object => object%id
       else if (present (import_object)) then
          call object%import_id (import_object)
          position%part = POS_ID
          if (present (next_object))  next_object => object%id
       else
          call composite_next_position &
               (object, position, next_object, import_object)
       end if
    case default
       call composite_next_position &
            (object, position, next_object, import_object)
    end select
  end subroutine assignment_next_position
  
  pure function expression_is_expression (object) result (flag)
    class(expression_t), intent(in) :: object
    logical :: flag
    call object%check_mode (flag)
  end function expression_is_expression
  
  function expression_get_prototype_index (object, repository) result (i)
    class(expression_t), intent(in) :: object
    type(repository_t), intent(in) :: repository
    integer :: i
    call repository%find_member (object%get_name (), index=i)
  end function expression_get_prototype_index

  function expression_get_code (object, repository) result (code)
    class(expression_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call object%get_base_code (code, repository)
  end function expression_get_code
  
  subroutine expression_init_from_code (object, code)
    class(expression_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    call object%set_mode (mode = code%get_att (2))
    call object%init_args (n_arg = code%get_att (5))
    call object%set_intrinsic (intrinsic = code%get_att (3) == 0)
  end subroutine expression_init_from_code

  recursive subroutine operator_unary_write_expression (object, unit, indent)
    class(operator_unary_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: arg
    integer :: u, priority
    u = given_output_unit (unit)
    priority = object%get_priority ()
    write (u, "(A,1x)", advance="no")  char (object%get_opname ())
    call object%get_member_ptr (1, arg)
    if (associated (arg)) then
       arg => arg%dereference ()
       call arg%write_as_expression (unit, indent, priority=priority)
    else
       write (u, "(A)", advance="no") "???"
    end if
  end subroutine operator_unary_write_expression
    
  recursive subroutine operator_binary_write_expression (object, unit, indent)
    class(operator_binary_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, priority, i, n_members
    u = given_output_unit (unit)
    priority = object%get_priority ()
    n_members = object%get_n_members ()
    select case (n_members)
    case (0)
       write (u, "('(',A,')')", advance="no")  char (object%get_opname (0))
    case (1)
       write (u, "('(')", advance="no")
       write (u, "(A)", advance="no")  char (object%get_opname (1))
       if (object%space_right ())  write (u, "(1x)", advance="no")
       call write_member (1)
       write (u, "(')')", advance="no")
    case default
       do i = 1, object%get_n_members ()
          if (i > 1) then
             if (object%space_left ())  write (u, "(1x)", advance="no")
             write (u, "(A)", advance="no")  char (object%get_opname (i))
             if (object%space_right ())  write (u, "(1x)", advance="no")
          end if
          call write_member (i)
       end do
    end select
  contains
    recursive subroutine write_member (i)
      integer, intent(in) :: i
      class(object_t), pointer :: arg
      call object%get_member_ptr (i, arg)
      if (associated (arg)) then
         arg => arg%dereference ()
         call arg%write_as_expression (unit, indent, priority=priority)
      else
         write (u, "(A)", advance="no") "???"
      end if
     end subroutine write_member
  end subroutine operator_binary_write_expression
    
  recursive function operator_get_prototype (object) result (prototype)
    class(operator_t), intent(in) :: object
    type(string_t) :: prototype
    class(object_t), pointer :: core
    call object%get_core_ptr (core)
    if (associated (core)) then
       select type (core)
       class is (composite_t)
          prototype = core%get_prototype ()
       class default
          prototype = core%get_name ()
       end select
    else
       prototype = "operator"
    end if
  end function operator_get_prototype
  
  pure function operator_unary_get_signature (object, verbose) &
       result (signature)
    class(operator_unary_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    signature = object%composite_t%get_signature ()
    if (signature /= "") then
       signature = "operator|unary|" // signature
    else
       signature = "operator|unary"
    end if
  end function operator_unary_get_signature
       
  pure function operator_binary_get_signature (object, verbose) &
       result (signature)
    class(operator_binary_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    signature = object%composite_t%get_signature ()
    if (signature /= "") then
       signature = "operator|binary|" // signature
    else
       signature = "operator|binary"
    end if
  end function operator_binary_get_signature
       
  pure function operator_get_opname (object, i) result (name)
    class(operator_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    name = object%get_name ()
  end function operator_get_opname
    
  function operator_space_left (object) result (flag)
    class(operator_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function operator_space_left
  
  function operator_space_right (object) result (flag)
    class(operator_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function operator_space_right
  
  subroutine operator_init_args (object, n_arg, check)
    class(operator_t), intent(inout) :: object
    integer, intent(in) :: n_arg
    logical, intent(in), optional :: check
    logical :: check_args
    check_args = .true.;  if (present (check))  check_args = check
    if (check_args) then
       select type (object)
       class is (operator_unary_t)
          if (n_arg /= 1) then
             call object%write ()
             call msg_bug ("Unary operator: number of arguments must be one")
          end if
       class is (operator_binary_t)
          if (n_arg < 2) then
             call object%write ()
             call msg_bug ("Binary operator: number of arguments less than two")
          end if
       end select
    end if
    call object%init_members (n_members = n_arg, n_arguments = n_arg)
  end subroutine operator_init_args


end module object_expr
