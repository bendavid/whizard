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

module object_comparison

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

  public :: compare_t

  type, extends (operator_binary_t) :: compare_t
     private
     type(logical_t), pointer :: res => null ()
     type(integer_p), dimension(:), allocatable :: arg_ptr
     integer, dimension(:), allocatable :: cmp_code
   contains
     procedure :: get_priority => compare_get_priority
     procedure :: show_opname => compare_show_opname
     procedure :: get_opname => compare_get_opname
     procedure :: instantiate => compare_instantiate
     generic :: init => compare_init
     procedure, private :: compare_init
     procedure :: init_members => compare_init_members
     procedure :: set_cmp_code => compare_set_cmp_code
     procedure :: get_code => compare_get_code
     procedure :: init_from_code => compare_init_from_code
     procedure :: resolve => compare_resolve
     procedure :: evaluate => compare_evaluate
end type compare_t


contains

  pure function compare_get_priority (object) result (priority)
    class(compare_t), intent(in) :: object
    integer :: priority
    priority = PRIO_COMPARE
  end function compare_get_priority
  
  pure function compare_show_opname (object, i) result (flag)
    class(compare_t), intent(in) :: object
    integer, intent(in), optional :: i
    logical :: flag
    if (present (i)) then
       flag = i > 1
    else
       flag = .false.
    end if
  end function compare_show_opname

  pure function compare_get_opname (object, i) result (name)
    class(compare_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    select case (object%cmp_code(i))
    case (CMP_EQ);  name = "=="
    case (CMP_NE);  name = "<>"
    case (CMP_LT);  name = "<"
    case (CMP_GT);  name = ">"
    case (CMP_LE);  name = "<="
    case (CMP_GE);  name = ">="
    end select
  end function compare_get_opname

  subroutine compare_instantiate (object, instance)
    class(compare_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (compare_t :: instance)
    select type (instance)
    type is (compare_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine compare_instantiate
  
  subroutine compare_init (object, prototype, mode)
    class(compare_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    class(object_t), pointer :: value, core
    call object%composite_t%init (var_str ("compare"), mode)
    select type (prototype)
    class is (composite_t)
       call prototype%get_core_ptr (value)
       call object%set_default_prototype (prototype)
    end select
    call value%instantiate (core)
    call object%import_core (core)
  end subroutine compare_init
  
  subroutine compare_init_members (object, n_members, n_arguments)
    class(compare_t), intent(inout), target :: object
    integer, intent(in) :: n_members
    integer, intent(in), optional :: n_arguments
    call object%composite_t%init_members (n_members, n_arguments)
    allocate (object%cmp_code (n_members), source = CMP_NONE)
  end subroutine compare_init_members

  subroutine compare_set_cmp_code (object, i, cmp_code)
    class(compare_t), intent(inout) :: object
    integer, intent(in) :: i
    integer, intent(in) :: cmp_code
    object%cmp_code(i) = cmp_code
  end subroutine compare_set_cmp_code
  
  function compare_get_code (object, repository) result (code)
    class(compare_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call object%get_base_code (code, repository)
    call code%create_integer_val (object%cmp_code)
  end function compare_get_code
  
  subroutine compare_init_from_code (object, code)
    class(compare_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    logical :: success
    call object%set_mode (mode = code%get_att (2))
    call object%init_args (n_arg = code%get_att (5))
    call object%set_intrinsic (intrinsic = code%get_att (3) == 0)
    call code%get_integer_array (object%cmp_code, success)
    if (.not. success)  call msg_bug &
         ("Sindarin: error in byte code for comparison operator")
  end subroutine compare_init_from_code

  recursive subroutine compare_resolve (object, success)
    class(compare_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: arg, core
    integer :: i, n_args
    success = .false.
    object%res => null ()
    n_args = object%get_n_members ()
    call object%get_core_ptr (core)
    call core%resolve (success);  if (.not. success)  return
    select type (core)
    type is (logical_t)
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
  end subroutine compare_resolve
  
  recursive subroutine compare_evaluate (object)
    class(compare_t), intent(inout), target :: object
    integer :: i
    integer :: lhs, rhs
    logical :: passed
    call object%composite_t%evaluate ()
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%is_defined ()) then
          call object%res%init ()
          return
       end if
    end do
    lhs = object%arg_ptr(1)%get_value ()
    do i = 2, size (object%arg_ptr)
       rhs = object%arg_ptr(i)%get_value ()
       select case (object%cmp_code(i))
       case (CMP_EQ);  passed = lhs == rhs
       case (CMP_NE);  passed = lhs /= rhs
       case (CMP_LT);  passed = lhs <  rhs
       case (CMP_GT);  passed = lhs >  rhs
       case (CMP_LE);  passed = lhs <= rhs
       case (CMP_GE);  passed = lhs >= rhs
       end select
       if (.not. passed) then
          call object%res%init (.false.)
          return
       end if
       lhs = rhs
    end do
    call object%res%init (.true.)
  end subroutine compare_evaluate
  

end module object_comparison
