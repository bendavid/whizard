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

module object_logical

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units

  use codes
  use object_base
  use object_builder
  use object_expr

  implicit none
  private

  public :: logical_t
  public :: logical_p
  public :: not_t
  public :: and_t
  public :: or_t

  type, extends (value_t) :: logical_t
     private
     logical :: value = .false.
   contains
     procedure :: final => logical_final
     procedure :: write_expression => logical_write_value
     procedure :: write_value => logical_write_value
     procedure :: get_name => logical_get_name
     procedure :: instantiate => logical_instantiate
     procedure :: get_code => logical_get_code
     procedure :: init_from_code => logical_init_from_code
     procedure :: init => logical_init
     procedure :: match_value => logical_match_value
     procedure :: assign_value => logical_assign_value
  end type logical_t
  
  type :: logical_p
     private
     type(logical_t), pointer :: p => null ()
   contains
     procedure :: associate => logical_p_associate
     procedure :: is_defined => logical_p_is_defined
     procedure :: get_value => logical_p_get_value
  end type logical_p
     
  type, extends (operator_unary_t), abstract :: logical_unary_t
     private
     type(logical_t), pointer :: res => null ()
     type(logical_t), pointer :: arg => null ()
   contains
     generic :: init => logical_unary_init
     procedure, private :: logical_unary_init
     procedure :: resolve => logical_unary_resolve
  end type logical_unary_t

  type, extends (logical_unary_t) :: not_t
     private
   contains
     procedure :: get_priority => not_get_priority
     procedure :: instantiate => not_instantiate
     generic :: init => not_init
     procedure, private :: not_init
     procedure :: evaluate => not_evaluate
  end type not_t
  
  type, extends (operator_binary_t), abstract :: logical_binary_t
     private
     type(logical_t), pointer :: res => null ()
     type(logical_p), dimension(:), allocatable :: arg_ptr
   contains
     generic :: init => logical_binary_init
     procedure, private :: logical_binary_init
     procedure :: resolve => logical_binary_resolve
  end type logical_binary_t

  type, extends (logical_binary_t) :: and_t
     private
   contains
     procedure :: get_priority => and_get_priority
     procedure :: instantiate => and_instantiate
     generic :: init => and_init
     procedure, private :: and_init
     procedure :: evaluate => and_evaluate
  end type and_t
  
  type, extends (logical_binary_t) :: or_t
     private
   contains
     procedure :: get_priority => or_get_priority
     procedure :: instantiate => or_instantiate
     generic :: init => or_init
     procedure, private :: or_init
     procedure :: evaluate => or_evaluate
  end type or_t
  

contains

  subroutine logical_p_associate (object, target_object)
    class(logical_p), intent(inout) :: object
    type(logical_t), intent(in), target :: target_object
    object%p => target_object
  end subroutine logical_p_associate
  
  pure function logical_p_is_defined (object) result (flag)
    class(logical_p), intent(in) :: object
    logical :: flag
    if (associated (object%p)) then
       flag = object%p%is_defined ()
    else
       flag = .false.
    end if
  end function logical_p_is_defined
  
  pure function logical_p_get_value (object) result (value)
    class(logical_p), intent(in) :: object
    logical :: value
    value = object%p%value
  end function logical_p_get_value
  
  subroutine logical_final (object)
    class(logical_t), intent(inout) :: object
  end subroutine logical_final
 
  subroutine logical_write_value (object, unit, indent)
    class(logical_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u
    u = given_output_unit (unit)
    if (object%value) then
       write (u, "(A)", advance="no")  "true"
    else
       write (u, "(A)", advance="no")  "false"
    end if
  end subroutine logical_write_value
       
  pure function logical_get_name (object) result (name)
    class(logical_t), intent(in) :: object
    type(string_t) :: name
    name = "logical"
  end function logical_get_name
  
  subroutine logical_instantiate (object, instance)
    class(logical_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (logical_t :: instance)
  end subroutine logical_instantiate
    
  function logical_get_code (object, repository) result (code)
    class(logical_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (CAT_VALUE)
    if (object%is_defined ()) then
       call code%create_logical_val ([object%value])
    end if
  end function logical_get_code
  
  subroutine logical_init_from_code (object, code)
    class(logical_t), intent(out) :: object
    type(code_t), intent(in) :: code
    logical :: value, is_defined
    call code%get_logical (value, is_defined)
    if (is_defined) then
       call object%init (value)
    else
       call object%init ()
    end if
  end subroutine logical_init_from_code
    
  pure subroutine logical_init (object, value)
    class(logical_t), intent(inout) :: object
    logical, intent(in), optional :: value
    if (present (value)) then
       object%value = value
       call object%set_defined (.true.)
    else
       call object%set_defined (.false.)
    end if
  end subroutine logical_init
 
  subroutine logical_match_value (object, source, success)
    class(logical_t), intent(in) :: object
    class(value_t), intent(in) :: source
    logical, intent(out) :: success
    select type (source)
    class is (logical_t)
       success = .true.
    class default
       success = .false.
    end select
  end subroutine logical_match_value
       
  subroutine logical_assign_value (object, source)
    class(logical_t), intent(inout) :: object
    class(value_t), intent(in) :: source
    select type (source)
    class is (logical_t)
       object%value = source%value
    end select
  end subroutine logical_assign_value
       
  pure function not_get_priority (object) result (priority)
    class(not_t), intent(in) :: object
    integer :: priority
    priority = PRIO_NOT
  end function not_get_priority
  
  subroutine not_instantiate (object, instance)
    class(not_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (not_t :: instance)
    select type (instance)
    class is (not_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine not_instantiate
  
  subroutine logical_unary_init (object, prototype, name, mode)
    class(logical_unary_t), intent(inout) :: object
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
  end subroutine logical_unary_init
  
  subroutine not_init (object, prototype, mode)
    class(not_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call logical_unary_init (object, prototype, var_str ("not"), mode)
  end subroutine not_init
  
  recursive subroutine logical_unary_resolve (object, success)
    class(logical_unary_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: arg, core
    success = .false.
    object%res => null ()
    object%arg => null ()
    call object%get_core_ptr (core)
    call core%resolve (success);  if (.not. success)  return
    select type (core)
    type is (logical_t)
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
          type is (logical_t)
             object%arg => core
             success = .true.
          end select
       end select
    end if
  end subroutine logical_unary_resolve
  
  recursive subroutine not_evaluate (object)
    class(not_t), intent(inout), target :: object
    call object%composite_t%evaluate ()
    if (object%arg%is_defined ()) then
       call object%res%init (.not. object%arg%value)
    else
       call object%res%init ()
    end if
  end subroutine not_evaluate
  
  pure function and_get_priority (object) result (priority)
    class(and_t), intent(in) :: object
    integer :: priority
    priority = PRIO_AND
  end function and_get_priority
  
  pure function or_get_priority (object) result (priority)
    class(or_t), intent(in) :: object
    integer :: priority
    priority = PRIO_OR
  end function or_get_priority
  
  subroutine and_instantiate (object, instance)
    class(and_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (and_t :: instance)
    select type (instance)
    type is (and_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine and_instantiate
  
  subroutine or_instantiate (object, instance)
    class(or_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: prototype
    allocate (or_t :: instance)
    select type (instance)
    type is (or_t)
       call object%get_prototype_ptr (prototype)
       call instance%init (prototype, mode=MODE_CONSTANT)
    end select
  end subroutine or_instantiate
  
  subroutine logical_binary_init (object, prototype, name, mode)
    class(logical_binary_t), intent(inout) :: object
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
  end subroutine logical_binary_init
  
  subroutine and_init (object, prototype, mode)
    class(and_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call logical_binary_init (object, prototype, var_str ("and"), mode)
  end subroutine and_init
  
  subroutine or_init (object, prototype, mode)
    class(or_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: mode
    call logical_binary_init (object, prototype, var_str ("or"), mode)
  end subroutine or_init
  
  recursive subroutine logical_binary_resolve (object, success)
    class(logical_binary_t), intent(inout), target :: object
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
             type is (logical_t)
                call object%arg_ptr(i)%associate (core)
                success = .true.
             end select
          end select
       end do
    end if
  end subroutine logical_binary_resolve
  
  recursive subroutine and_evaluate (object)
    class(and_t), intent(inout), target :: object
    integer :: i
    call object%composite_t%evaluate ()
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%is_defined ()) then
          call object%res%init ()
          return
       end if
    end do
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%get_value ()) then
          call object%res%init (.false.)
          return
       end if
    end do
    call object%res%init (.true.)
  end subroutine and_evaluate
  
  recursive subroutine or_evaluate (object)
    class(or_t), intent(inout), target :: object
    integer :: i
    call object%composite_t%evaluate ()
    do i = 1, size (object%arg_ptr)
       if (.not. object%arg_ptr(i)%is_defined ()) then
          call object%res%init ()
          return
       end if
    end do
    do i = 1, size (object%arg_ptr)
       if (object%arg_ptr(i)%get_value ()) then
          call object%res%init (.true.)
          return
       end if
    end do
    call object%res%init (.false.)
  end subroutine or_evaluate
  

end module object_logical
