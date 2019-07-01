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

module object_conditional

  use iso_varying_string, string_t => varying_string
  use format_utils
  use io_units

  use codes
  use object_base
  use object_builder
  use object_expr
  use object_logical
  use object_integer
  use object_comparison

  implicit none
  private

  public :: conditional_t

  type, extends (composite_t) :: conditional_t
     private
     class(object_t), pointer :: cond_prototype => null ()
     type(integer_t), pointer :: res => null ()
     type(logical_p), dimension(:), allocatable :: cond_ptr
     type(integer_p), dimension(:), allocatable :: branch_ptr
   contains
     procedure :: final => conditional_final
     procedure :: write_expression => conditional_write_expression
     procedure :: is_expression => conditional_is_expression
     procedure :: get_prototype_index => conditional_get_prototype_index
     procedure :: get_priority => conditional_get_priority
     procedure :: show_opname => conditional_show_opname
     procedure :: get_opname => conditional_get_opname
     procedure :: get_cond_prototype_ptr => conditional_get_cond_prototype_ptr
     procedure :: instantiate => conditional_instantiate
     generic :: init => conditional_init
     procedure, private :: conditional_init
     procedure :: init_branches => conditional_init_branches
     procedure :: resolve => conditional_resolve
     procedure :: evaluate => conditional_evaluate
end type conditional_t


contains

  recursive subroutine conditional_final (object)
    class(conditional_t), intent(inout) :: object
    call remove_object (object%cond_prototype)
    call object%composite_t%final ()
  end subroutine conditional_final
       
  recursive subroutine conditional_write_expression (object, unit, indent)
    class(conditional_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: arg
    integer :: u, priority, i, ii
    u = given_output_unit (unit)
    priority = object%get_priority ()
    do i = 1, object%get_n_members ()
       write (u, "(A,1x)", advance="no")  char (object%get_opname (i))
       if (i < object%get_n_members ()) then
          select case (mod(i,2))
          case (0);  ii = i - 1
          case (1);  ii = i + 1
          end select
       else
          ii = i
       end if
       call object%get_member_ptr (ii, arg)
       if (associated (arg)) then
          arg => arg%dereference ()
          call arg%write_as_expression (unit, indent, priority=priority)
       else
          write (u, "(A)", advance="no") "???"
       end if
       write (u, "(1x)", advance="no")
    end do
    write (u, "(A)", advance="no")  "endif"
  end subroutine conditional_write_expression
    
  pure function conditional_is_expression (object) result (flag)
    class(conditional_t), intent(in) :: object
    logical :: flag
    call object%check_mode (flag)
  end function conditional_is_expression
  
  function conditional_get_prototype_index (object, repository) result (i)
    class(conditional_t), intent(in) :: object
    type(repository_t), intent(in) :: repository
    integer :: i
    call repository%find_member (object%get_name (), index=i)
  end function conditional_get_prototype_index

  pure function conditional_get_priority (object) result (priority)
    class(conditional_t), intent(in) :: object
    integer :: priority
    priority = PRIO_CONDITIONAL
  end function conditional_get_priority
  
  pure function conditional_show_opname (object, i) result (flag)
    class(conditional_t), intent(in) :: object
    integer, intent(in), optional :: i
    logical :: flag
    flag = mod (i, 2) == 1
  end function conditional_show_opname

  pure function conditional_get_opname (object, i) result (name)
    class(conditional_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    select case (mod (i, 2))
    case (1)
       if (i == 1) then
          name = "if"
       else if (i < object%get_n_members ()) then
          name = "elsif"
       else
          name = "else"
       end if
    case (0)
       name = "then"
    end select
  end function conditional_get_opname

  subroutine conditional_get_cond_prototype_ptr (object, cond_prototype)
    class(conditional_t), intent(in) :: object
    class(object_t), pointer :: cond_prototype
    cond_prototype => object%cond_prototype
  end subroutine conditional_get_cond_prototype_ptr
  
  subroutine conditional_instantiate (object, instance)
    class(conditional_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    class(composite_t), pointer :: value_prototype
    allocate (conditional_t :: instance)
    select type (instance)
    type is (conditional_t)
       call object%get_prototype_ptr (value_prototype)
       select type (cond_prototype => object%cond_prototype)
       class is (composite_t)
          call instance%init (cond_prototype, value_prototype, &
               mode=MODE_CONSTANT)
       end select
    end select
  end subroutine conditional_instantiate
  
  subroutine conditional_init (object, cond_prototype, value_prototype, mode)
    class(conditional_t), intent(inout) :: object
    class(object_t), intent(inout), target :: cond_prototype
    class(object_t), intent(inout), target :: value_prototype
    integer, intent(in), optional :: mode
    class(object_t), pointer :: value, core
    call object%composite_t%init (var_str ("conditional_expr"), mode)
    select type (cond_prototype)
    class is (composite_t)
       object%cond_prototype => cond_prototype
       call object%register (cond_prototype)
    end select
    select type (value_prototype)
    class is (composite_t)
       call object%register (value_prototype)
       call value_prototype%get_core_ptr (value)
    end select
    call value%instantiate (core)
    call object%import_core (core)
  end subroutine conditional_init
  
  subroutine conditional_init_branches (object, n_branches)
    class(conditional_t), intent(inout) :: object
    integer, intent(in) :: n_branches
    call object%init_members &
         (n_members = 2 * n_branches - 1, n_arguments = 2 * n_branches - 1)
  end subroutine conditional_init_branches

  recursive subroutine conditional_resolve (object, success)
    class(conditional_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: arg, core
    integer :: i, n_args, n_branches, n_cond
    success = .false.
    object%res => null ()
    n_args = object%get_n_members ()
    n_cond = n_args / 2
    n_branches = n_cond + 1
    call object%get_core_ptr (core)
    select type (core)
    type is (integer_t)
       object%res => core
    class default
       return
    end select
    if (.not. allocated (object%branch_ptr)) &
         allocate (object%branch_ptr (n_branches))
    if (.not. allocated (object%cond_ptr)) &
         allocate (object%cond_ptr (n_cond))
    if (object%has_value ()) then
       do i = 1, n_args
          call object%get_member_ptr (i, arg)
          call arg%resolve (success);  if (.not. success)  return
          select type (arg)
          class is (wrapper_t)
             call arg%get_core_ptr (core)
             select case (mod (i, 2))
             case (0)
                select type (core)
                type is (logical_t)
                   call object%cond_ptr(i/2)%associate (core)
                   success = .true.
                end select
             case (1)
                select type (core)
                type is (integer_t)
                   call object%branch_ptr(i/2+1)%associate (core)
                   success = .true.
                end select
             end select
          end select
       end do
    end if
  end subroutine conditional_resolve
  
  recursive subroutine conditional_evaluate (object)
    class(conditional_t), intent(inout), target :: object
    class(object_t), pointer :: condition, branch
    integer :: i, n_branches, n_cond
    integer :: value
    n_cond = object%get_n_members () / 2
    n_branches = n_cond + 1
    do i = 1, n_cond
       call object%get_member_ptr (2*i, condition)
       call condition%evaluate ()
       if (object%cond_ptr(i)%is_defined ()) then
          if (object%cond_ptr(i)%get_value ()) then
             call object%get_member_ptr (2*i-1, branch)
             call branch%evaluate ()
             if (object%branch_ptr(i)%is_defined ()) then
                value = object%branch_ptr(i)%get_value ()
                call object%res%init (value)
                return
             else
                call object%res%init ()
                return
             end if
          end if
       else
          call object%res%init ()
          return
       end if
    end do
    call object%get_member_ptr (2*n_branches-1, branch)
    call branch%evaluate ()
    if (object%branch_ptr(n_branches)%is_defined ()) then
       value = object%branch_ptr(n_branches)%get_value ()
       call object%res%init (value)
    else
       call object%res%init ()
    end if
  end subroutine conditional_evaluate
  

end module object_conditional
