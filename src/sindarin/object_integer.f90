! WHIZARD 2.2.6 May 02 2015
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

module object_integer

  use iso_varying_string, string_t => varying_string
  use unit_tests
  use format_utils
  use io_units

  use codes
  use object_base
  use object_builder
  use object_expr

  implicit none
  private

  public :: integer_t
  public :: object_integer_test



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
  end type integer_p
     



contains

  pure subroutine integer_final (object)
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
    code%cat = CAT_VALUE
    if (object%is_defined ()) then
       allocate (val_integer_t :: code%val)
       select type (val => code%val)
       type is (val_integer_t)
          call val%init (1)
          val%x(1) = object%value
       end select
    end if
  end function integer_get_code
  
  subroutine integer_init_from_code (object, code)
    class(integer_t), intent(out) :: object
    type(code_t), intent(in) :: code
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_integer_t)
          if (val%get_nval () > 0)  call object%init (val%x(1))
       end select
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
       
  subroutine object_integer_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (object_integer_1, "object_integer_1", &
         "values", &
         u, results)  
  end subroutine object_integer_test
  

  subroutine object_integer_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: bare_integer, core
    class(object_t), pointer :: prototype, pos, neg, zero, undef

    write (u, "(A)")  "* Test output: object_integer_1"
    write (u, "(A)")  "*   Purpose: construct integer value objects"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Integer objects: prototype"

    allocate (integer_t :: bare_integer)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("integer"))
       call prototype%import_core (bare_integer)
    end select

    write (u, "(A)")
    call prototype%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Integer objects: zero, positive, negative, undefined"

    call prototype%instantiate (pos)
    select type (pos)
    type is (composite_t)
       call pos%init (mode = MODE_CONSTANT, name = var_str ("pos"))
       allocate (integer_t :: core)
       select type (core)
       type is (integer_t);  call core%init (value = 42)
       end select
       call pos%import_core (core)
    end select

    call prototype%instantiate (neg)
    select type (neg)
    type is (composite_t)
       call neg%init (mode = MODE_CONSTANT, name = var_str ("neg"))
       allocate (integer_t :: core)
       select type (core)
       type is (integer_t);  call core%init (value = -1234567890)
       end select
       call neg%import_core (core)
    end select

    call prototype%instantiate (zero)
    select type (zero)
    type is (composite_t)
       call zero%init (mode = MODE_CONSTANT, name = var_str ("zero"))
       allocate (integer_t :: core)
       select type (core)
       type is (integer_t);  call core%init (value = 0)
       end select
       call zero%import_core (core)
    end select

    call prototype%instantiate (undef)
    select type (undef)
    type is (composite_t)
       call undef%init (mode = MODE_CONSTANT, name = var_str ("undef"))
       allocate (integer_t :: core)
       call undef%import_core (core)
    end select

    write (u, "(A)")
    call pos%write (u)
    call neg%write (u)
    call zero%write (u)
    call undef%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (undef)
    call remove_object (pos)
    call remove_object (neg)
    call remove_object (zero)
    call remove_object (prototype)
    call remove_object (bare_integer)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_1"
    
    end subroutine object_integer_1


end module object_integer
