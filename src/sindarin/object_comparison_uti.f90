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

module object_comparison_uti
  
    use iso_varying_string, string_t => varying_string
    use codes
    use object_base
    use object_logical
    use object_expr
    use object_integer

    use object_comparison

  implicit none
  private

  public :: object_comparison_1
  public :: object_comparison_2

contains
  
  subroutine init_members (object, n_arg)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: n_arg
    select type (object)
    class is (operator_t)
       call object%init_args (n_arg)
    end select
  end subroutine init_members
    
  subroutine set_member_val (object, prototype, i, value, cmp_code)
    class(object_t), intent(inout) :: object
    class(object_t), intent(inout), target :: prototype
    integer, intent(in) :: i
    integer, intent(in), optional :: value
    integer, intent(in), optional :: cmp_code
    class(object_t), pointer :: member, core
    type(string_t) :: name
    if (present (value)) then
       select case (i)
       case (1);  name = "i"
       case (2);  name = "j"
       case (3);  name = "k"
       case (4);  name = "l"
       end select
    else
       name = "undef"
    end if
    select type (object)
    class is (composite_t)
       select type (prototype)
       class is (composite_t)
          call prototype%instantiate (member)
       end select
       select type (member)
       class is (composite_t)
          call member%init (name = name, mode = MODE_CONSTANT)
          call member%get_core_ptr (core)
          select type (core)
          class is (integer_t)
             call core%init (value)
          end select
       end select
       call object%import_member (i, member)
    end select
    if (present (cmp_code)) then
       select type (object)
       class is (compare_t)
          call object%set_cmp_code (i, cmp_code)
       end select
    end if
  end subroutine set_member_val
    

  subroutine object_comparison_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: pro_log, pro_int
    class(object_t), pointer :: compare
    class(object_t), pointer :: a1, a2, a3, a4, a5, a6
    class(object_t), pointer :: b1, b2, b3, b4, b5, b6
    logical :: success

    write (u, "(A)")  "* Test output: object_comparison_1"
    write (u, "(A)")  "*   Purpose: check integer comparison operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: compare"

    allocate (composite_t :: pro_log)
    select type (pro_log)
    type is (composite_t)
       call pro_log%init (var_str ("logical"))
       allocate (logical_t :: core)
       call pro_log%import_core (core)
    end select
    
    allocate (composite_t :: pro_int)
    select type (pro_int)
    type is (composite_t)
       call pro_int%init (var_str ("integer"))
       allocate (integer_t :: core)
       call pro_int%import_core (core)
    end select
    
    allocate (compare_t :: compare)
    select type (compare)
    type is (compare_t)
       call compare%init (pro_log)
    end select
    
    write (u, "(A)")
    call compare%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call compare%instantiate (a1)
    call init_members (a1, 2)
    call set_member_val (a1, pro_int, 1, 2)
    call set_member_val (a1, pro_int, 2, 3, CMP_EQ) 
    call a1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a2)
    call init_members (a2, 2)
    call set_member_val (a2, pro_int, 1, 2)
    call set_member_val (a2, pro_int, 2, 3, CMP_NE) 
    call a2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a3)
    call init_members (a3, 2)
    call set_member_val (a3, pro_int, 1, 2)
    call set_member_val (a3, pro_int, 2, 3, CMP_LT) 
    call a3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a4)
    call init_members (a4, 2)
    call set_member_val (a4, pro_int, 1, 2)
    call set_member_val (a4, pro_int, 2, 3, CMP_GT) 
    call a4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a5)
    call init_members (a5, 2)
    call set_member_val (a5, pro_int, 1, 2)
    call set_member_val (a5, pro_int, 2, 3, CMP_LE) 
    call a5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a6)
    call init_members (a6, 2)
    call set_member_val (a6, pro_int, 1, 2)
    call set_member_val (a6, pro_int, 2, 3, CMP_GE) 
    call a6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, *)

    call compare%instantiate (b1)
    call init_members (b1, 2)
    call set_member_val (b1, pro_int, 1, 2)
    call set_member_val (b1, pro_int, 2, 2, CMP_EQ) 
    call b1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (b2)
    call init_members (b2, 2)
    call set_member_val (b2, pro_int, 1, 2)
    call set_member_val (b2, pro_int, 2, 2, CMP_NE) 
    call b2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (b3)
    call init_members (b3, 2)
    call set_member_val (b3, pro_int, 1, 2)
    call set_member_val (b3, pro_int, 2, 2, CMP_LT) 
    call b3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (b4)
    call init_members (b4, 2)
    call set_member_val (b4, pro_int, 1, 2)
    call set_member_val (b4, pro_int, 2, 2, CMP_GT) 
    call b4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (b5)
    call init_members (b5, 2)
    call set_member_val (b5, pro_int, 1, 2)
    call set_member_val (b5, pro_int, 2, 2, CMP_LE) 
    call b5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (b6)
    call init_members (b6, 2)
    call set_member_val (b6, pro_int, 1, 2)
    call set_member_val (b6, pro_int, 2, 2, CMP_GE) 
    call b6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
    call a4%write (u)
    call a5%write (u)
    call a6%write (u)

    write (u, "(A)")
    call a1%write_as_expression (u)
    write (u, *)
    call a2%write_as_expression (u)
    write (u, *)
    call a3%write_as_expression (u)
    write (u, *)
    call a4%write_as_expression (u)
    write (u, *)
    call a5%write_as_expression (u)
    write (u, *)
    call a6%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    call b1%write_as_expression (u)
    write (u, *)
    call b2%write_as_expression (u)
    write (u, *)
    call b3%write_as_expression (u)
    write (u, *)
    call b4%write_as_expression (u)
    write (u, *)
    call b5%write_as_expression (u)
    write (u, *)
    call b6%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call a1%evaluate ()
    call a2%evaluate ()
    call a3%evaluate ()
    call a4%evaluate ()
    call a5%evaluate ()
    call a6%evaluate ()

    call b1%evaluate ()
    call b2%evaluate ()
    call b3%evaluate ()
    call b4%evaluate ()
    call b5%evaluate ()
    call b6%evaluate ()

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
    call a4%write (u)
    call a5%write (u)
    call a6%write (u)

    write (u, "(A)")
    call a1%write_as_value (u)
    write (u, *)
    call a2%write_as_value (u)
    write (u, *)
    call a3%write_as_value (u)
    write (u, *)
    call a4%write_as_value (u)
    write (u, *)
    call a5%write_as_value (u)
    write (u, *)
    call a6%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    call b1%write_as_value (u)
    write (u, *)
    call b2%write_as_value (u)
    write (u, *)
    call b3%write_as_value (u)
    write (u, *)
    call b4%write_as_value (u)
    write (u, *)
    call b5%write_as_value (u)
    write (u, *)
    call b6%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a3)
    call remove_object (a4)
    call remove_object (a5)
    call remove_object (a6)
    call remove_object (b1)
    call remove_object (b2)
    call remove_object (b3)
    call remove_object (b4)
    call remove_object (b5)
    call remove_object (b6)
    call remove_object (compare)
    call remove_object (pro_log)
    call remove_object (pro_int)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_comparison_1"
    
  end subroutine object_comparison_1

  subroutine object_comparison_2 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: pro_log, pro_int
    class(object_t), pointer :: compare
    class(object_t), pointer :: a1, a2, a3, a4
    logical :: success

    write (u, "(A)")  "* Test output: object_comparison_2"
    write (u, "(A)")  "*   Purpose: check chained integer comparison operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: compare"

    allocate (composite_t :: pro_log)
    select type (pro_log)
    type is (composite_t)
       call pro_log%init (var_str ("logical"))
       allocate (logical_t :: core)
       call pro_log%import_core (core)
    end select
    
    allocate (composite_t :: pro_int)
    select type (pro_int)
    type is (composite_t)
       call pro_int%init (var_str ("integer"))
       allocate (integer_t :: core)
       call pro_int%import_core (core)
    end select
    
    allocate (compare_t :: compare)
    select type (compare)
    type is (compare_t)
       call compare%init (pro_log)
    end select
    
    write (u, "(A)")
    call compare%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call compare%instantiate (a1)
    call init_members (a1, 2)
    call set_member_val (a1, pro_int, 1)
    call set_member_val (a1, pro_int, 2, 3, CMP_EQ) 
    call a1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a2)
    call init_members (a2, 2)
    call set_member_val (a2, pro_int, 1, 2)
    call set_member_val (a2, pro_int, 2, cmp_code = CMP_EQ) 
    call a2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a3)
    call init_members (a3, 3)
    call set_member_val (a3, pro_int, 1, 2)
    call set_member_val (a3, pro_int, 2, 3, CMP_LT) 
    call set_member_val (a3, pro_int, 3, 4, CMP_LT) 
    call a3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call compare%instantiate (a4)
    call init_members (a4, 4)
    call set_member_val (a4, pro_int, 1, 2)
    call set_member_val (a4, pro_int, 2, 3, CMP_LT) 
    call set_member_val (a4, pro_int, 3, 4, CMP_LE) 
    call set_member_val (a4, pro_int, 4, 5, CMP_GT) 
    call a4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
    call a4%write (u)

    write (u, "(A)")
    call a1%write_as_expression (u)
    write (u, *)
    call a2%write_as_expression (u)
    write (u, *)
    call a3%write_as_expression (u)
    write (u, *)
    call a4%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call a1%evaluate ()
    call a2%evaluate ()
    call a3%evaluate ()
    call a4%evaluate ()

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
    call a4%write (u)

    write (u, "(A)")
    call a1%write_as_value (u)
    write (u, *)
    call a2%write_as_value (u)
    write (u, *)
    call a3%write_as_value (u)
    write (u, *)
    call a4%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a3)
    call remove_object (a4)
    call remove_object (compare)
    call remove_object (pro_log)
    call remove_object (pro_int)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_comparison_2"
    
  end subroutine object_comparison_2


end module object_comparison_uti
