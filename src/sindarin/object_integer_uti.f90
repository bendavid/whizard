! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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

module object_integer_uti
  
    use iso_varying_string, string_t => varying_string
    use io_units
    use codes
    use object_base
    use object_expr
    use object_logical

    use object_integer

  implicit none
  private

  public :: object_integer_1
  public :: object_integer_2
  public :: object_integer_3
  public :: object_integer_4
  public :: object_integer_5
  public :: object_integer_6
  public :: object_integer_7
  public :: object_integer_8
  public :: object_integer_9
  public :: object_integer_10

contains
  
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

  subroutine object_integer_2 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core, rhs
    class(object_t), pointer :: prototype, v1, v2, ival1, ival2, ival3
    type(assignment_t) :: asg
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_2"
    write (u, "(A)")  "*   Purpose: assignments"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Create objects"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    call prototype%instantiate (v1)
    select type (v1)
    type is (composite_t)
       call v1%init (mode = MODE_CONSTANT, name = var_str ("v1"))
       allocate (integer_t :: core)
       select type (core)
       type is (integer_t);  call core%init (value = 12)
       end select
       call v1%import_core (core)
    end select

    call prototype%instantiate (v2)
    select type (v2)
    type is (composite_t)
       call v2%init (mode = MODE_CONSTANT, name = var_str ("v2"))
       allocate (integer_t :: core)
       select type (core)
       type is (integer_t);  call core%init (value = -177)
       end select
       call v2%import_core (core)
    end select

    call prototype%instantiate (ival1)
    select type (ival1)
    type is (composite_t)
       call ival1%init (mode = MODE_VARIABLE, name = var_str ("ival1"))
    end select
    
    call prototype%instantiate (ival2)
    select type (ival2)
    type is (composite_t)
       call ival2%init (mode = MODE_VARIABLE, name = var_str ("ival2"))
    end select
    
    call prototype%instantiate (ival3)
    select type (ival3)
    type is (composite_t)
       call ival3%init (mode = MODE_VARIABLE, name = var_str ("ival3"))
    end select
    
    write (u, "(A)")
    call v1%write (u)
    call v2%write (u)

    write (u, "(A)")
    call ival1%write_as_declaration (u)
    call ival2%write_as_declaration (u)
    call ival3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* ival1 = v1"
  
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (ival1)
    call asg%set_rhs (v1, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call ival1%write_as_declaration (u)
    call ival2%write_as_declaration (u)
    call ival3%write_as_declaration (u)
  
    write (u, "(A)")
    write (u, "(A)")  "* ival2 = v2"
  
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (ival2)
    call asg%set_rhs (v2, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()
    
    write (u, "(A)")
    call ival1%write_as_declaration (u)
    call ival2%write_as_declaration (u)
    call ival3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* ival2 = ival1"
  
    rhs => ival1
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (ival2)
    call asg%set_rhs (rhs, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call ival1%write_as_declaration (u)
    call ival2%write_as_declaration (u)
    call ival3%write_as_declaration (u)
  
    write (u, "(A)")
    write (u, "(A)")  "* ival2 = ival3"
  
    rhs => ival3
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (ival2)
    call asg%set_rhs (rhs, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call ival1%write_as_declaration (u)
    call ival2%write_as_declaration (u)
    call ival3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (ival1)
    call remove_object (ival2)
    call remove_object (ival3)
    call remove_object (v1)
    call remove_object (v2)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_2"
    
  end subroutine object_integer_2

  subroutine object_integer_3 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: prototype, proto2, main, val, core, rhs, asg
    type(object_iterator_t) :: it
    class(object_t), pointer :: object
    type(code_t) :: code
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_3"
    write (u, "(A)")  "*   Purpose: simple composite assignment"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare composite object with primer"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select

    call prototype%instantiate (val)
    select type (val)
    type is (composite_t)
       call val%init (mode=MODE_CONSTANT, name = var_str ("val"))
    end select
    
    call setup_assignment ()
    write (u, "(A)")
    write (u, "(A)")  "* Assignment object"
    write (u, "(A)")
    call asg%write (u)

    allocate (composite_t :: main)
    select type (main)
    type is (composite_t)
       call main%init (name = var_str ("main"), n_members = 1, n_primers = 1)
       call main%import_member (1, val)
       call main%import_primer (1, asg)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Main object"
    write (u, "(A)")
    call main%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize: evaluate primer"
    
    select type (main)
    type is (composite_t)
       call main%resolve (success)
       call main%evaluate ()
    end select
    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call main%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Check mode/type mismatch"

    call remove_object (main)

    allocate (composite_t :: proto2)
    select type (proto2)
    type is (composite_t)
       call proto2%init (var_str ("logical"))
       allocate (logical_t :: core)
       call proto2%import_core (core)
    end select

    call proto2%instantiate (val)
    select type (val)
    type is (composite_t)
       call val%init (name = var_str ("val"), mode = MODE_CONSTANT)
    end select
   
    call setup_assignment ()

    allocate (composite_t :: main)
    select type (main)
    type is (composite_t)
       call main%init (name = var_str ("main"), n_members = 1, n_primers = 1)
       call main%import_member (1, val)
       call main%import_primer (1, asg)
    end select

    write (u, "(A)")
    call main%write (u)

    select type (main)
    type is (composite_t)
       call main%resolve (success)
       call main%evaluate ()
    end select
    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (proto2)
    call remove_object (prototype)
    call remove_object (main)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_3"
    
  contains
      
    subroutine setup_assignment
      allocate (assignment_t :: asg)
      select type (asg)
      type is (assignment_t)
         call prototype%instantiate (rhs)
         select type (rhs)
         type is (composite_t)
            call rhs%init (mode=MODE_CONSTANT, name=var_str ("rhs"))
            call rhs%get_core_ptr (core)
            select type (core)
            type is (integer_t)
               call core%init (value = 42)
            end select
         end select
         call asg%init (MODE=MODE_CONSTANT)
         call asg%set_path ([var_str ("val")])
         call asg%set_rhs (rhs=rhs, link=.false.)
      end select
    end subroutine setup_assignment

  end subroutine object_integer_3

  subroutine object_integer_4 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: minus, expr1, expr2, expr3
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_4"
    write (u, "(A)")  "*   Purpose: check integer operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: minus"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (minus_t :: minus)
    select type (minus)
    type is (minus_t)
       call minus%init (prototype)
    end select
    
    write (u, "(A)")
    call minus%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"

    write (u, "(A)")
    
    call minus%instantiate (expr1)
    call init_members (expr1, 1)
    call set_member_val (expr1, 1, 42)
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call minus%instantiate (expr2)
    call init_members (expr2, 1)
    call set_member_val (expr2, 1, -1234567890)
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call minus%instantiate (expr3)
    call init_members (expr3, 1)
    call set_member_val (expr3, 1)
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u);  write (u, *)
    call expr2%write_as_expression (u);  write (u, *)
    call expr3%write_as_expression (u);  write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (minus)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_4"
    
  end subroutine object_integer_4

  subroutine object_integer_5 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: multiply
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_5"
    write (u, "(A)")  "*   Purpose: check integer operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: multiply"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (multiply_t :: multiply)
    select type (multiply)
    type is (multiply_t)
       call multiply%init (prototype)
    end select
    
    write (u, "(A)")
    call multiply%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call multiply%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, 2)
    call set_member_val (expr1, 2, 3) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, 2)
    call set_member_val (expr2, 2, 0) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, 2)
    call set_member_val (expr3, 2, -2) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr4)
    call init_members (expr4, 3)
    call set_member_val (expr4, 1, 2)
    call set_member_val (expr4, 2, 3) 
    call set_member_val (expr4, 3, 5) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr5)
    call init_members (expr5, 3)
    call set_member_val (expr5, 1, -2)
    call set_member_val (expr5, 2, 3) 
    call set_member_val (expr5, 3, 5) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr6)
    call init_members (expr6, 3)
    call set_member_val (expr6, 1, -2)
    call set_member_val (expr6, 2) 
    call set_member_val (expr6, 3, 5) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u)
    write (u, *)
    call expr2%write_as_expression (u)
    write (u, *)
    call expr3%write_as_expression (u)
    write (u, *)
    call expr4%write_as_expression (u)
    write (u, *)
    call expr5%write_as_expression (u)
    write (u, *)
    call expr6%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()
    call expr4%evaluate ()
    call expr5%evaluate ()
    call expr6%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)
    call expr4%write_as_value (u)
    write (u, *)
    call expr5%write_as_value (u)
    write (u, *)
    call expr6%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (expr4)
    call remove_object (expr5)
    call remove_object (expr6)
    call remove_object (multiply)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_5"
    
  end subroutine object_integer_5

  subroutine object_integer_6 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: multiply
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6, expr7
    class(object_t), pointer :: expr8, expr9, arg1, arg2
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_6"
    write (u, "(A)")  "*   Purpose: check integer operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: multiply"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (multiply_t :: multiply)
    select type (multiply)
    type is (multiply_t)
       call multiply%init (prototype)
    end select
    
    write (u, "(A)")
    call multiply%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call multiply%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, 4)
    call set_member_val (expr1, 2, 2, inverse=.true.) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, 2)
    call set_member_val (expr2, 2, 3, inverse=.true.) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, 2)
    call set_member_val (expr3, 2, -2, inverse=.true.) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr4)
    call init_members (expr4, 2)
    call set_member_val (expr4, 1, 2)
    call set_member_val (expr4, 2, 0, inverse=.true.) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr5)
    call init_members (expr5, 3)
    call set_member_val (expr5, 1, 24)
    call set_member_val (expr5, 2, 3, inverse=.true.) 
    call set_member_val (expr5, 3, 4) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr6)
    call init_members (expr6, 2)
    select type (expr6)
    class is (composite_t)
       call multiply%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, 24)
       call set_member_val (arg1, 2, 3, inverse=.true.) 
       call expr6%import_member (1, arg1)
    end select
    call set_member_val (expr6, 2, 4) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr7)
    call init_members (expr7, 2)
    call set_member_val (expr7, 1, 24)
    select type (expr7)
    class is (composite_t)
       call multiply%instantiate (arg2)
       call init_members (arg2, 2)
       call set_member_val (arg2, 1, 3) 
       call set_member_val (arg2, 2, 4) 
       call expr7%import_member (2, arg2)
    end select
    select type (expr7)
    class is (integer_binary_t)
       call expr7%tag_inverse (2, .true.)
    end select
    call expr7%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr8)
    call init_members (expr8, 3)
    call set_member_val (expr8, 1, 24)
    call set_member_val (expr8, 2, 3, inverse=.true.) 
    call set_member_val (expr8, 3, 4, inverse=.true.) 
    call expr8%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr9)
    call init_members (expr9, 3)
    call set_member_val (expr9, 1, 24)
    call set_member_val (expr9, 2, inverse=.true.) 
    call set_member_val (expr9, 3, 4) 
    call expr9%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)
    call expr7%write (u)
    call expr8%write (u)
    call expr9%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u)
    write (u, *)
    call expr2%write_as_expression (u)
    write (u, *)
    call expr3%write_as_expression (u)
    write (u, *)
    call expr4%write_as_expression (u)
    write (u, *)
    call expr5%write_as_expression (u)
    write (u, *)
    call expr6%write_as_expression (u)
    write (u, *)
    call expr7%write_as_expression (u)
    write (u, *)
    call expr8%write_as_expression (u)
    write (u, *)
    call expr9%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()
    call expr4%evaluate ()
    call expr5%evaluate ()
    call expr6%evaluate ()
    call expr7%evaluate ()
    call expr8%evaluate ()
    call expr9%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)
    call expr7%write (u)
    call expr8%write (u)
    call expr9%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)
    call expr4%write_as_value (u)
    write (u, *)
    call expr5%write_as_value (u)
    write (u, *)
    call expr6%write_as_value (u)
    write (u, *)
    call expr7%write_as_value (u)
    write (u, *)
    call expr8%write_as_value (u)
    write (u, *)
    call expr9%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (expr4)
    call remove_object (expr5)
    call remove_object (expr6)
    call remove_object (expr7)
    call remove_object (expr8)
    call remove_object (expr9)
    call remove_object (multiply)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_6"
    
  end subroutine object_integer_6

  subroutine object_integer_7 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: add
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_7"
    write (u, "(A)")  "*   Purpose: check integer operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: add"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (add_t :: add)
    select type (add)
    type is (add_t)
       call add%init (prototype)
    end select
    
    write (u, "(A)")
    call add%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call add%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, 2)
    call set_member_val (expr1, 2, 3) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, 2)
    call set_member_val (expr2, 2, 0) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, 2)
    call set_member_val (expr3, 2, -2) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr4)
    call init_members (expr4, 3)
    call set_member_val (expr4, 1, 2)
    call set_member_val (expr4, 2, 3) 
    call set_member_val (expr4, 3, 5) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr5)
    call init_members (expr5, 3)
    call set_member_val (expr5, 1, -2)
    call set_member_val (expr5, 2, 3) 
    call set_member_val (expr5, 3, 5) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr6)
    call init_members (expr6, 3)
    call set_member_val (expr6, 1, -2)
    call set_member_val (expr6, 2) 
    call set_member_val (expr6, 3, 5) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u)
    write (u, *)
    call expr2%write_as_expression (u)
    write (u, *)
    call expr3%write_as_expression (u)
    write (u, *)
    call expr4%write_as_expression (u)
    write (u, *)
    call expr5%write_as_expression (u)
    write (u, *)
    call expr6%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()
    call expr4%evaluate ()
    call expr5%evaluate ()
    call expr6%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)
    call expr4%write_as_value (u)
    write (u, *)
    call expr5%write_as_value (u)
    write (u, *)
    call expr6%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (expr4)
    call remove_object (expr5)
    call remove_object (expr6)
    call remove_object (add)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_7"
    
  end subroutine object_integer_7

  subroutine object_integer_8 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: add
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6, expr7
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_8"
    write (u, "(A)")  "*   Purpose: check integer operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: add"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (add_t :: add)
    select type (add)
    type is (add_t)
       call add%init (prototype)
    end select
    
    write (u, "(A)")
    call add%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call add%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, 4)
    call set_member_val (expr1, 2, 2, inverse=.true.) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, 2)
    call set_member_val (expr2, 2, 3, inverse=.true.) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, 2)
    call set_member_val (expr3, 2, -2, inverse=.true.) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr4)
    call init_members (expr4, 2)
    call set_member_val (expr4, 1, 2)
    call set_member_val (expr4, 2, 0, inverse=.true.) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr5)
    call init_members (expr5, 3)
    call set_member_val (expr5, 1, 24)
    call set_member_val (expr5, 2, 3, inverse=.true.) 
    call set_member_val (expr5, 3, 4) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr6)
    call init_members (expr6, 3)
    call set_member_val (expr6, 1, 24)
    call set_member_val (expr6, 2, 3, inverse=.true.) 
    call set_member_val (expr6, 3, 4, inverse=.true.) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr7)
    call init_members (expr7, 3)
    call set_member_val (expr7, 1, 24)
    call set_member_val (expr7, 2, inverse=.true.) 
    call set_member_val (expr7, 3, 4) 
    call expr7%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)
    call expr7%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u)
    write (u, *)
    call expr2%write_as_expression (u)
    write (u, *)
    call expr3%write_as_expression (u)
    write (u, *)
    call expr4%write_as_expression (u)
    write (u, *)
    call expr5%write_as_expression (u)
    write (u, *)
    call expr6%write_as_expression (u)
    write (u, *)
    call expr7%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()
    call expr4%evaluate ()
    call expr5%evaluate ()
    call expr6%evaluate ()
    call expr7%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    call expr2%write (u)
    call expr3%write (u)
    call expr4%write (u)
    call expr5%write (u)
    call expr6%write (u)
    call expr7%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)
    call expr4%write_as_value (u)
    write (u, *)
    call expr5%write_as_value (u)
    write (u, *)
    call expr6%write_as_value (u)
    write (u, *)
    call expr7%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (expr4)
    call remove_object (expr5)
    call remove_object (expr6)
    call remove_object (expr7)
    call remove_object (add)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_8"
    
  end subroutine object_integer_8

  subroutine object_integer_9 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: minus, multiply, add
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5
    class(object_t), pointer :: arg1, arg2, arg3
    logical :: success

    write (u, "(A)")  "* Test output: object_integer_9"
    write (u, "(A)")  "*   Purpose: check nested integer expressions"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototypes: minus, multiply, add"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("integer"))
       allocate (integer_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (minus_t :: minus)
    select type (minus)
    type is (minus_t)
       call minus%init (prototype)
    end select
    
    allocate (multiply_t :: multiply)
    select type (multiply)
    type is (multiply_t)
       call multiply%init (prototype)
    end select
    
    allocate (add_t :: add)
    select type (add)
    type is (add_t)
       call add%init (prototype)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call multiply%instantiate (expr1)
    call init_members (expr1, 2)
    select type (expr1)
    class is (composite_t)
       call minus%instantiate (arg1)
       call init_members (arg1, 1)
       call set_member_val (arg1, 1, -4)
       call expr1%import_member (1, arg1)
    end select
    call set_member_val (expr1, 2, 25)
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call minus%instantiate (expr2)
    call init_members (expr2, 1)
    select type (expr2)
    class is (composite_t)
       call multiply%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, -4)
       call set_member_val (arg1, 2, 25)
       call expr2%import_member (1, arg1)
    end select
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, 12)
    select type (expr3)
    class is (composite_t)
       call add%instantiate (arg2)
       call init_members (arg2, 2)
       call set_member_val (arg2, 1, 5)
       call set_member_val (arg2, 2, 3, inverse=.true.)
       call expr3%import_member (2, arg2)
    end select
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call add%instantiate (expr4)
    call init_members (expr4, 2)
    select type (expr4)
    class is (composite_t)
       call multiply%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, 12)
       call set_member_val (arg1, 2, 5)
       call expr4%import_member (1, arg1)
    end select
    call set_member_val (expr4, 2, 3, inverse=.true.)
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call multiply%instantiate (expr5)
    call init_members (expr5, 2)
    select type (expr5)
    class is (composite_t)
       call add%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, 5)
       call set_member_val (arg1, 2, 3)
       call expr5%import_member (1, arg1)
       call add%instantiate (arg2)
       call init_members (arg2, 2)
       call set_member_val (arg2, 1, 5)
       select type (arg2)
       class is (composite_t)
          call minus%instantiate (arg3)
          call init_members (arg3, 1)
          call set_member_val (arg3, 1, 3)
          call arg2%import_member (2, arg3)
       end select
       call expr5%import_member (2, arg2)
    end select
    select type (expr5)
    class is (integer_binary_t)
       call expr5%tag_inverse (2, .true.)
    end select
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call expr1%write (u)
    write (u, "(A)")
    call expr2%write (u)
    write (u, "(A)")
    call expr3%write (u)
    write (u, "(A)")
    call expr4%write (u)
    write (u, "(A)")
    call expr5%write (u)

    write (u, "(A)")
    call expr1%write_as_expression (u)
    write (u, *)
    call expr2%write_as_expression (u)
    write (u, *)
    call expr3%write_as_expression (u)
    write (u, *)
    call expr4%write_as_expression (u)
    write (u, *)
    call expr5%write_as_expression (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call expr1%evaluate ()
    call expr2%evaluate ()
    call expr3%evaluate ()
    call expr4%evaluate ()
    call expr5%evaluate ()

    write (u, "(A)")
    call expr1%write (u)
    write (u, "(A)")
    call expr2%write (u)
    write (u, "(A)")
    call expr3%write (u)
    write (u, "(A)")
    call expr4%write (u)
    write (u, "(A)")
    call expr5%write (u)

    write (u, "(A)")
    call expr1%write_as_value (u)
    write (u, *)
    call expr2%write_as_value (u)
    write (u, *)
    call expr3%write_as_value (u)
    write (u, *)
    call expr4%write_as_value (u)
    write (u, *)
    call expr5%write_as_value (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (expr1)
    call remove_object (expr2)
    call remove_object (expr3)
    call remove_object (expr4)
    call remove_object (expr5)
    call remove_object (multiply)
    call remove_object (add)
    call remove_object (minus)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_9"
    
  end subroutine object_integer_9

  subroutine object_integer_10 (u)
    integer, intent(in) :: u
    type(repository_t) :: repository
    class(object_t), pointer :: p_int, p_minus, p_mult, p_add
    class(object_t), pointer :: core, main, object
    class(object_t), pointer :: val1, val2, expr
    integer :: utmp, ncode, i
    character(80) :: buffer
    type(code_t) :: code
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_integer_10"
    write (u, "(A)")  "*   Purpose: construct expressions from code"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare repository"

    allocate (composite_t :: p_int)
    select type (p_int)
    type is (composite_t)
       call p_int%init (var_str ("integer"))
       allocate (integer_t :: core)
       call p_int%import_core (core)
    end select

    allocate (minus_t :: p_minus)
    select type (p_minus)
    type is (minus_t)
       call p_minus%init (p_int)
    end select

    allocate (multiply_t :: p_mult)
    select type (p_mult)
    type is (multiply_t)
       call p_mult%init (p_int)
    end select

    allocate (add_t :: p_add)
    select type (p_add)
    type is (add_t)
       call p_add%init (p_int)
    end select

    call repository%init (name = var_str ("repository"), n_members = 4)
    call repository%import_member (1, p_int)
    call repository%import_member (2, p_minus)
    call repository%import_member (3, p_mult)
    call repository%import_member (4, p_add)

    write (u, "(A)")
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Construct object: minus"
    

    call repository%spawn (var_str ("integer"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_two"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (integer_t)
          call core%init (2)
       end select
    end select

    call repository%spawn (var_str ("minus"), expr)
    select type (expr)
    class is (minus_t)
       call expr%init_args (1)
       call expr%import_member (1, val1)
    end select

    allocate (wrapper_t :: main)
    select type (main)
    class is (wrapper_t)
       call main%import_core (expr)
    end select
    
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Code from object"

    utmp = free_unit ()
    open (utmp, status="scratch")

    write (u, "(A)")

    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
    end select

    call it%init (object)
    do while (it%is_valid ())
       call it%get_object (object)
       code = object%get_code (repository)
       call code%write (u, verbose=.true.)
       call code%write (utmp)
       call it%advance ()
    end do

    rewind (utmp)
    
    write (u, "(A)")
    write (u, "(A)")  "* Construct object from code"

    ncode = 4
    
    call remove_object (main)
    allocate (wrapper_t :: main)
    call it%init (main)
    do i = 1, ncode
       call code%read (utmp)
       call build_object (object, code, repository)
       if (associated (object)) then
          call it%advance (import_object = object)
       else
          call it%advance ()
       end if
       call it%get_object (object)
       select type (object)
       class is (value_t)
          call object%init_from_code (code)
       end select
    end do
    close (utmp)
 
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Construct object: multiply"

    call remove_object (object)

    call repository%spawn (var_str ("integer"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_two"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (integer_t)
          call core%init (2)
       end select
    end select

    call repository%spawn (var_str ("integer"), val2)
    select type (val2)
    class is (composite_t)
       call val2%init (name = var_str ("val_three"), mode = MODE_CONSTANT)
       call val2%get_core_ptr (core)
       select type (core)
       type is (integer_t)
          call core%init (3)
       end select
    end select

    call repository%spawn (var_str ("multiply"), expr)
    select type (expr)
    class is (multiply_t)
       call expr%init_args (2)
       call expr%import_member (1, val1)
       call expr%import_member (2, val2)
    end select

    allocate (wrapper_t :: main)
    select type (main)
    class is (wrapper_t)
       call main%import_core (expr)
    end select
    
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Code from object"

    utmp = free_unit ()
    open (utmp, status="scratch")

    write (u, "(A)")

    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
    end select

    call it%init (object)
    do while (it%is_valid ())
       call it%get_object (object)
       code = object%get_code (repository)
       call code%write (u, verbose=.true.)
       call code%write (utmp)
       call it%advance ()
    end do

    rewind (utmp)
    
    write (u, "(A)")
    write (u, "(A)")  "* Construct object from code"

    ncode = 6
    
    call remove_object (main)
    allocate (wrapper_t :: main)
    call it%init (main)
    do i = 1, ncode
       call code%read (utmp)
       call build_object (object, code, repository)
       if (associated (object)) then
          call it%advance (import_object = object)
       else
          call it%advance ()
       end if
       call it%get_object (object)
       select type (object)
       class is (value_t)
          call object%init_from_code (code)
       end select
    end do
    close (utmp)
 
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Construct object: add"

    call remove_object (object)

    call repository%spawn (var_str ("integer"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_two"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (integer_t)
          call core%init (2)
       end select
    end select

    call repository%spawn (var_str ("integer"), val2)
    select type (val2)
    class is (composite_t)
       call val2%init (name = var_str ("val_three"), mode = MODE_CONSTANT)
       call val2%get_core_ptr (core)
       select type (core)
       type is (integer_t)
          call core%init (3)
       end select
    end select

    call repository%spawn (var_str ("add"), expr)
    select type (expr)
    class is (add_t)
       call expr%init_args (2)
       call expr%import_member (1, val1)
       call expr%import_member (2, val2)
       call expr%tag_inverse (2, .true.)
    end select

    allocate (wrapper_t :: main)
    select type (main)
    class is (wrapper_t)
       call main%import_core (expr)
    end select
    
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Code from object"

    utmp = free_unit ()
    open (utmp, status="scratch")

    write (u, "(A)")

    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
    end select

    call it%init (object)
    do while (it%is_valid ())
       call it%get_object (object)
       code = object%get_code (repository)
       call code%write (u, verbose=.true.)
       call code%write (utmp)
       call it%advance ()
    end do

    rewind (utmp)
    
    write (u, "(A)")
    write (u, "(A)")  "* Construct object from code"

    ncode = 6
    
    call remove_object (main)
    allocate (wrapper_t :: main)
    call it%init (main)
    do i = 1, ncode
       call code%read (utmp)
       call build_object (object, code, repository)
       if (associated (object)) then
          call it%advance (import_object = object)
       else
          call it%advance ()
       end if
       call it%get_object (object)
       select type (object)
       class is (value_t)
          call object%init_from_code (code)
       end select
    end do
    close (utmp)
 
    write (u, "(A)")
    select type (main)
    class is (wrapper_t);  call main%get_core_ptr (object)
       call object%write (u)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (main)
    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_integer_10"

  end subroutine object_integer_10


  subroutine init_members (object, n_arg)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: n_arg
    select type (object)
    class is (operator_t)
       call object%init_args (n_arg)
    end select
  end subroutine init_members
    
  subroutine set_member_val (object, i, value, inverse)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    integer, intent(in), optional :: value
    logical, intent(in), optional :: inverse
    class(object_t), pointer :: member, core
    class(composite_t), pointer :: prototype
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
       call object%get_prototype_ptr (prototype)
       call prototype%instantiate (member)
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
    if (present (inverse)) then
       select type (object)
       class is (integer_binary_t)
          call object%tag_inverse (i, inverse)
       end select
    end if
  end subroutine set_member_val
    

end module object_integer_uti
