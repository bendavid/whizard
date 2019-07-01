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

module object_logical_uti
  
  use iso_varying_string, string_t => varying_string
  use io_units
  use codes
  use object_base
  use object_builder
  use object_expr

  use object_logical

  implicit none
  private

  public :: object_logical_1
  public :: object_logical_2
  public :: object_logical_3
  public :: object_logical_4
  public :: object_logical_5
  public :: object_logical_6
  public :: object_logical_7
  public :: object_logical_8
  public :: object_logical_9
  public :: object_logical_10

contains
  
  subroutine object_logical_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: bare_logical, core
    class(object_t), pointer :: prototype, true, false, undef

    write (u, "(A)")  "* Test output: object_logical_1"
    write (u, "(A)")  "*   Purpose: construct logical value objects"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Logical objects: prototype"

    allocate (logical_t :: bare_logical)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("logical"))
       call prototype%import_core (bare_logical)
    end select

    write (u, "(A)")
    call prototype%write (u, refcount=.true.)

    write (u, "(A)")
    write (u, "(A)")  "* Logical objects: true, false, undefined"

    call prototype%instantiate (true)
    select type (true)
    type is (composite_t)
       call true%init (mode = MODE_CONSTANT, name = var_str ("true"))
       allocate (logical_t :: core)
       select type (core)
       type is (logical_t);  call core%init (value=.true.)
       end select
       call true%import_core (core)
    end select

    call prototype%instantiate (false)
    select type (false)
    type is (composite_t)
       call false%init (mode = MODE_CONSTANT, name = var_str ("false"))
       allocate (logical_t :: core)
       select type (core)
       type is (logical_t);  call core%init (value=.false.)
       end select
       call false%import_core (core)
    end select

    call prototype%instantiate (undef)
    select type (undef)
    type is (composite_t)
       call undef%init (mode = MODE_CONSTANT, name = var_str ("undef"))
       allocate (logical_t :: core)
       call undef%import_core (core)
    end select

    write (u, "(A)")
    call prototype%write (u, refcount=.true.)

    write (u, "(A)")
    call true%write (u, refcount=.true.)
    call false%write (u, refcount=.true.)
    call undef%write (u, refcount=.true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (undef)
    call remove_object (true)
    call remove_object (false)
    call remove_object (prototype)
    call remove_object (bare_logical)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_1"
    
    end subroutine object_logical_1

  subroutine object_logical_2 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core, rhs
    class(object_t), pointer :: prototype, true, false, lval1, lval2, lval3
    type(assignment_t) :: asg
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_2"
    write (u, "(A)")  "*   Purpose: assignments"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Create objects"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select
    
    call prototype%instantiate (true)
    select type (true)
    type is (composite_t)
       call true%init (mode = MODE_CONSTANT, name = var_str ("true"))
       allocate (logical_t :: core)
       select type (core)
       type is (logical_t);  call core%init (value = .true.)
       end select
       call true%import_core (core)
    end select

    call prototype%instantiate (false)
    select type (false)
    type is (composite_t)
       call false%init (mode = MODE_CONSTANT, name = var_str ("false"))
       allocate (logical_t :: core)
       select type (core)
       type is (logical_t);  call core%init (value = .false.)
       end select
       call false%import_core (core)
    end select

    call prototype%instantiate (lval1)
    select type (lval1)
    type is (composite_t)
       call lval1%init (mode = MODE_VARIABLE, name = var_str ("lval1"))
    end select
    
    call prototype%instantiate (lval2)
    select type (lval2)
    type is (composite_t)
       call lval2%init (mode = MODE_VARIABLE, name = var_str ("lval2"))
    end select
    
    call prototype%instantiate (lval3)
    select type (lval3)
    type is (composite_t)
       call lval3%init (mode = MODE_VARIABLE, name = var_str ("lval3"))
    end select
    
    write (u, "(A)")
    call true%write (u)
    call false%write (u)

    write (u, "(A)")
    call lval1%write_as_declaration (u)
    call lval2%write_as_declaration (u)
    call lval3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* lval1 = true"
  
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (lval1)
    call asg%set_rhs (true, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call lval1%write_as_declaration (u)
    call lval2%write_as_declaration (u)
    call lval3%write_as_declaration (u)
  
    write (u, "(A)")
    write (u, "(A)")  "* lval2 = false"
  
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (lval2)
    call asg%set_rhs (false, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()
    
    write (u, "(A)")
    call lval1%write_as_declaration (u)
    call lval2%write_as_declaration (u)
    call lval3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* lval2 = lval1"
  
    rhs => lval1
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (lval2)
    call asg%set_rhs (rhs, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call lval1%write_as_declaration (u)
    call lval2%write_as_declaration (u)
    call lval3%write_as_declaration (u)
  
    write (u, "(A)")
    write (u, "(A)")  "* lval2 = lval3"
  
    rhs => lval3
    call asg%init (mode=MODE_CONSTANT)
    call asg%set_lhs (lval2)
    call asg%set_rhs (rhs, link=.true.)
    call asg%resolve (success)

    write (u, "(A)")
    write (u, "(A,L1)")  "success = ", success
    if (success) then
       call asg%evaluate ()
    end if
    call asg%final ()

    write (u, "(A)")
    call lval1%write_as_declaration (u)
    call lval2%write_as_declaration (u)
    call lval3%write_as_declaration (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (lval1)
    call remove_object (lval2)
    call remove_object (lval3)
    call remove_object (true)
    call remove_object (false)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A,1x,L1)")  "prototype allocated =", associated (prototype)
    write (u, "(A,1x,L1)")  "true allocated =", associated (true)
    write (u, "(A,1x,L1)")  "false allocated =", associated (false)
    write (u, "(A,1x,L1)")  "lval1 allocated =", associated (lval1)
    write (u, "(A,1x,L1)")  "lval2 allocated =", associated (lval2)
    write (u, "(A,1x,L1)")  "lval3 allocated =", associated (lval3)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_2"
    
    end subroutine object_logical_2

  subroutine object_logical_3 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: prototype, proto2, main, val, core, rhs, asg
    type(object_iterator_t) :: it
    class(object_t), pointer :: object
    type(code_t) :: code
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_3"
    write (u, "(A)")  "*   Purpose: simple composite assignment"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare composite object with primer"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
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
    write (u, "(A)")  "* Check name mismatch"

    call remove_object (main)

    call prototype%instantiate (val)
    select type (val)
    type is (composite_t)
       call val%init (mode=MODE_CONSTANT, name = var_str ("foo"))
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
    write (u, "(A)")  "* Check mode/type mismatch"

    call remove_object (main)

    allocate (composite_t :: proto2)
    select type (proto2)
    type is (composite_t)
       call proto2%init (var_str ("tag"))
       allocate (tag_t :: core)
       call proto2%import_core (core)
    end select

    call proto2%instantiate (val)
    select type (val)
    type is (composite_t)
       call val%init (name = var_str ("val"))
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
    write (u, "(A)")  "* Test output end: object_logical_3"
    
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
              type is (logical_t)
                 call core%init (value = .true.)
              end select
           end select
           call asg%init (MODE=MODE_CONSTANT)
           call asg%set_path ([var_str ("val")])
           call asg%set_rhs (rhs=rhs, link=.false.)
        end select
      end subroutine setup_assignment

    end subroutine object_logical_3

  subroutine object_logical_4 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: pro_logical, pro_tag
    class(object_t), pointer :: main, foo, bar, a, core, member, rhs, asg, ptr
    type(object_iterator_t) :: it
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_4"
    write (u, "(A)")  "*   Purpose: nested composite assignment"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare composite object with primer"

    allocate (composite_t :: pro_tag)
    select type (pro_tag)
    type is (composite_t)
       call pro_tag%init (var_str ("tag"))
       allocate (tag_t :: core)
       call pro_tag%import_core (core)
    end select

    allocate (composite_t :: pro_logical)
    select type (pro_logical)
    type is (composite_t)
       call pro_logical%init (var_str ("logical"))
       allocate (logical_t :: core)
       call pro_logical%import_core (core)
    end select

    call pro_logical%instantiate (bar)
    select type (bar)
    type is (composite_t)
       call bar%init (mode=MODE_CONSTANT, name = var_str ("bar"))
    end select

    call pro_tag%instantiate (a)
    select type (a)
    type is (composite_t)
       call a%init (name = var_str ("a"))
    end select

    call pro_logical%instantiate (foo)
    select type (foo)
    type is (composite_t)
       call foo%init (mode=MODE_CONSTANT, name = var_str ("foo"), &
            n_members=2)
       call foo%import_member (1, a)
       call foo%import_member (2, bar)
    end select

    allocate (assignment_t :: asg)
    select type (asg)
    type is (assignment_t)
       allocate (logical_t :: core)
       select type (core)
       type is (logical_t)
          call core%init (value = .true.)
       end select

       call pro_logical%instantiate (bar)
       select type (bar)
       type is (composite_t)
          call bar%init (mode=MODE_CONSTANT, name = var_str ("bar"))
          call bar%get_core_ptr (core)
          select type (core)
          type is (logical_t);  call core%init (value = .false.)
          end select
       end select

       call pro_tag%instantiate (a)
       select type (a)
       type is (composite_t)
          call a%init (name = var_str ("a"))
       end select

       allocate (composite_t :: rhs)
       select type (rhs)
       type is (composite_t)
          call rhs%init (mode=MODE_CONSTANT, name = var_str ("rhs"), &
               n_members = 2)
          allocate (logical_t :: core)
          select type (core)
          type is (logical_t);  call core%init (value = .true.)
          end select
          call rhs%import_core (core)
          call rhs%import_member (1, bar)
          call rhs%import_member (2, a)
       end select

       call asg%init (mode=MODE_CONSTANT)
       call asg%set_path ([var_str ("foo")])
       call asg%set_rhs (rhs=rhs, link=.false.)
    end select

    allocate (composite_t :: main)
    select type (main)
    type is (composite_t)
       call main%init (name = var_str ("main"), n_members = 1, n_primers = 1)
       call main%import_member (1, foo)
       call main%import_primer (1, asg)
    end select

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
    write (u, "(A)")  "* Iterate through main"
    write (u, "(A)")      

    call it%init (main)
    do while (it%is_valid ())
       call it%get_object (ptr)
       call ptr%write (u, core=.false., mantle=.false.)
       call it%write (u)
       write (u, *)
       call it%advance ()
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (main)
    call remove_object (pro_tag)
    call remove_object (pro_logical)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_4"
    
    end subroutine object_logical_4

  subroutine object_logical_5 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: not, expr1, expr2, expr3
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_5"
    write (u, "(A)")  "*   Purpose: check logical operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: not"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (not_t :: not)
    select type (not)
    type is (not_t)
       call not%init (prototype)
    end select
    
    write (u, "(A)")
    call not%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"

    write (u, "(A)")
    
    call not%instantiate (expr1)
    call init_members (expr1, 1)
    call set_member_val (expr1, 1, .true.)
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call not%instantiate (expr2)
    call init_members (expr2, 1)
    call set_member_val (expr2, 1, .false.)
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call not%instantiate (expr3)
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
    call remove_object (not)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A,1x,L1)")  "prototype allocated =", associated (prototype)
    write (u, "(A,1x,L1)")  "not allocated =", associated (not)
    write (u, "(A,1x,L1)")  "expr1 allocated =", associated (expr1)
    write (u, "(A,1x,L1)")  "expr2 allocated =", associated (expr2)
    write (u, "(A,1x,L1)")  "expr3 allocated =", associated (expr3)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_5"
    
    end subroutine object_logical_5

  subroutine object_logical_6 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: and
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6, expr7
    class(object_t), pointer :: expr8, expr9
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_6"
    write (u, "(A)")  "*   Purpose: check logical operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: and"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (and_t :: and)
    select type (and)
    type is (and_t)
       call and%init (prototype)
    end select
    
    write (u, "(A)")
    call and%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call and%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, .true.)
    call set_member_val (expr1, 2, .true.) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, .true.)
    call set_member_val (expr2, 2, .false.) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, .false.)
    call set_member_val (expr3, 2, .true.) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr4)
    call init_members (expr4, 2)
    call set_member_val (expr4, 1, .false.)
    call set_member_val (expr4, 2, .false.) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr5)
    call init_members (expr5, 2)
    call set_member_val (expr5, 1)
    call set_member_val (expr5, 2, .true.) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr6)
    call init_members (expr6, 2)
    call set_member_val (expr6, 1, .false.)
    call set_member_val (expr6, 2) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr7)
    call init_members (expr7, 2)
    call set_member_val (expr7, 1)
    call set_member_val (expr7, 2) 
    call expr7%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, *)

    call and%instantiate (expr8)
    call init_members (expr8, 3)
    call set_member_val (expr8, 1, .true.)
    call set_member_val (expr8, 2, .true.) 
    call set_member_val (expr8, 3, .false.) 
    call expr8%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr9)
    call init_members (expr9, 4)
    call set_member_val (expr9, 1, .true.)
    call set_member_val (expr9, 2, .true.) 
    call set_member_val (expr9, 3, .true.)
    call set_member_val (expr9, 4, .true.) 
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

    write (u, "(A)")
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

    write (u, "(A)")
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
    call remove_object (and)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_6"
    
    end subroutine object_logical_6

  subroutine object_logical_7 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: or
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5, expr6, expr7
    class(object_t), pointer :: expr8, expr9
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_7"
    write (u, "(A)")  "*   Purpose: check logical operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: or"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (or_t :: or)
    select type (or)
    type is (or_t)
       call or%init (prototype)
    end select
    
    write (u, "(A)")
    call or%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call or%instantiate (expr1)
    call init_members (expr1, 2)
    call set_member_val (expr1, 1, .true.)
    call set_member_val (expr1, 2, .true.) 
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr2)
    call init_members (expr2, 2)
    call set_member_val (expr2, 1, .true.)
    call set_member_val (expr2, 2, .false.) 
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, .false.)
    call set_member_val (expr3, 2, .true.) 
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr4)
    call init_members (expr4, 2)
    call set_member_val (expr4, 1, .false.)
    call set_member_val (expr4, 2, .false.) 
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr5)
    call init_members (expr5, 2)
    call set_member_val (expr5, 1)
    call set_member_val (expr5, 2, .true.) 
    call expr5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr6)
    call init_members (expr6, 2)
    call set_member_val (expr6, 1, .false.)
    call set_member_val (expr6, 2) 
    call expr6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr7)
    call init_members (expr7, 2)
    call set_member_val (expr7, 1)
    call set_member_val (expr7, 2) 
    call expr7%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    write (u, *)

    call or%instantiate (expr8)
    call init_members (expr8, 3)
    call set_member_val (expr8, 1, .false.)
    call set_member_val (expr8, 2, .false.) 
    call set_member_val (expr8, 3, .true.) 
    call expr8%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call or%instantiate (expr9)
    call init_members (expr9, 4)
    call set_member_val (expr9, 1, .false.)
    call set_member_val (expr9, 2, .false.) 
    call set_member_val (expr9, 3, .false.)
    call set_member_val (expr9, 4, .false.) 
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

    write (u, "(A)")
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

    write (u, "(A)")
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
    call remove_object (or)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_7"
    
    end subroutine object_logical_7

  subroutine object_logical_8 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: prototype
    class(object_t), pointer :: not, and, or
    class(object_t), pointer :: expr1, expr2, expr3, expr4, expr5
    class(object_t), pointer :: arg1, arg2
    logical :: success

    write (u, "(A)")  "* Test output: object_logical_8"
    write (u, "(A)")  "*   Purpose: check nested logical expressions"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototypes: not, and, or"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select
    
    allocate (not_t :: not)
    select type (not)
    type is (not_t)
       call not%init (prototype)
    end select
    
    allocate (and_t :: and)
    select type (and)
    type is (and_t)
       call and%init (prototype)
    end select
    
    allocate (or_t :: or)
    select type (or)
    type is (or_t)
       call or%init (prototype)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call and%instantiate (expr1)
    call init_members (expr1, 2)
    select type (expr1)
    class is (composite_t)
       call not%instantiate (arg1)
       call init_members (arg1, 1)
       call set_member_val (arg1, 1, .true.)
       call expr1%import_member (1, arg1)
    end select
    call set_member_val (expr1, 2, .false.)
    call expr1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call not%instantiate (expr2)
    call init_members (expr2, 1)
    select type (expr2)
    class is (composite_t)
       call and%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, .true.)
       call set_member_val (arg1, 2, .false.)
       call expr2%import_member (1, arg1)
    end select
    call expr2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr3)
    call init_members (expr3, 2)
    call set_member_val (expr3, 1, .true.)
    select type (expr3)
    class is (composite_t)
       call or%instantiate (arg2)
       call init_members (arg2, 2)
       call set_member_val (arg2, 1, .false.)
       call set_member_val (arg2, 2, .true.)
       call expr3%import_member (2, arg2)
    end select
    call expr3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr4)
    call init_members (expr4, 2)
    call set_member_val (expr4, 1, .true.)
    select type (expr4)
    class is (composite_t)
       call and%instantiate (arg2)
       call init_members (arg2, 2)
       call set_member_val (arg2, 1, .false.)
       call set_member_val (arg2, 2, .true.)
       call expr4%import_member (2, arg2)
    end select
    call expr4%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call and%instantiate (expr5)
    call init_members (expr5, 2)
    select type (expr5)
    class is (composite_t)
       call and%instantiate (arg1)
       call init_members (arg1, 2)
       call set_member_val (arg1, 1, .true.)
       call set_member_val (arg1, 2, .false.)
       call expr5%import_member (1, arg1)
    end select
    call set_member_val (expr5, 2, .true.)
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
    call remove_object (and)
    call remove_object (or)
    call remove_object (not)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_8"
    
    end subroutine object_logical_8

  subroutine object_logical_9 (u)
    integer, intent(in) :: u
    type(repository_t), allocatable :: repository
    type(object_builder_t) :: builder
    class(object_t), pointer :: prototype, main, val, core, rhs, asg
    type(code_t) :: code
    logical :: success
    integer :: iostat
    integer :: utmp

    write (u, "(A)")  "* Test output: object_logical_9"
    write (u, "(A)")  "*   Purpose: simple composite assignment"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare repository"

    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (var_str ("logical"))
       allocate (logical_t :: core)
       call prototype%import_core (core)
    end select

    allocate (assignment_t :: asg)
    select type (asg)
    type is (assignment_t)
       call asg%init ()
    end select

    allocate (repository)
    call repository%init (name = var_str ("repository"), n_members = 2)
    call repository%import_member (1, prototype)
    call repository%import_member (2, asg)


    write (u, "(A)")
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Prepare composite object with primer"

    call repository%spawn (var_str ("logical"), val)
    select type (val)
    class is (composite_t)
       call val%init (name = var_str ("val"), mode = MODE_CONSTANT)
    end select
    
    call repository%spawn (var_str ("assignment"), asg)
    select type (asg)
    type is (assignment_t)
       call repository%spawn (var_str ("logical"), rhs)
       select type (rhs)
       type is (composite_t)
          call rhs%init (name = var_str ("rhs"), mode = MODE_CONSTANT)
          call rhs%get_core_ptr (core)
          select type (core)
          type is (logical_t)
             call core%init (value = .true.)
          end select
       end select
       call asg%set_path ([var_str ("val")])
       call asg%set_rhs (rhs=rhs, link=.false.)
    end select

    allocate (composite_t :: main)
    select type (main)
    type is (composite_t)
       call main%init (name = var_str ("main"), n_members = 1, n_primers = 1)
       call main%import_member (1, val)
       call main%import_primer (1, asg)
    end select

    write (u, "(A)")
    call main%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Code"

    call builder%import_repository (repository)

    utmp = free_unit ()
    open (utmp, status="scratch")

    write (u, "(A)")
    call builder%init_object (main)
    do
       call builder%decode (code, success)
       if (.not. success)  exit
       call code%write (u, verbose=.true.)
       call code%write (utmp)
    end do
    rewind (utmp)

    write (u, "(A)")
    write (u, "(A)")  "* Reconstruct object"

    call remove_object (main)

    call builder%init_empty ()
    do
       call code%read (utmp, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    call builder%export (main)

    close (utmp)
    
    write (u, "(A)")
    call main%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (prototype)
    call remove_object (main)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_9"
    
  end subroutine object_logical_9

  subroutine object_logical_10 (u)
    integer, intent(in) :: u
    type(repository_t) :: repository
    class(object_t), pointer :: p_log, p_not, p_and, p_or, core, main, object
    class(object_t), pointer :: val1, val2, expr
    integer :: utmp, ncode, i
    character(80) :: buffer
    type(code_t) :: code
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_logical_10"
    write (u, "(A)")  "*   Purpose: construct expressions from code"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare repository"

    allocate (composite_t :: p_log)
    select type (p_log)
    type is (composite_t)
       call p_log%init (var_str ("logical"))
       allocate (logical_t :: core)
       call p_log%import_core (core)
    end select

    allocate (not_t :: p_not)
    select type (p_not)
    type is (not_t)
       call p_not%init (p_log)
    end select

    allocate (and_t :: p_and)
    select type (p_and)
    type is (and_t)
       call p_and%init (p_log)
    end select

    allocate (or_t :: p_or)
    select type (p_or)
    type is (or_t)
       call p_or%init (p_log)
    end select

    call repository%init (name = var_str ("repository"), n_members = 4)
    call repository%import_member (1, p_log)
    call repository%import_member (2, p_not)
    call repository%import_member (3, p_and)
    call repository%import_member (4, p_or)

    write (u, "(A)")
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Construct object: not"
    

    call repository%spawn (var_str ("logical"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_true"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (logical_t)
          call core%init (.true.)
       end select
    end select

    call repository%spawn (var_str ("not"), expr)
    select type (expr)
    class is (not_t)
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
    write (u, "(A)")  "* Construct object: and"

    call remove_object (object)

    call repository%spawn (var_str ("logical"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_true"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (logical_t)
          call core%init (.true.)
       end select
    end select

    call repository%spawn (var_str ("logical"), val2)
    select type (val2)
    class is (composite_t)
       call val2%init (name = var_str ("val_false"), mode = MODE_CONSTANT)
       call val2%get_core_ptr (core)
       select type (core)
       type is (logical_t)
          call core%init (.false.)
       end select
    end select

    call repository%spawn (var_str ("and"), expr)
    select type (expr)
    class is (and_t)
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
    write (u, "(A)")  "* Construct object: or"

    call remove_object (object)

    call repository%spawn (var_str ("logical"), val1)
    select type (val1)
    class is (composite_t)
       call val1%init (name = var_str ("val_true"), mode = MODE_CONSTANT)
       call val1%get_core_ptr (core)
       select type (core)
       type is (logical_t)
          call core%init (.true.)
       end select
    end select

    call repository%spawn (var_str ("logical"), val2)
    select type (val2)
    class is (composite_t)
       call val2%init (name = var_str ("val_false"), mode = MODE_CONSTANT)
       call val2%get_core_ptr (core)
       select type (core)
       type is (logical_t)
          call core%init (.false.)
       end select
    end select

    call repository%spawn (var_str ("or"), expr)
    select type (expr)
    class is (or_t)
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
    write (u, "(A)")  "* Cleanup"

    call remove_object (main)
    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_logical_10"

  end subroutine object_logical_10


  subroutine init_members (object, n_arg)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: n_arg
    select type (object)
    class is (operator_t)
       call object%init_args (n_arg)
    end select
  end subroutine init_members
    
  subroutine set_member_val (object, i, value)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    logical, intent(in), optional :: value
    class(object_t), pointer :: member, core
    class(composite_t), pointer :: prototype
    type(string_t) :: name
    if (present (value)) then
       if (value) then
          name = "true"
       else
          name = "false"
       end if
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
          class is (logical_t)
             call core%init (value)
          end select
       end select
       call object%import_member (i, member)
    end select
  end subroutine set_member_val
    

end module object_logical_uti
