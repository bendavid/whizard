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

module object_conditional_uti
  
    use iso_varying_string, string_t => varying_string
    use object_base
    use object_logical
    use object_integer

    use object_conditional

  implicit none
  private

  public :: object_conditional_1

contains
  
  subroutine object_conditional_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: pro_log, pro_int
    class(object_t), pointer :: conditional
    class(object_t), pointer :: a1, a2, a3, a4, a5, a6
    logical :: success

    write (u, "(A)")  "* Test output: object_conditional_1"
    write (u, "(A)")  "*   Purpose: check integer conditional operators"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: conditional"

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
    
    allocate (conditional_t :: conditional)
    select type (conditional)
    type is (conditional_t)
       call conditional%init (pro_log, pro_int)
    end select
    
    write (u, "(A)")
    call conditional%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call conditional%instantiate (a1)
    call init_members (a1, 2)
    call set_branch_val (a1, 1, 2)
    call set_condit_val (a1, 1, .true.)
    call set_branch_val (a1, 2, 3)
    call a1%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call conditional%instantiate (a2)
    call init_members (a2, 2)
    call set_branch_val (a2, 1, 2)
    call set_condit_val (a2, 1, .false.)
    call set_branch_val (a2, 2, 3) 
    call a2%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call conditional%instantiate (a3)
    call init_members (a3, 2)
    call set_branch_val (a3, 1, 2)
    call set_condit_val (a3, 1, .false.)
    call set_branch_val (a3, 2)
    call a3%resolve (success)
    write (u, "(A,L1)")  "success = ", success

!     call conditional%instantiate (a4)
!     call init_members (a4, 2)
!     call set_branch_val (a4, 1, 2)
!     call set_branch_val (a4, 2, 3, CMP_GT) 
!     call a4%resolve (success)
!     write (u, "(A,L1)")  "success = ", success
! 
!     call conditional%instantiate (a5)
!     call init_members (a5, 2)
!     call set_branch_val (a5, 1, 2)
!     call set_branch_val (a5, 2, 3, CMP_LE) 
!     call a5%resolve (success)
!     write (u, "(A,L1)")  "success = ", success
! 
!     call conditional%instantiate (a6)
!     call init_members (a6, 2)
!     call set_branch_val (a6, 1, 2)
!     call set_branch_val (a6, 2, 3, CMP_GE) 
!     call a6%resolve (success)
!     write (u, "(A,L1)")  "success = ", success

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
!     call a4%write (u)
!     call a5%write (u)
!     call a6%write (u)

    write (u, "(A)")
    call a1%write_as_expression (u)
    write (u, *)
    call a2%write_as_expression (u)
    write (u, *)
    call a3%write_as_expression (u)
    write (u, *)
!     call a4%write_as_expression (u)
!     write (u, *)
!     call a5%write_as_expression (u)
!     write (u, *)
!     call a6%write_as_expression (u)
!     write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
  
    call a1%evaluate ()
    call a2%evaluate ()
    call a3%evaluate ()
!     call a4%evaluate ()
!     call a5%evaluate ()
!     call a6%evaluate ()

    write (u, "(A)")
    call a1%write (u)
    call a2%write (u)
    call a3%write (u)
!     call a4%write (u)
!     call a5%write (u)
!     call a6%write (u)

    write (u, "(A)")
    call a1%write_as_value (u)
    write (u, *)
    call a2%write_as_value (u)
    write (u, *)
    call a3%write_as_value (u)
    write (u, *)
!     call a4%write_as_value (u)
!     write (u, *)
!     call a5%write_as_value (u)
!     write (u, *)
!     call a6%write_as_value (u)
!     write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a3)
!     call remove_object (a4)
!     call remove_object (a5)
!     call remove_object (a6)
    call remove_object (conditional)
    call remove_object (pro_log)
    call remove_object (pro_int)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_conditional_1"
    
  end subroutine object_conditional_1


  subroutine init_members (object, n_branches)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: n_branches
    select type (object)
    class is (conditional_t)
       call object%init_branches (n_branches)
    end select
  end subroutine init_members
    
  subroutine set_condit_val (object, i, value)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    logical, intent(in), optional :: value
    class(composite_t), pointer :: prototype
    class(object_t), pointer :: member, core, cond_prototype
    type(string_t) :: name
    if (present (value)) then
       select case (i)
       case (1);  name = "c1"
       case (2);  name = "c2"
       case (3);  name = "c3"
       case (4);  name = "c4"
       end select
    else
       name = "undef"
    end if
    select type (object)
    class is (conditional_t)
       call object%get_cond_prototype_ptr (cond_prototype)
       call cond_prototype%instantiate (member)
       select type (member)
       class is (composite_t)
          call member%init (name = name, mode = MODE_CONSTANT)
          call member%get_core_ptr (core)
          select type (core)
          class is (logical_t)
             call core%init (value)
          end select
       end select
       call object%import_member (2*i, member)
    end select
  end subroutine set_condit_val
    
  subroutine set_branch_val (object, i, value)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    integer, intent(in), optional :: value
    class(composite_t), pointer :: prototype
    class(object_t), pointer :: member, core
    type(string_t) :: name
    if (present (value)) then
       select case (i)
       case (1);  name = "i1"
       case (2);  name = "i2"
       case (3);  name = "i3"
       case (4);  name = "i4"
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
       call object%import_member (2*i-1, member)
    end select
  end subroutine set_branch_val
    

end module object_conditional_uti
