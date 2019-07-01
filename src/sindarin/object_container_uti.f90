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

module object_container_uti
  
  use iso_varying_string, string_t => varying_string
  use io_units
  use codes
  use object_base
  use object_logical
  use object_integer

  use object_container

  implicit none
  private

  public :: object_container_1
  public :: object_container_2
  public :: object_container_3
  public :: object_container_4

contains

  subroutine object_container_1 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: pro_log, pro_int
    class(object_t), pointer :: container
    class(object_t), pointer :: a1, a2, a3, a4, a5, a6
    logical :: success

    write (u, "(A)")  "* Test output: object_container_1"
    write (u, "(A)")  "*   Purpose: construct containers"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: container"

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
    
    allocate (container_t :: container)
    select type (container)
    class is (composite_t)
       call container%init (var_str ("container"))
    end select
    
    write (u, "(A)")
    call container%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call container%instantiate (a1)
    call init_container (a1, var_str ("a1"), CT_TUPLE, 0)

    call container%instantiate (a2)
    call init_container (a2, var_str ("a2"), CT_TUPLE, 1)
    call set_member_int (a2, 1, pro_int, 1)

    call container%instantiate (a3)
    call init_container (a3, var_str ("a3"), CT_TUPLE, 2)
    call set_member_int (a3, 1, pro_int, 1)
    call set_member_log (a3, 2, pro_log, .true.) 

    call container%instantiate (a4)
    call init_container (a4, var_str ("a4"), CT_TUPLE, 3)
    call set_member_int (a4, 1, pro_int, 1)
    call set_member_log (a4, 2, pro_log, .true.) 
    call set_member_int (a4, 3, pro_int) 

    call container%instantiate (a5)
    call init_container (a5, var_str ("a5"), CT_LIST, 2)
    call set_member_int (a5, 1, pro_int, 1)
    call set_member_log (a5, 2, pro_log, .true.) 

    call container%instantiate (a6)
    call init_container (a6, var_str ("a6"), CT_SEQUENCE, 2)
    call set_member_int (a6, 1, pro_int, 1)
    call set_member_log (a6, 2, pro_log, .true.) 

    call a1%write (u)
    write (u, "(A)")
    call a2%write (u)
    write (u, "(A)")
    call a3%write (u)
    write (u, "(A)")
    call a4%write (u)
    write (u, "(A)")
    call a5%write (u)
    write (u, "(A)")
    call a6%write (u)
    write (u, "(A)")

    call a1%write_as_expression (u)
    write (u, "(A)")
    call a2%write_as_expression (u)
    write (u, "(A)")
    call a3%write_as_expression (u)
    write (u, "(A)")
    call a4%write_as_expression (u)
    write (u, "(A)")
    call a5%write_as_expression (u)
    write (u, "(A)")
    call a6%write_as_expression (u)
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)")  "* Resolve and evaluate"
    write (u, "(A)")
  
    call a1%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a2%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a3%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a4%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a5%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a6%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call a1%evaluate ()
    call a2%evaluate ()
    call a3%evaluate ()
    call a4%evaluate ()
    call a5%evaluate ()
    call a6%evaluate ()

    write (u, "(A)")
    call a1%write (u)
    write (u, "(A)")
    call a2%write (u)
    write (u, "(A)")
    call a3%write (u)
    write (u, "(A)")
    call a4%write (u)
    write (u, "(A)")
    call a5%write (u)
    write (u, "(A)")
    call a6%write (u)

    write (u, *)
    call a1%write_as_value (u)
    write (u, "(A)")
    call a2%write_as_value (u)
    write (u, "(A)")
    call a3%write_as_value (u)
    write (u, "(A)")
    call a4%write_as_value (u)
    write (u, "(A)")
    call a5%write_as_value (u)
    write (u, "(A)")
    call a6%write_as_value (u)
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a3)
    call remove_object (a4)
    call remove_object (a5)
    call remove_object (a6)
    call remove_object (container)
    call remove_object (pro_log)
    call remove_object (pro_int)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_container_1"
    
  end subroutine object_container_1

  subroutine object_container_2 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: core
    class(object_t), pointer :: pro_int
    class(object_t), pointer :: container
    class(object_t), pointer :: a1, a2, a3, a4, a5
    class(object_t), pointer :: b1, c1
    logical :: success
    
    write (u, "(A)")  "* Test output: object_container_2"
    write (u, "(A)")  "*   Purpose: construct nested containers"
    write (u, "(A)")      
    
    write (u, "(A)") "* Create expression prototype: container"

    allocate (composite_t :: pro_int)
    select type (pro_int)
    type is (composite_t)
       call pro_int%init (var_str ("integer"))
       allocate (integer_t :: core)
       call pro_int%import_core (core)
    end select
    
    allocate (container_t :: container)
    select type (container)
    class is (composite_t)
       call container%init (var_str ("container"))
    end select
    
    write (u, "(A)")
    call container%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call container%instantiate (a1)
    call init_container (a1, var_str ("a1"), CT_TUPLE, 2)
    select type (a1)
    class is (container_t)
       call container%instantiate (b1)
       call init_container (b1, var_str ("item1"), CT_LIST, 2)
       select type (b1)
       class is (container_t)
          call container%instantiate (c1)
          call init_container (c1, var_str ("item1"), CT_SEQUENCE, 2)
          call set_member_int (c1, 1, pro_int, 1)
          call set_member_int (c1, 2, pro_int, 2)
          call b1%import_member (1, c1)
       end select
       call set_member_int (b1, 2, pro_int, 3)
       call a1%import_member (1, b1)
    end select
    call set_member_int (a1, 2, pro_int, 4)
    
    call container%instantiate (a2)
    call init_container (a2, var_str ("a2"), CT_TUPLE, 2)
    select type (a2)
    class is (container_t)
       call container%instantiate (b1)
       call init_container (b1, var_str ("item1"), CT_SEQUENCE, 2)
       select type (b1)
       class is (container_t)
          call container%instantiate (c1)
          call init_container (c1, var_str ("item1"), CT_LIST, 2)
          call set_member_int (c1, 1, pro_int, 1)
          call set_member_int (c1, 2, pro_int, 2)
          call b1%import_member (1, c1)
       end select
       call set_member_int (b1, 2, pro_int, 3)
       call a2%import_member (1, b1)
    end select
    call set_member_int (a2, 2, pro_int, 4)
    
    call container%instantiate (a3)
    call init_container (a3, var_str ("a3"), CT_LIST, 2)
    select type (a3)
    class is (container_t)
       call container%instantiate (b1)
       call init_container (b1, var_str ("item1"), CT_SEQUENCE, 2)
       select type (b1)
       class is (container_t)
          call container%instantiate (c1)
          call init_container (c1, var_str ("item1"), CT_TUPLE, 2)
          call set_member_int (c1, 1, pro_int, 1)
          call set_member_int (c1, 2, pro_int, 2)
          call b1%import_member (1, c1)
       end select
       call set_member_int (b1, 2, pro_int, 3)
       call a3%import_member (1, b1)
    end select
    call set_member_int (a3, 2, pro_int, 4)
    
    call container%instantiate (a4)
    call init_container (a4, var_str ("a4"), CT_SEQUENCE, 2)
    select type (a4)
    class is (container_t)
       call container%instantiate (b1)
       call init_container (b1, var_str ("item1"), CT_LIST, 2)
       select type (b1)
       class is (container_t)
          call container%instantiate (c1)
          call init_container (c1, var_str ("item1"), CT_TUPLE, 2)
          call set_member_int (c1, 1, pro_int, 1)
          call set_member_int (c1, 2, pro_int, 2)
          call b1%import_member (1, c1)
       end select
       call set_member_int (b1, 2, pro_int, 3)
       call a4%import_member (1, b1)
    end select
    call set_member_int (a4, 2, pro_int, 4)
    
    call container%instantiate (a5)
    call init_container (a5, var_str ("a5"), CT_LIST, 2)
    select type (a5)
    class is (container_t)
       call container%instantiate (b1)
       call init_container (b1, var_str ("item1"), CT_LIST, 2)
       select type (b1)
       class is (container_t)
          call container%instantiate (c1)
          call init_container (c1, var_str ("item1"), CT_TUPLE, 2)
          call set_member_int (c1, 1, pro_int, 1)
          call set_member_int (c1, 2, pro_int, 2)
          call b1%import_member (1, c1)
       end select
       call set_member_int (b1, 2, pro_int, 3)
       call a5%import_member (1, b1)
    end select
    call set_member_int (a5, 2, pro_int, 4)

    call a1%write (u)
    write (u, "(A)")
    call a2%write (u)
    write (u, "(A)")
    call a3%write (u)
    write (u, "(A)")
    call a4%write (u)
    write (u, "(A)")
    call a5%write (u)
    write (u, "(A)")

    call a1%write_as_expression (u)
    write (u, "(A)")
    call a2%write_as_expression (u)
    write (u, "(A)")
    call a3%write_as_expression (u)
    write (u, "(A)")
    call a4%write_as_expression (u)
    write (u, "(A)")
    call a5%write_as_expression (u)
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)")  "* Resolve and evaluate"
    write (u, "(A)")
  
    call a1%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a2%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a3%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a4%resolve (success)
    write (u, "(A,L1)")  "success = ", success
    call a5%resolve (success)
    write (u, "(A,L1)")  "success = ", success

    call a1%evaluate ()
    call a2%evaluate ()
    call a3%evaluate ()
    call a4%evaluate ()
    call a5%evaluate ()

    write (u, "(A)")
    call a1%write (u)
    write (u, "(A)")
    call a2%write (u)
    write (u, "(A)")
    call a3%write (u)
    write (u, "(A)")
    call a4%write (u)
    write (u, "(A)")
    call a5%write (u)

    write (u, *)
    call a1%write_as_value (u)
    write (u, "(A)")
    call a2%write_as_value (u)
    write (u, "(A)")
    call a3%write_as_value (u)
    write (u, "(A)")
    call a4%write_as_value (u)
    write (u, "(A)")
    call a5%write_as_value (u)
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a3)
    call remove_object (a4)
    call remove_object (a5)
    call remove_object (container)
    call remove_object (pro_int)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_container_2"
    
  end subroutine object_container_2

  subroutine object_container_3 (u)
    integer, intent(in) :: u
    type(repository_t) :: repository
    class(object_t), pointer :: p_int, p_cont, core, main, object
    class(object_t), pointer :: expr, val1, val2
    integer :: utmp, ncode, i
    character(80) :: buffer
    type(code_t) :: code
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_container_3"
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

    allocate (container_t :: p_cont)
    select type (p_cont)
    type is (container_t)
       call p_cont%init (0)
    end select

    call repository%init (name = var_str ("repository"), n_members = 2)
    call repository%import_member (1, p_int)
    call repository%import_member (2, p_cont)

    write (u, "(A)")
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Construct object: list"
    
    call repository%get_member_ptr (1, p_int)

    call repository%spawn (var_str ("container"), expr)
    call init_container (expr, var_str ("container"), CT_LIST, 2)
    call set_member_int (expr, 1, p_int, 71)
    call set_member_int (expr, 2, p_int, 72)

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
    write (u, "(A)")  "* Reconstruct object from code"

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
       class is (ref_array_t)
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
    write (u, "(A)")  "* Test output end: object_container_3"

  end subroutine object_container_3

  subroutine object_container_4 (u)
    integer, intent(in) :: u
    class(object_t), pointer :: p_int, p_cont, core, main, object, expr
    class(object_t), pointer :: a1, a2, a3, a4, a5, a6
    type(repository_t) :: repository
    integer :: utmp, ncode, i
    character(80) :: buffer
    type(code_t) :: code
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_container_4"
    write (u, "(A)")  "*   Purpose: construct and encode range containers"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Prepare repository"

    allocate (composite_t :: p_int)
    select type (p_int)
    type is (composite_t)
       call p_int%init (var_str ("integer"))
       allocate (integer_t :: core)
       call p_int%import_core (core)
    end select

    allocate (container_t :: p_cont)
    select type (p_cont)
    type is (container_t)
       call p_cont%init (0)
    end select

    call repository%init (name = var_str ("repository"), n_members = 2)
    call repository%import_member (1, p_int)
    call repository%import_member (2, p_cont)

    write (u, "(A)")
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Create expressions"
    write (u, "(A)")
    
    call repository%get_member_ptr (1, p_int)

    call repository%spawn (var_str ("container"), a1)
    call init_range_container (a1, var_str ("a1"), CT_ADD)
    call set_member_int (a1, 1, p_int)
    call set_member_int (a1, 2, p_int)
    call set_member_int (a1, 3, p_int)

    call repository%spawn (var_str ("container"), a2)
    call init_range_container (a2, var_str ("a2"), CT_SUB)
    call set_member_int (a2, 1, p_int, 10)
    call set_member_int (a2, 2, p_int, 0)
    call set_member_int (a2, 3, p_int, 1)

    call repository%spawn (var_str ("container"), a3)
    call init_range_container (a3, var_str ("a3"), CT_MUL)
    call set_member_int (a3, 1, p_int, 1)
    call set_member_int (a3, 2, p_int)
    call set_member_int (a3, 3, p_int, 2)

    call repository%spawn (var_str ("container"), a4)
    call init_range_container (a4, var_str ("a4"), CT_DIV)
    call set_member_int (a4, 1, p_int, 20)
    call set_member_int (a4, 2, p_int, 2)
    call set_member_int (a4, 3, p_int, 1)

    call repository%spawn (var_str ("container"), a5)
    call init_range_container (a5, var_str ("a5"), CT_LIN)
    call set_member_int (a5, 1, p_int, 0)
    call set_member_int (a5, 2, p_int, 100)
    call set_member_int (a5, 3, p_int, 10)

    call repository%spawn (var_str ("container"), a6)
    call init_range_container (a6, var_str ("a6"), CT_LOG)
    call set_member_int (a6, 1, p_int, 1)
    call set_member_int (a6, 2, p_int, 200)
    call set_member_int (a6, 3, p_int, 5)

    call a1%write (u)
    write (u, "(A)")
    call a2%write (u)
    write (u, "(A)")
    call a3%write (u)
    write (u, "(A)")
    call a4%write (u)
    write (u, "(A)")
    call a5%write (u)
    write (u, "(A)")
    call a6%write (u)
    write (u, "(A)")

    call a1%write_as_expression (u)
    write (u, "(A)")
    call a2%write_as_expression (u)
    write (u, "(A)")
    call a3%write_as_expression (u)
    write (u, "(A)")
    call a4%write_as_expression (u)
    write (u, "(A)")
    call a5%write_as_expression (u)
    write (u, "(A)")
    call a6%write_as_expression (u)
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)")  "* Construct object: range"
    
    call repository%get_member_ptr (1, p_int)

    call repository%spawn (var_str ("container"), expr)
    call init_range_container (expr, var_str ("container"), CT_ADD)
    call set_member_int (expr, 1, p_int, 11)
    call set_member_int (expr, 2, p_int, 33)
    call set_member_int (expr, 3, p_int, 22)

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

    ncode = 8
    
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
       class is (ref_array_t)
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
    call remove_object (a1)
    call remove_object (a2)
    call remove_object (a2)
    call remove_object (a3)
    call remove_object (a4)
    call remove_object (a5)
    call remove_object (a6)
    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_container_4"
    
  end subroutine object_container_4


  subroutine init_container (object, name, ctype, n_item)
    class(object_t), intent(inout) :: object
    type(string_t), intent(in) :: name
    integer, intent(in) :: ctype
    integer, intent(in) :: n_item
    class(object_t), pointer :: core
    allocate (ref_array_t :: core)
    select type (core)
    class is (ref_array_t)
       call core%init (n_item)
    end select
    select type (object)
    class is (container_t)
       call object%init (ctype, name, MODE_CONSTANT)
       call object%import_core (core)
       call object%init_args (n_item, check=.false.)
    end select
  end subroutine init_container
    
  subroutine init_range_container (object, name, stype)
    class(object_t), intent(inout) :: object
    type(string_t), intent(in) :: name
    integer, intent(in) :: stype
    class(object_t), pointer :: core
    allocate (ref_array_t :: core)
    select type (core)
    class is (ref_array_t)
       call core%init (3)
    end select
    select type (object)
    class is (container_t)
       call object%init (CT_SEQUENCE, name, MODE_CONSTANT)
       call object%setup_range (stype)
       call object%import_core (core)
       call object%init_args (3, check=.false.)
    end select
  end subroutine init_range_container
    
  subroutine set_member_log (object, i, prototype, value)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), target :: prototype
    logical, intent(in), optional :: value
    class(object_t), pointer :: member, core
    type(string_t) :: name
    if (present (value)) then
       select case (i)
       case (1);  name = "item1"
       case (2);  name = "item2"
       case (3);  name = "item3"
       case (4);  name = "item4"
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
          class is (logical_t)
             call core%init (value)
          end select
       end select
       call object%import_member (i, member)
    end select
  end subroutine set_member_log
    
  subroutine set_member_int (object, i, prototype, value)
    class(object_t), intent(inout) :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), target :: prototype
    integer, intent(in), optional :: value
    class(object_t), pointer :: member, core
    type(string_t) :: name
    if (present (value)) then
       select case (i)
       case (1);  name = "item1"
       case (2);  name = "item2"
       case (3);  name = "item3"
       case (4);  name = "item4"
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
  end subroutine set_member_int
    
  
end module object_container_uti
