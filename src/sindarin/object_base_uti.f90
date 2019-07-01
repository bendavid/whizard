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

module object_base_uti
  
  implicit none
  private

  public :: object_base_1
  public :: object_base_2
  public :: object_base_3
  public :: object_base_4
  public :: object_base_5
  public :: object_base_6
  public :: object_base_7
  public :: object_base_8

contains
  
  subroutine object_base_1 (u)
    use codes
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: obj1, obj2
    type(code_t) :: code

    write (u, "(A)")  "* Test output: object_base_1"
    write (u, "(A)")  "*   Purpose: elementary operations with objects"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Trivial object (tag): create, instantiate, display"

    allocate (tag_t :: obj1)
    call obj1%instantiate (obj2)
    
    write (u, "(A)")
    call obj1%write (u, refcount=.true.)
    call obj2%write (u, refcount=.true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Object code:"
    write (u, "(A)")

    code = obj1%get_code ()
    call code%write (u, verbose=.true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup:"

    call remove_object (obj1)
    call remove_object (obj2)

    write (u, "(A)")
    write (u, "(A,1x,L1)")  "obj1 allocated =", associated (obj1)
    write (u, "(A,1x,L1)")  "obj2 allocated =", associated (obj2)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_1"
    
  end subroutine object_base_1

  subroutine object_base_2 (u)
    use iso_varying_string, string_t => varying_string
    use codes
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype, object1, object2, object3
    class(object_t), pointer :: member
    type(code_t) :: code

    write (u, "(A)")  "* Test output: object_base_2"
    write (u, "(A)")  "*   Purpose: build composite objects"

    write (u, "(A)")      
    write (u, "(A)")  "* Create tag prototype"
    
    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select

    write (u, "(A)")
    call prototype%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Instantiate as composite without members"
    
    call prototype%instantiate (object1)
    select type (object1)
    class is (composite_t)
       call object1%init (name = var_str ("obj1"))
    end select
   
    write (u, "(A)")
    call object1%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Instantiate as composite with two members"
    
    call prototype%instantiate (object2)
    select type (object2)
    class is (composite_t)
       call object2%tag_non_intrinsic ()
       call object2%init (name = var_str ("obj2"), n_members = 2)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("foo"))
       end select
       call object2%import_member (1, member)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("bar"))
       end select
       call object2%import_member (2, member)
    end select
    
    write (u, "(A)")
    call object2%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Code of obj2"
    
    code = object2%get_code ()

    write (u, "(A)")
    call code%write (u, verbose=.true.)

    
    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)
    call object1%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Instantiate further with additional member"
    
    call object2%instantiate (object3)
    select type (object3)
    class is (composite_t)
       call object3%init (name = var_str ("obj3"), n_members = 1)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("new"))
       end select
       call object3%import_member (1, member)
    end select
    
    write (u, "(A)")
    call object3%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)
    call object1%write (u, refcount=.true.)
    call object2%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Remove obj3"
    
    call remove_object (object3)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)
    call object1%write (u, refcount=.true.)
    call object2%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Remove obj2"
    
    call remove_object (object2)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)
    call object1%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Remove obj1"
    
    call remove_object (object1)

    write (u, "(A)")      
    write (u, "(A)")  "* Prototype status"
    
    write (u, "(A)")
    call prototype%write (u, refcount=.true.)

    call remove_object (prototype)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup:"

    write (u, "(A)")
    write (u, "(A,1x,L1)")  "tag allocated =", associated (tag)
    write (u, "(A,1x,L1)")  "prototype allocated =", associated (prototype)
    write (u, "(A,1x,L1)")  "obj1 allocated =", associated (object1)
    write (u, "(A,1x,L1)")  "obj2 allocated =", associated (object2)
    write (u, "(A,1x,L1)")  "obj3 allocated =", associated (object3)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_2"
    
  end subroutine object_base_2

  subroutine object_base_3 (u)
    use iso_varying_string, string_t => varying_string
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype, member
    class(object_t), pointer :: object1, object2, object3, foo, bar

    write (u, "(A)")  "* Test output: object_base_3"
    write (u, "(A)")  "*   Purpose: find objects by path"

    write (u, "(A)")      
    write (u, "(A)")  "* Create prototypes for tag and tag composite"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select

    write (u, "(A)")      
    write (u, "(A)")  "* Create nested composite"
    
    call prototype%instantiate (object1)
    select type (object1)
    class is (composite_t)
       call object1%tag_non_intrinsic ()
       call object1%init (name = var_str ("obj1"), n_members = 1)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("foo"))
       end select
       call object1%import_member (1, member)
    end select

    call object1%instantiate (object2)
    select type (object2)
    class is (composite_t)
       call object2%init (name = var_str ("obj2"))
    end select

    call prototype%instantiate (object3)
    select type (object3)
    class is (composite_t)
       call object3%init (name = var_str ("obj3"), n_members = 3)
       call object3%import_member (1, object1)
       call object3%import_member (2, object2)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("bar"))
       end select
       call object3%import_member (3, member)
    end select
    
    write (u, "(A)")
    call object3%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Return pointer to obj1"

    object1 => null ()
    select type (object3)
    class is (composite_t)
       call object3%find_member (var_str ("obj1"), object1)
    end select

    write (u, "(A)")
    call object1%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Return pointer to obj1.foo"

    foo => null ()
    call object3%find ([var_str ("obj1"), var_str ("foo")], foo)

    if (associated (foo)) then
       write (u, "(A)")
       call foo%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")      
    write (u, "(A)")  "* Return pointer to obj2.foo"

    foo => null ()
    call object3%find ([var_str ("obj2"), var_str ("foo")], foo)

    if (associated (foo)) then
       write (u, "(A)")
       call foo%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")      
    write (u, "(A)")  "* Starting at obj1, return pointer to obj2"

    object2 => null ()
    call object1%find ([var_str ("obj2")], object2)

    if (associated (object2)) then
       write (u, "(A)")
       call object2%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")      
    write (u, "(A)")  "* Starting at obj1, return pointer to obj2.foo"

    foo => null ()
    call object1%find ([var_str ("obj2"), var_str ("foo")], foo)

    if (associated (foo)) then
       write (u, "(A)")
       call foo%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")      
    write (u, "(A)")  "* Starting at obj1, return pointer to bar"

    bar => null ()
    call object1%find ([var_str ("bar")], bar)

    if (associated (bar)) then
       write (u, "(A)")
       call bar%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")      
    write (u, "(A)")  "* Starting at bar, return pointer to obj1.foo"

    foo => null ()
    call bar%find ([var_str ("obj1"), var_str ("foo")], foo)

    if (associated (foo)) then
       write (u, "(A)")
       call foo%write (u, refcount=.true.)
    end if
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call remove_object (object3)

    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_3"
    
  end subroutine object_base_3

  subroutine object_base_4 (u)
    use iso_varying_string, string_t => varying_string
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype
    class(object_t), pointer :: obj, ref

    write (u, "(A)")  "* Test output: object_base_4"
    write (u, "(A)")  "*   Purpose: create references and copies"

    write (u, "(A)")      
    write (u, "(A)")  "* Create prototype"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select

    write (u, "(A)")      
    call prototype%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create object"
    
    call prototype%instantiate (obj)
    select type (obj)
    class is (composite_t)
       call obj%init (name = var_str ("obj"))
    end select
    
    write (u, "(A)")      
    call prototype%write (u, refcount=.true.)
    call obj%write (u, refcount=.true.)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create reference"

    call obj%make_reference (ref)

    write (u, "(A)")      
    call prototype%write (u, refcount=.true.)
    call obj%write (u, refcount=.true.)
    call ref%write (u, refcount=.true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call remove_object (ref)
    call remove_object (obj)
    call remove_object (prototype)
    call remove_object (tag)

    write (u, "(A)")
    write (u, "(A,1x,L1)")  "tag allocated =", associated (tag)
    write (u, "(A,1x,L1)")  "prototype allocated =", associated (prototype)
    write (u, "(A,1x,L1)")  "obj allocated =", associated (obj)
    write (u, "(A,1x,L1)")  "ref allocated =", associated (ref)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_4"
    
  end subroutine object_base_4

  subroutine object_base_5 (u)
    use iso_varying_string, string_t => varying_string
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype, member
    class(object_t), pointer :: object1, object2, object3, ptr
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_base_5"
    write (u, "(A)")  "*   Purpose: use iterator"

    write (u, "(A)")      
    write (u, "(A)")  "* Create prototypes for tag and tag composite"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select

    write (u, "(A)")      
    write (u, "(A)")  "* Create nested composite"
    
    call prototype%instantiate (object1)
    select type (object1)
    class is (composite_t)
       call object1%tag_non_intrinsic ()
       call object1%init (name = var_str ("obj1"), n_members = 1)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("foo"))
       end select
       call object1%import_member (1, member)
    end select

    call object1%instantiate (object2)
    select type (object2)
    class is (composite_t)
       call object2%init (name = var_str ("obj2"))
    end select

    call prototype%instantiate (object3)
    select type (object3)
    class is (composite_t)
       call object3%init (name = var_str ("obj3"), n_members = 3)
       call object3%import_member (1, object1)
       call object3%import_member (2, object2)
       call prototype%instantiate (member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("bar"))
       end select
       call object3%import_member (3, member)
    end select
    
    write (u, "(A)")
    call object3%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Iterate through obj3"
    write (u, "(A)")      

    call it%init (object3)
    do while (it%is_valid ())
       call it%get_object (ptr)
       call ptr%write (u, mantle=.false.)
       call it%write (u)
       write (u, *)
       call it%advance ()
    end do

    write (u, "(A)")      
    write (u, "(A)")  "* Iterate through obj3, skipping obj2"
    write (u, "(A)")      

    call it%init (object3)
    do while (it%is_valid ())
       call it%get_object (ptr)
       if (ptr%get_name () == "obj2") then
          call it%skip ()
          cycle
       end if
       call ptr%write (u, mantle=.false.)
       call it%write (u)
       write (u, *)
       call it%advance ()
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call remove_object (object3)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_5"
    
  end subroutine object_base_5

  subroutine object_base_6 (u)
    use iso_varying_string, string_t => varying_string
    use codes
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype
    class(object_t), pointer :: object1, object2, object3, member
    type(repository_t), target :: repository
    type(code_t) :: code

    write (u, "(A)")  "* Test output: object_base_6"
    write (u, "(A)")  "*   Purpose: use prototype repository"

    write (u, "(A)")      
    write (u, "(A)")  "* Create repository with tag prototype"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select
    
    call repository%init (name = var_str ("repo"), n_members = 1)
    call repository%import_member (1, prototype)

    write (u, "(A)")
    call repository%write (u, refcount=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Create composite with member of type tag"
     
    call repository%spawn (var_str ("tag"), object1)
    select type (object1)
    class is (composite_t)
       call object1%tag_non_intrinsic ()
       call object1%init (name = var_str ("obj1"), n_members = 1)
       call repository%spawn (1, member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("foo"))
       end select
       call object1%import_member (1, member)
    end select
 
    write (u, "(A)")
    call object1%write (u, refcount=.true.)

    write (u, "(A)")      
    code = object1%get_code (repository)
    call code%write (u, verbose=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Repository state"
     
    write (u, "(A)")
    call repository%write (u, refcount=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Create extension of object1"
    
    call repository%include (object1)
    call repository%spawn (var_str ("obj1"), object2)
    select type (object2)
    class is (composite_t)
       call object2%init (name = var_str ("obj2"), n_members = 1)
       call repository%spawn (var_str ("tag"), member)
       select type (member)
       type is (composite_t);  call member%init (name = var_str ("bar"))
       end select
       call object2%import_member (1, member)
    end select
       
    write (u, "(A)")
    call object2%write (u, refcount=.true.)
    
    write (u, "(A)")      
    code = object2%get_code (repository)
    call code%write (u, verbose=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Repository state"
     
    write (u, "(A)")
    call repository%write (u, refcount=.true.)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"

    call remove_object (object2)
    call remove_object (object1)

    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_6"
    
  end subroutine object_base_6

  subroutine object_base_7 (u)
    use iso_varying_string, string_t => varying_string
    use io_units
    use codes
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype
    class(object_t), pointer :: main, object, core
    type(repository_t), target :: repository
    integer, parameter :: ncode = 4
    integer :: utmp, i
    type(code_t), dimension(ncode) :: code
    type(object_iterator_t) :: it

    write (u, "(A)")  "* Test output: object_base_7"
    write (u, "(A)")  "*   Purpose: object building using code and iterator"

    write (u, "(A)")      
    write (u, "(A)")  "* Create repository with tag prototype"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select
    
    call repository%init (name = var_str ("repo"), n_members = 1)
    call repository%import_member (1, prototype)

    write (u, "(A)")
    call repository%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create anonymous wrapper"
    
    allocate (wrapper_t :: main)

    write (u, "(A)")
    call main%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Code array for composite with member"
     
    utmp = free_unit ()
    open (utmp, status="scratch")
    ! Composite with 1 member, named 'obj1'
    write (utmp, "(A)")  "100 2 1 6 1 0 0 1 0 0"
    write (utmp, "(A)")  " obj1"
    ! Member: composite named 'foo'
    write (utmp, "(A)")  "100 2 1 6 1 0 0 0 0 0"
    write (utmp, "(A)")  " foo"
    ! Member core: tag
    write (utmp, "(A)")  "  1 0 0 0"
    ! Parent core: tag
    write (utmp, "(A)")  "  1 0 0 0"

    rewind (utmp)
    write (u, "(A)")
    do i = 1, ncode
       call code(i)%read (utmp)
       call code(i)%write (u, verbose=.true.)
    end do
    close (utmp)

    write (u, "(A)")      
    write (u, "(A)")  "* Build composite using code array"
     
    call it%init (main)
    do i = 1, ncode
       call build_object (object, code(i), repository)
       if (associated (object)) then
          call it%advance (import_object = object)
       else
          call it%advance ()
       end if
    end do

    write (u, "(A)")
    select type (main)
    class is (wrapper_t)
       call main%get_core_ptr (core)
       if (associated (core)) then
          call core%write (u)
       end if
    end select
    
    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"

    call remove_object (main)

    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_7"
    
  end subroutine object_base_7

  subroutine object_base_8 (u)
    use iso_varying_string, string_t => varying_string
    use object_base
    
    implicit none

    integer, intent(in) :: u
    class(object_t), pointer :: tag, prototype
    class(object_t), pointer :: main, foo, ref
    logical :: success

    write (u, "(A)")  "* Test output: object_base_8"
    write (u, "(A)")  "*   Purpose: resolve reference by ID"

    write (u, "(A)")      
    write (u, "(A)")  "* Create prototype"

    allocate (tag_t :: tag)
    
    allocate (composite_t :: prototype)
    select type (prototype)
    type is (composite_t)
       call prototype%init (name = var_str ("tag"))
       call prototype%import_core (tag)
    end select

    write (u, "(A)")      
    call prototype%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Create main"

    call prototype%instantiate (foo)
    select type (foo)
    class is (composite_t)
       call foo%init (name = var_str ("foo"))
    end select
    
    allocate (reference_t :: ref)
    select type (ref)
    class is (reference_t)
       call ref%set_path ([var_str ("foo")])
    end select

    allocate (composite_t :: main)
    select type (main)
    class is (composite_t)
       call main%init (name = var_str ("main"), n_members = 2)
       call main%import_member (1, foo)
       call main%import_member (2, ref)
    end select
    
    write (u, "(A)")      
    call main%write (u)
    
    write (u, "(A)")      
    write (u, "(A)")  "* Resolve reference"

    call main%resolve (success)

    write (u, "(A)")      
    write (u, "(A,1x,L1)")  "success =", success

    write (u, "(A)")      
    call main%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call remove_object (main)
    call remove_object (prototype)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: object_base_8"
    
  end subroutine object_base_8


end module object_base_uti
