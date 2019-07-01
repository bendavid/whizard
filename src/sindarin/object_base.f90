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

module object_base

  use iso_varying_string, string_t => varying_string

  use format_utils
  use io_units
  use diagnostics
  use codes

  implicit none
  private

  public :: object_t
  public :: remove_object
  public :: build_object
  public :: tag_t
  public :: value_t
  public :: id_t
  public :: wrapper_t
  public :: reference_t
  public :: ref_array_t
  public :: composite_t
  public :: composite_next_position
  public :: repository_t
  public :: object_iterator_t

  integer, parameter, public :: MODE_ABSTRACT = 0
  integer, parameter, public :: MODE_CONSTANT = 1
  integer, parameter, public :: MODE_VARIABLE = 2


  type, abstract :: object_t
     private
     class(wrapper_t), pointer :: context => null ()
     integer :: refcount = 1
   contains
     procedure (object_final), deferred :: final
     procedure :: write => object_write
     procedure :: write_as_declaration => object_write_as_declaration
     procedure :: write_as_statement => object_write_as_statement
     procedure :: write_as_expression => object_write_as_expression
     procedure :: write_as_value => object_write_as_value
     procedure :: write_core => object_write_stub
     procedure :: write_mantle => object_write_mantle
     procedure :: write_statement => object_write_statement
     procedure :: write_expression => object_write_stub
     procedure :: write_value => object_write_stub
     procedure, non_overridable, private :: get_refcount => object_get_refcount
     procedure, non_overridable, private :: set_context => object_set_context
     procedure (object_get_name), deferred :: get_name
     procedure :: get_prototype => object_get_prototype
     procedure (object_get_signature), deferred :: get_signature
     procedure :: get_priority => object_get_priority
     procedure :: is_reference => object_is_reference
     procedure :: is_statement => object_is_statement
     procedure :: is_expression => object_is_expression
     procedure :: has_id => object_has_id
     procedure :: get_id_ptr => object_get_id_ptr
     procedure :: has_value => object_has_value
     procedure :: is_literal => object_is_literal
     procedure :: has_literal => object_has_literal
     procedure :: has_mantle => object_has_mantle
     procedure :: get_n_members => object_get_n_members
     procedure :: get_n_arguments => object_get_n_arguments
     procedure :: get_n_primers => object_get_n_primers
     procedure :: get_member_ptr => object_get_member_ptr
     procedure :: get_primer_ptr => object_get_primer_ptr
     procedure :: is_defined => object_is_defined
     procedure (object_instantiate), deferred :: instantiate
     procedure :: resolve => object_resolve
     procedure :: evaluate => object_evaluate
     procedure (object_get_code), deferred :: get_code
     procedure :: next_position => object_next_position
     procedure :: make_reference => object_make_reference
     procedure :: dereference => object_dereference
     procedure :: find => object_find
     procedure :: push => object_push
     procedure :: match => object_match
  end type object_t
  
  type, extends (object_t) :: tag_t
     private
   contains
     procedure :: final => tag_final
     procedure :: get_name => tag_get_name
     procedure :: get_signature => tag_get_signature
     procedure :: instantiate => tag_instantiate
     procedure :: get_code => tag_get_code
  end type tag_t
  
  type, extends (object_t), abstract :: value_t
     private
     logical :: defined = .false.
   contains
     procedure :: get_signature => value_get_signature
     procedure :: is_literal => value_is_literal
     procedure :: is_defined => value_is_defined
     procedure(value_init_from_code), deferred :: init_from_code
     procedure :: set_defined => value_set_defined
     procedure :: match => value_match
     procedure :: assign => value_assign
     procedure (value_match_value), deferred :: match_value
     procedure (value_assign_value), deferred :: assign_value
  end type value_t
  
  type, extends (value_t) :: id_t
     private
     type(string_t), dimension(:), allocatable :: path
   contains
     procedure :: final => id_final
     procedure :: write_expression => id_write_expression
     procedure :: get_name => id_get_name
     procedure :: get_path => id_get_path
     procedure :: get_path_string => id_get_path_string
     procedure :: instantiate => id_instantiate
     procedure :: get_code => id_get_code
     procedure :: init_from_code => id_init_from_code
     procedure :: init => id_init_path
     procedure :: match_value => id_match_value
     procedure :: assign_value => id_assign_value
  end type id_t
  
  type, extends (object_t) :: wrapper_t
     private
     class(object_t), pointer :: core => null ()
   contains
     procedure :: final => wrapper_final
     procedure :: write_core => wrapper_write_core
     procedure :: get_name => wrapper_get_name
     procedure :: get_prototype => wrapper_get_prototype
     procedure :: get_signature => wrapper_get_signature
     procedure :: is_reference => wrapper_is_reference
     procedure :: is_statement => wrapper_is_statement
     procedure :: is_expression => wrapper_is_expression
     procedure :: is_defined => wrapper_is_defined
     procedure :: get_core_ptr => wrapper_get_core_ptr
     procedure :: instantiate => wrapper_instantiate
     procedure, non_overridable :: import_core => wrapper_import_core
     procedure :: get_code => wrapper_get_code
     procedure :: next_position => wrapper_next_position
     procedure :: find => wrapper_find
  end type wrapper_t
  
  type, extends (wrapper_t) :: reference_t
     private
     class(object_t), pointer :: id => null ()
   contains
     procedure :: final => reference_final
     procedure :: write_statement => reference_write_statement
     procedure :: write_expression => reference_write_expression
     procedure :: get_name => reference_get_name
     procedure :: get_signature => reference_get_signature
     procedure :: is_reference => reference_is_reference
     procedure :: is_associated => reference_is_associated
     procedure :: has_id => reference_has_id
     procedure :: get_id_ptr => reference_get_id_ptr
     procedure :: instantiate => reference_instantiate
     procedure :: set_path => reference_set_path
     procedure :: import_id => reference_import_id
     procedure :: link_core => reference_link_core
     procedure :: resolve => reference_resolve
     procedure :: get_code => reference_get_code
     procedure :: dereference => reference_dereference
  end type reference_t
  
  type, extends (object_t) :: ref_array_t
     private
     type(reference_t), dimension(:), pointer :: item => null ()
   contains
     procedure :: final => ref_array_final
     procedure :: write_expression => ref_array_write_expression
     procedure :: write_value => ref_array_write_value
     procedure :: get_name => ref_array_get_name
     procedure :: get_signature => ref_array_get_signature
     procedure :: is_expression => ref_array_is_expression
     procedure :: is_defined => ref_array_is_defined
     procedure :: get_item_ptr => ref_array_get_item_ptr
     procedure :: instantiate => ref_array_instantiate
     procedure :: init => ref_array_init
     procedure :: associate => ref_array_associate
     procedure :: get_code => ref_array_get_code
     procedure :: init_from_code => ref_array_init_from_code
  end type ref_array_t
  
  type, extends (wrapper_t) :: composite_t
     private
     type(string_t) :: name
     integer :: mode = MODE_ABSTRACT
     logical :: intrinsic = .true.
     class(composite_t), pointer :: prototype => null ()
     logical, dimension(:), allocatable :: member_is_argument
     type(wrapper_t), dimension(:), pointer :: member => null ()
     type(wrapper_t), dimension(:), pointer :: primer => null ()
   contains
     procedure :: final => composite_final
     procedure :: write_value => composite_write_value
     procedure :: write_mantle => composite_write_mantle
     procedure, non_overridable :: register => composite_register
     procedure, non_overridable, private :: unregister => composite_unregister
     procedure :: set_default_prototype => composite_set_default_prototype
     procedure :: get_prototype_index => composite_get_prototype_index
     procedure :: get_name => composite_get_name
     procedure :: get_prototype => composite_get_prototype
     procedure :: get_signature => composite_get_signature
     procedure :: has_value => composite_has_value
     procedure :: has_literal => composite_has_literal
     procedure :: is_defined => composite_is_defined
     procedure :: has_mantle => composite_has_mantle
     procedure :: get_n_members => composite_get_n_members
     procedure :: get_n_arguments => composite_get_n_arguments
     procedure :: get_n_primers => composite_get_n_primers
     procedure :: get_member_ptr => composite_get_member_ptr
     procedure :: get_primer_ptr => composite_get_primer_ptr
     procedure :: show_opname => composite_show_opname
     procedure :: get_opname => composite_get_opname
     procedure :: get_prototype_ptr => composite_get_prototype_ptr
     procedure :: check_mode => composite_check_mode
     procedure :: check_role => composite_check_role
     procedure :: instantiate => composite_instantiate
     procedure :: get_code => composite_get_code
     procedure :: get_base_code => composite_get_base_code
     procedure :: get_name_code => composite_get_name_code
     procedure :: init_from_code => composite_init_from_code
     generic :: init => composite_init
     procedure, private :: composite_init
     procedure :: set_mode => composite_set_mode
     procedure :: set_intrinsic => composite_set_intrinsic
     procedure :: init_members => composite_init_members
     procedure :: init_primers => composite_init_primers
     procedure :: tag_non_intrinsic => composite_tag_non_intrinsic
     procedure :: import_member => composite_import_member
     procedure :: link_member => composite_link_member
     procedure :: import_primer => composite_import_primer
     procedure :: resolve => composite_resolve
     procedure :: evaluate => composite_evaluate
     procedure :: next_position => composite_next_position
     procedure :: find => composite_find
     procedure :: find_member => composite_find_member
     procedure :: expand => composite_expand
  end type composite_t

  type, extends (composite_t) :: repository_t
     private
   contains
     procedure :: include => repository_include
     generic :: spawn => spawn_by_name, spawn_by_index
     procedure, private :: spawn_by_name
     procedure, private :: spawn_by_index
     procedure :: get_prototype_names => repository_get_prototype_names
  end type repository_t
  
  type, extends (position_t) :: position_entry_t
     private
     class(object_t), pointer :: object => null ()
     type(position_entry_t), pointer :: previous => null ()
  end type position_entry_t
  
  type :: object_iterator_t
     private
     type(position_entry_t), pointer :: current => null ()
   contains
     procedure :: final => object_iterator_final
     procedure :: write => object_iterator_write
     procedure, private :: push => object_iterator_push
     procedure, private :: pop => object_iterator_pop
     procedure :: init => object_iterator_init
     procedure :: is_valid => object_iterator_is_valid
     procedure :: get_object => object_iterator_get_object
     procedure :: get_next_position => object_iterator_get_next_position
     procedure :: advance => object_iterator_advance
     procedure :: skip => object_iterator_skip
     procedure :: to_context => object_iterator_to_context
     procedure :: to_id => object_iterator_to_id
     procedure :: to_core => object_iterator_to_core
     procedure :: to_member => object_iterator_to_member
     procedure :: to_primer => object_iterator_to_primer
  end type object_iterator_t
  

  abstract interface
     subroutine object_final (object)
       import
       class(object_t), intent(inout) :: object
     end subroutine object_final
  end interface
       
  abstract interface
     pure function object_get_name (object) result (name)
       import
       class(object_t), intent(in) :: object
       type(string_t) :: name
     end function object_get_name
  end interface
  
  abstract interface
     pure function object_get_signature (object, verbose) result (signature)
       import
       class(object_t), intent(in) :: object
       logical, intent(in), optional :: verbose
       type(string_t) :: signature
     end function object_get_signature
  end interface
  
  abstract interface
     subroutine object_instantiate (object, instance)
       import
       class(object_t), intent(inout), target :: object
       class(object_t), intent(out), pointer :: instance
     end subroutine object_instantiate
  end interface
       
  abstract interface
     function object_get_code (object, repository) result (code)
       import
       class(object_t), intent(in), target :: object
       type(repository_t), intent(in), optional :: repository
       type(code_t) :: code
     end function object_get_code
  end interface
  
  abstract interface
     subroutine value_init_from_code (object, code)
       import
       class(value_t), intent(out) :: object
       type(code_t), intent(in) :: code
     end subroutine value_init_from_code
  end interface

  abstract interface
     subroutine value_match_value (object, source, success)
       import
       class(value_t), intent(in) :: object
       class(value_t), intent(in) :: source
       logical, intent(out) :: success
     end subroutine value_match_value
  end interface
  
  abstract interface
     subroutine value_assign_value (object, source)
       import
       class(value_t), intent(inout) :: object
       class(value_t), intent(in) :: source
     end subroutine value_assign_value
  end interface
  

contains

  recursive subroutine remove_object (object)
    class(object_t), intent(inout), pointer :: object
    if (associated (object)) then
       object%refcount = object%refcount - 1
       if (object%refcount == 0) then
          call object%final ()
          deallocate (object)
       else
          object => null ()
       end if
    end if
  end subroutine remove_object
  
  recursive subroutine object_write &
       (object, unit, indent, refcount, core, mantle)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    logical, intent(in), optional :: refcount
    logical, intent(in), optional :: core
    logical, intent(in), optional :: mantle
    logical :: ref, cor, man
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    ref = .false.;  if (present (refcount))  ref = refcount
    cor = .true.;  if (present (core))  cor = core
    man = .true.;  if (present (mantle))  man = mantle
    write (u, "(A)", advance="no")  char (object%get_prototype ())
    write (u, "(1x,A)", advance="no")  char (object%get_name ())
    write (u, "(1x,'||',A,'||')", advance="no") &
         char (object%get_signature (verbose=.true.))
    if (ref) then
       write (u, "(1x,'(',I0,')')", advance="no")  object%get_refcount ()
    end if
    if (man .and. object%has_mantle ()) then
       write (u, "(1x,'{')")
       call object%write_mantle (u, ind+1, ref)
       call write_indent (u, ind)
       write (u, "('}')", advance="no")
    end if
    if (cor .and. object%has_value ()) then
       write (u, "(1x,'=',1x)", advance="no")
       call object%write_core (u, ind+1)
    end if
    write (u, *)
  end subroutine object_write
  
  recursive subroutine object_write_as_declaration (object, unit, indent)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    type(string_t) :: signature
    class(object_t), pointer :: member, primer
    integer :: u, ind, n_mem, n_pri, i
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    write (u, "(A)", advance="no")  char (object%get_prototype ())
    write (u, "(1x,A)", advance="no")  char (object%get_name ())
    signature = object%get_signature (verbose = .false.)
    if (signature /= "") then
       write (u, "(1x,'||',A,'||')", advance="no")  char (signature)
    end if
    n_mem = object%get_n_members ()
    n_pri = object%get_n_primers ()
    if (n_mem + n_pri > 0) then
       write (u, "(1x,'{')")
       do i = 1, n_mem
          call object%get_member_ptr (i, member)
          call write_indent (u, ind)
          call member%write_as_declaration (u, ind+1)
       end do
       do i = 1, n_pri
          call object%get_primer_ptr (i, primer)
          call write_indent (u, ind)
          call primer%write_as_statement (u, ind+1)
       end do
       call write_indent (u, ind)
       write (u, "('}')", advance="no")
    end if
    if (object%has_value ()) then
       write (u, "(1x,'=',1x)", advance="no")
       call object%write_core (u, ind+1)
    end if
    write (u, *)
  end subroutine object_write_as_declaration

  recursive subroutine object_write_as_statement (object, unit, indent)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    if (object%is_statement ()) then
       call object%write_statement (unit, indent)
    else
       call object%write_as_declaration (unit, indent)
    end if
  end subroutine object_write_as_statement
  
  recursive subroutine object_write_as_expression (object, unit, indent, &
       priority)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer, intent(in), optional :: priority
    class(object_t), pointer :: member
    logical :: paren
    integer :: u, i, ind, n_mem, n_arg
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    paren = .false.
    if (object%is_expression ()) then
       if (present (priority)) then
          paren = priority >= object%get_priority ()
       end if
       if (paren)  write (u, "('(')", advance="no")
       call object%write_expression (u, indent)
       if (paren)  write (u, "(')')", advance="no")
    else if (object%is_literal ()) then
       if (object%is_defined ()) then
          call object%write_expression (u, indent)
       else
          write (u, "('???')", advance="no")
       end if
    else if (object%has_value ()) then
       call object%write_core (u, ind)
    else
       write (u, "(A)", advance="no")  char (object%get_name ())
    end if
    n_mem = object%get_n_members ()
    n_arg = object%get_n_arguments ()
    if (n_mem - n_arg > 0) then
       write (u, "(1x,'{')")
       do i = n_arg + 1, n_mem
          call object%get_member_ptr (i, member)
          call write_indent (u, ind+1)
          call member%write_as_declaration (u, ind+1)
       end do
       call write_indent (u, ind)
       write (u, "('}')", advance="no")
    end if
  end subroutine object_write_as_expression
  
  recursive subroutine object_write_as_value (object, unit, indent)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: member
    integer :: u, i, ind, n_mem, n_arg
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    if (object%is_expression () &
         .or. object%is_literal () .or. object%has_value ()) then
       if (object%is_defined ()) then
          call object%write_value (u, indent)
       else
          write (u, "('???')", advance="no")
       end if
    else
       write (u, "(A)", advance="no")  char (object%get_name ())
    end if
    n_mem = object%get_n_members ()
    n_arg = object%get_n_arguments ()
    if (n_mem - n_arg > 0) then
       write (u, "(1x,'{')")
       do i = n_arg + 1, n_mem
          call object%get_member_ptr (i, member)
          call write_indent (u, ind+1)
          call member%write_as_declaration (u, ind+1)
       end do
       call write_indent (u, ind)
       write (u, "('}')", advance="no")
    end if
  end subroutine object_write_as_value
  
  subroutine object_write_stub (object, unit, indent)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit)
    write (u, "('<>')", advance="no")
  end subroutine object_write_stub
       
  subroutine object_write_mantle (object, unit, indent, refcount)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    logical, intent(in), optional :: refcount
  end subroutine object_write_mantle
  
  subroutine object_write_statement (object, unit, indent)
    class(object_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
  end subroutine object_write_statement
       
  function object_get_refcount (object) result (n)
    class(object_t), intent(in) :: object
    integer :: n
    n = object%refcount
  end function object_get_refcount
  
  subroutine object_set_context (object, context)
    class(object_t), intent(inout) :: object
    class(wrapper_t), intent(in), target :: context
    object%context => context
  end subroutine object_set_context
  
  function object_get_prototype (object) result (prototype)
    class(object_t), intent(in) :: object
    type(string_t) :: prototype
    prototype = "object"
  end function object_get_prototype
  
  pure function object_get_priority (object) result (priority)
    class(object_t), intent(in) :: object
    integer :: priority
    priority = 0
  end function object_get_priority
  
  pure function object_is_reference (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_is_reference
  
  pure function object_is_statement (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_is_statement
  
  pure function object_is_expression (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_is_expression
  
  pure function object_has_id (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_has_id
  
  subroutine object_get_id_ptr (object, id)
    class(object_t), intent(in) :: object
    class(object_t), pointer, intent(out) :: id
    id => null ()
  end subroutine object_get_id_ptr
  
  pure function object_has_value (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_has_value
  
  pure function object_is_literal (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_is_literal
  
  pure function object_has_literal (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_has_literal
  
  pure function object_has_mantle (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_has_mantle
  
  pure function object_get_n_members (object) result (n)
    class(object_t), intent(in) :: object
    integer :: n
    n = 0
  end function object_get_n_members
  
  pure function object_get_n_arguments (object) result (n)
    class(object_t), intent(in) :: object
    integer :: n
    n = 0
  end function object_get_n_arguments
  
  pure function object_get_n_primers (object) result (n)
    class(object_t), intent(in) :: object
    integer :: n
    n = 0
  end function object_get_n_primers
  
  subroutine object_get_member_ptr (object, i, member)
    class(object_t), intent(in) :: object
    integer, intent(in) :: i
    class(object_t), pointer, intent(out) :: member
    member => null ()
  end subroutine object_get_member_ptr
  
  subroutine object_get_primer_ptr (object, i, primer)
    class(object_t), intent(in) :: object
    integer, intent(in) :: i
    class(object_t), pointer, intent(out) :: primer
    primer => null ()
  end subroutine object_get_primer_ptr
  
  pure function object_is_defined (object) result (flag)
    class(object_t), intent(in) :: object
    logical :: flag
    flag = .false.
  end function object_is_defined
  
  subroutine object_resolve (object, success)
    class(object_t), intent(inout), target :: object
    logical, intent(out) :: success
    success = .true.
  end subroutine object_resolve
  
  subroutine object_evaluate (object)
    class(object_t), intent(inout), target :: object
  end subroutine object_evaluate
  
  subroutine build_object (object, code, repository)
    class(object_t), intent(out), pointer :: object
    type(code_t), intent(in) :: code
    type(repository_t), intent(in) :: repository
    integer :: prototype_index
    select case (code%get_cat ())
    case (CAT_COMPOSITE)
       prototype_index = code%get_att (1)
       if (prototype_index > 0) then
          call repository%spawn (prototype_index, object)
       else
          allocate (composite_t :: object)
       end if
       select type (object)
       class is (composite_t)
          call object%init_from_code (code)
       end select
    case (CAT_ID)
       allocate (id_t :: object)
       select type (object)
       type is (id_t)
          call object%init_from_code (code)
       end select
    case (CAT_REFERENCE)
       allocate (reference_t :: object)
    case (CAT_REF_ARRAY)
       allocate (ref_array_t :: object)
    case default
       object => null ()
    end select
  end subroutine build_object
  
  subroutine object_next_position (object, position, next_object, import_object)
    class(object_t), intent(inout), target :: object
    type(position_t), intent(inout) :: position
    class(object_t), intent(out), pointer, optional :: next_object
    class(object_t), intent(inout), pointer, optional :: import_object
    position%part = POS_NONE
    if (present (next_object))  next_object => null ()
  end subroutine object_next_position
  
  subroutine object_make_reference (object, reference)
    class(object_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: reference
    allocate (reference_t :: reference)
    select type (reference)
    type is (reference_t)
       call reference%link_core (object)
    end select
  end subroutine object_make_reference
  
  recursive function object_dereference (object) result (remote)
    class(object_t), intent(in), target :: object
    class(object_t), pointer :: remote
    remote => object
  end function object_dereference

  recursive subroutine object_find (object, path, member)
    class(object_t), intent(in) :: object
    type(string_t), dimension(:), intent(in) :: path
    class(object_t), intent(out), pointer :: member
    member => null ()
  end subroutine object_find
    
  subroutine object_push (object, lhs, rhs)
    class(object_t), intent(inout) :: object
    class(value_t), intent(in), pointer :: lhs, rhs
    call msg_bug ("Object: push method invalid in this context")
  end subroutine object_push
  
  recursive subroutine object_match (ref, source, success, master)
    class(object_t), intent(in), target :: ref
    class(object_t), intent(in), target :: source
    logical, intent(out) :: success
    class(object_t), intent(inout), optional :: master
    type(object_iterator_t) :: it_lhs, it_rhs
    class(object_t), pointer :: lhs, rhs, rhs_context
    class(value_t), pointer :: lval, rval
    type(position_t) :: position
    integer :: i
    logical :: mutable, required
    lhs => ref
    rhs => source
    call it_lhs%init (lhs)
    call it_rhs%init (rhs)
    ITERATE: do while (it_lhs%is_valid ())
       call it_lhs%get_next_position (position)
       select case (position%part)
       case (POS_NONE)
          call it_lhs%to_context (success);  if (.not. success)  exit ITERATE
          call it_rhs%to_context (success);  if (.not. success)  exit ITERATE
       case (POS_ID)
          call it_lhs%to_id (success);  if (.not. success)  exit ITERATE
          call it_rhs%to_id (success);  if (.not. success)  exit ITERATE
          call it_lhs%get_object (lhs)
          call it_rhs%get_object (rhs)
          select type (lhs)
             class is (id_t)
             call lhs%match (rhs, success);  if (.not. success)  exit ITERATE
             if (present (master)) then
                select type (rhs)
                class is (id_t)
                   lval => lhs
                   rval => rhs
                   call master%push (lval, rval)
                end select
             end if
          end select
       case (POS_CORE)
          call it_lhs%to_core (success);  if (.not. success)  exit ITERATE
          call it_rhs%to_core (success);  if (.not. success)  exit ITERATE
          call it_lhs%get_object (lhs)
          call it_rhs%get_object (rhs)
          select type (lhs)
             class is (value_t)
             call lhs%match (rhs, success);  if (.not. success)  exit ITERATE
             if (present (master)) then
                select type (rhs)
                class is (value_t)
                   lval => lhs
                   rval => rhs
                   call master%push (lval, rval)
                end select
             end if
          end select
       case (POS_MEMBER)
          call it_lhs%to_member &
               (position%i, success);  if (.not. success)  exit ITERATE
          call it_lhs%get_object (lhs)
          select type (lhs)
             class is (composite_t)
             call lhs%check_mode (mutable)
             call lhs%check_role (required)
          end select
          if (mutable .or. required) then
             call it_rhs%get_object (rhs_context)
             select type (rhs_context)
                class is (composite_t)
                call rhs_context%find_member (lhs%get_name (), index=i)
             end select
             if (i > 0) then
                call it_rhs%to_member &
                     (i, success);  if (.not. success)  exit ITERATE
             else if (required) then
                success = .false.;  exit ITERATE
             else
                call it_lhs%to_context &
                     (success);  if (.not. success)  exit ITERATE
             end if
          else
             call it_lhs%to_context &
                  (success);  if (.not. success)  exit ITERATE
          end if
       case default
          call it_lhs%to_context &
               (success);  if (.not. success)  exit ITERATE
       end select
    end do ITERATE
  end subroutine object_match
    
  subroutine tag_final (object)
    class(tag_t), intent(inout) :: object
  end subroutine tag_final
 
  pure function tag_get_name (object) result (name)
    class(tag_t), intent(in) :: object
    type(string_t) :: name
    name = "tag"
  end function tag_get_name
  
  pure function tag_get_signature (object, verbose) result (signature)
    class(tag_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       signature = "atom"
    else
       signature = ""
    end if
  end function tag_get_signature
  
  subroutine tag_instantiate (object, instance)
    class(tag_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (tag_t :: instance)
  end subroutine tag_instantiate
    
  function tag_get_code (object, repository) result (code)
    class(tag_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (CAT_TAG)
  end function tag_get_code
  
  pure function value_get_signature (object, verbose) result (signature)
    class(value_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       signature = "value"
    else
       signature = ""
    end if
  end function value_get_signature
  
  pure function value_is_literal (object) result (flag)
    class(value_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function value_is_literal
  
  pure function value_is_defined (object) result (flag)
    class(value_t), intent(in) :: object
    logical :: flag
    flag = object%defined
  end function value_is_defined
  
  pure subroutine value_set_defined (object, defined)
    class(value_t), intent(inout) :: object
    logical, intent(in) :: defined
    object%defined = defined
  end subroutine value_set_defined
  
  subroutine value_match (ref, source, success, master)
    class(value_t), intent(in), target :: ref
    class(object_t), intent(in), target :: source
    logical, intent(out) :: success
    class(object_t), intent(inout), optional :: master
    select type (source)
    class is (value_t)
       call ref%match_value (source, success)
    class default
       success = .false.
    end select
  end subroutine value_match
       
  subroutine value_assign (object, source)
    class(value_t), intent(inout) :: object
    class(value_t), intent(in) :: source
    call object%final ()
    object%defined = source%defined
    if (source%defined)  call object%assign_value (source)
  end subroutine value_assign
       
  subroutine id_final (object)
    class(id_t), intent(inout) :: object
    if (allocated (object%path))  deallocate (object%path)
    call object%set_defined (.false.)
  end subroutine id_final
  
  subroutine id_write_expression (object, unit, indent)
    class(id_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u
    u = given_output_unit (unit)
    if (allocated (object%path)) then
       write (u, "(A)", advance="no")  char (object%get_path_string ())
    else
       write (u, "(A)", advance="no")  "<id>"
    end if
  end subroutine id_write_expression
       
  pure function id_get_name (object) result (name)
    class(id_t), intent(in) :: object
    type(string_t) :: name
    name = "id"
  end function id_get_name

  pure function id_get_path (object) result (path)
    class(id_t), intent(in) :: object
    type(string_t), dimension(:), allocatable :: path
    if (allocated (object%path)) then
       allocate (path (size (object%path)))
       path = object%path
    end if
  end function id_get_path
  
  pure function id_get_path_string (object) result (path)
    class(id_t), intent(in) :: object
    type(string_t) :: path
    integer :: i
    path = ""
    if (allocated (object%path)) then
       do i = 1, size (object%path)
          if (i == 1) then
             path = object%path(i)
          else
             path = path // "." // object%path(i)
          end if
       end do
    end if
  end function id_get_path_string
  
  subroutine id_instantiate (object, instance)
    class(id_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (id_t :: instance)
  end subroutine id_instantiate

  function id_get_code (object, repository) result (code)
    class(id_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (CAT_ID)
    if (allocated (object%path)) then
       call code%create_string_val (object%path)
    end if
  end function id_get_code
  
  subroutine id_init_from_code (object, code)
    class(id_t), intent(out) :: object
    type(code_t), intent(in) :: code
    logical :: success
    call code%get_string_array (object%path, success)
    call object%set_defined (success)
  end subroutine id_init_from_code
    
  subroutine id_init_path (object, path)
    class(id_t), intent(out) :: object
    type(string_t), dimension(:), intent(in) :: path
    allocate (object%path (size (path)))
    object%path = path
    call object%set_defined (.true.)
  end subroutine id_init_path
    
  subroutine id_match_value (object, source, success)
    class(id_t), intent(in) :: object
    class(value_t), intent(in) :: source
    logical, intent(out) :: success
    select type (source)
    class is (id_t)
       success = .true.
    class default
       success = .false.
    end select
  end subroutine id_match_value
       
  subroutine id_assign_value (object, source)
    class(id_t), intent(inout) :: object
    class(value_t), intent(in) :: source
    select type (source)
    class is (id_t)
       call object%init (source%path)
    end select
  end subroutine id_assign_value
       
  recursive subroutine wrapper_final (object)
    class(wrapper_t), intent(inout) :: object
    if (associated (object%core)) then
       call object%core%final ()
       call remove_object (object%core)
    end if
  end subroutine wrapper_final
  
  recursive subroutine wrapper_write_core (object, unit, indent)
    class(wrapper_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    call object%core%write_as_expression (unit, indent)
  end subroutine wrapper_write_core
       
  pure recursive function wrapper_get_name (object) result (name)
    class(wrapper_t), intent(in) :: object
    type(string_t) :: name
    if (associated (object%core)) then
       name = object%core%get_name ()
    else
       name = "<???>"
    end if
  end function wrapper_get_name
  
  recursive function wrapper_get_prototype (object) result (prototype)
    class(wrapper_t), intent(in) :: object
    type(string_t) :: prototype
    if (associated (object%core)) then
       prototype = object%core%get_prototype ()
    else
       prototype = object_get_prototype (object)
    end if
  end function wrapper_get_prototype
  
  pure function wrapper_get_signature (object, verbose) result (signature)
    class(wrapper_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    signature = "wrapper"
  end function wrapper_get_signature
  
  pure recursive function wrapper_is_reference (object) result (flag)
    class(wrapper_t), intent(in) :: object
    logical :: flag
    if (associated (object%core)) then
       flag = object%core%is_reference ()
    else
       flag = .false.
    end if
  end function wrapper_is_reference
  
  pure recursive function wrapper_is_statement (object) result (flag)
    class(wrapper_t), intent(in) :: object
    logical :: flag
    if (associated (object%core)) then
       flag = object%core%is_statement ()
    else
       flag = .false.
    end if
  end function wrapper_is_statement
  
  pure recursive function wrapper_is_expression (object) result (flag)
    class(wrapper_t), intent(in) :: object
    logical :: flag
    if (associated (object%core)) then
       flag = object%core%is_expression ()
    else
       flag = .false.
    end if
  end function wrapper_is_expression
  
  pure recursive function wrapper_is_defined (object) result (flag)
    class(wrapper_t), intent(in) :: object
    logical :: flag
    if (associated (object%core)) then
       flag = object%core%is_defined ()
    else
       flag = .false.
    end if
  end function wrapper_is_defined
  
  subroutine wrapper_get_core_ptr (object, core)
    class(wrapper_t), intent(in) :: object
    class(object_t), intent(out), pointer :: core
    if (associated (object%core)) then
       core => object%core%dereference ()
    else
       core => null ()
    end if
  end subroutine wrapper_get_core_ptr
  
  recursive subroutine wrapper_instantiate (object, instance)
    class(wrapper_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (wrapper_t :: instance)
  end subroutine wrapper_instantiate
    
  subroutine wrapper_import_core (object, core)
    class(wrapper_t), intent(inout), target :: object
    class(object_t), intent(inout), pointer :: core
    if (associated (object%core))  call remove_object (object%core)
    call core%set_context (object)
    object%core => core
    core => null ()
  end subroutine wrapper_import_core
    
  function wrapper_get_code (object, repository) result (code)
    class(wrapper_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (0)
  end function wrapper_get_code
  
  subroutine wrapper_next_position &
       (object, position, next_object, import_object)
    class(wrapper_t), intent(inout), target :: object
    type(position_t), intent(inout) :: position
    class(object_t), intent(out), pointer, optional :: next_object
    class(object_t), intent(inout), pointer, optional :: import_object
    if (object%is_statement ()) then
       call object_next_position (object, position, next_object)
    else       
       select case (position%part)
       case (POS_CORE)
          call object_next_position (object, position, next_object)
       case default
          if (present (import_object)) then
             call object%import_core (import_object)
          end if
          if (associated (object%core)) then
             position%part = POS_CORE
             if (present (next_object))  next_object => object%core
          else
             call object_next_position (object, position, next_object)
          end if
       end select
    end if
  end subroutine wrapper_next_position
  
  recursive subroutine wrapper_find (object, path, member)
    class(wrapper_t), intent(in) :: object
    type(string_t), dimension(:), intent(in) :: path
    class(object_t), intent(out), pointer :: member
    class(object_t), pointer :: context
    member => null ()
    if (size (path) > 0) then
       if (associated (object%context)) then
          context => object%context
          call context%find (path, member)
       end if
    end if
  end subroutine wrapper_find
    
  recursive subroutine reference_final (object)
    class(reference_t), intent(inout) :: object
    if (associated (object%id)) then
       call object%id%final ()
       deallocate (object%id)
    end if
    if (associated (object%core)) then
       call remove_object (object%core)
    end if
  end subroutine reference_final
  
  recursive subroutine reference_write_statement (object, unit, indent)
    class(reference_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%id)) then
       if (object%id%is_defined ()) then
          call object%id%write_as_statement (unit, indent)
       else
          write (u, "('<REF>')", advance="no")
       end if
    else if (associated (object%core)) then
       call object%core%write_statement (unit, indent)
    end if
  end subroutine reference_write_statement
       
  recursive subroutine reference_write_expression (object, unit, indent)
    class(reference_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%id)) then
       if (object%id%is_defined ()) then
          call object%id%write_as_expression (unit, indent)
       else
          write (u, "('<REF>')", advance="no")
       end if
    else if (associated (object%core)) then
       call object%core%write_expression (unit, indent)
    end if
  end subroutine reference_write_expression
       
  pure recursive function reference_get_name (object) result (name)
    class(reference_t), intent(in) :: object
    type(string_t) :: name
    if (associated (object%id)) then
       select type (id => object%id)
       class is (id_t)
          name = id%get_path_string ()
       end select
    else if (associated (object%core)) then
       name = object%core%get_name ()
    else
       name = "<???>"
    end if
  end function reference_get_name
  
  pure recursive function reference_get_signature (object, verbose) &
       result (signature)
    class(reference_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    if (associated (object%core)) then
       signature = object%core%get_signature ()
       if (signature /= "") then
          signature = "reference|" // signature
       else
          signature = "reference"
       end if
    else
       signature = "reference"
    end if
  end function reference_get_signature
  
  pure function reference_is_reference (object) result (flag)
    class(reference_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function reference_is_reference
  
  elemental function reference_is_associated (object) result (flag)
    class(reference_t), intent(in) :: object
    logical :: flag
    flag = associated (object%core)
  end function reference_is_associated
  
  pure function reference_has_id (object) result (flag)
    class(reference_t), intent(in) :: object
    logical :: flag
    flag = associated (object%id)
  end function reference_has_id
  
  subroutine reference_get_id_ptr (object, id)
    class(reference_t), intent(in) :: object
    class(object_t), pointer, intent(out) :: id
    id => object%id
  end subroutine reference_get_id_ptr
  
  recursive subroutine reference_instantiate (object, instance)
    class(reference_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (reference_t :: instance)
  end subroutine reference_instantiate
    
  subroutine reference_set_path (object, lhs_path)
    class(reference_t), intent(inout) :: object
    type(string_t), dimension(:), intent(in) :: lhs_path
    allocate (id_t :: object%id)
    select type (id => object%id)
    type is (id_t)
       call id%init (lhs_path)
    end select
  end subroutine reference_set_path
    
  subroutine reference_import_id (object, id)
    class(reference_t), intent(inout) :: object
    class(object_t), intent(inout), pointer :: id
    if (associated (object%id)) then
       call object%id%final ()
       deallocate (object%id)
    end if
    object%id => id
    id => null ()
  end subroutine reference_import_id
  
  subroutine reference_link_core (object, remote)
    class(reference_t), intent(inout) :: object
    class(object_t), intent(inout), target :: remote
    class(object_t), pointer :: remote_target
    call object%final ()
    remote_target => remote%dereference ()
    object%core => remote_target
    remote_target%refcount = remote_target%refcount + 1
  end subroutine reference_link_core
    
  subroutine reference_resolve (object, success)
    class(reference_t), intent(inout), target :: object
    logical, intent(out) :: success
    class(object_t), pointer :: remote
    if (object%has_id ()) then
       select type (id => object%id)
       type is (id_t)
          call object%find (id%get_path (), remote)
       end select
    end if
    success = associated (remote)
    if (success) then
       call object%link_core (remote)
    end if
  end subroutine reference_resolve
    
  function reference_get_code (object, repository) result (code)
    class(reference_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call code%set (CAT_REFERENCE)
  end function reference_get_code
  
  recursive function reference_dereference (object) result (remote)
    class(reference_t), intent(in), target :: object
    class(object_t), pointer :: remote
    if (associated (object%core)) then
       remote => object%core%dereference ()
    else
       remote => object
    end if
  end function reference_dereference

  recursive subroutine ref_array_final (object)
    class(ref_array_t), intent(inout) :: object
    integer :: i
    if (associated (object%item)) then
       do i = 1, size (object%item)
          call object%item(i)%final ()
       end do
       deallocate (object%item)
    end if
  end subroutine ref_array_final
  
  subroutine ref_array_write_expression (object, unit, indent)
    class(ref_array_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: item
    integer :: u, i
    u = given_output_unit (unit)
    if (associated (object%item)) then
       write (u, "('(')", advance="no")
       do i = 1, size (object%item)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          if (object%item(i)%is_associated ()) then
             call object%item(i)%get_core_ptr (item)
             call item%write_as_expression (u, priority=0)
          else
             write (u, "('<>')", advance="no")
          end if
       end do
       write (u, "(')')", advance="no")
    else
       write (u, "('???')", advance="no")
    end if
  end subroutine ref_array_write_expression
       
  subroutine ref_array_write_value (object, unit, indent)
    class(ref_array_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    class(object_t), pointer :: item
    integer :: u, i
    u = given_output_unit (unit)
    if (associated (object%item)) then
       write (u, "('(')", advance="no")
       do i = 1, size (object%item)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          if (object%item(i)%is_associated ()) then
             call object%item(i)%get_core_ptr (item)
             call item%write_as_value (u)
          else
             write (u, "('<>')", advance="no")
          end if
       end do
       write (u, "(')')", advance="no")
    else
       write (u, "('???')", advance="no")
    end if
  end subroutine ref_array_write_value
       
  pure function ref_array_get_name (object) result (name)
    class(ref_array_t), intent(in) :: object
    type(string_t) :: name
    name = "<ref array>"
  end function ref_array_get_name
  
  pure function ref_array_get_signature (object, verbose) result (signature)
    class(ref_array_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    signature = ""
  end function ref_array_get_signature
  
  pure function ref_array_is_expression (object) result (flag)
    class(ref_array_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function ref_array_is_expression
  
  pure function ref_array_is_defined (object) result (flag)
    class(ref_array_t), intent(in) :: object
    logical :: flag
    if (associated (object%item)) then
       flag = all (object%item%is_associated ())
    else
       flag = .false.
    end if
  end function ref_array_is_defined
  
  subroutine ref_array_get_item_ptr (object, i, item)
    class(ref_array_t), intent(in) :: object
    integer, intent(in) :: i
    class(object_t), intent(out), pointer :: item
    if (associated (object%item)) then
       call object%item(i)%get_core_ptr (item)
    else
       item => null ()
    end if
  end subroutine ref_array_get_item_ptr
  
  subroutine ref_array_instantiate (object, instance)
    class(ref_array_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (ref_array_t :: instance)
  end subroutine ref_array_instantiate
    
  subroutine ref_array_init (object, n_item)
    class(ref_array_t), intent(inout) :: object
    integer, intent(in) :: n_item
    allocate (object%item (n_item))
  end subroutine ref_array_init
  
  subroutine ref_array_associate (object, i, remote)
    class(ref_array_t), intent(inout) :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), target :: remote
    call object%item(i)%link_core (remote)
  end subroutine ref_array_associate
  
  function ref_array_get_code (object, repository) result (code)
    class(ref_array_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    if (associated (object%item)) then
       call code%set (CAT_REF_ARRAY, [size (object%item)])
    else
       call code%set (CAT_REF_ARRAY)
    end if
  end function ref_array_get_code
  
  subroutine ref_array_init_from_code (object, code)
    class(ref_array_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    if (code%get_n_att () > 0) then
       call object%init (code%get_att (1))
    end if
  end subroutine ref_array_init_from_code
    
  recursive subroutine composite_final (object)
    class(composite_t), intent(inout) :: object
    integer :: i
    if (associated (object%primer)) then
       do i = 1, size (object%primer)
          if (associated (object%primer(i)%core)) then
             call remove_object (object%primer(i)%core)
          end if
       end do
       deallocate (object%primer)
    end if
    if (associated (object%member)) then
       do i = 1, size (object%member)
          if (associated (object%member(i)%core)) then
             call remove_object (object%member(i)%core)
          end if
       end do
       deallocate (object%member)
       deallocate (object%member_is_argument)
    end if
    call object%wrapper_t%final ()
    call object%unregister ()
  end subroutine composite_final
       
  recursive subroutine composite_write_value (object, unit, indent)
    class(composite_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    if (object%has_value ()) then
       call object%core%write_as_value (unit, indent)
    else
       call object_write_stub (object, unit, indent)
    end if
  end subroutine composite_write_value
    
  recursive subroutine composite_write_mantle (object, unit, indent, refcount)
    class(composite_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    logical, intent(in), optional :: refcount
    class(object_t), pointer :: member, primer
    integer :: u, i, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    if (associated (object%member)) then
       do i = 1, size (object%member)
          if (object%show_opname(i)) then
             call write_indent (u, ind)
             write (u, "('O')", advance="no")
             write (u, "(I0,':',1x)", advance="no") i
             write (u, "(A)")  char (object%get_opname (i))
          end if
          call write_indent (u, ind)
          if (object%member_is_argument (i)) then
             write (u, "('A')", advance="no")
          else
             write (u, "('M')", advance="no")
          end if
          write (u, "(I0,':',1x)", advance="no") i
          if (object%member(i)%is_reference ()) then
             write (u, "('*')", advance="no")
          end if
          call object%get_member_ptr (i, member)
          if (associated (member)) then
             call member%write (u, ind, refcount)
          else
             write (u, "('<>')")
          end if
       end do
    end if
    if (associated (object%primer)) then
       do i = 1, size (object%primer)
          call write_indent (u, ind)
          write (u, "('P',I0,':',1x)", advance="no") i
          call object%get_primer_ptr (i, primer)
          if (associated (primer)) then
             call primer%write_as_statement (u, ind)
          end if
          write (u, *)
       end do
    end if
  end subroutine composite_write_mantle
  
  subroutine composite_register (object, prototype)
    class(composite_t), intent(inout) :: object
    class(composite_t), intent(inout), target :: prototype
    object%prototype => prototype
    prototype%refcount = prototype%refcount + 1
    object%intrinsic = prototype%intrinsic
  end subroutine composite_register

  recursive subroutine composite_unregister (object)
    class(composite_t), intent(inout) :: object
    class(object_t), pointer :: prototype
    prototype => object%prototype 
    call remove_object (prototype)
    object%prototype => null ()
  end subroutine composite_unregister

  subroutine composite_set_default_prototype (object, prototype)
    class(composite_t), intent(inout) :: object
    class(composite_t), intent(inout), target :: prototype
    if (.not. associated (object%prototype)) then
       call object%register (prototype)
    end if
  end subroutine composite_set_default_prototype
    
  function composite_get_prototype_index (object, repository) result (i)
    class(composite_t), intent(in) :: object
    type(repository_t), intent(in) :: repository
    integer :: i
    if (associated (object%prototype)) then
       call repository%find_member (object%prototype%get_name (), index=i)
    else
       i = 0
    end if
  end function composite_get_prototype_index

  pure function composite_get_name (object) result (name)
    class(composite_t), intent(in) :: object
    type(string_t) :: name
    name = object%name
  end function composite_get_name
  
  recursive function composite_get_prototype (object) result (prototype)
    class(composite_t), intent(in) :: object
    type(string_t) :: prototype
    if (associated (object%prototype)) then
       if (object%prototype%intrinsic) then
          prototype = object%prototype%get_name ()
       else
          prototype = "type(" // object%prototype%get_name () // ")"
       end if
    else
       prototype = object_get_prototype (object)
    end if   
  end function composite_get_prototype
  
  pure function composite_get_signature (object, verbose) result (signature)
    class(composite_t), intent(in) :: object
    logical, intent(in), optional :: verbose
    type(string_t) :: signature
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    select case (object%mode)
    case (MODE_ABSTRACT)
       signature = "abstract"
    case (MODE_CONSTANT)
       signature = "constant"
    case (MODE_VARIABLE)
       if (verb) then
          signature = "variable"
       else
          signature = ""
       end if
    end select
  end function composite_get_signature
       
  pure function composite_has_value (object) result (flag)
    class(composite_t), intent(in) :: object
    logical :: flag
    if (object%is_statement ()) then
       flag = .false.
    else
       select case (object%mode)
       case (MODE_CONSTANT, MODE_VARIABLE)
          flag = associated (object%core)
       case default
          flag = .false.
       end select
    end if
  end function composite_has_value
  
  pure function composite_has_literal (object) result (flag)
    class(composite_t), intent(in) :: object
    logical :: flag
    if (object%has_value ()) then
       flag = object%core%is_literal ()
    else
       flag = .false.
    end if
  end function composite_has_literal
  
  pure function composite_is_defined (object) result (flag)
    class(composite_t), intent(in) :: object
    logical :: flag
    if (object%has_value ()) then
       flag = object%core%is_defined ()
    else
       flag = .false.
    end if
  end function composite_is_defined
  
  pure function composite_has_mantle (object) result (flag)
    class(composite_t), intent(in) :: object
    logical :: flag
    flag = object%get_n_members () + object%get_n_primers () > 0
  end function composite_has_mantle
  
  pure function composite_get_n_members (object) result (n)
    class(composite_t), intent(in) :: object
    integer :: n
    if (associated (object%member)) then
       n = size (object%member)
    else
       n = 0
    end if
  end function composite_get_n_members
  
  pure function composite_get_n_arguments (object) result (n)
    class(composite_t), intent(in) :: object
    integer :: n
    if (associated (object%member)) then
       n = count (object%member_is_argument)
    else
       n = 0
    end if
  end function composite_get_n_arguments
  
  pure function composite_get_n_primers (object) result (n)
    class(composite_t), intent(in) :: object
    integer :: n
    if (associated (object%primer)) then
       n = size (object%primer)
    else
       n = 0
    end if
  end function composite_get_n_primers
  
  subroutine composite_get_member_ptr (object, i, member)
    class(composite_t), intent(in) :: object
    integer, intent(in) :: i
    class(object_t), pointer, intent(out) :: member
    if (associated (object%member)) then
       if (associated (object%member(i)%core)) then
          member => object%member(i)%core%dereference ()
       else
          member => null ()
       end if
    else
       member => null ()
    end if
  end subroutine composite_get_member_ptr
  
  subroutine composite_get_primer_ptr (object, i, primer)
    class(composite_t), intent(in) :: object
    integer, intent(in) :: i
    class(object_t), pointer, intent(out) :: primer
    if (associated (object%primer)) then
       if (associated (object%primer(i)%core)) then
          primer => object%primer(i)%core%dereference ()
       else
          primer => null ()
       end if
    else
       primer => null ()
    end if
  end subroutine composite_get_primer_ptr
  
  pure function composite_show_opname (object, i) result (flag)
    class(composite_t), intent(in) :: object
    integer, intent(in), optional :: i
    logical :: flag
    flag = .false.
  end function composite_show_opname
    
  function composite_get_opname (object, i) result (name)
    class(composite_t), intent(in) :: object
    integer, intent(in), optional :: i
    type(string_t) :: name
    name = object%get_name ()
  end function composite_get_opname
    
  subroutine composite_get_prototype_ptr (object, prototype)
    class(composite_t), intent(in) :: object
    class(composite_t), intent(out), pointer :: prototype
    prototype => object%prototype
  end subroutine composite_get_prototype_ptr
  
  pure subroutine composite_check_mode (object, mutable)
    class(composite_t), intent(in) :: object
    logical, intent(out) :: mutable
    select case (object%mode)
    case (MODE_ABSTRACT);  mutable = .false.
    case default
       mutable = .true.
    end select
  end subroutine composite_check_mode
  
  pure subroutine composite_check_role (object, required)
    class(composite_t), intent(in) :: object
    logical, intent(out) :: required
    select case (object%mode)
    case (MODE_ABSTRACT)
       required = .false.
       if (associated (object%core)) then
          select type (core => object%core)
          type is (tag_t);  required = .true.
          end select
       end if
    case (MODE_CONSTANT, MODE_VARIABLE)
       required = .true.
    case default
       required = .false.
    end select
  end subroutine composite_check_role
  
  recursive subroutine composite_instantiate (object, instance)
    class(composite_t), intent(inout), target :: object
    class(object_t), intent(out), pointer :: instance
    allocate (composite_t :: instance)
    select type (instance)
    class is (composite_t)
       call instance%register (object)
       if (associated (object%core)) &
            call object%core%instantiate (instance%core)
    end select
  end subroutine composite_instantiate
  
  function composite_get_code (object, repository) result (code)
    class(composite_t), intent(in), target :: object
    type(repository_t), intent(in), optional :: repository
    type(code_t) :: code
    call object%get_base_code (code, repository)
    call object%get_name_code (code)
  end function composite_get_code
  
  subroutine composite_get_base_code (object, code, repository)
    class(composite_t), intent(in), target :: object
    type(code_t), intent(inout) :: code
    type(repository_t), intent(in), optional :: repository
    integer, dimension(6) :: att
    att = 0
    if (present (repository)) then
       att(1) = object%get_prototype_index (repository)
    end if
    att(2) = object%mode
    if (.not. object%intrinsic) then
       att(3) = 1
    end if
    att(4) = object%get_n_members ()
    att(5) = object%get_n_arguments ()
    att(6) = object%get_n_primers ()
    call code%set (CAT_COMPOSITE, att)
  end subroutine composite_get_base_code
  
  subroutine composite_get_name_code (object, code)
    class(composite_t), intent(in), target :: object
    type(code_t), intent(inout) :: code
    call code%create_string_val ([object%get_name ()])
  end subroutine composite_get_name_code
  
  subroutine composite_init_from_code (object, code)
    class(composite_t), intent(inout) :: object
    type(code_t), intent(in) :: code
    type(string_t) :: name
    logical :: success
    call code%get_string (name, success)
    if (.not. success)  name = ""
    call object%init ( &
         name = name, &
         mode = code%get_att (2), &
         n_members = code%get_att (4), &
         n_arguments = code%get_att (5), &
         n_primers = code%get_att (6))
    call object%set_intrinsic (code%get_att (3) == 0)
  end subroutine composite_init_from_code

  subroutine composite_init &
       (object, name, mode, n_members, n_arguments, n_primers)
    class(composite_t), intent(inout), target :: object
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: mode
    integer, intent(in), optional :: n_members
    integer, intent(in), optional :: n_arguments
    integer, intent(in), optional :: n_primers
    object%name = name
    if (present (mode))  call object%set_mode (mode)
    if (present (n_members)) then
       call object%init_members (n_members, n_arguments)
    end if
    if (present (n_primers)) then
       call object%init_primers (n_primers)
    end if
  end subroutine composite_init
 
  subroutine composite_set_mode (object, mode)
    class(composite_t), intent(inout) :: object
    integer, intent(in) :: mode
    object%mode = mode
  end subroutine composite_set_mode

  subroutine composite_set_intrinsic (object, intrinsic)
    class(composite_t), intent(inout) :: object
    logical, intent(in) :: intrinsic
    object%intrinsic = intrinsic
  end subroutine composite_set_intrinsic

  subroutine composite_init_members (object, n_members, n_arguments)
    class(composite_t), intent(inout), target :: object
    integer, intent(in) :: n_members
    integer, intent(in), optional :: n_arguments
    integer :: i
    allocate (object%member (n_members))
    do i = 1, n_members
       call object%member(i)%set_context (object)
    end do
    allocate (object%member_is_argument (n_members), source = .false.)
    if (present (n_arguments)) then
       object%member_is_argument(1:n_arguments) = .true.
    end if
  end subroutine composite_init_members
 
  subroutine composite_init_primers (object, n_primers)
    class(composite_t), intent(inout), target :: object
    integer, intent(in) :: n_primers
    integer :: i
    allocate (object%primer (n_primers))
    do i = 1, n_primers
       call object%primer(i)%set_context (object)
    end do
  end subroutine composite_init_primers
 
  pure subroutine composite_tag_non_intrinsic (object)
    class(composite_t), intent(inout) :: object
    object%intrinsic = .false.
  end subroutine composite_tag_non_intrinsic
  
  subroutine composite_import_member (object, i, member)
    class(composite_t), intent(inout), target :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), pointer :: member
    call member%set_context (object%member(i))
    call object%member(i)%final ()
    call object%member(i)%import_core (member)
  end subroutine composite_import_member
    
  subroutine composite_link_member (object, i, member)
    class(composite_t), intent(inout), target :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), pointer :: member
    class(object_t), pointer :: ref
    call member%make_reference (ref)
    call object%import_member (i, ref)
  end subroutine composite_link_member
    
  subroutine composite_import_primer (object, i, primer)
    class(composite_t), intent(inout), target :: object
    integer, intent(in) :: i
    class(object_t), intent(inout), pointer :: primer
    call primer%set_context (object%primer(i))
    call object%primer(i)%final ()
    call object%primer(i)%import_core (primer)
  end subroutine composite_import_primer
    
  recursive subroutine composite_resolve (object, success)
    class(composite_t), intent(inout), target :: object
    logical, intent(out) :: success
    integer :: i
    success = .true.
    if (associated (object%member)) then
       do i = 1, size (object%member)
          call object%member(i)%core%resolve (success)
          if (.not. success)  return
       end do
    end if
    if (associated (object%primer)) then
       do i = 1, size (object%primer)
          call object%primer(i)%core%resolve (success)
          if (.not. success)  return
       end do
    end if
  end subroutine composite_resolve
  
  recursive subroutine composite_evaluate (object)
    class(composite_t), intent(inout), target :: object
    integer :: i
    if (associated (object%member)) then
       do i = 1, size (object%member)
          if (object%member_is_argument(i)) then
             call object%member(i)%core%evaluate ()
          end if
       end do
    end if
    if (associated (object%primer)) then
       do i = 1, size (object%primer)
          call object%primer(i)%core%evaluate ()
       end do
    end if
  end subroutine composite_evaluate
  
  subroutine composite_next_position &
       (object, position, next_object, import_object)
    class(composite_t), intent(inout), target :: object
    type(position_t), intent(inout) :: position
    class(object_t), intent(out), pointer, optional :: next_object
    class(object_t), intent(inout), pointer, optional :: import_object
    select case (position%part)
    case (POS_HERE, POS_ID)
       if (object%get_n_members () > 0) then
          position%part = POS_MEMBER
          position%i = 1
          if (present (import_object)) then
             call object%import_member (position%i, import_object)
          end if
          if (present (next_object)) &
               next_object => object%member(position%i)%core
       else if (object%get_n_primers () > 0) then
          position%part = POS_PRIMER
          position%i = 1
          if (present (import_object)) then
             call object%import_primer (position%i, import_object)
          end if
          if (present (next_object)) &
               next_object => object%primer(position%i)%core
       else
          call wrapper_next_position &
               (object, position, next_object, import_object)
       end if
    case (POS_MEMBER)
       if (position%i < size (object%member)) then
          position%i = position%i + 1
          if (present (import_object)) then
             call object%import_member (position%i, import_object)
          end if
          if (present (next_object)) &
               next_object => object%member(position%i)%core
       else if (object%get_n_primers () > 0) then
          position%part = POS_PRIMER
          position%i = 1
          if (present (import_object)) then
             call object%import_primer (position%i, import_object)
          end if
          if (present (next_object)) &
               next_object => object%primer(position%i)%core
       else
          call wrapper_next_position &
               (object, position, next_object, import_object)
       end if
    case (POS_PRIMER)
       if (position%i < size (object%primer)) then
          position%i = position%i + 1
          if (present (import_object)) then
             call object%import_primer (position%i, import_object)
          end if
          if (present (next_object)) &
               next_object => object%primer(position%i)%core
       else
          call wrapper_next_position &
               (object, position, next_object, import_object)
       end if
    case default
       call wrapper_next_position &
            (object, position, next_object, import_object)
    end select
  end subroutine composite_next_position
  
  recursive subroutine composite_find (object, path, member)
    class(composite_t), intent(in) :: object
    type(string_t), dimension(:), intent(in) :: path
    class(object_t), intent(out), pointer :: member
    class(object_t), pointer :: parent
    member => null ()
    if (size (path) > 0) then
       call object%find_member (path(1), member)
       if (associated (member)) then
          if (size (path) > 1) then
             parent => member
             call parent%find (path(2:), member)
          end if
       else
          if (associated (object%prototype)) then
             call object%prototype%find (path, member)
          end if
          if (.not. associated (member)) then
             call wrapper_find (object, path, member)
          end if
       end if
    end if
  end subroutine composite_find
    
  subroutine composite_find_member (object, name, member, index)
    class(composite_t), intent(in) :: object
    type(string_t), intent(in) :: name
    class(object_t), intent(out), optional, pointer :: member
    integer, intent(out), optional :: index
    class(object_t), pointer :: core
    integer :: i
    if (associated (object%member)) then
       do i = 1, size (object%member)
          call object%member(i)%get_core_ptr (core)
          if (core%get_name () == name) then
             if (present (member))  member => core%dereference ()
             if (present (index))  index = i
             return
          end if
       end do
    end if
    if (present (member))  member => null ()
    if (present (index))  index = 0
  end subroutine composite_find_member
    
  subroutine composite_expand (object, n_extra, is_argument)
    class(composite_t), intent(inout) :: object
    integer, intent(in) :: n_extra
    logical, intent(in) :: is_argument
    type(wrapper_t), dimension(:), pointer :: member
    class(object_t), pointer :: core
    integer :: i
    member => object%member
    allocate (object%member (object%get_n_members () + n_extra))
    if (associated (member)) then
       do i = 1, size (member)
          call member(i)%get_core_ptr (core)
          if (associated (core))  call object%import_member (i, core)
       end do
    end if
    object%member_is_argument = &
         [object%member_is_argument, spread (is_argument, 1, n_extra)]
  end subroutine composite_expand
    
  subroutine repository_include (repository, object)
    class(repository_t), intent(inout), target :: repository
    class(object_t), intent(inout), target :: object
    type(wrapper_t), dimension(:), pointer :: new_member
    class(object_t), pointer :: ref
    integer :: n
    n = size (repository%member) 
    allocate (new_member (n + 1))
    new_member(1:n) = repository%member
    deallocate (repository%member)
    call object%make_reference (ref)
    call new_member(n+1)%import_core (ref)
    call new_member(n+1)%set_context (repository)
    repository%member => new_member
    deallocate (repository%member_is_argument)
    allocate (repository%member_is_argument (n+1), source=.false.)
  end subroutine repository_include
    
  subroutine spawn_by_name (repository, name, object)
    class(repository_t), intent(in) :: repository
    type(string_t), intent(in) :: name
    class(object_t), intent(out), pointer :: object
    class(object_t), pointer :: prototype
    call repository%find_member (name, prototype)
    if (associated (prototype)) then
       call prototype%instantiate (object)
    else
       object => null ()
    end if
  end subroutine spawn_by_name
    
  subroutine spawn_by_index (repository, i, object)
    class(repository_t), intent(in) :: repository
    integer, intent(in) :: i
    class(object_t), intent(out), pointer :: object
    class(object_t), pointer :: prototype
    call repository%get_member_ptr (i, prototype)
    if (associated (prototype)) then
       call prototype%instantiate (object)
    else
       object => null ()
    end if
  end subroutine spawn_by_index
    
  subroutine repository_get_prototype_names (repository, name)
    class(repository_t), intent(in) :: repository
    type(string_t), dimension(:), allocatable, intent(out) :: name
    class(object_t), pointer :: prototype
    integer :: i
    allocate (name (repository%get_n_members ()))
    do i = 1, size (name)
       call repository%get_member_ptr (i, prototype)
       name(i) = prototype%get_name ()
    end do
  end subroutine repository_get_prototype_names
    
  subroutine object_iterator_final (it)
    class(object_iterator_t), intent(inout) :: it
    do while (associated (it%current))
       call it%pop ()
    end do
  end subroutine object_iterator_final
    
  subroutine object_iterator_write (it, unit)
    class(object_iterator_t), intent(in) :: it
    integer, intent(in), optional :: unit
    type(position_entry_t), pointer :: entry
    integer :: u
    u = given_output_unit (unit)
    entry => it%current
    do while (associated (entry))
       call entry%write (u)
       entry => entry%previous
    end do
  end subroutine object_iterator_write
  
  subroutine object_iterator_push (it)
    class(object_iterator_t), intent(inout) :: it
    type(position_entry_t), pointer :: entry
    allocate (entry)
    entry%previous => it%current
    it%current => entry
  end subroutine object_iterator_push
    
  subroutine object_iterator_pop (it)
    class(object_iterator_t), intent(inout) :: it
    type(position_entry_t), pointer :: entry
    entry => it%current
    if (associated (entry)) then
       it%current => entry%previous
       deallocate (entry)
    else
       it%current => null ()
    end if
  end subroutine object_iterator_pop
  
  subroutine object_iterator_init (it, object)
    class(object_iterator_t), intent(out) :: it
    class(object_t), intent(in), target :: object
    call it%push ()
    it%current%object => object
  end subroutine object_iterator_init
    
  function object_iterator_is_valid (it) result (flag)
    class(object_iterator_t), intent(in) :: it
    logical :: flag
    flag = associated (it%current)
    if (flag)  flag = associated (it%current%object)
  end function object_iterator_is_valid
  
  subroutine object_iterator_get_object (it, object)
    class(object_iterator_t), intent(in) :: it
    class(object_t), pointer, intent(out) :: object
    if (associated (it%current)) then
       object => it%current%object
    else
       object => null ()
    end if
  end subroutine object_iterator_get_object
  
  subroutine object_iterator_get_next_position (it, position)
    class(object_iterator_t), intent(inout) :: it
    type(position_t), intent(out) :: position
    if (associated (it%current)) then
       position = it%current%position_t
       call it%current%object%next_position (position)
    end if
  end subroutine object_iterator_get_next_position
    
  recursive subroutine object_iterator_advance (it, import_object)
    class(object_iterator_t), intent(inout) :: it
    class(object_t), pointer, intent(inout), optional :: import_object
    class(position_t), pointer :: position
    class(object_t), pointer :: next_object
    position => it%current
    call it%current%object%next_position (position, next_object, import_object)
    select case (position%part)
    case (POS_NONE)
       call it%skip (import_object)
    case default
       call it%push ()
       it%current%object => next_object
    end select
  end subroutine object_iterator_advance
       
  recursive subroutine object_iterator_skip (it, import_object)
    class(object_iterator_t), intent(inout) :: it
    class(object_t), pointer, intent(inout), optional :: import_object
    call it%pop ()
    if (it%is_valid ()) then
       call it%advance (import_object)
    else
       call it%final ()
    end if
  end subroutine object_iterator_skip
       
  subroutine object_iterator_to_context (it, success)
    class(object_iterator_t), intent(inout) :: it
    logical, intent(out) :: success
    if (it%is_valid ()) then
       call it%pop ()
       success = .true.
    else
       success = .false.
    end if
  end subroutine object_iterator_to_context

  subroutine object_iterator_to_id (it, success)
    class(object_iterator_t), intent(inout) :: it
    logical, intent(out) :: success
    class(object_t), pointer :: object
    success = .false.
    object => it%current%object
    if (object%has_id ()) then
       it%current%part = POS_ID
       call it%push ()
       call object%get_id_ptr (it%current%object)
       success = .true.
    end if
  end subroutine object_iterator_to_id

  subroutine object_iterator_to_core (it, success)
    class(object_iterator_t), intent(inout) :: it
    logical, intent(out) :: success
    success = .false.
    select type (object => it%current%object)
    class is (wrapper_t)
       if (associated (object%core)) then
          it%current%part = POS_CORE
          call it%push ()
          it%current%object => object%core
          success = .true.
       end if
    end select
  end subroutine object_iterator_to_core

  subroutine object_iterator_to_member (it, i, success)
    class(object_iterator_t), intent(inout) :: it
    integer, intent(in) :: i
    logical, intent(out) :: success
    success = .false.
    select type (object => it%current%object)
    class is (composite_t)
       if (associated (object%member)) then
          if (0 < i .and. i <= size (object%member)) then
             if (associated (object%member(i)%core)) then
                it%current%part = POS_MEMBER
                it%current%i = i
                call it%push ()
                it%current%object => object%member(i)%core
                success = .true.
             end if
          end if
       end if
    end select
  end subroutine object_iterator_to_member

  subroutine object_iterator_to_primer (it, i, success)
    class(object_iterator_t), intent(inout) :: it
    integer, intent(in) :: i
    logical, intent(out) :: success
    success = .false.
    select type (object => it%current%object)
    class is (composite_t)
       if (associated (object%primer)) then
          if (0 < i .and. i <= size (object%primer)) then
             if (associated (object%primer(i)%core)) then
                it%current%part = POS_PRIMER
                it%current%i = i
                call it%push ()
                it%current%object => object%primer(i)%core
                success = .true.
             end if
          end if
       end if
    end select
  end subroutine object_iterator_to_primer


end module object_base
