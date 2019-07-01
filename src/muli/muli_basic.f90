!!! module: muli_basic_types
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: 2011-07-04 17:42:42 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_basic" which is included by all other
!!! modules. It's main purpose is serialization and deserialization, but it also
!!! contains some bitmodel parameters and some minor definitions common to all 
!!! modules and types.

!!! Serialization is implemented in three layers:
!!! 1) i/o layer: stream file access and exact, retreavable and compiler
!!!    independent representation of all intrinsic types
!!! 2) marking layer: storing/restoring pointer association and better human
!!!    readability
!!! 3) derived type layer: abstract type as parent for all serializable derived
!!!    types

!!! i/o layer:
!!! The i/o layer is implemented by "page_ring_type" and its type bound 
!!! procedures. I chose not to use the standard formatted sequential i/o for
!!! several reasons:
!!! *) Sequential i/o is line orientated, but serialization is not.
!!! *) Formatted i/o of floating point numbers is inexact. I have had problems
!!! in reading numbers like 1D0+epsilon(1D0) or huge(1D0) including arithmetic
!!! exeptions and wrong results.
!!! *) Formatted reading is slow. This does matter, when you read files of
!!! ~ 100 MB
!!!
!!! page_ring_type is a ring buffer of default kind characters holding a region
!!! of the addressed file. All read and write procedures use the ring buffer,
!!! not the file. page_ring automatically reads more characters from the file
!!! when needed and writes and discards parts of the ring buffer when not any
!!! longer needed.

!!! marking layer:
!!! Marking is done by marking_type and it's type bound procedures. Files 
!!! written by marking_type are regular XML files, so you can use your favorite
!!! XML editor to examine or edit serialized contents. The main purpose of this
!!! layer is to recover the type of polymorphic entities and to restore
!!! association of pointers, but it also assigns names to all contents, so it's
!!! much easier to read generated files.
!!!
!!! Type recovery is done by a reference list. You must push a representive 
!!! object to this reference list for each type that might get resolved before 
!!! starting deserialization. marker_type will care for comparing these
!!! representives to the contents of the files.
!!!
!!! Association restoration is done by a heap list. It is technically equal
!!! to the reference list, but holds all targets that have been processed so
!!! far. Generation of this list is handled by marking_type, so you dont't have
!!! to care for this aspect at all.
!!! Up to the present it is not possible to restore association, when a
!!! non-pointer target is serialized after it's associated pointer is serialized.
!!! There is no trivial solution and this case doesn't appear in my project, so
!!! I will not take care of this.

!!! derived type layer:
!!! Each instance that shall become serialized must extend serializable_class.
!!! Essential for type recovery is the virtual type bound procedure get_type.
!!! Each non-virtual type shall override get_type and return the actual name of
!!! it's type in lower-case letters.
!!! Each type which adds new, non-redundant components shall override
!!! write_to_marker and read_from_marker. These type bound procedures define,
!!! what contents get serialized. While the marker cares about tagging the type
!!! and association of the instance, every instance still has to define what to
!!! serialize. The rule is to mark the begin of it's contents, then it's
!!! parents procedure, then mark all non-redundant components, then mark the end
!!! of it's contents.
!!! Finally, each serializable type shall override print_to_unit. This procedure
!!! is called for an arbitrary human-readable output. It is quite similar to 
!!! write_to_ring, but without strict format and ignoring machine-readability.
!!!
!!! serializable_class has strictly speaking two layers. write_to_marker and
!!! read_from_marker are only for internal usage. Serialization and
!!! deserialization are triggered by the TBPs serialize and deserialize. These
!!! procedures take care of initialization and finalization of the marker. A
!!! serializable type should override these procedures to push a representive
!!! of itself and any other references to the reference list of it's marker
!!! before (de)serialization and to pop them from the list afterwards.
 

module muli_basic
  use,intrinsic::iso_fortran_env
  use kinds !NODEP!
  use iso_varying_string, string_t=>varying_string !NODEP!
  implicit none
  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Intrinsic Type Module Component Declaration !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! bitmodel parameters
  integer,public,parameter::drk=double
  integer,public,parameter::dik=i64
  integer(kind=dik),public,parameter::one=int(1,kind=dik)
  integer(kind=dik),public,parameter::zero=int(0,kind=dik)
  ! serialization parameters
  integer(kind=dik),public,parameter::serialize_page_size=1024
  integer(kind=dik),public,parameter::serialize_ok=0000
  integer(kind=dik),public,parameter::serialize_syntax_error=1001
  integer(kind=dik),public,parameter::serialize_wrong_tag=1002
  integer(kind=dik),public,parameter::serialize_wrong_id=1003
  integer(kind=dik),public,parameter::serialize_wrong_type=1004
  integer(kind=dik),public,parameter::serialize_wrong_name=1005
  integer(kind=dik),public,parameter::serialize_no_target=1006
  integer(kind=dik),public,parameter::serialize_no_pointer=1007
  integer(kind=dik),public,parameter::serialize_wrong_action=1008
  integer(kind=dik),public,parameter::serialize_unexpected_content=1009
  integer(kind=dik),public,parameter::serialize_null=1010
  integer(kind=dik),public,parameter::serialize_nothing=1011
  logical,public,parameter::serialize_default_indent=.true.
  logical,public,parameter::serialize_default_line_break=.true.
  logical,public,parameter::serialize_default_asynchronous=.false.
  ! private components
  integer(kind=dik),private::last_id=0
  character(len=*),private,parameter::serialize_integer_characters="-0123456789"

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type,public,abstract::serializable_class
   contains
     procedure(ser_write_if),deferred::write_to_marker
     procedure(ser_read_if),deferred::read_from_marker
     procedure(ser_unit),deferred::print_to_unit
     procedure(ser_type),nopass,deferred::get_type
     procedure,nopass::verify_type=>serializable_verify_type
     procedure::read_target_from_marker=>serializable_read_target_from_marker
     procedure::write_type=>serializable_write_type
     procedure::print=>serializable_print
     procedure::print_error=>serializable_print_error
     procedure::print_all=>serializable_print_all
     procedure::print_little=>serializable_print_little
     procedure::print_parents=>serializable_print_parents
     procedure::print_components=>serializable_print_components
     procedure::print_peers=>serializable_print_peers
     procedure::serialize_to_file=>serializable_serialize_to_file
     procedure::serialize_to_unit=>serializable_serialize_to_unit
     procedure::serialize_to_marker=>serializable_serialize_to_marker
     procedure::deserialize_from_file=>serializable_deserialize_from_file
     procedure::deserialize_from_unit=>serializable_deserialize_from_unit
     procedure::deserialize_from_marker=>serializable_deserialize_from_marker
     generic::serialize=>serialize_to_file,serialize_to_unit,serialize_to_marker
     generic::deserialize=>deserialize_from_file,deserialize_from_unit,deserialize_from_marker
  end type serializable_class

  type,public,abstract,extends(serializable_class)::measurable_class
   contains
     procedure(measure_int),public,deferred::measure
  end type measurable_class

  type,public,extends(serializable_class)::identified_type 
     private
     integer(kind=dik)::id
     type(string_t)::name
   contains
     ! overridden serializable_class procedures
     procedure,public::write_to_marker=>identified_write_to_marker
     procedure,public::read_from_marker=>identified_read_from_marker
     procedure,public::print_to_unit=>identified_print_to_unit
     procedure,public,nopass::get_type=>identified_get_type
     procedure,nopass::verify_type=>identified_verify_type
     ! new procedures
     procedure,public::identified_initialize
     procedure,public::get_id=>identified_get_id
     procedure,public::get_name=>identified_get_name
     generic,public::initialize=>identified_initialize
  end type identified_type
  
  type,public,extends(identified_type)::unique_type
     private
     integer(kind=dik)::unique_id
   contains
     ! overridden serializable_class procedures
     procedure,public,nopass::get_type=>unique_get_type
     procedure,nopass::verify_type=>unique_verify_type
     procedure,public::write_to_marker=>unique_write_to_marker
     procedure,public::read_from_marker=>unique_read_from_marker
     procedure,public::print_to_unit=>unique_print_to_unit
     ! overridden identified_type procedures
     procedure,public::identified_initialize=>unique_initialize
     ! new procedures
     procedure,public::get_unique_id=>unique_get_unique_id
  end type unique_type

  type,private::serializable_ref_type
     private
     integer(kind=dik)::id
     class(serializable_class),pointer::ref=>null()
     class(serializable_ref_type),pointer::next=>null()
   contains
     procedure,public::finalize=>serializable_ref_finalize
  end type serializable_ref_type

  type::position_stack_type
     private
     integer(kind=dik),dimension(2)::position
     class(position_stack_type),pointer::next=>null()
   contains
     procedure,public::push_head=>position_stack_push_head
     procedure,public::push_given=>position_stack_push_given
     procedure,public::position_stack_pop
     procedure,public::position_stack_drop
     procedure,public::nth_position=>position_stack_nth_position
     procedure,public::first=>position_stack_first
     procedure,public::last=>position_stack_last
     procedure,public::range=>position_stack_range
     generic,public::push=>push_head,push_given
     generic,public::pop=>position_stack_pop,position_stack_drop
  end type position_stack_type

  type,public::page_ring_type
     private
     logical::asynchronous=serialize_default_asynchronous
     logical::eof_reached=.false.
     integer::unit=-1
     integer(kind=dik)::ring_size=2
     integer(kind=dik)::action=0
     integer(kind=dik)::eof_int=-1
     integer(kind=dik)::out_unit=output_unit
     integer(kind=dik)::err_unit=error_unit
     integer(kind=dik),dimension(2)::active_pages=[0,-1]
     integer(kind=dik),dimension(2)::eof_pos=[-1,-1]
     type(string_t)::eof_string
     type(position_stack_type)::position_stack
     character(serialize_page_size),dimension(:),allocatable::ring     
   contains
     ! read access only procedures:
     procedure,public::open_for_read_access=>page_ring_open_for_read_access
     procedure,public::read_page=>page_ring_read_page
     ! write access only procedures:
     procedure,public::open_for_write_access=>page_ring_open_for_write_access
     procedure,public::flush=>page_ring_flush
     procedure,public::break=>page_ring_break
     ! comparing
     procedure,public::str_equal=>page_ring_str_equal
     ! searching:
     procedure,public::find_pure => page_ring_find_pure
     generic,  public::find=>page_ring_find,page_ring_find_default
     ! positioning:
     procedure,public::set_position=>page_ring_set_position
     procedure,public::turn_page=>page_ring_turn_page
     procedure,public::proceed=>page_ring_proceed
     generic,  public::push_position=>push_actual_position,push_given_position
     generic,  public::pop_position=>pop_actual_position,pop_given_position
     generic,  public::get_position=>page_ring_get_position1,page_ring_get_position2
     ! printing:
     procedure,public::print_to_unit=>page_ring_print_to_unit
     procedure,public::print_ring=>page_ring_print_ring
     procedure,public::print_position=>page_ring_print_position
     ! writing:
     procedure,public::put=>page_ring_put
     generic,  public::push=>&
          push_string,&
          push_integer,&
          push_integer_dik,&
          push_double,&
          push_integer_array,&
          push_integer_array_dik,&
          push_double_array
     ! reading:
     procedure,public::get_character=>page_ring_get_character
     procedure,public::allocate_substring=>page_ring_allocate_substring     
     procedure,public::pop_character=>page_ring_pop_character
     procedure,public::pop_by_keys=>page_ring_pop_by_keys
     generic,  public::substring=>page_ring_substring1,page_ring_substring2
     generic,  public::substring_by_keys=>page_ring_character_by_keys,page_ring_positions_by_keys
     generic,  public::pop=>&
          pop_string,&
          pop_integer,&
          pop_integer_dik,&
          pop_double,&
          pop_logical,&
          pop_integer_array,&
          pop_integer_array_dik,&
          pop_double_array
     ! misc:
     procedure,public::close=>page_ring_close
     procedure,public::ring_index=>page_ring_ring_index
     ! private:
     procedure,private::activate_next_page=>page_ring_activate_next_page
     procedure,private::enlarge=>page_ring_enlarge
     ! specific names for generic procedures:
     procedure,private::page_ring_substring1
     procedure,private::page_ring_substring2
     procedure,private::page_ring_character_by_keys
     procedure,private::page_ring_positions_by_keys
     procedure,private::push_string=>page_ring_push_string
     procedure,private::push_integer=>page_ring_push_integer
     procedure,private::push_integer_dik=>page_ring_push_integer_dik
     procedure,private::push_integer_array=>page_ring_push_integer_array
     procedure,private::push_integer_array_dik=>page_ring_push_integer_array_dik
     procedure,private::push_double=>page_ring_push_double
     procedure,private::push_double_array=>page_ring_push_double_array

     procedure,private::pop_string=>page_ring_pop_string
     procedure,private::pop_integer=>page_ring_pop_integer
     procedure,private::pop_integer_dik=>page_ring_pop_integer_dik
     procedure,private::pop_logical=>page_ring_pop_logical
     procedure,private::pop_integer_array=>page_ring_pop_integer_array
     procedure,private::pop_integer_array_dik=>page_ring_pop_integer_array_dik
     procedure,private::pop_double=>page_ring_pop_double
     procedure,private::pop_double_array=>page_ring_pop_double_array

     procedure,private::page_ring_find
     procedure,private::page_ring_find_default

     procedure,private::actual_index=>page_ring_actual_index
     procedure,private::actual_page=>page_ring_actual_page
     procedure,private::actual_offset=>page_ring_actual_offset
     procedure,private::actual_position=>page_ring_actual_position
     procedure,private::first_index=>page_ring_first_index
     procedure,private::first_page=>page_ring_first_page
     procedure,private::last_index=>page_ring_last_index
     procedure,private::last_page=>page_ring_last_page

     procedure,private::push_actual_position=>page_ring_ring_push_actual_position
     procedure,private::push_given_position=>page_ring_ring_push_given_position
     procedure,private::pop_actual_position=>page_ring_ring_pop_actual_position
     procedure,private::pop_given_position=>page_ring_ring_pop_given_position
     procedure,private::page_ring_get_position1
     procedure,private::page_ring_get_position2
  end type page_ring_type

  type,public,extends(page_ring_type)::marker_type
     private
     integer(kind=dik)::indentation=0
     integer(kind=dik)::n_instances=0
     logical::do_break=.true.
     logical::do_indent=.false.
     class(serializable_ref_type),pointer::heap=>null()
     class(serializable_ref_type),pointer::references=>null()
   contains
     procedure::mark_begin=>marker_mark_begin
     procedure::mark_instance_begin=>marker_mark_instance_begin
     procedure::mark_end=>marker_mark_end
     procedure::mark_instance_end=>marker_mark_instance_end
     procedure::mark_logical=>marker_mark_logical
     procedure::mark_integer=>marker_mark_integer
     procedure::mark_integer_array=>marker_mark_integer_array
     procedure::mark_integer_matrix=>marker_mark_integer_matrix
     procedure::mark_integer_dik=>marker_mark_integer_dik
     procedure::mark_integer_array_dik=>marker_mark_integer_array_dik
     procedure::mark_integer_matrix_dik=>marker_mark_integer_matrix_dik
     procedure::mark_double=>marker_mark_double
     procedure::mark_double_array=>marker_mark_double_array
     procedure::mark_double_matrix=>marker_mark_double_matrix
     procedure::mark_string=>marker_mark_string
     procedure::mark_instance=>marker_mark_instance
     procedure::mark_target=>marker_mark_target
     procedure::mark_allocatable=>marker_mark_allocatable
     procedure::mark_pointer=>marker_mark_pointer
     procedure::mark_null=>marker_mark_null
     procedure::mark_nothing=>marker_mark_nothing
     procedure::mark_empty=>marker_mark_empty
     procedure::pick_begin=>marker_pick_begin
     procedure::query_instance_begin=>marker_query_instance_begin
     procedure::pick_instance_begin=>marker_pick_instance_begin
     procedure::pick_end=>marker_pick_end
     procedure::pick_instance_end=>marker_pick_instance_end
     procedure::pick_instance=>marker_pick_instance
     procedure::pick_target=>marker_pick_target
     procedure::pick_allocatable=>marker_pick_allocatable
     procedure::pick_pointer=>marker_pick_pointer
     procedure::pick_logical=>marker_pick_logical
     procedure::pick_integer=>marker_pick_integer
     procedure::pick_integer_array=>marker_pick_integer_array
     procedure::pick_integer_matrix=>marker_pick_integer_matrix
     procedure::pick_integer_dik=>marker_pick_integer_dik
     procedure::pick_integer_array_dik=>marker_pick_integer_array_dik
     procedure::pick_integer_matrix_dik=>marker_pick_integer_matrix_dik
     procedure::pick_double=>marker_pick_double
     procedure::pick_double_array=>marker_pick_double_array
     procedure::pick_double_matrix=>marker_pick_double_matrix
     procedure::pick_string=>marker_pick_string
     generic,public::mark=>mark_logical,&
          mark_integer,mark_integer_array,mark_integer_matrix,&
          mark_integer_dik,mark_integer_array_dik,mark_integer_matrix_dik,&
          mark_double,mark_double_array,mark_double_matrix,mark_string
     generic,public::pick=>pick_logical,&
          pick_integer,pick_integer_array,pick_integer_matrix,&
          pick_integer_dik,pick_integer_array_dik,pick_integer_matrix_dik,&
          pick_double,pick_double_array,pick_double_matrix,pick_string
     procedure::verify_nothing=>marker_verify_nothing
     procedure::indent=>marker_indent
     procedure::push_heap=>marker_push_heap
     procedure::pop_heap=>marker_pop_heap
     procedure::search_heap_by_id=>marker_search_heap_by_id
     procedure::search_heap_by_ref=>marker_search_heap_by_ref
     procedure::push_reference=>marker_push_reference
     procedure::pop_reference=>marker_pop_reference
     procedure::reset_references=>marker_reset_references
     procedure::search_reference=>marker_search_reference
     procedure::reset_heap=>marker_reset_heap     
     procedure::finalize=>marker_finalize
     generic::search_heap=>search_heap_by_id,search_heap_by_ref
  end type marker_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Interface Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  abstract interface
     elemental function measure_int(this)
       import measurable_class
       import drk
       class(measurable_class),intent(in)::this
       real(kind=drk)::measure_int
     end function measure_int
  end interface
  interface operator(<)
     module procedure measurable_less_measurable
     module procedure measurable_less_double
  end interface
  interface operator(<=)
     module procedure measurable_less_or_equal_measurable
     module procedure measurable_less_or_equal_double
  end interface
  interface operator(==)
     module procedure measurable_equal_measurable
     module procedure measurable_equal_double
  end interface
  interface operator(>=)
     module procedure measurable_equal_or_greater_measurable
     module procedure measurable_equal_or_greater_double
  end interface
  interface operator(>)
     module procedure measurable_greater_measurable
     module procedure measurable_greater_double
  end interface
  abstract interface
     subroutine ser_write_if(this,marker,status)
       import serializable_class
       import marker_type
       import dik
       class(serializable_class),intent(in)::this
       class(marker_type),intent(inout)::marker
       integer(kind=dik),intent(out)::status
     end subroutine ser_write_if
  end interface
  abstract interface
     subroutine ser_read_if(this,marker,status)
       import serializable_class
       import marker_type
       import dik
       class(serializable_class),intent(out)::this
       class(marker_type),intent(inout)::marker
       integer(kind=dik),intent(out)::status
     end subroutine ser_read_if
  end interface
  abstract interface
     subroutine ser_unit(this,unit,parents,components,peers)
       import serializable_class
       import dik
       class(serializable_class),intent(in)::this
       integer,intent(in)::unit
       integer(kind=dik),intent(in)::parents,components,peers
     end subroutine ser_unit
  end interface
  abstract interface
     pure subroutine ser_type(type)
       character(:),allocatable,intent(out)::type
     end subroutine ser_type
  end interface
  interface page_ring_position_is_before
     module procedure &
          page_ring_position_is_before_int_pos,&
          page_ring_position_is_before_pos_pos,&
          page_ring_position_is_before_pos_int
  end interface

  public operator(<),operator(<=),operator(>=),operator(>)
  public serialize_print_comp_pointer,serialize_print_peer_pointer&
       &,serialize_print_allocatable
  public identified_initialize,identified_print_to_unit&
       &,identified_read_from_marker,identified_write_to_marker

  public serializable_deserialize_from_marker
  public ilog2,generate_unit,integer_with_leading_zeros

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for serializable_class !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  elemental logical function serializable_verify_type(type) result(match)
    character(*),intent(in)::type
    match=type=="serializable_class"
  end function serializable_verify_type

  subroutine serializable_read_target_from_marker(this,marker,status)
    ! This is a dummy procedure. Usually, you dont't need to deserialize targets,
    ! so by implementing this dummy we don"t force all descendants to override this
    ! procedure. Then again this is the only way to read targets from markers.
    class(serializable_class),target,intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    print *,"serializable_read_target_from_marker:"
    print *,"This is a dummy procedure. Usually, this message indicates a missing overridden &
         &read_target_from_marker TPB for "
    call this%write_type(output_unit)
    print *,""
    call this%read_from_marker(marker,status)
  end subroutine serializable_read_target_from_marker

  subroutine serializable_write_type(this,unit)
    class(serializable_class),intent(in)::this
    integer,intent(in)::unit
    character(:),allocatable::this_type
    call this%get_type(this_type)
    write(unit,fmt='(a)',advance="no")this_type
  end subroutine serializable_write_type

  recursive subroutine serializable_print(this,parents,components,peers,unit)
    class(serializable_class),intent(in)::this
    integer(kind=dik),intent(in)::parents,components,peers
    integer,optional::unit
    if(present(unit))then
       write(unit,'("")')
       write(unit,'("Instance of type: ")',advance="no")
       call this%write_type(unit)
       write(unit,fmt='("")')
       call this%print_to_unit(unit,parents,components,peers)
    else
       write(output_unit,'("")')
       write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
       call this%print_to_unit(output_unit,parents,components,peers)
    end if
  end subroutine serializable_print

  recursive subroutine serializable_print_all(this,unit)
    class(serializable_class),intent(in)::this
    integer,optional::unit
    if(present(unit))then
       write(unit,'("")')
       write(unit,'("Instance of type: ")',advance="no")
       call this%write_type(unit)
       write(unit,fmt='("")')
       call this%print_to_unit(unit,huge(one),huge(one),huge(one))
    else
       write(output_unit,'("")')
       write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
       call this%print_to_unit(output_unit,huge(one),huge(one),huge(one))
    end if
  end subroutine serializable_print_all

  recursive subroutine serializable_print_little(this,unit)
    class(serializable_class),intent(in)::this
    integer,optional::unit
    if(present(unit))then
       write(unit,'("")')
       write(unit,'("Instance of type: ")',advance="no")
       call this%write_type(unit)
       write(unit,fmt='("")')
       call this%print_to_unit(unit,zero,zero,zero)
    else
       write(output_unit,'("")')
       write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
       call this%print_to_unit(output_unit,zero,zero,zero)
    end if
  end subroutine serializable_print_little

  recursive subroutine serializable_print_parents(this)
    class(serializable_class),intent(in)::this
    write(output_unit,'("")')
    write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
    call this%print_to_unit(output_unit,huge(one),zero,zero)
  end subroutine serializable_print_parents

  recursive subroutine serializable_print_components(this)
    class(serializable_class),intent(in)::this
    write(output_unit,'("")')
    write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
    call this%print_to_unit(output_unit,zero,huge(one),zero)
  end subroutine serializable_print_components

  recursive subroutine serializable_print_peers(this)
    class(serializable_class),intent(in)::this
    write(output_unit,'("")')
    write(output_unit,'("Instance of type: ")',advance="no")
       call this%write_type(output_unit)
       write(output_unit,fmt='("")')
    call this%print_to_unit(output_unit,zero,zero,huge(one))
  end subroutine serializable_print_peers

  recursive subroutine serializable_print_error(this)
    class(serializable_class),intent(in)::this
    call this%print_to_unit(error_unit,zero,zero,zero)
  end subroutine serializable_print_error

  subroutine serializable_serialize_to_unit(this,unit,name)
    class(serializable_class),intent(in)::this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: name
    logical::opened
    character(32)::file
    !    gfortran bug
    !    character::stream
    character::write
    type(marker_type)::marker
    !    inquire(unit=unit,opened=opened,stream=stream,write=write)
    inquire(unit=unit,opened=opened,write=write)
    if(opened)then
!       if(stream=="Y")then
          if(write=="Y")then
             print *,"dummy: serializable_serialize_to_unit"
             stop
          else
             print *,"serializable_serialize_to_unit: cannot write to read-only unit."
          end if
!       else
!          print *,"serializable_serialize_to_unit: access kind of unit is not 'stream'."
!       end if
    else
       print *,"serializable_serialize_to_unit: file is not opened."
    end if
  end subroutine serializable_serialize_to_unit

  subroutine serializable_serialize_to_file(this,name,file)
    class(serializable_class),intent(in)::this
    character (len=*), intent(in) :: file,name
    type(marker_type)::marker
    call marker%open_for_write_access(file)
    print *,"serializable_serialize_to_file: writing xml preamble to ",file
    call marker%activate_next_page()
    call marker%push('<?xml version="1.0"?>')
    call marker%mark_begin(tag="file",name=file)
    flush(marker%unit)
    call this%serialize_to_marker(marker,name)
    call marker%mark_end("file")
    call marker%close()
    call marker%finalize()
  end subroutine serializable_serialize_to_file

  recursive subroutine serializable_serialize_to_marker(this,marker,name)
    class(serializable_class),intent(in)::this
    class(marker_type),intent(inout)::marker
    character (len=*), intent(in) :: name
    if(marker%action==1)then
       call marker%mark_instance(this,name)
    else       
       print *,"serializable_serialize_to_marker: Marker is not ready for write access. STOP."
       stop
    end if
  end subroutine serializable_serialize_to_marker

 subroutine serializable_deserialize_from_unit(this,unit,name)
    class(serializable_class),intent(inout)::this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: name
    logical::opened
    !    gfortran bug
    !    character::stream
    character::read
    type(marker_type)::marker
    !    inquire(unit=unit,opened=opened,stream=stream,read=read)
    inquire(unit=unit,opened=opened,read=read)
    if(opened)then
!       if(stream=="Y")then
          if(read=="Y")then
             print *,"dummy: serializable_serialize_from_unit"
             stop
          else
             print *,"serializable_serialize_from_unit: cannot write from read-only unit."
          end if
!       else
!          print *,"serializable_serialize_from_unit: access kind of unit is not 'stream'."
!       end if
    else
       print *,"serializable_serialize_from_unit: file is not opened."
    end if
  end subroutine serializable_deserialize_from_unit

  subroutine serializable_deserialize_from_marker(this,name,marker)
    class(serializable_class),intent(out)::this
    character(*),intent(in)::name
    class(marker_type),intent(inout)::marker
    integer(kind=dik)::status
    if(marker%action==2)then
       call marker%pick_instance(name,this,status)
    else       
       print *,"serializable_deserialize_from_ring: Ring is not ready for read access. STOP."
       stop
    end if
  end subroutine serializable_deserialize_from_marker

  subroutine serializable_deserialize_from_file(this,name,file)
    class(serializable_class),intent(out)::this
    character(*),intent(in)::name,file
    type(marker_type)::marker
    integer(kind=dik),dimension(2)::p1,p2
    call marker%open_for_read_access(file,"</file>")
    marker%eof_int=huge(one)
    marker%eof_pos=page_ring_position(marker%eof_int)
    call marker%read_page()
    call marker%find('<?',skip=2,proceed=.true.,pos=p1)
    call marker%find('?>',skip=3,proceed=.false.,pos=p2)
    if((p1(2)<=0).or.(p2(2)<=0))then
       print *,"no version substring found."
    end if
    call marker%set_position(p2)
    call marker%find('<file ',skip=4,proceed=.true.,pos=p1)
    call marker%find('>',skip=1,proceed=.false.,pos=p2)
    if((p1(2)>0).and.(p2(2)>0))then
       call marker%push_position(p2)
       call marker%find('name="',skip=4,proceed=.true.,pos=p1)
       call marker%find('"',skip=1,proceed=.false.,pos=p2)
       call marker%pop_position()
    else
       print *,"no file header found. STOP."
       STOP
    end if
    call this%deserialize_from_marker(name,marker)
    call marker%close()
    call marker%finalize()
  end subroutine serializable_deserialize_from_file

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for identified_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine identified_initialize(this,id,name)
    class(identified_type),intent(out)::this
    integer(kind=dik),intent(in)::id
    character(len=*),intent(in)::name
    this%name=name
    this%id=id
  end subroutine identified_initialize

  elemental function identified_get_id(this) result(id)
    class(identified_type),intent(in)::this
    integer(kind=dik)::id
    id=this%id
  end function identified_get_id

  pure function identified_get_name(this)
    class(identified_type),intent(in)::this
    character(len(this%name))::identified_get_name
    identified_get_name=char(this%name)
  end function identified_get_name

  subroutine identified_write_to_marker(this,marker,status)
    class(identified_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("identified_type")
    call marker%mark("name",this%get_name())
    call marker%mark("id",this%get_id())
    call marker%mark_end("identified_type")
  end subroutine identified_write_to_marker

  subroutine identified_read_from_marker(this,marker,status)
    class(identified_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    character(:),allocatable::name
    call marker%pick_begin("identified_type",status=status)
    call marker%pick("name",name,status)
    call marker%pick("id",this%id,status)
    call marker%pick_end("identified_type",status=status)
    this%name=name
  end subroutine identified_read_from_marker

  subroutine identified_print_to_unit(this,unit,parents,components,peers)
    class(identified_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of identified_type:")')
    write(unit,'("Name:             ",a)')this%get_name()
    write(unit,'("ID:               ",I10)')this%get_id()
  end subroutine identified_print_to_unit
  
  pure subroutine identified_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="identified_type")
  end subroutine identified_get_type

  elemental logical function identified_verify_type(type)
    character(len=*),intent(in)::type
    identified_verify_type=(type=="identified_type")
  end function identified_verify_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for unique_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine unique_initialize(this,id,name)
    class(unique_type),intent(out)::this
    integer(kind=dik),intent(in)::id
    character(len=*),intent(in)::name
    call identified_initialize(this,id,name)
    last_id=last_id+1
    this%unique_id=last_id
  end subroutine unique_initialize

  subroutine unique_print_to_unit(this,unit,parents,components,peers)
    class(unique_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    if(parents>0)call identified_print_to_unit(this,unit,parents-1,components&
         &,peers)
    write(unit,'("Unique ID:        ",I10)')this%get_unique_id()
  end subroutine unique_print_to_unit

  elemental function unique_get_unique_id(this)
    class(unique_type),intent(in)::this
    integer(kind=dik)::unique_get_unique_id
    unique_get_unique_id=this%unique_id
  end function unique_get_unique_id

  pure subroutine unique_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="unique_type")
  end subroutine unique_get_type

  elemental logical function unique_verify_type(type)
    character(len=*),intent(in)::type
    unique_verify_type=(type=="unique_type")
  end function unique_verify_type

  subroutine unique_write_to_marker(this,marker,status)
    class(unique_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("unique_type")
    call identified_write_to_marker(this,marker,status)
    call marker%mark("unique_id",this%get_unique_id())
    call marker%mark_end("unique_type")
  end subroutine unique_write_to_marker

  subroutine unique_read_from_marker(this,marker,status)
    class(unique_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("unique_type",status=status)    
    call identified_read_from_marker(this,marker,status)
    call marker%pick("unique_id",this%unique_id,status)
    call marker%pick_end("unique_type",status)
  end subroutine unique_read_from_marker

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for serializable_ref_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine serializable_ref_finalize(this)
    class(serializable_ref_type),intent(inout)::this
    class(serializable_ref_type),pointer::next
    do while (associated(this%next))
       next=>this%next
       this%next=>next%next
       nullify(next%ref)
       deallocate(next)
    end do
    if(associated(this%ref))nullify(this%ref)
  end subroutine serializable_ref_finalize

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for position_stack_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine position_stack_push_head(this)
    class(position_stack_type)::this
    class(position_stack_type),pointer::new
    allocate(new)
    new%next=>this%next
    new%position=this%position
    this%next=>new
  end subroutine position_stack_push_head

  subroutine position_stack_push_given(this,position)
    class(position_stack_type)::this
    integer(kind=dik),dimension(2),intent(in)::position
    class(position_stack_type),pointer::new
    allocate(new)
    new%next=>this%next
    new%position=position
    this%next=>new
  end subroutine position_stack_push_given

  subroutine position_stack_pop(this)
    class(position_stack_type)::this
    class(position_stack_type),pointer::old
    if(associated(this%next))then
       old=>this%next
       this%next=>old%next
       this%position=old%position
       deallocate(old)
    end if
  end subroutine position_stack_pop

  subroutine position_stack_drop(this,position)
    class(position_stack_type)::this
    integer(kind=dik),dimension(2),intent(out)::position
    class(position_stack_type),pointer::old
    if(associated(this%next))then
       old=>this%next
       this%next=>old%next
       position=old%position
       deallocate(old)
    else
       position=[0,0]
    end if
  end subroutine position_stack_drop

  function position_stack_nth_position(this,n) result(position)
    class(position_stack_type),intent(in)::this
    integer(kind=dik),intent(in)::n
    integer(kind=dik),dimension(2)::position
    class(position_stack_type),pointer::tmp
    integer(kind=dik)::pos
    tmp=>this%next
    pos=n
    do while(associated(tmp).and.pos>0)
       tmp=>tmp%next
       pos=pos-1
    end do
    if(associated(tmp))then
       position=tmp%position
    else
       position=[0,0]
    end if
  end function position_stack_nth_position
  
  function position_stack_first(this) result(position)
    class(position_stack_type),intent(in)::this
    integer(kind=dik),dimension(2)::position,tmp_position
    class(position_stack_type),pointer::tmp_stack
    tmp_position=this%position
    tmp_stack=>this%next
    do while(associated(tmp_stack))
       if(page_ring_position_is_before(tmp_stack%position,tmp_position))then
          tmp_position=tmp_stack%position
       end if
       tmp_stack=>tmp_stack%next
    end do
  end function position_stack_first
  
  function position_stack_last(this) result(position)
    class(position_stack_type),intent(in)::this
    integer(kind=dik),dimension(2)::position,tmp_position
    class(position_stack_type),pointer::tmp_stack
    tmp_position=this%position
    tmp_stack=>this%next
    do while(associated(tmp_stack))
       if(page_ring_position_is_before(tmp_position,tmp_stack%position))then
          tmp_position=tmp_stack%position
       end if
       tmp_stack=>tmp_stack%next
    end do
  end function position_stack_last
  
  pure function position_stack_range(this) result(position)
    class(position_stack_type),intent(in)::this
    integer(kind=dik),dimension(2)::position
    class(position_stack_type),pointer::tmp
  end function position_stack_range
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for page_ring_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine page_ring_open_for_read_access(this,file,eof_string,asynchronous)
    class(page_ring_type),intent(inout)::this
    character(*),intent(in)::file,eof_string
    logical,intent(in),optional::asynchronous
    logical::exist
    this%eof_string=eof_string
    inquire(file=file,exist=exist)
    if(exist)then
       this%action=2
    else
       print *,"page_ring_open: File ",file," is opened for read access but does not exist. STOP."
       STOP
    end if

    if(present(asynchronous))this%asynchronous=asynchronous
    if(this%unit<0)call generate_unit(this%unit,100,1000)
    if(this%unit<0)then
       print *,"page_ring_open: No free unit found. STOP."
       STOP
    end if
    this%ring_size=2
    call this%set_position([zero,one])
    this%active_pages=[zero,-one]
    if(allocated(this%ring))deallocate(this%ring)
    allocate(this%ring(zero:this%ring_size-one))
    if(this%asynchronous)then
       open(this%unit,file=file,access="stream",action="read",asynchronous="yes",status="old")
    else
       open(this%unit,file=file,access="stream",action="read",asynchronous="no",status="old")
    end if
    call this%read_page()
  end subroutine page_ring_open_for_read_access

  subroutine page_ring_open_for_write_access(this,file,asynchronous)
    class(page_ring_type),intent(inout)::this
    character(*),intent(in)::file
    logical,intent(in),optional::asynchronous
    this%action=1

    if(present(asynchronous))this%asynchronous=asynchronous
    if(this%unit<0)call generate_unit(this%unit,100,1000)
    if(this%unit<0)then
       print *,"page_ring_open: No free unit found. STOP."
       STOP
    end if
    this%ring_size=2
    call this%set_position([zero,one])
    this%active_pages=[zero,-one]
    if(allocated(this%ring))deallocate(this%ring)
    allocate(this%ring(zero:this%ring_size-one))

    if(this%asynchronous)then
       open(this%unit,file=file,access="stream",action="write",asynchronous="yes",status="replace")
    else
       open(this%unit,file=file,access="stream",action="write",asynchronous="no",status="replace")
    end if
  end subroutine page_ring_open_for_write_access

  subroutine page_ring_close(this)
    class(page_ring_type),intent(inout)::this
    if(this%action==1)then
       call this%flush()
       !call this%print_position()
       if(this%asynchronous)then
          write(this%unit,asynchronous="yes")&
               &this%ring(this%actual_index())(:this%actual_offset()-1)
       else
          write(this%unit,asynchronous="no")&
               &this%ring(this%actual_index())(:this%actual_offset()-1)
       end if
    end if
    close(this%unit)
  end subroutine page_ring_close

  subroutine page_ring_read_page(this)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik)::iostat
    character(8)::iomsg
    if(.not.this%eof_reached)then
       call page_ring_activate_next_page(this)
       read(this%unit,iostat=iostat)this%ring(this%last_index())
       if(iostat==iostat_end)then
          this%eof_reached=.true.
          this%eof_pos(1)=this%last_page()
          this%eof_pos(2)=index(this%ring(this%last_index()),char(this%eof_string))
          this%eof_pos(2)=this%eof_pos(2)+len(this%eof_string)-1
          this%eof_int=page_ring_ordinal(this%eof_pos)
       end if
    end if
  end subroutine page_ring_read_page

  subroutine page_ring_enlarge(this)
    class(page_ring_type),intent(inout)::this
    character(serialize_page_size),dimension(:),allocatable::tmp_ring
    integer(kind=dik)::n
    call move_alloc(this%ring,tmp_ring)
    allocate(this%ring(0:this%ring_size*2-1))
    do n=this%active_pages(1),this%active_pages(2)
       this%ring(mod(n,this%ring_size*2))=tmp_ring(mod(n,this%ring_size))
    end do
    this%ring_size=this%ring_size*2
  end subroutine page_ring_enlarge
  
  subroutine page_ring_print_to_unit(this,unit,parents,components,peers)
    class(page_ring_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of page_ring_type:")')
    print *,"asynchronous: ",this%asynchronous
    print *,"eof reached:  ",this%eof_reached
    print *,"ring_size:    ",this%ring_size
    print *,"unit:         ",this%unit
    print *,"action:       ",this%action
    print *,"position:     ",this%position_stack%position
    print *,"active_pages: ",this%active_pages
    print *,"file size:    ",this%eof_int
    print *,"eof position: ",this%eof_pos
    print *,"eof string:   ",char(this%eof_string)
    if(allocated(this%ring))then
       print *,"Ring is allocated."
       if(components>0)call this%print_ring(unit)
    else
       print *,"Ring is not allocated."
    end if
  end subroutine page_ring_print_to_unit
  
  subroutine page_ring_print_ring(this,unit)
    class(page_ring_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik)::n
    write(unit,fmt=*)"Begin of page ring"
    do n=this%active_pages(1),this%active_pages(2)
       write(unit=unit,fmt="('(',I0,')',a)")n,this%ring(mod(n,this%ring_size))
    end do
    write(unit,fmt=*)"End of page ring"
  end subroutine page_ring_print_ring
  
  recursive subroutine page_ring_push_string(this,string)
    class(page_ring_type),intent(inout)::this
    character(*),intent(in)::string
    integer(kind=dik)::cut,l
    l=len(string)
    if(l<=serialize_page_size-this%actual_offset()+1)then
       this%ring(this%actual_index())(this%actual_offset():this%actual_offset()+l-1)=string
       if(l==serialize_page_size-this%actual_offset()+1)then
          call this%break()
          call this%flush()
       else
          call this%proceed(l)
       end if       
    else
       cut=serialize_page_size-this%actual_offset()+1
       call this%push_string(string(:cut))
       call this%push_string(string(cut+1:))
    end if
  end subroutine page_ring_push_string

  recursive subroutine page_ring_push_integer_dik(this,int)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),intent(in)::int
    integer(kind=dik)::int1
    if(int<0)then
       call this%push("-")
       call page_ring_push_integer_dik(this,-int)
    else
       if(int>9)call this%push(int/10)
       int1=mod(int,10*one)
       select case (int1)
       case (0)
          call this%push("0")
       case (1)
          call this%push("1")
       case (2)
          call this%push("2")
       case (3)
          call this%push("3")
       case (4)
          call this%push("4")
       case (5)
          call this%push("5")
       case (6)
          call this%push("6")
       case (7)
          call this%push("7")
       case (8)
          call this%push("8")
       case (9)
          call this%push("9")
       end select
    end if
  end subroutine page_ring_push_integer_dik

  subroutine page_ring_push_integer(this,in)
    class(page_ring_type),intent(inout)::this
    integer,intent(in)::in
    call page_ring_push_integer_dik(this,int(in,kind=dik))
  end subroutine page_ring_push_integer

  subroutine page_ring_pop_integer(this,in)
    class(page_ring_type),intent(inout)::this
    integer,intent(out)::in
    integer(kind=dik)::in_dik
    call page_ring_pop_integer_dik(this,in_dik)
    in=int(in_dik)
  end subroutine page_ring_pop_integer

  subroutine page_ring_pop_integer_dik(this,int)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),intent(out)::int
    integer(kind=dik)::int1
    integer(kind=dik)::sign
    character::c
    int=0
    sign=1
    c=" "
    do while(scan(c,serialize_integer_characters)==0)
       call this%pop_character(c)
    end do
    if(c=="-")then
       sign=-1
       call this%pop_character(c)
    end if
    do while(scan(c,serialize_integer_characters)>0)
       int=int*10
       select case (c)
       case ("1")
          int=int+1
       case ("2")
          int=int+2
       case ("3")
          int=int+3
       case ("4")
          int=int+4
       case ("5")
          int=int+5
       case ("6")
          int=int+6
       case ("7")
          int=int+7
       case ("8")
          int=int+8
       case ("9")
          int=int+9
       end select
       call this%pop_character(c)
    end do
    int=int*sign
    if(c=="<")call this%proceed(-one)
  end subroutine page_ring_pop_integer_dik

  subroutine page_ring_pop_logical(this,l)
    class(page_ring_type),intent(inout)::this
    logical,intent(out)::l
    character(1)::lc
    call this%pop(lc)
    do while(scan(lc,"tTfF")==0)
       call this%pop(lc)
    end do
    read(lc,fmt="(l1)")l
  end subroutine page_ring_pop_logical

  subroutine page_ring_push_integer_array_dik(this,int)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(:),intent(in)::int
    integer(kind=dik)::n
    do n=1,size(int)
       call this%push(int(n))
       call this%push(" ")
    end do
  end subroutine page_ring_push_integer_array_dik
  
  subroutine page_ring_push_integer_array(this,int)
    class(page_ring_type),intent(inout)::this
    integer,dimension(:),intent(in)::int
    integer::n
    do n=1,size(int)
       call this%push(int(n))
       call this%push(" ")
    end do
  end subroutine page_ring_push_integer_array
  
  subroutine page_ring_pop_integer_array(this,int)
    class(page_ring_type),intent(inout)::this
    integer,dimension(:),intent(out)::int
    integer::n
    do n=1,size(int)
       call this%pop(int(n))
    end do
  end subroutine page_ring_pop_integer_array

   subroutine page_ring_pop_integer_array_dik(this,int)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(:),intent(out)::int
    integer(kind=dik)::n
    do n=1,size(int)
       call this%pop(int(n))
    end do
  end subroutine page_ring_pop_integer_array_dik

  subroutine page_ring_push_double(this,dou)
    class(page_ring_type),intent(inout)::this
    real(kind=drk),intent(in)::dou
    integer(kind=dik)::f
!    print *,"page_ring_push_double: ",dou
    if(dou==0D0)then
       call this%push("0")
    else
       f=int(scale(fraction(dou),digits(dou)),kind=dik)
       call this%push(digits(dou))
       call this%push(":")    
       call this%push(f)
       call this%push(":")
       call this%push(exponent(dou))
    end if
    call this%push(" ")
  end subroutine page_ring_push_double

  subroutine page_ring_push_double_array(this,dou)
    class(page_ring_type),intent(inout)::this
    real(kind=drk),dimension(:),intent(in)::dou
    integer(kind=dik)::n
    do n=1,size(dou)
       call this%push(dou(n))
    end do
  end subroutine page_ring_push_double_array

  subroutine page_ring_pop_double(this,dou,skip)
    class(page_ring_type),intent(inout)::this
    real(kind=drk),intent(out)::dou
    logical,optional,intent(in)::skip
    integer(kind=dik)::d,f,e
    call this%pop(d)
    if(d==zero)then
       dou=0D0
    else
       call this%pop(f)
       call this%pop(e)
       dou=set_exponent(scale(real(f,kind=double),-d),e)
    end if
    if(present(skip))then
       if(.not.skip)call this%proceed(-one)
    end if
  end subroutine page_ring_pop_double

  subroutine page_ring_pop_double_array(this,dou,skip)
    class(page_ring_type),intent(inout)::this
    real(kind=drk),dimension(:),intent(out)::dou
    logical,optional,intent(in)::skip
    integer(kind=dik)::n    
    call this%pop_double(dou(1))
    do n=2,size(dou)
       call this%pop_double(dou(n))
    end do
    if(present(skip))then
       if(.not.skip)call this%proceed(-one)
    end if
  end subroutine page_ring_pop_double_array

  subroutine page_ring_pop_character(this,c)
    class(page_ring_type),intent(inout)::this
    character,intent(out)::c
    c=this%ring(this%actual_index())(this%actual_offset():this%actual_offset())
    if(this%actual_offset()==serialize_page_size)call this%read_page
    call this%proceed(one)
  end subroutine page_ring_pop_character

  recursive subroutine page_ring_pop_string(this,res)
    class(page_ring_type),intent(inout)::this
    character(len=*),intent(out)::res
    integer(kind=dik)::n,cut
    n=len(res)
    cut=serialize_page_size-this%actual_offset()+1
    if(n<=cut)then
       res=this%ring(this%actual_index())(this%actual_offset():this%actual_offset()+n)
       if(n==cut)then
          call this%read_page
       end if
       call this%proceed(n)
    else
       call page_ring_pop_string(this,res(:cut))
       call page_ring_pop_string(this,res(cut+1:))
    end if
  end subroutine page_ring_pop_string

  pure function page_ring_substring2(this,i1,i2) result(res)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2),intent(in)::i1,i2
    character(ring_position_metric2(i1,i2))::res
    integer(kind=dik)::page,pos
    if(i1(1)==i2(1))then
       res=this%ring(mod(i1(1),this%ring_size))(i1(2):i2(2))
    else
       pos=serialize_page_size-i1(2)
       res(1:pos+1)=this%ring(mod(i1(1),this%ring_size))(i1(2):)
       do page=i1(1)+1,i2(1)-1
          res(pos+2:pos+2+serialize_page_size)=this%ring(mod(page,this%ring_size))
          pos=pos+serialize_page_size
       end do
       res(pos+2:pos+1+i2(2))=this%ring(mod(page,this%ring_size))(1:i2(2))
    end if
  end function page_ring_substring2

  pure function page_ring_substring1(this,i) result(res)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2,2),intent(in)::i
    character(ring_position_metric1(i))::res
    integer(kind=dik)::page,pos
    if(i(1,1)==i(1,2))then
       res=this%ring(mod(i(1,1),this%ring_size))(i(2,1):i(2,2))
    else
       pos=serialize_page_size-i(2,1)
       res(1:pos+1)=this%ring(mod(i(1,1),this%ring_size))(i(2,1):)
       do page=i(1,1)+1,i(1,1)-1
          res(pos+2:pos+2+serialize_page_size)=this%ring(mod(page,this%ring_size))
          pos=pos+serialize_page_size
       end do
       res(pos+2:pos+1+i(2,2))=this%ring(mod(page,this%ring_size))(1:i(2,2))
    end if
  end function page_ring_substring1

  subroutine page_ring_allocate_substring(this,p1,p2,string)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2),intent(in)::p1,p2
    character(:),allocatable,intent(out)::string
    string=page_ring_substring2(this,p1,p2)
  end subroutine page_ring_allocate_substring

  subroutine page_ring_find_default(this,exp,skip,proceed,pos)
    class(page_ring_type),intent(inout)::this
    character(*),optional,intent(in)::exp
    integer,intent(in)::skip
    logical,intent(in)::proceed
    integer(kind=dik),dimension(2),intent(out)::pos
    call page_ring_find(this,exp,this%position_stack%position,this%eof_pos,skip,proceed,pos)
  end subroutine page_ring_find_default

  recursive subroutine page_ring_find(this,exp,start,limit,skip,proceed,pos)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(2),intent(in)::start
    integer(kind=dik),dimension(2),intent(in)::limit
    character(*),intent(in)::exp
    integer,intent(in)::skip
    logical,intent(in)::proceed
    integer(kind=dik),dimension(2),intent(out)::pos
    integer(kind=dik)::page,page2,ind
    page=this%ring_index(start(1))
    if(limit(1)==start(1))then
       ind=index(this%ring(page)(start(2):limit(2)),exp)
       if(ind>0)then
          select case (skip)
          case(1)
             pos=[start(1),start(2)+ind-2]
             if(pos(2)==0)then
                pos(1)=pos(1)-1
                pos(2)=serialize_page_size
             end if
          case(2)
             pos=[start(1),start(2)+ind-1]
          case(3)
             pos=[start(1),start(2)+ind+len(exp)-2]
          case(4)
             pos=[start(1),start(2)+ind+len(exp)-1]
             if(pos(1)==this%last_page())call this%read_page()
             if(pos(2)>serialize_page_size)then
                pos(1)=pos(1)+1
                pos(2)=pos(2)-serialize_page_size
             end if
          end select
          if(proceed)call this%set_position(pos)
       else
          print *,"page_ring_find: limit reached."
          pos=[-1,-1]
       end if
    else
       ind=index(this%ring(page)(start(2):),exp)
       if(ind>0)then
          select case (skip)
          case(1)
             pos=[start(1),start(2)+ind-2]
             if(pos(2)==0)then
                pos(1)=pos(1)-1
                pos(2)=serialize_page_size
             end if
          case(2)
             pos=[start(1),start(2)+ind-1]
          case(3)
             pos=[start(1),start(2)+ind+len(exp)-2]
          case(4)
             pos=[start(1),start(2)+ind+len(exp)-1]
             if(pos(1)==this%last_page())call this%read_page()
             if(pos(2)>serialize_page_size)then
                pos(1)=pos(1)+1
                pos(2)=one
             end if
          end select
          if(proceed)call this%set_position(pos)
       else
          if(start(1)+1>this%active_pages(2))then
             call this%read_page()
             page=this%ring_index(start(1))
          end if
          page2=this%ring_index(start(1)+1)
          ind=index(this%ring(page)(serialize_page_size-len(exp)+1:)//this%ring(page2)(:len(exp)),exp)
          if(ind>0)then
             select case (skip)
             case(1)
                pos=[start(1),serialize_page_size-len(exp)+ind-1]
             case(2)
                pos=[start(1),serialize_page_size-len(exp)+ind]
             case(3)
                pos=[start(1)+1,ind-1]
             case(4)
                pos=[start(1)+1,ind]
             end select
             if(pos(2)>serialize_page_size)then
                pos(1)=pos(1)+1
                pos(2)=pos(2)-serialize_page_size
             else
                if(pos(2)<0)then
                   pos(1)=pos(1)-1
                   pos(2)=pos(2)+serialize_page_size
                end if
             end if
             if(proceed)call this%set_position(pos)
          else
             if(proceed)this%active_pages(1)=this%active_pages(2)
             call page_ring_find(this,exp,[start(1)+one,one],limit,skip,proceed,pos)
          end if
       end if
    end if
  end subroutine page_ring_find

  pure logical function page_ring_str_equal(this,string,pos)
    class(page_ring_type),intent(in)::this
    character(*),intent(in)::string
    integer(kind=dik),dimension(2,2),intent(in)::pos
    page_ring_str_equal=string==this%substring(pos)
  end function page_ring_str_equal

  pure recursive function page_ring_find_pure(this,exp,start,limit,skip) result(pos)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2),intent(in)::start
    integer(kind=dik),dimension(2),intent(in)::limit
    character(*),intent(in)::exp
    integer,optional,intent(in)::skip
    integer(kind=dik),dimension(2)::pos
    integer(kind=dik)::page,page2,ind,actual_skip
    ! Is the starting point before limit?
    if(start(1)<=limit(1))then
       ! Default skip is what you expect from the build-in index function
       if(present(skip))then
          actual_skip=skip
       else
          actual_skip=2
       end if
       page=mod(start(1),this%ring_size)
       ! Does the scanning region end on the page?
       if(start(1)==limit(1))then
          ind=index(this%ring(page)(start(2):limit(2)),exp)
       else
          ind=index(this%ring(page)(start(2):),exp)
       end if
       if(ind>0)then
          ! substring found on first page
          select case (actual_skip)
          case(1)
             pos=[start(1),start(2)+ind-2]
             if(pos(2)==0)then
                pos(1)=pos(1)-1
                pos(2)=serialize_page_size
             end if
          case(2)
             pos=[start(1),start(2)+ind-1]
          case(3)
             pos=[start(1),start(2)+ind+len(exp)-2]
          case(4)
             pos=[start(1),start(2)+ind+len(exp)-1]
             if(pos(2)>serialize_page_size)then
                pos(1)=pos(1)+1
                pos(2)=pos(2)-serialize_page_size
             end if
          end select
       else
          ! Substring not found on first page. Is the next page already read?
          if((start(1)>=limit(1)).or.(start(1)+1>this%active_pages(2)))then
             ! Either the limit is reached or the next page is not ready.
             pos=[0,0]
          else
             ! The next page is available.
             page2=mod(start(1)+1,this%ring_size)
             ! We concatenate the edges. When l is the length of exp, then we want to concat
             ! the l-1 last characters of page one and the first l characters of page two.
             !print *,"overlap: |",this%ring(page)(serialize_page_size-len(exp)+2:)//this%ring(page2)(:len(exp)),"|"
             ind=index(this%ring(page)(serialize_page_size-len(exp)+2:)//this%ring(page2)(:len(exp)),exp)
             if(ind>0)then
                select case (actual_skip)
                case(1)
                   pos=[start(1),serialize_page_size-len(exp)+ind]
                case(2)
                   pos=[start(1),serialize_page_size-len(exp)+ind+1]
                case(3)
                   pos=[start(1)+1,ind]
                case(4)
                   pos=[start(1)+1,ind+1]
                end select
             else
                ! EXP is not found in the overlap region. We recursively search the next pages.
                pos=page_ring_find_pure(this,exp,[start(one)+one,one],limit,skip)
             end if
          end if
       end if
    else
       ! limit is before start
       pos=[0,0]
    end if
  end function page_ring_find_pure

  pure recursive subroutine page_ring_positions_by_keys(this,exp1,exp2,start,limit,inclusive,length,pos)
    class(page_ring_type),intent(in)::this
    character(*),intent(in)::exp1,exp2
    integer(kind=dik),dimension(2),intent(in)::start,limit
    logical,optional,intent(in)::inclusive
    integer(kind=dik),intent(out),optional::length
    integer(kind=dik),dimension(2,2),intent(out)::pos
    if(inclusive)then
       pos(1:2,1)=this%find_pure(exp1,start,limit,2)
    else
       pos(1:2,1)=this%find_pure(exp1,start,limit,4)
    end if
    !print *,pos1
    if(present(length))then
       length=0
    end if
    if(pos(2,1)>0)then
       if(inclusive)then
          pos(1:2,2)=this%find_pure(exp2,pos(1:2,1),limit,3)
       else
          pos(1:2,2)=this%find_pure(exp2,pos(1:2,1),limit,1)
       end if
       !print *,pos2
       if(pos(2,2)>0)then
          if(present(length))then
             length=ring_position_metric1(pos)
          end if
       end if
    end if
  end subroutine page_ring_positions_by_keys

  pure recursive subroutine page_ring_character_by_keys(this,exp1,exp2,start,limit,inclusive,length,string)
    class(page_ring_type),intent(in)::this
    character(*),intent(in)::exp1,exp2
    integer(kind=dik),dimension(2),intent(in)::start,limit
    logical,optional,intent(in)::inclusive
    integer(kind=dik),intent(out),optional::length
    character(:),allocatable,intent(out)::string
    integer(kind=dik),dimension(2,2)::pos
    call this%substring_by_keys(exp1,exp2,start,limit,inclusive,length,pos)
    string=this%substring(pos(:,1),pos(:,2))
  end subroutine page_ring_character_by_keys

  subroutine page_ring_pop_by_keys(this,start,stop,inclusive,res)
    class(page_ring_type),intent(inout)::this
    character(*),intent(in),optional::start
    character(*),intent(in)::stop
    logical,optional,intent(in)::inclusive
    character(len=*),intent(out)::res
    integer(kind=dik),dimension(2)::i1,i2
    if(inclusive)then
       call this%find(start,2,.true.,i1)
       call this%find(stop,3,.false.,i2)
    else
       call this%find(start,4,.true.,i1)
       call this%find(stop,1,.false.,i2)
    end if
    res=this%substring(i1,i2)
    call this%set_position(i2)
  end subroutine page_ring_pop_by_keys

  elemental function  page_ring_get_character(this)
    class(page_ring_type),intent(in)::this
    character::page_ring_get_character
    page_ring_get_character=this%ring(this%actual_index())(this%actual_offset():this%actual_offset())
  end function page_ring_get_character

  subroutine page_ring_break(this)
    class(page_ring_type),intent(inout)::this
    if(this%actual_page()>=this%active_pages(2))call this%activate_next_page()
    call this%turn_page()
  end subroutine page_ring_break

  subroutine page_ring_turn_page(this)
    class(page_ring_type),intent(inout)::this
    this%position_stack%position(1)=this%position_stack%position(1)+1
    this%position_stack%position(2)=1
  end subroutine page_ring_turn_page

  subroutine page_ring_flush(this)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik)::page
    do while(this%active_pages(1)<this%actual_page())
       if(this%asynchronous)then
          write(this%unit,asynchronous="yes")this%ring(mod(this%active_pages(1),this%ring_size))
       else
          write(this%unit,asynchronous="no")this%ring(mod(this%active_pages(1),this%ring_size))
       end if
       this%active_pages(1)=this%active_pages(1)+1
    end do
  end subroutine page_ring_flush

  subroutine page_ring_activate_next_page(this)
    class(page_ring_type),intent(inout)::this
    if(this%active_pages(2)-this%active_pages(1)+1>=this%ring_size)call this%enlarge
    this%active_pages(2)=this%active_pages(2)+1
  end subroutine page_ring_activate_next_page

  subroutine page_ring_set_position(this,pos)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(2),intent(in)::pos
    this%position_stack%position=pos
  end subroutine page_ring_set_position

  subroutine page_ring_put(this)
    class(page_ring_type),intent(inout)::this
  end subroutine page_ring_put

  subroutine page_ring_proceed(this,n,deactivate)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),intent(in)::n
    logical,intent(in),optional::deactivate
    integer(kind=dik)::offset
    offset=this%position_stack%position(2)+n
    do while (offset>serialize_page_size)
       if(this%position_stack%position(1)>=this%active_pages(2))call this%activate_next_page()
       this%position_stack%position(1)=this%position_stack%position(1)+1
       offset=offset-serialize_page_size
    end do
    this%position_stack%position(2)=offset
    if(present(deactivate))then
       if(deactivate)this%active_pages(1)=this%actual_page()
    end if
  end subroutine page_ring_proceed

  subroutine page_ring_print_position(this)
    class(page_ring_type),intent(inout)::this
    print *,&
         this%actual_position(),&
         this%ring(this%actual_index())(:this%actual_offset()-1),&
         "|",&
         this%ring(this%actual_index())(this%actual_offset():)
  end subroutine page_ring_print_position
  
  elemental integer(kind=dik) function page_ring_ring_index(this,n)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),intent(in)::n
    page_ring_ring_index=mod(n,this%ring_size)
  end function page_ring_ring_index

  subroutine page_ring_ring_push_given_position(this,pos)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(2),intent(in)::pos
    call this%position_stack%push(pos)
  end subroutine page_ring_ring_push_given_position

  subroutine page_ring_ring_pop_actual_position(this)
    class(page_ring_type),intent(inout)::this
    call this%position_stack%pop()
  end subroutine page_ring_ring_pop_actual_position

  subroutine page_ring_ring_push_actual_position(this)
    class(page_ring_type),intent(inout)::this
    call this%position_stack%push()
  end subroutine page_ring_ring_push_actual_position

  subroutine page_ring_ring_pop_given_position(this,pos)
    class(page_ring_type),intent(inout)::this
    integer(kind=dik),dimension(2),intent(out)::pos
    call this%position_stack%pop(pos)
  end subroutine page_ring_ring_pop_given_position

  pure subroutine page_ring_get_position1(this,pos)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),intent(out)::pos
    pos=page_ring_ordinal(this%position_stack%position)
  end subroutine page_ring_get_position1

  pure subroutine page_ring_get_position2(this,pos)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2),intent(out)::pos
    pos=this%position_stack%position
  end subroutine page_ring_get_position2

  elemental integer(kind=dik) function page_ring_actual_index(this)
    class(page_ring_type),intent(in)::this
    page_ring_actual_index=mod(this%position_stack%position(1),this%ring_size)
  end function page_ring_actual_index

  elemental integer(kind=dik) function page_ring_actual_page(this)
    class(page_ring_type),intent(in)::this
    page_ring_actual_page=this%position_stack%position(1)
  end function page_ring_actual_page

  elemental integer(kind=dik) function page_ring_actual_offset(this)
    class(page_ring_type),intent(in)::this
    page_ring_actual_offset=this%position_stack%position(2)
  end function page_ring_actual_offset

  pure function page_ring_actual_position(this)
    class(page_ring_type),intent(in)::this
    integer(kind=dik),dimension(2)::page_ring_actual_position
    page_ring_actual_position=this%position_stack%position
  end function page_ring_actual_position
 
  elemental integer(kind=dik) function page_ring_first_index(this)
    class(page_ring_type),intent(in)::this
    page_ring_first_index=mod(this%active_pages(1),this%ring_size)
  end function page_ring_first_index

  elemental integer(kind=dik) function page_ring_first_page(this)
    class(page_ring_type),intent(in)::this
    page_ring_first_page=this%active_pages(1)
  end function page_ring_first_page

  elemental integer(kind=dik) function page_ring_last_index(this)
    class(page_ring_type),intent(in)::this
    page_ring_last_index=mod(this%active_pages(2),this%ring_size)
  end function page_ring_last_index

  elemental integer(kind=dik) function page_ring_last_page(this)
    class(page_ring_type),intent(in)::this
    page_ring_last_page=this%active_pages(2)
  end function page_ring_last_page
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for marker_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine marker_mark_begin(this,tag,type,name,target,pointer,shape)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::tag
    character(*),intent(in),optional::type,name
    integer(kind=dik),intent(in),optional::target,pointer
    integer,intent(in),dimension(:),optional::shape
    call this%indent()
    call this%push("<")
    call this%push(tag)
    if(present(type))call this%push(' type="'//type//'"')
    if(present(name))call this%push(' name="'//name//'"')
    if(present(target))then
       call this%push(' target="')
       call this%push(target)
       call this%push('"')
    end if
    if(present(pointer))then
       call this%push(' pointer="')
       call this%push(pointer)
       call this%push('"')
    end if
    if(present(shape))then
       call this%push(' shape="')
       call this%push(shape)
       call this%push('"')
    end if
    call this%push(">")
    this%indentation=this%indentation+1
  end subroutine marker_mark_begin

  subroutine marker_mark_instance_begin(this,ser,name,target,pointer,shape)
    class(marker_type),intent(inout)::this
    class(serializable_class),intent(in)::ser
    character(*),intent(in)::name
    integer(kind=dik),intent(in),optional::target,pointer
    integer,intent(in),dimension(:),optional::shape
    character(:),allocatable::this_type
    call ser%get_type(this_type)
    call this%mark_begin("ser",this_type,name,target,pointer,shape)
  end subroutine marker_mark_instance_begin

  subroutine marker_mark_end(this,tag)
    class(marker_type),intent(inout)::this
    character(*),intent(in),optional::tag
    this%indentation=this%indentation-1
    call this%indent()
    if(present(tag))then
       call this%push("</"//tag//">")
    else
       call this%push("</ser>")
    end if
  end subroutine marker_mark_end

  subroutine marker_mark_instance_end(this)
    class(marker_type),intent(inout)::this
    call this%mark_end("ser")
  end subroutine marker_mark_instance_end

  subroutine marker_mark_logical(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    logical,intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    if(content)then
       call this%push("T")
    else
       call this%push("F")
    end if
    call this%push("</"//name//">")
  end subroutine marker_mark_logical

  subroutine marker_mark_integer(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_integer

  subroutine marker_mark_integer_array(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,dimension(:),intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_integer_array

  subroutine marker_mark_integer_matrix(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,dimension(:,:),intent(in)::content
    integer::n
    integer,dimension(2)::s
    s=shape(content)
    call this%indent()
    call this%push("<"//name//">")
    do n=1,s(2)
       call this%push(content(:,n))
       call this%push(" ")
    end do
    call this%push("</"//name//">")
  end subroutine marker_mark_integer_matrix

  subroutine marker_mark_integer_dik(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_integer_dik

  subroutine marker_mark_integer_array_dik(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),dimension(:),intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_integer_array_dik

  subroutine marker_mark_integer_matrix_dik(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),dimension(:,:),intent(in)::content
    integer::n
    integer,dimension(2)::s
    call this%indent()
    call this%push("<"//name//">")
    do n=1,s(2)
       call this%push(content(:,n))
       call this%push(" ")
    end do
    call this%push("</"//name//">")
  end subroutine marker_mark_integer_matrix_dik

  subroutine marker_mark_double(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_double

  subroutine marker_mark_double_array(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),dimension(:),intent(in)::content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_double_array

  subroutine marker_mark_double_matrix(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),dimension(:,:),intent(in)::content
    integer::n
    integer,dimension(2)::s
    s=shape(content)
    call this%indent()
    call this%push("<"//name//">")
    do n=1,s(2)
       call this%push(content(:,n))
       call this%push(" ")
    end do
    call this%push("</"//name//">")
  end subroutine marker_mark_double_matrix

  subroutine marker_mark_string(this,name,content)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name,content
    call this%indent()
    call this%push("<"//name//">")
    call this%push(content)
    call this%push("</"//name//">")
  end subroutine marker_mark_string

  recursive subroutine marker_mark_instance(this,ser,name,target,pointer)
    class(marker_type),intent(inout)::this
    class(serializable_class),intent(in)::ser
    character (len=*), intent(in)::name
    integer(kind=dik),intent(in),optional::target,pointer
    integer(kind=dik)::status
    call this%mark_instance_begin(ser,name,target,pointer)
    call ser%write_to_marker(this,status)
    call this%mark_end("ser")
  end subroutine marker_mark_instance

  recursive subroutine marker_mark_target(this,name,ser)
    class(marker_type),intent(inout)::this
    class(serializable_class),target,intent(in)::ser
    character (len=*), intent(in)::name
    this%n_instances=this%n_instances+1
    call this%push_heap(ser,this%n_instances)
    call this%mark_instance(ser,name,target=this%n_instances)
  end subroutine marker_mark_target

  subroutine marker_mark_allocatable(this,name,ser)
    class(marker_type),intent(inout)::this
    class(serializable_class),allocatable,intent(in)::ser
    character (len=*), intent(in)::name
    if(allocated(ser))then
       call this%mark_instance(ser,name)
    else
       call this%mark_null(name)
    end if
  end subroutine marker_mark_allocatable

  recursive subroutine marker_mark_pointer(this,name,ser)
    class(marker_type),intent(inout)::this
    class(serializable_class),pointer,intent(in)::ser
    character(len=*),intent(in)::name
    character(:),allocatable::type
    integer(kind=dik)::p
    if(associated(ser))then
       call this%search_heap(ser,p)
       if(p>0)then
          call ser%get_type(type)
          call this%push('<ser type="')
          call this%push(type)
          call this%push('" name="')
          call this%push(name)
          call this%push('" pointer="')
          call this%push(p)
          call this%push('"/>')
       else
          call this%mark_target(name,ser)
       end if
    else
       call this%mark_null(name)
    end if
  end subroutine marker_mark_pointer

  subroutine marker_mark_null(this,name)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    call this%indent()
    call this%push('<ser type="null" name="')
    call this%push(name)
    call this%push('"/>')
  end subroutine marker_mark_null

  subroutine marker_mark_nothing(this,name)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    call this%indent()
    call this%push('<')
    call this%push(name)
    call this%push('/>')
  end subroutine marker_mark_nothing

  subroutine marker_mark_empty(this,tag,type,name,target,pointer,shape)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::tag
    character(*),intent(in),optional::type,name
    integer(kind=dik),intent(in),optional::target,pointer
    integer,intent(in),dimension(:),optional::shape
    call this%push("<")
    call this%push(tag)
    if(present(type))call this%push(' type="'//type//'"')
    if(present(name))call this%push(' name="'//name//'"')
    if(present(target))then
       call this%push(' target="')
       call this%push(target)
       call this%push('"')
    end if
    if(present(pointer))then
       call this%push(' pointer="')
       call this%push(pointer)
       call this%push('"')
    end if
    if(present(shape))then
       call this%push(' shape="')
       call this%push(shape)
       call this%push('"')
    end if
    call this%push("/>")
  end subroutine marker_mark_empty

  subroutine marker_pick_begin(this,tag,type,name,target,pointer,shape,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::tag
    integer(kind=dik),dimension(2,2),intent(out),optional::type,name
    integer(kind=dik),intent(out),optional::target,pointer
    integer,dimension(:),allocatable,optional,intent(out)::shape
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2)::p1,p2,p3
    integer(kind=dik)::l
    call this%find("<",skip=4,proceed=.true.,pos=p1)
    call this%find(">",skip=1,proceed=.false.,pos=p2)
    p3=this%find_pure(" ",p1,p2,skip=1)
    if(p3(2)>0)then
       if(this%substring(p1,p3)==tag)then
          status=serialize_ok
          if(present(type))then
             call this%substring_by_keys('type="','"',p3,p2,.false.,l,type)
             if(l<=0)then
                print *,"marker_pick_begin: No type found"
                status=serialize_wrong_type
             end if
          end if
          if(present(name))then
             call this%substring_by_keys('name="','"',p3,p2,.false.,l,name)
             if(l<=0)then
                print *,"marker_pick_begin: No name found"
                status=serialize_wrong_name
                call this%print_position()
                stop
             end if
          end if
          if(present(target))then
             p1=this%find_pure('target="',p3,p2,4)
             if(p1(2)>0)then
                call this%set_position(p1)
                call this%pop(target)
             else
                target=-1
                status=serialize_ok
             end if
          end if
          if(present(pointer))then
             p1=this%find_pure('pointer="',p3,p2,4)
             if(p1(2)>0)then
                call this%set_position(p1)
                call this%pop(pointer)
             else
                pointer=-1
                status=serialize_ok
             end if
          end if
          if(present(shape))then
             p1=this%find_pure('shape="',p3,p2,4)
             if(p1(2)>0)then
                call this%set_position(p1)
                call this%pop(shape)
             else
                status=serialize_ok
             end if
          end if
       else
          print *,"marker_pick_begin: Wrong tag. Expected: ",tag," Found: ",this%substring(p1,p3)
          status=serialize_wrong_tag
          call this%print_position()
       end if
    else
       if(this%substring(p1,p2)==tag)then
          status=serialize_ok
       else
          print *,"marker_pick_begin: Wrong tag. Expected: ",tag," Found: ",this%substring(p1,p2)
          status=serialize_wrong_tag
       end if
    end if
    call this%set_position(p2)
    call this%proceed(one*2,.true.)
  end subroutine marker_pick_begin

  subroutine marker_query_instance_begin(this,type,name,target,pointer,shape,status)
    class(marker_type),intent(inout)::this
    integer(kind=dik),dimension(2,2),intent(out),optional::type,name
    integer(kind=dik),intent(out),optional::target,pointer
    integer,dimension(:),allocatable,optional,intent(out)::shape
    integer(kind=dik),intent(out)::status
    call this%pick_begin("ser",type,name,target,pointer,shape,status)
  end subroutine marker_query_instance_begin

  subroutine marker_pick_instance_begin(this,name,type,target,pointer,shape,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),dimension(2,2),intent(out),optional::type
    integer(kind=dik),intent(out),optional::target,pointer
    integer,dimension(:),allocatable,optional,intent(out)::shape
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2,2)::read_name
    call this%query_instance_begin(type,read_name,target,pointer,shape,status)
    if(status==serialize_ok)then
       if(.not.this%str_equal(name,read_name))status=serialize_wrong_name
    end if
  end subroutine marker_pick_instance_begin

  subroutine marker_pick_end(this,tag,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::tag
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2)::p1,p2
    call this%find("</",skip=4,proceed=.true.,pos=p1)
    call this%find(">",skip=1,proceed=.false.,pos=p2)
    if(tag==this%substring(p1,p2))then
       status=serialize_ok
    else
       print *,"marker_pick_end: Wrong tag. Expected: ",tag," Found: ",this%substring(p1,p2)
       print *,"p1=",p1,"p2=",p2
       call this%print_position()
    end if
    call this%set_position(p2)
    call this%proceed(one*2,.true.)
  end subroutine marker_pick_end

  subroutine marker_pick_instance_end(this,status)
    class(marker_type),intent(inout)::this
    integer(kind=dik),intent(out)::status
    call this%pick_end("ser",status)
  end subroutine marker_pick_instance_end

  subroutine marker_pick_instance(this,name,ser,status)
    class(marker_type),intent(inout)::this
    class(serializable_class),intent(out)::ser
    character(*),intent(in)::name
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2,2)::type,r_name
    call this%pick_begin("ser",type,r_name,status=status)
    if(status==serialize_ok)then
       if(ser%verify_type(this%substring(type)))then
          if(this%str_equal(name,r_name))then
             call ser%read_from_marker(this,status)
             call this%pick_end("ser",status)
          else
             print *,"marker_pick_instance: Name mismatch: Expected: ",name," Found: ",r_name
             status=serialize_wrong_name
             call this%print_position
          end if
       else
          print *,"marker_pick_instance: Type mismatch: ",type
          call ser%write_type(output_unit)
          print *,""
          status=serialize_wrong_type
          call this%print_position
       end if
    end if
  end subroutine marker_pick_instance

  subroutine marker_pick_target(this,name,ser,status)
    class(marker_type),intent(inout)::this
    class(serializable_class),target,intent(out)::ser
    character(*),intent(in)::name
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2,2)::type,r_name
    integer(kind=dik)::target
    call this%pick_begin("ser",type,r_name,target,status=status)
    if(status==serialize_ok)then
       if(ser%verify_type(this%substring(type)))then
          if(this%str_equal(name,r_name))then
             call ser%read_target_from_marker(this,status)
             if(target>0)call this%push_heap(ser,target)
          else
             print *,"marker_pick_instance: Name mismatch: Expected: ",name," Found: ",r_name
             status=serialize_wrong_name
          end if
       else
          print *,"marker_pick_instance: Type mismatch: ",type
          status=serialize_wrong_type
       end if
    end if
    call this%pick_end("ser",status)
  end subroutine marker_pick_target

  subroutine marker_pick_allocatable(this,name,ser)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    class(serializable_class),allocatable,intent(out)::ser
    class(serializable_class),pointer::ref
    integer(kind=dik),dimension(2,2)::type,r_name
    integer(kind=dik)::status
    call this%pick_begin("ser",type,r_name,status=status)
    if(status==serialize_ok)then
       if(ser%verify_type(this%substring(type)))then
          if(this%str_equal(name,r_name))then
             call this%search_reference(type,ref)
             if(associated(ref))then
                allocate(ser,source=ref)
                call ser%read_from_marker(this,status)
             else
                print *,"marker_pick_allocatable:&
                     & Type ",type," not found on reference stack."
             end if
          else
             print *,"marker_pick_instance: Name mismatch: Expected: ",name," Found: ",r_name
             status=serialize_wrong_name
          end if
       else
          print *,"marker_pick_instance: Type mismatch: ",type
          status=serialize_wrong_type
       end if
    end if
    call this%pick_end("ser",status)
  end subroutine marker_pick_allocatable

  recursive subroutine marker_pick_pointer(this,name,ser)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    class(serializable_class),pointer,intent(out)::ser
    class(serializable_class),pointer::ref
    integer(kind=dik),dimension(2,2)::type,r_name
    integer(kind=dik)::status,t,p
    nullify(ser)
    call this%pick_begin("ser",type,r_name,target=t,pointer=p,status=status)
    if(status==serialize_ok)then
       if(.not.this%str_equal("null",type))then
          if(p>0)then
             call this%search_heap(p,ser)
          else
             call this%search_reference(type,ref)
             if(associated(ref))then
                allocate(ser,source=ref)
                call ser%read_target_from_marker(this,status)
                call this%pick_end("ser",status)
                if(t>0)call this%push_heap(ser,t)
             else
                print *,"marker_pick_pointer:&
                     & Type ",type," not found on reference stack."
             end if
          end if
       end if
    end if
  end subroutine marker_pick_pointer

  subroutine marker_pick_logical(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    logical,intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_logical

  subroutine marker_pick_integer(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer

  subroutine marker_pick_integer_array(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,dimension(:),intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer_array

  subroutine marker_pick_integer_matrix(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer,dimension(:,:),intent(out)::content
    integer(kind=dik),intent(out)::status
    integer::n
    integer,dimension(2)::s
    s=shape(content)
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       do n=1,s(2)
          call this%pop(content(:,n))
       end do
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer_matrix

  subroutine marker_pick_integer_dik(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer_dik

  subroutine marker_pick_integer_array_dik(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),dimension(:),intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer_array_dik

  subroutine marker_pick_integer_matrix_dik(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),dimension(:,:),intent(out)::content
    integer(kind=dik),intent(out)::status
    integer::n
    integer,dimension(2)::s
    s=shape(content)
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       do n=1,s(2)
          call this%pop(content(:,n))
       end do
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_integer_matrix_dik

  subroutine marker_pick_double(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_double

  subroutine marker_pick_double_array(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),dimension(:),intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_double_array

  subroutine marker_pick_double_matrix(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    real(kind=drk),dimension(:,:),intent(out)::content
    integer(kind=dik),intent(out)::status
    integer::n
    integer,dimension(2)::s
    s=shape(content)
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       do n=1,s(2)
          call this%pop(content(:,n))
       end do
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_double_matrix

  subroutine marker_pick_string(this,name,content,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    character(:),allocatable,intent(out)::content
    integer(kind=dik),intent(out)::status
    call this%pick_begin(name,status=status)
    if(status==serialize_ok)then
       call this%pop(content)
       call this%pick_end(name,status)
    end if
  end subroutine marker_pick_string
  
  subroutine marker_verify_nothing(this,name,status)
    class(marker_type),intent(inout)::this
    character(*),intent(in)::name
    integer(kind=dik),intent(out)::status
    integer(kind=dik),dimension(2)::p1,p2
    call this%find("<",skip=4,proceed=.false.,pos=p1)
    call this%find(">",1,.false.,p2)
    if(name//"/"==this%substring(p1,p2))then
       status=serialize_nothing
       call this%set_position(p2)
       call this%proceed(one*3,.true.)
    else
       if(name==this%substring(p1,p2))then
          status=serialize_ok
       else
          status=serialize_wrong_tag
       end if
    end if
  end subroutine marker_verify_nothing

  subroutine marker_indent(this,step)
    class(marker_type),intent(inout)::this
    integer(kind=dik),optional::step
    if(this%do_break)call this%push(new_line(" "))
    if(this%do_indent)then
       if(present(step))this%indentation=this%indentation+step
       call this%push(repeat(" ",this%indentation))
    end if
    this%active_pages(1)=this%actual_page()
  end subroutine marker_indent

  subroutine marker_push_heap(this,ser,id)
    class(marker_type),intent(inout)::this
    class(serializable_class),target,intent(in)::ser
    integer(kind=dik),intent(in)::id
    class(serializable_ref_type),pointer::new_ref
    allocate(new_ref)
    new_ref%next=>this%heap
    new_ref%ref=>ser
    new_ref%id=id
    this%heap=>new_ref
  end subroutine marker_push_heap
  
  subroutine marker_pop_heap(this,ser)
    class(marker_type),intent(inout)::this
    class(serializable_class),pointer,intent(out)::ser
    class(serializable_ref_type),pointer::old_ref
    if(associated(this%heap))then
       old_ref=>this%heap
       ser=>old_ref%ref
       this%heap=>this%heap%next
       deallocate(old_ref)
    else
       print('("marker_pop_heap: heap_stack is not associated.")')
    end if
  end subroutine marker_pop_heap

  subroutine marker_search_heap_by_id(this,id,ser)
    class(marker_type),intent(in)::this
    integer(kind=dik),intent(in)::id
    class(serializable_class),pointer,intent(out)::ser
    class(serializable_ref_type),pointer::ref
    ref=>this%heap
    do while(associated(ref))
       if(id==ref%id)then
          ser=>ref%ref
          exit
       end if
       ref=>ref%next
    end do
  end subroutine marker_search_heap_by_id

  subroutine marker_search_heap_by_ref(this,ref,id)
    class(marker_type),intent(in)::this
    class(serializable_class),pointer,intent(in)::ref
    integer(kind=dik),intent(out)::id
    class(serializable_ref_type),pointer::ref_p
    ref_p=>this%heap
    id=0
    do while(associated(ref_p))
       if(associated(ref,ref_p%ref))then
          id=ref_p%id
          exit
       end if
       ref_p=>ref_p%next
    end do
  end subroutine marker_search_heap_by_ref

  subroutine marker_push_reference(this,ser,id)
    class(marker_type),intent(inout)::this
    class(serializable_class),target,intent(in)::ser
    integer(kind=dik),intent(in),optional::id
    class(serializable_ref_type),pointer::new_ref
    allocate(new_ref)
    new_ref%next=>this%references
    new_ref%ref=>ser
    if(present(id))then
       new_ref%id=id
    else
       new_ref%id=-1
    end if
    this%references=>new_ref
  end subroutine marker_push_reference
  
  subroutine marker_pop_reference(this,ser)
    class(marker_type),intent(inout)::this
    class(serializable_class),pointer,intent(out)::ser
    class(serializable_ref_type),pointer::old_ref
    if(associated(this%references))then
       old_ref=>this%references
       ser=>old_ref%ref
       this%references=>this%references%next
       deallocate(old_ref)
    else
       print('("marker_pop_reference: reference_stack is not associated.")')
    end if
  end subroutine marker_pop_reference

  subroutine marker_search_reference(this,type,ser)
    class(marker_type),intent(in)::this
    integer(kind=dik),dimension(2,2),intent(in)::type
    class(serializable_class),pointer,intent(out)::ser
    class(serializable_class),pointer::tmp_ser!nag bug workaround
    class(serializable_ref_type),pointer::ref
    ref=>this%references
    nullify(ser)
    do while(associated(ref))
       tmp_ser=>ref%ref
       if(tmp_ser%verify_type(this%substring(type)))then
          ser=>tmp_ser
          exit
       end if
       ref=>ref%next
    end do
  end subroutine marker_search_reference

  subroutine marker_reset_heap(this)
    class(marker_type),intent(inout)::this
    if(associated(this%heap))then
       call this%heap%finalize()
       deallocate(this%heap)
    end if
  end subroutine marker_reset_heap

  subroutine marker_reset_references(this)
    class(marker_type),intent(inout)::this
    if(associated(this%references))then
       call this%references%finalize()
       deallocate(this%references)
    end if
  end subroutine marker_reset_references

  subroutine marker_finalize(this)
    class(marker_type),intent(inout)::this
    call this%reset_heap()
    call this%reset_references()
  end subroutine marker_finalize

!!$  subroutine serialize_remove_reference(ser)
!!$    class(serializable_class),target,intent(in)::ser
!!$    class(serializable_ref_type),pointer::ref1,ref2
!!$    if(associated(reference_stack))then
!!$       ref1=>reference_stack
!!$       if(associated(reference_stack%ref,ser))then
!!$          reference_stack=>reference_stack%next
!!$          deallocate(ref1)
!!$       else
!!$          do while (associated(ref1%next))
!!$             ref2=>ref1%next
!!$             if(associated(ref2%ref,ser))then
!!$                ref1%next=>ref2%next
!!$                deallocate(ref2)
!!$             else
!!$                ref2=>ref2%next
!!$                ref1=>ref1%next
!!$             end if
!!$          end do
!!$       end if
!!$    end if
!!$  end subroutine serialize_remove_reference

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! non type bound procedures !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  subroutine xml_verify_null_instance(ring,name,status)
!!$    class(page_ring_type),intent(inout)::ring
!!$    integer(kind=dik),intent(out)::status
!!$    character(len=*),intent(in)::name
!!$    character(:),allocatable::null
!!$    call xml_read_string(ring,null)
!!$    if(null=="NULL")then
!!$       status=serialize_null
!!$    else
!!$       status=serialize_ok
!!$    end if
!!$  end subroutine xml_verify_null_instance

  recursive subroutine serialize_print_comp_pointer(ser,unit,parents,components,peers,name)
    class(serializable_class),pointer,intent(in)::ser
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    character(len=*),intent(in)::name
    if(associated(ser))then
       write(unit,fmt=*)name," is associated."
       if(components>0)then
          write(unit,fmt=*)"Printing components of ",name
          call ser%print_to_unit(unit,parents,components-one,peers)
       else
          write(unit,fmt=*)"Skipping components of ",name
       end if
    else
       write(unit,fmt=*)name," is not associated."
    end if
  end subroutine serialize_print_comp_pointer

  recursive subroutine serialize_print_peer_pointer(ser,unit,parents,components,peers,name)
    class(serializable_class),pointer,intent(in)::ser
    integer,intent(in)::unit
    integer(kind=dik)::parents,components,peers
    character(len=*),intent(in)::name
    if(associated(ser))then
       write(unit,fmt=*)name," is associated."
       if(peers>0)then
          write(unit,fmt=*)"Printing components of ",name
          call ser%print_to_unit(unit,parents,components,peers-one)
       else
          write(unit,fmt=*)"Skipping components of ",name
       end if
    else
       write(unit,fmt=*)name," is not associated."
    end if
  end subroutine serialize_print_peer_pointer

  subroutine serialize_print_allocatable(ser,unit,parents,components,peers,name)
    class(serializable_class),allocatable,intent(in)::ser
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    character(len=*),intent(in)::name
    if(allocated(ser))then
       write(unit,fmt=*)name," is allocated."
       if(components>0)then
          write(unit,fmt=*)"Printing components of ",name
          call ser%print_to_unit(unit,parents,components-1,peers)
       else
          write(unit,fmt=*)"Skipping components of ",name
       end if
    else
       write(unit,fmt=*)name," is not allocated."
    end if
  end subroutine serialize_print_allocatable

  elemental function measurable_less_measurable(mea1,mea2)
    class(measurable_class),intent(in)::mea1,mea2
    logical::measurable_less_measurable
    measurable_less_measurable=mea1%measure()<mea2%measure()
  end function measurable_less_measurable
  
  elemental function measurable_less_double(mea1,dou)
    class(measurable_class),intent(in)::mea1
    real(kind=drk),intent(in)::dou
    logical::measurable_less_double
    measurable_less_double=mea1%measure()<dou
  end function measurable_less_double
  
  elemental function measurable_less_or_equal_measurable(mea1,mea2)
    class(measurable_class),intent(in)::mea1,mea2
    logical::measurable_less_or_equal_measurable
    measurable_less_or_equal_measurable=mea1%measure()<=mea2%measure()
  end function measurable_less_or_equal_measurable

   elemental function measurable_less_or_equal_double(mea1,dou)
    class(measurable_class),intent(in)::mea1
    real(kind=drk),intent(in)::dou
    logical::measurable_less_or_equal_double
    measurable_less_or_equal_double=mea1%measure()<=dou
  end function measurable_less_or_equal_double

 elemental function measurable_equal_measurable(mea1,mea2)
    class(measurable_class),intent(in)::mea1,mea2
    logical::measurable_equal_measurable
    measurable_equal_measurable=mea1%measure()==mea2%measure()
  end function measurable_equal_measurable

  elemental function measurable_equal_double(mea1,dou)
    class(measurable_class),intent(in)::mea1
    real(kind=drk),intent(in)::dou
    logical::measurable_equal_double
    measurable_equal_double=mea1%measure()==dou
  end function measurable_equal_double

  elemental function measurable_equal_or_greater_measurable(mea1,mea2)
    class(measurable_class),intent(in)::mea1,mea2
    logical::measurable_equal_or_greater_measurable
    measurable_equal_or_greater_measurable=mea1%measure()>=mea2%measure()
  end function measurable_equal_or_greater_measurable

  elemental function measurable_equal_or_greater_double(mea1,dou)
    class(measurable_class),intent(in)::mea1
    real(kind=drk),intent(in)::dou
    logical::measurable_equal_or_greater_double
    measurable_equal_or_greater_double=mea1%measure()>=dou
  end function measurable_equal_or_greater_double
  
  elemental function measurable_greater_measurable(mea1,mea2)
    class(measurable_class),intent(in)::mea1,mea2
    logical::measurable_greater_measurable
    measurable_greater_measurable=mea1%measure()>mea2%measure()
  end function measurable_greater_measurable

  elemental function measurable_greater_double(mea1,dou)
    class(measurable_class),intent(in)::mea1
    real(kind=drk),intent(in)::dou
    logical::measurable_greater_double
    measurable_greater_double=mea1%measure()>dou
  end function measurable_greater_double

  pure function page_ring_position(n)
    integer(kind=dik),intent(in)::n
    integer(kind=dik),dimension(2)::page_ring_position
    page_ring_position(2)=mod(n,serialize_page_size)
    page_ring_position(1)=(n-page_ring_position(2))/serialize_page_size
  end function page_ring_position

  pure integer(kind=dik) function page_ring_ordinal(pos)
    integer(kind=dik),dimension(2),intent(in)::pos
    page_ring_ordinal=pos(1)*serialize_page_size+pos(2)
  end function page_ring_ordinal

  pure logical function page_ring_position_is_before_int_pos(m,n)
    integer(kind=dik),intent(in)::m
    integer(kind=dik),dimension(2),intent(in)::n
    if(m<page_ring_ordinal(n))then
       page_ring_position_is_before_int_pos=.true.
    else
       page_ring_position_is_before_int_pos=.false.
    end if
  end function page_ring_position_is_before_int_pos

  pure logical function page_ring_position_is_before_pos_int(m,n)
    integer(kind=dik),dimension(2),intent(in)::m
    integer(kind=dik),intent(in)::n
    if(page_ring_ordinal(m)<n)then
       page_ring_position_is_before_pos_int=.true.
    else
       page_ring_position_is_before_pos_int=.false.
    end if
  end function page_ring_position_is_before_pos_int

  pure logical function page_ring_position_is_before_pos_pos(m,n)
    integer(kind=dik),dimension(2),intent(in)::m,n
    if(m(1)<n(1))then
       page_ring_position_is_before_pos_pos=.true.
    else
       if(m(1)>n(1))then
          page_ring_position_is_before_pos_pos=.false.
       else
          if(m(2)<n(2))then
             page_ring_position_is_before_pos_pos=.true.
          else
             page_ring_position_is_before_pos_pos=.false.
          end if
       end if
    end if
  end function page_ring_position_is_before_pos_pos

  subroutine ring_position_increase(pos,n)
    integer(kind=dik),dimension(2),intent(inout)::pos
    integer(kind=dik),intent(in)::n
    pos=page_ring_position(page_ring_ordinal(pos)+n)
  end subroutine ring_position_increase

  pure integer(kind=dik) function ring_position_metric2(p1,p2)
    integer(kind=dik),dimension(2),intent(in)::p1,p2
    ring_position_metric2=(p2(1)-p1(1))*serialize_page_size+p2(2)-p1(2)+1
  end function ring_position_metric2

  pure integer(kind=dik) function ring_position_metric1(p)
    integer(kind=dik),dimension(2,2),intent(in)::p
    ring_position_metric1=(p(1,2)-p(1,1))*serialize_page_size+p(2,2)-p(2,1)+1
  end function ring_position_metric1

  subroutine generate_unit(unit,min,max)
    integer,intent(out) :: unit
    integer,intent(in),optional :: min,max
    integer :: min_u,max_u
    logical :: is_open
    !print *,"generate_unit"
    unit = -1
    if(present(min))then
       min_u=min
    else
       min_u=10
    end if
    if(present(max))then
       max_u=max
    else
       max_u=huge(max_u)
    end if
    do unit=min_u,max_u
       !print *,"testing ",unit
       inquire(unit,opened=is_open)
       if (.not. is_open) then
          exit
       end if
    end do
  end subroutine generate_unit
  
  subroutine ilog2(int,exp,rem)
    integer,intent(in) :: int
    integer,intent(out) :: exp,rem
    integer :: count
    count = 2
    exp = 1
    do while (count<int)
       exp=exp+1
       count = ishft(count,1)
    end do
    if (count>int) then
       rem=(int-ishft(count,-1))
    else
       rem=0
    end if    
  end subroutine ilog2

  pure logical function character_is_in(c,array)
    character,intent(in)::c
    character,dimension(:),intent(in)::array
    integer(kind=dik)::n
    character_is_in=.false.
    do n=1,size(array)
       if(c==array(n))then
          character_is_in=.true.
          exit
       end if
    end do
  end function character_is_in

  subroutine integer_with_leading_zeros(number,length,string)
    integer,intent(in) :: number,length
    character(len=*),intent(out) :: string
    integer :: zeros
    character::sign
    if(number==0)then
       string = repeat("0",length)
    else
       if(number>0)then
          zeros=length-floor(log10(real(number)))-1
          if(zeros<0)then
             string=repeat("*",length)
          else
             write(string,fmt='(a,I0)') repeat("0",zeros),number
          end if
       else
          zeros=length-floor(log10(real(-number)))-2
          if(zeros<0)then
             string=repeat("*",length)
          else
             write(string,fmt='(a,a,I0)') "-",repeat("0",zeros),abs(number)
          end if
       end if
    end if
  end subroutine integer_with_leading_zeros

end module muli_basic
