!!! module: muli_trapezium
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
!!! Latest Change: 2011-03-10 15:07:31 CET(+0100)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_trapezium". The name is deriven from the
!!! trapezoidal integration rule. The purpose of this module is to define a
!!! binary tree "muli_trapezium_tree_type" which holds a probability function in
!!! terms of trapezoidal segments. Its leaves of type "muli_trapezium_list_type"
!!! are connected to form a list, so you can either walk the root function back
!!! and forth or pick a certain segment in logarithmic time by walking down the
!!! tree.
!!! All nodes extend "muli_trapezium_type" which holds the actual values. That
!!! is the values of the density function, the integral from this segment to the
!!! end of the integration area and a propability function calculated from these
!!! values. All values are taken at the upper "right" bound of the segment.
!!! Additionally the differences of these values to the values of the left
!!! neighbour is stored. 

module muli_trapezium
  use muli_basic
  use kinds !NODEP!
  implicit none

  !!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Module components !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!

  integer,private,parameter::value_dimension=7
  integer,private,parameter::r_value_index=1
  integer,private,parameter::d_value_index=2
  integer,private,parameter::r_integral_index=3
  integer,private,parameter::d_integral_index=4
  integer,private,parameter::r_propability_index=5
  integer,private,parameter::d_propability_index=6
  integer,private,parameter::error_index=7
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Definitions !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type,extends(measurable_class) :: muli_trapezium_type
     private
     integer::dim=0
     real(kind=double)::r_position=0D0
     real(kind=double)::d_position=0D0
     real(kind=double)::measure_comp=0D0
     real(kind=double),dimension(:,:),allocatable::values
     !first index is in {0 .. dim-1}
     !second index is in {r_value,d_value,r_integral,d_integral,r_propability,d_propability}
   contains
     ! overridden serializable_class procedures
     procedure :: write_to_marker => muli_trapezium_write_to_marker
     procedure :: read_from_marker => muli_trapezium_read_from_marker
     procedure :: print_to_unit => muli_trapezium_print_to_unit
     procedure,nopass :: get_type => muli_trapezium_get_type
     procedure,nopass :: verify_type => muli_trapezium_verify_type
     ! overridden measurable_class procedures
     procedure::measure=>muli_trapezium_measure
     ! init/deinit
     procedure::initialize=>muli_trapezium_initialize
     ! components    
     procedure,public::get_dimension      => muli_trapezium_get_dimension
     procedure,public::get_l_position     => muli_trapezium_get_l_position
     procedure,public::get_r_position     => muli_trapezium_get_r_position
     procedure,public::get_d_position     => muli_trapezium_get_d_position
     procedure,public::get_l_value_array  => muli_trapezium_get_l_value_array
     procedure,public::get_l_value_element=> muli_trapezium_get_l_value_element
     procedure,public::get_r_value_array  => muli_trapezium_get_r_value_array
     procedure,public::get_r_value_element=> muli_trapezium_get_r_value_element
     procedure,public::get_d_value_array  => muli_trapezium_get_d_value_array
     procedure,public::get_d_value_element=> muli_trapezium_get_d_value_element
     procedure,public::get_l_integral_array  => muli_trapezium_get_l_integral_array
     procedure,public::get_l_integral_element=> muli_trapezium_get_l_integral_element
     procedure,public::get_r_integral_array  => muli_trapezium_get_r_integral_array
     procedure,public::get_r_integral_element=> muli_trapezium_get_r_integral_element
     procedure,public::get_d_integral_array  => muli_trapezium_get_d_integral_array
     procedure,public::get_d_integral_element=> muli_trapezium_get_d_integral_element
     procedure,public::get_l_propability_element => muli_trapezium_get_l_propability_element
     procedure,public::get_l_propability_array   => muli_trapezium_get_l_propability_array
     procedure,public::get_r_propability_element => muli_trapezium_get_r_propability_element
     procedure,public::get_r_propability_array   => muli_trapezium_get_r_propability_array
     procedure,public::get_d_propability_element => muli_trapezium_get_d_propability_element
     procedure,public::get_d_propability_array   => muli_trapezium_get_d_propability_array
     procedure,public::get_error          => muli_trapezium_get_error
     procedure,public::get_error_sum      => muli_trapezium_get_error_sum
     procedure,public::get_integral_sum   => muli_trapezium_get_integral_sum
     generic,public::get_l_value => get_l_value_array,get_l_value_element
     generic,public::get_r_value => get_r_value_array,get_r_value_element
     generic,public::get_d_value => get_d_value_array,get_d_value_element
     generic,public::get_l_integral => get_l_integral_array,get_l_integral_element
     generic,public::get_r_integral => get_r_integral_array,get_r_integral_element
     generic,public::get_d_integral => get_d_integral_array,get_d_integral_element
     generic,public::get_l_propability => get_l_propability_array,get_l_propability_element
     generic,public::get_r_propability => get_r_propability_array,get_r_propability_element
     generic,public::get_d_propability => get_d_propability_array,get_d_propability_element
     ! interpolations
     procedure,public::get_value_at_position => muli_trapezium_get_value_at_position
     procedure::set_r_value       => muli_trapezium_set_r_value
     procedure::set_d_value       => muli_trapezium_set_d_value
     procedure::set_r_integral    => muli_trapezium_set_r_integral
     procedure::set_d_integral    => muli_trapezium_set_d_integral
     procedure::set_r_propability => muli_trapezium_set_r_propability
     procedure::set_d_propability => muli_trapezium_set_d_propability
     procedure::set_error         => muli_trapezium_set_error
     ! tests
     procedure,public:: is_left_of => muli_trapezium_is_left_of
     procedure,public:: includes => muli_trapezium_includes
     ! convert
     procedure :: to_node => muli_trapezium_to_node
     procedure :: sum_up => muli_trapezium_sum_up
     ! approximation
     procedure :: approx_value => muli_trapezium_approx_value
     procedure :: approx_value_n => muli_trapezium_approx_value_n
     procedure :: approx_integral => muli_trapezium_approx_integral
     procedure :: approx_integral_n => muli_trapezium_approx_integral_n
     procedure :: approx_propability => muli_trapezium_approx_propability
     procedure :: approx_propability_n => muli_trapezium_approx_propability_n
     procedure :: approx_position_by_integral => muli_trapezium_approx_position_by_integral
     !    procedure :: choose_partons => muli_trapezium_choose_partons
     procedure :: split => muli_trapezium_split
     procedure :: update => muli_trapezium_update
  end type muli_trapezium_type

  type,extends(muli_trapezium_type),abstract :: muli_trapezium_node_class
     private
     class(muli_trapezium_node_class), pointer :: left => null()
     class(muli_trapezium_node_class), pointer :: right => null()
!     real(kind=double) :: criterion
   contains
!     private
     ! overridden serializable_class procedures
     procedure,public :: deserialize_from_marker=>muli_trapezium_node_deserialize_from_marker
     ! new procedures
     procedure(muli_trapezium_append_interface),deferred,public::append
     procedure(muli_trapezium_final_interface),deferred,public :: finalize
     procedure,public :: nullify => muli_trapezium_node_nullify
     procedure,public :: get_left => muli_trapezium_node_get_left
     procedure,public :: get_right => muli_trapezium_node_get_right
     procedure,public :: get_leftmost => muli_trapezium_node_get_leftmost
     procedure,public :: get_rightmost => muli_trapezium_node_get_rightmost
     procedure,public :: decide_by_value => muli_trapezium_node_decide_by_value
     procedure,public :: decide_by_position => muli_trapezium_node_decide_by_position
     procedure,public :: decide_decreasing => muli_trapezium_node_decide_decreasing
     procedure,public :: muli_trapezium_node_to_tree
     procedure,private:: untangle => muli_trapezium_node_untangle
     procedure,public :: apply => muli_trapezium_node_apply
!     procedure,public :: copy => muli_trapezium_node_copy
!     generic,public   :: assignment(=) => copy
!     procedure,deferred,public :: approx => muli_trapezium_node_approx
     generic,public::decide=>decide_by_value,decide_by_position
  end type muli_trapezium_node_class

  type,extends(muli_trapezium_node_class) :: muli_trapezium_tree_type
     class(muli_trapezium_node_class), pointer :: down => null()
   contains
     ! overridden serializable_class procedures
     procedure :: write_to_marker => muli_trapezium_tree_write_to_marker
     procedure :: read_from_marker => muli_trapezium_tree_read_from_marker
     procedure :: print_to_unit => muli_trapezium_tree_print_to_unit
     procedure,nopass :: get_type => muli_trapezium_tree_get_type
     procedure,nopass :: verify_type => muli_trapezium_tree_verify_type
     ! overridden linn_approx_node_class procedures
     procedure,public :: nullify => muli_trapezium_tree_nullify
     procedure,public :: finalize => muli_trapezium_tree_finalize
     procedure,public :: decide_by_value => muli_trapezium_tree_decide_by_value
     procedure,public :: decide_by_position => muli_trapezium_tree_decide_by_position
     procedure,public :: decide_decreasing => muli_trapezium_tree_decide_decreasing     
     ! new procedures
     procedure,public :: get_left_list => muli_trapezium_tree_get_left_list
     procedure,public :: get_right_list => muli_trapezium_tree_get_right_list
     procedure,public :: find_by_value => muli_trapezium_tree_find_by_value
     procedure,public :: find_by_position => muli_trapezium_tree_find_by_position
     procedure,public :: find_decreasing => muli_trapezium_tree_find_decreasing
     procedure,public :: approx_by_integral => muli_trapezium_tree_approx_by_integral
     procedure,public :: approx_by_propability => muli_trapezium_tree_approx_by_propability
     procedure,public :: to_tree => muli_trapezium_tree_to_tree
     generic,public::find=>find_by_value,find_by_position
     procedure::append=>muli_trapezium_tree_append
     procedure::gnuplot=>muli_trapezium_tree_gnuplot
  end type muli_trapezium_tree_type

  type,extends(muli_trapezium_node_class) :: muli_trapezium_list_type
   contains
     ! overridden serializable_class procedures
     procedure :: write_to_marker => muli_trapezium_list_write_to_marker
     procedure :: read_from_marker => muli_trapezium_list_read_from_marker
     procedure :: read_target_from_marker => muli_trapezium_list_read_target_from_marker
     procedure :: print_to_unit => muli_trapezium_list_print_to_unit
     procedure,nopass :: get_type => muli_trapezium_list_get_type
     procedure,nopass :: verify_type => muli_trapezium_list_verify_type
     ! new procedures
     procedure,public :: finalize => muli_trapezium_list_finalize
     procedure,public :: insert_right_a => muli_trapezium_list_insert_right_a
!     procedure,public :: insert_right_b => muli_trapezium_list_insert_right_b
     generic,public :: insert_right => insert_right_a!,insert_right_b
     procedure,public :: insert_left_a => muli_trapezium_list_insert_left_a
!     procedure,public :: insert_left_b => muli_trapezium_list_insert_left_b
     generic,public :: insert_left => insert_left_a!,insert_left_b
     procedure::append=>muli_trapezium_list_append
     procedure,public :: to_tree => muli_trapezium_list_to_tree
     procedure,public :: gnuplot => muli_trapezium_list_gnuplot
     procedure,public :: integrate => muli_trapezium_list_integrate
     procedure,public :: check => muli_trapezium_list_check
     procedure,public :: apply => muli_trapezium_list_apply
  end type muli_trapezium_list_type

  abstract interface
     subroutine muli_trapezium_append_interface(this,right)
       import muli_trapezium_node_class
       class(muli_trapezium_node_class),intent(inout),target :: this,right
     end subroutine muli_trapezium_append_interface
     subroutine muli_trapezium_final_interface(this)
       import muli_trapezium_node_class
       class(muli_trapezium_node_class),intent(inout) :: this
     end subroutine muli_trapezium_final_interface
  end interface

contains
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for muli_trapezium_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! overridden serializable_class procedures
  
    subroutine muli_trapezium_write_to_marker (this,marker,status)
    class(muli_trapezium_type), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    ! local variables
    integer::dim
    call marker%mark_begin("muli_trapezium_type")
    call marker%mark("dim",this%dim)
    call marker%mark("r_position",this%r_position)
    call marker%mark("d_position",this%d_position)
    if(allocated(this%values))then
       call marker%mark("values",this%values)
    else
       call marker%mark_null("values")
    end if
    call marker%mark_end("muli_trapezium_type")
  end subroutine muli_trapezium_write_to_marker

  subroutine muli_trapezium_read_from_marker (this,marker,status)
    class(muli_trapezium_type), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    ! local variables
    integer::dim
    call marker%pick_begin("muli_trapezium_type",status=status)
    call marker%pick("dim",this%dim,status)
    call marker%pick("r_position",this%r_position,status)
    call marker%pick("d_position",this%d_position,status)
    if(allocated(this%values))deallocate(this%values)
    call marker%verify_nothing("values",status)
    if(status==serialize_ok)then
       allocate(this%values(0:this%dim-1,7))
       call marker%pick("values",this%values,status)
    end if
    call marker%pick_end("muli_trapezium_type",status)
  end subroutine muli_trapezium_read_from_marker

  subroutine muli_trapezium_print_to_unit(this,unit,parents,components,peers)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of muli_trapezium_type:")')
    write(unit,fmt=*)"Dimension:        ",this%dim
    write(unit,fmt=*)"Right position:   ",this%r_position
    write(unit,fmt=*)"Position step:    ",this%d_position
    if(allocated(this%values))then    
       if(components>0)then          
          write(unit,fmt=*)"Right values:     ",muli_trapezium_get_r_value_array(this)
          write(unit,fmt=*)"Value step:       ",this%get_d_value()
          write(unit,fmt=*)"Right integrals:  ",this%get_r_integral()
          write(unit,fmt=*)"Integral step:    ",this%get_d_integral()
          write(unit,fmt=*)"Right propabilities:",this%get_r_propability()
          write(unit,fmt=*)"Propability step: ",this%get_d_propability()
          write(unit,fmt=*)"Errors:           ",this%get_error()
       else
          write(unit,fmt=*)"Values are allocated."
       end if
    else
       write(unit,fmt=*)"Values are not allocated."
    end if
  end subroutine muli_trapezium_print_to_unit
  
  pure subroutine muli_trapezium_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="muli_trapezium_type")
  end subroutine muli_trapezium_get_type

  elemental logical function muli_trapezium_verify_type(type) result(match)
    character(*),intent(in)::type
    match=type=="muli_trapezium_type"
  end function muli_trapezium_verify_type

  elemental function muli_trapezium_measure(this)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::muli_trapezium_measure
    muli_trapezium_measure=this%measure_comp
  end function muli_trapezium_measure

  subroutine muli_trapezium_initialize(this,dim,r_position,d_position)
    class(muli_trapezium_type),intent(inout)::this
    integer,intent(in)::dim
    real(kind=double),intent(in)::r_position,d_position
    integer::dim1,dim2
    this%dim=dim
    this%r_position=r_position
    this%d_position=d_position
    if(allocated(this%values))deallocate(this%values)
    allocate(this%values(0:dim-1,value_dimension))
    do dim2=1,value_dimension-1
       do dim1=0,dim-1
          this%values(dim1,dim2)=0D0
       end do       
    end do
    do dim1=0,dim-1
       this%values(dim1,value_dimension)=huge(1D0)
    end do
    this%measure_comp=huge(1D0)
  end subroutine muli_trapezium_initialize

  !!! components !!!

  elemental function muli_trapezium_get_dimension(this) result(dim)
    class(muli_trapezium_type),intent(in)::this
    integer::dim
    dim=this%dim
  end function muli_trapezium_get_dimension

  pure function muli_trapezium_get_l_position(this) result(pos)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::pos
    pos=this%r_position-this%d_position
  end function muli_trapezium_get_l_position

  pure function muli_trapezium_get_r_position(this) result(pos)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::pos
    pos=this%r_position
  end function muli_trapezium_get_r_position

  pure function muli_trapezium_get_d_position(this) result(pos)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::pos
    pos=this%d_position
  end function muli_trapezium_get_d_position

  pure function muli_trapezium_get_error_sum(this) result(error)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::error
    error=sum(this%values(0:this%dim-1,error_index))
  end function muli_trapezium_get_error_sum

  pure function muli_trapezium_get_integral_sum(this) result(error)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double)::error
    error=sum(this%values(0:this%dim-1,d_integral_index))
  end function muli_trapezium_get_integral_sum

  pure function muli_trapezium_get_l_value_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_value_index)-this%values(set,d_value_index)
  end function muli_trapezium_get_l_value_element

  pure function muli_trapezium_get_l_value_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_value_index)-this%values(0:this%dim-1,d_value_index)
  end function muli_trapezium_get_l_value_array

  pure function muli_trapezium_get_r_value_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_value_index)
  end function muli_trapezium_get_r_value_element

  pure function muli_trapezium_get_r_value_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_value_index)
  end function muli_trapezium_get_r_value_array

  pure function muli_trapezium_get_d_value_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,d_value_index)
  end function muli_trapezium_get_d_value_element

  pure function muli_trapezium_get_d_value_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,d_value_index)
  end function muli_trapezium_get_d_value_array

  pure function muli_trapezium_get_l_integral_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_integral_index)-this%values(set,d_integral_index)
  end function muli_trapezium_get_l_integral_element

  pure function muli_trapezium_get_l_integral_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_integral_index)-this%values(0:this%dim-1,d_integral_index)
  end function muli_trapezium_get_l_integral_array

  pure function muli_trapezium_get_r_integral_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_integral_index)
  end function muli_trapezium_get_r_integral_element

  pure function muli_trapezium_get_r_integral_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_integral_index)
  end function muli_trapezium_get_r_integral_array

  pure function muli_trapezium_get_d_integral_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,d_integral_index)
  end function muli_trapezium_get_d_integral_element

  pure function muli_trapezium_get_d_integral_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,d_integral_index)
  end function muli_trapezium_get_d_integral_array

  pure function muli_trapezium_get_l_propability_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_propability_index)-this%values(set,d_propability_index)
  end function muli_trapezium_get_l_propability_element

  pure function muli_trapezium_get_l_propability_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_propability_index)-this%values(0:this%dim-1,d_propability_index)
  end function muli_trapezium_get_l_propability_array

  pure function muli_trapezium_get_r_propability_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,r_propability_index)
  end function muli_trapezium_get_r_propability_element

  pure function muli_trapezium_get_r_propability_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,r_propability_index)
  end function muli_trapezium_get_r_propability_array

  pure function muli_trapezium_get_d_propability_array(this) result(subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::subarray
    subarray=this%values(0:this%dim-1,d_propability_index)
  end function muli_trapezium_get_d_propability_array

  pure function muli_trapezium_get_d_propability_element(this,set) result(element)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::set
    real(kind=double)::element
    element=this%values(set,d_propability_index)
  end function muli_trapezium_get_d_propability_element

  pure function muli_trapezium_get_error(this) result(error)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),dimension(this%dim)::error
    error=this%values(0:this%dim-1,error_index)
  end function muli_trapezium_get_error

  ! interpolation
  
  subroutine muli_trapezium_get_value_at_position(this,pos,subarray)
    class(muli_trapezium_type),intent(in)::this
    real(kind=double),intent(in)::pos
    real(kind=double),dimension(this%dim),intent(out)::subarray
    subarray=this%get_r_value_array()-this%get_d_value()*this%d_position/(this%r_position-pos)
  end subroutine muli_trapezium_get_value_at_position

  ! write access

  subroutine muli_trapezium_set_r_value(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,r_value_index)=subarray
  end subroutine muli_trapezium_set_r_value

  subroutine muli_trapezium_set_d_value(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,d_value_index)=subarray
  end subroutine muli_trapezium_set_d_value

  subroutine muli_trapezium_set_r_integral(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,r_integral_index)=subarray
  end subroutine muli_trapezium_set_r_integral

  subroutine muli_trapezium_set_d_integral(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,d_integral_index)=subarray
  end subroutine muli_trapezium_set_d_integral

  subroutine muli_trapezium_set_r_propability(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,r_propability_index)=subarray
  end subroutine muli_trapezium_set_r_propability

  subroutine muli_trapezium_set_d_propability(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,d_propability_index)=subarray
  end subroutine muli_trapezium_set_d_propability

  subroutine muli_trapezium_set_error(this,subarray)
    class(muli_trapezium_type),intent(inout)::this
    real(kind=double),intent(in),dimension(0:this%dim-1)::subarray
    this%values(0:this%dim-1,error_index)=subarray
    this%measure_comp=sum(subarray)
  end subroutine muli_trapezium_set_error

  ! tests

  pure function muli_trapezium_is_left_of(this,that) result(is_left)
    logical::is_left
    class(muli_trapezium_type),intent(in)::this,that
    is_left=this%r_position<=that%r_position!-that%d_position
!    if (is_left.and.that%r_position<this%r_position) then
!       print *,"!"
!       STOP
!    end if
  end function muli_trapezium_is_left_of

  elemental logical function muli_trapezium_includes(this,dim,position,value,integral,propability) result(includes)
    class(muli_trapezium_type),intent(in)::this
    integer,intent(in)::dim
    real(kind=double),intent(in),optional::position,value,integral,propability
    includes=.true.
    if(present(position))then
       if(this%get_l_position()>position.or.position>=this%get_r_position())includes=.false.
    end if
    if(present(value))then
       if(this%get_l_value(dim)>value.or.value>=this%get_r_value(dim))includes=.false.
    end if
    if(present(integral))then
       if(this%get_l_integral(dim)>integral.or.integral>=this%get_r_integral(dim))includes=.false.
    end if
    if(present(propability))then
       if(this%get_l_propability(dim)>propability.or.propability>=this%get_r_propability(dim))includes=.false.
    end if    
  end function muli_trapezium_includes

  subroutine muli_trapezium_update(this)
    class(muli_trapezium_type),intent(inout) :: this
    real(kind=double),dimension(:),allocatable :: int
    allocate(int(0:this%dim-1),source=this%get_d_integral())
    call this%set_d_integral(-this%d_position*(this%get_r_value_array()-this%get_d_value()/2D0))
    call this%set_error(abs(this%get_d_integral()-int))
!    print('(11(D20.10))'),this%get_d_integral()
  end subroutine muli_trapezium_update

  subroutine muli_trapezium_split(this,c_value,c_position,new_node)
    class(muli_trapezium_type),intent(inout) :: this
    real(kind=double),intent(in) :: c_position
    real(kind=double),intent(in),dimension(this%dim) :: c_value
    class(muli_trapezium_type),intent(out),pointer :: new_node
    real(kind=double) :: ndpr,ndpl
    real(kind=double),dimension(:),allocatable::ov,edv
    ndpr=this%r_position-c_position
    ndpl=this%d_position-ndpr
    allocate(ov(0:this%dim-1),source=this%get_r_value_array()-ndpr*this%get_d_value()/this%d_position)
    allocate(edv(0:this%dim-1),source=c_value-ov)
    allocate(new_node)
    call new_node%initialize(dim=this%dim,&
         &r_position=c_position,&
         &d_position=ndpl)
    call new_node%set_r_value(c_value)
    call new_node%set_d_value(this%get_d_value()+c_value-this%get_r_value_array())
    call new_node%set_d_integral(ndpl*(this%get_d_value()-this%get_r_value_array()-c_value)/2D0)
    call new_node%set_error(abs((edv*ndpl)/2D0))
    !new_node%measure_comp=sum(abs((edv*ndpl)/2D0))
    this%d_position=ndpr
    call this%set_d_value(this%get_r_value_array()-c_value)
    call this%set_d_integral(-(ndpr*(this%get_r_value_array()+c_value)/2D0))
    call this%set_error(abs(edv*ndpr/2D0))
    !this%measure_comp=sum(abs(edv*ndpr/2D0))
!    print ('("muli_trapezium_split: new errors:")')
!    print ('(E14.7)'),this%get_error()
!    print ('(E14.7)'),new_node%get_error()
!    print('(11(D20.10))'),new_node%get_d_integral()
!    print('(11(D20.10))'),this%get_d_integral()
  end subroutine muli_trapezium_split

  pure function muli_trapezium_approx_value(this,x) result(val)
    ! returns the values at x
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double),dimension(this%dim) :: val
    real(kind=double), intent(in) :: x
    val = this%get_r_value_array()+(x-this%r_position)*this%get_d_value()/this%d_position
  end function muli_trapezium_approx_value

  elemental function muli_trapezium_approx_value_n(this,x,n) result(val)
    ! returns the value at x
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double)::val
    real(kind=double), intent(in) :: x
    integer,intent(in)::n
    val = this%get_r_value_element(n)+(x-this%r_position)*this%get_d_value_element(n)/this%d_position
  end function muli_trapezium_approx_value_n

  pure function muli_trapezium_approx_integral(this,x)
    ! returns the integral from x to r_position
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double),dimension(this%dim) :: muli_trapezium_approx_integral
    real(kind=double), intent(in) :: x
    muli_trapezium_approx_integral = &
!         &this%get_r_integral()+&
!         &(this%r_position-x)*this%get_r_value()+&
!         &(x**2-this%r_position**2)*this%get_d_integral()/(this%d_position*2D0)
         &this%get_r_integral()+&
         &((this%r_position-x)*&
         &(-this%get_d_value()*(this%r_position-x)+2*this%d_position*this%get_r_value_array()))/&
         &(2*this%d_position)
  end function muli_trapezium_approx_integral

  elemental function muli_trapezium_approx_integral_n(this,x,n) result(val)
    ! returns the integral from x to r_position
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double)::val
    real(kind=double), intent(in) :: x
    integer,intent(in)::n
    val = &
         &this%get_r_integral_element(n)+&
         &((this%r_position-x)*&
         &(-this%get_d_value_element(n)*(this%r_position-x)+2*this%d_position*this%get_r_value_element(n)))/&
         &(2*this%d_position)
  end function muli_trapezium_approx_integral_n

   pure function muli_trapezium_approx_propability(this,x) result(prop)
    ! returns the vlaues at x
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double),dimension(this%dim) :: prop
    real(kind=double), intent(in) :: x
    prop=exp(-this%approx_integral(x))
  end function muli_trapezium_approx_propability

  elemental function muli_trapezium_approx_propability_n(this,x,n) result(val)
    ! returns the integral from x to r_position
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double)::val
    real(kind=double), intent(in) :: x
    integer,intent(in)::n
    val = exp(-this%approx_integral_n(x,n))
  end function muli_trapezium_approx_propability_n
  
  elemental function muli_trapezium_approx_position_by_integral(this,dim,int) result(val)
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double)::val
    integer,intent(in)::dim
    real(kind=double),intent(in)::int
    real(kind=double)::dpdv    
    dpdv=(this%d_position/this%values(dim,d_value_index))
    val=this%r_position-dpdv*&
         &(this%values(dim,r_value_index)-&
           &sqrt(((this%values(dim,r_integral_index)-int)*2D0/dpdv)+this%values(dim,r_value_index)**2))
  end function muli_trapezium_approx_position_by_integral

  subroutine muli_trapezium_to_node(this,value,list,tree)
    class(muli_trapezium_type),intent(in) :: this
    real(kind=double),intent(in) :: value  
!    class(muli_trapezium_node_class),optional,pointer,intent(out) :: node
    class(muli_trapezium_list_type),optional,pointer,intent(out) :: list
    class(muli_trapezium_tree_type),optional,pointer,intent(out) :: tree
!!$    if(present(node))then
!!$       allocate(node)
!!$       node%dim=this%dim
!!$       node%r_position=this%r_position
!!$       node%d_position=this%d_position
!!$       allocate(node%values(this%dim,value_dimension),source=this%values)
!!$    end if
    if(present(list))then
       allocate(list)
       list%dim=this%dim
       list%r_position=this%r_position
       list%d_position=this%d_position
       allocate(list%values(0:this%dim-1,value_dimension),source=this%values)
    end if
    if(present(tree))then
       allocate(tree)
       tree%dim=this%dim
       tree%r_position=this%r_position
       tree%d_position=this%d_position
       allocate(tree%values(0:this%dim-1,value_dimension),source=this%values)
    end if    
  end subroutine muli_trapezium_to_node

  subroutine muli_trapezium_sum_up(this)
    class(muli_trapezium_type),intent(inout) :: this
    integer::i
    if(allocated(this%values))then
       do i=1,7
          this%values(0,i)=sum(this%values(1:this%dim-1,i))
       end do
    end if
  end subroutine muli_trapezium_sum_up

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for muli_trapezium_node_class !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine muli_trapezium_node_deserialize_from_marker(this,name,marker)
    class(muli_trapezium_node_class), intent(out) :: this
    character(*),intent(in)::name
    class(marker_type),intent(inout)::marker
    integer(kind=dik)::status
    class(serializable_class),pointer::ser
    allocate(muli_trapezium_tree_type::ser)
    call marker%push_reference(ser)
    allocate(muli_trapezium_list_type::ser)
    call marker%push_reference(ser)
    call serializable_deserialize_from_marker(this,name,marker)
    call marker%pop_reference(ser)
    deallocate(ser)
    call marker%pop_reference(ser)
    deallocate(ser)
  end subroutine muli_trapezium_node_deserialize_from_marker

  subroutine muli_trapezium_node_nullify(this)
    class(muli_trapezium_node_class),intent(out) :: this
    nullify(this%left)
    nullify(this%right)
  end subroutine muli_trapezium_node_nullify

  subroutine muli_trapezium_node_to_tree(this,out_tree)
    class(muli_trapezium_node_class),intent(in) :: this
    class(muli_trapezium_tree_type),intent(out) :: out_tree
    out_tree%left=>this%left
    out_tree%right=>this%right
  end subroutine muli_trapezium_node_to_tree

  subroutine muli_trapezium_node_get_left(this,left)
    class(muli_trapezium_node_class),intent(in) :: this
    class(muli_trapezium_node_class),pointer,intent(out) :: left
    left=>this%left
  end subroutine muli_trapezium_node_get_left

  subroutine muli_trapezium_node_get_right(this,right)
    class(muli_trapezium_node_class),intent(in) :: this
    class(muli_trapezium_node_class),pointer,intent(out) :: right
    right=>this%right
  end subroutine muli_trapezium_node_get_right

  subroutine muli_trapezium_node_get_leftmost(this,node)
    class(muli_trapezium_node_class),intent(in) :: this
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if (associated(this%left)) then
       node=>this%left
       do while (associated(node%left))
          node=>node%left
       end do
    else
       nullify(node)
    end if
  end subroutine muli_trapezium_node_get_leftmost

  subroutine muli_trapezium_node_get_rightmost(this,right)
    class(muli_trapezium_node_class),intent(in) :: this
    class(muli_trapezium_node_class),pointer,intent(out) :: right
    if (associated(this%right)) then
       right=>this%right
       do while (associated(right%right))
          right=>right%right
       end do
    else
       nullify(right)
    end if
  end subroutine muli_trapezium_node_get_rightmost

  subroutine muli_trapezium_node_decide_by_value(this,value,dim,record,node)
    class(muli_trapezium_node_class),intent(in) :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::record,dim
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if(this%values(dim,record)>value)then
       node=>this%left
    else
       node=>this%right
    end if
  end subroutine muli_trapezium_node_decide_by_value

  subroutine muli_trapezium_node_decide_by_position(this,position,node)
    class(muli_trapezium_node_class),intent(in) :: this
    real(kind=double),intent(in)::position
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if(this%r_position>position)then
       node=>this%left
    else
       node=>this%right
    end if
  end subroutine muli_trapezium_node_decide_by_position

  subroutine muli_trapezium_node_decide_decreasing(this,value,dim,record,node)
    class(muli_trapezium_node_class),intent(in) :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::record,dim
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if(this%values(dim,record)<=value)then
       node=>this%left
    else
       node=>this%right
    end if
  end subroutine muli_trapezium_node_decide_decreasing

  subroutine muli_trapezium_node_untangle(this)
    class(muli_trapezium_node_class),intent(inout),target :: this
    if(associated(this%left))then
       if(associated(this%left%right,this))then
          nullify(this%left%right)
          nullify(this%left)
       end if
    end if    
  end subroutine muli_trapezium_node_untangle

  recursive subroutine muli_trapezium_node_apply(this,proc)
    class(muli_trapezium_node_class),intent(inout) :: this
    interface
       subroutine proc(this)
         import muli_trapezium_node_class
         class(muli_trapezium_node_class),intent(inout) :: this
       end subroutine proc
    end interface
    if(associated(this%right))call proc(this%right)
    if(associated(this%left))call proc(this%left)
    call proc(this)
  end subroutine muli_trapezium_node_apply

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for muli_trapezium_list_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine muli_trapezium_list_write_to_marker (this,marker,status)
    class(muli_trapezium_list_type), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status 
    ! local variables
    class(serializable_class),pointer::ser
    call marker%mark_begin("muli_trapezium_list_type")
    call muli_trapezium_write_to_marker(this,marker,status)   
    ser=>this%right
    call marker%mark_pointer("right",ser)
    call marker%mark_end("muli_trapezium_list_type")
  end subroutine muli_trapezium_list_write_to_marker

  recursive subroutine muli_trapezium_list_read_from_marker (this,marker,status)
    class(muli_trapezium_list_type), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    print *,"muli_trapezium_list_read_from_marker: You cannot deserialize a list with this subroutine."
    print *,"Use muli_trapezium_list_read_target_from_marker instead."    
  end subroutine muli_trapezium_list_read_from_marker

  recursive subroutine muli_trapezium_list_read_target_from_marker (this,marker,status)
    class(muli_trapezium_list_type),target,intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    ! local variables
    class(serializable_class),pointer::ser
    call marker%pick_begin("muli_trapezium_list_type",status=status)
    call muli_trapezium_read_from_marker(this,marker,status)
    call marker%pick_pointer("right",ser)
    if(associated(ser))then
       select type(ser)
       class is (muli_trapezium_list_type)
          this%right=>ser
          ser%left=>this
       class default
          nullify(this%right)
          print *,"muli_trapezium_list_read_target_from_marker: Unexpected type for right component."
       end select
    else
       nullify(this%right)
    end if
    call marker%pick_end("muli_trapezium_list_type",status)
  end subroutine muli_trapezium_list_read_target_from_marker
  
  recursive subroutine muli_trapezium_list_print_to_unit(this,unit,parents,components,peers)
    class(muli_trapezium_list_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    if(parents>0)call muli_trapezium_print_to_unit(this,unit,parents-1,components,peers)
    ser=>this%left
    call serialize_print_peer_pointer(ser,unit,-one,-one,-one,"LEFT")
    ser=>this%right
    call serialize_print_peer_pointer(ser,unit,parents,components,peers,"RIGHT")
  end subroutine muli_trapezium_list_print_to_unit
  
  pure subroutine muli_trapezium_list_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="muli_trapezium_list_type")
  end subroutine muli_trapezium_list_get_type

  elemental logical function muli_trapezium_list_verify_type(type) result(match)
    character(*),intent(in)::type
    match=type=="muli_trapezium_list_type"
  end function muli_trapezium_list_verify_type

  recursive subroutine muli_trapezium_list_finalize(this)
    class(muli_trapezium_list_type),intent(inout)::this
    if (associated(this%right)) then
       call this%right%finalize()
       deallocate(this%right)
    end if
    this%dim=0
  end subroutine muli_trapezium_list_finalize

  subroutine muli_trapezium_list_insert_left_a(this,value,content,new_node)
    class(muli_trapezium_list_type),intent(inout),target :: this
    real(kind=double),intent(in) :: value
    class(muli_trapezium_type),intent(in) :: content
    class(muli_trapezium_list_type),pointer,intent(out) :: new_node
    call content%to_node(value,list=new_node)
    new_node%right=>this
    if(associated(this%left))then
       new_node%left=>this%left
       this%left%right=>new_node
    else
       nullify(new_node%left)
    end if
    this%left=>new_node
  end subroutine muli_trapezium_list_insert_left_a

!!$  subroutine muli_trapezium_list_insert_right_old(this,value,content,new_node)
!!$    class(muli_trapezium_list_type),intent(inout),target :: this
!!$    real(kind=double),intent(in) :: value
!!$    class(muli_trapezium_type),intent(in) :: content
!!$    class(muli_trapezium_list_type),pointer,intent(out) :: new_node
!!$    call content%to_node(value,list=new_node)
!!$    new_node%left=>this
!!$    if(associated(this%right))then
!!$       new_node%right=>this%right
!!$       this%right%left=>new_node
!!$    else
!!$       nullify(new_node%right)
!!$    end if
!!$    this%right=>new_node
!!$  end subroutine muli_trapezium_list_insert_right_old

  subroutine muli_trapezium_list_insert_right_a(this,value,content,new_node)
    class(muli_trapezium_list_type),intent(inout),target :: this
    real(kind=double),intent(in) :: value
    class(muli_trapezium_type),intent(in) :: content
    class(muli_trapezium_list_type),pointer,intent(out) :: new_node
    class(muli_trapezium_list_type),pointer :: tmp_list
    call content%to_node(value,list=tmp_list)
    if(associated(this%right))then
       this%right%left=>tmp_list
       tmp_list%right=>this%right
    else
       nullify(tmp_list%right)
    end if
    this%right=>tmp_list
    tmp_list%left=>this
    new_node=>tmp_list
  end subroutine muli_trapezium_list_insert_right_a

  subroutine muli_trapezium_list_append(this,right)
    class(muli_trapezium_list_type),intent(inout),target :: this
    class(muli_trapezium_node_class),intent(inout),target :: right
    this%right=>right
    right%left=>this
  end subroutine muli_trapezium_list_append

  subroutine muli_trapezium_list_to_tree(this,out_tree)
    class(muli_trapezium_list_type),target,intent(in) :: this
    class(muli_trapezium_tree_type),intent(out) :: out_tree
    type(muli_trapezium_tree_type),target :: do_list
    class(muli_trapezium_node_class),pointer :: this_entry,do_list_entry,node
    class(muli_trapezium_tree_type),pointer :: tree1,tree2
    integer :: ite,log,n_deep,n_leaves
    n_leaves=0
    this_entry => this
    count: do while(associated(this_entry))
       n_leaves=n_leaves+1
       this_entry=>this_entry%right
    end do count
    call ilog2(n_leaves,log,n_deep)
    this_entry => this
    do_list_entry => do_list
    deep: do ite=0,n_deep-1
       allocate(tree1)
       tree1%down=>this_entry%right
       allocate(tree2)
       tree2%down=>this_entry
       tree2%left=>this_entry
       tree2%right=>this_entry%right
       tree1%left=>tree2
       this_entry => this_entry%right%right
       do_list_entry%right=>tree1
       do_list_entry=>tree1
    end do deep
    rest: do while(associated(this_entry))
       allocate(tree1)
       tree1%down=>this_entry
       tree1%left=>this_entry
       do_list_entry%right => tree1
       do_list_entry => tree1
       this_entry => this_entry%right
       ite=ite+1
    end do rest
    tree: do while(ite>2)
       do_list_entry => do_list%right
       node=>do_list
       level: do while(associated(do_list_entry))
          node%right=>do_list_entry%right
          node=>do_list_entry%right
          do_list_entry%right=>node%left
          node%left=>do_list_entry
          do_list_entry=>node%right
          ite=ite-1
       end do level
    end do tree
    node=>do_list%right
    select type(node)
    type is (muli_trapezium_tree_type)
       call node%to_tree(out_tree)
    class default
       print *,"muli_trapezium_list_to_tree"
       print *,"unexpeted type for do_list%right"
    end select
    out_tree%right=>out_tree%right%left
    if(allocated(out_tree%values))then
       deallocate(out_tree%values)
    end if
    deallocate(do_list%right%right)
    deallocate(do_list%right)
  end subroutine muli_trapezium_list_to_tree

  subroutine muli_trapezium_list_gnuplot(this,dir)
    class(muli_trapezium_list_type),intent(in),target :: this
    character(len=*),intent(in)::dir
    character(len=*),parameter::val_file="/value.plot"
    character(len=*),parameter::int_file="/integral.plot"
    character(len=*),parameter::err_file="/integral_error.plot"
    character(len=*),parameter::pro_file="/propability.plot"
    character(len=*),parameter::den_file="/density.plot"
    character(len=*),parameter::fmt='(E20.10)'
    class(muli_trapezium_node_class),pointer::list
    integer::val_unit,err_unit,int_unit,pro_unit,den_unit
    list=>this
    call generate_unit(val_unit,100,1000)
    open(val_unit,file=dir//val_file)
    call generate_unit(int_unit,100,1000)
    open(int_unit,file=dir//int_file)
    call generate_unit(err_unit,100,1000)
    open(err_unit,file=dir//err_file)
    call generate_unit(pro_unit,100,1000)
    open(pro_unit,file=dir//pro_file)
    call generate_unit(den_unit,100,1000)
    open(den_unit,file=dir//den_file)
    do while (associated(list))
!       print *,list%r_position,list%get_r_value()
       write(val_unit,fmt,advance='NO')list%r_position
       call write_array(val_unit,list%get_r_value_array(),fmt)
       write(int_unit,fmt,advance='NO')list%r_position
       call write_array(int_unit,list%get_r_integral(),fmt)
       write(err_unit,fmt,advance='NO')list%r_position
       call write_array(err_unit,list%get_error(),fmt)
       write(pro_unit,fmt,advance='NO')list%r_position
       call write_array(pro_unit,list%get_r_propability(),fmt)
       write(den_unit,fmt,advance='NO')list%r_position
       call write_array(den_unit,list%get_r_propability()*list%get_r_value_array(),fmt)
       list=>list%right
    end do
    close(val_unit)
    close(int_unit)
    close(err_unit)
    close(pro_unit)
    close(den_unit)
    contains
      subroutine write_array(unit,array,form)
        integer,intent(in)::unit
        real(kind=double),dimension(:),intent(in)::array
        character(len=*),intent(in)::form
        integer::n
        do n=1,size(array)
           write(unit,form,ADVANCE='NO')array(n)
           flush(unit)
        end do
        write(unit,'("")')
      end subroutine write_array
  end subroutine muli_trapezium_list_gnuplot

!!$  subroutine muli_trapezium_node_error_no_content(this)
!!$    class(muli_trapezium_node_class),intent(in) :: this
!!$!    print("muli_trapezium_node: Trying to access unallocated content.")
!!$!    call this%print()
!!$  end subroutine muli_trapezium_node_error_no_content

  subroutine muli_trapezium_list_integrate(this,integral_sum,error_sum)
    class(muli_trapezium_list_type),intent(in),target :: this
    real(kind=double),intent(out)::error_sum,integral_sum
    real(kind=double),dimension(:),allocatable::integral
    class(muli_trapezium_node_class),pointer :: node
    allocate(integral(0:this%dim-1))
    call this%get_rightmost(node)
    integral=0D0
    integral_sum=0D0
    error_sum=0D0
    integrate: do while(associated(node))
       node%values(1,r_value_index)=sum(node%values(1:this%dim-1,r_value_index))
       node%values(1,d_value_index)=sum(node%values(1:this%dim-1,d_value_index))
!       node%values(1,r_integral_index)=sum(node%values(1:this%dim-1,r_integral_index))
!       node%values(1,d_integral_index)=sum(node%values(1:this%dim-1,d_integral_index))
       node%values(1,error_index)=sum(node%values(1:this%dim-1,error_index))
       error_sum=error_sum+node%values(1,error_index)
       call node%set_d_integral(node%get_d_position()*(node%get_d_value()/2D0-node%get_r_value_array()))
       call node%set_r_propability(exp(-integral))
       call node%set_r_integral(integral)
       integral=integral-node%get_d_integral()
       call node%set_d_propability(node%get_r_propability()-exp(-integral))
!       call muli_trapezium_write(node,output_unit)
       call node%get_left(node)
    end do integrate
    integral_sum=integral(1)
  end subroutine muli_trapezium_list_integrate

  recursive subroutine muli_trapezium_list_check(this)
    class(muli_trapezium_list_type),intent(in),target :: this
    class(muli_trapezium_node_class),pointer::tn,next
    real(kind=double),parameter::eps=1d-10
    logical::test
    if(associated(this%right))then
       next=>this%right
       test=(this%r_position.le.this%right%get_l_position()+eps)
       print *,"position check:  ",test
       if(.not.test)then
          call this%print_parents()
          call next%print_parents()
       end if
       select type (next)
       class is (muli_trapezium_list_type)
          tn=>this
          print *,"structure check: ",associated(tn,next%left)
          print *,"class check:    T"
          call next%check()
       class default
          print *,"class check:    F"
       end select
    else
       print *,"end of list at ",this%r_position
    end if
  end subroutine muli_trapezium_list_check

  recursive subroutine muli_trapezium_list_apply(this,proc)
    class(muli_trapezium_list_type),intent(inout) :: this
    interface
       subroutine proc(this)
         import muli_trapezium_node_class
         class(muli_trapezium_node_class),intent(inout) :: this
       end subroutine proc
    end interface
    if(associated(this%right))call this%right%apply(proc)
    call proc(this)
  end subroutine muli_trapezium_list_apply
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for muli_trapezium_tree_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine muli_trapezium_tree_write_to_marker (this,marker,status)
    class(muli_trapezium_tree_type), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    ! local variables
    class(muli_trapezium_list_type),pointer::list
    class(serializable_class),pointer::ser
    call marker%mark_begin("muli_trapezium_tree_type")
    call this%get_left_list(list)
    ser=>list
    call marker%mark_pointer("list",ser)
    call marker%mark_end("muli_trapezium_tree_type")
  end subroutine muli_trapezium_tree_write_to_marker

  subroutine muli_trapezium_tree_read_from_marker (this,marker,status)
    class(muli_trapezium_tree_type), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    ! local variables
    class(serializable_class),pointer::ser
    call marker%pick_begin("muli_trapezium_tree_type",status=status)
    call marker%pick_pointer("list",ser)
    if(associated(ser))then
       select type(ser)
       class is (muli_trapezium_list_type)
          call ser%to_tree(this)
       class default
          nullify(this%left)
          nullify(this%right)
          nullify(this%down)
       end select
    else
       nullify(this%left)
       nullify(this%right)
       nullify(this%down)
    end if
    call marker%pick_end("muli_trapezium_tree_type",status)
  end subroutine muli_trapezium_tree_read_from_marker
  
  recursive subroutine muli_trapezium_tree_print_to_unit(this,unit,parents,components,peers)
    class(muli_trapezium_tree_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    if(parents>0)call muli_trapezium_print_to_unit(this,unit,parents-1,components,peers)
    ser=>this%down
    call serialize_print_peer_pointer(ser,unit,one,zero,one,"DOWN")
    if(associated(this%left))then
       select type(sertmp=>this%left)
       class is(muli_trapezium_list_type)
          ser=>sertmp
          call serialize_print_peer_pointer(ser,unit,parents,components,zero,"LEFT")
       class default
          call serialize_print_peer_pointer(ser,unit,parents,components,peers,"LEFT")
       end select
    else
       write(unit,fmt=*)"Left is not associated."
    end if
    if(associated(this%right))then
       select type(sertmp=>this%right)
       class is(muli_trapezium_list_type)
          ser=>sertmp
          call serialize_print_peer_pointer(ser,unit,parents,components,zero,"RIGHT")
       class default
          call serialize_print_peer_pointer(ser,unit,parents,components,peers,"RIGHT")
       end select
    else
       write(unit,fmt=*)"Right is not associated."
    end if
  end subroutine muli_trapezium_tree_print_to_unit

  pure subroutine muli_trapezium_tree_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="muli_trapezium_tree_type")
  end subroutine muli_trapezium_tree_get_type

  elemental logical function muli_trapezium_tree_verify_type(type) result(match)
    character(*),intent(in)::type
    match=type=="muli_trapezium_tree_type"
  end function muli_trapezium_tree_verify_type

  ! overridden linn_approx_node_class procedures

  subroutine muli_trapezium_tree_nullify(this)
    class(muli_trapezium_tree_type),intent(out) :: this
    call muli_trapezium_node_nullify(this)
    nullify(this%down)
  end subroutine muli_trapezium_tree_nullify
  
  subroutine muli_trapezium_tree_get_left_list(this,list)
    class(muli_trapezium_tree_type),intent(in) :: this
    class(muli_trapezium_list_type),pointer,intent(out) :: list
    class(muli_trapezium_node_class),pointer::node
    call this%get_leftmost(node)
    if(associated(node))then
       select type(node)
       class is (muli_trapezium_list_type)
          list=>node
       class default
          nullify(list)
       end select
    else
       nullify(list)
    end if
  end subroutine muli_trapezium_tree_get_left_list

  subroutine muli_trapezium_tree_get_right_list(this,list)
    class(muli_trapezium_tree_type),intent(in) :: this
    class(muli_trapezium_list_type),pointer,intent(out) :: list
    class(muli_trapezium_node_class),pointer::node
    call this%get_rightmost(node)
    if(associated(node))then
       select type(node)
       class is (muli_trapezium_list_type)
          list=>node
       class default
          nullify(list)
       end select
    else
       nullify(list) 
    end if
  end subroutine muli_trapezium_tree_get_right_list

  recursive subroutine muli_trapezium_tree_finalize(this)
    class(muli_trapezium_tree_type),intent(inout) :: this
    if (associated(this%right)) then
       call this%right%untangle()
       call this%right%finalize()
       deallocate(this%right)
    end if
    if (associated(this%left)) then
       call this%left%untangle()
       call this%left%finalize()
       deallocate(this%left)
    end if
    this%dim=0
  end subroutine muli_trapezium_tree_finalize

  subroutine muli_trapezium_tree_decide_by_value(this,value,dim,record,node)
    class(muli_trapezium_tree_type),intent(in) :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::record,dim
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if(this%down%values(dim,record)>value)then
       node=>this%left
    else
       node=>this%right
    end if
  end subroutine muli_trapezium_tree_decide_by_value

  subroutine muli_trapezium_tree_decide_by_position(this,position,node)
    class(muli_trapezium_tree_type),intent(in) :: this
    real(kind=double),intent(in)::position
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    if(this%down%r_position>position)then
       node=>this%left
    else
       node=>this%right
    end if
  end subroutine muli_trapezium_tree_decide_by_position

  subroutine muli_trapezium_tree_decide_decreasing(this,value,dim,record,node)
    class(muli_trapezium_tree_type),intent(in) :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::record,dim
!    integer,save::count=0
    class(muli_trapezium_node_class),pointer,intent(out) :: node
!    count=count+1
    if(this%down%values(dim,record)<=value)then
!       print('("Decide: value(",I2,",",I1,")=",E20.7," > ",E20.7,": go left.")'),dim,record,this%down%values(dim,record),value
       node=>this%left
    else
!       print('("Decide: value(",I2,",",I1,")=",E20.7," <= ",E20.7,": go right.")'),dim,record,this%down%values(dim,record),value
       node=>this%right
    end if
  end subroutine muli_trapezium_tree_decide_decreasing

  subroutine muli_trapezium_tree_find_by_value(this,value,dim,record,node)
    class(muli_trapezium_tree_type),intent(in),target :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::record,dim
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    node=>this
    do while(.not.allocated(node%values))
       call node%decide(value,dim,record,node)
    end do
  end subroutine muli_trapezium_tree_find_by_value

  subroutine muli_trapezium_tree_find_by_position(this,position,node)
    class(muli_trapezium_tree_type),intent(in),target :: this
    real(kind=double),intent(in)::position
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    node=>this
    do while(.not.allocated(node%values))
       call node%decide(position,node)
    end do
  end subroutine muli_trapezium_tree_find_by_position

!!$  subroutine muli_trapezium_tree_find_decreasing(this,value,dim,record,node)
!!$    class(muli_trapezium_tree_type),intent(in),target :: this
!!$    real(kind=double),intent(in)::value
!!$    integer,intent(in)::record,dim
!!$    class(muli_trapezium_node_class),pointer,intent(out) :: node
!!$    node=>this
!!$    do while(.not.allocated(node%values))
!!$       call node%decide_decreasing(value,dim,record,node)
!!$    end do
!!$  end subroutine muli_trapezium_tree_find_decreasing

  subroutine muli_trapezium_tree_find_decreasing(this,value,dim,node)
    class(muli_trapezium_tree_type),intent(in),target :: this
    real(kind=double),intent(in)::value
    integer,intent(in)::dim
    class(muli_trapezium_node_class),pointer,intent(out) :: node
    node=>this
    do while(.not.allocated(node%values))
       call node%decide_decreasing(value,dim,r_integral_index,node)
    end do
  end subroutine muli_trapezium_tree_find_decreasing

  subroutine muli_trapezium_tree_approx_by_integral(this,int,dim,in_range,position,value,integral,content)
    class(muli_trapezium_tree_type),intent(in),target :: this
    real(kind=double),intent(in) :: int
    integer,intent(in)::dim
    logical,intent(out) :: in_range
    class(muli_trapezium_node_class),pointer,intent(out),optional :: content
    real(kind=double),intent(out),optional :: position,value,integral
    integer::i
    real(kind=double) :: DINT!,l_prop,r_prop,d_prop
    real(kind=double)::RP,DP,RV,DV,RI!FC = gfortran
    class(muli_trapezium_node_class),pointer :: node
    node=>this
    do while(.not.allocated(node%values))
       call node%decide_decreasing(INT,dim,r_integral_index,node)
    end do
    if(   int<=node%values(dim,r_integral_index)-node%values(dim,d_integral_index)&
         &.and.&
         &int>=node%values(dim,r_integral_index))then
       in_range=.true.
!       associate(&!FC = nagfor
!            &RP=>node%r_position,&!FC = nagfor
!            &DP=>node%d_position,&!FC = nagfor
!            &RV=>node%values(dim,r_value_index),&!FC = nagfor
!            &DV=>node%values(dim,d_value_index),&!FC = nagfor
!            &RI=>node%values(dim,r_integral_index))!FC = nagfor
         RP=node%r_position!FC = gfortran
         DP=node%d_position!FC = gfortran
         RV=node%values(dim,r_value_index)!FC = gfortran
         DV=node%values(dim,d_value_index)!FC = gfortran
         RI=node%values(dim,r_integral_index)!FC = gfortran
         if (present(position)) then
            DINT=(ri-int)*2D0*dv/dp
            position=rp-(dp/dv)*(rv-sqrt(dint+rv**2))
         end if
         if (present(value)) then
            value=Sqrt(dp*(-2*dv*int + 2*dv*ri + dp*rv**2))/dp
         end if
         if (present(integral)) then
            integral=int
         end if
         if (present(content)) then
            content=>node
         end if
!       end associate!FC = nagfor
    else
       in_range=.false.
    end if
  end subroutine muli_trapezium_tree_approx_by_integral

  subroutine muli_trapezium_tree_approx_by_propability(this,prop,dim,in_range,position,value,integral,content)
    class(muli_trapezium_tree_type),intent(in),target :: this
    real(kind=double),intent(in) :: prop
    integer,intent(in)::dim
    logical,intent(out) :: in_range
    class(muli_trapezium_node_class),pointer,intent(out),optional :: content
    real(kind=double),intent(out),optional :: position,value,integral
    integer::i
    real(kind=double) :: INT,DINT,l_prop,r_prop,d_prop
    class(muli_trapezium_node_class),pointer :: node
    if(0D0<prop.and.prop<1D0)then
       node=>this
       INT=-log(prop)
       call muli_trapezium_tree_approx_by_integral(this,int,dim,in_range,position,value,integral,content)
    else
       in_range=.false.
    end if
  end subroutine muli_trapezium_tree_approx_by_propability

  subroutine muli_trapezium_tree_to_tree(this,out_tree)
    class(muli_trapezium_tree_type),intent(in) :: this
    class(muli_trapezium_tree_type),intent(out) :: out_tree
    out_tree%left=>this%left
    out_tree%right=>this%right
    out_tree%down=>this%down
  end subroutine muli_trapezium_tree_to_tree

  subroutine muli_trapezium_tree_append(this,right)
    class(muli_trapezium_tree_type),intent(inout),target :: this
    class(muli_trapezium_node_class),intent(inout),target :: right
    print ('("muli_trapezium_tree_append: Not yet implemented.")')
  end subroutine muli_trapezium_tree_append

  subroutine muli_trapezium_tree_gnuplot(this,dir)
    class(muli_trapezium_tree_type),intent(in) :: this
    character(len=*),intent(in)::dir
    class(muli_trapezium_list_type),pointer::list
    call this%get_left_list(list)
    call list%gnuplot(dir)
  end subroutine muli_trapezium_tree_gnuplot
  
end module muli_trapezium

