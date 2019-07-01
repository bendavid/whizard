
module muli_trapezium

  use kinds, only: default
  use constants
  use diagnostics
  use muli_base

  implicit none
  private  
  
  integer, private, parameter :: value_dimension = 7
  integer, private, parameter :: r_value_index = 1
  integer, private, parameter :: d_value_index = 2
  integer, private, parameter :: r_integral_index = 3
  integer, private, parameter :: d_integral_index = 4
  integer, private, parameter :: r_probability_index = 5
  integer, private, parameter :: d_probability_index = 6
  integer, private, parameter :: error_index = 7  


  public :: muli_trapezium_t
  public :: muli_trapezium_node_class_t 
  public :: muli_trapezium_tree_t
  public :: muli_trapezium_list_t

  type, extends (measure_class_t) :: muli_trapezium_t
     private
     integer::dim=0
     real(default) :: r_position = 0
     real(default) :: d_position = 0
     real(default) :: measure_comp = 0
     real(default), dimension(:,:), allocatable :: values
   contains
     procedure :: write_to_marker => muli_trapezium_write_to_marker
     procedure :: read_from_marker => muli_trapezium_read_from_marker  
     procedure :: print_to_unit => muli_trapezium_print_to_unit  
     procedure, nopass :: get_type => muli_trapezium_get_type  
     procedure, nopass :: verify_type => muli_trapezium_verify_type
     procedure :: measure => muli_trapezium_measure
     procedure :: initialize => muli_trapezium_initialize  
     procedure :: get_dimension => muli_trapezium_get_dimension  
     procedure :: get_l_position => muli_trapezium_get_l_position
     procedure :: get_r_position => muli_trapezium_get_r_position  
     procedure :: get_d_position => muli_trapezium_get_d_position  
     generic :: get_l_value => get_l_value_array, get_l_value_element
     procedure :: get_l_value_array => muli_trapezium_get_l_value_array
     procedure :: get_l_value_element => muli_trapezium_get_l_value_element    
     generic :: get_r_value => get_r_value_array, get_r_value_element  
     procedure :: get_r_value_array => muli_trapezium_get_r_value_array
     procedure :: get_r_value_element => muli_trapezium_get_r_value_element  
     generic :: get_d_value => get_d_value_array, get_d_value_element  
     procedure :: get_d_value_array => muli_trapezium_get_d_value_array
     procedure :: get_d_value_element => muli_trapezium_get_d_value_element  
     generic :: get_l_integral => get_l_integral_array, get_l_integral_element  
     procedure :: get_l_integral_array => muli_trapezium_get_l_integral_array
     procedure :: get_l_integral_element => muli_trapezium_get_l_integral_element  
     generic :: get_r_integral => get_r_integral_array, get_r_integral_element  
     procedure :: get_r_integral_array => muli_trapezium_get_r_integral_array
     procedure :: get_r_integral_element => muli_trapezium_get_r_integral_element   
     generic :: get_d_integral => get_d_integral_array, get_d_integral_element  
     procedure :: get_d_integral_array => muli_trapezium_get_d_integral_array
     procedure :: get_d_integral_element => muli_trapezium_get_d_integral_element
     generic :: get_l_probability => &
          get_l_probability_array, get_l_probability_element  
     procedure :: get_l_probability_element => &
          muli_trapezium_get_l_probability_element
     procedure :: get_l_probability_array => &
          muli_trapezium_get_l_probability_array  
     generic :: get_r_probability => &
          get_r_probability_array, get_r_probability_element  
     procedure :: get_r_probability_element => &
          muli_trapezium_get_r_probability_element
     procedure :: get_r_probability_array => &
          muli_trapezium_get_r_probability_array  
     generic :: get_d_probability => &
          get_d_probability_array, get_d_probability_element  
     procedure :: get_d_probability_element => &
          muli_trapezium_get_d_probability_element
     procedure :: get_d_probability_array => &
          muli_trapezium_get_d_probability_array  
     procedure :: get_error => muli_trapezium_get_error
     procedure :: get_error_sum => muli_trapezium_get_error_sum
     procedure :: get_integral_sum => muli_trapezium_get_integral_sum
     procedure :: get_value_at_position => muli_trapezium_get_value_at_position  
     procedure :: set_r_value => muli_trapezium_set_r_value
     procedure :: set_d_value => muli_trapezium_set_d_value
     procedure :: set_r_integral => muli_trapezium_set_r_integral
     procedure :: set_d_integral => muli_trapezium_set_d_integral
     procedure :: set_r_probability => muli_trapezium_set_r_probability
     procedure :: set_d_probability => muli_trapezium_set_d_probability
     procedure :: set_error => muli_trapezium_set_error  
     procedure :: is_left_of => muli_trapezium_is_left_of
     procedure :: includes => muli_trapezium_includes
     procedure :: to_node => muli_trapezium_to_node
     procedure :: sum_up => muli_trapezium_sum_up  
     procedure :: approx_value => muli_trapezium_approx_value
     procedure :: approx_value_n => muli_trapezium_approx_value_n
     procedure :: approx_integral => muli_trapezium_approx_integral
     procedure :: approx_integral_n => muli_trapezium_approx_integral_n
     procedure :: approx_probability => muli_trapezium_approx_probability
     procedure :: approx_probability_n => muli_trapezium_approx_probability_n
     procedure :: approx_position_by_integral => &
          muli_trapezium_approx_position_by_integral
     ! procedure :: choose_partons => muli_trapezium_choose_partons
     procedure :: split => muli_trapezium_split
     procedure :: update => muli_trapezium_update    
  end type muli_trapezium_t  
  
  type, extends (muli_trapezium_t), abstract :: muli_trapezium_node_class_t
     private
     class(muli_trapezium_node_class_t), pointer :: left => null()
     class(muli_trapezium_node_class_t), pointer :: right => null()
     ! real(default) :: criterion
   contains
     procedure :: deserialize_from_marker => &
          muli_trapezium_node_deserialize_from_marker  
     procedure(muli_trapezium_append_interface), deferred :: append  
     procedure(muli_trapezium_final_interface), deferred :: finalize
     procedure :: nullify => muli_trapezium_node_nullify
     procedure :: get_left => muli_trapezium_node_get_left
     procedure :: get_right => muli_trapezium_node_get_right
     procedure :: get_leftmost => muli_trapezium_node_get_leftmost
     procedure :: get_rightmost => muli_trapezium_node_get_rightmost
     generic :: decide => decide_by_value, decide_by_position  
     procedure :: decide_by_value => muli_trapezium_node_decide_by_value  
     procedure :: decide_by_position => muli_trapezium_node_decide_by_position
     procedure :: decide_decreasing => muli_trapezium_node_decide_decreasing
     procedure :: to_tree => muli_trapezium_node_to_tree
     procedure :: untangle => muli_trapezium_node_untangle
     procedure :: apply => muli_trapezium_node_apply 
     ! procedure :: copy => muli_trapezium_node_copy
     ! generic :: assignment(=) => copy
     ! procedure, deferred :: approx => muli_trapezium_node_approx
  end type muli_trapezium_node_class_t
  
  type, extends(muli_trapezium_node_class_t) :: muli_trapezium_tree_t
     class(muli_trapezium_node_class_t), pointer :: down => null()
   contains
     procedure :: write_to_marker => muli_trapezium_tree_write_to_marker
     procedure :: read_from_marker => muli_trapezium_tree_read_from_marker  
     procedure :: print_to_unit => muli_trapezium_tree_print_to_unit    
     procedure, nopass :: get_type => muli_trapezium_tree_get_type
     procedure, nopass :: verify_type => muli_trapezium_tree_verify_type  
     procedure :: nullify => muli_trapezium_tree_nullify  
     procedure :: finalize => muli_trapezium_tree_finalize
     procedure :: decide_by_value => muli_trapezium_tree_decide_by_value    
     procedure :: decide_by_position => muli_trapezium_tree_decide_by_position
     procedure :: decide_decreasing => muli_trapezium_tree_decide_decreasing
     procedure :: get_left_list => muli_trapezium_tree_get_left_list
     procedure :: get_right_list => muli_trapezium_tree_get_right_list  
     generic :: find => find_by_value, find_by_position  
     procedure :: find_by_value => muli_trapezium_tree_find_by_value
     procedure :: find_by_position => muli_trapezium_tree_find_by_position  
     procedure :: find_decreasing => muli_trapezium_tree_find_decreasing
     procedure :: approx_by_integral => muli_trapezium_tree_approx_by_integral
     procedure :: approx_by_probability => muli_trapezium_tree_approx_by_probability
     procedure :: to_tree => muli_trapezium_tree_to_tree  
     procedure :: append => muli_trapezium_tree_append    
     procedure :: gnuplot => muli_trapezium_tree_gnuplot
  end type muli_trapezium_tree_t
  
  type, extends (muli_trapezium_node_class_t) :: muli_trapezium_list_t
   contains
     procedure :: append => muli_trapezium_list_append  
     procedure :: write_to_marker => muli_trapezium_list_write_to_marker
     procedure :: read_from_marker => muli_trapezium_list_read_from_marker  
     procedure :: read_target_from_marker => &
          muli_trapezium_list_read_target_from_marker  
     procedure :: print_to_unit => muli_trapezium_list_print_to_unit
     procedure, nopass :: get_type => muli_trapezium_list_get_type
     procedure, nopass :: verify_type => muli_trapezium_list_verify_type
     procedure :: finalize => muli_trapezium_list_finalize
     generic :: insert_right => insert_right_a   !, insert_right_b    
     procedure :: insert_right_a => muli_trapezium_list_insert_right_a
     ! procedure :: insert_right_b => muli_trapezium_list_insert_right_b  
     generic :: insert_left => insert_left_a   !, insert_left_b  
     procedure :: insert_left_a => muli_trapezium_list_insert_left_a
     ! procedure :: insert_left_b => muli_trapezium_list_insert_left_b  
     procedure :: to_tree => muli_trapezium_list_to_tree
     procedure :: gnuplot => muli_trapezium_list_gnuplot  
     procedure :: integrate => muli_trapezium_list_integrate  
     procedure :: check => muli_trapezium_list_check  
     procedure :: apply => muli_trapezium_list_apply  
  end type muli_trapezium_list_t
  

  abstract interface
     subroutine muli_trapezium_append_interface (this, right)
       import muli_trapezium_node_class_t
       class(muli_trapezium_node_class_t), intent(inout), target :: this, right
     end subroutine muli_trapezium_append_interface
  end interface  
  abstract interface
     subroutine muli_trapezium_final_interface (this)
       import muli_trapezium_node_class_t
       class(muli_trapezium_node_class_t), intent(inout) :: this
     end subroutine muli_trapezium_final_interface
  end interface
  

contains
  
    subroutine muli_trapezium_write_to_marker (this,marker,status)
    class(muli_trapezium_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer::dim
    call marker%mark_begin ("muli_trapezium_t")
    call marker%mark ("dim", this%dim)
    call marker%mark ("r_position", this%r_position)
    call marker%mark ("d_position", this%d_position)
    if (allocated(this%values)) then
       call marker%mark ("values", this%values)
    else
       call marker%mark_null ("values")
    end if
    call marker%mark_end ("muli_trapezium_t")
  end subroutine muli_trapezium_write_to_marker
  
  subroutine muli_trapezium_read_from_marker (this,marker,status)
    class(muli_trapezium_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer :: dim
    call marker%pick_begin ("muli_trapezium_t", status=status)
    call marker%pick ("dim", this%dim,status)
    call marker%pick ("r_position", this%r_position, status)
    call marker%pick ("d_position", this%d_position, status)
    if (allocated (this%values))  deallocate (this%values)
    call marker%verify_nothing ("values", status)
    if (status == serialize_ok) then
       allocate(this%values(0:this%dim-1,7))
       call marker%pick ("values",this%values, status)
    end if
    call marker%pick_end("muli_trapezium_t",status)
  end subroutine muli_trapezium_read_from_marker
  
  subroutine muli_trapezium_print_to_unit (this, unit, parents, components, peers)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    write (unit, "(1x,A)") "Components of muli_trapezium_t:"
    write (unit, fmt=*)"Dimension:        ",this%dim
    write (unit,fmt=*)"Right position:   ",this%r_position
    write (unit,fmt=*)"Position step:    ",this%d_position
    if (allocated(this%values)) then    
       if (components>0) then          
          write (unit,fmt=*)"Right values:     ",muli_trapezium_get_r_value_array(this)
          write (unit,fmt=*) "Value step:       ", this%get_d_value()
          write (unit,fmt=*)"Right integrals:  ",this%get_r_integral()
          write (unit,fmt=*)"Integral step:    ",this%get_d_integral()
          write (unit,fmt=*)"Right propabilities:",this%get_r_probability()
          write (unit,fmt=*)"Probability step: ",this%get_d_probability()
          write (unit,fmt=*)"Errors:           ",this%get_error()
       else
          write (unit, "(3x,A)") "Values are allocated."
       end if
    else
       write (unit, "(3x,A)") "Values are not allocated."
    end if
  end subroutine muli_trapezium_print_to_unit  

  pure subroutine muli_trapezium_get_type (type)
    character(:),allocatable, intent(out) :: type
    allocate (type, source="muli_trapezium_t")
  end subroutine muli_trapezium_get_type
  
  elemental logical function muli_trapezium_verify_type (type) result (match)
    character(*), intent(in) :: type
    match = type == "muli_trapezium_t"
  end function muli_trapezium_verify_type
  
  elemental function muli_trapezium_measure (this)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: muli_trapezium_measure
    muli_trapezium_measure = this%measure_comp
  end function muli_trapezium_measure
  
  subroutine muli_trapezium_initialize (this, dim, r_position, d_position)
    class(muli_trapezium_t), intent(inout) :: this
    integer, intent(in) :: dim
    real(default), intent(in) :: r_position, d_position
    integer :: dim1, dim2
    this%dim = dim
    this%r_position = r_position
    this%d_position = d_position
    if (allocated (this%values))  deallocate (this%values)
    allocate (this%values(0:dim-1,value_dimension))
    do dim2 = 1, value_dimension-1
       do dim1 = 0, dim-1
          this%values(dim1,dim2) = zero
       end do       
    end do
    do dim1 = 0, dim-1
       this%values(dim1, value_dimension) = huge(one)
    end do
    this%measure_comp = huge(one)
  end subroutine muli_trapezium_initialize

  elemental function muli_trapezium_get_dimension (this) result (dim)
    class(muli_trapezium_t), intent(in) :: this
    integer :: dim
    dim = this%dim
  end function muli_trapezium_get_dimension
  
  pure function muli_trapezium_get_l_position (this) result (pos)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: pos
    pos = this%r_position - this%d_position
  end function muli_trapezium_get_l_position
  
  pure function muli_trapezium_get_r_position (this) result (pos)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: pos
    pos = this%r_position
  end function muli_trapezium_get_r_position
  
  pure function muli_trapezium_get_d_position (this) result (pos)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: pos
    pos = this%d_position
  end function muli_trapezium_get_d_position
  
  pure function muli_trapezium_get_l_value_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values(0:this%dim-1, r_value_index) - &
         this%values(0:this%dim-1, d_value_index)
  end function muli_trapezium_get_l_value_array
  
  pure function muli_trapezium_get_l_value_element (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values(set, r_value_index) - this%values(set, d_value_index)
  end function muli_trapezium_get_l_value_element
  
  pure function muli_trapezium_get_r_value_element (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, r_value_index)
  end function muli_trapezium_get_r_value_element

  pure function muli_trapezium_get_r_value_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values(0:this%dim-1, r_value_index)
  end function muli_trapezium_get_r_value_array
  
  pure function muli_trapezium_get_d_value_element (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element=this%values (set, d_value_index)
  end function muli_trapezium_get_d_value_element

  pure function muli_trapezium_get_d_value_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values(0:this%dim-1, d_value_index)
  end function muli_trapezium_get_d_value_array
  
  pure function muli_trapezium_get_l_integral_element &
       (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, r_integral_index) - &
         this%values (set, d_integral_index)
  end function muli_trapezium_get_l_integral_element

  pure function muli_trapezium_get_l_integral_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, r_integral_index) - &
         this%values (0:this%dim-1, d_integral_index)
  end function muli_trapezium_get_l_integral_array

  pure function muli_trapezium_get_r_integral_element (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, r_integral_index)
  end function muli_trapezium_get_r_integral_element

  pure function muli_trapezium_get_r_integral_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, r_integral_index)
  end function muli_trapezium_get_r_integral_array
  
  pure function muli_trapezium_get_d_integral_element & 
       (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, d_integral_index)
  end function muli_trapezium_get_d_integral_element

  pure function muli_trapezium_get_d_integral_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, d_integral_index)
  end function muli_trapezium_get_d_integral_array
  
  pure function muli_trapezium_get_l_probability_element &
       (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, r_probability_index) - &
         this%values (set, d_probability_index)
  end function muli_trapezium_get_l_probability_element

  pure function muli_trapezium_get_l_probability_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, r_probability_index) - &
         this%values (0:this%dim-1, d_probability_index)
  end function muli_trapezium_get_l_probability_array

  pure function muli_trapezium_get_r_probability_element &
       (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, r_probability_index)
  end function muli_trapezium_get_r_probability_element

  pure function muli_trapezium_get_r_probability_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, r_probability_index)
  end function muli_trapezium_get_r_probability_array
  
  pure function muli_trapezium_get_d_probability_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: subarray
    subarray = this%values (0:this%dim-1, d_probability_index)
  end function muli_trapezium_get_d_probability_array

  pure function muli_trapezium_get_d_probability_element &
       (this, set) result (element)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: set
    real(default) :: element
    element = this%values (set, d_probability_index)
  end function muli_trapezium_get_d_probability_element

  pure function muli_trapezium_get_error_sum (this) result (error)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: error
    error = sum (this%values (0:this%dim-1, error_index))
  end function muli_trapezium_get_error_sum

  pure function muli_trapezium_get_error (this) result (error)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: error
    error = this%values (0:this%dim-1, error_index)
  end function muli_trapezium_get_error
  
  pure function muli_trapezium_get_integral_sum (this) result (error)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: error
    error = sum (this%values (0:this%dim-1, d_integral_index))
  end function muli_trapezium_get_integral_sum
  
  subroutine muli_trapezium_get_value_at_position (this, pos, subarray)
    class(muli_trapezium_t), intent(in) :: this
    real(default), intent(in) :: pos
    real(default), dimension(this%dim), intent(out) :: subarray
    subarray = this%get_r_value_array() - this%get_d_value() * &
         this%d_position / (this%r_position-pos)
  end subroutine muli_trapezium_get_value_at_position

  subroutine muli_trapezium_set_r_value (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values(0:this%dim-1, r_value_index) = subarray
  end subroutine muli_trapezium_set_r_value

  subroutine muli_trapezium_set_d_value (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values(0:this%dim-1,d_value_index) = subarray
  end subroutine muli_trapezium_set_d_value

  subroutine muli_trapezium_set_r_integral (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values(0:this%dim-1,r_integral_index) = subarray
  end subroutine muli_trapezium_set_r_integral

  subroutine muli_trapezium_set_d_integral (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values (0:this%dim-1, d_integral_index) = subarray
  end subroutine muli_trapezium_set_d_integral

  subroutine muli_trapezium_set_r_probability (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values (0:this%dim-1,r_probability_index) = subarray
  end subroutine muli_trapezium_set_r_probability
  
  subroutine muli_trapezium_set_d_probability (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values (0:this%dim-1,d_probability_index) = subarray
  end subroutine muli_trapezium_set_d_probability
  
  subroutine muli_trapezium_set_error (this, subarray)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in), dimension(0:this%dim-1) :: subarray
    this%values (0:this%dim-1, error_index) = subarray
    this%measure_comp = sum (subarray)
  end subroutine muli_trapezium_set_error
  
  pure function muli_trapezium_is_left_of (this, that) result (is_left)
    logical :: is_left
    class(muli_trapezium_t), intent(in) :: this, that
    is_left = this%r_position <= that%r_position   !-that%d_position
    ! if (is_left.and.that%r_position < this%r_position) then
    !    print *,"!"
    !    STOP
    ! end if
  end function muli_trapezium_is_left_of

  elemental logical function muli_trapezium_includes &
       (this, dim, position, value, integral, probability) result (includes)
    class(muli_trapezium_t), intent(in) :: this
    integer, intent(in) :: dim
    real(default), intent(in),optional :: position, value, integral, probability
    includes = .true.
    if (present (position)) then
       if (this%get_l_position() > position .or. &
           position >= this%get_r_position())  includes = .false.
    end if
    if (present (value)) then
       if (this%get_l_value(dim) > value .or. value >= &
            this%get_r_value(dim))  includes = .false.
    end if
    if (present (integral)) then
       if (this%get_l_integral(dim) > integral .or. integral >= &
            this%get_r_integral(dim))  includes = .false.
    end if
    if (present (probability)) then
       if (this%get_l_probability(dim) > probability .or. &
            probability >= this%get_r_probability(dim))  includes = .false.
    end if    
  end function muli_trapezium_includes

  subroutine muli_trapezium_to_node (this, value, list, tree)
    class(muli_trapezium_t), intent(in) :: this
    real(default), intent(in) :: value  
    ! class(muli_trapezium_node_class_t), optional, pointer, intent(out) :: node
    class(muli_trapezium_list_t), optional, pointer, intent(out) :: list
    class(muli_trapezium_tree_t), optional, pointer, intent(out) :: tree
    ! if (present (node)) then
    !    allocate (node)
    !    node%dim = this%dim
    !    node%r_position = this%r_position
    !    node%d_position = this%d_position
    !    allocate (node%values (this%dim, value_dimension), source=this%values)
    ! end if
    if (present (list)) then
       allocate (list)
       list%dim = this%dim
       list%r_position = this%r_position
       list%d_position = this%d_position
       allocate (list%values (0:this%dim-1, value_dimension), source=this%values)
    end if
    if (present (tree)) then
       allocate (tree)
       tree%dim = this%dim
       tree%r_position = this%r_position
       tree%d_position = this%d_position
       allocate (tree%values (0:this%dim-1, value_dimension), source=this%values)
    end if    
  end subroutine muli_trapezium_to_node

  subroutine muli_trapezium_sum_up (this)
    class(muli_trapezium_t), intent(inout) :: this
    integer :: i
    if (allocated (this%values)) then
       do i = 1, 7
          this%values (0,i) = sum (this%values (1:this%dim-1,i))
       end do
    end if
  end subroutine muli_trapezium_sum_up
  
  pure function muli_trapezium_approx_value (this, x) result (val)
    ! returns the values at x
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: val
    real(default), intent(in) :: x
    val = this%get_r_value_array() + (x - this%r_position) * &
         this%get_d_value() / this%d_position
  end function muli_trapezium_approx_value

  elemental function muli_trapezium_approx_value_n (this, x, n) result (val)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: val
    real(default), intent(in) :: x
    integer, intent(in) :: n
    val = this%get_r_value_element(n) + (x - this%r_position) * &
         this%get_d_value_element(n) / this%d_position
  end function muli_trapezium_approx_value_n

  pure function muli_trapezium_approx_integral (this, x)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: muli_trapezium_approx_integral
    real(default), intent(in) :: x
    muli_trapezium_approx_integral = &
         ! this%get_r_integral()+&
         ! (this%r_position-x)*this%get_r_value()+&
         ! (x**2-this%r_position**2)*this%get_d_integral()/(this%d_position*2D0)
         this%get_r_integral() + &
         ((this%r_position - x) * &
         (-this%get_d_value() * (this%r_position - x) + 2 * &
         this%d_position*this%get_r_value_array())) / &
         (2 * this%d_position)
  end function muli_trapezium_approx_integral
  
  elemental function muli_trapezium_approx_integral_n (this, x, n) result (val)    
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: val
    real(default), intent(in) :: x
    integer, intent(in) :: n
    val = this%get_r_integral_element (n) + ((this%r_position - x) * &
         (-this%get_d_value_element (n) * (this%r_position - x) + 2 * &
         this%d_position * this%get_r_value_element (n))) / &
         (2 * this%d_position)
  end function muli_trapezium_approx_integral_n
    
   pure function muli_trapezium_approx_probability (this, x) result (prop)
    class(muli_trapezium_t), intent(in) :: this
    real(default), dimension(this%dim) :: prop
    real(default), intent(in) :: x
    prop = exp (- this%approx_integral (x))
  end function muli_trapezium_approx_probability

  elemental function muli_trapezium_approx_probability_n (this, x, n) result (val)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: val
    real(default), intent(in) :: x
    integer, intent(in) :: n
    val = exp (- this%approx_integral_n (x, n))
  end function muli_trapezium_approx_probability_n
     
  elemental function muli_trapezium_approx_position_by_integral &
       (this, dim, int) result (val)
    class(muli_trapezium_t), intent(in) :: this
    real(default) :: val
    integer, intent(in) :: dim
    real(default), intent(in) :: int
    real(default) :: dpdv    
    dpdv = (this%d_position / this%values (dim,d_value_index))
    val = this%r_position - dpdv * (this%values (dim, r_value_index) - &
           sqrt (((this%values (dim, r_integral_index) - int) * two / dpdv) + &
           this%values (dim, r_value_index)**2))
  end function muli_trapezium_approx_position_by_integral
  
  subroutine muli_trapezium_split (this, c_value, c_position, new_node)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), intent(in) :: c_position
    real(default), intent(in), dimension(this%dim) :: c_value
    class(muli_trapezium_t), intent(out), pointer :: new_node
    real(default) :: ndpr, ndpl
    real(default), dimension(:), allocatable :: ov, edv
    ndpr = this%r_position - c_position
    ndpl = this%d_position - ndpr
    allocate (ov (0:this%dim-1), source=this%get_r_value_array() - ndpr * &
         this%get_d_value() / this%d_position)
    allocate (edv (0:this%dim-1), source=c_value-ov)
    allocate (new_node)
    call new_node%initialize (dim=this%dim, r_position=c_position, &
         d_position=ndpl)
    call new_node%set_r_value (c_value)
    call new_node%set_d_value (this%get_d_value() + &
         c_value-this%get_r_value_array())
    call new_node%set_d_integral (ndpl*(this%get_d_value() - &
         this%get_r_value_array() - c_value) / two)
    call new_node%set_error (abs((edv*ndpl) / two))
    ! new_node%measure_comp = sum (abs((edv*ndpl) / two))
    this%d_position = ndpr
    call this%set_d_value (this%get_r_value_array() - c_value)
    call this%set_d_integral (- (ndpr*(this%get_r_value_array() + c_value) / two))
    call this%set_error (abs(edv*ndpr / two))
    ! this%measure_comp = sum (abs(edv*ndpr / two))
    ! write (*, "(1x,A)") "muli_trapezium_split: new errors:"
    ! write (*, "(3x,ES14.7)")  this%get_error()
    ! write (*, "(3x,ES14.7)")  new_node%get_error()
    ! write (*, "(3x,11(ES20.10)")  new_node%get_d_integral()
    ! write (*, "(3x,11(ES20.10)")  this%get_d_integral()
  end subroutine muli_trapezium_split
  
  subroutine muli_trapezium_update (this)
    class(muli_trapezium_t), intent(inout) :: this
    real(default), dimension(:), allocatable :: integral
    allocate (integral (0:this%dim-1), source=this%get_d_integral())
    call this%set_d_integral (-this%d_position * (this%get_r_value_array() &
         - this%get_d_value() / 2))
    call this%set_error (abs (this%get_d_integral() - integral))
    ! write (*, "(3x,11(ES20.10)")  this%get_d_integral()
  end subroutine muli_trapezium_update
  
  subroutine muli_trapezium_node_deserialize_from_marker (this, name, marker)
    class(muli_trapezium_node_class_t), intent(out) :: this
    character(*), intent(in) :: name
    class(marker_t), intent(inout) :: marker
    integer(dik) :: status
    class(ser_class_t), pointer :: ser
    allocate (muli_trapezium_tree_t :: ser)
    call marker%push_reference (ser)
    allocate (muli_trapezium_list_t::ser)
    call marker%push_reference (ser)
    call serializable_deserialize_from_marker (this, name, marker)
    call marker%pop_reference (ser)
    deallocate (ser)
    call marker%pop_reference (ser)
    deallocate (ser)
  end subroutine muli_trapezium_node_deserialize_from_marker
  
  subroutine muli_trapezium_list_append (this, right)
    class(muli_trapezium_list_t), intent(inout), target :: this
    class(muli_trapezium_node_class_t), intent(inout), target :: right
    this%right => right
    right%left => this
  end subroutine muli_trapezium_list_append

  subroutine muli_trapezium_node_nullify (this)
    class(muli_trapezium_node_class_t), intent(out) :: this
    nullify (this%left)
    nullify (this%right)
  end subroutine muli_trapezium_node_nullify
  
  subroutine muli_trapezium_node_get_left (this, left)
    class(muli_trapezium_node_class_t), intent(in) :: this
    class(muli_trapezium_node_class_t), pointer, intent(out) :: left
    left => this%left
  end subroutine muli_trapezium_node_get_left
  
  subroutine muli_trapezium_node_get_right (this, right)
    class(muli_trapezium_node_class_t), intent(in) :: this
    class(muli_trapezium_node_class_t), pointer, intent(out) :: right
    right => this%right
  end subroutine muli_trapezium_node_get_right
  
  subroutine muli_trapezium_node_get_leftmost (this, node)
    class(muli_trapezium_node_class_t), intent(in) :: this
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (associated (this%left)) then
       node => this%left
       do while (associated (node%left))
          node => node%left
       end do
    else
       nullify (node)
    end if
  end subroutine muli_trapezium_node_get_leftmost
  
  subroutine muli_trapezium_node_get_rightmost (this, right)
    class(muli_trapezium_node_class_t), intent(in) :: this
    class(muli_trapezium_node_class_t), pointer, intent(out) :: right
    if (associated (this%right)) then
       right => this%right
       do while (associated (right%right))
          right => right%right
       end do
    else
       nullify (right)
    end if
  end subroutine muli_trapezium_node_get_rightmost
  
  subroutine muli_trapezium_node_decide_by_value (this, value, dim, record, node)
    class(muli_trapezium_node_class_t), intent(in) :: this
    real(default), intent(in) :: value
    integer, intent(in) :: record, dim
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (this%values (dim, record) > value) then
       node => this%left
    else
       node => this%right
    end if
  end subroutine muli_trapezium_node_decide_by_value

  subroutine muli_trapezium_node_decide_by_position (this, position, node)
    class(muli_trapezium_node_class_t), intent(in) :: this
    real(default), intent(in) :: position
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (this%r_position > position) then
       node => this%left
    else
       node => this%right
    end if
  end subroutine muli_trapezium_node_decide_by_position
  
  subroutine muli_trapezium_node_decide_decreasing &
       (this, value, dim, record, node)
    class(muli_trapezium_node_class_t), intent(in) :: this
    real(default), intent(in) :: value
    integer, intent(in) :: record, dim
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (this%values (dim, record) <= value) then
       node => this%left
    else
       node => this%right
    end if
  end subroutine muli_trapezium_node_decide_decreasing
  
  subroutine muli_trapezium_node_to_tree (this, out_tree)
    class(muli_trapezium_node_class_t), target, intent(in) :: this
    class(muli_trapezium_tree_t), intent(out) :: out_tree
    out_tree%left => this%left
    out_tree%right => this%right
  end subroutine muli_trapezium_node_to_tree
  
  subroutine muli_trapezium_node_untangle(this)
    class(muli_trapezium_node_class_t), intent(inout), target :: this
    if (associated (this%left)) then
       if (associated (this%left%right, this)) then
          nullify (this%left%right)
          nullify (this%left)
       end if
    end if    
  end subroutine muli_trapezium_node_untangle

  recursive subroutine muli_trapezium_node_apply(this,proc)
    class(muli_trapezium_node_class_t), intent(inout) :: this
    interface
       subroutine proc(this)
         import muli_trapezium_node_class_t
         class(muli_trapezium_node_class_t), intent(inout) :: this
       end subroutine proc
    end interface
    if (associated(this%right))call proc(this%right)
    if (associated(this%left))call proc(this%left)
    call proc(this)
  end subroutine muli_trapezium_node_apply
 
  subroutine muli_trapezium_tree_write_to_marker (this, marker, status)
    class(muli_trapezium_tree_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(muli_trapezium_list_t), pointer :: list
    class(ser_class_t), pointer :: ser
    call marker%mark_begin ("muli_trapezium_tree_t")
    call this%get_left_list (list)
    ser => list
    call marker%mark_pointer ("list", ser)
    call marker%mark_end ("muli_trapezium_tree_t")
  end subroutine muli_trapezium_tree_write_to_marker

  subroutine muli_trapezium_tree_read_from_marker (this, marker, status)
    class(muli_trapezium_tree_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(ser_class_t), pointer :: ser
    call marker%pick_begin ("muli_trapezium_tree_t", status=status)
    call marker%pick_pointer ("list", ser)
    if (associated (ser)) then
       select type (ser)
       class is (muli_trapezium_list_t)
          call ser%to_tree (this)
       class default
          nullify (this%left)
          nullify (this%right)
          nullify (this%down)
       end select
    else
       nullify (this%left)
       nullify (this%right)
       nullify (this%down)
    end if
    call marker%pick_end ("muli_trapezium_tree_t", status)
  end subroutine muli_trapezium_tree_read_from_marker

  recursive subroutine muli_trapezium_tree_print_to_unit &
       (this, unit, parents, components, peers)
    class(muli_trapezium_tree_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    class(ser_class_t), pointer :: ser
    if (parents > 0)  call muli_trapezium_print_to_unit &
         (this, unit, parents-1, components, peers)
    ser => this%down
    call serialize_print_peer_pointer (ser, unit, i_one, i_zero, i_one, "DOWN")
    if (associated (this%left)) then
       select type (sertmp => this%left)
       class is (muli_trapezium_list_t)
          ser => sertmp
          call serialize_print_peer_pointer &
               (ser, unit, parents, components, i_zero, "LEFT")
       class default
          call serialize_print_peer_pointer &
               (ser, unit, parents, components, peers, "LEFT")
       end select
    else
       write (unit, "(1x,A)")  "Left is not associated."
    end if
    if (associated (this%right)) then
       select type (sertmp => this%right)
       class is (muli_trapezium_list_t)
          ser => sertmp
          call serialize_print_peer_pointer &
               (ser, unit, parents, components, i_zero, "RIGHT")
       class default
          call serialize_print_peer_pointer &
               (ser, unit, parents, components, peers, "RIGHT")
       end select
    else
       write (unit, "(1x,A)")  "Right is not associated."
    end if
  end subroutine muli_trapezium_tree_print_to_unit
  
  pure subroutine muli_trapezium_tree_get_type (type)
    character(:),allocatable, intent(out) :: type
    allocate (type, source="muli_trapezium_tree_t")
  end subroutine muli_trapezium_tree_get_type
  
  elemental logical function muli_trapezium_tree_verify_type (type) result (match)
    character(*), intent(in) :: type
    match = type == "muli_trapezium_tree_t"
  end function muli_trapezium_tree_verify_type
  
  subroutine muli_trapezium_tree_nullify (this)
    class(muli_trapezium_tree_t), intent(out) :: this
    call muli_trapezium_node_nullify (this)
    nullify (this%down)
  end subroutine muli_trapezium_tree_nullify
    
  recursive subroutine muli_trapezium_tree_finalize (this)
    class(muli_trapezium_tree_t), intent(inout) :: this
    if (associated (this%right)) then
       call this%right%untangle ()
       call this%right%finalize ()
       deallocate (this%right)
    end if
    if (associated (this%left)) then
       call this%left%untangle ()
       call this%left%finalize ()
       deallocate (this%left)
    end if
    this%dim = 0 
  end subroutine muli_trapezium_tree_finalize
  
  subroutine muli_trapezium_tree_decide_by_value (this, value, dim, record, node)
    class(muli_trapezium_tree_t), intent(in) :: this
    real(default), intent(in) :: value
    integer, intent(in) :: record, dim
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (this%down%values (dim, record) > value) then
       node => this%left
    else
       node => this%right
    end if
  end subroutine muli_trapezium_tree_decide_by_value
  
  subroutine muli_trapezium_tree_decide_by_position (this, position, node)
    class(muli_trapezium_tree_t), intent(in) :: this
    real(default), intent(in) :: position
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    if (this%down%r_position > position) then
       node => this%left
    else
       node => this%right
    end if
  end subroutine muli_trapezium_tree_decide_by_position
  
  subroutine muli_trapezium_tree_decide_decreasing &
       (this, value, dim, record, node)
    class(muli_trapezium_tree_t), intent(in) :: this
    real(default), intent(in) :: value
    integer, intent(in) :: record, dim
    ! integer, save :: count=0
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    ! count = count + 1
    if (this%down%values (dim, record) <= value) then
       ! print ('("Decide: value(",I2,",",I1,")=",E20.7," > ",E20.7, &
       !     ": go left.")'), dim, record, this%down%values(dim, record), value
       node => this%left
    else
       ! print ('("Decide: value(",I2,",",I1,")=",E20.7," <= ", &
       !   E20.7,": go right.")'), &
       !   dim, record, this%down%values(dim, record), value
       node => this%right
    end if
  end subroutine muli_trapezium_tree_decide_decreasing
  
  subroutine muli_trapezium_tree_get_left_list (this, list)
    class(muli_trapezium_tree_t), intent(in) :: this
    class(muli_trapezium_list_t), pointer, intent(out) :: list
    class(muli_trapezium_node_class_t), pointer::node
    call this%get_leftmost (node)
    if (associated (node)) then
       select type (node)
       class is (muli_trapezium_list_t)
          list => node
       class default
          nullify (list)
       end select
    else
       nullify (list)
    end if
  end subroutine muli_trapezium_tree_get_left_list

  subroutine muli_trapezium_tree_get_right_list (this, list)
    class(muli_trapezium_tree_t), intent(in) :: this
    class(muli_trapezium_list_t), pointer, intent(out) :: list
    class(muli_trapezium_node_class_t), pointer::node
    call this%get_rightmost (node)
    if (associated (node)) then
       select type (node)
       class is (muli_trapezium_list_t)
          list => node
       class default
          nullify (list)
       end select
    else
       nullify (list) 
    end if
  end subroutine muli_trapezium_tree_get_right_list
  
  subroutine muli_trapezium_tree_find_by_value (this, value, dim, record, node)
    class(muli_trapezium_tree_t), intent(in), target :: this
    real(default), intent(in) :: value
    integer, intent(in) :: record, dim
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    node => this
    do while (.not. allocated (node%values))
       call node%decide (value, dim, record, node)
    end do
  end subroutine muli_trapezium_tree_find_by_value

  subroutine muli_trapezium_tree_find_by_position (this, position, node)
    class(muli_trapezium_tree_t), intent(in), target :: this
    real(default), intent(in) :: position
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    node => this
    do while (.not. allocated (node%values))
       call node%decide (position, node)
    end do
  end subroutine muli_trapezium_tree_find_by_position

  subroutine muli_trapezium_tree_find_decreasing (this, value, dim, node)
    class(muli_trapezium_tree_t), intent(in), target :: this
    real(default), intent(in) :: value
    integer, intent(in) :: dim
    class(muli_trapezium_node_class_t), pointer, intent(out) :: node
    node => this
    do while (.not. allocated (node%values))
       call node%decide_decreasing (value, dim, r_integral_index, node)
    end do
  end subroutine muli_trapezium_tree_find_decreasing
  
  subroutine muli_trapezium_tree_approx_by_integral &
       (this, int, dim, in_range, position, value, integral, content)
    class(muli_trapezium_tree_t), intent(in), target :: this
    real(default), intent(in) :: int
    integer, intent(in) :: dim
    logical, intent(out) :: in_range
    class(muli_trapezium_node_class_t), pointer, intent(out), optional :: content
    real(default), intent(out), optional :: position, value, integral
    integer :: i
    real(default) :: DINT    !,l_prop,r_prop,d_prop
    real(default) :: RP, DP, RV, DV, RI    !FC = gfortran
    class(muli_trapezium_node_class_t), pointer :: node
    node => this
    do while (.not. allocated (node%values))
       call node%decide_decreasing(INT, dim, r_integral_index, node)
    end do
    if (   int<=node%values(dim,r_integral_index)-node%values(dim,d_integral_index)&
         &.and.&
         &int>=node%values(dim,r_integral_index)) then
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

  subroutine muli_trapezium_tree_approx_by_probability &
       (this, prop, dim, in_range, position, value, integral, content)
    class(muli_trapezium_tree_t), intent(in), target :: this
    real(default), intent(in) :: prop
    integer, intent(in) :: dim
    logical, intent(out) :: in_range
    class(muli_trapezium_node_class_t), pointer, intent(out), optional :: content
    real(default), intent(out), optional :: position, value, integral
    integer :: i
    real(default) :: int
    class(muli_trapezium_node_class_t), pointer :: node
    if (zero < prop .and. prop < one) then
       node => this
       int = -log (prop)
       call muli_trapezium_tree_approx_by_integral &
            (this, int, dim, in_range, position, value, integral, content)
    else
       in_range = .false.
    end if
  end subroutine muli_trapezium_tree_approx_by_probability
  
  subroutine muli_trapezium_tree_to_tree (this, out_tree)
    class(muli_trapezium_tree_t), target, intent(in) :: this
    class(muli_trapezium_tree_t), intent(out) :: out_tree
    out_tree%left => this%left
    out_tree%right => this%right
    out_tree%down => this%down
  end subroutine muli_trapezium_tree_to_tree
  
  subroutine muli_trapezium_tree_append(this,right)
    class(muli_trapezium_tree_t), intent(inout), target :: this
    class(muli_trapezium_node_class_t), intent(inout), target :: right
    call msg_error ("muli_trapezium_tree_append: Not yet implemented.")
  end subroutine muli_trapezium_tree_append
  
  subroutine muli_trapezium_tree_gnuplot (this, dir)
    class(muli_trapezium_tree_t), intent(in) :: this
    character(len=*), intent(in) :: dir
    class(muli_trapezium_list_t), pointer :: list
    call this%get_left_list (list)
    call list%gnuplot (dir)
  end subroutine muli_trapezium_tree_gnuplot
  
  recursive subroutine muli_trapezium_list_write_to_marker (this, marker, status)
    class(muli_trapezium_list_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status 
    class(ser_class_t), pointer :: ser
    call marker%mark_begin ("muli_trapezium_list_t")
    call muli_trapezium_write_to_marker (this, marker, status)   
    ser => this%right
    call marker%mark_pointer ("right", ser)
    call marker%mark_end ("muli_trapezium_list_t")
  end subroutine muli_trapezium_list_write_to_marker

  recursive subroutine muli_trapezium_list_read_from_marker (this, marker, status)
    class(muli_trapezium_list_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call msg_warning ("muli_trapezium_list_read_from_marker: " // &
         "You cannot deserialize a list with this subroutine.")
    call msg_error ("Use muli_trapezium_list_read_target_from_marker instead.")
  end subroutine muli_trapezium_list_read_from_marker

  recursive subroutine muli_trapezium_list_read_target_from_marker &
       (this, marker, status)
    class(muli_trapezium_list_t), target, intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(ser_class_t), pointer :: ser
    call marker%pick_begin ("muli_trapezium_list_t", status=status)
    call muli_trapezium_read_from_marker (this, marker, status)
    call marker%pick_pointer ("right", ser)
    if (associated (ser)) then
       select type (ser)
       class is (muli_trapezium_list_t)
          this%right => ser
          ser%left => this
       class default
          nullify (this%right)
          call msg_error ("muli_trapezium_list_read_target_from_marker: " &
               // "Unexpected type for right component.")
       end select
    else
       nullify (this%right)
    end if
    call marker%pick_end ("muli_trapezium_list_t", status)
  end subroutine muli_trapezium_list_read_target_from_marker
  
  recursive subroutine muli_trapezium_list_print_to_unit &
       (this, unit, parents, components, peers)
    class(muli_trapezium_list_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    class(ser_class_t), pointer :: ser
    if (parents > 0)  call muli_trapezium_print_to_unit &
         (this, unit, parents-1, components, peers)
    ser => this%left
    call serialize_print_peer_pointer &
         (ser, unit, -i_one, -i_one, -i_one, "LEFT")
    ser => this%right
    call serialize_print_peer_pointer &
         (ser, unit, parents, components, peers, "RIGHT")
  end subroutine muli_trapezium_list_print_to_unit
    
  pure subroutine muli_trapezium_list_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="muli_trapezium_list_t")
  end subroutine muli_trapezium_list_get_type
  
  elemental logical function muli_trapezium_list_verify_type (type) result (match)
    character(*), intent(in) :: type
    match = type == "muli_trapezium_list_t"
  end function muli_trapezium_list_verify_type
  
  recursive subroutine muli_trapezium_list_finalize(this)
    class(muli_trapezium_list_t), intent(inout)::this
    if (associated(this%right)) then
       call this%right%finalize()
       deallocate(this%right)
    end if
    this%dim=0
  end subroutine muli_trapezium_list_finalize
  
  subroutine muli_trapezium_list_insert_right_a (this, value, content, new_node)
    class(muli_trapezium_list_t), intent(inout), target :: this
    real(default), intent(in) :: value
    class(muli_trapezium_t), intent(in) :: content
    class(muli_trapezium_list_t), pointer, intent(out) :: new_node
    class(muli_trapezium_list_t), pointer :: tmp_list
    call content%to_node (value, list=tmp_list)
    if (associated (this%right)) then
       this%right%left => tmp_list
       tmp_list%right => this%right
    else
       nullify (tmp_list%right)
    end if
    this%right => tmp_list
    tmp_list%left => this
    new_node => tmp_list
  end subroutine muli_trapezium_list_insert_right_a
  
  subroutine muli_trapezium_list_insert_left_a (this, value, content, new_node)
    class(muli_trapezium_list_t), intent(inout), target :: this
    real(default), intent(in) :: value
    class(muli_trapezium_t), intent(in) :: content
    class(muli_trapezium_list_t), pointer, intent(out) :: new_node
    call content%to_node (value, list=new_node)
    new_node%right => this
    if (associated (this%left)) then
       new_node%left => this%left
       this%left%right => new_node
    else
       nullify (new_node%left)
    end if
    this%left => new_node
  end subroutine muli_trapezium_list_insert_left_a

  subroutine muli_trapezium_list_to_tree (this, out_tree)
    class(muli_trapezium_list_t), target, intent(in) :: this
    class(muli_trapezium_tree_t), intent(out) :: out_tree
    type(muli_trapezium_tree_t),target :: do_list
    class(muli_trapezium_node_class_t),pointer :: this_entry,do_list_entry,node
    class(muli_trapezium_tree_t),pointer :: tree1,tree2
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
    type is (muli_trapezium_tree_t)
       call node%to_tree(out_tree)
    class default
       print *,"muli_trapezium_list_to_tree"
       print *,"unexpeted type for do_list%right"
    end select
    out_tree%right=>out_tree%right%left
    if (allocated(out_tree%values)) then
       deallocate(out_tree%values)
    end if
    deallocate(do_list%right%right)
    deallocate(do_list%right)
  end subroutine muli_trapezium_list_to_tree
  
  subroutine muli_trapezium_list_gnuplot (this, dir)
    class(muli_trapezium_list_t), intent(in), target :: this
    character(len=*), intent(in) :: dir
    character(len=*), parameter :: val_file = "/value.plot"
    character(len=*), parameter :: int_file = "/integral.plot"
    character(len=*), parameter :: err_file = "/integral_error.plot"
    character(len=*), parameter :: pro_file = "/probability.plot"
    character(len=*), parameter :: den_file = "/density.plot"
    character(len=*), parameter :: fmt = "(ES20.10)"
    class(muli_trapezium_node_class_t), pointer :: list
    integer :: val_unit, err_unit, int_unit, pro_unit, den_unit
    list => this
    call generate_unit (val_unit, 100, 1000)
    open (val_unit, file = dir // val_file)
    call generate_unit (int_unit, 100, 1000)
    open (int_unit, file = dir // int_file)
    call generate_unit (err_unit, 100, 1000)
    open (err_unit, file = dir // err_file)
    call generate_unit (pro_unit, 100, 1000)
    open (pro_unit, file = dir // pro_file)
    call generate_unit (den_unit, 100, 1000)
    open (den_unit, file = dir // den_file)
    do while (associated (list))
       ! print *,list%r_position,list%get_r_value()
       write (val_unit, fmt, advance="no")  list%r_position
       call write_array (val_unit, list%get_r_value_array(), fmt)
       write (int_unit,fmt,advance="no")  list%r_position
       call write_array (int_unit, list%get_r_integral(), fmt)
       write (err_unit, fmt, advance="no")  list%r_position
       call write_array (err_unit, list%get_error(), fmt)
       write (pro_unit, fmt, advance="no")  list%r_position
       call write_array (pro_unit, list%get_r_probability(), fmt)
       write (den_unit, fmt, advance="no")  list%r_position
       call write_array (den_unit, list%get_r_probability() * &
            list%get_r_value_array(), fmt)
       list => list%right
    end do
    close (val_unit)
    close (int_unit)
    close (err_unit)
    close (pro_unit)
    close (den_unit)
    contains
      subroutine write_array (unit, array, form)
        integer, intent(in) :: unit
        real(default), dimension(:), intent(in) :: array
        character(len=*), intent(in) :: form
        integer :: n
        do n = 1, size(array)
           write (unit, form, advance="no")  array(n)
           flush (unit)
        end do
        write (unit, *)
      end subroutine write_array
  end subroutine muli_trapezium_list_gnuplot

  subroutine muli_trapezium_list_integrate (this, integral_sum, error_sum)
    class(muli_trapezium_list_t), intent(in), target :: this
    real(default), intent(out) :: error_sum, integral_sum
    real(default), dimension(:), allocatable :: integral
    class(muli_trapezium_node_class_t), pointer :: node
    allocate (integral (0:this%dim-1))
    call this%get_rightmost (node)
    integral = 0._default
    integral_sum = 0._default
    error_sum = 0._default
    integrate: do while (associated (node))
       node%values(1,r_value_index) = sum(node%values(1:this%dim-1,r_value_index))
       node%values(1,d_value_index) = sum(node%values(1:this%dim-1,d_value_index))
       ! node%values (1, r_integral_index) = &
       !    sum (node%values (1:this%dim-1, r_integral_index))
       ! node%values (1, d_integral_index) = &
       !    sum (node%values (1:this%dim-1, d_integral_index))
       node%values(1, error_index) = sum (node%values(1:this%dim-1, error_index))
       error_sum = error_sum + node%values (1, error_index)
       call node%set_d_integral (node%get_d_position() * &
            (node%get_d_value() / 2 - node%get_r_value_array ()))
       call node%set_r_probability (exp (-integral))
       call node%set_r_integral (integral)
       integral = integral - node%get_d_integral()
       call node%set_d_probability (node%get_r_probability() - exp(-integral))
       ! call muli_trapezium_write (node, output_unit)
       call node%get_left (node)
    end do integrate
    integral_sum = integral (1)
  end subroutine muli_trapezium_list_integrate
  
  recursive subroutine muli_trapezium_list_check (this)
    class(muli_trapezium_list_t), intent(in),target :: this
    class(muli_trapezium_node_class_t), pointer :: tn, next
    real(default), parameter :: eps = 1E-10_default
    logical::test
    if (associated(this%right)) then
       next=>this%right
       test=(this%r_position.le.this%right%get_l_position()+eps)
       print *,"position check:  ",test
       if (.not.test) then
          call this%print_parents()
          call next%print_parents()
       end if
       select type (next)
       class is (muli_trapezium_list_t)
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
  
  recursive subroutine muli_trapezium_list_apply (this, proc)
    class(muli_trapezium_list_t), intent(inout) :: this
    interface
       subroutine proc (this)
         import muli_trapezium_node_class_t
         class(muli_trapezium_node_class_t), intent(inout) :: this
       end subroutine proc
    end interface
    if (associated (this%right))call this%right%apply (proc)
    call proc (this)
  end subroutine muli_trapezium_list_apply
  
!  subroutine muli_trapezium_list_insert_right_old &
!       (this, value, content, new_node)
!    class(muli_trapezium_list_t), intent(inout), target :: this
!    real(default), intent(in) :: value
!    class(muli_trapezium_t), intent(in) :: content
!    class(muli_trapezium_list_t), pointer, intent(out) :: new_node
!    call content%to_node (value, list=new_node)
!    new_node%left => this
!    if (associated (this%right)) then
!       new_node%right => this%right
!       this%right%left => new_node
!    else
!       nullify (new_node%right)
!    end if
!    this%right => new_node
!  end subroutine muli_trapezium_list_insert_right_old

!  subroutine muli_trapezium_node_error_no_content (this)
!    class(muli_trapezium_node_class_t), intent(in) :: this
!!    print ("muli_trapezium_node: Trying to access unallocated content.")
!!    call this%print()
!  end subroutine muli_trapezium_node_error_no_content
  
  
end module muli_trapezium
