! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module state_matrices

  use kinds, only: default
  use io_units
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_17, FMT_19
  use unit_tests
  use diagnostics
  use flavors
  use colors
  use helicities
  use quantum_numbers

  implicit none
  private

  public :: state_matrix_t
  public :: state_matrix_init
  public :: state_matrix_final
  public :: state_matrix_write
  public :: state_matrix_write_raw
  public :: state_matrix_read_raw
  public :: state_matrix_is_defined
  public :: state_matrix_is_empty
  public :: state_matrix_get_n_matrix_elements
  public :: state_matrix_get_n_leaves
  public :: state_matrix_get_depth
  public :: state_matrix_get_norm
  public :: state_matrix_get_quantum_numbers
  public :: state_matrix_get_matrix_element
  public :: state_matrix_get_max_color_value
  public :: state_matrix_add_state
  public :: state_matrix_collapse
  public :: state_matrix_reduce
  public :: state_matrix_freeze
  public :: state_matrix_set_matrix_element
  public :: state_matrix_add_to_matrix_element
  public :: state_iterator_t
  public :: state_iterator_init
  public :: state_iterator_advance
  public :: state_iterator_is_valid
  public :: state_iterator_get_me_index
  public :: state_iterator_get_me_count
  public :: state_iterator_get_quantum_numbers
  public :: state_iterator_get_flavor
  public :: state_iterator_get_color
  public :: state_iterator_get_helicity
  public :: state_iterator_get_matrix_element
  public :: state_iterator_set_matrix_element
  public :: assignment(=)
  public :: state_matrix_get_diagonal_entries
  public :: state_matrix_renormalize
  public :: state_matrix_normalize_by_trace
  public :: state_matrix_normalize_by_max
  public :: state_matrix_set_norm
  public :: state_matrix_sum
  public :: state_matrix_trace
  public :: state_matrix_add_color_contractions
  public :: merge_state_matrices
  public :: state_matrix_evaluate_product
  public :: state_matrix_evaluate_product_cf
  public :: state_matrix_evaluate_square_c
  public :: state_matrix_evaluate_sum
  public :: state_matrix_evaluate_me_sum
  public :: outer_multiply
  public :: state_matrix_factorize
  public :: state_matrix_test

  integer, parameter, public :: FM_IGNORE_HELICITY = 1
  integer, parameter, public :: FM_SELECT_HELICITY = 2
  integer, parameter, public :: FM_FACTOR_HELICITY = 3

       
  type :: node_t
     private
     type(quantum_numbers_t) :: qn
     type(node_t), pointer :: parent => null ()
     type(node_t), pointer :: child_first => null ()
     type(node_t), pointer :: child_last => null ()
     type(node_t), pointer :: next => null ()
     type(node_t), pointer :: previous => null ()
     integer :: me_index = 0
     integer, dimension(:), allocatable :: me_count
     complex(default) :: me = 0
  end type node_t

  type :: state_matrix_t
     private
     type(node_t), pointer :: root => null ()
     integer :: depth = 0
     integer :: n_matrix_elements = 0
     logical :: leaf_nodes_store_values = .false.
     integer :: n_counters = 0
     complex(default), dimension(:), allocatable :: me
     real(default) :: norm = 1
  end type state_matrix_t

  type :: state_iterator_t
     private
     integer :: depth = 0
     type(state_matrix_t), pointer :: state => null ()
     type(node_t), pointer :: node => null ()
  end type state_iterator_t


  interface state_matrix_freeze
     module procedure state_matrix_freeze1
     module procedure state_matrix_freeze2
  end interface
  interface state_matrix_set_matrix_element
     module procedure state_matrix_set_matrix_element_qn
     module procedure state_matrix_set_matrix_element_all
     module procedure state_matrix_set_matrix_element_array 
     module procedure state_matrix_set_matrix_element_single
     module procedure state_matrix_set_matrix_element_clone
  end interface
  interface state_iterator_get_quantum_numbers
     module procedure state_iterator_get_qn_multi
     module procedure state_iterator_get_qn_slice
     module procedure state_iterator_get_qn_range
     module procedure state_iterator_get_qn_single
  end interface

  interface state_iterator_get_flavor
     module procedure state_iterator_get_flv_multi
     module procedure state_iterator_get_flv_slice
     module procedure state_iterator_get_flv_range
     module procedure state_iterator_get_flv_single
  end interface

  interface state_iterator_get_color
     module procedure state_iterator_get_col_multi
     module procedure state_iterator_get_col_slice
     module procedure state_iterator_get_col_range
     module procedure state_iterator_get_col_single
  end interface

  interface state_iterator_get_helicity
     module procedure state_iterator_get_hel_multi
     module procedure state_iterator_get_hel_slice
     module procedure state_iterator_get_hel_range
     module procedure state_iterator_get_hel_single
  end interface

  interface assignment(=)
     module procedure state_matrix_assign
  end interface

  interface outer_multiply
     module procedure outer_multiply_pair
     module procedure outer_multiply_array
  end interface


contains

  pure recursive subroutine node_delete_offspring (node)
    type(node_t), pointer :: node
    type(node_t), pointer :: child
    child => node%child_first
    do while (associated (child))
       node%child_first => node%child_first%next
       call node_delete_offspring (child)
       deallocate (child)
       child => node%child_first
    end do
    node%child_last => null ()
  end subroutine node_delete_offspring

  pure subroutine node_delete (node)
    type(node_t), pointer :: node
    call node_delete_offspring (node)
    if (associated (node%previous)) then
       node%previous%next => node%next
    else if (associated (node%parent)) then
       node%parent%child_first => node%next
    end if
    if (associated (node%next)) then
       node%next%previous => node%previous
    else if (associated (node%parent)) then
       node%parent%child_last => node%previous
    end if
    deallocate (node)
  end subroutine node_delete

  subroutine node_append_child (node, child)
    type(node_t), target, intent(inout) :: node
    type(node_t), pointer :: child
    allocate (child)
    if (associated (node%child_last)) then
       node%child_last%next => child
       child%previous => node%child_last
    else
       node%child_first => child
    end if
    node%child_last => child
    child%parent => node
  end subroutine node_append_child

  subroutine node_write (node, me_array, verbose, unit, testflag)
    type(node_t), intent(in) :: node
    complex(default), dimension(:), intent(in), optional :: me_array
    logical, intent(in), optional :: verbose, testflag
    integer, intent(in), optional :: unit
    logical :: verb
    integer :: u
    character(len=7) :: fmt
    call pac_fmt (fmt, FMT_19, FMT_17, testflag)
    verb = .false.;  if (present (verbose)) verb = verbose
    u = given_output_unit (unit);  if (u < 0)  return
    call quantum_numbers_write (node%qn, u)
    if (node%me_index /= 0) then
       write (u, "(A,I0,A)", advance="no")  " => ME(", node%me_index, ")"
       if (present (me_array)) then
          write (u, "(A)", advance="no")  " = "
          write (u, "('('," // fmt // ",','," // fmt // ",')')", &
               advance="no") pacify_complex (me_array(node%me_index))
       end if
    end if
    write (u, *)
    if (verb) then
       call ptr_write ("parent     ", node%parent)
       call ptr_write ("child_first", node%child_first)
       call ptr_write ("child_last ", node%child_last)
       call ptr_write ("next       ", node%next)
       call ptr_write ("previous   ", node%previous)
    end if
  contains
    subroutine ptr_write (label, node)
      character(*), intent(in) :: label
      type(node_t), pointer :: node
      if (associated (node)) then
         write (u, "(10x,A,1x,'->',1x)", advance="no") label
         call quantum_numbers_write (node%qn, u)
         write (u, *)
      end if
    end subroutine ptr_write
  end subroutine node_write

  recursive subroutine node_write_rec (node, me_array, verbose, &
        indent, unit, testflag)
    type(node_t), intent(in), target :: node
    complex(default), dimension(:), intent(in), optional :: me_array
    logical, intent(in), optional :: verbose, testflag
    integer, intent(in), optional :: indent
    integer, intent(in), optional :: unit
    type(node_t), pointer :: current
    logical :: verb
    integer :: i, u
    verb = .false.;  if (present (verbose))  verb = verbose
    i = 0;  if (present (indent)) i = indent
    u = given_output_unit (unit);  if (u < 0)  return
    current => node%child_first
    do while (associated (current))
       write (u, "(A)", advance="no")  repeat (" ", i)
       call node_write (current, me_array, verbose=verb, &
          unit=u, testflag=testflag)
       call node_write_rec (current, me_array, verbose=verb, &
          indent=i+2, unit=u, testflag = testflag)
       current => current%next
    end do
  end subroutine node_write_rec
  
  recursive subroutine node_write_raw_rec (node, u)
    type(node_t), intent(in), target :: node
    integer, intent(in) :: u
    logical :: associated_child_first, associated_next
    call quantum_numbers_write_raw (node%qn, u)
    associated_child_first = associated (node%child_first)
    write (u) associated_child_first
    associated_next = associated (node%next)
    write (u) associated_next
    if (associated_child_first) then
       call node_write_raw_rec (node%child_first, u)
    else
       write (u)  node%me_index
       write (u)  node%me
    end if
    if (associated_next) then
       call node_write_raw_rec (node%next, u)
    end if
  end subroutine node_write_raw_rec

  recursive subroutine node_read_raw_rec (node, u, parent, iostat)
    type(node_t), intent(out), target :: node
    integer, intent(in) :: u
    type(node_t), intent(in), optional, target :: parent
    integer, intent(out), optional :: iostat
    logical :: associated_child_first, associated_next
    type(node_t), pointer :: child
    call quantum_numbers_read_raw (node%qn, u, iostat=iostat)
    read (u, iostat=iostat) associated_child_first
    read (u, iostat=iostat) associated_next
    if (present (parent))  node%parent => parent
    if (associated_child_first) then
       allocate (child)
       node%child_first => child
       node%child_last => null ()
       call node_read_raw_rec (child, u, node, iostat=iostat)
       do while (associated (child))
          child%previous => node%child_last
          node%child_last => child
          child => child%next
       end do
    else
       read (u, iostat=iostat)  node%me_index
       read (u, iostat=iostat)  node%me
    end if
    if (associated_next) then
       allocate (node%next)
       call node_read_raw_rec (node%next, u, parent, iostat=iostat)
    end if
  end subroutine node_read_raw_rec

  elemental subroutine state_matrix_init (state, store_values, n_counters)
    type(state_matrix_t), intent(out) :: state
    logical, intent(in), optional :: store_values
    integer, intent(in), optional :: n_counters
    allocate (state%root)
    if (present (store_values)) &
       state%leaf_nodes_store_values = store_values
    if (present (n_counters)) state%n_counters = n_counters
  end subroutine state_matrix_init

  elemental subroutine state_matrix_final (state)
    type(state_matrix_t), intent(inout) :: state
    if (allocated (state%me))  deallocate (state%me)
    if (associated (state%root))  call node_delete (state%root)
    state%depth = 0
    state%n_matrix_elements = 0
  end subroutine state_matrix_final

  subroutine state_matrix_write (state, unit, write_value_list, &
        verbose, testflag)
    type(state_matrix_t), intent(in) :: state
    logical, intent(in), optional :: write_value_list, verbose, testflag
    integer, intent(in), optional :: unit
    complex(default) :: me_dum
    character(len=7) :: fmt
    integer :: u
    integer :: i
    call pac_fmt (fmt, FMT_19, FMT_17, testflag)        
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A," // fmt // ")") "State matrix:  norm = ", state%norm
    if (associated (state%root)) then
       if (allocated (state%me)) then
          call node_write_rec (state%root, state%me, verbose=verbose, &
             indent=1, unit=u, testflag=testflag)
       else
          call node_write_rec (state%root, verbose=verbose, indent=1, &
             unit=u, testflag=testflag)
       end if
    end if
    if (present (write_value_list)) then
       if (write_value_list .and. allocated (state%me)) then
          do i = 1, size (state%me)
             write (u, "(1x,I0,A)", advance="no")  i, ":"
             me_dum = state%me(i)             
             if (real(state%me(i)) == -real(state%me(i))) then
                me_dum = &
                     cmplx (0._default, aimag(me_dum), kind=default)
             end if
             if (aimag(me_dum) == -aimag(me_dum)) then
                me_dum = &
                     cmplx (real(me_dum), 0._default, kind=default)
             end if
             write (u, "('('," // fmt // ",','," // fmt // &
                  ",')')")  me_dum
          end do
       end if
    end if
  end subroutine state_matrix_write

  subroutine state_matrix_write_raw (state, u)
    type(state_matrix_t), intent(in) :: state
    integer, intent(in) :: u
    logical :: associated_root
    associated_root = associated (state%root)
    write (u) associated_root
    if (associated_root) then
       write (u) state%depth
       write (u) state%norm
       call node_write_raw_rec (state%root, u)
    end if
  end subroutine state_matrix_write_raw

  subroutine state_matrix_read_raw (state, u, iostat)
    type(state_matrix_t), intent(out) :: state
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    logical :: associated_root
    read (u, iostat=iostat) associated_root
    if (associated_root) then
       read (u, iostat=iostat) state%depth
       read (u, iostat=iostat) state%norm
       call state_matrix_init (state)
       call node_read_raw_rec (state%root, u, iostat=iostat)
       call state_matrix_freeze (state)
    end if
  end subroutine state_matrix_read_raw

  elemental function state_matrix_is_defined (state) result (defined)
    logical :: defined
    type(state_matrix_t), intent(in) :: state
    defined = associated (state%root)
  end function state_matrix_is_defined

  elemental function state_matrix_is_empty (state) result (flag)
    logical :: flag
    type(state_matrix_t), intent(in) :: state
    flag = state%depth == 0
  end function state_matrix_is_empty

  function state_matrix_get_n_matrix_elements (state) result (n)
    integer :: n
    type(state_matrix_t), intent(in) :: state
    n = state%n_matrix_elements
  end function state_matrix_get_n_matrix_elements

  function state_matrix_get_n_leaves (state) result (n)
    integer :: n
    type(state_matrix_t), intent(in) :: state
    type(state_iterator_t) :: it
    n = 0
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       n = n + 1
       call state_iterator_advance (it)
    end do
  end function state_matrix_get_n_leaves

  function state_matrix_get_depth (state) result (depth)
    integer :: depth
    type(state_matrix_t), intent(in) :: state
    depth = state%depth
  end function state_matrix_get_depth

  function state_matrix_get_norm (state) result (norm)
    real(default) :: norm
    type(state_matrix_t), intent(in) :: state
    norm = state%norm
  end function state_matrix_get_norm

  function state_matrix_get_quantum_numbers (state, i) result (qn)
    type(state_matrix_t), intent(in), target :: state
    integer, intent(in) :: i
    type(quantum_numbers_t), dimension(state%depth) :: qn
    type(state_iterator_t) :: it
    integer :: k
    k = 0
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       k = k + 1
       if (k == i) then
          qn = state_iterator_get_quantum_numbers (it)
          return
       end if
       call state_iterator_advance (it)
    end do
  end function state_matrix_get_quantum_numbers

  function state_matrix_get_matrix_element (state, i) result (me)
    complex(default) :: me
    type(state_matrix_t), intent(in) :: state
    integer, intent(in) :: i
    if (allocated (state%me)) then
       me = state%me(i)
    else
       me = 0
    end if
  end function state_matrix_get_matrix_element

  function state_matrix_get_max_color_value (state) result (cmax)
    integer :: cmax
    type(state_matrix_t), intent(in) :: state
    if (associated (state%root)) then
       cmax = node_get_max_color_value (state%root)
    else
       cmax = 0
    end if
  contains
    recursive function node_get_max_color_value (node) result (cmax)
      integer :: cmax
      type(node_t), intent(in), target :: node
      type(node_t), pointer :: current
      cmax = quantum_numbers_get_max_color_value (node%qn)
      current => node%child_first
      do while (associated (current))
         cmax = max (cmax, node_get_max_color_value (current))
         current => current%next
      end do
    end function node_get_max_color_value
  end function state_matrix_get_max_color_value

  subroutine state_matrix_add_state &
       (state, qn, index, value, sum_values, counter_index, me_index)
    type(state_matrix_t), intent(inout) :: state
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    integer, intent(in), optional :: index
    complex(default), intent(in), optional :: value
    logical, intent(in), optional :: sum_values
    integer, intent(in), optional :: counter_index
    integer, intent(out), optional :: me_index
    logical :: set_index, get_index, add
    set_index = present (index)
    get_index = present (me_index)
    add = .false.;  if (present (sum_values))  add = sum_values
    if (state%depth == 0) then
       state%depth = size (qn)
    else if (state%depth /= size (qn)) then
       call state_matrix_write (state)
       call msg_bug ("State matrix: depth mismatch")
    end if
    if (size (qn) > 0)  call node_make_branch (state%root, qn)
  contains
     recursive subroutine node_make_branch (parent, qn)
       type(node_t), pointer :: parent
       type(quantum_numbers_t), dimension(:), intent(in) :: qn
       type(node_t), pointer :: child
       logical :: match
       match = .false.
       child => parent%child_first
       SCAN_CHILDREN: do while (associated (child))
          match = child%qn == qn(1)
          if (match)  exit SCAN_CHILDREN
          child => child%next
       end do SCAN_CHILDREN
       if (.not. match) then
          call node_append_child (parent, child)
          child%qn = qn(1)
       end if
       select case (size (qn))
       case (1)
          if (.not. match) then
             state%n_matrix_elements = state%n_matrix_elements + 1
             child%me_index = state%n_matrix_elements
          end if
          if (set_index) then
             child%me_index = index
          end if
          if (get_index) then
             me_index = child%me_index
          end if
          if (present (counter_index)) then
             if (.not. allocated (child%me_count)) then
                allocate (child%me_count (state%n_counters))
                child%me_count = 0
             end if
             child%me_count(counter_index) = child%me_count(counter_index) + 1
          end if
          if (present (value)) then
             if (add) then
                child%me = child%me + value
             else
                child%me = value
             end if
          end if
       case (2:)
          call node_make_branch (child, qn(2:))
       end select
     end subroutine node_make_branch
   end subroutine state_matrix_add_state

  subroutine state_matrix_collapse (state, mask)
    type(state_matrix_t), intent(inout) :: state
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    type(state_matrix_t) :: red_state
    if (state_matrix_is_defined (state)) then
       call state_matrix_reduce (state, mask, red_state)
       call state_matrix_final (state)
       state = red_state
    end if
   end subroutine state_matrix_collapse

  subroutine state_matrix_reduce (state, mask, red_state)
    type(state_matrix_t), intent(in), target :: state
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    type(state_matrix_t), intent(out) :: red_state
    type(state_iterator_t) :: it
    type(quantum_numbers_t), dimension(size(mask)) :: qn
    call state_matrix_init (red_state)
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       qn = state_iterator_get_quantum_numbers (it)
       call quantum_numbers_undefine (qn, mask)
       call state_matrix_add_state (red_state, qn)
       call state_iterator_advance (it)
    end do
  end subroutine state_matrix_reduce

  subroutine state_matrix_freeze1 (state)
    type(state_matrix_t), intent(inout), target :: state
    type(state_iterator_t) :: it
    if (associated (state%root)) then
       if (allocated (state%me))  deallocate (state%me)
       allocate (state%me (state%n_matrix_elements))
       state%me = 0
    end if
    if (state%leaf_nodes_store_values) then
       call state_iterator_init (it, state)
       do while (state_iterator_is_valid (it))
          state%me(state_iterator_get_me_index (it)) &
               = state_iterator_get_matrix_element (it)
          call state_iterator_advance (it)
       end do
       state%leaf_nodes_store_values = .false.
    end if
  end subroutine state_matrix_freeze1

  subroutine state_matrix_freeze2 (state)
    type(state_matrix_t), dimension(:), intent(inout), target :: state
    integer :: i
    do i = 1, size (state)
       call state_matrix_freeze1 (state(i))
    end do
  end subroutine state_matrix_freeze2

  subroutine state_matrix_set_matrix_element_qn (state, qn, value)
    type(state_matrix_t), intent(inout) :: state
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    complex(default), intent(in) :: value
    type(state_iterator_t) :: it
    if (.not. allocated (it%state%me)) then
       allocate (it%state%me (size(qn)))
    end if
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       if (all (qn == state_iterator_get_quantum_numbers (it))) then
          call state_iterator_set_matrix_element (it, value)
          return
       end if
       call state_iterator_advance (it)
    end do
  end subroutine state_matrix_set_matrix_element_qn

  subroutine state_matrix_set_matrix_element_all (state, value)
    type(state_matrix_t), intent(inout) :: state
    complex(default), intent(in) :: value
    if (.not. allocated (state%me)) then
       allocate (state%me (state%n_matrix_elements))    
    end if
    state%me = value
  end subroutine state_matrix_set_matrix_element_all

  subroutine state_matrix_set_matrix_element_array (state, value)
    type(state_matrix_t), intent(inout) :: state
    complex(default), dimension(:), intent(in) :: value
    if (.not. allocated (state%me)) then
       allocate (state%me (size (value)))
    end if
    state%me = value
  end subroutine state_matrix_set_matrix_element_array

  pure subroutine state_matrix_set_matrix_element_single (state, i, value)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    complex(default), intent(in) :: value
    if (.not. allocated (state%me)) then
       allocate (state%me (state%n_matrix_elements))    
    end if
    state%me(i) = value
  end subroutine state_matrix_set_matrix_element_single

  subroutine state_matrix_set_matrix_element_clone (state, state1)
    type(state_matrix_t), intent(inout) :: state
    type(state_matrix_t), intent(in) :: state1
    if (.not. allocated (state1%me)) return
    if (.not. allocated (state%me)) allocate (state%me (size (state1%me)))
    state%me = state1%me
  end subroutine state_matrix_set_matrix_element_clone

  subroutine state_matrix_add_to_matrix_element (state, i, value)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    complex(default), intent(in) :: value
    state%me(i) = state%me(i) + value
  end subroutine state_matrix_add_to_matrix_element

  subroutine state_iterator_init (it, state)
    type(state_iterator_t), intent(out) :: it
    type(state_matrix_t), intent(in), target :: state
    it%state => state
    it%depth = state%depth
    if (state_matrix_is_defined (state)) then
       it%node => state%root
       do while (associated (it%node%child_first))
          it%node => it%node%child_first
       end do
    else
       it%node => null ()
    end if
  end subroutine state_iterator_init
    
  subroutine state_iterator_advance (it)
    type(state_iterator_t), intent(inout) :: it
    call find_next (it%node)
  contains
    recursive subroutine find_next (node_in)
      type(node_t), intent(in), target :: node_in
      type(node_t), pointer :: node
      node => node_in
      if (associated (node%next)) then
         node => node%next
         do while (associated (node%child_first))
            node => node%child_first
         end do
         it%node => node
      else if (associated (node%parent)) then
         call find_next (node%parent)
      else
         it%node => null ()
      end if
    end subroutine find_next
  end subroutine state_iterator_advance

  function state_iterator_is_valid (it) result (defined)
    logical :: defined
    type(state_iterator_t), intent(in) :: it
    defined = associated (it%node)
  end function state_iterator_is_valid

  function state_iterator_get_me_index (it) result (n)
    integer :: n
    type(state_iterator_t), intent(in) :: it
    n = it%node%me_index
  end function state_iterator_get_me_index

  function state_iterator_get_me_count (it) result (n)
    integer, dimension(:), allocatable :: n
    type(state_iterator_t), intent(in) :: it
    if (allocated (it%node%me_count)) then
       allocate (n (size (it%node%me_count)))
       n = it%node%me_count
    else
       allocate (n (0))
    end if
  end function state_iterator_get_me_count

  function state_iterator_get_qn_multi (it) result (qn)
    type(state_iterator_t), intent(in) :: it
    type(quantum_numbers_t), dimension(it%depth) :: qn
    type(node_t), pointer :: node
    integer :: i
    node => it%node
    do i = it%depth, 1, -1
       qn(i) = node%qn
       node => node%parent
    end do
  end function state_iterator_get_qn_multi

  function state_iterator_get_flv_multi (it) result (flv)
    type(state_iterator_t), intent(in) :: it
    type(flavor_t), dimension(it%depth) :: flv
    flv = quantum_numbers_get_flavor &
         (state_iterator_get_quantum_numbers (it))
  end function state_iterator_get_flv_multi

  function state_iterator_get_col_multi (it) result (col)
    type(state_iterator_t), intent(in) :: it
    type(color_t), dimension(it%depth) :: col
    col = quantum_numbers_get_color &
         (state_iterator_get_quantum_numbers (it))
  end function state_iterator_get_col_multi

  function state_iterator_get_hel_multi (it) result (hel)
    type(state_iterator_t), intent(in) :: it
    type(helicity_t), dimension(it%depth) :: hel
    hel = quantum_numbers_get_helicity &
         (state_iterator_get_quantum_numbers (it))
  end function state_iterator_get_hel_multi

  function state_iterator_get_qn_slice (it, index) result (qn)
    type(state_iterator_t), intent(in) :: it
    integer, dimension(:), intent(in) :: index
    type(quantum_numbers_t), dimension(size(index)) :: qn
    type(quantum_numbers_t), dimension(it%depth) :: qn_tmp
    qn_tmp = state_iterator_get_qn_multi (it)
    qn = qn_tmp(index)
  end function state_iterator_get_qn_slice

  function state_iterator_get_flv_slice (it, index) result (flv)
    type(state_iterator_t), intent(in) :: it
    integer, dimension(:), intent(in) :: index
    type(flavor_t), dimension(size(index)) :: flv
    flv = quantum_numbers_get_flavor &
         (state_iterator_get_quantum_numbers (it, index))
  end function state_iterator_get_flv_slice

  function state_iterator_get_col_slice (it, index) result (col)
    type(state_iterator_t), intent(in) :: it
    integer, dimension(:), intent(in) :: index
    type(color_t), dimension(size(index)) :: col
    col = quantum_numbers_get_color &
         (state_iterator_get_quantum_numbers (it, index))
  end function state_iterator_get_col_slice

  function state_iterator_get_hel_slice (it, index) result (hel)
    type(state_iterator_t), intent(in) :: it
    integer, dimension(:), intent(in) :: index
    type(helicity_t), dimension(size(index)) :: hel
    hel = quantum_numbers_get_helicity &
         (state_iterator_get_quantum_numbers (it, index))
  end function state_iterator_get_hel_slice

  function state_iterator_get_qn_range (it, k1, k2) result (qn)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k1, k2
    type(quantum_numbers_t), dimension(k2-k1+1) :: qn
    type(node_t), pointer :: node
    integer :: i
    node => it%node
    SCAN: do i = it%depth, 1, -1
       if (k1 <= i .and. i <= k2) then
          qn(i-k1+1) = node%qn
       else
          node => node%parent
       end if
    end do SCAN
  end function state_iterator_get_qn_range

  function state_iterator_get_flv_range (it, k1, k2) result (flv)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k1, k2
    type(flavor_t), dimension(k2-k1+1) :: flv
    flv = quantum_numbers_get_flavor &
         (state_iterator_get_quantum_numbers (it, k1, k2))
  end function state_iterator_get_flv_range

  function state_iterator_get_col_range (it, k1, k2) result (col)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k1, k2
    type(color_t), dimension(k2-k1+1) :: col
    col = quantum_numbers_get_color &
         (state_iterator_get_quantum_numbers (it, k1, k2))
  end function state_iterator_get_col_range

  function state_iterator_get_hel_range (it, k1, k2) result (hel)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k1, k2
    type(helicity_t), dimension(k2-k1+1) :: hel
    hel = quantum_numbers_get_helicity &
         (state_iterator_get_quantum_numbers (it, k1, k2))
  end function state_iterator_get_hel_range

  function state_iterator_get_qn_single (it, k) result (qn)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k
    type(quantum_numbers_t) :: qn
    type(node_t), pointer :: node
    integer :: i
    node => it%node
    SCAN: do i = it%depth, 1, -1
       if (i == k) then
          qn = node%qn
          exit SCAN
       else
          node => node%parent
       end if
    end do SCAN
  end function state_iterator_get_qn_single

  function state_iterator_get_flv_single (it, k) result (flv)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k
    type(flavor_t) :: flv
    flv = quantum_numbers_get_flavor &
         (state_iterator_get_quantum_numbers (it, k))
  end function state_iterator_get_flv_single

  function state_iterator_get_col_single (it, k) result (col)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k
    type(color_t) :: col
    col = quantum_numbers_get_color &
         (state_iterator_get_quantum_numbers (it, k))
  end function state_iterator_get_col_single

  function state_iterator_get_hel_single (it, k) result (hel)
    type(state_iterator_t), intent(in) :: it
    integer, intent(in) :: k
    type(helicity_t) :: hel
    hel = quantum_numbers_get_helicity &
         (state_iterator_get_quantum_numbers (it, k))
  end function state_iterator_get_hel_single

  function state_iterator_get_matrix_element (it) result (me)
    complex(default) :: me
    type(state_iterator_t), intent(in) :: it
    if (it%state%leaf_nodes_store_values) then
       me = it%node%me
    else if (it%node%me_index /= 0) then
       me = it%state%me(it%node%me_index)
    else
       me = 0
    end if
  end function state_iterator_get_matrix_element

  subroutine state_iterator_set_matrix_element (it, value)
    type(state_iterator_t), intent(inout) :: it
    complex(default), intent(in) :: value
    if (it%node%me_index /= 0) then
       it%state%me(it%node%me_index) = value
    end if
  end subroutine state_iterator_set_matrix_element

  subroutine state_matrix_assign (state_out, state_in)
    type(state_matrix_t), intent(out) :: state_out
    type(state_matrix_t), intent(in), target :: state_in
    type(state_iterator_t) :: it
    if (.not. state_matrix_is_defined (state_in))  return
    call state_matrix_init (state_out)
    call state_iterator_init (it, state_in)
    do while (state_iterator_is_valid (it))
       call state_matrix_add_state (state_out, &
            state_iterator_get_quantum_numbers (it), &
            state_iterator_get_me_index (it))
       call state_iterator_advance (it)
    end do
    if (allocated (state_in%me)) then
       allocate (state_out%me (size (state_in%me)))
       state_out%me = state_in%me
    end if
  end subroutine state_matrix_assign

  subroutine state_matrix_get_diagonal_entries (state, i)
    type(state_matrix_t), intent(in) :: state
    integer, dimension(:), allocatable, intent(out) :: i
    integer, dimension(state%n_matrix_elements) :: tmp
    integer :: n
    type(state_iterator_t) :: it
    n = 0
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       if (all (quantum_numbers_are_diagonal ( &
             state_iterator_get_quantum_numbers (it)))) then
          n = n + 1
          tmp(n) = state_iterator_get_me_index (it)
       end if
       call state_iterator_advance (it)
    end do
    allocate (i(n))
    if (n > 0) i = tmp(:n)
  end subroutine state_matrix_get_diagonal_entries

  subroutine state_matrix_renormalize (state, factor)
    type(state_matrix_t), intent(inout) :: state
    complex(default), intent(in) :: factor
    state%me = state%me * factor
  end subroutine state_matrix_renormalize

  subroutine state_matrix_normalize_by_trace (state)
    type(state_matrix_t), intent(inout) :: state
    real(default) :: trace
    trace = state_matrix_trace (state)
    if (trace /= 0) then
       state%me = state%me / trace
       state%norm = state%norm * trace
    end if
  end subroutine state_matrix_normalize_by_trace

  subroutine state_matrix_normalize_by_max (state)
    type(state_matrix_t), intent(inout) :: state
    real(default) :: m
    m = maxval (abs (state%me))
    if (m /= 0) then
       state%me = state%me / m
       state%norm = state%norm * m
    end if
  end subroutine state_matrix_normalize_by_max

  subroutine state_matrix_set_norm (state, norm)
    type(state_matrix_t), intent(inout) :: state
    real(default), intent(in) :: norm
    state%norm = norm
  end subroutine state_matrix_set_norm
  
  function state_matrix_sum (state) result (value)
    complex(default) :: value
    type(state_matrix_t), intent(in) :: state
    value = sum (state%me)
  end function state_matrix_sum

  function state_matrix_trace (state, qn_in) result (trace)
    complex(default) :: trace
    type(state_matrix_t), intent(in), target :: state
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_in
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    type(state_iterator_t) :: it
    allocate (qn (state_matrix_get_depth (state)))
    trace = 0
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       qn = state_iterator_get_quantum_numbers (it)
       if (present (qn_in)) then
          if (.not. all (qn .match. qn_in)) then
             call state_iterator_advance (it);  cycle
          end if
       end if
       if (all (quantum_numbers_are_diagonal (qn))) then
          trace = trace + state_iterator_get_matrix_element (it)
       end if
       call state_iterator_advance (it)
    end do
  end function state_matrix_trace

  subroutine state_matrix_add_color_contractions (state)
    type(state_matrix_t), intent(inout), target :: state
    type(state_iterator_t) :: it
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn_con
    integer, dimension(:), allocatable :: me_index
    integer :: depth, n_me, i, j
    depth = state_matrix_get_depth (state)
    n_me = state_matrix_get_n_matrix_elements (state)
    allocate (qn (depth, n_me))
    allocate (me_index (n_me))
    i = 0
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       i = i + 1
       qn(:,i) = state_iterator_get_quantum_numbers (it)
       me_index(i) = state_iterator_get_me_index (it)
       call state_iterator_advance (it)
    end do
    do i = 1, n_me
       call quantum_number_array_make_color_contractions (qn(:,i), qn_con)
       do j = 1, size (qn_con, 2)
          call state_matrix_add_state (state, qn_con(:,j), index = me_index(i))
       end do
    end do
  end subroutine state_matrix_add_color_contractions

  subroutine merge_state_matrices (state1, state2, state3)
    type(state_matrix_t), intent(in), target :: state1, state2
    type(state_matrix_t), intent(out) :: state3
    type(state_iterator_t) :: it1, it2
    type(quantum_numbers_t), dimension(state1%depth) :: qn1, qn2
    if (state1%depth /= state2%depth) then
       call state_matrix_write (state1)
       call state_matrix_write (state2)
       call msg_bug ("State matrices merge impossible: incompatible depths")
    end if
    call state_matrix_init (state3)
    call state_iterator_init (it1, state1)
    do while (state_iterator_is_valid (it1))
       qn1 = state_iterator_get_quantum_numbers (it1)
       call state_iterator_init (it2, state2)
       do while (state_iterator_is_valid (it2))
          qn2 = state_iterator_get_quantum_numbers (it2)
          call state_matrix_add_state &
               (state3, qn1 .merge. qn2)
          call state_iterator_advance (it2)
       end do
       call state_iterator_advance (it1)
    end do
    call state_matrix_freeze (state3)
  end subroutine merge_state_matrices

  pure subroutine state_matrix_evaluate_product &
       (state, i, state1, state2, index1, index2)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    type(state_matrix_t), intent(in) :: state1, state2
    integer, dimension(:), intent(in) :: index1, index2
    state%me(i) = &
         dot_product (conjg (state1%me(index1)), state2%me(index2))
    state%norm = state1%norm * state2%norm
  end subroutine state_matrix_evaluate_product

  pure subroutine state_matrix_evaluate_product_cf &
       (state, i, state1, state2, index1, index2, factor)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    type(state_matrix_t), intent(in) :: state1, state2
    integer, dimension(:), intent(in) :: index1, index2
    complex(default), dimension(:), intent(in) :: factor
    state%me(i) = &
         dot_product (state1%me(index1), factor * state2%me(index2))
    state%norm = state1%norm * state2%norm
  end subroutine state_matrix_evaluate_product_cf

  pure subroutine state_matrix_evaluate_square_c (state, i, state1, index1)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    type(state_matrix_t), intent(in) :: state1
    integer, dimension(:), intent(in) :: index1
    state%me(i) = &
         dot_product (state1%me(index1), state1%me(index1))
    state%norm = abs (state1%norm) ** 2
  end subroutine state_matrix_evaluate_square_c

  pure subroutine state_matrix_evaluate_sum (state, i, state1, index1)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    type(state_matrix_t), intent(in) :: state1
    integer, dimension(:), intent(in) :: index1
    state%me(i) = &
         sum (state1%me(index1)) * state1%norm
  end subroutine state_matrix_evaluate_sum

  pure subroutine state_matrix_evaluate_me_sum (state, i, state1, index1)
    type(state_matrix_t), intent(inout) :: state
    integer, intent(in) :: i
    type(state_matrix_t), intent(in) :: state1
    integer, dimension(:), intent(in) :: index1
    state%me(i) = sum (state1%me(index1))
  end subroutine state_matrix_evaluate_me_sum

  subroutine outer_multiply_pair (state1, state2, state3)
    type(state_matrix_t), intent(in), target :: state1, state2
    type(state_matrix_t), intent(out) :: state3
    type(state_iterator_t) :: it1, it2
    type(quantum_numbers_t), dimension(state1%depth) :: qn1
    type(quantum_numbers_t), dimension(state2%depth) :: qn2
    type(quantum_numbers_t), dimension(state1%depth+state2%depth) :: qn3
    complex(default) :: val1, val2
    call state_matrix_init (state3, store_values=.true.)
    call state_iterator_init (it1, state1)
    do while (state_iterator_is_valid (it1))
       qn1 = state_iterator_get_quantum_numbers (it1)
       val1 = state_iterator_get_matrix_element (it1)
       call state_iterator_init (it2, state2)
       do while (state_iterator_is_valid (it2))
          qn2 = state_iterator_get_quantum_numbers (it2)
          val2 = state_iterator_get_matrix_element (it2)
          qn3(:state1%depth) = qn1
          qn3(state1%depth+1:) = qn2
          call state_matrix_add_state (state3, qn3, value=val1 * val2)
          call state_iterator_advance (it2)
       end do
       call state_iterator_advance (it1)
    end do
    call state_matrix_freeze (state3)
  end subroutine outer_multiply_pair

  subroutine outer_multiply_array (state_in, state_out)
    type(state_matrix_t), dimension(:), intent(in), target :: state_in
    type(state_matrix_t), intent(out) :: state_out
    type(state_matrix_t), dimension(:), allocatable, target :: state_tmp
    integer :: i, n
    n = size (state_in)
    select case (n)
    case (0)
       call state_matrix_init (state_out)
    case (1)
       state_out = state_in(1)
    case (2)
       call outer_multiply_pair (state_in(1), state_in(2), state_out)
    case default
       allocate (state_tmp (n-2))
       call outer_multiply_pair (state_in(1), state_in(2), state_tmp(1))
       do i = 2, n - 2
          call outer_multiply_pair (state_tmp(i-1), state_in(i+1), state_tmp(i))
       end do
       call outer_multiply_pair (state_tmp(n-2), state_in(n), state_out)
       call state_matrix_final (state_tmp)
    end select
  end subroutine outer_multiply_array

  subroutine state_matrix_factorize &
       (state, mode, x, ok, single_state, correlated_state, qn_in)
    type(state_matrix_t), intent(in), target :: state
    integer, intent(in) :: mode
    real(default), intent(in) :: x
    logical, intent(out) :: ok
    type(state_matrix_t), &
         dimension(:), allocatable, intent(out) :: single_state
    type(state_matrix_t), intent(out), optional :: correlated_state
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_in
    type(state_iterator_t) :: it
    real(default) :: s, xt
    complex(default) :: value
    integer :: i, depth
    type(quantum_numbers_t), dimension(:), allocatable :: qn, qn1
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    logical, dimension(:), allocatable :: diagonal
    logical, dimension(:,:), allocatable :: mask
    ok = .true.
    if (x /= 0) then
       xt = x * state_matrix_trace (state, qn_in)
    else
       xt = 0
    end if
    s = 0
    depth = state_matrix_get_depth (state)
    allocate (qn (depth), qn1 (depth), diagonal (depth))
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       qn = state_iterator_get_quantum_numbers (it)
       if (present (qn_in)) then
          if (.not. all (qn .fhmatch. qn_in)) then
             call state_iterator_advance (it); cycle
          end if
       end if
       if (all (quantum_numbers_are_diagonal (qn))) then
          value = state_iterator_get_matrix_element (it)
          if (real (value, default) < 0) then
             call state_matrix_write (state)
             print *, value
             call msg_bug ("Event generation: " &
                  // "Negative real part of squared matrix element value")
             value = 0
          end if
          s = s + value
          if (s > xt)  exit
       end if
       call state_iterator_advance (it)
    end do
    if (.not. state_iterator_is_valid (it)) then
       if (s == 0)  ok = .false.
       call state_iterator_init (it, state)
    end if
    allocate (single_state (depth))
    call state_matrix_init (single_state, store_values=.true.)
    if (present (correlated_state)) &
         call state_matrix_init (correlated_state, store_values=.true.)
    qn = state_iterator_get_quantum_numbers (it)
    select case (mode)
    case (FM_SELECT_HELICITY)  ! single branch selected; shortcut
       do i = 1, depth
          call state_matrix_add_state (single_state(i), &
               [qn(i)], value=value)
       end do
       if (.not. present (correlated_state)) then
          call state_matrix_freeze (single_state)
          return
       end if
    end select
    allocate (qn_mask (depth))
    call quantum_numbers_mask_init (qn_mask, .false., .false., .false., .true.)
    call quantum_numbers_undefine (qn, qn_mask)
    select case (mode)
    case (FM_FACTOR_HELICITY)
       allocate (mask (depth, depth))
       mask = .false.
       forall (i = 1:depth)  mask(i,i) = .true.
    end select
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       qn1 = state_iterator_get_quantum_numbers (it)
       if (all (qn .match. qn1)) then
          diagonal = quantum_numbers_are_diagonal (qn1)
          value = state_iterator_get_matrix_element (it)
          select case (mode)
          case (FM_IGNORE_HELICITY)  ! trace over diagonal states that match qn
             if (all (diagonal)) then
                do i = 1, depth
                   call state_matrix_add_state (single_state(i), &
                        [qn(i)], value=value, sum_values=.true.)
                end do
             end if
          case (FM_FACTOR_HELICITY)  ! trace over all other particles
             do i = 1, depth
                if (all (diagonal .or. mask(:,i))) then
                   call state_matrix_add_state (single_state(i), &
                        [qn1(i)], value=value, sum_values=.true.)
                end if
             end do
          end select
          if (present (correlated_state)) &
               call state_matrix_add_state (correlated_state, qn1, value=value)
       end if
       call state_iterator_advance (it)
    end do
    call state_matrix_freeze (single_state)
    if (present (correlated_state)) &
         call state_matrix_freeze (correlated_state)
  end subroutine state_matrix_factorize

  elemental function pacify_complex (c_in) result (c_pac)
    complex(default), intent(in) :: c_in
    complex(default) :: c_pac
    c_pac = c_in
    if (real(c_pac) == -real(c_pac)) then
       c_pac = &
            cmplx (0._default, aimag(c_pac), kind=default)
    end if
    if (aimag(c_pac) == -aimag(c_pac)) then
       c_pac = &
            cmplx (real(c_pac), 0._default, kind=default)
    end if
  end function pacify_complex
  
  subroutine state_matrix_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (state_matrix_1, "state_matrix_1", &
         "check merge of quantum states of equal depth", &
         u, results)
    call test (state_matrix_2, "state_matrix_2", &
         "check factorizing 3-particle state matrix", &
         u, results)
    call test (state_matrix_3, "state_matrix_3", &
         "check factorizing 3-particle state matrix", &
         u, results)  
  end subroutine state_matrix_test 
  

  subroutine state_matrix_1 (u)
    integer, intent(in) :: u
    type(state_matrix_t) :: state1, state2, state3
    type(flavor_t), dimension(3) :: flv
    type(color_t), dimension(3) :: col
    type(helicity_t), dimension(3) :: hel
    type(quantum_numbers_t), dimension(3) :: qn
    
    write (u, "(A)")  "* Test output: state_matrix_1"
    write (u, "(A)")  "*   Purpose: create and merge two quantum states"
    write (u, "(A)")

    write (u, "(A)")  "*  Initialization"
    write (u, "(A)")    
        
    write (u, "(A)")  "*  State matrix 1"
    write (u, "(A)")        
    
    call state_matrix_init (state1)
    call flavor_init (flv, [1, 2, 11])
    call helicity_init (hel, [1, 1, 1])
    call quantum_numbers_init (qn, flv, hel)
    call state_matrix_add_state (state1, qn)
    call helicity_init (hel, [1, 1, 1], [-1, 1, -1])
    call quantum_numbers_init (qn, flv, hel)
    call state_matrix_add_state (state1, qn)
    call state_matrix_freeze (state1)
    call state_matrix_write (state1, u)

    write (u, "(A)")
    write (u, "(A)")  "*  State matrix 2"
    write (u, "(A)")

    call state_matrix_init (state2)
    call color_init (col(1), [501])
    call color_init (col(2), [-501])
    call color_init (col(3), [0])
    call helicity_init (hel, [-1, -1, 0])
    call quantum_numbers_init (qn, col, hel)
    call state_matrix_add_state (state2, qn)
    call color_init (col(3), [99])
    call helicity_init (hel, [-1, -1, 0])
    call quantum_numbers_init (qn, col, hel)
    call state_matrix_add_state (state2, qn)
    call state_matrix_freeze (state2)
    call state_matrix_write (state2, u)

    write (u, "(A)")
    write (u, "(A)")  "* Merge the state matrices"   
    write (u, "(A)")
        
    call merge_state_matrices (state1, state2, state3)
    call state_matrix_write (state3, u)

    write (u, "(A)")
    write (u, "(A)")  "* Collapse the state matrix"    
    write (u, "(A)")
    
    call state_matrix_collapse (state3, &
         new_quantum_numbers_mask (.false., .false., &
                                   [.true.,.false.,.false.]))
    call state_matrix_write (state3, u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"    
    write (u, "(A)")    
    
    call state_matrix_final (state1)
    call state_matrix_final (state2)
    call state_matrix_final (state3)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: state_matrix_1"    
    write (u, "(A)")    
    
  end subroutine state_matrix_1

  subroutine state_matrix_2 (u)
    integer, intent(in) :: u
    type(state_matrix_t) :: state
    type(state_matrix_t), dimension(:), allocatable :: single_state
    type(state_matrix_t) :: correlated_state
    complex(default) :: z, val
    complex(default), dimension(-1:1) :: v
    integer :: f, h11, h12, h21, h22, i, mode
    type(flavor_t), dimension(2) :: flv
    type(color_t), dimension(2) :: col
    type(helicity_t), dimension(2) :: hel
    type(quantum_numbers_t), dimension(2) :: qn
    logical :: ok
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output: state_matrix_2"
    write (u, "(A)")  "*   Purpose: factorize correlated 3-particle state"
    write (u, "(A)")        
    
    write (u, "(A)")  "*  Initialization"
    write (u, "(A)")    
        
    z = 1 / 2._default
    v(-1) = (0.6_default, 0._default)
    v( 1) = (0._default, 0.8_default)
    call state_matrix_init (state)
    do f = 1, 2
       do h11 = -1, 1, 2
          do h12 = -1, 1, 2
             do h21 = -1, 1, 2
                do h22 = -1, 1, 2
                   call flavor_init (flv, [f, -f])
                   call color_init (col(1), [1])
                   call color_init (col(2), [-1])
                   call helicity_init (hel, [h11,h12], [h21, h22])
                   call quantum_numbers_init (qn, flv, col, hel)
                   val = z * v(h11) * v(h12) * conjg (v(h21) * v(h22))
                   call state_matrix_add_state (state, qn)
                end do
             end do
          end do
       end do
    end do
    call state_matrix_freeze (state)
    call state_matrix_write (state, u)

    write (u, "(A)")
    write (u, "(A,'('," // FMT_19 // ",','," // FMT_19 // ",')')") &
         "* Trace = ", state_matrix_trace (state)
    write (u, "(A)")
    
    do mode = 1, 3
       write (u, "(A)")
       write (u, "(A,I1)")  "* Mode = ", mode
       call state_matrix_factorize &
            (state, mode, 0.15_default, ok, single_state, correlated_state)
       do i = 1, size (single_state)
          write (u, "(A)")
          call state_matrix_write (single_state(i), u)
          write (u, "(A,'('," // FMT_19 // ",','," // FMT_19 // ",')')") &
               "Trace = ", state_matrix_trace (single_state(i))
       end do
       write (u, "(A)")
       call state_matrix_write (correlated_state, u)
       write (u, "(A,'('," // FMT_19 // ",','," // FMT_19 // ",')')")  &
            "Trace = ", state_matrix_trace (correlated_state)
       call state_matrix_final (single_state)
       call state_matrix_final (correlated_state)
    end do
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call state_matrix_final (state)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: state_matrix_2"
    
  end subroutine state_matrix_2

  subroutine state_matrix_3 (u)
    use physics_defs, only: HADRON_REMNANT_TRIPLET, HADRON_REMNANT_OCTET
    integer, intent(in) :: u
    type(state_matrix_t) :: state
    type(flavor_t), dimension(4) :: flv
    type(color_t), dimension(4) :: col
    type(quantum_numbers_t), dimension(4) :: qn
    
    write (u, "(A)")  "* Test output: state_matrix_3"
    write (u, "(A)")  "*   Purpose: add color connections to colored state"
    write (u, "(A)")    
       
    write (u, "(A)")  "*  Initialization"
    write (u, "(A)")    
    
    call state_matrix_init (state)
    call flavor_init (flv, &
         [ 1, -HADRON_REMNANT_TRIPLET, -1, HADRON_REMNANT_TRIPLET ])
    call color_init (col(1), [17])
    call color_init (col(2), [-17])
    call color_init (col(3), [-19])
    call color_init (col(4), [19])
    call quantum_numbers_init (qn, flv=flv, col=col)
    call state_matrix_add_state (state, qn)
    call flavor_init (flv, &
         [ 1, -HADRON_REMNANT_TRIPLET, 21, HADRON_REMNANT_OCTET ])
    call color_init (col(1), [17])
    call color_init (col(2), [-17])
    call color_init (col(3), [3, -5])
    call color_init (col(4), [5, -3])
    call quantum_numbers_init (qn, flv=flv, col=col)
    call state_matrix_add_state (state, qn)
    call state_matrix_freeze (state)

    write (u, "(A)") "* State:"
    write (u, "(A)") 
    
    call state_matrix_write (state, u)
    call state_matrix_add_color_contractions (state)

    write (u, "(A)") "* State with contractions:"
    write (u, "(A)")
    
    call state_matrix_write (state, u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
        
    call state_matrix_final (state)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: state_matrx_3"    
    
  end subroutine state_matrix_3


end module state_matrices
