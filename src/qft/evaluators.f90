! WHIZARD 2.2.4 Feb 06 2015
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

module evaluators

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_defs, only: FMT_19
  use unit_tests
  use diagnostics
  use lorentz
  use model_data
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices
  use interactions

  implicit none
  private

  public :: evaluator_t
  public :: assignment(=)
  public :: evaluator_init_product
  public :: evaluator_init_square
  public :: evaluator_init_square_diag
  public :: evaluator_init_square_nondiag
  public :: evaluator_init_color_contractions
  public :: evaluator_get_int_ptr
  public :: evaluator_is_empty
  public :: evaluator_get_tag
  public :: evaluator_get_norm
  public :: evaluator_get_n_tot
  public :: evaluator_get_n_in
  public :: evaluator_get_n_vir
  public :: evaluator_get_n_out
  public :: evaluator_get_matrix_element
  public :: evaluator_sum
  public :: evaluator_normalize_by_trace
  public :: evaluator_normalize_by_max
  public :: evaluator_add_color_contractions
  public :: evaluator_get_mask
  public :: interaction_set_source_link
  public :: evaluator_set_source_link
  public :: evaluator_receive_momenta
  public :: evaluator_send_momenta
  public :: evaluator_reassign_links
  public :: evaluator_get_unstable_particle
  public :: evaluator_final
  public :: evaluator_init_identity
  public :: evaluator_init_qn_sum
  public :: evaluator_test

  integer, parameter :: &
       EVAL_UNDEFINED = 0, &
       EVAL_PRODUCT = 1, &
       EVAL_SQUARED_FLOWS = 2, &
       EVAL_SQUARE_WITH_COLOR_FACTORS = 3, &
       EVAL_COLOR_CONTRACTION = 4, &
       EVAL_IDENTITY = 5, &
       EVAL_QN_SUM = 6

  type :: pairing_array_t
     integer, dimension(:), allocatable :: i1, i2
     complex(default), dimension(:), allocatable :: factor
  end type pairing_array_t
     
  type :: evaluator_t
     private
     integer :: type = EVAL_UNDEFINED
     type(interaction_t), pointer :: int_in1 => null ()
     type(interaction_t), pointer :: int_in2 => null ()
     type(interaction_t) :: int
     type(pairing_array_t), dimension(:), allocatable :: pairing_array
   contains
     procedure :: write => evaluator_write
     procedure :: evaluate => evaluator_evaluate
  end type evaluator_t

  type :: index_map_t
     integer, dimension(:), allocatable :: entry
  end type index_map_t

  type :: index_map2_t
     integer :: s = 0
     integer, dimension(:,:), allocatable :: entry
  end type index_map2_t

  type :: prt_mask_t
     logical, dimension(:), allocatable :: entry
  end type prt_mask_t

  type :: qn_list_t
     type(quantum_numbers_t), dimension(:,:), allocatable :: qn
  end type qn_list_t

  type :: qn_mask_array_t
     type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
  end type qn_mask_array_t

  type :: connection_entry_t
     type(quantum_numbers_t), dimension(:), allocatable :: qn_conn
     integer, dimension(:), allocatable :: n_index
     integer, dimension(:), allocatable :: count
     type(index_map_t), dimension(:), allocatable :: index_in
     type(qn_list_t), dimension(:), allocatable :: qn_in_list
  end type connection_entry_t

  type :: color_table_t
     integer, dimension(:), allocatable :: index
     type(color_t), dimension(:,:), allocatable :: col
     logical, dimension(:,:), allocatable :: factor_is_known
     complex(default), dimension(:,:), allocatable :: factor
  end type color_table_t


  interface assignment(=)
     module procedure evaluator_assign
  end interface

  interface size
     module procedure index_map_size
  end interface

  interface assignment(=)
     module procedure index_map_assign_int
     module procedure index_map_assign_array
  end interface

  interface size
     module procedure index_map2_size
  end interface

  interface assignment(=)
     module procedure index_map2_assign_int
  end interface

  interface size
     module procedure prt_mask_size
  end interface

  interface evaluator_init_product
     module procedure evaluator_init_product_ii
     module procedure evaluator_init_product_ie
     module procedure evaluator_init_product_ei
     module procedure evaluator_init_product_ee
  end interface

  interface interaction_set_source_link
     module procedure interaction_set_source_link_eval
  end interface
  interface evaluator_set_source_link
     module procedure evaluator_set_source_link_int
     module procedure evaluator_set_source_link_eval
  end interface
  interface evaluator_reassign_links
     module procedure evaluator_reassign_links_eval
     module procedure evaluator_reassign_links_int
  end interface

  interface evaluator_init_identity
     module procedure evaluator_init_identity_i
     module procedure evaluator_init_identity_e
  end interface

  interface evaluator_init_qn_sum
     module procedure evaluator_init_qn_sum_i
     module procedure evaluator_init_qn_sum_e
  end interface

contains

  elemental subroutine pairing_array_init (pa, n, has_i2, has_factor)
    type(pairing_array_t), intent(out) :: pa
    integer, intent(in) :: n
    logical, intent(in) :: has_i2, has_factor
    allocate (pa%i1 (n))
    if (has_i2)  allocate (pa%i2 (n))
    if (has_factor)  allocate (pa%factor (n))
  end subroutine pairing_array_init

  subroutine evaluator_write (eval, unit, &
       verbose, show_momentum_sum, show_mass, show_state, show_table, testflag)
    class(evaluator_t), intent(in) :: eval
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    logical, intent(in), optional :: show_state, show_table, testflag
    logical :: conjugate, square, show_tab
    integer :: u, i, j
    u = given_output_unit (unit);  if (u < 0)  return
    show_tab = .true.;  if (present (show_table))  show_tab = .false.
    ! write (u, "(1x,A)")  "Evaluator:"     !!! Debugging
    call interaction_write &
         (eval%int, unit, verbose, show_momentum_sum, show_mass, & 
            show_state, testflag)
    if (show_tab) then
       write (u, "(1x,A)")  "Matrix-element multiplication"
       write (u, "(2x,A)", advance="no")  "Input interaction 1:"
       if (associated (eval%int_in1)) then
          write (u, "(1x,I0)")  interaction_get_tag (eval%int_in1)
       else
          write (u, "(A)")  " [undefined]"
       end if
       write (u, "(2x,A)", advance="no")  "Input interaction 2:"
       if (associated (eval%int_in2)) then
          write (u, "(1x,I0)")  interaction_get_tag (eval%int_in2)
       else
          write (u, "(A)")  " [undefined]"
       end if
       select case (eval%type)
       case (EVAL_SQUARED_FLOWS, EVAL_SQUARE_WITH_COLOR_FACTORS)
          conjugate = .true.
          square = .true.
       case default
          conjugate = .false.
          square = .false.
       end select
       if (eval%type == EVAL_IDENTITY) then
          write (u, "(1X,A)") "Identity evaluator, pairing array unused"
          return
       end if
       if (allocated (eval%pairing_array)) then
          do i = 1, size (eval%pairing_array)
             write (u, "(2x,A,I0,A)")  "ME(", i, ") = "
             do j = 1, size (eval%pairing_array(i)%i1)
                write (u, "(4x,A)", advance="no")  "+"
                if (allocated (eval%pairing_array(i)%i2)) then
                   write (u, "(1x,A,I0,A)", advance="no")  &
                        "ME1(", eval%pairing_array(i)%i1(j), ")"
                   if (conjugate) then
                      write (u, "(A)", advance="no")  "* x"
                   else
                      write (u, "(A)", advance="no")  " x"
                   end if
                   write (u, "(1x,A,I0,A)", advance="no")  &
                        "ME2(", eval%pairing_array(i)%i2(j), ")"
                else if (square) then
                   write (u, "(1x,A)", advance="no")  "|"
                   write (u, "(A,I0,A)", advance="no")  &
                        "ME1(", eval%pairing_array(i)%i1(j), ")"
                   write (u, "(A)", advance="no")  "|^2"
                else
                   write (u, "(1x,A,I0,A)", advance="no")  &
                        "ME1(", eval%pairing_array(i)%i1(j), ")"
                end if
                if (allocated (eval%pairing_array(i)%factor)) then
                   write (u, "(1x,A)", advance="no")  "x"
                   write (u, "(1x,'('," // FMT_19 // ",','," // FMT_19 // &
                        ",')')") eval%pairing_array(i)%factor(j)
                else
                   write (u, *)
                end if
             end do
          end do
       end if
       ! print *, size (eval%pairing_array)     !!! Debugging
    end if
  end subroutine evaluator_write

  subroutine evaluator_assign (eval_out, eval_in)
    type(evaluator_t), intent(out) :: eval_out
    type(evaluator_t), intent(in) :: eval_in
    eval_out%type = eval_in%type
    eval_out%int_in1 => eval_in%int_in1
    eval_out%int_in2 => eval_in%int_in2
    eval_out%int = eval_in%int
    if (allocated (eval_in%pairing_array)) then
       allocate (eval_out%pairing_array (size (eval_in%pairing_array)))
       eval_out%pairing_array = eval_in%pairing_array
    end if
  end subroutine evaluator_assign

  elemental subroutine index_map_init (map, n)
    type(index_map_t), intent(out) :: map
    integer, intent(in) :: n
    allocate (map%entry (n))
  end subroutine index_map_init
    
  function index_map_exists (map) result (flag)
    logical :: flag
    type(index_map_t), intent(in) :: map
    flag = allocated (map%entry)
  end function index_map_exists

  function index_map_size (map) result (s)
    integer :: s
    type(index_map_t), intent(in) :: map
    if (allocated (map%entry)) then
       s = size (map%entry)
    else
       s = 0
    end if
  end function index_map_size

  elemental subroutine index_map_assign_int (map, ival)
    type(index_map_t), intent(inout) :: map
    integer, intent(in) :: ival
    map%entry = ival
  end subroutine index_map_assign_int

  subroutine index_map_assign_array (map, array)
    type(index_map_t), intent(inout) :: map
    integer, dimension(:), intent(in) :: array
    map%entry = array
  end subroutine index_map_assign_array

  elemental subroutine index_map_set_entry (map, i, ival)
    type(index_map_t), intent(inout) :: map
    integer, intent(in) :: i
    integer, intent(in) :: ival
    map%entry(i) = ival
  end subroutine index_map_set_entry
    
  elemental function index_map_get_entry (map, i) result (ival)
    integer :: ival
    type(index_map_t), intent(in) :: map
    integer, intent(in) :: i
    ival = map%entry(i)
  end function index_map_get_entry
    
  elemental subroutine index_map2_init (map, n)
    type(index_map2_t), intent(out) :: map
    integer, intent(in) :: n
    map%s = n
    allocate (map%entry (n, n))
  end subroutine index_map2_init
    
  function index_map2_exists (map) result (flag)
    logical :: flag
    type(index_map2_t), intent(in) :: map
    flag = allocated (map%entry)
  end function index_map2_exists

  function index_map2_size (map) result (s)
    integer :: s
    type(index_map2_t), intent(in) :: map
    s = map%s
  end function index_map2_size

  elemental subroutine index_map2_assign_int (map, ival)
    type(index_map2_t), intent(inout) :: map
    integer, intent(in) :: ival
    map%entry = ival
  end subroutine index_map2_assign_int

  elemental subroutine index_map2_set_entry (map, i, j, ival)
    type(index_map2_t), intent(inout) :: map
    integer, intent(in) :: i, j
    integer, intent(in) :: ival
    map%entry(i,j) = ival
  end subroutine index_map2_set_entry
    
  elemental function index_map2_get_entry (map, i, j) result (ival)
    integer :: ival
    type(index_map2_t), intent(in) :: map
    integer, intent(in) :: i, j
    ival = map%entry(i,j)
  end function index_map2_get_entry
    
  subroutine prt_mask_init (mask, n)
    type(prt_mask_t), intent(out) :: mask
    integer, intent(in) :: n
    allocate (mask%entry (n))
  end subroutine prt_mask_init

  function prt_mask_size (mask) result (s)
    integer :: s
    type(prt_mask_t), intent(in) :: mask
    s = size (mask%entry)
  end function prt_mask_size

  subroutine connection_entry_init &
      (entry, n_count, n_map, qn_conn, count, n_rest)
    type(connection_entry_t), intent(out) :: entry
    integer, intent(in) :: n_count, n_map
    type(quantum_numbers_t), dimension(:), intent(in) :: qn_conn
    integer, dimension(n_count), intent(in) :: count
    integer, dimension(n_count), intent(in) :: n_rest
    integer :: i
    allocate (entry%qn_conn (size (qn_conn)))
    allocate (entry%n_index (n_count))
    allocate (entry%count (n_count))
    allocate (entry%index_in (n_map))
    allocate (entry%qn_in_list (n_count))
    entry%qn_conn = qn_conn
    entry%n_index = count
    entry%count = 0
    if (size (entry%index_in) == size (count)) then
       call index_map_init (entry%index_in, count)
    else
       call index_map_init (entry%index_in, count(1))
    end if
    do i = 1, n_count
      allocate (entry%qn_in_list(i)%qn (n_rest(i), count(i)))
    end do
  end subroutine connection_entry_init

  subroutine connection_entry_write (entry, unit)
    type(connection_entry_t), intent(in) :: entry
    integer, intent(in), optional :: unit
    integer :: i, j
    integer :: u
    u = given_output_unit (unit)
    call quantum_numbers_write (entry%qn_conn, unit)
    write (u, *)
    do i = 1, size (entry%n_index)
       write (u, *)  "Input interaction", i
       do j = 1, entry%n_index(i)
          if (size (entry%n_index) == size (entry%index_in)) then
             write (u, "(2x,I0,4x,I0,2x)", advance = "no") &
                  j, index_map_get_entry (entry%index_in(i), j)
          else
             write (u, "(2x,I0,4x,I0,2x,I0,2x)", advance = "no") &
                  j, index_map_get_entry (entry%index_in(1), j), &
                     index_map_get_entry (entry%index_in(2), j)
          end if
          call quantum_numbers_write (entry%qn_in_list(i)%qn(:,j), unit)
          write (u, *)
       end do
    end do
  end subroutine connection_entry_write

  subroutine color_table_init (color_table, state, n_tot)
    type(color_table_t), intent(out) :: color_table
    type(state_matrix_t), intent(in) :: state
    integer, intent(in) :: n_tot
    type(state_iterator_t) :: it
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    type(state_matrix_t) :: state_col
    integer :: index, n_col_state
    allocate (color_table%index &
         (state_matrix_get_n_matrix_elements (state)))
    color_table%index = 0
    allocate (qn (n_tot))
    call state_matrix_init (state_col)
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       index = state_iterator_get_me_index (it)
       call quantum_numbers_init (qn, col = state_iterator_get_color (it))
       call state_matrix_add_state (state_col, qn, &
            me_index = color_table%index(index))
       call state_iterator_advance (it)
    end do
    n_col_state = state_matrix_get_n_matrix_elements (state_col)
    allocate (color_table%col (n_tot, n_col_state))
    call state_iterator_init (it, state_col)
    do while (state_iterator_is_valid (it))
       index = state_iterator_get_me_index (it)
       color_table%col(:,index) = state_iterator_get_color (it)
       call state_iterator_advance (it)
    end do
    call state_matrix_final (state_col)
    allocate (color_table%factor_is_known (n_col_state, n_col_state))
    allocate (color_table%factor (n_col_state, n_col_state))
    color_table%factor_is_known = .false.
  end subroutine color_table_init

  subroutine color_table_write (color_table, unit)
    type(color_table_t), intent(in) :: color_table
    integer, intent(in), optional :: unit
    integer :: i, j
    integer :: u
    u = given_output_unit (unit)
    write (u, *) "Color table:"
    if (allocated (color_table%index)) then
       write (u, *) "  Index mapping state => color table:"
       do i = 1, size (color_table%index)
          write (u, "(3x,I0,2x,I0,2x)")  i, color_table%index(i)
       end do
       write (u, *) "  Color table:"
       do i = 1, size (color_table%col, 2)
          write (u, "(3x,I0,2x)", advance = "no")  i
          call color_write (color_table%col(:,i), unit)
          write (u, *)
       end do
       write (u, *) "  Defined color factors:"
       do i = 1, size (color_table%factor, 1)
          do j = 1, size (color_table%factor, 2)
             if (color_table%factor_is_known(i,j)) then
                write (u, *)  i, j, color_table%factor(i,j)
             end if
          end do
       end do
    end if
  end subroutine color_table_write

  subroutine color_table_set_color_factors (color_table, &
       col_flow_index, col_factor, col_index_hi)
    type(color_table_t), intent(inout) :: color_table
    integer, dimension(:,:), intent(in) :: col_flow_index
    complex(default), dimension(:), intent(in) :: col_factor
    integer, dimension(:), intent(in) :: col_index_hi
    integer, dimension(:), allocatable :: hi_to_ct
    integer :: n_cflow
    integer :: hi_index, me_index, ct_index, cf_index
    integer, dimension(2) :: hi_index_pair, ct_index_pair
    n_cflow = size (col_index_hi)
    if (size (color_table%index) /= n_cflow) &
         call msg_bug ("Mismatch between hard matrix element and color table")
    allocate (hi_to_ct (n_cflow))
    do me_index = 1, size (color_table%index)
       ct_index = color_table%index(me_index)
       hi_index = col_index_hi(me_index)
       hi_to_ct(hi_index) = ct_index
    end do
    do cf_index = 1, size (col_flow_index, 2)
       hi_index_pair = col_flow_index(:,cf_index)
       ct_index_pair = hi_to_ct(hi_index_pair)
       color_table%factor(ct_index_pair(1), ct_index_pair(2)) = &
            col_factor(cf_index)
       color_table%factor_is_known(ct_index_pair(1), ct_index_pair(2)) = .true.
    end do
  end subroutine color_table_set_color_factors

  function color_table_get_color_factor (color_table, index1, index2, nc) &
      result (factor)
    real(default) :: factor
    type(color_table_t), intent(inout) :: color_table
    integer, intent(in) :: index1, index2
    integer, intent(in), optional :: nc
    integer :: i1, i2
    ! print *, "compute color factor ", index1, index2   !!! Debugging
    i1 = color_table%index(index1)
    i2 = color_table%index(index2)
    ! print *, "  indices = ", i1, i2                    !!! Debugging
    if (color_table%factor_is_known(i1,i2)) then
       factor = color_table%factor(i1,i2)
       ! print *, "  is known : ", factor                !!! Debugging
    else
       factor = compute_color_factor &
            (color_table%col(:,i1), color_table%col(:,i2), nc)
       color_table%factor(i1,i2) = factor
       color_table%factor_is_known(i1,i2) = .true.
       ! print *, "  computed : ", factor                !!! Debugging 
    end if
  end function color_table_get_color_factor

  subroutine evaluator_init_product_ii &
       (eval, int_in1, int_in2, qn_mask_conn, qn_mask_rest, &
        connections_are_resonant)

    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in1, int_in2
    type(quantum_numbers_mask_t), intent(in) :: qn_mask_conn
    type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_rest
    logical, intent(in), optional :: connections_are_resonant

    type(qn_mask_array_t), dimension(2) :: qn_mask_in

    type :: connection_table_t
       integer :: n_conn = 0
       integer, dimension(2) :: n_rest = 0
       integer :: n_tot = 0
       integer :: n_me_conn = 0
       type(state_matrix_t) :: state
       type(index_map_t), dimension(:), allocatable :: index_conn
       type(connection_entry_t), dimension(:), allocatable :: entry
       type(index_map_t) :: index_result
    end type connection_table_t
    type(connection_table_t) :: connection_table

    integer :: n_in, n_vir, n_out, n_tot
    integer, dimension(2) :: n_rest
    integer :: n_conn

    integer, dimension(:,:), allocatable :: connection_index
    type(index_map_t), dimension(2) :: prt_map_in
    type(index_map_t) :: prt_map_conn
    type(prt_mask_t), dimension(2) :: prt_is_connected
    type(quantum_numbers_mask_t), dimension(:), allocatable :: &
         qn_mask_conn_initial

    integer :: i

    eval%type = EVAL_PRODUCT
    eval%int_in1 => int_in1
    eval%int_in2 => int_in2
    ! print *, "Evaluator product"          !!! Debugging      
    ! print *, "First interaction"          !!! Debugging      
    ! call interaction_write (int_in1)      !!! Debugging      
    ! print *                               !!! Debugging      
    ! print *, "Second interaction"         !!! Debugging      
    ! call interaction_write (int_in2)      !!! Debugging      
    ! print *                               !!! Debugging      

    call find_connections (int_in1, int_in2, n_conn, connection_index)
    if (n_conn == 0) then
       call msg_message ("First interaction:")
       call interaction_write (int_in1)
       call msg_message ("Second interaction:")
       call interaction_write (int_in2)
       call msg_fatal ("Evaluator product: no connections found between factors")
    end if
    call compute_index_bounds_and_mappings &
         (int_in1, int_in2, n_conn, &
          n_in, n_vir, n_out, n_tot, &
          n_rest, prt_map_in, prt_map_conn)

    call prt_mask_init (prt_is_connected(1), interaction_get_n_tot (int_in1))
    call prt_mask_init (prt_is_connected(2), interaction_get_n_tot (int_in2))
    do i = 1, 2
       prt_is_connected(i)%entry = .true.
       prt_is_connected(i)%entry(connection_index(:,i)) = .false.
    end do
    allocate (qn_mask_conn_initial (n_conn))
    qn_mask_conn_initial = &
         interaction_get_mask (int_in1, connection_index(:,1)) .or. &
         interaction_get_mask (int_in2, connection_index(:,2))
    allocate (qn_mask_in(1)%mask (interaction_get_n_tot (int_in1)))
    allocate (qn_mask_in(2)%mask (interaction_get_n_tot (int_in2)))
    qn_mask_in(1)%mask = interaction_get_mask (int_in1)
    qn_mask_in(2)%mask = interaction_get_mask (int_in2)

    call connection_table_init (connection_table, &
         interaction_get_state_matrix_ptr (int_in1), &
         interaction_get_state_matrix_ptr (int_in2), &
         qn_mask_conn_initial,  &
         n_conn, connection_index, n_rest)
    call connection_table_fill (connection_table, &
         interaction_get_state_matrix_ptr (int_in1), &
         interaction_get_state_matrix_ptr (int_in2), &
         connection_index, prt_is_connected)
    call make_product_interaction (eval%int, &
         n_in, n_vir, n_out, &
         connection_table, &
         prt_map_in, prt_is_connected, &
         qn_mask_in, qn_mask_conn_initial, qn_mask_conn, qn_mask_rest)
    ! call connection_table_write (connection_table)    !!! Debugging
    call make_pairing_array (eval%pairing_array, &
         interaction_get_n_matrix_elements (eval%int), &
         connection_table)
    call record_links (eval%int, &
         int_in1, int_in2, connection_index, prt_map_in, prt_map_conn, &
         prt_is_connected, connections_are_resonant)
    call connection_table_final (connection_table)

    ! print *, "Result evaluator"                !!! Debugging
    ! call evaluator_write (eval)                !!! Debugging

    if (interaction_get_n_matrix_elements (eval%int) == 0) then
       print *, "Evaluator product"
       print *, "First interaction"
       call interaction_write (int_in1)
       print *
       print *, "Second interaction"
       call interaction_write (int_in2)
       print *
       call msg_fatal ("Product of density matrices is empty", &
           [var_str ("   --------------------------------------------"), &
            var_str ("This happens when two density matrices are convoluted "), &
            var_str ("but the processes they belong to (e.g., production "), &
            var_str ("and decay) do not match. This could happen if the "), &
            var_str ("beam specification does not match the hard "), &
            var_str ("process. Or it may indicate a WHIZARD bug.")])
    end if

  contains

    subroutine compute_index_bounds_and_mappings &
         (int1, int2, n_conn, &
          n_in, n_vir, n_out, n_tot, &
          n_rest, prt_map_in, prt_map_conn)
      type(interaction_t), intent(in) :: int1, int2
      integer, intent(in) :: n_conn
      integer, intent(out) :: n_in, n_vir, n_out, n_tot
      integer, dimension(2), intent(out) :: n_rest
      type(index_map_t), dimension(2), intent(out) :: prt_map_in
      type(index_map_t), intent(out) :: prt_map_conn
      integer, dimension(:), allocatable :: index
      integer :: n_in1, n_vir1, n_out1
      integer :: n_in2, n_vir2, n_out2
      integer :: k
      n_in1  = interaction_get_n_in  (int1)
      n_vir1 = interaction_get_n_vir (int1)
      n_out1 = interaction_get_n_out (int1) - n_conn
      n_rest(1) = n_in1 + n_vir1 + n_out1
      n_in2  = interaction_get_n_in  (int2) - n_conn
      n_vir2 = interaction_get_n_vir (int2)
      n_out2 = interaction_get_n_out (int2)
      n_rest(2) = n_in2 + n_vir2 + n_out2
      n_in  = n_in1  + n_in2
      n_vir = n_vir1 + n_vir2 + n_conn
      n_out = n_out1 + n_out2
      n_tot = n_in + n_vir + n_out
      call index_map_init (prt_map_in, n_rest)
      call index_map_init (prt_map_conn, n_conn)
      allocate (index (n_tot))
      index = [ (i, i = 1, n_tot) ]
      prt_map_in(1)%entry(1 : n_in1) = index(  1 :   n_in1)
      k =     n_in1
      prt_map_in(2)%entry(1 : n_in2) = index(k+1 : k+n_in2)
      k = k + n_in2
      prt_map_in(1)%entry(n_in1+1 : n_in1+n_vir1) = index(k+1 : k+n_vir1)
      k = k + n_vir1
      prt_map_in(2)%entry(n_in2+1 : n_in2+n_vir2) = index(k+1 : k+n_vir2)
      k = k + n_vir2
      prt_map_conn%entry = index(k+1 : k+n_conn)
      k = k + n_conn
      prt_map_in(1)%entry(n_in1+n_vir1+1 : n_rest(1)) = index(k+1 : k+n_out1)
      k = k + n_out1
      prt_map_in(2)%entry(n_in2+n_vir2+1 : n_rest(2)) = index(k+1 : k+n_out2)
    end subroutine compute_index_bounds_and_mappings

    subroutine connection_table_init &
        (connection_table, state_in1, state_in2, qn_mask_conn, &
         n_conn, connection_index, n_rest)
      type(connection_table_t), intent(out) :: connection_table
      type(state_matrix_t), intent(in), target :: state_in1, state_in2
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_conn
      integer, intent(in) :: n_conn
      integer, dimension(:,:), intent(in) :: connection_index
      integer, dimension(2), intent(in) :: n_rest
      integer, dimension(2) :: n_me_in
      type(state_iterator_t) :: it
      type(quantum_numbers_t), dimension(n_conn) :: qn
      integer :: i, me_index_in, me_index_conn, n_me_conn
      integer, dimension(2) :: me_count
      connection_table%n_conn = n_conn
      connection_table%n_rest = n_rest
      n_me_in(1) = state_matrix_get_n_matrix_elements (state_in1)
      n_me_in(2) = state_matrix_get_n_matrix_elements (state_in2)
      allocate (connection_table%index_conn (2))
      call index_map_init (connection_table%index_conn, n_me_in)
      connection_table%index_conn = 0
      call state_matrix_init (connection_table%state, n_counters=2)
      do i = 1, 2
         select case (i)
         case (1);  call state_iterator_init (it, state_in1)
         case (2);  call state_iterator_init (it, state_in2)
         end select
         do while (state_iterator_is_valid (it))
            qn = state_iterator_get_quantum_numbers (it, connection_index(:,i))
            call quantum_numbers_undefine (qn, qn_mask_conn)
            call quantum_numbers_canonicalize_color (qn)
            me_index_in = state_iterator_get_me_index (it)
            call state_matrix_add_state (connection_table%state, qn, &
                counter_index = i, me_index = me_index_conn)
            call index_map_set_entry (connection_table%index_conn(i), &
                 me_index_in, me_index_conn)
            call state_iterator_advance (it)
         end do
      end do
      n_me_conn = state_matrix_get_n_matrix_elements (connection_table%state)
      connection_table%n_me_conn = n_me_conn
      allocate (connection_table%entry (n_me_conn))
      call state_iterator_init (it, connection_table%state)
      do while (state_iterator_is_valid (it))
         i = state_iterator_get_me_index (it)
         me_count = state_iterator_get_me_count (it)
         call connection_entry_init (connection_table%entry(i), 2, 2, &
              state_iterator_get_quantum_numbers (it), &
              me_count, n_rest)
         call state_iterator_advance (it)
      end do
    end subroutine connection_table_init

    subroutine connection_table_final (connection_table)
      type(connection_table_t), intent(inout) :: connection_table
      call state_matrix_final (connection_table%state)
    end subroutine connection_table_final
   
    subroutine connection_table_write (connection_table, unit)
      type(connection_table_t), intent(in) :: connection_table
      integer, intent(in), optional :: unit
      integer :: i, j
      integer :: u
      u = given_output_unit (unit)
      write (u, *) "Connection table:"
      call state_matrix_write (connection_table%state, unit)
      if (allocated (connection_table%index_conn)) then
         write (u, *) "  Index mapping input => connection table:"
         do i = 1, size (connection_table%index_conn)
            write (u, *) "  Input state", i
            do j = 1, size (connection_table%index_conn(i))
               write (u, *)  j, &
                    index_map_get_entry (connection_table%index_conn(i), j)
            end do
         end do
      end if
      if (allocated (connection_table%entry)) then
         write (u, *) "  Connection table contents:"
         do i = 1, size (connection_table%entry)
            call connection_entry_write (connection_table%entry(i), unit)
         end do
      end if
      if (index_map_exists (connection_table%index_result)) then
         write (u, *) "  Index mapping connection table => output:"
         do i = 1, size (connection_table%index_result)
            write (u, *)  i, &
                 index_map_get_entry (connection_table%index_result, i)
         end do
      end if
    end subroutine connection_table_write

    subroutine connection_table_fill &
        (connection_table, state_in1, state_in2, &
         connection_index, prt_is_connected)
      type(connection_table_t), intent(inout) :: connection_table
      type(state_matrix_t), intent(in), target :: state_in1, state_in2
      integer, dimension(:,:), intent(in) :: connection_index
      type(prt_mask_t), dimension(2), intent(in) :: prt_is_connected
      type(state_iterator_t) :: it
      integer :: index_in, index_conn
      integer :: color_offset
      integer :: n_result_entries
      integer :: i, k
      color_offset = state_matrix_get_max_color_value (connection_table%state)
      do i = 1, 2
         select case (i)
         case (1);  call state_iterator_init (it, state_in1)
         case (2);  call state_iterator_init (it, state_in2)
         end select
         do while (state_iterator_is_valid (it))
            index_in = state_iterator_get_me_index (it)
            index_conn = index_map_get_entry &
                              (connection_table%index_conn(i), index_in)
            if (index_conn /= 0) then
               call connection_entry_add_state &
                    (connection_table%entry(index_conn), i, &
                     index_in, &
                     state_iterator_get_quantum_numbers (it), &
                     connection_index(:,i), prt_is_connected(i), &
                     color_offset)
            end if
            call state_iterator_advance (it)
         end do
         color_offset = color_offset &
              + state_matrix_get_max_color_value (state_in1)
      end do
      n_result_entries = 0
      do k = 1, size (connection_table%entry)
         n_result_entries = &
              n_result_entries + product (connection_table%entry(k)%n_index)
      end do
      call index_map_init (connection_table%index_result, n_result_entries)
    end subroutine connection_table_fill
      
    subroutine connection_entry_add_state &
        (entry, i, index_in, qn_in, connection_index, prt_is_connected, &
         color_offset)
      type(connection_entry_t), intent(inout) :: entry
      integer, intent(in) :: i
      integer, intent(in) :: index_in
      type(quantum_numbers_t), dimension(:), intent(in) :: qn_in
      integer, dimension(:), intent(in) :: connection_index
      type(prt_mask_t), intent(in) :: prt_is_connected
      integer, intent(in) :: color_offset
      integer :: c
      integer, dimension(:,:), allocatable :: color_map
      entry%count(i) = entry%count(i) + 1
      c = entry%count(i)
      call quantum_numbers_set_color_map &
           (color_map, qn_in(connection_index), entry%qn_conn)
      call index_map_set_entry (entry%index_in(i), c, index_in)
      entry%qn_in_list(i)%qn(:,c) = pack (qn_in, prt_is_connected%entry)
      call quantum_numbers_translate_color &
           (entry%qn_in_list(i)%qn(:,c), color_map, color_offset)
    end subroutine connection_entry_add_state

    subroutine make_product_interaction (int, &
         n_in, n_vir, n_out, &
         connection_table, &
         prt_map_in, prt_is_connected, &
         qn_mask_in, qn_mask_conn_initial, qn_mask_conn, qn_mask_rest)
      type(interaction_t), intent(out), target :: int
      integer, intent(in) :: n_in, n_vir, n_out
      type(connection_table_t), intent(inout), target :: connection_table
      type(index_map_t), dimension(2), intent(in) :: prt_map_in
      type(prt_mask_t), dimension(2), intent(in) :: prt_is_connected
      type(qn_mask_array_t), dimension(2), intent(in) :: qn_mask_in
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: &
           qn_mask_conn_initial
      type(quantum_numbers_mask_t), intent(in) :: qn_mask_conn
      type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_rest
      type(index_map_t), dimension(2) :: prt_index_in
      type(index_map_t) :: prt_index_conn
      integer :: n_tot, n_conn
      integer, dimension(2) :: n_rest
      integer :: i, j, k, m
      type(quantum_numbers_t), dimension(:), allocatable :: qn
      type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
      type(connection_entry_t), pointer :: entry
      integer :: result_index
      n_conn = connection_table%n_conn
      n_rest = connection_table%n_rest
      n_tot = sum (n_rest) + n_conn
      allocate (qn (n_tot), qn_mask (n_tot))
      do i = 1, 2
         call index_map_init (prt_index_in(i), n_rest(i))
         prt_index_in(i) = &
              prt_map_in(i)%entry ([ (j, j = 1, n_rest(i)) ])
      end do
      call index_map_init (prt_index_conn, n_conn)
      prt_index_conn = prt_map_conn%entry ([ (j, j = 1, n_conn) ])
      do i = 1, 2
         if (present (qn_mask_rest)) then
            qn_mask(prt_index_in(i)%entry) = &
                 pack (qn_mask_in(i)%mask, prt_is_connected(i)%entry) &
                 .or. qn_mask_rest
         else
            qn_mask(prt_index_in(i)%entry) = &
                 pack (qn_mask_in(i)%mask, prt_is_connected(i)%entry)
         end if
      end do
      qn_mask(prt_index_conn%entry) = qn_mask_conn_initial .or. qn_mask_conn
      call interaction_init (eval%int, n_in, n_vir, n_out, mask=qn_mask)
      m = 1
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         qn(prt_index_conn%entry) = &
              quantum_numbers_undefined (entry%qn_conn, qn_mask_conn)
         do j = 1, entry%n_index(1)
            qn(prt_index_in(1)%entry) = entry%qn_in_list(1)%qn(:,j)
            do k = 1, entry%n_index(2)
               qn(prt_index_in(2)%entry) = entry%qn_in_list(2)%qn(:,k)
               call interaction_add_state (int, qn, me_index = result_index)
               call index_map_set_entry &
                    (connection_table%index_result, m, result_index)
               m = m + 1
            end do
         end do
      end do
      call interaction_freeze (int)
    end subroutine make_product_interaction

    subroutine make_pairing_array (pa, n_matrix_elements, connection_table)
      type(pairing_array_t), dimension(:), intent(out), allocatable :: pa
      integer, intent(in) :: n_matrix_elements
      type(connection_table_t), intent(in), target :: connection_table
      type(connection_entry_t), pointer :: entry
      integer, dimension(:), allocatable :: n_entries
      integer :: i, j, k, m, r
      allocate (pa (n_matrix_elements))
      allocate (n_entries (n_matrix_elements))
      n_entries = 0
      do m = 1, size (connection_table%index_result)
         r = index_map_get_entry (connection_table%index_result, m)
         n_entries(r) = n_entries(r) + 1
      end do
      call pairing_array_init &
           (pa, n_entries, has_i2=.true., has_factor=.false.)
      m = 1
      n_entries = 0
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         do j = 1, entry%n_index(1)
            do k = 1, entry%n_index(2)
               r = index_map_get_entry (connection_table%index_result, m)
               n_entries(r) = n_entries(r) + 1
               pa(r)%i1(n_entries(r)) = &
                    index_map_get_entry (entry%index_in(1), j)
               pa(r)%i2(n_entries(r)) = &
                    index_map_get_entry (entry%index_in(2), k)
               m = m + 1
            end do
         end do
      end do
    end subroutine make_pairing_array

    subroutine record_links (int, &
         int_in1, int_in2, connection_index, prt_map_in, prt_map_conn, &
         prt_is_connected, connections_are_resonant)
      type(interaction_t), intent(inout) :: int
      type(interaction_t), intent(in), target :: int_in1, int_in2
      integer, dimension(:,:), intent(in) :: connection_index
      type(index_map_t), dimension(2), intent(in) :: prt_map_in
      type(index_map_t), intent(in) :: prt_map_conn
      type(prt_mask_t), dimension(2), intent(in) :: prt_is_connected
      logical, intent(in), optional :: connections_are_resonant
      type(index_map_t), dimension(2) :: prt_map_all
      integer :: i, j, k, ival
      call index_map_init (prt_map_all(1), size (prt_is_connected(1)))
      k = 0
      j = 0
      do i = 1, size (prt_is_connected(1))
         if (prt_is_connected(1)%entry(i)) then
            j = j + 1
            ival = index_map_get_entry (prt_map_in(1), j)
            call index_map_set_entry (prt_map_all(1), i, ival)
         else
            k = k + 1
            ival = index_map_get_entry (prt_map_conn, k)
            call index_map_set_entry (prt_map_all(1), i, ival)
         end if
         call interaction_set_source_link (int, ival, int_in1, i)
      end do
      call interaction_transfer_relations (int_in1, int, prt_map_all(1)%entry)
      call index_map_init (prt_map_all(2), size (prt_is_connected(2)))
      j = 0
      do i = 1, size (prt_is_connected(2))
         if (prt_is_connected(2)%entry(i)) then
            j = j + 1
            ival = index_map_get_entry (prt_map_in(2), j)
            call index_map_set_entry (prt_map_all(2), i, ival)
            call interaction_set_source_link (int, ival, int_in2, i)
         else
            call index_map_set_entry (prt_map_all(2), i, 0)
         end if
      end do
      call interaction_transfer_relations (int_in2, int, prt_map_all(2)%entry)
      call interaction_relate_connections (int, &
           int_in2, connection_index(:,2), prt_map_all(2)%entry, &
           prt_map_conn%entry, connections_are_resonant)
    end subroutine record_links

  end subroutine evaluator_init_product_ii

  subroutine evaluator_init_product_ie &
       (eval, int_in1, eval_in2, qn_mask_conn, qn_mask_rest, &
        connections_are_resonant)
    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in1
    type(evaluator_t), intent(in), target :: eval_in2
    type(quantum_numbers_mask_t), intent(in) :: qn_mask_conn
    type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_rest
    logical, intent(in), optional :: connections_are_resonant
    call evaluator_init_product_ii &
         (eval, int_in1, eval_in2%int, qn_mask_conn, qn_mask_rest, &
          connections_are_resonant)
  end subroutine evaluator_init_product_ie

  subroutine evaluator_init_product_ei &
       (eval, eval_in1, int_in2, qn_mask_conn, qn_mask_rest, &
        connections_are_resonant)
    type(evaluator_t), intent(out), target :: eval
    type(evaluator_t), intent(in), target :: eval_in1
    type(interaction_t), intent(in), target :: int_in2
    type(quantum_numbers_mask_t), intent(in) :: qn_mask_conn
    type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_rest
    logical, intent(in), optional :: connections_are_resonant
    call evaluator_init_product_ii &
         (eval, eval_in1%int, int_in2, qn_mask_conn, qn_mask_rest, &
         connections_are_resonant)
  end subroutine evaluator_init_product_ei

  subroutine evaluator_init_product_ee &
       (eval, eval_in1, eval_in2, qn_mask_conn, qn_mask_rest, &
        connections_are_resonant)
    type(evaluator_t), intent(out), target :: eval
    type(evaluator_t), intent(in), target :: eval_in1, eval_in2
    type(quantum_numbers_mask_t), intent(in) :: qn_mask_conn
    type(quantum_numbers_mask_t), intent(in), optional :: qn_mask_rest
    logical, intent(in), optional :: connections_are_resonant
    call evaluator_init_product_ii &
         (eval, eval_in1%int, eval_in2%int, qn_mask_conn, qn_mask_rest, &
          connections_are_resonant)
  end subroutine evaluator_init_product_ee

  subroutine evaluator_init_square (eval, int_in, qn_mask, &
       col_flow_index, col_factor, col_index_hi, expand_color_flows, nc)
    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
    integer, dimension(:,:), intent(in), optional :: col_flow_index
    complex(default), dimension(:), intent(in), optional :: col_factor
    integer, dimension(:), intent(in), optional :: col_index_hi
    logical, intent(in), optional :: expand_color_flows
    integer, intent(in), optional :: nc
    if (all (quantum_numbers_mask_diagonal_helicity (qn_mask))) then
       call evaluator_init_square_diag (eval, int_in, qn_mask, &
            col_flow_index, col_factor, col_index_hi, expand_color_flows, nc)
    else
       call evaluator_init_square_nondiag (eval, int_in, qn_mask, &
            col_flow_index, col_factor, col_index_hi, expand_color_flows, nc)
    end if
  end subroutine evaluator_init_square

  subroutine evaluator_init_square_diag (eval, int_in, qn_mask, &
       col_flow_index, col_factor, col_index_hi, expand_color_flows, nc)

    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
    integer, dimension(:,:), intent(in), optional :: col_flow_index
    complex(default), dimension(:), intent(in), optional :: col_factor
    integer, dimension(:), intent(in), optional :: col_index_hi
    logical, intent(in), optional :: expand_color_flows
    integer, intent(in), optional :: nc

    integer :: n_in, n_vir, n_out, n_tot
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask_initial

    type :: connection_table_t
      integer :: n_tot = 0
      integer :: n_me_conn = 0
      type(state_matrix_t) :: state
      type(index_map_t) :: index_conn
      type(connection_entry_t), dimension(:), allocatable :: entry
      type(index_map_t) :: index_result
    end type connection_table_t
    type(connection_table_t) :: connection_table

    logical :: sum_colors
    type(color_table_t) :: color_table

    if (present (expand_color_flows)) then
       sum_colors = .not. expand_color_flows
    else
       sum_colors = .true.
    end if

    if (sum_colors) then
       eval%type = EVAL_SQUARE_WITH_COLOR_FACTORS
    else
       eval%type = EVAL_SQUARED_FLOWS
    end if
    eval%int_in1 => int_in

    ! print *, "Interaction square with color factors (diag)"  !!! Debugging
    ! print *, "Input interaction"                             !!! Debugging
    ! call interaction_write (int_in)                          !!! Debugging
    
    n_in  = interaction_get_n_in  (int_in)
    n_vir = interaction_get_n_vir (int_in)
    n_out = interaction_get_n_out (int_in)
    n_tot = interaction_get_n_tot (int_in)

    allocate (qn_mask_initial (n_tot))
    qn_mask_initial = interaction_get_mask (int_in)
    call quantum_numbers_mask_set_color &
         (qn_mask_initial, sum_colors, mask_cg=.false.)
    if (sum_colors) then
       call color_table_init &
            (color_table, interaction_get_state_matrix_ptr (int_in), n_tot)
       if (present (col_flow_index) .and. present (col_factor) &
           .and. present (col_index_hi)) then
          call color_table_set_color_factors &
               (color_table, col_flow_index, col_factor, col_index_hi)
       end if
       ! call color_table_write (color_table)     !!! Debugging
    end if

    call connection_table_init (connection_table, &
         interaction_get_state_matrix_ptr (int_in), &
         qn_mask_initial, qn_mask, n_tot)
    call connection_table_fill (connection_table, &
         interaction_get_state_matrix_ptr (int_in))
    call make_squared_interaction (eval%int, &
         n_in, n_vir, n_out, n_tot, &
         connection_table, sum_colors, qn_mask_initial .or. qn_mask)
    call make_pairing_array (eval%pairing_array, &
         interaction_get_n_matrix_elements (eval%int), &
         connection_table, sum_colors, color_table, n_in, n_tot, nc)
    call record_links (eval%int, int_in, n_tot)
    call connection_table_final (connection_table)
    ! print *, "Result evaluator:"     !!! Debugging
    ! call evaluator_write (eval)      !!! Debugging

  contains
    
    subroutine connection_table_init &
         (connection_table, state_in, qn_mask_in, qn_mask, n_tot)
      type(connection_table_t), intent(out) :: connection_table
      type(state_matrix_t), intent(in), target :: state_in
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
      integer, intent(in) :: n_tot
      type(quantum_numbers_t), dimension(n_tot) :: qn
      type(state_iterator_t) :: it
      integer :: i, n_me_in, me_index_in
      integer :: me_index_conn, n_me_conn
      integer, dimension(1) :: me_count
      connection_table%n_tot = n_tot
      n_me_in = state_matrix_get_n_matrix_elements (state_in)
      call index_map_init (connection_table%index_conn, n_me_in)
      connection_table%index_conn = 0
      call state_matrix_init (connection_table%state, n_counters=1)
      call state_iterator_init (it, state_in)
      do while (state_iterator_is_valid (it))
         qn = state_iterator_get_quantum_numbers (it)
         if (all (quantum_numbers_are_physical (qn, qn_mask))) then
            call quantum_numbers_undefine (qn, qn_mask_in)
            me_index_in = state_iterator_get_me_index (it)
            call state_matrix_add_state (connection_table%state, qn, &
                 counter_index = 1, me_index = me_index_conn)
            call index_map_set_entry (connection_table%index_conn, &
                 me_index_in, me_index_conn)
         end if
         call state_iterator_advance (it)
      end do
      n_me_conn = state_matrix_get_n_matrix_elements (connection_table%state)
      connection_table%n_me_conn = n_me_conn
      allocate (connection_table%entry (n_me_conn))
      call state_iterator_init (it, connection_table%state)
      do while (state_iterator_is_valid (it))
         i = state_iterator_get_me_index (it)
         me_count = state_iterator_get_me_count (it)
         call connection_entry_init (connection_table%entry(i), 1, 2, &
              state_iterator_get_quantum_numbers (it), me_count, [n_tot])
         call state_iterator_advance (it)
      end do
    end subroutine connection_table_init
         
    subroutine connection_table_final (connection_table)
      type(connection_table_t), intent(inout) :: connection_table
      call state_matrix_final (connection_table%state)
    end subroutine connection_table_final

    subroutine connection_table_write (connection_table, unit)
      type(connection_table_t), intent(in) :: connection_table
      integer, intent(in), optional :: unit
      integer :: i
      integer :: u
      u = given_output_unit (unit)
      write (u, *) "Connection table:"
      call state_matrix_write (connection_table%state, unit)
      if (index_map_exists (connection_table%index_conn)) then
         write (u, *) "  Index mapping input => connection table:"
         do i = 1, size (connection_table%index_conn)
            write (u, *)  i, &
                   index_map_get_entry (connection_table%index_conn, i)
         end do
      end if
      if (allocated (connection_table%entry)) then
         write (u, *) "  Connection table contents"
         do i = 1, size (connection_table%entry)
            call connection_entry_write (connection_table%entry(i), unit)
         end do
      end if
      if (index_map_exists (connection_table%index_result)) then
         write (u, *) "  Index mapping connection table => output"
         do i = 1, size (connection_table%index_result)
            write (u, *)  i, &
                  index_map_get_entry (connection_table%index_result, i)
         end do
      end if
    end subroutine connection_table_write

    subroutine connection_table_fill (connection_table, state)
      type(connection_table_t), intent(inout) :: connection_table
      type(state_matrix_t), intent(in), target :: state
      integer :: index_in, index_conn, n_result_entries
      type(state_iterator_t) :: it
      integer :: k
      call state_iterator_init (it, state)
      do while (state_iterator_is_valid (it))
         index_in = state_iterator_get_me_index (it)
         index_conn = &
              index_map_get_entry (connection_table%index_conn, index_in)
         if (index_conn /= 0) then
            call connection_entry_add_state &
                 (connection_table%entry(index_conn), &
                  index_in, &
                  state_iterator_get_quantum_numbers (it))
         end if
         call state_iterator_advance (it)
      end do
      n_result_entries = 0
      do k = 1, size (connection_table%entry)
         n_result_entries = &
              n_result_entries + connection_table%entry(k)%n_index(1) ** 2
      end do
      call index_map_init (connection_table%index_result, n_result_entries)
      connection_table%index_result = 0
    end subroutine connection_table_fill

    subroutine connection_entry_add_state (entry, index_in, qn_in)
      type(connection_entry_t), intent(inout) :: entry
      integer, intent(in) :: index_in
      type(quantum_numbers_t), dimension(:), intent(in) :: qn_in
      integer :: c
      entry%count = entry%count + 1
      c = entry%count(1)
      call index_map_set_entry (entry%index_in(1), c, index_in)
      entry%qn_in_list(1)%qn(:,c) = qn_in
    end subroutine connection_entry_add_state

    subroutine make_squared_interaction (int, &
         n_in, n_vir, n_out, n_tot, &
         connection_table, sum_colors, qn_mask)
      type(interaction_t), intent(out), target :: int
      integer, intent(in) :: n_in, n_vir, n_out, n_tot
      type(connection_table_t), intent(inout), target :: connection_table
      logical, intent(in) :: sum_colors
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
      type(connection_entry_t), pointer :: entry
      integer :: result_index, n_contrib
      integer :: i, m
      type(quantum_numbers_t), dimension(n_tot) :: qn
      call interaction_init (eval%int, n_in, n_vir, n_out, mask=qn_mask)
      m = 0
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         qn = quantum_numbers_undefined (entry%qn_conn, qn_mask)
         if (.not. sum_colors)   call quantum_numbers_invert_color (qn(1:n_in))
         call interaction_add_state (int, qn, me_index = result_index)
         n_contrib = entry%n_index(1) ** 2
         connection_table%index_result%entry(m+1:m+n_contrib) = result_index
         m = m + n_contrib
      end do
      call interaction_freeze (int)
    end subroutine make_squared_interaction

    subroutine make_pairing_array (pa, &
         n_matrix_elements, connection_table, sum_colors, color_table, &
         n_in, n_tot, nc)
      type(pairing_array_t), dimension(:), intent(out), allocatable :: pa
      integer, intent(in) :: n_matrix_elements
      type(connection_table_t), intent(in), target :: connection_table
      logical, intent(in) :: sum_colors
      type(color_table_t), intent(inout) :: color_table
      type(connection_entry_t), pointer :: entry
      integer, intent(in) :: n_in, n_tot
      integer, intent(in), optional :: nc
      integer, dimension(:), allocatable :: n_entries
      integer :: i, k, l, ks, ls, m, r
      integer :: color_multiplicity_in
      allocate (pa (n_matrix_elements))
      allocate (n_entries (n_matrix_elements))
      n_entries = 0
      do m = 1, size (connection_table%index_result)
         r = index_map_get_entry (connection_table%index_result, m)
         n_entries(r) = n_entries(r) + 1
      end do
      call pairing_array_init &
           (pa, n_entries, has_i2 = sum_colors, has_factor = sum_colors)
      m = 1
      n_entries = 0
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         do k = 1, entry%n_index(1)
            if (sum_colors) then
               color_multiplicity_in = &
                     product (abs (quantum_numbers_get_color_type &
                                     (entry%qn_in_list(1)%qn(:n_in, k))))
               do l = 1, entry%n_index(1)
                  r = index_map_get_entry (connection_table%index_result, m)
                  n_entries(r) = n_entries(r) + 1
                  ks = index_map_get_entry (entry%index_in(1), k)
                  ls = index_map_get_entry (entry%index_in(1), l)
                  pa(r)%i1(n_entries(r)) = ks
                  pa(r)%i2(n_entries(r)) = ls
                  pa(r)%factor(n_entries(r)) = &
                       color_table_get_color_factor (color_table, ks, ls, nc) &
                       / color_multiplicity_in
                  m = m + 1
               end do
            else
               r = index_map_get_entry (connection_table%index_result, m)
               n_entries(r) = n_entries(r) + 1
               ks = index_map_get_entry (entry%index_in(1), k)
               pa(r)%i1(n_entries(r)) = ks
               m = m + 1  
            end if               
         end do
      end do
    end subroutine make_pairing_array

    subroutine record_links (int, int_in, n_tot)
      type(interaction_t), intent(inout) :: int
      type(interaction_t), intent(in), target :: int_in
      integer, intent(in) :: n_tot
      integer, dimension(n_tot) :: map
      integer :: i
      do i = 1, n_tot
         call interaction_set_source_link (int, i, int_in, i)
      end do
      map = [ (i, i = 1, n_tot) ]
      call interaction_transfer_relations (int_in, int, map)
    end subroutine record_links

  end subroutine evaluator_init_square_diag

  subroutine evaluator_init_square_nondiag (eval, int_in, qn_mask, &
      col_flow_index, col_factor, col_index_hi, expand_color_flows, nc)

    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
    integer, dimension(:,:), intent(in), optional :: col_flow_index
    complex(default), dimension(:), intent(in), optional :: col_factor
    integer, dimension(:), intent(in), optional :: col_index_hi
    logical, intent(in), optional :: expand_color_flows
    integer, intent(in), optional :: nc

    integer :: n_in, n_vir, n_out, n_tot
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask_initial

    type :: connection_table_t
      integer :: n_tot = 0
      integer :: n_me_conn = 0
      type(state_matrix_t) :: state
      type(index_map2_t) :: index_conn
      type(connection_entry_t), dimension(:), allocatable :: entry
      type(index_map_t) :: index_result
    end type connection_table_t
    type(connection_table_t) :: connection_table
       
    logical :: sum_colors
    type(color_table_t) :: color_table

    if (present (expand_color_flows)) then
       sum_colors = .not. expand_color_flows
    else
       sum_colors = .true.
    end if

    if (sum_colors) then
       eval%type = EVAL_SQUARE_WITH_COLOR_FACTORS
    else
       eval%type = EVAL_SQUARED_FLOWS
    end if
    eval%int_in1 => int_in

    ! print *, "Interaction square with color factors (nondiag)"  !!! Debugging
    ! print *, "Input interaction"                                !!! Debugging
    ! call interaction_write (int_in)                             !!! Debugging
    n_in  = interaction_get_n_in  (int_in)
    n_vir = interaction_get_n_vir (int_in)
    n_out = interaction_get_n_out (int_in)
    n_tot = interaction_get_n_tot (int_in)

    allocate (qn_mask_initial (n_tot))
    qn_mask_initial = interaction_get_mask (int_in)
    call quantum_numbers_mask_set_color &
         (qn_mask_initial, sum_colors, mask_cg=.false.)
    if (sum_colors) then
       call color_table_init &
            (color_table, interaction_get_state_matrix_ptr (int_in), n_tot)
       if (present (col_flow_index) .and. present (col_factor) &
           .and. present (col_index_hi)) then
          call color_table_set_color_factors &
               (color_table, col_flow_index, col_factor, col_index_hi)
       end if
       ! call color_table_write (color_table)    !!! Debugging
    end if

    call connection_table_init (connection_table, &
         interaction_get_state_matrix_ptr (int_in), &
         qn_mask_initial, qn_mask, n_tot)
    call connection_table_fill (connection_table, &
         interaction_get_state_matrix_ptr (int_in))
    call make_squared_interaction (eval%int, &
         n_in, n_vir, n_out, n_tot, &
         connection_table, sum_colors, qn_mask_initial .or. qn_mask)
    ! call connection_table_write (connection_table)     !!! Debugging
    call make_pairing_array (eval%pairing_array, &
         interaction_get_n_matrix_elements (eval%int), &
         connection_table, sum_colors, color_table, n_in, n_tot, nc)
    call record_links (eval%int, int_in, n_tot)
    call connection_table_final (connection_table)

    ! print *, "Result evaluator:"     !!! Debugging
    ! call evaluator_write (eval)      !!! Debugging

  contains
    
    subroutine connection_table_init &
         (connection_table, state_in, qn_mask_in, qn_mask, n_tot)
      type(connection_table_t), intent(out) :: connection_table
      type(state_matrix_t), intent(in), target :: state_in
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
      integer, intent(in) :: n_tot
      type(quantum_numbers_t), dimension(n_tot) :: qn1, qn2, qn
      type(state_iterator_t) :: it1, it2, it
      integer :: i, n_me_in, me_index_in1, me_index_in2
      integer :: me_index_conn, n_me_conn
      integer, dimension(1) :: me_count
      connection_table%n_tot = n_tot
      n_me_in = state_matrix_get_n_matrix_elements (state_in)
      call index_map2_init (connection_table%index_conn, n_me_in)
      connection_table%index_conn = 0
      call state_matrix_init (connection_table%state, n_counters=1)
      call state_iterator_init (it1, state_in)
      do while (state_iterator_is_valid (it1))
         qn1 = state_iterator_get_quantum_numbers (it1)
         me_index_in1 = state_iterator_get_me_index (it1)
         call state_iterator_init (it2, state_in)
         do while (state_iterator_is_valid (it2))
            qn2 = state_iterator_get_quantum_numbers (it2)
            if (all (quantum_numbers_are_compatible (qn1, qn2, qn_mask))) then
               qn = qn1 .merge. qn2
               call quantum_numbers_undefine (qn, qn_mask_in)
               me_index_in2 = state_iterator_get_me_index (it2)
               call state_matrix_add_state (connection_table%state, qn, &
                    counter_index = 1, me_index = me_index_conn)
               call index_map2_set_entry (connection_table%index_conn, &
                    me_index_in1, me_index_in2, me_index_conn)
            end if
            call state_iterator_advance (it2)
         end do
         call state_iterator_advance (it1)
      end do
      n_me_conn = state_matrix_get_n_matrix_elements (connection_table%state)
      connection_table%n_me_conn = n_me_conn
      allocate (connection_table%entry (n_me_conn))
      call state_iterator_init (it, connection_table%state)
      do while (state_iterator_is_valid (it))
         i = state_iterator_get_me_index (it)
         me_count = state_iterator_get_me_count (it)
         call connection_entry_init (connection_table%entry(i), 1, 2, &
              state_iterator_get_quantum_numbers (it), me_count, [n_tot])
         call state_iterator_advance (it)
      end do
    end subroutine connection_table_init
         
    subroutine connection_table_final (connection_table)
      type(connection_table_t), intent(inout) :: connection_table
      call state_matrix_final (connection_table%state)
    end subroutine connection_table_final

    subroutine connection_table_write (connection_table, unit)
      type(connection_table_t), intent(in) :: connection_table
      integer, intent(in), optional :: unit
      integer :: i, j
      integer :: u
      u = given_output_unit (unit)
      write (u, *) "Connection table:"
      call state_matrix_write (connection_table%state, unit)
      if (index_map2_exists (connection_table%index_conn)) then
         write (u, *) "  Index mapping input => connection table:"
         do i = 1, size (connection_table%index_conn)
            do j = 1, size (connection_table%index_conn)
               write (u, *)  i, j, &
                    index_map2_get_entry (connection_table%index_conn, i, j)
            end do
         end do
      end if
      if (allocated (connection_table%entry)) then
         write (u, *) "  Connection table contents"
         do i = 1, size (connection_table%entry)
            call connection_entry_write (connection_table%entry(i), unit)
         end do
      end if
      if (index_map_exists (connection_table%index_result)) then
         write (u, *) "  Index mapping connection table => output"
         do i = 1, size (connection_table%index_result)
            write (u, *)  i, &
                 index_map_get_entry (connection_table%index_result, i)
         end do
      end if
    end subroutine connection_table_write

    subroutine connection_table_fill (connection_table, state)
      type(connection_table_t), intent(inout), target :: connection_table
      type(state_matrix_t), intent(in), target :: state
      integer :: index1_in, index2_in, index_conn, n_result_entries
      type(state_iterator_t) :: it1, it2
      integer :: k
      call state_iterator_init (it1, state)
      do while (state_iterator_is_valid (it1))
         index1_in = state_iterator_get_me_index (it1)
         call state_iterator_init (it2, state)
         do while (state_iterator_is_valid (it2))
            index2_in = state_iterator_get_me_index (it2)
            index_conn = index_map2_get_entry &
                            (connection_table%index_conn, index1_in, index2_in)
            if (index_conn /= 0) then
               call connection_entry_add_state &
                    (connection_table%entry(index_conn), &
                     index1_in, index2_in, &
                     state_iterator_get_quantum_numbers (it1) &
                     .merge. &
                     state_iterator_get_quantum_numbers (it2))
            end if
            call state_iterator_advance (it2)
         end do
         call state_iterator_advance (it1)
      end do
      n_result_entries = 0
      do k = 1, size (connection_table%entry)
         n_result_entries = &
              n_result_entries + connection_table%entry(k)%n_index(1)
      end do
      call index_map_init (connection_table%index_result, n_result_entries)
      connection_table%index_result = 0
    end subroutine connection_table_fill

    subroutine connection_entry_add_state (entry, index1_in, index2_in, qn_in)
      type(connection_entry_t), intent(inout) :: entry
      integer, intent(in) :: index1_in, index2_in
      type(quantum_numbers_t), dimension(:), intent(in) :: qn_in
      integer :: c
      entry%count = entry%count + 1
      c = entry%count(1)
      call index_map_set_entry (entry%index_in(1), c, index1_in)
      call index_map_set_entry (entry%index_in(2), c, index2_in)
      entry%qn_in_list(1)%qn(:,c) = qn_in
    end subroutine connection_entry_add_state

    subroutine make_squared_interaction (int, &
         n_in, n_vir, n_out, n_tot, &
         connection_table, sum_colors, qn_mask)
      type(interaction_t), intent(out), target :: int
      integer, intent(in) :: n_in, n_vir, n_out, n_tot
      type(connection_table_t), intent(inout), target :: connection_table
      logical, intent(in) :: sum_colors
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
      type(connection_entry_t), pointer :: entry
      integer :: result_index
      integer :: i, k, m
      type(quantum_numbers_t), dimension(n_tot) :: qn
      call interaction_init (eval%int, n_in, n_vir, n_out, mask=qn_mask)
      m = 0
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         do k = 1, size (entry%qn_in_list(1)%qn, 2)
            qn = quantum_numbers_undefined &
                    (entry%qn_in_list(1)%qn(:,k), qn_mask)
            if (.not. sum_colors) &
                    call quantum_numbers_invert_color (qn(1:n_in))
            call interaction_add_state (int, qn, me_index = result_index)
            call index_map_set_entry (connection_table%index_result, m + 1, &
                 result_index)
            m = m + 1
         end do
      end do
      call interaction_freeze (int)
    end subroutine make_squared_interaction

    subroutine make_pairing_array (pa, &
         n_matrix_elements, connection_table, sum_colors, color_table, &
         n_in, n_tot, nc)
      type(pairing_array_t), dimension(:), intent(out), allocatable :: pa
      integer, intent(in) :: n_matrix_elements
      type(connection_table_t), intent(in), target :: connection_table
      logical, intent(in) :: sum_colors
      type(color_table_t), intent(inout) :: color_table
      type(connection_entry_t), pointer :: entry
      integer, intent(in) :: n_in, n_tot
      integer, intent(in), optional :: nc
      integer, dimension(:), allocatable :: n_entries
      integer :: i, k, k1s, k2s, m, r
      integer :: color_multiplicity_in
      allocate (pa (n_matrix_elements))
      allocate (n_entries (n_matrix_elements))
      n_entries = 0
      do m = 1, size (connection_table%index_result)
         r = index_map_get_entry (connection_table%index_result, m)
         n_entries(r) = n_entries(r) + 1
      end do
      call pairing_array_init &
           (pa, n_entries, has_i2 = sum_colors, has_factor = sum_colors)
      m = 1
      n_entries = 0
      do i = 1, connection_table%n_me_conn
         entry => connection_table%entry(i)
         do k = 1, entry%n_index(1)
            r = index_map_get_entry (connection_table%index_result, m)
            n_entries(r) = n_entries(r) + 1
            if (sum_colors) then
               k1s = index_map_get_entry (entry%index_in(1), k)
               k2s = index_map_get_entry (entry%index_in(2), k)
               pa(r)%i1(n_entries(r)) = k1s
               pa(r)%i2(n_entries(r)) = k2s
               color_multiplicity_in = &
                     product (abs (quantum_numbers_get_color_type &
                                     (entry%qn_in_list(1)%qn(:n_in, k))))
               pa(r)%factor(n_entries(r)) = &
                    color_table_get_color_factor (color_table, k1s, k2s, nc) &
                    / color_multiplicity_in
            else
               k1s = index_map_get_entry (entry%index_in(1), k)
               pa(r)%i1(n_entries(r)) = k1s
            end if               
            m = m + 1
         end do
      end do
    end subroutine make_pairing_array

    subroutine record_links (int, int_in, n_tot)
      type(interaction_t), intent(inout) :: int
      type(interaction_t), intent(in), target :: int_in
      integer, intent(in) :: n_tot
      integer, dimension(n_tot) :: map
      integer :: i
      do i = 1, n_tot
         call interaction_set_source_link (int, i, int_in, i)
      end do
      map = [ (i, i = 1, n_tot) ]
      call interaction_transfer_relations (int_in, int, map)
    end subroutine record_links

  end subroutine evaluator_init_square_nondiag

  subroutine evaluator_init_color_contractions (eval, int_in)
    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int_in
    integer :: n_in, n_vir, n_out, n_tot
    type(state_matrix_t) :: state_with_contractions
    integer, dimension(:), allocatable :: me_index
    integer, dimension(:), allocatable :: result_index
    eval%type = EVAL_COLOR_CONTRACTION
    eval%int_in1 => int_in
    ! print *, "Interaction with additional color contractions"  !!! Debugging
    ! print *, "Input interaction"                               !!! Debugging
    ! call interaction_write (int_in)                            !!! Debugging
    n_in  = interaction_get_n_in  (int_in)
    n_vir = interaction_get_n_vir (int_in)
    n_out = interaction_get_n_out (int_in)
    n_tot = interaction_get_n_tot (int_in)
    state_with_contractions = interaction_get_state_matrix_ptr (int_in)
    call state_matrix_add_color_contractions (state_with_contractions)
    call make_contracted_interaction (eval%int, &
         me_index, result_index, &
         n_in, n_vir, n_out, n_tot, &
         state_with_contractions, interaction_get_mask (int_in))
    call make_pairing_array (eval%pairing_array, me_index, result_index)
    call record_links (eval%int, int_in, n_tot)
    call state_matrix_final (state_with_contractions)
    ! print *, "Result evaluator:"     !!! Debugging
    ! call evaluator_write (eval)      !!! Debugging

  contains

    subroutine make_contracted_interaction (int, &
         me_index, result_index, &
         n_in, n_vir, n_out, n_tot, state, qn_mask)
      type(interaction_t), intent(out), target :: int
      integer, dimension(:), intent(out), allocatable :: me_index
      integer, dimension(:), intent(out), allocatable :: result_index
      integer, intent(in) :: n_in, n_vir, n_out, n_tot
      type(state_matrix_t), intent(in) :: state
      type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
      type(state_iterator_t) :: it
      integer :: n_me, i
      type(quantum_numbers_t), dimension(n_tot) :: qn
      call interaction_init (int, n_in, n_vir, n_out, mask=qn_mask)
      n_me = state_matrix_get_n_leaves (state)
      allocate (me_index (n_me))
      allocate (result_index (n_me))
      call state_iterator_init (it, state)
      i = 0
      do while (state_iterator_is_valid (it))
         i = i + 1
         me_index(i) = state_iterator_get_me_index (it)
         qn = state_iterator_get_quantum_numbers (it)
         call interaction_add_state (int, qn, me_index = result_index(i))
         call state_iterator_advance (it)
      end do
      call interaction_freeze (int)
    end subroutine make_contracted_interaction

    subroutine make_pairing_array (pa, me_index, result_index)
      type(pairing_array_t), dimension(:), intent(out), allocatable :: pa
      integer, dimension(:), intent(in) :: me_index, result_index
      integer, dimension(:), allocatable :: n_entries
      integer :: n_matrix_elements, r, i
      n_matrix_elements = size (me_index)
      allocate (pa (n_matrix_elements))
      allocate (n_entries (n_matrix_elements))
      n_entries = 1
      call pairing_array_init &
           (pa, n_entries, has_i2=.false., has_factor=.false.)
      do i = 1, n_matrix_elements
         r = result_index(i)
         pa(r)%i1(1) = me_index(i)
      end do
    end subroutine make_pairing_array

    subroutine record_links (int, int_in, n_tot)
      type(interaction_t), intent(inout) :: int
      type(interaction_t), intent(in), target :: int_in
      integer, intent(in) :: n_tot
      integer, dimension(n_tot) :: map
      integer :: i
      do i = 1, n_tot
         call interaction_set_source_link (int, i, int_in, i)
      end do
      map = [ (i, i = 1, n_tot) ]
      call interaction_transfer_relations (int_in, int, map)
    end subroutine record_links

  end subroutine evaluator_init_color_contractions

  function parity (mask)
    logical :: parity
    logical, dimension(:) :: mask
    integer :: i
    parity = .false.
    do i = 1, size (mask)
       if (mask(i))  parity = .not. parity
    end do
  end function parity

  function evaluator_get_int_ptr (eval) result (int)
    type(interaction_t), pointer :: int
    type(evaluator_t), intent(in), target :: eval
    int => eval%int
  end function evaluator_get_int_ptr

  function evaluator_is_empty (eval) result (flag)
    logical :: flag
    type(evaluator_t), intent(in) :: eval
    flag = interaction_is_empty (eval%int)
  end function evaluator_is_empty

  function evaluator_get_tag (eval) result (tag)
    integer :: tag
    type(evaluator_t), intent(in) :: eval
    tag = interaction_get_tag (eval%int)
  end function evaluator_get_tag

  function evaluator_get_norm (eval) result (norm)
    real(default) :: norm
    type(evaluator_t), intent(in) :: eval
    norm = interaction_get_norm (eval%int)
  end function evaluator_get_norm

  function evaluator_get_n_tot (eval) result (n_tot)
    integer :: n_tot
    type(evaluator_t), intent(in) :: eval
    n_tot = interaction_get_n_tot (eval%int)
  end function evaluator_get_n_tot

  function evaluator_get_n_in (eval) result (n_in)
    integer :: n_in
    type(evaluator_t), intent(in) :: eval
    n_in = interaction_get_n_in (eval%int)
  end function evaluator_get_n_in

  function evaluator_get_n_vir (eval) result (n_vir)
    integer :: n_vir
    type(evaluator_t), intent(in) :: eval
    n_vir = interaction_get_n_vir (eval%int)
  end function evaluator_get_n_vir

  function evaluator_get_n_out (eval) result (n_out)
    integer :: n_out
    type(evaluator_t), intent(in) :: eval
    n_out = interaction_get_n_out (eval%int)
  end function evaluator_get_n_out

  function evaluator_get_matrix_element (eval, i) result (value)
    complex(default) :: value
    type(evaluator_t), intent(in) :: eval
    integer :: i
    value = interaction_get_matrix_element (eval%int, i)
  end function evaluator_get_matrix_element

  function evaluator_sum (eval) result (value)
    complex(default) :: value
    type(evaluator_t), intent(in) :: eval
    value = interaction_sum (eval%int)
  end function evaluator_sum

  subroutine evaluator_normalize_by_trace (eval)
    type(evaluator_t), intent(inout) :: eval
    call interaction_normalize_by_trace (eval%int)
  end subroutine evaluator_normalize_by_trace

  subroutine evaluator_normalize_by_max (eval)
    type(evaluator_t), intent(inout) :: eval
    call interaction_normalize_by_max (eval%int)
  end subroutine evaluator_normalize_by_max

  subroutine evaluator_add_color_contractions (eval)
    type(evaluator_t), intent(inout) :: eval
    call interaction_add_color_contractions (eval%int)
  end subroutine evaluator_add_color_contractions

  function evaluator_get_mask (eval) result (mask)
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    type(evaluator_t), intent(in), target :: eval
    allocate (mask (interaction_get_n_tot (eval%int)))
    mask = interaction_get_mask (eval%int)
  end function evaluator_get_mask

  subroutine interaction_set_source_link_eval (int, i, eval1, i1)
    type(interaction_t), intent(inout) :: int
    type(evaluator_t), intent(in), target :: eval1
    integer, intent(in) :: i, i1
    call interaction_set_source_link (int, i, eval1%int, i1)
  end subroutine interaction_set_source_link_eval

  subroutine evaluator_set_source_link_int (eval, i, int1, i1)
    type(evaluator_t), intent(inout) :: eval
    type(interaction_t), intent(in), target :: int1
    integer, intent(in) :: i, i1
    call interaction_set_source_link (eval%int, i, int1, i1)
  end subroutine evaluator_set_source_link_int

  subroutine evaluator_set_source_link_eval (eval, i, eval1, i1)
    type(evaluator_t), intent(inout) :: eval
    type(evaluator_t), intent(in), target :: eval1
    integer, intent(in) :: i, i1
    call interaction_set_source_link (eval%int, i, eval1%int, i1)
  end subroutine evaluator_set_source_link_eval

  subroutine evaluator_receive_momenta (eval)
    type(evaluator_t), intent(inout) :: eval
    call interaction_receive_momenta (eval%int)
  end subroutine evaluator_receive_momenta

  subroutine evaluator_send_momenta (eval)
    type(evaluator_t), intent(in) :: eval
    call interaction_send_momenta (eval%int)
  end subroutine evaluator_send_momenta

  subroutine evaluator_reassign_links_eval (eval, eval_src, eval_target)
    type(evaluator_t), intent(inout) :: eval
    type(evaluator_t), intent(in) :: eval_src
    type(evaluator_t), intent(in), target :: eval_target
    if (associated (eval%int_in1)) then
       if (interaction_get_tag (eval%int_in1) &
            == interaction_get_tag (eval_src%int)) then
          eval%int_in1 => eval_target%int
       end if
    end if
    if (associated (eval%int_in2)) then
       if (interaction_get_tag (eval%int_in2) &
            == interaction_get_tag (eval_src%int)) then
          eval%int_in2 => eval_target%int
       end if
    end if
    call interaction_reassign_links (eval%int, eval_src%int, eval_target%int)
  end subroutine evaluator_reassign_links_eval

  subroutine evaluator_reassign_links_int (eval, int_src, int_target)
    type(evaluator_t), intent(inout) :: eval
    type(interaction_t), intent(in) :: int_src
    type(interaction_t), intent(in), target :: int_target
    if (associated (eval%int_in1)) then
       if (interaction_get_tag (eval%int_in1) &
            == interaction_get_tag (int_src)) then
          eval%int_in1 => int_target
       end if
    end if
    if (associated (eval%int_in2)) then
       if (interaction_get_tag (eval%int_in2) &
            == interaction_get_tag (int_src)) then
          eval%int_in2 => int_target
       end if
    end if
    call interaction_reassign_links (eval%int, int_src, int_target)
  end subroutine evaluator_reassign_links_int

  subroutine evaluator_get_unstable_particle (eval, flv, p, i)
    type(evaluator_t), intent(in) :: eval
    type(flavor_t), intent(out) :: flv
    type(vector4_t), intent(out) :: p
    integer, intent(out) :: i
    call interaction_get_unstable_particle (eval%int, flv, p, i)
  end subroutine evaluator_get_unstable_particle

  elemental subroutine evaluator_final (eval)
    type(evaluator_t), intent(inout) :: eval
    call interaction_final (eval%int)
  end subroutine evaluator_final

  subroutine evaluator_init_identity_i (eval, int)
    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), intent(in), target :: int
    integer :: n_in, n_out, n_vir, n_tot
    integer :: i
    integer, dimension(:), allocatable :: map
    type(state_matrix_t), pointer :: state
    type(state_iterator_t) :: it

    eval%type = EVAL_IDENTITY
    eval%int_in1 => int
    nullify (eval%int_in2)
    n_in = interaction_get_n_in (int)
    n_out = interaction_get_n_out (int)
    n_vir = interaction_get_n_vir (int)
    n_tot = interaction_get_n_tot (int)
    call interaction_init (eval%int, n_in, n_vir, n_out, &
       mask=interaction_get_mask (int), &
       resonant=interaction_get_resonance_flags (int))
    do i = 1, n_tot
       call interaction_set_source_link (eval%int, i, int, i)
    end do
    allocate (map(n_tot))
    map = [(i, i = 1, n_tot)]
    call interaction_transfer_relations (int, eval%int, map)
    state => interaction_get_state_matrix_ptr (int)
    call state_iterator_init (it, state)
    do while (state_iterator_is_valid (it))
       call interaction_add_state (eval%int, &
          state_iterator_get_quantum_numbers (it), &
          state_iterator_get_me_index (it))
       call state_iterator_advance (it)
    end do
    call interaction_freeze (eval%int)

  end subroutine evaluator_init_identity_i

  subroutine evaluator_init_identity_e (eval, eval1)
    type(evaluator_t), intent(out), target :: eval
    type(evaluator_t), intent(in), target :: eval1
    call evaluator_init_identity_i (eval, eval1%int)
  end subroutine evaluator_init_identity_e

  subroutine evaluator_init_qn_sum_i (eval, int, qn_mask, drop)
    type(evaluator_t), intent(out), target :: eval
    type(interaction_t), target, intent(in) :: int
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
    logical, intent(in), optional, dimension(:) :: drop
    type(state_iterator_t) :: it_old, it_new
    integer, dimension(:,:), allocatable :: pairing_proto
    integer, dimension(:), allocatable :: pairing_sizes
    integer, dimension(:), allocatable :: map
    integer :: n_in, n_out, n_vir, n_tot, n_me_old, n_me_new
    integer :: i, j
    type(state_matrix_t), pointer :: state_new, state_old
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    logical :: matched
    logical, dimension(size (qn_mask)) :: dropped
    integer :: ndropped
    integer, dimension(:), allocatable :: inotdropped
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    logical, dimension(:), allocatable :: resonant
 
    eval%type = EVAL_QN_SUM
    eval%int_in1 => int
    nullify (eval%int_in2)
    if (present (drop)) then
       dropped = drop
    else
       dropped = .false.
    end if
    ndropped = count (dropped)

    n_in = interaction_get_n_in (int)
    n_out = interaction_get_n_out (int) - ndropped
    n_vir = interaction_get_n_vir (int)
    n_tot = interaction_get_n_tot (int) - ndropped

    allocate (inotdropped (n_tot))
    i = 1
    do j = 1, n_tot + ndropped
       if (dropped (j)) cycle
       inotdropped(i) = j
       i = i + 1
    end do

    allocate (mask(n_tot + ndropped))
    mask = interaction_get_mask (int)
    allocate (resonant(n_tot + ndropped))
    resonant = interaction_get_resonance_flags (int)
    call interaction_init (eval%int, n_in, n_vir, n_out, &
       mask = mask(inotdropped) .or. qn_mask(inotdropped), &
       resonant = resonant(inotdropped))
    i = 1
    do j = 1, n_tot + ndropped
       if (dropped(j)) cycle
       call interaction_set_source_link (eval%int, i, int, j)
       i = i + 1
    end do    
    allocate (map(n_tot + ndropped))
    i = 1
    do j = 1, n_tot + ndropped
       if (dropped (j)) then
          map(j) = 0
       else
          map(j) = i
          i = i + 1
       end if
    end do
    call interaction_transfer_relations (int, eval%int, map)

    n_me_old = interaction_get_n_matrix_elements (int)
    allocate (pairing_proto(n_me_old, n_me_old))
    allocate (pairing_sizes(n_me_old))
    pairing_sizes = 0
    state_old => interaction_get_state_matrix_ptr (int)
    state_new => interaction_get_state_matrix_ptr (eval%int)
    call state_iterator_init (it_old, state_old)
    allocate (qn(n_tot + ndropped))
    do while (state_iterator_is_valid (it_old))
       qn = state_iterator_get_quantum_numbers (it_old)
       if (.not. all (quantum_numbers_are_diagonal (qn))) then
          call state_iterator_advance (it_old)
          cycle
       end if
       matched = .false.
       call state_iterator_init (it_new, state_new)
       if (interaction_get_n_matrix_elements (eval%int) > 0) then
          do while (state_iterator_is_valid (it_new))
             if (all (qn(inotdropped) .match. &
                state_iterator_get_quantum_numbers (it_new))) &
             then
                matched = .true.
                i = state_iterator_get_me_index (it_new)
                exit
             end if
             call state_iterator_advance (it_new)
          end do
       end if
       if (.not. matched) then
          call interaction_add_state (eval%int, qn(inotdropped))
          i = interaction_get_n_matrix_elements (eval%int)
       end if
       pairing_sizes(i) = pairing_sizes(i) + 1
       pairing_proto(pairing_sizes(i), i) = &
          state_iterator_get_me_index (it_old)
       ! print *, &                         !!! Debugging
       !     i, pairing_sizes(i), state_iterator_get_me_index (it_old)
       call state_iterator_advance (it_old)
    end do
    call interaction_freeze (eval%int)

    n_me_new = interaction_get_n_matrix_elements (eval%int)
    allocate (eval%pairing_array(n_me_new))
    do i = 1, n_me_new
       ! print *, i, pairing_sizes(i)     !!! Debugging
       call pairing_array_init (eval%pairing_array(i), &
          pairing_sizes(i), .false., .false.)
       eval%pairing_array(i)%i1 = pairing_proto(:pairing_sizes(i), i) 
    end do

  end subroutine evaluator_init_qn_sum_i

  subroutine evaluator_init_qn_sum_e (eval, eval1, qn_mask, drop)
    type(evaluator_t), intent(out) :: eval
    type(evaluator_t), intent(in), target :: eval1
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask
    logical, dimension(:), optional, intent(in) :: drop
    call evaluator_init_qn_sum_i (eval, eval1%int, qn_mask, drop)
  end subroutine

  subroutine evaluator_evaluate (eval)
    class(evaluator_t), intent(inout), target :: eval
    integer :: i
    select case (eval%type)
    case (EVAL_PRODUCT)
       do i = 1, size(eval%pairing_array)
          call interaction_evaluate_product (eval%int, i, &
               eval%int_in1, eval%int_in2, &
               eval%pairing_array(i)%i1, eval%pairing_array(i)%i2)
       end do
    case (EVAL_SQUARE_WITH_COLOR_FACTORS)
       do i = 1, size(eval%pairing_array)
          call interaction_evaluate_product_cf (eval%int, i, &
               eval%int_in1, eval%int_in1, &
               eval%pairing_array(i)%i1, eval%pairing_array(i)%i2, &
               eval%pairing_array(i)%factor)
       end do
    case (EVAL_SQUARED_FLOWS)
       do i = 1, size(eval%pairing_array)
          call interaction_evaluate_square_c (eval%int, i, &
               eval%int_in1, &
               eval%pairing_array(i)%i1)
       end do
    case (EVAL_COLOR_CONTRACTION)
       do i = 1, size(eval%pairing_array)
          call interaction_evaluate_sum (eval%int, i, &
               eval%int_in1, &
               eval%pairing_array(i)%i1)
       end do
    case (EVAL_IDENTITY)
       call interaction_set_matrix_element (eval%int, eval%int_in1)
    case (EVAL_QN_SUM)
       do i = 1, size (eval%pairing_array)
          call interaction_evaluate_me_sum (eval%int, i, &
             eval%int_in1, eval%pairing_array(i)%i1)
          call interaction_set_norm &
               (eval%int, interaction_get_norm (eval%int_in1))
       end do
    end select
  end subroutine evaluator_evaluate

  subroutine evaluator_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (evaluator_1, "evaluator_1", &
         "check evaluators (1)", &
         u, results)
    call test (evaluator_2, "evaluator_2", &
         "check evaluators (2)", &
         u, results)
    call test (evaluator_3, "evaluator_3", &
         "check evaluators (3)", &
         u, results)
  end subroutine evaluator_test


  subroutine evaluator_1 (u) 
    integer, intent(in) :: u
    type(model_data_t), target :: model       
    type(interaction_t), target :: int_qqtt, int_tbw, int1, int2
    type(flavor_t), dimension(:), allocatable :: flv
    type(color_t), dimension(:), allocatable :: col
    type(helicity_t), dimension(:), allocatable :: hel
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: f, c, h1, h2, h3
    type(vector4_t), dimension(4) :: p
    type(vector4_t), dimension(2) :: q
    type(quantum_numbers_mask_t) :: qn_mask_conn
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask2
    type(evaluator_t), target :: eval, eval2, eval3

    call model%init_sm_test ()
    
    write (u, "(A)")   "*** Evaluator for matrix product"
    write (u, "(A)")   "***   Construct interaction for qq -> tt"
    write (u, "(A)")
    call interaction_init (int_qqtt, 2, 0, 2, set_relations=.true.)
    allocate (flv (4), col (4), hel (4), qn (4))
    allocate (qn_mask2 (4))
    do c = 1, 2
       select case (c)
       case (1)
          call color_init_col_acl (col, [1, 0, 1, 0], [0, 2, 0, 2])
       case (2)
          call color_init_col_acl (col, [1, 0, 2, 0], [0, 1, 0, 2])
       end select
       do f = 1, 2
          call flavor_init (flv, [f, -f, 6, -6], model)
          do h1 = -1, 1, 2
             call helicity_init (hel(3), h1)
             do h2 = -1, 1, 2
                call helicity_init (hel(4), h2)
                call quantum_numbers_init (qn, flv, col, hel)
                call interaction_add_state (int_qqtt, qn)
             end do
          end do
       end do
    end do
    call interaction_freeze (int_qqtt)
    deallocate (flv, col, hel, qn)
    write (u, "(A)")  "***   Construct interaction for t -> bW"
    call interaction_init (int_tbw, 1, 0, 2, set_relations=.true.)
    allocate (flv (3), col (3), hel (3), qn (3))
    call flavor_init (flv, [6, 5, 24], model)
    call color_init_col_acl (col, [1, 1, 0], [0, 0, 0])
    do h1 = -1, 1, 2
       call helicity_init (hel(1), h1)
       do h2 = -1, 1, 2
          call helicity_init (hel(2), h2)
          do h3 = -1, 1
             call helicity_init (hel(3), h3)
             call quantum_numbers_init (qn, flv, col, hel)
             call interaction_add_state (int_tbw, qn)
          end do
       end do
    end do
    call interaction_freeze (int_tbw)
    deallocate (flv, col, hel, qn)
    write (u, "(A)")  "***   Link interactions"
    call interaction_set_source_link (int_tbw, 1, int_qqtt, 3)
    qn_mask_conn = new_quantum_numbers_mask (.false.,.false.,.true.)
    write (u, "(A)")
    write (u, "(A)")  "***   Show input"
    call interaction_write (int_qqtt, unit = u)
    write (u, "(A)")
    call interaction_write (int_tbw, unit = u)
    write (u, "(A)")
    write (u, "(A)")  "***   Evaluate product"
    call evaluator_init_product &
         (eval, int_qqtt, int_tbw, qn_mask_conn)
    call eval%write (unit = u)

     call interaction_init (int1, 2, 0, 2, set_relations=.true.)
     call interaction_init (int2, 1, 0, 2, set_relations=.true.)
     p(1) = vector4_moving (1000._default, 1000._default, 3)
     p(2) = vector4_moving (200._default, 200._default, 2)
     p(3) = vector4_moving (100._default, 200._default, 1)
     p(4) = p(1) - p(2) - p(3)
     call interaction_set_momenta (int1, p)
     q(1) = vector4_moving (50._default,-50._default, 3)
     q(2) = p(2) + p(4) - q(1)
     call interaction_set_momenta (int2, q, outgoing=.true.)
     call interaction_set_matrix_element &
          (int1, [(2._default,0._default), &
          (4._default,1._default), (-3._default,0._default)])
     call interaction_set_matrix_element &
          (int2, [(-3._default,0._default), &
          (0._default,1._default), (1._default,2._default)])
     call evaluator_receive_momenta (eval)
     call eval%evaluate ()
     call interaction_write (int1, unit = u)
     write (u, "(A)")
     call interaction_write (int2, unit = u)
     write (u, "(A)")
     call eval%write (unit = u)
     write (u, "(A)")
     call interaction_final (int1)
     call interaction_final (int2)
     call evaluator_final (eval)

     write (u, "(A)")
     write (u, "(A)")   "*** Evaluator for matrix square"
     allocate (flv(4), col(4), qn(4))
     call interaction_init (int1, 2, 0, 2, set_relations=.true.)
     call flavor_init (flv, [1, -1, 21, 21], model)
     call color_init (col(1), [1])
     call color_init (col(2), [-2])
     call color_init (col(3), [2, -3])
     call color_init (col(4), [3, -1])
     call quantum_numbers_init (qn, flv, col)
     call interaction_add_state (int1, qn)
     call color_init (col(3), [3, -1])
     call color_init (col(4), [2, -3])
     call quantum_numbers_init (qn, flv, col)
     call interaction_add_state (int1, qn)
     call color_init (col(3), [2, -1])
     call color_init (col(4), .true.)
     call quantum_numbers_init (qn, flv, col)
     call interaction_add_state (int1, qn)
     call interaction_freeze (int1)
     ! [qn_mask2 not set since default is false]
     call evaluator_init_square (eval, int1, qn_mask2, nc=3)
     call evaluator_init_square_nondiag (eval2, int1, qn_mask2)
     qn_mask2 = new_quantum_numbers_mask (.false., .true., .true.)
     call evaluator_init_square_diag (eval3, eval%int, qn_mask2)
     call interaction_set_matrix_element &
          (int1, [(2._default,0._default), &
          (4._default,1._default), (-3._default,0._default)])
     call interaction_set_momenta (int1, p)
     call interaction_write (int1, unit = u)
     write (u, "(A)")
     call evaluator_receive_momenta (eval)
     call eval%evaluate ()
     call eval%write (unit = u)
     write (u, "(A)")
     call evaluator_receive_momenta (eval2)
     call eval2%evaluate ()
     call eval2%write (unit = u)
     write (u, "(A)")
     call evaluator_receive_momenta (eval3)
     call eval3%evaluate ()
     call eval3%write (unit = u)
     call interaction_final (int1)
     call evaluator_final (eval)
     call evaluator_final (eval2)
     call evaluator_final (eval3)
     
     call model%final ()
  end subroutine evaluator_1

  subroutine evaluator_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model       
    type(interaction_t), target :: int
    integer :: h1, h2, h3, h4
    type(helicity_t), dimension(4) :: hel
    type(color_t), dimension(4) :: col
    type(flavor_t), dimension(4) :: flv
    type(quantum_numbers_t), dimension(4) :: qn
    type(vector4_t), dimension(4) :: p
    type(evaluator_t) :: eval
    integer :: i
    
    call model%init_sm_test ()
        
    write (u, "(A)") "*** Creating interaction for e+ e- -> W+ W-"
    write (u, "(A)") 
    
    call flavor_init (flv, [11, -11, 24, -24], model)
    do i = 1, 4
       call color_init (col (i))
    end do
    call interaction_init (int, 2, 0, 2, set_relations=.true.)
    do h1 = -1, 1, 2
       call helicity_init (hel(1), h1)
       do h2 = -1, 1, 2
          call helicity_init (hel(2), h2)
          do h3 = -1, 1
             call helicity_init (hel(3), h3)
             do h4 = -1, 1
                call helicity_init (hel(4), h4)
                call quantum_numbers_init (qn, flv, col, hel)
                call interaction_add_state (int, qn)
             end do
          end do
       end do
    end do
    call interaction_freeze (int)
    call interaction_set_matrix_element (int, &
       [(cmplx (i, kind=default), i = 1, 36)])
    p(1) = vector4_moving (1000._default, 1000._default, 3)    
    p(2) = vector4_moving (1000._default, -1000._default, 3)
    p(3) = vector4_moving (1000._default, &
       sqrt (1E6_default - 80._default**2), 3)
    p(4) = p(1) + p(2) - p(3)
    call interaction_set_momenta (int, p)
    write (u, "(A)") "*** Setting up evaluator"
    write (u, "(A)")
    
    call evaluator_init_identity (eval, int)
    write (u, "(A)") "*** Transferring momenta and evaluating"
    write (u, "(A)")
    
    call evaluator_receive_momenta (eval)
    call eval%evaluate ()
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Interaction dump"
    write (u, "(A)")  "*******************************************************"
    call interaction_write (int, unit = u)
    write (u, "(A)")  
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Evaluator dump"
    write (u, "(A)")  "*******************************************************"
    call eval%write (unit = u)
    write (u, "(A)")  
    write (u, "(A)")   "*** cleaning up"
    call interaction_final (int)
    call evaluator_final (eval)
    
    call model%final ()
  end subroutine evaluator_2

  subroutine evaluator_3 (u)   
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(interaction_t), target :: int
    integer :: h1, h2, h3, h4
    type(helicity_t), dimension(4) :: hel
    type(color_t), dimension(4) :: col
    type(flavor_t), dimension(4) :: flv1, flv2
    type(quantum_numbers_t), dimension(4) :: qn
    type(vector4_t), dimension(4) :: p
    type(evaluator_t) :: eval1, eval2, eval3
    type(quantum_numbers_mask_t), dimension(4) :: qn_mask
    integer :: i
    
    call model%init_sm_test ()
            
    write (u, "(A)")  "*** Creating interaction for e+/mu+ e-/mu- -> W+ W-"
    call flavor_init (flv1, [11, -11, 24, -24], model)
    call flavor_init (flv2, [13, -13, 24, -24], model)
    do i = 1, 4
       call color_init (col (i))
    end do
    call interaction_init (int, 2, 0, 2, set_relations=.true.)
    do h1 = -1, 1, 2
       call helicity_init (hel(1), h1)
       do h2 = -1, 1, 2
          call helicity_init (hel(2), h2)
          do h3 = -1, 1
             call helicity_init (hel(3), h3)
             do h4 = -1, 1
                call helicity_init (hel(4), h4)
                call quantum_numbers_init (qn, flv1, col, hel)
                call interaction_add_state (int, qn)
                call quantum_numbers_init (qn, flv2, col, hel)
                call interaction_add_state (int, qn)
             end do
          end do
       end do
    end do
    call interaction_freeze (int)
    call interaction_set_matrix_element (int, &
       [(cmplx (1, kind=default), i = 1, 72)])
    p(1) = vector4_moving (1000._default, 1000._default, 3)    
    p(2) = vector4_moving (1000._default, -1000._default, 3)
    p(3) = vector4_moving (1000._default, &
       sqrt (1E6_default - 80._default**2), 3)
    p(4) = p(1) + p(2) - p(3)
    call interaction_set_momenta (int, p)
    write (u, "(A)")  "*** Setting up evaluators"
    call quantum_numbers_mask_init (qn_mask, .false., .true., .true.)
    call evaluator_init_qn_sum (eval1, int, qn_mask)
    call quantum_numbers_mask_init (qn_mask, .true., .true., .true.)
    call evaluator_init_qn_sum (eval2, int, qn_mask)
    call quantum_numbers_mask_init (qn_mask, .false., .true., .false.)
    call evaluator_init_qn_sum (eval3, int, qn_mask, &
      [.false., .false., .false., .true.])
    write (u, "(A)")  "*** Transferring momenta and evaluating"
    call evaluator_receive_momenta (eval1)
    call eval1%evaluate ()
    call evaluator_receive_momenta (eval2)
    call eval2%evaluate ()
    call evaluator_receive_momenta (eval3)
    call eval3%evaluate ()
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Interaction dump"
    write (u, "(A)")  "*******************************************************"
    call interaction_write (int, unit = u)
    write (u, "(A)")  
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Evaluator dump --- spin sum"
    write (u, "(A)")  "*******************************************************"
    call eval1%write (unit = u)
    call interaction_write (evaluator_get_int_ptr (eval1), unit = u)
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Evaluator dump --- spin / flavor sum"
    write (u, "(A)")  "*******************************************************"
    call eval2%write (unit = u)
    call interaction_write (evaluator_get_int_ptr (eval2), unit = u)
    write (u, "(A)")  "*******************************************************"
    write (u, "(A)")  "   Evaluator dump --- flavor sum, drop last W"
    write (u, "(A)")  "*******************************************************"
    call eval3%write (unit = u)
    call interaction_write (evaluator_get_int_ptr (eval3), unit = u)
    write (u, "(A)")  
    write (u, "(A)")  "*** cleaning up"
    call interaction_final (int)
    call evaluator_final (eval1)
    call evaluator_final (eval2)
    call evaluator_final (eval3)
    
    call model%final ()
  end subroutine evaluator_3


end module evaluators
