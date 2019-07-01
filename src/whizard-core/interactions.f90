! WHIZARD 2.1.0 June 15 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module interactions

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use sorting
  use subevents
  use expressions
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices

  implicit none
  private

  public :: external_link_get_ptr
  public :: external_link_get_index
  public :: interaction_t
  public :: interaction_init
  public :: interaction_final
  public :: interaction_write
  public :: assignment(=)
  public :: interaction_add_state
  public :: interaction_freeze
  public :: interaction_is_empty
  public :: interaction_get_n_matrix_elements
  public :: interaction_get_norm
  public :: interaction_get_quantum_numbers
  public :: interaction_get_matrix_element
  public :: interaction_set_matrix_element
  public :: interaction_normalize_by_trace
  public :: interaction_normalize_by_max
  public :: interaction_get_max_color_value
  public :: interaction_factorize
  public :: interaction_sum
  public :: interaction_add_color_contractions
  public :: interaction_evaluate_product
  public :: interaction_evaluate_product_cf
  public :: interaction_evaluate_square_c
  public :: interaction_evaluate_sum
  public :: interaction_get_tag
  public :: interaction_get_n_tot
  public :: interaction_get_n_in
  public :: interaction_get_n_vir
  public :: interaction_get_n_out
  public :: interaction_get_momenta
  public :: interaction_get_momentum
  public :: interaction_get_momenta_sub
  public :: interaction_to_subevt
  public :: interaction_momenta_to_subevt
  public :: interaction_get_state_matrix_ptr
  public :: interaction_get_resonance_flags
  public :: interaction_get_mask
  public :: interaction_get_s
  public :: interaction_get_cm_transformation
  public :: interaction_get_unstable_particle
  public :: interaction_set_mask
  public :: interaction_reset_momenta
  public :: interaction_set_momenta
  public :: interaction_set_momentum
  public :: interaction_set_flavored_values
  public :: interaction_relate
  public :: interaction_transfer_relations
  public :: interaction_relate_connections
  public :: interaction_get_children
  public :: interaction_get_parents
  public :: interaction_set_source_link
  public :: interaction_reassign_links
  public :: interaction_find_link
  public :: interaction_exchange_mask
  public :: interaction_receive_momenta
  public :: find_connections
  public :: interaction_test

  type :: external_link_t
     private
     type(interaction_t), pointer :: int => null ()
     integer :: i
  end type external_link_t

  type :: internal_link_t
     private
     integer :: i
     type(internal_link_t), pointer :: next => null ()
  end type internal_link_t

  type :: internal_link_list_t
     private
     integer :: length = 0
     type(internal_link_t), pointer :: first => null ()
     type(internal_link_t), pointer :: last => null ()
  end type internal_link_list_t

  type :: interaction_t
     private
     integer :: tag = 0
     type(state_matrix_t) :: state_matrix
     integer :: n_in = 0
     integer :: n_vir = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     logical, dimension(:), allocatable :: p_is_known
     type(vector4_t), dimension(:), allocatable :: p
     type(external_link_t), dimension(:), allocatable :: source
     type(internal_link_list_t), dimension(:), allocatable :: parents
     type(internal_link_list_t), dimension(:), allocatable :: children
     logical, dimension(:), allocatable :: resonant
     type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
     integer, dimension(:), allocatable :: hel_lock
     logical :: update_state_matrix = .false.
     logical :: update_values = .false.
  end type interaction_t


  interface assignment(=)
     module procedure internal_link_list_assign
  end interface

  interface assignment(=)
     module procedure interaction_assign
  end interface

  interface interaction_set_matrix_element
     module procedure interaction_set_matrix_element_qn
     module procedure interaction_set_matrix_element_all
     module procedure interaction_set_matrix_element_array 
     module procedure interaction_set_matrix_element_single
  end interface
  interface interaction_get_momenta
     module procedure interaction_get_momenta_all
     module procedure interaction_get_momenta_idx
  end interface
  interface interaction_momenta_to_subevt
     module procedure interaction_momenta_to_subevt_id
     module procedure interaction_momenta_to_subevt_tr
  end interface

  interface interaction_get_mask
     module procedure interaction_get_mask_all
     module procedure interaction_get_mask_slice
  end interface

  interface interaction_set_source_link
     module procedure interaction_set_source_link_int
  end interface

contains

  subroutine external_link_set (link, int, i)
    type(external_link_t), intent(out) :: link
    type(interaction_t), target, intent(in) :: int
    integer, intent(in) :: i
    if (i /= 0) then
       link%int => int
       link%i = i
    end if
  end subroutine external_link_set

  subroutine external_link_reassign (link, int_src, int_target)
    type(external_link_t), intent(inout) :: link
    type(interaction_t), intent(in) :: int_src
    type(interaction_t), intent(in), target :: int_target
    if (associated (link%int)) then
       if (link%int%tag == int_src%tag)  link%int => int_target
    end if
  end subroutine external_link_reassign

  function external_link_is_set (link) result (flag)
    logical :: flag
    type(external_link_t), intent(in) :: link
    flag = associated (link%int)
  end function external_link_is_set

  function external_link_get_ptr (link) result (int)
    type(interaction_t), pointer :: int
    type(external_link_t), intent(in) :: link
    int => link%int
  end function external_link_get_ptr

  function external_link_get_index (link) result (i)
    integer :: i
    type(external_link_t), intent(in) :: link
    i = link%i
  end function external_link_get_index

  function external_link_get_momentum_ptr (link) result (p)
    type(vector4_t), pointer :: p
    type(external_link_t), intent(in) :: link
    if (associated (link%int)) then
       p => link%int%p(link%i)
    else
       p => null ()
    end if
  end function external_link_get_momentum_ptr

  subroutine internal_link_list_append (link_list, i)
    type(internal_link_list_t), intent(inout) :: link_list
    integer, intent(in) :: i
    type(internal_link_t), pointer :: current
    allocate (current)
    current%i = i
    if (associated (link_list%first)) then
       link_list%last%next => current
    else
       link_list%first => current
    end if
    link_list%last => current
    link_list%length = link_list%length + 1
  end subroutine internal_link_list_append

  subroutine internal_link_list_final (link_list)
     type(internal_link_list_t), intent(inout) :: link_list
     type(internal_link_t), pointer :: current
     do while (associated (link_list%first))
        current => link_list%first
        link_list%first => current%next
        deallocate (current)
     end do
     link_list%last => null ()
     link_list%length = 0
   end subroutine internal_link_list_final

  subroutine internal_link_list_assign (link_list_out, link_list_in)
     type(internal_link_list_t), intent(in) :: link_list_in
     type(internal_link_list_t), intent(out) :: link_list_out
     type(internal_link_t), pointer :: current, copy
     current => link_list_in%first
     do while (associated (current))
        allocate (copy)
        copy%i = current%i
        if (associated (link_list_out%first)) then
           link_list_out%last%next => copy
        else
           link_list_out%first => copy
        end if
        link_list_out%last => copy
        current => current%next
     end do
   end subroutine internal_link_list_assign

  function internal_link_list_has_entries (link_list) result (flag)
    logical :: flag
    type(internal_link_list_t), intent(in) :: link_list
    flag = associated (link_list%first)
  end function internal_link_list_has_entries

  function internal_link_list_get_first_ptr (link_list) result (link)
    type(internal_link_list_t), intent(in) :: link_list
    type(internal_link_t), pointer :: link
    link => link_list%first
  end function internal_link_list_get_first_ptr

  subroutine internal_link_advance (link)
    type(internal_link_t), pointer :: link
    link => link%next
  end subroutine internal_link_advance

  function internal_link_get_index (link) result (i)
    integer :: i
    type(internal_link_t), intent(in) :: link
    i = link%i
  end function internal_link_get_index

  function internal_link_list_get_length (link_list) result (length)
    integer :: length
    type(internal_link_list_t), intent(in) :: link_list
    length = link_list%length
  end function internal_link_list_get_length

  subroutine interaction_init &
       (int, n_in, n_vir, n_out, &
        tag, resonant, mask, hel_lock, set_relations, store_values)
    type(interaction_t), intent(out) :: int
    integer, intent(in) :: n_in, n_vir, n_out
    integer, intent(in), optional :: tag
    logical, dimension(:), intent(in), optional :: resonant
    type(quantum_numbers_mask_t), dimension(:), intent(in), optional :: mask
    integer, dimension(:), intent(in), optional :: hel_lock
    logical, intent(in), optional :: set_relations, store_values
    logical :: set_rel
    integer :: i, j
    set_rel = .false.;  if (present (set_relations))  set_rel = set_relations
    call interaction_set_tag (int, tag)
    call state_matrix_init (int%state_matrix, store_values)
    int%n_in = n_in
    int%n_vir = n_vir
    int%n_out = n_out
    int%n_tot = n_in + n_vir + n_out
    allocate (int%p_is_known (int%n_tot))
    int%p_is_known = .false.
    allocate (int%p (int%n_tot))
    allocate (int%source (int%n_tot))
    allocate (int%parents (int%n_tot))
    allocate (int%children (int%n_tot))
    allocate (int%resonant (int%n_tot))
    if (present (resonant)) then
       int%resonant = resonant
    else
       int%resonant = .false.
    end if
    allocate (int%mask (int%n_tot))
    allocate (int%hel_lock (int%n_tot))
    if (present (mask)) then
       int%mask = mask
    end if
    if (present (hel_lock)) then
       int%hel_lock = hel_lock
    else
       int%hel_lock = 0
    end if
    int%update_state_matrix = .false.
    int%update_values = .true.
    if (set_rel) then
       do i = 1, n_in
          do j = 1, n_out
             call interaction_relate (int, i, n_in + j)
          end do
       end do
    end if
  end subroutine interaction_init

  subroutine interaction_set_tag (int, tag)
    type(interaction_t), intent(inout) :: int
    integer, intent(in), optional :: tag
    integer, save :: stored_tag = 1
    if (present (tag)) then
       int%tag = tag
    else
       int%tag = stored_tag
       stored_tag = stored_tag + 1
    end if
  end subroutine interaction_set_tag

  elemental subroutine interaction_final (int)
    type(interaction_t), intent(inout) :: int
    call state_matrix_final (int%state_matrix)
  end subroutine interaction_final

  subroutine interaction_write &
       (int, unit, verbose, show_momentum_sum, show_mass, show_state)
    type(interaction_t), intent(in) :: int
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    logical, intent(in), optional :: show_state
    integer :: u
    integer :: i, index_link
    type(internal_link_t), pointer :: link
    type(interaction_t), pointer :: int_link
    logical :: show_st
    u = output_unit (unit);  if (u < 0)  return
    show_st = .true.;  if (present (show_state))  show_st = show_state
    if (int%tag /= 0) then
       write (u, *)  "Interaction:", int%tag
       do i = 1, int%n_tot
          if (i == 1 .and. int%n_in > 0) then
             write (u, *) "Incoming:"
          else if (i == int%n_in + 1 .and. int%n_vir > 0) then
             write (u, *) "Virtual:"
          else if (i == int%n_in + int%n_vir + 1 .and. int%n_out > 0) then
             write (u, *) "Outgoing:"
          end if
          write (u, "(1x,A,1x,I0)", advance="no") "Particle", i
          if (allocated (int%resonant)) then
             if (int%resonant(i)) then
                write (u, *) "[r]"
             else
                write (u, *)
             end if
          else
             write (u, *)
          end if
          if (allocated (int%p)) then
             if (int%p_is_known(i)) then
                call vector4_write (int%p(i), u, show_mass)
             else
                write (u, *)  "  [momentum undefined]"
             end if
          else
             write (u, *) "  [momentum not allocated]"
          end if
          if (allocated (int%mask)) then
             write (u, "(1x,A)", advance="no")  "mask [fch] = "
             call quantum_numbers_mask_write (int%mask(i), u)
             write (u, *)
          end if
          if (internal_link_list_has_entries (int%parents(i)) &
               .or. internal_link_list_has_entries (int%children(i))) then
             write (u, "(1x,A)", advance="no") "internal links:"
             link => internal_link_list_get_first_ptr (int%parents(i))
             do while (associated (link))
                write (u, "(1x,I0)", advance="no") &
                     internal_link_get_index (link)
                call internal_link_advance (link)
             end do
             if (internal_link_list_has_entries (int%parents(i))) &
                  write (u, "(1x,A)", advance="no") "=>"
             write (u, "(1x,A)", advance="no") "X"
             if (internal_link_list_has_entries (int%children(i))) &
                  write (u, "(1x,A)", advance="no") "=>" 
             link => internal_link_list_get_first_ptr (int%children(i))
             do while (associated (link))
                write (u, "(1x,I0)", advance="no") &
                     internal_link_get_index (link)
                call internal_link_advance (link)
             end do
             write (u, *)
          end if
          if (allocated (int%hel_lock)) then
             if (int%hel_lock(i) /= 0) then
                write (u, "(1x,A,1x,I0)")  "helicity lock:", int%hel_lock(i)
             end if
          end if
          if (external_link_is_set (int%source(i))) then
             write (u, "(1x,A)", advance="no") "source:"
             int_link => external_link_get_ptr (int%source(i))
             index_link = external_link_get_index (int%source(i))
             write (u, "(1x,'(',I0,')',I0)", advance="no") &
                  int_link%tag, index_link
             write (u, *)
          end if
       end do
       if (present (show_momentum_sum)) then
          if (allocated (int%p) .and. show_momentum_sum) then
             write (u, *) "Incoming particles (sum):"
             call vector4_write &
                  (sum (int%p(1:int%n_in)), u, show_mass)
             write (u, *) "Outgoing particles (sum):"
             call vector4_write &
                  (sum (int%p(int%n_in+int%n_vir+1:)), u, show_mass)
             write (u, *)
          end if
       end if
       if (show_st) then
          call state_matrix_write (int%state_matrix, &
               write_value_list=verbose, verbose=verbose, unit=unit)
       end if
    else
       write (u, *) "Interaction: [empty]"
    end if
  end subroutine interaction_write

  subroutine interaction_assign (int_out, int_in)
    type(interaction_t), intent(out) :: int_out
    type(interaction_t), intent(in), target :: int_in
    call interaction_set_tag (int_out)
    int_out%state_matrix = int_in%state_matrix
    int_out%n_in  = int_in%n_in
    int_out%n_out = int_in%n_out
    int_out%n_vir = int_in%n_vir
    int_out%n_tot = int_in%n_tot
    if (allocated (int_in%p_is_known)) then
       allocate (int_out%p_is_known (size (int_in%p_is_known)))
       int_out%p_is_known = int_in%p_is_known
    end if
    if (allocated (int_in%p)) then
       allocate (int_out%p (size (int_in%p)))
       int_out%p = int_in%p
    end if
    if (allocated (int_in%source)) then
       allocate (int_out%source (size (int_in%source)))
       int_out%source = int_in%source
    end if
    if (allocated (int_in%parents)) then
       allocate (int_out%parents (size (int_in%parents)))
       int_out%parents = int_in%parents
    end if
    if (allocated (int_in%children)) then
       allocate (int_out%children (size (int_in%children)))
       int_out%children = int_in%children
    end if
    if (allocated (int_in%resonant)) then
       allocate (int_out%resonant (size (int_in%resonant)))
       int_out%resonant = int_in%resonant
    end if
    if (allocated (int_in%mask)) then
       allocate (int_out%mask (size (int_in%mask)))
       int_out%mask = int_in%mask
    end if
    if (allocated (int_in%hel_lock)) then
       allocate (int_out%hel_lock (size (int_in%hel_lock)))
       int_out%hel_lock = int_in%hel_lock
    end if
    int_out%update_state_matrix = int_in%update_state_matrix
    int_out%update_values = int_in%update_values
  end subroutine interaction_assign

  subroutine interaction_add_state &
       (int, qn, index, value, sum_values, counter_index, me_index)
    type(interaction_t), intent(inout) :: int
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    integer, intent(in), optional :: index
    complex(default), intent(in), optional :: value
    logical, intent(in), optional :: sum_values
    integer, intent(in), optional :: counter_index
    integer, intent(out), optional :: me_index
    type(quantum_numbers_t), dimension(size(qn)) :: qn_tmp
    qn_tmp = qn
    call quantum_numbers_undefine (qn_tmp, int%mask)
    call state_matrix_add_state &
         (int%state_matrix, qn_tmp, index, value, sum_values, &
         counter_index, me_index)
    int%update_values = .true.
  end subroutine interaction_add_state

  subroutine interaction_freeze (int)
    type(interaction_t), intent(inout) :: int
    if (int%update_state_matrix) then
       call state_matrix_collapse (int%state_matrix, int%mask)
       int%update_state_matrix = .false.
       int%update_values = .true.
    end if
    if (int%update_values) then
       call state_matrix_freeze (int%state_matrix)
       int%update_values = .false.
    end if
  end subroutine interaction_freeze

  function interaction_is_empty (int) result (flag)
    logical :: flag
    type(interaction_t), intent(in) :: int
    flag = state_matrix_is_empty (int%state_matrix)
  end function interaction_is_empty

  function interaction_get_n_matrix_elements (int) result (n)
    integer :: n
    type(interaction_t), intent(in) :: int
    n = state_matrix_get_n_matrix_elements (int%state_matrix)
  end function interaction_get_n_matrix_elements

  function interaction_get_norm (int) result (norm)
    real(default) :: norm
    type(interaction_t), intent(in) :: int
    norm = state_matrix_get_norm (int%state_matrix)
  end function interaction_get_norm

  function interaction_get_quantum_numbers (int, i) result (qn)
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    type(interaction_t), intent(in), target :: int
    integer, intent(in) :: i
    allocate (qn (state_matrix_get_depth (int%state_matrix)))
    qn = state_matrix_get_quantum_numbers (int%state_matrix, i)
  end function interaction_get_quantum_numbers

  function interaction_get_matrix_element (int, i) result (me)
    complex(default) :: me
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    me = state_matrix_get_matrix_element (int%state_matrix, i)
  end function interaction_get_matrix_element

  subroutine interaction_set_matrix_element_qn (int, qn, val)
    type(interaction_t), intent(inout) :: int
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    complex(default), intent(in) :: val
    call state_matrix_set_matrix_element (int%state_matrix, qn, val)
  end subroutine interaction_set_matrix_element_qn

  subroutine interaction_set_matrix_element_all (int, value)
    type(interaction_t), intent(inout) :: int
    complex(default), intent(in) :: value
    call state_matrix_set_matrix_element (int%state_matrix, value)
  end subroutine interaction_set_matrix_element_all

  subroutine interaction_set_matrix_element_array (int, value)
    type(interaction_t), intent(inout) :: int
    complex(default), dimension(:), intent(in) :: value
    call state_matrix_set_matrix_element (int%state_matrix, value)
  end subroutine interaction_set_matrix_element_array

  pure subroutine interaction_set_matrix_element_single (int, i, value)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    complex(default), intent(in) :: value
    call state_matrix_set_matrix_element (int%state_matrix, i, value)
  end subroutine interaction_set_matrix_element_single

  subroutine interaction_normalize_by_trace (int)
    type(interaction_t), intent(inout) :: int
    call state_matrix_normalize_by_trace (int%state_matrix)
  end subroutine interaction_normalize_by_trace

  subroutine interaction_normalize_by_max (int)
    type(interaction_t), intent(inout) :: int
    call state_matrix_normalize_by_max (int%state_matrix)
  end subroutine interaction_normalize_by_max

  function interaction_get_max_color_value (int) result (cmax)
    integer :: cmax
    type(interaction_t), intent(in) :: int
    cmax = state_matrix_get_max_color_value (int%state_matrix)
  end function interaction_get_max_color_value

  subroutine interaction_factorize &
       (int, mode, x, ok, single_state, correlated_state, qn_in)
    type(interaction_t), intent(in), target :: int
    integer, intent(in) :: mode
    real(default), intent(in) :: x
    logical, intent(out) :: ok
    type(state_matrix_t), &
         dimension(:), allocatable, intent(out) :: single_state
    type(state_matrix_t), intent(out), optional :: correlated_state
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_in
    call state_matrix_factorize &
         (int%state_matrix, mode, x, ok, single_state, correlated_state, qn_in)
  end subroutine interaction_factorize

  function interaction_sum (int) result (value)
    complex(default) :: value
    type(interaction_t), intent(in) :: int
    value = state_matrix_sum (int%state_matrix)
  end function interaction_sum

  subroutine interaction_add_color_contractions (int)
    type(interaction_t), intent(inout) :: int
    call state_matrix_add_color_contractions (int%state_matrix)
  end subroutine interaction_add_color_contractions

  pure subroutine interaction_evaluate_product &
       (int, i, int1, int2, index1, index2)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1, int2
    integer, dimension(:), intent(in) :: index1, index2
    call state_matrix_evaluate_product &
         (int%state_matrix, i, int1%state_matrix, int2%state_matrix, &
          index1, index2)
  end subroutine interaction_evaluate_product

  pure subroutine interaction_evaluate_product_cf &
       (int, i, int1, int2, index1, index2, factor)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1, int2
    integer, dimension(:), intent(in) :: index1, index2
    complex(default), dimension(:), intent(in) :: factor
    call state_matrix_evaluate_product_cf &
         (int%state_matrix, i, int1%state_matrix, int2%state_matrix, &
          index1, index2, factor)
  end subroutine interaction_evaluate_product_cf

  pure subroutine interaction_evaluate_square_c (int, i, int1, index1)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1
    integer, dimension(:), intent(in) :: index1
    call state_matrix_evaluate_square_c &
         (int%state_matrix, i, int1%state_matrix, index1)
  end subroutine interaction_evaluate_square_c

  pure subroutine interaction_evaluate_sum (int, i, int1, index1)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1
    integer, dimension(:), intent(in) :: index1
    call state_matrix_evaluate_sum &
         (int%state_matrix, i, int1%state_matrix, index1)
  end subroutine interaction_evaluate_sum

  function interaction_get_tag (int) result (tag)
    integer :: tag
    type(interaction_t), intent(in) :: int
    tag = int%tag
  end function interaction_get_tag

  function interaction_get_n_tot (int) result (n_tot)
    integer :: n_tot
    type(interaction_t), intent(in) :: int
    n_tot = int%n_tot
  end function interaction_get_n_tot

  function interaction_get_n_in (int) result (n_in)
    integer :: n_in
    type(interaction_t), intent(in) :: int
    n_in = int%n_in
  end function interaction_get_n_in

  function interaction_get_n_vir (int) result (n_vir)
    integer :: n_vir
    type(interaction_t), intent(in) :: int
    n_vir = int%n_vir
  end function interaction_get_n_vir

  function interaction_get_n_out (int) result (n_out)
    integer :: n_out
    type(interaction_t), intent(in) :: int
    n_out = int%n_out
  end function interaction_get_n_out

  function idx (int, i, outgoing)
    integer :: idx
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    logical, intent(in), optional :: outgoing
    logical :: in, vir, out
    if (present (outgoing)) then
       in  = .not. outgoing
       vir = .false.
       out = outgoing
    else
       in = .true.
       vir = .true.
       out = .true.
    end if
    idx = 0
    if (in) then
       if (vir) then
          if (out) then
             if (i <= int%n_tot)  idx = i
          else
             if (i <= int%n_in + int%n_vir)  idx = i
          end if
       else if (out) then
          if (i <= int%n_in) then
             idx = i
          else if (i <= int%n_in + int%n_out) then
             idx = int%n_vir + i
          end if
       else
          if (i <= int%n_in)  idx = i
       end if
    else if (vir) then
       if (out) then
          if (i <= int%n_vir + int%n_out)  idx = int%n_in + i
       else
          if (i <= int%n_vir)  idx = int%n_in + i
       end if
    else if (out) then
       if (i <= int%n_out)  idx = int%n_in + int%n_vir + i
    end if
    if (idx == 0) then
       call interaction_write (int)
       print *, i, in, vir, out
       call msg_bug (" Momentum index is out of range for this interaction")
    end if
  end function idx

  function interaction_get_momenta_all (int, outgoing) result (p)
    type(vector4_t), dimension(:), allocatable :: p
    type(interaction_t), intent(in) :: int
    logical, intent(in), optional :: outgoing
    integer :: i
    if (present (outgoing)) then
       if (outgoing) then
          allocate (p (int%n_out))
       else
          allocate (p (int%n_in))
       end if
    else
       allocate (p (int%n_tot))
    end if
    do i = 1, size (p)
       p(i) = int%p(idx (int, i, outgoing))
    end do
  end function interaction_get_momenta_all

  function interaction_get_momenta_idx (int, jj) result (p)
    type(vector4_t), dimension(:), allocatable :: p
    type(interaction_t), intent(in) :: int
    integer, dimension(:), intent(in) :: jj
    allocate (p (size (jj)))
    p = int%p(jj)
  end function interaction_get_momenta_idx

  function interaction_get_momentum (int, i, outgoing) result (p)
    type(vector4_t) :: p
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    logical, intent(in), optional :: outgoing
    p = int%p(idx (int, i, outgoing))
  end function interaction_get_momentum

  subroutine interaction_get_momenta_sub (int, p, outgoing)
    type(vector4_t), dimension(:), intent(out) :: p
    type(interaction_t), intent(in) :: int
    logical, intent(in), optional :: outgoing
    integer :: i
    do i = 1, size (p)
       p(i) = int%p(idx (int, i, outgoing))
    end do
  end subroutine interaction_get_momenta_sub

  subroutine interaction_to_subevt (int, j_beam, j_in, j_out, subevt)
    type(interaction_t), intent(in), target :: int
    integer, dimension(:), intent(in) :: j_beam, j_in, j_out
    type(subevt_t), intent(out) :: subevt
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: n_beam, n_in, n_out, i, j
    allocate (flv (int%n_tot))
    flv = quantum_numbers_get_flavor (interaction_get_quantum_numbers (int, 1))
    n_beam = size (j_beam)
    n_in = size (j_in)
    n_out = size (j_out)
    call subevt_init (subevt, n_beam + n_in + n_out)
    do i = 1, n_beam
       j = j_beam(i)
       call subevt_set_beam (subevt, i, &
            flavor_get_pdg (flv(j)), &
            vector4_null, &
            flavor_get_mass (flv(j)) ** 2)
    end do
    do i = 1, n_in
       j = j_in(i)
       call subevt_set_incoming (subevt, n_beam + i, &
            flavor_get_pdg (flv(j)), &
            vector4_null, &
            flavor_get_mass (flv(j)) ** 2)
    end do
    do i = 1, n_out
       j = j_out(i)
       call subevt_set_outgoing (subevt, n_beam + n_in + i, &
            flavor_get_pdg (flv(j)), &
            vector4_null, &
            flavor_get_mass (flv(j)) ** 2)
    end do
  end subroutine interaction_to_subevt

  subroutine interaction_momenta_to_subevt_id (int, j_beam, j_in, j_out, subevt)
    type(interaction_t), intent(in) :: int
    integer, dimension(:), intent(in) :: j_beam, j_in, j_out
    type(subevt_t), intent(inout) :: subevt
    call subevt_set_p_beam &
         (subevt, - interaction_get_momenta (int, j_beam))
    call subevt_set_p_incoming &
         (subevt, - interaction_get_momenta (int, j_in))
    call subevt_set_p_outgoing &
         (subevt, interaction_get_momenta (int, j_out))
  end subroutine interaction_momenta_to_subevt_id

  subroutine interaction_momenta_to_subevt_tr &
       (int, j_beam, j_in, j_out, lt, subevt)
    type(interaction_t), intent(in) :: int
    integer, dimension(:), intent(in) :: j_beam, j_in, j_out
    type(subevt_t), intent(inout) :: subevt
    type(lorentz_transformation_t), intent(in) :: lt
    call subevt_set_p_beam &
         (subevt, - lt * interaction_get_momenta (int, j_beam))
    call subevt_set_p_incoming &
         (subevt, - lt * interaction_get_momenta (int, j_in))
    call subevt_set_p_outgoing &
         (subevt, lt * interaction_get_momenta (int, j_out))
  end subroutine interaction_momenta_to_subevt_tr

  function interaction_get_state_matrix_ptr (int) result (state)
    type(state_matrix_t), pointer :: state
    type(interaction_t), intent(in), target :: int
    state => int%state_matrix
  end function interaction_get_state_matrix_ptr

  function interaction_get_resonance_flags (int) result (resonant)
    type(interaction_t), intent(in) :: int
    logical, dimension(size(int%resonant)) :: resonant
    resonant = int%resonant
  end function interaction_get_resonance_flags

  function interaction_get_mask_all (int) result (mask)
    type(interaction_t), intent(in) :: int
    type(quantum_numbers_mask_t), dimension(size(int%mask)) :: mask
    mask = int%mask
  end function interaction_get_mask_all

  function interaction_get_mask_slice (int, index) result (mask)
    type(interaction_t), intent(in) :: int
    integer, dimension(:), intent(in) :: index
    type(quantum_numbers_mask_t), dimension(size(index)) :: mask
    mask = int%mask(index)
  end function interaction_get_mask_slice

  function interaction_get_s (int) result (s)
    real(default) :: s
    type(interaction_t), intent(in) :: int
    if (int%n_in /= 0) then
       s = sum (int%p(:int%n_in)) ** 2
    else
       s = sum (int%p(int%n_vir+1:)) ** 2
    end if
  end function interaction_get_s

  function interaction_get_cm_transformation (int) result (lt)
    type(lorentz_transformation_t) :: lt
    type(interaction_t), intent(in) :: int
    type(vector4_t) :: p_cm
    real(default) :: s
    if (int%n_in /= 0) then
       p_cm = sum (int%p(:int%n_in))
    else
       p_cm = sum (int%p(int%n_vir+1:))
    end if
    s = p_cm ** 2
    if (s > 0) then
       lt = boost (p_cm, sqrt (s))
    else
       lt = identity
    end if
  end function interaction_get_cm_transformation

  subroutine interaction_get_unstable_particle (int, flv, p, i)
    type(interaction_t), intent(in), target :: int
    type(flavor_t), intent(out) :: flv
    type(vector4_t), intent(out) :: p
    integer, intent(out) :: i
    type(state_iterator_t) :: it
    type(flavor_t), dimension(int%n_tot) :: flv_array
    call state_iterator_init (it, int%state_matrix)
    flv_array = state_iterator_get_flavor (it)
    do i = int%n_in + int%n_vir + 1, int%n_tot
       if (.not. flavor_is_stable (flv_array(i))) then
          flv = flv_array(i)
          p = int%p(i)
          return
       end if
    end do
  end subroutine interaction_get_unstable_particle

  subroutine interaction_set_mask (int, mask)
    type(interaction_t), intent(inout) :: int
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    int%mask = mask
    int%update_state_matrix = .true.
  end subroutine interaction_set_mask

  subroutine interaction_merge_mask_entry (int, i, mask)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(quantum_numbers_mask_t), intent(in) :: mask
    type(quantum_numbers_mask_t) :: mask_tmp
    integer :: ii
    ii = idx (int, i)
    if (int%mask(ii) .neqv. mask) then
       int%mask(ii) = int%mask(ii) .or. mask
       if (int%hel_lock(ii) /= 0) then
          call quantum_numbers_mask_assign (mask_tmp, mask, helicity=.true.)
          int%mask(int%hel_lock(ii)) = int%mask(int%hel_lock(ii)) .or. mask_tmp
       end if
    end if
    int%update_state_matrix = .true.
  end subroutine interaction_merge_mask_entry

  subroutine interaction_reset_momenta (int)
    type(interaction_t), intent(inout) :: int
    int%p = vector4_null
    int%p_is_known = .true.
  end subroutine interaction_reset_momenta

  subroutine interaction_set_momenta (int, p, outgoing)
    type(interaction_t), intent(inout) :: int
    type(vector4_t), dimension(:), intent(in) :: p
    logical, intent(in), optional :: outgoing
    integer :: i, index
    do i = 1, size (p)
       index = idx (int, i, outgoing)
       int%p(index) = p(i)
       int%p_is_known(index) = .true.
    end do
  end subroutine interaction_set_momenta

  subroutine interaction_set_momentum (int, p, i, outgoing)
    type(interaction_t), intent(inout) :: int
    type(vector4_t), intent(in) :: p
    integer, intent(in) :: i
    logical, intent(in), optional :: outgoing
    integer :: index
    index = idx (int, i, outgoing)
    int%p(index) = p
    int%p_is_known(index) = .true.
  end subroutine interaction_set_momentum

  subroutine interaction_set_flavored_values (int, value, flv_in, pos)
    type(interaction_t), intent(inout) :: int
    complex(default), dimension(:), intent(in) :: value
    type(flavor_t), dimension(:), intent(in) :: flv_in
    integer, intent(in) :: pos
    type(state_iterator_t) :: it
    type(flavor_t) :: flv
    integer :: i
    if (size (value) == 1) then
       call interaction_set_matrix_element (int, value(1))
    else
       call state_iterator_init (it, int%state_matrix)
       do while (state_iterator_is_valid (it))
          flv = state_iterator_get_flavor (it, pos)
          SCAN_FLV: do i = 1, size (value)
             if (flv == flv_in(i)) then
                call state_iterator_set_matrix_element (it, value(i))
                exit SCAN_FLV
             end if
          end do SCAN_FLV
          call state_iterator_advance (it)
       end do
    end if
  end subroutine interaction_set_flavored_values

  subroutine interaction_relate (int, i1, i2)
    type(interaction_t), intent(inout), target :: int
    integer, intent(in) :: i1, i2
    if (i1 /= 0 .and. i2 /= 0) then
       call internal_link_list_append (int%children(i1), i2)
       call internal_link_list_append (int%parents(i2), i1)
    end if
  end subroutine interaction_relate

  subroutine interaction_transfer_relations (int1, int2, map)
    type(interaction_t), intent(in) :: int1
    type(interaction_t), intent(inout), target :: int2
    integer, dimension(:), intent(in) :: map
    type(internal_link_t), pointer :: link
    integer :: i, k
    do i = 1, size (map)
       link => internal_link_list_get_first_ptr (int1%parents(i))
       do while (associated (link))
          k = internal_link_get_index (link)
          call interaction_relate (int2, map(k), map(i))
          call internal_link_advance (link)
       end do
       if (map(i) /= 0) then
          int2%resonant(map(i)) = int1%resonant(i)
       end if
    end do
  end subroutine interaction_transfer_relations

  subroutine interaction_relate_connections &
       (int, int_in, connection_index, &
        map, map_connections, resonant)
    type(interaction_t), intent(inout), target :: int
    type(interaction_t), intent(in) :: int_in
    integer, dimension(:), intent(in) :: connection_index
    integer, dimension(:), intent(in) :: map, map_connections
    logical, intent(in), optional :: resonant
    logical :: reson
    integer :: i, i2, k2
    type(internal_link_t), pointer :: link
    reson = .false.;  if (present (resonant))  reson = resonant 
    do i = 1, size (map_connections)
       k2 = connection_index(i)
       link => internal_link_list_get_first_ptr (int_in%children(k2))
       do while (associated (link))
          i2 = internal_link_get_index (link)
          call interaction_relate (int, map_connections(i), map(i2))
          call internal_link_advance (link)
       end do
       int%resonant(map_connections(i)) = reson
    end do
  end subroutine interaction_relate_connections

  function interaction_get_children (int, i) result (idx)
    integer, dimension(:), allocatable :: idx
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    integer :: k
    type(internal_link_t), pointer :: link
    allocate (idx (internal_link_list_get_length (int%children(i))))
    k = 0
    link => internal_link_list_get_first_ptr (int%children(i))
    do while (associated (link))
       k = k + 1
       idx(k) = internal_link_get_index (link)
       call internal_link_advance (link)
    end do
  end function interaction_get_children

  function interaction_get_parents (int, i) result (idx)
    integer, dimension(:), allocatable :: idx
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    integer :: k
    type(internal_link_t), pointer :: link
    allocate (idx (internal_link_list_get_length (int%parents(i))))
    k = 0
    link => internal_link_list_get_first_ptr (int%parents(i))
    do while (associated (link))
       k = k + 1
       idx(k) = internal_link_get_index (link)
       call internal_link_advance (link)
    end do
  end function interaction_get_parents

  subroutine interaction_set_source_link_int (int, i, int1, i1)
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in), target :: int1
    integer, intent(in) :: i1
    if (i /= 0)  call external_link_set (int%source(i), int1, i1)
  end subroutine interaction_set_source_link_int

  subroutine interaction_reassign_links (int, int_src, int_target)
    type(interaction_t), intent(inout) :: int
    type(interaction_t), intent(in) :: int_src
    type(interaction_t), intent(in), target :: int_target
    integer :: i
    if (allocated (int%source)) then
       do i = 1, size (int%source)
          call external_link_reassign (int%source(i), int_src, int_target)
       end do
    end if
  end subroutine interaction_reassign_links

  function interaction_find_link (int, int1, i1) result (i)
    integer :: i
    type(interaction_t), intent(in) :: int, int1
    integer, intent(in) :: i1
    type(interaction_t), pointer :: int_tmp
    do i = 1, int%n_tot
       int_tmp => external_link_get_ptr (int%source(i))
       if (int_tmp%tag == int1%tag) then
          if (external_link_get_index (int%source(i)) == i1)  return
       end if
    end do
    i = 0
  end function interaction_find_link

  function interaction_get_ultimate_source (int, i) result (link)
    type(external_link_t) :: link
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    type(interaction_t), pointer :: int_src
    integer :: i_src
    link = int%source(i)
    if (external_link_is_set (link)) then
       do
          int_src => external_link_get_ptr (link)
          i_src = external_link_get_index (link)
          if (external_link_is_set (int_src%source(i_src))) then
             link = int_src%source(i_src)
          else
             exit
          end if
       end do
    end if
  end function interaction_get_ultimate_source

  subroutine interaction_exchange_mask (int)
    type(interaction_t), intent(inout) :: int
    integer :: i, index_link
    type(interaction_t), pointer :: int_link
    do i = 1, int%n_tot
       if (external_link_is_set (int%source(i))) then
          int_link => external_link_get_ptr (int%source(i))
          index_link = external_link_get_index (int%source(i))
          call interaction_merge_mask_entry &
               (int, i, int_link%mask(index_link))
          call interaction_merge_mask_entry &
               (int_link, index_link, int%mask(i))
       end if
    end do
    call interaction_freeze (int)
  end subroutine interaction_exchange_mask

  subroutine interaction_receive_momenta (int)
    type(interaction_t), intent(inout) :: int
    integer :: i, index_link
    type(interaction_t), pointer :: int_link
    do i = 1, int%n_tot
       if (external_link_is_set (int%source(i))) then
          int_link => external_link_get_ptr (int%source(i))
          index_link = external_link_get_index (int%source(i))
          call interaction_set_momentum (int, int_link%p(index_link), i)
       end if
    end do
  end subroutine interaction_receive_momenta

  subroutine find_connections (int1, int2, n, connection_index)
    type(interaction_t), intent(in) :: int1, int2
    integer, intent(out) :: n
    integer, dimension(:,:), intent(out), allocatable :: connection_index
    integer, dimension(:,:), allocatable :: conn_index_tmp
    integer, dimension(:), allocatable :: ordering
    integer :: i, j, k
    type(external_link_t) :: link2, link1
    type(interaction_t), pointer :: int_link, int_link1
    n = 0
    do i = 1, size (int2%source)
       link2 = interaction_get_ultimate_source (int2, i)
       if (external_link_is_set (link2)) then
          int_link => external_link_get_ptr (link2)
          if (int_link%tag == int1%tag) then
             n = n + 1
          else
             k = external_link_get_index (link2)
             do j = 1, size (int1%source)
                link1 = interaction_get_ultimate_source (int1, j)
                if (external_link_is_set (link1)) then
                   int_link1 => external_link_get_ptr (link1)
                   if (int_link1%tag == int_link%tag) then
                      if (external_link_get_index (link1) == k) then
                         n = n + 1
                      end if
                   end if
                end if
             end do
          end if
       end if
    end do
    allocate (conn_index_tmp (n, 2))
    n = 0
    do i = 1, size (int2%source)
       link2 = interaction_get_ultimate_source (int2, i)
       if (external_link_is_set (link2)) then
          int_link => external_link_get_ptr (link2)
          if (int_link%tag == int1%tag) then
             n = n + 1
             conn_index_tmp(n,1) = external_link_get_index (int2%source(i))
             conn_index_tmp(n,2) = i
          else
             k = external_link_get_index (link2)
             do j = 1, size (int1%source)
                link1 = interaction_get_ultimate_source (int1, j)
                if (external_link_is_set (link1)) then
                   int_link1 => external_link_get_ptr (link1)
                   if (int_link1%tag == int_link%tag) then
                      if (external_link_get_index (link1) == k) then
                         n = n + 1
                         conn_index_tmp(n,1) = j
                         conn_index_tmp(n,2) = i
                      end if
                   end if
                end if
             end do
          end if
       end if
    end do
    allocate (connection_index (n, 2))
    if (n > 1) then
       allocate (ordering (n))
       ordering = order (conn_index_tmp(:,1))
       connection_index = conn_index_tmp(ordering,:)
    else               
       connection_index = conn_index_tmp
    end if
  end subroutine find_connections

  subroutine interaction_test ()
    type(interaction_t), target :: int, rad
    type(vector4_t), dimension(3) :: p
    type(quantum_numbers_mask_t), dimension(3) :: mask
    p(2) = vector4_moving (500._default, 500._default, 1)
    p(3) = vector4_moving (500._default,-500._default, 1)
    p(1) = p(2) + p(3)
    call interaction_init (int, 1, 0, 2, set_relations=.true., store_values = .true. )
    call int_set (int, 1, -1, 1, 1, cmplx (0.3_default, 0.1_default, kind=default))
    call int_set (int, 1, -1,-1, 1, cmplx (0.5_default,-0.7_default, kind=default))
    call int_set (int, 1, 1, 1, 1, cmplx (0.1_default, 0._default, kind=default))
    call int_set (int, -1, 1, -1, 2, cmplx (0.4_default, -0.1_default, kind=default))
    call int_set (int, 1, 1, 1, 2, cmplx (0.2_default, 0._default, kind=default))
    call interaction_freeze (int)
    call interaction_set_momenta (int, p)
    mask = new_quantum_numbers_mask (.false.,.false., (/.true.,.true.,.true./))
    call interaction_init (rad, 1, 0, 2, mask=mask, set_relations=.true., store_values = .true.)
    call rad_set (1)
    call rad_set (2)
    call interaction_set_source_link (rad, 1, int, 2)
    call interaction_exchange_mask (rad)
    call interaction_receive_momenta (rad)
    p(1) = interaction_get_momentum (rad, 1)
    p(2) = 0.4_default * p(1)
    p(3) = p(1) - p(2)
    call interaction_set_momenta (rad, p(2:3), outgoing=.true.)
    call interaction_freeze (int)
    call interaction_freeze (rad)
    call interaction_set_matrix_element (rad, cmplx (0._default, 0._default, kind=default))
    call interaction_write (int)
    print *
    call interaction_write (rad)
    call interaction_final (int)
    call interaction_final (rad)
  contains
    subroutine int_set (int, h1, h2, hq, q, val)
      type(interaction_t), target, intent(inout) :: int
      integer, intent(in) :: h1, h2, hq, q
      type(flavor_t), dimension(3) :: flv
      type(color_t), dimension(3) :: col
      type(helicity_t), dimension(3) :: hel
      type(quantum_numbers_t), dimension(3) :: qn
      complex(default), intent(in) :: val
      call flavor_init (flv, (/21, q, -q/))
      call color_init_col_acl (col(2), 5, 0)
      call color_init_col_acl (col(3), 0, 5)
      call helicity_init (hel, (/h1, hq, -hq/), (/h2, hq, -hq/))
      call quantum_numbers_init (qn, flv, col, hel)
      call interaction_add_state (int, qn)
      call interaction_set_matrix_element (int, val)
    end subroutine int_set
    subroutine rad_set (q)
      integer, intent(in) :: q
      type(flavor_t), dimension(3) :: flv
      type(quantum_numbers_t), dimension(3) :: qn
      call flavor_init (flv, (/ q, q, 21 /))
      call quantum_numbers_init (qn, flv)
      call interaction_add_state (rad, qn)
    end subroutine rad_set
  end subroutine interaction_test


end module interactions
