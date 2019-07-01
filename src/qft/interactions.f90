! WHIZARD 2.4.1 Mar 24 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module interactions

  use kinds, only: default
  use io_units
  use diagnostics
  use sorting
  use lorentz
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
  public :: reset_interaction_counter
  public :: assignment(=)
  public :: interaction_get_s
  public :: interaction_get_cm_transformation
  public :: interaction_get_unstable_particle
  public :: interaction_get_flv_out
  public :: interaction_get_flv_content
  public :: interaction_set_flavored_values
  public :: interaction_get_n_children
  public :: interaction_get_n_parents
  public :: interaction_get_children
  public :: interaction_get_parents
  public :: interaction_reassign_links
  public :: interaction_find_link
  public :: interaction_exchange_mask
  public :: interaction_send_momenta
  public :: interaction_pacify_momenta
  public :: interaction_declare_subtraction
  public :: find_connections

  type :: external_link_t
     private
     type(interaction_t), pointer :: int => null ()
     integer :: i
  end type external_link_t

  type :: internal_link_list_t
     private
     integer :: length = 0
     integer, dimension(:), allocatable :: link
   contains
     procedure :: write => internal_link_list_write
     procedure :: append => internal_link_list_append
     procedure :: has_entries => internal_link_list_has_entries
     procedure :: get_length => internal_link_list_get_length
     procedure :: get_link => internal_link_list_get_link
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
   contains
     procedure :: basic_init => interaction_init
     procedure :: final => interaction_final
     procedure :: basic_write => interaction_write
     procedure :: write_state_matrix => interaction_write_state_matrix
     procedure :: reduce_state_matrix => interaction_reduce_state_matrix
     procedure :: add_state => interaction_add_state
     procedure :: freeze => interaction_freeze
     procedure :: is_empty => interaction_is_empty
     procedure :: get_n_matrix_elements => &
          interaction_get_n_matrix_elements
     procedure :: get_n_in_helicities => interaction_get_n_in_helicities
     procedure :: get_me_size => interaction_get_me_size
     procedure :: get_norm => interaction_get_norm
     procedure :: get_n_sub => interaction_get_n_sub
     generic :: get_quantum_numbers => get_quantum_numbers_single, &
                                       get_quantum_numbers_all, &
                                       get_quantum_numbers_all_qn_mask
     procedure :: get_quantum_numbers_single => &
        interaction_get_quantum_numbers_single
     procedure :: get_quantum_numbers_all => &
        interaction_get_quantum_numbers_all
     procedure :: get_quantum_numbers_all_qn_mask => &
        interaction_get_quantum_numbers_all_qn_mask
     procedure :: get_quantum_numbers_all_sub => interaction_get_quantum_numbers_all_sub
     procedure :: get_quantum_numbers_mask => interaction_get_quantum_numbers_mask
     generic :: get_matrix_element => get_matrix_element_single
     generic :: get_matrix_element => get_matrix_element_array
     procedure :: get_matrix_element_single => &
        interaction_get_matrix_element_single
     procedure :: get_matrix_element_array => &
        interaction_get_matrix_element_array
     generic :: set_matrix_element => interaction_set_matrix_element_qn, &
          interaction_set_matrix_element_all, &
          interaction_set_matrix_element_array, &
          interaction_set_matrix_element_single, &
          interaction_set_matrix_element_clone
     procedure :: interaction_set_matrix_element_qn
     procedure :: interaction_set_matrix_element_all
     procedure :: interaction_set_matrix_element_array
     procedure :: interaction_set_matrix_element_single
     procedure :: interaction_set_matrix_element_clone
     procedure :: set_only_matrix_element => interaction_set_only_matrix_element
     procedure :: add_to_matrix_element => interaction_add_to_matrix_element
     procedure :: get_diagonal_entries => interaction_get_diagonal_entries
     procedure :: normalize_by_trace => interaction_normalize_by_trace
     procedure :: normalize_by_max => interaction_normalize_by_max
     procedure :: set_norm => interaction_set_norm
     procedure :: set_state_matrix => interaction_set_state_matrix
     procedure :: get_max_color_value => &
          interaction_get_max_color_value
     procedure :: factorize => interaction_factorize
     procedure :: sum => interaction_sum
     procedure :: add_color_contractions => &
          interaction_add_color_contractions
     procedure :: evaluate_product => interaction_evaluate_product
     procedure :: evaluate_product_cf => interaction_evaluate_product_cf
     procedure :: evaluate_square_c => interaction_evaluate_square_c
     procedure :: evaluate_sum => interaction_evaluate_sum
     procedure :: evaluate_me_sum => interaction_evaluate_me_sum
     procedure :: get_tag => interaction_get_tag
     procedure :: get_n_tot => interaction_get_n_tot
     procedure :: get_n_in => interaction_get_n_in
     procedure :: get_n_vir => interaction_get_n_vir
     procedure :: get_n_out => interaction_get_n_out
     generic :: get_momenta => get_momenta_all, get_momenta_idx
     procedure :: get_momentum => interaction_get_momentum
     procedure :: get_momenta_all => interaction_get_momenta_all
     procedure :: get_momenta_idx => interaction_get_momenta_idx
     procedure :: get_momenta_sub => interaction_get_momenta_sub
     procedure :: get_state_matrix_ptr => &
          interaction_get_state_matrix_ptr
     procedure :: get_resonance_flags => interaction_get_resonance_flags
     generic :: get_mask => get_mask_all, get_mask_slice
     procedure :: get_mask_all => interaction_get_mask_all
     procedure :: get_mask_slice => interaction_get_mask_slice
     procedure :: set_mask => interaction_set_mask
     procedure :: reset_momenta => interaction_reset_momenta
     procedure :: set_momenta => interaction_set_momenta
     procedure :: set_momentum => interaction_set_momentum
     procedure :: relate => interaction_relate
     procedure :: transfer_relations => interaction_transfer_relations
     procedure :: relate_connections => interaction_relate_connections
     procedure :: set_source_link => interaction_set_source_link
     procedure :: find_source => interaction_find_source
     procedure :: receive_momenta => interaction_receive_momenta
  end type interaction_t


  interface assignment(=)
     module procedure interaction_assign
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

  subroutine internal_link_list_write (object, unit)
    class(internal_link_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    do i = 1, object%length
       write (u, "(1x,I0)", advance="no")  object%link(i)
    end do
  end subroutine internal_link_list_write

  subroutine internal_link_list_append (link_list, link)
    class(internal_link_list_t), intent(inout) :: link_list
    integer, intent(in) :: link
    integer :: l, j
    integer, dimension(:), allocatable :: tmp
    l = link_list%length
    if (allocated (link_list%link)) then
       if (l == size (link_list%link)) then
          allocate (tmp (2 * l))
          tmp(:l) = link_list%link
          call move_alloc (from = tmp, to = link_list%link)
       end if
    else
       allocate (link_list%link (2))
    end if
    link_list%link(l+1) = link
    SHIFT_LINK_IN_PLACE: do j = l, 1, -1
       if (link >= link_list%link(j)) then
          exit SHIFT_LINK_IN_PLACE
       else
          link_list%link(j+1) = link_list%link(j)
          link_list%link(j) = link
       end if
    end do SHIFT_LINK_IN_PLACE
    link_list%length = l + 1
  end subroutine internal_link_list_append

  function internal_link_list_has_entries (link_list) result (flag)
    class(internal_link_list_t), intent(in) :: link_list
    logical :: flag
    flag = link_list%length > 0
  end function internal_link_list_has_entries

  function internal_link_list_get_length (link_list) result (length)
    class(internal_link_list_t), intent(in) :: link_list
    integer :: length
    length = link_list%length
  end function internal_link_list_get_length

  function internal_link_list_get_link (link_list, i) result (link)
    class(internal_link_list_t), intent(in) :: link_list
    integer, intent(in) :: i
    integer :: link
    if (i <= link_list%length) then
       link = link_list%link(i)
    else
       call msg_bug ("Internal link list: out of bounds")
    end if
  end function internal_link_list_get_link

  subroutine interaction_init &
       (int, n_in, n_vir, n_out, &
        tag, resonant, mask, hel_lock, set_relations, store_values)
    class(interaction_t), intent(out) :: int
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
    call int%state_matrix%init (store_values)
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
             call int%relate (i, n_in + j)
          end do
       end do
    end if
  end subroutine interaction_init

  subroutine interaction_set_tag (int, tag)
    type(interaction_t), intent(inout), optional :: int
    integer, intent(in), optional :: tag
    integer, save :: stored_tag = 1
    if (present (int)) then
       if (present (tag)) then
          int%tag = tag
       else
          int%tag = stored_tag
          stored_tag = stored_tag + 1
       end if
    else if (present (tag)) then
       stored_tag = tag
    else
       stored_tag = 1
    end if
  end subroutine interaction_set_tag

  subroutine reset_interaction_counter (tag)
    integer, intent(in), optional :: tag
    call interaction_set_tag (tag=tag)
  end subroutine reset_interaction_counter

  subroutine interaction_final (object)
    class(interaction_t), intent(inout) :: object
    call object%state_matrix%final ()
  end subroutine interaction_final

  subroutine interaction_write &
       (int, unit, verbose, show_momentum_sum, show_mass, show_state, &
       col_verbose, testflag)
    class(interaction_t), intent(in) :: int
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    logical, intent(in), optional :: show_state, col_verbose, testflag
    integer :: u
    integer :: i, index_link
    type(interaction_t), pointer :: int_link
    logical :: show_st
    u = given_output_unit (unit);  if (u < 0)  return
    show_st = .true.;  if (present (show_state))  show_st = show_state
    if (int%tag /= 0) then
       write (u, "(1x,A,I0)")  "Interaction: ", int%tag
       do i = 1, int%n_tot
          if (i == 1 .and. int%n_in > 0) then
             write (u, "(1x,A)") "Incoming:"
          else if (i == int%n_in + 1 .and. int%n_vir > 0) then
             write (u, "(1x,A)") "Virtual:"
          else if (i == int%n_in + int%n_vir + 1 .and. int%n_out > 0) then
             write (u, "(1x,A)") "Outgoing:"
          end if
          write (u, "(1x,A,1x,I0)", advance="no") "Particle", i
          if (allocated (int%resonant)) then
             if (int%resonant(i)) then
                write (u, "(A)") "[r]"
             else
                write (u, *)
             end if
          else
             write (u, *)
          end if
          if (allocated (int%p)) then
             if (int%p_is_known(i)) then
                call vector4_write (int%p(i), u, show_mass, testflag)
             else
                write (u, "(A)")  "  [momentum undefined]"
             end if
          else
             write (u, "(A)") "  [momentum not allocated]"
          end if
          if (allocated (int%mask)) then
             write (u, "(1x,A)", advance="no")  "mask [fch] = "
             call int%mask(i)%write (u)
             write (u, *)
          end if
          if (int%parents(i)%has_entries () &
               .or. int%children(i)%has_entries ()) then
             write (u, "(1x,A)", advance="no") "internal links:"
             call int%parents(i)%write (u)
             if (int%parents(i)%has_entries ()) &
                  write (u, "(1x,A)", advance="no") "=>"
             write (u, "(1x,A)", advance="no") "X"
             if (int%children(i)%has_entries ()) &
                  write (u, "(1x,A)", advance="no") "=>"
             call int%children(i)%write (u)
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
             write (u, "(1x,A)") "Incoming particles (sum):"
             call vector4_write &
                  (sum (int%p(1 : int%n_in)), u, show_mass = show_mass)
             write (u, "(1x,A)") "Outgoing particles (sum):"
             call vector4_write &
                  (sum (int%p(int%n_in + int%n_vir + 1 : )), &
                   u, show_mass = show_mass)
             write (u, *)
          end if
       end if
       if (show_st) then
          call int%write_state_matrix (write_value_list = verbose, &
             verbose = verbose, unit = unit, col_verbose = col_verbose, &
             testflag = testflag)
       end if
    else
       write (u, "(1x,A)") "Interaction: [empty]"
    end if
  end subroutine interaction_write

  subroutine interaction_write_state_matrix (int, unit, write_value_list, &
     verbose, col_verbose, testflag)
    class(interaction_t), intent(in) :: int
    logical, intent(in), optional :: write_value_list, verbose, col_verbose
    logical, intent(in), optional :: testflag
    integer, intent(in), optional :: unit
    call int%state_matrix%write (write_value_list = verbose, &
       verbose = verbose, unit = unit, col_verbose = col_verbose, &
       testflag = testflag)
  end subroutine interaction_write_state_matrix

  subroutine interaction_reduce_state_matrix (int, qn_mask)
    class(interaction_t), intent(inout) :: int
    type(quantum_numbers_mask_t), intent(in), dimension(:) :: qn_mask
    type(state_matrix_t) :: state
    call int%state_matrix%reduce (qn_mask, state)
    int%state_matrix = state
  end subroutine interaction_reduce_state_matrix

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
    class(interaction_t), intent(inout) :: int
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    integer, intent(in), optional :: index
    complex(default), intent(in), optional :: value
    logical, intent(in), optional :: sum_values
    integer, intent(in), optional :: counter_index
    integer, intent(out), optional :: me_index
    type(quantum_numbers_t), dimension(size(qn)) :: qn_tmp
    qn_tmp = qn
    call qn_tmp%undefine (int%mask)
    call int%state_matrix%add_state (qn_tmp, index, value, sum_values, &
         counter_index, me_index)
    int%update_values = .true.
  end subroutine interaction_add_state

  subroutine interaction_freeze (int)
    class(interaction_t), intent(inout) :: int
    if (int%update_state_matrix) then
       call int%state_matrix%collapse (int%mask)
       int%update_state_matrix = .false.
       int%update_values = .true.
    end if
    if (int%update_values) then
       call int%state_matrix%freeze ()
       int%update_values = .false.
    end if
  end subroutine interaction_freeze

  pure function interaction_is_empty (int) result (flag)
    logical :: flag
    class(interaction_t), intent(in) :: int
    flag = int%state_matrix%is_empty ()
  end function interaction_is_empty

  pure function interaction_get_n_matrix_elements (int) result (n)
    integer :: n
    class(interaction_t), intent(in) :: int
    n = int%state_matrix%get_n_matrix_elements ()
  end function interaction_get_n_matrix_elements

  function interaction_get_n_in_helicities (int) result (n_hel)
    integer :: n_hel
    class(interaction_t), intent(in) :: int
    type(interaction_t) :: int_copy
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    integer :: i
    allocate (qn_mask (int%n_tot))
    do i = 1, int%n_tot
       if (i <= int%n_in) then
          call qn_mask(i)%init (.true., .true., .false.)
       else
          call qn_mask(i)%init (.true., .true., .true.)
       end if
    end do
    int_copy = int
    call int_copy%set_mask (qn_mask)
    call int_copy%freeze ()
    allocate (qn (int_copy%state_matrix%get_n_matrix_elements (), &
         int_copy%state_matrix%get_depth ()))
    qn = int_copy%get_quantum_numbers ()
    n_hel = 0
    do i = 1, size (qn, dim=1)
       if (all (qn(i,:)%get_subtraction_index () == 0)) n_hel = n_hel + 1
    end do
    call int_copy%final ()
    deallocate (qn_mask)
    deallocate (qn)
  end function interaction_get_n_in_helicities

  pure function interaction_get_me_size (int) result (n)
    integer :: n
    class(interaction_t), intent(in) :: int
    n = int%state_matrix%get_me_size ()
  end function interaction_get_me_size

  pure function interaction_get_norm (int) result (norm)
    real(default) :: norm
    class(interaction_t), intent(in) :: int
    norm = int%state_matrix%get_norm ()
  end function interaction_get_norm

  pure function interaction_get_n_sub (int) result (n_sub)
    integer :: n_sub
    class(interaction_t), intent(in) :: int
    n_sub = int%state_matrix%get_n_sub ()
  end function interaction_get_n_sub

  function interaction_get_quantum_numbers_single (int, i) result (qn)
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    class(interaction_t), intent(in), target :: int
    integer, intent(in) :: i
    allocate (qn (int%state_matrix%get_depth ()))
    qn = int%state_matrix%get_quantum_number (i)
  end function interaction_get_quantum_numbers_single

  function interaction_get_quantum_numbers_all (int) result (qn)
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    class(interaction_t), intent(in), target :: int
    integer :: i
    allocate (qn (int%state_matrix%get_n_matrix_elements (), &
       int%state_matrix%get_depth()))
    do i = 1, int%state_matrix%get_n_matrix_elements ()
       qn (i, :) = int%state_matrix%get_quantum_number (i)
    end do
  end function interaction_get_quantum_numbers_all

  function interaction_get_quantum_numbers_all_qn_mask (int, qn_mask) &
     result (qn)
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    class(interaction_t), intent(in) :: int
    type(quantum_numbers_mask_t), intent(in) :: qn_mask
    integer :: n_redundant, n_all, n_me
    integer :: i
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn_all
    call int%state_matrix%get_quantum_numbers (qn_all)
    n_redundant = count (qn_all%are_redundant (qn_mask))
    n_all = size (qn_all)
    !!! Number of matrix elements = survivors / n_particles
    n_me = (n_all - n_redundant) / int%state_matrix%get_depth ()
    allocate (qn (n_me, int%state_matrix%get_depth()))
    do i = 1, n_me
       if (.not. any (qn_all(i, :)%are_redundant (qn_mask))) &
          qn (i, :) = qn_all (i, :)
    end do
  end function interaction_get_quantum_numbers_all_qn_mask

  subroutine interaction_get_quantum_numbers_all_sub (int, qn)
    class(interaction_t), intent(in) :: int
    type(quantum_numbers_t), dimension(:,:), allocatable, intent(out) :: qn
    integer :: i
    allocate (qn (int%state_matrix%get_n_matrix_elements (), &
       int%state_matrix%get_depth()))
    do i = 1, int%state_matrix%get_n_matrix_elements ()
       qn (i, :) = int%state_matrix%get_quantum_number (i)
    end do
  end subroutine interaction_get_quantum_numbers_all_sub

  subroutine interaction_get_quantum_numbers_mask (int, qn_mask, qn)
    class(interaction_t), intent(in) :: int
    type(quantum_numbers_mask_t), intent(in) :: qn_mask
    type(quantum_numbers_t), dimension(:,:), allocatable, intent(out) :: qn
    integer :: n_redundant, n_all, n_me
    integer :: i
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn_all
    call int%state_matrix%get_quantum_numbers (qn_all)
    n_redundant = count (qn_all%are_redundant (qn_mask))
    n_all = size (qn_all)
    !!! Number of matrix elements = survivors / n_particles
    n_me = (n_all - n_redundant) / int%state_matrix%get_depth ()
    allocate (qn (n_me, int%state_matrix%get_depth()))
    do i = 1, n_me
       if (.not. any (qn_all(i, :)%are_redundant (qn_mask))) &
          qn (i, :) = qn_all (i, :)
    end do
  end subroutine interaction_get_quantum_numbers_mask

  elemental function interaction_get_matrix_element_single (int, i) result (me)
    complex(default) :: me
    class(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    me = int%state_matrix%get_matrix_element (i)
  end function interaction_get_matrix_element_single

  function interaction_get_matrix_element_array (int) result (me)
    complex(default), dimension(:), allocatable :: me
    class(interaction_t), intent(in) :: int
    allocate (me (int%get_n_matrix_elements ()))
    me = int%state_matrix%get_matrix_element ()
  end function interaction_get_matrix_element_array

  subroutine interaction_set_matrix_element_qn (int, qn, val)
    class(interaction_t), intent(inout) :: int
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    complex(default), intent(in) :: val
    call int%state_matrix%set_matrix_element (qn, val)
  end subroutine interaction_set_matrix_element_qn

  subroutine interaction_set_matrix_element_all (int, value)
    class(interaction_t), intent(inout) :: int
    complex(default), intent(in) :: value
    call int%state_matrix%set_matrix_element (value)
  end subroutine interaction_set_matrix_element_all

  subroutine interaction_set_matrix_element_array (int, value)
    class(interaction_t), intent(inout) :: int
    complex(default), dimension(:), intent(in) :: value
    call int%state_matrix%set_matrix_element (value)
  end subroutine interaction_set_matrix_element_array

  pure subroutine interaction_set_matrix_element_single (int, i, value)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    complex(default), intent(in) :: value
    call int%state_matrix%set_matrix_element (i, value)
  end subroutine interaction_set_matrix_element_single

  subroutine interaction_set_matrix_element_clone (int, int1)
    class(interaction_t), intent(inout) :: int
    class(interaction_t), intent(in) :: int1
    call int%state_matrix%set_matrix_element (int1%state_matrix)
  end subroutine interaction_set_matrix_element_clone

  subroutine interaction_set_only_matrix_element (int, i, value)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    complex(default), intent(in) :: value
    call int%set_matrix_element (cmplx (0, 0, default))
    call int%set_matrix_element (i, value)
  end subroutine interaction_set_only_matrix_element

  subroutine interaction_add_to_matrix_element (int, qn, value, match_only_flavor)
    class(interaction_t), intent(inout) :: int
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    complex(default), intent(in) :: value
    logical, intent(in), optional :: match_only_flavor
    call int%state_matrix%add_to_matrix_element (qn, value, match_only_flavor)
  end subroutine interaction_add_to_matrix_element

  subroutine interaction_get_diagonal_entries (int, i)
    class(interaction_t), intent(in) :: int
    integer, dimension(:), allocatable, intent(out) :: i
    call int%state_matrix%get_diagonal_entries (i)
  end subroutine interaction_get_diagonal_entries

  subroutine interaction_normalize_by_trace (int)
    class(interaction_t), intent(inout) :: int
    call int%state_matrix%normalize_by_trace ()
  end subroutine interaction_normalize_by_trace

  subroutine interaction_normalize_by_max (int)
    class(interaction_t), intent(inout) :: int
    call int%state_matrix%normalize_by_max ()
  end subroutine interaction_normalize_by_max

  subroutine interaction_set_norm (int, norm)
    class(interaction_t), intent(inout) :: int
    real(default), intent(in) :: norm
    call int%state_matrix%set_norm (norm)
  end subroutine interaction_set_norm

  subroutine interaction_set_state_matrix (int, state)
    class(interaction_t), intent(inout) :: int
    type(state_matrix_t), intent(in) :: state
    int%state_matrix = state
  end subroutine interaction_set_state_matrix

  function interaction_get_max_color_value (int) result (cmax)
    class(interaction_t), intent(in) :: int
    integer :: cmax
    cmax = int%state_matrix%get_max_color_value ()
  end function interaction_get_max_color_value

  subroutine interaction_factorize &
       (int, mode, x, ok, single_state, correlated_state, qn_in)
    class(interaction_t), intent(in), target :: int
    integer, intent(in) :: mode
    real(default), intent(in) :: x
    logical, intent(out) :: ok
    type(state_matrix_t), &
         dimension(:), allocatable, intent(out) :: single_state
    type(state_matrix_t), intent(out), optional :: correlated_state
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_in
    call int%state_matrix%factorize &
         (mode, x, ok, single_state, correlated_state, qn_in)
  end subroutine interaction_factorize

  function interaction_sum (int) result (value)
    class(interaction_t), intent(in) :: int
    complex(default) :: value
    value = int%state_matrix%sum ()
  end function interaction_sum

  subroutine interaction_add_color_contractions (int)
    class(interaction_t), intent(inout) :: int
    call int%state_matrix%add_color_contractions ()
  end subroutine interaction_add_color_contractions

  pure subroutine interaction_evaluate_product &
       (int, i, int1, int2, index1, index2)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1, int2
    integer, dimension(:), intent(in) :: index1, index2
    call int%state_matrix%evaluate_product &
         (i, int1%state_matrix, int2%state_matrix, &
          index1, index2)
  end subroutine interaction_evaluate_product

  pure subroutine interaction_evaluate_product_cf &
       (int, i, int1, int2, index1, index2, factor)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1, int2
    integer, dimension(:), intent(in) :: index1, index2
    complex(default), dimension(:), intent(in) :: factor
    call int%state_matrix%evaluate_product_cf &
         (i, int1%state_matrix, int2%state_matrix, &
          index1, index2, factor)
  end subroutine interaction_evaluate_product_cf

  pure subroutine interaction_evaluate_square_c (int, i, int1, index1)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1
    integer, dimension(:), intent(in) :: index1
    call int%state_matrix%evaluate_square_c (i, int1%state_matrix, index1)
  end subroutine interaction_evaluate_square_c

  pure subroutine interaction_evaluate_sum (int, i, int1, index1)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1
    integer, dimension(:), intent(in) :: index1
    call int%state_matrix%evaluate_sum (i, int1%state_matrix, index1)
  end subroutine interaction_evaluate_sum

  pure subroutine interaction_evaluate_me_sum (int, i, int1, index1)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(in) :: int1
    integer, dimension(:), intent(in) :: index1
    call int%state_matrix%evaluate_me_sum (i, int1%state_matrix, index1)
  end subroutine interaction_evaluate_me_sum

  function interaction_get_tag (int) result (tag)
    class(interaction_t), intent(in) :: int
    integer :: tag
    tag = int%tag
  end function interaction_get_tag

  pure function interaction_get_n_tot (object) result (n_tot)
    class(interaction_t), intent(in) :: object
    integer :: n_tot
    n_tot = object%n_tot
  end function interaction_get_n_tot

  pure function interaction_get_n_in (object) result (n_in)
    class(interaction_t), intent(in) :: object
    integer :: n_in
    n_in = object%n_in
  end function interaction_get_n_in

  pure function interaction_get_n_vir (object) result (n_vir)
    class(interaction_t), intent(in) :: object
    integer :: n_vir
    n_vir = object%n_vir
  end function interaction_get_n_vir

  pure function interaction_get_n_out (object) result (n_out)
    class(interaction_t), intent(in) :: object
    integer :: n_out
    n_out = object%n_out
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
       call int%basic_write ()
       print *, i, in, vir, out
       call msg_bug (" Momentum index is out of range for this interaction")
    end if
  end function idx

  function interaction_get_momenta_all (int, outgoing) result (p)
    class(interaction_t), intent(in) :: int
    type(vector4_t), dimension(:), allocatable :: p
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
    class(interaction_t), intent(in) :: int
    type(vector4_t), dimension(:), allocatable :: p
    integer, dimension(:), intent(in) :: jj
    allocate (p (size (jj)))
    p = int%p(jj)
  end function interaction_get_momenta_idx

  function interaction_get_momentum (int, i, outgoing) result (p)
    class(interaction_t), intent(in) :: int
    type(vector4_t) :: p
    integer, intent(in) :: i
    logical, intent(in), optional :: outgoing
    p = int%p(idx (int, i, outgoing))
  end function interaction_get_momentum

  subroutine interaction_get_momenta_sub (int, p, outgoing)
    class(interaction_t), intent(in) :: int
    type(vector4_t), dimension(:), intent(out) :: p
    logical, intent(in), optional :: outgoing
    integer :: i
    do i = 1, size (p)
       p(i) = int%p(idx (int, i, outgoing))
    end do
  end subroutine interaction_get_momenta_sub

  function interaction_get_state_matrix_ptr (int) result (state)
    class(interaction_t), intent(in), target :: int
    type(state_matrix_t), pointer :: state
    state => int%state_matrix
  end function interaction_get_state_matrix_ptr

  function interaction_get_resonance_flags (int) result (resonant)
    class(interaction_t), intent(in) :: int
    logical, dimension(size(int%resonant)) :: resonant
    resonant = int%resonant
  end function interaction_get_resonance_flags

  function interaction_get_mask_all (int) result (mask)
    class(interaction_t), intent(in) :: int
    type(quantum_numbers_mask_t), dimension(size(int%mask)) :: mask
    mask = int%mask
  end function interaction_get_mask_all

  function interaction_get_mask_slice (int, index) result (mask)
    class(interaction_t), intent(in) :: int
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
       s = sum (int%p(int%n_vir + 1 : )) ** 2
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
    call it%init (int%state_matrix)
    flv_array = it%get_flavor ()
    do i = int%n_in + int%n_vir + 1, int%n_tot
       if (.not. flv_array(i)%is_stable ()) then
          flv = flv_array(i)
          p = int%p(i)
          return
       end if
    end do
  end subroutine interaction_get_unstable_particle

  subroutine interaction_get_flv_out (int, flv)
    type(interaction_t), intent(in), target :: int
    type(flavor_t), dimension(:,:), allocatable, intent(out) :: flv
    type(state_iterator_t) :: it
    type(flavor_t), dimension(:), allocatable :: flv_state
    integer :: n_in, n_vir, n_out, n_tot, n_state, i
    n_in = int%get_n_in ()
    n_vir = int%get_n_vir ()
    n_out = int%get_n_out ()
    n_tot = int%get_n_tot ()
    n_state = int%get_n_matrix_elements ()
    allocate (flv (n_out, n_state))
    allocate (flv_state (n_tot))
    i = 1
    call it%init (int%get_state_matrix_ptr ())
    do while (it%is_valid ())
       flv_state = it%get_flavor ()
       flv(:,i) = flv_state(n_in + n_vir + 1 : )
       i = i + 1
       call it%advance ()
    end do
  end subroutine interaction_get_flv_out

  subroutine interaction_get_flv_content (int, state_flv, n_out_hard)
    type(interaction_t), intent(in), target :: int
    type(state_flv_content_t), intent(out) :: state_flv
    integer, intent(in) :: n_out_hard
    logical, dimension(:), allocatable :: mask
    integer :: n_tot
    n_tot = int%get_n_tot ()
    allocate (mask (n_tot), source = .false.)
    mask(n_tot-n_out_hard + 1 : ) = .true.
    call state_flv%fill (int%get_state_matrix_ptr (), mask)
  end subroutine interaction_get_flv_content

  subroutine interaction_set_mask (int, mask)
    class(interaction_t), intent(inout) :: int
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    if (size (int%mask) /= size (mask)) &
       call msg_fatal ("Attempting to set mask with unfitting size!")
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
          call mask_tmp%assign (mask, helicity=.true.)
          int%mask(int%hel_lock(ii)) = int%mask(int%hel_lock(ii)) .or. mask_tmp
       end if
    end if
    int%update_state_matrix = .true.
  end subroutine interaction_merge_mask_entry

  subroutine interaction_reset_momenta (int)
    class(interaction_t), intent(inout) :: int
    int%p = vector4_null
    int%p_is_known = .true.
  end subroutine interaction_reset_momenta

  subroutine interaction_set_momenta (int, p, outgoing)
    class(interaction_t), intent(inout) :: int
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
    class(interaction_t), intent(inout) :: int
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
       call int%set_matrix_element (value(1))
    else
       call it%init (int%state_matrix)
       do while (it%is_valid ())
          flv = it%get_flavor (pos)
          SCAN_FLV: do i = 1, size (value)
             if (flv == flv_in(i)) then
                call it%set_matrix_element (value(i))
                exit SCAN_FLV
             end if
          end do SCAN_FLV
          call it%advance ()
       end do
    end if
  end subroutine interaction_set_flavored_values

  subroutine interaction_relate (int, i1, i2)
    class(interaction_t), intent(inout), target :: int
    integer, intent(in) :: i1, i2
    if (i1 /= 0 .and. i2 /= 0) then
       call int%children(i1)%append (i2)
       call int%parents(i2)%append (i1)
    end if
  end subroutine interaction_relate

  subroutine interaction_transfer_relations (int1, int2, map)
    class(interaction_t), intent(in) :: int1
    class(interaction_t), intent(inout), target :: int2
    integer, dimension(:), intent(in) :: map
    integer :: i, j, k
    do i = 1, size (map)
       do j = 1, int1%parents(i)%get_length ()
          k = int1%parents(i)%get_link (j)
          call int2%relate (map(k), map(i))
       end do
       if (map(i) /= 0) then
          int2%resonant(map(i)) = int1%resonant(i)
       end if
    end do
  end subroutine interaction_transfer_relations

  subroutine interaction_relate_connections &
       (int, int_in, connection_index, &
        map, map_connections, resonant)
    class(interaction_t), intent(inout), target :: int
    class(interaction_t), intent(in) :: int_in
    integer, dimension(:), intent(in) :: connection_index
    integer, dimension(:), intent(in) :: map, map_connections
    logical, intent(in), optional :: resonant
    logical :: reson
    integer :: i, j, i2, k2
    reson = .false.;  if (present (resonant))  reson = resonant
    do i = 1, size (map_connections)
       k2 = connection_index(i)
       do j = 1, int_in%children(k2)%get_length ()
          i2 = int_in%children(k2)%get_link (j)
          call int%relate (map_connections(i), map(i2))
       end do
       int%resonant(map_connections(i)) = reson
    end do
  end subroutine interaction_relate_connections

  function interaction_get_n_children (int, i) result (n)
    integer :: n
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    n = int%children(i)%get_length ()
  end function interaction_get_n_children

  function interaction_get_n_parents (int, i) result (n)
    integer :: n
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    n = int%parents(i)%get_length ()
  end function interaction_get_n_parents

  function interaction_get_children (int, i) result (idx)
    integer, dimension(:), allocatable :: idx
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    integer :: k, l
    l = int%children(i)%get_length ()
    allocate (idx (l))
    do k = 1, l
       idx(k) = int%children(i)%get_link (k)
    end do
  end function interaction_get_children

  function interaction_get_parents (int, i) result (idx)
    integer, dimension(:), allocatable :: idx
    type(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    integer :: k, l
    l = int%parents(i)%get_length ()
    allocate (idx (l))
    do k = 1, l
       idx(k) = int%parents(i)%get_link (k)
    end do
  end function interaction_get_parents

  subroutine interaction_set_source_link (int, i, int1, i1)
    class(interaction_t), intent(inout) :: int
    integer, intent(in) :: i
    class(interaction_t), intent(in), target :: int1
    integer, intent(in) :: i1
    if (i /= 0)  call external_link_set (int%source(i), int1, i1)
  end subroutine interaction_set_source_link

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

  subroutine interaction_find_source (int, i, int1, i1)
    class(interaction_t), intent(in) :: int
    integer, intent(in) :: i
    type(interaction_t), intent(out), pointer :: int1
    integer, intent(out) :: i1
    type(external_link_t) :: link
    link = interaction_get_ultimate_source (int, i)
    int1 => external_link_get_ptr (link)
    i1 = external_link_get_index (link)
  end subroutine interaction_find_source

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
    call int%freeze ()
  end subroutine interaction_exchange_mask

  subroutine interaction_receive_momenta (int)
    class(interaction_t), intent(inout) :: int
    integer :: i, index_link
    type(interaction_t), pointer :: int_link
    do i = 1, int%n_tot
       if (external_link_is_set (int%source(i))) then
          int_link => external_link_get_ptr (int%source(i))
          index_link = external_link_get_index (int%source(i))
          call int%set_momentum (int_link%p(index_link), i)
       end if
    end do
  end subroutine interaction_receive_momenta

  subroutine interaction_send_momenta (int)
    type(interaction_t), intent(in) :: int
    integer :: i, index_link
    type(interaction_t), pointer :: int_link
    do i = 1, int%n_tot
       if (external_link_is_set (int%source(i))) then
          int_link => external_link_get_ptr (int%source(i))
          index_link = external_link_get_index (int%source(i))
          call int_link%set_momentum (int%p(i), index_link)
       end if
    end do
  end subroutine interaction_send_momenta

  subroutine interaction_pacify_momenta (int, acc)
    type(interaction_t), intent(inout) :: int
    real(default), intent(in) :: acc
    integer :: i
    do i = 1, int%n_tot
       call pacify (int%p(i), acc)
    end do
  end subroutine interaction_pacify_momenta

  subroutine interaction_declare_subtraction (int, n_sub)
    type(interaction_t), intent(inout), target :: int
    integer, intent(in) :: n_sub
    type(state_iterator_t) :: it
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, s
    integer :: n_me_orig
    complex(default), dimension(:), allocatable :: me_orig
    call it%init (int%state_matrix)
    i = 1; n_me_orig = int%state_matrix%get_n_matrix_elements ()
    allocate (me_orig (n_me_orig))
    allocate (qn (it%get_depth ()))
    do while (it%is_valid () .and. i <= n_me_orig)
       qn = it%get_quantum_numbers ()
       me_orig (i) = it%get_matrix_element ()
       do s = 1, n_sub
          call qn%set_subtraction_index (s)
          call int%state_matrix%add_state (qn)
       end do
       call it%advance ()
       i = i + 1
    end do
    call int%state_matrix%freeze()
    do i = 1, n_me_orig
       call int%state_matrix%set_matrix_element (i, me_orig(i))
       call int%state_matrix%set_matrix_element (i + n_me_orig, me_orig(i))
    end do
  end subroutine interaction_declare_subtraction

  subroutine find_connections (int1, int2, n, connection_index)
    class(interaction_t), intent(in) :: int1, int2
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


end module interactions
