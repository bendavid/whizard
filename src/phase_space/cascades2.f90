! WHIZARD 2.6.1 Nov 03 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module cascades2

  use kinds, only: default
  use kinds, only: TC, i8
  use sorting
  use flavors
  use model_data
  use iso_varying_string, string_t => varying_string
  use io_units
  use physics_defs, only: SCALAR, SPINOR, VECTOR, VECTORSPINOR, TENSOR
  use phs_forests, only: phs_parameters_t
  use diagnostics
  use hashes
  use cascades, only: phase_space_vanishes, MAX_WARN_RESONANCE
  use, intrinsic :: iso_fortran_env, only : input_unit, output_unit, error_unit
  use resonances, only: resonance_info_t
  use resonances, only: resonance_history_t
  use resonances, only: resonance_history_set_t
  !$ use :: omp_lib !NODEP!

  implicit none
  private

  public :: DECAY, SCATTERING
  public :: feyngraph_set_t
  public :: init_sm_full_test
  public :: feyngraph_set_write_file_format
  public :: feyngraph_set_generate_single
  public :: feyngraph_set_write_process_bincode_format
  public :: feyngraph_set_write_graph_format
  public :: feyngraph_set_generate
  public :: feyngraph_set_is_valid
  public :: grove_list_get_n_trees
  public :: feyngraph_set_get_resonance_histories

  integer, parameter :: LABEL_LEN=30
  integer, parameter :: &
       & NONRESONANT = -2, EXTERNAL_PRT = -1, &
       & NO_MAPPING = 0, S_CHANNEL = 1, T_CHANNEL =  2, U_CHANNEL = 3, &
       & RADIATION = 4, COLLINEAR = 5, INFRARED = 6, &
       & STEP_MAPPING_E = 11, STEP_MAPPING_H = 12, &
       & ON_SHELL = 99
  integer, parameter :: FEYNGRAPH_LEN=300
  integer, parameter :: DECAY=1, SCATTERING=2

  integer, parameter :: BUFFER_LEN = 1000
  integer, parameter :: STACK_SIZE = 100
  integer, parameter :: PRT_ARRAY_SIZE = 200
  integer, parameter :: DAG_STACK_SIZE = 1000
  integer, parameter :: EMPTY = -999

  type :: part_prop_t
     character (len=LABEL_LEN) :: particle_label
     integer :: pdg = 0
     real(default) :: mass = 0.
     real :: width = 0.
     integer :: spin_type = 0
     logical :: is_vector = .false.
     logical :: empty = .true.
     type (part_prop_t), pointer :: anti => null ()
     type (string_t) :: tex_name
   contains
   procedure :: final => part_prop_final
     procedure :: init => part_prop_init
  end type part_prop_t

  type :: grove_prop_t
     integer :: multiplicity = 0
     integer :: n_resonances = 0
     integer :: n_log_enhanced = 0
     integer :: n_off_shell = 0
     integer :: n_t_channel = 0
     integer :: res_hash = 0
  end type grove_prop_t
  
  type :: tree_t
     integer(TC), dimension(:), allocatable :: bc
     integer, dimension(:), allocatable :: pdg
     integer, dimension(:), allocatable :: mapping
     integer :: n_entries = 0
     logical :: keep = .true.
     logical :: empty = .true.
   contains
     procedure :: add_entry_from_numbers => tree_add_entry_from_numbers
     procedure :: add_entry_from_node => tree_add_entry_from_node
     generic :: add_entry =>  add_entry_from_numbers, add_entry_from_node
     procedure :: sort => tree_sort
  end type tree_t

  type, abstract :: graph_t
     integer :: index = 0
     integer :: n_nodes = 0
     logical :: keep = .true.
  end type graph_t

  type, extends (graph_t) :: feyngraph_t
     character (len=FEYNGRAPH_LEN) :: omega_feyngraph_output
     type (f_node_t), pointer :: root => null ()
     type (feyngraph_t), pointer :: next => null()
     type (kingraph_t), pointer :: kin_first => null ()
     type (kingraph_t), pointer :: kin_last => null ()
   contains
     procedure :: final => feyngraph_final
     procedure :: make_kingraphs => feyngraph_make_kingraphs
     procedure :: compute_mappings => feyngraph_compute_mappings
     procedure :: make_invertible => feyngraph_make_invertible
  end type feyngraph_t

  type :: feyngraph_ptr_t
     type (feyngraph_t), pointer :: graph => null ()
  end type feyngraph_ptr_t
  
  type, extends (graph_t) :: kingraph_t
     type (k_node_t), pointer :: root => null ()
     type (kingraph_t), pointer :: next => null()
     type (kingraph_t), pointer :: grove_next => null ()
     type (tree_t) :: tree
     type (grove_prop_t) :: grove_prop
     logical :: inverse = .false.
     contains
     procedure :: final => kingraph_final
     procedure :: write_file_format => kingraph_write_file_format
     procedure :: make_inverse_copy => kingraph_make_inverse_copy
     procedure :: assign_resonance_hash => kingraph_assign_resonance_hash
     procedure :: extract_resonance_history => kingraph_extract_resonance_history
  end type kingraph_t

  type :: kingraph_ptr_t
     type (kingraph_t), pointer :: graph => null ()
  end type kingraph_ptr_t
  
  type, abstract :: node_t
     type (part_prop_t), pointer :: particle => null ()
     logical :: incoming = .false.
     logical :: t_line = .false.
     integer :: index = 0
     logical :: keep = .true.
     integer :: n_subtree_nodes = 1
  end type node_t

  type, abstract :: list_t
     integer :: n_entries = 0
  end type list_t

  type :: k_node_entry_t
     type (k_node_t), pointer :: node => null ()
     type (k_node_entry_t), pointer :: next => null ()
     logical :: recycle = .false.
   contains
     procedure :: final => k_node_entry_final
     procedure :: write => k_node_entry_write
  end type k_node_entry_t

  type, extends (list_t) :: k_node_list_t
     type (k_node_entry_t), pointer :: first => null ()
     type (k_node_entry_t), pointer :: last => null ()
     integer :: n_recycle
     logical :: observer = .false.
   contains
     procedure :: final => k_node_list_final
     procedure :: add_entry => k_node_list_add_entry
     procedure :: add_pointer => k_node_list_add_pointer
     procedure :: check_subtree_equivalences => k_node_list_check_subtree_equivalences
     procedure :: get_nodes => k_node_list_get_nodes
  end type k_node_list_t

  type, extends (node_t) :: f_node_t
     type (f_node_t), pointer :: daughter1 => null ()
     type (f_node_t), pointer :: daughter2 => null ()
     character (len=LABEL_LEN) :: particle_label
     type (k_node_list_t) :: k_node_list
   contains
     procedure :: final => f_node_final
     procedure :: set_index => f_node_set_index
     procedure :: assign_particle_properties => f_node_assign_particle_properties
  end type f_node_t

  type :: f_node_ptr_t
     type (f_node_t), pointer :: node => null ()
  end type f_node_ptr_t

  type :: k_node_ptr_t
     type (k_node_t), pointer :: node => null ()
  end type k_node_ptr_t

  type, extends (node_t) :: k_node_t
     type (k_node_t), pointer :: daughter1 => null ()
     type (k_node_t), pointer :: daughter2 => null ()
     type (k_node_t), pointer :: inverse_daughter1 => null ()
     type (k_node_t), pointer :: inverse_daughter2 => null ()
     type (f_node_t), pointer :: f_node => null ()
     type (tree_t) :: subtree
     real (default) :: ext_mass_sum = 0.
     real (default) :: effective_mass = 0.
     logical :: resonant = .false.
     logical :: on_shell = .false.
     logical :: log_enhanced = .false.
     integer :: mapping = NO_MAPPING
     integer(TC) :: bincode = 0
     logical :: mapping_assigned = .false.
     logical :: is_nonresonant_copy = .false.
     logical :: subtree_checked = .false.
     integer :: n_off_shell = 0
     integer :: n_log_enhanced = 0
     integer :: n_resonances = 0
     integer :: multiplicity = 0
     integer :: n_t_channel = 0
     integer :: f_node_index = 0
   contains
     procedure :: final => k_node_final
     procedure :: set_index => k_node_set_index
  end type k_node_t

  type :: f_node_entry_t
     character (len=FEYNGRAPH_LEN) :: subtree_string
     integer :: string_len = 0
     type (f_node_t), pointer :: node => null ()
     type (f_node_entry_t), pointer :: next => null ()
   contains
     procedure :: final => f_node_entry_final
     procedure :: write => f_node_entry_write
  end type f_node_entry_t

  type, extends (list_t) :: f_node_list_t
     type (f_node_entry_t), pointer :: first => null ()
     type (f_node_entry_t), pointer :: last => null ()
     type (k_node_list_t), pointer :: k_node_list => null ()
     integer :: max_tree_size = 0
   contains
     procedure :: add_entry => f_node_list_add_entry
     procedure :: write => f_node_list_write
     procedure :: final => f_node_list_final
  end type f_node_list_t

  type :: grove_t
     type (grove_prop_t) :: grove_prop
     type (grove_t), pointer :: next => null ()
     type (kingraph_t), pointer :: first => null ()
     !$ integer (OMP_lock_kind) :: lock
   contains
     procedure :: final => grove_final
     procedure :: write_file_format => grove_write_file_format
  end type grove_t

  type :: grove_ptr_t
     type (grove_t), pointer :: grove => null ()
  end type grove_ptr_t
  
  type :: grove_list_t
     type (grove_t), pointer :: first => null ()
     !$ integer (OMP_lock_kind) :: lock
   contains
     procedure :: final => grove_list_final
     procedure :: get_grove => grove_list_get_grove
     procedure :: add_kingraph => grove_list_add_kingraph
     procedure :: add_feyngraph => grove_list_add_feyngraph
     procedure :: rebuild => grove_list_rebuild
  end type grove_list_t

  type :: feyngraph_set_t
     type (model_data_t), pointer :: model => null ()
     type(flavor_t), dimension(:,:), allocatable :: flv
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: process_type = DECAY
     type (phs_parameters_t) :: phs_par
     logical :: fatal_beam_decay = .true.
     type (part_prop_t), dimension (:), pointer :: particle => null ()
     type (f_node_list_t) :: f_node_list
     type (feyngraph_t), pointer :: first => null ()
     type (feyngraph_t), pointer :: last => null ()
     integer :: n_graphs = 0
     type (grove_list_t), pointer :: grove_list => null ()
     logical :: use_dag = .true.
     type (feyngraph_set_t), dimension (:), pointer :: fset => null ()
   contains
     procedure :: final => feyngraph_set_final
     procedure :: build => feyngraph_set_build
  end type feyngraph_set_t

  type :: dag_node_t
     integer :: string_len
     type (string_t) :: string
     logical :: leaf = .false.
     type (f_node_ptr_t), dimension (:), allocatable :: f_node
   contains
       procedure :: make_f_nodes => dag_node_make_f_nodes
  end type dag_node_t

  type :: dag_options_t
     integer :: string_len
     type (string_t) :: string
   contains
       procedure :: make_f_nodes_single => dag_options_make_f_nodes_single
       procedure :: make_f_nodes_pair => dag_options_make_f_nodes_pair
       generic :: make_f_nodes => make_f_nodes_single, make_f_nodes_pair
  end type dag_options_t
  
  type :: dag_combination_t
     integer :: string_len
     type (string_t) :: string
     integer, dimension (2) :: combination
   contains
       procedure :: make_f_nodes => dag_combination_make_f_nodes
  end type dag_combination_t
  
  type :: dag_t
     type (string_t) :: string
     type (dag_node_t), dimension (:), allocatable :: node
     type (dag_options_t), dimension (:), allocatable :: options
     type (dag_combination_t), dimension (:), allocatable :: combination
     integer :: n_nodes = 0
     integer :: n_options = 0
     integer :: n_combinations = 0
     contains
       procedure :: read_string => dag_read_string
       procedure :: construct => dag_construct
       procedure :: get_nodes_and_combinations => dag_get_nodes_and_combinations
       procedure :: get_options => dag_get_options
       procedure :: add_node => dag_add_node 
       procedure :: add_options => dag_add_options
       procedure :: add_combination => dag_add_combination
       procedure :: make_feyngraphs => dag_make_feyngraphs
       procedure :: write => dag_write
  end type dag_t
  

  interface assignment (=)
     module procedure tree_assign
  end interface assignment (=)
  
  interface assignment (=)
     module procedure f_node_ptr_assign
  end interface assignment (=)
  
  interface assignment (=)
     module procedure k_node_assign
  end interface assignment (=)
  
  interface assignment (=)
     module procedure f_node_entry_assign
  end interface assignment (=)
  
  interface assignment (=)
     module procedure k_node_entry_assign
  end interface assignment (=)

  interface operator (.match.)
     module procedure grove_prop_match
  end interface operator (.match.)
  interface operator (==)
     module procedure grove_prop_equal
  end interface operator (==)
  
  interface operator (==)
     module procedure tree_equal
  end interface operator (==)
  
  interface operator (.eqv.)
     module procedure subtree_eqv
  end interface operator (.eqv.)
  

contains

  subroutine part_prop_final (part)
    class(part_prop_t), intent(inout) :: part
    part%anti => null ()
  end subroutine part_prop_final
  
  subroutine tree_assign (tree1, tree2)
    type (tree_t), intent (inout) :: tree1
    type (tree_t), intent (in) :: tree2
    if (allocated (tree2%bc)) then
       allocate (tree1%bc(size(tree2%bc)))
       tree1%bc = tree2%bc
    endif
    if (allocated (tree2%pdg)) then
       allocate (tree1%pdg(size(tree2%pdg)))
       tree1%pdg = tree2%pdg
    endif
    if (allocated (tree2%mapping)) then
       allocate (tree1%mapping(size(tree2%mapping)))
       tree1%mapping = tree2%mapping
    endif
    tree1%n_entries = tree2%n_entries
    tree1%keep = tree2%keep
    tree1%empty = tree2%empty
  end subroutine tree_assign

  subroutine tree_add_entry_from_numbers (tree, bincode, pdg, mapping)
    class (tree_t), intent (inout) :: tree
    integer(TC), intent (in) :: bincode
    integer, intent (in) :: pdg
    integer, intent (in) :: mapping
    integer :: pos
    if (tree%empty) then
       allocate (tree%bc(1))
       allocate (tree%pdg(1))
       allocate (tree%mapping(1))
       pos = tree%n_entries + 1
       tree%bc(pos) = bincode
       tree%pdg(pos) = pdg
       tree%mapping(pos) = mapping
       tree%n_entries = pos
       tree%empty = .false.
    endif
  end subroutine tree_add_entry_from_numbers

  subroutine tree_merge (tree, tree1, tree2, bc, pdg, mapping)
    class (tree_t), intent (inout) :: tree
    type (tree_t), intent (in) :: tree1, tree2
    integer(TC), intent (in) :: bc
    integer, intent (in) :: pdg, mapping
    integer :: tree_size
    integer :: i1, i2
    if (tree%empty) then
       i1 = tree1%n_entries
       i2 = tree1%n_entries + tree2%n_entries
       tree_size = tree1%n_entries + tree2%n_entries + 1
       allocate (tree%bc (tree_size))
       allocate (tree%pdg (tree_size))
       allocate (tree%mapping (tree_size))
       tree%bc(:i1) = tree1%bc
       tree%pdg(:i1) = tree1%pdg
       tree%mapping(:i1) = tree1%mapping
       tree%bc(i1+1:i2) = tree2%bc
       tree%pdg(i1+1:i2) = tree2%pdg
       tree%mapping(i1+1:i2) = tree2%mapping
       tree%bc(tree_size) = bc
       tree%pdg(tree_size) = pdg
       tree%mapping(tree_size) = mapping
       tree%n_entries = tree_size
       tree%empty = .false.
    endif
  end subroutine tree_merge

  subroutine tree_add_entry_from_node (tree, node)
    class (tree_t), intent (inout) :: tree
    type (k_node_t), intent (in) :: node
    integer :: pdg
    if (node%t_line) then
       pdg = abs (node%particle%pdg)
    else
       pdg = node%particle%pdg
    endif
    if (associated (node%daughter1) .and. &
         associated (node%daughter2)) then
       call tree_merge (tree, node%daughter1%subtree, &
            node%daughter2%subtree, node%bincode, &
            node%particle%pdg, node%mapping)
    else
       call tree_add_entry_from_numbers (tree, node%bincode, &
            node%particle%pdg, node%mapping)
    endif
    call tree%sort ()
  end subroutine tree_add_entry_from_node

  subroutine tree_sort (tree)
    class (tree_t), intent (inout) :: tree
    integer(TC), dimension(size(tree%bc)) :: bc_tmp
    integer, dimension(size(tree%pdg)) :: pdg_tmp, mapping_tmp
    integer, dimension(1) :: pos
    integer :: i
    bc_tmp = tree%bc
    pdg_tmp = tree%pdg
    mapping_tmp = tree%mapping
    do i = size(tree%bc),1,-1
       pos = maxloc (bc_tmp)
       tree%bc(i) = bc_tmp (pos(1))
       tree%pdg(i) = pdg_tmp (pos(1))
       tree%mapping(i) = mapping_tmp (pos(1))
       bc_tmp(pos(1)) = 0
    end do
  end subroutine tree_sort

  subroutine feyngraph_final (graph)
    class(feyngraph_t), intent(inout) :: graph
    type (kingraph_t), pointer :: current
    graph%root => null ()
    graph%kin_last => null ()
    do while (associated (graph%kin_first))
       current => graph%kin_first
       graph%kin_first => graph%kin_first%next
       call current%final ()
       deallocate (current)
    enddo
  end subroutine feyngraph_final

  subroutine kingraph_final (graph)
    class(kingraph_t), intent(inout) :: graph
    graph%root => null ()
    graph%next => null ()
    graph%grove_next => null ()
  end subroutine kingraph_final

  subroutine k_node_entry_final (entry)
    class(k_node_entry_t), intent(inout) :: entry
    if (associated (entry%node)) then
       call entry%node%final
       deallocate (entry%node)
    end if
    entry%next => null ()
  end subroutine k_node_entry_final

  subroutine k_node_entry_write (k_node_entry, u)
    class (k_node_entry_t), intent (in) :: k_node_entry
    integer, intent (in) :: u
  end subroutine k_node_entry_write

  subroutine k_node_list_final (list)
    class(k_node_list_t), intent(inout) :: list
    type (k_node_entry_t), pointer :: current
    do while (associated (list%first))
       current => list%first
       list%first => list%first%next
       if (list%observer) current%node => null ()
       call current%final ()
       deallocate (current)
    enddo
  end subroutine k_node_list_final

  recursive subroutine f_node_final (node)
    class(f_node_t), intent(inout) :: node
    call node%k_node_list%final ()
    node%daughter1 => null ()
    node%daughter2 => null ()
  end subroutine f_node_final

  subroutine f_node_entry_final (entry)
    class(f_node_entry_t), intent(inout) :: entry
    if (associated (entry%node)) then
       call entry%node%final ()
       deallocate (entry%node)
    end if
    entry%next => null ()
  end subroutine f_node_entry_final

  subroutine f_node_set_index (f_node)
    class (f_node_t), intent (inout) :: f_node
    integer, save :: counter = 0
    if (f_node%index == 0) then
       counter = counter + 1
       f_node%index = counter
    endif
  end subroutine f_node_set_index

  subroutine f_node_ptr_assign (ptr1, ptr2)
    type (f_node_ptr_t), intent (inout) :: ptr1
    type (f_node_ptr_t), intent (in) :: ptr2
    ptr1%node => ptr2%node
  end subroutine f_node_ptr_assign

  subroutine k_node_assign (k_node1, k_node2)
    type (k_node_t), intent (inout) :: k_node1
    type (k_node_t), intent (in) :: k_node2
    k_node1%f_node => k_node2%f_node
    k_node1%particle => k_node2%particle
    k_node1%incoming = k_node2%incoming
    k_node1%t_line = k_node2%t_line
    k_node1%keep = k_node2%keep
    k_node1%n_subtree_nodes = k_node2%n_subtree_nodes
    k_node1%ext_mass_sum = k_node2%ext_mass_sum
    k_node1%effective_mass = k_node2%effective_mass
    k_node1%resonant = k_node2%resonant
    k_node1%on_shell = k_node2%on_shell
    k_node1%log_enhanced = k_node2%log_enhanced
    k_node1%mapping = k_node2%mapping
    k_node1%bincode = k_node2%bincode
    k_node1%mapping_assigned = k_node2%mapping_assigned
    k_node1%is_nonresonant_copy = k_node2%is_nonresonant_copy
    k_node1%n_off_shell = k_node2%n_off_shell
    k_node1%n_log_enhanced = k_node2%n_log_enhanced
    k_node1%n_resonances = k_node2%n_resonances
    k_node1%multiplicity = k_node2%multiplicity
    k_node1%n_t_channel = k_node2%n_t_channel
    k_node1%f_node_index = k_node2%f_node_index
  end subroutine k_node_assign

  recursive subroutine k_node_final (k_node)
    class(k_node_t), intent(inout) :: k_node
    k_node%daughter1 => null ()
    k_node%daughter2 => null ()
    k_node%inverse_daughter1 => null ()
    k_node%inverse_daughter2 => null ()
    k_node%f_node => null ()
  end subroutine k_node_final

  subroutine k_node_set_index (k_node)
    class (k_node_t), intent (inout) :: k_node
    integer, save :: counter = 0
    if (k_node%index == 0) then
       counter = counter + 1
       k_node%index = counter
    endif
  end subroutine k_node_set_index

  subroutine f_node_entry_write (f_node_entry, u)
    class (f_node_entry_t), intent (in) :: f_node_entry
    integer, intent (in) :: u
    write (unit=u, fmt='(A)') trim(f_node_entry%subtree_string)
  end subroutine f_node_entry_write

  subroutine f_node_entry_assign (entry1, entry2)
    type (f_node_entry_t), intent (out) :: entry1
    type (f_node_entry_t), intent (in) :: entry2
    entry1%node => entry2%node
    entry1%subtree_string = entry2%subtree_string
    entry1%string_len = entry2%string_len
  end subroutine f_node_entry_assign

  subroutine f_node_list_add_entry (list, subtree_string, ptr_to_node, recycle)
    class (f_node_list_t), intent (inout) :: list
    character (len=*), intent (in) :: subtree_string
    type (f_node_t), pointer, intent (out) :: ptr_to_node
    logical, intent (in) :: recycle
    type (f_node_entry_t), pointer :: current
    integer :: subtree_len
    ptr_to_node => null ()
    if (recycle) then
       subtree_len = len_trim (subtree_string)
       current => list%first
       do while (associated (current))
          if (current%string_len == subtree_len) then
             if (trim (current%subtree_string) == trim (subtree_string)) then
                ptr_to_node => current%node
                exit
             endif
          endif
          current => current%next
       enddo
    endif
    if (.not. associated (ptr_to_node)) then
       if (list%n_entries == 0) then
          allocate (list%first)
          list%last => list%first
       else
          allocate (list%last%next)
          list%last => list%last%next
       endif
       list%n_entries = list%n_entries + 1
       list%last%subtree_string = trim(subtree_string)
       list%last%string_len = subtree_len
       allocate (list%last%node)
       call list%last%node%set_index ()
       ptr_to_node => list%last%node
    end if
  end subroutine f_node_list_add_entry

  subroutine f_node_list_write (f_node_list, u)
    class (f_node_list_t), intent (in) :: f_node_list
    integer, intent (in) :: u
    type (f_node_entry_t), pointer :: current
    integer :: pos = 0
    current => f_node_list%first
    do while (associated (current))
       pos = pos + 1
       write (unit=u, fmt='(A,I10)') 'entry #: ', pos
       call current%write (u)
       write (unit=u, fmt=*)
       current => current%next
    enddo
  end subroutine f_node_list_write

  subroutine k_node_entry_assign (entry1, entry2)
    type (k_node_entry_t), intent (out) :: entry1
    type (k_node_entry_t), intent (in) :: entry2
    entry1%node => entry2%node
    entry1%recycle = entry2%recycle
  end subroutine k_node_entry_assign

  recursive subroutine k_node_list_add_entry (list, ptr_to_node, recycle)
    class (k_node_list_t), intent (inout) :: list
    type (k_node_t), pointer, intent (out) :: ptr_to_node
    logical, intent (in) :: recycle
    if (list%n_entries == 0) then
       allocate (list%first)
       list%last => list%first
    else
       allocate (list%last%next)
       list%last => list%last%next
    endif
    list%n_entries = list%n_entries + 1
    list%last%recycle = recycle
    allocate (list%last%node)
    call list%last%node%set_index ()
    ptr_to_node => list%last%node
  end subroutine k_node_list_add_entry

  subroutine k_node_list_add_pointer (list, ptr_to_node, recycle)
    class (k_node_list_t), intent (inout) :: list
    type (k_node_t), pointer, intent (in) :: ptr_to_node
    logical, optional, intent (in) :: recycle
    logical :: rec
    if (present (recycle)) then
       rec = recycle
    else
       rec = .false.
    endif
    if (list%n_entries == 0) then
       allocate (list%first)
       list%last => list%first
    else
       allocate (list%last%next)
       list%last => list%last%next
    endif
    list%n_entries = list%n_entries + 1
    list%last%recycle = rec
    list%last%node => ptr_to_node
  end subroutine k_node_list_add_pointer

  subroutine k_node_list_check_subtree_equivalences (list, model)
    class (k_node_list_t), intent (inout) :: list
    type (model_data_t), intent (in) :: model
    type (k_node_ptr_t), dimension (:), allocatable :: set
    type (k_node_entry_t), pointer :: current
    integer :: pos
    integer :: i,j
    if (list%n_entries == 0) return
    allocate (set (list%n_entries))
    current => list%first
    pos = 0
    do while (associated (current))
       pos = pos + 1
       set(pos)%node => current%node
       current => current%next
    enddo
    !$OMP PARALLEL DO PRIVATE(j)
    do i=1, list%n_entries
       if (set(i)%node%keep) then
          do j=i+1, list%n_entries
             if (set(j)%node%keep) then
                if (set(i)%node%bincode == set(j)%node%bincode) then
                   call subtree_select (set(i)%node%subtree,set(j)%node%subtree, model)
                   if (.not. set(i)%node%subtree%keep) then
                      !$OMP CRITICAL (deactivate_subtree)
                      set(i)%node%keep = .false.
                      !$OMP END CRITICAL (deactivate_subtree)
                      exit
                   else if (.not. set(j)%node%subtree%keep) then
                      !$OMP CRITICAL (deactivate_subtree)
                      set(j)%node%keep = .false.
                      !$OMP END CRITICAL (deactivate_subtree)
                   endif
                endif
             endif
          enddo
       endif
    enddo
    !$OMP END PARALLEL DO
    deallocate (set)
  end subroutine k_node_list_check_subtree_equivalences
  
  subroutine k_node_list_get_nodes (list, nodes)
    class (k_node_list_t), intent (inout) :: list
    type (k_node_ptr_t), dimension(:), allocatable, intent (out) :: nodes
    integer :: n_nodes
    integer :: pos
    type (k_node_entry_t), pointer :: current, garbage
    n_nodes = 0
    current => list%first
    do while (associated (current))
       if (current%recycle .and. current%node%keep) n_nodes = n_nodes + 1
       current => current%next
    enddo
    if (n_nodes /= 0) then
       pos = 1
       allocate (nodes (n_nodes))
       do while (associated (list%first) .and. .not. list%first%node%keep)
          garbage => list%first
          list%first => list%first%next
          call garbage%final ()
          deallocate (garbage)
       enddo
       current => list%first
       do while (associated (current))
          do while (associated (current%next))
             if (.not. current%next%node%keep) then
                garbage => current%next
                current%next => current%next%next
                call garbage%final
                deallocate (garbage)
             else
                exit
             endif
          enddo
          if (current%recycle .and. current%node%keep) then
             nodes(pos)%node => current%node
             pos = pos + 1
          endif
          current => current%next
       enddo
    endif
  end subroutine k_node_list_get_nodes

  subroutine f_node_list_final (list)
    class (f_node_list_t) :: list
    type (f_node_entry_t), pointer :: current
    list%k_node_list => null ()
    do while (associated (list%first))
       current => list%first
       list%first => list%first%next
       call current%final ()
       deallocate (current)
    enddo
  end subroutine f_node_list_final

  subroutine grove_final (grove)
    class(grove_t), intent(inout) :: grove
    grove%first => null ()
    grove%next => null ()
  end subroutine grove_final

  subroutine grove_list_final (list)
    class(grove_list_t), intent(inout) :: list
    class(grove_t), pointer :: current
    do while (associated (list%first))
       call list%first%final ()
       current => list%first
       list%first => list%first%next
       deallocate (current)
    end do
  end subroutine grove_list_final

  subroutine feyngraph_set_final (set)
    class(feyngraph_set_t), intent(inout) :: set
    class(feyngraph_t), pointer :: current
    integer :: i
    if (associated (set%fset)) then
       do i=1, size (set%fset)
          do while (associated (set%fset(i)%first))
             current => set%fset(i)%first
             set%fset(i)%first => set%fset(i)%first%next
             call current%final ()
             deallocate (current)
          end do
          set%fset(i)%model => null ()
          if (allocated (set%fset(i)%flv)) deallocate (set%fset(i)%flv)
          set%fset(i)%particle => null ()
          set%fset(i)%last => null ()
          set%fset(i)%grove_list => null ()
          call msg_debug (D_PHASESPACE, "f_node_list: final")
          call set%fset(i)%f_node_list%final ()
       enddo
       deallocate (set%fset)
       set%model => null ()
       if (allocated (set%flv)) deallocate (set%flv)
       if (associated (set%particle)) then
          do i = 1, size (set%particle)
             call set%particle(i)%final ()
          end do
          deallocate (set%particle)
       endif
       call msg_debug (D_PHASESPACE, "grove_list: final")
       call set%grove_list%final ()
    endif
  end subroutine feyngraph_set_final

  subroutine feyngraph_set_build (feyngraph_set, u_in)
    class (feyngraph_set_t), intent (inout) :: feyngraph_set
    integer, intent (in) :: u_in
    integer :: stat = 0
    character (len=FEYNGRAPH_LEN) :: omega_feyngraph_output
    type (feyngraph_t), pointer :: current_graph
    type (feyngraph_t), pointer :: compare_graph
    logical :: present
    type (dag_t) :: dag
    if (feyngraph_set%use_dag) then
       if (.not. associated (feyngraph_set%first)) then
          call dag%read_string (u_in, feyngraph_set%flv(:,1))
          call dag%construct ()
          call dag%make_feyngraphs (feyngraph_set)
       endif
    else
       if (.not. associated (feyngraph_set%first)) then
          read (unit=u_in, fmt='(A)', iostat=stat, advance='yes') omega_feyngraph_output
          if (omega_feyngraph_output(1:1) == '(') then
             allocate (feyngraph_set%first)
             feyngraph_set%first%omega_feyngraph_output = omega_feyngraph_output
             feyngraph_set%last => feyngraph_set%first
             feyngraph_set%n_graphs = feyngraph_set%n_graphs + 1
          else
             call msg_fatal ("Invalid input file")
          endif
          read (unit=u_in, fmt='(A)', iostat=stat, advance='yes') omega_feyngraph_output
          do while (stat == 0)
             if (omega_feyngraph_output(1:1) == '(') then
                compare_graph => feyngraph_set%first
                present = .false.
                do while (associated (compare_graph))
                   if (len_trim(compare_graph%omega_feyngraph_output) &
                        == len_trim(omega_feyngraph_output)) then
                      if (compare_graph%omega_feyngraph_output == omega_feyngraph_output) then
                         present = .true.
                         exit
                      endif
                   endif
                   compare_graph => compare_graph%next
                enddo
                if (.not. present) then
                   allocate (feyngraph_set%last%next)
                   feyngraph_set%last => feyngraph_set%last%next
                   feyngraph_set%last%omega_feyngraph_output = omega_feyngraph_output
                   feyngraph_set%n_graphs = feyngraph_set%n_graphs + 1
                endif
                read (unit=u_in, fmt='(A)', iostat=stat, advance='yes') omega_feyngraph_output
             else
                exit
             endif
          enddo
          current_graph => feyngraph_set%first
          do while (associated (current_graph))
             call feyngraph_construct (feyngraph_set, current_graph)
             current_graph => current_graph%next
          enddo
          feyngraph_set%f_node_list%max_tree_size = feyngraph_set%first%n_nodes
       endif
    endif
  end subroutine feyngraph_set_build

  subroutine dag_read_string (dag, u_in, flv)
    class (dag_t), intent (inout) :: dag
    integer, intent (in) :: u_in
    type(flavor_t), dimension(:), intent(in) :: flv
    type (string_t) :: string
    logical :: process_found
    logical :: rewound
!!! find process string in file
    process_found = .false.
    rewound = .false.
    do while (.not. process_found)
       call fds_file_get_line (u_in, string)
       if (len_trim(string) /= 0) then
          if (index (string, "::") > 0) then
             process_found = process_string_match (char (trim (string)), flv)
          endif
       else if (.not. rewound) then
          rewind (u_in)
          rewound = .true.
       else
          call msg_bug ("Process string not found in O'Mega input file.")
       endif
    enddo
    call fds_file_get_line (u_in, dag%string)
    if (len_trim(dag%string) == 0) &
         call msg_bug ("Process string not found in O'Mega input file.")
  end subroutine dag_read_string
  
  subroutine fds_file_get_line (u, string)
    integer, intent (in) :: u
    type (string_t), intent (out) :: string
    character (len=:), allocatable :: tmp_string
    integer :: string_size, current_len
    character (len=BUFFER_LEN) :: buffer
    integer :: fragment_len
    integer :: stat
    current_len = 0
    stat = 0
    string_size = 0
    do while (stat == 0)
       read (unit=u, fmt='(A)', iostat=stat) buffer
       if (stat /= 0) exit
       fragment_len = len_trim (buffer)
       if (fragment_len == 0) then
          exit
       else if (buffer (fragment_len:fragment_len) == "\") then
          fragment_len = fragment_len - 1
       endif
       string = trim(char(string)) // buffer(:fragment_len)
       if (buffer(fragment_len+1:fragment_len+1) /= "\") exit
    enddo
  end subroutine fds_file_get_line
  
  function process_string_match (string, flv) result (match)
    character (len=*), intent(in) :: string
    type(flavor_t), dimension(:), intent(in) :: flv
    logical :: match
    integer :: pos
    integer :: occurence
    integer :: i
    pos = 1
    match = .false.
    do i=1, size (flv)
       occurence = index (string(pos:), char(flv(i)%get_name()))
       if (occurence > 0) then
          pos = pos + occurence
          match = .true.
       else
          match = .false.
          exit
       endif
    enddo
  end function process_string_match
  
  subroutine init_sm_full_test (model)
    class(model_data_t), intent(out) :: model
    type(field_data_t), pointer :: field
    integer, parameter :: n_real = 17
    integer, parameter :: n_field = 21
    integer, parameter :: n_vtx = 56
    integer :: i
    call model%init (var_str ("SM_vertex_test"), &
         n_real, 0, n_field, n_vtx)
    call model%init_par (1, var_str ("mZ"), 91.1882_default)
    call model%init_par (2, var_str ("mW"), 80.419_default)
    call model%init_par (3, var_str ("mH"), 125._default)
    call model%init_par (4, var_str ("me"), 0.000510997_default)
    call model%init_par (5, var_str ("mmu"), 0.105658389_default)
    call model%init_par (6, var_str ("mtau"), 1.77705_default)
    call model%init_par (7, var_str ("ms"), 0.095_default)
    call model%init_par (8, var_str ("mc"), 1.2_default)
    call model%init_par (9, var_str ("mb"), 4.2_default)
    call model%init_par (10, var_str ("mtop"), 173.1_default)
    call model%init_par (11, var_str ("wtop"), 1.523_default)
    call model%init_par (12, var_str ("wZ"), 2.443_default)
    call model%init_par (13, var_str ("wW"), 2.049_default)
    call model%init_par (14, var_str ("wH"), 0.004143_default)
    call model%init_par (15, var_str ("ee"), 0.3079561542961_default)
    call model%init_par (16, var_str ("cw"), 8.819013863636E-01_default)
    call model%init_par (17, var_str ("sw"), 4.714339240339E-01_default)
    i = 0
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("D_QUARK"), 1)
    call field%set (spin_type=2, color_type=3, charge_type=-2, isospin_type=-2)
    call field%set (name = [var_str ("d")], anti = [var_str ("dbar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("U_QUARK"), 2)
    call field%set (spin_type=2, color_type=3, charge_type=3, isospin_type=2)
    call field%set (name = [var_str ("u")], anti = [var_str ("ubar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("S_QUARK"), 3)
    call field%set (spin_type=2, color_type=3, charge_type=-2, isospin_type=-2)
    call field%set (mass_data=model%get_par_real_ptr (7))
    call field%set (name = [var_str ("s")], anti = [var_str ("sbar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("C_QUARK"), 4)
    call field%set (spin_type=2, color_type=3, charge_type=3, isospin_type=2)
    call field%set (mass_data=model%get_par_real_ptr (8))
    call field%set (name = [var_str ("c")], anti = [var_str ("cbar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("B_QUARK"), 5)
    call field%set (spin_type=2, color_type=3, charge_type=-2, isospin_type=-2)
    call field%set (mass_data=model%get_par_real_ptr (9))
    call field%set (name = [var_str ("b")], anti = [var_str ("bbar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("T_QUARK"), 6)
    call field%set (spin_type=2, color_type=3, charge_type=3, isospin_type=2)
    call field%set (mass_data=model%get_par_real_ptr (10))
    call field%set (width_data=model%get_par_real_ptr (11))
    call field%set (name = [var_str ("t")], anti = [var_str ("tbar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("E_LEPTON"), 11)
    call field%set (spin_type=2)
    call field%set (mass_data=model%get_par_real_ptr (4))
    call field%set (name = [var_str ("e-")], anti = [var_str ("e+")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("E_NEUTRINO"), 12)
    call field%set (spin_type=2, is_left_handed=.true.)
    call field%set (name = [var_str ("nue")], anti = [var_str ("nuebar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("MU_LEPTON"), 13)
    call field%set (spin_type=2)
    call field%set (mass_data=model%get_par_real_ptr (5))
    call field%set (name = [var_str ("mu-")], anti = [var_str ("mu+")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("MU_NEUTRINO"), 14)
    call field%set (spin_type=2, is_left_handed=.true.)
    call field%set (name = [var_str ("numu")], anti = [var_str ("numubar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("TAU_LEPTON"), 15)
    call field%set (spin_type=2)
    call field%set (mass_data=model%get_par_real_ptr (6))
    call field%set (name = [var_str ("tau-")], anti = [var_str ("tau+")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("TAU_NEUTRINO"), 16)
    call field%set (spin_type=2, is_left_handed=.true.)
    call field%set (name = [var_str ("nutau")], anti = [var_str ("nutaubar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("GLUON"), 21)
    call field%set (spin_type=3, color_type=8)
    call field%set (name = [var_str ("gl")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("PHOTON"), 22)
    call field%set (spin_type=3)
    call field%set (name = [var_str ("A")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("Z_BOSON"), 23)
    call field%set (spin_type=3)
    call field%set (mass_data=model%get_par_real_ptr (1))
    call field%set (width_data=model%get_par_real_ptr (12))
    call field%set (name = [var_str ("Z")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("W_BOSON"), 24)
    call field%set (spin_type=3)
    call field%set (mass_data=model%get_par_real_ptr (2))
    call field%set (width_data=model%get_par_real_ptr (13))
    call field%set (name = [var_str ("W+")], anti = [var_str ("W-")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("HIGGS"), 25)
    call field%set (spin_type=1)
    call field%set (mass_data=model%get_par_real_ptr (3))
    call field%set (width_data=model%get_par_real_ptr (14))
    call field%set (name = [var_str ("H")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("PROTON"), 2212)
    call field%set (spin_type=2)
    call field%set (name = [var_str ("p")], anti = [var_str ("pbar")])
!    call field%set (mass_data=model%get_par_real_ptr (12))
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("HADRON_REMNANT_SINGLET"), 91)
    call field%set (color_type=1)
    call field%set (name = [var_str ("hr1")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("HADRON_REMNANT_TRIPLET"), 92)
    call field%set (color_type=3)
    call field%set (name = [var_str ("hr3")], anti = [var_str ("hr3bar")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("HADRON_REMNANT_OCTET"), 93)
    call field%set (color_type=8)
    call field%set (name = [var_str ("hr8")])
    call model%freeze_fields ()
    i = 0
    i = i + 1
!!! QED
    call model%set_vertex (i, [var_str ("dbar"), var_str ("d"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("ubar"), var_str ("u"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("sbar"), var_str ("s"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("cbar"), var_str ("c"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("bbar"), var_str ("b"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tbar"), var_str ("t"), var_str ("A")])
    i = i + 1
!!!
    call model%set_vertex (i, [var_str ("e+"), var_str ("e-"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("mu+"), var_str ("mu-"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tau+"), var_str ("tau-"), var_str ("A")])
    i = i + 1
!!! QCD
    call model%set_vertex (i, [var_str ("gl"), var_str ("gl"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("gl"), var_str ("gl"), &
         var_str ("gl"), var_str ("gl")])
    i = i + 1
!!!
    call model%set_vertex (i, [var_str ("dbar"), var_str ("d"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("ubar"), var_str ("u"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("sbar"), var_str ("s"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("cbar"), var_str ("c"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("bbar"), var_str ("b"), var_str ("gl")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tbar"), var_str ("t"), var_str ("gl")])
    i = i + 1
!!! Neutral currents
    call model%set_vertex (i, [var_str ("dbar"), var_str ("d"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("ubar"), var_str ("u"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("sbar"), var_str ("s"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("cbar"), var_str ("c"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("bbar"), var_str ("b"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tbar"), var_str ("t"), var_str ("Z")])
    i = i + 1
!!!
    call model%set_vertex (i, [var_str ("e+"), var_str ("e-"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("mu+"), var_str ("muu-"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tau+"), var_str ("tau-"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("nuebar"), var_str ("nue"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("numubar"), var_str ("numu"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("nutaubar"), var_str ("nutau"), &
         var_str ("Z")])
    i = i + 1
!!! Charged currents
    call model%set_vertex (i, [var_str ("ubar"), var_str ("d"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("cbar"), var_str ("s"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tbar"), var_str ("b"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("dbar"), var_str ("u"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("sbar"), var_str ("c"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("bbar"), var_str ("t"), var_str ("W-")])
    i = i + 1
!!!
    call model%set_vertex (i, [var_str ("nuebar"), var_str ("e-"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("numubar"), var_str ("mu-"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("nutaubar"), var_str ("tau-"), var_str ("W+")])
    i = i + 1
    call model%set_vertex (i, [var_str ("e+"), var_str ("nue"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("mu+"), var_str ("numu"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tau+"), var_str ("nutau"), var_str ("W-")])
    i = i + 1
!!! Yukawa
!!! keeping only 3rd generation for the moment
    ! call model%set_vertex (i, [var_str ("sbar"), var_str ("s"), var_str ("H")])
    ! i = i + 1
    ! call model%set_vertex (i, [var_str ("cbar"), var_str ("c"), var_str ("H")])
    ! i = i + 1
    call model%set_vertex (i, [var_str ("bbar"), var_str ("b"), var_str ("H")])
    i = i + 1
    call model%set_vertex (i, [var_str ("tbar"), var_str ("t"), var_str ("H")])
    i = i + 1
    ! call model%set_vertex (i, [var_str ("mubar"), var_str ("mu"), var_str ("H")])
    ! i = i + 1
    call model%set_vertex (i, [var_str ("taubar"), var_str ("tau"), var_str ("H")])
    i = i + 1
!!! Vector-boson self-interactions
    call model%set_vertex (i, [var_str ("W+"), var_str ("W-"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("W+"), var_str ("W-"), var_str ("Z")])
    i = i + 1
!!!
    call model%set_vertex (i, [var_str ("W+"), var_str ("W-"), var_str ("Z"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("W+"), var_str ("W+"), var_str ("W-"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("W+"), var_str ("W-"), var_str ("Z"), var_str ("A")])
    i = i + 1
    call model%set_vertex (i, [var_str ("W+"), var_str ("W-"), var_str ("A"), var_str ("A")])
    i = i + 1
!!! Higgs - vector boson
    ! call model%set_vertex (i, [var_str ("H"), var_str ("Z"), var_str ("A")])
    ! i = i + 1
    ! call model%set_vertex (i, [var_str ("H"), var_str ("A"), var_str ("A")])
    ! i = i + 1
    ! call model%set_vertex (i, [var_str ("H"), var_str ("gl"), var_str ("gl")])
    ! i = i + 1
!!!
    call model%set_vertex (i, [var_str ("H"), var_str ("W+"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("H"), var_str ("Z"), var_str ("Z")])
    i = i + 1
    call model%set_vertex (i, [var_str ("H"), var_str ("H"), var_str ("W+"), var_str ("W-")])
    i = i + 1
    call model%set_vertex (i, [var_str ("H"), var_str ("H"), var_str ("Z"), var_str ("Z")])
    i = i + 1
!!! Higgs self-interactions
    call model%set_vertex (i, [var_str ("H"), var_str ("H"), var_str ("H")])
    i = i + 1
    call model%set_vertex (i, [var_str ("H"), var_str ("H"), var_str ("H"), var_str ("H")])
    i = i + 1
    call model%freeze_vertices ()
  end subroutine init_sm_full_test

  recursive subroutine part_prop_init (part_prop, feyngraph_set, particle_label)
    class (part_prop_t), intent (out), target :: part_prop
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    character (len=*), intent (in) :: particle_label
    type (flavor_t) :: flv, anti
    type (string_t) :: name
    integer :: i
    name = particle_label
    call flv%init (name, feyngraph_set%model)
    part_prop%particle_label = particle_label
    part_prop%pdg = flv%get_pdg ()
    part_prop%mass = flv%get_mass ()
    part_prop%width = flv%get_width()
    part_prop%spin_type = flv%get_spin_type ()
    part_prop%is_vector = flv%get_spin_type () == VECTOR
    part_prop%empty = .false.
    part_prop%tex_name = flv%get_tex_name ()
    anti = flv%anti ()
    if (flv%get_pdg() == anti%get_pdg()) then
       select type (part_prop)
       type is (part_prop_t)
          part_prop%anti => part_prop
       end select
    else
       do i=1, size (feyngraph_set%particle)
          if (feyngraph_set%particle(i)%pdg == (- part_prop%pdg)) then
             part_prop%anti => feyngraph_set%particle(i)
             exit
          else if (feyngraph_set%particle(i)%empty) then
             part_prop%anti => feyngraph_set%particle(i)
             call feyngraph_set%particle(i)%init (feyngraph_set, char(anti%get_name()))
             exit
          endif
       enddo
    endif
  end subroutine part_prop_init

  subroutine f_node_assign_particle_properties (node, feyngraph_set)
    class (f_node_t), intent (inout ) :: node
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    character (len=LABEL_LEN) :: particle_label
    integer :: i
    particle_label = node%particle_label(1:index (node%particle_label, '[')-1)
    if (.not. associated (feyngraph_set%particle)) then
       allocate (feyngraph_set%particle (PRT_ARRAY_SIZE))
    endif
    do i = 1, size (feyngraph_set%particle)
       if (particle_label == feyngraph_set%particle(i)%particle_label) then
          node%particle => feyngraph_set%particle(i)
          exit
       else if (feyngraph_set%particle(i)%empty) then
          call feyngraph_set%particle(i)%init (feyngraph_set, particle_label)
          node%particle => feyngraph_set%particle(i)
          exit
       endif
    enddo
!!! Since the OMega output uses the anti-particles instead of the particles specified
!!! in the process definition, we revert this here. An exception is the first particle
!!! in the parsable DAG output
    node%particle => node%particle%anti
  end subroutine f_node_assign_particle_properties

  function get_n_daughters (subtree_string, pos_first_colon) &
       result (n_daughters)
    character (len=*), intent (in) :: subtree_string
    integer, intent (in) :: pos_first_colon
    integer :: n_daughters
    integer :: n_open_par
    integer :: i
    n_open_par = 1
    n_daughters = 0
    if (len_trim(subtree_string) > 0) then
       if (pos_first_colon > 0) then
          do i=pos_first_colon, len_trim(subtree_string)
             if (subtree_string(i:i) == ',') then
                if (n_open_par == 1) n_daughters = n_daughters + 1
             else if (subtree_string(i:i) == '(') then
                n_open_par = n_open_par + 1
             else if (subtree_string(i:i) == ')') then
                n_open_par = n_open_par - 1
             end if
          end do
          if (n_open_par == 0) then
             n_daughters = n_daughters + 1
          end if
       end if
    end if
  end function get_n_daughters

  recursive subroutine node_construct_subtree_rec (feyngraph_set, &
       feyngraph, subtree_string, mother_node)
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (feyngraph_t), intent (inout) :: feyngraph
    character (len=*), intent (in) :: subtree_string
    type (f_node_t), pointer, intent (inout) :: mother_node
    integer :: n_daughters
    integer :: pos_first_colon
    integer :: current_daughter
    integer :: pos_subtree_begin, pos_subtree_end
    integer :: i
    integer :: n_open_par
    if (.not. associated (mother_node)) then
       call feyngraph_set%f_node_list%add_entry (subtree_string, mother_node, .true.)
       current_daughter = 1
       n_open_par = 1
       pos_first_colon = index (subtree_string, ':')
       n_daughters = get_n_daughters (subtree_string, pos_first_colon)
       if (pos_first_colon == 0) then
          mother_node%particle_label = subtree_string
       else
          mother_node%particle_label = subtree_string(2:pos_first_colon-1)
       end if
       if (.not. associated (mother_node%particle)) then
          call mother_node%assign_particle_properties (feyngraph_set)
       endif
       if (n_daughters /= 2 .and. n_daughters /= 0) then
          mother_node%keep = .false.
          feyngraph%keep = .false.
          return
       end if
       pos_subtree_begin = pos_first_colon + 1
       do i = pos_first_colon + 1, len(trim(subtree_string))
          if (current_daughter == 2) then
             pos_subtree_end = len(trim(subtree_string)) - 1
             call node_construct_subtree_rec (feyngraph_set, feyngraph, &
                  subtree_string(pos_subtree_begin:pos_subtree_end), &
                  mother_node%daughter2)
             exit
          else if (subtree_string(i:i) == ',') then
             if (n_open_par == 1) then
                pos_subtree_end = i - 1
                call node_construct_subtree_rec (feyngraph_set, feyngraph, &
                     subtree_string(pos_subtree_begin:pos_subtree_end), &
                     mother_node%daughter1)
                current_daughter = 2
                pos_subtree_begin = i + 1
             end if
          else if (subtree_string(i:i) == '(') then
             n_open_par = n_open_par + 1
          else if (subtree_string(i:i) == ')') then
             n_open_par = n_open_par - 1
          end if
       end do
    endif
    if (associated (mother_node%daughter1)) then
       if (.not. mother_node%daughter1%keep) then
          mother_node%keep = .false.
       endif
    endif
    if (associated (mother_node%daughter2)) then
       if (.not. mother_node%daughter2%keep) then
          mother_node%keep = .false.
       endif
    endif
    if (associated (mother_node%daughter1) .and. &
         associated (mother_node%daughter2)) then
       mother_node%n_subtree_nodes = &
            mother_node%daughter1%n_subtree_nodes &
            + mother_node%daughter2%n_subtree_nodes + 1
    endif
    if (.not. mother_node%keep) then
       feyngraph%keep = .false.
    endif
  end subroutine node_construct_subtree_rec

  recursive subroutine node_construct_subtree_rec_dag (feyngraph_set, &
       subtree_string, mother_node)
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    character (len=*), intent (in) :: subtree_string
    type (f_node_t), pointer, intent (inout) :: mother_node
    integer :: n_daughters
    integer :: pos_first_colon
    integer :: current_daughter
    integer :: pos_subtree_begin, pos_subtree_end
    integer :: i
    integer :: n_open_par
    if (.not. associated (mother_node)) then
       call feyngraph_set%f_node_list%add_entry (subtree_string, mother_node, .true.)
       current_daughter = 1
       n_open_par = 1
       pos_first_colon = index (subtree_string, ':')
       n_daughters = get_n_daughters (subtree_string, pos_first_colon)
       if (pos_first_colon == 0) then
          mother_node%particle_label = subtree_string
       else
          mother_node%particle_label = subtree_string(2:pos_first_colon-1)
       end if
       if (.not. associated (mother_node%particle)) then
          call mother_node%assign_particle_properties (feyngraph_set)
       endif
       if (n_daughters /= 2 .and. n_daughters /= 0) then
          mother_node%keep = .false.
          return
       end if
       pos_subtree_begin = pos_first_colon + 1
       do i = pos_first_colon + 1, len(trim(subtree_string))
          if (current_daughter == 2) then
             pos_subtree_end = len(trim(subtree_string)) - 1
             call node_construct_subtree_rec_dag (feyngraph_set, &
                  subtree_string(pos_subtree_begin:pos_subtree_end), &
                  mother_node%daughter2)
             exit
           else if (subtree_string(i:i) == ',') then
             if (n_open_par == 1) then
                pos_subtree_end = i - 1
                call node_construct_subtree_rec_dag (feyngraph_set, &
                     subtree_string(pos_subtree_begin:pos_subtree_end), &
                     mother_node%daughter1)
                current_daughter = 2
                pos_subtree_begin = i + 1
             end if
          else if (subtree_string(i:i) == '(') then
             n_open_par = n_open_par + 1
          else if (subtree_string(i:i) == ')') then
             n_open_par = n_open_par - 1
          end if
       end do
    endif
    if (associated (mother_node%daughter1)) then
       if (.not. mother_node%daughter1%keep) then
          mother_node%keep = .false.
       endif
    endif
    if (associated (mother_node%daughter2)) then
       if (.not. mother_node%daughter2%keep) then
          mother_node%keep = .false.
       endif
    endif
    if (associated (mother_node%daughter1) .and. &
         associated (mother_node%daughter2)) then
       mother_node%n_subtree_nodes = &
            mother_node%daughter1%n_subtree_nodes &
            + mother_node%daughter2%n_subtree_nodes + 1
    endif
  end subroutine node_construct_subtree_rec_dag

  subroutine feyngraph_construct (feyngraph_set, feyngraph)
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (feyngraph_t), pointer, intent (inout) :: feyngraph
    call node_construct_subtree_rec (feyngraph_set, feyngraph, &
         feyngraph%omega_feyngraph_output, feyngraph%root)
    feyngraph%n_nodes = feyngraph%root%n_subtree_nodes
  end subroutine feyngraph_construct

  subroutine dag_construct (dag)
    class (dag_t), intent (inout) :: dag
    character (len=len(dag%string)) :: string
    integer :: n_nodes
    integer :: n_options
    integer :: n_combinations
    logical :: continue_loop
    string = char (dag%string)
    call dag%get_nodes_and_combinations (string, .true.)
    continue_loop = .true.
    do while (continue_loop)
       n_nodes = dag%n_nodes
       n_options = dag%n_options
       n_combinations = dag%n_combinations
       call dag%get_nodes_and_combinations (string, .false.)
       call dag%get_options (string)
       if (n_nodes == dag%n_nodes .and. n_options == dag%n_options &
            .and. n_combinations == dag%n_combinations) then
          continue_loop = .false.
       endif
    enddo
!!! add root node to dag
    call dag%add_node (string, leaf = .false.)
    dag%string = string
    if (debug2_active (D_PHASESPACE)) then
       call dag%write (output_unit)
    endif
  end subroutine dag_construct
  
  subroutine dag_get_nodes_and_combinations (dag, string, leaves)
    class (dag_t), intent (inout) :: dag
    character (len=*), intent (inout) :: string
    logical, intent (in) :: leaves
    character (len=len(string)) :: new_string
    integer :: open_par
    logical :: keep
    integer :: i, j, k
    integer :: i_node
    integer :: index_start, index_end
    integer :: new_len
    character (len=10) :: index_char
    logical :: slash_found
    logical :: combination
!!! Create nodes (leaves) for every closed pair of parentheses
!!! which do not contain any optional branchings (i.e. {})
    new_string = ' '
    i = 1
    new_len = 1
    do while (i <= len (string))
       if (string(i:i) == '(') then
          open_par = 1
          keep = .true.
          combination = .true.
          j = i + 1
          do while (j <= len (string))
             if (string(j:j) == '(') then
                open_par = open_par + 1
             else if (string(j:j) == ')') then
                open_par = open_par - 1
                if (open_par == 0) then
                   if (combination) then
                      call dag%add_combination (string(i:j), i_node)
                   else
                      call dag%add_node (string(i:j), leaves, i_node)
                   endif
                   exit
                else if (open_par == 1) then
                   keep = .false.
                   exit
                endif
             else if (string(j:j) == '{') then
                keep = .false.
                exit
             else if (string(j:j) == ':') then
                combination = .false.
             endif
             j = j + 1
          enddo
       else
          keep = .false.
       endif
       if (keep) then
          index_char = ' '
          write (index_char, fmt='(I10)') i_node
          do k=1, len(index_char)
             if (index_char(k:k) /= ' ') then
                index_start = k
                exit
             endif
          enddo
          index_end = len_trim (index_char)
          if (combination) then
             new_string(new_len:new_len+index_end-index_start+3) = &
                  '<C' // index_char(index_start:index_end) // '>'             
          else
             new_string(new_len:new_len+index_end-index_start+3) = &
                  '<N' // index_char(index_start:index_end) // '>'
          endif
          new_len = new_len + index_end - index_start + 4
          i = j + 1
       else
          new_string(new_len:new_len) = string(i:i)
          new_len = new_len + 1
          i = i + 1
       endif
    enddo
    string = trim (new_string)
    if (leaves) then
!!! Create nodes also for those external particles which do not appear between (),
!!! except for the root particle (first incoming)
       new_string = ' '
       new_len = index (string, ':')
       new_string(:new_len) = string(:new_len)
       new_len = new_len + 1
       i = new_len
       do while (i <= len_trim (string))
          select case (string(i:i))
          case ('<')
             do while (string(i:i) /= '>')
                new_string(new_len:new_len) = string(i:i)
                new_len = new_len + 1
                i = i + 1
             enddo
          case ('{', '}', '(', ')', ':', ',', '|', ' ', '>')
             new_string(new_len:new_len) = string(i:i)
             new_len = new_len + 1
             i = i + 1
          case default
             do j = i, len_trim(string)
                if (string(j:j) == '[') then
                   slash_found = .false.
                else if (string(j:j) == ']') then
                   if (.not. slash_found) then
                      call dag%add_node (string(i:j), .true., i_node)
                      index_char = ' '
                      write (index_char, fmt='(I10)') i_node
                      do k=1, len(index_char)
                         if (index_char(k:k) /= ' ') then
                            index_start = k
                            exit
                         endif
                      enddo
                      index_end = len_trim (index_char)
                      new_string(new_len:new_len+index_end-index_start+3)= &
                           '<N' // index_char(index_start:index_end) // '>'
                      new_len = new_len + index_end - index_start + 4
                      i = j + 1
                   else
!!! particle not external
                      new_string(new_len:new_len+j-i) = string(i:j)
                      new_len = new_len + j - i + 1
                      i = j + 1
                   endif
                   exit
                else if (string(j:j) == '/') then
                   slash_found = .true.
                endif
             enddo
          end select
       enddo
       string = new_string
    endif
  end subroutine dag_get_nodes_and_combinations

  subroutine dag_get_options (dag, string)
    class (dag_t), intent (inout) :: dag
    character (len=*), intent (inout) :: string
    character (len=len(dag%string)) :: new_string
    integer :: i, j, k
    integer :: new_len
    integer :: i_options
    character (len=10) :: index_char
    integer :: index_start, index_end
    new_string = ' '
    i = 1
    new_len = 1
    do while (i <= len (string))
       if (string(i:i) == '{') then
          do j = i+1, len (string)
             if (string(j:j) == '{' .or. string(j:j) == '(') then
                new_string(new_len:new_len) = string(i:i)
                new_len = new_len + 1
                i = i + 1
                exit
             else if (string(j:j) == '<') then
                if (string(j+1:j+1) /= 'N' .and. string(j+1:j+1) /= 'C') then
                   new_string(new_len:new_len) = string(i:i)
                   new_len = new_len + 1
                   i = i + 1
                   exit
                endif
             else if (string(j:j) == '}') then
                call dag%add_options (string(i:j), i_options)
                index_char = ' '
                write (index_char, fmt='(I10)') i_options
                do k=1, len(index_char)
                   if (index_char(k:k) /= ' ') then
                      index_start = k
                      exit
                   endif
                enddo
                index_end = len_trim (index_char)
                new_string(new_len:new_len+index_end-index_start+3) = &
                     '<O' // index_char(index_start:index_end) // '>'
                new_len = new_len + index_end - index_start + 4
                i = j + 1
                exit
             endif
          enddo
       else
          new_string(new_len:new_len) = string(i:i)
          new_len = new_len + 1
          i = i + 1
       endif
    enddo
    string = trim (new_string)    
  end subroutine dag_get_options
  
  subroutine dag_add_node (dag, string, leaf, i_node)
    class (dag_t), intent (inout) :: dag
    character (len=*), intent (in) :: string
    logical, intent (in) :: leaf
    integer, intent (out), optional :: i_node
    type (dag_node_t), dimension (:), allocatable :: tmp_node
    integer :: string_len
    integer :: i
    string_len = len(string)
    if (.not. allocated (dag%node)) then
        allocate (dag%node (DAG_STACK_SIZE))
     else if (dag%n_nodes == size (dag%node)) then
        allocate (tmp_node (dag%n_nodes))
        tmp_node = dag%node
        deallocate (dag%node)
        allocate (dag%node (dag%n_nodes+DAG_STACK_SIZE))
        dag%node(:dag%n_nodes) = tmp_node
        deallocate (tmp_node)
     endif
     do i = 1, dag%n_nodes
        if (dag%node(i)%string_len == string_len) then
           if (dag%node(i)%string == string) then
              if (present (i_node)) i_node = i
              return
           endif
        endif
     enddo
     dag%n_nodes = dag%n_nodes + 1
     dag%node(dag%n_nodes)%string = string
     dag%node(dag%n_nodes)%string_len = string_len
     if (present (i_node)) i_node = dag%n_nodes
     dag%node(dag%n_nodes)%leaf = leaf
  end subroutine dag_add_node

  subroutine dag_add_options (dag, string, i_options)
    class (dag_t), intent (inout) :: dag
    character (len=*), intent (in) :: string
    integer, intent (out), optional :: i_options
    type (dag_options_t), dimension (:), allocatable :: tmp_options
    integer :: string_len
    integer :: i
    string_len = len(string)
    if (.not. allocated (dag%options)) then
        allocate (dag%options (DAG_STACK_SIZE))
     else if (dag%n_options == size (dag%options)) then
        allocate (tmp_options (dag%n_options))
        tmp_options = dag%options
        deallocate (dag%options)
        allocate (dag%options (dag%n_options+DAG_STACK_SIZE))
        dag%options(:dag%n_options) = tmp_options
        deallocate (tmp_options)
     endif
     do i = 1, dag%n_options
        if (dag%options(i)%string_len == string_len) then
           if (dag%options(i)%string == string) then
              if (present (i_options)) i_options = i
              return
           endif
        endif
     enddo
     dag%n_options = dag%n_options + 1
     dag%options(dag%n_options)%string = string
     dag%options(dag%n_options)%string_len = string_len
     if (present (i_options)) i_options = dag%n_options
  end subroutine dag_add_options
  
  subroutine dag_add_combination (dag, string, i_combination)
    class (dag_t), intent (inout) :: dag
    character (len=*), intent (in) :: string
    integer, intent (out), optional :: i_combination
    type (dag_combination_t), dimension (:), allocatable :: tmp_combination
    integer :: string_len
    integer :: i
    string_len = len(string)
    if (.not. allocated (dag%combination)) then
        allocate (dag%combination (DAG_STACK_SIZE))
     else if (dag%n_combinations == size (dag%combination)) then
        allocate (tmp_combination (dag%n_combinations))
        tmp_combination = dag%combination
        deallocate (dag%combination)
        allocate (dag%combination (dag%n_combinations+DAG_STACK_SIZE))
        dag%combination(:dag%n_combinations) = tmp_combination
        deallocate (tmp_combination)
     endif
     do i = 1, dag%n_combinations
        if (dag%combination(i)%string_len == string_len) then
           if (dag%combination(i)%string == string) then
              i_combination = i
              return
           endif
        endif
     enddo
     dag%n_combinations = dag%n_combinations + 1
     dag%combination(dag%n_combinations)%string = string
     dag%combination(dag%n_combinations)%string_len = string_len
     if (present (i_combination)) i_combination = dag%n_combinations
  end subroutine dag_add_combination
  
  recursive subroutine dag_node_make_f_nodes (dag_node, string, feyngraph_set, dag)
    class (dag_node_t), intent (inout) :: dag_node
    character (len=*), intent (in) :: string
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (dag_t), intent (inout) :: dag
    type (f_node_ptr_t), dimension (:), allocatable :: daughter1_ptr
    type (f_node_ptr_t), dimension (:), allocatable :: daughter2_ptr
    character (len=LABEL_LEN) :: particle_label
    integer :: colon_pos
    integer :: i, j
    character (len=1), dimension (2) :: obj
    integer, dimension (2) :: i_obj
    integer :: n_obj
    integer :: pos
    integer :: new_size, size1, size2
    if (allocated (dag_node%f_node)) return
    colon_pos = index(string,':')
    if (colon_pos /= 0) then
       if (string(1:1) == '(') then
          particle_label = string(2:colon_pos-1)
       else
          particle_label = string(:colon_pos-1)
       endif
    else
       particle_label = string
    endif
    if (dag_node%leaf) then
!!! construct subtree with procedure similar to the one for the old output
       allocate (dag_node%f_node(1))
       call node_construct_subtree_rec_dag (feyngraph_set, trim(string), dag_node%f_node(1)%node)
       if (.not. dag_node%f_node(1)%node%keep) then
          deallocate (dag_node%f_node)
          return
       endif
    else
       obj = ' '
       i_obj = 0
       n_obj = 0
       do i = colon_pos+1, len (string)
          if (string(i:i) == '<') then
             n_obj = n_obj + 1
             if (n_obj > 2) return
             do j = i+1, len (string)
                if (string(j:j) == '>') then
                   read (string(i+1:i+1), fmt='(A)') obj(n_obj)
                   read (string(i+2:j-1), fmt='(I10)') i_obj(n_obj)
                   exit
                endif
             enddo
          endif
       enddo
       if (n_obj == 1) then
          if (obj(1) == 'O') then
             call dag%options(i_obj(1))%make_f_nodes (char (dag%options(i_obj(1))%string), &
                  feyngraph_set, dag, daughter1_ptr, daughter2_ptr)
          else if (obj(1) == 'C') then
             call dag%combination(i_obj(1))%make_f_nodes(char (dag%combination(i_obj(1))%string), &
                  feyngraph_set, dag, daughter1_ptr, daughter2_ptr)
          endif
          allocate (dag_node%f_node(size(daughter1_ptr)))
          do i=1, size(dag_node%f_node)
             call feyngraph_set%f_node_list%add_entry (string, dag_node%f_node(i)%node, .false.)
             call dag_node%f_node(i)%node%set_index ()
             dag_node%f_node(i)%node%particle_label = particle_label
             call dag_node%f_node(i)%node%assign_particle_properties (feyngraph_set)
             dag_node%f_node(i)%node%daughter1 => daughter1_ptr(i)%node
             dag_node%f_node(i)%node%daughter2 => daughter2_ptr(i)%node
             dag_node%f_node(i)%node%n_subtree_nodes = daughter1_ptr(i)%node%n_subtree_nodes &
                  + daughter2_ptr(i)%node%n_subtree_nodes + 1
          enddo
!!! simply set daughter pointers, daughters are already combined correctly
       else if (n_obj == 2) then
          if (obj(1) == 'N') then
             call dag%node(i_obj(1))%make_f_nodes (char (dag%node(i_obj(1))%string), &
                  feyngraph_set, dag)
             allocate (daughter1_ptr (size (dag%node(i_obj(1))%f_node)))
             daughter1_ptr = dag%node(i_obj(1))%f_node
          else if (obj(1) == 'O') then
             call dag%options(i_obj(1))%make_f_nodes (char (dag%options(i_obj(1))%string), &
                  feyngraph_set, dag, daughter1_ptr)
          endif
          if (obj(2) == 'N') then
             call dag%node(i_obj(2))%make_f_nodes (char (dag%node(i_obj(2))%string), &
                  feyngraph_set, dag)
             allocate (daughter2_ptr (size (dag%node(i_obj(2))%f_node)))
             daughter2_ptr = dag%node(i_obj(2))%f_node
          else if (obj(2) == 'O') then
             call dag%options(i_obj(2))%make_f_nodes (char (dag%options(i_obj(2))%string), &
                  feyngraph_set, dag, daughter2_ptr)
          endif
!!! make all combinations of daughters
          size1 = size (daughter1_ptr)
          size2 = size (daughter2_ptr)
          new_size = size1*size2
          allocate (dag_node%f_node(new_size))
          pos = 0
          do i = 1, size1
             do j = 1, size2
                pos = pos + 1
                call feyngraph_set%f_node_list%add_entry(string, dag_node%f_node(pos)%node, .false.)
                call dag_node%f_node(pos)%node%set_index ()
                dag_node%f_node(pos)%node%particle_label = particle_label
                call dag_node%f_node(pos)%node%assign_particle_properties (feyngraph_set)
                dag_node%f_node(pos)%node%daughter1 => daughter1_ptr(i)%node
                dag_node%f_node(pos)%node%daughter2 => daughter2_ptr(j)%node
                dag_node%f_node(pos)%node%n_subtree_nodes = daughter1_ptr(i)%node%n_subtree_nodes &
                  + daughter2_ptr(j)%node%n_subtree_nodes + 1
             enddo
          enddo
          deallocate (daughter1_ptr, daughter2_ptr)
       endif
    endif
  end subroutine dag_node_make_f_nodes
  
  recursive subroutine dag_options_make_f_nodes_single (dag_options, &
       string, feyngraph_set, dag, node_ptr)
    class (dag_options_t), intent (inout) :: dag_options
    character (len=*), intent (in) :: string
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (dag_t), intent (inout) :: dag
    type (f_node_ptr_t), dimension(:), allocatable, intent (out) :: node_ptr
    type (f_node_ptr_t), dimension(:), allocatable :: tmp_node_ptr
    character (len=1), dimension (:), allocatable :: obj, tmp_obj
    integer, dimension (:), allocatable :: i_obj, tmp_i_obj
    integer :: n_obj
    integer :: i, j
!!! read options
    n_obj = 0
    do i = 1, len (string)
       if (string(i:i) == '<') then
          if (n_obj > 0) then
             allocate (tmp_obj(n_obj)); allocate (tmp_i_obj(n_obj))
             tmp_obj = obj; tmp_i_obj = i_obj
             deallocate (obj, i_obj)
          endif
          allocate (obj(n_obj+1)); allocate (i_obj(n_obj+1))
          if (n_obj > 0) then
             obj(:n_obj) = tmp_obj; i_obj(:n_obj) = tmp_i_obj
             deallocate (tmp_obj, tmp_i_obj)
          endif
          n_obj = n_obj + 1
          do j = i+1, len (string)
             if (string(j:j) == '>') then
                read (string(i+1:i+1), fmt='(A)') obj(n_obj)
                read (string(i+2:j-1), fmt='(I10)') i_obj(n_obj)
                exit
             endif
          enddo
       endif
    enddo
    do i=1, n_obj
       if (obj(i) == 'N') then
          if (allocated (node_ptr)) then
             call dag%node(i_obj(i))%make_f_nodes (char (dag%node(i_obj(i))%string), &
                  feyngraph_set, dag)
             if (allocated (dag%node(i_obj(i))%f_node)) then
                allocate (tmp_node_ptr (size (node_ptr)))
                tmp_node_ptr = node_ptr
                deallocate (node_ptr)
                allocate (node_ptr (size (tmp_node_ptr) + size (dag%node(i_obj(i))%f_node)))
                node_ptr(:size(tmp_node_ptr)) = tmp_node_ptr
                node_ptr(size(tmp_node_ptr)+1:) = dag%node(i_obj(i))%f_node
                deallocate (tmp_node_ptr)
             endif
          else
             call dag%node(i_obj(i))%make_f_nodes (char (dag%node(i_obj(i))%string), &
                  feyngraph_set, dag)
             allocate (node_ptr (size (dag%node(i_obj(i))%f_node)))
             node_ptr = dag%node(i_obj(i))%f_node
          endif
       else
          call msg_bug ('Node (N) expected, but parsed object is ' // obj(i))
       endif
    enddo
    deallocate (obj, i_obj)
  end subroutine dag_options_make_f_nodes_single
  
  subroutine dag_options_make_f_nodes_pair (dag_options, &
       string, feyngraph_set, dag, node_ptr1, node_ptr2)
    class (dag_options_t), intent (inout) :: dag_options
    character (len=*), intent (in) :: string
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (dag_t), intent (inout) :: dag
    type (f_node_ptr_t), dimension(:), allocatable, intent (out) :: node_ptr1
    type (f_node_ptr_t), dimension(:), allocatable, intent (out) :: node_ptr2
    type (f_node_ptr_t), dimension(:), allocatable :: tmp_node_ptr1, tmp_node_ptr2
    type (f_node_ptr_t), dimension(:), allocatable :: dummy_node_ptr1, dummy_node_ptr2   
    character (len=1), dimension (:), allocatable :: obj, tmp_obj
    integer, dimension (:), allocatable :: i_obj, tmp_i_obj
    integer :: n_obj
    integer :: i, j
!!! read options
    n_obj = 0
    do i = 1, len (string)
       if (string(i:i) == '<') then
          if (n_obj > 0) then
             allocate (tmp_obj(n_obj)); allocate (tmp_i_obj(n_obj))
             tmp_obj = obj; tmp_i_obj = i_obj
             deallocate (obj, i_obj)
          endif
          allocate (obj(n_obj+1)); allocate (i_obj(n_obj+1))
          if (n_obj > 0) then
             obj(:n_obj) = tmp_obj; i_obj(:n_obj) = tmp_i_obj
             deallocate (tmp_obj, tmp_i_obj)
          endif
          n_obj = n_obj + 1
          do j = i+1, len (string)
             if (string(j:j) == '>') then
                read (string(i+1:i+1), fmt='(A)') obj(n_obj)
                read (string(i+2:j-1), fmt='(I10)') i_obj(n_obj)
                exit
             endif
          enddo
       endif
    enddo
!!! get f_nodes from each combination
    do i=1, n_obj
       if (obj(i) == 'C') then
          if (allocated (node_ptr1) .and. allocated (node_ptr2)) then
             call dag%combination(i_obj(i))%make_f_nodes (char (dag%combination(i_obj(i))%string), &
                  feyngraph_set, dag, dummy_node_ptr1, dummy_node_ptr2)
             if (allocated (dummy_node_ptr1) .and. allocated (dummy_node_ptr2)) then
                allocate (tmp_node_ptr1 (size (node_ptr1)))
                allocate (tmp_node_ptr2 (size (node_ptr2)))
                tmp_node_ptr1 = node_ptr1
                tmp_node_ptr2 = node_ptr2
                deallocate (node_ptr1)
                deallocate (node_ptr2)
                allocate (node_ptr1 (size (tmp_node_ptr1) + size (dummy_node_ptr1)))
                allocate (node_ptr2 (size (tmp_node_ptr2) + size (dummy_node_ptr2)))
                node_ptr1(:size(tmp_node_ptr1)) = tmp_node_ptr1
                node_ptr2(:size(tmp_node_ptr2)) = tmp_node_ptr2
                node_ptr1(size(tmp_node_ptr1)+1:) = dummy_node_ptr1
                node_ptr2(size(tmp_node_ptr2)+1:) = dummy_node_ptr2
                deallocate (tmp_node_ptr1, dummy_node_ptr1)
                deallocate (tmp_node_ptr2, dummy_node_ptr2)
             endif
          else
             call dag%combination(i_obj(i))%make_f_nodes (char (dag%combination(i_obj(i))%string), &
                  feyngraph_set, dag, node_ptr1, node_ptr2)
          endif
       else
          call msg_bug ('Combination (C) expected, but parsed object is ' // obj(i))
       endif
    enddo
    deallocate (obj, i_obj)
  end subroutine dag_options_make_f_nodes_pair
  
  subroutine dag_combination_make_f_nodes (dag_combination, &
       string, feyngraph_set, dag, node_ptr1, node_ptr2)
    class (dag_combination_t), intent (inout) :: dag_combination
    character (len=*), intent (in) :: string
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (dag_t), intent (inout) :: dag
    type (f_node_ptr_t), dimension(:), allocatable, intent (out) :: node_ptr1
    type (f_node_ptr_t), dimension(:), allocatable, intent (out) :: node_ptr2
    type (f_node_ptr_t), dimension(:), allocatable :: dummy_node_ptr1
    type (f_node_ptr_t), dimension(:), allocatable :: dummy_node_ptr2
    character (len=1), dimension (2) :: obj
    integer, dimension (2) :: i_obj
    integer :: i, j
    integer :: n_obj
    integer :: new_size, size1, size2
    integer :: pos
    obj = ' '
    i_obj = 0
    n_obj = 0
    do i = 1, len (string)
       if (string(i:i) == '<') then
          n_obj = n_obj + 1
          if (n_obj > 2) return
          do j = i+1, len (string)
             if (string(j:j) == '>') then
                read (string(i+1:i+1), fmt='(A)') obj(n_obj)
                read (string(i+2:j-1), fmt='(I10)') i_obj(n_obj)
                exit
             endif
          enddo
       endif
    enddo
    if (n_obj /= 2) then
       call msg_bug ('Combination should contain 2 elements, but contains ' &
            // char(n_obj) // ' elements')
    else
       if (obj(1) == 'N') then
          call dag%node(i_obj(1))%make_f_nodes (char (dag%node(i_obj(1))%string), &
               feyngraph_set, dag)
          allocate (dummy_node_ptr1 (size (dag%node(i_obj(1))%f_node)))
          dummy_node_ptr1 = dag%node(i_obj(1))%f_node
       else if (obj(1) == 'O') then
          call dag%options(i_obj(1))%make_f_nodes (char (dag%options(i_obj(1))%string), &
               feyngraph_set, dag, dummy_node_ptr1)
       else
          call msg_bug ('Node (N) or option (O) expected, but parsed object is' // obj(1))
       endif
       if (allocated (dummy_node_ptr1)) then
          if (obj(2) == 'N') then
             call dag%node(i_obj(2))%make_f_nodes (char (dag%node(i_obj(2))%string), &
                  feyngraph_set, dag)
             allocate (dummy_node_ptr2 (size (dag%node(i_obj(2))%f_node)))
             dummy_node_ptr2 = dag%node(i_obj(2))%f_node
          else if (obj(2) == 'O') then
             call dag%options(i_obj(2))%make_f_nodes (char (dag%options(i_obj(2))%string), &
                  feyngraph_set, dag, dummy_node_ptr2)
          else
             call msg_bug ('Node (N) or option (O) expected, but parsed object is' // obj(2))
          endif
       endif
    endif
!!! combine the 2 arrays of f_nodes
    if (allocated (dummy_node_ptr1) .and. allocated (dummy_node_ptr2)) then
       size1 = size (dummy_node_ptr1)
       size2 = size (dummy_node_ptr2)
       new_size = size1*size2
       allocate (node_ptr1 (new_size))
       allocate (node_ptr2 (new_size))
       pos = 0
       do i = 1, size1
          do j = 1, size2
             pos = pos + 1
             node_ptr1(pos) = dummy_node_ptr1(i)
             node_ptr2(pos) = dummy_node_ptr2(j)
          enddo
       enddo
       deallocate (dummy_node_ptr1, dummy_node_ptr2)
    endif
  end subroutine dag_combination_make_f_nodes

  subroutine dag_make_feyngraphs (dag, feyngraph_set)
    class (dag_t), intent (inout) :: dag
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    integer :: i
    type (f_node_ptr_t), dimension (:), allocatable :: node_ptr
    call dag%node(dag%n_nodes)%make_f_nodes (char (dag%node(dag%n_nodes)%string), &
         feyngraph_set, dag)
    allocate (node_ptr (size (dag%node(dag%n_nodes)%f_node)))
    node_ptr = dag%node(dag%n_nodes)%f_node
    if (allocated (node_ptr)) then
       do i = 1, size (node_ptr)
          if (.not. associated (feyngraph_set%first)) then
             allocate (feyngraph_set%last)
             feyngraph_set%first => feyngraph_set%last
          else
             allocate (feyngraph_set%last%next)
             feyngraph_set%last => feyngraph_set%last%next
          endif
          feyngraph_set%last%root => node_ptr(i)%node
!!! The first particle was correct in the OMega parsable DAG output. It was however
!!! changed to its anti-particle in f_node_assign_particle_properties, which we revert here.
          feyngraph_set%last%root%particle => feyngraph_set%last%root%particle%anti
          feyngraph_set%last%n_nodes = feyngraph_set%last%root%n_subtree_nodes
          feyngraph_set%n_graphs = feyngraph_set%n_graphs + 1
       enddo
       deallocate (node_ptr)
       feyngraph_set%f_node_list%max_tree_size = feyngraph_set%first%n_nodes
    endif
  end subroutine dag_make_feyngraphs
  
  subroutine dag_write (dag, u)
    class (dag_t), intent (in) :: dag
    integer, intent(in) :: u
    integer :: i
    write (u,fmt='(A)') 'nodes'
    do i=1, dag%n_nodes
       write (u,fmt='(I5,3X,A)') i, char (dag%node(i)%string)
    enddo
    write (u,fmt='(A)') 'options'
    do i=1, dag%n_options
       write (u,fmt='(I5,3X,A)') i, char (dag%options(i)%string)
    enddo
    write (u,fmt='(A)') 'combination'
    do i=1, dag%n_combinations
       write (u,fmt='(I5,3X,A)') i, char (dag%combination(i)%string)
    enddo
  end subroutine dag_write

  subroutine k_node_make_nonresonant_copy (k_node)
    type (k_node_t), intent (in) :: k_node
    type (k_node_t), pointer :: copy => null ()
    call k_node%f_node%k_node_list%add_entry (copy, recycle=.true.)
    copy%daughter1 => k_node%daughter1
    copy%daughter2 => k_node%daughter2
    copy = k_node
    copy%mapping = NONRESONANT
    copy%resonant = .false.
    copy%on_shell = .false.
    copy%mapping_assigned = .true.
    copy%is_nonresonant_copy = .true.
  end subroutine k_node_make_nonresonant_copy
  
  subroutine feyngraph_make_kingraphs (feyngraph, feyngraph_set)
    class (feyngraph_t), intent (inout) :: feyngraph
    type (feyngraph_set_t), intent (in) :: feyngraph_set
    type (k_node_ptr_t), dimension (:), allocatable :: kingraph_root
    integer :: i
    if (.not. associated (feyngraph%kin_first)) then
       call k_node_init_from_f_node (feyngraph%root, &
            kingraph_root, feyngraph_set)
       if (.not. feyngraph%root%keep) return
       if (feyngraph_set%process_type == SCATTERING) then
          call split_up_t_lines (kingraph_root)
       endif
       do i=1, size (kingraph_root)
          if (associated (feyngraph%kin_last)) then
             allocate (feyngraph%kin_last%next)
             feyngraph%kin_last => feyngraph%kin_last%next
          else
             allocate (feyngraph%kin_last)
             feyngraph%kin_first => feyngraph%kin_last
          endif
          feyngraph%kin_last%root => kingraph_root(i)%node
          feyngraph%kin_last%n_nodes = feyngraph%n_nodes
          feyngraph%kin_last%keep = feyngraph%keep
          if (feyngraph_set%process_type == SCATTERING) then
             feyngraph%kin_last%root%bincode = &
                  f_node_get_external_bincode (feyngraph_set, feyngraph%root)
          endif
       enddo
       deallocate (kingraph_root)
    endif
  end subroutine feyngraph_make_kingraphs

  recursive subroutine k_node_init_from_f_node (f_node, k_node_ptr, feyngraph_set)
    type (f_node_t), target, intent (inout) :: f_node
    type (k_node_ptr_t), allocatable, dimension (:), intent (out) :: k_node_ptr
    type (feyngraph_set_t), intent (in) :: feyngraph_set
    type (k_node_ptr_t), allocatable, dimension(:) :: daughter_ptr1, daughter_ptr2
    integer :: n_nodes
    integer :: i, j
    integer :: pos
    integer, save :: counter = 0
    if (.not. (f_node%incoming .or. f_node%t_line)) then
       call f_node%k_node_list%get_nodes (k_node_ptr)
       if (.not. allocated (k_node_ptr) .and. f_node%k_node_list%n_entries > 0) then
          f_node%keep = .false.
          return
       endif
    endif
    if (.not. allocated (k_node_ptr)) then
       if (associated (f_node%daughter1) .and. associated (f_node%daughter2)) then
          call k_node_init_from_f_node (f_node%daughter1, daughter_ptr1, &
               feyngraph_set)
          call k_node_init_from_f_node (f_node%daughter2, daughter_ptr2, &
               feyngraph_set)
          if (.not. (f_node%daughter1%keep .and. f_node%daughter2%keep)) then
             f_node%keep = .false.
             return
          endif
          n_nodes = size (daughter_ptr1) * size (daughter_ptr2)
          allocate (k_node_ptr (n_nodes))
          pos = 1
          do i=1, size (daughter_ptr1)
             do j=1, size (daughter_ptr2)
                if (f_node%incoming .or. f_node%t_line) then
                   call f_node%k_node_list%add_entry (k_node_ptr(pos)%node, recycle = .false.)
                else
                   call f_node%k_node_list%add_entry (k_node_ptr(pos)%node, recycle = .true.)
                endif
                k_node_ptr(pos)%node%f_node => f_node
                k_node_ptr(pos)%node%daughter1 => daughter_ptr1(i)%node
                k_node_ptr(pos)%node%daughter2 => daughter_ptr2(j)%node
                k_node_ptr(pos)%node%f_node_index = f_node%index
                k_node_ptr(pos)%node%incoming = f_node%incoming
                k_node_ptr(pos)%node%t_line = f_node%t_line
                k_node_ptr(pos)%node%particle => f_node%particle
                pos = pos + 1
             enddo
          enddo
          deallocate (daughter_ptr1, daughter_ptr2)
       else
          allocate (k_node_ptr(1))
          if (f_node%incoming .or. f_node%t_line) then
             call f_node%k_node_list%add_entry (k_node_ptr(1)%node, recycle=.false.)
          else
             call f_node%k_node_list%add_entry (k_node_ptr(1)%node, recycle=.true.)
          endif
          k_node_ptr(1)%node%f_node => f_node
          k_node_ptr(1)%node%f_node_index = f_node%index
          k_node_ptr(1)%node%incoming = f_node%incoming
          k_node_ptr(1)%node%t_line = f_node%t_line
          k_node_ptr(1)%node%particle => f_node%particle
          k_node_ptr(1)%node%bincode = f_node_get_external_bincode (feyngraph_set, &
               f_node)
       endif
    endif
  end subroutine k_node_init_from_f_node

  recursive subroutine split_up_t_lines (t_node)
    type (k_node_ptr_t), dimension(:), intent (inout) :: t_node
    type (k_node_t), pointer :: ref_node => null ()
    type (k_node_t), pointer :: ref_daughter => null ()
    type (k_node_t), pointer :: new_daughter => null ()
    type (k_node_ptr_t), dimension(:), allocatable :: t_daughter
    integer :: ref_daughter_index
    integer :: i, j
    allocate (t_daughter (size (t_node)))
    do i=1, size (t_node)
       ref_node => t_node(i)%node
       if (associated (ref_node%daughter1) .and. associated (ref_node%daughter2)) then
          ref_daughter => null ()
          if (ref_node%daughter1%incoming .or. ref_node%daughter1%t_line) then
             ref_daughter => ref_node%daughter1
             ref_daughter_index = 1
          else if (ref_node%daughter2%incoming .or. ref_node%daughter2%t_line) then
             ref_daughter => ref_node%daughter2
             ref_daughter_index = 2
          endif
          do j=1, size (t_daughter)
             if (.not. associated (t_daughter(j)%node)) then
                t_daughter(j)%node => ref_daughter
                exit
             else if (t_daughter(j)%node%index == ref_daughter%index) then
                new_daughter => null ()
                call ref_daughter%f_node%k_node_list%add_entry (new_daughter, recycle=.false.)
                new_daughter = ref_daughter
                new_daughter%daughter1 => ref_daughter%daughter1
                new_daughter%daughter2 => ref_daughter%daughter2
                if (ref_daughter_index == 1) then
                   ref_node%daughter1 => new_daughter
                else if (ref_daughter_index == 2) then
                   ref_node%daughter2 => new_daughter
                endif
                ref_daughter => new_daughter
             endif
          enddo
       else
          return
       endif
    enddo
    call split_up_t_lines (t_daughter)
    deallocate (t_daughter)
  end subroutine split_up_t_lines

  subroutine kingraph_set_inverse_daughters (kingraph)
    type (kingraph_t), intent (inout) :: kingraph
    type (k_node_t), pointer :: mother => null ()
    type (k_node_t), pointer :: t_daughter => null ()
    type (k_node_t), pointer :: s_daughter => null ()
    mother => kingraph%root
    do while (associated (mother))
       if (associated (mother%daughter1) .and. &
            associated (mother%daughter2)) then
          if (mother%daughter1%t_line .or. mother%daughter1%incoming) then
             t_daughter => mother%daughter1; s_daughter => mother%daughter2
          else if (mother%daughter2%t_line .or. mother%daughter2%incoming) then
             t_daughter => mother%daughter2; s_daughter => mother%daughter1
          else
             exit
          endif
          t_daughter%inverse_daughter1 => mother
          t_daughter%inverse_daughter2 => s_daughter
          mother => t_daughter
       else
          exit
       endif
    enddo
  end subroutine kingraph_set_inverse_daughters

  function f_node_get_external_bincode (feyngraph_set, f_node) result (bincode)
    type (feyngraph_set_t), intent (in) :: feyngraph_set
    type (f_node_t), intent (in) :: f_node
    integer (TC) :: bincode
    character (len=LABEL_LEN) :: particle_label
    integer :: start_pos, end_pos, n_out_decay
    integer :: n_prt ! for DAG
    integer :: i
    bincode = 0
    if (feyngraph_set%process_type == DECAY) then
       n_out_decay = feyngraph_set%n_out
    else
       n_out_decay = feyngraph_set%n_out + 1
    endif
    particle_label = f_node%particle_label
    start_pos = index (particle_label, '[') + 1
    end_pos = index (particle_label, ']') - 1
    particle_label = particle_label(start_pos:end_pos)
!!! n_out_decay is the number of outgoing particles in the
!!! OMega output, which is always represented as a decay
    if (feyngraph_set%use_dag) then
       n_prt = 1
       do i=1, len(particle_label)
          if (particle_label(i:i) == '/') n_prt = n_prt + 1
       enddo
    else
       n_prt = end_pos - start_pos + 1
    endif
    if (n_prt == 1) then
       bincode = calculate_external_bincode (particle_label, &
            feyngraph_set%process_type, n_out_decay)
    else if (n_prt == n_out_decay) then
       bincode = ibset (0, n_out_decay)
    endif
  end function f_node_get_external_bincode

  subroutine node_assign_bincode (node)
    type (k_node_t), intent (inout) :: node
    if (associated (node%daughter1) .and. associated (node%daughter2) &
         .and. .not. node%incoming) then
       node%bincode = ior(node%daughter1%bincode, node%daughter2%bincode)
    endif
  end subroutine node_assign_bincode

  function calculate_external_bincode (label_number_string, process_type, n_out_decay) result (bincode)
    character (len=*), intent (in) :: label_number_string
    integer, intent (in) :: process_type
    integer, intent (in) :: n_out_decay
    character :: number_char
    integer :: number_int
    integer (kind=TC) :: bincode
    bincode = 0
    read (label_number_string, fmt='(A)') number_char
!!! check if the character is a letter (A,B,C,...) or a number (1...9)
!!! numbers 1 and 2 are special cases
    select case (number_char)
    case ('1')
       if (process_type == SCATTERING) then
          number_int = n_out_decay + 3
       else
          number_int = n_out_decay + 2
       endif
    case ('2')
       if (process_type == SCATTERING) then
          number_int = n_out_decay + 2
       else
          number_int = 2
       endif
    case ('A')
       number_int = 10
    case ('B')
       number_int = 11
    case ('C')
       number_int = 12
    case ('D')
       number_int = 13
    case default
       read (number_char, fmt='(I1)') number_int
    end select
    bincode = ibset (bincode, number_int - process_type - 1)
  end function calculate_external_bincode

  subroutine node_assign_mapping_s (feyngraph, node, feyngraph_set)
    type (feyngraph_t), intent (inout) :: feyngraph
    type (k_node_t), intent (inout) :: node
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    real(default) :: eff_mass_sum
    logical :: keep
    if (.not. node%mapping_assigned) then
       if (node%particle%mass > feyngraph_set%phs_par%m_threshold_s) then
          node%effective_mass = node%particle%mass
       endif
       if (associated (node%daughter1) .and. associated (node%daughter2)) then
          if (.not. (node%daughter1%keep .and. node%daughter2%keep)) then
             node%keep = .false.; return
          endif
          node%ext_mass_sum = node%daughter1%ext_mass_sum &
               + node%daughter2%ext_mass_sum
          keep = .false.
!!! Potentially resonant cases [sqrts = m_rea for on-shell decay]
          if (node%particle%mass > node%ext_mass_sum &
               .and. node%particle%mass <= feyngraph_set%phs_par%sqrts) then
             if (node%particle%width /= 0) then
                if (node%daughter1%on_shell .or. node%daughter2%on_shell) then
                   keep = .true.
                   node%mapping = S_CHANNEL
                   node%resonant = .true.
                endif
             else
                call warn_decay (node%particle)
             endif
!!! Collinear and IR singular cases
          else if (node%particle%mass < feyngraph_set%phs_par%sqrts) then
!!! Massless splitting
             if (node%daughter1%effective_mass == 0 &
                  .and. node%daughter2%effective_mass == 0 &
                  .and. .not. associated (node%daughter1%daughter1) &
                  .and. .not. associated (node%daughter1%daughter2) &
                  .and. .not. associated (node%daughter2%daughter1) &
                  .and. .not. associated (node%daughter2%daughter2)) then
                keep = .true.
                node%log_enhanced = .true.
                if (node%particle%is_vector) then
                   if (node%daughter1%particle%is_vector &
                        .and. node%daughter2%particle%is_vector) then
                      node%mapping = COLLINEAR   !!! three-vector-splitting
                   else
                      node%mapping = INFRARED    !!! vector spliiting into matter
                   endif
                else
                   if (node%daughter1%particle%is_vector &
                        .or. node%daughter2%particle%is_vector) then
                      node%mapping = COLLINEAR   !!! vector radiation off matter
                   else
                      node%mapping = INFRARED    !!! scalar radiation/splitting
                   endif
                endif
!!! IR radiation off massive particle [cascades]
             else if (node%effective_mass > 0 .and. &
                  node%daughter1%effective_mass > 0 .and. &
                  node%daughter2%effective_mass == 0 .and. &
                  (node%daughter1%on_shell .or. &
                  node%daughter1%mapping == RADIATION) .and. &
                  abs (node%effective_mass - &
                  node%daughter1%effective_mass) < feyngraph_set%phs_par%m_threshold_s) &
                  then
                keep = .true.
                node%log_enhanced = .true.
                node%mapping = RADIATION
             else if (node%effective_mass > 0 .and. &
                  node%daughter2%effective_mass > 0 .and. &
                  node%daughter1%effective_mass == 0 .and. &
                  (node%daughter2%on_shell .or. &
                  node%daughter2%mapping == RADIATION) .and. &
                  abs (node%effective_mass - &
                  node%daughter2%effective_mass) < feyngraph_set%phs_par%m_threshold_s) &
                  then
                keep = .true.
                node%log_enhanced = .true.
                node%mapping = RADIATION
             endif
          endif
!!! Non-singular cases, including failed resonances [from cascades]
          if (.not. keep) then
!!! Two on-shell particles from a virtual mother [from cascades, here eventually more than 2]
             if (node%daughter1%on_shell .or. node%daughter2%on_shell) then
                keep = .true.
                eff_mass_sum = node%daughter1%effective_mass &
                     + node%daughter2%effective_mass
                node%effective_mass = max (node%ext_mass_sum, eff_mass_sum)
                if (node%effective_mass < feyngraph_set%phs_par%m_threshold_s) then
                   node%effective_mass = 0
                endif
             endif
          endif
!!! Complete and register feyngraph (make copy in case of resonance)
          if (keep) then
             node%on_shell = node%resonant .or. node%log_enhanced
             if (node%resonant) then
                if (feyngraph_set%phs_par%keep_nonresonant) then
                   !$OMP CRITICAL (make_nonres_copy)
                   call k_node_make_nonresonant_copy (node)
                   !$OMP END CRITICAL (make_nonres_copy)
                endif
                node%ext_mass_sum = node%particle%mass
             endif
          endif
          node%mapping_assigned = .true.
          call node_assign_bincode (node)
          call node%subtree%add_entry (node)
       else !!! external (outgoing) particle
          node%ext_mass_sum = node%particle%mass
          node%mapping = EXTERNAL_PRT
          node%multiplicity = 1
          node%mapping_assigned = .true.
          call node%subtree%add_entry (node)
          node%on_shell = .true.
          if (node%particle%mass >= feyngraph_set%phs_par%m_threshold_s) then
             node%effective_mass = node%particle%mass
          endif
       endif
    else if (node%is_nonresonant_copy) then
       call node_assign_bincode (node)
       call node%subtree%add_entry (node)
       node%is_nonresonant_copy = .false.
    endif
    call node_count_specific_properties (node)
    if (node%n_off_shell > feyngraph_set%phs_par%off_shell) then
       node%keep = .false.
    endif
  contains
    subroutine warn_decay (particle)
      type(part_prop_t), intent(in) :: particle
      integer :: i
      integer, dimension(MAX_WARN_RESONANCE), save :: warned_code = 0
      LOOP_WARNED: do i = 1, MAX_WARN_RESONANCE
         if (warned_code(i) == 0) then
            warned_code(i) = particle%pdg
            write (msg_buffer, "(A)") &
                 & " Intermediate decay of zero-width particle " &
                 & // trim(particle%particle_label) &
                 & // " may be possible."
            call msg_warning
            exit LOOP_WARNED
         else if (warned_code(i) == particle%pdg) then
            exit LOOP_WARNED
         end if
      end do LOOP_WARNED
    end subroutine warn_decay
  end subroutine node_assign_mapping_s

  subroutine node_count_specific_properties (node)
    type (k_node_t), intent (inout) :: node
    if (associated (node%daughter1) .and. associated(node%daughter2)) then
       if (node%resonant) then
          node%multiplicity = 1
          node%n_resonances &
               = node%daughter1%n_resonances &
               + node%daughter2%n_resonances + 1
       else
          node%multiplicity &
               = node%daughter1%multiplicity &
               + node%daughter2%multiplicity
          node%n_resonances &
               = node%daughter1%n_resonances &
               + node%daughter2%n_resonances
       endif
       if (node%log_enhanced) then
          node%n_log_enhanced &
               = node%daughter1%n_log_enhanced &
               + node%daughter2%n_log_enhanced + 1
       else
          node%n_log_enhanced &
               = node%daughter1%n_log_enhanced &
               + node%daughter2%n_log_enhanced
       endif
       if (node%resonant) then
          node%n_off_shell = 0
       else if (node%log_enhanced) then
          node%n_off_shell &
               = node%daughter1%n_off_shell &
               + node%daughter2%n_off_shell
       else
          node%n_off_shell &
               = node%daughter1%n_off_shell &
               + node%daughter2%n_off_shell + 1
       endif
       if (node%t_line) then
          if (node%daughter1%t_line .or. node%daughter1%incoming) then
             node%n_t_channel = node%daughter1%n_t_channel + 1
          else if (node%daughter2%t_line .or. node%daughter2%incoming) then
             node%n_t_channel = node%daughter2%n_t_channel + 1
          endif
       endif
    endif
  end subroutine node_count_specific_properties

  subroutine kingraph_assign_mappings_s (feyngraph, kingraph, feyngraph_set)
    type (feyngraph_t), intent (inout) :: feyngraph
    type (kingraph_t), pointer, intent (inout) :: kingraph
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    if (.not. (kingraph%root%daughter1%keep .and. kingraph%root%daughter2%keep)) then
       kingraph%keep = .false.
    endif
    if (kingraph%keep) then
       kingraph%root%on_shell = .true.
       kingraph%root%mapping = EXTERNAL_PRT
       kingraph%root%mapping_assigned = .true.
       call node_assign_bincode (kingraph%root)
       kingraph%root%ext_mass_sum = &
            kingraph%root%daughter1%ext_mass_sum + &
            kingraph%root%daughter2%ext_mass_sum
       if (kingraph%root%ext_mass_sum >= feyngraph_set%phs_par%sqrts) then
          kingraph%root%keep = .false.; kingraph%keep = .false.; return
       endif
       call kingraph%root%subtree%add_entry (kingraph%root)
       kingraph%root%multiplicity &
            = kingraph%root%daughter1%multiplicity &
            + kingraph%root%daughter2%multiplicity
       kingraph%root%n_resonances &
            = kingraph%root%daughter1%n_resonances &
            + kingraph%root%daughter2%n_resonances
       kingraph%root%n_off_shell &
            = kingraph%root%daughter1%n_off_shell &
            + kingraph%root%daughter2%n_off_shell
       kingraph%root%n_log_enhanced &
            = kingraph%root%daughter1%n_log_enhanced &
            + kingraph%root%daughter2%n_log_enhanced
       if (kingraph%root%n_off_shell > feyngraph_set%phs_par%off_shell) then
          kingraph%root%keep = .false.; kingraph%keep = .false.; return
       else
          kingraph%grove_prop%multiplicity = &
               kingraph%root%multiplicity
          kingraph%grove_prop%n_resonances = &
               kingraph%root%n_resonances
          kingraph%grove_prop%n_off_shell = &
               kingraph%root%n_off_shell
          kingraph%grove_prop%n_log_enhanced = &
               kingraph%root%n_log_enhanced
       endif
       kingraph%tree = kingraph%root%subtree
    endif
  end subroutine kingraph_assign_mappings_s

  subroutine kingraph_compute_mappings_t_line (feyngraph, kingraph, feyngraph_set)
    type (feyngraph_t), intent (inout) :: feyngraph
    type (kingraph_t), pointer, intent (inout) :: kingraph
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    call node_compute_t_line (feyngraph, kingraph, kingraph%root, feyngraph_set)
    if (.not. kingraph%root%keep) kingraph%keep = .false.
    if (kingraph%keep) kingraph%tree = kingraph%root%subtree
  end subroutine kingraph_compute_mappings_t_line

  recursive subroutine node_compute_t_line (feyngraph, kingraph, node, feyngraph_set)
    type (feyngraph_t), intent (inout) :: feyngraph
    type (kingraph_t), intent (inout) :: kingraph
    type (k_node_t), intent (inout) :: node
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (k_node_t), pointer :: s_node
    type (k_node_t), pointer :: t_node
    type (k_node_t), pointer :: new_s_node
    if (.not. (node%daughter1%keep .and. node%daughter2%keep)) then
       node%keep = .false.
       return
    endif
    s_node => null ()
    t_node => null ()
    new_s_node => null ()
    if (associated (node%daughter1) .and. associated (node%daughter2)) then
       if (node%daughter1%t_line .or. node%daughter1%incoming) then
          t_node => node%daughter1; s_node => node%daughter2
       else if (node%daughter2%t_line .or. node%daughter2%incoming) then
          t_node => node%daughter2; s_node => node%daughter1
       endif
       if (t_node%t_line) then
          call node_compute_t_line (feyngraph, kingraph, t_node, feyngraph_set)
          if (.not. t_node%keep) then
             node%keep = .false.
             return
          endif
       else if (t_node%incoming) then
          t_node%mapping = EXTERNAL_PRT
          t_node%on_shell = .true.
          t_node%ext_mass_sum = t_node%particle%mass
          if (t_node%particle%mass >= feyngraph_set%phs_par%m_threshold_t) then
             t_node%effective_mass = t_node%particle%mass
          endif
          call t_node%subtree%add_entry (t_node)
       endif
!!! root:
       if (.not. node%incoming) then
          if (t_node%incoming) then
             node%ext_mass_sum = s_node%ext_mass_sum
          else
             node%ext_mass_sum &
                  = node%daughter1%ext_mass_sum &
                  + node%daughter2%ext_mass_sum
          endif
          if (node%particle%mass > feyngraph_set%phs_par%m_threshold_t) then
             node%effective_mass = max (node%particle%mass, &
                  s_node%effective_mass)
          else if (s_node%effective_mass > feyngraph_set%phs_par%m_threshold_t) then
             node%effective_mass = s_node%effective_mass
          else
             node%effective_mass = 0
          endif
!!! Allowed decay of beam particle
          if (t_node%incoming &
               .and. t_node%particle%mass > s_node%particle%mass &
               + node%particle%mass) then
             call beam_decay (feyngraph_set%fatal_beam_decay)
!!! Massless splitting
          else if (t_node%effective_mass == 0 &
               .and. s_node%effective_mass < feyngraph_set%phs_par%m_threshold_t &
               .and. node%effective_mass == 0) then
             node%mapping = U_CHANNEL
             node%log_enhanced = .true.
!!! IR radiation off massive particle
          else if (t_node%effective_mass /= 0 &
               .and. s_node%effective_mass == 0 &
               .and. node%effective_mass /= 0 &
               .and. (t_node%on_shell &
               .or. t_node%mapping == RADIATION) &
               .and. abs (t_node%effective_mass - node%effective_mass) &
               < feyngraph_set%phs_par%m_threshold_t) then
             node%log_enhanced = .true.
             node%mapping = RADIATION
          endif
          node%mapping_assigned = .true.
          call node_assign_bincode (node)
          call node%subtree%add_entry (node)
          call node_count_specific_properties (node)
          if (node%n_off_shell > feyngraph_set%phs_par%off_shell) then
             node%keep = .false.; kingraph%keep = .false.; return
          else if (node%n_t_channel > feyngraph_set%phs_par%t_channel) then
             node%keep = .false.; kingraph%keep = .false.; return
          endif
       else
          node%mapping = EXTERNAL_PRT
          node%on_shell = .true.
          node%ext_mass_sum &
               = t_node%ext_mass_sum &
               + s_node%ext_mass_sum
          node%effective_mass = node%particle%mass
          if (.not. (node%ext_mass_sum < feyngraph_set%phs_par%sqrts)) then
             node%keep = .false.; kingraph%keep = .false.; return
          endif
          if (kingraph%keep) then
             if (t_node%incoming .and. s_node%log_enhanced) then
                call s_node%f_node%k_node_list%add_entry (new_s_node, recycle=.false.)
                new_s_node = s_node
                new_s_node%daughter1 => s_node%daughter1
                new_s_node%daughter2 => s_node%daughter2
                if (s_node%index == node%daughter1%index) then
                   node%daughter1 => new_s_node
                else if (s_node%index ==  node%daughter2%index) then
                   node%daughter2 => new_s_node
                endif
                new_s_node%subtree = s_node%subtree
                new_s_node%mapping = NO_MAPPING
                new_s_node%log_enhanced = .false.
                new_s_node%n_log_enhanced &
                     = new_s_node%n_log_enhanced - 1
                new_s_node%log_enhanced = .false.
                where (new_s_node%subtree%bc == new_s_node%bincode)
                   new_s_node%subtree%mapping = NO_MAPPING
                endwhere
             else if ((t_node%t_line .or. t_node%incoming) .and. &
                  t_node%mapping == U_CHANNEL) then
                t_node%mapping = T_CHANNEL
                where (t_node%subtree%bc == t_node%bincode)
                   t_node%subtree%mapping = T_CHANNEL
                endwhere
             else if (t_node%incoming .and. &
                  .not. associated (s_node%daughter1) .and. &
                  .not. associated (s_node%daughter2)) then
                call s_node%f_node%k_node_list%add_entry (new_s_node, recycle=.false.)
                new_s_node = s_node
                new_s_node%mapping = ON_SHELL
                new_s_node%daughter1 => s_node%daughter1
                new_s_node%daughter2 => s_node%daughter2
                new_s_node%subtree = s_node%subtree
                if (s_node%index == node%daughter1%index) then
                   node%daughter1 => new_s_node
                else if (s_node%index == node%daughter2%index) then
                   node%daughter2 => new_s_node
                endif
                where (new_s_node%subtree%bc == new_s_node%bincode)
                   new_s_node%subtree%mapping = ON_SHELL
                endwhere
             endif
          endif
          call node%subtree%add_entry (node)
          node%multiplicity &
               = node%daughter1%multiplicity &
               + node%daughter2%multiplicity
          node%n_resonances &
               = node%daughter1%n_resonances &
               + node%daughter2%n_resonances
          node%n_off_shell &
               = node%daughter1%n_off_shell &
               + node%daughter2%n_off_shell
          node%n_log_enhanced &
               = node%daughter1%n_log_enhanced &
               + node%daughter2%n_log_enhanced
          node%n_t_channel &
               = node%daughter1%n_t_channel &
               + node%daughter2%n_t_channel
          if (node%n_off_shell > feyngraph_set%phs_par%off_shell) then
             node%keep = .false.; kingraph%keep = .false.; return
          else if (node%n_t_channel > feyngraph_set%phs_par%t_channel) then
             node%keep = .false.; kingraph%keep = .false.; return
          else
             kingraph%grove_prop%multiplicity = node%multiplicity
             kingraph%grove_prop%n_resonances = node%n_resonances
             kingraph%grove_prop%n_off_shell = node%n_off_shell
             kingraph%grove_prop%n_log_enhanced = node%n_log_enhanced
             kingraph%grove_prop%n_t_channel = node%n_t_channel
          endif
       endif
    endif
  contains
    subroutine beam_decay (fatal_beam_decay)
      logical, intent(in) :: fatal_beam_decay
      write (msg_buffer, "(1x,A,1x,'->',1x,A,1x,A)") &
           t_node%particle%particle_label, &
           node%particle%particle_label, &
           s_node%particle%particle_label
      call msg_message
      write (msg_buffer, "(1x,'mass(',A,') =',1x,E17.10)") &
           t_node%particle%particle_label, t_node%particle%mass
      call msg_message
      write (msg_buffer, "(1x,'mass(',A,') =',1x,E17.10)") &
           node%particle%particle_label, node%particle%mass
      call msg_message
      write (msg_buffer, "(1x,'mass(',A,') =',1x,E17.10)") &
           s_node%particle%particle_label, s_node%particle%mass
      call msg_message
      if (fatal_beam_decay) then
         call msg_fatal (" Phase space: Initial beam particle can decay")
      else
         call msg_warning (" Phase space: Initial beam particle can decay")
      end if
    end subroutine beam_decay
  end subroutine node_compute_t_line

  subroutine feyngraph_compute_mappings (feyngraph, feyngraph_set)
    class (feyngraph_t), intent (inout) :: feyngraph
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (kingraph_t), pointer :: current
    call feyngraph%make_kingraphs (feyngraph_set)
    current => feyngraph%kin_first
    do while (associated (current))
       if (feyngraph_set%process_type == DECAY) then
          call kingraph_assign_mappings_s (feyngraph, current, feyngraph_set)
       else if (feyngraph_set%process_type == SCATTERING) then
          if (.not. current%inverse) then
             call current%make_inverse_copy (feyngraph)
          endif
          call kingraph_compute_mappings_t_line (feyngraph, current, feyngraph_set)
       endif
       current => current%next
    enddo
  end subroutine feyngraph_compute_mappings

  subroutine f_node_list_compute_mappings_s (feyngraph_set)
    type (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (f_node_ptr_t), dimension(:), allocatable :: set
    type (k_node_ptr_t), dimension(:), allocatable :: k_set
    type (k_node_entry_t), pointer :: k_entry
    type (f_node_entry_t), pointer :: current
    type (k_node_list_t), allocatable :: compare_list
    integer :: n_entries
    integer :: pos
    integer :: i, j
    do i = 1, feyngraph_set%f_node_list%max_tree_size - 2, 2
       current => feyngraph_set%f_node_list%first
       n_entries = 0
       do while (associated (current))
          if (.not. (current%node%incoming .or. current%node%t_line) &
               .and. current%node%n_subtree_nodes == i) then
             n_entries = n_entries + 1
          endif
          current => current%next
       enddo
       if (n_entries == 0) exit
       allocate (set(n_entries))
       current => feyngraph_set%f_node_list%first
       pos = 0
       do while (associated (current))
          if (.not. (current%node%incoming .or. current%node%t_line) &
               .and. current%node%n_subtree_nodes == i) then
             pos = pos + 1
             set(pos)%node => current%node
          endif
          current => current%next
       enddo
       allocate (compare_list)
       compare_list%observer = .true.
       !$OMP PARALLEL DO PRIVATE (k_set, k_entry)
       do j = 1, n_entries
          call k_node_init_from_f_node (set(j)%node, k_set, &
               feyngraph_set)
          if (allocated (k_set)) then
             k_entry => set(j)%node%k_node_list%first
             do while (associated (k_entry))
                call node_assign_mapping_s(feyngraph_set%first, k_entry%node, feyngraph_set)
                k_entry => k_entry%next
             enddo
             deallocate (k_set)
          endif
       enddo
       !$OMP END PARALLEL DO
       do j = 1, size (set)
          k_entry => set(j)%node%k_node_list%first
          do while (associated (k_entry))
             if (k_entry%node%keep) then
                if (k_entry%node%mapping == NO_MAPPING .or. k_entry%node%mapping == NONRESONANT) then
                   call compare_list%add_pointer (k_entry%node)
                endif
             endif
             k_entry => k_entry%next
          enddo
       enddo
       deallocate (set)
       call compare_list%check_subtree_equivalences(feyngraph_set%model)
       call compare_list%final
       deallocate (compare_list)
    enddo
  end subroutine f_node_list_compute_mappings_s
  
  subroutine grove_list_get_grove (grove_list, kingraph, return_grove, preliminary)
    class (grove_list_t), intent (inout) :: grove_list
    type (kingraph_t), intent (in), pointer :: kingraph
    type (grove_t), intent (inout), pointer :: return_grove
    logical, intent (in) :: preliminary
    type (grove_t), pointer :: current_grove
    if (.not. associated(grove_list%first)) then
       !$ call OMP_set_lock (grove_list%lock)
       if (.not. associated (grove_list%first)) then
          allocate (grove_list%first)
          !$ call OMP_init_lock (grove_list%first%lock)
          grove_list%first%grove_prop = kingraph%grove_prop
          return_grove => grove_list%first
          !$ call OMP_unset_lock (grove_list%lock)
          return
       else
          !$ call OMP_unset_lock (grove_list%lock)
       endif
    endif
    current_grove => grove_list%first
    do while (associated (current_grove))
       if ((preliminary .and. (current_grove%grove_prop .match. kingraph%grove_prop)) .or. &
            (.not. preliminary .and. current_grove%grove_prop == kingraph%grove_prop)) then
          return_grove => current_grove
          exit
       else if (.not. associated (current_grove%next)) then
          !$ call OMP_set_lock (current_grove%lock)
          if (.not. associated (current_grove%next)) then
             allocate (current_grove%next)
             !$ call OMP_init_lock (current_grove%next%lock)
             current_grove%next%grove_prop = kingraph%grove_prop
             return_grove => current_grove%next
             !$ call OMP_unset_lock (current_grove%lock)
             exit
          else
             !$ call OMP_unset_lock (current_grove%lock)
          endif
       endif
       if (associated (current_grove%next)) then
          current_grove => current_grove%next
       endif
    enddo
  end subroutine grove_list_get_grove

  subroutine grove_list_add_kingraph (grove_list, kingraph, preliminary, model)
    class (grove_list_t), intent (inout) :: grove_list
    type (kingraph_t), pointer, intent (inout) :: kingraph
    logical, intent (in) :: preliminary
    type (model_data_t), optional, intent (in) :: model
    type (grove_t), pointer :: grove
    type (kingraph_t), pointer :: current
    integer, save :: index = 0
    grove => null ()
    current => null ()
    if (preliminary) then
       !$OMP CRITICAL (increase_index)
       index = index + 1
       !$OMP END CRITICAL (increase_index)
       kingraph%index = index
    else
       call kingraph%assign_resonance_hash ()
    endif
    call grove_list%get_grove (kingraph, grove, preliminary)
    !$ call OMP_set_lock (grove%lock)
    if (associated (grove%first)) then
       current => grove%first
       do while (associated (current))
          if (preliminary .and. current%keep) then
             call kingraph_select (current, kingraph, model)
             if (.not. kingraph%keep) exit
          endif
          if (associated (current%grove_next)) then
             current => current%grove_next
          else
             current%grove_next => kingraph
             exit
          endif
       enddo
    else
       grove%first => kingraph
    endif
    !$ call OMP_unset_lock (grove%lock)
  end subroutine grove_list_add_kingraph

  subroutine grove_list_add_feyngraph (grove_list, feyngraph, model)
    class (grove_list_t), intent (inout) :: grove_list
    type (feyngraph_t), intent (in) :: feyngraph
    type (model_data_t), intent (in) :: model
    type (kingraph_t), pointer :: current_kin_graph
    current_kin_graph => feyngraph%kin_first
    do while (associated (current_kin_graph))
       if (current_kin_graph%keep) then
          call grove_list%add_kingraph (current_kin_graph, &
               .true., model)
       endif
       current_kin_graph => current_kin_graph%next
    enddo
  end subroutine grove_list_add_feyngraph

  function grove_prop_match (grove_prop1, grove_prop2) result (gp_match)
    type (grove_prop_t), intent (in) :: grove_prop1
    type (grove_prop_t), intent (in) :: grove_prop2
    logical :: gp_match
    gp_match = (grove_prop1%n_resonances == grove_prop2%n_resonances) &
         .and. (grove_prop1%n_log_enhanced == grove_prop2%n_log_enhanced) &
         .and. (grove_prop1%n_t_channel == grove_prop2%n_t_channel)
  end function grove_prop_match

  function grove_prop_equal (grove_prop1, grove_prop2) result (gp_equal)
    type (grove_prop_t), intent (in) :: grove_prop1
    type (grove_prop_t), intent (in) :: grove_prop2
    logical :: gp_equal
    gp_equal = (grove_prop1%res_hash == grove_prop2%res_hash) &
         .and. (grove_prop1%n_resonances == grove_prop2%n_resonances) &
         .and. (grove_prop1%n_log_enhanced == grove_prop2%n_log_enhanced) &
         .and. (grove_prop1%n_off_shell == grove_prop2%n_off_shell) &
         .and. (grove_prop1%multiplicity == grove_prop2%multiplicity) &
         .and. (grove_prop1%n_t_channel == grove_prop2%n_t_channel)
  end function grove_prop_equal

  function kingraph_eqv (kingraph1, kingraph2) result (eqv)
    type (kingraph_t), intent (in) :: kingraph1
    type (kingraph_t), intent (inout) :: kingraph2
    logical :: eqv
    integer :: i
    logical :: equal
    eqv = .false.
    do i = kingraph1%tree%n_entries, 1, -1
       if (kingraph1%tree%bc(i) /= kingraph2%tree%bc(i)) return
    enddo
    do i = kingraph1%tree%n_entries, 1, -1
       if ( .not. (kingraph1%tree%mapping(i) == kingraph2%tree%mapping(i) &
            .or. ((kingraph1%tree%mapping(i) == NO_MAPPING .or. &
            kingraph1%tree%mapping(i) == NONRESONANT) .and. &
            (kingraph2%tree%mapping(i) == NO_MAPPING .or. &
            kingraph2%tree%mapping(i) == NONRESONANT)))) return
    enddo
    equal = .true.
    do i = kingraph1%tree%n_entries, 1, -1
       if (abs(kingraph1%tree%pdg(i)) /= abs(kingraph2%tree%pdg(i))) then
          equal = .false.;
          select case (kingraph1%tree%mapping(i))
          case (S_CHANNEL, RADIATION, EXTERNAL_PRT)
             select case (kingraph2%tree%mapping(i))
             case (S_CHANNEL, RADIATION, EXTERNAL_PRT)
                return
             end select
          end select
       endif
    enddo
    if (equal) then
       kingraph2%keep = .false.
    else
       eqv = .true.
    endif
  end function kingraph_eqv

  subroutine kingraph_select (kingraph1, kingraph2, model)
    type (kingraph_t), intent (inout) :: kingraph1
    type (kingraph_t), intent (inout) :: kingraph2
    type (model_data_t), intent (in) :: model
    integer(TC), dimension(:), allocatable :: tmp_bc, daughter_bc
    integer, dimension(:), allocatable :: tmp_pdg, daughter_pdg
    integer, dimension (:), allocatable :: pdg_match
    integer :: i, j
    integer :: n_ext1, n_ext2
    if (kingraph_eqv (kingraph1, kingraph2)) then
       do i=1, size (kingraph1%tree%bc)
          if (abs(kingraph1%tree%pdg(i)) /= abs(kingraph2%tree%pdg(i))) then
             n_ext1 = popcnt (kingraph1%tree%bc(i))
             n_ext2 = n_ext1
             do j=i+1, size (kingraph1%tree%bc)
                if (abs(kingraph1%tree%pdg(j)) /= abs(kingraph2%tree%pdg(j))) then
                   n_ext2 = popcnt (kingraph1%tree%bc(j))
                   if (n_ext2 < n_ext1) exit
                endif
             enddo
             if (n_ext2 < n_ext1) cycle
             allocate (tmp_bc(i-1))
             tmp_bc = kingraph1%tree%bc(:i-1)
             allocate (tmp_pdg(i-1))
             tmp_pdg = kingraph1%tree%pdg(:i-1)
             do j=i-1, 1, - 1
                where (iand (tmp_bc(:j-1),tmp_bc(j)) /= 0 &
                     .or. iand(tmp_bc(:j-1),kingraph1%tree%bc(i)) == 0)
                   tmp_bc(:j-1) = 0
                   tmp_pdg(:j-1) = 0
                endwhere
             enddo
             allocate (daughter_bc(size(pack(tmp_bc, tmp_bc /= 0))))
             daughter_bc = pack (tmp_bc, tmp_bc /= 0)
             allocate (daughter_pdg(size(pack(tmp_pdg, tmp_pdg /= 0))))
             daughter_pdg = pack (tmp_pdg, tmp_pdg /= 0)
             if (size (daughter_pdg) == 2) then
                call model%match_vertex(daughter_pdg(1), daughter_pdg(2), pdg_match)
             endif
             do j=1, size (pdg_match)
                if (abs(pdg_match(j)) == abs(kingraph1%tree%pdg(i))) then
                   kingraph2%keep = .false.
                   exit
                else if (abs(pdg_match(j)) == abs(kingraph2%tree%pdg(i))) then
                   kingraph1%keep = .false.
                   exit
                endif
             enddo
             deallocate (tmp_bc, tmp_pdg, daughter_bc, daughter_pdg, pdg_match)
             if (.not. (kingraph1%keep .and. kingraph2%keep)) exit
          endif
       enddo
    endif
  end subroutine kingraph_select

  subroutine grove_list_rebuild (grove_list)
    class (grove_list_t), intent (inout) :: grove_list
    type (grove_list_t) :: tmp_list
    type (grove_t), pointer :: current_grove
    type (grove_t), pointer :: remove_grove
    type (kingraph_t), pointer :: current_graph
    type (kingraph_t), pointer :: next_graph
    !$ call OMP_init_lock (grove_list%lock)
    tmp_list%first => grove_list%first
    grove_list%first => null ()
    current_grove => tmp_list%first
    do while (associated (current_grove))
       current_graph => current_grove%first
       do while (associated (current_graph))
          next_graph => current_graph%grove_next
          current_graph%grove_next => null ()
          if (current_graph%keep) then
             call grove_list%add_kingraph (current_graph, .false.)
          endif
          current_graph => next_graph
       enddo
       current_grove => current_grove%next
    enddo
    !$ call OMP_destroy_lock (grove_list%lock)
    call tmp_list%final
  end subroutine grove_list_rebuild
  
  subroutine feyngraph_set_write_file_format (feyngraph_set, u)
    type (feyngraph_set_t), intent (in) :: feyngraph_set
    integer, intent (in) :: u
    type (grove_t), pointer :: grove
    integer :: channel_number
    integer :: grove_number
    channel_number = 0
    grove_number = 0
    grove => feyngraph_set%grove_list%first
    do while (associated (grove))
       grove_number = grove_number + 1
       call grove%write_file_format (feyngraph_set, grove_number, channel_number, u)
       grove => grove%next
    enddo
  end subroutine feyngraph_set_write_file_format

    recursive subroutine grove_write_file_format (grove, feyngraph_set, gr_number, ch_number, u)
      class (grove_t), intent (in) :: grove
      type (feyngraph_set_t), intent (in) :: feyngraph_set
      integer, intent (in) :: u
      integer, intent (inout) :: gr_number
      integer, intent (inout) :: ch_number
      type (kingraph_t), pointer :: current
1     format(3x,A,1x,40(1x,I4))
      write (u, "(A)")
      write (u, "(1x,'!',1x,A,1x,I0,A)", advance='no') &
           'Multiplicity =', grove%grove_prop%multiplicity, ","
      select case (grove%grove_prop%n_resonances)
      case (0)
         write (u, '(1x,A)', advance='no') 'no resonances, '
      case (1)
         write (u, '(1x,A)', advance='no') '1 resonance,  '
      case default
         write (u, '(1x,I0,1x,A)', advance='no') &
              grove%grove_prop%n_resonances, 'resonances, '
      end select
      write (u, '(1x,I0,1x,A)', advance='no') &
           grove%grove_prop%n_log_enhanced, 'logs, '
      write (u, '(1x,I0,1x,A)', advance='no') &
           grove%grove_prop%n_off_shell, 'off-shell, '
      select case (grove%grove_prop%n_t_channel)
      case (0);  write (u, '(1x,A)') 's-channel graph'
      case (1);  write (u, '(1x,A)') '1 t-channel line'
      case default
         write(u,'(1x,I0,1x,A)') &
              grove%grove_prop%n_t_channel, 't-channel lines'
      end select
      write (u, '(1x,A,I0)') 'grove #', gr_number
      current => grove%first
      do while (associated (current))
         if (current%keep) then
            ch_number = ch_number + 1
            call current%write_file_format (feyngraph_set, ch_number, u)
         endif
         current => current%grove_next
      enddo
    end subroutine grove_write_file_format

  subroutine kingraph_write_file_format (kingraph, feyngraph_set, ch_number, u)
    class (kingraph_t), intent (in) :: kingraph
    type (feyngraph_set_t), intent (in) :: feyngraph_set
    integer, intent (in) :: ch_number
    integer, intent (in) :: u
    integer :: i
    integer(TC) :: bincode_incoming
2   format(3X,'map',1X,I3,1X,A,1X,I7,1X,'!',1X,A)
!!! determine bincode of incoming particle from tree
    bincode_incoming = maxval (kingraph%tree%bc)
    write (unit=u, fmt='(1X,A,I0)') '! Channel #', ch_number
    write (unit=u, fmt='(3X,A,1X)', advance='no') 'tree'
    do i=1, size (kingraph%tree%bc)
       if (kingraph%tree%mapping(i) >=0 .or. kingraph%tree%mapping(i) == NONRESONANT &
            .or. (kingraph%tree%bc(i) == bincode_incoming &
            .and. feyngraph_set%process_type == DECAY)) then
          write (unit=u, fmt='(1X,I0)', advance='no') kingraph%tree%bc(i)
       endif
    enddo
    write (unit=u, fmt='(A)', advance='yes')
    do i=1, size(kingraph%tree%bc)
       select case (kingraph%tree%mapping(i))
       case (NO_MAPPING, NONRESONANT, EXTERNAL_PRT)
       case (S_CHANNEL)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 's_channel', &
               kingraph%tree%pdg(i), &
               trim(get_particle_name (feyngraph_set, kingraph%tree%pdg(i)))
       case (T_CHANNEL)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 't_channel', &
               abs (kingraph%tree%pdg(i)), &
               trim(get_particle_name (feyngraph_set, abs(kingraph%tree%pdg(i))))
       case (U_CHANNEL)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 'u_channel', &
               abs (kingraph%tree%pdg(i)), &
               trim(get_particle_name (feyngraph_set, abs(kingraph%tree%pdg(i))))
       case (RADIATION)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 'radiation', &
               kingraph%tree%pdg(i), &
               trim(get_particle_name (feyngraph_set, kingraph%tree%pdg(i)))
       case (COLLINEAR)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 'collinear', &
               kingraph%tree%pdg(i), &
               trim(get_particle_name (feyngraph_set, kingraph%tree%pdg(i)))
       case (INFRARED)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 'infrared ', &
               kingraph%tree%pdg(i), &
               trim(get_particle_name (feyngraph_set, kingraph%tree%pdg(i)))
       case (ON_SHELL)
          write (unit=u, fmt=2) kingraph%tree%bc(i), 'on_shell ', &
               kingraph%tree%pdg(i), &
               trim(get_particle_name (feyngraph_set, kingraph%tree%pdg(i)))
       case default
          call msg_bug (" Impossible mapping mode encountered")
       end select
    enddo
  end subroutine kingraph_write_file_format
  
   function get_particle_name (feyngraph_set, pdg) result (particle_name)
     type (feyngraph_set_t), intent (in) :: feyngraph_set
     integer, intent (in) :: pdg
     character (len=LABEL_LEN) :: particle_name
     integer :: i
     do i=1, size (feyngraph_set%particle)
        if (feyngraph_set%particle(i)%pdg == pdg) then
           particle_name = feyngraph_set%particle(i)%particle_label
           exit
        endif
     enddo
   end function get_particle_name

  subroutine feyngraph_make_invertible (feyngraph)
    class (feyngraph_t), intent (inout) :: feyngraph
    logical :: t_line_found
    feyngraph%root%incoming = .true.
    t_line_found = .false.
    if (associated (feyngraph%root%daughter1)) then
       call f_node_t_line_check (feyngraph%root%daughter1, t_line_found)
       if (.not. t_line_found) then
          if (associated (feyngraph%root%daughter2)) then
             call f_node_t_line_check (feyngraph%root%daughter2, t_line_found)
          endif
       endif
    endif

  contains

  recursive subroutine f_node_t_line_check (node, t_line_found)
    type (f_node_t), target, intent (inout) :: node
    integer :: pos
    logical, intent (inout) :: t_line_found
    if (associated (node%daughter1)) then
       call f_node_t_line_check (node%daughter1, t_line_found)
       if (node%daughter1%incoming .or. node%daughter1%t_line) then
          node%t_line = .true.
       else if (associated (node%daughter2)) then
          call f_node_t_line_check (node%daughter2, t_line_found)
          if (node%daughter2%incoming .or. node%daughter2%t_line) then
             node%t_line = .true.
          endif
       endif
    else
       pos = index (node%particle_label, '[') + 1
       if (node%particle_label(pos:pos) == '2') then
          node%incoming = .true.
          t_line_found = .true.
       endif
    endif
  end subroutine f_node_t_line_check

  end subroutine feyngraph_make_invertible

  subroutine kingraph_make_inverse_copy (original_kingraph, feyngraph)
    class (kingraph_t), intent (inout) :: original_kingraph
    type (feyngraph_t), intent (inout) :: feyngraph
    type (kingraph_t), pointer :: kingraph_copy
    type (k_node_t), pointer :: potential_root
    allocate (kingraph_copy)
    if (associated (feyngraph%kin_last)) then
       allocate (feyngraph%kin_last%next)
       feyngraph%kin_last => feyngraph%kin_last%next
    else
       allocate(feyngraph%kin_first)
       feyngraph%kin_last => feyngraph%kin_first
    endif
    kingraph_copy => feyngraph%kin_last
    call kingraph_set_inverse_daughters (original_kingraph)
    kingraph_copy%inverse = .true.
    kingraph_copy%n_nodes = original_kingraph%n_nodes
    kingraph_copy%keep = original_kingraph%keep
    potential_root => original_kingraph%root
    do while (.not. potential_root%incoming .or. &
         (associated (potential_root%daughter1) .and. associated (potential_root%daughter2)))
       if (potential_root%daughter1%incoming .or. potential_root%daughter1%t_line) then
          potential_root => potential_root%daughter1
       else if (potential_root%daughter2%incoming .or. potential_root%daughter2%t_line) then
          potential_root => potential_root%daughter2
       endif
    enddo
    call node_inverse_deep_copy (potential_root, kingraph_copy%root)
  end subroutine kingraph_make_inverse_copy

  recursive subroutine node_inverse_deep_copy (original_node, node_copy)
    type (k_node_t), intent (in) :: original_node
    type (k_node_t), pointer, intent (out) :: node_copy
    call original_node%f_node%k_node_list%add_entry(node_copy, recycle=.false.)
    node_copy = original_node
    if (node_copy%t_line .or. node_copy%incoming) then
       node_copy%particle => original_node%particle%anti
    else
       node_copy%particle => original_node%particle
    endif
    if (associated (original_node%inverse_daughter1) .and. associated (original_node%inverse_daughter2)) then
       if (original_node%inverse_daughter1%incoming .or. original_node%inverse_daughter1%t_line) then
          node_copy%daughter2 => original_node%inverse_daughter2
          call node_inverse_deep_copy (original_node%inverse_daughter1, &
               node_copy%daughter1)
       else if (original_node%inverse_daughter2%incoming .or. original_node%inverse_daughter2%t_line) then
          node_copy%daughter1 => original_node%inverse_daughter1
          call node_inverse_deep_copy (original_node%inverse_daughter2, &
               node_copy%daughter2)
       endif
    endif
  end subroutine node_inverse_deep_copy

  subroutine feyngraph_set_generate_single (feyngraph_set, model, n_in, n_out, &
       phs_par, fatal_beam_decay, u_in)
    type(feyngraph_set_t), intent(inout) :: feyngraph_set
    type(model_data_t), target, intent(in) :: model
    integer, intent(in) :: n_in, n_out
    type(phs_parameters_t), intent(in) :: phs_par
    logical, intent(in) :: fatal_beam_decay
    integer, intent(in) :: u_in
    feyngraph_set%n_in = n_in
    feyngraph_set%n_out = n_out
    feyngraph_set%process_type = n_in
    feyngraph_set%phs_par = phs_par
    feyngraph_set%model => model
    call msg_debug (D_PHASESPACE, "Construct relevant Feynman diagrams from Omega output")
    call feyngraph_set%build (u_in)
    call msg_debug (D_PHASESPACE, "Find phase-space parametrizations")
    call feyngraph_set_find_phs_parametrizations(feyngraph_set)
  end subroutine feyngraph_set_generate_single

  subroutine feyngraph_set_find_phs_parametrizations (feyngraph_set)
    class (feyngraph_set_t), intent (inout) :: feyngraph_set
    type (feyngraph_t), pointer :: current => null ()
    type (feyngraph_ptr_t), dimension (:), allocatable :: set
    integer :: pos
    integer :: i
    allocate (set (feyngraph_set%n_graphs))
    pos = 0
    current => feyngraph_set%first
    do while (associated (current))
       pos = pos + 1
       set(pos)%graph => current
       current => current%next
    enddo
    if (feyngraph_set%process_type == SCATTERING) then
       !$OMP PARALLEL DO
       do i=1, feyngraph_set%n_graphs
          if (set(i)%graph%keep) then
             call set(i)%graph%make_invertible ()
          endif
       enddo
       !$OMP END PARALLEL DO
    endif
    call f_node_list_compute_mappings_s (feyngraph_set)
    do i=1, feyngraph_set%n_graphs
       if (set(i)%graph%keep) then
          call set(i)%graph%compute_mappings (feyngraph_set)
       endif
    enddo
    !$ call OMP_init_lock (feyngraph_set%grove_list%lock)
    !$OMP PARALLEL DO
    do i=1, feyngraph_set%n_graphs
       if (set(i)%graph%keep) then
          call feyngraph_set%grove_list%add_feyngraph (set(i)%graph, &
               feyngraph_set%model)
       endif
    enddo
    !$OMP END PARALLEL DO
    !$ call OMP_destroy_lock (feyngraph_set%grove_list%lock)
  end subroutine feyngraph_set_find_phs_parametrizations
    
  elemental function tree_equal (tree1, tree2) result (flag)
    type (tree_t), intent (in) :: tree1, tree2
    logical :: flag
    if (tree1%n_entries == tree2%n_entries) then
       if (tree1%bc(size(tree1%bc)) == tree2%bc(size(tree2%bc))) then
          flag = all (tree1%mapping == tree2%mapping) .and. &
               all (tree1%bc == tree2%bc) .and. &
               all (abs(tree1%pdg) == abs(tree2%pdg))
       else
          flag = .false.
       endif
    else
       flag = .false.
    endif
  end function tree_equal

  pure function subtree_eqv (subtree1, subtree2) result (eqv)
    type (tree_t), intent (in) :: subtree1, subtree2
    logical :: eqv
    integer :: root_pos
    integer :: i
    logical :: equal
    eqv = .false.
    if (subtree1%n_entries /= subtree2%n_entries) return
    root_pos = subtree1%n_entries
    if (subtree1%mapping(root_pos) == NONRESONANT .or. &
         subtree2%mapping(root_pos) == NONRESONANT .or. &
         (subtree1%mapping(root_pos) == NO_MAPPING .and. &
         subtree2%mapping(root_pos) == NO_MAPPING .and. &
         abs(subtree1%pdg(root_pos)) == abs(subtree2%pdg(root_pos)))) then
       do i = subtree1%n_entries, 1, -1
          if (subtree1%bc(i) /= subtree2%bc(i)) return
       enddo
       equal = .true.
       do i = subtree1%n_entries, 1, -1
          if (abs(subtree1%pdg(i)) /= abs (subtree2%pdg(i))) then
             select case (subtree1%mapping(i))
             case (NO_MAPPING, NONRESONANT)
                select case (subtree2%mapping(i))
                case (NO_MAPPING, NONRESONANT)
                   equal = .false.
                case default
                   return
                end select
             case default
                return
             end select
          endif
       enddo
       do i = subtree1%n_entries, 1, -1
          if (subtree1%mapping(i) /= subtree2%mapping(i)) then
             select case (subtree1%mapping(i))
             case (NO_MAPPING, NONRESONANT)
                select case (subtree2%mapping(i))
                case (NO_MAPPING, NONRESONANT)
                case default
                   return
                end select
             case default
                return
             end select
          endif
       enddo
       if (.not. equal) eqv = .true.
    endif
  end function subtree_eqv
  
  subroutine subtree_select (subtree1, subtree2, model)
    type (tree_t), intent (inout) :: subtree1, subtree2
    type (model_data_t), intent (in) :: model
    integer :: j, k
    integer(TC), dimension(:), allocatable :: tmp_bc, daughter_bc
    integer, dimension(:), allocatable :: tmp_pdg, daughter_pdg
    integer, dimension (:), allocatable :: pdg_match
    if (subtree1 .eqv. subtree2) then
       do j=1, subtree1%n_entries
          if (abs(subtree1%pdg(j)) /= abs(subtree2%pdg(j))) then
             tmp_bc = subtree1%bc(:j-1); tmp_pdg = subtree1%pdg(:j-1)
             do k=j-1, 1, - 1
                where (iand (tmp_bc(:k-1),tmp_bc(k)) /= 0 &
                     .or. iand(tmp_bc(:k-1),subtree1%bc(j)) == 0)
                   tmp_bc(:k-1) = 0
                   tmp_pdg(:k-1) = 0
                endwhere
             enddo
             daughter_bc = pack (tmp_bc, tmp_bc /= 0)
             daughter_pdg = pack (tmp_pdg, tmp_pdg /= 0)
             if (size (daughter_pdg) == 2) then
                call model%match_vertex(daughter_pdg(1), daughter_pdg(2), pdg_match)
                if (.not. allocated (pdg_match)) then
!!! Relevant if tree contains only abs (pdg). In this case, changing the
!!! sign of one of the pdg codes should give a result.
                   call model%match_vertex(-daughter_pdg(1), daughter_pdg(2), pdg_match)
                endif
             endif
             do k=1, size (pdg_match)
                if (abs(pdg_match(k)) == abs(subtree1%pdg(j))) then
                   !$OMP CRITICAL (deactivate)
                   if (subtree1%keep) subtree2%keep = .false.
                   !$OMP END CRITICAL (deactivate)
                   exit
                else if (abs(pdg_match(k)) == abs(subtree2%pdg(j))) then
                   !$OMP CRITICAL (deactivate)
                   if (subtree2%keep) subtree1%keep = .false.
                   !$OMP END CRITICAL (deactivate)
                   exit
                endif
             enddo
             deallocate (tmp_bc, tmp_pdg, daughter_bc, daughter_pdg, pdg_match)
             if (.not. (subtree1%keep .and. subtree2%keep)) exit
          endif
       enddo
    endif
  end subroutine subtree_select

  subroutine kingraph_assign_resonance_hash (kingraph)
    class (kingraph_t), intent (inout) :: kingraph
    logical, dimension (:), allocatable :: tree_resonant
    integer(i8), dimension(1) :: mold
    allocate (tree_resonant (kingraph%tree%n_entries))
    tree_resonant = (kingraph%tree%mapping == S_CHANNEL)
    kingraph%grove_prop%res_hash = hash (transfer &
         (concat (sort (pack (kingraph%tree%pdg, tree_resonant)), &
         sort (pack (abs (kingraph%tree%pdg), &
         kingraph%tree%mapping == T_CHANNEL .or. &
         kingraph%tree%mapping == U_CHANNEL))), mold))
    deallocate (tree_resonant)
  end subroutine kingraph_assign_resonance_hash

  subroutine feyngraph_set_write_process_bincode_format (feyngraph_set, unit)
    type(feyngraph_set_t), intent(in), target :: feyngraph_set
    integer, intent(in), optional :: unit
    integer, dimension(:), allocatable :: bincode, field_width
    integer :: n_in, n_out, n_tot, n_flv
    integer :: u, f, i, bc
    character(20) :: str
    type(string_t) :: fmt_head
    type(string_t), dimension(:), allocatable :: fmt_proc
    u = given_output_unit (unit);  if (u < 0)  return
    if (.not. allocated (feyngraph_set%flv)) return
    write (u, "('!',1x,A)")  "List of subprocesses with particle bincodes:"
    n_in  = feyngraph_set%n_in
    n_out = feyngraph_set%n_out
    n_tot = n_in + n_out
    n_flv = size (feyngraph_set%flv, 2)
    allocate (bincode (n_tot), field_width (n_tot), fmt_proc (n_tot))
    bc = 1
    do i = 1, n_out
       bincode(n_in + i) = bc
       bc = 2 * bc
    end do
    do i = n_in, 1, -1
       bincode(i) = bc
       bc = 2 * bc
    end do
    do i = 1, n_tot
       write (str, "(I0)")  bincode(i)
       field_width(i) = len_trim (str)
       do f = 1, n_flv
          field_width(i) = max (field_width(i), &
               len (feyngraph_set%flv(i,f)%get_name ()))
       end do
    end do
    fmt_head = "('!'"
    do i = 1, n_tot
       fmt_head = fmt_head // ",1x,"
       fmt_proc(i) = "(1x,"
       write (str, "(I0)")  field_width(i)
       fmt_head = fmt_head // "I" // trim(str)
       fmt_proc(i) = fmt_proc(i) // "A" // trim(str)
       if (i == n_in) then
          fmt_head = fmt_head // ",1x,'  '"
       end if
    end do
    do i = 1, n_tot
       fmt_proc(i) = fmt_proc(i) // ")"
    end do
    fmt_head = fmt_head // ")"
    write (u, char (fmt_head))  bincode
    do f = 1, n_flv
       write (u, "('!')", advance="no")
       do i = 1, n_tot
          write (u, char (fmt_proc(i)), advance="no") &
               char (feyngraph_set%flv(i,f)%get_name ())
          if (i == n_in)  write (u, "(1x,'=>')", advance="no")
       end do
       write (u, *)
    end do
    write (u, char (fmt_head))  bincode
  end subroutine feyngraph_set_write_process_bincode_format

  subroutine feyngraph_set_write_graph_format (feyngraph_set, filename, process_id, unit)
    type(feyngraph_set_t), intent(in), target :: feyngraph_set
    type(string_t), intent(in) :: filename, process_id
    integer, intent(in), optional :: unit
    type(kingraph_t), pointer :: kingraph
    type(grove_t), pointer :: grove
    integer :: u, n_grove, count, pgcount
    logical :: first_in_grove
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, '(A)') "\documentclass[10pt]{article}"
    write (u, '(A)') "\usepackage{amsmath}"
    write (u, '(A)') "\usepackage{feynmp}"
    write (u, '(A)') "\usepackage{url}"
    write (u, '(A)') "\usepackage{color}"
    write (u, *)
    write (u, '(A)') "\textwidth 18.5cm"
    write (u, '(A)') "\evensidemargin -1.5cm"
    write (u, '(A)') "\oddsidemargin -1.5cm"
    write (u, *)
    write (u, '(A)') "\newcommand{\blue}{\color{blue}}"
    write (u, '(A)') "\newcommand{\green}{\color{green}}"
    write (u, '(A)') "\newcommand{\red}{\color{red}}"
    write (u, '(A)') "\newcommand{\magenta}{\color{magenta}}"
    write (u, '(A)') "\newcommand{\cyan}{\color{cyan}}"
    write (u, '(A)') "\newcommand{\sm}{\footnotesize}"
    write (u, '(A)') "\setlength{\parindent}{0pt}"
    write (u, '(A)') "\setlength{\parsep}{20pt}"
    write (u, *)
    write (u, '(A)') "\begin{document}"
    write (u, '(A)') "\begin{fmffile}{" // char (filename) // "}"
    write (u, '(A)') "\fmfcmd{color magenta; magenta = red + blue;}"
    write (u, '(A)') "\fmfcmd{color cyan; cyan = green + blue;}"
    write (u, '(A)') "\begin{fmfshrink}{0.5}"
    write (u, '(A)') "\begin{flushleft}"
    write (u, *)
    write (u, '(A)') "\noindent" // &
         & "\textbf{\large\texttt{WHIZARD} phase space channels}" // &
         & "\hfill\today"
    write (u, *)
    write (u, '(A)') "\vspace{10pt}"
    write (u, '(A)') "\noindent" // &
         & "\textbf{Process:} \url{" // char (process_id) // "}"
    call feyngraph_set_write_process_tex_format (feyngraph_set, u)
    write (u, *)
    write (u, '(A)') "\noindent" // &
         & "\textbf{Note:} These are pseudo Feynman graphs that "
    write (u, '(A)') "visualize phase-space parameterizations " // &
         & "(``integration channels'').  "
    write (u, '(A)') "They do \emph{not} indicate Feynman graphs used for the " // &
         & "matrix element."
    write (u, *)
    write (u, '(A)') "\textbf{Color code:} " // &
         & "{\blue resonance,} " // &
         & "{\cyan t-channel,} " // &
         & "{\green radiation,} "
    write (u, '(A)') "{\red infrared,} " // &
         & "{\magenta collinear,} " // &
         & "external/off-shell"
    write (u, *)
    write (u, '(A)') "\noindent" // &
         & "\textbf{Black square:} Keystone, indicates ordering of " // &
         & "phase space parameters."
    write (u, *)
    write (u, '(A)') "\vspace{-20pt}"
    count = 0
    pgcount = 0
    n_grove = 0
    grove => feyngraph_set%grove_list%first
    do while (associated (grove))
       n_grove = n_grove + 1
       write (u, *)
       write (u, '(A)') "\vspace{20pt}"
       write (u, '(A)') "\begin{tabular}{l}"
       write (u, '(A,I5,A)') &
            & "\fbox{\bf Grove \boldmath$", n_grove, "$} \\[10pt]"
       write (u, '(A,I1,A)') "Multiplicity: ", &
            grove%grove_prop%multiplicity, "\\"
       write (u, '(A,I1,A)') "Resonances:   ", &
            grove%grove_prop%n_resonances, "\\"
       write (u, '(A,I1,A)') "Log-enhanced: ", &
            grove%grove_prop%n_log_enhanced, "\\"
       write (u, '(A,I1,A)') "Off-shell:    ", &
            grove%grove_prop%n_off_shell, "\\"
       write (u, '(A,I1,A)') "t-channel:    ", &
            grove%grove_prop%n_t_channel, ""
       write (u, '(A)') "\end{tabular}"
       kingraph => grove%first
       do while (associated (kingraph))
          count = count + 1
          call kingraph_write_graph_format (kingraph, count, unit)
          kingraph => kingraph%grove_next
       enddo
       grove => grove%next
    enddo
    write (u, '(A)') "\end{flushleft}"
    write (u, '(A)') "\end{fmfshrink}"
    write (u, '(A)') "\end{fmffile}"
    write (u, '(A)') "\end{document}"
  end subroutine feyngraph_set_write_graph_format
  
  subroutine feyngraph_set_write_process_tex_format (feyngraph_set, unit)
    type(feyngraph_set_t), intent(in), target :: feyngraph_set
    integer, intent(in), optional :: unit
    integer :: n_tot
    integer :: u, f, i
    n_tot = feyngraph_set%n_in + feyngraph_set%n_out
    u = given_output_unit (unit);  if (u < 0)  return
    if (.not. allocated (feyngraph_set%flv)) return
    write (u, "(A)")  "\begin{align*}"
    do f = 1, size (feyngraph_set%flv, 2)
       do i = 1, feyngraph_set%n_in
          if (i > 1)  write (u, "(A)", advance="no") "\quad "
          write (u, "(A)", advance="no") &
               char (feyngraph_set%flv(i,f)%get_tex_name ())
       end do
       write (u, "(A)", advance="no")  "\quad &\to\quad "
       do i = feyngraph_set%n_in + 1, n_tot
          if (i > feyngraph_set%n_in + 1)  write (u, "(A)", advance="no") "\quad "
          write (u, "(A)", advance="no") &
               char (feyngraph_set%flv(i,f)%get_tex_name ())
       end do
       if (f < size (feyngraph_set%flv, 2)) then
          write (u, "(A)")  "\\"
       else
          write (u, "(A)")  ""
       end if
    end do
    write (u, "(A)")  "\end{align*}"
  end subroutine feyngraph_set_write_process_tex_format

  subroutine kingraph_write_graph_format (kingraph, count, unit)
    type(kingraph_t), intent(in) :: kingraph
    integer, intent(in) :: count
    integer, intent(in), optional :: unit
    integer :: u
    type(string_t) :: left_str, right_str
    u = given_output_unit (unit);  if (u < 0)  return
    left_str = ""
    right_str = ""
    write (u, '(A)') "\begin{minipage}{105pt}"
    write (u, '(A)') "\vspace{30pt}"
    write (u, '(A)') "\begin{center}"
    write (u, '(A)') "\begin{fmfgraph*}(55,55)"
    call graph_write_node (kingraph%root)
    write (u, '(A)') "\fmfleft{" // char (extract (left_str, 2)) // "}"
    write (u, '(A)') "\fmfright{" // char (extract (right_str, 2)) // "}"
    write (u, '(A)') "\end{fmfgraph*}\\"
    write (u, '(A,I5,A)') "\fbox{$", count, "$}"
    write (u, '(A)') "\end{center}"
    write (u, '(A)') "\end{minipage}"
    write (u, '(A)') "%"
  contains
    recursive subroutine graph_write_node (node)
      type(k_node_t), intent(in) :: node
      if (associated (node%daughter1) .or. associated (node%daughter2)) then
         if (node%daughter2%t_line .or. node%daughter2%incoming) then
            call vertex_write (node, node%daughter2)
            call vertex_write (node, node%daughter1)
         else
            call vertex_write (node, node%daughter1)
            call vertex_write (node, node%daughter2)
         endif
         if (node%mapping == EXTERNAL_PRT) then
            call line_write (node%bincode, 0, node%particle)
            call external_write (node%bincode, node%particle%tex_name, &
                 left_str)
            write (u, '(A,I0,A)') "\fmfv{d.shape=square}{v0}"
         end if
      else
         if (node%incoming) then
            call external_write (node%bincode, node%particle%anti%tex_name, &
                 left_str)
         else
            call external_write (node%bincode, node%particle%tex_name, &
                 right_str)
         end if
      end if
    end subroutine graph_write_node
    recursive subroutine vertex_write (node, daughter)
      type(k_node_t), intent(in) :: node, daughter
      integer :: bincode
      if (associated (node%daughter1) .and. associated (node%daughter2) &
           .and. node%mapping == EXTERNAL_PRT) then
         bincode = 0
      else
         bincode = node%bincode
      end if
      call graph_write_node (daughter)
      if (associated (node%daughter1) .or. associated (node%daughter2)) then
         call line_write (bincode, daughter%bincode, daughter%particle, &
              mapping=daughter%mapping)
      else
         call line_write (bincode, daughter%bincode, daughter%particle)
      end if
    end subroutine vertex_write
    subroutine line_write (i1, i2, particle, mapping)
      integer(TC), intent(in) :: i1, i2
      type(part_prop_t), intent(in) :: particle
      integer, intent(in), optional :: mapping
      integer :: k1, k2
      type(string_t) :: prt_type
      select case (particle%spin_type)
      case (SCALAR);       prt_type = "plain"
      case (SPINOR);       prt_type = "fermion"
      case (VECTOR);       prt_type = "boson"
      case (VECTORSPINOR); prt_type = "fermion"
      case (TENSOR);       prt_type = "dbl_wiggly"
      case default;        prt_type = "dashes"
      end select
      if (particle%pdg < 0) then
!!! anti-particle
         k1 = i2;  k2 = i1
      else
         k1 = i1;  k2 = i2
      end if
      if (present (mapping)) then
         select case (mapping)
         case (S_CHANNEL)
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=blue,lab=\sm\blue$" // &
                 & char (particle%tex_name) // "$}" // &
                 & "{v", k1, ",v", k2, "}"
         case (T_CHANNEL, U_CHANNEL)
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=cyan,lab=\sm\cyan$" // &
                 & char (particle%tex_name) // "$}" // &
                 & "{v", k1, ",v", k2, "}"
         case (RADIATION)
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=green,lab=\sm\green$" // &
                 & char (particle%tex_name) // "$}" // &
                 & "{v", k1, ",v", k2, "}"
         case (COLLINEAR)
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=magenta,lab=\sm\magenta$" // &
                 & char (particle%tex_name) // "$}" // &
                 & "{v", k1, ",v", k2, "}"
         case (INFRARED)
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=red,lab=\sm\red$" // &
                 & char (particle%tex_name) // "$}" // &
                 & "{v", k1, ",v", k2, "}"
         case default
            write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & ",f=black}" // &
                 & "{v", k1, ",v", k2, "}"
         end select
      else
         write (u, '(A,I0,A,I0,A)') "\fmf{" // char (prt_type) // &
                 & "}" // &
                 & "{v", k1, ",v", k2, "}"
      end if
    end subroutine line_write
    subroutine external_write (bincode, name, ext_str)
      integer(TC), intent(in) :: bincode
      type(string_t), intent(in) :: name
      type(string_t), intent(inout) :: ext_str
      character(len=20) :: str
      write (str, '(A2,I0)') ",v", bincode
      ext_str = ext_str // trim (str)
      write (u, '(A,I0,A,I0,A)') "\fmflabel{\sm$" &
        // char (name) &
        // "\,(", bincode, ")" &
        // "$}{v", bincode, "}"
    end subroutine external_write
  end subroutine kingraph_write_graph_format

  subroutine feyngraph_set_generate &
    (feyngraph_set, model, n_in, n_out, flv, phs_par, fatal_beam_decay, u_in, use_dag)
    type(feyngraph_set_t), intent(out) :: feyngraph_set
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: n_in, n_out
    type(flavor_t), dimension(:,:), intent(in) :: flv
    type(phs_parameters_t), intent(in) :: phs_par
    logical, intent(in) :: fatal_beam_decay
    integer, intent(in) :: u_in
    logical, optional, intent(in) :: use_dag
    type(grove_t), pointer :: grove
    integer :: i, j
    type(kingraph_t), pointer :: kingraph
    if (phase_space_vanishes (phs_par%sqrts, n_in, flv))  return
    if (present (use_dag)) feyngraph_set%use_dag = use_dag
    feyngraph_set%process_type = n_in
    feyngraph_set%n_in = n_in
    feyngraph_set%n_out = n_out
    allocate (feyngraph_set%flv (size (flv, 1), size (flv, 2)))
    do i = 1, size (flv, 2)
       do j = 1, size (flv, 1)
          call feyngraph_set%flv(j,i)%init (flv(j,i)%get_pdg (), model)
       end do
    end do
    allocate (feyngraph_set%particle (PRT_ARRAY_SIZE))
    allocate (feyngraph_set%grove_list)
    allocate (feyngraph_set%fset (size (flv, 2)))
    do i = 1, size (feyngraph_set%fset)
       feyngraph_set%fset(i)%use_dag = feyngraph_set%use_dag
       allocate (feyngraph_set%fset(i)%flv(size (flv,1),1))
       feyngraph_set%fset(i)%flv(:,1) = flv(:,i)
       feyngraph_set%fset(i)%particle => feyngraph_set%particle
       feyngraph_set%fset(i)%grove_list => feyngraph_set%grove_list
       call feyngraph_set_generate_single (feyngraph_set%fset(i), &
            model, n_in, n_out, phs_par, fatal_beam_decay, u_in)
    enddo
    call feyngraph_set%grove_list%rebuild ()
  end subroutine feyngraph_set_generate

  function feyngraph_set_is_valid (feyngraph_set) result (flag)
    class (feyngraph_set_t), intent(in) :: feyngraph_set
    type (kingraph_t), pointer :: kingraph
    type (grove_t), pointer :: grove
    logical :: flag
    flag = .false.
    if (associated (feyngraph_set%grove_list)) then
       grove => feyngraph_set%grove_list%first
       do while (associated (grove))
          kingraph => grove%first
          do while (associated (kingraph))
             if (kingraph%keep) then
                flag = .true.
                return
             endif
             kingraph => kingraph%next
          enddo
          grove => grove%next
       enddo
    endif
  end function feyngraph_set_is_valid
  
  subroutine kingraph_extract_resonance_history &
       (kingraph, res_hist, model, n_out)
    class(kingraph_t), intent(in), target :: kingraph
    type(resonance_history_t), intent(out) :: res_hist
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: n_out
    type(resonance_info_t) :: resonance
    integer :: i, mom_id, pdg
    call msg_debug2 (D_PHASESPACE, "kingraph_extract_resonance_history")
    if (kingraph%grove_prop%n_resonances > 0) then
       if (associated (kingraph%root%daughter1) .or. &
            associated (kingraph%root%daughter2)) then
          call msg_debug2 (D_PHASESPACE, "kingraph has resonances, root has children")
          do i = 1, kingraph%tree%n_entries
             if (kingraph%tree%mapping(i) == S_CHANNEL) then
                mom_id = kingraph%tree%bc (i)
                pdg = kingraph%tree%pdg (i)
                call resonance%init (mom_id, pdg, model, n_out)
                if (debug2_active (D_PHASESPACE)) then
                   print *, 'D: Adding resonance'
                   call resonance%write ()
                end if
                call res_hist%add_resonance (resonance)
             end if
          end do
       end if
    end if
  end subroutine kingraph_extract_resonance_history

  function grove_list_get_n_trees (grove_list) result (n)
    class (grove_list_t), intent (in) :: grove_list
    integer :: n
    type(kingraph_t), pointer :: kingraph
    type(grove_t), pointer :: grove
    call msg_debug (D_PHASESPACE, "grove_list_get_n_trees")
    n = 0
    grove => grove_list%first
    do while (associated (grove))
       kingraph => grove%first
       do while (associated (kingraph))
          if (kingraph%keep) n = n + 1
          kingraph => kingraph%grove_next
       enddo
       grove => grove%next
    enddo
    call msg_debug (D_PHASESPACE, "n", n)
  end function grove_list_get_n_trees

  subroutine feyngraph_set_get_resonance_histories (feyngraph_set, n_filter, res_hists)
    type(feyngraph_set_t), intent(in), target :: feyngraph_set
    integer, intent(in), optional :: n_filter
    type(resonance_history_t), dimension(:), allocatable, intent(out) :: res_hists
    type(kingraph_t), pointer :: kingraph
    type(grove_t), pointer :: grove
    type(resonance_history_t) :: res_hist
    type(resonance_history_set_t) :: res_hist_set
    integer :: i_grove
    call msg_debug (D_PHASESPACE, "grove_list_get_resonance_histories")
    call res_hist_set%init (n_filter = n_filter)
    grove => feyngraph_set%grove_list%first
    i_grove = 0
    do while (associated (grove))
       i_grove = i_grove + 1
       kingraph => grove%first
       do while (associated (kingraph))
          if (kingraph%keep) then
             call msg_debug2 (D_PHASESPACE, "grove", i_grove)
             call kingraph%extract_resonance_history &
                  (res_hist, feyngraph_set%model, feyngraph_set%n_out)
             call res_hist_set%enter (res_hist)
          end if
          kingraph => kingraph%grove_next
       end do
    end do
    call res_hist_set%freeze ()
    call res_hist_set%to_array (res_hists)
  end subroutine feyngraph_set_get_resonance_histories


end module cascades2

