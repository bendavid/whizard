! WHIZARD 2.0.0 Mon Apr 12 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module phs_forests

  use kinds, only: default !NODEP!
  use kinds, only: TC !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use vamp_equivalences !NODEP!
  use permutations
  use ifiles
  use syntax_rules
  use lexers
  use parser
  use models
  use flavors
  use interactions
  use mappings
  use phs_trees

  implicit none
  private

  public :: phs_parameters_t
  public :: phs_parameters_write
  public :: phs_parameters_read
  public :: phs_forest_t
  public :: phs_forest_init
  public :: phs_forest_final
  public :: phs_forest_write
  public :: assignment(=)
  public :: phs_forest_get_n_parameters
  public :: phs_forest_get_n_channels
  public :: phs_forest_get_n_groves
  public :: phs_forest_get_grove_bounds
  public :: phs_forest_get_n_equivalences
  public :: syntax_phs_forest_init
  public :: syntax_phs_forest_final
  public :: syntax_phs_forest_write
  public :: phs_forest_read
  public :: phs_forest_set_flavors
  public :: phs_forest_set_parameters
  public :: phs_forest_set_prt_in
  public :: phs_forest_get_prt_out
  public :: phs_forest_set_equivalences
  public :: phs_forest_setup_vamp_equivalences
  public :: phs_forest_evaluate_phase_space
  public :: phs_forest_test

  type :: phs_parameters_t
     real(default) :: sqrts = 0
     real(default) :: m_threshold_s = 50._default
     real(default) :: m_threshold_t = 100._default
     integer :: off_shell = 1
     integer :: t_channel = 2
  end type phs_parameters_t

  type :: equivalence_t
     private
     integer :: left, right
     type(permutation_t) :: perm
     type(permutation_t) :: msq_perm, angle_perm
     logical, dimension(:), allocatable :: angle_sig
     type(equivalence_t), pointer :: next => null ()
  end type equivalence_t

  type :: equivalence_list_t
     private
     integer :: length = 0
     type(equivalence_t), pointer :: first => null ()
     type(equivalence_t), pointer :: last => null ()
  end type equivalence_list_t

  type :: phs_grove_t
     private
     integer :: tree_count_offset
     type(phs_tree_t), dimension(:), allocatable :: tree
     type(equivalence_list_t) :: equivalence_list
  end type phs_grove_t

  type :: phs_forest_t
     private
     integer :: n_in, n_out, n_tot
     integer :: n_masses, n_angles, n_dimensions
     integer :: n_trees, n_equivalences
     type(flavor_t), dimension(:), allocatable :: flv
     type(phs_grove_t), dimension(:), allocatable :: grove
     integer, dimension(:), allocatable :: grove_lookup
     type(phs_prt_t), dimension(:), allocatable :: prt_in
     type(phs_prt_t), dimension(:), allocatable :: prt_out
     type(phs_prt_t), dimension(:), allocatable :: prt
  end type phs_forest_t

  interface operator(==)
     module procedure phs_parameters_eq
  end interface
  interface operator(/=)
     module procedure phs_parameters_ne
  end interface
  interface assignment(=)
     module procedure equivalence_list_assign
  end interface

  interface assignment(=)
     module procedure phs_grove_assign0
     module procedure phs_grove_assign1
  end interface

  interface assignment(=)
     module procedure phs_forest_assign
  end interface

  interface phs_forest_read
     module procedure phs_forest_read_file
     module procedure phs_forest_read_unit
     module procedure phs_forest_read_parse_tree
  end interface


  type(syntax_t), target, save :: syntax_phs_forest


contains

  subroutine phs_parameters_write (phs_par, unit)
    type(phs_parameters_t), intent(in) :: phs_par
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *) "  sqrts         = ", phs_par%sqrts
    write (u, *) "  m_threshold_s = ", phs_par%m_threshold_s
    write (u, *) "  m_threshold_t = ", phs_par%m_threshold_t
    write (u, *) "  off_shell = ", phs_par%off_shell
    write (u, *) "  t_channel = ", phs_par%t_channel
  end subroutine phs_parameters_write

  subroutine phs_parameters_read (phs_par, unit)
    type(phs_parameters_t), intent(out) :: phs_par
    integer, intent(in) :: unit
    character(20) :: dummy
    character :: equals
    read (unit, *)  dummy, equals, phs_par%sqrts
    read (unit, *)  dummy, equals, phs_par%m_threshold_s
    read (unit, *)  dummy, equals, phs_par%m_threshold_t
    read (unit, *)  dummy, equals, phs_par%off_shell
    read (unit, *)  dummy, equals, phs_par%t_channel
  end subroutine phs_parameters_read

  function phs_parameters_eq (phs_par1, phs_par2) result (equal)
    logical :: equal
    type(phs_parameters_t), intent(in) :: phs_par1, phs_par2
    equal = phs_par1%sqrts == phs_par2%sqrts &
         .and. phs_par1%m_threshold_s == phs_par2%m_threshold_s &
         .and. phs_par1%m_threshold_t == phs_par2%m_threshold_t &
         .and. phs_par1%off_shell == phs_par2%off_shell &
         .and. phs_par1%t_channel == phs_par2%t_channel
  end function phs_parameters_eq

  function phs_parameters_ne (phs_par1, phs_par2) result (ne)
    logical :: ne
    type(phs_parameters_t), intent(in) :: phs_par1, phs_par2
    ne = phs_par1%sqrts /= phs_par2%sqrts &
         .or. phs_par1%m_threshold_s /= phs_par2%m_threshold_s &
         .or. phs_par1%m_threshold_t /= phs_par2%m_threshold_t &
         .or. phs_par1%off_shell /= phs_par2%off_shell &
         .or. phs_par1%t_channel /= phs_par2%t_channel
  end function phs_parameters_ne

  subroutine equivalence_list_add (eql, left, right, perm)
    type(equivalence_list_t), intent(inout) :: eql
    integer, intent(in) :: left, right
    type(permutation_t), intent(in) :: perm
    type(equivalence_t), pointer :: eq
    allocate (eq)
    eq%left = left
    eq%right = right
    eq%perm = perm
    if (associated (eql%last)) then
       eql%last%next => eq
    else
       eql%first => eq
    end if
    eql%last => eq
    eql%length = eql%length + 1
  end subroutine equivalence_list_add

  pure subroutine equivalence_list_final (eql)
    type(equivalence_list_t), intent(inout) :: eql
    type(equivalence_t), pointer :: eq
    do while (associated (eql%first))
       eq => eql%first
       eql%first => eql%first%next
       deallocate (eq)
    end do
    eql%last => null ()
    eql%length = 0
  end subroutine equivalence_list_final

  subroutine equivalence_list_assign (eql_out, eql_in)
    type(equivalence_list_t), intent(out) :: eql_out
    type(equivalence_list_t), intent(in) :: eql_in
    type(equivalence_t), pointer :: eq, eq_copy
    eq => eql_in%first
    do while (associated (eq))
       allocate (eq_copy)
       eq_copy = eq
       eq_copy%next => null ()
       if (associated (eql_out%first)) then
          eql_out%last%next => eq_copy
       else
          eql_out%first => eq_copy
       end if
       eql_out%last => eq_copy
       eq => eq%next
    end do
  end subroutine equivalence_list_assign

  elemental function equivalence_list_length (eql) result (length)
    integer :: length
    type(equivalence_list_t), intent(in) :: eql
    length = eql%length
  end function equivalence_list_length

  subroutine equivalence_list_write (eql, unit)
    type(equivalence_list_t), intent(in) :: eql
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (associated (eql%first)) then
       call equivalence_write_rec (eql%first, u)
    else
       write (u, *) " [empty]"
    end if
  contains
    recursive subroutine equivalence_write_rec (eq, u)
      type(equivalence_t), intent(in) :: eq
      integer, intent(in) :: u
      integer :: i
      write (u, "(1x,A,1x,I5,1x,I5,5x,A)", advance="no") &
           "Equivalence:", eq%left, eq%right, "Final state permutation:"
      call permutation_write (eq%perm, u)
      write (u, "(1x,12x,1x,A,1x)", advance="no") &
           "       msq permutation:  "
      call permutation_write (eq%msq_perm, u)
      write (u, "(1x,12x,1x,A,1x)", advance="no") &
           "       angle permutation:"
      call permutation_write (eq%angle_perm, u)
      write (u, "(1x,12x,1x,26x)", advance="no")
      do i = 1, size (eq%angle_sig)
         if (eq%angle_sig(i)) then
            write (u, "(1x,A)", advance="no") "+"
         else
            write (u, "(1x,A)", advance="no") "-"
         end if
      end do
      write (u, *) 
      if (associated (eq%next))  call equivalence_write_rec (eq%next, u)
    end subroutine equivalence_write_rec
  end subroutine equivalence_list_write

  elemental subroutine phs_grove_init &
       (grove, n_trees, n_in, n_out, n_masses, n_angles)
    type(phs_grove_t), intent(inout) :: grove
    integer, intent(in) :: n_trees, n_in, n_out, n_masses, n_angles
    grove%tree_count_offset = 0
    allocate (grove%tree (n_trees))
    call phs_tree_init (grove%tree, n_in, n_out, n_masses, n_angles)
  end subroutine phs_grove_init

  elemental subroutine phs_grove_final (grove)
    type(phs_grove_t), intent(inout) :: grove
    deallocate (grove%tree)
    call equivalence_list_final (grove%equivalence_list)
  end subroutine phs_grove_final

  subroutine phs_grove_assign0 (grove_out, grove_in)
    type(phs_grove_t), intent(out) :: grove_out
    type(phs_grove_t), intent(in) :: grove_in
    grove_out%tree_count_offset = grove_in%tree_count_offset
    if (allocated (grove_in%tree)) then
       allocate (grove_out%tree (size (grove_in%tree)))
       grove_out%tree = grove_in%tree
    end if
    grove_out%equivalence_list = grove_in%equivalence_list
  end subroutine phs_grove_assign0

  subroutine phs_grove_assign1 (grove_out, grove_in)
    type(phs_grove_t), dimension(:), intent(out) :: grove_out
    type(phs_grove_t), dimension(:), intent(in) :: grove_in
    integer :: i
    do i = 1, size (grove_in)
       call phs_grove_assign0 (grove_out(i), grove_in(i))
    end do
  end subroutine phs_grove_assign1

  subroutine phs_forest_init (forest, n_tree, n_in, n_out)
    type(phs_forest_t), intent(inout) :: forest
    integer, dimension(:), intent(in) :: n_tree
    integer, intent(in) :: n_in, n_out
    integer :: g, count
    forest%n_in = n_in
    forest%n_out = n_out
    forest%n_tot = n_in + n_out
    forest%n_masses = max (n_out - 2, 0)
    forest%n_angles = max (2*n_out - 2, 0)
    forest%n_dimensions = forest%n_masses + forest%n_angles
    forest%n_trees = sum (n_tree)
    forest%n_equivalences = 0
    allocate (forest%grove (size (n_tree)))
    call phs_grove_init &
         (forest%grove, n_tree, n_in, n_out, forest%n_masses, forest%n_angles)
    allocate (forest%grove_lookup (forest%n_trees))
    count = 0
    do g = 1, size (forest%grove)
       forest%grove(g)%tree_count_offset = count
       forest%grove_lookup (count+1:count+n_tree(g)) = g
       count = count + n_tree(g)
    end do
    allocate (forest%prt_in  (n_in))
    allocate (forest%prt_out (n_out))
    allocate (forest%prt (2**forest%n_tot - 1))
  end subroutine phs_forest_init

  subroutine phs_forest_final (forest)
    type(phs_forest_t), intent(inout) :: forest
    if (allocated (forest%grove)) then
       call phs_grove_final (forest%grove)
       deallocate (forest%grove)
    end if
    if (allocated (forest%grove_lookup))  deallocate (forest%grove_lookup)
    if (allocated (forest%prt))  deallocate (forest%prt)
  end subroutine phs_forest_final

  subroutine phs_forest_write (forest, unit)
    type(phs_forest_t), intent(in) :: forest
    integer, intent(in), optional :: unit
    integer :: u
    integer :: i, g
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Phase space forest:"
    write (u, *) "n_in  = ", forest%n_in
    write (u, *) "n_out = ", forest%n_out
    write (u, *) "n_tot = ", forest%n_tot
    write (u, *) "n_masses = ", forest%n_masses
    write (u, *) "n_angles = ", forest%n_angles
    write (u, *) "n_dim    = ", forest%n_dimensions
    write (u, *) "n_trees  = ", forest%n_trees
    write (u, *) "n_equiv  = ", forest%n_equivalences
    write (u, "(1x,A)", advance="no") "flavors  ="
    if (allocated (forest%flv)) then
       do i = 1, size (forest%flv)
          write (u, "(1x,I6)", advance="no")  flavor_get_pdg (forest%flv(i))
       end do
       write (u, *)
    else
       write (u, *) "[empty]"
    end if
    write (u, *) "Groves and trees:"
    if (allocated (forest%grove)) then
       do g = 1, size (forest%grove)
          write (u, "(1x,A,1x,I4)") "Grove ", g
          call phs_grove_write (forest%grove(g), unit)
       end do
    else
       write (u, *) "  [empty]"
    end if
    write (u, *) "Total number of equivalences: ", forest%n_equivalences
    write (u, *)
    write (u, *) "Incoming particles:"
    if (allocated (forest%prt_in)) then
       if (any (phs_prt_is_defined (forest%prt_in))) then
          do i = 1, size (forest%prt_in)
             if (phs_prt_is_defined (forest%prt_in(i))) then
                write (u, *)  "Particle", i
                call phs_prt_write (forest%prt_in(i), u)
             end if
          end do
       else
          write (u, "(3x,A)")  "[all undefined]"
       end if
    else
       write (u, *)  "  [empty]"
    end if
    write (u, *)
    write (u, *) "Outgoing particles:"
    if (allocated (forest%prt_out)) then
       if (any (phs_prt_is_defined (forest%prt_out))) then
          do i = 1, size (forest%prt_out)
             if (phs_prt_is_defined (forest%prt_out(i))) then
                write (u, *)  "Particle", i
                call phs_prt_write (forest%prt_out(i), u)
             end if
          end do
       else
          write (u, "(3x,A)")  "[all undefined]"
       end if
    else
       write (u, *)  "  [empty]"
    end if
    write (u, *)
    write (u, *) "Tree particles:"
    if (allocated (forest%prt)) then
       if (any (phs_prt_is_defined (forest%prt))) then
          do i = 1, size (forest%prt)
             if (phs_prt_is_defined (forest%prt(i))) then
                write (u, *)  "Particle", i
                call phs_prt_write (forest%prt(i), u)
             end if
          end do
       else
          write (u, "(3x,A)")  "[all undefined]"
       end if
    else
       write (u, *)  "  [empty]"
    end if
  end subroutine phs_forest_write

  subroutine phs_grove_write (grove, unit)
    type(phs_grove_t), intent(in) :: grove
    integer, intent(in), optional :: unit
    integer :: u
    integer :: t
    u = output_unit (unit);  if (u < 0)  return
    do t = 1, size (grove%tree)
       write (u, "(1x,A,1x,I4)") "Tree  ", t
       call phs_tree_write (grove%tree(t), unit)
    end do
    write (u, "(1x,A)") "Equivalence list:"
    call equivalence_list_write (grove%equivalence_list, unit)
  end subroutine phs_grove_write

  subroutine phs_forest_assign (forest_out, forest_in)
    type(phs_forest_t), intent(out) :: forest_out
    type(phs_forest_t), intent(in) :: forest_in
    forest_out%n_in  = forest_in%n_in
    forest_out%n_out = forest_in%n_out
    forest_out%n_tot = forest_in%n_tot
    forest_out%n_masses = forest_in%n_masses
    forest_out%n_angles = forest_in%n_angles
    forest_out%n_dimensions  = forest_in%n_dimensions
    forest_out%n_trees  = forest_in%n_trees
    forest_out%n_equivalences  = forest_in%n_equivalences
    if (allocated (forest_in%flv)) then
       allocate (forest_out%flv (size (forest_in%flv)))
       forest_out%flv = forest_in%flv
    end if
    if (allocated (forest_in%grove)) then
       allocate (forest_out%grove (size (forest_in%grove)))
       forest_out%grove = forest_in%grove
    end if
    if (allocated (forest_in%grove_lookup)) then
       allocate (forest_out%grove_lookup (size (forest_in%grove_lookup)))
       forest_out%grove_lookup = forest_in%grove_lookup
    end if
    if (allocated (forest_in%prt_in)) then
       allocate (forest_out%prt_in (size (forest_in%prt_in)))
       forest_out%prt_in = forest_in%prt_in
    end if
    if (allocated (forest_in%prt_out)) then
       allocate (forest_out%prt_out (size (forest_in%prt_out)))
       forest_out%prt_out = forest_in%prt_out
    end if
    if (allocated (forest_in%prt)) then
       allocate (forest_out%prt (size (forest_in%prt)))
       forest_out%prt = forest_in%prt
    end if
  end subroutine phs_forest_assign

  function phs_forest_get_n_parameters (forest) result (n)
    integer :: n
    type(phs_forest_t), intent(in) :: forest
    n = forest%n_dimensions
  end function phs_forest_get_n_parameters

  function phs_forest_get_n_channels (forest) result (n)
    integer :: n
    type(phs_forest_t), intent(in) :: forest
    n = forest%n_trees
  end function phs_forest_get_n_channels

  function phs_forest_get_n_groves (forest) result (n)
    integer :: n
    type(phs_forest_t), intent(in) :: forest
    n = size (forest%grove)
  end function phs_forest_get_n_groves

  subroutine phs_forest_get_grove_bounds (forest, g, i0, i1, n)
    type(phs_forest_t), intent(in) :: forest
    integer, intent(in) :: g
    integer, intent(out) :: i0, i1, n
    n = size (forest%grove(g)%tree)
    i0 = forest%grove(g)%tree_count_offset + 1
    i1 = forest%grove(g)%tree_count_offset + n
  end subroutine phs_forest_get_grove_bounds

  function phs_forest_get_n_equivalences (forest) result (n)
    integer :: n
    type(phs_forest_t), intent(in) :: forest
    n = forest%n_equivalences
  end function phs_forest_get_n_equivalences

  subroutine define_phs_forest_syntax (ifile)
    type(ifile_t) :: ifile
    call ifile_append (ifile, "SEQ phase_space_list = process_phase_space*")
    call ifile_append (ifile, "SEQ process_phase_space = " &
         // "process_def process_header phase_space")
    call ifile_append (ifile, "SEQ process_def = process process_list")
    call ifile_append (ifile, "KEY process")
    call ifile_append (ifile, "LIS process_list = process_tag*")
    call ifile_append (ifile, "IDE process_tag")
    call ifile_append (ifile, "SEQ process_header = " &
         // "md5sum_process = md5sum " &
         // "md5sum_model = md5sum " &
         // "md5sum_parameters = md5sum " &
         // "sqrts = real " &
         // "m_threshold_s = real " &
         // "m_threshold_t = real " &
         // "off_shell = integer " &
         // "t_channel = integer ")
    call ifile_append (ifile, "KEY '='")
    call ifile_append (ifile, "KEY md5sum_process")
    call ifile_append (ifile, "KEY md5sum_model")
    call ifile_append (ifile, "KEY md5sum_parameters")
    call ifile_append (ifile, "KEY sqrts")
    call ifile_append (ifile, "KEY m_threshold_s")
    call ifile_append (ifile, "KEY m_threshold_t")
    call ifile_append (ifile, "KEY off_shell")
    call ifile_append (ifile, "KEY t_channel")
    call ifile_append (ifile, "QUO md5sum = '""' ... '""'")
    call ifile_append (ifile, "REA real")
    call ifile_append (ifile, "INT integer")
    call ifile_append (ifile, "SEQ phase_space = grove_def+")
    call ifile_append (ifile, "SEQ grove_def = grove tree_def+")
    call ifile_append (ifile, "KEY grove")
    call ifile_append (ifile, "SEQ tree_def = tree bincodes mapping*")
    call ifile_append (ifile, "KEY tree")
    call ifile_append (ifile, "SEQ bincodes = bincode+")
    call ifile_append (ifile, "INT bincode")
    call ifile_append (ifile, "SEQ mapping = map bincode channel pdg")
    call ifile_append (ifile, "KEY map")
    call ifile_append (ifile, "ALT channel = s_channel | t_channel | u_channel | collinear | infrared | radiation")
    call ifile_append (ifile, "KEY s_channel")
    ! call ifile_append (ifile, "KEY t_channel")
    call ifile_append (ifile, "KEY u_channel")
    call ifile_append (ifile, "KEY collinear")
    call ifile_append (ifile, "KEY infrared")
    call ifile_append (ifile, "KEY radiation")
    call ifile_append (ifile, "INT pdg")
  end subroutine define_phs_forest_syntax

  subroutine syntax_phs_forest_init ()
    type(ifile_t) :: ifile
    call define_phs_forest_syntax (ifile)
    call syntax_init (syntax_phs_forest, ifile)
    call ifile_final (ifile)
  end subroutine syntax_phs_forest_init

  subroutine lexer_init_phs_forest (lexer)
    type(lexer_t), intent(out) :: lexer
    call lexer_init (lexer, &
         comment_chars = "#!", &
         quote_chars = '"', &
         quote_match = '"', &
         single_chars = "", &
         special_class = (/ "=" /) , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_phs_forest))
  end subroutine lexer_init_phs_forest

  subroutine syntax_phs_forest_final ()
    call syntax_final (syntax_phs_forest)
  end subroutine syntax_phs_forest_final

  subroutine syntax_phs_forest_write (unit)
    integer, intent(in), optional :: unit
    call syntax_write (syntax_phs_forest, unit)
  end subroutine syntax_phs_forest_write

  subroutine phs_forest_read_file &
       (forest, filename, process_id, n_in, n_out, model, found, &
        md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    type(phs_forest_t), intent(out) :: forest
    type(string_t), intent(in) :: filename
    type(string_t), intent(in) :: process_id
    integer, intent(in) :: n_in, n_out
    type(model_t), intent(in), target :: model
    logical, intent(out) :: found
    character(32), intent(in), optional :: &
         md5sum_process, md5sum_model, md5sum_parameters
    type(phs_parameters_t), intent(in), optional :: phs_par
    logical, intent(out), optional :: match
    type(parse_tree_t), target :: parse_tree
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    call lexer_init_phs_forest (lexer)
    call stream_init (stream, char (filename))
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_phs_forest, lexer)
    call phs_forest_read (forest, parse_tree, &
         process_id, n_in, n_out, model, found, &
         md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    call stream_final (stream)
    call lexer_final (lexer)
    call parse_tree_final (parse_tree)
  end subroutine phs_forest_read_file

  subroutine phs_forest_read_unit &
       (forest, unit, process_id, n_in, n_out, model, found, &
        md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    type(phs_forest_t), intent(out) :: forest
    integer, intent(in) :: unit
    type(string_t), intent(in) :: process_id
    integer, intent(in) :: n_in, n_out
    type(model_t), intent(in), target :: model
    logical, intent(out) :: found
    character(32), intent(in), optional :: &
         md5sum_process, md5sum_model, md5sum_parameters
    type(phs_parameters_t), intent(in), optional :: phs_par
    logical, intent(out), optional :: match
    type(parse_tree_t), target :: parse_tree
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    call lexer_init_phs_forest (lexer)
    call stream_init (stream, unit)
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_phs_forest, lexer)
    call phs_forest_read (forest, parse_tree, &
         process_id, n_in, n_out, model, found, &
         md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    call stream_final (stream)
    call lexer_final (lexer)
    call parse_tree_final (parse_tree)
  end subroutine phs_forest_read_unit

  subroutine phs_forest_read_parse_tree &
       (forest, parse_tree, process_id, n_in, n_out, model, found, &
        md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    type(phs_forest_t), intent(out) :: forest
    type(parse_tree_t), intent(in), target :: parse_tree
    type(string_t), intent(in) :: process_id
    integer, intent(in) :: n_in, n_out
    type(model_t), intent(in), target :: model
    logical, intent(out) :: found
    character(32), intent(in), optional :: &
         md5sum_process, md5sum_model, md5sum_parameters
    type(phs_parameters_t), intent(in), optional :: phs_par
    logical, intent(out), optional :: match
    type(parse_node_t), pointer :: node_header, node_phs, node_grove
    integer :: n_grove, g
    integer, dimension(:), allocatable :: n_tree
    integer :: t
!    call parse_tree_write (parse_tree)
    node_header => parse_tree_get_process_ptr (parse_tree, process_id)
    found = associated (node_header);  if (.not. found)  return
    if (present (match)) then
       call phs_forest_check_input (node_header, &
            md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
       if (.not. match)  return
    end if
    node_phs => parse_node_get_next_ptr (node_header)
    n_grove = parse_node_get_n_sub (node_phs)
    allocate (n_tree (n_grove))
    do g = 1, n_grove
       node_grove => parse_node_get_sub_ptr (node_phs, g)
       n_tree(g) = parse_node_get_n_sub (node_grove) - 1
    end do
    call phs_forest_init (forest, n_tree, n_in, n_out)
    do g = 1, n_grove
       node_grove => parse_node_get_sub_ptr (node_phs, g)
       do t = 1, n_tree(g)
          call phs_tree_set (forest%grove(g)%tree(t), &
               parse_node_get_sub_ptr (node_grove, t+1), model)
       end do
    end do
  end subroutine phs_forest_read_parse_tree

  subroutine phs_forest_check_input (pn_header, &
       md5sum_process, md5sum_model, md5sum_parameters, phs_par, match)
    type(parse_node_t), intent(in), target :: pn_header
    character(32), intent(in) :: &
         md5sum_process, md5sum_model, md5sum_parameters
    type(phs_parameters_t), intent(in) :: phs_par
    logical, intent(out) :: match
    type(parse_node_t), pointer :: pn_md5sum, pn_rval, pn_ival
    character(32) :: md5sum
    type(phs_parameters_t) :: phs_par_old
    pn_md5sum => parse_node_get_sub_ptr (pn_header, 3)
    md5sum = parse_node_get_string (pn_md5sum)
    if (md5sum /= "" .and. md5sum /= md5sum_process) then
       call msg_message ("Rebuilding phase space (process has changed)")
       match = .false.;  return
    end if
    pn_md5sum => parse_node_get_next_ptr (pn_md5sum, 3)
    md5sum = parse_node_get_string (pn_md5sum)
    if (md5sum /= "" .and. md5sum /= md5sum_model) then
       call msg_message ("Rebuilding phase space (model has changed)")
       match = .false.;  return
    end if
    pn_md5sum => parse_node_get_next_ptr (pn_md5sum, 3)
    md5sum = parse_node_get_string (pn_md5sum)
    if (md5sum /= "" .and. md5sum /= md5sum_parameters) then
       call msg_message &
            ("Rebuilding phase space (model parameters have changed)")
       match = .false.;  return
    end if
    pn_rval => parse_node_get_next_ptr (pn_md5sum, 3)
    phs_par_old%sqrts = parse_node_get_real (pn_rval)
    pn_rval => parse_node_get_next_ptr (pn_rval, 3)
    phs_par_old%m_threshold_s = parse_node_get_real (pn_rval)
    pn_rval => parse_node_get_next_ptr (pn_rval, 3)
    phs_par_old%m_threshold_t = parse_node_get_real (pn_rval)
    pn_ival => parse_node_get_next_ptr (pn_rval, 3)
    phs_par_old%off_shell = parse_node_get_integer (pn_ival)
    pn_ival => parse_node_get_next_ptr (pn_ival, 3)
    phs_par_old%t_channel = parse_node_get_integer (pn_ival)
    if (phs_par_old /= phs_par) then
       call msg_message &
            ("Rebuilding phase space (phase-space parameters have changed)")
       match = .false.;  return
    end if
    match = .true.
  end subroutine phs_forest_check_input

  subroutine phs_tree_set (tree, node, model)
    type(phs_tree_t), intent(inout) :: tree
    type(parse_node_t), intent(in), target :: node
    type(model_t), intent(in), target :: model
    type(parse_node_t), pointer :: node_bincodes, node_mapping
    integer :: n_bincodes
    integer(TC), dimension(:), allocatable :: bincode
    integer :: b, n_mappings, m
    integer(TC) :: k
    type(string_t) :: type
    integer :: pdg
    node_bincodes => parse_node_get_sub_ptr (node, 2)
    n_bincodes = parse_node_get_n_sub (node_bincodes)
    allocate (bincode (n_bincodes))
    do b = 1, n_bincodes
       bincode(b) = parse_node_get_integer &
            (parse_node_get_sub_ptr (node_bincodes, b))
    end do
    call phs_tree_from_array (tree, bincode)
    call phs_tree_flip_t_to_s_channel (tree)
    call phs_tree_canonicalize (tree)
    n_mappings = parse_node_get_n_sub (node) - 2
    do m = 1, n_mappings
       node_mapping => parse_node_get_sub_ptr (node, m+2)
       k = parse_node_get_integer &
            (parse_node_get_sub_ptr (node_mapping, 2))
       type = parse_node_get_key &
            (parse_node_get_sub_ptr (node_mapping, 3))
       pdg = parse_node_get_integer &
            (parse_node_get_sub_ptr (node_mapping, 4))
       call phs_tree_init_mapping (tree, k, type, pdg, model)
    end do
  end subroutine phs_tree_set

  subroutine phs_forest_set_flavors (forest, flv)
    type(phs_forest_t), intent(inout) :: forest
    type(flavor_t), dimension(:), intent(in) :: flv
    allocate (forest%flv (size (flv)))
    forest%flv = flv
  end subroutine phs_forest_set_flavors

  subroutine phs_forest_set_parameters &
       (forest, mapping_defaults, variable_limits)
    type(phs_forest_t), intent(inout) :: forest
    type(mapping_defaults_t), intent(in) :: mapping_defaults
    logical, intent(in) :: variable_limits
    integer :: g, t
    do g = 1, size (forest%grove)
       do t = 1, size (forest%grove(g)%tree)
          call phs_tree_set_mass_sum &
               (forest%grove(g)%tree(t), forest%flv(forest%n_in+1:))
          call phs_tree_set_mapping_parameters (forest%grove(g)%tree(t), &
               mapping_defaults, variable_limits)
       end do
    end do
  end subroutine phs_forest_set_parameters

  subroutine phs_forest_set_prt_in (forest, int, lt_cm_to_lab)
    type(phs_forest_t), intent(inout) :: forest
    type(interaction_t), intent(in) :: int
    type(lorentz_transformation_t), intent(in), optional :: lt_cm_to_lab
    if (present (lt_cm_to_lab)) then
       call phs_prt_set_momentum (forest%prt_in, &
            inverse (lt_cm_to_lab) * &
            interaction_get_momenta (int, outgoing=.false.))
    else
       call phs_prt_set_momentum (forest%prt_in, &
            interaction_get_momenta (int, outgoing=.false.))
    end if
    call phs_prt_set_msq (forest%prt_in, &
         flavor_get_mass (forest%flv(:forest%n_in)) ** 2)
    call phs_prt_set_defined (forest%prt_in)
  end subroutine phs_forest_set_prt_in

  subroutine phs_forest_get_prt_out (forest, int, lt_cm_to_lab)
    type(phs_forest_t), intent(in) :: forest
    type(interaction_t), intent(inout) :: int
    type(lorentz_transformation_t), intent(in), optional :: lt_cm_to_lab
    if (present (lt_cm_to_lab)) then
       call interaction_set_momenta (int, &
            lt_cm_to_lab * &
            phs_prt_get_momentum (forest%prt_out), outgoing=.true.)
    else
       call interaction_set_momenta (int, &
            phs_prt_get_momentum (forest%prt_out), outgoing=.true.)
    end if
  end subroutine phs_forest_get_prt_out

  subroutine phs_grove_set_equivalences (grove, perm_array)
    type(phs_grove_t), intent(inout) :: grove
    type(permutation_t), dimension(:), intent(in) :: perm_array
    type(equivalence_t), pointer :: eq
    integer :: t1, t2, i
    do t1 = 1, size (grove%tree)
       do t2 = 1, size (grove%tree)
          SCAN_PERM: do i = 1, size (perm_array)
             if (phs_tree_equivalent &
                  (grove%tree(t1), grove%tree(t2), perm_array(i))) then
                call equivalence_list_add &
                     (grove%equivalence_list, t1, t2, perm_array(i))
                eq => grove%equivalence_list%last
                call phs_tree_find_msq_permutation &
                     (grove%tree(t1), grove%tree(t2), eq%perm, &
                      eq%msq_perm)
                call phs_tree_find_angle_permutation &
                     (grove%tree(t1), grove%tree(t2), eq%perm, &
                      eq%angle_perm, eq%angle_sig)
             end if
          end do SCAN_PERM
       end do
    end do
  end subroutine phs_grove_set_equivalences

  subroutine phs_forest_set_equivalences (forest)
    type(phs_forest_t), intent(inout) :: forest
    type(permutation_t), dimension(:), allocatable :: perm_array
    integer :: i
    call permutation_array_make &
         (perm_array, flavor_get_pdg (forest%flv(forest%n_in+1:)))
    do i = 1, size (forest%grove)
       call phs_grove_set_equivalences (forest%grove(i), perm_array)
    end do
    forest%n_equivalences = sum (forest%grove%equivalence_list%length)
  end subroutine phs_forest_set_equivalences

  subroutine phs_forest_setup_vamp_equivalences &
       (forest, n_dim_extra, externally_generated, azimuthal_dependence, &
        vamp_eq)
    type(phs_forest_t), intent(in) :: forest
    integer, intent(in) :: n_dim_extra
    logical, dimension(n_dim_extra), intent(in) :: externally_generated
    logical, intent(in) :: azimuthal_dependence
    type(vamp_equivalences_t), intent(out) :: vamp_eq
    integer :: n_equivalences, n_channels, n_dim, n_masses, n_angles
    integer, dimension(forest%n_dimensions + n_dim_extra) :: perm, mode
    integer :: mode_azimuthal_angle
    type(equivalence_t), pointer :: eq
    integer :: i, j, g
    integer :: left, right
    n_equivalences = forest%n_equivalences
    n_channels = forest%n_trees
    n_dim = forest%n_dimensions
    n_masses = forest%n_masses
    n_angles = forest%n_angles
    call vamp_equivalences_init &
         (vamp_eq, n_equivalences, n_channels, n_dim + n_dim_extra)
    if (azimuthal_dependence) then
       mode_azimuthal_angle = VEQ_IDENTITY
    else
       mode_azimuthal_angle = VEQ_INVARIANT
    end if
    g = 0
    eq => null ()
    do i = 1, n_equivalences
       if (.not. associated (eq)) then
          g = g + 1
          eq => forest%grove(g)%equivalence_list%first
       end if
       do j = 1, n_masses
          perm(j) = permute (j, eq%msq_perm)
          mode(j) = VEQ_IDENTITY
       end do
       do j = 1, n_angles
          perm(n_masses+j) = n_masses + permute (j, eq%angle_perm)
          if (j == 1) then
             mode(n_masses+j) = mode_azimuthal_angle   ! first azimuthal angle
          else if (mod(j,2) == 1) then
             mode(n_masses+j) = VEQ_SYMMETRIC          ! other azimuthal angles
          else if (eq%angle_sig(j)) then
             mode(n_masses+j) = VEQ_IDENTITY           ! polar angle +
          else
             mode(n_masses+j) = VEQ_INVERT             ! polar angle -
          end if
       end do
       do  j = 1, n_dim_extra
          perm(n_dim+j) = n_dim + j
          if (externally_generated(j)) then
             mode(n_dim+j) = VEQ_INVARIANT
          else
             mode(n_dim+j) = VEQ_IDENTITY
          end if
       end do
       left = eq%left + forest%grove(g)%tree_count_offset
       right = eq%right + forest%grove(g)%tree_count_offset
       call vamp_equivalence_set (vamp_eq, i, left, right, perm, mode)
       eq => eq%next
    end do
    call vamp_equivalences_complete (vamp_eq)
  end subroutine phs_forest_setup_vamp_equivalences

  subroutine phs_forest_evaluate_phase_space &
       (forest, channel, active, sqrts, x, factor, volume, ok)
    type(phs_forest_t), intent(inout) :: forest
    integer, intent(in) :: channel
    logical, dimension(:), intent(in) :: active
    real(default), intent(in) :: sqrts
    real(default), dimension(:,:), intent(inout) :: x
    real(default), dimension(:), intent(out) :: factor
    real(default), intent(out) :: volume
    logical, intent(out) :: ok
    integer :: g, t, ch
    integer(TC) :: k, k_root, k_in
    g = forest%grove_lookup (channel)
    t = channel - forest%grove(g)%tree_count_offset
    call phs_prt_set_undefined (forest%prt)
    call phs_prt_set_undefined (forest%prt_out)
    k_in = forest%n_tot
    forall (k = 1:forest%n_in)
       forest%prt(ibset(0,k_in-k)) = forest%prt_in(k)
    end forall
    do k = 1, forest%n_out
       call phs_prt_set_msq (forest%prt(ibset(0,k-1)), &
            flavor_get_mass (forest%flv(forest%n_in+k)) ** 2)
    end do
    k_root = 2**forest%n_out - 1
    select case (forest%n_in)
    case (1)
       forest%prt(k_root) = forest%prt_in(1)
    case (2)
       call phs_prt_combine &
            (forest%prt(k_root), forest%prt_in(1), forest%prt_in(2))
    end select
    call phs_tree_compute_momenta_from_x (forest%grove(g)%tree(t), &
         forest%prt, factor(channel), volume, sqrts, x(:,channel), ok)
    if (ok) then
       ch = 0
       do g = 1, size (forest%grove)
          do t = 1, size (forest%grove(g)%tree)
             ch = ch + 1
             if (ch == channel)  cycle
             if (active(ch)) then
                call phs_tree_combine_particles &
                     (forest%grove(g)%tree(t), forest%prt)
                call phs_tree_compute_x_from_momenta &
                     (forest%grove(g)%tree(t), &
                      forest%prt, factor(ch), sqrts, x(:,ch))
             end if
          end do
       end do
       forall (k = 1:forest%n_out)
          forest%prt_out(k) = forest%prt(ibset(0,k-1))
       end forall
    end if
  end subroutine phs_forest_evaluate_phase_space

  subroutine phs_forest_test ()
    use os_interface, only: os_data_t
    type(os_data_t) :: os_data
    type(phs_forest_t) :: forest
    type(model_t), pointer :: model
    type(string_t) :: process_id
    type(flavor_t), dimension(5) :: flv
    type(vamp_equivalences_t) :: vamp_eq
    type(string_t) :: filename
    type(interaction_t) :: int
    integer :: unit
    integer, parameter :: u = 20
    integer, parameter :: n_dim_extra = 2
    logical, dimension(2), parameter :: &
         externally_generated = (/ .true., .false. /)
    logical, parameter :: azimuthal_dependence = .false.
    type(mapping_defaults_t) :: mapping_defaults
    logical :: found_process, ok
    integer :: channel, ch
    logical, dimension(4) :: active = .true.
    real(default) :: sqrts = 1000
    real(default), dimension(5,4) :: x
    real(default), dimension(4) :: factor
    real(default) :: volume
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("QCD"), var_str("test.mdl"), os_data, model)
    call syntax_model_file_final ()
    print *
    print *, "*** Create phase-space file 'test.phs'"
    call flavor_init (flv, (/ 11, -11, 11, -11, 22 /), model)
    open (file="test.phs", unit=u, action="write")
    write (u, *) "process foo"
    write (u, *) "  grove"
    write (u, *) "    tree 3 7"
    write (u, *) "      map 3 s_channel 23"
    write (u, *) "    tree 5 7"
    write (u, *) "    tree 6 7"
    write (u, *) "  grove"
    write (u, *) "    tree 9 11"
    write (u, *) "      map 9 t_channel 22"
    close (u)
    print *
    print *, "*** Read phase-space file 'test.phs'"
    call syntax_phs_forest_init ()
    process_id = "foo"
    unit = free_unit ()
    filename = "test.phs"
    call phs_forest_read &
         (forest, filename, process_id, 2, 3, model, found_process)
    print *
    print *, "*** Set parameters, flavors, equiv, momenta"
    call phs_forest_set_flavors (forest, flv)
    call phs_forest_set_parameters (forest, mapping_defaults, .false.)
    call phs_forest_set_equivalences (forest)
    call interaction_init (int, 2, 0, 3)
    call interaction_set_momentum (int, &
         vector4_moving (500._default, 500._default, 3), 1)
    call interaction_set_momentum (int, &
         vector4_moving (500._default,-500._default, 3), 2)
    call phs_forest_set_prt_in (forest, int)
    channel = 2
    x = 0
    x(:,channel) = (/ 0.3, 0.4, 0.1, 0.9, 0.6 /)
1   format (5(1x,G12.5))
    print *, "Input values:"
    print 1, x(:,channel)
    print *
    print *, "*** Evaluate phase space"
    call phs_forest_evaluate_phase_space (forest, &
         channel, active, sqrts, x, factor, volume, ok)
    call phs_forest_get_prt_out (forest, int)
    print *, "Output values:"
    do ch = 1, 4
       print 1, x(:,ch)
    end do
    call interaction_write (int)
    print *, "factors:"
    print 1, factor
    print *, "volume:"
    print 1, volume
    call phs_forest_write (forest)
    print *
    print *, "*** Compute equivalences"
    call phs_forest_setup_vamp_equivalences (forest, &
         n_dim_extra, externally_generated, azimuthal_dependence, &
         vamp_eq)
    call vamp_equivalences_write (vamp_eq)
    print *
    print *, "*** Cleanup"
    call vamp_equivalences_final (vamp_eq)
    call phs_forest_final (forest)
    call syntax_phs_forest_final ()
  end subroutine phs_forest_test


end module phs_forests
