! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module blha_config

  use iso_varying_string, string_t => varying_string !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use md5
  use models
  use flavors
  use quantum_numbers
  use pdg_arrays
  use sorting
  use lexers
  use parser
  use syntax_rules
  use ifiles
  use limits, only: EOF !NODEP!

  implicit none
  private

  public :: blha_configuration_t
  public :: blha_cfg_process_node_t
  public :: blha_configuration_init
  public :: blha_configuration_final
  public :: blha_configuration_append_process
  public :: blha_configuration_set
  public :: blha_configuration_write
  public :: blha_configuration_freeze
  public :: blha_read_contract
  public :: syntax_blha_contract_init
  public :: syntax_blha_contract_final
  public :: blha_config_test

  integer, public, parameter :: &
       BLHA_MEST_SUM=1, BLHA_MEST_AVG=2, BLHA_MEST_OTHER=3
  integer, public, parameter :: &
       BLHA_CT_QCD=1, BLHA_CT_EW=2, BLHA_CT_QED=3, BLHA_CT_OTHER=4
  integer, public, parameter :: &
       BLHA_IRREG_CDR=1, BLHA_IRREG_DRED=2, BLHA_IRREG_THV=3, &
       BLHA_IRREG_MREG=4, BLHA_IRREG_OTHER=5
  integer, public, parameter :: &
       BLHA_SUBMODE_NONE = 1, BLHA_SUBMODE_OTHER = 2
  integer, public, parameter :: &
       BLHA_MPS_ONSHELL=1, BLHA_MPS_OTHER=2
  integer, public, parameter :: &
       BLHA_MODE_GOSAM=1, BLHA_MODE_FEYNARTS = 2, BLHA_MODE_GENERIC=3
  integer, public, parameter :: &
       BLHA_OM_NONE=1, BLHA_OM_NOCPL=2, BLHA_OM_OTHER=3
  

  type :: blha_cfg_process_node_t
     integer, dimension(:), allocatable :: pdg_in, pdg_out
     integer, dimension(:), allocatable :: fingerprint
     integer :: nsub
     integer, dimension(:), allocatable :: ids
     type(blha_cfg_process_node_t), pointer :: next => null ()
  end type blha_cfg_process_node_t

  type :: blha_configuration_t
     type(string_t) :: name
     type(model_t), pointer :: model
     type(string_t) :: md5
     logical :: dirty = .true.
     integer :: n_proc = 0
     integer :: mode = BLHA_MODE_GENERIC
     type(blha_cfg_process_node_t), pointer :: processes => null ()
     integer, dimension(2) :: matrix_element_square_type = BLHA_MEST_SUM
     type(string_t), dimension (2) :: matrix_element_square_type_other
     integer :: correction_type = BLHA_CT_QCD
     type(string_t) :: correction_type_other
     integer :: irreg = BLHA_IRREG_THV
     type(string_t) :: irreg_other
     integer :: massive_particle_scheme = BLHA_MPS_ONSHELL
     type(string_t) :: massive_particle_scheme_other
     integer :: subtraction_mode = BLHA_SUBMODE_NONE
     type(string_t) :: subtraction_mode_other
     type(string_t) :: model_file
     logical :: subdivide_subprocesses = .false.
     integer :: alphas_power = -1, alpha_power = -1
     integer :: operation_mode = BLHA_OM_NONE
     type(string_t) :: operation_mode_other
  end type blha_configuration_t


  type(syntax_t), target, save :: syntax_blha_contract

  interface blha_read_contract
     module procedure blha_read_contract_unit, &
          blha_read_contract_file
  end interface blha_read_contract 
  

contains

  subroutine blha_configuration_init (cfg, name, model, mode)
    type(blha_configuration_t), intent(out) :: cfg
    type(string_t), intent(in) :: name
    type(model_t), target, intent(in) :: model
    integer, intent(in), optional :: mode
    cfg%name = name
    cfg%model => model
    if (present (mode)) cfg%mode = mode
  end subroutine blha_configuration_init

  subroutine blha_configuration_final (cfg)
    type(blha_configuration_t), intent(inout) :: cfg
    type(blha_cfg_process_node_t), pointer :: cur, next
    cur => cfg%processes
    do while (associated (cur))
       next => cur%next
       deallocate (cur)
       nullify (cur)
       cur => next
    end do
  end subroutine blha_configuration_final

  subroutine sort_processes (list, n)
    type(blha_cfg_process_node_t), pointer :: list
    integer, intent(in), optional :: n
    type :: pnode
       type(blha_cfg_process_node_t), pointer :: p
    end type pnode
    type(pnode), dimension(:), allocatable :: array
    integer :: count, i, s, i1, i2, i3
    type(blha_cfg_process_node_t), pointer :: node
    if (present (n)) then
       count = n
    else
       node => list
       count = 0
       do while (associated (node))
          node => node%next
          count = count + 1
       end do
    end if
    ! Store list nodes into an array
    if (count == 0) return
    allocate (array(count))
    i = 1
    node => list
    do i = 1, count
       array(i)%p => node
       node => node%next
    end do
    s = 1
    ! Merge sort the array
    do while (s < count)
       i = 0
       i1 = 1
       i2 = s
       do while (i2 < count)
          i3 = min (s*(i+2), count)
          array(i1:i3) = merge (array(i1:i2), array(i2+1:i3))
          i = i + 2
          i1 = s*i+1
          i2 = s*(i+1)
       end do
       s = s * 2
    end do
    ! Relink according to their new order
    list => array(1)%p
    nullify (array(count)%p%next)
    node => list
    do i = 2, count
       node%next => array(i)%p
       node => node%next
    end do

  contains

    ! .le. comparision
    function lt (n1, n2) result (predicate)
      type(blha_cfg_process_node_t), intent(in) :: n1, n2
      logical :: predicate
      integer :: i
      predicate = .true.
      do i = 1, size (n1%fingerprint)
         if (n1%fingerprint(i) < n2%fingerprint(i)) return
         if (n1%fingerprint(i) > n2%fingerprint(i)) then
            predicate = .false.
            return
         end if
      end do
    end function lt

    ! Sorting core --- merge two sorted chunks
    function merge (l1, l2) result (lo)
      type(pnode), dimension(:), intent(in) :: l1, l2
      type(pnode), dimension(size (l1) + size (l2)) :: lo
      integer :: i, i1, i2
      i1 = 1
      i2 = 1
      do i = 1, size (lo)
         if (i1 > size (l1)) then
            lo(i)%p => l2(i2)%p
            i2 = i2 + 1
         elseif (i2 > size (l2)) then
            lo(i)%p => l1(i1)%p
            i1 = i1 + 1
         elseif (lt (l1(i1)%p, l2(i2)%p)) then
            lo(i)%p => l1(i1)%p
            i1 = i1 + 1
         else
            lo(i)%p => l2(i2)%p
            i2 = i2 + 1
         end if
      end do
    end function merge

  end subroutine sort_processes
  
  subroutine blha_configuration_append_process (cfg, pdg_in, pdg_out, nsub, ids)
    type(blha_configuration_t), intent(inout) :: cfg
    type(pdg_array_t), dimension(:), intent(in) :: pdg_in, pdg_out
    integer, optional, intent(in) :: nsub
    integer, optional, dimension(:), intent(in) :: ids
    type(blha_cfg_process_node_t), pointer :: root, node, tmp
    ! Multiindex for counting through the PDG numbers
    integer, dimension(size (pdg_in)) :: i_in
    integer, dimension(size (pdg_out)) :: i_out
    ! Handle the list of lists
    type :: ilist
       integer, dimension(:), allocatable :: i
    end type ilist
    type(ilist), dimension(size (pdg_in)) :: ilist_i
    type(ilist), dimension(size (pdg_out)) :: ilist_o
    integer :: i, j, nproc
    logical :: inc
    ! Extract PDGs into integer lists
    do i = 1, size (pdg_in)
       ilist_i(i)%i = pdg_in(i)
    end do
    do i = 1, size (pdg_out)
       ilist_o(i)%i = pdg_out(i)
    end do
    i_in = 1
    i_out = 1
    allocate (root)
    node => root
    ! Perform the expansion
    nproc = 0
    EXPAND: do
       ! Transfer the PDG selection...
       allocate (node%pdg_in(size (pdg_in)))
       allocate (node%pdg_out(size (pdg_out)))
       allocate (node%fingerprint (size (pdg_in) + size (pdg_out)))
       if (present (nsub)) node%nsub = nsub
       if (present (ids)) then
          allocate (node%ids(size (ids)))
          node%ids = ids
       end if
       forall (j=1:size(ilist_i)) &
          node%pdg_in(j) = ilist_i(j)%i(i_in(j))
       forall (j=1:size(ilist_o)) &
          node%pdg_out(j) = ilist_o(j)%i(i_out(j))
       node%fingerprint = [ node%pdg_in, sort (node%pdg_out) ]
       nproc = nproc + 1
       inc = .false.
       ! ... and increment the multiindex
       do j = 1, size (i_out)
          if (i_out(j) < size (ilist_o(j)%i)) then
             i_out(j) = i_out(j) + 1
             inc = .true.
             exit
          else
             i_out(j) = 1
          end if
       end do
       if (.not. inc) then
          do j = 1, size (i_in)
             if (i_in(j) < size (ilist_i(j)%i)) then
                i_in(j) = i_in(j) + 1
                inc = .true.
                exit
             else
                i_in(j) = 1
             end if
          end do
       end if
       if (.not. inc) exit EXPAND
       allocate (node%next)
       node => node%next
    end do EXPAND
    ! Do the sorting
    call sort_processes (root, nproc)
    ! Kill duplicates
    node => root
    do while (associated (node))
       if (.not. associated (node%next)) exit
       if (all (node%fingerprint == node%next%fingerprint)) then
          tmp => node%next%next
          deallocate (node%next)
          node%next => tmp
          nproc = nproc - 1
       else
          node => node%next
       end if
    end do
    ! Append the remaining list
    if (associated (cfg%processes)) then
       node => cfg%processes
       do while (associated (node%next))
          node => node%next
       end do
       node%next => root
    else
       cfg%processes => root
    end if
    cfg%n_proc = cfg%n_proc + nproc
    cfg%dirty = .true.
    
  end subroutine blha_configuration_append_process

  subroutine blha_configuration_set ( cfg, &
       matrix_element_square_type_hel, matrix_element_square_type_hel_other, &
       matrix_element_square_type_col, matrix_element_square_type_col_other, &
       correction_type, correction_type_other, &
       irreg, irreg_other, &
       massive_particle_scheme, massive_particle_scheme_other, &
       subtraction_mode, subtraction_mode_other, &
       model_file, subdivide_subprocesses, alphas_power, alpha_power, &
       operation_mode, operation_mode_other)
    type(blha_configuration_t), intent(inout) :: cfg
    integer, optional, intent(in) :: matrix_element_square_type_hel
    type(string_t), optional, intent(in) :: matrix_element_square_type_hel_other
    integer, optional, intent(in) :: matrix_element_square_type_col
    type(string_t), optional, intent(in) :: matrix_element_square_type_col_other
    integer, optional, intent(in) :: correction_type
    type(string_t), optional, intent(in) :: correction_type_other
    integer, optional, intent(in) :: irreg
    type(string_t), optional, intent(in) :: irreg_other
    integer, optional, intent(in) :: massive_particle_scheme
    type(string_t), optional, intent(in) :: massive_particle_scheme_other
    integer, optional, intent(in) :: subtraction_mode
    type(string_t), optional, intent(in) :: subtraction_mode_other
    type(string_t), optional, intent(in) :: model_file
    logical, optional, intent(in) :: subdivide_subprocesses
    integer, intent(in), optional :: alphas_power, alpha_power
    integer, intent(in), optional :: operation_mode
    type(string_t), intent(in), optional :: operation_mode_other
    if (present (matrix_element_square_type_hel)) &
       cfg%matrix_element_square_type(1) = matrix_element_square_type_hel
    if (present (matrix_element_square_type_hel_other)) &
       cfg%matrix_element_square_type_other(1) = matrix_element_square_type_hel_other
    if (present (matrix_element_square_type_col)) &
       cfg%matrix_element_square_type(2) = matrix_element_square_type_col
    if (present (matrix_element_square_type_col_other)) &
       cfg%matrix_element_square_type_other(2) = matrix_element_square_type_col_other
    if (present (correction_type)) &
       cfg%correction_type = correction_type
    if (present (correction_type_other)) &
       cfg%correction_type_other = correction_type_other
    if (present (irreg)) &
       cfg%irreg = irreg
    if (present (irreg_other)) &
       cfg%irreg_other = irreg_other
    if (present (massive_particle_scheme)) &
       cfg%massive_particle_scheme = massive_particle_scheme
    if (present (massive_particle_scheme_other)) &
       cfg%massive_particle_scheme_other = massive_particle_scheme_other
    if (present (subtraction_mode)) &
       cfg%subtraction_mode = subtraction_mode
    if (present (subtraction_mode_other)) &
       cfg%subtraction_mode_other = subtraction_mode_other
    if (present (model_file)) &
       cfg%model_file = model_file
    if (present (subdivide_subprocesses)) &
       cfg%subdivide_subprocesses = subdivide_subprocesses
    if (present (alphas_power)) &
       cfg%alphas_power = alphas_power
    if (present (alpha_power)) &
       cfg%alpha_power = alpha_power
    if (present (operation_mode)) &
       cfg%operation_mode = operation_mode
    if (present (operation_mode_other)) &
       cfg%operation_mode_other = operation_mode_other
    cfg%dirty = .true.
  end subroutine blha_configuration_set

  subroutine blha_configuration_write (cfg, unit, internal)
    type(blha_configuration_t), intent(in) :: cfg
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: internal
    integer :: u
    logical :: full
    type(string_t) :: buf
    type(blha_cfg_process_node_t), pointer :: node
    integer :: i
    character(3) :: pdg_char
    character(len=25), parameter :: pad = ""
    u = output_unit (unit)
    full = .true.; if (present (internal)) full = .not. internal
    if (full .and. cfg%dirty) call msg_bug ( &
       "BUG: attempted to write out a dirty BLHA configuration")
    if (full) then
       write (u,'(A)') "# BLHA order written by WHIZARD 2.2.2"
       write (u,'(A)')
    end if
    select case (cfg%mode)
       case (BLHA_MODE_GOSAM); buf = "GoSam"
       case default; buf = "vanilla"
    end select
    write (u,'(A)') "# BLHA interface mode: " // char (buf)
    write (u,'(A)') "# process: " // char (cfg%name)
    write (u,'(A)') "# model: " // char (cfg%model%get_name ())
    if (full) then
       write (u,'(A)')
       write (u,'(A)') '#@WO MD5 "' // char (cfg%md5) // '"'
       write (u,'(A)')
    end if
    if (all (cfg%matrix_element_square_type == BLHA_MEST_SUM)) then
       buf = "CHsummed"
    elseif (all (cfg%matrix_element_square_type == BLHA_MEST_AVG)) then
       buf = "CHaveraged"
    else
       buf = (render_mest ("H", cfg%matrix_element_square_type(1), &
             cfg%matrix_element_square_type_other(1)) // " ") // &
          render_mest ("C", cfg%matrix_element_square_type(2), &
             cfg%matrix_element_square_type_other(2))
    end if
    write (u,'(A25,A)') "MatrixElementSquareType" // pad, char (buf)
    select case (cfg%correction_type)
       case (BLHA_CT_QCD); buf = "QCD"
       case (BLHA_CT_EW); buf = "EW"
       case (BLHA_CT_QED); buf = "QED"
       case default; buf = cfg%correction_type_other
    end select
    write (u,'(A25,A)') "CorrectionType" // pad, char (buf)
    select case (cfg%irreg)
       case (BLHA_IRREG_CDR); buf = "CDR"
       case (BLHA_IRREG_DRED); buf = "DRED"
       case (BLHA_IRREG_THV); buf = "tHV"
       case (BLHA_IRREG_MREG); buf = "MassReg"
       case default; buf = cfg%irreg_other
    end select
    write (u,'(A25,A)') "IRregularisation" // pad, char (buf)
    select case (cfg%massive_particle_scheme)
       case (BLHA_MPS_ONSHELL); buf = "OnShell"
       case default; buf = cfg%massive_particle_scheme_other
    end select
    write (u,'(A25,A)') "MassiveParticleScheme" // pad, char (buf)
    select case (cfg%subtraction_mode)
       case (BLHA_SUBMODE_NONE); buf = "None"
       case default; buf = cfg%subtraction_mode_other
    end select
    write (u,'(A25,A)') "IRsubtractionMethod" // pad, char (buf)
    write (u,'(A25,A)') "ModelFile" // pad, char (cfg%model_file)
    if (cfg%subdivide_subprocesses) then
       write (u,'(A25,A)') "SubdivideSubprocesses" // pad, "yes"
    else
       write (u,'(A25,A)') "SubdivideSubprocess" // pad, "no"
    end if
    if (cfg%alphas_power >= 0) write (u,'(A25,A)') &
       "AlphasPower" // pad, int2char (cfg%alphas_power)
    if (cfg%alpha_power >= 0) write (u,'(A25,A)') &
       "AlphaPower " // pad, int2char (cfg%alpha_power)
    if (full) then
       write (u,'(A)')
       write (u,'(A)') "# Process definitions"
       write (u,'(A)')
    end if
    node => cfg%processes
    do while (associated (node))
       buf = ""
       do i = 1, size (node%pdg_in)
          write (pdg_char,'(I3)') node%pdg_in(i)
          buf = (buf // pdg_char) // " "
       end do
       buf = buf // "-> "
       do i = 1, size (node%pdg_out)
          write (pdg_char,'(I3)') node%pdg_out(i)
          buf = (buf // pdg_char) // " "
       end do
       write (u,'(A)') char (trim (buf))
       node => node%next
    end do

  contains

    function render_mest (prefix, mest, other) result (tag)
      character, intent(in) :: prefix
      integer, intent(in) :: mest
      type(string_t), intent(in) :: other
      type(string_t) :: tag
      select case (mest)
      case (BLHA_MEST_AVG); tag = prefix // "averaged"
      case (BLHA_MEST_SUM); tag = prefix // "summed"
      case default; tag = other
      end select
    end function render_mest

  end subroutine blha_configuration_write

  subroutine blha_configuration_freeze (cfg)
    type(blha_configuration_t), intent(inout) :: cfg
    integer :: u
    if (.not. cfg%dirty) return
    call sort_processes (cfg%processes)
    u = free_unit ()
    open (unit=u, status="scratch", action="readwrite")
    call blha_configuration_write (cfg, u, internal=.true.)
    rewind (u)
    cfg%md5 = md5sum (u)
    cfg%dirty = .false.
    close (u)
  end subroutine blha_configuration_freeze

  subroutine blha_read_contract_file (cfg, ok, fname, success)
    type(blha_configuration_t), intent(inout) :: cfg
    logical, intent(out) :: ok
    type(string_t), intent(in) :: fname
    logical, intent(out), optional :: success
    integer :: u, stat
    u = free_unit ()
    open (u, file=char (fname), status="old", action="read", iostat=stat)
    if (stat /= 0) then
       if (present (success)) then
          success = .false.
          return
       else
          call msg_bug ('Unable to open contract file "' // char (fname) // '"')
       end if
    end if
    call blha_read_contract_unit (cfg, ok, u, success)
    close (u)
  end subroutine blha_read_contract_file

  subroutine blha_read_contract_unit (cfg, ok, u, success)
    type(blha_configuration_t), intent(inout) :: cfg
    logical, intent(out) :: ok
    integer, intent(in) :: u
    logical, intent(out), optional :: success
    type(stream_t) :: stream
    type(ifile_t) :: preprocessed
    type(lexer_t) :: lexer
    type(parse_tree_t) :: parse_tree
    type(string_t) :: md5
    call stream_init (stream, u)
    call contract_preprocess (stream, preprocessed)
    call stream_final (stream)
    call stream_init (stream, preprocessed)
    call blha_init_lexer (lexer)
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_blha_contract, lexer)
    call blha_transfer_contract (cfg, ok, parse_tree, success)
    call blha_configuration_write (cfg, internal=.true.)
    call lexer_final (lexer)
    call stream_final (stream)
    call ifile_final (preprocessed)
    if (ok) then
       md5 = cfg%md5
       call blha_configuration_freeze (cfg)
       if (char (trim (md5 )) /= "") then
          if (md5 /= cfg%md5) then
             call msg_warning ("BLHA contract does not match the recorded " &
                // "checksum --- this counts as an error!")
             ok = .false.
          end if
       else
          call msg_warning ("It seems the OLP scrubbed our checksum, unable " &
             // "to check contract consistency.")
       end if
    end if
  end subroutine blha_read_contract_unit

  subroutine blha_transfer_contract (cfg, ok, parse_tree, success)
    type(blha_configuration_t), intent(inout) :: cfg
    logical, intent(out) :: ok
    type(parse_tree_t), intent(in), target :: parse_tree
    logical, intent(out), optional :: success
    type(parse_node_t), pointer :: pn_root, pn_line, pn_request, &
         pn_result, pn_key, pn_opt, pn_state_in, pn_state_out, pn_pdg
    type(string_t) :: emsg
    integer :: nopt, i, nsub
    integer, dimension(:), allocatable :: ids
    logical, dimension(2) :: flags
    type(pdg_array_t), dimension(:), allocatable :: pdg_in, pdg_out
    ok = .true.
    pn_root => parse_tree_get_root_ptr (parse_tree)
    pn_line => parse_node_get_sub_ptr (pn_root)
    do while (associated (pn_line))
       pn_request => parse_node_get_sub_ptr (pn_line)
       if (.not. associated (pn_request)) cycle
       if (char (parse_node_get_rule_key (pn_request)) == "process") then
          pn_result => parse_node_get_sub_ptr (pn_line, 2)
          pn_state_in => parse_node_get_sub_ptr (pn_request, 1)
          pn_state_out => parse_node_get_sub_ptr (pn_request, 3)
          allocate (pdg_in (parse_node_get_n_sub (pn_state_in)))
          allocate (pdg_out (parse_node_get_n_sub (pn_state_out)))
          i = 1
          pn_pdg => parse_node_get_sub_ptr (pn_state_in)
          do while (associated (pn_pdg))
             pdg_in(i) = [get_int (pn_pdg)]
             pn_pdg => parse_node_get_next_ptr (pn_pdg)
             i = i + 1
          end do
          i = 1
          pn_pdg => parse_node_get_sub_ptr (pn_state_out)
          do while (associated (pn_pdg))
             pdg_out(i) = [get_int (pn_pdg)]
             pn_pdg => parse_node_get_next_ptr (pn_pdg)
             i = i + 1
          end do
          i = parse_node_get_n_sub (pn_result)
          emsg = "broken process line"
          if (i < 2) goto 10
          pn_opt => parse_node_get_sub_ptr (pn_result, 2)
          do while (associated (pn_opt))
             if (char (parse_node_get_rule_key (pn_opt)) == "string") then
                call msg_warning ("While reading the BLHA contract: " // &
                   'the OLP returned an error for a process: "' // &
                   char (parse_node_get_string (pn_opt)) // '"')
                ok = .false.
                return
             end if
             pn_opt => parse_node_get_next_ptr (pn_opt)
          end do
          pn_opt => parse_node_get_sub_ptr (pn_result, 2)
          nsub = get_int (pn_opt)
          if (nsub /= i - 2) goto 10
          allocate (ids(nsub))
          i = 1
          pn_opt => parse_node_get_next_ptr (pn_opt)
          do while (associated (pn_opt))
             ids(i) = get_int (pn_opt)
             pn_opt => parse_node_get_next_ptr (pn_opt)
          end do
          call blha_configuration_append_process (cfg, pdg_in, pdg_out, &
             nsub=nsub, ids=ids)
          deallocate (pdg_in, pdg_out, ids)
       else
          pn_result => parse_node_get_sub_ptr (parse_node_get_next_ptr (pn_request), 2)
          pn_key => parse_node_get_sub_ptr (pn_request)
          pn_opt => parse_node_get_next_ptr (pn_key)
          nopt = parse_node_get_n_sub (pn_request) - 1
          select case (char (parse_node_get_rule_key (pn_key)))
             case ("md5")
                cfg%md5 = parse_node_get_string (pn_opt)
             case ("modelfile")
                cfg%model_file = get_fname (pn_opt)
                call check_result (pn_result, "ModelFile")
             case ("irregularisation")
                select case (lower_case (char (parse_node_get_string (pn_opt))))
                   case ("cdr"); cfg%irreg = BLHA_IRREG_CDR
                   case ("dred"); cfg%irreg = BLHA_IRREG_DRED
                   case ("thv"); cfg%irreg = BLHA_IRREG_THV
                   case ("mreg"); cfg%irreg = BLHA_IRREG_MREG
                   case default
                      cfg%irreg = BLHA_IRREG_OTHER 
                      cfg%irreg_other = parse_node_get_string (pn_opt)
                end select
                call check_result (pn_result, "IRRegularisation")
             case ("irsubtractionmethod")
                select case (lower_case (char (parse_node_get_string (pn_opt))))
                   case ("none"); cfg%subtraction_mode = BLHA_SUBMODE_NONE
                   case default
                      cfg%subtraction_mode = BLHA_SUBMODE_OTHER
                      cfg%subtraction_mode_other = parse_node_get_string(pn_opt)
                end select
                call check_result (pn_result, "IRSubtractionMethod")
             case ("massiveparticlescheme")
                select case (lower_case (char (parse_node_get_string (pn_opt))))
                   case ("onshell")
                      cfg%massive_particle_scheme = BLHA_MPS_ONSHELL
                   case default
                      cfg%massive_particle_scheme = BLHA_MPS_OTHER
                      cfg%massive_particle_scheme_other = &
                         parse_node_get_string (pn_opt)
                end select
                call check_result (pn_result, "MassiveParticleScheme")
             case ("matrixelementsquaretype")
                select case (nopt)
                   case (1)
                      select case (lower_case (char (parse_node_get_string (pn_opt))))
                         case ("chsummed")
                            cfg%matrix_element_square_type = BLHA_MEST_SUM
                         case ("chaveraged")
                            cfg%matrix_element_square_type = BLHA_MEST_AVG
                         case default
                            emsg = "invalid MatrixElementSquareType: " // &
                               parse_node_get_string (pn_opt)
                            goto 10
                      end select
                   case (2)
                      do i = 1, 2
                         pn_opt => parse_node_get_next_ptr (pn_key, i)
                         select case (lower_case (char (parse_node_get_string ( &
                               pn_opt))))
                            case ("csummed")
                               cfg%matrix_element_square_type(2) = BLHA_MEST_SUM
                               flags(2) = .true.
                            case ("caveraged")
                               cfg%matrix_element_square_type(2) = BLHA_MEST_AVG
                               flags(2) = .true.
                            case ("hsummed")
                               cfg%matrix_element_square_type(1) = BLHA_MEST_SUM
                               flags(1) = .true.
                            case ("haveraged")
                               cfg%matrix_element_square_type(1) = BLHA_MEST_AVG
                               flags(1) = .true.
                            case default
                               emsg = "invalid MatrixElementSquareType: " // &
                                  parse_node_get_string (pn_opt)
                               goto 10
                         end select
                      end do
                      if (.not. all (flags)) then
                         emsg = "MatrixElementSquareType: setup not exhaustive"
                         goto 10
                      end if
                   case default
                      emsg = "MatrixElementSquareType: too many options"
                      goto 10
                end select
                call check_result (pn_result, "MatrixElementSquareType")
             case ("correctiontype")
                select case (lower_case (char (parse_node_get_string (pn_opt))))
                   case ("qcd"); cfg%correction_type = BLHA_CT_QCD
                   case ("qed"); cfg%correction_type = BLHA_CT_QED
                   case ("ew"); cfg%correction_type = BLHA_CT_EW
                   case default
                      cfg%correction_type = BLHA_CT_OTHER
                      cfg%correction_type_other = parse_node_get_string (pn_opt)
                end select
                call check_result (pn_result, "CorrectionType")
             case ("alphaspower")
                cfg%alphas_power = get_int (pn_opt)
                call check_result (pn_result, "AlphasPower")
             case ("alphapower")
                cfg%alpha_power = get_int (pn_opt)
                call check_result (pn_result, "AlphaPower")
             case ("subdividesubprocess")
                select case (lower_case (char (parse_node_get_string (pn_opt))))
                   case ("yes"); cfg%subdivide_subprocesses = .true.
                   case ("no"); cfg%subdivide_subprocesses = .false.
                   case default
                      emsg = 'SubdivideSubprocess: invalid argument "' // &
                         parse_node_get_string (pn_opt) // '"'
                      goto 10
                end select
                call check_result (pn_result, "SubdivideSubprocess")
             case default
                emsg = "unknown statement: " // parse_node_get_rule_key (pn_key)
                goto 10
          end select
       end if
       pn_line => parse_node_get_next_ptr (pn_line)
    end do
    if (present (success)) success = .true.
    return
10  continue
    if (present (success)) then
       call msg_error ("Error reading BLHA contract: " // char (emsg))
       success = .false.
       return
    else
       call msg_fatal ("Error reading BLHA contract: " // char (emsg))
    end if

  contains

    function get_int (pn) result (i)
      type(parse_node_t), pointer :: pn
      integer :: i
      if (char (parse_node_get_rule_key (pn)) == "integer") then
         i = parse_node_get_integer (pn)
      else
         i = parse_node_get_integer (parse_node_get_sub_ptr (pn, 2))
         if (char (parse_node_get_rule_key (parse_node_get_sub_ptr (pn))) &
              == "-") i = -i
      end if
    end function get_int

    subroutine check_result (pn, step)
      type(parse_node_t), pointer :: pn
      character(*), intent(in) :: step
      type(string_t) :: res
      res = parse_node_get_string (pn)
      if (char (trim (res)) == "") then
         call msg_warning ("BLHA contract file: " // step // &
              ": OLP didn't return a status --- assuming an error")
         ok = .false.
      elseif (char (upper_case (res)) /= "OK") then
         call msg_warning ("BLHA contract file: " // step // &
              ': OLP error "' // char (res) // '"')
         ok = .false.
      end if
    end subroutine check_result

    function get_fname (pn) result (fname)
      type(parse_node_t), pointer :: pn
      type(string_t) :: fname
      type(parse_node_t), pointer :: pn_component
      if (char (parse_node_get_rule_key (pn)) == "string") then
         fname = parse_node_get_string (pn)
      else
         fname = ""
         pn_component => parse_node_get_sub_ptr (pn)
         do while (associated (pn_component))
            if (char (parse_node_get_rule_key (pn_component)) == "id") then
               fname = fname // parse_node_get_string (pn_component)
            else
               fname = fname // parse_node_get_key (pn_component)
            end if
            pn_component => parse_node_get_next_ptr (pn_component)
         end do
      end if
    end function get_fname

  end subroutine blha_transfer_contract

  subroutine blha_init_lexer (lexer)
    type(lexer_t), intent(inout) :: lexer
    call lexer_init (lexer, &
       comment_chars = "#", &
       quote_chars = '"', &
       quote_match = '"', &
       single_chars = '{}|./\:', &
       special_class = ["->"], &
       keyword_list = syntax_get_keyword_list_ptr (syntax_blha_contract), &
       upper_case_keywords = .false. &
       ) 
  end subroutine blha_init_lexer

  subroutine syntax_blha_contract_init ()
    type(ifile_t) :: ifile
    call ifile_append (ifile, "SEQ contract = line*")
    call ifile_append (ifile, "KEY '->'")
    call ifile_append (ifile, "KEY '.'")
    call ifile_append (ifile, "KEY '/'")
    call ifile_append (ifile, "KEY '\'")
    call ifile_append (ifile, "KEY '+'")
    call ifile_append (ifile, "KEY '-'")
    call ifile_append (ifile, "KEY '|'")
    call ifile_append (ifile, "KEY ':'")
    call ifile_append (ifile, "IDE id")
    call ifile_append (ifile, "INT integer")
    call ifile_append (ifile, "ALT sign = '+' | '-'")
    call ifile_append (ifile, "SEQ signed_integer = sign integer")
    call ifile_append (ifile, "QUO string = '""'...'""'")
    call ifile_append (ifile, "GRO line = '{' line_contents '}'")
    call ifile_append (ifile, "SEQ line_contents = request result?")
    call ifile_append (ifile, "ALT request = definition | process")
    call ifile_append (ifile, "ALT definition = option_unary | option_nary | " &
       // "option_path | option_numeric")
    call ifile_append (ifile, "KEY matrixelementsquaretype")
    call ifile_append (ifile, "KEY irregularisation")
    call ifile_append (ifile, "KEY massiveparticlescheme")
    call ifile_append (ifile, "KEY irsubtractionmethod")
    call ifile_append (ifile, "KEY modelfile")
    call ifile_append (ifile, "KEY operationmode")
    call ifile_append (ifile, "KEY subdividesubprocess")
    call ifile_append (ifile, "KEY alphaspower")
    call ifile_append (ifile, "KEY alphapower")
    call ifile_append (ifile, "KEY correctiontype")
    call ifile_append (ifile, "KEY md5")
    call ifile_append (ifile, "SEQ option_unary = key_unary arg")
    call ifile_append (ifile, "SEQ option_nary = key_nary arg+")
    call ifile_append (ifile, "SEQ option_path = key_path arg_path")
    call ifile_append (ifile, "SEQ option_numeric = key_numeric arg_numeric")
    call ifile_append (ifile, "ALT key_unary = irregularisation | " &
       // "massiveparticlescheme | irsubtractionmethod | subdividesubprocess | " &
       // "correctiontype | md5")
    call ifile_append (ifile, "ALT key_nary = matrixelementsquaretype | " &
       // "operationmode")
    call ifile_append (ifile, "ALT key_numeric = alphaspower | alphapower")
    call ifile_append (ifile, "ALT key_path = modelfile")
    call ifile_append (ifile, "ALT arg = id | string")
    call ifile_append (ifile, "ALT arg_numeric = integer | signed_integer")
    call ifile_append (ifile, "ALT arg_path = filename | string")
    call ifile_append (ifile, "SEQ filename = filename_atom+")
    call ifile_append (ifile, "ALT filename_atom = id | '.' | '/' | '\' | ':'")
    call ifile_append (ifile, "SEQ process = state '->' state")
    call ifile_append (ifile, "SEQ state = pdg+")
    call ifile_append (ifile, "ALT pdg = integer | signed_integer")
    call ifile_append (ifile, "SEQ result = '|' result_atom+")
    call ifile_append (ifile, "ALT result_atom = integer | string")
    call syntax_init (syntax_blha_contract, ifile)
    call ifile_final (ifile)
  end subroutine syntax_blha_contract_init

  subroutine syntax_blha_contract_final
    call syntax_final (syntax_blha_contract)
  end subroutine syntax_blha_contract_final

  subroutine contract_preprocess (stream, ifile)
    type(stream_t), intent(inout) :: stream
    type(ifile_t), intent(out) :: ifile
    type(string_t) :: buf, reg, transformed
    integer :: stat, n
    buf = ""
    LINES: do
       call stream_get_record (stream, reg, stat)
       select case (stat)
          case (0)
          case (EOF); exit LINES
          case default
             call msg_bug ("I/O error while reading BLHA contract file")
       end select
       buf = buf // trim (reg)
       ! Take care of continuation lines
       if (char (extract (buf, len (buf), len(buf))) == '&') then
          buf = extract (buf, 1, len (buf) - 1) // " "
          cycle LINES
       end if
       buf = adjustl (buf)
       ! Transform #@WO comments into ordinary statements
       if (char (extract (buf, 1, 4)) == "#@WO") &
          buf = extract (buf, 5)
       ! Kill comments and blank lines
       if ((char (trim (buf)) == "") .or. &
          (char (extract (buf, 1, 1)) == "#")) then
             buf = ""
             cycle LINES
          end if
       ! Chop off any end-of-line comments
       call split (buf, reg, "#")
       ! Split line into order and result
       call split (reg, buf, "|")
       reg = trim (adjustl (reg))
       buf = trim (adjustl (buf))
       ! Check whether the order is a process definition
       n = scan (buf, ">")
       if (n == 0) then
          ! No -> quote result
          reg = ('"' // reg) // '"'
       else
          ! Yes -> leave any numbers as they are, quote any leftovers
          n = scan (reg, "0123456789", back=.true.)
          if (n < len (reg)) &
             reg = char (extract (reg, 1, n)) // ' "' // &
                char (trim (adjustl (extract (reg, n+1)))) // '"'
       end if
       ! Enclose the line into curly brackets
       transformed = "{" // char (buf) // " | " // char (reg) // "}"
       call ifile_append (ifile, transformed)
       buf = ""
    end do LINES
  end subroutine contract_preprocess

  subroutine blha_config_test (model, cfg, ok)
    type(pdg_array_t), dimension(2) :: pdg_in
    type(pdg_array_t), dimension(4) :: pdg_out
    type(model_t), pointer :: model
    type(blha_configuration_t), intent(out) :: cfg
    logical, intent(out) :: ok
    integer :: u
    logical :: flag
    ok = .false.
    pdg_in(1) = [1, 2, -1, -2]
    pdg_in(2) = pdg_in(1)
    pdg_out(1) = pdg_in(1)
    pdg_out(2) = [11]
    pdg_out(3) = [-11]
    pdg_out(4) = pdg_out(1)
    call blha_configuration_init (cfg, var_str ("test"), model)
    call blha_configuration_set (cfg, alphas_power = 2, alpha_power = 3)
    call blha_configuration_append_process (cfg, pdg_in, pdg_out)
    call blha_configuration_freeze (cfg)
    print *
    call blha_configuration_write (cfg)
    print *
    call blha_configuration_final (cfg)
    call blha_configuration_init (cfg, var_str ("test"), model, &
       mode=BLHA_MODE_GOSAM)
    call blha_configuration_set (cfg, alphas_power = 0, &
       model_file = var_str ("test.slha"))
    pdg_in(1) = [1]
    pdg_in(2) = [-1]
    pdg_out(1) = [22]
    pdg_out(2) = [22]   
    call blha_configuration_append_process (cfg, pdg_in, pdg_out(1:2))
    call blha_configuration_freeze (cfg)
    u = free_unit ()
    open (u, file="test.blha.order", action="write", status="replace")
    call blha_configuration_write (cfg, u)
    call blha_configuration_final (cfg)
    inquire (file="test.blha.contract", exist=flag)
    if (.not. flag) return
    call blha_configuration_init (cfg, var_str ("test"), model, mode=BLHA_MODE_GOSAM)
    call blha_read_contract (cfg, ok, var_str ("test.blha.contract"), success=flag)
    print *, "Reading back processed configuration: success? ", ok
  end subroutine blha_config_test


end module blha_config

