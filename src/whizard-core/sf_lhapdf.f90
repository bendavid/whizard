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

module sf_lhapdf

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use system_dependencies, only: LHAPDF_PDFSETS_PATH !NODEP!
  use system_dependencies, only: LHAPDF5_AVAILABLE !NODEP!
  use system_dependencies, only: LHAPDF6_AVAILABLE !NODEP!  
  use limits, only: LHAPDF5_DEFAULT_PROTON !NODEP!
  use limits, only: LHAPDF5_DEFAULT_PION !NODEP!
  use limits, only: LHAPDF5_DEFAULT_PHOTON !NODEP!
  use limits, only: LHAPDF6_DEFAULT_PROTON !NODEP!
  use limits, only: FMT_17, FMT_19 !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use unit_tests
  use os_interface
  use sm_qcd
  use pdg_arrays
  use models
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux
  use sf_base
  use lhapdf !NODEP!
  use hoppet_interface

  implicit none
  private

  public :: lhapdf_status_t
  public :: lhapdf_status_reset
  public :: lhapdf_initialize
  public :: lhapdf_data_t
  public :: alpha_qcd_lhapdf_t
  public :: sf_lhapdf_test

  type :: lhapdf_status_t
     private
     logical, dimension(3) :: initialized = .false.
  end type lhapdf_status_t

  type, extends (sf_data_t) :: lhapdf_data_t
     private
     type(string_t) :: prefix
     type(string_t) :: file
     type(lhapdf_pdf_t) :: pdf 
     integer :: member = 0
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     integer :: set = 0
     logical :: invert = .false.
     logical :: photon = .false.
     logical :: has_photon = .false.
     integer :: photon_scheme = 0
     real(default) :: xmin = 0, xmax = 0
     real(default) :: qmin = 0, qmax = 0
     logical, dimension(-6:6) :: mask = .true.
     logical :: mask_photon = .true.
     logical :: hoppet_b_matching = .false.     
   contains
       procedure :: init => lhapdf_data_init
       procedure :: write => lhapdf_data_write
       procedure :: get_n_par => lhapdf_data_get_n_par
       procedure :: get_pdg_out => lhapdf_data_get_pdg_out
       procedure :: allocate_sf_int => lhapdf_data_allocate_sf_int
       procedure :: get_pdf_set => lhapdf_data_get_pdf_set
  end type lhapdf_data_t

  type, extends (sf_int_t) :: lhapdf_t
     type(lhapdf_data_t), pointer :: data => null ()
     real(default) :: x = 0
     real(default) :: q = 0
     real(default) :: s = 0
   contains
     procedure :: complete_kinematics => lhapdf_complete_kinematics
     procedure :: inverse_kinematics => lhapdf_inverse_kinematics
     procedure :: type_string => lhapdf_type_string
     procedure :: write => lhapdf_write
     procedure :: init => lhapdf_init
     procedure :: apply => lhapdf_apply
  end type lhapdf_t 
  
  type, extends (alpha_qcd_t) :: alpha_qcd_lhapdf_t
     type(string_t) :: pdfset_dir
     type(string_t) :: pdfset_file
     integer :: pdfset_member = -1
     type(lhapdf_pdf_t) :: pdf
   contains
     procedure :: write => alpha_qcd_lhapdf_write
     procedure :: get => alpha_qcd_lhapdf_get
     procedure :: init => alpha_qcd_lhapdf_init
  end type alpha_qcd_lhapdf_t
  

  interface
     subroutine InitPDFsetM (set, file)
       integer, intent(in) :: set
       character(*), intent(in) :: file
     end subroutine InitPDFsetM
  end interface

  interface
     subroutine InitPDFM (set, mem)
       integer, intent(in) :: set, mem
     end subroutine InitPDFM
  end interface

  interface
     subroutine numberPDFM (set, n_members)
       integer, intent(in) :: set
       integer, intent(out) :: n_members
     end subroutine numberPDFM
  end interface

  interface
     subroutine evolvePDFM (set, x, q, ff)
       integer, intent(in) :: set
       double precision, intent(in) :: x, q
       double precision, dimension(-6:6), intent(out) :: ff
     end subroutine evolvePDFM
  end interface

  interface
     subroutine evolvePDFphotonM (set, x, q, ff, fphot)
       integer, intent(in) :: set
       double precision, intent(in) :: x, q
       double precision, dimension(-6:6), intent(out) :: ff
       double precision, intent(out) :: fphot
     end subroutine evolvePDFphotonM
  end interface

  interface
     subroutine evolvePDFpM (set, x, q, s, scheme, ff)
       integer, intent(in) :: set
       double precision, intent(in) :: x, q, s
       integer, intent(in) :: scheme
       double precision, dimension(-6:6), intent(out) :: ff
     end subroutine evolvePDFpM
  end interface

  interface
     subroutine GetXminM (set, mem, xmin)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: xmin
     end subroutine GetXminM
  end interface

  interface
     subroutine GetXmaxM (set, mem, xmax)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: xmax
     end subroutine GetXmaxM
  end interface

  interface
     subroutine GetQ2minM (set, mem, q2min)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: q2min
     end subroutine GetQ2minM
  end interface

  interface
     subroutine GetQ2maxM (set, mem, q2max)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: q2max
     end subroutine GetQ2maxM
  end interface

  interface
     function has_photon () result(flag)
        logical :: flag
     end function has_photon
  end interface

  interface
     double precision function alphasPDF (Q)
       double precision, intent(in) :: Q
     end function alphasPDF
  end interface


contains

  subroutine lhapdf_status_reset (lhapdf_status)
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    lhapdf_status%initialized = .false.
  end subroutine lhapdf_status_reset

  function lhapdf_status_is_initialized (lhapdf_status, set) result (flag)
    logical :: flag
    type(lhapdf_status_t), intent(in) :: lhapdf_status
    integer, intent(in), optional :: set
    if (present (set)) then
       select case (set)
       case (1:3);    flag = lhapdf_status%initialized(set)
       case default;  flag = .false.
       end select
    else
       flag = any (lhapdf_status%initialized)
    end if
  end function lhapdf_status_is_initialized

  subroutine lhapdf_status_set_initialized (lhapdf_status, set)
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    integer, intent(in) :: set
    lhapdf_status%initialized(set) = .true.
  end subroutine lhapdf_status_set_initialized

  subroutine lhapdf_initialize (status, set, prefix, file, member, pdf, b_match)
    type(lhapdf_status_t), intent(inout) :: status
    integer, intent(in) :: set
    type(string_t), intent(inout) :: prefix
    type(string_t), intent(inout) :: file
    type(lhapdf_pdf_t), intent(inout), optional :: pdf
    integer, intent(inout) :: member
    logical, intent(in), optional :: b_match
    if (prefix == "")  prefix = LHAPDF_PDFSETS_PATH
    if (LHAPDF5_AVAILABLE) then
       if (lhapdf_status_is_initialized (status, set))  return       
       if (file == "") then
          select case (set)
          case (1);  file = LHAPDF5_DEFAULT_PROTON
          case (2);  file = LHAPDF5_DEFAULT_PION
          case (3);  file = LHAPDF5_DEFAULT_PHOTON
          end select
       end if
       if (data_file_exists (prefix // "/" // file)) then
          call InitPDFsetM (set, char (prefix // "/" // file))
       else
          call msg_fatal ("LHAPDF: Data file '" &
               // char (file) // "' not found in '" // char (prefix) // "'.")
          return
       end if
       if (.not. dataset_member_exists (set, member)) then
          call msg_error (" LHAPDF: Chosen member does not exist for set '" &
               // char (file) // "', using default.")
          member = 0
       end if
       call InitPDFM (set, member)
    else if (LHAPDF6_AVAILABLE) then
       if (lhapdf_status_is_initialized (status, set) .and. &
            pdf%is_associated ())  return
       if (file == "") then
          select case (set)
          case (1);  file = LHAPDF6_DEFAULT_PROTON
          case (2);  
             call msg_fatal ("LHAPDF6: no pion PDFs supported (yet)")
          case (3);  
             call msg_fatal ("LHAPDF6: no photon PDFs supported (yet)")
          end select
       end if
       if (data_file_exists (prefix // "/" // file // "/" // file // ".info")) then          
          call pdf%init (char (file), member)
       else
          call msg_fatal ("LHAPDF: Data file '" &
               // char (file) // "' not found in '" // char (prefix) // "'.")
          return
       end if 
    end if
    if (present (b_match)) then
       if (b_match) then
          if (LHAPDF5_AVAILABLE) then
             call hoppet_init (.false.)
          else if (LHAPDF6_AVAILABLE) then
             call hoppet_init (.false., pdf)
          end if
       end if
    end if
    call lhapdf_status_set_initialized (status, set)
  contains
    function data_file_exists (fq_name) result (exist)
      type(string_t), intent(in) :: fq_name
      logical :: exist
      inquire (file = char(fq_name), exist = exist)
    end function data_file_exists
    function dataset_member_exists (set, member) result (exist)
      integer, intent(in) :: set, member
      logical :: exist
      integer :: n_members
      call numberPDFM (set, n_members)
      exist = member >= 0 .and. member <= n_members
    end function dataset_member_exists
  end subroutine lhapdf_initialize

  subroutine lhapdf_complete_kinematics (sf_int, x, f, r, rb, map)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1
    if (map) then
       call msg_fatal ("LHAPDF: map flag not supported")
    else
       x(1) = r(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    call sf_int%split_momentum (x, xb1)
    select case (sf_int%status)
    case (SF_DONE_KINEMATICS)
       sf_int%x = x(1)
    case (SF_FAILED_KINEMATICS)
       sf_int%x = 0
       f = 0
    end select
  end subroutine lhapdf_complete_kinematics

  subroutine lhapdf_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: xb1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       call msg_fatal ("LHAPDF: map flag not supported")
    else
       r(1) = x(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    rb = 1 - r
    if (set_mom) then
       call sf_int%split_momentum (x, xb1)
       select case (sf_int%status)
       case (SF_DONE_KINEMATICS)
          sf_int%x = x(1)
       case (SF_FAILED_KINEMATICS)
          sf_int%x = 0
          f = 0
       end select
    end if
  end subroutine lhapdf_inverse_kinematics

  subroutine lhapdf_data_init &
       (data, status, model, pdg_in, prefix, file, member, photon_scheme, &
            hoppet_b_matching)
    class(lhapdf_data_t), intent(out) :: data
    type(lhapdf_status_t), intent(inout) :: status
    type(model_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    type(string_t), intent(in), optional :: prefix, file
    integer, intent(in), optional :: member
    integer, intent(in), optional :: photon_scheme
    logical, intent(in), optional :: hoppet_b_matching
    double precision :: xmin, xmax, q2min, q2max
    external :: InitPDFsetM, InitPDFM, numberPDFM
    external :: GetXminM, GetXmaxM, GetQ2minM, GetQ2maxM
    if (.not. LHAPDF5_AVAILABLE .and. .not. LHAPDF6_AVAILABLE) then
       call msg_fatal ("LHAPDF requested but library is not linked")
       return
    end if
    data%model => model
    if (pdg_array_get_length (pdg_in) /= 1) &
         call msg_fatal ("PDF: incoming particle must be unique")
    call flavor_init (data%flv_in, pdg_array_get (pdg_in, 1), model)
    select case (pdg_array_get (pdg_in, 1))
    case (PROTON)
       data%set = 1
    case (-PROTON)
       data%set = 1
       data%invert = .true.
    case (PIPLUS)
       data%set = 2
    case (-PIPLUS)
       data%set = 2
       data%invert = .true.
    case (PHOTON)
       data%set = 3
       data%photon = .true.
       if (present (photon_scheme))  data%photon_scheme = photon_scheme
    case default
       call msg_fatal (" LHAPDF: " &
            // "incoming particle must be (anti)proton, pion, or photon.")
       return
    end select
    if (present (prefix)) then
       data%prefix = prefix
    else
       data%prefix = ""
    end if
    if (present (file)) then
       data%file = file
    else
       data%file = ""
    end if
    if (present (hoppet_b_matching))  data%hoppet_b_matching = hoppet_b_matching    
    if (LHAPDF5_AVAILABLE) then
       call lhapdf_initialize &
            (status, data%set, data%prefix, data%file, data%member, &
            b_match = data%hoppet_b_matching)       
       call GetXminM (data%set, data%member, xmin)
       call GetXmaxM (data%set, data%member, xmax)
       call GetQ2minM (data%set, data%member, q2min)
       call GetQ2maxM (data%set, data%member, q2max)
       data%xmin = xmin
       data%xmax = xmax
       data%qmin = sqrt (q2min)
       data%qmax = sqrt (q2max)       
       data%has_photon = has_photon ()       
    else if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
            (status, data%set, data%prefix, data%file, data%member, &
            data%pdf, data%hoppet_b_matching)              
       data%xmin = data%pdf%getxmin ()
       data%xmax = data%pdf%getxmax ()
       data%qmin = sqrt(data%pdf%getq2min ())
       data%qmax = sqrt(data%pdf%getq2max ())
       data%has_photon = data%pdf%has_photon ()
    end if
  end subroutine lhapdf_data_init

  subroutine lhapdf_data_write (data, unit, verbose) 
    class(lhapdf_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: u
    if (present (verbose)) then
       verb = verbose
    else
       verb = .false.
    end if       
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "LHAPDF data:"
    if (data%set /= 0) then
       write (u, "(3x,A)", advance="no") "flavor       =  "
       call flavor_write (data%flv_in, u);  write (u, *)
       if (verb) then
          write (u, "(3x,A,A)")       "  prefix       =  ", char (data%prefix)
       else
          write (u, "(3x,A,A)")       "  prefix       = ", &
               " <empty (non-verbose version)>"
       end if
       write (u, "(3x,A,A)")       "  file         =  ", char (data%file)
       write (u, "(3x,A,I3)")      "  member       = ", data%member
       write (u, "(3x,A," // FMT_19 // ")") "  x(min)       = ", data%xmin
       write (u, "(3x,A," // FMT_19 // ")") "  x(max)       = ", data%xmax
       write (u, "(3x,A," // FMT_19 // ")") "  Q(min)       = ", data%qmin
       write (u, "(3x,A," // FMT_19 // ")") "  Q(max)       = ", data%qmax
       write (u, "(3x,A,L1)")      "  invert       =  ", data%invert
       if (data%photon)  write (u, "(3x,A,I3)") &
            "  IP2 (scheme) = ", data%photon_scheme
          write (u, "(3x,A,6(1x,L1),1x,A,1x,L1,1x,A,6(1x,L1))") &
               "  mask         = ", &
               data%mask(-6:-1), "*", data%mask(0), "*", data%mask(1:6)
          write (u, "(3x,A,L1)")   "  photon mask  =  ", data%mask_photon
       if (data%set == 1)  write (u, "(3x,A,L1)") &
            "  hoppet_b     =  ", data%hoppet_b_matching
    else
       write (u, "(3x,A)") "[undefined]"       
    end if
  end subroutine lhapdf_data_write
  function lhapdf_data_get_n_par (data) result (n)
    class(lhapdf_data_t), intent(in) :: data
    integer :: n
    n = 1
  end function lhapdf_data_get_n_par
  
  subroutine lhapdf_data_get_pdg_out (data, pdg_out)
    class(lhapdf_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    integer :: n, np, i
    n = count (data%mask)
    np = 0;  if (data%has_photon .and. data%mask_photon)  np = 1
    allocate (pdg1 (n + np))
    pdg1(1:n) = pack ([(i, i = -6, 6)], data%mask)
    if (np == 1)  pdg1(n+np) = PHOTON
    pdg_out(1) = pdg1
  end subroutine lhapdf_data_get_pdg_out
  
  subroutine lhapdf_data_allocate_sf_int (data, sf_int)
    class(lhapdf_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (lhapdf_t :: sf_int)
  end subroutine lhapdf_data_allocate_sf_int
  
  function lhapdf_data_get_pdf_set (data) result (pdf_set)
    class(lhapdf_data_t), intent(in) :: data
    integer :: pdf_set
    pdf_set = data%set
  end function lhapdf_data_get_pdf_set
  
  function lhapdf_type_string (object) result (string)
    class(lhapdf_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "LHAPDF: " // object%data%file
    else
       string = "LHAPDF: [undefined]"
    end if
  end function lhapdf_type_string
  
  subroutine lhapdf_write (object, unit, testflag)
    class(lhapdf_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       if (object%status >= SF_DONE_KINEMATICS) then
          write (u, "(1x,A)")  "SF parameters:"
          write (u, "(3x,A," // FMT_17 // ")")  "x =", object%x
          if (object%status >= SF_FAILED_EVALUATION) then
             write (u, "(3x,A," // FMT_17 // ")")  "Q =", object%q
          end if
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "LHAPDF data: [undefined]"
    end if
  end subroutine lhapdf_write
    
  subroutine lhapdf_init (sf_int, data)
    class(lhapdf_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    type(flavor_t) :: flv, flv_remnant
    type(quantum_numbers_t), dimension(3) :: qn
    integer :: i
    select type (data)
    type is (lhapdf_data_t)
       mask = new_quantum_numbers_mask (.false., .false., .true.)
       call sf_int%base_init (mask, [0._default], [0._default], [0._default])
       sf_int%data => data
       do i = -6, 6
          if (data%mask(i)) then
             call quantum_numbers_init (qn(1), data%flv_in)
             if (i == 0) then                
                call flavor_init (flv, GLUON, data%model)
                call flavor_init (flv_remnant, HADRON_REMNANT_OCTET, data%model)
             else
                call flavor_init (flv, i, data%model)
                call flavor_init (flv_remnant, &
                     sign (HADRON_REMNANT_TRIPLET, -i), data%model)
             end if
             call quantum_numbers_init (qn(2), &
                  flv = flv_remnant, col = color_from_flavor (flv_remnant, 1))
             call quantum_numbers_init (qn(3), &
                  flv = flv, col = color_from_flavor (flv, 1, reverse=.true.))
             call interaction_add_state (sf_int%interaction_t, qn)
          end if
       end do
       if (data%has_photon .and. data%mask_photon) then
          call flavor_init (flv, PHOTON, data%model)
          call flavor_init (flv_remnant, HADRON_REMNANT_SINGLET, data%model)
          call quantum_numbers_init (qn(2), flv = flv_remnant, &
               col = color_from_flavor (flv_remnant, 1))
          call quantum_numbers_init (qn(3), flv = flv, &
               col = color_from_flavor (flv, 1, reverse=.true.))
          call interaction_add_state (sf_int%interaction_t, qn)
       end if
       call interaction_freeze (sf_int%interaction_t)
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
       sf_int%status = SF_INITIAL
    end select
  end subroutine lhapdf_init

  subroutine lhapdf_apply (sf_int, scale)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: x, s
    double precision :: xx, qq, ss    
    double precision, dimension(-6:6) :: ff
    double precision :: fphot
    complex(default), dimension(:), allocatable :: fc
    external :: evolvePDFM, evolvePDFpM
    associate (data => sf_int%data)
      sf_int%q = scale
      x = sf_int%x
      s = sf_int%s
      qq = min (data% qmax, scale)
      qq = max (data% qmin, qq)
      if (.not. data% photon) then
         xx = x
         if (data% invert) then
            if (data%has_photon) then
               if (LHAPDF5_AVAILABLE) then
                  call evolvePDFphotonM &
                       (data% set, xx, qq, ff(6:-6:-1), fphot)
               else if (LHAPDF6_AVAILABLE) then
                  call data%pdf%evolve_pdfphotonm &
                       (xx, qq, ff(6:-6:-1), fphot)
               end if
            else
               if (data%hoppet_b_matching) then
                  call hoppet_eval (xx, qq, ff(6:-6:-1)) 
               else
                  if (LHAPDF5_AVAILABLE) then
                     call evolvePDFM (data% set, xx, qq, ff(6:-6:-1))
                  else if (LHAPDF6_AVAILABLE) then
                     call data%pdf%evolve_pdfm (xx, qq, ff(6:-6:-1))
                  end if
               end if
            end if
         else
            if (data%has_photon) then
               if (LHAPDF5_AVAILABLE) then
                  call evolvePDFphotonM (data% set, xx, qq, ff, fphot)
               else if (LHAPDF6_AVAILABLE) then
                  call data%pdf%evolve_pdfphotonm (xx, qq, ff, fphot)
               end if
            else
               if (data%hoppet_b_matching) then
                  call hoppet_eval (xx, qq, ff) 
               else
                  if (LHAPDF5_AVAILABLE) then
                     call evolvePDFM (data% set, xx, qq, ff)
                  else if (LHAPDF6_AVAILABLE) then
                     call data%pdf%evolve_pdfm (xx, qq, ff)
                  end if
               end if
            end if
         end if
      else
         ss = s
         if (LHAPDF5_AVAILABLE) then
            call evolvePDFpM (data% set, xx, qq, &
                 ss, data% photon_scheme, ff)
         else if (LHAPDF6_AVAILABLE) then
            call data%pdf%evolve_pdfpm (xx, qq, ss, &
                 data%photon_scheme, ff)
         end if
      end if
      if (data%has_photon) then
         allocate (fc (count ([data%mask, data%mask_photon])))
         fc = max (pack ([ff, fphot] / x, &
              [data% mask, data%mask_photon]), 0._default)
      else
         allocate (fc (count (data%mask)))
         fc = max (pack (ff / x, data%mask), 0._default)
      end if
    end associate
    call interaction_set_matrix_element (sf_int%interaction_t, fc)
    sf_int%status = SF_EVALUATED
  end subroutine lhapdf_apply
  
  subroutine alpha_qcd_lhapdf_write (object, unit)
    class(alpha_qcd_lhapdf_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A)")  "QCD parameters (lhapdf):"
    write (u, "(5x,A,A)")  "PDF set    = ", char (object%pdfset_file)
    write (u, "(5x,A,I0)") "PDF member = ", object%pdfset_member
  end subroutine alpha_qcd_lhapdf_write
  
  function alpha_qcd_lhapdf_get (alpha_qcd, scale) result (alpha)
    class(alpha_qcd_lhapdf_t), intent(in) :: alpha_qcd
    real(default), intent(in) :: scale
    real(default) :: alpha
    if (LHAPDF5_AVAILABLE) then
       alpha = alphasPDF (dble (scale))
    else if (LHAPDF6_AVAILABLE) then
       alpha = alpha_qcd%pdf%alphas_pdf (dble (scale))
    end if
  end function alpha_qcd_lhapdf_get
  
  subroutine alpha_qcd_lhapdf_init (alpha_qcd, status, file, member, path)
    class(alpha_qcd_lhapdf_t), intent(out) :: alpha_qcd
    type(lhapdf_status_t), intent(inout) :: status
    type(string_t), intent(inout) :: file
    integer, intent(inout) :: member
    type(string_t), intent(inout) :: path
    alpha_qcd%pdfset_file = file
    alpha_qcd%pdfset_member = member
    if (alpha_qcd%pdfset_member < 0) &
         call msg_fatal ("QCD parameter initialization: PDF set " &
         // char (file) // " is unknown")
    if (LHAPDF5_AVAILABLE) then
       call lhapdf_initialize (status, 1, path, file, member)
    else if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
            (status, 1, path, file, member, alpha_qcd%pdf)
    end if
  end subroutine alpha_qcd_lhapdf_init
    

  subroutine sf_lhapdf_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    if (LHAPDF5_AVAILABLE) then  
       call test (sf_lhapdf_1, "sf_lhapdf5_1", &
            "structure function configuration", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_1, "sf_lhapdf6_1", &
            "structure function configuration", &
            u, results)
    end if
    if (LHAPDF5_AVAILABLE) then
       call test (sf_lhapdf_2, "sf_lhapdf5_2", &
            "structure function instance", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_2, "sf_lhapdf6_2", &
            "structure function instance", &
            u, results)     
    end if
    if (LHAPDF5_AVAILABLE) then
       call test (sf_lhapdf_3, "sf_lhapdf5_3", &
            "running alpha_s", &
            u, results)
    else if (LHAPDF6_AVAILABLE) then
       call test (sf_lhapdf_3, "sf_lhapdf6_3", &
            "running alpha_s", &
            u, results)     
    end if
  end subroutine sf_lhapdf_test
  
  subroutine sf_lhapdf_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    type(lhapdf_status_t) :: status
    
    write (u, "(A)")  "* Test output: sf_lhapdf_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QCD"), &
         var_str ("QCD.mdl"), os_data, model)
    pdg_in = PROTON

    allocate (lhapdf_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    select type (data)
    type is (lhapdf_data_t)
       call data%init (status, model, pdg_in)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(2x,99(1x,I0))")  pdg1

    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_lhapdf_1"

  end subroutine sf_lhapdf_1

  subroutine sf_lhapdf_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(lhapdf_status_t) :: status
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_lhapdf_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QCD"), &
         var_str ("QCD.mdl"), os_data, model)
    call flavor_init (flv, PROTON, model)
    pdg_in = PROTON

    call reset_interaction_counter ()
    
    allocate (lhapdf_data_t :: data)
    select type (data)
    type is (lhapdf_data_t)
       call data%init (status, model, pdg_in)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r = 0.5_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Recover x from momenta"
    write (u, "(A)")

    q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
    call sf_int%final ()
    deallocate (sf_int)

    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])

    call sf_int%seed_kinematics ([k])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)

    write (u, "(A,9(1x,F10.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for Q = 100 GeV"
    write (u, "(A)")

    call sf_int%complete_kinematics (x, f, r, rb, map=.false.) 
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u, testflag = .true.)


    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_lhapdf_2"

  end subroutine sf_lhapdf_2

  subroutine sf_lhapdf_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(lhapdf_status_t) :: status
    type(qcd_t) :: qcd
    type(string_t) :: name, path
    integer :: member
    
    write (u, "(A)")  "* Test output: sf_lhapdf_3"
    write (u, "(A)")  "*   Purpose: initialize and evaluate alpha_s"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)

    if (LHAPDF5_AVAILABLE) then
       name = "cteq6ll.LHpdf"
       member = 1
       path = ""
    else if (LHAPDF6_AVAILABLE) then
       name = "CT10"
       member = 1
       path = ""
    end if
       
    write (u, "(A)")  "* Initialize qcd object"
    write (u, "(A)")
    
    allocate (alpha_qcd_lhapdf_t :: qcd%alpha)
    select type (alpha => qcd%alpha)
    type is (alpha_qcd_lhapdf_t)
       call alpha%init (status, name, member, path)
    end select
    call qcd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for Q = 100"
    write (u, "(A)")
    
    write (u, "(1x,A,F8.5)")  "alpha = ", qcd%alpha%get (100._default)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model_list%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_lhapdf_3"

  end subroutine sf_lhapdf_3


end module sf_lhapdf
