! WHIZARD 2.6.2 Dec 13 2017
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

module sf_lhapdf

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_defs, only: FMT_17, FMT_19
  use io_units
  use system_dependencies, only: LHAPDF_PDFSETS_PATH
  use system_dependencies, only: LHAPDF5_AVAILABLE
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use diagnostics
  use physics_defs, only: PROTON, PHOTON, PIPLUS, GLUON
  use physics_defs, only: HADRON_REMNANT_SINGLET
  use physics_defs, only: HADRON_REMNANT_TRIPLET
  use physics_defs, only: HADRON_REMNANT_OCTET
  use lorentz
  use sm_qcd
  use pdg_arrays
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use sf_base
  use lhapdf !NODEP!
  use hoppet_interface

  implicit none
  private

  public :: lhapdf_global_reset
  public :: lhapdf_initialize
  public :: lhapdf_data_t
  public :: lhapdf_t
  public :: alpha_qcd_lhapdf_t

  type :: lhapdf_global_status_t
     private
     logical, dimension(3) :: initialized = .false.
  end type lhapdf_global_status_t

  type, extends (sf_data_t) :: lhapdf_data_t
     private
     type(string_t) :: prefix
     type(string_t) :: file
     type(lhapdf_pdf_t) :: pdf
     integer :: member = 0
     class(model_data_t), pointer :: model => null ()
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


  character(*), parameter :: LHAPDF5_DEFAULT_PROTON = "cteq6ll.LHpdf"
  character(*), parameter :: LHAPDF5_DEFAULT_PION   = "ABFKWPI.LHgrid"
  character(*), parameter :: LHAPDF5_DEFAULT_PHOTON = "GSG960.LHgrid"
  character(*), parameter :: LHAPDF6_DEFAULT_PROTON = "CT10"

  type(lhapdf_global_status_t), save :: lhapdf_global_status

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

  function lhapdf_global_status_is_initialized (set) result (flag)
    logical :: flag
    integer, intent(in), optional :: set
    if (present (set)) then
       select case (set)
       case (1:3);    flag = lhapdf_global_status%initialized(set)
       case default;  flag = .false.
       end select
    else
       flag = any (lhapdf_global_status%initialized)
    end if
  end function lhapdf_global_status_is_initialized

  subroutine lhapdf_global_status_set_initialized (set)
    integer, intent(in) :: set
    lhapdf_global_status%initialized(set) = .true.
  end subroutine lhapdf_global_status_set_initialized

  subroutine lhapdf_global_reset ()
    lhapdf_global_status%initialized = .false.
  end subroutine lhapdf_global_reset

  subroutine lhapdf_initialize (set, prefix, file, member, pdf, b_match)
    integer, intent(in) :: set
    type(string_t), intent(inout) :: prefix
    type(string_t), intent(inout) :: file
    type(lhapdf_pdf_t), intent(inout), optional :: pdf
    integer, intent(inout) :: member
    logical, intent(in), optional :: b_match
    if (prefix == "")  prefix = LHAPDF_PDFSETS_PATH
    if (LHAPDF5_AVAILABLE) then
       if (lhapdf_global_status_is_initialized (set))  return
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
    ! TODO: (bcn 2015-07-07) we should have a closer look why this global
    !                        check must not be executed
    !   if (lhapdf_global_status_is_initialized (set) .and. &
    !        pdf%is_associated ()) return
       if (file == "") then
          select case (set)
          case (1);  file = LHAPDF6_DEFAULT_PROTON
          case (2);
             call msg_fatal ("LHAPDF6: no pion PDFs supported")
          case (3);
             call msg_fatal ("LHAPDF6: no photon PDFs supported")
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
    call lhapdf_global_status_set_initialized (set)
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

  subroutine lhapdf_complete_kinematics (sf_int, x, xb, f, r, rb, map)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), dimension(:), intent(out) :: xb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    if (map) then
       call msg_fatal ("LHAPDF: map flag not supported")
    else
       x(1) = r(1)
       xb(1)= rb(1)
       f = 1
    end if
    call sf_int%split_momentum (x, xb)
    select case (sf_int%status)
    case (SF_DONE_KINEMATICS)
       sf_int%x = x(1)
    case (SF_FAILED_KINEMATICS)
       sf_int%x = 0
       f = 0
    end select
  end subroutine lhapdf_complete_kinematics

  subroutine lhapdf_inverse_kinematics (sf_int, x, xb, f, r, rb, map, set_momenta)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), dimension(:), intent(in) :: xb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       call msg_fatal ("LHAPDF: map flag not supported")
    else
       r(1) = x(1)
       rb(1)= xb(1)
       f = 1
    end if
    if (set_mom) then
       call sf_int%split_momentum (x, xb)
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
       (data, model, pdg_in, prefix, file, member, photon_scheme, &
            hoppet_b_matching)
    class(lhapdf_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
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
    call data%flv_in%init (pdg_array_get (pdg_in, 1), model)
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
            (data%set, data%prefix, data%file, data%member, &
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
            (data%set, data%prefix, data%file, data%member, &
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
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "LHAPDF data:"
    if (data%set /= 0) then
       write (u, "(3x,A)", advance="no") "flavor       =  "
       call data%flv_in%write (u);  write (u, *)
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

  elemental function lhapdf_data_get_pdf_set (data) result (pdf_set)
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
    u = given_output_unit (unit)
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
    type(color_t) :: col0
    type(quantum_numbers_t), dimension(3) :: qn
    integer :: i
    select type (data)
    type is (lhapdf_data_t)
       mask = quantum_numbers_mask (.false., .false., .true.)
       call col0%init ()
       call sf_int%base_init (mask, [0._default], [0._default], [0._default])
       sf_int%data => data
       do i = -6, 6
          if (data%mask(i)) then
             call qn(1)%init (data%flv_in, col = col0)
             if (i == 0) then
                call flv%init (GLUON, data%model)
                call flv_remnant%init (HADRON_REMNANT_OCTET, data%model)
             else
                call flv%init (i, data%model)
                call flv_remnant%init &
                     (sign (HADRON_REMNANT_TRIPLET, -i), data%model)
             end if
             call qn(2)%init ( &
                  flv = flv_remnant, col = color_from_flavor (flv_remnant, 1))
             call qn(2)%tag_radiated ()
             call qn(3)%init ( &
                  flv = flv, col = color_from_flavor (flv, 1, reverse=.true.))
             call sf_int%add_state (qn)
          end if
       end do
       if (data%has_photon .and. data%mask_photon) then
          call flv%init (PHOTON, data%model)
          call flv_remnant%init (HADRON_REMNANT_SINGLET, data%model)
          call qn(2)%init (flv = flv_remnant, &
               col = color_from_flavor (flv_remnant, 1))
          call qn(2)%tag_radiated ()
          call qn(3)%init (flv = flv, &
               col = color_from_flavor (flv, 1, reverse=.true.))
          call sf_int%add_state (qn)
       end if
       call sf_int%freeze ()
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
       sf_int%status = SF_INITIAL
    end select
  end subroutine lhapdf_init

  subroutine lhapdf_apply (sf_int, scale, rescaling_function, i_rescale)
    class(lhapdf_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    class(rescaling_function_t), intent(in), optional :: rescaling_function
    integer, intent(in), optional :: i_rescale
    real(default) :: x, s
    double precision :: xx, qq, ss
    double precision, dimension(-6:6) :: ff
    double precision :: fphot
    complex(default), dimension(:), allocatable :: fc
    integer :: i, i_idx
    external :: evolvePDFM, evolvePDFpM
    associate (data => sf_int%data)
      sf_int%q = scale
      x = sf_int%x
      if (present (rescaling_function))  call rescaling_function%apply (x)
      s = sf_int%s
      xx = x
      call msg_debug (D_BEAMS, "lhapdf_apply")
      call msg_debug (D_BEAMS, "rescaling_function: ", present(rescaling_function))
      call msg_debug (D_BEAMS, "x: ", x)
      qq = min (data%qmax, scale)
      qq = max (data%qmin, qq)
      if (.not. data% photon) then
         if (data%invert) then
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
    if (debug_active (D_BEAMS))  print *, 'Set pdfs: ', real (fc)
    if (present (rescaling_function)) then
       if (present (i_rescale)) then
          i_idx = i_rescale + 1
       else
          i_idx = 1
       end if
       call sf_int%set_matrix_element (fc, rescaling_function%sf_indices(i_idx, :))
       if (allocated (rescaling_function%sf_indices_gluon)) then
          do i = 1, 13
             call sf_int%set_matrix_element (rescaling_function%sf_indices_gluon(i_rescale, i), fc(7))
          end do
       end if
    else
       call sf_int%set_matrix_element (fc, [(i, i = 1, size(fc))])
    end if
    sf_int%status = SF_EVALUATED
  end subroutine lhapdf_apply

  subroutine alpha_qcd_lhapdf_write (object, unit)
    class(alpha_qcd_lhapdf_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
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

  subroutine alpha_qcd_lhapdf_init (alpha_qcd, file, member, path)
    class(alpha_qcd_lhapdf_t), intent(out) :: alpha_qcd
    type(string_t), intent(inout) :: file
    integer, intent(inout) :: member
    type(string_t), intent(inout) :: path
    alpha_qcd%pdfset_file = file
    alpha_qcd%pdfset_member = member
    if (alpha_qcd%pdfset_member < 0) &
         call msg_fatal ("QCD parameter initialization: PDF set " &
         // char (file) // " is unknown")
    if (LHAPDF5_AVAILABLE) then
       call lhapdf_initialize (1, path, file, member)
    else if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
            (1, path, file, member, alpha_qcd%pdf)
    end if
  end subroutine alpha_qcd_lhapdf_init


end module sf_lhapdf
