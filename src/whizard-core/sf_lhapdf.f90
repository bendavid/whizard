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

module sf_lhapdf

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use system_dependencies, only: LHAPDF_PDFSETS_PATH !NODEP!
  use system_dependencies, only: LHAPDF_AVAILABLE !NODEP!
  use limits, only: LHAPDF_DEFAULT_PROTON !NODEP!
  use limits, only: LHAPDF_DEFAULT_PION !NODEP!
  use limits, only: LHAPDF_DEFAULT_PHOTON !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux

  implicit none
  private

  public :: lhapdf_status_t
  public :: lhapdf_status_reset
  public :: lhapdf_init
  public :: lhapdf_data_t
  public :: lhapdf_data_init
  public :: lhapdf_data_set_mask
  public :: lhapdf_data_get_public_info
  public :: lhapdf_data_get_set
  public :: lhapdf_data_write
  public :: interaction_init_lhapdf
  public :: interaction_set_kinematics_lhapdf
  public :: interaction_apply_lhapdf

  type :: lhapdf_status_t
     private
     logical, dimension(3) :: initialized = .false.
  end type lhapdf_status_t

  type :: lhapdf_data_t
     private
     type(string_t) :: prefix
     type(string_t) :: file
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
  end type lhapdf_data_t


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

  subroutine lhapdf_init (status, set, prefix, file, member)
    type(lhapdf_status_t), intent(inout) :: status
    integer, intent(in) :: set
    type(string_t), intent(inout) :: prefix
    type(string_t), intent(inout) :: file
    integer, intent(inout) :: member
    if (lhapdf_status_is_initialized (status, set))  return
    if (prefix == "")  prefix = LHAPDF_PDFSETS_PATH
    if (file == "") then
       select case (set)
       case (1);  file = LHAPDF_DEFAULT_PROTON
       case (2);  file = LHAPDF_DEFAULT_PION
       case (3);  file = LHAPDF_DEFAULT_PHOTON
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
  end subroutine lhapdf_init

  subroutine lhapdf_data_init &
       (data, status, model, flv, prefix, file, member, photon_scheme)
    type(lhapdf_data_t), intent(out) :: data
    type(lhapdf_status_t), intent(inout) :: status
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    type(string_t), intent(in), optional :: prefix, file
    integer, intent(in), optional :: member
    integer, intent(in), optional :: photon_scheme
    integer :: mem
    double precision :: xmin, xmax, q2min, q2max
    external :: InitPDFsetM, InitPDFM, numberPDFM
    external :: GetXminM, GetXmaxM, GetQ2minM, GetQ2maxM
    if (.not. LHAPDF_AVAILABLE) then
       call msg_fatal ("LHAPDF requested but library is not linked")
       return
    end if
    data%model => model
    data%flv_in = flv
    select case (flavor_get_pdg (flv))
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
    call lhapdf_init (status, data%set, data%prefix, data%file, data%member)
    call GetXminM (data%set, data%member, xmin)
    call GetXmaxM (data%set, data%member, xmax)
    call GetQ2minM (data%set, data%member, q2min)
    call GetQ2maxM (data%set, data%member, q2max)
    data%xmin = xmin
    data%xmax = xmax
    data%qmin = sqrt (q2min)
    data%qmax = sqrt (q2max)
    data%has_photon = has_photon ()
  end subroutine lhapdf_data_init

  subroutine lhapdf_data_set_mask (data, mask)
    type(lhapdf_data_t), intent(inout) :: data
    logical, dimension(-6:6), intent(in) :: mask
    data%mask = mask
  end subroutine lhapdf_data_set_mask

  subroutine lhapdf_data_get_public_info &
       (data, lhapdf_dir, lhapdf_file, lhapdf_member)
    type(lhapdf_data_t), intent(in) :: data
    type(string_t), intent(out) :: lhapdf_dir, lhapdf_file
    integer, intent(out) :: lhapdf_member
    lhapdf_dir = data%prefix
    lhapdf_file = data%file
    lhapdf_member = data%member
  end subroutine lhapdf_data_get_public_info

  function lhapdf_data_get_set(data) result(set)
    type(lhapdf_data_t), intent(in) :: data
    integer :: set
    set = data%set
  end function lhapdf_data_get_set

  subroutine lhapdf_data_write (data, unit, md5, beam_fmt)
    type(lhapdf_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    logical, intent(in), optional :: beam_fmt
    logical :: is_md5, is_beam_fmt
    if (present (md5)) then
       is_md5 = md5
    else
       is_md5 = .false.
    end if
    if (present (beam_fmt)) then
       is_beam_fmt = beam_fmt
    else
       is_beam_fmt = .false.
    end if
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "LHAPDF data:"
    if (data%set /= 0) then
       write (u, "(3x,A)", advance="no") "flavor       = "
       call flavor_write (data%flv_in, u);  write (u, *)
       if (.not. is_md5) &
          write (u, *) "  prefix       = ", char (data%prefix)
       write (u, *) "  file         = ", char (data%file)
       write (u, *) "  member       = ", data%member
       write (u, *) "  x(min)       = ", data%xmin
       write (u, *) "  x(max)       = ", data%xmax
       write (u, *) "  Q^2(min)     = ", data%qmin
       write (u, *) "  Q^2(max)     = ", data%qmax
       write (u, *) "  invert       = ", data%invert
       if (data%photon)  write (u, *) "  IP2 (scheme) = ", data%photon_scheme
       if (.not. is_beam_fmt) then          
          write (u, *) "  mask         = ", &
               data%mask(-6:-1), "*", data%mask(0), "*", data%mask(1:6)
          write (u, *) "  photon mask  = ", data%mask_photon
       end if  
    else
       write (u, *) "  [undefined]"
    end if
  end subroutine lhapdf_data_write

  subroutine interaction_init_lhapdf (int, data)
    type(interaction_t), intent(out) :: int
    type(lhapdf_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    type(quantum_numbers_t) :: qn_beam, qn_remnant, qn_parton
    type(flavor_t) :: flv, flv_remnant
    integer :: i
    mask = new_quantum_numbers_mask (.false., .false., .true.)
    call interaction_init (int, 1, 0, 2, mask=mask, set_relations=.true.)
    call quantum_numbers_init (qn_beam, flv = data%flv_in)
    do i = -6, 6
       if (data%mask(i)) then
          if (i == 0) then
             call flavor_init (flv, GLUON, data%model)
             call flavor_init (flv_remnant, HADRON_REMNANT_OCTET, data%model)
          else
             call flavor_init (flv, i, data%model)
             call flavor_init (flv_remnant, &
                  sign (HADRON_REMNANT_TRIPLET, -i), data%model)
          end if
          call quantum_numbers_init (qn_remnant, &
               flv = flv_remnant, col = color_from_flavor (flv_remnant, 1))
          call quantum_numbers_init (qn_parton, &
               flv = flv, col = color_from_flavor (flv, 1, reverse=.true.))
          call interaction_add_state (int, &
               (/ qn_beam, qn_remnant, qn_parton /))
       end if
    end do
    if (data%has_photon .and. data%mask_photon) then
       call flavor_init (flv, PHOTON, data%model)
       call flavor_init (flv_remnant, HADRON_REMNANT_SINGLET, data%model)
       call quantum_numbers_init (qn_remnant, flv = flv_remnant, &
          col = color_from_flavor (flv_remnant, 1))
       call quantum_numbers_init (qn_parton, flv = flv, &
          col = color_from_flavor (flv, 1, reverse=.true.))
       call interaction_add_state (int, &
          (/qn_beam, qn_remnant, qn_parton /))
    end if
    call interaction_freeze (int)
  end subroutine interaction_init_lhapdf

  subroutine generate_x (x, f, r, lhapdf_data)
    real(default), intent(out) :: x, f
    real(default), intent(in) :: r
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    real(default) :: lg
    x = r
    f = 1
!     lg = log (lhapdf_data% xmax / lhapdf_data% xmin)
!     x = lhapdf_data% xmin * exp (r * lg)
!     f = x * lg
  end subroutine generate_x

  subroutine interaction_set_kinematics_lhapdf (int, x, f, s, r, lhapdf_data)
    type(interaction_t), intent(inout) :: int
    real(default), intent(out) :: x, f, s
    real(default), intent(in) :: r
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    type(vector4_t) :: k
    type(splitting_data_t) :: sd
    call generate_x (x, f, r, lhapdf_data)
    k = interaction_get_momentum (int, 1)
    s = k**2
    sd = new_splitting_data (k,  s, 0._default, 0._default)
    call splitting_set_t_bounds (sd,  x, 1 -  x)
    call splitting_set_collinear (sd)
    call interaction_set_momenta &
         (int, split_momentum (k, sd), outgoing=.true.)
  end subroutine interaction_set_kinematics_lhapdf

  subroutine interaction_apply_lhapdf (int, scale, x, f, s, lhapdf_data)
    type(interaction_t), intent(inout) :: int
    real(default), intent(in) :: scale, x, f, s
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    double precision :: xx, qq, ss
    double precision, dimension(-6:6) :: ff
    double precision :: fphot
    complex(default), dimension(:), allocatable :: fc
    external :: evolvePDFM, evolvePDFpM
    xx = x
    qq = min (lhapdf_data% qmax, scale)
    qq = max (lhapdf_data% qmin, qq)
    if (.not. lhapdf_data% photon) then
       if (lhapdf_data% invert) then
          if (lhapdf_data%has_photon) then
             call evolvePDFphotonM (lhapdf_data% set, xx, qq, ff(6:-6:-1), fphot)
          else
             call evolvePDFM (lhapdf_data% set, xx, qq, ff(6:-6:-1))
          end if
       else
          if (lhapdf_data%has_photon) then
             call evolvePDFphotonM (lhapdf_data% set, xx, qq, ff, fphot)
          else
             call evolvePDFM (lhapdf_data% set, xx, qq, ff)
          end if
       end if
    else
       ss = s
       call evolvePDFpM (lhapdf_data% set, xx, qq, &
            ss, lhapdf_data% photon_scheme, ff)
    end if
    if (lhapdf_data%has_photon) then
       allocate (fc (count ((/lhapdf_data%mask, lhapdf_data%mask_photon/))))
       fc = max (pack ((/ff, fphot/) / x, &
         (/lhapdf_data% mask, lhapdf_data%mask_photon/)) * f, 0._default)
    else
       allocate (fc (count (lhapdf_data%mask)))
       fc = max (pack (ff / x, lhapdf_data%mask) * f, 0._default)
    end if
    call interaction_set_matrix_element (int, fc)
  end subroutine interaction_apply_lhapdf


end module sf_lhapdf
