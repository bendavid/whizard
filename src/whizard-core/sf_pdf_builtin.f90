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

module sf_pdf_builtin

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PROTON !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PION !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PHOTON !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use pdf_builtin !NODEP!
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

  public :: pdf_builtin_data_t
  public :: pdf_builtin_init
  public :: pdf_builtin_final
  public :: pdf_builtin_get_name
  public :: pdf_builtin_get_id
  public :: pdf_builtin_data_set_mask
  public :: pdf_builtin_data_write
  public :: interaction_init_pdf_builtin
  public :: interaction_set_kinematics_pdf_builtin
  public :: interaction_apply_pdf_builtin

  type :: pdf_builtin_data_t
     private
     type(string_t) :: path
     integer :: id = -1
     type (string_t) :: name
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     logical :: invert
     logical :: has_photon
     logical :: photon
     logical, dimension(-6:6) :: mask
     logical :: mask_photon
  end type pdf_builtin_data_t


contains

  subroutine pdf_builtin_init (data, pdf_status, model, flv, name, path)
    type(pdf_builtin_data_t), intent(out) :: data
    type(pdf_builtin_status_t), intent(inout) :: pdf_status
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    type(string_t), intent(in), optional :: name
    type(string_t), intent(in), optional :: path
    data%model => model
    data%flv_in = flv
    data%mask = .true.
    data%mask_photon = .true.
    select case (flavor_get_pdg (flv))
    case (PROTON)
       data%name = var_str (PDF_BUILTIN_DEFAULT_PROTON)
       data%invert = .false.
       data%photon = .false.
    case (-PROTON)
       data%name = var_str (PDF_BUILTIN_DEFAULT_PROTON)
       data%invert = .true.
       data%photon = .false.
!    case (PIPLUS)
!       data%name = var_str (PDF_BUILTIN_DEFAULT_PION)
!       data%invert = .false.
!       data%photon = .false.
!    case (-PIPLUS)
!       data%name = var_str (PDF_BUILTIN_DEFAULT_PION)
!       data%invert = .true.
!       data%photon = .false.
!    case (PHOTON)
!       data%name = var_str (PDF_BUILTIN_DEFAULT_PHOTON)
!       data%invert = .false.
!       data%photon = .true.
    case default
       call msg_fatal (" PDF: " &
            // "incoming particle must either proton or antiproton.")
       return
    end select
    if (present (name)) then
       data%name = name
    end if
    if (present (path)) then
       data%path = path
    else
       data%path = "."
    end if
    data%id = pdf_get_id (data%name)
    if (data%id < 0) call msg_fatal ("unknown PDF set " // char (data%name))
    data%has_photon = pdf_provides_photon (data%id)
    call pdf_init (pdf_status, data%id, path)
  end subroutine pdf_builtin_init

  subroutine pdf_builtin_final (data)
    type(pdf_builtin_data_t), intent(inout) :: data
    data%id = -1
  end subroutine pdf_builtin_final

  function pdf_builtin_get_name (data) result (name)
    type(pdf_builtin_data_t), intent(in) :: data
    type(string_t) :: name
    if (data%id < 0) then
       name = var_str ("undefined")
    else
       name = data%name
    end if
  end function pdf_builtin_get_name

  function pdf_builtin_get_id (data) result (id)
    type(pdf_builtin_data_t), intent(in) :: data
    integer :: id
    id = data%id
  end function pdf_builtin_get_id

  subroutine pdf_builtin_data_set_mask (data, mask)
    type(pdf_builtin_data_t), intent(inout) :: data
    logical, dimension(-6:6), intent(in) :: mask
    data%mask = mask
  end subroutine pdf_builtin_data_set_mask

  subroutine pdf_builtin_data_write (data, unit, md5, beam_fmt)
    type(pdf_builtin_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    logical, intent(in), optional :: beam_fmt
    logical :: is_md5
    logical :: is_beam_fmt
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
    if (data%id < 0) then
       write (u, *) "[undefined]"
       return
    end if
    write (u, "(3x,A)", advance="no") "flavor       = "
    call flavor_write (data%flv_in, u);  write (u, *)
    write (u, *) "  name         = ", char (data%name)
    if (.not. is_md5) &
       write (u, *) "  grid path    = ", char (data%path)
    write (u, *) "  invert       = ", data%invert
    write (u, *) "  has photon   = ", data%has_photon
    if (.not. is_beam_fmt) then
       write (u, *) "  mask         = ", &
            data%mask(-6:-1), "*", data%mask(0), "*", data%mask(1:6)
       write (u, *) "  photon mask  = ", data%mask_photon
    end if
  end subroutine pdf_builtin_data_write

  subroutine interaction_init_pdf_builtin (int, data)
    type(interaction_t), intent(out) :: int
    type(pdf_builtin_data_t), intent(in) :: data
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
          col = color_from_flavor (flv, 1, reverse = .true.))
       call interaction_add_state (int, &
          (/ qn_beam, qn_remnant, qn_parton /))
    end if
    call interaction_freeze (int)
  end subroutine interaction_init_pdf_builtin

  subroutine generate_x (x, f, r, data)
    real(default), intent(out) :: x, f
    real(default), intent(in) :: r
    type(pdf_builtin_data_t), intent(in) :: data
    real(default) :: lg
    x = r
    f = 1
!     lg = log (lhapdf_data% xmax / lhapdf_data% xmin)
!     x = lhapdf_data% xmin * exp (r * lg)
!     f = x * lg
  end subroutine generate_x

  subroutine interaction_set_kinematics_pdf_builtin (int, x, f, s, r, data)
    type(interaction_t), intent(inout) :: int
    real(default), intent(out) :: x, f, s
    real(default), intent(in) :: r
    type(pdf_builtin_data_t), intent(in) :: data
    type(vector4_t) :: k
    type(splitting_data_t) :: sd
    call generate_x (x, f, r, data)
    k = interaction_get_momentum (int, 1)
    s = k**2
    sd = new_splitting_data (k,  s, 0._default, 0._default)
    call splitting_set_t_bounds (sd,  x, 1 -  x)
    call splitting_set_collinear (sd)
    call interaction_set_momenta &
         (int, split_momentum (k, sd), outgoing=.true.)
  end subroutine interaction_set_kinematics_pdf_builtin

  subroutine interaction_apply_pdf_builtin (int, scale, x, f, s, data)
    type(interaction_t), intent(inout) :: int
    real(default), intent(in) :: scale, x, f, s
    type(pdf_builtin_data_t), intent(in) :: data
    real(kind=default) :: ff(-6:6), fph
    complex(default), dimension(:), allocatable :: fc
    if (data%invert) then
       if (data%has_photon) then
          call pdf_evolve (data%id, x, scale, ff(6:-6:-1), fph)
       else
          call pdf_evolve (data%id, x, scale, ff(6:-6:-1))
       end if
    else
       if (data%has_photon) then
          call pdf_evolve (data%id, x, scale, ff, fph)
       else
          call pdf_evolve (data%id, x, scale, ff)
       end if
    end if
    if (data%has_photon) then
       allocate (fc (count ((/data%mask, data%mask_photon/))))
       fc = max (pack ((/ff, fph/), &
          (/data%mask, data%mask_photon/)) * f, 0._default)
    else
       allocate (fc (count (data%mask)))
       fc = max (pack (ff, data%mask) * f, 0._default)
    end if
    call interaction_set_matrix_element (int, fc)
  end subroutine interaction_apply_pdf_builtin


end module sf_pdf_builtin
