! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
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

module sf_pdf_builtin

  use kinds, only: default
  use kinds, only: double
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_defs, only: FMT_17
  use diagnostics
  use os_interface
  use physics_defs, only: PROTON, PHOTON, GLUON
  use physics_defs, only: HADRON_REMNANT_SINGLET
  use physics_defs, only: HADRON_REMNANT_TRIPLET
  use physics_defs, only: HADRON_REMNANT_OCTET
  use sm_qcd
  use lorentz
  use pdg_arrays
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use sf_base
  use pdf_builtin !NODEP!
  use hoppet_interface

  implicit none
  private

  public :: pdf_builtin_data_t
  public :: pdf_builtin_t
  public :: alpha_qcd_pdf_builtin_t

  type, extends (sf_data_t) :: pdf_builtin_data_t
     private
     integer :: id = -1
     type (string_t) :: name
     class(model_data_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     logical :: invert
     logical :: has_photon
     logical :: photon
     logical, dimension(-6:6) :: mask
     logical :: mask_photon
     logical :: hoppet_b_matching = .false.
   contains
     procedure :: init => pdf_builtin_data_init
     procedure :: set_mask => pdf_builtin_data_set_mask
     procedure :: write => pdf_builtin_data_write
     procedure :: get_n_par => pdf_builtin_data_get_n_par
     procedure :: get_pdg_out => pdf_builtin_data_get_pdg_out
     procedure :: allocate_sf_int => pdf_builtin_data_allocate_sf_int
     procedure :: get_pdf_set => pdf_builtin_data_get_pdf_set
  end type pdf_builtin_data_t

  type, extends (sf_int_t) :: pdf_builtin_t
     type(pdf_builtin_data_t), pointer :: data => null ()
     real(default) :: x = 0
     real(default) :: q = 0
   contains
     procedure :: type_string => pdf_builtin_type_string
     procedure :: write => pdf_builtin_write
     procedure :: init => pdf_builtin_init
     procedure :: complete_kinematics => pdf_builtin_complete_kinematics
     procedure :: inverse_kinematics => pdf_builtin_inverse_kinematics
     procedure :: apply => pdf_builtin_apply
  end type pdf_builtin_t
  
  type, extends (alpha_qcd_t) :: alpha_qcd_pdf_builtin_t
     type(string_t) :: pdfset_name
     integer :: pdfset_id = -1
   contains
     procedure :: write => alpha_qcd_pdf_builtin_write
     procedure :: get => alpha_qcd_pdf_builtin_get
     procedure :: init => alpha_qcd_pdf_builtin_init
  end type alpha_qcd_pdf_builtin_t
  

  character(*), parameter :: PDF_BUILTIN_DEFAULT_PROTON = "CTEQ6L"
  character(*), parameter :: PDF_BUILTIN_DEFAULT_PION   = "NONE"
  character(*), parameter :: PDF_BUILTIN_DEFAULT_PHOTON = "MRST2004QEDp"

contains

  subroutine pdf_builtin_data_init (data, &
       model, pdg_in, name, path, hoppet_b_matching)
    class(pdf_builtin_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: path
    logical, intent(in), optional :: hoppet_b_matching
    data%model => model
    if (pdg_array_get_length (pdg_in) /= 1) &
         call msg_fatal ("PDF: incoming particle must be unique")
    call data%flv_in%init (pdg_array_get (pdg_in, 1), model)
    data%mask = .true.
    data%mask_photon = .true.
    select case (pdg_array_get (pdg_in, 1))
    case (PROTON)
       data%name = var_str (PDF_BUILTIN_DEFAULT_PROTON)
       data%invert = .false.
       data%photon = .false.
    case (-PROTON)
       data%name = var_str (PDF_BUILTIN_DEFAULT_PROTON)
       data%invert = .true.
       data%photon = .false.
       ! case (PIPLUS)
       !    data%name = var_str (PDF_BUILTIN_DEFAULT_PION)
       !    data%invert = .false.
       !    data%photon = .false.
       ! case (-PIPLUS)
       !    data%name = var_str (PDF_BUILTIN_DEFAULT_PION)
       !    data%invert = .true.
       !    data%photon = .false.
       ! case (PHOTON)
       !    data%name = var_str (PDF_BUILTIN_DEFAULT_PHOTON)
       !    data%invert = .false.
       !    data%photon = .true.
    case default
       call msg_fatal ("PDF: " &
            // "incoming particle must either proton or antiproton.")
       return
    end select
    data%name = name
    data%id = pdf_get_id (data%name)
    if (data%id < 0) call msg_fatal ("unknown PDF set " // char (data%name))
    data%has_photon = pdf_provides_photon (data%id)
    if (present (hoppet_b_matching))  data%hoppet_b_matching = hoppet_b_matching
    call pdf_init (data%id, path)
    if (data%hoppet_b_matching)  call hoppet_init (.true., pdf_id = data%id)
  end subroutine pdf_builtin_data_init

  subroutine pdf_builtin_data_set_mask (data, mask)
    class(pdf_builtin_data_t), intent(inout) :: data
    logical, dimension(-6:6), intent(in) :: mask
    data%mask = mask
  end subroutine pdf_builtin_data_set_mask

  subroutine pdf_builtin_data_write (data, unit, verbose)
    class(pdf_builtin_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose    
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "PDF builtin data:"
    if (data%id < 0) then
       write (u, "(3x,A)") "[undefined]"
       return
    end if
    write (u, "(3x,A)", advance="no") "flavor       = "
    call data%flv_in%write (u);  write (u, *)
    write (u, "(3x,A,A)")   "name         = ", char (data%name)
    write (u, "(3x,A,L1)")  "invert       = ", data%invert
    write (u, "(3x,A,L1)")  "has photon   = ", data%has_photon
    write (u, "(3x,A,6(1x,L1),1x,A,1x,L1,1x,A,6(1x,L1))") &
         "mask         =", &
         data%mask(-6:-1), "*", data%mask(0), "*", data%mask(1:6)
    write (u, "(3x,A,L1)") "photon mask  = ", data%mask_photon
    write (u, "(3x,A,L1)") "hoppet_b     = ", data%hoppet_b_matching    
  end subroutine pdf_builtin_data_write

  function pdf_builtin_data_get_n_par (data) result (n)
    class(pdf_builtin_data_t), intent(in) :: data
    integer :: n
    n = 1
  end function pdf_builtin_data_get_n_par
  
  subroutine pdf_builtin_data_get_pdg_out (data, pdg_out)
    class(pdf_builtin_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    integer :: n, np, i
    n = count (data%mask)
    np = 0;  if (data%has_photon .and. data%mask_photon)  np = 1
    allocate (pdg1 (n + np))
    pdg1(1:n) = pack ([(i, i = -6, 6)], data%mask)
    if (np == 1)  pdg1(n+np) = PHOTON
    pdg_out(1) = pdg1
  end subroutine pdf_builtin_data_get_pdg_out
  
  subroutine pdf_builtin_data_allocate_sf_int (data, sf_int)
    class(pdf_builtin_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (pdf_builtin_t :: sf_int)
  end subroutine pdf_builtin_data_allocate_sf_int
  
  function pdf_builtin_data_get_pdf_set (data) result (pdf_set)
    class(pdf_builtin_data_t), intent(in) :: data
    integer :: pdf_set
    pdf_set = data%id
  end function pdf_builtin_data_get_pdf_set
  
  function pdf_builtin_type_string (object) result (string)
    class(pdf_builtin_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "PDF builtin: " // object%data%name
    else
       string = "PDF builtin: [undefined]"
    end if
  end function pdf_builtin_type_string
  
  subroutine pdf_builtin_write (object, unit, testflag)
    class(pdf_builtin_t), intent(in) :: object
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
       write (u, "(1x,A)")  "PDF builtin data: [undefined]"
    end if
  end subroutine pdf_builtin_write
    
  subroutine pdf_builtin_init (sf_int, data)
    class(pdf_builtin_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    type(flavor_t) :: flv, flv_remnant
    type(color_t) :: col0
    type(quantum_numbers_t), dimension(3) :: qn
    integer :: i
    select type (data)
    type is (pdf_builtin_data_t)
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
               col = color_from_flavor (flv, 1, reverse = .true.))
          call sf_int%add_state (qn)
       end if
       call sf_int%freeze ()
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
       sf_int%status = SF_INITIAL
    end select
  end subroutine pdf_builtin_init

  subroutine pdf_builtin_complete_kinematics (sf_int, x, f, r, rb, map)
    class(pdf_builtin_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1
    if (map) then
       call msg_fatal ("PDF builtin: map flag not supported")
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
  end subroutine pdf_builtin_complete_kinematics

  subroutine pdf_builtin_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(pdf_builtin_t), intent(inout) :: sf_int
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
       call msg_fatal ("PDF builtin: map flag not supported")
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
  end subroutine pdf_builtin_inverse_kinematics

  subroutine pdf_builtin_apply (sf_int, scale)
    class(pdf_builtin_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default), dimension(-6:6) :: ff
    real(double), dimension(-6:6) :: ff_dbl
    real(default) :: x, fph
    real(double) :: xx, qq
    complex(default), dimension(:), allocatable :: fc
    associate (data => sf_int%data)
      sf_int%q = scale
      x = sf_int%x
      xx = x
      qq = scale
      if (data%invert) then
         if (data%has_photon) then
            call pdf_evolve (data%id, x, scale, ff(6:-6:-1), fph)
         else
            if (data%hoppet_b_matching) then
               call hoppet_eval (xx, qq, ff_dbl(6:-6:-1))
               ff = ff_dbl
            else
               call pdf_evolve (data%id, x, scale, ff(6:-6:-1))
            end if
         end if
      else
         if (data%has_photon) then
            call pdf_evolve (data%id, x, scale, ff, fph)
         else
            if (data%hoppet_b_matching) then
               call hoppet_eval (xx, qq, ff_dbl)
               ff = ff_dbl
            else
               call pdf_evolve (data%id, x, scale, ff)               
            end if
         end if
      end if
      if (data%has_photon) then
         allocate (fc (count ([data%mask, data%mask_photon])))
         fc = max (pack ([ff, fph], &
              [data%mask, data%mask_photon]), 0._default)
      else
         allocate (fc (count (data%mask)))
         fc = max (pack (ff, data%mask), 0._default)
      end if
    end associate
    call sf_int%set_matrix_element (fc)
    sf_int%status = SF_EVALUATED
  end subroutine pdf_builtin_apply

  subroutine alpha_qcd_pdf_builtin_write (object, unit)
    class(alpha_qcd_pdf_builtin_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A)")  "QCD parameters (pdf_builtin):"
    write (u, "(5x,A,A)")  "PDF set = ", char (object%pdfset_name)
    write (u, "(5x,A,I0)") "PDF ID  = ", object%pdfset_id
  end subroutine alpha_qcd_pdf_builtin_write
  
  function alpha_qcd_pdf_builtin_get (alpha_qcd, scale) result (alpha)
    class(alpha_qcd_pdf_builtin_t), intent(in) :: alpha_qcd
    real(default), intent(in) :: scale
    real(default) :: alpha
    alpha = pdf_alphas (alpha_qcd%pdfset_id, scale)
  end function alpha_qcd_pdf_builtin_get
  
  subroutine alpha_qcd_pdf_builtin_init (alpha_qcd, name, path)
    class(alpha_qcd_pdf_builtin_t), intent(out) :: alpha_qcd
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: path
    alpha_qcd%pdfset_name = name
    alpha_qcd%pdfset_id = pdf_get_id (name)
    if (alpha_qcd%pdfset_id < 0) &
         call msg_fatal ("QCD parameter initialization: PDF set " &
         // char (name) // " is unknown")
    call pdf_init (alpha_qcd%pdfset_id, path)
  end subroutine alpha_qcd_pdf_builtin_init
    

end module sf_pdf_builtin
