! WHIZARD 2.2.1 June 3 2014
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

module sf_pdf_builtin

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PROTON !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PION !NODEP!
  use limits, only: PDF_BUILTIN_DEFAULT_PHOTON !NODEP!
  use limits, only: FMT_17 !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use pdf_builtin !NODEP!
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

  implicit none
  private

  public :: pdf_builtin_data_t
  public :: alpha_qcd_pdf_builtin_t
  public :: sf_pdf_builtin_test

  type, extends (sf_data_t) :: pdf_builtin_data_t
     private
     integer :: id = -1
     type (string_t) :: name
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     logical :: invert
     logical :: has_photon
     logical :: photon
     logical, dimension(-6:6) :: mask
     logical :: mask_photon
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
  

contains

  subroutine pdf_builtin_data_init (data, pdf_status, model, pdg_in, name, path)
    class(pdf_builtin_data_t), intent(out) :: data
    type(pdf_builtin_status_t), intent(inout) :: pdf_status
    type(model_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: path
    data%model => model
    if (pdg_array_get_length (pdg_in) /= 1) &
         call msg_fatal ("PDF: incoming particle must be unique")
    call flavor_init (data%flv_in, pdg_array_get (pdg_in, 1), model)
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
    call pdf_init (pdf_status, data%id, path)
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
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "PDF builtin data:"
    if (data%id < 0) then
       write (u, "(3x,A)") "[undefined]"
       return
    end if
    write (u, "(3x,A)", advance="no") "flavor       = "
    call flavor_write (data%flv_in, u);  write (u, *)
    write (u, "(3x,A,A)")   "name         = ", char (data%name)
    write (u, "(3x,A,L1)")  "invert       = ", data%invert
    write (u, "(3x,A,L1)")  "has photon   = ", data%has_photon
    write (u, "(3x,A,6(1x,L1),1x,A,1x,L1,1x,A,6(1x,L1))") &
         "mask         =", &
         data%mask(-6:-1), "*", data%mask(0), "*", data%mask(1:6)
    write (u, "(3x,A,L1)") "photon mask  = ", data%mask_photon
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
       write (u, "(1x,A)")  "PDF builtin data: [undefined]"
    end if
  end subroutine pdf_builtin_write
    
  subroutine pdf_builtin_init (sf_int, data)
    class(pdf_builtin_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    type(flavor_t) :: flv, flv_remnant
    type(quantum_numbers_t), dimension(3) :: qn
    integer :: i
    select type (data)
    type is (pdf_builtin_data_t)
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
               col = color_from_flavor (flv, 1, reverse = .true.))
          call interaction_add_state (sf_int%interaction_t, qn)
       end if
       call interaction_freeze (sf_int%interaction_t)
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
    real(default) :: x, fph
    complex(default), dimension(:), allocatable :: fc
    associate (data => sf_int%data)
      sf_int%q = scale
      x = sf_int%x
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
         allocate (fc (count ([data%mask, data%mask_photon])))
         fc = max (pack ([ff, fph], &
              [data%mask, data%mask_photon]), 0._default)
      else
         allocate (fc (count (data%mask)))
         fc = max (pack (ff, data%mask), 0._default)
      end if
    end associate
    call interaction_set_matrix_element (sf_int%interaction_t, fc)
    sf_int%status = SF_EVALUATED
  end subroutine pdf_builtin_apply

  subroutine alpha_qcd_pdf_builtin_write (object, unit)
    class(alpha_qcd_pdf_builtin_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
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
  
  subroutine alpha_qcd_pdf_builtin_init (alpha_qcd, status, name, path)
    class(alpha_qcd_pdf_builtin_t), intent(out) :: alpha_qcd
    type(pdf_builtin_status_t), intent(inout) :: status
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: path
    alpha_qcd%pdfset_name = name
    alpha_qcd%pdfset_id = pdf_get_id (name)
    if (alpha_qcd%pdfset_id < 0) &
         call msg_fatal ("QCD parameter initialization: PDF set " &
         // char (name) // " is unknown")
    call pdf_init (status, alpha_qcd%pdfset_id, path)
  end subroutine alpha_qcd_pdf_builtin_init
    

  subroutine sf_pdf_builtin_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_pdf_builtin_1, "sf_pdf_builtin_1", &
         "structure function configuration", &
         u, results)
    call test (sf_pdf_builtin_2, "sf_pdf_builtin_2", &
         "structure function instance", &
         u, results)
    call test (sf_pdf_builtin_3, "sf_pdf_builtin_3", &
         "running alpha_s", &
         u, results)
  end subroutine sf_pdf_builtin_test
  
  subroutine sf_pdf_builtin_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    type(pdf_builtin_status_t) :: status
    type(string_t) :: name
    
    write (u, "(A)")  "* Test output: sf_pdf_builtin_1"
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

    allocate (pdf_builtin_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    name = "CTEQ6L"

    select type (data)
    type is (pdf_builtin_data_t)
       call data%init (status, model, pdg_in, name, &
            os_data%pdf_builtin_datapath)
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
    write (u, "(A)")  "* Test output end: sf_pdf_builtin_1"

  end subroutine sf_pdf_builtin_1

  subroutine sf_pdf_builtin_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(pdf_builtin_status_t) :: status
    type(string_t) :: name
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_pdf_builtin_2"
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
    
    name = "CTEQ6L"

    allocate (pdf_builtin_data_t :: data)
    select type (data)
    type is (pdf_builtin_data_t)
       call data%init (status, model, pdg_in, name, &
            os_data%pdf_builtin_datapath)
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
    call sf_int%write (u)


    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_pdf_builtin_2"

  end subroutine sf_pdf_builtin_2

  subroutine sf_pdf_builtin_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(pdf_builtin_status_t) :: status
    type(qcd_t) :: qcd
    type(string_t) :: name
    
    write (u, "(A)")  "* Test output: sf_pdf_builtin_3"
    write (u, "(A)")  "*   Purpose: initialize and evaluate alpha_s"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)

    name = "CTEQ6L"
       
    write (u, "(A)")  "* Initialize qcd object"
    write (u, "(A)")
    
    allocate (alpha_qcd_pdf_builtin_t :: qcd%alpha)
    select type (alpha => qcd%alpha)
    type is (alpha_qcd_pdf_builtin_t)
       call alpha%init (status, name, os_data%pdf_builtin_datapath)
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
    write (u, "(A)")  "* Test output end: sf_pdf_builtin_3"

  end subroutine sf_pdf_builtin_3


end module sf_pdf_builtin
