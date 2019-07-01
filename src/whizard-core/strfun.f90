! WHIZARD 2.1.1 September 18 2012
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

module strfun

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use models
  use quantum_numbers
  use interactions
  use evaluators
  use beams
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_circe1
  use sf_circe2
  use sf_escan
  use sf_beam_events
  use sf_lhapdf
  use sf_pdf_builtin
  use sf_user

  implicit none
  private

  public :: strfun_chain_t
  public :: strfun_chain_init
  public :: strfun_chain_allocate_mappings
  public :: strfun_chain_set_beam_momenta
  public :: strfun_chain_final
  public :: strfun_chain_write
  public :: assignment(=)
  public :: strfun_chain_get_strfun_type
  public :: strfun_chain_get_strfun_set
  public :: strfun_chain_get_n_strfun
  public :: strfun_chain_get_n_parameters_tot
  public :: strfun_chain_get_n_vir
  public :: strfun_chain_multichannel_enabled
  public :: strfun_chain_get_mapping_factor
  public :: strfun_chain_dimension_is_rigid
  public :: strfun_chain_get_colliding_particles
  public :: strfun_chain_get_colliding_particles_mask
  public :: strfun_chain_get_beam_int_ptr
  public :: strfun_chain_get_last_evaluator_ptr
  public :: strfun_chain_set_strfun
  public :: strfun_chain_set_mapping
  public :: strfun_chain_make_evaluators
  public :: strfun_chain_set_kinematics
  public :: strfun_chain_evaluate
  public :: strfun_test

  integer, parameter, public :: STRF_NONE = 0
  integer, parameter, public :: STRF_LHAPDF = 1, STRF_ISR = 2, &
       STRF_EPA = 3, STRF_EWA = 4, STRF_CIRCE1 = 5, STRF_CIRCE2 = 6, &
       STRF_ESCAN = 7, STRF_BEVT = 8, STRF_PDF_BUILTIN = 9
  integer, parameter, public :: STRF_USER = 99
  
  integer, parameter, public :: SFM_NONE = 0
  integer, parameter, public :: SFM_PAIR = 1
  integer, parameter, public :: SFM_PAIR_RESONANCE = 2

  type :: strfun_t
     private
     integer :: type = STRF_NONE
     type(string_t) :: name
     type(interaction_t) :: int
     type(lhapdf_data_t), dimension(:), allocatable :: lhapdf_data
     type(pdf_builtin_data_t), dimension(:), allocatable :: pdf_builtin_data
     type(isr_data_t), dimension(:), allocatable :: isr_data
     type(epa_data_t), dimension(:), allocatable :: epa_data
     type(ewa_data_t), dimension(:), allocatable :: ewa_data     
     type(circe1_data_t), dimension(:), allocatable :: circe1_data     
     type(circe2_data_t), dimension(:), allocatable :: circe2_data     
     type(escan_data_t), dimension(:), allocatable :: escan_data     
     type(beam_events_data_t), dimension(:), allocatable :: beam_events_data
     type(sf_user_data_t), dimension(:), allocatable :: user_data
     real(default) :: x = 0, f = 1, s = 0
     real(default), dimension(:), allocatable :: user_xval
     real(default) :: scale = 0
  end type strfun_t

  type :: strfun_mapping_t
     private
     integer, dimension(:), allocatable :: index
     integer :: type = SFM_NONE
     real(default) :: p = 0
     real(default) :: m2 = 0, mg = 0, s = 0
     real(default) :: a1 = 0, a2 = 0, a3 = 0
  end type strfun_mapping_t

  type :: strfun_chain_t
     private
     type(beam_t) :: beam
     integer :: n_strfun = 0
     logical :: multichannel = .false.
     integer :: n_mapping = 0
     type(strfun_t), dimension(:), allocatable :: strfun
     type(strfun_mapping_t), dimension(:,:), allocatable :: sf_mapping
     real(default) :: mapping_factor = 0
     integer :: n_parameters_tot = 0
     integer, dimension(:), allocatable :: n_parameters
     type(evaluator_t), dimension(:), allocatable :: eval
     integer, dimension(:), allocatable :: last_strfun
     integer, dimension(:), allocatable :: out_index
     integer, dimension(:), allocatable :: coll_index
  end type strfun_chain_t


  interface strfun_init
     module procedure strfun_init_lhapdf
     module procedure strfun_init_isr
     module procedure strfun_init_epa
     module procedure strfun_init_ewa
     module procedure strfun_init_circe1
     module procedure strfun_init_circe2
     module procedure strfun_init_escan
     module procedure strfun_init_beam_events
     module procedure strfun_init_pdf_builtin
     module procedure strfun_init_user
  end interface

  interface strfun_final
     module procedure strfun_final0
     module procedure strfun_final1
  end interface
  interface assignment(=)
     module procedure strfun_chain_assign
  end interface

  interface strfun_chain_set_strfun
     module procedure strfun_chain_set_lhapdf
     module procedure strfun_chain_set_pdf_builtin
     module procedure strfun_chain_set_isr
     module procedure strfun_chain_set_epa
     module procedure strfun_chain_set_ewa     
     module procedure strfun_chain_set_circe1     
     module procedure strfun_chain_set_circe2     
     module procedure strfun_chain_set_escan
     module procedure strfun_chain_set_beam_events
     module procedure strfun_chain_set_user
  end interface

contains

  subroutine strfun_init_lhapdf (strfun, lhapdf_data)
    type(strfun_t), intent(out) :: strfun
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    strfun%type = STRF_LHAPDF
    strfun%name = "LHAPDF"
    allocate (strfun%lhapdf_data (1))
    strfun%lhapdf_data = lhapdf_data
    call interaction_init_lhapdf (strfun%int, lhapdf_data)
  end subroutine strfun_init_lhapdf

  subroutine strfun_init_pdf_builtin (strfun, pdf_builtin_data)
    type(strfun_t), intent(out) :: strfun
    type(pdf_builtin_data_t), intent(in) :: pdf_builtin_data
    strfun%type = STRF_PDF_BUILTIN
    strfun%name = "builtin PDF: " // pdf_builtin_get_name (pdf_builtin_data)
    allocate (strfun%pdf_builtin_data (1))
    strfun%pdf_builtin_data = pdf_builtin_data
    call interaction_init_pdf_builtin (strfun%int, pdf_builtin_data)
  end subroutine strfun_init_pdf_builtin  

  subroutine strfun_init_isr (strfun, isr_data)
    type(strfun_t), intent(out) :: strfun
    type(isr_data_t), intent(in) :: isr_data
    strfun%type = STRF_ISR
    strfun%name = "ISR"
    allocate (strfun%isr_data (1))
    strfun%isr_data = isr_data
    call interaction_init_isr (strfun%int, isr_data)
  end subroutine strfun_init_isr

  subroutine strfun_init_epa (strfun, epa_data)
    type(strfun_t), intent(out) :: strfun
    type(epa_data_t), intent(in) :: epa_data
    strfun%type = STRF_EPA
    strfun%name = "EPA"
    allocate (strfun%epa_data (1))
    strfun%epa_data = epa_data
    call interaction_init_epa (strfun%int, epa_data)
  end subroutine strfun_init_epa
  
  subroutine strfun_init_ewa (strfun, ewa_data, id)
    type(strfun_t), intent(out) :: strfun
    type(ewa_data_t), intent(inout) :: ewa_data
    integer, intent(in) :: id
    strfun%type = STRF_EWA
    strfun%name = "EWA"
    allocate (strfun%ewa_data (1))
    call ewa_set_id (ewa_data, id)
    strfun%ewa_data = ewa_data
    call interaction_init_ewa (strfun%int, ewa_data)
  end subroutine strfun_init_ewa
  
  subroutine strfun_init_circe1 (strfun, circe1_data)
    type(strfun_t), intent(out) :: strfun
    type(circe1_data_t), intent(in) :: circe1_data
    strfun%type = STRF_CIRCE1
    strfun%name = "CIRCE1"
    allocate (strfun%circe1_data (1))
    strfun%circe1_data = circe1_data
    call interaction_init_circe1 (strfun%int, circe1_data)
  end subroutine strfun_init_circe1
  
  subroutine strfun_init_circe2 (strfun, circe2_data)
    type(strfun_t), intent(out) :: strfun
    type(circe2_data_t), intent(in) :: circe2_data
    strfun%type = STRF_CIRCE2
    strfun%name = "CIRCE2"
    allocate (strfun%circe2_data (1))
    strfun%circe2_data = circe2_data
    call interaction_init_circe2 (strfun%int, circe2_data)
  end subroutine strfun_init_circe2

  subroutine strfun_init_escan (strfun, escan_data)
    type(strfun_t), intent(out) :: strfun
    type(escan_data_t), intent(in) :: escan_data
    strfun%type = STRF_ESCAN
    strfun%name = "Energy scan"
    allocate (strfun%escan_data (1))
    strfun%escan_data = escan_data
    call interaction_init_escan (strfun%int, escan_data)
  end subroutine strfun_init_escan

  subroutine strfun_init_beam_events (strfun, beam_events_data)
    type(strfun_t), intent(out) :: strfun
    type(beam_events_data_t), intent(in) :: beam_events_data
    strfun%type = STRF_BEVT
    strfun%name = "Beam events"
    allocate (strfun%beam_events_data (1))
    strfun%beam_events_data = beam_events_data
    call interaction_init_beam_events (strfun%int, beam_events_data)
  end subroutine strfun_init_beam_events

  subroutine strfun_init_user (strfun, user_data)
    type(strfun_t), intent(out) :: strfun
    type(sf_user_data_t), intent(in) :: user_data
    strfun%type = STRF_USER
    strfun%name = "User structure function: " &
         // sf_user_data_get_name (user_data)
    allocate (strfun%user_data (1))
    strfun%user_data = user_data
    call interaction_init_sf_user (strfun%int, user_data)
    allocate (strfun%user_xval (sf_user_data_get_n_var (user_data)))
  end subroutine strfun_init_user  

  subroutine strfun_final1 (strfun)
    type(strfun_t), dimension(:), intent(inout) :: strfun
    integer :: i
    do i = 1, size (strfun)
       call strfun_final0 (strfun(i))
    end do
  end subroutine strfun_final1

  subroutine strfun_final0 (strfun)
    type(strfun_t), intent(inout) :: strfun
    select case (strfun%type)
    case (STRF_ISR)
       deallocate (strfun%isr_data)
    case (STRF_EPA)
       deallocate (strfun%epa_data)
    case (STRF_EWA)
       deallocate (strfun%ewa_data)
    case (STRF_CIRCE1)
       deallocate (strfun%circe1_data)
    case (STRF_CIRCE2)
       deallocate (strfun%circe2_data)
    case (STRF_ESCAN)
       deallocate (strfun%escan_data)
    case (STRF_BEVT)
       call beam_events_data_close (strfun%beam_events_data(1))
       deallocate (strfun%beam_events_data)
    case (STRF_LHAPDF)
       deallocate (strfun%lhapdf_data)
    case (STRF_PDF_BUILTIN)
       call pdf_builtin_final (strfun%pdf_builtin_data(1))
       deallocate (strfun%pdf_builtin_data)
    case (STRF_USER)
       deallocate (strfun%user_data)
       deallocate (strfun%user_xval)
    end select
    call interaction_final (strfun%int)
    strfun%type = STRF_NONE
  end subroutine strfun_final0

  subroutine strfun_write (strfun, unit, verbose, show_momentum_sum, show_mass)
    type(strfun_t), intent(in) :: strfun
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (strfun%type /= STRF_NONE) then
       write (u, *) char (strfun_get_name (strfun)) // " setup:"
       select case (strfun%type)
       case (STRF_LHAPDF)
          call lhapdf_data_write (strfun%lhapdf_data(1), u)
          write (u, *) "LHAPDF event data:"
          write (u, *) "  x     =", strfun%x
          write (u, *) "  f     =", strfun%f
          write (u, *) "  scale =", strfun%scale
          write (u, *) "  p2    =", strfun%s
       case (STRF_PDF_BUILTIN)
          call pdf_builtin_data_write (strfun%pdf_builtin_data(1), u)
          write (u, *) "PDF event data:"
          write (u, *) "  x     =", strfun%x
          write (u, *) "  f     =", strfun%f
          write (u, *) "  scale =", strfun%scale
          write (u, *) "  p2    =", strfun%s
       case (STRF_ISR)
          call isr_data_write (strfun%isr_data(1), u)
       case (STRF_EPA)
          call epa_data_write (strfun%epa_data(1), u)
       case (STRF_EWA)
          call ewa_data_write (strfun%ewa_data(1), u)
       case (STRF_CIRCE1)
          call circe1_data_write (strfun%circe1_data(1), u)
       case (STRF_CIRCE2)
          call circe2_data_write (strfun%circe2_data(1), u)
       case (STRF_ESCAN)
          call escan_data_write (strfun%escan_data(1), u)
       case (STRF_BEVT)
          call beam_events_data_write (strfun%beam_events_data(1), u)
       case (STRF_USER)
          call sf_user_data_write (strfun%user_data(1), u)
          write (u, *) "User event data:"
          if (allocated (strfun%user_xval)) then
             write (u, *) "  x     =", strfun%user_xval
          else
             write (u, *) "  x     = [not allocated]"
          end if
          write (u, *) "  scale =", strfun%scale
       end select
       call interaction_write &
            (strfun%int, unit, verbose, show_momentum_sum, show_mass)
    else
       write (u, *) "Structure function setup: [empty]"
    end if
  end subroutine strfun_write

  function strfun_get_name (strfun) result (name)
    type(string_t) :: name
    type(strfun_t), intent(in) :: strfun
    name = strfun%name
  end function strfun_get_name

  function strfun_get_type (strfun) result (type)
    integer :: type
    type(strfun_t), intent(in) :: strfun
    type = strfun%type
  end function strfun_get_type

  subroutine strfun_set_kinematics (strfun, r, no_map)
    type(strfun_t), intent(inout) :: strfun
    real(default), dimension(:), intent(in) :: r
    logical, intent(in) :: no_map
    select case (strfun%type)
    case (STRF_LHAPDF)
       call interaction_set_kinematics_lhapdf (strfun%int, &
            strfun%x, strfun%f, strfun%s, r(1), strfun%lhapdf_data(1))
    case (STRF_PDF_BUILTIN)
       call interaction_set_kinematics_pdf_builtin (strfun%int, &
            strfun%x, strfun%f, strfun%s, r(1), strfun%pdf_builtin_data(1))
    case (STRF_ISR)
       call interaction_apply_isr (strfun%int, r, strfun%isr_data(1), no_map)
    case (STRF_EPA)
       call interaction_apply_epa (strfun%int, r, strfun%epa_data, no_map)
    case (STRF_EWA)
       call interaction_apply_ewa (strfun%int, r, strfun%ewa_data, no_map)
    case (STRF_CIRCE1)
       call interaction_apply_circe1 &
            (strfun%int, r, strfun%circe1_data(1), no_map)
    case (STRF_CIRCE2)
       call interaction_apply_circe2 &
            (strfun%int, r, strfun%circe2_data(1), no_map)
    case (STRF_ESCAN)
       call interaction_apply_escan &
            (strfun%int, r, strfun%escan_data(1))
    case (STRF_BEVT)
       call interaction_apply_beam_events &
            (strfun%int, strfun%beam_events_data(1))
    case (STRF_USER)
       call interaction_set_kinematics_sf_user (strfun%int, &
            strfun%user_xval, r, strfun%user_data(1))
    end select
  end subroutine strfun_set_kinematics

  subroutine strfun_apply (strfun, scale)
    type(strfun_t), intent(inout) :: strfun
    real(default), intent(in) :: scale
    strfun%scale = scale
    select case (strfun%type)
    case (STRF_LHAPDF)
       call interaction_apply_lhapdf (strfun%int, scale, &
            strfun%x, strfun%f, strfun%s, strfun%lhapdf_data(1))
    case (STRF_PDF_BUILTIN)
       call interaction_apply_pdf_builtin (strfun%int, scale, &
            strfun%x, strfun%f, strfun%s, strfun%pdf_builtin_data(1))
    case (STRF_USER)
       call interaction_apply_sf_user (strfun%int, scale, &
            strfun%user_xval, strfun%user_data(1))
    end select
  end subroutine strfun_apply
    
  subroutine strfun_mapping_init (sf_mapping, index, type, par)
    type(strfun_mapping_t), intent(out) :: sf_mapping
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: type
    real(default), dimension(:), intent(in) :: par
    real(default) :: s, m2, mg
    allocate (sf_mapping%index (size (index)))
    sf_mapping%index = index
    sf_mapping%type = type
    select case (type)
    case (SFM_PAIR)
       sf_mapping%p = par(1)
    case (SFM_PAIR_RESONANCE)
       s = par(1)**2
       m2 = par(2)**2
       mg = par(2) * par(3)
       sf_mapping%s = s
       sf_mapping%m2 = m2
       sf_mapping%mg = mg
       sf_mapping%a1 = atan (- m2 / mg)
       sf_mapping%a2 = atan ((s - m2) / mg)
       sf_mapping%a3 = (sf_mapping%a2 - sf_mapping%a1) * mg / s
    end select
  end subroutine strfun_mapping_init

  subroutine strfun_mapping_write (sf_mapping, unit)
    type(strfun_mapping_t), intent(in) :: sf_mapping
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)", advance="no") "Strfun mapping for indices: "
    write (u, "(10(1x,I0))")  sf_mapping%index
    write (u, "(1x,A,1x,I0)")  "mapping type =", sf_mapping%type
    write (u, "(1x,A)", advance="no")  "mapping pars ="
    select case (sf_mapping%type)
    case (SFM_NONE)
       write (u, *) "[none]"
    case (SFM_PAIR)
       write (u, *) sf_mapping%p
    case (SFM_PAIR_RESONANCE)
       write (u, *)  sf_mapping%s, sf_mapping%m2, sf_mapping%mg
    end select
  end subroutine strfun_mapping_write

  subroutine strfun_mapping_apply (sf_mapping, x, factor)
    type(strfun_mapping_t), intent(in) :: sf_mapping
    real(default), dimension(:), intent(inout) :: x
    real(default), intent(out) :: factor
    real(default) :: f1, f2
    real(default), dimension(2) :: x2
    select case (sf_mapping%type)
    case (SFM_PAIR)
       x2 = x(sf_mapping%index)
       call map_unit_square (x2, factor, sf_mapping%p)
       x(sf_mapping%index) = x2
    case (SFM_PAIR_RESONANCE)
       x2 = x(sf_mapping%index)
       call map_resonance (x2(1), f2, &
            sf_mapping%s, sf_mapping%m2, sf_mapping%mg, &
            sf_mapping%a1, sf_mapping%a2, sf_mapping%a3)
       call map_unit_square (x2, f1)
       x(sf_mapping%index) = x2
       factor = f1 * f2
    case default
       factor = 1
    end select
  end subroutine strfun_mapping_apply

  subroutine strfun_mapping_apply_inverse (sf_mapping, x, factor)
    type(strfun_mapping_t), intent(in) :: sf_mapping
    real(default), dimension(:), intent(inout) :: x
    real(default), intent(out) :: factor
    real(default) :: f1, f2
    real(default), dimension(2) :: x2
    select case (sf_mapping%type)
    case (SFM_PAIR)
       x2 = x(sf_mapping%index)
       call map_unit_square_inverse (x2, factor, sf_mapping%p)
       x(sf_mapping%index) = x2
    case (SFM_PAIR_RESONANCE)
       x2 = x(sf_mapping%index)
       call map_unit_square_inverse (x2, f1)
       call map_resonance_inverse (x2(1), f2, &
            sf_mapping%s, sf_mapping%m2, sf_mapping%mg, &
            sf_mapping%a1, sf_mapping%a2, sf_mapping%a3)
       x(sf_mapping%index) = x2
       factor = f1 * f2
    case default
       factor = 1
    end select
  end subroutine strfun_mapping_apply_inverse

  subroutine map_unit_square (x, factor, power)
    real(kind=default), dimension(2), intent(inout) :: x
    real(kind=default), intent(out) :: factor
    real(kind=default), intent(in), optional :: power
    real(kind=default) :: xx, yy
    factor = 1
    xx = x(1)
    yy = x(2)
    if (present(power)) then
       if (x(1) > 0 .and. power > 1) then
          xx = x(1)**power
          factor = factor * power * xx / x(1)
       end if
    end if
    if (xx /= 0) then
       x(1) = xx ** yy
       x(2) = xx / x(1)
       factor = factor * abs (log (xx))
    else
       x = 0
    end if
  end subroutine map_unit_square

  subroutine map_unit_square_inverse (x, factor, power)
    real(kind=default), dimension(2), intent(inout) :: x
    real(kind=default), intent(out) :: factor
    real(kind=default), intent(in), optional :: power
    real(kind=default) :: lg, xx, yy
    factor = 1
    xx = x(1) * x(2)
    if (xx /= 0) then
       lg = log (xx)
       yy = log (x(1)) / lg
       x(2) = yy
       factor = factor * abs (lg)
       if (present(power)) then
          x(1) = xx**(1._default/power)
          factor = factor * power * xx / x(1)
       else
          x(1) = xx
       end if
    else
       x = 0
    end if
  end subroutine map_unit_square_inverse

  subroutine map_resonance (x, factor, s, m2, mg, a1, a2, a3)
    real(default), intent(inout) :: x
    real(default), intent(out) :: factor
    real(default), intent(in) :: s, m2, mg, a1, a2, a3
    real(default) :: t, z
    z = (1 - x) * a1 + x * a2
    t = tan (z)
    x = (m2 + t * mg) / s
    factor = a3 * (1 + t**2)
  end subroutine map_resonance

  subroutine map_resonance_inverse (x, factor, s, m2, mg, a1, a2, a3)
    real(default), intent(inout) :: x
    real(default), intent(out) :: factor
    real(default), intent(in) :: s, m2, mg, a1, a2, a3
    real(default) :: t
    t = (x * s - m2) / mg
    x = (atan (t) - a1) / (a2 - a1)
    factor = a3 * (1 + t**2)
  end subroutine map_resonance_inverse

  subroutine strfun_chain_init (sfchain, beam_data, n_strfun)
    type(strfun_chain_t), intent(out) :: sfchain
    type(beam_data_t), intent(in), target :: beam_data
    integer, intent(in) :: n_strfun
    integer :: i
    sfchain%n_strfun = n_strfun
    allocate (sfchain%strfun (n_strfun))
    allocate (sfchain%n_parameters (n_strfun))
    sfchain%n_parameters = 0
    allocate (sfchain%eval (n_strfun))
    call beam_init (sfchain%beam, beam_data)
    allocate (sfchain%last_strfun (beam_data%n))
    allocate (sfchain%out_index (beam_data%n))
    allocate (sfchain%coll_index (beam_data%n))
    sfchain%last_strfun = 0
    do i = 1, size (sfchain%out_index)
       sfchain%out_index(i) = i
       sfchain%coll_index(i) = i
    end do
  end subroutine strfun_chain_init

  subroutine strfun_chain_allocate_mappings &
       (sfchain, multichannel, n_mapping, n_channel)
    type(strfun_chain_t), intent(inout) :: sfchain
    logical, intent(in) :: multichannel
    integer, intent(in) :: n_mapping, n_channel
    sfchain%multichannel = multichannel
    sfchain%n_mapping = n_mapping
    allocate (sfchain%sf_mapping (n_mapping, n_channel))
  end subroutine strfun_chain_allocate_mappings

  subroutine strfun_chain_set_beam_momenta (sfchain, p)
    type(strfun_chain_t), intent(inout) :: sfchain
    type(vector4_t), dimension(:), intent(in) :: p
    call beam_set_momenta (sfchain%beam, p)
  end subroutine strfun_chain_set_beam_momenta

  subroutine strfun_chain_final (sfchain)
    type(strfun_chain_t), intent(inout) :: sfchain
    call beam_final (sfchain%beam)
    if (allocated (sfchain%strfun))  call strfun_final (sfchain%strfun)
    if (allocated (sfchain%eval))  call evaluator_final (sfchain%eval)
  end subroutine strfun_chain_final

  subroutine strfun_chain_write &
       (sfchain, unit, verbose, show_momentum_sum, show_mass)
    type(strfun_chain_t), intent(in) :: sfchain
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u, i, ch
    logical :: verb
    verb = .false.;  if (present (verbose))  verb = verbose
    u = output_unit (unit);  if (u < 0)  return
    write (u, *)  "Structure function chain:"
    write (u, *)
    call beam_write (sfchain%beam, unit, verbose, show_momentum_sum, show_mass)
    if (allocated (sfchain%strfun)) then
       do i = 1, size (sfchain%strfun)
          write (u, *)
          call strfun_write &
               (sfchain%strfun(i), unit, verbose, show_momentum_sum, show_mass)
          write (u, *)  "number of parameters = ", sfchain%n_parameters(i)
       end do
    end if
    if (allocated (sfchain%sf_mapping)) then
       if (sfchain%multichannel) then
          do ch = 1, size (sfchain%sf_mapping, 2)
             write (u, *)
             write (u, *) "Mappings for channel #", ch
             do i = 1, size (sfchain%sf_mapping, 1)
                call strfun_mapping_write (sfchain%sf_mapping(i, ch), unit)
             end do
          end do
       else
          do i = 1, size (sfchain%sf_mapping, 1)
             write (u, *)
             call strfun_mapping_write (sfchain%sf_mapping(i,1), unit)
          end do
       end if
    end if
    if (allocated (sfchain%eval)) then
       write (u, *)
       write (u, *) "Evaluators:"
       do i = 1, size (sfchain%eval)
          call evaluator_write &
               (sfchain%eval(i), unit, verbose, show_momentum_sum, show_mass)
       end do
    end if
    write (u, *)
    write (u, *)  "Total number of parameters      = ", &
         sfchain%n_parameters_tot
    write (u, "(1x,A)", advance="no")  "Last structure function (index) = "
    if (allocated (sfchain%last_strfun)) then
       write (u, *) sfchain%last_strfun
    else
       write (u, *) "[not allocated]"
    end if
    write (u, "(1x,A)", advance="no")  "Outgoing particles (index)      = "
    if (allocated (sfchain%out_index)) then
       write (u, *) sfchain%out_index
    else
       write (u, *) "[not allocated]"
    end if
    write (u, "(1x,A)", advance="no")  "Colliding particles (index)     = "
    if (allocated (sfchain%coll_index)) then
       write (u, *)  sfchain%coll_index
    else
       write (u, *) "[not allocated]"
    end if
  end subroutine strfun_chain_write

  subroutine strfun_chain_assign (sfchain_out, sfchain_in)
    type(strfun_chain_t), intent(out) :: sfchain_out
    type(strfun_chain_t), intent(in) :: sfchain_in
    sfchain_out%beam = sfchain_in%beam
    sfchain_out%n_strfun = sfchain_in%n_strfun
    sfchain_out%multichannel = sfchain_in%multichannel
    sfchain_out%n_mapping = sfchain_in%n_mapping
    if (allocated (sfchain_in%strfun)) then
       allocate (sfchain_out%strfun (size (sfchain_in%strfun)))
       sfchain_out%strfun = sfchain_in%strfun
    end if
    if (allocated (sfchain_in%sf_mapping)) then
       allocate (sfchain_out%sf_mapping &
            (size (sfchain_in%sf_mapping, 1), size (sfchain_in%sf_mapping, 2)))
       sfchain_out%sf_mapping = sfchain_in%sf_mapping
    end if
    sfchain_out%mapping_factor = sfchain_in%mapping_factor
    sfchain_out%n_parameters_tot = sfchain_in%n_parameters_tot
    if (allocated (sfchain_in%n_parameters)) then
       allocate (sfchain_out%n_parameters (size (sfchain_in%n_parameters)))
       sfchain_out%n_parameters = sfchain_in%n_parameters
    end if
    if (allocated (sfchain_in%eval)) then
       allocate (sfchain_out%eval (size (sfchain_in%eval)))
       sfchain_out%eval = sfchain_in%eval
    end if
    if (allocated (sfchain_in%last_strfun)) then
       allocate (sfchain_out%last_strfun (size (sfchain_in%last_strfun)))
       sfchain_out%last_strfun = sfchain_in%last_strfun
    end if
    if (allocated (sfchain_in%out_index)) then
       allocate (sfchain_out%out_index (size (sfchain_in%out_index)))
       sfchain_out%out_index = sfchain_in%out_index
    end if
    if (allocated (sfchain_in%coll_index)) then
       allocate (sfchain_out%coll_index (size (sfchain_in%coll_index)))
       sfchain_out%coll_index = sfchain_in%coll_index
    end if
  end subroutine strfun_chain_assign

  function strfun_chain_get_strfun_type(sfchain) result(type)
    type(strfun_chain_t), intent(in) :: sfchain
    integer :: type

    if(size(sfchain%strfun).eq.2) then
       if(sfchain%strfun(1)%type .eq. sfchain%strfun(2)%type) then
          type = sfchain%strfun(1)%type
       else
          type = STRF_NONE
       end if
    else
       type = STRF_NONE
    end if
  end function strfun_chain_get_strfun_type
  function strfun_chain_get_strfun_set(sfchain) result(set)
    type(strfun_chain_t), intent(in) :: sfchain
    integer :: set

    set = 0
    if(size(sfchain%strfun).eq.2) then
       if(sfchain%strfun(1)%type .eq. sfchain%strfun(2)%type) then
          if(sfchain%strfun(1)%type .eq. STRF_LHAPDF) then
             set = lhapdf_data_get_set(sfchain%strfun(1)%lhapdf_data(1))
          else if(sfchain%strfun(1)%type .eq. STRF_PDF_BUILTIN) then
             set = pdf_builtin_get_id(sfchain%strfun(1)%pdf_builtin_data(1))
         end if
      end if
    end if
  end function strfun_chain_get_strfun_set
  function strfun_chain_get_n_strfun (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    n = sfchain%n_strfun
  end function strfun_chain_get_n_strfun

  function strfun_chain_get_n_parameters_tot (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    n = sfchain%n_parameters_tot
  end function strfun_chain_get_n_parameters_tot

  function strfun_chain_get_n_vir (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    if (sfchain%n_strfun /= 0) then
       n = evaluator_get_n_vir (sfchain%eval(sfchain%n_strfun))
    else
       n = 0
    end if
  end function strfun_chain_get_n_vir

  function strfun_chain_multichannel_enabled (sfchain) result (flag)
    logical :: flag
    type(strfun_chain_t), intent(in) :: sfchain
    flag = sfchain%multichannel
  end function strfun_chain_multichannel_enabled

  function strfun_chain_get_mapping_factor (sfchain) result (f)
    real(default) :: f
    type(strfun_chain_t), intent(in) :: sfchain
    f = sfchain%mapping_factor
  end function strfun_chain_get_mapping_factor

  function strfun_chain_dimension_is_rigid (sfchain) result (rigid)
    logical, dimension(:), allocatable :: rigid
    type(strfun_chain_t), intent(in) :: sfchain
    integer :: i, j, k
    allocate (rigid (sfchain%n_parameters_tot))
    k = 0
    do i = 1, size (sfchain%n_parameters)
       do j = 1, sfchain%n_parameters(i)
          k = k + 1
          select case (sfchain%strfun(i)%type)
          case default
             rigid(k) = .false.
          end select
       end do
    end do
  end function strfun_chain_dimension_is_rigid

  function strfun_chain_get_colliding_particles (sfchain) result (index)
    integer, dimension(:), allocatable :: index
    type(strfun_chain_t), intent(in) :: sfchain
    allocate (index (size (sfchain%coll_index)))
    index = sfchain%coll_index
  end function strfun_chain_get_colliding_particles

  function strfun_chain_get_colliding_particles_mask (sfchain) result (mask)
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    type(strfun_chain_t), intent(in), target :: sfchain
    integer :: n_strfun
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_eval
    allocate (mask (size (sfchain%coll_index)))
    n_strfun = sfchain%n_strfun
    if (n_strfun /= 0) then
       allocate (mask_eval (evaluator_get_n_tot (sfchain%eval(n_strfun))))
       mask_eval = evaluator_get_mask (sfchain%eval(n_strfun))
       mask = mask_eval(sfchain%coll_index)
    else
       mask = interaction_get_mask (beam_get_int_ptr (sfchain%beam))
    end if
  end function strfun_chain_get_colliding_particles_mask

  function strfun_chain_get_beam_int_ptr (sfchain) result (int)
    type(interaction_t), pointer :: int
    type(strfun_chain_t), intent(in), target :: sfchain
    int => beam_get_int_ptr (sfchain%beam)
  end function strfun_chain_get_beam_int_ptr
  
  function strfun_chain_get_last_evaluator_ptr (sfchain) result (eval)
    type(evaluator_t), pointer :: eval
    type(strfun_chain_t), intent(in), target :: sfchain
    if (sfchain%n_strfun /= 0) then
       eval => sfchain%eval(sfchain%n_strfun)
    else
       eval => null ()
    end if
  end function strfun_chain_get_last_evaluator_ptr

  subroutine strfun_chain_set_lhapdf &
       (sfchain, i, line, lhapdf_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    call strfun_init (sfchain%strfun(i), lhapdf_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_lhapdf

  subroutine strfun_chain_set_pdf_builtin &
       (sfchain, i, line, pdf_builtin_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(pdf_builtin_data_t), intent(in) :: pdf_builtin_data
    call strfun_init (sfchain%strfun(i), pdf_builtin_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_pdf_builtin

  subroutine strfun_chain_set_isr &
       (sfchain, i, line, isr_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(isr_data_t), intent(in) :: isr_data
    call strfun_init (sfchain%strfun(i), isr_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_isr

  subroutine strfun_chain_set_epa &
       (sfchain, i, line, epa_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(epa_data_t), intent(in) :: epa_data
    call strfun_init (sfchain%strfun(i), epa_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_epa

  subroutine strfun_chain_set_ewa &
       (sfchain, i, line, ewa_data, n_parameters, id)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters, id
    type(ewa_data_t), intent(inout) :: ewa_data
    call strfun_init (sfchain%strfun(i), ewa_data, id)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_ewa

  subroutine strfun_chain_set_circe1 &
       (sfchain, i, line, circe1_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(circe1_data_t), intent(in) :: circe1_data
    call strfun_init (sfchain%strfun(i), circe1_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1, 2/), (/5, 6/))
  end subroutine strfun_chain_set_circe1

  subroutine strfun_chain_set_circe2 &
       (sfchain, i, line, circe2_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(circe2_data_t), intent(in) :: circe2_data
    call strfun_init (sfchain%strfun(i), circe2_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1, 2/), (/3, 4/))
  end subroutine strfun_chain_set_circe2

  subroutine strfun_chain_set_escan &
       (sfchain, i, line, escan_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(escan_data_t), intent(in) :: escan_data
    call strfun_init (sfchain%strfun(i), escan_data)
    sfchain%n_parameters(i) = n_parameters
    if (line == 0) then
       call strfun_chain_link (sfchain, i, line, (/1, 2/), (/3, 4/))
    else
       call strfun_chain_link (sfchain, i, line, (/1/), (/2/))
    end if
  end subroutine strfun_chain_set_escan

  subroutine strfun_chain_set_beam_events &
       (sfchain, i, line, beam_events_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(beam_events_data_t), intent(in) :: beam_events_data
    call strfun_init (sfchain%strfun(i), beam_events_data)
    sfchain%n_parameters(i) = n_parameters
    if (line == 0) then
       call strfun_chain_link (sfchain, i, line, (/1, 2/), (/3, 4/))
    else
       call strfun_chain_link (sfchain, i, line, (/1/), (/2/))
    end if
  end subroutine strfun_chain_set_beam_events

  subroutine strfun_chain_set_user &
       (sfchain, i, line, user_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(sf_user_data_t), intent(in) :: user_data
    integer :: n_tot
    call strfun_init (sfchain%strfun(i), user_data)
    n_tot = sf_user_data_get_n_tot (user_data)
    sfchain%n_parameters(i) = n_parameters
    if (line == 0) then
       call strfun_chain_link (sfchain, i, line, (/1, 2/), (/n_tot-1, n_tot/))
    else
       call strfun_chain_link (sfchain, i, line, (/1/), (/n_tot/))
    end if
  end subroutine strfun_chain_set_user

  subroutine strfun_chain_link (sfchain, i, line, in_index, out_index)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line
    integer, dimension(:), intent(in) :: in_index, out_index
    select case (line)
    case (0)
       call link_single (1, in_index(1))
       call link_single (2, in_index(2))
       sfchain%last_strfun = i
       sfchain%out_index = out_index
    case default
       call link_single (line, in_index(1))
       sfchain%last_strfun(line) = i
       sfchain%out_index(line) = out_index(1)
    end select
  contains
    subroutine link_single (line, in_index)
      integer, intent(in) :: line, in_index
      integer :: j
      j = sfchain%last_strfun(line)
      select case (j)
      case (0)
         call interaction_set_source_link &
              (sfchain%strfun(i)%int, in_index, &
               sfchain%beam, sfchain%out_index(line))
      case default
         call interaction_set_source_link &
              (sfchain%strfun(i)%int, in_index, &
               sfchain%strfun(j)%int, sfchain%out_index(line))
      end select
    end subroutine link_single
  end subroutine strfun_chain_link

  subroutine strfun_chain_set_mapping (sfchain, i, ch, index, type, par)
    type(strfun_chain_t), intent(inout) :: sfchain
    integer, intent(in) :: i, ch
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: type
    real(default), dimension(:), intent(in) :: par
    call strfun_mapping_init (sfchain%sf_mapping(i, ch), index, type, par)
  end subroutine strfun_chain_set_mapping

  subroutine strfun_chain_make_evaluators (sfchain, ok)
    type(strfun_chain_t), intent(inout), target :: sfchain
    logical, intent(out), optional :: ok
    type(interaction_t), pointer :: beam_int, eval_int, sf_int, eval_int_next
    type(quantum_numbers_mask_t) :: qn_mask_conn
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask_beam
    integer :: i, j, last, out_index, coll_index
    sfchain%n_parameters_tot = sum (sfchain%n_parameters)
    beam_int => beam_get_int_ptr (sfchain%beam)
    if (.not. associated (beam_int))  call msg_bug &
         ("strfun_chain_make_evaluators: null beam pointer")
    allocate (qn_mask_beam (interaction_get_n_out (beam_int)))
    qn_mask_beam = interaction_get_mask (beam_int)
    call interaction_exchange_mask (beam_int)
    do i = 1, size (sfchain%strfun) - 1
       call interaction_exchange_mask (sfchain%strfun(i)%int)
    end do
    do i = size (sfchain%strfun), 1, -1
       call interaction_exchange_mask (sfchain%strfun(i)%int)
    end do
    if (any (qn_mask_beam .neqv. interaction_get_mask (beam_int))) then
       call beam_write (sfchain%beam)
       call msg_fatal (" Beam polarization/color/flavor incompatible with structure functions")
    end if
    eval_int => beam_int
    do i = 1, size (sfchain%strfun)
       qn_mask_conn = new_quantum_numbers_mask (.false., .false., .true.)
       call evaluator_init_product (sfchain%eval(i), eval_int, &
            sfchain%strfun(i)%int, qn_mask_conn)
       if (evaluator_is_empty (sfchain%eval(i))) then
          call msg_fatal ("Mismatch in beam and structure-function chain")
          if (present (ok))  ok = .false.
          return
       end if
       eval_int => evaluator_get_int_ptr (sfchain%eval(i))
    end do
    if (size (sfchain%strfun) /= 0) then    
       do j = 1, size (sfchain%coll_index)
          last = sfchain%last_strfun(j)
          select case (last)
          case (0)
             eval_int => beam_get_int_ptr (sfchain%beam)
             out_index = sfchain%out_index(j)
             coll_index = out_index
          case default
             sf_int => sfchain%strfun(last)%int
             eval_int => evaluator_get_int_ptr (sfchain%eval(last))
             out_index = sfchain%out_index(j)
             coll_index = interaction_find_link (eval_int, sf_int, out_index)
          end select
          if (coll_index /= 0) then
             do i = last + 1, size (sfchain%strfun)
                out_index = coll_index
                eval_int_next => evaluator_get_int_ptr (sfchain%eval(i))
                coll_index = &
                     interaction_find_link (eval_int_next, eval_int, out_index)
                if (coll_index == 0)  call msg_bug ("Structure functions: " &
                     // "broken links in structure function chain")
                eval_int => eval_int_next
             end do
          end if
          if (coll_index /= 0) then
             sfchain%coll_index(j) =  coll_index
          else
             call msg_bug ("Structure functions: " &
                  // "colliding particles can't be determined")
          end if
       end do
    end if
    if (present (ok))  ok = .true.
  end subroutine strfun_chain_make_evaluators

  subroutine strfun_chain_set_kinematics (sfchain, r, &
       channel, offset, r_all, sf_factor, ok)
    type(strfun_chain_t), intent(inout) :: sfchain
    real(default), dimension(:), intent(in) :: r
    integer, intent(in), optional :: channel, offset
    real(default), dimension(:,:), intent(inout), optional :: r_all
    real(default), dimension(:), intent(out), optional :: sf_factor
    logical, intent(out), optional :: ok
    real(default), dimension(size(r)) :: x
    integer :: n_mapping
    integer :: i, i1, i2, ch, n, n1, n_sf
    real(default) :: xprod, factor
    real(default), dimension(:), allocatable :: factor_channel
    integer, dimension(:), allocatable :: mapping_type
    n_sf = size (sfchain%strfun)
    sfchain%mapping_factor = 1
    if (size (r) == sfchain%n_parameters_tot) then
       x = r
       if (allocated (sfchain%sf_mapping)) then
          n_mapping = size (sfchain%sf_mapping, 1)
          if (present (channel)) then
             allocate (factor_channel (n_mapping))
             allocate (mapping_type (n_mapping))
             do i = 1, n_mapping
                mapping_type(i) = sfchain%sf_mapping(i,channel)%type
                call strfun_mapping_apply &
                     (sfchain%sf_mapping(i, channel), x, factor_channel(i))
             end do
             sf_factor(channel) = product (factor_channel)
          else
             do i = 1, n_mapping
                call strfun_mapping_apply &
                     (sfchain%sf_mapping(i, 1), x, factor)
                sfchain%mapping_factor = sfchain%mapping_factor * factor
             end do
          end if
       end if
       n = 0
       do i = 1, size (sfchain%strfun)
          call interaction_receive_momenta (sfchain%strfun(i)%int)
          n1 = sfchain%n_parameters(i)
          call strfun_set_kinematics (sfchain%strfun(i), x(n+1:n+n1), .false.)
          n = n + n1
       end do
       if (present (channel)) then
          i1 = offset
          i2 = offset + size (r)
          do ch = 1, size (r_all, 2)
             if (ch /= channel) then
                sf_factor(ch) = 1
                do i = 1, n_mapping
                   if (sfchain%sf_mapping(i,ch)%type == mapping_type(i)) then
                      r_all(i1+1:i2,ch) = r
                      sf_factor(ch) = sf_factor(ch) * factor_channel(i)
                   else
                      r_all(i1+1:i2,ch) = x
                      call strfun_mapping_apply_inverse &
                           (sfchain%sf_mapping(1,ch), r_all(i1+1:i2,ch), &
                           factor)
                      sf_factor(ch) = sf_factor(ch) * factor
                   end if
                end do
             end if
          end do
       end if
       do i = 1, size (sfchain%strfun)
          call evaluator_receive_momenta (sfchain%eval(i))
       end do
       if (present (ok))  ok = .true.
    else
       call msg_bug ("Structure functions: mismatch in number of parameters")
    end if
  end subroutine strfun_chain_set_kinematics

  subroutine strfun_chain_evaluate (sfchain, scale)
    type(strfun_chain_t), intent(inout) :: sfchain
    real(default), intent(in) :: scale
    integer :: i
    do i = size (sfchain%strfun), 1, -1
       call strfun_apply (sfchain%strfun(i), scale)
    end do
    do i = 1, size (sfchain%eval)
       call evaluator_evaluate (sfchain%eval(i))
    end do
  end subroutine strfun_chain_evaluate

  subroutine strfun_test (lhapdf_present)
    use os_interface, only: os_data_t
    type(os_data_t) :: os_data
    type(model_t), pointer :: model
    logical, intent(in) :: lhapdf_present
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)
    call syntax_model_file_final ()
    print *, "***********************************************************"
    call isr_test (model)
    print *, "***********************************************************"
    call epa_test (model)
    if (lhapdf_present) then
      print *, "***********************************************************"
      call lhapdf_test (model)
    end if
  end subroutine strfun_test

  subroutine isr_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t), target :: beam_data
    type(isr_data_t), dimension(2) :: isr_data
    type(strfun_chain_t), target :: sfchain
    integer :: i
    print *, "*** ISR test"
    call flavor_init (flv, (/11, -11/), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 500._default, flv, pol)
    do i = 1, 2
       call isr_data_init (isr_data(i), &
            model, flv(i), 0.06_default, 500._default, 0.511e-3_default)
    end do
    call strfun_chain_init (sfchain, beam_data, 2)
    call strfun_chain_set_strfun (sfchain, 1, 1, isr_data(1), 1)
    call strfun_chain_set_strfun (sfchain, 2, 2, isr_data(2), 3)
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics &
         (sfchain, (/0.8_default, 0.4_default, 0.5_default, 0.2_default/))
    call strfun_chain_evaluate (sfchain, 0._default)
    call strfun_chain_write (sfchain)
    call strfun_chain_final (sfchain)
  end subroutine isr_test

  subroutine epa_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t) :: beam_data
    type(epa_data_t) :: epa_data1
    type(epa_data_t) :: epa_data2
    type(strfun_chain_t), target :: sfchain
    print *, "*** EPA test"
    call flavor_init (flv, (/2, 1/), model)
    ! Prepare beams
    call polarization_init_circular (pol(1), flv(1), 0.3_default)
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 1000._default, flv, pol)
    call strfun_chain_init (sfchain, beam_data, 2)
    ! Initialize EPA for both
    call epa_data_init (epa_data1, model, &
         flv(1), 0.06_default, 1.e-6_default, 0._default, 500._default, &
         511.e-6_default)
    call epa_data_init (epa_data2, model, &
         flv(2), 0.06_default, 1.e-6_default, 1._default, 500._default)
    call strfun_chain_set_strfun (sfchain, 1, 1, epa_data1, 1)
    call strfun_chain_set_strfun (sfchain, 2, 2, epa_data2, 3)
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics &
         (sfchain, (/0.8_default, 0.4_default, 0.5_default, 0.2_default/))
    call strfun_chain_evaluate (sfchain, 0._default)
    call strfun_chain_write (sfchain)
    ! Clean up
    call beam_data_final (beam_data)
    call polarization_final (pol)
    call strfun_chain_final (sfchain)
  end subroutine epa_test

  subroutine lhapdf_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(beam_data_t) :: beam_data
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(lhapdf_data_t), dimension(2) :: data
    type(lhapdf_status_t) :: lhapdf_status
    type(strfun_chain_t), target :: sfchain
    real(default) :: scale
    print *, "*** LHAPDF test"
    call flavor_init (flv, (/ -PROTON, PHOTON /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 2000._default, flv, pol)
    call strfun_chain_init (sfchain, beam_data, 2)
    call lhapdf_data_init (data(1), lhapdf_status, model, flv(1), member=1)
    !!! Use the same photon PDF that is demanded by the LHAPDF tests.
    call lhapdf_data_init (data(2), lhapdf_status, model, flv(2), &
         file=var_str("GSG961.LHgrid"), photon_scheme=1)
    call lhapdf_data_set_mask (data(2), &
         (/.false.,.false.,.false., .true., .true., .true., &
           .false., &
           .true., .true., .true., .false., .false., .false. /))
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_set_strfun (sfchain, 1, 1, data(1), 1) 
    call strfun_chain_set_strfun (sfchain, 2, 2, data(2), 1)
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics (sfchain, (/0.9_default, 0.4_default/))
    scale = 1.e3_default
    call strfun_chain_evaluate (sfchain, scale)
    call strfun_chain_write (sfchain)
    call strfun_chain_final (sfchain)
  end subroutine lhapdf_test


end module strfun
