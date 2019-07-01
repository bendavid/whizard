! WHIZARD 2.2.7 Aug 11 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module sf_isr
 
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: pi
  use format_defs, only: FMT_17, FMT_19
  use unit_tests, only: vanishes
  use diagnostics
  use physics_defs, only: PHOTON
  use lorentz
  use sm_physics, only: Li2
  use pdg_arrays
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use sf_mappings
  use sf_base

  implicit none
  private

  public :: isr_data_t
  public :: isr_t

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_MASS = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: EPS_TOO_LARGE = 3
  integer, parameter :: INVALID_ORDER = 4
  integer, parameter :: CHARGE_MIX = 5
  integer, parameter :: CHARGE_ZERO = 6
  integer, parameter :: MASS_MIX = 7

  type, extends (sf_data_t) :: isr_data_t
     private
     class(model_data_t), pointer :: model => null ()
     type(flavor_t), dimension(:), allocatable :: flv_in
     real(default) :: alpha = 0
     real(default) :: q_max = 0
     real(default) :: real_mass = 0
     real(default) :: mass = 0
     real(default) :: eps = 0
     real(default) :: log = 0
     logical :: recoil = .false.
     integer :: order = 3
     integer :: error = NONE
   contains
     procedure :: init => isr_data_init
     procedure :: set_order => isr_data_set_order
     procedure :: check => isr_data_check
     procedure :: write => isr_data_write
     procedure :: get_n_par => isr_data_get_n_par
     procedure :: get_pdg_out => isr_data_get_pdg_out
     procedure :: get_eps => isr_data_get_eps
     procedure :: allocate_sf_int => isr_data_allocate_sf_int  
  end type isr_data_t

  type, extends (sf_int_t) :: isr_t
     private
     type(isr_data_t), pointer :: data => null ()
     real(default) :: x = 0
     real(default) :: xb= 0
   contains
     procedure :: type_string => isr_type_string
     procedure :: write => isr_write
     procedure :: set_order => isr_set_order
     procedure :: complete_kinematics => isr_complete_kinematics
     procedure :: recover_x => sf_isr_recover_x
     procedure :: inverse_kinematics => isr_inverse_kinematics
     procedure :: init => isr_init
     procedure :: apply => isr_apply
  end type isr_t 
  

contains

  subroutine isr_data_init &
       (data, model, pdg_in, alpha, q_max, mass, order, recoil)
    class(isr_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    real(default), intent(in) :: alpha
    real(default), intent(in) :: q_max
    real(default), intent(in), optional :: mass
    integer, intent(in), optional :: order
    logical, intent(in), optional :: recoil
    integer :: i, n_flv
    real(default) :: charge
    data%model => model
    n_flv = pdg_array_get_length (pdg_in)
    allocate (data%flv_in (n_flv))
    do i = 1, n_flv
       call data%flv_in(i)%init (pdg_array_get (pdg_in, i), model)
    end do
    data%alpha = alpha
    data%q_max = q_max
    if (present (order)) then
       call data%set_order (order)
    end if
    if (present (recoil)) then
       data%recoil = recoil
    end if
    data%real_mass = data%flv_in(1)%get_mass ()
    if (present (mass)) then
       if (mass > 0) then
          data%mass = mass
       else
          data%mass = data%real_mass
          if (any (data%flv_in%get_mass () /= data%mass)) then
             data%error = MASS_MIX;  return
          end if
       end if
    else
       data%mass = data%real_mass
       if (any (data%flv_in%get_mass () /= data%mass)) then
          data%error = MASS_MIX;  return
       end if
    end if
    if (vanishes (data%mass)) then
       data%error = ZERO_MASS;  return
    else if (data%mass >= data%q_max) then
       data%error = Q_MAX_TOO_SMALL;  return
    end if
    data%log = log (1 + (data%q_max / data%mass)**2)
    charge = data%flv_in(1)%get_charge ()
    if (any (abs (data%flv_in%get_charge ()) /= abs (charge))) then
       data%error = CHARGE_MIX;  return
    else if (charge == 0) then
       data%error = CHARGE_ZERO;  return
    end if
    data%eps = data%alpha / pi * charge ** 2 &
         * (2 * log (data%q_max / data%mass) - 1)
    if (data%eps > 1) then
       data%error = EPS_TOO_LARGE;  return
    end if
  end subroutine isr_data_init

  elemental subroutine isr_data_set_order (data, order)
    class(isr_data_t), intent(inout) :: data
    integer, intent(in) :: order
    if (order < 0 .or. order > 3) then
       data%error = INVALID_ORDER
    else
       data%order = order
    end if
  end subroutine isr_data_set_order

  subroutine isr_data_check (data)
    class(isr_data_t), intent(in) :: data
    select case (data%error)
    case (ZERO_MASS)
       call msg_fatal ("ISR: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal ("ISR: Particle mass exceeds Qmax")
    case (EPS_TOO_LARGE)
       call msg_fatal ("ISR: Expansion parameter too large, " // &
            "perturbative expansion breaks down")
    case (INVALID_ORDER)
       call msg_error ("ISR: LLA order invalid (valid values are 0,1,2,3)")
    case (MASS_MIX)
       call msg_fatal ("ISR: Incoming particle masses must be uniform")
    case (CHARGE_MIX)
       call msg_fatal ("ISR: Incoming particle charges must be uniform")
    case (CHARGE_ZERO)
       call msg_fatal ("ISR: Incoming particle must be charged")
    end select
  end subroutine isr_data_check

  subroutine isr_data_write (data, unit, verbose) 
    class(isr_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "ISR data:"
    if (allocated (data%flv_in)) then
       write (u, "(3x,A)", advance="no") "  flavor =  "
       do i = 1, size (data%flv_in)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          call data%flv_in(i)%write (u)
       end do
       write (u, *)    
       write (u, "(3x,A," // FMT_19 // ")") "  alpha  = ", data%alpha
       write (u, "(3x,A," // FMT_19 // ")") "  q_max  = ", data%q_max
       write (u, "(3x,A," // FMT_19 // ")") "  mass   = ", data%mass
       write (u, "(3x,A," // FMT_19 // ")") "  eps    = ", data%eps
       write (u, "(3x,A," // FMT_19 // ")") "  log    = ", data%log
       write (u, "(3x,A,I2)")      "  order  = ", data%order
       write (u, "(3x,A,L2)")      "  recoil = ", data%recoil
    else
       write (u, "(3x,A)") "[undefined]"       
    end if
  end subroutine isr_data_write

  function isr_data_get_n_par (data) result (n)
    class(isr_data_t), intent(in) :: data
    integer :: n
    if (data%recoil) then
       n = 3
    else
       n = 1
    end if
  end function isr_data_get_n_par
  
  subroutine isr_data_get_pdg_out (data, pdg_out)
    class(isr_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    pdg_out(1) = data%flv_in%get_pdg ()
  end subroutine isr_data_get_pdg_out
  
  function isr_data_get_eps (data) result (eps)
    class(isr_data_t), intent(in) :: data
    real(default) :: eps
    eps = data%eps
  end function isr_data_get_eps

  subroutine isr_data_allocate_sf_int (data, sf_int)
    class(isr_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (isr_t :: sf_int)
  end subroutine isr_data_allocate_sf_int
  
  function isr_type_string (object) result (string)
    class(isr_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "ISR: e+ e- ISR spectrum" 
    else
       string = "ISR: [undefined]"
    end if
  end function isr_type_string
  
  subroutine isr_write (object, unit, testflag)
    class(isr_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       if (object%status >= SF_DONE_KINEMATICS) then
          write (u, "(1x,A)")  "SF parameters:"
          write (u, "(3x,A," // FMT_17 // ")")  "x =", object%x
          write (u, "(3x,A," // FMT_17 // ")")  "xb=", object%xb
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "ISR data: [undefined]"
    end if
  end subroutine isr_write
    
  subroutine isr_set_order (object, order)
    class(isr_t), intent(inout) :: object
    integer, intent(in) :: order
    call object%data%set_order (order)
  end subroutine isr_set_order

  subroutine isr_complete_kinematics (sf_int, x, f, r, rb, map)
    class(isr_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: eps
    eps = sf_int%data%eps
    if (map) then
       call map_power_1 (sf_int%xb, f, rb(1), eps)
    else
       sf_int%xb = rb(1)
       if (rb(1) > 0) then
          f = 1
       else
          f = 0
       end if
    end if
    sf_int%x = 1 - sf_int%xb
    x(1) = sf_int%x
    if (size (x) == 3)  x(2:3) = r(2:3)
    call sf_int%split_momentum (x, sf_int%xb)
    select case (sf_int%status)
    case (SF_FAILED_KINEMATICS)
       sf_int%x = 0
       sf_int%xb= 0
       f = 0
    end select
  end subroutine isr_complete_kinematics

  subroutine sf_isr_recover_x (sf_int, x, x_free)
    class(isr_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(inout), optional :: x_free
    call sf_int%base_recover_x (x, x_free)
    sf_int%x  = x(1)
    sf_int%xb = 1 - x(1)
  end subroutine sf_isr_recover_x
  
  subroutine isr_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(isr_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: eps
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    eps = sf_int%data%eps
    if (map) then
       call map_power_inverse_1 (sf_int%xb, f, rb(1), eps)
    else
       rb(1) = sf_int%xb
       if (rb(1) > 0) then
          f = 1
       else
          f = 0
       end if
    end if
    r(1) = 1 - rb(1)
    if (size(r) == 3) then
       r(2:3) = x(2:3)
       rb(2:3)= 1 - r(2:3)
    end if
    if (set_mom) then
       call sf_int%split_momentum (x, sf_int%xb)
       select case (sf_int%status)
       case (SF_FAILED_KINEMATICS)
          r = 0
          rb= 0
          f = 0
       end select
    end if
  end subroutine isr_inverse_kinematics

  subroutine isr_init (sf_int, data)
    class(isr_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: hel_lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel
    type(flavor_t) :: flv_photon
    type(color_t) :: col_photon
    type(quantum_numbers_t) :: qn_photon, qn
    type(state_iterator_t) :: it_hel
    real(default) :: m2
    integer :: i
    mask = quantum_numbers_mask (.false., .false., &
         mask_h = [.false., .true., .false.])
    hel_lock = [3, 0, 1]
    select type (data)
    type is (isr_data_t)   
       m2 = data%mass**2
       call sf_int%base_init (mask, [m2], [0._default], [m2], &
            hel_lock = hel_lock)
       sf_int%data => data              
       call flv_photon%init (PHOTON, data%model)
       call col_photon%init ()
       call qn_photon%init (flv_photon, col_photon)
       call qn_photon%tag_radiated ()
       do i = 1, size (data%flv_in)
          call polarization_init_generic (pol, data%flv_in(i))
          call qn_fc(1)%init (&
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i), 1))
          call it_hel%init (pol%state)
          do while (it_hel%is_valid ())
             qn_hel = it_hel%get_quantum_numbers ()
             qn = qn_hel(1) .merge. qn_fc(1)
             call sf_int%add_state ([qn, qn_photon, qn])
             call it_hel%advance ()
          end do
          call polarization_final (pol)
       end do
       call sf_int%freeze ()
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
       sf_int%status = SF_INITIAL
    end select
  end subroutine isr_init
    
  subroutine isr_apply (sf_int, scale)
    class(isr_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: f, finv, x, xb, eps, rb
    real(default) :: log_x, log_xb, x_2
    real(default), parameter :: &
         & xmin = 0.00714053329734592839549879772019_default
    real(default), parameter :: &
         & zeta3 = 1.20205690315959428539973816151_default
    real(default), parameter :: &
         & g1 = 3._default / 4._default, &
         & g2 = (27 - 8*pi**2) / 96._default, &
         & g3 = (27 - 24*pi**2 + 128*zeta3) / 384._default    
    associate (data => sf_int%data)
      eps = sf_int%data%eps
      x = sf_int%x
      xb = sf_int%xb
      call map_power_inverse_1 (xb, finv, rb, eps)
      if (finv > 0) then
         f = 1 / finv
      else
         f = 0
      end if   
      if (f > 0 .and. data%order > 0) then
         f = f * (1 + g1 * eps)
         x_2 = x*x
         if (rb>0)  f = f * (1 - (1-x_2) / (2 * rb))
         if (data%order > 1) then
            f = f * (1 + g2 * eps**2)
            if (rb>0 .and. xb>0 .and. x>xmin) then
               log_x  = log_prec (x, xb)
               log_xb = log_prec (xb, x)
               f = f * (1 - ((1+3*x_2)*log_x + xb * (4*(1+x)*log_xb + 5 + x)) &
                    / ( 8 * rb) * eps)
            end if
            if (data%order > 2) then
               f = f * (1 + g3 * eps**3)
               if (rb > 0 .and. xb > 0 .and. x > xmin) then
                  f = f * (1 - ((1+x) * xb &
                       * (6 * Li2(x) + 12 * log_xb**2 - 3 * pi**2) &
                       + 1.5_default * (1 + 8*x + 3*x_2) * log_x &
                       + 6 * (x+5) * xb * log_xb &
                       + 12 * (1+x_2) * log_x * log_xb &
                       - (1 + 7*x_2) * log_x**2 / 2 &
                       + (39 - 24*x - 15*x_2) / 4) &
                       / ( 48 * rb) * eps**2)
               end if
            end if
         end if
      end if
    end associate
    call sf_int%set_matrix_element (cmplx (f, kind=default))    
    sf_int%status = SF_EVALUATED
  end subroutine isr_apply


end module sf_isr
