! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use format_defs, only: FMT_12, FMT_17, FMT_19
  use unit_tests
  use diagnostics
  use physics_defs, only: ELECTRON, PHOTON
  use lorentz
  use sm_physics, only: Li2
  use pdg_arrays
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use interactions
  use polarizations
  use sf_mappings
  use sf_aux
  use sf_base

  implicit none
  private

  public :: isr_data_t
  public :: sf_isr_test

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
     type(isr_data_t), pointer :: data => null ()
     real(default) :: x = 0
     real(default) :: xb= 0
   contains
     procedure :: type_string => isr_type_string
     procedure :: write => isr_write
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
       call flavor_init (data%flv_in(i), pdg_array_get (pdg_in, i), model)
    end do
    data%alpha = alpha
    data%q_max = q_max
    if (present (order)) then
       call data%set_order (order)
    end if
    if (present (recoil)) then
       data%recoil = recoil
    end if
    data%real_mass = flavor_get_mass (data%flv_in(1))
    if (present (mass)) then
       if (mass > 0) then
          data%mass = mass
       else
          data%mass = data%real_mass
          if (any (flavor_get_mass (data%flv_in) /= data%mass)) then
             data%error = MASS_MIX;  return
          end if
       end if
    else
       data%mass = data%real_mass
       if (any (flavor_get_mass (data%flv_in) /= data%mass)) then
          data%error = MASS_MIX;  return
       end if
    end if
    if (data%mass == 0) then
       data%error = ZERO_MASS;  return
    else if (data%mass >= data%q_max) then
       data%error = Q_MAX_TOO_SMALL;  return
    end if
    data%log = log (1 + (data%q_max / data%mass)**2)
    charge = flavor_get_charge (data%flv_in(1))
    if (any (abs (flavor_get_charge (data%flv_in)) /= abs (charge))) then
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
          call flavor_write (data%flv_in(i), u)
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
    pdg_out(1) = flavor_get_pdg (data%flv_in)
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
    type(quantum_numbers_t) :: qn_photon, qn
    type(state_iterator_t) :: it_hel
    real(default) :: m2
    integer :: i
    mask = new_quantum_numbers_mask (.false., .false., &
         mask_h = [.false., .true., .false.])
    hel_lock = [3, 0, 1]
    select type (data)
    type is (isr_data_t)   
       m2 = data%mass**2
       call sf_int%base_init (mask, [m2], [0._default], [m2], &
            hel_lock = hel_lock)
       sf_int%data => data              
       call flavor_init (flv_photon, PHOTON, data%model)
       call quantum_numbers_init (qn_photon, flv_photon)
       do i = 1, size (data%flv_in)
          call polarization_init_generic (pol, data%flv_in(i))
          call quantum_numbers_init (qn_fc(1), &
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i), 1))
          call state_iterator_init (it_hel, pol%state)
          do while (state_iterator_is_valid (it_hel))
             qn_hel = state_iterator_get_quantum_numbers (it_hel)
             qn = qn_hel(1) .merge. qn_fc(1)
             call interaction_add_state &
                  (sf_int%interaction_t, [qn, qn_photon, qn])
             call state_iterator_advance (it_hel)
          end do
          call polarization_final (pol)
       end do
       call interaction_freeze (sf_int%interaction_t)
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
    call interaction_set_matrix_element &
           (sf_int%interaction_t, cmplx (f, kind=default))    
    sf_int%status = SF_EVALUATED
  end subroutine isr_apply


  subroutine sf_isr_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_isr_1, "sf_isr_1", &
         "structure function configuration", &
         u, results)
    call test (sf_isr_2, "sf_isr_2", &
         "no ISR mapping", &
         u, results)
    call test (sf_isr_3, "sf_isr_3", &
         "ISR mapping", &
         u, results)
    call test (sf_isr_4, "sf_isr_4", &
         "ISR non-collinear", &
         u, results)
    call test (sf_isr_5, "sf_isr_5", &
         "ISR pair mapping", &
         u, results)
  end subroutine sf_isr_test
  
  subroutine sf_isr_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_isr_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call model%init_qed_test ()
    pdg_in = ELECTRON

    allocate (isr_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    select type (data)
    type is (isr_data_t)
       call data%init (model, pdg_in, 1./137._default, 10._default, &
            0.000511_default, order = 3, recoil = .false.)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(2x,99(1x,I0))")  pdg1
        
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_isr_1"

  end subroutine sf_isr_1

  subroutine sf_isr_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, f_isr
    
    write (u, "(A)")  "* Test output: sf_isr_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    pdg_in = ELECTRON
    call flavor_init (flv, ELECTRON, model)

    call reset_interaction_counter ()
    
    allocate (isr_data_t :: data)
    select type (data)
    type is (isr_data_t)
       call data%init (model, pdg_in, 1./137._default, 500._default, &
            0.000511_default, order = 3, recoil = .false.)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.9, no ISR mapping, &
         &collinear"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = 0.9_default
    rb = 1 - r
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb

    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    
    write (u, "(A)")
    write (u, "(A,9(1x," // FMT_12 // "))")  "x =", x
    write (u, "(A,9(1x," // FMT_12 // "))")  "f =", f
    
    write (u, "(A)")
    write (u, "(A)")  "* Invert kinematics"
    write (u, "(A)")
    
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.) 
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb
    write (u, "(A,9(1x," // FMT_12 // "))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate ISR structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure-function value, default order"
    write (u, "(A)")

    f_isr = interaction_get_matrix_element (sf_int%interaction_t, 1)
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", f_isr
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", f_isr * f

    write (u, "(A)")
    write (u, "(A)")  "* Re-evaluate structure function, leading order"
    write (u, "(A)")
    
    select type (sf_int)
    type is (isr_t)
       sf_int%data%order = 0
    end select
    call sf_int%apply (scale = 100._default)
    f_isr = interaction_get_matrix_element (sf_int%interaction_t, 1)
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", f_isr
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", f_isr * f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_isr_2"

  end subroutine sf_isr_2

  subroutine sf_isr_3 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, f_isr
    
    write (u, "(A)")  "* Test output: sf_isr_3"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()
    
    allocate (isr_data_t :: data)
    select type (data)
    type is (isr_data_t)
       call data%init (model, pdg_in, 1./137._default, 500._default, &
            0.000511_default, order = 3, recoil = .false.)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.7, with ISR mapping, &
         &collinear"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = 0.7_default
    rb = 1 - r
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb

    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    
    write (u, "(A)")
    write (u, "(A,9(1x," // FMT_12 // "))")  "x =", x
    write (u, "(A,9(1x," // FMT_12 // "))")  "f =", f
    
    write (u, "(A)")
    write (u, "(A)")  "* Invert kinematics"
    write (u, "(A)")
    
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true.) 
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb
    write (u, "(A,9(1x," // FMT_12 // "))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate ISR structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure-function value, default order"
    write (u, "(A)")

    f_isr = interaction_get_matrix_element (sf_int%interaction_t, 1)
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", f_isr
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", f_isr * f

    write (u, "(A)")
    write (u, "(A)")  "* Re-evaluate structure function, leading order"
    write (u, "(A)")
    
    select type (sf_int)
    type is (isr_t)
       sf_int%data%order = 0
    end select
    call sf_int%apply (scale = 100._default)
    f_isr = interaction_get_matrix_element (sf_int%interaction_t, 1)
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", f_isr
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", f_isr * f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_isr_3"

  end subroutine sf_isr_3

  subroutine sf_isr_4 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, f_isr
    character(len=80) :: buffer
    integer :: u_scratch, iostat
    
    write (u, "(A)")  "* Test output: sf_isr_4"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")

    allocate (isr_data_t :: data)
    select type (data)
    type is (isr_data_t)
       call data%init (model, pdg_in, 1./137._default, 500._default, &
            0.000511_default, order = 3, recoil = .true.)
    end select    
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
        
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.25, with ISR mapping, "
    write (u, "(A)")  "          non-coll., keeping energy"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = [0.5_default, 0.5_default, 0.25_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    call interaction_pacify_momenta (sf_int%interaction_t, 1e-10_default)
    
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    
    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r from momenta"
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
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true.)    
    
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate ISR structure function"
    write (u, "(A)")
    
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.) 
    call interaction_pacify_momenta (sf_int%interaction_t, 1e-10_default)    
    call sf_int%apply (scale = 10._default)
    u_scratch = free_unit ()
    open (u_scratch, status="scratch", action = "readwrite")
    call sf_int%write (u_scratch, testflag = .true.)
    rewind (u_scratch)
    do 
       read (u_scratch, "(A)", iostat=iostat) buffer    
       if (iostat /= 0) exit
       if (buffer(1:27) == " P =   0.00000000E+00  9.57") then
          buffer = replace (buffer, 28, "XXXXXX")
       end if
       if (buffer(1:27) == " P =   0.00000000E+00 -9.57") then
          buffer = replace (buffer, 28, "XXXXXX")
       end if       
       write (u, "(A)") buffer
    end do
    close (u_scratch)
       
    write (u, "(A)")
    write (u, "(A)")  "* Structure-function value"
    write (u, "(A)")

    f_isr = interaction_get_matrix_element (sf_int%interaction_t, 1)
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", f_isr
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", f_isr * f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_isr_4"

  end subroutine sf_isr_4

  subroutine sf_isr_5 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_mapping_t), allocatable :: mapping
    class(sf_int_t), dimension(:), allocatable :: sf_int
    type(vector4_t), dimension(2) :: k
    real(default) :: E, f_map
    real(default), dimension(:), allocatable :: p, pb, r, rb, x
    real(default), dimension(2) :: f, f_isr
    integer :: i
    
    write (u, "(A)")  "* Test output: sf_isr_5"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()
    
    allocate (isr_data_t :: data)
    select type (data)
    type is (isr_data_t)
       call data%init (model, pdg_in, 1./137._default, 500._default, &
            0.000511_default, order = 3, recoil = .false.)
    end select
       
    allocate (sf_ip_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ip_mapping_t)
       select type (data)
       type is (isr_data_t)
          call mapping%init (eps = data%eps)
       end select
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select

    call mapping%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")

    allocate (isr_t :: sf_int (2))

    do i = 1, 2
       call sf_int(i)%init (data)
       call sf_int(i)%set_beam_index ([i])
    end do

    write (u, "(A)")  "* Initialize incoming momenta with E=500"
    write (u, "(A)")
    E = 500
    k(1) = vector4_moving (E,   sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    k(2) = vector4_moving (E, - sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    do i = 1, 2
       call vector4_write (k(i), u)
       call sf_int(i)%seed_kinematics (k(i:i))
    end do
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for p=[0.7,0.4], collinear"
    write (u, "(A)")
    
    allocate (p (2 * data%get_n_par ()))
    allocate (pb(size (p)))
    allocate (r (size (p)))
    allocate (rb(size (p)))
    allocate (x (size (p)))
    
    p = [0.7_default, 0.4_default]
    pb= 1 - p
    call mapping%compute (r, rb, f_map, p, pb)

    write (u, "(A,9(1x," // FMT_12 // "))")  "p =", p
    write (u, "(A,9(1x," // FMT_12 // "))")  "pb=", pb
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb
    write (u, "(A,9(1x," // FMT_12 // "))")  "fm=", f_map

    do i = 1, 2
       call sf_int(i)%complete_kinematics (x(i:i), f(i), r(i:i), rb(i:i), &
            map=.false.)
    end do
    
    write (u, "(A)")
    write (u, "(A,9(1x," // FMT_12 // "))")  "x =", x
    write (u, "(A,9(1x," // FMT_12 // "))")  "f =", f
    
    write (u, "(A)")
    write (u, "(A)")  "* Invert kinematics"
    write (u, "(A)")
    
    do i = 1, 2
       call sf_int(i)%inverse_kinematics (x(i:i), f(i), r(i:i), rb(i:i), &
            map=.false.) 
    end do
    call mapping%inverse (r, rb, f_map, p, pb)

    write (u, "(A,9(1x," // FMT_12 // "))")  "p =", p
    write (u, "(A,9(1x," // FMT_12 // "))")  "pb=", pb
    write (u, "(A,9(1x," // FMT_12 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_12 // "))")  "rb=", rb
    write (u, "(A,9(1x," // FMT_12 // "))")  "fm=", f_map

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate ISR structure function"
    
    call sf_int(1)%apply (scale = 100._default)
    call sf_int(2)%apply (scale = 100._default)

    write (u, "(A)")
    write (u, "(A)")  "* Structure function #1"
    write (u, "(A)")
    call sf_int(1)%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure function #2"
    write (u, "(A)")
    call sf_int(2)%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure-function value, default order"
    write (u, "(A)")

    do i = 1, 2
       f_isr(i) = interaction_get_matrix_element (sf_int(i)%interaction_t, 1)
    end do
    
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr         =", &
         product (f_isr)
    write (u, "(A,9(1x," // FMT_12 // "))")  "f_isr * f_map =", &
         product (f_isr * f) * f_map

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    do i = 1, 2
       call sf_int(i)%final ()
    end do
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_isr_5"

  end subroutine sf_isr_5


end module sf_isr
