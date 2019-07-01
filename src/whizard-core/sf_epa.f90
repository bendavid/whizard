! WHIZARD 2.2.0 May 18 2014
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

module sf_epa

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants, only: pi !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_17, FMT_19 !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use unit_tests
  use os_interface
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

  public :: epa_data_t
  public :: sf_epa_test

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_QMIN = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: ZERO_XMIN = 3
  integer, parameter :: MASS_MIX = 4
  integer, parameter :: NO_EPA = 5

  type, extends(sf_data_t) :: epa_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t), dimension(:), allocatable :: flv_in
     real(default) :: alpha
     real(default) :: x_min
     real(default) :: x_max
     real(default) :: q_min
     real(default) :: q_max
     real(default) :: E_max
     real(default) :: mass
     real(default) :: log
     real(default) :: a
     real(default) :: c0
     real(default) :: c1
     real(default) :: dc
     integer :: error = NONE
     logical :: recoil = .false.
   contains
     procedure :: init => epa_data_init
     procedure :: check => epa_data_check
     procedure :: write => epa_data_write
     procedure :: get_n_par => epa_data_get_n_par
     procedure :: get_pdg_out => epa_data_get_pdg_out
     procedure :: allocate_sf_int => epa_data_allocate_sf_int  
  end type epa_data_t

  type, extends (sf_int_t) :: epa_t
     type(epa_data_t), pointer :: data => null ()
     real(default) :: x  = 0
     real(default) :: xb = 0
     real(default) :: E  = 0     
     real(default), dimension(:), allocatable :: charge2
   contains
     procedure :: type_string => epa_type_string
     procedure :: write => epa_write
     procedure :: init => epa_init
     procedure :: setup_constants => epa_setup_constants
     procedure :: complete_kinematics => epa_complete_kinematics
     procedure :: inverse_kinematics => epa_inverse_kinematics
     procedure :: recover_x => sf_epa_recover_x
     procedure :: apply => epa_apply
  end type epa_t 
  

contains

  subroutine epa_data_init &
       (data, model, pdg_in, alpha, x_min, q_min, E_max, mass, recoil)
    class(epa_data_t), intent(inout) :: data
    type(model_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    real(default), intent(in) :: alpha, x_min, q_min, E_max
    real(default), intent(in), optional :: mass
    logical, intent(in), optional :: recoil
    integer :: n_flv, i
    data%model => model
    n_flv = pdg_array_get_length (pdg_in)
    allocate (data%flv_in (n_flv))
    do i = 1, n_flv
       call flavor_init (data%flv_in(i), pdg_array_get (pdg_in, i), model)
    end do
    data%alpha = alpha
    data%E_max = E_max
    data%x_min = x_min
    data%x_max = 1
    if (data%x_min == 0) then
       data%error = ZERO_XMIN;  return
    end if
    data%q_min = q_min
    data%q_max = 2 * data%E_max
    select case (char (data%model%get_name ()))
    case ("QCD","Test")
       data%error = NO_EPA;  return
    end select     
    if (present (recoil)) then
       data%recoil = recoil
    end if
    if (present (mass)) then
       data%mass = mass
    else
       data%mass = flavor_get_mass (data%flv_in(1))
       if (any (flavor_get_mass (data%flv_in) /= data%mass)) then
          data%error = MASS_MIX;  return
       end if 
    end if
    if (max (data%mass, data%q_min) == 0) then
       data%error = ZERO_QMIN;  return
    else if (max (data%mass, data%q_min) >= data%E_max) then
       data%error = Q_MAX_TOO_SMALL;  return
    end if
    data%log = log (4 * (data%E_max / max (data%mass, data%q_min)) ** 2 )
    data%a  = data%alpha / pi
    data%c0 = log (data%x_min) * (data%log - log (data%x_min))
    data%c1 = log (data%x_max) * (data%log - log (data%x_max))
    data%dc = data%c1 - data%c0
  end subroutine epa_data_init

  subroutine epa_data_check (data)
    class(epa_data_t), intent(in) :: data
    select case (data%error)
    case (NO_EPA)
       call msg_fatal ("EPA structure function not available for model " &
            // char (data%model%get_name ()) // ".")
    case (ZERO_QMIN)
       call msg_fatal ("EPA: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal ("EPA: Particle mass exceeds Qmax")
    case (ZERO_XMIN)
       call msg_fatal ("EPA: x_min must be larger than zero")
    case (MASS_MIX)
       call msg_fatal ("EPA: incoming particle masses must be uniform")
    end select
  end subroutine epa_data_check

  subroutine epa_data_write (data, unit, verbose) 
    class(epa_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "EPA data:"
    if (allocated (data%flv_in)) then
       write (u, "(3x,A)", advance="no") "  flavor =  "
       do i = 1, size (data%flv_in)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          call flavor_write (data%flv_in(i), u)
       end do
       write (u, *)
       write (u, "(3x,A," // FMT_19 // ")") "  alpha  = ", data%alpha
       write (u, "(3x,A," // FMT_19 // ")") "  x_min  = ", data%x_min
       write (u, "(3x,A," // FMT_19 // ")") "  x_max  = ", data%x_max
       write (u, "(3x,A," // FMT_19 // ")") "  q_min  = ", data%q_min
       write (u, "(3x,A," // FMT_19 // ")") "  q_max  = ", data%q_max
       write (u, "(3x,A," // FMT_19 // ")") "  E_max  = ", data%e_max
       write (u, "(3x,A," // FMT_19 // ")") "  mass   = ", data%mass
       write (u, "(3x,A," // FMT_19 // ")") "  a      = ", data%a
       write (u, "(3x,A," // FMT_19 // ")") "  c0     = ", data%c0
       write (u, "(3x,A," // FMT_19 // ")") "  c1     = ", data%c1
       write (u, "(3x,A," // FMT_19 // ")") "  log    = ", data%log
       write (u, "(3x,A,L2)")      "  recoil = ", data%recoil
    else
       write (u, "(3x,A)") "[undefined]"
    end if
  end subroutine epa_data_write

  function epa_data_get_n_par (data) result (n)
    class(epa_data_t), intent(in) :: data
    integer :: n
    if (data%recoil) then
       n = 3
    else
       n = 1
    end if
  end function epa_data_get_n_par
  
  subroutine epa_data_get_pdg_out (data, pdg_out)
    class(epa_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    pdg_out(1) = PHOTON
  end subroutine epa_data_get_pdg_out
  
  subroutine epa_data_allocate_sf_int (data, sf_int)
    class(epa_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (epa_t :: sf_int)
  end subroutine epa_data_allocate_sf_int
  
  function epa_type_string (object) result (string)
    class(epa_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "EPA: equivalent photon approx." 
    else
       string = "EPA: [undefined]"
    end if
  end function epa_type_string
  
  subroutine epa_write (object, unit, testflag)
    class(epa_t), intent(in) :: object
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
             write (u, "(3x,A," // FMT_17 // ")")  "E =", object%E
          end if          
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "EPA data: [undefined]"
    end if
  end subroutine epa_write
    
  subroutine epa_init (sf_int, data)
    class(epa_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: hel_lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel
    type(flavor_t) :: flv_photon
    type(quantum_numbers_t) :: qn_photon, qn
    type(state_iterator_t) :: it_hel
    integer :: i
    mask = new_quantum_numbers_mask (.false., .false., &
         mask_h = [.false., .false., .true.])
    hel_lock = [2, 1, 0]
    select type (data)
    type is (epa_data_t)
       call sf_int%base_init (mask, [data%mass**2], &
            [data%mass**2], [0._default], hel_lock = hel_lock)       
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
             call interaction_add_state (sf_int%interaction_t, &
                  [qn, qn, qn_photon])
             call state_iterator_advance (it_hel)
          end do
          call polarization_final (pol)
       end do
       call interaction_freeze (sf_int%interaction_t)
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
    end select
  end subroutine epa_init
    
  subroutine epa_setup_constants (sf_int)
    class(epa_t), intent(inout) :: sf_int
    type(state_iterator_t) :: it
    integer :: i, n_me
    n_me = interaction_get_n_matrix_elements (sf_int%interaction_t)
    allocate (sf_int%charge2 (n_me))
    call state_iterator_init (it, &
         interaction_get_state_matrix_ptr (sf_int%interaction_t))
    do while (state_iterator_is_valid (it))
       i = state_iterator_get_me_index (it)
       sf_int%charge2(i) = &
            flavor_get_charge (state_iterator_get_flavor (it, 1)) ** 2
       call state_iterator_advance (it)
    end do
    sf_int%status = SF_INITIAL
  end subroutine epa_setup_constants
  
  subroutine epa_complete_kinematics (sf_int, x, f, r, rb, map)
    class(epa_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1
    real(default) :: delta, sqrt_delta, lx
    if (map) then                                   
       associate (data => sf_int%data)
         delta = data%log ** 2 -  4 * (r(1) * data%c1 + rb(1) * data%c0)
         if (delta > 0) then
            sqrt_delta = sqrt (delta)
            lx = (data%log - sqrt_delta) / 2
         else
            sf_int%status = SF_FAILED_KINEMATICS          
            f = 0
            return
         end if
         x(1) = exp (lx)
         f = x(1) * data%dc / sqrt_delta
       end associate
    else
       x(1) = r(1)
       if (sf_int%data%x_min < x(1) .and. x(1) < sf_int%data%x_max) then
          f = 1
       else
          sf_int%status = SF_FAILED_KINEMATICS
          f = 0
          return
       end if       
    end if       
    xb1 = 1 - x(1)
    if (size(x) == 3)  x(2:3) = r(2:3)
    call sf_int%split_momentum (x, xb1) 
    select case (sf_int%status)
    case (SF_DONE_KINEMATICS)
       sf_int%x = x(1)
       sf_int%xb= xb1
       sf_int%E  = &
            energy (interaction_get_momentum (sf_int%interaction_t, 1))
    case (SF_FAILED_KINEMATICS)
       sf_int%x = 0
       sf_int%xb= 0
       f = 0
    end select
  end subroutine epa_complete_kinematics

  subroutine epa_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(epa_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: lx, delta, sqrt_delta, c
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       associate (data => sf_int%data)
         lx = log (x(1))
         sqrt_delta = data%log - 2 * lx
         delta = sqrt_delta ** 2
         c = (data%log ** 2 - delta) / 4
         r (1) = (c - data%c0) / data%dc
         rb(1) = (data%c1 - c) / data%dc
         f = x(1) * data%dc / sqrt_delta
       end associate
    else
       r (1) = x(1)
       rb(1) = 1 - x(1)
       if (sf_int%data%x_min < x(1) .and. x(1) < sf_int%data%x_max) then
          f = 1
       else
          f = 0
       end if
    end if
    if (size(r) == 3) then
       r (2:3) = x(2:3)
       rb(2:3) = 1 - x(2:3)
    end if
    if (set_mom) then
       call sf_int%split_momentum (x, sf_int%xb)
       select case (sf_int%status)
       case (SF_DONE_KINEMATICS)
          sf_int%x  = x(1)
          sf_int%xb = 1 - x(1)
          sf_int%E  = &
               energy (interaction_get_momentum (sf_int%interaction_t, 1))
       case (SF_FAILED_KINEMATICS)
          sf_int%x = 0
          f = 0
       end select
    end if
  end subroutine epa_inverse_kinematics

  subroutine sf_epa_recover_x (sf_int, x, x_free)
    class(epa_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(inout), optional :: x_free
    call sf_int%base_recover_x (x, x_free)
    sf_int%x  = x(1)
    sf_int%xb = 1 - x(1)
  end subroutine sf_epa_recover_x
  
  subroutine epa_apply (sf_int, scale)
    class(epa_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: x, xb, qminsq, qmaxsq, f, E
    associate (data => sf_int%data)
      x = sf_int%x
      xb= sf_int%xb
      E = sf_int%E
      qminsq = max (x ** 2 / xb * data%mass ** 2, data%q_min ** 2)
      qmaxsq = min (4 * xb * E ** 2, data%q_max ** 2)
      if (qminsq < qmaxsq) then
         f = data%a / x &
              * ((xb + x ** 2 / 2) * log (qmaxsq / qminsq) &
              - (1 - x / 2) ** 2 &
              * log ((x**2 + qmaxsq / E ** 2) / (x**2 + qminsq / E ** 2)) &
              - x ** 2 * data%mass ** 2 / qminsq * (1 - qminsq / qmaxsq))
      else
         f = 0
      end if
      call interaction_set_matrix_element &
           (sf_int%interaction_t, cmplx (f, kind=default) * sf_int%charge2)
    end associate
    sf_int%status = SF_EVALUATED
  end subroutine epa_apply


  subroutine sf_epa_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_epa_1, "sf_epa_1", &
         "structure function configuration", &
         u, results)
    call test (sf_epa_2, "sf_epa_2", &
         "structure function instance", &
         u, results)
    call test (sf_epa_3, "sf_epa_3", &
         "apply mapping", &
         u, results)
    call test (sf_epa_4, "sf_epa_4", &
         "non-collinear", &
         u, results)
    call test (sf_epa_5, "sf_epa_5", &
         "multiple flavors", &
         u, results)
  end subroutine sf_epa_test
  
  subroutine sf_epa_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_epa_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    pdg_in = ELECTRON

    allocate (epa_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    select type (data)
    type is (epa_data_t)
       call data%init (model, pdg_in, 1./137._default, 0.01_default, &
            10._default, 50._default, 0.000511_default, recoil = .false.)
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
    write (u, "(A)")  "* Test output end: sf_epa_1"

  end subroutine sf_epa_1

  subroutine sf_epa_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_epa_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()
    
    allocate (epa_data_t :: data)
    select type (data)
    type is (epa_data_t)
       call data%init (model, pdg_in, 1./137._default, 0.01_default, &
            10._default, 50._default, 0.000511_default, recoil = .false.)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, no EPA mapping, collinear"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = 0.4_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
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
    call sf_int%setup_constants ()
    
    call sf_int%seed_kinematics ([k])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false., &
         set_momenta=.true.)
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate EPA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_epa_2"

  end subroutine sf_epa_2

  subroutine sf_epa_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_epa_3"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()
    
    allocate (epa_data_t :: data)
    select type (data)
    type is (epa_data_t)
       call data%init (model, pdg_in, 1./137._default, 0.01_default, &
            10._default, 50._default, 0.000511_default, recoil = .false.)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, with EPA mapping, collinear"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = 0.4_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
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
    call sf_int%setup_constants ()
    
    call sf_int%seed_kinematics ([k])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true., &
         set_momenta=.true.)
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate EPA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_epa_3"

  end subroutine sf_epa_3

  subroutine sf_epa_4 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E, m
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_epa_4"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    call flavor_init (flv, ELECTRON, model)
    pdg_in = ELECTRON

    call reset_interaction_counter ()

    allocate (epa_data_t :: data)
    select type (data)
    type is (epa_data_t)
       call data%init (model, pdg_in, 1./137._default, 0.01_default, &
            10._default, 50._default, 5.0_default, recoil = .true.)
    end select    

    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
        
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()

    write (u, "(A)")  "* Initialize incoming momentum with E=500, me = 5 GeV"
    write (u, "(A)")
    E = 500
    m = 5
    k = vector4_moving (E, sqrt (E**2 - m**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
        
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.5/0.5/0.25, with EPA mapping, "
    write (u, "(A)")  "          non-coll., keeping energy, me = 5 GeV"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = [0.5_default, 0.5_default, 0.25_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY    
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    call interaction_pacify_momenta (sf_int%interaction_t, 1e-10_default)

    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
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
    call sf_int%setup_constants ()
    
    call sf_int%seed_kinematics ([k])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true., &
         set_momenta=.true.)    
    call interaction_pacify_momenta (sf_int%interaction_t, 1e-10_default)    
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate EPA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u, testflag = .true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_epa_4"

  end subroutine sf_epa_4

  subroutine sf_epa_5 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_epa_5"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    call flavor_init (flv, 1, model)
    pdg_in = [1, 2, -1, -2]

    call reset_interaction_counter ()
    
    allocate (epa_data_t :: data)
    select type (data)
    type is (epa_data_t)
       call data%init (model, pdg_in, 1./137._default, 0.01_default, &
            10._default, 50._default, 0.000511_default, recoil = .false.)
       call data%check ()
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, no EPA mapping, collinear"
    write (u, "(A)")
    
    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    r = 0.4_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate EPA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_epa_5"

  end subroutine sf_epa_5


end module sf_epa
