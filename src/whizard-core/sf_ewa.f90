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

module sf_ewa

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

  public :: ewa_data_t
  public :: sf_ewa_test

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_QMIN = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: ZERO_XMIN = 3
  integer, parameter :: MASS_MIX = 4
  integer, parameter :: ZERO_SW = 5
  integer, parameter :: ISOSPIN_MIX = 6
  integer, parameter :: WRONG_PRT = 7
  integer, parameter :: MASS_MIX_OUT = 8
  integer, parameter :: NO_EWA = 9

  type, extends(sf_data_t) :: ewa_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t), dimension(:), allocatable :: flv_in
     type(flavor_t), dimension(:), allocatable :: flv_out
     real(default) :: pt_max
     real(default) :: sqrts
     real(default) :: x_min
     real(default) :: x_max
     real(default) :: mass
     real(default) :: m_out
     real(default) :: q_min
     real(default) :: cv
     real(default) :: ca
     real(default) :: costhw
     real(default) :: sinthw
     real(default) :: mW
     real(default) :: mZ
     real(default) :: coeff
     logical :: mass_set = .false.
     logical :: keep_momentum
     logical :: keep_energy     
     integer :: id = 0 
     integer :: error = NONE
   contains
     procedure :: init => ewa_data_init
     procedure :: set_id => ewa_set_id
     procedure :: check => ewa_data_check
     procedure :: write => ewa_data_write
     procedure :: get_n_par => ewa_data_get_n_par
     procedure :: get_pdg_out => ewa_data_get_pdg_out
     procedure :: allocate_sf_int => ewa_data_allocate_sf_int  
  end type ewa_data_t

  type, extends (sf_int_t) :: ewa_t
     type(ewa_data_t), pointer :: data => null ()
     real(default) :: x  = 0
     real(default) :: xb = 0
     integer :: n_me = 0
     real(default), dimension(:), allocatable :: cv
     real(default), dimension(:), allocatable :: ca
   contains
     procedure :: type_string => ewa_type_string
     procedure :: write => ewa_write
     procedure :: init => ewa_init
     procedure :: setup_constants => ewa_setup_constants
     procedure :: complete_kinematics => ewa_complete_kinematics
     procedure :: inverse_kinematics => ewa_inverse_kinematics
     procedure :: apply => ewa_apply
  end type ewa_t 
  

contains

  subroutine ewa_data_init (data, model, pdg_in, x_min, pt_max, &
        sqrts, keep_momentum, keep_energy, mass)
    class(ewa_data_t), intent(inout) :: data
    type(model_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    real(default), intent(in) :: x_min, pt_max, sqrts
    logical, intent(in) :: keep_momentum, keep_energy
    real(default), intent(in), optional :: mass
    real(default) :: g, ee
    integer :: n_flv, i
    data%model => model
    if (.not. any (pdg_in .match. &
         [1,2,3,4,5,6,11,13,15,-1,-2,-3,-4,-5,-6,-11,-13,-15])) then
       data%error = WRONG_PRT;  return
    end if   
    n_flv = pdg_array_get_length (pdg_in)
    allocate (data%flv_in (n_flv))
    allocate (data%flv_out(n_flv))
    do i = 1, n_flv
       call flavor_init (data%flv_in(i), pdg_array_get (pdg_in, i), model)
    end do
    data%pt_max = pt_max
    data%sqrts = sqrts
    data%x_min = x_min
    data%x_max = 1
    if (data%x_min == 0) then
       data%error = ZERO_XMIN;  return
    end if    
    select case (char (data%model%get_name ()))
    case ("QCD","QED","Test")
       data%error = NO_EWA;  return
    end select
    ee = model_get_parameter_value (data%model, var_str ("ee"))
    data%sinthw = model_get_parameter_value (data%model, var_str ("sw"))    
    data%costhw = model_get_parameter_value (data%model, var_str ("cw"))        
    data%mZ = model_get_parameter_value (data%model, var_str ("mZ"))
    data%mW = model_get_parameter_value (data%model, var_str ("mW"))    
    if (data%sinthw /= 0) then
       g = ee / data%sinthw
    else
       data%error = ZERO_SW;  return
    end if
    data%cv = g / 2._default
    data%ca = g / 2._default   
    data%coeff = 1._default / (8._default * PI**2)
    data%keep_momentum = keep_momentum
    data%keep_energy = keep_energy
    if (present (mass)) then
       data%mass = mass
       data%m_out = mass
       data%mass_set = .true.
    else
       data%mass = flavor_get_mass (data%flv_in(1))
       if (any (flavor_get_mass (data%flv_in) /= data%mass)) then
          data%error = MASS_MIX;  return
       end if
    end if
  end subroutine ewa_data_init

  subroutine ewa_set_id (data, id)
    class(ewa_data_t), intent(inout) :: data
    integer, intent(in) :: id
    integer :: i, isospin, pdg
    if (.not. allocated (data%flv_in)) &
         call msg_bug ("EWA: incoming particles not set")
    data%id = id
    select case (data%id)
    case (23)
       data%m_out = data%mass
       data%flv_out = data%flv_in
    case (24)
       do i = 1, size (data%flv_in)
          pdg = flavor_get_pdg (data%flv_in(i)) 
          isospin = flavor_get_isospin_type (data%flv_in(i))
          if (isospin > 0) then            
             !!! up-type quark or neutrinos
             if (flavor_is_antiparticle (data%flv_in(i))) then
                call flavor_init (data%flv_out(i), pdg + 1, data%model)
             else
                call flavor_init (data%flv_out(i), pdg - 1, data%model)
             end if
          else
             !!! down-type quark or lepton
             if (flavor_is_antiparticle (data%flv_in(i))) then
                call flavor_init (data%flv_out(i), pdg - 1, data%model)
             else
                call flavor_init (data%flv_out(i), pdg + 1, data%model)
             end if
          end if
       end do
       if (.not. data%mass_set) then
          data%m_out = flavor_get_mass (data%flv_out(1))
          if (any (flavor_get_mass (data%flv_out) /= data%m_out)) then
             data%error = MASS_MIX_OUT;  return
          end if
       end if
    end select
  end subroutine ewa_set_id 

  subroutine ewa_data_check (data)
    class(ewa_data_t), intent(in) :: data
    select case (data%error)
    case (WRONG_PRT)
       call msg_fatal ("EWA structure function only accessible for " &
            // "SM quarks and leptons.")
    case (NO_EWA)
       call msg_fatal ("EWA structure function not available for model " &
            // char (data%model%get_name ()))
    case (ZERO_SW)
       call msg_fatal ("EWA: Vanishing value of sin(theta_w)")
    case (ZERO_QMIN)
       call msg_fatal ("EWA: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal ("EWA: Particle mass exceeds Qmax")
    case (ZERO_XMIN)
       call msg_fatal ("EWA: x_min must be larger than zero")
    case (MASS_MIX)
       call msg_fatal ("EWA: incoming particle masses must be uniform")
    case (MASS_MIX_OUT)
       call msg_fatal ("EWA: outgoing particle masses must be uniform")
    case (ISOSPIN_MIX)
       call msg_fatal ("EWA: incoming particle isospins must be uniform")
    end select
  end subroutine ewa_data_check

  subroutine ewa_data_write (data, unit, verbose) 
    class(ewa_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "EWA data:"
    if (allocated (data%flv_in) .and. allocated (data%flv_out)) then
       write (u, "(3x,A)", advance="no") "  flavor(in)  =  "
       do i = 1, size (data%flv_in)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          call flavor_write (data%flv_in(i), u)
       end do
       write (u, *)
       write (u, "(3x,A)", advance="no") "  flavor(out) =  "
       do i = 1, size (data%flv_out)
          if (i > 1)  write (u, "(',',1x)", advance="no")
          call flavor_write (data%flv_out(i), u)
       end do
       write (u, *)
       write (u, "(3x,A," // FMT_19 // ")") "  x_min     = ", data%x_min
       write (u, "(3x,A," // FMT_19 // ")") "  x_max     = ", data%x_max
       write (u, "(3x,A," // FMT_19 // ")") "  pt_max    = ", data%pt_max
       write (u, "(3x,A," // FMT_19 // ")") "  sqrts     = ", data%sqrts    
       write (u, "(3x,A," // FMT_19 // ")") "  mass      = ", data%mass
       write (u, "(3x,A," // FMT_19 // ")") "  cv        = ", data%cv
       write (u, "(3x,A," // FMT_19 // ")") "  ca        = ", data%ca
       write (u, "(3x,A," // FMT_19 // ")") "  coeff     = ", data%coeff
       write (u, "(3x,A," // FMT_19 // ")") "  costhw    = ", data%costhw
       write (u, "(3x,A," // FMT_19 // ")") "  sinthw    = ", data%sinthw    
       write (u, "(3x,A," // FMT_19 // ")") "  mZ        = ", data%mZ
       write (u, "(3x,A," // FMT_19 // ")") "  mW        = ", data%mW
       write (u, "(3x,A,L2)")      "  keep_mom. = ", data%keep_momentum
       write (u, "(3x,A,L2)")      "  keep_en.  = ", data%keep_energy 
       write (u, "(3x,A,I2)")      "  PDG (VB)  = ", data%id
    else
       write (u, "(3x,A)") "[undefined]"
    end if       
  end subroutine ewa_data_write

  function ewa_data_get_n_par (data) result (n)
    class(ewa_data_t), intent(in) :: data
    integer :: n
    if (data%keep_energy .or. data%keep_momentum) then
       n = 3
    else
       n = 1
    end if
  end function ewa_data_get_n_par
  
  subroutine ewa_data_get_pdg_out (data, pdg_out)
    class(ewa_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    integer :: i, n_flv
    if (allocated (data%flv_out)) then
       n_flv = size (data%flv_out)
    else
       n_flv = 0
    end if
    allocate (pdg1 (n_flv))
    do i = 1, n_flv
       pdg1(i) = flavor_get_pdg (data%flv_out(i))
    end do
    pdg_out(1) = pdg1
  end subroutine ewa_data_get_pdg_out
  
  subroutine ewa_data_allocate_sf_int (data, sf_int)
    class(ewa_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (ewa_t :: sf_int)
  end subroutine ewa_data_allocate_sf_int
  
  function ewa_type_string (object) result (string)
    class(ewa_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "EWA: equivalent W/Z approx." 
    else
       string = "EWA: [undefined]"
    end if
  end function ewa_type_string
  
  subroutine ewa_write (object, unit, testflag)
    class(ewa_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       if (object%status >= SF_DONE_KINEMATICS) then
          write (u, "(1x,A)")  "SF parameters:"
          write (u, "(3x,A," // FMT_17 // ")")  "x =", object%x
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "EWA data: [undefined]"
    end if    
  end subroutine ewa_write
    
  subroutine ewa_init (sf_int, data)
    class(ewa_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: hel_lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel, qn_fc_fin
    type(flavor_t) :: flv_z, flv_wp, flv_wm
    type(quantum_numbers_t) :: qn_z, qn_wp, qn_wm, qn, qn_out, qn_w
    type(state_iterator_t) :: it_hel
    integer :: i, isospin
    select type (data)
    type is (ewa_data_t)   
       mask = new_quantum_numbers_mask (.false., .false., &
            mask_h = [.false., .false., .true.])
       hel_lock = [2, 1, 0]
       select case (data%id)
       case (23)
          !!! Z boson, flavor is not changing    
          call sf_int%base_init (mask, [data%mass**2], [data%mass**2], &
               [data%mZ**2], hel_lock = hel_lock)
          sf_int%data => data          
          call flavor_init (flv_z, Z_BOSON, data%model)
          call quantum_numbers_init (qn_z, flv_z)
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
                     (sf_int%interaction_t, [qn, qn, qn_z])
                call state_iterator_advance (it_hel)
             end do
             call polarization_final (pol)
          end do
       case (24)    
          call sf_int%base_init (mask, [data%mass**2], [data%m_out**2], &
               [data%mW**2], hel_lock = hel_lock)
          sf_int%data => data                       
          call flavor_init (flv_wp, W_BOSON, data%model)
          call flavor_init (flv_wm, - W_BOSON, data%model)
          call quantum_numbers_init (qn_wp, flv_wp)
          call quantum_numbers_init (qn_wm, flv_wm)
          do i = 1, size (data%flv_in)
             isospin = flavor_get_isospin_type (data%flv_in(i))
             if (isospin > 0) then            
                !!! up-type quark or neutrinos
                if (flavor_is_antiparticle (data%flv_in(i))) then
                   qn_w = qn_wm
                else
                   qn_w = qn_wp
                end if
             else
                !!! down-type quark or lepton
                if (flavor_is_antiparticle (data%flv_in(i))) then
                   qn_w = qn_wp
                else
                   qn_w = qn_wm
                end if
             end if
             call polarization_init_generic (pol, data%flv_in(i))
             call quantum_numbers_init (qn_fc(1), &
                  flv = data%flv_in(i), &
                  col = color_from_flavor (data%flv_in(i), 1))
             call quantum_numbers_init (qn_fc_fin(1), &
                  flv = data%flv_out(i), &
                  col = color_from_flavor (data%flv_out(i), 1))
             call state_iterator_init (it_hel, pol%state)
             do while (state_iterator_is_valid (it_hel))
                qn_hel = state_iterator_get_quantum_numbers (it_hel)
                qn = qn_hel(1) .merge. qn_fc(1)
                qn_out = qn_hel(1) .merge. qn_fc_fin(1)           
                call interaction_add_state &
                     (sf_int%interaction_t, [qn, qn_out, qn_w])
                call state_iterator_advance (it_hel)
             end do
             call polarization_final (pol)    
          end do
       case default
          call msg_fatal ("EWA initialization failed: wrong particle type.")
       end select
       call interaction_freeze (sf_int%interaction_t)
       if (data%keep_momentum) then
          if (data%keep_energy) then
             call msg_fatal ("EWA: momentum and energy" // &
                  "cannot be simultaneously conserved.")
          else          
             sf_int%on_shell_mode = KEEP_MOMENTUM
          end if
       else 
          if (data%keep_energy) then
             sf_int%on_shell_mode = KEEP_ENERGY
          end if
       end if
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
    end select
  end subroutine ewa_init
    
  subroutine ewa_setup_constants (sf_int)
    class(ewa_t), intent(inout) :: sf_int
    type(state_iterator_t) :: it
    type(flavor_t) :: flv
    real(default) :: q, t3
    integer :: i
    sf_int%n_me = interaction_get_n_matrix_elements (sf_int%interaction_t) 
    allocate (sf_int%cv (sf_int%n_me))
    allocate (sf_int%ca (sf_int%n_me))
    associate (data => sf_int%data)
      select case (data%id)
      case (23)
         call state_iterator_init (it, &
              interaction_get_state_matrix_ptr (sf_int%interaction_t))
         do while (state_iterator_is_valid (it))
            i = state_iterator_get_me_index (it)
            flv = state_iterator_get_flavor (it, 1)
            q = flavor_get_charge (flv)
            t3 = flavor_get_isospin (flv)
            if (flavor_is_antiparticle (flv)) then
               sf_int%cv(i) = - data%cv &
                    * (t3 - 2._default * q * data%sinthw**2) / data%costhw
               sf_int%ca(i) = data%ca *  t3 / data%costhw    
            else          
               sf_int%cv(i) = data%cv &
                    * (t3 - 2._default * q * data%sinthw**2) / data%costhw
               sf_int%ca(i) = data%ca *  t3 / data%costhw    
            end if
            call state_iterator_advance (it)
         end do
      case (24)
         call state_iterator_init (it, &
              interaction_get_state_matrix_ptr (sf_int%interaction_t))
         do while (state_iterator_is_valid (it))
            i = state_iterator_get_me_index (it)
            if (flavor_is_antiparticle (state_iterator_get_flavor (it, 1))) &
                 then
               sf_int%cv(i) = data%cv / sqrt(2._default)
               sf_int%ca(i) = - data%ca / sqrt(2._default)
            else          
               sf_int%cv(i) = data%cv / sqrt(2._default)
               sf_int%ca(i) = data%ca / sqrt(2._default)
            end if
            call state_iterator_advance (it)
         end do
      end select
    end associate
    sf_int%status = SF_INITIAL
  end subroutine ewa_setup_constants
  
  subroutine ewa_complete_kinematics (sf_int, x, f, r, rb, map)
    class(ewa_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1, e_1
    real(default) :: x0, x1, lx0, lx1, lx
    e_1 = energy (interaction_get_momentum (sf_int%interaction_t, 1))
    if (sf_int%data%keep_momentum .or. sf_int%data%keep_energy) then
       select case (sf_int%data%id)
       case (23)
          x0 = max (sf_int%data%x_min, sf_int%data%mz / e_1)
       case (24)
          x0 = max (sf_int%data%x_min, sf_int%data%mw / e_1)
       end select
    else 
       x0 = sf_int%data%x_min
    end if
    x1 = sf_int%data%x_max
    if ( x0 >= x1) then
       f = 0
       sf_int%status = SF_FAILED_KINEMATICS
       return
    end if
    if (map) then
       lx0 = log (x0)
       lx1 = log (x1)
       lx = lx1 * r(1) + lx0 * rb(1)
       x(1) = exp(lx)       
       f = x(1) * (lx1 - lx0)
    else       
       x(1) = r(1)
       if (x0 < x(1) .and. x(1) < x1) then
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
       sf_int%x  = x(1)
       sf_int%xb = xb1
    case (SF_FAILED_KINEMATICS)
       sf_int%x  = 0
       sf_int%xb = 0
       f = 0
    end select
  end subroutine ewa_complete_kinematics

  subroutine ewa_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(ewa_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: x0, x1, lx0, lx1, lx, e_1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    e_1 = energy (interaction_get_momentum (sf_int%interaction_t, 1))    
    if (sf_int%data%keep_momentum .or. sf_int%data%keep_energy) then
       select case (sf_int%data%id)
       case (23)
          x0 = max (sf_int%data%x_min, sf_int%data%mz / e_1)
       case (24)
          x0 = max (sf_int%data%x_min, sf_int%data%mw / e_1)
       end select
    else 
       x0 = sf_int%data%x_min
    end if
    x1 = sf_int%data%x_max
    if (map) then
       lx0 = log (x0)
       lx1 = log (x1)
       lx = log (x(1))
       r(1)  = (lx - lx0) / (lx1 - lx0)
       rb(1) = (lx1 - lx) / (lx1 - lx0)
       f = x(1) * (lx1 - lx0)
    else
       r (1) = x(1)
       rb(1) = 1 - x(1)
       if (x0 < x(1) .and. x(1) < x1) then
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
       case (SF_FAILED_KINEMATICS)
          sf_int%x = 0
          f = 0
       end select
    end if
  end subroutine ewa_inverse_kinematics

  subroutine ewa_apply (sf_int, scale)
    class(ewa_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: x, xb, pt2, c1, c2
    real(default) :: cv, ca
    real(default) :: f, fm, fp, fL
    integer :: i
    associate (data => sf_int%data)     
      x  = sf_int%x      
      xb = sf_int%xb
      pt2 = min ((data%pt_max)**2, (xb * data%sqrts / 2)**2)
      select case (data%id)
      case (23)
         !!! Z boson structure function
         c1 = log (1 + pt2 / (xb * (data%mZ)**2))
         c2 = 1 / (1 + (xb * (data%mZ)**2) / pt2)
      case (24)
         !!! W boson structure function
         c1 = log (1 + pt2 / (xb * (data%mW)**2))
         c2 = 1 / (1 + (xb * (data%mW)**2) / pt2)
      end select
      do i = 1, sf_int%n_me
         cv = sf_int%cv(i)
         ca = sf_int%ca(i)
         fm = data%coeff * &
              ((cv + ca)**2 + ((cv - ca) * xb)**2) * (c1 - c2) / (2 * x)
         fp = data%coeff * &
              ((cv - ca)**2 + ((cv + ca) * xb)**2) * (c1 - c2) / (2 * x)
         fL = data%coeff * &
              (cv**2 + ca**2) * (2 * xb / x) * c2       
         f = fp + fm + fL
         if (f /= 0) then
            fp = fp / f
            fm = fm / f
            fL = fL / f      
         end if
         call interaction_set_matrix_element &
              (sf_int%interaction_t, i, cmplx (f, kind=default))
      end do
    end associate
    sf_int%status = SF_EVALUATED
  end subroutine ewa_apply


  subroutine sf_ewa_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_ewa_1, "sf_ewa_1", &
         "structure function configuration", &
         u, results)
    call test (sf_ewa_2, "sf_ewa_2", &
         "structure function instance", &
         u, results)
    call test (sf_ewa_3, "sf_ewa_3", &
         "apply mapping", &
         u, results)
    call test (sf_ewa_4, "sf_ewa_4", &
         "non-collinear", &
         u, results)
    call test (sf_ewa_5, "sf_ewa_5", &
         "structure function instance", &
         u, results)
  end subroutine sf_ewa_test
  
  subroutine sf_ewa_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_ewa_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    pdg_in = 2

    allocate (ewa_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize for Z boson"
    write (u, "(A)")

    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 5000._default, .false., .false.)
       call data%set_id (23)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(2x,99(1x,I0))")  pdg1
        
    write (u, "(A)")
    write (u, "(A)")  "* Initialize for W boson"
    write (u, "(A)")
    
    deallocate (data)
    allocate (ewa_data_t :: data)
    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 5000._default, .false., .false.)
       call data%set_id (24)
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
    write (u, "(A)")  "* Test output end: sf_ewa_1"

  end subroutine sf_ewa_1

  subroutine sf_ewa_2 (u)
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
    
    write (u, "(A)")  "* Test output: sf_ewa_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    call flavor_init (flv, 2, model)
    pdg_in = 2

    call reset_interaction_counter ()
    
    allocate (ewa_data_t :: data)
    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 3000._default, .false., .false.)
       call data%set_id (24)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=1500"
    write (u, "(A)")
    E = 1500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, no EWA mapping, collinear"
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
    write (u, "(A)")  "* Evaluate EWA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
        
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_ewa_2"
    
  end subroutine sf_ewa_2
  
  subroutine sf_ewa_3 (u)
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
    
    write (u, "(A)")  "* Test output: sf_ewa_3"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    call flavor_init (flv, 2, model)
    pdg_in = 2

    call reset_interaction_counter ()
    
    allocate (ewa_data_t :: data)
    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 3000._default, .false., .false.)
       call data%set_id (24)
    end select
    
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")    
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=1500"
    write (u, "(A)")
    E = 1500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, with EWA mapping, collinear"
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
    write (u, "(A)")  "* Evaluate EWA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_ewa_3"

  end subroutine sf_ewa_3

  subroutine sf_ewa_4 (u)
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
    
    write (u, "(A)")  "* Test output: sf_ewa_4"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    call flavor_init (flv, 2, model)
    pdg_in = 2

    call reset_interaction_counter ()

    allocate (ewa_data_t :: data)
    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 3000.0_default, .false., .true.)
       call data%set_id (24)
    end select    

    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
        
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()

    write (u, "(A)")  "* Initialize incoming momentum with E=1500"
    write (u, "(A)")
    E = 1500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])        
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.5/0.5/0.25, with EWA mapping, "
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
    write (u, "(A)")  "* Evaluate EWA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 1500._default)
    call sf_int%write (u, testflag = .true.)    
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_ewa_4"

  end subroutine sf_ewa_4

  subroutine sf_ewa_5 (u)
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
    
    write (u, "(A)")  "* Test output: sf_ewa_5"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM"), &
         var_str ("SM.mdl"), os_data, model)
    call flavor_init (flv, 2, model)
    pdg_in = [1, 2, -1, -2]

    call reset_interaction_counter ()
    
    allocate (ewa_data_t :: data)
    select type (data)
    type is (ewa_data_t)
       call data%init (model, pdg_in, 0.01_default, &
            500._default, 3000._default, .false., .false.)
       call data%set_id (24)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    call sf_int%setup_constants ()
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=1500"
    write (u, "(A)")
    E = 1500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call pacify (k, 1e-10_default)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for r=0.4, no EWA mapping, collinear"
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
    write (u, "(A)")  "* Evaluate EWA structure function"
    write (u, "(A)")
    
    call sf_int%apply (scale = 100._default)
    call sf_int%write (u)
        
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_ewa_5"
    
  end subroutine sf_ewa_5
  

end module sf_ewa
