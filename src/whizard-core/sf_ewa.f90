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

module sf_ewa

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use constants, only: pi !NODEP!
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

  public :: ewa_data_t
  public :: ewa_data_init
  public :: ewa_data_check
  public :: ewa_set_id
  public :: ewa_data_write
  public :: interaction_init_ewa
  public :: interaction_apply_ewa

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_QMIN = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: ZERO_XMIN = 3

  type :: ewa_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv
     real(default) :: pt_max
     real(default) :: sqrts
     real(default) :: x_min
     real(default) :: x_max
     real(default) :: mass
     real(default) :: q_min
     real(default) :: cv
     real(default) :: ca
     real(default) :: costhw
     real(default) :: sinthw
     real(default) :: mW
     real(default) :: mZ
     real(default) :: coeff
     logical :: keep_momentum
     logical :: keep_energy     
     integer :: id = 0 
     integer :: error = 0
  end type ewa_data_t


contains

  subroutine ewa_data_init (data, model, flv, x_min, q_min, pt_max, &
        sqrts, keep_momentum, keep_energy, mass)
    type(ewa_data_t), intent(inout) :: data
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: x_min, q_min, pt_max, sqrts
    logical, intent(in) :: keep_momentum, keep_energy
    real(default), intent(in), optional :: mass
    real(default) :: g, ee, sinthw
    data%model => model
    data%flv = flv
    data%pt_max = pt_max
    data%sqrts = sqrts
    data%x_min = x_min
    data%x_max = 1
    if (data%x_min == 0) then
       data%error = ZERO_XMIN;  return
    end if    
    select case (char (model_get_name (data%model)))
    case ("QCD","QED","Test")
       call msg_fatal ("EWA structure function not available for model " &
            // char (model_get_name (data%model)) // ".")
    end select     
    ee = model_get_parameter_value (data%model, var_str ("ee"))
    data%sinthw = model_get_parameter_value (data%model, var_str ("sw"))    
    data%costhw = model_get_parameter_value (data%model, var_str ("cw"))        
    data%mZ = model_get_parameter_value (data%model, var_str ("mZ"))
    data%mW = model_get_parameter_value (data%model, var_str ("mW"))    
    if (data%sinthw /= 0) then
       g = ee / data%sinthw
    else
       call msg_fatal ("Vanishing value of sin(theta_w).")
    end if
    data%cv = g / 2._default
    data%ca = g / 2._default   
    data%coeff = 1._default / (8._default * PI**2)
    data%keep_momentum = keep_momentum
    data%keep_energy = keep_energy
    if (present (mass)) then
       data%mass = mass
    else
       data%mass = flavor_get_mass (flv)
    end if
  end subroutine ewa_data_init

  subroutine ewa_data_check (data)
    type(ewa_data_t), intent(in) :: data
    select case (data%error)
    case (ZERO_QMIN)
       call msg_fatal (" EWA: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal (" EWA: Particle mass exceeds Qmax")
    case (ZERO_XMIN)
       call msg_fatal (" EWA: x_min must be larger than zero")
    end select
  end subroutine ewa_data_check

  subroutine ewa_set_id (data, id)
    type (ewa_data_t), intent(inout) :: data
    integer, intent(in) :: id
    data%id = id
  end subroutine ewa_set_id 

  subroutine ewa_data_write (data, unit, md5)
    type(ewa_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "EWA data:"
    write (u, *) "  prt       = ", char (flavor_get_name (data%flv))
    write (u, *) "  x_min     = ", data%x_min
    write (u, *) "  x_max     = ", data%x_max
    write (u, *) "  pt_max    = ", data%pt_max
    write (u, *) "  sqrts     = ", data%sqrts    
    write (u, *) "  mass      = ", data%mass
    write (u, *) "  cv        = ", data%cv
    write (u, *) "  ca        = ", data%ca
    write (u, *) "  coeff     = ", data%coeff
    write (u, *) "  costhw    = ", data%costhw
    write (u, *) "  sinthw    = ", data%sinthw    
    write (u, *) "  mZ        = ", data%mZ
    write (u, *) "  mW        = ", data%mW
    write (u, *) "  keep_mom. = ", data%keep_momentum
    write (u, *) "  keep_en.  = ", data%keep_energy 
  end subroutine ewa_data_write

  subroutine interaction_init_ewa (int, data)
    type(interaction_t), intent(out) :: int
    type(ewa_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: hel_lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel, qn_fc_fin
    type(flavor_t) :: flv_z, flv_wp, flv_wm, flv_down, flv_up
    type(quantum_numbers_t) :: qn_z, qn_wp, qn_wm, qn, qn_down, &
         qn_up
    type(state_iterator_t) :: it_hel
    real(default) :: q, t3       
    integer :: i, isospin, pdg
    logical :: up_type
    isospin = flavor_get_isospin_type (data%flv)
    pdg = flavor_get_pdg (data%flv) 
    if (abs(isospin) /= 2 .or. abs(pdg) > 16 .or. pdg == 0 .or. pdg == 9) then
       call msg_fatal ("EWA structure function only accessible for " &
            // "SM quarks and leptons.")
    end if
    q = - flavor_get_charge (data%flv)        
    t3 = - flavor_get_isospin (data%flv)
    up_type = (t3 > 0)
    mask = new_quantum_numbers_mask (.false., .false., &
         mask_h = (/ .false., .false., .true. /))
    hel_lock = (/ 2, 1, 0 /)
    call interaction_init &
         (int, 1, 0, 2, mask=mask, hel_lock=hel_lock, set_relations=.true.)
    select case (data%id)
    case (23)
       !!! Z boson, flavor is not changing    
       call flavor_init (flv_z, Z_BOSON, data%model)
       call quantum_numbers_init (qn_z, flv_z)
       call polarization_init_generic (pol, data%flv)
       call quantum_numbers_init (qn_fc(1), &
               flv = data%flv, col = color_from_flavor (data%flv))
       call state_iterator_init (it_hel, pol%state)
       do while (state_iterator_is_valid (it_hel))
           qn_hel = state_iterator_get_quantum_numbers (it_hel)
           qn = qn_hel(1) .merge. qn_fc(1)
           call interaction_add_state (int, (/ qn, qn, qn_z /))
           call state_iterator_advance (it_hel)
       end do
       call polarization_final (pol)
    case (24)    
       if (up_type) then
          !!! W+, flavor changing
          call flavor_init (flv_down, pdg - 1, data%model)
          call flavor_init (flv_wp, W_BOSON, data%model)
          call quantum_numbers_init (qn_wp, flv_wp)
          call polarization_init_generic (pol, data%flv)
          call quantum_numbers_init (qn_fc(1), &
                   flv = data%flv, col = color_from_flavor (data%flv))
          call quantum_numbers_init (qn_fc_fin(1), &
                  flv = flv_down, col = color_from_flavor (flv_down))
          call state_iterator_init (it_hel, pol%state)
          do while (state_iterator_is_valid (it_hel))
              qn_hel = state_iterator_get_quantum_numbers (it_hel)
              qn = qn_hel(1) .merge. qn_fc(1)
              qn_down = qn_hel(1) .merge. qn_fc_fin(1)           
              call interaction_add_state (int, (/ qn, qn_down, qn_wp /))
              call state_iterator_advance (it_hel)
          end do
          call polarization_final (pol)    
       else
          !!! W-, flavor changing
          call flavor_init (flv_up, pdg + 1, data%model)
          call flavor_init (flv_wm, - W_BOSON, data%model)
          call quantum_numbers_init (qn_wm, flv_wm)
          call polarization_init_generic (pol, data%flv)
          call quantum_numbers_init (qn_fc(1), &
                  flv = data%flv, col = color_from_flavor (data%flv))
          call quantum_numbers_init (qn_fc_fin(1), &
                  flv = flv_up, col = color_from_flavor (flv_up))
          call state_iterator_init (it_hel, pol%state)
          do while (state_iterator_is_valid (it_hel))
              qn_hel = state_iterator_get_quantum_numbers (it_hel)
              qn = qn_hel(1) .merge. qn_fc(1)
              qn_up = qn_hel(1) .merge. qn_fc_fin(1)           
              call interaction_add_state (int, (/ qn, qn_up, qn_wm /))
              call state_iterator_advance (it_hel)
          end do
          call polarization_final (pol)    
       end if   
    case default
       call msg_fatal ("EWA initialization failed: wrong particle type.")
    end select   
    call interaction_freeze (int)
  end subroutine interaction_init_ewa
    
  elemental subroutine strfun (f, fp, fm, fL, x, xb, r, E, data, no_map)  
    real(default), intent(out) :: f, fm, fp, fL
    real(default) :: fsum
    real(default), intent(out) :: x, xb
    real(default), intent(in) :: r, E
    type(ewa_data_t), intent(in) :: data
    logical, intent(in) :: no_map
    real(default) :: x0, x1
    real(default) :: rb, lx0, lx1, lx, d, den
    real(default) :: c1, c2, pt2
    real(default) :: costhw, sinthw
    real(default) :: cv, ca, q, t3
    if (data%keep_momentum .or. data%keep_energy) then
       select case (data%id)
       case (23)
          x0 = max (data%x_min, data%mz/E)
       case (24)
          x0 = max (data%x_min, data%mw/E)
       end select
    else 
       x0 = data%x_min
    end if
    x1 = data%x_max
    if (x0 >= x1) then
       f = 0
       fp = 0
       fm = 0
       fL = 0
       return
    end if
    lx0 = log (x0)
    lx1 = log (x1)
    lx = lx1 * r + lx0 * (1-r)
    x = exp(lx)
    xb = 1 - x
    f = data%coeff * (lx1 - lx0)
    pt2 = min ((data%pt_max)**2, (xb * data%sqrts / 2)**2)
    select case (data%id)
    case (23)
       !!! Z boson structure function
       c1 = log (1 + pt2 / (xb * (data%mZ)**2))
       c2 = 1 / (1 + (xb * (data%mZ)**2) / pt2)
       if (flavor_is_antiparticle (data%flv)) then
          q = flavor_get_charge (data%flv)
          t3 = flavor_get_isospin (data%flv)
       else       
          q = - flavor_get_charge (data%flv)        
          t3 = - flavor_get_isospin (data%flv)        
       end if
       cv = data%cv * (t3 - 2._default * q * data%sinthw**2) / data%costhw
       ca = data%ca *  t3 / data%costhw    
       if (flavor_is_antiparticle (data%flv)) ca = - ca
       fm = ((cv + ca)**2 + ((cv - ca) * xb)**2) / 2 * (c1 - c2)
       fp = ((cv - ca)**2 + ((cv + ca) * xb)**2) / 2 * (c1 - c2)    
       fL = (cv**2 + ca**2) * 2 * xb * c2
       fsum = fp + fm + fL
       f = f * fsum
       if (fsum /= 0) then
          fp = fp / fsum
          fm = fm / fsum
          fL = fL / fsum
       end if 
    case (24)
       !!! W boson structure function
       c1 = log (1 + pt2 / (xb * (data%mW)**2))
       c2 = 1 / (1 + (xb * (data%mW)**2) / pt2)
       cv = data%cv / sqrt(2._default)
       ca = data%ca / sqrt(2._default)
       if (flavor_is_antiparticle (data%flv)) ca = - ca        
       fm = ((cv + ca)**2 + ((cv - ca) * xb)**2) / 2 * (c1 - c2)
       fp = ((cv - ca)**2 + ((cv + ca) * xb)**2) / 2 * (c1 - c2)    
       fL = (cv**2 + ca**2) * 2 * xb * c2    
       fsum = fp + fm + fL
       f = f * fsum
       if (fsum /= 0) then
          fp = fp / fsum
          fm = fm / fsum
          fL = fL / fsum
       end if 
    end select   
  end subroutine strfun
       
  subroutine interaction_apply_ewa (int, r, ewa_data, no_map)
    type(interaction_t), intent(inout) :: int
    real(default), dimension(:), intent(in) :: r
    type(ewa_data_t), dimension(:), intent(in) :: ewa_data
    logical, intent(in) :: no_map
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: k_split
    type(splitting_data_t) :: sd
    real(default), dimension(size(ewa_data)) :: f, fp, fm, fL 
    real(default), dimension(size(ewa_data)) :: x, xb
    k = interaction_get_momentum (int, 1)
    select case (ewa_data(1)%id)
    case (23) 
       sd = new_splitting_data (k, k**2, ewa_data(1)%mass**2, ewa_data(1)%mz)       
    case (24) 
       sd = new_splitting_data (k, k**2, ewa_data(1)%mass**2, ewa_data(1)%mw)    
    end select   
    call strfun (f, fp, fm, fL, x, xb, r(1), energy (k), ewa_data(1), no_map)
    call interaction_set_flavored_values &
         (int, cmplx (f, kind=default), ewa_data%flv, 2)
    call splitting_set_t_bounds (sd, x(1), xb(1))
    call splitting_narrow_t_bounds (sd, ewa_data(1)%q_min, ewa_data(1)%pt_max)
    select case (size (r))
     case (1)
        call splitting_set_collinear (sd)
     case (3)
        call splitting_sample_t (sd, r(2)) 
        call splitting_sample_phi (sd, r(3))
     case default
        print *, "n_rand = ", size (r)
        call msg_bug (" EWA: number of random numbers must be 1 or 3")
     end select
     k_split = split_momentum (k, sd)
     if (ewa_data(1)%keep_momentum) then 
        call on_shell (k_split(1), 0._default, KEEP_MOMENTUM)
        select case (ewa_data(1)%id) 
        case (23)
           call on_shell (k_split(2), ewa_data(1)%mz, KEEP_MOMENTUM)       
        case (24)
           call on_shell (k_split(2), ewa_data(1)%mw, KEEP_MOMENTUM)              
        end select
     else if (ewa_data(1)%keep_energy) then   
        call on_shell (k_split(1), 0._default, KEEP_ENERGY)
        select case (ewa_data(1)%id) 
        case (23)
           call on_shell (k_split(2), ewa_data(1)%mz, KEEP_ENERGY)       
        case (24)
           call on_shell (k_split(2), ewa_data(1)%mw, KEEP_ENERGY)                    
        end select        
     end if               
     call interaction_set_momenta &
          (int, k_split, outgoing=.true.)
  end subroutine interaction_apply_ewa


end module sf_ewa
