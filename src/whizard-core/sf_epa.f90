! WHIZARD 2.0.4 Tue Oct 26 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

module sf_epa

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants, only: pi !NODEP!
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

  public :: epa_data_t
  public :: epa_data_init
  public :: epa_data_check
  public :: epa_data_write
  public :: interaction_init_epa
  public :: interaction_apply_epa

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_QMIN = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: ZERO_XMIN = 3

  type :: epa_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv
     real(default) :: alpha
     real(default) :: x_min
     real(default) :: x_max
     real(default) :: q_min
     real(default) :: q_max
     real(default) :: E_max
     real(default) :: mass
     real(default) :: log
     real(default) :: c0
     real(default) :: c1
     integer :: error = 0
  end type epa_data_t


contains

  subroutine epa_data_init (data, model, flv, alpha, x_min, q_min, E_max, mass)
    type(epa_data_t), intent(inout) :: data
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: alpha, x_min, q_min, E_max
    real(default), intent(in), optional :: mass
    data%model => model
    data%flv = flv
    data%alpha = alpha
    data%E_max = E_max
    data%x_min = x_min
    data%x_max = 1
    if (data%x_min == 0) then
       data%error = ZERO_XMIN;  return
    end if
    data%q_min = q_min
    data%q_max = 2 * data%E_max
    select case (char (model_get_name (data%model)))
    case ("QCD","Test")
       call msg_fatal ("EPA structure function not available for model " &
            // char (model_get_name (data%model)) // ".")
    end select     
    if (present (mass)) then
       data%mass = mass
    else
       data%mass = flavor_get_mass (flv)
    end if
    if (max (data%mass, data%q_min) == 0) then
       data%error = ZERO_QMIN;  return
    else if (max (data%mass, data%q_min) >= data%E_max) then
       data%error = Q_MAX_TOO_SMALL;  return
    end if
    data%log = log (4 * (data%E_max / max (data%mass, data%q_min)) ** 2 )
    data%c0 = data%alpha / pi &
         * flavor_get_charge (data%flv)**2 &
         * log (data%x_min) * (data%log - log (data%x_min))
    data%c1 = data%alpha / pi &
         * flavor_get_charge (data%flv)**2 &
         * log (data%x_max) * (data%log - log (data%x_max))
  end subroutine epa_data_init

  subroutine epa_data_check (data)
    type(epa_data_t), intent(in) :: data
    select case (data%error)
    case (ZERO_QMIN)
       call msg_fatal (" EPA: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal (" EPA: Particle mass exceeds Qmax")
    case (ZERO_XMIN)
       call msg_fatal (" EPA: x_min must be larger than zero")
    end select
  end subroutine epa_data_check

  subroutine epa_data_write (data, unit)
    type(epa_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "EPA data:"
    write (u, *) "  prt   = ", char (flavor_get_name (data%flv))
    write (u, *) "  alpha = ", data%alpha
    write (u, *) "  x_min = ", data%x_min
    write (u, *) "  x_max = ", data%x_max
    write (u, *) "  q_min = ", data%q_min
    write (u, *) "  q_max = ", data%q_max
    write (u, *) "  E_max = ", data%q_max
    write (u, *) "  mass  = ", data%mass
    write (u, *) "  log   = ", data%log
    write (u, *) "  c0    = ", data%c0
    write (u, *) "  c1    = ", data%c1
  end subroutine epa_data_write

  subroutine interaction_init_epa (int, data)
    type(interaction_t), intent(out) :: int
    type(epa_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel
    type(flavor_t) :: flv_photon
    type(quantum_numbers_t) :: qn_photon, qn
    type(state_iterator_t) :: it_hel
    mask = new_quantum_numbers_mask (.false., .false., &
         mask_h = (/ .false., .false., .true. /))
    lock = (/ 2, 1, 0 /)
    call interaction_init &
         (int, 1, 0, 2, mask=mask, lock=lock, set_relations=.true.)
    call flavor_init (flv_photon, PHOTON, data%model)
    call quantum_numbers_init (qn_photon, flv_photon)
    call polarization_init_generic (pol, data%flv)
    call quantum_numbers_init (qn_fc(1), &
         flv = data%flv, col = color_from_flavor (data%flv))
    call state_iterator_init (it_hel, pol%state)
    do while (state_iterator_is_valid (it_hel))
       qn_hel = state_iterator_get_quantum_numbers (it_hel)
       qn = qn_hel(1) .merge. qn_fc(1)
       call interaction_add_state (int, (/ qn, qn, qn_photon /))
       call state_iterator_advance (it_hel)
    end do
    call polarization_final (pol)
    call interaction_freeze (int)
  end subroutine interaction_init_epa
    
  elemental subroutine strfun (f, x, xb, r, E, data, no_map)
    real(default), intent(out) :: f, x, xb
    real(default), intent(in) :: r, E
    type(epa_data_t), intent(in) :: data
    logical, intent(in) :: no_map
    real(default) :: rb, lx0, lx1, lx, d, den
    real(default) :: qmaxsq, qminsq
    f = 0
    rb = 1 - r
    if (no_map) then
       x = r
       if (data%x_min < x .and. x < data%x_max) then
          den = (log (data%x_max) * (data%log - log (data%x_max)) &
               - log (data%x_min) * (data%log - log (data%x_min))) / x
       else
          den = 0
       end if
    else
       lx0 = log (data%x_min)
       lx1 = log (data%x_max)
       d = data%log ** 2 &
            -  4 * (r * lx1 * (data%log - lx1) + rb * lx0 * (data%log - lx0))
       if (d <= 0) then
          return
       else
          lx = (data%log - sqrt (d)) / 2
       end if
       x = exp (lx)
       den = data%log - 2 * lx
    end if
    if (den <= 0)  return
    xb = 1 - x
    qminsq = max (x ** 2 / xb * data%mass ** 2, data%q_min ** 2)
    qmaxsq = min (4 * E ** 2, data%q_max ** 2)
    if (qminsq < qmaxsq) then
       f = ((xb + x ** 2 / 2) * log (qmaxsq / qminsq) &
            - (1 - x / 2) ** 2 &
              * log ((x**2 + qmaxsq / E ** 2) / (x**2 + qminsq / E ** 2)) &
            - x ** 2 * data%mass ** 2 / qminsq * (1 - qminsq / qmaxsq)) &
           * ( data%c1 - data%c0 ) / den
    end if
  end subroutine strfun

  subroutine interaction_apply_epa (int, r, epa_data, no_map)
    type(interaction_t), intent(inout) :: int
    real(default), dimension(:), intent(in) :: r
    type(epa_data_t), dimension(:), intent(in) :: epa_data
    logical, intent(in) :: no_map
    type(vector4_t) :: k
    type(splitting_data_t) :: sd
    real(default), dimension(size(epa_data)) :: f, x, xb
    k = interaction_get_momentum (int, 1)
    sd = new_splitting_data (k, 0._default, epa_data(1)%mass**2, 0._default)
    call strfun (f, x, xb, r(1), energy (k), epa_data, no_map)
    call interaction_set_flavored_values &
         (int, cmplx (f, kind=default), epa_data%flv, 2)
    call splitting_set_t_bounds (sd, x(1), xb(1))
    call splitting_narrow_t_bounds (sd, epa_data(1)%q_min, epa_data(1)%q_max)
    select case (size (r))
    case (1)
       call splitting_set_collinear (sd)
    case (3)
       call splitting_sample_t (sd, r(2)) 
       call splitting_sample_phi (sd, r(3))
    case default
       print *, "n_rand = ", size (r)
       call msg_bug (" EPA: number of random numbers must be 1 or 3")
    end select
    call interaction_set_momenta &
         (int, split_momentum (k, sd), outgoing=.true.)
  end subroutine interaction_apply_epa


end module sf_epa
