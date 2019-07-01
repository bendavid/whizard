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

module sf_isr

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants, only: pi !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use sm_physics, only: Li2 !NODEP!
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

  public :: isr_data_t
  public :: isr_data_init
  public :: isr_data_set_order
  public :: isr_data_check
  public :: isr_data_write
  public :: interaction_init_isr
  public :: interaction_apply_isr

  integer, parameter :: NONE = 0
  integer, parameter :: ZERO_MASS = 1
  integer, parameter :: Q_MAX_TOO_SMALL = 2
  integer, parameter :: EPS_TOO_LARGE = 3
  integer, parameter :: INVALID_ORDER = 4

  type :: isr_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t) :: flv
     real(default) :: alpha = 0
     real(default) :: q_max = 0
     real(default) :: real_mass = 0
     real(default) :: mass = 0
     real(default) :: eps = 0
     real(default) :: log = 0
     integer :: order = 3
     integer :: error = NONE
  end type isr_data_t


contains

  subroutine isr_data_init (data, model, flv, alpha, q_max, mass)
    type(isr_data_t), intent(out) :: data
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: alpha
    real(default), intent(in) :: q_max
    real(default), intent(in), optional :: mass
    data%model => model
    data%flv = flv
    data%alpha = alpha
    data%q_max = q_max
    data%real_mass = flavor_get_mass (flv)
    if (present (mass)) then
       if (mass > 0) then
          data%mass = mass
       else
          data%mass = data%real_mass
       end if
    else
       data%mass = data%real_mass
    end if
    if (data%mass == 0) then
       data%error = ZERO_MASS;  return
    else if (data%mass >= data%q_max) then
       data%error = Q_MAX_TOO_SMALL;  return
    end if
    data%log = log (1 + (data%q_max / data%mass)**2)
    data%eps = data%alpha / pi &
         * flavor_get_charge (data%flv)**2 &
         * (2 * log (data%q_max / data%mass) - 1)
    if (data%eps > 1) then
       data%error = EPS_TOO_LARGE;  return
    end if
  end subroutine isr_data_init

  elemental subroutine isr_data_set_order (data, order)
    type(isr_data_t), intent(inout) :: data
    integer, intent(in) :: order
    if (order < 0 .or. order > 3) then
       data%error = INVALID_ORDER
    else
       data%order = order
    end if
  end subroutine isr_data_set_order

  subroutine isr_data_check (data)
    type(isr_data_t), intent(in) :: data
    select case (data%error)
    case (ZERO_MASS)
       call msg_fatal (" ISR: Particle mass is zero")
    case (Q_MAX_TOO_SMALL)
       call msg_fatal (" ISR: Particle mass exceeds Qmax")
    case (EPS_TOO_LARGE)
       call msg_fatal (" ISR: Expansion parameter too large, perturbative expansion breaks down")
    case (INVALID_ORDER)
       call msg_error (" ISR: LLA order invalid (valid values are 0,1,2,3)")
    end select
  end subroutine isr_data_check

  subroutine isr_data_write (data, unit, md5)
    type(isr_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "ISR data:"
    write (u, *) "  prt    = ", char (flavor_get_name (data%flv))
    write (u, *) "  alpha  = ", data%alpha
    write (u, *) "  q_max  = ", data%q_max
    write (u, *) "  mass   = ", data%mass
    write (u, *) "  eps    = ", data%eps
    write (u, *) "  log    = ", data%log
    write (u, *) "  order  = ", data%order
  end subroutine isr_data_write

  subroutine interaction_init_isr (int, data)
    type(interaction_t), intent(out) :: int
    type(isr_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    integer, dimension(3) :: hel_lock
    type(polarization_t) :: pol
    type(quantum_numbers_t), dimension(1) :: qn_fc, qn_hel
    type(flavor_t) :: flv_photon
    type(quantum_numbers_t) :: qn_photon, qn
    type(state_iterator_t) :: it_hel
    mask = new_quantum_numbers_mask (.false., .false., &
         mask_h = (/ .false., .true., .false. /))
    hel_lock = (/ 3, 0, 1 /)
    call interaction_init &
         (int, 1, 0, 2, mask=mask, hel_lock=hel_lock, set_relations=.true.)
    call flavor_init (flv_photon, PHOTON, data%model)
    call quantum_numbers_init (qn_photon, flv_photon)
    call polarization_init_generic (pol, data%flv)
    call quantum_numbers_init (qn_fc(1), &
         flv = data%flv, col = color_from_flavor (data%flv))
    call state_iterator_init (it_hel, pol%state)
    do while (state_iterator_is_valid (it_hel))
       qn_hel = state_iterator_get_quantum_numbers (it_hel)
       qn = qn_hel(1) .merge. qn_fc(1)
       call interaction_add_state (int, (/ qn, qn_photon, qn /))
       call state_iterator_advance (it_hel)
    end do
    call polarization_final (pol)
    call interaction_freeze (int)
  end subroutine interaction_init_isr
    
  subroutine strfun (f, x, xb, r, data, no_map)
    real(default), intent(out) :: f, x, xb
    real(default), intent(in) :: r
    type(isr_data_t), intent(in) :: data
    logical, intent(in) :: no_map
    real(default) :: eps
    real(default) :: rb, log_x, log_xb, x_2
    real(default), parameter :: &
         & xmin = 0.00714053329734592839549810275603_default
    real(default), parameter :: &
         & zeta3 = 1.20205690315959428539973816151_default
    real(default), parameter :: &
         & g1 = 3._default / 4._default, &
         & g2 = (27 - 8*pi**2) / 96._default, &
         & g3 = (27 - 24*pi**2 + 128*zeta3) / 384._default
    eps = data%eps
    rb = 1 - r
    if (no_map) then
       xb = rb
       f = xb ** (-1 + eps)
    else
       if (rb < tiny(1._default)**eps) then
          xb = 0
       else
          xb = rb**(1/eps)
       end if
       f = 1
    end if
    x = 1 - xb
    if (data%order > 0) then
       f = f * (1 + g1 * eps)
       x_2 = x*x
       if (rb>0)  f = f * (1 - (1-x_2) / (2 * rb))
       if (data%order > 1) then
          f = f * (1 + g2 * eps**2)
          if (rb>0 .and. xb>0 .and. x>xmin) then
             log_x = log (x)
             log_xb = log (xb)
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
  end subroutine strfun

  subroutine interaction_apply_isr (int, r, isr_data, no_map)
    type(interaction_t), intent(inout) :: int
    real(default), dimension(:), intent(in) :: r
    type(isr_data_t), intent(in) :: isr_data
    logical, intent(in) :: no_map
    type(vector4_t) :: k
    type(splitting_data_t) :: sd
    real(default) :: f, x, xb
    k = interaction_get_momentum (int, 1)
    sd = new_splitting_data (k, isr_data%mass**2, 0._default, isr_data%mass)
    call strfun (f, x, xb, r(1), isr_data, no_map)
    call interaction_set_matrix_element (int, cmplx (f, kind=default))
    call splitting_set_t_bounds (sd, x, xb)
    select case (size (r))
    case (1)
       call splitting_set_collinear (sd)
    case (3)
       call splitting_sample_t (sd, r(2)) 
       call splitting_sample_phi (sd, r(3))
    case default
       print *, "n_rand = ", size (r)
       call msg_bug (" ISR: number of random numbers must be 1 or 3")
    end select
    call interaction_set_momenta &
         (int, split_momentum (k, sd), outgoing=.true.)
  end subroutine interaction_apply_isr


end module sf_isr
