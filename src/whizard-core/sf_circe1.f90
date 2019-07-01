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

module sf_circe1

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: CIRCE1_EPSILON !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux
  use circe1 !NODEP!

  implicit none
  private

  public :: circe1_data_t 
  public :: circe1_data_init 
  public :: circe1_data_check 
  public :: circe1_data_write 
  public :: interaction_init_circe1 
  public :: interaction_apply_circe1 

  integer, parameter :: NONE = 0
  integer, parameter :: FLV_INAPPLICABLE = 1

  type :: circe1_data_t 
     private 
     type(model_t), pointer :: model => null () 
     type(flavor_t), dimension(2) :: flv_in 
     integer, dimension(2) :: pdg_in
     real(default), dimension(2) :: m_in = 0
     logical, dimension(2) :: photon = .false.
     logical :: generate = .true.
     type(tao_random_state), pointer :: rng => null ()
     logical :: map = .true.
     real(default), dimension(2) :: beta = 0
     real(default), dimension(2) :: gamma = 0   
     real(default) :: sqrts = 0
     integer :: ver = 0 
     integer :: rev = 0      
     integer :: acc = 0      
     integer :: chat = 0    
     integer :: error = NONE 
  end type circe1_data_t 
 

  type(tao_random_state), pointer :: rng_tmp => null ()

contains

  subroutine circe1_data_init &
       (data, model, flv, sqrts, out_photon, generate, rng, map, &
        ver, rev, acc, chat)
    type(circe1_data_t), intent(out) :: data 
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts 
    logical, dimension(2), intent(in) :: out_photon
    logical, intent(in) :: generate, map
    type(tao_random_state), intent(in), target :: rng
    integer, intent(in) :: ver, rev, acc, chat 
    data%model => model 
    data%flv_in = flv 
    data%pdg_in = flavor_get_pdg (data%flv_in)
    if (any (abs (data%pdg_in) /= ELECTRON))  data%error = FLV_INAPPLICABLE
    if (data%pdg_in(1) /= - data%pdg_in(2))  data%error = FLV_INAPPLICABLE
    data%m_in = flavor_get_mass (data%flv_in)
    data%sqrts = sqrts
    data%photon = out_photon
    data%generate = generate
    data%rng => rng
    data%map = map
    data%ver = ver
    data%rev = rev
    data%acc = acc 
    data%chat = chat 
    select case (char (model_get_name (data%model)))
    case ("QCD","Test")
       call msg_fatal ("CIRCE1 structure function not available for model " &
            // char (model_get_name (data%model)) // ".")
    end select         
    call circes (0.d0, 0.d0, dble (data%sqrts), &
         data%acc, data%ver, data%rev, data%chat)
    call circe1_get_parameters (data%photon, data%beta, data%gamma)
  end subroutine circe1_data_init 
 
  elemental subroutine circe1_get_parameters (photon, beta, gamma)
    logical, intent(in) :: photon
    real(default), intent(out) :: beta, gamma
    if (photon) then
       beta = circe1_params%a1(6)
       gamma = circe1_params%a1(5)
    else
       beta = circe1_params%a1(2)
       gamma = circe1_params%a1(3)
    end if
  end subroutine circe1_get_parameters
    
  subroutine circe1_data_check (data) 
    type(circe1_data_t), intent(in) :: data 
    select case (data%error) 
    case (FLV_INAPPLICABLE)
       call msg_fatal ("CIRCE1: applicable only for incoming " &
            // "electron or positron")
    end select 
  end subroutine circe1_data_check 
 
  subroutine circe1_data_write (data, unit, md5)
    type(circe1_data_t), intent(in) :: data 
    integer, intent(in), optional :: unit 
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return 
    write (u, *) "CIRCE1 data:" 
    write (u, *) "  prt_in  = ", char (flavor_get_name (data%flv_in(1))), &
         ", ", char (flavor_get_name (data%flv_in(2)))
    write (u, *) "  photon  = ", data%photon
    write (u, *) "  generate = ", data%generate
    write (u, *) "  map      = ", data%map
    write (u, *) "  beta  = ", data%beta 
    write (u, *) "  gamma = ", data%gamma 
    write (u, *) "  m_in  = ", data%m_in
    write (u, *) "  sqrts = ", data%sqrts 
    write (u, *) "  ver = ", data%ver 
    write (u, *) "  rev = ", data%rev 
    write (u, *) "  acc = ", data%acc 
    write (u, *) "  chat = ", data%chat 
  end subroutine circe1_data_write 
 
  subroutine interaction_init_circe1 (int, circe1_data) 
    type(interaction_t), intent(out) :: int 
    type(circe1_data_t), intent(in) :: circe1_data 
    logical, dimension(6) :: mask_h
    type(quantum_numbers_mask_t), dimension(6) :: mask 
    integer, dimension(6) :: hel_lock 
    type(polarization_t) :: pol1, pol2
    type(quantum_numbers_t), dimension(1) :: qn_fc1, qn_hel1, qn_fc2, qn_hel2
    type(flavor_t) :: flv_photon
    type(quantum_numbers_t) :: qn_photon, qn1, qn2
    type(quantum_numbers_t), dimension(6) :: qn
    type(state_iterator_t) :: it_hel1, it_hel2
    hel_lock = 0
    mask_h = .false.
    if (circe1_data%photon(1)) then
       hel_lock(1) = 3;  hel_lock(3) = 1;  mask_h(5) = .true.
    else
       hel_lock(1) = 5;  hel_lock(5) = 1;  mask_h(3) = .true.
    end if
    if (circe1_data%photon(2)) then
       hel_lock(2) = 4;  hel_lock(4) = 2;  mask_h(6) = .true.
    else
       hel_lock(2) = 6;  hel_lock(6) = 2;  mask_h(4) = .true.
    end if
    mask = new_quantum_numbers_mask (.false., .false., mask_h)
    call interaction_init & 
         (int, 2, 0, 4, mask=mask, hel_lock=hel_lock, set_relations=.true.) 
    call flavor_init (flv_photon, PHOTON, circe1_data%model)
    call quantum_numbers_init (qn_photon, flv_photon)
    call polarization_init_generic (pol1, circe1_data%flv_in(1))
    call quantum_numbers_init (qn_fc1(1), flv = circe1_data%flv_in(1))
    call polarization_init_generic (pol2, circe1_data%flv_in(2))
    call quantum_numbers_init (qn_fc2(1), flv = circe1_data%flv_in(2))
    call state_iterator_init (it_hel1, pol1%state) 
    do while (state_iterator_is_valid (it_hel1)) 
       qn_hel1 = state_iterator_get_quantum_numbers (it_hel1)
       qn1 = qn_hel1(1) .merge. qn_fc1(1) 
       qn(1) = qn1
       if (circe1_data%photon(1)) then
          qn(3) = qn1;  qn(5) = qn_photon
       else
          qn(3) = qn_photon;  qn(5) = qn1
       end if
       call state_iterator_init (it_hel2, pol2%state) 
       do while (state_iterator_is_valid (it_hel2)) 
          qn_hel2 = state_iterator_get_quantum_numbers (it_hel2) 
          qn2 = qn_hel2(1) .merge. qn_fc2(1) 
          qn(2) = qn2
          if (circe1_data%photon(2)) then
             qn(4) = qn2;  qn(6) = qn_photon
          else
             qn(4) = qn_photon;  qn(6) = qn2
          end if
          call interaction_add_state (int, qn)
          call state_iterator_advance (it_hel2) 
       end do 
       call state_iterator_advance (it_hel1)
    end do 
    call polarization_final (pol1)
    call polarization_final (pol2)
    call interaction_freeze (int) 
  end subroutine interaction_init_circe1 

  subroutine strfun (f, x, xb, r, circe1_data, no_map)
    real(default), intent(out) :: f
    real(default), dimension(2), intent(out) :: x, xb
    real(default), dimension(2), intent(in) :: r
    type(circe1_data_t), intent(in) :: circe1_data
    logical, intent(in) :: no_map
    real(kind=default), parameter :: eps = CIRCE1_EPSILON
    real(default), dimension(2) :: fi
    if (circe1_data%generate .and. .not. no_map) then
       call circe_generate &
            (x, circe1_data%rng, circe1_data%pdg_in < 0, circe1_data%photon)
       xb = 1 - x
       f = 1
    else if (circe1_data%map .and. .not. no_map) then
       call circe_map_p (fi, x, xb, r, eps, &
            circe1_data%beta, circe1_data%gamma, circe1_data%photon)
       f = product (fi) * strfun_circe1 (x, circe1_data%pdg_in)
    else
       x  = r
       xb = 1 - r
       f = strfun_circe1 (x, circe1_data%pdg_in)
    end if
  end subroutine strfun
 
  subroutine rn_sub (r)
    double precision, intent(out) :: r
    real(default) :: x
    call tao_random_number (rng_tmp, x)
    r = x
  end subroutine rn_sub

  subroutine circe_generate (x, rng, anti, photon)
    logical, dimension(2), intent(in) :: anti
    logical, dimension(2), intent(in) :: photon
    real(default), dimension(2), intent(out) :: x
    real(double), dimension(2) :: xdum
    type(tao_random_state), intent(in), target :: rng
    rng_tmp => rng    
    if (all (photon)) then
       call gircgg (xdum(1), xdum(2), rn_sub)
    else if (photon(2)) then
       call girceg (xdum(1), xdum(2), rn_sub)
    else if (photon(1)) then
       call girceg (xdum(2), xdum(1), rn_sub)
    else if (.not. anti(1) .and. anti(2)) then
       call gircee (xdum(1), xdum(2), rn_sub)
    else if (anti(1) .and. .not. anti(2)) then
       call gircee (xdum(2), xdum(1), rn_sub)
    else
       call msg_bug ("CIRCE1: impossible flavor assigment")
    end if
    x = xdum
    rng_tmp => null ()
  end subroutine circe_generate
    
  elemental subroutine circe_map_p (f, x, xb, r, eps, beta, gamma, photon)
    real(default), intent(out) :: f, x, xb
    real(default), intent(in) :: r
    real(default), intent(in) :: eps, beta, gamma
    logical, intent(in) :: photon
    real(default) :: rb
    if (photon) then
       rb = 1 - r
       call circe_map_s (f, xb, rb, 1-eps, beta, gamma)
       x = 1 - xb
    else
       call circe_map_s (f, x, r, 1-eps, beta, gamma)
       xb = 1 - x
    end if
  end subroutine circe_map_p

  pure subroutine circe_map_s (f, x, z, x0, beta, gamma)
    real(default), intent(out) :: f, x
    real(default), intent(in) :: z, x0, beta, gamma
    real(default) :: y, ay, az, y0, z0
    call circe_set_const (ay, y0, x0, gamma, invert=.true.)
    call circe_set_const (az, z0, y0, beta, invert=.false.)
    y = circe_map_x (z, z0, y0, az, beta, invert=.false.)
    x = circe_map_x (y, y0, x0, ay, gamma, invert=.true.)
    f = 1 / circe_jacobian (x, x0, ay, gamma, invert=.true.) &
          / circe_jacobian (y, y0, az, beta, invert=.false.)
  end subroutine circe_map_s
    
  pure function circe_map_x (y, y0, x0, a, power, invert) result (x)
    real(kind=default), intent(in) :: y, y0, x0, a, power
    logical, intent(in) :: invert
    real(kind=default) :: x
    if (y<=0) then
       x = 0
    else if (y>1) then
       x = 1
    else if (invert) then
       if (y<y0) then
          x = 1 - (1 - (1+power)*y/a)**(1/(1+power))
       else
          x = x0 + (y-y0)/(a*(1-x0)**power)
       end if
    else
       if (y<y0) then
          x = ((1+power)*y/a)**(1/(1+power))
       else
          x = x0 + (y-y0)/(a*x0**power)
       end if
    end if
  end function circe_map_x

  pure subroutine circe_set_const (a, y0, x0, power, invert)
    real(default), intent(out) :: a, y0
    real(default), intent(in) :: x0, power
    logical, intent(in) :: invert
    real(default) :: tmp
    if (invert) then
       tmp = (1 - (1-x0)**(1+power)) / (1+power)
       a = 1 / (tmp + (1-x0)**(1+power))
    else
       tmp = x0**(1+power) / (1+power)
       a = 1 / (tmp + (1-x0) * x0**power)
    end if
    y0 = a * tmp
  end subroutine circe_set_const

  pure function circe_jacobian (x, x0, a, power, invert) result (dy_dx)
    real(kind=default), intent(in) :: x, x0, a, power
    logical, intent(in) :: invert
    real(kind=default) :: dy_dx
    if (x<=0) then
       dy_dx = a
    else if (invert) then
       if (x<x0) then
          dy_dx = a * (1-x)**power
       else
          dy_dx = a * (1-x0)**power
       end if
    else
       if (x<x0) then
          dy_dx = a * x**power
       else
          dy_dx = a * x0**power
       end if
    end if
  end function circe_jacobian

  function strfun_circe1 (x, pdg) result (f)
    real(default) :: f
    real(default), dimension(2), intent(in) :: x
    integer, dimension(2), intent(in) :: pdg
    if (all (x /= 0)) then
       f = kirke (dble (x(1)), dble (x(2)), pdg(1), pdg(2))
    else
       f = 0
    end if
  end function strfun_circe1

  subroutine interaction_apply_circe1 (int, r, circe1_data, no_map) 
    type(interaction_t), intent(inout) :: int 
    real(default), dimension(2), intent(in) :: r 
    type(circe1_data_t), intent(in) :: circe1_data 
    logical, intent(in) :: no_map
    type(vector4_t), dimension(2) :: k
    type(splitting_data_t), dimension(2) :: sd
    real(default), dimension(2) :: m_in, x, xb
    real(default) :: f
    type(vector4_t), dimension(4) :: q
    k(1) = interaction_get_momentum (int, 1) 
    k(2) = interaction_get_momentum (int, 2) 
    m_in = circe1_data%m_in
    sd = new_splitting_data (k, m_in**2, m_in**2, m_in) 
    call strfun (f, x, xb, r, circe1_data, no_map)
    call interaction_set_matrix_element (int, cmplx (f, kind=default)) 
    call splitting_set_t_bounds (sd, x, xb) 
    call splitting_set_collinear (sd)
    q((/1, 3/)) = split_momentum (k(1), sd(1))
    q((/2, 4/)) = split_momentum (k(2), sd(2))
    call interaction_set_momenta (int, q, outgoing=.true.) 
  end subroutine interaction_apply_circe1 
 

end module sf_circe1
