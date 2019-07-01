! WHIZARD 2.0.6 Wed Dec 7 2011
! 
! Copyright (C) 1999-2011 by 
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

module polarizations

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants, only: imago !NODEP!
  use file_utils !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices

  implicit none
  private

  public :: polarization_t
  public :: polarization_final
  public :: polarization_write
  public :: assignment(=)
  public :: polarization_write_raw
  public :: polarization_read_raw
  public :: polarization_is_polarized
  public :: polarization_is_diagonal
  public :: polarization_init_state_matrix
  public :: polarization_init_unpolarized
  public :: polarization_init_trivial
  public :: polarization_init_circular
  public :: polarization_init_transversal
  public :: polarization_init_axis
  public :: polarization_init_angles
  public :: polarization_init_longitudinal
  public :: polarization_init_diagonal
  public :: polarization_init_generic
  public :: combine_polarization_states
  public :: polarization_get_axis
  public :: polarization_to_angles
  public :: polarization_test

  type :: polarization_t
     logical :: polarized = .false.
     integer :: spin_type = 0
     integer :: multiplicity = 0
     type(state_matrix_t) :: state
  end type polarization_t


  interface assignment(=)
     module procedure polarization_assign
  end interface

  interface polarization_is_diagonal
     module procedure polarization_is_diagonal0
     module procedure polarization_is_diagonal1
  end interface


contains

  elemental subroutine polarization_init (pol, flv)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    pol%spin_type = flavor_get_spin_type (flv)
    pol%multiplicity = flavor_get_multiplicity (flv)
    call state_matrix_init (pol%state, store_values = .true.)
  end subroutine polarization_init
    
  elemental subroutine polarization_final (pol)
    type(polarization_t), intent(inout) :: pol
    call state_matrix_final (pol%state)
  end subroutine polarization_final

  subroutine polarization_write (pol, unit)
    type(polarization_t), intent(in) :: pol
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A,I1,A,I1,A)")  &
         "Polarization: [spin_type = ", pol%spin_type, &
         ", mult = ", pol%multiplicity, "]"
    call state_matrix_write (pol%state, unit=unit)
  end subroutine polarization_write

  subroutine polarization_assign (pol_out, pol_in)
    type(polarization_t), intent(out) :: pol_out
    type(polarization_t), intent(in) :: pol_in
    pol_out%polarized = pol_in%polarized
    pol_out%spin_type = pol_in%spin_type
    pol_out%multiplicity = pol_in%multiplicity
    pol_out%state = pol_in%state
  end subroutine polarization_assign

  subroutine polarization_write_raw (pol, u)
    type(polarization_t), intent(in) :: pol
    integer, intent(in) :: u
    write (u) pol%polarized
    write (u) pol%spin_type
    write (u) pol%multiplicity
    call state_matrix_write_raw (pol%state, u)
  end subroutine polarization_write_raw

  subroutine polarization_read_raw (pol, u, iostat)
    type(polarization_t), intent(out) :: pol
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    read (u, iostat=iostat) pol%polarized
    read (u, iostat=iostat) pol%spin_type
    read (u, iostat=iostat) pol%multiplicity
    call state_matrix_read_raw (pol%state, u, iostat=iostat)
  end subroutine polarization_read_raw

  elemental function polarization_is_polarized (pol) result (polarized)
    logical :: polarized
    type(polarization_t), intent(in) :: pol
    polarized = pol%polarized
  end function polarization_is_polarized

  function polarization_is_diagonal0 (pol) result (diagonal)
    logical :: diagonal
    type(polarization_t), intent(in) :: pol
    type(state_iterator_t) :: it
    diagonal = .true.
    call state_iterator_init (it, pol%state)
    do while (state_iterator_is_valid (it))
       diagonal = all (quantum_numbers_are_diagonal &
            (state_iterator_get_quantum_numbers (it)))
       if (.not. diagonal) exit
       call state_iterator_advance (it)
    end do
  end function polarization_is_diagonal0

  function polarization_is_diagonal1 (pol) result (diagonal)
    type(polarization_t), dimension(:), intent(in) :: pol
    logical, dimension(size(pol)) :: diagonal
    integer :: i
    do i = 1, size (pol)
       diagonal(i) = polarization_is_diagonal0 (pol(i))
    end do
  end function polarization_is_diagonal1

  subroutine polarization_init_state_matrix (pol, state)
    type(polarization_t), intent(out) :: pol
    type(state_matrix_t), intent(in), target :: state
    type(state_iterator_t) :: it
    type(flavor_t) :: flv
    type(helicity_t) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    complex(default) :: value, t
    call state_iterator_init (it, state)
    flv = state_iterator_get_flavor (it, 1)
    hel = state_iterator_get_helicity (it, 1)
    if (helicity_is_defined (hel)) then
       call polarization_init (pol, flv)
       pol%polarized = .true.
       t = 0
       do while (state_iterator_is_valid (it))
          hel = state_iterator_get_helicity (it, 1)
          call quantum_numbers_init (qn(1), hel)
          value = state_iterator_get_matrix_element (it)
          call state_matrix_add_state (pol%state, qn, value=value)
          if (helicity_is_diagonal (hel))  t = t + value
          call state_iterator_advance (it)
       end do
       call state_matrix_freeze (pol%state)
       if (t /= 0)  call state_matrix_renormalize (pol%state, 1._default / t)
    else
       call polarization_init_unpolarized (pol, flv)
    end if
  end subroutine polarization_init_state_matrix

  subroutine polarization_init_unpolarized (pol, flv)
    type(polarization_t), intent(inout) :: pol
    type(flavor_t), intent(in) :: flv
    type(quantum_numbers_t), dimension(1) :: qn
    complex(default) :: value
    if (flavor_is_left_handed (flv)) then
       call polarization_init_circular (pol, flv, -1._default)
    else if (flavor_is_right_handed (flv)) then
       call polarization_init_circular (pol, flv, 1._default)
    else
       call polarization_init (pol, flv)
       value = 1._default / flavor_get_multiplicity (flv)
       call state_matrix_add_state (pol%state, qn)
       call state_matrix_freeze (pol%state)
       call state_matrix_set_matrix_element (pol%state, value)
    end if
  end subroutine polarization_init_unpolarized
    
  subroutine polarization_init_trivial (pol, flv, fraction)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), intent(in), optional :: fraction
    type(helicity_t) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    integer :: h, hmax
    logical :: fermion
    complex(default) :: value
    call polarization_init (pol, flv)
    pol%polarized = .true.
    if (present (fraction)) then
       value = fraction / pol%multiplicity
    else
       value = 1._default / pol%multiplicity
    end if
    fermion = mod (pol%spin_type, 2) == 0
    hmax = pol%spin_type / 2
    select case (pol%multiplicity)
    case (1)
       if (flavor_is_left_handed (flv)) then
          call helicity_init (hel, -hmax)
       else if (flavor_is_right_handed (flv)) then
          call helicity_init (hel, hmax)
       else
          call helicity_init (hel, 0)
       end if
       call quantum_numbers_init (qn(1), hel)
       call state_matrix_add_state (pol%state, qn)
    case (2)
       do h = -hmax, hmax, 2*hmax
          call helicity_init (hel, h)
          call quantum_numbers_init (qn(1), hel)
          call state_matrix_add_state (pol%state, qn)
       end do
    case default
       do h = -hmax, hmax
          if (fermion .and. h == 0)  cycle
          call helicity_init (hel, h)
          call quantum_numbers_init (qn(1), hel)
          call state_matrix_add_state (pol%state, qn)
       end do
    end select
    call state_matrix_freeze (pol%state)
    call state_matrix_set_matrix_element (pol%state, value)
  end subroutine polarization_init_trivial

  subroutine polarization_init_circular (pol, flv, fraction)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: fraction
    type(helicity_t), dimension(2) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    complex(default) :: value
    integer :: hmax
    call polarization_init (pol, flv)
    pol%polarized = .true.
    hmax = pol%spin_type / 2
    call helicity_init (hel(1), hmax)
    call helicity_init (hel(2),-hmax)
    if (abs (fraction) /= 1) then
       value = (1 + fraction) / 2
       call quantum_numbers_init (qn(1), hel(1))
       call state_matrix_add_state (pol%state, qn, value=value)
       value = (1 - fraction) / 2
       call quantum_numbers_init (qn(1), hel(2))
       call state_matrix_add_state (pol%state, qn, value=value)
    else
       value = abs (fraction)
       if (fraction > 0) then
          call quantum_numbers_init (qn(1), hel(1))
       else
          call quantum_numbers_init (qn(1), hel(2))
       end if
       call state_matrix_add_state (pol%state, qn, value=value)
    end if
    call state_matrix_freeze (pol%state)
  end subroutine polarization_init_circular

  subroutine polarization_init_transversal (pol, flv, phi, fraction)
    type(polarization_t), intent(inout) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: phi, fraction
    call polarization_init_axis &
         (pol, flv, fraction * (/ cos (phi), sin (phi), 0._default/))
  end subroutine polarization_init_transversal

  subroutine polarization_init_axis (pol, flv, alpha)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), dimension(3), intent(in) :: alpha
    type(quantum_numbers_t), dimension(1) :: qn
    type(helicity_t), dimension(2,2) :: hel
    complex(default), dimension(2,2) :: value
    integer :: hmax
    call polarization_init (pol, flv)
    pol%polarized = .true.
    hmax = pol%spin_type / 2
    call helicity_init (hel(1,1), hmax, hmax)
    call helicity_init (hel(1,2), hmax,-hmax)
    call helicity_init (hel(2,1),-hmax, hmax)
    call helicity_init (hel(2,2),-hmax,-hmax)
    value(1,1) = (1 + alpha(3)) / 2
    value(2,2) = (1 - alpha(3)) / 2
    if (flavor_is_antiparticle (flv)) then
       value(1,2) = (alpha(1) + imago * alpha(2)) / 2
    else
       value(1,2) = (alpha(1) - imago * alpha(2)) / 2
    end if
    value(2,1) = conjg (value(1,2))
    if (value(1,1) /= 0) then
       call quantum_numbers_init (qn(1), hel(1,1))
       call state_matrix_add_state (pol%state, qn, value=value(1,1))
    end if
    if (value(2,2) /= 0) then
       call quantum_numbers_init (qn(1), hel(2,2))
       call state_matrix_add_state (pol%state, qn, value=value(2,2))
    end if
    if (value(1,2) /= 0) then
       call quantum_numbers_init (qn(1), hel(1,2))
       call state_matrix_add_state (pol%state, qn, value=value(1,2))
       call quantum_numbers_init (qn(1), hel(2,1))
       call state_matrix_add_state (pol%state, qn, value=value(2,1))
    end if
    call state_matrix_freeze (pol%state)
  end subroutine polarization_init_axis

  subroutine polarization_init_angles (pol, flv, r, theta, phi)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: r, theta, phi
    real(default), dimension(3) :: alpha
    real(default), parameter :: eps = 10 * epsilon (1._default)
    alpha(1) = r * sin (theta) * cos (phi)
    alpha(2) = r * sin (theta) * sin (phi)
    alpha(3) = r * cos (theta)
    where (abs (alpha) < eps)  alpha = 0
    call polarization_init_axis (pol, flv, alpha)
  end subroutine polarization_init_angles

  subroutine polarization_init_longitudinal (pol, flv, fraction)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) :: fraction
    integer :: spin_type, multiplicity
    type(helicity_t) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    complex(default) :: value
    integer :: n_values
    value = abs (fraction)
    spin_type = flavor_get_spin_type (flv)
    multiplicity = flavor_get_multiplicity (flv)
    if (mod (spin_type, 2) == 1 .and. multiplicity > 2) then
       if (fraction /= 1) then
          call polarization_init_trivial (pol, flv, 1 - fraction)
          n_values = state_matrix_get_n_matrix_elements (pol%state)
          call state_matrix_add_to_matrix_element &
               (pol%state, n_values/2 + 1, value)
       else
          call polarization_init (pol, flv)
          pol%polarized = .true.
          call helicity_init (hel, 0)
          call quantum_numbers_init (qn(1), hel)
          call state_matrix_add_state (pol%state, qn)
          call state_matrix_freeze (pol%state)
          call state_matrix_set_matrix_element (pol%state, value)
       end if
    else
       call polarization_init_unpolarized (pol, flv)
    end if
  end subroutine polarization_init_longitudinal

  subroutine polarization_init_diagonal (pol, flv, alpha)
    type(polarization_t), intent(inout) :: pol
    type(flavor_t), intent(in) :: flv
    real(default), dimension(:), intent(in) :: alpha
    type(helicity_t) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    logical, dimension(size(alpha)) :: mask
    real(default) :: norm
    complex(default), dimension(:), allocatable :: value
    logical :: fermion
    integer :: h, hmax, i
    mask = alpha > 0
    norm = sum (alpha, mask);  if (norm == 0)  norm = 1
    allocate (value (count (mask)))
    value = pack (alpha / norm, mask)
    call polarization_init (pol, flv)
    pol%polarized = .true.
    fermion = mod (pol%spin_type, 2) == 0
    hmax = pol%spin_type / 2
    i = 0
    select case (pol%multiplicity)
    case (1)
       if (flavor_is_left_handed (flv)) then
          call helicity_init (hel, -hmax)
       else if (flavor_is_right_handed (flv)) then
          call helicity_init (hel, hmax)
       else
          call helicity_init (hel, 0)
       end if
       call quantum_numbers_init (qn(1), hel)
       call state_matrix_add_state (pol%state, qn)
    case (2)
       do h = -hmax, hmax, 2*hmax
          i = i + 1
          if (mask(i)) then
             call helicity_init (hel, h)
             call quantum_numbers_init (qn(1), hel)
             call state_matrix_add_state (pol%state, qn)
          end if
       end do
    case default
       do h = -hmax, hmax
          if (fermion .and. h == 0)  cycle
          i = i + 1
          if (mask(i)) then
             call helicity_init (hel, h)
             call quantum_numbers_init (qn(1), hel)
             call state_matrix_add_state (pol%state, qn)
          end if
       end do
    end select
    call state_matrix_freeze (pol%state)
    call state_matrix_set_matrix_element (pol%state, value)
  end subroutine polarization_init_diagonal

  subroutine polarization_init_generic (pol, flv)
    type(polarization_t), intent(out) :: pol
    type(flavor_t), intent(in) :: flv
    type(helicity_t) :: hel
    type(quantum_numbers_t), dimension(1) :: qn
    logical :: fermion
    integer :: hmax, h1, h2
    call polarization_init (pol, flv)
    pol%polarized = .true.
    fermion = mod (pol%spin_type, 2) == 0
    hmax = pol%spin_type / 2
    select case (pol%multiplicity)
    case (1)
       if (flavor_is_left_handed (flv)) then
          call helicity_init (hel, -hmax)
       else if (flavor_is_right_handed (flv)) then
          call helicity_init (hel, hmax)
       else
          call helicity_init (hel, 0)
       end if
       call quantum_numbers_init (qn(1), hel)
       call state_matrix_add_state (pol%state, qn)
    case (2)
       do h1 = -hmax, hmax, 2*hmax
          do h2 = -hmax, hmax, 2*hmax
             call helicity_init (hel, h1, h2)
             call quantum_numbers_init (qn(1), hel)
             call state_matrix_add_state (pol%state, qn)
          end do
       end do
    case default
       do h1 = -hmax, hmax
          if (fermion .and. h1 == 0)  cycle
          do h2 = -hmax, hmax
             if (fermion .and. h2 == 0)  cycle
             call helicity_init (hel, h1, h2)
             call quantum_numbers_init (qn(1), hel)
             call state_matrix_add_state (pol%state, qn)
          end do
       end do
    end select
    call state_matrix_freeze (pol%state)
  end subroutine polarization_init_generic

  subroutine combine_polarization_states (pol, state)
    type(polarization_t), dimension(:), intent(in), target :: pol
    type(state_matrix_t), intent(out) :: state
    call outer_multiply (pol%state, state)
  end subroutine combine_polarization_states

  function polarization_get_axis (pol) result (alpha)
    real(default), dimension(3) :: alpha
    type(polarization_t), intent(in), target :: pol
    type(state_iterator_t) :: it
    complex(default), dimension(2,2) :: value
    type(helicity_t), dimension(2,2) :: hel
    type(helicity_t), dimension(1) :: hel1
    integer :: hmax, i, j
    if (pol%polarized) then
       hmax = pol%spin_type / 2
       call helicity_init (hel(1,1), hmax, hmax)
       call helicity_init (hel(1,2), hmax,-hmax)
       call helicity_init (hel(2,1),-hmax, hmax)
       call helicity_init (hel(2,2),-hmax,-hmax)
       value = 0
       call state_iterator_init (it, pol%state)
       do while (state_iterator_is_valid (it))
          hel1 = state_iterator_get_helicity (it)
          SCAN_HEL: do i = 1, 2
             do j = 1, 2
                if (hel1(1) == hel(i,j)) then
                   value(i,j) = state_iterator_get_matrix_element (it)
                   exit SCAN_HEL
                end if
             end do
          end do SCAN_HEL
          call state_iterator_advance (it)
       end do
       alpha(1) = value(1,2) + value(2,1)
       alpha(2) = imago * (value(1,2) - value(2,1))
       alpha(3) = value(1,1) - value(2,2)
    else
       alpha = 0
    end if
  end function polarization_get_axis

  subroutine polarization_to_angles (pol, r, theta, phi)
    type(polarization_t), intent(in) :: pol
    real(default), intent(out) :: r, theta, phi
    real(default), dimension(3) :: alpha
    real(default) :: r12
    if (pol%polarized) then
       alpha = polarization_get_axis (pol)
       r = sqrt (sum (alpha**2))
       if (any (alpha /= 0)) then
          r12 = sqrt (alpha(1)**2 + alpha(2)**2)
          theta = atan2 (r12, alpha(3))
          if (any (alpha(1:2) /= 0)) then
             phi = atan2 (alpha(2), alpha(1))
          else
             phi = 0
          end if
       else
          theta = 0
       end if
    else
       r = 0
       theta = 0
       phi = 0
    end if
  end subroutine polarization_to_angles

  subroutine polarization_test
    use os_interface, only: os_data_t
    type(os_data_t) :: os_data
    type(model_t), pointer :: model
    type(polarization_t) :: pol
    type(flavor_t) :: flv
    real(default), dimension(3) :: alpha
    real(default) :: r, theta, phi
    print *, "* Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)
    print *, "Unpolarized fermion"
    call flavor_init (flv, 1, model)
    call polarization_init_unpolarized (pol, flv)
    call polarization_write (pol)
    print *, "diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    print *, "Unpolarized fermion"
    call polarization_init_circular (pol, flv, 0._default)
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "Transversally polarized fermion, phi=0"
    call polarization_init_transversal (pol, flv, 0._default, 1._default)
    call polarization_write (pol)
    print *, "diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    print *, "Transversally polarized fermion, phi=0.9, frac=0.8"
    call polarization_init_transversal (pol, flv, 0.9_default, 0.8_default)
    call polarization_write (pol)
    print *, "diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    print *, "All polarization directions of a fermion"
    call polarization_init_generic (pol, flv)
    call polarization_write (pol)
    call polarization_final (pol)
    call flavor_init (flv, 21, model)
    print *, "Circularly polarized gluon, frac=0.3"
    call polarization_init_circular (pol, flv, 0.3_default)
    call polarization_write (pol)
    call polarization_final (pol)
    call flavor_init (flv, 23, model)
    print *, "Circularly polarized massive vector, frac=-0.7"
    call polarization_init_circular (pol, flv,  -0.7_default)
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "Circularly polarized massive vector"
    call polarization_init_circular (pol, flv, 1._default)
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "Longitudinally polarized massive vector, frac=0.4"
    call polarization_init_longitudinal (pol, flv, 0.4_default)
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "Longitudinally polarized massive vector"
    call polarization_init_longitudinal (pol, flv, 1._default)
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "Diagonally polarized massive vector"
    call polarization_init_diagonal &
         (pol, flv, (/0._default, 1._default, 2._default/))
    call polarization_write (pol)
    call polarization_final (pol)
    print *, "All polarization directions of a massive vector"
    call polarization_init_generic (pol, flv)
    call polarization_write (pol)
    call polarization_final (pol)
    call flavor_init (flv, 21, model)
    print *, "Axis polarization (0.2, 0.4, 0.6)"
    alpha = (/0.2_default, 0.4_default, 0.6_default/)
    call polarization_init_axis (pol, flv, alpha)
    call polarization_write (pol)
    print *, "Recovered axis:"
    alpha = polarization_get_axis (pol)
    print *, "Angle polarization (0.5, 0.6, -1)"
    r = 0.5_default
    theta = 0.6_default
    phi = -1._default
    call polarization_init_angles (pol, flv, r, theta, phi)
    call polarization_write (pol)
    print *, "Recovered parameters (r, theta, phi):"
    call polarization_to_angles (pol, r, theta, phi)
    print *, r, theta, phi
    call polarization_final (pol)
  end subroutine polarization_test


end module polarizations
