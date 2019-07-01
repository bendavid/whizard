! compare_lib.f90 --
! compare_lib.f90 -- compare two O'Mega versions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
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

module compare_lib
  ! use ieee_arithmetic
  use kinds
  use constants
  use tao_random_numbers
  use omega95
  use omega_interface
  use omega_testtools
  implicit none
  private
  public :: check
contains

  elemental function ieee_is_nan (x) result (yorn)
    logical :: yorn
    real (kind=default), intent(in) :: x
    yorn = (x /= x)
  end function ieee_is_nan

  subroutine check (v1, v2, roots, threshold, n, &
                    failures, attempts, seed, abs_threshold)
    type(omega_procedures), intent(in) :: v1, v2
    real(kind=default), intent(in) :: roots, threshold
    integer, intent(in) :: n
    integer, intent(out) :: failures, attempts
    integer, intent(in), optional :: seed
    real(kind=default), intent(in), optional :: abs_threshold
    logical :: match, passed
    integer :: n_out, n_flv, n_hel, n_col
    integer :: i, i_flv, i_hel, i_col
    real(kind=default), dimension(:,:), allocatable :: p
    complex(kind=default) :: a1, a2
    real(kind=default) :: asq1, asq2, s_asq1, s_asq2
    character(len=80) :: msg
    failures = 0
    attempts = 0
    a1 = 0
    a2 = 0
    asq1 = 0
    asq2 = 0
    s_asq1 = 0
    s_asq2 = 0
    call quantum_numbers (v1, v2, n_out, n_flv, n_hel, n_col, match)
    if (.not.match) then
       failures = 1
       return
    end if
    if (present (seed)) then
       call tao_random_seed (seed)
    end if
    call v1%reset_helicity_selection (-1.0_default, -1)
    call v2%reset_helicity_selection (-1.0_default, -1)
    allocate (p(0:3,2+n_out))
    call beams (ROOTS, 0.0_default, 0.0_default, p(:,1), p(:,2))
    do i = 1, N
       if (n_out > 1) then
          call massless_isotropic_decay (ROOTS, p(:,3:))
       end if
       if (n_out == 1) then
          p(:,3) = p(:,1) + p(:,2)
       end if
       call v1%new_event (p)
       call v2%new_event (p)
       do i_flv = 1, n_flv
          do i_hel = 1, n_hel
             attempts = attempts + 1
             passed = .true.
             do i_col = 1, n_col
                a1 = v1%get_amplitude (i_flv, i_hel, i_col)
                a2 = v2%get_amplitude (i_flv, i_hel, i_col)
                if (ieee_is_nan (real (a1)) .or. ieee_is_nan (aimag (a1))) then
                   write (*, "(1X,'evt=',I5,', flv=',I3,', col=',I3,': ', A)") &
                        i, i_flv, i_col, "v1 amplitude NaN"
                end if
                if (ieee_is_nan (real (a2)) .or. ieee_is_nan (aimag (a2))) then
                   write (*, "(1X,'evt=',I5,', flv=',I3,', col=',I3,': ', A)") &
                        i, i_flv, i_col, "v2 amplitude NaN"
                end if
                write (msg, "(1X,'evt=',I5,', flv=',I3,', col=',I3,', hel=',I3)") &
                     i, i_flv, i_col, i_hel
                call expect (a1, a2, trim(msg), passed, &
                             quiet=.true., threshold=threshold, &
                             abs_threshold=abs_threshold)
             end do
             write (msg, "(1X,'evt=',I5,', flv=',I3,', hel=',I3)") &
                  i, i_flv, i_hel
             asq1 = v1%color_sum (i_flv, i_hel)
             s_asq1 = s_asq1 + asq1
             asq2 = v2%color_sum (i_flv, i_hel)
             s_asq2 = s_asq2 + asq2
             call expect (asq1, asq2, trim(msg), passed, &
                          quiet=.true., threshold=threshold, &
                          abs_threshold=abs_threshold)
             if (.not.passed) then
                failures = failures + 1
             end if
          end do
       end do
    end do
    print *, 'Summed results: '
    print *, 's_asq1, s_asq2 =    ', s_asq1, s_asq2
    deallocate (p)
  end subroutine check

  subroutine quantum_numbers (v1, v2, n_out, n_flv, n_hel, n_col, match)
    type(omega_procedures), intent(in) :: v1, v2
    integer, intent(out) :: n_out, n_flv, n_hel, n_col
    logical, intent(out) :: match
    integer, dimension(:,:), allocatable :: &
         v1_flavor_states, v2_flavor_states, &
         v1_spin_states, v2_spin_states
    integer, dimension(:,:,:), allocatable :: &
         v1_color_flows, v2_color_flows
    logical, dimension(:,:), allocatable :: &
         v1_ghost_flags, v2_ghost_flags
    type(omega_color_factor), dimension(:), allocatable :: &
         v1_color_factors, v2_color_factors
    integer :: n_in, n_prt, n_cix, n_cfs
    n_in = v1%number_particles_in ()
    n_out = v1%number_particles_out ()
    n_prt = n_in + n_out
    n_flv = v1%number_flavor_states ()
    n_hel = v1%number_spin_states ()
    n_cix = v1%number_color_indices ()
    n_col = v1%number_color_flows ()
    n_cfs = v1%number_color_factors ()
    match = .true.
    if (v2%number_particles_in () .ne. n_in) then
       print *, "number_particles_in don't match!"
       match = .false.
    end if
    if (v2%number_particles_out () .ne. n_out) then
       print *, "number_particles_out don't match!"
       match = .false.
    end if
    if (v2%number_flavor_states () .ne. n_flv) then
       print *, "number_flavor_states don't match!"
       match = .false.
    end if
    if (v2%number_spin_states () .ne. n_hel) then
       print *, "number_spin_states don't match!"
       match = .false.
    end if
    if (v2%number_color_indices () .ne. n_cix) then
       print *, "number_color_indices don't match!"
       match = .false.
    end if
    if (v2%number_color_flows () .ne. n_col) then
       print *, "number_color_flows don't match!"
       match = .false.
    end if
    ! We save only the symmetric part in the OVM
    !if (v2%number_color_factors () .ne. n_cfs) then
       !print *, "number_color_factors don't match!"
       !match = .false.
    !end if
    if (match) then
       allocate (v1_flavor_states(n_prt,n_flv), v2_flavor_states(n_prt,n_flv))
       allocate (v1_spin_states(n_prt,n_hel), v2_spin_states(n_prt,n_hel))
       allocate (v1_color_flows(n_cix,n_prt,n_col), &
                 v2_color_flows(n_cix,n_prt,n_col))
       allocate (v1_ghost_flags(n_prt,n_col), v2_ghost_flags(n_prt,n_col))
       !allocate (v1_color_factors(n_cfs), v2_color_factors(n_cfs))
       call v1%flavor_states (v1_flavor_states)
       call v2%flavor_states (v2_flavor_states)
       call v1%spin_states (v1_spin_states)
       call v2%spin_states (v2_spin_states)
       call v1%color_flows (v1_color_flows, v1_ghost_flags)
       call v2%color_flows (v2_color_flows, v2_ghost_flags)
       !call v1%color_factors (v1_color_factors)
       !call v2%color_factors (v2_color_factors)
       if (any (v1_flavor_states .ne. v2_flavor_states)) then
          print *, "flavor states don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       if (any (v1_spin_states .ne. v2_spin_states)) then
          print *, "spin states don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       if (any (v1_color_flows .ne. v2_color_flows)) then
          print *, "color flows don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       if (any (v1_ghost_flags .neqv. v2_ghost_flags)) then
          print *, "ghost flags don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       !if (any (.not. color_factors_equal (v1_color_factors, &
                                           !v2_color_factors))) then
          !print *, "color_factors don't match!"
          !print *, "CAVEAT: this might be due to simple reordering!"
          !match = .false.
       !end if
       deallocate (v1_flavor_states, v2_flavor_states)
       deallocate (v1_spin_states, v2_spin_states)
       deallocate (v1_color_flows, v2_color_flows)
       deallocate (v1_ghost_flags, v2_ghost_flags)
       !deallocate (v1_color_factors, v2_color_factors)
    end if
  end subroutine quantum_numbers

  elemental function color_factors_equal (cf1, cf2) result (eq)
    logical :: eq
    type(omega_color_factor), intent(in) :: cf1, cf2
    eq = (cf1%i1 .eq. cf2%i1) .and. (cf1%i2 .eq. cf2%i2) .and. (cf1%factor .eq. cf2%factor)
  end function color_factors_equal

  pure function dot (p, q) result (pq)
    real(kind=default), dimension(0:), intent(in) :: p, q
    real(kind=default) :: pq
    pq = p(0)*q(0) - dot_product (p(1:), q(1:))
  end function dot

  pure subroutine beams (roots, m1, m2, p1, p2)
    real(kind=default), intent(in) :: roots, m1, m2
    real(kind=default), dimension(0:), intent(out) :: p1, p2
    real(kind=default) :: m12, m22
    m12 = m1**2
    m22 = m2**2
    p1(0) = (roots**2 + m12 - m22) / (2*roots)
    p1(1:2) = 0
    p1(3) = sqrt (p1(0)**2 - m12)
    p2(0) = roots - p1(0)
    p2(1:3) = - p1(1:3)
  end subroutine beams

  ! The massless RAMBO algorithm
  subroutine massless_isotropic_decay (roots, p)
    real(kind=default), intent(in) :: roots
    real(kind=default), dimension(0:,:), intent(out) :: p
    real(kind=default), dimension(0:3,size(p,dim=2)) :: q
    real(kind=default), dimension(0:3) :: qsum
    real(kind=double), dimension(4) :: ran_double
    real(kind=default), dimension(4) :: ran
    real(kind=default) :: c, s, f, qabs, x, r, z
    integer :: k
    ! Generate isotropic null vectors
    do k = 1, size (p, dim = 2)
       ! if default is not double or single, we can't use
       ! tao_random_number directly ...
       call tao_random_number (ran_double)
       ran = ran_double
       ! generate a x*exp(-x) distribution for q(0,k)
       q(0,k)= -log(ran(1)*ran(2))
       c = 2*ran(3)-1
       f = 2*PI*ran(4)
       s = sqrt(1-c*c)
       q(2,k) = q(0,k)*s*sin(f)
       q(3,k) = q(0,k)*s*cos(f)
       q(1,k) = q(0,k)*c
    enddo
    ! Boost and rescale the vectors
    qsum = sum (q, dim = 2)
    qabs = sqrt (dot (qsum, qsum))
    x = roots/qabs
    do k = 1, size (p, dim = 2)
       r = dot (q(0:,k), qsum) / qabs
       z = (q(0,k)+r)/(qsum(0)+qabs)
       p(1:3,k) = x*(q(1:3,k)-qsum(1:3)*z)
       p(0,k) = x*r
    enddo
  end subroutine massless_isotropic_decay

end module compare_lib
