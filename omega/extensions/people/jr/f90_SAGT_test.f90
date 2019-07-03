program f90_SAGT_test
  use omega95_bispinors
  use omega_parameters
  use kinematics
  use rambo
  use tao_random_numbers
  implicit none
  integer, parameter :: N = 4
  real(kind=omega_prec), save :: roots = 100, nada = 0
  real(kind=omega_prec), dimension(N+1) :: m
  real(kind=omega_prec), dimension(0:3,N+1) :: p
  real(kind=omega_prec) :: rel
  complex(kind=omega_prec) :: j1, j2, j3, j4, j5, j6, res
  ! complex(kind=omega_prec) :: j1, j2, j3
  integer :: seed, pol
  read *, seed, roots !, pol 
  call tao_random_seed (seed)
  call setup_parameters ()
  ! call print_parameters ()
  call tao_random_number (m)
  m = 0.2 * roots * m
  !m(1:) = nada
  ! p(:,1) = (/ roots, sqrt(roots**2 - m**2) , nada, nada /)
  ! p(:,2) = - p (:,1)
  call beams (roots, m(1), m(2), p(:,1), p(:,2))
  call massive_decay (roots, m(3:), p(:,3:))    
  !p(:,3) = (/ roots, nada, nada, nada /)
  !p(:,4) = (/ nada, nada, nada, nada /)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  j1 = contact1 (p, (/ 1, 1, 0, 0, 0 /), (/ 1003, 3, 1, 1, (-6) /))
  j2 = contact2 (p, (/ 1, 1, 0, 0, 0 /), (/ 3, 1003, 1, 1, (-6) /))
  j3 = contact3 (p, (/ 1, 1, 0, 0, 0 /), (/ 3, 3, 1001, 1, (-6) /))
  j4 = contact4 (p, (/ 1, 1, 0, 0, 0 /), (/ 3, 3, 1, 1001, (-6) /))
  j5 = currenta (p, (/ 1, 1, 0, 0, 3 /), (/ 3, 3, 1, 1, 4 /))
  rel = (abs(j1)+abs(j2)+abs(j3)+abs(j4)+abs(j5))/5.0_omega_prec
  res = (j1 + j2 + j3 + j4 + j5)/rel
  print *, j1, abs(j1) 
  print *, j2, abs(j2)
  print *, j3, abs(j3)
  print *, j4, abs(j4)
  print *, j5, abs(j5)
  print *, res
  !print *, (-j1 + j2 + j3 + j4 + j5)/rel
  !print *, ( j1 - j2 + j3 + j4 + j5)/rel
  !print *, ( j1 + j2 - j3 + j4 + j5)/rel
  !print *, ( j1 + j2 + j3 - j4 + j5)/rel
  !print *, ( j1 + j2 + j3 + j4 - j5)/rel
  !print *, (j1 - j2 - j3 + j4 + j5)/rel
  !print *, (j1 - j2 + j3 - j4 + j5)/rel
  !print *, (j1 - j2 + j3 + j4 - j5)/rel
  !print *, (j1 + j2 - j3 - j4 + j5)/rel
  !print *, (j1 + j2 - j3 + j4 - j5)/rel
  !print *, (j1 + j2 + j3 - j4 - j5)/rel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  j1 = contact1 (p, (/ 0, 0, 1, pol /), (/ 1005, 1, 2, 7 /))
!  j2 = contact2 (p, (/ 0, 0, 1, pol /), (/ 5, 1001, 2, 7 /))
!  j3 = contact3 (p, (/ 0, 0, 1, pol /), (/ 5, 1, 1002, 7 /))
!!  j4 = contact4 (p, (/ 1, 1, 1, 1, 0, 1, 0 /), (/ 3, 5, 3, 3, 1001, 4, (-6) /))  
!!  j5 = contact5 (p, (/ 1, 1, 1, 1, 0, 1, 3 /), (/ 3, 5, 3, 3, 1, 4, 4 /))
!  print *, "brs(p)f->axi", j1, abs(j1)
!  print *, "pbrs(f)->axi", j2, abs(j2)
!  print *, "pf->brs(a)xi", j3, abs(j3)
!!  print *, "aa->abrs(a)cbar", j4, abs(j4)
!!  print *, "aa->aap", j5, abs(j5)
!!  print *, "ff->fbrs(f)ppcbar", j4, abs(j4)
!!  print *, "ff->ffppp", j5, abs(j5)
!!  rel = (abs(j1)+abs(j2)+abs(j3)+abs(j4)+abs(j5))/5.0_omega_prec
!  rel = (abs(j1)+abs(j2)+abs(j3))/3.0_omega_prec
!  res = abs(-j1 + j2 + j3)
!!  res = abs(j1 + j2 + j3 + j4 + j5)/rel
!  print *, res
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !j1 = contact1 (p, (/ 1, 0, 1 /), (/ 1003, 6, 3 /))
  !j2 = contact2 (p, (/ 1, 0, 1 /), (/ 3, 6, 1003 /))
  !j3 = currenta (p, (/ 1, 3, 1 /), (/ 3, 4, 3 /))
  !rel = (abs(j1)+abs(j2)+abs(j3))/3.0_omega_prec
  !res = abs(j1 + j2 + j3)/rel
  !print *, j1
  !print *, j2
  !print *, j3
  !print *, res
  !print *, abs(-j1 + j2 + j3)/rel
  !print *, abs( j1 - j2 + j3)/rel
  !print *, abs( j1 + j2 - j3)/rel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
  pure function contact1 (k, s, f) result (amp)
    use amp1, only: amplitude, symmetry
    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=omega_prec) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function contact1
  pure function contact2 (k, s, f) result (amp)
    use amp2, only: amplitude, symmetry
    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=omega_prec) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function contact2
  pure function contact3 (k, s, f) result (amp)
    use amp3, only: amplitude, symmetry
    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=omega_prec) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function contact3
  pure function contact4 (k, s, f) result (amp)
    use amp4, only: amplitude, symmetry
    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=omega_prec) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function contact4
!  pure function contact5 (k, s, f) result (amp)
!    use amp5, only: amplitude, symmetry
!    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
!    integer, dimension(:), intent(in) :: s, f
!    complex(kind=omega_prec) :: amp
!    amp = symmetry (f) * amplitude (k, s, f) 
!  end function contact5
  !pure function contact6 (k, s, f) result (amp)
  !  use amp6, only: amplitude, symmetry
  !  real(kind=omega_prec), dimension(0:,:), intent(in) :: k
  !  integer, dimension(:), intent(in) :: s, f
  !  complex(kind=omega_prec) :: amp
  !  amp = symmetry (f) * amplitude (k, s, f) 
  !end function contact6
  pure function currenta (k, s, f) result (amp)
    use amp5, only: amplitude, symmetry
    real(kind=omega_prec), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=omega_prec) :: amp
    amp = symmetry (f) * amplitude (k, s, f)
  end function currenta
end program f90_SAGT_test


