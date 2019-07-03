program f90_O2_test
  use omega95
  use omega_parameters
  use kinds
  use kinematics
  use rambo
  use tao_random_numbers
  implicit none
  integer, parameter :: N = 5
  real(kind=default), save :: roots = 100
  real(kind=default), dimension(N+1) :: m
  real(kind=default), dimension(0:3,N+1) :: p
  real(kind=default), dimension(N,0:3,N) :: pk
  complex(kind=default) :: a(N), j(2)
  integer :: seed, i
  read *, seed, roots
  call tao_random_seed (seed)
  g = 0.1_default
  vev = 10_default
  call setup_parameters ()
  ! call print_parameters ()
  call tao_random_number (m)
  m = 0.2 * roots * m
  mj = m(N+1)
  call beams (roots, m(1), m(2), p(:,1), p(:,2))
  call massive_decay (roots, m(3:), p(:,3:))
  forall (i = 1:N)
     pk(i,:,:) = p(:,1:N)
  end forall
  pk(1,:,1) = p(:,1) - p(:,N+1)
  pk(2,:,2) = p(:,2) - p(:,N+1)
  forall (i = 3:N)
     pk(i,:,i) = p(:,i) + p(:,N+1)
  end forall
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  j(1) =       - mj * with_insertion (p, (/ 0, 0, 0, 0, 0, 4 /), (/ 2, 1, 1, 1, 1, 0 /))
  j(2) =        vev * with_insertion (p, (/ 0, 0, 0, 0, 0, 0 /), (/ 2, 1, 1, 1, 1, 2 /))
  a(1) =   without_insertion (pk(1,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 1, 1, 1, 1    /))
  a(2) = - without_insertion (pk(2,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 1, 1, 1    /))
  a(3) = - without_insertion (pk(3,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 2, 1, 1    /))
  a(4) = - without_insertion (pk(4,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 1, 2, 1    /))
  a(5) = - without_insertion (pk(5,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 1, 1, 2    /))
  a(1) = g2 (pk(1,:,1), m1) / g2 (p(:,1), m2) * a(1)
  forall (i = 2:N)
     a(i) = g2 (pk(i,:,i), m2) / g2 (p(:,i), m1) * a(i)
  end forall
! print *, '  A=', cmplx (sum (a)), 'J=', cmplx (sum (j))
  print *, 'A/J=', sum (a) / (sum (j)) - 1
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  j(1) =        - mj * with_insertion (p, (/ 0, 0, 0, 0, 0, 4 /), (/ 1, 2, 1, 1, 1, 0 /))
  j(2) =         vev * with_insertion (p, (/ 0, 0, 0, 0, 0, 0 /), (/ 1, 2, 1, 1, 1, 2 /))
  a(1)  = - without_insertion (pk(1,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 1, 1, 1    /))
  a(2)  =   without_insertion (pk(2,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 1, 1, 1, 1    /))
  a(3)  = - without_insertion (pk(3,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 2, 1, 1    /))
  a(4)  = - without_insertion (pk(4,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 1, 2, 1    /))
  a(5)  = - without_insertion (pk(5,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 1, 1, 2    /))
  forall (i = 1:1)
     a(i) = g2 (pk(i,:,i), m2) / g2 (p(:,i), m1) * a(i)
  end forall
  a(2) = g2 (pk(2,:,2), m1) / g2 (p(:,2), m2) * a(2)
  forall (i = 3:N)
     a(i) = g2 (pk(i,:,i), m2) / g2 (p(:,i), m1) * a(i)
  end forall
! print *, '  A=', cmplx (sum (a)), 'J=', cmplx (sum (j))
  print *, 'A/J=', sum (a) / (sum (j)) - 1
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  j(1) =        - mj * with_insertion (p, (/ 0, 0, 0, 0, 0, 4 /), (/ 1, 1, 2, 1, 1, 0 /))
  j(2) =         vev * with_insertion (p, (/ 0, 0, 0, 0, 0, 0 /), (/ 1, 1, 2, 1, 1, 2 /))
  a(1)  = - without_insertion (pk(1,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 2, 1, 1    /))
  a(2)  = - without_insertion (pk(2,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 2, 1, 1    /))
  a(3)  =   without_insertion (pk(3,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 1, 1, 1, 1    /))
  a(4)  = - without_insertion (pk(4,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 1, 2, 2, 1    /))
  a(5)  = - without_insertion (pk(5,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 1, 2, 1, 2    /))
  forall (i = 1:2)
     a(i) = g2 (pk(i,:,i), m2) / g2 (p(:,i), m1) * a(i)
  end forall
  a(3) = g2 (pk(3,:,3), m1) / g2 (p(:,3), m2) * a(3)
  forall (i = 4:N)
     a(i) = g2 (pk(i,:,i), m2) / g2 (p(:,i), m1) * a(i)
  end forall
! print *, '  A=', cmplx (sum (a)), 'J=', cmplx (sum (j))
  print *, 'A/J=', sum (a) / (sum (j)) - 1
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  j(1) =       - mj * with_insertion (p, (/ 0, 0, 0, 0, 0, 4 /), (/ 2, 2, 2, 2, 2, 0 /))
  j(2) =        vev * with_insertion (p, (/ 0, 0, 0, 0, 0, 0 /), (/ 2, 2, 2, 2, 2, 2 /))
  a(1) =   without_insertion (pk(1,:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 2, 2, 2    /))
  a(2) =   without_insertion (pk(2,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 2, 2, 2    /))
  a(3) =   without_insertion (pk(3,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 1, 2, 2    /))
  a(4) =   without_insertion (pk(4,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 2, 1, 2    /))
  a(5) =   without_insertion (pk(5,:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 2, 2, 1    /))
  forall (i = 1:N)
     a(i) = g2 (pk(i,:,i), m1) / g2 (p(:,i), m2) * a(i)
  end forall
! print *, '  A=', cmplx (sum (a)), 'J=', cmplx (sum (j))
  print *, 'A/J=', sum (a) / (sum (j)) - 1
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  m(1) = m1
!  m(2:N) = m2
  call beams (roots, m(1), m(2), p(:,1), p(:,2))
  call massive_decay (roots, m(3:N), p(:,3:N))
  p(:,N+1) = 0
  j(1) =    - mj * with_insertion (p, (/ 0, 0, 0, 0, 0, 4 /), (/ 2, 2, 2, 2, 2, 0 /))
  j(2) =     vev * with_insertion (p, (/ 0, 0, 0, 0, 0, 0 /), (/ 2, 2, 2, 2, 2, 2 /))
  a(1) =   without_insertion (p(:,:), (/ 0, 0, 0, 0, 0    /), (/ 1, 2, 2, 2, 2    /))
  a(2) =   without_insertion (p(:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 1, 2, 2, 2    /))
  a(3) =   without_insertion (p(:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 1, 2, 2    /))
  a(4) =   without_insertion (p(:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 2, 1, 2    /))
  a(5) =   without_insertion (p(:,:), (/ 0, 0, 0, 0, 0    /), (/ 2, 2, 2, 2, 1    /))
  forall (i = 1:N)
     a(i) = g2 (p(:,i), m1) / g2 (p(:,i), m2) * a(i)
  end forall
! print *, '  A=', cmplx (sum (a)), 'J=', cmplx (sum (j))
  print *, 'A/J=', sum (a) / (sum (j)) - 1
contains
  pure function without_insertion (k, s, f) result (amp)
    use j20, only: amplitude, symmetry
    real(kind=default), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=default) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function without_insertion
  pure function with_insertion (k, s, f) result (amp)
    use j21, only: amplitude, symmetry
    real(kind=default), dimension(0:,:), intent(in) :: k
    integer, dimension(:), intent(in) :: s, f
    complex(kind=default) :: amp
    amp = symmetry (f) * amplitude (k, s, f) 
  end function with_insertion
  pure function g2 (p, m) result (g)
    real(kind=default) :: g
    real(kind=default), dimension(0:), intent(in) :: p
    real(kind=default), intent(in) :: m
    real(kind=default) :: p2
    p2 = dot (p, p)
    g = 1 / (p2 - m*m)
  end function g2
end program f90_O2_test
