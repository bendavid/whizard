module rambo

  use kinds
  use kinematics
  use tao_random_numbers
  implicit none
  !!! Should be there, bt chokes the Intel compiler
  ! private

  public :: massless_isotropic_decay, massive_decay
  real (kind = omega_prec), private, parameter :: &
       PI = 3.1415926535897932384626433832795028841972

contains

  !!! The massless RAMBO algorithm
  subroutine massless_isotropic_decay (roots, p)
    real (kind = omega_prec), intent(in) :: roots
    ! It's a bit stupid that F disallows an explicit `dimension(0:3,:)' here.
    real (kind = omega_prec), dimension(0:,:), intent(out) :: p
    real (kind = omega_prec), dimension(0:3,size(p,dim=2)) :: q
    real (kind = omega_prec), dimension(0:3) :: qsum
    real (kind = double), dimension(4) :: ran
    real (kind = omega_prec) :: c, s, f, qabs, x, r, z
    integer :: k
    ! Generate isotropic null vectors
    do k = 1, size (p, dim = 2)
       call tao_random_number (ran)
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

  !!! The massive RAMBO algorithm (not reweighted, therefore not isotropic)
  subroutine massive_decay (roots, m, p)
    real (kind = omega_prec), intent(in) :: roots
    real (kind = omega_prec), dimension(:), intent(in) :: m
    real (kind = omega_prec), dimension(0:,:), intent(out) :: p
    real (kind = omega_prec), dimension(0:3,size(p,dim=2)) :: q
    real (kind = omega_prec), dimension(size(p,dim=2)) :: p2, m2, p0
    real (kind = omega_prec), dimension(0:3) :: qsum
    real (kind = double), dimension(2) :: ran
    real (kind = omega_prec) :: c, s, f, qq
    real (kind = omega_prec) :: w, a, xu, u, umax, xv, v, vmax, x
    real (kind = omega_prec) :: xi, delta
    integer :: k, i
    if (sum(m) > roots) then
       print *, "no solution: sum(m) > roots"
       p = 0
       return
    end if
    m2 = m*m
    ! Generate isotropic massive vectors
    w = 1
    do k = 1, size (p, dim = 2)
       ! Kinderman/Monahan (a la Kleiss/Sterling)
       a = 2 * m(k) / w
       xu = 0.5 * (1 - a + sqrt (1 + a*a))
       xv = 0.5 * (3 - a + sqrt (9 + 4*a + a*a))
       umax = exp (-0.5*xu) * sqrt (sqrt (xu*xu + a*xu))
       vmax = xv * exp (-0.5*xv) * sqrt (sqrt (xv*xv + a*xv))
       rejection: do
          call tao_random_number (ran)
          u = ran(1) * umax
          v = ran(2) * vmax
          x = v / u
          if (u*u < exp(-x) * sqrt (x*x + a*x)) then
             qq = m(k) + w*x
             exit rejection
          end if
       end do rejection
       call tao_random_number (ran)
       c = 2*ran(1) - 1
       !!! select case (k)
       !!!    case (1,3)
       !!!        c = 1 - 0.0000002*ran(1)
       !!!    case (2,4)
       !!!        c = 0.0000002*ran(1) - 1
       !!! end select
       f = 2*PI*ran(2)
       s = sqrt (1 - c*c)
       q(0,k) = sqrt (qq*qq + m2(k))
       q(1,k) = qq * s * sin(f)  
       q(2,k) = qq * s * cos(f)
       q(3,k) = qq * c
    enddo
    ! Boost the vectors to the common rest frame
    qsum = sum (q, dim = 2)
    call boost ((/ qsum(0), - qsum(1:3) /) / sqrt (mass2 (qsum)), q, p)
    ! rescale momenta
    do k = 1, size (p, dim = 2)
       p2(k) = dot_product (p(1:3,k), p(1:3,k))
    end do
    i = 1
    xi = 1
    find_xi: do
       p0 = sqrt (xi*xi*p2 + m2)
       delta = sum (p0) - roots
       if ((i > 100) .or. (abs (delta) <= 10 * epsilon (roots))) then
          exit find_xi
       end if
       ! Newton / Ralphson iteration
       xi = xi - delta / (xi * sum (p2 / p0))
       i = i + 1
    end do find_xi
    p(0,:) = p0
    p(1:3,:) = xi * p(1:3,:)
  end subroutine massive_decay

end module rambo
