! kinematics.f90 --

module kinematics

  use kinds
  implicit none

  private
  public :: dot, mass2
  public :: beams, decay2
  public :: boost, split_massive

  private :: boost_one, boost_many
  interface boost
     module procedure boost_one, boost_many
  end interface

contains

  pure function dot (p, q) result (pq)
    real(kind = omega_prec), dimension(0:), intent(in) :: p, q
    real(kind = omega_prec) :: pq
    pq = p(0)*q(0) - dot_product (p(1:), q(1:))
  end function dot

  pure function mass2 (p) result (m2)
    real (kind = omega_prec), dimension(0:), intent(in) :: p
    real (kind = omega_prec) :: m2
    m2 = p(0)*p(0) - p(1)*p(1) - p(2)*p(2) - p(3)*p(3)
  end function mass2

  pure subroutine beams (roots, m1, m2, p1, p2)
    real (kind = omega_prec), intent(in) :: roots, m1, m2
    real (kind = omega_prec), dimension(0:), intent(out) :: p1, p2
    real (kind = omega_prec) :: m12, m22
    m12 = m1**2
    m22 = m2**2
    p1(0) = (roots**2 + m12 - m22) / (2*roots)
    p1(1:2) = 0
    p1(3) = sqrt (p1(0)**2 - m12)
    p2(0) = roots - p1(0)
    p2(1:3) = - p1(1:3)
  end subroutine beams

  pure subroutine decay2 (mass, m1, m2, costh, phi, p1, p2)
    real (kind = omega_prec), intent(in) :: mass, m1, m2, costh, phi
    real (kind = omega_prec), dimension(0:), intent(out) :: p1, p2
    real (kind = omega_prec) :: m12, m22, pabs, sinth, sinphi, cosphi
    m12 = m1**2
    m22 = m2**2
    p1(0) = (mass**2 + m12 - m22) / (2*mass)
    pabs = sqrt (p1(0)**2 - m12)
    cosphi = cos (phi)
    sinphi = sqrt (1 - cosphi**2)
    sinth = sqrt (1 - costh**2)
    p1(1:3) = pabs * (/ sinth*cosphi, sinth*sinphi, costh /)
    p2(0) = mass - p1(0)
    p2(1:3) = - p1(1:3)
  end subroutine decay2

  pure subroutine boost_one (v, p, q)
    real (kind = omega_prec), dimension(0:), intent(in) :: v, p
    real (kind = omega_prec), dimension(0:), intent(out) :: q
    q(0) = dot_product (p, v)
    q(1:3) = p(1:3) &
         + v(1:3) * (p(0) + dot_product (p(1:3), v(1:3)) / (1 + v(0)))
  end subroutine boost_one

  pure subroutine boost_many (v, p, q)
    real (kind = omega_prec), dimension(0:), intent(in) :: v
    real (kind = omega_prec), dimension(0:,:), intent(in) :: p
    real (kind = omega_prec), dimension(0:,:), intent(out) :: q
    integer :: k
    do k = 1, size (p, dim = 2)
       call boost_one (v, p(:,k), q(:,k))
    enddo
  end subroutine boost_many

  pure subroutine split_massive (p, p_plus, p_minus)
    real (kind = omega_prec), dimension(0:), intent(in) :: p
    real (kind = omega_prec), dimension(0:), intent(out) :: p_plus, p_minus
    real (kind = omega_prec), dimension(3) :: q
    real (kind = omega_prec), dimension(0:3) :: b
    real (kind = omega_prec) :: m, E
    m = sqrt (mass2 (p))
    E = 0.5 * m
    q = 0.5 * m * p(1:3) / sqrt (dot_product (p(1:3), p(1:3)))
    b = p / m
    call boost (b, (/ E, + q /), p_plus)
    call boost (b, (/ E, - q /), p_minus)
  end subroutine split_massive

end module kinematics
