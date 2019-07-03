! $Id: illustrations.F 314 2010-04-17 20:32:33Z ohl $

module foo
  use kinds
  private

  public :: f, g, phi, w
  private :: f0, phi0
  integer, parameter, private :: N = 2
  real(kind=double), dimension(N), public :: x0, x0_true, a, weights

contains

  function phi0 (x, x0, a) result (phi0_x)
    real(kind=double), intent(in) :: x, x0, a
    real(kind=double) :: phi0_x
    phi0_x = x0 &
         + a * tan (x * atan ((1 - x0) / a) - (1 - x) * atan (x0 / a))
  end function phi0

  function f0 (x, x0, a) result (f0_x)
    real(kind=double), intent(in) :: x, x0, a
    real(kind=double) :: f0_x
    f0_x = a / ((x - x0)**2 + a**2) &
         / (atan ((1 - x0) / a) + atan (x0 / a))
  end function f0

  function phi (x, channel) result (phi_x)
    real(kind=double), intent(in) :: x
    integer, intent(in) :: channel
    real(kind=double) :: phi_x
    phi_x = phi0 (x, x0(channel), a(channel))
  end function phi

  function g (x, channel) result (g_x)
    real(kind=double), intent(in) :: x
    integer, intent(in) :: channel
    real(kind=double) :: g_x
    integer :: ch
    if (channel == 0) then
       g_x = 0.0
       do ch = 1, N
          g_x = g_x + weights(ch) * f0 (x, x0(ch), a(ch))
       end do
    else
       g_x = f0 (x, x0(channel), a(channel))
    end if
  end function g

  function f (x) result (f_x)
    real(kind=double), intent(in) :: x
    real(kind=double) :: f_x
    complex(kind=double) :: s_x
    s_x = 1.0 / cmplx (x - x0_true(1), a(1)) &
         + 1.0 / cmplx (x - x0_true(2), a(2))
    f_x = conjg (s_x) * s_x
  end function f

  function w (x, channel) result (w_x)
    real(kind=double), intent(in) :: x
    integer, intent(in) :: channel
    real(kind=double) :: w_x
    real(kind=double) :: x_prime
    if (channel > 0) then
       x_prime = phi (x, channel)
       w_x = f (x_prime) / g (x_prime, 0)
    else if (channel < 0) then
       x_prime = phi (x, -channel)
       w_x = g (x_prime, -channel) / g (x_prime, 0)
    else
       w_x = f (x)
    end if
  end function w

end module foo

program illustrations
  use kinds
  use foo
  real(kind=double) :: x, s, shift
  integer :: i, i_max
  integer, parameter :: io_unit = 10
  character(len=*), parameter :: fmt = "(5(1X,F12.6))"
  weights = 1.0
  a = 0.1
  shift = 0.15
  x0_true(1) = 0.2
  x0_true(2) = 0.8
  x0(1) = x0_true(1) - shift * a(1)
  x0(2) = x0_true(2) + shift * a(2)
  weights = weights / sum (weights)
  i_max = 250
  print *, "PHI"
  open (unit = io_unit, action = "write", status = "replace", file = "phi1.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, phi (x, 1)
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "phi2.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, phi (x, 2)
  end do
  close (unit = io_unit)
  print *, "F/G"
  open (unit = io_unit, action = "write", status = "replace", file = "fog.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, f (x) / g (x, 0)
  end do
  close (unit = io_unit)
  print *, "F, G, G1, G2"
  s = f(x0(1)) / g(x0(1), 0)
  open (unit = io_unit, action = "write", status = "replace", file = "f.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, f (x)
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "g.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, s * g (x, 0)
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "g1.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, s/2 * g (x, 1)
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "g2.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, s/2 * g (x, 2)
  end do
  close (unit = io_unit)
  print *, "W1, W2, W12"
  open (unit = io_unit, action = "write", status = "replace", file = "w1.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, w (x, 1) / s
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "w2.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, w (x, 2) / s
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "w12.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, (w (x, 1) + w (x, 2)) / (2*s)
  end do
  close (unit = io_unit)
  x0 = x0_true
  print *, "W/true"
  open (unit = io_unit, action = "write", status = "replace", file = "w1t.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, w (x, 1) / s
  end do
  close (unit = io_unit)
  open (unit = io_unit, action = "write", status = "replace", file = "w2t.plot")
  do i = 0, i_max
     x = real (i, kind = double) / i_max
     write (unit = io_unit, fmt = fmt) x, w (x, 2) / s
  end do
  close (unit = io_unit)
end program illustrations


