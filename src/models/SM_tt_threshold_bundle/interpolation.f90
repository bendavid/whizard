! WHIZARD <<Version>> <<Date>>

! Copyright (C) 1999-2013 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     Fabian Bach <fabian.bach@desy.de> (only this file)
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

module interpolation
  use kinds
  implicit none
  save
  private

  public :: interpolate_linear, strictly_monotonous

  interface interpolate_linear
    module  procedure interpolate_linear_1D_complex_array, &
      interpolate_linear_1D_complex_scalar, &
      interpolate_linear_1D_real_array, &
      interpolate_linear_1D_real_scalar, &
      interpolate_linear_2D_complex_array, &
      interpolate_linear_2D_complex_scalar, &
      interpolate_linear_2D_real_array, &
      interpolate_linear_2D_real_scalar
  end interface
  
  interface strictly_monotonous
    module procedure monotonous
  end interface strictly_monotonous

  interface find_nearest_left
    !!! recursive bisection seems slower with these array sizes
    module procedure find_nearest_left_loop
  end interface find_nearest_left

contains

  pure subroutine interpolate_linear_1D_complex_array (xa, ya, x, y)
    real(default), dimension(:), intent(in) :: xa
    complex(default), dimension(:,:), intent(in) :: ya
    real(default), intent(in) :: x
    complex(default), dimension(:), intent(out) :: y
    integer :: ny, ixl, iy
    real(default) :: t
    ny = size(ya(1,:))
    y = (/ (0.0_default, iy=1, ny) /)
    !!! don't check this at runtime:
    ! if ( .not.monotonous(xa) ) return
    if ( out_of_range(xa, x) ) return
    ixl = 0
    call find_nearest_left (xa, x, ixl)
    t = ( x - xa(ixl) ) / ( xa(ixl+1) - xa(ixl) )
    do iy = 1, ny
      y(iy) = (1.-t)*ya(ixl,iy) + t*ya(ixl+1,iy)
    end do
  end subroutine interpolate_linear_1D_complex_array

  pure subroutine interpolate_linear_2D_complex_array (x1a, x2a, ya, x1, x2, y)
    real(default), dimension(:), intent(in) :: x1a
    real(default), dimension(:), intent(in) :: x2a
    complex(default), dimension(:,:,:), intent(in) :: ya
    real(default), intent(in) :: x1
    real(default), intent(in) :: x2
    complex(default), dimension(:), intent(out) :: y
    integer :: ny, ix1l, ix2l, iy
    real(default) :: t, u
    ny  = size(ya(1,1,:))
    y = (/ (0.0_default, iy=1, ny) /)
    !!! don't check this at runtime:
    ! if ( (.not.monotonous(x1a)) .or. (.not.monotonous(x2a)) ) return
    if ( out_of_range(x1a, x1) .or. out_of_range(x2a, x2) ) return
    ix1l = 0
    call find_nearest_left (x1a, x1, ix1l)
    ix2l = 0
    call find_nearest_left (x2a, x2, ix2l)
    t = ( x1 - x1a(ix1l) ) / ( x1a(ix1l+1) - x1a(ix1l) )
    u = ( x2 - x2a(ix2l) ) / ( x2a(ix2l+1) - x2a(ix2l) )
    do iy = 1, ny
      y(iy) =  (1.-t)*(1.-u)*ya(ix1l  ,ix2l  ,iy) &
              +    t *(1.-u)*ya(ix1l+1,ix2l  ,iy) &
              +    t *    u *ya(ix1l+1,ix2l+1,iy) &
              +(1.-t)*    u *ya(ix1l  ,ix2l+1,iy)
    end do
  end subroutine interpolate_linear_2D_complex_array

  pure subroutine find_nearest_left_loop (xa, x, ixl)
    real(default), dimension(:), intent(in) :: xa
    real(default), intent(in) :: x
    integer, intent(out) :: ixl
    integer :: ix
    do ix = 2, size(xa)
      if ( x < xa(ix) ) then
        ixl = ix-1
        return
      end if
    end do
  end subroutine find_nearest_left_loop

  pure recursive subroutine find_nearest_left_rec (xa, x, ixl)
    real(default), dimension(:), intent(in) :: xa
    real(default), intent(in) :: x
    integer, intent(inout) :: ixl
    integer :: nx, bs
    real(default), dimension(:), allocatable :: xa_new
    nx = size(xa)
    bs = nx/2 + 1
    if ( nx < 3 ) then
      ixl = ixl + bs - 1
      return
    else
      if ( x < xa(bs) ) then
        allocate( xa_new(1:bs) )
        xa_new = xa(1:bs)
      else
        ixl = ixl + bs - 1
        allocate( xa_new(bs:nx) )
        xa_new = xa(bs:nx)
      end if
      call find_nearest_left (xa_new, x, ixl)
      deallocate( xa_new )
    end if
  end subroutine find_nearest_left_rec

  pure function monotonous (xa) result (flag)
    real(default), dimension(:), intent(in) :: xa
    integer :: ix
    logical :: flag
    flag = .false.
    do ix = 1, size(xa)-1
      flag = ( xa(ix) < xa(ix+1) )
      if ( .not. flag ) return
    end do
  end function monotonous

  pure function out_of_range (xa, x) result (flag)
    real(default), dimension(:), intent(in) :: xa
    real(default), intent(in) :: x
    logical :: flag
    flag = ( x < xa(1) .or. x > xa(size(xa)) )
  end function out_of_range

  pure subroutine interpolate_linear_1D_complex_scalar (xa, ya, x, y)
    real(default), dimension(:), intent(in) :: xa
    complex(default), dimension(:), intent(in) :: ya
    real(default), intent(in) :: x
    complex(default), intent(out) :: y
    complex(default), dimension(size(ya),1) :: ya_c
    complex(default), dimension(1) :: y_c
    ya_c(:,1) = ya
    call interpolate_linear_1D_complex_array (xa, ya_c, x, y_c)
    y = y_c(1)
  end subroutine interpolate_linear_1D_complex_scalar

  pure subroutine interpolate_linear_1D_real_array (xa, ya, x, y)
    real(default), dimension(:), intent(in) :: xa
    real(default), dimension(:,:), intent(in) :: ya
    real(default), intent(in) :: x
    real(default), dimension(:), intent(out) :: y
    complex(default), dimension(size(ya(1,:))) :: y_c
    call interpolate_linear_1D_complex_array (xa, cmplx(ya,kind=default), x, y_c)
    y = real(y_c,kind=default)
  end subroutine interpolate_linear_1D_real_array

  pure subroutine interpolate_linear_1D_real_scalar (xa, ya, x, y)
    real(default), dimension(:), intent(in) :: xa
    real(default), dimension(:), intent(in) :: ya
    real(default), intent(in) :: x
    real(default), intent(out) :: y
    complex(default), dimension(size(ya),1) :: ya_c
    complex(default), dimension(1) :: y_c
    ya_c(:,1) = cmplx(ya,kind=default)
    call interpolate_linear_1D_complex_array (xa, ya_c, x, y_c)
    y = real(y_c(1),kind=default)
  end subroutine interpolate_linear_1D_real_scalar

  pure subroutine interpolate_linear_2D_complex_scalar (x1a, x2a, ya, x1, x2, y)
    real(default), dimension(:), intent(in) :: x1a
    real(default), dimension(:), intent(in) :: x2a
    complex(default), dimension(:,:), intent(in) :: ya
    real(default), intent(in) :: x1
    real(default), intent(in) :: x2
    complex(default), intent(out) :: y
    complex(default), dimension(size(ya(:,1)),size(ya(1,:)),1) :: ya_c
    complex(default), dimension(1) :: y_c
    ya_c(:,:,1) = ya
    call interpolate_linear_2D_complex_array (x1a, x2a, ya_c, x1, x2, y_c)
    y = y_c(1)
  end subroutine interpolate_linear_2D_complex_scalar

  pure subroutine interpolate_linear_2D_real_array (x1a, x2a, ya, x1, x2, y)
    real(default), dimension(:), intent(in) :: x1a
    real(default), dimension(:), intent(in) :: x2a
    real(default), dimension(:,:,:), intent(in) :: ya
    real(default), intent(in) :: x1
    real(default), intent(in) :: x2
    real(default), dimension(:), intent(out) :: y
    complex(default), dimension(size(ya(1,1,:))) :: y_c
    call interpolate_linear_2D_complex_array (x1a, x2a, cmplx(ya,kind=default), x1, x2, y_c)
    y = real(y_c,kind=default)
  end subroutine interpolate_linear_2D_real_array

  pure subroutine interpolate_linear_2D_real_scalar (x1a, x2a, ya, x1, x2, y)
    real(default), dimension(:), intent(in) :: x1a
    real(default), dimension(:), intent(in) :: x2a
    real(default), dimension(:,:), intent(in) :: ya
    real(default), intent(in) :: x1
    real(default), intent(in) :: x2
    real(default), intent(out) :: y
    complex(default), dimension(size(ya(:,1)),size(ya(1,:)),1) :: ya_c
    complex(default), dimension(1) :: y_c
    ya_c(:,:,1) = cmplx(ya,kind=default)
    call interpolate_linear_2D_complex_array (x1a, x2a, ya_c, x1, x2, y_c)
    y = real(y_c(1),kind=default)
  end subroutine interpolate_linear_2D_real_scalar
end module interpolation
