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

module sorting

  use kinds, only: default !NODEP!
  use diagnostics !NODEP!

  implicit none
  private

  public :: sort
  public :: order
  public :: concat
  public :: sorting_test

  interface sort
     module procedure sort_int
     module procedure sort_real
  end interface

  interface order
     module procedure order_int
     module procedure order_real
  end interface

  interface merge
     module procedure merge_int
     module procedure merge_real
  end interface

  interface concat
     module procedure concat_int
     module procedure concat_real
  end interface


contains

  function sort_int (val_in) result (val)
    integer, dimension(:), intent(in) :: val_in
    integer, dimension(size(val_in)) :: val
    val = val_in( order (val_in) )
  end function sort_int

  function sort_real (val_in) result (val)
    real(default), dimension(:), intent(in) :: val_in
    real(default), dimension(size(val_in)) :: val
    val = val_in( order (val_in) )
  end function sort_real

  function order_int (val) result (idx)
    integer, dimension(:), intent(in) :: val
    integer, dimension(size(val)) :: idx
    integer :: n, i, s, b1, b2, e1, e2
    n = size (idx)
    forall (i = 1:n)
       idx(i) = i
    end forall
    s = 1
    do while (s < n)
       do b1 = 1, n-s, 2*s
          b2 = b1 + s
          e1 = b2 - 1
          e2 = min (e1 + s, n)
          call merge (idx(b1:e2), idx(b1:e1), idx(b2:e2), val)
       end do
       s = 2 * s
    end do
  end function order_int

  function order_real (val) result (idx)
    real(default), dimension(:), intent(in) :: val
    integer, dimension(size(val)) :: idx
    integer :: n, i, s, b1, b2, e1, e2
    n = size (idx)
    forall (i = 1:n)
       idx(i) = i
    end forall
    s = 1
    do while (s < n)
       do b1 = 1, n-s, 2*s
          b2 = b1 + s
          e1 = b2 - 1
          e2 = min (e1 + s, n)
          call merge (idx(b1:e2), idx(b1:e1), idx(b2:e2), val)
       end do
       s = 2 * s
    end do
  end function order_real

  subroutine merge_int (res, src1, src2, val)
    integer, dimension(:), intent(out) :: res
    integer, dimension(:), intent(in) :: src1, src2
    integer, dimension(:), intent(in) :: val
    integer, dimension(size(res)) :: tmp
    integer :: i1, i2, i
    i1 = 1
    i2 = 1
    do i = 1, size (tmp)
       if (val(src1(i1)) <= val(src2(i2))) then
          tmp(i) = src1(i1);  i1 = i1 + 1
          if (i1 > size (src1)) then
             tmp(i+1:) = src2(i2:)
             exit
          end if
       else
          tmp(i) = src2(i2);  i2 = i2 + 1
          if (i2 > size (src2)) then
             tmp(i+1:) = src1(i1:)
             exit
          end if
       end if
    end do
    res = tmp
  end subroutine merge_int
    
  subroutine merge_real (res, src1, src2, val)
    integer, dimension(:), intent(out) :: res
    integer, dimension(:), intent(in) :: src1, src2
    real(default), dimension(:), intent(in) :: val
    integer, dimension(size(res)) :: tmp
    integer :: i1, i2, i
    i1 = 1
    i2 = 1
    do i = 1, size (tmp)
       if (val(src1(i1)) <= val(src2(i2))) then
          tmp(i) = src1(i1);  i1 = i1 + 1
          if (i1 > size (src1)) then
             tmp(i+1:) = src2(i2:)
             exit
          end if
       else
          tmp(i) = src2(i2);  i2 = i2 + 1
          if (i2 > size (src2)) then
             tmp(i+1:) = src1(i1:)
             exit
          end if
       end if
    end do
    res = tmp
  end subroutine merge_real

  function concat_int (val1, val2) result (val12)
    integer, dimension(:), intent(in) :: val1, val2
    integer, dimension(size(val1)+size(val2)) :: val12
    val12(:size(val1)) = val1
    val12(size(val1)+1:) = val2
  end function concat_int

  function concat_real (val1, val2) result (val12)
    real(default), dimension(:), intent(in) :: val1, val2
    integer, dimension(size(val1)+size(val2)) :: val12
    val12(:size(val1)) = val1
    val12(size(val1)+1:) = val2
  end function concat_real

  subroutine sorting_test ()
    integer, parameter :: NMAX = 10
    real(default), dimension(NMAX) :: rval
    integer, dimension(NMAX) :: ival
    real, dimension(NMAX) :: harvest
    integer :: i, j
    print *, "Sorting real values:"
    do i = 1, NMAX
       print *
       call random_number (harvest(:i))
       rval(:i) = harvest(:i)
       print "(10(1x,F7.4))", rval(:i)
       rval(:i) = sort (rval(:i))
       print "(10(1x,F7.4))", rval(:i)
       do j = i, 2, -1
          if (rval(j)-rval(j-1) < 0) &
             call msg_fatal ("Sorting failure")
       end do
    end do
    print *
    print *, "Sorting integer values:"
    do i = 1, NMAX
       print *
       call random_number (harvest(:i))
       ival(:i) = harvest(:i) * NMAX * 2
       print "(10(1x,I2))", ival(:i)
       ival(:i) = sort (ival(:i))
       print "(10(1x,I2))", ival(:i)
       do j = i, 2, -1
          if (ival(j)-ival(j-1) < 0) &
             call msg_fatal ("Sorting failure")
       end do
    end do
  end subroutine sorting_test


end module sorting
