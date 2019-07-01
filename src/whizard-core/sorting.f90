! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use unit_tests
  
  implicit none
  private

  public :: sort
  public :: sort_abs
  public :: order
  public :: order_abs
  public :: concat
  public :: sorting_test

  interface sort
     module procedure sort_int
     module procedure sort_real
  end interface

  interface sort_abs
     module procedure sort_int_abs
  end interface

  interface order
     module procedure order_int
     module procedure order_real
  end interface

  interface order_abs
     module procedure order_int_abs
  end interface

  interface merge
     module procedure merge_int
     module procedure merge_real
  end interface

  interface merge_abs
     module procedure merge_int_abs
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

  function sort_int_abs (val_in) result (val)
    integer, dimension(:), intent(in) :: val_in
    integer, dimension(size(val_in)) :: val
    val = val_in( order_abs (val_in) )
  end function sort_int_abs

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

  function order_int_abs (val) result (idx)
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
          call merge_abs (idx(b1:e2), idx(b1:e1), idx(b2:e2), val)
       end do
       s = 2 * s
    end do
  end function order_int_abs

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

  subroutine merge_int_abs (res, src1, src2, val)
    integer, dimension(:), intent(out) :: res
    integer, dimension(:), intent(in) :: src1, src2
    integer, dimension(:), intent(in) :: val
    integer, dimension(size(res)) :: tmp
    integer :: i1, i2, i
    i1 = 1
    i2 = 1
    do i = 1, size (tmp)
       if (abs (val(src1(i1))) < abs (val(src2(i2))) .or. &
          (abs (val(src1(i1))) == abs (val(src2(i2))) .and. &
          val(src1(i1)) >= val(src2(i2)))) then
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
  end subroutine merge_int_abs
    
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

  subroutine sorting_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sorting_1, "sorting_1", &
         "check sorting routines", &
         u, results)
  end subroutine sorting_test


  subroutine sorting_1 (u)
    integer, intent(in) :: u
    integer, parameter :: NMAX = 10
    real(default), dimension(NMAX) :: rval
    integer, dimension(NMAX) :: ival
    real, dimension(NMAX,NMAX) :: harvest_r
    integer, dimension(NMAX,NMAX) :: harvest_i
    integer, dimension(NMAX,NMAX) :: harvest_a
    integer :: i, j
    harvest_r(:, 1) = [0.9976, 0., 0., 0., 0., 0., 0., 0., 0., 0.]
    harvest_r(:, 2) = [0.5668, 0.9659, 0., 0., 0., 0., 0., 0., 0., 0.]
    harvest_r(:, 3) = [0.7479, 0.3674, 0.4806, 0., 0., 0., 0., 0., 0., &
         0.]
    harvest_r(:, 4) = [0.0738, 0.0054, 0.3471, 0.3422, 0., 0., 0., 0., &
         0., 0.]
    harvest_r(:, 5) = [0.2180, 0.1332, 0.9005, 0.3868, 0.4455, 0., 0., &
         0., 0., 0.]
    harvest_r(:, 6) = [0.6619, 0.0161, 0.6509, 0.6464, 0.3230, &
         0.8557, 0., 0., 0., 0.]
    harvest_r(:, 7) = [0.4013, 0.2069, 0.9685, 0.5984, 0.6730, &
         0.4569, 0.3300, 0., 0., 0.]
    harvest_r(:, 8) = [0.1004, 0.7555, 0.6057, 0.7190, 0.8973, &
         0.6582, 0.1507, 0.6123, 0., 0.]
    harvest_r(:, 9) = [0.9787, 0.9991, 0.2568, 0.5509, 0.6590, &
         0.5540, 0.9778, 0.9019, 0.6579, 0.]
    harvest_r(:,10) = [0.7289, 0.4025, 0.9286, 0.1478, 0.6745, &
         0.7696, 0.3393, 0.1158, 0.6144, 0.8206]
    
    harvest_i(:, 1) = [18, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    harvest_i(:, 2) = [14, 9, 0, 0, 0, 0, 0, 0, 0, 0]
    harvest_i(:, 3) = [ 7, 8,11, 0, 0, 0, 0, 0, 0, 0]
    harvest_i(:, 4) = [19,19,14,19, 0, 0, 0, 0, 0, 0]
    harvest_i(:, 5) = [ 1,14,15,18,14, 0, 0, 0, 0, 0]
    harvest_i(:, 6) = [16,11, 1, 9,11, 2, 0, 0, 0, 0]
    harvest_i(:, 7) = [11,10,17, 6,13,13,10, 0, 0, 0]
    harvest_i(:, 8) = [ 5, 1, 2,10, 7, 0,15,12, 0, 0]
    harvest_i(:, 9) = [15,19, 2, 6,11, 0, 2, 4, 2, 0]
    harvest_i(:,10) = [ 1, 4, 8, 4,11, 0, 8, 7,19,13]
    
    harvest_a(:, 1) = [-6,  0,  0,  0,  0,  0,  0,  0,  0,  0]
    harvest_a(:, 2) = [-8, -9,  0,  0,  0,  0,  0,  0,  0,  0]
    harvest_a(:, 3) = [ 4, -3,  3,  0,  0,  0,  0,  0,  0,  0]
    harvest_a(:, 4) = [-6,  6,  2, -2,  0,  0,  0,  0,  0,  0]
    harvest_a(:, 5) = [ 1, -2,  0, -6,  8,  0,  0,  0,  0,  0]
    harvest_a(:, 6) = [-2, -1, -8, -5,  8, -5,  0,  0,  0,  0]
    harvest_a(:, 7) = [-9,  0, -6,  2,  5,  3,  2,  0,  0,  0]
    harvest_a(:, 8) = [-5, -7,  6,  7, -3,  0, -7,  4,  0,  0]
    harvest_a(:, 9) = [ 5,  0, -1, -7,  5,  2,  7, -3,  3,  0]
    harvest_a(:,10) = [-9,  2, -6,  3, -9,  5,  5,  7,  5, -9]


    write (u, "(A)")  "* Test output: Sorting"
    write (u, "(A)")  "*   Purpose: test sorting routines"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Sorting real values:"
    
    do i = 1, NMAX
       write (u, "(A)")
       rval(:i) = harvest_r(:i,i)
       write (u, "(10(1x,F7.4))") rval(:i)
       rval(:i) = sort (rval(:i))
       write (u, "(10(1x,F7.4))") rval(:i)
       do j = i, 2, -1
          if (rval(j)-rval(j-1) < 0) &
             write (u, "(A)") "*** Sorting failure. ***"
       end do
    end do
    
    write (u, "(A)")
    write (u, "(A)") "* Sorting integer values:"
    
    do i = 1, NMAX
       write (u, "(A)")
       ival(:i) = harvest_i(:i,i)
       write (u, "(10(1x,I2))") ival(:i)
       ival(:i) = sort (ival(:i))
       write (u, "(10(1x,I2))") ival(:i)
       do j = i, 2, -1
          if (ival(j)-ival(j-1) < 0) &
             write (u, "(A)")  "*** Sorting failure. ***"
       end do
    end do
    
    write (u, "(A)")
    write (u, "(A)") "* Sorting integer values by absolute value:"
    
    do i = 1, NMAX
       write (u, "(A)")
       ival(:i) = harvest_a(:i,i)
       write (u, "(10(1x,I2))") ival(:i)
       ival(:i) = sort_abs (ival(:i))
       write (u, "(10(1x,I2))") ival(:i)
       do j = i, 2, -1
          if (abs(ival(j))-abs(ival(j-1)) < 0 .or. &
               (abs(ival(j))==abs(ival(j-1))) .and. ival(j)>ival(j-1)) &
             write (u, "(A)")  "*** Sorting failure. ***"
       end do
    end do
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sorting_1"    

  end subroutine sorting_1


end module sorting
