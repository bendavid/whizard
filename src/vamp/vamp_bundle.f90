! exceptions.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module exceptions
  use kinds
  implicit none

  private :: raise_exception_s, raise_exception_v
  private :: clear_exception_s, clear_exception_v
  public :: handle_exception
  public :: raise_exception, clear_exception, gather_exceptions
  interface raise_exception
     module procedure raise_exception_s, raise_exception_v
  end interface
  interface clear_exception
     module procedure clear_exception_s, clear_exception_v
  end interface
  integer, public, parameter :: &
       EXC_NONE = 0, &
       EXC_INFO = 1, &
       EXC_WARN = 2, &
       EXC_ERROR = 3, &
       EXC_FATAL = 4
  integer, private, parameter :: EXC_DEFAULT = EXC_ERROR
  integer, private, parameter :: NAME_LENGTH = 64
  type, public :: exception
     integer :: level                                          
     character(len=NAME_LENGTH) :: message                     
     character(len=NAME_LENGTH) :: origin                      
  end type exception
  character(len=*), public, parameter :: EXCEPTIONS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine raise_exception_v (exc, level, message, origin)
    type(exception), dimension(:), intent(inout) :: exc
    integer, dimension(:), intent(in) :: level
    character(len=*), dimension(:), intent(in), optional :: message, origin
    integer :: i
    do i = 1, size (exc)
       call raise_exception_s (exc(i), level(i), message(i), origin(i))
    end do
  end subroutine raise_exception_v
  subroutine clear_exception_v (exc)
    type(exception), dimension(:), intent(inout) :: exc
    integer :: i
    do i = 1, size (exc)
       call clear_exception_s (exc(i))
    end do
  end subroutine clear_exception_v
  subroutine handle_exception (exc)
    type(exception), intent(inout) :: exc
    character(len=10) :: name
    if (exc%level > 0) then
       select case (exc%level)
       case (EXC_NONE)
          name = "(none)"
       case (EXC_INFO)
          name = "info"
       case (EXC_WARN)
          name = "warning"
       case (EXC_ERROR)
          name = "error"
       case (EXC_FATAL)
          name = "fatal"
       case default
          name = "invalid"
       end select
       print *, trim (exc%origin), ": ", trim(name), ": ", trim (exc%message)
       if (exc%level >= EXC_FATAL) then
          print *, "terminated."
          stop
       end if
    end if
  end subroutine handle_exception
  subroutine raise_exception_s (exc, level, origin, message)
    type(exception), intent(inout), optional :: exc
    integer, intent(in), optional :: level
    character(len=*), intent(in), optional :: origin, message
    integer :: local_level
    if (present (exc)) then
       if (present (level)) then
          local_level = level
       else
          local_level = EXC_DEFAULT
       end if
       if (exc%level < local_level) then
          exc%level = local_level
          if (present (origin)) then
             exc%origin = origin
          else
             exc%origin = "[vamp]"
          end if
          if (present (message)) then
             exc%message = message
          else
             exc%message = "[vamp]"
          end if
       end if
    end if
  end subroutine raise_exception_s
  subroutine clear_exception_s (exc)
    type(exception), intent(inout) :: exc
    exc%level = 0
    exc%message = ""
    exc%origin = ""
  end subroutine clear_exception_s
  subroutine gather_exceptions (exc, excs)
    type(exception), intent(inout) :: exc
    type(exception), dimension(:), intent(in) :: excs
    integer :: i
    i = sum (maxloc (excs%level))
    if (exc%level < excs(i)%level) then
       call raise_exception_s (exc, excs(i)%level, excs(i)%origin, &
            excs(i)%message)
    end if
  end subroutine gather_exceptions
end module exceptions
! stat.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module vamp_stat
  use kinds
  implicit none

  public:: average, standard_deviation, value_spread
  public :: standard_deviation_percent, value_spread_percent
  character(len=*), public, parameter :: STAT_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  function average (x) result (a)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: a
    integer :: n
    n = size (x)
    if (n == 0) then
       a = 0.0
    else
       a = sum (x) / n
    end if
  end function average
  function standard_deviation (x) result (s)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: s
    integer :: n
    n = size (x)
    if (n < 2) then
       s = huge (s)
    else
       s = sqrt (max ((sum (x**2) / n - (average (x))**2) / (n - 1), &
            0.0_default))
    end if
  end function standard_deviation
  function value_spread (x) result (s)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: s
    s = maxval(x) - minval(x)
  end function value_spread
  function standard_deviation_percent (x) result (s)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: s
    real(kind=default) :: abs_avg
    abs_avg = abs (average (x))
    if (abs_avg <= tiny (abs_avg)) then
       s = huge (s)
    else
       s = 100.0 * standard_deviation (x) / abs_avg
    end if
  end function standard_deviation_percent
  function value_spread_percent (x) result (s)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: s
    real(kind=default) :: abs_avg
    abs_avg = abs (average (x))
    if (abs_avg <= tiny (abs_avg)) then
       s = huge (s)
    else
       s = 100.0 * value_spread (x) / abs_avg
    end if
  end function value_spread_percent
end module vamp_stat
! utils.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module utils
  use kinds
  implicit none

  private :: swap_integer_array, swap_real_array
  private :: gcd_s, gcd_v, gcd_a
  private :: lcm_s, lcm_v, lcm_a
  public :: create_array_pointer
  private :: create_integer_array_pointer
  private :: create_real_array_pointer
  private :: create_integer_array2_pointer
  private :: create_real_array2_pointer
  public :: copy_array_pointer
  private :: copy_integer_array_pointer
  private :: copy_real_array_pointer
  private :: copy_integer_array2_pointer
  private :: copy_real_array2_pointer
  public :: swap
  private :: swap_integer, swap_real
  public :: sort
  private :: sort_real, sort_real_and_real_array, sort_real_and_integer
  public :: outer_product
  public :: factorize, gcd, lcm
  private :: gcd_internal
  public :: find_free_unit
  integer, dimension(13), parameter, private :: &
       PRIMES = (/ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41 /)
  integer, parameter, private :: MIN_UNIT = 11, MAX_UNIT = 99
  interface swap
     module procedure swap_integer_array, swap_real_array
  end interface
  interface gcd
     module procedure gcd_s, gcd_v, gcd_a
  end interface
  interface lcm
     module procedure lcm_s, lcm_v, lcm_a
  end interface
  interface create_array_pointer
     module procedure &
          create_integer_array_pointer, &
          create_real_array_pointer, &
          create_integer_array2_pointer, &
          create_real_array2_pointer
  end interface
  interface copy_array_pointer
     module procedure &
          copy_integer_array_pointer, &
          copy_real_array_pointer, &
          copy_integer_array2_pointer, &
          copy_real_array2_pointer
  end interface
  interface swap
     module procedure swap_integer, swap_real
  end interface
  interface sort
     module procedure &
          sort_real, sort_real_and_real_array, &
          sort_real_and_integer
  end interface
  character(len=*), public, parameter :: UTILS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine swap_integer_array (a, b)
    integer, dimension(:), intent(inout) :: a, b
    integer, dimension(max(size(a),size(b))) :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine swap_integer_array
  subroutine swap_real_array (a, b)
    real(kind=default), dimension(:), intent(inout) :: a, b
    real(kind=default), dimension(max(size(a),size(b))) :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine swap_real_array
  function gcd_v (m, n) result (gcd_m_n)
    integer, dimension(:), intent(in) :: m, n
    integer, dimension(size(m)) :: gcd_m_n
    integer :: i
    do i = 1, size (m)
       gcd_m_n(i) = gcd_s (m(i), n(i))
    end do
  end function gcd_v
  function lcm_v (m, n) result (lcm_m_n)
    integer, dimension(:), intent(in) :: m, n
    integer, dimension(size(m)) :: lcm_m_n
    integer :: i
    do i = 1, size (m)
       lcm_m_n(i) = lcm_s (m(i), n(i))
    end do
  end function lcm_v
  function gcd_a (m, n) result (gcd_m_n)
    integer, dimension(:), intent(in) :: m
    integer, intent(in) :: n
    integer, dimension(size(m)) :: gcd_m_n
    integer :: i
    do i = 1, size (m)
       gcd_m_n(i) = gcd_s (m(i), n)
    end do
  end function gcd_a
  function lcm_a (m, n) result (lcm_m_n)
    integer, dimension(:), intent(in) :: m
    integer, intent(in) :: n
    integer, dimension(size(m)) :: lcm_m_n
    integer :: i
    do i = 1, size (m)
       lcm_m_n(i) = lcm_s (m(i), n)
    end do
  end function lcm_a
  subroutine create_integer_array_pointer (lhs, n, lb)
    integer, dimension(:), pointer :: lhs
    integer, intent(in) :: n
    integer, intent(in), optional :: lb
    if (associated (lhs)) then
       if (size (lhs) /= n) then
          deallocate (lhs)
          if (present (lb)) then
             allocate (lhs(lb:n+lb-1))
          else
             allocate (lhs(n))
          end if
       end if
    else
       if (present (lb)) then
          allocate (lhs(lb:n+lb-1))
       else
          allocate (lhs(n))
       end if
    end if
    lhs = 0
  end subroutine create_integer_array_pointer
  subroutine create_real_array_pointer (lhs, n, lb)
    real(kind=default), dimension(:), pointer :: lhs
    integer, intent(in) :: n
    integer, intent(in), optional :: lb
    if (associated (lhs)) then
       if (size (lhs) /= n) then
          deallocate (lhs)
          if (present (lb)) then
             allocate (lhs(lb:n+lb-1))
          else
             allocate (lhs(n))
          end if
       end if
    else
       if (present (lb)) then
          allocate (lhs(lb:n+lb-1))
       else
          allocate (lhs(n))
       end if
    end if
    lhs = 0
  end subroutine create_real_array_pointer
  subroutine create_integer_array2_pointer (lhs, n, lb)
    integer, dimension(:,:), pointer :: lhs
    integer, dimension(:), intent(in) :: n
    integer, dimension(:), intent(in), optional :: lb
    if (associated (lhs)) then
       if (any (ubound (lhs) /= n)) then
          deallocate (lhs)
          if (present (lb)) then
             allocate (lhs(lb(1):n(1)+lb(1)-1,lb(2):n(2)+lb(2)-1))
          else
             allocate (lhs(n(1),n(2)))
          end if
       end if
    else
       if (present (lb)) then
          allocate (lhs(lb(1):n(1)+lb(1)-1,lb(2):n(2)+lb(2)-1))
       else
          allocate (lhs(n(1),n(2)))
       end if
    end if
    lhs = 0
  end subroutine create_integer_array2_pointer
  subroutine create_real_array2_pointer (lhs, n, lb)
    real(kind=default), dimension(:,:), pointer :: lhs
    integer, dimension(:), intent(in) :: n
    integer, dimension(:), intent(in), optional :: lb
    if (associated (lhs)) then
       if (any (ubound (lhs) /= n)) then
          deallocate (lhs)
          if (present (lb)) then
             allocate (lhs(lb(1):n(1)+lb(1)-1,lb(2):n(2)+lb(2)-1))
          else
             allocate (lhs(n(1),n(2)))
          end if
       end if
    else
       if (present (lb)) then
          allocate (lhs(lb(1):n(1)+lb(1)-1,lb(2):n(2)+lb(2)-1))
       else
          allocate (lhs(n(1),n(2)))
       end if
    end if
    lhs = 0
  end subroutine create_real_array2_pointer
  subroutine copy_integer_array_pointer (lhs, rhs, lb)
    integer, dimension(:), pointer :: lhs
    integer, dimension(:), intent(in) :: rhs
    integer, intent(in), optional :: lb
    call create_integer_array_pointer (lhs, size (rhs), lb)
    lhs = rhs
  end subroutine copy_integer_array_pointer
  subroutine copy_real_array_pointer (lhs, rhs, lb)
    real(kind=default), dimension(:), pointer :: lhs
    real(kind=default), dimension(:), intent(in) :: rhs
    integer, intent(in), optional :: lb
    call create_real_array_pointer (lhs, size (rhs), lb)
    lhs = rhs
  end subroutine copy_real_array_pointer
  subroutine copy_integer_array2_pointer (lhs, rhs, lb)
    integer, dimension(:,:), pointer :: lhs
    integer, dimension(:,:), intent(in) :: rhs
    integer, dimension(:), intent(in), optional :: lb
    call create_integer_array2_pointer &
         (lhs, (/ size (rhs, dim=1), size (rhs, dim=2) /), lb)
    lhs = rhs
  end subroutine copy_integer_array2_pointer
  subroutine copy_real_array2_pointer (lhs, rhs, lb)
    real(kind=default), dimension(:,:), pointer :: lhs
    real(kind=default), dimension(:,:), intent(in) :: rhs
    integer, dimension(:), intent(in), optional :: lb
    call create_real_array2_pointer &
         (lhs, (/ size (rhs, dim=1), size (rhs, dim=2) /), lb)
    lhs = rhs
  end subroutine copy_real_array2_pointer
  subroutine swap_integer (a, b)
    integer, intent(inout) :: a, b
    integer :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine swap_integer
  subroutine swap_real (a, b)
    real(kind=default), intent(inout) :: a, b
    real(kind=default) :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine swap_real
  subroutine sort_real (key, reverse)
    real(kind=default), dimension(:), intent(inout) :: key
    logical, intent(in), optional :: reverse
    logical :: rev
    integer :: i, j
    if (present (reverse)) then
       rev = reverse
    else
       rev = .false.
    end if
    do i = 1, size (key) - 1
       if (rev) then
          j = sum (maxloc (key(i:))) + i - 1
       else
          j = sum (minloc (key(i:))) + i - 1
       end if
       if (j /= i) then
          call swap (key(i), key(j))
       end if
    end do
  end subroutine sort_real
  subroutine sort_real_and_real_array (key, table, reverse)
    real(kind=default), dimension(:), intent(inout) :: key
    real(kind=default), dimension(:,:), intent(inout) :: table
    logical, intent(in), optional :: reverse
    logical :: rev
    integer :: i, j
    if (present (reverse)) then
       rev = reverse
    else
       rev = .false.
    end if
    do i = 1, size (key) - 1
       if (rev) then
          j = sum (maxloc (key(i:))) + i - 1
       else
          j = sum (minloc (key(i:))) + i - 1
       end if
       if (j /= i) then
          call swap (key(i), key(j))
          call swap (table(:,i), table(:,j))
       end if
    end do
  end subroutine sort_real_and_real_array
  subroutine sort_real_and_integer (key, table, reverse)
    real(kind=default), dimension(:), intent(inout) :: key
    integer, dimension(:), intent(inout) :: table
    logical, intent(in), optional :: reverse
    logical :: rev
    integer :: i, j
    if (present (reverse)) then
       rev = reverse
    else
       rev = .false.
    end if
    do i = 1, size (key) - 1
       if (rev) then
          j = sum (maxloc (key(i:))) + i - 1
       else
          j = sum (minloc (key(i:))) + i - 1
       end if
       if (j /= i) then
          call swap (key(i), key(j))
          call swap (table(i), table(j))
       end if
    end do
  end subroutine sort_real_and_integer
  function outer_product (x, y) result (xy)
    real(kind=default), dimension(:), intent(in) :: x, y
    real(kind=default), dimension(size(x),size(y)) :: xy
    xy = spread (x, dim=2, ncopies=size(y)) &
         * spread (y, dim=1, ncopies=size(x))
  end function outer_product
  recursive function gcd_internal (m, n) result (gcd_m_n)
    integer, intent(in) :: m, n
    integer :: gcd_m_n
    if (n <= 0) then
       gcd_m_n = m
    else
       gcd_m_n = gcd_internal (n, modulo (m, n))
    end if
  end function gcd_internal
  function gcd_s (m, n) result (gcd_m_n)
    integer, intent(in) :: m, n
    integer :: gcd_m_n
    gcd_m_n = gcd_internal (m, n)
  end function gcd_s
  function lcm_s (m, n) result (lcm_m_n)
    integer, intent(in) :: m, n
    integer :: lcm_m_n
    lcm_m_n = (m * n) / gcd_s (m, n)
  end function lcm_s
  subroutine factorize (n, factors, i)
    integer, intent(in) :: n
    integer, dimension(:), intent(out) :: factors
    integer, intent(out) :: i
    integer :: nn, p
    nn = n
    i = 0
    do p = 1, size (PRIMES)
       try: do
          if (modulo (nn, PRIMES(p)) == 0) then
             i = i + 1
             factors(i) = PRIMES(p)
             nn = nn / PRIMES(p)
             if (i >= size (factors)) then
                factors(i) = nn
                return
             end if
          else
             exit try
          end if
       end do try
       if (nn == 1) then
          return
       end if
    end do
  end subroutine factorize
  subroutine find_free_unit (u, iostat)
    integer, intent(out) :: u
    integer, intent(out), optional :: iostat
    logical :: exists, is_open
    integer :: i, status
    do i = MIN_UNIT, MAX_UNIT
       inquire (unit = i, exist = exists, opened = is_open, &
            iostat = status)
       if (status == 0) then
          if (exists .and. .not. is_open) then
             u = i
             if (present (iostat)) then
                iostat = 0
             end if
             return
          end if
       end if
    end do
    if (present (iostat)) then
       iostat = -1
    end if
    u = -1
  end subroutine find_free_unit
end module utils
! tao_random_numbers.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module tao_random_numbers
  use kinds

  implicit none
  private :: generate
  private :: seed_static, seed_state, seed_raw_state
  private :: seed_stateless
  private :: create_state_from_seed, create_raw_state_from_seed, &
       create_state_from_state, create_raw_state_from_state, &
       create_state_from_raw_state, create_raw_state_from_raw_st
  private :: destroy_state, destroy_raw_state
  public :: assignment(=)
  private :: copy_state, copy_raw_state, &
       copy_raw_state_to_state, copy_state_to_raw_state
  private :: write_state_unit, write_state_name
  private :: write_raw_state_unit, write_raw_state_name
  private :: read_state_unit, read_state_name
  private :: read_raw_state_unit, read_raw_state_name
  private :: find_free_unit
  public :: tao_random_marshal
  private :: marshal_state, marshal_raw_state
  public :: tao_random_marshal_size
  private :: marshal_state_size, marshal_raw_state_size
  public :: tao_random_unmarshal
  private :: unmarshal_state, unmarshal_raw_state
  public :: tao_random_number
  public :: tao_random_seed
  public :: tao_random_create
  public :: tao_random_destroy
  public :: tao_random_copy
  public :: tao_random_read
  public :: tao_random_write
  public :: tao_random_flush
!  public :: tao_random_luxury
  public :: tao_random_test
!   private :: luxury_stateless
!   private :: luxury_static, luxury_state, &
!        luxury_static_integer, luxury_state_integer, &
!        luxury_static_real, luxury_state_real, &
!        luxury_static_double, luxury_state_double
  private :: write_state_array
  private :: read_state_array
  private :: &
       integer_stateless, integer_array_stateless, &
       real_stateless, real_array_stateless, &
       double_stateless, double_array_stateless
  private :: integer_static, integer_state, &
       integer_array_static, integer_array_state, &
       real_static, real_state, real_array_static, real_array_state, &
       double_static, double_state, double_array_static, double_array_state
  interface tao_random_seed
     module procedure seed_static, seed_state, seed_raw_state
  end interface
  interface tao_random_create
     module procedure create_state_from_seed, create_raw_state_from_seed, &
          create_state_from_state, create_raw_state_from_state, &
          create_state_from_raw_state, create_raw_state_from_raw_st
  end interface
  interface tao_random_destroy
     module procedure destroy_state, destroy_raw_state
  end interface
  interface tao_random_copy
     module procedure copy_state, copy_raw_state, &
          copy_raw_state_to_state, copy_state_to_raw_state
  end interface
  interface assignment(=)
     module procedure copy_state, copy_raw_state, &
          copy_raw_state_to_state, copy_state_to_raw_state
  end interface
  interface tao_random_write
     module procedure &
          write_state_unit, write_state_name, &
          write_raw_state_unit, write_raw_state_name
  end interface
  interface tao_random_read
     module procedure &
          read_state_unit, read_state_name, &
          read_raw_state_unit, read_raw_state_name
  end interface
  interface tao_random_marshal_size
     module procedure marshal_state_size, marshal_raw_state_size
  end interface
  interface tao_random_marshal
     module procedure marshal_state, marshal_raw_state
  end interface
  interface tao_random_unmarshal
     module procedure unmarshal_state, unmarshal_raw_state
  end interface
!   interface tao_random_luxury
!      module procedure luxury_static, luxury_state, &
!           luxury_static_integer, luxury_state_integer, &
!           luxury_static_real, luxury_state_real, &
!           luxury_static_double, luxury_state_double
!   end interface
  interface tao_random_number
     module procedure integer_static, integer_state, &
          integer_array_static, integer_array_state, &
          real_static, real_state, real_array_static, real_array_state, &
          double_static, double_state, double_array_static, double_array_state
  end interface
  integer, parameter, private :: K = 100, L = 37
  integer, parameter, private :: DEFAULT_BUFFER_SIZE = 1009
  integer, parameter, private :: MIN_UNIT = 11, MAX_UNIT = 99
  integer(kind=i32), parameter, private :: M = 2**30
  integer(kind=i32), dimension(K), save, private :: s_state
  logical, save, private :: s_virginal = .true.
  integer(kind=i32), dimension(DEFAULT_BUFFER_SIZE), save, private :: s_buffer
  integer, save, private :: s_buffer_end = size (s_buffer)
  integer, save, private :: s_last = size (s_buffer)
  type, public :: tao_random_raw_state

     integer(kind=i32), dimension(K) :: x
  end type tao_random_raw_state
  type, public :: tao_random_state

     type(tao_random_raw_state) :: state
     integer(kind=i32), dimension(:), pointer :: buffer       
     integer :: buffer_end, last
  end type tao_random_state
  character(len=*), public, parameter :: TAO_RANDOM_NUMBERS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine seed_static (seed)
    integer, optional, intent(in) :: seed
    call seed_stateless (s_state, seed)
    s_virginal = .false.
    s_last = size (s_buffer)
  end subroutine seed_static
  subroutine seed_raw_state (s, seed)
    type(tao_random_raw_state), intent(inout) :: s
    integer, optional, intent(in) :: seed
    call seed_stateless (s%x, seed)
  end subroutine seed_raw_state
  subroutine seed_state (s, seed)
    type(tao_random_state), intent(inout) :: s
    integer, optional, intent(in) :: seed
    call seed_raw_state (s%state, seed)
    s%last = size (s%buffer)
  end subroutine seed_state
  subroutine create_state_from_seed (s, seed, buffer_size)
    type(tao_random_state), intent(out) :: s
    integer, intent(in) :: seed
    integer, intent(in), optional :: buffer_size
    call create_raw_state_from_seed (s%state, seed)
    if (present (buffer_size)) then
       s%buffer_end = max (buffer_size, K)
    else
       s%buffer_end = DEFAULT_BUFFER_SIZE
    end if
    allocate (s%buffer(s%buffer_end))
    call tao_random_flush (s)
  end subroutine create_state_from_seed
  subroutine create_state_from_state (s, state)
    type(tao_random_state), intent(out) :: s
    type(tao_random_state), intent(in) :: state
    call create_raw_state_from_raw_st (s%state, state%state)
    allocate (s%buffer(size(state%buffer)))
    call tao_random_copy (s, state)
  end subroutine create_state_from_state
  subroutine create_state_from_raw_state &
       (s, raw_state, buffer_size)
    type(tao_random_state), intent(out) :: s
    type(tao_random_raw_state), intent(in) :: raw_state
    integer, intent(in), optional :: buffer_size
    call create_raw_state_from_raw_st (s%state, raw_state)
    if (present (buffer_size)) then
       s%buffer_end = max (buffer_size, K)
    else
       s%buffer_end = DEFAULT_BUFFER_SIZE
    end if
    allocate (s%buffer(s%buffer_end))
    call tao_random_flush (s)
  end subroutine create_state_from_raw_state
  subroutine create_raw_state_from_seed (s, seed)
    type(tao_random_raw_state), intent(out) :: s
    integer, intent(in) :: seed
    call seed_raw_state (s, seed)
  end subroutine create_raw_state_from_seed
  subroutine create_raw_state_from_state (s, state)
    type(tao_random_raw_state), intent(out) :: s
    type(tao_random_state), intent(in) :: state
    call copy_state_to_raw_state (s, state)
  end subroutine create_raw_state_from_state
  subroutine create_raw_state_from_raw_st (s, raw_state)
    type(tao_random_raw_state), intent(out) :: s
    type(tao_random_raw_state), intent(in) :: raw_state
    call copy_raw_state (s, raw_state)
  end subroutine create_raw_state_from_raw_st
  subroutine destroy_state (s)
    type(tao_random_state), intent(inout) :: s
    deallocate (s%buffer)
  end subroutine destroy_state
  subroutine destroy_raw_state (s)
    type(tao_random_raw_state), intent(inout) :: s
  end subroutine destroy_raw_state
  subroutine copy_state (lhs, rhs)
    type(tao_random_state), intent(inout) :: lhs
    type(tao_random_state), intent(in) :: rhs
    call copy_raw_state (lhs%state, rhs%state)
    if (size (lhs%buffer) /= size (rhs%buffer)) then
       deallocate (lhs%buffer)
       allocate (lhs%buffer(size(rhs%buffer)))
    end if
    lhs%buffer = rhs%buffer
    lhs%buffer_end = rhs%buffer_end
    lhs%last = rhs%last
  end subroutine copy_state
  subroutine copy_raw_state (lhs, rhs)
    type(tao_random_raw_state), intent(out) :: lhs
    type(tao_random_raw_state), intent(in) :: rhs
    lhs%x = rhs%x
  end subroutine copy_raw_state
  subroutine copy_raw_state_to_state (lhs, rhs)
    type(tao_random_state), intent(inout) :: lhs
    type(tao_random_raw_state), intent(in) :: rhs
    call copy_raw_state (lhs%state, rhs)
    call tao_random_flush (lhs)
  end subroutine copy_raw_state_to_state
  subroutine copy_state_to_raw_state (lhs, rhs)
    type(tao_random_raw_state), intent(out) :: lhs
    type(tao_random_state), intent(in) :: rhs
    call copy_raw_state (lhs, rhs%state)
  end subroutine copy_state_to_raw_state
  subroutine tao_random_flush (s)
    type(tao_random_state), intent(inout) :: s
    s%last = size (s%buffer)
  end subroutine tao_random_flush
  subroutine write_state_unit (s, unit)
    type(tao_random_state), intent(in) :: s
    integer, intent(in) :: unit
    write (unit = unit, fmt = *) "BEGIN TAO_RANDOM_STATE"
    call write_raw_state_unit (s%state, unit)
    write (unit = unit, fmt = "(2(1x,a16,1x,i10/),1x,a16,1x,i10)") &
         "BUFFER_SIZE", size (s%buffer), &
         "BUFFER_END", s%buffer_end, &
         "LAST", s%last
    write (unit = unit, fmt = *) "BEGIN BUFFER"
    call write_state_array (s%buffer, unit)
    write (unit = unit, fmt = *) "END BUFFER"
    write (unit = unit, fmt = *) "END TAO_RANDOM_STATE"
  end subroutine write_state_unit
  subroutine read_state_unit (s, unit)
    type(tao_random_state), intent(inout) :: s
    integer, intent(in) :: unit
    integer :: buffer_size
    read (unit = unit, fmt = *)
    call read_raw_state_unit (s%state, unit)
    read (unit = unit, fmt = "(2(1x,16x,1x,i10/),1x,16x,1x,i10)") &
         buffer_size, s%buffer_end, s%last
    read (unit = unit, fmt = *)
    if (buffer_size /= size (s%buffer)) then
       deallocate (s%buffer)
       allocate (s%buffer(buffer_size))
    end if
    call read_state_array (s%buffer, unit)
    read (unit = unit, fmt = *)
    read (unit = unit, fmt = *)
  end subroutine read_state_unit
  subroutine write_raw_state_unit (s, unit)
    type(tao_random_raw_state), intent(in) :: s
    integer, intent(in) :: unit
    write (unit = unit, fmt = *) "BEGIN TAO_RANDOM_RAW_STATE"
    call write_state_array (s%x, unit)
    write (unit = unit, fmt = *) "END TAO_RANDOM_RAW_STATE"
  end subroutine write_raw_state_unit
  subroutine read_raw_state_unit (s, unit)
    type(tao_random_raw_state), intent(inout) :: s
    integer, intent(in) :: unit
    read (unit = unit, fmt = *)
    call read_state_array (s%x, unit)
    read (unit = unit, fmt = *)
  end subroutine read_raw_state_unit
  subroutine find_free_unit (u, iostat)
    integer, intent(out) :: u
    integer, intent(out), optional :: iostat
    logical :: exists, is_open
    integer :: i, status
    do i = MIN_UNIT, MAX_UNIT
       inquire (unit = i, exist = exists, opened = is_open, &
            iostat = status)
       if (status == 0) then
          if (exists .and. .not. is_open) then
             u = i
             if (present (iostat)) then
                iostat = 0
             end if
             return
          end if
       end if
    end do
    if (present (iostat)) then
       iostat = -1
    end if
    u = -1
  end subroutine find_free_unit
  subroutine write_state_name (s, name)
    type(tao_random_state), intent(in) :: s
    character(len=*), intent(in) :: name
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", file = name)
    call write_state_unit (s, unit)
    close (unit = unit)
  end subroutine write_state_name
  subroutine write_raw_state_name (s, name)
    type(tao_random_raw_state), intent(in) :: s
    character(len=*), intent(in) :: name
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", file = name)
    call write_raw_state_unit (s, unit)
    close (unit = unit)
  end subroutine write_raw_state_name
  subroutine read_state_name (s, name)
    type(tao_random_state), intent(inout) :: s
    character(len=*), intent(in) :: name
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", file = name)
    call read_state_unit (s, unit)
    close (unit = unit)
  end subroutine read_state_name
  subroutine read_raw_state_name (s, name)
    type(tao_random_raw_state), intent(inout) :: s
    character(len=*), intent(in) :: name
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", file = name)
    call read_raw_state_unit (s, unit)
    close (unit = unit)
  end subroutine read_raw_state_name
  subroutine double_state (s, r)
    type(tao_random_state), intent(inout) :: s
    real(kind=double), intent(out) :: r
    call double_stateless (s%state%x, s%buffer, s%buffer_end, s%last, r)
  end subroutine double_state
  subroutine double_array_state (s, v, num)
    type(tao_random_state), intent(inout) :: s
    real(kind=double), dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    call double_array_stateless &
         (s%state%x, s%buffer, s%buffer_end, s%last, v, num)
  end subroutine double_array_state
  subroutine double_static (r)
    real(kind=double), intent(out) :: r
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call double_stateless (s_state, s_buffer, s_buffer_end, s_last, r)
  end subroutine double_static
  subroutine double_array_static (v, num)
    real(kind=double), dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call double_array_stateless &
         (s_state, s_buffer, s_buffer_end, s_last, v, num)
  end subroutine double_array_static
  subroutine luxury_stateless &
       (buffer_size, buffer_end, last, consumption)
    integer, intent(in) :: buffer_size
    integer, intent(inout) :: buffer_end
    integer, intent(inout) :: last
    integer, intent(in) :: consumption
    if (consumption >= 1 .and. consumption <= buffer_size) then
       buffer_end = consumption
       last = min (last, buffer_end)
    else
!!! print *, "tao_random_luxury: ", "invalid consumption ", &
     !!!      consumption, ", not in [ 1,", buffer_size, "]."
       buffer_end = buffer_size
    end if
  end subroutine luxury_stateless
  subroutine luxury_state (s)
    type(tao_random_state), intent(inout) :: s
    call luxury_state_integer (s, size (s%buffer))
  end subroutine luxury_state
  subroutine luxury_state_integer (s, consumption)
    type(tao_random_state), intent(inout) :: s
    integer, intent(in) :: consumption
    call luxury_stateless (size (s%buffer), s%buffer_end, s%last, consumption)
  end subroutine luxury_state_integer
  subroutine luxury_state_real (s, consumption)
    type(tao_random_state), intent(inout) :: s
    real, intent(in) :: consumption
    call luxury_state_integer (s, int (consumption * size (s%buffer)))
  end subroutine luxury_state_real
  subroutine luxury_state_double (s, consumption)
    type(tao_random_state), intent(inout) :: s
    real(kind=double), intent(in) :: consumption
    call luxury_state_integer (s, int (consumption * size (s%buffer)))
  end subroutine luxury_state_double
  subroutine luxury_static ()
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call luxury_static_integer (size (s_buffer))
  end subroutine luxury_static
  subroutine luxury_static_integer (consumption)
    integer, intent(in) :: consumption
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call luxury_stateless (size (s_buffer), s_buffer_end, s_last, consumption)
  end subroutine luxury_static_integer
  subroutine luxury_static_real (consumption)
    real, intent(in) :: consumption
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call luxury_static_integer (int (consumption * size (s_buffer)))
  end subroutine luxury_static_real
  subroutine luxury_static_double (consumption)
    real(kind=double), intent(in) :: consumption
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call luxury_static_integer (int (consumption * size (s_buffer)))
  end subroutine luxury_static_double
  subroutine generate (a, state)
    integer(kind=i32), dimension(:), intent(inout) :: a, state
    integer :: j, n
    n = size (a)
    a(1:K) = state(1:K)
    do j = K+1, n
       a(j) = modulo (a(j-K) - a(j-L), M)
    end do
    state(1:L) = modulo (a(n+1-K:n+L-K) - a(n+1-L:n), M)
    do j = L+1, K
       state(j) = modulo (a(n+j-K) - state(j-L), M)
    end do
  end subroutine generate
  subroutine seed_stateless (state, seed)
    integer(kind=i32), dimension(:), intent(out) :: state
    integer, optional, intent(in) :: seed
    integer, parameter :: DEFAULT_SEED = 0
    integer, parameter :: MAX_SEED = 2**30 - 3
    integer, parameter :: TT = 70
    integer :: seed_value, j, s, t
    integer(kind=i32), dimension(2*K-1) :: x
    if (present (seed)) then
       seed_value = seed
    else
       seed_value = DEFAULT_SEED
    end if
    if (seed_value < 0 .or. seed_value > MAX_SEED) then
!!! print *, "tao_random_seed: seed (", seed_value, &
     !!!      ") not in [ 0,", MAX_SEED, "]!"
       seed_value = modulo (abs (seed_value), MAX_SEED + 1)
!!! print *, "tao_random_seed: seed set to ", seed_value, "!"
    end if
    s = seed_value - modulo (seed_value, 2) + 2
    do j = 1, K
       x(j) = s
       s = 2*s
       if (s >= M) then
          s = s - M + 2
       end if
    end do
    x(K+1:2*K-1) = 0
    x(2) = x(2) + 1
    s = seed_value
    t = TT - 1
    do
       x(3:2*K-1:2) = x(2:K)
       x(2:K+L-1:2) = x(2*K-1:K-L+2:-2) - modulo (x(2*K-1:K-L+2:-2), 2)
       do j= 2*K-1, K+1, -1
          if (modulo (x(j), 2) == 1) then
             x(j-(K-L)) = modulo (x(j-(K-L)) - x(j), M)
             x(j-K) = modulo (x(j-K) - x(j), M)
          end if
       end do
       if (modulo (s, 2) == 1) then
          x(2:K+1) = x(1:K)
          x(1) = x(K+1)
          if (modulo (x(K+1), 2) == 1) then
             x(L+1) = modulo (x(L+1) - x(K+1), M)
          end if
       end if
       if (s /= 0) then
          s = s / 2
       else
          t = t - 1
       end if
       if (t <= 0) then
          exit
       end if
    end do
    state(K-L+1:K) = x(1:L)
    state(1:K-L) = x(L+1:K)
  end subroutine seed_stateless
  subroutine write_state_array (a, unit)
    integer(kind=i32), dimension(:), intent(in) :: a
    integer, intent(in) :: unit
    integer :: i
    do i = 1, size (a)
       write (unit = unit, fmt = "(1x,i10,1x,i10)") i, a(i)
    end do
  end subroutine write_state_array
  subroutine read_state_array (a, unit)
    integer(kind=i32), dimension(:), intent(inout) :: a
    integer, intent(in) :: unit
    integer :: i, idum
    do i = 1, size (a)
       read (unit = unit, fmt = *) idum, a(i)
    end do
  end subroutine read_state_array
  subroutine marshal_state (s, ibuf, dbuf)
    type(tao_random_state), intent(in) :: s
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=double), dimension(:), intent(inout) :: dbuf
    integer :: buf_size
    buf_size = size (s%buffer)
    ibuf(1) = s%buffer_end
    ibuf(2) = s%last
    ibuf(3) = buf_size
    ibuf(4:3+buf_size) = s%buffer
    call marshal_raw_state (s%state, ibuf(4+buf_size:), dbuf)
  end subroutine marshal_state
  subroutine marshal_state_size (s, iwords, dwords)
    type(tao_random_state), intent(in) :: s
    integer, intent(out) :: iwords, dwords
    call marshal_raw_state_size (s%state, iwords, dwords)
    iwords = iwords + 3 + size (s%buffer)
  end subroutine marshal_state_size
  subroutine unmarshal_state (s, ibuf, dbuf)
    type(tao_random_state), intent(inout) :: s
    integer, dimension(:), intent(in) :: ibuf
    real(kind=double), dimension(:), intent(in) :: dbuf
    integer :: buf_size
    s%buffer_end = ibuf(1)
    s%last = ibuf(2)
    buf_size = ibuf(3)
    s%buffer = ibuf(4:3+buf_size)
    call unmarshal_raw_state (s%state, ibuf(4+buf_size:), dbuf)
  end subroutine unmarshal_state
  subroutine marshal_raw_state (s, ibuf, dbuf)
    type(tao_random_raw_state), intent(in) :: s
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=double), dimension(:), intent(inout) :: dbuf
    ibuf(1) = size (s%x)
    ibuf(2:1+size(s%x)) = s%x
  end subroutine marshal_raw_state
  subroutine marshal_raw_state_size (s, iwords, dwords)
    type(tao_random_raw_state), intent(in) :: s
    integer, intent(out) :: iwords, dwords
    iwords = 1 + size (s%x)
    dwords = 0
  end subroutine marshal_raw_state_size
  subroutine unmarshal_raw_state (s, ibuf, dbuf)
    type(tao_random_raw_state), intent(inout) :: s
    integer, dimension(:), intent(in) :: ibuf
    real(kind=double), dimension(:), intent(in) :: dbuf
    integer :: buf_size
    buf_size = ibuf(1)
    s%x = ibuf(2:1+buf_size)
  end subroutine unmarshal_raw_state
  subroutine integer_stateless &
       (state, buffer, buffer_end, last, r)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    integer, intent(out) :: r
    integer, parameter :: NORM = 1
    last = last + 1
    if (last > buffer_end) then
       call generate (buffer, state)
       last = 1
    end if
    r = NORM * buffer(last) 
  end subroutine integer_stateless
  subroutine real_stateless (state, buffer, buffer_end, last, r)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    real, intent(out) :: r
    real, parameter :: NORM = 1.0 / M
    last = last + 1
    if (last > buffer_end) then
       call generate (buffer, state)
       last = 1
    end if
    r = NORM * buffer(last) 
  end subroutine real_stateless
  subroutine double_stateless (state, buffer, buffer_end, last, r)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    real(kind=double), intent(out) :: r
    real(kind=double), parameter :: NORM = 1.0_double / M
    last = last + 1
    if (last > buffer_end) then
       call generate (buffer, state)
       last = 1
    end if
    r = NORM * buffer(last) 
  end subroutine double_stateless
  subroutine integer_array_stateless &
       (state, buffer, buffer_end, last, v, num)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    integer, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    integer, parameter :: NORM = 1
    integer :: nu, done, todo, chunk
    if (present (num)) then
       nu = num
    else
       nu = size (v)
    end if
    if (last >= buffer_end) then
       call generate (buffer, state)
       last = 0
    end if
    done = 0
    todo = nu
    chunk = min (todo, buffer_end - last)
    v(1:chunk) = NORM * buffer(last+1:last+chunk)
    do
       last = last + chunk
       done = done + chunk
       todo = todo - chunk
       chunk = min (todo, buffer_end)
       if (chunk <= 0) then
          exit
       end if
       call generate (buffer, state)
       last = 0
       v(done+1:done+chunk) = NORM * buffer(1:chunk)
    end do
  end subroutine integer_array_stateless
  subroutine real_array_stateless &
       (state, buffer, buffer_end, last, v, num)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    real, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    real, parameter :: NORM = 1.0 / M
    integer :: nu, done, todo, chunk
    if (present (num)) then
       nu = num
    else
       nu = size (v)
    end if
    if (last >= buffer_end) then
       call generate (buffer, state)
       last = 0
    end if
    done = 0
    todo = nu
    chunk = min (todo, buffer_end - last)
    v(1:chunk) = NORM * buffer(last+1:last+chunk)
    do
       last = last + chunk
       done = done + chunk
       todo = todo - chunk
       chunk = min (todo, buffer_end)
       if (chunk <= 0) then
          exit
       end if
       call generate (buffer, state)
       last = 0
       v(done+1:done+chunk) = NORM * buffer(1:chunk)
    end do
  end subroutine real_array_stateless
  subroutine double_array_stateless &
       (state, buffer, buffer_end, last, v, num)
    integer(kind=i32), dimension(:), intent(inout) :: state, buffer
    integer, intent(in) :: buffer_end
    integer, intent(inout) :: last
    real(kind=double), dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    real(kind=double), parameter :: NORM = 1.0_double / M
    integer :: nu, done, todo, chunk
    if (present (num)) then
       nu = num
    else
       nu = size (v)
    end if
    if (last >= buffer_end) then
       call generate (buffer, state)
       last = 0
    end if
    done = 0
    todo = nu
    chunk = min (todo, buffer_end - last)
    v(1:chunk) = NORM * buffer(last+1:last+chunk)
    do
       last = last + chunk
       done = done + chunk
       todo = todo - chunk
       chunk = min (todo, buffer_end)
       if (chunk <= 0) then
          exit
       end if
       call generate (buffer, state)
       last = 0
       v(done+1:done+chunk) = NORM * buffer(1:chunk)
    end do
  end subroutine double_array_stateless
  subroutine integer_state (s, r)
    type(tao_random_state), intent(inout) :: s
    integer, intent(out) :: r
    call integer_stateless (s%state%x, s%buffer, s%buffer_end, s%last, r)
  end subroutine integer_state
  subroutine real_state (s, r)
    type(tao_random_state), intent(inout) :: s
    real, intent(out) :: r
    call real_stateless (s%state%x, s%buffer, s%buffer_end, s%last, r)
  end subroutine real_state
  subroutine integer_array_state (s, v, num)
    type(tao_random_state), intent(inout) :: s
    integer, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    call integer_array_stateless &
         (s%state%x, s%buffer, s%buffer_end, s%last, v, num)
  end subroutine integer_array_state
  subroutine real_array_state (s, v, num)
    type(tao_random_state), intent(inout) :: s
    real, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    call real_array_stateless &
         (s%state%x, s%buffer, s%buffer_end, s%last, v, num)
  end subroutine real_array_state
  subroutine integer_static (r)
    integer, intent(out) :: r
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call integer_stateless (s_state, s_buffer, s_buffer_end, s_last, r)
  end subroutine integer_static
  subroutine real_static (r)
    real, intent(out) :: r
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call real_stateless (s_state, s_buffer, s_buffer_end, s_last, r)
  end subroutine real_static
  subroutine integer_array_static (v, num)
    integer, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call integer_array_stateless &
         (s_state, s_buffer, s_buffer_end, s_last, v, num)
  end subroutine integer_array_static
  subroutine real_array_static (v, num)
    real, dimension(:), intent(out) :: v
    integer, optional, intent(in) :: num
    if (s_virginal) then
       call tao_random_seed ()
    end if
    call real_array_stateless &
         (s_state, s_buffer, s_buffer_end, s_last, v, num)
  end subroutine real_array_static
  subroutine tao_random_test (name)
    character(len=*), optional, intent(in) :: name
    character (len = *), parameter :: &
         OK = "(1x,i10,' is ok.')", &
         NOT_OK = "(1x,i10,' is not ok, (expected ',i10,')!')"
    integer, parameter :: &
         SEED = 310952, &
         N = 2009, M = 1009, &
         N_SHORT = 1984
    integer, parameter :: &
         A_2027082 = 461390032
    integer, dimension(N) :: a
    type(tao_random_state) :: s, t
    integer, dimension(:), allocatable :: ibuf
    real(kind=double), dimension(:), allocatable :: dbuf
    integer :: i, ibuf_size, dbuf_size
    print *, TAO_RANDOM_NUMBERS_RCS_ID
    print *, "testing the 30-bit tao_random_numbers ..."
!    call tao_random_luxury ()
    call tao_random_seed (SEED)
    do i = 1, N+1
       call tao_random_number (a, M)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
    call tao_random_seed (SEED)
    do i = 1, M+1
       call tao_random_number (a)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
    print *, "testing the stateless stuff ..."
    call tao_random_create (s, SEED)
    do i = 1, N_SHORT
       call tao_random_number (s, a, M)
    end do
    call tao_random_create (t, s)
    do i = 1, N+1 - N_SHORT
       call tao_random_number (s, a, M)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
    do i = 1, N+1 - N_SHORT
       call tao_random_number (t, a, M)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
    if (present (name)) then
       print *, "testing I/O ..."
       call tao_random_seed (s, SEED)
       do i = 1, N_SHORT
          call tao_random_number (s, a, M)
       end do
       call tao_random_write (s, name)
       do i = 1, N+1 - N_SHORT
          call tao_random_number (s, a, M)
       end do
       if (a(1) == A_2027082) then
          print OK, a(1)
       else
          print NOT_OK, a(1), A_2027082
       end if
       call tao_random_read (s, name)
       do i = 1, N+1 - N_SHORT
          call tao_random_number (s, a, M)
       end do
       if (a(1) == A_2027082) then
          print OK, a(1)
       else
          print NOT_OK, a(1), A_2027082
       end if
    end if
    print *, "testing marshaling/unmarshaling ..."
    call tao_random_seed (s, SEED)
    do i = 1, N_SHORT
       call tao_random_number (s, a, M)
    end do
    call tao_random_marshal_size (s, ibuf_size, dbuf_size)
    allocate (ibuf(ibuf_size), dbuf(dbuf_size))
    call tao_random_marshal (s, ibuf, dbuf)
    do i = 1, N+1 - N_SHORT
       call tao_random_number (s, a, M)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
    call tao_random_unmarshal (s, ibuf, dbuf)
    do i = 1, N+1 - N_SHORT
       call tao_random_number (s, a, M)
    end do
    if (a(1) == A_2027082) then
       print OK, a(1)
    else
       print NOT_OK, a(1), A_2027082
    end if
  end subroutine tao_random_test
end module tao_random_numbers
! linalg.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module linalg
  use kinds
  use utils
  implicit none

  public :: lu_decompose
  public :: determinant
  public :: diagonalize_real_symmetric
  private :: jacobi_rotation
  public :: unit, diag
  character(len=*), public, parameter :: LINALG_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine lu_decompose (a, pivots, eps, l, u)
    real(kind=default), dimension(:,:), intent(inout) :: a
    integer, dimension(:), intent(out), optional :: pivots
    real(kind=default), intent(out), optional :: eps
    real(kind=default), dimension(:,:), intent(out), optional :: l, u
    real(kind=default), dimension(size(a,dim=1)) :: vv
    integer, dimension(size(a,dim=1)) :: p
    integer :: j, pivot
    if (present (eps)) then
       eps = 1.0
    end if
    vv = maxval (abs (a), dim=2)
    if (any (vv == 0.0)) then
       a = 0.0
       if (present (pivots)) then
          pivots = 0
       end if
       if (present (eps)) then
          eps = 0
       end if
       return
    end if
    vv = 1.0 / vv
    do j = 1, size (a, dim=1)
       pivot = j - 1 + sum (maxloc (vv(j:) * abs (a(j:,j))))
       if (j /= pivot) then
          call swap (a(pivot,:), a(j,:))
          if (present (eps)) then
             eps = - eps
          end if
          vv(pivot) = vv(j)
       end if
       p(j) = pivot
       if (a(j,j) == 0.0) then
          a(j,j) = tiny (a(j,j))
       end if
       a(j+1:,j) = a(j+1:,j) / a(j,j)
       a(j+1:,j+1:) &
            = a(j+1:,j+1:) - outer_product (a(j+1:,j), a(j,j+1:))
    end do
    if (present (pivots)) then
       pivots = p
    end if
    if (present (l)) then
       do j = 1, size (a, dim=1)
          l(1:j-1,j) = 0.0
          l(j,j) = 1.0
          l(j+1:,j) = a(j+1:,j)
       end do
       do j = size (a, dim=1), 1, -1
          call swap (l(j,:), l(p(j),:))
       end do
    end if
    if (present (u)) then
       do j = 1, size (a, dim=1)
          u(1:j,j) = a(1:j,j)
          u(j+1:,j) = 0.0
       end do
    end if
  end subroutine lu_decompose
  subroutine determinant (a, det)
    real(kind=default), dimension(:,:), intent(in) :: a
    real(kind=default), intent(out) :: det
    real(kind=default), dimension(size(a,dim=1),size(a,dim=2)) :: lu
    integer :: i
    lu = a
    call lu_decompose (lu, eps = det)
    do i = 1, size (a, dim = 1)
       det = det * lu(i,i)
    end do
  end subroutine determinant
  subroutine diagonalize_real_symmetric (a, eval, evec, num_rot)
    real(kind=default), dimension(:,:), intent(in) :: a
    real(kind=default), dimension(:), intent(out) :: eval
    real(kind=default), dimension(:,:), intent(out) :: evec
    integer, intent(out), optional :: num_rot
    real(kind=default), dimension(size(a,dim=1),size(a,dim=2)) :: aa
    real(kind=default) :: off_diagonal_norm, threshold, &
         c, g, h, s, t, tau, cot_2phi
    logical, dimension(size(eval),size(eval)) :: upper_triangle
    integer, dimension(size(eval)) :: one_to_ndim
    integer :: p, q, ndim, j, sweep
    integer, parameter :: MAX_SWEEPS = 50
    ndim = size (eval)
    one_to_ndim = (/ (j, j=1,ndim) /)
    upper_triangle = &
         spread (one_to_ndim, dim=1, ncopies=ndim) &
         > spread (one_to_ndim, dim=2, ncopies=ndim)
    aa = a
    call unit (evec)
    if (present (num_rot)) then
       num_rot = 0
    end if
    sweeps: do sweep = 1, MAX_SWEEPS
       off_diagonal_norm = sum (abs (aa), mask=upper_triangle)
       if (off_diagonal_norm == 0.0) then
          eval = diag (aa)
          return
       end if
       if (sweep < 4) then
          threshold = 0.2 * off_diagonal_norm / ndim**2
       else
          threshold = 0.0
       end if
       do p = 1, ndim - 1
          do q = p + 1, ndim
             g = 100 * abs (aa (p,q))
             if ((sweep > 4) &
                  .and. (g <= min (spacing (aa(p,p)), spacing (aa(q,q))))) then
                aa(p,q) = 0.0
             else if (abs (aa(p,q)) > threshold) then
                h = aa(q,q) - aa(p,p)
                if (g <= spacing (h)) then
                   t = aa(p,q) / h
                else
                   cot_2phi = 0.5 * h / aa(p,q)
                   t = sign (1.0_default, cot_2phi) &
                        / (abs (cot_2phi) + sqrt (1.0 + cot_2phi**2))
                end if
                c = 1.0 / sqrt (1.0 + t**2)
                s = t * c
                tau = s / (1.0 + c)
                aa(p,p) = aa(p,p) - t * aa(p,q)
                aa(q,q) = aa(q,q) + t * aa(p,q)
                aa(p,q) = 0.0
                call jacobi_rotation (s, tau, aa(1:p-1,p), aa(1:p-1,q))
                call jacobi_rotation (s, tau, aa(p,p+1:q-1), aa(p+1:q-1,q))
                call jacobi_rotation (s, tau, aa(p,q+1:ndim), aa(q,q+1:ndim))
                call jacobi_rotation (s, tau, evec(:,p), evec(:,q))
                if (present (num_rot)) then
                   num_rot = num_rot + 1
                end if
             end if
          end do
       end do
    end do sweeps
    if (present (num_rot)) then
       num_rot = -1
    end if
!!! print *, "linalg::diagonalize_real_symmetric: exceeded sweep count"
  end subroutine diagonalize_real_symmetric
  subroutine jacobi_rotation (s, tau, vp, vq)
    real(kind=default), intent(in) :: s, tau
    real(kind=default), dimension(:), intent(inout) :: vp, vq
    real(kind=default), dimension(size(vp)) :: vp_tmp
    vp_tmp = vp
    vp = vp - s * (vq     + tau * vp)
    vq = vq + s * (vp_tmp - tau * vq)
  end subroutine jacobi_rotation
  subroutine unit (u)
    real(kind=default), dimension(:,:), intent(out) :: u
    integer :: i
    u = 0.0
    do i = 1, min (size (u, dim = 1), size (u, dim = 2))
       u(i,i) = 1.0
    end do
  end subroutine unit
  function diag (a) result (d)
    real(kind=default), dimension(:,:), intent(in) :: a
    real(kind=default), dimension(min(size(a,dim=1),size(a,dim=2))) :: d
    integer :: i
    do i = 1, min (size (a, dim = 1), size (a, dim = 2))
       d(i) = a(i,i)
    end do
  end function diag
end module linalg
! products.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module products
  use kinds
  implicit none

  public :: dot !, sp, spc
  character(len=*), public, parameter :: PRODUCTS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  function dot (p, q) result (pq)
    real(kind=default), dimension(0:), intent(in) :: p, q
    real(kind=default) :: pq
    pq = p(0)*q(0) - dot_product (p(1:), q(1:))
  end function dot
!  function sp (p, q) result (sppq)
!    real(kind=default), dimension(0:), intent(in) :: p, q
!    complex(kind=default) :: sppq
!    sppq = cmplx (p(2), p(3)) * sqrt ((q(0)-q(1))/(p(0)-p(1))) &
!         - cmplx (q(2), q(3)) * sqrt ((p(0)-p(1))/(q(0)-q(1)))
!  end function sp
!  function spc (p, q) result (spcpq)
!    real(kind=default), dimension(0:), intent(in) :: p, q
!    complex(kind=default) :: spcpq
!    spcpq = conjg (sp (p, q))
!  end function spc
end module products

! divisions.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module divisions
  use kinds
  use exceptions
  use vamp_stat
  use utils
  implicit none

  private :: create_division_s, create_division_v
  private :: create_empty_division_s, create_empty_division_v
  private :: copy_division_s, copy_division_v, copy_division_a
  private :: set_rigid_division_s, set_rigid_division_v
  private :: reshape_division_s, reshape_division_v
  private :: delete_division_s, delete_division_v
  private :: inject_division_s, inject_division_v
  private :: inject_division_short_s, inject_division_short_v
  private :: record_integral_s, record_integral_v
  private :: record_variance_s, record_variance_v
  private :: clear_integral_and_variance_s, clear_integral_and_variance_v
  private :: refine_division_s, refine_division_v
  private :: probability_s, probability_v
  private :: inside_division_s, inside_division_v
  private :: stratified_division_s, stratified_division_v
  private :: volume_division_s, volume_division_v
  private :: rigid_division_s, rigid_division_v
  private :: adaptive_division_s, adaptive_division_v
  private :: quadrupole_division_s, quadrupole_division_v
  private :: copy_history_s, copy_history_v
  private :: summarize_division_s, summarize_division_v
  public :: create_division, create_empty_division
  public :: copy_division, delete_division
  public :: set_rigid_division, reshape_division
  public :: inject_division, inject_division_short
  public :: record_integral, record_variance, clear_integral_and_variance
  public :: refine_division
  private :: rebinning_weights
  private :: rebin
  public :: probability
  public :: quadrupole_division
  public :: fork_division, join_division, sum_division
  private :: subdivide
  private :: distribute
  private :: collect
  public :: debug_division
  public :: dump_division
  public :: inside_division, stratified_division
  public :: volume_division, rigid_division, adaptive_division
  public :: copy_history, summarize_division
  private :: probabilities
  public :: print_history, write_history
  public :: write_division
  private :: write_division_unit, write_division_name
  public :: read_division
  private :: read_division_unit, read_division_name
  public :: marshal_division_size, marshal_division, unmarshal_division
  public :: marshal_div_history_size, marshal_div_history, unmarshal_div_history
! *THO
  public :: write_division_raw
  private :: write_division_raw_unit, write_division_raw_name
  public :: read_division_raw
  private :: read_division_raw_unit, read_division_raw_name
!
  interface create_division
     module procedure create_division_s, create_division_v
  end interface
  interface create_empty_division
     module procedure create_empty_division_s, create_empty_division_v
  end interface
  interface copy_division
     module procedure copy_division_s, copy_division_v, copy_division_a
  end interface
  interface set_rigid_division
     module procedure set_rigid_division_s, set_rigid_division_v
  end interface
  interface reshape_division
     module procedure reshape_division_s, reshape_division_v
  end interface
  interface delete_division
     module procedure delete_division_s, delete_division_v
  end interface
  interface inject_division
     module procedure inject_division_s, inject_division_v
  end interface
  interface inject_division_short
     module procedure inject_division_short_s, inject_division_short_v
  end interface
  interface record_integral
     module procedure record_integral_s, record_integral_v
  end interface
  interface record_variance
     module procedure record_variance_s, record_variance_v
  end interface
  interface clear_integral_and_variance
     module procedure clear_integral_and_variance_s, clear_integral_and_variance_v
  end interface
  interface refine_division
     module procedure refine_division_s, refine_division_v
  end interface
  interface probability
     module procedure probability_s, probability_v
  end interface
  interface inside_division
     module procedure inside_division_s, inside_division_v
  end interface
  interface stratified_division
     module procedure stratified_division_s, stratified_division_v
  end interface
  interface volume_division
     module procedure volume_division_s, volume_division_v
  end interface
  interface rigid_division
     module procedure rigid_division_s, rigid_division_v
  end interface
  interface adaptive_division
     module procedure adaptive_division_s, adaptive_division_v
  end interface
  interface quadrupole_division
     module procedure quadrupole_division_s, quadrupole_division_v
  end interface
  interface copy_history
     module procedure copy_history_s, copy_history_v
  end interface
  interface summarize_division
     module procedure summarize_division_s, summarize_division_v
  end interface
  interface write_division
     module procedure write_division_unit, write_division_name
  end interface
  interface read_division
     module procedure read_division_unit, read_division_name
  end interface
! *THO
  interface write_division_raw
     module procedure write_division_raw_unit, write_division_raw_name
  end interface
  interface read_division_raw
     module procedure read_division_raw_unit, read_division_raw_name
  end interface
  integer, parameter, private :: MAGIC_DIVISION = 11111111
  integer, parameter, private :: MAGIC_DIVISION_BEGIN = MAGIC_DIVISION + 1
  integer, parameter, private :: MAGIC_DIVISION_END = MAGIC_DIVISION + 2
!
  integer, private, parameter :: MIN_NUM_DIV = 3
  integer, private, parameter :: BUFFER_SIZE = 50
  character(len=*), parameter, private :: &
       descr_fmt =        "(1x,a)", &
       integer_fmt =      "(1x,a15,1x,i15)", &
       logical_fmt =      "(1x,a15,1x,l1)", &
       double_fmt =       "(1x,a15,1x,e30.22)", &
! *WK: extended this format
       double_array_fmt = "(1x,i15,1x,3(e30.22))"
  type, public :: division

     real(kind=default), dimension(:), pointer :: x              => null ()
     real(kind=default), dimension(:), pointer :: integral       => null ()
     real(kind=default), dimension(:), pointer :: variance       => null ()
     real(kind=default) :: x_min, x_max
     real(kind=default) :: x_min_true, x_max_true
     real(kind=default) :: dx, dxg
     integer :: ng                                              
     logical :: stratified                                      
  end type division
  type, public :: div_history

     logical :: stratified
     integer :: ng, num_div
     real(kind=default) :: x_min, x_max, x_min_true, x_max_true
     real(kind=default) :: &
          spread_f_p, stddev_f_p, spread_p, stddev_p, spread_m, stddev_m
  end type div_history
  character(len=*), public, parameter :: DIVISIONS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine create_division_v (d, x_min, x_max, x_min_true, x_max_true)
    type(division), dimension(:), intent(out) :: d
    real(kind=default), dimension(:), intent(in) :: x_min, x_max
    real(kind=default), dimension(:), intent(in), optional :: &
         x_min_true, x_max_true
    integer :: j
    do j = 1, size (d)
       if (present (x_min_true) .and. present (x_max_true)) then
          call create_division_s &
               (d(j), x_min(j), x_max(j), x_min_true(j), x_max_true(j))
       else
          call create_division_s (d(j), x_min(j), x_max(j))
       end if
    end do
  end subroutine create_division_v
  subroutine create_empty_division_v (d)
    type(division), dimension(:), intent(out) :: d
    integer :: j
    do j = 1, size (d)
       call create_empty_division_s (d(j))
    end do
  end subroutine create_empty_division_v
  subroutine copy_division_v (lhs, rhs)
    type(division), dimension(:), intent(inout) :: lhs
    type(division), dimension(:), intent(in) :: rhs
    integer :: j
    do j = 1, size(lhs)
       call copy_division_s (lhs(j), rhs(j))
    end do
  end subroutine copy_division_v
  subroutine copy_division_a (lhs, rhs)
    type(division), dimension(:), intent(inout) :: lhs
    type(division), intent(in) :: rhs
    integer :: j
    do j = 1, size(lhs)
       call copy_division_s (lhs(j), rhs)
    end do
  end subroutine copy_division_a
  subroutine set_rigid_division_v (d, ng)
    type(division), dimension(:), intent(inout) :: d
    integer, dimension(:), intent(in) :: ng
    integer :: j
    do j = 1, size(d)
       call set_rigid_division_s (d(j), ng(j))
    end do
  end subroutine set_rigid_division_v
  subroutine reshape_division_v (d, max_num_div, ng, use_variance)
    type(division), dimension(:), intent(inout) :: d
    integer, dimension(:), intent(in) :: max_num_div
    integer, dimension(:), intent(in), optional :: ng
    logical, intent(in), optional :: use_variance
    integer :: j
    do j = 1, size(d)
       call reshape_division_s (d(j), max_num_div(j), ng(j), use_variance)
    end do
  end subroutine reshape_division_v
  subroutine delete_division_v (d)
    type(division), dimension(:), intent(inout) :: d
    integer :: j
    do j = 1, size(d)
       call delete_division_s (d(j))
    end do
  end subroutine delete_division_v
  subroutine inject_division_v (d, r, cell, x, x_mid, idx, wgt)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(:), intent(in) :: r
    integer, dimension(:), intent(in) :: cell
    real(kind=default), dimension(:), intent(out) :: x, x_mid
    integer, dimension(:), intent(out) :: idx
    real(kind=default), dimension(:), intent(out) :: wgt
    integer :: j
    do j = 1, size (d)
       call inject_division_s (d(j), r(j), cell(j), x(j), &
            x_mid(j), idx(j), wgt(j))
    end do
  end subroutine inject_division_v
  subroutine inject_division_short_v (d, r, x, wgt)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(:), intent(in) :: r
    real(kind=default), dimension(:), intent(out) :: x, wgt
    integer :: j
    do j = 1, size (d)
       call inject_division_short_s (d(j), r(j), x(j), wgt(j))
    end do
  end subroutine inject_division_short_v
  subroutine record_integral_v (d, i, f)
    type(division), dimension(:), intent(inout) :: d
    integer, dimension(:), intent(in) :: i
    real(kind=default), intent(in) :: f
    integer :: j
    do j = 1, size (d)
       call record_integral_s (d(j), i(j), f)
    end do
  end subroutine record_integral_v
  subroutine record_variance_v (d, i, var_f)
    type(division), dimension(:), intent(inout) :: d
    integer, dimension(:), intent(in) :: i
    real(kind=default), intent(in) :: var_f
    integer :: j
    do j = 1, size (d)
       call record_variance_s (d(j), i(j), var_f)
    end do
  end subroutine record_variance_v
  subroutine clear_integral_and_variance_v (d)
    type(division), dimension(:), intent(inout) :: d
    integer :: j
    do j = 1, size (d)
       call clear_integral_and_variance_s (d(j))
    end do
  end subroutine clear_integral_and_variance_v
  subroutine refine_division_v (d)
    type(division), dimension(:), intent(inout) :: d
    integer :: j
    do j = 1, size (d)
       call refine_division_s (d(j))
    end do
  end subroutine refine_division_v
  function probability_v (d, xi) result (p)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(:), intent(in) :: xi
    real(kind=default), dimension(size(d)) :: p
    integer :: j
    do j = 1, size (d)
       p(j) = probability_s (d(j), xi(j))
    end do
  end function probability_v
  function inside_division_v (d, x) result (theta)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(:), intent(in) :: x
    logical, dimension(size(d)) :: theta
    integer :: j
    do j = 1, size (d)
       theta(j) = inside_division_s (d(j), x(j))
    end do
  end function inside_division_v
  function stratified_division_v (d) result (yorn)
    type(division), dimension(:), intent(in) :: d
    logical, dimension(size(d)) :: yorn
    integer :: j
    do j = 1, size (d)
       yorn(j) = stratified_division_s (d(j))
    end do
  end function stratified_division_v
  function volume_division_v (d) result (vol)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(size(d)) :: vol
    integer :: j
    do j = 1, size(d)
       vol(j) = volume_division_s (d(j))
    end do
  end function volume_division_v
  function rigid_division_v (d) result (n)
    type(division), dimension(:), intent(in) :: d
    integer, dimension(size(d)) :: n
    integer :: j
    do j = 1, size(d)
       n(j) = rigid_division_s (d(j))
    end do
  end function rigid_division_v
  function adaptive_division_v (d) result (n)
    type(division), dimension(:), intent(in) :: d
    integer, dimension(size(d)) :: n
    integer :: j
    do j = 1, size(d)
       n(j) = adaptive_division_s (d(j))
    end do
  end function adaptive_division_v
  function quadrupole_division_v (d) result (q)
    type(division), dimension(:), intent(in) :: d
    real(kind=default), dimension(size(d)) :: q
    integer :: j
    do j = 1, size (d)
       q(j) = quadrupole_division_s (d(j))
    end do
  end function quadrupole_division_v
  subroutine copy_history_v (lhs, rhs)
    type(div_history), dimension(:), intent(inout) :: lhs
    type(div_history), dimension(:), intent(in) :: rhs
    integer :: j
    do j = 1, size (rhs)
       call copy_history_s (lhs(j), rhs(j))
    end do
  end subroutine copy_history_v
  function summarize_division_v (d) result (s)
    type(division), dimension(:), intent(in) :: d
    type(div_history), dimension(size(d))  :: s
    integer :: j
    do j = 1, size (d)
       s(j) = summarize_division_s (d(j))
    end do
  end function summarize_division_v
  subroutine create_division_s &
       (d, x_min, x_max, x_min_true, x_max_true)
    type(division), intent(out) :: d
    real(kind=default), intent(in) :: x_min, x_max
    real(kind=default), intent(in), optional :: x_min_true, x_max_true
    allocate (d%x(0:1), d%integral(1), d%variance(1))
    d%x(0) = 0.0
    d%x(1) = 1.0
    d%x_min = x_min
    d%x_max = x_max
    d%dx = d%x_max - d%x_min
    d%stratified = .false.
    d%ng = 1
    d%dxg = 1.0 / d%ng
    if (present (x_min_true)) then
       d%x_min_true = x_min_true
    else
       d%x_min_true = x_min
    end if
    if (present (x_max_true)) then
       d%x_max_true = x_max_true
    else
       d%x_max_true = x_max
    end if
  end subroutine create_division_s
  subroutine create_empty_division_s (d)
    type(division), intent(out) :: d
    nullify (d%x, d%integral, d%variance)
  end subroutine create_empty_division_s
  subroutine set_rigid_division_s (d, ng)
    type(division), intent(inout) :: d
    integer, intent(in) :: ng
    d%stratified = ng > 1
    d%ng = ng
    d%dxg = real (ubound (d%x, dim=1), kind=default) / d%ng
  end subroutine set_rigid_division_s
  subroutine reshape_division_s (d, max_num_div, ng, use_variance)
    type(division), intent(inout) :: d
    integer, intent(in) :: max_num_div
    integer, intent(in), optional :: ng
    logical, intent(in), optional :: use_variance
    real(kind=default), dimension(:), allocatable :: old_x, m
    integer :: num_div, equ_per_adap
    if (present (ng)) then
! *WK: allow for max_num_div = 1
       if (max_num_div > 1) then
          d%stratified = ng > 1
       else
          d%stratified = .false.
       end if
! *WK
    else
       d%stratified = .false.
    end if
    if (d%stratified) then
       d%ng = ng
       if (d%ng >= max_num_div / 2) then 
          d%stratified = .true.
          equ_per_adap = d%ng / max_num_div + 1
          num_div = d%ng / equ_per_adap
! *WK num_div must be even (allow for adding arrays with offset 1/2)
          if (num_div < 2) then
             d%stratified = .false.
             num_div = 2
             d%ng = 1
          else if (mod (num_div,2) == 1) then
             num_div = num_div - 1
             d%ng = equ_per_adap * num_div
          else
             d%ng = equ_per_adap * num_div
          end if
       else
          d%stratified = .false.
          num_div = max_num_div
          d%ng = 1
! *WK
       end if
    else
       num_div = max_num_div
       d%ng = 1
    end if
    d%dxg = real (num_div, kind=default) / d%ng
    allocate (old_x(0:ubound(d%x,dim=1)), m(ubound(d%x,dim=1)))
    old_x = d%x
    if (present (use_variance)) then
       if (use_variance) then
          m = rebinning_weights (d%variance)
       else
          m = 1.0
       end if
    else
       m = 1.0
    end if
    if (ubound (d%x, dim=1) /= num_div) then
       deallocate (d%x, d%integral, d%variance)
       allocate (d%x(0:num_div), d%integral(num_div), d%variance(num_div))
    end if
    d%x = rebin (m, old_x, num_div)
    deallocate (old_x, m)
  end subroutine reshape_division_s
  subroutine inject_division_s (d, r, cell, x, x_mid, idx, wgt)
    type(division), intent(in) :: d
    real(kind=default), intent(in) :: r
    integer, intent(in) :: cell
    real(kind=default), intent(out) :: x, x_mid
    integer, intent(out) :: idx
    real(kind=default), intent(out) :: wgt
    real(kind=default) :: delta_x, xi
    integer :: i
    xi = (cell - r) * d%dxg + 1.0
    i = max (min (int (xi), ubound (d%x, dim=1)), 1)
    delta_x = d%x(i) - d%x(i-1)
    x = d%x_min + (d%x(i-1) + (xi - i) * delta_x) * d%dx
    wgt = delta_x * ubound (d%x, dim=1)
    idx = i
    x_mid = d%x_min + 0.5 * (d%x(i-1) + d%x(i)) * d%dx
  end subroutine inject_division_s
  subroutine inject_division_short_s (d, r, x, wgt)
    type(division), intent(in) :: d
    real(kind=default), intent(in) :: r
    real(kind=default), intent(out) :: x, wgt
    real(kind=default) :: delta_x, xi
    integer :: i
    xi = r * ubound (d%x, dim=1) + 1.0
    i = max (min (int (xi), ubound (d%x, dim=1)), 1)
    delta_x = d%x(i) - d%x(i-1)
    x = d%x_min + (d%x(i-1) + (xi - i) * delta_x) * d%dx
    wgt = delta_x * ubound (d%x, dim=1)
  end subroutine inject_division_short_s
  subroutine record_integral_s (d, i, f)
    type(division), intent(inout) :: d
    integer, intent(in) :: i
    real(kind=default), intent(in) :: f
    d%integral(i) = d%integral(i) + f
    if (.not. d%stratified) then 
       d%variance(i) = d%variance(i) + f*f
    end if
  end subroutine record_integral_s
  subroutine record_variance_s (d, i, var_f)
    type(division), intent(inout) :: d
    integer, intent(in) :: i
    real(kind=default), intent(in) :: var_f
    if (d%stratified) then 
       d%variance(i) = d%variance(i) + var_f
    end if
  end subroutine record_variance_s
  subroutine clear_integral_and_variance_s (d)
    type(division), intent(inout) :: d
    d%integral = 0.0
    d%variance = 0.0
  end subroutine clear_integral_and_variance_s
  subroutine refine_division_s (d)
    type(division), intent(inout) :: d
    character(len=*), parameter :: FN = "refine_division_s"
    d%x = rebin (rebinning_weights (d%variance), d%x, size (d%variance))
  end subroutine refine_division_s
  function rebinning_weights (d) result (m)
    real(kind=default), dimension(:), intent(in) :: d
    real(kind=default), dimension(size(d)) :: m
    real(kind=default), dimension(size(d)) :: smooth_d
    real(kind=default), parameter :: ALPHA = 1.5
    integer :: nd
    if (any (d /= d)) then
       m = 1.0
       return
    end if
    nd = size (d)
! *WK: Allow for nd < 3
    if (nd > 2) then
       smooth_d(1) = (d(1) + d(2)) / 2.0
       smooth_d(2:nd-1) = (d(1:nd-2) + d(2:nd-1) + d(3:nd)) / 3.0
       smooth_d(nd) = (d(nd-1) + d(nd)) / 2.0
    else
       smooth_d = d
    end if
! *WK: Inserted sanity checks
    if (all(smooth_d < tiny (1.0_default))) then
       m = 1.0
    else
       smooth_d = smooth_d / sum (smooth_d)
       where (smooth_d < tiny (1.0_default))
          smooth_d = tiny (1.0_default)
       end where
       where (smooth_d /= 1._default)
          m = ((smooth_d - 1.0) / (log (smooth_d)))**ALPHA
       elsewhere
          m = 1.0
       end where
    end if
  end function rebinning_weights
  function rebin (m, x, num_div) result (x_new)
    real(kind=default), dimension(:), intent(in) :: m
    real(kind=default), dimension(0:), intent(in) :: x
    integer, intent(in) :: num_div
    real(kind=default), dimension(0:num_div) :: x_new
    integer :: i, k
    real(kind=default) :: step, delta
    step = sum (m) / num_div
    k = 0
    delta = 0.0
    x_new(0) = x(0)
    do i = 1, num_div - 1
       do
          if (step <= delta) then 
             exit
          end if
          k = k + 1
          delta = delta + m(k)
       end do
       delta = delta - step
       x_new(i) = x(k) - (x(k) - x(k-1)) * delta / m(k)
    end do
    x_new(num_div) = 1.0
  end function rebin
  function probability_s (d, x) result (p)
    type(division), intent(in) :: d
    real(kind=default), intent(in) :: x
    real(kind=default) :: p
    real(kind=default) :: xi
    integer :: hi, mid, lo
    xi = (x - d%x_min) / d%dx
    if ((xi >= 0) .and. (xi <= 1)) then
       lo = lbound (d%x, dim=1)
       hi = ubound (d%x, dim=1)
       bracket: do
          if (lo >= hi - 1) then
             p = 1.0 / (ubound (d%x, dim=1) * d%dx * (d%x(hi) - d%x(hi-1)))
             return
          end if
          mid = (hi + lo) / 2
          if (xi > d%x(mid)) then
             lo = mid
          else
             hi = mid
          end if
       end do bracket
    else
       p = 0
    end if
  end function probability_s
  function quadrupole_division_s (d) result (q)
    type(division), intent(in) :: d
    real(kind=default) :: q
!!!   q = value_spread_percent (rebinning_weights (d%variance))
    q = standard_deviation_percent (rebinning_weights (d%variance))
  end function quadrupole_division_s
  subroutine fork_division (d, ds, sum_calls, num_calls, exc)
    type(division), intent(in) :: d
    type(division), dimension(:), intent(inout) :: ds
    integer, intent(in) :: sum_calls
    integer, dimension(:), intent(inout) :: num_calls
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "fork_division"
    integer, dimension(size(ds)) :: n0, n1
    integer, dimension(0:size(ds)) :: n, ds_ng
    integer :: i, j, num_div, num_forks, nx
    real(kind=default), dimension(:), allocatable :: d_x, d_integral, d_variance
    num_div = ubound (d%x, dim=1)
    num_forks = size (ds)
    if (d%ng == 1) then
       if (d%stratified) then
          call raise_exception (exc, EXC_FATAL, FN, &
               "ng == 1 incompatiple w/ stratification")
       else
          call copy_division (ds, d)
          num_calls(2:) = ceiling (real (sum_calls) / num_forks)
          num_calls(1) = sum_calls - sum (num_calls(2:))
       end if
    else if (num_div >= num_forks) then
       if (modulo (d%ng, num_div) == 0) then
          n = (num_div * (/ (j, j=0,num_forks) /)) / num_forks
          n0(1:num_forks) = n(0:num_forks-1)
          n1(1:num_forks) = n(1:num_forks)
          do i = 1, num_forks
             call copy_array_pointer (ds(i)%x, d%x(n0(i):n1(i)), lb = 0)
             call copy_array_pointer (ds(i)%integral, d%integral(n0(i)+1:n1(i)))
             call copy_array_pointer (ds(i)%variance, d%variance(n0(i)+1:n1(i)))
             ds(i)%x = (ds(i)%x - ds(i)%x(0)) / (d%x(n1(i)) - d%x(n0(i)))
          end do
          ds%x_min = d%x_min + d%dx * d%x(n0)
          ds%x_max = d%x_min + d%dx * d%x(n1)
          ds%dx = ds%x_max - ds%x_min
          ds%x_min_true = d%x_min_true
          ds%x_max_true = d%x_max_true
          ds%stratified = d%stratified
          ds%ng = (d%ng * (n1 - n0)) / num_div
          num_calls = sum_calls !: this is a misnomer, it remains `calls per cell' here
          ds%dxg = real (n1 - n0, kind=default) / ds%ng
       else
          nx = lcm (d%ng / gcd (num_forks, d%ng), num_div)
          ds_ng = (d%ng * (/ (j, j=0,num_forks) /)) / num_forks
          n = (nx * ds_ng) / d%ng
          n0(1:num_forks) = n(0:num_forks-1)
          n1(1:num_forks) = n(1:num_forks)
          allocate (d_x(0:nx), d_integral(nx), d_variance(nx))
          call subdivide (d_x, d%x)
          call distribute (d_integral, d%integral)
          call distribute (d_variance, d%variance)
          do i = 1, num_forks
             call copy_array_pointer (ds(i)%x, d_x(n0(i):n1(i)), lb = 0)
             call copy_array_pointer (ds(i)%integral, d_integral(n0(i)+1:n1(i)))
             call copy_array_pointer (ds(i)%variance, d_variance(n0(i)+1:n1(i)))
             ds(i)%x = (ds(i)%x - ds(i)%x(0)) / (d_x(n1(i)) - d_x(n0(i)))
          end do
          ds%x_min = d%x_min + d%dx * d_x(n0)
          ds%x_max = d%x_min + d%dx * d_x(n1)
          ds%dx = ds%x_max - ds%x_min
          ds%x_min_true = d%x_min_true
          ds%x_max_true = d%x_max_true
          ds%stratified = d%stratified
          ds%ng = ds_ng(1:num_forks) - ds_ng(0:num_forks-1)
          num_calls = sum_calls !: this is a misnomer, it remains `calls per cell' here
          ds%dxg = real (n1 - n0, kind=default) / ds%ng
          deallocate (d_x, d_integral, d_variance)
       end if
    else
       if (present (exc)) then
          call raise_exception (exc, EXC_FATAL, FN, "internal error")
       end if
       num_calls = 0
    end if
  end subroutine fork_division
  subroutine join_division (d, ds, exc)
    type(division), intent(inout) :: d
    type(division), dimension(:), intent(in) :: ds
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "join_division"
    integer, dimension(size(ds)) :: n0, n1
    integer, dimension(0:size(ds)) :: n, ds_ng
    integer :: i, j, num_div, num_forks, nx
    real(kind=default), dimension(:), allocatable :: d_x, d_integral, d_variance
    num_div = ubound (d%x, dim=1)
    num_forks = size (ds)
    if (d%ng == 1) then
       call sum_division (d, ds)
    else if (num_div >= num_forks) then
       if (modulo (d%ng, num_div) == 0) then
          n = (num_div * (/ (j, j=0,num_forks) /)) / num_forks
          n0(1:num_forks) = n(0:num_forks-1)
          n1(1:num_forks) = n(1:num_forks)
          do i = 1, num_forks
             d%integral(n0(i)+1:n1(i)) = ds(i)%integral
             d%variance(n0(i)+1:n1(i)) = ds(i)%variance
          end do
       else
          nx = lcm (d%ng / gcd (num_forks, d%ng), num_div)
          ds_ng = (d%ng * (/ (j, j=0,num_forks) /)) / num_forks
          n = (nx * ds_ng) / d%ng
          n0(1:num_forks) = n(0:num_forks-1)
          n1(1:num_forks) = n(1:num_forks)
          allocate (d_x(0:nx), d_integral(nx), d_variance(nx))
          do i = 1, num_forks
             d_integral(n0(i)+1:n1(i)) = ds(i)%integral
             d_variance(n0(i)+1:n1(i)) = ds(i)%variance
          end do
          call collect (d%integral, d_integral)
          call collect (d%variance, d_variance)
          deallocate (d_x, d_integral, d_variance)
       end if
    else
       if (present (exc)) then
          call raise_exception (exc, EXC_FATAL, FN, "internal error")
       end if
    end if
  end subroutine join_division
  subroutine subdivide (x, x0)
    real(kind=default), dimension(0:), intent(inout) :: x
    real(kind=default), dimension(0:), intent(in) :: x0
    integer :: i, n, n0
    n0 = ubound (x0, dim=1)
    n = ubound (x, dim=1) / n0
    x(0) = x0(0)
    do i = 1, n
       x(i::n) = x0(0:n0-1) * real (n - i) / n + x0(1:n0) * real (i) / n
    end do
  end subroutine subdivide
  subroutine distribute (x, x0)
    real(kind=default), dimension(:), intent(inout) :: x
    real(kind=default), dimension(:), intent(in) :: x0
    integer :: i, n
    n = ubound (x, dim=1) / ubound (x0, dim=1)
    do i = 1, n
       x(i::n) = x0 / n
    end do
  end subroutine distribute
  subroutine collect (x0, x)
    real(kind=default), dimension(:), intent(inout) :: x0
    real(kind=default), dimension(:), intent(in) :: x
    integer :: i, n, n0
    n0 = ubound (x0, dim=1)
    n = ubound (x, dim=1) / n0
    do i = 1, n0
       x0(i) = sum (x((i-1)*n+1:i*n))
    end do
  end subroutine collect
  subroutine sum_division (d, ds)
    type(division), intent(inout) :: d
    type(division), dimension(:), intent(in) :: ds
    integer :: i
    d%integral = 0.0
    d%variance = 0.0
    do i = 1, size (ds)
       d%integral = d%integral + ds(i)%integral
       d%variance = d%variance + ds(i)%variance
    end do
  end subroutine sum_division
  subroutine debug_division (d, prefix)
    type(division), intent(in) :: d
    character(len=*), intent(in) :: prefix
    print "(1x,a,2(a,1x,i3,1x,f10.7))", prefix, ": d%x: ", &
         lbound(d%x,dim=1), d%x(lbound(d%x,dim=1)), &
         " ... ", &
         ubound(d%x,dim=1), d%x(ubound(d%x,dim=1))
    print "(1x,a,2(a,1x,i3,1x,f10.7))", prefix, ": d%i: ", &
         lbound(d%integral,dim=1), d%integral(lbound(d%integral,dim=1)), &
         " ... ", &
         ubound(d%integral,dim=1), d%integral(ubound(d%integral,dim=1))
    print "(1x,a,2(a,1x,i3,1x,f10.7))", prefix, ": d%v: ", &
         lbound(d%variance,dim=1), d%variance(lbound(d%variance,dim=1)), &
         " ... ", &
         ubound(d%variance,dim=1), d%variance(ubound(d%variance,dim=1))
  end subroutine debug_division
  subroutine dump_division (d, prefix)
    type(division), intent(in) :: d
    character(len=*), intent(in) :: prefix
    ! print "(2(1x,a),100(1x,f10.7))", prefix, ":x: ", d%x
    print "(2(1x,a),100(1x,f10.7))", prefix, ":x: ", d%x(1:)
    print "(2(1x,a),100(1x,e10.3))", prefix, ":i: ", d%integral
    print "(2(1x,a),100(1x,e10.3))", prefix, ":v: ", d%variance
  end subroutine dump_division
  function inside_division_s (d, x) result (theta)
    type(division), intent(in) :: d
    real(kind=default), intent(in) :: x
    logical :: theta
    theta = (x >= d%x_min_true) .and. (x <= d%x_max_true)
  end function inside_division_s
  function stratified_division_s (d) result (yorn)
    type(division), intent(in) :: d
    logical :: yorn
    yorn = d%stratified
  end function stratified_division_s
  function volume_division_s (d) result (vol)
    type(division), intent(in) :: d
    real(kind=default) :: vol
    vol = d%dx
  end function volume_division_s
  function rigid_division_s (d) result (n)
    type(division), intent(in) :: d
    integer :: n
    n = d%ng
  end function rigid_division_s
  function adaptive_division_s (d) result (n)
    type(division), intent(in) :: d
    integer :: n
    n = ubound (d%x, dim=1)
  end function adaptive_division_s
  function summarize_division_s (d) result (s)
    type(division), intent(in) :: d
    type(div_history) :: s
    real(kind=default), dimension(:), allocatable :: p, m
    allocate (p(ubound(d%x,dim=1)), m(ubound(d%x,dim=1)))
    p = probabilities (d%x)
    m = rebinning_weights (d%variance)
    s%ng = d%ng
    s%num_div = ubound (d%x, dim=1)
    s%stratified = d%stratified
    s%x_min = d%x_min
    s%x_max = d%x_max
    s%x_min_true = d%x_min_true
    s%x_max_true = d%x_max_true
    s%spread_f_p = value_spread_percent (d%integral)
    s%stddev_f_p = standard_deviation_percent (d%integral)
    s%spread_p = value_spread_percent (p)
    s%stddev_p = standard_deviation_percent (p)
    s%spread_m = value_spread_percent (m)
    s%stddev_m = standard_deviation_percent (m)
    deallocate (p, m)
  end function summarize_division_s
  function probabilities (x) result (p)
    real(kind=default), dimension(0:), intent(in) :: x
    real(kind=default), dimension(ubound(x,dim=1)) :: p
    integer :: num_div
    num_div = ubound (x, dim=1)
    p = 1.0 / (x(1:num_div) - x(0:num_div-1))
    p = p / sum(p)
  end function probabilities
  subroutine print_history (h, tag)
    type(div_history), dimension(:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    call write_history(6, h, tag)
  end subroutine print_history
  subroutine write_history (u, h, tag)
    integer, intent(in) :: u
    type(div_history), dimension(:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    character(len=BUFFER_SIZE) :: pfx
    character(len=1) :: s
    integer :: i
    if (present (tag)) then
       pfx = tag
    else
       pfx = "[vamp]"
    end if
    if ((minval (h%x_min) == maxval (h%x_min)) &
         .and. (minval (h%x_max) == maxval (h%x_max))) then
       write(u, "(1X,A11,1X,2X,1X,2(E10.3,A4,E10.3,A7))") pfx, &
            h(1)%x_min, " <= ", h(1)%x_min_true, &
            " < x < ", h(1)%x_max_true, " <= ", h(1)%x_max
    else
       do i = 1, size (h)
          write(u, "(1X,A11,1X,I2,1X,2(E10.3,A4,E10.3,A7))") pfx, &
               i, h(i)%x_min, " <= ", h(i)%x_min_true, &
               " < x < ", h(i)%x_max_true, " <= ", h(i)%x_max
       end do
    end if
    write(u, "(1X,A11,1X,A2,2(1X,A3),A1,6(1X,A8))") pfx, &
         "it", "nd", "ng", "", &
         "spr(f/p)", "dev(f/p)", "spr(m)", "dev(m)", "spr(p)", "dev(p)"
    iterations: do i = 1, size (h)
       if (h(i)%stratified) then
          s = "*"
       else
          s = ""
       end if
       write(u, "(1X,A11,1X,I2,2(1X,I3),A1,6(1X,F7.2,A1))") pfx, &
            i, h(i)%num_div, h(i)%ng, s, &
            h(i)%spread_f_p, "%", h(i)%stddev_f_p, "%", &
            h(i)%spread_m, "%", h(i)%stddev_m, "%", &
            h(i)%spread_p, "%", h(i)%stddev_p, "%"
    end do iterations
  end subroutine write_history
! *WK: Optional argument write_integrals
  subroutine write_division_unit (d, unit, write_integrals)
    type(division), intent(in) :: d
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    logical :: write_integrals0
    integer :: i
    write_integrals0 = .false.
    if (present(write_integrals))  write_integrals0 = write_integrals
    write (unit = unit, fmt = descr_fmt) "begin type(division) :: d"
    write (unit = unit, fmt = integer_fmt) "ubound(d%x,1) = ", ubound (d%x, dim=1)
    write (unit = unit, fmt = integer_fmt) "d%ng = ", d%ng
    write (unit = unit, fmt = logical_fmt) "d%stratified = ", d%stratified
    write (unit = unit, fmt = double_fmt) "d%dx = ", d%dx
    write (unit = unit, fmt = double_fmt) "d%dxg = ", d%dxg
    write (unit = unit, fmt = double_fmt) "d%x_min = ", d%x_min
    write (unit = unit, fmt = double_fmt) "d%x_max = ", d%x_max
    write (unit = unit, fmt = double_fmt) "d%x_min_true = ", d%x_min_true
    write (unit = unit, fmt = double_fmt) "d%x_max_true = ", d%x_max_true
    write (unit = unit, fmt = descr_fmt) "begin d%x" 
    do i = 0, ubound (d%x, dim=1)
! *WK: Write also integral and variance
       if (write_integrals0 .and. i/=0) then
          write (unit = unit, fmt = double_array_fmt) &
               i, d%x(i), d%integral(i), d%variance(i)
       else
          write (unit = unit, fmt = double_array_fmt) i, d%x(i)
       end if
    end do
    write (unit = unit, fmt = descr_fmt) "end d%x"
    write (unit = unit, fmt = descr_fmt) "end type(division)"
  end subroutine write_division_unit
! *WK: Optional argument read_integrals
  subroutine read_division_unit (d, unit, read_integrals)
    type(division), intent(inout) :: d
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    logical :: read_integrals0
    integer :: i, idum, num_div
    character(len=80) :: chdum
    read_integrals0 = .false.
    if (present(read_integrals))  read_integrals0 = read_integrals
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = integer_fmt) chdum, num_div
    if (associated (d%x)) then
       if (ubound (d%x, dim=1) /= num_div) then
          deallocate (d%x, d%integral, d%variance)
          allocate (d%x(0:num_div), d%integral(num_div), d%variance(num_div))
       end if
    else
       allocate (d%x(0:num_div), d%integral(num_div), d%variance(num_div))
    end if
    read (unit = unit, fmt = integer_fmt) chdum, d%ng
    read (unit = unit, fmt = logical_fmt) chdum, d%stratified
    read (unit = unit, fmt = double_fmt) chdum, d%dx
    read (unit = unit, fmt = double_fmt) chdum, d%dxg
    read (unit = unit, fmt = double_fmt) chdum, d%x_min
    read (unit = unit, fmt = double_fmt) chdum, d%x_max
    read (unit = unit, fmt = double_fmt) chdum, d%x_min_true
    read (unit = unit, fmt = double_fmt) chdum, d%x_max_true
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 0, ubound (d%x, dim=1)
! *WK: Read also integral and variance
       if (read_integrals0 .and. i/=0) then
          read (unit = unit, fmt = double_array_fmt) &
            & idum, d%x(i), d%integral(i), d%variance(i)
       else
          read (unit = unit, fmt = double_array_fmt) idum, d%x(i)
       end if
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
! *WK: These are now read from file as well
    if (.not.read_integrals0) then
       d%integral = 0.0
       d%variance = 0.0
    end if
  end subroutine read_division_unit
! *WK: Optional argument write_integrals
  subroutine write_division_name (d, name, write_integrals)
    type(division), intent(in) :: d
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", file = name)
    call write_division_unit (d, unit, write_integrals)
    close (unit = unit)
  end subroutine write_division_name
! *WK: Optional argument read_integrals
  subroutine read_division_name (d, name, read_integrals)
    type(division), intent(inout) :: d
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", file = name)
    call read_division_unit (d, unit, read_integrals)
    close (unit = unit)
  end subroutine read_division_name
! *THO
  subroutine write_division_raw_unit (d, unit, write_integrals)
    type(division), intent(in) :: d
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    logical :: write_integrals0
    integer :: i
    write_integrals0 = .false.
    if (present(write_integrals))  write_integrals0 = write_integrals
    write (unit = unit) MAGIC_DIVISION_BEGIN
    write (unit = unit) ubound (d%x, dim=1)
    write (unit = unit) d%ng
    write (unit = unit) d%stratified
    write (unit = unit) d%dx
    write (unit = unit) d%dxg
    write (unit = unit) d%x_min
    write (unit = unit) d%x_max
    write (unit = unit) d%x_min_true
    write (unit = unit) d%x_max_true
    do i = 0, ubound (d%x, dim=1)
       if (write_integrals0 .and. i/=0) then
          write (unit = unit) d%x(i), d%integral(i), d%variance(i)
       else
          write (unit = unit) d%x(i)
       end if
    end do
    write (unit = unit) MAGIC_DIVISION_END
  end subroutine write_division_raw_unit
  subroutine read_division_raw_unit (d, unit, read_integrals)
    type(division), intent(inout) :: d
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    logical :: read_integrals0
    integer :: i, num_div, magic
    character(len=*), parameter :: FN = "read_division_raw_unit"
    read_integrals0 = .false.
    if (present(read_integrals))  read_integrals0 = read_integrals
    read (unit = unit) magic
    if (magic /= MAGIC_DIVISION_BEGIN) then
       print *, FN, " fatal: expecting magic ", MAGIC_DIVISION_BEGIN, &
                    ", found ", magic
       stop
    end if
    read (unit = unit) num_div
    if (associated (d%x)) then
       if (ubound (d%x, dim=1) /= num_div) then
          deallocate (d%x, d%integral, d%variance)
          allocate (d%x(0:num_div), d%integral(num_div))
          allocate (d%variance(num_div))
       end if
    else
       allocate (d%x(0:num_div), d%integral(num_div))
       allocate (d%variance(num_div))
    end if
    read (unit = unit) d%ng
    read (unit = unit) d%stratified
    read (unit = unit) d%dx
    read (unit = unit) d%dxg
    read (unit = unit) d%x_min
    read (unit = unit) d%x_max
    read (unit = unit) d%x_min_true
    read (unit = unit) d%x_max_true
    do i = 0, ubound (d%x, dim=1)
       if (read_integrals0 .and. i/=0) then
          read (unit = unit) d%x(i), d%integral(i), d%variance(i)
       else
          read (unit = unit) d%x(i)
       end if
    end do
    if (.not.read_integrals0) then
       d%integral = 0.0
       d%variance = 0.0
    end if
    read (unit = unit) magic
    if (magic /= MAGIC_DIVISION_END) then
       print *, FN, " fatal: expecting magic ", MAGIC_DIVISION_END, &
                    ", found ", magic
       stop
    end if
  end subroutine read_division_raw_unit
  subroutine write_division_raw_name (d, name, write_integrals)
    type(division), intent(in) :: d
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", &
          form = "unformatted", file = name)
    call write_division_unit (d, unit, write_integrals)
    close (unit = unit)
  end subroutine write_division_raw_name
  subroutine read_division_raw_name (d, name, read_integrals)
    type(division), intent(inout) :: d
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", &
          form = "unformatted", file = name)
    call read_division_unit (d, unit, read_integrals)
    close (unit = unit)
  end subroutine read_division_raw_name
!
  subroutine marshal_division (d, ibuf, dbuf)
    type(division), intent(in) :: d
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=default), dimension(:), intent(inout) :: dbuf
    integer :: num_div
    num_div = ubound (d%x, dim=1)
    ibuf(1) = d%ng
    ibuf(2) = num_div
    if (d%stratified) then
       ibuf(3) = 1
    else
       ibuf(3) = 0
    end if
    dbuf(1) = d%x_min
    dbuf(2) = d%x_max
    dbuf(3) = d%x_min_true
    dbuf(4) = d%x_max_true
    dbuf(5) = d%dx
    dbuf(6) = d%dxg
    dbuf(7:7+num_div) = d%x
    dbuf(8+num_div:7+2*num_div) = d%integral
    dbuf(8+2*num_div:7+3*num_div) = d%variance
  end subroutine marshal_division
  subroutine marshal_division_size (d, iwords, dwords)
    type(division), intent(in) :: d
    integer, intent(out) :: iwords, dwords
    iwords = 3
    dwords = 7 + 3 * ubound (d%x, dim=1)
  end subroutine marshal_division_size
  subroutine unmarshal_division (d, ibuf, dbuf)
    type(division), intent(inout) :: d
    integer, dimension(:), intent(in) :: ibuf
    real(kind=default), dimension(:), intent(in) :: dbuf
    integer :: num_div
    d%ng = ibuf(1)
    num_div = ibuf(2)
    d%stratified = ibuf(3) /= 0
    d%x_min = dbuf(1)
    d%x_max = dbuf(2)
    d%x_min_true = dbuf(3)
    d%x_max_true = dbuf(4)
    d%dx = dbuf(5)
    d%dxg = dbuf(6)
    if (associated (d%x)) then
       if (ubound (d%x, dim=1) /= num_div) then
          deallocate (d%x, d%integral, d%variance)
          allocate (d%x(0:num_div), d%integral(num_div), d%variance(num_div))
       end if
    else
       allocate (d%x(0:num_div), d%integral(num_div), d%variance(num_div))
    end if
    d%x = dbuf(7:7+num_div)
    d%integral = dbuf(8+num_div:7+2*num_div)
    d%variance = dbuf(8+2*num_div:7+3*num_div)
  end subroutine unmarshal_division
  subroutine marshal_div_history (h, ibuf, dbuf)
    type(div_history), intent(in) :: h
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=default), dimension(:), intent(inout) :: dbuf
    ibuf(1) = h%ng
    ibuf(2) = h%num_div
    if (h%stratified) then
       ibuf(3) = 1
    else
       ibuf(3) = 0
    end if
    dbuf(1) = h%x_min
    dbuf(2) = h%x_max
    dbuf(3) = h%x_min_true
    dbuf(4) = h%x_max_true
    dbuf(5) = h%spread_f_p
    dbuf(6) = h%stddev_f_p
    dbuf(7) = h%spread_p
    dbuf(8) = h%stddev_p
    dbuf(9) = h%spread_m
    dbuf(10) = h%stddev_m
  end subroutine marshal_div_history
  subroutine marshal_div_history_size (h, iwords, dwords)
    type(div_history), intent(in) :: h
    integer, intent(out) :: iwords, dwords
    iwords = 3
    dwords = 10
  end subroutine marshal_div_history_size
  subroutine unmarshal_div_history (h, ibuf, dbuf)
    type(div_history), intent(inout) :: h
    integer, dimension(:), intent(in) :: ibuf
    real(kind=default), dimension(:), intent(in) :: dbuf
    h%ng = ibuf(1)
    h%num_div = ibuf(2)
    h%stratified = ibuf(3) /= 0
    h%x_min = dbuf(1)
    h%x_max = dbuf(2)
    h%x_min_true = dbuf(3)
    h%x_max_true = dbuf(4)
    h%spread_f_p = dbuf(5)
    h%stddev_f_p = dbuf(6)
    h%spread_p = dbuf(7)
    h%stddev_p = dbuf(8)
    h%spread_m = dbuf(9)
    h%stddev_m = dbuf(10)
  end subroutine unmarshal_div_history
  subroutine copy_division_s (lhs, rhs)
    type(division), intent(inout) :: lhs
    type(division), intent(in) :: rhs
    if (associated (rhs%x)) then
       call copy_array_pointer (lhs%x, rhs%x, lb = 0)
    else if (associated (lhs%x)) then
       deallocate (lhs%x)
    end if
    if (associated (rhs%integral)) then
       call copy_array_pointer (lhs%integral, rhs%integral)
    else if (associated (lhs%integral)) then
       deallocate (lhs%integral)
    end if
    if (associated (rhs%variance)) then
       call copy_array_pointer (lhs%variance, rhs%variance)
    else if (associated (lhs%variance)) then
       deallocate (lhs%variance)
    end if
    lhs%dx = rhs%dx
    lhs%dxg = rhs%dxg
    lhs%x_min = rhs%x_min
    lhs%x_max = rhs%x_max
    lhs%x_min_true = rhs%x_min_true
    lhs%x_max_true = rhs%x_max_true
    lhs%ng = rhs%ng
    lhs%stratified = rhs%stratified
  end subroutine copy_division_s
  subroutine delete_division_s (d)
    type(division), intent(inout) :: d
    if (associated (d%x)) then
       deallocate (d%x, d%integral, d%variance)
    end if
  end subroutine delete_division_s
  subroutine copy_history_s (lhs, rhs)
    type(div_history), intent(out) :: lhs
    type(div_history), intent(in) :: rhs
    lhs%stratified = rhs%stratified
    lhs%ng = rhs%ng
    lhs%num_div = rhs%num_div
    lhs%x_min = rhs%x_min
    lhs%x_max = rhs%x_max
    lhs%x_min_true = rhs%x_min_true
    lhs%x_max_true = rhs%x_max_true
    lhs%spread_f_p = rhs%spread_f_p
    lhs%stddev_f_p = rhs%stddev_f_p
    lhs%spread_p = rhs%spread_p
    lhs%stddev_p = rhs%stddev_p
    lhs%spread_m = rhs%spread_m
    lhs%stddev_m = rhs%stddev_m
  end subroutine copy_history_s
end module divisions
! vamp.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module vamp_grid_type
  use kinds
  use divisions

! *WK: Null pointer initialization (F95) useful for vamp_copy_grid
  type, public :: vamp_grid
     ! private !: forced by \texttt{use} association in interface
     type(division), dimension(:), pointer :: div               => null ()
     real(kind=default), dimension(:,:), pointer :: map         => null () 
     real(kind=default), dimension(:), pointer :: mu_x          => null () 
     real(kind=default), dimension(:), pointer :: sum_mu_x      => null () 
     real(kind=default), dimension(:,:), pointer :: mu_xx       => null () 
     real(kind=default), dimension(:,:), pointer :: sum_mu_xx   => null () 
     real(kind=default), dimension(2) :: mu
     real(kind=default) :: sum_integral, sum_weights, sum_chi2
     real(kind=default) :: calls, dv2g, jacobi
     real(kind=default) :: f_min, f_max
     real(kind=default) :: mu_gi, sum_mu_gi
     integer, dimension(:), pointer :: num_div                  => null ()
     integer :: num_calls, calls_per_cell
     logical :: stratified                                      
     logical :: all_stratified                                  
     logical :: quadrupole                                      
! *WK: New entries
     logical :: independent
     integer :: equivalent_to_ch, multiplicity
  end type vamp_grid
end module vamp_grid_type

! *WK This module has been added
module vamp_equivalences
  use kinds
  use divisions
  use vamp_grid_type !NODEP!
  implicit none
  private

  public :: vamp_equivalence_t
  public :: vamp_equivalences_t
  public :: vamp_equivalences_init
  public :: vamp_equivalences_final
  public :: vamp_equivalences_write
  public :: vamp_equivalence_set 
  public :: vamp_equivalences_complete

  type :: vamp_equivalence_t
     integer :: left, right
     integer, dimension(:), allocatable :: permutation
     integer, dimension(:), allocatable :: mode
  end type vamp_equivalence_t

  type :: vamp_equivalences_t
     type(vamp_equivalence_t), dimension(:), allocatable :: eq
     integer :: n_eq, n_ch
     integer, dimension(:), allocatable :: pointer
     logical, dimension(:), allocatable :: independent
     integer, dimension(:), allocatable :: equivalent_to_ch
     integer, dimension(:), allocatable :: multiplicity
     integer, dimension(:), allocatable :: symmetry
     logical, dimension(:,:), allocatable :: div_is_invariant
  end type vamp_equivalences_t

  integer, parameter, public :: &
       VEQ_IDENTITY = 0, VEQ_INVERT = 1, VEQ_SYMMETRIC = 2, VEQ_INVARIANT = 3

contains

  subroutine vamp_equivalence_init (eq, n_dim)
    type(vamp_equivalence_t), intent(inout) :: eq
    integer, intent(in) :: n_dim
    allocate (eq%permutation(n_dim), eq%mode(n_dim))
  end subroutine vamp_equivalence_init

  subroutine vamp_equivalences_init (eq, n_eq, n_ch, n_dim)
    type(vamp_equivalences_t), intent(inout) :: eq
    integer, intent(in) :: n_eq, n_ch, n_dim
    integer :: i
    eq%n_eq = n_eq
    eq%n_ch = n_ch
    allocate (eq%eq(n_eq))
    allocate (eq%pointer(n_ch+1))
    do i=1, n_eq
       call vamp_equivalence_init (eq%eq(i), n_dim)
    end do
    allocate (eq%independent(n_ch), eq%equivalent_to_ch(n_ch))
    allocate (eq%multiplicity(n_ch), eq%symmetry(n_ch))
    allocate (eq%div_is_invariant(n_ch, n_dim))
    eq%independent = .true.
    eq%equivalent_to_ch = 0
    eq%multiplicity = 0
    eq%symmetry = 0
    eq%div_is_invariant = .false.
  end subroutine vamp_equivalences_init

  subroutine vamp_equivalence_final (eq)
    type(vamp_equivalence_t), intent(inout) :: eq
    deallocate (eq%permutation, eq%mode)
  end subroutine vamp_equivalence_final

  subroutine vamp_equivalences_final (eq)
    type(vamp_equivalences_t), intent(inout) :: eq
!     integer :: i
!     do i=1, eq%n_eq
!        call vamp_equivalence_final (eq%eq(i))
!     end do
    if (allocated (eq%eq))  deallocate (eq%eq)
    if (allocated (eq%pointer))  deallocate (eq%pointer)
    if (allocated (eq%multiplicity))  deallocate (eq%multiplicity)
    if (allocated (eq%symmetry))  deallocate (eq%symmetry)
    if (allocated (eq%independent))  deallocate (eq%independent)
    if (allocated (eq%equivalent_to_ch))  deallocate (eq%equivalent_to_ch)
    if (allocated (eq%div_is_invariant))  deallocate (eq%div_is_invariant)
    eq%n_eq = 0
    eq%n_ch = 0
  end subroutine vamp_equivalences_final

  subroutine vamp_equivalence_write (eq, unit)
    integer, intent(in), optional :: unit
    integer :: u
    type(vamp_equivalence_t), intent(in) :: eq
    u = 6;  if (present (unit))  u = unit
    write (u, "(1x,A,2(1x,I4))") "Equivalent channels:", eq%left, eq%right
    write (u, "(1x,A,25(1x,I2))") "  Permutation:", eq%permutation
    write (u, "(1x,A,25(1x,I2))") "  Mode:       ", eq%mode
  end subroutine vamp_equivalence_write

  subroutine vamp_equivalences_write (eq, unit)
    type(vamp_equivalences_t), intent(in) :: eq
    integer, intent(in), optional :: unit
    integer :: u
    integer :: ch, i
    u = 6;  if (present (unit))  u = unit
    write (u, *) "Inequivalent channels:"
    do ch=1, eq%n_ch
       if (eq%independent(ch)) then
          write (u, *) "  Channel", ch, ":", &
               & "    Mult. =", eq%multiplicity(ch), &
               & "    Symm. =", eq%symmetry(ch), &
               & "    Invar.:", eq%div_is_invariant(ch,:)
       end if
    end do
    write (u, *) "Equivalence list:"
    if (allocated (eq%eq)) then
       do i=1, size (eq%eq)
          call vamp_equivalence_write (eq%eq(i), u)
       end do
    else
       write (u, *) "[not allocated]"
    end if
  end subroutine vamp_equivalences_write

  subroutine vamp_equivalence_set (eq, i, left, right, perm, mode)
    type(vamp_equivalences_t), intent(inout) :: eq
    integer, intent(in) :: i
    integer, intent(in) :: left, right
    integer, dimension(:), intent(in) :: perm, mode
    eq%eq(i)%left = left
    eq%eq(i)%right = right
    eq%eq(i)%permutation = perm
    eq%eq(i)%mode = mode
  end subroutine vamp_equivalence_set

  subroutine vamp_equivalences_complete (eq)
    type(vamp_equivalences_t), intent(inout) :: eq
    integer :: i, ch
    ch = 0
    do i=1, eq%n_eq
       if (ch /= eq%eq(i)%left) then
          ch = eq%eq(i)%left
          eq%pointer(ch) = i
       end if
    end do
    eq%pointer(ch+1) = eq%n_eq + 1
    do ch=1, eq%n_ch
       call set_multiplicities (eq%eq(eq%pointer(ch):eq%pointer(ch+1)-1))
    end do
 !   call write (6, eq)
  contains
    subroutine set_multiplicities (eq_ch)
      type(vamp_equivalence_t), dimension(:), intent(in) :: eq_ch
      integer :: i
      if (.not. all(eq_ch%left == ch) .or. eq_ch(1)%right > ch) then
         do i = 1, size (eq_ch)
            call vamp_equivalence_write (eq_ch(i))
         end do
         stop "VAMP: Equivalences: Something's wrong with equivalence ordering"
      end if
      eq%symmetry(ch) = count (eq_ch%right == ch)
      if (mod (size(eq_ch), eq%symmetry(ch)) /= 0) then
         do i = 1, size (eq_ch)
            call vamp_equivalence_write (eq_ch(i))
         end do
         stop "VAMP: Equivalences: Something's wrong with permutation count"
      end if
      eq%multiplicity(ch) = size (eq_ch) / eq%symmetry(ch)
      eq%independent(ch) = all (eq_ch%right >= ch)
      eq%equivalent_to_ch(ch) = eq_ch(1)%right
      eq%div_is_invariant(ch,:) = eq_ch(1)%mode == VEQ_INVARIANT
    end subroutine set_multiplicities
  end subroutine vamp_equivalences_complete

end module vamp_equivalences
! *WK

module vamp_rest
  use kinds
  use utils
  use exceptions
  use divisions
  use tao_random_numbers
  use vamp_stat
  use linalg
  use vamp_grid_type !NODEP!
  use vamp_equivalences !NODEP!
  implicit none

  private :: vamp_create_empty_grid_s, vamp_create_empty_grid_v
  private :: vamp_nullify_covariance_s, vamp_nullify_covariance_v
  private :: vamp_get_variance_s, vamp_get_variance_v
  private :: vamp_nullify_variance_s, vamp_nullify_variance_v
  private :: vamp_copy_grid_s, vamp_copy_grid_v
  private :: vamp_delete_grid_s, vamp_delete_grid_v
  private :: vamp_copy_grids_s, vamp_copy_grids_v
  private :: vamp_delete_grids_s, vamp_delete_grids_v
  private :: vamp_create_history_s, vamp_create_history_v, vamp_create_history_a
  private :: vamp_terminate_history_s, vamp_terminate_history_v
  private :: vamp_copy_history_s, vamp_copy_history_v, vamp_copy_history_a
  private :: vamp_delete_history_s, vamp_delete_history_v, vamp_delete_history_a
  public :: vamp_copy_grid, vamp_delete_grid
  public :: vamp_create_grid, vamp_create_empty_grid
  private :: map_domain
  public :: vamp_discard_integral
  private :: set_grid_options
  private :: vamp_reshape_grid_internal
  public :: vamp_reshape_grid
  public :: vamp_rigid_divisions
  public :: vamp_get_covariance, vamp_nullify_covariance
  public :: vamp_get_variance, vamp_nullify_variance
  public :: vamp_sample_grid
  public :: vamp_sample_grid0
  public :: vamp_refine_grid
! *WK: added this
  public :: vamp_refine_grids
! *WK
  public :: vamp_probability
  public :: vamp_average_iterations
  private :: vamp_average_iterations_grid
  public :: vamp_fork_grid
  private :: vamp_fork_grid_single, vamp_fork_grid_multi
  public :: vamp_join_grid
  private :: vamp_join_grid_single, vamp_join_grid_multi
  public :: vamp_fork_grid_joints
  public :: vamp_sample_grid_parallel
  public :: vamp_distribute_work
  public :: vamp_create_history, vamp_copy_history, vamp_delete_history
  public :: vamp_terminate_history
  public :: vamp_get_history, vamp_get_history_single
  public :: vamp_print_history, vamp_write_history
  private :: vamp_print_one_history, vamp_print_histories
  public :: vamp_jacobian, vamp_check_jacobian
  public :: vamp_create_grids, vamp_create_empty_grids
  public :: vamp_copy_grids, vamp_delete_grids
  public :: vamp_discard_integrals
  public :: vamp_update_weights
  public :: vamp_reshape_grids
  public :: vamp_sample_grids
  public :: vamp_reduce_channels
  public :: vamp_refine_weights
  private :: vamp_average_iterations_grids
  private :: vamp_get_history_multi
  public :: vamp_sum_channels
  public :: select_rotation_axis
  public :: select_rotation_subspace
  private :: more_pancake_than_cigar
  private :: select_subspace_explicit
  private :: select_subspace_guess
  public :: vamp_print_covariance
  ! private :: condense
  public :: condense
  ! private :: condense_action
  public :: condense_action
  public :: vamp_next_event
  private :: vamp_next_event_single, vamp_next_event_multi
  public :: vamp_warmup_grid, vamp_warmup_grids
  public :: vamp_integrate
  private :: vamp_integrate_grid, vamp_integrate_region
  public :: vamp_integratex
  private :: vamp_integratex_region
  public :: vamp_write_grid
  private :: write_grid_unit, write_grid_name
  public :: vamp_read_grid
  private :: read_grid_unit, read_grid_name
  public :: vamp_write_grids
  private :: write_grids_unit, write_grids_name
  public :: vamp_read_grids
  private :: read_grids_unit, read_grids_name
! *THO
  public :: vamp_read_grids_raw
  private :: read_grids_raw_unit, read_grids_raw_name
  public :: vamp_read_grid_raw
  private :: read_grid_raw_unit, read_grid_raw_name
  public :: vamp_write_grids_raw
  private :: write_grids_raw_unit, write_grids_raw_name
  public :: vamp_write_grid_raw
  private :: write_grid_raw_unit, write_grid_raw_name
!
  public :: vamp_marshal_grid_size, vamp_marshal_grid, vamp_unmarshal_grid
  public :: vamp_marshal_history_size, vamp_marshal_history
  public :: vamp_unmarshal_history
  interface vamp_create_empty_grid
     module procedure vamp_create_empty_grid_s, vamp_create_empty_grid_v
  end interface
  interface vamp_nullify_covariance
     module procedure vamp_nullify_covariance_s, vamp_nullify_covariance_v
  end interface
  interface vamp_get_variance
     module procedure vamp_get_variance_s, vamp_get_variance_v
  end interface
  interface vamp_nullify_variance
     module procedure vamp_nullify_variance_s, vamp_nullify_variance_v
  end interface
  interface vamp_copy_grid
     module procedure vamp_copy_grid_s, vamp_copy_grid_v
  end interface
  interface vamp_delete_grid
     module procedure vamp_delete_grid_s, vamp_delete_grid_v
  end interface
  interface vamp_copy_grids
     module procedure vamp_copy_grids_s, vamp_copy_grids_v
  end interface
  interface vamp_delete_grids
     module procedure vamp_delete_grids_s, vamp_delete_grids_v
  end interface
  interface vamp_create_history
     module procedure vamp_create_history_s, vamp_create_history_v, vamp_create_history_a
  end interface
  interface vamp_terminate_history
     module procedure vamp_terminate_history_s, vamp_terminate_history_v
  end interface
  interface vamp_copy_history
     module procedure vamp_copy_history_s, vamp_copy_history_v, vamp_copy_history_a
  end interface
  interface vamp_delete_history
     module procedure vamp_delete_history_s, vamp_delete_history_v, vamp_delete_history_a
  end interface
  interface vamp_average_iterations
     module procedure vamp_average_iterations_grid
  end interface
  interface vamp_fork_grid
     module procedure vamp_fork_grid_single, vamp_fork_grid_multi
  end interface
  interface vamp_join_grid
     module procedure vamp_join_grid_single, vamp_join_grid_multi
  end interface
  interface vamp_get_history
     module procedure vamp_get_history_single
  end interface
  interface vamp_print_history
     module procedure vamp_print_one_history, vamp_print_histories
  end interface
  interface vamp_write_history
     module procedure vamp_write_one_history_unit, vamp_write_histories_unit
  end interface
  interface vamp_average_iterations
     module procedure  vamp_average_iterations_grids
  end interface
  interface vamp_get_history
     module procedure vamp_get_history_multi
  end interface
  interface select_rotation_subspace
     module procedure select_subspace_explicit, select_subspace_guess
  end interface
  interface vamp_next_event
     module procedure vamp_next_event_single, vamp_next_event_multi
  end interface
  interface vamp_integrate
     module procedure vamp_integrate_grid, vamp_integrate_region
  end interface
  interface vamp_integratex
     module procedure vamp_integratex_region
  end interface
  interface vamp_write_grid
     module procedure write_grid_unit, write_grid_name
  end interface
  interface vamp_read_grid
     module procedure read_grid_unit, read_grid_name
  end interface
  interface vamp_write_grids
     module procedure write_grids_unit, write_grids_name
  end interface
  interface vamp_read_grids
     module procedure read_grids_unit, read_grids_name
  end interface
! *THO
  interface vamp_write_grid_raw
     module procedure write_grid_raw_unit, write_grid_raw_name
  end interface
  interface vamp_read_grid_raw
     module procedure read_grid_raw_unit, read_grid_raw_name
  end interface
  interface vamp_write_grids_raw
     module procedure write_grids_raw_unit, write_grids_raw_name
  end interface
  interface vamp_read_grids_raw
     module procedure read_grids_raw_unit, read_grids_raw_name
  end interface
  integer, parameter, private :: MAGIC_GRID = 22222222
  integer, parameter, private :: MAGIC_GRID_BEGIN = MAGIC_GRID + 1
  integer, parameter, private :: MAGIC_GRID_END = MAGIC_GRID + 2
  integer, parameter, private :: MAGIC_GRID_EMPTY = MAGIC_GRID + 3
  integer, parameter, private :: MAGIC_GRID_MAP = MAGIC_GRID + 4
  integer, parameter, private :: MAGIC_GRID_MU_X = MAGIC_GRID + 5
  integer, parameter, private :: MAGIC_GRIDS = 33333333
  integer, parameter, private :: MAGIC_GRIDS_BEGIN = MAGIC_GRIDS + 1
  integer, parameter, private :: MAGIC_GRIDS_END = MAGIC_GRIDS + 2
!
  type, public :: vamp_history

     real(kind=default) :: &
          integral, std_dev, avg_integral, avg_std_dev, avg_chi2, f_min, f_max
     integer :: calls
     logical :: stratified
     logical :: verbose
     type(div_history), dimension(:), pointer :: div            
  end type vamp_history
! *WK null-pointer initialization (F95) needed for vamp_copy_grids
  type, public :: vamp_grids
!!! private !: \emph{used by \texttt{vampi}}
     real(kind=default), dimension(:), pointer :: weights => null ()
     type(vamp_grid), dimension(:), pointer :: grids      => null ()
     integer, dimension(:), pointer :: num_calls          => null ()      
     real(kind=default) :: sum_chi2, sum_integral, sum_weights
  end type vamp_grids
! *WK 50 -> 20
  integer, private, parameter :: NUM_DIV_DEFAULT = 20
  real(kind=default), private, parameter :: QUAD_POWER = 0.5_default
  integer, private, parameter :: BUFFER_SIZE = 50
  character(len=*), parameter, private :: &
       descr_fmt =         "(1x,a)", &
       integer_fmt =       "(1x,a17,1x,i15)", &
       integer_array_fmt = "(1x,i17,1x,i15)", &
       logical_fmt =       "(1x,a17,1x,l1)", &
       double_fmt =        "(1x,a17,1x,e30.22)", &
       double_array_fmt =  "(1x,i17,1x,e30.22)", &
       double_array2_fmt =  "(2(1x,i8),1x,e30.22)"
  character(len=*), public, parameter :: VAMP_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"

contains
  subroutine vamp_create_empty_grid_v (g)
    type(vamp_grid), dimension(:), intent(inout) :: g
    integer :: i
    do i = 1, size (g)
       call vamp_create_empty_grid_s (g(i))
    end do
  end subroutine vamp_create_empty_grid_v
  subroutine vamp_nullify_covariance_v (g)
    type(vamp_grid), dimension(:), intent(inout) :: g
    integer :: i
    do i = 1, size (g)
       call vamp_nullify_covariance_s (g(i))
    end do
  end subroutine vamp_nullify_covariance_v
  function vamp_get_variance_v (g) result (v)
    type(vamp_grid), dimension(:), intent(in) :: g
    real(kind=default), dimension(size(g)) :: v
    integer :: i
    do i = 1, size (g)
       v(i) = vamp_get_variance_s (g(i))
    end do
  end function vamp_get_variance_v
  subroutine vamp_nullify_variance_v (g)
    type(vamp_grid), dimension(:), intent(inout) :: g
    integer :: i
    do i = 1, size (g)
       call vamp_nullify_variance_s (g(i))
    end do
  end subroutine vamp_nullify_variance_v
  subroutine vamp_copy_grid_v (lhs, rhs)
    type(vamp_grid), dimension(:), intent(inout) :: lhs
    type(vamp_grid), dimension(:), intent(in) :: rhs
    integer :: i
    do i = 1, size (lhs)
       call vamp_copy_grid_s (lhs(i), rhs(i))
    end do
  end subroutine vamp_copy_grid_v
  subroutine vamp_delete_grid_v (g)
    type(vamp_grid), dimension(:), intent(inout) :: g
    integer :: i
    do i = 1, size (g)
       call vamp_delete_grid_s (g(i))
    end do
  end subroutine vamp_delete_grid_v
  subroutine vamp_copy_grids_v (lhs, rhs)
    type(vamp_grids), dimension(:), intent(inout) :: lhs
    type(vamp_grids), dimension(:), intent(in) :: rhs
    integer :: i
    do i = 1, size (lhs)
       call vamp_copy_grids_s (lhs(i), rhs(i))
    end do
  end subroutine vamp_copy_grids_v
  subroutine vamp_delete_grids_v (g)
    type(vamp_grids), dimension(:), intent(inout) :: g
    integer :: i
    do i = 1, size (g)
       call vamp_delete_grids_s (g(i))
    end do
  end subroutine vamp_delete_grids_v
  subroutine vamp_create_history_v (h, ndim, verbose)
    type(vamp_history), dimension(:), intent(inout) :: h
    integer, intent(in), optional :: ndim
    logical, intent(in), optional :: verbose
    integer :: i
    do i = 1, size (h)
       call vamp_create_history_s (h(i), ndim, verbose)
    end do
  end subroutine vamp_create_history_v
  subroutine vamp_create_history_a (h, ndim, verbose)
    type(vamp_history), dimension(:,:), intent(inout) :: h
    integer, intent(in), optional :: ndim
    logical, intent(in), optional :: verbose
    integer :: i
    do i = 1, size (h, dim=2)
       call vamp_create_history_v (h(:,i), ndim, verbose)
    end do
  end subroutine vamp_create_history_a
  subroutine vamp_terminate_history_v (h)
    type(vamp_history), dimension(:), intent(inout) :: h
    integer :: i
    do i = 1, size (h)
       call vamp_terminate_history_s (h(i))
    end do
  end subroutine vamp_terminate_history_v
  subroutine vamp_copy_history_v (lhs, rhs)
    type(vamp_history), dimension(:), intent(out) :: lhs
    type(vamp_history), dimension(:), intent(in) :: rhs
    integer :: i
    do i = 1, size (rhs)
       call vamp_copy_history_s (lhs(i), rhs(i))
    end do
  end subroutine vamp_copy_history_v
  subroutine vamp_copy_history_a (lhs, rhs)
    type(vamp_history), dimension(:,:), intent(out) :: lhs
    type(vamp_history), dimension(:,:), intent(in) :: rhs
    integer :: i
    do i = 1, size (rhs, dim=2)
       call vamp_copy_history_v (lhs(:,i), rhs(:,i))
    end do
  end subroutine vamp_copy_history_a
  subroutine vamp_delete_history_v (h)
    type(vamp_history), dimension(:), intent(inout) :: h
    integer :: i
    do i = 1, size (h)
       call vamp_delete_history_s (h(i))
    end do
  end subroutine vamp_delete_history_v
  subroutine vamp_delete_history_a (h)
    type(vamp_history), dimension(:,:), intent(inout) :: h
    integer :: i
    do i = 1, size (h, dim=2)
       call vamp_delete_history_v (h(:,i))
    end do
  end subroutine vamp_delete_history_a
  subroutine vamp_create_grid &
       (g, domain, num_calls, num_div, &
       stratified, quadrupole, covariance, map, exc)
    type(vamp_grid), intent(inout) :: g
    real(kind=default), dimension(:,:), intent(in) :: domain
    integer, intent(in) :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole, covariance
    real(kind=default), dimension(:,:), intent(in), optional :: map
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_create_grid"
    real(kind=default), dimension(size(domain,dim=2)) :: &
         x_min, x_max, x_min_true, x_max_true
    integer :: ndim
    ndim = size (domain, dim=2)
    allocate (g%div(ndim), g%num_div(ndim))
    x_min = domain(1,:)
    x_max = domain(2,:)
    if (present (map)) then
       allocate (g%map(ndim,ndim))
       g%map = map
       x_min_true = x_min
       x_max_true = x_max
       call map_domain (g%map, x_min_true, x_max_true, x_min, x_max)
       call create_division (g%div, x_min, x_max, x_min_true, x_max_true)
    else
       nullify (g%map)
       call create_division (g%div, x_min, x_max)
    end if
    g%num_calls = num_calls
    if (present (num_div)) then
       g%num_div = num_div
    else
       g%num_div = NUM_DIV_DEFAULT
    end if
    g%stratified = .true.
    g%quadrupole = .false.
! *WK: initialize extra parameters
    g%independent = .true.
    g%equivalent_to_ch = 0
    g%multiplicity = 1
! *WK
    nullify (g%mu_x, g%mu_xx, g%sum_mu_x, g%sum_mu_xx)
    call vamp_discard_integral &
         (g, num_calls, num_div, stratified, quadrupole, covariance, exc)
  end subroutine vamp_create_grid
  subroutine map_domain (map, true_xmin, true_xmax, xmin, xmax)
    real(kind=default), dimension(:,:), intent(in) :: map
    real(kind=default), dimension(:), intent(in) :: true_xmin, true_xmax
    real(kind=default), dimension(:), intent(out) :: xmin, xmax
    real(kind=default), dimension(2**size(xmin),size(xmin)) :: corners
    integer, dimension(size(xmin)) :: zero_to_n
    integer :: j, ndim, perm
    ndim = size (xmin)
    zero_to_n = (/ (j, j=0,ndim-1) /)
    do perm = 1, 2**ndim
       corners (perm,:) = &
            merge (true_xmin, true_xmax, btest (perm-1, zero_to_n))
    end do
    corners = matmul (corners, map)
    xmin = minval (corners, dim=1)
    xmax = maxval (corners, dim=1)
  end subroutine map_domain
  subroutine vamp_create_empty_grid_s (g)
    type(vamp_grid), intent(inout) :: g
    nullify (g%div, g%num_div, g%map, g%mu_x, g%mu_xx, g%sum_mu_x, g%sum_mu_xx)
  end subroutine vamp_create_empty_grid_s
! *WK: Extra args independent, equivalent_to_ch, multiplicity
  subroutine vamp_discard_integral &
       (g, num_calls, num_div, stratified, quadrupole, covariance, exc, &
       & independent, equivalent_to_ch, multiplicity)
! *WK
    type(vamp_grid), intent(inout) :: g
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole, covariance
    type(exception), intent(inout), optional :: exc
    logical, intent(in), optional :: independent
    integer, intent(in), optional :: equivalent_to_ch, multiplicity
    character(len=*), parameter :: FN = "vamp_discard_integral"
    g%mu = 0.0
! *WK: was missing
    g%mu_gi = 0.0
! *WK
    g%sum_integral = 0.0
    g%sum_weights = 0.0
    g%sum_chi2 = 0.0
    g%sum_mu_gi = 0.0
    if (associated (g%sum_mu_x)) then
       g%sum_mu_x = 0.0
       g%sum_mu_xx = 0.0
    end if
! *WK: new options 'independent' and 'multiplicity'
    call set_grid_options (g, num_calls, num_div, stratified, quadrupole, &
         independent, equivalent_to_ch, multiplicity)
! *WK
    if ((present (num_calls)) &
         .or. (present (num_div)) &
         .or. (present (stratified)) &
         .or. (present (quadrupole)) &
         .or. (present (covariance))) then
       call vamp_reshape_grid &
            (g, g%num_calls, g%num_div, &
            g%stratified, g%quadrupole, covariance, exc)
    end if
  end subroutine vamp_discard_integral
! *WK: new options 'independent' and 'multiplicity'
  subroutine set_grid_options &
       (g, num_calls, num_div, stratified, quadrupole, &
        independent, equivalent_to_ch, multiplicity)
! *WK
    type(vamp_grid), intent(inout) :: g
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    logical, intent(in), optional :: independent
    integer, intent(in), optional :: equivalent_to_ch, multiplicity
    if (present (num_calls)) then
       g%num_calls = num_calls
    end if
    if (present (num_div)) then
       g%num_div = num_div
    end if
    if (present (stratified)) then
       g%stratified = stratified
    end if
    if (present (quadrupole)) then
       g%quadrupole = quadrupole
    end if
! *WK: new options 'independent' and 'multiplicity'
    if (present (independent)) then
       g%independent = independent
    end if
    if (present (equivalent_to_ch)) then
       g%equivalent_to_ch = equivalent_to_ch
    end if
    if (present (multiplicity)) then
       g%multiplicity = multiplicity
    end if
! *WK
  end subroutine set_grid_options
! *WK optional arguments multiplicity, independent
  subroutine vamp_reshape_grid_internal &
       (g, num_calls, num_div, &
       stratified, quadrupole, covariance, exc, use_variance, &
       independent, equivalent_to_ch, multiplicity)
! *WK
    type(vamp_grid), intent(inout) :: g
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole, covariance
    type(exception), intent(inout), optional :: exc
    logical, intent(in), optional :: use_variance
! *WK optional arguments multiplicity, independent
    logical, intent(in), optional :: independent
    integer, intent(in), optional :: equivalent_to_ch, multiplicity
! *WK
    integer :: ndim, num_cells
    integer, dimension(size(g%div)) :: ng
    character(len=*), parameter :: FN = "vamp_reshape_grid_internal"
    ndim = size (g%div)
! *WK: Set grid multiplicity etc. here
    call set_grid_options &
         (g, num_calls, num_div, stratified, quadrupole, &
         & independent, equivalent_to_ch, multiplicity)
! *WK
    if (g%stratified) then
       ng = (g%num_calls / 2.0 + 0.25)**(1.0/ndim)
! *WK: This did not work properly, disable it
!       ng = ng * real (g%num_div, kind=default) &
!            / (product (real (g%num_div, kind=default)))**(1.0/ndim)
! *WK
    else
       ng = 1
    end if
    call reshape_division (g%div, g%num_div, ng, use_variance)
    ! *WK: needed if a grid is used before sampled
    call clear_integral_and_variance (g%div)
    ! *WK
    num_cells = product (rigid_division (g%div))
    g%calls_per_cell = max (g%num_calls / num_cells, 2)
    g%calls = real (g%calls_per_cell) * real (num_cells)
    g%jacobi = product (volume_division (g%div)) / g%calls 
    g%dv2g = (g%calls / num_cells)**2 &
         / g%calls_per_cell / g%calls_per_cell / (g%calls_per_cell - 1.0)
    g%f_min = 1.0
    g%f_max = 0.0
    g%all_stratified = all (stratified_division (g%div))
    if (present (covariance)) then
       ndim = size (g%div)
       if (covariance .and. (.not. associated (g%mu_x))) then
          allocate (g%mu_x(ndim), g%mu_xx(ndim,ndim))
          allocate (g%sum_mu_x(ndim), g%sum_mu_xx(ndim,ndim))
          g%sum_mu_x = 0.0
          g%sum_mu_xx = 0.0
       else if ((.not. covariance) .and. (associated (g%mu_x))) then
          deallocate (g%mu_x, g%mu_xx, g%sum_mu_x, g%sum_mu_xx)
       end if
    end if
  end subroutine vamp_reshape_grid_internal
! *WK extra args independent, equivalent_to_ch, multiplicity
  subroutine vamp_reshape_grid &
       (g, num_calls, num_div, stratified, quadrupole, covariance, exc, &
       independent, equivalent_to_ch, multiplicity)
! *WK
    type(vamp_grid), intent(inout) :: g
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole, covariance
    type(exception), intent(inout), optional :: exc
! *WK extra args independent, equivalent_to_ch, multiplicity
    logical, intent(in), optional :: independent
    integer, intent(in), optional :: equivalent_to_ch, multiplicity
    call vamp_reshape_grid_internal &
         (g, num_calls, num_div, stratified, quadrupole, covariance, &
         exc, use_variance = .false., &
         independent=independent, equivalent_to_ch=equivalent_to_ch, &
         multiplicity=multiplicity)
! *WK
  end subroutine vamp_reshape_grid
  function vamp_rigid_divisions (g) result (ng)
    type(vamp_grid), intent(in) :: g
    integer, dimension(size(g%div)) :: ng
    ng = rigid_division (g%div)
  end function vamp_rigid_divisions
  function vamp_get_covariance (g) result (cov)
    type(vamp_grid), intent(in) :: g
    real(kind=default), dimension(size(g%div),size(g%div)) :: cov
    if (associated (g%mu_x)) then
       if (abs (g%sum_weights) <= tiny (cov(1,1))) then
          where (g%sum_mu_xx == 0.0_default)
             cov = 0.0
             elsewhere
             cov = huge (cov(1,1))
          endwhere
       else
          cov = g%sum_mu_xx / g%sum_weights &
               - outer_product (g%sum_mu_x, g%sum_mu_x) / g%sum_weights**2
       end if
    else
       cov = 0.0
    end if
  end function vamp_get_covariance
  subroutine vamp_nullify_covariance_s (g)
    type(vamp_grid), intent(inout) :: g
    if (associated (g%mu_x)) then
       g%sum_mu_x = 0
       g%sum_mu_xx = 0
    end if
  end subroutine vamp_nullify_covariance_s
  function vamp_get_variance_s (g) result (v)
    type(vamp_grid), intent(in) :: g
    real(kind=default) :: v
    if (abs (g%sum_weights) <= tiny (v)) then
       if (g%sum_mu_gi == 0.0_default) then
          v = 0.0
       else
          v = huge (v)
       end if
    else
       v = g%sum_mu_gi / g%sum_weights
    end if
  end function vamp_get_variance_s
  subroutine vamp_nullify_variance_s (g)
    type(vamp_grid), intent(inout) :: g
    g%sum_mu_gi = 0
  end subroutine vamp_nullify_variance_s
! *WK: extra argument prc_index
  subroutine vamp_sample_grid0 &
       (rng, g, func, prc_index, channel, weights, grids, exc)
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in), optional :: channel
    real(kind=default), dimension(:), intent(in), optional :: weights
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(exception), intent(inout), optional :: exc
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_sample_grid0"
! *WK: should work for all kind types
    real(kind=default), parameter :: &
         eps =  tiny (1._default) / epsilon (1._default)
! *WK: for exception warnings
    character(len=6) :: buffer
! *WK
    integer :: j, k
    integer, dimension(size(g%div)) :: cell
    real(kind=default) :: wgt, f, f2, sum_f, sum_f2, var_f
!    real(kind=default), dimension(size(g%div)):: r, x, x_mid, wgts
    real(kind=default), dimension(size(g%div)):: x, x_mid, wgts
    real(kind=double), dimension(size(g%div)) :: r
    integer, dimension(size(g%div)) :: ia
    integer :: ndim
    ndim = size (g%div)
    if (present (channel) .neqv. present (weights)) then
       call raise_exception (exc, EXC_FATAL, FN, &
            "channel and weights required together")
       return
    end if
    g%mu = 0.0
    cell = 1
    call clear_integral_and_variance (g%div)
    if (associated (g%mu_x)) then
       g%mu_x = 0.0
       g%mu_xx = 0.0
    end if
    if (present (channel)) then
       g%mu_gi = 0.0
    end if
    loop_over_cells: do
       sum_f = 0.0
       sum_f2 = 0.0
       do k = 1, g%calls_per_cell
          call tao_random_number (rng, r)
          call inject_division (g%div, real(r,kind=default), &
               cell, x, x_mid, ia, wgts)
          wgt = g%jacobi * product (wgts)
          if (associated (g%map)) then
             x = matmul (g%map, x)
          end if
          if (associated (g%map)) then
             if (all (inside_division (g%div, x))) then
                f = wgt * func (x, prc_index, weights, channel, grids)
             else
                f = 0.0
             end if
          else
             f = wgt * func (x, prc_index, weights, channel, grids)
          end if
          if (g%f_min > g%f_max) then
             g%f_min = f * g%calls
             g%f_max = f * g%calls
          else if (f * g%calls < g%f_min) then
             g%f_min = f * g%calls
          else if (f * g%calls > g%f_max) then
             g%f_max = f * g%calls
          end if
          f2 = f * f
          sum_f = sum_f + f
          sum_f2 = sum_f2 + f2
          call record_integral (g%div, ia, f)
          if ((associated (g%mu_x)) .and. (.not. g%all_stratified)) then
             g%mu_x = g%mu_x + x * f
             g%mu_xx = g%mu_xx + outer_product (x, x) * f
          end if
          if (present (channel)) then
             g%mu_gi = g%mu_gi + f2
          end if
       end do
       var_f = sum_f2 * g%calls_per_cell - sum_f**2
       if (var_f <= 0.0) then 
          var_f = tiny (1.0_default)
       end if
       g%mu = g%mu + (/ sum_f, var_f /)
       call record_variance (g%div, ia, var_f)
       if ((associated (g%mu_x)) .and. g%all_stratified) then
          if (associated (g%map)) then
             x_mid = matmul (g%map, x_mid)
          end if
          g%mu_x = g%mu_x + x_mid * var_f
          g%mu_xx = g%mu_xx + outer_product (x_mid, x_mid) * var_f
       end if
       do j = ndim, 1, -1
          cell(j) = modulo (cell(j), rigid_division (g%div(j))) + 1
          if (cell(j) /= 1) then
             cycle loop_over_cells
          end if
       end do
       exit loop_over_cells
    end do loop_over_cells
    g%mu(2) = g%mu(2) * g%dv2g
    if (g%mu(2) < eps * max (g%mu(1)**2, 1._default)) then
       g%mu(2) = eps * max (g%mu(1)**2, 1._default)
    end if
    if (g%mu(1)>0) then
       g%sum_integral = g%sum_integral + g%mu(1) / g%mu(2)
       g%sum_weights = g%sum_weights + 1.0 / g%mu(2)
       g%sum_chi2 = g%sum_chi2 + g%mu(1)**2 / g%mu(2)
       if (associated (g%mu_x)) then
          if (g%all_stratified) then
             g%mu_x = g%mu_x / g%mu(2)
             g%mu_xx = g%mu_xx / g%mu(2)
          else
             g%mu_x = g%mu_x / g%mu(1)
             g%mu_xx = g%mu_xx / g%mu(1)
          end if
          g%sum_mu_x = g%sum_mu_x + g%mu_x / g%mu(2)
          g%sum_mu_xx = g%sum_mu_xx + g%mu_xx / g%mu(2)
       end if
       if (present (channel)) then
          g%sum_mu_gi = g%sum_mu_gi + g%mu_gi / g%mu(2)
       end if
    else
       ! *WK: Warning/error message 
       if (present(channel) .and. g%mu(1)==0) then
          write (buffer, "(I6)")  channel
          call raise_exception (exc, EXC_WARN, "! vamp", &
               "Function identically zero in channel " // buffer)
       else if (present(channel) .and. g%mu(1)<0) then
          write (buffer, "(I6)")  channel
          call raise_exception (exc, EXC_ERROR, "! vamp", &
               "Negative integral in channel " // buffer)
       end if
       g%sum_integral = 0
       g%sum_chi2 = 0
       g%sum_weights = 0
    end if
  end subroutine vamp_sample_grid0
  function vamp_probability (g, x) result (p)
    type(vamp_grid), intent(in) :: g
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default) :: p
    p = product (probability (g%div, x))
  end function vamp_probability
! *WK Apply channel equivalences: sum variance entries
!     Note that n_bin must be even for this to work
  subroutine vamp_apply_equivalences (g, eq)
    type(vamp_grids), intent(inout) :: g
    type(vamp_equivalences_t), intent(in) :: eq
    integer :: n_ch, n_dim, nb, i, ch, ch_src, dim, dim_src
    integer, dimension(:,:), allocatable :: n_bin
    real(kind=default), dimension(:,:,:), allocatable :: var_tmp
    n_ch = size (g%grids)
    if (n_ch == 0)  return
    n_dim = size (g%grids(1)%div)
    allocate (n_bin(n_ch, n_dim))
    do ch = 1, n_ch
       do dim = 1, n_dim
          n_bin(ch, dim) = size (g%grids(ch)%div(dim)%variance)
       end do
    end do
    allocate (var_tmp (maxval(n_bin), n_dim, n_ch))
    var_tmp = 0
    do i=1, eq%n_eq
       ch = eq%eq(i)%left
       ch_src = eq%eq(i)%right
       do dim=1, n_dim
          nb = n_bin(ch_src, dim)
          dim_src = eq%eq(i)%permutation(dim)
          select case (eq%eq(i)%mode(dim))
          case (VEQ_IDENTITY)
             var_tmp(:nb,dim,ch) = var_tmp(:nb,dim,ch) &
                  & + g%grids(ch_src)%div(dim_src)%variance
          case (VEQ_INVERT)
             var_tmp(:nb,dim,ch) = var_tmp(:nb,dim,ch) &
                  & + g%grids(ch_src)%div(dim_src)%variance(nb:1:-1)
          case (VEQ_SYMMETRIC)
             var_tmp(:nb,dim,ch) = var_tmp(:nb,dim,ch) &
                  & + g%grids(ch_src)%div(dim_src)%variance / 2 &
                  & + g%grids(ch_src)%div(dim_src)%variance(nb:1:-1)/2
          case (VEQ_INVARIANT)
             var_tmp(:nb,dim,ch) = 1
          end select
       end do
    end do
    do ch=1, n_ch
       do dim=1, n_dim
          g%grids(ch)%div(dim)%variance = var_tmp(:n_bin(ch, dim),dim,ch)
       end do
    end do
    deallocate (var_tmp)
    deallocate (n_bin)
  end subroutine vamp_apply_equivalences
! *WK
  subroutine vamp_refine_grid (g, exc)
    type(vamp_grid), intent(inout) :: g
    type(exception), intent(inout), optional :: exc
    real(kind=default), dimension(size(g%div)) :: quad
    integer :: ndim
    if (g%quadrupole) then
       ndim = size (g%div)
       quad = (quadrupole_division (g%div))**QUAD_POWER
       call vamp_reshape_grid_internal &
            (g, use_variance = .true., exc = exc, &
            num_div = int (quad / product (quad)**(1.0/ndim) * g%num_div))
    else
       call refine_division (g%div)
    end if
  end subroutine vamp_refine_grid
! *WK: Added this for completeness.  No support for quadrupole, though
  subroutine vamp_refine_grids (g)
    type(vamp_grids), intent(inout) :: g
    integer :: ch
    do ch=1, size(g%grids)
! *WK: With equivalences, the integral may be zero
!       if (g%grids(ch)%sum_integral > 0)  &
       call refine_division (g%grids(ch)%div)
! *WK
    end do
  end subroutine vamp_refine_grids
! *WK
! *WK: extra argument prc_index
  subroutine vamp_sample_grid &
       (rng, g, func, prc_index, iterations, &
       integral, std_dev, avg_chi2, accuracy, &
       channel, weights, grids, exc, history)
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in) :: iterations
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    real(kind=default), intent(in), optional :: accuracy
    integer, intent(in), optional :: channel
    real(kind=default), dimension(:), intent(in), optional :: weights
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_sample_grid"
    real(kind=default) :: local_integral, local_std_dev, local_avg_chi2
    integer :: iteration, ndim
    ndim = size (g%div)
    iterate: do iteration = 1, iterations
       call vamp_sample_grid0 &
            (rng, g, func, prc_index, channel, weights, grids, exc)
       call vamp_average_iterations &
            (g, iteration, local_integral, local_std_dev, local_avg_chi2)
       if (present (history)) then
          if (iteration <= size (history)) then
             call vamp_get_history &
                  (history(iteration), g, local_integral, local_std_dev, &
                  local_avg_chi2)
          else
             call raise_exception (exc, EXC_WARN, FN, "history too short")
          end if
          call vamp_terminate_history (history(iteration+1:))
       end if
       if (present (accuracy)) then
          if (local_std_dev <= accuracy * local_integral) then
             call raise_exception (exc, EXC_INFO, FN, &
                  "requested accuracy reached")
             exit iterate
          end if
       end if
! *WK: Don't do this in the last iteration
       if (iteration < iterations)  call vamp_refine_grid (g)
    end do iterate
    if (present (integral)) then
       integral = local_integral
    end if
    if (present (std_dev)) then
       std_dev = local_std_dev
    end if
    if (present (avg_chi2)) then
       avg_chi2 = local_avg_chi2
    end if
  end subroutine vamp_sample_grid
! *WK: Fallback for zero channel
  subroutine vamp_average_iterations_grid &
       (g, iteration, integral, std_dev, avg_chi2)
    type(vamp_grid), intent(in) :: g
    integer, intent(in) :: iteration
    real(kind=default), intent(out) :: integral, std_dev, avg_chi2
! *WK
    real(kind=default), parameter :: eps = 1000 * epsilon (1._default)
    if (g%sum_weights>0) then
       integral = g%sum_integral / g%sum_weights
       std_dev = sqrt (1.0 / g%sum_weights)
       avg_chi2 = &
            max ((g%sum_chi2 - g%sum_integral * integral) / (iteration-0.99), &
            0.0_default)
! *WK This applies for constant functions:
       if (avg_chi2 < eps * g%sum_chi2)  avg_chi2 = 0
    else
       integral = 0
       std_dev = 0
       avg_chi2 = 0
    end if
  end subroutine vamp_average_iterations_grid
  subroutine vamp_fork_grid_single (g, gs, d, exc)
    type(vamp_grid), intent(in) :: g
    type(vamp_grid), dimension(:), intent(inout) :: gs
    integer, intent(in) :: d
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_fork_grid_single"
    type(division), dimension(:), allocatable :: d_tmp
    integer :: i, j, num_grids, num_div, ndim, num_cells
    num_grids = size (gs)
    ndim = size (g%div)
    num_div = size (g%div)
    do i = 1, size (gs)
       if (associated (gs(i)%div)) then
          if (size (gs(i)%div) /= num_div) then
             allocate (gs(i)%div(num_div))
             call create_empty_division (gs(i)%div)
          end if
       else
          allocate (gs(i)%div(num_div))
          call create_empty_division (gs(i)%div)
       end if
    end do
    do j = 1, ndim
       if (j == d) then
          allocate (d_tmp(num_grids))
          do i = 1, num_grids
             d_tmp(i) = gs(i)%div(j)
          end do
          call fork_division (g%div(j), d_tmp, g%calls_per_cell, gs%calls_per_cell, exc)
          do i = 1, num_grids
             gs(i)%div(j) = d_tmp(i)
          end do
          deallocate (d_tmp)
          if (present (exc)) then
             if (exc%level > EXC_WARN) then
                return
             end if
          end if
       else
          do i = 1, num_grids
             call copy_division (gs(i)%div(j), g%div(j))
          end do
       end if
    end do
    if (d == 0) then
       if (any (stratified_division (g%div))) then
          call raise_exception (exc, EXC_FATAL, FN, &
               "d == 0 incompatiple w/ stratification")
       else
          gs(2:)%calls_per_cell = ceiling (real (g%calls_per_cell) / num_grids)
          gs(1)%calls_per_cell = g%calls_per_cell - sum (gs(2:)%calls_per_cell)
       end if
    end if
    do i = 1, num_grids
       call copy_array_pointer (gs(i)%num_div, g%num_div)
       if (associated (g%map)) then
          call copy_array_pointer (gs(i)%map, g%map)
       end if
       if (associated (g%mu_x)) then
          call create_array_pointer (gs(i)%mu_x, ndim)
          call create_array_pointer (gs(i)%sum_mu_x, ndim)
          call create_array_pointer (gs(i)%mu_xx, (/ ndim, ndim /))
          call create_array_pointer (gs(i)%sum_mu_xx, (/ ndim, ndim /))
       end if
    end do
    gs%mu(1) = 0.0
    gs%mu(2) = 0.0
    gs%sum_integral = 0.0
    gs%sum_weights = 0.0
    gs%sum_chi2 = 0.0
    gs%mu_gi = 0.0
    gs%sum_mu_gi = 0.0
    gs%stratified = g%stratified
    gs%all_stratified = g%all_stratified
    gs%quadrupole = g%quadrupole
    do i = 1, num_grids
       num_cells = product (rigid_division (gs(i)%div))
       gs(i)%calls = gs(i)%calls_per_cell * num_cells
       gs(i)%num_calls = gs(i)%calls
       gs(i)%jacobi = product (volume_division (gs(i)%div)) / gs(i)%calls
       gs(i)%dv2g = (gs(i)%calls / num_cells)**2 &
            / gs(i)%calls_per_cell / gs(i)%calls_per_cell / (gs(i)%calls_per_cell - 1.0)
    end do
    gs%f_min = g%f_min * (gs%jacobi * gs%calls) / (g%jacobi * g%calls)
    gs%f_max = g%f_max * (gs%jacobi * gs%calls) / (g%jacobi * g%calls)
  end subroutine vamp_fork_grid_single
  subroutine vamp_join_grid_single (g, gs, d, exc)
    type(vamp_grid), intent(inout) :: g
    type(vamp_grid), dimension(:), intent(inout) :: gs
    integer, intent(in) :: d
    type(exception), intent(inout), optional :: exc
    type(division), dimension(:), allocatable :: d_tmp
    integer :: i, j, num_grids
    num_grids = size (gs)
    do j = 1, size (g%div)
       if (j == d) then
          allocate (d_tmp(num_grids))
          do i = 1, num_grids
             d_tmp(i) = gs(i)%div(j)
          end do
          call join_division (g%div(j), d_tmp, exc)
          deallocate (d_tmp)
          if (present (exc)) then
             if (exc%level > EXC_WARN) then
                return
             end if
          end if
       else
          allocate (d_tmp(num_grids))
          do i = 1, num_grids
             d_tmp(i) = gs(i)%div(j)
          end do
          call sum_division (g%div(j), d_tmp)
          deallocate (d_tmp)
       end if
    end do
    g%f_min = minval (gs%f_min * (g%jacobi * g%calls) / (gs%jacobi * gs%calls))
    g%f_max = maxval (gs%f_max * (g%jacobi * g%calls) / (gs%jacobi * gs%calls))
    g%mu(1) = sum (gs%mu(1))
    g%mu(2) = sum (gs%mu(2))
    g%mu_gi = sum (gs%mu_gi)
    g%sum_mu_gi = g%sum_mu_gi + g%mu_gi / g%mu(2)
    g%sum_integral = g%sum_integral + g%mu(1) / g%mu(2)
    g%sum_chi2 = g%sum_chi2 + g%mu(1)**2 / g%mu(2)
    g%sum_weights = g%sum_weights + 1.0 / g%mu(2)
    if (associated (g%mu_x)) then
       do i = 1, num_grids
          g%mu_x = g%mu_x + gs(i)%mu_x
          g%mu_xx = g%mu_xx + gs(i)%mu_xx
       end do
       g%sum_mu_x = g%sum_mu_x + g%mu_x / g%mu(2)
       g%sum_mu_xx = g%sum_mu_xx + g%mu_xx / g%mu(2)
    end if
  end subroutine vamp_join_grid_single
  recursive subroutine vamp_fork_grid_multi (g, gs, gx, d, exc)
    type(vamp_grid), intent(in) :: g
    type(vamp_grid), dimension(:), intent(inout) :: gs, gx
    integer, dimension(:,:), intent(in) :: d
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_fork_grid_multi"
    integer :: i, offset, stride, joints_offset, joints_stride
    select case (size (d, dim=2))
    case (0)
       return
    case (1)
       call vamp_fork_grid_single (g, gs, d(1,1), exc)
    case default
       offset = 1
       stride = product (d(2,2:))
       joints_offset = 1 + d(2,1)
       joints_stride = vamp_fork_grid_joints (d(:,2:))
       call vamp_create_empty_grid (gx(1:d(2,1)))
       call vamp_fork_grid_single (g, gx(1:d(2,1)), d(1,1), exc)
       do i = 1, d(2,1)
          call vamp_fork_grid_multi &
               (gx(i), gs(offset:offset+stride-1), &
               gx(joints_offset:joints_offset+joints_stride-1), &
               d(:,2:), exc)
          offset = offset + stride
          joints_offset = joints_offset + joints_stride
       end do
    end select
  end subroutine vamp_fork_grid_multi
  function vamp_fork_grid_joints (d) result (s)
    integer, dimension(:,:), intent(in) :: d
    integer :: s
    integer :: i
    s = 0
    do i = size (d, dim=2) - 1, 1, -1
       s = (s + 1) * d(2,i)
    end do
  end function vamp_fork_grid_joints
  recursive subroutine vamp_join_grid_multi (g, gs, gx, d, exc)
    type(vamp_grid), intent(inout) :: g
    type(vamp_grid), dimension(:), intent(inout) :: gs, gx
    integer, dimension(:,:), intent(in) :: d
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_join_grid_multi"
    integer :: i, offset, stride, joints_offset, joints_stride
    select case (size (d, dim=2))
    case (0)
       return
    case (1)
       call vamp_join_grid_single (g, gs, d(1,1), exc)
    case default
       offset = 1
       stride = product (d(2,2:))
       joints_offset = 1 + d(2,1)
       joints_stride = vamp_fork_grid_joints (d(:,2:))
       do i = 1, d(2,1)
          call vamp_join_grid_multi &
               (gx(i), gs(offset:offset+stride-1), &
               gx(joints_offset:joints_offset+joints_stride-1), &
               d(:,2:), exc)
          offset = offset + stride
          joints_offset = joints_offset + joints_stride
       end do
       call vamp_join_grid_single (g, gx(1:d(2,1)), d(1,1), exc)
       call vamp_delete_grid (gx(1:d(2,1)))
    end select
  end subroutine vamp_join_grid_multi
! *WK: extra argument prc_index
  subroutine vamp_sample_grid_parallel &
       (rng, g, func, prc_index, iterations, &
       integral, std_dev, avg_chi2, accuracy, &
       channel, weights, grids, exc, history)
    type(tao_random_state), dimension(:), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in) :: iterations
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    real(kind=default), intent(in), optional :: accuracy
    integer, intent(in), optional :: channel
    real(kind=default), dimension(:), intent(in), optional :: weights
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_sample_grid_parallel"
    real(kind=default) :: local_integral, local_std_dev, local_avg_chi2
    type(exception), dimension(size(rng)) :: excs
    type(vamp_grid), dimension(:), allocatable :: gs, gx
!hpf$ processors p(number_of_processors())
!hpf$ distribute gs(cyclic(1)) onto p
    integer, dimension(:,:), pointer :: d
    integer :: iteration, i
    integer :: num_workers
    nullify (d)
    call clear_exception (excs)
    iterate: do iteration = 1, iterations
       call vamp_distribute_work (size (rng), vamp_rigid_divisions (g), d)
       num_workers = max (1, product (d(2,:)))
       if (num_workers > 1) then
          allocate (gs(num_workers), gx(vamp_fork_grid_joints (d)))
          call vamp_create_empty_grid (gs)
          !: \texttt{vamp\_fork\_grid} is certainly not local.  Speed freaks might
          !: want to tune it to the processor topology, but the gain will be small.
          call vamp_fork_grid (g, gs, gx, d, exc)
!hpf$ independent
          do i = 1, num_workers
             call vamp_sample_grid0 &
                  (rng(i), gs(i), func, prc_index, &
                   channel, weights, grids, exc)
          end do
          if ((present (exc)) .and. (any (excs(1:num_workers)%level > 0))) then
             call gather_exceptions (exc, excs(1:num_workers))
          end if
          call vamp_join_grid (g, gs, gx, d, exc)
          call vamp_delete_grid (gs)
          deallocate (gs, gx)
       else
          call vamp_sample_grid0 &
               (rng(1), g, func, prc_index, channel, weights, grids, exc)
       end if
       if (present (exc)) then
          if (exc%level > EXC_WARN) then
             return
          end if
       end if
       call vamp_average_iterations &
            (g, iteration, local_integral, local_std_dev, local_avg_chi2)
       if (present (history)) then
          if (iteration <= size (history)) then
             call vamp_get_history &
                  (history(iteration), g, local_integral, local_std_dev, &
                  local_avg_chi2)
          else
             call raise_exception (exc, EXC_WARN, FN, "history too short")
          end if
          call vamp_terminate_history (history(iteration+1:))
       end if
       if (present (accuracy)) then
          if (local_std_dev <= accuracy * local_integral) then
             call raise_exception (exc, EXC_INFO, FN, &
                  "requested accuracy reached")
             exit iterate
          end if
       end if
! *WK: don't do this in the last iteration
       if (iteration < iterations)   call vamp_refine_grid (g)
!
    end do iterate
    deallocate (d)
    if (present (integral)) then
       integral = local_integral
    end if
    if (present (std_dev)) then
       std_dev = local_std_dev
    end if
    if (present (avg_chi2)) then
       avg_chi2 = local_avg_chi2
    end if
  end subroutine vamp_sample_grid_parallel
  subroutine vamp_distribute_work (num_workers, ng, d)
    integer, intent(in) :: num_workers
    integer, dimension(:), intent(in) :: ng
    integer, dimension(:,:), pointer :: d
    integer, dimension(32) :: factors
    integer :: n, num_factors, i, j
    integer, dimension(size(ng)) :: num_forks
    integer :: nfork
    try: do n = num_workers, 1, -1
       call factorize (n, factors, num_factors)
       num_forks = 1
       do i = num_factors, 1, -1
          j = sum (maxloc (ng / num_forks))
          nfork = num_forks(j) * factors(i)
          if (nfork <= ng(j)) then
             num_forks(j) = nfork
          else
             cycle try
          end if
       end do
       j = count (num_forks > 1)
       if (associated (d)) then
          if (size (d, dim = 2) /= j) then
             deallocate (d)
             allocate (d(2,j))
          end if
       else
          allocate (d(2,j))
       end if
       j = 1
       do i = 1, size (ng)
          if (num_forks(i) > 1) then
             d(:,j) = (/ i, num_forks(i) /)
             j = j + 1
          end if
       end do
       return
    end do try
  end subroutine vamp_distribute_work
  subroutine vamp_create_history_s (h, ndim, verbose)
    type(vamp_history), intent(out) :: h
    integer, intent(in), optional :: ndim
    logical, intent(in), optional :: verbose
    if (present (verbose)) then
       h%verbose = verbose
    else
       h%verbose = .false.
    end if
    h%calls = 0.0
    if (h%verbose .and. (present (ndim))) then
       allocate (h%div(ndim))
    else
       nullify (h%div)
    end if
  end subroutine vamp_create_history_s
  subroutine vamp_terminate_history_s (h)
    type(vamp_history), intent(inout) :: h
    h%calls = 0.0
  end subroutine vamp_terminate_history_s
  subroutine vamp_get_history_single (h, g, integral, std_dev, avg_chi2)
    type(vamp_history), intent(inout) :: h
    type(vamp_grid), intent(in) :: g
    real(kind=default), intent(in) :: integral, std_dev, avg_chi2
    h%calls = g%calls
    h%stratified = g%all_stratified
    h%integral = g%mu(1)
    h%std_dev = sqrt (g%mu(2))
    h%avg_integral = integral
    h%avg_std_dev = std_dev
    h%avg_chi2 = avg_chi2
    h%f_min = g%f_min
    h%f_max = g%f_max
    if (h%verbose) then
       if (associated (h%div)) then
          if (size (h%div) /= size (g%div)) then
             deallocate (h%div)
             allocate (h%div(size(g%div)))
          end if
       else
          allocate (h%div(size(g%div)))
       end if
       call copy_history (h%div, summarize_division (g%div))
    end if
  end subroutine vamp_get_history_single
  subroutine vamp_print_one_history (h, tag)
    type(vamp_history), dimension(:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    type(div_history), dimension(:), allocatable :: h_tmp
    character(len=BUFFER_SIZE) :: pfx
    character(len=1) :: s
    integer :: i, imax, j
    if (present (tag)) then
       pfx = tag
    else
       pfx = "[vamp]"
    end if
    print "(1X,A78)", repeat ("-", 78)
    print "(1X,A8,1X,A2,A9,A1,1X,A11,1X,8X,1X," &
         // "1X,A13,1X,8X,1X,A5,1X,A5)", &
         pfx, "it", "#calls", "", "integral", "average", "chi2", "eff."
    imax = size (h)
    iterations: do i = 1, imax
       if (h(i)%calls <= 0) then
          imax = i - 1
          exit iterations
       end if
       if (h(i)%stratified) then
          s = "*"
       else
          s = ""
       end if
       print "(1X,A8,1X,I2,I9,A1,1X,E11.4,A1,E8.2,A1," &
            // "1X,E13.6,A1,E8.2,A1,F5.1,1X,F5.3)", pfx, &
            i, h(i)%calls, s, h(i)%integral, "(", h(i)%std_dev, ")", &
            h(i)%avg_integral, "(", h(i)%avg_std_dev, ")", h(i)%avg_chi2, &
            h(i)%integral / h(i)%f_max
    end do iterations
    print "(1X,A78)", repeat ("-", 78)
    if (all (h%verbose) .and. (imax >= 1)) then
       if (associated (h(1)%div)) then
          allocate (h_tmp(imax))
          dimensions: do j = 1, size (h(1)%div)
             do i = 1, imax
                call copy_history (h_tmp(i), h(i)%div(j))
             end do
             if (present (tag)) then
                write (unit = pfx, fmt = "(A,A1,I2.2)") &
                     trim (tag(1:min(len_trim(tag),8))), "#", j
             else
                write (unit = pfx, fmt = "(A,A1,I2.2)") "[vamp]", "#", j
             end if
             call print_history (h_tmp, tag = pfx)
             print "(1X,A78)", repeat ("-", 78)
          end do dimensions
          deallocate (h_tmp)
       end if
    end if
  end subroutine vamp_print_one_history
  subroutine vamp_print_histories (h, tag)
    type(vamp_history), dimension(:,:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    character(len=BUFFER_SIZE) :: pfx
    integer :: i
    print "(1X,A78)", repeat ("=", 78)
    channels: do i = 1, size (h, dim=2)
       if (present (tag)) then
          write (unit = pfx, fmt = "(A4,A1,I3.3)") tag, "#", i
       else
          write (unit = pfx, fmt = "(A4,A1,I3.3)") "chan", "#", i
       end if
       call vamp_print_one_history (h(:,i), pfx)
    end do channels
    print "(1X,A78)", repeat ("=", 78)
  end subroutine vamp_print_histories
  subroutine vamp_write_one_history_unit (u, h, tag)
    integer, intent(in) :: u
    type(vamp_history), dimension(:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    type(div_history), dimension(:), allocatable :: h_tmp
    character(len=BUFFER_SIZE) :: pfx
    character(len=1) :: s
    integer :: i, imax, j
    if (present (tag)) then
       pfx = tag
    else
       pfx = "[vamp]"
    end if
    write (u, "(1X,A78)") repeat ("-", 78)
    write (u, "(1X,A8,1X,A2,A9,A1,1X,A11,1X,8X,1X," &
         // "1X,A13,1X,8X,1X,A5,1X,A5)") &
         pfx, "it", "#calls", "", "integral", "average", "chi2", "eff."
    imax = size (h)
    iterations: do i = 1, imax
       if (h(i)%calls <= 0) then
          imax = i - 1
          exit iterations
       end if
       ! *WK: Skip zero channel
       if (h(i)%f_max==0) cycle
       if (h(i)%stratified) then
          s = "*"
       else
          s = ""
       end if
       write (u, "(1X,A8,1X,I2,I9,A1,1X,E11.4,A1,E8.2,A1," &
            // "1X,E13.6,A1,E8.2,A1,F5.1,1X,F5.3)") pfx, &
            i, h(i)%calls, s, h(i)%integral, "(", h(i)%std_dev, ")", &
            h(i)%avg_integral, "(", h(i)%avg_std_dev, ")", h(i)%avg_chi2, &
            h(i)%integral / h(i)%f_max
    end do iterations
    write (u, "(1X,A78)") repeat ("-", 78)
    if (all (h%verbose) .and. (imax >= 1)) then
       if (associated (h(1)%div)) then
          allocate (h_tmp(imax))
          dimensions: do j = 1, size (h(1)%div)
             do i = 1, imax
                call copy_history (h_tmp(i), h(i)%div(j))
             end do
             if (present (tag)) then
                write (unit = pfx, fmt = "(A,A1,I2.2)") &
                     trim (tag(1:min(len_trim(tag),8))), "#", j
             else
                write (unit = pfx, fmt = "(A,A1,I2.2)") "[vamp]", "#", j
             end if
             call write_history (u, h_tmp, tag = pfx)
             print "(1X,A78)", repeat ("-", 78)
          end do dimensions
          deallocate (h_tmp)
       end if
    end if
  end subroutine vamp_write_one_history_unit
  subroutine vamp_write_histories_unit (u, h, tag)
    integer, intent(in) :: u
    type(vamp_history), dimension(:,:), intent(in) :: h
    character(len=*), intent(in), optional :: tag
    character(len=BUFFER_SIZE) :: pfx
    integer :: i
    write (u, "(1X,A78)") repeat ("=", 78)
    channels: do i = 1, size (h, dim=2)
       if (present (tag)) then
          write (unit = pfx, fmt = "(A4,A1,I3.3)") tag, "#", i
       else
          write (unit = pfx, fmt = "(A4,A1,I3.3)") "chan", "#", i
       end if
       call vamp_write_one_history_unit (u, h(:,i), pfx)
    end do channels
    write (u, "(1X,A78)") repeat ("=", 78)
  end subroutine vamp_write_histories_unit
  subroutine vamp_jacobian (phi, channel, x, region, jacobian, delta_x)
    integer, intent(in) :: channel
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default), dimension(:,:), intent(in) :: region
    real(kind=default), intent(out) :: jacobian
    real(kind=default), intent(in), optional :: delta_x
    interface
       function phi (xi, channel) result (x)
         use kinds
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: channel
         real(kind=default), dimension(size(xi)) :: x
       end function phi
    end interface
    real(kind=default), dimension(size(x)) :: x_min, x_max
    real(kind=default), dimension(size(x)) :: x_plus, x_minus
    real(kind=default), dimension(size(x),size(x)) :: d_phi
    real(kind=default), parameter :: &
         dx_default = 10.0_default**(-precision(jacobian)/3)
    real(kind=default) :: dx
    integer :: j
    if (present (delta_x)) then
       dx = delta_x
    else
       dx = dx_default
    end if
    x_min = region(1,:)
    x_max = region(2,:)
    x_minus = max (x_min, x)
    x_plus = min (x_max, x)
    do j = 1, size (x)
       x_minus(j) = max (x_min(j), x(j) - dx)
       x_plus(j) = min (x_max(j), x(j) + dx)
       d_phi(:,j) = (phi (x_plus, channel) - phi (x_minus, channel)) &
            / (x_plus(j) - x_minus(j))
       x_minus(j) = max (x_min(j), x(j))
       x_plus(j) = min (x_max(j), x(j))
    end do
    call determinant (d_phi, jacobian)
    jacobian = abs (jacobian)
  end subroutine vamp_jacobian
! *WK: extra argument prc_index
  subroutine vamp_check_jacobian &
       (rng, n, func, prc_index, phi, channel, region, delta, x_delta)
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: n
    integer, intent(in) :: prc_index
    integer, intent(in) :: channel
    real(kind=default), dimension(:,:), intent(in) :: region
    real(kind=default), intent(out) :: delta
    real(kind=default), dimension(:), intent(out), optional :: x_delta
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    interface
       function phi (xi, channel) result (x)
         use kinds
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: channel
         real(kind=default), dimension(size(xi)) :: x
       end function phi
    end interface
    real(kind=default), dimension(size(region,dim=2)) :: x
    real(kind=double), dimension(size(region,dim=2)) :: xtmp
    real(kind=default) :: jac, d
    real(kind=default), dimension(0) :: wgts
    integer :: i
    delta = 0.0
    do i = 1, max (1, n)
       call tao_random_number (rng, xtmp)
       x = region(1,:) + (region(2,:) - region(1,:)) * xtmp
       call vamp_jacobian (phi, channel, x, region, jac)
       d = func (phi (x, channel), prc_index, wgts, channel) * jac &
            - 1.0_default
       if (abs (d) >= abs (delta)) then
          delta = d
          if (present (x_delta)) then
             x_delta = x
          end if
       end if
    end do
  end subroutine vamp_check_jacobian
  subroutine vamp_create_grids &
       (g, domain, num_calls, weights, maps, num_div, &
       stratified, quadrupole, exc)
    type(vamp_grids), intent(inout) :: g
    real(kind=default), dimension(:,:), intent(in) :: domain
    integer, intent(in) :: num_calls
    real(kind=default), dimension(:), intent(in) :: weights
    real(kind=default), dimension(:,:,:), intent(in), optional :: maps
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_create_grids"
    integer :: ch, nch
    nch = size (weights)
    allocate (g%grids(nch), g%weights(nch), g%num_calls(nch))
    g%weights = weights / sum (weights)
    g%num_calls = g%weights * num_calls
    do ch = 1, size (g%grids)
       if (present (maps)) then
          call vamp_create_grid &
               (g%grids(ch), domain, g%num_calls(ch), num_div, &
               stratified, quadrupole, map = maps(:,:,ch), exc = exc)
       else
          call vamp_create_grid &
               (g%grids(ch), domain, g%num_calls(ch), num_div, &
               stratified, quadrupole, exc = exc)
       end if
    end do
    g%sum_integral = 0.0
    g%sum_chi2 = 0.0
    g%sum_weights = 0.0
  end subroutine vamp_create_grids
  subroutine vamp_create_empty_grids (g)
    type(vamp_grids), intent(inout) :: g
    nullify (g%grids, g%weights, g%num_calls)
  end subroutine vamp_create_empty_grids
! *WK: Account for equivalences
  subroutine vamp_discard_integrals &
       (g, num_calls, num_div, stratified, quadrupole, exc, eq)
! *WK
    type(vamp_grids), intent(inout) :: g
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    type(exception), intent(inout), optional :: exc
! *WK: Account for equivalences
    type(vamp_equivalences_t), intent(in), optional :: eq
! *WK
    integer :: ch
    character(len=*), parameter :: FN = "vamp_discard_integrals"
    g%sum_integral = 0.0
    g%sum_weights = 0.0
    g%sum_chi2 = 0.0
    do ch = 1, size (g%grids)
       call vamp_discard_integral (g%grids(ch))
    end do
    if (present (num_calls)) then
! *WK: Account for equivalences
       call vamp_reshape_grids &
            (g, num_calls, num_div, stratified, quadrupole, exc, eq)
! *WK
    end if
  end subroutine vamp_discard_integrals
  subroutine vamp_update_weights &
       (g, weights, num_calls, num_div, stratified, quadrupole, exc)
    type(vamp_grids), intent(inout) :: g
    real(kind=default), dimension(:), intent(in) :: weights
    integer, intent(in), optional :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    type(exception), intent(inout), optional :: exc
    character(len=*), parameter :: FN = "vamp_update_weights"
    if (sum(weights)>0) then
       g%weights = weights / sum (weights)
    else
       g%weights = 1._default / size(g%weights)
    end if
    if (present (num_calls)) then
       call vamp_discard_integrals (g, num_calls, num_div, &
            stratified, quadrupole, exc)
    else
       call vamp_discard_integrals (g, sum (g%num_calls), num_div, &
            stratified, quadrupole, exc)
    end if
  end subroutine vamp_update_weights
! *WK: account for equivalences
  subroutine vamp_reshape_grids &
       (g, num_calls, num_div, stratified, quadrupole, exc, eq)
! *WK
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: num_calls
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    type(exception), intent(inout), optional :: exc
! *WK: account for equivalences
    type(vamp_equivalences_t), intent(in), optional :: eq
    integer, dimension(size(g%grids(1)%num_div)) :: num_div_new
! *WK
    integer :: ch
    character(len=*), parameter :: FN = "vamp_reshape_grids"
    g%num_calls = g%weights * num_calls
    do ch = 1, size (g%grids)
       if (g%num_calls(ch) >= 2) then
          if (present (eq)) then
! *WK: account for equivalences, make invariant divisions trivial
             if (present (num_div)) then
                num_div_new = num_div
             else
                num_div_new = g%grids(ch)%num_div
             end if
             where (eq%div_is_invariant(ch,:))
                num_div_new = 1
             end where
             call vamp_reshape_grid (g%grids(ch), g%num_calls(ch), &
                  num_div_new, stratified, quadrupole, exc = exc, &
                  independent = eq%independent(ch), &
                  equivalent_to_ch = eq%equivalent_to_ch(ch), &
                  multiplicity = eq%multiplicity(ch))
          else
             call vamp_reshape_grid (g%grids(ch), g%num_calls(ch), &
                  num_div, stratified, quadrupole, exc = exc)
          end if
! *WK
       else
          g%num_calls(ch) = 0
       end if
    end do
  end subroutine vamp_reshape_grids
! *WK: account for equivalences and empty-channel warnings
! *WK: extra argument prc_index
  subroutine vamp_sample_grids &
       (rng, g, func, prc_index, iterations, integral, std_dev, avg_chi2, &
       accuracy, history, histories, exc, eq, warn_error)
! *WK
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in) :: iterations
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    real(kind=default), intent(in), optional :: accuracy
    type(vamp_history), dimension(:), intent(inout), optional :: history
    type(vamp_history), dimension(:,:), intent(inout), optional :: histories
    type(exception), intent(inout), optional :: exc
! *WK: account for equivalences and empty-channel warnings
    type(vamp_equivalences_t), intent(in), optional :: eq
    logical, intent(in), optional :: warn_error
! *WK
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    integer :: ch, iteration
    type(exception), dimension(size(g%grids)) :: excs
    logical, dimension(size(g%grids)) :: active
    real(kind=default), dimension(size(g%grids)) :: weights, integrals, std_devs
    real(kind=default) :: local_integral, local_std_dev, local_avg_chi2
    character(len=*), parameter :: FN = "vamp_sample_grids"
    integrals = 0
    std_devs = 0
    active = (g%num_calls >= 2)
    where (active)
       weights = g%num_calls
       elsewhere
       weights = 0.0
    endwhere
    if (sum (weights) /= 0)  weights = weights / sum (weights)
    call clear_exception (excs)
    iterate: do iteration = 1, iterations
       do ch = 1, size (g%grids)
          if (active(ch)) then
             call vamp_discard_integral (g%grids(ch))
             call vamp_sample_grid0 &
                  (rng, g%grids(ch), func, prc_index, &
                   ch, weights, g%grids, excs(ch))
! *WK: exceptions processed immediately here, but kept
             if (present (exc) .and. present (warn_error)) then
                if (warn_error)  call handle_exception (excs(ch))
             end if
! *WK
             call vamp_average_iterations &
                  (g%grids(ch), iteration, integrals(ch), std_devs(ch), local_avg_chi2)
             if (present (histories)) then
                if (iteration <= ubound (histories, dim=1)) then
                   call vamp_get_history &
                        (histories(iteration,ch), g%grids(ch), &
                        integrals(ch), std_devs(ch), local_avg_chi2)
                else
                   call raise_exception (exc, EXC_WARN, FN, "history too short")
                end if
                call vamp_terminate_history (histories(iteration+1:,ch))
             end if
          else
             call vamp_nullify_variance_s (g%grids(ch))
             call vamp_nullify_covariance_s (g%grids(ch))
          end if
       end do
! *WK: Make use of symmetries to add equivalent variance entries
       if (present(eq))  call vamp_apply_equivalences (g, eq)
! *WK
! *WK: Call this only if there are further iterations; otherwise leave it
!      to the user
       if (iteration < iterations) then
          do ch = 1, size (g%grids)
             ! *WK: skip zero channel
             active(ch) = (integrals(ch)/=0)
             if (active(ch)) then
                call vamp_refine_grid (g%grids(ch))
             end if
          end do
       end if
! *WK
       if (present (exc) .and. (any (excs%level > 0))) then
          call gather_exceptions (exc, excs)
!          return
       end if
       call vamp_reduce_channels (g, integrals, std_devs, active)
       call vamp_average_iterations &
            (g, iteration, local_integral, local_std_dev, local_avg_chi2)
       if (present (history)) then
          if (iteration <= size (history)) then
             call vamp_get_history &
                  (history(iteration), g, local_integral, local_std_dev, &
                  local_avg_chi2)
          else
             call raise_exception (exc, EXC_WARN, FN, "history too short")
          end if
          call vamp_terminate_history (history(iteration+1:))
       end if
       if (present (accuracy)) then
          if (local_std_dev <= accuracy * local_integral) then
             call raise_exception (exc, EXC_INFO, FN, &
                  "requested accuracy reached")
             exit iterate
          end if
       end if
    end do iterate
    if (present (integral)) then
       integral = local_integral
    end if
    if (present (std_dev)) then
       std_dev = local_std_dev
    end if
    if (present (avg_chi2)) then
       avg_chi2 = local_avg_chi2
    end if
  end subroutine vamp_sample_grids
  subroutine vamp_reduce_channels (g, integrals, std_devs, active)
    type(vamp_grids), intent(inout) :: g
    real(kind=default), dimension(:), intent(in) :: integrals, std_devs
    logical, dimension(:), intent(in) :: active
    real(kind=default) :: this_integral, this_weight, total_calls
! *WK
    real(kind=default) :: total_variance
    if (.not.any(active)) return
    total_calls = sum (g%num_calls, mask=active)
    if (total_calls > 0) then
       this_integral = sum (g%num_calls * integrals, mask=active) / total_calls
    else
       this_integral = 0
    end if
    total_variance = sum ((g%num_calls*std_devs)**2, mask=active)
    if (total_variance > 0) then
       this_weight = total_calls**2 / total_variance
    else
       this_weight = 0
    end if
!
    g%sum_weights = g%sum_weights + this_weight
    g%sum_integral = g%sum_integral + this_weight * this_integral
    g%sum_chi2 = g%sum_chi2 + this_weight * this_integral**2
  end subroutine vamp_reduce_channels
  subroutine vamp_average_iterations_grids &
       (g, iteration, integral, std_dev, avg_chi2)
    type(vamp_grids), intent(in) :: g
    integer, intent(in) :: iteration
    real(kind=default), intent(out) :: integral, std_dev, avg_chi2
! *WK
    real(kind=default), parameter :: eps = 1000 * epsilon (1._default)
    if (g%sum_weights>0) then
       integral = g%sum_integral / g%sum_weights
       std_dev = sqrt (1.0 / g%sum_weights)
       avg_chi2 = &
            max ((g%sum_chi2 - g%sum_integral * integral) / (iteration-0.99), &
            0.0_default)
! *WK This applies for constant functions:
       if (avg_chi2 < eps * g%sum_chi2)  avg_chi2 = 0
    else
       integral = 0
       std_dev = 0
       avg_chi2 = 0
    end if
  end subroutine vamp_average_iterations_grids
  subroutine vamp_refine_weights (g, power)
    type(vamp_grids), intent(inout) :: g
    real(kind=default), intent(in), optional :: power
    real(kind=default) :: local_power 
    real(kind=default), parameter :: DEFAULT_POWER = 0.5_default
    if (present (power)) then
       local_power = power
    else
       local_power = DEFAULT_POWER
    end if
    call vamp_update_weights &
         (g, g%weights * vamp_get_variance (g%grids) ** local_power)
  end subroutine vamp_refine_weights
  subroutine vamp_get_history_multi (h, g, integral, std_dev, avg_chi2)
    type(vamp_history), intent(inout) :: h
    type(vamp_grids), intent(in) :: g
    real(kind=default), intent(in) :: integral, std_dev, avg_chi2
    h%calls = sum (g%grids%calls)
    h%stratified = all (g%grids%all_stratified)
    h%integral = 0.0
    h%std_dev = 0.0
    h%avg_integral = integral
    h%avg_std_dev = std_dev
    h%avg_chi2 = avg_chi2
    h%f_min = 0.0
    h%f_max = huge (h%f_max)
    if (h%verbose) then
       h%verbose = .false.
       if (associated (h%div)) then
          deallocate (h%div)
       end if
    end if
  end subroutine vamp_get_history_multi
! *WK: extra argument prc_index
  function vamp_sum_channels (x, weights, func, prc_index, grids) result (g)
    real(kind=default), dimension(:), intent(in) :: x, weights
    integer, intent(in) :: prc_index
    type(vamp_grid), dimension(:), intent(in), optional :: grids
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    real(kind=default) :: g
    integer :: ch
    g = 0.0
    do ch = 1, size (weights)
       g = g + weights(ch) * func (x, prc_index, weights, ch, grids)
    end do
  end function vamp_sum_channels
  subroutine more_pancake_than_cigar (eval, yes_or_no)
    real(kind=default), dimension(:), intent(in) :: eval
    logical, intent(out) :: yes_or_no
    integer, parameter :: N_CL = 2
    real(kind=default), dimension(size(eval)) :: evals
    real(kind=default), dimension(N_CL) :: cluster_pos
    integer, dimension(N_CL,2) :: clusters
    evals = eval
    call sort (evals)
    call condense (evals, cluster_pos, clusters)
    print *, clusters(1,2) - clusters(1,1) + 1, "small EVs: ", &
         evals(clusters(1,1):clusters(1,2))
    print *, clusters(2,2) - clusters(2,1) + 1, "large EVs: ", &
         evals(clusters(2,1):clusters(2,2))
    if ((clusters(1,2) - clusters(1,1)) &
         < (clusters(2,2) - clusters(2,1))) then
       print *, " => PANCAKE!"
       yes_or_no = .true.
    else
       print *, " => CIGAR!"
       yes_or_no = .false.
    end if
  end subroutine more_pancake_than_cigar
  subroutine select_rotation_axis (cov, r, pancake, cigar)
    real(kind=default), dimension(:,:), intent(in) :: cov
    real(kind=default), dimension(:,:), intent(out) :: r
    integer, intent(in), optional :: pancake, cigar
    integer :: num_pancake, num_cigar
    logical :: like_pancake
    real(kind=default), dimension(size(cov,dim=1),size(cov,dim=2)) :: evecs
    real(kind=default), dimension(size(cov,dim=1)) :: evals, abs_evec
    integer :: iv
    integer, dimension(2) :: i
    real(kind=default) :: cos_theta, sin_theta, norm
    if (present (pancake)) then
       num_pancake = pancake
    else
       num_pancake = -1
    endif
    if (present (cigar)) then
       num_cigar = cigar
    else
       num_cigar = -1
    endif
    call diagonalize_real_symmetric (cov, evals, evecs)
    if (num_pancake > 0) then
       print *, "FORCED PANCAKE: ", num_pancake
       iv = sum (minloc (evals))
    else if (num_cigar > 0) then
       print *, "FORCED CIGAR: ", num_cigar
       iv = sum (maxloc (evals))
    else
       call more_pancake_than_cigar (evals, like_pancake)
       if (like_pancake) then
          iv = sum (minloc (evals))
       else
          iv = sum (maxloc (evals))
       end if
    end if
    abs_evec = abs (evecs(:,iv))
    i(1) = sum (maxloc (abs_evec))
    abs_evec(i(1)) = -1.0
    i(2) = sum (maxloc (abs_evec))
    print *, iv, evals(iv), " => ", evecs(:,iv)
    print *, i(1), abs_evec(i(1)), ", ", i(2), abs_evec(i(2))
    print *, i(1), evecs(i(1),iv), ", ", i(2), evecs(i(2),iv)
    cos_theta = evecs(i(1),iv)
    sin_theta = evecs(i(2),iv)
    norm = 1.0 / sqrt (cos_theta**2 + sin_theta**2)
    cos_theta = cos_theta * norm
    sin_theta = sin_theta * norm
    call unit (r)
    r(i(1),i) =  (/   cos_theta, - sin_theta /)
    r(i(2),i) =  (/   sin_theta,   cos_theta /)
  end subroutine select_rotation_axis
  subroutine select_subspace_explicit (cov, r, subspace)
    real(kind=default), dimension(:,:), intent(in) :: cov
    real(kind=default), dimension(:,:), intent(out) :: r
    integer, dimension(:), intent(in) :: subspace
    real(kind=default), dimension(size(subspace)) :: eval_sub
    real(kind=default), dimension(size(subspace),size(subspace)) :: &
         cov_sub, evec_sub
    cov_sub = cov(subspace,subspace)
    call diagonalize_real_symmetric (cov_sub, eval_sub, evec_sub)
    call unit (r)
    r(subspace,subspace) = evec_sub
  end subroutine select_subspace_explicit
  subroutine select_subspace_guess (cov, r, ndim, pancake, cigar)
    real(kind=default), dimension(:,:), intent(in) :: cov
    real(kind=default), dimension(:,:), intent(out) :: r
    integer, intent(in) :: ndim
    integer, intent(in), optional :: pancake, cigar
    integer :: num_pancake, num_cigar
    logical :: like_pancake
    real(kind=default), dimension(size(cov,dim=1),size(cov,dim=2)) :: evecs
    real(kind=default), dimension(size(cov,dim=1)) :: evals, abs_evec
    integer :: iv, i
    integer, dimension(ndim) :: subspace
    if (present (pancake)) then
       num_pancake = pancake
    else
       num_pancake = -1
    endif
    if (present (cigar)) then
       num_cigar = cigar
    else
       num_cigar = -1
    endif
    call diagonalize_real_symmetric (cov, evals, evecs)
    if (num_pancake > 0) then
       print *, "FORCED PANCAKE: ", num_pancake
       iv = sum (minloc (evals))
    else if (num_cigar > 0) then
       print *, "FORCED CIGAR: ", num_cigar
       iv = sum (maxloc (evals))
    else
       call more_pancake_than_cigar (evals, like_pancake)
       if (like_pancake) then
          iv = sum (minloc (evals))
       else
          iv = sum (maxloc (evals))
       end if
    end if
    abs_evec = abs (evecs(:,iv))
    subspace(1) = sum (maxloc (abs_evec))
    do i = 2, ndim
       abs_evec(subspace(i-1)) = -1.0
       subspace(i) = sum (maxloc (abs_evec))
    end do
    call select_subspace_explicit (cov, r, subspace)
  end subroutine select_subspace_guess
  subroutine vamp_print_covariance (cov)
    real(kind=default), dimension(:,:), intent(in) :: cov
    real(kind=default), dimension(size(cov,dim=1)) :: &
         evals, abs_evals, tmp
    real(kind=default), dimension(size(cov,dim=1),size(cov,dim=2)) :: &
         evecs, abs_evecs
    integer, dimension(size(cov,dim=1)) :: idx
    integer :: i, i_max, j
    i_max = size (evals)
    call diagonalize_real_symmetric (cov, evals, evecs)
    call sort (evals, evecs)
    abs_evals = abs (evals)
    abs_evecs = abs (evecs)
    print "(1X,A78)", repeat ("-", 78)
    print "(1X,A)", "Eigenvalues and eigenvectors:"
    print "(1X,A78)", repeat ("-", 78)
    do i = 1, i_max
       print "(1X,I2,A1,1X,E11.4,1X,A1,10(10(1X,F5.2)/,18X))", &
            i, ":", evals(i), "|", evecs(:,i)
    end do
    print "(1X,A78)", repeat ("-", 78)
    print "(1X,A)", "Approximate subspaces:"
    print "(1X,A78)", repeat ("-", 78)
    do i = 1, i_max
       idx = (/ (j, j=1,i_max) /)
       tmp = abs_evecs(:,i)
       call sort (tmp, idx, reverse = .true.)
       print "(1X,I2,A1,1X,E11.4,1X,A1,10(1X,I5))", &
            i, ":", evals(i), "|", idx(1:min(10,size(idx)))
       print "(17X,A1,10(1X,F5.2))", &
            "|", evecs(idx(1:min(10,size(idx))),i)
    end do
    print "(1X,A78)", repeat ("-", 78)
  end subroutine vamp_print_covariance
  subroutine condense (x, cluster_pos, clusters, linear)
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default), dimension(:), intent(out) :: cluster_pos
    integer, dimension(:,:), intent(out) :: clusters
    logical, intent(in), optional :: linear
    logical :: linear_metric
    real(kind=default), dimension(size(x)) :: cl_pos
    real(kind=default) :: wgt0, wgt1
    integer :: cl_num
    integer, dimension(size(x),2) :: cl
    integer :: i, gap
    linear_metric = .false.
    if (present (linear)) then
       linear_metric = linear
    end if
    cl_pos = x
    cl_num = size (cl_pos)
    cl = spread ((/ (i, i=1,cl_num) /), dim = 2, ncopies = 2)
    do cl_num = size (cl_pos), size (cluster_pos) + 1, -1
       if (linear_metric) then
          gap = sum (minloc (cl_pos(2:cl_num) - cl_pos(1:cl_num-1)))
       else
          gap = sum (minloc (cl_pos(2:cl_num) / cl_pos(1:cl_num-1)))
       end if
       wgt0 = cl(gap,2) - cl(gap,1) + 1
       wgt1 = cl(gap+1,2) - cl(gap+1,1) + 1
       cl_pos(gap) = (wgt0 * cl_pos(gap) + wgt1 * cl_pos(gap+1)) / (wgt0 + wgt1)
       cl(gap,2) = cl(gap+1,2)
       cl_pos(gap+1:cl_num-1) = cl_pos(gap+2:cl_num)
       cl(gap+1:cl_num-1,:) = cl(gap+2:cl_num,:)
       print *, cl_num, ": action = ", condense_action (x, cl)
    end do
    cluster_pos = cl_pos(1:cl_num)
    clusters = cl(1:cl_num,:)
  end subroutine condense
  function condense_action (positions, clusters) result (s)
    real(kind=default), dimension(:), intent(in) :: positions
    integer, dimension(:,:), intent(in) :: clusters
    real(kind=default) :: s
    integer :: i
    integer, parameter :: POWER = 2
    s = 0
    do i = 1, size (clusters, dim = 1)
       s = s + standard_deviation (positions(clusters(i,1) &
            :clusters(i,2))) ** POWER
    end do
  end function condense_action
! *WK: extra argument prc_index
  subroutine vamp_next_event_single &
       (x, rng, g, func, prc_index, &
       weight, channel, weights, grids, exc)
    real(kind=default), dimension(:), intent(out) :: x
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    real(kind=default), intent(out), optional :: weight
    integer, intent(in) :: prc_index
    integer, intent(in), optional :: channel
    real(kind=default), dimension(:), intent(in), optional :: weights
    type(vamp_grid), dimension(:), intent(in), optional :: grids
    type(exception), intent(inout), optional :: exc
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_next_event_single"
!    real(kind=default), dimension(size(g%div)):: r, wgts
    real(kind=default), dimension(size(g%div)):: wgts
    real(kind=double), dimension(size(g%div)):: r
!    real(kind=default) :: f, r0, wgt
    real(kind=default) :: f, wgt
    real(kind=double) :: r0
    rejection: do
       call tao_random_number (rng, r)
       call inject_division_short (g%div, real(r,kind=default), x, wgts)
       wgt = g%jacobi * product (wgts)
       wgt = g%calls * wgt !: the calling procedure will divide by \#calls
       if (associated (g%map)) then
          x = matmul (g%map, x)
       end if
       if (associated (g%map)) then
          if (all (inside_division (g%div, x))) then
             f = wgt * func (x, prc_index, weights, channel, grids)
          else
             f = 0.0
          end if
       else
          f = wgt * func (x, prc_index, weights, channel, grids)
       end if
       if (present (weight)) then
          weight = f
          exit rejection
       else
          if (f > g%f_max) then
             g%f_max = f
             call raise_exception (exc, EXC_WARN, FN, "weight > 1")
             exit rejection
          end if
          call tao_random_number (rng, r0)
          if (r0 * g%f_max <= f) then
             exit rejection
          end if
       end if
    end do rejection
  end subroutine vamp_next_event_single
!  subroutine vamp_next_event_multi (x, rng, g, func, phi, weight, exc)
! WK
! *WK: extra argument prc_index
  subroutine vamp_next_event_multi &
       (x, rng, g, func, prc_index, phi, weight, excess, exc)
!
    real(kind=default), dimension(:), intent(out) :: x
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: prc_index
    real(kind=default), intent(out), optional :: weight
! WK
    real(kind=default), intent(out), optional :: excess
!
    type(exception), intent(inout), optional :: exc
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    interface
       function phi (xi, channel) result (x)
         use kinds
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: channel
         real(kind=default), dimension(size(xi)) :: x
       end function phi
    end interface
    character(len=*), parameter :: FN = "vamp_next_event_multi"
    real(kind=default), dimension(size(x)) :: xi
    real(kind=default) :: r, wgt
    real(kind=double) :: rtmp
    real(kind=default), dimension(size(g%weights)) :: weights
    integer :: channel
! *WK
    if (any (g%grids%f_max > 0)) then
       weights = g%weights * g%grids%f_max
    else
       weights = g%weights
    end if
!
    weights = weights / sum (weights)
    rejection: do
       call tao_random_number (rng, rtmp);  r = rtmp
       select_channel: do channel = 1, size (g%weights)
          r = r - weights(channel)
          if (r <= 0.0) then
             exit select_channel
          end if
       end do select_channel
       channel = min (channel, size (g%weights)) !: for $r=1$ and rounding errors
       call vamp_next_event_single &
            (xi, rng, g%grids(channel), func, prc_index, wgt, &
            channel, g%weights, g%grids, exc)
       if (present (weight)) then
          weight = wgt * g%weights(channel) / weights(channel)
          exit rejection
       else
          if (wgt > g%grids(channel)%f_max) then
! *WK: disable this feature (temporarily?)
!             g%grids(channel)%f_max = wgt
!             weights = g%weights * g%grids%f_max
!             weights = weights / sum (weights)
!             call raise_exception (exc, EXC_WARN, FN, "weight > 1")
! *WK: issue a warning (not for use in a pure function!)
             if (present(excess)) then
                excess = wgt/g%grids(channel)%f_max - 1
             else
                print *, "weight > 1 (", wgt/g%grids(channel)%f_max, &
                     &   ") in channel ", channel
             end if
          else
             if (present(excess)) excess = 0
! *WK
          end if
          call tao_random_number (rng, rtmp);  r = rtmp
          if (r * g%grids(channel)%f_max <= wgt) then
             exit rejection
          end if
       end if
    end do rejection
    x = phi (xi, channel)
  end subroutine vamp_next_event_multi
! *WK: extra argument prc_index
  subroutine vamp_warmup_grid &
       (rng, g, func, prc_index, iterations, exc, history)
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in) :: iterations
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    call vamp_sample_grid &
         (rng, g, func, prc_index, &
          iterations - 1, exc = exc, history = history)
    call vamp_sample_grid0 (rng, g, func, prc_index, exc = exc)
  end subroutine vamp_warmup_grid
! *WK: extra argument prc_index
  subroutine vamp_warmup_grids &
       (rng, g, func, prc_index, iterations, history, histories, exc)
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, intent(in) :: iterations
    type(vamp_history), dimension(:), intent(inout), optional :: history
    type(vamp_history), dimension(:,:), intent(inout), optional :: histories
    type(exception), intent(inout), optional :: exc
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    integer :: ch
    logical, dimension(size(g%grids)) :: active
    real(kind=default), dimension(size(g%grids)) :: weights
    active = (g%num_calls >= 2)
    where (active)
       weights = g%num_calls
       elsewhere
       weights = 0.0
    endwhere
    weights = weights / sum (weights)
    call vamp_sample_grids (rng, g, func, prc_index, iterations - 1, &
         exc = exc, history = history, histories = histories)
    do ch = 1, size (g%grids)
       if (g%grids(ch)%num_calls >= 2) then
          call vamp_sample_grid0 &
               (rng, g%grids(ch), func, prc_index, &
                ch, weights, g%grids, exc = exc)
       end if
    end do
  end subroutine vamp_warmup_grids
! *WK: extra argument prc_index
  subroutine vamp_integrate_grid &
       (rng, g, func, prc_index, calls, integral, std_dev, avg_chi2, num_div, &
       stratified, quadrupole, accuracy, exc, history)
    type(tao_random_state), intent(inout) :: rng
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: prc_index
    integer, dimension(:,:), intent(in) :: calls
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    real(kind=default), intent(in), optional :: accuracy
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_integrate_grid"
    integer :: step, last_step, it
    last_step = size (calls, dim = 2)
    it = 1
    do step = 1, last_step - 1
       call vamp_discard_integral (g, calls(2,step), num_div, &
            stratified, quadrupole, exc = exc)
       call vamp_sample_grid (rng, g, func, prc_index, calls(1,step), &
            exc = exc, history = history(it:))
       if (present (exc)) then
          if (exc%level > EXC_WARN) then
             return
          end if
       end if
       it = it + calls(1,step)
    end do
    call vamp_discard_integral (g, calls(2,last_step), exc = exc)
    call vamp_sample_grid (rng, g, func, prc_index, calls(1,last_step), &
         integral, std_dev, avg_chi2, accuracy, exc = exc, &
         history = history(it:))
  end subroutine vamp_integrate_grid
! *WK: extra argument prc_index
  subroutine vamp_integrate_region &
       (rng, region, func, prc_index, calls, &
       integral, std_dev, avg_chi2, num_div, &
       stratified, quadrupole, accuracy, map, covariance, exc, history)
    type(tao_random_state), intent(inout) :: rng
    real(kind=default), dimension(:,:), intent(in) :: region
    integer, intent(in) :: prc_index
    integer, dimension(:,:), intent(in) :: calls
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    real(kind=default), intent(in), optional :: accuracy
    real(kind=default), dimension(:,:), intent(in), optional :: map
    real(kind=default), dimension(:,:), intent(out), optional :: covariance
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    character(len=*), parameter :: FN = "vamp_integrate_region"
    type(vamp_grid) :: g
    call vamp_create_grid &
         (g, region, calls(2,1), num_div, &
         stratified, quadrupole, present (covariance), map, exc)
    call vamp_integrate_grid &
         (rng, g, func, prc_index, calls, &
         integral, std_dev, avg_chi2, num_div, &
         accuracy = accuracy, exc = exc, history = history)
    if (present (covariance)) then
       covariance = vamp_get_covariance (g)
    end if
    call vamp_delete_grid_s (g)
  end subroutine vamp_integrate_region
! *WK: extra argument prc_index
  subroutine vamp_integratex_region &
       (rng, region, func, prc_index, calls, integral, std_dev, avg_chi2, &
       num_div, stratified, quadrupole, accuracy, pancake, cigar, &
       exc, history)
    type(tao_random_state), intent(inout) :: rng
    real(kind=default), dimension(:,:), intent(in) :: region
    integer, intent(in) :: prc_index
    integer, dimension(:,:,:), intent(in) :: calls
    real(kind=default), intent(out), optional :: integral, std_dev, avg_chi2
    integer, dimension(:), intent(in), optional :: num_div
    logical, intent(in), optional :: stratified, quadrupole
    real(kind=default), intent(in), optional :: accuracy
    integer, intent(in), optional :: pancake, cigar
    type(exception), intent(inout), optional :: exc
    type(vamp_history), dimension(:), intent(inout), optional :: history
! *WK: extra argument prc_index
    interface
       function func (xi, prc_index, weights, channel, grids) result (f)
         use kinds
         use vamp_grid_type !NODEP!
         real(kind=default), dimension(:), intent(in) :: xi
         integer, intent(in) :: prc_index
         real(kind=default), dimension(:), intent(in), optional :: weights
         integer, intent(in), optional :: channel
         type(vamp_grid), dimension(:), intent(in), optional :: grids
         real(kind=default) :: f
       end function func
    end interface
    real(kind=default), dimension(size(region,dim=2)) :: eval
    real(kind=default), dimension(size(region,dim=2),size(region,dim=2)) :: evec
    type(vamp_grid) :: g
    integer :: step, last_step, it
    it = 1
    call vamp_create_grid &
         (g, region, calls(2,1,1), num_div, &
         stratified, quadrupole, covariance = .true., exc = exc)
    call vamp_integrate_grid &
         (rng, g, func, prc_index, calls(:,:,1), num_div = num_div, &
         exc = exc, history = history(it:))
    if (present (exc)) then
       if (exc%level > EXC_WARN) then
          return
       end if
    end if
    it = it + sum (calls(1,:,1))
    last_step = size (calls, dim = 3)
    do step = 2, last_step - 1
       call diagonalize_real_symmetric (vamp_get_covariance(g), eval, evec)
       call sort (eval, evec)
       call select_rotation_axis (vamp_get_covariance(g), evec, pancake, cigar)
       call vamp_delete_grid_s (g)
       call vamp_create_grid &
            (g, region, calls(2,1,step), num_div, stratified, quadrupole, &
            covariance = .true., map = evec, exc = exc)
       call vamp_integrate_grid &
            (rng, g, func, prc_index, calls(:,:,step), num_div = num_div, &
            exc = exc, history = history(it:))
       if (present (exc)) then
          if (exc%level > EXC_WARN) then
             return
          end if
       end if
       it = it + sum (calls(1,:,step))
    end do
    call diagonalize_real_symmetric (vamp_get_covariance(g), eval, evec)
    call sort (eval, evec)
    call select_rotation_axis (vamp_get_covariance(g), evec, pancake, cigar)
    call vamp_delete_grid_s (g)
    call vamp_create_grid &
         (g, region, calls(2,1,last_step), num_div, stratified, quadrupole, &
         covariance = .true., map = evec, exc = exc)
    call vamp_integrate_grid &
         (rng, g, func, prc_index, calls(:,:,last_step), &
         integral, std_dev, avg_chi2, &
         num_div = num_div, exc = exc, history = history(it:))
    call vamp_delete_grid_s (g)
  end subroutine vamp_integratex_region
! *WK: Optional argument write_integrals
  subroutine write_grid_unit (g, unit, write_integrals)
    type(vamp_grid), intent(in) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    integer :: i, j
    write (unit = unit, fmt = descr_fmt) "begin type(vamp_grid) :: g"
    write (unit = unit, fmt = integer_fmt) "size (g%div) = ", size (g%div)
    write (unit = unit, fmt = integer_fmt) "num_calls = ", g%num_calls
    write (unit = unit, fmt = integer_fmt) "calls_per_cell = ", g%calls_per_cell
    write (unit = unit, fmt = logical_fmt) "stratified = ", g%stratified
    write (unit = unit, fmt = logical_fmt) "all_stratified = ", g%all_stratified
    write (unit = unit, fmt = logical_fmt) "quadrupole = ", g%quadrupole
    write (unit = unit, fmt = double_fmt) "mu(1) = ", g%mu(1)
    write (unit = unit, fmt = double_fmt) "mu(2) = ", g%mu(2)
    write (unit = unit, fmt = double_fmt) "sum_integral = ", g%sum_integral
    write (unit = unit, fmt = double_fmt) "sum_weights = ", g%sum_weights
    write (unit = unit, fmt = double_fmt) "sum_chi2 = ", g%sum_chi2
    write (unit = unit, fmt = double_fmt) "calls = ", g%calls
    write (unit = unit, fmt = double_fmt) "dv2g = ", g%dv2g
    write (unit = unit, fmt = double_fmt) "jacobi = ", g%jacobi
    write (unit = unit, fmt = double_fmt) "f_min = ", g%f_min
    write (unit = unit, fmt = double_fmt) "f_max = ", g%f_max
    write (unit = unit, fmt = double_fmt) "mu_gi = ", g%mu_gi
    write (unit = unit, fmt = double_fmt) "sum_mu_gi = ", g%sum_mu_gi
    write (unit = unit, fmt = descr_fmt) "begin g%num_div"
    do i = 1, size (g%div)
       write (unit = unit, fmt = integer_array_fmt) i, g%num_div(i)
    end do
    write (unit = unit, fmt = descr_fmt) "end g%num_div"
    write (unit = unit, fmt = descr_fmt) "begin g%div"
    do i = 1, size (g%div)
       call write_division (g%div(i), unit, write_integrals)
    end do
    write (unit = unit, fmt = descr_fmt) "end g%div"
    if (associated (g%map)) then
       write (unit = unit, fmt = descr_fmt) "begin g%map"
       do i = 1, size (g%div)
          do j = 1, size (g%div)
             write (unit = unit, fmt = double_array2_fmt) i, j, g%map(i,j)
          end do
       end do
       write (unit = unit, fmt = descr_fmt) "end g%map"
    else
       write (unit = unit, fmt = descr_fmt) "empty g%map"
    end if
    if (associated (g%mu_x)) then
       write (unit = unit, fmt = descr_fmt) "begin g%mu_x"
       do i = 1, size (g%div)
          write (unit = unit, fmt = double_array_fmt) i, g%mu_x(i)
          write (unit = unit, fmt = double_array_fmt) i, g%sum_mu_x(i)
          do j = 1, size (g%div)
             write (unit = unit, fmt = double_array2_fmt) i, j, g%mu_xx(i,j)
             write (unit = unit, fmt = double_array2_fmt) i, j, g%sum_mu_xx(i,j)
          end do
       end do
       write (unit = unit, fmt = descr_fmt) "end g%mu_x"
    else
       write (unit = unit, fmt = descr_fmt) "empty g%mu_x"
    end if
    write (unit = unit, fmt = descr_fmt) "end type(vamp_grid)"
  end subroutine write_grid_unit
! *WK: Optional argument read_integrals
  subroutine read_grid_unit (g, unit, read_integrals)
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    character(len=*), parameter :: FN = "vamp_read_grid"
    character(len=80) :: chdum
    integer :: ndim, i, j, idum, jdum
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = integer_fmt) chdum, ndim
    if (associated (g%div)) then
       if (size (g%div) /= ndim) then
          call delete_division (g%div)
          deallocate (g%div)
          allocate (g%div(ndim))
          call create_empty_division (g%div)
       end if
    else
       allocate (g%div(ndim))
       call create_empty_division (g%div)
    end if
    call create_array_pointer (g%num_div, ndim)
    read (unit = unit, fmt = integer_fmt) chdum, g%num_calls
    read (unit = unit, fmt = integer_fmt) chdum, g%calls_per_cell
    read (unit = unit, fmt = logical_fmt) chdum, g%stratified
    read (unit = unit, fmt = logical_fmt) chdum, g%all_stratified
    read (unit = unit, fmt = logical_fmt) chdum, g%quadrupole
    read (unit = unit, fmt = double_fmt) chdum, g%mu(1)
    read (unit = unit, fmt = double_fmt) chdum, g%mu(2)
    read (unit = unit, fmt = double_fmt) chdum, g%sum_integral
    read (unit = unit, fmt = double_fmt) chdum, g%sum_weights
    read (unit = unit, fmt = double_fmt) chdum, g%sum_chi2
    read (unit = unit, fmt = double_fmt) chdum, g%calls
    read (unit = unit, fmt = double_fmt) chdum, g%dv2g
    read (unit = unit, fmt = double_fmt) chdum, g%jacobi
    read (unit = unit, fmt = double_fmt) chdum, g%f_min
    read (unit = unit, fmt = double_fmt) chdum, g%f_max
    read (unit = unit, fmt = double_fmt) chdum, g%mu_gi
    read (unit = unit, fmt = double_fmt) chdum, g%sum_mu_gi
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 1, size (g%div)
       read (unit = unit, fmt = integer_array_fmt) idum, g%num_div(i)
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 1, size (g%div)
       call read_division (g%div(i), unit, read_integrals)
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
    if (chdum == "begin g%map") then
       call create_array_pointer (g%map, (/ ndim, ndim /))
       do i = 1, size (g%div)
          do j = 1, size (g%div)
             read (unit = unit, fmt = double_array2_fmt) idum, jdum, g%map(i,j)
          end do
       end do
       read (unit = unit, fmt = descr_fmt) chdum
    else
       if (associated (g%map)) then
          deallocate (g%map)
       end if
    end if
    read (unit = unit, fmt = descr_fmt) chdum
    if (chdum == "begin g%mu_x") then
       call create_array_pointer (g%mu_x, ndim )
       call create_array_pointer (g%sum_mu_x, ndim)
       call create_array_pointer (g%mu_xx, (/ ndim, ndim /))
       call create_array_pointer (g%sum_mu_xx, (/ ndim, ndim /))
       do i = 1, size (g%div)
          read (unit = unit, fmt = double_array_fmt) idum, jdum, g%mu_x(i)
          read (unit = unit, fmt = double_array_fmt) idum, jdum, g%sum_mu_x(i)
          do j = 1, size (g%div)
             read (unit = unit, fmt = double_array2_fmt) &
                  idum, jdum, g%mu_xx(i,j)
             read (unit = unit, fmt = double_array2_fmt) &
                  idum, jdum, g%sum_mu_xx(i,j)
          end do
       end do
       read (unit = unit, fmt = descr_fmt) chdum
    else
       if (associated (g%mu_x)) then
          deallocate (g%mu_x)
       end if
       if (associated (g%mu_xx)) then
          deallocate (g%mu_xx)
       end if
       if (associated (g%sum_mu_x)) then
          deallocate (g%sum_mu_x)
       end if
       if (associated (g%sum_mu_xx)) then
          deallocate (g%sum_mu_xx)
       end if
    end if
    read (unit = unit, fmt = descr_fmt) chdum
  end subroutine read_grid_unit
! *WK: Optional argument write_integrals
  subroutine write_grid_name (g, name, write_integrals)
    type(vamp_grid), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", file = name)
    call write_grid_unit (g, unit, write_integrals)
    close (unit = unit)
  end subroutine write_grid_name
! *WK: Optional argument read_integrals
  subroutine read_grid_name (g, name, read_integrals)
    type(vamp_grid), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", file = name)
    call read_grid_unit (g, unit, read_integrals)
    close (unit = unit)
  end subroutine read_grid_name
! *WK: Optional argument write_integrals
  subroutine write_grids_unit (g, unit, write_integrals)
    type(vamp_grids), intent(in) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    integer :: i
    write (unit = unit, fmt = descr_fmt) "begin type(vamp_grids) :: g"
    write (unit = unit, fmt = integer_fmt) "size (g%grids) = ", size (g%grids)
    write (unit = unit, fmt = double_fmt) "sum_integral = ", g%sum_integral
    write (unit = unit, fmt = double_fmt) "sum_weights = ", g%sum_weights
    write (unit = unit, fmt = double_fmt) "sum_chi2 = ", g%sum_chi2
    write (unit = unit, fmt = descr_fmt) "begin g%weights"
    do i = 1, size (g%grids)
       write (unit = unit, fmt = double_array_fmt) i, g%weights(i)
    end do
    write (unit = unit, fmt = descr_fmt) "end g%weights"
    write (unit = unit, fmt = descr_fmt) "begin g%num_calls"
    do i = 1, size (g%grids)
       write (unit = unit, fmt = integer_array_fmt) i, g%num_calls(i)
    end do
    write (unit = unit, fmt = descr_fmt) "end g%num_calls"
    write (unit = unit, fmt = descr_fmt) "begin g%grids"
    do i = 1, size (g%grids)
       call write_grid_unit (g%grids(i), unit, write_integrals)
    end do
    write (unit = unit, fmt = descr_fmt) "end g%grids"
    write (unit = unit, fmt = descr_fmt) "end type(vamp_grids)"
  end subroutine write_grids_unit
! *WK: Optional argument read_integrals
  subroutine read_grids_unit (g, unit, read_integrals)
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    character(len=*), parameter :: FN = "vamp_read_grids"
    character(len=80) :: chdum
    integer :: i, nch, idum
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = integer_fmt) chdum, nch
    if (associated (g%grids)) then
       if (size (g%grids) /= nch) then
          call vamp_delete_grid (g%grids)
          deallocate (g%grids, g%weights, g%num_calls)
          allocate (g%grids(nch), g%weights(nch), g%num_calls(nch))
          call vamp_create_empty_grid (g%grids)
       end if
    else
       allocate (g%grids(nch), g%weights(nch), g%num_calls(nch))
       call vamp_create_empty_grid (g%grids)
    end if
    read (unit = unit, fmt = double_fmt) chdum, g%sum_integral
    read (unit = unit, fmt = double_fmt) chdum, g%sum_weights
    read (unit = unit, fmt = double_fmt) chdum, g%sum_chi2
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 1, nch
       read (unit = unit, fmt = double_array_fmt) idum, g%weights(i)
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 1, nch
       read (unit = unit, fmt = integer_array_fmt) idum, g%num_calls(i)
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
    do i = 1, nch
       call read_grid_unit (g%grids(i), unit, read_integrals)
    end do
    read (unit = unit, fmt = descr_fmt) chdum
    read (unit = unit, fmt = descr_fmt) chdum
  end subroutine read_grids_unit
! *WK: Optional argument write_integrals
  subroutine write_grids_name (g, name, write_integrals)
    type(vamp_grids), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", file = name)
    call write_grids_unit (g, unit, write_integrals)
    close (unit = unit)
  end subroutine write_grids_name
! *WK: Optional argument read_integrals
  subroutine read_grids_name (g, name, read_integrals)
    type(vamp_grids), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", file = name)
    call read_grids_unit (g, unit, read_integrals)
    close (unit = unit)
  end subroutine read_grids_name
! *THO
  subroutine write_grid_raw_unit (g, unit, write_integrals)
    type(vamp_grid), intent(in) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    integer :: i, j
    write (unit = unit) MAGIC_GRID_BEGIN
    write (unit = unit) size (g%div)
    write (unit = unit) g%num_calls
    write (unit = unit) g%calls_per_cell
    write (unit = unit) g%stratified
    write (unit = unit) g%all_stratified
    write (unit = unit) g%quadrupole
    write (unit = unit) g%mu(1)
    write (unit = unit) g%mu(2)
    write (unit = unit) g%sum_integral
    write (unit = unit) g%sum_weights
    write (unit = unit) g%sum_chi2
    write (unit = unit) g%calls
    write (unit = unit) g%dv2g
    write (unit = unit) g%jacobi
    write (unit = unit) g%f_min
    write (unit = unit) g%f_max
    write (unit = unit) g%mu_gi
    write (unit = unit) g%sum_mu_gi
    do i = 1, size (g%div)
       write (unit = unit) g%num_div(i)
    end do
    do i = 1, size (g%div)
       call write_division_raw (g%div(i), unit, write_integrals)
    end do
    if (associated (g%map)) then
       write (unit = unit) MAGIC_GRID_MAP
       do i = 1, size (g%div)
          do j = 1, size (g%div)
             write (unit = unit) g%map(i,j)
          end do
       end do
    else
       write (unit = unit) MAGIC_GRID_EMPTY
    end if
    if (associated (g%mu_x)) then
       write (unit = unit) MAGIC_GRID_MU_X
       do i = 1, size (g%div)
          write (unit = unit) g%mu_x(i)
          write (unit = unit) g%sum_mu_x(i)
          do j = 1, size (g%div)
             write (unit = unit) g%mu_xx(i,j)
             write (unit = unit) g%sum_mu_xx(i,j)
          end do
       end do
    else
       write (unit = unit) MAGIC_GRID_EMPTY
    end if
    write (unit = unit) MAGIC_GRID_END
  end subroutine write_grid_raw_unit
  subroutine read_grid_raw_unit (g, unit, read_integrals)
    type(vamp_grid), intent(inout) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    character(len=*), parameter :: FN = "vamp_read_raw_grid"
    integer :: ndim, i, j, magic
    read (unit = unit) magic
    if (magic /= MAGIC_GRID_BEGIN) then
       print *, FN, " fatal: expecting magic ", MAGIC_GRID_BEGIN, &
                    ", found ", magic
       stop
    end if
    read (unit = unit) ndim
    if (associated (g%div)) then
       if (size (g%div) /= ndim) then
          call delete_division (g%div)
          deallocate (g%div)
          allocate (g%div(ndim))
          call create_empty_division (g%div)
       end if
    else
       allocate (g%div(ndim))
       call create_empty_division (g%div)
    end if
    call create_array_pointer (g%num_div, ndim)
    read (unit = unit) g%num_calls
    read (unit = unit) g%calls_per_cell
    read (unit = unit) g%stratified
    read (unit = unit) g%all_stratified
    read (unit = unit) g%quadrupole
    read (unit = unit) g%mu(1)
    read (unit = unit) g%mu(2)
    read (unit = unit) g%sum_integral
    read (unit = unit) g%sum_weights
    read (unit = unit) g%sum_chi2
    read (unit = unit) g%calls
    read (unit = unit) g%dv2g
    read (unit = unit) g%jacobi
    read (unit = unit) g%f_min
    read (unit = unit) g%f_max
    read (unit = unit) g%mu_gi
    read (unit = unit) g%sum_mu_gi
    do i = 1, size (g%div)
       read (unit = unit) g%num_div(i)
    end do
    do i = 1, size (g%div)
       call read_division_raw (g%div(i), unit, read_integrals)
    end do
    read (unit = unit) magic
    if (magic == MAGIC_GRID_MAP) then
       call create_array_pointer (g%map, (/ ndim, ndim /))
       do i = 1, size (g%div)
          do j = 1, size (g%div)
             read (unit = unit) g%map(i,j)
          end do
       end do
    else if (magic == MAGIC_GRID_EMPTY) then
       if (associated (g%map)) then
          deallocate (g%map)
       end if
    else
       print *, FN, " fatal: expecting magic ", MAGIC_GRID_EMPTY, &
                    " or ", MAGIC_GRID_MAP, ", found ", magic
       stop
    end if
    read (unit = unit) magic
    if (magic == MAGIC_GRID_MU_X) then
       call create_array_pointer (g%mu_x, ndim )
       call create_array_pointer (g%sum_mu_x, ndim)
       call create_array_pointer (g%mu_xx, (/ ndim, ndim /))
       call create_array_pointer (g%sum_mu_xx, (/ ndim, ndim /))
       do i = 1, size (g%div)
          read (unit = unit) g%mu_x(i)
          read (unit = unit) g%sum_mu_x(i)
          do j = 1, size (g%div)
             read (unit = unit) g%mu_xx(i,j)
             read (unit = unit) g%sum_mu_xx(i,j)
          end do
       end do
    else if (magic == MAGIC_GRID_EMPTY) then
       if (associated (g%mu_x)) then
          deallocate (g%mu_x)
       end if
       if (associated (g%mu_xx)) then
          deallocate (g%mu_xx)
       end if
       if (associated (g%sum_mu_x)) then
          deallocate (g%sum_mu_x)
       end if
       if (associated (g%sum_mu_xx)) then
          deallocate (g%sum_mu_xx)
       end if
    else 
       print *, FN, " fatal: expecting magic ", MAGIC_GRID_EMPTY, &
                    " or ", MAGIC_GRID_MU_X, ", found ", magic
       stop
    end if
    read (unit = unit) magic
    if (magic /= MAGIC_GRID_END) then
       print *, FN, " fatal: expecting magic ", MAGIC_GRID_END, &
                    " found ", magic
       stop
    end if
  end subroutine read_grid_raw_unit
  subroutine write_grid_raw_name (g, name, write_integrals)
    type(vamp_grid), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", &
          form = "unformatted", file = name)
    call write_grid_raw_unit (g, unit, write_integrals)
    close (unit = unit)
  end subroutine write_grid_raw_name
  subroutine read_grid_raw_name (g, name, read_integrals)
    type(vamp_grid), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", &
          form = "unformatted",  file = name)
    call read_grid_raw_unit (g, unit, read_integrals)
    close (unit = unit)
  end subroutine read_grid_raw_name
  subroutine write_grids_raw_unit (g, unit, write_integrals)
    type(vamp_grids), intent(in) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: write_integrals
    integer :: i
    write (unit = unit) MAGIC_GRIDS_BEGIN
    write (unit = unit) size (g%grids)
    write (unit = unit) g%sum_integral
    write (unit = unit) g%sum_weights
    write (unit = unit) g%sum_chi2
    do i = 1, size (g%grids)
       write (unit = unit) g%weights(i)
    end do
    do i = 1, size (g%grids)
       write (unit = unit) g%num_calls(i)
    end do
    do i = 1, size (g%grids)
       call write_grid_raw_unit (g%grids(i), unit, write_integrals)
    end do
    write (unit = unit) MAGIC_GRIDS_END
  end subroutine write_grids_raw_unit
  subroutine read_grids_raw_unit (g, unit, read_integrals)
    type(vamp_grids), intent(inout) :: g
    integer, intent(in) :: unit
    logical, intent(in), optional :: read_integrals
    character(len=*), parameter :: FN = "vamp_read_grids_raw"
    integer :: i, nch, magic
    read (unit = unit) magic
    if (magic /= MAGIC_GRIDS_BEGIN) then
       print *, FN, " fatal: expecting magic ", MAGIC_GRIDS_BEGIN, &
                    " found ", magic
       stop
    end if
    read (unit = unit) nch
    if (associated (g%grids)) then
       if (size (g%grids) /= nch) then
          call vamp_delete_grid (g%grids)
          deallocate (g%grids, g%weights, g%num_calls)
          allocate (g%grids(nch), g%weights(nch), g%num_calls(nch))
          call vamp_create_empty_grid (g%grids)
       end if
    else
       allocate (g%grids(nch), g%weights(nch), g%num_calls(nch))
       call vamp_create_empty_grid (g%grids)
    end if
    read (unit = unit) g%sum_integral
    read (unit = unit) g%sum_weights
    read (unit = unit) g%sum_chi2
    do i = 1, nch
       read (unit = unit) g%weights(i)
    end do
    do i = 1, nch
       read (unit = unit) g%num_calls(i)
    end do
    do i = 1, nch
       call read_grid_raw_unit (g%grids(i), unit, read_integrals)
    end do
    read (unit = unit) magic
    if (magic /= MAGIC_GRIDS_END) then
       print *, FN, " fatal: expecting magic ", MAGIC_GRIDS_END, &
                    " found ", magic
       stop
    end if
  end subroutine read_grids_raw_unit
  subroutine write_grids_raw_name (g, name, write_integrals)
    type(vamp_grids), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: write_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "write", status = "replace", &
          form = "unformatted", file = name)
    call write_grids_raw_unit (g, unit, write_integrals)
    close (unit = unit)
  end subroutine write_grids_raw_name
  subroutine read_grids_raw_name (g, name, read_integrals)
    type(vamp_grids), intent(inout) :: g
    character(len=*), intent(in) :: name
    logical, intent(in), optional :: read_integrals
    integer :: unit
    call find_free_unit (unit)
    open (unit = unit, action = "read", status = "old", &
          form = "unformatted", file = name)
    call read_grids_raw_unit (g, unit, read_integrals)
    close (unit = unit)
  end subroutine read_grids_raw_name
!
  subroutine vamp_marshal_grid (g, ibuf, dbuf)
    type(vamp_grid), intent(in) :: g
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=default), dimension(:), intent(inout) :: dbuf
    integer :: i, iwords, dwords, iidx, didx, ndim
    ndim = size (g%div)
    ibuf(1) = g%num_calls
    ibuf(2) = g%calls_per_cell
    ibuf(3) = ndim
    if (g%stratified) then
       ibuf(4) = 1
    else
       ibuf(4) = 0
    end if
    if (g%all_stratified) then
       ibuf(5) = 1
    else
       ibuf(5) = 0
    end if
    if (g%quadrupole) then
       ibuf(6) = 1
    else
       ibuf(6) = 0
    end if
    dbuf(1:2) = g%mu
    dbuf(3) = g%sum_integral
    dbuf(4) = g%sum_weights
    dbuf(5) = g%sum_chi2
    dbuf(6) = g%calls
    dbuf(7) = g%dv2g
    dbuf(8) = g%jacobi
    dbuf(9) = g%f_min
    dbuf(10) = g%f_max
    dbuf(11) = g%mu_gi
    dbuf(12) = g%sum_mu_gi
    ibuf(7:6+ndim) = g%num_div
    iidx = 7 + ndim
    didx = 13
    do i = 1, ndim
       call marshal_division_size (g%div(i), iwords, dwords)
       ibuf(iidx) = iwords
       ibuf(iidx+1) = dwords
       iidx = iidx + 2
       call marshal_division (g%div(i), ibuf(iidx:iidx-1+iwords), &
            dbuf(didx:didx-1+dwords))
       iidx = iidx + iwords
       didx = didx + dwords
    end do
    if (associated (g%map)) then
       ibuf(iidx) = 1
       dbuf(didx:didx-1+ndim**2) = reshape (g%map, (/ ndim**2 /))
       didx = didx + ndim**2
    else
       ibuf(iidx) = 0
    end if
    iidx = iidx + 1
    if (associated (g%mu_x)) then
       ibuf(iidx) = 1
       dbuf(didx:didx-1+ndim) = g%mu_x
       didx = didx + ndim
       dbuf(didx:didx-1+ndim) = g%sum_mu_x
       didx = didx + ndim
       dbuf(didx:didx-1+ndim**2) = reshape (g%mu_xx, (/ ndim**2 /))
       didx = didx + ndim**2
       dbuf(didx:didx-1+ndim**2) = reshape (g%sum_mu_xx, (/ ndim**2 /))
       didx = didx + ndim**2
    else
       ibuf(iidx) = 0
    end if
    iidx = iidx + 1
  end subroutine vamp_marshal_grid
  subroutine vamp_marshal_grid_size (g, iwords, dwords)
    type(vamp_grid), intent(in) :: g
    integer, intent(out) :: iwords, dwords
    integer :: i, ndim, iw, dw
    ndim = size (g%div)
    iwords = 6 + ndim
    dwords = 12
    do i = 1, ndim
       call marshal_division_size (g%div(i), iw, dw)
       iwords = iwords + 2 + iw
       dwords = dwords + dw
    end do
    iwords = iwords + 1
    if (associated (g%map)) then
       dwords = dwords + ndim**2
    end if
    iwords = iwords + 1
    if (associated (g%mu_x)) then
       dwords = dwords + 2 * (ndim + ndim**2)
    end if
  end subroutine vamp_marshal_grid_size
  subroutine vamp_unmarshal_grid (g, ibuf, dbuf)
    type(vamp_grid), intent(inout) :: g
    integer, dimension(:), intent(in) :: ibuf
    real(kind=default), dimension(:), intent(in) :: dbuf
    integer :: i, iwords, dwords, iidx, didx, ndim
    g%num_calls = ibuf(1)
    g%calls_per_cell = ibuf(2)
    ndim = ibuf(3)
    g%stratified = ibuf(4) /= 0
    g%all_stratified = ibuf(5) /= 0
    g%quadrupole = ibuf(6) /= 0
    g%mu = dbuf(1:2)
    g%sum_integral = dbuf(3)
    g%sum_weights = dbuf(4)
    g%sum_chi2 = dbuf(5)
    g%calls = dbuf(6)
    g%dv2g = dbuf(7)
    g%jacobi = dbuf(8)
    g%f_min = dbuf(9)
    g%f_max = dbuf(10)
    g%mu_gi = dbuf(11)
    g%sum_mu_gi = dbuf(12)
    call copy_array_pointer (g%num_div, ibuf(7:6+ndim))
    if (associated (g%div)) then
       if (size (g%div) /= ndim) then
          call delete_division (g%div)
          deallocate (g%div)
          allocate (g%div(ndim))
          call create_empty_division (g%div)
       end if
    else
       allocate (g%div(ndim))
       call create_empty_division (g%div)
    end if
    iidx = 7 + ndim
    didx = 13
    do i = 1, ndim
       iwords = ibuf(iidx)
       dwords = ibuf(iidx+1)
       iidx = iidx + 2
       call unmarshal_division (g%div(i), ibuf(iidx:iidx-1+iwords), &
            dbuf(didx:didx-1+dwords))
       iidx = iidx + iwords
       didx = didx + dwords
    end do
    if (ibuf(iidx) > 0) then
       call copy_array_pointer &
            (g%map, reshape (dbuf(didx:didx-1+ibuf(iidx)), (/ ndim, ndim /)))
       didx = didx + ibuf(iidx)
    else
       if (associated (g%map)) then
          deallocate (g%map)
       end if
    end if
    iidx = iidx + 1
    if (ibuf(iidx) > 0) then
       call copy_array_pointer (g%mu_x, dbuf(didx:didx-1+ndim))
       didx = didx + ndim
       call copy_array_pointer (g%sum_mu_x, dbuf(didx:didx-1+ndim))
       didx = didx + ndim
       call copy_array_pointer &
            (g%mu_xx, reshape (dbuf(didx:didx-1+ndim**2), (/ ndim, ndim /)))
       didx = didx + ndim**2
       call copy_array_pointer &
            (g%sum_mu_xx, reshape (dbuf(didx:didx-1+ndim**2), (/ ndim, ndim /)))
       didx = didx + ndim**2
    else
       if (associated (g%mu_x)) then
          deallocate (g%mu_x)
       end if
       if (associated (g%mu_xx)) then
          deallocate (g%mu_xx)
       end if
       if (associated (g%sum_mu_x)) then
          deallocate (g%sum_mu_x)
       end if
       if (associated (g%sum_mu_xx)) then
          deallocate (g%sum_mu_xx)
       end if
    end if
    iidx = iidx + 1
  end subroutine vamp_unmarshal_grid
  subroutine vamp_marshal_history (h, ibuf, dbuf)
    type(vamp_history), intent(in) :: h
    integer, dimension(:), intent(inout) :: ibuf
    real(kind=default), dimension(:), intent(inout) :: dbuf
    integer :: j, ndim, iidx, didx, iwords, dwords
    if (h%verbose .and. (associated (h%div))) then
       ndim = size (h%div)
    else
       ndim = 0
    end if
    ibuf(1) = ndim
    ibuf(2) = h%calls
    if (h%stratified) then
       ibuf(3) = 1
    else
       ibuf(3) = 0
    end if
    dbuf(1) = h%integral
    dbuf(2) = h%std_dev
    dbuf(3) = h%avg_integral
    dbuf(4) = h%avg_std_dev
    dbuf(5) = h%avg_chi2
    dbuf(6) = h%f_min
    dbuf(7) = h%f_max
    iidx = 4
    didx = 8
    do j = 1, ndim
       call marshal_div_history_size (h%div(j), iwords, dwords)
       ibuf(iidx) = iwords
       ibuf(iidx+1) = dwords
       iidx = iidx + 2
       call marshal_div_history (h%div(j), ibuf(iidx:iidx-1+iwords), &
            dbuf(didx:didx-1+dwords))
       iidx = iidx + iwords
       didx = didx + dwords
    end do
  end subroutine vamp_marshal_history
  subroutine vamp_marshal_history_size (h, iwords, dwords)
    type(vamp_history), intent(in) :: h
    integer, intent(out) :: iwords, dwords
    integer :: i, ndim, iw, dw
    if (h%verbose .and. (associated (h%div))) then
       ndim = size (h%div)
    else
       ndim = 0
    end if
    iwords = 3
    dwords = 7
    do i = 1, ndim
       call marshal_div_history_size (h%div(i), iw, dw)
       iwords = iwords + 2 + iw
       dwords = dwords + dw
    end do
  end subroutine vamp_marshal_history_size
  subroutine vamp_unmarshal_history (h, ibuf, dbuf)
    type(vamp_history), intent(inout) :: h
    integer, dimension(:), intent(in) :: ibuf
    real(kind=default), dimension(:), intent(in) :: dbuf
    integer :: j, ndim, iidx, didx, iwords, dwords
    ndim = ibuf(1)
    h%calls = ibuf(2)
    h%stratified = ibuf(3) /= 0
    h%integral = dbuf(1)
    h%std_dev = dbuf(2)
    h%avg_integral = dbuf(3)
    h%avg_std_dev = dbuf(4)
    h%avg_chi2 = dbuf(5)
    h%f_min = dbuf(6)
    h%f_max = dbuf(7)
    if (ndim > 0) then
       if (associated (h%div)) then
          if (size (h%div) /= ndim) then
             deallocate (h%div)
             allocate (h%div(ndim))
          end if
       else
          allocate (h%div(ndim))
       end if
       iidx = 4
       didx = 8
       do j = 1, ndim
          iwords = ibuf(iidx)
          dwords = ibuf(iidx+1)
          iidx = iidx + 2
          call unmarshal_div_history (h%div(j), ibuf(iidx:iidx-1+iwords), &
               dbuf(didx:didx-1+dwords))
          iidx = iidx + iwords
          didx = didx + dwords
       end do
    end if
  end subroutine vamp_unmarshal_history
  subroutine vamp_copy_grid_s (lhs, rhs)
    type(vamp_grid), intent(inout) :: lhs
    type(vamp_grid), intent(in) :: rhs
    integer :: ndim
    ndim = size (rhs%div)
    lhs%mu = rhs%mu
    lhs%sum_integral = rhs%sum_integral
    lhs%sum_weights = rhs%sum_weights
    lhs%sum_chi2 = rhs%sum_chi2
    lhs%calls = rhs%calls
    lhs%num_calls = rhs%num_calls
    call copy_array_pointer (lhs%num_div, rhs%num_div)
    lhs%dv2g = rhs%dv2g
    lhs%jacobi = rhs%jacobi
    lhs%f_min = rhs%f_min
    lhs%f_max = rhs%f_max
    lhs%mu_gi = rhs%mu_gi
    lhs%sum_mu_gi = rhs%sum_mu_gi
    lhs%calls_per_cell = rhs%calls_per_cell
    lhs%stratified = rhs%stratified
    lhs%all_stratified = rhs%all_stratified
    lhs%quadrupole = rhs%quadrupole
    if (associated (lhs%div)) then
       if (size (lhs%div) /= ndim) then
          call delete_division (lhs%div)
          deallocate (lhs%div)
          allocate (lhs%div(ndim))
       end if
    else
       allocate (lhs%div(ndim))
    end if
    call copy_division (lhs%div, rhs%div)
    if (associated (rhs%map)) then
       call copy_array_pointer (lhs%map, rhs%map)
    else if (associated (lhs%map)) then
       deallocate (lhs%map)
    end if
    if (associated (rhs%mu_x)) then
       call copy_array_pointer (lhs%mu_x, rhs%mu_x)
       call copy_array_pointer (lhs%mu_xx, rhs%mu_xx)
       call copy_array_pointer (lhs%sum_mu_x, rhs%sum_mu_x)
       call copy_array_pointer (lhs%sum_mu_xx, rhs%sum_mu_xx)
    else if (associated (lhs%mu_x)) then
       deallocate (lhs%mu_x, lhs%mu_xx, lhs%sum_mu_x, lhs%sum_mu_xx)
    end if
  end subroutine vamp_copy_grid_s
  subroutine vamp_delete_grid_s (g)
    type(vamp_grid), intent(inout) :: g
    if (associated (g%div)) then
       call delete_division (g%div)
       deallocate (g%div, g%num_div)
    end if
    if (associated (g%map)) then
       deallocate (g%map)
    end if
    if (associated (g%mu_x)) then
       deallocate (g%mu_x, g%mu_xx, g%sum_mu_x, g%sum_mu_xx)
    end if
  end subroutine vamp_delete_grid_s
  subroutine vamp_copy_grids_s (lhs, rhs)
    type(vamp_grids), intent(inout) :: lhs
    type(vamp_grids), intent(in) :: rhs
    integer :: nch
    nch = size (rhs%grids)
    lhs%sum_integral = rhs%sum_integral
    lhs%sum_chi2 = rhs%sum_chi2
    lhs%sum_weights = rhs%sum_weights
    if (associated (lhs%grids)) then
       if (size (lhs%grids) /= nch) then
          deallocate (lhs%grids)
          allocate (lhs%grids(nch))
          call vamp_create_empty_grid_s (lhs%grids(nch))
       end if
    else
       allocate (lhs%grids(nch))
       call vamp_create_empty_grid_s (lhs%grids(nch))
    end if
    call vamp_copy_grid (lhs%grids, rhs%grids)
    call copy_array_pointer (lhs%weights, rhs%weights)
    call copy_array_pointer (lhs%num_calls, rhs%num_calls)
  end subroutine vamp_copy_grids_s
  subroutine vamp_delete_grids_s (g)
    type(vamp_grids), intent(inout) :: g
    if (associated (g%grids)) then
       call vamp_delete_grid (g%grids)
       deallocate (g%weights, g%grids, g%num_calls)
    end if
  end subroutine vamp_delete_grids_s
  subroutine vamp_copy_history_s (lhs, rhs)
    type(vamp_history), intent(inout) :: lhs
    type(vamp_history), intent(in) :: rhs
    lhs%calls = rhs%calls
    lhs%stratified = rhs%stratified
    lhs%verbose = rhs%verbose
    lhs%integral = rhs%integral
    lhs%std_dev = rhs%std_dev
    lhs%avg_integral = rhs%avg_integral
    lhs%avg_std_dev = rhs%avg_std_dev
    lhs%avg_chi2 = rhs%avg_chi2
    lhs%f_min = rhs%f_min
    lhs%f_max = rhs%f_max
    if (rhs%verbose) then
       if (associated (lhs%div)) then
          if (size (lhs%div) /= size (rhs%div)) then
             deallocate (lhs%div)
             allocate (lhs%div(size(rhs%div)))
          end if
       else
          allocate (lhs%div(size(rhs%div)))
       end if
       call copy_history (lhs%div, rhs%div)
    end if
  end subroutine vamp_copy_history_s
  subroutine vamp_delete_history_s (h)
    type(vamp_history), intent(inout) :: h
    if (associated (h%div)) then
       deallocate (h%div)
    end if
  end subroutine vamp_delete_history_s
end module vamp_rest
module vamp
  use vamp_grid_type  !NODEP!
  use vamp_rest       !NODEP!
! *WK reference to equivalence module needed here
  use vamp_equivalences !NODEP!
! *WK
  public
end module vamp
! ! constants.f90 --
! ! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! ! 
! ! VAMP is free software; you can redistribute it and/or modify it
! ! under the terms of the GNU General Public License as published by 
! ! the Free Software Foundation; either version 2, or (at your option)
! ! any later version.
! ! 
! ! VAMP is distributed in the hope that it will be useful, but
! ! WITHOUT ANY WARRANTY; without even the implied warranty of
! ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! ! GNU General Public License for more details.
! ! 
! ! You should have received a copy of the GNU General Public License
! ! along with this program; if not, write to the Free Software
! ! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! This version of the source code of vamp has no comments and
! ! can be hard to understand, modify, and improve.  You should have
! ! received a copy of the literate noweb sources of vamp that
! ! contain the documentation in full detail.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! module constants
!   use kinds
!   implicit none

!   real(kind=double), public, parameter :: &
!        PI = 3.1415926535897932384626433832795028841972_double
!   character(len=*), public, parameter :: CONSTANTS_RCS_ID = &
!        "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
! end module constants
! specfun.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module specfun
  use kinds
!  use constants
  implicit none

  public :: gamma
  character(len=*), public, parameter :: SPECFUN_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
  real(kind=default), public, parameter :: &
       PI = 3.1415926535897932384626433832795028841972_default
contains
  function gamma (x) result (g)
    real(kind=default), intent(in) :: x
    real(kind=default) :: g
    integer :: i
    real(kind=default) :: u, f, alpha, b0, b1, b2
    real(kind=default), dimension(0:15), parameter :: &
         c = (/ 3.65738772508338244_default, &
         1.95754345666126827_default, &
         0.33829711382616039_default, &
         0.04208951276557549_default, &
         0.00428765048212909_default, &
         0.00036521216929462_default, &
         0.00002740064222642_default, &
         0.00000181240233365_default, &
         0.00000010965775866_default, &
         0.00000000598718405_default, &
         0.00000000030769081_default, &
         0.00000000001431793_default, &
         0.00000000000065109_default, &
         0.00000000000002596_default, &
         0.00000000000000111_default, &
         0.00000000000000004_default /)

    u = x
    if (u <= 0.0) then
       if (u == int (u)) then
          g = huge (g)
          return
       else
          u = 1 - u
       end if
    endif
    f = 1
    if (u < 3) then
       do i = 1, int (4 - u)
          f = f / u
          u = u + 1
       end do
    else
       do i = 1, int (u - 3)
          u = u - 1
          f = f * u
       end do
    end if
    g = 2*u - 7
    alpha = 2*g
    b1 = 0
    b2 = 0
    do i = 15, 0, -1
       b0 = c(i) + alpha * b1 - b2
       b2 = b1
       b1 = b0
    end do
    g = f * (b0 - g * b2)
    if (x < 0) then
       g = PI / (sin (PI * x) * g)
    end if
  end function gamma
end module specfun
! histograms.f90 --
! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! 
! VAMP is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! VAMP is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This version of the source code of vamp has no comments and
! can be hard to understand, modify, and improve.  You should have
! received a copy of the literate noweb sources of vamp that
! contain the documentation in full detail.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! WK: modified to introduce write_histogram1_unit
!
module histograms
  use kinds
  use utils, only: find_free_unit
  implicit none

  public :: create_histogram
  public :: fill_histogram
  public :: delete_histogram
  public :: write_histogram
  private :: create_histogram1, create_histogram2
  private :: fill_histogram1, fill_histogram2s, fill_histogram2v
  private :: delete_histogram1, delete_histogram2
  private :: write_histogram1, write_histogram2
  private :: midpoint
  private :: midpoint1, midpoint2
  interface create_histogram
     module procedure create_histogram1, create_histogram2
  end interface
  interface fill_histogram
     module procedure fill_histogram1, fill_histogram2s, fill_histogram2v
  end interface
  interface delete_histogram
     module procedure delete_histogram1, delete_histogram2
  end interface
  interface write_histogram
     module procedure write_histogram1, write_histogram2
! WK
     module procedure write_histogram1_unit
!
  end interface
  interface midpoint
     module procedure midpoint1, midpoint2
  end interface
  integer, parameter, private :: N_BINS_DEFAULT = 10
  type, public :: histogram

     integer :: n_bins
     real(kind=default) :: x_min, x_max
     real(kind=default), dimension(:), pointer :: bins, bins2
! WK
     real(kind=default), dimension(:), pointer :: bins3
!
  end type histogram
  type, public :: histogram2

     integer, dimension(2) :: n_bins
     real(kind=default), dimension(2) :: x_min, x_max
     real(kind=default), dimension(:,:), pointer :: bins, bins2
  end type histogram2
  character(len=*), public, parameter :: HISTOGRAMS_RCS_ID = &
       "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
contains
  subroutine create_histogram1 (h, x_min, x_max, nb)
    type(histogram), intent(out) :: h
    real(kind=default), intent(in) :: x_min, x_max
    integer, intent(in), optional :: nb
    if (present (nb)) then
       h%n_bins = nb
    else
       h%n_bins = N_BINS_DEFAULT
    end if
    h%x_min = x_min
    h%x_max = x_max
    allocate (h%bins(0:h%n_bins+1), h%bins2(0:h%n_bins+1))
    h%bins = 0
    h%bins2 = 0
! WK
    allocate (h%bins3(0:h%n_bins+1))
    h%bins3 = 0
!
  end subroutine create_histogram1
  subroutine create_histogram2 (h, x_min, x_max, nb)
    type(histogram2), intent(out) :: h
    real(kind=default), dimension(:), intent(in) :: x_min, x_max
    integer, intent(in), dimension(:), optional :: nb
    if (present (nb)) then
       h%n_bins = nb
    else
       h%n_bins = N_BINS_DEFAULT
    end if
    h%x_min = x_min
    h%x_max = x_max
    allocate (h%bins(0:h%n_bins(1)+1,0:h%n_bins(1)+1), &
         h%bins2(0:h%n_bins(2)+1,0:h%n_bins(2)+1))
    h%bins = 0
    h%bins2 = 0
  end subroutine create_histogram2
!  subroutine fill_histogram1 (h, x, weight)
! WK
  subroutine fill_histogram1 (h, x, weight, excess)
!
    type(histogram), intent(inout) :: h
    real(kind=default), intent(in) :: x
    real(kind=default), intent(in), optional :: weight
! WK
    real(kind=default), intent(in), optional :: excess
!
    integer :: i
    if (x < h%x_min) then
       i = 0
    else if (x > h%x_max) then
       i = h%n_bins + 1
    else
       i = 1 + h%n_bins * (x - h%x_min) / (h%x_max - h%x_min)
    end if
    if (present (weight)) then
       h%bins(i) = h%bins(i) + weight
       h%bins2(i) = h%bins2(i) + weight*weight
    else
       h%bins(i) = h%bins(i) + 1
       h%bins2(i) = h%bins2(i) + 1
    end if
! WK
    if (present (excess)) h%bins3(i) = h%bins3(i) + excess
!
  end subroutine fill_histogram1
  subroutine fill_histogram2s (h, x1, x2, weight)
    type(histogram2), intent(inout) :: h
    real(kind=default), intent(in) :: x1, x2
    real(kind=default), intent(in), optional :: weight
    call fill_histogram2v (h, (/ x1, x2 /), weight)
  end subroutine fill_histogram2s
  subroutine fill_histogram2v (h, x, weight)
    type(histogram2), intent(inout) :: h
    real(kind=default), dimension(:), intent(in) :: x
    real(kind=default), intent(in), optional :: weight
    integer, dimension(2) :: i
    i = 1 + h%n_bins * (x - h%x_min) / (h%x_max - h%x_min)
    i = min (max (i, 0), h%n_bins + 1)
    if (present (weight)) then
       h%bins(i(1),i(2)) = h%bins(i(1),i(2)) + weight
       h%bins2(i(1),i(2)) = h%bins2(i(1),i(2)) + weight*weight
    else
       h%bins(i(1),i(2)) = h%bins(i(1),i(2)) + 1
       h%bins2(i(1),i(2)) = h%bins2(i(1),i(2)) + 1
    end if
  end subroutine fill_histogram2v
  subroutine delete_histogram1 (h)
    type(histogram), intent(inout) :: h
    deallocate (h%bins, h%bins2)
! WK
    deallocate (h%bins3)
!
  end subroutine delete_histogram1
  subroutine delete_histogram2 (h)
    type(histogram2), intent(inout) :: h
    deallocate (h%bins, h%bins2)
  end subroutine delete_histogram2
  subroutine write_histogram1 (h, name, over)
    type(histogram), intent(in) :: h
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: over
    integer :: i, iounit
    if (present (name)) then
       call find_free_unit (iounit)
       if (iounit > 0) then
          open (unit = iounit, action = "write", status = "replace", &
               file = name)
          if (present (over)) then
             if (over) then
                write (unit = iounit, fmt = *) &
                     "underflow", h%bins(0), sqrt (h%bins2(0))
             end if
          end if
          do i = 1, h%n_bins
             write (unit = iounit, fmt = *) &
                  midpoint (h, i), h%bins(i), sqrt (h%bins2(i))
          end do
          if (present (over)) then
             if (over) then
                write (unit = iounit, fmt = *) &
                     "overflow", h%bins(h%n_bins+1), &
                     sqrt (h%bins2(h%n_bins+1))
             end if
          end if
          close (unit = iounit)
       else
          print *, "write_histogram: Can't find a free unit!"
       end if
    else
       if (present (over)) then
          if (over) then
             print *, "underflow", h%bins(0), sqrt (h%bins2(0))
          end if
       end if
       do i = 1, h%n_bins
          print *, midpoint (h, i), h%bins(i), sqrt (h%bins2(i))
       end do
       if (present (over)) then
          if (over) then
             print *, "overflow", h%bins(h%n_bins+1), &
                  sqrt (h%bins2(h%n_bins+1))
          end if
       end if
    end if
  end subroutine write_histogram1
! WK
  subroutine write_histogram1_unit (h, iounit, over, show_excess)
    type(histogram), intent(in) :: h
    integer, intent(in) :: iounit
    logical, intent(in), optional :: over, show_excess
    integer :: i
    logical :: show_exc
    show_exc = .false.; if (present(show_excess)) show_exc = show_excess
    if (present (over)) then
       if (over) then
          if (show_exc) then
             write (unit = iounit, fmt = 1) &
                  "underflow", h%bins(0), sqrt (h%bins2(0)), h%bins3(0)
          else
             write (unit = iounit, fmt = 1) &
                  "underflow", h%bins(0), sqrt (h%bins2(0))
          end if
       end if
    end if
    do i = 1, h%n_bins
       if (show_exc) then
          write (unit = iounit, fmt = 1) &
               midpoint (h, i), h%bins(i), sqrt (h%bins2(i)), h%bins3(i)
       else
          write (unit = iounit, fmt = 1) &
               midpoint (h, i), h%bins(i), sqrt (h%bins2(i))
       end if
    end do
    if (present (over)) then
       if (over) then
          if (show_exc) then
             write (unit = iounit, fmt = 1) &
                  "overflow", h%bins(h%n_bins+1), &
                  sqrt (h%bins2(h%n_bins+1)), &
                  h%bins3(h%n_bins+1)
          else
             write (unit = iounit, fmt = 1) &
                  "overflow", h%bins(h%n_bins+1), &
                  sqrt (h%bins2(h%n_bins+1))
          end if
       end if
    end if
1   format(1x,4(G16.9,2x))
  end subroutine write_histogram1_unit
!
  function midpoint1 (h, bin) result (x)
    type(histogram), intent(in) :: h
    integer, intent(in) :: bin
    real(kind=default) :: x
    x = h%x_min + (h%x_max - h%x_min) * (bin - 0.5) / h%n_bins
  end function midpoint1
  function midpoint2 (h, bin, d) result (x)
    type(histogram2), intent(in) :: h
    integer, intent(in) :: bin, d
    real(kind=default) :: x
    x = h%x_min(d) + (h%x_max(d) - h%x_min(d)) * (bin - 0.5) / h%n_bins(d)
  end function midpoint2
  subroutine write_histogram2 (h, name, over)
    type(histogram2), intent(in) :: h
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: over
    integer :: i1, i2, iounit
    if (present (name)) then
       call find_free_unit (iounit)
       if (iounit > 0) then
          open (unit = iounit, action = "write", status = "replace", &
               file = name)
          if (present (over)) then
             if (over) then
                write (unit = iounit, fmt = *) &
                     "double underflow", h%bins(0,0), sqrt (h%bins2(0,0))
                do i2 = 1, h%n_bins(2)
                   write (unit = iounit, fmt = *) &
                        "x1 underflow", midpoint (h, i2, 2), &
                        h%bins(0,i2), sqrt (h%bins2(0,i2))
                end do
                do i1 = 1, h%n_bins(1)
                   write (unit = iounit, fmt = *) &
                        "x2 underflow", midpoint (h, i1, 1), &
                        h%bins(i1,0), sqrt (h%bins2(i1,0))
                end do
             end if
          end if
          do i1 = 1, h%n_bins(1)
             do i2 = 1, h%n_bins(2)
                write (unit = iounit, fmt = *) &
                     midpoint (h, i1, 1), midpoint (h, i2, 2), &
                     h%bins(i1,i2), sqrt (h%bins2(i1,i2))
             end do
          end do
          if (present (over)) then
             if (over) then
                do i2 = 1, h%n_bins(2)
                   write (unit = iounit, fmt = *) &
                        "x1 overflow", midpoint (h, i2, 2), &
                        h%bins(h%n_bins(1)+1,i2), &
                        sqrt (h%bins2(h%n_bins(1)+1,i2))
                end do
                do i1 = 1, h%n_bins(1)
                   write (unit = iounit, fmt = *) &
                        "x2 overflow", midpoint (h, i1, 1), &
                        h%bins(i1,h%n_bins(2)+1), &
                        sqrt (h%bins2(i1,h%n_bins(2)+1))
                end do
                write (unit = iounit, fmt = *) "double overflow", &
                     h%bins(h%n_bins(1)+1,h%n_bins(2)+1), &
                     sqrt (h%bins2(h%n_bins(1)+1,h%n_bins(2)+1))
             end if
          end if
          close (unit = iounit)
       else
          print *, "write_histogram: Can't find a free unit!"
       end if
    else
       if (present (over)) then
          if (over) then
             print *, "double underflow", h%bins(0,0), sqrt (h%bins2(0,0))
             do i2 = 1, h%n_bins(2)
                print *, "x1 underflow", midpoint (h, i2, 2), &
                     h%bins(0,i2), sqrt (h%bins2(0,i2))
             end do
             do i1 = 1, h%n_bins(1)
                print *, "x2 underflow", midpoint (h, i1, 1), &
                     h%bins(i1,0), sqrt (h%bins2(i1,0))
             end do
          end if
       end if
       do i1 = 1, h%n_bins(1)
          do i2 = 1, h%n_bins(2)
             print *, midpoint (h, i1, 1), midpoint (h, i2, 2), &
                  h%bins(i1,i2), sqrt (h%bins2(i1,i2))
          end do
       end do
       if (present (over)) then
          if (over) then
             do i2 = 1, h%n_bins(2)
                print *, "x1 overflow", midpoint (h, i2, 2), &
                     h%bins(h%n_bins(1)+1,i2), &
                     sqrt (h%bins2(h%n_bins(1)+1,i2))
             end do
             do i1 = 1, h%n_bins(1)
                print *, "x2 overflow", midpoint (h, i1, 1), &
                     h%bins(i1,h%n_bins(2)+1), &
                     sqrt (h%bins2(i1,h%n_bins(2)+1))
             end do
             print *, "double overflow", &
                  h%bins(h%n_bins(1)+1,h%n_bins(2)+1), &
                  sqrt (h%bins2(h%n_bins(1)+1,h%n_bins(2)+1))
          end if
       end if
    end if
  end subroutine write_histogram2
end module histograms
!!! kinematics: removed
! ! kinematics.f90 --
! ! Copyright (C) 1998 by Thorsten Ohl <ohl@hep.tu-darmstadt.de>
! ! 
! ! VAMP is free software; you can redistribute it and/or modify it
! ! under the terms of the GNU General Public License as published by 
! ! the Free Software Foundation; either version 2, or (at your option)
! ! any later version.
! ! 
! ! VAMP is distributed in the hope that it will be useful, but
! ! WITHOUT ANY WARRANTY; without even the implied warranty of
! ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! ! GNU General Public License for more details.
! ! 
! ! You should have received a copy of the GNU General Public License
! ! along with this program; if not, write to the Free Software
! ! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! This version of the source code of vamp has no comments and
! ! can be hard to understand, modify, and improve.  You should have
! ! received a copy of the literate noweb sources of vamp that
! ! contain the documentation in full detail.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! module kinematics
!   use kinds
!   use constants
!   use products, only: dot
!   use specfun, only: gamma
!   implicit none

!   public :: boost_velocity
!   private :: boost_one_velocity, boost_many_velocity
!   public :: boost_momentum
!   private :: boost_one_momentum, boost_many_momentum
!   public :: lambda
!   public :: two_to_three
!   private :: two_to_three_massive, two_to_three_massless
!   public :: one_to_two
!   private :: one_to_two_massive, one_to_two_massless
!   public :: polar_to_cartesian, on_shell
!   public :: massless_isotropic_decay
!   public :: phase_space_volume
!   interface boost_velocity
!      module procedure boost_one_velocity, boost_many_velocity
!   end interface
!   interface boost_momentum
!      module procedure boost_one_momentum, boost_many_momentum
!   end interface
!   interface two_to_three
!      module procedure two_to_three_massive, two_to_three_massless
!   end interface
!   interface one_to_two
!      module procedure one_to_two_massive, one_to_two_massless
!   end interface
!   type, public :: LIPS3
!      real(kind=double), dimension(3,0:3) :: p
!      real(kind=double) :: jacobian
!   end type LIPS3
!   character(len=*), public, parameter :: KINEMATICS_RCS_ID = &
!        "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
! contains
!   function boost_one_velocity (p, beta) result (p_prime)
!     real(kind=double), dimension(0:), intent(in) :: p
!     real(kind=double), dimension(1:), intent(in) :: beta
!     real(kind=double), dimension(0:3) :: p_prime
!     real(kind=double), dimension(1:3) :: b
!     real(kind=double) :: gamma, b_dot_p
!     gamma = 1.0 / sqrt (1.0 - dot_product (beta, beta))
!     b = gamma * beta
!     b_dot_p = dot_product (b, p(1:3))
!     p_prime(0) = gamma * p(0) - b_dot_p
!     p_prime(1:3) = p(1:3) + (b_dot_p / (1.0 + gamma) - p(0)) * b
!   end function boost_one_velocity
!   function boost_many_velocity (p, beta) result (p_prime)
!     real(kind=double), dimension(:,0:), intent(in) :: p
!     real(kind=double), dimension(1:), intent(in) :: beta
!     real(kind=double), dimension(size(p,dim=1),0:3) :: p_prime
!     integer :: i
!     do i = 1, size (p, dim=1)
!        p_prime(i,:) = boost_one_velocity (p(i,:), beta)
!     end do
!   end function boost_many_velocity
!   function boost_one_momentum (p, q) result (p_prime)
!     real(kind=double), dimension(0:), intent(in) :: p, q
!     real(kind=double), dimension(0:3) :: p_prime
!     p_prime = boost_velocity (p, q(1:3) / abs (q(0)))
!   end function boost_one_momentum
!   function boost_many_momentum (p, q) result (p_prime)
!     real(kind=double), dimension(:,0:), intent(in) :: p
!     real(kind=double), dimension(0:), intent(in) :: q
!     real(kind=double), dimension(size(p,dim=1),0:3) :: p_prime
!     p_prime = boost_many_velocity (p, q(1:3) / abs (q(0)))
!   end function boost_many_momentum
!   function lambda (a, b, c) result (lam)
!     real(kind=double), intent(in) :: a, b, c
!     real(kind=double) :: lam
!     lam = a**2 + b**2 + c**2 - 2*(a*b + b*c + c*a)
!   end function lambda
!   function two_to_three_massive &
!        (s, t1, s2, phi, cos_theta3, phi3, ma, mb, m1, m2, m3) result (p)
!     real(kind=double), intent(in) :: &
!          s, t1, s2, phi, cos_theta3, phi3, ma, mb, m1, m2, m3
!     type(LIPS3) :: p
!     real(kind=double), dimension(0:3) :: p23
!     real(kind=double) :: Ea, pa_abs, E1, p1_abs, p3_abs, cos_theta
!     pa_abs = sqrt (lambda (s, ma**2, mb**2) / (4 * s))
!     Ea = sqrt (ma**2 + pa_abs**2)
!     p1_abs = sqrt (lambda (s, m1**2, s2) / (4 * s))
!     E1 = sqrt (m1**2 + p1_abs**2)
!     p3_abs = sqrt (lambda (s2, m2**2, m3**2) / (4 * s2))
!     p%jacobian = &
!          1.0 / (2*PI)**5 * (p3_abs / pa_abs) / (32 * sqrt (s * s2))
!     cos_theta = (t1 - ma**2 - m1**2 + 2*Ea*E1) / (2*pa_abs*p1_abs)
!     p%p(1,1:3) = polar_to_cartesian (p1_abs, cos_theta, phi)
!     p%p(1,0) = on_shell (p%p(1,:), m1)
!     p23(1:3) = - p%p(1,1:3)
!     p23(0) = on_shell (p23, sqrt (s2))
!     p%p(3:2:-1,:) = one_to_two (p23, cos_theta3, phi3, m3, m2)
!   end function two_to_three_massive
!   function two_to_three_massless (s, t1, s2, phi, cos_theta3, phi3) &
!        result (p)
!     real(kind=double), intent(in) :: s, t1, s2, phi, cos_theta3, phi3
!     type(LIPS3) :: p
!     real(kind=double), dimension(0:3) :: p23
!     real(kind=double) :: pa_abs, p1_abs, p3_abs, cos_theta
!     pa_abs = sqrt (s) / 2
!     p1_abs = (s - s2) / (2 * sqrt (s))
!     p3_abs = sqrt (s2) / 2
!     p%jacobian = 1.0 / ((2*PI)**5 * 32 * s)
!     cos_theta = 1 + t1 / (2*pa_abs*p1_abs)
!     p%p(1,0) = p1_abs
!     p%p(1,1:3) = polar_to_cartesian (p1_abs, cos_theta, phi)
!     p23(1:3) = - p%p(1,1:3)
!     p23(0) = on_shell (p23, sqrt (s2))
!     p%p(3:2:-1,:) = one_to_two (p23, cos_theta3, phi3)
!   end function two_to_three_massless
!   function one_to_two_massive (p12, cos_theta, phi, m1, m2) result (p)
!     real(kind=double), dimension(0:), intent(in) :: p12
!     real(kind=double), intent(in) :: cos_theta, phi, m1, m2
!     real(kind=double), dimension(2,0:3) :: p
!     real(kind=double) :: s, p1_abs
!     s = dot (p12, p12)
!     p1_abs = sqrt (lambda (s, m1**2, m2**2) / (4 * s))
!     p(1,1:3) = polar_to_cartesian (p1_abs, cos_theta, phi)
!     p(2,1:3) = - p(1,1:3)
!     p(1,0) = on_shell (p(1,:), m1)
!     p(2,0) = on_shell (p(2,:), m2)
!     p = boost_momentum (p, - p12)
!   end function one_to_two_massive
!   function one_to_two_massless (p12, cos_theta, phi) result (p)
!     real(kind=double), dimension(0:), intent(in) :: p12
!     real(kind=double), intent(in) :: cos_theta, phi
!     real(kind=double), dimension(2,0:3) :: p
!     real(kind=double) :: p1_abs
!     p1_abs = sqrt (dot (p12, p12)) / 2
!     p(1,0) = p1_abs
!     p(1,1:3) = polar_to_cartesian (p1_abs, cos_theta, phi)
!     p(2,0) = p1_abs
!     p(2,1:3) = - p(1,1:3)
!     p = boost_momentum (p, - p12)
!   end function one_to_two_massless
!   function polar_to_cartesian (v_abs, cos_theta, phi) result (v)
!     real(kind=double), intent(in) :: v_abs, cos_theta, phi
!     real(kind=double), dimension(3) :: v
!     real(kind=double) :: sin_phi, cos_phi, sin_theta
!     sin_theta = sqrt (1.0 - cos_theta**2)
!     cos_phi = cos (phi)
!     sin_phi = sin (phi)
!     v = (/ sin_theta * cos_phi, sin_theta * sin_phi, cos_theta /) * v_abs
!   end function polar_to_cartesian
!   function on_shell (p, m) result (E)
!     real(kind=double), dimension(0:), intent(in) :: p
!     real(kind=double), intent(in) :: m
!     real(kind=double) :: E
!     E = sqrt (m**2 + dot_product (p(1:3), p(1:3)))
!   end function on_shell
!   function massless_isotropic_decay (roots, ran) result (p)
!     real (kind=double), intent(in) :: roots
!     real (kind=double), dimension(:,:), intent(in) :: ran
!     real (kind=double), dimension(size(ran,dim=1),0:3) :: p
!     real (kind=double), dimension(size(ran,dim=1),0:3) :: q
!     real (kind=double), dimension(0:3) :: qsum
!     real (kind=double) :: cos_theta, sin_theta, phi, qabs, x, r, z
!     integer :: k
!     do k = 1, size (p, dim = 1)
!        q(k,0) = - log (ran(k,1) * ran(k,2))
!        cos_theta = 2 * ran(k,3) - 1
!        sin_theta = sqrt (1 - cos_theta**2)
!        phi = 2 * PI * ran(k,4)
!        q(k,1) = q(k,0) * sin_theta * cos (phi)
!        q(k,2) = q(k,0) * sin_theta * sin (phi)  
!        q(k,3) = q(k,0) * cos_theta
!     enddo
!     qsum = sum (q, dim = 1)
!     qabs = sqrt (dot (qsum, qsum))
!     x = roots / qabs
!     do k = 1, size (p, dim = 1)
!        r = dot (q(k,:), qsum) / qabs
!        z = (q(k,0) + r) / (qsum(0) + qabs)
!        p(k,1:3) = x * (q(k,1:3) - qsum(1:3) * z)
!        p(k,0) = x * r
!     enddo
!   end function massless_isotropic_decay
!   function phase_space_volume (n, roots) result (volume)
!     integer, intent(in) :: n
!     real (kind=double), intent(in) :: roots
!     real (kind=double) :: volume
!     real (kind=double) :: nd
!     nd = n
!     volume = (nd - 1) / (8*PI * (gamma (nd))**2) * (roots / (4*PI))**(2*n-4)
!   end function phase_space_volume
! end module kinematics
!!! phase_space removed
! module phase_space
!   use kinds
!   use constants
!   use kinematics !NODEP!
!   use tao_random_numbers
!   implicit none

!   public :: random_LIPS3
!   private :: random_LIPS3_unit, random_LIPS3_unit_massless
!   private :: LIPS3_unit_to_s2_t1_angles, LIPS3_unit_to_s2_t1_angles_m0
!   interface random_LIPS3
!      module procedure random_LIPS3_unit, random_LIPS3_unit_massless
!   end interface
!   type, public :: LIPS3_unit
!      real(kind=double), dimension(5) :: x
!      real(kind=double) :: s
!      real(kind=double), dimension(2) :: mass_in
!      real(kind=double), dimension(3) :: mass_out
!      real(kind=double) :: jacobian
!   end type LIPS3_unit
!   type, public :: LIPS3_unit_massless
!      real(kind=double), dimension(5) :: x
!      real(kind=double) :: s
!      real(kind=double) :: jacobian
!   end type LIPS3_unit_massless
!   type, public :: LIPS3_s2_t1_angles
!      real(kind=double) :: s2, t1, phi, cos_theta3, phi3
!      real(kind=double) :: s
!      real(kind=double), dimension(2) :: mass_in
!      real(kind=double), dimension(3) :: mass_out
!      real(kind=double) :: jacobian
!   end type LIPS3_s2_t1_angles
!   type, public :: LIPS3_s2_t1_angles_massless
!      real(kind=double) :: s2, t1, phi, cos_theta3, phi3
!      real(kind=double) :: s
!      real(kind=double) :: jacobian
!   end type LIPS3_s2_t1_angles_massless
!   type, public :: LIPS3_momenta
!      real(kind=double), dimension(0:3,3) :: p
!      real(kind=double) :: s
!      real(kind=double), dimension(2) :: mass_in
!      real(kind=double), dimension(3) :: mass_out
!      real(kind=double) :: jacobian
!   end type LIPS3_momenta
!   type, public :: LIPS3_momenta_massless
!      real(kind=double), dimension(0:3,3) :: p
!      real(kind=double) :: s
!      real(kind=double) :: jacobian
!   end type LIPS3_momenta_massless
!   character(len=*), public, parameter :: PHASE_SPACE_RCS_ID = &
!        "$Id: vamp_bundle.f90,v 1.20 2005/10/11 15:18:48 kilian Exp $"
! contains
!   subroutine random_LIPS3_unit (rng, lips)
!     type(tao_random_state), intent(inout) :: rng
!     type(LIPS3_unit), intent(inout) :: lips
!     call tao_random_number (rng, lips%x)
!     lips%jacobian = 1
!   end subroutine random_LIPS3_unit
!   subroutine random_LIPS3_unit_massless (rng, lips)
!     type(tao_random_state), intent(inout) :: rng
!     type(LIPS3_unit_massless), intent(inout) :: lips
!     call tao_random_number (rng, lips%x)
!     lips%jacobian = 1
!   end subroutine random_LIPS3_unit_massless
!   subroutine LIPS3_unit_to_s2_t1_angles (s2_t1_angles, unit)
!     type(LIPS3_s2_t1_angles), intent(out) :: s2_t1_angles
!     type(LIPS3_unit), intent(in) :: unit
!   end subroutine  LIPS3_unit_to_s2_t1_angles
!   subroutine LIPS3_unit_to_s2_t1_angles_m0 (s2_t1_angles, unit)
!     type(LIPS3_s2_t1_angles_massless), intent(out) :: s2_t1_angles
!     type(LIPS3_unit_massless), intent(in) :: unit
!   end subroutine  LIPS3_unit_to_s2_t1_angles_m0
! end module phase_space
