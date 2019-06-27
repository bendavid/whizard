! WHIZARD 2.0.0 Mon Apr 12 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module clock

  use kinds, only: default !NODEP!
  use file_utils !NODEP!

  implicit none
  private

  public :: time_t
  public :: time_write
  public :: time_current
  public :: time_to_seconds, time_from_seconds
  public :: operator(+), operator(-)
  public :: operator(==)
  public :: operator(<), operator(>)
  public :: operator(<=), operator(>=)

  type :: time_t
     integer :: day = 0
     integer :: hour = 0
     integer :: minute = 0
     integer :: second = 0
     integer :: millisecond = 0
  end type time_t

  interface time_write
     module procedure time_write_unit
     module procedure time_write_string
  end interface
  interface operator(+)
     module procedure time_add
  end interface
  interface operator(-)
     module procedure time_subtract
  end interface
  interface operator(==)
     module procedure time_equal
  end interface
  interface operator(<)
     module procedure time_less_than
  end interface
  interface operator(>)
     module procedure time_greater_than
  end interface
  interface operator(<=)
     module procedure time_less_equal
  end interface
  interface operator(>=)
     module procedure time_greater_equal
  end interface

contains

  subroutine time_write_unit (t, unit, advance, days, seconds, milliseconds)
    type(time_t), intent(in) :: t
    integer, intent(in) :: unit
    character(len=*), intent(in), optional :: advance
    logical, intent(in), optional :: days, seconds, milliseconds
    logical :: dd, ss, ms
    character(len=3) :: adv
    if (present (advance)) then
       adv = advance
    else
       adv = "yes"
    end if
    dd = .false.;  if (present (days)) dd = days
    ss = .true.;   if (present (seconds))  ss = seconds
    ms = .false.;  if (present (milliseconds)) ms = milliseconds
    if (dd) then
       write (unit, "(I3,A,1x,I2.2,A,1x,I2.2,A)", advance="no") &
            t%day, "d", t%hour, "h", t%minute, "m"
    else
       write (unit, "(I5,A,1x,I2.2,A)", advance="no") &
            t%day * 24 + t%hour, "h", t%minute, "m"
    end if
    if (ss) then
       write (unit, "(1x,I2.2)", advance="no")  t%second
       if (ms) then
          write (unit, "(A,I3.3,A)", advance=trim(adv))  ".", t%millisecond, "s"
       else
          write (unit, "(A)", advance=trim(adv)) "s"
       end if
    end if
  end subroutine time_write_unit

  subroutine time_write_string (t, string, days, seconds, milliseconds)
    type(time_t), intent(in) :: t
    character(*), intent(out) :: string
    logical, intent(in), optional :: days, seconds, milliseconds
    integer :: unit
    unit = free_unit ()
    open (unit, status="scratch", action="readwrite")
    call time_write_unit (t, unit, &
         days=days, seconds=seconds, milliseconds=milliseconds)
    rewind (unit)
    read (unit, "(A)") string
    close (unit)
  end subroutine time_write_string

  function time_current () result (t)
    type(time_t) :: t
    integer, dimension(8) :: values
    integer :: year
    call date_and_time (values = values)
    t%millisecond = values(8)
    t%second = values(7)
    t%minute = values(6)
    t%hour = values(5)
    ! values(4) is the difference local time - GMT in minutes
    t%day = values(3) - 1
    select case (values(2))
    case ( 1)
    case ( 2); t%day = t%day + 31
    case ( 3); t%day = t%day + 31 + 28
    case ( 4); t%day = t%day + 31 + 28 + 31
    case ( 5); t%day = t%day + 31 + 28 + 31 + 30
    case ( 6); t%day = t%day + 31 + 28 + 31 + 30 + 31
    case ( 7); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30
    case ( 8); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30 + 31
    case ( 9); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31
    case (10); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30
    case (11); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31
    case (12); t%day = t%day + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30
    end select
    year = values(1) - 2000
    if (year < 1)  return
    t%day = t%day + 365 * year + ((year-1) / 4 + 1)
    if (mod (year, 4) == 0 .and. values(2) > 2)  t%day = t%day + 1
  end function time_current

  function time_to_seconds (t) result (s)
    type(time_t), intent(in) :: t
    real(kind=default) :: s
    s = t%millisecond / 1000._default &
         + t%second &
         + 60._default * (t%minute &
                          + 60._default * (t%hour &
                                           + 24._default * t%day))
  end function time_to_seconds
    
  function time_from_seconds (s) result (t)
    real(kind=default), intent(in) :: s
    type(time_t) :: t
    t%millisecond = s * 1000
    t%second = t%millisecond / 1000
    t%millisecond = mod (t%millisecond, 1000)
    t%minute = t%second / 60
    t%second = mod (t%second, 60)
    t%hour = t%minute / 60
    t%minute = mod (t%minute, 60)
    t%day = t%hour / 24
    t%hour = mod (t%hour, 24)
  end function time_from_seconds

  function time_add (t1, t2) result (t)
    type(time_t), intent(in) :: t1, t2
    type(time_t) :: t
    t%millisecond = t1%millisecond + t2%millisecond
    t%second = t1%second + t2%second
    t%minute = t1%minute + t2%minute
    t%hour = t1%hour + t2%hour
    t%day = t1%day + t2%day
    if (t%millisecond > 999) then
       t%second = t%second + t%millisecond / 1000
       t%millisecond = modulo (t%millisecond, 1000)
    end if
    if (t%second > 59) then
       t%minute = t%minute + t%second / 60
       t%second = modulo (t%second, 60)
    end if
    if (t%minute > 59) then
       t%hour = t%hour + t%minute / 60
       t%minute = modulo (t%minute, 60)
    end if
    if (t%hour > 23) then
       t%day = t%day + t%hour / 24
       t%hour = modulo (t%hour, 24)
    end if
  end function time_add
    
  function time_subtract (t1, t2) result (t)
    type(time_t), intent(in) :: t1, t2
    type(time_t) :: t
    t%millisecond = t1%millisecond - t2%millisecond
    t%second = t1%second - t2%second
    t%minute = t1%minute - t2%minute
    t%hour = t1%hour - t2%hour
    t%day = t1%day - t2%day
    if (t%millisecond < 0) then
       t%second = t%second - 1 + t%millisecond / 1000
       t%millisecond = modulo (t%millisecond, 1000)
    end if
    if (t%second < 0) then
       t%minute = t%minute - 1 + t%second / 60
       t%second = modulo (t%second, 60)
    end if
    if (t%minute < 0) then
       t%hour = t%hour - 1 + t%minute / 60
       t%minute = modulo (t%minute, 60)
    end if
    if (t%hour < 0) then
       t%day = t%day - 1 + t%hour / 24
       t%hour = modulo (t%hour, 24)
    end if
    if (t%day < 0) then
       t%day = 0
    end if
  end function time_subtract
    
  function time_equal (t1, t2) result (equal)
    type(time_t), intent(in) :: t1, t2
    logical :: equal
    equal = (t1%day==t2%day) .and. (t1%hour==t2%hour) .and. &
         (t1%minute==t2%minute) .and. (t1%second==t2%second) .and. &
         (t1%millisecond==t2%millisecond)
  end function time_equal

  function time_less_than (t1, t2) result (less)
    type(time_t), intent(in) :: t1, t2
    logical :: less
    if (t1%day < t2%day) then
       less = .true.
    else if (t1%day == t2%day) then
       if (t1%hour < t2%hour) then
          less = .true.
       else if (t1%hour == t2%hour) then
          if (t1%minute < t2%minute) then
             less = .true.
          else if (t1%minute == t2%minute) then
             if (t1%second < t2%second) then
                less = .true.
             else if (t1%second == t2%second) then
                if (t1%millisecond < t2%millisecond) then
                   less = .true.
                else
                   less = .false.
                end if
             else
                less = .false.
             end if
          else
             less = .false.
          end if
       else
          less = .false.
       end if
    else
       less = .false.
    end if
  end function time_less_than

  function time_greater_than (t1, t2) result (greater)
    type(time_t), intent(in) :: t1, t2
    logical :: greater
    greater = time_less_than (t2, t1)
  end function time_greater_than

  function time_less_equal (t1, t2) result (less_equal)
    type(time_t), intent(in) :: t1, t2
    logical :: less_equal
    less_equal = time_equal (t1, t2) .or. time_less_than (t1, t2)
  end function time_less_equal

  function time_greater_equal (t1, t2) result (greater_equal)
    type(time_t), intent(in) :: t1, t2
    logical :: greater_equal
    greater_equal = time_equal (t1, t2) .or. time_greater_than (t1, t2)
  end function time_greater_equal


end module clock
