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

module cputime

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
use diagnostics !NODEP!

  implicit none
  private

  public :: time_t
  public :: time_write
  public :: time_current
  public :: assignment(=)
  public :: operator(-)
  public :: time_retrieve
  public :: time2string_s
  public :: time2string_ms
  public :: time2string_hms
  public :: time2string_dhms
  public :: time2string

  type :: time_t
     private
     logical :: known = .false.
     real :: value = 0
  end type time_t


  interface assignment(=)
    module procedure real_assign_time
  end interface

  interface operator(-)
    module procedure subtract_times
  end interface

  interface time_retrieve
    module procedure time_retrieve_s, time_retrieve_ms, time_retrieve_hms, &
       time_retrieve_dhms
  end interface time_retrieve

contains

  subroutine time_write (time, unit)
    type(time_t), intent(in) :: time
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(A)", advance="no")  "Time in seconds = "
    if (time%known) then
       write (u, *)  time%value
    else
       write (u, *)  "[unknown]"
    end if
  end subroutine time_write

  function time_current () result (time)
    type(time_t) :: time
    integer :: msecs
    call system_clock (msecs)
    time%value = real (msecs) / 1000.
    time%known = time%value > 0
  end function time_current

  pure subroutine real_assign_time (r, time)
    real(default), intent(out) :: r
    type(time_t), intent(in) :: time
    if (time%known) then
       r = time%value
    else
       r = 0
    end if
  end subroutine real_assign_time
    
  pure function subtract_times (t_end, t_begin) result (time)
    type(time_t) :: time
    type(time_t), intent(in) :: t_end, t_begin
    if (t_end%known .and. t_begin%known) then
       time%known = .true.
       time%value = t_end%value - t_begin%value
    end if
  end function subtract_times
    
  subroutine time_retrieve_s (time, sec)
    integer, intent(in) :: time
    integer, intent(out) :: sec
    sec = time
  end subroutine time_retrieve_s

  subroutine time_retrieve_ms (time, min, sec)
    integer, intent(in) :: time
    integer, intent(out) :: min, sec
    sec = mod (time, 60)
    min = time / 60
  end subroutine time_retrieve_ms

  subroutine time_retrieve_hms (time, hour, min, sec)
    integer, intent(in) :: time
    integer, intent(out) :: hour, min, sec
    call time_retrieve_ms (time, min, sec)
    hour = min / 60
    min = mod (min, 60)
  end subroutine time_retrieve_hms

  subroutine time_retrieve_dhms (time, day, hour, min, sec)
    integer, intent(in) :: time
    integer, intent(out) :: day, hour, min, sec
    call time_retrieve_hms (time, hour, min, sec)
    day = hour / 24
    hour = mod (hour, 24)
  end subroutine time_retrieve_dhms

  function time2string_s (time) result (str)
    integer, intent(in) :: time
    type(string_t) :: str
    str = int2string (time) // "s"
  end function time2string_s

  function time2string_ms (time) result (str)
    integer, intent(in) :: time
    type(string_t) :: str
    integer :: min, sec
    call time_retrieve (time, min, sec)
    str = int2string (min) // "m:" // int2string (sec) // "s"
  end function time2string_ms

 function time2string_hms (time) result (str)
    integer, intent(in) :: time
    type(string_t) :: str
    integer :: hour, min, sec
    call time_retrieve (time, hour, min, sec)
    str = int2string (hour) // "h:" // &
       int2string (min) // "m:" // int2string (sec) // "s"
  end function time2string_hms

  function time2string_dhms (time) result (str)
    integer, intent(in) :: time
    type(string_t) :: str
    integer :: day, hour, min, sec
    call time_retrieve (time, day, hour, min, sec)
    str = int2string (day) // "d:" // int2string (hour) // "h:" // &
       int2string (min) // "m:" // int2string (sec) // "s"
  end function time2string_dhms

  function time2string (time) result (str)
    integer, intent(in) :: time
    type(string_t) :: str
    integer :: day, hour, min, sec
    call time_retrieve (time, day, hour, min, sec)
    if (day /= 0) then
       str = time2string_dhms (time)
    else if (hour /= 0) then
       str = time2string_hms (time)
    else if (min /= 0) then
       str = time2string_ms (time)
    else
       str = time2string_s (time)
    end if
  end function time2string

end module cputime
