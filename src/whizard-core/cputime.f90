! WHIZARD 2.2.2 July 6 2014
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

module cputime

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use unit_tests

  implicit none
  private

  public :: time_t
  public :: assignment(=)
  public :: timer_t
  public :: cputime_test

  type :: time_t
     private
     logical :: known = .false.
     real :: value = 0
   contains
     procedure :: write => time_write
     procedure :: set_current => time_set_current
     generic :: assignment(=) => time_assign_from_integer, time_assign_from_real
     procedure, private :: time_assign_from_integer
     procedure, private :: time_assign_from_real
     generic :: operator(-) => subtract_times
     generic :: operator(+) => add_times
     procedure, private :: subtract_times
     procedure, private :: add_times
     procedure :: is_known => time_is_known
     generic :: expand => time_expand_s, time_expand_ms, &
          time_expand_hms, time_expand_dhms
     procedure, private :: time_expand_s
     procedure, private :: time_expand_ms
     procedure, private :: time_expand_hms
     procedure, private :: time_expand_dhms
     procedure :: to_string_s => time_to_string_s
     procedure :: to_string_ms => time_to_string_ms
     procedure :: to_string_hms => time_to_string_hms
     procedure :: to_string_dhms => time_to_string_dhms
  end type time_t

  type, extends (time_t) :: timer_t
     private
     logical :: running = .false.
     type(time_t) :: t1, t2
   contains
     procedure :: write => timer_write
     procedure :: start => timer_start
     procedure :: restart => timer_restart
     procedure :: stop => timer_stop
     procedure, private :: evaluate => timer_evaluate
  end type timer_t
  

  interface assignment(=)
    module procedure real_assign_time
    module procedure real_default_assign_time
  end interface


contains

  subroutine time_write (object, unit)
    class(time_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "Time in seconds ="
    if (object%known) then
       write (u, "(1x,ES10.3)")  object%value
    else
       write (u, "(1x,A)")  "[unknown]"
    end if
  end subroutine time_write

  subroutine time_set_current (time)
    class(time_t), intent(out) :: time
    integer :: msecs
    call system_clock (msecs)
    time%value = real (msecs) / 1000.
    time%known = time%value > 0
  end subroutine time_set_current

  pure subroutine real_assign_time (r, time)
    real, intent(out) :: r
    class(time_t), intent(in) :: time
    if (time%known) then
       r = time%value
    else
       r = 0
    end if
  end subroutine real_assign_time
    
  pure subroutine real_default_assign_time (r, time)
    real(default), intent(out) :: r
    class(time_t), intent(in) :: time
    if (time%known) then
       r = time%value
    else
       r = 0
    end if
  end subroutine real_default_assign_time
    
  subroutine time_assign_from_integer (time, ival)
    class(time_t), intent(out) :: time
    integer, intent(in) :: ival
    time%value = ival
    time%known = .true.
  end subroutine time_assign_from_integer
  
  subroutine time_assign_from_real (time, rval)
    class(time_t), intent(out) :: time
    real, intent(in) :: rval
    time%value = rval
    time%known = .true.
  end subroutine time_assign_from_real
  
  pure function subtract_times (t_end, t_begin) result (time)
    type(time_t) :: time
    class(time_t), intent(in) :: t_end, t_begin
    if (t_end%known .and. t_begin%known) then
       time%known = .true.
       time%value = t_end%value - t_begin%value
    end if
  end function subtract_times
    
  pure function add_times (t1, t2) result (time)
    type(time_t) :: time
    class(time_t), intent(in) :: t1, t2
    if (t1%known .and. t2%known) then
       time%known = .true.
       time%value = t1%value + t2%value
    end if
  end function add_times
    
  function time_is_known (time) result (flag)
    class(time_t), intent(in) :: time
    logical :: flag
    flag = time%known
  end function time_is_known
  
  subroutine time_expand_s (time, sec)
    class(time_t), intent(in) :: time
    integer, intent(out) :: sec
    if (time%known) then
       sec = time%value
    else
       call msg_bug ("Time: attempt to expand undefined value")
    end if
  end subroutine time_expand_s

  subroutine time_expand_ms (time, min, sec)
    class(time_t), intent(in) :: time
    integer, intent(out) :: min, sec
    if (time%known) then
       if (time%value >= 0) then
          sec = mod (int (time%value), 60)
       else
          sec = - mod (int (- time%value), 60)
       end if
       min = time%value / 60
    else
       call msg_bug ("Time: attempt to expand undefined value")
    end if
  end subroutine time_expand_ms

  subroutine time_expand_hms (time, hour, min, sec)
    class(time_t), intent(in) :: time
    integer, intent(out) :: hour, min, sec
    call time%expand (min, sec)
    hour = min / 60
    if (min >= 0) then
       min = mod (min, 60)
    else
       min = - mod (-min, 60)
    end if
  end subroutine time_expand_hms

  subroutine time_expand_dhms (time, day, hour, min, sec)
    class(time_t), intent(in) :: time
    integer, intent(out) :: day, hour, min, sec
    call time%expand (hour, min, sec)
    day = hour / 24
    if (hour >= 0) then
       hour = mod (hour, 24)
    else
       hour = - mod (- hour, 24)
    end if
  end subroutine time_expand_dhms

  function time_to_string_s (time) result (str)
    class(time_t), intent(in) :: time
    type(string_t) :: str
    character(256) :: buffer
    integer :: s
    call time%expand (s)
    write (buffer, "(I0,'s')")  s
    str = trim (buffer)
  end function time_to_string_s

  function time_to_string_ms (time, blank) result (str)
    class(time_t), intent(in) :: time
    logical, intent(in), optional :: blank
    type(string_t) :: str
    character(256) :: buffer    
    integer :: s, m
    logical :: x_out
    x_out = .false.
    if (present (blank))  x_out = blank
    call time%expand (m, s)
    write (buffer, "(I0,'m:',I2.2,'s')")  m, abs (s)
    str = trim (buffer)
    if (x_out) then
       str = replace (str, len(str)-1, "X")
    end if
  end function time_to_string_ms

  function time_to_string_hms (time) result (str)
    class(time_t), intent(in) :: time
    type(string_t) :: str
    character(256) :: buffer
    integer :: s, m, h
    call time%expand (h, m, s)
    write (buffer, "(I0,'h:',I2.2,'m:',I2.2,'s')")  h, abs (m), abs (s)
    str = trim (buffer)
  end function time_to_string_hms

  function time_to_string_dhms (time) result (str)
    class(time_t), intent(in) :: time
    type(string_t) :: str
    character(256) :: buffer
    integer :: s, m, h, d
    call time%expand (d, h, m, s)
    write (buffer, "(I0,'d:',I2.2,'h:',I2.2,'m:',I2.2,'s')")  &
         d, abs (h), abs (m), abs (s)
    str = trim (buffer)
  end function time_to_string_dhms

  subroutine timer_write (object, unit)
    class(timer_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    if (object%running) then
       write (u, "(1x,A)")  "Time in seconds = [running]"
    else
       call object%time_t%write (u)
    end if
  end subroutine timer_write

  subroutine timer_start (timer)
    class(timer_t), intent(out) :: timer
    call timer%t1%set_current ()
    timer%running = .true.
  end subroutine timer_start
  
  subroutine timer_restart (timer)
    class(timer_t), intent(inout) :: timer
    if (timer%t1%known .and. .not. timer%running) then
       timer%running = .true.
    else
       call msg_bug ("Timer: restart attempt from wrong status")
    end if       
  end subroutine timer_restart
  
  subroutine timer_stop (timer)
    class(timer_t), intent(inout) :: timer
    call timer%t2%set_current ()
    timer%running = .false.
    call timer%evaluate ()
  end subroutine timer_stop
  
  subroutine timer_evaluate (timer)
    class(timer_t), intent(inout) :: timer
    timer%time_t = timer%t2 - timer%t1
  end subroutine timer_evaluate
  

  subroutine cputime_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (cputime_1, "cputime_1", &
         "time operations", &
         u, results)
    call test (cputime_2, "cputime_2", &
         "timer", &
         u, results)
  end subroutine cputime_test
  
  subroutine cputime_1 (u)
    integer, intent(in) :: u
    type(time_t) :: time, time1, time2
    real :: t
    integer :: d, h, m, s

    write (u, "(A)")  "* Test output: cputime_1"
    write (u, "(A)")  "*   Purpose: check time operations"
    write (u, "(A)")      
    
    write (u, "(A)") "* Undefined time"
    write (u, *)

    call time%write (u)

    write (u, *)
    write (u, "(A)") "* Set time to zero"
    write (u, *)

    time = 0
    call time%write (u)

    write (u, *)
    write (u, "(A)") "* Set time to 1.234 s"
    write (u, *)

    time = 1.234
    call time%write (u)

    t = time
    write (u, "(1x,A,F6.3)")  "Time as real =", t

    write (u, *)
    write (u, "(A)") "* Compute time difference"
    write (u, *)
    
    time1 = 5.33
    time2 = 7.55
    time = time2 - time1
    
    call time1%write (u)
    call time2%write (u)
    call time%write (u)

    write (u, *)
    write (u, "(A)") "* Compute time sum"
    write (u, *)
    
    time = time2 + time1
    
    call time1%write (u)
    call time2%write (u)
    call time%write (u)

    write (u, *)
    write (u, "(A)") "* Expand time"
    write (u, *)
    
    time1 = ((24 + 1) * 60 + 1) * 60 + 1
    time2 = ((3 * 24 + 23) * 60 + 59) * 60 + 59
    
    call time1%expand (s)
    write (u, 1)  "s =", s
    call time1%expand (m,s)
    write (u, 1)  "ms =", m, s
    call time1%expand (h,m,s)
    write (u, 1)  "hms =", h, m, s
    call time1%expand (d,h,m,s)
    write (u, 1)  "dhms =", d, h, m, s
    
    call time2%expand (s)
    write (u, 1)  "s =", s
    call time2%expand (m,s)
    write (u, 1)  "ms =", m, s
    call time2%expand (h,m,s)
    write (u, 1)  "hms =", h, m, s
    call time2%expand (d,h,m,s)
    write (u, 1)  "dhms =", d, h, m, s
    
    write (u, *)
    write (u, "(A)") "* Expand negative time"
    write (u, *)
    
    time1 = - (((24 + 1) * 60 + 1) * 60 + 1)
    time2 = - (((3 * 24 + 23) * 60 + 59) * 60 + 59)
    
    call time1%expand (s)
    write (u, 1)  "s =", s
    call time1%expand (m,s)
    write (u, 1)  "ms =", m, s
    call time1%expand (h,m,s)
    write (u, 1)  "hms =", h, m, s
    call time1%expand (d,h,m,s)
    write (u, 1)  "dhms =", d, h, m, s
    
    call time2%expand (s)
    write (u, 1)  "s =", s
    call time2%expand (m,s)
    write (u, 1)  "ms =", m, s
    call time2%expand (h,m,s)
    write (u, 1)  "hms =", h, m, s
    call time2%expand (d,h,m,s)
    write (u, 1)  "dhms =", d, h, m, s
    
1   format (1x,A,1x,4(I0,:,':'))

    write (u, *)
    write (u, "(A)") "* String from time"
    write (u, *)
    
    time1 = ((24 + 1) * 60 + 1) * 60 + 1
    time2 = ((3 * 24 + 23) * 60 + 59) * 60 + 59
    
    write (u, "(A)")  char (time1%to_string_s ())
    write (u, "(A)")  char (time1%to_string_ms ())
    write (u, "(A)")  char (time1%to_string_hms ())
    write (u, "(A)")  char (time1%to_string_dhms ())

    write (u, "(A)")  char (time2%to_string_s ())
    write (u, "(A)")  char (time2%to_string_ms ())
    write (u, "(A)")  char (time2%to_string_hms ())
    write (u, "(A)")  char (time2%to_string_dhms ())

    write (u, "(A)")
    write (u, "(A)")  "* Blanking out the last second entry"
    write (u, "(A)")
    
    write (u, "(A)")  char (time1%to_string_ms ())
    write (u, "(A)")  char (time1%to_string_ms (.true.))
        
    write (u, *)
    write (u, "(A)") "* String from negative time"
    write (u, *)
    
    time1 = -(((24 + 1) * 60 + 1) * 60 + 1)
    time2 = -(((3 * 24 + 23) * 60 + 59) * 60 + 59)
    
    write (u, "(A)")  char (time1%to_string_s ())
    write (u, "(A)")  char (time1%to_string_ms ())
    write (u, "(A)")  char (time1%to_string_hms ())
    write (u, "(A)")  char (time1%to_string_dhms ())

    write (u, "(A)")  char (time2%to_string_s ())
    write (u, "(A)")  char (time2%to_string_ms ())
    write (u, "(A)")  char (time2%to_string_hms ())
    write (u, "(A)")  char (time2%to_string_dhms ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: cputime_1"    

  end subroutine cputime_1
  
  subroutine cputime_2 (u)
    integer, intent(in) :: u
    type(timer_t) :: timer

    write (u, "(A)")  "* Test output: cputime_2"
    write (u, "(A)")  "*   Purpose: check timer"
    write (u, "(A)")      
    
    write (u, "(A)") "* Undefined timer"
    write (u, *)

    call timer%write (u)

    write (u, *)
    write (u, "(A)") "* Start timer"
    write (u, *)

    call timer%start ()
    call timer%write (u)

    write (u, *)
    write (u, "(A)") "* Stop timer (injecting fake timings)"
    write (u, *)

    call timer%stop ()
    timer%t1 = 2
    timer%t2 = 5
    call timer%evaluate ()
    call timer%write (u)

    write (u, *)
    write (u, "(A)") "* Restart timer"
    write (u, *)

    call timer%restart ()
    call timer%write (u)

    write (u, *)
    write (u, "(A)") "* Stop timer again (injecting fake timing)"
    write (u, *)

    call timer%stop ()
    timer%t2 = 10
    call timer%evaluate ()
    call timer%write (u)

    write (u, *)
    write (u, "(A)")  "* Test output end: cputime_2"    

  end subroutine cputime_2
  

end module cputime
