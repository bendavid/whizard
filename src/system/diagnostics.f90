! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module diagnostics

  use, intrinsic :: iso_c_binding !NODEP! 
  use, intrinsic :: iso_fortran_env, only: output_unit !NODEP!

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use string_utils, only: str
  use io_units

  use system_dependencies
  use system_defs, only: BUFFER_SIZE, MAX_ERRORS

  implicit none
  private

  public :: RESULT, DEBUG, DEBUG2
  public :: d_area
  public :: D_ALL, D_PARTICLES, D_EVENTS, D_SHOWER, D_MODEL_F, &
       D_MATCHING, D_TRANSFORMS, D_SUBTRACTION, D_VIRTUAL, D_THRESHOLD
  public :: msg_level
  public :: mask_fatal_errors
  public :: msg_count
  public :: msg_list_clear
  public :: msg_summary
  public :: msg_listing
  public :: msg_buffer
  public :: msg_terminate
  public :: msg_bug, msg_fatal, msg_error, msg_warning
  public :: msg_message, msg_result
  public :: msg_debug
  public :: msg_debug2
  public :: debug_active
  public :: debug2_active
  public :: msg_show_progress
  public :: msg_banner
  public :: logging
  public :: logfile_init
  public :: logfile_final
  public :: logfile_unit
  public :: expect_record
  public :: expect_clear
  public :: expect_summary
  public :: int2string
  public :: int2char
  public :: int2fixed
  public :: real2string
  public :: real2char
  public :: real2fixed
  public :: pacify
  public :: wo_sigint
  public :: wo_sigterm
  public :: wo_sigxcpu
  public :: wo_sigxfsz

  public :: mask_term_signals
  public :: release_term_signals
  public :: signal_is_pending
  public :: terminate_now_if_signal

  integer, parameter :: TERMINATE=-2, BUG=-1, FATAL=1, &
       ERROR=2, WARNING=3, MESSAGE=4, RESULT=5, &
       DEBUG=6, DEBUG2=7
  integer, parameter :: D_ALL=0, D_PARTICLES=1, D_EVENTS=2, &
       D_SHOWER=3, D_MODEL_F=4, &
       D_MATCHING=5, D_TRANSFORMS=6, &
       D_SUBTRACTION=7, D_VIRTUAL=8, D_THRESHOLD=9
  integer, parameter :: TERM_STOP = 0, TERM_EXIT = 1, TERM_CRASH = 2

  type :: string_list
     character(len=BUFFER_SIZE) :: string
     type(string_list), pointer :: next
  end type string_list
  type :: string_list_pointer
     type(string_list), pointer :: first, last
  end type string_list_pointer
  

  integer, save, dimension(D_ALL:20) :: msg_level = RESULT
  logical, save :: mask_fatal_errors = .false.
  integer, save :: handle_fatal_errors = TERM_EXIT
  integer, dimension(TERMINATE:WARNING), save :: msg_count = 0
  type(string_list_pointer), dimension(TERMINATE:WARNING), save :: &
       & msg_list = string_list_pointer (null(), null())
  character(len=BUFFER_SIZE), save :: msg_buffer = " "
  integer, save :: log_unit = -1
  logical, target, save :: logging = .false.
  integer, save :: expect_total = 0
  integer, save :: expect_failures = 0

  integer(c_int), bind(C), volatile :: wo_sigint = 0
  integer(c_int), bind(C), volatile :: wo_sigterm = 0
  integer(c_int), bind(C), volatile :: wo_sigxcpu = 0
  integer(c_int), bind(C), volatile :: wo_sigxfsz = 0


  interface d_area
     module procedure d_area_of_string
     module procedure d_area_to_string
  end interface
  interface msg_debug
     module procedure msg_debug_none
     module procedure msg_debug_logical
     module procedure msg_debug_integer
     module procedure msg_debug_real
  end interface
  interface msg_debug2
     module procedure msg_debug2_none
     module procedure msg_debug2_logical
     module procedure msg_debug2_integer
     module procedure msg_debug2_real
  end interface
  interface
     subroutine exit (status) bind (C)
       use iso_c_binding !NODEP!
       integer(c_int), value :: status
     end subroutine exit
  end interface

  interface real2string
     module procedure real2string_list, real2string_fmt
  end interface
  interface real2char
     module procedure real2char_list, real2char_fmt
  end interface
  interface pacify
     module procedure pacify_real_default
     module procedure pacify_complex_default
  end interface pacify
  
  interface
     integer(c_int) function wo_mask_sigint () bind(C)
       import
     end function wo_mask_sigint
  end interface
  interface
     integer(c_int) function wo_mask_sigterm () bind(C)
       import
     end function wo_mask_sigterm
  end interface
  interface
     integer(c_int) function wo_mask_sigxcpu () bind(C)
       import
     end function wo_mask_sigxcpu
  end interface
  interface
     integer(c_int) function wo_mask_sigxfsz () bind(C)
       import
     end function wo_mask_sigxfsz
  end interface

  interface
     integer(c_int) function wo_release_sigint () bind(C)
       import
     end function wo_release_sigint
  end interface
  interface
     integer(c_int) function wo_release_sigterm () bind(C)
       import
     end function wo_release_sigterm
  end interface
  interface
     integer(c_int) function wo_release_sigxcpu () bind(C)
       import
     end function wo_release_sigxcpu
  end interface
  interface
     integer(c_int) function wo_release_sigxfsz () bind(C)
       import
     end function wo_release_sigxfsz
  end interface


contains

  elemental function d_area_of_string (string) result (i)
    integer :: i
    type(string_t), intent(in) :: string
    select case (char (string))
    case ("all")
       i = D_ALL
    case ("particles")
       i = D_PARTICLES
    case ("events")
       i = D_EVENTS
    case ("shower")
       i = D_SHOWER
    case ("model_features")
       i = D_MODEL_F
    case ("matching")
       i = D_MATCHING
    case ("transforms")
       i = D_TRANSFORMS
    case ("subtraction")
       i = D_SUBTRACTION
    case ("virtual")
       i = D_VIRTUAL
    case ("threshold")
       i = D_THRESHOLD
    case default
       i = D_ALL
    end select
  end function d_area_of_string

  elemental function d_area_to_string (i) result (string)
    type(string_t) :: string
    integer, intent(in) :: i
    select case (i)
    case (D_ALL)
       string = "all"
    case (D_PARTICLES)
       string = "particles"
    case (D_EVENTS)
       string = "events"
    case (D_SHOWER)
       string = "shower"
    case (D_MODEL_F)
       string = "model_features"
    case (D_MATCHING)
       string = "matching"
    case (D_TRANSFORMS)
       string = "transforms"
    case (D_SUBTRACTION)
       string = "subtraction"
    case (D_VIRTUAL)
       string = "virtual"
    case (D_THRESHOLD)
       string = "threshold"
    case default
       string = "undefined"
    end select
  end function d_area_to_string

  subroutine msg_add (level)
    integer, intent(in) :: level
    type(string_list), pointer :: message
    select case (level)
    case (TERMINATE:WARNING)
       allocate (message)
       message%string = msg_buffer
       nullify (message%next)
       if (.not.associated (msg_list(level)%first)) &
            & msg_list(level)%first => message
       if (associated (msg_list(level)%last)) &
            & msg_list(level)%last%next => message
       msg_list(level)%last => message
       msg_count(level) = msg_count(level) + 1
    end select
  end subroutine msg_add

  subroutine msg_list_clear
    integer :: level
    type(string_list), pointer :: message
    do level = TERMINATE, WARNING
       do while (associated (msg_list(level)%first))
          message => msg_list(level)%first
          msg_list(level)%first => message%next
          deallocate (message)
       end do
       nullify (msg_list(level)%last)
    end do
    msg_count = 0
  end subroutine msg_list_clear

  subroutine msg_summary (unit)
    integer, intent(in), optional :: unit
    call expect_summary (unit)
1   format (A,1x,I2,1x,A,I2,1x,A)
    if (msg_count(ERROR) > 0 .and. msg_count(WARNING) > 0) then
       write (msg_buffer, 1) "There were", &
            & msg_count(ERROR), "error(s) and  ", &
            & msg_count(WARNING), "warning(s)."
       call msg_message (unit=unit)
    else if (msg_count(ERROR) > 0) then
       write (msg_buffer, 1) "There were", &
            & msg_count(ERROR), "error(s) and no warnings."
       call msg_message (unit=unit)
    else if (msg_count(WARNING) > 0) then
       write (msg_buffer, 1) "There were no errors and  ", &
            & msg_count(WARNING), "warning(s)."
       call msg_message (unit=unit)
    end if
  end subroutine msg_summary

  subroutine msg_listing (level, unit, prefix)
    integer, intent(in) :: level
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: prefix
    type(string_list), pointer :: message
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    if (present (unit))  u = unit
    message => msg_list(level)%first
    do while (associated (message))
       if (present (prefix)) then
          write (u, "(A)") prefix // trim (message%string)
       else
          write (u, "(A)") trim (message%string)
       end if
       message => message%next
    end do
    flush (u)
  end subroutine msg_listing

  subroutine buffer_clear
    msg_buffer = " "
  end subroutine buffer_clear

  subroutine message_print (level, string, str_arr, unit, logfile, area)
    integer, intent(in) :: level
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: str_arr
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: logfile
    integer, intent(in), optional :: area
    type(string_t) :: prep_string, aux_string, head_footer, app_string
    integer :: lu, i, ar
    logical :: severe, is_error
    ar = D_ALL; if (present (area))  ar = area
    severe = .false.
    head_footer  = "******************************************************************************"
    aux_string = ""
    is_error = .false.
    app_string = ""
    select case (level)
    case (TERMINATE)
       prep_string = ""
    case (BUG)
       prep_string   = "*** WHIZARD BUG: "
       aux_string    = "***              "
       severe = .true.
       is_error = .true.
    case (FATAL)
       prep_string   = "*** FATAL ERROR: "
       aux_string    = "***              "                     
       severe = .true.
       is_error = .true.
    case (ERROR)
       prep_string = "*** ERROR: "
       aux_string  = "***        "
       is_error = .true.
    case (WARNING)
       prep_string = "Warning: "
    case (MESSAGE)
       prep_string = "| "
    case (DEBUG, DEBUG2)
       prep_string = achar(27) // "[34mD: "
       app_string = achar(27) // "[0m"
    case default
       prep_string = ""
    end select
    if (present(string))  msg_buffer = string
    lu = log_unit
    if (present(unit)) then
       if (unit /= output_unit) then
          if (severe) write (unit, "(A)") char(head_footer) 
          if (is_error) write (unit, "(A)") char(head_footer) 
          write (unit, "(A,A,A)") char(prep_string), trim(msg_buffer), &
               char(app_string)
          if (present (str_arr)) then
             do i = 1, size(str_arr)
                write (unit, "(A,A)") char(aux_string), char(trim(str_arr(i)))
             end do
          end if
          if (is_error) write (unit, "(A)") char(head_footer) 
          if (severe) write (unit, "(A)") char(head_footer)
          flush (unit)
          lu = -1
       else if (level <= msg_level(ar)) then
          if (severe) print "(A)", char(head_footer) 
          if (is_error) print "(A)", char(head_footer) 
          print "(A,A,A)", char(prep_string), trim(msg_buffer), &
               char(app_string)
          if (present (str_arr)) then
             do i = 1, size(str_arr)
                print "(A,A)", char(aux_string), char(trim(str_arr(i)))
             end do                
          end if
          if (is_error) print "(A)", char(head_footer) 
          if (severe) print "(A)", char(head_footer)
          flush (output_unit)
          if (unit == log_unit)  lu = -1
       end if
    else if (level <= msg_level(ar)) then
       if (severe) print "(A)", char(head_footer) 
       if (is_error) print "(A)", char(head_footer) 
       print "(A,A,A)", char(prep_string), trim(msg_buffer), &
               char(app_string)
          if (present (str_arr)) then
             do i = 1, size(str_arr)
                print "(A,A)", char(aux_string), char(trim(str_arr(i)))
             end do                
          end if
       if (is_error) print "(A)", char(head_footer) 
       if (severe) print "(A)", char(head_footer) 
       flush (output_unit)
    end if
    if (present (logfile)) then
       if (.not. logfile)  lu = -1
    end if
    if (logging .and. lu >= 0) then
       if (severe) write (lu, "(A)") char(head_footer) 
       if (is_error) write (lu, "(A)") char(head_footer) 
       write (lu, "(A,A,A)")  char(prep_string), trim(msg_buffer), &
               char(app_string)
       if (present (str_arr)) then
          do i = 1, size(str_arr)
             write (lu, "(A,A)") char(aux_string), char(trim(str_arr(i)))
          end do                
       end if
       if (is_error) write (lu, "(A)") char(head_footer) 
       if (severe) write (lu, "(A)") char(head_footer) 
       flush (lu)
    end if
    call msg_add (level)
    call buffer_clear
  end subroutine message_print

  subroutine msg_terminate (string, unit, quit_code)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    integer, intent(in), optional :: quit_code
    integer(c_int) :: return_code
    call release_term_signals ()
    if (present (quit_code)) then
       return_code = quit_code
    else
       return_code = 0
    end if
    if (present (string)) &
         call message_print (MESSAGE, string, unit=unit)
    call msg_summary (unit)
    if (return_code == 0 .and. expect_failures /= 0) then
       return_code = 5
       call message_print (MESSAGE, &
            "WHIZARD run finished with 'expect' failure(s).", unit=unit)
    else if (return_code == 7) then
       call message_print (MESSAGE, &
            "WHIZARD run finished with failed self-test.", unit=unit)
    else
       call message_print (MESSAGE, "WHIZARD run finished.", unit=unit)
    end if
    call message_print (0, &
         "|=============================================================================|", unit=unit)
    call logfile_final ()
    call msg_list_clear ()
    if (return_code /= 0) then
       call exit (return_code)
    else 
       !!! Should implement WHIZARD exit code (currently only via C)
       ! stop
       call exit (0)
    end if
  end subroutine msg_terminate

  subroutine msg_bug (string, arr, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    logical, pointer :: crash_ptr
    call message_print (BUG, string, arr, unit)
    call msg_summary (unit)
    select case (handle_fatal_errors)
    case (TERM_EXIT)
       call message_print (TERMINATE, "WHIZARD run aborted.", unit=unit)
       call exit (-1_c_int)
    case (TERM_CRASH)
       print *, "*** Intentional crash ***"
       crash_ptr => null ()
       print *, crash_ptr
    end select
    stop "WHIZARD run aborted."
  end subroutine msg_bug

  recursive subroutine msg_fatal (string, arr, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    logical, pointer :: crash_ptr
    if (mask_fatal_errors) then
       call msg_error (string, arr, unit)
    else
       call message_print (FATAL, string, arr, unit)
       call msg_summary (unit)
       select case (handle_fatal_errors)
       case (TERM_EXIT)
          call message_print (TERMINATE, "WHIZARD run aborted.", unit=unit)
          call exit (1_c_int)
       case (TERM_CRASH)
          print *, "*** Intentional crash ***"
          crash_ptr => null ()
          print *, crash_ptr
       end select
       stop "WHIZARD run aborted."
    end if
  end subroutine msg_fatal

  subroutine msg_error (string, arr, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    call message_print (ERROR, string, arr, unit)
    if (msg_count(ERROR) >= MAX_ERRORS) then
       mask_fatal_errors = .false.
       call msg_fatal (" Too many errors encountered.")
    else if (.not.present(unit) .and. .not.mask_fatal_errors)  then
       call message_print (MESSAGE, "            (WHIZARD run continues)")
    end if
  end subroutine msg_error

  subroutine msg_warning (string, arr, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    call message_print (WARNING, string, arr, unit)
  end subroutine msg_warning

  subroutine msg_message (string, unit, arr, logfile)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    logical, intent(in), optional :: logfile
    call message_print (MESSAGE, string, arr, unit, logfile)
  end subroutine msg_message

  subroutine msg_result (string, arr, unit, logfile)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    type(string_t), dimension(:), intent(in), optional :: arr
    logical, intent(in), optional :: logfile
    call message_print (RESULT, string, arr, unit, logfile)
  end subroutine msg_result

  subroutine msg_debug_none (area, string)
    integer, intent(in) :: area
    character(len=*), intent(in), optional :: string
    call message_print (DEBUG, string, unit=output_unit, &
         area=area, logfile=.false.)
  end subroutine msg_debug_none

  subroutine msg_debug_logical (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    logical, intent(in) :: value
    call msg_debug_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug_logical

  subroutine msg_debug_integer (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    integer, intent(in) :: value
    call msg_debug_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug_integer

  subroutine msg_debug_real (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    real(default), intent(in) :: value
    call msg_debug_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug_real

  subroutine msg_debug2_none (area, string)
    integer, intent(in) :: area
    character(len=*), intent(in), optional :: string
    call message_print (DEBUG2, string, unit=output_unit, &
         area=area, logfile=.false.)
  end subroutine msg_debug2_none

  subroutine msg_debug2_logical (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    logical, intent(in) :: value
    call msg_debug2_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug2_logical

  subroutine msg_debug2_integer (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    integer, intent(in) :: value
    call msg_debug2_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug2_integer

  subroutine msg_debug2_real (area, string, value)
    integer, intent(in) :: area
    character(len=*), intent(in) :: string
    real(default), intent(in) :: value
    call msg_debug2_none (area, char (string // " = " // str (value)))
  end subroutine msg_debug2_real

  elemental function debug_active (area) result (active)
    logical :: active
    integer, intent(in) :: area
    active = msg_level(area) >= DEBUG
  end function debug_active

  elemental function debug2_active (area) result (active)
    logical :: active
    integer, intent(in) :: area
    active = msg_level(area) >= DEBUG2
  end function debug2_active

  subroutine msg_show_progress (i_call, n_calls)
    integer, intent(in) :: i_call, n_calls
    real(default) :: progress
    integer, save :: next_check
    if (i_call == 1) next_check = 10
    progress = (i_call * 100._default) / n_calls
    if (progress >= next_check) then
       write (msg_buffer, "(F5.1,A)") progress, "%"
       call msg_message ()
       next_check = next_check + 10
    end if
  end subroutine msg_show_progress

  subroutine msg_banner (unit)
    integer, intent(in), optional :: unit
    call message_print (0, "|=============================================================================|", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|    WW             WW  WW   WW  WW  WWWWWW      WW      WWWWW    WWWW        |", unit=unit)
    call message_print (0, "|     WW    WW     WW   WW   WW  WW     WW      WWWW     WW  WW   WW  WW      |", unit=unit)
    call message_print (0, "|      WW  WW WW  WW    WWWWWWW  WW    WW      WW  WW    WWWWW    WW   WW     |", unit=unit)
    call message_print (0, "|       WWWW   WWWW     WW   WW  WW   WW      WWWWWWWW   WW  WW   WW  WW      |", unit=unit)
    call message_print (0, "|        WW     WW      WW   WW  WW  WWWWWW  WW      WW  WW   WW  WWWW        |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|                                        W                                    |", unit=unit)
    call message_print (0, "|                                       sW                                    |", unit=unit)
    call message_print (0, "|                                       WW                                    |", unit=unit)
    call message_print (0, "|                                      sWW                                    |", unit=unit)
    call message_print (0, "|                                      WWW                                    |", unit=unit)
    call message_print (0, "|                                     wWWW                                    |", unit=unit)
    call message_print (0, "|                                    wWWWW                                    |", unit=unit)
    call message_print (0, "|                                    WW WW                                    |", unit=unit)
    call message_print (0, "|                                    WW WW                                    |", unit=unit)
    call message_print (0, "|                                   wWW WW                                    |", unit=unit)
    call message_print (0, "|                                  wWW  WW                                    |", unit=unit)
    call message_print (0, "|                                  WW   WW                                    |", unit=unit)
    call message_print (0, "|                                  WW   WW                                    |", unit=unit)
    call message_print (0, "|                                 WW    WW                                    |", unit=unit)
    call message_print (0, "|                                 WW    WW                                    |", unit=unit)
    call message_print (0, "|                                WW     WW                                    |", unit=unit)
    call message_print (0, "|                                WW     WW                                    |", unit=unit)
    call message_print (0, "|           wwwwww              WW      WW                                    |", unit=unit)
    call message_print (0, "|              WWWWWww          WW      WW                                    |", unit=unit)
    call message_print (0, "|                 WWWWWwwwww   WW       WW                                    |", unit=unit)
    call message_print (0, "|                     wWWWwwwwwWW       WW                                    |", unit=unit)
    call message_print (0, "|                 wWWWWWWWWWWwWWW       WW                                    |", unit=unit)
    call message_print (0, "|                wWWWWW       wW        WWWWWWW                               |", unit=unit)
    call message_print (0, "|                  WWWW       wW        WW  wWWWWWWWwww                       |", unit=unit)
    call message_print (0, "|                   WWWW                      wWWWWWWWwwww                    |", unit=unit)
    call message_print (0, "|                     WWWW                      WWWW     WWw                  |", unit=unit)
    call message_print (0, "|                       WWWWww                   WWWW                         |", unit=unit)
    call message_print (0, "|                           WWWwwww              WWWW                         |", unit=unit)
    call message_print (0, "|                               wWWWWwww       wWWWWW                         |", unit=unit)
    call message_print (0, "|                                     WwwwwwwwwWWW                            |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|  by:   Wolfgang Kilian, Thorsten Ohl, Juergen Reuter                        |", unit=unit)
    call message_print (0, "|        with contributions from Christian Speckner                           |", unit=unit)
    call message_print (0, "|        Contact: <whizard@desy.de>                                           |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|  if you use WHIZARD please cite:                                            |", unit=unit)   
    call message_print (0, "|        W. Kilian, T. Ohl, J. Reuter,  Eur.Phys.J.C71 (2011) 1742            |", unit=unit)
    call message_print (0, "|                                          [arXiv: 0708.4233 [hep-ph]]        |", unit=unit)   
    call message_print (0, "|        M. Moretti, T. Ohl, J. Reuter, arXiv: hep-ph/0102195                 |", unit=unit)
    call message_print (0, "|                                                                             |", unit=unit)
    call message_print (0, "|=============================================================================|", unit=unit)
    call message_print (0, "|                               WHIZARD " // WHIZARD_VERSION, unit=unit)
    call message_print (0, "|=============================================================================|", unit=unit)
  end subroutine msg_banner

  subroutine logfile_init (filename)
    type(string_t), intent(in) :: filename
    call msg_message ("Writing log to '" // char (filename) // "'")
    if (.not. logging)  call msg_message ("(Logging turned off.)")
    log_unit = free_unit ()
    open (file = char (filename), unit = log_unit, &
          action = "write", status = "replace")
  end subroutine logfile_init

  subroutine logfile_final ()
    if (log_unit >= 0) then
       close (log_unit)
       log_unit = -1
    end if
  end subroutine logfile_final

  function logfile_unit (unit, logfile)
    integer :: logfile_unit
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: logfile
    if (logging) then
       if (present (unit)) then
          if (unit == output_unit) then
             logfile_unit = log_unit
          else
             logfile_unit = -1
          end if
       else if (present (logfile)) then
          if (logfile) then
             logfile_unit = log_unit
          else
             logfile_unit = -1
          end if
       else
          logfile_unit = log_unit
       end if
    else
       logfile_unit = -1
    end if
  end function logfile_unit

  subroutine expect_record (success)
    logical, intent(in) :: success
    expect_total = expect_total + 1
    if (.not. success)  expect_failures = expect_failures + 1
  end subroutine expect_record

  subroutine expect_clear ()
    expect_total = 0
    expect_failures = 0
  end subroutine expect_clear

  subroutine expect_summary (unit, force)
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: force
    logical :: force_output
    force_output = .false.;  if (present (force))  force_output = force
    if (expect_total /= 0 .or. force_output) then
       call msg_message ("Summary of value checks:", unit)
       write (msg_buffer, "(2x,A,1x,I0,1x,A,1x,A,1x,I0)") &
            "Failures:", expect_failures, "/", "Total:", expect_total
       call msg_message (unit=unit)
    end if
  end subroutine expect_summary

  pure function int2fixed (i) result (c)
    integer, intent(in) :: i
    character(200) :: c
    c = ""
    write (c, *) i
    c = adjustl (c)
  end function int2fixed

  pure function int2string (i) result (s)
    integer, intent(in) :: i
    type (string_t) :: s
    s = trim (int2fixed (i))
  end function int2string

  pure function int2char (i) result (c)
    integer, intent(in) :: i
    character(len (trim (int2fixed (i)))) :: c
    c = int2fixed (i)
  end function int2char

  pure function real2fixed (x, fmt) result (c)
    real(default), intent(in) :: x
    character(*), intent(in), optional :: fmt
    character(200) :: c
    c = ""
    write (c, *) x
    c = adjustl (c)
  end function real2fixed

  pure function real2fixed_fmt (x, fmt) result (c)
    real(default), intent(in) :: x
    character(*), intent(in) :: fmt
    character(200) :: c
    c = ""
    write (c, fmt)  x
    c = adjustl (c)
  end function real2fixed_fmt

  pure function real2string_list (x) result (s)
    real(default), intent(in) :: x
    type(string_t) :: s
    s = trim (real2fixed (x))
  end function real2string_list

  pure function real2string_fmt (x, fmt) result (s)
    real(default), intent(in) :: x
    character(*), intent(in) :: fmt
    type(string_t) :: s
    s = trim (real2fixed_fmt (x, fmt))
  end function real2string_fmt

  pure function real2char_list (x) result (c)
    real(default), intent(in) :: x
    character(len_trim (real2fixed (x))) :: c
    c = real2fixed (x)
  end function real2char_list

  pure function real2char_fmt (x, fmt) result (c)
    real(default), intent(in) :: x
    character(*), intent(in) :: fmt
    character(len_trim (real2fixed_fmt (x, fmt))) :: c
    c = real2fixed_fmt (x, fmt)
  end function real2char_fmt

  elemental subroutine pacify_real_default (x, tolerance)
    real(default), intent(inout) :: x
    real(default), intent(in) :: tolerance
    if (abs (x) < tolerance)  x = 0._default
  end subroutine pacify_real_default
  
  elemental subroutine pacify_complex_default (x, tolerance)
    complex(default), intent(inout) :: x
    real(default), intent(in) :: tolerance
    if (abs (real (x)) < tolerance)   &
         x = cmplx (0._default, aimag (x), kind=default)
    if (abs (aimag (x)) < tolerance)  &
         x = cmplx (real (x), 0._default, kind=default)
  end subroutine pacify_complex_default  

  subroutine mask_term_signals ()
    logical :: ok
    wo_sigint = 0
    ok = wo_mask_sigint () == 0
    if (.not. ok)  call msg_error ("Masking SIGINT failed")
    wo_sigterm = 0
    ok = wo_mask_sigterm () == 0
    if (.not. ok)  call msg_error ("Masking SIGTERM failed")
    wo_sigxcpu = 0
    ok = wo_mask_sigxcpu () == 0
    if (.not. ok)  call msg_error ("Masking SIGXCPU failed")
    wo_sigxfsz = 0
    ok = wo_mask_sigxfsz () == 0
    if (.not. ok)  call msg_error ("Masking SIGXFSZ failed")
  end subroutine mask_term_signals

  subroutine release_term_signals ()
    logical :: ok
    ok = wo_release_sigint () == 0
    if (.not. ok)  call msg_error ("Releasing SIGINT failed")
    ok = wo_release_sigterm () == 0
    if (.not. ok)  call msg_error ("Releasing SIGTERM failed")
    ok = wo_release_sigxcpu () == 0
    if (.not. ok)  call msg_error ("Releasing SIGXCPU failed")
    ok = wo_release_sigxfsz () == 0
    if (.not. ok)  call msg_error ("Releasing SIGXFSZ failed")
  end subroutine release_term_signals

  function signal_is_pending () result (flag)
    logical :: flag
    flag = &
         wo_sigint /= 0 .or. &
         wo_sigterm /= 0 .or. &
         wo_sigxcpu /= 0 .or. &
         wo_sigxfsz /= 0
  end function signal_is_pending
  
  subroutine terminate_now_if_signal ()
    if (wo_sigint /= 0) then
       call msg_terminate ("Signal SIGINT (keyboard interrupt) received.", &
          quit_code=int (wo_sigint))
    else if (wo_sigterm /= 0) then
       call msg_terminate ("Signal SIGTERM (termination signal) received.", &
          quit_code=int (wo_sigterm))
    else if (wo_sigxcpu /= 0) then
       call msg_terminate ("Signal SIGXCPU (CPU time limit exceeded) received.", &
          quit_code=int (wo_sigxcpu))
    else if (wo_sigxfsz /= 0) then
       call msg_terminate ("Signal SIGXFSZ (file size limit exceeded) received.", &
          quit_code=int (wo_sigxfsz))
    end if
  end subroutine terminate_now_if_signal    


end module diagnostics
