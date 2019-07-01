! WHIZARD 2.0.7 Mar 19 2012
! 
! Copyright (C) 1999-2012 by 
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

module file_utils

  use iso_fortran_env, only: stdout => output_unit !NODEP!
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: MIN_UNIT, MAX_UNIT !NODEP!

  implicit none
  private

  public :: free_unit
  public :: output_unit
  public :: upper_case
  public :: lower_case
  public :: quote_underscore
  public :: tex_format
  public :: mp_format

  interface upper_case
     module procedure upper_case_char, upper_case_string
  end interface
  interface lower_case
     module procedure lower_case_char, lower_case_string
  end interface

contains

  function free_unit () result (unit)
    integer :: unit
    logical :: exists, is_open
    integer :: i, status
    do i = MIN_UNIT, MAX_UNIT
       inquire (unit=i, exist=exists, opened=is_open, iostat=status)
       if (status == 0) then
          if (exists .and. .not. is_open) then
            unit = i; return
          end if
       end if
    end do
    unit = -1
  end function free_unit

  function output_unit (unit) result (u)
    integer, intent(in), optional :: unit
    integer :: u
    if (present (unit)) then
       u = unit
    else
       u = stdout
    end if
  end function output_unit

  function upper_case_char (string) result (new_string)
    character(*), intent(in) :: string
    character(len(string)) :: new_string
    integer :: pos, code
    integer, parameter :: offset = ichar('A')-ichar('a')
    do pos = 1, len (string)
       code = ichar (string(pos:pos))
       select case (code)
       case (ichar('a'):ichar('z'))
          new_string(pos:pos) = char (code + offset)
       case default
          new_string(pos:pos) = string(pos:pos)
       end select
    end do
  end function upper_case_char

  function lower_case_char (string) result (new_string)
    character(*), intent(in) :: string
    character(len(string)) :: new_string
    integer :: pos, code
    integer, parameter :: offset = ichar('a')-ichar('A')
    do pos = 1, len (string)
       code = ichar (string(pos:pos))
       select case (code)
       case (ichar('A'):ichar('Z'))
          new_string(pos:pos) = char (code + offset)
       case default
          new_string(pos:pos) = string(pos:pos)
       end select
    end do
  end function lower_case_char

  function upper_case_string (string) result (new_string)
    type(string_t), intent(in) :: string
    type(string_t) :: new_string
    new_string = upper_case_char (char (string))
  end function upper_case_string

  function lower_case_string (string) result (new_string)
    type(string_t), intent(in) :: string
    type(string_t) :: new_string
    new_string = lower_case_char (char (string))
  end function lower_case_string

  function quote_underscore (string) result (quoted)
    type(string_t) :: quoted
    type(string_t), intent(in) :: string
    type(string_t) :: part
    type(string_t) :: buffer
    buffer = string
    quoted = ""
    do
      call split (part, buffer, "_")
      quoted = quoted // part
      if (buffer == "")  exit
      quoted = quoted // "\_"
    end do
  end function quote_underscore

  function tex_format (rval, n_digits) result (string)
    type(string_t) :: string
    real(default), intent(in) :: rval
    integer, intent(in) :: n_digits
    integer :: e, n, w, d
    real(default) :: absval
    real(default) :: mantissa
    character :: sign
    character(20) :: format
    character(80) :: cstr
    n = min (abs (n_digits), 16)
    if (rval == 0) then
       string = "0"
    else
       absval = abs (rval)
       e = log10 (absval)
       if (rval < 0) then
          sign = "-"
       else
          sign = ""
       end if
       select case (e)
       case (:-3)
          d = max (n - 1, 0)
          w = max (d + 2, 2)
          write (format, "('(F',I0,'.',I0,',A,I0,A)')")  w, d
          mantissa = absval * 10._default ** (1 - e)
          write (cstr, fmt=format)  mantissa, "\times 10^{", e - 1, "}"
       case (-2:0)
          d = max (n - e, 1 - e)
          w = max (d + e + 2, d + 2)
          write (format, "('(F',I0,'.',I0,')')")  w, d
          write (cstr, fmt=format)  absval
       case (1:2)
          d = max (n - e - 1, -e, 0)
          w = max (d + e + 2, d + 2, e + 2)
          write (format, "('(F',I0,'.',I0,')')")  w, d
          write (cstr, fmt=format)  absval
       case default
          d = max (n - 1, 0)
          w = max (d + 2, 2)
          write (format, "('(F',I0,'.',I0,',A,I0,A)')")  w, d
          mantissa = absval * 10._default ** (- e)
          write (cstr, fmt=format)  mantissa, "\times 10^{", e, "}"
       end select
       string = sign // trim (cstr)
    end if
  end function tex_format

  function mp_format (rval) result (string)
    type(string_t) :: string
    real(default), intent(in) :: rval
    character(16) :: tmp
    write (tmp, "(G16.8)")  rval
    string = lower_case (trim (adjustl (trim (tmp))))
  end function mp_format


end module file_utils
