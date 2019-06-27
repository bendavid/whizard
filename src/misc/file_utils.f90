! WHIZARD 2.0.2 Tue May 18 2010
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

module file_utils

  use iso_fortran_env, only: stdout => output_unit !NODEP!
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: MIN_UNIT, MAX_UNIT !NODEP!
  use limits, only: DEFAULT_FILENAME, FILENAME_LEN !NODEP!

  implicit none
  private

  public :: free_unit
  public :: output_unit
  public :: flush_all
  public :: choose_filename
  public :: file_exists_else_default
  public :: upper_case
  public :: lower_case
  public :: tex_format

  interface concat
     module procedure concat_two, concat_three
  end interface
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

 subroutine flush_all ()
   integer :: u
   do u = MIN_UNIT, MAX_UNIT
      flush (u)
   end do
 end subroutine flush_all

  function choose_filename (file, default_file) result (chosen_file)
    character(len=*), intent(in) :: file, default_file
    character(len=FILENAME_LEN) :: chosen_file
    if (len_trim (file) == 0) then
       chosen_file = default_file
    else
       chosen_file = file
    end if
  end function choose_filename

  function concat_two (prefix, filename, extension) result (file)
    character(len=*) :: prefix, filename, extension
    character(len=FILENAME_LEN) :: file
    file = trim(filename)//"."//trim(extension)
    if (prefix /= "" .and. filename(1:1) /= "/" .and. filename(1:2) /= "./") &
         & file = trim(prefix) // "/" // file
  end function concat_two

  function concat_three (prefix, filename, process_id, extension) result (file)
    character(len=*) :: prefix, filename, process_id, extension
    character(len=FILENAME_LEN) :: file
    file = trim(filename)//"."//trim(process_id)//"."//trim(extension)
    if (prefix /= "" .and. filename(1:1) /= "/" .and. filename(1:2) /= "./") &
         & file = trim(prefix) // "/" // file
  end function concat_three

  function file_exists_else_default (prefix, filename, extension) result (file)
    character(len=*) :: prefix, filename, extension
    character(len=FILENAME_LEN) :: file
    logical :: exist
    file = concat (prefix, filename, extension)
    inquire (file=trim(file), exist=exist)
    if (.not.exist) then
       file = concat (prefix, DEFAULT_FILENAME, extension)
       inquire (file=trim(file), exist=exist)
       if (.not.exist)  then
          file = ""
       end if
    end if
  end function file_exists_else_default

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


end module file_utils
