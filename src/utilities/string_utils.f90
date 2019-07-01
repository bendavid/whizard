! WHIZARD 2.3.1 Aug 25 2016
! 
! Copyright (C) 1999-2016 by 
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

module string_utils
  
  use, intrinsic :: iso_c_binding

  use kinds, only: default
  use iso_varying_string, string_t => varying_string

  implicit none
  private

  public :: upper_case
  public :: lower_case
  public :: string_f2c
  public :: str
  public :: read_rval
  public :: read_ival
  public :: split_string

  interface upper_case
     module procedure upper_case_char, upper_case_string
  end interface
  interface lower_case
     module procedure lower_case_char, lower_case_string
  end interface
  interface string_f2c
     module procedure string_f2c_char, string_f2c_var_str
  end interface string_f2c
  interface str
     module procedure str_log, str_logs, str_int, str_ints, &
            str_real, str_reals, str_complex, str_complexs
  end interface

contains

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

  pure function string_f2c_char (i) result (o)
    character(*), intent(in) :: i
    character(kind=c_char, len=len (i) + 1) :: o
    o = i // c_null_char
  end function string_f2c_char

  pure function string_f2c_var_str (i) result (o)
    type(string_t), intent(in) :: i
    character(kind=c_char, len=len (i) + 1) :: o
    o = char (i) // c_null_char
  end function string_f2c_var_str

  function str_log (l) result (s)
    logical, intent(in) :: l
    type(string_t) :: s
    if (l) then
       s = "True"
    else
       s = "False"
    end if
  end function str_log

  function str_logs (x) result (s)
    logical, dimension(:), intent(in) :: x
    type(string_t) :: s
    integer :: i
    s = '['
    do i = 1, size(x) - 1
       s = s // str(x(i)) // ', '
    end do
    s = s // str(x(size(x))) // ']'
  end function str_logs

  function str_int (i) result (s)
    integer, intent(in) :: i
    type(string_t) :: s
    character(32) :: buffer
    write (buffer, "(I0)")  i
    s = var_str (trim (adjustl (buffer)))
  end function str_int

  function str_ints (x) result (s)
    integer, dimension(:), intent(in) :: x
    type(string_t) :: s
    integer :: i
    s = '['
    do i = 1, size(x) - 1
       s = s // str(x(i)) // ', '
    end do
    s = s // str(x(size(x))) // ']'
  end function str_ints

  function str_real (x) result (s)
    real(default), intent(in) :: x
    type(string_t) :: s
    character(32) :: buffer
    write (buffer, "(ES17.10)")  x
    s = var_str (trim (adjustl (buffer)))
  end function str_real

  function str_reals (x) result (s)
    real(default), dimension(:), intent(in) :: x
    type(string_t) :: s
    integer :: i
    s = '['
    do i = 1, size(x) - 1
       s = s // str(x(i)) // ', '
    end do
    s = s // str(x(size(x))) // ']'
  end function str_reals

  function str_complex (x) result (s)
    complex(default), intent(in) :: x
    type(string_t) :: s
    s = str_real (real (x)) // " + i " // str_real (aimag (x))
  end function str_complex

  function str_complexs (x) result (s)
    complex(default), dimension(:), intent(in) :: x
    type(string_t) :: s
    integer :: i
    s = '['
    do i = 1, size(x) - 1
       s = s // str(x(i)) // ', '
    end do
    s = s // str(x(size(x))) // ']'
  end function str_complexs

  function read_rval (s) result (rval)
    type(string_t), intent(in) :: s
    real(default) :: rval
    character(80) :: buffer
    buffer = s
    read (buffer, *)  rval
  end function read_rval
    
  function read_ival (s) result (ival)
    type(string_t), intent(in) :: s
    integer :: ival
    character(80) :: buffer
    buffer = s
    read (buffer, *)  ival
  end function read_ival
    
  function split_string (str, separator) result (str_array)
    type(string_t), dimension(:), allocatable :: str_array
    type(string_t), intent(in) :: str, separator
    type(string_t) :: str_tmp, str_out
    integer :: n_str
    n_str = 0; str_tmp = str
    do while (contains_word (str_tmp, separator))
       n_str = n_str + 1
       call split (str_tmp, str_out, separator)
    end do
    allocate (str_array (n_str))
    n_str = 1; str_tmp = str
    do while (contains_word (str_tmp, separator))
       call split (str_tmp, str_array (n_str), separator)
       n_str = n_str + 1
    end do
  contains
    function contains_word (str, word) result (val)
      logical :: val
      type(string_t), intent(in) :: str, word
      type(string_t) :: str_tmp, str_out
      str_tmp = str
      call split (str_tmp, str_out, word)
      val = str_out /= "" 
    end function contains_word
  end function split_string      


end module string_utils
