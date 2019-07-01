! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use iso_varying_string, string_t => varying_string

  implicit none
  private

  public :: upper_case
  public :: lower_case
  public :: string_f2c

  interface upper_case
     module procedure upper_case_char, upper_case_string
  end interface
  interface lower_case
     module procedure lower_case_char, lower_case_string
  end interface
  interface string_f2c
     module procedure string_f2c_char, string_f2c_var_str
  end interface string_f2c

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


end module string_utils
