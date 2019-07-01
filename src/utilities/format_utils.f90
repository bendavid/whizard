! WHIZARD 2.2.6 May 02 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module format_utils

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use string_utils, only: lower_case

  implicit none
  private

  public :: write_separator
  public :: write_indent
  public :: quote_underscore
  public :: tex_format
  public :: mp_format
  public :: pac_fmt
  public :: write_compressed_integer_array

contains

  subroutine write_separator (u, mode)
    integer, intent(in) :: u
    integer, intent(in), optional :: mode
    integer :: m
    m = 1;  if (present (mode))  m = mode
    select case (m)
    case default
       write (u, "(A)")  repeat ("-", 72)
    case (1)
       write (u, "(A)")  repeat ("-", 72)
    case (2)
       write (u, "(A)")  repeat ("=", 72)
    end select
  end subroutine write_separator

  subroutine write_indent (unit, indent)
    integer, intent(in) :: unit
    integer, intent(in), optional :: indent
    if (present (indent)) then
       write (unit, "(1x,A)", advance="no")  repeat ("  ", indent)
    end if
  end subroutine write_indent

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
       e = int (log10 (absval))
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

  subroutine pac_fmt (fmt, fmt_orig, fmt_pac, pacify)
    character(*), intent(in) :: fmt_orig, fmt_pac
    character(*), intent(out) :: fmt
    logical, intent(in), optional :: pacify
    logical :: pacified
    pacified = .false.
    if (present (pacify))  pacified = pacify
    if (pacified) then
       fmt = fmt_pac
    else
       fmt = fmt_orig
    end if
  end subroutine pac_fmt

  subroutine write_compressed_integer_array (chars, array)
    character(len=*), intent(out) :: chars
    integer, intent(in), allocatable, dimension(:) :: array
    logical, dimension(:), allocatable :: used
    character(len=16) :: tmp
    type(string_t) :: string
    integer :: i, j, start_chain, end_chain
    chars = '[none]'
    string = ""
    if (allocated (array)) then
       if (size (array) > 0) then
          allocate (used (size (array)))
          used = .false.
          do i = 1, size (array)
             if (.not. used(i)) then
                start_chain = array(i)
                end_chain = array(i)
                used(i) = .true.
                EXTEND: do
                   do j = 1, size (array)
                      if (array(j) == end_chain + 1) then
                         end_chain = array(j)
                         used(j) = .true.
                         cycle EXTEND
                      end if
                      if (array(j) == start_chain - 1) then
                         start_chain = array(j)
                         used(j) = .true.
                         cycle EXTEND
                      end if
                   end do
                   exit
                end do EXTEND
                if (end_chain - start_chain > 0) then
                   write (tmp, "(I0,A,I0)") start_chain, "-", end_chain
                else
                   write (tmp, "(I0)") start_chain
                end if
                string = string // trim (tmp)
                if (any (.not. used)) then
                   string = string // ','
                end if
             end if
          end do
          chars = string
       end if
    end if
    chars = adjustr (chars)
  end subroutine write_compressed_integer_array


end module format_utils
