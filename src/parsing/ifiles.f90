! WHIZARD 2.4.0 Nov 28 2016
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
!     So Young Shim <soyoung.shim@desy.de>
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

module ifiles

  use iso_varying_string, string_t => varying_string
  use io_units
  use system_defs, only: EOF

  implicit none
  private

  public :: ifile_t
  public :: ifile_clear
  public :: ifile_final
  public :: ifile_read
  public :: ifile_append
  public :: ifile_write
  public :: ifile_to_string_array
  public :: ifile_get_length
  public :: line_p
  public :: line_init
  public :: line_final
  public :: line_advance
  public :: line_backspace
  public :: line_is_associated
  public :: line_get_string
  public :: line_get_string_advance
  public :: line_get_index
  public :: line_get_length

  type :: line_entry_t
     private
     type(line_entry_t), pointer :: previous => null ()
     type(line_entry_t), pointer :: next => null ()
     type(string_t) :: string
     integer :: index
  end type line_entry_t

  type :: ifile_t
     private
     type(line_entry_t), pointer :: first => null ()
     type(line_entry_t), pointer :: last => null ()
     integer :: n_lines = 0
   contains
     procedure :: final => ifile_clear
     generic :: append => &
          ifile_append_from_char
     procedure, private :: ifile_append_from_char
  end type ifile_t

  type :: line_p
     private
     type(line_entry_t), pointer :: p => null ()
  end type line_p


  interface ifile_final
     module procedure ifile_clear
  end interface
  interface ifile_read
     module procedure ifile_read_from_string
     module procedure ifile_read_from_char
     module procedure ifile_read_from_unit
     module procedure ifile_read_from_char_array
     module procedure ifile_read_from_ifile
  end interface
  interface ifile_append
     module procedure ifile_append_from_string
     module procedure ifile_append_from_char
     module procedure ifile_append_from_unit
     module procedure ifile_append_from_char_array
     module procedure ifile_append_from_ifile
  end interface

contains

  subroutine line_entry_create (line, string)
    type(line_entry_t), pointer :: line
    type(string_t), intent(in) :: string
    allocate (line)
    line%string = string
  end subroutine line_entry_create

  subroutine line_entry_destroy (line)
    type(line_entry_t), pointer :: line
    deallocate (line)
  end subroutine line_entry_destroy

  subroutine ifile_clear (ifile)
    class(ifile_t), intent(inout) :: ifile
    type(line_entry_t), pointer :: current
    do while (associated (ifile%first))
       current => ifile%first
       ifile%first => current%next
       call line_entry_destroy (current)
    end do
    nullify (ifile%last)
    ifile%n_lines = 0
  end subroutine ifile_clear

  subroutine ifile_read_from_string (ifile, string)
    type(ifile_t), intent(inout) :: ifile
    type(string_t), intent(in) :: string
    call ifile_clear (ifile)
    call ifile_append (ifile, string)
  end subroutine ifile_read_from_string

  subroutine ifile_read_from_char (ifile, char)
    type(ifile_t), intent(inout) :: ifile
    character(*), intent(in) :: char
    call ifile_clear (ifile)
    call ifile_append (ifile, char)
  end subroutine ifile_read_from_char

  subroutine ifile_read_from_char_array (ifile, char)
    type(ifile_t), intent(inout) :: ifile
    character(*), dimension(:), intent(in) :: char
    call ifile_clear (ifile)
    call ifile_append (ifile, char)
  end subroutine ifile_read_from_char_array
    
  subroutine ifile_read_from_unit (ifile, unit, iostat)
    type(ifile_t), intent(inout) :: ifile
    integer, intent(in) :: unit
    integer, intent(out), optional :: iostat
    call ifile_clear (ifile)
    call ifile_append (ifile, unit, iostat)
  end subroutine ifile_read_from_unit
    
  subroutine ifile_read_from_ifile (ifile, ifile_in)
    type(ifile_t), intent(inout) :: ifile
    type(ifile_t), intent(in) :: ifile_in
    call ifile_clear (ifile)
    call ifile_append (ifile, ifile_in)
  end subroutine ifile_read_from_ifile
    
  subroutine ifile_append_from_string (ifile, string)
    class(ifile_t), intent(inout) :: ifile
    type(string_t), intent(in) :: string
    type(line_entry_t), pointer :: current
    call line_entry_create (current, string)
    current%index = ifile%n_lines + 1
    if (associated (ifile%last)) then
       current%previous => ifile%last
       ifile%last%next => current
    else
       ifile%first => current
    end if
    ifile%last => current
    ifile%n_lines = current%index
  end subroutine ifile_append_from_string

  subroutine ifile_append_from_char (ifile, char)
    class(ifile_t), intent(inout) :: ifile
    character(*), intent(in) :: char
    call ifile_append_from_string (ifile, var_str (trim (char)))
  end subroutine ifile_append_from_char

  subroutine ifile_append_from_char_array (ifile, char)
    class(ifile_t), intent(inout) :: ifile
    character(*), dimension(:), intent(in) :: char
    integer :: i
    do i = 1, size (char)
       call ifile_append_from_string (ifile, var_str (trim (char(i))))
    end do
  end subroutine ifile_append_from_char_array
    
  subroutine ifile_append_from_unit (ifile, unit, iostat)
    class(ifile_t), intent(inout) :: ifile
    integer, intent(in) :: unit
    integer, intent(out), optional :: iostat
    type(string_t) :: buffer
    integer :: ios
    ios = 0
    READ_LOOP: do
       call get (unit, buffer, iostat = ios)
       if (ios == EOF .or. ios > 0)  exit READ_LOOP
       call ifile_append_from_string (ifile, buffer)
    end do READ_LOOP
    if (present (iostat)) then
       iostat = ios
    else if (ios > 0) then
       call get (unit, buffer)  ! trigger error again
    end if
  end subroutine ifile_append_from_unit
    
  subroutine ifile_append_from_ifile (ifile, ifile_in)
    class(ifile_t), intent(inout) :: ifile
    type(ifile_t), intent(in) :: ifile_in
    type(line_entry_t), pointer :: current
    current => ifile_in%first
    do while (associated (current))
       call ifile_append_from_string (ifile, current%string)
       current => current%next
    end do
  end subroutine ifile_append_from_ifile
    
  subroutine ifile_write (ifile, unit, iostat)
    type(ifile_t), intent(in) :: ifile
    integer, intent(in), optional :: unit
    integer, intent(out), optional :: iostat
    integer :: u
    type(line_entry_t), pointer :: current
    u = given_output_unit (unit);  if (u < 0)  return
    current => ifile%first
    do while (associated (current))
       call put_line (u, current%string, iostat)
       current => current%next
    end do
  end subroutine ifile_write

  subroutine ifile_to_string_array (ifile, string)
    type(ifile_t), intent(in) :: ifile
    type(string_t), dimension(:), intent(inout), allocatable :: string
    type(line_entry_t), pointer :: current
    integer :: i
    allocate (string (ifile_get_length (ifile)))
    current => ifile%first
    do i = 1, ifile_get_length (ifile)
       string(i) = current%string
       current => current%next
    end do
  end subroutine ifile_to_string_array

  function ifile_get_length (ifile) result (length)
    integer :: length
    type(ifile_t), intent(in) :: ifile
    length = ifile%n_lines
  end function ifile_get_length

  subroutine line_init (line, ifile, back)
    type(line_p), intent(inout) :: line
    type(ifile_t), intent(in) :: ifile
    logical, intent(in), optional :: back
    if (present (back)) then
       if (back) then
          line%p => ifile%last
       else
          line%p => ifile%first
       end if
    else
       line%p => ifile%first
    end if
  end subroutine line_init

  subroutine line_final (line)
    type(line_p), intent(inout) :: line
    nullify (line%p)
  end subroutine line_final

  subroutine line_advance (line)
    type(line_p), intent(inout) :: line
    if (associated (line%p))  line%p => line%p%next
  end subroutine line_advance

  subroutine line_backspace (line)
    type(line_p), intent(inout) :: line
    if (associated (line%p))  line%p => line%p%previous
  end subroutine line_backspace

  function line_is_associated (line) result (ok)
    logical :: ok
    type(line_p), intent(in) :: line
    ok = associated (line%p)
  end function line_is_associated

  function line_get_string (line) result (string)
    type(string_t) :: string
    type(line_p), intent(in) :: line
    if (associated (line%p)) then
       string = line%p%string
    else
       string = ""
    end if
  end function line_get_string

  function line_get_string_advance (line) result (string)
    type(string_t) :: string
    type(line_p), intent(inout) :: line
    if (associated (line%p)) then
       string = line%p%string
       call line_advance (line)
    else
       string = ""
    end if
  end function line_get_string_advance

  function line_get_index (line) result (index)
    integer :: index
    type(line_p), intent(in) :: line
    if (associated (line%p)) then
       index = line%p%index
    else
       index = 0
    end if
  end function line_get_index

  function line_get_length (line) result (length)
    integer :: length
    type(line_p), intent(in) :: line
    if (associated (line%p)) then
       length = len (line%p%string)
    else
       length = 0
    end if
  end function line_get_length


end module ifiles
