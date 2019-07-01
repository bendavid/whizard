! WHIZARD 2.2.4 Feb 06 2015
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

module loop_archive

  use io_units
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use os_interface

  public :: loop_archive_t

  type :: loop_archive_t
    logical :: active = .false.
    type(string_t) :: name
    type(string_t) :: current_prefix
  contains
    procedure :: activate => loop_archive_activate
    procedure :: record => loop_archive_record
    procedure :: search => loop_archive_search
    procedure :: restore => loop_archive_restore
  end type loop_archive_t


contains

  subroutine loop_archive_activate (archive, name)
    class(loop_archive_t), intent(inout) :: archive
    type(string_t), intent(in) :: name
    integer :: status, success
    type(string_t) :: prefix
    archive%name = name
    call os_system_call ('test -d "' // name // &
                         '"', status = status, verbose = .true.)
    if (status /= 0) then
      call os_system_call ('mkdir ' // name, &
                            status = success, verbose = .true.)
      if (success /= 0) call msg_fatal ("Creation of loop archive failed!")
    end if
    archive%active = .true.
  end subroutine loop_archive_activate

  subroutine loop_archive_record (archive, olp_file, config_file, lib)
    class(loop_archive_t), intent(inout) :: archive
    type(string_t), intent(in) :: olp_file, config_file, lib
    type(string_t) :: current_prefix
    type(string_t) :: filename
    
! Copy, rename and move olp-file
    filename = archive%current_prefix // '.olp'
    call os_system_call ('cp ' // olp_file // ' ' // &
                         filename)
    call os_system_call ('mv ' // filename // ' ' // &
                         archive%name)
! Do the same with the loop-library and the config file
    filename = archive%current_prefix // '_libgolem_olp.so'
    call os_system_call ('cp ' // lib // ' ' // filename)
    call os_system_call ('mv ' // filename // ' ' // &
                         archive%name)
    filename = archive%current_prefix // '_golem.in'
    call os_system_call ('cp ' // config_file // ' ' // &
                         filename)
    call os_system_call ('mv ' // filename // ' ' // &
                         archive%name)

  end subroutine loop_archive_record

  subroutine loop_archive_search (archive, files, found)
    class(loop_archive_t), intent(inout) :: archive
    type(string_t), dimension(3), intent(in) :: files
    logical, intent(out) :: found
    type(string_t) :: current_olp, current_config, current_lib
    character(len=3) :: prefix
    integer :: counter
    logical, dimension(2) :: exist
    integer, dimension(2) :: identical
    integer :: i

    counter = 1
    do
       write(prefix,"(A,I2.2)") 'V', counter
       current_olp = archive%name // '/' // var_str (prefix) // '.olp'
       current_config = archive%name // '/' // var_str (prefix) // '_golem.in'
       current_lib = archive%name // '/' // var_str (prefix) // '_libgolem_olp.so'
       inquire (file = char (current_olp), exist = exist(1))
       inquire (file = char (current_config), exist = exist(2))
       if (all (exist)) then
          call os_system_call ('diff ' // current_olp // ' ' // files(1) // &
                               ' > /dev/null', status=identical(1))
          call os_system_call ('diff ' // current_config // ' ' // files(2) // &
                               ' > /dev/null', status=identical(2))
            if (all (identical == 0)) then
               found = .true.
               exit
            else
               counter = counter+1
            end if
       else
          found = .false.
          exit
       end if
       if (counter >= 100) call msg_fatal ("Maximum number of loop-libraries exceeded!")
    end do
    write(prefix,"(A,I2.2)") 'V', counter
    archive%current_prefix = var_str (prefix)    
  end subroutine loop_archive_search

  subroutine loop_archive_restore (archive, olp_orig, path)
    class(loop_archive_t), intent(inout) :: archive
    type(string_t), intent(in) :: olp_orig, path
    type(string_t) :: olp_file, config_file, lib
    
    olp_file = archive%current_prefix // '.olp'
    config_file = archive%current_prefix // '_golem.in'
    lib = archive%current_prefix // '_libgolem_olp.so'

    call os_system_call ('cp ' // archive%name // '/' // olp_file // ' .')
    call os_system_call ('mv ' // olp_file // ' ' // olp_orig)
    
    call os_system_call ('cp ' // archive%name // '/' // config_file // ' .')
    call os_system_call ('mv ' // config_file // ' golem.in')

    call os_system_call ('cp ' // archive%name // '/' // lib // ' .')
    call os_system_call ('mv ' // lib // ' libgolem_olp.so')
    call os_system_call ('cp libgolem_olp.so ' // path)
    
  end subroutine loop_archive_restore


end module loop_archive
