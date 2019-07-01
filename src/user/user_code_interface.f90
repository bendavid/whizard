! WHIZARD 2.2.5 Feb 27 2015
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

module user_code_interface

  use iso_c_binding !NODEP!
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use c_particles
  use os_interface

  implicit none
  private

  public :: has_user_lib
  public :: user_code_init
  public :: user_code_final
  public :: user_code_find_proc
  public :: user_obs_int_unary
  public :: user_obs_int_binary
  public :: user_obs_real_unary
  public :: user_obs_real_binary
  public :: user_cut_fun
  public :: user_event_shape_fun
  public :: user_int_info
  public :: user_int_mask
  public :: user_int_state
  public :: user_int_kinematics
  public :: user_int_evaluate

  type(dlaccess_t), save :: user_lib_handle
  logical, save :: has_user_lib = .false.
  type(string_t), save :: user


  abstract interface
     function user_obs_int_unary (prt1) result (ival) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), intent(in) :: prt1
       integer(c_int) :: ival
     end function user_obs_int_unary
  end interface

  abstract interface
     function user_obs_int_binary (prt1, prt2) result (ival) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), intent(in) :: prt1, prt2
       integer(c_int) :: ival
     end function user_obs_int_binary
  end interface

  abstract interface
     function user_obs_real_unary (prt1) result (rval) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), intent(in) :: prt1
       real(c_double) :: rval
     end function user_obs_real_unary
  end interface

  abstract interface
     function user_obs_real_binary (prt1, prt2) result (rval) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), intent(in) :: prt1, prt2
       real(c_double) :: rval
     end function user_obs_real_binary
  end interface

  abstract interface
     function user_cut_fun (prt, n_prt) result (iflag) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), dimension(*), intent(in) :: prt
       integer(c_int), intent(in) :: n_prt
       integer(c_int) :: iflag
     end function user_cut_fun
  end interface

  abstract interface
     function user_event_shape_fun (prt, n_prt) result (rval) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), dimension(*), intent(in) :: prt
       integer(c_int), intent(in) :: n_prt
       real(c_double) :: rval
     end function user_event_shape_fun
  end interface

  abstract interface
     subroutine user_int_info (n_in, n_out, n_states, n_col, n_dim, n_var) &
          bind(C)
       use iso_c_binding !NODEP!
       integer(c_int), intent(inout) :: n_in, n_out, n_states, n_col
       integer(c_int), intent(inout) :: n_dim, n_var
     end subroutine user_int_info
  end interface

  abstract interface
     subroutine user_int_mask (i_prt, m_flv, m_hel, m_col, i_lock) bind(C)
       use iso_c_binding !NODEP!
       integer(c_int), intent(in) :: i_prt
       integer(c_int), intent(inout) :: m_flv, m_hel, m_col, i_lock
     end subroutine user_int_mask
  end interface

  abstract interface
     subroutine user_int_state (i_state, i_prt, flv, hel, col) bind(C)
       use iso_c_binding !NODEP!
       integer(c_int), intent(in) :: i_state, i_prt
       integer(c_int), intent(inout) :: flv, hel
       integer(c_int), dimension(*), intent(inout) :: col
     end subroutine user_int_state
  end interface

  abstract interface
     subroutine user_int_kinematics (prt_in, rval, prt_out, xval) bind(C)
       use iso_c_binding !NODEP!
       use c_particles !NODEP!
       type(c_prt_t), dimension(*), intent(in) :: prt_in
       real(c_double), dimension(*), intent(in) :: rval
       type(c_prt_t), dimension(*), intent(inout) :: prt_out
       real(c_double), dimension(*), intent(out) :: xval
     end subroutine user_int_kinematics
  end interface

  abstract interface
     subroutine user_int_evaluate (xval, scale, fval) bind(C)
       use iso_c_binding !NODEP!
       real(c_double), dimension(*), intent(in) :: xval
       real(c_double), intent(in) :: scale
       real(c_double), dimension(*), intent(out) :: fval
     end subroutine user_int_evaluate
  end interface


contains

  subroutine user_code_init (user_src, user_lib, user_target, rebuild, os_data)
    type(string_t), dimension(:), intent(in) :: user_src, user_lib
    type(string_t), intent(in) :: user_target
    logical, intent(in) :: rebuild
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: user_src_file, user_obj_files, user_lib_file
    logical :: exist
    type(c_funptr) :: fptr
    integer :: i
    call msg_message ("Initializing user code")
    user = user_target;  if (user == "")  user = "user"
    user_obj_files = ""
    inquire (file = char (user) // ".la", exist = exist)
    if (rebuild .or. .not. exist) then
       do i = 1, size (user_src)
          user_src_file = user_src(i) // os_data%fc_src_ext
          inquire (file = char (user_src_file), exist = exist)
          if (exist) then
             call msg_message ("Found user-code source '" &
                  // char (user_src_file) // "'.")
             call compile_user_src (user_src_file, user_obj_files)
          else
             call msg_fatal ("User-code source '" // char (user_src_file) &
                  // "' not found")
          end if
       end do
       do i = 1, size (user_lib)
          user_lib_file = user_lib(i) // ".la"
          inquire (file = char (user_lib_file), exist = exist)
          if (exist) then
             call msg_message ("Found user-code library '" &
                  // char (user_lib_file) // "'.")
          else
             user_lib_file = user_lib(i) // os_data%shlib_ext
             inquire (file = char (user_lib_file), exist = exist)
             if (exist) then
                call msg_message ("Found user-code library '" &
                     // char (user_lib_file) // "'.")
             else
                call msg_fatal ("User-code library '" // char (user_lib(i)) &
                     // "' not found")
             end if
          end if
          user_obj_files = user_obj_files // " " // user_lib_file
       end do
       if (user_obj_files == "") then
          user_src_file = user // os_data%fc_src_ext
          inquire (file = char (user_src_file), exist = exist)
          if (exist) then
             call msg_message ("Found user-code source '" &
                  // char (user_src_file) // "'.")
             call compile_user_src (user_src_file, user_obj_files)
          else
             call msg_fatal ("User-code source '" // char (user_src_file) &
                  // "' not found")
          end if
       end if
       if (user_obj_files /= "") then
          call link_user (char (user), user_obj_files)
       end if
    end if
    call dlaccess_init &
         (user_lib_handle, var_str ("."), &
          user // os_data%shlib_ext, os_data)
    if (dlaccess_has_error (user_lib_handle)) then
       call msg_error (char (dlaccess_get_error (user_lib_handle)))
       call msg_fatal ("Loading user code library '" // char (user) &
            // ".la' failed")
    else
       call msg_message ("User code library '" // char (user) &
            // ".la' successfully loaded")
       has_user_lib = .true.
    end if
  contains
    subroutine compile_user_src (user_src_file, user_obj_files)
      type(string_t), intent(in) :: user_src_file
      type(string_t), intent(inout) :: user_obj_files
      type(string_t) :: basename, ext
      logical :: exist
      basename = user_src_file
      call split (basename, ext, ".", back=.true.)
      if ("." // ext == os_data%fc_src_ext) then
         inquire (file = char (user_src_file), exist = exist)
         if (exist) then
            call msg_message ("Compiling user code file '" &
                 // char (user_src_file) // "'")
            call os_compile_shared (basename, os_data)
            user_obj_files = user_obj_files // " " // basename // ".lo"
         else
            call msg_error ("User code file '" // char (user_src_file) &
                 // "' not found.")
         end if
      else
         call msg_error ("User code file '" // char (user_src_file) &
              // "' should have file extension '" &
              // char (os_data%fc_src_ext) // "'")
      end if
    end subroutine compile_user_src
    subroutine link_user (user_lib, user_obj_files)
      character(*), intent(in) :: user_lib
      type(string_t), intent(in) :: user_obj_files
      call msg_message ("Linking user code library '" &
           // user_lib // char (os_data%shlib_ext) // "'")
      call os_link_shared (user_obj_files, var_str (user_lib), os_data)
    end subroutine link_user
  end subroutine user_code_init

  subroutine user_code_final ()
    if (has_user_lib) then
       call dlaccess_final (user_lib_handle)
       has_user_lib = .false.
    end if
  end subroutine user_code_final

  function user_code_find_proc (name) result (fptr)
    type(string_t), intent(in) :: name
    type(c_funptr) :: fptr
    integer :: i
    fptr = c_null_funptr
    !!! Ticket #529
    ! fptr = libmanager_get_c_funptr (char (user), char (name))
    if (.not. c_associated (fptr)) then
       if (has_user_lib) then
          fptr = dlaccess_get_c_funptr (user_lib_handle, name)
          if (.not. c_associated (fptr)) then      
             call msg_fatal ("User procedure '" // char (name) // "' not found")
          end if
       else
          call msg_fatal ("User procedure '" // char (name) &
               // "' called without user library (missing -u flag?)")
       end if
    end if
  end function user_code_find_proc


end module user_code_interface
