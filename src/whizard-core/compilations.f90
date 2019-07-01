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

module compilations

  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use os_interface
  use variables
  use process_libraries
  use rt_data

  implicit none
  private

  public :: compile_library
  public :: compile_executable
  public :: load_library

  type :: compilation_t
    type(string_t) :: libname
    type(process_library_t), pointer :: prc_lib => null ()
    logical :: recompile_library = .false.
    logical :: make_executable = .false.
  end type compilation_t


  interface compile_library
    module procedure compile_library0
    module procedure compile_library1
  end interface

  interface compile_executable
    module procedure compile_executable0
    module procedure compile_executable1
  end interface

  interface load_library
    module procedure load_library0
    module procedure load_library1
  end interface


contains

  subroutine compilation_basic_init (comp, libname, var_list)
    type(compilation_t), intent(out) :: comp
    type(string_t), intent(in) :: libname
    type(var_list_t), intent(in) :: var_list
    comp%libname = libname
    comp%prc_lib => process_library_store_get_ptr (comp%libname)
    if (.not. associated (comp%prc_lib)) then
       call msg_fatal ("Process library '" // char (comp%libname) &
            // "' has not been declared.")
    end if
    comp%recompile_library = &
         var_list_get_lval (var_list, var_str ("?recompile_library"))
  end subroutine compilation_basic_init

  subroutine compilation_compile_and_link (comp, os_data)
    type(compilation_t), intent(inout) :: comp
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: objlist
    if (associated (comp%prc_lib)) then
       if (process_library_get_n_processes (comp%prc_lib) > 0) then
          call process_library_generate_code (comp%prc_lib, os_data)
          call process_library_write_driver (comp%prc_lib)
          call process_library_compile &
               (comp%prc_lib, os_data, comp%recompile_library, objlist)
          call process_library_link &
               (comp%prc_lib, os_data, objlist)
       end if
    end if
  end subroutine compilation_compile_and_link

  subroutine compilations_make_executable (comp, os_data, exec_name, var_list)
    type(compilation_t), dimension(:), intent(in) :: comp
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: exec_name
    type(var_list_t), intent(in) :: var_list
    type(string_t) :: flags
    integer :: lib
    type(user_procs_t) :: user_procs
    call splice &
         (var_list_get_sval (var_list, var_str ("$user_procs_cut")), &
          user_procs%cut)
    call splice &
         (var_list_get_sval (var_list, var_str ("$user_procs_event_shape")), &
          user_procs%event_shape)
    call splice &
         (var_list_get_sval (var_list, var_str ("$user_procs_obs1")), &
          user_procs%obs_real_unary)
    call splice &
         (var_list_get_sval (var_list, var_str ("$user_procs_obs2")), &
          user_procs%obs_real_binary)
    call splice &
         (var_list_get_sval (var_list, var_str ("$user_procs_sf")), &
          user_procs%sf)                                               ! $
    call write_library_manager (comp%libname, user_procs)
    call compile_library_manager (os_data)
    flags = ""
    do lib = 1, size (comp)
       flags = flags // get_modellibs_flags (comp(lib)%prc_lib, os_data)
    end do
    call link_executable (comp%libname, exec_name, flags, os_data)
  contains
    subroutine splice (string, array)
      type(string_t), intent(in) :: string
      type(string_t), dimension(:), intent(out), allocatable :: array
      type(string_t) :: buffer, word, separator
      integer :: n_commas, i
      if (string /= "") then
         buffer = string
         n_commas = 0
         COUNT_COMMAS: do
            call split (buffer, word, ",", separator)
            if (len (separator) == 0)  exit COUNT_COMMAS
            n_commas = n_commas + 1
         end do COUNT_COMMAS
         allocate (array (n_commas + 1))
         buffer = string
         ASSIGN_STRINGS: do i = 1, size (array)
            call split (buffer, word, ",")
            array(i) = adjustl (trim (word))
         end do ASSIGN_STRINGS
      else
         allocate (array (0))
      end if
    end subroutine splice
  end subroutine compilations_make_executable

  subroutine compilation_load_library (comp, os_data, global_var_list)
    type(compilation_t), intent(in) :: comp
    type(os_data_t), intent(in) :: os_data
    type(var_list_t), intent(inout) :: global_var_list
    type(process_library_t), pointer :: prc_lib
    call process_library_store_append (comp%libname, os_data, prc_lib)
    call process_library_load (prc_lib, os_data, var_list=global_var_list)
  end subroutine compilation_load_library

  subroutine compile_library0 (libname, global, global_var_list, global_prc_lib)
    type(string_t), intent(in) :: libname
    type(rt_data_t), intent(in) :: global
    type(var_list_t), intent(inout) :: global_var_list
    type(process_library_t), pointer :: global_prc_lib
    type(compilation_t) :: comp
    call compilation_basic_init (comp, libname, global%var_list)
    call compilation_compile_and_link (comp, global%os_data)
    call compilation_load_library (comp, global%os_data, global_var_list)
    global_prc_lib => comp%prc_lib
  end subroutine compile_library0

  subroutine compile_library1 (libname, global, global_var_list, global_prc_lib)
    type(string_t), dimension(:), intent(in) :: libname
    type(rt_data_t), intent(in) :: global
    type(var_list_t), intent(inout) :: global_var_list
    type(process_library_t), pointer :: global_prc_lib
    integer :: lib
    do lib = 1, size (libname)
       call compile_library0 &
            (libname(lib), global, global_var_list, global_prc_lib)
    end do
  end subroutine compile_library1

  subroutine compile_executable0 (libname, exec_name, global)
    type(string_t), intent(in) :: libname, exec_name
    type(rt_data_t), intent(in) :: global
    type(compilation_t), dimension(1) :: comp
    call compilation_basic_init (comp(1), libname, global%var_list)
    call compilation_compile_and_link (comp(1), global%os_data)
    call compilations_make_executable &
         (comp, global%os_data, exec_name, global%var_list)
  end subroutine compile_executable0

  subroutine compile_executable1 (libname, exec_name, global)
    type(string_t), dimension(:), intent(in) :: libname
    type(string_t), intent(in) :: exec_name
    type(rt_data_t), intent(in) :: global
    type(compilation_t), dimension(size(libname)) :: comp
    integer :: lib
    do lib = 1, size (libname)
       call compilation_basic_init (comp(lib), libname(lib), global%var_list)
       call compilation_compile_and_link (comp(lib), global%os_data)
    end do
    call compilations_make_executable &
         (comp, global%os_data, exec_name, global%var_list)
  end subroutine compile_executable1

  subroutine load_library0 (libname, global, global_var_list, global_prc_lib)
    type(string_t), intent(in) :: libname
    type(rt_data_t), intent(in) :: global
    type(var_list_t), intent(inout) :: global_var_list
    type(process_library_t), pointer :: global_prc_lib
    type(compilation_t) :: comp
    call compilation_basic_init (comp, libname, global%var_list)
    call compilation_load_library (comp, global%os_data, global_var_list)
    global_prc_lib => comp%prc_lib
  end subroutine load_library0

  subroutine load_library1 (libname, global, global_var_list, global_prc_lib)
    type(string_t), dimension(:), intent(in) :: libname
    type(rt_data_t), intent(in) :: global
    type(var_list_t), intent(inout) :: global_var_list
    type(process_library_t), pointer :: global_prc_lib
    integer :: lib
    do lib = 1, size (libname)
       call load_library0 &
            (libname(lib), global, global_var_list, global_prc_lib)
    end do
  end subroutine load_library1


end module compilations
