! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use limits, only: TAB !NODEP!
  use os_interface
  use unit_tests
  use md5
  use variables
  use models
  use process_libraries
  use prclib_stacks
  use rt_data
  use process_configurations

  implicit none
  private

  public :: compile_library
  public :: compile_executable
  public :: compilations_test
  public :: compilations_static_test

  type :: compilation_item_t
     type(string_t) :: libname
     type(process_library_t), pointer :: lib => null ()
     logical :: recompile_library = .false.
   contains
     procedure :: init => compilation_item_init
     procedure :: compile => compilation_item_compile
     procedure :: load => compilation_item_load
     procedure :: success => compilation_item_success
  end type compilation_item_t

  type :: compilation_t
     private
     type(string_t) :: exe_name
     type(string_t), dimension(:), allocatable :: lib_name
   contains
     procedure :: write => compilation_write
     procedure :: init => compilation_init
     procedure :: write_dispatcher => compilation_write_dispatcher
     procedure :: write_makefile => compilation_write_makefile
     procedure :: make_compile => compilation_make_compile
     procedure :: make_link => compilation_make_link
     procedure :: make_clean_exe => compilation_make_clean_exe
  end type compilation_t


contains

  subroutine compilation_item_init (comp, libname, stack, var_list)
    class(compilation_item_t), intent(out) :: comp
    type(string_t), intent(in) :: libname
    type(prclib_stack_t), intent(inout) :: stack
    type(var_list_t), intent(in) :: var_list
    comp%libname = libname
    comp%lib => stack%get_library_ptr (comp%libname)
    if (.not. associated (comp%lib)) then
       call msg_fatal ("Process library '" // char (comp%libname) &
            // "' has not been declared.")
    end if
    comp%recompile_library = &
         var_list_get_lval (var_list, var_str ("?recompile_library"))
  end subroutine compilation_item_init

  subroutine compilation_item_compile (comp, os_data, force, recompile)
    class(compilation_item_t), intent(inout) :: comp
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: force, recompile
    if (associated (comp%lib)) then
       call msg_message ("Process library '" &
            // char (comp%libname) // "': compiling ...")
       call comp%lib%configure (os_data)
       if (signal_is_pending ())  return
       call comp%lib%compute_md5sum ()
       call comp%lib%write_makefile (os_data, force)
       if (signal_is_pending ())  return
       if (force) then
          call comp%lib%clean (os_data, distclean = .false.)
          if (signal_is_pending ())  return
       end if
       call comp%lib%write_driver (force)
       if (signal_is_pending ())  return
       if (recompile) then
          call comp%lib%load (os_data, keep_old_source = .true.)
          if (signal_is_pending ())  return
       end if
       call comp%lib%update_status (os_data)
    end if
  end subroutine compilation_item_compile

  subroutine compilation_item_load (comp, os_data)
    class(compilation_item_t), intent(inout) :: comp
    type(os_data_t), intent(in) :: os_data
    if (associated (comp%lib)) then
       call comp%lib%load (os_data)
    end if
  end subroutine compilation_item_load

  subroutine compilation_item_success (comp)
    class(compilation_item_t), intent(in) :: comp
    if (associated (comp%lib)) then
       call msg_message ("Process library '" // char (comp%libname) &
            // "': ... success.")
    end if
  end subroutine compilation_item_success

  subroutine compile_library (libname, global)
    type(string_t), intent(in) :: libname
    type(rt_data_t), intent(inout), target :: global
    type(compilation_item_t) :: comp
    logical :: force, recompile
    force = &
         var_list_get_lval (global%var_list, var_str ("?rebuild_library"))
    recompile = &
         var_list_get_lval (global%var_list, var_str ("?recompile_library"))
    call comp%init (libname, global%prclib_stack, global%var_list)
    call comp%compile (global%os_data, force, recompile)
    if (signal_is_pending ())  return
    call comp%load (global%os_data)
    if (signal_is_pending ())  return
    call comp%success ()
  end subroutine compile_library

  subroutine compilation_write (object, unit)
    class(compilation_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(1x,A)")  "Compilation object:"
    write (u, "(3x,3A)")  "executable        = '", &
         char (object%exe_name), "'"
    write (u, "(3x,A)", advance="no")  "process libraries ="
    do i = 1, size (object%lib_name)
       write (u, "(1x,3A)", advance="no")  "'", char (object%lib_name(i)), "'"
    end do
    write (u, *)
  end subroutine compilation_write
  
  subroutine compilation_init (compilation, exe_name, lib_name)
    class(compilation_t), intent(out) :: compilation
    type(string_t), intent(in) :: exe_name
    type(string_t), dimension(:), intent(in) :: lib_name
    compilation%exe_name = exe_name
    allocate (compilation%lib_name (size (lib_name)))
    compilation%lib_name = lib_name
  end subroutine compilation_init
    
  subroutine compilation_write_dispatcher (compilation)
    class(compilation_t), intent(in) :: compilation
    type(string_t) :: file
    integer :: u, i
    file = compilation%exe_name // "_prclib_dispatcher.f90"
    call msg_message ("Static executable '" // char (compilation%exe_name) &
         // "': writing library dispatcher")
    u = free_unit ()
    open (u, file = char (file), status="replace", action="write")
    write (u, "(3A)")  "! Whizard: process libraries for executable '", &
         char (compilation%exe_name), "'"
    write (u, "(A)")  "! Automatically generated file, do not edit"
    write (u, "(A)")  "subroutine dispatch_prclib_static " // &
         "(driver, basename, modellibs_ldflags)"
    write (u, "(A)")  "  use iso_varying_string, string_t => varying_string"
    write (u, "(A)")  "  use prclib_interfaces"
    do i = 1, size (compilation%lib_name)
       associate (lib_name => compilation%lib_name(i))
         write (u, "(A)")  "  use " // char (lib_name) // "_driver"
       end associate
    end do
    write (u, "(A)")  "  implicit none"
    write (u, "(A)")  "  class(prclib_driver_t), intent(inout), allocatable &
         &:: driver"
    write (u, "(A)")  "  type(string_t), intent(in) :: basename"
    write (u, "(A)")  "  logical, intent(in), optional :: " // &
         "modellibs_ldflags"
    write (u, "(A)")  "  select case (char (basename))"
    do i = 1, size (compilation%lib_name)
       associate (lib_name => compilation%lib_name(i))
         write (u, "(3A)")  "  case ('", char (lib_name), "')"
         write (u, "(3A)")  "     allocate (", char (lib_name), "_driver_t &
              &:: driver)"
       end associate
    end do
    write (u, "(A)")  "  end select"
    write (u, "(A)")  "end subroutine dispatch_prclib_static"
    write (u, *)
    write (u, "(A)")  "subroutine get_prclib_static (libname)"
    write (u, "(A)")  "  use iso_varying_string, string_t => varying_string"
    write (u, "(A)")  "  implicit none"
    write (u, "(A)")  "  type(string_t), dimension(:), intent(inout), &
         &allocatable :: libname"
    write (u, "(A,I0,A)")  "  allocate (libname (", &
         size (compilation%lib_name), "))"
    do i = 1, size (compilation%lib_name)
       associate (lib_name => compilation%lib_name(i))
         write (u, "(A,I0,A,A,A)")  "  libname(", i, ") = '", &
              char (lib_name), "'"
       end associate
    end do
    write (u, "(A)")  "end subroutine get_prclib_static"
    close (u)
  end subroutine compilation_write_dispatcher
    
  subroutine compilation_write_makefile (compilation, os_data)
    class(compilation_t), intent(in) :: compilation
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: file
    integer :: u, i
    file = compilation%exe_name // ".makefile"
    call msg_message ("Static executable '" // char (compilation%exe_name) &
         // "': writing makefile")
    u = free_unit ()
    open (u, file = char (file), status="replace", action="write")
    write (u, "(3A)")  "# WHIZARD: Makefile for executable '", &
         char (compilation%exe_name), "'"
    write (u, "(A)")  "# Automatically generated file, do not edit"
    write (u, "(A)") ""
    write (u, "(A)") "# Executable name"
    write (u, "(A)") "EXE = " // char (compilation%exe_name)
    write (u, "(A)") ""
    write (u, "(A)") "# Compiler"
    write (u, "(A)") "FC = " // char (os_data%fc)
    write (u, "(A)") ""
    write (u, "(A)") "# Included libraries"
    write (u, "(A)") "FCINCL = " // char (os_data%whizard_includes)
    write (u, "(A)") ""
    write (u, "(A)") "# Compiler flags"
    write (u, "(A)") "FCFLAGS = " // char (os_data%fcflags)
    write (u, "(A)") "LDFLAGS = " // char (os_data%ldflags)
    write (u, "(A)") "LDFLAGS_STATIC = " // char (os_data%ldflags_static)   
    write (u, "(A)") "LDFLAGS_HEPMC = " // char (os_data%ldflags_hepmc)
    write (u, "(A)") "LDFLAGS_HOPPET = " // char (os_data%ldflags_hoppet)    
    write (u, "(A)") "LDWHIZARD = " // char (os_data%whizard_ldflags)
    write (u, "(A)") ""
    write (u, "(A)") "# Libtool"
    write (u, "(A)") "LIBTOOL = " // char (os_data%whizard_libtool)
    write (u, "(A)") "FCOMPILE = $(LIBTOOL) --tag=FC --mode=compile"
    write (u, "(A)") "LINK = $(LIBTOOL) --tag=FC --mode=link"
    write (u, "(A)") ""
    write (u, "(A)") "# Compile commands (default)"
    write (u, "(A)") "LTFCOMPILE = $(FCOMPILE) $(FC) -c $(FCINCL) $(FCFLAGS)"
    write (u, "(A)") ""
    write (u, "(A)") "# Default target"
    write (u, "(A)") "all: link"
    write (u, "(A)") ""
    write (u, "(A)") "# Libraries"
    do i = 1, size (compilation%lib_name)
       associate (lib_name => compilation%lib_name(i))
         write (u, "(A)") "LIBRARIES += " // char (lib_name) // ".la"
         write (u, "(A)") char (lib_name) // ".la:"
         write (u, "(A)") TAB // "$(MAKE) -f " // char (lib_name) // ".makefile"
       end associate
    end do
    write (u, "(A)") ""
    write (u, "(A)") "# Library dispatcher"
    write (u, "(A)") "DISP = $(EXE)_prclib_dispatcher"
    write (u, "(A)") "$(DISP).lo: $(DISP).f90 $(LIBRARIES)"
    write (u, "(A)") TAB // "$(LTFCOMPILE) $<"
    write (u, "(A)") ""
    write (u, "(A)") "# Executable"
    write (u, "(A)") "$(EXE): $(DISP).lo $(LIBRARIES)"
    write (u, "(A)") TAB // "$(LINK) $(FC) -static-libtool-libs $(FCFLAGS) \"
    write (u, "(A)") TAB // "   $(LDWHIZARD) $(LDFLAGS) $(LDFLAGS_STATIC) \" 
    write (u, "(A)") TAB // "     -o $(EXE) $^ $(LDFLAGS_HEPMC) $(LDFLAGS_HOPPET)"
    write (u, "(A)") ""
    write (u, "(A)") "# Main targets"
    write (u, "(A)") "link: compile $(EXE)"
    write (u, "(A)") "compile: $(LIBRARIES) $(DISP).lo"
    write (u, "(A)") ".PHONY: link compile"
    write (u, "(A)") ""
    write (u, "(A)") "# Cleanup targets"
    write (u, "(A)") "clean-exe:"
    write (u, "(A)") TAB // "rm -f $(EXE)"
    write (u, "(A)") "clean-objects:"
    write (u, "(A)") TAB // "rm -f $(DISP).lo"
    write (u, "(A)") "clean-source:"
    write (u, "(A)") TAB // "rm -f $(DISP).f90"
    write (u, "(A)") "clean-makefile:"
    write (u, "(A)") TAB // "rm -f $(EXE).makefile"
    write (u, "(A)") ""
    write (u, "(A)") "clean: clean-exe clean-objects clean-source"
    write (u, "(A)") "distclean: clean clean-makefile"
    write (u, "(A)") ".PHONY: clean distclean"
    close (u)
  end subroutine compilation_write_makefile
    
  subroutine compilation_make_compile (compilation, os_data)
    class(compilation_t), intent(in) :: compilation
    type(os_data_t), intent(in) :: os_data
    call os_system_call ("make compile " // os_data%makeflags &
         // " -f " // compilation%exe_name // ".makefile")
  end subroutine compilation_make_compile
  
  subroutine compilation_make_link (compilation, os_data)
    class(compilation_t), intent(in) :: compilation
    type(os_data_t), intent(in) :: os_data
    call os_system_call ("make link " // os_data%makeflags &
         // " -f " // compilation%exe_name // ".makefile")
  end subroutine compilation_make_link
  
  subroutine compilation_make_clean_exe (compilation, os_data)
    class(compilation_t), intent(in) :: compilation
    type(os_data_t), intent(in) :: os_data
    call os_system_call ("make clean-exe " // os_data%makeflags &
         // " -f " // compilation%exe_name // ".makefile")
  end subroutine compilation_make_clean_exe
  
  subroutine compile_executable (exename, libname, global)
    type(string_t), intent(in) :: exename
    type(string_t), dimension(:), intent(in) :: libname
    type(rt_data_t), intent(inout), target :: global
    type(compilation_t) :: compilation
    type(compilation_item_t) :: item
    logical :: force, recompile
    integer :: i
    force = &
         var_list_get_lval (global%var_list, var_str ("?rebuild_library"))
    recompile = &
         var_list_get_lval (global%var_list, var_str ("?recompile_library"))
    call compilation%init (exename, [libname])
    if (signal_is_pending ())  return
    call compilation%write_dispatcher ()
    if (signal_is_pending ())  return
    call compilation%write_makefile (global%os_data)
    if (signal_is_pending ())  return
    do i = 1, size (libname)
       call item%init (libname(i), global%prclib_stack, global%var_list)
       call item%compile (global%os_data, force=force, recompile=recompile)
       if (signal_is_pending ())  return
       call item%success ()
    end do
    call compilation%make_compile (global%os_data)
    if (signal_is_pending ())  return
    call compilation%make_link (global%os_data)
  end subroutine compile_executable


  subroutine compilations_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (compilations_1, "compilations_1", &
         "intrinsic test processes", &
         u, results)
    call test (compilations_2, "compilations_2", &
         "external process (omega)", &
         u, results)
    call test (compilations_3, "compilations_3", &
         "static executable: driver", &
         u, results)
end subroutine compilations_test

  subroutine compilations_1 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: compilations_1"
    write (u, "(A)")  "*   Purpose: configure and compile test process"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    libname = "compilation_1"
    procname = "prc_comp_1"
    call prepare_test_library (global, libname, 1, [procname])

    call compile_library (libname, global)
    
    call global%write_libraries (u)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: compilations_1"
    
  end subroutine compilations_1
  
  subroutine compilations_2 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: compilations_2"
    write (u, "(A)")  "*   Purpose: configure and compile test process"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    libname = "compilation_2"
    procname = "prc_comp_2"
    call prepare_test_library (global, libname, 2, [procname,procname])

    call compile_library (libname, global)
    
    call global%write_libraries (u, libpath = .false.)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: compilations_2"
    
  end subroutine compilations_2
  
  subroutine compilations_3 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname, exename
    type(rt_data_t), target :: global
    type(compilation_t) :: compilation
    integer :: u_file
    character(80) :: buffer

    write (u, "(A)")  "* Test output: compilations_3"
    write (u, "(A)")  "*   Purpose: make static executable"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize library"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    libname = "compilations_3_lib"
    procname = "prc_comp_3"
    exename = "compilations_3"
    
    call prepare_test_library (global, libname, 2, [procname,procname])

    call compilation%init (exename, [libname])
    call compilation%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Write dispatcher"
    write (u, "(A)")

    call compilation%write_dispatcher ()

    u_file = free_unit ()
    open (u_file, file = char (exename) // "_prclib_dispatcher.f90", &
         status = "old", action = "read")
    do
       read (u_file, "(A)", end = 1)  buffer
       write (u, "(A)")  trim (buffer)
    end do
1   close (u_file)

    write (u, "(A)")
    write (u, "(A)")  "* Write Makefile"
    write (u, "(A)")

    associate (os_data => global%os_data)
      os_data%fc = "fortran-compiler"
      os_data%whizard_includes = "my-includes"
      os_data%fcflags = "my-fcflags"
      os_data%ldflags = "my-ldflags"
      os_data%ldflags_static = "my-ldflags-static"
      os_data%ldflags_hepmc = "my-ldflags-hepmc"
      os_data%ldflags_hoppet = "my-ldflags-hoppet"      
      os_data%whizard_ldflags = "my-ldwhizard"
      os_data%whizard_libtool = "my-libtool"
    end associate

    call compilation%write_makefile (global%os_data)

    open (u_file, file = char (exename) // ".makefile", &
         status = "old", action = "read")
    do
       read (u_file, "(A)", end = 2)  buffer
       write (u, "(A)")  trim (buffer)
    end do
2   close (u_file)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: compilations_3"
    
  end subroutine compilations_3
  
  subroutine compilations_static_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (compilations_static_1, "compilations_static_1", &
         "static executable: compilation", &
         u, results)
    call test (compilations_static_2, "compilations_static_2", &
         "static executable: shortcut", &
         u, results)
end subroutine compilations_static_test

  subroutine compilations_static_1 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname, exename
    type(rt_data_t), target :: global
    type(compilation_item_t) :: item
    type(compilation_t) :: compilation
    logical :: exist

    write (u, "(A)")  "* Test output: compilations_static_1"
    write (u, "(A)")  "*   Purpose: make static executable"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize library"

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    libname = "compilations_static_1_lib"
    procname = "prc_comp_stat_1"
    exename = "compilations_static_1"
    
    call prepare_test_library (global, libname, 2, [procname,procname])

    call compilation%init (exename, [libname])

    write (u, "(A)")
    write (u, "(A)")  "* Write dispatcher"

    call compilation%write_dispatcher ()

    write (u, "(A)")
    write (u, "(A)")  "* Write Makefile"

    call compilation%write_makefile (global%os_data)

    write (u, "(A)")
    write (u, "(A)")  "* Build libraries"

    call item%init (libname, global%prclib_stack, global%var_list)
    call item%compile (global%os_data, force=.true., recompile=.false.)
    call item%success ()

    write (u, "(A)")
    write (u, "(A)")  "* Check executable (should be absent)"
    write (u, "(A)")
    
    call compilation%make_clean_exe (global%os_data)
    inquire (file = char (exename), exist = exist)
    write (u, "(A,A,L1)")  char (exename), " exists = ", exist

    write (u, "(A)")
    write (u, "(A)")  "* Build executable"
    write (u, "(A)")

    call compilation%make_compile (global%os_data)
    call compilation%make_link (global%os_data)

    write (u, "(A)")  "* Check executable (should be present)"
    write (u, "(A)")
    
    inquire (file = char (exename), exist = exist)
    write (u, "(A,A,L1)")  char (exename), " exists = ", exist

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call compilation%make_clean_exe (global%os_data)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: compilations_static_1"
    
  end subroutine compilations_static_1
  
  subroutine compilations_static_2 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname, exename
    type(rt_data_t), target :: global
    type(compilation_item_t) :: item
    type(compilation_t) :: compilation
    logical :: exist
    integer :: u_file

    write (u, "(A)")  "* Test output: compilations_static_2"
    write (u, "(A)")  "*   Purpose: make static executable"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize library and compile"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    libname = "compilations_static_2_lib"
    procname = "prc_comp_stat_2"
    exename = "compilations_static_2"
    
    call prepare_test_library (global, libname, 2, [procname,procname])

    call compile_executable (exename, [libname], global)

    write (u, "(A)")  "* Check executable (should be present)"
    write (u, "(A)")
    
    inquire (file = char (exename), exist = exist)
    write (u, "(A,A,L1)")  char (exename), " exists = ", exist

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    u_file = free_unit ()
    open (u_file, file = char (exename), status = "old", action = "write")
    close (u_file, status = "delete")

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: compilations_static_2"
    
  end subroutine compilations_static_2
  

end module compilations
