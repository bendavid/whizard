! WHIZARD 2.0.0 Mon Apr 12 2010
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

module os_interface

  use iso_c_binding !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use system_dependencies !NODEP!
  use limits, only: DLERROR_LEN, ENVVAR_LEN !NODEP!
  use diagnostics !NODEP!

  implicit none
  private

  public :: paths_t
  public :: paths_init
  public :: os_data_t
  public :: os_data_init
  public :: os_data_write
  public :: dlaccess_t
  public :: dlaccess_init
  public :: dlaccess_final
  public :: dlaccess_has_error
  public :: dlaccess_get_error
  public :: dlaccess_get_c_funptr
  public :: dlaccess_is_open
  public :: os_system_call
  public :: os_dir_exist
  public :: os_compile_shared
  public :: os_link_shared
  public :: os_link_static
  public :: os_get_dlname
  public :: os_interface_test

  type :: paths_t
     type(string_t) :: prefix
     type(string_t) :: exec_prefix
     type(string_t) :: bindir
     type(string_t) :: libdir
     type(string_t) :: includedir
     type(string_t) :: datarootdir
     type(string_t) :: localprefix
  end type paths_t

  type :: os_data_t
     logical :: use_libtool
     logical :: use_testfiles
     type(string_t) :: fc
     type(string_t) :: fcflags
     type(string_t) :: fcflags_pic
     type(string_t) :: fc_src_ext
     type(string_t) :: obj_ext
     type(string_t) :: ld
     type(string_t) :: ldflags
     type(string_t) :: ldflags_so
     type(string_t) :: ldflags_static
     type(string_t) :: shlib_ext
     type(string_t) :: prefix
     type(string_t) :: exec_prefix
     type(string_t) :: bindir
     type(string_t) :: libdir
     type(string_t) :: includedir
     type(string_t) :: datarootdir
     type(string_t) :: whizard_omega_binpath
     type(string_t) :: whizard_includes
     type(string_t) :: whizard_ldflags
     type(string_t) :: whizard_libtool
     type(string_t) :: whizard_modelpath
     type(string_t) :: whizard_models_libpath
     type(string_t) :: whizard_susypath
     type(string_t) :: whizard_gmlpath
     type(string_t) :: whizard_cutspath
     type(string_t) :: whizard_texpath
     type(string_t) :: whizard_testdatapath
     type(string_t) :: whizard_modelpath_local
     type(string_t) :: whizard_models_libpath_local
     type(string_t) :: whizard_omega_binpath_local
     logical :: event_analysis_ps  = .false.
     logical :: event_analysis_pdf = .false.
     type(string_t) :: latex
     type(string_t) :: gml
     type(string_t) :: dvips
     type(string_t) :: ps2pdf
  end type os_data_t

  type :: dlaccess_t
     private
     type(string_t) :: filename
     type(c_ptr) :: handle = c_null_ptr
     logical :: is_open = .false.
     logical :: has_error = .false.
     type(string_t) :: error
  end type dlaccess_t


  interface 
     function dlopen (filename, flag) result (handle) bind(C)
       import
       character(c_char), dimension(*) :: filename
       integer(c_int), value :: flag
       type(c_ptr) :: handle
     end function dlopen
  end interface

  interface
     function dlclose (handle) result (status) bind(C)
       import
       type(c_ptr), value :: handle
       integer(c_int) :: status
     end function dlclose
  end interface

  interface 
     function dlerror () result (str) bind(C)
       import
       type(c_ptr) :: str
     end function dlerror
  end interface

  interface 
     function dlsym (handle, symbol) result (fptr) bind(C)
       import
       type(c_ptr), value :: handle
       character(c_char), dimension(*) :: symbol
       type(c_funptr) :: fptr
     end function dlsym
  end interface

  interface
     function system (command) result (status) bind(C)
       import
       integer(c_int) :: status
       character(c_char), dimension(*) :: command
     end function system
  end interface


contains

  subroutine paths_init (paths)
    type(paths_t), intent(out) :: paths
    paths%prefix = ""
    paths%exec_prefix = ""
    paths%bindir = ""
    paths%libdir = ""
    paths%includedir = ""
    paths%datarootdir = ""
    paths%localprefix = ""
  end subroutine paths_init

  subroutine os_data_init (os_data, paths)
    type(os_data_t), intent(out) :: os_data
    type(paths_t), intent(in), optional :: paths
    character(len=ENVVAR_LEN) :: home
    type(string_t) :: localprefix, local_includes
    os_data%use_libtool = .true.
    inquire (file = "TESTFLAG", exist = os_data%use_testfiles)
    call get_environment_variable ("HOME", home)
    if (paths%localprefix == "") then
       localprefix = trim (home) // "/.whizard"
    else
       localprefix = paths%localprefix
    end if
    local_includes = localprefix // "/lib/whizard/mod/models"
    os_data%whizard_modelpath_local = localprefix // "/share/whizard/models"
    os_data%whizard_models_libpath_local = localprefix // "/lib/whizard/models"
    os_data%whizard_omega_binpath_local = localprefix // "/bin"
    os_data%fc             = DEFAULT_FC
    os_data%fcflags        = DEFAULT_FCFLAGS
    os_data%fcflags_pic    = DEFAULT_FCFLAGS_PIC
    os_data%fc_src_ext     = DEFAULT_FC_SRC_EXT
    os_data%obj_ext        = DEFAULT_OBJ_EXT
    os_data%ld             = DEFAULT_LD
    os_data%ldflags        = DEFAULT_LDFLAGS
    os_data%ldflags_so     = DEFAULT_LDFLAGS_SO
    os_data%ldflags_static = DEFAULT_LDFLAGS_STATIC
    os_data%shlib_ext      = DEFAULT_SHLIB_EXT
    os_data%prefix      = PREFIX
    os_data%exec_prefix = EXEC_PREFIX
    os_data%bindir      = BINDIR
    os_data%libdir      = LIBDIR
    os_data%includedir  = INCLUDEDIR
    os_data%datarootdir = DATAROOTDIR
    if (present (paths)) then
       if (paths%prefix      /= "")  os_data%prefix      = paths%prefix
       if (paths%exec_prefix /= "")  os_data%exec_prefix = paths%prefix
       if (paths%bindir      /= "")  os_data%bindir      = paths%prefix
       if (paths%libdir      /= "")  os_data%libdir      = paths%prefix
       if (paths%includedir  /= "")  os_data%includedir  = paths%prefix
       if (paths%datarootdir /= "")  os_data%datarootdir = paths%prefix
    end if
    if (os_data%use_testfiles) then
       os_data%whizard_omega_binpath  = WHIZARD_TEST_OMEGA_BINPATH
       os_data%whizard_includes       = WHIZARD_TEST_INCLUDES
       os_data%whizard_ldflags        = WHIZARD_TEST_LDFLAGS
       os_data%whizard_libtool        = WHIZARD_LIBTOOL_TEST
       os_data%whizard_modelpath      = WHIZARD_TEST_MODELPATH
       os_data%whizard_models_libpath = WHIZARD_TEST_MODELS_LIBPATH
       os_data%whizard_susypath       = WHIZARD_TEST_SUSYPATH
       os_data%whizard_gmlpath        = WHIZARD_TEST_GMLPATH
       os_data%whizard_cutspath       = WHIZARD_TEST_CUTSPATH
       os_data%whizard_texpath        = WHIZARD_TEST_TEXPATH
       os_data%whizard_testdatapath   = WHIZARD_TEST_TESTDATAPATH
    else
       if (os_dir_exist (local_includes)) then
          os_data%whizard_includes = "-I" // local_includes // " "// &
             WHIZARD_INCLUDES 
       else
          os_data%whizard_includes = WHIZARD_INCLUDES
       end if
       os_data%whizard_omega_binpath  = WHIZARD_OMEGA_BINPATH
       os_data%whizard_ldflags        = WHIZARD_LDFLAGS
       os_data%whizard_libtool        = WHIZARD_LIBTOOL
       os_data%whizard_modelpath      = WHIZARD_MODELPATH
       os_data%whizard_models_libpath = WHIZARD_MODELS_LIBPATH
       os_data%whizard_susypath       = WHIZARD_SUSYPATH
       os_data%whizard_gmlpath        = WHIZARD_GMLPATH
       os_data%whizard_cutspath       = WHIZARD_CUTSPATH
       os_data%whizard_texpath        = WHIZARD_TEXPATH
       os_data%whizard_testdatapath   = WHIZARD_TESTDATAPATH
    end if
    os_data%event_analysis_ps  = EVENT_ANALYSIS_PS  == "yes"
    os_data%event_analysis_pdf = EVENT_ANALYSIS_PDF == "yes"
    os_data%latex  = PRG_LATEX
    os_data%gml    = os_data%whizard_gmlpath // "/gml"
    os_data%dvips  = PRG_DVIPS
    os_data%ps2pdf = PRG_PS2PDF
    call os_data_expand_paths (os_data)
  end subroutine os_data_init
    
  subroutine os_data_expand_paths (os_data)
    type(os_data_t), intent(inout) :: os_data
    integer, parameter :: N_VARIABLES = 6
    type(string_t), dimension(N_VARIABLES) :: variable, value
    variable(1) = "${prefix}";       value(1) = os_data%prefix
    variable(2) = "${exec_prefix}";  value(2) = os_data%exec_prefix
    variable(3) = "${bindir}";       value(3) = os_data%bindir
    variable(4) = "${libdir}";       value(4) = os_data%libdir
    variable(5) = "${includedir}";   value(5) = os_data%includedir
    variable(6) = "${datarootdir}";  value(6) = os_data%datarootdir
    call expand_paths (os_data%whizard_omega_binpath)
    call expand_paths (os_data%whizard_includes)
    call expand_paths (os_data%whizard_ldflags)
    call expand_paths (os_data%whizard_libtool)
    call expand_paths (os_data%whizard_modelpath)
    call expand_paths (os_data%whizard_models_libpath)
    call expand_paths (os_data%whizard_susypath)
    call expand_paths (os_data%whizard_gmlpath)
    call expand_paths (os_data%whizard_cutspath)
    call expand_paths (os_data%whizard_texpath)
    call expand_paths (os_data%whizard_testdatapath)
    call expand_paths (os_data%whizard_models_libpath_local)
    call expand_paths (os_data%whizard_modelpath_local)
    call expand_paths (os_data%whizard_omega_binpath_local)
    call expand_paths (os_data%latex)
    call expand_paths (os_data%gml)
    call expand_paths (os_data%dvips)
    call expand_paths (os_data%ps2pdf)
  contains
    subroutine expand_paths (string)
      type(string_t), intent(inout) :: string
      integer :: i
      do i = N_VARIABLES, 1, -1
         string = replace (string, variable(i), value(i), every=.true.)
      end do
    end subroutine expand_paths
  end subroutine os_data_expand_paths

  subroutine os_data_write (os_data, unit)
    type(os_data_t), intent(in) :: os_data
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "OS data:"
    write (u, *) "use_libtool    = ", os_data%use_libtool
    write (u, *) "use_testfiles  = ", os_data%use_testfiles
    write (u, *) "fc             = ", char (os_data%fc)
    write (u, *) "fcflags        = ", char (os_data%fcflags)
    write (u, *) "fcflags_pic    = ", char (os_data%fcflags_pic)
    write (u, *) "fc_src_ext     = ", char (os_data%fc_src_ext)
    write (u, *) "obj_ext        = ", char (os_data%obj_ext)
    write (u, *) "ld             = ", char (os_data%ld)
    write (u, *) "ldflags        = ", char (os_data%ldflags)
    write (u, *) "ldflags_so     = ", char (os_data%ldflags_so)
    write (u, *) "ldflags_static = ", char (os_data%ldflags_static)
    write (u, *) "shlib_ext      = ", char (os_data%shlib_ext)
    write (u, *) "prefix         = ", char (os_data%prefix)
    write (u, *) "exec_prefix    = ", char (os_data%exec_prefix)
    write (u, *) "bindir         = ", char (os_data%bindir)
    write (u, *) "libdir         = ", char (os_data%libdir)
    write (u, *) "includedir     = ", char (os_data%includedir)
    write (u, *) "datarootdir    = ", char (os_data%datarootdir)
    write (u, *) "whizard_omega_binpath  = ", &
         char (os_data%whizard_omega_binpath)
    write (u, *) "whizard_includes       = ", char (os_data%whizard_includes)
    write (u, *) "whizard_ldflags        = ", char (os_data%whizard_ldflags)
    write (u, *) "whizard_libtool        = ", char (os_data%whizard_libtool)
    write (u, *) "whizard_modelpath      = ", &
         char (os_data%whizard_modelpath)
    write (u, *) "whizard_models_libpath = ", &
         char (os_data%whizard_modelpath)
    write (u, *) "whizard_susypath       = ", char (os_data%whizard_includes)
    write (u, *) "whizard_gmlpath        = ", char (os_data%whizard_includes)
    write (u, *) "whizard_cutspath       = ", char (os_data%whizard_includes)
    write (u, *) "whizard_texpath        = ", char (os_data%whizard_includes)
    write (u, *) "whizard_testdatapath  = ", &
         char (os_data%whizard_testdatapath)
    write (u, *) "whizard_modelpath_local      = ", &
         char (os_data%whizard_modelpath_local)
    write (u, *) "whizard_models_libpath_local = ", &
         char (os_data%whizard_models_libpath_local)
    write (u, *) "whizard_omega_binpath_local  = ", &
         char (os_data%whizard_omega_binpath_local)
    write (u, *) "event_analysis_ps  = ", os_data%event_analysis_ps
    write (u, *) "event_analysis_pdf = ", os_data%event_analysis_pdf
    write (u, *) "latex  = ", char (os_data%latex)
    write (u, *) "gml    = ", char (os_data%gml)
    write (u, *) "dvips  = ", char (os_data%dvips)
    write (u, *) "ps2pdf = ", char (os_data%ps2pdf)
  end subroutine os_data_write

  subroutine read_dlerror (has_error, error)
    logical, intent(out) :: has_error
    type(string_t), intent(out) :: error
    type(c_ptr) :: err_cptr
    character(len=DLERROR_LEN, kind=c_char), pointer :: err_fptr
    integer :: str_end
    err_cptr = dlerror ()
    if (c_associated (err_cptr)) then
       call c_f_pointer (err_cptr, err_fptr)
       has_error = .true.
       str_end = scan (err_fptr, c_null_char)
       if (str_end > 0) then
          error = err_fptr(1:str_end-1)
       else
          error = err_fptr
       end if
    else
       has_error = .false.
       error = ""
    end if
  end subroutine read_dlerror

  subroutine dlaccess_init (dlaccess, prefix, libname, os_data)
    type(dlaccess_t), intent(out) :: dlaccess
    type(string_t), intent(in) :: prefix, libname
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: filename
    logical :: exist
    dlaccess%filename = libname
    filename = prefix // "/" // libname
    inquire (file=char(filename), exist=exist)
    if (.not. exist) then
       filename = prefix // "/.libs/" // libname
       inquire (file=char(filename), exist=exist)
       if (.not. exist) then
          dlaccess%has_error = .true.
          dlaccess%error = "Library '" // filename // "' not found"
          return
       end if
    end if
    dlaccess%handle = dlopen (char (filename) // c_null_char, 1_c_int)
    dlaccess%is_open = c_associated (dlaccess%handle)
    call read_dlerror (dlaccess%has_error, dlaccess%error)
  end subroutine dlaccess_init

  subroutine dlaccess_final (dlaccess)
    type(dlaccess_t), intent(inout) :: dlaccess
    integer(c_int) :: status
    if (dlaccess%is_open) then
       status = dlclose (dlaccess%handle)
       dlaccess%is_open = .false.
       call read_dlerror (dlaccess%has_error, dlaccess%error)
    end if
  end subroutine dlaccess_final

  function dlaccess_has_error (dlaccess) result (flag)
    logical :: flag
    type(dlaccess_t), intent(in) :: dlaccess
    flag = dlaccess%has_error
  end function dlaccess_has_error

  function dlaccess_get_error (dlaccess) result (error)
    type(string_t) :: error
    type(dlaccess_t), intent(in) :: dlaccess
    error = dlaccess%error
  end function dlaccess_get_error

  function dlaccess_get_c_funptr (dlaccess, fname) result (fptr)
    type(c_funptr) :: fptr
    type(dlaccess_t), intent(inout) :: dlaccess
    type(string_t), intent(in) :: fname
    fptr = dlsym (dlaccess%handle, char (fname) // c_null_char)
    call read_dlerror (dlaccess%has_error, dlaccess%error)
  end function dlaccess_get_c_funptr

  function dlaccess_is_open (dlaccess) result (flag)
    logical :: flag
    type(dlaccess_t), intent(in) :: dlaccess
    flag = dlaccess%is_open
  end function dlaccess_is_open

  subroutine os_system_call (command_string, status, verbose)
    type(string_t), intent(in) :: command_string
    integer, intent(out), optional :: status
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: stat
    verb = .false.;  if (present (verbose))  verb = verbose
    if (verb) &
         call msg_message ("command: " // char (command_string))
    stat = system (char (command_string) // c_null_char)
    if (present (status)) then
       status = stat
    else if (stat /= 0) then
       if (.not. verb) &
            call msg_message ("command: " // char (command_string))
       write (msg_buffer, "(A,I0)")  "Return code = ", stat
       call msg_message ()
       call msg_fatal ("System command returned with nonzero status code")
    end if
  end subroutine os_system_call

  function os_dir_exist (name) result (res)
    type(string_t), intent(in) :: name
    logical :: res
    integer :: status
    call os_system_call ('test -d "' // name // '"', status=status)
    res = status == 0
  end function os_dir_exist
  subroutine os_compile_shared (src, os_data, status)
    type(string_t), intent(in) :: src
    type(os_data_t), intent(in) :: os_data
    integer, intent(out), optional :: status
    type(string_t) :: command_string
    if (os_data%use_libtool) then
       command_string = &
            os_data%whizard_libtool // " --mode=compile " // &
            os_data%fc // " " // &
            "-c " // &
            os_data%whizard_includes // " " // &
            os_data%fcflags // " " // &
            "'" // src // os_data%fc_src_ext // "'"
    else
       command_string = &
            os_data%fc // " " // &
            "-c  " // &
            os_data%fcflags_pic // " " // &
            os_data%whizard_includes // " " // &
            os_data%fcflags // " " // &
            "'" // src // os_data%fc_src_ext // "'"
    end if
    call os_system_call (command_string, status)
  end subroutine os_compile_shared
   
  subroutine os_link_shared (objlist, lib, os_data, status)
    type(string_t), intent(in) :: objlist, lib
    type(os_data_t), intent(in) :: os_data
    integer, intent(out), optional :: status
    type(string_t) :: command_string
    if (os_data%use_libtool) then
       command_string = &
            os_data%whizard_libtool // " --mode=link " // &
            os_data%fc // " " // &
            "-module " // &
            "-rpath /usr/local/lib" // " " // &
            os_data%fcflags // " " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            "-o '" // lib // ".la' " // &
            objlist
    else
       command_string = &
            os_data%ld // " " // &
            os_data%ldflags_so // " " // &
            os_data%fcflags // " " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            "-o '" // lib // os_data%shlib_ext // "' " // &
            objlist
    end if
    call os_system_call (command_string, status)
  end subroutine os_link_shared

  subroutine os_link_static (objlist, exec_name, os_data, status)
    type(string_t), intent(in) :: objlist, exec_name
    type(os_data_t), intent(in) :: os_data
    integer, intent(out), optional :: status
    type(string_t) :: command_string
    if (os_data%use_libtool) then
       command_string = &
            os_data%whizard_libtool // " --mode=link " // &
            os_data%fc // " " // &
            "-static " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            os_data%ldflags_static // " " // &
            "-o '" // exec_name // "' " // &
            objlist
    else
       command_string = &
            os_data%ld // " " // &
            os_data%ldflags_so // " " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            os_data%ldflags_static // " " // &
            "-o '" // exec_name // "' " // &
            objlist
    end if
    call os_system_call (command_string, status)
  end subroutine os_link_static

  function os_get_dlname (lib, os_data, ignore, silent) result (dlname)
    type(string_t) :: dlname
    type(string_t), intent(in) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: ignore, silent
    type(string_t) :: filename
    type(string_t) :: buffer
    logical :: exist, required, quiet
    integer :: u
    u = free_unit ()
    if (present (ignore)) then
       required = .not. ignore
    else
       required = .true.
    end if
         if (present (silent)) then
       quiet = silent
    else
       quiet = .false.
    end if
    if (os_data%use_libtool) then
       filename = lib // ".la"
       inquire (file=char(filename), exist=exist)
       if (exist) then
          open (unit=u, file=char(filename), action="read", status="old")
          SCAN_LTFILE: do
             call get (u, buffer)
             if (extract (buffer, 1, 7) == "dlname=") then
                dlname = extract (buffer, 9)
                dlname = remove (dlname, len (dlname))
                exit SCAN_LTFILE
             end if
          end do SCAN_LTFILE
          close (u)
       else if (required) then
          if (.not. quiet) call msg_fatal (" Library '" // char (lib) &
               // "': libtool archive not found")
          dlname = ""
       else
          if (.not. quiet) call msg_message ("[No compiled library '" &
               // char (lib) // "']")
          dlname = ""
       end if
    else
       dlname = lib // os_data%shlib_ext
       inquire (file=char(dlname), exist=exist)
       if (.not. exist) then
          if (required) then
             if (.not. quiet) call msg_fatal (" Library '" // char (lib) &
                  // "' not found")
          else
             if (.not. quiet) call msg_message &
                ("[No compiled process library '" // char (lib) // "']")
             dlname = ""
          end if
       end if
    end if
  end function os_get_dlname

  subroutine os_interface_test ()
    call os_interface_test1 ()
  end subroutine os_interface_test

  subroutine os_interface_test1 ()
    type(dlaccess_t) :: dlaccess
    type(string_t) :: fname, libname
    type(os_data_t) :: os_data
    type(string_t) :: filename_src, filename_obj
    interface
       function so_test_proc (i) result (j) bind(C)
         import c_int
         integer(c_int), intent(in) :: i
         integer(c_int) :: j
       end function so_test_proc
    end interface
    procedure(so_test_proc), pointer :: so_test => null ()
    type(c_funptr) :: c_fptr
    integer :: u
    integer(c_int) :: i
    call os_data_init (os_data)
    fname = "so_test"
    filename_src = fname // os_data%fc_src_ext
    filename_obj = fname // os_data%obj_ext
    libname = fname // os_data%shlib_ext
    print *, "* write source file 'so_test.f90'"
    u = free_unit ()
    open (unit=u, file=char(filename_src), action="write")
    write (u, "(A)")  "function so_test (i) result (j) bind(C)"
    write (u, "(A)")  "  integer(c_int), intent(in) :: i"
    write (u, "(A)")  "  integer(c_int) :: j"
    write (u, "(A)")  "  j = 2 * i"
    write (u, "(A)")  "end function so_test"
    close (u)
    print *, "* compile and link as 'so_test.so'"
    call os_compile_shared (fname, os_data)
    call os_link_shared (filename_obj, fname, os_data)
    print *, "* load library 'so_test.so'"
    call dlaccess_init (dlaccess, var_str ("."), libname, os_data)
    if (dlaccess_is_open (dlaccess)) then
       print *, "  success"
    else
       print *, "  failure"
    end if
    print *, "* load symbol 'so_test'"
    c_fptr = dlaccess_get_c_funptr (dlaccess, fname)
    if (c_associated (c_fptr)) then
       print *, "  success"
    else
       print *, "  failure"
    end if
    call c_f_procpointer (c_fptr, so_test)
    print *, "* Execute function from 'so_test.so'"
    i = 7
    print *, "  input = ", i
    print *, "  result =", so_test(i)
    print *, "* Cleanup"
    call dlaccess_final (dlaccess)
  end subroutine os_interface_test1


end module os_interface
