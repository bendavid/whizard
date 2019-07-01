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

module os_interface

  use, intrinsic :: iso_c_binding !NODEP!

  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use diagnostics
  use system_defs, only: DLERROR_LEN, ENVVAR_LEN
  use system_dependencies
  
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
  public :: os_file_exist
  public :: os_compile_shared
  public :: os_link_shared
  public :: os_link_static
  public :: os_get_dlname
  public :: openmp_set_num_threads_verbose
  public :: os_interface_test

  type :: paths_t
     type(string_t) :: prefix
     type(string_t) :: exec_prefix
     type(string_t) :: bindir
     type(string_t) :: libdir
     type(string_t) :: includedir
     type(string_t) :: datarootdir
     type(string_t) :: localprefix
     type(string_t) :: libtool
     type(string_t) :: lhapdfdir
  end type paths_t

  type :: os_data_t
     logical :: use_libtool
     logical :: use_testfiles
     type(string_t) :: fc
     type(string_t) :: fcflags
     type(string_t) :: fcflags_pic
     type(string_t) :: fc_src_ext
     type(string_t) :: cc
     type(string_t) :: cflags
     type(string_t) :: cflags_pic
     type(string_t) :: obj_ext
     type(string_t) :: ld
     type(string_t) :: ldflags
     type(string_t) :: ldflags_so
     type(string_t) :: ldflags_static
     type(string_t) :: ldflags_hepmc
     type(string_t) :: ldflags_hoppet
     type(string_t) :: shlib_ext
     type(string_t) :: makeflags
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
     type(string_t) :: whizard_circe2path
     type(string_t) :: whizard_beamsimpath
     type(string_t) :: whizard_mulipath
     type(string_t) :: pdf_builtin_datapath
     logical :: event_analysis = .false.
     logical :: event_analysis_ps  = .false.
     logical :: event_analysis_pdf = .false.
     type(string_t) :: latex
     type(string_t) :: mpost
     type(string_t) :: gml
     type(string_t) :: dvips
     type(string_t) :: ps2pdf
     type(string_t) :: gosampath
     type(string_t) :: golempath
     type(string_t) :: formpath
     type(string_t) :: qgrafpath
     type(string_t) :: ninjapath
     type(string_t) :: samuraipath
  end type os_data_t

  type :: dlaccess_t
     private
     type(string_t) :: filename
     type(c_ptr) :: handle = c_null_ptr
     logical :: is_open = .false.
     logical :: has_error = .false.
     type(string_t) :: error
   contains
     procedure :: write => dlaccess_write
     procedure :: init => dlaccess_init
     procedure :: final => dlaccess_final
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
    paths%libtool = ""
    paths%lhapdfdir = ""
  end subroutine paths_init

  subroutine os_data_init (os_data, paths)
    type(os_data_t), intent(out) :: os_data
    type(paths_t), intent(in), optional :: paths
    character(len=ENVVAR_LEN) :: home
    type(string_t) :: localprefix, local_includes
    os_data%use_libtool = .true.
    inquire (file = "TESTFLAG", exist = os_data%use_testfiles)
    call get_environment_variable ("HOME", home)
    if (present(paths)) then
       if (paths%localprefix == "") then
          localprefix = trim (home) // "/.whizard"
       else
          localprefix = paths%localprefix
       end if
    else
       localprefix = trim (home) // "/.whizard"
    end if
    local_includes = localprefix // "/lib/whizard/mod/models"
    os_data%whizard_modelpath_local = localprefix // "/share/whizard/models"
    os_data%whizard_models_libpath_local = localprefix // "/lib/whizard/models"
    os_data%whizard_omega_binpath_local = localprefix // "/bin"
    os_data%fc             = DEFAULT_FC
    os_data%fcflags        = DEFAULT_FCFLAGS
    os_data%fcflags_pic    = DEFAULT_FCFLAGS_PIC
    os_data%fc_src_ext     = DEFAULT_FC_SRC_EXT
    os_data%cc             = DEFAULT_CC
    os_data%cflags         = DEFAULT_CFLAGS
    os_data%cflags_pic     = DEFAULT_CFLAGS_PIC
    os_data%obj_ext        = DEFAULT_OBJ_EXT
    os_data%ld             = DEFAULT_LD
    os_data%ldflags        = DEFAULT_LDFLAGS
    os_data%ldflags_so     = DEFAULT_LDFLAGS_SO
    os_data%ldflags_static = DEFAULT_LDFLAGS_STATIC
    os_data%ldflags_hepmc  = DEFAULT_LDFLAGS_HEPMC
    os_data%ldflags_hoppet = DEFAULT_LDFLAGS_HOPPET
    os_data%shlib_ext      = DEFAULT_SHLIB_EXT
    os_data%makeflags      = DEFAULT_MAKEFLAGS
    os_data%prefix      = PREFIX
    os_data%exec_prefix = EXEC_PREFIX
    os_data%bindir      = BINDIR
    os_data%libdir      = LIBDIR
    os_data%includedir  = INCLUDEDIR
    os_data%datarootdir = DATAROOTDIR
    if (present (paths)) then
       if (paths%prefix      /= "")  os_data%prefix      = paths%prefix
       if (paths%exec_prefix /= "")  os_data%exec_prefix = paths%exec_prefix
       if (paths%bindir      /= "")  os_data%bindir      = paths%bindir
       if (paths%libdir      /= "")  os_data%libdir      = paths%libdir
       if (paths%includedir  /= "")  os_data%includedir  = paths%includedir
       if (paths%datarootdir /= "")  os_data%datarootdir = paths%datarootdir
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
       os_data%whizard_circe2path     = WHIZARD_TEST_CIRCE2PATH
       os_data%whizard_beamsimpath    = WHIZARD_TEST_BEAMSIMPATH
       os_data%whizard_mulipath       = WHIZARD_TEST_MULIPATH
       os_data%pdf_builtin_datapath   = PDF_BUILTIN_TEST_DATAPATH
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
       if(present(paths)) then
          if (paths%libtool /= "")  os_data%whizard_libtool = paths%libtool
       end if
       os_data%whizard_modelpath      = WHIZARD_MODELPATH
       os_data%whizard_models_libpath = WHIZARD_MODELS_LIBPATH
       os_data%whizard_susypath       = WHIZARD_SUSYPATH
       os_data%whizard_gmlpath        = WHIZARD_GMLPATH
       os_data%whizard_cutspath       = WHIZARD_CUTSPATH
       os_data%whizard_texpath        = WHIZARD_TEXPATH
       os_data%whizard_testdatapath   = WHIZARD_TESTDATAPATH
       os_data%whizard_circe2path     = WHIZARD_CIRCE2PATH
       os_data%whizard_beamsimpath    = WHIZARD_BEAMSIMPATH
       os_data%whizard_mulipath       = WHIZARD_MULIPATH
       os_data%pdf_builtin_datapath   = PDF_BUILTIN_DATAPATH
    end if
    os_data%event_analysis     = EVENT_ANALYSIS     == "yes"
    os_data%event_analysis_ps  = EVENT_ANALYSIS_PS  == "yes"
    os_data%event_analysis_pdf = EVENT_ANALYSIS_PDF == "yes"
    os_data%latex  = PRG_LATEX // " " // OPT_LATEX
    os_data%mpost  = PRG_MPOST // " " // OPT_MPOST
    os_data%gml    = os_data%whizard_gmlpath // "/gml" // " " // OPT_MPOST &
         // " " // "--gmldir " // os_data%whizard_gmlpath
    os_data%dvips  = PRG_DVIPS
    os_data%ps2pdf = PRG_PS2PDF
    call os_data_expand_paths (os_data)
    os_data%gosampath = GOSAM_DIR
    os_data%golempath = GOLEM_DIR
    os_data%formpath = FORM_DIR
    os_data%qgrafpath = QGRAF_DIR
    os_data%ninjapath = NINJA_DIR
    os_data%samuraipath = SAMURAI_DIR
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
    call expand_paths (os_data%whizard_circe2path)
    call expand_paths (os_data%whizard_beamsimpath)
    call expand_paths (os_data%whizard_mulipath)
    call expand_paths (os_data%whizard_models_libpath_local)
    call expand_paths (os_data%whizard_modelpath_local)
    call expand_paths (os_data%whizard_omega_binpath_local)
    call expand_paths (os_data%pdf_builtin_datapath)
    call expand_paths (os_data%latex)
    call expand_paths (os_data%mpost)    
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
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "OS data:"
    write (u, *) "use_libtool    = ", os_data%use_libtool
    write (u, *) "use_testfiles  = ", os_data%use_testfiles
    write (u, *) "fc             = ", char (os_data%fc)
    write (u, *) "fcflags        = ", char (os_data%fcflags)
    write (u, *) "fcflags_pic    = ", char (os_data%fcflags_pic)
    write (u, *) "fc_src_ext     = ", char (os_data%fc_src_ext)
    write (u, *) "cc             = ", char (os_data%cc)
    write (u, *) "cflags         = ", char (os_data%cflags)
    write (u, *) "cflags_pic     = ", char (os_data%cflags_pic)
    write (u, *) "obj_ext        = ", char (os_data%obj_ext)
    write (u, *) "ld             = ", char (os_data%ld)
    write (u, *) "ldflags        = ", char (os_data%ldflags)
    write (u, *) "ldflags_so     = ", char (os_data%ldflags_so)
    write (u, *) "ldflags_static = ", char (os_data%ldflags_static)
    write (u, *) "ldflags_hepmc  = ", char (os_data%ldflags_hepmc)
    write (u, *) "ldflags_hoppet = ", char (os_data%ldflags_hoppet)    
    write (u, *) "shlib_ext      = ", char (os_data%shlib_ext)
    write (u, *) "makeflags      = ", char (os_data%makeflags)
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
    write (u, *) "whizard_susypath       = ", char (os_data%whizard_susypath)
    write (u, *) "whizard_gmlpath        = ", char (os_data%whizard_gmlpath)
    write (u, *) "whizard_cutspath       = ", char (os_data%whizard_cutspath)
    write (u, *) "whizard_texpath        = ", char (os_data%whizard_texpath)
    write (u, *) "whizard_circe2path     = ", char (os_data%whizard_circe2path)
    write (u, *) "whizard_beamsimpath    = ", char (os_data%whizard_beamsimpath)
    write (u, *) "whizard_mulipath    = ", char (os_data%whizard_mulipath)
    write (u, *) "whizard_testdatapath  = ", &
         char (os_data%whizard_testdatapath)
    write (u, *) "whizard_modelpath_local      = ", &
         char (os_data%whizard_modelpath_local)
    write (u, *) "whizard_models_libpath_local = ", &
         char (os_data%whizard_models_libpath_local)
    write (u, *) "whizard_omega_binpath_local  = ", &
         char (os_data%whizard_omega_binpath_local)
    write (u, *) "event_analysis     = ", os_data%event_analysis
    write (u, *) "event_analysis_ps  = ", os_data%event_analysis_ps
    write (u, *) "event_analysis_pdf = ", os_data%event_analysis_pdf
    write (u, *) "latex  = ", char (os_data%latex)
    write (u, *) "mpost  = ", char (os_data%mpost)    
    write (u, *) "gml    = ", char (os_data%gml)
    write (u, *) "dvips  = ", char (os_data%dvips)
    write (u, *) "ps2pdf = ", char (os_data%ps2pdf)
    if (os_data%gosampath /= "") then
       write (u, *) "gosam   = ", char (os_data%gosampath)
       write (u, *) "golem   = ", char (os_data%golempath)
       write (u, *) "form    = ", char (os_data%formpath)
       write (u, *) "qgraf   = ", char (os_data%qgrafpath)
       write (u, *) "ninja   = ", char (os_data%ninjapath)
       write (u, *) "samurai = ", char (os_data%samuraipath)
    end if
  end subroutine os_data_write

  subroutine dlaccess_write (object, unit)
    class(dlaccess_t), intent(in) :: object
    integer, intent(in) :: unit
    write (unit, "(1x,A)")  "DL access info:"
    write (unit, "(3x,A,L1)")   "is open   = ", object%is_open
    if (object%has_error) then
       write (unit, "(3x,A,A,A)")  "error     = '", char (object%error), "'"
    else
       write (unit, "(3x,A)")      "error     = [none]"
    end if
  end subroutine dlaccess_write
    
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
    class(dlaccess_t), intent(out) :: dlaccess
    type(string_t), intent(in) :: prefix, libname
    type(os_data_t), intent(in), optional :: os_data
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
    dlaccess%handle = dlopen (char (filename) // c_null_char, ior ( &
       RTLD_LAZY, RTLD_LOCAL))
    dlaccess%is_open = c_associated (dlaccess%handle)
    call read_dlerror (dlaccess%has_error, dlaccess%error)
  end subroutine dlaccess_init

  subroutine dlaccess_final (dlaccess)
    class(dlaccess_t), intent(inout) :: dlaccess
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
  function os_file_exist (name) result (exist)
    type(string_t), intent(in) :: name
!    logical, intent(in), optional :: verb
    logical :: exist
!    integer :: status
!    call  os_system_call ('test -f "' // name // '"', status=status, verbose=verb)
!    res = (status == 0)
    inquire (file = char (name), exist=exist)
  end function os_file_exist

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
            "-static-libtool-libs " // &
            os_data%fcflags // " " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            os_data%ldflags_static // " " // &
            "-o '" // exec_name // "' " // &
            objlist // " " // &
            os_data%ldflags_hepmc // " " // &
            os_data%ldflags_hoppet
    else
       command_string = &
            os_data%ld // " " // &
            os_data%ldflags_so // " " // &
            os_data%fcflags // " " // &
            os_data%whizard_ldflags // " " // &
            os_data%ldflags // " " // &
            os_data%ldflags_static // " " // &
            "-o '" // exec_name // "' " // &
            objlist // " " // &
            os_data%ldflags_hepmc // " " // &
            os_data%ldflags_hoppet
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

  subroutine openmp_set_num_threads_verbose (num_threads, openmp_logging)
    integer, intent(in) :: num_threads
    integer :: n_threads
    logical, intent(in), optional :: openmp_logging
    logical :: logging
    if (present (openmp_logging)) then
       logging = openmp_logging
    else
       logging = .true.
    end if
    n_threads = num_threads
    if (openmp_is_active ()) then
       if (num_threads == 1) then
          if (logging) then
             write (msg_buffer, "(A,I0,A)")  "OpenMP: Using ", num_threads, &
                  " thread"
             call msg_message
          end if
          n_threads = num_threads
       else if (num_threads > 1) then
          if (logging) then
             write (msg_buffer, "(A,I0,A)")  "OpenMP: Using ", num_threads, &
                  " threads"
             call msg_message
          end if
          n_threads = num_threads
       else
          if (logging) then
             write (msg_buffer, "(A,I0,A)")  "OpenMP: " &
                  // "Illegal value of openmp_num_threads (", num_threads, &
               ") ignored"
             call msg_error
          end if
          n_threads = openmp_get_default_max_threads ()
          if (logging) then
             write (msg_buffer, "(A,I0,A)")  "OpenMP: Using ", &
                  n_threads, " threads"
             call msg_message          
          end if
       end if
       if (n_threads > openmp_get_default_max_threads ()) then
          if (logging) then
             write (msg_buffer, "(A,I0)")  "OpenMP: " &
                  // "Number of threads is greater than library default of ", &
                  openmp_get_default_max_threads ()
             call msg_warning
          end if
       end if
       call openmp_set_num_threads (n_threads)
    else if (num_threads /= 1) then
       if (logging) then
          write (msg_buffer, "(A,I0,A)")  "openmp_num_threads set to ", &
               num_threads, ", but OpenMP is not active: ignored"
          call msg_warning
       end if
    end if
  end subroutine openmp_set_num_threads_verbose

  subroutine os_interface_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (os_interface_1, "os_interface_1", &
         "check OS interface routines", &
         u, results)
  end subroutine os_interface_test


  subroutine os_interface_1 (u)
    integer, intent(in) :: u
    type(dlaccess_t) :: dlaccess
    type(string_t) :: fname, libname, ext
    type(os_data_t) :: os_data
    type(string_t) :: filename_src, filename_obj
    abstract interface
       function so_test_proc (i) result (j) bind(C)
         import c_int
         integer(c_int), intent(in) :: i
         integer(c_int) :: j
       end function so_test_proc
    end interface
    procedure(so_test_proc), pointer :: so_test => null ()
    type(c_funptr) :: c_fptr
    integer :: unit
    integer(c_int) :: i
    call os_data_init (os_data)
    fname = "so_test"
    filename_src = fname // os_data%fc_src_ext
    if (os_data%use_libtool) then
       ext = ".lo"
    else
       ext = os_data%obj_ext
    end if
    filename_obj = fname // ext
    libname = fname // os_data%shlib_ext
    
    write (u, "(A)")  "* Test output: OS interface"
    write (u, "(A)")  "*   Purpose: check os_interface routines"
    write (u, "(A)")      
            
    write (u, "(A)")  "* write source file 'so_test.f90'"
    write (u, "(A)")
    unit = free_unit ()
    open (unit=unit, file=char(filename_src), action="write")
    write (unit, "(A)")  "function so_test (i) result (j) bind(C)"
    write (unit, "(A)")  "  use iso_c_binding"
    write (unit, "(A)")  "  integer(c_int), intent(in) :: i"
    write (unit, "(A)")  "  integer(c_int) :: j"
    write (unit, "(A)")  "  j = 2 * i"
    write (unit, "(A)")  "end function so_test"
    close (unit)
    write (u, "(A)")  "* compile and link as 'so_test.so/dylib'"
    write (u, "(A)")
    call os_compile_shared (fname, os_data)
    call os_link_shared (filename_obj, fname, os_data)
    write (u, "(A)")  "* load library 'so_test.so/dylib'"
    write (u, "(A)")
    call dlaccess_init (dlaccess, var_str ("."), libname, os_data)
    if (dlaccess_is_open (dlaccess)) then
       write (u, "(A)") "     success"
    else
       write (u, "(A)") "     failure"
    end if
    write (u, "(A)")  "* load symbol 'so_test'"
    write (u, "(A)")
    c_fptr = dlaccess_get_c_funptr (dlaccess, fname)
    if (c_associated (c_fptr)) then
       write (u, "(A)") "     success"
    else
       write (u, "(A)") "     failure"
    end if
    call c_f_procpointer (c_fptr, so_test)
    write (u, "(A)") "* Execute function from 'so_test.so/dylib'"
    i = 7
    write (u, "(A,1x,I1)")  "     input  = ", i
    write (u, "(A,1x,I1)")  "     result = ", so_test(i)
    if (so_test(i) / i .ne. 2) then
       write (u, "(A)")  "* Compiling and linking ISO C functions failed."
    else
       write (u, "(A)")  "* Successful."
    end if
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    call dlaccess_final (dlaccess)
  end subroutine os_interface_1


end module os_interface
