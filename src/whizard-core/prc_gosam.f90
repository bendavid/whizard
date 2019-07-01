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

module prc_gosam
  
  use, intrinsic :: iso_c_binding !NODEP!

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string  
  use io_units
  use constants
  use system_defs, only: TAB
  use system_dependencies
  use physics_defs
  use diagnostics
  use os_interface
  use lorentz
  use interactions
  use pdg_arrays
  use sm_qcd
  use flavors
  use model_data
!  use models
  use md5
  
  use process_constants
  use prclib_interfaces
  use prc_core_def
  use prc_core

  use blha_config
  use loop_archive

  implicit none
  private

  character(10), dimension(5), parameter ::  &
             lib_suffix = [character(10) :: &
             '.a', '.la', '.so', '.so.0', '.so.0.0.0']

  integer, parameter :: GOSAM_PARAMETER_LIMIT = 10
  integer, parameter :: GOSAM_MOMENTUM_LIMIT = 50
  integer, parameter :: GOSAM_RESULTS_LIMIT = 60


  public :: gosam_writer_t
  public :: gosam_def_t
  public :: prc_gosam_t
  public :: gosam_writer_template_t

  type, extends (prc_writer_f_module_t) :: gosam_writer_t
    type(string_t) :: gosam_dir
    type(string_t) :: golem_dir
    type(string_t) :: samurai_dir
    type(string_t) :: ninja_dir
    type(string_t) :: form_dir
    type(string_t) :: qgraf_dir
    type(blha_configuration_t) :: blha_cfg
  contains
    procedure :: write_wrapper => gosam_write_wrapper
    procedure :: write_interface => gosam_write_interface
    procedure :: write_source_code => gosam_write_source_code
    procedure :: write_makefile_code => gosam_write_makefile_code
    procedure, nopass :: type_name => gosam_writer_type_name
    procedure :: write => gosam_writer_write
    procedure :: init => gosam_writer_init
    procedure :: generate_configuration_file => &
              gosam_writer_generate_configuration_file
    procedure :: generate_olp_file => gosam_writer_generate_olp_file
    procedure :: get_process_string => gosam_writer_get_process_string
    procedure :: get_n_proc => gosam_writer_get_n_proc
  end type gosam_writer_t

  type, extends (prc_core_def_t) :: gosam_def_t
    type(string_t) :: basename
    logical :: execute_olp = .true.
  contains
    procedure :: init => gosam_def_init
    procedure, nopass :: type_string => gosam_def_type_string
    procedure :: write => gosam_def_write
    procedure :: read => gosam_def_read
    procedure :: allocate_driver => gosam_def_allocate_driver
    procedure, nopass :: get_features => gosam_def_get_features
    procedure :: connect => gosam_def_connect
  end type gosam_def_t

  type, extends (prc_core_driver_t) :: gosam_driver_t 
    type(string_t) :: gosam_dir
    type(string_t) :: olp_file
    type(string_t) :: olc_file
    type(string_t) :: olp_dir
    type(loop_archive_t) :: loop_archive
    procedure(olp_start),nopass,  pointer :: &
              gosam_olp_start => null ()
    procedure(olp_eval), nopass, pointer :: &
              gosam_olp_eval => null()
    procedure(olp_info), nopass, pointer :: &
              gosam_olp_info => null ()
    procedure(olp_set_parameter), nopass, pointer :: &
              gosam_olp_set_parameter => null ()
    procedure(olp_eval2), nopass, pointer :: &
              gosam_olp_eval2 => null ()
    procedure(olp_option), nopass, pointer :: &
              gosam_olp_option => null ()
    procedure(olp_polvec), nopass, pointer :: &
              gosam_olp_polvec => null ()
    procedure(olp_finalize), nopass, pointer :: &
              gosam_olp_finalize => null ()
    procedure(olp_print_parameter), nopass, pointer :: &
              gosam_olp_print_parameter => null ()
  contains
    procedure, nopass :: type_name => gosam_driver_type_name
    procedure :: init => gosam_driver_init
    procedure :: execute_olp_file => gosam_driver_execute_olp_file
    procedure :: load => gosam_driver_load
  end type gosam_driver_t

  type, extends (prc_core_t) :: prc_gosam_t
    type(qcd_t) :: qcd
    integer :: n_proc
    real(default) :: maximum_accuracy = 10000.0
    logical :: initialized = .false.
    integer :: i_born, i_cc, i_sc, i_virt
  contains
    procedure :: write => prc_gosam_write
    procedure :: needs_mcset => prc_gosam_needs_mcset
    procedure :: get_n_terms => prc_gosam_get_n_terms
    procedure :: is_allowed => prc_gosam_is_allowed
    procedure :: compute_hard_kinematics => prc_gosam_compute_hard_kinematics
    procedure :: compute_eff_kinematics => prc_gosam_compute_eff_kinematics
    procedure :: compute_amplitude => prc_gosam_compute_amplitude
    procedure :: recover_kinematics => prc_gosam_recover_kinematics
     procedure :: init_writer => prc_gosam_init_writer
    procedure :: init_driver => prc_gosam_init_driver
    procedure :: set_initialized => prc_gosam_set_initialized
    procedure :: set_parameters => prc_gosam_set_parameters
    procedure :: compute_sqme_virt => prc_gosam_compute_sqme_virt
    procedure :: compute_sqme_cc => prc_gosam_compute_sqme_cc
    procedure :: compute_sqme_sc => prc_gosam_compute_sqme_sc
    procedure :: fill_constants => prc_gosam_fill_constants
    procedure :: allocate_workspace => prc_gosam_allocate_workspace
    procedure :: get_alpha_s => prc_gosam_get_alpha_s
    procedure :: set_n_proc => prc_gosam_set_n_proc
    procedure :: get_n_proc => prc_gosam_get_n_proc
  end type prc_gosam_t

  type, extends (workspace_t) :: gosam_state_t
    logical :: new_kinematics = .true.
    real(default) :: alpha_qcd = -1
  contains
    procedure :: write => gosam_state_write
    procedure :: reset_new_kinematics => gosam_state_reset_new_kinematics
  end type gosam_state_t

  type :: gosam_writer_template_t
    type(process_constants_t) :: data
    logical :: compute_loops = .true.
    logical :: compute_correlations = .false.
    integer :: alpha_power, alphas_power
    logical :: new
  end type gosam_writer_template_t


  interface 
    subroutine olp_start (contract_file_name, ierr) bind (C,name="OLP_Start")
      import
      character(kind=c_char, len=1), intent(in) :: contract_file_name
      integer(kind=c_int), intent(out) :: ierr
    end subroutine olp_start
  end interface

  interface
    subroutine olp_eval (label, momenta, mu, parameters, res) &
         bind (C,name="OLP_EvalSubProcess")
      import
      integer(kind=c_int), value, intent(in) :: label
      real(kind=c_double), value, intent(in) :: mu
      real(kind=c_double), dimension(GOSAM_MOMENTUM_LIMIT), intent(in) :: momenta
      real(kind=c_double), dimension(GOSAM_PARAMETER_LIMIT), intent(in) :: parameters
      real(kind=c_double), dimension(GOSAM_RESULTS_LIMIT), intent(out) :: res
    end subroutine olp_eval
  end interface

  interface
    subroutine olp_info (olp_file, olp_version, message) &
         bind(C,name="OLP_Info")
      import
      character(kind=c_char), intent(inout), dimension(15) :: olp_file
      character(kind=c_char), intent(inout), dimension(15) :: olp_version
      character(kind=c_char), intent(inout), dimension(255) :: message
    end subroutine olp_info
  end interface

  interface
    subroutine olp_set_parameter &
         (variable_name, real_part, complex_part, success) &
            bind(C,name="OLP_SetParameter")
      import
      character(kind=c_char,len=1), intent(in) :: variable_name
      real(kind=c_double), intent(in) :: real_part, complex_part
      integer(kind=c_int), intent(out) :: success
    end subroutine olp_set_parameter
  end interface

  interface
    subroutine olp_eval2 (label, momenta, mu, res, acc) &
         bind(C,name="OLP_EvalSubProcess2")
      import
      integer(kind=c_int), intent(in) :: label
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(GOSAM_MOMENTUM_LIMIT), intent(in) :: momenta
      real(kind=c_double), dimension(GOSAM_RESULTS_LIMIT), intent(out) :: res
      real(kind=c_double), intent(out) :: acc
    end subroutine olp_eval2
  end interface

  interface
    subroutine olp_option (line, stat) bind(C,name="OLP_Option")
      import
      character(kind=c_char, len=1), intent(in) :: line
      integer(kind=c_int), intent(out) :: stat
    end subroutine
  end interface

  interface
    subroutine olp_polvec (p, q, eps) bind(C,name="OLP_Polvec")
      import
      real(kind=c_double), dimension(0:3), intent(in) :: p, q
      real(kind=c_double), dimension(0:7), intent(out) :: eps
    end subroutine
  end interface

  interface
    subroutine olp_finalize () bind(C,name="OLP_Finalize")
      import
    end subroutine olp_finalize
  end interface

  interface
    subroutine olp_print_parameter (filename) bind(C,name="OLP_PrintParameter")
      import
      character(kind=c_char, len=1), intent(in) :: filename
    end subroutine olp_print_parameter
  end interface


contains

  subroutine gosam_def_init (object, basename)
    class(gosam_def_t), intent(inout) :: object
    type(string_t), intent(in) :: basename
    object%basename = basename
    allocate (gosam_writer_t :: object%writer)
  end subroutine gosam_def_init

  function gosam_def_type_string () result (string)
    type(string_t) :: string
    string = "gosam"
  end function gosam_def_type_string

  subroutine gosam_def_write (object, unit)
    class(gosam_def_t), intent(in) :: object
    integer, intent(in) :: unit
    select type (writer => object%writer)
    type is (gosam_writer_t)
      call writer%write (unit)
    end select
  end subroutine gosam_def_write

  subroutine gosam_def_read (object, unit)
    class(gosam_def_t), intent(out) :: object
    integer, intent(in) :: unit
    call msg_bug ("GoSam process definition: input not supported yet")
  end subroutine gosam_def_read


  subroutine gosam_def_allocate_driver (object, driver, basename)
    class(gosam_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    if (.not. allocated (driver)) allocate (gosam_driver_t :: driver)
  end subroutine gosam_def_allocate_driver

  subroutine gosam_def_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (6))
    features = [ &
         var_str ("init"), &
         var_str ("update_alpha_s"), &
         var_str ("reset_helicity_selection"), &
         var_str ("is_allowed"), &
         var_str ("new_event"), &
         var_str ("get_amplitude")]
  end subroutine gosam_def_get_features 

  subroutine gosam_def_connect (def, lib_driver, i, proc_driver)   
    class(gosam_def_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
    call msg_bug ("No implementation for gosam_def_connect")
  end subroutine gosam_def_connect

  subroutine gosam_write_wrapper (writer, unit, id, feature)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id, feature
    call msg_bug ("Gosam write wrapper: Do not know what to do!")
  end subroutine gosam_write_wrapper

  subroutine gosam_write_interface (writer, unit, id, feature)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id, feature
  end subroutine gosam_write_interface 

  subroutine gosam_write_source_code (writer, id)
    class(gosam_writer_t), intent(in) :: writer
    type(string_t), intent(in) :: id
    call msg_bug ("Gosam does not have to be " // &
         "supported with Whizard-generated code")
  end subroutine gosam_write_source_code

  subroutine gosam_write_makefile_code (writer, unit, id, os_data, testflag)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: testflag
    call msg_bug ("Gosam does not have to be " // &
         "supported with a Whizard-generated Makefile")
  end subroutine gosam_write_makefile_code

  function gosam_writer_type_name () result (string)
    type(string_t) :: string
    string = "gosam"
  end function gosam_writer_type_name

  subroutine gosam_writer_write (writer, unit)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit    
    write (unit, "(1x,A)")  char (writer%get_process_string ())
  end subroutine gosam_writer_write

  subroutine gosam_writer_init (writer, basename, flv_states, amp_type, & 
                                alpha_power, alphas_power, &
                                os_data, model, ex_olp)
    class(gosam_writer_t), intent(inout) :: writer
    type(string_t), intent(in) :: basename
    integer, intent(in), dimension(:,:) :: flv_states
    integer, intent(in), dimension(:), optional :: amp_type
    integer, intent(in) :: alpha_power, alphas_power
    type(os_data_t), intent(in) :: os_data
    class(model_data_t), intent(in), pointer :: model
    logical, intent(out) :: ex_olp
    ex_olp = .false.

    writer%gosam_dir = GOSAM_DIR
    writer%golem_dir = GOLEM_DIR 
    writer%samurai_dir = SAMURAI_DIR
    writer%ninja_dir = NINJA_DIR
    writer%form_dir = FORM_DIR
    writer%qgraf_dir = QGRAF_DIR

    call check_file_change (var_str ('golem.in'), 'config')
  contains
    subroutine check_file_change (filename, file_type)
      type(string_t), intent(in) :: filename
      character(*), intent(in) :: file_type
      logical :: exist
      integer :: unit
      character(len=32) :: md5sum1, md5sum2
      inquire (file = char (filename), exist = exist)
      if (exist) then
        unit = free_unit ()
        open (unit, file = char (filename), status = 'old', action = 'read')
        md5sum1 = md5sum (unit)
        close (unit)
        open (unit, file = char (filename // '_tmp'), status = 'replace',&
              action = 'readwrite')
        call generate_file (unit, file_type)
        rewind (unit)
        md5sum2 = md5sum (unit)
        close (unit)
        if (md5sum1 == md5sum2) then
          call os_system_call ('rm ' // filename // '_tmp')
          ex_olp = ex_olp .or. .false.
        else
          call os_system_call ('mv ' // filename // '_tmp' &
                  // ' ' // filename)
          ex_olp = ex_olp .or. .true.
        end if
      else
        unit = free_unit ()
        open (unit, file = char (filename), status = 'new', action = 'write')
        call generate_file (unit, file_type)
        close(unit)
        ex_olp = ex_olp .or..true.
      end if
    end subroutine check_file_change

    subroutine generate_file (unit, file_type)
      integer, intent(in) :: unit
      character(*), intent(in) :: file_type
      select case (file_type)
      case ('config')
        call writer%generate_configuration_file (unit, os_data)
      case ('olp')
        call writer%generate_olp_file (unit, os_data)
      case default
        call msg_fatal &
          ('Gosam writer: can only create configuration- and olp-files')
      end select
    end subroutine generate_file
  end subroutine gosam_writer_init  

  function gosam_driver_type_name () result (string)
    type(string_t) :: string
    string = "gosam"
  end function gosam_driver_type_name

  subroutine gosam_driver_init (object, os_data, olp_file, &
                                olc_file, olp_dir)
    class(gosam_driver_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: olp_file, olc_file, olp_dir
    object%gosam_dir = GOSAM_DIR
    object%olp_file = olp_file
    object%olc_file = olc_file
    object%olp_dir = olp_dir
    call object%loop_archive%activate (var_str ('Generated_Loops'))
  end subroutine gosam_driver_init

  subroutine gosam_writer_generate_configuration_file &
          (object, unit, os_data)
      class(gosam_writer_t), intent(in) :: object
      integer, intent(in) :: unit
      type(os_data_t), intent(in) :: os_data
      type(string_t) :: fc_bin
      type(string_t) :: form_bin, qgraf_bin, haggies_bin
      type(string_t) :: fcflags_golem, ldflags_golem
      type(string_t) :: fcflags_samurai, ldflags_samurai
      type(string_t) :: fcflags_ninja, ldflags_ninja
      type(string_t) :: ldflags_avh_olo, ldflags_qcdloop
      fc_bin = DEFAULT_FC
      form_bin = object%form_dir // '/bin/tform'
      qgraf_bin = object%qgraf_dir // '/bin/qgraf'
      if (object%gosam_dir /= "") then
        haggies_bin = '/usr/bin/java -jar ' // object%gosam_dir // &
                       '/share/golem/haggies/haggies.jar'
      else
        call msg_fatal ("generate_configuration_file: At least " // &
             "the GoSam Directory has to be specified!")
      end if
      if (object%golem_dir /= "") then
        fcflags_golem = "-I" // object%golem_dir // "/include/golem95"
        ldflags_golem = "-L" // object%golem_dir // "/lib -lgolem"
      end if
      if (object%samurai_dir /= "") then
        fcflags_samurai = "-I" // object%samurai_dir // "/include/samurai"
        ldflags_samurai = "-L" // object%samurai_dir // "/lib -lsamurai"
        ldflags_avh_olo = "-L" // object%samurai_dir // "/lib -lavh_olo"
        ldflags_qcdloop = "-L" // object%samurai_dir // "/lib -lqcdloop"
      end if
      if (object%ninja_dir /= "") then
        fcflags_ninja = "-I" // object%ninja_dir // "/include/ninja " &
                        // "-I" // object%ninja_dir // "/include"
        ldflags_ninja = "-L" // object%ninja_dir // "/lib -lninja"
      end if
      write (unit, "(A)") "+avh_olo.ldflags=" &
            // char (ldflags_avh_olo) 
      write (unit, "(A)") "reduction_programs=golem95, samurai, ninja"
      write (unit, "(A)") "extensions=autotools"
      write (unit, "(A)") "+qcdloop.ldflags=" &
            // char (ldflags_qcdloop)
      write (unit, "(A)") "+zzz.extensions=qcdloop, avh_olo"
      write (unit, "(A)") "fc.bin=" // char (os_data%fc)
      write (unit, "(A)") "form.bin=" // char (form_bin)
      write (unit, "(A)") "qgraf.bin=" // char (qgraf_bin)
      write (unit, "(A)") "golem95.fcflags=" // char (fcflags_golem)
      write (unit, "(A)") "golem95.ldflags=" // char (ldflags_golem)
      write (unit, "(A)") "haggies.bin=" // char (haggies_bin)
      write (unit, "(A)") "samurai.fcflags=" // char (fcflags_samurai)
      write (unit, "(A)") "samurai.ldflags=" // char (ldflags_samurai)
      write (unit, "(A)") "ninja.fcflags=" // char (fcflags_ninja)
      write (unit, "(A)") "ninja.ldflags=" // char (ldflags_ninja)
      write (unit, "(A)") "zero=mU,mD,mC,mS,mB"
      write (unit, "(A)") "PSP_check=False"
      write (unit, "(A)") "filter.lo=lambda d: d.iprop(H) == 0 and d.iprop(chi) == 0"
      write (unit, "(A)") "filter.nlo=lambda d: d.iprop(H) == 0 and d.iprop(chi) == 0"
  end subroutine gosam_writer_generate_configuration_file

  subroutine gosam_writer_generate_olp_file (object, unit, os_data)
    class(gosam_writer_t), intent(in) :: object
    integer, intent(in) :: unit
    type(os_data_t), intent(in) :: os_data
    call blha_configuration_write (object%blha_cfg, unit)
  end subroutine gosam_writer_generate_olp_file

  function gosam_writer_get_process_string (writer) result (s_proc)
    class(gosam_writer_t), intent(in) :: writer
    type(string_t) :: s_proc
    character (3) :: c 
    integer :: i
    ! write (c, '(I3)') writer%pdg_in (1)
    ! s_proc = var_str (c) // " "
    ! write (c, '(I3)') writer%pdg_in (2)
    ! s_proc = s_proc // var_str (c) // " -> "
    ! do i = 1, size (writer%pdg_out)
    !   write (c, '(I3)') writer%pdg_out (i) 
    !   s_proc = s_proc // var_str (c) // " "
    ! end do
  end function gosam_writer_get_process_string

  function gosam_writer_get_n_proc (writer) result (n_proc)
    class(gosam_writer_t), intent(in) :: writer
    integer :: n_proc
    n_proc = blha_configuration_get_n_proc (writer%blha_cfg)
  end function gosam_writer_get_n_proc

  subroutine gosam_driver_execute_olp_file (object, os_data)
    class(gosam_driver_t), intent(in) :: object
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: command
    integer :: unit
    command = object%gosam_dir // "/bin" // &
              '/gosam.py --olp ' // object%olp_file // &
              ' --destination=' // object%olp_dir // &
              ' -f -z'
    call os_system_call (command, verbose = .true.)
    call msg_message ("Configure olp_modules")
    command = "cd " // object%olp_dir // "; ./autogen.sh --prefix=$(pwd)"
    call os_system_call (command, verbose = .true.)
    call msg_message ("Installing olp_modules")
    command = "cd " // object%olp_dir // ";  make install"
    call os_system_call (command, verbose = .true.)
  end subroutine gosam_driver_execute_olp_file

  subroutine gosam_driver_load (object, os_data, store, success)
    class(gosam_driver_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: store
    logical, intent(out) :: success
    type(dlaccess_t) :: dlaccess
    type(string_t) ::  path
    type(c_funptr) :: c_fptr

    path = object%olp_dir // '/.libs'
    call dlaccess_init &
          (dlaccess, var_str ("."), path // '/libgolem_olp.so', os_data)
    if (os_file_exist (path // '/libgolem_olp.so')) then
      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Start"))
      call c_f_procpointer (c_fptr, object%gosam_olp_start)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_EvalSubProcess"))
      call c_f_procpointer (c_fptr, object%gosam_olp_eval)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Info"))
      call c_f_procpointer (c_fptr, object%gosam_olp_info)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_SetParameter"))
      call c_f_procpointer (c_fptr, object%gosam_olp_set_parameter)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_EvalSubProcess2"))
      call c_f_procpointer (c_fptr, object%gosam_olp_eval2)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Option"))
      call c_f_procpointer (c_fptr, object%gosam_olp_option)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Polvec"))
      call c_f_procpointer (c_fptr, object%gosam_olp_polvec)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Finalize"))
      call c_f_procpointer (c_fptr, object%gosam_olp_finalize)

      c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_PrintParameter"))
      call c_f_procpointer (c_fptr, object%gosam_olp_print_parameter)

      success = .not. dlaccess_has_error (dlaccess)
      if (store .and. success) call object%loop_archive%record (object%olp_file, &
                                  var_str ('golem.in'), path // '/libgolem_olp.so')
    else
      success = .false.
    end if
  end subroutine gosam_driver_load

  subroutine prc_gosam_write (object, unit)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_message ("GOSAM")
  end subroutine prc_gosam_write

  function prc_gosam_needs_mcset (object) result (flag)
    class(prc_gosam_t), intent(in) :: object
    logical :: flag
    !!! Is this really the same for Gosam as for Omega?
    flag = .true.
  end function prc_gosam_needs_mcset

  function prc_gosam_get_n_terms (object) result (n)
    class(prc_gosam_t), intent(in) :: object
    integer :: n
    n = 1
  end function prc_gosam_get_n_terms

  function prc_gosam_is_allowed (object, i_term, f, h, c) result (flag)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in) :: i_term, f, h, c
    logical :: flag
    flag = .true.
  end function prc_gosam_is_allowed

  subroutine prc_gosam_compute_hard_kinematics &
       (object, p_seed, i_term, int_hard, tmp)
    class(prc_gosam_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(in) :: p_seed
    integer, intent(in) :: i_term
    type(interaction_t), intent(inout) :: int_hard
    class(workspace_t), intent(inout), allocatable :: tmp 
    call interaction_set_momenta (int_hard, p_seed)
    if (allocated (tmp)) then
      select type (tmp)
      type is (gosam_state_t); tmp%new_kinematics = .true.
      end select
    end if
  end subroutine prc_gosam_compute_hard_kinematics

  subroutine prc_gosam_compute_eff_kinematics &
       (object, i_term, int_hard, int_eff, tmp)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in) :: i_term
    type(interaction_t), intent(in) :: int_hard
    type(interaction_t), intent(inout) :: int_eff
    class(workspace_t), intent(inout), allocatable :: tmp
  end subroutine prc_gosam_compute_eff_kinematics

  function prc_gosam_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, tmp) result (amp)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    class(workspace_t), intent(inout), allocatable, optional :: tmp
    complex(default) :: amp
    select type (tmp)
    type is (gosam_state_t)
      tmp%alpha_qcd = object%qcd%alpha%get (fac_scale)
    end select
    amp = 0.0
  end function prc_gosam_compute_amplitude

  subroutine prc_gosam_recover_kinematics &
       (object, p_seed, int_hard, int_eff, tmp)
    class(prc_gosam_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(inout) :: p_seed
    type(interaction_t), intent(inout) :: int_hard, int_eff
    class(workspace_t), intent(inout), allocatable :: tmp
    integer :: n_in
    n_in = interaction_get_n_in (int_eff)
    call interaction_set_momenta (int_eff, p_seed(1:n_in), outgoing = .false.)
    p_seed(n_in+1:) = interaction_get_momenta (int_eff, outgoing = .true.)
  end subroutine prc_gosam_recover_kinematics

  subroutine prc_gosam_init_writer (object, gosam_template, os_data, model)
    class(prc_gosam_t), intent(inout) :: object
    type(gosam_writer_template_t), intent(inout) :: gosam_template
    type(os_data_t), intent(in) :: os_data
    class(model_data_t), intent(in), pointer :: model
    integer,  dimension(:), allocatable :: amp_type
    integer, dimension(:,:), allocatable :: flv_states
    integer :: i, n_proc
    
    n_proc = 0
    if (gosam_template%compute_loops) n_proc = n_proc+1
    if (gosam_template%compute_correlations) n_proc = n_proc+3

    associate (data => gosam_template%data)
      allocate (amp_type (n_proc))
      allocate (flv_states (data%n_in + data%n_out, n_proc))
      do i = 1, n_proc
         flv_states (:,i:i) = data%get_flv_state ()
      end do
      i = 1
      if (gosam_template%compute_loops) then
         amp_type (i) = BLHA_AMP_LOOP
         object%i_virt = i-1
         i = i+1
      end if
      if (gosam_template%compute_correlations) then
         amp_type (i) = BLHA_AMP_TREE
         object%i_born = i-1
         i = i+1
         amp_type (i) = BLHA_AMP_CC
         object%i_cc = i-1
         i = i+1
         amp_type (i) = BLHA_AMP_SC
         object%i_sc = i-1
      end if  
     
      select type (def => object%def) 
        type is (gosam_def_t)
        select type (writer => def%writer)
        type is (gosam_writer_t)
          call writer%init (def%basename, flv_states, amp_type, &
                            gosam_template%alpha_power, &
                            gosam_template%alphas_power, &
                            os_data, model, def%execute_olp)
        end select
      end select
    end associate
  end subroutine prc_gosam_init_writer

  subroutine prc_gosam_init_driver (object, os_data)
    class(prc_gosam_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical :: dl_success
    type(string_t) :: olp_file, olc_file, olp_dir
    integer(c_int) :: success
    logical :: found = .false.

    select type (def => object%def)
    type is (gosam_def_t)
      olp_file = def%basename // '.olp'
      olc_file = def%basename // '.olc'
      olp_dir = def%basename // '_olp_modules'
    class default
      call msg_bug ("prc_gosam_init_driver: core_def should be of gosam-type")
    end select

    call object%def%allocate_driver (object%driver, var_str ("Test_Gosam"))
    associate (driver=>object%driver)
    select type(driver)
    type is (gosam_driver_t)

      call driver%init (os_data, olp_file, olc_file, olp_dir)
      call driver%loop_archive%search ([olp_file, var_str ('golem.in'), olp_dir // &
                                        '/.libs/libgolem_olp_.so'], found)
      if (found) then
         call driver%loop_archive%restore (olp_file, olp_dir)
      else
        select type (def => object%def)
        type is (gosam_def_t)
           call driver%execute_olp_file (os_data)
        end select  
      end if
      call driver%load (os_data, .not.found, dl_success)
      if (.not. dl_success) &
        call msg_fatal ("Error in linking gosam libraries")
      call driver%gosam_olp_start (char (driver%olc_file), success)
    end select
    end associate
  end subroutine prc_gosam_init_driver

  subroutine prc_gosam_set_initialized (prc_gosam)
    class(prc_gosam_t), intent(inout) :: prc_gosam
    prc_gosam%initialized = .true.
  end subroutine prc_gosam_set_initialized 

  subroutine prc_gosam_set_parameters (object, qcd)
    class(prc_gosam_t), intent(inout) :: object
    type(qcd_t), intent(in) :: qcd
    object%qcd = qcd
  end subroutine prc_gosam_set_parameters

  function create_blha_momentum_array (p) result (mom)
    type(vector4_t), intent(in), dimension(:) :: p
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    integer :: n, i, k

    n = size (p)
    if (n > 10) call msg_fatal ("Number of external particles exceeeds" &
                                 // "size of GoSam-internal momentum array")
    k = 1
    do i = 1, n
       mom(k:k+3) = vector4_get_components (p(i))
       mom(k+4) = invariant_mass (p(i))
       k = k+5
    end do
    mom (k:50) = 0.0
  end function create_blha_momentum_array

  subroutine prc_gosam_compute_sqme_virt &
       (object, p, ren_scale, alpha_s, sqme, bad_point)
    class(prc_gosam_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(in) :: alpha_s
    logical, intent(out) :: bad_point
    real(default), dimension(4), intent(out) :: sqme
    real(double) :: mu
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    integer :: i, comp
    real(double) :: acc_dble
    real(default) :: acc
    integer :: ierr

    mom = create_blha_momentum_array (p)
    if (ren_scale == 0.0) then
      mu = sqrt (2* (p(1)*p(2)))
    else
      mu = ren_scale
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
      call driver%gosam_olp_set_parameter &
           (c_char_'alphaS'//c_null_char, &
            dble (twopi), 0._double, ierr)
      call driver%gosam_olp_eval2 (object%i_virt, mom, mu, r, acc_dble) 
    end select
    acc = acc_dble
    sqme = r(1:4)
    if (acc > object%maximum_accuracy) then
       bad_point = .true.
    else
       bad_point = .false.
    end if
  end subroutine prc_gosam_compute_sqme_virt

  subroutine prc_gosam_compute_sqme_cc &
         (object, p, ren_scale, born_out, born_cc, bad_point)
    class(prc_gosam_t), intent(in) :: object
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(out), optional :: born_out
    real(default), intent(inout), dimension(:,:) :: born_cc
    logical, intent(out) :: bad_point
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    real(double) :: mu
    integer :: i, j, pos
    integer :: im1, jm1
    real(double) :: acc_dble1, acc_dble2
    real(default) :: acc1, acc2
    real(default) :: born
    integer :: ierr

    mom = create_blha_momentum_array (p)
    if (ren_scale == 0.0) then
       mu = sqrt (2*p(1)*p(2))
    else
       mu = ren_scale
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%gosam_olp_eval2 (object%i_born, mom, mu, r, acc_dble1)
       born = r(4)
       if (present (born_out)) born_out = born
       call driver%gosam_olp_eval2 (object%i_cc, mom, mu, r, acc_dble2)
    end select
    do j = 1, size (p)
      do i = 1, j
        if (i <= 2 .or. j <= 2) then
          born_cc (i,j) = 0._default
        else if (i == j) then
          born_cc (i,j) = -cf*born
        else
          im1 = i-1; jm1 = j-1
          pos = im1 + jm1*(jm1-1)/2 + 1
          born_cc (i,j) = -r(pos)
        end if
        born_cc (j,i) = born_cc (i,j)
      end do
    end do
    acc1 = acc_dble1; acc2 = acc_dble2
    if (acc1 > object%maximum_accuracy .or. &
        acc2 > object%maximum_accuracy) then
      bad_point = .true.
    end if
  end subroutine prc_gosam_compute_sqme_cc

  subroutine prc_gosam_compute_sqme_sc &
           (object, em, p, ren_scale_in, born_sc, bad_point)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in) :: em
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale_in
    real(default), intent(inout), dimension(:,:) :: born_sc
    logical, intent(out) :: bad_point
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    real(double) :: ren_scale
    real(double), dimension(0:3) :: p_gluon, q_ref
    real(double), dimension(0:7) :: eps
    integer :: i, igm1, n
    integer :: pos
    integer :: mu, nu
    real(default) :: sum_born
    real(double) :: acc_dble
    real(default) :: acc
    integer :: ierr

    sum_born = 0.0
    mom = create_blha_momentum_array (p)
    q_ref = 0.5*[1,1,1,1]
    p_gluon = vector4_get_components (p(em))
    if (ren_scale_in == 0.0) then
      ren_scale = sqrt (2*p(1)*p(2))
    else
      ren_scale = ren_scale_in
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%gosam_olp_eval2 (object%i_sc, mom, ren_scale, r, acc_dble)
       call driver%gosam_olp_polvec (p_gluon, q_ref, eps)
    end select

    igm1 = em-1
    n = size(p)
    do i = 0, n-1
      pos = 2*igm1 + 2*n*i + 1
      sum_born = sum_born + r(pos)
    end do

    do mu = 0, 3
      do nu = 0, 3
        born_sc (mu,nu) = sum_born * eps(2*mu) * eps(2*nu)
      end do
    end do
  
    acc = acc_dble
    if (acc > object%maximum_accuracy) bad_point = .true.
  end subroutine prc_gosam_compute_sqme_sc

  subroutine prc_gosam_fill_constants (object, data_born)
    class(prc_gosam_t), intent(inout) :: object
    type(process_constants_t), intent(in) :: data_born
    associate (data => object%data)
      data%id = 'eeuu'
      print *, 'Model name: ', char (data_born%model_name)
      data%model_name = data_born%model_name
      data%md5sum = data_born%md5sum
      data%openmp_supported = .false.
      data%n_in = data_born%n_in
      data%n_out = data_born%n_out
      data%n_flv = data_born%n_flv
      data%n_hel = data_born%n_hel
      data%n_col = data_born%n_col
      data%n_cin = data_born%n_cin
      data%n_cf = data_born%n_cf
      data%flv_state = data_born%flv_state
      data%hel_state = data_born%hel_state
      data%col_state = data_born%col_state
      data%color_factors = data_born%color_factors
      data%cf_index = data_born%cf_index
      data%ghost_flag = data_born%ghost_flag
    end associate
  end subroutine prc_gosam_fill_constants

  subroutine prc_gosam_allocate_workspace (object, tmp)
    class(prc_gosam_t), intent(in) :: object
    class(workspace_t), intent(inout), allocatable :: tmp
    allocate (gosam_state_t :: tmp)
  end subroutine prc_gosam_allocate_workspace

  function prc_gosam_get_alpha_s (object, tmp) result (alpha)
    class(prc_gosam_t), intent(in) :: object
    class(workspace_t), intent(in), allocatable :: tmp
    real(default) :: alpha
    if (allocated (tmp)) then
      select type (tmp)
      type is (gosam_state_t)
        alpha = tmp%alpha_qcd
      end select
    end if
  end function prc_gosam_get_alpha_s

  subroutine prc_gosam_set_n_proc (object)
    class(prc_gosam_t), intent(inout) :: object
    select type (writer => object%def%writer)
    type is (gosam_writer_t)
      object%n_proc = writer%get_n_proc ()
    end select
  end subroutine prc_gosam_set_n_proc

  function prc_gosam_get_n_proc (object) result (n_proc)
    class(prc_gosam_t), intent(in) :: object
    integer :: n_proc
    n_proc = object%n_proc
  end function prc_gosam_get_n_proc

  subroutine gosam_state_write (object, unit)
    class(gosam_state_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_bug ("gosam_state_write: What to write?")
  end subroutine gosam_state_write

  subroutine gosam_state_reset_new_kinematics (object)
    class(gosam_state_t), intent(inout) :: object
    object%new_kinematics = .true.
  end subroutine gosam_state_reset_new_kinematics

  function check_golem_installation (os_data) result (res)
     type(os_data_t), intent(in) :: os_data
     logical :: res
     type(string_t) :: libavh_olo, libgolem
     integer :: i
     res = .true.
     do i = 1, size (lib_suffix)
       libavh_olo = os_data%prefix // '/lib/libavh_olo' // &
            trim (lib_suffix (i))
       libgolem = os_data%prefix // '/lib/libgolem' // trim (lib_suffix (i))
       res = res .and. (os_file_exist (libavh_olo) .and. &
            os_file_exist (libgolem))
       if (.not. res) exit
     end do
   end function check_golem_installation

  function check_samurai_installation (os_data) result (res)
    type(os_data_t), intent(in) :: os_data
    logical :: res
    type(string_t) :: libsamurai
    integer :: i
    res = .true.
    do i = 1, size (lib_suffix)
      libsamurai = os_data%prefix // '/lib/libsamurai' // trim (lib_suffix(i))
      res = os_file_exist (libsamurai)
      if (.not. res) exit
    end do
  end function check_samurai_installation 

  function check_ninja_installation (os_data) result (res)
    type(os_data_t), intent(in) :: os_data
    logical :: res
    type(string_t) :: libninja
    integer :: i
    res = .true.
    do i = 1, size (lib_suffix)
      libninja = os_data%prefix // '/lib/libninja' // trim (lib_suffix(i))
      res = os_file_exist (libninja)
      if (.not. res) exit
    end do
  end function check_ninja_installation


end module prc_gosam

