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

module prc_gosam
  
  use, intrinsic :: iso_c_binding !NODEP!
  use, intrinsic :: iso_fortran_env

  use kinds
  use iso_varying_string, string_t => varying_string  
  use io_units
  use constants
  use system_defs, only: TAB
  use system_dependencies
  use file_utils
  use string_utils
  use physics_defs
  use diagnostics
  use os_interface
  use lorentz
  use interactions
  use pdg_arrays
  use sm_qcd
  use flavors
  use model_data
  
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

  integer, parameter :: Q_TO_QG = 1
  integer, parameter :: G_TO_GG = 2
  integer, parameter :: G_TO_QQ = 3


  public :: gosam_writer_t
  public :: gosam_def_t
  public :: prc_gosam_t
  public :: gosam_template_t

  type, extends (prc_writer_f_module_t) :: gosam_writer_t
    type(string_t) :: gosam_dir
    type(string_t) :: golem_dir
    type(string_t) :: samurai_dir
    type(string_t) :: ninja_dir
    type(string_t) :: form_dir
    type(string_t) :: qgraf_dir
    type(blha_configuration_t) :: blha_cfg
    type(string_t) :: model_name
    type(string_t) :: process_mode
    type(string_t) :: process_string
  contains
    procedure :: write_wrapper => gosam_write_wrapper
    procedure :: write_interface => gosam_write_interface
    procedure :: write_source_code => gosam_write_source_code
    procedure :: write_makefile_code => gosam_write_makefile_code
    procedure, nopass:: get_procname => gosam_writer_get_procname
    procedure, nopass :: get_module_name => gosam_writer_get_module_name
    procedure, nopass :: type_name => gosam_writer_type_name
    procedure :: write => gosam_writer_write
    procedure :: init => gosam_writer_init
    procedure :: generate_configuration_file => &
              gosam_writer_generate_configuration_file
    procedure :: get_process_string => gosam_writer_get_process_string
    procedure :: get_n_proc => gosam_writer_get_n_proc
  end type gosam_writer_t

  type, extends (prc_core_def_t) :: gosam_def_t
    type(string_t) :: basename
    type(string_t) :: suffix
    logical :: execute_olp = .true.
  contains
    procedure :: init => gosam_def_init
    procedure, nopass :: needs_code => gosam_def_needs_code
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
    procedure(omega_update_alpha_s), nopass, pointer :: &
              update_alpha_s => null ()
    procedure(omega_is_allowed), nopass, pointer :: &
              is_allowed => null ()

  contains
    procedure, nopass :: type_name => gosam_driver_type_name
    procedure :: init_gosam => gosam_driver_init_gosam
    procedure :: execute_olp_file => gosam_driver_execute_olp_file
    procedure :: set_alpha_s => gosam_driver_set_alpha_s
    procedure :: set_mass_and_width => gosam_driver_set_mass_and_width
    procedure :: load => gosam_driver_load
    procedure :: read_olc_file => gosam_driver_read_olc_file
  end type gosam_driver_t

  type, extends (prc_core_t) :: prc_gosam_t
    type(qcd_t) :: qcd
    integer :: n_flv
    real(default) :: maximum_accuracy = 10000.0
    logical :: initialized = .false.
    integer, dimension(:), allocatable :: i_born, i_sc, i_cc
    integer, dimension(:), allocatable :: i_real
    integer, dimension(:), allocatable :: i_virt
  contains
    procedure :: execute_olp_file => prc_gosam_execute_olp_file
    procedure :: write => prc_gosam_write
    procedure :: needs_mcset => prc_gosam_needs_mcset
    procedure :: get_n_terms => prc_gosam_get_n_terms
    procedure :: is_allowed => prc_gosam_is_allowed
    procedure :: update_alpha_s => prc_gosam_update_alpha_s
    procedure :: compute_hard_kinematics => prc_gosam_compute_hard_kinematics
    procedure :: compute_eff_kinematics => prc_gosam_compute_eff_kinematics
    procedure :: compute_amplitude => prc_gosam_compute_amplitude
    procedure :: recover_kinematics => prc_gosam_recover_kinematics
     procedure :: init_gosam => prc_gosam_init_gosam
    procedure :: get_nflv => prc_gosam_get_nflv
    procedure :: init_driver => prc_gosam_init_driver
    procedure :: set_initialized => prc_gosam_set_initialized
    procedure :: set_parameters => prc_gosam_set_parameters
    procedure :: compute_sqme_virt => prc_gosam_compute_sqme_virt
    procedure :: compute_sqme_real => prc_gosam_compute_sqme_real
    procedure :: compute_sqme_cc => prc_gosam_compute_sqme_cc
    procedure :: compute_sqme_sc => prc_gosam_compute_sqme_sc
    procedure :: allocate_workspace => prc_gosam_allocate_workspace
    procedure :: get_alpha_s => prc_gosam_get_alpha_s
    procedure :: read_olc_file => prc_gosam_read_olc_file
    procedure :: set_particle_properties => prc_gosam_set_particle_properties
  end type prc_gosam_t

  type, extends (workspace_t) :: gosam_state_t
    logical :: new_kinematics = .true.
    real(default) :: alpha_qcd = -1
  contains
    procedure :: write => gosam_state_write
    procedure :: reset_new_kinematics => gosam_state_reset_new_kinematics
  end type gosam_state_t

  type :: gosam_template_t
    integer :: I_REAL = 1
    integer :: I_LOOP = 2
    integer :: I_SUB = 3
    logical, dimension(3) :: compute_component
  contains
    procedure :: init => gosam_template_init
    procedure :: set_loop => gosam_template_set_loop
    procedure :: set_subtraction => gosam_template_set_subtraction
    procedure :: set_real_trees => gosam_template_set_real_trees
    procedure :: compute_loop => gosam_template_compute_loop
    procedure :: compute_subtraction => gosam_template_compute_subtraction
    procedure :: compute_real_trees => gosam_template_compute_real_trees
    procedure :: check => gosam_template_check
    procedure :: reset => gosam_template_reset
  end type gosam_template_t


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

  abstract interface
     subroutine omega_update_alpha_s (alpha_s) bind(C)
       import
       real(c_default_float), intent(in) :: alpha_s
     end subroutine omega_update_alpha_s
  end interface
  
  abstract interface
     subroutine omega_is_allowed (flv, hel, col, flag) bind(C)
       import
       integer(c_int), intent(in) :: flv, hel, col
       logical(c_bool), intent(out) :: flag
     end subroutine omega_is_allowed
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

  subroutine gosam_def_init (object, basename, model_name, &
                             prt_in, prt_out, nlo_type)
    class(gosam_def_t), intent(inout) :: object
    type(string_t), intent(in) :: model_name
    type(string_t), intent(in) :: basename
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    type(string_t), intent(in) :: nlo_type
    object%basename = basename
    allocate (gosam_writer_t :: object%writer)
    select case (char (nlo_type))
    case ('Real')
       object%suffix = '_REAL'
    case ('Virtual')
       object%suffix = '_LOOP'
    case ('Subtraction')
       object%suffix = '_SUB'
    end select
    select type (writer => object%writer)
    type is (gosam_writer_t)
      call writer%init (model_name, prt_in, prt_out)
    end select
  end subroutine gosam_def_init

  function gosam_def_needs_code () result (flag)
    logical :: flag
    flag = .true.
  end function gosam_def_needs_code

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
    integer :: pid, fid
    class(prc_core_driver_t), intent(inout) :: proc_driver
    type(c_funptr) :: fptr
    select type (proc_driver)
    type is (gosam_driver_t)       
       pid = i
       fid = 2
       call lib_driver%get_fptr (pid, fid, fptr)
       call c_f_procpointer (fptr, proc_driver%update_alpha_s)
       fid = 4
       call lib_driver%get_fptr (pid, fid, fptr)
       call c_f_procpointer (fptr, proc_driver%is_allowed)
    end select
  end subroutine gosam_def_connect

  subroutine gosam_write_wrapper (writer, unit, id, feature)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id, feature    
    type(string_t) :: name
    name = writer%get_c_procname (id, feature)
    write (unit, *)
    select case (char (feature))
    case ("init")
       write (unit, "(9A)")  "subroutine ", char (name), " (par) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       write (unit, "(2x,9A)")  "real(c_default_float), dimension(*), &
            &intent(in) :: par"
       if (c_default_float == default) then
          write (unit, "(2x,9A)")  "call ", char (feature), " (par)"
       end if
       write (unit, "(9A)")  "end subroutine ", char (name)
    case ("update_alpha_s")
       write (unit, "(9A)")  "subroutine ", char (name), " (alpha_s) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       if (c_default_float == default) then
          write (unit, "(2x,9A)")  "real(c_default_float), intent(in) &
               &:: alpha_s"
          write (unit, "(2x,9A)")  "call ", char (feature), " (alpha_s)"
       end if
       write (unit, "(9A)")  "end subroutine ", char (name)
    case ("reset_helicity_selection")
       write (unit, "(9A)")  "subroutine ", char (name), &
            " (threshold, cutoff) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       if (c_default_float == default) then
          write (unit, "(2x,9A)")  "real(c_default_float), intent(in) &
               &:: threshold"
          write (unit, "(2x,9A)")  "integer(c_int), intent(in) :: cutoff"
          write (unit, "(2x,9A)")  "call ", char (feature), &
               " (threshold, int (cutoff))"
       end if
       write (unit, "(9A)")  "end subroutine ", char (name)
    case ("is_allowed")
       write (unit, "(9A)")  "subroutine ", char (name), &
            " (flv, hel, col, flag) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       write (unit, "(2x,9A)")  "integer(c_int), intent(in) :: flv, hel, col"
       write (unit, "(2x,9A)")  "logical(c_bool), intent(out) :: flag"    
       write (unit, "(2x,9A)")  "flag = ", char (feature), &
            " (int (flv), int (hel), int (col))"
       write (unit, "(9A)")  "end subroutine ", char (name)
    case ("new_event")
       write (unit, "(9A)")  "subroutine ", char (name), " (p) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       if (c_default_float == default) then
          write (unit, "(2x,9A)")  "real(c_default_float), dimension(0:3,*), &
               &intent(in) :: p"
          write (unit, "(2x,9A)")  "call ", char (feature), " (p)"
       end if
       write (unit, "(9A)")  "end subroutine ", char (name)
    case ("get_amplitude")
       write (unit, "(9A)")  "subroutine ", char (name), &
            " (flv, hel, col, amp) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       write (unit, "(2x,9A)")  "integer(c_int), intent(in) :: flv, hel, col"
       write (unit, "(2x,9A)")  "complex(c_default_complex), intent(out) &
            &:: amp"    
       write (unit, "(2x,9A)")  "amp = ", char (feature), &
            " (int (flv), int (hel), int (col))"
       write (unit, "(9A)")  "end subroutine ", char (name)
    end select

  end subroutine gosam_write_wrapper

  subroutine gosam_write_interface (writer, unit, id, feature)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(string_t), intent(in) :: feature
    type(string_t) :: name
    name = writer%get_c_procname (id, feature)
    write (unit, "(2x,9A)")  "interface"
    select case (char (feature))
    case ("init")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " (par) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "real(c_default_float), dimension(*), &
            &intent(in) :: par"
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    case ("update_alpha_s")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " (alpha_s) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "real(c_default_float), intent(in) :: alpha_s"
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    case ("reset_helicity_selection")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " &
            &(threshold, cutoff) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "real(c_default_float), intent(in) :: threshold"
       write (unit, "(7x,9A)")  "integer(c_int), intent(in) :: cutoff"
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    case ("is_allowed")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " &
            &(flv, hel, col, flag) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "integer(c_int), intent(in) :: flv, hel, col"
       write (unit, "(7x,9A)")  "logical(c_bool), intent(out) :: flag"    
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    case ("new_event")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " (p) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "real(c_default_float), dimension(0:3,*), &
            &intent(in) :: p"
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    case ("get_amplitude")
       write (unit, "(5x,9A)")  "subroutine ", char (name), " &
            &(flv, hel, col, amp) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "integer(c_int), intent(in) :: flv, hel, col"
       write (unit, "(7x,9A)")  "complex(c_default_complex), intent(out) &
            &:: amp"    
       write (unit, "(5x,9A)")  "end subroutine ", char (name)
    end select
    write (unit, "(2x,9A)")  "end interface"
  end subroutine gosam_write_interface

  subroutine gosam_write_source_code (writer, id)
    class(gosam_writer_t), intent(in) :: writer
    type(string_t), intent(in) :: id
  end subroutine gosam_write_source_code

  subroutine gosam_write_makefile_code (writer, unit, id, os_data, testflag)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: testflag
    type(string_t) :: omega_binary, omega_path
    omega_binary = "omega_" // writer%model_name // ".opt"
    omega_path = os_data%whizard_omega_binpath // "/" // omega_binary
    write (unit, "(5A)")  "OBJECTS += ", char (id), ".lo"
    write (unit, "(5A)")  char (id), ".f90:"
    write (unit, "(99A)")  TAB, char (omega_path), &
         " -o ", char (id), ".f90", &
         " -target:whizard", &
         " -target:parameter_module parameters_", char (writer%model_name), &
         " -target:module opr_", char (id), &
         " -target:md5sum '", writer%md5sum, "'", &
         char (writer%process_mode), char (writer%process_string)
    write (unit, "(5A)")  "clean-", char (id), ":"
    write (unit, "(5A)")  TAB, "rm -f ", char (id), ".f90"
    write (unit, "(5A)")  TAB, "rm -f opr_", char (id), ".mod"
    write (unit, "(5A)")  TAB, "rm -f ", char (id), ".lo"
    write (unit, "(5A)")  "CLEAN_SOURCES += ", char (id), ".f90"    
    write (unit, "(5A)")  "CLEAN_OBJECTS += opr_", char (id), ".mod"       
    write (unit, "(5A)")  "CLEAN_OBJECTS += ", char (id), ".lo"
    write (unit, "(5A)")  char (id), ".lo: ", char (id), ".f90"
    write (unit, "(5A)")  TAB, "$(LTFCOMPILE) $<"

  end subroutine gosam_write_makefile_code

  function gosam_writer_get_procname (feature) result (name)
    type(string_t) :: name
    type(string_t), intent(in) :: feature
    select case (char (feature))
    case ("n_in");   name = "number_particles_in"
    case ("n_out");  name = "number_particles_out"
    case ("n_flv");  name = "number_flavor_states"
    case ("n_hel");  name = "number_spin_states"
    case ("n_col");  name = "number_color_flows"
    case ("n_cin");  name = "number_color_indices"
    case ("n_cf");   name = "number_color_factors"
    case ("flv_state");  name = "flavor_states"
    case ("hel_state");  name = "spin_states"
    case ("col_state");  name = "color_flows"
    case default
       name = feature
    end select
  end function gosam_writer_get_procname

  function gosam_writer_get_module_name (id) result (name)
    type(string_t) :: name
    type(string_t), intent(in) :: id
    name = "opr_" // id
  end function gosam_writer_get_module_name

  function gosam_writer_type_name () result (string)
    type(string_t) :: string
    string = "gosam"
  end function gosam_writer_type_name

  subroutine gosam_writer_write (writer, unit)
    class(gosam_writer_t), intent(in) :: writer
    integer, intent(in) :: unit    
    write (unit, "(1x,A)")  char (writer%get_process_string ())
  end subroutine gosam_writer_write

  subroutine gosam_writer_init (writer,model_name, prt_in, prt_out) 
    class(gosam_writer_t), intent(inout) :: writer
    type(string_t), intent(in) :: model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    integer :: i, unit

    writer%gosam_dir = GOSAM_DIR
    writer%golem_dir = GOLEM_DIR 
    writer%samurai_dir = SAMURAI_DIR
    writer%ninja_dir = NINJA_DIR
    writer%form_dir = FORM_DIR
    writer%qgraf_dir = QGRAF_DIR

    writer%model_name = model_name

    select case (size (prt_in))
      case (1); writer%process_mode = " -decay"
      case (2); writer%process_mode = " -scatter"
    end select
    associate (s => writer%process_string)
      s = " '" 
      do i = 1, size (prt_in)
         if (i > 1) s = s // " "
         s = s // prt_in(i)
      end do
      s = s // " ->"
      do i = 1, size (prt_out)
         s = s // " " // prt_out(i)
      end do
      s = s // "'"
    end associate

    unit = free_unit ()
    open (unit, file = "golem.in", status = "replace", action = "write")
    call writer%generate_configuration_file (unit)
    close(unit)
  end subroutine gosam_writer_init  

  function gosam_driver_type_name () result (string)
    type(string_t) :: string
    string = "gosam"
  end function gosam_driver_type_name

  subroutine gosam_driver_init_gosam (object, os_data, olp_file, &
                                olc_file, olp_dir)
    class(gosam_driver_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: olp_file, olc_file, olp_dir
    object%gosam_dir = GOSAM_DIR
    object%olp_file = olp_file
    object%olc_file = olc_file
    object%olp_dir = olp_dir
    call object%loop_archive%activate (var_str ('Generated_Loops'))
  end subroutine gosam_driver_init_gosam

  subroutine gosam_writer_generate_configuration_file &
          (object, unit)
      class(gosam_writer_t), intent(in) :: object
      integer, intent(in) :: unit
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
      write (unit, "(A)") "fc.bin=" // char (fc_bin)
      write (unit, "(A)") "form.bin=" // char (form_bin)
      write (unit, "(A)") "qgraf.bin=" // char (qgraf_bin)
      write (unit, "(A)") "golem95.fcflags=" // char (fcflags_golem)
      write (unit, "(A)") "golem95.ldflags=" // char (ldflags_golem)
      write (unit, "(A)") "haggies.bin=" // char (haggies_bin)
      write (unit, "(A)") "samurai.fcflags=" // char (fcflags_samurai)
      write (unit, "(A)") "samurai.ldflags=" // char (ldflags_samurai)
      write (unit, "(A)") "ninja.fcflags=" // char (fcflags_ninja)
      write (unit, "(A)") "ninja.ldflags=" // char (ldflags_ninja)
      !!! This might collide with the mass-setup in the order-file
      !!! write (unit, "(A)") "zero=mU,mD,mC,mS,mB"
      write (unit, "(A)") "PSP_check=False"
      write (unit, "(A)") "filter.lo=lambda d: d.iprop(H) == 0 and d.iprop(chi) == 0"
      write (unit, "(A)") "filter.nlo=lambda d: d.iprop(H) == 0 and d.iprop(chi) == 0"
  end subroutine gosam_writer_generate_configuration_file

  function gosam_writer_get_process_string (writer) result (s_proc)
    class(gosam_writer_t), intent(in) :: writer
    type(string_t) :: s_proc
  end function gosam_writer_get_process_string

  function gosam_writer_get_n_proc (writer) result (n_proc)
    class(gosam_writer_t), intent(in) :: writer
    integer :: n_proc
    n_proc = blha_configuration_get_n_proc (writer%blha_cfg)
  end function gosam_writer_get_n_proc

  subroutine gosam_driver_execute_olp_file (object)
    class(gosam_driver_t), intent(in) :: object
    type(string_t) :: command
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

  subroutine gosam_driver_set_alpha_s (driver, alpha_s)
     class(gosam_driver_t), intent(inout) :: driver
     real(default), intent(in) :: alpha_s
     integer :: ierr
     call driver%gosam_olp_set_parameter &
              (c_char_'alphaS'//c_null_char, &
               dble (alpha_s), 0._double, ierr)
  end subroutine gosam_driver_set_alpha_s

  subroutine gosam_driver_set_mass_and_width (driver, &
                                       i_pdg, mass, width)
    class(gosam_driver_t), intent(inout) :: driver
    integer, intent(in) :: i_pdg
    real(default), intent(in), optional :: mass
    real(default), intent(in), optional :: width
    type(string_t) :: buf
    character(kind=c_char,len=20) :: c_string
    integer :: ierr
    if (present (mass)) then
       buf = 'mass(' // integer_to_string (abs(i_pdg)) // ')'
       c_string = char(buf)//c_null_char
       call driver%gosam_olp_set_parameter &
                (c_string, dble(mass), 0._double, ierr)
       if (ierr == 0) then
          buf = "GoSam: Attempt to set mass of particle " // &
                integer_to_string (abs(i_pdg)) // "failed"
          call msg_fatal (char(buf))
       end if
    end if
    if (present (width)) then
       buf = 'width(' // integer_to_string (abs(i_pdg)) // ')'
       c_string = char(buf)//c_null_char
       call driver%gosam_olp_set_parameter &
                (c_string, dble(width), 0._double, ierr)
       if (ierr == 0) then
          buf = "GoSam: Attempt to set width of particle " // &
                integer_to_string (abs(i_pdg)) // "failed"
          call msg_fatal (char(buf))
       end if
    end if
  end subroutine gosam_driver_set_mass_and_width

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

  subroutine gosam_driver_read_olc_file (driver, flavors, amp_type, flv_index, label)
    class(gosam_driver_t), intent(inout) :: driver
    integer, intent(in), dimension(:,:) :: flavors
    integer, intent(out), dimension(20) :: amp_type, flv_index, label
    integer :: unit, filestat
    character(len=100):: rd_line 
    character(len=3) :: rd_flavor
    logical :: read_flavor, born_found
    integer, dimension(:,:,:), allocatable :: flv_position
    integer, dimension(:,:), allocatable :: flv_read
    integer :: k, i_flv, i_part, current_flavor
    integer :: pos_label

    flv_position = compute_flavor_positions (flavors)
    amp_type = -1; flv_index = -1; label = -1
    allocate (flv_read (size (flavors, 1), &
                        size (flavors, 2)))
    unit = free_unit ()
    open (unit, file=char(driver%olc_file), status="old") 
    read_flavor=.false.
    k = 1
    do
      read (unit, '(A)', iostat = filestat) rd_line
      if (filestat == iostat_end) then
         exit
      else
         if (rd_line(1:13) == 'AmplitudeType') then
            if (rd_line(15:19) == 'Loop') then
               amp_type(k) = BLHA_AMP_LOOP
            else if (rd_line(15:19) == 'Tree') then
               amp_type(k) = BLHA_AMP_TREE
            else if (rd_line(15:21) == 'ccTree') then
               amp_type(k) = BLHA_AMP_CC
            else if (rd_line(15:21) == 'scTree') then
               amp_type(k) = BLHA_AMP_SC
            else
               call msg_fatal ("AmplitudeType present but &
                               &AmpType not known!")
            end if
            read_flavor = .true.
         else if (read_flavor) then
            do i_flv = 1, size (flavors, 2)
               do i_part = 1, size (flavors, 1)
                  read (rd_line (flv_position (i_part, i_flv, 1): &
                                 flv_position (i_part, i_flv, 2)), '(I3)') &
                                 flv_read (i_part, i_flv)
               end do
            end do
            born_found = .false.
            do i_flv = 1, size (flavors, 2)
               if (all (flv_read (:,i_flv) == flavors (:,i_flv))) then
                  flv_index (k) = i_flv
                  pos_label = maxval (flv_position (:,i_flv,:)) + 6
                  read (rd_line (pos_label:pos_label), '(I2)') label(k)
                  born_found = .true.
                  k = k+1
                  exit
               end if
            end do
            if (.not. born_found) call msg_fatal & 
                     ("No underlying Born found")
         end if   
      end if
    end do
    close(unit)
  contains
    function compute_flavor_positions (flavors) result (pos)
      integer, intent(in), dimension(:,:) :: flavors
      integer, dimension(:,:,:), allocatable :: pos
      integer :: i_flv, i_part
      integer :: i_first, i_last
      allocate (pos (size (flavors, 1), &
                     size (flavors, 2), 2))
      do i_flv = 1, size (flavors, 2)
         i_first = 1; i_last = 0
         do i_part = 1, size (flavors, 1)
            if (i_last > 0 .and. i_part /= 3) then
               i_first = i_last+2
            else if (i_last > 0 .and. i_part == 3) then
               i_first = i_first + 6
            end if
            i_last = i_first + flavor_digits (flavors (i_part, i_flv)) - 1
            pos (i_part, i_flv, 1) = i_first
            pos (i_part, i_flv, 2) = i_last
         end do
      end do
    end function compute_flavor_positions             
            
    function flavor_digits (flavor) result (ndigits)
      integer, intent(in) :: flavor
      integer :: ndigits
      if (abs(flavor) >= 10) then
         ndigits = 2
      else
         ndigits = 1
      end if
      if (flavor < 0) ndigits = ndigits+1
    end function flavor_digits
  end subroutine gosam_driver_read_olc_file

  subroutine prc_gosam_execute_olp_file (object, os_data)
    class(prc_gosam_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical :: found, dl_success
    integer :: ierr
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%loop_archive%search ([driver%olp_file, &
                        var_str ('golem.in'), driver%olp_dir // &
                        '/.libs/libgolem_olp.so'], found)
       if (found) then
          call driver%loop_archive%restore (driver%olp_file, driver%olp_dir)
       else
         call driver%execute_olp_file ()
       end if
       call driver%load (os_data, .not.found, dl_success)
       if (.not. dl_success) &
          call msg_fatal ('Error linking GoSam libraries')
       call driver%gosam_olp_start (char (driver%olc_file), ierr)
    end select
  end subroutine prc_gosam_execute_olp_file
       
  subroutine prc_gosam_write (object, unit)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_message ("GOSAM")
  end subroutine prc_gosam_write

  function prc_gosam_needs_mcset (object) result (flag)
    class(prc_gosam_t), intent(in) :: object
    logical :: flag
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
    logical(c_bool) :: cflag
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%is_allowed (f, h, c, cflag)
       flag = cflag
    class default
       call msg_fatal ("Gosam instance created, but driver is not a GoSam driver!")
    end select
  end function prc_gosam_is_allowed

  subroutine prc_gosam_update_alpha_s (object, tmp, fac_scale) 
    class(prc_gosam_t), intent(in) :: object
    class(workspace_t), intent(inout), allocatable :: tmp
    real(default), intent(in) :: fac_scale
    real(default) :: alpha_qcd
    if (allocated (object%qcd%alpha)) then
       alpha_qcd = object%qcd%alpha%get (fac_scale)
       select type (driver => object%driver)
       type is (gosam_driver_t)
          call driver%update_alpha_s (alpha_qcd)
       end select 
       if (allocated (tmp)) then
          select type (tmp)
          type is (gosam_state_t)
             tmp%alpha_qcd = alpha_qcd
          end select
       end if
    end if
  end subroutine prc_gosam_update_alpha_s

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
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, tmp) &
       result (amp)
    class(prc_gosam_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
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

  subroutine prc_gosam_init_gosam (object, gosam_template)
    !!! Not prc_gosam_init to avoid name-clash
    class(prc_gosam_t), intent(inout) :: object
    type(gosam_template_t), intent(inout) :: gosam_template
    integer :: i_flv

    object%n_flv = size (object%data%flv_state, 2)
   
    if (gosam_template%compute_loop ()) then
       allocate (object%i_virt (object%n_flv), &
                 object%i_cc (object%n_flv))
    else if (gosam_template%compute_subtraction ()) then
       allocate (object%i_born (object%n_flv), &
                 object%i_cc (object%n_flv) , &
                 object%i_sc (object%n_flv))
    else if (gosam_template%compute_real_trees ()) then
       allocate (object%i_real (object%n_flv))
    end if
  end subroutine prc_gosam_init_gosam

  function prc_gosam_get_nflv (object) result (n_flv)
    class(prc_gosam_t), intent(in) :: object
    integer :: n_flv
    n_flv = object%n_flv
  end function prc_gosam_get_nflv

  subroutine prc_gosam_init_driver (object, os_data)
    class(prc_gosam_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical :: dl_success
    type(string_t) :: olp_file, olc_file, olp_dir
    integer(c_int) :: success
    logical :: found = .false.

    select type (def => object%def)
    type is (gosam_def_t)
      olp_file = def%basename // def%suffix // '.olp'
      olc_file = def%basename // def%suffix // '.olc'
      olp_dir = def%basename // def%suffix // '_olp_modules'
    class default
      call msg_bug ("prc_gosam_init_driver: core_def should be of gosam-type")
    end select

    select type(driver => object%driver)
    type is (gosam_driver_t)
      call driver%init_gosam (os_data, olp_file, olc_file, olp_dir)
    end select
  end subroutine prc_gosam_init_driver

  subroutine prc_gosam_set_initialized (prc_gosam)
    class(prc_gosam_t), intent(inout) :: prc_gosam
    prc_gosam%initialized = .true.
  end subroutine prc_gosam_set_initialized 

  subroutine prc_gosam_set_parameters (object, qcd, use_color_factors)
    class(prc_gosam_t), intent(inout) :: object
    type(qcd_t), intent(in) :: qcd
    logical, intent(in) :: use_color_factors
    object%qcd = qcd
    object%use_color_factors = use_color_factors

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

  subroutine prc_gosam_compute_sqme_virt (object, &
                i_flv, p, ren_scale, alpha_s, sqme, bad_point)
    class(prc_gosam_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(in) :: alpha_s
    logical, intent(out) :: bad_point
    real(default), dimension(4), intent(out) :: sqme
    real(double) :: mu
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    real(double) :: acc_dble
    real(default) :: acc

    mom = create_blha_momentum_array (p)
    if (ren_scale == 0.0) then
      mu = sqrt (2* (p(1)*p(2)))
    else
      mu = ren_scale
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
      call driver%set_alpha_s (alpha_s)
      call driver%gosam_olp_eval2 (object%i_virt(i_flv), &
                                   mom, mu, r, acc_dble) 
    end select
    acc = acc_dble
    sqme = r(1:4)
    if (acc > object%maximum_accuracy) then
       bad_point = .true.
    else
       bad_point = .false.
    end if
  end subroutine prc_gosam_compute_sqme_virt

  subroutine prc_gosam_compute_sqme_real &
         (object, i_flv, p, ren_scale, alpha_s, sqme, bad_point)
    class(prc_gosam_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme
    logical, intent(out) :: bad_point
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    real(double) :: mu
    real(double) :: acc_dble
    real(default) :: acc
 
    mom = create_blha_momentum_array (p)
    if (ren_scale == 0.0) then
       mu = sqrt (2*p(1)*p(2))
    else
      mu = ren_scale
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%set_alpha_s (alpha_s)
       call driver%gosam_olp_eval2 (object%i_real(i_flv), mom, &
                                    mu, r, acc_dble)
       sqme = r(4)
    end select
    acc = acc_dble
    if (acc > object%maximum_accuracy) bad_point = .true.
  end subroutine prc_gosam_compute_sqme_real

  subroutine prc_gosam_compute_sqme_cc &
         (object, i_flv, p, ren_scale, alpha_s, &
          born_out, born_cc, bad_point)
    class(prc_gosam_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(in) :: alpha_s
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

    mom = create_blha_momentum_array (p)
    if (ren_scale == 0.0) then
       mu = sqrt (2*p(1)*p(2))
    else
       mu = ren_scale
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%set_alpha_s (alpha_s)
       if (allocated (object%i_born)) then
          call driver%gosam_olp_eval2 (object%i_born(i_flv), &
                                       mom, mu, r, acc_dble1)
          born = r(4)
       end if
       if (present (born_out)) born_out = born
       call driver%gosam_olp_eval2 (object%i_cc(i_flv), &
                                    mom, mu, r, acc_dble2)
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

  subroutine prc_gosam_compute_sqme_sc (object, &
                i_flv, em, p, ren_scale_in, alpha_s, &
            me_sc, bad_point)
    class(prc_gosam_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    integer, intent(in) :: em
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale_in
    real(default), intent(in) :: alpha_s
    complex(default), intent(out) :: me_sc
    logical, intent(out) :: bad_point
    real(double), dimension(GOSAM_MOMENTUM_LIMIT) :: mom
    real(double), dimension(GOSAM_RESULTS_LIMIT) :: r
    real(double) :: ren_scale
    integer :: i, igm1, n
    integer :: pos_real, pos_imag
    real(double) :: acc_dble
    real(default) :: acc

    me_sc = cmplx(0,0,default)
    mom = create_blha_momentum_array (p)
    if (ren_scale_in == 0.0) then
      ren_scale = sqrt (2*p(1)*p(2))
    else
      ren_scale = ren_scale_in
    end if
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%set_alpha_s (alpha_s)
       call driver%gosam_olp_eval2 (object%i_sc(i_flv), &
                                    mom, ren_scale, r, acc_dble)
    end select

    igm1 = em-1
    n = size(p)
    do i = 0, n-1
      pos_real = 2*igm1 + 2*n*i + 1
      pos_imag = pos_real + 1
      me_sc = me_sc + cmplx (r(pos_real), r(pos_imag), default)
    end do

    me_sc = -conjg(me_sc)/CA
  
    acc = acc_dble
    if (acc > object%maximum_accuracy) bad_point = .true.
  end subroutine prc_gosam_compute_sqme_sc

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
    else
      alpha = 0._default 
    end if
  end function prc_gosam_get_alpha_s

  subroutine prc_gosam_read_olc_file (object, flavors)
    class(prc_gosam_t), intent(inout) :: object
    integer, intent(in), dimension(:,:) :: flavors
    integer, dimension(20) :: amp_type, flv_index, label
    integer :: i_proc
    select type (driver => object%driver)
    type is (gosam_driver_t)
       call driver%read_olc_file (flavors, amp_type, flv_index, label)
    end select
    do i_proc = 1, size (amp_type)
       if (amp_type (i_proc) < 0) exit
       select case (amp_type (i_proc))
       case (BLHA_AMP_TREE)
          if (allocated (object%i_born)) then
             object%i_born(flv_index(i_proc)) = label(i_proc)
          
          else if (allocated (object%i_real)) then
             object%i_real(flv_index(i_proc)) = label(i_proc)
          else 
             call msg_fatal ("Tree matrix element present, &
                             &but neither Born nor real indices are allocated!")
          end if
       case (BLHA_AMP_CC)
          if (allocated (object%i_cc)) then
             object%i_cc(flv_index(i_proc)) = label(i_proc)
          else
             call msg_fatal ("Color-correlated matrix element present, &
                              &but cc-indices are not allocated!")
          end if
       case (BLHA_AMP_SC)
          if (allocated (object%i_sc)) then
             object%i_sc(flv_index(i_proc)) = label(i_proc)
          else
             call msg_fatal ("Spin-correlated matrix element present, &
                             &but sc-indices are not allocated!")
          end if
       case (BLHA_AMP_LOOP)
          if (allocated (object%i_virt)) then
             object%i_virt(flv_index(i_proc)) = label(i_proc)
          else
             call msg_fatal ("Loop matrix element present, &
                             &but virt-indices are not allocated!")
          end if
       case default
          call msg_fatal ("Undefined amplitude type")
       end select
    end do
  end subroutine prc_gosam_read_olc_file

  subroutine prc_gosam_set_particle_properties (object, model) 
    class(prc_gosam_t), intent(inout) :: object
    class(model_data_t), intent(in), target :: model
    type(flavor_t), dimension(:), allocatable :: flv
    real(default), dimension(:), allocatable :: masses
    real(default), dimension(:), allocatable :: widths
    integer :: i_part, i_flv
    integer :: n_part, n_flv
    n_part = size (object%data%flv_state,1)
    n_flv = size (object%data%flv_state,2)
    allocate (flv (n_part), masses (n_part), widths (n_part))
    do i_flv = 1, n_flv
       associate (i_pdg => object%data%flv_state (:,i_flv))
          call flv%init (i_pdg, model)
          masses = flv%get_mass ()
          widths = flv%get_width ()
          select type (driver => object%driver)
          type is (gosam_driver_t)
             do i_part = 1, n_part
                if (masses(i_part) /= 0._default) &
                   call driver%set_mass_and_width (abs(i_pdg(i_part)), &
                                                   mass=masses(i_part))
                if (abs(i_pdg(i_part)) == 6) &
                   call driver%set_mass_and_width (abs(i_pdg(i_part)), &
                                                   width=widths(i_part))
             end do
          end select
       end associate
    end do
  end subroutine prc_gosam_set_particle_properties

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

  subroutine gosam_template_init (template)
    class(gosam_template_t), intent(inout) :: template
    template%compute_component = .false.
  end subroutine gosam_template_init

  subroutine gosam_template_set_loop (template, val)
    class(gosam_template_t), intent(inout) :: template
    logical, intent(in) :: val
    template%compute_component(template%I_LOOP) = val
  end subroutine gosam_template_set_loop

  subroutine gosam_template_set_subtraction (template, val)
    class(gosam_template_t), intent(inout) :: template
    logical, intent(in) :: val
    template%compute_component (template%I_SUB) = val
  end subroutine gosam_template_set_subtraction

  subroutine gosam_template_set_real_trees (template, val)
    class(gosam_template_t), intent(inout) :: template
    logical, intent(in) :: val
    template%compute_component (template%I_REAL) = val
  end subroutine gosam_template_set_real_trees

  function gosam_template_compute_loop (template) result (val)
    class(gosam_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_LOOP)
  end function gosam_template_compute_loop  

  function gosam_template_compute_subtraction (template) result (val)
    class(gosam_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_SUB)
  end function gosam_template_compute_subtraction

  function gosam_template_compute_real_trees (template) result (val)
    class(gosam_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_REAL)
  end function gosam_template_compute_real_trees

  function gosam_template_check (template) result (val)
    class(gosam_template_t), intent(in) :: template
    logical :: val
    val = count (template%compute_component) == 1
  end function gosam_template_check

  subroutine gosam_template_reset (template)
    class(gosam_template_t), intent(inout) :: template
    template%compute_component = .false.
  end subroutine gosam_template_reset


end module prc_gosam

