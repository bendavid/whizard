! WHIZARD 2.6.2 Dec 13 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module blha_olp_interfaces

  use, intrinsic :: iso_c_binding !NODEP!
  use, intrinsic :: iso_fortran_env

  use kinds
  use iso_varying_string, string_t => varying_string
  use constants
  use numeric_utils, only: vanishes
  use numeric_utils, only: extend_integer_array, crop_integer_array
  use io_units
  use string_utils
  use physics_defs
  use diagnostics
  use os_interface
  use lorentz
  use sm_qcd
  use interactions
  use flavors
  use model_data
  use pdg_arrays, only: is_gluon, is_quark

  use prclib_interfaces
  use process_libraries
  use prc_core_def
  use prc_core

  use prc_user_defined

  use blha_config

  implicit none
  private

  public :: blha_template_t
  public :: prc_blha_t
  public :: blha_driver_t
  public :: prc_blha_writer_t
  public :: blha_def_t
  public :: blha_state_t
  public :: olp_start
  public :: olp_eval
  public :: olp_info
  public :: olp_set_parameter
  public :: olp_eval2
  public :: olp_option
  public :: olp_polvec
  public :: olp_finalize
  public :: olp_print_parameter
  public :: blha_result_array_size
  public :: parameter_error_message
  public :: blha_color_c_fill_diag
  public :: blha_color_c_fill_offdiag
  public :: blha_loop_positions

  integer, parameter, public :: OLP_PARAMETER_LIMIT = 10
  integer, parameter, public :: OLP_MOMENTUM_LIMIT = 50
  integer, parameter, public :: OLP_RESULTS_LIMIT = 60


  integer, parameter :: I_ALPHA = 1
  integer, parameter :: I_GF = 2
  integer, parameter :: I_SW2 = 3

  integer, parameter :: LEN_MAX_FLAVOR_STRING = 100
  integer, parameter :: N_MAX_FLAVORS = 100

  type :: blha_template_t
    integer :: I_BORN = 0
    integer :: I_REAL = 1
    integer :: I_LOOP = 2
    integer :: I_SUB = 3
    integer :: I_DGLAP = 4
    logical, dimension(0:4) :: compute_component
    logical :: include_polarizations = .false.
    logical :: switch_off_muon_yukawas = .false.
    logical :: use_internal_color_correlations = .true.
    real(default) :: external_top_yukawa = -1._default
    integer :: ew_scheme
  contains
    procedure :: write => blha_template_write
    procedure :: init => blha_template_init
    procedure :: set_born => blha_template_set_born
    procedure :: set_real_trees => blha_template_set_real_trees
    procedure :: set_loop => blha_template_set_loop
    procedure :: set_subtraction => blha_template_set_subtraction
    procedure :: set_dglap => blha_template_set_dglap
    procedure :: set_internal_color_correlations &
       => blha_template_set_internal_color_correlations
    procedure :: get_internal_color_correlations &
       => blha_template_get_internal_color_correlations
    procedure :: compute_born => blha_template_compute_born
    procedure :: compute_real_trees => blha_template_compute_real_trees
    procedure :: compute_loop => blha_template_compute_loop
    procedure :: compute_subtraction => blha_template_compute_subtraction
    procedure :: compute_dglap => blha_template_compute_dglap
    procedure :: check => blha_template_check
    procedure :: reset => blha_template_reset
  end type blha_template_t

  type, abstract, extends (prc_user_defined_base_t) :: prc_blha_t
    integer :: n_particles
    integer :: n_hel
    integer :: n_proc
    integer, dimension(:), allocatable :: i_tree, i_spin_c, i_color_c
    integer, dimension(:), allocatable :: i_virt
    integer, dimension(:,:), allocatable :: i_hel
    integer, dimension(:), allocatable :: i_whizard_to_i_olc
    logical, dimension(3) :: ew_parameter_mask
    integer :: sqme_tree_pos
  contains
    procedure :: create_momentum_array => prc_blha_create_momentum_array
    procedure :: set_alpha_qed => prc_blha_set_alpha_qed
    procedure :: set_GF => prc_blha_set_GF
    procedure :: set_weinberg_angle => prc_blha_set_weinberg_angle
    procedure :: set_electroweak_parameters => &
       prc_blha_set_electroweak_parameters
    procedure :: read_contract_file => prc_blha_read_contract_file
    procedure :: print_parameter_file => prc_blha_print_parameter_file
    procedure :: compute_amplitude => prc_blha_compute_amplitude
     procedure :: init_blha => prc_blha_init_blha
    procedure :: set_mass_and_width => prc_blha_set_mass_and_width
    procedure :: set_particle_properties => prc_blha_set_particle_properties
    procedure :: init_ew_parameters => prc_blha_init_ew_parameters
    procedure :: compute_sqme_virt => prc_blha_compute_sqme_virt
    procedure :: compute_sqme => prc_blha_compute_sqme
    procedure :: compute_sqme_color_c_raw => prc_blha_compute_sqme_color_c_raw
    procedure :: compute_sqme_color_c => prc_blha_compute_sqme_color_c
    generic :: get_beam_helicities => get_beam_helicities_single
    generic :: get_beam_helicities => get_beam_helicities_array
    procedure :: get_beam_helicities_single => prc_blha_get_beam_helicities_single
    procedure :: get_beam_helicities_array => prc_blha_get_beam_helicities_array
    procedure :: includes_polarization => prc_blha_includes_polarization
    procedure(prc_blha_init_driver), deferred :: &
        init_driver
    procedure :: reset_i_whizard_to_i_olc => prc_blha_reset_i_whizard_to_i_olc
    procedure :: set_i_whizard_to_i_olc_trivial => prc_blha_set_i_whizard_to_i_olc_trivial
    procedure :: set_i_whizard_to_i_olc => prc_blha_set_i_whizard_to_i_olc
    generic :: get_i_whizard_to_i_olc => get_i_whizard_to_i_olc_all
    generic :: get_i_whizard_to_i_olc => get_i_whizard_to_i_olc_single
    procedure :: get_i_whizard_to_i_olc_all => prc_blha_get_i_whizard_to_i_olc_all
    procedure :: get_i_whizard_to_i_olc_single => prc_blha_get_i_whizard_to_i_olc_single
    procedure :: warmup_helicities => prc_blha_warmup_helicities
  end type prc_blha_t

  type, abstract, extends (user_defined_driver_t) :: blha_driver_t
    type(string_t) :: contract_file
    type(string_t) :: nlo_suffix
    logical :: include_polarizations = .false.
    logical :: switch_off_muon_yukawas = .false.
    real(default) :: external_top_yukawa = -1.0
    procedure(olp_start),nopass,  pointer :: &
              blha_olp_start => null ()
    procedure(olp_eval), nopass, pointer :: &
              blha_olp_eval => null()
    procedure(olp_info), nopass, pointer :: &
              blha_olp_info => null ()
    procedure(olp_set_parameter), nopass, pointer :: &
              blha_olp_set_parameter => null ()
    procedure(olp_eval2), nopass, pointer :: &
              blha_olp_eval2 => null ()
    procedure(olp_option), nopass, pointer :: &
              blha_olp_option => null ()
    procedure(olp_polvec), nopass, pointer :: &
              blha_olp_polvec => null ()
    procedure(olp_finalize), nopass, pointer :: &
              blha_olp_finalize => null ()
    procedure(olp_print_parameter), nopass, pointer :: &
              blha_olp_print_parameter => null ()
  contains
    procedure(blha_driver_set_GF), deferred :: &
       set_GF
    procedure(blha_driver_set_alpha_s), deferred :: &
       set_alpha_s
    procedure(blha_driver_set_weinberg_angle), deferred :: &
       set_weinberg_angle
    procedure(blha_driver_set_alpha_qed), deferred :: set_alpha_qed
    procedure(blha_driver_print_alpha_s), deferred :: &
       print_alpha_s
    procedure :: set_mass_and_width => blha_driver_set_mass_and_width
    procedure(blha_driver_init_dlaccess_to_library), deferred :: &
      init_dlaccess_to_library
    procedure :: load => blha_driver_load
    procedure :: read_contract_file => blha_driver_read_contract_file
  end type blha_driver_t

  type, abstract, extends (prc_user_defined_writer_t) :: prc_blha_writer_t
    type(blha_configuration_t) :: blha_cfg
  contains
    procedure :: write => prc_blha_writer_write
    procedure :: get_process_string => prc_blha_writer_get_process_string
    procedure :: get_n_proc => prc_blha_writer_get_n_proc
  end type prc_blha_writer_t

  type, abstract, extends (user_defined_def_t) :: blha_def_t
    type(string_t) :: suffix
  contains
  
  end type blha_def_t

  type, abstract, extends (user_defined_state_t) :: blha_state_t
  contains
    procedure :: reset_new_kinematics => blha_state_reset_new_kinematics
  end type blha_state_t


  interface
    subroutine olp_start (contract_file_name, ierr) bind (C,name = "OLP_Start")
      import
      character(kind = c_char, len = 1), intent(in) :: contract_file_name
      integer(kind = c_int), intent(out) :: ierr
    end subroutine olp_start
  end interface

  interface
    subroutine olp_eval (label, momenta, mu, parameters, res) &
         bind (C, name = "OLP_EvalSubProcess")
      import
      integer(kind = c_int), value, intent(in) :: label
      real(kind = c_double), value, intent(in) :: mu
      real(kind = c_double), dimension(OLP_MOMENTUM_LIMIT), intent(in) :: &
           momenta
      real(kind = c_double), dimension(OLP_PARAMETER_LIMIT), intent(in) :: &
           parameters
      real(kind = c_double), dimension(OLP_RESULTS_LIMIT), intent(out) :: res
    end subroutine olp_eval
  end interface

  interface
    subroutine olp_info (olp_file, olp_version, message) bind(C)
      import
      character(kind = c_char), intent(inout), dimension(15) :: olp_file
      character(kind = c_char), intent(inout), dimension(15) :: olp_version
      character(kind = c_char), intent(inout), dimension(255) :: message
    end subroutine olp_info
  end interface

  interface
    subroutine olp_set_parameter &
         (variable_name, real_part, complex_part, success) bind(C)
      import
      character(kind = c_char,len = 1), intent(in) :: variable_name
      real(kind = c_double), intent(in) :: real_part, complex_part
      integer(kind = c_int), intent(out) :: success
    end subroutine olp_set_parameter
  end interface

  interface
    subroutine olp_eval2 (label, momenta, mu, res, acc) bind(C)
      import
      integer(kind = c_int), intent(in) :: label
      real(kind = c_double), intent(in) :: mu
      real(kind = c_double), dimension(OLP_MOMENTUM_LIMIT), intent(in) :: momenta
      real(kind = c_double), dimension(OLP_RESULTS_LIMIT), intent(out) :: res
      real(kind = c_double), intent(out) :: acc
    end subroutine olp_eval2
  end interface

  interface
    subroutine olp_option (line, stat) bind(C)
      import
      character(kind = c_char, len=1), intent(in) :: line
      integer(kind = c_int), intent(out) :: stat
    end subroutine
  end interface

  interface
    subroutine olp_polvec (p, q, eps) bind(C)
      import
      real(kind = c_double), dimension(0:3), intent(in) :: p, q
      real(kind = c_double), dimension(0:7), intent(out) :: eps
    end subroutine
  end interface

  interface
    subroutine olp_finalize () bind(C)
      import
    end subroutine olp_finalize
  end interface

  interface
    subroutine olp_print_parameter (filename) bind(C)
      import
      character(kind = c_char, len = 1), intent(in) :: filename
    end subroutine olp_print_parameter
  end interface

  abstract interface
    subroutine blha_driver_set_GF (driver, GF)
      import
      class(blha_driver_t), intent(inout) :: driver
      real(default), intent(in) :: GF
    end subroutine blha_driver_set_GF
  end interface

  abstract interface
    subroutine blha_driver_set_alpha_s (driver, alpha_s)
       import
       class(blha_driver_t), intent(in) :: driver
       real(default), intent(in) :: alpha_s
    end subroutine blha_driver_set_alpha_s
  end interface

  abstract interface
    subroutine blha_driver_set_weinberg_angle (driver, sw2)
      import
      class(blha_driver_t), intent(inout) :: driver
      real(default), intent(in) :: sw2
    end subroutine blha_driver_set_weinberg_angle
  end interface

  abstract interface
    subroutine blha_driver_set_alpha_qed (driver, alpha)
      import
      class(blha_driver_t), intent(inout) :: driver
      real(default), intent(in) :: alpha
    end subroutine blha_driver_set_alpha_qed
  end interface

  abstract interface
    subroutine blha_driver_print_alpha_s (object)
      import
      class(blha_driver_t), intent(in) :: object
    end subroutine blha_driver_print_alpha_s
  end interface

  abstract interface
    subroutine blha_driver_init_dlaccess_to_library &
       (object, os_data, dlaccess, success)
      import
      class(blha_driver_t), intent(in) :: object
      type(os_data_t), intent(in) :: os_data
      type(dlaccess_t), intent(out) :: dlaccess
      logical, intent(out) :: success
    end subroutine blha_driver_init_dlaccess_to_library
  end interface

  abstract interface
    subroutine prc_blha_init_driver (object, os_data)
      import
      class(prc_blha_t), intent(inout) :: object
      type(os_data_t), intent(in) :: os_data
    end subroutine prc_blha_init_driver
  end interface


contains

  subroutine blha_template_write (blha_template, unit)
    class(blha_template_t), intent(in) :: blha_template
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,"(A,4(L1))") "Compute components: ", &
       blha_template%compute_component
    write (u,"(A,L1)") "Include polarizations: ", &
       blha_template%include_polarizations
    write (u,"(A,L1)") "Switch off muon yukawas: ", &
       blha_template%switch_off_muon_yukawas
    write (u,"(A,L1)") "Use internal color correlations: ", &
       blha_template%use_internal_color_correlations
  end subroutine blha_template_write

  subroutine blha_state_reset_new_kinematics (object)
    class(blha_state_t), intent(inout) :: object
    object%new_kinematics = .true.
  end subroutine blha_state_reset_new_kinematics

  pure function blha_result_array_size (n_part, amp_type) result (rsize)
    integer, intent(in) :: n_part, amp_type
    integer :: rsize
    select case (amp_type)
       case (BLHA_AMP_TREE)
          rsize = 1
       case (BLHA_AMP_LOOP)
          rsize = 4
       case (BLHA_AMP_COLOR_C)
          rsize = n_part * (n_part - 1) / 2
       case (BLHA_AMP_SPIN_C)
          rsize = 2 * n_part**2
       case default
          rsize = 0
     end select
  end function blha_result_array_size

  function prc_blha_create_momentum_array (object, p) result (mom)
    class(prc_blha_t), intent(in) :: object
    type(vector4_t), intent(in), dimension(:) :: p
    real(double), dimension(5*object%n_particles) :: mom
    integer :: n, i, k

    n = size (p)
    if (n > 10) call msg_fatal ("Number of external particles exceeds" &
           // "size of BLHA-internal momentum array")

    mom = zero
    k = 1
    do i = 1, n
       mom(k : k + 3) = vector4_get_components (p(i))
       mom(k + 4) = invariant_mass (p(i))
       k = k + 5
    end do
  end function prc_blha_create_momentum_array

  subroutine blha_template_init (template, requires_polarizations, &
       switch_off_muon_yukawas, external_top_yukawa, ew_scheme)
    class(blha_template_t), intent(inout) :: template
    logical, intent(in) :: requires_polarizations, switch_off_muon_yukawas
    real(default), intent(in) :: external_top_yukawa
    type(string_t), intent(in) :: ew_scheme
    template%compute_component = .false.
    template%include_polarizations = requires_polarizations
    template%switch_off_muon_yukawas = switch_off_muon_yukawas
    template%external_top_yukawa = external_top_yukawa
    template%ew_scheme = ew_scheme_string_to_int (ew_scheme)
  end subroutine blha_template_init

  subroutine blha_template_set_born (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component (template%I_BORN) = .true.
  end subroutine blha_template_set_born

  subroutine blha_template_set_real_trees (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component (template%I_REAL) = .true.
  end subroutine blha_template_set_real_trees

  subroutine blha_template_set_loop (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component(template%I_LOOP) = .true.
  end subroutine blha_template_set_loop

  subroutine blha_template_set_subtraction (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component (template%I_SUB) = .true.
  end subroutine blha_template_set_subtraction

  subroutine blha_template_set_dglap (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component (template%I_DGLAP) = .true.
  end subroutine blha_template_set_dglap

  subroutine blha_template_set_internal_color_correlations (template)
    class(blha_template_t), intent(inout) :: template
    template%use_internal_color_correlations = .true.
  end subroutine blha_template_set_internal_color_correlations

  pure function blha_template_get_internal_color_correlations (template) &
     result (val)
    logical :: val
    class(blha_template_t), intent(in) :: template
    val = template%use_internal_color_correlations
  end function blha_template_get_internal_color_correlations

  pure function blha_template_compute_born (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_BORN)
  end function blha_template_compute_born

  pure function blha_template_compute_real_trees (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_REAL)
  end function blha_template_compute_real_trees

  pure function blha_template_compute_loop (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_LOOP)
  end function blha_template_compute_loop

  pure function blha_template_compute_subtraction (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_SUB)
  end function blha_template_compute_subtraction

  pure function blha_template_compute_dglap (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = template%compute_component (template%I_DGLAP)
  end function blha_template_compute_dglap

  function blha_template_check (template) result (val)
    class(blha_template_t), intent(in) :: template
    logical :: val
    val = count (template%compute_component) == 1
  end function blha_template_check

  subroutine blha_template_reset (template)
    class(blha_template_t), intent(inout) :: template
    template%compute_component = .false.
  end subroutine blha_template_reset

  subroutine prc_blha_writer_write (writer, unit)
    class(prc_blha_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    write (unit, "(1x,A)")  char (writer%get_process_string ())
  end subroutine prc_blha_writer_write

  function prc_blha_writer_get_process_string (writer) result (s_proc)
    class(prc_blha_writer_t), intent(in) :: writer
    type(string_t) :: s_proc
    s_proc = var_str ("")
  end function prc_blha_writer_get_process_string

  function prc_blha_writer_get_n_proc (writer) result (n_proc)
    class(prc_blha_writer_t), intent(in) :: writer
    integer :: n_proc
    n_proc = blha_configuration_get_n_proc (writer%blha_cfg)
  end function prc_blha_writer_get_n_proc

  subroutine parameter_error_message (par)
     type(string_t), intent(in) :: par
     type(string_t) :: message
     message = "Setting of parameter " // par &
        // "failed. This happens because the chosen " &
        // "EWScheme in the BLHA file does not fit " &
        // "your parameter choice"
     call msg_fatal (char (message))
  end subroutine parameter_error_message

  subroutine blha_driver_set_mass_and_width &
         (driver, i_pdg, mass, width)
    class(blha_driver_t), intent(inout) :: driver
    integer, intent(in) :: i_pdg
    real(default), intent(in), optional :: mass
    real(default), intent(in), optional :: width
    type(string_t) :: buf
    character(kind=c_char,len=20) :: c_string
    integer :: ierr
    if (present (mass)) then
       buf = 'mass(' // str (abs(i_pdg)) // ')'
       c_string = char(buf) // c_null_char
       call driver%blha_olp_set_parameter &
                (c_string, dble(mass), 0._double, ierr)
       if (ierr == 0) then
          buf = "BLHA driver: Attempt to set mass of particle " // &
                str (abs(i_pdg)) // "failed"
          call msg_fatal (char(buf))
       end if
    end if
    if (present (width)) then
       buf = 'width(' // str (abs(i_pdg)) // ')'
       c_string = char(buf)//c_null_char
       call driver%blha_olp_set_parameter &
                (c_string, dble(width), 0._double, ierr)
       if (ierr == 0) then
          buf = "BLHA driver: Attempt to set width of particle " // &
                str (abs(i_pdg)) // "failed"
          call msg_fatal (char(buf))
       end if
    end if
  end subroutine blha_driver_set_mass_and_width

  subroutine blha_driver_load (object, os_data, success)
    class(blha_driver_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical, intent(out) :: success
    type(dlaccess_t) :: dlaccess
    type(c_funptr) :: c_fptr
    logical :: init_success

    call object%init_dlaccess_to_library (os_data, dlaccess, init_success)

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Start"))
    call c_f_procpointer (c_fptr, object%blha_olp_start)
    call check_for_error (var_str ("OLP_Start"))

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_EvalSubProcess"))
    call c_f_procpointer (c_fptr, object%blha_olp_eval)
    call check_for_error (var_str ("OLP_EvalSubProcess"))

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Info"))
    call c_f_procpointer (c_fptr, object%blha_olp_info)
    call check_for_error (var_str ("OLP_Info"))

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_SetParameter"))
    call c_f_procpointer (c_fptr, object%blha_olp_set_parameter)
    call check_for_error (var_str ("OLP_SetParameter"))

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_EvalSubProcess2"))
    call c_f_procpointer (c_fptr, object%blha_olp_eval2)
    call check_for_error (var_str ("OLP_EvalSubProcess2"))

    !!! The following three functions are not implemented in OpenLoops.
    !!! In another BLHA provider, they need to be implemented separately.

    !!! c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Option"))
    !!! call c_f_procpointer (c_fptr, object%blha_olp_option)
    !!! call check_for_error (var_str ("OLP_Option"))

    !!! c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Polvec"))
    !!! call c_f_procpointer (c_fptr, object%blha_olp_polvec)
    !!! call check_for_error (var_str ("OLP_Polvec"))

    !!! c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_Finalize"))
    !!! call c_f_procpointer (c_fptr, object%blha_olp_finalize)
    !!! call check_for_error (var_str ("OLP_Finalize"))

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("OLP_PrintParameter"))
    call c_f_procpointer (c_fptr, object%blha_olp_print_parameter)
    call check_for_error (var_str ("OLP_PrintParameter"))

    success = .true.
    contains
      subroutine check_for_error (function_name)
        type(string_t), intent(in) :: function_name
        if (dlaccess_has_error (dlaccess)) &
           call msg_fatal (char ("Loading of " // function_name // " failed!"))
     end subroutine check_for_error
  end subroutine blha_driver_load

  subroutine blha_driver_read_contract_file (driver, flavors, &
       amp_type, flv_index, label, helicities)
    class(blha_driver_t), intent(inout) :: driver
    integer, intent(in), dimension(:,:) :: flavors
    integer, intent(out), dimension(:), allocatable :: amp_type, flv_index, label
    integer, intent(out), dimension(:,:) :: helicities
    integer :: unit, filestat
    character(len=LEN_MAX_FLAVOR_STRING) :: rd_line
    logical :: read_flavor, give_warning
    integer :: label_count, i_flv
    integer :: i_hel, n_in
    integer :: i_next, n_entries
    integer, dimension(size(flavors, 1) + 2) :: i_array
    integer, dimension(size(flavors, 1) + 2) :: hel_array
    integer, parameter :: NO_NUMBER = -1000
    integer, parameter :: PROC_NOT_FOUND = -1001
    integer, parameter :: list_incr = 50
    integer :: n_found
    allocate (amp_type (N_MAX_FLAVORS), flv_index (N_MAX_FLAVORS), &
           label (N_MAX_FLAVORS))
    amp_type = -1; flv_index = -1; label = -1
    helicities = 0
    n_in = size (helicities, dim = 2)
    n_entries = size (flavors, 1) + 2
    unit = free_unit ()
    open (unit, file = char (driver%contract_file), status="old")
    read_flavor = .false.
    label_count = 1
    i_hel = 0
    n_found = 0
    give_warning = .false.
    do
      read (unit, "(A)", iostat = filestat) rd_line
      if (filestat == iostat_end) then
         exit
      else
         if (rd_line(1:13) == 'AmplitudeType') then
            if (i_hel > 2 * n_in - 1) i_hel = 0
            i_next = find_next_word_index (rd_line, 13)
            if (label_count > size (amp_type)) &
                 call extend_integer_array (amp_type, list_incr)
            if (rd_line(i_next : i_next + 4) == 'Loop') then
               amp_type(label_count) = BLHA_AMP_LOOP
            else if (rd_line(i_next : i_next + 4) == 'Tree') then
               amp_type(label_count) = BLHA_AMP_TREE
            else if (rd_line(i_next : i_next + 6) == 'ccTree') then
               amp_type(label_count) = BLHA_AMP_COLOR_C
            else if (rd_line(i_next : i_next + 6) == 'scTree' .or. &
                 rd_line(i_next : i_next + 14) == 'sctree_polvect') then
               amp_type(label_count) = BLHA_AMP_SPIN_C
            else
               call msg_fatal ("AmplitudeType present but AmpType not known!")
            end if
            read_flavor = .true.
         else if (read_flavor) then
            i_array = create_flavor_string (rd_line, n_entries)
            if (driver%include_polarizations) then
               hel_array = create_helicity_string (rd_line, n_entries)
               call check_helicity_array (hel_array, n_entries, n_in)
            else
               hel_array = 0
            end if
            if (.not. all (i_array == PROC_NOT_FOUND)) then
               do i_flv = 1, size (flavors, 2)
                   if (all (i_array (1 : n_entries - 2) == flavors (:,i_flv))) then
                      if (label_count > size (label)) &
                           call extend_integer_array (label, list_incr)
                      label(label_count) = i_array (n_entries)
                      if (label_count > size (flv_index)) &
                           call extend_integer_array (flv_index, list_incr)
                      flv_index (label_count) = i_flv + i_hel
                      if (driver%include_polarizations) then
                         helicities (label(label_count), :) = hel_array (1:n_in)
                         i_hel = i_hel + 1
                      end if
                      n_found = n_found + 1
                      label_count = label_count + 1
                      exit
                   end if
               end do
               give_warning = .false.
            else
               give_warning = .true.
            end if
            read_flavor = .false.
         end if
      end if
    end do
    call crop_integer_array (amp_type, label_count-1)
    if (n_found == 0) then
       call msg_fatal ("The desired process has not been found ",  &
            [var_str ("by the OLP-Provider. Maybe the value of alpha_power "), &
             var_str ("or alphas_power does not correspond to the process. "), &
             var_str ("If you are using OpenLoops, you can set the option "), &
             var_str ("openloops_verbosity to a value larger than 1 to obtain "),  &
             var_str ("more information")])
    else if (give_warning) then
       call msg_warning ("Some processes have not been found in the OLC file.", &
            [var_str ("This is because these processes do not fit the required "), &
             var_str ("coupling alpha_power and alphas_power. Be aware that the "), &
             var_str ("results of this calculation are not necessarily an accurate "), &
             var_str ("description of the physics of interest.")])
    end if
    close(unit)

  contains

    function create_flavor_string (s, n_entries) result (i_array)
      character(len=LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(in) :: n_entries
      integer, dimension(n_entries) :: i_array
      integer :: k, current_position
      integer :: i_entry
      k = 1; current_position = 1
      do
         if (current_position > LEN_MAX_FLAVOR_STRING) &
            call msg_fatal ("Read OLC File: Current position exceeds maximum value")
         if (s(current_position:current_position) /= " ") then
            call create_flavor (s, i_entry, current_position)
            if (i_entry /= NO_NUMBER .and. i_entry /= PROC_NOT_FOUND) then
               i_array(k) = i_entry
               k = k + 1
               if (k > n_entries) then
                  return
               else
                  call increment_current_position (s, current_position)
               end if
            else if (i_entry == PROC_NOT_FOUND) then
               i_array = PROC_NOT_FOUND
               return
            else
               call increment_current_position (s, current_position)
            end if
         else
            call increment_current_position (s, current_position)
         end if
      end do
    end function create_flavor_string

    function create_helicity_string (s, n_entries) result (hel_array)
      character(len = LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(in) :: n_entries
      integer, dimension(n_entries) :: hel_array
      integer :: k, current_position
      integer :: hel
      k = 1; current_position = 1
      do
         if (current_position > LEN_MAX_FLAVOR_STRING) &
            call msg_fatal ("Read OLC File: Current position exceeds maximum value")
         if (s(current_position:current_position) /= " ") then
            call create_helicity (s, hel, current_position)
            if (hel >= -1 .and. hel <= 1) then
               hel_array(k) = hel
               k = k + 1
               if (k > n_entries) then
                  return
               else
                  call increment_current_position (s, current_position)
               end if
            else
               call increment_current_position (s, current_position)
            end if
         else
            call increment_current_position (s, current_position)
         end if
      end do
    end function create_helicity_string

    subroutine increment_current_position (s, current_position)
      character(len = LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(inout) :: current_position
      current_position = find_next_word_index (s, current_position)
    end subroutine increment_current_position

    subroutine get_next_buffer (s, current_position, buf, last_buffer_index)
      character(len = LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(inout) :: current_position
      character(len = 10), intent(out) :: buf
      integer, intent(out) :: last_buffer_index
      integer :: i
      i = 1; buf = ""
      do
         if (s(current_position:current_position) /= " ") then
            buf(i:i) = s(current_position:current_position)
            i = i + 1; current_position = current_position + 1
         else
            exit
         end if
      end do
      last_buffer_index = i
    end subroutine get_next_buffer

    function is_particle_buffer (buf, i) result (valid)
      logical :: valid
      character(len = 10), intent(in) :: buf
      integer, intent(in) :: i
      valid = (buf(1 : i - 1) /= "->" .and. buf(1 : i - 1) /= "|" &
         .and. buf(1 : i - 1) /= "Process")
    end function is_particle_buffer

    subroutine create_flavor (s, i_particle, current_position)
      character(len=LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(out) :: i_particle
      integer, intent(inout) :: current_position
      character(len=10) :: buf
      integer :: i, last_buffer_index
      call get_next_buffer (s, current_position, buf, last_buffer_index)
      i = last_buffer_index
      if (is_particle_buffer (buf, i)) then
         call strip_helicity (buf, i)
         i_particle = read_ival (var_str (buf(1 : i - 1)))
      else if (buf(1 : i - 1) == "Process") then
         i_particle = PROC_NOT_FOUND
      else
         i_particle = NO_NUMBER
      end if
    end subroutine create_flavor

    subroutine create_helicity (s, helicity, current_position)
      character(len = LEN_MAX_FLAVOR_STRING), intent(in) :: s
      integer, intent(out) :: helicity
      integer, intent(inout) :: current_position
      character(len = 10) :: buf
      integer :: i, last_buffer_index
      logical :: success
      call get_next_buffer (s, current_position, buf, last_buffer_index)
      i = last_buffer_index
      if (is_particle_buffer (buf, i)) then
         call strip_flavor (buf, i, helicity, success)
      else
         helicity = 0
      end if
    end subroutine create_helicity

    subroutine strip_helicity (buf, i)
      character(len = 10), intent(in) :: buf
      integer, intent(inout) :: i
      integer :: i_last
      i_last = i - 1
      if (i_last < 4) return
      if (buf(i_last - 2 : i_last) == "(1)") then
         i = i - 3
      else if (buf(i_last - 3 : i_last) == "(-1)") then
         i = i - 4
      end if
    end subroutine strip_helicity

    subroutine strip_flavor (buf, i, helicity, success)
      character(len = 10), intent(in) :: buf
      integer, intent(in) :: i
      integer, intent(out) :: helicity
      logical, intent(out) :: success
      integer :: i_last
      i_last = i - 1
      helicity = 0
      if (i_last < 4) return
      if (buf(i_last - 2 : i_last) == "(1)") then
         helicity = 1
         success = .true.
      else if (buf(i_last - 3 : i_last) == "(-1)") then
         helicity = -1
         success = .true.
      else
         success = .false.
      end if
    end subroutine strip_flavor

    function find_next_word_index (word, i_start) result (i_next)
      character(len = LEN_MAX_FLAVOR_STRING), intent(in) :: word
      integer, intent(in) :: i_start
      integer :: i_next
      i_next = i_start + 1
      do
         if (word(i_next : i_next) /= " ") then
            exit
         else
            i_next = i_next + 1
         end if
         if (i_next > LEN_MAX_FLAVOR_STRING) &
              call msg_fatal ("Find next word: line limit exceeded")
      end do
    end function find_next_word_index

    subroutine check_helicity_array (hel_array, n_entries, n_in)
      integer, intent(in), dimension(:) :: hel_array
      integer, intent(in) :: n_entries, n_in
      integer :: n_particles, i
      logical :: valid
      n_particles = n_entries - 2
      !!! only allow polarisations for incoming fermions for now
      valid = all (hel_array (n_in + 1 : n_particles) == 0)
      do i = 1, n_in
         valid = valid .and. (hel_array(i) == 1 .or. hel_array(i) == -1)
      end do
      if (.not. valid) &
         call msg_fatal ("Invalid helicities encountered!")
    end subroutine check_helicity_array

  end subroutine blha_driver_read_contract_file

  subroutine prc_blha_set_alpha_qed (object, model)
    class(prc_blha_t), intent(inout) :: object
    type(model_data_t), intent(in), target :: model
    real(default) :: alpha

    alpha = one / model%get_real (var_str ('alpha_em_i'))

    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%set_alpha_qed (alpha)
    end select
  end subroutine prc_blha_set_alpha_qed

  subroutine prc_blha_set_GF (object, model)
    class(prc_blha_t), intent(inout) :: object
    type(model_data_t), intent(in), target :: model
    real(default) :: GF

    GF = model%get_real (var_str ('GF'))
    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%set_GF (GF)
    end select
  end subroutine prc_blha_set_GF

  subroutine prc_blha_set_weinberg_angle (object, model)
    class(prc_blha_t), intent(inout) :: object
    type(model_data_t), intent(in), target :: model
    real(default) :: sw2

    sw2 = model%get_real (var_str ('sw2'))
    select type (driver => object%driver)
    class is (blha_driver_t)
      call driver%set_weinberg_angle (sw2)
    end select
  end subroutine prc_blha_set_weinberg_angle

  subroutine prc_blha_set_electroweak_parameters (object, model)
     class(prc_blha_t), intent(inout) :: object
     type(model_data_t), intent(in), target :: model
     if (count (object%ew_parameter_mask) == 0) then
        call msg_fatal ("Cannot decide EW parameter setting: No scheme set!")
     else if (count (object%ew_parameter_mask) > 1) then
        call msg_fatal ("Cannot decide EW parameter setting: More than one scheme set!")
     end if
     if (object%ew_parameter_mask (I_ALPHA)) call object%set_alpha_qed (model)
     if (object%ew_parameter_mask (I_GF)) call object%set_GF (model)
     if (object%ew_parameter_mask (I_SW2)) call object%set_weinberg_angle (model)
  end subroutine prc_blha_set_electroweak_parameters

  subroutine prc_blha_read_contract_file (object, flavors)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in), dimension(:,:) :: flavors
    integer, dimension(:), allocatable :: amp_type, flv_index, label
    integer, dimension(:,:), allocatable :: helicities
    integer :: i_proc
    allocate (helicities (N_MAX_FLAVORS, object%data%n_in))
    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%read_contract_file (flavors, amp_type, flv_index, &
            label, helicities)
    end select
    object%n_proc = count (amp_type >= 0)
    do i_proc = 1, object%n_proc
       if (amp_type (i_proc) < 0) exit
       select case (amp_type (i_proc))
       case (BLHA_AMP_TREE)
          if (allocated (object%i_tree)) then
             object%i_tree(flv_index(i_proc)) = label(i_proc)
          else
             call msg_fatal ("Tree matrix element present, &
                  &but neither Born nor real indices are allocated!")
          end if
       case (BLHA_AMP_COLOR_C)
          if (allocated (object%i_color_c)) then
             object%i_color_c(flv_index(i_proc)) = label(i_proc)
          else
             call msg_fatal ("Color-correlated matrix element present, &
                  &but cc-indices are not allocated!")
          end if
       case (BLHA_AMP_SPIN_C)
          if (allocated (object%i_spin_c)) then
             object%i_spin_c(flv_index(i_proc)) = label(i_proc)
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
       if (allocated (object%i_hel)) &
          object%i_hel (i_proc, :) = helicities (label(i_proc), :)
    end do
  end subroutine prc_blha_read_contract_file

  subroutine prc_blha_print_parameter_file (object, i_component)
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: i_component
    type(string_t) :: filename

    select type (def => object%def)
    class is (blha_def_t)
       filename = def%basename // '_' // str (i_component) // '.olp_parameters'
    end select
    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%blha_olp_print_parameter (char(filename)//c_null_char)
    end select
  end subroutine prc_blha_print_parameter_file

  function prc_blha_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
       core_state)  result (amp)
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: core_state
    complex(default) :: amp
    select type (core_state)
    class is (blha_state_t)
      core_state%alpha_qcd = object%qcd%alpha%get (fac_scale)
    end select
    amp = zero
  end function prc_blha_compute_amplitude

  subroutine prc_blha_init_blha (object, blha_template, n_in, &
         n_particles, n_flv, n_hel)
    class(prc_blha_t), intent(inout) :: object
    type(blha_template_t), intent(in) :: blha_template
    integer, intent(in) :: n_in, n_particles, n_flv, n_hel
    object%n_particles = n_particles
    object%n_flv = n_flv
    object%n_hel = n_hel
    if (blha_template%compute_loop ()) then
       if (blha_template%include_polarizations) then
          allocate (object%i_virt (n_flv * n_hel), &
               object%i_color_c (n_flv * n_hel))
          if (blha_template%use_internal_color_correlations) then
             allocate (object%i_hel (n_flv * n_in * n_hel * 2, n_in))
          else
             allocate (object%i_hel (n_flv * n_in * n_hel, n_in))
          end if
       else
          allocate (object%i_virt (n_flv), object%i_color_c (n_flv))
       end if
       object%i_virt = 0
       object%i_color_c = 0
    else if (blha_template%compute_subtraction ()) then
       if (blha_template%include_polarizations) then
          allocate (object%i_tree (n_flv * n_hel), &
               object%i_color_c (n_flv * n_hel), &
               object%i_spin_c (n_flv * n_hel), &
               object%i_hel (3 * (n_flv * n_hel * n_in), n_in))
          object%i_hel = 0
       else
          allocate (object%i_tree (n_flv), object%i_color_c (n_flv) , &
               object%i_spin_c (n_flv))
       end if
       object%i_tree = 0
       object%i_color_c = 0
       object%i_spin_c = 0
    else if (blha_template%compute_real_trees () .or. blha_template%compute_born () &
           .or. blha_template%compute_dglap ()) then
       if (blha_template%include_polarizations) then
          allocate (object%i_hel (n_flv * n_hel * n_in, n_in))
          object%i_hel = 0
       end if
       allocate (object%i_tree (n_flv * n_hel))
       object%i_tree = 0
    end if

    call object%init_ew_parameters (blha_template%ew_scheme)

    select type (driver => object%driver)
    class is (blha_driver_t)
       driver%include_polarizations = blha_template%include_polarizations
       driver%switch_off_muon_yukawas = blha_template%switch_off_muon_yukawas
       driver%external_top_yukawa = blha_template%external_top_yukawa
    end select
  end subroutine prc_blha_init_blha

  subroutine prc_blha_set_mass_and_width (object, i_pdg, mass, width)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in) :: i_pdg
    real(default), intent(in) :: mass, width
    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%set_mass_and_width (i_pdg, mass, width)
    end select
  end subroutine prc_blha_set_mass_and_width

  subroutine prc_blha_set_particle_properties (object, model)
    class(prc_blha_t), intent(inout) :: object
    class(model_data_t), intent(in), target :: model
    integer :: i, i_pdg
    type(flavor_t) :: flv
    real(default) :: mass, width
    integer :: ierr
    real(default) :: top_yukawa
    do i = 1, OLP_N_MASSIVE_PARTICLES
       i_pdg = OLP_MASSIVE_PARTICLES(i)
       if (i_pdg < 0) cycle
       call flv%init (i_pdg, model)
       mass = flv%get_mass (); width = flv%get_width ()
       select type (driver => object%driver)
       class is (blha_driver_t)
          call driver%set_mass_and_width (i_pdg, mass = mass, width = width)
          if (i_pdg == 5) call driver%blha_olp_set_parameter &
             ('yuk(5)'//c_null_char, dble(mass), 0._double, ierr)
          if (i_pdg == 6) then
             if (driver%external_top_yukawa > 0._default) then
                top_yukawa = driver%external_top_yukawa
             else
                top_yukawa = mass
             end if
             call driver%blha_olp_set_parameter &
                ('yuk(6)'//c_null_char, dble(top_yukawa), 0._double, ierr)
          end if
          if (driver%switch_off_muon_yukawas) then
             if (i_pdg == 13) call driver%blha_olp_set_parameter &
                ('yuk(13)' //c_null_char, 0._double, 0._double, ierr)
          end if
       end select
    end do
  end subroutine prc_blha_set_particle_properties

  subroutine prc_blha_init_ew_parameters (object, ew_scheme)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in) :: ew_scheme
    object%ew_parameter_mask = .false.
    select case (ew_scheme)
    case (BLHA_EW_QED)
       object%ew_parameter_mask (I_ALPHA) = .true.
    case (BLHA_EW_GF)
       object%ew_parameter_mask (I_GF) = .true.
    end select
  end subroutine prc_blha_init_ew_parameters

  subroutine prc_blha_compute_sqme_virt (object, &
       i_flv, p, ren_scale, sqme, bad_point)
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    real(default), dimension(4), intent(out) :: sqme
    logical, intent(out) :: bad_point
    real(double), dimension(5 * object%n_particles) :: mom
    real(double), dimension(:), allocatable :: r
    real(double) :: mu_dble
    real(double) :: acc_dble
    real(default) :: acc
    real(default) :: alpha_s
    if (object%i_virt(i_flv) > 0) then
       allocate (r (blha_result_array_size (object%n_particles, BLHA_AMP_LOOP)))
       call msg_debug2 (D_VIRTUAL, "prc_blha_compute_sqme_virt")
       call msg_debug2 (D_VIRTUAL, "i_flv", i_flv)
       call msg_debug2 (D_VIRTUAL, "object%i_virt(i_flv)", object%i_virt(i_flv))
       if (debug2_active (D_VIRTUAL)) then
           call msg_debug2 (D_VIRTUAL, "use momenta: ")
           call vector4_write_set (p, show_mass = .true., &
                check_conservation = .true.)
       end if
       mom = object%create_momentum_array (p)
       if (vanishes (ren_scale)) &
            call msg_fatal ("prc_blha_compute_sqme_virt: ren_scale vanishes")
       mu_dble = dble(ren_scale)
       alpha_s = object%qcd%alpha%get (ren_scale)
       select type (driver => object%driver)
       class is (blha_driver_t)
          call driver%set_alpha_s (alpha_s)
          call driver%blha_olp_eval2 (object%i_virt(i_flv), mom, mu_dble, r, acc_dble)
       end select
       acc = acc_dble
       sqme = r(1:4)
       bad_point = acc > object%maximum_accuracy
       if (object%includes_polarization ()) sqme = object%n_hel * sqme
    else
       sqme = zero
    end if
  end subroutine prc_blha_compute_sqme_virt

  subroutine prc_blha_compute_sqme (object, i_flv, p, &
      ren_scale, sqme, bad_point)
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(out) :: sqme
    logical, intent(out) :: bad_point
    real(double), dimension(5*object%n_particles) :: mom
    real(double), dimension(OLP_RESULTS_LIMIT) :: r
    real(double) :: mu_dble, acc_dble
    real(default) :: acc, alpha_s
    if (object%i_tree(i_flv) > 0) then
       mom = object%create_momentum_array (p)
       if (vanishes (ren_scale)) &
            call msg_fatal ("prc_blha_compute_sqme: ren_scale vanishes")
       mu_dble = dble(ren_scale)
       alpha_s = object%qcd%alpha%get (ren_scale)
       select type (driver => object%driver)
       class is (blha_driver_t)
          call driver%set_alpha_s (alpha_s)
          call driver%blha_olp_eval2 (object%i_tree(i_flv), mom, &
               mu_dble, r, acc_dble)
          sqme = r(object%sqme_tree_pos)
       end select
       acc = acc_dble
       bad_point = acc > object%maximum_accuracy
       if (object%includes_polarization ()) sqme = object%n_hel * sqme
    else
       sqme = zero
    end if
  end subroutine prc_blha_compute_sqme

  subroutine blha_color_c_fill_diag (sqme_born, flavors, sqme_color_c)
     real(default), intent(in) :: sqme_born
     integer, intent(in), dimension(:) :: flavors
     real(default), intent(inout), dimension(:,:) :: sqme_color_c
     integer :: i
     do i = 1, size (flavors)
        if (is_quark (flavors(i))) then
           sqme_color_c (i, i) = -cf * sqme_born
        else if (is_gluon (flavors(i))) then
           sqme_color_c (i, i) = -ca * sqme_born
        else
           sqme_color_c (i, i) = zero
        end if
     end do
  end subroutine blha_color_c_fill_diag

  subroutine blha_color_c_fill_offdiag (n, r, sqme_color_c, offset, n_flv)
    integer, intent(in) :: n
    real(default), intent(in), dimension(:) :: r
    real(default), intent(inout), dimension(:,:) :: sqme_color_c
    integer, intent(in), optional :: offset, n_flv
    integer :: i, j, pos, incr
    if (present (offset)) then
       incr = offset
    else
       incr = 0
    end if
    do j = 1, n
       do i = 1, j
          if (i /= j) then
             pos = (j - 1) * (j - 2) / 2 + i
             if (present (n_flv))  incr = incr + n_flv - 1
             if (present (offset))  pos = pos + incr
             sqme_color_c (i, j) = -r (pos)
          end if
          sqme_color_c (j, i) = sqme_color_c (i, j)
       end do
    end do
  end subroutine blha_color_c_fill_offdiag

  subroutine prc_blha_compute_sqme_color_c_raw &
     (object, i_flv, p, ren_scale, rr, bad_point)
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(out), dimension(:) :: rr
    logical, intent(out) :: bad_point
    real(double), dimension(5 * object%n_particles) :: mom
    real(double), dimension(size(rr)) :: r
    real(default) :: alpha_s, acc
    real(double) :: mu_dble, acc_dble
    if (object%i_color_c(i_flv) > 0) then
       mom = object%create_momentum_array (p)
       if (vanishes (ren_scale)) &
          call msg_fatal ("prc_blha_compute_sqme_color_c: ren_scale vanishes")
       mu_dble = dble(ren_scale)
       alpha_s = object%qcd%alpha%get (ren_scale)

       select type (driver => object%driver)
       class is (blha_driver_t)
          call driver%set_alpha_s (alpha_s)
          call driver%blha_olp_eval2 (object%i_color_c(i_flv), &
               mom, mu_dble, r, acc_dble)
       end select
       rr = r
       acc = acc_dble
       bad_point = acc > object%maximum_accuracy
       if (object%includes_polarization ())  rr = object%n_hel * rr
    else
       rr = zero
    end if
  end subroutine prc_blha_compute_sqme_color_c_raw

  subroutine prc_blha_compute_sqme_color_c &
         (object, i_flv, p, ren_scale, born_color_c, bad_point, born_out)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(inout), dimension(:,:) :: born_color_c
    real(default), intent(out), optional :: born_out
    logical, intent(out) :: bad_point
    real(default), dimension(:), allocatable :: r
    logical :: bad_point2
    real(default) :: born
    integer, dimension(:), allocatable :: flavors
    allocate (r (blha_result_array_size &
         (size(born_color_c, dim=1), BLHA_AMP_COLOR_C)))
    call object%compute_sqme_color_c_raw (i_flv, p, ren_scale, r, bad_point)

    select type (driver => object%driver)
    class is (blha_driver_t)
       if (allocated (object%i_tree)) then
          call object%compute_sqme (i_flv, p, ren_scale, born, bad_point2)
       else
          born = zero
       end if
       if (present (born_out)) born_out = born
    end select
    call blha_color_c_fill_offdiag (object%n_particles, r, born_color_c)
    flavors = object%get_flv_state (i_flv)
    call blha_color_c_fill_diag (born, flavors, born_color_c)

    bad_point = bad_point .or. bad_point2
  end subroutine prc_blha_compute_sqme_color_c

  function prc_blha_get_beam_helicities_single (object, i, invert_second) result (hel)
    integer, dimension(:), allocatable :: hel
    class(prc_blha_t), intent(in) :: object
    logical, intent(in), optional :: invert_second
    integer, intent(in) :: i
    logical :: inv
    inv = .false.; if (present (invert_second)) inv = invert_second
    allocate (hel (object%data%n_in))
    hel = object%i_hel (i, :)
    if (inv .and. object%data%n_in == 2) hel(2) = -hel(2)
  end function prc_blha_get_beam_helicities_single

  function prc_blha_includes_polarization (object) result (polarized)
    logical :: polarized
    class(prc_blha_t), intent(in) :: object
    select type (driver => object%driver)
    class is (blha_driver_t)
       polarized = driver%include_polarizations
    end select
  end function prc_blha_includes_polarization

  function prc_blha_get_beam_helicities_array (object, invert_second) result (hel)
    integer, dimension(:,:), allocatable :: hel
    class(prc_blha_t), intent(in) :: object
    logical, intent(in), optional :: invert_second
    integer :: i
    allocate (hel (object%n_proc, object%data%n_in))
    do i = 1, object%n_proc
       hel(i,:) = object%get_beam_helicities (i, invert_second)
    end do
  end function prc_blha_get_beam_helicities_array

  subroutine prc_blha_reset_i_whizard_to_i_olc (object)
    class(prc_blha_t), intent(inout) :: object
    if (allocated (object%i_whizard_to_i_olc)) &
         deallocate (object%i_whizard_to_i_olc)
  end subroutine prc_blha_reset_i_whizard_to_i_olc

  recursive function blha_loop_positions (i_flv, n_sub) result (index)
    integer :: index
    integer, intent(in) :: i_flv, n_sub
    index = 0
    if (i_flv == 1) then
       index = 1
    else
       index = blha_loop_positions (i_flv - 1, n_sub) + n_sub + 1
    end if
  end function blha_loop_positions

  subroutine prc_blha_set_i_whizard_to_i_olc_trivial (object, n_flv, n_hel)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in) :: n_flv, n_hel
    integer :: i_flv
    call msg_debug (D_CORE, "setting up trivial helicity list")
    if (allocated (object%i_whizard_to_i_olc)) &
         deallocate (object%i_whizard_to_i_olc)
    allocate (object%i_whizard_to_i_olc (n_flv * n_hel))
    object%i_whizard_to_i_olc = 0
    do i_flv = 1, n_flv
       object%i_whizard_to_i_olc(i_flv) = i_flv
    end do
  end subroutine prc_blha_set_i_whizard_to_i_olc_trivial

  subroutine prc_blha_set_i_whizard_to_i_olc (object, helicities)
    class(prc_blha_t), intent(inout) :: object
    integer, intent(in), dimension(:,:) :: helicities
    integer, dimension(:, :), allocatable :: hel_olc
    integer :: n1, n2
    integer :: i, j
    call msg_debug (D_CORE, "setting up helicity list from OLC file")
    n1 = size (helicities, dim=1)
    n2 = size (object%get_beam_helicities (), dim=1)
    if (allocated (object%i_whizard_to_i_olc)) &
         deallocate (object%i_whizard_to_i_olc)
    allocate (object%i_whizard_to_i_olc (n2))
    allocate (hel_olc (n2, 2))
    hel_olc = object%get_beam_helicities (invert_second = .true.)
    do i = 1, n1
       do j = 1, n2
          if (all (helicities(i, :) == hel_olc(j, :))) &
             object%i_whizard_to_i_olc(j) = i
       end do
    end do
    deallocate (hel_olc)
  end subroutine prc_blha_set_i_whizard_to_i_olc

  function prc_blha_get_i_whizard_to_i_olc_all (object) result (i_out)
    integer, dimension(:), allocatable :: i_out
    class(prc_blha_t), intent(in) :: object
    allocate (i_out (size (object%i_whizard_to_i_olc)))
    i_out = object%i_whizard_to_i_olc
  end function prc_blha_get_i_whizard_to_i_olc_all

  function prc_blha_get_i_whizard_to_i_olc_single (object, i) result (i_out)
    integer :: i_out
    class(prc_blha_t), intent(in) :: object
    integer, intent(in) :: i
    i_out = object%i_whizard_to_i_olc (i)
  end function prc_blha_get_i_whizard_to_i_olc_single

  subroutine prc_blha_warmup_helicities (object, p)
    class(prc_blha_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:) :: p
    real(double), dimension(5 * object%n_particles) :: mom
    real(double), dimension(:), allocatable :: r
    real(default) :: alpha_s
    real(double) :: ren_scale, acc
    integer :: i_hel
    allocate (r (blha_result_array_size (object%n_particles, BLHA_AMP_LOOP)))
    alpha_s = 0.1178_double
    ren_scale = 200._double
    mom = object%create_momentum_array (p)
    select type (driver => object%driver)
    class is (blha_driver_t)
       call driver%set_alpha_s (alpha_s)
       do i_hel = 1, object%n_hel
          call driver%blha_olp_eval2 (i_hel, mom, ren_scale, r, acc)
       end do
    end select
  end subroutine prc_blha_warmup_helicities


end module blha_olp_interfaces

