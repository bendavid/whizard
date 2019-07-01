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

module prc_openloops

  use, intrinsic :: iso_c_binding !NODEP!

  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use string_utils, only: str
  use constants
  use numeric_utils
  use diagnostics
  use system_dependencies
  use physics_defs
  use variables
  use os_interface
  use lorentz
  use interactions
  use sm_qcd
  use sm_physics, only: top_width_sm_lo, top_width_sm_qcd_nlo_jk
  use model_data

  use prclib_interfaces
  use prc_core_def
  use prc_core

  use blha_config
  use blha_olp_interfaces


  implicit none
  private

  public :: openloops_def_t
  public :: openloops_state_t
  public :: prc_openloops_t

  real(default), parameter :: openloops_default_bmass = 0._default
  real(default), parameter :: openloops_default_topmass = 172._default
  real(default), parameter :: openloops_default_topwidth = 0._default
  real(default), parameter :: openloops_default_wmass = 80.399_default
  real(default), parameter :: openloops_default_wwidth = 0._default
  real(default), parameter :: openloops_default_zmass = 91.1876_default
  real(default), parameter :: openloops_default_zwidth = 0._default
  real(default), parameter :: openloops_default_higgsmass = 125._default
  real(default), parameter :: openloops_default_higgswidth = 0._default

  integer :: N_EXTERNAL = 0


  type, extends (prc_blha_writer_t) :: openloops_writer_t
  contains
    procedure, nopass :: type_name => openloops_writer_type_name
  end type openloops_writer_t

  type, extends (blha_def_t) :: openloops_def_t
     integer :: verbosity
  contains
    procedure :: init => openloops_def_init
    procedure, nopass :: type_string => openloops_def_type_string
    procedure :: write => openloops_def_write
    procedure :: read => openloops_def_read
    procedure :: allocate_driver => openloops_def_allocate_driver
  end type openloops_def_t

  type, extends (blha_driver_t) :: openloops_driver_t
    integer :: n_external = 0
    type(string_t) :: olp_file
    procedure(ol_evaluate_scpowheg), nopass, pointer :: &
         evaluate_spin_correlations_powheg => null ()
  contains
    procedure :: init_dlaccess_to_library => openloops_driver_init_dlaccess_to_library
    procedure :: set_alpha_s => openloops_driver_set_alpha_s
    procedure :: set_alpha_qed => openloops_driver_set_alpha_qed
    procedure :: set_GF => openloops_driver_set_GF
    procedure :: set_weinberg_angle => openloops_driver_set_weinberg_angle
    procedure :: print_alpha_s => openloops_driver_print_alpha_s
    procedure, nopass :: type_name => openloops_driver_type_name
    procedure :: load_sc_procedure => openloops_driver_load_sc_procedure
  end type openloops_driver_t

  type :: openloops_threshold_data_t
    logical :: nlo = .true.
    real(default) :: alpha_ew
    real(default) :: sinthw
    real(default) :: m_b, m_W
    real(default) :: vtb
  contains
    procedure :: compute_top_width => &
         openloops_threshold_data_compute_top_width
  end type openloops_threshold_data_t

  type, extends (blha_state_t) :: openloops_state_t
    type(openloops_threshold_data_t), allocatable :: threshold_data
  contains
    procedure :: init_threshold => openloops_state_init_threshold
    procedure :: write => openloops_state_write
  end type openloops_state_t

  type, extends (prc_blha_t) :: prc_openloops_t
  contains
    procedure :: allocate_workspace => prc_openloops_allocate_workspace
    procedure :: init_driver => prc_openloops_init_driver
    procedure :: write => prc_openloops_write
    procedure :: write_name => prc_openloops_write_name
    procedure :: prepare_library => prc_openloops_prepare_library
    procedure :: load_driver => prc_openloops_load_driver
    procedure :: start => prc_openloops_start
    procedure :: set_n_external => prc_openloops_set_n_external
    procedure :: reset_parameters => prc_openloops_reset_parameters
    procedure :: set_verbosity => prc_openloops_set_verbosity
    procedure :: create_and_load_extra_libraries => &
         prc_openloops_create_and_load_extra_libraries
    procedure :: compute_sqme_spin_c => prc_openloops_compute_sqme_spin_c
  end type prc_openloops_t


  abstract interface
     subroutine ol_evaluate_scpowheg (id, pp, emitter, res, resmunu) bind(C)
       import
       integer(kind = c_int), value :: id, emitter
       real(kind = c_double), intent(in) :: pp(5 * N_EXTERNAL)
       real(kind = c_double), intent(out) :: res(N_EXTERNAL), resmunu(16)
     end subroutine ol_evaluate_scpowheg
  end interface


contains

  function openloops_threshold_data_compute_top_width &
       (data, mtop, alpha_s) result (wtop)
    real(default) :: wtop
    class(openloops_threshold_data_t), intent(in) :: data
    real(default), intent(in) :: mtop, alpha_s
    if (data%nlo) then
       wtop = top_width_sm_qcd_nlo_jk (data%alpha_ew, data%sinthw, &
              data%vtb, mtop, data%m_W, data%m_b, alpha_s)
    else
       wtop = top_width_sm_lo (data%alpha_ew, data%sinthw, data%vtb, &
              mtop, data%m_W, data%m_b)
    end if
  end function openloops_threshold_data_compute_top_width

  subroutine openloops_state_init_threshold (object, model)
    class(openloops_state_t), intent(inout) :: object
    type(model_data_t), intent(in) :: model
    if (model%get_name () == "SM_tt_threshold") then
       allocate (object%threshold_data)
       associate (data => object%threshold_data)
          data%nlo = btest (int (model%get_real (var_str ('offshell_strategy'))), 0)
          data%alpha_ew = one / model%get_real (var_str ('alpha_em_i'))
          data%sinthw = model%get_real (var_str ('sw'))
          data%m_b = model%get_real (var_str ('mb'))
          data%m_W = model%get_real (var_str ('mW'))
          data%vtb = model%get_real (var_str ('Vtb'))
       end associate
    end if
  end subroutine openloops_state_init_threshold

  function openloops_writer_type_name () result (string)
    type(string_t) :: string
    string = "openloops"
  end function openloops_writer_type_name

  subroutine openloops_def_init (object, basename, model_name, &
     prt_in, prt_out, nlo_type, var_list)
    class(openloops_def_t), intent(inout) :: object
    type(string_t), intent(in) :: basename, model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    integer, intent(in) :: nlo_type
    type(var_list_t), intent(in) :: var_list
  
    object%basename = basename
    allocate (openloops_writer_t :: object%writer)
    select case (nlo_type)
    case (BORN)
       object%suffix = '_BORN'
    case (NLO_REAL)
       object%suffix = '_REAL'
    case (NLO_VIRTUAL)
       object%suffix = '_LOOP'
    case (NLO_SUBTRACTION, NLO_MISMATCH)
       object%suffix = '_SUB'
    case (NLO_DGLAP)
       object%suffix = '_DGLAP'
    end select
  
    select type (writer => object%writer)
    class is (prc_blha_writer_t)
       call writer%init (model_name, prt_in, prt_out)
    end select
    object%verbosity = var_list%get_ival (var_str ("openloops_verbosity"))
  end subroutine openloops_def_init

  function openloops_def_type_string () result (string)
    type(string_t) :: string
    string = "openloops"
  end function openloops_def_type_string

  subroutine openloops_def_write (object, unit)
    class(openloops_def_t), intent(in) :: object
    integer, intent(in) :: unit
    select type (writer => object%writer)
    type is (openloops_writer_t)
       call writer%write (unit)
    end select
  end subroutine openloops_def_write

  subroutine openloops_driver_init_dlaccess_to_library &
     (object, os_data, dlaccess, success)
    class(openloops_driver_t), intent(in) :: object
    type(os_data_t), intent(in) :: os_data
    type(dlaccess_t), intent(out) :: dlaccess
    logical, intent(out) :: success
    type(string_t) :: ol_library, msg_buffer
    ol_library = OPENLOOPS_DIR // '/lib/libopenloops.' // &
         os_data%shrlib_ext
    msg_buffer = "One-Loop-Provider: Using OpenLoops"
    call msg_message (char(msg_buffer))
    msg_buffer = "Loading library: " // ol_library
    call msg_message (char(msg_buffer))
    if (os_file_exist (ol_library)) then
       call dlaccess_init (dlaccess, var_str (""), ol_library, os_data)
    else
       call msg_fatal ("Link OpenLoops: library not found")
    end if
    success = .not. dlaccess_has_error (dlaccess)
  end subroutine openloops_driver_init_dlaccess_to_library

  subroutine openloops_driver_set_alpha_s (driver, alpha_s)
    class(openloops_driver_t), intent(in) :: driver
    real(default), intent(in) :: alpha_s
    integer :: ierr
    if (associated (driver%blha_olp_set_parameter)) then
       call driver%blha_olp_set_parameter &
            (c_char_'alphas'//c_null_char, &
             dble (alpha_s), 0._double, ierr)
    else
       call msg_fatal ("blha_olp_set_parameter not associated!")
    end if
    if (ierr == 0) call parameter_error_message (var_str ('alphas'))
  end subroutine openloops_driver_set_alpha_s

  subroutine openloops_driver_set_alpha_qed (driver, alpha)
    class(openloops_driver_t), intent(inout) :: driver
    real(default), intent(in) :: alpha
    integer :: ierr
    call driver%blha_olp_set_parameter &
       (c_char_'alpha_qed'//c_null_char, &
        dble (alpha), 0._double, ierr)
    if (ierr == 0) call parameter_error_message (var_str ('alpha_qed'))
  end subroutine openloops_driver_set_alpha_qed

  subroutine openloops_driver_set_GF (driver, GF)
    class(openloops_driver_t), intent(inout) :: driver
    real(default), intent(in) :: GF
    integer :: ierr
    call driver%blha_olp_set_parameter &
       (c_char_'Gmu'//c_null_char, &
        dble(GF), 0._double, ierr)
    if (ierr == 0) call parameter_error_message (var_str ('Gmu'))
  end subroutine openloops_driver_set_GF

  subroutine openloops_driver_set_weinberg_angle (driver, sw2)
    class(openloops_driver_t), intent(inout) :: driver
    real(default), intent(in) :: sw2
    integer :: ierr
    call driver%blha_olp_set_parameter &
       (c_char_'sw2'//c_null_char, &
        dble(sw2), 0._double, ierr)
    if (ierr == 0) call parameter_error_message (var_str ('sw2'))
  end subroutine openloops_driver_set_weinberg_angle

  subroutine openloops_driver_print_alpha_s (object)
    class(openloops_driver_t), intent(in) :: object
    call object%blha_olp_print_parameter (c_char_'alphas'//c_null_char)
  end subroutine openloops_driver_print_alpha_s

  function openloops_driver_type_name () result (type)
    type(string_t) :: type
    type = "OpenLoops"
  end function openloops_driver_type_name

  subroutine openloops_driver_load_sc_procedure (object, os_data, success)
    class(openloops_driver_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical, intent(out) :: success
    type(dlaccess_t) :: dlaccess
    type(c_funptr) :: c_fptr
    logical :: init_success

    call object%init_dlaccess_to_library (os_data, dlaccess, init_success)

    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("ol_evaluate_scpowheg"))
    call c_f_procpointer (c_fptr, object%evaluate_spin_correlations_powheg)
    if (dlaccess_has_error (dlaccess)) then
       call msg_fatal ("Could not load Openloops-powheg spin correlations!")
    else
       success = .true.
    end if

  end subroutine openloops_driver_load_sc_procedure

  subroutine openloops_def_read (object, unit)
    class(openloops_def_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine openloops_def_read

  subroutine openloops_def_allocate_driver (object, driver, basename)
    class(openloops_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    if (.not. allocated (driver)) allocate (openloops_driver_t :: driver)
  end subroutine openloops_def_allocate_driver

  subroutine openloops_state_write (object, unit)
    class(openloops_state_t), intent(in) :: object
    integer, intent(in), optional :: unit
  end subroutine openloops_state_write

  subroutine prc_openloops_allocate_workspace (object, core_state)
    class(prc_openloops_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    allocate (openloops_state_t :: core_state)
  end subroutine prc_openloops_allocate_workspace

  subroutine prc_openloops_init_driver (object, os_data)
    class(prc_openloops_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: olp_file, olc_file
    type(string_t) :: suffix

    select type (def => object%def)
    type is (openloops_def_t)
       suffix = def%suffix
       olp_file = def%basename // suffix // '.olp'
       olc_file = def%basename // suffix // '.olc'
    class default
       call msg_bug ("prc_openloops_init_driver: core_def should be openloops-type")
    end select

    select type (driver => object%driver)
    type is (openloops_driver_t)
       driver%olp_file = olp_file
       driver%contract_file = olc_file
       driver%nlo_suffix = suffix
    end select
  end subroutine prc_openloops_init_driver

  subroutine prc_openloops_write (object, unit)
    class(prc_openloops_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_message (unit = unit, string = "OpenLoops")
  end subroutine prc_openloops_write

  subroutine prc_openloops_write_name (object, unit)
    class(prc_openloops_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,"(1x,A)") "Core: OpenLoops"
  end subroutine prc_openloops_write_name

  subroutine prc_openloops_prepare_library (object, os_data, model)
    class(prc_openloops_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    type(model_data_t), intent(in), target :: model
    call object%load_driver (os_data)
    call object%reset_parameters ()
    call object%set_particle_properties (model)
    call object%set_electroweak_parameters (model)
    select type(def => object%def)
    type is (openloops_def_t)
       call object%set_verbosity (def%verbosity)
    end select
  end subroutine prc_openloops_prepare_library

  subroutine prc_openloops_load_driver (object, os_data)
    class(prc_openloops_t), intent(inout) :: object
    type(os_data_t), intent(in) :: os_data
    logical :: success
    select type (driver => object%driver)
    type is (openloops_driver_t)
       call driver%load (os_data, success)
       call driver%load_sc_procedure (os_data, success)
    end select
  end subroutine prc_openloops_load_driver

  subroutine prc_openloops_start (object)
    class(prc_openloops_t), intent(inout) :: object
    integer :: ierr
    select type (driver => object%driver)
    type is (openloops_driver_t)
       call driver%blha_olp_start (char (driver%olp_file)//c_null_char, ierr)
    end select
  end subroutine prc_openloops_start

  subroutine prc_openloops_set_n_external (object, n)
    class(prc_openloops_t), intent(inout) :: object
    integer, intent(in) :: n
    N_EXTERNAL = n
  end subroutine prc_openloops_set_n_external

  subroutine prc_openloops_reset_parameters (object)
    class(prc_openloops_t), intent(inout) :: object
    integer :: ierr
    select type (driver => object%driver)
    type is (openloops_driver_t)
       call driver%blha_olp_set_parameter ('mass(5)'//c_null_char, &
            dble(openloops_default_bmass), 0._double, ierr)
       call driver%blha_olp_set_parameter ('mass(6)'//c_null_char, &
            dble(openloops_default_topmass), 0._double, ierr)
       call driver%blha_olp_set_parameter ('width(6)'//c_null_char, &
            dble(openloops_default_topwidth), 0._double, ierr)
       call driver%blha_olp_set_parameter ('mass(23)'//c_null_char, &
            dble(openloops_default_zmass), 0._double, ierr)
       call driver%blha_olp_set_parameter ('width(23)'//c_null_char, &
            dble(openloops_default_zwidth), 0._double, ierr)
       call driver%blha_olp_set_parameter ('mass(24)'//c_null_char, &
            dble(openloops_default_wmass), 0._double, ierr)
       call driver%blha_olp_set_parameter ('width(24)'//c_null_char, &
            dble(openloops_default_wwidth), 0._double, ierr)
       call driver%blha_olp_set_parameter ('mass(25)'//c_null_char, &
            dble(openloops_default_higgsmass), 0._double, ierr)
       call driver%blha_olp_set_parameter ('width(25)'//c_null_char, &
            dble(openloops_default_higgswidth), 0._double, ierr)
    end select
  end subroutine prc_openloops_reset_parameters

  subroutine prc_openloops_set_verbosity (object, verbose)
    class(prc_openloops_t), intent(inout) :: object
    integer, intent(in) :: verbose
    integer :: ierr
    select type (driver => object%driver)
    type is (openloops_driver_t)
       call driver%blha_olp_set_parameter ('verbose'//c_null_char, &
            dble(verbose), 0._double, ierr)
    end select
  end subroutine prc_openloops_set_verbosity

  subroutine prc_openloops_create_and_load_extra_libraries &
       (core, flv_states, var_list, os_data, libname, model, i_core, is_nlo)
    class(prc_openloops_t), intent(inout) :: core
    integer, intent(in), dimension(:,:), allocatable :: flv_states
    type(var_list_t), intent(in) :: var_list
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: libname
    type(model_data_t), intent(in), target :: model
    integer, intent(in) :: i_core
    logical, intent(in) :: is_nlo
    core%sqme_tree_pos = 1
    call core%set_n_external (core%data%get_n_tot ())
    call core%prepare_library (os_data, model)
    call core%start ()
    call core%read_contract_file (flv_states)
    call core%print_parameter_file (i_core)
    call core%reset_i_whizard_to_i_olc ()
  end subroutine prc_openloops_create_and_load_extra_libraries

  subroutine prc_openloops_compute_sqme_spin_c (object, &
       i_flv, em, p, ren_scale, sqme_spin_c, bad_point)
    class(prc_openloops_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    integer, intent(in) :: em
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(out), dimension(0:3, 0:3) :: sqme_spin_c
    logical, intent(out) :: bad_point
    real(double), dimension(5*N_EXTERNAL) :: mom
    real(double), dimension(N_EXTERNAL) :: res
    real(double), dimension(16) :: res_munu
    real(default) :: alpha_s
    if (object%i_spin_c(i_flv) > 0) then
       mom = object%create_momentum_array (p)
       sqme_spin_c = zero
       if (vanishes (ren_scale)) call msg_fatal &
            ("prc_openloops_compute_sqme_spin_c: ren_scale vanishes")
       alpha_s = object%qcd%alpha%get (ren_scale)

       select type (driver => object%driver)
       type is (openloops_driver_t)
          call driver%set_alpha_s (alpha_s)
          call driver%evaluate_spin_correlations_powheg &
               (object%i_spin_c(i_flv), mom, em, res, res_munu)
       end select
       sqme_spin_c = reshape (res_munu, (/4,4/))
       bad_point = .false.
       if (object%includes_polarization ()) &
            sqme_spin_c = object%n_hel * sqme_spin_c
    else
       sqme_spin_c = zero
    end if
  end subroutine prc_openloops_compute_sqme_spin_c


end module prc_openloops
