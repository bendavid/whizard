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
module prc_user_defined

  use, intrinsic :: iso_c_binding !NODEP!

  use kinds
  use constants
  use io_units
  use iso_varying_string, string_t => varying_string
  use system_defs, only: TAB
  use physics_defs, only: CF
  use diagnostics
  use os_interface
  use lorentz
  use interactions
  use sm_qcd
  use variables, only: var_list_t

  use model_data
  use prclib_interfaces
  use prc_core_def
  use prc_core
  use prc_omega, only: omega_state_t

  use sf_base
  use sf_pdf_builtin, only: pdf_builtin_t
  use sf_lhapdf, only: lhapdf_t
  use pdg_arrays, only: is_gluon, is_quark

  implicit none
  private

  public :: user_defined_state_t
  public :: user_defined_driver_t
  public :: prc_user_defined_base_t
  public :: user_defined_def_t
  public :: prc_user_defined_writer_t
  public :: user_defined_test_writer_t
  public :: user_defined_test_state_t
  public :: user_defined_test_driver_t
  public :: user_defined_test_def_t
  public :: prc_user_defined_test_t

  integer, parameter :: LEPTONS = 1
  integer, parameter :: HADRONS = 2

  type :: sf_handler_t
     integer :: initial_state_type = 0
     integer :: n_sf = -1
     real(default) :: val = one
  contains
    procedure :: init => sf_handler_init
    procedure :: init_dummy => sf_handler_init_dummy
    procedure :: apply_structure_functions => sf_handler_apply_structure_functions
    procedure :: get_pdf => sf_handler_get_pdf
  end type sf_handler_t

  type, abstract, extends (prc_core_state_t) :: user_defined_state_t
    logical :: new_kinematics = .true.
    real(default) :: alpha_qcd = -1
  contains
    procedure :: reset_new_kinematics => user_defined_state_reset_new_kinematics
    procedure :: get_alpha_qcd => user_defined_state_get_alpha_qcd
  end type user_defined_state_t

  type, abstract, extends (prc_core_driver_t) :: user_defined_driver_t
     procedure(omega_update_alpha_s), nopass, pointer :: &
              update_alpha_s => null ()
     procedure(omega_is_allowed), nopass, pointer :: &
              is_allowed => null ()
  !contains
  !<Prc User: user defined driver: TBP>>
  end type user_defined_driver_t

  type, abstract, extends (prc_core_t) :: prc_user_defined_base_t
    type(qcd_t) :: qcd
    integer :: n_flv = 1
    real(default), dimension(:), allocatable :: par
    integer :: scheme = 0
    type(sf_handler_t) :: sf_handler
    real(default) :: maximum_accuracy = 10000.0
  contains
    procedure :: get_n_flvs => prc_user_defined_base_get_n_flvs
    procedure :: get_flv_state => prc_user_defined_base_get_flv_state
    procedure :: compute_sqme => prc_user_defined_base_compute_sqme
    procedure :: compute_sqme_virt => prc_user_defined_base_compute_sqme_virt
    procedure :: compute_sqme_color_c => prc_user_defined_base_compute_sqme_color_c
    procedure :: compute_alpha_s => prc_user_defined_base_compute_alpha_s
    procedure :: get_alpha_s => prc_user_defined_base_get_alpha_s
    procedure :: is_allowed => prc_user_defined_base_is_allowed
    procedure :: get_nflv => prc_user_defined_base_get_nflv
    procedure :: compute_hard_kinematics => prc_user_defined_base_compute_hard_kinematics
    procedure :: compute_eff_kinematics => prc_user_defined_base_compute_eff_kinematics
    procedure :: set_parameters => prc_user_defined_base_set_parameters
    procedure :: update_alpha_s => prc_user_defined_base_update_alpha_s
    procedure :: init_sf_handler => prc_user_defined_base_init_sf_handler
    procedure :: init_sf_handler_dummy => prc_user_defined_base_init_sf_handler_dummy
    procedure :: apply_structure_functions => prc_user_defined_base_apply_structure_functions
    procedure :: get_sf_value => prc_user_defined_base_get_sf_value
    procedure :: get_i_whizard_to_i_olc_base => prc_user_defined_base_get_i_whizard_to_i_olc
    procedure(prc_user_defined_base_includes_polarization), deferred :: &
      includes_polarization
    procedure(prc_user_defined_base_create_and_load_extra_libraries), &
         deferred :: create_and_load_extra_libraries
  end type prc_user_defined_base_t

  type, abstract, extends (prc_core_def_t) :: user_defined_def_t
    type(string_t) :: basename
  contains
    procedure :: set_active_writer => user_defined_def_set_active_writer
    procedure, nopass :: get_features => user_defined_def_get_features
    procedure :: connect => user_defined_def_connect
    procedure :: omega_connect => user_defined_def_connect
    procedure, nopass :: needs_code => user_def_needs_code
  end type user_defined_def_t

  type, abstract, extends (prc_writer_f_module_t) :: prc_user_defined_writer_t
    type(string_t) :: model_name
    type(string_t) :: process_mode
    type(string_t) :: process_string
    type(string_t) :: restrictions
    integer :: n_in = 0
    integer :: n_out = 0
    logical :: active = .true.
  contains
    procedure :: init => user_defined_writer_init
    procedure :: base_init => user_defined_writer_init
    procedure, nopass :: get_module_name => prc_user_defined_writer_get_module_name
    procedure :: write_wrapper => prc_user_defined_writer_write_wrapper
    procedure :: write_interface => prc_user_defined_writer_write_interface
    procedure :: write_source_code => prc_user_defined_writer_write_source_code
    procedure :: before_compile => prc_user_defined_writer_before_compile
    procedure :: after_compile => prc_user_defined_writer_after_compile
    procedure :: write_makefile_code => prc_user_defined_writer_write_makefile_code
    procedure :: base_write_makefile_code => prc_user_defined_writer_write_makefile_code
    procedure, nopass:: get_procname => prc_user_defined_writer_writer_get_procname
  end type prc_user_defined_writer_t

  type, extends (prc_user_defined_writer_t) :: user_defined_test_writer_t
  contains
    procedure, nopass :: type_name => user_defined_test_writer_type_name
  end type user_defined_test_writer_t

  type, extends (user_defined_state_t) :: user_defined_test_state_t
  contains
    procedure :: write => user_defined_test_state_write
  end type user_defined_test_state_t

  type, extends (user_defined_driver_t) :: user_defined_test_driver_t
  contains
    procedure, nopass :: type_name => user_defined_test_driver_type_name
  end type user_defined_test_driver_t

  type, extends (user_defined_def_t) :: user_defined_test_def_t
  contains
    procedure :: init => user_defined_test_def_init
    procedure, nopass :: type_string => user_defined_test_def_type_string
    procedure :: write => user_defined_test_def_write
    procedure :: read => user_defined_test_def_read
    procedure :: allocate_driver => user_defined_test_def_allocate_driver
  end type user_defined_test_def_t

  type, extends (prc_user_defined_base_t) :: prc_user_defined_test_t
  contains
    procedure :: write => prc_user_defined_test_write
    procedure :: write_name => prc_user_defined_test_write_name
    procedure :: compute_amplitude => prc_user_defined_test_compute_amplitude
    procedure :: allocate_workspace => prc_user_defined_test_allocate_workspace
    procedure :: includes_polarization => prc_user_defined_test_includes_polarization
    procedure :: create_and_load_extra_libraries => &
         prc_user_defined_test_create_and_load_extra_libraries
  end type prc_user_defined_test_t


  abstract interface
    function prc_user_defined_base_includes_polarization (object) result (polarized)
      import
      logical :: polarized
      class(prc_user_defined_base_t), intent(in) :: object
    end function prc_user_defined_base_includes_polarization
  end interface

  abstract interface
    subroutine prc_user_defined_base_create_and_load_extra_libraries &
         (core, flv_states, var_list, os_data, libname, model, i_core, is_nlo)
      import
      class(prc_user_defined_base_t), intent(inout) :: core
      integer, intent(in), dimension(:,:), allocatable :: flv_states
      type(var_list_t), intent(in) :: var_list
      type(os_data_t), intent(in) :: os_data
      type(string_t), intent(in) :: libname
      type(model_data_t), intent(in), target :: model
      integer, intent(in) :: i_core
      logical, intent(in) :: is_nlo
    end subroutine prc_user_defined_base_create_and_load_extra_libraries
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


contains

  subroutine sf_handler_init (sf_handler, sf_chain)
    class(sf_handler_t), intent(out) :: sf_handler
    type(sf_chain_instance_t), intent(in) :: sf_chain
    integer :: i
    sf_handler%n_sf = size (sf_chain%sf)
    if (sf_handler%n_sf == 0) then
       sf_handler%initial_state_type = LEPTONS
    else
       do i = 1, sf_handler%n_sf
          select type (int => sf_chain%sf(i)%int)
          type is (pdf_builtin_t)
             sf_handler%initial_state_type = HADRONS
          type is (lhapdf_t)
             sf_handler%initial_state_type = HADRONS
          class default
             sf_handler%initial_state_type = LEPTONS
          end select
       end do
     end if
  end subroutine sf_handler_init

  subroutine sf_handler_init_dummy (sf_handler)
    class(sf_handler_t), intent(out) :: sf_handler
    sf_handler%n_sf = 0
    sf_handler%initial_state_type = LEPTONS
  end subroutine sf_handler_init_dummy

  subroutine sf_handler_apply_structure_functions (sf_handler, sf_chain, flavors)
     class(sf_handler_t), intent(inout) :: sf_handler
     type(sf_chain_instance_t), intent(in) :: sf_chain
     integer, intent(in), dimension(2) :: flavors
     integer :: i
     real(default), dimension(:), allocatable :: f
     if (sf_handler%n_sf < 0) call msg_fatal ("sf_handler not initialized")
     sf_handler%val = one
     do i = 1, sf_handler%n_sf
        select case (sf_handler%initial_state_type)
        case (HADRONS)
           sf_handler%val = sf_handler%val * sf_handler%get_pdf (sf_chain, i, flavors(i))
        case (LEPTONS)
           call sf_chain%get_matrix_elements (i, f)
           sf_handler%val = sf_handler%val * f(1)
        case default
           call msg_fatal ("sf_handler not initialized")
        end select
     end do
  end subroutine sf_handler_apply_structure_functions

  function sf_handler_get_pdf (sf_handler, sf_chain, i, flavor) result (f)
     real(default) :: f
     class(sf_handler_t), intent(in) :: sf_handler
     type(sf_chain_instance_t), intent(in) :: sf_chain
     integer, intent(in) :: i, flavor
     integer :: k
     real(default), dimension(:), allocatable :: ff
     integer, parameter :: n_flv_light = 6

     call sf_chain%get_matrix_elements (i, ff)

     if (is_gluon (flavor)) then
        k = n_flv_light + 1
     else if (is_quark (abs(flavor))) then
        k = n_flv_light + 1 + flavor
     else
        call msg_fatal ("Not a colored particle")
     end if

     f = ff(k)
   end function sf_handler_get_pdf

  subroutine user_defined_state_reset_new_kinematics (object)
    class(user_defined_state_t), intent(inout) :: object
    object%new_kinematics = .true.
  end subroutine user_defined_state_reset_new_kinematics

  function user_defined_state_get_alpha_qcd (object) result (alpha_qcd)
    real(default) :: alpha_qcd
    class(user_defined_state_t), intent(in) :: object
    alpha_qcd = object%alpha_qcd
  end function user_defined_state_get_alpha_qcd

  pure function prc_user_defined_base_get_n_flvs (object, i_flv) result (n)
    integer :: n
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_flv
    n = size (object%data%flv_state (:,i_flv))
  end function prc_user_defined_base_get_n_flvs

  function prc_user_defined_base_get_flv_state (object, i_flv) result (flv)
    integer, dimension(:), allocatable :: flv
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_flv
    allocate (flv (size (object%data%flv_state (:,i_flv))))
    flv = object%data%flv_state (:,i_flv)
  end function prc_user_defined_base_get_flv_state

  subroutine prc_user_defined_base_compute_sqme (object, i_flv, p, &
         ren_scale, sqme, bad_point)
     class(prc_user_defined_base_t), intent(in) :: object
     integer, intent(in) :: i_flv
     type(vector4_t), dimension(:), intent(in) :: p
     real(default), intent(in) :: ren_scale
     real(default), intent(out) :: sqme
     logical, intent(out) :: bad_point
     sqme = one
     bad_point = .false.
  end subroutine prc_user_defined_base_compute_sqme

  subroutine prc_user_defined_base_compute_sqme_virt (object, i_flv, &
     p, ren_scale, sqme, bad_point)
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    logical, intent(out) :: bad_point
    real(default), dimension(4), intent(out) :: sqme
    call msg_debug2 (D_ME_METHODS, "prc_user_defined_base_compute_sqme_virt")
    sqme(1) = 0.001_default
    sqme(2) = 0.001_default
    sqme(3) = 0.001_default
    sqme(4) = 0.0015_default
    bad_point = .false.
  end subroutine prc_user_defined_base_compute_sqme_virt

  subroutine prc_user_defined_base_compute_sqme_color_c (object, i_flv, p, &
     ren_scale, born_color_c, bad_point, born_out)
    class(prc_user_defined_base_t), intent(inout) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    real(default), intent(inout), dimension(:,:) :: born_color_c
    logical, intent(out) :: bad_point
    real(default), intent(out), optional :: born_out
    call msg_debug2 (D_ME_METHODS, "prc_user_defined_base_compute_sqme_color_c")
    if (size (p) == 4) then
       if (present (born_out)) then
          born_out = 0.0015_default
          born_color_c = zero
          born_color_c(3,3) = - CF * born_out
          born_color_c(4,4) = - CF * born_out
          born_color_c(3,4) = CF * born_out
          born_color_c(4,3) = born_color_c(3,4)
          bad_point = .false.
       end if
    else
       if (present (born_out)) born_out = zero
       born_color_c = zero
    end if
  end subroutine prc_user_defined_base_compute_sqme_color_c

  subroutine prc_user_defined_base_compute_alpha_s &
       (object, core_state, ren_scale)
    class(prc_user_defined_base_t), intent(in) :: object
    class(user_defined_state_t), intent(inout) :: core_state
    real(default), intent(in) :: ren_scale
    core_state%alpha_qcd = object%qcd%alpha%get (ren_scale)
  end subroutine prc_user_defined_base_compute_alpha_s

  function prc_user_defined_base_get_alpha_s (object, core_state) result (alpha)
    class(prc_user_defined_base_t), intent(in) :: object
    class(prc_core_state_t), intent(in), allocatable :: core_state
    real(default) :: alpha
    if (allocated (core_state)) then
      select type (core_state)
      class is (user_defined_state_t)
         alpha = core_state%alpha_qcd
      type is (omega_state_t)
         alpha = core_state%alpha_qcd
      class default
         alpha = zero
      end select
    else
       alpha = zero
    end if
  end function prc_user_defined_base_get_alpha_s

  function prc_user_defined_base_is_allowed (object, i_term, f, h, c) result (flag)
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_term, f, h, c
    logical :: flag
    logical(c_bool) :: cflag
    select type (driver => object%driver)
    class is (user_defined_driver_t)
       call driver%is_allowed (f, h, c, cflag)
       flag = cflag
    class default
       call msg_fatal &
          ("Driver does not fit to user_defined_base_t")
    end select
  end function prc_user_defined_base_is_allowed

  function prc_user_defined_base_get_nflv (object) result (n_flv)
    class(prc_user_defined_base_t), intent(in) :: object
    integer :: n_flv
    n_flv = object%n_flv
  end function prc_user_defined_base_get_nflv

  subroutine prc_user_defined_base_compute_hard_kinematics &
       (object, p_seed, i_term, int_hard, core_state)
    class(prc_user_defined_base_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(in) :: p_seed
    integer, intent(in) :: i_term
    type(interaction_t), intent(inout) :: int_hard
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    call int_hard%set_momenta (p_seed)
    if (allocated (core_state)) then
      select type (core_state)
      class is (user_defined_state_t); core_state%new_kinematics = .true.
      end select
    end if
  end subroutine prc_user_defined_base_compute_hard_kinematics

  subroutine prc_user_defined_base_compute_eff_kinematics &
       (object, i_term, int_hard, int_eff, core_state)
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i_term
    type(interaction_t), intent(in) :: int_hard
    type(interaction_t), intent(inout) :: int_eff
    class(prc_core_state_t), intent(inout), allocatable :: core_state
  end subroutine prc_user_defined_base_compute_eff_kinematics

  subroutine prc_user_defined_base_set_parameters (object, qcd, model)
    class(prc_user_defined_base_t), intent(inout) :: object
    type(qcd_t), intent(in) :: qcd
    class(model_data_t), intent(in), target, optional :: model
    object%qcd = qcd
    if (present (model)) then
       if (.not. allocated (object%par)) &
            allocate (object%par (model%get_n_real ()))
       call model%real_parameters_to_array (object%par)
       object%scheme = model%get_scheme_num ()
    end if
  end subroutine prc_user_defined_base_set_parameters

  subroutine prc_user_defined_base_update_alpha_s (object, core_state, fac_scale)
    class(prc_user_defined_base_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    real(default), intent(in) :: fac_scale
    real(default) :: alpha_qcd
    if (allocated (object%qcd%alpha)) then
       alpha_qcd = object%qcd%alpha%get (fac_scale)
       select type (driver => object%driver)
       class is (user_defined_driver_t)
          call driver%update_alpha_s (alpha_qcd)
       end select
       select type (core_state)
       class is (user_defined_state_t)
          core_state%alpha_qcd = alpha_qcd
       type is (omega_state_t)
          core_state%alpha_qcd = alpha_qcd
       end select
    end if
  end subroutine prc_user_defined_base_update_alpha_s

  subroutine prc_user_defined_base_init_sf_handler (core, sf_chain)
     class(prc_user_defined_base_t), intent(inout) :: core
     type(sf_chain_instance_t), intent(in) :: sf_chain
     if (allocated (sf_chain%sf)) then
        call core%sf_handler%init (sf_chain)
     else
        call core%sf_handler%init_dummy ()
     end if
  end subroutine prc_user_defined_base_init_sf_handler

  subroutine prc_user_defined_base_init_sf_handler_dummy (core)
     class(prc_user_defined_base_t), intent(inout) :: core
     call core%sf_handler%init_dummy ()
  end subroutine prc_user_defined_base_init_sf_handler_dummy

  subroutine prc_user_defined_base_apply_structure_functions (core, sf_chain, flavors)
    class(prc_user_defined_base_t), intent(inout) :: core
    type(sf_chain_instance_t), intent(in) :: sf_chain
    integer, dimension(2), intent(in) :: flavors
    call core%sf_handler%apply_structure_functions (sf_chain, flavors)
  end subroutine prc_user_defined_base_apply_structure_functions

  function prc_user_defined_base_get_sf_value (core) result (val)
    real(default) :: val
    class(prc_user_defined_base_t), intent(in) :: core
    val = core%sf_handler%val
  end function prc_user_defined_base_get_sf_value

  function prc_user_defined_base_get_i_whizard_to_i_olc (object, i) result (i_out)
    integer :: i_out
    class(prc_user_defined_base_t), intent(in) :: object
    integer, intent(in) :: i
    i_out = i
  end function prc_user_defined_base_get_i_whizard_to_i_olc

  subroutine user_defined_def_set_active_writer (def, active)
    class(user_defined_def_t), intent(inout) :: def
    logical, intent(in) :: active
    select type (writer => def%writer)
    class is (prc_user_defined_writer_t)
       writer%active = active
    end select
  end subroutine user_defined_def_set_active_writer

  subroutine user_defined_def_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (6))
    features = [ &
         var_str ("init"), &
         var_str ("update_alpha_s"), &
         var_str ("reset_helicity_selection"), &
         var_str ("is_allowed"), &
         var_str ("new_event"), &
         var_str ("get_amplitude")]
  end subroutine user_defined_def_get_features

  subroutine user_defined_def_connect (def, lib_driver, i, proc_driver)
    class(user_defined_def_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
    integer :: pid, fid
    type(c_funptr) :: fptr
    select type (proc_driver)
    class is (user_defined_driver_t)
       pid = i
       fid = 2
       call lib_driver%get_fptr (pid, fid, fptr)
       call c_f_procpointer (fptr, proc_driver%update_alpha_s)
       fid = 4
       call lib_driver%get_fptr (pid, fid, fptr)
       call c_f_procpointer (fptr, proc_driver%is_allowed)
    end select
  end subroutine user_defined_def_connect

  function user_def_needs_code () result (flag)
    logical :: flag
    flag = .true.
  end function user_def_needs_code

  pure subroutine user_defined_writer_init &
       (writer, model_name, prt_in, prt_out, restrictions)
    class(prc_user_defined_writer_t), intent(inout) :: writer
    type(string_t), intent(in) :: model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    type(string_t), intent(in), optional :: restrictions
    integer :: i
    writer%model_name = model_name
    if (present (restrictions)) then
       writer%restrictions = restrictions
    else
       writer%restrictions = ""
    end if
    writer%n_in = size (prt_in)
    writer%n_out = size (prt_out)
    select case (size (prt_in))
       case(1); writer%process_mode = " -decay"
       case(2); writer%process_mode = " -scatter"
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
  end subroutine user_defined_writer_init

  function prc_user_defined_writer_get_module_name (id) result (name)
    type(string_t) :: name
    type(string_t), intent(in) :: id
    name = "opr_" // id
  end function prc_user_defined_writer_get_module_name

  subroutine prc_user_defined_writer_write_wrapper (writer, unit, id, feature)
    class(prc_user_defined_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id, feature
    type(string_t) :: name
    name = writer%get_c_procname (id, feature)
    write (unit, *)
    select case (char (feature))
    case ("init")
       write (unit, "(9A)")  "subroutine ", char (name), &
            " (par, scheme) bind(C)"
       write (unit, "(2x,9A)")  "use iso_c_binding"
       write (unit, "(2x,9A)")  "use kinds"
       write (unit, "(2x,9A)")  "use opr_", char (id)
       write (unit, "(2x,9A)")  "real(c_default_float), dimension(*), &
            &intent(in) :: par"
       write (unit, "(2x,9A)")  "integer(c_int), intent(in) :: scheme"
       if (c_default_float == default .and. c_int == kind (1)) then
          write (unit, "(2x,9A)")  "call ", char (feature), " (par, scheme)"
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

  end subroutine prc_user_defined_writer_write_wrapper

  subroutine prc_user_defined_writer_write_interface (writer, unit, id, feature)
    class(prc_user_defined_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(string_t), intent(in) :: feature
    type(string_t) :: name
    name = writer%get_c_procname (id, feature)
    write (unit, "(2x,9A)")  "interface"
    select case (char (feature))
    case ("init")
       write (unit, "(5x,9A)")  "subroutine ", char (name), &
            " (par, scheme) bind(C)"
       write (unit, "(7x,9A)")  "import"
       write (unit, "(7x,9A)")  "real(c_default_float), dimension(*), &
            &intent(in) :: par"
       write (unit, "(7x,9A)")  "integer(c_int), intent(in) :: scheme"
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
  end subroutine prc_user_defined_writer_write_interface

  subroutine prc_user_defined_writer_write_source_code (writer, id)
    class(prc_user_defined_writer_t), intent(in) :: writer
    type(string_t), intent(in) :: id
    call msg_debug (D_ME_METHODS, &
         "prc_user_defined_writer_write_source_code (no-op)")
    !!! This is a dummy
  end subroutine prc_user_defined_writer_write_source_code

  subroutine prc_user_defined_writer_before_compile (writer, id)
    class(prc_user_defined_writer_t), intent(in) :: writer
    type(string_t), intent(in) :: id
    call msg_debug (D_ME_METHODS, &
         "prc_user_defined_writer_before_compile (no-op)")
    !!! This is a dummy
  end subroutine prc_user_defined_writer_before_compile
  
  subroutine prc_user_defined_writer_after_compile (writer, id)
    class(prc_user_defined_writer_t), intent(in) :: writer
    type(string_t), intent(in) :: id
    call msg_debug (D_ME_METHODS, &
         "prc_user_defined_writer_after_compile (no-op)")
    !!! This is a dummy
  end subroutine prc_user_defined_writer_after_compile
  
  subroutine prc_user_defined_writer_write_makefile_code &
       (writer, unit, id, os_data, verbose, testflag)
    class(prc_user_defined_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: verbose
    logical, intent(in), optional :: testflag
    type(string_t) :: omega_binary, omega_path
    type(string_t) :: restrictions_string
    omega_binary = "omega_" // writer%model_name // ".opt"
    omega_path = os_data%whizard_omega_binpath // "/" // omega_binary
    if (.not. verbose)  omega_path = "@" // omega_path
    if (writer%restrictions /= "") then
       restrictions_string = " -cascade '" // writer%restrictions // "'"
    else
       restrictions_string = ""
    end if
    write (unit, "(5A)")  "OBJECTS += ", char (id), ".lo"
    write (unit, "(5A)")  char (id), ".f90:"
    if (.not. verbose) then
       write (unit, "(5A)")  TAB // '@echo  "  OMEGA     ', trim (char (id)), '.f90"'
    end if
    write (unit, "(99A)")  TAB, char (omega_path), &
         " -o ", char (id), ".f90", &
         " -target:whizard", &
         " -target:parameter_module parameters_", char (writer%model_name), &
         " -target:module opr_", char (id), &
         " -target:md5sum '", writer%md5sum, "'", &
         char (writer%process_mode), char (writer%process_string), &
         char (restrictions_string)
    write (unit, "(5A)")  "clean-", char (id), ":"
    write (unit, "(5A)")  TAB, "rm -f ", char (id), ".f90"
    write (unit, "(5A)")  TAB, "rm -f opr_", char (id), ".mod"
    write (unit, "(5A)")  TAB, "rm -f ", char (id), ".lo"
    write (unit, "(5A)")  "CLEAN_SOURCES += ", char (id), ".f90"
    write (unit, "(5A)")  "CLEAN_OBJECTS += opr_", char (id), ".mod"
    write (unit, "(5A)")  "CLEAN_OBJECTS += ", char (id), ".lo"
    write (unit, "(5A)")  char (id), ".lo: ", char (id), ".f90"
    if (.not. verbose) then
       write (unit, "(5A)")  TAB // '@echo  "  FC       " $@'
    end if    
    write (unit, "(5A)")  TAB, "$(LTFCOMPILE) $<"

  end subroutine prc_user_defined_writer_write_makefile_code

  function prc_user_defined_writer_writer_get_procname (feature) result (name)
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
  end function prc_user_defined_writer_writer_get_procname

  function user_defined_test_writer_type_name () result (string)
    type(string_t) :: string
    string = "User-defined dummy"
  end function user_defined_test_writer_type_name

  subroutine user_defined_test_state_write (object, unit)
    class(user_defined_test_state_t), intent(in) :: object
    integer, intent(in), optional :: unit
  end subroutine user_defined_test_state_write

  function user_defined_test_driver_type_name () result (type)
    type(string_t) :: type
    type = "User-defined dummy"
  end function user_defined_test_driver_type_name

  subroutine user_defined_test_def_init (object, basename, model_name, &
       prt_in, prt_out)
    class(user_defined_test_def_t), intent(inout) :: object
    type(string_t), intent(in) :: basename, model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    object%basename = basename
    allocate (user_defined_test_writer_t :: object%writer)
    select type (writer => object%writer)
    type is (user_defined_test_writer_t)
       call writer%init (model_name, prt_in, prt_out)
    end select
  end subroutine user_defined_test_def_init

  function user_defined_test_def_type_string () result (string)
    type(string_t) :: string
    string = "user test dummy"
  end function user_defined_test_def_type_string

  subroutine user_defined_test_def_write (object, unit)
    class(user_defined_test_def_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine user_defined_test_def_write

  subroutine user_defined_test_def_read (object, unit)
    class(user_defined_test_def_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine user_defined_test_def_read

  subroutine user_defined_test_def_allocate_driver (object, driver, basename)
    class(user_defined_test_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    if (.not. allocated (driver)) allocate (user_defined_test_driver_t :: driver)
  end subroutine user_defined_test_def_allocate_driver

  subroutine prc_user_defined_test_write (object, unit)
    class(prc_user_defined_test_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call msg_message ("Test user-defined matrix elements")
  end subroutine prc_user_defined_test_write

  subroutine prc_user_defined_test_write_name (object, unit)
    class(prc_user_defined_test_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,"(1x,A)") "Core: user defined test"
  end subroutine prc_user_defined_test_write_name

  function prc_user_defined_test_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
       core_state)  result (amp)
    class(prc_user_defined_test_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: core_state
    complex(default) :: amp
    select type (core_state)
    class is (user_defined_test_state_t)
       core_state%alpha_qcd = object%qcd%alpha%get (fac_scale)
    end select
    amp = 0.0
  end function prc_user_defined_test_compute_amplitude

  subroutine prc_user_defined_test_allocate_workspace (object, core_state)
    class(prc_user_defined_test_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    allocate (user_defined_test_state_t :: core_state)
  end subroutine prc_user_defined_test_allocate_workspace

  function prc_user_defined_test_includes_polarization (object) result (polarized)
    logical :: polarized
    class(prc_user_defined_test_t), intent(in) :: object
    polarized = .false.
  end function prc_user_defined_test_includes_polarization

  subroutine prc_user_defined_test_create_and_load_extra_libraries &
       (core, flv_states, var_list, os_data, libname, model, i_core, is_nlo)
    class(prc_user_defined_test_t), intent(inout) :: core
    integer, intent(in), dimension(:,:), allocatable :: flv_states
    type(var_list_t), intent(in) :: var_list
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: libname
    type(model_data_t), intent(in), target :: model
    integer, intent(in) :: i_core
    logical, intent(in) :: is_nlo
  end subroutine prc_user_defined_test_create_and_load_extra_libraries


end module prc_user_defined
