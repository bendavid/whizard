! WHIZARD 2.6.1 Nov 03 2017
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

module prc_recola

  use recola_wrapper !NODEP!

  use kinds
  use constants, only: pi, zero
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use io_units
  use lorentz
  use physics_defs
  use variables, only: var_list_t

  use os_interface, only: os_data_t
  use sm_qcd, only: qcd_t
  use model_data, only: model_data_t

  use prc_core, only: prc_core_state_t
  use prc_core_def, only: prc_core_driver_t, prc_core_def_t
  use prc_user_defined
  use process_libraries, only: process_library_t

  implicit none
  private

  public :: abort_if_recola_not_active
  public :: prc_recola_t
  public :: recola_def_t



  type, extends (prc_user_defined_base_t) :: prc_recola_t
     integer :: recola_id = 0
     integer, dimension(:,:), allocatable :: color_state
     integer :: alpha_power = 0
     integer :: alphas_power = 0
     integer :: n_f = 0
     logical :: nlo_computation = .false.
  contains
    procedure :: set_coupling_powers => prc_recola_set_coupling_powers
    procedure :: compute_alpha_s => prc_recola_compute_alpha_s
    procedure :: allocate_workspace => prc_recola_allocate_workspace
    procedure :: includes_polarization => prc_recola_includes_polarization
    procedure :: write_name => prc_recola_write_name
    procedure :: create_and_load_extra_libraries => &
         prc_recola_create_and_load_extra_libraries
    procedure :: replace_helicity_and_color_arrays => &
         prc_recola_replace_helicity_and_color_arrays
    procedure :: enable_dynamic_settings => prc_recola_enable_dynamic_settings
    procedure :: register_processes => prc_recola_register_processes
    procedure :: compute_amplitude => prc_recola_compute_amplitude
    procedure :: write => prc_recola_write
    procedure :: compute_sqme => prc_recola_compute_sqme
    procedure :: compute_sqme_virt => prc_recola_compute_sqme_virt
    procedure :: set_parameters => prc_recola_set_parameters
    procedure :: set_mu_ir => prc_recola_set_mu_ir
    procedure :: has_matrix_element => prc_recola_has_matrix_element
  end type prc_recola_t

  type, extends (prc_user_defined_writer_t) :: recola_writer_t
  contains
    procedure, nopass :: type_name => recola_writer_type_name
  end type recola_writer_t

  type, extends (user_defined_state_t) :: recola_state_t
  contains
    procedure :: write => recola_state_write
  end type recola_state_t

  type, extends (user_defined_def_t) :: recola_def_t
    type(string_t) :: suffix
  contains
    procedure :: init => recola_def_init
    procedure :: allocate_driver => recola_def_allocate_driver
    procedure :: read => recola_def_read
    procedure, nopass :: type_string => recola_def_type_string
    procedure :: write => recola_def_write
  end type recola_def_t

  type, extends (user_defined_driver_t) :: recola_driver_t
  contains
    procedure, nopass :: type_name => recola_driver_type_name
  end type recola_driver_t




contains

  subroutine abort_if_recola_not_active ()
    if (.not. rclwrap_is_active) call msg_fatal ("You want to use Recola, ", &
       [var_str("but either the compiler with which Whizard has been build "), &
        var_str("is not supported by it, or you have not linked Recola "), &
        var_str("correctly to Whizard. Either reconfigure Whizard with a path to "), &
        var_str("a valid Recola installation (for details consult the manual), "), &
        var_str("or choose a different matrix-element method.")])
  end subroutine abort_if_recola_not_active

  subroutine recola_state_write (object, unit)
    class(recola_state_t), intent(in) :: object
    integer, intent(in), optional :: unit
  end subroutine recola_state_write

  function recola_writer_type_name () result (string)
    type(string_t) :: string
    string = "recola"
  end function recola_writer_type_name

  subroutine prc_recola_set_coupling_powers (object, alpha_power, alphas_power)
    class(prc_recola_t), intent(inout) :: object
    integer, intent(in) :: alpha_power, alphas_power
    call msg_debug2 (D_ME_METHODS, "prc_recola_set_coupling_powers")
    call msg_debug2 (D_ME_METHODS, "alphas_power", alphas_power)
    call msg_debug2 (D_ME_METHODS, "alpha_power", alpha_power)
    object%alphas_power = alphas_power
    object%alpha_power = alpha_power
  end subroutine prc_recola_set_coupling_powers

  subroutine prc_recola_compute_alpha_s (object, core_state, ren_scale)
    class(prc_recola_t), intent(in) :: object
    class(user_defined_state_t), intent(inout) :: core_state
    real(default), intent(in) :: ren_scale
    core_state%alpha_qcd = object%qcd%alpha%get (ren_scale)
  end subroutine prc_recola_compute_alpha_s

  subroutine prc_recola_allocate_workspace (object, core_state)
    class(prc_recola_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    allocate (recola_state_t :: core_state)
  end subroutine prc_recola_allocate_workspace

  function prc_recola_includes_polarization (object) result (polarized)
    logical :: polarized
    class(prc_recola_t), intent(in) :: object
    polarized = .false.
  end function prc_recola_includes_polarization

  subroutine prc_recola_write_name (object, unit)
    class(prc_recola_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,"(1x,A)") "Core: Recola"
  end subroutine prc_recola_write_name

  subroutine prc_recola_create_and_load_extra_libraries &
       (core, flv_states, var_list, os_data, libname, model, i_core, is_nlo)
     class(prc_recola_t), intent(inout) :: core
     integer, intent(in), dimension(:,:), allocatable :: flv_states
     type(var_list_t), intent(in) :: var_list
     type(os_data_t), intent(in) :: os_data
     type(string_t), intent(in) :: libname
     type(model_data_t), intent(in), target :: model
     integer, intent(in) :: i_core
     logical, intent(in) :: is_nlo
     integer :: i_recola = 0, alpha_power, alphas_power
     core%nlo_computation = is_nlo
     core%n_f = var_list%get_ival (var_str ("alphas_nf"))
     alpha_power = var_list%get_ival (var_str ("alpha_power"))
     alphas_power = var_list%get_ival (var_str ("alphas_power"))
     call core%set_coupling_powers (alpha_power, alphas_power)
     call core%enable_dynamic_settings ()
     call core%register_processes (i_recola)
     call core%replace_helicity_and_color_arrays ()
  end subroutine prc_recola_create_and_load_extra_libraries

  subroutine prc_recola_replace_helicity_and_color_arrays (object)
    class(prc_recola_t), intent(inout) :: object
    integer, dimension(:,:), allocatable :: col_recola
    integer :: i
    deallocate (object%data%hel_state)
    call rclwrap_get_helicity_configurations &
         (object%recola_id, object%data%hel_state)
    call rclwrap_get_color_configurations (object%recola_id, col_recola)
    allocate (object%color_state (object%data%n_in + object%data%n_out, &
           size (col_recola, dim = 2)))
    do i = 1, size (col_recola, dim = 2)
       object%color_state (:, i) = col_recola (:, i)
    end do
    object%data%n_hel = size (object%data%hel_state, dim = 1)
  end subroutine prc_recola_replace_helicity_and_color_arrays

  subroutine prc_recola_enable_dynamic_settings (object)
    class(prc_recola_t), intent(inout) :: object
    call rclwrap_set_dynamic_settings ()
  end subroutine prc_recola_enable_dynamic_settings

  subroutine prc_recola_register_processes (object, recola_id)
    class(prc_recola_t), intent(inout) :: object
    integer, intent(inout) :: recola_id
    type(string_t), dimension(:), allocatable :: particle_names
    type(string_t) :: process_string
    integer :: i_flv, i_part
    integer :: n_tot
    !!! TODO (cw-2016-08-08): Include helicities
    call msg_debug (D_ME_METHODS, "RECOLA: register process")
    n_tot = object%data%n_in + object%data%n_out
    allocate (particle_names (n_tot))
    do i_flv = 1, object%data%n_flv
       recola_id = recola_id + 1
       object%recola_id = recola_id
       particle_names = &
            get_recola_particle_string (object%data%flv_state (:, i_flv))
       process_string = var_str ("")
       do i_part = 1, n_tot
          if (debug2_active (D_ME_METHODS)) &
               print *, "Appending particle: ", char (particle_names(i_part))
          process_string = process_string // &
               particle_names (i_part) // var_str (" ")
          if (i_part == object%data%n_in) &
             process_string = process_string // var_str ("-> ")
       end do
       if (debug_active (D_ME_METHODS)) then
          print *, "RECOLA process_id: ", object%recola_id
          print *, "NLO computation  : ", object%nlo_computation
       end if
       if (object%nlo_computation) then
          call rclwrap_define_process (object%recola_id, process_string, 'NLO')
       else
          call rclwrap_define_process (object%recola_id, process_string, 'LO')
       end if
       call msg_debug (D_ME_METHODS, "RECOLA: generating process")
    end do
    call rclwrap_generate_processes ()
    call msg_debug (D_ME_METHODS, "RECOLA: generating process successfull")
  end subroutine prc_recola_register_processes

  function prc_recola_compute_amplitude &
     (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
     core_state) result (amp)
    complex(default) :: amp
    class(prc_recola_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: &
         core_state
    real(double), dimension(0:3, object%data%n_in + object%data%n_out) :: &
         p_recola
    integer :: i
    logical :: new_event
    complex(double) :: amp_dble

    if (present (core_state)) then
       if (allocated (core_state)) then
          select type (core_state)
          type is (recola_state_t)
             new_event = core_state%new_kinematics
             core_state%new_kinematics = .false.
          end select
       end if
    end if
    if (new_event) then
       do i = 1, object%data%n_in + object%data%n_out
          p_recola(:, i) = dble(p(i)%p)
       end do
       call rclwrap_compute_process (object%recola_id, p_recola, 'LO')
    end if
    call rclwrap_get_amplitude (object%recola_id, 0, 'LO', &
         object%color_state (:, c), object%data%hel_state (h, :), amp_dble)
    amp = amp_dble
  end function prc_recola_compute_amplitude

  subroutine prc_recola_write (object, unit)
    class(prc_recola_t), intent(in) :: object
    integer, intent(in), optional :: unit
  end subroutine prc_recola_write

  subroutine prc_recola_compute_sqme (object, i_flv, p, &
         ren_scale, sqme, bad_point)
     class(prc_recola_t), intent(in) :: object
     integer, intent(in) :: i_flv
     type(vector4_t), dimension(:), intent(in) :: p
     real(default), intent(in) :: ren_scale
     real(default), intent(out) :: sqme
     real(double) :: sqme_dble
     real(double), dimension(0:3, object%data%n_in + object%data%n_out) :: &
          p_recola
     real(default) :: alpha_s
     integer :: i
     logical, intent(out) :: bad_point
     call msg_debug2 (D_ME_METHODS, "prc_recola_compute_sqme")
     do i = 1, object%data%n_in + object%data%n_out
        p_recola(:, i) = dble(p(i)%p)
     end do
     call rclwrap_set_mu_ir (dble (ren_scale))
     alpha_s = object%qcd%alpha%get (ren_scale)
     call msg_debug2 (D_ME_METHODS, "alpha_s = ", alpha_s)
     call msg_debug2 (D_ME_METHODS, "ren_scale = ", ren_scale)
     call rclwrap_set_alpha_s (dble (alpha_s), dble (ren_scale), object%n_f)
     call rclwrap_compute_process (object%recola_id, p_recola, 'LO')
     call rclwrap_get_squared_amplitude &
          (object%recola_id, object%alphas_power, 'LO', sqme_dble)
     sqme = sqme_dble
     bad_point = .false.
  end subroutine prc_recola_compute_sqme

  subroutine prc_recola_compute_sqme_virt (object, i_flv, &
          p, ren_scale, sqme, bad_point)
    class(prc_recola_t), intent(in) :: object
    integer, intent(in) :: i_flv
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    real(default), dimension(4), intent(out) :: sqme
    real(default) :: amp
    logical, intent(out) :: bad_point
    real(double), dimension(0:3, object%data%n_in + object%data%n_out) :: &
         p_recola
    real(double) :: sqme_dble
    integer :: i
    real(default) :: alpha_s

    sqme = zero
    do i = 1, object%data%n_in + object%data%n_out
       p_recola(:, i) = dble(p(i)%p)
    end do
    call rclwrap_set_mu_ir (dble (ren_scale))
    alpha_s = object%qcd%alpha%get (ren_scale)
    call rclwrap_set_alpha_s (dble (alpha_s), dble (ren_scale), object%n_f)
    call rclwrap_compute_process (object%recola_id, p_recola, 'NLO')
    call rclwrap_get_squared_amplitude &
         (object%recola_id, object%alphas_power + 1, 'NLO', sqme_dble)
    sqme(3) = sqme_dble
    call rclwrap_get_squared_amplitude &
         (object%recola_id, object%alphas_power, 'LO', sqme_dble)
    sqme(4) = sqme_dble

    bad_point = .false.
  end subroutine prc_recola_compute_sqme_virt

  subroutine prc_recola_set_parameters (object, qcd, model)
    class(prc_recola_t), intent(inout) :: object
    type(qcd_t), intent(in) :: qcd
    class(model_data_t), intent(in), target, optional :: model
    object%qcd = qcd

    call rclwrap_set_pole_mass &
         (11, dble(model%get_real (var_str ('me'))), 0._double)
    call rclwrap_set_pole_mass &
         (13, dble(model%get_real (var_str ('mmu'))), 0._double)
    call rclwrap_set_pole_mass &
         (15, dble(model%get_real (var_str ('mtau'))), 0._double)

    call rclwrap_set_pole_mass (1, 0._double, 0._double)
    call rclwrap_set_pole_mass (2, 0._double, 0._double)

    call rclwrap_set_pole_mass (3, dble(model%get_real (var_str ('ms'))), 0._double)
    call rclwrap_set_pole_mass (4, dble(model%get_real (var_str ('mc'))), 0._double)
    call rclwrap_set_pole_mass (5, dble(model%get_real (var_str ('mb'))), 0._double)
    call rclwrap_set_pole_mass (6, dble(model%get_real (var_str ('mtop'))), &
         dble(model%get_real (var_str ('wtop'))))

    call rclwrap_set_pole_mass (23, dble(model%get_real (var_str ('mZ'))), &
         dble(model%get_real (var_str ('wZ'))))
    call rclwrap_set_pole_mass (24, dble(model%get_real (var_str ('mW'))), &
         dble(model%get_real (var_str ('wW'))))
    call rclwrap_set_pole_mass (25, dble(model%get_real (var_str ('mH'))), &
         dble(model%get_real (var_str ('wH'))))

    call rclwrap_use_gfermi_scheme (dble(model%get_real (var_str ('GF'))))
    call rclwrap_set_light_fermions (0._double)
    call rclwrap_set_delta_ir (0._double, dble(pi**2 / 6))
  end subroutine prc_recola_set_parameters

  subroutine prc_recola_set_mu_ir (object, mu)
    class(prc_recola_t), intent(inout) :: object
    real(default), intent(in) :: mu
    call rclwrap_set_mu_ir (dble(mu))
  end subroutine prc_recola_set_mu_ir

  function prc_recola_has_matrix_element (object) result (flag)
    logical :: flag
    class(prc_recola_t), intent(in) :: object
    flag = .true.
  end function prc_recola_has_matrix_element

  subroutine recola_def_init (object, basename, model_name, &
     prt_in, prt_out, nlo_type)
    class(recola_def_t), intent(inout) :: object
    type(string_t), intent(in) :: basename, model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    integer, intent(in) :: nlo_type
    object%basename = basename
    allocate (recola_writer_t :: object%writer)
    select case (nlo_type)
    case (BORN)
       object%suffix = '_BORN'
    case (NLO_REAL)
       object%suffix = '_REAL'
    case (NLO_VIRTUAL)
       object%suffix = '_LOOP'
    case (NLO_SUBTRACTION)
       object%suffix = '_SUB'
    case (NLO_MISMATCH)
       object%suffix = '_MISMATCH'
    case (NLO_DGLAP)
       object%suffix = '_DGLAP'
    end select
    select type (writer => object%writer)
    class is (recola_writer_t)
       call writer%init (model_name, prt_in, prt_out)
    end select
  end subroutine recola_def_init

  subroutine recola_def_allocate_driver (object, driver, basename)
    class(recola_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    if (.not. allocated (driver)) allocate (recola_driver_t :: driver)
  end subroutine recola_def_allocate_driver

  subroutine recola_def_read (object, unit)
    class(recola_def_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine recola_def_read

  function recola_def_type_string () result (string)
    type(string_t) :: string
    string = "recola"
  end function recola_def_type_string

  subroutine recola_def_write (object, unit)
    class(recola_def_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine recola_def_write

  function recola_driver_type_name () result (type)
    type(string_t) :: type
    type = "Recola"
  end function recola_driver_type_name


end module prc_recola
