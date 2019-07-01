! WHIZARD 2.6.3 Feb 10 2018
!
! Copyright (C) 1999-2018 by
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

module process

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use constants
  use diagnostics
  use numeric_utils
  use lorentz
  use cputime
  use md5
  use rng_base
  use os_interface
  use sm_qcd
  use integration_results
  use mci_base
  use flavors
  use model_data
  use models
  use physics_defs
  use process_libraries
  use process_constants
  use particles
  use variables
  use beam_structures
  use beams
  use interactions
  use pdg_arrays
  use expr_base
  use sf_base
  use sf_mappings
  use resonances, only: resonance_history_t, resonance_history_set_t

  use prc_test_core, only: test_t
  use prc_core, only: prc_core_t
  use prc_user_defined, only: prc_user_defined_base_t
  use prc_recola, only: prc_recola_t
  use blha_olp_interfaces, only: prc_blha_t, blha_template_t
  use prc_threshold, only: prc_threshold_t

  use phs_base
  use phs_wood, only: phs_wood_config_t
  use phs_wood, only: EXTENSION_DEFAULT, EXTENSION_DGLAP
  use phs_fks, only: phs_fks_config_t, get_filtered_resonance_histories
  use blha_config, only: blha_master_t
  use nlo_data, only: FKS_DEFAULT, FKS_RESONANCES
  use nlo_data, only: fks_template_t

  use parton_states, only: connected_state_t
  use pcm_base
  use pcm
  use process_counter
  use core_manager
  use process_config
  use process_mci

  implicit none
  private

  public :: process_t
  public :: process_ptr_t

  type :: process_t
     private
     type(process_metadata_t) :: &
          meta
     type(process_config_data_t) :: &
          config
     type(process_counter_t) :: &
          counter
     type(process_component_t), dimension(:), allocatable :: &
          component
     type(process_term_t), dimension(:), allocatable :: &
          term
     type(process_beam_config_t) :: &
          beam_config
     type(process_mci_entry_t), dimension(:), allocatable :: &
          mci_entry
     class(pcm_t), allocatable :: &
          pcm
     type(core_manager_t) :: cm
     logical, dimension(:), allocatable :: component_selected
   contains
     procedure :: write => process_write
     procedure :: write_meta => process_write_meta
     procedure :: show => process_show
     procedure :: final => process_final
     procedure :: init => process_init
     procedure :: set_var_list => process_set_var_list
     procedure :: core_manager_register => process_core_manager_register
     procedure :: core_manager_register_default => process_core_manager_register_default
     procedure :: core_manager_register_sub => process_core_manager_register_sub
     procedure :: allocate_cm_arrays => process_allocate_cm_arrays
     procedure :: allocate_core => process_allocate_core
     procedure :: init_component => process_init_component
     procedure :: setup_terms => process_setup_terms
     procedure :: setup_beams_sqrts => process_setup_beams_sqrts
     procedure :: setup_beams_decay => process_setup_beams_decay
     procedure :: check_masses => process_check_masses
     procedure :: get_pdg_in => process_get_pdg_in
     procedure :: get_phs_config => process_get_phs_config
     procedure :: extract_resonance_history_set &
          => process_extract_resonance_history_set
     procedure :: setup_beams_beam_structure => process_setup_beams_beam_structure
     procedure :: beams_startup_message => process_beams_startup_message
     procedure :: configure_phs => process_configure_phs
     procedure :: print_phs_startup_message => process_print_phs_startup_message
     procedure :: init_sf_chain => process_init_sf_chain
     generic :: set_sf_channel => set_sf_channel_single
     procedure :: set_sf_channel_single => process_set_sf_channel
     generic :: set_sf_channel => set_sf_channel_array
     procedure :: set_sf_channel_array => process_set_sf_channel_array
     procedure :: sf_startup_message => process_sf_startup_message
     procedure :: collect_channels => process_collect_channels
     procedure :: contains_trivial_component => process_contains_trivial_component
     procedure :: get_master_component => process_get_master_component
     procedure :: setup_mci => process_setup_mci
     procedure :: set_cuts => process_set_cuts
     procedure :: set_scale => process_set_scale
     procedure :: set_fac_scale => process_set_fac_scale
     procedure :: set_ren_scale => process_set_ren_scale
     procedure :: set_weight => process_set_weight
     procedure :: compute_md5sum => process_compute_md5sum
     procedure :: sampler_test => process_sampler_test
     procedure :: final_integration => process_final_integration
     procedure :: integrate_dummy => process_integrate_dummy
     procedure :: integrate => process_integrate
     procedure :: generate_weighted_event => process_generate_weighted_event
     procedure :: generate_unweighted_event => process_generate_unweighted_event
     procedure :: display_summed_results => process_display_summed_results
     procedure :: display_integration_history => &
          process_display_integration_history
     procedure :: write_logfile => process_write_logfile
     procedure :: write_state_summary => process_write_state_summary
     procedure :: prepare_simulation => process_prepare_simulation
     generic :: has_integral => has_integral_tot, has_integral_mci
     procedure :: has_integral_tot => process_has_integral_tot
     procedure :: has_integral_mci => process_has_integral_mci
     generic :: get_integral => get_integral_tot, get_integral_mci
     generic :: get_error => get_error_tot, get_error_mci
     generic :: get_efficiency => get_efficiency_tot, get_efficiency_mci
     procedure :: get_integral_tot => process_get_integral_tot
     procedure :: get_integral_mci => process_get_integral_mci
     procedure :: get_error_tot => process_get_error_tot
     procedure :: get_error_mci => process_get_error_mci
     procedure :: get_efficiency_tot => process_get_efficiency_tot
     procedure :: get_efficiency_mci => process_get_efficiency_mci
     procedure :: get_correction => process_get_correction
     procedure :: get_correction_error => process_get_correction_error
     procedure :: lab_is_cm_frame => process_lab_is_cm_frame
     procedure :: get_component_ptr => process_get_component_ptr
     procedure :: get_qcd_ptr => process_get_qcd_ptr
     generic :: get_component_type => get_component_type_single
     procedure :: get_component_type_single => process_get_component_type_single
     generic :: get_component_type => get_component_type_all
     procedure :: get_component_type_all => process_get_component_type_all
     procedure :: get_component_i_terms => process_get_component_i_terms
     procedure :: get_n_allowed_born => process_get_n_allowed_born
     procedure :: get_pcm_ptr => process_get_pcm_ptr
     generic :: component_can_be_integrated => component_can_be_integrated_single
     generic :: component_can_be_integrated => component_can_be_integrated_all
     procedure :: component_can_be_integrated_single => process_component_can_be_integrated_single
     procedure :: component_can_be_integrated_all => process_component_can_be_integrated_all
     procedure :: reset_selected_cores => process_reset_selected_cores
     procedure :: select_components => process_select_components
     procedure :: component_is_selected => process_component_is_selected
     procedure :: get_coupling_powers => process_get_coupling_powers
     procedure :: get_real_component => process_get_real_component
     procedure :: extract_active_component_mci => process_extract_active_component_mci
     procedure :: needs_extra_code => process_needs_extra_code
     procedure :: uses_real_partition => process_uses_real_partition
     procedure :: get_md5sum_prc => process_get_md5sum_prc
     procedure :: get_md5sum_mci => process_get_md5sum_mci
     procedure :: get_md5sum_cfg => process_get_md5sum_cfg
     procedure :: init_cores => process_init_cores
     procedure :: init_blha_cores => process_init_blha_cores
     procedure :: get_n_cores => process_get_n_cores
     procedure :: get_core_manager_index => process_get_core_manager_index
     procedure :: get_core_manager => process_get_core_manager
     procedure :: get_core_manager_ptr => process_get_core_manager_ptr
     procedure :: get_base_i_term => process_get_base_i_term
     procedure :: get_core_term => process_get_core_term
     procedure :: get_subtraction_core => process_get_subtraction_core
     procedure :: get_term_ptr => process_get_term_ptr
     procedure :: get_core_from_md5sum => process_get_core_from_md5sum
     procedure :: get_i_core_nlo_type => process_get_i_core_nlo_type
     procedure :: get_i_term => process_get_i_term
     procedure :: set_i_mci_work => process_set_i_mci_work
     procedure :: get_i_mci_work => process_get_i_mci_work
     procedure :: get_i_sub => process_get_i_sub
     procedure :: get_i_term_virtual => process_get_i_term_virtual
     generic :: component_is_active => component_is_active_single
     procedure :: component_is_active_single => process_component_is_active_single
     generic :: component_is_active => component_is_active_all
     procedure :: component_is_active_all => process_component_is_active_all
     procedure :: get_n_pass_default => process_get_n_pass_default
     procedure :: adapt_grids_default => process_adapt_grids_default
     procedure :: adapt_weights_default => process_adapt_weights_default
     procedure :: get_n_it_default => process_get_n_it_default
     procedure :: get_n_calls_default => process_get_n_calls_default
     procedure :: get_id => process_get_id
     procedure :: get_num_id => process_get_num_id
     procedure :: get_run_id => process_get_run_id
     procedure :: get_library_name => process_get_library_name
     procedure :: get_n_in => process_get_n_in
     procedure :: get_n_mci => process_get_n_mci
     procedure :: get_n_components => process_get_n_components
     procedure :: get_n_terms => process_get_n_terms
     procedure :: get_i_component => process_get_i_component
     procedure :: get_component_id => process_get_component_id
     procedure :: get_component_def_ptr => process_get_component_def_ptr
     procedure :: extract_core => process_extract_core
     procedure :: restore_core => process_restore_core
     procedure :: get_constants => process_get_constants
     procedure :: get_config => process_get_config
     procedure :: get_md5sum_constants => process_get_md5sum_constants
     procedure :: get_term_flv_out => process_get_term_flv_out
     procedure :: contains_unstable => process_contains_unstable
     procedure :: get_sqrts => process_get_sqrts
     procedure :: get_polarization => process_get_polarization
     procedure :: get_meta => process_get_meta
     procedure :: has_matrix_element => process_has_matrix_element
     procedure :: get_beam_data_ptr => process_get_beam_data_ptr
     procedure :: get_beam_config => process_get_beam_config
     procedure :: get_beam_config_ptr => process_get_beam_config_ptr
     procedure :: cm_frame => process_cm_frame
     procedure :: get_pdf_set => process_get_pdf_set
     procedure :: pcm_contains_pdfs => process_pcm_contains_pdfs
     procedure :: get_beam_file => process_get_beam_file
     procedure :: get_var_list_ptr => process_get_var_list_ptr
     procedure :: get_model_ptr => process_get_model_ptr
     procedure :: make_rng => process_make_rng
     procedure :: compute_amplitude => process_compute_amplitude
     procedure :: check_library_sanity => process_check_library_sanity
     procedure :: nullify_library_pointer => process_nullify_library_pointer
     procedure :: set_component_type => process_set_component_type
     procedure :: set_counter_mci_entry => process_set_counter_mci_entry
     procedure :: pacify => process_pacify
     procedure :: test_allocate_sf_channels
     procedure :: test_set_component_sf_channel
     procedure :: test_get_mci_ptr
     procedure :: init_mci_work => process_init_mci_work
     procedure :: setup_test_cores => process_setup_test_cores
     procedure :: write_cm => process_write_cm
     procedure :: get_connected_states => process_get_connected_states
     procedure :: init_nlo_settings => process_init_nlo_settings
     procedure :: get_nlo_type => process_get_nlo_type
     generic :: get_nlo_type_component => get_nlo_type_component_single
     procedure :: get_nlo_type_component_single => process_get_nlo_type_component_single
     generic :: get_nlo_type_component => get_nlo_type_component_all
     procedure :: get_nlo_type_component_all => process_get_nlo_type_component_all
     procedure :: is_nlo_calculation => process_is_nlo_calculation
     procedure :: is_combined_nlo_integration &
          => process_is_combined_nlo_integration
     procedure :: component_is_real_finite => process_component_is_real_finite
     procedure :: get_component_nlo_type => process_get_component_nlo_type
     procedure :: get_component_associated_born &
               => process_get_component_associated_born
     procedure :: get_first_real_component => process_get_first_real_component
     procedure :: get_first_real_term => process_get_first_real_term
     procedure :: get_associated_real_fin => process_get_associated_real_fin
     procedure :: setup_region_data => process_setup_region_data
     procedure :: setup_real_partition => process_setup_real_partition
     procedure :: check_if_threshold_method => process_check_if_threshold_method
     procedure :: select_i_term => process_select_i_term
     procedure :: create_blha_interface => process_create_blha_interface
     procedure :: create_and_load_extra_libraries &
        => process_create_and_load_extra_libraries
  end type process_t

  type :: process_ptr_t
     type(process_t), pointer :: p => null ()
  end type process_ptr_t


contains

  subroutine process_write (process, screen, unit, &
       show_all, show_var_list, &
       show_os_data, &
       show_rng_factory, show_model, show_expressions, &
       show_sfchain, &
       show_equivalences, show_history, show_histories, &
       show_forest, show_x, &
       show_subevt, show_evaluators, pacify)
    class(process_t), intent(in) :: process
    logical, intent(in) :: screen
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_all
    logical, intent(in), optional :: show_var_list
    logical, intent(in), optional :: show_os_data
    logical, intent(in), optional :: show_rng_factory
    logical, intent(in), optional :: show_model, show_expressions
    logical, intent(in), optional :: show_sfchain
    logical, intent(in), optional :: show_equivalences
    logical, intent(in), optional :: show_history, show_histories
    logical, intent(in), optional :: show_forest, show_x
    logical, intent(in), optional :: show_subevt, show_evaluators
    logical, intent(in), optional :: pacify
    logical :: all
    logical :: var_list
    logical :: counters
    logical :: os_data
    logical :: rng_factory, model, expressions
    integer :: u, i
    u = given_output_unit (unit)
    if (present (show_all)) then
       all = show_all
    else
       all = .false.
    end if
    var_list = .false.
    counters = .true.
    os_data = .false.
    model = .false.
    rng_factory = .true.
    expressions = .false.
    if (present (show_var_list)) then
       all = .false.; var_list = show_var_list
    end if
    if (present (show_os_data)) then
       all = .false.; os_data = show_os_data
    end if
    if (present (show_rng_factory)) then
       all = .false.; rng_factory = show_rng_factory
    end if
    if (present (show_model)) then
       all = .false.; model = show_model
    end if
    if (present (show_expressions)) then
       all = .false.; expressions = show_expressions
    end if
    if (all) then
       var_list = .true.
       rng_factory = .true.
       model = .true.
       expressions = .true.
    end if
    if (screen) then
       write (msg_buffer, "(A)")  repeat ("-", 72)
       call msg_message ()
    else
       call write_separator (u, 2)
    end if
    call process%meta%write (u, var_list, screen)
    if (process%meta%type == PRC_UNKNOWN) then
       call write_separator (u, 2)
       return
    else
       if (.not. screen)  call write_separator (u)
    end if
    if (screen)  return
    call process%config%write &
         (u, counters, os_data, rng_factory, model, expressions)
    call write_separator (u, 2)
    if (allocated (process%component)) then
       write (u, "(1x,A)") "Process component configuration:"
       do i = 1, size (process%component)
          call write_separator (u)
          call process%component(i)%write (u)
       end do
    else
       write (u, "(1x,A)") "Process component configuration: [undefined]"
    end if
    call write_separator (u, 2)
    if (allocated (process%term)) then
       write (u, "(1x,A)") "Process term configuration:"
       do i = 1, size (process%term)
          call write_separator (u)
          call process%term(i)%write (u)
       end do
    else
       write (u, "(1x,A)") "Process term configuration: [undefined]"
    end if
    call write_separator (u, 2)
    call process%beam_config%write (u)
    call write_separator (u, 2)
    if (allocated (process%mci_entry)) then
       write (u, "(1x,A)") "Multi-channel integrator configurations:"
       do i = 1, size (process%mci_entry)
          call write_separator (u)
          write (u, "(1x,A,I0,A)")  "MCI #", i, ":"
          call process%mci_entry(i)%write (u, pacify)
       end do
    end if
    call write_separator (u, 2)
  end subroutine process_write

  subroutine process_write_meta (process, unit, testflag)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit)
    select case (process%meta%type)
    case (PRC_UNKNOWN)
       write (u, "(1x,A)") "Process instance [undefined]"
       return
    case (PRC_DECAY)
       write (u, "(1x,A)", advance="no") "Process instance [decay]:"
    case (PRC_SCATTERING)
       write (u, "(1x,A)", advance="no") "Process instance [scattering]:"
    case default
       call msg_bug ("process_instance_write: undefined process type")
    end select
    write (u, "(1x,A,A,A)") "'", char (process%meta%id), "'"
    write (u, "(3x,A,A,A)") "Run ID = '", char (process%meta%run_id), "'"
    if (allocated (process%meta%component_id)) then
       write (u, "(3x,A)")  "Process components:"
       do i = 1, size (process%meta%component_id)
          if (process%component_selected (i)) then
             write (u, "(3x,'*')", advance="no")
          else
             write (u, "(4x)", advance="no")
          end if
          write (u, "(1x,I0,9A)")  i, ": '", &
               char (process%meta%component_id (i)), "':   ", &
               char (process%meta%component_description (i))
       end do
    end if
  end subroutine process_write_meta

  subroutine process_show (object, unit, verbose)
    class(process_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    logical :: verb
    u = given_output_unit (unit)
    verb = .true.;  if (present (verbose)) verb = verbose
    if (verb) then
       call object%meta%show (u, object%config%model%get_name ())
       select case (object%meta%type)
       case (PRC_DECAY)
          write (u, "(2x,A)", advance="no")  "Computed width ="
       case (PRC_SCATTERING)
          write (u, "(2x,A)", advance="no")  "Computed cross section ="
       case default;  return
       end select
    else
       write (u, "(A)", advance="no") char (object%meta%id)
       select case (object%meta%num_id)
       case (0)
          write (u, "(':')")
       case default
          write (u, "(1x,'(',I0,')',':')") object%meta%num_id
       end select
       write (u, "(2x)", advance="no")
    end if
    if (object%has_integral_tot ()) then
       write (u, "(ES14.7,1x,'+-',ES9.2)", advance="no") &
            object%get_integral_tot (), object%get_error_tot ()
       select case (object%meta%type)
       case (PRC_DECAY)
          write (u, "(1x,A)")  "GeV"
       case (PRC_SCATTERING)
          write (u, "(1x,A)")  "fb"
       case default
          write (u, *)
       end select
    else
       write (u, "(A)")  "[integral undefined]"
    end if
  end subroutine process_show

  subroutine process_final (process)
    class(process_t), intent(inout) :: process
    integer :: i
    call process%meta%final ()
    call process%config%final ()
    if (allocated (process%component)) then
       do i = 1, size (process%component)
          call process%component(i)%final ()
       end do
    end if
    if (allocated (process%term)) then
       do i = 1, size (process%term)
          call process%term(i)%final ()
       end do
    end if
    call process%beam_config%final ()
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          call process%mci_entry(i)%final ()
       end do
    end if
    call process%cm%final ()
    if (allocated (process%pcm)) then
       call process%pcm%final ()
       deallocate (process%pcm)
    end if
  end subroutine process_final

  subroutine process_init (process, proc_id, run_id, &
       lib, os_data, qcd, rng_factory, model)
    class(process_t), intent(out) :: process
    type(string_t), intent(in) :: proc_id
    type(string_t), intent(in) :: run_id
    type(process_library_t), intent(in), target :: lib
    type(os_data_t), intent(in) :: os_data
    type(qcd_t), intent(in) :: qcd
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    class(model_data_t), intent(inout), pointer :: model
    call msg_debug (D_PROCESS_INTEGRATION, "process_init")
    if (.not. lib%is_active ()) then
       call msg_bug ("Process init: inactive library not handled yet")
    end if
    if (.not. lib%contains (proc_id)) then
       call msg_fatal ("Process library doesn't contain process '" &
            // char (proc_id) // "'")
       return
    end if
    associate (meta => process%meta)
      call meta%init (proc_id, run_id, lib)
      call process%config%init &
           (meta, os_data, qcd, rng_factory, model)
      allocate (process%component (meta%n_components))
      allocate (process%component_selected (meta%n_components))
      process%component_selected = .false.
    end associate
    if (.not. lib%get_nlo_process (proc_id)) then
       allocate (pcm_default_t :: process%pcm)
    else
       allocate (pcm_nlo_t :: process%pcm)
    end if
  end subroutine process_init

  subroutine process_set_var_list (process, var_list)
    class(process_t), intent(inout) :: process
    type(var_list_t), intent(in) :: var_list
    call var_list_init_snapshot &
         (process%meta%var_list, var_list, follow_link=.true.)
  end subroutine process_set_var_list

  subroutine process_core_manager_register (process, &
     nlo_type, i_component, type_string)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: nlo_type, i_component
    type(string_t), intent(in), optional :: type_string
    select case (nlo_type)
    case (NLO_SUBTRACTION)
       call process%core_manager_register_sub (nlo_type, i_component, type_string)
    case (NLO_MISMATCH)
       process%cm%i_component_to_i_core(i_component) = &
            process%get_i_core_nlo_type (NLO_SUBTRACTION, .true.)
    case default
       call process%core_manager_register_default (nlo_type, i_component, type_string)
    end select
  end subroutine process_core_manager_register

  subroutine process_core_manager_register_sub (process, nlo_type, i_component, type_string)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: nlo_type, i_component
    type(string_t), intent(in), optional :: type_string
    character(32) :: md5sum
    integer :: i
    md5sum = process%get_md5sum_constants (i_component, type_string, nlo_type)
    if (any (process%cm%md5s == md5sum)) then
       do i = 1, N_MAX_CORES
          if (process%cm%i_core(i) == 0) exit
          if (md5sum == process%cm%md5s(i)) then
            process%cm%sub(i) = .true.
         end if
       end do
    else
       process%cm%sub(process%cm%current_index) = .true.
       call process%cm%register_new (nlo_type, i_component, md5sum)
    end if
  end subroutine process_core_manager_register_sub

  subroutine process_core_manager_register_default &
     (process, nlo_type, i_component, type_string)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: nlo_type, i_component
    type(string_t), intent(in), optional :: type_string
    character(32) :: md5sum
    integer :: i
    logical :: check
    md5sum = process%get_md5sum_constants (i_component, type_string, nlo_type)
    check = .false.
    associate (cm => process%cm)
       if (.not. any (cm%md5s == md5sum)) then
          call cm%register_new (nlo_type, i_component, md5sum)
       else
          do i = 1, N_MAX_CORES
             if (cm%md5s(i) == md5sum) then
                call cm%register_existing (i, i_component)
                check = .true.
                exit
             end if
          end do
          if (.not. check) call msg_fatal ("Register core: Inconsistency encountered!")
       end if
    end associate
  end subroutine process_core_manager_register_default

  subroutine process_allocate_cm_arrays (process, n_components)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: n_components
    call process%cm%allocate_core_array ()
    call process%cm%create_i_core_to_first_i_component (n_components)
  end subroutine process_allocate_cm_arrays

  subroutine process_allocate_core (process, i_core, core_template)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_core
    class(prc_core_t), intent(in) :: core_template
    call process%cm%allocate_core (i_core, core_template)
  end subroutine process_allocate_core

  subroutine process_init_component &
       (process, index, active, mci_template, phs_config_template)
    class(process_t), intent(inout), target :: process
    integer, intent(in) :: index
    logical, intent(in) :: active
    class(mci_t), intent(in), allocatable :: mci_template
    class(phs_config_t), intent(in), allocatable :: phs_config_template
    type(process_constants_t) :: data
    call process%meta%lib%fill_constants (process%meta%id, index, data)
    associate (component => process%component(index))
       call component%init (index, &
            process%meta, process%config, &
            active, data, &
            mci_template, phs_config_template)
       if (.not. component%active .and. &
            component%config%get_nlo_type () /= NLO_SUBTRACTION) &
            call process%meta%deactivate_component(index)
    end associate
  end subroutine process_init_component

  subroutine process_setup_terms (process, with_beams)
    class(process_t), intent(inout), target :: process
    logical, intent(in), optional :: with_beams
    class(model_data_t), pointer :: model
    integer :: i, j, k, i_term
    integer, dimension(:), allocatable :: n_entry
    integer :: n_components, n_tot
    integer :: i_sub = 0
    type(string_t) :: subtraction_method
    class(prc_core_t), pointer :: core => null ()
    logical :: setup_subtraction_component, singular_real
    integer :: nlo_type_to_fetch
    model => process%config%model
    n_components = process%meta%n_components
    allocate (n_entry (n_components), source = 0)
    do i = 1, n_components
       associate (component => process%component(i))
         if (component%active) then
            n_entry(i) = 1
            if (component%get_nlo_type () == NLO_REAL) then
               select type (pcm => process%pcm)
               type is (pcm_nlo_t)
                  if (component%component_type /= COMP_REAL_FIN) &
                       n_entry(i) = n_entry(i) + pcm%region_data%get_n_phs ()
               end select
            end if
         end if
       end associate
    end do
    n_tot = sum (n_entry)
    allocate (process%term (n_tot))
    k = 0
    if (process%is_nlo_calculation ()) then
       i_sub = process%component(1)%config%get_associated_subtraction ()
       subtraction_method = process%component(i_sub)%config%get_me_method ()
       call msg_debug2 (D_PROCESS_INTEGRATION, "process_setup_terms: ", &
            subtraction_method)
    end if

    do i = 1, n_components
       associate (component => process%component(i))
         if (.not. component%active)  cycle
           allocate (component%i_term (n_entry(i)))
           do j = 1, n_entry(i)
              singular_real = component%get_nlo_type () == NLO_REAL &
                   .and. component%component_type /= COMP_REAL_FIN
              setup_subtraction_component = singular_real .and. j == n_entry(i)
              i_term = k + j
              component%i_term(j) = i_term
              if (singular_real) then
                 process%term(i_term)%i_sub = k + n_entry(i)
              else
                 process%term(i_term)%i_sub = 0
              end if
              nlo_type_to_fetch = component%get_nlo_type ()
              if (nlo_type_to_fetch == NLO_MISMATCH)  nlo_type_to_fetch = NLO_SUBTRACTION
              process%term(i_term)%i_core = set_i_core (i, nlo_type_to_fetch, &
                   setup_subtraction_component, component%config%get_def_type_string ())
              if (process%term(i_term)%i_core == 0) then
                 call msg_bug ("Process '" // char (process%get_id ()) &
                      // "': core not found!")
              end if
              core => process%get_core_term (i_term)
              process%term(i_term)%pcm => process%pcm
              if (i_sub > 0) then
                 select type (pcm => process%pcm)
                 type is (pcm_nlo_t)
                    call process%term(i_term)%init (i_term, i, j, core, model, &
                         nlo_type = component%config%get_nlo_type (), &
                         use_beam_pol = with_beams, &
                         subtraction_method = subtraction_method)
                 class default
                    call process%term(i_term)%init (i_term, i, j, core, model, &
                         nlo_type = component%config%get_nlo_type (), &
                         use_beam_pol = with_beams, &
                         subtraction_method = subtraction_method)
                 end select
              else
                 call process%term(i_term)%init (i_term, i, j, core, model, &
                      nlo_type = component%config%get_nlo_type (), &
                      use_beam_pol = with_beams)
              end if
           end do
       end associate
       k = k + n_entry(i)
    end do
    process%config%n_terms = n_tot
  contains
    function set_i_core (i_component, nlo_type, sub, type_string) result (i_core)
      integer :: i_core
      integer, intent(in) :: i_component, nlo_type
      logical, intent(in) :: sub
      type(string_t), intent(in) :: type_string
      character(32) :: md5sum
      integer :: index
      i_core = 0
      md5sum = process%get_md5sum_constants (i_component, type_string, nlo_type)
      do index = 1, N_MAX_CORES
         if (sub) then
            if (process%cm%sub(index)) then
                i_core = index
                exit
            end if
         else
            i_core = process%cm%i_core(index)
            if (process%cm%md5s(index) == md5sum) then
               i_core = index
               exit
            end if
         end if
      end do
    end function set_i_core
  end subroutine process_setup_terms

  subroutine process_setup_beams_sqrts (process, sqrts, beam_structure, i_core)
    class(process_t), intent(inout) :: process
    real(default), intent(in) :: sqrts
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer, intent(in), optional :: i_core
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_in
    integer, dimension(2) :: pdg_scattering
    type(flavor_t), dimension(2) :: flv_in
    integer :: i, i0, ic
    class(prc_core_t), pointer :: core => null ()
    allocate (pdg_in (2, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          if (present (i_core)) then
             ic = i_core
          else
             ic = process%cm%i_component_to_i_core (i)
          end if
          core => process%cm%get_core (ic)
          pdg_in(:,i) = core%data%get_pdg_in ()
          if (i0 == 0)  i0 = i
       end if
    end do
    do i = 1, process%meta%n_components
       if (.not. process%component(i)%active) then
          pdg_in(:,i) = pdg_in(:,i0)
       end if
    end do
    if (all (pdg_array_get_length (pdg_in) == 1) .and. &
         all (pdg_in(1,:) == pdg_in(1,i0)) .and. &
         all (pdg_in(2,:) == pdg_in(2,i0))) then
       pdg_scattering = pdg_array_get (pdg_in(:,i0), 1)
       call flv_in%init (pdg_scattering, process%config%model)
       call process%beam_config%init_scattering (flv_in, sqrts, beam_structure)
    else
       call msg_fatal ("Setting up process '" // char (process%meta%id) // "':", &
           [var_str ("   --------------------------------------------"), &
            var_str ("Inconsistent initial state. This happens if either "), &
            var_str ("several processes with non-matching initial states "), &
            var_str ("have been added, or for a single process with an "), &
            var_str ("initial state flavor sum. In that case, please set beams "), &
            var_str ("explicitly [singling out a flavor / structure function.]")])
    end if
  end subroutine process_setup_beams_sqrts

  subroutine process_setup_beams_decay (process, rest_frame, beam_structure, i_core)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: rest_frame
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer, intent(in), optional :: i_core
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_in
    integer, dimension(1) :: pdg_decay
    type(flavor_t), dimension(1) :: flv_in
    integer :: i, i0, ic
    class(prc_core_t), pointer :: core => null ()
    allocate (pdg_in (1, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          if (present (i_core)) then
             ic = i_core
          else
             ic = process%cm%i_component_to_i_core (i)
          end if
          core => process%cm%get_core (ic)
          pdg_in(:,i) = core%data%get_pdg_in ()
          if (i0 == 0)  i0 = i
       end if
    end do
    do i = 1, process%meta%n_components
       if (.not. process%component(i)%active) then
          pdg_in(:,i) = pdg_in(:,i0)
       end if
    end do
    if (all (pdg_array_get_length (pdg_in) == 1) &
         .and. all (pdg_in(1,:) == pdg_in(1,i0))) then
       pdg_decay = pdg_array_get (pdg_in(:,i0), 1)
       call flv_in%init (pdg_decay, process%config%model)
       call process%beam_config%init_decay (flv_in, rest_frame, beam_structure)
    else
       call msg_fatal ("Setting up decay '" &
            // char (process%meta%id) // "': decaying particle not unique")
    end if
  end subroutine process_setup_beams_decay

  subroutine process_check_masses (process)
       class(process_t), intent(in) :: process
       type(flavor_t), dimension(:), allocatable :: flv
       real(default), dimension(:), allocatable :: mass
       integer :: i, j
       integer :: i_component
       class(prc_core_t), pointer :: core => null ()
       do i = 1, process%get_n_terms ()
          i_component = process%term(i)%i_component
          if (.not. process%component(i_component)%active)  cycle
          core => process%get_core_term (i)
          associate (data => core%data)
            allocate (flv (data%n_flv), mass (data%n_flv))
            do j = 1, data%n_in + data%n_out
               call flv%init (data%flv_state(j,:), process%config%model)
               mass = flv%get_mass ()
               if (any (.not. nearly_equal(mass, mass(1)))) then
                  call msg_fatal ("Process '" // char (process%meta%id) // "': " &
                       // "mass values in flavor combination do not coincide. ")
               end if
            end do
            deallocate (flv, mass)
          end associate
       end do
   end subroutine process_check_masses

  subroutine process_get_pdg_in (process, pdg_in)
    class(process_t), intent(in) :: process
    type(pdg_array_t), dimension(:,:), allocatable, intent(out) :: pdg_in
    integer :: i, i_core
    class(prc_core_t), pointer :: core => null ()
    allocate (pdg_in (process%config%n_in, process%meta%n_components))
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          i_core = process%cm%i_component_to_i_core (i)
          core => process%cm%get_core (i_core)
          pdg_in(:,i) = core%data%get_pdg_in ()
       end if
    end do
    core => null ()
  end subroutine process_get_pdg_in

  function process_get_phs_config (process, i_component) result (phs_config)
    class(phs_config_t), pointer :: phs_config
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_component
    if (allocated (process%component)) then
       phs_config => process%component(i_component)%phs_config
    else
       phs_config => null ()
    end if
  end function process_get_phs_config

  subroutine process_extract_resonance_history_set &
       (process, res_set, include_trivial, i_component)
    class(process_t), intent(in), target :: process
    type(resonance_history_set_t), intent(out) :: res_set
    logical, intent(in), optional :: include_trivial
    integer, intent(in), optional :: i_component
    integer :: i
    i = 1;  if (present (i_component))  i = i_component
    select type (phs_config => process%get_phs_config (i))
    class is (phs_wood_config_t)
       call phs_config%extract_resonance_history_set (res_set, include_trivial)
    class default
       call msg_error ("process '" // char (process%get_id ()) &
            // "': extract resonance histories: phase-space method must be &
            &'wood'.  No resonances can be determined.")
    end select
  end subroutine process_extract_resonance_history_set

  subroutine process_setup_beams_beam_structure &
       (process, beam_structure, sqrts, decay_rest_frame)
    class(process_t), intent(inout) :: process
    type(beam_structure_t), intent(in) :: beam_structure
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: decay_rest_frame
    integer :: n_in
    logical :: applies
    n_in = process%get_n_in ()
    call beam_structure%check_against_n_in (process%get_n_in (), applies)
    if (applies) then
       call process%beam_config%init_beam_structure &
            (beam_structure, sqrts, process%get_model_ptr (), decay_rest_frame)
    else if (n_in == 2) then
       call process%setup_beams_sqrts (sqrts, beam_structure)
    else
       call process%setup_beams_decay (decay_rest_frame, beam_structure)
    end if
  end subroutine process_setup_beams_beam_structure

  subroutine process_beams_startup_message (process, unit, beam_structure)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    type(beam_structure_t), intent(in), optional :: beam_structure
    call process%beam_config%startup_message (unit, beam_structure)
  end subroutine process_beams_startup_message

  subroutine process_configure_phs (process, rebuild, ignore_mismatch, &
     combined_integration)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    logical, intent(in), optional :: combined_integration
    real(default) :: sqrts
    integer :: i, i_born
    class(phs_config_t), pointer :: phs_config_born
    sqrts = process%get_sqrts ()
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
         if (component%active) then
            select type (pcm => process%pcm)
            type is (pcm_default_t)
               call component%configure_phs (sqrts, process%beam_config, &
                    rebuild, ignore_mismatch)
            class is (pcm_nlo_t)
               select case (component%config%get_nlo_type ())
               case (BORN, NLO_VIRTUAL, NLO_SUBTRACTION)
                  call component%configure_phs (sqrts, process%beam_config, &
                       rebuild, ignore_mismatch)
                  call check_and_extend_phs (component)
               case (NLO_REAL, NLO_MISMATCH, NLO_DGLAP)
                  i_born = component%config%get_associated_born ()
                  if (component%component_type /= COMP_REAL_FIN) &
                       call check_and_extend_phs (component)
                  call process%component(i_born)%get_phs_config (phs_config_born)
                  select type (config => component%phs_config)
                  type is (phs_fks_config_t)
                     select type (phs_config_born)
                     type is (phs_wood_config_t)
                        config%md5sum_born_config = phs_config_born%md5sum_phs_config
                        call config%set_born_config (phs_config_born)
                        call config%set_mode (component%config%get_nlo_type ())
                     end select
                  end select
                  call component%configure_phs (sqrts, &
                       process%beam_config, rebuild, ignore_mismatch)
               end select
            class default
               call msg_bug ("process_configure_phs: unsupported PCM type")
            end select
         end if
       end associate
    end do
  contains
    subroutine check_and_extend_phs (component)
      type(process_component_t), intent(inout) :: component
      logical :: requires_dglap_random_number
      if (combined_integration) then
         requires_dglap_random_number = any (process%component%get_nlo_type () == NLO_DGLAP)
         select type (phs_config => component%phs_config)
         class is (phs_wood_config_t)
            if (requires_dglap_random_number) then
               call phs_config%set_extension_mode (EXTENSION_DGLAP)
            else
               call phs_config%set_extension_mode (EXTENSION_DEFAULT)
            end if
            call phs_config%increase_n_par ()
         end select
      end if
    end subroutine check_and_extend_phs
  end subroutine process_configure_phs

  subroutine process_print_phs_startup_message (process)
    class(process_t), intent(in) :: process
    integer :: i_component
    do i_component = 1, process%meta%n_components
       associate (component => process%component(i_component))
          if (component%active) then
             call component%phs_config%startup_message ()
          end if
       end associate
    end do
  end subroutine process_print_phs_startup_message

  subroutine process_init_sf_chain (process, sf_config, sf_trace_file)
    class(process_t), intent(inout) :: process
    type(sf_config_t), dimension(:), intent(in) :: sf_config
    type(string_t), intent(in), optional :: sf_trace_file
    type(string_t) :: file
    if (present (sf_trace_file)) then
       if (sf_trace_file /= "") then
          file = sf_trace_file
       else
          file = process%get_id () // "_sftrace.dat"
       end if
       call process%beam_config%init_sf_chain (sf_config, file)
    else
       call process%beam_config%init_sf_chain (sf_config)
    end if
  end subroutine process_init_sf_chain

  subroutine process_set_sf_channel (process, c, sf_channel)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: c
    type(sf_channel_t), intent(in) :: sf_channel
    call process%beam_config%set_sf_channel (c, sf_channel)
  end subroutine process_set_sf_channel

  subroutine process_set_sf_channel_array (process, sf_channel)
    class(process_t), intent(inout) :: process
    type(sf_channel_t), dimension(:), intent(in) :: sf_channel
    integer :: c
    call process%beam_config%allocate_sf_channels (size (sf_channel))
    do c = 1, size (sf_channel)
       call process%beam_config%set_sf_channel (c, sf_channel(c))
    end do
  end subroutine process_set_sf_channel_array

  subroutine process_sf_startup_message (process, sf_string, unit)
    class(process_t), intent(in) :: process
    type(string_t), intent(in) :: sf_string
    integer, intent(in), optional :: unit
    call process%beam_config%sf_startup_message (sf_string, unit)
  end subroutine process_sf_startup_message

  subroutine process_collect_channels (process, coll)
    class(process_t), intent(inout) :: process
    type(phs_channel_collection_t), intent(inout) :: coll
    integer :: i
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
         if (component%active) &
            call component%collect_channels (coll)
       end associate
    end do
  end subroutine process_collect_channels

  function process_contains_trivial_component (process) result (flag)
    class(process_t), intent(in) :: process
    logical :: flag
    integer :: i
    flag = .true.
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
         if (component%active) then
            if (component%get_n_phs_par () == 0)  return
         end if
       end associate
    end do
    flag = .false.
  end function process_contains_trivial_component

  function process_get_master_component (process, i_mci) result (i_component)
     integer :: i_component
     class(process_t), intent(in) :: process
     integer, intent(in) :: i_mci
     integer :: i
     i_component = 0
     do i = 1, size (process%component)
        if (process%component(i)%i_mci == i_mci) then
           i_component = i
           return
        end if
     end do
  end function process_get_master_component


  subroutine process_setup_mci (process, combined_integration)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: combined_integration
    integer :: n_mci, i_mci
    integer :: i
    logical :: uses_real_partition
    call msg_debug (D_PROCESS_INTEGRATION, "process_setup_mci")
    n_mci = 0
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
          if (component%needs_mci_entry (combined_integration) .and. &
              component%config%get_nlo_type () /= NLO_SUBTRACTION) then
            n_mci = n_mci + 1
            component%i_mci = n_mci
         end if
         call msg_debug (D_PROCESS_INTEGRATION, &
              "component%component_type", component%component_type)
       end associate
    end do
    process%config%n_mci = n_mci
    if (.not. allocated (process%config%rng_factory)) &
         call msg_bug ("Process setup: rng factory not allocated")
    allocate (process%mci_entry (n_mci))
    i_mci = 0
    uses_real_partition = &
        any (process%component%component_type == COMP_REAL_FIN)
    call msg_debug (D_PROCESS_INTEGRATION, "uses_real_partition", &
         uses_real_partition)
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
          if (component%needs_mci_entry (combined_integration) .and. &
              component%config%get_nlo_type () /= NLO_SUBTRACTION) then
            i_mci = i_mci + 1
            associate (mci_entry => process%mci_entry(i_mci))
              call mci_entry%set_combined_integration (combined_integration)
              if (uses_real_partition) then
                 if (component%component_type == COMP_REAL_FIN) then
                    mci_entry%real_partition_type = REAL_FINITE
                 else
                    mci_entry%real_partition_type = REAL_SINGULAR
                 end if
              end if

              call mci_entry%init (process%meta%type, &
                   i_mci, i, component, process%beam_config%n_sfpar, &
                   process%config%rng_factory)
            end associate
          end if
       end associate
    end do
    do i_mci = 1, size (process%mci_entry)
       call process%mci_entry(i_mci)%set_parameters (process%meta%var_list)
    end do
  end subroutine process_setup_mci

  subroutine process_set_cuts (process, ef_cuts)
    class(process_t), intent(inout) :: process
    class(expr_factory_t), intent(in) :: ef_cuts
    allocate (process%config%ef_cuts, source = ef_cuts)
  end subroutine process_set_cuts

  subroutine process_set_scale (process, ef_scale)
    class(process_t), intent(inout) :: process
    class(expr_factory_t), intent(in) :: ef_scale
    allocate (process%config%ef_scale, source = ef_scale)
  end subroutine process_set_scale

  subroutine process_set_fac_scale (process, ef_fac_scale)
    class(process_t), intent(inout) :: process
    class(expr_factory_t), intent(in) :: ef_fac_scale
    allocate (process%config%ef_fac_scale, source = ef_fac_scale)
  end subroutine process_set_fac_scale

  subroutine process_set_ren_scale (process, ef_ren_scale)
    class(process_t), intent(inout) :: process
    class(expr_factory_t), intent(in) :: ef_ren_scale
    allocate (process%config%ef_ren_scale, source = ef_ren_scale)
  end subroutine process_set_ren_scale

  subroutine process_set_weight (process, ef_weight)
    class(process_t), intent(inout) :: process
    class(expr_factory_t), intent(in) :: ef_weight
    allocate (process%config%ef_weight, source = ef_weight)
  end subroutine process_set_weight

  subroutine process_compute_md5sum (process)
    class(process_t), intent(inout) :: process
    integer :: i
    call process%config%compute_md5sum ()
    do i = 1, process%config%n_components
       associate (component => process%component(i))
         if (component%active) then
            call component%compute_md5sum ()
         end if
       end associate
    end do
    call process%beam_config%compute_md5sum ()
    do i = 1, process%config%n_mci
       call process%mci_entry(i)%compute_md5sum &
            (process%config, process%component, process%beam_config)
    end do
  end subroutine process_compute_md5sum

  subroutine process_sampler_test (process, sampler, n_calls, i_mci)
    class(process_t), intent(inout) :: process
    class(mci_sampler_t), intent(inout) :: sampler
    integer, intent(in) :: n_calls, i_mci
    call process%mci_entry(i_mci)%sampler_test (sampler, n_calls)
  end subroutine process_sampler_test

  subroutine process_final_integration (process, i_mci)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    call process%mci_entry(i_mci)%final_integration ()
  end subroutine process_final_integration

  subroutine process_integrate_dummy (process)
    class(process_t), intent(inout) :: process
    type(integration_results_t) :: results
    integer :: u_log
    u_log = logfile_unit ()
    call results%init (process%meta%type)
    call results%display_init (screen = .true., unit = u_log)
    call results%new_pass ()
    call results%record (1, 0, 0._default, 0._default, 0._default)
    call results%display_final ()
  end subroutine process_integrate_dummy

  subroutine process_integrate (process, i_mci, mci_work, &
     mci_sampler, n_it, n_calls, adapt_grids, adapt_weights, final, &
     pacify, nlo_type)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(mci_work_t), intent(inout) :: mci_work
    class(mci_sampler_t), intent(inout) :: mci_sampler
    integer, intent(in) :: n_it, n_calls
    logical, intent(in), optional :: adapt_grids, adapt_weights
    logical, intent(in), optional :: final
    logical, intent(in), optional :: pacify
    integer, intent(in), optional :: nlo_type
    associate (mci_entry => process%mci_entry(i_mci))
       call mci_entry%integrate (mci_work%mci, mci_sampler, n_it, n_calls, &
            adapt_grids, adapt_weights, final, pacify, &
            nlo_type = nlo_type)
       call mci_entry%results%display_pass (pacify)
    end associate
  end subroutine process_integrate

  subroutine process_generate_weighted_event (process, i_mci, mci_work, &
     mci_sampler, keep_failed_events)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(mci_work_t), intent(inout) :: mci_work
    class(mci_sampler_t), intent(inout) :: mci_sampler
    logical, intent(in) :: keep_failed_events
    associate (mci_entry => process%mci_entry(i_mci))
       call mci_entry%generate_weighted_event (mci_work%mci, &
            mci_sampler, keep_failed_events)
    end associate
  end subroutine process_generate_weighted_event

  subroutine process_generate_unweighted_event (process, i_mci, &
     mci_work, mci_sampler)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(mci_work_t), intent(inout) :: mci_work
    class(mci_sampler_t), intent(inout) :: mci_sampler
    associate (mci_entry => process%mci_entry(i_mci))
       call mci_entry%generate_unweighted_event &
          (mci_work%mci, mci_sampler)
    end associate
  end subroutine process_generate_unweighted_event

  subroutine process_display_summed_results (process, pacify)
    class(process_t), intent(inout) :: process
    logical, intent(in) :: pacify
    type(integration_results_t) :: results
    integer :: u_log
    u_log = logfile_unit ()
    call results%init (process%meta%type)
    call results%display_init (screen = .true., unit = u_log)
    call results%new_pass ()
    call results%record (1, 0, &
         process%get_integral (), &
         process%get_error (), &
         process%get_efficiency (), suppress = pacify)
    select type (pcm => process%pcm)
    class is (pcm_nlo_t)
       !!! Check that Born integral is there
       if (process%component_can_be_integrated (1)) then
          call results%record_correction (process%get_correction (), &
               process%get_correction_error ())
       end if
    end select
    call results%display_final ()
  end subroutine process_display_summed_results

  subroutine process_display_integration_history &
       (process, i_mci, filename, os_data, eff_reset)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(string_t), intent(in) :: filename
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: eff_reset
    call integration_results_write_driver &
         (process%mci_entry(i_mci)%results, filename, eff_reset)
    call integration_results_compile_driver &
         (process%mci_entry(i_mci)%results, filename, os_data)
  end subroutine process_display_integration_history

  subroutine process_write_logfile (process, i_mci, filename)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(string_t), intent(in) :: filename
    type(time_t) :: time
    integer :: unit, u
    unit = free_unit ()
    open (unit = unit, file = char (filename), action = "write", &
          status = "replace")
    u = given_output_unit (unit)
    write (u, "(A)")  repeat ("#", 79)
    call process%meta%write (u, .false., .false.)
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(3x,A,ES17.10)")  "Integral   = ", &
         process%mci_entry(i_mci)%get_integral ()
    write (u, "(3x,A,ES17.10)")  "Error      = ", &
         process%mci_entry(i_mci)%get_error ()
    write (u, "(3x,A,ES17.10)")  "Accuracy   = ", &
         process%mci_entry(i_mci)%get_accuracy ()
    write (u, "(3x,A,ES17.10)")  "Chi2       = ", &
         process%mci_entry(i_mci)%get_chi2 ()
    write (u, "(3x,A,ES17.10)")  "Efficiency = ", &
         process%mci_entry(i_mci)%get_efficiency ()
    call process%mci_entry(i_mci)%get_time (time, 10000)
    if (time%is_known ()) then
       write (u, "(3x,A,1x,A)")  "T(10k evt) = ", char (time%to_string_dhms ())
    else
       write (u, "(3x,A)")  "T(10k evt) =  [undefined]"
    end if
    call process%mci_entry(i_mci)%results%write (u)
    write (u, "(A)")  repeat ("#", 79)
    call process%mci_entry(i_mci)%results%write_chain_weights (u)
    write (u, "(A)")  repeat ("#", 79)
    call process%mci_entry(i_mci)%counter%write (u)
    write (u, "(A)")  repeat ("#", 79)
    call process%mci_entry(i_mci)%mci%write_log_entry (u)
    write (u, "(A)")  repeat ("#", 79)
    call process%beam_config%data%write (u)
    write (u, "(A)")  repeat ("#", 79)
    if (allocated (process%config%ef_cuts)) then
       write (u, "(3x,A)") "Cut expression:"
       call process%config%ef_cuts%write (u)
    else
       write (u, "(3x,A)") "No cuts used."
    end if
    call write_separator (u)
    if (allocated (process%config%ef_scale)) then
       write (u, "(3x,A)") "Scale expression:"
       call process%config%ef_scale%write (u)
    else
       write (u, "(3x,A)") "No scale expression was given."
    end if
    call write_separator (u)
    if (allocated (process%config%ef_fac_scale)) then
       write (u, "(3x,A)") "Factorization scale expression:"
       call process%config%ef_fac_scale%write (u)
    else
       write (u, "(3x,A)") "No factorization scale expression was given."
    end if
    call write_separator (u)
    if (allocated (process%config%ef_ren_scale)) then
       write (u, "(3x,A)") "Renormalization scale expression:"
       call process%config%ef_ren_scale%write (u)
    else
       write (u, "(3x,A)") "No renormalization scale expression was given."
    end if
    call write_separator (u)
    if (allocated (process%config%ef_weight)) then
       call write_separator (u)
       write (u, "(3x,A)") "Weight expression:"
       call process%config%ef_weight%write (u)
    else
       write (u, "(3x,A)") "No weight expression was given."
    end if
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(1x,A)") "Summary of quantum-number states:"
    write (u, "(1x,A)")  " + sign: allowed and contributing"
    write (u, "(1x,A)")  " no +  : switched off at runtime"
    call process%write_state_summary (u)
    write (u, "(A)")  repeat ("#", 79)
    write (u, "(A)")  "Variable list:"
    call var_list_write (process%meta%var_list, u)
    write (u, "(A)")  repeat ("#", 79)
    close (u)
  end subroutine process_write_logfile

  subroutine process_write_state_summary (process, unit)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    integer :: i, i_component, u
    u = given_output_unit (unit)
    do i = 1, size (process%term)
       call write_separator (u)
       i_component = process%term(i)%i_component
       if (i_component /= 0) then
          call process%term(i)%write_state_summary &
               (process%get_core_term(i), unit)
       end if
    end do
  end subroutine process_write_state_summary

  subroutine process_prepare_simulation (process, i_mci)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    call process%mci_entry(i_mci)%prepare_simulation ()
  end subroutine process_prepare_simulation

  function process_has_integral_mci (process, i_mci) result (flag)
    logical :: flag
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    if (allocated (process%mci_entry)) then
       flag = process%mci_entry(i_mci)%has_integral ()
    else
       flag = .false.
    end if
  end function process_has_integral_mci

  function process_has_integral_tot (process) result (flag)
    logical :: flag
    class(process_t), intent(in) :: process
    integer :: i, j, i_component
    if (allocated (process%mci_entry)) then
       flag = .true.
       do i = 1, size (process%mci_entry)
          do j = 1, size (process%mci_entry(i)%i_component)
             i_component = process%mci_entry(i)%i_component(j)
             if (process%component_can_be_integrated (i_component)) &
                flag = flag .and. process%mci_entry(i)%has_integral ()
          end do
       end do
    else
       flag = .false.
    end if
  end function process_has_integral_tot

  function process_get_integral_mci (process, i_mci) result (integral)
    real(default) :: integral
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    integral = process%mci_entry(i_mci)%get_integral ()
  end function process_get_integral_mci

  function process_get_error_mci (process, i_mci) result (error)
    real(default) :: error
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    error = process%mci_entry(i_mci)%get_error ()
  end function process_get_error_mci

  function process_get_efficiency_mci (process, i_mci) result (efficiency)
    real(default) :: efficiency
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    efficiency = process%mci_entry(i_mci)%get_efficiency ()
  end function process_get_efficiency_mci

  function process_get_integral_tot (process) result (integral)
    real(default) :: integral
    class(process_t), intent(in) :: process
    integer :: i, j, i_component
    integral = zero
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          do j = 1, size (process%mci_entry(i)%i_component)
             i_component = process%mci_entry(i)%i_component(j)
             if (process%component_can_be_integrated(i_component)) &
                  integral = integral + process%mci_entry(i)%get_integral ()
          end do
       end do
    end if
  end function process_get_integral_tot

  function process_get_error_tot (process) result (error)
    real(default) :: variance
    class(process_t), intent(in) :: process
    real(default) :: error
    integer :: i, j, i_component
    variance = zero
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          do j = 1, size (process%mci_entry(i)%i_component)
             i_component = process%mci_entry(i)%i_component(j)
             if (process%component_can_be_integrated(i_component)) &
                  variance = variance + process%mci_entry(i)%get_error () ** 2
          end do
       end do
    end if
    error = sqrt (variance)
  end function process_get_error_tot

  function process_get_efficiency_tot (process) result (efficiency)
    real(default) :: efficiency
    class(process_t), intent(in) :: process
    real(default) :: den, eff, int
    integer :: i, j, i_component
    den = zero
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          do j = 1, size (process%mci_entry(i)%i_component)
             i_component = process%mci_entry(i)%i_component(j)
             if (process%component_can_be_integrated(i_component)) then
                int = process%get_integral (i)
                if (int > 0) then
                   eff = process%mci_entry(i)%get_efficiency ()
                   if (eff > 0) then
                      den = den + int / eff
                   else
                      efficiency = 0
                      return
                   end if
                end if
             end if
          end do
       end do
    end if
    if (den > 0) then
       efficiency = process%get_integral () / den
    else
       efficiency = 0
    end if
  end function process_get_efficiency_tot

  function process_get_correction (process) result (ratio)
    real(default) :: ratio
    class(process_t), intent(in) :: process
    integer :: i_mci
    real(default) :: int_born, int_nlo
    int_nlo = zero
    int_born = process%mci_entry(1)%get_integral ()
    do i_mci = 2, size (process%mci_entry)
       if (process%component_can_be_integrated (i_mci)) &
          int_nlo = int_nlo + process%mci_entry(i_mci)%get_integral ()
    end do
    ratio = int_nlo / int_born * 100
  end function process_get_correction

  function process_get_correction_error (process) result (error)
    real(default) :: error
    class(process_t), intent(in) :: process
    real(default) :: int_born, sum_int_nlo
    real(default) :: err_born, err2
    integer :: i_mci
    sum_int_nlo = zero; err2 = zero
    int_born = process%mci_entry(1)%get_integral ()
    err_born = process%mci_entry(1)%get_error ()
    do i_mci = 2, size (process%mci_entry)
       if (process%component_can_be_integrated (i_mci)) then
          sum_int_nlo = sum_int_nlo + process%mci_entry(i_mci)%get_integral ()
          err2 = err2 + process%mci_entry(i_mci)%get_error()**2
       end if
    end do
    error = sqrt (err2 / int_born**2 + sum_int_nlo**2 * err_born**2 / int_born**4) * 100
  end function process_get_correction_error

  pure function process_lab_is_cm_frame (process) result (cm_frame)
    logical :: cm_frame
    class(process_t), intent(in) :: process
    cm_frame = process%beam_config%lab_is_cm_frame
  end function process_lab_is_cm_frame

  function process_get_component_ptr (process, i) result (component)
    type(process_component_t), pointer :: component
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i
    component => process%component(i)
  end function process_get_component_ptr

  function process_get_qcd_ptr (process) result (qcd)
    type(qcd_t), pointer :: qcd
    class(process_t), intent(in), target :: process
    qcd => process%config%qcd
  end function process_get_qcd_ptr

  elemental function process_get_component_type_single &
     (process, i_component) result (comp_type)
    integer :: comp_type
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    comp_type = process%component(i_component)%component_type
  end function process_get_component_type_single

  function process_get_component_type_all &
     (process) result (comp_type)
    integer, dimension(:), allocatable :: comp_type
    class(process_t), intent(in) :: process
    allocate (comp_type (size (process%component)))
    comp_type = process%component%component_type
  end function process_get_component_type_all

  function process_get_component_i_terms (process, i_component) result (i_term)
     integer, dimension(:), allocatable :: i_term
     class(process_t), intent(in) :: process
     integer, intent(in) :: i_component
     allocate (i_term (size (process%component(i_component)%i_term)))
     i_term = process%component(i_component)%i_term
  end function process_get_component_i_terms

  function process_get_n_allowed_born (process, i_born) result (n_born)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_born
    integer :: n_born
    n_born = process%term(i_born)%n_allowed
  end function process_get_n_allowed_born

  function process_get_pcm_ptr (process) result (pcm)
    class(pcm_t), pointer :: pcm
    class(process_t), intent(in), target :: process
    pcm => process%pcm
  end function process_get_pcm_ptr

  function process_component_can_be_integrated_single (process, i_component) &
           result (active)
    logical :: active
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    logical :: combined_integration
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       combined_integration = pcm%settings%combined_integration
    class default
       combined_integration = .false.
    end select
    associate (component => process%component(i_component))
       active = component%can_be_integrated ()
       if (combined_integration) &
            active = active .and. component%component_type <= COMP_MASTER
    end associate
  end function process_component_can_be_integrated_single

  function process_component_can_be_integrated_all (process) result (val)
    logical, dimension(:), allocatable :: val
    class(process_t), intent(in) :: process
    integer :: i
    allocate (val (size (process%component)))
    do i = 1, size (process%component)
       val(i) = process%component_can_be_integrated (i)
    end do
  end function process_component_can_be_integrated_all

  pure subroutine process_reset_selected_cores (process)
    class(process_t), intent(inout) :: process
    process%component_selected = .false.
  end subroutine process_reset_selected_cores

  pure subroutine process_select_components (process, indices)
    class(process_t), intent(inout) :: process
    integer, dimension(:), intent(in) :: indices
    process%component_selected(indices) = .true.
  end subroutine process_select_components

  pure function process_component_is_selected (process, index) result (val)
    logical :: val
    class(process_t), intent(in) :: process
    integer, intent(in) :: index
    val = process%component_selected(index)
  end function process_component_is_selected

  pure subroutine process_get_coupling_powers (process, alpha_power, alphas_power)
    class(process_t), intent(in) :: process
    integer, intent(out) :: alpha_power, alphas_power
    call process%component(1)%config%get_coupling_powers (alpha_power, alphas_power)
  end subroutine process_get_coupling_powers

  function process_get_real_component (process) result (i_real)
    integer :: i_real
    class(process_t), intent(in) :: process
    integer :: i_component
    type(process_component_def_t), pointer :: config => null ()
    i_real = 0
    do i_component = 1, size (process%component)
       config => process%get_component_def_ptr (i_component)
       if (config%get_nlo_type () == NLO_REAL) then
          i_real = i_component
          exit
       end if
    end do
  end function process_get_real_component

  function process_extract_active_component_mci (process) result (i_active)
    integer :: i_active
    class(process_t), intent(in) :: process
    integer :: i_mci, j, i_component, n_active
    call count_n_active ()
    if (n_active /= 1) i_active = 0
  contains
    subroutine count_n_active ()
       n_active = 0
       do i_mci = 1, size (process%mci_entry)
          associate (mci_entry => process%mci_entry(i_mci))
             do j = 1, size (mci_entry%i_component)
                i_component = mci_entry%i_component(j)
                associate (component => process%component (i_component))
                   if (component%can_be_integrated ()) then
                      i_active = i_mci
                      n_active = n_active + 1
                   end if
                end associate
             end do
          end associate
       end do
    end subroutine count_n_active
  end function process_extract_active_component_mci

  function process_needs_extra_code (process, only_blha) result (val)
    logical :: val
    class(process_t), intent(in) :: process
    logical, intent(in), optional :: only_blha
    integer :: i
    logical :: skip_other
    type(process_component_def_t), pointer :: config => null ()
    val = .false.; skip_other = .false.
    if (present (only_blha)) skip_other = only_blha
    associate (cm => process%cm)
       do i = 1, cm%n_cores
          config => process%get_component_def_ptr &
               (cm%i_core_to_first_i_component(i))
          if (config%can_be_integrated () .or. cm%sub(i)) then
             select type (core => cm%cores(i)%core)
             type is (prc_recola_t)
                if (skip_other) cycle
                val = .true.
                exit
             class is (prc_blha_t)
                val = .true.
                exit
             class is (prc_threshold_t)
                if (skip_other) cycle
                val = .true.
                exit
             end select
          end if
       end do
    end associate

  end function process_needs_extra_code

  function process_uses_real_partition (process) result (val)
     logical :: val
     class(process_t), intent(in) :: process
     val = any (process%mci_entry%real_partition_type /= REAL_FULL)
  end function process_uses_real_partition

  function process_get_md5sum_prc (process, i_component) result (md5sum)
    character(32) :: md5sum
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    if (process%component(i_component)%active) then
       md5sum = process%component(i_component)%config%get_md5sum ()
    else
       md5sum = ""
    end if
  end function process_get_md5sum_prc

  function process_get_md5sum_mci (process, i_mci) result (md5sum)
    character(32) :: md5sum
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    md5sum = process%mci_entry(i_mci)%get_md5sum ()
  end function process_get_md5sum_mci

  function process_get_md5sum_cfg (process) result (md5sum)
    character(32) :: md5sum
    class(process_t), intent(in) :: process
    md5sum = process%config%md5sum
  end function process_get_md5sum_cfg

  subroutine process_init_cores (process)
    class(process_t), intent(inout) :: process
    integer :: i_core, i_component
    type(process_component_def_t), pointer :: config
    do i_core = 1, process%get_n_cores ()
       i_component = process%cm%i_core_to_first_i_component (i_core)
       config => process%meta%lib%get_component_def_ptr (process%meta%id, i_component)
       associate (core => process%cm%cores(i_core)%core)
          call core%init (config%get_core_def_ptr (), &
               process%meta%lib, process%meta%id, i_component)
       end associate
    end do
  end subroutine process_init_cores

  subroutine process_init_blha_cores (process, blha_template, var_list)
    class(process_t), intent(inout) :: process
    type(blha_template_t), intent(inout) :: blha_template
    type(var_list_t), intent(in), pointer :: var_list
    integer :: i_core, n_in, n_legs, n_flv, n_hel, f
    type(flavor_t) :: flv_in
    do i_core = 1, process%get_n_cores ()
       call fill_blha_template (process%get_nlo_type (i_core))
       select type (core => process%cm%cores(i_core)%core)
       class is (prc_blha_t)
          select type (pcm => process%pcm)
          type is (pcm_nlo_t)
             n_in = pcm%region_data%get_n_in ()
             if (process%cm%core_is_radiation(i_core)) then
                n_legs = pcm%region_data%get_n_legs_real ()
                n_flv = pcm%region_data%get_n_flv_real ()
             else
                n_legs = pcm%region_data%get_n_legs_born ()
                n_flv = pcm%region_data%get_n_flv_born ()
             end if
          class default
             n_in = core%data%n_in
             n_legs = core%data%get_n_tot ()
             n_flv = core%data%n_flv
          end select
          n_hel = 1
          if (blha_template%include_polarizations) then
             do f = 1, core%data%n_in
                call flv_in%init (core%data%flv_state (f, 1), process%config%model)
                n_hel = n_hel * flv_in%get_multiplicity ()
             end do
          end if
          call core%init_blha (blha_template, n_in, n_legs, n_flv, n_hel)
          call core%init_driver (process%config%os_data)
       end select
       call blha_template%reset ()
    end do
  contains
    function needs_entry (me_method) result (val)
      logical :: val
      type(string_t), intent(in) :: me_method
      val = char (me_method) == 'gosam' .or. char (me_method) == 'openloops'
    end function needs_entry

    subroutine fill_blha_template (nlo_type)
      integer, intent(in) :: nlo_type
      type(string_t) :: method, born_me_method, real_tree_me_method, &
           loop_me_method, correlation_me_method, dglap_me_method
      method = var_list%get_sval (var_str ("$method"))
      born_me_method = var_list%get_sval (var_str ("$born_me_method"))
      if (born_me_method == "")  born_me_method = method
      real_tree_me_method = var_list%get_sval (var_str ("$real_tree_me_method"))
      if (real_tree_me_method == "")  real_tree_me_method = method
      loop_me_method = var_list%get_sval (var_str ("$loop_me_method"))
      if (loop_me_method == "")  loop_me_method = method
      correlation_me_method = var_list%get_sval (var_str ("$correlation_me_method"))
      if (correlation_me_method == "")  correlation_me_method = method
      dglap_me_method = var_list%get_sval (var_str ("$dglap_me_method"))
      if (dglap_me_method == "")  dglap_me_method = method
      call msg_debug2 (D_PROCESS_INTEGRATION, &
           "process_init_blha_cores: method = ", method)
      call msg_debug2 (D_PROCESS_INTEGRATION, &
           "process_init_blha_cores: method = ", born_me_method)
      call msg_debug2 (D_PROCESS_INTEGRATION, &
           "process_init_blha_cores: method = ", loop_me_method)
      call msg_debug2 (D_PROCESS_INTEGRATION, &
           "process_init_blha_cores: method = ", correlation_me_method)
      call msg_debug2 (D_PROCESS_INTEGRATION, &
           "process_init_blha_cores: method = ", dglap_me_method)
      select case (nlo_type)
      case (BORN)
         if (needs_entry (method) .or. needs_entry (born_me_method)) &
            call blha_template%set_born ()
      case (NLO_REAL)
         if (needs_entry (real_tree_me_method)) &
            call blha_template%set_real_trees ()
      case (NLO_VIRTUAL)
         if (needs_entry (loop_me_method)) &
            call blha_template%set_loop ()
      case (NLO_SUBTRACTION)
         if (needs_entry (correlation_me_method)) then
            call blha_template%set_subtraction ()
            call blha_template%set_internal_color_correlations ()
         end if
      case (NLO_DGLAP)
         if (needs_entry (dglap_me_method)) then
            call blha_template%set_dglap ()
         end if
      end select
    end subroutine fill_blha_template
  end subroutine process_init_blha_cores

  function process_get_n_cores (process) result (n)
    integer :: n
    class(process_t), intent(in) :: process
    n = process%cm%n_cores
  end function process_get_n_cores

  function process_get_core_manager_index (process, i_core) result (i)
    integer :: i
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    i = process%cm%i_core_to_first_i_component (i_core)
  end function process_get_core_manager_index

  function process_get_core_manager (process) result (cm)
    type(core_manager_t) :: cm
    class(process_t), intent(in) :: process
    cm = process%cm
  end function process_get_core_manager

  function process_get_core_manager_ptr (process) result (cm)
    type(core_manager_t), pointer :: cm
    class(process_t), intent(in), target :: process
    cm => process%cm
  end function process_get_core_manager_ptr

  function process_get_base_i_term (process, i_component) result (i_term)
    integer :: i_term
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    i_term = process%component(i_component)%i_term(1)
  end function process_get_base_i_term

  function process_get_core_term (process, i_term) result (core)
    class(prc_core_t), pointer :: core
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_term
    integer :: i_core
    i_core = process%term(i_term)%i_core
    core => process%cm%cores(i_core)%core
  end function process_get_core_term

  function process_get_subtraction_core (process) result (core)
    class(prc_core_t), pointer :: core
    class(process_t), intent(in), target :: process
    core => process%cm%get_subtraction_core ()
  end function process_get_subtraction_core

  function process_get_term_ptr (process, i) result (term)
    type(process_term_t), pointer :: term
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i
    term => process%term(i)
  end function process_get_term_ptr

  function process_get_core_from_md5sum (process, md5sum) result (core)
    class(prc_core_t), pointer :: core
    class(process_t), intent(in), target :: process
    character(32), intent(in) :: md5sum
    integer :: i_core
    associate (cm => process%cm)
       do i_core = 1, N_MAX_CORES
          if (cm%md5s(i_core) == md5sum) exit
       end do
       core => cm%cores(i_core)%core
    end associate
  end function process_get_core_from_md5sum

  function process_get_i_core_nlo_type (process, nlo_type, include_sub) result (i_core)
    integer :: i_core
    class(process_t), intent(in) :: process
    integer, intent(in) :: nlo_type
    logical, intent(in), optional :: include_sub
    logical :: skip_sub
    skip_sub = .false.
    if (present (include_sub)) skip_sub = .not. include_sub
    do i_core = 1, N_MAX_CORES
       if (skip_sub) then
          if (process%cm%sub(i_core)) cycle
       end if
       if (process%cm%nlo_type (i_core) == nlo_type) return
    end do
    i_core = -1
  end function process_get_i_core_nlo_type

  function process_get_i_term (process, i_core) result (i_term)
    integer :: i_term
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    do i_term = 1, process%get_n_terms ()
       if (process%term(i_term)%i_core == i_core) return
    end do
    i_term = -1
  end function process_get_i_term

  subroutine process_set_i_mci_work (process, i_mci)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    process%mci_entry(i_mci)%i_mci = i_mci
  end subroutine process_set_i_mci_work

  pure function process_get_i_mci_work (process, i_mci) result (i_mci_work)
    integer :: i_mci_work
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    i_mci_work = process%mci_entry(i_mci)%i_mci
  end function process_get_i_mci_work

  elemental function process_get_i_sub (process, i_term) result (i_sub)
    integer :: i_sub
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_term
    i_sub = process%term(i_term)%i_sub
  end function process_get_i_sub

  elemental function process_get_i_term_virtual (process) result (i_term)
    integer :: i_term
    class(process_t), intent(in) :: process
    integer :: i_component
    i_term = 0
    do i_component = 1, size (process%component)
       if (process%component(i_component)%get_nlo_type () == NLO_VIRTUAL) &
            i_term = process%component(i_component)%i_term(1)
    end do
  end function process_get_i_term_virtual

  elemental function process_component_is_active_single (process, i_comp) result (val)
    logical :: val
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_comp
    val = process%component(i_comp)%is_active ()
  end function process_component_is_active_single

  pure function process_component_is_active_all (process) result (val)
    logical, dimension(:), allocatable :: val
    class(process_t), intent(in) :: process
    allocate (val (size (process%component)))
    val = process%component%is_active ()
  end function process_component_is_active_all

  function process_get_n_pass_default (process) result (n_pass)
    class(process_t), intent(in) :: process
    integer :: n_pass
    integer :: n_eff
    type(process_component_def_t), pointer :: config
    config => process%component(1)%config
    n_eff = config%get_n_tot () - 2
    select case (n_eff)
    case (1)
       n_pass = 1
    case default
       n_pass = 2
    end select
  end function process_get_n_pass_default

  function process_adapt_grids_default (process, pass) result (flag)
    class(process_t), intent(in) :: process
    integer, intent(in) :: pass
    logical :: flag
    integer :: n_eff
    type(process_component_def_t), pointer :: config
    config => process%component(1)%config
    n_eff = config%get_n_tot () - 2
    select case (n_eff)
    case (1)
       flag = .false.
    case default
       select case (pass)
       case (1);  flag = .true.
       case (2);  flag = .false.
       case default
          call msg_bug ("adapt grids default: impossible pass index")
       end select
    end select
  end function process_adapt_grids_default

  function process_adapt_weights_default (process, pass) result (flag)
    class(process_t), intent(in) :: process
    integer, intent(in) :: pass
    logical :: flag
    integer :: n_eff
    type(process_component_def_t), pointer :: config
    config => process%component(1)%config
    n_eff = config%get_n_tot () - 2
    select case (n_eff)
    case (1)
       flag = .false.
    case default
       select case (pass)
       case (1);  flag = .true.
       case (2);  flag = .false.
       case default
          call msg_bug ("adapt weights default: impossible pass index")
       end select
    end select
  end function process_adapt_weights_default

  function process_get_n_it_default (process, pass) result (n_it)
    class(process_t), intent(in) :: process
    integer, intent(in) :: pass
    integer :: n_it
    integer :: n_eff
    type(process_component_def_t), pointer :: config
    config => process%component(1)%config
    n_eff = config%get_n_tot () - 2
    select case (pass)
    case (1)
       select case (n_eff)
       case (1);   n_it = 1
       case (2);   n_it = 3
       case (3);   n_it = 5
       case (4:5); n_it = 10
       case (6);   n_it = 15
       case (7:);  n_it = 20
       end select
    case (2)
       select case (n_eff)
       case (:3);   n_it = 3
       case (4:);   n_it = 5
       end select
    end select
  end function process_get_n_it_default

  function process_get_n_calls_default (process, pass) result (n_calls)
    class(process_t), intent(in) :: process
    integer, intent(in) :: pass
    integer :: n_calls
    integer :: n_eff
    type(process_component_def_t), pointer :: config
    config => process%component(1)%config
    n_eff = config%get_n_tot () - 2
    select case (pass)
    case (1)
       select case (n_eff)
       case (1);   n_calls =   100
       case (2);   n_calls =  1000
       case (3);   n_calls =  5000
       case (4);   n_calls = 10000
       case (5);   n_calls = 20000
       case (6:);  n_calls = 50000
       end select
    case (2)
       select case (n_eff)
       case (:3);  n_calls =  10000
       case (4);   n_calls =  20000
       case (5);   n_calls =  50000
       case (6);   n_calls = 100000
       case (7:);  n_calls = 200000
       end select
    end select
  end function process_get_n_calls_default

  function process_get_id (process) result (id)
    class(process_t), intent(in) :: process
    type(string_t) :: id
    id = process%meta%id
  end function process_get_id

  function process_get_num_id (process) result (id)
    class(process_t), intent(in) :: process
    integer :: id
    id = process%meta%num_id
  end function process_get_num_id

  function process_get_run_id (process) result (id)
    class(process_t), intent(in) :: process
    type(string_t) :: id
    id = process%meta%run_id
  end function process_get_run_id

  function process_get_library_name (process) result (id)
    class(process_t), intent(in) :: process
    type(string_t) :: id
    id = process%meta%lib%get_name ()
  end function process_get_library_name

  function process_get_n_in (process) result (n)
    class(process_t), intent(in) :: process
    integer :: n
    n = process%config%n_in
  end function process_get_n_in

  function process_get_n_mci (process) result (n)
    class(process_t), intent(in) :: process
    integer :: n
    n = process%config%n_mci
  end function process_get_n_mci

  function process_get_n_components (process) result (n)
    class(process_t), intent(in) :: process
    integer :: n
    n = process%meta%n_components
  end function process_get_n_components

  function process_get_n_terms (process) result (n)
    class(process_t), intent(in) :: process
    integer :: n
    n = process%config%n_terms
  end function process_get_n_terms

  subroutine process_get_i_component (process, i_mci, i_component)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    integer, dimension(:), intent(out), allocatable :: i_component
    associate (mci_entry => process%mci_entry(i_mci))
      allocate (i_component (size (mci_entry%i_component)))
      i_component = mci_entry%i_component
    end associate
  end subroutine process_get_i_component

  function process_get_component_id (process, i_component) result (id)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    type(string_t) :: id
    id = process%meta%component_id(i_component)
  end function process_get_component_id

  function process_get_component_def_ptr (process, i_component) result (ptr)
    type(process_component_def_t), pointer :: ptr
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    ptr => process%meta%lib%get_component_def_ptr (process%meta%id, i_component)
  end function process_get_component_def_ptr

  subroutine process_extract_core (process, i_term, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_term
    class(prc_core_t), intent(inout), allocatable :: core
    integer :: i_core
    i_core = process%term(i_term)%i_core
    call move_alloc (from = process%cm%cores(i_core)%core, to = core)
  end subroutine process_extract_core

  subroutine process_restore_core (process, i_term, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_term
    class(prc_core_t), intent(inout), allocatable :: core
    integer :: i_core
    i_core = process%term(i_term)%i_core
    call move_alloc (from = core, to = process%cm%cores(i_core)%core)
  end subroutine process_restore_core

  function process_get_constants (process, i_core) result (data)
    type(process_constants_t) :: data
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    data = process%cm%cores(i_core)%core%data
  end function process_get_constants

  function process_get_config (process) result (config)
    type(process_config_data_t) :: config
    class(process_t), intent(in) :: process
    config = process%config
  end function process_get_config

  function process_get_md5sum_constants (process, i_component, &
     type_string, nlo_type) result (this_md5sum)
    character(32) :: this_md5sum
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    type(string_t), intent(in) :: type_string
    integer, intent(in) :: nlo_type
    type(process_constants_t) :: data
    integer :: unit
    call process%meta%lib%fill_constants (process%meta%id, i_component, data)
    unit = data%fill_unit_for_md5sum (.false.)
    write (unit, '(A)') char(type_string)
    write (unit, '(I0)') nlo_type
    rewind (unit)
    this_md5sum = md5sum (unit)
    close (unit)
  end function process_get_md5sum_constants

  subroutine process_get_term_flv_out (process, i_term, flv)
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_term
    type(flavor_t), dimension(:,:), allocatable, intent(out) :: flv
    type(interaction_t), pointer :: int
    int => process%term(i_term)%int_eff
    if (.not. associated (int))  int => process%term(i_term)%int
    call interaction_get_flv_out (int, flv)
  end subroutine process_get_term_flv_out

  function process_contains_unstable (process, model) result (flag)
    class(process_t), intent(in) :: process
    class(model_data_t), intent(in), target :: model
    logical :: flag
    integer :: i_term
    type(flavor_t), dimension(:,:), allocatable :: flv
    flag = .false.
    do i_term = 1, process%get_n_terms ()
       call process%get_term_flv_out (i_term, flv)
       call flv%set_model (model)
       flag = .not. all (flv%is_stable ())
       deallocate (flv)
       if (flag)  return
    end do
  end function process_contains_unstable

  function process_get_sqrts (process) result (sqrts)
    class(process_t), intent(in) :: process
    real(default) :: sqrts
    sqrts = process%beam_config%data%get_sqrts ()
  end function process_get_sqrts

  function process_get_polarization (process) result (pol)
    class(process_t), intent(in) :: process
    real(default), dimension(2) :: pol
    pol = process%beam_config%data%get_polarization ()
  end function process_get_polarization

  function process_get_meta (process) result (meta)
    type(process_metadata_t) :: meta
    class(process_t), intent(in) :: process
    meta = process%meta
  end function process_get_meta

  function process_has_matrix_element (process, i, is_term_index) result (active)
    logical :: active
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: i
    logical, intent(in), optional :: is_term_index
    integer :: i_component
    logical :: is_term
    is_term = .false.
    if (present (i)) then
       if (present (is_term_index)) is_term = is_term_index
       if (is_term) then
          i_component = process%term(i)%i_component
       else
          i_component = i
       end if
       active = process%component(i_component)%active
    else
       active = any (process%component%active)
    end if
  end function process_has_matrix_element

  function process_get_beam_data_ptr (process) result (beam_data)
    class(process_t), intent(in), target :: process
    type(beam_data_t), pointer :: beam_data
    beam_data => process%beam_config%data
  end function process_get_beam_data_ptr

  function process_get_beam_config (process) result (beam_config)
    type(process_beam_config_t) :: beam_config
    class(process_t), intent(in) :: process
    beam_config = process%beam_config
  end function process_get_beam_config

  function process_get_beam_config_ptr (process) result (beam_config)
    type(process_beam_config_t), pointer :: beam_config
    class(process_t), intent(in), target :: process
    beam_config => process%beam_config
  end function process_get_beam_config_ptr

  function process_cm_frame (process) result (flag)
    class(process_t), intent(in), target :: process
    logical :: flag
    type(beam_data_t), pointer :: beam_data
    beam_data => process%beam_config%data
    flag = beam_data%cm_frame ()
  end function process_cm_frame

  function process_get_pdf_set (process) result (pdf_set)
    class(process_t), intent(in) :: process
    integer :: pdf_set
    pdf_set = process%beam_config%get_pdf_set ()
  end function process_get_pdf_set

  function process_pcm_contains_pdfs (process) result (has_pdfs)
    logical :: has_pdfs
    class(process_t), intent(in) :: process
    has_pdfs = process%pcm%has_pdfs
  end function process_pcm_contains_pdfs

  function process_get_beam_file (process) result (file)
    class(process_t), intent(in) :: process
    type(string_t) :: file
    file = process%beam_config%get_beam_file ()
  end function process_get_beam_file

  function process_get_var_list_ptr (process) result (ptr)
    class(process_t), intent(in), target :: process
    type(var_list_t), pointer :: ptr
    ptr => process%meta%var_list
  end function process_get_var_list_ptr

  function process_get_model_ptr (process) result (ptr)
    class(process_t), intent(in) :: process
    class(model_data_t), pointer :: ptr
    ptr => process%config%model
  end function process_get_model_ptr

  subroutine process_make_rng (process, rng)
    class(process_t), intent(inout) :: process
    class(rng_t), intent(out), allocatable :: rng
    if (allocated (process%config%rng_factory)) then
       call process%config%rng_factory%make (rng)
    else
       call msg_bug ("Process: make rng: factory not allocated")
    end if
  end subroutine process_make_rng

  function process_compute_amplitude &
       (process, i_core, i, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced) &
       result (amp)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    integer, intent(in) :: i, j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in), optional :: fac_scale, ren_scale
    real(default), intent(in), allocatable, optional :: alpha_qcd_forced
    class(prc_core_t), pointer :: core => null ()
    real(default) :: fscale, rscale
    real(default), allocatable :: aqcd_forced
    complex(default) :: amp
    amp = 0
    if (0 < i .and. i <= process%meta%n_components) then
       core => process%cm%get_core(i_core)
       if (process%component(i)%active) then
          !associate (data => process%component(i)%core%data)
          associate (data => core%data)
            if (size (p) == data%n_in + data%n_out &
                 .and. 0 < f .and. f <= data%n_flv &
                 .and. 0 < h .and. h <= data%n_hel &
                 .and. 0 < c .and. c <= data%n_col) then
               if (present (fac_scale)) then
                  fscale = fac_scale
               else
                  fscale = sum (p(data%n_in+1:)) ** 1
               end if
               if (present (ren_scale)) then
                  rscale = ren_scale
               else
                  rscale = fscale
               end if
               if (present (alpha_qcd_forced)) then
                  if (allocated (alpha_qcd_forced)) &
                       allocate (aqcd_forced, source = alpha_qcd_forced)
               end if
               amp = core%compute_amplitude (j, p, f, h, c, &
                  fscale, rscale, aqcd_forced)
            end if
          end associate
       else
          amp = 0
       end if
    end if
  end function process_compute_amplitude

  subroutine process_check_library_sanity (process)
    class(process_t), intent(in) :: process
    if (associated (process%meta%lib)) then
       if (process%meta%lib%get_update_counter () /= process%meta%lib_update_counter) then
          call msg_fatal ("Process '" // char (process%get_id ()) &
               // "': library has been recompiled after integration")
       end if
    end if
  end subroutine process_check_library_sanity

  subroutine process_nullify_library_pointer (process)
    class(process_t), intent(inout) :: process
    process%meta%lib => null ()
  end subroutine process_nullify_library_pointer

  subroutine process_set_component_type (process, i_component, i_type)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_component, i_type
    process%component(i_component)%component_type = i_type
  end subroutine process_set_component_type

  subroutine process_set_counter_mci_entry (process, i_mci, counter)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    type(process_counter_t), intent(in) :: counter
    process%mci_entry(i_mci)%counter = counter
  end subroutine process_set_counter_mci_entry

  subroutine process_pacify (process, efficiency_reset, error_reset)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: efficiency_reset, error_reset
    logical :: eff_reset, err_reset
    integer :: i
    eff_reset = .false.
    err_reset = .false.
    if (present (efficiency_reset))  eff_reset = efficiency_reset
    if (present (error_reset))  err_reset = error_reset
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          call process%mci_entry(i)%results%pacify (efficiency_reset)
          if (allocated (process%mci_entry(i)%mci)) then
             associate (mci => process%mci_entry(i)%mci)
               if (process%mci_entry(i)%mci%error_known &
                    .and. err_reset) &
                    mci%error = 0
               if (process%mci_entry(i)%mci%efficiency_known &
                    .and. eff_reset)  &
                    mci%efficiency = 1
               call mci%pacify (efficiency_reset, error_reset)
               call mci%compute_md5sum ()
             end associate
          end if
       end do
    end if
  end subroutine process_pacify

  subroutine test_allocate_sf_channels (process, n)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: n
    call process%beam_config%allocate_sf_channels (n)
  end subroutine test_allocate_sf_channels

  subroutine test_set_component_sf_channel (process, c)
    class(process_t), intent(inout) :: process
    integer, dimension(:), intent(in) :: c
    call process%component(1)%phs_config%set_sf_channel (c)
  end subroutine test_set_component_sf_channel

  subroutine test_get_mci_ptr (process, mci)
    class(process_t), intent(in), target :: process
    class(mci_t), intent(out), pointer :: mci
    mci => process%mci_entry(1)%mci
  end subroutine test_get_mci_ptr

  subroutine process_init_mci_work (process, mci_work, i)
    class(process_t), intent(in), target :: process
    type(mci_work_t), intent(out) :: mci_work
    integer, intent(in) :: i
    call mci_work%init (process%mci_entry(i))
  end subroutine process_init_mci_work

  subroutine process_setup_test_cores (process, type_string)
    class(process_t), intent(inout) :: process
    class(prc_core_t), allocatable :: core
    type(string_t), intent(in), optional :: type_string
    allocate (test_t :: core)
    if (present (type_string)) then
       call process%core_manager_register (BORN, 1, type_string)
    else
       call process%core_manager_register (BORN, 1, var_str ("test_me"))
    end if
    call process%allocate_cm_arrays (1)
    call process%allocate_core (1, core)
    call process%init_cores ()
  end subroutine process_setup_test_cores

  subroutine process_write_cm (process, unit)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    call process%cm%write (unit)
  end subroutine process_write_cm

  function process_get_connected_states (process, i_component, &
         connected_terms) result (connected)
    type(connected_state_t), dimension(:), allocatable :: connected
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    type(connected_state_t), dimension(:), intent(in) :: connected_terms
    integer :: i, i_conn
    integer :: n_conn
    n_conn = 0
    do i = 1, process%get_n_terms ()
       if (process%term(i)%i_component == i_component) then
          n_conn = n_conn + 1
       end if
    end do
    allocate (connected (n_conn))
    i_conn = 1
    do i = 1, process%get_n_terms ()
       if (process%term(i)%i_component == i_component) then
          connected (i_conn) = connected_terms(i)
          i_conn = i_conn + 1
       end if
    end do
  end function process_get_connected_states

  subroutine process_init_nlo_settings (process, var_list, fks_template)
    class(process_t), intent(inout) :: process
    type(var_list_t), intent(in), target :: var_list
    type(fks_template_t), intent(in), optional :: fks_template
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       call pcm%settings%init (var_list, fks_template)
       if (debug_active (D_SUBTRACTION) .or. debug_active (D_VIRTUAL)) &
              call pcm%settings%write ()
    class default
       call msg_fatal ("Attempt to set nlo_settings with a non-NLO pcm!")
    end select
  end subroutine process_init_nlo_settings

  elemental function process_get_nlo_type (process, i_core) result (nlo_type)
    integer :: nlo_type
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    nlo_type = process%cm%nlo_type(i_core)
  end function process_get_nlo_type

  elemental function process_get_nlo_type_component_single (process, i_component) result (val)
    integer :: val
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    val = process%component(i_component)%get_nlo_type ()
  end function process_get_nlo_type_component_single

  pure function process_get_nlo_type_component_all (process) result (val)
    integer, dimension(:), allocatable :: val
    class(process_t), intent(in) :: process
    allocate (val (size (process%component)))
    val = process%component%get_nlo_type ()
  end function process_get_nlo_type_component_all

  function process_is_nlo_calculation (process) result (nlo)
    logical :: nlo
    class(process_t), intent(in) :: process
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       nlo = .true.
    class default
       nlo = .false.
    end select
  end function process_is_nlo_calculation

  function process_is_combined_nlo_integration (process) result (combined)
    logical :: combined
    class(process_t), intent(in) :: process
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       combined = pcm%settings%combined_integration
    class default
       combined = .false.
    end select
  end function process_is_combined_nlo_integration

  pure function process_component_is_real_finite (process, i_component) &
         result (val)
    logical :: val
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    val = process%component(i_component)%component_type == COMP_REAL_FIN
  end function process_component_is_real_finite

  elemental function process_get_component_nlo_type (process, i_component) &
           result (nlo_type)
    integer :: nlo_type
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    nlo_type = process%component(i_component)%config%get_nlo_type ()
  end function process_get_component_nlo_type

  function process_get_component_associated_born (process, i_component) &
           result (i_born)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    integer :: i_born
    i_born = process%component(i_component)%config%get_associated_born ()
  end function process_get_component_associated_born

  function process_get_first_real_component (process) result (i_real)
     integer :: i_real
     class(process_t), intent(in) :: process
     i_real = process%component(1)%config%get_associated_real ()
  end function process_get_first_real_component

  function process_get_first_real_term (process) result (i_real)
     integer :: i_real
     class(process_t), intent(in) :: process
     integer :: i_component, i_term
     i_component = process%component(1)%config%get_associated_real ()
     i_real = 0
     do i_term = 1, size (process%term)
        if (process%term(i_term)%i_component == i_component) then
           i_real = i_term
           exit
        end if
     end do
     if (i_real == 0) call msg_fatal ("Did not find associated real term!")
  end function process_get_first_real_term

  elemental function process_get_associated_real_fin (process, i_component) result (i_real)
     integer :: i_real
     class(process_t), intent(in) :: process
     integer, intent(in) :: i_component
     i_real = process%component(i_component)%config%get_associated_real_fin ()
  end function process_get_associated_real_fin

  subroutine process_setup_region_data (process, i_real, data_born, data_real)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_real
    type(process_constants_t), intent(in) :: data_born, data_real
    integer, dimension (:,:), allocatable :: flavor_born, flavor_real
    type(resonance_history_t), dimension(:), allocatable :: resonance_histories
    logical :: success
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       call data_born%get_flv_state (flavor_born)
       call data_real%get_flv_state (flavor_real)
       select type (model => process%config%model)
       type is (model_t)
          call pcm%region_data%init (data_born%n_in, model, &
               flavor_born, flavor_real, pcm%settings%nlo_correction_type)
          associate (template => pcm%settings%fks_template)
             if (template%mapping_type == FKS_RESONANCES) then
                select type (phs_config => process%component(i_real)%phs_config)
                type is (phs_fks_config_t)
                   call get_filtered_resonance_histories (phs_config, &
                        data_born%n_in, flavor_born, model, template%excluded_resonances, &
                        resonance_histories, success)
                end select
                if (.not. success) template%mapping_type = FKS_DEFAULT
             end if
             call pcm%region_data%setup_fks_mappings &
                  (pcm%settings%fks_template, data_born%n_in)
             !!! Check again, mapping_type might have changed
             if (template%mapping_type == FKS_RESONANCES) then
                call pcm%region_data%set_resonance_mappings (resonance_histories)
                call pcm%region_data%init_resonance_information ()
                pcm%settings%use_resonance_mappings = .true.
             end if
          end associate
       end select
       if (pcm%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
           call pcm%region_data%set_isr_pseudo_regions ()
           call pcm%region_data%split_up_interference_regions_for_threshold ()
       end if
       call pcm%region_data%compute_number_of_phase_spaces ()
       call pcm%region_data%set_i_phs_to_i_con ()
       associate (var_list => process%meta%var_list)
          call pcm%region_data%write_to_file (process%meta%id, &
               var_list%get_lval (var_str ("?vis_fks_regions")), &
               process%config%os_data)
       end associate
       if (debug_active (D_SUBTRACTION)) call pcm%region_data%check_consistency (.true.)
    end select
  end subroutine process_setup_region_data

  subroutine process_setup_real_partition (process, partition_scale)
    class(process_t), intent(inout) :: process
    real(default), intent(in) :: partition_scale
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       call pcm%setup_real_partition (partition_scale)
    end select
  end subroutine process_setup_real_partition

  subroutine process_check_if_threshold_method (process)
    class(process_t), intent(inout) :: process
    integer :: i_core
    associate (cm => process%cm)
       do i_core = 1, cm%n_cores
          select type (core => cm%cores(i_core)%core)
          type is (prc_threshold_t)
             select type (pcm => process%pcm)
             type is (pcm_nlo_t)
                pcm%settings%factorization_mode = FACTORIZATION_THRESHOLD
             end select
          end select
       end do
    end associate
  end subroutine process_check_if_threshold_method

  pure function process_select_i_term (process, i_mci) result (i_term)
    integer :: i_term
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    integer :: i_component, i_sub
    i_component = process%mci_entry(i_mci)%i_component(1)
    i_term = process%component(i_component)%i_term(1)
    i_sub = process%term(i_term)%i_sub
    if (i_sub > 0) &
       i_term = process%term(i_sub)%i_term_global
  end function process_select_i_term

  subroutine process_create_blha_interface (process, flv_born, flv_real, n_in, beam_structure)
    class(process_t), intent(inout) :: process
    integer, intent(in), dimension(:,:), allocatable :: flv_born, flv_real
    integer, intent(in) :: n_in
    type(beam_structure_t), intent(in) :: beam_structure
    integer :: alpha_power, alphas_power
    type(blha_master_t) :: blha_master
    integer :: openloops_phs_tolerance, openloops_stability_log
    logical :: use_cms, use_collier
    type(string_t) :: openloops_extra_cmd
    type(string_t) :: ew_scheme, correction_type
    type(process_component_def_t), pointer :: config => null ()
    config => process%meta%lib%get_component_def_ptr (process%meta%id, 1)
    call config%get_coupling_powers (alpha_power, alphas_power)
    associate (cm => process%cm)
       associate (var_list => process%meta%var_list)
          openloops_phs_tolerance = &
               var_list%get_ival (var_str ("openloops_phs_tolerance"))
          openloops_stability_log = &
               var_list%get_ival (var_str ("openloops_stability_log"))
          openloops_extra_cmd = var_list%get_sval (var_str ("$openloops_extra_cmd"))
          use_cms = var_list%get_lval (var_str ("?openloops_use_cms"))
          use_collier = var_list%get_lval (var_str ("?openloops_use_collier"))
          ew_scheme = var_list%get_sval (var_str ("$blha_ew_scheme"))
          correction_type = var_list%get_sval (var_str ("$nlo_correction_type"))
          call blha_master%set_ew_scheme (ew_scheme)
       end associate
       call blha_master%set_methods &
            (process%is_nlo_calculation (), process%meta%var_list)
       call blha_master%allocate_config_files ()
       call blha_master%set_correction_type (correction_type)
       call blha_master%setup_additional_features (openloops_phs_tolerance, &
            use_cms, &
            openloops_stability_log, &
            use_collier, &
            extra_cmd = openloops_extra_cmd, &
            beam_structure = beam_structure)
       call blha_master%generate (process%meta%id, process%config%model, &
            n_in, alpha_power, alphas_power, flv_born, flv_real)
       call blha_master%write_olp (process%meta%id)
    end associate
  end subroutine process_create_blha_interface

  subroutine process_create_and_load_extra_libraries &
       (process, beam_structure, var_list, os_data)
    class(process_t), intent(inout), target :: process
    type(beam_structure_t), intent(in) :: beam_structure
    type(var_list_t), intent(in) :: var_list
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: libname
    integer :: i_component, i_core
    logical, dimension(process%cm%n_cores) :: loaded
    logical :: give_warning, is_nlo
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(process_component_def_t), pointer :: config => null ()
    integer :: nlo_type_fetched

    loaded = .false.
    call msg_debug2 (D_PROCESS_INTEGRATION, &
         "process_create_and_load_extra_libraries")
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       call pcm%region_data%get_all_flv_states (flv_born, flv_real)
       n_in = pcm%region_data%get_n_in ()
       is_nlo = .true.
    class default
       i_core = process%get_i_core_nlo_type (BORN)
       is_nlo = .false.
       associate (core => process%cm%cores(i_core)%core)
          allocate (flv_born (core%data%get_n_tot (), core%data%n_flv))
          flv_born = core%data%flv_state
          n_in = core%data%n_in
       end associate
    end select
    if (process%needs_extra_code (only_blha = .true.)) &
         call process%create_blha_interface &
         (flv_born, flv_real, n_in, beam_structure)
    give_warning = .false.
    do i_component = 1, process%meta%n_components
       config => process%meta%lib%get_component_def_ptr &
            (process%meta%id, i_component)
       nlo_type_fetched = config%get_nlo_type ()
       if (nlo_type_fetched == NLO_MISMATCH)  nlo_type_fetched = NLO_SUBTRACTION
       i_core = process%get_i_core_nlo_type (nlo_type_fetched)
       if (config%can_be_integrated () .or. &
            process%get_nlo_type (i_core) == NLO_SUBTRACTION) then
          if (.not. loaded (i_core)) then
             select type (core => process%cm%cores(i_core)%core)
             class is (prc_user_defined_base_t)
                libname = process%get_library_name ()
                if (process%cm%core_is_radiation(i_core)) then
                   if (allocated (flv_real)) then
                      call core%data%set_flv_state (flv_real)
                   else
                      give_warning = .true.
                   end if
                   call msg_debug2 (D_PROCESS_INTEGRATION, &
                        "create and load radiation libraries")
                   call core%create_and_load_extra_libraries &
                        (flv_real, var_list, os_data, libname, &
                        process%config%model, i_core, is_nlo)
                else
                   if (allocated (flv_born)) then
                      call core%data%set_flv_state (flv_born)
                   else
                      give_warning = .true.
                   end if
                   call msg_debug2 (D_PROCESS_INTEGRATION, &
                        "create and load Born libraries")
                   call core%create_and_load_extra_libraries &
                        (flv_born, var_list, os_data, libname, &
                        process%config%model, i_core, is_nlo)
                end if
             end select
             select type (core => process%cm%cores(i_core)%core)
             type is (prc_threshold_t)
                core%has_beam_pol = beam_structure%has_polarized_beams ()
             end select
             loaded(i_core) = .true.
          end if
       end if
    end do
    if (give_warning) call msg_warning ("Some flavor structures ", &
         [var_str ("are not allocated. This is totally fine if "), &
          var_str ("$method = 'threshold' is used, but you should "), &
          var_str ("have a closer look if this is not the case.")])
  end subroutine process_create_and_load_extra_libraries


end module process
