! WHIZARD 2.7.0 Jan 21 2019
!
! Copyright (C) 1999-2019 by
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
  use dispatch_rng, only: dispatch_rng_factory
  use dispatch_rng, only: update_rng_seed_in_var_list
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
  use prc_core_def, only: prc_core_def_t
  use prc_core, only: prc_core_t, helicity_selection_t
  use prc_external, only: prc_external_t
  use prc_recola, only: prc_recola_t
  use blha_olp_interfaces, only: prc_blha_t, blha_template_t
  use prc_threshold, only: prc_threshold_t
  use phs_fks, only: phs_fks_config_t

  use phs_base
  use mappings, only: mapping_defaults_t
  use phs_forests, only: phs_parameters_t
  use phs_wood, only: phs_wood_config_t
  use phs_wood, only: EXTENSION_DEFAULT, EXTENSION_DGLAP
  use dispatch_phase_space, only: dispatch_phs
  use blha_config, only: blha_master_t
  use nlo_data, only: FKS_DEFAULT, FKS_RESONANCES

  use parton_states, only: connected_state_t
  use pcm_base
  use pcm
  use process_counter
  use process_config
  use process_mci

  implicit none
  private

  public :: process_t
  public :: process_ptr_t



  type :: process_status_t
     private
  end type process_status_t
  
  type :: process_results_t
     private
  end type process_results_t
  
  type :: process_t
     private
     type(process_metadata_t) :: &
          meta
     type(process_environment_t) :: &
          env
     type(process_config_data_t) :: &
          config
     class(pcm_t), allocatable :: &
          pcm
     type(process_component_t), dimension(:), allocatable :: &
          component
     type(process_phs_config_t), dimension(:), allocatable :: &
          phs_entry
     type(core_entry_t), dimension(:), allocatable :: &
          core_entry
     type(process_mci_entry_t), dimension(:), allocatable :: &
          mci_entry
     class(rng_factory_t), allocatable :: &
          rng_factory
     type(process_beam_config_t) :: &
          beam_config
     type(process_term_t), dimension(:), allocatable :: &
          term
     type(process_status_t) :: &
          status
     type(process_results_t) :: &
          result
   contains
     procedure :: write => process_write
     ! generic :: write (formatted) => write_formatted
     procedure :: write_formatted => process_write_formatted
     procedure :: write_meta => process_write_meta
     procedure :: show => process_show
     procedure :: final => process_final
     procedure :: init => process_init
     procedure :: complete_pcm_setup => process_complete_pcm_setup
     procedure :: setup_cores => process_setup_cores
     procedure :: prepare_blha_cores => process_prepare_blha_cores
     procedure :: create_blha_interface => process_create_blha_interface
     procedure :: init_components => process_init_components
     procedure :: record_inactive_components => process_record_inactive_components
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
     procedure :: init_phs_config => process_init_phs_config
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
     procedure :: get_qcd => process_get_qcd
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
     procedure :: uses_real_partition => process_uses_real_partition
     procedure :: get_md5sum_prc => process_get_md5sum_prc
     procedure :: get_md5sum_mci => process_get_md5sum_mci
     procedure :: get_md5sum_cfg => process_get_md5sum_cfg
     procedure :: get_n_cores => process_get_n_cores
     procedure :: get_base_i_term => process_get_base_i_term
     procedure :: get_core_term => process_get_core_term
     procedure :: get_core_ptr => process_get_core_ptr
     procedure :: get_term_ptr => process_get_term_ptr
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
     procedure :: set_run_id => process_set_run_id
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
     procedure :: reset_library_ptr => process_reset_library_ptr
     procedure :: set_component_type => process_set_component_type
     procedure :: set_counter_mci_entry => process_set_counter_mci_entry
     procedure :: pacify => process_pacify
     procedure :: test_allocate_sf_channels
     procedure :: test_set_component_sf_channel
     procedure :: test_get_mci_ptr
     procedure :: init_mci_work => process_init_mci_work
     procedure :: setup_test_cores => process_setup_test_cores
     procedure :: get_connected_states => process_get_connected_states
     procedure :: init_nlo_settings => process_init_nlo_settings
     generic :: get_nlo_type_component => get_nlo_type_component_single
     procedure :: get_nlo_type_component_single => process_get_nlo_type_component_single
     generic :: get_nlo_type_component => get_nlo_type_component_all
     procedure :: get_nlo_type_component_all => process_get_nlo_type_component_all
     procedure :: is_nlo_calculation => process_is_nlo_calculation
     procedure :: is_combined_nlo_integration &
          => process_is_combined_nlo_integration
     procedure :: component_is_real_finite => process_component_is_real_finite
     procedure :: get_component_nlo_type => process_get_component_nlo_type
     procedure :: get_component_core_ptr => process_get_component_core_ptr
     procedure :: get_component_associated_born &
               => process_get_component_associated_born
     procedure :: get_first_real_component => process_get_first_real_component
     procedure :: get_first_real_term => process_get_first_real_term
     procedure :: get_associated_real_fin => process_get_associated_real_fin
     procedure :: select_i_term => process_select_i_term
     procedure :: prepare_any_external_code &
        => process_prepare_any_external_code
  end type process_t

  type :: process_ptr_t
     type(process_t), pointer :: p => null ()
  end type process_ptr_t


  abstract interface
     subroutine dispatch_core_proc (core, core_def, model, &
          helicity_selection, qcd, use_color_factors, has_beam_pol)
       import
       class(prc_core_t), allocatable, intent(inout) :: core
       class(prc_core_def_t), intent(in) :: core_def
       class(model_data_t), intent(in), target, optional :: model
       type(helicity_selection_t), intent(in), optional :: helicity_selection
       type(qcd_t), intent(in), optional :: qcd
       logical, intent(in), optional :: use_color_factors
       logical, intent(in), optional :: has_beam_pol
     end subroutine dispatch_core_proc
  end interface
     

contains

  subroutine process_write (process, screen, unit, &
       show_os_data, show_var_list, show_rng, show_expressions, pacify)
    class(process_t), intent(in) :: process
    logical, intent(in) :: screen
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_os_data
    logical, intent(in), optional :: show_var_list
    logical, intent(in), optional :: show_rng
    logical, intent(in), optional :: show_expressions
    logical, intent(in), optional :: pacify
    integer :: u, iostat
    character(0) :: iomsg
    integer, dimension(:), allocatable :: v_list
    u = given_output_unit (unit)
    allocate (v_list (0))
    call set_flag (v_list, F_SHOW_OS_DATA, show_os_data)
    call set_flag (v_list, F_SHOW_VAR_LIST, show_var_list)
    call set_flag (v_list, F_SHOW_RNG, show_rng)
    call set_flag (v_list, F_SHOW_EXPRESSIONS, show_expressions)
    call set_flag (v_list, F_PACIFY, pacify)
    if (screen) then
       call process%write_formatted (u, "LISTDIRECTED", v_list, iostat, iomsg)
    else
       call process%write_formatted (u, "DT", v_list, iostat, iomsg)
    end if
  end subroutine process_write

  subroutine process_write_formatted (dtv, unit, iotype, v_list, iostat, iomsg)
    class(process_t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, dimension(:), intent(in) :: v_list
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: u
    logical :: screen
    logical :: var_list
    logical :: rng_factory
    logical :: expressions
    logical :: counters
    logical :: os_data
    logical :: model
    logical :: pacify
    integer :: i
    u = unit
    select case (iotype)
    case ("LISTDIRECTED")
       screen = .true.
    case default
       screen = .false.
    end select
    var_list = flagged (v_list, F_SHOW_VAR_LIST)
    rng_factory = flagged (v_list, F_SHOW_RNG, .true.)
    expressions = flagged (v_list, F_SHOW_EXPRESSIONS)
    counters = .true.
    os_data = flagged (v_list, F_SHOW_OS_DATA)
    model = .false.
    pacify = flagged (v_list, F_PACIFY)
    associate (process => dtv)
      if (screen) then
         write (msg_buffer, "(A)")  repeat ("-", 72)
         call msg_message ()
      else
         call write_separator (u, 2)
      end if
      call process%meta%write (u, screen)
      if (var_list) then
         call process%env%write (u, show_var_list=var_list, &
              show_model=.false., show_lib=.false., &
              show_os_data=os_data)
      else if (.not. screen) then
         write (u, "(1x,A)")  "Variable list: [not shown]"
      end if
      if (process%meta%type == PRC_UNKNOWN) then
         call write_separator (u, 2)
         return
      else if (screen) then
         return
      end if
      call write_separator (u)
      call process%config%write (u, counters, model, expressions)
      if (rng_factory) then
         if (allocated (process%rng_factory)) then
            call write_separator (u)
            call process%rng_factory%write (u)
         end if
      end if
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
    end associate
    iostat = 0
    iomsg = ""
  end subroutine process_write_formatted
  
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
          if (process%pcm%component_selected(i)) then
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
    real(default) :: err_percent
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
       if (object%meta%run_id /= "") then
          write (u, "('Run',1x,A,':',1x)", advance="no") &
               char (object%meta%run_id)
       end if
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
          write (u, "(1x,A)", advance="no")  "GeV"
       case (PRC_SCATTERING)
          write (u, "(1x,A)", advance="no")  "fb "
       case default
          write (u, "(1x,A)", advance="no")  "   "
       end select
       if (object%get_integral_tot () /= 0) then
          err_percent = abs (100 &
               * object%get_error_tot () / object%get_integral_tot ())
       else
          err_percent = 0
       end if
       if (err_percent == 0) then
          write (u, "(1x,'(',F4.0,4x,'%)')")  err_percent
       else if (err_percent < 0.1) then
          write (u, "(1x,'(',F7.3,1x,'%)')")  err_percent
       else if (err_percent < 1) then
          write (u, "(1x,'(',F6.2,2x,'%)')")  err_percent
       else if (err_percent < 10) then
          write (u, "(1x,'(',F5.1,3x,'%)')")  err_percent
       else
          write (u, "(1x,'(',F4.0,4x,'%)')")  err_percent
       end if
    else
       write (u, "(A)")  "[integral undefined]"
    end if
  end subroutine process_show

  subroutine process_final (process)
    class(process_t), intent(inout) :: process
    integer :: i
    ! call process%meta%final ()
    call process%env%final ()
    ! call process%config%final ()
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
    if (allocated (process%pcm)) then
       call process%pcm%final ()
       deallocate (process%pcm)
    end if
  end subroutine process_final

  subroutine process_init &
       (process, proc_id, lib, os_data, model, var_list, beam_structure)
    class(process_t), intent(out) :: process
    type(string_t), intent(in) :: proc_id
    type(process_library_t), intent(in), target :: lib
    type(os_data_t), intent(in) :: os_data
    class(model_t), intent(in), target :: model
    type(var_list_t), intent(inout), target, optional :: var_list
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer :: next_rng_seed
    call msg_debug (D_PROCESS_INTEGRATION, "process_init")
    associate &
         (meta => process%meta, env => process%env, config => process%config)
      call env%init &
           (model, lib, os_data, var_list, beam_structure)
      call meta%init &
           (proc_id, lib, env%get_var_list_ptr ())
      call config%init &
           (meta, env)
      call dispatch_rng_factory &
           (process%rng_factory, env%get_var_list_ptr (), next_rng_seed)
      call update_rng_seed_in_var_list (var_list, next_rng_seed)
      call dispatch_pcm &
           (process%pcm, config%process_def%is_nlo ())
      associate (pcm => process%pcm)
        call pcm%init (env, meta)
        call pcm%allocate_components (process%component, meta)
        call pcm%categorize_components (config)
      end associate
    end associate
  end subroutine process_init

  subroutine dispatch_pcm (pcm, is_nlo)
    class(pcm_t), allocatable, intent(out) :: pcm
    logical, intent(in) :: is_nlo
    if (.not. is_nlo) then
       allocate (pcm_default_t :: pcm)
    else
       allocate (pcm_nlo_t :: pcm)
    end if
  end subroutine dispatch_pcm

  subroutine process_complete_pcm_setup (process)
    class(process_t), intent(inout) :: process
    call process%pcm%complete_setup &
         (process%core_entry, process%component, process%env%get_model_ptr ())
  end subroutine process_complete_pcm_setup
  
  subroutine process_setup_cores (process, dispatch_core, &
       helicity_selection, use_color_factors, has_beam_pol)
    class(process_t), intent(inout) :: process
    procedure(dispatch_core_proc) :: dispatch_core
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    logical, intent(in), optional :: use_color_factors
    logical, intent(in), optional :: has_beam_pol
    integer :: i
    associate (pcm => process%pcm)
      call pcm%allocate_cores (process%config, process%core_entry)
      do i = 1, size (process%core_entry)
         call dispatch_core (process%core_entry(i)%core, &
              process%core_entry(i)%core_def, &
              process%config%model, &
              helicity_selection, &
              process%config%qcd, &
              use_color_factors, &
              has_beam_pol)
         call process%core_entry(i)%configure &
              (process%env%get_lib_ptr (), process%meta%id)
         if (process%core_entry(i)%core%uses_blha ()) then
            call pcm%setup_blha (process%core_entry(i))
         end if
      end do
    end associate
  end subroutine process_setup_cores
    
  subroutine process_prepare_blha_cores (process)
    class(process_t), intent(inout), target :: process
    integer :: i
    associate (pcm => process%pcm)
      do i = 1, size (process%core_entry)
         associate (core_entry => process%core_entry(i))
           if (core_entry%core%uses_blha ()) then
              pcm%uses_blha = .true.
              call pcm%prepare_blha_core (core_entry, process%config%model)
           end if
         end associate
      end do
    end associate
  end subroutine process_prepare_blha_cores
    
  subroutine process_create_blha_interface (process)
    class(process_t), intent(in) :: process
    integer :: alpha_power, alphas_power
    integer :: openloops_phs_tolerance, openloops_stability_log
    logical :: use_cms, use_collier
    type(string_t) :: ew_scheme, correction_type
    type(string_t) :: openloops_extra_cmd
    type(blha_master_t) :: blha_master
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    if (process%pcm%uses_blha) then
       call collect_configuration_parameters (process%get_var_list_ptr ())
       call process%component(1)%config%get_coupling_powers &
            (alpha_power, alphas_power)
       associate (pcm => process%pcm)
         call pcm%set_blha_methods (blha_master, process%get_var_list_ptr ())
         call blha_master%set_ew_scheme (ew_scheme)
         call blha_master%allocate_config_files ()
         call blha_master%set_correction_type (correction_type)
         call blha_master%setup_additional_features ( &
              openloops_phs_tolerance, &
              use_cms, &
              openloops_stability_log, &
              use_collier, &
              extra_cmd = openloops_extra_cmd, &
              beam_structure = process%env%get_beam_structure ())
         call pcm%get_blha_flv_states (process%core_entry, flv_born, flv_real)
         call blha_master%generate (process%meta%id, &
              process%config%model, process%config%n_in, &
              alpha_power, alphas_power, &
              flv_born, flv_real)
         call blha_master%write_olp (process%meta%id)
       end associate
    end if
  contains
    subroutine collect_configuration_parameters (var_list)
      type(var_list_t), intent(in) :: var_list
      openloops_phs_tolerance = &
           var_list%get_ival (var_str ("openloops_phs_tolerance"))
      openloops_stability_log = &
           var_list%get_ival (var_str ("openloops_stability_log"))
      use_cms = &
           var_list%get_lval (var_str ("?openloops_use_cms"))
      use_collier = &
           var_list%get_lval (var_str ("?openloops_use_collier"))
      ew_scheme = &
           var_list%get_sval (var_str ("$blha_ew_scheme"))
      correction_type = &
           var_list%get_sval (var_str ("$nlo_correction_type"))
      openloops_extra_cmd = &
           var_list%get_sval (var_str ("$openloops_extra_cmd"))
    end subroutine collect_configuration_parameters
  end subroutine process_create_blha_interface
    
  subroutine process_init_components (process, phs_config)
    class(process_t), intent(inout), target :: process
    class(phs_config_t), allocatable, intent(in), optional :: phs_config
    integer :: i, i_core
    class(prc_core_t), pointer :: core
    logical :: active
    associate (pcm => process%pcm)
      do i = 1, pcm%n_components
         i_core = pcm%get_i_core(i)
         if (i_core > 0) then
            core => process%get_core_ptr (i_core)
            active = core%has_matrix_element ()
         else
            active = .true.
         end if
         if (present (phs_config)) then
            call pcm%init_component (process%component(i), &
              i, &
              active, &
              phs_config, &
              process%env, process%meta, process%config)
         else
            call pcm%init_component (process%component(i), &
              i, &
              active, &
              process%phs_entry(pcm%i_phs_config(i))%phs_config, &
              process%env, process%meta, process%config)
         end if
      end do
    end associate
  end subroutine process_init_components

  subroutine process_record_inactive_components (process)
    class(process_t), intent(inout) :: process
    associate (pcm => process%pcm)
      call pcm%record_inactive_components (process%component, process%meta)
    end associate
  end subroutine process_record_inactive_components

  subroutine process_setup_terms (process, with_beams)
    class(process_t), intent(inout), target :: process
    logical, intent(in), optional :: with_beams
    class(model_data_t), pointer :: model
    integer :: i, j, k, i_term
    integer, dimension(:), allocatable :: n_entry
    integer :: n_components, n_tot
    integer :: i_sub
    type(string_t) :: subtraction_method
    class(prc_core_t), pointer :: core => null ()
    logical :: setup_subtraction_component, singular_real
    logical :: requires_spin_correlations
    integer :: nlo_type_to_fetch, n_emitters
    i_sub = 0
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
              if (setup_subtraction_component) then
                 select type (pcm => process%pcm)
                 class is (pcm_nlo_t)
                    process%term(i_term)%i_core = pcm%i_core(pcm%i_sub)
                 end select
              else
                 process%term(i_term)%i_core = process%pcm%get_i_core(i)
              end if
              
              if (process%term(i_term)%i_core == 0) then
                 call msg_bug ("Process '" // char (process%get_id ()) &
                      // "': core not found!")
              end if
              core => process%get_core_term (i_term)
              if (i_sub > 0) then
                 select type (pcm => process%pcm)
                 type is (pcm_nlo_t)
                    requires_spin_correlations = &
                         pcm%region_data%requires_spin_correlations ()
                    n_emitters = pcm%region_data%n_emitters
                 class default
                    requires_spin_correlations = .false.
                    n_emitters = 0
                 end select
                 if (requires_spin_correlations) then
                    call process%term(i_term)%init ( &
                         i_term, i, j, core, model, &
                         nlo_type = component%config%get_nlo_type (), &
                         use_beam_pol = with_beams, &
                         subtraction_method = subtraction_method, &
                         has_pdfs = process%pcm%has_pdfs, &
                         n_emitters = n_emitters)
                 else
                    call process%term(i_term)%init ( &
                         i_term, i, j, core, model, &
                         nlo_type = component%config%get_nlo_type (), &
                         use_beam_pol = with_beams, &
                         subtraction_method = subtraction_method, &
                         has_pdfs = process%pcm%has_pdfs)
                 end if
              else
                 call process%term(i_term)%init ( &
                      i_term, i, j, core, model, &
                      nlo_type = component%config%get_nlo_type (), &
                      use_beam_pol = with_beams, &
                      has_pdfs = process%pcm%has_pdfs)
              end if
           end do
       end associate
       k = k + n_entry(i)
    end do
    process%config%n_terms = n_tot
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
    allocate (pdg_in (2, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          if (present (i_core)) then
             ic = i_core
          else
             ic = process%pcm%get_i_core (i)
          end if
          associate (core => process%core_entry(ic)%core)
            pdg_in(:,i) = core%data%get_pdg_in ()
          end associate
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
    class(process_t), intent(inout), target :: process
    logical, intent(in), optional :: rest_frame
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer, intent(in), optional :: i_core
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_in
    integer, dimension(1) :: pdg_decay
    type(flavor_t), dimension(1) :: flv_in
    integer :: i, i0, ic
    allocate (pdg_in (1, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          if (present (i_core)) then
             ic = i_core
          else
             ic = process%pcm%get_i_core (i)
          end if
          associate (core => process%core_entry(ic)%core)
            pdg_in(:,i) = core%data%get_pdg_in ()
          end associate
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
       class(prc_core_t), pointer :: core
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
    class(process_t), intent(in), target :: process
    type(pdg_array_t), dimension(:,:), allocatable, intent(out) :: pdg_in
    integer :: i, i_core
    allocate (pdg_in (process%config%n_in, process%meta%n_components))
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          i_core = process%pcm%get_i_core (i)
          associate (core => process%core_entry(i_core)%core)
            pdg_in(:,i) = core%data%get_pdg_in ()
          end associate
       end if
    end do
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

  subroutine process_init_phs_config (process)
    class(process_t), intent(inout) :: process

    type(var_list_t), pointer :: var_list
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defs

    var_list => process%env%get_var_list_ptr ()

    phs_par%m_threshold_s = &
         var_list%get_rval (var_str ("phs_threshold_s"))
    phs_par%m_threshold_t = &
         var_list%get_rval (var_str ("phs_threshold_t"))
    phs_par%off_shell = &
         var_list%get_ival (var_str ("phs_off_shell"))
    phs_par%keep_nonresonant = &
         var_list%get_lval (var_str ("?phs_keep_nonresonant"))
    phs_par%t_channel = &
         var_list%get_ival (var_str ("phs_t_channel"))

    mapping_defs%energy_scale = &
         var_list%get_rval (var_str ("phs_e_scale"))
    mapping_defs%invariant_mass_scale = &
         var_list%get_rval (var_str ("phs_m_scale"))
    mapping_defs%momentum_transfer_scale = &
         var_list%get_rval (var_str ("phs_q_scale"))
    mapping_defs%step_mapping = &
         var_list%get_lval (var_str ("?phs_step_mapping"))
    mapping_defs%step_mapping_exp = &
         var_list%get_lval (var_str ("?phs_step_mapping_exp"))
    mapping_defs%enable_s_mapping = &
         var_list%get_lval (var_str ("?phs_s_mapping"))

    associate (pcm => process%pcm)
      call pcm%init_phs_config (process%phs_entry, &
           process%meta, process%env, phs_par, mapping_defs)
    end associate

  end subroutine process_init_phs_config
    
  subroutine process_configure_phs (process, rebuild, ignore_mismatch, &
     combined_integration, subdir)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    logical, intent(in), optional :: combined_integration
    type(string_t), intent(in), optional :: subdir
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
                    rebuild, ignore_mismatch, subdir)
            class is (pcm_nlo_t)
               select case (component%config%get_nlo_type ())
               case (BORN, NLO_VIRTUAL, NLO_SUBTRACTION)
                  call component%configure_phs (sqrts, process%beam_config, &
                       rebuild, ignore_mismatch, subdir)
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
                       process%beam_config, rebuild, ignore_mismatch, subdir)
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


  subroutine process_setup_mci (process, dispatch_mci)
    class(process_t), intent(inout) :: process
    procedure(dispatch_mci_proc) :: dispatch_mci
    class(mci_t), allocatable :: mci_template
    integer :: i, i_mci
    call msg_debug (D_PROCESS_INTEGRATION, "process_setup_mci")
    associate (pcm => process%pcm)
      call pcm%call_dispatch_mci (dispatch_mci, &
           process%get_var_list_ptr (), process%meta%id, mci_template)
      call pcm%setup_mci (process%mci_entry)
      process%config%n_mci = pcm%n_mci
      process%component(:)%i_mci = pcm%i_mci(:)
      do i = 1, pcm%n_components
         i_mci = process%pcm%i_mci(i)
         if (i_mci > 0) then
            associate (component => process%component(i), &
                 mci_entry => process%mci_entry(i_mci))
              call mci_entry%configure (mci_template, &
                   process%meta%type, &
                   i_mci, i, component, process%beam_config%n_sfpar, &
                   process%rng_factory)
              call mci_entry%set_parameters (process%get_var_list_ptr ())
            end associate
         end if
      end do
    end associate
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
    call process%meta%write (u, .false.)
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
    call process%env%write (u, show_var_list=.true., &
              show_model=.false., show_lib=.false., show_os_data=.false.)
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

  function process_get_qcd (process) result (qcd)
    type(qcd_t) :: qcd
    class(process_t), intent(in) :: process
    qcd = process%config%get_qcd ()
  end function process_get_qcd

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
    process%pcm%component_selected = .false.
  end subroutine process_reset_selected_cores

  pure subroutine process_select_components (process, indices)
    class(process_t), intent(inout) :: process
    integer, dimension(:), intent(in) :: indices
    associate (pcm => process%pcm)
      pcm%component_selected(indices) = .true.
    end associate
  end subroutine process_select_components

  pure function process_component_is_selected (process, index) result (val)
    logical :: val
    class(process_t), intent(in) :: process
    integer, intent(in) :: index
    associate (pcm => process%pcm)
      val = pcm%component_selected(index)
    end associate
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

  function process_get_n_cores (process) result (n)
    integer :: n
    class(process_t), intent(in) :: process
    n = process%pcm%n_cores
  end function process_get_n_cores

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
    core => process%core_entry(i_core)%get_core_ptr ()
  end function process_get_core_term

  function process_get_core_ptr (process, i_core) result (core)
    class(prc_core_t), pointer :: core
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_core
    if (allocated (process%core_entry)) then
       core => process%core_entry(i_core)%get_core_ptr ()
    else
       core => null ()
    end if
  end function process_get_core_ptr

  function process_get_term_ptr (process, i) result (term)
    type(process_term_t), pointer :: term
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i
    term => process%term(i)
  end function process_get_term_ptr

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

  subroutine process_set_run_id (process, run_id)
    class(process_t), intent(inout) :: process
    type(string_t), intent(in) :: run_id
    process%meta%run_id = run_id
  end subroutine process_set_run_id
  
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
    id = process%meta%lib_name
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
    ptr => process%config%process_def%get_component_def_ptr (i_component)
  end function process_get_component_def_ptr

  subroutine process_extract_core (process, i_term, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_term
    class(prc_core_t), intent(inout), allocatable :: core
    integer :: i_core
    i_core = process%term(i_term)%i_core
    call move_alloc (from = process%core_entry(i_core)%core, to = core)
  end subroutine process_extract_core

  subroutine process_restore_core (process, i_term, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_term
    class(prc_core_t), intent(inout), allocatable :: core
    integer :: i_core
    i_core = process%term(i_term)%i_core
    call move_alloc (from = core, to = process%core_entry(i_core)%core)
  end subroutine process_restore_core

  function process_get_constants (process, i_core) result (data)
    type(process_constants_t) :: data
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_core
    data = process%core_entry(i_core)%core%data
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
    call process%env%fill_process_constants (process%meta%id, i_component, data)
    unit = data%fill_unit_for_md5sum (.false.)
    write (unit, '(A)') char(type_string)
    select case (nlo_type)
    case (NLO_MISMATCH)
       write (unit, '(I0)')  NLO_SUBTRACTION
    case default
       write (unit, '(I0)')  nlo_type
    end select
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
    ptr => process%env%get_var_list_ptr ()
  end function process_get_var_list_ptr

  function process_get_model_ptr (process) result (ptr)
    class(process_t), intent(in) :: process
    class(model_data_t), pointer :: ptr
    ptr => process%config%model
  end function process_get_model_ptr

  subroutine process_make_rng (process, rng)
    class(process_t), intent(inout) :: process
    class(rng_t), intent(out), allocatable :: rng
    if (allocated (process%rng_factory)) then
       call process%rng_factory%make (rng)
    else
       call msg_bug ("Process: make rng: factory not allocated")
    end if
  end subroutine process_make_rng

  function process_compute_amplitude &
       (process, i_core, i, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced) &
       result (amp)
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_core
    integer, intent(in) :: i, j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in), optional :: fac_scale, ren_scale
    real(default), intent(in), allocatable, optional :: alpha_qcd_forced
    real(default) :: fscale, rscale
    real(default), allocatable :: aqcd_forced
    complex(default) :: amp
    class(prc_core_t), pointer :: core
    amp = 0
    if (0 < i .and. i <= process%meta%n_components) then
       if (process%component(i)%active) then
          associate (core => process%core_entry(i_core)%core)
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
          end associate
       else
          amp = 0
       end if
    end if
  end function process_compute_amplitude

  subroutine process_check_library_sanity (process)
    class(process_t), intent(in) :: process
    call process%env%check_lib_sanity (process%meta)
  end subroutine process_check_library_sanity

  subroutine process_reset_library_ptr (process)
    class(process_t), intent(inout) :: process
    call process%env%reset_lib_ptr ()
  end subroutine process_reset_library_ptr

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
    if (present (type_string)) then
       select case (char (type_string))
       case ("template")
          call process%setup_cores (dispatch_template_core)
       case ("test_me")
          call process%setup_cores (dispatch_test_me_core)
       case default
          call msg_bug ("process setup test cores: unsupported type string")
       end select
    else 
       call process%setup_cores (dispatch_test_me_core)
    end if
  end subroutine process_setup_test_cores
 
  subroutine dispatch_test_me_core (core, core_def, model, &
       helicity_selection, qcd, use_color_factors, has_beam_pol)
    use prc_test_core, only: test_t
    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_def_t), intent(in) :: core_def
    class(model_data_t), intent(in), target, optional :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    logical, intent(in), optional :: use_color_factors
    logical, intent(in), optional :: has_beam_pol
    allocate (test_t :: core)
  end subroutine dispatch_test_me_core
    
  subroutine dispatch_template_core (core, core_def, model, &
       helicity_selection, qcd, use_color_factors, has_beam_pol)
    use prc_template_me, only: prc_template_me_t
    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_def_t), intent(in) :: core_def
    class(model_data_t), intent(in), target, optional :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    logical, intent(in), optional :: use_color_factors
    logical, intent(in), optional :: has_beam_pol
    allocate (prc_template_me_t :: core)
    select type (core)
    type is (prc_template_me_t)
       call core%set_parameters (model)
    end select
  end subroutine dispatch_template_core

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

  subroutine process_init_nlo_settings (process, var_list)
    class(process_t), intent(inout) :: process
    type(var_list_t), intent(in), target :: var_list
    select type (pcm => process%pcm)
    type is (pcm_nlo_t)
       call pcm%init_nlo_settings (var_list)
       if (debug_active (D_SUBTRACTION) .or. debug_active (D_VIRTUAL)) &
              call pcm%settings%write ()
    class default
       call msg_fatal ("Attempt to set nlo_settings with a non-NLO pcm!")
    end select
  end subroutine process_init_nlo_settings

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

  function process_get_component_core_ptr (process, i_component) result (core)
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_component
    class(prc_core_t), pointer :: core
    integer :: i_core
    i_core = process%pcm%get_i_core(i_component)
    core => process%core_entry(i_core)%core
  end function process_get_component_core_ptr

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

  subroutine process_prepare_any_external_code (process)
    class(process_t), intent(inout), target :: process
    integer :: i
    call msg_debug2 (D_PROCESS_INTEGRATION, &
         "process_prepare_external_code")
    associate (pcm => process%pcm)
      do i = 1, pcm%n_cores
         call pcm%prepare_any_external_code ( &
              process%core_entry(i), i, &
              process%get_library_name (), &
              process%config%model, &
              process%env%get_var_list_ptr ())
      end do
    end associate
  end subroutine process_prepare_any_external_code


end module process
