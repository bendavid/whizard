! WHIZARD 2.2.6 May 02 2015
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

module processes

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: twopi4
  use format_utils, only: write_separator
  use unit_tests
  use system_dependencies
  use diagnostics
  use md5
  use cputime
  use os_interface
  use physics_defs
  use lorentz
  use sm_qcd
  use pdg_arrays
  use subevents
  use variables
  use expr_base
  use model_data
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use evaluators
  use particles
  use beam_structures
  use beams
  use sf_mappings
  use sf_base
  use process_constants
  use phs_base
  use phs_single
  use rng_base
  use mci_base
  use mci_midpoint
  use mci_vamp
  
  use vamp !NODEP!  

  use process_constants
  use prclib_interfaces
  use prc_core_def
  use process_libraries
  use prc_test

  use integration_results
  use prc_core
  use parton_states

  use nlo_data
  use nlo_controller
  use phs_wood
  use phs_fks
  use prc_gosam
  use prc_openloops
  use prc_omega
  use blha_config
  use blha_olp_interfaces
  
  implicit none
  private

  public :: process_t
  public :: process_instance_t
  public :: pacify
  public :: setup_nlo_component_cores
  public :: processes_test
  public :: test_t
  public :: prepare_test_process
  public :: cleanup_test_process

  integer, parameter :: STAT_UNDEFINED = 0
  integer, parameter :: STAT_INITIAL = 1
  integer, parameter :: STAT_ACTIVATED = 2
  integer, parameter :: STAT_BEAM_MOMENTA = 3
  integer, parameter :: STAT_FAILED_KINEMATICS = 4
  integer, parameter :: STAT_SEED_KINEMATICS = 5
  integer, parameter :: STAT_HARD_KINEMATICS = 6
  integer, parameter :: STAT_EFF_KINEMATICS = 7
  integer, parameter :: STAT_FAILED_CUTS = 8
  integer, parameter :: STAT_PASSED_CUTS = 9
  integer, parameter :: STAT_EVALUATED_TRACE = 10
  integer, parameter :: STAT_EVENT_COMPLETE = 11

  integer, parameter, public :: COMP_DEFAULT = 0
  integer, parameter, public :: COMP_MASTER  = 1
  integer, parameter, public :: COMP_VIRT = 2
  integer, parameter, public :: COMP_REAL = 3
  integer, parameter, public :: COMP_PDF = 4
  integer, parameter, public :: COMP_SUB = 5
  

  type :: process_counter_t
     integer :: total = 0
     integer :: failed_kinematics = 0
     integer :: failed_cuts = 0
     integer :: passed = 0
     integer :: evaluated = 0
     integer :: complete = 0
   contains
     procedure :: write => process_counter_write
     procedure :: reset => process_counter_reset
     procedure :: record => process_counter_record
  end type process_counter_t
  
  type :: kinematics_t
     integer :: n_in = 0
     integer :: n_channel = 0
     integer :: selected_channel = 0
     type(sf_chain_instance_t), pointer :: sf_chain => null ()
     class(phs_t), pointer :: phs => null ()
     real(default), dimension(:), pointer :: f => null ()
     real(default) :: phs_factor
     logical :: sf_chain_allocated = .false.
     logical :: phs_allocated = .false.
     logical :: f_allocated = .false.
     integer :: nlo_type
     integer :: emitter
   contains
     procedure :: write => kinematics_write
     procedure :: final => kinematics_final
     procedure :: set_nlo_info => kinematics_set_nlo_info
     procedure :: init_sf_chain => kinematics_init_sf_chain
     procedure :: init_phs => kinematics_init_phs
     procedure :: init_ptr => kinematics_init_ptr
     procedure :: compute_selected_channel => kinematics_compute_selected_channel
     procedure :: compute_other_channels => kinematics_compute_other_channels
     procedure :: get_incoming_momenta => kinematics_get_incoming_momenta
     procedure :: recover_mcpar => kinematics_recover_mcpar
     procedure :: get_mcpar => kinematics_get_mcpar
     procedure :: evaluate_sf_chain => kinematics_evaluate_sf_chain
     procedure :: return_beam_momenta => kinematics_return_beam_momenta
  end type kinematics_t

  type :: component_instance_t
     type(process_component_t), pointer :: config => null ()
     logical :: active = .false.
     type(kinematics_t) :: k_seed
     type(vector4_t), dimension(:), allocatable :: p_seed
     logical :: sqme_known = .false.
     real(default) :: sqme = 0
     class(prc_core_state_t), allocatable :: core_state
     type(nlo_controller_t), pointer :: nlo_controller => null ()
     integer :: nlo_type = BORN
   contains
     procedure :: write => component_instance_write
     procedure :: final => component_instance_final
     procedure :: init => component_instance_init
     procedure :: setup_kinematics => component_instance_setup_kinematics
     procedure :: setup_fks_kinematics => component_instance_setup_fks_kinematics
     procedure :: compute_seed_kinematics => &
          component_instance_compute_seed_kinematics
     procedure :: recover_mcpar => component_instance_recover_mcpar
     procedure :: compute_hard_kinematics => &
          component_instance_compute_hard_kinematics
     procedure :: recover_seed_kinematics => &
          component_instance_recover_seed_kinematics
     procedure :: compute_other_channels => &
          component_instance_compute_other_channels
     procedure :: return_beam_momenta => component_instance_return_beam_momenta
     procedure :: evaluate_sqme_born => component_instance_evaluate_sqme_born
     procedure :: evaluate_sqme => component_instance_evaluate_sqme
     procedure :: evaluate_sqme_virt => component_instance_evaluate_sqme_virt
     procedure :: evaluate_sqme_pdf => component_instance_evaluate_sqme_pdf 
     procedure :: evaluate_sqme_real => component_instance_evaluate_sqme_real
     procedure :: get_nlo_type => component_instance_get_nlo_type
     procedure :: get_component_type => &
                      component_instance_get_component_type
     procedure :: set_component_type => &
                      component_instance_set_component_type
  end type component_instance_t
  
  type :: term_instance_t
     type(process_term_t), pointer :: config => null ()
     logical :: active = .false.
     type(kinematics_t) :: k_term
     complex(default), dimension(:), allocatable :: amp
     type(interaction_t) :: int_hard
     type(interaction_t) :: int_hard_real
     type(isolated_state_t) :: isolated
     type(connected_state_t) :: connected
     type(isolated_state_t) :: isolated_real
     type(connected_state_t) :: connected_real
     logical :: checked = .false.
     logical :: passed = .false.
     real(default) :: scale = 0
     real(default) :: fac_scale = 0
     real(default) :: ren_scale = 0
     real(default), allocatable :: alpha_qcd_forced
     real(default) :: weight = 1
     type(vector4_t), dimension(:), allocatable :: p_hard
     type(nlo_controller_t), pointer :: nlo_controller => null ()
     integer :: nlo_type = BORN
   contains
     procedure :: write => term_instance_write
     procedure :: final => term_instance_final
     procedure :: init => term_instance_init
     procedure :: init_states => term_instance_init_states
     procedure :: setup_expressions => term_instance_setup_expressions
     procedure :: setup_expressions_real => term_instance_setup_expressions_real
     procedure :: setup_event_data => term_instance_setup_event_data
     procedure :: setup_real_interaction => term_instance_setup_real_interaction
     procedure :: reset => term_instance_reset
     procedure :: set_alpha_qcd_forced => term_instance_set_alpha_qcd_forced
     procedure :: compute_eff_kinematics => &
          term_instance_compute_eff_kinematics
     procedure :: recover_hard_kinematics => &
          term_instance_recover_hard_kinematics
     procedure :: evaluate_expressions => &
          term_instance_evaluate_expressions
     procedure :: evaluate_interaction => term_instance_evaluate_interaction
     procedure :: evaluate_trace => term_instance_evaluate_trace
     procedure :: evaluate_event_data => term_instance_evaluate_event_data
     procedure :: evaluate_interaction_real &
                  => term_instance_evaluate_interaction_real
     procedure :: evaluate_interaction_real_rad => &
                           term_instance_evaluate_interaction_real_rad
     procedure :: set_fac_scale => term_instance_set_fac_scale
     procedure :: get_fac_scale => term_instance_get_fac_scale
     procedure :: get_alpha_s => term_instance_get_alpha_s
  end type term_instance_t
  
  type :: mci_work_t
     type(process_mci_entry_t), pointer :: config => null ()
     real(default), dimension(:), allocatable :: x
     class(mci_instance_t), pointer :: mci => null ()
     type(process_counter_t) :: counter
   contains
     procedure :: write => mci_work_write
     procedure :: final => mci_work_final
     procedure :: init => mci_work_init
     procedure :: set => mci_work_set
     procedure :: set_x_strfun => mci_work_set_x_strfun
     procedure :: set_x_process => mci_work_set_x_process
     procedure :: get_active_components => mci_work_get_active_components
     procedure :: get_x_strfun => mci_work_get_x_strfun
     procedure :: get_x_process => mci_work_get_x_process
     procedure :: init_simulation => mci_work_init_simulation
     procedure :: final_simulation => mci_work_final_simulation
     procedure :: reset_counter => mci_work_reset_counter
     procedure :: record_call => mci_work_record_call
     procedure :: get_counter => mci_work_get_counter
  end type mci_work_t

  type, extends (mci_sampler_t) :: process_instance_t
     type(process_t), pointer :: process => null ()
     integer :: evaluation_status = STAT_UNDEFINED
     real(default) :: sqme = 0
     real(default), dimension(:), allocatable :: sqme_real
     real(default) :: weight = 0
     real(default) :: excess = 0
     integer :: i_mci = 0
     integer :: selected_channel = 0
     type(sf_chain_t) :: sf_chain
     type(component_instance_t), dimension(:), allocatable :: component
     type(term_instance_t), dimension(:), allocatable :: term
     type(mci_work_t), dimension(:), allocatable :: mci_work
     type(sqme_collector_t), pointer :: sqme_collector => null()
     type(nlo_controller_t), pointer :: nlo_controller => null()
   contains
     procedure :: write_header => process_instance_write_header
     procedure :: write => process_instance_write
     procedure :: final => process_instance_final
     procedure :: reset => process_instance_reset
     procedure :: activate => process_instance_activate
     procedure :: disable_virtual_components => &
                  process_instance_disable_virtual_components
     procedure :: init => process_instance_init
     procedure :: setup_sf_chain => process_instance_setup_sf_chain
     procedure :: setup_event_data => process_instance_setup_event_data
     procedure :: choose_mci => process_instance_choose_mci
     procedure :: set_mcpar => process_instance_set_mcpar
     procedure :: receive_beam_momenta => process_instance_receive_beam_momenta
     procedure :: set_beam_momenta => process_instance_set_beam_momenta
     procedure :: recover_beam_momenta => process_instance_recover_beam_momenta
     procedure :: select_channel => process_instance_select_channel
     procedure :: compute_seed_kinematics => &
          process_instance_compute_seed_kinematics
     procedure :: recover_mcpar => process_instance_recover_mcpar
     procedure :: compute_hard_kinematics => &
          process_instance_compute_hard_kinematics
     procedure :: recover_seed_kinematics => &
          process_instance_recover_seed_kinematics
     procedure :: compute_eff_kinematics => &
          process_instance_compute_eff_kinematics
     procedure :: recover_hard_kinematics => &
          process_instance_recover_hard_kinematics
     procedure :: evaluate_expressions => &
          process_instance_evaluate_expressions
     procedure :: compute_other_channels => &
          process_instance_compute_other_channels
     procedure :: evaluate_trace => process_instance_evaluate_trace
     procedure :: evaluate_event_data => process_instance_evaluate_event_data
     procedure :: evaluate_trace_real => process_instance_evaluate_trace_real
     procedure :: evaluate_trace_real_rad => process_instance_evaluate_trace_real_rad
     procedure :: compute_sqme_real_rad => process_instance_compute_sqme_real_rad
     procedure :: get_matrix_elements => process_instance_get_matrix_elements
     procedure :: normalize_weight => process_instance_normalize_weight
     procedure :: evaluate_sqme => process_instance_evaluate_sqme
     procedure :: recover => process_instance_recover
     procedure :: evaluate => process_instance_evaluate
     procedure :: is_valid => process_instance_is_valid
     procedure :: rebuild => process_instance_rebuild
     procedure :: fetch => process_instance_fetch
     procedure :: init_simulation => process_instance_init_simulation
     procedure :: final_simulation => process_instance_final_simulation
     procedure :: get_mcpar => process_instance_get_mcpar
     procedure :: has_evaluated_trace => process_instance_has_evaluated_trace
     procedure :: is_complete_event => process_instance_is_complete_event
     procedure :: select_i_term => process_instance_select_i_term
     procedure :: get_beam_int_ptr => process_instance_get_beam_int_ptr
     procedure :: get_trace_int_ptr => process_instance_get_trace_int_ptr
     procedure :: get_matrix_int_ptr => process_instance_get_matrix_int_ptr
     procedure :: get_flows_int_ptr => process_instance_get_flows_int_ptr
     procedure :: get_state_flv => process_instance_get_state_flv
     procedure :: get_isolated_state_ptr => &
          process_instance_get_isolated_state_ptr
     procedure :: get_connected_state_ptr => &
          process_instance_get_connected_state_ptr
     procedure :: get_beam_index => process_instance_get_beam_index
     procedure :: get_in_index => process_instance_get_in_index
     procedure :: get_sqme => process_instance_get_sqme
     procedure :: get_weight => process_instance_get_weight
     procedure :: get_excess => process_instance_get_excess
     procedure :: get_channel => process_instance_get_channel
     procedure :: set_fac_scale => process_instance_set_fac_scale
     procedure :: get_fac_scale => process_instance_get_fac_scale
     procedure :: get_alpha_s => process_instance_get_alpha_s
     procedure :: reset_counter => process_instance_reset_counter
     procedure :: record_call => process_instance_record_call
     procedure :: get_counter => process_instance_get_counter
     procedure :: get_trace => process_instance_get_trace
     procedure :: set_trace => process_instance_set_trace
     procedure :: set_alpha_qcd_forced => process_instance_set_alpha_qcd_forced
     procedure :: display_real_kinematics => &
          process_instance_display_real_kinematics
     procedure :: has_nlo_component => process_instance_has_nlo_component
     procedure :: create_blha_interface => process_instance_create_blha_interface
     procedure :: load_blha_libraries => process_instance_load_blha_libraries
     procedure :: set_blha_constants => process_instance_set_blha_constants
  end type process_instance_t
     

  type :: process_metadata_t
     private
     integer :: type = PRC_UNKNOWN
     type(string_t) :: id
     integer :: num_id = 0
     type(string_t) :: run_id
     type(var_list_t) :: var_list
     type(process_library_t), pointer :: lib => null ()
     integer :: lib_index = 0
     integer :: n_components = 0
     type(string_t), dimension(:), allocatable :: component_id
     type(string_t), dimension(:), allocatable :: component_description
     logical, dimension(:), allocatable :: active
   contains
     procedure :: final => process_metadata_final
     procedure :: write => process_metadata_write
     procedure :: show => process_metadata_show
     procedure :: init => process_metadata_init
     procedure :: deactivate_component => process_metadata_deactivate_component
  end type process_metadata_t

  type :: process_config_data_t
     private
     integer :: n_in = 0
     integer :: n_components = 0
     integer :: n_terms = 0
     integer :: n_mci = 0
     type(os_data_t) :: os_data
     class(rng_factory_t), allocatable :: rng_factory
     type(string_t) :: model_name
     class(model_data_t), pointer :: model => null ()
     type(qcd_t) :: qcd
     class(expr_factory_t), allocatable :: ef_cuts
     class(expr_factory_t), allocatable :: ef_scale
     class(expr_factory_t), allocatable :: ef_fac_scale
     class(expr_factory_t), allocatable :: ef_ren_scale
     class(expr_factory_t), allocatable :: ef_weight
     character(32) :: md5sum = ""
   contains
     procedure :: write => process_config_data_write
     procedure :: init => process_config_data_init
     procedure :: final => process_config_data_final
     procedure :: compute_md5sum => process_config_data_compute_md5sum
  end type process_config_data_t

  type :: process_beam_config_t
     private
     type(beam_data_t) :: data
     integer :: n_strfun = 0
     integer :: n_channel = 1
     integer :: n_sfpar = 0
     type(sf_config_t), dimension(:), allocatable :: sf
     type(sf_channel_t), dimension(:), allocatable :: sf_channel
     logical :: azimuthal_dependence = .false.
     logical :: lab_is_cm_frame = .true.
     character(32) :: md5sum = ""
     logical :: sf_trace = .false.
     type(string_t) :: sf_trace_file
   contains
     procedure :: write => process_beam_config_write
     procedure :: final => process_beam_config_final
     procedure :: init_beam_structure => process_beam_config_init_beam_structure
     procedure :: init_scattering => process_beam_config_init_scattering
     procedure :: init_decay => process_beam_config_init_decay
     procedure :: startup_message => process_beam_config_startup_message
     procedure :: init_sf_chain => process_beam_config_init_sf_chain
     procedure :: allocate_sf_channels => process_beam_config_allocate_sf_channels
     procedure :: set_sf_channel => process_beam_config_set_sf_channel
     procedure :: sf_startup_message => process_beam_config_sf_startup_message
     procedure :: get_pdf_set => process_beam_config_get_pdf_set
     procedure :: compute_md5sum => process_beam_config_compute_md5sum
  end type process_beam_config_t

  type :: process_mci_entry_t
     integer :: i_mci = 0
     integer, dimension(:), allocatable :: i_component
     integer :: process_type = PRC_UNKNOWN
     integer :: n_par = 0
     integer :: n_par_sf = 0
     integer :: n_par_phs = 0
     character(32) :: md5sum = ""
     integer :: pass = 0
     integer :: n_it = 0
     integer :: n_calls = 0
     logical :: activate_timer = .false.
     real(default) :: error_threshold = 0
     class(mci_t), allocatable :: mci
     type(process_counter_t) :: counter
     type(integration_results_t) :: results
     logical :: negative_weights
     logical :: combined_integration = .false.
   contains
     procedure :: final => process_mci_entry_final
     procedure :: write => process_mci_entry_write
     procedure :: write_chain_weights => process_mci_entry_write_chain_weights
     procedure :: init => process_mci_entry_init
     procedure :: create_component_list => &
                        process_mci_entry_create_component_list
     procedure :: set_combined_integration => &
                        process_mci_entry_set_combined_integration
     procedure :: set_parameters => process_mci_entry_set_parameters
     procedure :: compute_md5sum => process_mci_entry_compute_md5sum
     procedure :: sampler_test => process_mci_entry_sampler_test
     procedure :: integrate => process_mci_entry_integrate
     procedure :: final_integration => process_mci_entry_final_integration
     procedure :: get_time => process_mci_entry_get_time
     procedure :: time_message => process_mci_entry_time_message
     procedure :: prepare_simulation => process_mci_entry_prepare_simulation
     procedure :: generate_weighted_event => &
          process_mci_entry_generate_weighted_event
     procedure :: generate_unweighted_event => &
          process_mci_entry_generate_unweighted_event
     procedure :: recover_event => process_mci_entry_recover_event
     procedure :: has_integral => process_mci_entry_has_integral
     procedure :: get_integral => process_mci_entry_get_integral
     procedure :: get_error => process_mci_entry_get_error  
     procedure :: get_accuracy => process_mci_entry_get_accuracy
     procedure :: get_chi2 => process_mci_entry_get_chi2
     procedure :: get_efficiency => process_mci_entry_get_efficiency
     procedure :: get_md5sum => process_mci_entry_get_md5sum
  end type process_mci_entry_t

  type :: process_component_t
     private
     type(process_component_def_t), pointer :: config => null ()
     integer :: index = 0
     class(prc_core_t), allocatable :: core
     class(prc_core_t), pointer :: core_sub_born
     logical :: active = .false.
     class(mci_t), allocatable :: mci_template
     integer, dimension(:), allocatable :: i_term
     integer :: i_mci = 0
     class(phs_config_t), allocatable :: phs_config
     character(32) :: md5sum_phs = ""
     type(fks_template_t) :: fks_template
     integer :: component_type = COMP_DEFAULT
   contains
     procedure :: final => process_component_final
     procedure :: write => process_component_write
     procedure :: init => process_component_init
     procedure :: configure_phs => process_component_configure_phs
     procedure :: increase_phs_par => process_component_increase_phs_par
     procedure :: compute_md5sum => process_component_compute_md5sum
     procedure :: collect_channels => process_component_collect_channels
     procedure :: get_n_phs_par => process_component_get_n_phs_par
     procedure :: get_pdg_in => process_component_get_pdg_in
     procedure :: extract_phs_config => process_component_extract_phs_config
     procedure :: restore_phs_config => process_component_restore_phs_config
     procedure :: needs_mci_entry => process_component_needs_mci_entry
  end type process_component_t

  type :: process_term_t
     integer :: i_term_global = 0
     integer :: i_component = 0
     integer :: i_term = 0
     integer :: n_allowed = 0
     integer :: n_allowed_sub_born = 0
     type(process_constants_t) :: data
     real(default) :: alpha_s = 0
     integer, dimension(:), allocatable :: flv, hel, col
     logical :: rearrange = .false.
     type(interaction_t) :: int
     type(interaction_t), pointer :: int_eff => null ()
   contains
     procedure :: write => process_term_write
     procedure :: write_state_summary => process_term_write_state_summary
     procedure :: final => process_term_final
     procedure :: init => process_term_init
     procedure :: setup_interaction => process_term_setup_interaction
     procedure :: fetch_constants => process_term_fetch_process_constants
  end type process_term_t
  

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
     logical :: nlo_process
   contains
     procedure :: write => process_write
     procedure :: show => process_show
     procedure :: final => process_final
     procedure :: init => process_init
     procedure :: set_var_list => process_set_var_list
     procedure :: init_component => process_init_component
     procedure :: setup_terms => process_setup_terms
     procedure :: setup_beams_sqrts => process_setup_beams_sqrts
     procedure :: setup_beams_decay => process_setup_beams_decay
     procedure :: check_masses => process_check_masses
     procedure :: get_pdg_in => process_get_pdg_in
     procedure :: setup_beams_beam_structure => process_setup_beams_beam_structure
     procedure :: beams_startup_message => process_beams_startup_message
     procedure :: configure_phs => process_configure_phs
     procedure :: init_sf_chain => process_init_sf_chain
     generic :: set_sf_channel => set_sf_channel_single
     procedure :: set_sf_channel_single => process_set_sf_channel
     generic :: set_sf_channel => set_sf_channel_array
     procedure :: set_sf_channel_array => process_set_sf_channel_array
     procedure :: sf_startup_message => process_sf_startup_message
     procedure :: collect_channels => process_collect_channels
     procedure :: contains_trivial_component => process_contains_trivial_component
     procedure :: setup_mci => process_setup_mci
     procedure :: set_cuts => process_set_cuts
     procedure :: set_scale => process_set_scale
     procedure :: set_fac_scale => process_set_fac_scale
     procedure :: set_ren_scale => process_set_ren_scale
     procedure :: set_weight => process_set_weight
     procedure :: compute_md5sum => process_compute_md5sum
     procedure :: sampler_test => process_sampler_test
     procedure :: integrate => process_integrate
     procedure :: final_integration => process_final_integration
     procedure :: integrate_dummy => process_integrate_dummy
     procedure :: display_summed_results => process_display_summed_results
     procedure :: display_integration_history => &
          process_display_integration_history
     procedure :: write_logfile => process_write_logfile
     procedure :: write_state_summary => process_write_state_summary
     procedure :: prepare_simulation => process_prepare_simulation
     procedure :: generate_weighted_event => process_generate_weighted_event
     procedure :: generate_unweighted_event => process_generate_unweighted_event
     procedure :: recover_event => process_recover_event
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
     procedure :: component_reset_helicity => process_component_reset_helicity
     procedure :: is_nlo_calculation => process_is_nlo_calculation
     procedure :: get_component_nlo_type => process_get_component_nlo_type
     procedure :: get_component_associated_born &
               => process_get_component_associated_born
     procedure :: get_n_allowed_born => process_get_n_allowed_born
     procedure :: is_active_nlo_component => process_is_active_nlo_component 
     procedure :: get_md5sum_prc => process_get_md5sum_prc
     procedure :: get_md5sum_mci => process_get_md5sum_mci
     procedure :: get_md5sum_cfg => process_get_md5sum_cfg
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
     procedure :: extract_component_core => process_extract_component_core
     procedure :: restore_component_core => process_restore_component_core
     procedure :: get_constants => process_get_constants
     procedure :: get_term_flv_out => process_get_term_flv_out
     procedure :: contains_unstable => process_contains_unstable
     procedure :: get_sqrts => process_get_sqrts
     procedure :: has_matrix_element => process_has_matrix_element
     procedure :: get_beam_data_ptr => process_get_beam_data_ptr
     procedure :: cm_frame => process_cm_frame
     procedure :: get_pdf_set => process_get_pdf_set
     procedure :: get_var_list_ptr => process_get_var_list_ptr
     procedure :: get_model_ptr => process_get_model_ptr
     procedure :: make_rng => process_make_rng
     procedure :: compute_amplitude => process_compute_amplitude
     procedure :: set_component_type => process_set_component_type
     procedure :: pacify => process_pacify
  end type process_t


  interface pacify
     module procedure pacify_process_instance
  end interface pacify
  

  type, extends (prc_core_t) :: test_t
   contains
     procedure :: write => test_write
     procedure :: needs_mcset => test_needs_mcset
     procedure :: get_n_terms => test_get_n_terms
     procedure :: is_allowed => test_is_allowed
     procedure :: compute_hard_kinematics => test_compute_hard_kinematics
     procedure :: compute_eff_kinematics => test_compute_eff_kinematics
     procedure :: recover_kinematics => test_recover_kinematics
     procedure :: compute_amplitude => test_compute_amplitude
  end type test_t


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
  end subroutine process_final
    
  subroutine process_init &
       (process, proc_id, run_id, lib, os_data, qcd, rng_factory, model)
    class(process_t), intent(out) :: process
    type(string_t), intent(in) :: proc_id
    type(string_t), intent(in) :: run_id
    type(process_library_t), intent(in), target :: lib
    type(os_data_t), intent(in) :: os_data
    type(qcd_t), intent(in) :: qcd
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    class(model_data_t), intent(inout), pointer :: model
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
      call process%config%init (meta, os_data, qcd, rng_factory, model)
      allocate (process%component (meta%n_components))
    end associate
    process%nlo_process = lib%get_nlo_process (proc_id)
  end subroutine process_init
  
  subroutine process_set_var_list (process, var_list)
    class(process_t), intent(inout) :: process
    type(var_list_t), intent(in) :: var_list
    call var_list_init_snapshot &
         (process%meta%var_list, var_list, follow_link=.true.)
  end subroutine process_set_var_list
  
  subroutine process_init_component &
       (process, index, core_template, mci_template, phs_config_template, &
        fks_template, blha_template)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: index
    class(prc_core_t), intent(in), allocatable :: core_template
    class(mci_t), intent(in), allocatable :: mci_template
    class(phs_config_t), intent(in), allocatable :: phs_config_template
    type(fks_template_t), intent(in), optional :: fks_template
    type(blha_template_t), intent(inout), optional :: blha_template
    call process%component(index)%init (index, &
         process%meta, process%config, &
         core_template, mci_template, phs_config_template, &
         blha_template, fks_template)
    if (.not. process%component(index)%active) then
       call process%meta%deactivate_component(index)
    end if
  end subroutine process_init_component

  subroutine process_setup_terms (process)
    class(process_t), intent(inout) :: process
    class(model_data_t), pointer :: model
    integer :: i, j, k
    integer, dimension(:), allocatable :: n_entry
    integer :: n_components, n_tot
    model => process%config%model
    n_components = process%meta%n_components
    allocate (n_entry (n_components), source = 0)
    do i = 1, n_components
       associate (component => process%component(i))
         if (component%active)  n_entry(i) = component%core%get_n_terms () 
       end associate
    end do
    n_tot = sum (n_entry)
    allocate (process%term (n_tot))
    k = 0
    do i = 1, n_components
       associate (component => process%component(i))
         if (.not. component%active)  cycle
         associate (core => component%core)
           allocate (component%i_term (n_entry(i)))
           do j = 1, n_entry(i)
              component%i_term(j) = k + j
              call process%term(k+j)%init (k+j, i, j, core, model)
           end do
         end associate
       end associate
       k = k + n_entry(i)
    end do
    process%config%n_terms = n_tot
  end subroutine process_setup_terms

  subroutine process_setup_beams_sqrts (process, sqrts, beam_structure)
    class(process_t), intent(inout) :: process
    real(default), intent(in) :: sqrts
    type(beam_structure_t), intent(in), optional :: beam_structure
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_in
    integer, dimension(2) :: pdg_scattering
    type(flavor_t), dimension(2) :: flv_in
    integer :: i, i0
    allocate (pdg_in (2, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          pdg_in(:,i) = process%component(i)%get_pdg_in ()
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

  subroutine process_setup_beams_decay (process, rest_frame, beam_structure)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: rest_frame
    type(beam_structure_t), intent(in), optional :: beam_structure
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_in
    integer, dimension(1) :: pdg_decay
    type(flavor_t), dimension(1) :: flv_in
    integer :: i, i0
    allocate (pdg_in (1, process%meta%n_components))
    i0 = 0
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          pdg_in(:,i) = process%component(i)%get_pdg_in ()
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
       do i = 1, process%meta%n_components
          if (.not. process%component(i)%active)  cycle
          associate (data => process%component(i)%core%data)
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
    integer :: i
    allocate (pdg_in (process%config%n_in, process%meta%n_components))
    do i = 1, process%meta%n_components
       if (process%component(i)%active) then
          pdg_in(:,i) = process%component(i)%get_pdg_in ()
       end if
    end do    
  end subroutine process_get_pdg_in
  
  subroutine process_setup_beams_beam_structure &
       (process, beam_structure, sqrts, decay_rest_frame)
    class(process_t), intent(inout) :: process
    type(beam_structure_t), intent(in) :: beam_structure
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: decay_rest_frame
    if (process%get_n_in () == beam_structure%get_n_beam ()) then
       call process%beam_config%init_beam_structure &
            (beam_structure, sqrts, process%get_model_ptr (), decay_rest_frame)
    else if (beam_structure%get_n_beam () == 0) then
       call msg_fatal ("Asymmetric beams: missing beam particle specification")
    else
       call msg_fatal ("Mismatch of process and beam setup (scattering/decay)")
    end if
  end subroutine process_setup_beams_beam_structure
  
  subroutine process_beams_startup_message (process, unit, beam_structure)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: unit
    type(beam_structure_t), intent(in), optional :: beam_structure
    call process%beam_config%startup_message (unit, beam_structure)
  end subroutine process_beams_startup_message
  
  subroutine process_configure_phs (process, rebuild, ignore_mismatch, verbose, &
                                    combined_integration)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch, verbose
    logical, intent(in), optional :: combined_integration
    real(default) :: sqrts
    integer :: i, i_born
    class(phs_config_t), allocatable :: phs_config_born
    sqrts = process%get_sqrts ()
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
         if (component%active) then
            if (process%nlo_process) then
               select case (component%config%get_nlo_type ())
               case (BORN, NLO_VIRTUAL, NLO_SUBTRACTION)
                  call component%configure_phs (sqrts, process%beam_config, &
                       rebuild, ignore_mismatch, verbose=verbose)
                  call check_and_extend_phs (component, combined_integration)
               case (NLO_REAL, NLO_PDF)
                  i_born = component%config%get_associated_born ()
                  call check_and_extend_phs (component, combined_integration)
                  call process%component(i_born)%extract_phs_config (phs_config_born)
                  select type (phs_config_born)
                  type is (phs_wood_config_t)
                     call  component%configure_phs (sqrts, &
                          process%beam_config, rebuild, ignore_mismatch, &
                          phs_config_born, verbose=verbose)
                  end select
                  call process%component(i_born)%restore_phs_config (phs_config_born)
               end select
            else
               call component%configure_phs (sqrts, process%beam_config, &
                    rebuild, ignore_mismatch, verbose=verbose)
            end if
         end if
       end associate
    end do
  contains
    subroutine check_and_extend_phs (component, combined_integration)
      type(process_component_t), intent(inout) :: component
      logical, intent(in), optional :: combined_integration
      if (present (combined_integration)) then
         if (combined_integration) then
            select type (phs_config => component%phs_config)
            type is (phs_wood_config_t)
               call phs_config%set_extended_phs ()
               call phs_config%increase_n_par ()
            type is (phs_fks_config_t)
               call phs_config%set_extended_phs ()
            end select
         end if
      end if
    end subroutine check_and_extend_phs
  end subroutine process_configure_phs
         
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
         if (component%active) then
            call component%collect_channels (coll)
         end if
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
  
  subroutine process_setup_mci (process, combined_integration)
    class(process_t), intent(inout) :: process
    logical, intent(in), optional :: combined_integration
    integer :: n_mci, i_mci
    integer :: i
    n_mci = 0
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
          if (component%needs_mci_entry () .and. &
              component%config%get_nlo_type () /= NLO_SUBTRACTION) then
            n_mci = n_mci + 1
            component%i_mci = n_mci
         end if
       end associate
    end do
    process%config%n_mci = n_mci
    if (.not. allocated (process%config%rng_factory)) &
         call msg_bug ("Process setup: rng factory not allocated")
    allocate (process%mci_entry (n_mci))
    i_mci = 0
    do i = 1, process%meta%n_components
       associate (component => process%component(i))
          if (component%needs_mci_entry () .and. &
              component%config%get_nlo_type () /= NLO_SUBTRACTION) then
            i_mci = i_mci + 1
            associate (mci_entry => process%mci_entry(i_mci))
              call mci_entry%set_combined_integration (combined_integration)
              call mci_entry%init (process%meta%type, &
                   i_mci, i, component, process%beam_config, &
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
  
  subroutine process_sampler_test (process, instance, i_mci, n_calls)
    class(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout), target :: instance
    integer, intent(in) :: i_mci
    integer, intent(in) :: n_calls
    call process%mci_entry(i_mci)%sampler_test (instance, n_calls)
  end subroutine process_sampler_test

  subroutine process_integrate (process, instance, i_mci, n_it, n_calls, &
       adapt_grids, adapt_weights, final, pacify)
    class(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: adapt_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: final, pacify
    call process%mci_entry(i_mci)%integrate (instance, n_it, n_calls, &
         adapt_grids, adapt_weights, final, pacify, i_mci)        
    call instance%display_real_kinematics (i_mci)
  end subroutine process_integrate

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
    call results%display_init (process%meta%type, screen = .true., unit = u_log)
    call results%new_pass ()
    call results%record (1, 0, 0._default, 0._default, 0._default)
    call results%display_final ()
  end subroutine process_integrate_dummy
  
  subroutine process_display_summed_results (process)
    class(process_t), intent(inout) :: process
    type(integration_results_t) :: results
    integer :: u_log
    u_log = logfile_unit ()
    call results%init (process%meta%type)
    call results%display_init (process%meta%type, screen = .true., unit = u_log)
    call results%new_pass ()
    call results%record (1, 0, &
         process%get_integral (), &
         process%get_error (), &
         process%get_efficiency ())
    if (process%nlo_process) &
       call results%record_correction (process%get_correction (), &
                                        process%get_correction_error ())
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
    select type (mci => process%mci_entry(i_mci)%mci)
    type is (mci_midpoint_t)
       write (u, "(1x,A)")  "MC Integrator is Midpoint rule"
    type is (mci_vamp_t)
       write (u, "(1x,A)")  "MC Integrator is VAMP"       
       call write_separator (u)
       call mci%write_history (u)
       call write_separator (u)       
       if (mci%grid_par%use_vamp_equivalences) then
          call vamp_equivalences_write (mci%equivalences, u)          
       else
          write (u, "(3x,A)") "No VAMP equivalences have been used"
       end if
       call write_separator (u)
       call process%mci_entry(i_mci)%write_chain_weights (u) 
    class default
       write (u, "(1x,A)")  "MC Integrator: [unknown]"
    end select
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
               (process%component(i_component)%core, unit)
       end if
    end do
  end subroutine process_write_state_summary
       
  subroutine process_prepare_simulation (process, i_mci)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_mci
    call process%mci_entry(i_mci)%prepare_simulation ()
  end subroutine process_prepare_simulation

  subroutine process_generate_weighted_event (process, instance, i_mci)
    class(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    call process%mci_entry(i_mci)%generate_weighted_event (instance)
  end subroutine process_generate_weighted_event

  subroutine process_generate_unweighted_event (process, instance, i_mci)
    class(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    call process%mci_entry(i_mci)%generate_unweighted_event (instance)
  end subroutine process_generate_unweighted_event

  subroutine process_recover_event (process, instance, i_term)
    class(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    call process%mci_entry(instance%i_mci)%recover_event (instance, i_term)
  end subroutine process_recover_event

  function process_has_integral_mci (process, i_mci) result (flag)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    logical :: flag
    flag = process%mci_entry(i_mci)%has_integral ()
  end function process_has_integral_mci

  function process_has_integral_tot (process) result (flag)
    class(process_t), intent(in) :: process
    logical :: flag
    integer :: i
    flag = .true.
    do i = 1, size (process%mci_entry)
       if (process%is_active_nlo_component (i)) &
          flag = flag .and. process%mci_entry(i)%has_integral ()
    end do
  end function process_has_integral_tot
  
  function process_get_integral_mci (process, i_mci) result (integral)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    real(default) :: integral
    integral = process%mci_entry(i_mci)%get_integral ()
  end function process_get_integral_mci
  
  function process_get_error_mci (process, i_mci) result (error)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    real(default) :: error
    error = process%mci_entry(i_mci)%get_error ()
  end function process_get_error_mci
  
  function process_get_efficiency_mci (process, i_mci) result (efficiency)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    real(default) :: efficiency
    efficiency = process%mci_entry(i_mci)%get_efficiency ()
  end function process_get_efficiency_mci
  
  function process_get_integral_tot (process) result (integral)
    class(process_t), intent(in) :: process
    real(default) :: integral
    integer :: i
    integral = 0
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          if (process%is_active_nlo_component(i)) &
               integral = integral + process%mci_entry(i)%get_integral ()
       end do
    end if
  end function process_get_integral_tot
  
  function process_get_error_tot (process) result (error)
    class(process_t), intent(in) :: process
    real(default) :: error
    real(default) :: variance
    integer :: i
    variance = 0
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          if (process%is_active_nlo_component(i)) &
               variance = variance + process%mci_entry(i)%get_error () ** 2
       end do
    end if
    error = sqrt (variance)
  end function process_get_error_tot
  
  function process_get_efficiency_tot (process) result (efficiency)
    class(process_t), intent(in) :: process
    real(default) :: efficiency
    real(default) :: den, eff, int
    integer :: i
    den = 0
    if (allocated (process%mci_entry)) then
       do i = 1, size (process%mci_entry)
          if (process%is_active_nlo_component(i)) then
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
    end if
    if (den > 0) then
       efficiency = process%get_integral () / den
    else
       efficiency = 0
    end if
  end function process_get_efficiency_tot
  
  function process_get_correction (process) result (ratio)
    class(process_t), intent(in) :: process
    real(default) :: ratio
    real(default) :: int_real, int_virt, int_born
    int_born = process%mci_entry(1)%get_integral ()
    int_real = process%mci_entry(2)%get_integral ()
    int_virt = process%mci_entry(3)%get_integral ()
    ratio = (int_real + int_virt) / int_born
  end function process_get_correction

  function process_get_correction_error (process) result (error)
    class(process_t), intent(in) :: process
    real(default) :: error
    real(default) :: int_born, int_real, int_virt
    real(default) :: err_born, err_real, err_virt
    real(default) :: err2
    int_born = process%mci_entry(1)%get_integral ()
    int_real = process%mci_entry(2)%get_integral ()
    int_virt = process%mci_entry(3)%get_integral ()
    err_born = process%mci_entry(1)%get_error ()
    err_real = process%mci_entry(2)%get_error ()
    err_virt = process%mci_entry(3)%get_error ()
    err2 = (int_real + int_virt)**2 / int_born**4 * err_born**2 &
             + (err_real**2 + err_virt**2) / int_born**2
    error = sqrt (err2)
  end function process_get_correction_error

  subroutine process_component_reset_helicity(proc, i)
    class(process_t), intent(inout) :: proc
    integer, intent(in) :: i
    select type (core => proc%component(i)%core)
      type is (prc_omega_t)
        call core%reset_helicity_selection ()
    end select
    if (i == 2) then
      select type (core_sub_born => proc%component(i)%core_sub_born)
      type is (prc_omega_t)
         call core_sub_born%reset_helicity_selection ()
      end select
    end if
  end subroutine process_component_reset_helicity

  function process_is_nlo_calculation (process) result (nlo)
    class(process_t), intent(in) :: process
    logical :: nlo
    nlo = process%nlo_process
  end function process_is_nlo_calculation

  function process_get_component_nlo_type (process, i_component) &
           result (nlo_type)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    integer :: nlo_type
    nlo_type = process%component(i_component)%config%get_nlo_type ()
  end function process_get_component_nlo_type

  function process_get_component_associated_born (process, i_component) &
           result (i_born)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    integer :: i_born
    i_born = process%component(i_component)%config%get_associated_born ()
  end function process_get_component_associated_born

  function process_get_n_allowed_born (process, i_born) result (n_born)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_born
    integer :: n_born
    n_born = process%term(i_born)%n_allowed

  end function process_get_n_allowed_born

  function process_is_active_nlo_component (process, i_component) &
           result (active)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    logical :: active
    associate (component => process%component(i_component))
       active = component%config%is_active_nlo_component () &
                .and. component%component_type <= COMP_MASTER
    end associate
  end function process_is_active_nlo_component

  function process_get_md5sum_prc (process, i_component) result (md5sum)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    character(32) :: md5sum
    if (process%component(i_component)%active) then
       md5sum = process%component(i_component)%config%get_md5sum ()
    else
       md5sum = ""
    end if
  end function process_get_md5sum_prc
    
  function process_get_md5sum_mci (process, i_mci) result (md5sum)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_mci
    character(32) :: md5sum
    md5sum = process%mci_entry(i_mci)%get_md5sum ()
  end function process_get_md5sum_mci
    
  function process_get_md5sum_cfg (process) result (md5sum)
    class(process_t), intent(in) :: process
    character(32) :: md5sum
    md5sum = process%config%md5sum
  end function process_get_md5sum_cfg

  subroutine process_metadata_final (meta)
    class(process_metadata_t), intent(inout) :: meta
    call meta%var_list%final (follow_link=.true.)
  end subroutine process_metadata_final
  
  subroutine process_metadata_write (meta, u, var_list, screen)
    class(process_metadata_t), intent(in) :: meta
    integer, intent(in) :: u
    logical, intent(in) :: var_list, screen    
    integer :: i
    select case (meta%type)
    case (PRC_UNKNOWN)
       if (screen) then
          write (msg_buffer, "(A)") "Process [undefined]"
       else
          write (u, "(1x,A)") "Process [undefined]"
       end if
       return
    case (PRC_DECAY)
       if (screen) then
          write (msg_buffer, "(A,1x,A,A,A)") "Process [decay]:", & 
               "'", char (meta%id), "'"
       else
          write (u, "(1x,A)", advance="no") "Process [decay]:"
       end if
    case (PRC_SCATTERING)
       if (screen) then
          write (msg_buffer, "(A,1x,A,A,A)") "Process [scattering]:", &
               "'", char (meta%id), "'"
       else
          write (u, "(1x,A)", advance="no") "Process [scattering]:"
       end if
    case default
       call msg_bug ("process_write: undefined process type")
    end select    
    if (screen)  then
       call msg_message ()
    else
       write (u, "(1x,A,A,A)") "'", char (meta%id), "'"
    end if
    if (meta%num_id /= 0) then
       if (screen) then
          write (msg_buffer, "(2x,A,I0)") "ID (num)      = ", meta%num_id
          call msg_message ()
       else
          write (u, "(3x,A,I0)") "ID (num)      = ", meta%num_id            
       end if
    end if
    if (screen) then
       if (meta%run_id /= "") then
          write (msg_buffer, "(2x,A,A,A)") "Run ID        = '", &
               char (meta%run_id), "'"
          call msg_message ()
       end if
    else
       write (u, "(3x,A,A,A)") "Run ID        = '", char (meta%run_id), "'"       
    end if
    if (associated (meta%lib)) then
       if (screen) then
          write (msg_buffer, "(2x,A,A,A)")  "Library name  = '", &
               char (meta%lib%get_name ()), "'"
          call msg_message ()
       else          
          write (u, "(3x,A,A,A)")  "Library name  = '", &
               char (meta%lib%get_name ()), "'"          
       end if
    else
       if (screen) then
          write (msg_buffer, "(2x,A)")  "Library name  = [not associated]"
          call msg_message ()
       else
          write (u, "(3x,A)")  "Library name  = [not associated]"
       end if
    end if
    if (screen) then
       write (msg_buffer, "(2x,A,I0)")  "Process index = ", meta%lib_index
       call msg_message ()
    else
       write (u, "(3x,A,I0)")  "Process index = ", meta%lib_index
    end if
    if (allocated (meta%component_id)) then
       if (screen) then
          if (any (meta%active)) then
             write (msg_buffer, "(2x,A)")  "Process components:"
          else
             write (msg_buffer, "(2x,A)")  "Process components: [none]"
          end if
          call msg_message ()
       else
          write (u, "(3x,A)")  "Process components:"
       end if
       do i = 1, size (meta%component_id)
          if (.not. meta%active(i))  cycle
          if (screen) then
             write (msg_buffer, "(4x,I0,9A)")  i, ": '", &
                  char (meta%component_id (i)), "':   ", &
                  char (meta%component_description (i))
             call msg_message ()
          else
             write (u, "(5x,I0,9A)")  i, ": '", &
                  char (meta%component_id (i)), "':   ", &
                  char (meta%component_description (i))             
          end if
       end do
    end if
    if (screen) then
       write (msg_buffer, "(A)")  repeat ("-", 72)
       call msg_message ()
    else
       call write_separator (u)       
    end if
    if (screen)  return
    if (var_list) then
       write (u, "(1x,A)")  "Variable list:"
       call write_separator (u)
       call var_list_write (meta%var_list, u)
    else
       write (u, "(1x,A)")  "Variable list: [not shown]"
    end if
  end subroutine process_metadata_write

  subroutine process_metadata_show (meta, u, model_name)
    class(process_metadata_t), intent(in) :: meta
    integer, intent(in) :: u
    type(string_t), intent(in) :: model_name
    integer :: i
    select case (meta%type)
    case (PRC_UNKNOWN)
       write (u, "(A)") "Process: [undefined]"
       return
    case default
       write (u, "(A)", advance="no") "Process:"
    end select
    write (u, "(1x,A)", advance="no") char (meta%id)
    select case (meta%num_id)
    case (0)
    case default
       write (u, "(1x,'(',I0,')')", advance="no") meta%num_id
    end select
    select case (char (model_name))
    case ("")
    case default
       write (u, "(1x,'[',A,']')", advance="no")  char (model_name)
    end select
    write (u, *)
    if (allocated (meta%component_id)) then
       do i = 1, size (meta%component_id)
          if (meta%active(i)) then
             write (u, "(2x,I0,':',1x,A)")  i, &
                  char (meta%component_description (i))
          end if
       end do
    end if
  end subroutine process_metadata_show

  subroutine process_metadata_init (meta, id, run_id, lib)
    class(process_metadata_t), intent(out) :: meta
    type(string_t), intent(in) :: id
    type(string_t), intent(in) :: run_id
    type(process_library_t), intent(in), target :: lib
    select case (lib%get_n_in (id))
    case (1);  meta%type = PRC_DECAY
    case (2);  meta%type = PRC_SCATTERING
    case default
       call msg_bug ("Process '" // char (id) // "': impossible n_in")
    end select
    meta%id = id
    meta%run_id = run_id
    meta%lib => lib
    meta%lib_index = lib%get_entry_index (id)
    meta%num_id = lib%get_num_id (id)
    call lib%get_component_list (id, meta%component_id)
    meta%n_components = size (meta%component_id)
    call lib%get_component_description_list (id, meta%component_description)
    allocate (meta%active (meta%n_components), source = .true.)
  end subroutine process_metadata_init
  
  subroutine process_metadata_deactivate_component (meta, i)
    class(process_metadata_t), intent(inout) :: meta
    integer, intent(in) :: i
    call msg_message ("Process component '" &
         // char (meta%component_id(i)) // "': matrix element vanishes")
    meta%active(i) = .false.
  end subroutine process_metadata_deactivate_component
  
  subroutine process_config_data_write (config, u, &
       counters, os_data, rng_factory, model, expressions)
    class(process_config_data_t), intent(in) :: config
    integer, intent(in) :: u
    logical, intent(in) :: counters
    logical, intent(in) :: os_data
    logical, intent(in) :: rng_factory
    logical, intent(in) :: model
    logical, intent(in) :: expressions
    write (u, "(1x,A)") "Configuration data:"
    if (counters) then
       write (u, "(3x,A,I0)") "Number of incoming particles = ", &
            config%n_in
       write (u, "(3x,A,I0)") "Number of process components = ", &
            config%n_components
       write (u, "(3x,A,I0)") "Number of process terms      = ", &
            config%n_terms
       write (u, "(3x,A,I0)") "Number of MCI configurations = ", &
            config%n_mci
    end if
    if (os_data) then
       call os_data_write (config%os_data, u)
    end if
    if (associated (config%model)) then
       write (u, "(3x,A,A)")  "Model = ", char (config%model_name)
       if (model) then
          call write_separator (u)
          call config%model%write (u)
          call write_separator (u)
       end if
    else
       write (u, "(3x,A,A,A)")  "Model = ", char (config%model_name), &
            " [not associated]"
    end if
    call config%qcd%write (u, show_md5sum = .false.)
    if (rng_factory) then
       if (allocated (config%rng_factory)) then
          write (u, "(2x)", advance = "no")
          call config%rng_factory%write (u)
       end if
    end if
    call write_separator (u)
    if (expressions) then
       if (allocated (config%ef_cuts)) then
          call write_separator (u)
          write (u, "(3x,A)") "Cut expression:"
          call config%ef_cuts%write (u)
       end if
       if (allocated (config%ef_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Scale expression:"
          call config%ef_scale%write (u)
       end if
       if (allocated (config%ef_fac_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Factorization scale expression:"
          call config%ef_fac_scale%write (u)
       end if
       if (allocated (config%ef_ren_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Renormalization scale expression:"
          call config%ef_ren_scale%write (u)
       end if
       if (allocated (config%ef_weight)) then
          call write_separator (u)
          write (u, "(3x,A)") "Weight expression:"
          call config%ef_weight%write (u)
       end if
    else
       call write_separator (u)
       write (u, "(3x,A)") "Expressions (cut, scales, weight): [not shown]"
    end if
    if (config%md5sum /= "") then
       call write_separator (u)
       write (u, "(3x,A,A,A)")  "MD5 sum (config)  = '", config%md5sum, "'"
    end if
  end subroutine process_config_data_write
       
  subroutine process_config_data_init &
       (config, meta, os_data, qcd, rng_factory, model)
    class(process_config_data_t), intent(out) :: config
    type(process_metadata_t), intent(in) :: meta
    type(os_data_t), intent(in) :: os_data
    type(qcd_t), intent(in) :: qcd
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    class(model_data_t), intent(inout), pointer :: model
    config%n_in = meta%lib%get_n_in (meta%id)
    config%n_components = size (meta%component_id)
    config%os_data = os_data
    config%qcd = qcd
    call move_alloc (from = rng_factory, to = config%rng_factory)
    config%model_name = model%get_name ()
    config%model => model
    model => null ()
  end subroutine process_config_data_init

  subroutine process_config_data_final (config)
    class(process_config_data_t), intent(inout) :: config
    if (associated (config%model)) then
       call config%model%final ()
       deallocate (config%model)
    end if
  end subroutine process_config_data_final
  
  subroutine process_config_data_compute_md5sum (config)
    class(process_config_data_t), intent(inout) :: config
    integer :: u
    if (config%md5sum == "") then
       u = free_unit ()
       open (u, status = "scratch", action = "readwrite")
       call config%write (u, counters = .false., os_data = .false., &
            rng_factory = .false., model = .true., expressions = .true.)
       rewind (u)
       config%md5sum = md5sum (u)
       close (u)
    end if
  end subroutine process_config_data_compute_md5sum
  
  subroutine process_beam_config_write (object, u)
    class(process_beam_config_t), intent(in) :: object
    integer, intent(in) :: u
    integer :: i, c
    call object%data%write (u)
    if (object%data%initialized) then
       write (u, "(3x,A,L1)")  "Azimuthal dependence    = ", &
            object%azimuthal_dependence
       write (u, "(3x,A,L1)")  "Lab frame is c.m. frame = ", &
            object%lab_is_cm_frame
       if (object%md5sum /= "") then
          write (u, "(3x,A,A,A)")  "MD5 sum (beams/strf) = '", &
               object%md5sum, "'"
       end if
       if (allocated (object%sf)) then
          do i = 1, size (object%sf)
             call object%sf(i)%write (u)
          end do
          if (any_sf_channel_has_mapping (object%sf_channel)) then
             write (u, "(1x,A,L1)")  "Structure-function mappings per channel:"
             do c = 1, object%n_channel
                write (u, "(3x,I0,':')", advance="no")  c
                call object%sf_channel(c)%write (u)
             end do
          end if
       end if
    end if
  end subroutine process_beam_config_write
  
  subroutine process_beam_config_final (object)
    class(process_beam_config_t), intent(inout) :: object
    call beam_data_final (object%data)
  end subroutine process_beam_config_final

  subroutine process_beam_config_init_beam_structure &
       (beam_config, beam_structure, sqrts, model, decay_rest_frame)
    class(process_beam_config_t), intent(out) :: beam_config
    type(beam_structure_t), intent(in) :: beam_structure
    logical, intent(in), optional :: decay_rest_frame
    real(default), intent(in) :: sqrts
    class(model_data_t), intent(in), target :: model
    call beam_data_init_structure (beam_config%data, &
         beam_structure, sqrts, model, decay_rest_frame)
    beam_config%lab_is_cm_frame = beam_data_cm_frame (beam_config%data)
  end subroutine process_beam_config_init_beam_structure
  
  subroutine process_beam_config_init_scattering &
       (beam_config, flv_in, sqrts, beam_structure)
    class(process_beam_config_t), intent(out) :: beam_config
    type(flavor_t), dimension(2), intent(in) :: flv_in
    real(default), intent(in) :: sqrts
    type(beam_structure_t), intent(in), optional :: beam_structure
    if (present (beam_structure)) then
       if (beam_structure%polarized ()) then
          call beam_data_init_sqrts (beam_config%data, sqrts, flv_in, &
               beam_structure%get_smatrix (), beam_structure%get_pol_f ())
       else
          call beam_data_init_sqrts (beam_config%data, sqrts, flv_in)
       end if
    else
       call beam_data_init_sqrts (beam_config%data, sqrts, flv_in)
    end if
  end subroutine process_beam_config_init_scattering
    
  subroutine process_beam_config_init_decay &
       (beam_config, flv_in, rest_frame, beam_structure)
    class(process_beam_config_t), intent(out) :: beam_config
    type(flavor_t), dimension(1), intent(in) :: flv_in
    logical, intent(in), optional :: rest_frame
    type(beam_structure_t), intent(in), optional :: beam_structure
    if (present (beam_structure)) then
       if (beam_structure%polarized ()) then
          call beam_data_init_decay (beam_config%data, flv_in, &
               beam_structure%get_smatrix (), beam_structure%get_pol_f (), &
               rest_frame = rest_frame)
       else
          call beam_data_init_decay (beam_config%data, flv_in, &
               rest_frame = rest_frame)
       end if
    else
       call beam_data_init_decay (beam_config%data, flv_in, &
            rest_frame = rest_frame)
    end if 
    beam_config%lab_is_cm_frame = beam_data_cm_frame (beam_config%data)
  end subroutine process_beam_config_init_decay
    
  subroutine process_beam_config_startup_message &
       (beam_config, unit, beam_structure)
    class(process_beam_config_t), intent(in) :: beam_config
    integer, intent(in), optional :: unit
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer :: u
    u = free_unit ()
    open (u, status="scratch", action="readwrite")
    if (present (beam_structure)) then
       call beam_structure%write (u)
    end if
    call beam_data_write (beam_config%data, u)
    rewind (u)
    do
       read (u, "(1x,A)", end=1)  msg_buffer
       call msg_message ()
    end do
1   continue
    close (u)
  end subroutine process_beam_config_startup_message

  subroutine process_beam_config_init_sf_chain &
       (beam_config, sf_config, sf_trace_file)
    class(process_beam_config_t), intent(inout) :: beam_config
    type(sf_config_t), dimension(:), intent(in) :: sf_config
    type(string_t), intent(in), optional :: sf_trace_file
    integer :: i
    beam_config%n_strfun = size (sf_config)
    allocate (beam_config%sf (beam_config%n_strfun))
    do i = 1, beam_config%n_strfun
       associate (sf => sf_config(i))
         call beam_config%sf(i)%init (sf%i, sf%data)
         if (.not. sf%data%is_generator ()) then
            beam_config%n_sfpar = beam_config%n_sfpar + sf%data%get_n_par ()
         end if
       end associate
    end do
    if (present (sf_trace_file)) then
       beam_config%sf_trace = .true.
       beam_config%sf_trace_file = sf_trace_file
    end if
  end subroutine process_beam_config_init_sf_chain

  subroutine process_beam_config_allocate_sf_channels (beam_config, n_channel)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer, intent(in) :: n_channel
    beam_config%n_channel = n_channel
    call allocate_sf_channels (beam_config%sf_channel, &
         n_channel = n_channel, &
         n_strfun = beam_config%n_strfun)
  end subroutine process_beam_config_allocate_sf_channels
    
  subroutine process_beam_config_set_sf_channel (beam_config, c, sf_channel)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer, intent(in) :: c
    type(sf_channel_t), intent(in) :: sf_channel
    beam_config%sf_channel(c) = sf_channel
  end subroutine process_beam_config_set_sf_channel
  
  subroutine process_beam_config_sf_startup_message &
       (beam_config, sf_string, unit)
    class(process_beam_config_t), intent(in) :: beam_config
    type(string_t), intent(in) :: sf_string
    integer, intent(in), optional :: unit
    if (beam_config%n_strfun > 0) then
       call msg_message ("Beam structure: " // char (sf_string), unit = unit)
       write (msg_buffer, "(A,3(1x,I0,1x,A))") &
            "Beam structure:", &
            beam_config%n_channel, "channels,", &
            beam_config%n_sfpar, "dimensions"
       call msg_message (unit = unit)
       if (beam_config%sf_trace) then
          call msg_message ("Beam structure: tracing &
               &values in '" // char (beam_config%sf_trace_file) // "'")
       end if
    end if
  end subroutine process_beam_config_sf_startup_message
    
  function process_beam_config_get_pdf_set (beam_config) result (pdf_set)
    class(process_beam_config_t), intent(in) :: beam_config
    integer :: pdf_set
    integer :: i
    if (allocated (beam_config%sf)) then
       do i = 1, size (beam_config%sf)
          pdf_set = beam_config%sf(i)%get_pdf_set ()
          if (pdf_set /= 0)  return
       end do
    else
       pdf_set = 0
    end if
  end function process_beam_config_get_pdf_set
  
  subroutine process_beam_config_compute_md5sum (beam_config)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer :: u
    if (beam_config%md5sum == "") then
       u = free_unit ()
       open (u, status = "scratch", action = "readwrite")
       call beam_config%write (u)
       rewind (u)
       beam_config%md5sum = md5sum (u)
       close (u)
    end if
  end subroutine process_beam_config_compute_md5sum

  subroutine process_counter_write (object, unit)
    class(process_counter_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (object%total > 0) then
       write (u, "(1x,A)")  "Call statistics (current run):"
       write (u, "(3x,A,I0)")  "total       = ", object%total
       write (u, "(3x,A,I0)")  "failed kin. = ", object%failed_kinematics
       write (u, "(3x,A,I0)")  "failed cuts = ", object%failed_cuts
       write (u, "(3x,A,I0)")  "passed cuts = ", object%passed
       write (u, "(3x,A,I0)")  "evaluated   = ", object%evaluated
    else
       write (u, "(1x,A)")  "Call statistics (current run): [no calls]"
    end if
  end subroutine process_counter_write
    
  subroutine process_counter_reset (counter)
    class(process_counter_t), intent(out) :: counter
  end subroutine process_counter_reset

  subroutine process_counter_record (counter, status)
    class(process_counter_t), intent(inout) :: counter
    integer, intent(in) :: status
    if (status <= STAT_FAILED_KINEMATICS) then
       counter%failed_kinematics = counter%failed_kinematics + 1
    else if (status <= STAT_FAILED_CUTS) then
       counter%failed_cuts = counter%failed_cuts + 1
    else if (status <= STAT_PASSED_CUTS) then
       counter%passed = counter%passed + 1
    else
       counter%evaluated = counter%evaluated + 1
    end if
    counter%total = counter%total + 1
  end subroutine process_counter_record
       
  subroutine process_mci_entry_final (object)
    class(process_mci_entry_t), intent(inout) :: object
    if (allocated (object%mci))  call object%mci%final ()
  end subroutine process_mci_entry_final
  
  subroutine process_mci_entry_write (object, unit, pacify)
    class(process_mci_entry_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A,I0)")  "Associated components = ", object%i_component
    write (u, "(3x,A,I0)")  "MC input parameters   = ", object%n_par
    write (u, "(3x,A,I0)")  "MC parameters (SF)    = ", object%n_par_sf
    write (u, "(3x,A,I0)")  "MC parameters (PHS)   = ", object%n_par_phs
    if (object%pass > 0) then
       write (u, "(3x,A,I0)")  "Current pass          = ", object%pass
       write (u, "(3x,A,I0)")  "Number of iterations  = ", object%n_it
       write (u, "(3x,A,I0)")  "Number of calls       = ", object%n_calls
    end if
    if (object%md5sum /= "") then
       write (u, "(3x,A,A,A)") "MD5 sum (components)  = '", object%md5sum, "'"
    end if
    if (allocated (object%mci)) then
       call object%mci%write (u)
    end if
    call object%counter%write (u)
    if (object%results%exist ()) then
       call object%results%write (u, suppress = pacify)
       call object%results%write_chain_weights (u)
    end if
  end subroutine process_mci_entry_write
       
  subroutine process_mci_entry_write_chain_weights (mci_entry, unit)
    class(process_mci_entry_t), intent(in) :: mci_entry
    integer, intent(in), optional :: unit
    if (allocated (mci_entry%mci)) then
       call mci_entry%mci%write_chain_weights (unit)
    end if
  end subroutine process_mci_entry_write_chain_weights
       
  subroutine process_mci_entry_init (mci_entry, &
       process_type, i_mci, i_component, component,&
       beam_config, rng_factory)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    integer, intent(in) :: process_type
    integer, intent(in) :: i_mci
    integer, intent(in) :: i_component
    type(process_component_t), intent(in), target :: component
    type(process_beam_config_t), intent(in) :: beam_config
    class(rng_factory_t), intent(inout) :: rng_factory
    class(rng_t), allocatable :: rng
    associate (phs_config => component%phs_config)
      mci_entry%i_mci = i_mci
      call mci_entry%create_component_list (i_component, component)
      mci_entry%n_par_sf = beam_config%n_sfpar
      mci_entry%n_par_phs = phs_config%get_n_par () 
      mci_entry%n_par = mci_entry%n_par_sf + mci_entry%n_par_phs
      mci_entry%process_type = process_type
      if (allocated (component%mci_template)) then
         allocate (mci_entry%mci, source=component%mci_template)
         call mci_entry%mci%record_index (mci_entry%i_mci)
         call mci_entry%mci%set_dimensions &
              (mci_entry%n_par, phs_config%get_n_channel ())
         call mci_entry%mci%declare_flat_dimensions &
              (phs_config%get_flat_dimensions ())
         if (phs_config%provides_equivalences) then
            call mci_entry%mci%declare_equivalences &
                 (phs_config%channel, mci_entry%n_par_sf)
         end if
         if (phs_config%provides_chains) then
            call mci_entry%mci%declare_chains (phs_config%chain)
         end if
         call rng_factory%make (rng)
         call mci_entry%mci%import_rng (rng)
      end if
    end associate
    call mci_entry%results%init (process_type)
  end subroutine process_mci_entry_init
  
  subroutine process_mci_entry_create_component_list (mci_entry, &
                                     i_component, component)
    class (process_mci_entry_t), intent(inout) :: mci_entry
    integer, intent(in) :: i_component
    type(process_component_t), intent(in), target :: component
    integer, dimension(4) :: i_list
    if (mci_entry%combined_integration) then
      allocate (mci_entry%i_component (3))
      i_list = component%config%get_association_list ()
      mci_entry%i_component = i_list(1:3)
    else
      allocate (mci_entry%i_component (1))
      mci_entry%i_component(1) = i_component
    end if
  end subroutine process_mci_entry_create_component_list

  subroutine process_mci_entry_set_combined_integration (mci_entry, value)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    logical, intent(in), optional :: value
    if (present (value)) &
       mci_entry%combined_integration = value
  end subroutine process_mci_entry_set_combined_integration

  subroutine process_mci_entry_set_parameters (mci_entry, var_list)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(var_list_t), intent(in) :: var_list
    real(default) :: error_threshold
    error_threshold = &
         var_list%get_rval (var_str ("error_threshold"))
    mci_entry%activate_timer = &
         var_list%get_lval (var_str ("?integration_timer"))
    call mci_entry%results%set_error_threshold (error_threshold)
  end subroutine process_mci_entry_set_parameters
  
  subroutine process_mci_entry_compute_md5sum (mci_entry, &
       config, component, beam_config)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_config_data_t), intent(in) :: config
    type(process_component_t), dimension(:), intent(in) :: component
    type(process_beam_config_t), intent(in) :: beam_config
    type(string_t) :: buffer
    integer :: i
    if (mci_entry%md5sum == "") then
       buffer = config%md5sum // beam_config%md5sum
       do i = 1, size (component)
          if (component(i)%active) then
             buffer = buffer // component(i)%config%get_md5sum () &
                  // component(i)%md5sum_phs
          end if
       end do
       mci_entry%md5sum = md5sum (char (buffer))
    end if
    if (allocated (mci_entry%mci)) then
       call mci_entry%mci%set_md5sum (mci_entry%md5sum)
    end if
  end subroutine process_mci_entry_compute_md5sum
  
  subroutine process_mci_entry_sampler_test (mci_entry, instance, n_calls)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_instance_t), intent(inout), target :: instance
    integer, intent(in) :: n_calls
    call instance%choose_mci (mci_entry%i_mci)
    call instance%reset_counter ()
    call mci_entry%mci%sampler_test (instance, n_calls)
    mci_entry%counter = instance%get_counter ()
  end subroutine process_mci_entry_sampler_test

  subroutine process_mci_entry_integrate (mci_entry, instance, n_it, n_calls, &
       adapt_grids, adapt_weights, final, pacify, &
       i_component)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: adapt_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: final, pacify
    integer, intent(in), optional :: i_component
    integer :: u_log
    integer :: nlo_type
    if (present (i_component)) then
      if (associated (instance%component(i_component)%config)) then
        nlo_type = instance%component(i_component)%config%config%get_nlo_type ()
      end if
    else
      nlo_type = BORN
    end if
    u_log = logfile_unit ()
    call instance%choose_mci (mci_entry%i_mci)
    call instance%reset_counter ()
    mci_entry%pass = mci_entry%pass + 1
    mci_entry%n_it = n_it
    mci_entry%n_calls = n_calls
    if (mci_entry%pass == 1)  &
         call mci_entry%mci%startup_message (n_calls = n_calls)
    call mci_entry%mci%set_timer (active = mci_entry%activate_timer)
    call mci_entry%results%display_init &
         (mci_entry%process_type, screen = .true., unit = u_log)
    call mci_entry%results%new_pass ()
    associate (mci_instance => instance%mci_work(mci_entry%i_mci)%mci)
      select case (nlo_type)
      case (NLO_VIRTUAL, NLO_REAL, NLO_PDF)
        select type (mci_instance)
        type is (mci_vamp_instance_t)
          mci_instance%negative_weights = .true.
        end select
      end select
      call mci_entry%mci%add_pass (adapt_grids, adapt_weights, final)
      call mci_entry%mci%start_timer ()
      call mci_entry%mci%integrate (mci_instance, instance, n_it, &
           n_calls, mci_entry%results, pacify = pacify)
      call mci_entry%mci%stop_timer ()
      if (signal_is_pending ())  return
    end associate
    mci_entry%counter = instance%get_counter ()
    call mci_entry%results%display_pass (pacify)
  end subroutine process_mci_entry_integrate

  subroutine process_mci_entry_final_integration (mci_entry)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    call mci_entry%results%display_final ()
    call mci_entry%time_message ()
  end subroutine process_mci_entry_final_integration

  subroutine process_mci_entry_get_time (mci_entry, time, sample)
    class(process_mci_entry_t), intent(in) :: mci_entry
    type(time_t), intent(out) :: time
    integer, intent(in) :: sample
    real(default) :: time_last_pass, efficiency, calls
    time_last_pass = mci_entry%mci%get_time ()
    calls = mci_entry%results%get_n_calls ()
    efficiency = mci_entry%mci%get_efficiency ()
    if (time_last_pass > 0 .and. calls > 0 .and. efficiency > 0) then
       time = nint (time_last_pass / calls / efficiency * sample)
    end if
  end subroutine process_mci_entry_get_time   

  subroutine process_mci_entry_time_message (mci_entry)
    class(process_mci_entry_t), intent(in) :: mci_entry
    type(time_t) :: time
    integer :: sample
    sample = 10000
    call mci_entry%get_time (time, sample)
    if (time%is_known ()) then
       call msg_message ("Time estimate for generating 10000 events: " &
            // char (time%to_string_dhms ()))
    end if
  end subroutine process_mci_entry_time_message
  
  subroutine process_mci_entry_prepare_simulation (mci_entry)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    call mci_entry%mci%prepare_simulation ()
  end subroutine process_mci_entry_prepare_simulation

  subroutine process_mci_entry_generate_weighted_event (mci_entry, instance)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_instance_t), intent(inout) :: instance
    call instance%choose_mci (mci_entry%i_mci)
    associate (mci_instance => instance%mci_work(mci_entry%i_mci)%mci)
      REJECTION: do
         call mci_entry%mci%generate_weighted_event (mci_instance, instance)
         if (signal_is_pending ())  return
         if (instance%is_valid ())  exit REJECTION
      end do REJECTION
    end associate
  end subroutine process_mci_entry_generate_weighted_event
  
  subroutine process_mci_entry_generate_unweighted_event (mci_entry, instance)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_instance_t), intent(inout) :: instance
    call instance%choose_mci (mci_entry%i_mci)
    associate (mci_instance => instance%mci_work(mci_entry%i_mci)%mci)
      call mci_entry%mci%generate_unweighted_event (mci_instance, instance)
    end associate
  end subroutine process_mci_entry_generate_unweighted_event
  
  subroutine process_mci_entry_recover_event (mci_entry, instance, i_term)
    class(process_mci_entry_t), intent(inout) :: mci_entry
    type(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: channel
    mci_entry%i_mci = instance%i_mci
    channel = instance%get_channel ()
    associate (mci_instance => instance%mci_work(mci_entry%i_mci)%mci)
      call mci_instance%fetch (instance, channel)
    end associate
  end subroutine process_mci_entry_recover_event
  
  function process_mci_entry_has_integral (mci_entry) result (flag)
    class(process_mci_entry_t), intent(in) :: mci_entry
    logical :: flag
    flag = mci_entry%results%exist ()
  end function process_mci_entry_has_integral
    
  function process_mci_entry_get_integral (mci_entry) result (integral)
    class(process_mci_entry_t), intent(in) :: mci_entry
    real(default) :: integral
    integral = mci_entry%results%get_integral ()
  end function process_mci_entry_get_integral

  function process_mci_entry_get_error (mci_entry) result (error)
    class(process_mci_entry_t), intent(in) :: mci_entry
    real(default) :: error
    error = mci_entry%results%get_error ()
  end function process_mci_entry_get_error
  
  function process_mci_entry_get_accuracy (mci_entry) result (accuracy)
    class(process_mci_entry_t), intent(in) :: mci_entry
    real(default) :: accuracy
    accuracy = mci_entry%results%get_accuracy ()
  end function process_mci_entry_get_accuracy
  
  function process_mci_entry_get_chi2 (mci_entry) result (chi2)
    class(process_mci_entry_t), intent(in) :: mci_entry
    real(default) :: chi2
    chi2 = mci_entry%results%get_chi2 ()
  end function process_mci_entry_get_chi2

  function process_mci_entry_get_efficiency (mci_entry) result (efficiency)
    class(process_mci_entry_t), intent(in) :: mci_entry
    real(default) :: efficiency
    efficiency = mci_entry%results%get_efficiency ()
  end function process_mci_entry_get_efficiency  
  
  function process_mci_entry_get_md5sum (entry) result (md5sum)
    class(process_mci_entry_t), intent(in) :: entry
    character(32) :: md5sum
    md5sum = entry%mci%get_md5sum ()
  end function process_mci_entry_get_md5sum
  
  subroutine process_component_final (object)
    class(process_component_t), intent(inout) :: object
    if (allocated (object%mci_template)) then
       call object%mci_template%final ()
    end if
    if (allocated (object%phs_config)) then
       call object%phs_config%final ()
    end if
  end subroutine process_component_final
  
  subroutine process_component_write (object, unit)
    class(process_component_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (allocated (object%core)) then
       write (u, "(1x,A,I0)")  "Component #", object%index
       if (associated (object%config)) then
          call object%config%write (u)
          if (object%md5sum_phs /= "") then
             write (u, "(3x,A,A,A)")  "MD5 sum (phs)       = '", &
                  object%md5sum_phs, "'"
          end if
       end if
       write (u, "(1x,A)") "Process core:"
       call object%core%write (u)
    else
       write (u, "(1x,A)") "Process component: [not allocated]"
    end if
    if (.not. object%active) then
       write (u, "(1x,A)") "[Inactive]"
       return
    end if
    write (u, "(1x,A)") "Referenced data:"
    if (allocated (object%i_term)) then
       write (u, "(3x,A,999(1x,I0))") "Terms                    =", &
            object%i_term
    else
       write (u, "(3x,A)") "Terms                    = [undefined]"
    end if
    if (object%i_mci /= 0) then
       write (u, "(3x,A,I0)") "MC dataset               = ", object%i_mci
    else
       write (u, "(3x,A)") "MC dataset               = [undefined]"
    end if
    if (allocated (object%phs_config)) then
       call object%phs_config%write (u)
    end if
  end subroutine process_component_write

  subroutine process_component_init (component, &
       i_component, meta, config, &
       core_template, mci_template, phs_config_template, &
       blha_template, fks_template)
    class(process_component_t), intent(out) :: component
    integer, intent(in) :: i_component
    type(process_metadata_t), intent(in) :: meta
    type(process_config_data_t), intent(in) :: config
    class(prc_core_t), intent(in), allocatable :: core_template
    class(mci_t), intent(in), allocatable :: mci_template
    class(phs_config_t), intent(in), allocatable :: phs_config_template
    type(blha_template_t), intent(inout), optional :: blha_template
    type(fks_template_t), intent(in), optional :: fks_template

    component%index = i_component
    component%config => meta%lib%get_component_def_ptr (meta%id, i_component)
    allocate (component%core, source=core_template)
    call component%core%init (component%config%get_core_def_ptr (), &
         meta%lib, meta%id, i_component)
    if (present (fks_template)) then
      component%fks_template = fks_template
    end if
    select type (core => component%core)
    class is (prc_blha_t)
      if (present (blha_template)) then
        call core%init_blha (blha_template)
      else
        call msg_bug ("process_component_init: BLHA core cannot be initialized - &
                       & missing template")
      end if
      call core%init_driver (config%os_data)
!      call core%set_initialized ()
    end select
    component%active = component%core%has_matrix_element ()
    if (component%active) then
       if (allocated (mci_template)) &
            allocate (component%mci_template, source=mci_template)
       allocate (component%phs_config, source=phs_config_template)
       call component%phs_config%init (component%core%data, config%model)
    end if
  end subroutine process_component_init

  subroutine process_component_configure_phs &
       (component, sqrts, beam_config, rebuild, &
        ignore_mismatch, phs_config, verbose)
    class(process_component_t), intent(inout) :: component
    real(default), intent(in) :: sqrts
    type(process_beam_config_t), intent(in) :: beam_config
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    type(phs_wood_config_t), intent(in), optional, target :: phs_config
    logical, intent(in), optional :: verbose
    logical :: no_strfun, verb
    integer :: nlo_type
    no_strfun = beam_config%n_strfun == 0
    verb = .true.;  if (present (verbose))  verb = verbose
    nlo_type = component%config%get_nlo_type ()
    if (present (phs_config)) then
       select type (config => component%phs_config)
       type is (phs_fks_config_t)
          call config%set_born_config (phs_config)
          call config%set_mode (nlo_type)
       end select
    end if
    call component%phs_config%configure (sqrts, &
         azimuthal_dependence = beam_config%azimuthal_dependence, &
         sqrts_fixed = no_strfun, &
         cm_frame = beam_config%lab_is_cm_frame .and. no_strfun, &
         rebuild = rebuild, ignore_mismatch = ignore_mismatch, &
         nlo_type = nlo_type)
    if (verb)  call component%phs_config%startup_message ()
  end subroutine process_component_configure_phs
    
  subroutine process_component_increase_phs_par (component)
    class(process_component_t), intent(inout) :: component
    select type (phs_config => component%phs_config)
    type is (phs_wood_config_t)
       call phs_config%increase_n_par ()
    end select
  end subroutine process_component_increase_phs_par

  subroutine process_component_compute_md5sum (component)
    class(process_component_t), intent(inout) :: component
    component%md5sum_phs = component%phs_config%get_md5sum ()
  end subroutine process_component_compute_md5sum
  
  subroutine process_component_collect_channels (component, coll)
    class(process_component_t), intent(inout) :: component
    type(phs_channel_collection_t), intent(inout) :: coll
    call component%phs_config%collect_channels (coll)
  end subroutine process_component_collect_channels
    
  function process_component_get_n_phs_par (component) result (n_par)
    class(process_component_t), intent(in) :: component
    integer :: n_par
    n_par = component%phs_config%get_n_par ()
  end function process_component_get_n_phs_par
    
  function process_component_get_pdg_in (component) result (pdg_in)
    class(process_component_t), intent(in) :: component
    type(pdg_array_t), dimension(:), allocatable :: pdg_in
    type(pdg_array_t) :: pdg_tmp
    integer :: i
    associate (data => component%core%data)
      allocate (pdg_in (data%n_in))
      do i = 1, data%n_in
         pdg_tmp = data%flv_state(i,:)
         pdg_in(i) = sort_abs (pdg_tmp, unique = .true.)
      end do
    end associate
  end function process_component_get_pdg_in
  
  subroutine process_component_extract_phs_config (component, phs_config)
    class(process_component_t), intent(inout) :: component
    class(phs_config_t), intent(inout), allocatable :: phs_config
    call move_alloc (from = component%phs_config, to = phs_config)
  end subroutine process_component_extract_phs_config

  subroutine process_component_restore_phs_config (component, phs_config)
    class(process_component_t), intent(inout) :: component
    class(phs_config_t), intent(inout), allocatable :: phs_config
    call move_alloc (from = phs_config, to = component%phs_config)
  end subroutine process_component_restore_phs_config

  subroutine process_term_write (term, unit)
    class(process_term_t), intent(in) :: term
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A,I0)")  "Term #", term%i_term_global
    write (u, "(3x,A,I0)")  "Process component index      = ", &
         term%i_component
    write (u, "(3x,A,I0)")  "Term index w.r.t. component  = ", &
         term%i_term
    write (u, "(3x,A,L1)")  "Rearrange partons            = ", &
         term%rearrange
    call write_separator (u)
    write (u, "(1x,A)")  "Hard interaction:"
    call write_separator (u)
    call term%int%basic_write (u)
    if (term%rearrange) then
       call write_separator (u)
       write (u, "(1x,A)")  "Rearranged hard interaction:"
       call write_separator (u)
       call term%int_eff%basic_write (u)
    end if
  end subroutine process_term_write
     
  subroutine process_term_write_state_summary (term, core, unit)
    class(process_term_t), intent(in) :: term
    class(prc_core_t), intent(in) :: core
    integer, intent(in), optional :: unit
    integer :: u, i, f, h, c
    type(state_iterator_t) :: it
    character :: sgn
    u = given_output_unit (unit)
    write (u, "(1x,A,I0)")  "Term #", term%i_term_global
    call it%init (term%int%get_state_matrix_ptr ())
    do while (it%is_valid ())
       i = it%get_me_index ()
       f = term%flv(i)
       h = term%hel(i)
       c = term%col(i)
       if (core%is_allowed (term%i_term, f, h, c)) then
          sgn = "+"
       else
          sgn = " "
       end if
       write (u, "(1x,A1,1x,I0,2x)", advance="no")  sgn, i
       call quantum_numbers_write (it%get_quantum_numbers (), u)
       write (u, *)
       call it%advance ()
    end do
  end subroutine process_term_write_state_summary
  
  subroutine process_term_final (term)
    class(process_term_t), intent(inout) :: term
    call term%int%final ()
    if (term%rearrange) then
       call term%int_eff%final ()
       deallocate (term%int_eff)
    end if
  end subroutine process_term_final

  subroutine process_term_init &
       (term, i_term_global, i_component, i_term, core, model)
    class(process_term_t), intent(inout), target :: term
    integer, intent(in) :: i_term_global
    integer, intent(in) :: i_component
    integer, intent(in) :: i_term
    class(prc_core_t), intent(in) :: core
    class(model_data_t), intent(in), target :: model
    class(modelpar_data_t), pointer :: alpha_s_ptr
!    type(var_list_t), pointer :: var_list
    term%i_term_global = i_term_global
    term%i_component = i_component
    term%i_term = i_term
    call core%get_constants (term%data, i_term)
!    var_list => model%get_var_list_ptr ()
!    if (var_list%contains (var_str ("alphas"))) then
!       term%alpha_s = var_list%get_rval (var_list, var_str ("alphas"))
!    else
!       term%alpha_s = -1
!    end if
    alpha_s_ptr => model%get_par_data_ptr (var_str ("alphas"))
    if (associated (alpha_s_ptr)) then
       term%alpha_s = alpha_s_ptr%get_real ()
    else
       term%alpha_s = -1
    end if

    call term%setup_interaction (core, model)
!     if (term%rearrange) then
!       call term%setup_effective_interaction (core, term%int, term%int_eff)
!     end if
  end subroutine process_term_init
    
  subroutine process_term_setup_interaction (term, core, model)
    class(process_term_t), intent(inout) :: term
    class(prc_core_t), intent(in) :: core
    class(model_data_t), intent(in), target :: model
    integer :: n_tot
    type(flavor_t), dimension(:), allocatable :: flv
    type(color_t), dimension(:), allocatable :: col
    type(helicity_t), dimension(:), allocatable :: hel
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, n, f, h, c
    associate (data => term%data)
      n_tot = data%n_in + data%n_out
      n = 0
      do f = 1, data%n_flv
         do h = 1, data%n_hel
            do c = 1, data%n_col
               if (core%is_allowed (term%i_term, f, h, c))  n = n + 1
            end do
         end do
      end do
      allocate (term%flv (n), term%col (n), term%hel (n))
      term%n_allowed = n
      allocate (flv (n_tot), col (n_tot), hel (n_tot))
      allocate (qn (n_tot))
      call term%int%basic_init &
           (data%n_in, 0, data%n_out, set_relations=.true.)
      i = 0
      do f = 1, data%n_flv
         do h = 1, data%n_hel
            do c = 1, data%n_col
               if (core%is_allowed (term%i_term, f, h, c)) then
                  i = i + 1
                  term%flv(i) = f
                  term%hel(i) = h
                  term%col(i) = c
                  call flv%init (data%flv_state(:,f), model)
                  call color_init_from_array (col, &
                       data%col_state(:,:,c), &
                       data%ghost_flag(:,c))
                  call col(:data%n_in)%invert ()
                  call hel%init (data%hel_state(:,h))
                  call qn%init (flv, col, hel)
                  call term%int%add_state (qn)
               end if
            end do
         end do
      end do
      call term%int%freeze ()
    end associate
  end subroutine process_term_setup_interaction
  
   subroutine process_term_fetch_process_constants &
       (term, prc_constants)
    class(process_term_t), intent(inout) :: term
    type(process_constants_t), intent(out) :: prc_constants
    prc_constants = term%data
  end subroutine process_term_fetch_process_constants

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
    class(process_t), intent(in) :: process
    integer, intent(in) :: i_component
    type(process_component_def_t), pointer :: ptr
    ptr => process%meta%lib%get_component_def_ptr (process%meta%id, i_component)
  end function process_get_component_def_ptr
  
  subroutine process_extract_component_core (process, i_component, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_component
    class(prc_core_t), intent(inout), allocatable :: core
    call move_alloc (from = process%component(i_component)%core, to = core)
  end subroutine process_extract_component_core
    
  subroutine process_restore_component_core (process, i_component, core)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_component
    class(prc_core_t), intent(inout), allocatable :: core
    call move_alloc (from = core, to = process%component(i_component)%core)
  end subroutine process_restore_component_core
    
  function process_get_constants (process, i) result (data)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i
    type(process_constants_t) :: data
    data = process%component(i)%core%data
  end function process_get_constants
  
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
    sqrts = beam_data_get_sqrts (process%beam_config%data)
  end function process_get_sqrts
  
  function process_has_matrix_element (process, i) result (flag)
    class(process_t), intent(in) :: process
    integer, intent(in), optional :: i
    logical :: flag
    if (present (i)) then
       flag = process%component(i)%active
    else
       flag = any (process%component%active)
    end if
  end function process_has_matrix_element
  
  function process_get_beam_data_ptr (process) result (beam_data)
    class(process_t), intent(in), target :: process
    type(beam_data_t), pointer :: beam_data
    beam_data => process%beam_config%data
  end function process_get_beam_data_ptr

  function process_cm_frame (process) result (flag)
    class(process_t), intent(in), target :: process
    logical :: flag
    type(beam_data_t), pointer :: beam_data
    beam_data => process%beam_config%data
    flag = beam_data_cm_frame (beam_data)
  end function process_cm_frame
  
  function process_get_pdf_set (process) result (pdf_set)
    class(process_t), intent(in) :: process
    integer :: pdf_set
    pdf_set = process%beam_config%get_pdf_set ()
  end function process_get_pdf_set
  
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
       (process, i, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced) &
       result (amp)
    class(process_t), intent(in) :: process
    integer, intent(in) :: i, j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in), optional :: fac_scale, ren_scale
    real(default), intent(in), allocatable, optional :: alpha_qcd_forced
    real(default) :: fscale, rscale
    real(default), allocatable :: aqcd_forced
    complex(default) :: amp
    amp = 0
    if (0 < i .and. i <= process%meta%n_components) then
       if (process%component(i)%active) then
          associate (data => process%component(i)%core%data)
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
               amp = process%component(i)%core%compute_amplitude &
                    (j, p, f, h, c, fscale, rscale, aqcd_forced)
            end if
          end associate
       else
          amp = 0
       end if
    end if
  end function process_compute_amplitude

  subroutine process_set_component_type (process, i_component, i_type)
    class(process_t), intent(inout) :: process
    integer, intent(in) :: i_component, i_type
    process%component(i_component)%component_type = i_type
  end subroutine process_set_component_type

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
             if (process%mci_entry(i)%mci%error_known .and. err_reset) &
               process%mci_entry(i)%mci%error = 0
             if (process%mci_entry(i)%mci%efficiency_known .and. &
               eff_reset)  process%mci_entry(i)%mci%efficiency = 1             
             select type (mci => process%mci_entry(i)%mci) 
             type is (mci_vamp_t)
                call mci%pacify (efficiency_reset, error_reset)
                call mci%compute_md5sum ()
             end select
          end if
       end do
    end if
  end subroutine process_pacify

  subroutine kinematics_write (object, unit)
    class(kinematics_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, c
    u = given_output_unit (unit)
    if (object%f_allocated) then
       write (u, "(1x,A)")  "Flux * PHS volume:"
       write (u, "(2x,ES19.12)")  object%phs_factor
       write (u, "(1x,A)")  "Jacobian factors per channel:"
       do c = 1, size (object%f)
          write (u, "(3x,I0,':',1x,ES13.7)", advance="no")  c, object%f(c)
          if (c == object%selected_channel) then
             write (u, "(1x,A)")  "[selected]"
          else
             write (u, *)
          end if
       end do
    end if
    if (object%sf_chain_allocated) then
       call write_separator (u)
       call object%sf_chain%write (u)
    end if
    if (object%phs_allocated) then
       call write_separator (u)
       call object%phs%write (u)
    end if
  end subroutine kinematics_write
    
  subroutine kinematics_final (object)
    class(kinematics_t), intent(inout) :: object
    if (object%sf_chain_allocated) then
       call object%sf_chain%final ()
       deallocate (object%sf_chain)
       object%sf_chain_allocated = .false.
    end if
    if (object%phs_allocated) then
       call object%phs%final ()
       deallocate (object%phs)
       object%phs_allocated = .false.
    end if
    if (object%f_allocated) then
       deallocate (object%f)
       object%f_allocated = .false.
    end if
  end subroutine kinematics_final
  
  subroutine kinematics_set_nlo_info (k, nlo_type, emitter)
    class(kinematics_t), intent(inout) :: k
    integer, intent(in) :: nlo_type
    integer, intent(in), optional :: emitter
    k%nlo_type = nlo_type
    if (present (emitter)) then
      k%emitter = emitter
    end if
  end subroutine kinematics_set_nlo_info

  subroutine kinematics_init_sf_chain (k, core, sf_chain, config, core_state)
    class(kinematics_t), intent(inout) :: k
    class(prc_core_t), intent(in) :: core
    type(sf_chain_t), intent(in), target :: sf_chain
    type(process_beam_config_t), intent(in) :: config
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    integer :: n_strfun, n_channel
    integer :: c
    k%n_in = beam_data_get_n_in (config%data)
    n_strfun = config%n_strfun
    n_channel = config%n_channel
    allocate (k%sf_chain)
    k%sf_chain_allocated = .true.
    call core%init_sf_chain (k%sf_chain, sf_chain, n_channel, core_state)
    if (n_strfun /= 0) then
       do c = 1, n_channel
          call k%sf_chain%set_channel (c, config%sf_channel(c))
       end do
    end if
    call k%sf_chain%link_interactions ()
    call k%sf_chain%exchange_mask ()
    call k%sf_chain%init_evaluators ()
  end subroutine kinematics_init_sf_chain

  subroutine kinematics_init_phs (k, config)
    class(kinematics_t), intent(inout) :: k
    class(phs_config_t), intent(in), target :: config
    k%n_channel = config%get_n_channel ()
    call config%allocate_instance (k%phs)
    call k%phs%init (config)
    k%phs_allocated = .true.
    allocate (k%f (k%n_channel))
    k%f = 0
    k%f_allocated = .true.
  end subroutine kinematics_init_phs
    
  subroutine kinematics_init_ptr (k, k_in)
    class(kinematics_t), intent(out) :: k
    type(kinematics_t), intent(in) :: k_in
    k%n_in = k_in%n_in
    k%n_channel = k_in%n_channel
    k%sf_chain => k_in%sf_chain
    k%phs => k_in%phs
    k%f => k_in%f
  end subroutine kinematics_init_ptr
  
  subroutine kinematics_compute_selected_channel &
       (k, mci_work, phs_channel, p, success, nlo_controller)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    type(vector4_t), dimension(:), intent(out) :: p
    logical, intent(out) :: success
    type(nlo_controller_t), intent(inout), optional :: nlo_controller
    integer :: sf_channel
    k%selected_channel = phs_channel
    sf_channel = k%phs%config%get_sf_channel (phs_channel)
    call k%sf_chain%compute_kinematics (sf_channel, mci_work%get_x_strfun ())
    call k%sf_chain%get_out_momenta (p(1:k%n_in))
    call k%phs%set_incoming_momenta (p(1:k%n_in))
    call k%phs%compute_flux ()
    call k%phs%select_channel (phs_channel)
    call k%phs%evaluate_selected_channel &
           (phs_channel, mci_work%get_x_process ())

    select type (phs => k%phs)
    type is (phs_fks_t)
      if (phs%q_defined) then
         call phs%get_born_momenta (p)
         k%phs_factor = phs%get_overall_factor ()
         success = .true.
      else
         k%phs_factor = 0
         success = .false.
      end if
    class default
      if (phs%q_defined) then
         call k%phs%get_outgoing_momenta (p(k%n_in+1:))
         k%phs_factor = k%phs%get_overall_factor ()
         success = .true.
      else
         k%phs_factor = 0
         success = .false.
      end if
    end select
  end subroutine kinematics_compute_selected_channel
  
  subroutine kinematics_compute_other_channels (k, mci_work, phs_channel)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    integer :: c, c_sf
    call k%phs%evaluate_other_channels (phs_channel)
    do c = 1, k%n_channel
       c_sf = k%phs%config%get_sf_channel (c)
       k%f(c) = k%sf_chain%get_f (c_sf) * k%phs%get_f (c)
    end do
  end subroutine kinematics_compute_other_channels
  
  subroutine kinematics_get_incoming_momenta (k, p)
    class(kinematics_t), intent(in) :: k
    type(vector4_t), dimension(:), intent(out) :: p
    type(interaction_t), pointer :: int
    integer :: i
    int => k%sf_chain%get_out_int_ptr ()
    do i = 1, k%n_in
       p(i) = int%get_momentum (k%sf_chain%get_out_i (i))
    end do
  end subroutine kinematics_get_incoming_momenta
  
  subroutine kinematics_recover_mcpar (k, mci_work, phs_channel, p)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(inout) :: mci_work
    integer, intent(in) :: phs_channel
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: c, c_sf
    real(default), dimension(:), allocatable :: x_sf, x_phs
    c = phs_channel
    c_sf = k%phs%config%get_sf_channel (c)
    k%selected_channel = c
    call k%sf_chain%recover_kinematics (c_sf)
    call k%phs%set_incoming_momenta (p(1:k%n_in))
    call k%phs%compute_flux ()
    call k%phs%set_outgoing_momenta (p(k%n_in+1:))
    call k%phs%inverse ()
    do c = 1, k%n_channel
       c_sf = k%phs%config%get_sf_channel (c)
       k%f(c) = k%sf_chain%get_f (c_sf) * k%phs%get_f (c)
    end do
    k%phs_factor = k%phs%get_overall_factor ()
    c = phs_channel
    c_sf = k%phs%config%get_sf_channel (c)
    allocate (x_sf (k%sf_chain%config%get_n_bound ()))
    allocate (x_phs (k%phs%config%get_n_par ()))
    call k%phs%select_channel (c)
    call k%sf_chain%get_mcpar (c_sf, x_sf)
    call k%phs%get_mcpar (c, x_phs)
    call mci_work%set_x_strfun (x_sf)
    call mci_work%set_x_process (x_phs)
  end subroutine kinematics_recover_mcpar

  subroutine kinematics_get_mcpar (k, phs_channel, r)
    class(kinematics_t), intent(in) :: k
    integer, intent(in) :: phs_channel
    real(default), dimension(:), intent(out) :: r
    integer :: sf_channel, n_par_sf, n_par_phs
    sf_channel = k%phs%config%get_sf_channel (phs_channel)
    n_par_phs = k%phs%config%get_n_par ()
    n_par_sf = k%sf_chain%config%get_n_bound ()
    if (n_par_sf > 0) then
       call k%sf_chain%get_mcpar (sf_channel, r(1:n_par_sf))
    end if
    if (n_par_phs > 0) then
       call k%phs%get_mcpar (phs_channel, r(n_par_sf+1:))
    end if
  end subroutine kinematics_get_mcpar
  
  subroutine kinematics_evaluate_sf_chain (k, fac_scale)
    class(kinematics_t), intent(inout) :: k
    real(default), intent(in) :: fac_scale
    select case (k%sf_chain%get_status ())
    case (SF_DONE_KINEMATICS)
       call k%sf_chain%evaluate (fac_scale)
    end select
  end subroutine kinematics_evaluate_sf_chain
  
  subroutine kinematics_return_beam_momenta (k)
    class(kinematics_t), intent(in) :: k
    call k%sf_chain%return_beam_momenta ()
  end subroutine kinematics_return_beam_momenta
  
  subroutine component_instance_write (object, unit, testflag)
    class(component_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit)
    if (object%active) then
       if (associated (object%config)) then
          write (u, "(1x,A,I0)")  "Component #", object%config%index
       else
          write (u, "(1x,A)")  "Component [undefined]"
       end if
    else
       write (u, "(1x,A,I0,A)")  "Component #", object%config%index, &
            " [inactive]"
    end if
    if (allocated (object%p_seed)) then
       write (u, "(1x,A)")  "Seed momenta:"
       do i = 1, size (object%p_seed)
          call vector4_write (object%p_seed(i), u, testflag = testflag)
       end do
    end if
    write (u, "(1x,A)")  "Squared matrix element:"
    if (object%sqme_known) then
       write (u, "(2x,ES19.12)")  object%sqme
    else
       write (u, "(2x,A)")  "[undefined]"
    end if
    call object%k_seed%write (u)
    if (allocated (object%core_state)) then
       call write_separator (u)
       call object%core_state%write (u)
    end if
  end subroutine component_instance_write
    
  subroutine component_instance_final (object)
    class(component_instance_t), intent(inout) :: object
    call object%k_seed%final ()
  end subroutine component_instance_final
  
  subroutine component_instance_init (component, config)
    class(component_instance_t), intent(out) :: component
    type(process_component_t), intent(in), target :: config
    integer :: nlo_type
    integer :: n_in, n_tot
    component%config => config
    nlo_type = component%config%config%get_nlo_type ()
    component%nlo_type = nlo_type
    associate (core => component%config%core)
      n_in = core%data%n_in
      n_tot = n_in + core%data%n_out
      select case (nlo_type)
      case (NLO_REAL)
        allocate (component%p_seed (n_tot -1))
      case (BORN, NLO_VIRTUAL, NLO_PDF)
        allocate (component%p_seed (n_tot))
      end select
      call core%allocate_workspace (component%core_state)
    end associate
  end subroutine component_instance_init

  subroutine component_instance_setup_kinematics (component, sf_chain, config, &
                                                  combined_integration)
    class(component_instance_t), intent(inout) :: component
    type(sf_chain_t), intent(in), target :: sf_chain
    type(process_beam_config_t), intent(in) :: config
    logical, intent(in), optional :: combined_integration
    integer :: nlo_type
    nlo_type = component%config%config%get_nlo_type ()
    if (present (combined_integration)) then
       if (combined_integration) then
          select type (phs_config => component%config%phs_config)
          type is (phs_wood_config_t)
             call phs_config%set_extended_phs ()
          end select
       end if
    end if
    call component%k_seed%init_sf_chain &
         (component%config%core, sf_chain, config, component%core_state)
    call component%k_seed%init_phs (component%config%phs_config)
  end subroutine component_instance_setup_kinematics

  subroutine component_instance_setup_fks_kinematics (component, var_list)
    class(component_instance_t), intent(inout), target :: component
    type(var_list_t), intent(in) :: var_list
    logical :: singular_jacobian
    logical, dimension(:), allocatable :: is_massive
    select type (phs => component%k_seed%phs)
    type is (phs_fks_t)
       associate (nlo_controller => component%nlo_controller)
          call phs%setup_isr_kinematics (nlo_controller%isr_kinematics)
          call phs%setup_real_kinematics (nlo_controller%real_kinematics)
          call phs%set_beam_energy ()
          phs%massive_phsp = nlo_controller%has_massive_emitter ()
          call phs%set_emitters (nlo_controller%reg_data%emitters)
          call phs%setup_masses (nlo_controller%particle_data%n_in + &
                                 nlo_controller%particle_data%n_out_born)
          is_massive = nlo_controller%get_mass_info(1)
          call phs%set_mass_info (is_massive)
          singular_jacobian = var_list%get_lval (var_str ("?powheg_use_singular_jacobian"))
          call phs%set_singular_jacobian (singular_jacobian)
       end associate
!       call phs%setup_real_kinematics (component%nlo_controller%real_kinematics)
!       call phs%set_beam_energy ()
!       call phs%setup_isr_kinematics (component%nlo_controller%isr_kinematics)
       call phs%setup_generator_kinematics ()
    class default 
       call msg_fatal ("Phase space should be an FKS phase space!")
    end select
  end subroutine component_instance_setup_fks_kinematics

  subroutine component_instance_compute_seed_kinematics &
       (component, mci_work, phs_channel, success)
    class(component_instance_t), intent(inout), target :: component
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    logical, intent(out) :: success
    select type (phs => component%k_seed%phs)
    type is (phs_fks_t)
       call component%k_seed%compute_selected_channel &
            (mci_work, phs_channel, component%p_seed, success, &
            component%nlo_controller)
       call component%nlo_controller%int_born%set_momenta &
            (component%p_seed)
    class default
       call component%k_seed%compute_selected_channel &
            (mci_work, phs_channel, component%p_seed, success)
    end select
  end subroutine component_instance_compute_seed_kinematics
    
  subroutine component_instance_recover_mcpar (component, mci_work, phs_channel)
    class(component_instance_t), intent(inout), target :: component
    type(mci_work_t), intent(inout) :: mci_work
    integer, intent(in) :: phs_channel
    call component%k_seed%recover_mcpar &
         (mci_work, phs_channel, component%p_seed)
  end subroutine component_instance_recover_mcpar
  
  subroutine component_instance_compute_hard_kinematics &
       (component, term, skip_term, real_phsp)
    class(component_instance_t), intent(inout) :: component
    type(term_instance_t), dimension(:), intent(inout) :: term
    integer, intent(in), optional :: skip_term
    logical, intent(in), optional :: real_phsp
    integer :: j, i
    associate (core => component%config%core)
      associate (i_term => component%config%i_term)
        if (allocated (component%core_state)) then
           call component%core_state%reset_new_kinematics ()
        end if
        do j = 1, size (i_term)
           i = i_term(j)
           if (present (skip_term)) then
              if (i == skip_term)  cycle
           end if
           if (present (real_phsp)) then
             if (real_phsp) then
               call core%compute_hard_kinematics &
                     (component%p_seed, i, term(i)%nlo_controller%int_born, &
                     component%core_state)
             else 
               call core%compute_hard_kinematics &
                     (component%p_seed, i, term(i)%int_hard, &
                     component%core_state)
             end if
          else
             call core%compute_hard_kinematics & 
                 (component%p_seed, i, term(i)%int_hard, component%core_state)
          end if
        end do
      end associate
    end associate
  end subroutine component_instance_compute_hard_kinematics
    
  subroutine component_instance_recover_seed_kinematics (component, term)
    class(component_instance_t), intent(inout) :: component
    type(term_instance_t), intent(inout) :: term
    integer :: n_in
    n_in = component%k_seed%n_in
    call component%k_seed%get_incoming_momenta (component%p_seed(1:n_in))
    associate (core => component%config%core)
      call core%recover_kinematics &
           (component%p_seed, term%int_hard, term%isolated%int_eff, &
           component%core_state)
      call term%isolated%receive_kinematics ()
    end associate
  end subroutine component_instance_recover_seed_kinematics
  
  subroutine component_instance_compute_other_channels &
       (component, mci_work, phs_channel)
    class(component_instance_t), intent(inout), target :: component
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    call component%k_seed%compute_other_channels (mci_work, phs_channel)
  end subroutine component_instance_compute_other_channels
    
  subroutine component_instance_return_beam_momenta (component)
    class(component_instance_t), intent(in) :: component
    call component%k_seed%return_beam_momenta ()
  end subroutine component_instance_return_beam_momenta
    
  subroutine component_instance_evaluate_sqme_born (component, term)
    class(component_instance_t), intent(inout) :: component
    type(term_instance_t), intent(inout), target :: term
    real(default) :: sqme
    sqme = real (term%connected%trace%get_matrix_element (1))
    component%sqme = sqme * term%weight
  end subroutine component_instance_evaluate_sqme_born    

  subroutine component_instance_evaluate_sqme (component, term)
    class(component_instance_t), intent(inout) :: component
    type(term_instance_t), dimension(:), intent(inout), target :: term
    integer :: j, i

    component%sqme = 0
    call reset_nlo_components (component)
    associate (i_term => component%config%i_term)
      do j = 1, size (i_term)
         i = i_term(j)
         if (term(i)%passed) then
            call component%evaluate_sqme_born (term(i))
            call associate_sqme_born (component, component%sqme*term(i)%weight)
            call component%evaluate_sqme_real (term(i))
            call component%evaluate_sqme_virt (term(i))
            call component%evaluate_sqme_pdf (term(i))
         end if
      end do
    end associate
    component%sqme_known = .true.
  end subroutine component_instance_evaluate_sqme
  
  subroutine reset_nlo_components (component)
     type(component_instance_t), intent(inout) :: component
     select case (component%config%config%get_nlo_type ())
     case (NLO_REAL, NLO_VIRTUAL, NLO_PDF)
        associate (collector => component%nlo_controller%sqme_collector)
           collector%current_sqme_born = 0
           collector%sqme_real_non_sub = 0
        end associate
     end select
  end subroutine reset_nlo_components

  subroutine associate_sqme_born (component, summand)
     type(component_instance_t), intent(inout) :: component
     real(default), intent(in) :: summand
     select case (component%config%config%get_nlo_type ())
     case (NLO_REAL)
        associate (collector => component%nlo_controller%sqme_collector)
           collector%current_sqme_born = summand 
           collector%sqme_born_list(1) = summand
        end associate
     end select
  end subroutine associate_sqme_born

  subroutine component_instance_evaluate_sqme_virt (component, term)
     class(component_instance_t), intent(inout) :: component
     type(term_instance_t), intent(inout) :: term
     real(default), dimension(4) :: sqme_virt 
     integer :: i_flv
     logical :: bad_point

     if (term%nlo_type /= NLO_VIRTUAL) return
     associate (nlo_controller => component%nlo_controller)
        call nlo_controller%set_alpha_s_born &
                  (component%config%core%get_alpha_s (component%core_state))
        call nlo_controller%virtual_terms%set_ren_scale &
                   (term%int_hard%get_momenta (), term%ren_scale) 
        call nlo_controller%virtual_terms%set_fac_scale &
                   (term%int_hard%get_momenta (), term%fac_scale)
        select type (core => component%config%core)
        class is (prc_blha_t)
           do i_flv = 1, core%get_nflv()
              call core%update_alpha_s (component%core_state, term%fac_scale)
              call core%compute_sqme_virt (i_flv, &
                         term%int_hard%get_momenta (), &
                         term%ren_scale, &
                         sqme_virt, bad_point)
              if (.not. nlo_controller%use_internal_color_correlations) &
                 call core%compute_sqme_cc (i_flv, &
                           term%int_hard%get_momenta (), &
                           term%ren_scale, &
                           born_cc = nlo_controller%sqme_collector%sqme_born_cc (:,:,i_flv), &
                           bad_point = bad_point)
              call nlo_controller%virtual_terms%set_vfin (sqme_virt(3))
              call nlo_controller%virtual_terms%set_bad_point (bad_point)
              nlo_controller%sqme_collector%current_sqme_born = sqme_virt (4)             
              component%sqme = component%sqme + &
                               nlo_controller%compute_virt (i_flv, term%int_hard) * &
                               term%weight
           end do
           call core%print_parameter_file ()
        end select

     end associate
  end subroutine component_instance_evaluate_sqme_virt

  subroutine component_instance_evaluate_sqme_pdf (component, term)
    class(component_instance_t), intent(inout) :: component
    type(term_instance_t), intent(inout) :: term
    real(default) :: sqme_pdf
    if (term%nlo_type /= NLO_PDF) return
    associate (nlo_controller => component%nlo_controller)
       if (.not. nlo_controller%pdf_subtraction_is_required ()) then
          component%sqme = 0._default
          return
       end if
       call nlo_controller%set_alpha_s_born &
                 (component%config%core%get_alpha_s (component%core_state))
       call nlo_controller%evaluate_pdf_subtraction (component%sqme)
    end associate
  end subroutine component_instance_evaluate_sqme_pdf

  subroutine component_instance_evaluate_sqme_real (component, term)
     class(component_instance_t), intent(inout) :: component
     type(term_instance_t), intent(inout), target :: term
     type(vector4_t), dimension(:), allocatable :: p_real
     integer :: i_flv
     logical :: bad_point

     if (term%nlo_type /= NLO_REAL) return
     p_real = term%nlo_controller%get_real_momenta ()
     select type (core => component%config%core)
     !!! Get the unsubtracted real matrix element, 
     !!! either from the parton states or from GoSam
     type is (prc_omega_t)
        component%nlo_controller%sqme_collector%sqme_real_non_sub = &
             real (term%connected_real%trace%get_matrix_element (1))
     class is (prc_blha_t)
        do i_flv = 1, component%nlo_controller%get_n_flv_real ()
           call core%update_alpha_s (component%core_state, term%fac_scale)
           call core%compute_sqme_real (i_flv, p_real, 0._default, &
                component%nlo_controller%sqme_collector%sqme_real_non_sub(i_flv), &
                bad_point)
        end do
     end select 
     !!! Get the necessary subtraction matrix elements.
     associate (nlo_controller => component%nlo_controller)
       if (nlo_controller%is_subtraction_active ()) then
          associate (collector => nlo_controller%sqme_collector)
            select type (core_born => component%config%core_sub_born)
            class is (prc_blha_t)
               do i_flv = 1, nlo_controller%get_n_flv_born ()
                  if (.not. nlo_controller%use_internal_color_correlations) then
                     call core_born%update_alpha_s (component%core_state, term%fac_scale)
                     call core_born%compute_sqme_cc (i_flv, &
                             nlo_controller%int_born%get_momenta (), &
                             0._default, &
                             collector%sqme_born_list (i_flv), &
                             collector%sqme_born_cc (:,:,i_flv), &
                             bad_point)
                  else
                  !!! Implementation for color-correlations using color_data
                  end if
                  if (.not. nlo_controller%use_internal_spin_correlations  &
                       .and. nlo_controller%requires_spin_correlation (i_flv)) then
                       call core_born%update_alpha_s (component%core_state, term%fac_scale)
                       call core_born%compute_sqme_sc (i_flv, 5, &
                          nlo_controller%int_born%get_momenta (), &
                          0._default, collector%sqme_born_sc(i_flv), bad_point)
                  end if
               end do
            type is (prc_omega_t)
               collector%sqme_born_cc (:,:,1) = collector%current_sqme_born * &
                    nlo_controller%color_data%beta_ij (:,:,1)
            end select
          end associate
       end if
       call nlo_controller%set_alpha_s_born &
            (component%config%core%get_alpha_s (component%core_state))
       nlo_controller%sqme_collector%current_sqme_real = &
            nlo_controller%compute_sqme_real_fin (term%weight, p_real) 
     end associate
  end subroutine component_instance_evaluate_sqme_real

  elemental function component_instance_get_nlo_type (component) &
       result (nlo_type)
    class(component_instance_t), intent(in) :: component
    integer :: nlo_type
    nlo_type = component%nlo_type
  end function component_instance_get_nlo_type

  function component_instance_get_component_type (component) result (val)
    class(component_instance_t), intent(in) :: component
    integer :: val
    val = component%config%component_type
  end function component_instance_get_component_type

  subroutine component_instance_set_component_type (component, val)
    class(component_instance_t), intent(in) :: component
    integer, intent(in) :: val
    component%config%component_type = val
  end subroutine component_instance_set_component_type

  function process_component_needs_mci_entry (component) result (value)
    class(process_component_t), intent(in) :: component
    logical :: value
    value = component%active .and. component%core%needs_mcset () &
            .and. component%component_type <= COMP_MASTER 
  end function process_component_needs_mci_entry

  subroutine term_instance_write (term, unit, show_eff_state, testflag)
    class(term_instance_t), intent(in) :: term
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_eff_state
    logical, intent(in), optional :: testflag 
    integer :: u
    logical :: state
    u = given_output_unit (unit)
    state = .true.;  if (present (show_eff_state))  state = show_eff_state
    if (term%active) then
       if (associated (term%config)) then
          write (u, "(1x,A,I0,A,I0,A)")  "Term #", term%config%i_term, &
               " (component #", term%config%i_component, ")"
       else
          write (u, "(1x,A)")  "Term [undefined]"
       end if
    else
       write (u, "(1x,A,I0,A)")  "Term #", term%config%i_term, &
            " [inactive]"
    end if
    if (term%checked) then
       write (u, "(3x,A,L1)")      "passed cuts           = ", term%passed
    end if
    if (term%passed) then
       write (u, "(3x,A,ES19.12)")  "overall scale         = ", term%scale
       write (u, "(3x,A,ES19.12)")  "factorization scale   = ", term%fac_scale
       write (u, "(3x,A,ES19.12)")  "renormalization scale = ", term%ren_scale
       if (allocated (term%alpha_qcd_forced)) then
          write (u, "(3x,A,ES19.12)")  "alpha(QCD) forced     = ", &
               term%alpha_qcd_forced
       end if
       write (u, "(3x,A,ES19.12)")  "reweighting factor    = ", term%weight
    end if
    call term%k_term%write (u)
    call write_separator (u)
    write (u, "(1x,A)")  "Amplitude (transition matrix of the &
         &hard interaction):"
    call write_separator (u)
    call term%int_hard%basic_write (u, testflag = testflag)
    if (state .and. term%isolated%has_trace) then
       call write_separator (u)
       write (u, "(1x,A)")  "Evaluators for the hard interaction:"
       call term%isolated%write (u, testflag = testflag)
    end if
    if (state .and. term%connected%has_trace) then
       call write_separator (u)
       write (u, "(1x,A)")  "Evaluators for the connected process:"
       call term%connected%write (u, testflag = testflag)
    end if
  end subroutine term_instance_write
    
  subroutine term_instance_final (term)
    class(term_instance_t), intent(inout) :: term
    call term%k_term%final ()
    call term%connected%final ()
    call term%isolated%final ()
    call term%int_hard%final ()
    select case (term%nlo_type)
    case (NLO_REAL)
      call term%connected_real%final ()
      call term%isolated_real%final ()
      call term%int_hard_real%final ()
    end select
  end subroutine term_instance_final
  
  subroutine term_instance_init (term, &
       config, k_seed, beam_config, core, process_var_list, nlo_controller)
    class(term_instance_t), intent(inout), target :: term
    type(process_term_t), intent(in), target :: config
    type(kinematics_t), intent(in) :: k_seed
    type(process_beam_config_t), intent(in) :: beam_config
    type(interaction_t), pointer :: sf_chain_int
    type(interaction_t), pointer :: src_int
    class(prc_core_t), intent(in) :: core
    type(var_list_t), intent(in), target :: process_var_list
    type(nlo_controller_t), intent(inout), pointer :: nlo_controller
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    type(state_matrix_t), pointer :: state_matrix
    type(flavor_t), dimension(:), allocatable :: flv_int, flv_src, f_in, f_out
    type(flavor_t), dimension(:), allocatable :: f_out_real
    integer :: n_in, n_vir, n_out, n_tot
    integer :: i, j
    type(interaction_t), pointer, save :: int_sav 
    type(sf_chain_instance_t), pointer, save :: sf_sav
    integer, dimension(:), allocatable, save :: col_sav
    type(prc_omega_t), save :: core_sav

    term%config => config
    term%nlo_controller => nlo_controller
    if (config%rearrange) then
       ! rearrangement of seed to hard kinematics not implemented yet
       ! allocate k_term distinct from k_seed as needed.
    else
       ! here, k_term trivially accesses k_seed via pointers
       call term%k_term%init_ptr (k_seed)
    end if
    allocate (term%amp (config%n_allowed))
    term%int_hard = config%int
    allocate (term%p_hard (term%int_hard%get_n_tot ()))
    sf_chain_int => term%k_term%sf_chain%get_out_int_ptr ()
    n_in = term%int_hard%get_n_in ()
    do j = 1, n_in
       i = term%k_term%sf_chain%get_out_i (j)
       call term%int_hard%set_source_link (j, sf_chain_int, i)
    end do
    if (config%rearrange) then
       ! rearrangement hard to effective kinematics not implemented yet
       ! should use term%config%int_eff as template
       ! allocate distinct sf_chain in term%connected as needed
    else
      select type (phs => term%k_term%phs)
        type is (phs_wood_t)
          ! here, int_hard and sf_chain are trivially accessed via pointers
          call term%isolated%init (term%k_term%sf_chain, term%int_hard)
          int_sav => term%int_hard
          sf_sav => term%k_term%sf_chain
        type is (phs_fks_t)
          select case (phs%mode)
          case (PHS_MODE_ADDITIONAL_PARTICLE)
             term%nlo_controller%int_born = int_sav
             call term%isolated%init (sf_sav, term%nlo_controller%int_born)
             call term%setup_real_interaction (term%int_hard)
          case (PHS_MODE_COLLINEAR_REMNANT)
             call term%isolated%init (term%k_term%sf_chain, term%int_hard)
          end select
        class default
          call term%isolated%init (term%k_term%sf_chain, term%int_hard)
      end select
    end if
    allocate (mask_in (n_in))
    mask_in = term%k_term%sf_chain%get_out_mask ()
    select type (phs => term%k_term%phs)
      type is (phs_wood_t)
      call term%isolated%setup_square_trace (core, mask_in, term%config%col)
      if (.not. allocated (col_sav)) then
        allocate (col_sav (size (term%config%col)))
        col_sav = term%config%col
      end if
      select type (core)
      type is (prc_omega_t)
        core_sav = core
      end select
      type is (phs_fks_t)
        select case (phs%mode)
        case (PHS_MODE_ADDITIONAL_PARTICLE)
           mask_in = sf_sav%get_out_mask ()
           call term%isolated%setup_square_trace (core_sav, mask_in, col_sav)
           call term%init_states (core)
        case (PHS_MODE_COLLINEAR_REMNANT)
           call term%isolated%setup_square_trace (core, mask_in, term%config%col)
        end select      
      class default
        call term%isolated%setup_square_trace (core, mask_in, term%config%col)
    end select
    call term%connected%setup_connected_trace (term%isolated)
    associate (int_eff => term%isolated%int_eff)
      state_matrix => int_eff%get_state_matrix_ptr ()
      n_tot = int_eff%get_n_tot  ()
      allocate (flv_int (n_tot))
      flv_int = quantum_numbers_get_flavor &
           (state_matrix%get_quantum_numbers (1))
      allocate (f_in (n_in))
      f_in = flv_int(1:n_in)
      deallocate (flv_int)
    end associate
    n_in = term%connected%trace%get_n_in ()
    n_vir = term%connected%trace%get_n_vir ()
    n_out = term%connected%trace%get_n_out ()
    allocate (f_out (n_out))
    do j = 1, n_out
       call term%connected%trace%find_source &
            (n_in + n_vir + j, src_int, i)
       if (associated (src_int)) then
          state_matrix => src_int%get_state_matrix_ptr ()
          allocate (flv_src (src_int%get_n_tot ()))
          flv_src = quantum_numbers_get_flavor &
               (state_matrix%get_quantum_numbers (1))
          f_out(j) = flv_src(i)
          deallocate (flv_src)
       end if
    end do
    call term%connected%setup_subevt (term%isolated%sf_chain_eff, &
         beam_config%data%flv, f_in, f_out)
    call term%connected%setup_var_list (process_var_list, beam_config%data)
    select case (term%nlo_type)
    case (NLO_REAL)
      allocate (f_out_real (n_out+1))
      f_out_real (1:n_out) = f_out
      f_out_real (n_out+1) = term%nlo_controller%reg_data%flv_extra
      call term%connected_real%setup_subevt (term%isolated_real%sf_chain_eff, &
           beam_config%data%flv, f_in, f_out_real)
      call term%connected_real%setup_var_list (process_var_list, beam_config%data)
    end select

  end subroutine term_instance_init

  subroutine term_instance_init_states (term, core)
    class(term_instance_t), intent(inout), target :: term
    class(prc_core_t), intent(in) :: core
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    integer :: i
    mask_in = term%k_term%sf_chain%get_out_mask ()
    call term%isolated_real%init (term%k_term%sf_chain, term%int_hard_real)
    call term%isolated_real%setup_square_trace (core, mask_in, term%config%col)
    call term%connected_real%setup_connected_trace (term%isolated_real) 
  end subroutine term_instance_init_states

  subroutine term_instance_setup_expressions (term, meta, config)
    class(term_instance_t), intent(inout), target :: term
    type(process_metadata_t), intent(in), target :: meta
    type(process_config_data_t), intent(in) :: config
    if (allocated (config%ef_cuts)) &
         call term%connected%setup_cuts (config%ef_cuts)
    if (allocated (config%ef_scale)) &
         call term%connected%setup_scale (config%ef_scale)
    if (allocated (config%ef_fac_scale)) &
         call term%connected%setup_fac_scale (config%ef_fac_scale)
    if (allocated (config%ef_ren_scale)) &
         call term%connected%setup_ren_scale (config%ef_ren_scale)
    if (allocated (config%ef_weight)) &
         call term%connected%setup_weight (config%ef_weight)
  end subroutine term_instance_setup_expressions
    
  subroutine term_instance_setup_expressions_real (term, meta, config)
    class(term_instance_t), intent(inout), target :: term
    type(process_metadata_t), intent(in), target :: meta
    type(process_config_data_t), intent(in) :: config
    if (allocated (config%ef_cuts)) &
         call term%connected_real%setup_cuts (config%ef_cuts)
    if (allocated (config%ef_scale)) &
         call term%connected_real%setup_scale (config%ef_scale)
    if (allocated (config%ef_fac_scale)) &
         call term%connected_real%setup_fac_scale (config%ef_fac_scale)
    if (allocated (config%ef_ren_scale)) &
         call term%connected_real%setup_ren_scale (config%ef_ren_scale)
    if (allocated (config%ef_weight)) &
         call term%connected_real%setup_weight (config%ef_weight)
  end subroutine term_instance_setup_expressions_real

  subroutine term_instance_setup_event_data (term, core, model)
    class(term_instance_t), intent(inout), target :: term
    class(prc_core_t), intent(in) :: core
    class(model_data_t), intent(in), target :: model
    integer :: n_in
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    n_in = term%int_hard%get_n_in ()
    allocate (mask_in (n_in))
    mask_in = term%k_term%sf_chain%get_out_mask ()
    select case (term%nlo_type)
    case (BORN, NLO_VIRTUAL, NLO_PDF)
      call term%isolated%setup_square_matrix (core, model, mask_in, &
         term%config%col)
      call term%isolated%setup_square_flows (core, model, mask_in)
      call term%connected%setup_connected_matrix (term%isolated)
      call term%connected%setup_connected_flows (term%isolated)
      call term%connected%setup_state_flv (term%isolated%get_n_out ())
    case (NLO_REAL)
      call term%isolated_real%setup_square_matrix (core, model, mask_in, &
               term%config%col)
      call term%isolated_real%setup_square_flows (core, model, mask_in)
      call term%connected_real%setup_connected_matrix &
                (term%isolated_real)
      call term%connected_real%setup_connected_flows &
                (term%isolated_real)
      call term%connected_real%setup_state_flv (term%isolated_real%get_n_out ())
    end select
 end subroutine term_instance_setup_event_data
    
  subroutine term_instance_setup_real_interaction (term, int)
    class(term_instance_t), intent(inout) :: term
    type(interaction_t), intent(in) :: int
    integer :: i
    term%int_hard_real = int
  end subroutine term_instance_setup_real_interaction

  subroutine term_instance_reset (term)
    class(term_instance_t), intent(inout) :: term
    call term%connected%reset_expressions ()
    if (allocated (term%alpha_qcd_forced))  deallocate (term%alpha_qcd_forced)
    term%active = .false.
  end subroutine term_instance_reset
  
  subroutine term_instance_set_alpha_qcd_forced (term, alpha_qcd)
    class(term_instance_t), intent(inout) :: term
    real(default), intent(in) :: alpha_qcd
    if (allocated (term%alpha_qcd_forced)) then
       term%alpha_qcd_forced = alpha_qcd
    else
       allocate (term%alpha_qcd_forced, source = alpha_qcd)
    end if
  end subroutine term_instance_set_alpha_qcd_forced
  
  subroutine term_instance_compute_eff_kinematics (term, component)
    class(term_instance_t), intent(inout) :: term
    type(component_instance_t), dimension(:), intent(inout) :: component
    integer :: i_component, i_term
    term%checked = .false.
    term%passed = .false.
    if (term%config%rearrange) then
       ! should evaluate k_term first if allocated separately, not impl. yet
       i_component = term%config%i_component
       i_term = term%config%i_term
      associate (core => component(i_component)%config%core)
         select case (term%nlo_type)
         case (BORN, NLO_VIRTUAL, NLO_PDF)
           call core%compute_eff_kinematics &
              (i_term, term%int_hard, term%isolated%int_eff, &
              component(i_component)%core_state)
         case (NLO_REAL)
           call component(i_component)%config%core_sub_born%compute_eff_kinematics & 
                (i_term, term%nlo_controller%int_born, term%isolated%int_eff, &
                component(i_component)%core_state)
         end select
        select case (term%nlo_type)
        case (NLO_REAL)
          call core%compute_eff_kinematics &
                (i_term, term%int_hard_real, term%isolated_real%int_eff, &
                 component(i_component)%core_state)
        end select
      end associate
    end if
    call term%isolated%receive_kinematics ()
    call term%connected%receive_kinematics ()
  end subroutine term_instance_compute_eff_kinematics
    
  subroutine term_instance_recover_hard_kinematics (term, component)
    class(term_instance_t), intent(inout) :: term
    type(component_instance_t), dimension(:), intent(inout) :: component
    term%checked = .false.
    term%passed = .false.
    call term%connected%send_kinematics ()
    call term%isolated%send_kinematics ()
  end subroutine term_instance_recover_hard_kinematics

  subroutine term_instance_evaluate_expressions (term, scale_forced)
    class(term_instance_t), intent(inout) :: term
    real(default), intent(in), allocatable, optional :: scale_forced
    call term%connected%evaluate_expressions (term%passed, &
         term%scale, term%fac_scale, term%ren_scale, term%weight, &
         scale_forced)
    term%checked = .true.
  end subroutine term_instance_evaluate_expressions
       
  subroutine term_instance_evaluate_interaction (term, component)
    class(term_instance_t), intent(inout) :: term
    type(component_instance_t), dimension(:), intent(inout) :: component
    integer :: i_component, i_term, i
    i_component = term%config%i_component
    i_term = term%config%i_term
    term%p_hard = term%int_hard%get_momenta ()
    select case (term%nlo_type)
    case (BORN, NLO_VIRTUAL, NLO_PDF)
       associate (core => component(i_component)%config%core)
          do i = 1, term%config%n_allowed
             term%amp(i) = core%compute_amplitude (i_term, term%p_hard, &
                term%config%flv(i), term%config%hel(i), term%config%col(i), &
                term%fac_scale, term%ren_scale, term%alpha_qcd_forced, &
                component(i_component)%core_state)
          end do
          call term%int_hard%set_matrix_element (term%amp)
          if (associated (term%nlo_controller)) call term%nlo_controller%set_fac_scale (term%fac_scale)
       end associate 
    case (NLO_REAL)
       call term%evaluate_interaction_real (component(i_component), i_term)
    end select  
  end subroutine term_instance_evaluate_interaction
  
  subroutine term_instance_evaluate_trace (term)
    class(term_instance_t), intent(inout) :: term
    call term%k_term%evaluate_sf_chain (term%fac_scale)
    call term%isolated%evaluate_sf_chain (term%fac_scale)
    call term%isolated%evaluate_trace ()
    call term%connected%evaluate_trace ()
    select case (term%nlo_type)
    case (NLO_REAL)
      call term%isolated_real%evaluate_trace ()
      call term%connected_real%evaluate_trace ()
    end select
  end subroutine term_instance_evaluate_trace
  
  subroutine term_instance_evaluate_event_data (term)
    class(term_instance_t), intent(inout) :: term
    select case (term%nlo_type)
    case (BORN, NLO_VIRTUAL, NLO_PDF)
      call term%isolated%evaluate_event_data ()
      call term%connected%evaluate_event_data ()
    case (NLO_REAL)
      call term%isolated_real%evaluate_event_data ()
      call term%connected_real%evaluate_event_data ()
    end select
  end subroutine term_instance_evaluate_event_data
  
  subroutine term_instance_evaluate_interaction_real (term, component, i_term)
     class(term_instance_t), intent(inout) :: term
     type(component_instance_t), intent(inout) :: component
     integer, intent(in) :: i_term
     integer :: emitter
     type(vector4_t), dimension(:), allocatable :: p_born
     type(vector4_t), dimension(:), allocatable :: p_real
     integer :: i

     emitter = term%nlo_controller%get_active_emitter ()
     p_born = term%nlo_controller%int_born%get_momenta ()
     select type (phs => term%k_term%phs)
     type is (phs_fks_t)
        if (emitter > 2) then
           call phs%generate_fsr (emitter, p_born, p_real)
        else
           call phs%generate_isr (emitter, p_born, p_real)
        end if
     class default 
        call msg_fatal ("Evaluate real interaction: Phase space not of FKS-type!")
     end select

     call term%nlo_controller%set_real_momenta (p_real)
     call term%int_hard_real%set_momenta (p_real)

     call term%isolated_real%receive_kinematics ()
     call term%connected_real%receive_kinematics ()

     call term%connected_real%evaluate_expressions (term%passed, &
          term%scale, term%fac_scale, term%ren_scale, term%weight)
     

     if (term%passed) then
        call component%core_state%reset_new_kinematics ()
        select type (core => component%config%core)
        type is (prc_omega_t)
           do i = 1, term%config%n_allowed
              term%amp(i) = core%compute_amplitude (i_term, &
                 term%int_hard_real%get_momenta (), &
                 term%config%flv(i), term%config%hel(i), term%config%col(i), &
                 term%fac_scale, term%ren_scale, term%alpha_qcd_forced, &
                 component%core_state)
           end do
        class is (prc_blha_t)
           call core%update_alpha_s (component%core_state, term%fac_scale)
           term%amp = 0._default
        end select
        call term%int_hard_real%set_matrix_element (term%amp)
        call component%core_state%reset_new_kinematics ()
        select type (core_sub_born => component%config%core_sub_born)
        type is (prc_omega_t)
           do i = 1, term%nlo_controller%n_allowed_born
              term%nlo_controller%amp_born(i) = &
                   core_sub_born%compute_amplitude (i_term, &
                   term%nlo_controller%int_born%get_momenta (), &
                   term%nlo_controller%get_flv_born(i), &
                   term%nlo_controller%get_hel_born(i), &
                   term%nlo_controller%get_col_born(i), &
                   term%fac_scale, term%ren_scale, &
                   term%alpha_qcd_forced, &
                   component%core_state)
           end do
        class is (prc_blha_t)
           call core_sub_born%update_alpha_s &
                (component%core_state, term%fac_scale)
           term%nlo_controller%amp_born = 0._default
        end select 
        call term%nlo_controller%int_born%set_matrix_element &
             (term%nlo_controller%amp_born) 
     end if
  end subroutine term_instance_evaluate_interaction_real

  subroutine term_instance_evaluate_interaction_real_rad (term, &
                          component, p_born, p_real, alpha_s, i_term)
    class(term_instance_t), intent(inout) :: term
    type(component_instance_t), intent(inout) :: component
    type(vector4_t), intent(in), dimension(:) :: p_born, p_real
    real(default), intent(in) :: alpha_s
    integer, intent(in) :: i_term
    integer :: i
    real(default), allocatable :: alpha_qcd_forced

    call term%nlo_controller%int_born%set_momenta (p_born)
    call term%int_hard_real%set_momenta (p_real)
    call term%nlo_controller%set_real_momenta (p_real)

    call term%isolated_real%receive_kinematics ()
    call term%connected_real%receive_kinematics ()
    
    call term%connected_real%evaluate_expressions (term%passed, &
         term%scale, term%fac_scale, term%ren_scale, term%weight)

    call component%core_state%reset_new_kinematics ()
    select type (core => component%config%core)
    type is (prc_omega_t)
       if (allocated (alpha_qcd_forced)) then
          alpha_qcd_forced = alpha_s
       else
          allocate (alpha_qcd_forced, source = alpha_s)
       end if
       do i = 1, term%config%n_allowed
          term%amp(i) = core%compute_amplitude (i_term, &
             term%int_hard_real%get_momenta (), &
             term%config%flv(i), term%config%hel(i), term%config%col(i), &
             term%fac_scale, term%ren_scale, alpha_qcd_forced, &
             component%core_state)
       end do
    class is (prc_blha_t)
       call core%update_alpha_s (component%core_state, term%fac_scale)
       term%amp = 0._default
    end select
    call term%int_hard_real%set_matrix_element (term%amp)
    call component%core_state%reset_new_kinematics ()
    select type (core_sub_born => component%config%core_sub_born)
    type is (prc_omega_t)
       do i = 1, term%nlo_controller%n_allowed_born
          term%nlo_controller%amp_born(i) = core_sub_born%compute_amplitude (i_term, &
              term%nlo_controller%int_born%get_momenta (), &
              term%nlo_controller%get_flv_born(i), term%nlo_controller%get_hel_born(i), &
              term%nlo_controller%get_col_born(i), term%fac_scale, term%ren_scale, &
              term%alpha_qcd_forced, component%core_state)
       end do
    class is (prc_blha_t)
       term%nlo_controller%amp_born = 0._default
       call core_sub_born%update_alpha_s (component%core_state, term%fac_scale)
    end select 
    call term%nlo_controller%int_born%set_matrix_element &
         (term%nlo_controller%amp_born) 
  end subroutine term_instance_evaluate_interaction_real_rad

  subroutine term_instance_set_fac_scale (term, fac_scale)
    class(term_instance_t), intent(inout) :: term
    real(default), intent(in) :: fac_scale
    term%fac_scale = fac_scale
  end subroutine term_instance_set_fac_scale

  function term_instance_get_fac_scale (term) result (fac_scale)
    class(term_instance_t), intent(in) :: term
    real(default) :: fac_scale
    fac_scale = term%fac_scale
  end function term_instance_get_fac_scale
  
  function term_instance_get_alpha_s (term, component) result (alpha_s)
    class(term_instance_t), intent(in) :: term
    type(component_instance_t), dimension(:), intent(in) :: component
    real(default) :: alpha_s
    integer :: i_component
    i_component = term%config%i_component
    associate (core => component(i_component)%config%core)
      alpha_s = core%get_alpha_s (component(i_component)%core_state)
    end associate
    if (alpha_s < 0)  alpha_s = term%config%alpha_s
  end function term_instance_get_alpha_s
  
  subroutine mci_work_write (mci_work, unit, testflag)
    class(mci_work_t), intent(in) :: mci_work
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A,I0,A)")  "Active MCI instance #", &
         mci_work%config%i_mci, " ="
    write (u, "(2x)", advance="no")
    do i = 1, mci_work%config%n_par
       write (u, "(1x,F7.5)", advance="no")  mci_work%x(i)
       if (i == mci_work%config%n_par_sf) &
            write (u, "(1x,'|')", advance="no")
    end do
    write (u, *)
    if (associated (mci_work%mci)) then
       call mci_work%mci%write (u, pacify = testflag)
       call mci_work%counter%write (u)
    end if
  end subroutine mci_work_write
         
  subroutine mci_work_final (mci_work)
    class(mci_work_t), intent(inout) :: mci_work
    if (associated (mci_work%mci)) then
       call mci_work%mci%final ()
       deallocate (mci_work%mci)
    end if
  end subroutine mci_work_final
  
  subroutine mci_work_init (mci_work, mci_entry)
    class(mci_work_t), intent(out) :: mci_work
    type(process_mci_entry_t), intent(in), target :: mci_entry
    mci_work%config => mci_entry
    allocate (mci_work%x (mci_entry%n_par))
    if (allocated (mci_entry%mci)) then
       call mci_entry%mci%allocate_instance (mci_work%mci)
       call mci_work%mci%init (mci_entry%mci)
    end if
  end subroutine mci_work_init
  
  subroutine mci_work_set (mci_work, x)
    class(mci_work_t), intent(inout) :: mci_work
    real(default), dimension(:), intent(in) :: x
    mci_work%x = x
  end subroutine mci_work_set
    
  subroutine mci_work_set_x_strfun (mci_work, x)
    class(mci_work_t), intent(inout) :: mci_work
    real(default), dimension(:), intent(in) :: x
    mci_work%x(1 : mci_work%config%n_par_sf) = x
  end subroutine mci_work_set_x_strfun
    
  subroutine mci_work_set_x_process (mci_work, x)
    class(mci_work_t), intent(inout) :: mci_work
    real(default), dimension(:), intent(in) :: x
    mci_work%x(mci_work%config%n_par_sf + 1 : mci_work%config%n_par) = x
  end subroutine mci_work_set_x_process
    
  function mci_work_get_active_components (mci_work) result (i_component)
    class(mci_work_t), intent(in) :: mci_work
    integer, dimension(:), allocatable :: i_component
    allocate (i_component (size (mci_work%config%i_component)))
    i_component = mci_work%config%i_component
  end function mci_work_get_active_components

  function mci_work_get_x_strfun (mci_work) result (x)
    class(mci_work_t), intent(in) :: mci_work
    real(default), dimension(mci_work%config%n_par_sf) :: x
    x = mci_work%x(1 : mci_work%config%n_par_sf)
  end function mci_work_get_x_strfun

  function mci_work_get_x_process (mci_work) result (x)
    class(mci_work_t), intent(in) :: mci_work
    real(default), dimension(mci_work%config%n_par_phs) :: x
    x = mci_work%x(mci_work%config%n_par_sf + 1 : mci_work%config%n_par)
  end function mci_work_get_x_process

  subroutine mci_work_init_simulation (mci_work, safety_factor)
    class(mci_work_t), intent(inout) :: mci_work
    real(default), intent(in), optional :: safety_factor
    call mci_work%mci%init_simulation (safety_factor)
    call mci_work%counter%reset ()
  end subroutine mci_work_init_simulation

  subroutine mci_work_final_simulation (mci_work)
    class(mci_work_t), intent(inout) :: mci_work
    call mci_work%mci%final_simulation ()
  end subroutine mci_work_final_simulation

  subroutine mci_work_reset_counter (mci_work)
    class(mci_work_t), intent(inout) :: mci_work
    call mci_work%counter%reset ()
  end subroutine mci_work_reset_counter
  
  subroutine mci_work_record_call (mci_work, status)
    class(mci_work_t), intent(inout) :: mci_work
    integer, intent(in) :: status
    call mci_work%counter%record (status)
  end subroutine mci_work_record_call
    
  function mci_work_get_counter (mci_work) result (counter)
    class(mci_work_t), intent(in) :: mci_work
    type(process_counter_t) :: counter
    counter = mci_work%counter
  end function mci_work_get_counter
  
  subroutine process_instance_write_header (object, unit, testflag)
    class(process_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit)
    call write_separator (u, 2)
    if (associated (object%process)) then
       associate (meta => object%process%meta)
         select case (meta%type)
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
         write (u, "(1x,A,A,A)") "'", char (meta%id), "'"
         write (u, "(3x,A,A,A)") "Run ID = '", char (meta%run_id), "'"
         if (allocated (meta%component_id)) then
            write (u, "(3x,A)")  "Process components:"
            do i = 1, size (meta%component_id)
               if (object%component(i)%active) then
                  write (u, "(3x,'*')", advance="no")
               else
                  write (u, "(4x)", advance="no")
               end if
               write (u, "(1x,I0,9A)")  i, ": '", &
                    char (meta%component_id (i)), "':   ", &
                    char (meta%component_description (i))
            end do
         end if
       end associate
    else
       write (u, "(1x,A)") "Process instance [undefined process]"
       return
    end if
    write (u, "(3x,A)", advance = "no")  "status = "
    select case (object%evaluation_status)
    case (STAT_INITIAL);            write (u, "(A)")  "initialized"
    case (STAT_ACTIVATED);          write (u, "(A)")  "activated"
    case (STAT_BEAM_MOMENTA);       write (u, "(A)")  "beam momenta set"
    case (STAT_FAILED_KINEMATICS);  write (u, "(A)")  "failed kinematics"
    case (STAT_SEED_KINEMATICS);    write (u, "(A)")  "seed kinematics"
    case (STAT_HARD_KINEMATICS);    write (u, "(A)")  "hard kinematics"
    case (STAT_EFF_KINEMATICS);     write (u, "(A)")  "effective kinematics"
    case (STAT_FAILED_CUTS);        write (u, "(A)")  "failed cuts"
    case (STAT_PASSED_CUTS);        write (u, "(A)")  "passed cuts"
    case (STAT_EVALUATED_TRACE);    write (u, "(A)")  "evaluated trace"
       call write_separator (u)
       write (u, "(3x,A,ES19.12)")  "sqme   = ", object%sqme
    case (STAT_EVENT_COMPLETE);   write (u, "(A)")  "event complete"
       call write_separator (u)
       write (u, "(3x,A,ES19.12)")  "sqme   = ", object%sqme
       write (u, "(3x,A,ES19.12)")  "weight = ", object%weight
       if (.not. vanishes (object%excess)) &
            write (u, "(3x,A,ES19.12)")  "excess = ", object%excess
    case default;                 write (u, "(A)")  "undefined"
    end select
    if (object%i_mci /= 0) then
       call write_separator (u)
       call object%mci_work(object%i_mci)%write (u, testflag)
    end if
    call write_separator (u, 2)
  end subroutine process_instance_write_header

  subroutine process_instance_write (object, unit, testflag)
    class(process_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit)
    call object%write_header (u)
    if (object%evaluation_status >= STAT_BEAM_MOMENTA) then
       call object%sf_chain%write (u)
       call write_separator (u, 2)
       if (object%evaluation_status >= STAT_SEED_KINEMATICS) then
          write (u, "(1x,A)") "Active components:"
          do i = 1, size (object%component)
             if (object%component(i)%active) then
                call write_separator (u)
                call object%component(i)%write (u, testflag)
             end if
          end do
          if (object%evaluation_status >= STAT_HARD_KINEMATICS) then
             call write_separator (u, 2)
             write (u, "(1x,A)") "Active terms:"
             if (any (object%term%active)) then
                do i = 1, size (object%term)
                   if (object%term(i)%active) then
                      call write_separator (u)
                      call object%term(i)%write (u, &
                           show_eff_state = &
                           object%evaluation_status >= STAT_EFF_KINEMATICS, &
                           testflag = testflag)
                   end if
                end do
             end if
          end if
          call write_separator (u, 2)
       end if
    end if
  end subroutine process_instance_write

  subroutine process_instance_final (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i
    if (allocated (instance%mci_work)) then
       do i = 1, size (instance%mci_work)
          call instance%mci_work(i)%final ()
       end do
    end if
    call instance%sf_chain%final ()
    if (allocated (instance%component)) then
       do i = 1, size (instance%component)
          call instance%component(i)%final ()
       end do
    end if
    if (allocated (instance%term)) then
       do i = 1, size (instance%term)
          call instance%term(i)%final ()
       end do
    end if
    deallocate (instance%nlo_controller)
    instance%evaluation_status = STAT_UNDEFINED
  end subroutine process_instance_final

  subroutine process_instance_reset (instance, reset_mci)
    class(process_instance_t), intent(inout) :: instance
    logical, intent(in), optional :: reset_mci
    integer :: i
    instance%component%active = .false.
    do i = 1, size (instance%term)
       call instance%term(i)%reset ()
    end do
    instance%term%checked = .false.
    instance%term%passed = .false.
    if (present (reset_mci)) then
       if (reset_mci)  instance%i_mci = 0
    end if
    instance%selected_channel = 0
    instance%evaluation_status = STAT_INITIAL
  end subroutine process_instance_reset
  
  subroutine process_instance_activate (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i, j
    associate (mci_work => instance%mci_work(instance%i_mci))
      instance%component(mci_work%get_active_components ())%active &
           = .true.
      do i = 1, size (instance%component)
         associate (component => instance%component(i))
           if (component%active) then
              do j = 1, size (component%config%i_term)
                 instance%term(component%config%i_term(j))%active &
                      = .true.
              end do
           end if
         end associate
      end do
    end associate
    instance%evaluation_status = STAT_ACTIVATED
  end subroutine process_instance_activate
  
  subroutine process_instance_disable_virtual_components (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i
    if (.not. associated (instance%sqme_collector)) &
       call msg_fatal ("Sqme collector must be allocated to prepare for&
                        &component selection")
    do i = 1, size (instance%component)
       associate (component => instance%component(i))
          if (component%config%component_type == COMP_VIRT) then
             component%active = .false.
             instance%term(component%config%i_term)%active = .false.
          end if
       end associate
    end do
  end subroutine process_instance_disable_virtual_components
          
  subroutine process_instance_init (instance, process, cc_flag, &
                                    combined_integration)
    class(process_instance_t), intent(out), target :: instance
    type(process_t), intent(inout), target :: process
    logical, intent(in), optional :: cc_flag
    logical, intent(in), optional :: combined_integration
    integer :: i, i_component
    type(process_constants_t), dimension(2), save :: prc_constants
    integer :: nlo_type
    integer :: i_born, i_real
    logical :: use_internal_cc, use_internal_sc

    instance%process => process
    call instance%setup_sf_chain (process%beam_config)
    allocate (instance%mci_work (process%config%n_mci))
    do i = 1, size (instance%mci_work)
       call instance%mci_work(i)%init (process%mci_entry(i))
    end do
    allocate (instance%component (process%config%n_components))
    allocate (instance%nlo_controller)
    do i_component = 1, size (instance%component)
       if (process%component(i_component)%active) then
          associate (component => instance%component(i_component))
            call component%init (process%component(i_component))
            nlo_type = component%config%config%get_nlo_type ()
            select case (nlo_type)
            case (BORN)
              component%nlo_controller => instance%nlo_controller
              if (associated (instance%sqme_collector)) &
                 call component%set_component_type (COMP_MASTER)
            case (NLO_REAL)
              component%nlo_controller => instance%nlo_controller
              i_born = component%config%config%get_associated_born ()
              call process%term(i_born)%fetch_constants (prc_constants(1))
              call process%term(i_component)%fetch_constants(prc_constants(2))
              if (present (cc_flag)) then
                 use_internal_cc = cc_flag
                 use_internal_sc = .false.
              else
                 use_internal_cc = .true.
                 use_internal_sc = .false.
              end if 
              component%nlo_type = NLO_REAL
              call component%nlo_controller%set_internal_procedures &
                                          (use_internal_cc, use_internal_sc)
              process%component(i_component)%fks_template%id = &
                 prc_constants(1)%id
              call component%nlo_controller%init (prc_constants, &
                   process%component(i_component)%fks_template, &
                   process%config%model)
              call component%nlo_controller%set_flv_born &
                   (process%term(i_born)%flv)
              call component%nlo_controller%set_col_born &
                   (process%term(i_born)%col)
              call component%nlo_controller%set_hel_born &
                   (process%term(i_born)%hel)
              allocate (instance%sqme_real &
                   (size (component%nlo_controller%reg_data%regions)))
              call component%nlo_controller%init_born_amps &
                   (process%get_n_allowed_born (i_born))
              if (associated (instance%sqme_collector)) &
                 call component%set_component_type (COMP_REAL)
            case (NLO_VIRTUAL)
              i_born = component%config%config%get_associated_born ()
              i_real = i_born + process%config%n_components / 3
              component%nlo_controller => instance%nlo_controller
              component%nlo_type = NLO_VIRTUAL
              call component%nlo_controller%init_virtual ()
              if (associated (instance%sqme_collector)) &
                 call component%set_component_type (COMP_VIRT) 
            case (NLO_PDF)
               i_born = component%config%config%get_associated_born ()
               component%nlo_controller => instance%nlo_controller
               component%nlo_type = NLO_PDF
               call component%nlo_controller%init_pdf_subtraction ()
               if (associated (instance%sqme_collector)) &
                  call component%set_component_type (COMP_PDF)
            case (NLO_SUBTRACTION)
               component%nlo_controller => instance%nlo_controller
               component%nlo_type = NLO_SUBTRACTION
            end select
            call component%setup_kinematics &
                 (instance%sf_chain, process%beam_config, &
                  combined_integration)
            if (nlo_type == NLO_REAL .or. nlo_type == NLO_PDF) &
               call component%setup_fks_kinematics (process%meta%var_list)
          end associate
       end if
    end do
    if (associated (instance%sqme_collector)) then
         call instance%sqme_collector%setup_sqme_real &
              (instance%nlo_controller%particle_data%n_in + &
               instance%nlo_controller%particle_data%n_out_born)
    end if
    allocate (instance%term (process%config%n_terms))
    do i = 1, size (instance%term)
       associate (term => instance%term(i))
         i_component = process%term(i)%i_component
         if (i_component /= 0) then
            associate (component => instance%component(i_component))
              nlo_type = component%config%config%get_nlo_type ()
              term%nlo_type = nlo_type
              call component%k_seed%set_nlo_info (nlo_type)

              call term%init (process%term(i), &
                   component%k_seed, &
                   process%beam_config, &
                   process%component(i_component)%core, &
                   process%meta%var_list, component%nlo_controller)
              call term%setup_expressions (process%meta, process%config)
              select case (nlo_type)
              case (NLO_REAL)
                call term%setup_expressions_real (process%meta, process%config)
              end select
           end associate
         end if
       end associate
    end do
    if (present (combined_integration)) then
       if (combined_integration) &
          instance%sqme_collector => instance%nlo_controller%sqme_collector
    end if
    instance%evaluation_status = STAT_INITIAL
  end subroutine process_instance_init
  
  subroutine process_instance_setup_sf_chain (instance, config)
    class(process_instance_t), intent(inout) :: instance
    type(process_beam_config_t), intent(in), target :: config
    integer :: n_strfun
    n_strfun = config%n_strfun
    if (n_strfun /= 0) then
       call instance%sf_chain%init (config%data, config%sf)
    else
       call instance%sf_chain%init (config%data)
    end if
    if (config%sf_trace) then
       call instance%sf_chain%setup_tracing (config%sf_trace_file)
    end if
  end subroutine process_instance_setup_sf_chain
    
  subroutine process_instance_setup_event_data (instance, model)
    class(process_instance_t), intent(inout), target :: instance
    class(model_data_t), intent(in), optional, target :: model
    class(model_data_t), pointer :: current_model
    integer :: i, i_component
    if (present (model)) then
       current_model => model
    else
       current_model => instance%process%config%model
    end if
    do i = 1, size (instance%term)
       associate (term => instance%term(i))
         if (associated (term%config)) then
            i_component = term%config%i_component
            associate (component => instance%process%component(i_component))
              call term%setup_event_data (component%core, current_model)
            end associate
         end if
       end associate
    end do
  end subroutine process_instance_setup_event_data

  subroutine process_instance_choose_mci (instance, i_mci)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    instance%i_mci = i_mci
    call instance%reset ()
  end subroutine process_instance_choose_mci
    
  subroutine process_instance_set_mcpar (instance, x)
    class(process_instance_t), intent(inout) :: instance
    real(default), dimension(:), intent(in) :: x
    if (instance%evaluation_status == STAT_INITIAL) then
       associate (mci_work => instance%mci_work(instance%i_mci))
         call mci_work%set (x)
       end associate
       call instance%activate ()
    end if
  end subroutine process_instance_set_mcpar

  subroutine process_instance_receive_beam_momenta (instance)
    class(process_instance_t), intent(inout) :: instance
    if (instance%evaluation_status >= STAT_INITIAL) then
       call instance%sf_chain%receive_beam_momenta ()
       instance%evaluation_status = STAT_BEAM_MOMENTA
    end if
  end subroutine process_instance_receive_beam_momenta
    
  subroutine process_instance_set_beam_momenta (instance, p)
    class(process_instance_t), intent(inout) :: instance
    type(vector4_t), dimension(:), intent(in) :: p
    if (instance%evaluation_status >= STAT_INITIAL) then
       call instance%sf_chain%set_beam_momenta (p)
       instance%evaluation_status = STAT_BEAM_MOMENTA
    end if
  end subroutine process_instance_set_beam_momenta
    
  subroutine process_instance_recover_beam_momenta (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: i
    if (.not. instance%process%beam_config%lab_is_cm_frame) then
       if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
          i = instance%term(i_term)%config%i_component
          call instance%component(i)%return_beam_momenta ()
       end if
    end if
  end subroutine process_instance_recover_beam_momenta

  subroutine process_instance_select_channel (instance, channel)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: channel
    instance%selected_channel = channel
  end subroutine process_instance_select_channel
  
  subroutine process_instance_compute_seed_kinematics (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: channel, skip_component, i
    logical :: success
    channel = instance%selected_channel
    if (channel == 0) then
       call msg_bug ("Compute seed kinematics: undefined integration channel")
    end if
    if (present (skip_term)) then
       skip_component = instance%term(skip_term)%config%i_component
    else
       skip_component = 0
    end if
    if (instance%evaluation_status >= STAT_ACTIVATED) then
       success = .true.
       do i = 1, size (instance%component)
          if (i == skip_component)  cycle
          if (instance%component(i)%active) then
             call instance%component(i)%compute_seed_kinematics &
                  (instance%mci_work(instance%i_mci), channel, success)
             if (.not. success)  exit
          end if
       end do
       if (success) then
          instance%evaluation_status = STAT_SEED_KINEMATICS
       else
          instance%evaluation_status = STAT_FAILED_KINEMATICS
       end if
    end if
  end subroutine process_instance_compute_seed_kinematics

  subroutine process_instance_recover_mcpar (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: channel
    integer :: i
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
       channel = instance%selected_channel
       if (channel == 0) then
          call msg_bug ("Recover MC parameters: undefined integration channel")
       end if
       i = instance%term(i_term)%config%i_component
       call instance%component(i)%recover_mcpar &
                  (instance%mci_work(instance%i_mci), channel)
    end if
  end subroutine process_instance_recover_mcpar

  subroutine process_instance_compute_hard_kinematics (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: i
    logical :: real_phsp
    if (instance%evaluation_status >= STAT_SEED_KINEMATICS) then
       do i = 1, size (instance%component)
          if (instance%component(i)%active) then
            select case (instance%component(i)%config%config%get_nlo_type())
            case (BORN, NLO_VIRTUAL, NLO_PDF)
              real_phsp = .false.
            case (NLO_REAL)
              real_phsp = .true.
            end select
              call instance%component(i)% &
                  compute_hard_kinematics (instance%term, skip_term, real_phsp)
          end if
       end do
       instance%evaluation_status = STAT_HARD_KINEMATICS
    end if
  end subroutine process_instance_compute_hard_kinematics

  subroutine process_instance_recover_seed_kinematics (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
       associate (i_component => instance%term(i_term)%config%i_component)
         call instance%component(i_component)% &
              recover_seed_kinematics (instance%term(i_term))
       end associate
    end if
  end subroutine process_instance_recover_seed_kinematics
  
  subroutine process_instance_compute_eff_kinematics (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: i
    if (instance%evaluation_status >= STAT_HARD_KINEMATICS) then
       do i = 1, size (instance%term)
          if (present (skip_term)) then
             if (i == skip_term)  cycle
          end if
          if (instance%term(i)%active) then
             call instance%term(i)% &
                  compute_eff_kinematics (instance%component)
          end if
       end do
       instance%evaluation_status = STAT_EFF_KINEMATICS
    end if
  end subroutine process_instance_compute_eff_kinematics

  subroutine process_instance_recover_hard_kinematics (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: i
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
       call instance%term(i_term)%recover_hard_kinematics (instance%component)
       do i = 1, size (instance%term)
          if (i /= i_term) then
             if (instance%term(i)%active) then
                call instance%term(i)% &
                     compute_eff_kinematics (instance%component)
             end if
          end if
       end do
       instance%evaluation_status = STAT_EFF_KINEMATICS
    end if
  end subroutine process_instance_recover_hard_kinematics
       
  subroutine process_instance_evaluate_expressions (instance, scale_forced)
    class(process_instance_t), intent(inout) :: instance
    real(default), intent(in), allocatable, optional :: scale_forced
    integer :: i
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
       do i = 1, size (instance%term)
          if (instance%term(i)%active) then
             call instance%term(i)%evaluate_expressions (scale_forced)
          end if
       end do
       if (any (instance%term%passed)) then
          instance%evaluation_status = STAT_PASSED_CUTS
       else
          instance%evaluation_status = STAT_FAILED_CUTS
       end if
    end if
  end subroutine process_instance_evaluate_expressions

  subroutine process_instance_compute_other_channels (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: channel, skip_component, i
    channel = instance%selected_channel
    if (channel == 0) then
       call msg_bug ("Compute other channels: undefined integration channel")
    end if
    if (present (skip_term)) then
       skip_component = instance%term(skip_term)%config%i_component
    else
       skip_component = 0
    end if
    if (instance%evaluation_status >= STAT_PASSED_CUTS) then
       do i = 1, size (instance%component)
          if (i == skip_component)  cycle
          if (instance%component(i)%active) then
             call instance%component(i)%compute_other_channels &
                  (instance%mci_work(instance%i_mci), channel)
          end if
       end do
    end if
  end subroutine process_instance_compute_other_channels

  subroutine process_instance_evaluate_trace (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i, i_real
    instance%sqme = 0
    i_real = 0
    if (instance%evaluation_status >= STAT_PASSED_CUTS) then
       do i = 1, size (instance%term)
          associate (term => instance%term(i))
            if (term%active .and. term%passed) then
              select case (term%nlo_type)
              case (NLO_REAL)
                 if (.not. associated (instance%sqme_collector)) then
                    call instance%evaluate_trace_real (term, i)
                 else
                    i_real = i
                 end if
              case (BORN, NLO_VIRTUAL, NLO_PDF)
                 call term%evaluate_interaction (instance%component)
                 call term%evaluate_trace ()
              end select
            end if
          end associate
       end do
       do i = 1, size (instance%component)
          associate (component => instance%component(i))
            if (component%active) then
              select case (component%config%config%get_nlo_type())
              case (BORN, NLO_VIRTUAL, NLO_PDF)
                call component%evaluate_sqme (instance%term)
                instance%sqme = instance%sqme + component%sqme
                instance%evaluation_status = STAT_EVALUATED_TRACE
                associate (collector => instance%nlo_controller%sqme_collector)
                   select case (component%get_component_type())
                   case (COMP_DEFAULT, COMP_MASTER)
                      collector%current_sqme_born = component%sqme
                   case (COMP_VIRT)
                      collector%current_sqme_virt = component%sqme
                   end select
                end associate
              end select
            end if
          end associate
       end do
       if (associated (instance%sqme_collector)) &
          call instance%evaluate_trace_real (instance%term(i_real), i_real)
    else
       ! failed kinematics, failed cuts: set sqme to zero
       instance%sqme = 0
    end if
  end subroutine process_instance_evaluate_trace

  subroutine process_instance_evaluate_event_data (instance, weight)
    class(process_instance_t), intent(inout) :: instance
    real(default), intent(in), optional :: weight
    integer :: i
    if (instance%evaluation_status >= STAT_EVALUATED_TRACE) then
       do i = 1, size (instance%term)
          associate (term => instance%term(i))
            if (term%active .and. term%passed) then
               call term%evaluate_event_data ()
            end if
          end associate
       end do
       if (present (weight)) then
          instance%weight = weight
       else
          instance%weight = &
               instance%mci_work(instance%i_mci)%mci%get_event_weight ()
          instance%excess = &
               instance%mci_work(instance%i_mci)%mci%get_event_excess ()
       end if
       instance%evaluation_status = STAT_EVENT_COMPLETE
    else
       ! failed kinematics etc.: set weight to zero
       instance%weight = 0
    end if
  end subroutine process_instance_evaluate_event_data

  subroutine process_instance_evaluate_trace_real (instance, term, i)
     class(process_instance_t), intent(inout) :: instance
     type(term_instance_t), intent(inout) :: term
     integer, intent(in) :: i
     integer :: j, ireg
     integer :: nlegs
     real(default) :: sqme_born

     nlegs = term%nlo_controller%reg_data%nlegs_real
     ireg = 1
     instance%sqme_real = 0
     do j = 1, nlegs
        if (any (term%nlo_controller%reg_data%emitters == j)) then
           call term%nlo_controller%set_active_emitter (j)
           call term%evaluate_interaction_real (instance%component(i), i)
           if (.not. term%passed) then
           !!! Cuts failed, leave subroutine
              instance%evaluation_status = STAT_FAILED_CUTS
              instance%sqme_real = 0
              return
           end if
           call term%evaluate_trace ()
           if (instance%component(i)%active) then
              associate (component => instance%component(i))
                 if (.not. associated (instance%sqme_collector)) then
                    call component%evaluate_sqme_born (term)
                    sqme_born = component%sqme
                 else
                    sqme_born = instance%sqme_collector%current_sqme_born
                 end if
                 call associate_sqme_born (component, sqme_born*term%weight)
                 call component%evaluate_sqme_real (term)
                 instance%sqme_real(ireg) = &
                   component%nlo_controller%sqme_collector%current_sqme_real
                 if (associated (instance%sqme_collector)) then
                    instance%sqme_collector%sqme_real_per_emitter(j) = &
                                          instance%sqme_real(ireg)
                 end if
              end associate
           end if
           ireg = ireg + 1
        end if
     end do
     instance%sqme = instance%sqme + sum (instance%sqme_real)
     if (associated (instance%sqme_collector)) &
        instance%sqme_collector%sqme_real_sum = sum (instance%sqme_real)
     instance%evaluation_status = STAT_EVALUATED_TRACE  
  end subroutine process_instance_evaluate_trace_real

  subroutine process_instance_evaluate_trace_real_rad (instance, term, i)
    class(process_instance_t), intent(inout) :: instance
    type(term_instance_t), intent(inout) :: term
    integer, intent(in) :: i
    integer :: emitter
    
    call term%evaluate_trace ()
    emitter = term%nlo_controller%active_emitter
    associate (component => instance%component(i), &
         collector => instance%sqme_collector)
      call component%evaluate_sqme_real (term)
      collector%sqme_real_per_emitter (emitter) = collector%current_sqme_real
    end associate 
  end subroutine process_instance_evaluate_trace_real_rad
    
  subroutine process_instance_compute_sqme_real_rad &
       (instance, emitter, p_born, p_real, alpha_s)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born 
    type(vector4_t), intent(in), dimension(:), allocatable :: p_real
    real(default), intent(in) :: alpha_s
    integer :: i_real
    
    if (.not. associated (instance%sqme_collector)) &
         call msg_fatal ("Compute radiation matrix elements: " // &
         "Sqme collector must be allocated!")
    call instance%nlo_controller%set_active_emitter (emitter)
    call instance%nlo_controller%disable_subtraction () 
    i_real = instance%component(1)%config%config%get_associated_real ()
    associate (term => instance%term(i_real))
      call term%evaluate_interaction_real_rad (instance%component(i_real), &
           p_born, p_real, alpha_s, i_real)
       call instance%evaluate_trace_real_rad (term, i_real)
    end associate
  end subroutine process_instance_compute_sqme_real_rad

  subroutine process_instance_get_matrix_elements &
       (instance, sqme_born, sqme_real)
    class(process_instance_t), intent(inout) :: instance
    real(default), intent(out) :: sqme_born, sqme_real
    integer :: emitter
    emitter = instance%nlo_controller%active_emitter 
    sqme_born = instance%sqme_collector%current_sqme_born
    sqme_real = instance%sqme_collector%sqme_real_per_emitter (emitter)
  end subroutine process_instance_get_matrix_elements

  subroutine process_instance_normalize_weight (instance)
    class(process_instance_t), intent(inout) :: instance
    if (.not. vanishes (instance%weight)) then
       instance%weight = sign (1._default, instance%weight)
    end if
  end subroutine process_instance_normalize_weight
  
  subroutine process_instance_evaluate_sqme (instance, channel, x)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: channel
    real(default), dimension(:), intent(in) :: x
    call instance%reset ()
    call instance%set_mcpar (x)
    call instance%select_channel (channel)
    call instance%compute_seed_kinematics ()
    call instance%compute_hard_kinematics ()
    call instance%compute_eff_kinematics ()
    call instance%evaluate_expressions ()
    call instance%compute_other_channels ()
    call instance%evaluate_trace ()
    if (associated (instance%sqme_collector)) &
       instance%sqme = instance%sqme_collector%get_sqme_sum ()
  end subroutine process_instance_evaluate_sqme
  
  subroutine process_instance_recover &
       (instance, channel, i_term, update_sqme, scale_forced)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: channel
    integer, intent(in) :: i_term
    logical, intent(in) :: update_sqme
    real(default), intent(in), allocatable, optional :: scale_forced
    call instance%activate ()
    instance%evaluation_status = STAT_EFF_KINEMATICS
    call instance%recover_hard_kinematics (i_term)
    call instance%recover_seed_kinematics (i_term)
    call instance%select_channel (channel)
    call instance%recover_mcpar (i_term)
    call instance%recover_beam_momenta (i_term)
    call instance%compute_seed_kinematics (i_term)
    call instance%compute_hard_kinematics (i_term)
    call instance%compute_eff_kinematics (i_term)
    call instance%compute_other_channels (i_term)
    call instance%evaluate_expressions (scale_forced)
    if (update_sqme)  call instance%evaluate_trace ()
  end subroutine process_instance_recover
  
  subroutine process_instance_evaluate (sampler, c, x_in, val, x, f)
    class(process_instance_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call sampler%evaluate_sqme (c, x_in)
    if (sampler%is_valid ())  call sampler%fetch (val, x, f)
    call sampler%record_call ()
  end subroutine process_instance_evaluate

  function process_instance_is_valid (sampler) result (valid)
    class(process_instance_t), intent(in) :: sampler
    logical :: valid
    valid = sampler%evaluation_status >= STAT_PASSED_CUTS
  end function process_instance_is_valid
  
  subroutine process_instance_rebuild (sampler, c, x_in, val, x, f)
    class(process_instance_t), intent(inout) :: sampler
    integer, intent(in) :: c
    real(default), dimension(:), intent(in) :: x_in
    real(default), intent(in) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    call msg_bug ("process_instance_rebuild not implemented yet")
    x = 0
    f = 0
  end subroutine process_instance_rebuild

  subroutine process_instance_fetch (sampler, val, x, f)
    class(process_instance_t), intent(in) :: sampler
    real(default), intent(out) :: val
    real(default), dimension(:,:), intent(out) :: x
    real(default), dimension(:), intent(out) :: f
    integer :: i, cc
    integer :: n_channel
    val = 0
    FIND_COMPONENT: do i = 1, size (sampler%component)
       associate (component => sampler%component(i))
         if (component%active) then
            associate (k => component%k_seed)
              n_channel = k%n_channel
              do cc = 1, n_channel
                 call k%get_mcpar (cc, x(:,cc))
              end do
              f = k%f
              val = sampler%sqme * k%phs_factor
            end associate
            exit FIND_COMPONENT
         end if
       end associate
    end do FIND_COMPONENT
  end subroutine process_instance_fetch
  
  subroutine process_instance_init_simulation (instance, i_mci, safety_factor)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    real(default), intent(in), optional :: safety_factor
    call instance%mci_work(i_mci)%init_simulation (safety_factor)
  end subroutine process_instance_init_simulation

  subroutine process_instance_final_simulation (instance, i_mci)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    call instance%mci_work(i_mci)%final_simulation ()
  end subroutine process_instance_final_simulation

  subroutine process_instance_get_mcpar (instance, channel, x)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: channel
    real(default), dimension(:), intent(out) :: x
    integer :: i
    if (instance%evaluation_status >= STAT_SEED_KINEMATICS) then
       do i = 1, size (instance%component)
          if (instance%component(i)%active) then
             call instance%component(i)%k_seed%get_mcpar (channel, x)
             return
          end if
       end do
       call msg_bug ("Process instance: get_mcpar: no active channels")
    else
       call msg_bug ("Process instance: get_mcpar: no seed kinematics")
    end if
  end subroutine process_instance_get_mcpar

  function process_instance_has_evaluated_trace (instance) result (flag)
    class(process_instance_t), intent(in) :: instance
    logical :: flag
    flag = instance%evaluation_status >= STAT_EVALUATED_TRACE
  end function process_instance_has_evaluated_trace
  
  function process_instance_is_complete_event (instance) result (flag)
    class(process_instance_t), intent(in) :: instance
    logical :: flag
    flag = instance%evaluation_status >= STAT_EVENT_COMPLETE
  end function process_instance_is_complete_event
  
  subroutine process_instance_select_i_term (instance, i_term)
    class(process_instance_t), intent(in) :: instance
    integer, intent(out) :: i_term
    integer :: i_mci, i_component
    i_mci = instance%i_mci
    i_component = instance%process%mci_entry(i_mci)%i_component(1)
    i_term = instance%process%component(i_component)%i_term(1)
  end subroutine process_instance_select_i_term
    
  function process_instance_get_beam_int_ptr (instance) result (ptr)
    class(process_instance_t), intent(in), target :: instance
    type(interaction_t), pointer :: ptr
    ptr => instance%sf_chain%get_beam_int_ptr ()
  end function process_instance_get_beam_int_ptr
  
  function process_instance_get_trace_int_ptr (instance, i_term) result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(interaction_t), pointer :: ptr
    ptr => instance%term(i_term)%connected%get_trace_int_ptr ()
  end function process_instance_get_trace_int_ptr
  
  function process_instance_get_matrix_int_ptr (instance, i_term) result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(interaction_t), pointer :: ptr
    select case (instance%term(i_term)%nlo_type)
    case (BORN, NLO_VIRTUAL, NLO_PDF)
      ptr => instance%term(i_term)%connected%get_matrix_int_ptr ()
    case (NLO_REAL)
      ptr => instance%term(i_term)%connected_real%get_matrix_int_ptr ()
    end select
  end function process_instance_get_matrix_int_ptr
  
  function process_instance_get_flows_int_ptr (instance, i_term) result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(interaction_t), pointer :: ptr
    select case (instance%term(i_term)%nlo_type)
    case (BORN, NLO_VIRTUAL, NLO_PDF)
      ptr => instance%term(i_term)%connected%get_flows_int_ptr ()
    case (NLO_REAL)
      ptr => instance%term(i_term)%connected_real%get_flows_int_ptr ()
    end select
  end function process_instance_get_flows_int_ptr
  
  function process_instance_get_state_flv (instance, i_term) result (state_flv)
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    type(state_flv_content_t) :: state_flv
    state_flv = instance%term(i_term)%connected%get_state_flv ()
  end function process_instance_get_state_flv
  
  function process_instance_get_isolated_state_ptr (instance, i_term) &
       result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(isolated_state_t), pointer :: ptr
    ptr => instance%term(i_term)%isolated
  end function process_instance_get_isolated_state_ptr
  
  function process_instance_get_connected_state_ptr (instance, i_term) &
       result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(connected_state_t), pointer :: ptr
    ptr => instance%term(i_term)%connected
  end function process_instance_get_connected_state_ptr
  
  subroutine process_instance_get_beam_index (instance, i_term, i_beam)
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    integer, dimension(:), intent(out) :: i_beam
    call instance%term(i_term)%connected%get_beam_index (i_beam)
  end subroutine process_instance_get_beam_index
  
  subroutine process_instance_get_in_index (instance, i_term, i_in)
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    integer, dimension(:), intent(out) :: i_in
    call instance%term(i_term)%connected%get_in_index (i_in)
  end subroutine process_instance_get_in_index
  
  function process_instance_get_sqme (instance) result (sqme)
    class(process_instance_t), intent(in) :: instance
    real(default) :: sqme
    if (instance%evaluation_status >= STAT_EVALUATED_TRACE) then
       sqme = instance%sqme
    else
       sqme = 0
    end if
  end function process_instance_get_sqme
  
  function process_instance_get_weight (instance) result (weight)
    class(process_instance_t), intent(in) :: instance
    real(default) :: weight
    if (instance%evaluation_status >= STAT_EVENT_COMPLETE) then
       weight = instance%weight
    else
       weight = 0
    end if
  end function process_instance_get_weight
  
  function process_instance_get_excess (instance) result (excess)
    class(process_instance_t), intent(in) :: instance
    real(default) :: excess
    if (instance%evaluation_status >= STAT_EVENT_COMPLETE) then
       excess = instance%excess
    else
       excess = 0
    end if
  end function process_instance_get_excess
  
  function process_instance_get_channel (instance) result (channel)
    class(process_instance_t), intent(in) :: instance
    integer :: channel
    channel = instance%selected_channel
  end function process_instance_get_channel

  subroutine process_instance_set_fac_scale (instance, fac_scale)
    class(process_instance_t), intent(inout) :: instance
    real(default), intent(in) :: fac_scale
    integer :: i_term
    i_term = 1
    call instance%term(i_term)%set_fac_scale (fac_scale)
  end subroutine process_instance_set_fac_scale

  function process_instance_get_fac_scale (instance, i_term) result (fac_scale)
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    real(default) :: fac_scale
    fac_scale = instance%term(i_term)%get_fac_scale ()
  end function process_instance_get_fac_scale
  
  function process_instance_get_alpha_s (instance, i_term) result (alpha_s)
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    real(default) :: alpha_s
    alpha_s = instance%term(i_term)%get_alpha_s (instance%component)
  end function process_instance_get_alpha_s
  
  subroutine process_instance_reset_counter (process_instance)
    class(process_instance_t), intent(inout) :: process_instance
    call process_instance%mci_work(process_instance%i_mci)%reset_counter ()
  end subroutine process_instance_reset_counter
  
  subroutine process_instance_record_call (process_instance)
    class(process_instance_t), intent(inout) :: process_instance
    call process_instance%mci_work(process_instance%i_mci)%record_call &
         (process_instance%evaluation_status)
  end subroutine process_instance_record_call
    
  function process_instance_get_counter (process_instance) result (counter)
    class(process_instance_t), intent(in) :: process_instance
    type(process_counter_t) :: counter
    counter = process_instance%mci_work(process_instance%i_mci)%get_counter ()
  end function process_instance_get_counter
  
  subroutine process_instance_get_trace (instance, pset, i_term)
    class(process_instance_t), intent(in), target :: instance
    type(particle_set_t), intent(out) :: pset
    integer, intent(in) :: i_term
    type(interaction_t), pointer :: int
    logical :: ok
    int => instance%get_trace_int_ptr (i_term)
    call pset%init (ok, int, int, FM_IGNORE_HELICITY, &
         [0._default, 0._default], .false., .true.)
  end subroutine process_instance_get_trace
    
  subroutine process_instance_set_trace &
       (instance, pset, i_term, recover_beams, check_match)
    class(process_instance_t), intent(inout), target :: instance
    type(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i_term
    logical, intent(in), optional :: recover_beams, check_match
    type(interaction_t), pointer :: int
    integer :: n_in
    int => instance%get_trace_int_ptr (i_term)
    n_in = instance%process%get_n_in ()
    call pset%fill_interaction (int, n_in, &
         recover_beams = recover_beams, &
         check_match = check_match, &
         state_flv = instance%get_state_flv (i_term))
  end subroutine process_instance_set_trace

  subroutine process_instance_set_alpha_qcd_forced (instance, i_term, alpha_qcd)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    real(default), intent(in) :: alpha_qcd
    call instance%term(i_term)%set_alpha_qcd_forced (alpha_qcd)
  end subroutine process_instance_set_alpha_qcd_forced
  
  subroutine process_instance_display_real_kinematics (instance, i)
     class(process_instance_t), intent(in) :: instance
     integer, intent(in) :: i
     if (associated (instance%term(i)%nlo_controller)) then
        if (instance%term(i)%nlo_controller%counter_exists) &
            call instance%term(i)%nlo_controller%counter%display ()
     end if
  end subroutine process_instance_display_real_kinematics

  function process_instance_has_nlo_component (instance) result (nlo)
    class(process_instance_t), intent(in) :: instance
    logical :: nlo
    nlo = any (instance%component%nlo_type /= BORN)
  end function process_instance_has_nlo_component

  subroutine process_instance_create_blha_interface (instance)
    class(process_instance_t), intent(inout) :: instance
    logical :: use_external_loops
    logical :: use_external_correlations
    logical :: use_external_real_trees
    integer :: alpha_power, alphas_power
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    integer :: i
    type(blha_master_t) :: blha_master
    type(string_t) :: loop_me_method
    type(string_t) :: correlation_me_method
    type(string_t) :: real_tree_me_method

    associate (process => instance%process)
       associate (var_list => process%meta%var_list)
          loop_me_method = var_list%get_sval (var_str ("$loop_me_method"))
          correlation_me_method = var_list%get_sval (var_str ("$correlation_me_method"))
          real_tree_me_method = var_list%get_sval (var_str ("$real_tree_me_method"))
          alpha_power = var_list%get_ival (&
                                           var_str ('alpha_power'))
          alphas_power = var_list%get_ival (&
                                           var_str ('alphas_power'))
       end associate
       do i = 1, size (process%term)
          if (instance%component(i)%nlo_type == BORN) then
             flv_born = process%term(i)%data%flv_state
          else if (instance%component(i)%nlo_type == NLO_REAL) then
             flv_real = process%term(i)%data%flv_state
          end if
       end do
       use_external_loops = loop_me_method /= 'omega'
       use_external_correlations = correlation_me_method /= 'omega'
       use_external_real_trees = real_tree_me_method /= 'omega'

       select case (char (loop_me_method))
       case ('gosam')
          call blha_master%set_gosam (1)
       case ('openloops')
          call blha_master%set_openloops (1)
       end select
       select case (char (correlation_me_method))
       case ('gosam')
          call blha_master%set_gosam (2)
       case ('openloops')
          call blha_master%set_openloops (2)
       end select
       select case (char (real_tree_me_method))
       case ('gosam')
          call blha_master%set_gosam (3)
       case ('openloops')
          call blha_master%set_openloops (3)
       end select

       call blha_master%init (process%meta%id, process%config%model, &
                              2, size (flv_born,1)-2, use_external_loops, &
                              use_external_correlations, use_external_real_trees, &
                              alpha_power, alphas_power, &
                              flv_born, flv_real)
       call blha_master%generate (process%meta%id)
    end associate 
  end subroutine process_instance_create_blha_interface 

  subroutine process_instance_load_blha_libraries (instance, os_data)
    class(process_instance_t), intent(inout) :: instance
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: libname
    logical :: lib_found
    integer :: i
    libname = instance%process%get_library_name ()
    do i = 1, size (instance%component)
       select type (core => instance%component(i)%config%core)
       type is (prc_gosam_t)
          call core%search_for_existing_library (os_data, lib_found)
          call core%create_olp_library (libname, lib_found)
          call core%load_driver (os_data, .not. lib_found)
          call core%start ()
          call core%read_contract_file (instance%process%term(i)%data%flv_state)
          call core%set_particle_properties (instance%process%config%model)
       type is (prc_openloops_t)
         call core%load_driver (os_data)
         call core%reset_parameters ()
         call core%set_bquark_mass (instance%process%config%model)
         call core%set_verbosity (3)
         call core%start ()
         call core%read_contract_file (instance%process%term(i)%data%flv_state)
         call core%set_particle_properties (instance%process%config%model)
       end select
    end do
  end subroutine process_instance_load_blha_libraries

  subroutine process_instance_set_blha_constants (instance, var_list)
    class(process_instance_t), intent(inout) :: instance
    type(var_list_t), intent(in), pointer :: var_list
    real(default) :: alpha
    integer :: i
    
    do i = 1, size (instance%component)
       select type (core => instance%component(i)%config%core)
       type is (prc_openloops_t)
           alpha = 1._default / var_list%get_rval (var_str ("alpha_em_i"))
           call core%set_alpha_qed (alpha)
        end select
    end do
  end subroutine process_instance_set_blha_constants
    
  subroutine pacify_process_instance (instance)
    type(process_instance_t), intent(inout) :: instance
    integer :: i
    do i = 1, size (instance%component)
       call pacify (instance%component(i)%k_seed%phs)
    end do
  end subroutine pacify_process_instance
    
  subroutine setup_nlo_component_cores (process)
    type(process_t), intent(inout), target :: process
    integer :: i_born, i_sub
    integer :: n_components, i_component
    integer :: nlo_type
    n_components = process%get_n_components ()
    do i_component = 1, n_components
       nlo_type = process%get_component_nlo_type (i_component)
       i_born = process%get_component_associated_born (i_component)
       i_sub = i_born + 3
       select case (nlo_type)
       case (NLO_REAL, NLO_VIRTUAL)
          associate (component => process%component (i_component))
             component%core_sub_born => process%component(i_sub)%core 
          end associate
       end select
    end do
  end subroutine setup_nlo_component_cores

  subroutine prepare_test_process (process, process_instance, model)
    type(process_t), intent(out), target :: process
    type(process_instance_t), intent(out), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    class(model_data_t), pointer :: process_model
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    libname = "processes_test"
    procname = libname
    run_id = "run_test"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)
    call reset_interaction_counter ()
    allocate (process_model)
    call process_model%init (model%get_name (), &
         model%get_n_real (), &
         model%get_n_complex (), &
         model%get_n_field (), &
         model%get_n_vtx ())
    call process_model%copy_from (model)
    call process%init (procname, run_id, &
         lib, os_data, qcd, rng_factory, process_model)
    allocate (test_t :: core_template)
    allocate (mci_test_t :: mci_template)
    select type (mci_template)
    type is (mci_test_t);  call mci_template%set_divisions (100)
    end select
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)
    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()
    call process%setup_terms ()
    call process_instance%init (process)
    select type (mci => process%mci_entry(1)%mci)
    type is (mci_test_t)
       ! This ensures that the next 'random' numbers are 0.3, 0.5, 0.7
       call mci%rng%init (3)
       ! Include the constant PHS factor in the stored maximum of the integrand
       call mci%set_max_factor (conv * twopi4 &
            / (2 * sqrt (lambda (sqrts **2, 125._default**2, 125._default**2))))
    end select
  end subroutine prepare_test_process

  subroutine cleanup_test_process (process, process_instance)
    type(process_t), intent(inout) :: process
    type(process_instance_t), intent(inout) :: process_instance
    call process_instance%final ()
    call process%final ()
  end subroutine cleanup_test_process
    

  subroutine processes_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (processes_1, "processes_1", &
         "write an empty process object", &
         u, results)
    call test (processes_2, "processes_2", &
         "initialize a simple process object", &
         u, results)
    call test (processes_3, "processes_3", &
         "retrieve a trivial matrix element", &
         u, results)
    call test (processes_4, "processes_4", &
         "create and fill a process instance (partonic event)", &
         u, results)
    call test (processes_7, "processes_7", &
         "process configuration with structure functions", &
         u, results)
    call test (processes_8, "processes_8", &
         "process evaluation with structure functions", &
         u, results)
    call test (processes_9, "processes_9", &
         "multichannel kinematics and structure functions", &
         u, results)
    call test (processes_10, "processes_10", &
         "event generation", &
         u, results)
    call test (processes_11, "processes_11", &
         "integration", &
         u, results)
    call test (processes_12, "processes_12", &
         "event post-processing", &
         u, results)
    call test (processes_13, "processes_13", &
         "colored interaction", &
         u, results)
    call test (processes_14, "processes_14", &
         "process configuration and MD5 sum", &
         u, results)
    call test (processes_15, "processes_15", &
         "decay process", &
         u, results)
    call test (processes_16, "processes_16", &
         "decay integration", &
         u, results)
    call test (processes_17, "processes_17", &
         "decay of moving particle", &
         u, results)
  end subroutine processes_test
  
  subroutine test_write (object, unit)
    class(test_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A)")  "test type implementing prc_test"
  end subroutine test_write
  
  function test_needs_mcset (object) result (flag)
    class(test_t), intent(in) :: object
    logical :: flag
    flag = .true.
  end function test_needs_mcset
  
  function test_get_n_terms (object) result (n)
    class(test_t), intent(in) :: object
    integer :: n
    n = 1
  end function test_get_n_terms
  
  function test_is_allowed (object, i_term, f, h, c) result (flag)
    class(test_t), intent(in) :: object
    integer, intent(in) :: i_term, f, h, c
    logical :: flag
    flag = .true.
  end function test_is_allowed
  
  subroutine test_compute_hard_kinematics &
       (object, p_seed, i_term, int_hard, core_state)
    class(test_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(in) :: p_seed
    integer, intent(in) :: i_term
    type(interaction_t), intent(inout) :: int_hard
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    call int_hard%set_momenta (p_seed)
  end subroutine test_compute_hard_kinematics
  
  subroutine test_compute_eff_kinematics &
       (object, i_term, int_hard, int_eff, core_state)
    class(test_t), intent(in) :: object
    integer, intent(in) :: i_term
    type(interaction_t), intent(in) :: int_hard
    type(interaction_t), intent(inout) :: int_eff
    class(prc_core_state_t), intent(inout), allocatable :: core_state
  end subroutine test_compute_eff_kinematics
  
  subroutine test_recover_kinematics &
       (object, p_seed, int_hard, int_eff, core_state)
    class(test_t), intent(in) :: object
    type(vector4_t), dimension(:), intent(inout) :: p_seed
    type(interaction_t), intent(inout) :: int_hard
    type(interaction_t), intent(inout) :: int_eff
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    integer :: n_in
    n_in = int_eff%get_n_in ()
    call int_eff%set_momenta (p_seed(1:n_in), outgoing = .false.)
    p_seed(n_in+1:) = int_eff%get_momenta (outgoing = .true.)
  end subroutine test_recover_kinematics
    
  function test_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, core_state) &
       result (amp)
    class(test_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: core_state
    complex(default) :: amp
    real(default), dimension(:,:), allocatable :: parray
    integer :: i, n_tot
    select type (driver => object%driver)
    type is (prc_test_t)
       if (driver%scattering) then
          n_tot = 4
       else
          n_tot = 3
       end if
       allocate (parray (0:3,n_tot))
       forall (i = 1:n_tot)  parray(:,i) = vector4_get_components (p(i))
       amp = driver%get_amplitude (parray)
    end select
  end function test_compute_amplitude
    
  subroutine processes_1 (u)
    integer, intent(in) :: u
    type(process_t) :: process

    write (u, "(A)")  "* Test output: processes_1"
    write (u, "(A)")  "*   Purpose: display an empty process object"
    write (u, "(A)")

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_1"
    
  end subroutine processes_1
  
  subroutine processes_2 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template

    write (u, "(A)")  "* Test output: processes_2"
    write (u, "(A)")  "*   Purpose: initialize a simple process object"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a test library with one process"
    write (u, "(A)")

    libname = "processes2"
    procname = libname
    run_id = "run2"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    write (u, "(A)")  "* Initialize a process object"
    write (u, "(A)")

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    call process%setup_mci ()

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call process%final ()
    deallocate (process)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_2"
    
  end subroutine processes_2
  
  subroutine processes_3 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    type(process_constants_t) :: data
    type(vector4_t), dimension(:), allocatable :: p

    write (u, "(A)")  "* Test output: processes_3"
    write (u, "(A)")  "*   Purpose: create a process &
         &and compute a matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a test library with one process"
    write (u, "(A)")

    libname = "processes3"
    procname = libname
    run_id = "run3"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (mci_test_t :: mci_template)
    select type (mci_template)
    type is (mci_test_t)
       call mci_template%set_dimensions (2, 2)
       call mci_template%set_divisions (100)
    end select
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Return the number of process components"
    write (u, "(A)")

    write (u, "(A,I0)")  "n_components = ", process%get_n_components ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Return the number of flavor states"
    write (u, "(A)")

    data = process%get_constants (1)
    
    write (u, "(A,I0)")  "n_flv(1) = ", data%n_flv
    
    write (u, "(A)")
    write (u, "(A)")  "* Return the first flavor state"
    write (u, "(A)")

    write (u, "(A,4(1x,I0))")  "flv_state(1) =", data%flv_state (:,1)

    write (u, "(A)")
    write (u, "(A)")  "* Set up kinematics &
         &[arbitrary, the matrix element is constant]"
    
    allocate (p (4))

    write (u, "(A)")
    write (u, "(A)")  "* Retrieve the matrix element"
    write (u, "(A)")

    write (u, "(A,F5.3,' + ',F5.3,' I')")  "me (1, p, 1, 1, 1) = ", &
         process%compute_amplitude (1, 1, p, 1, 1, 1)
    

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_3"
    
  end subroutine processes_3
  
  subroutine processes_4 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: pset

    write (u, "(A)")  "* Test output: processes_4"
    write (u, "(A)")  "*   Purpose: create a process &
         &and fill a process instance"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a test process"
    write (u, "(A)")

    libname = "processes4"
    procname = libname
    run_id = "run4"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model) 
   
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")
    
    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()

    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)

    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Inject a set of random numbers"
    write (u, "(A)")
     
    call process_instance%choose_mci (1)
    call process_instance%set_mcpar ([0._default, 0._default])
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Set up hard kinematics"
    write (u, "(A)")
  
    call process_instance%select_channel (1)
    call process_instance%compute_seed_kinematics ()
    call process_instance%compute_hard_kinematics ()
    call process_instance%compute_eff_kinematics ()
    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels ()

    write (u, "(A)")  "* Evaluate matrix element and square"
    write (u, "(A)")
  
    call process_instance%evaluate_trace ()
    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)
    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")
    
    call write_separator (u)
    call pset%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)

    call process_instance%activate ()
    process_instance%evaluation_status = STAT_EFF_KINEMATICS
    call process_instance%recover_hard_kinematics (i_term = 1)
    call process_instance%recover_seed_kinematics (i_term = 1)
    call process_instance%select_channel (1)
    call process_instance%recover_mcpar (i_term = 1)

    call process_instance%compute_seed_kinematics (skip_term = 1)
    call process_instance%compute_hard_kinematics (skip_term = 1)
    call process_instance%compute_eff_kinematics (skip_term = 1)

    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels (skip_term = 1)
    call process_instance%evaluate_trace ()
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call pset%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_4"
    
  end subroutine processes_4
  
  subroutine processes_7 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_channel_t), dimension(2) :: sf_channel

    write (u, "(A)")  "* Test output: processes_7"
    write (u, "(A)")  "*   Purpose: initialize a process with &
         &structure functions"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes7"
    procname = libname
    run_id = "run7"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Set beam, structure functions, and mappings"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    
    pdg_in = 25
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       model => process%get_model_ptr ()
       call data%init (model, pdg_in)
    end select

    allocate (sf_config (2))
    call sf_config(1)%init ([1], data)
    call sf_config(2)%init ([2], data)
    call process%init_sf_chain (sf_config)
    deallocate (sf_config)

    call process%beam_config%allocate_sf_channels (3)

    call sf_channel(1)%init (2)
    call sf_channel(1)%activate_mapping ([1,2])
    call process%set_sf_channel (2, sf_channel(1))
    
    call sf_channel(2)%init (2)
    call sf_channel(2)%set_s_mapping ([1,2])
    call process%set_sf_channel (3, sf_channel(2))
    
    call process%setup_mci ()

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call process%final ()
    deallocate (process)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_7"
    
  end subroutine processes_7
  
  subroutine processes_8 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_channel_t) :: sf_channel
    type(particle_set_t) :: pset

    write (u, "(A)")  "* Test output: processes_8"
    write (u, "(A)")  "*   Purpose: evaluate a process with &
         &structure functions"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes8"
    procname = libname
    run_id = "run8"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Set beam, structure functions, and mappings"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)

    pdg_in = 25
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       model => process%get_model_ptr ()
       call data%init (model, pdg_in)
    end select

    allocate (sf_config (2))
    call sf_config(1)%init ([1], data)
    call sf_config(2)%init ([2], data)
    call process%init_sf_chain (sf_config)
    deallocate (sf_config)
    
    call process%configure_phs ()
    
    call process%beam_config%allocate_sf_channels (1)

    call sf_channel%init (2)
    call sf_channel%activate_mapping ([1,2])
    call process%set_sf_channel (1, sf_channel)
    
    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_mci ()
    call process%setup_terms ()

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")  "* Set up kinematics and evaluate"
    write (u, "(A)")

    call process_instance%choose_mci (1)
    call process_instance%evaluate_sqme (1, &
         [0.8_default, 0.8_default, 0.1_default, 0.2_default])
    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)
    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")
    
    call write_separator (u)
    call pset%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover process instance"
    write (u, "(A)")

    call reset_interaction_counter (2)

    allocate (process_instance)
    call process_instance%init (process)

    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)
    call process_instance%recover &
         (channel = 1, i_term = 1, update_sqme = .true.)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call pset%final ()

    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_8"
    
  end subroutine processes_8
  
  subroutine processes_9 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_channel_t) :: sf_channel
    real(default), dimension(4) :: x_saved
    type(particle_set_t) :: pset

    write (u, "(A)")  "* Test output: processes_9"
    write (u, "(A)")  "*   Purpose: evaluate a process with &
         &structure functions"
    write (u, "(A)")  "*            in a multi-channel configuration"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes9"
    procname = libname
    run_id = "run9"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Set beam, structure functions, and mappings"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)

    pdg_in = 25
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       model => process%get_model_ptr ()
       call data%init (model, pdg_in)
    end select

    allocate (sf_config (2))
    call sf_config(1)%init ([1], data)
    call sf_config(2)%init ([2], data)
    call process%init_sf_chain (sf_config)
    deallocate (sf_config)
    
    call process%configure_phs ()
    
    call process%beam_config%allocate_sf_channels (2)

    call sf_channel%init (2)
    call process%set_sf_channel (1, sf_channel)
    
    call sf_channel%init (2)
    call sf_channel%activate_mapping ([1,2])
    call process%set_sf_channel (2, sf_channel)
    
    call process%component(1)%phs_config%set_sf_channel ([1, 2])

    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_mci ()
    call process%setup_terms ()

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")  "* Set up kinematics in channel 1 and evaluate"
    write (u, "(A)")

    call process_instance%choose_mci (1)
    call process_instance%evaluate_sqme (1, &
         [0.8_default, 0.8_default, 0.1_default, 0.2_default])
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Extract MC input parameters"
    write (u, "(A)")
    
    write (u, "(A)")  "Channel 1:"
    call process_instance%get_mcpar (1, x_saved)
    write (u, "(2x,9(1x,F7.5))")  x_saved

    write (u, "(A)")  "Channel 2:"
    call process_instance%get_mcpar (2, x_saved)
    write (u, "(2x,9(1x,F7.5))")  x_saved

    write (u, "(A)")
    write (u, "(A)")  "* Set up kinematics in channel 2 and evaluate"
    write (u, "(A)")

    call process_instance%evaluate_sqme (2, x_saved)
    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)
    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Recover process instance for channel 2"
    write (u, "(A)")

    call reset_interaction_counter (2)

    allocate (process_instance)
    call process_instance%init (process)

    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)
    call process_instance%recover &
         (channel = 2, i_term = 1, update_sqme = .true.)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call pset%final ()

    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_9"
    
  end subroutine processes_9
  
  subroutine processes_10 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: processes_10"
    write (u, "(A)")  "*   Purpose: generate events for a process without &
         &structure functions"
    write (u, "(A)")  "*            in a multi-channel configuration"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes10"
    procname = libname
    run_id = "run10"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (mci_test_t :: mci_template)
    select type (mci_template)
    type is (mci_test_t);  call mci_template%set_divisions (100)
    end select
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()

    call process%setup_mci ()
    
    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")  "* Generate weighted event"
    write (u, "(A)")

    select type (mci => process%mci_entry(1)%mci)
    type is (mci_test_t)
       ! This ensures that the next 'random' numbers are 0.3, 0.5, 0.7
       call mci%rng%init (3)
       ! Include the constant PHS factor in the stored maximum of the integrand
       call mci%set_max_factor (conv * twopi4 &
            / (2 * sqrt (lambda (sqrts **2, 125._default**2, 125._default**2))))
    end select

    call process%generate_weighted_event (process_instance, 1)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate unweighted event"
    write (u, "(A)")

    call process%generate_unweighted_event (process_instance, 1)
    select type (mci => process%mci_entry(1)%mci)
    type is (mci_test_t)
       write (u, "(A,I0)")  " Success in try ", mci%tries
       write (u, "(A)")
    end select
    
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_10"
    
  end subroutine processes_10
  
  subroutine processes_11 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: processes_11"
    write (u, "(A)")  "*   Purpose: integrate a process without &
         &structure functions"
    write (u, "(A)")  "*            in a multi-channel configuration"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes11"
    procname = libname
    run_id = "run11"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (mci_test_t :: mci_template)
    select type (mci_template)
    type is (mci_test_t)
       call mci_template%set_divisions (100)
    end select
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()

    call process%setup_mci ()
    
    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")  "* Integrate with default test parameters"
    write (u, "(A)")

    call process%integrate (process_instance, 1, n_it=1, n_calls=10000)
    call process%final_integration (1)
    
    call process%write (.false., u)

    write (u, "(A)")
    write (u, "(A,ES13.7)")  " Integral divided by phs factor = ", &
         process%get_integral (1) &
         / process_instance%component(1)%k_seed%phs_factor

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_11"
    
  end subroutine processes_11
  
  subroutine processes_12 (u)
    integer, intent(in) :: u
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: pset
    type(model_data_t), target :: model

    write (u, "(A)")  "* Test output: processes_12"
    write (u, "(A)")  "*   Purpose: generate a complete partonic event"
    write (u, "(A)")

    call model%init_test ()

    write (u, "(A)")  "* Build and initialize process and process instance &
         &and generate event"
    write (u, "(A)")

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model)
    call process_instance%setup_event_data ()

    call process%prepare_simulation (1)
    call process_instance%init_simulation (1)
    call process%generate_weighted_event (process_instance, 1)
    call process_instance%evaluate_event_data ()

    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)

    call process_instance%final_simulation (1)
    call process_instance%final ()
    deallocate (process_instance)
    
    write (u, "(A)")
    write (u, "(A)")  "* Recover kinematics and recalculate"
    write (u, "(A)")

    call reset_interaction_counter (2)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()

    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)
    call process_instance%recover &
         (channel = 1, i_term = 1, update_sqme = .true.)

    call process%recover_event (process_instance, 1)
    call process_instance%evaluate_event_data ()

    call process_instance%write (u)
    
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_12"
    
  end subroutine processes_12
  
  subroutine processes_13 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_data_t), target :: model
    type(process_term_t) :: term
    class(prc_core_t), allocatable :: core
    
    write (u, "(A)")  "* Test output: processes_13"
    write (u, "(A)")  "*   Purpose: initialized a colored interaction"
    write (u, "(A)")

    write (u, "(A)")  "* Set up a process constants block"
    write (u, "(A)")

    call os_data_init (os_data)
    call model%init_sm_test ()
!    call model_list%read_model (var_str ("QCD"), var_str ("QCD.mdl"), &
!         os_data, model)
    allocate (test_t :: core)

    associate (data => term%data)
      data%n_in = 2
      data%n_out = 3
      data%n_flv = 2
      data%n_hel = 2
      data%n_col = 2
      data%n_cin = 2

      allocate (data%flv_state (5, 2))
      data%flv_state (:,1) = [ 1, 21, 1, 21, 21]
      data%flv_state (:,2) = [ 2, 21, 2, 21, 21]

      allocate (data%hel_state (5, 2))
      data%hel_state (:,1) = [1, 1, 1, 1, 0]
      data%hel_state (:,2) = [1,-1, 1,-1, 0]

      allocate (data%col_state (2, 5, 2))
      data%col_state (:,:,1) = &
           reshape ([[1, 0], [2,-1], [3, 0], [2,-3], [0,0]], [2,5])
      data%col_state (:,:,2) = &
           reshape ([[1, 0], [2,-3], [3, 0], [2,-1], [0,0]], [2,5])

      allocate (data%ghost_flag (5, 2))
      data%ghost_flag(1:4,:) = .false.
      data%ghost_flag(5,:) = .true.
     
    end associate
    
    write (u, "(A)")  "* Set up the interaction"
    write (u, "(A)")
    
    call reset_interaction_counter ()
    call term%setup_interaction (core, model)
    call term%int%basic_write (u)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_13"
  end subroutine processes_13
    
  subroutine processes_14 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_channel_t), dimension(3) :: sf_channel

    write (u, "(A)")  "* Test output: processes_14"
    write (u, "(A)")  "*   Purpose: initialize a process with &
         &structure functions"
    write (u, "(A)")  "*            and compute MD5 sum"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes7"
    procname = libname
    run_id = "run7"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)
    call lib%compute_md5sum ()
    
    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Set beam, structure functions, and mappings"
    write (u, "(A)")

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    
    pdg_in = 25
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       model => process%get_model_ptr ()
       call data%init (model, pdg_in)
    end select

    call process%beam_config%allocate_sf_channels (3)

    allocate (sf_config (2))
    call sf_config(1)%init ([1], data)
    call sf_config(2)%init ([2], data)
    call process%init_sf_chain (sf_config)
    deallocate (sf_config)

    call sf_channel(1)%init (2)
    call process%set_sf_channel (1, sf_channel(1))

    call sf_channel(2)%init (2)
    call sf_channel(2)%activate_mapping ([1,2])
    call process%set_sf_channel (2, sf_channel(2))
    
    call sf_channel(3)%init (2)
    call sf_channel(3)%set_s_mapping ([1,2])
    call process%set_sf_channel (3, sf_channel(3))
    
    call process%setup_mci ()

    call process%compute_md5sum ()

    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call process%final ()
    deallocate (process)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_14"
    
  end subroutine processes_14
  
  subroutine processes_15 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: pset

    write (u, "(A)")  "* Test output: processes_15"
    write (u, "(A)")  "*   Purpose: initialize a decay process object"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a test library with one process"
    write (u, "(A)")

    libname = "processes15"
    procname = libname
    run_id = "run15"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib, scattering = .false., &
         decay = .true.)

    allocate (model)
    call model%init_test ()
    call model%set_par (var_str ("ff"), 0.4_default)
    call model%set_par (var_str ("mf"), &
         model%get_real (var_str ("ff")) * model%get_real (var_str ("ms")))

    write (u, "(A)")  "* Initialize a process object"
    write (u, "(A)")

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_single_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")
    
    call process%setup_beams_decay ()
    call process%configure_phs ()
    call process%setup_mci ()

    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    call reset_interaction_counter (3)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Inject a set of random numbers"
    write (u, "(A)")
     
    call process_instance%choose_mci (1)
    call process_instance%set_mcpar ([0._default, 0._default])
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Set up hard kinematics"
    write (u, "(A)")
  
    call process_instance%select_channel (1)
    call process_instance%compute_seed_kinematics ()
    call process_instance%compute_hard_kinematics ()

    write (u, "(A)")  "* Evaluate matrix element and square"
    write (u, "(A)")
  
    call process_instance%compute_eff_kinematics ()
    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels ()
    call process_instance%evaluate_trace ()
    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)
    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")
    
    call write_separator (u)
    call pset%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover process instance"
    write (u, "(A)")

    call reset_interaction_counter (3)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)
    call process_instance%recover (1, 1, .true.)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call pset%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_15"
    
  end subroutine processes_15
  
  subroutine processes_16 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: processes_16"
    write (u, "(A)")  "*   Purpose: integrate a process without &
         &structure functions"
    write (u, "(A)")  "*            in a multi-channel configuration"
    write (u, "(A)")

    write (u, "(A)")  "* Build and initialize a process object"
    write (u, "(A)")

    libname = "processes16"
    procname = libname
    run_id = "run16"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib, scattering = .false., &
         decay = .true.)

    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()
    call model%set_par (var_str ("ff"), 0.4_default)
    call model%set_par (var_str ("mf"), &
         model%get_real (var_str ("ff")) * model%get_real (var_str ("ms")))

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")

    call process%setup_beams_decay ()
    call process%configure_phs ()

    call process%setup_mci ()
    
    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")  "* Integrate with default test parameters"
    write (u, "(A)")

    call process%integrate (process_instance, 1, n_it=1, n_calls=10000)
    call process%final_integration (1)
    
    call process%write (.false., u)

    write (u, "(A)")
    write (u, "(A,ES13.7)")  " Integral divided by phs factor = ", &
         process%get_integral (1) &
         / process_instance%component(1)%k_seed%phs_factor

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_16"
    
  end subroutine processes_16
  
  subroutine processes_17 (u)
    integer, intent(in) :: u
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    type(process_instance_t), allocatable, target :: process_instance
    type(particle_set_t) :: pset
    type(flavor_t) :: flv_beam
    real(default) :: m, p, E

    write (u, "(A)")  "* Test output: processes_17"
    write (u, "(A)")  "*   Purpose: initialize a decay process object"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a test library with one process"
    write (u, "(A)")

    libname = "processes17"
    procname = libname
    run_id = "run17"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)

    call prc_test_create_library (libname, lib, scattering = .false., &
         decay = .true.)

    write (u, "(A)")  "* Initialize a process object"
    write (u, "(A)")

    allocate (model)
    call model%init_test ()
    call model%set_par (var_str ("ff"), 0.4_default)
    call model%set_par (var_str ("mf"), &
         model%get_real (var_str ("ff")) * model%get_real (var_str ("ms")))

    allocate (process)
    call process%init (procname, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (phs_single_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")
    
    call process%setup_beams_decay (rest_frame = .false.)
    call process%configure_phs ()
    call process%setup_mci ()

    write (u, "(A)")  "* Complete process initialization"
    write (u, "(A)")

    call process%setup_terms ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    call reset_interaction_counter (3)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Set parent momentum and random numbers"
    write (u, "(A)")
     
    call process_instance%choose_mci (1)
    call process_instance%set_mcpar ([0._default, 0._default])

    model => process%get_model_ptr ()
    call flv_beam%init (25, model)
    m = flv_beam%get_mass ()
    p = 3 * m / 4
    E = sqrt (m**2 + p**2)
    call process_instance%set_beam_momenta ([vector4_moving (E, p, 3)])
     
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Set up hard kinematics"
    write (u, "(A)")
  
    call process_instance%select_channel (1)
    call process_instance%compute_seed_kinematics ()
    call process_instance%compute_hard_kinematics ()

    write (u, "(A)")  "* Evaluate matrix element and square"
    write (u, "(A)")
  
    call process_instance%compute_eff_kinematics ()
    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels ()
    call process_instance%evaluate_trace ()
    call process_instance%write (u)

    call process_instance%get_trace (pset, 1)
    call process_instance%final ()
    deallocate (process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")
    
    call write_separator (u)
    call pset%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover process instance"
    write (u, "(A)")

    call reset_interaction_counter (3)

    allocate (process_instance)
    call process_instance%init (process)

    call process_instance%choose_mci (1)
    call process_instance%set_trace (pset, 1, check_match = .false.)
    call process_instance%recover (1, 1, .true.)
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call pset%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_17"
    
  end subroutine processes_17
  

end module processes
