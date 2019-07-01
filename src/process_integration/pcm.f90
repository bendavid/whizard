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

module pcm

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants, only: zero, two
  use diagnostics
  use lorentz
  use io_units, only: free_unit
  use os_interface
  use process_constants, only: process_constants_t
  use physics_defs
  use model_data, only: model_data_t
  use models, only: model_t
  use interactions, only: interaction_t
  use quantum_numbers, only: quantum_numbers_t, quantum_numbers_mask_t
  use flavors, only: flavor_t
  use variables, only: var_list_t
  use nlo_data, only: nlo_settings_t
  use mci_base, only: mci_t
  use phs_base, only: phs_config_t
  use mappings, only: mapping_defaults_t
  use phs_forests, only: phs_parameters_t
  use phs_fks, only: isr_kinematics_t, real_kinematics_t
  use phs_fks, only: phs_identifier_t
  use dispatch_fks, only: dispatch_fks_s
  use fks_regions, only: region_data_t
  use nlo_data, only: fks_template_t
  use phs_fks, only: phs_fks_generator_t
  use phs_fks, only: dalitz_plot_t
  use phs_fks, only: phs_fks_config_t, get_filtered_resonance_histories
  use dispatch_phase_space, only: dispatch_phs
  use process_libraries, only: process_component_def_t
  use real_subtraction, only: real_subtraction_t, soft_mismatch_t
  use real_subtraction, only: FIXED_ORDER_EVENTS, POWHEG
  use real_subtraction, only: real_partition_t, powheg_damping_simple_t
  use real_subtraction, only: real_partition_fixed_order_t
  use virtual, only: virtual_t
  use dglap_remnant, only: dglap_remnant_t
  use prc_threshold, only: threshold_def_t
  use resonances, only: resonance_history_t, resonance_history_set_t
  use nlo_data, only: FKS_DEFAULT, FKS_RESONANCES
  use blha_config, only: blha_master_t
  use blha_olp_interfaces, only: prc_blha_t

  use pcm_base
  use process_config
  use process_mci, only: process_mci_entry_t
  use process_mci, only: REAL_SINGULAR, REAL_FINITE

  implicit none
  private

  public :: pcm_default_t
  public :: pcm_nlo_t
  public :: pcm_instance_nlo_t

  type, extends (pcm_t) :: pcm_default_t
   contains
     procedure :: allocate_instance => pcm_default_allocate_instance
     procedure :: final => pcm_default_final
     procedure :: is_nlo => pcm_default_is_nlo
     procedure :: init => pcm_default_init
     procedure :: categorize_components => pcm_default_categorize_components
     procedure :: init_phs_config => pcm_default_init_phs_config
     procedure :: allocate_cores => pcm_default_allocate_cores
     procedure :: prepare_any_external_code => &
          pcm_default_prepare_any_external_code
     procedure :: setup_blha => pcm_default_setup_blha
     procedure :: prepare_blha_core => pcm_default_prepare_blha_core
     procedure :: set_blha_methods => pcm_default_set_blha_methods
     procedure :: get_blha_flv_states => pcm_default_get_blha_flv_states
     procedure :: setup_mci => pcm_default_setup_mci
     procedure :: call_dispatch_mci => pcm_default_call_dispatch_mci
     procedure :: complete_setup => pcm_default_complete_setup
     procedure :: init_component => pcm_default_init_component
  end type pcm_default_t

  type, extends (pcm_instance_t) :: pcm_instance_default_t
  contains
    procedure :: final => pcm_instance_default_final
  end type pcm_instance_default_t

  type, extends (pcm_t) :: pcm_nlo_t
     type(string_t) :: id
     logical :: combined_integration = .false.
     logical :: vis_fks_regions = .false.
     integer, dimension(:), allocatable :: nlo_type
     integer, dimension(:), allocatable :: nlo_type_core
     integer, dimension(:), allocatable :: component_type
     integer :: i_born = 0
     integer :: i_real = 0
     integer :: i_sub = 0
     type(nlo_settings_t) :: settings
     type(region_data_t) :: region_data
     logical :: use_real_partition = .false.
     real(default) :: real_partition_scale = 0
     class(real_partition_t), allocatable :: real_partition
     type(dalitz_plot_t) :: dalitz_plot
     type(quantum_numbers_t), dimension(:,:), allocatable :: qn_real, qn_born
  contains
    procedure :: init => pcm_nlo_init
    procedure :: init_nlo_settings => pcm_nlo_init_nlo_settings
    procedure :: categorize_components => pcm_nlo_categorize_components
    procedure :: init_phs_config => pcm_nlo_init_phs_config
    procedure :: allocate_cores => pcm_nlo_allocate_cores
    procedure :: prepare_any_external_code => &
         pcm_nlo_prepare_any_external_code
    procedure :: setup_blha => pcm_nlo_setup_blha
    procedure :: complete_setup => pcm_nlo_complete_setup
    procedure :: prepare_blha_core => pcm_nlo_prepare_blha_core
    procedure :: set_blha_methods => pcm_nlo_set_blha_methods
    procedure :: get_blha_flv_states => pcm_nlo_get_blha_flv_states
    procedure :: setup_mci => pcm_nlo_setup_mci
    procedure :: call_dispatch_mci => pcm_nlo_call_dispatch_mci
    procedure :: handle_threshold_core => pcm_nlo_handle_threshold_core
    procedure :: setup_region_data => pcm_nlo_setup_region_data
    procedure :: setup_real_partition => pcm_nlo_setup_real_partition
    procedure :: init_component => pcm_nlo_init_component
    procedure :: record_inactive_components => pcm_nlo_record_inactive_components
    procedure :: core_is_radiation => pcm_nlo_core_is_radiation
    procedure :: get_n_flv_born => pcm_nlo_get_n_flv_born
    procedure :: get_n_flv_real => pcm_nlo_get_n_flv_real
    procedure :: get_n_alr => pcm_nlo_get_n_alr
    procedure :: get_flv_states => pcm_nlo_get_flv_states
    procedure :: get_qn => pcm_nlo_get_qn
    procedure :: has_massive_emitter => pcm_nlo_has_massive_emitter
    procedure :: get_mass_info => pcm_nlo_get_mass_info
    procedure :: allocate_instance => pcm_nlo_allocate_instance
    procedure :: init_qn => pcm_nlo_init_qn
    procedure :: allocate_ps_matching => pcm_nlo_allocate_ps_matching
    procedure :: activate_dalitz_plot => pcm_nlo_activate_dalitz_plot
    procedure :: register_dalitz_plot => pcm_nlo_register_dalitz_plot
    procedure :: setup_phs_generator => pcm_nlo_setup_phs_generator
    procedure :: final => pcm_nlo_final
    procedure :: is_nlo => pcm_nlo_is_nlo
  end type pcm_nlo_t

  type, extends (pcm_instance_t) :: pcm_instance_nlo_t
     logical :: use_internal_color_correlation = .true.
     type(real_kinematics_t), pointer :: real_kinematics => null ()
     type(isr_kinematics_t), pointer :: isr_kinematics => null ()
     type(real_subtraction_t) :: real_sub
     type(virtual_t) :: virtual
     type(soft_mismatch_t) :: soft_mismatch
     type(dglap_remnant_t) :: dglap_remnant
     integer, dimension(:), allocatable :: i_mci_to_real_component
  contains
    procedure :: set_radiation_event => pcm_instance_nlo_set_radiation_event
    procedure :: set_subtraction_event => pcm_instance_nlo_set_subtraction_event
    procedure :: disable_subtraction => pcm_instance_nlo_disable_subtraction
    procedure :: init_config => pcm_instance_nlo_init_config
    procedure :: setup_real_component => pcm_instance_nlo_setup_real_component
    procedure :: init_real_and_isr_kinematics => &
         pcm_instance_nlo_init_real_and_isr_kinematics
    procedure :: set_real_and_isr_kinematics => &
        pcm_instance_nlo_set_real_and_isr_kinematics
    procedure :: init_real_subtraction => pcm_instance_nlo_init_real_subtraction
    procedure :: set_momenta_and_scales_virtual => &
       pcm_instance_nlo_set_momenta_and_scales_virtual
    procedure :: set_fac_scale => pcm_instance_nlo_set_fac_scale
    procedure :: set_momenta => pcm_instance_nlo_set_momenta
    procedure :: get_momenta => pcm_instance_nlo_get_momenta
    procedure :: get_xi_max => pcm_instance_nlo_get_xi_max
    procedure :: get_n_born => pcm_instance_nlo_get_n_born
    procedure :: get_n_real => pcm_instance_nlo_get_n_real
    procedure :: get_n_regions => pcm_instance_nlo_get_n_regions
    procedure :: set_x_rad => pcm_instance_nlo_set_x_rad
    procedure :: init_virtual => pcm_instance_nlo_init_virtual
    procedure :: disable_virtual_subtraction => pcm_instance_nlo_disable_virtual_subtraction
    procedure :: compute_sqme_virt => pcm_instance_nlo_compute_sqme_virt
    procedure :: compute_sqme_mismatch => pcm_instance_nlo_compute_sqme_mismatch
    procedure :: compute_sqme_dglap_remnant => pcm_instance_nlo_compute_sqme_dglap_remnant
    procedure :: set_fixed_order_event_mode => pcm_instance_nlo_set_fixed_order_event_mode
    procedure :: set_powheg_mode => pcm_instance_nlo_set_powheg_mode
    procedure :: init_soft_mismatch => pcm_instance_nlo_init_soft_mismatch
    procedure :: init_dglap_remnant => pcm_instance_nlo_init_dglap_remnant
    procedure :: is_fixed_order_nlo_events &
         => pcm_instance_nlo_is_fixed_order_nlo_events
    procedure :: final => pcm_instance_nlo_final
  end type pcm_instance_nlo_t


contains

  subroutine pcm_default_allocate_instance (pcm, instance)
    class(pcm_default_t), intent(in) :: pcm
    class(pcm_instance_t), intent(inout), allocatable :: instance
    allocate (pcm_instance_default_t :: instance)
  end subroutine pcm_default_allocate_instance

  subroutine pcm_default_final (pcm)
    class(pcm_default_t), intent(inout) :: pcm
  end subroutine pcm_default_final

  function pcm_default_is_nlo (pcm) result (is_nlo)
    logical :: is_nlo
    class(pcm_default_t), intent(in) :: pcm
    is_nlo = .false.
  end function pcm_default_is_nlo

  subroutine pcm_default_init (pcm, env, meta)
    class(pcm_default_t), intent(out) :: pcm
    type(process_environment_t), intent(in) :: env
    type(process_metadata_t), intent(in) :: meta
    pcm%has_pdfs = env%has_pdfs ()
    call pcm%set_blha_defaults &
         (env%has_polarized_beams (), env%get_var_list_ptr ())
    pcm%os_data = env%get_os_data ()
  end subroutine pcm_default_init
  
  subroutine pcm_instance_default_final (pcm_instance)
    class(pcm_instance_default_t), intent(inout) :: pcm_instance
  end subroutine pcm_instance_default_final

  subroutine pcm_default_categorize_components (pcm, config)
    class(pcm_default_t), intent(inout) :: pcm
    type(process_config_data_t), intent(in) :: config
  end subroutine pcm_default_categorize_components

  subroutine pcm_default_init_phs_config &
       (pcm, phs_entry, meta, env, phs_par, mapping_defs)
    class(pcm_default_t), intent(inout) :: pcm
    type(process_phs_config_t), &
         dimension(:), allocatable, intent(out) :: phs_entry
    type(process_metadata_t), intent(in) :: meta
    type(process_environment_t), intent(in) :: env
    type(mapping_defaults_t), intent(in) :: mapping_defs
    type(phs_parameters_t), intent(in) :: phs_par
    allocate (phs_entry (1))
    allocate (pcm%i_phs_config (pcm%n_components), source=1)
    call dispatch_phs (phs_entry(1)%phs_config, &
         env%get_var_list_ptr (), &
         env%get_os_data (), &
         meta%id, &
         mapping_defs, phs_par)
  end subroutine pcm_default_init_phs_config
  
  subroutine pcm_default_allocate_cores (pcm, config, core_entry)
    class(pcm_default_t), intent(inout) :: pcm
    type(process_config_data_t), intent(in) :: config
    type(core_entry_t), dimension(:), allocatable, intent(out) :: core_entry
    type(process_component_def_t), pointer :: component_def
    integer :: i
    allocate (pcm%i_core (pcm%n_components), source = 0)
    pcm%n_cores = pcm%n_components
    allocate (core_entry (pcm%n_cores))
    do i = 1, pcm%n_cores
       pcm%i_core(i) = i
       core_entry(i)%i_component = i
       component_def => config%process_def%get_component_def_ptr (i)
       core_entry(i)%core_def => component_def%get_core_def_ptr ()
       core_entry(i)%active = component_def%can_be_integrated ()
    end do
  end subroutine pcm_default_allocate_cores
  
  subroutine pcm_default_prepare_any_external_code &
       (pcm, core_entry, i_core, libname, model, var_list)
    class(pcm_default_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    integer, intent(in) :: i_core
    type(string_t), intent(in) :: libname
    type(model_data_t), intent(in), target :: model
    type(var_list_t), intent(in) :: var_list
    if (core_entry%active) then
       associate (core => core_entry%core)
         if (core%needs_external_code ()) then
            call core%prepare_external_code &
                 (core%data%flv_state, &
                 var_list, pcm%os_data, libname, model, i_core, .false.)
         end if
       end associate
    end if
  end subroutine pcm_default_prepare_any_external_code
  
  subroutine pcm_default_setup_blha (pcm, core_entry)
    class(pcm_default_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    allocate (core_entry%blha_config, source = pcm%blha_defaults)
    call core_entry%blha_config%set_born ()
  end subroutine pcm_default_setup_blha
    
  subroutine pcm_default_prepare_blha_core (pcm, core_entry, model)
    class(pcm_default_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    class(model_data_t), intent(in), target :: model
    integer :: n_in
    integer :: n_legs
    integer :: n_flv
    integer :: n_hel
    select type (core => core_entry%core)
    class is (prc_blha_t)
       associate (blha_config => core_entry%blha_config)
         n_in = core%data%n_in
         n_legs = core%data%get_n_tot ()
         n_flv = core%data%n_flv
         n_hel = blha_config%get_n_hel (core%data%flv_state (1:n_in,1), model)
         call core%init_blha (blha_config, n_in, n_legs, n_flv, n_hel)
         call core%init_driver (pcm%os_data)
       end associate
    end select
  end subroutine pcm_default_prepare_blha_core
    
  subroutine pcm_default_set_blha_methods (pcm, blha_master, var_list)
    class(pcm_default_t), intent(in) :: pcm
    type(blha_master_t), intent(inout) :: blha_master
    type(var_list_t), intent(in) :: var_list
    call blha_master%set_methods (.false., var_list)
  end subroutine pcm_default_set_blha_methods
 
  subroutine pcm_default_get_blha_flv_states &
       (pcm, core_entry, flv_born, flv_real)
    class(pcm_default_t), intent(in) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    integer, dimension(:,:), allocatable, intent(out) :: flv_born
    integer, dimension(:,:), allocatable, intent(out) :: flv_real
    flv_born = core_entry(1)%core%data%flv_state
  end subroutine pcm_default_get_blha_flv_states
       
  subroutine pcm_default_setup_mci (pcm, mci_entry)
    class(pcm_default_t), intent(inout) :: pcm
    type(process_mci_entry_t), &
         dimension(:), allocatable, intent(out) :: mci_entry
    class(mci_t), allocatable :: mci_template
    integer :: i, i_mci
    pcm%n_mci = count (pcm%component_active)
    allocate (pcm%i_mci (pcm%n_components), source = 0)
    i_mci = 0
    do i = 1, pcm%n_components
       if (pcm%component_active(i)) then
          i_mci = i_mci + 1
          pcm%i_mci(i) = i_mci
       end if
    end do
    allocate (mci_entry (pcm%n_mci))
  end subroutine pcm_default_setup_mci
       
  subroutine pcm_default_call_dispatch_mci (pcm, &
          dispatch_mci, var_list, process_id, mci_template)
    class(pcm_default_t), intent(inout) :: pcm
    procedure(dispatch_mci_proc) :: dispatch_mci
    type(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: process_id
    class(mci_t), allocatable, intent(out) :: mci_template
    call dispatch_mci (mci_template, var_list, process_id)
  end subroutine pcm_default_call_dispatch_mci
       
  subroutine pcm_default_complete_setup (pcm, core_entry, component, model)
    class(pcm_default_t), intent(inout) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    type(process_component_t), dimension(:), intent(inout) :: component
    type(model_t), intent(in), target :: model
  end subroutine pcm_default_complete_setup
  
  subroutine pcm_default_init_component &
          (pcm, component, i, active, &
          phs_config, env, meta, config)
    class(pcm_default_t), intent(in) :: pcm
    type(process_component_t), intent(out) :: component
    integer, intent(in) :: i
    logical, intent(in) :: active
    class(phs_config_t), allocatable, intent(in) :: phs_config
    type(process_environment_t), intent(in) :: env
    type(process_metadata_t), intent(in) :: meta
    type(process_config_data_t), intent(in) :: config
    call component%init (i, &
         env, meta, config, &
         active, &
         phs_config)
    component%component_type = COMP_MASTER
  end subroutine pcm_default_init_component
       
  subroutine pcm_nlo_init (pcm, env, meta)
    class(pcm_nlo_t), intent(out) :: pcm
    type(process_metadata_t), intent(in) :: meta
    type(process_environment_t), intent(in) :: env
    type(var_list_t), pointer :: var_list
    type(fks_template_t) :: fks_template
    pcm%id = meta%id
    pcm%has_pdfs = env%has_pdfs ()
    var_list => env%get_var_list_ptr ()
    call dispatch_fks_s (fks_template, var_list)
    call pcm%settings%init (var_list, fks_template)
    pcm%combined_integration = &
         var_list%get_lval (var_str ('?combined_nlo_integration'))
    pcm%use_real_partition = &
         var_list%get_lval (var_str ("?nlo_use_real_partition"))
    pcm%real_partition_scale = &
         var_list%get_rval (var_str ("real_partition_scale"))
    pcm%vis_fks_regions = &
         var_list%get_lval (var_str ("?vis_fks_regions"))
    call pcm%set_blha_defaults &
         (env%has_polarized_beams (), env%get_var_list_ptr ())
    pcm%os_data = env%get_os_data ()
  end subroutine pcm_nlo_init
  
  subroutine pcm_nlo_init_nlo_settings (pcm, var_list)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(var_list_t), intent(in), target :: var_list
    call pcm%settings%init (var_list)
  end subroutine pcm_nlo_init_nlo_settings
    
  subroutine pcm_nlo_categorize_components (pcm, config)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_config_data_t), intent(in) :: config
    type(process_component_def_t), pointer :: component_def
    integer :: i
    allocate (pcm%nlo_type (pcm%n_components), source = COMPONENT_UNDEFINED)
    allocate (pcm%component_type (pcm%n_components), source = COMP_DEFAULT)
    do i = 1, pcm%n_components
       component_def => config%process_def%get_component_def_ptr (i)
       pcm%nlo_type(i) = component_def%get_nlo_type ()
       if (pcm%combined_integration) then
          select case (pcm%nlo_type(i))
          case (BORN)
             pcm%i_born = i
             pcm%component_type(i) = COMP_MASTER
          case (NLO_REAL)
             pcm%component_type(i) = COMP_REAL
          case (NLO_VIRTUAL)
             pcm%component_type(i) = COMP_VIRT
          case (NLO_MISMATCH)
             pcm%component_type(i) = COMP_MISMATCH
          case (NLO_DGLAP)
             pcm%component_type(i) = COMP_PDF
          case (NLO_SUBTRACTION)
             pcm%component_type(i) = COMP_SUB
             pcm%i_sub = i
          end select
       else
          select case (pcm%nlo_type(i))
          case (BORN)
             pcm%i_born = i
             pcm%component_type(i) = COMP_MASTER
          case (NLO_REAL)
             pcm%component_type(i) = COMP_REAL
          case (NLO_VIRTUAL)
             pcm%component_type(i) = COMP_VIRT
          case (NLO_MISMATCH)
             pcm%component_type(i) = COMP_MISMATCH
          case (NLO_SUBTRACTION)
             pcm%i_sub = i
          end select
       end if
    end do
    call refine_real_type ( &
         pack ([(i, i=1, pcm%n_components)], &
         pcm%component_type==COMP_REAL))
  contains
    subroutine refine_real_type (i_real)
      integer, dimension(:), intent(in) :: i_real
      pcm%i_real = i_real(1)
      if (pcm%use_real_partition) then
         pcm%component_type (i_real(1)) = COMP_REAL_SING
         pcm%component_type (i_real(2:)) = COMP_REAL_FIN
      end if
    end subroutine refine_real_type
  end subroutine pcm_nlo_categorize_components

  subroutine pcm_nlo_init_phs_config &
       (pcm, phs_entry, meta, env, phs_par, mapping_defs)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_phs_config_t), &
         dimension(:), allocatable, intent(out) :: phs_entry
    type(process_metadata_t), intent(in) :: meta
    type(process_environment_t), intent(in) :: env
    type(mapping_defaults_t), intent(in) :: mapping_defs
    type(phs_parameters_t), intent(in) :: phs_par
    integer :: i
    logical :: first_real_component
    allocate (phs_entry (2))
    call dispatch_phs (phs_entry(1)%phs_config, &
         env%get_var_list_ptr (), &
         env%get_os_data (), &
         meta%id, &
         mapping_defs, phs_par, &
         var_str ("wood"))
    call dispatch_phs (phs_entry(2)%phs_config, &
         env%get_var_list_ptr (), &
         env%get_os_data (), &
         meta%id, &
         mapping_defs, phs_par, &
         var_str ("fks"))
    allocate (pcm%i_phs_config (pcm%n_components), source=0)
    first_real_component = .true.
    do i = 1, pcm%n_components
       select case (pcm%nlo_type(i))
       case (BORN, NLO_VIRTUAL, NLO_SUBTRACTION)
          pcm%i_phs_config(i) = 1
       case (NLO_REAL)
          if (first_real_component) then
             pcm%i_phs_config(i) = 2
             if (pcm%use_real_partition)  first_real_component = .false.
          else
             pcm%i_phs_config(i) = 1
          end if
       case (NLO_MISMATCH, NLO_DGLAP, GKS)
          pcm%i_phs_config(i) = 2
       end select
    end do
  end subroutine pcm_nlo_init_phs_config
  
  subroutine pcm_nlo_allocate_cores (pcm, config, core_entry)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_config_data_t), intent(in) :: config
    type(core_entry_t), dimension(:), allocatable, intent(out) :: core_entry
        type(process_component_def_t), pointer :: component_def
    integer :: i, i_core
    allocate (pcm%i_core (pcm%n_components), source = 0)
    pcm%n_cores = pcm%n_components &
         - count (pcm%component_type(:) == COMP_REAL_FIN) &
         - count (pcm%component_type(:) == COMP_MISMATCH)
    allocate (core_entry (pcm%n_cores))
    allocate (pcm%nlo_type_core (pcm%n_cores), source = BORN)
    i_core = 0
    do i = 1, pcm%n_components
       select case (pcm%component_type(i))
       case default
          i_core = i_core + 1
          pcm%i_core(i) = i_core
          pcm%nlo_type_core(i_core) = pcm%nlo_type(i)
          core_entry(i_core)%i_component = i
          component_def => config%process_def%get_component_def_ptr (i)
          core_entry(i_core)%core_def => component_def%get_core_def_ptr ()
          select case (pcm%nlo_type(i))
          case default
             core_entry(i)%active = component_def%can_be_integrated ()
          case (NLO_REAL, NLO_SUBTRACTION)
             core_entry(i)%active = .true.
          end select
       case (COMP_REAL_FIN)
          pcm%i_core(i) = pcm%i_core(pcm%i_real)
       case (COMP_MISMATCH)
          pcm%i_core(i) = pcm%i_core(pcm%i_sub)
       end select
    end do
  end subroutine pcm_nlo_allocate_cores
  
  subroutine pcm_nlo_prepare_any_external_code &
       (pcm, core_entry, i_core, libname, model, var_list)
    class(pcm_nlo_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    integer, intent(in) :: i_core
    type(string_t), intent(in) :: libname
    type(model_data_t), intent(in), target :: model
    type(var_list_t), intent(in) :: var_list
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    integer :: i
    call pcm%region_data%get_all_flv_states (flv_born, flv_real)
    if (core_entry%active) then
       associate (core => core_entry%core)
         if (core%needs_external_code ()) then
            select case (pcm%nlo_type (core_entry%i_component))
            case default
               call core%data%set_flv_state (flv_born)
            case (NLO_REAL)
               call core%data%set_flv_state (flv_real)
            end select
            call core%prepare_external_code &
                 (core%data%flv_state, &
                 var_list, pcm%os_data, libname, model, i_core, .true.)
         end if
       end associate
    end if
  end subroutine pcm_nlo_prepare_any_external_code
  
  subroutine pcm_nlo_setup_blha (pcm, core_entry)
    class(pcm_nlo_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    allocate (core_entry%blha_config, source = pcm%blha_defaults)
    select case (pcm%nlo_type(core_entry%i_component))
    case (BORN)
       call core_entry%blha_config%set_born ()
    case (NLO_REAL)
       call core_entry%blha_config%set_real_trees ()
    case (NLO_VIRTUAL)
       call core_entry%blha_config%set_loop ()
    case (NLO_SUBTRACTION)
       call core_entry%blha_config%set_subtraction ()
       call core_entry%blha_config%set_internal_color_correlations ()
    case (NLO_DGLAP)
       call core_entry%blha_config%set_dglap ()
    end select
  end subroutine pcm_nlo_setup_blha

  subroutine pcm_nlo_complete_setup (pcm, core_entry, component, model)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    type(process_component_t), dimension(:), intent(inout) :: component
    type(model_t), intent(in), target :: model
    integer :: i
    call pcm%handle_threshold_core (core_entry)
    call pcm%setup_region_data &
         (core_entry, component(pcm%i_real)%phs_config, model)
    call pcm%setup_real_partition ()
  end subroutine pcm_nlo_complete_setup

  subroutine pcm_nlo_prepare_blha_core (pcm, core_entry, model)
    class(pcm_nlo_t), intent(in) :: pcm
    type(core_entry_t), intent(inout) :: core_entry
    class(model_data_t), intent(in), target :: model
    integer :: n_in
    integer :: n_legs
    integer :: n_flv
    integer :: n_hel
    select type (core => core_entry%core)
    class is (prc_blha_t)
       associate (blha_config => core_entry%blha_config)
         n_in = core%data%n_in
         select case (pcm%nlo_type(core_entry%i_component))
         case (NLO_REAL)
            n_legs = pcm%region_data%get_n_legs_real ()
            n_flv = pcm%region_data%get_n_flv_real ()
         case default
            n_legs = pcm%region_data%get_n_legs_born ()
            n_flv = pcm%region_data%get_n_flv_born ()
         end select
         n_hel = blha_config%get_n_hel (core%data%flv_state (1:n_in,1), model)
         call core%init_blha (blha_config, n_in, n_legs, n_flv, n_hel)
         call core%init_driver (pcm%os_data)
       end associate
    end select
  end subroutine pcm_nlo_prepare_blha_core
    
  subroutine pcm_nlo_set_blha_methods (pcm, blha_master, var_list)
    class(pcm_nlo_t), intent(in) :: pcm
    type(blha_master_t), intent(inout) :: blha_master
    type(var_list_t), intent(in) :: var_list
    call blha_master%set_methods (.true., var_list)
  end subroutine pcm_nlo_set_blha_methods
 
  subroutine pcm_nlo_get_blha_flv_states &
       (pcm, core_entry, flv_born, flv_real)
    class(pcm_nlo_t), intent(in) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    integer, dimension(:,:), allocatable, intent(out) :: flv_born
    integer, dimension(:,:), allocatable, intent(out) :: flv_real
    call pcm%region_data%get_all_flv_states (flv_born, flv_real)
  end subroutine pcm_nlo_get_blha_flv_states
       
  subroutine pcm_nlo_setup_mci (pcm, mci_entry)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_mci_entry_t), &
         dimension(:), allocatable, intent(out) :: mci_entry
    class(mci_t), allocatable :: mci_template
    integer :: i, i_mci
    if (pcm%combined_integration) then
       pcm%n_mci = 1 &
            + count (pcm%component_active(:) &
            &        .and. pcm%component_type(:) == COMP_REAL_FIN)
       allocate (pcm%i_mci (pcm%n_components), source = 0)
       do i = 1, pcm%n_components
          if (pcm%component_active(i)) then
             select case (pcm%component_type(i))
             case (COMP_MASTER)
                pcm%i_mci(i) = 1
             case (COMP_REAL_FIN)
                pcm%i_mci(i) = 2
             end select
          end if
       end do
    else
       pcm%n_mci = count (pcm%component_active(:) &
            &             .and. pcm%nlo_type(:) /= NLO_SUBTRACTION)
       allocate (pcm%i_mci (pcm%n_components), source = 0)
       i_mci = 0
       do i = 1, pcm%n_components
          if (pcm%component_active(i)) then
             select case (pcm%nlo_type(i))
             case default
                i_mci = i_mci + 1
                pcm%i_mci(i) = i_mci
             case (NLO_SUBTRACTION)
             end select
          end if
       end do
    end if
    allocate (mci_entry (pcm%n_mci))
    mci_entry(:)%combined_integration = pcm%combined_integration
    if (pcm%use_real_partition) then
       do i = 1, pcm%n_components
          i_mci = pcm%i_mci(i)
          if (i_mci > 0) then
             select case (pcm%component_type(i))
             case (COMP_REAL_FIN)
                mci_entry(i_mci)%real_partition_type = REAL_FINITE
             case default
                mci_entry(i_mci)%real_partition_type = REAL_SINGULAR
             end select
          end if
       end do
    end if
  end subroutine pcm_nlo_setup_mci
       
  subroutine pcm_nlo_call_dispatch_mci (pcm, &
          dispatch_mci, var_list, process_id, mci_template)
    class(pcm_nlo_t), intent(inout) :: pcm
    procedure(dispatch_mci_proc) :: dispatch_mci
    type(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: process_id
    class(mci_t), allocatable, intent(out) :: mci_template
    call dispatch_mci (mci_template, var_list, process_id, is_nlo = .true.)
  end subroutine pcm_nlo_call_dispatch_mci
       
  subroutine pcm_nlo_handle_threshold_core (pcm, core_entry)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    integer :: i
    do i = 1, size (core_entry)
       select type (core => core_entry(i)%core_def)
       type is (threshold_def_t)
          pcm%settings%factorization_mode = FACTORIZATION_THRESHOLD
          return
       end select
    end do
  end subroutine pcm_nlo_handle_threshold_core
    
  subroutine pcm_nlo_setup_region_data (pcm, core_entry, phs_config, model)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(core_entry_t), dimension(:), intent(in) :: core_entry
    class(phs_config_t), intent(inout) :: phs_config
    type(model_t), intent(in), target :: model
    type(process_constants_t) :: data_born, data_real
    integer, dimension (:,:), allocatable :: flavor_born, flavor_real
    type(resonance_history_t), dimension(:), allocatable :: resonance_histories
    type(var_list_t), pointer :: var_list
    logical :: success
    data_born = core_entry(pcm%i_core(pcm%i_born))%core%data
    data_real = core_entry(pcm%i_core(pcm%i_real))%core%data
    call data_born%get_flv_state (flavor_born)
    call data_real%get_flv_state (flavor_real)
    call pcm%region_data%init &
         (data_born%n_in, model, flavor_born, flavor_real, &
         pcm%settings%nlo_correction_type)
    associate (template => pcm%settings%fks_template)
      if (template%mapping_type == FKS_RESONANCES) then
         select type (phs_config)
         type is (phs_fks_config_t)
            call get_filtered_resonance_histories (phs_config, &
                 data_born%n_in, flavor_born, model, &
                 template%excluded_resonances, &
                 resonance_histories, success)
         end select
         if (.not. success) template%mapping_type = FKS_DEFAULT
      end if
      call pcm%region_data%setup_fks_mappings (template, data_born%n_in)
!!! Check again, mapping_type might have changed
      if (template%mapping_type == FKS_RESONANCES) then
         call pcm%region_data%set_resonance_mappings (resonance_histories)
         call pcm%region_data%init_resonance_information ()
         pcm%settings%use_resonance_mappings = .true.
      end if
    end associate
    if (pcm%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
       call pcm%region_data%set_isr_pseudo_regions ()
       call pcm%region_data%split_up_interference_regions_for_threshold ()
    end if
    call pcm%region_data%compute_number_of_phase_spaces ()
    call pcm%region_data%set_i_phs_to_i_con ()
    call pcm%region_data%write_to_file &
         (pcm%id, pcm%vis_fks_regions, pcm%os_data)
    if (debug_active (D_SUBTRACTION)) &
         call pcm%region_data%check_consistency (.true.)
  end subroutine pcm_nlo_setup_region_data

  subroutine pcm_nlo_setup_real_partition (pcm)
    class(pcm_nlo_t), intent(inout) :: pcm
    if (pcm%use_real_partition) then
       if (.not. allocated (pcm%real_partition)) then
          allocate (real_partition_fixed_order_t :: pcm%real_partition)
          select type (partition => pcm%real_partition)
          type is (real_partition_fixed_order_t)
             call pcm%region_data%get_all_ftuples (partition%fks_pairs)
             partition%scale = pcm%real_partition_scale
          end select
       end if
    end if
  end subroutine pcm_nlo_setup_real_partition

  subroutine pcm_nlo_init_component &
          (pcm, component, i, active, &
          phs_config, env, meta, config)
    class(pcm_nlo_t), intent(in) :: pcm
    type(process_component_t), intent(out) :: component
    integer, intent(in) :: i
    logical, intent(in) :: active
    class(phs_config_t), allocatable, intent(in) :: phs_config
    type(process_environment_t), intent(in) :: env
    type(process_metadata_t), intent(in) :: meta
    type(process_config_data_t), intent(in) :: config
    logical :: activate
    select case (pcm%nlo_type(i))
    case default;            activate = active
    case (NLO_SUBTRACTION);  activate = .false.
    end select
    call component%init (i, &
         env, meta, config, &
         activate, &
         phs_config)
    component%component_type = pcm%component_type(i)
  end subroutine pcm_nlo_init_component
       
  subroutine pcm_nlo_record_inactive_components (pcm, component, meta)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_component_t), dimension(:), intent(in) :: component
    type(process_metadata_t), intent(inout) :: meta
    integer :: i
    pcm%component_active = component%active
    do i = 1, pcm%n_components
       select case (pcm%nlo_type(i))
       case (NLO_SUBTRACTION)
       case default
          if (.not. component(i)%active)  call meta%deactivate_component (i)
       end select
    end do
  end subroutine pcm_nlo_record_inactive_components
  
  function pcm_nlo_core_is_radiation (pcm, i_core) result (is_rad)
    logical :: is_rad
    class(pcm_nlo_t), intent(in) :: pcm
    integer, intent(in) :: i_core
    is_rad = pcm%nlo_type(i_core) == NLO_REAL ! .and. .not. pcm%cm%sub(i_core)
  end function pcm_nlo_core_is_radiation

  function pcm_nlo_get_n_flv_born (pcm_nlo) result (n_flv)
    integer :: n_flv
    class(pcm_nlo_t), intent(in) :: pcm_nlo
    n_flv = pcm_nlo%region_data%n_flv_born
  end function pcm_nlo_get_n_flv_born

  function pcm_nlo_get_n_flv_real (pcm_nlo) result (n_flv)
    integer :: n_flv
    class(pcm_nlo_t), intent(in) :: pcm_nlo
    n_flv = pcm_nlo%region_data%n_flv_real
  end function pcm_nlo_get_n_flv_real

  function pcm_nlo_get_n_alr (pcm) result (n_alr)
    integer :: n_alr
    class(pcm_nlo_t), intent(in) :: pcm
    n_alr = pcm%region_data%n_regions
  end function pcm_nlo_get_n_alr

  function pcm_nlo_get_flv_states (pcm, born) result (flv)
    integer, dimension(:,:), allocatable :: flv
    class(pcm_nlo_t), intent(in) :: pcm
    logical, intent(in) :: born
    if (born) then
       flv = pcm%region_data%get_flv_states_born ()
    else
       flv = pcm%region_data%get_flv_states_real ()
    end if
  end function pcm_nlo_get_flv_states

  function pcm_nlo_get_qn (pcm, born) result (qn)
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    class(pcm_nlo_t), intent(in) :: pcm
    logical, intent(in) :: born
    if (born) then
       qn = pcm%qn_born
    else
       qn = pcm%qn_real
    end if
  end function pcm_nlo_get_qn

  function pcm_nlo_has_massive_emitter (pcm) result (val)
    logical :: val
    class(pcm_nlo_t), intent(in) :: pcm
    integer :: i
    val = .false.
    associate (reg_data => pcm%region_data)
       do i = reg_data%n_in + 1, reg_data%n_legs_born
          if (any (i == reg_data%emitters)) &
             val = val .or. reg_data%flv_born(1)%massive(i)
       end do
    end associate
  end function pcm_nlo_has_massive_emitter

  function pcm_nlo_get_mass_info (pcm, i_flv) result (massive)
    class(pcm_nlo_t), intent(in) :: pcm
    integer, intent(in) :: i_flv
    logical, dimension(:), allocatable :: massive
    allocate (massive (size (pcm%region_data%flv_born(i_flv)%massive)))
    massive = pcm%region_data%flv_born(i_flv)%massive
  end function pcm_nlo_get_mass_info

  subroutine pcm_nlo_allocate_instance (pcm, instance)
    class(pcm_nlo_t), intent(in) :: pcm
    class(pcm_instance_t), intent(inout), allocatable :: instance
    allocate (pcm_instance_nlo_t :: instance)
  end subroutine pcm_nlo_allocate_instance

  subroutine pcm_nlo_init_qn (pcm, model)
    class(pcm_nlo_t), intent(inout) :: pcm
    class(model_data_t), intent(in) :: model
    integer, dimension(:,:), allocatable :: flv_states
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    allocate (flv_states (pcm%region_data%n_legs_born, pcm%region_data%n_flv_born))
    flv_states = pcm%get_flv_states (.true.)
    allocate (pcm%qn_born (size (flv_states, dim = 1), size (flv_states, dim = 2)))
    allocate (flv (size (flv_states, dim = 1)))
    allocate (qn (size (flv_states, dim = 1)))
    do i = 1, pcm%get_n_flv_born ()
       call flv%init (flv_states (:,i), model)
       call qn%init (flv)
       pcm%qn_born(:,i) = qn
    end do
    deallocate (flv); deallocate (qn)
    deallocate (flv_states)
    allocate (flv_states (pcm%region_data%n_legs_real, pcm%region_data%n_flv_real))
    flv_states = pcm%get_flv_states (.false.)
    allocate (pcm%qn_real (size (flv_states, dim = 1), size (flv_states, dim = 2)))
    allocate (flv (size (flv_states, dim = 1)))
    allocate (qn (size (flv_states, dim = 1)))
    do i = 1, pcm%get_n_flv_real ()
       call flv%init (flv_states (:,i), model)
       call qn%init (flv)
       pcm%qn_real(:,i) = qn
    end do
  end subroutine pcm_nlo_init_qn

  subroutine pcm_nlo_allocate_ps_matching (pcm)
    class(pcm_nlo_t), intent(inout) :: pcm
    if (.not. allocated (pcm%real_partition)) then
       allocate (powheg_damping_simple_t :: pcm%real_partition)
    end if
  end subroutine pcm_nlo_allocate_ps_matching

  subroutine pcm_nlo_activate_dalitz_plot (pcm, filename)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(string_t), intent(in) :: filename
    call pcm%dalitz_plot%init (free_unit (), filename, .false.)
    call pcm%dalitz_plot%write_header ()
  end subroutine pcm_nlo_activate_dalitz_plot

  subroutine pcm_nlo_register_dalitz_plot (pcm, emitter, p)
    class(pcm_nlo_t), intent(inout) :: pcm
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: k0_n, k0_np1
    k0_n = p(emitter)%p(0)
    k0_np1 = p(size(p))%p(0)
    call pcm%dalitz_plot%register (k0_n, k0_np1)
  end subroutine pcm_nlo_register_dalitz_plot

  subroutine pcm_nlo_setup_phs_generator (pcm, pcm_instance, generator, &
     sqrts, mode, singular_jacobian)
    class(pcm_nlo_t), intent(in) :: pcm
    type(phs_fks_generator_t), intent(inout) :: generator
    type(pcm_instance_nlo_t), intent(in), target :: pcm_instance
    real(default), intent(in) :: sqrts
    integer, intent(in), optional:: mode
    logical, intent(in), optional :: singular_jacobian
    logical :: yorn
    yorn = .false.; if (present (singular_jacobian)) yorn = singular_jacobian
    call generator%connect_kinematics (pcm_instance%isr_kinematics, &
         pcm_instance%real_kinematics, pcm%has_massive_emitter ())
    generator%n_in = pcm%region_data%n_in
    call generator%set_sqrts_hat (sqrts)
    call generator%set_emitters (pcm%region_data%emitters)
    call generator%setup_masses (pcm%region_data%n_legs_born)
    generator%is_massive = pcm%get_mass_info (1)
    generator%singular_jacobian = yorn
    if (present (mode)) generator%mode = mode
  end subroutine pcm_nlo_setup_phs_generator

  subroutine pcm_nlo_final (pcm)
    class(pcm_nlo_t), intent(inout) :: pcm
    if (allocated (pcm%real_partition)) deallocate (pcm%real_partition)
    call pcm%dalitz_plot%final ()
  end subroutine pcm_nlo_final

  function pcm_nlo_is_nlo (pcm) result (is_nlo)
    logical :: is_nlo
    class(pcm_nlo_t), intent(in) :: pcm
    is_nlo = .true.
  end function pcm_nlo_is_nlo

  subroutine pcm_instance_nlo_set_radiation_event (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%radiation_event = .true.
    pcm_instance%real_sub%subtraction_event = .false.
  end subroutine pcm_instance_nlo_set_radiation_event

  subroutine pcm_instance_nlo_set_subtraction_event (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%radiation_event = .false.
    pcm_instance%real_sub%subtraction_event = .true.
  end subroutine pcm_instance_nlo_set_subtraction_event

  subroutine pcm_instance_nlo_disable_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%subtraction_deactivated = .true.
  end subroutine pcm_instance_nlo_disable_subtraction

  subroutine pcm_instance_nlo_init_config (pcm_instance, active_components, &
     nlo_types, sqrts, i_real_fin, model)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    logical, intent(in), dimension(:) :: active_components
    integer, intent(in), dimension(:) :: nlo_types
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i_real_fin
    class(model_data_t), intent(in) :: model
    integer :: i_component
    call msg_debug (D_PROCESS_INTEGRATION, "pcm_instance_nlo_init_config")
    call pcm_instance%init_real_and_isr_kinematics (sqrts)
    select type (pcm => pcm_instance%config)
    type is (pcm_nlo_t)
       do i_component = 1, size (active_components)
          if (active_components(i_component) .or. pcm%settings%combined_integration) then
             select case (nlo_types(i_component))
             case (NLO_REAL)
                if (i_component /= i_real_fin) then
                   call pcm_instance%setup_real_component &
                        (pcm%settings%fks_template%subtraction_disabled)
                end if
             case (NLO_VIRTUAL)
                call pcm_instance%init_virtual (model)
             case (NLO_MISMATCH)
                call pcm_instance%init_soft_mismatch ()
             case (NLO_DGLAP)
                call pcm_instance%init_dglap_remnant ()
             end select
          end if
       end do
    end select
  end subroutine pcm_instance_nlo_init_config

  subroutine pcm_instance_nlo_setup_real_component (pcm_instance, &
     subtraction_disabled)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    logical, intent(in) :: subtraction_disabled
    call pcm_instance%init_real_subtraction ()
    if (subtraction_disabled)  call pcm_instance%disable_subtraction ()
  end subroutine pcm_instance_nlo_setup_real_component

  subroutine pcm_instance_nlo_init_real_and_isr_kinematics (pcm_instance, sqrts)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default) :: sqrts
    integer :: n_contr
    allocate (pcm_instance%real_kinematics)
    allocate (pcm_instance%isr_kinematics)
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          if (allocated (region_data%alr_contributors)) then
             n_contr = size (region_data%alr_contributors)
          else if (config%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
             n_contr = 2
          else
             n_contr = 1
          end if
          call pcm_instance%real_kinematics%init &
               (region_data%n_legs_real, region_data%n_phs, &
               region_data%n_regions, n_contr)
          if (config%settings%factorization_mode == FACTORIZATION_THRESHOLD) &
             call pcm_instance%real_kinematics%init_onshell &
                  (region_data%n_legs_real, region_data%n_phs)
          pcm_instance%isr_kinematics%n_in = region_data%n_in
       end associate
    end select
    pcm_instance%isr_kinematics%beam_energy = sqrts / two
  end subroutine pcm_instance_nlo_init_real_and_isr_kinematics

  subroutine pcm_instance_nlo_set_real_and_isr_kinematics (pcm_instance, phs_identifiers, sqrts)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers
    real(default), intent(in) :: sqrts
    call pcm_instance%real_sub%set_real_kinematics &
         (pcm_instance%real_kinematics)
    call pcm_instance%real_sub%set_isr_kinematics &
         (pcm_instance%isr_kinematics)
  end subroutine pcm_instance_nlo_set_real_and_isr_kinematics

  subroutine pcm_instance_nlo_init_real_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          call pcm_instance%real_sub%init (region_data, config%settings)
          if (allocated (config%settings%selected_alr)) then
              associate (selected_alr => config%settings%selected_alr)
                if (any (selected_alr < 0)) then
                   call msg_fatal ("Fixed alpha region must be non-negative!")
                else if (any (selected_alr > region_data%n_regions)) then
                   call msg_fatal ("Fixed alpha region is larger than the total"&
                        &" number of singular regions!")
                else
                   allocate (pcm_instance%real_sub%selected_alr (size (selected_alr)))
                   pcm_instance%real_sub%selected_alr = selected_alr
                end if
             end associate
          end if
       end associate
    end select
  end subroutine pcm_instance_nlo_init_real_subtraction

  subroutine pcm_instance_nlo_set_momenta_and_scales_virtual (pcm_instance, p, &
     ren_scale, fac_scale)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale, fac_scale
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (virtual => pcm_instance%virtual)
          call virtual%set_ren_scale (p, ren_scale)
          call virtual%set_fac_scale (p, fac_scale)
          call virtual%set_ellis_sexton_scale ()
       end associate
    end select
  end subroutine pcm_instance_nlo_set_momenta_and_scales_virtual

  subroutine pcm_instance_nlo_set_fac_scale (pcm_instance, fac_scale)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in) :: fac_scale
    pcm_instance%isr_kinematics%fac_scale = fac_scale
  end subroutine pcm_instance_nlo_set_fac_scale

  subroutine pcm_instance_nlo_set_momenta (pcm_instance, p_born, p_real, i_phs, cms)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), dimension(:), intent(in) :: p_born, p_real
    integer, intent(in) :: i_phs
    logical, intent(in), optional :: cms
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    associate (kinematics => pcm_instance%real_kinematics)
       if (yorn) then
          if (.not. kinematics%p_born_cms%initialized) &
               call kinematics%p_born_cms%init (size (p_born), 1)
          if (.not. kinematics%p_real_cms%initialized) &
               call kinematics%p_real_cms%init (size (p_real), 1)
          kinematics%p_born_cms%phs_point(1)%p = p_born
          kinematics%p_real_cms%phs_point(i_phs)%p = p_real
       else
          if (.not. kinematics%p_born_lab%initialized) &
               call kinematics%p_born_lab%init (size (p_born), 1)
          if (.not. kinematics%p_real_lab%initialized) &
               call kinematics%p_real_lab%init (size (p_real), 1)
          kinematics%p_born_lab%phs_point(1)%p = p_born
          kinematics%p_real_lab%phs_point(i_phs)%p = p_real
       end if
    end associate
  end subroutine pcm_instance_nlo_set_momenta

  function pcm_instance_nlo_get_momenta (pcm_instance, i_phs, born_phsp, cms) result (p)
    type(vector4_t), dimension(:), allocatable :: p
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    integer, intent(in) :: i_phs
    logical, intent(in) :: born_phsp
    logical, intent(in), optional :: cms
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       if (born_phsp) then
          if (yorn) then
             allocate (p (1 : config%region_data%n_legs_born), &
                source = pcm_instance%real_kinematics%p_born_cms%phs_point(1)%p)
          else
             allocate (p (1 : config%region_data%n_legs_born), &
                source = pcm_instance%real_kinematics%p_born_lab%phs_point(1)%p)
          end if
       else
          if (yorn) then
             allocate (p (1 : config%region_data%n_legs_real), &
                source = pcm_instance%real_kinematics%p_real_cms%phs_point(i_phs)%p)
          else
             allocate (p ( 1 : config%region_data%n_legs_real), &
                  source = pcm_instance%real_kinematics%p_real_lab%phs_point(i_phs)%p)
          end if
       end if
    end select
  end function pcm_instance_nlo_get_momenta

  function pcm_instance_nlo_get_xi_max (pcm_instance, alr) result (xi_max)
    real(default) :: xi_max
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    integer, intent(in) :: alr
    integer :: i_phs
    i_phs = pcm_instance%real_kinematics%alr_to_i_phs (alr)
    xi_max = pcm_instance%real_kinematics%xi_max (i_phs)
  end function pcm_instance_nlo_get_xi_max

  function pcm_instance_nlo_get_n_born (pcm_instance) result (n_born)
    integer :: n_born
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       n_born = config%region_data%n_legs_born
    end select
  end function pcm_instance_nlo_get_n_born

  function pcm_instance_nlo_get_n_real (pcm_instance) result (n_real)
    integer :: n_real
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       n_real = config%region_data%n_legs_real
    end select
  end function pcm_instance_nlo_get_n_real

  function pcm_instance_nlo_get_n_regions (pcm_instance) result (n_regions)
    integer :: n_regions
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       n_regions = config%region_data%n_regions
    end select
  end function pcm_instance_nlo_get_n_regions

  subroutine pcm_instance_nlo_set_x_rad (pcm_instance, x_tot)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in), dimension(:) :: x_tot
    integer :: n_par
    n_par = size (x_tot)
    if (n_par < 3) then
       pcm_instance%real_kinematics%x_rad = zero
    else
       pcm_instance%real_kinematics%x_rad = x_tot (n_par - 2 : n_par)
    end if
  end subroutine pcm_instance_nlo_set_x_rad

  subroutine pcm_instance_nlo_init_virtual (pcm_instance, model)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    class(model_data_t), intent(in) :: model
    type(nlo_settings_t), pointer :: settings
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
         settings => config%settings
         call pcm_instance%virtual%init (region_data%get_flv_states_born (), &
              region_data%n_in, settings, &
              region_data%regions(1)%nlo_correction_type, model, config%has_pdfs)
       end associate
    end select
  end subroutine pcm_instance_nlo_init_virtual

  subroutine pcm_instance_nlo_disable_virtual_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
  end subroutine pcm_instance_nlo_disable_virtual_subtraction

  subroutine pcm_instance_nlo_compute_sqme_virt (pcm_instance, p, &
         alpha_coupling, separate_alrs, sqme_virt)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: alpha_coupling
    logical, intent(in) :: separate_alrs
    real(default), dimension(:), allocatable, intent(inout) :: sqme_virt
    type(vector4_t), dimension(:), allocatable :: pp
    associate (virtual => pcm_instance%virtual)
       allocate (pp (size (p)))
       if (virtual%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
          pp = pcm_instance%real_kinematics%p_born_onshell%get_momenta (1)
       else
          pp = p
       end if
       select type (config => pcm_instance%config)
       type is (pcm_nlo_t)
          if (separate_alrs) then
             allocate (sqme_virt (config%get_n_flv_born ()))
          else
             allocate (sqme_virt (1))
          end if
          sqme_virt = zero
          call virtual%evaluate (config%region_data, &
               alpha_coupling, pp, separate_alrs, sqme_virt)
       end select
    end associate
  end subroutine pcm_instance_nlo_compute_sqme_virt

  subroutine pcm_instance_nlo_compute_sqme_mismatch (pcm_instance, &
           alpha_s, separate_alrs, sqme_mism)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in) :: alpha_s
    logical, intent(in) :: separate_alrs
    real(default), dimension(:), allocatable, intent(inout) :: sqme_mism
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       if (separate_alrs) then
          allocate (sqme_mism (config%get_n_flv_born ()))
       else
          allocate (sqme_mism (1))
       end if
       sqme_mism = zero
       sqme_mism = pcm_instance%soft_mismatch%evaluate (alpha_s)
    end select
  end subroutine pcm_instance_nlo_compute_sqme_mismatch

  subroutine pcm_instance_nlo_compute_sqme_dglap_remnant (pcm_instance, &
            alpha_s, separate_alrs, sqme_dglap)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in) :: alpha_s
    logical, intent(in) :: separate_alrs
    real(default), dimension(:), allocatable, intent(inout) :: sqme_dglap
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       if (separate_alrs) then
          allocate (sqme_dglap (config%get_n_flv_born ()))
       else
          allocate (sqme_dglap (1))
       end if
    end select
    sqme_dglap = zero
    call pcm_instance%dglap_remnant%evaluate (alpha_s, separate_alrs, sqme_dglap)
  end subroutine pcm_instance_nlo_compute_sqme_dglap_remnant

  subroutine pcm_instance_nlo_set_fixed_order_event_mode (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%purpose = FIXED_ORDER_EVENTS
  end subroutine pcm_instance_nlo_set_fixed_order_event_mode

  subroutine pcm_instance_nlo_set_powheg_mode (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%purpose = POWHEG
  end subroutine pcm_instance_nlo_set_powheg_mode

  subroutine pcm_instance_nlo_init_soft_mismatch (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       call pcm_instance%soft_mismatch%init (config%region_data, &
            pcm_instance%real_kinematics, config%settings%factorization_mode)
    end select
  end subroutine pcm_instance_nlo_init_soft_mismatch

  subroutine pcm_instance_nlo_init_dglap_remnant (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       call pcm_instance%dglap_remnant%init ( &
            config%settings, &
            config%region_data%n_flv_born, &
            pcm_instance%isr_kinematics, &
            config%region_data%get_flv_states_born (), config%get_n_alr ())
    end select
  end subroutine pcm_instance_nlo_init_dglap_remnant

  function pcm_instance_nlo_is_fixed_order_nlo_events (pcm_instance) result (is_nlo)
    logical :: is_nlo
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    is_nlo = pcm_instance%real_sub%purpose == FIXED_ORDER_EVENTS
  end function pcm_instance_nlo_is_fixed_order_nlo_events

  subroutine pcm_instance_nlo_final (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    call pcm_instance%real_sub%final ()
    call pcm_instance%virtual%final ()
    call pcm_instance%soft_mismatch%final ()
    call pcm_instance%dglap_remnant%final ()
    if (associated (pcm_instance%real_kinematics)) then
       call pcm_instance%real_kinematics%final ()
       nullify (pcm_instance%real_kinematics)
    end if
    if (associated (pcm_instance%isr_kinematics)) then
       nullify (pcm_instance%isr_kinematics)
    end if
  end subroutine pcm_instance_nlo_final


end module pcm
