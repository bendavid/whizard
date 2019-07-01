! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module integrations

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use os_interface
  use cputime
  use sm_qcd
  use physics_defs
  use model_data
  use pdg_arrays
  use variables
  use eval_trees
  use sf_mappings
  use sf_base
  use phs_base
  use mappings
  use phs_forests, only: phs_parameters_t
  use rng_base
  use mci_base
  use process_libraries
  use prc_core
  use processes
  use process_stacks
  use models
  use iterations
  use rt_data
  
  use dispatch, only: dispatch_qcd
  use dispatch, only: dispatch_rng_factory
  use dispatch, only: dispatch_core
  use dispatch, only: sf_prop_t
  use dispatch, only: dispatch_sf_channels, dispatch_sf_config
  use dispatch, only: dispatch_phs
  use dispatch, only: dispatch_mci

  use compilations, only: compile_library

  use dispatch, only: dispatch_fks
  use blha_olp_interfaces
  use nlo_data

  implicit none
  private

  public :: integration_t
  public :: integrate_process

  type :: integration_t
    private
    type(string_t) :: process_id
    type(string_t) :: run_id
    type(process_t), pointer :: process => null ()
    type(var_list_t), pointer :: model_vars => null ()
    type(qcd_t) :: qcd
    logical :: rebuild_phs = .false.
    logical :: ignore_phs_mismatch = .false.
    logical :: phs_only = .false.
    logical :: process_has_me = .true.
    integer :: n_calls_test = 0
    logical :: vis_history = .true.
    type(string_t) :: history_filename
    type(string_t) :: log_filename
    logical :: combined_integration = .false.
    type(iteration_multipliers_t) :: iteration_multipliers
   contains
     procedure :: create_process => integration_create_process
     procedure :: init_process => integration_init_process
     procedure :: setup_process => integration_setup_process
     procedure :: evaluate => integration_evaluate
     procedure :: make_iterations_list => integration_make_iterations_list
     procedure :: init_iteration_multipliers => integration_init_iteration_multipliers
     procedure :: apply_call_multipliers => integration_apply_call_multipliers
     procedure :: init => integration_init
     procedure :: integrate => integration_integrate
     procedure :: setup_component_cores => integration_setup_component_cores
     procedure :: setup_process_mci => integration_setup_process_mci
     procedure :: integrate_dummy => integration_integrate_dummy 
     procedure :: sampler_test => integration_sampler_test
     procedure :: get_process_ptr => integration_get_process_ptr 
  end type integration_t


contains

  subroutine integration_create_process (intg, process_id, global)
    class(integration_t), intent(out) :: intg
    type(rt_data_t), intent(inout), optional, target :: global
    type(string_t), intent(in) :: process_id
    type(process_entry_t), pointer :: process_entry
    intg%process_id = process_id
    if (present (global)) then
       allocate (process_entry)
       intg%process => process_entry%process_t
       call global%process_stack%push (process_entry)
    else
       allocate (process_t :: intg%process)
    end if
    intg%model_vars => null ()
  end subroutine integration_create_process

  subroutine integration_init_process (intg, local)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(inout), target :: local
    type(string_t) :: model_name
    type(model_t), pointer :: model
    class(model_data_t), pointer :: model_instance
    class(rng_factory_t), allocatable :: rng_factory
    if (.not. local%prclib%contains (intg%process_id)) then
       call msg_fatal ("Process '" // char (intg%process_id) // "' not found" &
            // " in library '" // char (local%prclib%get_name ()) // "'")
       return
    end if
    intg%run_id = &
         local%var_list%get_sval (var_str ("$run_id"))
    call dispatch_qcd (intg%qcd, local)
    call dispatch_rng_factory (rng_factory, local)
    model_name = local%prclib%get_model_name (intg%process_id)
    if (local%get_sval (var_str ("$model_name")) == model_name) then
       model => local%model
    else
       model => local%model_list%get_model_ptr (model_name)
    end if
    allocate (model_t :: model_instance)
    select type (model_instance)
    type is (model_t)
       call model_instance%init_instance (model)
       intg%model_vars => model_instance%get_var_list_ptr ()
    end select
    call intg%process%init (intg%process_id, intg%run_id, &
         local%prclib, &
         local%os_data, intg%qcd, rng_factory, model_instance)
  end subroutine integration_init_process
    
  subroutine integration_setup_process (intg, local, verbose)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(inout), target :: local
    logical, intent(in), optional :: verbose
    type(var_list_t), pointer :: var_list
    class(prc_core_t), allocatable :: core_template
    class(phs_config_t), allocatable :: phs_config_template
    class(phs_config_t), allocatable :: phs_config_template_other
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defs
    class(mci_t), allocatable :: mci_template
    integer :: n_components, n_in, i_component
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_prc
    type(process_component_def_t), pointer :: config
    type(helicity_selection_t) :: helicity_selection
    real(default) :: sqrts
    logical :: decay_rest_frame, use_color_factors
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_prop_t) :: sf_prop
    type(sf_channel_t), dimension(:), allocatable :: sf_channel
    type(phs_channel_collection_t) :: phs_channel_collection
    logical :: sf_trace
    type(string_t) :: sf_string, sf_trace_file
    logical :: verb
    type(fks_template_t) :: fks_template
    type(blha_template_t) :: blha_template
    type(string_t) :: me_method
    type(eval_tree_factory_t) :: expr_factory
    logical :: use_powheg_damping_factors
    integer :: i = 0
  
    verb = .true.; if (present (verbose))  verb = verbose
    
    call intg%process%set_var_list (local%get_var_list_ptr ())
    var_list => intg%process%get_var_list_ptr ()

    intg%rebuild_phs = &
         var_list%get_lval (var_str ("?rebuild_phase_space"))
    intg%ignore_phs_mismatch = &
         .not. var_list%get_lval (var_str ("?check_phs_file"))
    intg%phs_only = &
         var_list%get_lval (var_str ("?phs_only"))
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

    call dispatch_phs (phs_config_template, local, &
         intg%process_id, mapping_defs, phs_par)
    
    
    intg%n_calls_test = &
         var_list%get_ival (var_str ("n_calls_test"))

    !!! We avoid two dots in the filename due to a bug in certain MetaPost versions.
    if (intg%run_id /= "") then
       intg%history_filename = intg%process_id // "." // intg%run_id &
            // "-history"
       intg%log_filename = intg%process_id // "." // intg%run_id // ".log"
    else
       intg%history_filename = intg%process_id // "-history"
       intg%log_filename = intg%process_id // ".log"
    end if

    call dispatch_mci (mci_template, local, intg%process_id, &
       intg%process%is_nlo_calculation ())

    if (verb) then
       call msg_message ("Initializing integration for process " &
            // char (intg%process_id) // ":")
       if (intg%run_id /= "") then
          call msg_message ("Run ID = " // '"' // char (intg%run_id) // '"')
       end if
    end if
    
    helicity_selection = local%get_helicity_selection ()

    intg%vis_history = &
         var_list%get_lval (var_str ("?vis_history"))
    use_color_factors = var_list%get_lval &
         (var_str ("?read_color_factors"))
    
    n_components = intg%process%get_n_components ()
    n_in = intg%process%get_n_in ()

    call blha_template%init (local%beam_structure%has_polarized_beams())
    intg%combined_integration = var_list%get_lval (&
                                var_str ('?combined_nlo_integration')) &
                                .and. intg%process%is_nlo_calculation ()

    do i_component = 1, n_components
       config => intg%process%get_component_def_ptr (i_component)
       call dispatch_core (core_template, config%get_core_def_ptr (), &
            intg%process%get_model_ptr (), helicity_selection, intg%qcd, &
            use_color_factors)
       select case (config%get_nlo_type ())
       case (NLO_VIRTUAL)
         me_method = var_list%get_sval (var_str ("$loop_me_method"))
         select case (char (me_method)) 
         case ('gosam', 'openloops')
            call blha_template%set_loop ()
         end select
         call intg%process%init_component &
            (i_component, core_template, mci_template, phs_config_template, &
             blha_template = blha_template)
         if (intg%combined_integration) &
            call intg%process%set_component_type (i_component, COMP_VIRT)
       case (NLO_REAL)
         me_method = var_list%get_sval (var_str ("$real_tree_me_method"))
         use_powheg_damping_factors = var_list%get_lval (var_str ("?use_powheg_damping"))
         select case (char (me_method))
         case ('gosam', 'openloops')
            call blha_template%set_real_trees ()
         end select
         call dispatch_phs (phs_config_template_other, local, &
             intg%process_id, mapping_defs, phs_par, &
             var_str ('fks'))
         call dispatch_fks (fks_template, local)
         call intg%process%init_component &
            (i_component, core_template, mci_template, &
             phs_config_template_other, fks_template = fks_template, &
             blha_template = blha_template)
         if (intg%combined_integration) then
            if (use_powheg_damping_factors) then
               if (i == 0) then
                  call intg%process%set_component_type (i_component, COMP_REAL_SING)
                  i = i + 1
               else
                  call intg%process%set_component_type (i_component, COMP_REAL_FIN)
               end if
            else
               call intg%process%set_component_type (i_component, COMP_REAL)
            end if
         end if
       case (NLO_PDF)
         call dispatch_phs (phs_config_template_other, local, &
             intg%process_id, mapping_defs, phs_par, &
             var_str ('fks'))
         call intg%process%init_component &
           (i_component, core_template, mci_template, phs_config_template_other)
         if (intg%combined_integration) &
            call intg%process%set_component_type (i_component, COMP_PDF)
       case (BORN)
         me_method = var_list%get_sval (var_str ("$born_me_method"))
         select case (char (me_method))
         case ('gosam', 'openloops')
            call blha_template%set_born ()
            call intg%process%init_component &
               (i_component, core_template, mci_template, phs_config_template, &
                blha_template = blha_template)
         case default
            call intg%process%init_component &
               (i_component, core_template, mci_template, phs_config_template)
         end select
         if (intg%combined_integration) &
            call intg%process%set_component_type (i_component, COMP_MASTER)
       case (NLO_SUBTRACTION)
         me_method = var_list%get_sval (var_str ("$correlation_me_method"))
         select case (char (me_method))
         case ('gosam', 'openloops')
            call blha_template%set_subtraction ()
         end select
         call intg%process%init_component &
             (i_component, core_template, mci_template, phs_config_template, &
              blha_template = blha_template)
         if (intg%combined_integration) &
            call intg%process%set_component_type (i_component, COMP_SUB)
       case (GKS)
         call intg%process%init_component &
             (i_component, core_template, mci_template, phs_config_template) 
       case (NLO_THRESHOLD_RESUMMATION)
         call intg%process%init_component &
             (i_component, core_template, mci_template, phs_config_template)
         if (intg%combined_integration) &
            call intg%process%set_component_type (i_component, COMP_RESUM)
       case default
         call msg_fatal ("setup_process: NLO type not implemented!")
       end select
       call blha_template%reset ()
       deallocate (core_template)
       if (allocated (phs_config_template_other)) deallocate (phs_config_template_other)
    end do

    if (verb)  call intg%process%write (screen = .true.)
    
    intg%process_has_me = intg%process%has_matrix_element ()
    if (.not. intg%process_has_me) then
       call msg_warning ("Process '" &
            // char (intg%process_id) // "': matrix element vanishes")
    end if
    
    sqrts = local%get_sqrts ()
    decay_rest_frame = &
         var_list%get_lval (var_str ("?decay_rest_frame"))    
    if (intg%process_has_me) then
       call intg%process%setup_beams_beam_structure &
            (local%beam_structure, sqrts, decay_rest_frame)
    end if
    call intg%process%check_masses ()
    if (verb .and. intg%process_has_me) then
       call intg%process%beams_startup_message &
            (beam_structure = local%beam_structure)
    end if
    
    if (intg%process_has_me) then
       call intg%process%get_pdg_in (pdg_prc)
    else
       allocate (pdg_prc (n_in, n_components))
       pdg_prc = 0
    end if
    call dispatch_sf_config (sf_config, sf_prop, local, pdg_prc)
    sf_trace = &
         var_list%get_lval (var_str ("?sf_trace"))
    sf_trace_file = &
         var_list%get_sval (var_str ("$sf_trace_file"))
    if (sf_trace) then
       call intg%process%init_sf_chain (sf_config, sf_trace_file)
    else
       call intg%process%init_sf_chain (sf_config)
    end if

    if (intg%process_has_me) then
       call intg%process%configure_phs &
            (intg%rebuild_phs, intg%ignore_phs_mismatch, verbose=verbose, &
             combined_integration=intg%combined_integration)
       if (size (sf_config) > 0) then
          call intg%process%collect_channels (phs_channel_collection)
       else if (intg%process%contains_trivial_component ()) then
          call msg_fatal ("Integrate: 2 -> 1 process can't be handled &
               &with fixed-energy beams")
       end if
       call dispatch_sf_channels &
            (sf_channel, sf_string, sf_prop, phs_channel_collection, local)
       if (allocated (sf_channel)) then
          if (size (sf_channel) > 0) then
             call intg%process%set_sf_channel (sf_channel)
          end if
       end if
       call phs_channel_collection%final ()
       if (verb)  call intg%process%sf_startup_message (sf_string)    
    end if
    
    call intg%setup_process_mci ()
    call intg%process%setup_terms ()

    if (associated (local%pn%cuts_lexpr)) then
       if (verb)  call msg_message ("Applying user-defined cuts.")
       call expr_factory%init (local%pn%cuts_lexpr)
       call intg%process%set_cuts (expr_factory)
    else
       if (verb)  call msg_warning ("No cuts have been defined.")
    end if    
    if (associated (local%pn%scale_expr)) then
       if (verb) call msg_message ("Using user-defined general scale.")
       call expr_factory%init (local%pn%scale_expr)
       call intg%process%set_scale (expr_factory)
    end if
    if (associated (local%pn%fac_scale_expr)) then
       if (verb) call msg_message ("Using user-defined factorization scale.")
       call expr_factory%init (local%pn%fac_scale_expr)
       call intg%process%set_fac_scale (expr_factory)
    end if
    if (associated (local%pn%ren_scale_expr)) then
       if (verb) call msg_message ("Using user-defined renormalization scale.")
       call expr_factory%init (local%pn%ren_scale_expr)
       call intg%process%set_ren_scale (expr_factory)
    end if
    if (associated (local%pn%weight_expr)) then
       if (verb) call msg_message ("Using user-defined reweighting factor.")
       call expr_factory%init (local%pn%weight_expr)
       call intg%process%set_weight (expr_factory)
    end if

    call intg%process%compute_md5sum ()
  end subroutine integration_setup_process

  subroutine integration_evaluate &
       (intg, process_instance, i_mci, pass, it_list, pacify)
    class(integration_t), intent(inout) :: intg
    type(process_instance_t), intent(inout), target :: process_instance
    integer, intent(in) :: i_mci
    integer, intent(in) :: pass
    type(iterations_list_t), intent(in) :: it_list
    logical, intent(in), optional :: pacify
    integer :: n_calls, n_it
    logical :: adapt_grids, adapt_weights, final
        
    n_it = it_list%get_n_it (pass)
    n_calls = it_list%get_n_calls (pass)
    adapt_grids = it_list%adapt_grids (pass)
    adapt_weights = it_list%adapt_weights (pass)
    final = pass == it_list%get_n_pass ()
    
    call intg%process%integrate (process_instance, &
         i_mci, n_it, n_calls, adapt_grids, adapt_weights, &
         final, pacify)

  end subroutine integration_evaluate

  subroutine integration_make_iterations_list (intg, it_list)
    class(integration_t), intent(in) :: intg
    type(iterations_list_t), intent(out) :: it_list
    integer :: pass, n_pass
    integer, dimension(:), allocatable :: n_it, n_calls
    logical, dimension(:), allocatable :: adapt_grids, adapt_weights
    n_pass = intg%process%get_n_pass_default ()
    allocate (n_it (n_pass), n_calls (n_pass))
    allocate (adapt_grids (n_pass), adapt_weights (n_pass))
    do pass = 1, n_pass
       n_it(pass)          = intg%process%get_n_it_default (pass)
       n_calls(pass)       = intg%process%get_n_calls_default (pass)
       adapt_grids(pass)   = intg%process%adapt_grids_default (pass)
       adapt_weights(pass) = intg%process%adapt_weights_default (pass)
    end do
    call it_list%init (n_it, n_calls, &
         adapt_grids = adapt_grids, adapt_weights = adapt_weights)
  end subroutine integration_make_iterations_list
  
  subroutine integration_init_iteration_multipliers (intg, local)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(in) :: local
    integer :: n_pass, pass
    n_pass = local%it_list%get_n_pass ()
    associate (it_multipliers => intg%iteration_multipliers)
       allocate (it_multipliers%n_calls0 (n_pass))
       do pass = 1, n_pass
          it_multipliers%n_calls0(pass) = local%it_list%get_n_calls (pass)
       end do
       it_multipliers%mult_real = local%var_list%get_rval &
           (var_str ("mult_call_real"))
       it_multipliers%mult_virt = local%var_list%get_rval &
           (var_str ("mult_call_virt"))
       it_multipliers%mult_pdf = local%var_list%get_rval &
           (var_str ("mult_call_pdf"))
    end associate
  end subroutine integration_init_iteration_multipliers

  subroutine integration_apply_call_multipliers (intg, n_pass, i_component, it_list)
    class(integration_t), intent(in) :: intg
    integer, intent(in) :: n_pass, i_component
    type(iterations_list_t), intent(inout) :: it_list
    integer :: nlo_type
    integer :: n_calls0, n_calls
    integer :: pass
    real(default) :: multiplier
    nlo_type = intg%process%get_component_nlo_type (i_component)
    do pass = 1, n_pass
       associate (multipliers => intg%iteration_multipliers)
          select case (nlo_type)
          case (NLO_REAL)
             multiplier = multipliers%mult_real
          case (NLO_VIRTUAL)
             multiplier = multipliers%mult_virt
          case (NLO_PDF)
             multiplier = multipliers%mult_pdf
          case (NLO_THRESHOLD_RESUMMATION)
             multiplier = multipliers%mult_threshold
          case default
             return
          end select
       end associate
       n_calls0 = intg%iteration_multipliers%n_calls0 (pass)
       n_calls = floor (multiplier * n_calls0)
       call it_list%set_n_calls (pass, n_calls)
    end do
  end subroutine integration_apply_call_multipliers

  subroutine integration_init (intg, process_id, local, global, local_stack)
    class(integration_t), intent(out) :: intg
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(inout), optional, target :: global
    logical, intent(in), optional :: local_stack
    logical :: use_local
    use_local = .false.;  if (present (local_stack))  use_local = local_stack
    if (present (global)) then
       call intg%create_process (process_id, global)
    else if (use_local) then
       call intg%create_process (process_id, local)
    else
       call intg%create_process (process_id)
    end if
    call intg%init_process (local)
    call intg%setup_process (local)
    call intg%init_iteration_multipliers (local)
  end subroutine integration_init

  subroutine integration_integrate (intg, local, eff_reset)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(in), target :: local
    logical, intent(in), optional :: eff_reset
    type(string_t) :: log_filename
    type(var_list_t), pointer :: var_list
    type(process_instance_t), allocatable, target :: process_instance
    type(iterations_list_t) :: it_list
    logical :: pacify
    integer :: pass, i_mci, n_mci, n_pass
    integer :: i_component
    integer :: nlo_type
    logical :: display_summed
    logical :: use_internal_color_correlations
    type(string_t) :: color_method

    var_list => intg%process%get_var_list_ptr ()
    
    color_method = var_list%get_sval (var_str ('$correlation_me_method'))
    use_internal_color_correlations = color_method == 'omega'

    allocate (process_instance)
    call process_instance%init (intg%process, use_internal_color_correlations, &
                                combined_integration = intg%combined_integration)

    if (process_instance%has_blha_component ()) then
       call process_instance%create_blha_interface (local%beam_structure)
       call process_instance%load_blha_libraries (local%os_data)
    end if

    call openmp_set_num_threads_verbose &
         (var_list%get_ival (var_str ("openmp_num_threads")), &
          var_list%get_lval (var_str ("?openmp_logging")))
    pacify = var_list%get_lval (var_str ("?pacify"))

    display_summed = .true.
    n_mci = intg%process%get_n_mci ()
    if (n_mci == 1) then
       write (msg_buffer, "(A,A,A)") &
            "Starting integration for process '", &
            char (intg%process%get_id ()), "'"
       call msg_message ()
    end if
    call intg%setup_component_cores ()

    do i_mci = 1, n_mci
       i_component = intg%process%i_mci_to_i_component (i_mci)
       if (intg%process%is_active_nlo_component (i_component)) then
          select type (pcm => process_instance%pcm)
          class is (pcm_instance_nlo_t)
             if (pcm%collect_matrix_elements)  call pcm%collector%reset ()
          end select
         if (n_mci > 1) then
            write (msg_buffer, "(A,A,A,I0)") &
                 "Starting integration for process '", &
                 char (intg%process%get_id ()), "' part ", i_mci
            call msg_message ()
         end if
         n_pass = local%it_list%get_n_pass ()
         if (n_pass == 0) then
            call msg_message ("Integrate: iterations not specified, &
                 &using default")
            call intg%make_iterations_list (it_list)
            n_pass = it_list%get_n_pass ()
         else
            it_list = local%it_list
         end if
         call intg%apply_call_multipliers (n_pass, i_mci, it_list)
         call msg_message ("Integrate: " // char (it_list%to_string ()))
         do pass = 1, n_pass
            call intg%evaluate (process_instance, i_mci, pass, it_list, pacify)
            if (signal_is_pending ())  return
         end do
         call intg%process%final_integration (i_mci)       
         if (intg%vis_history) then
            call intg%process%display_integration_history &
                 (i_mci, intg%history_filename, local%os_data, eff_reset)
         end if       
         if (local%logfile == intg%log_filename) then
            if (intg%run_id /= "") then
               log_filename = intg%process_id // "." // intg%run_id // &
                    ".var.log"
            else
               log_filename = intg%process_id // ".var.log"
            end if
            call msg_message ("Name clash for global logfile and process log: ", &
                 arr =[var_str ("| Renaming log file from ") // local%logfile, &
                       var_str ("|   to ") // log_filename // var_str (" .")])
         else
            log_filename = intg%log_filename
         end if
         call intg%process%write_logfile (i_mci, log_filename)    
       else
         nlo_type = intg%process%get_component_nlo_type (i_mci)
         if (nlo_type /= NLO_SUBTRACTION) display_summed = .false.
       end if          
    end do

    if (n_mci > 1 .and. display_summed) then
       call msg_message ("Integrate: sum of all components")
       call intg%process%display_summed_results ()
    end if

    call process_instance%final ()
    deallocate (process_instance)

  end subroutine integration_integrate
  
  subroutine integration_setup_component_cores (intg)
    class(integration_t), intent(inout) :: intg
    associate (process => intg%process)
       call setup_nlo_component_cores (process)
    end associate
  end subroutine integration_setup_component_cores

  subroutine integration_setup_process_mci (intg)
    class(integration_t), intent(inout) :: intg
    call intg%process%setup_mci (intg%combined_integration)
  end subroutine integration_setup_process_mci

  subroutine integration_integrate_dummy (intg)
    class(integration_t), intent(inout) :: intg
    call intg%process%integrate_dummy ()
  end subroutine integration_integrate_dummy
     
  subroutine integration_sampler_test (intg)
    class(integration_t), intent(inout) :: intg
    type(process_instance_t), allocatable, target :: process_instance
    integer :: n_mci, i_mci
    type(timer_t) :: timer_mci, timer_tot
    real(default) :: t_mci, t_tot
    allocate (process_instance)
    call process_instance%init (intg%process)
    n_mci = intg%process%get_n_mci ()
    if (n_mci == 1) then
       write (msg_buffer, "(A,A,A)") &
            "Test: probing process '", &
            char (intg%process%get_id ()), "'"
       call msg_message ()
    end if
    call timer_tot%start ()
    do i_mci = 1, n_mci
       if (n_mci > 1) then
          write (msg_buffer, "(A,A,A,I0)") &
               "Test: probing process '", &
               char (intg%process%get_id ()), "' part ", i_mci
          call msg_message ()
       end if
       call timer_mci%start ()
       call intg%process%sampler_test &
            (process_instance, i_mci, intg%n_calls_test)
       call timer_mci%stop ()
       t_mci = timer_mci
       write (msg_buffer, "(A,ES12.5)")  "Test: " &
            // "time in seconds (wallclock): ", t_mci
       call msg_message ()
    end do
    call timer_tot%stop ()
    t_tot = timer_tot
    if (n_mci > 1) then
       write (msg_buffer, "(A,ES12.5)")  "Test: " &
            // "total time      (wallclock): ", t_tot
       call msg_message ()
    end if
    call process_instance%final ()
  end subroutine integration_sampler_test

  function integration_get_process_ptr (intg) result (ptr)
    class(integration_t), intent(in) :: intg
    type(process_t), pointer :: ptr
    ptr => intg%process
  end function integration_get_process_ptr

  subroutine integrate_process (process_id, local, global, local_stack, init_only, eff_reset)
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(inout), optional, target :: global
    logical, intent(in), optional :: local_stack, init_only, eff_reset
    type(string_t) :: prclib_name
    type(integration_t) :: intg
    character(32) :: buffer

    if (.not. associated (local%prclib)) then
       call msg_fatal ("Integrate: current process library is undefined")
       return
    end if

    if (.not. local%prclib%is_active ()) then
       call msg_message ("Integrate: current process library needs compilation")
       prclib_name = local%prclib%get_name ()
       call compile_library (prclib_name, local)
       if (signal_is_pending ())  return
       call msg_message ("Integrate: compilation done")
    end if

    call intg%init (process_id, local, global, local_stack)
    if (signal_is_pending ())  return

    if (present (init_only)) then
       if (init_only) return
    end if

    if (intg%n_calls_test > 0) then
       write (buffer, "(I0)")  intg%n_calls_test
       call msg_message ("Integrate: test (" // trim (buffer) // " calls) ...")
       call intg%sampler_test ()
       call msg_message ("Integrate: ... test complete.")
       if (signal_is_pending ())  return
    end if

    if (intg%phs_only) then
       call msg_message ("Integrate: phase space only, skipping integration")
    else
       if (intg%process_has_me) then
          call intg%integrate (local, eff_reset)
       else
          call intg%integrate_dummy ()
       end if
    end if

  end subroutine integrate_process


end module integrations
