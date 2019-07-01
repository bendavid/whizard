! WHIZARD 2.2.0 May 18 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use os_interface
  use cputime
  use sm_qcd
  use ifiles
  use lexers
  use parser
  use flavors
  use pdg_arrays
  use variables
  use expressions
  use models
  use interactions
  use sf_mappings
  use sf_base
  use phs_base
  use mappings
  use phs_forests
  use phs_wood
  use rng_base
  use mci_base
  use process_libraries
  use prc_core
  use processes
  use process_stacks
  use iterations
  use rt_data
  use dispatch
  use process_configurations
  use compilations

  implicit none
  private

  public :: integration_t
  public :: integrate_process
  public :: integrations_test
  public :: integrations_history_test    

  type :: integration_t
    private
    type(string_t) :: process_id
    type(string_t) :: run_id
    type(process_t), pointer :: process => null ()
    logical :: rebuild_phs = .false.
    logical :: ignore_phs_mismatch = .false.
    logical :: phs_only = .false.
    logical :: process_has_me = .true.
    integer :: n_calls_test = 0
    logical :: vis_history = .true.
    type(string_t) :: history_filename
    type(string_t) :: log_filename
   contains
     procedure :: create_process => integration_create_process
     procedure :: setup_process => integration_setup_process
     procedure :: evaluate => integration_evaluate
     procedure :: make_iterations_list => integration_make_iterations_list
     procedure :: init => integration_init
     procedure :: integrate => integration_integrate
     procedure :: integrate_dummy => integration_integrate_dummy 
     procedure :: sampler_test => integration_sampler_test 
  end type integration_t


contains

  subroutine integration_create_process (intg, process_id, global) !, verbose)
    class(integration_t), intent(out) :: intg
    type(rt_data_t), intent(inout), target :: global
    type(string_t), intent(in) :: process_id
    type(var_list_t), pointer :: var_list
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    type(process_entry_t), pointer :: process_entry

    var_list => global%var_list
    intg%process_id = process_id
    intg%run_id = var_list_get_sval (var_list, var_str ("$run_id"))

    call dispatch_qcd (qcd, global)    
    call dispatch_rng_factory (rng_factory, global)

    allocate (process_entry)
    call process_entry%init (intg%process_id, intg%run_id, global%prclib, &
         global%os_data, qcd, rng_factory, global%model_list)
    call global%process_stack%push (process_entry)

  end subroutine integration_create_process

  subroutine integration_setup_process (intg, global, process, verbose)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(inout), target :: global
    type(process_t), intent(in), target, optional :: process
    logical, intent(in), optional :: verbose
    
    class(prc_core_t), allocatable :: core_template
    class(phs_config_t), allocatable :: phs_config_template
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defs
    class(mci_t), allocatable :: mci_template
    type(qcd_t) :: qcd
    integer :: n_components, n_in, i_component
    type(pdg_array_t), dimension(:,:), allocatable :: pdg_prc
    type(process_component_def_t), pointer :: config
    type(helicity_selection_t), allocatable :: helicity_selection
    real(default) :: sqrts
    logical :: decay_rest_frame, use_color_factors
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_prop_t) :: sf_prop
    type(sf_channel_t), dimension(:), allocatable :: sf_channel
    type(phs_channel_collection_t) :: phs_channel_collection
    logical :: sf_trace
    type(string_t) :: sf_string, sf_trace_file
    logical :: verb

    verb = .true.; if (present (verbose))  verb = verbose
    
    if (present (process)) then
       intg%process => process
       intg%process_id = process%get_id ()
       intg%run_id = process%get_run_id ()
    else
       intg%process => global%process_stack%get_process_ptr (intg%process_id)
    end if

    call intg%process%set_var_list (global%var_list)

    intg%rebuild_phs = &
         var_list_get_lval (global%var_list, var_str ("?rebuild_phase_space"))
    intg%ignore_phs_mismatch = &
         .not. var_list_get_lval (global%var_list, var_str ("?check_phs_file"))
    intg%phs_only = var_list_get_lval &
         (global%var_list, var_str ("?phs_only"))
    phs_par%m_threshold_s = var_list_get_rval &
         (global%var_list, var_str ("phs_threshold_s"))
    phs_par%m_threshold_t = var_list_get_rval &
         (global%var_list, var_str ("phs_threshold_t"))
    phs_par%off_shell = var_list_get_ival &
         (global%var_list, var_str ("phs_off_shell"))
    phs_par%keep_nonresonant = var_list_get_lval &
         (global%var_list, var_str ("?phs_keep_nonresonant"))
    phs_par%t_channel = var_list_get_ival &
         (global%var_list, var_str ("phs_t_channel"))
    mapping_defs%energy_scale = var_list_get_rval &
         (global%var_list, var_str ("phs_e_scale"))
    mapping_defs%invariant_mass_scale = var_list_get_rval &
         (global%var_list, var_str ("phs_m_scale"))
    mapping_defs%momentum_transfer_scale = var_list_get_rval &
         (global%var_list, var_str ("phs_q_scale"))
    mapping_defs%step_mapping = var_list_get_lval &
         (global%var_list, var_str ("?phs_step_mapping"))
    mapping_defs%step_mapping_exp = var_list_get_lval &
         (global%var_list, var_str ("?phs_step_mapping_exp"))
    mapping_defs%enable_s_mapping = var_list_get_lval &
         (global%var_list, var_str ("?phs_s_mapping"))

    call dispatch_phs (phs_config_template, global, &
         intg%process_id, mapping_defs, phs_par)
    
    intg%n_calls_test = &
         var_list_get_ival (global%var_list, var_str ("n_calls_test"))

    !!! We avoid two dots in the filename due to a bug in certain MetaPost versions.
    if (intg%run_id /= "") then
       intg%history_filename = intg%process_id // "." // intg%run_id &
            // "-history"
       intg%log_filename = intg%process_id // "." // intg%run_id // ".log"
    else
       intg%history_filename = intg%process_id // "-history"
       intg%log_filename = intg%process_id // ".log"
    end if

    call dispatch_mci (mci_template, global, intg%process_id)

    if (verb) then
       call msg_message ("Initializing integration for process " &
            // char (intg%process_id) // ":")
       if (intg%run_id /= "") then
          call msg_message ("Run ID = " // '"' // char (intg%run_id) // '"')
       end if
    end if
    
    helicity_selection = global%get_helicity_selection ()

    intg%vis_history = &
         var_list_get_lval (global%var_list, var_str ("?vis_history"))
    use_color_factors = var_list_get_lval &
         (global%var_list, var_str ("?read_color_factors"))
    
    call dispatch_qcd (qcd, global)    

    n_components = intg%process%get_n_components ()
    n_in = intg%process%get_n_in ()
    
    do i_component = 1, n_components
       config => intg%process%get_component_def_ptr (i_component)
       call dispatch_core (core_template, config%get_core_def_ptr (), &
            intg%process%get_model_ptr (), helicity_selection, qcd, &
            use_color_factors)
       call intg%process%init_component &
            (i_component, core_template, mci_template, phs_config_template)
       deallocate (core_template)
    end do

    call intg%process%write (screen = .true.)
    
    intg%process_has_me = intg%process%has_matrix_element ()
    if (.not. intg%process_has_me) then
       call msg_warning ("Process '" &
            // char (intg%process_id) // "': matrix element vanishes")
    end if
    
    sqrts = global%get_sqrts ()
    decay_rest_frame = &
         var_list_get_lval (global%var_list, var_str ("?decay_rest_frame"))    
    if (intg%process_has_me) then
       if (global%beam_structure%is_set ()) then
          call intg%process%setup_beams_beam_structure &
               (global%beam_structure, sqrts, global%model, decay_rest_frame)
        else if (n_in == 2) then
          call intg%process%setup_beams_sqrts &
               (sqrts, global%beam_structure)
       else 
          call intg%process%setup_beams_decay &
               (decay_rest_frame, global%beam_structure)
       end if
    end if
    call intg%process%check_masses ()
    if (intg%process_has_me)  call intg%process%beams_startup_message &
         (beam_structure = global%beam_structure)

    if (intg%process_has_me) then
       call intg%process%get_pdg_in (pdg_prc)
    else
       allocate (pdg_prc (n_in, n_components))
       pdg_prc = 0
    end if
    call dispatch_sf_config (sf_config, sf_prop, global, pdg_prc)
    sf_trace = &
         var_list_get_lval (global%var_list, var_str ("?sf_trace"))
    sf_trace_file = &
         var_list_get_sval (global%var_list, var_str ("$sf_trace_file"))
    if (sf_trace) then
       call intg%process%init_sf_chain (sf_config, sf_trace_file)
    else
       call intg%process%init_sf_chain (sf_config)
    end if

    if (intg%process_has_me) then
       call intg%process%configure_phs (intg%rebuild_phs, intg%ignore_phs_mismatch)
       if (size (sf_config) > 0) then
          call intg%process%collect_channels (phs_channel_collection)
       else if (intg%process%contains_trivial_component ()) then
          call msg_fatal ("Integrate: 2 -> 1 process can't be handled &
               &with fixed-energy beams")
       end if

       call dispatch_sf_channels &
            (sf_channel, sf_string, sf_prop, phs_channel_collection, global)
       if (allocated (sf_channel)) then
          if (size (sf_channel) > 0) then
             call intg%process%set_sf_channel (sf_channel)
          end if
       end if
       call phs_channel_collection%final ()
       call intg%process%sf_startup_message (sf_string)    
    end if
    
    call intg%process%setup_mci ()
    call intg%process%setup_terms ()

    if (associated (global%pn%cuts_lexpr)) then
       if (verb)  call msg_message ("Applying user-defined cuts.")
       call intg%process%set_cuts (global%pn%cuts_lexpr)       
    else
       if (verb)  call msg_warning ("No cuts have been defined.")
    end if    
    if (associated (global%pn%scale_expr) .and. verb) then
       call msg_message ("Using user-defined general scale.")
       call intg%process%set_scale (global%pn%scale_expr)       
    end if
    if (associated (global%pn%fac_scale_expr) .and. verb) then
       call msg_message ("Using user-defined factorization scale.")
       call intg%process%set_fac_scale (global%pn%fac_scale_expr)
    end if
    if (associated (global%pn%ren_scale_expr) .and. verb) then
       call msg_message ("Using user-defined renormalization scale.")
       call intg%process%set_ren_scale (global%pn%ren_scale_expr)
    end if
    if (associated (global%pn%weight_expr) .and. verb) then
       call msg_message ("Using user-defined reweighting factor.")
       call intg%process%set_weight (global%pn%weight_expr)
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
  
  subroutine integration_init (intg, process_id, global)
    class(integration_t), intent(out) :: intg
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    
    call intg%create_process (process_id, global)
    call intg%setup_process (global)
  end subroutine integration_init

  subroutine integration_integrate (intg, global, eff_reset)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: eff_reset
    type(string_t) :: log_filename
    type(process_instance_t), allocatable, target :: process_instance
    type(iterations_list_t) :: it_list
    logical :: pacify
    integer :: pass, i_mci, n_mci, n_pass

    allocate (process_instance)
    call process_instance%init (intg%process)

    call openmp_set_num_threads_verbose &
         (var_list_get_ival (global%var_list, "openmp_num_threads"), &
          var_list_get_lval (global%var_list, "?openmp_logging"))    
    pacify = var_list_get_lval (global%var_list, var_str ("?pacify"))

    n_mci = intg%process%get_n_mci ()
    if (n_mci == 1) then
       write (msg_buffer, "(A,A,A)") &
            "Starting integration for process '", &
            char (intg%process%get_id ()), "'"
       call msg_message ()
    end if
    do i_mci = 1, n_mci
       if (n_mci > 1) then
          write (msg_buffer, "(A,A,A,I0)") &
               "Starting integration for process '", &
               char (intg%process%get_id ()), "' part ", i_mci
          call msg_message ()
       end if
       n_pass = global%it_list%get_n_pass ()
       if (n_pass == 0) then
          call msg_message ("Integrate: iterations not specified, &
               &using default")
          call intg%make_iterations_list (it_list)
          n_pass = it_list%get_n_pass ()
       else
          it_list = global%it_list
       end if
       call msg_message ("Integrate: " // char (it_list%to_string ()))
       do pass = 1, n_pass
          call intg%evaluate (process_instance, i_mci, pass, it_list, pacify)
          if (signal_is_pending ())  return
       end do
       call intg%process%final_integration (i_mci)       
       if (intg%vis_history) then
          call intg%process%display_integration_history &
               (i_mci, intg%history_filename, global%os_data, eff_reset)
       end if       
       if (global%logfile == intg%log_filename) then
          if (intg%run_id /= "") then
             log_filename = intg%process_id // "." // intg%run_id // &
                  ".var.log"
          else
             log_filename = intg%process_id // ".var.log"
          end if
          call msg_message ("Name clash for global logfile and process log: ", &
               arr =[var_str ("| Renaming log file from ") // global%logfile, &
                     var_str ("|   to ") // log_filename // var_str (" .")])
       else
          log_filename = intg%log_filename
       end if
       call intg%process%write_logfile (i_mci, log_filename)              
    end do

    if (n_mci > 1) then
       call msg_message ("Integrate: sum of all components")
       call intg%process%display_summed_results ()
    end if

    call process_instance%final ()
    deallocate (process_instance)

  end subroutine integration_integrate
  
  subroutine integration_integrate_dummy (intg)
    class(integration_t), intent(inout) :: intg
    call intg%process%integrate_dummy ()
  end subroutine integration_integrate_dummy
     
  subroutine integration_sampler_test (intg, global)
    class(integration_t), intent(inout) :: intg
    type(rt_data_t), intent(inout), target :: global
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

  subroutine integrate_process (process_id, global, init_only, eff_reset)
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: init_only, eff_reset
    type(string_t) :: prclib_name
    type(integration_t) :: intg
    character(32) :: buffer

    if (.not. associated (global%prclib)) then
       call msg_fatal ("Integrate: current process library is undefined")
       return
    end if

    if (.not. global%prclib%is_active ()) then
       call msg_message ("Integrate: current process library needs compilation")
       prclib_name = global%prclib%get_name ()
       call compile_library (prclib_name, global)
       if (signal_is_pending ())  return
       call msg_message ("Integrate: compilation done")
    end if

    call intg%init (process_id, global)
    if (signal_is_pending ())  return

    if (present (init_only)) then
       if (init_only) return
    end if

    if (intg%n_calls_test > 0) then
       write (buffer, "(I0)")  intg%n_calls_test
       call msg_message ("Integrate: test (" // trim (buffer) // " calls) ...")
       call intg%sampler_test (global)
       call msg_message ("Integrate: ... test complete.")
       if (signal_is_pending ())  return
    end if

    if (intg%phs_only) then
       call msg_message ("Integrate: phase space only, skipping integration")
    else
       if (intg%process_has_me) then
          call intg%integrate (global, eff_reset)
       else
          call intg%integrate_dummy ()
       end if
    end if
  end subroutine integrate_process


  subroutine integrations_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (integrations_1, "integrations_1", &
         "intrinsic test process", &
         u, results)
    call test (integrations_2, "integrations_2", &
         "intrinsic test process with cut", &
         u, results)
    call test (integrations_3, "integrations_3", &
         "standard phase space", &
         u, results)
    call test (integrations_4, "integrations_4", &
         "VAMP integration (one iteration)", &
         u, results)
    call test (integrations_5, "integrations_5", &
         "VAMP integration (three iterations)", &
         u, results)
    call test (integrations_6, "integrations_6", &
         "VAMP integration (three passes)", &
         u, results)
    call test (integrations_7, "integrations_7", &
         "VAMP integration with wood phase space", &
         u, results)
    call test (integrations_8, "integrations_8", &
         "integration with structure function", &
         u, results)
  end subroutine integrations_test

  subroutine integrations_history_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (integrations_history_1, "integrations_history_1", &
         "Test integration history files", &
         u, results)
  end subroutine integrations_history_test  

  subroutine integrations_1 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: integrations_1"
    write (u, "(A)")  "*   Purpose: integrate test process"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    libname = "integration_1"
    procname = "prc_config_a"
    
    call prepare_test_library (global, libname, 1)
    call compile_library (libname, global)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("integrations1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.) 
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    call reset_interaction_counter ()
    call integrate_process (procname, global)

    call global%write (u, vars = [ &
         var_str ("$method"), &
         var_str ("sqrts"), &
         var_str ("$integration_method"), &
         var_str ("$phs_method"), &
         var_str ("$run_id")])
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_1"
    
  end subroutine integrations_1
  
  subroutine integrations_2 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global

    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    
    type(string_t), dimension(0) :: empty_string_array

    write (u, "(A)")  "* Test output: integrations_2"
    write (u, "(A)")  "*   Purpose: integrate test process with cut"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    write (u, "(A)")  "* Prepare a cut expression"
    write (u, "(A)")

    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    global%pn%cuts_lexpr => parse_tree_get_root_ptr (parse_tree)
    
    write (u, "(A)")  "* Build and initialize a test process"
    write (u, "(A)")

    libname = "integration_3"
    procname = "prc_config_a"
    
    call prepare_test_library (global, libname, 1)
    call compile_library (libname, global)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("integrations1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)  
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])
    
    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%write (u, vars = empty_string_array)
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_2"
    
  end subroutine integrations_2
  
  subroutine integrations_3 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    integer :: u_phs
    
    write (u, "(A)")  "* Test output: integrations_3"
    write (u, "(A)")  "*   Purpose: integrate test process"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_phs_forest_init ()

    call global%global_init ()

    libname = "integration_3"
    procname = "prc_config_a"
    
    call prepare_test_library (global, libname, 1)
    call compile_library (libname, global)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("integrations1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("default"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("midpoint"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?phs_s_mapping"),&
         .false., is_known = .true.)   
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    write (u, "(A)")  "* Create a scratch phase-space file"
    write (u, "(A)")

    u_phs = free_unit ()
    open (u_phs, file = "integrations_3.phs", &
         status = "replace", action = "write")
    call write_test_phs_file (u_phs, var_str ("prc_config_a_i1"))
    close (u_phs)

    call var_list_set_string (global%var_list, var_str ("$phs_file"),&
         var_str ("integrations_3.phs"), is_known = .true.)

    call global%it_list%init ([1], [1000])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%write (u, vars = [ &
         var_str ("$phs_method"), &
         var_str ("$phs_file")])
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_3"
    
  end subroutine integrations_3
  
  subroutine integrations_4 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: integrations_4"
    write (u, "(A)")  "*   Purpose: integrate test process using VAMP"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    libname = "integrations_4_lib"
    procname = "integrations_4"
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([1], [1000])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%pacify (efficiency_reset = .true., error_reset = .true.)
    call global%write (u, vars = [var_str ("$integration_method")], &
            pacify = .true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_4"
    
  end subroutine integrations_4
  
  subroutine integrations_5 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: integrations_5"
    write (u, "(A)")  "*   Purpose: integrate test process using VAMP"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    libname = "integrations_5_lib"
    procname = "integrations_5"
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([3], [1000])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%pacify (efficiency_reset = .true., error_reset = .true.)
    call global%write (u, vars = [var_str ("$integration_method")], &
            pacify = .true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_5"
    
  end subroutine integrations_5
  
  subroutine integrations_6 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: no_vars
    
    write (u, "(A)")  "* Test output: integrations_6"
    write (u, "(A)")  "*   Purpose: integrate test process using VAMP"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()

    libname = "integrations_6_lib"
    procname = "integrations_6"
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([3, 3, 3], [1000, 1000, 1000], &
         adapt = [.true., .true., .false.], &
         adapt_code = [var_str ("wg"), var_str ("g"), var_str ("")])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%pacify (efficiency_reset = .true., error_reset = .true.)
    call global%write (u, vars = no_vars, pacify = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_6"
    
  end subroutine integrations_6
  
  subroutine integrations_7 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: no_vars
    integer :: iostat, u_phs
    character(95) :: buffer
    type(string_t) :: phs_file
    logical :: exist
    
    write (u, "(A)")  "* Test output: integrations_7"
    write (u, "(A)")  "*   Purpose: integrate test process using VAMP"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_phs_forest_init ()

    call global%global_init ()

    libname = "integrations_7_lib"
    procname = "integrations_7"
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?phs_s_mapping"),&
         .false., is_known = .true.)    
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([3, 3, 3], [1000, 1000, 1000], &
         adapt = [.true., .true., .false.], &
         adapt_code = [var_str ("wg"), var_str ("g"), var_str ("")])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global)
    
    call global%pacify (efficiency_reset = .true., error_reset = .true.)
    call global%write (u, vars = no_vars, pacify = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Generated phase-space file"
    write (u, "(A)")

    phs_file = procname // "_i1.r1.phs"
    inquire (file = char (phs_file), exist = exist)
    if (exist) then
       u_phs = free_unit ()
       open (u_phs, file = char (phs_file), action = "read", status = "old")
       iostat = 0
       do while (iostat == 0)
          read (u_phs, "(A)", iostat = iostat)  buffer
          if (iostat == 0)  write (u, "(A)")  trim (buffer)
       end do
       close (u_phs)
    else
       write (u, "(A)")  "[file is missing]"
    end if

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_7"
    
  end subroutine integrations_7
  
  subroutine integrations_8 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    type(flavor_t) :: flv
    
    write (u, "(A)")  "* Test output: integrations_8"
    write (u, "(A)")  "*   Purpose: integrate test process using VAMP &
         &with structure function"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_phs_forest_init ()

    call global%global_init ()

    libname = "integrations_8_lib"
    procname = "integrations_8"
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?vis_history"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?phs_s_mapping"),&
         .false., is_known = .true.)  
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    
    
    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("ms"), &
         0._default, is_known = .true.)

    call reset_interaction_counter ()

    call flavor_init (flv, 25, global%model)
         
    call global%beam_structure%init_sf (flavor_get_name ([flv, flv]), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("sf_test_1"))

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call global%it_list%init ([1], [1000])
    call integrate_process (procname, global)
    
    call global%write (u, vars = [var_str ("ms")])

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_8"
    
  end subroutine integrations_8
  
  subroutine integrations_history_1 (u)
    integer, intent(in) :: u
    type(string_t) :: libname, procname
    type(rt_data_t), target :: global
    type(string_t), dimension(0) :: no_vars
    integer :: iostat, u_his
    character(91) :: buffer
    type(string_t) :: his_file, ps_file, pdf_file
    logical :: exist, exist_ps, exist_pdf
    
    write (u, "(A)")  "* Test output: integrations_history_1"
    write (u, "(A)")  "*   Purpose: test integration history files"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process and parameters"
    write (u, "(A)")

    call syntax_model_file_init ()
    call syntax_phs_forest_init ()

    call global%global_init ()

    libname = "integrations_history_1_lib"
    procname = "integrations_history_1"

    call var_list_set_log (global%var_list, var_str ("?vis_history"), &
         .true., is_known = .true.)        
    call var_list_set_log (global%var_list, var_str ("?integration_timer"),&
         .false., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?phs_s_mapping"),&
         .false., is_known = .true.)    
    
    call prepare_test_library (global, libname, 1, [procname])
    call compile_library (libname, global)

    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_phase_space"), .true., intrinsic = .true.)
    call var_list_append_log (global%var_list, &
         var_str ("?rebuild_grids"), .true., intrinsic = .true.)

    call var_list_set_string (global%var_list, var_str ("$run_id"), &
         var_str ("r1"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$integration_method"),&
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .true., is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("error_threshold"),&
         5E-6_default, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("seed"), &
         0, is_known=.true.)    

    call var_list_set_real (global%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)

    call global%it_list%init ([2, 2, 2], [1000, 1000, 1000], &
         adapt = [.true., .true., .false.], &
         adapt_code = [var_str ("wg"), var_str ("g"), var_str ("")])

    write (u, "(A)")  "* Integrate"
    write (u, "(A)")

    call reset_interaction_counter ()
    call integrate_process (procname, global, eff_reset = .true.)
    
    call global%pacify (efficiency_reset = .true., error_reset = .true.)
    call global%write (u, vars = no_vars, pacify = .true.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Generated history files"
    write (u, "(A)")

    his_file = procname // ".r1-history.tex"
    ps_file  = procname // ".r1-history.ps"
    pdf_file = procname // ".r1-history.pdf"
    inquire (file = char (his_file), exist = exist)
    if (exist) then
       u_his = free_unit ()
       open (u_his, file = char (his_file), action = "read", status = "old")
       iostat = 0
       do while (iostat == 0)
          read (u_his, "(A)", iostat = iostat)  buffer
          if (iostat == 0)  write (u, "(A)")  trim (buffer)
       end do
       close (u_his)
    else
       write (u, "(A)")  "[History LaTeX file is missing]"
    end if
    inquire (file = char (ps_file), exist = exist_ps)
    if (exist_ps) then
       write (u, "(A)")  "[History Postscript file exists and is nonempty]"
    else
       write (u, "(A)")  "[History Postscript file is missing/non-regular]"
    end if
    inquire (file = char (pdf_file), exist = exist_pdf)
    if (exist_pdf) then
       write (u, "(A)")  "[History PDF file exists and is nonempty]"
    else
       write (u, "(A)")  "[History PDF file is missing/non-regular]"
    end if    
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()    
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: integrations_history_1"
    
  end subroutine integrations_history_1
  

end module integrations
