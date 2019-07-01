! WHIZARD 2.1.1 September 18 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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
  use limits, only: ITERATIONS_DEFAULT_LIST_SIZE !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!
  use os_interface
  use parser
  use variables
  use models
  use beams
  use mappings
  use phs_forests
  use process_libraries
  use processes
  use strfun_config
  use rt_data
  use iterations
  use compilations

  implicit none
  private

  public :: prepare_me_evaluation
  public :: prepare_me_missing_processes
  public :: integrate_process
  public :: me_test_process
  public :: integrate_missing_processes

  type :: integration_t
    private
    type(string_t) :: process_id
    type(process_t), pointer :: process => null ()
    type(string_t) :: run_id
    logical :: rebuild_phs = .false.
    logical :: check_phs_file = .true.
    type(string_t) :: phs_filename
    type(string_t) :: phs_filename_out
    type(string_t) :: phs_filename_vis
    logical :: phs_only = .false.
    logical :: vis_channels = .false.
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    logical :: rebuild_grids = .false.
    logical :: check_grid_file = .true.
    type(grid_parameters_t) :: grid_parameters
    type(string_t) :: grids_filename
    logical :: use_best_grid = .true.
    real(default) :: accuracy_goal = 0
    real(default) :: abs_error_goal = 0
    real(default) :: rel_error_goal = 0
    logical :: helicity_selection_active = .false.
    real(default) :: helicity_selection_threshold = -1
    integer :: helicity_selection_cutoff = 1000
    logical :: sqrts_known = .false.
    real(default) :: sqrts = -1
    real(default) :: alpha_s = -1
    integer :: n_events_for_me_test = 0
    logical :: time_estimate = .false.
    logical :: vis_history = .true.
    type(string_t) :: history_filename
    type(beam_data_t) :: beam_data
    logical :: use_beams = .false.
    type(sf_list_t), pointer :: sf_list => null ()
    logical :: use_strfun = .false.
    type(iterations_list_t) :: it_list
    integer :: pass_on_file = 0
    integer :: it_on_file = 0
    type(md5sum_grids_t) :: md5sum
    integer :: pass = 0
    integer :: it = 0
    type(string_t) :: log_filename
  end type integration_t


  interface integration_init
    module procedure integration_init0
    module procedure integration_init1
  end interface

  interface integration_integrate
     module procedure integration_integrate0
     module procedure integration_integrate1
  end interface

  interface integration_integrate_dummy
     module procedure integration_integrate_dummy0
     module procedure integration_integrate_dummy1
  end interface

  interface prepare_me_evaluation
     module procedure prepare_me_evaluation0
     module procedure prepare_me_evaluation1
  end interface prepare_me_evaluation

  interface integrate_process
     module procedure integrate_process0
     module procedure integrate_process1
  end interface


contains

  subroutine integration_basic_init (intg, process_id, var_list, verbose)
    type(integration_t), intent(out) :: intg
    type(string_t), intent(in) :: process_id
    type(var_list_t), intent(in) :: var_list
    logical, intent(in), optional :: verbose
    logical :: verb
    intg%process_id = process_id
    intg%run_id = var_list_get_sval (var_list, var_str ("$run_id"))  ! $
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       call msg_message ("Initializating integration for process " &
            // char (intg%process_id) // ":")
       if (intg%run_id /= "") then
          call msg_message ("Run ID = " // '"' // char (intg%run_id) // '"')
       end if
    end if
    intg%rebuild_phs = &
         var_list_get_lval (var_list, var_str ("?rebuild_phase_space"))
    intg%check_phs_file = &
         var_list_get_lval (var_list, var_str ("?check_phs_file"))
    intg%phs_filename = &
         var_list_get_sval (var_list, var_str ("$phs_file"))         ! $
    intg%phs_only = &
         var_list_get_lval (var_list, var_str ("?phs_only"))
    intg%vis_channels = &
         var_list_get_lval (var_list, var_str ("?vis_channels"))
    intg%phs_par%m_threshold_s = &
         var_list_get_rval (var_list, var_str ("phs_threshold_s"))
    intg%phs_par%m_threshold_t = &
         var_list_get_rval (var_list, var_str ("phs_threshold_t"))
    intg%phs_par%off_shell = &
         var_list_get_ival (var_list, var_str ("phs_off_shell"))
    intg%phs_par%keep_nonresonant = &
         var_list_get_lval (var_list, var_str ("?phs_keep_nonresonant"))
    intg%phs_par%t_channel = &
         var_list_get_ival (var_list, var_str ("phs_t_channel"))
    intg%mapping_defaults%energy_scale = &
         var_list_get_rval (var_list, var_str ("phs_e_scale"))
    intg%mapping_defaults%invariant_mass_scale = &
         var_list_get_rval (var_list, var_str ("phs_m_scale"))
    intg%mapping_defaults%momentum_transfer_scale = &
         var_list_get_rval (var_list, var_str ("phs_q_scale"))
    intg%mapping_defaults%step_mapping = &
         var_list_get_lval (var_list, var_str ("?phs_step_mapping"))
    intg%mapping_defaults%step_mapping_exp = &
         var_list_get_lval (var_list, var_str ("?phs_step_mapping_exp"))
    intg%mapping_defaults%enable_s_mapping = &
         var_list_get_lval (var_list, var_str ("?phs_s_mapping"))
    intg%rebuild_grids = &
         var_list_get_lval (var_list, var_str ("?rebuild_grids"))
    intg%check_grid_file = &
         var_list_get_lval (var_list, var_str ("?check_grid_file"))
    intg%grid_parameters%threshold_calls = &
         var_list_get_ival (var_list, var_str ("threshold_calls"))
    intg%grid_parameters%min_calls_per_channel = &
         var_list_get_ival (var_list, var_str ("min_calls_per_channel"))
    intg%grid_parameters%min_calls_per_bin = &
         var_list_get_ival (var_list, var_str ("min_calls_per_bin"))
    intg%grid_parameters%min_bins = &
         var_list_get_ival (var_list, var_str ("min_bins"))
    intg%grid_parameters%max_bins = &
         var_list_get_ival (var_list, var_str ("max_bins"))
    intg%grid_parameters%stratified = &
         var_list_get_lval (var_list, var_str ("?stratified"))
    intg%grid_parameters%use_vamp_equivalences = &
         var_list_get_lval (var_list, var_str ("?use_vamp_equivalences"))
    intg%grid_parameters%channel_weights_power = &
         var_list_get_rval (var_list, var_str ("channel_weights_power"))
    intg%run_id = &
         var_list_get_sval (var_list, var_str ("$run_id"))  ! $
    if (intg%run_id /= "") then
       intg%phs_filename_out = intg%process_id // "." // intg%run_id // ".phs"
       intg%phs_filename_vis = intg%process_id // "." // intg%run_id // "_phs"
       intg%grids_filename = intg%process_id // "." // intg%run_id // ".vg"
       intg%history_filename = intg%process_id // "." // intg%run_id &
            // ".history"
       intg%log_filename = intg%process_id // "." // intg%run_id // ".log"
    else
       intg%phs_filename_out = intg%process_id // ".phs"
       intg%phs_filename_vis = intg%process_id // "_phs"
       intg%grids_filename = intg%process_id // ".vg"
       intg%history_filename = intg%process_id // ".history"
       intg%log_filename = intg%process_id // ".log"
    end if
    intg%use_best_grid = &
         var_list_get_lval (var_list, var_str ("?use_best_grid"))
    intg%accuracy_goal = &
         var_list_get_rval (var_list, var_str ("accuracy_goal"))
    intg%abs_error_goal = &
         var_list_get_rval (var_list, var_str ("error_goal"))
    intg%rel_error_goal = &
         var_list_get_rval (var_list, var_str ("relative_error_goal"))
    intg%helicity_selection_active = &
         var_list_get_lval (var_list, var_str ("?helicity_selection_active"))
    if (intg%helicity_selection_active) then
       intg%helicity_selection_threshold = var_list_get_rval (var_list, &
            var_str ("helicity_selection_threshold"))
       intg%helicity_selection_cutoff = var_list_get_ival (var_list, &
            var_str ("helicity_selection_cutoff"))
    end if
    if (var_list_is_known (var_list, var_str ("alphas"))) then
       intg%alpha_s = var_list_get_rval (var_list, var_str ("alphas"))
    end if
    intg%sqrts_known = var_list_is_known (var_list, "sqrts")
    intg%sqrts = var_list_get_rval (var_list, "sqrts")
    intg%time_estimate = &
         var_list_get_lval (var_list, var_str ("?time_estimate"))
    intg%vis_history = &
         var_list_get_lval (var_list, var_str ("?vis_history"))
    intg%n_events_for_me_test = &
         var_list_get_ival (var_list, var_str ("n_events"))
  end subroutine integration_basic_init

  subroutine integration_check_beam_data (intg, beam_data)
    type(integration_t), intent(inout) :: intg
    type(beam_data_t), intent(in) :: beam_data
    intg%beam_data = beam_data
    intg%use_beams = beam_data_are_valid (intg%beam_data)
    if (intg%use_beams) then
       if (.not. beam_data_masses_are_consistent (intg%beam_data)) then
          call msg_warning &
               ("Masses of beam particle(s) differ from beam masses")
       end if
    end if
  end subroutine integration_check_beam_data

  subroutine maybe_compile_library (global)
    type(rt_data_t), target :: global
    call process_library_update_status (global%prc_lib)
    if (.not. process_library_is_compiled (global%prc_lib)) then
       call compile_library (process_library_get_name (global%prc_lib), &
            global, global%var_list, global%prc_lib)
    end if
  end subroutine maybe_compile_library    

  subroutine integration_init_process (intg, prc_lib, model, var_list, ok)
    type(integration_t), intent(inout) :: intg
    type(process_library_t), intent(inout), target :: prc_lib
    type(model_t), intent(in), target :: model
    type(var_list_t), intent(in), target :: var_list
    logical, intent(out) :: ok
    call process_store_init_process (intg%process, &
         prc_lib, intg%process_id, model, var_list, &
         use_beams = intg%use_beams)
    if (.not. process_is_valid (intg%process)) then
       call msg_fatal ("Process '" &
            // char (intg%process_id) // "': " &
            // "initialization failed.")
       ok = .false.
    else if (.not. intg%phs_only) then
       ok = process_has_matrix_element (intg%process)
    else
       ok = .true.
    end if
    call process_reset_helicity_selection (intg%process, &
         intg%helicity_selection_threshold, intg%helicity_selection_cutoff)
  end subroutine integration_init_process

  subroutine integration_setup_beams (intg, sf_list, var_list, ok)
    type(integration_t), intent(inout) :: intg
    type(sf_list_t), pointer :: sf_list
    type(var_list_t), intent(in) :: var_list
    logical, intent(out) :: ok
    if (intg%use_beams) then
       if (beam_data_get_n_in (intg%beam_data) &
            == process_get_n_in (intg%process)) then
          if (associated (sf_list)) then
             intg%sf_list => sf_list
             call process_setup_beams (intg%process, intg%beam_data, &
                  sf_list_get_n_strfun (intg%sf_list))
             call process_check_beam_setup (intg%process, var_list)
             call sf_list_transfer_to_process (intg%sf_list, intg%process)
             intg%use_strfun = .true.
          else
             call process_setup_beams (intg%process, intg%beam_data, 0)
          end if
          ok = .true.
       else
          call msg_fatal ("Process '" // char (intg%process_id) &
               // "': beam/process mismatch (collision/decay)", &
               (/ var_str ("   --------------------------------------------"), &
                  var_str ("This possibly means that you tried to generate "), &
                  var_str ("a forbidden process, for which WHIZARD could not"), &
                  var_str ("find a matrix element. Or there is a"), &
                  var_str ("mismatch between beams and hard interaction.") /) )
          ok = .false.
       end if
    else if (intg%sqrts_known) then
       call process_setup_beams &
            (intg%process, intg%beam_data, 0, sqrts = intg%sqrts)
    else
       call process_setup_beams &
            (intg%process, intg%beam_data, 0)
    end if
    if (ok)  call process_connect_strfun (intg%process, ok)
    if (.not. ok) then
       call msg_error ("Process '" // char (intg%process_id) &
            // "': beam/structure function setup failed.")
    end if
  end subroutine integration_setup_beams

  subroutine integration_setup_qcd &
       (intg, lhapdf_status, pdf_builtin_status, os_data, var_list)
    type(integration_t), intent(inout) :: intg
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(pdf_builtin_status_t), intent(inout) :: pdf_builtin_status
    type(os_data_t), intent(in) :: os_data
    type(var_list_t), intent(in) :: var_list
    if (intg%alpha_s > 0)  call process_set_alpha_s (intg%process, intg%alpha_s)
    if (associated (intg%sf_list)) then
       call process_setup_qcd (intg%process, &
            lhapdf_status, pdf_builtin_status, &
            sf_list_get_lhapdf_data_ptr (intg%sf_list), &
            sf_list_get_pdf_builtin_data_ptr (intg%sf_list), &
            os_data, var_list)
    else
       call process_setup_qcd (intg%process, &
            lhapdf_status, pdf_builtin_status, &
            null (), null (), &
            os_data, var_list)
    end if
  end subroutine integration_setup_qcd

  subroutine integration_setup_phase_space (intg, os_data, ok)
    type(integration_t), intent(inout) :: intg
    type(os_data_t), intent(in) :: os_data
    logical, intent(out) :: ok
    if (intg%phs_filename == "") then
       call process_setup_phase_space (intg%process, &
            intg%rebuild_phs, &
            os_data, &
            intg%phs_par, intg%mapping_defaults, &
            filename_out = intg%phs_filename_out, &
            filename_vis = intg%phs_filename_vis, &
            vis_channels = intg%vis_channels, &
            check_phs_file = intg%check_phs_file, &
            ok = ok)
    else
       call process_setup_phase_space (intg%process, &
            intg%rebuild_phs, &
            os_data, &
            intg%phs_par, intg%mapping_defaults, &
            filename_in = intg%phs_filename, &
            filename_out = intg%phs_filename_out, &
            filename_vis = intg%phs_filename_vis, &
            vis_channels = intg%vis_channels, &
            ok = ok)
    end if       
    if (ok) then
       if (intg%phs_only) then
          call msg_message ("Process '" // char (intg%process_id) &
               // "': phase space setup complete.")
       end if
    else
       call msg_error ("Process '" // char (intg%process_id) &
            // "': phase space setup failed.")
    end if
  end subroutine integration_setup_phase_space

  subroutine integration_setup_strfun_mappings (intg)
    type(integration_t), intent(inout) :: intg
    if (intg%use_strfun) then
       call sf_list_setup_mappings (intg%sf_list, intg%process)
    end if
  end subroutine integration_setup_strfun_mappings

  subroutine integration_setup_iterations &
      (intg, it_list, it_list_default, ok, verbose)
    type(integration_t), intent(inout) :: intg
    type(iterations_list_t), intent(in) :: it_list
    type(iterations_list_t), dimension(:), intent(in) :: it_list_default
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    integer :: n_in, n_out
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    ok = .true.
    n_in  = process_get_n_in  (intg%process)
    n_out = process_get_n_out (intg%process)
    intg%it_list = it_list
    if (iterations_list_get_n_pass (intg%it_list) > 0) then
       call iterations_list_complete (intg%it_list, it_list_default(n_out))
    else
       select case (n_out)
       case (:1)
          call msg_error ("Integrate: number of outgoing particles " &
                  // "must be at least 2")
          ok = .false.
       case (2:ITERATIONS_DEFAULT_LIST_SIZE)
          intg%it_list = it_list_default(n_out + (n_in-2))
       case default
          intg%it_list = it_list_default(ITERATIONS_DEFAULT_LIST_SIZE)
       end select
    end if
    call iterations_list_adjust_n_calls (intg%it_list, &
         intg%process, intg%grid_parameters)
    if (verb)  call iterations_list_write (intg%it_list)
    call process_init_vamp_history &
         (intg%process, iterations_list_get_n_it (intg%it_list))
  end subroutine integration_setup_iterations

  subroutine integration_collect_md5sums (intg)
    type(integration_t), intent(inout) :: intg
    if (intg%use_beams) then
       intg%md5sum%beams = beam_data_get_md5sum (intg%beam_data, intg%sqrts)
    else
       intg%md5sum%beams = ""
    end if
    if (intg%use_strfun) then
       intg%md5sum%sf_list = sf_list_get_md5sum (intg%sf_list)
    else
       intg%md5sum%sf_list = ""
    end if
    intg%md5sum%mappings = mapping_defaults_md5sum (intg%mapping_defaults)
  end subroutine integration_collect_md5sums

  subroutine integration_setup_subevt (intg)
    type(integration_t), intent(inout) :: intg
    call process_setup_subevt (intg%process)
  end subroutine integration_setup_subevt

  subroutine integration_setup_cuts (intg, pn_cuts_lexpr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_cuts_lexpr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_cuts_lexpr)) then
       call process_setup_cuts (intg%process, pn_cuts_lexpr, &
            intg%md5sum%cuts)
       if (verb)  call msg_message ("Applying user-defined cuts.")
    else
       if (verb)  call msg_warning ("No cuts have been defined.")
    end if
  end subroutine integration_setup_cuts

  subroutine integration_setup_weight (intg, pn_weight_expr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_weight_expr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_weight_expr)) then
       call process_setup_weight (intg%process, pn_weight_expr, &
            intg%md5sum%weight)
       if (verb)  call msg_message ("Using user-defined reweighting factor.")
    end if
  end subroutine integration_setup_weight

  subroutine integration_setup_scale (intg, pn_scale_expr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_scale_expr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_scale_expr)) then
       call process_setup_scale (intg%process, pn_scale_expr, &
            intg%md5sum%scale)
       if (verb)  call msg_message ("Using user-defined general scale.")
    end if
  end subroutine integration_setup_scale

  subroutine integration_setup_fac_scale (intg, pn_scale_expr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_scale_expr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_scale_expr)) then
       call process_setup_fac_scale (intg%process, pn_scale_expr, &
            intg%md5sum%fac_scale)
       if (verb)  call msg_message ("Using user-defined factorization scale.")
    end if
  end subroutine integration_setup_fac_scale

  subroutine integration_setup_ren_scale (intg, pn_scale_expr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_scale_expr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_scale_expr)) then
       call process_setup_ren_scale (intg%process, pn_scale_expr, &
            intg%md5sum%ren_scale)
       if (verb)  call msg_message ("Using user-defined renormalization scale.")
    end if
  end subroutine integration_setup_ren_scale

  subroutine integration_setup_grids (intg, verbose)
    type(integration_t), intent(inout) :: intg
    logical, intent(in), optional :: verbose
    integer :: n_calls
    logical :: verb, ok
    verb = .true.;  if (present (verbose))  verb = verbose
    call process_store_iteration_parameters (intg%process, &
         iterations_list_get_pass_array (intg%it_list), &
         iterations_list_get_n_calls_array (intg%it_list))
    if (iterations_list_get_n_pass (intg%it_list) > 0) then
       n_calls = iterations_list_get_n_calls (intg%it_list, 1)
       if (.not. intg%rebuild_grids) then
          call process_read_grid_file (intg%process, intg%grids_filename, &
               intg%check_grid_file, intg%md5sum, intg%grid_parameters, &
               iterations_list_get_pass_array (intg%it_list), &
               iterations_list_get_n_calls_array (intg%it_list),&
               ok)
          intg%rebuild_grids = .not. ok
       end if
       if (intg%rebuild_grids) then
          call process_setup_grids &
               (intg%process, intg%grid_parameters, calls=n_calls)
       end if
       if (verb) then
          write (msg_buffer, "(4(I0,A),A,L1)")  &
               n_calls, " calls, ", &
               process_get_n_channels (intg%process), " channels, ", &
               process_get_n_parameters (intg%process), " dimensions, ", &
               process_get_n_bins (intg%process), " bins, ", &
               "stratified = ", intg%grid_parameters%stratified
          call msg_message ()
       end if
       intg%pass_on_file = process_get_current_pass (intg%process)
       intg%it_on_file = process_get_current_it (intg%process)
     end if
  end subroutine integration_setup_grids

  subroutine integration_write_header (intg, verbose)
    type(integration_t), intent(inout) :: intg
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: u
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       call msg_message ("Integrating process '" &
            // char (intg%process_id) // "':")
       u = logfile_unit ()
       call process_results_write_header (intg%process, logfile=.false.)
       if (u > 0) then
          call process_results_write_header (intg%process, unit=u)
          flush (u)
       end if
    end if
  end subroutine integration_write_header

  subroutine integration_warmup (intg, rng, pass, verbose)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: pass
    logical, intent(in), optional :: verbose
    integer :: n_it, n_calls, i, u
    logical :: iteration_is_on_file, adapt_grids, adapt_weights
    logical :: verb 
    real(default) :: last_accuracy, current_accuracy
    real(default) :: last_abs_error, current_abs_error
    real(default) :: last_rel_error, current_rel_error
    logical :: accuracy_reached, abs_error_reached, rel_error_reached
    logical :: goal_set
    verb = .true.;  if (present (verbose))  verb = verbose
    u = logfile_unit ()
    intg%pass = pass
    n_calls = iterations_list_get_n_calls (intg%it_list, intg%pass)
    if (iterations_list_has_custom_adaptation (intg%it_list, intg%pass)) then
       adapt_grids = iterations_list_adapt_grids (intg%it_list, intg%pass)
       adapt_weights = iterations_list_adapt_weights (intg%it_list, intg%pass)
    else
       adapt_grids = .true.
       adapt_weights = .true.
    end if
    last_accuracy = 0
    last_abs_error = 0
    last_rel_error = 0
    goal_set = intg%accuracy_goal > 0 &
         .or. intg%abs_error_goal > 0 .or. intg%rel_error_goal > 0
    accuracy_reached = .false.
    abs_error_reached = .false.
    rel_error_reached = .false.
    n_it = iterations_list_get_n_it (intg%it_list, intg%pass)
    LOOP_IT: do i = 1, n_it
       intg%it = intg%it + 1
       iteration_is_on_file = intg%pass < intg%pass_on_file &
            .or. intg%pass == intg%pass_on_file .and. i <= intg%it_on_file
       if (iteration_is_on_file) then
          current_accuracy =  process_get_accuracy (intg%process, it=intg%it)
          current_abs_error =  process_get_error (intg%process, it=intg%it)
          current_rel_error =  process_get_rel_error (intg%process, it=intg%it)
          if (current_accuracy == 0) then
             if (.not. goal_set .or. &
                  intg%accuracy_goal > 0 .and. .not. accuracy_reached .or. &
                  intg%abs_error_goal > 0 .and. .not. abs_error_reached .or. &
                  intg%rel_error_goal > 0 .and. .not. rel_error_reached) then
                call process_discard_results (intg%process, intg%it)
                intg%pass_on_file = process_get_current_pass (intg%process)
                intg%it_on_file = process_get_current_it (intg%process)
                iteration_is_on_file = .false.
             else
                call msg_message &
                     ("Accuracy/error goals reached, skipped iterations")
                intg%it = intg%it + n_it - i
                exit LOOP_IT
             end if
          end if
       end if
       if (iteration_is_on_file) then
          if (verb) then
             call process_results_write_entry (intg%process, intg%it)
             if (u > 0) then
                call process_results_write_entry (intg%process, intg%it, unit=u)
                flush (u)
             end if
          end if
       else
          if (.not. goal_set .or. &
               intg%accuracy_goal > 0 .and. .not. accuracy_reached .or. &
               intg%abs_error_goal > 0 .and. .not. abs_error_reached .or. &
               intg%rel_error_goal > 0 .and. .not. rel_error_reached) then
             call process_integrate (intg%process, rng, &
                  intg%grid_parameters, &
                  intg%pass, 1, 1, n_calls, &
                  discard_integrals = i==1, &
                  adapt_grids = adapt_grids, &
                  adapt_weights = adapt_weights .and. i>2, &
                  print_current = verb, &
                  time_estimate = intg%time_estimate, &
                  grids_filename = intg%grids_filename, &
                  write_best_grid = intg%use_best_grid, &
                  md5sum = intg%md5sum, &
                  history_filename = intg%history_filename, &
                  log_filename = intg%log_filename)
          else
             call msg_message &
                  ("Accuracy/error goals reached, skipping iterations")
             call process_skip_iterations &
                  (intg%process, pass, intg%it - 1, n_it - i + 1)
             intg%it = intg%it + n_it - i
             exit LOOP_IT
          end if
          current_accuracy = process_get_accuracy (intg%process, it=intg%it)
          current_abs_error = process_get_error (intg%process, it=intg%it)
          current_rel_error = process_get_rel_error (intg%process, it=intg%it)
       end if
       last_accuracy = current_accuracy
       last_abs_error = current_abs_error
       last_rel_error = current_rel_error
       accuracy_reached = last_accuracy < intg%accuracy_goal
       abs_error_reached = last_abs_error < intg%abs_error_goal
       rel_error_reached = last_rel_error < intg%rel_error_goal
    end do LOOP_IT
    if (verb) then
       call process_results_write_average (intg%process, intg%pass)
       if (u > 0) then
          call process_results_write_average (intg%process, intg%pass, unit=u)
          flush (u)
       end if
    end if
  end subroutine integration_warmup

  subroutine integration_evaluate &
       (intg, rng, pass, global_var_list, os_data, verbose)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: pass
    type(var_list_t), intent(inout) :: global_var_list
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: verbose
    integer :: it_on_file, n_calls, n_it, i, u
    logical :: adapt_grids, adapt_weights
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    u = logfile_unit ()
    intg%pass = pass
    if (intg%pass == intg%pass_on_file) then
       it_on_file = intg%it_on_file
    else
       it_on_file = 0
    end if
    n_calls = iterations_list_get_n_calls (intg%it_list, intg%pass)
    n_it = iterations_list_get_n_it (intg%it_list, intg%pass)
    if (iterations_list_has_custom_adaptation (intg%it_list, intg%pass)) then
       adapt_grids = iterations_list_adapt_grids (intg%it_list, intg%pass)
       adapt_weights = iterations_list_adapt_weights (intg%it_list, intg%pass)
    else
       adapt_grids = .true.
       adapt_weights = .false.
    end if
    do i = 1, it_on_file
       intg%it = intg%it + 1
       if (verb) then
          call process_results_write_entry (intg%process, intg%it)
          if (u > 0) then
             call process_results_write_entry (intg%process, intg%it, unit=u)
             flush (u)
          end if
       end if
    end do
    call process_integrate (intg%process, rng, &
         intg%grid_parameters, &
         intg%pass, it_on_file + 1, n_it, n_calls, &
         discard_integrals = .true., &
         adapt_grids = adapt_grids, &
         adapt_weights = adapt_weights, &
         print_current = verb, &
         time_estimate = intg%time_estimate, &
         grids_filename = intg%grids_filename, &
         write_best_grid = intg%use_best_grid, &
         md5sum = intg%md5sum, &
         history_filename = intg%history_filename, &
         log_filename = intg%log_filename)
    if (verb) then
       call process_results_write_average (intg%process, intg%pass)
       if (u > 0) then
          call process_results_write_average (intg%process, intg%pass, unit=u)
          flush (u)
       end if
    end if
    if (intg%vis_history) then
       call process_display_integration_history &
            (intg%process, intg%history_filename, os_data)
    end if
    call process_record_integral (intg%process, global_var_list)
  end subroutine integration_evaluate

  subroutine integration_write_footer (intg, verbose)
    type(integration_t), intent(in) :: intg
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: u
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       u = logfile_unit ()
       call process_results_write_footer (intg%process)
       if (u > 0) then
          call process_results_write_footer (intg%process, unit=u)
          flush (u)
       end if
       if (intg%time_estimate .and. intg%rebuild_grids) &
             call process_write_time_estimate (intg%process)
    end if
  end subroutine integration_write_footer

  subroutine integration_init0 &
      (intg, process_id, global, ok, me_only, no_beams, verbose)
    type(integration_t), intent(out) :: intg
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(out) :: ok
    logical, intent(in), optional :: me_only, no_beams, verbose
    logical :: integrate, allow_beams
    integrate = .true.;  if (present (me_only))  integrate = .not. me_only
    allow_beams = .true.;  if (present (no_beams)) allow_beams = .not. no_beams
    call integration_basic_init (intg, process_id, global%var_list, verbose)
    if (allow_beams)  call integration_check_beam_data (intg, global%beam_data)
    call maybe_compile_library (global)
    call integration_init_process (intg, &
         global%prc_lib, global%model, global%var_list, ok)
    if (ok) then
       call integration_setup_beams &
            (intg, global%sf_list, global%var_list, ok)
    end if
    if (ok) then
       call integration_setup_qcd &
            (intg, global%lhapdf_status, global%pdf_builtin_status, &
             global%os_data, global%var_list)
    end if
    if (integrate .and. ok) then
       call integration_setup_phase_space (intg, global%os_data, ok)
    end if
    if (integrate .and. ok) then
       call integration_setup_strfun_mappings (intg)
    end if
    if (.not. intg%phs_only) then
       if (integrate .and. ok) then
          call integration_collect_md5sums (intg)
          call integration_setup_iterations &
               (intg, global%it_list, global%it_list_default, ok, verbose)
       end if
       if (ok) then
          call integration_setup_subevt (intg)
          call integration_setup_cuts (intg, global%pn_cuts_lexpr, verbose)
          call integration_setup_scale (intg, global%pn_scale_expr, verbose)      
          call integration_setup_fac_scale (intg, global%pn_fac_scale_expr, verbose)
          call integration_setup_ren_scale (intg, global%pn_ren_scale_expr, verbose)      
          call integration_setup_weight (intg, global%pn_weight_expr, verbose)
       end if
       if (integrate .and. ok) then
          call integration_setup_grids (intg)
       end if
    end if
  end subroutine integration_init0

  subroutine integration_init1 &
       (intg, process_id, global, ok, no_beams, me_only, verbose)
    type(integration_t), dimension(:), intent(out) :: intg
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(out) :: ok
    logical, intent(in), optional :: no_beams, me_only, verbose
    integer :: proc
    do proc = 1, size (intg)
       call integration_init0 &
            (intg(proc), process_id(proc), global, ok, me_only, verbose)
       if (.not. ok)  exit
    end do
  end subroutine integration_init1

  subroutine integration_integrate0 &
       (intg, rng, global_var_list, os_data, verbose)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout) :: global_var_list
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: verbose
    integer :: pass
    call openmp_set_num_threads_verbose &
       (var_list_get_ival (global_var_list, var_str ("openmp_num_threads")))
    call integration_write_header (intg, verbose)
    do pass = 1, iterations_list_get_n_pass (intg%it_list) - 1
       call integration_warmup (intg, rng, pass, verbose)
    end do
    if (intg%use_best_grid) &
         call process_choose_best_grid (intg%process, intg%check_grid_file)
    call integration_evaluate &
         (intg, rng, pass, global_var_list, os_data, verbose)
    call integration_write_footer (intg, verbose)
  end subroutine integration_integrate0

  subroutine integration_integrate1 &
      (intg, rng, global_var_list, os_data, verbose)
    type(integration_t), dimension(:), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout) :: global_var_list
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: verbose
    integer :: proc
    do proc = 1, size (intg)
       call integration_integrate0 &
            (intg(proc), rng, global_var_list, os_data, verbose)
    end do
  end subroutine integration_integrate1

  subroutine integration_integrate_dummy0 (intg, global_var_list, verbose)
    type(integration_t), intent(inout) :: intg
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: verbose
    call integration_write_header (intg, verbose)
    call process_do_dummy_integration (intg%process)
    call integration_write_footer (intg, verbose)
    call process_record_integral (intg%process, global_var_list)
  end subroutine integration_integrate_dummy0
     
  subroutine integration_integrate_dummy1 (intg, global_var_list, verbose)
    type(integration_t), dimension(:), intent(inout) :: intg
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: verbose
    integer :: proc
    do proc = 1, size (intg)
       call integration_integrate_dummy0 (intg(proc), global_var_list, verbose)
    end do
  end subroutine integration_integrate_dummy1

  subroutine integration_me_test (intg, rng, global_var_list, os_data)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout) :: global_var_list
    type(os_data_t), intent(in) :: os_data
    integer :: openmp_num_threads
    real(default) :: time_in_seconds, time_per_call, sample_function_sum
    openmp_num_threads = &
         var_list_get_ival (global_var_list, var_str ("openmp_num_threads"))
    call openmp_set_num_threads_verbose (openmp_num_threads)
    write (msg_buffer, "(A,1x,I0,1x,A)")  "Matrix element test: " &
         // "Calling the sampling function", &
         intg%n_events_for_me_test, "times ..."
    call msg_message ()
    call process_me_test (intg%process, rng, intg%n_events_for_me_test, &
         time_in_seconds, sample_function_sum)
    call msg_message ("... test finished.")
    call process_status_write_counters (process_get_status (intg%process))
    write (msg_buffer, "(A,1PG22.15)")  "Matrix element test: " &
         // "Sample function sum:         ", sample_function_sum
    call msg_message ()
    write (msg_buffer, "(A,1PG12.5)")  "Matrix element test: " &
         // "Time in seconds (wallclock): ", time_in_seconds
    call msg_message ()
    if (intg%n_events_for_me_test /= 0) then
       time_per_call = time_in_seconds / intg%n_events_for_me_test
       write (msg_buffer, "(A,1PG12.5)")  "Matrix element test: " &
            // "Time per call in seconds:    ", time_per_call
       call msg_message ()
       if (openmp_num_threads /= 0) then
          write (msg_buffer, "(A,1PG12.5)")  "Matrix element test: " &
               // "Time times number of threads:", &
               time_per_call * openmp_num_threads
          call msg_message ()
       end if
    end if
  end subroutine integration_me_test

  subroutine prepare_me_evaluation0 (process_id, global, verbose)
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: verbose
    type(integration_t) :: intg
    logical :: ok
    call integration_init &
         (intg, process_id, global, ok, me_only = .true., verbose = verbose)
  end subroutine prepare_me_evaluation0

  subroutine prepare_me_evaluation1 (process_id, global, verbose)
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: verbose
    integer :: proc
    do proc = 1, size (process_id)
       call prepare_me_evaluation0 (process_id(proc), global, verbose)
    end do
  end subroutine prepare_me_evaluation1

  subroutine prepare_me_missing_processes &
      (process_id, global, verbose)
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: verbose
    integer :: n_proc, n_missing, proc
    type(process_t), pointer :: process
    type(string_t), dimension(:), allocatable :: process_id_missing
    logical, dimension(:), allocatable :: missing
    n_proc = size (process_id)
    allocate (missing (n_proc))
    do proc = 1, n_proc
       process => process_store_get_process_ptr (process_id(proc))
       missing(proc) = .not. associated (process)       
    end do
    n_missing = count (missing)
    if (n_missing > 0) then
       allocate (process_id_missing (n_missing))
       process_id_missing = pack (process_id, missing)
       call prepare_me_evaluation (process_id_missing, global, verbose)
    end if
  end subroutine prepare_me_missing_processes

  subroutine integrate_process0 &
      (process_id, global, global_var_list, no_beams, verbose)
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: no_beams, verbose
    type(integration_t) :: intg
    logical :: ok
    call integration_init &
         (intg, process_id, global, ok, no_beams=no_beams, verbose=verbose)
    if (.not. intg%phs_only) then
       if (ok) then
          call integration_integrate &
               (intg, global%rng, global_var_list, global%os_data, verbose)
       else
          call integration_integrate_dummy (intg, global_var_list, verbose)
       end if
    end if
  end subroutine integrate_process0

  subroutine integrate_process1 &
       (process_id, global, global_var_list, no_beams, verbose)
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: no_beams, verbose
    integer :: proc
    do proc = 1, size (process_id)
       call integrate_process0 &
            (process_id(proc), global, global_var_list, no_beams, verbose)
    end do
  end subroutine integrate_process1

  subroutine me_test_process (process_id, global, global_var_list)
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    type(integration_t) :: intg
    logical :: ok
    call integration_init (intg, process_id, global, ok)
    if (ok) then
       call integration_me_test &
            (intg, global%rng, global_var_list, global%os_data)
    else
       call msg_error ("Matrix element test fails " &
            // "because process has no matrix element")
    end if
  end subroutine me_test_process

  subroutine integrate_missing_processes &
      (process_id, global, global_var_list, no_beams, verbose)
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: no_beams, verbose
    integer :: n_proc, n_missing, proc
    type(process_t), pointer :: process
    type(string_t), dimension(:), allocatable :: process_id_missing
    logical, dimension(:), allocatable :: missing
    type(string_t) :: prc_string
    logical :: verb, nobeams
    verb = .false.;  if (present (verbose))  verb = verbose
    nobeams = .false.;  if (present (no_beams))  nobeams = no_beams
    n_proc = size (process_id)
    allocate (missing (n_proc))
    do proc = 1, n_proc
       process => process_store_get_process_ptr (process_id(proc))
       if (associated (process)) then
          if (process_has_integral (process)) then
             if (nobeams .and. process_uses_beams (process)) then
                call msg_warning ("Discarding previous result for process '" &
                     // char (process_id(proc)) // "': no beam setup allowed")
                missing(proc) = .true.
             else
                missing(proc) = .false.
             end if
          else
             missing(proc) = .false.
          end if
       else
          missing(proc) = .true.
       end if
    end do
    n_missing = count (missing)
    if (n_missing > 0) then
       allocate (process_id_missing (n_missing))
       process_id_missing = pack (process_id, missing)
       if (verb) then
          prc_string = process_id_missing(1)
          do proc = 2, n_missing
             prc_string = prc_string // ", " // process_id_missing(proc)
          end do
          call msg_message ("Integrating missing processes: " &
               // char (prc_string))
       end if
       if (var_list_get_lval (global%var_list, var_str ("?phs_only"))) then
          call msg_fatal &
               ("Computing missing integrals: ?phs_only must not be set")
       else
          call integrate_process &
               (process_id_missing, global, global_var_list, no_beams, verbose)
       end if
       if (verb) then
          call msg_message ("Integration of missing processes complete.")
       end if
    end if
  end subroutine integrate_missing_processes


end module integrations
