! WHIZARD 2.0.3 Tue Aug 10 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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
  public :: integrate_missing_processes

  type :: integration_t
    private
    type(string_t) :: process_id
    type(process_t), pointer :: process => null ()
    logical :: rebuild_phs = .false.
    type(string_t) :: phs_filename
    logical :: phs_only = .false.
    logical :: vis_channels = .false.
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    logical :: rebuild_grids = .false.
    logical :: adapt_final_grids = .false.
    logical :: adapt_final_weights = .false.
    type(grid_parameters_t) :: grid_parameters
    type(string_t) :: grids_filename
    logical :: helicity_selection_active = .false.
    real(default) :: helicity_selection_threshold = -1
    integer :: helicity_selection_cutoff = 1000
    real(default) :: sqrts = -1
    real(default) :: alpha_s = -1
    logical :: time_estimate = .false.
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
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb) then
       call msg_message ("Initializating integration for process " &
            // char (intg%process_id) // ":")
    end if
    intg%rebuild_phs = &
         var_list_get_lval (var_list, var_str ("?rebuild_phase_space"))
    intg%phs_filename = &
         var_list_get_sval (var_list, var_str ("$phs_file"))   ! $ sign
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
    intg%phs_par%t_channel = &
         var_list_get_ival (var_list, var_str ("phs_t_channel"))
    intg%mapping_defaults%energy_scale = &
         var_list_get_rval (var_list, var_str ("phs_e_scale"))
    intg%mapping_defaults%invariant_mass_scale = &
         var_list_get_rval (var_list, var_str ("phs_m_scale"))
    intg%mapping_defaults%momentum_transfer_scale = &
         var_list_get_rval (var_list, var_str ("phs_q_scale"))
    intg%rebuild_grids = &
         var_list_get_lval (var_list, var_str ("?rebuild_grids"))
    intg%adapt_final_grids = &
         var_list_get_lval (var_list, var_str ("?adapt_final_grids"))
    intg%adapt_final_weights = &
         var_list_get_lval (var_list, var_str ("?adapt_final_weights"))
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
    intg%grids_filename = intg%process_id // ".vg"
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
    intg%sqrts = &
         var_list_get_rval (var_list, var_str ("sqrts"))
    intg%time_estimate = &
         var_list_get_lval (var_list, var_str ("?time_estimate"))
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

  subroutine integration_init_process &
      (intg, prc_lib, model, lhapdf_status, var_list, ok)
    type(integration_t), intent(inout) :: intg
    type(process_library_t), intent(inout), target :: prc_lib
    type(model_t), intent(in), target :: model
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(var_list_t), intent(in), target :: var_list
    logical, intent(out) :: ok
    call process_store_init_process (intg%process, &
         prc_lib, intg%process_id, model, lhapdf_status, var_list, &
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
    if (ok .and. intg%alpha_s > 0) &
       call process_set_alpha_s (intg%process, intg%alpha_s)
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
                  sf_list_get_n_strfun (intg%sf_list), &
                  sf_list_get_n_mapping (intg%sf_list))
             call process_check_beam_setup (intg%process, var_list)
             call sf_list_transfer_to_process (intg%sf_list, intg%process)
             intg%use_strfun = .true.
          else
             call process_setup_beams (intg%process, intg%beam_data, 0, 0)
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
    else
       call process_setup_beams &
            (intg%process, intg%beam_data, 0, 0, sqrts = intg%sqrts)
    end if
    if (ok)  call process_connect_strfun (intg%process, ok)
    if (.not. ok) then
       call msg_error ("Process '" // char (intg%process_id) &
            // "': beam/structure function setup failed.")
    end if
  end subroutine integration_setup_beams

  subroutine integration_setup_phase_space (intg, os_data, ok)
    type(integration_t), intent(inout) :: intg
    type(os_data_t), intent(in) :: os_data
    logical, intent(out) :: ok
    type(string_t) :: filename_out, filename_vis
    filename_out = intg%process_id // ".phs"
    filename_vis = intg%process_id // "_phs"
    if (intg%phs_filename == "") then
       call process_setup_phase_space (intg%process, &
            intg%rebuild_phs, &
       os_data, &
            intg%phs_par, intg%mapping_defaults, &
            filename_out = filename_out, &
       filename_vis = filename_vis, &
            vis_channels = intg%vis_channels, &
            ok = ok)
    else
       call process_setup_phase_space (intg%process, &
            intg%rebuild_phs, &
            os_data, &
            intg%phs_par, intg%mapping_defaults, &
            filename_in = intg%phs_filename, &
            filename_out = filename_out, &
       filename_vis = filename_vis, &
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

  subroutine integration_setup_cuts (intg, pn_cuts_lexpr, verbose)
    type(integration_t), intent(inout) :: intg
    type(parse_node_t), pointer :: pn_cuts_lexpr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (pn_cuts_lexpr)) then
       call process_setup_cuts (intg%process, pn_cuts_lexpr)
       intg%md5sum%cuts = parse_node_get_md5sum (pn_cuts_lexpr)
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
       call process_setup_weight (intg%process, pn_weight_expr)
       intg%md5sum%weight = parse_node_get_md5sum (pn_weight_expr)
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
       call process_setup_scale (intg%process, pn_scale_expr)
       intg%md5sum%scale = parse_node_get_md5sum (pn_scale_expr)
       if (verb)  call msg_message ("Using user-defined event scale.")
    end if
  end subroutine integration_setup_scale

  subroutine integration_setup_grids (intg, verbose)
    type(integration_t), intent(inout) :: intg
    logical, intent(in), optional :: verbose
    integer :: n_calls
    logical :: verb, ok
    verb = .true.;  if (present (verbose))  verb = verbose
    if (iterations_list_get_n_pass (intg%it_list) > 0) then
       n_calls = iterations_list_get_n_calls (intg%it_list, 1)
       if (.not. intg%rebuild_grids) then
          call process_read_grid_file (intg%process, &
               intg%grids_filename, intg%md5sum, intg%grid_parameters, &
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
    integer :: n_calls, i, u
    logical :: verb, iteration_is_on_file
    verb = .true.;  if (present (verbose))  verb = verbose
    u = logfile_unit ()
    intg%pass = pass
    n_calls = iterations_list_get_n_calls (intg%it_list, intg%pass)
    LOOP_IT: do i = 1, iterations_list_get_n_it (intg%it_list, intg%pass)
       intg%it = intg%it + 1
       iteration_is_on_file = intg%pass < intg%pass_on_file &
            .or. intg%pass == intg%pass_on_file .and. i <= intg%it_on_file
       if (iteration_is_on_file) then
          if (verb) then
             call process_results_write_entry (intg%process, intg%it)
             if (u > 0) then
                call process_results_write_entry (intg%process, intg%it, unit=u)
                flush (u)
             end if
          end if
       else
          call process_integrate (intg%process, rng, &
               intg%grid_parameters, &
               intg%pass, 1, 1, n_calls, &
               discard_integrals = i==1, &
               adapt_grids = .true., &
               adapt_weights = i>2, &
               print_current = verb, &
               time_estimate = intg%time_estimate, &
               grids_filename = intg%grids_filename, &
               md5sum = intg%md5sum)
       end if
    end do LOOP_IT
    if (verb) then
       call process_results_write_average (intg%process, intg%pass)
       if (u > 0) then
          call process_results_write_average (intg%process, intg%pass, unit=u)
          flush (u)
       end if
    end if
    call process_write_logfile (intg%process)
  end subroutine integration_warmup

  subroutine integration_evaluate (intg, rng, pass, global_var_list, verbose)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: pass
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: verbose
    integer :: it_on_file, n_calls, n_it, i, u
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
         adapt_grids = intg%adapt_final_grids, &
         adapt_weights = intg%adapt_final_weights, &
         print_current = verb, &
         time_estimate = intg%time_estimate, &
         grids_filename = intg%grids_filename, &
         md5sum = intg%md5sum)
    if (verb) then
       call process_results_write_average (intg%process, intg%pass)
       if (u > 0) then
          call process_results_write_average (intg%process, intg%pass, unit=u)
          flush (u)
       end if
    end if
    call process_record_integral (intg%process, global_var_list)
    call process_write_logfile (intg%process)
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
         global%prc_lib, global%model, global%lhapdf_status, &
         global%var_list, ok)
    if (ok) then
       call integration_setup_beams &
            (intg, global%sf_list, global%var_list, ok)
    end if
    if (integrate .and. ok) then
       call integration_setup_phase_space (intg, global%os_data, ok)
    end if
    if (.not. intg%phs_only) then
       if (integrate .and. ok) then
          call integration_collect_md5sums (intg)
          call integration_setup_iterations &
               (intg, global%it_list, global%it_list_default, ok, verbose)
       end if
       if (ok) then
          call integration_setup_cuts (intg, global%pn_cuts_lexpr, verbose)
          call integration_setup_scale (intg, global%pn_scale_expr, verbose)
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

  subroutine integration_integrate0 (intg, rng, global_var_list, verbose)
    type(integration_t), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: verbose
    integer :: pass
    call integration_write_header (intg, verbose)
    do pass = 1, iterations_list_get_n_pass (intg%it_list) - 1
       call integration_warmup (intg, rng, pass, verbose)
    end do
    call integration_evaluate (intg, rng, pass, global_var_list, verbose)
    call integration_write_footer (intg, verbose)
  end subroutine integration_integrate0

  subroutine integration_integrate1 (intg, rng, global_var_list, verbose)
    type(integration_t), dimension(:), intent(inout) :: intg
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: verbose
    integer :: proc
    do proc = 1, size (intg)
       call integration_integrate0 (intg(proc), rng, global_var_list, verbose)
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
               (intg, global%rng, global_var_list, verbose)
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
