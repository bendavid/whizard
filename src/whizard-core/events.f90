! WHIZARD 2.0.1 Sun Apr 25 2010
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

module events

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: RAW_EVENT_FILE_VERSION !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use os_interface
  use lexers
  use parser
  use prt_lists
  use expressions
  use models
  use flavors
  use state_matrices
  use polarizations
  use event_formats
  use hepmc_interface
  use particles
  use interactions
  use evaluators
  use process_libraries
  use beams
  use sf_lhapdf
  use mappings
  use phs_forests
  use cascades
  use processes
  use decays

  implicit none
  private

  public :: event_t
  public :: event_init
  public :: event_final
  public :: event_write
  public :: event_generate
  public :: event_do_analysis
  public :: md5sum_events_t
  public :: raw_event_file_write_header
  public :: raw_event_file_read_header
  public :: event_write_raw
  public :: event_read_raw
  public :: event_read_from_hepmc
  public :: event_write_to_hepmc
  public :: event_write_to_hepeup
  public :: event_write_to_hepevt
  public :: event_test

  type :: event_t
     type(process_t), pointer :: process => null ()
     type(decay_tree_t), pointer :: decay_tree => null ()
     logical :: particle_set_exists = .false.
     type(particle_set_t) :: particle_set
     real(default), pointer :: weight => null ()
     real(default), pointer :: sqme => null ()
     real(default) :: excess = 0
  end type event_t

  type :: md5sum_events_t
    character(32), dimension(:), allocatable :: process
    character(32), dimension(:), allocatable :: parameters
    character(32), dimension(:), allocatable :: results
    character(32) :: decays
    character(32) :: simulation
  end type md5sum_events_t


contains

  subroutine event_init (event, process, event_weight, event_sqme, decay_tree)
    type(event_t), intent(out) :: event
    type(process_t), intent(in), target :: process
    real(default), intent(in), target :: event_weight, event_sqme
    type(decay_tree_t), intent(in), optional, target :: decay_tree
    event%process => process
    event%weight => event_weight
    event%sqme => event_sqme
    if (present (decay_tree))  event%decay_tree => decay_tree
  end subroutine event_init

  subroutine event_final (event)
    type(event_t), intent(inout) :: event
    call particle_set_final (event%particle_set)
  end subroutine event_final

  subroutine event_write (event, unit, verbose)
    type(event_t), intent(in) :: event
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) repeat ("=", 72)
    write (u, *) "Event record:"
    if (associated (event%process)) then
       if (present (verbose)) then
          if (verbose) then
             call process_write (event%process, unit)
             write (u, *)
          end if
       end if
    else
       write (u, *) "  [empty]"
    end if
    write (u, *) repeat ("=", 72)
    if (associated (event%decay_tree)) then
       write (u, *) repeat ("=", 72)
       call decay_tree_write (event%decay_tree, unit)
    end if
    write (u, *) "  [Process: ", char (process_get_id (event%process)), "]"
    write (u, *)
    call particle_set_write (event%particle_set, unit)
    write (u, *)
    if (associated (event%weight)) then
       write (u, *) "Event weight  =", event%weight
    else
       write (u, *) "Event weight  = [undefined]"
    end if
    write (u, *) "Excess weight =", event%excess
    write (u, *) repeat ("=", 72)
  end subroutine event_write

  subroutine event_generate (event, rng, unweighted, &
       factorization_mode, keep_correlations, keep_virtual)
    type(event_t), intent(inout), target :: event
    type(tao_random_state), intent(inout) :: rng
    logical, intent(in) :: unweighted
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations, keep_virtual
    call event_discard_particle_set (event)
    if (unweighted) then
       call process_generate_unweighted_event (event%process, rng, event%excess)
       event%weight = 1
    else
       call process_generate_weighted_event (event%process, rng, event%weight)
    end if
    event%sqme = process_get_sqme (event%process)
    if (associated (event%decay_tree)) then
       call decay_tree_generate_event (event%decay_tree, rng)
       call event_factorize_process (event, rng, &
            factorization_mode, keep_correlations, keep_virtual)
    end if
  end subroutine event_generate

  subroutine event_factorize_process (event, rng, &
       factorization_mode, keep_correlations, keep_virtual)
    type(event_t), intent(inout), target :: event
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations, keep_virtual
    type(interaction_t), pointer :: int_sqme, int_flows
    real(default), dimension(2) :: r
    integer, dimension(:), allocatable :: beam_index
    integer, dimension(:), allocatable :: incoming_parton_index
    int_sqme => evaluator_get_int_ptr &
         (decay_tree_get_eval_sqme_ptr  (event%decay_tree))
    int_flows => evaluator_get_int_ptr &
         (decay_tree_get_eval_flows_ptr (event%decay_tree))
    call tao_random_number (rng, r)
    if (interaction_get_n_in (int_sqme) /= 0) then
       call particle_set_init (event%particle_set, &
            int_sqme, int_flows, factorization_mode, r, &
            keep_correlations, keep_virtual)
    else
       call particle_set_init (event%particle_set, &
            int_sqme, int_flows, factorization_mode, r, &
            keep_correlations, keep_virtual, &
            n_incoming = process_get_n_in (event%process))
    end if
    call process_get_beam_index (event%process, beam_index)
    if (allocated (beam_index)) then
       call particle_set_reset_status (event%particle_set, &
            beam_index, PRT_BEAM)
    end if
    call process_get_incoming_parton_index (event%process, &
         incoming_parton_index)
    if (allocated (incoming_parton_index)) then
       call particle_set_reset_status (event%particle_set, &
            incoming_parton_index, PRT_INCOMING)
    end if
    event%particle_set_exists = .true.
  end subroutine event_factorize_process
    
  subroutine event_do_analysis (event, prt_list, analysis_expr)
    type(event_t), intent(inout), target :: event
    type(prt_list_t), intent(inout), target :: prt_list
    type(eval_tree_t), intent(inout), target :: analysis_expr
    if (eval_tree_is_defined (analysis_expr)) then
       call particle_set_to_prt_list (event%particle_set, prt_list)
       call eval_tree_evaluate (analysis_expr)
    end if
  end subroutine event_do_analysis

  subroutine event_discard_particle_set (event)
    type(event_t), intent(inout), target :: event
    if (event%particle_set_exists) then
       call particle_set_final (event%particle_set)
       event%particle_set_exists = .false.
    end if
  end subroutine event_discard_particle_set

  subroutine raw_event_file_write_header (unit, md5sum)
    integer, intent(in) :: unit
    type(md5sum_events_t), intent(in) :: md5sum
    write (unit)  RAW_EVENT_FILE_VERSION
    write (unit)  size (md5sum%process)
    write (unit)  md5sum%process
    write (unit)  md5sum%parameters
    write (unit)  md5sum%results
    write (unit)  md5sum%decays
    write (unit)  md5sum%simulation
  end subroutine raw_event_file_write_header

  subroutine raw_event_file_read_header (unit, md5sum, ok, iostat)
    integer, intent(in) :: unit
    type(md5sum_events_t), intent(in) :: md5sum
    logical, intent(out) :: ok
    integer, intent(out), optional :: iostat
    integer :: version, n
    character(32), dimension(:), allocatable :: md5sum_array
    character(32) :: md5sum_single
    logical :: unweighted
    ok = .false.
    read (unit, iostat=iostat)  version
    if (version /= RAW_EVENT_FILE_VERSION) then
       call msg_message &
            ("Event-file format version has changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  n
    if (n /= size (md5sum%process)) then
       call msg_message &
            ("Process number has changed, discarding old event file")
       return
    end if
    allocate (md5sum_array (n))
    read (unit, iostat=iostat)  md5sum_array
    if (any (md5sum%process /= md5sum_array)) then
       call msg_message &
            ("Process configuration has changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_array
    if (any (md5sum%parameters /= md5sum_array)) then
       call msg_message &
            ("Model parameters have changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_array
    if (any (md5sum%results /= md5sum_array)) then
       call msg_message &
            ("Integration results have changed, skipping event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_single
    if (md5sum%decays /= md5sum_single) then
       call msg_message &
            ("Decay configuration has changed, skipping event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_single
    if (md5sum%simulation /= md5sum_single) then
       call msg_message &
            ("Simulation parameters have changed, skipping event file")
       return
    end if
    ok = .true.
  end subroutine raw_event_file_read_header

  subroutine event_write_raw (event, unit)
    type(event_t), intent(in) :: event
    integer, intent(in) :: unit
    if (associated (event%process)) then
       write (unit)  process_get_store_index (event%process)
       write (unit)  process_get_scale (event%process)
       write (unit)  process_get_alpha_s (event%process)
       write (unit)  process_get_sqme (event%process)
    else
       write (unit)  0
       write (unit)  0._default
       write (unit)  0._default
       write (unit)  0._default
    end if
    call particle_set_write_raw (event%particle_set, unit)
    write (unit)  event%weight, event%excess
  end subroutine event_write_raw

  subroutine event_read_raw (event, unit, event_weight, event_sqme, iostat)
    type(event_t), intent(out) :: event
    integer, intent(in) :: unit
    real(default), intent(inout), target :: event_weight, event_sqme
    integer, intent(out), optional :: iostat
    integer :: index
    real(default) :: scale, alpha_s, sqme
    read (unit, iostat=iostat)  index
    if (iostat /= 0) return
    event%process => process_store_get_process_ptr (index)
    event%weight => event_weight
    event%sqme => event_sqme
    read (unit, iostat=iostat)  scale
    read (unit, iostat=iostat)  alpha_s
    read (unit, iostat=iostat)  sqme
    event%sqme = sqme
    call particle_set_read_raw (event%particle_set, unit, iostat=iostat)
    event%particle_set_exists = .true.
    if (associated (event%process)) then
       call process_set_particles (event%process, event%particle_set)
       call process_set_scale (event%process, scale)
       call process_set_alpha_s (event%process, alpha_s)
       call process_set_sqme (event%process, sqme)
    end if
    read (unit, iostat=iostat)  event%weight, event%excess
  end subroutine event_read_raw
    
  subroutine event_read_from_hepmc (event, hepmc_event, polarization_mode)
    type(event_t), intent(inout) :: event
    type(hepmc_event_t), intent(in) :: hepmc_event
    integer, intent(in) :: polarization_mode
    call event_discard_particle_set (event)
    call particle_set_init (event%particle_set, hepmc_event, &
         process_get_model_ptr (event%process), polarization_mode)
    event%particle_set_exists = .true.
  end subroutine event_read_from_hepmc

  subroutine event_write_to_hepmc (event, hepmc_event)
    type(event_t), intent(in) :: event
    type(hepmc_event_t), intent(inout) :: hepmc_event
    call particle_set_fill_hepmc_event (event%particle_set, hepmc_event)
  end subroutine event_write_to_hepmc

  subroutine event_write_to_hepeup (event)
    type(event_t), intent(in) :: event
    integer :: proc_id
    real(default) :: scale, alpha_qcd
    call particle_set_fill_hepeup (event%particle_set)
    if (associated (event%process)) then
       proc_id = process_get_lib_index (event%process)
       call hepeup_set_event_parameters (proc_id = proc_id)
       scale = process_get_scale (event%process)
       if (scale /= 0)  call hepeup_set_event_parameters (scale = scale)
       alpha_qcd = process_get_alpha_s (event%process)       
       if (alpha_qcd /= 0) &
            call hepeup_set_event_parameters (alpha_qcd = alpha_qcd)
       call hepeup_set_event_parameters (weight = event%weight)
    end if
  end subroutine event_write_to_hepeup

  subroutine event_write_to_hepevt (event, keep_beams, i_evt)
    type(event_t), intent(in) :: event
    type(particle_set_t), target :: pset_reduced
    integer :: proc_id, n_tot, n_out, n_remnants
    integer, intent(in), optional :: i_evt
    logical, intent(in), optional :: keep_beams  
    logical :: kb 
    integer :: evt_count
    real(default) :: weight, function_value    
    kb = .false.
    evt_count = 0
    if (present (keep_beams)) kb = keep_beams
    if (present (i_evt)) evt_count = i_evt
    call particle_set_fill_hepevt (event%particle_set, kb)
    call particle_set_reduce (event%particle_set, pset_reduced, kb)
    n_tot = particle_set_get_n_tot (pset_reduced)
    n_out = particle_set_get_n_out (pset_reduced)
    n_remnants = 0 
    function_value = process_get_sample_function_value (event%process)
    call hepevt_set_event_parameters (n_tot, n_out, &
       n_remnants, weight = event%weight, &
       function_value = function_value, i_evt = evt_count)    
  end subroutine event_write_to_hepevt

  subroutine event_test ()
    type(os_data_t) :: os_data
    type(process_library_t) :: prc_lib
    type(event_t), target :: event
    type(model_t), pointer :: model
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("QCD"), var_str("test.mdl"), os_data, model)
    call syntax_pexpr_init ()
    call syntax_phs_forest_init ()
    print *
    print *, "*** Load process library"
    call process_library_init (prc_lib, var_str("proc"), os_data)
    call process_library_load (prc_lib, os_data)
    print *
    call event_test1 (prc_lib, model)
    print *
    print *, "* Cleanup"
    call event_final (event)
    call process_store_final ()
    call syntax_pexpr_final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
  end subroutine event_test

  subroutine event_test1 (prc_lib, model)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(lhapdf_status_t) :: lhapdf_status
    type(process_t), pointer :: process
    type(os_data_t) :: os_data
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t) :: beam_data
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(grid_parameters_t) :: grid_parameters
    integer :: i
    type(tao_random_state) :: rng
    type(event_t), target :: event
    real(default), target :: event_weight = 0
    real(default), target :: event_sqme = 0
    type(decay_tree_t), target :: decay_tree
    logical :: rebuild_phs = .true.
    print *, "*** Test process setup"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    call process_store_init_process &
         (process, prc_lib, var_str ("qq"), model, lhapdf_status)
    print *, "  Process ID = ", char (process_get_id (process))
    print *
    print *, "* Beam setup"
    print *
    call os_data_init (os_data)
    call flavor_init (flv, (/ 21, 21 /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 1000._default, flv, pol)
    call process_setup_beams (process, beam_data, 0, 0)
    call process_connect_strfun (process)
    print *
    print *, "* Phase space setup"
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("qq.phs"), &
         vis_channels = .false.)
    print *
    print *, "* Cuts setup"
    call stream_init (stream, var_str ("all Pt > 200 GeV (outgoing u:d:U:D)"))
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    call process_setup_cuts (process, parse_tree_get_root_ptr (parse_tree))
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    print *
    print *, "*** Integration"
    print *, "* Grids setup"
    call process_setup_grids (process, grid_parameters, calls=10000)
    print *
    print *, "* 5 + 3 iterations"
    call process_results_write_header (process)
    do i = 1, 5
       call process_integrate (process, rng, grid_parameters, &
            1, 1, 1, 5000, i==1, .true., i>2, .true., .true.)
    end do
    call process_results_write_current_average (process)
    call process_integrate (process, rng, grid_parameters, &
         2, 1, 3, 5000, .true., .false., .true., .true., .true.)
    call process_results_write_footer (process)
    call process_write_time_estimate (process)
    print *
    print *, "*** Event generation"
    call process_setup_event_generation (process)
    call decay_tree_init (decay_tree, process)
    call event_init (event, process, event_weight, event_sqme, decay_tree)
    print *
    print *, "* Weighted event"
    call event_generate &
         (event, rng, .false., FM_IGNORE_HELICITY, .false., .false.)
    call event_write (event)
    print *
    print *, "* Unweighted event"
    call event_generate &
         (event, rng, .true., FM_SELECT_HELICITY, .false., .true.)
    call event_write (event)
    print *, "  Process data written to fort.81"
    call process_write (process, 81)
    call event_final (event)
  end subroutine event_test1


end module events
