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

module simulations

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: MAX_TRIES_FOR_SINGLE_EVENT !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use cputime
  use md5
  use parser
  use variables
  use prt_lists
  use expressions
  use flavors
  use state_matrices
  use beams
  use processes
  use decays
  use events
  use strfun_config
  use rt_data
  use integrations
  use event_files

  implicit none
  private

  public :: simulation_t
  public :: simulation_setup_reweight
  public :: simulation_setup_analysis
  public :: simulation_init
  public :: simulation_get_n_events
  public :: simulation_event
  public :: simulation_final
  public :: simulation_check_matching

  integer, parameter :: NORM_UNDEFINED = 0
  integer, parameter :: NORM_UNIT = 1
  integer, parameter :: NORM_N_EVT = 2
  integer, parameter :: NORM_SIGMA = 3
  integer, parameter :: NORM_SIGMA_N_EVT = 4

  character(*), parameter :: &
     checkpoint_head = &
        "| % complete | events generated | events remaining | time remaining", &
     checkpoint_bar = &
        "|===================================================================|", &
     checkpoint_fmt = "('   ',F5.1,T16,I9,T35,I9,T56,A)"


  type :: simulation_parameters_t
    logical :: unweighted = .true.
    integer :: normalization_mode = NORM_UNDEFINED
    logical :: negative_weights = .false.
    logical :: polarized = .false.
  end type simulation_parameters_t

  type :: checkpointing_t
    logical :: active = .false.
    logical :: running = .false.
    integer :: val = 0
    real(default) :: tzero = 0
  end type checkpointing_t

  type :: simulation_t
    private
    integer :: n_proc = 0
    type(string_t), dimension(:), allocatable :: process_id
    type(process_p), dimension(:), allocatable :: prc_array
    type(var_list_t) :: var_list
    logical :: rebuild_events = .false.
    integer :: n_in = 0
    type(flavor_t), dimension(:), allocatable :: beam_flv
    real(default), dimension(:), allocatable :: beam_energy
    type(string_t) :: basename
    logical :: rescan = .false.
    logical :: use_num_id = .false.
    integer, dimension(:), allocatable :: num_id
    logical :: update_parameters = .true.
    logical :: update_scale = .false.
    logical :: update_alpha_s = .false.
    logical :: update_sqme = .true.
    logical :: update_weight = .true.
    logical :: read_raw = .false.
    logical :: read_hepmc = .false.
    logical :: write_raw = .false.
    type(string_t) :: file_rescan
    type(string_t) :: file_raw
    type(string_t) :: file_hepmc
    type(input_event_stream_t) :: input_stream
    type(event_file_list_t) :: event_file_list
    integer :: u_raw = -1
    type(simulation_parameters_t) :: spar
    real(default), dimension(:), allocatable :: integral
    real(default) :: integral_sum = 0
    real(default) :: norm_weight = 0
    logical :: helicity_selection_active = .false.
    real(default) :: helicity_selection_threshold = -1
    integer :: helicity_selection_cutoff = 1000
    type(md5sum_events_t) :: md5sum
    integer :: n_events = 0
    integer :: n_read = 0
    integer :: i_evt = 0
    real(default) :: luminosity = 0
    type(eval_tree_t) :: reweight_expr
    type(eval_tree_t) :: analysis_expr
    type(prt_list_t) :: prt_list
    type(event_vars_t) :: event_vars
    logical :: allow_decays = .true.
    type(decay_tree_t), dimension(:), allocatable :: decay_tree
    type(checkpointing_t) :: checkpointing
    type(event_t) :: event
  end type simulation_t


contains

  recursive subroutine simulation_parameters_init &
      (sim, unweighted, event_normalization, negative_weights, &
         polarized)
    type(simulation_parameters_t), intent(out) :: sim
    logical, intent(in) :: unweighted
    type(string_t), intent(in) :: event_normalization
    logical, intent(in) :: negative_weights, polarized
    sim%unweighted = unweighted
    sim%negative_weights = negative_weights
    sim%polarized = polarized
    select case (char (event_normalization))
    case ("auto", "Auto", "AUTO", "automatic", "Automatic", "AUTOMATIC")
       if (unweighted) then
          sim%normalization_mode = NORM_UNIT
       else
          sim%normalization_mode = NORM_SIGMA
       end if
    case ("1", "unity", "Unity", "UNITY")
       sim%normalization_mode = NORM_UNIT
    case ("1/n", "1/N")
       sim%normalization_mode = NORM_N_EVT
    case ("sigma", "Sigma", "SIGMA")
       sim%normalization_mode = NORM_SIGMA
    case ("sigma/n", "Sigma/n", "Sigma/N", "SIGMA/N")
       sim%normalization_mode = NORM_SIGMA_N_EVT
    case default
       call msg_error ("Unknown value '" // char (event_normalization) &
            // "for $event_normalization.  I'll assume 'auto'")
       call simulation_parameters_init &
            (sim, unweighted, var_str ("auto"), negative_weights, polarized)
    end select
  end subroutine simulation_parameters_init

  subroutine simulation_parameters_write_message (sim, unit)
    type(simulation_parameters_t), intent(in) :: sim
    integer, intent(in), optional :: unit
    type(string_t) :: weight_str, norm_str, neg_str, polarized_str
    if (sim%unweighted) then
       weight_str = "unweighted"
    else
       weight_str = "weighted"
    end if     
    if (sim%polarized) then 
       polarized_str = ", polarized events" 
    else 
       polarized_str = ", unpolarized_events" 
    end if 
    select case (sim%normalization_mode)
    case (NORM_UNIT)
       norm_str = "1"
    case (NORM_N_EVT)
       norm_str = "1/n"
    case (NORM_SIGMA)
       norm_str = "sigma"
    case (NORM_SIGMA_N_EVT)
       norm_str = "sigma/n"
    case default
       norm_str = "unknown"
    end select
    if (sim%negative_weights) then
       neg_str = ", allow negative weights"
    else
       neg_str = ""
    end if
    call msg_message ("Simulation mode = " // char (weight_str) &
         // ", event_normalization = '" // char (norm_str) &
         // "'" // char (neg_str) // char (polarized_str), &
         unit)
  end subroutine simulation_parameters_write_message

  subroutine simulation_parameters_write (sim, unit)
    type(simulation_parameters_t), intent(in) :: sim
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *) "Simulation parameters:"
    write (u, *) "  unweighted         = ", sim%unweighted
    write (u, *) "  normalization_mode = ", sim%normalization_mode
    write (u, *) "  negative_weights   = ", sim%negative_weights
    write (u, *) "  polarized          = ", sim%polarized
  end subroutine simulation_parameters_write

  function simulation_parameters_get_norm (sim, sigma, n) result (norm)
    real(default) :: norm
    type(simulation_parameters_t), intent(in) :: sim
    real(default), intent(in) :: sigma
    integer, intent(in) :: n
    select case (sim%normalization_mode)
    case (NORM_UNIT)
       norm = 1
    case (NORM_N_EVT)
       if (n /= 0) then
          norm = 1._default / n
       else
          norm = 1
       end if
    case (NORM_SIGMA)
       norm = sigma
    case (NORM_SIGMA_N_EVT)
       if (n /= 0) then
          norm = sigma / n
       else
          norm = sigma
       end if
    case default
       norm = 1
    end select
    if ((.not. sim%unweighted) .and. sigma /= 0)  norm = norm / sigma
  end function simulation_parameters_get_norm

  function simulation_parameters_get_md5sum (sim) result (md5sum_sim)
    character(32) :: md5sum_sim
    type(simulation_parameters_t), intent(in) :: sim
    integer :: u
    u = free_unit ()
    open (u, status = "scratch")
    call simulation_parameters_write (sim, u)
    rewind (u)
    md5sum_sim = md5sum (u)
    close (u)
  end function simulation_parameters_get_md5sum

  subroutine checkpointing_init (checkpointing, var_list)
    type(checkpointing_t), intent(out) :: checkpointing
    type(var_list_t), intent(in) :: var_list
    checkpointing%active = var_list_is_known (var_list, var_str ("checkpoint"))
    if (checkpointing%active) then
       checkpointing%val = &
       var_list_get_ival (var_list, var_str("checkpoint"))
       if (checkpointing%val <= 0) then
          call msg_warning ("ignoring nonpositive value of 'checkpoint'")
          checkpointing%active = .false.
       end if
    end if
  end subroutine checkpointing_init

  subroutine checkpointing_msg_start (checkpointing, n_events, i_evt)
    type(checkpointing_t), intent(inout) :: checkpointing
    integer, intent(in) :: n_events, i_evt
    if (checkpointing%active .and. n_events > i_evt) then
       call msg_message ("")
       call msg_message (checkpoint_bar)
       call msg_message (checkpoint_head)
       call msg_message (checkpoint_bar)
       write (msg_buffer, checkpoint_fmt) 0., 0, n_events - i_evt, "???"
       call msg_message ()
       checkpointing%running = .true.
       checkpointing%tzero = time_current ()
    end if
  end subroutine checkpointing_msg_start

  subroutine checkpointing_msg_event (checkpointing, n_events, n_read, i_evt)
    type(checkpointing_t), intent(in) :: checkpointing
    integer, intent(in) :: n_events, n_read, i_evt
    real(default) :: tcurrent
    type(string_t) :: tremain
    if (checkpointing%active .and. checkpointing%running &
          .and. mod (i_evt, checkpointing%val) == 0) then
       tcurrent = time_current ()
       tremain = time2string ( &
          int ((tcurrent - checkpointing%tzero) / (i_evt - n_read) &
             * (n_events - i_evt)))
       write (msg_buffer, checkpoint_fmt) &
             100 * (i_evt - n_read) / real (n_events - n_read), &
             i_evt - n_read, &
             n_events - i_evt, char (tremain)
       call msg_message ()
    end if
  end subroutine checkpointing_msg_event

  subroutine checkpointing_msg_end (checkpointing, n_read, i_evt)
    type(checkpointing_t), intent(inout) :: checkpointing
    integer, intent(in) :: n_read, i_evt
    if (checkpointing%active .and. checkpointing%running) then
       if (mod (i_evt, checkpointing%val) /= 0) then
          write (msg_buffer, checkpoint_fmt) 100., i_evt - n_read, 0, "0s"
          call msg_message ()
       end if
       call msg_message (checkpoint_bar)
       call msg_message ("")
       checkpointing%running = .false.
    end if
  end subroutine checkpointing_msg_end

  subroutine simulation_basic_init (sim, process_id, var_list, rescan, verbose)
    type(simulation_t), intent(out) :: sim
    type(string_t), dimension(:), intent(in) :: process_id
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: rescan, verbose
    type(string_t) :: process_string
    integer :: proc
    logical :: generate, verb
    generate = .true.;  if (present (rescan))  generate = .not. rescan
    verb = .true.;  if (present (verbose))  verb = verbose
    sim%n_proc = size (process_id)
    allocate (sim%process_id (sim%n_proc))
    sim%process_id = process_id
    allocate (sim%prc_array (sim%n_proc))
    do proc = 1, sim%n_proc
       sim%prc_array(proc)%ptr => &
            process_store_get_process_ptr (sim%process_id(proc))
    end do
    if (verb) then
       process_string = ""
       do proc = 1, size (process_id)
          if (proc > 1)  process_string = process_string // ", "
          process_string = process_string // sim%process_id (proc)
       end do
       if (generate) then
          call msg_message ("Initializing simulation for processes " &
            // char (process_string) // ":")
       else
          call msg_message ("Initializing rescanning for processes " &
            // char (process_string) // ":")
       end if
    end if
    sim%rebuild_events = &
         var_list_get_lval (var_list, var_str ("?rebuild_events"))
    call simulation_parameters_init (sim%spar, &
         var_list_get_lval &
              (var_list, var_str ("?unweighted")), &
         var_list_get_sval &
              (var_list, var_str ("$event_normalization")), &
         var_list_get_lval &
              (var_list, var_str ("?negative_weights")), &
         var_list_get_lval &
              (var_list, var_str ("?polarized_events")))
    if (present (verbose)) then
       if (verbose)  call simulation_parameters_write_message (sim%spar)
    end if
    sim%helicity_selection_active = &
         var_list_get_lval (var_list, var_str ("?helicity_selection_active"))
    if (sim%helicity_selection_active) then
       sim%helicity_selection_threshold = var_list_get_rval (var_list, &
            var_str ("helicity_selection_threshold"))
       sim%helicity_selection_cutoff = var_list_get_ival (var_list, &
            var_str ("helicity_selection_cutoff"))
    end if
    sim%use_num_id = &
         var_list_get_lval (var_list, var_str ("?use_num_id"))
    if (sim%use_num_id) then
       allocate (sim%num_id (size (process_id)))
       do proc = 1, sim%n_proc
          sim%num_id(proc) = proc_get_num_id (sim%process_id(proc), var_list)
       end do
    end if       
    sim%allow_decays = &
         var_list_get_lval (var_list, var_str ("?allow_decays"))
    call var_list_init_snapshot (sim%var_list, var_list)
  end subroutine simulation_basic_init

  function proc_get_num_id (process_id, var_list) result (num_id)
    integer :: num_id
    type(string_t), intent(in) :: process_id
    type(var_list_t), intent(in) :: var_list
    type(string_t) :: var_name
    var_name = "num_id(" // process_id // ")"
    if (var_list_is_known (var_list, var_name)) then
       num_id = var_list_get_ival (var_list, var_name)
    else
       call msg_error ("Numeric process ID '" &
            // char (var_name) // "' is undefined, inserting zero.")
       num_id = 0
    end if
  end function proc_get_num_id

  subroutine simulation_init_rescan &
       (sim, file_rescan, process_id, var_list, verbose)
    type(simulation_t), intent(out) :: sim
    type(string_t), intent(in) :: file_rescan
    type(string_t), dimension(:), intent(in) :: process_id
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: verbose
    integer :: proc
    type(process_t), pointer :: process
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    call simulation_basic_init &
         (sim, process_id, var_list, rescan=.true., verbose=verbose)
    sim%rebuild_events = .false.
    sim%rescan = .true.
    sim%file_rescan = file_rescan
    sim%update_parameters = &
         var_list_get_lval (var_list, var_str ("?update_parameters"))
    sim%update_scale = &
         var_list_get_lval (var_list, var_str ("?update_scale"))
    sim%update_alpha_s = &
         var_list_get_lval (var_list, var_str ("?update_alpha_s"))
    sim%update_sqme = &
         var_list_get_lval (var_list, var_str ("?update_sqme"))
    sim%update_weight = &
         var_list_get_lval (var_list, var_str ("?update_weight"))
    if (verb) then
       call msg_message ("Reading events from file '" &
            // char (sim%file_rescan) // "'")
       if (sim%update_scale)  call msg_message &
            ("Recalculating event scale")
       if (sim%update_alpha_s)  call msg_message &
            ("Recalculating alpha_s")
       if (sim%update_sqme) then
          if (sim%update_parameters) then
             call msg_message ("Recalculating squared matrix element " &
                  // "with updated parameters")
          else
             call msg_message ("Recalculating squared matrix element")
          end if
       end if
       if (sim%update_weight)  call msg_message ("Updating event weight " &
            // "using matrix element ratio")
    end if          
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       call process_reset_helicity_selection (process, &
            sim%helicity_selection_threshold, sim%helicity_selection_cutoff)
    end do
  end subroutine simulation_init_rescan

  subroutine simulation_compute_missing_integrals &
      (sim, global, global_var_list, rescan, verbose)
    type(simulation_t), intent(inout) :: sim
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(in), optional :: rescan, verbose
    integer :: proc
    logical :: me_only
    me_only = .false.;  if (present (rescan))  me_only = rescan
    if (me_only) then
       call prepare_me_missing_processes (sim%process_id, global, verbose)
    else
       call integrate_missing_processes &
            (sim%process_id, global, global_var_list, verbose = verbose)
    end if
    do proc = 1, sim%n_proc
       sim%prc_array(proc)%ptr => &
            process_store_get_process_ptr (sim%process_id(proc))
    end do
  end subroutine simulation_compute_missing_integrals

  subroutine simulation_init_missing_processes (sim, global, verbose)
    type(simulation_t), intent(inout) :: sim
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: verbose
    integer :: n_missing
    type(string_t), dimension(:), allocatable :: missing_process_id
    logical, dimension(:), allocatable :: missing
    integer :: proc
    logical :: verb
    verb = .false.;  if (present (verbose))  verb = verbose
    allocate (missing (sim%n_proc))
    do proc = 1, sim%n_proc
       missing(proc) = .not. associated (sim%prc_array(proc)%ptr)
    end do
    n_missing = count (missing)
    if (n_missing > 0) then
       allocate (missing_process_id (n_missing))
       missing_process_id = pack (sim%process_id, missing)
       call prepare_me_evaluation (missing_process_id, global)
       do proc = 1, sim%n_proc
          if (missing(proc)) sim%prc_array(proc)%ptr => &
               process_store_get_process_ptr (sim%process_id(proc))
       end do
    end if
  end subroutine simulation_init_missing_processes

  subroutine simulation_check (sim, ok)
    type(simulation_t), intent(inout) :: sim
    logical, intent(out) :: ok
    type(process_t), pointer :: process
    integer :: proc
    type(flavor_t), dimension(:), allocatable :: beam_flv
    real(default), dimension(:), allocatable :: beam_energy
    ok = .false.
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       if (.not. associated (process)) then
          call msg_fatal ("Process '" // char (sim%process_id(proc)) &
               // "' is not available for simulation.")
          return
       end if
       select case (proc)
       case (1)
          sim%n_in = process_get_n_in (process)
          allocate (beam_flv (sim%n_in), beam_energy (sim%n_in))
          beam_flv = process_get_beam_flv (process)
          beam_energy = process_get_beam_energy (process)
       case default
          if (.not. process_has_matrix_element (process))  cycle
          if (process_get_n_in (process) /= sim%n_in) then
             call msg_fatal ("Simulation: " &
                  // "Mixture of scattering and decays")
             return
          else if (any (process_get_beam_flv (process) /= beam_flv)) then
             call msg_fatal ("Simulation: Mismatch in beam particles")
             return
          else if (any (process_get_beam_energy (process) &
                        /= beam_energy))then
             call msg_fatal ("Simulation: Mismatch in beam energies")
             return
          end if
       end select
    end do
    allocate (sim%beam_flv (sim%n_in), sim%beam_energy (sim%n_in))
    sim%beam_flv = beam_flv
    sim%beam_energy = beam_energy
    ok = .true.
  end subroutine simulation_check

  subroutine simulation_setup_event_file_list (sim, event_fmt, basename_default)
    type(simulation_t), intent(inout) :: sim
    integer, dimension(:), intent(in), allocatable :: event_fmt
    type(string_t), intent(in) :: basename_default
    type(string_t) :: extension_raw
    integer :: i
    logical :: mlm_matching
    type(string_t) :: matching_basename
    sim%basename = var_list_get_sval (sim%var_list, var_str ("$sample"))
    if (sim%basename == "")  sim%basename = basename_default
    if (sim%rescan) then
       select case (event_file_get_format (sim%file_rescan))
       case (FMT_RAW)
          sim%read_raw = .true.
          sim%file_raw = sim%file_rescan
       case (FMT_HEPMC)
          sim%read_hepmc = .true.
          sim%file_hepmc = sim%file_rescan
       case default
          call msg_fatal ("Rescanning event file '" // char (sim%file_rescan) &
               // "': file format not supported")
       end select
       sim%write_raw = .false.
    else
       sim%read_raw = var_list_get_lval (sim%var_list, var_str ("?read_raw")) &
           .and. .not. sim%rebuild_events
       sim%write_raw = var_list_get_lval (sim%var_list, var_str ("?write_raw"))
       extension_raw = var_list_get_sval (sim%var_list, var_str ("$extension_raw"))
       sim%file_raw = sim%basename // "." // extension_raw
    end if
    if (allocated (event_fmt)) then
       do i = 1, size (event_fmt)
          call event_file_list_append_file_spec (sim%event_file_list, &
               sim%basename, sim%var_list, event_fmt(i), &
               sim%beam_flv, sim%beam_energy, sim%n_proc)
       end do
    end if
    mlm_matching = var_list_get_lval &
        (sim%var_list, var_str ("?mlm_matching"))
    if (mlm_matching) then
        matching_basename = "mlm_sample"
        call event_file_list_append_file_spec (sim%event_file_list, &
               matching_basename, sim%var_list, FMT_LHEF, &
               sim%beam_flv, sim%beam_energy, sim%n_proc)
    end if
    if (sim%rescan) then
       if (sim%read_raw) then
          if (event_file_list_is_filename (sim%event_file_list, sim%file_raw)) &
               call msg_fatal ("Output event file '" &
                    // char (sim%file_raw) // "' coincides with input file")
       else if (sim%read_hepmc) then
          if (event_file_list_is_filename (sim%event_file_list, sim%file_hepmc)) &
               call msg_fatal ("Output event file '" &
                    // char (sim%file_hepmc) // "' coincides with input file")
       end if
    end if
  end subroutine simulation_setup_event_file_list

  subroutine simulation_collect_integrals (sim, var_list, ok)
    type(simulation_t), intent(inout) :: sim
    type(var_list_t), intent(in) :: var_list
    logical, intent(out) :: ok
    integer :: proc
    type(process_t), pointer :: process
    type(string_t) :: process_id
    allocate (sim%integral (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       process_id = process_get_id (process)
       sim%integral(proc) = var_list_get_rval (var_list, &
            var_str ("integral(") // process_id // ")")
       if (sim%integral(proc) < 0 .and. .not.sim%spar%negative_weights) then       
          call msg_fatal ("Integral of process '" &
               // char (process_id) // "' is negative")
       end if
    end do
    sim%integral_sum = sum (sim%integral)
    if (sim%integral_sum > 0) then
       ok = .true.
    else
       if (sim%spar%negative_weights) then
          ok = .false.
       else
          call msg_error ("Simulation: " &
               // "sum of process integrals must be positive; skipping")
          ok = .false.
       end if
    end if
  end subroutine simulation_collect_integrals

  subroutine simulation_collect_md5sums (sim)
    type(simulation_t), intent(inout) :: sim
    integer :: proc
    type(process_t), pointer :: process
    allocate (sim%md5sum%process (sim%n_proc))
    allocate (sim%md5sum%parameters (sim%n_proc))
    allocate (sim%md5sum%results (sim%n_proc))
    allocate (sim%md5sum%polarized (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       sim%md5sum%process(proc) = process_get_md5sum (process)
       sim%md5sum%parameters(proc) = process_get_md5sum_parameters (process)
       sim%md5sum%results(proc) = process_get_md5sum_results (process)
       sim%md5sum%polarized(proc) = process_get_md5sum_polarized (process)
    end do
    if (sim%allow_decays) then
       sim%md5sum%decays = decay_store_get_md5sum ()
    else
       sim%md5sum%decays = ""
    end if
    sim%md5sum%simulation = simulation_parameters_get_md5sum (sim%spar)
  end subroutine simulation_collect_md5sums

  subroutine simulation_setup_n_events (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    integer :: n_events
    real(default) :: luminosity
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    n_events = var_list_get_ival (sim%var_list, var_str ("n_events"))
    if (sim%rescan) then
       if (n_events /= 0) then
          sim%n_events = n_events
          if (verb) then
             write (msg_buffer, "(A,1x,I0)") &
                   "Requested number of events =", sim%n_events
             call msg_message ()           
          end if
       else
          sim%n_events = huge (1)
       end if
       sim%luminosity = 0
       sim%norm_weight = 0
    else
       luminosity = var_list_get_rval (sim%var_list, var_str ("luminosity"))
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !!! To be discussed for 2.0.4
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !!! if (sim%spar%unweighted) then
       !!!    luminosity = var_list_get_rval (sim%var_list, var_str ("luminosity"))
       !!! else
       !!!    luminosity = 0
       !!! end if
       sim%n_events = max (nint (luminosity * sim%integral_sum), n_events)
       sim%luminosity = max (luminosity, sim%n_events / sim%integral_sum)
       sim%norm_weight = simulation_parameters_get_norm &
            (sim%spar, sim%integral_sum, sim%n_events)
       if (verb) then
          write (msg_buffer, "(A,1x,I0)") &
                "Requested number of events =", sim%n_events
          call msg_message ()           
          if (sim%spar%unweighted) then
             write (msg_buffer, "(A,1x,G11.4)") &
                   "This corresponds to luminosity [fb-1] = ", &
                   sim%luminosity
             call msg_message ()
          end if
       end if
    end if
  end subroutine simulation_setup_n_events

  subroutine simulation_prepare_event_generation (sim, verbose)
    type(simulation_t), intent(inout), target :: sim
    logical, intent(in), optional :: verbose
    integer :: proc
    logical :: ok, verb
    type(process_t), pointer :: process
    verb = .false.;  if (present (verbose)) verb = verbose
    if (sim%allow_decays)  allocate (sim%decay_tree (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       call process_setup_event_generation (process)
       if (sim%allow_decays) &
            call decay_tree_init (sim%decay_tree(proc), process)
    end do
    call event_file_list_open (sim%event_file_list, sim%process_id, &
        sim%n_events, sim%var_list)
    if (sim%read_raw) then
       call open_raw_event_file_for_reading &
            (sim%file_raw, sim%rescan, sim%md5sum, sim%u_raw, ok, verbose)
       if (.not. ok)  sim%read_raw = .false.
    else if (sim%read_hepmc) then
       call input_event_stream_init &
            (sim%input_stream, sim%file_hepmc, FMT_HEPMC)
    else
       if (verb) then
          write (msg_buffer, "(A,I0,A)") &
                "Generating ", sim%n_events, " events ..."
          call msg_message
       end if
    end if
    if (.not. sim%read_raw) then
       if (sim%write_raw) then
          call open_raw_event_file_for_writing &
               (sim%file_raw, sim%md5sum, sim%u_raw, verbose)
       end if
    end if
    call checkpointing_init (sim%checkpointing, sim%var_list)
    sim%n_read = 0
    sim%i_evt = 0
  end subroutine simulation_prepare_event_generation

  subroutine simulation_setup_reweight (sim, pn_reweight_expr, verbose)
    type(simulation_t), intent(inout), target :: sim
    type(parse_node_t), pointer :: pn_reweight_expr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       if (associated (pn_reweight_expr)) then
          call msg_message ("Applying user-defined reweighting expression.")
       end if
    end if
    if (associated (pn_reweight_expr)) then
       call eval_tree_init_expr (sim%reweight_expr, &
            pn_reweight_expr, sim%var_list, sim%prt_list, &
            sim%event_vars)
    end if
  end subroutine simulation_setup_reweight

  subroutine simulation_setup_analysis (sim, pn_analysis_lexpr, verbose)
    type(simulation_t), intent(inout), target :: sim
    type(parse_node_t), pointer :: pn_analysis_lexpr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       if (associated (pn_analysis_lexpr)) then
          call msg_message ("Applying user-defined analysis setup.")
       else
          call msg_message ("No analysis setup has been provided.")
       end if
    end if
    if (associated (pn_analysis_lexpr)) then
       call eval_tree_init_lexpr (sim%analysis_expr, &
            pn_analysis_lexpr, sim%var_list, sim%prt_list, &
            sim%event_vars)
    end if
  end subroutine simulation_setup_analysis

  subroutine simulation_read_event_raw (sim, ok, verbose)
    type(simulation_t), intent(inout), target :: sim
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: iostat
    verb = .false.;  if (present (verbose)) verb = verbose
    if (sim%use_num_id) then
       call event_read_raw (sim%event, sim%u_raw, &
            sim%event_vars, sim%prc_array, num_id_array=sim%num_id, &
            iostat=iostat)
    else
       call event_read_raw (sim%event, sim%u_raw, &
            sim%event_vars, sim%prc_array, iostat=iostat)
    end if
    if (iostat == 0) then
       sim%i_evt = sim%i_evt + 1
       sim%n_read = sim%n_read + 1
       ok = .true.
    else
       ok = .false.
       if (verb) then
          write (msg_buffer, "(A,1x,I0,1x,A)")  &
                "...", sim%n_read, "events read."
          call msg_message ()
       end if
       if (.not. sim%rescan) then
          sim%read_raw = .false.
          if (verb) then
             write (msg_buffer, "(A,1x,I0,1x,A)") &
                   "Generating", sim%n_events - sim%n_read, " events ..."
             call msg_message ()
          end if
          if (sim%write_raw) then
              call reopen_raw_event_file_for_writing &
                   (sim%file_raw, sim%u_raw, verbose)
          else
              close (sim%u_raw)
          end if
          ok = .true.
       end if
    end if
  end subroutine simulation_read_event_raw

  subroutine simulation_read_event_hepmc (sim, ok)
    type(simulation_t), intent(inout), target :: sim
    logical, intent(out) :: ok
    if (sim%use_num_id) then
       call input_event_stream_read_event (sim%input_stream, sim%event, &
            sim%event_vars, sim%prc_array, ok, num_id_array=sim%num_id)
    else
       call input_event_stream_read_event (sim%input_stream, sim%event, &
            sim%event_vars, sim%prc_array, ok)
    end if
  end subroutine simulation_read_event_hepmc

  subroutine simulation_select_process (sim, rng, process, proc)
    type(simulation_t), intent(in) :: sim
    type(tao_random_state), intent(inout) :: rng
    type(process_t), pointer :: process
    integer, intent(out) :: proc
    real(default) :: integral_cmp, x
    call tao_random_number (rng, x)
    integral_cmp = 0
    do proc = 1, sim%n_proc
       integral_cmp = integral_cmp + sim%integral(proc)
       if (integral_cmp > x * sim%integral_sum)  exit
    end do
    proc = min (proc, sim%n_proc)
    process => sim%prc_array(proc)%ptr
  end subroutine simulation_select_process

  subroutine simulation_recover_process (sim, proc)
    type(simulation_t), intent(inout) :: sim
    integer, intent(out) :: proc
    type(string_t) :: process_id
    type(process_t), pointer :: process
    process => event_get_process_ptr (sim%event)
    if (associated (process)) then
       process_id = process_get_id (process)
       do proc = 1, sim%n_proc
          if (process_id == process_get_id (sim%prc_array(proc)%ptr)) then
            call event_recover_process (sim%event)
            return
          end if
       end do
    end if
    call event_write (sim%event)
    call msg_fatal ("Simulation: recovering process data from event failed.")
    proc = 0
  end subroutine simulation_recover_process

  subroutine simulation_recalculate (sim)
    type(simulation_t), intent(inout) :: sim
    if (sim%update_parameters)  call event_update_parameters (sim%event)
    if (sim%update_scale)  call event_compute_scale (sim%event)
    if (sim%update_alpha_s)  call event_update_alpha_s (sim%event)
    if (sim%update_sqme)  call event_compute_sqme (sim%event)
    if (sim%update_weight)  call event_update_weight (sim%event)
  end subroutine simulation_recalculate

  subroutine simulation_generate_event (sim, rng, process, proc)
    type(simulation_t), intent(inout), target :: sim
    type(tao_random_state), intent(inout) :: rng
    type(process_t), intent(in), target :: process
    integer, intent(in) :: proc
    integer :: factorization_mode, try
    if (sim%allow_decays) then
       call event_init (sim%event, process, &
            sim%event_vars, sim%decay_tree(proc))
    else
       call event_init (sim%event, process, sim%event_vars)
    end if
    if (sim%use_num_id) then
       sim%event_vars%process_num_id = sim%num_id(proc)
    else
       sim%event_vars%process_num_id = proc
    end if
    if (sim%spar%polarized) then 
       factorization_mode = FM_SELECT_HELICITY 
    else 
       factorization_mode = FM_IGNORE_HELICITY 
    end if 
    GENERATE: do try = 1, MAX_TRIES_FOR_SINGLE_EVENT
       call event_generate &
            (sim%event, rng, sim%spar%unweighted, &
             factorization_mode, &
             keep_correlations=.false., &
             keep_virtual=.true.)
       if (event_is_valid (sim%event))  exit GENERATE
    end do GENERATE
    if (.not. event_is_valid (sim%event)) then
       write (msg_buffer, "(A,I0,A)") "Failed to generate a valid event " &
            // "after ", MAX_TRIES_FOR_SINGLE_EVENT, " tries"
       call msg_fatal ()
    end if
    sim%i_evt = sim%i_evt + 1
    sim%event_vars%process_index = proc
    sim%event_vars%event_index = sim%i_evt
    call event_renormalize_weight (sim%event, sim%norm_weight)
  end subroutine simulation_generate_event

  subroutine simulation_decay (sim, rng, proc)
    type(simulation_t), intent(inout), target :: sim
    type(tao_random_state), intent(inout) :: rng
    integer, intent(in) :: proc
    if (sim%allow_decays) then
       call event_decay (sim%event, rng, sim%decay_tree(proc))
       call event_factorize_process (sim%event, rng, &
             FM_IGNORE_HELICITY, &
             keep_correlations=.false., &
             keep_virtual=.true.)
    end if
  end subroutine simulation_decay

  subroutine simulation_handle_event (sim)
    type(simulation_t), intent(inout), target :: sim
    call event_reweight (sim%event, sim%prt_list, sim%reweight_expr)
    call event_do_analysis (sim%event, sim%prt_list, sim%analysis_expr)
    call event_file_list_write_event (sim%event_file_list, sim%event, i_evt=sim%i_evt)
    if (sim%write_raw .and. .not. sim%read_raw) &
         call event_write_raw (sim%event, sim%u_raw)
    call checkpointing_msg_event &
         (sim%checkpointing, sim%n_events, sim%n_read, sim%i_evt)
  end subroutine simulation_handle_event

  subroutine simulation_final_event (sim)
    type(simulation_t), intent(inout), target :: sim
    call event_final (sim%event)
  end subroutine simulation_final_event

  subroutine simulation_finish_event_generation (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    integer :: proc
    logical :: verb, mlm_matching
    verb = .false.;  if (present (verbose)) verb = verbose
    call checkpointing_msg_end &
         (sim%checkpointing, sim%n_read, sim%i_evt)
    call event_file_list_close (sim%event_file_list)
    if (sim%read_raw .or. sim%write_raw)  close (sim%u_raw)
    if (sim%read_hepmc)  call input_event_stream_final (sim%input_stream)
    if (sim%allow_decays) then
       do proc = 1, sim%n_proc
          call decay_tree_final (sim%decay_tree(proc))
       end do
    end if
    call eval_tree_final (sim%analysis_expr)
    if (verb) then
       if (sim%rescan) then
          call msg_message ("Rescanning finished.")
       else
          if (sim%read_raw) then
             write (msg_buffer, "(A,1x,I0,1x,A,1x,I0,1x,A)")  &
                   "...", sim%n_read, "events read,", sim%n_events, "total."
             call msg_message ()
          else       
             write (msg_buffer, "(A,1x,I0,1x,A,1x,I0,1x,A)")  &
                   "...", sim%n_events - sim%n_read, "events generated.", &
                   sim%n_events, "total."
             call msg_message ()
          end if
          call msg_message ("Simulation finished.")
       end if
    end if
  end subroutine simulation_finish_event_generation

  subroutine simulation_basic_final (sim)
    type(simulation_t), intent(inout) :: sim
    call var_list_final (sim%var_list)
  end subroutine simulation_basic_final

  subroutine open_raw_event_file_for_reading &
      (file_raw, rescan, md5sum, u_raw, ok, verbose)
    type(string_t), intent(in) :: file_raw
    logical, intent(in) :: rescan
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(out) :: u_raw
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: iostat
    verb = .false.;  if (present (verbose))  verb = verbose
    inquire (file = char (file_raw), exist = ok)
    if (ok) then
       ok = event_file_get_format (file_raw) == FMT_RAW
       if (.not. ok) then
          call msg_warning ("File '" // char (file_raw) &
               // "' is not a WHIZARD raw event file, discarding.")
       end if
    end if
    if (ok) then
       if (verb)  call msg_message ("Reading events from file '" &
            // char (file_raw) // "' ...")
       u_raw = free_unit ()
       open (file = char (file_raw), unit = u_raw, form = "unformatted", &
             action = "read", status = "old")
       call raw_event_file_read_header (u_raw, rescan, md5sum, ok, iostat)
       if (iostat /= 0) then
          call msg_error ("Event file '" & 
               // char (file_raw) // "' is corrupt, discarding.")
          close (u_raw)
          ok = .false.
       else if (.not. ok) then
          close (u_raw)
          ok = .false.
       else
          ok = .true.
       end if
    end if
  end subroutine open_raw_event_file_for_reading

  subroutine open_raw_event_file_for_writing (file_raw, md5sum, u_raw, verbose)
    type(string_t), intent(in) :: file_raw
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(out) :: u_raw
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       call msg_message ("Writing events in internal format to file '" &
            // char (file_raw) // "'")
    end if
    u_raw = free_unit ()
    open (file = char (file_raw), unit = u_raw, form = "unformatted", &
          action = "write", status = "replace")
    call raw_event_file_write_header (u_raw, md5sum)
  end subroutine open_raw_event_file_for_writing

  subroutine reopen_raw_event_file_for_writing (file_raw, u_raw, verbose)
    type(string_t), intent(in) :: file_raw
    integer, intent(in) :: u_raw
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       call msg_message ("Appending events in internal format to file '" &
            // char (file_raw) // "'")
    end if
    close (u_raw)
    open (file = char (file_raw), unit = u_raw, form = "unformatted", &
          action = "write", status = "old", position = "append")
  end subroutine reopen_raw_event_file_for_writing

  subroutine simulation_init &
      (sim, process_id, global, global_var_list, ok, filename, verbose)
    type(simulation_t), intent(out) :: sim
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), intent(inout) :: global_var_list
    logical, intent(out) :: ok
    type(string_t), intent(in), optional :: filename
    logical, intent(in), optional :: verbose
    type(string_t) :: basename_default
    logical :: rescan
    rescan = present (filename)
    if (size (process_id) /= 0) then
       basename_default = process_id(1)
    else
       basename_default = "whizard"
    end if
    if (rescan) then
       call simulation_init_rescan &
            (sim, filename, process_id, global%var_list, verbose)
    else
       call simulation_basic_init &
            (sim, process_id, global%var_list, verbose=verbose)
    end if
    call simulation_compute_missing_integrals &
         (sim, global, global_var_list, rescan, verbose)
    call simulation_check (sim, ok)
    if (ok .and. .not. rescan) then
       call simulation_collect_integrals (sim, global%var_list, ok)
    end if
    if (ok) then
       call simulation_setup_event_file_list &
            (sim, global%event_fmt, basename_default)
       call simulation_collect_md5sums (sim)
       call simulation_setup_n_events (sim, verbose)
       call simulation_prepare_event_generation (sim, verbose)
    end if
    if (.not. ok)  call simulation_basic_final (sim)
  end subroutine simulation_init

  function simulation_get_n_events (sim) result (n_events)
    integer :: n_events
    type(simulation_t), intent(in) :: sim
    n_events = sim%n_events
  end function simulation_get_n_events

  subroutine simulation_event (sim, rng, ok, verbose)
    type(simulation_t), intent(inout), target :: sim
    type(tao_random_state), intent(inout) :: rng
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    type(process_t), pointer :: process
    integer :: proc
    if (sim%read_raw) then
       call simulation_read_event_raw (sim, ok, verbose)
    else if (sim%read_hepmc) then
       call simulation_read_event_hepmc (sim, ok)
    end if
    if (sim%rescan) then
       if (.not. ok)  return
       call simulation_recover_process (sim, proc)
       call simulation_recalculate (sim)
       call simulation_decay (sim, rng, proc)
    else if (.not. sim%read_raw) then
       if (sim%checkpointing%active .and. (.not. sim%checkpointing%running)) &
          call checkpointing_msg_start (sim%checkpointing, sim%n_events, &
               sim%i_evt)
       call simulation_select_process (sim, rng, process, proc)
       call simulation_generate_event (sim, rng, process, proc)
    end if
    call simulation_handle_event (sim)
    call simulation_final_event (sim)
  end subroutine simulation_event

  subroutine simulation_final (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    call simulation_finish_event_generation (sim, verbose)
    call simulation_basic_final (sim)
  end subroutine simulation_final

  function simulation_check_matching (sim) result (mlm_matching)
    type(simulation_t), intent(inout) :: sim
    logical :: mlm_matching
    mlm_matching = var_list_get_lval &
          (sim%var_list, var_str ("?mlm_matching"))
  end function simulation_check_matching


end module simulations
