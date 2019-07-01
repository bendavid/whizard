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

module events

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: RAW_EVENT_FILE_ID_STRING !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!
  use os_interface
  use lexers
  use parser
  use subevents
  use variables
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
  use lorentz !NODEP!
  use shower_interface
  use lorentz !NODEP!
  use ckkw_pseudo_weights_module !NODEP!
  use ckkw_matching_module !NODEP!

  implicit none
  private

  public :: event_t
  public :: event_init
  public :: event_final
  public :: event_write
  public :: event_generate
  public :: ckkw_fake_pseudo_shower_weights
  public :: event_decay
  public :: event_factorize_process
  public :: event_recover_process
  public :: event_compute_scale
  public :: event_update_parameters
  public :: event_update_alpha_s
  public :: event_compute_sqme
  public :: event_update_weight
  public :: event_passes_selection
  public :: event_renormalize_weight
  public :: event_reweight
  public :: event_do_analysis
  public :: event_is_valid
  public :: event_is_vetoed
  public :: md5sum_events_t
  public :: is_raw_event_file
  public :: raw_event_file_write_header
  public :: raw_event_file_read_header
  public :: event_write_raw
  public :: event_read_raw
  public :: is_hepmc_event_file
  public :: event_read_from_hepmc
  public :: event_write_to_hepmc
  public :: event_write_to_hepeup
  public :: event_assure_heprup
  public :: event_write_to_hepevt
  public :: event_get_process_ptr
  public :: FM_IGNORE_HELICITY
  public :: FM_SELECT_HELICITY
  public :: FM_FACTOR_HELICITY
  public :: event_test

  type :: event_t
     private
     integer :: num_proc_id = 0
     type(process_t), pointer :: process => null ()
     type(event_vars_t), pointer :: vars => null ()
     type(decay_tree_t), pointer :: decay_tree => null ()
     logical :: particle_set_exists = .false.
     logical :: is_valid = .false.
     logical :: is_vetoed = .false.
     type(particle_set_t) :: particle_set
     real(default) :: excess = 0
  end type event_t

  type :: md5sum_events_t
    character(32), dimension(:), allocatable :: process
    character(32), dimension(:), allocatable :: parameters
    character(32), dimension(:), allocatable :: results
    character(32), dimension(:), allocatable :: polarized
    character(32) :: decays = ""
    character(32) :: simulation = ""
  end type md5sum_events_t


contains

  subroutine event_init (event, process, event_vars, decay_tree)
    type(event_t), intent(out) :: event
    type(process_t), intent(in), target :: process
    type(event_vars_t), intent(in), target :: event_vars
    type(decay_tree_t), intent(in), optional, target :: decay_tree
    event%process => process
    event%vars => event_vars
    if (present (decay_tree))  event%decay_tree => decay_tree
  end subroutine event_init

  subroutine event_final (event)
    type(event_t), intent(inout) :: event
    call particle_set_final (event%particle_set)
    event%is_valid = .false.
  end subroutine event_final

  subroutine event_write (event, analysis_expr, unit, verbose)
    type(event_t), intent(in) :: event
    type(eval_tree_t), intent(in), optional :: analysis_expr
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) repeat ("=", 72)
    write (u, *) "Event record:"
    if (.not. event%is_valid)  write (u, *) "  [invalid event record]"
    if (associated (event%vars)) then
       call event_vars_write (event%vars, unit)
    end if
    if (associated (event%process)) then
       if (present (verbose)) then
          if (verbose) then
             call process_write (event%process, unit)
             if (present (analysis_expr)) then
                write (u, "(A)")  "Analysis expression:"
                call eval_tree_write (analysis_expr, unit)
                write (u, "(A)")  repeat ("#", 79)
             end if
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
  end subroutine event_write

  subroutine event_generate (event, rng, unweighted, &
       factorization_mode, keep_correlations, keep_virtual, os_data, &
       shower_settings)
    type(event_t), intent(inout), target :: event
    type(tao_random_state), intent(inout) :: rng
    logical, intent(in) :: unweighted
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations, keep_virtual
    type(os_data_t), intent(in) :: os_data
!!! shower_settings should be intent(in), but the calls to ckkw_pseudo_shower_weights_init
!!! and ckkw_fake_pseudo_shower_weights force it to be declared as intent(inout)
!    type(shower_settings_t), intent(in), optional :: shower_settings
    type(shower_settings_t), intent(inout), optional :: shower_settings
    integer :: u

    event%is_vetoed = .false.
    if (unweighted) then
       call process_generate_unweighted_event &
            (event%process, rng, event%vars%excess)
       event%vars%weight = 1
    else
       call process_generate_weighted_event &
            (event%process, rng, event%vars%weight)
       event%vars%excess = 0
    end if
    event%vars%n_in = process_get_n_in (event%process)
    event%vars%n_out = process_get_n_out (event%process)
    event%vars%n_tot = process_get_n_tot (event%process)
    event%vars%sqrts = process_get_sqrts (event%process)
    event%vars%sqrts_hat = process_get_sqrts_hat (event%process)
    event%vars%sqme = process_get_sqme (event%process)
    event%vars%sqme_ref = event%vars%sqme
    if (associated (event%decay_tree)) then
       call decay_tree_generate_event (event%decay_tree, rng)
    end if
    call event_factorize_process (event, rng, &
         factorization_mode, keep_correlations, keep_virtual)
    if(event%particle_set_exists.and.present (shower_settings)) then
       call event_assure_heprup(event)

       if(shower_settings%ckkw_matching) then
          call ckkw_pseudo_shower_weights_init(shower_settings%ckkw_weights)
          call ckkw_fake_pseudo_shower_weights(shower_settings%ckkw_settings, &
               shower_settings%ckkw_weights, event%particle_set)
       end if
      
       call apply_shower_particle_set(event%particle_set, & 
            shower_settings, &
            process_get_model_ptr(event%process), &
            os_data, &
            process_get_strfun_type(event%process), &
            process_get_strfun_set(event%process), &
            event%is_valid, event%is_vetoed)
    end if
  end subroutine event_generate

  subroutine ckkw_fake_pseudo_shower_weights(ckkw_pseudo_shower_settings, &
       ckkw_pseudo_shower_weights, particle_set)
    type(ckkw_matching_settings_t), intent(inout) :: ckkw_pseudo_shower_settings
    type(ckkw_pseudo_shower_weights_t), intent(inout) :: ckkw_pseudo_shower_weights
    type(particle_set_t), intent(in) :: particle_set
    integer :: i, j, k
    integer :: n
    type(vector4_t) :: momentum

    ckkw_pseudo_shower_settings%alphaS = 1.0_default
    ckkw_pseudo_shower_settings%Qmin = 1.0_default
    ckkw_pseudo_shower_settings%n_max_jets = 3

    n = 2**particle_set_get_n_tot(particle_set)
    if(allocated(ckkw_pseudo_shower_weights%weights)) then 
      deallocate(ckkw_pseudo_shower_weights%weights)
    end if
    allocate(ckkw_pseudo_shower_weights%weights(1:n))
    do i=1,n
       momentum = vector4_null
       do j=1, particle_set_get_n_tot(particle_set)
          if(btest(i,j-1)) then
             momentum = momentum + particle_get_momentum(particle_set_get_particle(particle_set, j))
          end if
       end do
       if(momentum**1 > 0.0) then
          ckkw_pseudo_shower_weights%weights(i) = 1.0 / (momentum**2)
       end if
    end do

    ! equally distribute the weights by type
    if(allocated(ckkw_pseudo_shower_weights%weights_by_type)) then
       deallocate(ckkw_pseudo_shower_weights%weights_by_type)
    end if
    allocate(ckkw_pseudo_shower_weights%weights_by_type(1:n, 0:4))
    do i=1,n
       do j=0,4
          ckkw_pseudo_shower_weights%weights_by_type(i,j) = 0.2 * ckkw_pseudo_shower_weights%weights(i)
       end do
    end do
  end subroutine ckkw_fake_pseudo_shower_weights
  subroutine event_decay (event, rng, decay_tree)
    type(event_t), intent(inout) :: event
    type(tao_random_state), intent(inout) :: rng
    type(decay_tree_t), intent(in), target :: decay_tree
    if (event%is_valid) then
       call process_complete_evaluators (event%process)
       event%decay_tree => decay_tree
       call decay_tree_generate_event (event%decay_tree, rng)
    end if
  end subroutine event_decay

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
    if (associated (event%decay_tree)) then
       int_sqme => evaluator_get_int_ptr &
            (decay_tree_get_eval_sqme_ptr  (event%decay_tree))
       int_flows => evaluator_get_int_ptr &
            (decay_tree_get_eval_flows_ptr (event%decay_tree))
    else
       int_sqme => evaluator_get_int_ptr &
            (process_get_eval_sqme_ptr (event%process))
       int_flows => evaluator_get_int_ptr &
            (process_get_eval_flows_ptr (event%process))
    end if
    call tao_random_number (rng, r)
    if (interaction_get_n_in (int_sqme) /= 0) then
       call particle_set_init (event%particle_set, event%is_valid, &
            int_sqme, int_flows, factorization_mode, r, &
            keep_correlations, keep_virtual)
    else
       call particle_set_init (event%particle_set, event%is_valid, &
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
    
  subroutine event_recover_process (event)
    type(event_t), intent(inout) :: event
    call process_recover_kinematics (event%process, event%particle_set)
    call process_fill_subevt (event%process)
  end subroutine event_recover_process

  subroutine event_compute_scale (event)
    type(event_t), intent(inout) :: event
    call process_compute_scale (event%process)
  end subroutine event_compute_scale

  subroutine event_update_parameters (event)
    type(event_t), intent(inout) :: event
    call process_update_parameters (event%process)
  end subroutine event_update_parameters

  subroutine event_update_alpha_s (event)
    type(event_t), intent(inout) :: event
    call process_update_alpha_s (event%process)
  end subroutine event_update_alpha_s

  subroutine event_compute_sqme (event)
    type(event_t), intent(inout) :: event
    call process_evaluate (event%process)
    event%vars%sqme = process_get_sqme (event%process)
  end subroutine event_compute_sqme

  subroutine event_update_weight (event)
    type(event_t), intent(inout) :: event
    if (event%vars%sqme_ref /= 0) then
       call event_renormalize_weight &
            (event, event%vars%sqme / event%vars%sqme_ref)
    end if
  end subroutine event_update_weight

  function event_passes_selection (event, subevt, selection_expr) result (flag)
    logical :: flag
    type(event_t), intent(inout), target :: event
    type(subevt_t), intent(inout), target :: subevt
    type(eval_tree_t), intent(inout), target :: selection_expr
    real(default) :: factor
    if (event%is_valid .and. eval_tree_is_defined (selection_expr)) then
       call particle_set_to_subevt (event%particle_set, subevt)
       call eval_tree_evaluate (selection_expr)
       flag = eval_tree_get_log (selection_expr)
    else
       flag = .true.
    end if
  end function event_passes_selection

  subroutine event_renormalize_weight (event, factor)
    type(event_t), intent(inout) :: event
    real(default), intent(in) :: factor
    event%vars%weight = event%vars%weight * factor
  end subroutine event_renormalize_weight

  subroutine event_reweight (event, subevt, reweight_expr)
    type(event_t), intent(inout), target :: event
    type(subevt_t), intent(inout), target :: subevt
    type(eval_tree_t), intent(inout), target :: reweight_expr
    real(default) :: factor
    if (event%is_valid .and. eval_tree_is_defined (reweight_expr)) then
       call particle_set_to_subevt (event%particle_set, subevt)
       call eval_tree_evaluate (reweight_expr)
       factor = eval_tree_get_real (reweight_expr)
       call event_renormalize_weight (event, factor)
    end if
  end subroutine event_reweight

  subroutine event_do_analysis (event, subevt, analysis_expr)
    type(event_t), intent(inout), target :: event
    type(subevt_t), intent(inout), target :: subevt
    type(eval_tree_t), intent(inout), target :: analysis_expr
    if (event%is_valid .and. eval_tree_is_defined (analysis_expr)) then
       call particle_set_to_subevt (event%particle_set, subevt)
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

  function event_is_valid (event) result (flag)
    logical :: flag
    type(event_t), intent(in) :: event
    flag = event%is_valid
  end function event_is_valid

  function event_is_vetoed (event) result (flag)
    logical :: flag
    type(event_t), intent(in) :: event
    flag = event%is_vetoed
  end function event_is_vetoed

  function is_raw_event_file (unit) result (flag)
    logical :: flag
    integer, intent(in) :: unit
    character(len=len(RAW_EVENT_FILE_ID_STRING)) :: id_string
    integer :: iostat
    read (unit, iostat=iostat)  id_string
    if (iostat /= 0) then
       flag = .false.
    else if (id_string /= RAW_EVENT_FILE_ID_STRING) then
       flag = .false.
    else
       flag = .true.
    end if
  end function is_raw_event_file

  subroutine raw_event_file_write_header (unit, md5sum, version)
    integer, intent(in) :: unit
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(in) :: version
    write (unit)  RAW_EVENT_FILE_ID_STRING
    write (unit)  version
    write (unit)  size (md5sum%process)
    write (unit)  md5sum%process
    write (unit)  md5sum%parameters
    write (unit)  md5sum%results
    write (unit)  md5sum%polarized
    write (unit)  md5sum%decays
    write (unit)  md5sum%simulation
  end subroutine raw_event_file_write_header

  subroutine raw_event_file_read_header &
       (unit, rescan, check, md5sum, version, ok, iostat)
    integer, intent(in) :: unit
    logical, intent(in) :: rescan, check
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(in) :: version
    logical, intent(out) :: ok
    integer, intent(out), optional :: iostat
    character(len=len(RAW_EVENT_FILE_ID_STRING)) :: id_string
    integer :: file_version, n
    character(32), dimension(:), allocatable :: md5sum_array
    character(32) :: md5sum_single
    logical :: unweighted
    ok = .false.
    read (unit, iostat=iostat)  id_string
    if (check .and. id_string /= RAW_EVENT_FILE_ID_STRING) then
       call msg_fatal &
            ("File doesn't appear to be a WHIZARD raw event file")
       return
    end if
    read (unit, iostat=iostat)  file_version
    if (check .and. file_version /= version) then
       call msg_fatal &
            ("Event-file format version mismatch")
       return
    end if
    read (unit, iostat=iostat)  n
    if (check .and. n /= size (md5sum%process)) then
       call msg_message &
            ("Process number has changed, discarding old event file")
       return
    end if
    allocate (md5sum_array (n))
    read (unit, iostat=iostat)  md5sum_array
    if (check .and. any (md5sum%process /= md5sum_array)) then
       call msg_message &
            ("Process configuration has changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_array
    if (check .and. .not. rescan &
         .and. any (md5sum%parameters /= md5sum_array)) then
       call msg_message &
            ("Model parameters have changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_array
    if (check .and. .not. rescan &
         .and. any (md5sum%results /= md5sum_array)) then
       call msg_message &
            ("Integration results have changed, skipping event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_array
    if (check .and. any (md5sum%polarized /= md5sum_array)) then
       call msg_message &
            ("Polarization setup has changed, discarding old event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_single
    if (check .and. .not. rescan .and. md5sum%decays /= md5sum_single) then
       call msg_message &
            ("Decay configuration has changed, skipping event file")
       return
    end if
    read (unit, iostat=iostat)  md5sum_single
    if (check .and. md5sum%simulation /= md5sum_single) then
       call msg_message &
            ("Simulation parameters have changed, skipping event file")
       return
    end if
    ok = .true.
  end subroutine raw_event_file_read_header

  subroutine event_write_raw (event, unit, version)
    type(event_t), intent(in) :: event
    integer, intent(in) :: unit
    integer, intent(in) :: version
    if (event%is_valid) then
       if (.not. associated (event%process)) &
            call msg_bug ("Writing event: process not associated")
       if (.not. associated (event%vars)) &
            call msg_bug ("Writing event: event variables not associated")
       call event_vars_write_raw (event%vars, unit, version)
       write (unit)  process_get_scale (event%process)       
       write (unit)  process_get_fac_scale (event%process)
       write (unit)  process_get_ren_scale (event%process)       
       write (unit)  process_get_alpha_s (event%process)
       call particle_set_write_raw (event%particle_set, unit)
    end if
  end subroutine event_write_raw

  subroutine event_read_raw &
       (event, unit, event_vars, prc_array, num_id_array, iostat, version)
    type(event_t), intent(out) :: event
    integer, intent(in) :: unit
    type(event_vars_t), intent(inout), target :: event_vars
    type(process_p), dimension(:), intent(in) :: prc_array
    integer, dimension(:), intent(in), optional :: num_id_array
    integer, intent(out) :: iostat
    integer, intent(in) :: version
    integer :: proc
    type(process_t), pointer :: process
    real(default) :: scale, ren_scale, fac_scale, alpha_s, sqme
    call event_vars_read_raw (event_vars, unit, iostat, version)
    if (iostat /= 0) return
    proc = event_vars%process_index
    if (proc > 0 .and. proc <= size (prc_array)) then
       process => prc_array(proc)%ptr
       event_vars%process_id = process_get_id (process)
       if (present (num_id_array)) then
          event_vars%process_num_id = num_id_array(proc)
       else
          event_vars%process_num_id = proc
       end if
    else
       call msg_fatal ("Invalid process index encountered in raw event file")
       return
    end if
    call event_init (event, process, event_vars)
    event%is_valid = .true.
    read (unit, iostat=iostat)  scale    
    if (iostat /= 0)  return
    read (unit, iostat=iostat)  fac_scale
    if (iostat /= 0)  return
    read (unit, iostat=iostat)  ren_scale    
    if (iostat /= 0)  return
    read (unit, iostat=iostat)  alpha_s
    if (iostat /= 0)  return
    call particle_set_read_raw (event%particle_set, unit, iostat=iostat)
    if (iostat /= 0)  return
    event%particle_set_exists = .true.
    if (associated (event%process)) then
       call process_set_particles (event%process, event%particle_set)
       call process_set_scale (event%process, scale)
       call process_set_fac_scale (event%process, fac_scale)
       call process_set_ren_scale (event%process, ren_scale)       
       call process_set_alpha_s (event%process, alpha_s)
       call process_set_sqme (event%process, event%vars%sqme)
    end if
  end subroutine event_read_raw
    
  function is_hepmc_event_file (u) result (flag)
    logical :: flag
    integer, intent(in) :: u
    integer :: iostat
    character(*), parameter :: HEPMC_ID_STRING = "HepMC::Version"
    character(len=len(HEPMC_ID_STRING)) :: id_string
    id_string = ""
    do while (id_string == "")
       read (u, "(A)", iostat=iostat)  id_string
       if (iostat /= 0)  exit
    end do
    if (iostat == 0) then
       flag = id_string == HEPMC_ID_STRING
    else
       flag = .false.
    end if
  end function is_hepmc_event_file

  subroutine event_read_from_hepmc (event, hepmc_event, polarization_mode, &
       event_vars, prc_array, num_id_array)
    type(event_t), intent(out) :: event
    type(hepmc_event_t), intent(in) :: hepmc_event
    integer, intent(in) :: polarization_mode
    type(event_vars_t), intent(inout), target :: event_vars
    type(process_p), dimension(:), intent(in) :: prc_array
    integer, dimension(:), intent(in), optional :: num_id_array
    real(default) :: scale, alpha_s
    integer :: num_id, proc, n_weights
    type(process_t), pointer :: process
    num_id = hepmc_event_get_process_id (hepmc_event)
    proc = get_process_index (num_id, num_id_array)
    if (proc > 0 .and. proc <= size (prc_array)) then
       process => prc_array(proc)%ptr
       call event_init (event, process, event_vars)
       event%is_valid = .true.
       scale = hepmc_event_get_scale (hepmc_event)
       if (scale > 0)  call process_set_fac_scale (process, scale)
       alpha_s = hepmc_event_get_alpha_qcd (hepmc_event)
       if (alpha_s > 0)  call process_set_alpha_s (process, alpha_s)
       event_vars%event_index = hepmc_event_get_event_index (hepmc_event)
       event_vars%process_index = proc
       event_vars%process_id = process_get_id (process)
       event_vars%process_num_id = num_id
       event_vars%sqrts_hat = 0
       n_weights = hepmc_event_get_weights_size (hepmc_event)
       if (n_weights > 0) then
          event_vars%weight = hepmc_event_get_weight (hepmc_event, 1)
       else
          event_vars%weight = 1
       end if
       event_vars%excess   = hepmc_event_get_weight (hepmc_event, 2)
       event_vars%sqme     = hepmc_event_get_weight (hepmc_event, 3)
       event_vars%sqme_ref = hepmc_event_get_weight (hepmc_event, 4)
       call particle_set_init (event%particle_set, hepmc_event, &
            process_get_model_ptr (event%process), polarization_mode)
       event%particle_set_exists = .true.
    else
       call hepmc_event_print (hepmc_event)
       write (msg_buffer, "(A,I0,A)") "HepMC event: process ID ", &
            proc, " is invalid in the current context"
       call msg_fatal ()
    end if
  end subroutine event_read_from_hepmc

  subroutine event_write_to_hepmc (event, hepmc_event)
    type(event_t), intent(in) :: event
    type(hepmc_event_t), intent(inout) :: hepmc_event
    if (event%is_valid) then
       call hepmc_event_set_process_id (hepmc_event, event%vars%process_num_id)
       call hepmc_event_clear_weights (hepmc_event)
       call hepmc_event_add_weight (hepmc_event, event%vars%weight)
       call hepmc_event_add_weight (hepmc_event, event%vars%excess)
       call hepmc_event_add_weight (hepmc_event, event%vars%sqme)
       call hepmc_event_add_weight (hepmc_event, event%vars%sqme_ref)
       call hepmc_event_set_scale (hepmc_event, &
            process_get_fac_scale (event%process))
       call hepmc_event_set_alpha_qcd (hepmc_event, &
            process_get_alpha_s (event%process))
!        call hepmc_event_set_cross_section (hepmc_event, &
!             process_get_integral (event%process), &
!             process_get_error (event%process))
       call particle_set_fill_hepmc_event (event%particle_set, hepmc_event)
    end if
  end subroutine event_write_to_hepmc

  subroutine event_write_to_hepeup (event, keep_beams)
    type(event_t), intent(in) :: event
    integer :: proc_id
    logical, intent(in), optional :: keep_beams  
    real(default) :: scale, alpha_qcd
    if (event%is_valid) then
       call particle_set_fill_hepeup (event%particle_set, keep_beams)
       if (associated (event%process)) then
          call hepeup_set_event_parameters (proc_id = event%vars%process_num_id)
          scale = process_get_fac_scale (event%process)
          if (scale /= 0)  call hepeup_set_event_parameters (scale = scale)
          alpha_qcd = process_get_alpha_s (event%process)       
          if (alpha_qcd /= 0) &
               call hepeup_set_event_parameters (alpha_qcd = alpha_qcd)
          call hepeup_set_event_parameters (weight = event%vars%weight)
       end if
    end if
  end subroutine event_write_to_hepeup

  subroutine event_assure_heprup (event)
    type(event_t), intent(in) :: event
    integer :: i
    integer, parameter :: min_processes = 10

  integer, parameter :: MAXPUP = 100
  integer, parameter :: MAXNUP = 500
  integer, dimension(2) :: IDBMUP
  double precision, dimension(2) :: EBMUP
  integer, dimension(2) :: PDFGUP
  integer, dimension(2) :: PDFSUP
  integer :: IDWTUP
  integer :: NPRUP
  double precision, dimension(MAXPUP) :: XSECUP
  double precision, dimension(MAXPUP) :: XERRUP
  double precision, dimension(MAXPUP) :: XMAXUP
  integer, dimension(MAXPUP) :: LPRUP
  integer :: NUP
  integer :: IDPRUP
  double precision :: XWGTUP
  double precision :: SCALUP
  double precision :: AQEDUP
  double precision :: AQCDUP
  integer, dimension(MAXNUP) :: IDUP
  integer, dimension(MAXNUP) :: ISTUP
  integer, dimension(2,MAXNUP) :: MOTHUP
  integer, dimension(2,MAXNUP) :: ICOLUP
  double precision, dimension(5,MAXNUP) :: PUP
  double precision, dimension(MAXNUP) :: VTIMUP
  double precision, dimension(MAXNUP) :: SPINUP
  integer, parameter :: NMXHEP = 4000

  integer :: NEVHEP

  integer :: NHEP

  integer, dimension(NMXHEP) :: ISTHEP

  integer, dimension(NMXHEP) :: IDHEP

  integer, dimension(2, NMXHEP) :: JMOHEP

  integer, dimension(2, NMXHEP) :: JDAHEP

  double precision, dimension(5, NMXHEP) :: PHEP
  
  double precision, dimension(4, NMXHEP) :: VHEP
  
  integer, dimension(NMXHEP) :: hepevt_pol

  integer :: hepevt_n_out, hepevt_n_remnants

  double precision :: hepevt_weight, hepevt_function_value
  double precision :: hepevt_function_ratio
  
  common /HEPRUP/ &
       IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP, &
       XSECUP, XERRUP, XMAXUP, LPRUP
  save /HEPRUP/

  common /HEPEUP/ &
       NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP, &
       IDUP, ISTUP, MOTHUP, ICOLUP, PUP, VTIMUP, SPINUP
  save /HEPEUP/

  common /HEPEVT/ &
       NEVHEP, NHEP, ISTHEP, IDHEP, &
       JMOHEP, JDAHEP, PHEP, VHEP
  save /HEPEVT/
  

    if (.not.event%is_valid) return
    if(LPRUP(event%vars%process_num_id).ne.0) return

    call heprup_init( &
         (/particle_get_pdg(particle_set_get_particle(event%particle_set, 1)), &
         particle_get_pdg(particle_set_get_particle(event%particle_set, 2)) /) , &
         (/vector4_get_component(particle_get_momentum(particle_set_get_particle(event%particle_set, 1)), 0),&
         vector4_get_component(particle_get_momentum(particle_set_get_particle(event%particle_set, 1)), 0) /), &
         event%vars%process_num_id, .false., .false. )
    do i=1, (event%vars%process_num_id/min_processes+1)*min_processes
       call heprup_set_process_parameters (i = i, process_id = &
            i, cross_section = 1._default, error = 1._default)
    end do
  end subroutine event_assure_heprup

  subroutine event_write_to_hepevt (event, keep_beams)
    type(event_t), intent(in) :: event
    integer :: proc_id
    logical, intent(in), optional :: keep_beams  
    if (event%is_valid) then
       call particle_set_fill_hepevt (event%particle_set, keep_beams)
       call hepevt_set_event_parameters ( &
            weight = event%vars%weight, &
            function_value = event%vars%sqme, &
            i_evt = event%vars%event_index)
    end if
  end subroutine event_write_to_hepevt

  function get_process_index (num_id, num_id_array) result (proc)
    integer :: proc
    integer, intent(in) :: num_id
    integer, dimension(:), intent(in), optional :: num_id_array
    if (present (num_id_array)) then
       do proc = 1, size (num_id_array)
          if (num_id_array(proc) == num_id)  return
       end do
       write (msg_buffer, "(A,I0,A)")  "Reading events: numeric process ID ", &
            num_id, " does not match any process"
       call msg_fatal
       proc = 0
    else
       proc = num_id
    end if
  end function get_process_index

  function event_get_process_ptr (event) result (process)
    type(process_t), pointer :: process
    type(event_t), intent(in) :: event
    process => event%process
  end function event_get_process_ptr

  subroutine event_test ()
    type(os_data_t), pointer :: os_data => null ()
    type(process_library_t), pointer :: prc_lib => null ()
    type(event_t), target :: event
    type(model_t), pointer :: model
    type(var_list_t), pointer :: var_list => null ()
    print *, "*** Read model file"
    allocate (os_data)
    allocate (prc_lib)
    allocate (var_list)
    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)
    var_list => model_get_var_list_ptr (model)
    call syntax_pexpr_init ()
    call syntax_phs_forest_init ()
    print *
    print *, "*** Load process library"
    call var_list_append_string (var_list, name = "$library_name", sval = "test_me")  ! $
    call var_list_append_log (var_list, name = "?read_color_factors", lval = .true.)
    call var_list_append_log (var_list, name = "?alpha_s_is_fixed", lval = .true.)
    call process_library_init (prc_lib, var_str("test_me"), os_data)
    call process_library_load (prc_lib, os_data, var_list = var_list)
    print *
    call event_test1 (prc_lib, model, os_data, var_list)
    print *
    print *, "* Cleanup"
    call event_final (event)
    call process_store_final ()
    call syntax_pexpr_final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
    call process_library_final (prc_lib)
    deallocate (os_data)
    deallocate (prc_lib)
  end subroutine event_test

  subroutine event_test1 (prc_lib, model, os_data, var_list)
    type(process_library_t), intent(inout) :: prc_lib
    type(model_t), intent(in), target :: model
    type(os_data_t), intent(in) :: os_data
    type(var_list_t), target :: var_list
    type(process_t), pointer :: process
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
    type(event_vars_t), target :: event_vars
    type(event_t), target :: event
    type(decay_tree_t), target :: decay_tree
    logical :: rebuild_phs = .true.
    print *, "*** Test process setup"
    print *
    print *, "* Initialization"
    call tao_random_create (rng, 0)
    call process_store_init_process &
         (process, prc_lib, var_str ("test_me_unit_col"), model, &
         var_list, use_beams = .false.)
    print *, "  Process ID = ", char (process_get_id (process))
    print *
    print *, "* Beam setup"
    print *
    call flavor_init (flv, (/ 2, -2 /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call process_setup_beams (process, beam_data, 0, sqrts = 1000._default)
    call process_connect_strfun (process)
    call process_setup_subevt (process)
    print *
    print *, "* Phase space setup"
    call openmp_set_num_threads_verbose (1) 
    call process_setup_phase_space (process, rebuild_phs, &
         os_data, phs_par, mapping_defaults, filename_out=var_str("test_me_unit_col.phs"), &
         vis_channels = .false.)
    print *
    print *, "* Cuts setup"
    call stream_init (stream, var_str ("all Pt > 200 GeV [g]"))
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
    call process_init_vamp_history (process, 8)
    call openmp_set_num_threads_verbose (1) 
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
    call event_init (event, process, event_vars, decay_tree=decay_tree)
    print *
    print *, "* Weighted event"
    call event_generate &
         (event, rng, .false., FM_IGNORE_HELICITY, .false., .false., os_data)
    call event_write (event)
    print *
    print *, "* Unweighted event"
    call event_generate &
         (event, rng, .true., FM_SELECT_HELICITY, .false., .true., os_data)
    call event_write (event)
    print *, "  Process data written to fort.81"
    call process_write (process, 81)
    call event_final (event)
  end subroutine event_test1


end module events
