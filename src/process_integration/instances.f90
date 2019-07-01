! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     So Young Shim <soyoung.shim@desy.de>
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

module instances

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use constants
  use diagnostics
  use os_interface
  use numeric_utils
  use lorentz
  use mci_base
  use particles
  use interactions
  use quantum_numbers
  use model_data
  use helicities
  use flavors
  use beam_structures
  use variables
  use sf_base
  use physics_defs
  use process_constants
  use process_libraries
  use state_matrices
  use integration_results
  use phs_base
  use prc_core, only: prc_core_t, prc_core_state_t

  !!! We should depend less on these modules (move it to pcm_nlo_t e.g.)
  use phs_wood, only: phs_wood_t
  use phs_fks, only: phs_fks_t, phs_fks_config_t
  use phs_fks, only: GEN_REAL_PHASE_SPACE, GEN_SOFT_MISMATCH
  use phs_fks, only: GEN_SOFT_LIMIT_TEST, GEN_COLL_LIMIT_TEST, GEN_ANTI_COLL_LIMIT_TEST
  use phs_fks, only: PHS_MODE_UNDEFINED, GEN_SOFT_COLL_LIMIT_TEST, GEN_SOFT_ANTI_COLL_LIMIT_TEST
  use phs_fks, only: PHS_MODE_ADDITIONAL_PARTICLE, PHS_MODE_COLLINEAR_REMNANT
  use blha_olp_interfaces, only: prc_blha_t
  use blha_config, only: BLHA_AMP_CC
  use prc_user_defined, only: prc_user_defined_base_t, user_defined_state_t
  use prc_threshold, only: prc_threshold_t
  use blha_olp_interfaces, only: blha_result_array_size

  !!! local modules
  use parton_states
  use process_counter
  use pcm_base
  use pcm
  use core_manager
  use process_config
  use process_mci
  use process
  use kinematics

  implicit none
  private

  public :: process_instance_t
  public :: pacify

  type :: term_instance_t
     type(process_term_t), pointer :: config => null ()
     logical :: active = .false.
     type(kinematics_t) :: k_term
     complex(default), dimension(:), allocatable :: amp
     type(interaction_t) :: int_hard
     type(isolated_state_t) :: isolated
     type(connected_state_t) :: connected
     class(prc_core_state_t), allocatable :: core_state
     logical :: checked = .false.
     logical :: passed = .false.
     real(default) :: scale = 0
     real(default) :: fac_scale = 0
     real(default) :: ren_scale = 0
     real(default), allocatable :: alpha_qcd_forced
     real(default) :: weight = 1
     type(vector4_t), dimension(:), allocatable :: p_seed
     type(vector4_t), dimension(:), allocatable :: p_hard
     type(pcm_instance_nlo_t), pointer :: pcm_instance => null ()
     integer :: nlo_type = BORN
     integer, dimension(:), allocatable :: same_kinematics
   contains
     procedure :: write => term_instance_write
     procedure :: final => term_instance_final
     procedure :: init => term_instance_init
     procedure :: init_from_process => term_instance_init_from_process
     procedure :: setup_kinematics => term_instance_setup_kinematics
     procedure :: setup_fks_kinematics => term_instance_setup_fks_kinematics
     procedure :: compute_seed_kinematics => term_instance_compute_seed_kinematics
     procedure :: evaluate_radiation_kinematics => term_instance_evaluate_radiation_kinematics
     procedure :: evaluate_projections => term_instance_evaluate_projections
     procedure :: redo_sf_chain => term_instance_redo_sf_chain
     procedure :: recover_mcpar => term_instance_recover_mcpar
     procedure :: compute_hard_kinematics => &
          term_instance_compute_hard_kinematics
     procedure :: recover_seed_kinematics => &
          term_instance_recover_seed_kinematics
     procedure :: compute_other_channels => &
          term_instance_compute_other_channels
     procedure :: return_beam_momenta => term_instance_return_beam_momenta
     procedure :: apply_damping_factor => term_instance_apply_damping_factor
     procedure :: get_lorentz_transformation => term_instance_get_lorentz_transformation
     procedure :: set_emitter => term_instance_set_emitter
     procedure :: set_threshold => term_instance_set_threshold
     procedure :: setup_expressions => term_instance_setup_expressions
     procedure :: setup_event_data => term_instance_setup_event_data
     procedure :: evaluate_color_correlations => &
        term_instance_evaluate_color_correlations
     procedure :: apply_fks => term_instance_apply_fks
     procedure :: evaluate_sqme_virt => term_instance_evaluate_sqme_virt
     procedure :: evaluate_sqme_mismatch => term_instance_evaluate_sqme_mismatch
     procedure :: evaluate_sqme_dglap => term_instance_evaluate_sqme_dglap
     procedure :: reset => term_instance_reset
     procedure :: set_alpha_qcd_forced => term_instance_set_alpha_qcd_forced
     procedure :: compute_eff_kinematics => &
          term_instance_compute_eff_kinematics
     procedure :: recover_hard_kinematics => &
          term_instance_recover_hard_kinematics
     procedure :: evaluate_expressions => &
          term_instance_evaluate_expressions
     procedure :: evaluate_interaction => term_instance_evaluate_interaction
     procedure :: evaluate_interaction_default &
        => term_instance_evaluate_interaction_default
     procedure :: evaluate_interaction_userdef &
        => term_instance_evaluate_interaction_userdef
     procedure :: evaluate_interaction_userdef_tree &
        => term_instance_evaluate_interaction_userdef_tree
     procedure :: evaluate_interaction_userdef_loop &
        => term_instance_evaluate_interaction_userdef_loop
     procedure :: compute_virt_me_array_sizes => &
          term_instance_compute_virt_me_array_sizes
     procedure :: evaluate_trace => term_instance_evaluate_trace
     procedure :: evaluate_event_data => term_instance_evaluate_event_data
     procedure :: set_fac_scale => term_instance_set_fac_scale
     procedure :: get_fac_scale => term_instance_get_fac_scale
     procedure :: get_alpha_s => term_instance_get_alpha_s
     procedure :: reset_phs_identifiers => term_instance_reset_phs_identifiers
     procedure :: get_helicities_for_openloops => term_instance_get_helicities_for_openloops
     procedure :: get_boost_to_lab => term_instance_get_boost_to_lab
     procedure :: get_boost_to_cms => term_instance_get_boost_to_cms
  end type term_instance_t

  type, extends (mci_sampler_t) :: process_instance_t
     type(process_t), pointer :: process => null ()
     integer :: evaluation_status = STAT_UNDEFINED
     real(default) :: sqme = 0
     real(default) :: weight = 0
     real(default) :: excess = 0
     integer :: i_mci = 0
     integer :: selected_channel = 0
     type(sf_chain_t) :: sf_chain
     type(term_instance_t), dimension(:), allocatable :: term
     type(mci_work_t), dimension(:), allocatable :: mci_work
     class(pcm_instance_t), allocatable :: pcm
   contains
     procedure :: write_header => process_instance_write_header
     procedure :: write => process_instance_write
     procedure :: init => process_instance_init
     procedure :: final => process_instance_final
     procedure :: reset => process_instance_reset
     procedure :: sampler_test => process_instance_sampler_test
     procedure :: generate_weighted_event => process_instance_generate_weighted_event
     procedure :: generate_unweighted_event => process_instance_generate_unweighted_event
     procedure :: recover_event => process_instance_recover_event
     procedure :: activate => process_instance_activate
     procedure :: find_same_kinematics => process_instance_find_same_kinematics
     procedure :: transfer_same_kinematics => process_instance_transfer_same_kinematics
     procedure :: redo_sf_chains => process_instance_redo_sf_chains
     procedure :: integrate => process_instance_integrate
     procedure :: transfer_helicities => process_instance_transfer_helicities
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
     procedure :: apply_powheg_dampings => process_instance_apply_powheg_dampings
     procedure :: evaluate_event_data => process_instance_evaluate_event_data
     procedure :: compute_sqme_rad => process_instance_compute_sqme_rad
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
     procedure :: get_actual_calls_total => process_instance_get_actual_calls_total
     procedure :: reset_matrix_elements => process_instance_reset_matrix_elements
     procedure :: get_phase_space_point &
        => process_instance_get_phase_space_point
     procedure :: get_associated_real => process_instance_get_associated_real
     procedure :: get_connected_states => process_instance_get_connected_states
     procedure :: get_sqrts => process_instance_get_sqrts
     procedure :: get_trace => process_instance_get_trace
     procedure :: set_trace => process_instance_set_trace
     procedure :: set_alpha_qcd_forced => process_instance_set_alpha_qcd_forced
     procedure :: has_nlo_component => process_instance_has_nlo_component
     procedure :: keep_failed_events => process_instance_keep_failed_events
     procedure :: create_and_load_extra_libraries &
        => process_instance_create_and_load_extra_libraries
     procedure :: get_boost_to_lab => process_instance_get_boost_to_lab
     procedure :: get_boost_to_cms => process_instance_get_boost_to_cms
     procedure :: is_cm_frame => process_instance_is_cm_frame
  end type process_instance_t


  interface pacify
     module procedure pacify_process_instance
  end interface pacify


contains

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
    if (allocated (term%amp)) deallocate (term%amp)
    if (allocated (term%core_state)) deallocate (term%core_state)
    if (allocated (term%alpha_qcd_forced)) &
       deallocate (term%alpha_qcd_forced)
    if (allocated (term%p_seed)) deallocate(term%p_seed)
    if (allocated (term%p_hard)) deallocate (term%p_hard)
    call term%k_term%final ()
    call term%connected%final ()
    call term%isolated%final ()
    call term%int_hard%final ()
    term%pcm_instance => null ()
  end subroutine term_instance_final

  subroutine term_instance_init (term, process, i_term, real_finite)
    class(term_instance_t), intent(inout), target :: term
    type(process_t), intent(in), target:: process
    integer, intent(in) :: i_term
    logical, intent(in), optional :: real_finite
    class(prc_core_t), pointer :: core => null ()
    type(process_beam_config_t) :: beam_config
    type(interaction_t), pointer :: sf_chain_int
    type(interaction_t), pointer :: src_int
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    type(state_matrix_t), pointer :: state_matrix
    type(flavor_t), dimension(:), allocatable :: flv_int, flv_src, f_in, f_out
    integer :: n_in, n_vir, n_out, n_tot, n_sub
    !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
    integer :: i, j, k
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    logical :: me_already_squared
    logical :: decrease_n_tot

    me_already_squared = .false.

    term%config => process%get_term_ptr (i_term)
    term%int_hard = term%config%int
    core => process%get_core_term (i_term)
    call core%allocate_workspace (term%core_state)
    select type (core)
    class is (prc_user_defined_base_t)
       call reduce_interaction (term%int_hard, &
          core%includes_polarization (), .true., .false.)
       me_already_squared = .true.
       allocate (term%amp (term%int_hard%get_n_matrix_elements ()))
    class default
       allocate (term%amp (term%config%n_allowed))
    end select
    term%amp = cmplx (0, 0, default)
    decrease_n_tot = term%nlo_type == NLO_REAL .and. &
         term%config%i_term_global /= term%config%i_sub
    if (present (real_finite)) then
       if (real_finite) decrease_n_tot = .false.
    end if
    if (decrease_n_tot) then
       allocate (term%p_seed (term%int_hard%get_n_tot () - 1))
    else
       allocate (term%p_seed (term%int_hard%get_n_tot ()))
    end if
    allocate (term%p_hard (term%int_hard%get_n_tot ()))
    sf_chain_int => term%k_term%sf_chain%get_out_int_ptr ()
    n_in = term%int_hard%get_n_in ()
    do j = 1, n_in
       i = term%k_term%sf_chain%get_out_i (j)
       call term%int_hard%set_source_link (j, sf_chain_int, i)
    end do
    call term%isolated%init (term%k_term%sf_chain, term%int_hard)
    allocate (mask_in (n_in))
    mask_in = term%k_term%sf_chain%get_out_mask ()
    select type (phs => term%k_term%phs)
      type is (phs_wood_t)
         if (me_already_squared) then
            call term%isolated%setup_identity_trace (core, mask_in, .true., .false.)
         else
            call term%isolated%setup_square_trace (core, mask_in, term%config%col)
         end if
      type is (phs_fks_t)
         select case (phs%mode)
         case (PHS_MODE_ADDITIONAL_PARTICLE)
            if (me_already_squared) then
               call term%isolated%setup_identity_trace (core, mask_in, .true., .false.)
            else
               call term%isolated%setup_square_trace (core, mask_in, term%config%col)
            end if
         case (PHS_MODE_COLLINEAR_REMNANT)
            if (me_already_squared) then
               call term%isolated%setup_identity_trace (core, mask_in)
            else
               call term%isolated%setup_square_trace (core, mask_in, term%config%col)
            end if
         end select
      class default
         call term%isolated%setup_square_trace (core, mask_in, term%config%col)
    end select
    if (term%nlo_type == NLO_VIRTUAL) then
       select type (pcm => process%get_pcm_ptr ())
       type is (pcm_nlo_t)
          n_sub = pcm%get_n_sub ()
       end select
    else
       !!! No integration of real subtraction in interactions yet
       n_sub = 0
    end if
    call term%connected%setup_connected_trace (term%isolated, &
       undo_helicities = undo_helicities (core, me_already_squared), &
       n_sub = n_sub, keep_fs_flavors = me_already_squared)

    associate (int_eff => term%isolated%int_eff)
      state_matrix => int_eff%get_state_matrix_ptr ()
      n_tot = int_eff%get_n_tot  ()
      !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
      allocate (flv_int (n_tot), qn (n_tot))
      qn = state_matrix%get_quantum_number (1)
      do k = 1, n_tot
         flv_int(k) = quantum_numbers_get_flavor (qn (k))
      end do
      deallocate (qn)
      !!! flv_int = quantum_numbers_get_flavor &
      !!!      (state_matrix%get_quantum_number (1))
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
          !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
          n_tot = src_int%get_n_tot ()
          allocate (flv_src (n_tot), qn (n_tot))
          qn = state_matrix%get_quantum_number (1)
          do k = 1, n_tot
             flv_src(k) = quantum_numbers_get_flavor (qn (k))
          end do
          deallocate (qn)
          !!! flv_src = quantum_numbers_get_flavor &
          !!!      (state_matrix%get_quantum_number (1))
          f_out(j) = flv_src(i)
          deallocate (flv_src)
       end if
    end do

    beam_config = process%get_beam_config ()

    call term%connected%setup_subevt (term%isolated%sf_chain_eff, &
         beam_config%data%flv, f_in, f_out)
    call term%connected%setup_var_list &
         (process%get_var_list_ptr (), beam_config%data)

  contains

   function undo_helicities (core, me_squared) result (val)
     logical :: val
     class(prc_core_t), intent(in) :: core
     logical, intent(in) :: me_squared
     select type (core)
     class is (prc_user_defined_base_t)
        val = me_squared .and. .not. core%includes_polarization ()
     class default
        val = .false.
     end select
   end function undo_helicities

   subroutine reduce_interaction (int, polarized_beams, keep_fs_flavors, &
      keep_colors)
     type(interaction_t), intent(inout) :: int
     logical, intent(in) :: polarized_beams
     logical, intent(in) :: keep_fs_flavors, keep_colors
     type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
     logical, dimension(:), allocatable :: mask_f, mask_c, mask_h
     integer :: n_tot, n_in
     n_in = int%get_n_in (); n_tot = int%get_n_tot ()
     allocate (qn_mask (n_tot))
     allocate (mask_f (n_tot), mask_c (n_tot), mask_h (n_tot))
     mask_c = .not. keep_colors
     mask_f (1 : n_in) = .false.
     if (keep_fs_flavors) then
        mask_f (n_in + 1 : ) = .false.
     else
        mask_f (n_in + 1 : ) = .true.
     end if
     if (polarized_beams) then
        mask_h (1 : n_in) = .false.
     else
        mask_h (1 : n_in) = .true.
     end if
     mask_h (n_in + 1 : ) = .true.
     call qn_mask%init (mask_f, mask_c, mask_h)
     call int%reduce_state_matrix (qn_mask)
   end subroutine

  end subroutine term_instance_init

  subroutine term_instance_init_from_process (term_instance, &
         process, i, pcm_instance, sf_chain)
    class(term_instance_t), intent(inout), target :: term_instance
    type(process_t), intent(in), target :: process
    integer, intent(in) :: i
    class(pcm_instance_t), intent(in), target :: pcm_instance
    type(sf_chain_t), intent(in), target :: sf_chain
    type(process_term_t) :: term
    integer :: i_component
    term = process%get_term_ptr (i)
    i_component = term%i_component
    if (i_component /= 0) then
       select type (pcm_instance)
       type is (pcm_instance_nlo_t)
          term_instance%pcm_instance => pcm_instance
       end select
       term_instance%nlo_type = process%get_nlo_type_component (i_component)
       call term_instance%setup_kinematics (sf_chain, &
          process%get_beam_config (), process%get_phs_config (i_component))
       call term_instance%init (process, i, &
            real_finite = process%component_is_real_finite (i_component))
       select type (phs => term_instance%k_term%phs)
       type is (phs_fks_t)
          call term_instance%set_emitter (process%get_pcm_ptr ())
          call term_instance%set_threshold (process%get_pcm_ptr ())
          call term_instance%setup_fks_kinematics (process%get_var_list_ptr ())
       end select
       call term_instance%setup_expressions (process%get_meta (), process%get_config ())
    end if
  end subroutine term_instance_init_from_process

  subroutine term_instance_setup_kinematics (term, sf_chain, &
     beam_config, phs_config)
    class(term_instance_t), intent(inout) :: term
    type(sf_chain_t), intent(in), target :: sf_chain
    type(process_beam_config_t), intent(in) :: beam_config
    class(phs_config_t), intent(in), target :: phs_config
    call term%k_term%init_sf_chain (sf_chain, beam_config)
    call term%k_term%init_phs (phs_config)
    call term%k_term%set_nlo_info (term%nlo_type)
    select type (phs => term%k_term%phs)
    type is (phs_fks_t)
       call phs%allocate_momenta (phs_config, &
          .not. (term%nlo_type == NLO_REAL))
       select type (config => term%pcm_instance%config)
       type is (pcm_nlo_t)
          call config%region_data%init_phs_identifiers (phs%phs_identifiers)
          call config%region_data%set_alr_to_i_phs (phs%phs_identifiers, &
             term%pcm_instance%real_kinematics%alr_to_i_phs)
       end select
    end select
  end subroutine term_instance_setup_kinematics

  subroutine term_instance_setup_fks_kinematics (term, var_list)
    class(term_instance_t), intent(inout), target :: term
    type(var_list_t), intent(in) :: var_list
    integer :: mode
    logical :: singular_jacobian
    if (.not. (term%nlo_type == NLO_REAL .or. term%nlo_type == NLO_DGLAP .or. &
       term%nlo_type == NLO_MISMATCH)) return
    singular_jacobian = var_list%get_lval (var_str ("?powheg_use_singular_jacobian"))
    if (term%nlo_type == NLO_REAL) then
       mode = check_generator_mode (GEN_REAL_PHASE_SPACE)
    else if (term%nlo_type == NLO_MISMATCH) then
       mode = check_generator_mode (GEN_SOFT_MISMATCH)
    else
       mode = PHS_MODE_UNDEFINED
    end if
    select type (phs => term%k_term%phs)
    type is (phs_fks_t)
       select type (config => term%pcm_instance%config)
       type is (pcm_nlo_t)
          call config%setup_phs_generator (term%pcm_instance, &
             phs%generator, phs%config%sqrts, mode, singular_jacobian)
       end select
    class default
       call msg_fatal ("Phase space should be an FKS phase space!")
    end select
  contains
    function check_generator_mode (gen_mode_default) result (gen_mode)
       integer :: gen_mode
       integer, intent(in) :: gen_mode_default
       select type (config => term%pcm_instance%config)
       type is (pcm_nlo_t)
          associate (settings => config%settings)
             if (settings%test_coll_limit .and. settings%test_anti_coll_limit) &
                call msg_fatal ("You cannot check the collinear and anti-collinear limit "&
                   &"at the same time!")
             if (settings%test_soft_limit .and. .not. settings%test_coll_limit) then
                gen_mode = GEN_SOFT_LIMIT_TEST
             else if (.not. settings%test_soft_limit .and. settings%test_coll_limit) then
                gen_mode = GEN_COLL_LIMIT_TEST
             else if (.not. settings%test_soft_limit .and. settings%test_anti_coll_limit) then
                gen_mode = GEN_ANTI_COLL_LIMIT_TEST
             else if (settings%test_soft_limit .and. settings%test_coll_limit) then
                gen_mode = GEN_SOFT_COLL_LIMIT_TEST
             else if (settings%test_soft_limit .and. settings%test_anti_coll_limit) then
                gen_mode = GEN_SOFT_ANTI_COLL_LIMIT_TEST
             else
                gen_mode = gen_mode_default
             end if
          end associate
       end select
    end function check_generator_mode
  end subroutine term_instance_setup_fks_kinematics

  subroutine term_instance_compute_seed_kinematics &
       (term, mci_work, phs_channel, success)
    class(term_instance_t), intent(inout), target :: term
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    logical, intent(out) :: success
     call term%k_term%compute_selected_channel &
        (mci_work, phs_channel, term%p_seed, success)
  end subroutine term_instance_compute_seed_kinematics

  subroutine term_instance_evaluate_radiation_kinematics (term, mci_work)
    class(term_instance_t), intent(inout) :: term
    type(mci_work_t), intent(in) :: mci_work
    select type (phs => term%k_term%phs)
    type is (phs_fks_t)
       if (phs%mode == PHS_MODE_ADDITIONAL_PARTICLE) then
          select type (config => term%pcm_instance%config)
          type is (pcm_nlo_t)
             call term%k_term%evaluate_radiation_kinematics &
                  (mci_work%get_x_process (), config%region_data)
          end select
       end if
    end select
  end subroutine term_instance_evaluate_radiation_kinematics

  subroutine term_instance_evaluate_projections (term)
    class(term_instance_t), intent(inout) :: term
    select type (phs => term%k_term%phs)
    type is (phs_fks_t)
       if (phs%mode == PHS_MODE_ADDITIONAL_PARTICLE) then
          if (term%k_term%threshold) call term%k_term%threshold_projection ()
       end if
    end select
  end subroutine term_instance_evaluate_projections

  subroutine term_instance_redo_sf_chain (term, mci_work, phs_channel)
    class(term_instance_t), intent(inout) :: term
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    integer :: sf_channel
    associate (k => term%k_term)
       sf_channel = k%phs%config%get_sf_channel (phs_channel)
       call k%sf_chain%compute_kinematics (sf_channel, mci_work%get_x_strfun ())
    end associate
  end subroutine term_instance_redo_sf_chain

  subroutine term_instance_recover_mcpar (term, mci_work, phs_channel)
    class(term_instance_t), intent(inout), target :: term
    type(mci_work_t), intent(inout) :: mci_work
    integer, intent(in) :: phs_channel
    call term%k_term%recover_mcpar (mci_work, phs_channel, term%p_seed)
  end subroutine term_instance_recover_mcpar

  subroutine term_instance_compute_hard_kinematics (term, skip_term)
    class(term_instance_t), intent(inout) :: term
    integer, intent(in), optional :: skip_term
    type(vector4_t), dimension(:), allocatable :: p
    if (allocated (term%core_state)) &
       call term%core_state%reset_new_kinematics ()
    if (present (skip_term)) then
       if (term%config%i_term_global == skip_term) return
    end if

    if (term%nlo_type == NLO_REAL .and. term%k_term%emitter >= 0) then
       call term%k_term%evaluate_radiation (term%p_seed, p)
    else if (term%nlo_type == NLO_REAL .and. term%k_term%emitter < 0) then
       call term%k_term%modify_momenta_for_subtraction (term%p_seed, p)
    else
       allocate (p (size (term%p_seed))); p = term%p_seed
    end if
    call term%int_hard%set_momenta (p)
  end subroutine term_instance_compute_hard_kinematics

  subroutine term_instance_recover_seed_kinematics (term)
    class(term_instance_t), intent(inout) :: term
    integer :: n_in
    n_in = term%k_term%n_in
    call term%k_term%get_incoming_momenta (term%p_seed(1:n_in))
    associate (int_eff => term%isolated%int_eff)
       call int_eff%set_momenta (term%p_seed(1:n_in), outgoing = .false.)
       term%p_seed(n_in + 1 : ) = int_eff%get_momenta (outgoing = .true.)
    end associate
    call term%isolated%receive_kinematics ()
  end subroutine term_instance_recover_seed_kinematics

  subroutine term_instance_compute_other_channels &
       (term, mci_work, phs_channel)
    class(term_instance_t), intent(inout), target :: term
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    call term%k_term%compute_other_channels (mci_work, phs_channel)
  end subroutine term_instance_compute_other_channels

  subroutine term_instance_return_beam_momenta (term)
    class(term_instance_t), intent(in) :: term
    call term%k_term%return_beam_momenta ()
  end subroutine term_instance_return_beam_momenta

  subroutine term_instance_apply_damping_factor (term, component_type)
    class(term_instance_t), intent(inout) :: term
    integer, intent(in) :: component_type
    real(default) :: E_gluon, sqme
    integer :: nlegs
    sqme = real (term%connected%trace%get_matrix_element (1))
    call msg_debug2 (D_PROCESS_INTEGRATION, "term_instance_apply_damping_factor")
    select type (pcm => term%pcm_instance%config)
    type is (pcm_nlo_t)
       select case (component_type)
       case (COMP_REAL_FIN, COMP_REAL_SING)
          nlegs = pcm%region_data%n_legs_real
          select case (component_type)
          case (COMP_REAL_FIN)
             call msg_debug2 (D_PROCESS_INTEGRATION, "Real finite")
             E_gluon = energy (term%p_hard(nlegs))
             sqme = sqme * (one - pcm%powheg_damping%get_f (E_gluon**2))
          case (COMP_REAL_SING)
             if (term%k_term%emitter < 0) return
             call msg_debug2 (D_PROCESS_INTEGRATION, "Real singular")
             E_gluon = energy (term%pcm_instance%real_kinematics%p_real_lab%phs_point(1)%p(nlegs))
             sqme = sqme * pcm%powheg_damping%get_f (E_gluon**2)
          end select
       end select
    end select
    call msg_debug2 (D_PROCESS_INTEGRATION, "apply_damping: sqme", sqme)
    call term%connected%trace%set_matrix_element (cmplx (sqme, zero, default))
  end subroutine term_instance_apply_damping_factor

  function term_instance_get_lorentz_transformation (term) result (lt)
    type(lorentz_transformation_t) :: lt
    class(term_instance_t), intent(in) :: term
    lt = term%k_term%phs%get_lorentz_transformation ()
  end function term_instance_get_lorentz_transformation

  subroutine term_instance_set_emitter (term, pcm)
    class(term_instance_t), intent(inout) :: term
    class(pcm_t), intent(in) :: pcm
    integer :: i_phs
    logical :: no_subtraction_component
    select type (pcm)
    type is (pcm_nlo_t)
       !!! Without resonances, i_alr = i_phs
       i_phs = term%config%i_term
       term%k_term%i_phs = term%config%i_term
       select type (phs => term%k_term%phs)
       type is (phs_fks_t)
          no_subtraction_component = i_phs <= pcm%region_data%n_phs
          if (no_subtraction_component) then
             term%k_term%emitter = phs%phs_identifiers(i_phs)%emitter
             select type (pcm => term%pcm_instance%config)
             type is (pcm_nlo_t)
                if (allocated (pcm%region_data%i_phs_to_i_con)) &
                   term%k_term%i_con = pcm%region_data%i_phs_to_i_con (i_phs)
             end select
          end if
       end select
    end select
  end subroutine term_instance_set_emitter

  subroutine term_instance_set_threshold (term, pcm)
    class(term_instance_t), intent(inout) :: term
    class(pcm_t), intent(in) :: pcm
    select type (pcm)
    type is (pcm_nlo_t)
       term%k_term%threshold = pcm%region_data%has_pseudo_isr ()
    class default
       term%k_term%threshold = .false.
    end select
  end subroutine term_instance_set_threshold

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

  subroutine term_instance_setup_event_data (term, core, model)
    class(term_instance_t), intent(inout), target :: term
    class(prc_core_t), intent(in) :: core
    class(model_data_t), intent(in), target :: model
    integer :: n_in
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_in
    n_in = term%int_hard%get_n_in ()
    allocate (mask_in (n_in))
    mask_in = term%k_term%sf_chain%get_out_mask ()
    call setup_isolated (term%isolated, core, model, mask_in, term%config%col)
    call setup_connected (term%connected, term%isolated)
 contains
   subroutine setup_isolated (isolated, core, model, mask, color)
     type(isolated_state_t), intent(inout), target :: isolated
     class(prc_core_t), intent(in) :: core
     class(model_data_t), intent(in), target :: model
     type(quantum_numbers_mask_t), intent(in), dimension(:) :: mask
     integer, intent(in), dimension(:) :: color
     call isolated%setup_square_matrix (core, model, mask, color)
     call isolated%setup_square_flows (core, model, mask)
   end subroutine setup_isolated

   subroutine setup_connected (connected, isolated)
     type(connected_state_t), intent(inout), target :: connected
     type(isolated_state_t), intent(in), target :: isolated
     call connected%setup_connected_matrix (isolated)
     call connected%setup_connected_flows (isolated)
     call connected%setup_state_flv (isolated%get_n_out ())
   end subroutine setup_connected
 end subroutine term_instance_setup_event_data

  subroutine term_instance_evaluate_color_correlations (term, p, core, bad_point)
    class(term_instance_t), intent(inout) :: term
    type(vector4_t), intent(in), dimension(:) :: p
    class(prc_core_t), intent(inout) :: core
    logical, intent(out), optional :: bad_point
    integer :: i_flv_born
    logical :: bp
    real(default), dimension(:,:), allocatable :: sqme_cc
    real(default) :: sqme_born
    associate (pcm_instance => term%pcm_instance)
       select type (config => pcm_instance%config)
       type is (pcm_nlo_t)
          allocate (sqme_cc (config%region_data%n_legs_born, &
               config%region_data%n_legs_born))
          call msg_debug2 (D_SUBTRACTION, &
            "term_instance_evaluate_color_correlations: " // &
            "use_internal_color_correlations:", &
            config%settings%use_internal_color_correlations)
          call msg_debug2 (D_SUBTRACTION, "fac_scale", term%fac_scale)

          do i_flv_born = 1, config%region_data%n_flv_born
             if (config%settings%use_internal_color_correlations) then
                select case (term%nlo_type)
                case (NLO_REAL)
                   sqme_born = pcm_instance%real_sub%sqme_born (i_flv_born)
                   sqme_cc = sqme_born * config%color_data%beta_ij (:, :, i_flv_born)
                case (NLO_MISMATCH)
                   sqme_born = pcm_instance%soft_mismatch%sqme_born (i_flv_born)
                   sqme_cc = sqme_born * config%color_data%beta_ij (:, :, i_flv_born)
                end select
                bp = .false.
             else
                select type (core)
                class is (prc_user_defined_base_t)
                   call core%update_alpha_s (term%core_state, term%fac_scale)
                   call core%compute_sqme_cc (i_flv_born, p, &
                        term%fac_scale, sqme_cc, bp, sqme_born)
                end select
             end if

             select case (term%nlo_type)
             case (NLO_REAL)
                pcm_instance%real_sub%sqme_born_cc (:, :, i_flv_born) = sqme_cc
             case (NLO_MISMATCH)
                pcm_instance%soft_mismatch%sqme_born_cc (:, :, i_flv_born) = sqme_cc
             case (NLO_VIRTUAL)
                if (config%settings%use_internal_color_correlations) then
                   pcm_instance%virtual%sqme_cc (:, :, i_flv_born) = &
                      config%color_data%beta_ij (:, :, i_flv_born)
                else
                   pcm_instance%virtual%sqme_cc (:, :, i_flv_born) = sqme_cc
                end if
             end select
          end do
       end select
    end associate
    if (present (bad_point)) bad_point = bp
  end subroutine term_instance_evaluate_color_correlations

  subroutine term_instance_apply_fks (term, core, alpha_s_sub)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(inout) :: core
    real(default), intent(in) :: alpha_s_sub
    real(default) :: sqme
    integer :: i_phs, emitter
    sqme = zero
    associate (pcm_instance => term%pcm_instance)
       select type (config => pcm_instance%config)
       type is (pcm_nlo_t)
          select type (phs => term%k_term%phs)
          type is (phs_fks_t)
             call pcm_instance%set_real_and_isr_kinematics &
                  (phs%phs_identifiers, term%k_term%phs%get_sqrts ())
             if (term%k_term%emitter < 0) then
                call pcm_instance%disable_sqme_np1 ()
                do i_phs = 1, config%region_data%n_phs
                   emitter = phs%phs_identifiers(i_phs)%emitter
                   sqme = sqme + pcm_instance%real_sub%compute (emitter, &
                          i_phs, alpha_s_sub)
                end do
             else
                call pcm_instance%disable_subtraction ()
                emitter = term%k_term%emitter; i_phs = term%k_term%i_phs
                pcm_instance%real_sub%sqme_real_non_sub (1, i_phs) = &
                     real (term%connected%trace%get_matrix_element (1))
                sqme = pcm_instance%real_sub%compute (emitter, i_phs, alpha_s_sub)
             end if
          end select
       end select
    end associate
    call term%connected%trace%set_matrix_element (cmplx (sqme, zero, default))
  end subroutine term_instance_apply_fks

  subroutine term_instance_evaluate_sqme_virt (term, alpha_s)
     class(term_instance_t), intent(inout) :: term
     real(default), intent(in) :: alpha_s
     real(default) :: sqme_virt
     if (term%nlo_type /= NLO_VIRTUAL) call msg_fatal &
        ("Trying to evaluate virtual matrix element with unsuited term_instance.")
     if (debug2_active (D_VIRTUAL)) then
        call msg_debug2 (D_VIRTUAL, "Evaluating virtual-subtracted matrix elements")
        print *, 'alpha_s: ', alpha_s
        print *, 'ren_scale: ', term%ren_scale
        print *, 'fac_scale: ', term%fac_scale
     end if
     call term%pcm_instance%set_momenta_and_scales_virtual &
          (term%int_hard%get_momenta (), term%ren_scale, term%fac_scale)
     sqme_virt = term%pcm_instance%compute_sqme_virt (term%p_hard, alpha_s, &
          term%connected%trace%get_matrix_element ())
     call term%connected%trace%set_matrix_element &
          (cmplx (sqme_virt * term%weight, 0, default))
  end subroutine term_instance_evaluate_sqme_virt

  subroutine term_instance_evaluate_sqme_mismatch (term, alpha_s)
    class(term_instance_t), intent(inout) :: term
    real(default), intent(in) :: alpha_s
    if (term%nlo_type /= NLO_MISMATCH) call msg_fatal &
       ("Trying to evaluate soft mismatch with unsuited term_instance.")
    call term%connected%trace%set_matrix_element &
       (cmplx (term%pcm_instance%compute_sqme_mismatch (alpha_s) * term%weight, zero, default))
  end subroutine term_instance_evaluate_sqme_mismatch

  subroutine term_instance_evaluate_sqme_dglap (term, alpha_s)
    class(term_instance_t), intent(inout) :: term
    real(default), intent(in) :: alpha_s
    real(default) :: sqme
    if (term%nlo_type /= NLO_DGLAP) call msg_fatal &
       ("Trying to evaluate DGLAP remnant with unsuited term_instance.")
    sqme = term%pcm_instance%compute_sqme_dglap_remnant (alpha_s, &
       real (term%connected%trace%get_matrix_element (1)))
    call term%connected%trace%set_matrix_element (cmplx (sqme, 0, default))
  end subroutine term_instance_evaluate_sqme_dglap

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

  subroutine term_instance_compute_eff_kinematics (term)
    class(term_instance_t), intent(inout) :: term
    term%checked = .false.
    term%passed = .false.
    call term%isolated%receive_kinematics ()
    call term%connected%receive_kinematics ()
  end subroutine term_instance_compute_eff_kinematics

  subroutine term_instance_recover_hard_kinematics (term)
    class(term_instance_t), intent(inout) :: term
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

  subroutine term_instance_evaluate_interaction (term, core)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(in), pointer :: core

    term%p_hard = term%int_hard%get_momenta ()
    select type (core)
    class is (prc_user_defined_base_t)
       call term%evaluate_interaction_userdef (core)
    class default
       call term%evaluate_interaction_default (core)
    end select
    call term%int_hard%set_matrix_element (term%amp)
  end subroutine term_instance_evaluate_interaction

  subroutine term_instance_evaluate_interaction_default (term, core)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(in) :: core
    integer :: i, i_term
    integer :: only_flavor
    i_term = term%config%i_term
    if (term%nlo_type == NLO_REAL .and. term%config%i_term_global /= term%config%i_sub) then
       select type (config => term%pcm_instance%config)
       type is (pcm_nlo_t)
          only_flavor = config%region_data%get_real_index (i_term)
       end select
    else
       only_flavor = 0
    end if
    do i = 1, term%config%n_allowed
       if (only_flavor > 0 .and. term%config%flv(i) /= only_flavor) cycle
       term%amp(i) = core%compute_amplitude (i_term, term%p_hard, &
          term%config%flv(i), term%config%hel(i), term%config%col(i), &
          term%fac_scale, term%ren_scale, term%alpha_qcd_forced, &
          term%core_state)
    end do
    if (associated (term%pcm_instance)) &
       call term%pcm_instance%set_fac_scale (term%fac_scale)
  end subroutine term_instance_evaluate_interaction_default

  subroutine term_instance_evaluate_interaction_userdef (term, core)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(in) :: core
    integer :: leg

    select type (core_state => term%core_state)
    class is (user_defined_state_t)
       select type (core)
       class is (prc_user_defined_base_t)
          call core%compute_alpha_s (core_state, term%ren_scale)
       end select
    end select
    select type (core)
    type is (prc_threshold_t)
       if (term%nlo_type == NLO_REAL) then
          associate (pcm => term%pcm_instance)
             if (term%k_term%emitter >= 0) then
                call core%set_offshell_momenta &
                     (pcm%real_kinematics%p_real_cms%get_momenta(term%config%i_term))
                leg = thr_leg (term%k_term%emitter)
                call core%set_onshell_momenta &
                     (pcm%real_kinematics%p_real_onshell(leg)%get_momenta(term%config%i_term))
             else
                call core%set_offshell_momenta &
                     (pcm%real_kinematics%p_born_cms%get_momenta(1))
             end if
          end associate
       else
          call core%set_offshell_momenta (term%p_hard)
       end if
    end select
    select case (term%nlo_type)
    case (NLO_VIRTUAL)
       call term%evaluate_interaction_userdef_loop (core)
    case (BORN, NLO_REAL, NLO_DGLAP)
       call term%evaluate_interaction_userdef_tree (core)
    end select

  end subroutine term_instance_evaluate_interaction_userdef

  subroutine term_instance_evaluate_interaction_userdef_tree (term, core)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(in) :: core
    real(default) :: sqme
    integer :: n_amps, i, i_born
    logical :: bad_point
    n_amps = term%int_hard%get_n_matrix_elements ()
    select type (core)
    class is (prc_user_defined_base_t)
       call core%update_alpha_s (term%core_state, term%fac_scale)
       do i = 1, n_amps
          call core%compute_sqme (i, term%p_hard, term%ren_scale, &
             sqme, bad_point)
          i_born = core%get_helicity_list_base (i)
          term%amp(i_born) = cmplx (sqme, 0, default)
       end do
    end select
  end subroutine term_instance_evaluate_interaction_userdef_tree

  subroutine term_instance_evaluate_interaction_userdef_loop (term, core)
    class(term_instance_t), intent(inout) :: term
    class(prc_core_t), intent(in) :: core
    integer :: n_virtuals, n_hel, n_sub, n_flv
    integer :: i, i_born, i_virt, i_cc, i_sub
    logical :: bad_point
    real(default), dimension(4) :: sqme_virt
    real(default), dimension(:), allocatable :: sqme_cc
    allocate (sqme_cc (blha_result_array_size &
         (term%int_hard%get_n_tot (), BLHA_AMP_CC)))

    call term%compute_virt_me_array_sizes (core, n_hel, n_sub, n_flv, n_virtuals)
    do i = 1, n_virtuals
       select type (core)
       class is (prc_blha_t)
          i_virt = core%get_helicity_list (i)
       class default
          i_virt = 1
       end select
       i_born = i_virt + n_hel
       select type (core)
       class is (prc_user_defined_base_t)
          call core%compute_sqme_virt (i, term%p_hard, &
             term%ren_scale, sqme_virt, bad_point)
          associate (bad_blha => term%pcm_instance%bad_blha_point (term%config%i_component))
             bad_blha = bad_blha .or. bad_point
          end associate
       end select
       term%amp(i_virt) = cmplx (sqme_virt(3), 0, default)
       select type (config => term%pcm_instance%config)
       type is (pcm_nlo_t)
          if (config%settings%use_internal_color_correlations) then
             term%amp(i_born) = cmplx (sqme_virt(4), 0, default)
          else
             select type (core)
             class is (prc_blha_t)
                call core%compute_sqme_cc_raw (i, term%p_hard, term%ren_scale, &
                   sqme_cc, bad_point)
                term%amp (i_born) = cmplx (sqme_virt(4), 0, default)
                do i_sub = 1, n_sub - 1
                   i_cc = core%get_helicity_list (i + n_hel - 1)
                   i_cc = i_cc + (i_sub + 1) * n_hel
                   term%amp(i_cc) = cmplx (sqme_cc(i_sub), 0, default)
                end do
             end select
          end if
       end select
    end do
  end subroutine term_instance_evaluate_interaction_userdef_loop

  subroutine term_instance_compute_virt_me_array_sizes &
         (term_instance, core, n_hel, n_sub, n_flv, n_virtuals)
    class(term_instance_t), intent(in) :: term_instance
    class(prc_core_t), intent(in) :: core
    integer, intent(out) :: n_hel, n_sub, n_flv, n_virtuals
    logical :: includes_polarization
    n_sub = term_instance%int_hard%get_n_sub ()
    n_virtuals = term_instance%int_hard%get_n_matrix_elements ()
    n_flv = term_instance%config%data%n_flv
    select type (core)
    class is (prc_user_defined_base_t)
      includes_polarization = core%includes_polarization ()
    class default
      includes_polarization = .false.
    end select
    if (includes_polarization) then
       n_hel = 4    !!! This assumes two polarized beams
    else
       n_hel = 1
    end if
    if (n_sub > 0) then
       n_virtuals = n_virtuals - n_hel * n_sub * n_flv
    end if
  end subroutine term_instance_compute_virt_me_array_sizes

  subroutine term_instance_evaluate_trace (term)
    class(term_instance_t), intent(inout) :: term
    call term%k_term%evaluate_sf_chain (term%fac_scale)
    call term%isolated%evaluate_sf_chain (term%fac_scale)
    call term%isolated%evaluate_trace ()
    call term%connected%evaluate_trace ()
  end subroutine term_instance_evaluate_trace

  subroutine term_instance_evaluate_event_data (term)
    class(term_instance_t), intent(inout) :: term
    call term%isolated%evaluate_event_data ()
    call term%connected%evaluate_event_data ()
  end subroutine term_instance_evaluate_event_data

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

  function term_instance_get_alpha_s (term, core) result (alpha_s)
    class(term_instance_t), intent(in) :: term
    class(prc_core_t), intent(in) :: core
    real(default) :: alpha_s
    alpha_s = core%get_alpha_s (term%core_state)
    if (alpha_s < zero)  alpha_s = term%config%alpha_s
  end function term_instance_get_alpha_s

  subroutine term_instance_reset_phs_identifiers (term)
    class(term_instance_t), intent(inout) :: term
    select type (phs => term%k_term%phs)
    type is (phs_fks_t)
       phs%phs_identifiers%evaluated = .false.
    end select
  end subroutine term_instance_reset_phs_identifiers

  subroutine term_instance_get_helicities_for_openloops (term, helicities)
    class(term_instance_t), intent(in) :: term
    integer, dimension(:,:), allocatable, intent(out) :: helicities
    type(helicity_t), dimension(:), allocatable :: hel
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    type(quantum_numbers_mask_t) :: qn_mask
    integer :: h, i, j, n_in
    call qn_mask%set_sub (1)
    call term%isolated%trace%get_quantum_numbers_mask (qn_mask, qn)
    n_in = term%int_hard%get_n_in ()
    allocate (helicities (size (qn, dim=1), 2))
    allocate (hel (size (qn, dim=1)))
    do i = 1, size (qn, dim=1)
       do j = 1, n_in
          hel(j) = qn(i, j)%get_helicity ()
          call hel(j)%diagonalize ()
          call hel(j)%get_indices (h, h)
          !!! Second helicity comes with a minus sign because OpenLoops
          !!! inverts the helicity index of antiparticles
          helicities (i, j) = h
       end do
    end do
  end subroutine term_instance_get_helicities_for_openloops

  function term_instance_get_boost_to_lab (term) result (lt)
    type(lorentz_transformation_t) :: lt
    class(term_instance_t), intent(in) :: term
    lt = term%k_term%phs%get_lorentz_transformation ()
  end function term_instance_get_boost_to_lab

  function term_instance_get_boost_to_cms (term) result (lt)
    type(lorentz_transformation_t) :: lt
    class(term_instance_t), intent(in) :: term
    lt = inverse (term%k_term%phs%get_lorentz_transformation ())
  end function term_instance_get_boost_to_cms

  subroutine process_instance_write_header (object, unit, testflag)
    class(process_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    if (associated (object%process)) then
       call object%process%write_meta (u, testflag)
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

  subroutine process_instance_init (instance, process)
    class(process_instance_t), intent(out), target :: instance
    type(process_t), intent(inout), target :: process
    integer :: i
    class(pcm_t), pointer :: pcm
    type(process_term_t) :: term
    integer :: i_born, i_real, i_real_fin

    instance%process => process
    call instance%setup_sf_chain (process%get_beam_config_ptr ())

    allocate (instance%mci_work (process%get_n_mci ()))
    do i = 1, size (instance%mci_work)
       call instance%process%init_mci_work (instance%mci_work(i), i)
    end do

    call instance%process%reset_selected_cores ()

    pcm => instance%process%get_pcm_ptr ()
    call pcm%allocate_instance (instance%pcm)
    call instance%pcm%link_config (pcm)

    select type (pcm)
    type is (pcm_nlo_t)
       !!! The process is kept when the integration is finalized, but not the
       !!! process_instance. Thus, we check whether pcm has been initialized
       !!! but set up the pcm_instance each time.
       i_real_fin = process%get_associated_real_fin (1)
       if (.not. pcm%initialized) then
          i_born = process%get_i_core_nlo_type (BORN)
          i_real = process%get_i_core_nlo_type (NLO_REAL, include_sub = .false.)
          term = process%get_term_ptr (process%get_i_term (i_real))
          call pcm%init_color_data &
             ([process%get_constants (i_born), process%get_constants (i_real)], &
              term%flv, term%col)
          if (i_real_fin > 0) call pcm%allocate_ps_matching ()
       end if
       pcm%initialized = .true.
       select type (pcm_instance => instance%pcm)
       type is (pcm_instance_nlo_t)
          call pcm_instance%init_config (process%component_is_active (), &
             process%get_nlo_type_component (), process%get_sqrts (), i_real_fin)
       end select
    end select

    allocate (instance%term (process%get_n_terms ()))
    do i = 1, process%get_n_terms ()
       call instance%term(i)%init_from_process (process, i, instance%pcm, &
            instance%sf_chain)
    end do
    call instance%find_same_kinematics ()

    instance%evaluation_status = STAT_INITIAL
  end subroutine process_instance_init

  subroutine process_instance_final (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i
    instance%process => null ()
    if (allocated (instance%mci_work)) then
       do i = 1, size (instance%mci_work)
          call instance%mci_work(i)%final ()
       end do
       deallocate (instance%mci_work)
    end if
    call instance%sf_chain%final ()
    if (allocated (instance%term)) then
       do i = 1, size (instance%term)
          call instance%term(i)%final ()
       end do
       deallocate (instance%term)
    end if
    call instance%pcm%final ()
    instance%evaluation_status = STAT_UNDEFINED
  end subroutine process_instance_final

  subroutine process_instance_reset (instance, reset_mci)
    class(process_instance_t), intent(inout) :: instance
    logical, intent(in), optional :: reset_mci
    integer :: i
    call instance%process%reset_selected_cores ()
    do i = 1, size (instance%term)
       call instance%term(i)%reset ()
    end do
    instance%term%checked = .false.
    instance%term%passed = .false.
    instance%term%k_term%new_seed = .true.
    if (present (reset_mci)) then
       if (reset_mci)  instance%i_mci = 0
    end if
    instance%selected_channel = 0
    instance%evaluation_status = STAT_INITIAL
  end subroutine process_instance_reset

  subroutine process_instance_sampler_test (instance, i_mci, n_calls)
    class(process_instance_t), intent(inout), target :: instance
    integer, intent(in) :: i_mci
    integer, intent(in) :: n_calls
    integer :: i_mci_work
    i_mci_work = instance%process%get_i_mci_work (i_mci)
    call instance%choose_mci (i_mci_work)
    call instance%reset_counter ()
    call instance%process%sampler_test (instance, n_calls, i_mci_work)
    call instance%process%set_counter_mci_entry (i_mci_work, instance%get_counter ())
  end subroutine process_instance_sampler_test

  subroutine process_instance_generate_weighted_event (instance, i_mci)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    integer :: i_mci_work
    i_mci_work = instance%process%get_i_mci_work (i_mci)
    call instance%choose_mci (i_mci_work)
    associate (mci_work => instance%mci_work(i_mci_work))
       call instance%process%generate_weighted_event &
          (i_mci_work, mci_work, instance, &
           instance%keep_failed_events ())
    end associate
  end subroutine process_instance_generate_weighted_event

  subroutine process_instance_generate_unweighted_event (instance, i_mci)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    integer :: i_mci_work
    i_mci_work = instance%process%get_i_mci_work (i_mci)
    call instance%choose_mci (i_mci_work)
    associate (mci_work => instance%mci_work(i_mci_work))
       call instance%process%generate_unweighted_event &
          (i_mci_work, mci_work, instance)
    end associate
  end subroutine process_instance_generate_unweighted_event

  subroutine process_instance_recover_event (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i_mci
    i_mci = instance%i_mci
    call instance%process%set_i_mci_work (i_mci)
    associate (mci_instance => instance%mci_work(i_mci)%mci)
      call mci_instance%fetch (instance, instance%selected_channel)
    end associate
  end subroutine process_instance_recover_event

  subroutine process_instance_activate (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i, j
    integer, dimension(:), allocatable :: i_term
    associate (mci_work => instance%mci_work(instance%i_mci))
       call instance%process%select_components (mci_work%get_active_components ())
    end associate
    associate (process => instance%process)
       do i = 1, instance%process%get_n_components ()
          if (instance%process%component_is_selected (i)) then
            allocate (i_term (size (process%get_component_i_terms (i))))
            i_term = process%get_component_i_terms (i)
            do j = 1, size (i_term)
               instance%term(i_term(j))%active = .true.
            end do
          end if
          if (allocated (i_term)) deallocate (i_term)
       end do
    end associate
    instance%evaluation_status = STAT_ACTIVATED
  end subroutine process_instance_activate

  subroutine process_instance_find_same_kinematics (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i_term1, i_term2, k, n_same
    do i_term1 = 1, size (instance%term)
       if (.not. allocated (instance%term(i_term1)%same_kinematics)) then
          n_same = 1 !!! Index group includes the index of its term_instance
          do i_term2 = 1, size (instance%term)
             if (i_term1 == i_term2) cycle
             if (compare_md5s (i_term1, i_term2)) n_same = n_same + 1
          end do
          allocate (instance%term(i_term1)%same_kinematics (n_same))
          associate (same_kinematics1 => instance%term(i_term1)%same_kinematics)
             same_kinematics1 = 0
             k = 1
             do i_term2 = 1, size (instance%term)
                if (compare_md5s (i_term1, i_term2)) then
                   same_kinematics1(k) = i_term2
                   k = k + 1
                end if
             end do
             do k = 1, size (same_kinematics1)
                if (same_kinematics1(k) == i_term1) cycle
                i_term2 = same_kinematics1(k)
                allocate (instance%term(i_term2)%same_kinematics (n_same))
                instance%term(i_term2)%same_kinematics = same_kinematics1
             end do
          end associate
       end if
    end do
  contains
    function compare_md5s (i, j) result (same)
      logical :: same
      integer, intent(in) :: i, j
      character(32) :: md5sum_1, md5sum_2
      integer :: mode_1, mode_2
      mode_1 = 0; mode_2 = 0
      select type (phs => instance%term(i)%k_term%phs%config)
      type is (phs_fks_config_t)
         md5sum_1 = phs%md5sum_born_config
         mode_1 = phs%mode
      class default
         md5sum_1 = phs%md5sum_phs_config
      end select
      select type (phs => instance%term(j)%k_term%phs%config)
      type is (phs_fks_config_t)
         md5sum_2 = phs%md5sum_born_config
         mode_2 = phs%mode
      class default
         md5sum_2 = phs%md5sum_phs_config
      end select
      same = (md5sum_1 == md5sum_2) .and. (mode_1 == mode_2)
    end function compare_md5s
  end subroutine process_instance_find_same_kinematics

  subroutine process_instance_transfer_same_kinematics (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: i, i_term_same
    associate (same_kinematics => instance%term(i_term)%same_kinematics)
       do i = 1, size (same_kinematics)
          i_term_same = same_kinematics(i)
          if (i_term_same /= i_term) then
             instance%term(i_term_same)%p_seed = instance%term(i_term)%p_seed
             associate (phs => instance%term(i_term_same)%k_term%phs)
                call phs%set_lorentz_transformation &
                   (instance%term(i_term)%k_term%phs%get_lorentz_transformation ())
                select type (phs)
                type is (phs_fks_t)
                   call phs%set_momenta (instance%term(i_term_same)%p_seed)
                   call phs%set_reference_frames ()
                   call phs%set_isr_kinematics ()
                end select
             end associate
          end if
          instance%term(i_term_same)%k_term%new_seed = .false.
       end do
    end associate
  end subroutine process_instance_transfer_same_kinematics

  subroutine process_instance_redo_sf_chains (instance, i_term, phs_channel)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer, intent(in) :: phs_channel
    integer :: i, i_term_same
    associate (same_kinematics => instance%term(i_term)%same_kinematics)
       do i = 1, size (same_kinematics)
          i_term_same = same_kinematics(i)
          if (i_term_same /= i_term) &
             call instance%term(i_term_same)%redo_sf_chain &
                (instance%mci_work(instance%i_mci), phs_channel)
       end do
    end associate
  end subroutine process_instance_redo_sf_chains

  subroutine process_instance_integrate (instance, i_mci, n_it, n_calls, &
       adapt_grids, adapt_weights, final, pacify)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    integer, intent(in) :: n_it
    integer, intent(in) :: n_calls
    logical, intent(in), optional :: adapt_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: final, pacify
    integer :: nlo_type, i_mci_work
    nlo_type = instance%process%get_component_nlo_type (i_mci)
    i_mci_work = instance%process%get_i_mci_work (i_mci)
    call instance%choose_mci (i_mci_work)
    call instance%reset_counter ()
    associate (mci_work => instance%mci_work(i_mci_work), &
               process => instance%process)
       call process%integrate (i_mci_work, mci_work, &
          instance, n_it, n_calls, adapt_grids, adapt_weights, &
          final, pacify, nlo_type = nlo_type)
       call process%set_counter_mci_entry (i_mci_work, instance%get_counter ())
    end associate
  end subroutine process_instance_integrate

  subroutine process_instance_transfer_helicities (instance, i_component, core)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_component
    !!! intent(inout) is annoying. Transfer list should be somewhere else
    class(prc_blha_t), intent(inout) :: core
    integer, dimension(:,:), allocatable :: helicities
    integer, dimension(:), allocatable :: i_term
    allocate (i_term (size (instance%process%get_component_i_terms (i_component))))
    i_term = instance%process%get_component_i_terms (i_component)
    call instance%term(i_term(1))%get_helicities_for_openloops (helicities)
    call core%set_helicity_list (helicities)
  end subroutine process_instance_transfer_helicities

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

  subroutine process_instance_setup_event_data (instance, model, i_core)
    class(process_instance_t), intent(inout), target :: instance
    class(model_data_t), intent(in), optional, target :: model
    integer, intent(in), optional :: i_core
    class(model_data_t), pointer :: current_model
    integer :: i
    class(prc_core_t), pointer :: core => null ()
    if (present (model)) then
       current_model => model
    else
       current_model => instance%process%get_model_ptr ()
    end if
    do i = 1, size (instance%term)
       associate (term => instance%term(i))
         if (associated (term%config)) then
            core => instance%process%get_core_term (i)
            call term%setup_event_data (core, current_model)
         end if
       end associate
    end do
    core => null ()
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
    if (.not. instance%process%lab_is_cm_frame ()) then
       if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
          call instance%term(i_term)%return_beam_momenta ()
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
    integer :: channel, skip_component, i, j
    logical :: success
    integer, dimension(:), allocatable :: i_term
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
       do i = 1, instance%process%get_n_components ()
          if (i == skip_component)  cycle
          if (instance%process%component_is_selected (i)) then
             allocate (i_term (size (instance%process%get_component_i_terms (i))))
             i_term = instance%process%get_component_i_terms (i)
             do j = 1, size (i_term)
                if (instance%term(i_term(j))%k_term%new_seed) then
                   call instance%term(i_term(j))%compute_seed_kinematics &
                      (instance%mci_work(instance%i_mci), channel, success)
                   call instance%transfer_same_kinematics (i_term(j))
                   call instance%redo_sf_chains (i_term(j), channel)
                end if
                if (.not. success)  exit
                call instance%term(i_term(j))%evaluate_projections ()
                call instance%term(i_term(j))%evaluate_radiation_kinematics &
                   (instance%mci_work(instance%i_mci))
             end do
          end if
          if (allocated (i_term)) deallocate (i_term)
       end do
       if (success) then
          instance%evaluation_status = STAT_SEED_KINEMATICS
       else
          instance%evaluation_status = STAT_FAILED_KINEMATICS
       end if
    end if
    associate (mci_work => instance%mci_work(instance%i_mci))
       select type (pcm => instance%pcm)
       class is (pcm_instance_nlo_t)
          call pcm%set_x_rad (mci_work%get_x_process ())
       end select
    end associate
  end subroutine process_instance_compute_seed_kinematics

  subroutine process_instance_recover_mcpar (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    integer :: channel
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) then
       channel = instance%selected_channel
       if (channel == 0) then
          call msg_bug ("Recover MC parameters: undefined integration channel")
       end if
       call instance%term(i_term)%recover_mcpar &
          (instance%mci_work(instance%i_mci), channel)
    end if
  end subroutine process_instance_recover_mcpar

  subroutine process_instance_compute_hard_kinematics (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: i
    if (instance%evaluation_status >= STAT_SEED_KINEMATICS) then
       do i = 1, size (instance%term)
          if (instance%term(i)%active) &
             call instance%term(i)%compute_hard_kinematics (skip_term)
       end do
       instance%evaluation_status = STAT_HARD_KINEMATICS
    end if
  end subroutine process_instance_compute_hard_kinematics

  subroutine process_instance_recover_seed_kinematics (instance, i_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term
    if (instance%evaluation_status >= STAT_EFF_KINEMATICS) &
       call instance%term(i_term)%recover_seed_kinematics ()
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
             call instance%term(i)%compute_eff_kinematics ()
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
       call instance%term(i_term)%recover_hard_kinematics ()
       do i = 1, size (instance%term)
          if (i /= i_term) then
             if (instance%term(i)%active) then
                call instance%term(i)%compute_eff_kinematics ()
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
       do i = 1, size (instance%term)
          if (instance%term(i)%nlo_type == NLO_REAL) call replace_scales (instance%term(i))
       end do
    end if
  contains
    subroutine replace_scales (this_term)
      type(term_instance_t), intent(inout) :: this_term
      real(default) :: ren_scale_born, fac_scale_born
      integer :: i_sub
      i_sub = this_term%config%i_sub
      if (this_term%config%i_term_global /= i_sub) then
         ren_scale_born = instance%term(i_sub)%ren_scale
         fac_scale_born = instance%term(i_sub)%fac_scale
         this_term%ren_scale = ren_scale_born
         this_term%fac_scale = fac_scale_born
      end if
    end subroutine replace_scales
  end subroutine process_instance_evaluate_expressions

  subroutine process_instance_compute_other_channels (instance, skip_term)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: skip_term
    integer :: channel, skip_component, i, j
    integer, dimension(:), allocatable :: i_term
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
       do i = 1, instance%process%get_n_components ()
          if (i == skip_component)  cycle
          if (instance%process%component_is_selected (i)) then
             allocate (i_term (size (instance%process%get_component_i_terms (i))))
             i_term = instance%process%get_component_i_terms (i)
             do j = 1, size (i_term)
                call instance%term(i_term(j))%compute_other_channels &
                   (instance%mci_work(instance%i_mci), channel)
             end do
          end if
          if (allocated (i_term)) deallocate (i_term)
       end do
    end if
  end subroutine process_instance_compute_other_channels

  subroutine process_instance_evaluate_trace (instance)
    class(process_instance_t), intent(inout) :: instance
    class(prc_core_t), pointer :: core => null ()
    integer :: i, i_real_fin
    real(default) :: alpha_s
    class(prc_core_t), pointer :: core_sub => null ()
    instance%sqme = zero
    call instance%reset_matrix_elements ()
    if (instance%evaluation_status >= STAT_PASSED_CUTS) then
       do i = 1, size (instance%term)
          associate (term => instance%term(i))
            if (term%active .and. term%passed) then
              core => instance%process%get_core_term (i)
              if (instance%pcm%config%is_nlo ()) &
                 core_sub => instance%process%get_subtraction_core ()
              call term%evaluate_interaction (core)
              call term%evaluate_trace ()
              if (term%nlo_type == NLO_REAL .or. term%nlo_type == NLO_MISMATCH) &
                 call set_sqme_born (term, 1)
              if (term%nlo_type > BORN) then
                 if (.not. (term%nlo_type == NLO_REAL .and. term%k_term%emitter > 0)) &
                    call term%evaluate_color_correlations (term%p_seed, core_sub)
              end if
              alpha_s = core%get_alpha_s (term%core_state)
              select case (term%nlo_type)
              case (NLO_REAL)
                 i_real_fin = instance%process%get_associated_real_fin (1)
                 if (term%config%i_component /= i_real_fin) &
                    call term%apply_fks (core, alpha_s)
              case (NLO_VIRTUAL)
                 call term%evaluate_sqme_virt (alpha_s)
              case (NLO_MISMATCH)
                 call term%evaluate_sqme_mismatch (alpha_s)
              case (NLO_DGLAP)
                 call term%evaluate_sqme_dglap (alpha_s)
              end select
            end if
            core_sub => null ()
          end associate
       end do

       core => null ()
       if (instance%pcm%is_valid ()) then
          instance%evaluation_status = STAT_EVALUATED_TRACE
       else
          instance%evaluation_status = STAT_FAILED_KINEMATICS
       end if

       call instance%apply_powheg_dampings ()

       instance%sqme = instance%sqme + real (sum (&
          instance%term%connected%trace%get_matrix_element (1) * &
          instance%term%weight))
    else
       ! failed kinematics, failed cuts: set sqme to zero
       instance%sqme = zero
    end if
  contains

    subroutine set_sqme_born (term, i_flv)
      type(term_instance_t), intent(inout) :: term
      integer, intent(in) :: i_flv
      real(default) :: sqme
      sqme = real (term%connected%trace%get_matrix_element (i_flv))
      select case (term%nlo_type)
      case (NLO_REAL)
         term%pcm_instance%real_sub%sqme_born (i_flv) = sqme
      case (NLO_MISMATCH)
         term%pcm_instance%soft_mismatch%sqme_born (i_flv) = sqme
      end select
    end subroutine set_sqme_born
  end subroutine process_instance_evaluate_trace

  subroutine process_instance_apply_powheg_dampings (instance, only_component)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in), optional :: only_component
    integer :: i, i_term
    integer, dimension(:), allocatable :: i_terms
    integer :: i_skip
    i_skip = 0; if (present (only_component)) i_skip = only_component
    associate (process => instance%process)
       if (.not. process%uses_powheg_damping_factors ()) return
       do i = 1, process%get_n_components ()
          if (i_skip > 0 .and. i /= i_skip) cycle
          if (instance%process%component_is_selected (i) .and. &
             process%get_component_nlo_type (i) == NLO_REAL) then
             allocate (i_terms (size (process%get_component_i_terms (i))))
             i_terms = process%get_component_i_terms (i)
             do i_term = 1, size (i_terms)
                call instance%term(i_terms(i_term))%apply_damping_factor &
                   (process%get_component_type (i))
             end do
          end if
          if (allocated (i_terms)) deallocate (i_terms)
       end do
    end associate
  end subroutine process_instance_apply_powheg_dampings

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
       !!! failed kinematics etc.: set weight to zero
       instance%weight = zero
       !!! Maybe we want to keep the event nevertheless
       if (instance%keep_failed_events ()) then
          !!! Force factorization scale, otherwise writing to event output fails
          do i = 1, size (instance%term)
             instance%term(i)%fac_scale = zero
          end do
          instance%evaluation_status = STAT_EVENT_COMPLETE
       end if
    end if
  end subroutine process_instance_evaluate_event_data

  subroutine process_instance_compute_sqme_rad &
       (instance, i_term, i_phs, is_subtraction, alpha_s_external)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_term, i_phs
    logical, intent(in) :: is_subtraction
    real(default), intent(in), optional :: alpha_s_external
    class(prc_core_t), pointer :: core
    integer :: i_real, i_real_fin

    select type (pcm => instance%pcm)
    type is (pcm_instance_nlo_t)
       associate (term => instance%term(i_term))
         core => instance%process%get_core_term (i_term)
         if (is_subtraction) then
            call pcm%disable_sqme_np1 ()
         else
            call pcm%disable_subtraction ()
         end if
         call term%int_hard%set_momenta (pcm%get_momenta &
            (i_phs = i_phs, born_phsp = is_subtraction))
         if (allocated (term%core_state)) &
            call term%core_state%reset_new_kinematics ()
         if (present (alpha_s_external)) &
            call term%set_alpha_qcd_forced (alpha_s_external)
         call term%compute_eff_kinematics ()
         call term%evaluate_expressions ()
         call term%evaluate_interaction (core)
         call term%evaluate_trace ()
         pcm%real_sub%sqme_born (1) = &
            real (term%connected%trace%get_matrix_element (1))
         call term%evaluate_color_correlations (term%p_seed, core)
         i_real_fin = instance%process%get_associated_real_fin (1)
         if (term%config%i_component /= i_real_fin) &
            call term%apply_fks (core, core%get_alpha_s (term%core_state))
         if (instance%process%uses_powheg_damping_factors ()) then
            i_real = instance%get_associated_real ()
            call instance%apply_powheg_dampings (i_real)
         end if
       end associate
    end select
    core => null ()
  end subroutine process_instance_compute_sqme_rad

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
    integer, dimension(:), allocatable :: i_terms
    integer :: i, i_term_base, cc
    integer :: n_channel

    val = 0
    associate (process => sampler%process)
       FIND_COMPONENT: do i = 1, process%get_n_components ()
         if (sampler%process%component_is_selected (i)) then
            allocate (i_terms (size (process%get_component_i_terms (i))))
            i_terms = process%get_component_i_terms (i)
            i_term_base = i_terms(1)
            associate (k => sampler%term(i_term_base)%k_term)
              n_channel = k%n_channel
              do cc = 1, n_channel
                 call k%get_mcpar (cc, x(:,cc))
              end do
              f = k%f
              val = sampler%sqme * k%phs_factor
            end associate
            if (allocated (i_terms)) deallocate (i_terms)
            exit FIND_COMPONENT
         end if
       end do FIND_COMPONENT
    end associate
  end subroutine process_instance_fetch

  subroutine process_instance_init_simulation (instance, i_mci, &
     safety_factor, keep_failed_events)
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_mci
    real(default), intent(in), optional :: safety_factor
    logical, intent(in), optional :: keep_failed_events
    call instance%mci_work(i_mci)%init_simulation (safety_factor, keep_failed_events)
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
       do i = 1, size (instance%term)
          if (instance%term(i)%active) then
             call instance%term(i)%k_term%get_mcpar (channel, x)
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

  function process_instance_select_i_term (instance) result (i_term)
    integer :: i_term
    class(process_instance_t), intent(in) :: instance
    integer :: i_mci
    i_mci = instance%i_mci
    i_term = instance%process%select_i_term (i_mci)
  end function process_instance_select_i_term

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
    ptr => instance%term(i_term)%connected%get_matrix_int_ptr ()
  end function process_instance_get_matrix_int_ptr

  function process_instance_get_flows_int_ptr (instance, i_term) result (ptr)
    class(process_instance_t), intent(in), target :: instance
    integer, intent(in) :: i_term
    type(interaction_t), pointer :: ptr
    ptr => instance%term(i_term)%connected%get_flows_int_ptr ()
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

  function process_instance_get_sqme (instance, i_term) result (sqme)
    real(default) :: sqme
    class(process_instance_t), intent(in) :: instance
    integer, intent(in), optional :: i_term
    if (instance%evaluation_status >= STAT_EVALUATED_TRACE) then
       if (present (i_term)) then
          sqme = instance%term(i_term)%connected%trace%get_matrix_element (1)
       else
          sqme = instance%sqme
       end if
    else
       sqme = 0
    end if
  end function process_instance_get_sqme

  function process_instance_get_weight (instance) result (weight)
    real(default) :: weight
    class(process_instance_t), intent(in) :: instance
    if (instance%evaluation_status >= STAT_EVENT_COMPLETE) then
       weight = instance%weight
    else
       weight = 0
    end if
  end function process_instance_get_weight

  function process_instance_get_excess (instance) result (excess)
    real(default) :: excess
    class(process_instance_t), intent(in) :: instance
    if (instance%evaluation_status >= STAT_EVENT_COMPLETE) then
       excess = instance%excess
    else
       excess = 0
    end if
  end function process_instance_get_excess

  function process_instance_get_channel (instance) result (channel)
    integer :: channel
    class(process_instance_t), intent(in) :: instance
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
    real(default) :: alpha_s
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    class(prc_core_t), pointer :: core => null ()
    core => instance%process%get_core_term (i_term)
    alpha_s = instance%term(i_term)%get_alpha_s (core)
    core => null ()
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

  pure function process_instance_get_counter (process_instance) result (counter)
    class(process_instance_t), intent(in) :: process_instance
    type(process_counter_t) :: counter
    counter = process_instance%mci_work(process_instance%i_mci)%get_counter ()
  end function process_instance_get_counter

  pure function process_instance_get_actual_calls_total (process_instance) &
       result (n)
    class(process_instance_t), intent(in) :: process_instance
    integer :: n
    integer :: i
    type(process_counter_t) :: counter
    n = 0
    do i = 1, size (process_instance%mci_work)
       counter = process_instance%mci_work(i)%get_counter ()
       n = n + counter%total
    end do
  end function process_instance_get_actual_calls_total

  subroutine process_instance_reset_matrix_elements (instance)
    class(process_instance_t), intent(inout) :: instance
    integer :: i_term
    do i_term = 1, size (instance%term)
       call instance%term(i_term)%connected%trace%set_matrix_element (cmplx (0,0,default))
    end do
  end subroutine process_instance_reset_matrix_elements

  subroutine process_instance_get_phase_space_point (instance, &
         i_component, i_core, p)
    type(vector4_t), dimension(:), allocatable, intent(out) :: p
    class(process_instance_t), intent(inout) :: instance
    integer, intent(in) :: i_component, i_core
    real(default), dimension(:), allocatable :: x
    logical :: success
    integer :: i_term
    instance%i_mci = i_component
    i_term = instance%process%get_i_term (i_core)
    associate (term => instance%term(i_term))
       allocate (x (size (term%p_hard)))
       x = 0.5_default
       call instance%set_mcpar (x)
       call instance%select_channel (1)
       call term%compute_seed_kinematics &
          (instance%mci_work(i_component), 1, success)
       allocate (p (size (term%p_hard)))
       p = term%p_seed
    end associate
  end subroutine process_instance_get_phase_space_point

  function process_instance_get_associated_real (instance, avoid_finite) result (i_real)
    integer :: i_real
    class(process_instance_t), intent(in) :: instance
    logical, intent(in), optional :: avoid_finite
    logical :: avoid
    avoid = .false.; if (present (avoid_finite)) avoid = avoid_finite
    select type (pcm => instance%pcm)
    type is (pcm_instance_nlo_t)
       if (instance%process%get_component_type (pcm%active_real_component) &
          == COMP_REAL_FIN .and. .not. avoid) then
          i_real = instance%process%get_associated_real_fin (pcm%active_real_component)
       else
          i_real = pcm%active_real_component
       end if
    end select
  end function process_instance_get_associated_real

  function process_instance_get_connected_states (instance, i_component) result (connected)
    type(connected_state_t), dimension(:), allocatable :: connected
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_component
    connected = instance%process%get_connected_states (i_component, &
        instance%term(:)%connected)
  end function process_instance_get_connected_states

  function process_instance_get_sqrts (instance) result (sqrts)
    class(process_instance_t), intent(in) :: instance
    real(default) :: sqrts
    sqrts = instance%process%get_sqrts ()
  end function process_instance_get_sqrts

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

  function process_instance_has_nlo_component (instance) result (nlo)
    class(process_instance_t), intent(in) :: instance
    logical :: nlo
    nlo = instance%process%is_nlo_calculation ()
  end function process_instance_has_nlo_component

  function process_instance_keep_failed_events (instance) result (keep)
    logical :: keep
    class(process_instance_t), intent(in) :: instance
    keep = instance%mci_work(instance%i_mci)%keep_failed_events
  end function process_instance_keep_failed_events

  subroutine process_instance_create_and_load_extra_libraries &
     (instance, beam_structure, os_data)
    class(process_instance_t), intent(inout), target :: instance
    type(beam_structure_t), intent(in) :: beam_structure
    type(os_data_t), intent(in) :: os_data
    integer :: i, i_component, n_sub
    type(vector4_t), dimension(:), allocatable :: p
    type(core_manager_t), pointer :: cm => null ()

    call instance%process%create_and_load_extra_libraries ( &
         beam_structure, os_data, n_sub)
    cm => instance%process%get_core_manager_ptr ()
    do i = 1, cm%n_cores
       select type (core => cm%cores(i)%core)
       class is (prc_blha_t)
          if (cm%nlo_type(i) /= NLO_SUBTRACTION .and. &
               core%includes_polarization ()) then
             i_component = cm%i_core_to_first_i_component (i)
             call instance%transfer_helicities (i_component, core)
             call instance%get_phase_space_point (i_component, i, p)
             call core%warmup_helicities (p)
          else
             call core%set_helicity_list_trivial (n_sub)
          end if
       end select
    end do
  end subroutine process_instance_create_and_load_extra_libraries

  function process_instance_get_boost_to_lab (instance, i_term) result (lt)
    type(lorentz_transformation_t) :: lt
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    lt = instance%term(i_term)%get_boost_to_lab ()
  end function process_instance_get_boost_to_lab

  function process_instance_get_boost_to_cms (instance, i_term) result (lt)
    type(lorentz_transformation_t) :: lt
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    lt = instance%term(i_term)%get_boost_to_cms ()
  end function process_instance_get_boost_to_cms

  function process_instance_is_cm_frame (instance, i_term) result (cm_frame)
    logical :: cm_frame
    class(process_instance_t), intent(in) :: instance
    integer, intent(in) :: i_term
    cm_frame = instance%term(i_term)%k_term%phs%is_cm_frame ()
  end function process_instance_is_cm_frame

  subroutine pacify_process_instance (instance)
    type(process_instance_t), intent(inout) :: instance
    integer :: i
    do i = 1, size (instance%term)
       call pacify (instance%term(i)%k_term%phs)
    end do
  end subroutine pacify_process_instance


end module instances
