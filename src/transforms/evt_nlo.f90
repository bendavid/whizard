! WHIZARD 2.6.0 Sep 08 2017
!
! Copyright (C) 1999-2017 by
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

module evt_nlo

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units, only: given_output_unit
  use constants
  use lorentz
  use diagnostics
  use physics_defs, only: NLO_REAL
  use sm_qcd
  use model_data
  use particles
  use instances, only: process_instance_t
  ! TODO (cw-2016-09-16): Ideally, only pcm_base
  use pcm, only: pcm_nlo_t, pcm_instance_nlo_t
  use process_stacks
  use event_transforms

  use phs_fks, only: phs_fks_t, phs_fks_generator_t
  use phs_fks, only: phs_identifier_t, phs_point_set_t
  use resonances, only: resonance_contributors_t
  use fks_regions, only: region_data_t

  implicit none
  private

  public :: evt_nlo_t

  integer, parameter, public :: EVT_NLO_UNDEFINED = 0
  integer, parameter, public :: EVT_NLO_SEPARATE_BORNLIKE = 1
  integer, parameter, public :: EVT_NLO_SEPARATE_REAL = 2
  integer, parameter, public :: EVT_NLO_COMBINED = 3

  type :: nlo_event_deps_t
     logical :: cm_frame = .true.
     type(phs_point_set_t) :: p_born_cms
     type(phs_point_set_t) :: p_born_lab
     type(phs_point_set_t) :: p_real_cms
     type(phs_point_set_t) :: p_real_lab
     type(resonance_contributors_t), dimension(:), allocatable :: contributors
     type(phs_identifier_t), dimension(:), allocatable :: phs_identifiers
     integer, dimension(:), allocatable :: alr_to_i_con
     integer :: n_phs = 0
  end type nlo_event_deps_t

  type, extends (evt_t) :: evt_nlo_t
    type(phs_fks_generator_t) :: phs_fks_generator
    real(default) :: sqme_rad = zero
    integer :: i_evaluation = 0
    integer :: weight_multiplier = 1
    type(particle_set_t), dimension(:), allocatable :: particle_set_radiated
    type(qcd_t), pointer :: qcd => null ()
    type(nlo_event_deps_t) :: event_deps
    integer :: mode = EVT_NLO_UNDEFINED
    integer, dimension(:), allocatable :: &
       i_evaluation_to_i_phs, i_evaluation_to_emitter, &
       i_evaluation_to_i_term
    logical :: keep_failed_events = .false.
    integer :: selected_i_flv = 0
  contains
    procedure :: write_name => evt_nlo_write_name
    procedure :: write => evt_nlo_write
    procedure :: connect => evt_nlo_connect
    procedure :: set_i_evaluation_mappings => evt_nlo_set_i_evaluation_mappings
    procedure :: get_i_phs => evt_nlo_get_i_phs
    procedure :: get_emitter => evt_nlo_get_emitter
    procedure :: get_i_term => evt_nlo_get_i_term
    procedure :: copy_previous_particle_set => evt_nlo_copy_previous_particle_set
    procedure :: generate_weighted => evt_nlo_generate_weighted
    procedure :: reset_phs_identifiers => evt_nlo_reset_phs_identifiers
    procedure :: make_particle_set => evt_nlo_make_particle_set
    procedure :: keep_and_boost_born_particle_set => &
         evt_nlo_keep_and_boost_born_particle_set
    procedure :: evaluate_real_kinematics => evt_nlo_evaluate_real_kinematics
    procedure :: compute_subtraction_weights => evt_nlo_compute_subtraction_weights
    procedure :: compute_real => evt_nlo_compute_real
    procedure :: boost_to_cms => evt_nlo_boost_to_cms
    procedure :: boost_to_lab => evt_nlo_boost_to_lab
    procedure :: setup_general_event_kinematics => evt_nlo_setup_general_event_kinematics
    procedure :: setup_real_event_kinematics => evt_nlo_setup_real_event_kinematics
    procedure :: set_mode => evt_nlo_set_mode
    procedure :: is_valid_event => evt_nlo_is_valid_event
    procedure :: prepare_new_event => evt_nlo_prepare_new_event
  end type evt_nlo_t


contains

  subroutine evt_nlo_write_name (evt, unit)
    class(evt_nlo_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "Event transform: NLO"
  end subroutine evt_nlo_write_name

  subroutine evt_nlo_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_nlo_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
  end subroutine evt_nlo_write

  subroutine evt_nlo_connect (evt, process_instance, model, process_stack)
    class(evt_nlo_t), intent(inout), target :: evt
    type(process_instance_t), intent(in), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    call msg_debug (D_TRANSFORMS, "evt_nlo_connect")
    call evt%base_connect (process_instance, model, process_stack)
    select type (pcm => process_instance%pcm)
    class is (pcm_instance_nlo_t)
       select type (config => pcm%config)
       type is (pcm_nlo_t)
          call config%setup_phs_generator (pcm, evt%phs_fks_generator, &
               process_instance%get_sqrts ())
          call evt%set_i_evaluation_mappings (config%region_data, &
               pcm%real_kinematics%alr_to_i_phs)
       end select
    end select
    call evt%set_mode (process_instance)
    call evt%setup_general_event_kinematics (process_instance)
    if (evt%mode > EVT_NLO_SEPARATE_BORNLIKE) &
         call evt%setup_real_event_kinematics (process_instance)
    call msg_debug2 (D_TRANSFORMS, "evt_nlo_connect: success")
  end subroutine evt_nlo_connect

  subroutine evt_nlo_set_i_evaluation_mappings (evt, reg_data, alr_to_i_phs)
    class(evt_nlo_t), intent(inout) :: evt
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in), dimension(:) :: alr_to_i_phs
    integer :: n_phs, alr
    integer :: i_evaluation, i_phs, emitter
    logical :: checked
    type :: registered_triple_t
      integer, dimension(2) :: phs_em
      type(registered_triple_t), pointer :: next => null ()
    end type registered_triple_t
    type(registered_triple_t), allocatable, target :: check_list
    i_evaluation = 1
    n_phs = reg_data%n_phs
    evt%weight_multiplier = n_phs + 1
    allocate (evt%i_evaluation_to_i_phs (n_phs), source = 0)
    allocate (evt%i_evaluation_to_emitter (n_phs), source = -1)
    allocate (evt%i_evaluation_to_i_term (0 : n_phs), source = 0)
    do alr = 1, reg_data%n_regions
       i_phs = alr_to_i_phs (alr)
       emitter = reg_data%regions(alr)%emitter
       call search_check_list (checked)
       if (.not. checked) then
          evt%i_evaluation_to_i_phs (i_evaluation) = i_phs
          evt%i_evaluation_to_emitter (i_evaluation) = emitter
          i_evaluation = i_evaluation + 1
       end if
    end do
    call fill_i_evaluation_to_i_term ()
    if (.not. (all (evt%i_evaluation_to_i_phs > 0) &
       .and. all (evt%i_evaluation_to_emitter > -1))) then
       call msg_fatal ("evt_nlo: Inconsistent mappings!")
    else
       if (debug2_active (D_TRANSFORMS)) then
          print *, 'evt_nlo Mappings, i_evaluation -> '
          print *, 'i_phs: ', evt%i_evaluation_to_i_phs
          print *, 'emitter: ', evt%i_evaluation_to_emitter
       end if
    end if
  contains
    subroutine fill_i_evaluation_to_i_term ()
      integer :: i_term, i_evaluation, term_emitter
      !!! First find subtraction component
      i_evaluation = 1
      do i_term = 1, evt%process%get_n_terms ()
         if (evt%process_instance%term(i_term)%nlo_type /= NLO_REAL) cycle
         term_emitter = evt%process_instance%term(i_term)%k_term%emitter
         if (term_emitter < 0) then
            evt%i_evaluation_to_i_term (0) = i_term
         else if (evt%i_evaluation_to_emitter(i_evaluation) == term_emitter) then
            evt%i_evaluation_to_i_term (i_evaluation) = i_term
            i_evaluation = i_evaluation + 1
         end if
      end do
    end subroutine fill_i_evaluation_to_i_term

    subroutine search_check_list (found)
      logical, intent(out) :: found
      type(registered_triple_t), pointer :: current_triple => null ()
      if (allocated (check_list)) then
         current_triple => check_list
         do
            if (all (current_triple%phs_em == [i_phs, emitter])) then
               found = .true.
               exit
            end if
            if (.not. associated (current_triple%next)) then
               allocate (current_triple%next)
               current_triple%next%phs_em = [i_phs, emitter]
               found = .false.
               exit
            else
               current_triple => current_triple%next
            end if
         end do
      else
         allocate (check_list)
         check_list%phs_em = [i_phs, emitter]
         found = .false.
      end if
    end subroutine search_check_list
  end subroutine evt_nlo_set_i_evaluation_mappings

  function evt_nlo_get_i_phs (evt) result (i_phs)
    integer :: i_phs
    class(evt_nlo_t), intent(in) :: evt
    i_phs = evt%i_evaluation_to_i_phs (evt%i_evaluation)
  end function evt_nlo_get_i_phs

  function evt_nlo_get_emitter (evt) result (emitter)
    integer :: emitter
    class(evt_nlo_t), intent(in) :: evt
    emitter = evt%i_evaluation_to_emitter (evt%i_evaluation)
  end function evt_nlo_get_emitter

  function evt_nlo_get_i_term (evt) result (i_term)
    integer :: i_term
    class(evt_nlo_t), intent(in) :: evt
    if (evt%mode >= EVT_NLO_SEPARATE_REAL) then
       i_term = evt%i_evaluation_to_i_term (evt%i_evaluation)
    else
       i_term = evt%process_instance%get_first_active_i_term ()
    end if
  end function evt_nlo_get_i_term

  subroutine evt_nlo_copy_previous_particle_set (evt)
    class(evt_nlo_t), intent(inout) :: evt
    if (associated (evt%previous)) then
       evt%particle_set = evt%previous%particle_set
    else
       call msg_fatal ("evt_nlo requires one preceeding evt_trivial!")
    end if
  end subroutine evt_nlo_copy_previous_particle_set

  subroutine evt_nlo_generate_weighted (evt, probability)
    class(evt_nlo_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    real(default) :: weight
    call print_debug_info ()
    if (evt%mode > EVT_NLO_SEPARATE_BORNLIKE) then
       if (evt%i_evaluation == 0) then
          call evt%reset_phs_identifiers ()
          call evt%evaluate_real_kinematics ()
          weight = evt%compute_subtraction_weights ()
          if (evt%mode == EVT_NLO_SEPARATE_REAL) then
             probability = weight
          else
             probability = probability + weight
          end if
       else
          call evt%compute_real ()
          probability = evt%sqme_rad
       end if
       call msg_debug2 (D_TRANSFORMS, "event weight multiplier:", evt%weight_multiplier)
       probability = probability * evt%weight_multiplier
    end if
    call msg_debug (D_TRANSFORMS, "probability (after)", probability)
    evt%particle_set_exists = .true.
  contains
    function status_code_to_string (mode) result (smode)
      type(string_t) :: smode
      integer, intent(in) :: mode
      select case (mode)
      case (EVT_NLO_UNDEFINED)
         smode = var_str ("Undefined")
      case (EVT_NLO_SEPARATE_BORNLIKE)
         smode = var_str ("Born-like")
      case (EVT_NLO_SEPARATE_REAL)
         smode = var_str ("Real")
      case (EVT_NLO_COMBINED)
         smode = var_str ("Combined")
      end select
    end function status_code_to_string

    subroutine print_debug_info ()
       call msg_debug (D_TRANSFORMS, "evt_nlo_generate_weighted")
       call msg_debug (D_TRANSFORMS, char ("mode: " // status_code_to_string (evt%mode)))
       call msg_debug (D_TRANSFORMS, "probability (before)", probability)
       call msg_debug (D_TRANSFORMS, "evt%i_evaluation", evt%i_evaluation)
       if (debug2_active (D_TRANSFORMS)) then
          if (evt%mode > EVT_NLO_SEPARATE_BORNLIKE) then
             if (evt%i_evaluation == 0) then
                print *, 'Evaluate subtraction component'
             else
                print *, 'Evaluate radiation component'
             end if
          end if
       end if
    end subroutine print_debug_info
  end subroutine evt_nlo_generate_weighted

  subroutine evt_nlo_reset_phs_identifiers (evt)
     class(evt_nlo_t), intent(inout) :: evt
     evt%event_deps%phs_identifiers%evaluated = .false.
  end subroutine evt_nlo_reset_phs_identifiers

  subroutine evt_nlo_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    if (evt%mode >= EVT_NLO_SEPARATE_BORNLIKE) then
       select type (config => evt%process_instance%pcm%config)
       type is (pcm_nlo_t)
          if (evt%i_evaluation > 0) then
             call make_factorized_particle_set (evt, factorization_mode, &
                  keep_correlations, r, evt%get_i_term (), &
                  config%qn_real(:, evt%selected_i_flv))
          else
             call make_factorized_particle_set (evt, factorization_mode, &
                  keep_correlations, r, evt%get_i_term (), &
                  config%qn_born(:, evt%selected_i_flv))
          end if
       end select
    else
       call make_factorized_particle_set (evt, factorization_mode, &
            keep_correlations, r)
    end if
  end subroutine evt_nlo_make_particle_set

  subroutine evt_nlo_keep_and_boost_born_particle_set (evt, i_event)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_event
    evt%particle_set_radiated(i_event) = evt%particle_set
    if (evt%event_deps%cm_frame) then
       evt%event_deps%p_born_cms%phs_point(1) = &
          evt%particle_set%get_in_and_out_momenta ()
       evt%event_deps%p_born_lab%phs_point(1) = &
          evt%boost_to_lab (evt%event_deps%p_born_cms%phs_point(1))
       call evt%particle_set_radiated(i_event)%replace_incoming_momenta &
          (evt%event_deps%p_born_lab%phs_point(1)%p)
       call evt%particle_set_radiated(i_event)%replace_outgoing_momenta &
          (evt%event_deps%p_born_lab%phs_point(1)%p)
    end if
  end subroutine evt_nlo_keep_and_boost_born_particle_set

  subroutine evt_nlo_evaluate_real_kinematics (evt)
    class(evt_nlo_t), intent(inout) :: evt
    integer :: alr, i_phs, i_con, emitter
    real(default), dimension(3) :: x_rad
    logical :: use_contributors
    integer :: i_term

    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       x_rad = pcm%real_kinematics%x_rad
       associate (event_deps => evt%event_deps)
          i_term = evt%get_i_term ()
          event_deps%p_born_lab%phs_point(1) = &
               evt%process_instance%term(i_term)%connected%matrix%get_momenta ()
          event_deps%p_born_cms%phs_point(1) &
               = evt%boost_to_cms (event_deps%p_born_lab%phs_point(1))
          call evt%phs_fks_generator%set_sqrts_hat &
               (event_deps%p_born_cms%get_energy (1, 1))
          use_contributors = allocated (event_deps%contributors)
          do alr = 1, pcm%get_n_regions ()
             i_phs = pcm%real_kinematics%alr_to_i_phs(alr)
             if (event_deps%phs_identifiers(i_phs)%evaluated) cycle
             emitter = event_deps%phs_identifiers(i_phs)%emitter
             associate (generator => evt%phs_fks_generator)
                !!! TODO: (cw-2016-12-30): Replace by n_in
                if (emitter <= 2) then
                   call generator%prepare_generation (x_rad, i_phs, emitter, &
                        event_deps%p_born_cms%phs_point(1)%p, event_deps%phs_identifiers)
                   call generator%generate_isr (i_phs, &
                        event_deps%p_born_lab%phs_point(1)%p, &
                        event_deps%p_real_lab%phs_point(i_phs)%p)
                   event_deps%p_real_cms%phs_point(i_phs) &
                        = evt%boost_to_cms (event_deps%p_real_lab%phs_point(i_phs))
                else
                   if (use_contributors) then
                      i_con = event_deps%alr_to_i_con(alr)
                      call generator%prepare_generation (x_rad, i_phs, emitter, &
                           event_deps%p_born_cms%phs_point(1)%p, &
                           event_deps%phs_identifiers, event_deps%contributors, i_con)
                      call generator%generate_fsr (emitter, i_phs, i_con, &
                           event_deps%p_born_cms%phs_point(1)%p, &
                           event_deps%p_real_cms%phs_point(i_phs)%p)
                   else
                      call generator%prepare_generation (x_rad, i_phs, emitter, &
                           event_deps%p_born_cms%phs_point(1)%p, event_deps%phs_identifiers)
                      call generator%generate_fsr (emitter, i_phs, &
                           event_deps%p_born_cms%phs_point(1)%p, &
                           event_deps%p_real_cms%phs_point(i_phs)%p)
                   end if
                   event_deps%p_real_lab%phs_point(i_phs) &
                        = evt%boost_to_lab (event_deps%p_real_cms%phs_point(i_phs))
                end if
             end associate
             call pcm%set_momenta (event_deps%p_born_lab%phs_point(1)%p, &
                  event_deps%p_real_lab%phs_point(i_phs)%p, i_phs)
             call pcm%set_momenta (event_deps%p_born_cms%phs_point(1)%p, &
                  event_deps%p_real_cms%phs_point(i_phs)%p, i_phs, cms = .true.)
             event_deps%phs_identifiers(i_phs)%evaluated = .true.
          end do
       end associate
    end select
  end subroutine evt_nlo_evaluate_real_kinematics

  function evt_nlo_compute_subtraction_weights (evt) result (weight)
    class(evt_nlo_t), intent(inout) :: evt
    real(default) :: weight
    integer :: i_phs, i_term
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_subtraction_weights")
    weight = zero
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       associate (event_deps => evt%event_deps)
          i_phs = 1; i_term = evt%i_evaluation_to_i_term(0)
          call evt%process_instance%compute_sqme_rad (i_term, i_phs, .true.)
          weight = weight + evt%process_instance%get_sqme (i_term)
       end associate
    end select
  end function evt_nlo_compute_subtraction_weights

  subroutine evt_nlo_compute_real (evt)
    class(evt_nlo_t), intent(inout) :: evt
    integer :: i_phs, i_term
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_real")
    i_phs = evt%get_i_phs ()
    i_term = evt%i_evaluation_to_i_term (evt%i_evaluation)
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       associate (event_deps => evt%event_deps)
          call evt%process_instance%compute_sqme_rad (i_term, i_phs, .false.)
          evt%sqme_rad = evt%process_instance%get_sqme (i_term)
       end associate
    end select
  end subroutine evt_nlo_compute_real

  function evt_nlo_boost_to_cms (evt, p_lab) result (p_cms)
    type(phs_point_t), intent(in) :: p_lab
    class(evt_nlo_t), intent(in) :: evt
    type(phs_point_t) :: p_cms
    type(lorentz_transformation_t) :: lt_lab_to_cms
    integer :: i_boost
    if (evt%event_deps%cm_frame) then
       lt_lab_to_cms = identity
    else
       if (evt%mode == EVT_NLO_COMBINED) then
          i_boost = 1
       else
          i_boost = evt%process_instance%select_i_term ()
       end if
       lt_lab_to_cms = evt%process_instance%get_boost_to_cms (i_boost)
    end if
    p_cms = lt_lab_to_cms * p_lab
  end function evt_nlo_boost_to_cms

  function evt_nlo_boost_to_lab (evt, p_cms) result (p_lab)
    type(phs_point_t) :: p_lab
    class(evt_nlo_t), intent(in) :: evt
    type(phs_point_t), intent(in) :: p_cms
    type(lorentz_transformation_t) :: lt_cms_to_lab
    integer :: i_boost
    if (.not. evt%event_deps%cm_frame) then
       lt_cms_to_lab = identity
    else
       if (evt%mode == EVT_NLO_COMBINED) then
          i_boost = 1
       else
          i_boost = evt%process_instance%select_i_term ()
       end if
       lt_cms_to_lab = evt%process_instance%get_boost_to_lab (i_boost)
    end if
    p_lab = lt_cms_to_lab * p_cms
  end function evt_nlo_boost_to_lab

  subroutine evt_nlo_setup_general_event_kinematics (evt, process_instance)
    class(evt_nlo_t), intent(inout) :: evt
    type(process_instance_t), intent(in) :: process_instance
    integer :: n_born
    associate (event_deps => evt%event_deps)
       event_deps%cm_frame = process_instance%is_cm_frame (1)
       select type (pcm => process_instance%pcm)
       type is (pcm_instance_nlo_t)
          n_born = pcm%get_n_born ()
       end select
       call event_deps%p_born_cms%init (n_born, 1)
       call event_deps%p_born_lab%init (n_born, 1)
    end associate
  end subroutine evt_nlo_setup_general_event_kinematics

  subroutine evt_nlo_setup_real_event_kinematics (evt, process_instance)
    class(evt_nlo_t), intent(inout) :: evt
    type(process_instance_t), intent(in) :: process_instance
    integer :: n_real, n_phs
    integer :: i_real
    associate (event_deps => evt%event_deps)
       select type (pcm => process_instance%pcm)
       class is (pcm_instance_nlo_t)
          n_real = pcm%get_n_real ()
       end select
       i_real = evt%process%get_first_real_term ()
       select type (phs => process_instance%term(i_real)%k_term%phs)
       type is (phs_fks_t)
          event_deps%phs_identifiers = phs%phs_identifiers
       end select
       n_phs = size (event_deps%phs_identifiers)
       call event_deps%p_real_cms%init (n_real, n_phs)
       call event_deps%p_real_lab%init (n_real, n_phs)
       select type (pcm => process_instance%pcm)
       type is (pcm_instance_nlo_t)
          select type (config => pcm%config)
          type is (pcm_nlo_t)
             if (allocated (config%region_data%alr_contributors)) then
                allocate (event_deps%contributors (size (config%region_data%alr_contributors)))
                event_deps%contributors = config%region_data%alr_contributors
             end if
             if (allocated (config%region_data%alr_to_i_contributor)) then
                allocate (event_deps%alr_to_i_con &
                   (size (config%region_data%alr_to_i_contributor)))
                event_deps%alr_to_i_con = config%region_data%alr_to_i_contributor
             end if
          end select
       end select
    end associate
  end subroutine evt_nlo_setup_real_event_kinematics

  subroutine evt_nlo_set_mode (evt, process_instance)
    class(evt_nlo_t), intent(inout) :: evt
    type(process_instance_t), intent(in) :: process_instance
    integer :: i_real
    select type (pcm => process_instance%pcm)
    type is (pcm_instance_nlo_t)
       select type (config => pcm%config)
       type is (pcm_nlo_t)
          if (config%settings%combined_integration) then
             evt%mode = EVT_NLO_COMBINED
          else
             i_real = evt%process%get_first_real_component ()
             if (i_real == evt%process%extract_active_component_mci ()) then
                evt%mode = EVT_NLO_SEPARATE_REAL
             else
                evt%mode = EVT_NLO_SEPARATE_BORNLIKE
             end if
          end if
       end select
    end select
  end subroutine evt_nlo_set_mode

  function evt_nlo_is_valid_event (evt, i_term) result (valid)
    logical :: valid
    class(evt_nlo_t), intent(in) :: evt
    integer, intent(in) :: i_term
    valid = evt%process_instance%term(i_term)%passed
  end function evt_nlo_is_valid_event

  subroutine evt_nlo_prepare_new_event (evt, i_mci, i_term)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    real(default) :: s, x
    real(default) :: sqme_total
    real(default), dimension(:), allocatable :: sqme_flv
    integer :: i
    call evt%reset ()
    if (evt%i_evaluation > 0) return
    call evt%rng%generate (x)
    sqme_total = zero
    allocate (sqme_flv (evt%process_instance%term(1)%config%data%n_flv))
    sqme_flv = zero
    do i = 1, size (evt%process_instance%term)
       associate (term => evt%process_instance%term(i))
          sqme_total = sqme_total + real (sum ( &
               term%connected%matrix%get_matrix_element ()))
          sqme_flv = sqme_flv + real (term%connected%matrix%get_matrix_element ())
       end associate
    end do
    !!! Need absolute values to take into account negative weights
    x = x * abs (sqme_total)
    s = zero
    do i = 1, size (sqme_flv)
       s = s + abs (sqme_flv (i))
       if (s > x) then
          evt%selected_i_flv = i
          exit
       end if
    end do
    if (debug2_active (D_TRANSFORMS)) then
       call msg_print_color ("Selected i_flv: ", COL_GREEN)
       print *, evt%selected_i_flv
    end if
  end subroutine evt_nlo_prepare_new_event


end module evt_nlo

