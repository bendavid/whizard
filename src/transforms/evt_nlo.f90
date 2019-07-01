! WHIZARD 2.3.1 Aug 25 2016
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

module evt_nlo

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units, only: given_output_unit
  use constants
  use lorentz
  use diagnostics
  use sm_qcd
  use model_data
  use particles
  use processes
  use process_stacks
  use event_transforms

  use nlo_data, only: sqme_collector_t, phs_identifier_t, phs_point_set_t
  use cascades, only: resonance_contributors_t
  use phs_fks
  use nlo_controller, only: nlo_controller_t

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
     integer, dimension(:), allocatable :: i_evaluation_to_i_phs
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
       i_evaluation_to_i_phs, i_evaluation_to_i_flv, i_evaluation_to_emitter
  contains
    procedure :: write_name => evt_nlo_write_name
    procedure :: write => evt_nlo_write
    procedure :: connect => evt_nlo_connect
    procedure :: set_i_evaluation_mappings => evt_nlo_set_i_evaluation_mappings
    procedure :: get_i_phs => evt_nlo_get_i_phs
    procedure :: get_i_flv => evt_nlo_get_i_flv
    procedure :: get_emitter => evt_nlo_get_emitter
    procedure :: prepare_new_event => evt_nlo_prepare_new_event
    procedure :: generate_weighted => evt_nlo_generate_weighted
    procedure :: reset_phs_identifiers => evt_nlo_reset_phs_identifiers
    procedure :: make_particle_set => evt_nlo_make_particle_set
    procedure :: build_radiated_particle_set => evt_nlo_build_radiated_particle_set
    procedure :: evaluate_real_kinematics => evt_nlo_evaluate_real_kinematics
    procedure :: compute_subtraction_weights => evt_nlo_compute_subtraction_weights
    procedure :: compute_real => evt_nlo_compute_real
    procedure :: boost_to_cms => evt_nlo_boost_to_cms
    procedure :: boost_to_lab => evt_nlo_boost_to_lab
    procedure :: setup_event_kinematics => evt_nlo_setup_event_kinematics
    procedure :: set_mode => evt_nlo_set_mode
    procedure :: is_valid_event => evt_nlo_is_valid_event
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
    real(default) :: sqrts
    call msg_debug (D_TRANSFORMS, "evt_nlo_connect")
    call evt%base_connect (process_instance, model, process_stack)
    select type (pcm => process_instance%pcm)
    class is (pcm_instance_nlo_t)
       associate (generator => evt%phs_fks_generator)
         sqrts = process_instance%get_sqrts ()
         call pcm%controller%setup_generator (generator, sqrts)
       end associate
       call evt%set_i_evaluation_mappings (pcm%controller)
    end select
    call evt%set_mode (process_instance) 
    if (evt%mode > EVT_NLO_SEPARATE_BORNLIKE) &
       call evt%setup_event_kinematics (process_instance)
    call msg_debug2 (D_TRANSFORMS, "evt_nlo_connect: success")
  end subroutine evt_nlo_connect

  subroutine evt_nlo_set_i_evaluation_mappings (evt, controller)
    class(evt_nlo_t), intent(inout) :: evt
    type(nlo_controller_t), intent(in) :: controller
    integer :: n_flv, n_phs, alr
    integer :: i_evaluation, i_phs, i_flv, emitter
    integer, parameter :: N_MAX_PHS = 10, N_MAX_FLV = 100, N_MAX_EMITTER = 10
    logical :: checked
    type :: registered_triple_t
      integer, dimension(3) :: phs_flv_em
      type(registered_triple_t), pointer :: next => null ()
    end type registered_triple_t
    type(registered_triple_t), pointer :: check_list => null ()
    i_evaluation = 1
    associate (reg_data => controller%reg_data)
       n_phs = reg_data%n_phs; n_flv = reg_data%n_flv_real
       evt%weight_multiplier = n_phs * n_flv + 1
       allocate (evt%i_evaluation_to_i_phs (n_phs * n_flv), source = 0)
       allocate (evt%i_evaluation_to_i_flv (n_phs * n_flv), source = 0)
       allocate (evt%i_evaluation_to_emitter (n_phs * n_flv), source = -1)
       do alr = 1, reg_data%n_regions
          i_phs = controller%real_kinematics%alr_to_i_phs (alr)
          i_flv = reg_data%regions(alr)%real_index 
          emitter = reg_data%regions(alr)%emitter 
          call search_check_list (checked)
          if (.not. checked) then
             evt%i_evaluation_to_i_phs (i_evaluation) = i_phs
             evt%i_evaluation_to_i_flv (i_evaluation) = i_flv
             evt%i_evaluation_to_emitter (i_evaluation) = emitter
             i_evaluation = i_evaluation + 1
          end if
       end do 
    end associate
    if (.not. (all (evt%i_evaluation_to_i_phs > 0) &
       .and. all (evt%i_evaluation_to_i_flv > 0) &
       .and. all (evt%i_evaluation_to_emitter > -1))) then
       call msg_fatal ("evt_nlo: Inconsistent mappings!")
    else
       if (debug2_active (D_TRANSFORMS)) then
          print *, 'evt_nlo Mappings, i_evaluation -> '
          print *, 'i_phs: ', evt%i_evaluation_to_i_phs
          print *, 'i_flv: ', evt%i_evaluation_to_i_flv
          print *, 'emitter: ', evt%i_evaluation_to_emitter
       end if
    end if
  contains
    subroutine search_check_list (found)
      logical, intent(out) :: found
      type(registered_triple_t), pointer :: current_triple => null ()
      if (associated (check_list)) then
         current_triple => check_list
         do
            if (all (current_triple%phs_flv_em == [i_phs, i_flv, emitter])) then
               found = .true.
               exit
            end if 
            if (.not. associated (current_triple%next)) then
               allocate (current_triple%next)
               current_triple%next%phs_flv_em = [i_phs, i_flv, emitter]
               found = .false.
               exit
            else
               current_triple => current_triple%next
            end if
         end do
      else
         allocate (check_list)
         check_list%phs_flv_em = [i_phs, i_flv, emitter]
         found = .false.
      end if
    end subroutine search_check_list
  end subroutine evt_nlo_set_i_evaluation_mappings

  function evt_nlo_get_i_phs (evt) result (i_phs)
    integer :: i_phs
    class(evt_nlo_t), intent(in) :: evt
    i_phs = evt%i_evaluation_to_i_phs (evt%i_evaluation)
  end function evt_nlo_get_i_phs

  function evt_nlo_get_i_flv (evt) result (i_flv)
    integer :: i_flv
    class(evt_nlo_t), intent(in) :: evt
    i_flv = evt%i_evaluation_to_i_flv (evt%i_evaluation)
  end function evt_nlo_get_i_flv

  function evt_nlo_get_emitter (evt) result (emitter)
    integer :: emitter
    class(evt_nlo_t), intent(in) :: evt
    emitter = evt%i_evaluation_to_emitter (evt%i_evaluation)
  end function evt_nlo_get_emitter

  subroutine evt_nlo_prepare_new_event (evt, i_mci, i_term)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
  end subroutine evt_nlo_prepare_new_event

  subroutine evt_nlo_generate_weighted (evt, probability)
    class(evt_nlo_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    real(default) :: weight
    call print_debug_info ()
    evt%particle_set = evt%previous%particle_set
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
    evt%particle_set_exists = .true.
  end subroutine evt_nlo_make_particle_set

  subroutine evt_nlo_build_radiated_particle_set (evt, i_event)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_event
    integer :: emitter
    type(vector4_t), dimension(:), allocatable :: p_new
    integer, dimension(:), allocatable :: flv_radiated
    real(default) :: r_col
    integer :: i_phs, i_flv
    call msg_debug (D_TRANSFORMS, "evt_nlo_build_radiated_particle_set")
    call msg_debug (D_TRANSFORMS, "evt%i_evaluation", evt%i_evaluation)
    evt%particle_set_radiated(i_event) = evt%particle_set
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       if (evt%i_evaluation /= 0) then
          i_flv = evt%get_i_flv ()
          allocate (flv_radiated (size (pcm%controller%get_flv_state_real (i_flv))))
          flv_radiated = pcm%controller%get_flv_state_real (i_flv)
          call evt%rng%generate (r_col)
          call msg_debug2 (D_TRANSFORMS, "r_col", r_col)
          if (debug2_active (D_TRANSFORMS))  print *, 'flv_radiated =    ', flv_radiated
          i_phs = evt%get_i_phs()
          emitter = evt%get_emitter ()
          if (emitter == 0) emitter = choose_in_or_out ()
          call msg_debug (D_TRANSFORMS, "emitter", emitter)
          allocate (p_new (size (pcm%controller%get_momenta &
             (born_phsp = .false., i_phs = i_phs))))
          p_new = pcm%controller%get_momenta (born_phsp = .false., i_phs = i_phs)
          call evt%particle_set_radiated(i_event)%build_radiation (p_new, emitter, flv_radiated, &
             evt%process_instance%process%get_model_ptr (), r_col)
       end if
       evt%i_evaluation = evt%i_evaluation + 1
    end select
  contains
    function choose_in_or_out () result (em)
      integer :: em
      real(default) :: r
      call evt%rng%generate (r)
      if (r > 0.5_default) then
         em = 2
      else
         em = 1
      end if
    end function choose_in_or_out
  end subroutine evt_nlo_build_radiated_particle_set

  subroutine evt_nlo_evaluate_real_kinematics (evt)
    class(evt_nlo_t), intent(inout) :: evt
    integer :: alr, i_phs, i_con, emitter
    real(default), dimension(3) :: x_rad
    logical :: use_contributors

    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       x_rad = pcm%controller%real_kinematics%x_rad
       associate (event_deps => evt%event_deps)
          event_deps%p_born_lab%phs_point(1) = &
             evt%particle_set%get_in_and_out_momenta ()
          event_deps%p_born_cms%phs_point(1) &
             = evt%boost_to_cms (event_deps%p_born_lab%phs_point(1)) 
          call evt%phs_fks_generator%set_sqrts_hat &
             (event_deps%p_born_cms%get_energy (1, 1))
          use_contributors = allocated (event_deps%contributors)
          do alr = 1, size (event_deps%i_evaluation_to_i_phs)
             i_phs = event_deps%i_evaluation_to_i_phs(alr)
             if (event_deps%phs_identifiers(i_phs)%evaluated) cycle
             emitter = event_deps%phs_identifiers(i_phs)%emitter 
             associate (generator => evt%phs_fks_generator)
                if (emitter <= 2) then
                   !!!call msg_fatal ("NLO Events only supply final-state emissions")
                   call generator%prepare_generation (x_rad, i_phs, emitter, &
                      event_deps%p_born_cms%phs_point(1)%p, event_deps%phs_identifiers)
                   call generator%generate_isr (i_phs, &
                      event_deps%p_born_lab%phs_point(1)%p, &
                      event_deps%p_real_lab%phs_point(i_phs))
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
                         event_deps%p_real_cms%phs_point(i_phs))
                   else
                      call generator%prepare_generation (x_rad, i_phs, emitter, &
                         event_deps%p_born_cms%phs_point(1)%p, event_deps%phs_identifiers)
                      call generator%generate_fsr (emitter, i_phs, &
                         event_deps%p_born_cms%phs_point(1)%p, &
                         event_deps%p_real_cms%phs_point(i_phs))
                   end if
                end if
                event_deps%p_real_lab%phs_point(i_phs) &
                   = evt%boost_to_lab (event_deps%p_real_cms%phs_point(i_phs))
             end associate
             call pcm%controller%set_momenta &
                (event_deps%p_born_lab%phs_point(1)%p, &
                event_deps%p_real_lab%phs_point(i_phs)%p, i_phs)
             call pcm%controller%set_momenta &
                (event_deps%p_born_cms%phs_point(1)%p, &
                event_deps%p_real_cms%phs_point(i_phs)%p, i_phs, cms = .true.)
             event_deps%phs_identifiers(i_phs)%evaluated = .true.
          end do
       end associate
    end select
  end subroutine evt_nlo_evaluate_real_kinematics

  function evt_nlo_compute_subtraction_weights (evt) result (weight)
    class(evt_nlo_t), intent(inout) :: evt
    real(default) :: weight
    integer :: i_phs
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_subtraction_weights")
    weight = zero
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       associate (event_deps => evt%event_deps)
          do i_phs = 1, evt%event_deps%n_phs           
             call evt%process_instance%compute_sqme_real_sub (i_phs)
             if (debug2_active (D_TRANSFORMS)) then
                call msg_debug (D_TRANSFORMS, &
                   "instance%sqme_collector%sqme_real_per_phs(:, i_phs)")
                print *, pcm%collector%sqme_real_per_phs(:, i_phs)
             end if
             weight = weight + sum (pcm%collector%sqme_real_per_phs (:, i_phs))
          end do
       end associate
    end select
  end function evt_nlo_compute_subtraction_weights

  subroutine evt_nlo_compute_real (evt)
    class(evt_nlo_t), intent(inout) :: evt
    integer :: i_phs, i_flv
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_real")
    i_phs = evt%get_i_phs ()
    i_flv = evt%get_i_flv ()
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       associate (event_deps => evt%event_deps)
          call evt%process_instance%compute_sqme_real_rad (i_phs, i_flv)
          call msg_debug (D_TRANSFORMS, &
               "instance%sqme_collector%sqme_real_per_phs(i_flv, i_phs)", &
               pcm%collector%sqme_real_per_phs(i_flv, i_phs))
          evt%sqme_rad = pcm%collector%sqme_real_per_phs (i_flv, i_phs)
       end associate
    end select
  end subroutine evt_nlo_compute_real

  function evt_nlo_boost_to_cms (evt, p_lab) result (p_cms)
    type(phs_point_t), intent(in) :: p_lab
    class(evt_nlo_t), intent(in) :: evt
    type(phs_point_t) :: p_cms
    type(lorentz_transformation_t) :: lt_lab_to_cms
    integer :: i_real
    if (evt%event_deps%cm_frame) then
       lt_lab_to_cms = identity
    else
       i_real = evt%process_instance%get_associated_real ()
       lt_lab_to_cms = inverse (evt%process_instance%get_lorentz_transformation (i_real))
    end if
    p_cms = lt_lab_to_cms * p_lab
  end function evt_nlo_boost_to_cms

  function evt_nlo_boost_to_lab (evt, p_cms) result (p_lab)
    type(phs_point_t) :: p_lab
    class(evt_nlo_t), intent(in) :: evt
    type(phs_point_t), intent(in) :: p_cms
    type(lorentz_transformation_t) :: lt_cms_to_lab
    integer :: i_real
    if (evt%event_deps%cm_frame) then
       lt_cms_to_lab = identity
    else
       i_real = evt%process_instance%get_associated_real ()
       lt_cms_to_lab = evt%process_instance%get_lorentz_transformation (i_real)
    end if
    p_lab = lt_cms_to_lab * p_cms
  end function evt_nlo_boost_to_lab

  subroutine evt_nlo_setup_event_kinematics (evt, process_instance)
    class(evt_nlo_t), intent(inout) :: evt
    type(process_instance_t), intent(in) :: process_instance
    integer :: n_born, n_real, n_phs 
    integer :: i_real
    associate (event_deps => evt%event_deps)
       event_deps%cm_frame = process_instance%is_cm_frame (1)
       select type (pcm => process_instance%pcm)
       class is (pcm_instance_nlo_t)
          n_born = pcm%controller%get_n_particles ()
          n_real = pcm%controller%get_n_particles_real ()
       end select
       call event_deps%p_born_cms%init (n_born, 1)
       call event_deps%p_born_lab%init (n_born, 1)
       i_real = process_instance%get_associated_real ()
       select type (phs => process_instance%term(i_real)%k_term%phs)
       type is (phs_fks_t)
          !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
          allocate (event_deps%phs_identifiers (size (phs%phs_identifiers)))
          event_deps%phs_identifiers = phs%phs_identifiers
       end select
       n_phs = size (event_deps%phs_identifiers)
       call event_deps%p_real_cms%init (n_real, n_phs)
       call event_deps%p_real_lab%init (n_real, n_phs)
       associate (nlo_controller => process_instance%component(i_real)%nlo_controller)
          allocate (event_deps%i_evaluation_to_i_phs &
             (size (nlo_controller%real_kinematics%alr_to_i_phs)))
          event_deps%i_evaluation_to_i_phs = nlo_controller%real_kinematics%alr_to_i_phs
          if (allocated (nlo_controller%reg_data%alr_contributors)) then
             allocate (event_deps%contributors (size (nlo_controller%reg_data%alr_contributors)))
             event_deps%contributors = nlo_controller%reg_data%alr_contributors
          end if
          if (allocated (nlo_controller%reg_data%alr_to_i_contributor)) then
             allocate (event_deps%alr_to_i_con &
                (size (nlo_controller%reg_data%alr_to_i_contributor)))
             event_deps%alr_to_i_con = nlo_controller%reg_data%alr_to_i_contributor
          end if
       end associate
    end associate
  end subroutine evt_nlo_setup_event_kinematics

  subroutine evt_nlo_set_mode (evt, process_instance)
    class(evt_nlo_t), intent(inout) :: evt
    type(process_instance_t), intent(in) :: process_instance
    integer :: i_real
    select type (pcm => process_instance%pcm)
    type is (pcm_instance_nlo_t)
       associate (nlo_settings => pcm%controller%settings)
          if (nlo_settings%combined_integration) then
             evt%mode = EVT_NLO_COMBINED
          else
             i_real = evt%process_instance%get_associated_real ()
             if (i_real == evt%process%extract_active_component ()) then
                evt%mode = EVT_NLO_SEPARATE_REAL
             else
                evt%mode = EVT_NLO_SEPARATE_BORNLIKE
             end if
          end if
       end associate
    end select
  end subroutine evt_nlo_set_mode

  function evt_nlo_is_valid_event (evt) result (valid)
    logical :: valid
    class(evt_nlo_t), intent(in) :: evt
    select type (pcm => evt%process_instance%pcm)
    type is (pcm_instance_nlo_t)
       associate (nlo_cuts => pcm%controller%nlo_cuts)
          if (evt%i_evaluation == 0) then
             valid = nlo_cuts%passed_born 
          else
             valid = nlo_cuts%passed_real (evt%get_i_phs ())
          end if
       end associate
    class default
       valid = .true.
    end select
  end function evt_nlo_is_valid_event


end module evt_nlo

