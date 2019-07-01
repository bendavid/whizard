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

module evt_nlo

  use kinds, only: default
  use constants
  use lorentz
  use diagnostics
  use sm_qcd
  use model_data
  use particles
  use processes
  use process_stacks
  use event_transforms

  use nlo_data, only: sqme_collector_t
  use phs_fks

  implicit none
  private

  public :: evt_nlo_t

  type, extends (evt_t) :: evt_nlo_t
    type(phs_fks_generator_t) :: phs_fks_generator
    real(default) :: sqme_rad
    integer :: i_evaluation
    integer, dimension(:), allocatable :: emitters
    type(particle_set_t), dimension(:), allocatable :: particle_set_radiated
    type(qcd_t), pointer :: qcd => null ()
  contains
    procedure :: write => evt_nlo_write
    procedure :: connect => evt_nlo_connect
    procedure :: prepare_new_event => evt_nlo_prepare_new_event
    procedure :: generate_weighted => evt_nlo_generate_weighted
    procedure :: make_particle_set => evt_nlo_make_particle_set
    procedure :: build_radiated_particle_set => evt_nlo_build_radiated_particle_set
    procedure :: compute_subtraction_weights => evt_nlo_compute_subtraction_weights
    procedure :: compute_real => evt_nlo_compute_real
  end type evt_nlo_t


contains

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
    select type (pcm => process_instance%pcm)
    class is (pcm_instance_nlo_t)
       call evt%base_connect (process_instance, model, process_stack)
       associate (generator => evt%phs_fks_generator)
         sqrts = process_instance%get_sqrts ()
         call pcm%controller%setup_generator (generator, sqrts)
       end associate
    end select
  end subroutine evt_nlo_connect

  subroutine evt_nlo_prepare_new_event (evt, i_mci, i_term)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
  end subroutine evt_nlo_prepare_new_event

  subroutine evt_nlo_generate_weighted (evt, probability)
    class(evt_nlo_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    real(default) :: weight
    integer :: emitter
    call msg_debug (D_TRANSFORMS, "evt_nlo_generate_weighted")
    call msg_debug (D_TRANSFORMS, "probability (before)", probability)
    call msg_debug (D_TRANSFORMS, "evt%i_evaluation", evt%i_evaluation)
    evt%particle_set = evt%previous%particle_set
    if (evt%i_evaluation == 0) then
       weight = evt%compute_subtraction_weights ()
       probability = probability + weight
    else
       emitter = evt%emitters (evt%i_evaluation)
       call evt%compute_real (emitter)
       probability = evt%sqme_rad
    end if
    probability = probability * (size (evt%emitters) + 1)
    call msg_debug (D_TRANSFORMS, "probability (after)", probability)
    evt%particle_set_exists = .true.
  end subroutine evt_nlo_generate_weighted

  subroutine evt_nlo_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
  end subroutine evt_nlo_make_particle_set

  subroutine evt_nlo_build_radiated_particle_set (evt, i_event)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: i_event
    integer :: emitter
    type(vector4_t), dimension(:), allocatable :: p_new
    integer, dimension(:), allocatable :: flv_radiated
    real(default) :: r_col
    call msg_debug (D_TRANSFORMS, "evt_nlo_build_radiated_particle_set")
    call msg_debug (D_TRANSFORMS, "evt%i_evaluation", evt%i_evaluation)
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       evt%particle_set_radiated(i_event) = evt%particle_set
       if (evt%i_evaluation /= 0) then
          allocate (flv_radiated (size (pcm%controller%get_flv_state_real (1))))
          flv_radiated = pcm%controller%get_flv_state_real (1)
          call evt%rng%generate (r_col)
          call msg_debug2 (D_TRANSFORMS, "r_col", r_col)
          if (debug2_active (D_TRANSFORMS))  print *, 'flv_radiated =    ', flv_radiated
          emitter = evt%emitters (evt%i_evaluation)
          call msg_debug (D_TRANSFORMS, "emitter", emitter)
          allocate (p_new (size (pcm%controller%get_momenta (born_phsp = .false.))))
          p_new = pcm%controller%get_momenta (born_phsp = .false.)
          call evt%particle_set_radiated(i_event)%build_radiation (p_new, emitter, flv_radiated, &
               evt%process_instance%process%get_model_ptr (), r_col)
       end if
       evt%i_evaluation = evt%i_evaluation + 1
    end select
  end subroutine evt_nlo_build_radiated_particle_set

  function evt_nlo_compute_subtraction_weights (evt) result (weight)
    class(evt_nlo_t), intent(inout) :: evt
    real(default) :: weight
    type(vector4_t), dimension(:), allocatable :: p_born, p_real
    integer, dimension(:), allocatable :: emitters
    real(default), dimension(3) :: x_rad
    integer :: i, emitter
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_subtraction_weights")
    weight = zero
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       allocate (emitters (size (pcm%controller%get_emitter_list ())))       
       allocate (p_born (size (evt%particle_set%get_momenta ())))
       emitters = pcm%controller%get_emitter_list ()
       x_rad = pcm%controller%real_kinematics%x_rad
       p_born = evt%particle_set%get_momenta ()
       call evt%phs_fks_generator%set_beam_energy (p_born(1)%p(0))
       call evt%phs_fks_generator%generate_radiation_variables (x_rad, p_born)
       do i = 1, size (emitters)
          emitter = emitters(i)
          if (emitter <= 2) then
             call msg_fatal ("NLO Events only for lepton collisions so far")
          else
             allocate (p_real (size (evt%phs_fks_generator%generate_fsr_from_x &
                  (x_rad, emitter, p_born))))
             p_real = evt%phs_fks_generator%generate_fsr_from_x &
                  (x_rad, emitter, p_born)
          end if
          call pcm%controller%set_momenta (p_born, p_real)
          call pcm%controller%set_momenta (p_born, p_real, cms=.true.)
          call evt%process_instance%compute_sqme_real_sub &
               (emitter, p_born, p_real)
          call msg_debug (D_TRANSFORMS, &
               "instance%sqme_collector%sqme_real_per_emitter(1,emitter)", &
               pcm%collector%sqme_real_per_emitter(1,emitter))
          weight = weight + pcm%collector%sqme_real_per_emitter (1,emitter)
          deallocate (p_real)
       end do
    end select
  end function evt_nlo_compute_subtraction_weights

  subroutine evt_nlo_compute_real (evt, emitter)
    class(evt_nlo_t), intent(inout) :: evt
    integer, intent(in) :: emitter
    type(vector4_t), dimension(:), allocatable :: p_born, p_real
    real(default), dimension(3) :: x_rad
    call msg_debug (D_TRANSFORMS, "evt_nlo_compute_real")
    allocate (p_born (size (evt%particle_set%get_momenta ())))
    p_born = evt%particle_set%get_momenta ()
    select type (pcm => evt%process_instance%pcm)
    class is (pcm_instance_nlo_t)
       x_rad = pcm%controller%real_kinematics%x_rad
       call evt%phs_fks_generator%generate_radiation_variables (x_rad, p_born)
       if (emitter <= 2) then
          call msg_fatal ("NLO Events only for lepton collisions so far")
       else
          if (allocated (p_real))  deallocate (p_real)
          allocate (p_real (size (evt%phs_fks_generator%generate_fsr_from_x &
              (x_rad, emitter, p_born))))
          p_real = evt%phs_fks_generator%generate_fsr_from_x &
              (x_rad, emitter, p_born)
       end if
       call pcm%controller%set_momenta (p_born, p_real)
       call pcm%controller%set_momenta (p_born, p_real, cms=.true.)
       call evt%process_instance%compute_sqme_real_rad (emitter, p_born, p_real)
       call msg_debug (D_TRANSFORMS, &
            "instance%sqme_collector%sqme_real_per_emitter(1,emitter)", &
            pcm%collector%sqme_real_per_emitter(1,emitter))
       evt%sqme_rad = pcm%collector%sqme_real_per_emitter (1, emitter)
    end select
  end subroutine evt_nlo_compute_real


end module evt_nlo

