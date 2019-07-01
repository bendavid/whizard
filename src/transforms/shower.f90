! WHIZARD 2.6.2 Dec 13 2017
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

module shower

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use system_defs, only: LF
  use os_interface
  use diagnostics
  use lorentz
  use pdf
  use subevents, only: PRT_BEAM_REMNANT, PRT_INCOMING, PRT_OUTGOING

  use shower_base
  use matching_base
  use powheg_matching, only: powheg_matching_t

  use sm_qcd
  use model_data
  use rng_base

  use event_transforms
  use models
  use hep_common
  use process, only: process_t
  use instances, only: process_instance_t
  use process_stacks

  implicit none
  private

  public :: evt_shower_t

  logical, parameter :: POWHEG_TESTING = .false.


  type, extends (evt_t) :: evt_shower_t
     class(shower_base_t), allocatable :: shower
     class(matching_t), allocatable :: matching
     type(model_t), pointer :: model_hadrons => null ()
     type(qcd_t), pointer :: qcd => null()
     type(pdf_data_t) :: pdf_data
     type(os_data_t) :: os_data
     logical :: is_first_event
   contains
     procedure :: write_name => evt_shower_write_name
     procedure :: write => evt_shower_write
     procedure :: connect => evt_shower_connect
     procedure :: init => evt_shower_init
     procedure :: make_rng => evt_shower_make_rng
     procedure :: prepare_new_event => evt_shower_prepare_new_event
     procedure :: first_event => evt_shower_first_event
     procedure :: generate_weighted => evt_shower_generate_weighted
     procedure :: make_particle_set => evt_shower_make_particle_set
     procedure :: contains_powheg_matching => evt_shower_contains_powheg_matching
     procedure :: disable_powheg_matching => evt_shower_disable_powheg_matching
     procedure :: enable_powheg_matching => evt_shower_enable_powheg_matching
     procedure :: final => evt_shower_final
  end type evt_shower_t


contains

  subroutine evt_shower_write_name (evt, unit)
    class(evt_shower_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event transform: shower"
  end subroutine evt_shower_write_name

  subroutine evt_shower_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_shower_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    call evt%write_name (u)
    call write_separator (u)
    call evt%base_write (u, testflag = testflag, show_set = .false.)
    if (evt%particle_set_exists)  call evt%particle_set%write &
         (u, summary = .true., compressed = .true., testflag = testflag)
    call write_separator (u)
    call evt%shower%settings%write (u)
  end subroutine evt_shower_write

  subroutine evt_shower_connect &
       (evt, process_instance, model, process_stack)
    class(evt_shower_t), intent(inout), target :: evt
    type(process_instance_t), intent(in), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    call evt%base_connect (process_instance, model, process_stack)
    call evt%make_rng (evt%process)
    if (allocated (evt%matching)) then
       call evt%matching%connect (process_instance, model, evt%shower)
    end if
  end subroutine evt_shower_connect

  subroutine evt_shower_init (evt, model_hadrons, os_data)
    class(evt_shower_t), intent(out) :: evt
    type(model_t), intent(in), target :: model_hadrons
    type(os_data_t), intent(in) :: os_data
    evt%os_data = os_data
    evt%model_hadrons => model_hadrons
    evt%is_first_event = .true.
  end subroutine evt_shower_init

  subroutine evt_shower_make_rng (evt, process)
    class(evt_shower_t), intent(inout) :: evt
    type(process_t), intent(inout) :: process
    class(rng_t), allocatable :: rng
    call process%make_rng (rng)
    call evt%shower%import_rng (rng)
    if (allocated (evt%matching)) then
       call process%make_rng (rng)
       call evt%matching%import_rng (rng)
    end if
  end subroutine evt_shower_make_rng

  subroutine evt_shower_prepare_new_event (evt, i_mci, i_term)
    class(evt_shower_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
    call evt%shower%prepare_new_event ()
  end subroutine evt_shower_prepare_new_event

  subroutine evt_shower_first_event (evt)
    class(evt_shower_t), intent(inout) :: evt
    double precision :: pdftest
    call msg_debug (D_TRANSFORMS, "evt_shower_first_event")
    associate (settings => evt%shower%settings)
       settings%hadron_collision = .false.
       !!! !!! !!! Workaround for PGF90 v16.1
       !!! if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () <= 39)) then
       if (evt%particle_set%prt(1)%flv%get_pdg_abs () <= 39 .and. &
           evt%particle_set%prt(2)%flv%get_pdg_abs () <= 39) then
          settings%hadron_collision = .false.
       !!! else if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () >= 100)) then
       else if (evt%particle_set%prt(1)%flv%get_pdg_abs () >= 100 .and. &
                evt%particle_set%prt(2)%flv%get_pdg_abs () >= 100) then
          settings%hadron_collision = .true.
       else
          call msg_fatal ("evt_shower didn't recognize beams setup")
       end if
       call msg_debug (D_TRANSFORMS, "hadron_collision", settings%hadron_collision)
       if (allocated (evt%matching)) then
          evt%matching%is_hadron_collision = settings%hadron_collision
          call evt%matching%first_event ()
       end if
       if (.not. settings%hadron_collision .and. settings%isr_active) then
          call msg_fatal ("?ps_isr_active is only intended for hadron-collisions")
       end if
       if (evt%pdf_data%type == STRF_LHAPDF5) then
          if (settings%isr_active .and. settings%hadron_collision) then
             call GetQ2max (0, pdftest)
             if (pdftest < epsilon (pdftest)) then
                call msg_bug ("ISR QCD shower enabled, but LHAPDF not " // &
                     "initialized," // LF // "     aborting simulation")
                return
             end if
          end if
       else if (evt%pdf_data%type == STRF_PDF_BUILTIN .and. &
                settings%method == PS_PYTHIA6) then
          call msg_fatal ("Builtin PDFs cannot be used for PYTHIA showers," &
               // LF // "     aborting simulation")
          return
       end if
    end associate
    evt%is_first_event = .false.
  end subroutine evt_shower_first_event

  subroutine evt_shower_generate_weighted (evt, probability)
    class(evt_shower_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    logical :: valid, vetoed
    integer :: i_term
    real(default) :: fac_scale
    call msg_debug (D_TRANSFORMS, "evt_shower_generate_weighted")
    if (signal_is_pending ())  return
    i_term = 1
    evt%particle_set = evt%previous%particle_set
    valid = .true.;  vetoed = .false.
    fac_scale = evt%process_instance%get_fac_scale (i_term)
    if (evt%is_first_event)  call evt%first_event ()
    call evt%shower%import_particle_set &
         (evt%particle_set, evt%os_data, fac_scale)
    if (allocated (evt%matching)) then
       call evt%matching%before_shower (evt%particle_set, vetoed)
       if (msg_level(D_TRANSFORMS) >= DEBUG) then
          call msg_debug (D_TRANSFORMS, "Matching before generate emissions")
          call evt%matching%write ()
       end if
    end if
    if (.not. (vetoed .or. POWHEG_TESTING)) then
       if (evt%shower%settings%method == PS_PYTHIA6 .or. &
           evt%shower%settings%hadronization_active) then
          call assure_heprup (evt%particle_set)
       end if
       call evt%shower%generate_emissions (valid)
    end if
    probability = 1
    evt%particle_set_exists = valid .and. .not. vetoed
  end subroutine evt_shower_generate_weighted

  subroutine evt_shower_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_shower_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    type(vector4_t) :: sum_vec_in, sum_vec_out, sum_vec_beamrem, &
         sum_vec_beamrem_before
    logical :: vetoed, sane
    if (evt%particle_set_exists) then
       vetoed = .false.
       sum_vec_beamrem_before = sum (evt%particle_set%prt%p, &
            mask=evt%particle_set%prt%get_status () == PRT_BEAM_REMNANT)
       call evt%shower%make_particle_set (evt%particle_set, &
            evt%model, evt%model_hadrons)
       if (allocated (evt%matching)) then
          call evt%matching%after_shower (evt%particle_set, vetoed)
       end if
       if (debug_active (D_TRANSFORMS)) then
          call msg_debug (D_TRANSFORMS, &
               "Shower: obtained particle set after shower + matching")
          call evt%particle_set%write (summary = .true., compressed = .true.)
       end if
       sum_vec_in = sum (evt%particle_set%prt%p, &
            mask=evt%particle_set%prt%get_status () == PRT_INCOMING)
       sum_vec_out = sum (evt%particle_set%prt%p, &
            mask=evt%particle_set%prt%get_status () == PRT_OUTGOING)
       sum_vec_beamrem = sum (evt%particle_set%prt%p, &
            mask=evt%particle_set%prt%get_status () == PRT_BEAM_REMNANT)
       sum_vec_beamrem = sum_vec_beamrem - sum_vec_beamrem_before
       sane = abs(sum_vec_out%p(0) - sum_vec_in%p(0)) < &
            sum_vec_in%p(0) / 10 .or. &
            abs((sum_vec_out%p(0) + sum_vec_beamrem%p(0)) - sum_vec_in%p(0)) < &
            sum_vec_in%p(0) / 10
       sane = .true.
       evt%particle_set_exists = .not. vetoed .and. sane
    end if
  end subroutine evt_shower_make_particle_set

  function evt_shower_contains_powheg_matching (evt) result (val)
     logical :: val
     class(evt_shower_t), intent(in) :: evt
     val = .false.
     if (allocated (evt%matching)) &
        val = evt%matching%get_method () == "POWHEG"
  end function evt_shower_contains_powheg_matching

  subroutine evt_shower_disable_powheg_matching (evt)
     class(evt_shower_t), intent(inout) :: evt
     select type (matching => evt%matching)
     type is (powheg_matching_t)
        matching%active = .false.
     class default
        call msg_fatal ("Trying to disable powheg but no powheg matching is allocated!")
     end select
  end subroutine evt_shower_disable_powheg_matching

  subroutine evt_shower_enable_powheg_matching (evt)
     class(evt_shower_t), intent(inout) :: evt
     select type (matching => evt%matching)
     type is (powheg_matching_t)
        matching%active = .true.
     class default
        call msg_fatal ("Trying to enable powheg but no powheg matching is allocated!")
     end select
  end subroutine evt_shower_enable_powheg_matching

  subroutine evt_shower_final (evt)
    class(evt_shower_t), intent(inout) :: evt
    call evt%base_final ()
    if (allocated (evt%matching))  call evt%matching%final ()
  end subroutine evt_shower_final


end module shower
