! WHIZARD 2.5.0 May 06 2017
!
! Copyright (C) 1999-2017 by
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

module event_transforms

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use diagnostics
  use model_data
  use interactions
  use particles
  use subevents
  use rng_base
  use quantum_numbers, only: quantum_numbers_t
  use process, only: process_t
  use instances, only: process_instance_t
  use process_stacks

  implicit none
  private

  public :: evt_t
  public :: make_factorized_particle_set
  public :: evt_trivial_t

  type, abstract :: evt_t
     type(process_t), pointer :: process => null ()
     type(process_instance_t), pointer :: process_instance => null ()
     class(model_data_t), pointer :: model => null ()
     class(rng_t), allocatable :: rng
     integer :: rejection_count = 0
     logical :: particle_set_exists = .false.
     type(particle_set_t) :: particle_set
     class(evt_t), pointer :: previous => null ()
     class(evt_t), pointer :: next => null ()
     real(default) :: weight = 0._default
     logical :: only_weighted_events = .false.
   contains
     procedure :: final => evt_final
     procedure :: base_final => evt_final
     procedure (evt_write_name), deferred :: write_name
     procedure (evt_write), deferred :: write
     procedure :: base_write => evt_base_write
     procedure :: connect => evt_connect
     procedure :: base_connect => evt_connect
     procedure :: reset => evt_reset
     procedure :: base_reset => evt_reset
     procedure (evt_prepare_new_event), deferred :: prepare_new_event
     procedure (evt_generate_weighted), deferred :: generate_weighted
     procedure :: generate_unweighted => evt_generate_unweighted
     procedure :: base_generate_unweighted => evt_generate_unweighted
     procedure (evt_make_particle_set), deferred :: make_particle_set
     procedure :: set_particle_set => evt_set_particle_set
     procedure :: factorize_interactions => evt_factorize_interactions
     procedure :: tag_incoming => evt_tag_incoming
  end type evt_t

  type, extends (evt_t) :: evt_trivial_t
   contains
     procedure :: write_name => evt_trivial_write_name
     procedure :: write => evt_trivial_write
     procedure :: prepare_new_event => evt_trivial_prepare_new_event
     procedure :: generate_weighted => evt_trivial_generate_weighted
     procedure :: make_particle_set => evt_trivial_make_particle_set
  end type evt_trivial_t


  abstract interface
     subroutine evt_write_name (evt, unit)
       import
       class(evt_t), intent(in) :: evt
       integer, intent(in), optional :: unit
     end subroutine evt_write_name
  end interface

  abstract interface
     subroutine evt_write (evt, unit, verbose, more_verbose, testflag)
       import
       class(evt_t), intent(in) :: evt
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: verbose, more_verbose, testflag
     end subroutine evt_write
  end interface

  interface
     subroutine evt_prepare_new_event (evt, i_mci, i_term)
       import
       class(evt_t), intent(inout) :: evt
       integer, intent(in) :: i_mci, i_term
     end subroutine evt_prepare_new_event
  end interface

  abstract interface
     subroutine evt_generate_weighted (evt, probability)
       import
       class(evt_t), intent(inout) :: evt
       real(default), intent(inout) :: probability
     end subroutine evt_generate_weighted
  end interface

  interface
     subroutine evt_make_particle_set &
          (evt, factorization_mode, keep_correlations, r)
       import
       class(evt_t), intent(inout) :: evt
       integer, intent(in) :: factorization_mode
       logical, intent(in) :: keep_correlations
       real(default), dimension(:), intent(in), optional :: r
     end subroutine evt_make_particle_set
  end interface


contains

  subroutine evt_final (evt)
    class(evt_t), intent(inout) :: evt
    if (allocated (evt%rng))  call evt%rng%final ()
    if (evt%particle_set_exists) &
         call evt%particle_set%final ()
  end subroutine evt_final

  subroutine evt_base_write (evt, unit, testflag, show_set)
    class(evt_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag, show_set
    integer :: u
    logical :: show
    u = given_output_unit (unit)
    show = .true.;  if (present (show_set))  show = show_set
    if (associated (evt%process)) then
       write (u, "(3x,A,A,A)")   "Associated process: '", &
            char (evt%process%get_id ()), "'"
    end if
    if (allocated (evt%rng)) then
       call evt%rng%write (u, 1)
       write (u, "(3x,A,I0)")  "Number of tries = ", evt%rejection_count
    end if
    if (show) then
       if (evt%particle_set_exists) then
          call write_separator (u)
          call evt%particle_set%write (u, testflag = testflag)
       end if
    end if
  end subroutine evt_base_write

  subroutine evt_connect (evt, process_instance, model, process_stack)
    class(evt_t), intent(inout), target :: evt
    type(process_instance_t), intent(in), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    evt%process => process_instance%process
    evt%process_instance => process_instance
    evt%model => model
    call evt%process%make_rng (evt%rng)
  end subroutine evt_connect

  subroutine evt_reset (evt)
    class(evt_t), intent(inout) :: evt
    evt%rejection_count = 0
    evt%particle_set_exists = .false.
  end subroutine evt_reset

  subroutine evt_generate_unweighted (evt)
    class(evt_t), intent(inout) :: evt
    real(default) :: p, x
    evt%rejection_count = 0
    REJECTION: do
       evt%rejection_count = evt%rejection_count + 1
       call evt%generate_weighted (p)
       if (signal_is_pending ())  return
       call evt%rng%generate (x)
       if (x < p)  exit REJECTION
    end do REJECTION
  end subroutine evt_generate_unweighted

  subroutine evt_set_particle_set (evt, particle_set, i_mci, i_term)
    class(evt_t), intent(inout) :: evt
    type(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: i_term, i_mci
    call evt%prepare_new_event (i_mci, i_term)
    evt%particle_set = particle_set
    evt%particle_set_exists = .true.
  end subroutine evt_set_particle_set

  subroutine evt_factorize_interactions &
       (evt, int_matrix, int_flows, factorization_mode, &
       keep_correlations, r, qn_select)
    class(evt_t), intent(inout) :: evt
    type(interaction_t), intent(in), target :: int_matrix, int_flows
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_select
    real(default), dimension(2) :: x
    if (present (r)) then
       if (size (r) == 2) then
          x = r
       else
          call msg_bug ("event factorization: size of r array must be 2")
       end if
    else
       call evt%rng%generate (x)
    end if
    call evt%particle_set%init (evt%particle_set_exists, &
         int_matrix, int_flows, factorization_mode, x, &
         keep_correlations, keep_virtual=.true., qn_select = qn_select)
    evt%particle_set_exists = .true.
  end subroutine evt_factorize_interactions

  subroutine make_factorized_particle_set (evt, factorization_mode, &
         keep_correlations, r, ii_term, qn_select)
    class(evt_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    integer, intent(in), optional :: ii_term
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_select
    integer :: i_term
    type(interaction_t), pointer :: int_matrix, int_flows
    if (evt%process_instance%is_complete_event ()) then
       if (present (ii_term)) then
          i_term = ii_term
       else
          i_term = evt%process_instance%select_i_term ()
       end if
       int_matrix => evt%process_instance%get_matrix_int_ptr (i_term)
       int_flows  => evt%process_instance%get_flows_int_ptr (i_term)
       call evt%factorize_interactions (int_matrix, int_flows, &
            factorization_mode, keep_correlations, r, qn_select)
       call evt%tag_incoming ()
    else
       call msg_bug ("Event factorization: event is incomplete")
    end if
  end subroutine make_factorized_particle_set

  subroutine evt_tag_incoming (evt)
    class(evt_t), intent(inout) :: evt
    integer :: i_term, n_in
    integer, dimension(:), allocatable :: beam_index, in_index
    n_in = evt%process%get_n_in ()
    i_term = 1
    allocate (beam_index (n_in))
    call evt%process_instance%get_beam_index (i_term, beam_index)
    call evt%particle_set%reset_status (beam_index, PRT_BEAM)
    allocate (in_index (n_in))
    call evt%process_instance%get_in_index (i_term, in_index)
    call evt%particle_set%reset_status (in_index, PRT_INCOMING)
  end subroutine evt_tag_incoming

  subroutine evt_trivial_write_name (evt, unit)
    class(evt_trivial_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event transform: trivial (hard process)"
  end subroutine evt_trivial_write_name

  subroutine evt_trivial_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_trivial_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    call evt%write_name (u)
    call write_separator (u)
    call evt%base_write (u, testflag = testflag)
  end subroutine evt_trivial_write

  subroutine evt_trivial_prepare_new_event (evt, i_mci, i_term)
    class(evt_trivial_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
  end subroutine evt_trivial_prepare_new_event

  subroutine evt_trivial_generate_weighted (evt, probability)
    class(evt_trivial_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    probability = 1
  end subroutine evt_trivial_generate_weighted

  subroutine evt_trivial_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_trivial_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    call make_factorized_particle_set (evt, factorization_mode, &
         keep_correlations, r)
    evt%particle_set_exists = .true.
  end subroutine evt_trivial_make_particle_set


end module event_transforms

