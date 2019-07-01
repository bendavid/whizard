! WHIZARD 2.2.4 Feb 06 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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
  use unit_tests
  use diagnostics
  use os_interface
  use sm_qcd
  use model_data
  use state_matrices
  use interactions
  use particles
  use subevents
  use process_libraries
  use prc_core
  use prc_test
  use rng_base
  use mci_base
  use mci_midpoint
  use phs_base
  use phs_single
  use processes
  use process_stacks
  
  implicit none
  private

  public :: evt_t
  public :: evt_trivial_t
  public :: event_transforms_test

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
   contains
     procedure :: final => evt_final
     procedure :: base_final => evt_final
     procedure :: base_write => evt_write
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
     procedure :: write => evt_trivial_write
     procedure :: prepare_new_event => evt_trivial_prepare_new_event
     procedure :: generate_weighted => evt_trivial_generate_weighted
     procedure :: make_particle_set => evt_trivial_make_particle_set
  end type evt_trivial_t


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
       real(default), intent(out) :: probability
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

  subroutine evt_final (object)
    class(evt_t), intent(inout) :: object
    if (allocated (object%rng))  call object%rng%final ()
    if (object%particle_set_exists) &
         call particle_set_final (object%particle_set)
  end subroutine evt_final
  
  subroutine evt_write (object, unit, verbose, testflag)
    class(evt_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%process)) then
       write (u, "(3x,A,A,A)")   "Associated process: '", &
            char (object%process%get_id ()), "'"
    end if
    if (allocated (object%rng)) then
       call object%rng%write (u, 1)
       write (u, "(3x,A,I0)")  "Number of tries = ", object%rejection_count
    end if
    if (object%particle_set_exists) then
       call write_separator (u)
       call particle_set_write (object%particle_set, u, testflag)
    end if
  end subroutine evt_write
  
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
       (evt, int_matrix, int_flows, factorization_mode, keep_correlations, r)
    class(evt_t), intent(inout) :: evt
    type(interaction_t), intent(in), target :: int_matrix, int_flows
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
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
    call particle_set_init (evt%particle_set, evt%particle_set_exists, &
         int_matrix, int_flows, factorization_mode, x, &
         keep_correlations, keep_virtual=.true.)
    evt%particle_set_exists = .true.
  end subroutine evt_factorize_interactions
  
  subroutine evt_tag_incoming (evt)
    class(evt_t), intent(inout) :: evt
    integer :: i_term, n_in
    integer, dimension(:), allocatable :: beam_index, in_index
    n_in = evt%process%get_n_in ()
    i_term = 1
    allocate (beam_index (n_in))
    call evt%process_instance%get_beam_index (i_term, beam_index)
    call particle_set_reset_status (evt%particle_set, &
         beam_index, PRT_BEAM)
    allocate (in_index (n_in))
    call evt%process_instance%get_in_index (i_term, in_index)
    call particle_set_reset_status (evt%particle_set, &
         in_index, PRT_INCOMING)
  end subroutine evt_tag_incoming

  subroutine evt_trivial_write (object, unit, verbose, testflag)
    class(evt_trivial_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Event transform: trivial (hard process)"
    call write_separator (u)
    call object%base_write (u, testflag = testflag)
  end subroutine evt_trivial_write
  
  subroutine evt_trivial_prepare_new_event (evt, i_mci, i_term)
    class(evt_trivial_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
  end subroutine evt_trivial_prepare_new_event
  
  subroutine evt_trivial_generate_weighted (evt, probability)
    class(evt_trivial_t), intent(inout) :: evt
    real(default), intent(out) :: probability
    probability = 1
  end subroutine evt_trivial_generate_weighted
    
  subroutine evt_trivial_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_trivial_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    integer :: i_term
    type(interaction_t), pointer :: int_matrix, int_flows
    if (evt%process_instance%is_complete_event ()) then
       call evt%process_instance%select_i_term (i_term)
       int_matrix => evt%process_instance%get_matrix_int_ptr (i_term)
       int_flows  => evt%process_instance%get_flows_int_ptr (i_term)
       call evt%factorize_interactions (int_matrix, int_flows, &
            factorization_mode, keep_correlations, r)
       call evt%tag_incoming ()
    else
       call msg_bug ("Event factorization: event is incomplete")
    end if
  end subroutine evt_trivial_make_particle_set
    

  subroutine event_transforms_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (event_transforms_1, "event_transforms_1", &
         "trivial event transform", &
         u, results)
  end subroutine event_transforms_test
  
  subroutine event_transforms_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(model_data_t), pointer :: model
    type(process_library_t), target :: lib
    type(string_t) :: libname, procname1, run_id
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    class(evt_t), allocatable :: evt
    integer :: factorization_mode
    logical :: keep_correlations

    write (u, "(A)")  "* Test output: event_transforms_1"
    write (u, "(A)")  "*   Purpose: handle trivial transform"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment and parent process"
    write (u, "(A)")

    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)

    libname = "event_transforms_1_lib"
    procname1 = "event_transforms_1_p"
    run_id = "event_transforms_1"

    call prc_test_create_library (libname, lib, &
         scattering = .true., procname1 = procname1)
    call reset_interaction_counter ()

    allocate (model)
    call model%init_test ()

    allocate (process)
    call process%init &
         (procname1, run_id, lib, os_data, qcd, rng_factory, model)
    
    allocate (test_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()
    call process%setup_terms ()

    allocate (process_instance)
    call process_instance%init (process)
    call process%integrate (process_instance, 1, n_it=1, n_calls=100)
    call process%final_integration (1)
    call process_instance%final ()
    deallocate (process_instance)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)

    write (u, "(A)")  "* Initialize trivial event transform"
    write (u, "(A)")

    allocate (evt_trivial_t :: evt)
    model => process%get_model_ptr ()
    call evt%connect (process_instance, model)
    
    write (u, "(A)")  "* Generate event and subsequent transform"
    write (u, "(A)")
    
    call process%generate_unweighted_event (process_instance, 1)
    call process_instance%evaluate_event_data ()
    
    call evt%prepare_new_event (1, 1)
    call evt%generate_unweighted ()

    select type (evt)
    type is (evt_trivial_t)
       call write_separator (u, 2)
       call evt%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Obtain particle set"
    write (u, "(A)")
    
    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    
    call evt%make_particle_set (factorization_mode, keep_correlations)

    select type (evt)
    type is (evt_trivial_t)
       call write_separator (u, 2)
       call evt%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: event_transforms_1"
    
  end subroutine event_transforms_1
  

end module event_transforms

