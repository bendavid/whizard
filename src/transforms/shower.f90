! WHIZARD 2.2.6 May 02 2015
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

module shower

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: pi, twopi, zero
  use format_utils, only: write_separator
  use string_utils
  use unit_tests
  use system_defs, only: LF
  use os_interface
  use diagnostics
  use lorentz
  use system_dependencies, only: LHAPDF5_AVAILABLE, LHAPDF6_AVAILABLE
  use lhapdf !NODEP!
  use pdf_builtin !NODEP!
  
  use shower_base
  use shower_partons
  use shower_core
  use shower_pythia6
  use mlm_matching
  use ckkw_base
  use ckkw_matching
  use powheg
  
  use sm_qcd
  use particles
  use state_matrices, only: FM_IGNORE_HELICITY
  use model_data
  use variables
  use beam_structures
  use process_libraries
  use rng_base
  use mci_base
  use phs_base

  use event_transforms
  use models
  use hep_common
  use processes
  use process_stacks
  use rng_tao
  use mci_midpoint
  use phs_single
  use prc_core
  use prc_omega

  implicit none
  private

  public :: evt_shower_t
  public :: ckkw_fake_pseudo_shower_weights
  public :: shower_test

  logical, parameter :: DEBUG_EVT_SHOWER = .false.
  logical, parameter :: POWHEG_TESTING = .false.
  logical, parameter :: TEST_SUDAKOV = .false.
  logical, parameter :: TO_FILE = .false.


  type, extends (evt_t) :: evt_shower_t
     type(shower_settings_t) :: settings
     class(shower_base_t), allocatable :: shower
     type(model_t), pointer :: model_hadrons => null ()
     type(powheg_t) :: powheg
     class(matching_settings_t), allocatable :: matching_settings
     class(matching_data_t), allocatable :: data
     type(qcd_t), pointer :: qcd => null()
     type(pdf_data_t) :: pdf_data     
     type(os_data_t) :: os_data     
   contains
     procedure :: write => evt_shower_write
     procedure :: connect => evt_shower_connect
     procedure :: init => evt_shower_init
     procedure :: make_rng => evt_shower_make_rng
     procedure :: setup_pdf => evt_shower_setup_pdf
     procedure :: prepare_new_event => evt_shower_prepare_new_event
     procedure :: generate_weighted => evt_shower_generate_weighted
     procedure :: make_particle_set => evt_shower_make_particle_set
     procedure :: final => evt_shower_final
  end type evt_shower_t
  

contains

  subroutine evt_shower_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_shower_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Event transform: shower"
    call write_separator (u)
    call evt%base_write (u, testflag = testflag, show_set = .false.)
    if (evt%particle_set_exists)  call evt%particle_set%write &
         (u, summary = .true., compressed = .true., testflag = testflag)
    call write_separator (u)
    call evt%settings%write (u)
  end subroutine evt_shower_write
    
  subroutine evt_shower_connect &
       (evt, process_instance, model, process_stack)
    class(evt_shower_t), intent(inout), target :: evt
    type(process_instance_t), intent(in), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    call evt%base_connect (process_instance, model, process_stack)
    call evt%make_rng (evt%process)
    if (evt%settings%powheg_matching) then
       call evt%powheg%connect (process_instance)
       call evt%powheg%setup_grids ()
    end if
  end subroutine evt_shower_connect

  subroutine evt_shower_init &
       (evt, settings, model_hadrons, os_data, powheg)
    class(evt_shower_t), intent(out) :: evt
    type(shower_settings_t), intent(in) :: settings
    type(model_t), intent(in), target :: model_hadrons
    type(os_data_t), intent(in) :: os_data    
    type(powheg_t), intent(in), optional :: powheg
    evt%settings = settings
    evt%os_data = os_data
    evt%model_hadrons => model_hadrons
    if (present (powheg))  evt%powheg = powheg
  end subroutine evt_shower_init
  
  subroutine evt_shower_make_rng (evt, process)
    class(evt_shower_t), intent(inout) :: evt
    type(process_t), intent(inout) :: process
    class(rng_t), allocatable :: rng
    call process%make_rng (rng)
    call evt%shower%import_rng (rng)
    if (evt%settings%powheg_matching) then
       call process%make_rng (rng)
       call evt%powheg%import_rng (rng)
    end if
  end subroutine evt_shower_make_rng

  subroutine evt_shower_setup_pdf (evt, process, beam_structure, lhapdf_member)
    class(evt_shower_t), intent(inout) :: evt
    type(process_t), intent(in) :: process
    type(beam_structure_t), intent(in) :: beam_structure
    integer, intent(in) :: lhapdf_member
    real(default) :: xmin, xmax, q2min, q2max
    if (beam_structure%contains ("lhapdf")) then
       if (LHAPDF6_AVAILABLE) then
          evt%pdf_data%type = STRF_LHAPDF6
       else if (LHAPDF5_AVAILABLE) then
          evt%pdf_data%type = STRF_LHAPDF5
       end if
       evt%pdf_data%set = process%get_pdf_set ()
       write (msg_buffer, "(A,I0)")  "Shower: interfacing LHAPDF set #", &
            evt%pdf_data%set
       call msg_message ()
    else if (beam_structure%contains ("pdf_builtin")) then
       evt%pdf_data%type = STRF_PDF_BUILTIN
       evt%pdf_data%set = process%get_pdf_set ()
       write (msg_buffer, "(A,I0)")  "Shower: interfacing PDF builtin set #", &
            evt%pdf_data%set
       call msg_message ()
    end if
    select case (evt%pdf_data%type)
    case (STRF_LHAPDF6)
       evt%pdf_data%xmin = evt%pdf_data%pdf%getxmin ()
       evt%pdf_data%xmax = evt%pdf_data%pdf%getxmax ()
       evt%pdf_data%qmin = sqrt(evt%pdf_data%pdf%getq2min ())
       evt%pdf_data%qmax = sqrt(evt%pdf_data%pdf%getq2max ())
    case (STRF_LHAPDF5)
       if (LHAPDF5_AVAILABLE) then
          call GetXminM (1, lhapdf_member, xmin)
          call GetXmaxM (1, lhapdf_member, xmax)
          call GetQ2minM (1, lhapdf_member, q2min)
          call GetQ2maxM (1, lhapdf_member, q2max)
          evt%pdf_data%xmin = xmin
          evt%pdf_data%xmax = xmax
          evt%pdf_data%qmin = sqrt(q2min)
          evt%pdf_data%qmax = sqrt(q2max)
       end if
    end select
  end subroutine evt_shower_setup_pdf
    
  subroutine evt_shower_prepare_new_event (evt, i_mci, i_term)
    class(evt_shower_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
    if (.not. evt%settings%active .or. signal_is_pending ()) then
       return
    end if
  end subroutine evt_shower_prepare_new_event

  subroutine evt_shower_generate_weighted (evt, probability)
    class(evt_shower_t), intent(inout) :: evt
    real(default), intent(out) :: probability
    logical :: valid, vetoed
    logical, save :: matching_disabled = .false.    
    integer :: i
    type(particle_t), dimension(1:2) :: prt_in    
    real(kind=double) :: pdftest    
    valid = .true.
    vetoed = .false.
    if (evt%previous%particle_set_exists) then
       evt%particle_set = evt%previous%particle_set
       if (evt%settings%powheg_matching) then
          call evt%powheg%update (evt%particle_set)
          if (TEST_SUDAKOV) then
             call evt%powheg%test_sudakov ()
             stop
          end if
          call evt%powheg%generate_emission (particle_set = evt%particle_set)
          if (DEBUG_EVT_SHOWER) call evt%powheg%write ()
       end if       
       if (evt%settings%method == PS_PYTHIA6 .or. &
           evt%settings%hadronization_active) then
          call assure_heprup (evt%particle_set)
       end if
       if (evt%settings%ckkw_matching) then
          select type (data => evt%data)
          type is (ckkw_matching_data_t)
             call ckkw_pseudo_shower_weights_init (data%ckkw_weights)
             call ckkw_fake_pseudo_shower_weights (evt%matching_settings, &
                  data%ckkw_weights, evt%particle_set)
          end select
       end if
       if (evt%settings%fsr_active .or. &
            evt%settings%isr_active .or. &
            evt%settings%hadronization_active .or. &
            evt%settings%mlm_matching .and. &
            .not. POWHEG_TESTING) then
          do i = 1, 2
             prt_in(i) = evt%particle_set%get_particle (i)
          end do
          ! ensure that lhapdf is initialized
          if (evt%pdf_data%type .eq. STRF_LHAPDF5) then
             if (evt%settings%isr_active .and. &
                  (all (abs (prt_in%get_pdg ()) >= 1000))) then
                call GetQ2max (0, pdftest)
                if (pdftest < epsilon(pdftest)) then
                   call msg_fatal ("ISR QCD shower enabled, but LHAPDF not" // &
                        "initialized," // LF // "     aborting simulation")
                   return
                end if
             end if
          else if (evt%pdf_data%type == STRF_PDF_BUILTIN .and. &
             evt%settings%method == PS_PYTHIA6) then
             call msg_fatal ("Builtin PDFs cannot be used for PYTHIA showers," &
                  // LF // "     aborting simulation")
             return
          end if
          if (.not. matching_disabled .and. allocated (evt%data)) then
             if (DEBUG_EVT_SHOWER)  &
                  print *, "Shower: Set up beam type for mlm_matching"
             if (all (abs (prt_in%get_pdg ()) <= 18)) then
                evt%data%is_hadron_collision = .false.
             else if (all (abs (prt_in%get_pdg ()) >= 1000)) then
                evt%data%is_hadron_collision = .true.
             else 
                call msg_error (" Matching didn't recognize beams setup," // &
                     LF // "     disabling matching")
                matching_disabled = .true.
                return
             end if
          end if              
          call evt%shower%generate_emissions (evt%particle_set, &
               evt%model, evt%model_hadrons, evt%os_data, &
               evt%matching_settings, evt%data, valid, vetoed)
          if (evt%settings%mlm_matching .and. &
               (.not. matching_disabled)) then
             !!! MLM stage 2 -> PS jets and momenta
             select type (data => evt%data)
             type is (mlm_matching_data_t)
                select type (ms => evt%matching_settings)
                type is (mlm_matching_settings_t)
                   call matching_transfer_PS (data, evt%particle_set, ms)
                   !!! MLM stage 3 -> reconstruct and possibly reject
                   call mlm_matching_apply (data, ms, vetoed)
                   call mlm_matching_data_final (data)
                end select
             end select
          end if
          if (DEBUG_EVT_SHOWER)  print *, "SHOWER+MATCHING finished"          
       end if          
       if (DEBUG_EVT_SHOWER) then
          print *, "Shower: obtained particle set after SHOWER"
          call evt%particle_set%write (summary=.true., &
                                                compressed=.true.)
       end if
       probability = 1
       if (valid .and. .not. vetoed) then
          evt%particle_set_exists = .true.
       else
          evt%particle_set_exists = .false.
       end if
    else
       call msg_bug ("Shower: input particle set does not exist")
    end if
  end subroutine evt_shower_generate_weighted

  subroutine evt_shower_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_shower_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
  end subroutine evt_shower_make_particle_set

  subroutine evt_shower_final (object)
    class(evt_shower_t), intent(inout) :: object
    if (object%settings%powheg_matching) &
         call object%powheg%write_statistics ()
  end subroutine evt_shower_final

  subroutine ckkw_fake_pseudo_shower_weights &
       (settings, &
        ckkw_pseudo_shower_weights, particle_set)
    class(matching_settings_t), intent(inout) :: settings
    type(ckkw_pseudo_shower_weights_t), intent(inout) :: &
         ckkw_pseudo_shower_weights
    type(particle_set_t), intent(in) :: particle_set
    type(particle_t) :: prt
    integer :: i, j
    integer :: n
    type(vector4_t) :: momentum

    select type (settings)
    type is (ckkw_matching_settings_t)    
       settings%alphaS = 1.0_default
       settings%Qmin = 1.0_default
       settings%n_max_jets = 3
    class default
       call msg_fatal ("CKKW matching called with wrong data.")
    end select
       
    n = 2**particle_set%get_n_tot()
    if (allocated (ckkw_pseudo_shower_weights%weights)) then 
       deallocate (ckkw_pseudo_shower_weights%weights)
    end if
    allocate (ckkw_pseudo_shower_weights%weights (1:n))
    do i = 1, n
       momentum = vector4_null
       do j = 1, particle_set%get_n_tot ()
          if (btest (i,j-1)) then
             prt = particle_set%get_particle (j) 
             momentum = momentum + prt%get_momentum ()
          end if
       end do
       if (momentum**1 > 0.0) then
          ckkw_pseudo_shower_weights%weights(i) = 1.0 / (momentum**2)
       end if
    end do
    ! equally distribute the weights by type
    if (allocated (ckkw_pseudo_shower_weights%weights_by_type)) then
       deallocate (ckkw_pseudo_shower_weights%weights_by_type)
    end if
    allocate (ckkw_pseudo_shower_weights%weights_by_type (1:n, 0:4))
    do i = 1, n
       do j = 0, 4
          ckkw_pseudo_shower_weights%weights_by_type(i,j) = &
               0.2 * ckkw_pseudo_shower_weights%weights(i)
       end do
    end do
  end subroutine ckkw_fake_pseudo_shower_weights
  

  subroutine shower_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (shower_1, "shower_1", &
         "disabled shower", &
         u, results)
    call test (shower_2, "shower_2", &
         "final-state shower", &
         u, results)
  end subroutine shower_test
  
  subroutine setup_testbed &
       (prefix, os_data, lib, model_list, process, process_instance)
    type(string_t), intent(in) :: prefix
    type(os_data_t), intent(out) :: os_data
    type(process_library_t), intent(out), target :: lib
    type(model_list_t), intent(out) :: model_list
    class(model_data_t), pointer :: model
    type(model_t), pointer :: model_tmp
    type(process_t), target, intent(out) :: process
    type(process_instance_t), target, intent(out) :: process_instance
    type(var_list_t), pointer :: model_vars
    type(string_t) :: model_name, libname, procname, run_id
    type(process_def_entry_t), pointer :: entry
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts

    model_name = "SM"
    libname = prefix // "_lib"
    procname = prefix // "p"
    run_id = "1"
    
    call os_data_init (os_data)
    allocate (rng_tao_factory_t :: rng_factory)
    allocate (model_tmp)
    call model_list%read_model (model_name, model_name // ".mdl", &
         os_data, model_tmp)
    model_vars => model_tmp%get_var_list_ptr ()
    call var_list_set_real (model_vars, var_str ("me"), 0._default, &
         is_known = .true.)
    model => model_tmp

    call lib%init (libname)

    allocate (prt_in (2), source = [var_str ("e-"), var_str ("e+")])
    allocate (prt_out (2), source = [var_str ("d"), var_str ("dbar")])

    allocate (entry)
    call entry%init (procname, model, n_in = 2, n_components = 1)
    call omega_make_process_component (entry, 1, &
         model_name, prt_in, prt_out, &
         report_progress=.true.)
    call lib%append (entry)

    call lib%configure (os_data)
    call lib%write_makefile (os_data, force = .true.)
    call lib%clean (os_data, distclean = .false.)
    call lib%write_driver (force = .true.)
    call lib%load (os_data)
    
    call process%init (procname, run_id, lib, os_data, &
         qcd, rng_factory, model)
    
    allocate (prc_omega_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)

    model => process%get_model_ptr ()

    select type (core_template)
    type is (prc_omega_t)
       call core_template%set_parameters (model = model)
    end select
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()
    call process%setup_terms ()
    
    call process_instance%init (process)
    call process%integrate (process_instance, 1, 1, 1000)
    call process%final_integration (1)

    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)
    call process%generate_weighted_event (process_instance, 1)
    call process_instance%evaluate_event_data ()

  end subroutine setup_testbed

  subroutine shower_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t), target :: lib
    type(model_list_t) :: model_list
    class(model_data_t), pointer :: model
    type(model_t), pointer :: model_hadrons
    type(process_t), target :: process
    type(process_instance_t), target :: process_instance
    type(pdf_data_t) :: pdf_data
    integer :: factorization_mode
    logical :: keep_correlations
    class(evt_t), allocatable, target :: evt_trivial
    class(evt_t), allocatable, target :: evt_shower
    type(shower_settings_t) :: settings

    write (u, "(A)")  "* Test output: shower_1"
    write (u, "(A)")  "*   Purpose: Two-jet event with disabled shower"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
         os_data, model_hadrons)
    call setup_testbed (var_str ("shower_1"), &
         os_data, lib, model_list, process, process_instance)

    write (u, "(A)")  "* Set up trivial transform"
    write (u, "(A)")
    
    allocate (evt_trivial_t :: evt_trivial)
    model => process%get_model_ptr ()
    call evt_trivial%connect (process_instance, model)
    call evt_trivial%prepare_new_event (1, 1)
    call evt_trivial%generate_unweighted ()

    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    call evt_trivial%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_trivial)
    type is (evt_trivial_t)
       call evt_trivial%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (settings, model_hadrons, os_data)
       allocate (shower_t :: evt_shower%shower)
       call evt_shower%shower%init (evt_shower%settings, pdf_data)
       call evt_shower%connect (process_instance, model)                
    end select

    evt_trivial%next => evt_shower
    evt_shower%previous => evt_trivial

    call evt_shower%prepare_new_event (1, 1)
    call evt_shower%generate_unweighted ()
    call evt_shower%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_hadrons%final ()
    deallocate (model_hadrons)
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_1"
    
  end subroutine shower_1
  
  subroutine shower_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t), target :: lib
    type(model_list_t) :: model_list
    type(model_t), pointer :: model_hadrons
    class(model_data_t), pointer :: model
    type(process_t), target :: process
    type(process_instance_t), target :: process_instance
    integer :: factorization_mode
    logical :: keep_correlations
    type(pdf_data_t) :: pdf_data
    class(evt_t), allocatable, target :: evt_trivial
    class(evt_t), allocatable, target :: evt_shower
    type(shower_settings_t) :: settings

    write (u, "(A)")  "* Test output: shower_2"
    write (u, "(A)")  "*   Purpose: Two-jet event with FSR shower"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
         os_data, model_hadrons)
    call setup_testbed (var_str ("shower_2"), &
         os_data, lib, model_list, process, process_instance)
    model => process%get_model_ptr ()
    
    write (u, "(A)")  "* Set up trivial transform"
    write (u, "(A)")
    
    allocate (evt_trivial_t :: evt_trivial)
    call evt_trivial%connect (process_instance, model)
    call evt_trivial%prepare_new_event (1, 1)
    call evt_trivial%generate_unweighted ()

    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    call evt_trivial%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_trivial)
    type is (evt_trivial_t)
       call evt_trivial%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    settings%fsr_active = .true.

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (settings, model_hadrons, os_data)
       allocate (shower_t :: evt_shower%shower)
       call evt_shower%shower%init (evt_shower%settings, pdf_data)
       call evt_shower%connect (process_instance, model)       
    end select

    evt_trivial%next => evt_shower
    evt_shower%previous => evt_trivial

    call evt_shower%prepare_new_event (1, 1)
    call evt_shower%generate_unweighted ()
    call evt_shower%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%write (u, testflag = .true.)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_hadrons%final ()
    deallocate (model_hadrons)
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_2"
    
  end subroutine shower_2
  

end module shower
