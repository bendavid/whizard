! WHIZARD 2.2.5 Feb 27 2015
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
  use unit_tests
  use system_defs, only: LF
  use os_interface
  use xml
  use diagnostics
  use lorentz
  use system_dependencies, only: LHAPDF5_AVAILABLE
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use lhapdf !NODEP!
  use pdf_builtin !NODEP!
  
  use shower_base
  use shower_partons
  use shower_core
  use shower_topythia
  use muli, only: muli_t
  use mlm_matching
  use ckkw_pseudo_weights
  use ckkw_matching
  
  use sm_qcd
  use flavors
  use colors
  use particles
  use state_matrices
  use subevents
  use model_data
  use variables
  use beam_structures
  use process_libraries
  use rng_base
  use mci_base
  use phs_base

  use event_transforms
  ! For model_hadrons and testing
  use models
  ! For Pythia6 communication
  use hep_common
  ! For PDF information
  use processes
  ! For integration tests
  use rng_tao
  use mci_midpoint
  use phs_single
  use prc_core
  use prc_omega

  implicit none
  private

  public :: shower_settings_t
  public :: apply_shower_particle_set
  public :: evt_shower_t
  public :: ckkw_fake_pseudo_shower_weights
  public :: shower_test

  type :: shower_settings_t
     logical :: ps_isr_active = .false.
     logical :: ps_fsr_active = .false.
     logical :: ps_use_PYTHIA_shower = .false.
     logical :: hadronization_active = .false.
     logical :: mlm_matching = .false.
     logical :: ckkw_matching = .false.
     logical :: muli_active = .false.

     logical :: ps_PYTHIA_verbose = .false.
     type(string_t) :: ps_PYTHIA_PYGIVE

     !!! values present in PYTHIA and WHIZARDs PS, 
     !!! comments denote corresponding PYTHIA values
     real(default) :: ps_mass_cutoff = 1._default      ! PARJ(82)
     real(default) :: ps_fsr_lambda = 0.29_default     ! PARP(72)
     real(default) :: ps_isr_lambda = 0.29_default     ! PARP(61)
     integer :: ps_max_n_flavors = 5                   ! MSTJ(45)
     logical :: ps_isr_alpha_s_running = .true.        ! MSTP(64)
     logical :: ps_fsr_alpha_s_running = .true.        ! MSTJ(44)
     real(default) :: ps_fixed_alpha_s = 0._default    ! PARU(111)
     logical :: ps_isr_pt_ordered = .false.
     logical :: ps_isr_angular_ordered = .true.        ! MSTP(62)
     real(default) :: ps_isr_primordial_kt_width = 0._default  ! PARP(91)
     real(default) :: ps_isr_primordial_kt_cutoff = 5._default ! PARP(93)
     real(default) :: ps_isr_z_cutoff = 0.999_default  ! 1-PARP(66)
     real(default) :: ps_isr_minenergy = 2._default    ! PARP(65)
     real(default) :: ps_isr_tscalefactor = 1._default
     logical :: ps_isr_only_onshell_emitted_partons = .true.   ! MSTP(63)

     !!! MLM settings
     type(mlm_matching_settings_t) :: ms

     !!! CKKW Matching
     type(ckkw_matching_settings_t) :: ckkw_settings
     type(ckkw_pseudo_shower_weights_t) :: ckkw_weights
   contains 
     procedure :: init => shower_settings_init
     procedure :: write => shower_settings_write  
  end type shower_settings_t

  type, extends (evt_t) :: evt_shower_t
     type(shower_settings_t) :: settings
     type(model_t), pointer :: model_hadrons => null ()
     type(lhapdf_pdf_t) :: pdf
     real(double) :: xmin = 0, xmax = 0
     real(double) :: qmin = 0, qmax = 0
     type(os_data_t) :: os_data     
     integer :: pdf_type = STRF_NONE
     integer :: pdf_set = 0
   contains
     procedure :: write => evt_shower_write
     procedure :: init => evt_shower_init
     procedure :: setup_pdf => evt_shower_setup_pdf
     procedure :: prepare_new_event => evt_shower_prepare_new_event
     procedure :: generate_weighted => evt_shower_generate_weighted
     procedure :: make_particle_set => evt_shower_make_particle_set
     procedure :: assure_heprup => event_shower_assure_heprup
  end type evt_shower_t
  

contains

  subroutine shower_settings_init (shower_settings, var_list)
    class(shower_settings_t), intent(out) :: shower_settings
    type(var_list_t), intent(in) :: var_list

    shower_settings%ps_isr_active = &
         var_list%get_lval (var_str ("?ps_isr_active"))
    shower_settings%ps_fsr_active = &
         var_list%get_lval (var_str ("?ps_fsr_active"))
    shower_settings%hadronization_active = &
         var_list%get_lval (var_str ("?hadronization_active"))
    shower_settings%mlm_matching = &
         var_list%get_lval (var_str ("?mlm_matching"))
    shower_settings%ckkw_matching = & 
         var_list%get_lval (var_str ("?ckkw_matching"))
    shower_settings%muli_active = &
         var_list%get_lval (var_str ("?muli_active"))

    if (.not. shower_settings%ps_fsr_active .and. &
        .not. shower_settings%ps_isr_active .and. &
        .not. shower_settings%hadronization_active .and. &
        .not. shower_settings%mlm_matching) then
       return
    end if

    shower_settings%ps_use_PYTHIA_shower = &
         var_list%get_lval (var_str ("?ps_use_PYTHIA_shower"))
    shower_settings%ps_PYTHIA_verbose = &
         var_list%get_lval (var_str ("?ps_PYTHIA_verbose"))
    shower_settings%ps_PYTHIA_PYGIVE = &
         var_list%get_sval (var_str ("$ps_PYTHIA_PYGIVE"))
    shower_settings%ps_mass_cutoff = &
         var_list%get_rval (var_str ("ps_mass_cutoff"))
    shower_settings%ps_fsr_lambda = &
         var_list%get_rval (var_str ("ps_fsr_lambda"))
    shower_settings%ps_isr_lambda = &
         var_list%get_rval (var_str ("ps_isr_lambda"))
    shower_settings%ps_max_n_flavors = &
         var_list%get_ival (var_str ("ps_max_n_flavors"))
    shower_settings%ps_isr_alpha_s_running = &
         var_list%get_lval (var_str ("?ps_isr_alpha_s_running"))
    shower_settings%ps_fsr_alpha_s_running = &
         var_list%get_lval (var_str ("?ps_fsr_alpha_s_running"))
    shower_settings%ps_fixed_alpha_s = &
         var_list%get_rval (var_str ("ps_fixed_alpha_s"))
    shower_settings%ps_isr_pt_ordered = &
         var_list%get_lval (var_str ("?ps_isr_pt_ordered"))
    shower_settings%ps_isr_angular_ordered = &
         var_list%get_lval (var_str ("?ps_isr_angular_ordered"))
    shower_settings%ps_isr_primordial_kt_width = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_width"))
    shower_settings%ps_isr_primordial_kt_cutoff = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_cutoff"))
    shower_settings%ps_isr_z_cutoff = &
         var_list%get_rval (var_str ("ps_isr_z_cutoff"))
    shower_settings%ps_isr_minenergy = &
         var_list%get_rval (var_str ("ps_isr_minenergy"))
    shower_settings%ps_isr_tscalefactor = &
         var_list%get_rval (var_str ("ps_isr_tscalefactor"))
    shower_settings%ps_isr_only_onshell_emitted_partons = &
         var_list%get_lval (&
         var_str ("?ps_isr_only_onshell_emitted_partons"))

    !!! MLM matching
    shower_settings%ms%mlm_Qcut_ME = &
         var_list%get_rval (var_str ("mlm_Qcut_ME"))
    shower_settings%ms%mlm_Qcut_PS = &
         var_list%get_rval (var_str ("mlm_Qcut_PS"))
    shower_settings%ms%mlm_ptmin = &
         var_list%get_rval (var_str ("mlm_ptmin"))
    shower_settings%ms%mlm_etamax = &
         var_list%get_rval (var_str ("mlm_etamax"))
    shower_settings%ms%mlm_Rmin = &
         var_list%get_rval (var_str ("mlm_Rmin"))
    shower_settings%ms%mlm_Emin = &
         var_list%get_rval (var_str ("mlm_Emin"))
    shower_settings%ms%mlm_nmaxMEjets = &
         var_list%get_ival (var_str ("mlm_nmaxMEjets"))

    shower_settings%ms%mlm_ETclusfactor = &
         var_list%get_rval (var_str ("mlm_ETclusfactor"))
    shower_settings%ms%mlm_ETclusminE = &
         var_list%get_rval (var_str ("mlm_ETclusminE"))
    shower_settings%ms%mlm_etaclusfactor = &
         var_list%get_rval (var_str ("mlm_etaclusfactor"))
    shower_settings%ms%mlm_Rclusfactor = &
         var_list%get_rval (var_str ("mlm_Rclusfactor"))
    shower_settings%ms%mlm_Eclusfactor = &
         var_list%get_rval (var_str ("mlm_Eclusfactor"))

    !!! CKKW matching
    ! TODO
  end subroutine shower_settings_init

  subroutine shower_settings_write (object, unit)
    class(shower_settings_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "Shower settings:"
    call write_separator (u)
    write (u, "(1x,A)")  "Master switches:"
    write (u, "(3x,A,1x,L1)") &
         "ps_isr_active                = ", object%ps_isr_active
    write (u, "(3x,A,1x,L1)") &
         "ps_fsr_active                = ", object%ps_fsr_active
    write (u, "(1x,A)")  "General settings:"
    if (object%ps_isr_active .or. object%ps_fsr_active) then
       write (u, "(3x,A,1x,L1)") &
            "ps_use_PYTHIA_shower         = ", object%ps_use_PYTHIA_shower
       write (u, "(3x,A,ES19.12)") &
            "ps_mass_cutoff               = ", object%ps_mass_cutoff
       write (u, "(3x,A,1x,I1)") &
            "ps_max_n_flavors             = ", object%ps_max_n_flavors
    else
       write (u, "(3x,A)") " [ISR and FSR off]"
    end if
    if (object%ps_isr_active) then
       write (u, "(1x,A)")  "ISR settings:"
       write (u, "(3x,A,1x,L1)") &
            "ps_isr_pt_ordered            = ", object%ps_isr_pt_ordered
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_lambda                = ", object%ps_isr_lambda
       write (u, "(3x,A,1x,L1)") &
            "ps_isr_alpha_s_running       = ", object%ps_isr_alpha_s_running
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_primordial_kt_width   = ", object%ps_isr_primordial_kt_width
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_primordial_kt_cutoff  = ", &
            object%ps_isr_primordial_kt_cutoff
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_z_cutoff              = ", object%ps_isr_z_cutoff
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_minenergy             = ", object%ps_isr_minenergy
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_tscalefactor          = ", object%ps_isr_tscalefactor
    else if (object%ps_fsr_active) then
       write (u, "(3x,A)") " [ISR off]"       
    end if
    if (object%ps_fsr_active) then
       write (u, "(1x,A)")  "FSR settings:"
       write (u, "(3x,A,ES19.12)") &
            "ps_fsr_lambda                = ", object%ps_fsr_lambda
       write (u, "(3x,A,1x,L1)") &
            "ps_fsr_alpha_s_running       = ", object%ps_fsr_alpha_s_running
    else if (object%ps_isr_active) then
       write (u, "(3x,A)") " [FSR off]"              
    end if
    write (u, "(1x,A)")  "Hadronization settings:"
    write (u, "(3x,A,1x,L1)") &
         "hadronization_active         = ", object%hadronization_active
    write (u, "(1x,A)")  "Matching Settings:"
    write (u, "(3x,A,1x,L1)") &
         "mlm_matching                 = ", object%mlm_matching
    if (object%mlm_matching) then
       call mlm_matching_settings_write (object%ms, u)
    end if
    write (u, "(3x,A,1x,L1)") &
         "ckkw_matching                = ", object%ckkw_matching
    if (object%ckkw_matching) then
       ! TODO ckkw settings etc.
    end if
    write (u, "(1x,A)")  "PYTHIA specific settings:"
    write (u, "(3x,A,1x,L1)") &
         "ps_PYTHIA_verbose            = ", object%ps_PYTHIA_verbose
    write (u, "(3x,A,A,A)") &
         "ps_PYTHIA_PYGIVE             =  '", &
         char(object%ps_PYTHIA_PYGIVE), "'"
  end subroutine shower_settings_write

  subroutine apply_shower_particle_set & 
       (evt, valid, vetoed)
    class(evt_shower_t), intent(inout) :: evt
    type(particle_t), dimension(1:2) :: prt_in
    logical, intent(inout) :: valid
    logical, intent(inout) :: vetoed
    real(kind=double) :: pdftest       
    logical, parameter :: debug = .false., to_file = .false.
    integer :: i

    type(mlm_matching_data_t) :: mlm_matching_data
    logical, save :: matching_disabled = .false.

    if (.not. evt%settings%ps_fsr_active .and. &
        .not. evt%settings%ps_isr_active .and. &
        .not. evt%settings%hadronization_active .and. &
        .not. evt%settings%mlm_matching) then
       ! return if nothing to do
       return
    end if

    ! return if already invalid or vetoed
    if (.not. valid .or. vetoed)  return

    if (signal_is_pending ()) return    

    do i = 1, 2
       prt_in(i) = evt%particle_set%get_particle (i)
    end do
    
    ! ensure that lhapdf is initialized
    if (evt%pdf_type .eq. STRF_LHAPDF5) then
       if (evt%settings%ps_isr_active .and. &
            (all (abs (prt_in%get_pdg ()) >= 1000))) then
          call GetQ2max (0, pdftest)
          if (pdftest < epsilon(pdftest)) then
             call msg_fatal ("ISR QCD shower enabled, but LHAPDF not" // &
                  "initialized," // LF // "     aborting simulation")
             return
          end if
       end if
    else if (evt%pdf_type == STRF_PDF_BUILTIN) then
       if (evt%settings%ps_use_PYTHIA_shower) then
          call msg_fatal ("Builtin PDFs cannot be used for PYTHIA showers," &
               // LF // "     aborting simulation")
          return
       end if
    end if
    if (evt%settings%mlm_matching .and. evt%settings%ckkw_matching) then
       call msg_fatal ("Both MLM and CKKW matching activated," // &
            LF // "     aborting simulation")
       return      
    end if

    if (debug)  call evt%settings%write ()
    
    if (evt%settings%ps_use_PYTHIA_shower .or. &
         evt%settings%hadronization_active) then
       if (.not. evt%settings%ps_PYTHIA_verbose) then
          call PYGIVE ('MSTU(12)=12345')
          call PYGIVE ('MSTU(13)=0')
       else 
          call PYGIVE ('MSTU(13)=1')
       end if
    end if

    if (debug)  print *, "Shower: beam checks for mlm_matching"

    if (.not. matching_disabled) then
       !!! Check if the beams are hadrons
       if (all (abs (prt_in%get_pdg ()) <= 18)) then
          mlm_matching_data%is_hadron_collision = .false.
       else if (all (abs (prt_in%get_pdg ()) >= 1000)) then
          mlm_matching_data%is_hadron_collision = .true.
       else 
          call msg_error (" Matching didn't recognize beams setup," // &
               LF // "     disabling matching")
          matching_disabled = .true.
          return
       end if
    end if
    
    if (debug)  print *, "Shower: apply shower"

    !!! SHOWER
    if (evt%settings%ps_use_PYTHIA_shower .or. &
         (.not. evt%settings%ps_fsr_active .and. &
          .not. evt%settings%ps_isr_active .and. &
           evt%settings%hadronization_active)) then
       call apply_PYTHIAshower_particle_set (evt%particle_set, &
            evt%settings, mlm_matching_data%P_ME, evt%model, evt%model_hadrons, &
            valid)
       if (debug)  call pylist(2)
    else
       call apply_WHIZARDshower_particle_set (evt%particle_set, &
            evt%settings, mlm_matching_data%P_ME, evt%model, evt%model_hadrons, &
            evt%os_data, evt%pdf_type, evt%pdf_set, evt%pdf, &
            evt%xmin, evt%xmax, evt%qmin, evt%qmax, valid, vetoed)
       if (vetoed) return
    end if
    if (debug) then
       print *, " after SHOWER"
       call evt%particle_set%write ()
    end if
       
    if (evt%settings%mlm_matching .and. &
         (matching_disabled.eqv..false.)) then
    !!! MLM stage 2 -> PS jets and momenta
       call matching_transfer_PS &
            (mlm_matching_data, evt%particle_set, evt%settings)
    !!! MLM stage 3 -> reconstruct and possible reject
       call mlm_matching_apply (mlm_matching_data, evt%settings%ms, vetoed)
       if (vetoed) then
          call mlm_matching_data_final (mlm_matching_data)
          return
       end if
    endif

!!! HADRONIZATION
    if (evt%settings%hadronization_active) then
       !! Assume that the event record is still in the PYTHIA COMMON BLOCKS
       !! transferred there by one of the shower routines
       if (valid) then
          call apply_PYTHIAhadronization (evt%particle_set, &
               evt%settings, evt%model, evt%model_hadrons, valid)
       end if
    end if
!!! FINAL

    call mlm_matching_data_final (mlm_matching_data)

    if (debug)  print *, "SHOWER+HADRONIZATION+MATCHING finished"

  contains
    
    subroutine shower_set_PYTHIA_error (mstu23)
      ! PYTHIA common blocks
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE/PYDAT1/

      integer, intent(in) :: mstu23

      MSTU(23) = mstu23
    end subroutine shower_set_PYTHIA_error

    function shower_get_PYTHIA_error () result (mstu23)
      ! PYTHIA common blocks
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE/PYDAT1/

      integer :: mstu23

      mstu23 = MSTU(23)
    end function shower_get_PYTHIA_error

    subroutine apply_PYTHIAshower_particle_set &
         (particle_set, shower_settings, JETS_ME, model, model_hadrons, valid)

      type(particle_set_t), intent(inout) :: particle_set
      type(particle_set_t) :: pset_reduced
      type(shower_settings_t), intent(in) :: shower_settings
      type(vector4_t), dimension(:), allocatable, intent(inout) :: JETS_ME
      class(model_data_t), intent(in), target :: model
      class(model_data_t), intent(in), target :: model_hadrons
      logical, intent(inout) :: valid
      real(kind=default) :: rand
      
      ! units for transfer from WHIZARD to PYTHIA and back
      integer :: u_W2P, u_P2W
      integer, save :: pythia_initialized_for_NPRUP = 0
      logical, save :: pythia_warning_given = .false.
      logical, save :: msg_written = .false.
      type(string_t) :: remaining_PYGIVE, partial_PYGIVE
      character(len=10) :: buffer

      if (signal_is_pending ()) return      
      
      if (debug) then
         print *, "debugging the shower"
         print *, IDBMUP(1), IDBMUP(2)
         print *, EBMUP, PDFGUP, PDFSUP, IDWTUP
         print *, "NPRUP = ", NPRUP
      end if
      
      ! check if the beam particles are quarks
      if (any (abs(IDBMUP) <= 8)) then
         ! PYTHIA doesn't support these settings
         if (.not. pythia_warning_given) then
            call msg_error ("PYTHIA doesn't support quarks as beam particles," &
                 // LF // "     neglecting ISR, FSR and hadronization")
            pythia_warning_given = .true.
         end if
         return
      end if
      
      call particle_set%reduce (pset_reduced)
      call hepeup_from_particle_set (pset_reduced)
      
      call hepeup_set_event_parameters (proc_id=1)

      u_W2P = free_unit ()
      if (debug .and. to_file) then
         open (unit=u_W2P, status="replace", &
              file="whizardout1.lhe", action="readwrite")  
      else
         open (unit=u_W2P, status="scratch", action="readwrite")
      end if
      call shower_W2P_write_event (u_W2P)
      rewind (u_W2P)
      if (signal_is_pending ()) return          
      write (buffer, "(I10)")  u_W2P
      call PYGIVE ("MSTP(161)="//buffer)
      call PYGIVE ("MSTP(162)="//buffer)
      if (debug)  write (*, "(A)")  buffer
      if (shower_settings%ps_isr_active) then
         call PYGIVE ("MSTP(61)=1")  
      else
         call PYGIVE ("MSTP(61)=0")  !!! switch off ISR
      end if
      if (shower_settings%ps_fsr_active) then
         call PYGIVE ("MSTP(71)=1")  
      else
         call PYGIVE ("MSTP(71)=0")   !!! switch off FSR
      end if
      call PYGIVE ("MSTP(111)=0")     !!! switch off hadronization

      if (pythia_initialized_for_NPRUP >= NPRUP) then
         if (debug)  print *, "calling upinit"
         call upinit
         if (debug)  print *, "returned from upinit"
      else
         write (buffer, "(F10.5)") shower_settings%ps_mass_cutoff
         call PYGIVE ("PARJ(82)="//buffer)
         write (buffer, "(F10.5)") shower_settings%ps_isr_tscalefactor
         call PYGIVE ("PARP(71)="//buffer)    

         write (buffer, "(F10.5)") shower_settings%ps_fsr_lambda
         call PYGIVE ("PARP(72)="//buffer)
         write(buffer, "(F10.5)") shower_settings%ps_isr_lambda
         call PYGIVE ("PARP(61)="//buffer)
         write (buffer, "(I10)") shower_settings%ps_max_n_flavors
         call PYGIVE ("MSTJ(45)="//buffer)
         if (shower_settings%ps_isr_alpha_s_running) then
            call PYGIVE ("MSTP(64)=2")
         else
            call PYGIVE ("MSTP(64)=0")
         end if
         if (shower_settings%ps_fsr_alpha_s_running) then
            call PYGIVE ("MSTJ(44)=2")
         else
            call PYGIVE ("MSTJ(44)=0")
         end if
         write (buffer, "(F10.5)") shower_settings%ps_fixed_alpha_s
         call PYGIVE ("PARU(111)="//buffer)
         write (buffer, "(F10.5)") shower_settings%ps_isr_primordial_kt_width
         call PYGIVE ("PARP(91)="//buffer)
         write (buffer, "(F10.5)") shower_settings%ps_isr_primordial_kt_cutoff
         call PYGIVE ("PARP(93)="//buffer)
         write (buffer, "(F10.5)") 1._double - shower_settings%ps_isr_z_cutoff
         call PYGIVE ("PARP(66)="//buffer)
         write (buffer, "(F10.5)") shower_settings%ps_isr_minenergy
         call PYGIVE ("PARP(65)="//buffer)
         if (shower_settings%ps_isr_only_onshell_emitted_partons) then
            call PYGIVE ("MSTP(63)=0")
         else
            call PYGIVE ("MSTP(63)=2")
         end if
         if (shower_settings%mlm_matching) then
            CALL PYGIVE ("MSTP(62)=2")
            CALL PYGIVE ("MSTP(67)=0")
         end if
         if (debug)  print *, "calling pyinit"
         call PYINIT ("USER", "", "", 0D0)

         call evt%rng%generate (rand)
         write (buffer, "(I10)") floor (rand*900000000)
         call pygive ("MRPY(1)="//buffer)
         call pygive ("MRPY(2)=0")

         if (len(shower_settings%ps_PYTHIA_PYGIVE) > 0) then
            remaining_PYGIVE = shower_settings%ps_PYTHIA_PYGIVE
            do while (len (remaining_PYGIVE)>0)
               call split (remaining_PYGIVE, partial_PYGIVE, ";")
               call PYGIVE (char (partial_PYGIVE))
            end do
            if (shower_get_PYTHIA_error() /= 0) then
               call msg_fatal &
                    (" PYTHIA did not recognize ps_PYTHIA_PYGIVE setting.")
            end if
         end if

         pythia_initialized_for_NPRUP = NPRUP
      end if

      if (.not. msg_written) then
         call msg_message ("Using PYTHIA interface for parton showers")
         msg_written = .true.
      end if      
      call PYEVNT ()
            
      if (debug)  write (*, "(A)")  "called pyevnt"
      
      u_P2W = free_unit ()
      write (buffer, "(I10)")  u_P2W
      call PYGIVE ("MSTP(163)="//buffer)
      if (debug .and. to_file) then
         open (unit = u_P2W, file="pythiaout.lhe", status="replace", &
              action="readwrite")  
      else
         open (unit = u_P2W, status="scratch", action="readwrite")
      end if
      if (debug)  write (*, "(A)")  "calling PYLHEO"
      !!! convert pythia /PYJETS/ to lhef given in MSTU(163)=u_P2W
      call PYLHEO
      !!! read and add lhef from u_P2W
      if (signal_is_pending ()) return          
      call shower_add_lhef_to_particle_set &
           (particle_set, u_P2W, model, model_hadrons)
      close (unit=u_P2W)
      
      !!! Transfer momenta of the partons in the final state of 
      !!!     the hard initeraction
      if (shower_settings%mlm_matching)  &
           call get_ME_momenta_from_PYTHIA (JETS_ME)

      if (shower_get_PYTHIA_error () > 0) then
         !!! clean up, discard shower and exit
         call shower_set_PYTHIA_error (0)
         valid = .false.
      end if
      close (unit=u_W2P)
    end subroutine apply_PYTHIAshower_particle_set

    subroutine apply_WHIZARDshower_particle_set & 
         (particle_set, shower_settings, JETS_ME, model, model_hadrons, &
         os_data, pdf_type, pdf_set, pdf, &
         xmin, xmax, qmin, qmax, valid, vetoed)
      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      type(vector4_t), dimension(:), allocatable, intent(inout) :: JETS_ME
      class(model_data_t), intent(in), target :: model
      class(model_data_t), intent(in), target :: model_hadrons
      type(os_data_t), intent(in) :: os_data
      type(lhapdf_pdf_t), intent(inout) :: pdf
      integer, intent(in) :: pdf_set, pdf_type
      real(double), intent(in) :: xmin, xmax, qmin, qmax
      logical, intent(inout) :: valid
      logical, intent(out) :: vetoed

      type(muli_t), save :: mi
      type(shower_t) :: shower
      type(parton_t), dimension(:), allocatable, target :: partons, hadrons
      type(parton_pointer_t), dimension(:), allocatable :: &
           parton_pointers, final_ME_partons
      real(kind=default) :: mi_scale, ps_scale, shat, phi
      type(parton_pointer_t) :: temppp
      type(particle_t) :: prt
      integer, dimension(:), allocatable :: connections
      integer :: n_loop, i, j, k
      integer :: n_hadrons, n_in, n_out
      integer :: n_int
      integer :: max_color_nr
      integer, dimension(2) :: col_array
      integer, dimension(1) :: parent
      logical, save :: msg_written = .false.
      integer, dimension(2,4) :: color_corr
      integer :: u_S2W      

      vetoed = .false.

      if (signal_is_pending ()) return          
      
      if (debug) print *, "Transfer settings from shower_settings to shower"
      call shower_set_D_Min_t (shower_settings%ps_mass_cutoff**2)
      call shower_set_D_Lambda_fsr (shower_settings%ps_fsr_lambda)
      call shower_set_D_Lambda_isr (shower_settings%ps_isr_lambda)
      call shower_set_D_Nf (shower_settings%ps_max_n_flavors)
      call shower_set_D_running_alpha_s_fsr &
           (shower_settings%ps_fsr_alpha_s_running)
      call shower_set_D_running_alpha_s_isr &
           (shower_settings%ps_isr_alpha_s_running)
      call shower_set_D_constantalpha_s &
           (shower_settings%ps_fixed_alpha_s)
      call shower_set_isr_pt_ordered &
           (shower_settings%ps_isr_pt_ordered)
      Call shower_set_primordial_kt_width &
           (shower_settings%ps_isr_primordial_kt_width)
      call shower_set_primordial_kt_cutoff &
           (shower_settings%ps_isr_primordial_kt_cutoff)
      shower%isr_angular_ordered = shower_settings%ps_isr_angular_ordered
      shower%pdf_set = pdf_set
      shower%pdf_type = pdf_type
      shower%maxz_isr = shower_settings%ps_isr_z_cutoff
      shower%minenergy_timelike = shower_settings%ps_isr_minenergy
      shower%tscalefactor_isr = shower_settings%ps_isr_tscalefactor
      call shower_set_rng(evt%rng)
      
      if (.not. msg_written) then
         call msg_message ("Using WHIZARD's internal showering")
         msg_written = .true.
      end if

      n_loop = 0
      TRY_SHOWER: do ! just a loop to be able to discard events
         n_loop = n_loop + 1
         ! TODO: (bcn 2014-12-02) Can this loop even occur twice?
         if (n_loop > 1000) call msg_fatal &
              ("Shower: too many loops (try_shower)")
         if (pdf_type == STRF_LHAPDF6) then
            call shower%create (xmin, xmax, qmin, qmax, pdf)
         else
            call shower%create (xmin, xmax, qmin, qmax)
         end if
         if (signal_is_pending ()) return             
         max_color_nr = 0

         n_hadrons = 0
         n_in = 0
         n_out = 0
         do i = 1, particle_set%get_n_tot ()
            prt = particle_set%get_particle (i)
            if (prt%get_status () == PRT_BEAM) &
                 n_hadrons = n_hadrons + 1
            if (prt%get_status () == PRT_INCOMING) &
                 n_in = n_in + 1
            if (prt%get_status () == PRT_OUTGOING) &
                 n_out = n_out + 1
         end do

         allocate (connections (1:particle_set%get_n_tot ()))
         connections = 0

         allocate (hadrons (1:2))
         allocate (partons (1:n_in+n_out))
         allocate (parton_pointers (1:n_in+n_out))

         j=0
         if (n_hadrons > 0) then
            if (debug) print *, "Transfer hadrons from particle_set to hadrons"
            do i = 1, particle_set%get_n_tot ()
               prt = particle_set%get_particle (i)
               if (prt%get_status () == PRT_BEAM) then
                  j = j + 1
                  hadrons(j)%nr = shower%get_next_free_nr ()
                  hadrons(j)%momentum = prt%get_momentum ()
                  hadrons(j)%t = hadrons(j)%momentum**2
                  hadrons(j)%type = prt%get_pdg ()
                  col_array = prt%get_color ()
                  hadrons(j)%c1 = col_array(1)
                  hadrons(j)%c2 = col_array(2)
                  max_color_nr = max (max_color_nr, abs(hadrons(j)%c1), &
                       abs(hadrons(j)%c2))
                  hadrons(j)%interactionnr = 1
                  connections(i) = j
               end if
            end do
         end if

         j = 0
         if (debug) print *, "Transfer incoming partons from particle_set to partons"
         do i = 1, particle_set%get_n_tot ()
            prt = particle_set%get_particle (i)
            if (prt%get_status () == PRT_INCOMING) then
               j = j+1
               partons(j)%nr = shower%get_next_free_nr ()
               partons(j)%momentum = prt%get_momentum ()
               partons(j)%t = partons(j)%momentum**2
               partons(j)%type = prt%get_pdg ()
               col_array = prt%get_color ()
               partons(j)%c1 = col_array (1)
               partons(j)%c2 = col_array (2)
               parton_pointers(j)%p => partons(j)
               max_color_nr = max (max_color_nr, abs (partons(j)%c1), &
                    abs (partons(j)%c2))
               connections(i)=j
               ! insert dependences on hadrons
               if (prt%get_n_parents () == 1) then
                  parent = prt%get_parents ()
                  partons(j)%initial => hadrons (connections (parent(1)))
                  partons(j)%x = space_part_norm (partons(j)%momentum) / &
                                 space_part_norm (partons(j)%initial%momentum)
               end if
            end if
         end do
         if (signal_is_pending ()) return             
         if (debug) print *, "Transfer outgoing partons from particle_set to partons"
         do i = 1, particle_set%get_n_tot ()
            prt = particle_set%get_particle (i)
            if (prt%get_status () == PRT_OUTGOING) then               
               j = j + 1
               partons(j)%nr = shower%get_next_free_nr ()
               partons(j)%momentum = prt%get_momentum ()
               partons(j)%t = partons(j)%momentum**2
               partons(j)%type = prt%get_pdg ()
               col_array = prt%get_color ()
               partons(j)%c1 = col_array(1)
               partons(j)%c2 = col_array(2)
               parton_pointers(j)%p => partons(j)
               max_color_nr = max (max_color_nr, abs &
                    (partons(j)%c1), abs (partons(j)%c2))
               connections(i) = j
            end if
         end do

         deallocate (connections)

         if (debug) print *, "Insert partons in shower"
         call shower%set_next_color_nr (1 + max_color_nr)
         call shower%add_interaction_2ton_CKKW &
              (parton_pointers, shower_settings%ckkw_weights)

         if (signal_is_pending ()) return             
         if (shower_settings%muli_active) then
            if (debug) print *, "Activate multiple interactions"
            !!! Initialize muli pdf sets, unless initialized
            if (mi%is_initialized ()) then
               call mi%restart ()
            else
               call mi%initialize (&
                    GeV2_scale_cutoff=D_Min_t, &
                    GeV2_s=shower_interaction_get_s &
                    (shower%interactions(1)%i), &
                    muli_dir=char(os_data%whizard_mulipath))
            end if

            !!! initial interaction
            call mi%apply_initial_interaction ( &
                 GeV2_s=shower_interaction_get_s(shower%interactions(1)%i), &
                 x1=shower%interactions(1)%i%partons(1)%p%parent%x, &
                 x2=shower%interactions(1)%i%partons(2)%p%parent%x, &
                 pdg_f1=shower%interactions(1)%i%partons(1)%p%parent%type, &
                 pdg_f2=shower%interactions(1)%i%partons(2)%p%parent%type, &
                 n1=shower%interactions(1)%i%partons(1)%p%parent%nr, &
                 n2=shower%interactions(1)%i%partons(2)%p%parent%nr)
         end if

         if (signal_is_pending ()) return             
         
         if (shower_settings%ckkw_matching) then
            if (debug) print *, "Apply CKKW matching"
            call ckkw_matching_apply (shower, &
                 shower_settings%ckkw_settings, &
                 shower_settings%ckkw_weights, vetoed)
            if (vetoed) then
               return
            end if
         end if

         if (shower_settings%ps_isr_active) then
            i = 0
            BRANCHINGS: do
               i = i+1
               if (signal_is_pending ()) return                   
               if (shower_settings%muli_active) then
                  call mi%generate_gev2_pt2 &
                       (shower%get_ISR_scale (), mi_scale)
               else
                  mi_scale = 0.0
               end if

               !!! Shower: debugging
               !!! shower%generate_next_isr_branching returns a pointer to 
               !!! the parton with the next ISR-branching, this parton's 
               !!! scale is the scale of the next branching 
               ! temppp=shower%generate_next_isr_branching_veto ()
               temppp = shower%generate_next_isr_branching ()
                  
               if (.not. associated (temppp%p) .and. &
                    mi_scale < D_Min_t) then
                  exit BRANCHINGS
               end if
               !!! check if branching or interaction occurs next
               if (associated (temppp%p)) then
                  ps_scale = abs(temppp%p%t)
               else
                  ps_scale = 0._default
               end if
               if (mi_scale > ps_scale) then
                  !!! discard branching evolution lower than mi_scale
                  call shower%set_max_ISR_scale (mi_scale)
                  if (associated (temppp%p)) &
                       call parton_set_simulated(temppp%p, .false.)
                 
                  !!! execute new interaction
                  deallocate (partons)
                  deallocate (parton_pointers)
                  allocate (partons(1:4))
                  allocate (parton_pointers(1:4))
                  do j = 1, 4
                     partons(j)%nr = shower%get_next_free_nr ()
                     partons(j)%belongstointeraction = .true.
                     parton_pointers(j)%p => partons(j)
                  end do
                  call mi%generate_partons (partons(1)%nr, partons(2)%nr, &
                       partons(1)%x, partons(2)%x, &
                       partons(1)%type, partons(2)%type, &
                       partons(3)%type, partons(4)%type)
                  !!! calculate momenta
                  shat = partons(1)%x *partons(2)%x * &
                       shower_interaction_get_s(shower%interactions(1)%i)
                  partons(1)%momentum = [0.5_default * sqrt(shat), &
                       zero, zero, 0.5_default*sqrt(shat)]
                  partons(2)%momentum = [0.5_default * sqrt(shat), &
                       zero, zero, -0.5_default*sqrt(shat)]
                  call parton_set_initial (partons(1), &
                       shower%interactions(1)%i%partons(1)%p%initial)
                  call parton_set_initial (partons(2), &
                       shower%interactions(1)%i%partons(2)%p%initial)
                  partons(1)%belongstoFSR = .false.
                  partons(2)%belongstoFSR = .false.
                  !!! calculate color connection
                  call mi%get_color_correlations &
                      (shower%get_next_color_nr (), &
                      max_color_nr,color_corr)
                  call shower%set_next_color_nr (max_color_nr)

                  partons(1)%c1 = color_corr(1,1)
                  partons(1)%c2 = color_corr(2,1)
                  partons(2)%c1 = color_corr(1,2)
                  partons(2)%c2 = color_corr(2,2)
                  partons(3)%c1 = color_corr(1,3)
                  partons(3)%c2 = color_corr(2,3)
                  partons(4)%c1 = color_corr(1,4)
                  partons(4)%c2 = color_corr(2,4)

                  call evt%rng%generate (phi)
                  phi = 2 * pi * phi
                  partons(3)%momentum = [0.5_default*sqrt(shat), &
                       sqrt(mi_scale)*cos(phi), &
                       sqrt(mi_scale)*sin(phi), &
                       sqrt(0.25_default*shat - mi_scale)]
                  partons(4)%momentum = [ 0.5_default*sqrt(shat), &
                       -sqrt(mi_scale)*cos(phi), &
                       -sqrt(mi_scale)*sin(phi), &
                       -sqrt(0.25_default*shat - mi_scale)]
                  partons(3)%belongstoFSR = .true.
                  partons(4)%belongstoFSR = .true.

                  call shower%add_interaction_2ton (parton_pointers)
                  n_int = size (shower%interactions)
                  do k = 1, 2
                     call mi%replace_parton &
                       (shower%interactions(n_int)%i%partons(k)%p%initial%nr, &
                        shower%interactions(n_int)%i%partons(k)%p%nr, &
                        shower%interactions(n_int)%i%partons(k)%p%parent%nr, &
                        shower%interactions(n_int)%i%partons(k)%p%type, &
                        shower%interactions(n_int)%i%partons(k)%p%x, &
                        mi_scale)
                  end do
                  call shower%write ()
               else
                  !!! execute the next branching 'found' in the previous step
                  call shower%execute_next_isr_branching (temppp)
                  if (shower_settings%muli_active) then
                     call mi%replace_parton (temppp%p%initial%nr, &
                          temppp%p%child1%nr, temppp%p%nr, &
                          temppp%p%type, temppp%p%x, ps_scale)
                  end if

               end if
            end do BRANCHINGS
               
            call shower%generate_fsr_for_isr_partons ()
         else
            if (signal_is_pending ()) return                
            call shower%simulate_no_isr_shower ()
         end if

         !!! some bookkeeping, needed after the shower is done
         call shower%boost_to_labframe ()
         call shower%generate_primordial_kt ()
         call shower%update_beamremnants ()
         !!! clean-up muli: we should finalize the muli pdf sets when 
         !!!      all runs are done. 
         ! call mi%finalize ()

         if (shower_settings%ps_fsr_active) then
            do i = 1, size (shower%interactions)
               if (signal_is_pending ()) return                   
               call shower%interaction_generate_fsr_2ton &
                    (shower%interactions(i)%i)
            end do
         else
            call shower%simulate_no_fsr_shower ()
         end if
         if (debug) then
            write (*, "(A)")  "SHOWER_FINISHED: "
            call shower%write ()
         end if
            
         if (shower_settings%mlm_matching) then
            !!! transfer momenta of the partons in the final state of 
            !!!        the hard initeraction
            if (signal_is_pending ()) return                
            if (allocated (JETS_ME))  deallocate (JETS_ME)
            call shower%get_final_colored_ME_partons (final_ME_partons)
            if (allocated (final_ME_partons)) then
               allocate (JETS_ME(1:size (final_ME_partons)))
               do i = 1, size (final_ME_partons)
                  !!! transfer
                  JETS_ME(i) = final_ME_partons(i)%p%momentum
               end do
               deallocate (final_ME_partons)
            end if
         end if

         u_S2W = free_unit ()
         if (debug .and. to_file) then
            open (unit=u_S2W, file="showerout.lhe", &
                 status="replace", action="readwrite")  
         else
            open (unit=u_S2W, status="scratch", action="readwrite")
         end if
         call shower%write_lhef (u_S2W)
         call shower_add_lhef_to_particle_set &
              (particle_set, u_S2W, model, model_hadrons)
         close (u_S2W)
         
         !!! move the particle data to the PYTHIA COMMON BLOCKS in case 
         !!! hadronization is active
         if (shower_settings%hadronization_active) then
            if (signal_is_pending ()) return                
            call shower_converttopythia (shower)
         end if
         deallocate (partons)
         deallocate (parton_pointers)
         exit TRY_SHOWER
      end do TRY_SHOWER
      if (debug) then
         call particle_set%write ()
         print *, &
           "----------------------apply_shower_particle_set------------------"
         print *, &
           "-----------------------------------------------------------------"         
         if (size (shower%interactions) >= 2) then
            call shower%write ()
         end if
      end if         
         
      call shower%final ()
      call shower_get_rng(evt%rng)
      !!! clean-up muli: we should finalize the muli pdf sets 
      !!!      when all runs are done. 
      ! call mi%finalize()
      return
    end subroutine apply_WHIZARDshower_particle_set

    subroutine get_ME_momenta_from_PYTHIA (JETS_ME)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      SAVE /PYJETS/

      type(vector4_t), dimension(:), allocatable :: JETS_ME
      real(kind=default), dimension(:,:), allocatable :: pdum 
      integer :: i, j, n_jets

      if (allocated (JETS_ME))  deallocate (JETS_ME)
      if (allocated (pdum))  deallocate (pdum)

      if (signal_is_pending ()) return          
      !!! final ME partons start in 7th row of event record
      i = 7
      !!! find number of jets
      n_jets = 0
      do
         if (K(I,1) /= 21) exit
         if ((K(I,2) == 21) .or. (abs(K(I,2)) <= 6)) then
            n_jets = n_jets + 1
         end if
         i = i + 1 
      end do

      if (n_jets == 0) return
      allocate (JETS_ME(1:n_jets))
      allocate (pdum(1:n_jets,4))

      !!! transfer jets
      i = 7
      j = 1
      pdum = p
      do
         if (K(I,1) /= 21) exit
         if ((K(I,2) == 21) .or. (abs(K(I,2)).le.6)) then
            JETS_ME(j)= vector4_moving (pdum(I,4), & 
              vector3_moving ( [pdum(I,1),pdum(I,2),pdum(I,3)] ))
            j = j + 1
         end if
         i = i + 1
      end do
    end subroutine get_ME_momenta_from_PYTHIA
    
    subroutine matching_transfer_PS &
         (data, particle_set, settings)
      !!! transfer partons after parton shower to data%P_PS
      type(mlm_matching_data_t), intent(inout) :: data
      type(particle_set_t), intent(in) :: particle_set
      type(shower_settings_t), intent(in) :: settings
      integer :: i, j, n_jets_PS
      integer, dimension(2) :: col
      type(particle_t) :: tempprt
      real(double) :: eta
      type(vector4_t) :: p_tmp      

      !!! loop over particles and extract final colored ones with eta<etamax
      n_jets_PS = 0
      do i = 1, particle_set%get_n_tot ()
         if (signal_is_pending ()) return             
         tempprt = particle_set%get_particle (i)
         if (tempprt%get_status () /= PRT_OUTGOING) cycle
         col = tempprt%get_color ()
         if (all (col == 0)) cycle
         if (data%is_hadron_collision) then
            p_tmp = tempprt%get_momentum ()
            if (energy (p_tmp) - longitudinal_part (p_tmp) < 1.E-10_default .or. &
                energy (p_tmp) + longitudinal_part (p_tmp) < 1.E-10_default) then
               eta = pseudorapidity (p_tmp)
            else
               eta = rapidity (p_tmp)
            end if
            if (eta > settings%ms%mlm_etaClusfactor * &
                 settings%ms%mlm_etamax)  then
               if (debug) then
                  print *, "REJECTING"
                  call tempprt%write ()
               end if
               cycle
            end if
         end if
         n_jets_PS = n_jets_PS + 1
      end do

      allocate (data%P_PS(1:n_jets_PS))
      if (debug)  write (*, "(A,1x,I0)")  "n_jets_ps =", n_jets_ps

      j = 1
      do i = 1, particle_set%get_n_tot ()
         tempprt = particle_set%get_particle (i)
         if (tempprt%get_status () /= PRT_OUTGOING) cycle
         col = tempprt%get_color ()
         if(all(col == 0)) cycle
         if (data%is_hadron_collision) then
            p_tmp = tempprt%get_momentum ()
            if (energy (p_tmp) - longitudinal_part (p_tmp) < 1.E-10_default .or. &
                energy (p_tmp) + longitudinal_part (p_tmp) < 1.E-10_default) then
               eta = pseudorapidity (p_tmp)
            else
               eta = rapidity (p_tmp)
            end if
            if (eta > settings%ms%mlm_etaClusfactor * &
                 settings%ms%mlm_etamax) cycle
         end if
         data%P_PS(j) = tempprt%get_momentum ()
         j = j + 1
      end do
    end subroutine matching_transfer_PS
    
    subroutine apply_PYTHIAhadronization &
         (particle_set, shower_settings, model, model_hadrons, valid)
      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      class(model_data_t), intent(in), target :: model
      class(model_data_t), intent(in), target :: model_hadrons
      logical, intent(inout) :: valid
      integer :: u_W2P, u_P2W
      type(string_t) :: remaining_PYGIVE, partial_PYGIVE
      logical, save :: msg_written = .false.
      character(len=10) :: buffer

      if (.not. shower_settings%hadronization_active)  return
      if (.not. valid) return
      if (signal_is_pending ()) return          

      u_W2P = free_unit ()
      if (debug) then
         open (unit=u_W2P, status="replace", file="whizardout.lhe", &
              action="readwrite") 
      else
         open (unit=u_W2P, status="scratch", action="readwrite")
      end if
      call shower_W2P_write_event (u_W2P)
      rewind (u_W2P)
      write (buffer, "(I10)")  u_W2P
      call PYGIVE ("MSTP(161)=" // buffer)
      call PYGIVE ("MSTP(162)=" // buffer)

      !!! Assume that the event is still present in the PYTHIA common blocks
      ! call pygive ("MSTP(61)=0")  ! switch off ISR
      ! call pygive ("MSTP(71)=0")  ! switch off FSR

      if (.not. shower_settings%ps_use_PYTHIA_shower .and. &
           len(shower_settings%ps_PYTHIA_PYGIVE) > 0) then
         remaining_PYGIVE = shower_settings%ps_PYTHIA_PYGIVE
         do while (len(remaining_PYGIVE) > 0)
            if (signal_is_pending ()) return                            
            call split (remaining_PYGIVE, partial_PYGIVE, ";")
            call PYGIVE (char (partial_PYGIVE))
         end do
         if (shower_get_PYTHIA_error () /= 0) then
            call msg_fatal ("PYTHIA didn't recognize ps_PYTHIA_PYGIVE setting")
         end if
      end if

      if (.not. (shower_settings%ps_use_PYTHIA_shower .and. &
           (shower_settings%ps_isr_active.or. &
           shower_settings%ps_fsr_active))) then
         if (len(shower_settings%ps_PYTHIA_PYGIVE) > 0) then
            remaining_PYGIVE = shower_settings%ps_PYTHIA_PYGIVE
            do while (len(remaining_PYGIVE) > 0)
               if (signal_is_pending ()) return                   
               call split (remaining_PYGIVE, partial_PYGIVE, ";")
               call PYGIVE (char(partial_PYGIVE))
            end do
            if (shower_get_PYTHIA_error () /= 0) then
               call msg_fatal &
                    ("PYTHIA did not recognize ps_PYTHIA_PYGIVE setting")
            end if
         end if
      end if

      if (.not.msg_written) then
         call msg_message &
              ("Using PYTHIA interface for hadronization and decays")
         msg_written = .true.
      end if

      call PYGIVE ("MSTP(111)=1") !!! switch on hadronization
      if (signal_is_pending ()) return          
      call PYEXEC

      if (shower_get_PYTHIA_error () > 0) then
         !!! clean up, discard shower and exit
         call shower_set_PYTHIA_error (0)
         close (u_W2P)
         valid = .false.
      else
         !!! convert back
         u_P2W = free_unit ()
         write (buffer, "(I10)")  u_P2W
         call PYGIVE ("MSTP(163)=" // buffer)
         if (debug .and. to_file) then
            open (unit=u_P2W, file="pythiaout2.lhe", status="replace", &
                 action="readwrite")  
         else            
            open (unit=u_P2W, status="scratch", action="readwrite")
         end if
         !!! convert pythia /PYJETS/ to lhef given in MSTU(163)=u1
         call pylheo
         !!! read and add lhef from u_P2W
         if (signal_is_pending ()) return             
         call shower_add_lhef_to_particle_set &
              (particle_set, u_P2W, model, model_hadrons)
         close (u_W2P)
         close (u_P2W)
         valid = .true.
      end if
    end subroutine apply_PYTHIAhadronization

    subroutine shower_W2P_write_event (unit)
      integer, intent(in) :: unit
      type(xml_tag_t), allocatable :: tag_lhef, tag_head, tag_init, &
           tag_event, tag_gen_n, tag_gen_v
      allocate (tag_lhef, tag_head, tag_init, tag_event, &
           tag_gen_n, tag_gen_v)
      call tag_lhef%init (var_str ("LesHouchesEvents"), &
         [xml_attribute (var_str ("version"), var_str ("1.0"))], .true.)
      call tag_head%init (var_str ("header"), .true.)
      call tag_init%init (var_str ("init"), .true.)
      call tag_event%init (var_str ("event"), .true.)
      call tag_gen_n%init (var_str ("generator_name"), .true.)
      call tag_gen_v%init (var_str ("generator_version"), .true.)      
      call tag_lhef%write (unit); write (unit, *)
      call tag_head%write (unit); write (unit, *)
      write (unit, "(2x)", advance = "no")
      call tag_gen_n%write (var_str ("WHIZARD"), unit)
      write (unit, *)
      write (unit, "(2x)", advance = "no")      
      call tag_gen_v%write (var_str ("2.2.5"), unit)
      write (unit, *)
      call tag_head%close (unit); write (unit, *)
      call tag_init%write (unit); write (unit, *)
      call heprup_write_lhef (unit)
      call tag_init%close (unit); write (unit, *)
      call tag_event%write (unit); write (unit, *)
      call hepeup_write_lhef (unit)
      call tag_event%close (unit); write (unit, *)
      call tag_lhef%close (unit); write (unit, *)
      deallocate (tag_lhef, tag_head, tag_init, tag_event, &
           tag_gen_n, tag_gen_v)
    end subroutine shower_W2P_write_event
  
  end subroutine apply_shower_particle_set
  subroutine shower_add_lhef_to_particle_set &
       (particle_set, u, model_in, model_hadrons)
    type(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: u
    class(model_data_t), intent(in), target :: model_in
    class(model_data_t), intent(in), target :: model_hadrons
    type(flavor_t) :: flv
    type(color_t) :: col
    class(model_data_t), pointer :: model
    type(particle_t), dimension(:), allocatable :: prt_tmp, prt
    integer :: i, j
    type(vector4_t) :: mom, d_mom
    integer, PARAMETER :: MAXLEN=200
    character(len=maxlen) :: string
    integer :: ibeg, n_tot, n_entries
    integer, dimension(:), allocatable :: relations, mothers
    INTEGER :: NUP,IDPRUP,IDUP,ISTUP
    real(kind=double) :: XWGTUP,SCALUP,AQEDUP,AQCDUP,VTIMUP,SPINUP
    integer :: MOTHUP(1:2), ICOLUP(1:2)
    real(kind=double) :: PUP(1:5)
    real(kind=default) :: pup_dum(1:5)
    character(len=5) :: buffer
    character(len=6) :: strfmt
    logical :: not_found
    STRFMT='(A000)'
    WRITE (STRFMT(3:5),'(I3)') MAXLEN

    rewind (u)

    do
       read (u,*, END=501, ERR=502) STRING
       IBEG = 0
       do
          if (signal_is_pending ()) return              
          IBEG = IBEG + 1
          ! Allow indentation.
          IF (STRING (IBEG:IBEG) .EQ. ' ' .and. IBEG < MAXLEN-6) cycle
          exit
       end do
       IF (string(IBEG:IBEG+6) /= '<event>' .and. &
            string(IBEG:IBEG+6) /= '<event ') cycle
       exit
    end do
    !!! Read first line of event info -> number of entries
    read (u, *, END=503, ERR=504) NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    n_tot = particle_set%get_n_tot ()
    allocate (prt_tmp (1:n_tot+NUP))
    allocate (relations (1:NUP), mothers (1:NUP))
    do i = 1, n_tot
       if (signal_is_pending ()) return           
       prt_tmp (i) = particle_set%get_particle (i)
       if (prt_tmp(i)%get_status () == PRT_OUTGOING .or. &
            prt_tmp(i)%get_status () == PRT_BEAM_REMNANT) then
          call prt_tmp(i)%reset_status (PRT_VIRTUAL)
       end if
    end do

    !!! transfer particles from lhef to particle_set
    !!!...Read NUP subsequent lines with information on each particle.
    n_entries = 1
    mothers = 0
    relations = 0    
    PARTICLE_LOOP: do I = 1, NUP
       read (u,*, END=200, ERR=505) IDUP, ISTUP, MOTHUP(1), MOTHUP(2), &
            ICOLUP(1), ICOLUP(2), (PUP (J),J=1,5), VTIMUP, SPINUP
       if (model_in%test_field (IDUP)) then
          model => model_in
       else if (model_hadrons%test_field (IDUP)) then
          model => model_hadrons
       else
          write (buffer, "(I5)") IDUP
          call msg_error ("Parton " // buffer // &
               " found neither in given model file nor in SM_hadrons")
          return
       end if
       call flv%init (IDUP, model)
       if (IABS(IDUP) == 2212 .or. IABS(IDUP) == 2112) then
          ! PYTHIA sometimes sets color indices for protons and neutrons (?)
          ICOLUP (1) = 0
          ICOLUP (2) = 0
       end if
       call col%init_col_acl (ICOLUP (1), ICOLUP (2))
       !!! Settings for unpolarized particles
       ! particle_set%prt (oldsize+i)%hel = ??
       ! particle_set%prt (oldsize+i)%pol = ??
       if (MOTHUP(1) /= 0) then
          mothers(i) = MOTHUP(1)
       end if
       pup_dum = PUP
       if (pup_dum(4) < 1E-10_default)  cycle
       mom = vector4_moving (pup_dum (4), &
            vector3_moving ([pup_dum (1), pup_dum (2), pup_dum (3)]))
       not_found = .true.
       SCAN_PARTICLES: do j = 1, n_tot
          d_mom = prt_tmp(j)%get_momentum () - mom
          if (abs(d_mom**1) < 1E-8_default .and. &               
                (prt_tmp(j)%get_pdg () == IDUP)) then
             not_found = .false.             
             if (.not. prt_tmp(j)%get_status () == PRT_BEAM .or. &
                  .not. prt_tmp(j)%get_status () == PRT_BEAM_REMNANT) &
                  relations(i) = j
          end if
       end do SCAN_PARTICLES               
       if (not_found) then
          call prt_tmp(n_tot+n_entries)%set_flavor (flv)    
          call prt_tmp(n_tot+n_entries)%set_color (col)
          call prt_tmp(n_tot+n_entries)%set_momentum (mom)
          if (MOTHUP(1) /= 0) then 
             if (relations(MOTHUP(1)) /= 0) then
                call prt_tmp(n_tot+n_entries)%set_parents &
                     ([relations(MOTHUP(1))])             
                call prt_tmp(relations(MOTHUP(1)))%add_child (n_tot+n_entries)
                if (prt_tmp(relations(MOTHUP(1)))%get_status () &
                     == PRT_OUTGOING) &
                     call prt_tmp(relations(MOTHUP(1)))%reset_status &
                     (PRT_VIRTUAL)
             end if
          end if
          call prt_tmp(n_tot+n_entries)%set_status (PRT_OUTGOING)
          n_entries = n_entries + 1
       end if
    end do PARTICLE_LOOP

    allocate (prt (1:n_tot+n_entries-1))
    prt = prt_tmp (1:n_tot+n_entries-1)
    ! transfer to particle_set
    call particle_set%replace (prt)
    deallocate (prt, prt_tmp)

200 continue
    return

501 write(*,*) "READING LHEF failed 501"
    return
502 write(*,*) "READING LHEF failed 502"
    return
503 write(*,*) "READING LHEF failed 503"
    return
504 write(*,*) "READING LHEF failed 504"
    return
505 write(*,*) "READING LHEF failed 505"
    return
  end subroutine shower_add_lhef_to_particle_set
!!!!!!!!!!PYTHIA STYLE!!!!!!!!!!!!!
!!! originally PYLHEF subroutine from PYTHIA 6.4.22

  !C...Write out the showered event to a Les Houches Event File.
  !C...Take MSTP(161) as the input for <init>...</init>

  subroutine pylheo ()

  !C...Double precision and integer declarations.
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)

    !C...PYTHIA commonblock: only used to provide read/write units and version.
    common /PYPARS/ MSTP(200), PARP(200), MSTI(200), PARI(200)
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYPARS/
    save /PYJETS/

    !C...User process initialization commonblock.
    !C...User process event common block.    
    integer, parameter :: MAXPUP = 100, MAXNUP = 500
    integer :: IDBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP, LPRUP
    integer :: NUP, IDPRUP, IDUP, ISTUP, MOTHUP, ICOLUP
    real(double) :: EBMUP, XSECUP, XERRUP, XMAXUP
    real(double) :: XWGTUP, SCALUP, AQEDUP, AQCDUP, PUP, VTIMUP, SPINUP    
    integer, parameter :: KSUSY1 = 1000000, KSUSY2 = 2000000
    common /HEPRUP/ &
         IDBMUP(2), EBMUP(2), PDFGUP(2), PDFSUP(2), IDWTUP, NPRUP, &
         XSECUP(MAXPUP), XERRUP(MAXPUP), XMAXUP(MAXPUP), LPRUP(MAXPUP)
    save /HEPRUP/
    common /HEPEUP/ &
         NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP, IDUP(MAXNUP), &
         ISTUP(MAXNUP), MOTHUP(2,MAXNUP), ICOLUP(2,MAXNUP), &
         PUP(5,MAXNUP), VTIMUP(MAXNUP), SPINUP(MAXNUP)
    save /HEPEUP/
    
    !C...Lines to read in assumed never longer than 200 characters.
    PARAMETER (MAXLEN=200)
    character(len=maxlen) :: string
    
    integer :: LEN, ndangling_color, ndangling_antic, ncolor
    
    !C...Format for reading lines.
    character(len=6) :: strfmt
    STRFMT='(A000)'
    write (STRFMT(3:5),'(I3)') MAXLEN

    !C...Rewind initialization and event files.
    rewind MSTP(161)
    rewind MSTP(162)

    !C...Write header info.
    write (MSTP(163), "(A)")  '<LesHouchesEvents version="1.0">'
    write (MSTP(163), "(A)")  "<!--"
    write (MSTP(163), "(A,I1,A1,I3)")  "File generated with PYTHIA ", &
         MSTP(181), ".", MSTP(182)
    write (MSTP(163), "(A)")  " and the WHIZARD2 interface"
    write (MSTP(163), "(A)")  "-->"

    !C...Loop until finds line beginning with "<init>" or "<init ".
100 READ(MSTP(161),STRFMT,END=400,ERR=400) STRING
    IBEG=0
110 IBEG=IBEG+1
    !C...Allow indentation.
    IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-5) GOTO 110
    IF(STRING(IBEG:IBEG+5).NE.'<init>'.AND.STRING(IBEG:IBEG+5).NE.'<init ') GOTO 100
    
    !C...Read first line of initialization info and get number of processes.
    READ(MSTP(161),'(A)',END=400,ERR=400) STRING
    READ(STRING,*,ERR=400) IDBMUP(1),IDBMUP(2),EBMUP(1),EBMUP(2),PDFGUP(1),PDFGUP(2),PDFSUP(1),PDFSUP(2),IDWTUP,NPRUP

    !C...Copy initialization lines, omitting trailing blanks.
    !C...Embed in <init> ... </init> block.
    WRITE(MSTP(163),'(A)') '<init>'
    do IPR = 0, NPRUP
       IF(IPR.GT.0) READ(MSTP(161),'(A)',END=400,ERR=400) STRING
       LEN=MAXLEN+1
120    LEN=LEN-1
       IF(LEN.GT.1.AND.STRING(LEN:LEN).EQ.' ') GOTO 120
       WRITE(MSTP(163),'(A)',ERR=400) STRING(1:LEN)
    end DO
    write (MSTP(163), "(A)")  "</init>"
    
    !!! Find the numbers of entries of the <event block>
    NENTRIES = 0
    do I = 1, N
       if (K(I,1) == 1 .or. K(I,1) == 2 .or. K(I,1) == 21) then
          NENTRIES = NENTRIES + 1
       end if
    end do
    
    !C...Begin an <event> block. Copy event lines, omitting trailing blanks.
    write (MSTP(163), "(A)")  "<event>"
    write (MSTP(163), *)  NENTRIES, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    
    ndangling_color = 0
    ncolor = 0
    ndangling_antic = 0
    NANTIC = 0
    NNEXTC = 1   ! TODO find next free color number ??
    do I = 1, N
       if (signal_is_pending ()) return             
       if ((K(I,1) >= 1 .and. K(I,1) <= 15) .or. (K(I,1) == 21)) then
          if ((K(I,2).eq.21) .or. (IABS(K(I,2)) <= 8) .or. &
               (IABS(K(I,2)) >= KSUSY1+1 .and. IABS(K(I,2)) <= KSUSY1+8) &
               .or. &
               (IABS(K(I,2)) >= KSUSY2+1 .and. IABS(K(I,2)) <= KSUSY2+8) .or. &
               (IABS(K(I,2)) >= 1000 .and. IABS(K(I,2)) <= 9999) ) then
             if (ndangling_color.eq.0 .and. ndangling_antic.eq.0) then
                ! new color string
                ! Gluon and gluino only color octets implemented so far
                if (K(I,2).eq.21 .or. K(I,2).eq.1000021) then  
                   ncolor = NNEXTC
                   ndangling_color = ncolor
                   NNEXTC = NNEXTC + 1
                   NANTIC = NNEXTC
                   ndangling_antic = NANTIC
                   NNEXTC = NNEXTC + 1
                else if (K(I,2) .gt. 0) then  ! particles to have color
                   ncolor = NNEXTC
                   ndangling_color = ncolor
                   NANTIC = 0
                   NNEXTC = NNEXTC + 1
                else if (K(I,2) .lt. 0) then  ! antiparticles to have anticolor
                   NANTIC = NNEXTC
                   ndangling_antic = NANTIC
                   ncolor = 0
                   NNEXTC = NNEXTC + 1
                end if
             else if(K(I,1).eq.1) then
                ! end of string
                ncolor = ndangling_antic
                NANTIC = ndangling_color
                ndangling_color = 0
                ndangling_antic = 0
             else
                ! inside the string
                if(ndangling_color .ne. 0) then
                   NANTIC = ndangling_color
                   ncolor = NNEXTC
                   ndangling_color = NNEXTC
                   NNEXTC = NNEXTC +1
                else if(ndangling_antic .ne. 0) then
                   ncolor = ndangling_antic
                   NANTIC = NNEXTC
                   ndangling_antic = NNEXTC
                   NNEXTC = NNEXTC +1
                else
                   print *, "ERROR IN PYLHEO"
                end if
             end if
          else
             ncolor = 0
             NANTIC = 0
          end if
       !!! As no intermediate are given out here, assume the 
       !!!   incoming partons to be the mothers
          write (MSTP(163),*)  K(I,2), K(I,1), K(I,3), K(I,3), &
               ncolor, NANTIC, (P(I,J),J=1,5), 0, -9
       end if
    end do
    
    !C..End the <event> block. Loop back to look for next event.
    write (MSTP(163), "(A)")  "</event>"
      
    !C...Successfully reached end of event loop: write closing tag
    !C...and remove temporary intermediate files (unless asked not to).
    write (MSTP(163), "(A)")  "</LesHouchesEvents>"
    return
      
    !!C...Error exit.
400 write(*,*) ' PYLHEO file joining failed!'
    
    return
  end subroutine pylheo

  subroutine evt_shower_write (object, unit, testflag)
    class(evt_shower_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Event transform: shower"
    call write_separator (u)
    call object%base_write (u, testflag = testflag)
    call write_separator (u)
    call object%settings%write (u)
  end subroutine evt_shower_write
    
  subroutine evt_shower_init (evt, settings, model_hadrons, os_data)
    class(evt_shower_t), intent(out) :: evt
    type(shower_settings_t), intent(in) :: settings
    type(model_t), intent(in), target :: model_hadrons
    type(os_data_t), intent(in) :: os_data
    evt%settings = settings
    evt%os_data = os_data
    evt%model_hadrons => model_hadrons
  end subroutine evt_shower_init
  
  subroutine evt_shower_setup_pdf (evt, process, beam_structure)
    class(evt_shower_t), intent(inout) :: evt
    type(process_t), intent(in) :: process
    type(beam_structure_t), intent(in) :: beam_structure
    if (beam_structure%contains ("lhapdf")) then
       if (LHAPDF6_AVAILABLE) then
          evt%pdf_type = STRF_LHAPDF6
       else if (LHAPDF5_AVAILABLE) then
          evt%pdf_type = STRF_LHAPDF5
       end if
       evt%pdf_set = process%get_pdf_set ()
       write (msg_buffer, "(A,I0)")  "Shower: interfacing LHAPDF set #", &
            evt%pdf_set
       call msg_message ()
    else if (beam_structure%contains ("pdf_builtin")) then
       evt%pdf_type = STRF_PDF_BUILTIN
       evt%pdf_set = process%get_pdf_set ()
       write (msg_buffer, "(A,I0)")  "Shower: interfacing PDF builtin set #", &
            evt%pdf_set
       call msg_message ()
    end if
  end subroutine evt_shower_setup_pdf
    
  subroutine evt_shower_prepare_new_event (evt, i_mci, i_term)
    class(evt_shower_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
  end subroutine evt_shower_prepare_new_event

  subroutine evt_shower_generate_weighted (evt, probability)
    class(evt_shower_t), intent(inout) :: evt
    real(default), intent(out) :: probability
    logical :: valid, vetoed
    valid = .true.
    vetoed = .false.
    if (evt%previous%particle_set_exists) then
       evt%particle_set = evt%previous%particle_set
       if (evt%settings%ps_use_PYTHIA_shower .or. &
           evt%settings%hadronization_active) then
          call evt%assure_heprup ()
       end if
       if (evt%settings%ckkw_matching) then
          call ckkw_pseudo_shower_weights_init (evt%settings%ckkw_weights)
          call ckkw_fake_pseudo_shower_weights (evt%settings%ckkw_settings, &
               evt%settings%ckkw_weights, evt%particle_set)
       end if
       call apply_shower_particle_set (evt, valid, vetoed)
       probability = 1
       !!! BCN: WK please check: In 2.1.1 vetoed events reduced sim%n_events by
       ! one while invalid events did not. This bookkeeping should be reenabled
       ! to have the correct cross section / luminosity.
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

  subroutine event_shower_assure_heprup (evt)
    class(evt_shower_t), intent(in) :: evt
    type(particle_t), dimension(2) :: prt
    integer :: i, num_id
    integer, parameter :: min_processes = 10

    num_id = 1
    if (LPRUP (num_id) /= 0)  return

    do i = 1, 2
       prt(i) = evt%particle_set%get_particle (i)
    end do
    call heprup_init ( &
         [ prt(1)%get_pdg (), prt(2)%get_pdg () ] , &
         [ vector4_get_component (prt(1)%get_momentum (), 0), &
           vector4_get_component (prt(2)%get_momentum (), 0) ], &
           num_id, .false., .false.)
    do i = 1, (num_id / min_processes + 1) * min_processes
       call heprup_set_process_parameters (i = i, process_id = &
            i, cross_section = 1._default, error = 1._default)
    end do
  end subroutine event_shower_assure_heprup

  subroutine ckkw_fake_pseudo_shower_weights &
       (ckkw_pseudo_shower_settings, &
        ckkw_pseudo_shower_weights, particle_set)
    type(ckkw_matching_settings_t), intent(inout) :: &
         ckkw_pseudo_shower_settings
    type(ckkw_pseudo_shower_weights_t), intent(inout) :: &
         ckkw_pseudo_shower_weights
    type(particle_set_t), intent(in) :: particle_set
    type(particle_t) :: prt
    integer :: i, j
    integer :: n
    type(vector4_t) :: momentum

    ckkw_pseudo_shower_settings%alphaS = 1.0_default
    ckkw_pseudo_shower_settings%Qmin = 1.0_default
    ckkw_pseudo_shower_settings%n_max_jets = 3

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
    call model_list%read_model (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
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
    end select

    call evt_shower%connect (process_instance, model)
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
    !    call model_list%final ()    ! no, would deallocate model twice
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
    call model_list%read_model (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
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

    settings%ps_fsr_active = .true.

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (settings, model_hadrons, os_data)
    end select

    call evt_shower%connect (process_instance, model)
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
    !    call model_list%final ()    ! no, would deallocate model twice
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_2"
    
  end subroutine shower_2
  

end module shower
