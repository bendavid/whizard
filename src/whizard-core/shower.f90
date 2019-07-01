! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  use kinds, only: default, double !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use constants, only : pi, twopi !NODEP!  
  use limits, only: LF !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!  
  use shower_base !NODEP!
  use shower_partons !NODEP!
  use shower_core !NODEP!
  use shower_topythia !NODEP!
  use muli, muli_output_unit => output_unit !NODEP!
  use mlm_matching !NODEP!
  use ckkw_pseudo_weights !NODEP!
  use ckkw_matching !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!

  use unit_tests
  use os_interface
  use xml
  
  use sm_qcd
  use flavors
  use colors
  use particles
  use state_matrices
  use subevents
  use models
  use variables
  use beam_structures
  use hep_common
  use process_libraries
  use prc_core
  use prc_omega
  use rng_base
  use rng_tao
  use mci_base
  use mci_midpoint
  use phs_base
  use phs_single
  use processes
  use event_transforms

  implicit none
  private

  public :: shower_settings_t
  public :: apply_shower_particle_set
  public :: evt_shower_t
  public :: ckkw_fake_pseudo_shower_weights
  public :: shower_test

  integer, parameter :: STRF_NONE = 0
  integer, parameter :: STRF_LHAPDF = 1
  integer, parameter :: STRF_PDF_BUILTIN = 2
  

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
         var_list_get_lval (var_list, var_str ("?ps_isr_active"))
    shower_settings%ps_fsr_active = &
         var_list_get_lval (var_list, var_str ("?ps_fsr_active"))
    shower_settings%hadronization_active = &
         var_list_get_lval (var_list, var_str ("?hadronization_active"))
    shower_settings%mlm_matching = &
         var_list_get_lval (var_list, var_str ("?mlm_matching"))
    shower_settings%ckkw_matching = & 
         var_list_get_lval (var_list, var_str ("?ckkw_matching"))
    shower_settings%muli_active = &
         var_list_get_lval (var_list, var_str ("?muli_active"))

    if (.not. shower_settings%ps_fsr_active .and. &
        .not. shower_settings%ps_isr_active .and. &
        .not. shower_settings%hadronization_active .and. &
        .not. shower_settings%mlm_matching) then
       return
    end if

    shower_settings%ps_use_PYTHIA_shower = &
         var_list_get_lval (var_list, var_str ("?ps_use_PYTHIA_shower"))
    shower_settings%ps_PYTHIA_verbose = &
         var_list_get_lval (var_list, var_str ("?ps_PYTHIA_verbose"))
    shower_settings%ps_PYTHIA_PYGIVE = &
         var_list_get_sval (var_list, var_str ("$ps_PYTHIA_PYGIVE"))
    shower_settings%ps_mass_cutoff = &
         var_list_get_rval (var_list, var_str ("ps_mass_cutoff"))
    shower_settings%ps_fsr_lambda = &
         var_list_get_rval (var_list, var_str ("ps_fsr_lambda"))
    shower_settings%ps_isr_lambda = &
         var_list_get_rval (var_list, var_str ("ps_isr_lambda"))
    shower_settings%ps_max_n_flavors = &
         var_list_get_ival (var_list, var_str ("ps_max_n_flavors"))
    shower_settings%ps_isr_alpha_s_running = &
         var_list_get_lval (var_list, var_str ("?ps_isr_alpha_s_running"))
    shower_settings%ps_fsr_alpha_s_running = &
         var_list_get_lval (var_list, var_str ("?ps_fsr_alpha_s_running"))
    shower_settings%ps_fixed_alpha_s = &
         var_list_get_rval (var_list, var_str ("ps_fixed_alpha_s"))
    shower_settings%ps_isr_pt_ordered = &
         var_list_get_lval (var_list, var_str ("?ps_isr_pt_ordered"))
    shower_settings%ps_isr_angular_ordered = &
         var_list_get_lval (var_list, var_str ("?ps_isr_angular_ordered"))
    shower_settings%ps_isr_primordial_kt_width = &
         var_list_get_rval (var_list, var_str ("ps_isr_primordial_kt_width"))
    shower_settings%ps_isr_primordial_kt_cutoff = &
         var_list_get_rval (var_list, var_str ("ps_isr_primordial_kt_cutoff"))
    shower_settings%ps_isr_z_cutoff = &
         var_list_get_rval (var_list, var_str ("ps_isr_z_cutoff"))
    shower_settings%ps_isr_minenergy = &
         var_list_get_rval (var_list, var_str ("ps_isr_minenergy"))
    shower_settings%ps_isr_tscalefactor = &
         var_list_get_rval (var_list, var_str ("ps_isr_tscalefactor"))
    shower_settings%ps_isr_only_onshell_emitted_partons = &
         var_list_get_lval (var_list, &
         var_str ("?ps_isr_only_onshell_emitted_partons"))

    !!! MLM matching
    shower_settings%ms%mlm_Qcut_ME = &
         var_list_get_rval(var_list, var_str ("mlm_Qcut_ME"))
    shower_settings%ms%mlm_Qcut_PS = &
         var_list_get_rval(var_list, var_str ("mlm_Qcut_PS"))
    shower_settings%ms%mlm_ptmin = &
         var_list_get_rval(var_list, var_str ("mlm_ptmin"))
    shower_settings%ms%mlm_etamax = &
         var_list_get_rval(var_list, var_str ("mlm_etamax"))
    shower_settings%ms%mlm_Rmin = &
         var_list_get_rval(var_list, var_str ("mlm_Rmin"))
    shower_settings%ms%mlm_Emin = &
         var_list_get_rval(var_list, var_str ("mlm_Emin"))
    shower_settings%ms%mlm_nmaxMEjets = &
         var_list_get_ival(var_list, var_str ("mlm_nmaxMEjets"))

    shower_settings%ms%mlm_ETclusfactor = &
         var_list_get_rval(var_list, var_str ("mlm_ETclusfactor"))
    shower_settings%ms%mlm_ETclusminE = &
         var_list_get_rval(var_list, var_str ("mlm_ETclusminE"))
    shower_settings%ms%mlm_etaclusfactor = &
         var_list_get_rval(var_list, var_str ("mlm_etaclusfactor"))
    shower_settings%ms%mlm_Rclusfactor = &
         var_list_get_rval(var_list, var_str ("mlm_Rclusfactor"))
    shower_settings%ms%mlm_Eclusfactor = &
         var_list_get_rval(var_list, var_str ("mlm_Eclusfactor"))

    !!! CKKW matching
    ! TODO
  end subroutine shower_settings_init

  subroutine shower_settings_write (object, unit)
    class(shower_settings_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
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
       (particle_set, shower_settings,  model, &
        os_data, pdf_type, pdf_set, valid, vetoed)
    type(particle_set_t), intent(inout) :: particle_set
    type(shower_settings_t), intent(in) :: shower_settings
    type(model_t), pointer, intent(in) :: model
    type(os_data_t), intent(in) :: os_data
    integer, intent(in) :: pdf_type
    integer, intent(in) :: pdf_set
    logical, intent(inout) :: valid
    logical, intent(inout) :: vetoed
    real(kind=double) :: pdftest
    logical, parameter :: debug = .false., to_file = .false.

    type(mlm_matching_data_t) :: mlm_matching_data
    logical, save :: matching_disabled=.false.
    procedure(shower_pdf), pointer :: pdf_func => null()

    interface
       subroutine evolvePDFM (set, x, q, ff)
         integer, intent(in) :: set
         double precision, intent(in) :: x, q
         double precision, dimension(-6:6), intent(out) :: ff
       end subroutine evolvePDFM
    end interface

    if (.not. shower_settings%ps_fsr_active .and. &
        .not. shower_settings%ps_isr_active .and. &
        .not. shower_settings%hadronization_active .and. &
        .not. shower_settings%mlm_matching) then
       ! return if nothing to do
       return
    end if

    ! return if already invalid or vetoed
    if (.not. valid .or. vetoed)  return

    if (signal_is_pending ()) return    
    
    ! ensure that lhapdf is initialized
    if (pdf_type .eq. STRF_LHAPDF) then       
       if (shower_settings%ps_isr_active .and. &
            (abs (particle_get_pdg (particle_set_get_particle &
                 (particle_set, 1))) >= 1000) .and. &
            (abs (particle_get_pdg (particle_set_get_particle &
                 (particle_set, 2))) >= 1000)) then
          call GetQ2max (0,pdftest)
          if (pdftest == 0._double) then
             call msg_fatal ("ISR QCD shower enabled, but LHAPDF not" // &
                  "initialized," // LF // "     aborting simulation")
             return
          end if
       end if
       pdf_func => evolvePDFM
    else if (pdf_type == STRF_PDF_BUILTIN) then
       if (shower_settings%ps_use_PYTHIA_shower) then
          call msg_fatal ("Builtin PDFs cannot be used for PYTHIA showers," &
               // LF // "     aborting simulation")
          return
       end if
       pdf_func => pdf_evolve_LHAPDF
    end if
    if (shower_settings%mlm_matching .and. shower_settings%ckkw_matching) then
       call msg_fatal ("Both MLM and CKKW matching activated," // &
            LF // "     aborting simulation")
       return      
    end if

    if (debug)  call shower_settings%write ()
    
    if (shower_settings%ps_use_PYTHIA_shower .or. &
         shower_settings%hadronization_active) then
       if (.not. shower_settings%ps_PYTHIA_verbose) then
          call PYGIVE ('MSTU(12)=12345')
          call PYGIVE ('MSTU(13)=0')
       else 
          call PYGIVE ('MSTU(13)=1')
       end if
    end if
    if (debug)  print *, "Shower: beam checks"

    if (.not. matching_disabled) then
       !!! Check if the beams are hadrons
       if ((abs (particle_get_pdg (particle_set_get_particle &
            (particle_set, 1))) <= 18) .and. &
            (abs (particle_get_pdg (particle_set_get_particle &
            (particle_set, 2))) <= 18)) then
          mlm_matching_data%is_hadron_collision = .false.
       else if ((abs (particle_get_pdg (particle_set_get_particle &
            (particle_set, 1))) >= 1000) .and. &
            (abs (particle_get_pdg (particle_set_get_particle &
            (particle_set, 2))) >= 1000)) then
          mlm_matching_data%is_hadron_collision = .true.
       else 
          call msg_error (" Matching didn't recognize beams setup," // &
               LF // "     disabling matching")
          matching_disabled = .true.
          return
       end if
    end if
    
    !!! SHOWER
    if (shower_settings%ps_use_PYTHIA_shower .or. &
         (.not. shower_settings%ps_fsr_active .and. &
          .not. shower_settings%ps_isr_active .and. &
           shower_settings%hadronization_active)) then
       call apply_PYTHIAshower_particle_set (particle_set, &
            shower_settings, mlm_matching_data%P_ME, model, valid)
       if (debug)  call pylist(2)
    else
       call apply_WHIZARDshower_particle_set (particle_set, &
            shower_settings, mlm_matching_data%P_ME, model, &
            os_data, pdf_func, pdf_set, valid, vetoed)
            if (vetoed) return
    end if
    if (debug) then
       call particle_set_write (particle_set)
       print *, " after SHOWER"
    end if
       
    if (shower_settings%mlm_matching .and. &
         (matching_disabled.eqv..false.)) then
    !!! MLM stage 2 -> PS jets and momenta
       call matching_transfer_PS &
            (mlm_matching_data, particle_set, shower_settings)
    !!! MLM stage 3 -> reconstruct and possible reject
       call mlm_matching_apply (mlm_matching_data, shower_settings%ms, vetoed)
       if (vetoed) then
          call mlm_matching_data_final (mlm_matching_data)
          return
       end if
    endif

!!! HADRONIZATION
    if (shower_settings%hadronization_active) then
       !! Assume that the event record is still in the PYTHIA COMMON BLOCKS
       !! transferred there by one of the shower routines
       if (valid) then
          call apply_PYTHIAhadronization (particle_set, &
               shower_settings, model, valid)
       end if
    end if
!!! FINAL

    call mlm_matching_data_final (mlm_matching_data)

    if (debug)  print *, "SHOWER+HAD+MATCHING finished"

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
         (particle_set, shower_settings, JETS_ME, model, valid)
      integer, parameter :: MAXNUP = 500
      integer, parameter :: MAXPUP = 100
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
      

      type(particle_set_t), intent(inout) :: particle_set
      type(particle_set_t) :: pset_reduced
      type(shower_settings_t), intent(in) :: shower_settings
      type(vector4_t), dimension(:), allocatable, intent(inout) :: JETS_ME
      type(model_t), pointer, intent(in) :: model
      logical, intent(inout) :: valid
      real(kind=default) :: rand
      
      ! units for transfer from WHIZARD to PYTHIA and back
      integer :: u_W2P, u_P2W
      integer, save :: pythia_initialized_for_NPRUP = 0
      logical, save :: pythia_warning_given = .false.
      logical, save :: msg_written = .false.
      type(string_t) :: remaining_PYGIVE, partial_PYGIVE
      character*10 buffer

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
      
      call particle_set_reduce (particle_set, pset_reduced)
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
            CALL PYGIVE ('MSTP(62)=2')
            CALL PYGIVE ('MSTP(67)=0')
         end if
         if (debug)  print *, "calling pyinit"
         call PYINIT ("USER", "", "", 0D0)

         call tao_random_number (rand)
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
           (particle_set, u_P2W, model, os_data)
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
         (particle_set, shower_settings, JETS_ME, model_in, &
         os_data, pdf_func, pdf_set, valid, vetoed)
      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      type(vector4_t), dimension(:), allocatable, intent(inout) :: JETS_ME
      type(model_t), pointer, intent(in) :: model_in
      type(os_data_t), intent(in) :: os_data
      procedure(shower_pdf), pointer, intent(in) :: pdf_func
      integer, intent(in) :: pdf_set
      logical, intent(inout) :: valid
      logical, intent(out) :: vetoed

      type(muli_type),save :: mi
      type(shower_t) :: shower
      type(parton_t), dimension(:), allocatable, target :: partons, hadrons
      type(parton_pointer_t), dimension(:), allocatable :: &
           parton_pointers, final_ME_partons
      real(kind=default) :: mi_scale, ps_scale, shat, phi
      type(parton_pointer_t) :: temppp
      integer, dimension(:), allocatable :: connections
      integer :: n_loop, i, j, k
      integer :: n_hadrons, n_in, n_out
      integer :: n_int
      integer :: max_color_nr
      integer, dimension(2) :: col_array
      integer, dimension(1) :: parent
      type(flavor_t) :: flv
      type(color_t) :: col
      type(model_t), pointer :: model
      type(model_t), target, save :: model_SM_hadrons
      logical, save :: model_SM_hadrons_associated = .false.
      logical, save :: msg_written = .false.
      logical :: exist_SM_hadrons
      type(string_t) :: filename
      integer, dimension(2,4) :: color_corr
      integer :: colori, colorj
      character*5 buffer
      integer :: u_S2W

      vetoed = .false.

      if (signal_is_pending ()) return          
      
      ! transfer settings from shower_settings to shower
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
      call shower_set_isr_angular_ordered &
           (shower_settings%ps_isr_angular_ordered)
      Call shower_set_primordial_kt_width &
           (shower_settings%ps_isr_primordial_kt_width)
      call shower_set_primordial_kt_cutoff &
           (shower_settings%ps_isr_primordial_kt_cutoff)
      call shower_set_maxz_isr (shower_settings%ps_isr_z_cutoff)
      call shower_set_minenergy_timelike (shower_settings%ps_isr_minenergy)
      call shower_set_tscalefactor_isr (shower_settings%ps_isr_tscalefactor)
      call shower_set_isr_only_onshell_emitted_partons &
           (shower_settings%ps_isr_only_onshell_emitted_partons)
      call shower_set_pdf_set (pdf_set)
      call shower_set_pdf_func (pdf_func)

      if (.not.msg_written) then
         call msg_message ("Using WHIZARD's internal showering")
         msg_written = .true.
      end if

      n_loop = 0
      TRY_SHOWER: do ! just a loop to be able to discard events
         n_loop = n_loop + 1
         if (n_loop > 1000) call msg_fatal &
              ("Shower: too many loops (try_shower)")
         call shower%create ()
         if (signal_is_pending ()) return             
         max_color_nr = 0

         n_hadrons = 0
         n_in = 0
         n_out = 0
         do i = 1, particle_set_get_n_tot (particle_set)
            if (particle_get_status (particle_set_get_particle &
                 (particle_set, i)) == PRT_BEAM) &
                 n_hadrons = n_hadrons + 1
            if (particle_get_status (particle_set_get_particle &
                 (particle_set, i)) == PRT_INCOMING) &
                 n_in = n_in + 1
            if (particle_get_status (particle_set_get_particle &
                 (particle_set, i)) == PRT_OUTGOING) &
                 n_out = n_out + 1
         end do

         allocate (connections (1:particle_set_get_n_tot (particle_set)))
         connections = 0

         allocate (hadrons (1:2))
         allocate (partons (1:n_in+n_out))
         allocate (parton_pointers (1:n_in+n_out))

         j=0
         if (n_hadrons > 0) then
            ! Transfer hadrons
            do i = 1, particle_set_get_n_tot (particle_set)
               if (particle_get_status (particle_set_get_particle &
                    (particle_set, i)) == PRT_BEAM) then
                  j = j+1
                  hadrons(j)%nr = shower%get_next_free_nr ()
                  hadrons(j)%momentum = particle_get_momentum &
                       (particle_set_get_particle (particle_set, i))
                  hadrons(j)%t = hadrons(j)%momentum**2
                  hadrons(j)%type = particle_get_pdg &
                       (particle_set_get_particle (particle_set, i))
                  col_array=particle_get_color (particle_set_get_particle &
                       (particle_set, i))
                  hadrons(j)%c1 = col_array(1)
                  hadrons(j)%c2 = col_array(2)
                  max_color_nr = max (max_color_nr, abs(hadrons(j)%c1), &
                       abs(hadrons(j)%c2))
                  hadrons(j)%interactionnr = 1
                  connections(i)=j
               end if
            end do
         end if

         ! transfer incoming partons
         j = 0
         do i = 1, particle_set_get_n_tot (particle_set)
            if (particle_get_status (particle_set_get_particle &
                 (particle_set, i)) == PRT_INCOMING) then
               j = j+1
               partons(j)%nr = shower%get_next_free_nr ()
               partons(j)%momentum = particle_get_momentum &
                    (particle_set_get_particle (particle_set, i))
               partons(j)%t = partons(j)%momentum**2
               partons(j)%type = particle_get_pdg &
                    (particle_set_get_particle (particle_set, i))
               col_array=particle_get_color &
                    (particle_set_get_particle (particle_set, i))
               partons(j)%c1 = col_array (1)
               partons(j)%c2 = col_array (2)
               parton_pointers(j)%p => partons(j)
               max_color_nr = max (max_color_nr, abs (partons(j)%c1), &
                    abs (partons(j)%c2))
               connections(i)=j
               ! insert dependences on hadrons
               if (particle_get_n_parents (particle_set_get_particle &
                    (particle_set, i))==1) then
                  parent = particle_get_parents (particle_set_get_particle &
                       (particle_set, i))
                  partons(j)%initial => hadrons (connections (parent(1)))
                  partons(j)%x = space_part_norm (partons(j)%momentum) / &
                                 space_part_norm (partons(j)%initial%momentum)
               end if
            end if
         end do
         if (signal_is_pending ()) return             
         !!! transfer outgoing partons
         do i = 1, particle_set_get_n_tot (particle_set)
            if (particle_get_status (particle_set_get_particle &
                 (particle_set, i)) == PRT_OUTGOING) then
               j = j + 1
               partons(j)%nr = shower%get_next_free_nr ()
               partons(j)%momentum = particle_get_momentum &
                    (particle_set_get_particle (particle_set, i))
               partons(j)%t = partons(j)%momentum**2
               partons(j)%type = particle_get_pdg &
                    (particle_set_get_particle (particle_set, i))
               col_array=particle_get_color &
                    (particle_set_get_particle (particle_set, i))
               partons(j)%c1 = col_array(1)
               partons(j)%c2 = col_array(2)
               parton_pointers(j)%p => partons(j)
               max_color_nr = max (max_color_nr, abs &
                    (partons(j)%c1), abs (partons(j)%c2))
               connections(i) = j
            end if
         end do

         deallocate (connections)

         ! insert these partons in shower
         call shower%set_next_color_nr (1 + max_color_nr)
         call shower%add_interaction_2ton_CKKW &
              (parton_pointers, shower_settings%ckkw_weights)

         if (signal_is_pending ()) return             
         if (shower_settings%muli_active) then
            !!! Initialize muli pdf sets, unless initialized
            if (mi%is_initialized ()) then
               call mi%restart ()
            else
               if (debug) then
                  ! call shower%write ()
                  ! print *, "---------------"
                  ! call interaction_write (shower%interactions(i)%i)
                  ! print *, "---------------"
                  ! call vector4_write &
                  !    (shower%interactions(1)%i%partons(1)%p%momentum)
                  ! call vector4_write &
                  !    (shower%interactions(1)%i%partons(2)%p%momentum)
                  ! print *, "---------------"
               end if
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
            ! CKKW Matching
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
                  call parton_set_momentum (partons(1), &
                       0.5_default*sqrt(shat), 0._default, 0._default, &
                       0.5_default*sqrt(shat))
                  call parton_set_momentum (partons(2), &
                       0.5_default*sqrt(shat), 0._default, 0._default, &
                       -0.5_default*sqrt(shat))
                  call parton_set_initial (partons(1), &
                       shower%interactions(1)%i%partons(1)%p%initial)
                  call parton_set_initial (partons(2), &
                       shower%interactions(1)%i%partons(2)%p%initial)
                  partons(1)%belongstoFSR = .false.
                  partons(2)%belongstoFSR = .false.
                  !!! calculate color connections
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

                  call tao_random_number (phi)
                  phi = 2*pi*phi
                  call parton_set_momentum (partons(3), &
                       0.5_default*sqrt(shat), sqrt(mi_scale)*cos(phi), &
                       sqrt(mi_scale)*sin(phi), sqrt(0.25_default*shat - &
                       mi_scale))
                  call parton_set_momentum (partons(4), &
                       0.5_default*sqrt(shat), -sqrt(mi_scale)*cos(phi), &
                       -sqrt(mi_scale)*sin(phi), -sqrt(0.25_default*shat - &
                       mi_scale))
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

                  if (debug)  call shower%write ()
               end if
            end do BRANCHINGS
               
            call shower%generate_fsr_for_isr_partons ()
            if (debug)  call shower%write ()
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
            call shower%write ()
            write (*, "(A)")  "SHOWER_FINISHED"
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
              (particle_set, u_S2W, model_in, os_data)
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
         call particle_set_write (particle_set)
         print *, &
           "----------------------apply_shower_particle_set------------------"
         print *, &
           "-----------------------------------------------------------------"         
         if (size (shower%interactions) >= 2) then
            call shower%write ()
         end if
      end if         
         
      call shower%final ()
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
      real(double) :: eta, E, pl
      type(vector4_t) :: p_tmp      

      !!! loop over particles and extract final colored ones with eta<etamax
      n_jets_PS = 0
      do i = 1, particle_set_get_n_tot (particle_set)
         if (signal_is_pending ()) return             
         tempprt = particle_set_get_particle (particle_set, i)
         if (particle_get_status (tempprt) /= PRT_OUTGOING) cycle
         col = particle_get_color (tempprt)
         if (all (col == 0)) cycle
         if (data%is_hadron_collision) then
            p_tmp = particle_get_momentum (tempprt)
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
                  call particle_write (tempprt)
               end if
               cycle
            end if
         end if
         n_jets_PS = n_jets_PS + 1
      end do

      allocate (data%P_PS(1:n_jets_PS))
      if (debug)  write (*, "(A,1x,I0)")  "n_jets_ps =", n_jets_ps

      j = 1
      do i = 1, particle_set_get_n_tot (particle_set)
         tempprt = particle_set_get_particle (particle_set, i)
         if (particle_get_status (tempprt) /= PRT_OUTGOING) cycle
         col = particle_get_color (tempprt)
         if(all(col == 0)) cycle
         if (data%is_hadron_collision) then
            p_tmp = particle_get_momentum (tempprt)
            if (energy (p_tmp) - longitudinal_part (p_tmp) < 1.E-10_default .or. &
                energy (p_tmp) + longitudinal_part (p_tmp) < 1.E-10_default) then
               eta = pseudorapidity (p_tmp)
            else
               eta = rapidity (p_tmp)
            end if
            if (eta > settings%ms%mlm_etaClusfactor * &
                 settings%ms%mlm_etamax) cycle
         end if
         data%P_PS(j) = particle_get_momentum (tempprt)
         j = j + 1
      end do
    end subroutine matching_transfer_PS
    
    subroutine apply_PYTHIAhadronization &
         (particle_set, shower_settings, model, valid)
      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      type(model_t), pointer, intent(in) :: model
      logical, intent(inout) :: valid
      integer :: u_W2P, u_P2W
      type(string_t) :: remaining_PYGIVE, partial_PYGIVE
      logical, save :: msg_written = .false.
      character*10 buffer

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
              (particle_set, u_P2W, model, os_data)
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
      call tag_gen_v%write (var_str ("2.2.1"), unit)
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
       (particle_set, u, model_in, os_data)
    type(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: u
    type(model_t), intent(in), pointer :: model_in
    type(model_t), target, save :: model_SM_hadrons
    type(model_t), pointer :: model
    logical, save :: model_SM_hadrons_associated = .false.
    type(os_data_t), intent(in) :: os_data 
    logical :: exist_SM_hadrons
    type(string_t) :: filename
    type(flavor_t) :: flv
    type(color_t) :: col
    logical :: logging_save

    integer :: newsize, oldsize
    type(particle_t), dimension(:), allocatable :: temp_prt
    integer :: i, j
    integer :: n_available_parents;
    integer, dimension(:), allocatable :: available_parents
    integer, dimension(:), allocatable :: available_children
    logical, dimension(:), allocatable :: direct_child
    type(vector4_t) :: diffmomentum
    integer, PARAMETER :: MAXLEN=200
    CHARACTER*(MAXLEN) STRING
    integer ibeg
    INTEGER :: NUP,IDPRUP,IDUP,ISTUP
    real(kind=double) :: XWGTUP,SCALUP,AQEDUP,AQCDUP,VTIMUP,SPINUP
    integer :: MOTHUP(1:2),ICOLUP(1:2)
    real(kind=double) :: PUP(1:5)
    real(kind=default) :: pup_dum(1:5)
    character*5 buffer

    CHARACTER*6 STRFMT
    STRFMT='(A000)'
    WRITE(STRFMT(3:5),'(I3)') MAXLEN

    rewind (u)

    !!! get newsize of particle_set, newsize = old size of 
    !!!    particle_set + #entries - 2 (incoming partons in lhef)
    oldsize = particle_set_get_n_tot (particle_set)
    !!! Loop until finds line beginning with "<event>" or "<event ".
    do
       read (u,*,END=501,ERR=502) STRING
       IBEG=0
       do
          if (signal_is_pending ()) return              
          IBEG = IBEG + 1
          ! Allow indentation.
          IF (STRING(IBEG:IBEG).EQ.' ' .and. IBEG < MAXLEN-6) cycle
          exit
       end do
       IF (string(IBEG:IBEG+6) /= '<event>' .and. &
            string(IBEG:IBEG+6) /= '<event ') cycle
       exit
    end do
    !!! Read first line of event info -> number of entries
    read (u, *, END=503, ERR=504) NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    newsize = oldsize + NUP - 2
    allocate (temp_prt (1:newsize))

    allocate (available_parents (1:oldsize))
    available_parents = 0
    do i = 1, particle_set_get_n_tot (particle_set)
       if (signal_is_pending ()) return           
       temp_prt (i) = particle_set_get_particle (particle_set, i)
       if (particle_get_status (temp_prt (i)) == PRT_OUTGOING .or. &
            particle_get_status (temp_prt (i)) == PRT_BEAM_REMNANT) then
          call particle_reset_status (temp_prt (i), PRT_VIRTUAL)
          available_parents (i) = i
       end if
    end do

    allocate (available_children (1:newsize))
    allocate (direct_child (1:newsize))
    available_children = 0
    direct_child = .false.

    !!! transfer particles from lhef to particle_set
    !!!...Read NUP subsequent lines with information on each particle.
    DO I = 1, NUP
       READ (u,*,END=200,ERR=505) IDUP, ISTUP, MOTHUP(1), MOTHUP(2), &
            ICOLUP(1), ICOLUP(2), (PUP (J),J=1,5), VTIMUP, SPINUP
       if ((I.eq.1).or.(I.eq.2)) cycle

       call particle_reset_status (temp_prt(oldsize+i-2), PRT_OUTGOING)
       !!! Settings for unpolarized particles
       ! particle_set%prt (oldsize+i-2)%polarization = 0 ! =PRT_UNPOLARIZED !??
       if (model_test_particle (model_in, IDUP)) then
          model => model_in
       else 
          ! prepare model_SM_hadrons for hadrons created in the hadronization
          ! and not present in the model file
          if (.not. model_SM_hadrons_associated) then
             ! call os_data_init (os_data)
             filename = "SM_hadrons.mdl"
             logging_save = logging
             logging = .false.
             call model_read (model_SM_hadrons, filename, os_data, & 
                  exist_SM_hadrons)
             logging = logging_save
             model_SM_hadrons_associated = .true.
          end if
          if (model_test_particle (model_SM_hadrons, IDUP)) then
             model => model_SM_hadrons
          else
             write (buffer, "(I5)") IDUP
             call msg_error ("Parton " // buffer // &
                  " found neither in given model file nor in SM_hadrons")
             return
          end if
       end if
       call flavor_init (flv, IDUP, model)
       call particle_set_flavor (temp_prt (oldsize+i-2), flv)
       
       if (IABS(IDUP) == 2212 .or. IABS(IDUP) == 2112) then
          ! PYTHIA sometimes sets color indices for protons and neutrons (?)
          ICOLUP (1) = 0
          ICOLUP (2) = 0
       end if
       call color_init_col_acl (col, ICOLUP (1), ICOLUP (2))
       call particle_set_color (temp_prt (oldsize+i-2), col)
       !!! Settings for unpolarized particles
       ! particle_set%prt (oldsize+i-2)%hel = ??
       ! particle_set%prt (oldsize+i-2)%pol = ??
       pup_dum = PUP
       call particle_set_momentum (temp_prt (oldsize+i-2), &
            vector4_moving (pup_dum (4), &
            vector3_moving ([pup_dum (1), pup_dum (2), pup_dum (3)])))

       available_children (oldsize+i-2) = oldsize+i-2
       !!! search for an existing particle with the same momentum 
       !!!   -> treat these as mother and daughter
       do j = 1, size (available_parents)
          if (available_parents (j) == 0) cycle
          diffmomentum = particle_get_momentum &
               (temp_prt (available_parents (j))) - &
               particle_get_momentum (temp_prt(oldsize+i-2))
          if (abs(diffmomentum**2) < 1E-10_default .and. &
               particle_get_pdg (temp_prt (available_parents (j))).eq. &
               particle_get_pdg (temp_prt (oldsize+i-2))) then
             direct_child (available_parents (j)) = .true.
             direct_child (oldsize+i-2) = .true.
             call particle_set_parents (temp_prt (oldsize+i-2), &
                  [available_parents(j)] )
             call particle_set_children (temp_prt (available_parents(j)), &
                  [oldsize+i-2] )
             available_parents (j) = 0
             available_children (oldsize+i-2) = 0
          end if
       end do
    end do

    !!! remove zeros in available parents and available children
    available_parents  = pack (available_parents , available_parents  /= 0)
    available_children = pack (available_children, available_children /= 0)

    do i = 1, size (available_parents) 
      if (direct_child (available_parents (i))) cycle
      call particle_set_children &
           (temp_prt (available_parents (i)), available_children)
    end do
    do i = oldsize + 1, newsize
       if (direct_child (i)) cycle
       call particle_set_parents (temp_prt(i), available_parents)
    end do

    ! transfer to particle_set
    call particle_set_replace (particle_set, temp_prt)
    if (allocated (available_children)) deallocate (available_children)
    if (allocated (available_parents))  deallocate (available_parents)
    deallocate (direct_child)
    deallocate (temp_prt)
    call model_final (model_SM_hadrons)

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
505 write(*,*) "READING LHEF failed 504"
    return
  end subroutine shower_add_lhef_to_particle_set
!!!!!!!!!!PYTHIA STYLE!!!!!!!!!!!!!
!!! originally PYLHEF subroutine from PYTHIA 6.4.22

!C...Write out the showered event to a Les Houches Event File.
!C...Take MSTP(161) as the input for <init>...</init>

      SUBROUTINE PYLHEO

!C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

!C...PYTHIA commonblock: only used to provide read/write units and version.
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      SAVE /PYPARS/
      SAVE /PYJETS/

!C...User process initialization commonblock.
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),LPRUP(MAXPUP)
      SAVE /HEPRUP/

!C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      PARAMETER (KSUSY1=1000000,KSUSY2=2000000,KTECHN=3000000, &
           KEXCIT=4000000,KDIMEN=5000000)
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP), &
                      PUP(5,MAXNUP),VTIMUP(MAXNUP),SPINUP(MAXNUP)
      SAVE /HEPEUP/

!C...Lines to read in assumed never longer than 200 characters.
      PARAMETER (MAXLEN=200)
      CHARACTER*(MAXLEN) STRING

      INTEGER LEN

!C...Format for reading lines.
      CHARACTER*6 STRFMT
      STRFMT='(A000)'
      WRITE(STRFMT(3:5),'(I3)') MAXLEN

!C...Rewind initialization and event files.
      REWIND MSTP(161)
      REWIND MSTP(162)

!C...Write header info.
      WRITE(MSTP(163),'(A)') '<LesHouchesEvents version="1.0">'
      WRITE(MSTP(163),'(A)') '<!--'
      WRITE(MSTP(163),'(A,I1,A1,I3)') 'File generated with PYTHIA ',MSTP(181),'.',MSTP(182)
      WRITE(MSTP(163),'(A)') ' and the WHIZARD2 interface'
      WRITE(MSTP(163),'(A)') '-->'

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
      DO IPR=0,NPRUP
        IF(IPR.GT.0) READ(MSTP(161),'(A)',END=400,ERR=400) STRING
        LEN=MAXLEN+1
  120   LEN=LEN-1
        IF(LEN.GT.1.AND.STRING(LEN:LEN).EQ.' ') GOTO 120
        WRITE(MSTP(163),'(A)',ERR=400) STRING(1:LEN)
     end DO
      WRITE(MSTP(163),'(A)') '</init>'

!!!! Find the numbers of entries of the <event block>
      NENTRIES = 2      ! incoming partons (nearest to the beam particles)
      DO I=1,N
         if((K(I,1).eq.1) .or. (K(I,1).eq.2)) then
            if(P(I,4) < 1D-10) cycle
            NENTRIES = NENTRIES + 1
         end if
      end DO

!C...Begin an <event> block. Copy event lines, omitting trailing blanks.
      WRITE(MSTP(163),'(A)') '<event>'
      WRITE(MSTP(163),*) NENTRIES,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP

      DO I=3,4       ! the incoming partons nearest to the beam particles
         WRITE(MSTP(163),*)  K(I,2),-1,0,0,0,0,(P(I,J),J=1,5),0, -9
      end DO
      NDANGLING_COLOR = 0
      NCOLOR = 0
      NDANGLING_ANTIC = 0
      NANTIC = 0
      NNEXTC = 1   ! TODO find next free color number ??
      DO I=1,N
         if (signal_is_pending ()) return             
         if((K(I,1).eq.1) .or. (K(I,1).eq.2)) then
            ! workaround for zero energy photon in electron ISR            
            if (P(I,4) < 1E-10_default) cycle   
            if ((K(I,2).eq.21) .or. (IABS(K(I,2)) <= 8) .or. &
                (IABS(K(I,2)) >= KSUSY1+1 .and. IABS(K(I,2)) <= KSUSY1+8) &
                .or. &
                (IABS(K(I,2)) >= KSUSY2+1 .and. IABS(K(I,2)) <= KSUSY2+8) .or. &
                 (IABS(K(I,2)) >= 1000 .and. IABS(K(I,2)) <= 9999) ) then
               if(NDANGLING_COLOR.eq.0 .and. NDANGLING_ANTIC.eq.0) then
                  ! new color string
                  ! Gluon and gluino only color octets implemented so far
                  if(K(I,2).eq.21 .or. K(I,2).eq.1000021) then  
                     NCOLOR = NNEXTC
                     NDANGLING_COLOR = NCOLOR
                     NNEXTC = NNEXTC + 1
                     NANTIC = NNEXTC
                     NDANGLING_ANTIC = NANTIC
                     NNEXTC = NNEXTC + 1
                  elseif(K(I,2) .gt. 0) then  ! particles to have color
                     NCOLOR = NNEXTC
                     NDANGLING_COLOR = NCOLOR
                     NANTIC = 0
                     NNEXTC = NNEXTC + 1
                  elseif(K(I,2) .lt. 0) then  ! antiparticles to have anticolor
                     NANTIC = NNEXTC
                     NDANGLING_ANTIC = NANTIC
                     NCOLOR = 0
                     NNEXTC = NNEXTC + 1
                  end if
               else if(K(I,1).eq.1) then
                  ! end of string
                  NCOLOR = NDANGLING_ANTIC
                  NANTIC = NDANGLING_COLOR
                  NDANGLING_COLOR = 0
                  NDANGLING_ANTIC = 0
               else
                  ! inside the string
                  if(NDANGLING_COLOR .ne. 0) then
                     NANTIC = NDANGLING_COLOR
                     NCOLOR = NNEXTC
                     NDANGLING_COLOR = NNEXTC
                     NNEXTC = NNEXTC +1
                  else if(NDANGLING_ANTIC .ne. 0) then
                     NCOLOR = NDANGLING_ANTIC
                     NANTIC = NNEXTC
                     NDANGLING_ANTIC = NNEXTC
                     NNEXTC = NNEXTC +1
                  else
                     print *, "ERROR IN PYLHEO"
                  end if
               end if
            else
               NCOLOR = 0
               NANTIC = 0
            end if

            !!! As no intermediate are given out here, assume the 
            !!!   incoming partons to be the mothers
            WRITE(MSTP(163),*)  K(I,2),1,1,2,NCOLOR,NANTIC,(P(I,J),J=1,5),0, -9
         end if
      end DO

!C..End the <event> block. Loop back to look for next event.
      WRITE(MSTP(163),'(A)') '</event>'

!C...Successfully reached end of event loop: write closing tag
!C...and remove temporary intermediate files (unless asked not to).
320   WRITE(MSTP(163),'(A)') '</LesHouchesEvents>'
      RETURN

!!C...Error exit.
  400 WRITE(*,*) ' PYLHEO file joining failed!'

      RETURN
    END SUBROUTINE PYLHEO

  subroutine evt_shower_write (object, unit, testflag)
    class(evt_shower_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    call write_separator_double (u)
    write (u, "(1x,A)")  "Event transform: shower"
    call write_separator (u)
    call object%base_write (u, testflag = testflag)
    call write_separator (u)
    call object%settings%write (u)
  end subroutine evt_shower_write
    
  subroutine evt_shower_init (evt, settings, os_data)
    class(evt_shower_t), intent(out) :: evt
    type(shower_settings_t), intent(in) :: settings
    type(os_data_t), intent(in) :: os_data
    evt%settings = settings
    evt%os_data = os_data
  end subroutine evt_shower_init
  
  subroutine evt_shower_setup_pdf (evt, process, beam_structure)
    class(evt_shower_t), intent(inout) :: evt
    type(process_t), intent(in) :: process
    type(beam_structure_t), intent(in) :: beam_structure
    if (beam_structure%contains ("lhapdf")) then
       evt%pdf_type = STRF_LHAPDF
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
       call apply_shower_particle_set (evt%particle_set, &
            evt%settings, evt%model, &
            evt%os_data, evt%pdf_type, evt%pdf_set, valid, vetoed)
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
    integer :: i, num_id
    integer, parameter :: min_processes = 10

    integer, parameter :: MAXNUP = 500
    integer, parameter :: MAXPUP = 100
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
    
    
    num_id = 1
    if (LPRUP (num_id) /= 0)  return

    call heprup_init ( &
         [ particle_get_pdg (particle_set_get_particle &
                              (evt%particle_set, 1)), &
           particle_get_pdg (particle_set_get_particle &
                              (evt%particle_set, 2)) ] , &
         [ vector4_get_component (particle_get_momentum &
            (particle_set_get_particle (evt%particle_set, 1)), 0), &
           vector4_get_component (particle_get_momentum &
            (particle_set_get_particle (evt%particle_set, 2)), 0) ], &
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
    integer :: i, j, k
    integer :: n
    type(vector4_t) :: momentum

    ckkw_pseudo_shower_settings%alphaS = 1.0_default
    ckkw_pseudo_shower_settings%Qmin = 1.0_default
    ckkw_pseudo_shower_settings%n_max_jets = 3

    n = 2**particle_set_get_n_tot(particle_set)
    if (allocated (ckkw_pseudo_shower_weights%weights)) then 
       deallocate (ckkw_pseudo_shower_weights%weights)
    end if
    allocate (ckkw_pseudo_shower_weights%weights (1:n))
    do i = 1, n
       momentum = vector4_null
       do j = 1, particle_set_get_n_tot (particle_set)
          if (btest (i,j-1)) then
             momentum = momentum + particle_get_momentum &
                  (particle_set_get_particle (particle_set, j))
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
       (prefix, os_data, lib, model_list, model, process, process_instance)
    type(string_t), intent(in) :: prefix
    type(os_data_t), intent(out) :: os_data
    type(process_library_t), intent(out), target :: lib
    type(model_list_t), intent(out) :: model_list
    type(model_t), pointer, intent(out) :: model
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
    call model_list%read_model (model_name, model_name // ".mdl", &
         os_data, model)
    model_vars => model_get_var_list_ptr (model)
    call var_list_set_real (model_vars, var_str ("me"), 0._default, &
         is_known = .true.)

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
         qcd, rng_factory, model_list)
    
    allocate (prc_omega_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)

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
    type(model_t), pointer :: model
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
    call setup_testbed (var_str ("shower_1"), &
         os_data, lib, model_list, model, process, process_instance)

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
       call write_separator_double (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    settings%ps_fsr_active = .true.

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (settings, os_data)
    end select

    allocate (evt_shower_t :: evt_shower)
    call evt_shower%connect (process_instance, model)
    evt_trivial%next => evt_shower
    evt_shower%previous => evt_trivial

    call evt_shower%prepare_new_event (1, 1)
    call evt_shower%generate_unweighted ()
    call evt_shower%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%write (u)
       call write_separator_double (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_1"
    
  end subroutine shower_1
  
  subroutine shower_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t), target :: lib
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
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
    call setup_testbed (var_str ("shower_2"), &
         os_data, lib, model_list, model, process, process_instance)

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
       call write_separator_double (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    settings%ps_fsr_active = .true.

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (settings, os_data)
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
       call write_separator_double (u)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_2"
    
  end subroutine shower_2
  

end module shower
