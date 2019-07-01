! WHIZARD 2.0.5 Tue May 10 2011
! 
! Copyright (C) 1999-2011 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module shower_interface

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use shower_basics_module !NODEP!
  use shower_module !NODEP!
  use shower_topythia_module !NODEP!
  use flavors
  use colors
  use particles
  use subevents
  use models
  use variables
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use event_formats
  use os_interface
  use diagnostics !NODEP!

  implicit none
  private

  public :: shower_settings_t
  public :: shower_settings_init
  public :: shower_settings_write
  public :: event_apply_shower_particle_set

  type :: shower_settings_t
     logical :: ps_isr_active = .false.
     logical :: ps_fsr_active = .false.
     logical :: ps_use_PYTHIA_shower = .false.
     logical :: hadronization_active = .false.

     ! values present in PYTHIA and WHIZARDs PS, comments denote corresponding PYTHIA values
     real(default) :: ps_mass_cutoff = 1._default      ! PARJ(82)
     real(default) :: ps_fsr_lambda = 0.29_default     ! PARP(72)
     real(default) :: ps_isr_lambda = 0.29_default     ! PARP(61)
     integer :: ps_max_n_flavors = 5            ! MSTJ(45)
     logical :: ps_isr_alpha_s_running = .true.           ! MSTP(64)
     logical :: ps_fsr_alpha_s_running = .true.           ! MSTJ(44)
     real(default) :: ps_fixed_alpha_s = 0._default    ! PARU(111)
     logical :: ps_isr_pt_ordered = .false.
     logical :: ps_isr_angular_ordered = .true.           ! MSTP(62)
     real(default) :: ps_isr_primordial_kt_width = 0._default  ! PARP(91)
     real(default) :: ps_isr_primordial_kt_cutoff = 5._default ! PARP(93)
     real(default) :: ps_isr_z_cutoff = 0.999_default  ! 1-PARP(66)
     real(default) :: ps_isr_minenergy = 2._default            ! PARP(65)
     real(default) :: ps_isr_tscalefactor = 1._default
     logical :: ps_isr_only_onshell_emitted_partons = .true.  ! MSTP(63)
  end type shower_settings_t


contains

  subroutine shower_settings_init(shower_settings, var_list)
    type(shower_settings_t), intent(out) :: shower_settings
    type(var_list_t), intent(in) :: var_list

    shower_settings%ps_isr_active =  var_list_get_lval(var_list, var_str("?ps_isr_active"))
    shower_settings%ps_fsr_active =  var_list_get_lval(var_list, var_str("?ps_fsr_active"))
    shower_settings%hadronization_active =  var_list_get_lval(var_list, var_str("?hadronization_active"))

    if( (shower_settings%ps_fsr_active .eqv. .false.).and.(shower_settings%ps_isr_active.eqv..false.) &
         .and.(shower_settings%hadronization_active.eqv..false.) ) then
       return
    end if

    shower_settings%ps_use_PYTHIA_shower = var_list_get_lval(var_list, var_str("?ps_use_PYTHIA_shower"))
    shower_settings%ps_mass_cutoff = var_list_get_rval(var_list, var_str("ps_mass_cutoff"))
    shower_settings%ps_fsr_lambda = var_list_get_rval(var_list, var_str("ps_fsr_lambda"))
    shower_settings%ps_isr_lambda = var_list_get_rval(var_list, var_str("ps_isr_lambda"))
    shower_settings%ps_max_n_flavors = var_list_get_ival(var_list, var_str("ps_max_n_flavors"))
    shower_settings%ps_isr_alpha_s_running = var_list_get_lval(var_list, var_str("?ps_isr_alpha_s_running"))
    shower_settings%ps_fsr_alpha_s_running = var_list_get_lval(var_list, var_str("?ps_fsr_alpha_s_running"))
    shower_settings%ps_fixed_alpha_s = var_list_get_rval(var_list, var_str("ps_fixed_alpha_s"))
    shower_settings%ps_isr_pt_ordered = var_list_get_lval(var_list, var_str("?ps_isr_pt_ordered"))
    shower_settings%ps_isr_angular_ordered = var_list_get_lval(var_list, var_str("?ps_isr_angular_ordered"))
    shower_settings%ps_isr_primordial_kt_width = var_list_get_rval(var_list, var_str("ps_isr_primordial_kt_width"))
    shower_settings%ps_isr_primordial_kt_cutoff = var_list_get_rval(var_list, var_str("ps_isr_primordial_kt_cutoff"))
    shower_settings%ps_isr_z_cutoff = var_list_get_rval(var_list, var_str("ps_isr_z_cutoff"))
    shower_settings%ps_isr_minenergy = var_list_get_rval(var_list, var_str("ps_isr_minenergy"))
    shower_settings%ps_isr_tscalefactor = var_list_get_rval(var_list, var_str("ps_isr_tscalefactor"))
    shower_settings%ps_isr_only_onshell_emitted_partons = &
                var_list_get_lval(var_list, var_str("?ps_isr_only_onshell_emitted_partons"))
  end subroutine shower_settings_init
  subroutine shower_settings_write(shower_settings, unit)
    type(shower_settings_t), intent(in) :: shower_settings
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Shower Settings:"
    write (u, *) "ps_isr_active                = ", shower_settings%ps_isr_active
    write (u, *) "ps_fsr_active                = ", shower_settings%ps_fsr_active
    if(shower_settings%ps_isr_active .or. shower_settings%ps_fsr_active) then
       write (u, *) "ps_use_PYTHIA_shower        = ", shower_settings%ps_use_PYTHIA_shower
       write (u, *) "ps_mass_cutoff              = ", shower_settings%ps_mass_cutoff
       write (u, *) "ps_max_n_flavors            = ", shower_settings%ps_max_n_flavors
    end if
    if(shower_settings%ps_isr_active) then
       write (u, "(A)")  "  ISR Settings:"
       write (u, *) "ps_isr_pt_ordered           = ", shower_settings%ps_isr_pt_ordered
       write (u, *) "ps_isr_lambda               = ", shower_settings%ps_isr_lambda
       write (u, *) "ps_isr_alpha_s_running      = ", shower_settings%ps_isr_alpha_s_running
       write (u, *) "ps_isr_primordial_kt_width  = ", shower_settings%ps_isr_primordial_kt_width
       write (u, *) "ps_isr_primordial_kt_cutoff = ", shower_settings%ps_isr_primordial_kt_cutoff
       write (u, *) "ps_isr_z_cutoff             = ", shower_settings%ps_isr_z_cutoff
       write (u, *) "ps_isr_minenergy            = ", shower_settings%ps_isr_minenergy
       write (u, *) "ps_isr_tscalefactor         = ", shower_settings%ps_isr_tscalefactor
    end if
    if(shower_settings%ps_fsr_active) then
       write (u, "(A)")  "  FSR Settings:"
       write (u, *) "ps_fsr_lambda               = ", shower_settings%ps_fsr_lambda
       write (u, *) "ps_fsr_alpha_s_running      = ", shower_settings%ps_fsr_alpha_s_running
    end if
    write (u, "(A)")  "Hadronization Settings:"
    write (u, *) "hadronization_active         = ", shower_settings%hadronization_active
  end subroutine shower_settings_write

  subroutine event_apply_shower_particle_set(particle_set, shower_settings, model)
    type(particle_set_t), intent(inout) :: particle_set
    type(shower_settings_t), intent(in) :: shower_settings
    type(model_t), pointer, intent(in) :: model

    if( (shower_settings%ps_fsr_active .eqv. .false.).and.(shower_settings%ps_isr_active.eqv..false.) &
         .and.(shower_settings%hadronization_active.eqv..false.) ) then
       return
    end if

    if(shower_settings%ps_use_PYTHIA_shower) then
       call event_apply_PYTHIAshower_particle_set(particle_set, shower_settings, model)
    else
       call event_apply_WHIZARDshower_particle_set(particle_set, shower_settings, model)
    end if

  contains
    subroutine event_apply_PYTHIAshower_particle_set(particle_set, shower_settings, model)
      ! PYTHIA common blocks
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      SAVE/PYDAT1/,/PYDAT3/

  integer, parameter :: MAXPUP = 100
  integer, parameter :: MAXNUP = 500
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
      type(shower_settings_t), intent(in) :: shower_settings
      type(model_t), pointer, intent(in) :: model

      ! units for transfer from WHIZARD to PYTHIA and back
      integer :: u_W2P, u_P2W
      logical, save :: pythia_initialized = .false.
      logical, save :: pythia_warning_given = .false.
      character*10 buffer

      ! check if the beam particles are quarks
      if( (abs(IDBMUP(1)).le.8).or.(abs(IDBMUP(2)).le.8) ) then
         ! PYTHIA doesn't support these settings
         if(pythia_warning_given.eqv..false.) then
            call msg_error("PYTHIA doesn't support quarks as beam particles," // &
                 char(10) // "     negelecting ISR, FSR and hadronization")
            pythia_warning_given = .true.
         end if
         return
      end if

      u_W2P = free_unit()
      !open(unit=u_W2P, status="replace", file="whizardout.lhe", action="readwrite")  ! only for debugging purposes
      open(unit=u_W2P, status="scratch", action="readwrite")
      call les_houches_events_write_header (u_W2P)
      call heprup_write_lhef(u_W2P)
      call hepeup_write_lhef(u_W2P)
      call les_houches_events_write_footer (u_W2P)
      rewind(u_W2P)
      write (buffer, "(I10)")  u_W2P
      call pygive ("MSTP(161)="//buffer)
      call pygive ("MSTP(162)="//buffer)
      if(pythia_initialized) then
         call upinit
      else
         if(shower_settings%ps_isr_active.eqv..false.) then
            call pygive ("MSTP(61)=0")  ! switch off ISR
         else
            call pygive("MSTP(61)=1")
         end if
         if(shower_settings%ps_fsr_active.eqv..false.) then
            call pygive ("MSTP(71)=0")  ! switch off FSR
         else
            call pygive ("MSTP(71)=1")
         end if
         if(shower_settings%hadronization_active.eqv..false.) then
            call pygive ("MSTP(111)=0") ! swith off hadronization
         else
            call pygive ("MSTP(111)=1")
         end if

         write(buffer, "(F10.5)") shower_settings%ps_mass_cutoff
         call pygive("PARJ(82)="//buffer)

         write(buffer, "(F10.5)") shower_settings%ps_fsr_lambda
         call pygive("PARP(72)="//buffer)
         write(buffer, "(F10.5)") shower_settings%ps_isr_lambda
         call pygive("PARP(61)="//buffer)
         write(buffer, "(I10)") shower_settings%ps_max_n_flavors
         call pygive("MSTJ(45)="//buffer)
         if(shower_settings%ps_isr_alpha_s_running) then
            call pygive ("MSTP(64)=2")
         else
            call pygive ("MSTP(64)=0")
         end if
         if(shower_settings%ps_fsr_alpha_s_running) then
            call pygive ("MSTJ(44)=2")
         else
            call pygive ("MSTJ(44)=0")
         end if
         write(buffer, "(F10.5)") shower_settings%ps_fixed_alpha_s
         call pygive("PARU(111)="//buffer)
         write(buffer, "(F10.5)") shower_settings%ps_isr_primordial_kt_width
         call pygive("PARP(91)="//buffer)
         write(buffer, "(F10.5)") shower_settings%ps_isr_primordial_kt_cutoff
         call pygive("PARP(93)="//buffer)
         write(buffer, "(F10.5)") 1._double - shower_settings%ps_isr_z_cutoff
         call pygive("PARP(66)="//buffer)
         write(buffer, "(F10.5)") shower_settings%ps_isr_minenergy
         call pygive("PARP(65)="//buffer)
         if(shower_settings%ps_isr_only_onshell_emitted_partons) then
            call pygive ("MSTP(63)=0")
         else
            call pygive ("MSTP(63)=2")
         end if
         pythia_initialized = .true.

         call pyinit("USER", "", "", 0D0)
      end if
      call pyevnt()
      u_P2W = free_unit()
      write (buffer, "(I10)")  u_P2W
      call pygive ("MSTP(163)="//buffer)
      !open(unit=u_P2W, file="pythiaout.lhe", status="replace", action="readwrite")  ! only for debugging purposes
      open(unit=u_P2W, status="scratch", action="readwrite")
      ! convert pythia /PYJETS/ to lhef given in MSTU(163)=u_P2W
      call pylheo
      ! read and add lhef from u_P2W
      call shower_add_lhef_to_particle_set(particle_set, u_P2W, model)
      close(unit=u_W2P)
      close(unit=u_P2W)
    end subroutine event_apply_PYTHIAshower_particle_set
    subroutine event_apply_WHIZARDshower_particle_set(particle_set, shower_settings, model_in)
      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      type(model_t), pointer, intent(in) :: model_in

      type(shower_t) :: shower
      type(parton_t), dimension(:), allocatable, target :: partons
      type(parton_pointer_t), dimension(:), allocatable :: parton_pointers, final_partons
      type(particle_t), dimension(:), allocatable :: temp_prt
      type(parton_pointer_t) :: temppp
      integer, dimension(:), allocatable :: connections
      integer :: n_loop, i, j
      integer :: n_hadrons, n_in, n_out
      integer :: max_color_nr
      logical :: had_successfull
      integer, dimension(2) :: col_array
      integer, dimension(1) :: parent
      type(flavor_t) :: flv
      type(color_t) :: col
      type(model_t), pointer :: model
      type(model_t), target, save :: model_SM_hadrons
      logical, save :: model_SM_hadrons_associated = .false.
      type(os_data_t) :: os_data 
      logical :: exist_SM_hadrons
      type(string_t) :: filename

      ! transfer settings from shower_settings to shower
      call shower_set_D_Min_t(shower_settings%ps_mass_cutoff)
      call shower_set_D_Lambda_fsr(shower_settings%ps_fsr_lambda)
      call shower_set_D_Lambda_isr(shower_settings%ps_isr_lambda)
      call shower_set_D_Nf(shower_settings%ps_max_n_flavors)
      call shower_set_D_running_alpha_s_fsr(shower_settings%ps_fsr_alpha_s_running)
      call shower_set_D_running_alpha_s_isr(shower_settings%ps_isr_alpha_s_running)
      call shower_set_D_constantalpha_s(shower_settings%ps_fixed_alpha_s)
      call shower_set_isr_pt_ordered(shower_settings%ps_isr_pt_ordered)
      call shower_set_isr_angular_ordered(shower_settings%ps_isr_angular_ordered)
      Call shower_set_primordial_kt_width(shower_settings%ps_isr_primordial_kt_width)
      call shower_set_primordial_kt_cutoff(shower_settings%ps_isr_primordial_kt_cutoff)
      call shower_set_maxz_isr(shower_settings%ps_isr_z_cutoff)
      call shower_set_minenergy_timelike(shower_settings%ps_isr_minenergy)
      call shower_set_tscalefactor_isr(shower_settings%ps_isr_tscalefactor)
      call shower_set_isr_only_onshell_emitted_partons( &
           shower_settings%ps_isr_only_onshell_emitted_partons)

      n_loop = 0
      try_shower: do ! just a loop to be able to discard events
         n_loop = n_loop + 1
         if(n_loop .gt. 1000) STOP "BUG: too many loops (try_shower)"
         call shower_create(shower)
         max_color_nr = 0

         n_hadrons = 0
         n_in = 0
         n_out = 0
         do i=1, particle_set_get_n_tot(particle_set)
            if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_BEAM) n_hadrons = n_hadrons+1
            if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_INCOMING) n_in = n_in+1
            if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_OUTGOING) n_out = n_out+1
         end do

         allocate(connections(1:particle_set_get_n_tot(particle_set)))
         connections = 0

         allocate(partons(1:n_hadrons+n_in+n_out))
         allocate(parton_pointers(1:n_in+n_out))

         j=0
         if(n_hadrons > 0) then
            ! Transfer hadrons
            do i=1,particle_set_get_n_tot(particle_set)
               if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_BEAM) then
                  j=j+1
                  partons(j)%nr = shower_get_next_free_nr(shower)
                  partons(j)%momentum = particle_get_momentum(particle_set_get_particle(particle_set, i))
                  partons(j)%t = partons(j)%momentum**2
                  partons(j)%typ = particle_get_pdg(particle_set_get_particle(particle_set, i))
                  col_array=particle_get_color(particle_set_get_particle(particle_set, i))
                  partons(j)%c1 = col_array(1)
                  partons(j)%c2 = col_array(2)
                  max_color_nr = max(max_color_nr, abs(partons(j)%c1), abs(partons(j)%c2))
                  connections(i)=j
               end if
            end do
         end if

         ! transfer incoming partons
         do i=1,particle_set_get_n_tot(particle_set)
            if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_INCOMING) then
               j=j+1
               partons(j)%nr = shower_get_next_free_nr(shower)
               partons(j)%momentum = particle_get_momentum(particle_set_get_particle(particle_set, i))
               partons(j)%t = partons(j)%momentum**2
               partons(j)%typ = particle_get_pdg(particle_set_get_particle(particle_set, i))
               col_array=particle_get_color(particle_set_get_particle(particle_set, i))
               partons(j)%c1 = col_array(1)
               partons(j)%c2 = col_array(2)
               parton_pointers(j-n_hadrons)%p => partons(j)
               max_color_nr = max(max_color_nr, abs(partons(j)%c1), abs(partons(j)%c2))
               connections(i)=j
               ! insert dependences on hadrons -> TODO
               if(particle_get_n_parents(particle_set_get_particle(particle_set, i))==1) then
                  parent = particle_get_parents(particle_set_get_particle(particle_set, i))
                  partons(j)%initial => partons(connections(parent(1)))
                  partons(j)%x = space_part_norm(partons(j)%momentum) / space_part_norm(partons(j)%initial%momentum)
               end if
            end if
         end do
         ! transfer outgoing partons
         do i=1,particle_set_get_n_tot(particle_set)
            if(particle_get_status(particle_set_get_particle(particle_set, i))==PRT_OUTGOING) then
               j=j+1
               partons(j)%nr = shower_get_next_free_nr(shower)
               partons(j)%momentum = particle_get_momentum(particle_set_get_particle(particle_set, i))
               partons(j)%t = partons(j)%momentum**2
               partons(j)%typ = particle_get_pdg(particle_set_get_particle(particle_set, i))
               col_array=particle_get_color(particle_set_get_particle(particle_set, i))
               partons(j)%c1 = col_array(1)
               partons(j)%c2 = col_array(2)
               parton_pointers(j-n_hadrons)%p => partons(j)
               max_color_nr = max(max_color_nr, abs(partons(j)%c1), abs(partons(j)%c2))
               connections(i)=j
            end if
         end do

         deallocate(connections)

         ! insert these partons in shower
         call shower_set_next_color_nr(shower, 1+max_color_nr)
         call shower_add_interaction2ton(shower, parton_pointers)

         if(shower_settings%ps_isr_active) then
            i=0
            branchings: do
               i=i+1
               ! shower_generate_next_isr_branching returns a pointer to the parton with the next ISR-branching, this parton's scale is the scale of the next branching
               temppp=shower_generate_next_isr_branching(shower)

               if(.not. associated(temppp%p)) then
                  exit branchings
               end if
               ! execute the next branching 'found' in the previous step
               call shower_execute_next_isr_branching(shower, temppp)
               !        call shower_print(shower)
            end do branchings

            call shower_generate_fsr_for_partons_emitted_in_ISR(shower)
            !     call shower_print(shower)
         else
            call shower_simulate_no_isr_shower(shower)
         end if

         ! some bookkeeping, needed after the shower is done
         call shower_boost_to_labframe(shower)
         call shower_generate_primordial_kt(shower)
         call shower_update_beamremnants(shower)

         if(shower_settings%ps_fsr_active) then
            ! FSR
            do i=1, size(shower%interactions)
               call shower_interaction_generate_fsr2ton(shower, shower%interactions(i)%i)
            end do
         else
            call shower_simulate_no_fsr_shower(shower)
         end if
         call shower_print(shower)
         print *, "SHOWER_FINISHED"

         if(shower_settings%hadronization_active) then
            call event_apply_PYTHIAhadronization_particle_set(particle_set, shower_settings, model_in, shower,had_successfull)
            if(.not. had_successfull) then
               ! clean up and try again
               deallocate(partons)
               deallocate(parton_pointers)
               call shower_final(shower)
               cycle try_shower
            end if
         else
            ! convert shower back into new particle_set
            call shower_get_final_partons(shower, final_partons, .true.)
            !        call particle_set_write(particle_set)
            !     print *, particle_set_get_n_in(particle_set), particle_set_get_n_vir(particle_set), &
            !        particle_set_get_n_out(particle_set), particle_set_get_n_tot(particle_set)

            ! transfer particle_set to temporary array
            allocate(temp_prt(1:particle_set_get_n_tot(particle_set)+size(final_partons)))
            do i=1, particle_set_get_n_tot(particle_set)
               temp_prt(i) = particle_set_get_particle(particle_set, i)
               if(particle_get_status(temp_prt(i))==PRT_OUTGOING .or. particle_get_status(temp_prt(i))==PRT_BEAM_REMNANT) then
                  call particle_reset_status(temp_prt(i), PRT_VIRTUAL)
               end if
            end do
            j = particle_set_get_n_tot(particle_set)
            do i= particle_set_get_n_tot(particle_set)+1, size(temp_prt)
               call particle_set_momentum(temp_prt(i),final_partons(i-j)%p%momentum)
               call particle_reset_status(temp_prt(i), PRT_OUTGOING)
               if(model_test_particle(model_in, final_partons(i-j)%p%typ)) then
                  model => model_in
               else 
                  ! prepare model_SM_hadrons for hadrons created in the hadronization
                  ! and not present in the model file
                  if(.not. model_SM_hadrons_associated) then
                     call os_data_init(os_data)
                     filename = "SM_hadrons.mdl"
                     call model_read(model_SM_hadrons, filename, os_data, & 
                          exist_SM_hadrons)
                     model_SM_hadrons_associated = .true.
                  end if
                  if(model_test_particle(model_SM_hadrons,  final_partons(i-j)%p%typ)) then
                     model => model_SM_hadrons
                  else
                     print *, final_partons(i-j)%p%typ
                     call msg_error ("Parton found neither in given model file " &
                          // " nor in SM_hadrons")
                     return
                  end if
               end if

               call flavor_init(flv, final_partons(i-j)%p%typ, model)
               ! call particle_reset_status(temp_prt(i), PRT_BEAM_REMNANT) !??
               call color_init(col, (/ final_partons(i-j)%p%c1, -final_partons(i-j)%p%c2 /) )
               call particle_set_flavor(temp_prt(i), flv)
               call particle_set_color(temp_prt(i), col)
            end do

            call particle_set_replace(particle_set, temp_prt)
            deallocate(final_partons)
            deallocate(temp_prt)
         end if
         deallocate(partons)
         deallocate(parton_pointers)
         exit try_shower
      end do try_shower
      !     call particle_set_write(particle_set)
      print *, "----------------event_apply_shower_particle_set------------------"
      print *, "-----------------------------------------------------------------"
      call shower_final(shower) !????
    end subroutine event_apply_WHIZARDshower_particle_set
    subroutine event_apply_PYTHIAhadronization_particle_set(particle_set, shower_settings, model, shower, successful)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      SAVE/PYDAT1/,/PYDAT3/

      type(particle_set_t), intent(inout) :: particle_set
      type(shower_settings_t), intent(in) :: shower_settings
      type(model_t), pointer, intent(in) :: model
      type(shower_t), intent(in) :: shower
      type(vector4_t) :: totalmomentum
      logical, intent(out) :: successful
      integer :: u_W2P, u_P2W
      character*10 buffer

      successful = .true.
      u_W2P = free_unit()
      open(unit=u_W2P, status="scratch", action="readwrite")
      call les_houches_events_write_header (u_W2P)
      call heprup_write_lhef(u_W2P)
      call hepeup_write_lhef(u_W2P)
      call les_houches_events_write_footer (u_W2P)
      rewind(u_W2P)
      write (buffer, "(I10)")  u_W2P
      call pygive ("MSTP(161)="//buffer)
      call pygive ("MSTP(162)="//buffer)
      ! convert shower to PYTHIA so it can be hadronized and read back in
      ! TODO? write a shower_to_lhef and read a lhef back in
      call shower_converttopythia(shower)
      call pygive ("MSTP(111)=1")
      call pygive ("MSTU(21)=1")
      call PYEXEC
      if(associated(shower%interactions(1)%i%partons(1)%p%initial) .and. &
           associated(shower%interactions(1)%i%partons(2)%p%initial)) then
         totalmomentum = shower%interactions(1)%i%partons(1)%p%initial%momentum & 
              + shower%interactions(1)%i%partons(2)%p%initial%momentum
      else
         totalmomentum = shower%interactions(1)%i%partons(1)%p%momentum &
              + shower%interactions(1)%i%partons(2)%p%momentum
      end if
      ! TODO: needed?
!      if(abs(PYP(0,1)-vector4_get_component(totalmomentum, 1)).gt.1D-5) MSTU(23) = 1
!      if(abs(PYP(0,2)-vector4_get_component(totalmomentum, 2)).gt.1D-5) MSTU(23) = 1
!      if(abs(PYP(0,3)-vector4_get_component(totalmomentum, 3)).gt.1D-5) MSTU(23) = 1
!      if(abs(PYP(0,4)-vector4_get_component(totalmomentum, 0)).gt.1D-5) MSTU(23) = 1
      ! check if hadronization was successful
      if (MSTU(23) .gt. 0) then
         ! clean up, discard shower and exit
         close(u_W2P)
         MSTU(23)=0
         successful = .false.
      else
         ! convert back
         u_P2W = free_unit()
         write (buffer, "(I10)")  u_P2W
         call pygive ("MSTP(163)="//buffer)
         !open(unit=u_P2W, file="pythiaout.lhe", status="replace", action="readwrite")  ! only for debugging purposes
         open(unit=u_P2W, status="scratch", action="readwrite")
         ! convert pythia /PYJETS/ to lhef given in MSTU(163)=u1
         call pylheo
         ! read and add lhef from u_P2W
         call shower_add_lhef_to_particle_set(particle_set, u_P2W, model)
         close(u_W2P)
         close(u_P2W)
         successful = .true.
      end if
    end subroutine event_apply_PYTHIAhadronization_particle_set
  end subroutine event_apply_shower_particle_set

  subroutine shower_add_lhef_to_particle_set (particle_set, u, model_in)
    type(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: u
    type(model_t), intent(in), pointer :: model_in
    type(model_t), target, save :: model_SM_hadrons
    type(model_t), pointer :: model
    logical, save :: model_SM_hadrons_associated = .false.
    type(os_data_t) :: os_data 
    logical :: exist_SM_hadrons
    type(string_t) :: filename
    type(flavor_t) :: flv
    type(color_t) :: col

    integer :: newsize, oldsize
    type(particle_t), dimension(:), allocatable :: temp_prt
    integer :: i, j
    integer, PARAMETER :: MAXLEN=200
    CHARACTER*(MAXLEN) STRING
    integer ibeg
    INTEGER :: NUP,IDPRUP,IDUP,ISTUP
    real(kind=double) :: XWGTUP,SCALUP,AQEDUP,AQCDUP,VTIMUP,SPINUP
    integer :: MOTHUP(1:2),ICOLUP(1:2)
    real(kind=double) :: PUP(1:5)

    CHARACTER*6 STRFMT
    STRFMT='(A000)'
    WRITE(STRFMT(3:5),'(I3)') MAXLEN

    !! set the outgoing particles of the particle_set to be virtual
    ! add outgoing particles from /HEPEVT/ to the particle_set as outgoing particles

!    print *, "shower_add_lhef_to_particle_set"
!    call particle_set_write(particle_set)

    rewind(u)

    ! get newsize of particle_set, newsize = old size of particle_set + #entries - 2 (incoming partons in lhef)
    oldsize=particle_set_get_n_tot(particle_set)
    ! Loop until finds line beginning with "<event>" or "<event ".
    do
       READ(u,*,END=501,ERR=502) STRING
       IBEG=0
       do
          IBEG=IBEG+1
          ! Allow indentation.
          IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-6) cycle
          exit
       end do
       IF(STRING(IBEG:IBEG+6).NE.'<event>'.AND.STRING(IBEG:IBEG+6).NE.'<event ') cycle
       exit
    end do
    ! Read first line of event info -> number of entries
    READ(u,*,END=503,ERR=504) NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
    print *, "NUP=", NUP
    newsize=oldsize+NUP-2         !! should be -4 for hadron collisions, but due to workaround below -2

    ! transfer particle_set to temporary array
    allocate(temp_prt(1:newsize))
    do i=1, particle_set_get_n_tot(particle_set)
       temp_prt(i) = particle_set_get_particle(particle_set, i)
       if(particle_get_status(temp_prt(i)) == PRT_OUTGOING .or. particle_get_status(temp_prt(i))==PRT_BEAM_REMNANT) then
          call particle_reset_status(temp_prt(i), PRT_VIRTUAL)
       end if
    end do


    ! transfer particles from lhef to particle_set
!...Read NUP subsequent lines with information on each particle.
    DO I=1,NUP
       READ(u,*,END=200,ERR=505) IDUP,ISTUP,MOTHUP(1),MOTHUP(2),ICOLUP(1),ICOLUP(2), (PUP(J),J=1,5),VTIMUP,SPINUP
       if((I.eq.1).or.(I.eq.2)) cycle

       call particle_reset_status(temp_prt(oldsize+i-2), PRT_OUTGOING)
       ! particle_set%prt(oldsize+i-2)%polarization=0 ! =PRT_UNPOLARIZED !??
       if(model_test_particle(model_in, IDUP)) then
          model => model_in
       else 
          ! prepare model_SM_hadrons for hadrons created in the hadronization
          ! and not present in the model file
          if(.not. model_SM_hadrons_associated) then
             call os_data_init(os_data)
             filename = "SM_hadrons.mdl"
             call model_read(model_SM_hadrons, filename, os_data, & 
                  exist_SM_hadrons)
             model_SM_hadrons_associated = .true.
          end if
          if(model_test_particle(model_SM_hadrons, IDUP)) then
             model => model_SM_hadrons
          else
             print *, IDUP
             call msg_error ("Parton found neither in given model file " &
                  // " nor in SM_hadrons")
             return
          end if
       end if
       call flavor_init(flv, IDUP, model)
       call particle_set_flavor(temp_prt(oldsize+i-2), flv)
       
       if(IABS(IDUP).eq.2212 .or. IABS(IDUP).eq.2112) then
          ! PYTHIA sets color indices for protons and neutrons (?)
          ICOLUP(1) = 0
          ICOLUP(2) = 0
       end if
       call color_init_col_acl(col, ICOLUP(1), ICOLUP(2))
       call particle_set_color(temp_prt(oldsize+i-2),col)
       !particle_set%prt(oldsize+i-2)%hel=??
       !particle_set%prt(oldsize+i-2)%pol=??
       call particle_set_momentum(temp_prt(oldsize+i-2), vector4_moving(PUP(4), vector3_moving( (/PUP(1),PUP(2),PUP(3)/) ) ) )
       ! particle_set%prt(oldsize+i-2)%p2 = PUP(5)
       !particle_set%prt(oldsize+i-2)%parent   --> JMOHEP(1,i)
       !particle_set%prt(oldsize+i-2)%child= !none!
       !print *, IDUP, oldsize+i-2, size(particle_set%prt), particle_set_get_n_tot(particle_set)
    end DO


    ! transfer to particle_set
    call particle_set_replace(particle_set, temp_prt)
    deallocate(temp_prt)
    call model_final(model_SM_hadrons)

200 continue
    call particle_set_write(particle_set)
    print *, "shower_add_lhef_to_particle_set finished"
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

!C...Begin event loop. Read first line of event info or already done.
!      READ(MSTP(162),'(A)',END=320,ERR=400) STRING
  200 CONTINUE

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
         if((K(I,1).eq.1) .or. (K(I,1).eq.2)) then
            if(P(I,4) < 1D-10) cycle   ! workaround for zero energy photon in electron ISR
            if((K(I,2).eq.21).or.(IABS(K(I,2)).le.8).or.(IABS(K(I,2)).GE.KSUSY1+1.AND.IABS(K(I,2)).LE.KSUSY1+8).OR. &
                 (IABS(K(I,2)).GE.KSUSY2+1.AND.IABS(K(I,2)).LE.KSUSY2+8).or. &
                 (IABS(K(I,2)).GE.1000.AND.IABS(K(I,2)).le.9999) ) then
               if(NDANGLING_COLOR.eq.0 .and. NDANGLING_ANTIC.eq.0) then
                  ! new color string
                  if(K(I,2).eq.21 .or. K(I,2).eq.1000021) then  ! Gluon and gluino only color octets implemented so far
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

            !! mothers = 1, 2 ??
            WRITE(MSTP(163),*)  K(I,2),1,1,2,NCOLOR,NANTIC,(P(I,J),J=1,5),0, -9
         end if
      end DO

!C..End the <event> block. Loop back to look for next event.
      WRITE(MSTP(163),'(A)') '</event>'

!C...Successfully reached end of event loop: write closing tag
!C...and remove temporary intermediate files (unless asked not to).
  320 WRITE(MSTP(163),'(A)') '</LesHouchesEvents>'
      RETURN

!!C...Error exit.
  400 WRITE(*,*) ' PYLHEO file joining failed!'

      RETURN
    END SUBROUTINE PYLHEO


end module shower_interface
