! WHIZARD 2.0.3 Tue Aug 10 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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
  use shower_basics_module !NODEP!
  use shower_module !NODEP!
  use flavors
  use colors
  use particles
  use prt_lists
  use models
  use variables
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use event_formats

  implicit none
  private

  public :: shower_settings_t
  public :: shower_settings_init
  public :: shower_settings_write
  public :: event_apply_shower_particle_set
  public :: event_apply_PYTHIAshower_particle_set
!  public :: event_apply_shower

  type :: shower_settings_t
     logical :: ps_isr_active = .false.
     logical :: ps_fsr_active = .false.
     logical :: ps_use_PYTHIA_shower = .false.

     ! values present in PYTHIA and WHIZARDs PS, comments denote corresponding PYTHIA values
     real(kind=double) :: ps_mass_cutoff = 1._double      ! PARJ(82)
     real(kind=double) :: ps_fsr_lambda = 0.29_double     ! PARP(72)
     real(kind=double) :: ps_isr_lambda = 0.29_double     ! PARP(61)
     integer :: ps_max_n_flavors = 5            ! MSTJ(45)
     logical :: ps_isr_alpha_s_running = .true.           ! MSTP(64)
     logical :: ps_fsr_alpha_s_running = .true.           ! MSTJ(44)
     real(kind=double) :: ps_fixed_alpha_s = 0._double    ! PARU(111)
     logical :: ps_isr_pt_ordered = .false.
     logical :: ps_isr_angular_ordered = .true.           ! MSTP(62)
     real(kind=double) :: ps_isr_primordial_kt_width = 0._double  ! PARP(91)
     real(kind=double) :: ps_isr_primordial_kt_cutoff = 5._double ! PARP(93)
     real(kind=double) :: ps_isr_z_cutoff = 0.999_double  ! 1-PARP(66)
     real(kind=double) :: ps_isr_minenergy = 2            ! PARP(65)
     logical :: ps_isr_only_onshell_emitted_partons = .true.  ! MSTP(63)
  end type shower_settings_t


contains

  subroutine shower_settings_init(shower_settings, var_list)
    type(shower_settings_t), intent(out) :: shower_settings
    type(var_list_t), intent(in) :: var_list

!    print *, "shower_settings_init"
    shower_settings%ps_isr_active =  var_list_get_lval(var_list, var_str("?ps_isr_active"))
    shower_settings%ps_fsr_active =  var_list_get_lval(var_list, var_str("?ps_fsr_active"))

    if( (shower_settings%ps_fsr_active .eqv. .false.).and.(shower_settings%ps_isr_active.eqv..false.) ) then
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
       write (u, *) "ps_isr_lambda               = ", shower_settings%ps_isr_lambda
       write (u, *) "ps_isr_alpha_s_running      = ", shower_settings%ps_isr_alpha_s_running
       write (u, *) "ps_isr_primordial_kt_width  = ", shower_settings%ps_isr_primordial_kt_width
       write (u, *) "ps_isr_primordial_kt_cutoff = ", shower_settings%ps_isr_primordial_kt_cutoff
       write (u, *) "ps_isr_z_cutoff             = ", shower_settings%ps_isr_z_cutoff
       write (u, *) "ps_isr_minenergy            = ", shower_settings%ps_isr_minenergy
    end if
    if(shower_settings%ps_fsr_active) then
       write (u, "(A)")  "  FSR Settings:"
       write (u, *) "ps_fsr_lambda               = ", shower_settings%ps_isr_lambda
       write (u, *) "ps_fsr_alpha_s_running      = ", shower_settings%ps_isr_alpha_s_running
    end if
  end subroutine shower_settings_write

subroutine event_apply_shower_particle_set(particle_set, shower_settings, model)
  type(particle_set_t), intent(inout) :: particle_set
  type(shower_settings_t), intent(in) :: shower_settings
  type(model_t), pointer, intent(in) :: model
  type(parton_t), dimension(:), allocatable, target :: partons, hadrons
  type(parton_pointer_t), dimension(:), allocatable :: parton_pointers, final_partons
  integer, dimension(:), allocatable :: connections
  integer :: i, j, u
  integer :: n_hadrons, n_in, n_out
  integer :: max_color_nr

  type(shower_t) :: shower
  type(particle_set_t) :: new_particle_set
  type(particle_t), dimension(:), allocatable :: temp_prt
  type(parton_pointer_t) :: temppp

  type(flavor_t) :: flv
  type(color_t) :: col
  integer, dimension(2) :: col_array
  integer, dimension(1) :: parent


  if( (shower_settings%ps_isr_active .or. shower_settings%ps_fsr_active).eqv. .false.) then
     return
  end if

  print *, "-----------------------------------------------------------------"
  print *, "----------------event_apply_shower_particle_set------------------"
  call particle_set_write(particle_set)
  call shower_settings_write(shower_settings)

  if(shower_settings%ps_use_PYTHIA_shower) then
     u = free_unit()
     !open(unit=u, status="replace", file="whizardout.lhe", action="readwrite")  ! only for dubugging purposes
     open(unit=u, status="scratch", action="readwrite")
     call les_houches_events_write_header (u)
     call heprup_write_lhef(u)
     call hepeup_write_lhef(u)
     call les_houches_events_write_footer (u)
     call event_apply_PYTHIAshower_particle_set(u, particle_set, shower_settings, model)
     close(unit=u)
  else
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
     call shower_set_isr_only_onshell_emitted_partons( &
            shower_settings%ps_isr_only_onshell_emitted_partons)

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

     ! insert these partons in shower
     call shower_set_next_color_nr(shower, 1+max_color_nr)
     call shower_add_interaction2ton(shower, parton_pointers)
 !    print *, "SHOWER BEFORE"
 !    call shower_print(shower)

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

     ! some bookkeeping, needed after the shower is done
     call shower_boost_to_labframe(shower)
     call shower_generate_primordial_kt(shower)
     call shower_update_beamremnants(shower)

     ! FSR
     do i=1, size(shower%interactions)
        call shower_interaction_generate_fsr2ton(shower, shower%interactions(i)%i)
     end do

     call shower_print(shower)
     print *, "SHOWER_FINISHED"

     ! convert shower back into new particle_set
     call shower_get_final_partons(shower, final_partons, .true.)
     call particle_set_write(particle_set)
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
        ! particle_set%prt(j)%p2 = final_partons(i)%p%momentum**2
        if(final_partons(i-j)%p%typ.eq.9999) then   ! remnant
           call flavor_init(flv, HADRON_REMNANT, model)
           call particle_reset_status(temp_prt(i), PRT_BEAM_REMNANT)
        else
           call flavor_init(flv, final_partons(i-j)%p%typ, model)
        end if
        call color_init(col, (/ final_partons(i-j)%p%c1, -final_partons(i-j)%p%c2 /) )
        call particle_set_flavor(temp_prt(i), flv)
        call particle_set_color(temp_prt(i), col)
     end do

     call particle_set_replace(particle_set, temp_prt)
     deallocate(temp_prt)

     call particle_set_write(particle_set)
     print *, "----------------event_apply_shower_particle_set------------------"
     print *, "-----------------------------------------------------------------"
     call shower_final(shower) !????
  end if
end subroutine event_apply_shower_particle_set

  subroutine event_apply_PYTHIAshower_particle_set(u, particle_set, shower_settings, model)
    type(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: u
    type(shower_settings_t), intent(in) :: shower_settings
    type(model_t), pointer, intent(in) :: model
    integer :: u1, u2
    character*10 buffer

    print *, "-------------------------------------"
    print *, "event_apply_PYTHIAshower_particle_set"
    print *, "-------------------------------------"

    rewind(u)
    write (buffer, "(I10)")  u
    call pygive ("MSTP(161)="//buffer)
    call pygive ("MSTP(162)="//buffer)

    call pygive ("MSTP(111)=0")  ! switch off hadronization
!    call pygive ("MSTP(81)=0")  ! switch off MI

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

    call pyinit("USER", "", "", 0D0)
    call pylist(2)
    call pyevnt()
    call pylist(2)

    u1 = free_unit()
    write (buffer, "(I10)")  u1
    call pygive ("MSTP(163)="//buffer)
    !open(unit=u1, file="pythiaout.lhe", status="replace", action="readwrite")  ! only for dubugging purposes
    open(unit=u1, status="scratch", action="readwrite")
    ! convert pythia /PYJETS/ to lhef given in MSTU(163)=u1
    call pylheo
    ! read and add lhef from u1
    call shower_add_lhef_to_particle_set(particle_set, u1, model)

    print *, "-------------------------------------"
    print *, "event_apply_PYTHIAshower_particle_set finshed"
    print *, "-------------------------------------"
    close(u)
    close(u1)
  end subroutine event_apply_PYTHIAshower_particle_set

  subroutine shower_add_lhef_to_particle_set(particle_set, u, model)
    type(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: u
    type(model_t), intent(in), pointer :: model
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

    print *, "shower_add_lhef_to_particle_set finished"
    call particle_set_write(particle_set)

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
    end do

    ! transfer particles from lhef to particle_set
!...Read NUP subsequent lines with information on each particle.
    DO I=1,NUP
       READ(u,*,END=200,ERR=505) IDUP,ISTUP,MOTHUP(1),MOTHUP(2),ICOLUP(1),ICOLUP(2), (PUP(J),J=1,5),VTIMUP,SPINUP
       if((I.eq.1).or.(I.eq.2)) cycle
       if((abs(IDUP).gt.2000).and.(abs(IDUP).lt.4000)) IDUP=21            !! workaround for beam remnants  !! WRONG
       if((abs(IDUP).gt.300).and.(abs(IDUP).lt.400)) IDUP=21            !! workaround for mesons built if s quarks are present !! WRONG

       call particle_reset_status(temp_prt(oldsize+i-2), PRT_OUTGOING)
       ! particle_set%prt(oldsize+i-2)%polarization=0 ! =PRT_UNPOLARIZED !??
       call flavor_init(flv, IDUP, model)
       call particle_set_flavor(temp_prt(oldsize+i-2), flv)

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

!!$  subroutine event_apply_shower(process, decay_tree)
!!$!!!!!!! out of date !!!!!!!
!!$    type(process_t), intent(inout), target :: process
!!$    type(decay_tree_t), intent(inout) :: decay_tree
!!$    type(interaction_t) :: shower_interaction
!!$    type(hard_interaction_t) :: shower_hard_interaction
!!$    type(process_t) :: shower_process
!!$    type(decay_tree_t) :: shower_decay_tree
!!$    type(flavor_t) :: flv
!!$    type(color_t) :: col
!!$    type(quantum_numbers_t), dimension(:), allocatable :: hi_qn
!!$    type(quantum_numbers_t), dimension(:), allocatable :: qn
!!$    type(interaction_t) :: temp_interaction
!!$    type(vector4_t), dimension(:), allocatable :: p
!!$    integer :: i, j, n_out
!!$    type(state_iterator_t) :: it
!!$    integer :: max_color_nr
!!$    type(evaluator_t) :: shower_eval
!!$    type(interaction_t), pointer :: hi_int
!!$    type(evaluator_t), pointer :: eval_ptr => null()
!!$    complex(default) :: me
!!$
!!$    type(interaction_t), pointer :: int_link
!!$    integer :: index_link
!!$
!!$    type(shower_t) :: shower
!!$    type(parton_t) :: prtin1, prtin2
!!$    type(parton_t), dimension(:), allocatable, target :: partons
!!$    type(particle_pointer_t), dimension(:), allocatable :: parton_pointers, final_partons
!!$    type(interaction_t), pointer :: int_sqme, int_flows
!!$    integer :: hi_n_in, hi_n_out, hi_n_inout
!!$    type(evaluator_t), pointer :: eval_sqme, eval_flows
!!$    
!!$    hi_int => evaluator_get_int_ptr(process_get_eval_sqme_ptr(process))
!!$    hi_int => process_get_hi_int_ptr(process)
!!$    call interaction_write(hi_int)
!!$
!!$    int_sqme => evaluator_get_int_ptr &
!!$         (decay_tree_get_eval_sqme_ptr  (decay_tree))
!!$    int_flows => evaluator_get_int_ptr &
!!$         (decay_tree_get_eval_flows_ptr (decay_tree))
!!$    
!!$!    int_sqme => evaluator_get_int_ptr(process_get_eval_sqme_ptr(process))
!!$!    int_flows => evaluator_get_int_ptr(process_get_eval_flows_ptr(process))
!!$    print *, interaction_get_tag(external_link_get_ptr(int_sqme%source(1)))
!!$    print *, interaction_get_tag(external_link_get_ptr(int_flows%source(1)))
!!$!    pause
!!$!    call interaction_write(external_link_get_ptr(int_sqme%source(1)))
!!$!    call interaction_write(external_link_get_ptr(int_flows%source(1)))
!!$!    pause
!!$!    call interaction_write(int_sqme)
!!$!    call interaction_write(int_flows)
!!$!    return
!!$    print *, " process: : ", process_get_n_in(process), process_get_n_out(process)
!!$    print *, " hi_int   : ", interaction_get_n_in(hi_int), interaction_get_n_out(hi_int), interaction_get_tag(hi_int)
!!$    print *, " int_sqme : ", interaction_get_n_in(int_sqme), interaction_get_n_out(int_sqme), interaction_get_tag(int_sqme)
!!$    print *, " int_flows: ", interaction_get_n_in(int_flows), interaction_get_n_out(int_flows), interaction_get_tag(int_flows)
!!$!    return
!!$    hi_n_in = process_get_n_in(process)
!!$    hi_n_out = interaction_get_n_out(int_sqme)
!!$    hi_n_inout = hi_n_in+hi_n_out
!!$!    hi_n_tot = 
!!$
!!$    j=0
!!$    allocate(partons(1:hi_n_in+hi_n_out))
!!$    allocate(parton_pointers(1:hi_n_in+hi_n_out))
!!$
!!$    call state_iterator_init(it, int_flows%state_matrix)
!!$    allocate(hi_qn(1:size(state_iterator_get_quantum_numbers(it))))
!!$    hi_qn=state_iterator_get_quantum_numbers(it)
!!$
!!$    print *, " size=", size(hi_qn), size(state_iterator_get_quantum_numbers(it))
!!$    
!!$    max_color_nr=0
!!$    call shower_create(shower)
!!$
!!$    do i=1 , hi_n_in
!!$       ! these are the flavors and momenta of the incoming particles
!!$       j=j+1
!!$       partons(j)%nr = shower_get_next_free_nr(shower)
!!$!       partons(j)%momentum = hi_int%p(i)
!!$       print *, "i1: ", i
!!$       partons(j)%momentum = interaction_get_momentum(int_sqme, i)
!!$       partons(j)%typ = flavor_get_pdg(quantum_numbers_get_flavor(hi_qn(i)))
!!$       col = quantum_numbers_get_color(hi_qn(i))
!!$       partons(j)%c1 = color_get_col(quantum_numbers_get_color(hi_qn(i)))
!!$       partons(j)%c2 = color_get_acl(quantum_numbers_get_color(hi_qn(i)))
!!$       max_color_nr = max(max_color_nr, partons(j)%c1, abs(partons(j)%c2))
!!$       parton_pointers(j)%p=>partons(j)
!!$    end do
!!$    
!!$    do i=size(hi_qn)-hi_n_out+1 , size(hi_qn)
!!$       ! these are the flavors and momenta of the final particles
!!$       j=j+1
!!$       partons(j)%nr = shower_get_next_free_nr(shower)
!!$       print *, "i2: ", i
!!$       partons(j)%momentum = interaction_get_momentum(int_sqme, i-(size(hi_qn)-hi_n_out), .true.)
!!$       partons(j)%typ = flavor_get_pdg(quantum_numbers_get_flavor(hi_qn(i)))
!!$       partons(j)%c1 = color_get_col(quantum_numbers_get_color(hi_qn(i)))
!!$       partons(j)%c2 = color_get_acl(quantum_numbers_get_color(hi_qn(i)))
!!$       max_color_nr = max(max_color_nr, partons(j)%c1, abs(partons(j)%c2))
!!$       parton_pointers(j)%p=>partons(j)
!!$     end do
!!$
!!$     !! perform showering
!!$
!!$     call shower_set_next_color_nr(shower, 1+max_color_nr)
!!$     print *, size(parton_pointers)
!!$     do i=1, size(parton_pointers)
!!$        call particle_print(parton_pointers(i)%p)
!!$     end do
!!$     call shower_add_interaction2ton(shower, parton_pointers)
!!$     call shower_print(shower)
!!$     call shower_interaction_generate_fsr2ton(shower, shower%interactions(1)%i)
!!$     call shower_print(shower)
!!$
!!$     deallocate(parton_pointers)
!!$     
!!$     call shower_get_final_partons(shower, final_partons)
!!$
!!$     !! convert shower to shower_interaction using eval_flows
!!$     n_out = hi_int%n_out
!!$     allocate(qn(1:hi_n_out+size(final_partons)))
!!$     call interaction_init(shower_interaction, hi_n_out, 0, size(final_partons))
!!$    print *, "-----------------------------------------"
!!$    print *, evaluator_get_tag(process_get_eval_flows_ptr(process)), evaluator_get_tag(process_get_eval_sqme_ptr(process))
!!$    print *, interaction_get_tag(shower_interaction)
!!$    print *, "-----------------------------------------"
!!$     eval_ptr => process_get_eval_flows_ptr(process)
!!$     print *, "TEST: ", associated(eval_ptr)
!!$     call evaluator_write(eval_ptr)
!!$     call interaction_write(hi_int)
!!$     print *, "EVALUATOR WRITE2"
!!$     ! transfer outgoing(hi)/incoming(PS) partons
!!$     print *, size(hi_qn), hi_int%n_out, hi_n_out
!!$     do i=1, size(hi_qn)
!!$        call quantum_numbers_write(hi_qn(i))
!!$     end do
!!$     pause
!!$     do i=1, hi_int%n_out
!!$        print *, i
!!$        call flavor_init(flv, flavor_get_pdg(quantum_numbers_get_flavor(hi_qn(size(hi_qn)-n_out+i))), process%model)
!!$        call quantum_numbers_set_flavor(qn(i), flv)
!!$        call quantum_numbers_set_color(qn(i), quantum_numbers_get_color(hi_qn(size(hi_qn)-n_out+i)))
!!$
!!$        ! search for the particle in eval_ptr connected to the corresponding particle in hi_int
!!$        ! so that the particle in shower_interaction can be connected to the one in eval_ptr
!!$        do j=1, size(eval_ptr%int%source)
!!$           int_link => external_link_get_ptr(eval_ptr%int%source(j))
!!$           index_link = external_link_get_index(eval_ptr%int%source(j))
!!$           print *, eval_ptr%int%tag, i, " connnected to ", int_link%tag, index_link, interaction_get_tag(int_link)
!!$           if((interaction_get_tag(int_link) == 10) .or. (interaction_get_tag(int_link)==11)) then
!!$              call interaction_write(int_link)
!!$           end if
!!$           if(index_link == i + hi_int%n_in + hi_int%n_vir) then
!!$              call interaction_set_source_link(shower_interaction, i, evaluator_get_int_ptr(eval_ptr), j)
!!$              print *, "CONNECTED"
!!$           end if
!!$        end do
!!$     end do
!!$     pause
!!$     call interaction_receive_momenta(shower_interaction)
!!$     ! transfer outgoing(PS) partons
!!$     do i=1, size(final_partons)
!!$        call interaction_set_momentum(shower_interaction, final_partons(i)%p%momentum, n_out+i)
!!$        call flavor_init(flv, final_partons(i)%p%typ, process%model)
!!$        call color_init(col, (/ final_partons(i)%p%c1, -final_partons(i)%p%c2 /) )
!!$        call quantum_numbers_set_flavor(qn(n_out+i), flv)
!!$        call quantum_numbers_set_color(qn(n_out+i), col)
!!$     end do
!!$     call state_matrix_add_state(shower_interaction%state_matrix, qn)
!!$     me = 1
!!$     allocate(shower_interaction%state_matrix%me(1:1))
!!$     shower_interaction%state_matrix%me = 1
!!$     call state_matrix_freeze(shower_interaction%state_matrix)
!!$     shower_interaction%state_matrix%me = 1
!!$
!!$     ! combine with the eval_flows evaluator
!!$     print *, "FIRST", evaluator_get_tag(process_get_eval_flows_ptr(process)), shower_interaction%tag
!!$     call evaluator_init_product(shower_eval, process_get_eval_flows_ptr(process), shower_interaction, new_quantum_numbers_mask(.false., .true., .false.))
!!$     print *, "evalautor_init_product finished 1"
!!$     call interaction_receive_momenta(shower_eval%int)
!!$     process%eval_flows = shower_eval
!!$     print *, "evalautor_init_product finished 2"
!!$
!!$     ! combination with eval_flows done
!!$     ! changing to eval_sqme
!!$
!!$     eval_ptr => process_get_eval_sqme_ptr(process)
!!$
!!$     do i=1, hi_n_out
!!$!        call interaction_set_source_link(shower_interaction, i, evaluator_get_int_ptr(process_get_eval_sqme_ptr(process)), process%eval_sqme%int%n_in + process%eval_sqme%int%n_vir + i)
!!$        print *, i, size(eval_ptr%int%source)
!!$        pause
!!$        do j=1, size(eval_ptr%int%source)
!!$           int_link => external_link_get_ptr(eval_ptr%int%source(j))
!!$           index_link = external_link_get_index(eval_ptr%int%source(j))
!!$           print *, eval_ptr%int%tag, i, " connnected to ", int_link%tag, index_link, interaction_get_tag(int_link)
!!$           if((interaction_get_tag(int_link) == 10) .or. (interaction_get_tag(int_link)==11)) then
!!$              call interaction_write(int_link)
!!$           end if
!!$           if(index_link == i + hi_int%n_in + hi_int%n_vir) then
!!$              call interaction_set_source_link(shower_interaction, i, evaluator_get_int_ptr(eval_ptr), j)
!!$              print *, "CONNECTED"
!!$           end if
!!$        end do
!!$
!!$     end do
!!$     do i=1, size(final_partons)
!!$        ! undefine color for eval_sqme
!!$        call color_undefine(col)
!!$        call quantum_numbers_set_color(qn(n_out+i), col)
!!$     end do
!!$     call state_matrix_final(shower_interaction%state_matrix)
!!$     call state_matrix_init(shower_interaction%state_matrix)
!!$     call state_matrix_add_state(shower_interaction%state_matrix, qn)
!!$     shower_interaction%state_matrix%me = 1
!!$     call state_matrix_freeze(shower_interaction%state_matrix)
!!$     shower_interaction%state_matrix%me = 1
!!$
!!$     print *, "SECOND"
!!$     call evaluator_init_product(shower_eval, process_get_eval_sqme_ptr(process), shower_interaction, new_quantum_numbers_mask(.false., .true., .false.))
!!$     call interaction_receive_momenta(shower_eval%int)
!!$     process%eval_sqme = shower_eval
!!$
!!$     !!! THIS CANNOT BE CORRECT !!!
!!$     me = 1
!!$     call state_iterator_init(it, process%eval_flows%int%state_matrix)
!!$     call state_iterator_set_matrix_element(it, me)
!!$     call state_iterator_init(it, process%eval_sqme%int%state_matrix)
!!$     call state_iterator_set_matrix_element(it, me)
!!$
!!$     ! call shower_final(shower)
!!$  end subroutine event_apply_shower

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
            if(P(I,4) < 1D-10) cycle
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
