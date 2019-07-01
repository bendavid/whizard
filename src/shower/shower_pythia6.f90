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

module shower_pythia6

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use unit_tests, only: vanishes, nearly_equal
  use constants
  use io_units
  use physics_defs
  use diagnostics
  use system_defs, only: LF
  use os_interface
  use lorentz
  use subevents
  use mlm_matching
  use shower_base
  use particles
  use model_data
  use hep_common

  implicit none
  private

  public :: shower_pythia6_t
  public :: pythia6_combine_with_particle_set
  public :: pylheo
  public :: pythia6_setup_lhe_io_units
  public :: pythia6_set_config
  public :: pythia6_set_error
  public :: pythia6_get_error
  public :: pythia6_handle_errors
  public :: pythia6_set_verbose

  type, extends (shower_base_t) :: shower_pythia6_t
     integer :: initialized_for_NPRUP = 0
     logical :: warning_given = .false.
   contains
       procedure :: init => shower_pythia6_init
       procedure :: generate_emissions => shower_pythia6_generate_emissions
       procedure :: transfer_settings => shower_pythia6_transfer_settings
       procedure :: combine_with_particle_set => &
            shower_pythia6_combine_with_particle_set
  end type shower_pythia6_t


contains

  subroutine shower_pythia6_init (shower, settings, pdf_data)
    class(shower_pythia6_t), intent(out) :: shower
    type(shower_settings_t), intent(in) :: settings
    type(pdf_data_t), intent(in) :: pdf_data
    if (DEBUG_SHOWER) print *,  &
         "Transfer settings from shower_settings to shower"
    shower%settings = settings
    call pythia6_set_verbose (settings%verbose)
    call shower%pdf_data%init (pdf_data)
    shower%name = "PYTHIA6"
    call shower%write_msg ()
  end subroutine shower_pythia6_init

  subroutine shower_pythia6_generate_emissions ( &
       shower, particle_set, model, model_hadrons, &
       os_data, matching_settings, data, valid, vetoed, number_of_emissions)
    integer :: N, NPAD, K
    real(double) :: P, V
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYJETS/
    class(shower_pythia6_t), intent(inout), target :: shower
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    class(model_data_t), intent(in), target :: model_hadrons
    class(matching_settings_t), intent(in), allocatable :: matching_settings
    class(matching_data_t), intent(inout), allocatable :: data
    type(os_data_t), intent(in) :: os_data
    logical, intent(inout) :: valid
    logical, intent(inout) :: vetoed
    integer, optional, intent(in) :: number_of_emissions
    type(particle_set_t) :: pset_reduced
    integer :: u_W2P
    logical :: varying_energy = .true.

    if (signal_is_pending ()) return
    if (DEBUG_SHOWER) then
       print *, "pythia6_generate_emissions"
       print *, 'IDBMUP(1:2) =    ', IDBMUP(1:2)
       print *, 'EBMUP, PDFGUP, PDFSUP, IDWTUP =    ', &
            EBMUP, PDFGUP, PDFSUP, IDWTUP
       print *, "NPRUP = ", NPRUP
    end if
    if (any (abs(IDBMUP) <= 8)) then
       if (.not. shower%warning_given) then
          call msg_error ("PYTHIA doesn't support quarks as beam particles," &
               // LF // "     neglecting ISR, FSR and hadronization")
          shower%warning_given = .true.
       end if
       return
    end if
    call particle_set%reduce (pset_reduced)
    call hepeup_from_particle_set (pset_reduced)
    call hepeup_set_event_parameters (proc_id=1)
    call pythia6_setup_lhe_io_units (u_W2P)
    call w2p_write_lhef_event (u_W2P)
    rewind (u_W2P)
    call shower%transfer_settings ()

    if (DEBUG_SHOWER)  write (*, "(A)")  "calling pyevnt"
    if (varying_energy) then
          P(1,1:5) = pset_reduced%prt(1)%p%to_pythia6 ()
          P(2,1:5) = pset_reduced%prt(2)%p%to_pythia6 ()
    end if
    call pyevnt ()

    call shower%combine_with_particle_set (particle_set, model, model_hadrons)

    !!! Transfer momenta of the partons in the final state of
    !!!     the hard initeraction
    if (shower%settings%mlm_matching .and. allocated (data)) then
       select type (data)
       type is (mlm_matching_data_t)
          call get_ME_momenta_from_PYTHIA (data%P_ME)
       class default
          call msg_fatal ("MLM matching called with wrong data.")
       end select
    end if

    valid = pythia6_handle_errors ()
    close (u_W2P)

  end subroutine shower_pythia6_generate_emissions

  subroutine shower_pythia6_transfer_settings (shower)
    class(shower_pythia6_t), intent(inout) :: shower
    character(len=10) :: buffer
    real(default) :: rand
    if (shower%settings%isr_active) then
       call pygive ("MSTP(61)=1")
    else
       call pygive ("MSTP(61)=0")  !!! switch off ISR
    end if
    if (shower%settings%fsr_active) then
       call pygive ("MSTP(71)=1")
    else
       call pygive ("MSTP(71)=0")   !!! switch off FSR
    end if
    call pygive ("MSTP(111)=1")     !!! Allow hadronization and decays
    call pygive ("MSTJ(1)=0")       !!! No jet fragmentation
    call pygive ("MSTJ(21)=1")      !!! Allow decays but no jet fragmentation
    call pygive ("MSTP(11)=0")      !!! Disable Pythias QED-ISR per default
    call pygive ("MSTP(171)=1")     !!! Allow variable energies

    if (shower%initialized_for_NPRUP >= NPRUP) then
       if (DEBUG_SHOWER)  print *, "calling upinit"
       call upinit
    else
       write (buffer, "(F10.5)") sqrt (abs (shower%settings%d_min_t))
       call pygive ("PARJ(82)=" // buffer)
       write (buffer, "(F10.5)") shower%settings%isr_tscalefactor
       call pygive ("PARP(71)=" // buffer)
       write (buffer, "(F10.5)") shower%settings%fsr_lambda
       call pygive ("PARP(72)=" // buffer)
       write(buffer, "(F10.5)") shower%settings%isr_lambda
       call pygive ("PARP(61)=" // buffer)
       write (buffer, "(I10)") shower%settings%max_n_flavors
       call pygive ("MSTJ(45)=" // buffer)
       if (shower%settings%isr_alpha_s_running) then
          call pygive ("MSTP(64)=2")
       else
          call pygive ("MSTP(64)=0")
       end if
       if (shower%settings%fsr_alpha_s_running) then
          call pygive ("MSTJ(44)=2")
       else
          call pygive ("MSTJ(44)=0")
       end if
       write (buffer, "(F10.5)") shower%settings%fixed_alpha_s
       call pygive ("PARU(111)=" // buffer)
       write (buffer, "(F10.5)") shower%settings%isr_primordial_kt_width
       call pygive ("PARP(91)=" // buffer)
       write (buffer, "(F10.5)") shower%settings%isr_primordial_kt_cutoff
       call pygive ("PARP(93)=" // buffer)
       write (buffer, "(F10.5)") 1._double - shower%settings%isr_z_cutoff
       call pygive ("PARP(66)=" // buffer)
       write (buffer, "(F10.5)") shower%settings%isr_minenergy
       call pygive ("PARP(65)=" // buffer)
       if (shower%settings%isr_only_onshell_emitted_partons) then
          call pygive ("MSTP(63)=0")
       else
          call pygive ("MSTP(63)=2")
       end if
       if (shower%settings%mlm_matching) then
          call pygive ("MSTP(62)=2")
          call pygive ("MSTP(67)=0")
       end if
       if (DEBUG_SHOWER)  print *, "calling pyinit"
       call PYINIT ("USER", "", "", 0D0)
       call shower%rng%generate (rand)
       write (buffer, "(I10)") floor (rand*900000000)
       call pygive ("MRPY(1)=" // buffer)
       call pygive ("MRPY(2)=0")
       call pythia6_set_config (shower%settings%pythia6_pygive)
       shower%initialized_for_NPRUP = NPRUP
    end if
  end subroutine shower_pythia6_transfer_settings

  subroutine shower_pythia6_combine_with_particle_set &
         (shower, particle_set, model_in, model_hadrons)
    class(shower_pythia6_t), intent(inout) :: shower
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model_in
    class(model_data_t), intent(in), target :: model_hadrons
    call pythia6_combine_with_particle_set &
         (particle_set, model_in, model_hadrons)
  end subroutine shower_pythia6_combine_with_particle_set

  subroutine pythia6_combine_with_particle_set (particle_set, model_in, model_hadrons)
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model_in
    class(model_data_t), intent(in), target :: model_hadrons
    class(model_data_t), pointer :: model
    type(vector4_t) :: momentum
    type(particle_t), dimension(:), allocatable :: particles
    integer :: dangling_col, dangling_anti_col, color, anti_color
    integer :: i, j, py_entries, next_color, n_tot_old, parent, real_parent
    integer :: pdg, status, child, hadro_start
    integer, allocatable, dimension(:) :: old_index, new_index, backup_parents
    logical, allocatable, dimension(:) :: pythia_particle, valid
    type(lorentz_transformation_t) :: L
    logical :: boost_required
    real(default), parameter :: py_tiny = 1E-10_default
    integer :: N, NPAD, K
    real(double) :: P, V
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYJETS/
    integer, parameter :: KSUSY1 = 1000000, KSUSY2 = 2000000

    if (signal_is_pending ()) return
    if (DEBUG_SHOWER) then
       print *, 'Combine PYTHIA6 with particle set'
       print *, 'Particle set before replacing'
       call particle_set%write (summary=.true., compressed=.true.)
       call pylist (2)
    end if
    call determine_boost ()
    call count_valid_entries_in_pythia_record ()
    call particle_set%without_hadronic_remnants &
         (particles, n_tot_old, py_entries)
    if (DEBUG_SHOWER) then
       print *, 'n_tot_old =    ', n_tot_old
       print *, 'py_entries =    ', py_entries
    end if
    call prepare_temporary_objects_and_particles ()
    call add_particles_of_pythia ()
    if (boost_required) then
       where (pythia_particle)  particles%p = L * particles%p
    end if
    call particle_set%replace (particles)
    call set_parent_child_relations_of_known_pythia_parents ()
    call set_parent_child_relations_of_color_strings_to_hadrons ()
    call give_orphans_backup_parents ()
    where ((particle_set%prt%status == PRT_OUTGOING .or. &
            particle_set%prt%status == PRT_VIRTUAL .or. &
            particle_set%prt%status == PRT_BEAM_REMNANT) .and. &
            particle_set%prt%has_children ()) &
            particle_set%prt%status = PRT_RESONANT
    if (DEBUG_SHOWER) then
       print *, 'Particle set after replacing'
       call particle_set%write (summary=.true., compressed=.true.)
    end if

  contains

    subroutine count_valid_entries_in_pythia_record ()
      integer :: pset_idx
      hadro_start = 0
      allocate (valid(N))
      valid = .false.
      FIND: do i = 5, N
         if (K(i,2) >= 91 .and. K(i,2) <= 94) then
            hadro_start = i
            exit FIND
         end if
      end do FIND
      do i = 5, N
         status = K(i,1)
         if (any (P(i,1:4) > 1E5_default * py_tiny) .and. (status >= 1 .and. status <= 21)) then
            pset_idx = find_pythia_particle (i)
            if (pset_idx == 0) then
               valid (i) = .true.
            end if
         end if
      end do
      py_entries = count (valid)
      allocate (old_index (py_entries))
      allocate (new_index (N))
    end subroutine count_valid_entries_in_pythia_record

    subroutine prepare_temporary_objects_and_particles ()
      allocate (pythia_particle (n_tot_old + py_entries))
      pythia_particle = .false.
      backup_parents = pack ([(i, i=1, size (particles))], &
           (particles%get_status () == PRT_INCOMING .or. &
            particles%get_status () == PRT_OUTGOING))
    end subroutine prepare_temporary_objects_and_particles
    subroutine add_particles_of_pythia ()
      integer :: whizard_status
      dangling_col = 0
      dangling_anti_col = 0
      next_color = 500
      j = 1
      do i = 5, N
         status = K(i,1)
         if (valid(i)) then
            call assign_colors (color, anti_color)
            momentum = real ([P(i,4), P(i,1:3)], kind=default)
            pdg = K(i,2)
            parent = K(i,3)
            call find_model (model, pdg, model_in, model_hadrons)
            if (status <= 10) then
               whizard_status = PRT_OUTGOING
            else
               whizard_status = PRT_VIRTUAL
            end if
            call particles(n_tot_old + j)%init &
                 (whizard_status, pdg, model, color, anti_color, momentum)
            old_index(j) = i
            new_index(i) = n_tot_old + j
            pythia_particle(n_tot_old + j) = .true.
            j = j + 1
         end if
      end do
    end subroutine add_particles_of_pythia

    subroutine determine_boost ()
      type(vector4_t) :: sum_vec_in, sum_vec_out
      boost_required = .false.
      if (all (particle_set%prt(1:2)%flv%get_pdg_abs () >= ELECTRON .and. &
           particle_set%prt(1:2)%flv%get_pdg_abs () <= TAU)) then
         sum_vec_in = sum (particle_set%prt%p, &
              mask=particle_set%prt%get_status () == PRT_INCOMING)
         sum_vec_out = [zero, zero, zero, zero]
         do i = 1, N
            if (K(i,1) <= 10) then
               momentum = real ([P(i,4), P(i,1:3)], kind=default)
               sum_vec_out = sum_vec_out + momentum
            end if
         end do
         !sum_vec_out = sum (particles%p, &
              !mask=particles%get_status () == PRT_OUTGOING)
         if (DEBUG_SHOWER) then
            print *, 'sum_vec_in%p =    ', sum_vec_in%p
            print *, 'sum_vec_out%p =    ', sum_vec_out%p
         end if
         if (.not. nearly_equal (sum_vec_in%p(3), sum_vec_out%p(3), &
              abs_smallness = 1E-9_default, &
              rel_smallness = 1E-6_default)) then
            boost_required = .true.
            L = boost (sum_vec_in, sum_vec_out%p(0))
         end if
      end if
      if (DEBUG_SHOWER) then
         print *, 'boost_required =    ', boost_required
      end if
    end subroutine determine_boost

    subroutine assign_colors (color, anti_color)
      integer, intent(out) :: color, anti_color
      if ((K(I,2) == 21) .or. (abs (K(I,2)) <= 8) .or. &
           (abs (K(I,2)) >= KSUSY1+1 .and. abs (K(I,2)) <= KSUSY1+8) .or. &
           (abs (K(I,2)) >= KSUSY2+1 .and. abs (K(I,2)) <= KSUSY2+8) .or. &
           (abs (K(I,2)) >= 1000 .and. abs (K(I,2)) <= 9999) .and. &
           hadro_start == 0) then
         if (dangling_col == 0 .and. dangling_anti_col == 0) then
            ! new color string
            ! Gluon and gluino only color octets implemented so far
            if (K(I,2) == 21 .or. K(I,2) == 1000021) then
               color = next_color
               dangling_col = color
               next_color = next_color + 1
               anti_color = next_color
               dangling_anti_col = anti_color
               next_color = next_color + 1
            else if (K(I,2) > 0) then  ! particles have color
               color = next_color
               dangling_col = color
               anti_color = 0
               next_color = next_color + 1
            else if (K(I,2) < 0) then  ! antiparticles have anticolor
               anti_color = next_color
               dangling_anti_col = anti_color
               color = 0
               next_color = next_color + 1
            end if
         else if(status == 1) then
            ! end of string
            color = dangling_anti_col
            anti_color = dangling_col
            dangling_col = 0
            dangling_anti_col = 0
         else
            ! inside the string
            if(dangling_col /= 0) then
               anti_color = dangling_col
               color = next_color
               dangling_col = next_color
               next_color = next_color +1
            else if(dangling_anti_col /= 0) then
               color = dangling_anti_col
               anti_color = next_color
               dangling_anti_col = next_color
               next_color = next_color +1
            else
               call msg_bug ("Couldn't assign colors")
            end if
         end if
      else
         color = 0
         anti_color = 0
      end if
    end subroutine assign_colors

    subroutine set_parent_child_relations_of_known_pythia_parents ()
      do j = 1, py_entries
         parent = K(old_index(j),3)
         child = n_tot_old + j
         if (parent > 0) then
            if (parent >= 1 .and. parent <= 2) then
               call particle_set%parent_add_child (parent, child)
            else
               real_parent = find_pythia_particle (parent)
               if (real_parent > 0 .and. real_parent /= child) then
                  call particle_set%parent_add_child (real_parent, child)
               end if
            end if
         end if
      end do
    end subroutine set_parent_child_relations_of_known_pythia_parents

    subroutine set_parent_child_relations_of_color_strings_to_hadrons ()
      integer :: begin_string, end_string, old_start, next_start, real_child
      integer, allocatable, dimension(:) :: parents
      if (hadro_start > 0) then
         old_start = hadro_start
         do
            next_start = 0
            FIND: do i = old_start + 1, N
               if (K(i,2) >= 91 .and. K(i,2) <= 94) then
                  next_start = i
                  exit FIND
               end if
            end do FIND
            begin_string = K(old_start,3)
            do i = begin_string, N
               if (K(i,1) == 11) then
                  end_string = i
                  exit
               end if
            end do
            allocate (parents (end_string - begin_string + 1))
            parents = 0
            real_child = find_pythia_particle (old_start)
            do i = begin_string, end_string
               real_parent = find_pythia_particle (i)
               if (real_parent > 0) then
                  call particle_set%prt(real_parent)%add_child (real_child)
                  parents (i - begin_string + 1) = real_parent
               end if
            end do
            call particle_set%prt(real_child)%set_parents (parents)
            deallocate (parents)
            if (next_start == 0) exit
            old_start = next_start
         end do
      end if
    end subroutine set_parent_child_relations_of_color_strings_to_hadrons

    function find_pythia_particle (i) result (j)
      integer :: j
      integer, intent(in) :: i
      pdg = K(i,2)
      momentum = real([P(i,4), P(i,1:3)], kind=default)
      if (boost_required) then
         momentum = L * momentum
      end if
      j = particle_set%find_particle (pdg, momentum, &
           abs_smallness = py_tiny, &
           rel_smallness = 1E3_default * py_tiny)
    end function find_pythia_particle

    subroutine give_orphans_backup_parents ()
      do i = 1, size (particle_set%prt)
         if ((particle_set%prt(i)%status == PRT_OUTGOING .or. &
              particle_set%prt(i)%status == PRT_VIRTUAL .or. &
              particle_set%prt(i)%status == PRT_RESONANT) .and. .not. &
              particle_set%prt(i)%has_parents ()) then
            call particle_set%prt(i)%set_parents (backup_parents)
            do j = 1, size(backup_parents)
               call particle_set%prt(backup_parents(j))%add_child (i)
            end do
         end if
      end do
    end subroutine give_orphans_backup_parents


  end subroutine pythia6_combine_with_particle_set

  subroutine get_ME_momenta_from_PYTHIA (jets_me)
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
    SAVE /PYJETS/

    type(vector4_t), dimension(:), allocatable :: jets_me
    integer :: i, j, n_jets

    if (allocated (jets_me))  deallocate (jets_me)

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
    allocate (jets_me(1:n_jets))

    !!! transfer jets
    i = 7
    j = 1
    do
       if (K(I,1) /= 21) exit
       if ((K(I,2) == 21) .or. (abs(K(I,2)) <= 6)) then
          jets_me(j) = real ([P(i,4), P(i,1:3)], kind=default)
          j = j + 1
       end if
       i = i + 1
    end do
  end subroutine get_ME_momenta_from_PYTHIA

!!!!!!!!!!PYTHIA STYLE!!!!!!!!!!!!!
!!! originally PYLHEF subroutine from PYTHIA 6.4.22

  !C...Write out the showered event to a Les Houches Event File.

  subroutine pylheo (u_P2W)

  !C...Double precision and integer declarations.
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    integer, intent(in) :: u_P2W

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
    write (u_P2W, "(A)")  '<LesHouchesEvents version="1.0">'
    write (u_P2W, "(A)")  "<!--"
    write (u_P2W, "(A,I1,A1,I3)")  "File generated with PYTHIA ", &
         MSTP(181), ".", MSTP(182)
    write (u_P2W, "(A)")  " and the WHIZARD2 interface"
    write (u_P2W, "(A)")  "-->"

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
    WRITE(u_P2W,'(A)') '<init>'
    do IPR = 0, NPRUP
       IF(IPR.GT.0) READ(MSTP(161),'(A)',END=400,ERR=400) STRING
       LEN=MAXLEN+1
120    LEN=LEN-1
       IF(LEN.GT.1.AND.STRING(LEN:LEN).EQ.' ') GOTO 120
       WRITE(u_P2W,'(A)',ERR=400) STRING(1:LEN)
    end DO
    write (u_P2W, "(A)")  "</init>"

    !!! Find the numbers of entries of the <event block>
    NENTRIES = 0
    do I = 1, N
       if (K(I,1) == 1 .or. K(I,1) == 2 .or. K(I,1) == 21) then
          NENTRIES = NENTRIES + 1
       end if
    end do

    !C...Begin an <event> block. Copy event lines, omitting trailing blanks.
    write (u_P2W, "(A)")  "<event>"
    write (u_P2W, *)  NENTRIES, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP

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
          write (u_P2W,*)  K(I,2), K(I,1), K(I,3), K(I,3), &
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

  subroutine pythia6_setup_lhe_io_units (u_W2P, u_P2W)
    integer, intent(out) :: u_W2P
    integer, intent(out), optional :: u_P2W
    character(len=10) :: buffer
    u_W2P = free_unit ()
    if (DEBUG_SHOWER) then
       open (unit=u_W2P, status="replace", file="whizardout.lhe", &
            action="readwrite")
    else
       open (unit=u_W2P, status="scratch", action="readwrite")
    end if
    write (buffer, "(I10)")  u_W2P
    call pygive ("MSTP(161)=" // buffer)  !!! Unit for PYUPIN (LHA)
    call pygive ("MSTP(162)=" // buffer)  !!! Unit for PYUPEV (LHA)
    if (present (u_P2W)) then
       u_P2W = free_unit ()
       write (buffer, "(I10)")  u_P2W
       call pygive ("MSTP(163)=" // buffer)
       if (DEBUG_SHOWER) then
          open (unit=u_P2W, file="pythiaout2.lhe", status="replace", &
               action="readwrite")
       else
          open (unit=u_P2W, status="scratch", action="readwrite")
       end if
    end if
  end subroutine pythia6_setup_lhe_io_units

  subroutine pythia6_set_config (pygive_all)
    type(string_t), intent(in) :: pygive_all
    type(string_t) :: pygive_remaining, pygive_partial
    if (len (pygive_all) > 0) then
       pygive_remaining = pygive_all
       do while (len (pygive_remaining) > 0)
          call split (pygive_remaining, pygive_partial, ";")
          call pygive (char (pygive_partial))
       end do
       if (pythia6_get_error() /= 0) then
          call msg_fatal &
               (" PYTHIA6 did not recognize ps_PYTHIA_PYGIVE setting.")
       end if
    end if
  end subroutine pythia6_set_config

  subroutine pythia6_set_error (mstu23)
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
    SAVE/PYDAT1/
    integer, intent(in) :: mstu23
    MSTU(23) = mstu23
  end subroutine pythia6_set_error

  function pythia6_get_error () result (mstu23)
    IMPLICIT DOUBLE PRECISION(A-H, O-Z)
    IMPLICIT INTEGER(I-N)
    COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
    SAVE/PYDAT1/
    integer :: mstu23
    mstu23 = MSTU(23)
  end function pythia6_get_error

  function pythia6_handle_errors () result (valid)
    logical :: valid
    valid = pythia6_get_error () == 0
    if (.not. valid) then
       call pythia6_set_error (0)
    end if
  end function pythia6_handle_errors

  subroutine pythia6_set_verbose (verbose)
    logical, intent(in) :: verbose
    if (verbose) then
       call pygive ('MSTU(13)=1')
    else
       call pygive ('MSTU(12)=12345') !!! No title page is written
       call pygive ('MSTU(13)=0')     !!! No information is written
    end if
  end subroutine pythia6_set_verbose


  end module shower_pythia6

