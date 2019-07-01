! WHIZARD 2.2.7 Aug 11 2015
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
  use constants
  use io_units
  use physics_defs
  use diagnostics
  use os_interface
  use lorentz
  use subevents
  use shower_base
  use particles
  use model_data
  use hep_common
  use pdf

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
  public :: pythia6_set_last_treated_line
  integer :: N_old

  type, extends (shower_base_t) :: shower_pythia6_t
     integer :: initialized_for_NPRUP = 0
     logical :: warning_given = .false.
   contains
       procedure :: init => shower_pythia6_init
       procedure :: prepare_new_event => shower_pythia6_prepare_new_event
       procedure :: import_particle_set => shower_pythia6_import_particle_set
       procedure :: generate_emissions => shower_pythia6_generate_emissions
       procedure :: make_particle_set => shower_pythia6_make_particle_set
       procedure :: transfer_settings => shower_pythia6_transfer_settings
       procedure :: combine_with_particle_set => &
            shower_pythia6_combine_with_particle_set
       procedure :: get_final_colored_ME_momenta => shower_pythia6_get_final_colored_ME_momenta
  end type shower_pythia6_t


contains

  subroutine shower_pythia6_init (shower, settings, pdf_data)
    class(shower_pythia6_t), intent(out) :: shower
    type(shower_settings_t), intent(in) :: settings
    type(pdf_data_t), intent(in) :: pdf_data
    call msg_debug (D_SHOWER, "shower_pythia6_init")
    shower%settings = settings
    call pythia6_set_verbose (settings%verbose)
    call shower%pdf_data%init (pdf_data)
    shower%name = "PYTHIA6"
    call shower%write_msg ()
  end subroutine shower_pythia6_init

  pure subroutine shower_pythia6_prepare_new_event (shower)
    class(shower_pythia6_t), intent(inout) :: shower
  end subroutine shower_pythia6_prepare_new_event

  subroutine shower_pythia6_import_particle_set &
         (shower, particle_set, os_data)
    class(shower_pythia6_t), target, intent(inout) :: shower
    type(particle_set_t), intent(in) :: particle_set
    type(os_data_t), intent(in) :: os_data
    type(particle_set_t) :: pset_reduced
    call msg_debug (D_SHOWER, "shower_pythia6_import_particle_set")
    if (debug_active (D_SHOWER)) then
       print *, 'IDBMUP(1:2) =    ', IDBMUP(1:2)
       print *, 'EBMUP, PDFGUP =    ', EBMUP, PDFGUP
       print *, 'PDFSUP, IDWTUP =    ', PDFSUP, IDWTUP
       print *, "NPRUP = ", NPRUP
       call particle_set%write (summary=.true., compressed=.true.)
    end if
    call particle_set%reduce (pset_reduced)
    if (debug2_active (D_SHOWER)) then
       print *, 'After particle_set%reduce: pset_reduced'
       call pset_reduced%write (summary=.true., compressed=.true.)
    end if
    call hepeup_from_particle_set (pset_reduced)
    call hepeup_set_event_parameters (proc_id=1)
  end subroutine shower_pythia6_import_particle_set

  subroutine shower_pythia6_generate_emissions &
         (shower, valid, number_of_emissions)
    class(shower_pythia6_t), intent(inout), target :: shower
    logical, intent(out) :: valid
    integer, optional, intent(in) :: number_of_emissions
    integer :: N, NPAD, K
    real(double) :: P, V
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYJETS/
    integer :: u_W2P
    integer :: i
    real(double) :: beta_z, pz_in, E_in
    integer, parameter :: lower = 5
    real(double), parameter :: beta_x = 0.0_double
    real(double), parameter :: beta_y = 0.0_double
    real(double), parameter :: theta = 0.0_double
    real(double), parameter :: phi = 0.0_double
    if (signal_is_pending ()) return
    call pythia6_setup_lhe_io_units (u_W2P)
    call w2p_write_lhef_event (u_W2P)
    rewind (u_W2P)
    call pythia6_set_last_treated_line(6)
    call shower%transfer_settings ()
    call msg_debug (D_SHOWER, "calling pyevnt")
    ! TODO: (bcn 2015-04-24) doesnt change anything I think
    ! P(1,1:5) = pset_reduced%prt(1)%momentum_to_pythia6 ()
    ! P(2,1:5) = pset_reduced%prt(2)%momentum_to_pythia6 ()
    call pyevnt ()
    call pyedit(12)
    do i = 1, n
      if (K(i,1) == 14 .and. abs(K(i,2)) >= 11 .and. abs(K(i,2)) <= 16) then
        if (K(i,4) > 0 .and. K(i,5) > 0 .and. K(i,4) < N .and. K(i,5) < N) then
          K(i,1) = 11
          K(i,4) = K(K(i,4),3)
          K(i,5) = K(K(i,5),3)
        end if
      end if
    end do
    if (.not. shower%settings%hadron_collision) then
       pz_in = pup(3,1) + pup(3,2)
       E_in = pup(4,1) + pup(4,2)
       beta_z = pz_in / E_in
       call pyrobo (lower, N, theta, phi, beta_x, beta_y, beta_z)
    end if
    if (debug_active (D_SHOWER)) then
       print *, ' After pyevnt, after boosting :'
       call pylist(2)
    end if
    close (u_W2P)
    valid = pythia6_handle_errors ()
  end subroutine shower_pythia6_generate_emissions

  subroutine shower_pythia6_make_particle_set &
         (shower, particle_set, model, model_hadrons)
    class(shower_pythia6_t), intent(in) :: shower
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    class(model_data_t), intent(in), target :: model_hadrons
    call shower%combine_with_particle_set (particle_set, model, model_hadrons)
  end subroutine shower_pythia6_make_particle_set

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
       call msg_debug (D_SHOWER, "calling upinit")
       call upinit
    else
       write (buffer, "(F10.5)") sqrt (abs (shower%settings%min_virtuality))
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
       call msg_debug (D_SHOWER, "calling pyinit")
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
    class(shower_pythia6_t), intent(in) :: shower
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model_in
    class(model_data_t), intent(in), target :: model_hadrons
    call pythia6_combine_with_particle_set &
         (particle_set, model_in, model_hadrons, shower%settings)
  end subroutine shower_pythia6_combine_with_particle_set

  subroutine pythia6_combine_with_particle_set (particle_set, model_in, &
       model_hadrons, settings)
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model_in
    class(model_data_t), intent(in), target :: model_hadrons
    type(shower_settings_t), intent(in) :: settings
    class(model_data_t), pointer :: model
    type(vector4_t) :: momentum
    type(particle_t), dimension(:), allocatable :: particles, beams
    type(particle_t), dimension(2) :: incomings
    integer :: dangling_col, dangling_anti_col, color, anti_color
    integer :: i, j, py_entries, next_color, n_tot_old, parent, real_parent
    integer :: pdg, status, child, hadro_start
    integer, allocatable, dimension(:) :: old_index, new_index, &
         backup_parents, incoming_ids
    logical, allocatable, dimension(:) :: valid
    real(default), parameter :: py_tiny = 1E-10_default
    integer :: N, NPAD, K
    real(double) :: P, V
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYJETS/
    integer, parameter :: KSUSY1 = 1000000, KSUSY2 = 2000000

    if (signal_is_pending ()) return
    if (debug_active (D_SHOWER)) then
       call msg_debug (D_SHOWER, 'Combine PYTHIA6 with particle set')
       call msg_debug (D_SHOWER, 'Particle set before replacing')
       call particle_set%write (summary=.true., compressed=.true.)
       call pylist (2)
       call msg_debug (D_SHOWER, "settings%hadron_collision", settings%hadron_collision)
    end if
    if (settings%method == PS_PYTHIA6 .and. settings%hadron_collision) then
       call pythia6_set_last_treated_line(2)
       allocate (beams(2))
       beams = particle_set%prt(1:2)
       call particle_set%replace (beams)
       if (debug_active (D_SHOWER)) then
         call msg_debug (D_SHOWER, 'Resetting particle set to')
         call particle_set%write (summary=.true., compressed=.true.)
       end if
    end if
    call count_valid_entries_in_pythia_record ()
    call particle_set%without_hadronic_remnants &
         (particles, n_tot_old, py_entries)
    if (debug_active (D_SHOWER)) then
       print *, 'n_tot_old =    ', n_tot_old
       print *, 'py_entries =    ', py_entries
    end if
    call add_particles_of_pythia ()
    call particle_set%replace (particles)
    if (settings%hadron_collision) then
       call set_parent_child_relations_from_K ()
       call set_parent_child_relations_of_color_strings_to_hadrons ()
       !!! call particle_set%remove_duplicates (py_tiny * 100.0_default)
    else
       call set_parent_child_relations_from_hepevt ()
    end if
    if (settings%method == PS_WHIZARD) then
       call fudge_whizard_partons_in_hadro ()
    end if
    where ((particle_set%prt%status == PRT_OUTGOING .or. &
            particle_set%prt%status == PRT_VIRTUAL .or. &
            particle_set%prt%status == PRT_BEAM_REMNANT) .and. &
            particle_set%prt%has_children ()) &
            particle_set%prt%status = PRT_RESONANT
    if (debug_active (D_SHOWER)) then
       print *, 'Particle set after replacing'
       call particle_set%write (summary=.true., compressed=.true.)
       print *, ' pythia6_set_last_treated_line will set to: ', N
    end if
    call pythia6_set_last_treated_line(N)

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
      do i = N, N_old+1, -1
         status = K(i,1)
         if (any (P(i,1:4) > 1E-8_default * P(1,4)) .and. (status >= 1 .and. status <= 21)) then
            pset_idx = find_pythia_particle (i, more_fuzzy=.false.)
            if (pset_idx == 0) then
               valid(i) = .true.
            end if
         end if
      end do
      py_entries = count (valid)
      allocate (old_index (py_entries))
      allocate (new_index (N))
      new_index = 0
    end subroutine count_valid_entries_in_pythia_record

    subroutine add_particles_of_pythia ()
      integer :: whizard_status
      integer :: pset_idx, start_in_py
      dangling_col = 0
      dangling_anti_col = 0
      next_color = 500
      j = 1
      if (settings%method == PS_PYTHIA6 .and. settings%hadron_collision) then
         start_in_py = 3
      else
         start_in_py = 7
      end if
      do i = start_in_py, N
         status = K(i,1)
         if (valid(i)) then
            call assign_colors (color, anti_color)
            momentum = real ([P(i,4), P(i,1:3)], kind=default)
            pdg = K(i,2)
            parent = K(i,3)
            call find_model (model, pdg, model_in, model_hadrons)
            if (i <= 4) then
               whizard_status = PRT_INCOMING
            else
               if (status <= 10) then
                  whizard_status = PRT_OUTGOING
               else
                  whizard_status = PRT_VIRTUAL
               end if
            end if
            call particles(n_tot_old+j)%init &
                 (whizard_status, pdg, model, color, anti_color, momentum)
            old_index(j) = i
            new_index(i) = n_tot_old + j
            j = j + 1
         else
            pset_idx = find_pythia_particle (i, more_fuzzy=.true.)
            new_index(i) = pset_idx
         end if
      end do
    end subroutine add_particles_of_pythia

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

    subroutine set_parent_child_relations_from_hepevt ()
      integer, allocatable, dimension(:) :: parents
      integer:: parent2, parent1, npar
      integer, parameter :: NMXHEP = 4000
      integer :: NEVHEP
      integer :: NHEP
      integer, dimension(NMXHEP) :: ISTHEP
      integer, dimension(NMXHEP) :: IDHEP
      integer, dimension(2, NMXHEP) :: JMOHEP
      integer, dimension(2, NMXHEP) :: JDAHEP
      double precision, dimension(5, NMXHEP) :: PHEP
      double precision, dimension(4, NMXHEP) :: VHEP
      common /HEPEVT/ &
       NEVHEP, NHEP, ISTHEP, IDHEP, &
       JMOHEP, JDAHEP, PHEP, VHEP
      save /HEPEVT/
      integer :: i,j
      call msg_debug (D_SHOWER, &
           "set_parent_child_relations_from_hepevt")
      call pyhepc(1)
      do i = 1, NHEP
        if (JDAHEP(1,i) > 0) then
           if (count (JDAHEP(1,i:NHEP) == JDAHEP(1,i)) > 1) then
             if (JMOHEP(2,JDAHEP(1,i)) == 0) then
               if (JMOHEP(1,JDAHEP(1,i)) /= i ) then
                  call msg_error('problem in set_parent_child_' // &
                       'relations_of_known_pythia_parents')
               end if
               JMOHEP(1,JDAHEP(1,i)) = i
               do j = i + 1, NHEP
                 if (JDAHEP(1,j) == JDAHEP(1,i)) then
                    JMOHEP(2,JDAHEP(1,i)) = j
                 end if
               end do
             end if
           end if
        end if
      end do
      if (debug_active (D_SHOWER)) then
         print *, '  NHEP, n, py_entries:' , NHEP, n, py_entries
      end if
      do j = 1, py_entries
         parent1 = JMOHEP(1,old_index(j))
         parent2 = parent1
         if (JMOHEP(2,old_index(j)) > 0 ) then
           parent2 = JMOHEP(2,old_index(j))
         end if
         allocate (parents(parent2-parent1+1))
         child = n_tot_old + j
         npar = 0
         do parent = parent1, parent2
            if (parent > 0) then
               if (parent >= 1 .and. parent <= 2) then
                  call particle_set%parent_add_child (parent, child)
               else
                  if (new_index(parent) > 0 ) then
                     npar = npar + 1
                     parents(npar) = new_index(parent)
                     call particle_set%prt(new_index(parent) )%add_child (child)
                  end if
               end if
            end if
         end do
         if (npar > 0) call particle_set%prt(child)%set_parents (parents)
         deallocate (parents)
      end do
      NHEP = 0
    end subroutine set_parent_child_relations_from_hepevt

    subroutine set_parent_child_relations_from_K ()
      do j = 1, py_entries
         parent = K(old_index(j),3)
         child = n_tot_old + j
         if (parent > 0) then
            if (parent >= 1 .and. parent <= 2) then
               call particle_set%parent_add_child (parent, child)
            else
               real_parent = new_index (parent)
               if (real_parent > 0 .and. real_parent /= child) then
                  call particle_set%parent_add_child (real_parent, child)
               end if
            end if
         end if
      end do
    end subroutine set_parent_child_relations_from_K

    subroutine set_parent_child_relations_of_color_strings_to_hadrons ()
      integer :: begin_string, end_string, old_start, next_start, real_child
      integer, allocatable, dimension(:) :: parents
      call msg_debug (D_SHOWER, "set_parent_child_relations_of_color_strings_to_hadrons")
      call msg_debug (D_SHOWER, "hadro_start", hadro_start)
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
            end_string = N
            do i = begin_string, N
               if (K(i,1) == 11) then
                  end_string = i
                  exit
               end if
            end do
            allocate (parents (end_string - begin_string + 1))
            parents = 0
            real_child = new_index (old_start)
            do i = begin_string, end_string
               real_parent = new_index (i)
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

    function find_pythia_particle (i, more_fuzzy) result (j)
      integer :: j
      integer, intent(in) :: i
      logical, intent(in) :: more_fuzzy
      real(default) :: rel_small
      pdg = K(i,2)
      momentum = real([P(i,4), P(i,1:3)], kind=default)
      if (more_fuzzy) then
         rel_small = 1E-6_default
      else
         rel_small = 1E-10_default
      end if
      j = particle_set%reverse_find_particle (pdg, momentum, &
           abs_smallness = py_tiny, &
           rel_smallness = rel_small)
    end function find_pythia_particle

    subroutine fudge_whizard_partons_in_hadro ()
      do i = 1, size (particle_set%prt)
         if (particle_set%prt(i)%status == PRT_OUTGOING .and. &
             (particle_set%prt(i)%flv%get_pdg () == GLUON .or. &
              particle_set%prt(i)%flv%get_pdg_abs () < 6)  .or. &
             particle_set%prt(i)%status == PRT_BEAM_REMNANT) then
            particle_set%prt(i)%status = PRT_VIRTUAL
         end if
      end do
    end subroutine fudge_whizard_partons_in_hadro


  end subroutine pythia6_combine_with_particle_set

  subroutine shower_pythia6_get_final_colored_ME_momenta &
         (shower, momenta)
    class(shower_pythia6_t), intent(in) :: shower
    type(vector4_t), dimension(:), allocatable, intent(out) :: momenta
    integer :: N, NPAD, K
    real(double) :: P, V
    COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
    SAVE /PYJETS/
    integer :: i, j, n_jets
    if (signal_is_pending ()) return

    i = 7 !!! final ME partons start in 7th row of event record
    n_jets = 0
    do
       if (K(I,1) /= 21) exit
       if ((K(I,2) == 21) .or. (abs(K(I,2)) <= 6)) then
          n_jets = n_jets + 1
       end if
       i = i + 1
    end do
    if (n_jets == 0) return
    allocate (momenta(1:n_jets))
    i = 7
    j = 1
    do
       if (K(I,1) /= 21) exit
       if ((K(I,2) == 21) .or. (abs(K(I,2)) <= 6)) then
          momenta(j) = real ([P(i,4), P(i,1:3)], kind=default)
          j = j + 1
       end if
       i = i + 1
    end do
  end subroutine shower_pythia6_get_final_colored_ME_momenta

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
    if (debug_active (D_SHOWER)) then
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
       if (debug_active (D_SHOWER)) then
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

  subroutine pythia6_set_last_treated_line (last_line)
    integer,intent(in) :: last_line
    N_old = last_line
  end subroutine pythia6_set_last_treated_line


  end module shower_pythia6

