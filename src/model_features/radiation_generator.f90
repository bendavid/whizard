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

module radiation_generator

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use format_utils, only: write_separator
  use os_interface
  use models
  use pdg_arrays
  use particle_specifiers
  use model_data
  use auto_components
  use physics_defs
  use unit_tests

  implicit none
  private

  public :: radiation_generator_t
  public :: radiation_generator_test
  
  type :: pdg_sorter_t
     integer :: pdg
     logical :: checked = .false.
     integer :: associated_born = 0
  end type pdg_sorter_t

  type :: pdg_states_t
    type(pdg_array_t), dimension(:), allocatable :: pdg
    type(pdg_states_t), pointer :: next
    integer :: n_particles
  contains
    procedure :: init => pdg_states_init
    procedure :: add => pdg_states_add
    procedure :: get_n_states => pdg_states_get_n_states
  end type pdg_states_t

  type :: reshuffle_list_t
     integer, dimension(:), allocatable :: ii
     type(reshuffle_list_t), pointer :: next => null ()
  contains
    procedure :: append => reshuffle_list_append 
    procedure :: get => reshuffle_list_get
  end type reshuffle_list_t

  type :: radiation_generator_t
    logical :: qcd_enabled = .false.
    logical :: qed_enabled = .false.
    logical :: is_gluon = .false.
    logical :: fs_gluon = .false.
    logical :: only_final_state = .true.
    type(pdg_list_t) :: pl_in, pl_out
    type(split_constraints_t) :: constraints
    integer :: n_tot
    integer :: n_in, n_out
    integer :: n_loops
    integer :: n_light_quarks
    real(default) :: mass_sum
    class(model_data_t), pointer :: radiation_model
    type(pdg_states_t) :: pdg_raw
    type(pdg_array_t), dimension(:), allocatable :: pdg_in_born, pdg_out_born
  contains
    generic :: init => init_pdg_list, init_pdg_array
    procedure :: init_pdg_list => radiation_generator_init_pdg_list
    procedure :: init_pdg_array => radiation_generator_init_pdg_array
    procedure :: init_radiation_model => &
                      radiation_generator_init_radiation_model
    procedure :: set_n => radiation_generator_set_n
    procedure :: set_constraints => radiation_generator_set_constraints
    procedure :: generate => radiation_generator_generate
    procedure :: get_raw_states => radiation_generator_get_raw_states
    procedure :: save_born_raw => radiation_generator_save_born_raw
    procedure :: get_born_raw => radiation_generator_get_born_raw
  end type radiation_generator_t




contains

  subroutine reshuffle_list_append (rlist, ii) 
     class(reshuffle_list_t), intent(inout) :: rlist
     integer, dimension(:), allocatable :: ii
     type(reshuffle_list_t), pointer :: current
     if (associated (rlist%next)) then
        current => rlist%next
        do
           if (associated (current%next)) then
              current => current%next
           else
              allocate (current%next)
              current%next%ii = ii
              exit
           end if
        end do
     else
        allocate (rlist%next)
        rlist%next%ii = ii
     end if
   end subroutine reshuffle_list_append

  function reshuffle_list_get (rlist, index) result (ii)
    class(reshuffle_list_t), intent(inout) :: rlist
    integer, intent(in) :: index
    integer, dimension(:), allocatable :: ii
    type(reshuffle_list_t), pointer :: current
    integer :: i
    if (associated (rlist%next)) then
       current => rlist%next
    else
       call msg_fatal ("Reshuffle list is emtpy")
    end if
    do i = 1, index-1
       if (associated (current%next)) then
          current => current%next
       else
          call msg_fatal ("Index exceeds size of reshuffling list")
       end if
    end do
    ii = current%ii
  end function reshuffle_list_get

  subroutine pdg_states_init (states)
    class(pdg_states_t), intent(inout) :: states
    nullify (states%next)
  end subroutine pdg_states_init

  subroutine pdg_states_add (states, pdg)
    class(pdg_states_t), intent(inout), target :: states
    type(pdg_array_t), dimension(:), intent(in) :: pdg
    type(pdg_states_t), pointer :: current_state
    select type (states)
    type is (pdg_states_t)
      current_state => states
      do
        if (associated (current_state%next)) then
          current_state => current_state%next
        else
          allocate (current_state%next)
          nullify(current_state%next%next)
          current_state%pdg = pdg
          exit
        end if
      end do
    end select
  end subroutine pdg_states_add

  function pdg_states_get_n_states (states) result (n)
    class(pdg_states_t), intent(in), target :: states
    integer :: n
    type(pdg_states_t), pointer :: current_state
    n = 0
    select type(states)
    type is (pdg_states_t)
      current_state => states
      do
        if (associated (current_state%next)) then
          n = n+1
          current_state => current_state%next
        else
          exit
        end if
      end do
    end select
  end function pdg_states_get_n_states

  subroutine radiation_generator_init_pdg_list &
       (generator, pl_in, pl_out, qcd, qed)
    class(radiation_generator_t), intent(inout) :: generator
    type(pdg_list_t), intent(in) :: pl_in, pl_out
    logical, intent(in), optional :: qcd, qed
    if (present (qcd))  generator%qcd_enabled = qcd
    if (present (qed))  generator%qed_enabled = qed
    generator%pl_in = pl_in
    generator%pl_out = pl_out
    generator%is_gluon = pl_in%search_for_particle (GLUON)
    generator%fs_gluon = pl_out%search_for_particle (GLUON)
    generator%only_final_state = .not. (&
       generator%qcd_enabled .and. pl_in%contains_colored_particles ())
    generator%mass_sum = 0._default
    call generator%pdg_raw%init ()
  end subroutine radiation_generator_init_pdg_list

  subroutine radiation_generator_init_pdg_array &
       (generator, pdg_in, pdg_out, qcd, qed)
    class(radiation_generator_t), intent(inout) :: generator
    type(pdg_array_t), intent(in), dimension(:) :: pdg_in, pdg_out
    logical, intent(in), optional :: qcd, qed
    type(pdg_list_t) :: pl_in, pl_out
    integer :: i
    call pl_in%init(size (pdg_in))    
    call pl_out%init(size (pdg_out))
    do i = 1, size (pdg_in)
       call pl_in%set (i, pdg_in(i))
    end do
    do i = 1, size (pdg_out)
       call pl_out%set (i, pdg_out(i))
    end do
    call generator%init (pl_in, pl_out, qcd, qed)
  end subroutine radiation_generator_init_pdg_array

  subroutine radiation_generator_init_radiation_model (generator, model)
    class(radiation_generator_t), intent(inout) :: generator
    class(model_data_t), intent(in), target :: model
    generator%radiation_model => model
  end subroutine radiation_generator_init_radiation_model

  subroutine radiation_generator_set_n (generator, n_in, n_out, n_loops)
    class(radiation_generator_t), intent(inout) :: generator
    integer, intent(in) :: n_in, n_out, n_loops
    generator%n_tot = n_in + n_out + 1
    generator%n_in = n_in
    generator%n_out = n_out
    generator%n_loops = n_loops
  end subroutine radiation_generator_set_n

  subroutine radiation_generator_set_constraints &
       (generator, set_n_loop, set_mass_sum, &
        set_selected_particles, set_required_particles)
    class(radiation_generator_t), intent(inout), target :: generator
    logical, intent(in) :: set_n_loop   
    logical, intent(in) :: set_mass_sum
    logical, intent(in) :: set_selected_particles
    logical, intent(in) :: set_required_particles
    integer :: i, j, n, n_constraints
    type(pdg_list_t) :: pl_req, pl_insert
    type(pdg_list_t) :: pl_antiparticles
    type(pdg_array_t) :: pdg_gluon, pdg_photon
    type(pdg_array_t) :: pdg_add, pdg_tmp
    integer :: i_skip, last_index
    integer :: n_new_particles
    i_skip = -1
    
    n_constraints = 1 + count([set_n_loop, set_mass_sum, &
         set_selected_particles, set_required_particles])
    associate (constraints => generator%constraints)
      n = 1
      call constraints%init (n_constraints)
      call constraints%set (n, constrain_n_tot (generator%n_tot))
      n = n+1
      if (set_n_loop) then
         call constraints%set (n, constrain_n_loop(generator%n_loops))
         n = n+1
      end if 
      if (set_mass_sum) then
        call constraints%set (n, constrain_mass_sum(generator%mass_sum))
        n = n+1
      end if
      if (set_required_particles) then
        if (generator%fs_gluon) then
           do i = 1, generator%n_out
              pdg_tmp = generator%pl_out%get(i)
              if (pdg_tmp%search_for_particle (GLUON)) then
                 i_skip = i
                 exit
              end if
           end do
           call pl_req%init (generator%n_out-1)
        else
           call pl_req%init (generator%n_out)
        end if
        do i = 1, generator%n_out
           if (i == i_skip) cycle
           call pl_req%set (i, generator%pl_out%get(i))
        end do          
        call constraints%set (n, constrain_require (pl_req))
        n = n+1
      end if
      if (set_selected_particles) then
        if (generator%only_final_state ) then
           call pl_insert%init (generator%n_out+1)
           do i = 1, generator%n_out
              call pl_insert%set(i, generator%pl_out%get(i))
           end do
           last_index = generator%n_out
        else
           call generator%pl_in%create_antiparticles (pl_antiparticles, n_new_particles)
           call pl_insert%init (generator%n_tot+n_new_particles+1)
           do i = 1, generator%n_in
              call pl_insert%set(i, generator%pl_in%get(i))
           end do
           do i = 1, generator%n_out
              j = i + generator%n_in
              call pl_insert%set(j, generator%pl_out%get(i))
           end do
           do i = 1, n_new_particles
              j = i + generator%n_in + generator%n_out
              call pl_insert%set(j, pl_antiparticles%get(i))
           end do
           last_index = generator%n_tot + n_new_particles + 1
        end if
        pdg_gluon = GLUON; pdg_photon = PHOTON
        if (generator%qcd_enabled) pdg_add = pdg_add // pdg_gluon
        if (generator%qed_enabled) pdg_add = pdg_add // pdg_photon
        call pl_insert%set (last_index, pdg_add)
        call constraints%set (n, constrain_insert (pl_insert))
      end if
    end associate

  end subroutine radiation_generator_set_constraints

  subroutine radiation_generator_generate (generator, prt_tot_in, prt_tot_out)
    type :: prt_array_t
       type(string_t), dimension(:), allocatable :: prt
    end type
    integer, parameter :: n_flv_max = 10
    class(radiation_generator_t), intent(inout) :: generator
    type(string_t), intent(out), dimension(:), allocatable :: prt_tot_in, prt_tot_out
    type(prt_array_t), dimension(n_flv_max) :: prt_in, prt_out
    type(prt_array_t), dimension(n_flv_max) :: prt_out0, prt_in0
    type(pdg_array_t), dimension(:), allocatable :: pdg_tmp, pdg_out, pdg_in
    type(if_table_t) :: if_table
    type(pdg_list_t), dimension(:), allocatable :: pl_in, pl_out
    integer :: i, j
    integer, dimension(:), allocatable :: reshuffle_list_local
    type(reshuffle_list_t) :: reshuffle_list
    logical :: found
    integer :: flv
    integer :: n_out
    type(string_t), dimension(:), allocatable :: buf
    integer :: i_buf

    allocate (pl_in (1), pl_out (1))
    found = .false.
    flv = 0
 
    pl_in(1) = generator%pl_in
    pl_out(1) = generator%pl_out
    
    call pl_in(1)%create_pdg_array (pdg_in)
    call pl_out(1)%create_pdg_array (pdg_out)

    call if_table%init &
         (generator%radiation_model, pl_in, pl_out, generator%constraints)
    call if_table%radiate (generator%constraints)

    do i = 1, if_table%get_length ()
      call if_table%get_pdg_out (i, pdg_tmp)
      if (size (pdg_tmp) == generator%n_tot) then
         call if_table%get_particle_string (i, 2, &
            prt_in0(flv+1)%prt, prt_out0(flv+1)%prt)
         call pdg_reshuffle (pdg_out, pdg_tmp, reshuffle_list_local)
         call reshuffle_list%append (reshuffle_list_local)
         found = .true.
         flv = flv+1
      end if
    end do

    if (found) then
      do i = 1, flv
         allocate (prt_in(i)%prt (generator%n_in))
         allocate (prt_out(i)%prt (generator%n_tot-generator%n_in))
      end do
      allocate (prt_tot_in (generator%n_in))
      allocate (prt_tot_out (generator%n_tot-generator%n_in))
      allocate (buf (generator%n_tot))
      buf = ""

      do j = 1, flv
         do i = 1, generator%n_in
            prt_in(j)%prt(i) = prt_in0(j)%prt(i)
            call fill_buffer (buf(i), prt_in0(j)%prt(i))
         end do
      end do
      prt_tot_in = buf(1:generator%n_in)

      do j = 1, flv
         reshuffle_list_local = reshuffle_list%get(j)
         do i = 1, size (reshuffle_list_local)
            prt_out(j)%prt(reshuffle_list_local(i)) = prt_out0(j)%prt(i)
            i_buf = reshuffle_list_local(i) + generator%n_in
            call fill_buffer (buf(i_buf), &
                              prt_out(j)%prt(reshuffle_list_local(i)))
         end do
      end do
      prt_tot_out = buf(generator%n_in+1:generator%n_tot)
    else
      call msg_fatal ("No NLO QCD corrections for this process!")
    end if
  contains
    subroutine pdg_reshuffle (pdg_born, pdg_real, list)
      type(pdg_array_t), intent(in), dimension(:) :: pdg_born, pdg_real
      integer, intent(out), dimension(:), allocatable :: list
      type(pdg_sorter_t), dimension(:), allocatable :: sort_born
      type(pdg_sorter_t), dimension(:), allocatable :: sort_real
      integer :: i, i_min
      integer :: n_born, n_real
      integer :: ib, ir
      logical :: check
      integer, parameter :: n_in = 2
 
      n_born = size (pdg_born); n_real = size (pdg_real)
      allocate (list (n_real-n_in))
      allocate (sort_born (n_born))
      allocate (sort_real (n_real-n_in))

      sort_born%pdg = pdg_born%get ()
      sort_real%pdg = pdg_real(3:n_real)%get ()

      do ib = 1, n_born
         if (any (sort_born(ib)%pdg == sort_real%pdg)) &
            call associate_born_indices (sort_born(ib), sort_real, ib, n_real)
      end do

      i_min = maxval (sort_real%associated_born) + 1

      do ir = 1, n_real-2
         if (sort_real(ir)%associated_born == 0) then
            sort_real(ir)%associated_born = i_min
            i_min = i_min+1
         end if
      end do

      list = sort_real%associated_born

    end subroutine pdg_reshuffle 

    subroutine associate_born_indices (sort_born, sort_real, ib, n_real)
      type(pdg_sorter_t), intent(in) :: sort_born
      type(pdg_sorter_t), intent(inout), dimension(:) :: sort_real
      integer, intent(in) :: ib, n_real
      integer :: ir
      
      do ir = 1, n_real-2
         if (sort_born%pdg == sort_real(ir)%pdg &
            .and..not. sort_real(ir)%checked) then
            sort_real(ir)%associated_born = ib
            sort_real(ir)%checked = .true.
            exit
        end if
      end do
    end subroutine associate_born_indices            

    subroutine fill_buffer (buffer, particle)
      type(string_t), intent(inout) :: buffer
      type(string_t), intent(in) :: particle
      logical :: particle_present
      if (len(buffer) > 0) then
         particle_present = check_for_substring (char(buffer), char(particle))
         if (.not. particle_present) buffer = buffer // ":" // particle
      else
         buffer = buffer // particle
      end if
    end subroutine fill_buffer

    function check_for_substring (buffer, substring) result (exist)
      character(len=*), intent(in) :: buffer
      character(len=*), intent(in) :: substring
      character(len=50) :: buffer_internal
      logical :: exist
      integer :: i_first, i_last
      exist = .false.
      i_first = 1; i_last = 1
      do  
         if (buffer(i_last:i_last) == ":") then
            buffer_internal = buffer (i_first:i_last-1)
            if (buffer_internal == substring) then
               exist = .true.
               exit
            end if
            i_first = i_last+1; i_last = i_first+1
            if (i_last > len(buffer)) exit
         else if (i_last == len(buffer)) then
            buffer_internal = buffer (i_first:i_last)
            exist = (buffer_internal == substring)
            exit
         else
            i_last = i_last+1
            if (i_last > len(buffer)) exit
         end if
      end do
    end function check_for_substring
  end subroutine radiation_generator_generate

  function radiation_generator_get_raw_states (generator) result (raw_states)
    class(radiation_generator_t), intent(in), target :: generator
    integer, dimension(:,:), allocatable :: raw_states
    type(pdg_states_t), pointer :: state
    integer :: n_states, n_particles
    integer :: i_state
    integer :: j
    state => generator%pdg_raw
    n_states = generator%pdg_raw%get_n_states ()
    n_particles = size (generator%pdg_raw%pdg)
    allocate (raw_states (n_particles, n_states))
    do i_state = 1, n_states
      do j = 1, n_particles
        raw_states (j, i_state) = state%pdg(j)%get ()
      end do
        state => state%next
    end do
  end function radiation_generator_get_raw_states

  subroutine radiation_generator_save_born_raw (generator, pdg_in, pdg_out)
    class(radiation_generator_t), intent(inout) :: generator
    type(pdg_array_t), dimension(:), allocatable, intent(in) :: pdg_in, pdg_out
    integer :: i 
    !!! !!! !!! Explicit allocation due to gfortran 4.7.4  
    allocate (generator%pdg_in_born (size (pdg_in))) 
    do i = 1, size (pdg_in) 
       generator%pdg_in_born(i) = pdg_in(i) 
    end do 
    allocate (generator%pdg_out_born (size (pdg_out))) 
    do i = 1, size (pdg_out) 
       generator%pdg_out_born(i) = pdg_out(i) 
    end do 
  end subroutine radiation_generator_save_born_raw
  function radiation_generator_get_born_raw (generator) result (flv_born)
    class(radiation_generator_t), intent(in) :: generator
    integer, dimension(:,:), allocatable :: flv_born
    integer :: i_part, n_particles
    n_particles = size (generator%pdg_in_born) + size (generator%pdg_out_born)
    allocate (flv_born (n_particles, 1))
    flv_born(1,1) = generator%pdg_in_born(1)%get ()
    flv_born(2,1) = generator%pdg_in_born(2)%get ()
    do i_part = 3, n_particles
      flv_born(i_part, 1) = generator%pdg_out_born(i_part-2)%get ()
    end do
  end function radiation_generator_get_born_raw

  subroutine radiation_generator_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test(radiation_generator_1, "radiation_generator_1", &
       "Test the generator of N+1-particle flavor structures", &
       u, results)
  end subroutine radiation_generator_test

  subroutine radiation_generator_1 (u)
    integer, intent(in) :: u
    type (radiation_generator_t) :: generator
    type(pdg_array_t), dimension(:), allocatable :: pdg_in, pdg_out
    type(string_t), dimension(:), allocatable :: prt_strings_in
    type(string_t), dimension(:), allocatable :: prt_strings_out
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: radiation_model => null ()
    integer :: i
    
    write (u, "(A)") "* Test output: radiation_generator_1"
    write (u, "(A)") "* Purpose: Create N+1-particle flavor structures from predefined N-particle flavor structures"
    write (u, "(A)") "* One additional strong coupling, no additional electroweak coupling"
    write (u, "(A)")
    write (u, "(A)") "* Loading radiation model: SM_rad.mdl"

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
       (var_str ("SM_rad"), var_str ("SM_rad.mdl"), &
        os_data, radiation_model)
    call generator%init_radiation_model (radiation_model)
    write (u, "(A)") "* Success"    

    allocate (pdg_in (2))
    pdg_in(1) = 11; pdg_in(2) = -11    
    
    write (u, "(A)") "* Start checking processes"
    call write_separator (u)    

    write (u, "(A)") "* Process 1: Quark-antiquark production"
    allocate (pdg_out(2))
    pdg_out(1) = 2; pdg_out(2) = -2
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 2: Quark-antiquark production with additional gluon"
    allocate (pdg_out(3))
    pdg_out(1) = 2; pdg_out(2) = -2; pdg_out(3) = 21 
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 3: Z + jets"
    allocate (pdg_out(3))
    pdg_out(1) = 2; pdg_out(2) = -2; pdg_out(3) = 23
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)
    
    write (u, "(A)") "* Process 4: Top Decay"
    allocate (pdg_out(4))
    pdg_out(1) = 24; pdg_out(2) = -24
    pdg_out(3) = 5; pdg_out(4) = -5
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 5: Production of four quarks"
    allocate (pdg_out(4))
    pdg_out(1) = 2; pdg_out(2) = -2;
    pdg_out(3) = 2; pdg_out(4) = -2
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

    write (u, "(A)") "* Process 6: Drell-Yan lepto-production"
    allocate (pdg_in (2)); allocate (pdg_out (2))
    pdg_in(1) = 2; pdg_in(2) = -2
    pdg_out(1) = 11; pdg_out(2) = -11
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

    write (u, "(A)") "* Process 7: WZ production at hadron-colliders"
    allocate (pdg_in (2)); allocate (pdg_out (2))
    pdg_in(1) = 1; pdg_in(2) = -2
    pdg_out(1) = -24; pdg_out(2) = 23
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

  contains
    subroutine test_process (generator, pdg_in, pdg_out, u)
      type(radiation_generator_t), intent(inout) :: generator
      type(pdg_array_t), dimension(:), intent(in) :: pdg_in, pdg_out
      integer, intent(in) :: u
      type(string_t), dimension(:), allocatable :: prt_strings_in
      type(string_t), dimension(:), allocatable :: prt_strings_out
      write (u, "(A)") "* Leading order: "
      write (u, "(A)", advance = 'no') '* Incoming: '
      call write_pdg_array (pdg_in, u)
      write (u, "(A)", advance = 'no') '* Outgoing: '
      call write_pdg_array (pdg_out, u)

      call generator%init (pdg_in, pdg_out, qcd = .true., qed = .false.)
      call generator%set_n (2, size(pdg_out), 0)
      call generator%set_constraints (.false., .false., .true., .true.)
      call generator%generate (prt_strings_in, prt_strings_out)
      write (u, "(A)") "* Additional radiation: "
      write (u, "(A)") "* Incoming: "
      call write_particle_string (prt_strings_in, u)
      write (u, "(A)") "* Outgoing: "
      call write_particle_string (prt_strings_out, u) 
      call write_separator(u)
    end subroutine test_process

    subroutine write_pdg_array (pdg, u)
      type(pdg_array_t), dimension(:), intent(in) :: pdg
      integer, intent(in) :: u
      integer :: i
      do i = 1, size (pdg)
         call pdg(i)%write (u)
      end do
      write (u, "(A)")
    end subroutine write_pdg_array
 
    subroutine write_particle_string (prt, u)
      type(string_t), dimension(:), intent(in) :: prt
      integer, intent(in) :: u
      integer :: i
      do i = 1, size (prt)
         write (u, "(A,1X)", advance = "no") char (prt(i))
      end do
      write (u, "(A)") 
    end subroutine write_particle_string
  end subroutine radiation_generator_1


end module radiation_generator
