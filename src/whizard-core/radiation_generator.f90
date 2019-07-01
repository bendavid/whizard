! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use os_interface
  use models
  use pdg_arrays
  use particle_specifiers
  use auto_components
  use physics_defs

  implicit none
  private

  public :: radiation_generator_init
  public :: radiation_generator_t
  
  type :: pdg_sorter_t
     integer :: pdg
  end type pdg_sorter_t

  type, extends (pdg_sorter_t) :: pdg_sorter_born_t
     logical :: checked
  end type pdg_sorter_born_t

  type, extends (pdg_sorter_t) :: pdg_sorter_real_t
     integer :: associated_born 
  end type pdg_sorter_real_t

  type :: pdg_states_t
    type(pdg_array_t), dimension(:), allocatable :: pdg
    type(pdg_states_t), pointer :: next
    integer :: n_particles
  contains
    procedure :: init => pdg_states_init
    procedure :: add => pdg_states_add
    procedure :: get_n_states => pdg_states_get_n_states
  end type pdg_states_t

  type :: radiation_generator_t
    logical :: qcd_enabled = .false.
    logical :: qed_enabled = .false.
    logical :: is_gluon = .false.
    logical :: fs_gluon = .false.
    type(pdg_list_t) :: pl_in, pl_out
    type(split_constraints_t) :: constraints
    integer :: n_tot
    integer :: n_out
    integer :: n_loops
    integer :: n_light_quarks
    real(default) :: mass_sum
    type(model_t), pointer :: radiation_model
    type(pdg_states_t) :: pdg_raw
    type(pdg_array_t), dimension(:), allocatable :: pdg_in_born, pdg_out_born
  contains
    procedure :: init_radiation_model => &
                      radiation_generator_init_radiation_model
    procedure :: set_n => radiation_generator_set_n
    procedure :: set_constraints => radiation_generator_set_constraints
    procedure :: generate => radiation_generator_generate
    procedure :: get_raw_states => radiation_generator_get_raw_states
    procedure :: save_born_raw => radiation_generator_save_born_raw
    procedure :: get_born_raw => radiation_generator_get_born_raw
  end type radiation_generator_t


  interface radiation_generator_init
     module procedure radiation_generator_init_pdg_list
     module procedure radiation_generator_init_pdg_array
  end interface


contains

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
       (generator, qcd, qed, pl_in, pl_out)
    type(radiation_generator_t), intent(inout) :: generator
    logical, intent(in), optional :: qcd, qed
    type(pdg_list_t), intent(in) :: pl_in, pl_out
    if (present (qcd))  generator%qcd_enabled = qcd
    if (present (qed))  generator%qed_enabled = qed
    generator%pl_in = pl_in
    generator%pl_out = pl_out
    generator%is_gluon = pl_in%search_for_particle (GLUON)
    generator%fs_gluon = pl_out%search_for_particle (GLUON)
    call generator%pdg_raw%init ()
  end subroutine radiation_generator_init_pdg_list

  subroutine radiation_generator_init_pdg_array &
       (generator, qcd, qed, pdg_in, pdg_out)
    type(radiation_generator_t), intent(inout) :: generator
    logical, intent(in), optional :: qcd, qed
    type(pdg_array_t), intent(in), dimension(:), allocatable :: pdg_in, pdg_out
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
    call radiation_generator_init (generator, qcd, qed, pl_in, pl_out)
  end subroutine radiation_generator_init_pdg_array

  subroutine radiation_generator_init_radiation_model (generator, os_data)
    class(radiation_generator_t), intent(inout) :: generator
    type(os_data_t), intent(in) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    call model_list%read_model (var_str ("SM_rad"), var_str ("SM_rad.mdl"), &
                                os_data, model)
    generator%radiation_model => model
  end subroutine radiation_generator_init_radiation_model

  subroutine radiation_generator_set_n (generator, n_in, n_out, n_loops)
    class(radiation_generator_t), intent(inout) :: generator
    integer, intent(in) :: n_in, n_out, n_loops
    generator%n_tot = n_in + n_out + 1
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
    integer :: i, n, n_constraints
    type(pdg_list_t) :: pl_req, pl_insert
    type(pdg_array_t) :: pdg_gluon, pdg_photon
    type(pdg_array_t) :: pdg_add, pdg_tmp
    integer :: i_skip
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
        call pl_insert%init (generator%n_out+1)
        do i = 1, generator%n_out
           call pl_insert%set(i, generator%pl_out%get(i))
        end do
        pdg_gluon = GLUON; pdg_photon = PHOTON
        if (generator%fs_gluon .and. generator%qcd_enabled) then
           do i = 1, generator%n_light_quarks
              pdg_tmp = i
              pdg_add = pdg_add // pdg_tmp
           end do
        end if
        if (generator%qcd_enabled) pdg_add = pdg_add // pdg_gluon
        if (generator%qed_enabled) pdg_add = pdg_add // pdg_photon
        call pl_insert%set (generator%n_out+1, pdg_add)
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
    type(prt_array_t), dimension(n_flv_max) :: prt_out0
    type(pdg_array_t), dimension(:), allocatable :: pdg_tmp, pdg_out, pdg_in, pdg_raw
    type(if_table_t) :: if_table
    type(pdg_list_t), dimension(:), allocatable :: pl_in, pl_out
    integer :: i, j
    integer, dimension(:), allocatable :: reshuffle_list
    logical :: found
    integer :: flv = 0
    integer :: n_out
    type(string_t), dimension(:), allocatable :: buf

    allocate (pl_in (1), pl_out (1))
    found = .false.
 
    pl_in(1) = generator%pl_in
    pl_out(1) = generator%pl_out
    
    call pl_in(1)%create_pdg_array (pdg_in)
    call pl_out(1)%create_pdg_array (pdg_out)
    call generator%save_born_raw (pdg_in, pdg_out)

    call if_table%init &
         (generator%radiation_model, pl_in, pl_out, generator%constraints)
    call if_table%radiate (generator%constraints)

    allocate (pdg_raw (generator%n_tot))

    do i = 1, if_table%get_length ()
      call if_table%get_pdg_out (i, pdg_tmp)
      if (size (pdg_tmp) == generator%n_tot) then
         call if_table%get_particle_string (i, 2, prt_tot_in, prt_out0(flv+1)%prt)
         call pdg_reshuffle (pdg_out, pdg_tmp, reshuffle_list)
         pdg_raw(1:2) = pdg_tmp(1:2)
         do j = 1, size (reshuffle_list)
            pdg_raw(reshuffle_list(j)+2) = pdg_tmp(j+2)
         end do
         call generator%pdg_raw%add (pdg_raw)
         found = .true.
         flv = flv+1
      end if
    end do

    if (found) then
      do i = 1, flv
         allocate (prt_out(i)%prt (generator%n_tot-2))
      end do
      allocate (prt_tot_out (generator%n_tot-2))
      allocate (buf (generator%n_tot-2))
      buf = ""

      do j = 1, flv
         do i = 1, size (reshuffle_list)
            prt_out(j)%prt(reshuffle_list(i)) = prt_out0(j)%prt(i)
            buf(i) = buf(i) // prt_out(j)%prt(i)
            if (j /= flv) buf(i) = buf(i) // ":"
         end do
      end do
      prt_tot_out = buf
    else
      call msg_fatal ("No QCD corrections for this process!")
    end if
    deallocate (pdg_raw)
  contains
    subroutine pdg_reshuffle (pdg_born, pdg_real, list)
      type(pdg_array_t), intent(in), dimension(:) :: pdg_born, pdg_real
      integer, intent(out), dimension(:), allocatable :: list
      type(pdg_sorter_born_t), dimension(:), allocatable :: sort_born
      type(pdg_sorter_real_t), dimension(:), allocatable :: sort_real
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
      sort_born%checked = .false.
      sort_real%associated_born = 0

      do ib = 1, n_born
         sort_born(ib)%checked = any (sort_born(ib)%pdg == sort_real%pdg)
         if (sort_born(ib)%checked) then
            do ir = 1, n_real-2
               if (sort_born(ib)%pdg == sort_real(ir)%pdg) then
                  sort_real(ir)%associated_born = ib
                  exit
               end if
            end do
         end if
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


end module radiation_generator
