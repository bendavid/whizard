! WHIZARD 2.3.1 Aug 25 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module auto_components

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use model_data
  use pdg_arrays

  implicit none
  private

  public :: split_constraints_t
  public :: constrain_n_tot
  public :: constrain_n_loop
  public :: constrain_insert
  public :: constrain_require
  public :: constrain_radiation
  public :: constrain_mass_sum
  public :: constrain_in_state
  public :: ps_table_t
  public :: ds_table_t
  public :: fs_table_t
  public :: if_table_t

  integer, parameter :: PROC_UNDEFINED = 0
  integer, parameter :: PROC_DECAY = 1
  integer, parameter :: PROC_SCATTER = 2


  type, abstract :: split_constraint_t
   contains
     procedure :: check_before_split  => split_constraint_check_before_split
     procedure :: check_before_insert => split_constraint_check_before_insert
     procedure :: check_before_record => split_constraint_check_before_record
  end type split_constraint_t

  type :: split_constraint_wrap_t
     class(split_constraint_t), allocatable :: c
  end type split_constraint_wrap_t
  
  type :: split_constraints_t
     class(split_constraint_wrap_t), dimension(:), allocatable :: cc
   contains
     procedure :: init => split_constraints_init
     procedure :: set => split_constraints_set
     procedure :: check_before_split  => split_constraints_check_before_split
     procedure :: check_before_insert => split_constraints_check_before_insert
     procedure :: check_before_record => split_constraints_check_before_record
  end type split_constraints_t
  
  type, extends (split_constraint_t) :: constraint_n_tot
     private
     integer :: n_max = 0
   contains
     procedure :: check_before_split => constraint_n_tot_check_before_split
     procedure :: check_before_record => constraint_n_tot_check_before_record
  end type constraint_n_tot
  
  type, extends (split_constraint_t) :: constraint_n_loop
     private
     integer :: n_loop_max = 0
   contains
     procedure :: check_before_record => constraint_n_loop_check_before_record
  end type constraint_n_loop
  
  type, extends (split_constraint_t) :: constraint_insert
     private
     type(pdg_list_t) :: pl_match
   contains
     procedure :: check_before_insert => constraint_insert_check_before_insert
  end type constraint_insert
  
  type, extends (split_constraint_t) :: constraint_require
     private
     type(pdg_list_t) :: pl
   contains
     procedure :: check_before_record => constraint_require_check_before_record
  end type constraint_require
  
  type, extends (split_constraint_t) :: constraint_radiation
     private
   contains
     procedure :: check_before_insert => &
          constraint_radiation_check_before_insert
  end type constraint_radiation
  
  type, extends (split_constraint_t) :: constraint_mass_sum
     private
     real(default) :: mass_limit = 0
     logical :: strictly_less = .false.
     real(default) :: margin = 0
   contains
     procedure :: check_before_record => constraint_mass_sum_check_before_record
  end type constraint_mass_sum
  
  type, extends (split_constraint_t) :: constraint_in_state
     private
     type(pdg_list_t) :: pl
   contains
     procedure :: check_before_record => constraint_in_state_check_before_record
  end type constraint_in_state

  type, extends (pdg_list_t) :: ps_entry_t
     integer :: n_loop = 0
     integer :: n_rad = 0
     type(ps_entry_t), pointer :: previous => null ()
     type(ps_entry_t), pointer :: next => null ()
  end type ps_entry_t

  type, abstract :: ps_table_t
     private
     class(model_data_t), pointer :: model => null ()
     logical :: loops = .false.
     type(ps_entry_t), pointer :: first => null ()
     type(ps_entry_t), pointer :: last => null ()
     integer :: proc_type
   contains
     procedure :: final => ps_table_final
     procedure :: base_write => ps_table_base_write
     procedure (ps_table_write), deferred :: write
     procedure :: get_particle_string => ps_table_get_particle_string
     generic :: init => ps_table_init
     procedure, private :: ps_table_init
     procedure :: enable_loops => ps_table_enable_loops
     procedure :: split => ps_table_split
     procedure :: insert => ps_table_insert
     procedure :: record_sorted => ps_table_record_sorted
     procedure :: record => ps_table_record
     procedure :: get_length => ps_table_get_length
     procedure :: get_emitters => ps_table_get_emitters
     procedure :: get_pdg_out => ps_table_get_pdg_out
  end type ps_table_t
     
  type, extends (ps_table_t) :: ds_table_t
     private
     integer :: pdg_in = 0
   contains
     procedure :: write => ds_table_write
     procedure :: make => ds_table_make
  end type ds_table_t

  type, extends (ps_table_t) :: fs_table_t
   contains
     procedure :: write => fs_table_write
     procedure :: radiate => fs_table_radiate
  end type fs_table_t

  type, extends (fs_table_t) :: if_table_t
   contains
     procedure :: write => if_table_write
     generic :: init => if_table_init
     procedure, private :: if_table_init
     procedure :: insert => if_table_insert
     procedure :: record_sorted => if_table_record_sorted
  end type if_table_t


  interface
     subroutine ps_table_write (object, unit)
       import
       class(ps_table_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine ps_table_write
  end interface

contains

  subroutine split_constraint_check_before_split (c, table, pl, k, passed)
    class(split_constraint_t), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: k
    logical, intent(out) :: passed
    passed = .true.
  end subroutine split_constraint_check_before_split
  
  subroutine split_constraint_check_before_insert (c, table, pa, pl, passed)
    class(split_constraint_t), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_array_t), intent(in) :: pa
    type(pdg_list_t), intent(inout) :: pl
    logical, intent(out) :: passed
    passed = .true.
  end subroutine split_constraint_check_before_insert
  
  subroutine split_constraint_check_before_record (c, table, pl, n_loop, passed)
    class(split_constraint_t), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    passed = .true.
  end subroutine split_constraint_check_before_record
  
  subroutine split_constraints_init (constraints, n)
    class(split_constraints_t), intent(out) :: constraints
    integer, intent(in) :: n
    allocate (constraints%cc (n))
  end subroutine split_constraints_init
  
  subroutine split_constraints_set (constraints, i, c)
    class(split_constraints_t), intent(inout) :: constraints
    integer, intent(in) :: i
    class(split_constraint_t), intent(in) :: c
    allocate (constraints%cc(i)%c, source = c)
  end subroutine split_constraints_set
  
  subroutine split_constraints_check_before_split &
       (constraints, table, pl, k, passed)
    class(split_constraints_t), intent(in) :: constraints
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: k
    logical, intent(out) :: passed
    integer :: i
    passed = .true.
    do i = 1, size (constraints%cc)
       call constraints%cc(i)%c%check_before_split (table, pl, k, passed)
       if (.not. passed)  return
    end do
  end subroutine split_constraints_check_before_split
    
  subroutine split_constraints_check_before_insert &
       (constraints, table, pa, pl, passed)
    class(split_constraints_t), intent(in) :: constraints
    class(ps_table_t), intent(in) :: table
    type(pdg_array_t), intent(in) :: pa
    type(pdg_list_t), intent(inout) :: pl
    logical, intent(out) :: passed
    integer :: i
    passed = .true.
    do i = 1, size (constraints%cc)
       call constraints%cc(i)%c%check_before_insert (table, pa, pl, passed)
       if (.not. passed)  return
    end do
  end subroutine split_constraints_check_before_insert
    
  subroutine split_constraints_check_before_record &
       (constraints, table, pl, n_loop, passed)
    class(split_constraints_t), intent(in) :: constraints
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    integer :: i
    passed = .true.
    do i = 1, size (constraints%cc)
       call constraints%cc(i)%c%check_before_record (table, pl, n_loop, passed)
       if (.not. passed)  return
    end do
  end subroutine split_constraints_check_before_record
    
  function constrain_n_tot (n_max) result (c)
    integer, intent(in) :: n_max
    type(constraint_n_tot) :: c
    c%n_max = n_max
  end function constrain_n_tot
  
  subroutine constraint_n_tot_check_before_split (c, table, pl, k, passed)
    class(constraint_n_tot), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: k
    logical, intent(out) :: passed
    passed = pl%get_size () < c%n_max
  end subroutine constraint_n_tot_check_before_split

  subroutine constraint_n_tot_check_before_record (c, table, pl, n_loop, passed)
    class(constraint_n_tot), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    passed = pl%get_size () + n_loop <= c%n_max
  end subroutine constraint_n_tot_check_before_record

  function constrain_n_loop (n_loop_max) result (c)
    integer, intent(in) :: n_loop_max
    type(constraint_n_loop) :: c
    c%n_loop_max = n_loop_max
  end function constrain_n_loop

  subroutine constraint_n_loop_check_before_record &
       (c, table, pl, n_loop, passed)
    class(constraint_n_loop), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    passed = n_loop <= c%n_loop_max
  end subroutine constraint_n_loop_check_before_record

  function constrain_insert (pl_match) result (c)
    type(pdg_list_t), intent(in) :: pl_match
    type(constraint_insert) :: c
    c%pl_match = pl_match
  end function constrain_insert
  
  subroutine constraint_insert_check_before_insert (c, table, pa, pl, passed)
    class(constraint_insert), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_array_t), intent(in) :: pa
    type(pdg_list_t), intent(inout) :: pl
    logical, intent(out) :: passed
    call pl%match_replace (c%pl_match, passed)
  end subroutine constraint_insert_check_before_insert

  function constrain_require (pl) result (c)
    type(pdg_list_t), intent(in) :: pl
    type(constraint_require) :: c
    c%pl = pl
  end function constrain_require
  
  subroutine constraint_require_check_before_record &
       (c, table, pl, n_loop, passed)
    class(constraint_require), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    logical, dimension(:), allocatable :: mask
    integer :: i, k, n_in
    select type (table)
    type is (if_table_t)
       if (table%proc_type > 0) then
          select case (table%proc_type)
          case (PROC_DECAY)
             n_in = 1
          case (PROC_SCATTER)
             n_in = 2
          end select
       else
          call msg_fatal ("Neither a decay nor a scattering process")
       end if
    class default
       n_in = 0
    end select
    allocate (mask (c%pl%get_size ()), source = .true.)
    do i = n_in + 1, pl%get_size ()
       k = c%pl%find_match (pl%get (i), mask)
       if (k /= 0)  mask(k) = .false.
    end do
    passed = .not. any (mask)
  end subroutine constraint_require_check_before_record

  function constrain_radiation () result (c)
    type(constraint_radiation) :: c
  end function constrain_radiation
  
  subroutine constraint_radiation_check_before_insert (c, table, pa, pl, passed)
    class(constraint_radiation), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_array_t), intent(in) :: pa
    type(pdg_list_t), intent(inout) :: pl
    logical, intent(out) :: passed
    passed = .not. (pl .match. pa)
  end subroutine constraint_radiation_check_before_insert

  function constrain_mass_sum (mass_limit, margin) result (c)
    real(default), intent(in) :: mass_limit
    real(default), intent(in), optional :: margin
    type(constraint_mass_sum) :: c
    c%mass_limit = mass_limit
    if (present (margin)) then
       c%strictly_less = .true.
       c%margin = margin
    end if
  end function constrain_mass_sum
  
  subroutine constraint_mass_sum_check_before_record &
       (c, table, pl, n_loop, passed)
    class(constraint_mass_sum), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    real(default) :: limit
    if (c%strictly_less) then
       limit = c%mass_limit - c%margin
       select type (table)
       type is (if_table_t)
          passed = mass_sum (pl, 1, 2, table%model) < limit  &
               .and. mass_sum (pl, 3, pl%get_size (), table%model) < limit
       class default
          passed = mass_sum (pl, 1, pl%get_size (), table%model) < limit
       end select
    else
       limit = c%mass_limit
       select type (table)
       type is (if_table_t)
          passed = mass_sum (pl, 1, 2, table%model) <= limit &
               .and. mass_sum (pl, 3, pl%get_size (), table%model) <= limit
       class default
          passed = mass_sum (pl, 1, pl%get_size (), table%model) <= limit
       end select
    end if
  end subroutine constraint_mass_sum_check_before_record

  function constrain_in_state (pl) result (c)
    type(pdg_list_t), intent(in) :: pl
    type(constraint_in_state) :: c
    c%pl = pl
  end function constrain_in_state

  subroutine constraint_in_state_check_before_record &
       (c, table, pl, n_loop, passed)
    class(constraint_in_state), intent(in) :: c
    class(ps_table_t), intent(in) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop
    logical, intent(out) :: passed
    integer :: i
    select type (table)
    type is (if_table_t)
       passed = .false.
       do i = 1, 2
          if (.not. (c%pl .match. pl%get (i)))  return
       end do
    end select
    passed = .true.
  end subroutine constraint_in_state_check_before_record
  
  subroutine ps_table_final (object)
    class(ps_table_t), intent(inout) :: object
    type(ps_entry_t), pointer :: current
    do while (associated (object%first))
       current => object%first
       object%first => current%next
       deallocate (current)
    end do
    nullify (object%last)
  end subroutine ps_table_final
  
  subroutine ps_table_base_write (object, unit, n_in)
    class(ps_table_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: n_in
    integer, dimension(:), allocatable :: pdg
    type(ps_entry_t), pointer :: entry
    type(field_data_t), pointer :: prt
    integer :: u, i, j, n0
    u = given_output_unit (unit)
    entry => object%first
    do while (associated (entry))
       write (u, "(2x)", advance = "no")
       if (present (n_in)) then
          do i = 1, n_in
             write (u, "(1x)", advance = "no")
             pdg = entry%get (i)
             do j = 1, size (pdg)
                prt => object%model%get_field_ptr (pdg(j))
                if (j > 1)  write (u, "(':')", advance = "no")
                write (u, "(A)", advance = "no") &
                     char (prt%get_name (pdg(j) >= 0))
             end do
          end do
          write (u, "(1x,A)", advance = "no")  "=>"
          n0 = n_in + 1
       else
          n0 = 1
       end if
       do i = n0, entry%get_size ()
          write (u, "(1x)", advance = "no")
          pdg = entry%get (i)
          do j = 1, size (pdg)
             prt => object%model%get_field_ptr (pdg(j))
             if (j > 1)  write (u, "(':')", advance = "no")
             write (u, "(A)", advance = "no") &
                  char (prt%get_name (pdg(j) < 0))
          end do
       end do
       if (object%loops) then
          write (u, "(2x,'[',I0,',',I0,']')")  entry%n_loop, entry%n_rad
       else
          write (u, *)
       end if
       entry => entry%next
    end do
  end subroutine ps_table_base_write
          
  subroutine ds_table_write (object, unit)
    class(ds_table_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: prt
    integer :: u
    u = given_output_unit (unit)
    prt => object%model%get_field_ptr (object%pdg_in)
    write (u, "(1x,A,1x,A)")  "Decays for particle:", &
         char (prt%get_name (object%pdg_in < 0))
    call object%base_write (u)
  end subroutine ds_table_write
          
  subroutine fs_table_write (object, unit)
    class(fs_table_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Table of final states:"
    call object%base_write (u)
  end subroutine fs_table_write
          
  subroutine if_table_write (object, unit)
    class(if_table_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Table of in/out states:"
    select case (object%proc_type)
    case (PROC_DECAY)
       call object%base_write (u, n_in = 1)
    case (PROC_SCATTER)
       call object%base_write (u, n_in = 2)
     end select
  end subroutine if_table_write
          
  subroutine ps_table_get_particle_string (object, index, prt_in, prt_out)
    class(ps_table_t), intent(in) :: object
    integer, intent(in) :: index
    type(string_t), intent(out), dimension(:), allocatable :: prt_in, prt_out
    integer :: n_in
    type(field_data_t), pointer :: prt
    type(ps_entry_t), pointer :: entry
    integer, dimension(:), allocatable :: pdg
    integer :: n0
    integer :: i, j 
    entry => object%first
    i = 1
    do while (i < index)
      if (associated (entry%next)) then
        entry => entry%next
        i=i+1
      else
        call msg_fatal ("ps_table: entry with requested index does not exist!")
      end if
    end do

    if (object%proc_type > 0) then
       select case (object%proc_type)
       case (PROC_DECAY)
          n_in = 1
       case (PROC_SCATTER)
          n_in = 2
       end select
    else
       call msg_fatal ("Neither decay nor scattering process")
    end if

    n0 = n_in + 1
    allocate (prt_in (n_in), prt_out (entry%get_size () - n_in))
    do i = 1, n_in
      prt_in(i) = ""
      pdg = entry%get(i)
      do j = 1, size(pdg)
        prt => object%model%get_field_ptr (pdg(j))
        prt_in(i) = prt_in(i) // prt%get_name (pdg(j) >= 0)
        if (j /= size(pdg)) &
           prt_in(i) = prt_in(i) // ":"
      end do
    end do
    do i = n0, entry%get_size ()
      prt_out(i-n_in) = ""
      pdg = entry%get(i)
      do j = 1, size(pdg)
         prt => object%model%get_field_ptr (pdg(j))
         prt_out(i-n_in) = prt_out(i-n_in) // prt%get_name (pdg(j) < 0)
         if (j /= size(pdg)) &
            prt_out(i-n_in) = prt_out(i-n_in) // ":"
      end do
    end do
  end subroutine ps_table_get_particle_string
                        
  subroutine ps_table_init (table, model, pl, constraints, n_in)
    class(ps_table_t), intent(out) :: table
    class(model_data_t), intent(in), target :: model
    type(pdg_list_t), dimension(:), intent(in) :: pl
    type(split_constraints_t), intent(in) :: constraints
    integer, intent(in), optional :: n_in
    logical :: passed
    integer :: i
    table%model => model

    if (present (n_in)) then
       select case (n_in)
       case (1) 
          table%proc_type = PROC_DECAY
       case (2) 
          table%proc_type = PROC_SCATTER
       case default 
          table%proc_type = PROC_UNDEFINED
       end select
    else
       table%proc_type = PROC_UNDEFINED
    end if

    do i = 1, size (pl)
       call table%record (pl(i), 0, 0, constraints, passed)
       if (.not. passed) then
          call msg_fatal ("ps_table: Registering process components failed")
       end if
    end do
  end subroutine ps_table_init
    
  subroutine if_table_init (table, model, pl_in, pl_out, constraints)
    class(if_table_t), intent(out) :: table
    class(model_data_t), intent(in), target :: model
    type(pdg_list_t), dimension(:), intent(in) :: pl_in, pl_out
    type(split_constraints_t), intent(in) :: constraints
    integer :: i, j, k, p, n_in, n_out
    type(pdg_array_t), dimension(:), allocatable :: pa_in
    type(pdg_list_t), dimension(:), allocatable :: pl
    allocate (pl (size (pl_in) * size (pl_out)))
    k = 0
    do i = 1, size (pl_in)
       n_in = pl_in(i)%get_size ()
       allocate (pa_in (n_in))
       do p = 1, n_in
          pa_in(p) = pl_in(i)%get (p)
       end do
       do j = 1, size (pl_out)
          n_out = pl_out(j)%get_size ()
          k = k + 1
          call pl(k)%init (n_in + n_out)
          do p = 1, n_in
             call pl(k)%set (p, invert_pdg_array (pa_in(p), model))
          end do
          do p = 1, n_out
             call pl(k)%set (n_in + p, pl_out(j)%get (p))
          end do
       end do
       deallocate (pa_in)
    end do
    n_in = size (pl_in(1)%a)
    call table%init (model, pl, constraints, n_in)
  end subroutine if_table_init
    
  subroutine ps_table_enable_loops (table)
    class(ps_table_t), intent(inout) :: table
    table%loops = .true.
  end subroutine ps_table_enable_loops
    
  subroutine ds_table_make (table, model, pdg_in, constraints)
    class(ds_table_t), intent(out) :: table
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: pdg_in
    type(split_constraints_t), intent(in) :: constraints
    type(pdg_list_t) :: pl_in
    type(pdg_list_t), dimension(0) :: pl
    call table%init (model, pl, constraints)
    table%pdg_in = pdg_in
    call pl_in%init (1)
    call pl_in%set (1, [pdg_in])
    call table%split (pl_in, 0, constraints)
  end subroutine ds_table_make
    
  subroutine fs_table_radiate (table, constraints)
    class(fs_table_t), intent(inout) :: table
    type(split_constraints_t) :: constraints
    type(ps_entry_t), pointer :: current
    current => table%first
    do while (associated (current))
       call table%split (current, 0, constraints, record = .true.)
       current => current%next
    end do
  end subroutine fs_table_radiate

  recursive subroutine ps_table_split (table, pl, n_rad, constraints, &
        record)
    class(ps_table_t), intent(inout) :: table
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_rad
    type(split_constraints_t), intent(in) :: constraints
    logical, intent(in), optional :: record
    integer :: n_loop, i
    logical :: passed
    type(vertex_iterator_t) :: vit
    integer, dimension(:), allocatable :: pdg1
    integer, dimension(:), allocatable :: pdg2
    if (present (record)) then
       if (record) then
          n_loop = 0
          INCR_LOOPS: do
             call table%record_sorted (pl, n_loop, n_rad, constraints, passed)
             if (.not. passed)  exit INCR_LOOPS
             if (.not. table%loops)  exit INCR_LOOPS
             n_loop = n_loop + 1
          end do INCR_LOOPS
       end if
    end if
    do i = 1, pl%get_size ()
       call constraints%check_before_split (table, pl, i, passed)
       if (passed) then
          pdg1 = pl%get (i)
          call vit%init (table%model, pdg1)
          SCAN_VERTICES: do
             call vit%get_next_match (pdg2)
             if (allocated (pdg2)) then
                call table%insert (pl, n_rad, i, pdg2, constraints)
             else
                exit SCAN_VERTICES
             end if
          end do SCAN_VERTICES
       end if
    end do
  end subroutine ps_table_split
    
  recursive subroutine ps_table_insert &
       (table, pl, n_rad, i, pdg, constraints, n_in)
    class(ps_table_t), intent(inout) :: table
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_rad, i
    integer, dimension(:), intent(in) :: pdg
    type(split_constraints_t), intent(in) :: constraints
    integer, intent(in), optional :: n_in
    type(pdg_list_t) :: pl_insert
    logical :: passed
    integer :: k, s
    s = size (pdg)
    call pl_insert%init (s)
    do k = 1, s
       call pl_insert%set (k, pdg(k))
    end do
    call constraints%check_before_insert (table, pl%get (i), pl_insert, passed)
    if (passed) then
       call table%split (pl%replace (i, pl_insert, n_in), n_rad + s - 1, &
            constraints, record = .true.)
    end if
  end subroutine ps_table_insert
    
  recursive subroutine if_table_insert  &
       (table, pl, n_rad, i, pdg, constraints, n_in)
    class(if_table_t), intent(inout) :: table
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_rad, i
    integer, dimension(:), intent(in) :: pdg
    type(split_constraints_t), intent(in) :: constraints
    integer, intent(in), optional :: n_in
    integer, dimension(:), allocatable :: pdg_work
    integer :: p
    if (i > 2) then
       call ps_table_insert (table, pl, n_rad, i, pdg, constraints)
    else
       allocate (pdg_work (size (pdg)))
       do p = 1, size (pdg)
          pdg_work(1) = pdg(p)
          pdg_work(2:p) = pdg(1:p-1)
          pdg_work(p+1:) = pdg(p+1:)
          select case (table%proc_type)
          case (PROC_DECAY)
             call ps_table_insert (table, &
               pl, n_rad, i, pdg_work, constraints, n_in = 1)
          case (PROC_SCATTER)
             call ps_table_insert (table, &
               pl, n_rad, i, pdg_work, constraints, n_in = 2)
          end select
       end do
    end if
  end subroutine if_table_insert

  subroutine ps_table_record_sorted &
       (table, pl, n_loop, n_rad, constraints, passed)
    class(ps_table_t), intent(inout) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop, n_rad
    type(split_constraints_t), intent(in) :: constraints
    logical, intent(out) :: passed
    call table%record (pl%sort_abs (), n_loop, n_rad, constraints, passed)
  end subroutine ps_table_record_sorted
  
  subroutine if_table_record_sorted &
       (table, pl, n_loop, n_rad, constraints, passed)
    class(if_table_t), intent(inout) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop, n_rad
    type(split_constraints_t), intent(in) :: constraints
    logical, intent(out) :: passed
    call table%record (pl%sort_abs (2), n_loop, n_rad, constraints, passed)
  end subroutine if_table_record_sorted

  subroutine ps_table_record (table, pl, n_loop, n_rad, constraints, passed)
    class(ps_table_t), intent(inout) :: table
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n_loop, n_rad
    type(split_constraints_t), intent(in) :: constraints
    logical, intent(out) :: passed
    type(ps_entry_t), pointer :: current
    passed = .false.
    if (.not. pl%is_regular ()) then
       call msg_warning ("Record ps_table entry: Irregular pdg-list encountered!")
       return
    end if
    call constraints%check_before_record (table, pl, n_loop, passed)
    if (.not. passed)  then
       return
    end if
    current => table%first
    do while (associated (current))
       if (pl == current) then
          if (n_loop == current%n_loop)  return
       else if (pl < current) then
          call insert
          return
       end if
       current => current%next
    end do
    call insert
  contains
    subroutine insert ()
      type(ps_entry_t), pointer :: entry
      allocate (entry)
      entry%pdg_list_t = pl
      entry%n_loop = n_loop
      entry%n_rad = n_rad
      if (associated (current)) then
         if (associated (current%previous)) then
            current%previous%next => entry
            entry%previous => current%previous
         else
            table%first => entry
         end if
         entry%next => current
         current%previous => entry
      else
         if (associated (table%last)) then
            table%last%next => entry
            entry%previous => table%last
         else
            table%first => entry
         end if
         table%last => entry
      end if
    end subroutine insert
  end subroutine ps_table_record
    
  function mass_sum (pl, n1, n2, model) result (m)
    type(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: n1, n2
    class(model_data_t), intent(in), target :: model
    integer, dimension(:), allocatable :: pdg
    real(default) :: m
    type(field_data_t), pointer :: prt
    integer :: i
    m = 0
    do i = n1, n2
       pdg = pl%get (i)
       prt => model%get_field_ptr (pdg(1))
       m = m + prt%get_mass ()
    end do
  end function mass_sum
  
  function invert_pdg_array (pa, model) result (pa_inv)
    type(pdg_array_t), intent(in) :: pa
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t) :: pa_inv
    type(field_data_t), pointer :: prt
    integer :: i, pdg
    pa_inv = pa
    do i = 1, pa_inv%get_length ()
       pdg = pa_inv%get (i)
       prt => model%get_field_ptr (pdg)
       if (prt%has_antiparticle ())  call pa_inv%set (i, -pdg)
    end do
  end function invert_pdg_array
          
  function ps_table_get_length (ps_table) result (n)
    class(ps_table_t), intent(in) :: ps_table
    integer :: n
    type(ps_entry_t), pointer :: entry
    n = 0
    entry => ps_table%first
    do while (associated (entry))
       n = n + 1
       entry => entry%next
    end do
  end function ps_table_get_length

  function ps_table_get_emitters (table, constraints) result (emitters)
     integer, dimension(:), allocatable :: emitters
     class(ps_table_t), intent(in) :: table
     type(split_constraints_t), intent(in) :: constraints
     class(pdg_list_t), pointer :: pl
     integer :: i
     logical :: passed
     type(vertex_iterator_t) :: vit
     integer, dimension(:), allocatable :: pdg1, pdg2
     integer :: n_emitters
     integer, dimension(20) :: emitters_tmp

     n_emitters = 0
     pl => table%first
     do i = 1, pl%get_size ()
        call constraints%check_before_split (table, pl, i, passed)
        if (passed) then
           pdg1 = pl%get(i)
           call vit%init (table%model, pdg1)
           do 
               call vit%get_next_match(pdg2)
               if (allocated (pdg2)) then
                  emitters_tmp (n_emitters+1) = pdg1(1)
                  n_emitters = n_emitters + 1
               else
                  exit
               end if
           end do
        end if
     end do
     allocate (emitters (n_emitters))
     emitters = emitters_tmp (1:n_emitters)
  end function ps_table_get_emitters

  subroutine ps_table_get_pdg_out (ps_table, i, pa_out, n_loop, n_rad)
    class(ps_table_t), intent(in) :: ps_table
    integer, intent(in) :: i
    type(pdg_array_t), dimension(:), allocatable, intent(out) :: pa_out
    integer, intent(out), optional :: n_loop, n_rad
    type(ps_entry_t), pointer :: entry
    integer :: n, j
    n = 0
    entry => ps_table%first
    FIND_ENTRY: do while (associated (entry))
       n = n + 1
       if (n == i) then
          allocate (pa_out (entry%get_size ()))
          do j = 1, entry%get_size ()
             pa_out(j) = entry%get (j)
             if (present (n_loop))  n_loop = entry%n_loop
             if (present (n_rad))  n_rad = entry%n_rad
          end do
          exit FIND_ENTRY
       end if
       entry => entry%next
    end do FIND_ENTRY
  end subroutine ps_table_get_pdg_out
  

end module auto_components
