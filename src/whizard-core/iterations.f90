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

module iterations

  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: ITERATIONS_DEFAULT_LIST_SIZE !NODEP!
  use diagnostics !NODEP!
  use processes

  implicit none
  private

  public :: iterations_list_t
  public :: iterations_list_init
  public :: iterations_list_complete
  public :: iterations_list_clear
  public :: iterations_list_write
  public :: iterations_list_get_pass_array
  public :: iterations_list_get_n_calls_array
  public :: iterations_list_get_n_pass
  public :: iterations_list_get_n_calls
  public :: iterations_list_has_custom_adaptation
  public :: iterations_list_adapt_grids
  public :: iterations_list_adapt_weights
  public :: iterations_list_get_n_it
  public :: iterations_list_adjust_n_calls
  public :: iterations_lists_init_default

  type :: iterations_spec_t
     private
     integer :: n_it = 0
     integer :: n_calls = 0
     logical :: custom_adaptation = .false.
     logical :: adapt_grids = .false.
     logical :: adapt_weights = .false.
  end type iterations_spec_t

  type :: iterations_list_t
     private
     integer :: n_pass = 0
     type(iterations_spec_t), dimension(:), allocatable :: pass
  end type iterations_list_t
     

  interface iterations_list_get_n_it
     module procedure iterations_list_get_n_it_tot
     module procedure iterations_list_get_n_it_pass
  end interface


contains

  subroutine iterations_list_init (it_list, n_it, n_calls, adapt, adapt_code)
    type(iterations_list_t), intent(inout) :: it_list
    integer, dimension(:), intent(in) :: n_it, n_calls
    logical, dimension(:), intent(in), optional :: adapt
    type(string_t), dimension(:), intent(in), optional :: adapt_code
    it_list%n_pass = size (n_it)
    if (allocated (it_list%pass)) deallocate (it_list%pass)    
    allocate (it_list%pass (it_list%n_pass))
    it_list%pass%n_it = n_it
    it_list%pass%n_calls = n_calls
    if (present (adapt)) then
       it_list%pass%custom_adaptation = adapt
       if (any (verify (adapt_code, "wg") /= 0)) then
          call msg_error ("iteration specification: " &
               // "adaptation code letters must be 'w' or 'g'")
       end if
       it_list%pass%adapt_grids = scan (adapt_code, "g") /= 0
       it_list%pass%adapt_weights = scan (adapt_code, "w") /= 0
    end if
  end subroutine iterations_list_init

  subroutine iterations_list_complete (it_list, it_list_default)
    type(iterations_list_t), intent(inout) :: it_list
    type(iterations_list_t), intent(in) :: it_list_default
    if (it_list%n_pass >= 1) then
       if (it_list%pass(1)%n_it == 0)  &
            it_list%pass(1)%n_it = it_list_default%pass(1)%n_it
       if (it_list%pass(1)%n_calls == 0)  &
            it_list%pass(1)%n_calls = it_list_default%pass(1)%n_calls
    end if
    if (it_list%n_pass >= 2) then
       where (it_list%pass%n_it == 0) &
            it_list%pass%n_it = it_list_default%pass(2)%n_it
       where (it_list%pass%n_calls == 0) &
            it_list%pass%n_calls = it_list_default%pass(2)%n_calls
    end if
  end subroutine iterations_list_complete
    
  subroutine iterations_list_clear (it_list)
    type(iterations_list_t), intent(inout) :: it_list
    it_list%n_pass = 0
    deallocate (it_list%pass)
  end subroutine iterations_list_clear

  subroutine iterations_list_write (it_list, unit)
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in), optional :: unit
    type(string_t) :: buffer
    character(30) :: ibuf
    integer :: i
    buffer = "iterations = "
    if (it_list%n_pass > 0) then
       do i = 1, it_list%n_pass
          if (i > 1)  buffer = buffer // ", "
          write (ibuf, "(I0,':',I0)") &
               it_list%pass(i)%n_it, it_list%pass(i)%n_calls
          buffer = buffer // trim (ibuf)
          if (it_list%pass(i)%custom_adaptation) then
             buffer = buffer // '"'
             if (it_list%pass(i)%adapt_grids)  buffer = buffer // "g"
             if (it_list%pass(i)%adapt_weights)  buffer = buffer // "w"
             buffer = buffer // '"'
          end if          
       end do
    else
       buffer = buffer // "[undefined]"
    end if
    call msg_message (char (buffer), unit)
  end subroutine iterations_list_write

  function iterations_list_get_pass_array (it_list) result (pass)
    integer, dimension(:), allocatable :: pass
    type(iterations_list_t), intent(in) :: it_list
    integer :: it, i
    allocate (pass (sum (it_list%pass%n_it)))
    it = 0
    do i = 1, it_list%n_pass
       pass(it+1 : it+it_list%pass(i)%n_it) = i
       it = it + it_list%pass(i)%n_it
    end do
  end function iterations_list_get_pass_array

  function iterations_list_get_n_calls_array (it_list) result (n_calls)
    integer, dimension(:), allocatable :: n_calls
    type(iterations_list_t), intent(in) :: it_list
    integer :: it, i
    allocate (n_calls (sum (it_list%pass%n_it)))
    it = 0
    do i = 1, it_list%n_pass
       n_calls(it+1 : it+it_list%pass(i)%n_it) = it_list%pass(i)%n_calls
       it = it + it_list%pass(i)%n_it
    end do
  end function iterations_list_get_n_calls_array

  function iterations_list_get_n_pass (it_list) result (n_pass)
    integer :: n_pass
    type(iterations_list_t), intent(in) :: it_list
    n_pass = it_list%n_pass
  end function iterations_list_get_n_pass

  function iterations_list_get_n_calls (it_list, pass) result (n_calls)
    integer :: n_calls
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       n_calls = it_list%pass(pass)%n_calls
    else
       n_calls = 0
    end if
  end function iterations_list_get_n_calls

  function iterations_list_has_custom_adaptation (it_list, pass) result (flag)
    logical :: flag
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       flag = it_list%pass(pass)%custom_adaptation
    else
       flag = .false.
    end if
  end function iterations_list_has_custom_adaptation

  function iterations_list_adapt_grids (it_list, pass) result (flag)
    logical :: flag
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       flag = it_list%pass(pass)%adapt_grids
    else
       flag = .false.
    end if
  end function iterations_list_adapt_grids

  function iterations_list_adapt_weights (it_list, pass) result (flag)
    logical :: flag
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       flag = it_list%pass(pass)%adapt_weights
    else
       flag = .false.
    end if
  end function iterations_list_adapt_weights

  function iterations_list_get_n_it_tot (it_list) result (n_it)
    integer :: n_it
    type(iterations_list_t), intent(in) :: it_list
    n_it = sum (it_list%pass%n_it)
  end function iterations_list_get_n_it_tot

  function iterations_list_get_n_it_pass (it_list, pass) result (n_it)
    integer :: n_it
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       n_it = it_list%pass(pass)%n_it
    else
       n_it = 0
    end if
  end function iterations_list_get_n_it_pass

   subroutine iterations_list_adjust_n_calls (it_list, process, grid_parameters)
     type(iterations_list_t), intent(inout), target :: it_list
     type(process_t), intent(in) :: process
     type(grid_parameters_t), intent(in) :: grid_parameters
     type(iterations_spec_t), pointer :: it_spec
     integer :: n_calls, pass
     logical :: changed
     changed = .false.
     do pass = 1, it_list%n_pass
        it_spec => it_list%pass(pass)
        n_calls = max (it_spec%n_calls, &
             process_get_n_channels (process) &
             * grid_parameters%min_calls_per_channel)
        if (n_calls /= it_spec%n_calls) then
           it_spec%n_calls = n_calls
           changed = .true.
        end if
     end do
     if (changed) then
        write (msg_buffer, "(A,I0)") "Process '" &
             // char (process_get_id (process)) // "': " &
             // "resetting n_calls to ", n_calls
        call msg_warning ()
     end if
  end subroutine iterations_list_adjust_n_calls

  subroutine iterations_lists_init_default (it_list)
    type(iterations_list_t), dimension(:), pointer :: it_list
    allocate (it_list (ITERATIONS_DEFAULT_LIST_SIZE))
    call iterations_list_init (it_list(1), (/  1 /), (/ 100 /))
    call iterations_list_init (it_list(2), (/  3, 3 /), (/   1000,  10000 /))
    call iterations_list_init (it_list(3), (/  5, 3 /), (/   5000,  10000 /))
    call iterations_list_init (it_list(4), (/ 10, 5 /), (/  10000,  20000 /))
    call iterations_list_init (it_list(5), (/ 10, 5 /), (/  20000,  50000 /))
    call iterations_list_init (it_list(6), (/ 15, 5 /), (/  50000, 100000 /))
    call iterations_list_init (it_list(7), (/ 20, 5 /), (/  50000, 200000 /))
  end subroutine iterations_lists_init_default


end module iterations
