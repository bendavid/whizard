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

module iterations

  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use diagnostics

  implicit none
  private

  public :: iterations_list_t
  public :: iterations_test

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
   contains
     procedure :: init => iterations_list_init
     procedure :: clear => iterations_list_clear
     procedure :: write => iterations_list_write
     procedure :: to_string => iterations_list_to_string
     procedure :: get_n_pass => iterations_list_get_n_pass
     procedure :: get_n_calls => iterations_list_get_n_calls
     procedure :: adapt_grids => iterations_list_adapt_grids
     procedure :: adapt_weights => iterations_list_adapt_weights
     procedure :: get_n_it => iterations_list_get_n_it
  end type iterations_list_t
     

contains

  subroutine iterations_list_init &
       (it_list, n_it, n_calls, adapt, adapt_code, adapt_grids, adapt_weights)
    class(iterations_list_t), intent(inout) :: it_list
    integer, dimension(:), intent(in) :: n_it, n_calls
    logical, dimension(:), intent(in), optional :: adapt
    type(string_t), dimension(:), intent(in), optional :: adapt_code
    logical, dimension(:), intent(in), optional :: adapt_grids, adapt_weights
    integer :: i
    it_list%n_pass = size (n_it)
    if (allocated (it_list%pass)) deallocate (it_list%pass)    
    allocate (it_list%pass (it_list%n_pass))
    it_list%pass%n_it = n_it
    it_list%pass%n_calls = n_calls
    if (present (adapt)) then
       it_list%pass%custom_adaptation = adapt
       do i = 1, it_list%n_pass
          if (adapt(i)) then
             if (verify (adapt_code(i), "wg") /= 0) then
                call msg_error ("iteration specification: " &
                     // "adaptation code letters must be 'w' or 'g'")
             end if
             it_list%pass(i)%adapt_grids = scan (adapt_code(i), "g") /= 0
             it_list%pass(i)%adapt_weights = scan (adapt_code(i), "w") /= 0
          end if
       end do
    else if (present (adapt_grids) .and. present (adapt_weights)) then
       it_list%pass%custom_adaptation = .true.
       it_list%pass%adapt_grids = adapt_grids
       it_list%pass%adapt_weights = adapt_weights
    end if
    do i = 1, it_list%n_pass - 1
       if (.not. it_list%pass(i)%custom_adaptation) then
          it_list%pass(i)%adapt_grids = .true.
          it_list%pass(i)%adapt_weights = .true.
       end if
    end do
  end subroutine iterations_list_init

  subroutine iterations_list_clear (it_list)
    class(iterations_list_t), intent(inout) :: it_list
    it_list%n_pass = 0
    deallocate (it_list%pass)
  end subroutine iterations_list_clear

  subroutine iterations_list_write (it_list, unit)
    class(iterations_list_t), intent(in) :: it_list
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A)")  char (it_list%to_string ())
  end subroutine iterations_list_write

  function iterations_list_to_string (it_list) result (buffer)
    class(iterations_list_t), intent(in) :: it_list
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
          if (it_list%pass(i)%custom_adaptation &
               .or. it_list%pass(i)%adapt_grids &
               .or. it_list%pass(i)%adapt_weights) then
             buffer = buffer // ':"'
             if (it_list%pass(i)%adapt_grids)  buffer = buffer // "g"
             if (it_list%pass(i)%adapt_weights)  buffer = buffer // "w"
             buffer = buffer // '"'
          end if          
       end do
    else
       buffer = buffer // "[undefined]"
    end if
  end function iterations_list_to_string
    
  function iterations_list_get_n_pass (it_list) result (n_pass)
    class(iterations_list_t), intent(in) :: it_list
    integer :: n_pass
    n_pass = it_list%n_pass
  end function iterations_list_get_n_pass

  function iterations_list_get_n_calls (it_list, pass) result (n_calls)
    class(iterations_list_t), intent(in) :: it_list
    integer :: n_calls
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       n_calls = it_list%pass(pass)%n_calls
    else
       n_calls = 0
    end if
  end function iterations_list_get_n_calls

  function iterations_list_adapt_grids (it_list, pass) result (flag)
    logical :: flag
    class(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       flag = it_list%pass(pass)%adapt_grids
    else
       flag = .false.
    end if
  end function iterations_list_adapt_grids

  function iterations_list_adapt_weights (it_list, pass) result (flag)
    logical :: flag
    class(iterations_list_t), intent(in) :: it_list
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       flag = it_list%pass(pass)%adapt_weights
    else
       flag = .false.
    end if
  end function iterations_list_adapt_weights

  function iterations_list_get_n_it (it_list, pass) result (n_it)
    class(iterations_list_t), intent(in) :: it_list
    integer :: n_it
    integer, intent(in) :: pass
    if (pass <= it_list%n_pass) then
       n_it = it_list%pass(pass)%n_it
    else
       n_it = 0
    end if
  end function iterations_list_get_n_it


  subroutine iterations_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (iterations_1, "iterations_1", &
         "empty iterations list", &
         u, results)
    call test (iterations_2, "iterations_2", &
         "create iterations list", &
         u, results)
end subroutine iterations_test

  subroutine iterations_1 (u)
    integer, intent(in) :: u
    type(iterations_list_t) :: it_list
    
    write (u, "(A)")  "* Test output: iterations_1"
    write (u, "(A)")  "*   Purpose: display empty iterations list"
    write (u, "(A)")

    call it_list%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: iterations_1"
    
  end subroutine iterations_1
  
  subroutine iterations_2 (u)
    integer, intent(in) :: u
    type(iterations_list_t) :: it_list
    
    write (u, "(A)")  "* Test output: iterations_2"
    write (u, "(A)")  "*   Purpose: fill and display iterations list"
    write (u, "(A)")

    write (u, "(A)")  "* Minimal setup (2 passes)"
    write (u, "(A)")

    call it_list%init ([2, 4], [5000, 20000])

    call it_list%write (u)
    call it_list%clear ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Setup with flags (3 passes)"
    write (u, "(A)")

    call it_list%init ([2, 4, 5], [5000, 20000, 400], &
         [.false., .true., .true.], &
         [var_str (""), var_str ("g"), var_str ("wg")])

    call it_list%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Extract data"
    write (u, "(A)")
    
    write (u, "(A,I0)")  "n_pass = ", it_list%get_n_pass ()
    write (u, "(A)")
    write (u, "(A,I0)")  "n_calls(2) = ", it_list%get_n_calls (2)
    write (u, "(A)")
    write (u, "(A,I0)")  "n_it(3) = ", it_list%get_n_it (3)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: iterations_2"
    
  end subroutine iterations_2
  

end module iterations
