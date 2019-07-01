! WHIZARD 2.2.5 Feb 27 2015
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

module selectors

  use kinds
  use io_units
  use unit_tests
  use diagnostics
  use rng_base

  implicit none
  private

  public :: selector_t
  public :: selectors_test

  type :: selector_t
     integer, dimension(:), allocatable :: map
     real(default), dimension(:), allocatable :: weight
     real(default), dimension(:), allocatable :: acc
   contains
     procedure :: write => selector_write
     procedure :: init => selector_init
     procedure :: select => selector_select
     procedure :: generate => selector_generate
     procedure :: get_weight => selector_get_weight
end type selector_t


contains
  
  subroutine selector_write (object, unit)
    class(selector_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Selector: i, weight, acc. weight"
    if (allocated (object%weight)) then
       do i = 1, size (object%weight)
          write (u, "(3x,I0,1x,ES19.12,1x,ES19.12)") &
               object%map(i), object%weight(i), object%acc(i)
       end do
    else
       write (u, "(3x,A)")  "[undefined]"
    end if
  end subroutine selector_write
 
  subroutine selector_init (selector, weight)
    class(selector_t), intent(out) :: selector
    real(default), dimension(:), intent(in) :: weight
    real(default) :: s
    integer :: n, i
    logical, dimension(:), allocatable :: mask
    if (size (weight) == 0) &
         call msg_bug ("Selector init: zero-size weight array")
    if (any (weight < 0)) &
         call msg_bug ("Selector init: negative weight")
    s = sum (weight)
    if (s == 0) &
         call msg_bug ("Selector init: all weights are zero")
    allocate (mask (size (weight)), &
         source = weight /= 0)
    n = count (mask)
    allocate (selector%map (n), &
         source = pack ([(i, i = 1, size (weight))], mask))
    allocate (selector%weight (n), &
         source = pack (weight / s, mask))
    allocate (selector%acc (n))
    selector%acc(1) = selector%weight(1)
    do i = 2, n - 1
       selector%acc(i) = selector%acc(i-1) + selector%weight(i)
    end do
    selector%acc(n) = 1
  end subroutine selector_init
    
  function selector_select (selector, x) result (n)
    class(selector_t), intent(in) :: selector
    real(default), intent(in) :: x
    integer :: n
    integer :: i
    if (x < 0 .or. x > 1) &
         call msg_bug ("Selector: random number out of range")
    do i = 1, size (selector%acc)
       if (x <= selector%acc(i))  exit
    end do
    n = selector%map(i)
  end function selector_select

  subroutine selector_generate (selector, rng, n)
    class(selector_t), intent(in) :: selector
    class(rng_t), intent(inout) :: rng
    integer, intent(out) :: n
    real(default) :: x
    select case (size (selector%acc))
    case (1);  n = 1
    case default
       call rng%generate (x)
       n = selector%select (x)
    end select
  end subroutine selector_generate

  function selector_get_weight (selector, n) result (weight)
    class(selector_t), intent(in) :: selector
    integer, intent(in) :: n
    real(default) :: weight
    integer :: i
    do i = 1, size (selector%weight)
       if (selector%map(i) == n) then
          weight = selector%weight(i)
          return
       end if
    end do
    weight = 0
  end function selector_get_weight


  subroutine selectors_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (selectors_1, "selectors_1", &
         "rng initialization and call", &
         u, results)
  end subroutine selectors_test
  
  subroutine selectors_1 (u)
    integer, intent(in) :: u
    type(selector_t) :: selector
    class(rng_t), allocatable, target :: rng
    integer :: i, n

    write (u, "(A)")  "* Test output: selectors_1"
    write (u, "(A)")  "*   Purpose: initialize a selector and test it"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize selector"
    write (u, "(A)")

    call selector%init &
         ([2._default, 3.5_default, 0._default, &
         2._default, 0.5_default, 2._default])
    call selector%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Select numbers using predictable test generator"
    write (u, "(A)")

    allocate (rng_test_t :: rng)
    call rng%init (1)

    do i = 1, 5
       call selector%generate (rng, n)
       write (u, "(1x,I0)")  n
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Select numbers using real input number"
    write (u, "(A)")
    
    write (u, "(1x,A,I0)")  "select(0.00) = ", selector%select (0._default)
    write (u, "(1x,A,I0)")  "select(0.77) = ", selector%select (0.77_default)
    write (u, "(1x,A,I0)")  "select(1.00) = ", selector%select (1._default)

    write (u, "(A)")
    write (u, "(A)")  "* Get weight"
    write (u, "(A)")
    
    write (u, "(1x,A,ES19.12)")  "weight(2) =", selector%get_weight(2)
    write (u, "(1x,A,ES19.12)")  "weight(3) =", selector%get_weight(3)
        
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
        
    call rng%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: selectors_1"
    
  end subroutine selectors_1
    

end module selectors
