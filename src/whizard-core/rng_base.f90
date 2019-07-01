! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module rng_base

  use kinds !NODEP!
  use file_utils !NODEP!
  use unit_tests

  implicit none
  private

  public :: rng_t
  public :: rng_factory_t
  public :: rng_base_test
  public :: rng_test_t
  public :: rng_test_factory_t

  type, abstract :: rng_t
   contains
     procedure (rng_init), deferred :: init
     procedure (rng_final), deferred :: final
     procedure (rng_write), deferred :: write
     generic :: generate => generate_single, generate_array
     procedure (rng_generate_single), deferred :: generate_single
     procedure (rng_generate_array), deferred :: generate_array
  end type rng_t

  type, abstract :: rng_factory_t
   contains
     procedure (rng_factory_write), deferred :: write
     procedure (rng_factory_init), deferred :: init
     procedure (rng_factory_make), deferred :: make
  end type rng_factory_t
  

  abstract interface
     subroutine rng_init (rng, seed)
       import
       class(rng_t), intent(out) :: rng
       integer, intent(in), optional :: seed
     end subroutine rng_init
  end interface
  
  abstract interface
     subroutine rng_final (rng)
       import
       class(rng_t), intent(inout) :: rng
     end subroutine rng_final
  end interface
  
  abstract interface
     subroutine rng_write (rng, unit, indent)
       import
       class(rng_t), intent(in) :: rng
       integer, intent(in), optional :: unit, indent
     end subroutine rng_write
  end interface
  
  abstract interface
     subroutine rng_generate_single (rng, x)
       import
       class(rng_t), intent(inout) :: rng
       real(default), intent(out) :: x
     end subroutine rng_generate_single
  end interface
  
  abstract interface
     subroutine rng_generate_array (rng, x)
       import
       class(rng_t), intent(inout) :: rng
       real(default), dimension(:), intent(out) :: x
     end subroutine rng_generate_array
  end interface
  
  abstract interface
     subroutine rng_factory_write (object, unit)
       import
       class(rng_factory_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine rng_factory_write
  end interface
  
  abstract interface
     subroutine rng_factory_init (factory, seed)
       import
       class(rng_factory_t), intent(out) :: factory
       integer(i16), intent(in), optional :: seed
     end subroutine rng_factory_init
  end interface
       
  abstract interface
     subroutine rng_factory_make (factory, rng)
       import
       class(rng_factory_t), intent(inout) :: factory
       class(rng_t), intent(out), allocatable :: rng
     end subroutine rng_factory_make
  end interface
  

  type, extends (rng_t) :: rng_test_t
     integer :: state = 1
   contains
     procedure :: write => rng_test_write
     procedure :: init => rng_test_init
     procedure :: final => rng_test_final
     procedure :: generate_single => rng_test_generate_single
     procedure :: generate_array => rng_test_generate_array
  end type rng_test_t
  
  type, extends (rng_factory_t) :: rng_test_factory_t
     integer :: seed = 1
   contains
     procedure :: write => rng_test_factory_write
     procedure :: init => rng_test_factory_init
     procedure :: make => rng_test_factory_make
  end type rng_test_factory_t
  

contains

  subroutine rng_base_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (rng_base_1, "rng_base_1", &
         "rng initialization and call", &
         u, results)
    call test (rng_base_2, "rng_base_2", &
         "rng factory", &
         u, results)
  end subroutine rng_base_test
  
  subroutine rng_test_write (rng, unit, indent)
    class(rng_test_t), intent(in) :: rng
    integer, intent(in), optional :: unit, indent
    integer :: u, ind
    u = output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(A,I0,A)")  "Random-number generator: &
         &test (state = ", rng%state, ")"
  end subroutine rng_test_write
  
  subroutine rng_test_init (rng, seed)
    class(rng_test_t), intent(out) :: rng
    integer, intent(in), optional :: seed
    if (present (seed))  rng%state = seed
  end subroutine rng_test_init
  
  subroutine rng_test_final (rng)
    class(rng_test_t), intent(inout) :: rng
  end subroutine rng_test_final
  
  subroutine rng_test_generate_single (rng, x)
    class(rng_test_t), intent(inout) :: rng
    real(default), intent(out) :: x
    x = rng%state / 10._default
    rng%state = mod (rng%state + 2, 10)
  end subroutine rng_test_generate_single
  
  subroutine rng_test_generate_array (rng, x)
    class(rng_test_t), intent(inout) :: rng
    real(default), dimension(:), intent(out) :: x
    integer :: i
    do i = 1, size (x)
       call rng%generate (x(i))
    end do
  end subroutine rng_test_generate_array
  
  subroutine rng_test_factory_write (object, unit)
    class(rng_test_factory_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A,I0,A)")  "RNG factory: test (", object%seed, ")"
  end subroutine rng_test_factory_write

  subroutine rng_test_factory_init (factory, seed)
    class(rng_test_factory_t), intent(out) :: factory
    integer(i16), intent(in), optional :: seed
    if (present (seed))  factory%seed = mod (seed * 2 + 1, 10)
  end subroutine rng_test_factory_init
    
  subroutine rng_test_factory_make (factory, rng)
    class(rng_test_factory_t), intent(inout) :: factory
    class(rng_t), intent(out), allocatable :: rng
    allocate (rng_test_t :: rng)
    select type (rng)
    type is (rng_test_t)
       call rng%init (int (factory%seed))
    end select
  end subroutine rng_test_factory_make

  subroutine rng_base_1 (u)
    integer, intent(in) :: u
    class(rng_t), allocatable :: rng

    real(default) :: x
    real(default), dimension(2) :: x2
    
    write (u, "(A)")  "* Test output: rng_base_1"
    write (u, "(A)")  "*   Purpose: initialize and call a test random-number &
         &generator"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize generator"
    write (u, "(A)")

    allocate (rng_test_t :: rng)
    call rng%init (3)

    call rng%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Get random number"
    write (u, "(A)")
    
    call rng%generate (x)
    write (u, "(A,2(1x,F9.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Get random number pair"
    write (u, "(A)")
    
    call rng%generate (x2)
    write (u, "(A,2(1x,F9.7))")  "x =", x2
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
        
    call rng%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rng_base_1"
    
  end subroutine rng_base_1
    
  subroutine rng_base_2 (u)
    integer, intent(in) :: u
    type(rng_test_factory_t) :: rng_factory
    class(rng_t), allocatable :: rng
    
    write (u, "(A)")  "* Test output: rng_base_2"
    write (u, "(A)")  "*   Purpose: initialize and use a rng factory"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize factory"
    write (u, "(A)")

    call rng_factory%init ()
    call rng_factory%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Make a generator"
    write (u, "(A)")

    call rng_factory%make (rng)
    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
        
    call rng%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rng_base_2"
    
  end subroutine rng_base_2
    

end module rng_base
