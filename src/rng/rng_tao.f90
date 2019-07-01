! WHIZARD 2.5.0 May 06 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module rng_tao

  use kinds, only: default
  use io_units
  use format_utils, only: write_indent
  use tao_random_numbers !NODEP!

  use rng_base

  implicit none
  private

  public :: rng_tao_t
  public :: rng_tao_factory_t

  type, extends (rng_t) :: rng_tao_t
     integer :: seed = 0
     integer :: n_calls = 0
     type(tao_random_state) :: state
   contains
     procedure :: write => rng_tao_write
     procedure :: init => rng_tao_init
     procedure :: final => rng_tao_final
     procedure :: generate_single => rng_tao_generate_single
     procedure :: generate_array => rng_tao_generate_array
  end type rng_tao_t

  type, extends (rng_factory_t) :: rng_tao_factory_t
     integer(i16) :: s = 0
     integer(i16) :: i = 0
   contains
     procedure :: write => rng_tao_factory_write
     procedure :: init => rng_tao_factory_init
     procedure :: make => rng_tao_factory_make
  end type rng_tao_factory_t


contains

  subroutine rng_tao_write (rng, unit, indent)
    class(rng_tao_t), intent(in) :: rng
    integer, intent(in), optional :: unit, indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(A)")  "TAO random-number generator:"
    call write_indent (u, ind)
    write (u, "(2x,A,I0)")  "seed  = ", rng%seed
    call write_indent (u, ind)
    write (u, "(2x,A,I0)")  "calls = ", rng%n_calls
  end subroutine rng_tao_write

  subroutine rng_tao_init (rng, seed)
    class(rng_tao_t), intent(out) :: rng
    integer, intent(in), optional :: seed
    if (present (seed))  rng%seed = seed
    call tao_random_create (rng%state, rng%seed)
  end subroutine rng_tao_init

  subroutine rng_tao_final (rng)
    class(rng_tao_t), intent(inout) :: rng
    call tao_random_destroy (rng%state)
  end subroutine rng_tao_final

  subroutine rng_tao_generate_single (rng, x)
    class(rng_tao_t), intent(inout) :: rng
    real(default), intent(out) :: x
    real(default) :: r
    call tao_random_number (rng%state, r)
    x = r
    rng%n_calls = rng%n_calls + 1
  end subroutine rng_tao_generate_single

  subroutine rng_tao_generate_array (rng, x)
    class(rng_tao_t), intent(inout) :: rng
    real(default), dimension(:), intent(out) :: x
    real(default) :: r
    integer :: i
    do i = 1, size (x)
       call tao_random_number (rng%state, r)
       x(i) = r
    end do
    rng%n_calls = rng%n_calls + size (x)
  end subroutine rng_tao_generate_array

  subroutine rng_tao_factory_write (object, unit)
    class(rng_tao_factory_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A,2(I0,A))") &
         "RNG factory: tao (", object%s, ",", object%i, ")"
  end subroutine rng_tao_factory_write

  subroutine rng_tao_factory_init (factory, seed)
    class(rng_tao_factory_t), intent(out) :: factory
    integer(i16), intent(in), optional :: seed
    if (present (seed))  factory%s = seed
  end subroutine rng_tao_factory_init

  subroutine rng_tao_factory_make (factory, rng)
    class(rng_tao_factory_t), intent(inout) :: factory
    class(rng_t), intent(out), allocatable :: rng
    allocate (rng_tao_t :: rng)
    select type (rng)
    type is (rng_tao_t)
       call rng%init (factory%s * 65536 + factory%i)
       factory%i = int (factory%i + 1, kind = i16)
    end select
  end subroutine rng_tao_factory_make


end module rng_tao
