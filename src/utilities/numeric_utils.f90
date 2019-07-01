! WHIZARD 2.4.1 Mar 24 2017
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

module numeric_utils

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use string_utils
  use constants
  use format_defs

  implicit none
  private

  public :: assert
  public:: assert_equal
  interface assert_equal
     module procedure assert_equal_integer, assert_equal_integers, &
            assert_equal_real, assert_equal_reals, &
            assert_equal_complex, assert_equal_complexs
  end interface

  public :: nearly_equal
  public:: vanishes
  interface vanishes
     module procedure vanishes_real, vanishes_complex
  end interface
  public :: expanded_amp2
  public :: abs2
  public :: remove_duplicates_from_list
  public :: extend_integer_array
  public :: crop_integer_array





  interface nearly_equal
     module procedure nearly_equal_real
     module procedure nearly_equal_complex
  end interface nearly_equal


contains

  subroutine assert (unit, ok, description, exit_on_fail)
    integer, intent(in) :: unit
    logical, intent(in) :: ok
    character(*), intent(in), optional :: description
    logical, intent(in), optional :: exit_on_fail
    logical :: ef
    ef = .false.;  if (present (exit_on_fail)) ef = exit_on_fail
    if (.not. ok) then
       if (present(description)) then
          write (unit, "(A)") "* FAIL: " // description
       else
          write (unit, "(A)") "* FAIL: Assertion error"
       end if
       if (ef)  stop 1
    end if
  end subroutine assert

  subroutine assert_equal_integer (unit, lhs, rhs, description, exit_on_fail)
    integer, intent(in) :: unit
    integer, intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = lhs == rhs
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_integer

  subroutine assert_equal_integers (unit, lhs, rhs, description, exit_on_fail)
    integer, intent(in) :: unit
    integer, dimension(:), intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = all(lhs == rhs)
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_integers

  subroutine assert_equal_real (unit, lhs, rhs, description, &
                                abs_smallness, rel_smallness, exit_on_fail)
    integer, intent(in) :: unit
    real(default), intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = nearly_equal (lhs, rhs, abs_smallness, rel_smallness)
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_real

  subroutine assert_equal_reals (unit, lhs, rhs, description, &
                                abs_smallness, rel_smallness, exit_on_fail)
    integer, intent(in) :: unit
    real(default), dimension(:), intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = all(nearly_equal (lhs, rhs, abs_smallness, rel_smallness))
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_reals

  subroutine assert_equal_complex (unit, lhs, rhs, description, &
                                abs_smallness, rel_smallness, exit_on_fail)
    integer, intent(in) :: unit
    complex(default), intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = nearly_equal (real(lhs), real(rhs), abs_smallness, rel_smallness) &
         .and. nearly_equal (aimag(lhs), aimag(rhs), abs_smallness, rel_smallness)
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_complex

  subroutine assert_equal_complexs (unit, lhs, rhs, description, &
                                abs_smallness, rel_smallness, exit_on_fail)
    integer, intent(in) :: unit
    complex(default), dimension(:), intent(in) :: lhs, rhs
    character(*), intent(in), optional :: description
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    logical, intent(in), optional :: exit_on_fail
    type(string_t) :: desc
    logical :: ok
    ok = all (nearly_equal (real(lhs), real(rhs), abs_smallness, rel_smallness)) &
         .and. all (nearly_equal (aimag(lhs), aimag(rhs), abs_smallness, rel_smallness))
    desc = '';  if (present (description)) desc = var_str(description) // ": "
    call assert (unit, ok, char(desc // str (lhs) // " /= " // str (rhs)), exit_on_fail)
  end subroutine assert_equal_complexs

  elemental function ieee_is_nan (x) result (yorn)
    logical :: yorn
    real(default), intent(in) :: x
    yorn = (x /= x)
  end function ieee_is_nan

  elemental function nearly_equal_real (a, b, abs_smallness, rel_smallness) result (r)
    logical :: r
    real(default), intent(in) :: a, b
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    real(default) :: abs_a, abs_b, diff, abs_small, rel_small
    abs_a = abs (a)
    abs_b = abs (b)
    diff = abs (a - b)
    ! shortcut, handles infinities and nans
    if (a == b) then
       r = .true.
       return
    else if (ieee_is_nan (a) .or. ieee_is_nan (b) .or. ieee_is_nan (diff)) then
       r = .false.
       return
    end if
    abs_small = tiny_13; if (present (abs_smallness)) abs_small = abs_smallness
    rel_small = tiny_10; if (present (rel_smallness)) rel_small = rel_smallness
    if (abs_a < abs_small .and. abs_b < abs_small) then
       r = diff < abs_small
    else
       r = diff / max (abs_a, abs_b) < rel_small
    end if
  end function nearly_equal_real

  elemental function nearly_equal_complex (a, b, abs_smallness, rel_smallness) result (r)
    logical :: r
    complex(default), intent(in) :: a, b
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = nearly_equal_real (real (a), real (b), abs_smallness, rel_smallness) .and. &
        nearly_equal_real (aimag (a), aimag(b), abs_smallness, rel_smallness)
  end function nearly_equal_complex

  elemental function vanishes_real (x, abs_smallness, rel_smallness) result (r)
    logical :: r
    real(default), intent(in) :: x
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = nearly_equal (x, zero, abs_smallness, rel_smallness)
  end function vanishes_real

  elemental function vanishes_complex (x, abs_smallness, rel_smallness) result (r)
    logical :: r
    complex(default), intent(in) :: x
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = vanishes_real (abs (x), abs_smallness, rel_smallness)
  end function vanishes_complex

  pure function expanded_amp2 (amp_tree, amp_blob) result (amp2)
    real(default) :: amp2
    complex(default), dimension(:), intent(in) :: amp_tree, amp_blob
    amp2 = sum (amp_tree * conjg (amp_tree) + &
                amp_tree * conjg (amp_blob) + &
                amp_blob * conjg (amp_tree))
  end function expanded_amp2

  elemental function abs2 (c) result (c2)
    real(default) :: c2
    complex(default), intent(in) :: c
    c2 = real (c * conjg(c))
  end function abs2

  function remove_duplicates_from_list (list) result (list_clean)
    integer, dimension(:), allocatable :: list_clean
    integer, intent(in), dimension(:) :: list
    integer, parameter :: N_MAX = 20
    integer, dimension(N_MAX) :: buf
    integer :: i_buf, i_list, n_buf
    buf = -1; i_buf = 1
    if (size (list) > N_MAX) return
    do i_list = 1, size (list)
       if (.not. any (list(i_list) == buf)) then
          buf(i_buf) = list(i_list)
          i_buf = i_buf + 1
       end if
    end do
    n_buf = count (buf >= 0)
    allocate (list_clean (n_buf))
    list_clean = buf (1 : n_buf)
  end function remove_duplicates_from_list

  subroutine extend_integer_array (list, incr, initial_value)
    integer, intent(inout), dimension(:), allocatable :: list
    integer, intent(in) :: incr
    integer, intent(in), optional :: initial_value
    integer, dimension(:), allocatable :: list_store
    integer :: n, ini
    ini = 0; if (present (initial_value)) ini = initial_value
    n = size (list)
    allocate (list_store (n))
    list_store = list
    deallocate (list)
    allocate (list (n+incr))
    list(1:n) = list_store
    list(1+n : n+incr) = ini
    deallocate (list_store)
  end subroutine extend_integer_array

  subroutine crop_integer_array (list, i_crop)
    integer, intent(inout), dimension(:), allocatable :: list
    integer, intent(in) :: i_crop
    integer, dimension(:), allocatable :: list_store
    allocate (list_store (i_crop))
    list_store = list(1:i_crop)
    deallocate (list)
    allocate (list (i_crop))
    list = list_store
    deallocate (list_store)
  end subroutine crop_integer_array


end module numeric_utils
