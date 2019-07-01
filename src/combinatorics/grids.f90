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

module grids

  use kinds, only: default
  use unit_tests
  use constants
  use io_units
  use format_defs
  use file_utils
  use diagnostics

  implicit none
  private

  public :: grid_t
  public :: grids_test



  integer, parameter :: DEFAULT_POINTS_PER_DIMENSION = 100
  character(len=*), parameter :: DEFAULT_OUTPUT_PRECISION = FMT_16

  type :: grid_t
     real(default), dimension(:), allocatable, private :: values
     integer, dimension(:), allocatable, private :: points
  contains
     generic :: init => init_base, init_simple
     procedure :: init_base => grid_init_base
     procedure :: init_simple => grid_init_simple
     procedure :: final => grid_final
     generic :: get_value => get_value_from_x, get_value_from_indices
     procedure :: get_value_from_x => grid_get_value_from_x
     procedure :: get_value_from_indices => grid_get_value_from_indices
     procedure :: get_segment => grid_get_segment
     procedure :: get_index => grid_get_index
     procedure :: update_maxima => grid_update_maxima
     procedure :: get_maximum_in_3d => grid_get_maximum_in_3d
     procedure :: is_non_zero_everywhere => grid_is_non_zero_everywhere
     procedure :: write => grid_write
     procedure :: compute_and_write_mean_and_max => &
          grid_compute_and_write_mean_and_max
     procedure :: save_to_file => grid_save_to_file
     procedure :: load_from_file => grid_load_from_file
  end type grid_t


contains

  pure subroutine grid_init_base (grid, points)
    class(grid_t), intent(inout) :: grid
    integer, dimension(:), intent(in) :: points
    allocate (grid%points (size (points)))
    allocate (grid%values (product (points)))
    grid%points = points
    grid%values = zero
  end subroutine grid_init_base

  pure subroutine grid_init_simple (grid, dimensions)
    class(grid_t), intent(inout) :: grid
    integer, intent(in) :: dimensions
    allocate (grid%points (dimensions))
    allocate (grid%values (DEFAULT_POINTS_PER_DIMENSION ** dimensions))
    grid%points = DEFAULT_POINTS_PER_DIMENSION
    grid%values = zero
  end subroutine grid_init_simple

  pure subroutine grid_final (grid)
    class(grid_t), intent(inout) :: grid
    if (allocated (grid%values)) then
       deallocate (grid%values)
    end if
    if (allocated (grid%points)) then
       deallocate (grid%points)
    end if
  end subroutine grid_final

  function grid_get_value_from_indices (grid, indices)
    real(default) :: grid_get_value_from_indices
    class(grid_t), intent(in) :: grid
    integer, dimension(:), intent(in) :: indices
    grid_get_value_from_indices = grid%values(grid%get_index(indices))
  end function grid_get_value_from_indices

  function grid_get_value_from_x (grid, x)
    real(default) :: grid_get_value_from_x
    class(grid_t), intent(in) :: grid
    real(default), dimension(:), intent(in) :: x
    grid_get_value_from_x = grid_get_value_from_indices &
         (grid, grid_get_segment (grid, x))
  end function grid_get_value_from_x

  function grid_get_segment (grid, x, unit)
    class(grid_t), intent(in) :: grid
    real(default), dimension(:), intent(in) :: x
    integer, intent(in), optional :: unit
    integer, dimension(1:size (x)) :: grid_get_segment
    integer :: dim, i
    real(default) :: segment_width
    grid_get_segment = 0
    do dim = 1, size (grid%points)
       segment_width = one / grid%points (dim)
       SEARCH: do i = 1, grid%points (dim)
          if (x (dim) <= i * segment_width) then
             grid_get_segment (dim) = i
             exit SEARCH
          end if
       end do SEARCH
       if (grid_get_segment (dim) == 0) then
          call msg_error ("grid_get_segment: Did not find x in [0,1]^d", &
               unit=unit)
       end if
    end do
  end function grid_get_segment

  pure function grid_get_index (grid, indices) result (grid_index)
    integer :: grid_index
    class(grid_t), intent(in) :: grid
    integer, dimension(:), intent(in) :: indices
    integer :: dim_innerloop, dim_outerloop, multiplier
    grid_index = 1
    do dim_outerloop = 1, size(indices)
       multiplier = 1
       do dim_innerloop = 1, dim_outerloop - 1
          multiplier = multiplier * grid%points (dim_innerloop)
       end do
       grid_index = grid_index + (indices(dim_outerloop) - 1) * multiplier
    end do
  end function grid_get_index

  subroutine grid_update_maxima (grid, x, y)
    class(grid_t), intent(inout) :: grid
    real(default), dimension(:), intent(in) :: x
    real(default), intent(in) :: y
    integer, dimension(1:size(x)) :: indices
    indices = grid%get_segment (x)
    if (grid%get_value (indices) < y) then
       grid%values (grid%get_index (indices)) = y
    end if
  end subroutine grid_update_maxima

  function grid_get_maximum_in_3d (grid, projected_index) result (maximum)
    real(default) :: maximum
    class(grid_t), intent(in) :: grid
    integer, intent(in) :: projected_index
    real(default) :: val
    integer :: i, j
    maximum = zero
    do i = 1, grid%points(1)
       do j = 1, grid%points(2)
          val = grid%get_value ([i, j, projected_index])
          if (val > maximum) then
             maximum = val
          end if
       end do
    end do

  end function grid_get_maximum_in_3d

  pure function grid_is_non_zero_everywhere (grid) result (yorn)
    logical :: yorn
    class(grid_t), intent(in) :: grid
    yorn = all (abs (grid%values) > zero)
  end function grid_is_non_zero_everywhere

  subroutine grid_write (grid, unit)
    class(grid_t), intent(in) :: grid
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1X,A)") "Grid"
    write (u, "(2X,A,2X)", advance='no') "Number of points per dimension:"
    if (allocated (grid%points)) then
       do i = 1, size (grid%points)
          write (u, "(I12,1X)", advance='no') &
               grid%points (i)
       end do
    end if
    write (u, *)
    write (u, "(2X,A)") "Values of the grid:"
    if (allocated (grid%values)) then
       do i = 1, size (grid%values)
          write (u, "(" // DEFAULT_OUTPUT_PRECISION // ",1X)") &
               grid%values (i)
       end do
    end if
    call grid%compute_and_write_mean_and_max (u)
  end subroutine grid_write

  subroutine grid_compute_and_write_mean_and_max (grid, unit)
    class(grid_t), intent(in) :: grid
    integer, intent(in), optional :: unit
    integer :: u, i, n_values
    real(default) :: mean, val, maximum
    u = given_output_unit (unit);  if (u < 0)  return
    mean = zero
    maximum = zero
    n_values = size (grid%values)
    do i = 1, n_values
       val = grid%values (i)
       mean = mean + val / n_values
       if (val > maximum) then
          maximum = val
       end if
    end do
    write (u, "(2X,A," // DEFAULT_OUTPUT_PRECISION // ")") &
         "Mean value of the grid: ", mean
    write (u, "(2X,A," // DEFAULT_OUTPUT_PRECISION // ")") &
         "Max value of the grid: ", maximum
    if (maximum > zero) then
       write (u, "(2X,A," // DEFAULT_OUTPUT_PRECISION // ")") &
            "Mean/Max value of the grid: ", mean / maximum
    end if
  end subroutine grid_compute_and_write_mean_and_max

  subroutine grid_save_to_file (grid, file)
    class(grid_t), intent(in) :: grid
    character(len=*), intent(in) :: file
    integer :: iostat, u, i
    u = free_unit ()
    open (file=file, unit=u, action='write')
    if (allocated (grid%points)) then
       write (u, "(I12)") size (grid%points)
       do i = 1, size (grid%points)
          write (u, "(I12,1X)", advance='no', iostat=iostat) &
               grid%points (i)
       end do
    end if
    write (u, *)
    if (allocated (grid%values)) then
       do i = 1, size (grid%values)
          write (u, "(" // DEFAULT_OUTPUT_PRECISION // ",1X)", &
               advance='no', iostat=iostat) grid%values (i)
       end do
    end if
    if (iostat < 0) then
       call msg_warning &
            ('grid_save_to_file: Could not save grid to file')
    end if
    close (u)
  end subroutine grid_save_to_file

  subroutine grid_load_from_file (grid, file)
    class(grid_t), intent(out) :: grid
    character(len=*), intent(in) :: file
    integer :: iostat, u, i, n_dimensions
    integer, dimension(:), allocatable :: points
    u = free_unit ()
    open (file=file, unit=u, action='read', iostat=iostat)
    read (u, "(I12)", iostat=iostat) n_dimensions
    allocate (points (n_dimensions))
    do i = 1, size (points)
       read (u, "(I12,1X)", advance='no', iostat=iostat) &
            points (i)
    end do
    read (u, *)
    call grid%init (points)
    do i = 1, size (grid%values)
       read (u, "(" // DEFAULT_OUTPUT_PRECISION // ",1X)", advance='no', iostat=iostat) &
            grid%values (i)
    end do
    if (iostat < 0) then
       call msg_warning ('grid_load_from_file: Could not load grid from file')
    end if
    close (u)
  end subroutine grid_load_from_file

  subroutine grids_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test(grids_1, "grids_1", &
         "Test Index Function", u, results)
    call test(grids_2, "grids_2", &
              "Saving and Loading", u, results)
    call test(grids_3, "grids_3", &
              "Get Segments", u, results)
    call test(grids_4, "grids_4", &
              "Update Maxima", u, results)
    call test(grids_5, "grids_5", &
              "Finding and checking", u, results)
  end subroutine grids_test

  subroutine grids_1 (u)
    integer, intent(in) :: u
    type(grid_t) :: grid
    write (u, "(A)")  "* Test output: grids_1"
    write (u, "(A)")  "*   Purpose: Test Index Function"
    write (u, "(A)")

    call grid%init ([3])
    call grid%write(u)
    call assert (u, grid%get_index([1]) == 1, "grid%get_index(1) == 1")
    call assert (u, grid%get_index([2]) == 2, "grid%get_index(2) == 2")
    call assert (u, grid%get_index([3]) == 3, "grid%get_index(3) == 3")
    call grid%final ()

    call grid%init ([3,3])
    call grid%write(u)
    call assert (u, grid%get_index([1,1]) == 1, "grid%get_index(1,1) == 1")
    call assert (u, grid%get_index([2,1]) == 2, "grid%get_index(2,1) == 2")
    call assert (u, grid%get_index([3,1]) == 3, "grid%get_index(3,1) == 3")
    call assert (u, grid%get_index([1,2]) == 4, "grid%get_index(1,2) == 4")
    call assert (u, grid%get_index([2,2]) == 5, "grid%get_index(2,2) == 5")
    call assert (u, grid%get_index([3,2]) == 6, "grid%get_index(3,2) == 6")
    call assert (u, grid%get_index([1,3]) == 7, "grid%get_index(1,3) == 7")
    call assert (u, grid%get_index([2,3]) == 8, "grid%get_index(2,3) == 8")
    call assert (u, grid%get_index([3,3]) == 9, "grid%get_index(3,3) == 9")
    call grid%final ()

    call grid%init ([3,3,2])
    call grid%write(u)
    call assert (u, grid%get_index([1,1,1]) == 1,   "grid%get_index(1,1,1) == 1")
    call assert (u, grid%get_index([2,1,2]) == 2+9, "grid%get_index(2,1,2) == 2+9")
    call assert (u, grid%get_index([3,3,1]) == 9,   "grid%get_index(3,3,1) == 3")
    call assert (u, grid%get_index([3,1,2]) == 3+9, "grid%get_index(3,1,2) == 4+9")
    call assert (u, grid%get_index([2,2,1]) == 5,   "grid%get_index(2,2,1) == 5")
    call assert (u, grid%get_index([3,2,2]) == 6+9, "grid%get_index(3,2,2) == 6+9")
    call assert (u, grid%get_index([1,3,1]) == 7,   "grid%get_index(1,3,1) == 7")
    call assert (u, grid%get_index([2,3,2]) == 8+9, "grid%get_index(2,3,2) == 8+9")
    call assert (u, grid%get_index([3,3,2]) == 9+9, "grid%get_index(3,3,2) == 9+9")
    call grid%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: grids_1"
  end subroutine grids_1

  subroutine grids_2 (u)
    integer, intent(in) :: u
    type(grid_t) :: grid
    write (u, "(A)")  "* Test output: grids_2"
    write (u, "(A)")  "*   Purpose: Saving and Loading"
    write (u, "(A)")

    call grid%init ([3])
    grid%values = [one, two, three]
    call grid%save_to_file ('grids_2_test')
    call grid%final ()

    call grid%load_from_file ('grids_2_test')
    call grid%write (u)
    call assert (u, nearly_equal (grid%get_value([1]), one),   "grid%get_value(1) == 1")
    call assert (u, nearly_equal (grid%get_value([2]), two),   "grid%get_value(2) == 2")
    call assert (u, nearly_equal (grid%get_value([3]), three), "grid%get_value(3) == 3")
    call grid%final ()

    call grid%init ([3,3])
    grid%values = [one, two, three, four, zero, zero, zero, zero, zero]
    call grid%save_to_file ('grids_2_test')
    call grid%final ()

    call grid%load_from_file ('grids_2_test')
    call grid%write (u)
    call assert (u, nearly_equal (grid%get_value([1,1]), one),   "grid%get_value(1,1) == 1")
    call assert (u, nearly_equal (grid%get_value([2,1]), two),   "grid%get_value(2,1) == 2")
    call assert (u, nearly_equal (grid%get_value([3,1]), three), "grid%get_value(3,1) == 3")
    call assert (u, nearly_equal (grid%get_value([1,2]), four),  "grid%get_value(1,2) == 4")
    call delete_file ('grids_2_test')

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: grids_2"
  end subroutine grids_2

  subroutine grids_3 (u)
    integer, intent(in) :: u
    type(grid_t) :: grid
    integer, dimension(2) :: fail
    write (u, "(A)")  "* Test output: grids_3"
    write (u, "(A)")  "*   Purpose: Get Segments"
    write (u, "(A)")

    call grid%init ([3])
    call assert (u, all(grid%get_segment([0.00_default]) == [1]), &
                   "all(grid%get_segment([0.00_default]) == [1])")
    call assert (u, all(grid%get_segment([0.32_default]) == [1]), &
                   "all(grid%get_segment([0.32_default]) == [1])")
    call assert (u, all(grid%get_segment([0.52_default]) == [2]), &
                   "all(grid%get_segment([0.52_default]) == [2])")
    call assert (u, all(grid%get_segment([1.00_default]) == [3]), &
                   "all(grid%get_segment([1.00_default]) == [3])")
    call grid%final ()

    call grid%init ([3,3])
    call assert (u, all(grid%get_segment([0.00_default,0.00_default]) == [1,1]), &
                   "all(grid%get_segment([0.00_default,0.00_default]) == [1,1])")
    call assert (u, all(grid%get_segment([0.32_default,0.32_default]) == [1,1]), &
                   "all(grid%get_segment([0.32_default,0.32_default]) == [1,1])")
    call assert (u, all(grid%get_segment([0.52_default,0.52_default]) == [2,2]), &
                   "all(grid%get_segment([0.52_default,0.52_default]) == [2,2])")
    call assert (u, all(grid%get_segment([1.00_default,1.00_default]) == [3,3]), &
                   "all(grid%get_segment([1.00_default,1.00_default]) == [3,3])")
    write (u, "(A)")  "* A double error is expected"
    fail = grid%get_segment([1.10_default,1.10_default], u)
    call grid%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: grids_3"
  end subroutine grids_3

  subroutine grids_4 (u)
    integer, intent(in) :: u
    type(grid_t) :: grid
    write (u, "(A)")  "* Test output: grids_4"
    write (u, "(A)")  "*   Purpose: Update Maxima"
    write (u, "(A)")

    call grid%init ([4,4])
    call grid%update_maxima ([0.1_default, 0.0_default], 0.3_default)
    call grid%update_maxima ([0.9_default, 0.95_default], 1.7_default)
    call grid%write (u)
    call assert_equal (u, grid%get_value([1,1]), 0.3_default, &
               "grid%get_value([1,1]")
    call assert_equal (u, grid%get_value([2,2]), 0.0_default, &
               "grid%get_value([2,2]")
    call assert_equal (u, grid%get_value([4,4]), 1.7_default, &
               "grid%get_value([4,4]")

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: grids_4"
  end subroutine grids_4

  subroutine grids_5 (u)
    integer, intent(in) :: u
    type(grid_t) :: grid
    real(default) :: first, second
    write (u, "(A)")  "* Test output: grids_5"
    write (u, "(A)")  "*   Purpose: Finding and checking"
    write (u, "(A)")

    call grid%init ([2,2,2])
    first = one / two - tiny_07
    second = two / two - tiny_07
    call grid%update_maxima ([0.1_default, 0.0_default, first], 0.3_default)
    call grid%update_maxima ([0.9_default, 0.95_default, second], 1.7_default)
    call grid%write (u)
    call assert (u, .not. grid%is_non_zero_everywhere (), &
               ".not. grid%is_non_zero_everywhere (")
    call assert_equal (u, grid%get_maximum_in_3d (1), 0.3_default, &
         "grid%get_maximum_in_3d (1)")
    call assert_equal (u, grid%get_maximum_in_3d (2), 1.7_default, &
         "grid%get_maximum_in_3d (2)")

    call grid%update_maxima ([0.9_default, 0.95_default, first], 1.8_default)
    call grid%update_maxima ([0.1_default, 0.95_default, first], 1.5_default)
    call grid%update_maxima ([0.9_default, 0.15_default, first], 1.5_default)
    call grid%update_maxima ([0.1_default, 0.0_default, second], 0.2_default)
    call grid%update_maxima ([0.1_default, 0.9_default, second], 0.2_default)
    call grid%update_maxima ([0.9_default, 0.0_default, second], 0.2_default)
    call grid%write (u)
    call assert (u, grid%is_non_zero_everywhere (), &
               "grid%is_non_zero_everywhere (")
    call assert_equal (u, grid%get_maximum_in_3d (1), 1.8_default, &
         "grid%get_maximum_in_3d (1)")
    call assert_equal (u, grid%get_maximum_in_3d (2), 1.7_default, &
         "grid%get_maximum_in_3d (2)")

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: grids_5"
  end subroutine grids_5


end module grids
