! WHIZARD 2.6.1 Nov 03 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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
  use constants, only: zero, one, tiny_07
  use io_units
  use format_defs, only: FMT_16
  use diagnostics


  implicit none
  private

  public :: grid_t
  public :: verify_points_for_grid

  integer, parameter :: DEFAULT_POINTS_PER_DIMENSION = 100
  character(len=*), parameter :: DEFAULT_OUTPUT_PRECISION = FMT_16

  type :: grid_t
     private
     real(default), dimension(:), allocatable :: values
     integer, dimension(:), allocatable :: points
  contains
     generic :: init => init_base, init_simple
     procedure :: init_base => grid_init_base
     procedure :: init_simple => grid_init_simple
     procedure :: set_values => grid_set_values
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

  subroutine grid_set_values (grid, values)
    class(grid_t), intent(inout) :: grid
    real(default), dimension(:), intent(in) :: values
    grid%values = values
  end subroutine grid_set_values

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
          if (x (dim) <= i * segment_width + tiny_07) then
             grid_get_segment (dim) = i
             exit SEARCH
          end if
       end do SEARCH
       if (grid_get_segment (dim) == 0) then
          do i = 1, size(x)
             write (msg_buffer, "(A," // DEFAULT_OUTPUT_PRECISION // ")") &
                  "x[i] = ", x(i)
             call msg_message ()
          end do
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
    if (allocated (grid%values)) then
       n_values = size (grid%values)
       do i = 1, n_values
          val = grid%values (i)
          mean = mean + val / n_values
          if (val > maximum) then
             maximum = val
          end if
       end do
       write (msg_buffer, "(A," // DEFAULT_OUTPUT_PRECISION // ")") &
            "Grid: Mean value of the grid: ", mean
       call msg_message ()
       write (msg_buffer, "(A," // DEFAULT_OUTPUT_PRECISION // ")") &
            "Grid: Max value of the grid: ", maximum
       call msg_message ()
       if (maximum > zero) then
          write (msg_buffer, "(A," // DEFAULT_OUTPUT_PRECISION // ")") &
               "Grid: Mean/Max value of the grid: ", mean / maximum
          call msg_message ()
       end if
    else
       call msg_warning ("Grid: Grid is not allocated!")
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
    if (iostat /= 0) then
       call msg_warning &
            ('grid_save_to_file: Could not save grid to file')
    end if
    close (u)
  end subroutine grid_save_to_file

  function verify_points_for_grid (file, points) result (valid)
    logical :: valid
    character(len=*), intent(in) :: file
    integer, dimension(:), intent(in) :: points
    integer, dimension(:), allocatable :: points_from_file
    integer :: u
    call load_points_from_file (file, u, points_from_file)
    close (u)
    if (allocated (points_from_file)) then
      valid = all (points == points_from_file)
    else
      valid = .false.
    end if
  end function verify_points_for_grid

  subroutine load_points_from_file (file, unit, points)
    character(len=*), intent(in) :: file
    integer, intent(out) :: unit
    integer, dimension(:), allocatable :: points
    integer :: iostat, n_dimensions, i_dim
    unit = free_unit ()
    open (file=file, unit=unit, action='read', iostat=iostat)
    if (iostat /= 0)  return
    read (unit, "(I12)", iostat=iostat) n_dimensions
    if (iostat /= 0)  return
    allocate (points (n_dimensions))
    do i_dim = 1, size (points)
       read (unit, "(I12,1X)", advance='no', iostat=iostat) &
            points (i_dim)
    end do
    if (iostat /= 0)  return
    read (unit, *)
    if (iostat /= 0)  return
  end subroutine load_points_from_file

  subroutine grid_load_from_file (grid, file)
    class(grid_t), intent(out) :: grid
    character(len=*), intent(in) :: file
    integer :: iostat, u, i
    integer, dimension(:), allocatable :: points
    call load_points_from_file (file, u, points)
    if (.not. allocated (points))  return
    call grid%init (points)
    do i = 1, size (grid%values)
       read (u, "(" // DEFAULT_OUTPUT_PRECISION // ",1X)", advance='no', iostat=iostat) &
            grid%values (i)
    end do
    if (iostat /= 0) then
       call msg_warning ('grid_load_from_file: Could not load grid from file')
    end if
    close (u)
  end subroutine grid_load_from_file


end module grids
