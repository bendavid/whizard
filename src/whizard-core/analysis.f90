! WHIZARD 2.0.0 Mon Apr 12 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module analysis

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: HISTOGRAM_HEAD_FORMAT, HISTOGRAM_DATA_FORMAT !NODEP!
  use limits, only: HISTOGRAM_INTG_FORMAT !NODEP!
  use file_utils !NODEP!
  use file_utils, only: tex_format !NODEP!
  use diagnostics !NODEP!
  use os_interface

  implicit none
  private

  public :: plot_labels_t
  public :: plot_labels_init
  public :: analysis_final
  public :: analysis_init_observable
  public :: analysis_init_histogram
  public :: analysis_init_plot
  public :: analysis_clear
  public :: analysis_record_data
  public :: analysis_get_n_entries
  public :: analysis_get_average
  public :: analysis_get_error
  public :: analysis_has_plots
  public :: analysis_write
  public :: analysis_write_driver
  public :: analysis_compile_tex
  public :: analysis_test

  integer, parameter :: AN_UNDEFINED = 0
  integer, parameter :: AN_OBSERVABLE = 1
  integer, parameter :: AN_HISTOGRAM = 2
  integer, parameter :: AN_PLOT = 3


  type :: plot_labels_t
     private
     type(string_t) :: title
     type(string_t) :: description
     type(string_t) :: xlabel
     type(string_t) :: ylabel
     integer :: width_mm = 130
     integer :: height_mm = 90
  end type plot_labels_t

  type :: observable_t
     private
     real(default) :: sum_values = 0
     real(default) :: sum_squared_values = 0
     real(default) :: sum_weights = 0
     real(default) :: sum_squared_weights = 0
     integer :: count = 0
     type(string_t) :: label
     type(string_t) :: physical_unit
     type(plot_labels_t) :: plot_labels
  end type observable_t

  type :: bin_t
     private
     real(default) :: midpoint = 0
     real(default) :: width = 0
     real(default) :: sum_weights = 0
     real(default) :: sum_squared_weights = 0
     real(default) :: sum_excess_weights = 0
     integer :: count = 0
  end type bin_t

  type :: histogram_t
     private
     real(default) :: lower_bound = 0
     real(default) :: upper_bound = 0
     real(default) :: width = 0
     integer :: n_bins = 0
     type(observable_t) :: obs
     type(observable_t) :: obs_within_bounds
     type(bin_t) :: underflow
     type(bin_t), dimension(:), allocatable :: bin
     type(bin_t) :: overflow
     type(plot_labels_t) :: plot_labels
  end type histogram_t

  type :: point_t
     private
     real(default) :: x = 0
     real(default) :: y = 0
     real(default) :: xerr = 0
     real(default) :: yerr = 0
     type(point_t), pointer :: next => null ()
  end type point_t

  type :: plot_t
     private
     real(default) :: lower_bound = 0
     real(default) :: upper_bound = 0
     real(default) :: width = 0
     type(bin_t) :: underflow
     type(point_t), pointer :: first => null ()
     type(point_t), pointer :: last => null ()
     type(bin_t) :: overflow
     integer :: count = 0
     integer :: count_within_bounds = 0
     type(plot_labels_t) :: plot_labels
  end type plot_t

  type :: analysis_object_t
     private
     type(string_t) :: id
     integer :: type = AN_UNDEFINED
     type(observable_t), pointer :: obs => null ()
     type(histogram_t), pointer :: h => null ()
     type(plot_t), pointer :: plot => null ()
     type(analysis_object_t), pointer :: next => null ()
  end type analysis_object_t

  type :: analysis_store_t
     private
     type(analysis_object_t), pointer :: first => null ()
     type(analysis_object_t), pointer :: last => null ()
  end type analysis_store_t


  interface observable_record_value
     module procedure observable_record_value_unweighted
     module procedure observable_record_value_weighted
  end interface

  interface histogram_init
     module procedure histogram_init_n_bins
     module procedure histogram_init_bin_width
  end interface

  interface analysis_init_histogram
     module procedure analysis_init_histogram_n_bins
     module procedure analysis_init_histogram_bin_width
  end interface

  interface analysis_clear
     module procedure analysis_store_clear_obj
     module procedure analysis_store_clear_all
  end interface

  interface analysis_has_plots
     module procedure analysis_has_plots_any
     module procedure analysis_has_plots_obj
  end interface

  interface analysis_write
     module procedure analysis_write_object
     module procedure analysis_write_all
  end interface


  type(analysis_store_t), save :: analysis_store


contains

  subroutine plot_labels_init (plot_labels, &
       title, description, xlabel, ylabel, width_mm, height_mm)
    type(plot_labels_t), intent(out) :: plot_labels
    type(string_t), intent(in) :: title
    type(string_t), intent(in), optional :: description
    type(string_t), intent(in), optional :: xlabel, ylabel
    integer, intent(in), optional :: width_mm, height_mm
    plot_labels%title = title
    if (present (description)) then
       plot_labels%description = description
    else
       plot_labels%description = ""
    end if
    if (present (xlabel)) then
       plot_labels%xlabel = xlabel
    else
       plot_labels%xlabel = ""
    end if
    if (present (ylabel)) then
       plot_labels%ylabel = ylabel
    else
       plot_labels%ylabel = ""
    end if
    if (present (width_mm))  plot_labels%width_mm = width_mm
    if (present (height_mm))  plot_labels%height_mm = height_mm
  end subroutine plot_labels_init

  subroutine observable_init (obs, label, physical_unit, plot_labels)
    type(observable_t), intent(out) :: obs
    type(string_t), intent(in), optional :: label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    if (present (label)) then
       obs%label = label
    else
       obs%label = ""
    end if
    if (present (physical_unit)) then
       obs%physical_unit = physical_unit
    else
       obs%physical_unit = ""
    end if
    if (present (plot_labels)) then
       obs%plot_labels = plot_labels
    else
       call plot_labels_init (obs%plot_labels, title = var_str ("Observable"))
    end if
  end subroutine observable_init

  subroutine observable_clear (obs)
    type(observable_t), intent(inout) :: obs
    obs%sum_values = 0
    obs%sum_squared_values = 0
    obs%sum_weights = 0
    obs%sum_squared_weights = 0
    obs%count = 0
  end subroutine observable_clear

  subroutine observable_record_value_unweighted (obs, value, success)
    type(observable_t), intent(inout) :: obs
    real(default), intent(in) :: value
    logical, intent(out), optional :: success
    obs%sum_values = obs%sum_values + value
    obs%sum_squared_values = obs%sum_squared_values + value**2
    obs%sum_weights = obs%sum_weights + 1
    obs%sum_squared_weights = obs%sum_squared_weights + 1
    obs%count = obs%count + 1
    if (present (success))  success = .true.
  end subroutine observable_record_value_unweighted

  subroutine observable_record_value_weighted (obs, value, weight, success)
    type(observable_t), intent(inout) :: obs
    real(default), intent(in) :: value, weight
    logical, intent(out), optional :: success
    obs%sum_values = obs%sum_values + value * weight
    obs%sum_squared_values = obs%sum_squared_values + value**2 * weight
    obs%sum_weights = obs%sum_weights + abs (weight)
    obs%sum_squared_weights = obs%sum_squared_weights + weight**2
    obs%count = obs%count + 1
    if (present (success))  success = .true.
  end subroutine observable_record_value_weighted

  function observable_get_n_entries (obs) result (n)
    integer :: n
    type(observable_t), intent(in) :: obs
    n = obs%count
  end function observable_get_n_entries

  function observable_get_average (obs) result (avg)
    real(default) :: avg
    type(observable_t), intent(in) :: obs
    if (obs%sum_weights /= 0) then
       avg = obs%sum_values / obs%sum_weights
    else
       avg = 0
    end if
  end function observable_get_average

  function observable_get_error (obs) result (err)
    real(default) :: err
    type(observable_t), intent(in) :: obs
    real(default) :: var, n
    if (obs%sum_weights /= 0) then
       select case (obs%count)
       case (0:1)
          err = 0
       case default
          n = obs%count
          var = obs%sum_squared_values / obs%sum_weights &
                - (obs%sum_values / obs%sum_weights) ** 2
          err = sqrt (max (var, 0._default) / (n - 1))
       end select
    else
       err = 0
    end if
  end function observable_get_error

  function observable_get_label (obs, wl, wu) result (string)
    type(string_t) :: string
    type(observable_t), intent(in) :: obs 
    logical, intent(in) :: wl, wu
    type(string_t) :: label, physical_unit
    if (wl) then
       if (obs%label /= "") then
          label = obs%label
       else
          label = "\mathcal{O}"
       end if
    else
       label = ""
    end if
    if (wu) then
       if (obs%physical_unit /= "") then
          if (wl) then
             physical_unit = "\;[" // obs%physical_unit // "]"
          else
             physical_unit = obs%physical_unit
          end if
       else
          physical_unit = ""
       end if
    else
       physical_unit = ""
    end if
    string = label // physical_unit
  end function observable_get_label

  subroutine observable_write (obs, unit)
    type(observable_t), intent(in) :: obs
    integer, intent(in), optional :: unit
    real(default) :: avg, err, relerr
    integer :: n
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    avg = observable_get_average (obs)
    err = observable_get_error (obs)
    if (avg /= 0) then
       relerr = err / avg
    else
       relerr = 0
    end if
    n = observable_get_n_entries (obs)
    write (u, "(A,1x," // HISTOGRAM_DATA_FORMAT // ")") &
         "average     =", avg
    write (u, "(A,1x," // HISTOGRAM_DATA_FORMAT // ")") &
         "error[abs]  =", err
    write (u, "(A,1x," // HISTOGRAM_DATA_FORMAT // ")") &
         "error[rel]  =", relerr
    write (u, "(A,1x," // HISTOGRAM_INTG_FORMAT // ")") &
         "n_entries   =", n
  end subroutine observable_write

  subroutine observable_write_driver (obs, unit, write_heading)
    type(observable_t), intent(in) :: obs
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: write_heading
    real(default) :: avg, err
    integer :: n_digits
    logical :: heading
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    heading = .true.;  if (present (write_heading))  heading = write_heading
    avg = observable_get_average (obs)
    err = observable_get_error (obs)
    if (avg /= 0 .and. err /= 0) then
       n_digits = max (2, 2 - int (log10 (abs (err / real (avg, default)))))
    else if (avg /= 0) then
       n_digits = 100
    else
       n_digits = 1
    end if
    if (heading) then
       write (u, "(A)") 
       if (obs%plot_labels%title /= "") then
          write (u, "(A)")  "\section{" // char (obs%plot_labels%title) &
               // "}"
       else
          write (u, "(A)")  "\section{Observable}"
       end if
       if (obs%plot_labels%description /= "") then
          write (u, "(A)")  char (obs%plot_labels%description)
          write (u, *)
       end if
       write (u, "(A)")  "\begin{flushleft}"
    end if
    write (u, "(A)", advance="no")  "  $\langle{" ! $ sign
    write (u, "(A)", advance="no")  char (observable_get_label (obs, wl=.true., wu=.false.))
    write (u, "(A)", advance="no")  "}\rangle = "
    write (u, "(A)", advance="no")  char (tex_format (avg, n_digits))
    write (u, "(A)", advance="no")  "\pm"
    write (u, "(A)", advance="no")  char (tex_format (err, 2))
    write (u, "(A)", advance="no")  "\;{"
    write (u, "(A)", advance="no")  char (observable_get_label (obs, wl=.false., wu=.true.))
    write (u, "(A)")  "}"
    write (u, "(A)", advance="no")  "     \quad[n_{\text{entries}} = "
    write (u, "(I0)",advance="no")  observable_get_n_entries (obs)
    write (u, "(A)")  "]$"          ! $ fool Emacs' noweb mode 
    if (heading) then
       write (u, "(A)") "\end{flushleft}"
    end if
  end subroutine observable_write_driver

  subroutine bin_init (bin, midpoint, width)
    type(bin_t), intent(out) :: bin
    real(default), intent(in) :: midpoint, width
    bin%midpoint = midpoint
    bin%width = width
  end subroutine bin_init

  elemental subroutine bin_clear (bin)
    type(bin_t), intent(inout) :: bin
    bin%sum_weights = 0
    bin%sum_squared_weights = 0
    bin%sum_excess_weights = 0
    bin%count = 0
  end subroutine bin_clear

  subroutine bin_record_value (bin, weight, excess)
    type(bin_t), intent(inout) :: bin
    real(default), intent(in) :: weight
    real(default), intent(in), optional :: excess
    bin%sum_weights = bin%sum_weights + abs (weight)
    bin%sum_squared_weights = bin%sum_squared_weights + weight ** 2
    if (present (excess)) &
         bin%sum_excess_weights = bin%sum_excess_weights + abs (excess)
    bin%count = bin%count + 1
  end subroutine bin_record_value

  function bin_get_midpoint (bin) result (x)
    real(default) :: x
    type(bin_t), intent(in) :: bin
    x = bin%midpoint
  end function bin_get_midpoint

  function bin_get_width (bin) result (w)
    real(default) :: w
    type(bin_t), intent(in) :: bin
    w = bin%width
  end function bin_get_width

  function bin_get_n_entries (bin) result (n)
    integer :: n
    type(bin_t), intent(in) :: bin
    n = bin%count
  end function bin_get_n_entries
  
  function bin_get_sum (bin) result (s)
    real(default) :: s
    type(bin_t), intent(in) :: bin
    s = bin%sum_weights
  end function bin_get_sum

  function bin_get_error (bin) result (err)
    real(default) :: err
    type(bin_t), intent(in) :: bin
    err = sqrt (bin%sum_squared_weights)
  end function bin_get_error

  function bin_get_excess (bin) result (excess)
    real(default) :: excess
    type(bin_t), intent(in) :: bin
    excess = bin%sum_excess_weights
  end function bin_get_excess

  subroutine bin_write_header (unit)
    integer, intent(in), optional :: unit
    character(120) :: buffer
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (buffer, "(A,5(1x," // HISTOGRAM_HEAD_FORMAT // "))") &
         "#", "bin midpoint", "value    ", "error    ", &
         "n_entries ", "excess     "
    write (u, "(A)")  trim (buffer)
  end subroutine bin_write_header

  subroutine bin_write (bin, unit)
    type(bin_t), intent(in) :: bin
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,3(1x," // HISTOGRAM_DATA_FORMAT // ")," &
                       // HISTOGRAM_INTG_FORMAT // "," &
                       // HISTOGRAM_DATA_FORMAT // ")") &
         bin_get_midpoint (bin), &
         bin_get_sum (bin), &
         bin_get_error (bin), &
         bin_get_n_entries (bin), &
         bin_get_excess (bin)
  end subroutine bin_write

  subroutine histogram_init_n_bins (h, &
       lower_bound, upper_bound, n_bins, obs_label, physical_unit, plot_labels)
    type(histogram_t), intent(out) :: h
    real(default), intent(in) :: lower_bound, upper_bound
    integer, intent(in) :: n_bins
    type(string_t), intent(in), optional :: obs_label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    real(default) :: bin_width
    integer :: i
    call observable_init (h%obs_within_bounds, obs_label, physical_unit) 
    call observable_init (h%obs, obs_label, physical_unit) 
    h%lower_bound = lower_bound
    h%upper_bound = upper_bound
    h%n_bins = max (n_bins, 1)
    h%width = h%upper_bound - h%lower_bound
    bin_width = h%width / h%n_bins
    allocate (h%bin (h%n_bins))
    call bin_init (h%underflow, h%lower_bound, 0._default)
    do i = 1, h%n_bins
       call bin_init (h%bin(i), &
            h%lower_bound - bin_width/2 + i * bin_width, bin_width)
    end do
    call bin_init (h%overflow, h%upper_bound, 0._default)
    if (present (plot_labels)) then
       h%plot_labels = plot_labels
    else
       call plot_labels_init (h%plot_labels, &
            title = var_str (""), xlabel = var_str (""), ylabel = var_str (""))
    end if
  end subroutine histogram_init_n_bins

  subroutine histogram_init_bin_width (h, &
       lower_bound, upper_bound, bin_width, &
       obs_label, physical_unit, plot_labels)
    type(histogram_t), intent(out) :: h
    real(default), intent(in) :: lower_bound, upper_bound, bin_width
    type(string_t), intent(in), optional :: obs_label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    integer :: n_bins
    if (bin_width /= 0) then
       n_bins = nint ((upper_bound - lower_bound) / bin_width)
    else
       n_bins = 1
    end if
    call histogram_init_n_bins (h, &
         lower_bound, upper_bound, n_bins, &
         obs_label, physical_unit, plot_labels)
  end subroutine histogram_init_bin_width

  subroutine histogram_clear (h)
    type(histogram_t), intent(inout) :: h
    call observable_clear (h%obs)
    call observable_clear (h%obs_within_bounds)
    call bin_clear (h%underflow)
    if (allocated (h%bin))  call bin_clear (h%bin)
    call bin_clear (h%overflow)
  end subroutine histogram_clear

  subroutine histogram_record_value_unweighted (h, value, excess, success)
    type(histogram_t), intent(inout) :: h
    real(default), intent(in) :: value
    real(default), intent(in), optional :: excess
    logical, intent(out), optional :: success
    integer :: i_bin
    call observable_record_value (h%obs, value)
    if (h%width /= 0) then
       i_bin = floor (((value - h%lower_bound) / h%width) * h%n_bins) + 1
    else
       i_bin = 0
    end if
    if (i_bin <= 0) then
       call bin_record_value (h%underflow, 1._default, excess)
       if (present (success))  success = .false.
    else if (i_bin <= h%n_bins) then
       call observable_record_value (h%obs_within_bounds, value)
       call bin_record_value (h%bin(i_bin), 1._default, excess)
       if (present (success))  success = .true.
    else
       call bin_record_value (h%overflow, 1._default, excess)
       if (present (success))  success = .false.
    end if
  end subroutine histogram_record_value_unweighted

  subroutine histogram_record_value_weighted (h, value, weight, success)
    type(histogram_t), intent(inout) :: h
    real(default), intent(in) :: value, weight
    logical, intent(out), optional :: success
    integer :: i_bin
    call observable_record_value (h%obs, value, weight)
    if (h%width /= 0) then
       i_bin = floor (((value - h%lower_bound) / h%width) * h%n_bins) + 1
    else
       i_bin = 0
    end if
    if (i_bin <= 0) then
       call bin_record_value (h%underflow, weight)
       if (present (success))  success = .false.
    else if (i_bin <= h%n_bins) then
       call observable_record_value (h%obs_within_bounds, weight)
       call bin_record_value (h%bin(i_bin), weight)
       if (present (success))  success = .true.
    else
       call bin_record_value (h%overflow, weight)
       if (present (success))  success = .false.
    end if
  end subroutine histogram_record_value_weighted

  function histogram_get_n_entries (h) result (n)
    integer :: n
    type(histogram_t), intent(in) :: h
    n = observable_get_n_entries (h%obs)
  end function histogram_get_n_entries

  function histogram_get_average (h) result (avg)
    real(default) :: avg
    type(histogram_t), intent(in) :: h
    avg = observable_get_average (h%obs)
  end function histogram_get_average

  function histogram_get_error (h) result (err)
    real(default) :: err
    type(histogram_t), intent(in) :: h
    err = observable_get_error (h%obs)
  end function histogram_get_error

  function histogram_get_n_entries_within_bounds (h) result (n)
    integer :: n
    type(histogram_t), intent(in) :: h
    n = observable_get_n_entries (h%obs_within_bounds)
  end function histogram_get_n_entries_within_bounds

  function histogram_get_average_within_bounds (h) result (avg)
    real(default) :: avg
    type(histogram_t), intent(in) :: h
    avg = observable_get_average (h%obs_within_bounds)
  end function histogram_get_average_within_bounds

  function histogram_get_error_within_bounds (h) result (err)
    real(default) :: err
    type(histogram_t), intent(in) :: h
    err = observable_get_error (h%obs_within_bounds)
  end function histogram_get_error_within_bounds

  function histogram_get_n_entries_for_bin (h, i) result (n)
    integer :: n
    type(histogram_t), intent(in) :: h
    integer, intent(in) :: i
    if (i <= 0) then
       n = bin_get_n_entries (h%underflow)
    else if (i <= h%n_bins) then
       n = bin_get_n_entries (h%bin(i))
    else
       n = bin_get_n_entries (h%overflow)
    end if
  end function histogram_get_n_entries_for_bin

  function histogram_get_sum_for_bin (h, i) result (avg)
    real(default) :: avg
    type(histogram_t), intent(in) :: h
    integer, intent(in) :: i
    if (i <= 0) then
       avg = bin_get_sum (h%underflow)
    else if (i <= h%n_bins) then
       avg = bin_get_sum (h%bin(i))
    else
       avg = bin_get_sum (h%overflow)
    end if
  end function histogram_get_sum_for_bin

  function histogram_get_error_for_bin (h, i) result (err)
    real(default) :: err
    type(histogram_t), intent(in) :: h
    integer, intent(in) :: i
    if (i <= 0) then
       err = bin_get_error (h%underflow)
    else if (i <= h%n_bins) then
       err = bin_get_error (h%bin(i))
    else
       err = bin_get_error (h%overflow)
    end if
  end function histogram_get_error_for_bin

  function histogram_get_excess_for_bin (h, i) result (err)
    real(default) :: err
    type(histogram_t), intent(in) :: h
    integer, intent(in) :: i
    if (i <= 0) then
       err = bin_get_excess (h%underflow)
    else if (i <= h%n_bins) then
       err = bin_get_excess (h%bin(i))
    else
       err = bin_get_excess (h%overflow)
    end if
  end function histogram_get_excess_for_bin

  subroutine histogram_write (h, unit)
    type(histogram_t), intent(in) :: h
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call bin_write_header (u)
    if (allocated (h%bin)) then
       do i = 1, h%n_bins
          call bin_write (h%bin(i), u)
       end do
    end if
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Underflow:"
    call bin_write (h%underflow, u)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Overflow:"
    call bin_write (h%overflow, u)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Summary: data within bounds"
    call observable_write (h%obs_within_bounds, u)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Summary: all data"
    call observable_write (h%obs, u)
    write (u, *)
  end subroutine histogram_write

  subroutine histogram_write_driver (h, filename, unit, write_heading)
    type(histogram_t), intent(in) :: h
    type(string_t), intent(in) :: filename
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: write_heading
    logical :: heading
    character(len=32) :: lower_bound_str, upper_bound_str, bin_half_width_str
    logical, parameter :: log_plot = .false.
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    heading = .true.;  if (present (write_heading))  heading = write_heading
    if (heading) then
       write (u, "(A)")
       if (h%plot_labels%title /= "") then
          write (u, "(A)")  "\section{" // char (h%plot_labels%title) // "}"
       else
          write (u, "(A)")  "\section{Histogram}"
       end if
    end if
    if (h%plot_labels%description /= "") then
       write (u, "(A)")  char (h%plot_labels%description)
       write (u, *)
       write (u, "(A)")  "\vspace*{\baselineskip}"
    end if
    lower_bound_str = ""
    upper_bound_str = ""
    bin_half_width_str = ""
1   format (G10.3)
    write (lower_bound_str, 1)  h%lower_bound
    write (upper_bound_str, 1)  h%upper_bound
    write (bin_half_width_str,   1)  h%width / h%n_bins / 2
    lower_bound_str = adjustl (lower_bound_str)
    upper_bound_str = adjustl (upper_bound_str)
    bin_half_width_str = adjustl (bin_half_width_str)
    write (u, "(A)")  "\vspace*{\baselineskip}"
    write (u, "(A)")  "\unitlength 1mm"
    write (u, "(A,I0,',',I0,A)")  &
         "\begin{gmlgraph*}(", &
         h%plot_labels%width_mm, h%plot_labels%height_mm, &
         ")[dat]"
    if (log_plot) then
       write (u, "(2x,A)")  "setup (linear,log); "
       write (u, "(2x,A)")  &
            "graphrange (#" // trim (lower_bound_str) // ", ??), " &
            //         "(#" // trim (upper_bound_str) // ", ??);"
    else
       write (u, "(2x,A)")  "setup (linear,linear); "
       write (u, "(2x,A)")  &
            "graphrange (#" // trim (lower_bound_str) // ", #0), " &
            //         "(#" // trim (upper_bound_str) // ", ??);"
    end if
    write (u, "(2x,A)")  'fromfile "' // char (filename) // '":'
    write (u, "(4x,A)")  'key "# Histogram:";'
    write (u, "(4x,A)")  'dx := #' // trim (bin_half_width_str) // ';'
    write (u, "(4x,A)")  'for i withinblock:' 
    write (u, "(6x,A)")  'get x, y, y.d, n, y.e;'
    write (u, "(6x,A)")  'plot (dat) (x,y) hbar dx;'
!     write (u, "(6x,A)")  'if show_excess: ' // &
!                & 'plot(dat.e)(x, y plus y.e) hbar dx; fi'
    write (u, "(4x,A)")  'endfor'
    write (u, "(4x,A)")  'calculate dat.base (dat) (x,#0);'
    write (u, "(2x,A)")  'endfrom'
    write (u, "(2x,A)")  'fill piecewise from (dat, dat.base/\) ' &
         // 'withcolor col.default outlined;'
!     write (u, "(2x,A)") 'if show_excess: ' // &
!                & 'fill piecewise from(dat.e, dat/\) '// &
!                & 'withcolor col.excess outlined; fi'
!     if (mcs%normalize_weight) then
    if (h%plot_labels%ylabel /= "") then
       write (u, "(2x,A)")  'label.ulft (<' // '<' &
            // char (h%plot_labels%ylabel) // '>' // '>, out);'
    else
       write (u, "(2x,A)")  'label.ulft (<' // '<\#evt/bin>' // '>, out);'
    end if
!     else
!        write(u, '(2x,A)') 'label.ulft(<'//'<$d\sigma\,[{\rm fb}]$/bin>'//'>, out);'
!     end if
    if (h%plot_labels%xlabel /= "") then
       write (u, "(2x,A)", advance="no")  'label.bot (<' // '<' &
            // char (h%plot_labels%xlabel) // '>' &
            // '>, out);'
    else
       write (u, "(2x,A)", advance="no")  'label.bot (<' // '<${'
       write (u, "(A)", advance="no")  &
            char (observable_get_label (h%obs, wl=.true., wu=.true.))
       write (u, "(A)")  '}$>' // '>, out);'
    end if
    write (u, "(A)") "\end{gmlgraph*}"
    write (u, "(A)") "\vspace*{2\baselineskip}"
    write (u, "(A)") "\begin{flushleft}"
    write (u, "(A)") "\textbf{Data within bounds:} \\"
    call observable_write_driver (h%obs_within_bounds, unit, &
                                  write_heading=.false.)
    write (u, "(A)") "\\[0.5\baselineskip]"
    write (u, "(A)") "\textbf{All data:} \\"
    call observable_write_driver (h%obs, unit, write_heading=.false.)
    write (u, "(A)") "\end{flushleft}"
  end subroutine histogram_write_driver

  subroutine point_init (point, x, y, xerr, yerr)
    type(point_t), intent(out) :: point
    real(default), intent(in) :: x, y
    real(default), intent(in), optional :: xerr, yerr
    point%x = x
    point%y = y
    if (present (xerr))  point%xerr = xerr
    if (present (yerr))  point%yerr = yerr
  end subroutine point_init

  function point_get_x (point) result (x)
    real(default) :: x
    type(point_t), intent(in) :: point
    x = point%x
  end function point_get_x

  function point_get_y (point) result (y)
    real(default) :: y
    type(point_t), intent(in) :: point
    y = point%y
  end function point_get_y

  function point_get_xerr (point) result (xerr)
    real(default) :: xerr
    type(point_t), intent(in) :: point
    xerr = point%xerr
  end function point_get_xerr

  function point_get_yerr (point) result (yerr)
    real(default) :: yerr
    type(point_t), intent(in) :: point
    yerr = point%yerr
  end function point_get_yerr

  subroutine point_write_header (unit)
    integer, intent(in) :: unit
    character(120) :: buffer
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (buffer, "(A,4(1x," // HISTOGRAM_HEAD_FORMAT // "))") &
         "#", "x       ", "y       ", "xerr     ", "yerr     "
    write (u, "(A)")  trim (buffer)
  end subroutine point_write_header

  subroutine point_write (point, unit)
    type(point_t), intent(in) :: point
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,4(1x," // HISTOGRAM_DATA_FORMAT // ")),") &
         point_get_x (point), &
         point_get_y (point), &
         point_get_xerr (point), &
         point_get_yerr (point)
  end subroutine point_write

  subroutine plot_init (plot, lower_bound, upper_bound, plot_labels)
    type(plot_t), intent(out) :: plot
    real(default), intent(in) :: lower_bound, upper_bound
    type(plot_labels_t), intent(in), optional :: plot_labels
    plot%lower_bound = lower_bound
    plot%upper_bound = upper_bound
    plot%width = plot%upper_bound - plot%lower_bound
    call bin_init (plot%underflow, plot%lower_bound, 0._default)
    call bin_init (plot%overflow, plot%upper_bound, 0._default)
    if (present (plot_labels)) then
       plot%plot_labels = plot_labels
    else
       call plot_labels_init (plot%plot_labels, &
            title = var_str (""), xlabel = var_str (""), ylabel = var_str (""))
    end if
  end subroutine plot_init

  subroutine plot_final (plot)
    type(plot_t), intent(inout) :: plot
    type(point_t), pointer :: current
    do while (associated (plot%first))
       current => plot%first
       plot%first => current%next
       deallocate (current)
    end do
    plot%last => null ()
  end subroutine plot_final

  subroutine plot_clear (plot)
    type(plot_t), intent(inout) :: plot
    call bin_clear (plot%underflow)
    call bin_clear (plot%overflow)
    plot%count = 0
    plot%count_within_bounds = 0
    call plot_final (plot)
  end subroutine plot_clear

  subroutine plot_record_value (plot, x, y, xerr, yerr, success)
    type(plot_t), intent(inout) :: plot
    real(default), intent(in) :: x, y
    real(default), intent(in), optional :: xerr, yerr
    logical, intent(out), optional :: success
    type(point_t), pointer :: point
    plot%count = plot%count + 1
    if (x < plot%lower_bound) then
       call bin_record_value (plot%underflow, 1._default)
       if (present (success))  success = .false.
    else if (x <= plot%upper_bound) then
       plot%count_within_bounds = plot%count_within_bounds + 1
       allocate (point)
       call point_init (point, x, y, xerr, yerr)
       if (associated (plot%first)) then
          plot%last%next => point
       else
          plot%first => point
       end if
       plot%last => point
       if (present (success))  success = .true.
    else
       call bin_record_value (plot%overflow, 1._default)
       if (present (success))  success = .false.
    end if
  end subroutine plot_record_value

  function plot_get_n_entries (plot) result (n)
    integer :: n
    type(plot_t), intent(in) :: plot
    n = plot%count
  end function plot_get_n_entries

  function plot_get_n_entries_within_bounds (plot) result (n)
    integer :: n
    type(plot_t), intent(in) :: plot
    n = plot%count_within_bounds
  end function plot_get_n_entries_within_bounds

  subroutine plot_write (plot, unit)
    type(plot_t), intent(in) :: plot
    integer, intent(in), optional :: unit
    type(point_t), pointer :: point
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call point_write_header (u)
    point => plot%first
    do while (associated (point))
       call point_write (point, unit)
       point => point%next
    end do
    write (u, *)
    call bin_write_header (u)
    write (u, "(A,1x,A)")  "#", "Underflow:"
    call bin_write (plot%underflow, u)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Overflow:"
    call bin_write (plot%overflow, u)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Summary: points within bounds"
    write (u, "(A," // HISTOGRAM_INTG_FORMAT // ")") &
         "n_entries = ", plot_get_n_entries_within_bounds (plot)
    write (u, *)
    write (u, "(A,1x,A)")  "#", "Summary: all points"
    write (u, "(A," // HISTOGRAM_INTG_FORMAT // ")") &
         "n_entries = ", plot_get_n_entries (plot)
    write (u, *)
  end subroutine plot_write

  subroutine plot_write_driver (plot, filename, unit, write_heading)
    type(plot_t), intent(in) :: plot
    type(string_t), intent(in) :: filename
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: write_heading
    logical :: heading
    character(len=32) :: lower_bound_str, upper_bound_str
    logical, parameter :: log_plot = .false.
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    heading = .true.;  if (present (write_heading))  heading = write_heading
    if (heading) then
       write (u, "(A)")
       if (plot%plot_labels%title /= "") then
          write (u, "(A)")  "\section{" // char (plot%plot_labels%title) // "}"
       else
          write (u, "(A)")  "\section{Plot}"
       end if
    end if
    if (plot%plot_labels%description /= "") then
       write (u, "(A)")  char (plot%plot_labels%description)
       write (u, *)
       write (u, "(A)")  "\vspace*{\baselineskip}"
    end if
    lower_bound_str = ""
    upper_bound_str = ""
1   format (G10.3)
    write (lower_bound_str, 1)  plot%lower_bound
    write (upper_bound_str, 1)  plot%upper_bound
    lower_bound_str = adjustl (lower_bound_str)
    upper_bound_str = adjustl (upper_bound_str)
    write (u, "(A)")  "\vspace*{\baselineskip}"
    write (u, "(A)")  "\unitlength 1mm"
    write (u, "(A,I0,',',I0,A)")  &
         "\begin{gmlgraph*}(", &
         plot%plot_labels%width_mm, plot%plot_labels%height_mm, &
         ")[dat]"
    if (log_plot) then
       write (u, "(2x,A)")  "setup (linear,log); "
       write (u, "(2x,A)")  &
            "graphrange (#" // trim (lower_bound_str) // ", ??), " &
            //         "(#" // trim (upper_bound_str) // ", ??);"
    else
       write (u, "(2x,A)")  "setup (linear,linear); "
       write (u, "(2x,A)")  &
            "graphrange (#" // trim (lower_bound_str) // ", #0), " &
            //         "(#" // trim (upper_bound_str) // ", ??);"
    end if
    write (u, "(2x,A)")  'fromfile "' // char (filename) // '":'
    write (u, "(4x,A)")  'key "# Plot:";'
    write (u, "(4x,A)")  'for i withinblock:' 
    write (u, "(6x,A)")  'get x, y, x.err, y.err;'
    write (u, "(6x,A)")  'plot (dat) (x,y);'
    write (u, "(4x,A)")  'endfor'
    write (u, "(2x,A)")  'endfrom'
    write (u, "(2x,A)")  'draw from (dat); '
    if (plot%plot_labels%ylabel /= "") then
       write (u, "(2x,A)")  'label.ulft (<' // '<' &
            // char (plot%plot_labels%ylabel) // '>' // '>, out);'
    else
       write (u, "(2x,A)")  'label.ulft (<' // '<y>' // '>, out);'
    end if
    if (plot%plot_labels%xlabel /= "") then
       write (u, "(2x,A)", advance="no")  'label.bot (<' // '<' &
            // char (plot%plot_labels%xlabel) // '>' &
            // '>, out);'
    else
       write (u, "(2x,A)")  'label.ulft (<' // '<x>' // '>, out);'
    end if
    write (u, "(A)") "\end{gmlgraph*}"
    write (u, "(A)") "\vspace*{2\baselineskip}"
    write (u, "(A)") "\begin{flushleft}"
    write (u, "(A)") "\textbf{Data within bounds:} \\"
    write (u, "(A)", advance="no")  "$n_{\text{entries}} = "
    write (u, "(I0,A)",advance="no")  plot%count_within_bounds, "$"
    write (u, "(A)") "\\[0.5\baselineskip]"
    write (u, "(A)") "\textbf{All data:} \\"
    write (u, "(A)", advance="no")  "$n_{\text{entries}} = "
    write (u, "(I0,A)")  plot%count, "$"
    write (u, "(A)") "\end{flushleft}"
  end subroutine plot_write_driver

  subroutine analysis_object_init (obj, id, type)
    type(analysis_object_t), intent(out) :: obj
    type(string_t), intent(in) :: id
    integer, intent(in) :: type
    obj%id = id
    obj%type = type
    select case (obj%type)
    case (AN_OBSERVABLE);  allocate (obj%obs)
    case (AN_HISTOGRAM);   allocate (obj%h)
    case (AN_PLOT);        allocate (obj%plot)
    end select
  end subroutine analysis_object_init

  subroutine analysis_object_final (obj)
    type(analysis_object_t), intent(inout) :: obj
    select case (obj%type)
    case (AN_OBSERVABLE)
       deallocate (obj%obs)
    case (AN_HISTOGRAM)
       deallocate (obj%h)
    case (AN_PLOT)
       call plot_final (obj%plot)
       deallocate (obj%plot)
    end select
    obj%type = AN_UNDEFINED
  end subroutine analysis_object_final

  subroutine analysis_object_clear (obj)
    type(analysis_object_t), intent(inout) :: obj
    select case (obj%type)
    case (AN_OBSERVABLE)
       call observable_clear (obj%obs)
    case (AN_HISTOGRAM)
       call histogram_clear (obj%h)
    case (AN_PLOT)
       call plot_clear (obj%plot)
    end select
  end subroutine analysis_object_clear

  subroutine analysis_object_record_data (obj, &
       x, y, xerr, yerr, weight, excess, success)
    type(analysis_object_t), intent(inout) :: obj
    real(default), intent(in) :: x
    real(default), intent(in), optional :: y, xerr, yerr, weight, excess
    logical, intent(out), optional :: success
    select case (obj%type)
    case (AN_OBSERVABLE)
       if (present (weight)) then
          call observable_record_value_weighted (obj%obs, x, weight, success)
       else
          call observable_record_value_unweighted (obj%obs, x, success)
       end if
    case (AN_HISTOGRAM)
       if (present (weight)) then
          call histogram_record_value_weighted (obj%h, x, weight, success)
       else
          call histogram_record_value_unweighted (obj%h, x, excess, success)
       end if
    case (AN_PLOT)
       if (present (y)) then
          call plot_record_value (obj%plot, x, y, xerr, yerr, success)
       else
          if (present (success))  success = .false.
       end if
    case default
       if (present (success))  success = .false.
    end select
  end subroutine analysis_object_record_data

  subroutine analysis_object_set_next_ptr (obj, next)
    type(analysis_object_t), intent(inout) :: obj
    type(analysis_object_t), pointer :: next
    obj%next => next
  end subroutine analysis_object_set_next_ptr

  function analysis_object_get_next_ptr (obj) result (next)
    type(analysis_object_t), pointer :: next
    type(analysis_object_t), intent(in) :: obj
    next => obj%next
  end function analysis_object_get_next_ptr

  function analysis_object_get_n_entries (obj, within_bounds) result (n)
    integer :: n
    type(analysis_object_t), intent(in) :: obj
    logical, intent(in), optional :: within_bounds
    logical :: wb
    select case (obj%type)
    case (AN_OBSERVABLE)
       n = observable_get_n_entries (obj%obs)
    case (AN_HISTOGRAM)
       wb = .false.;  if (present (within_bounds)) wb = within_bounds
       if (wb) then
          n = histogram_get_n_entries_within_bounds (obj%h)
       else
          n = histogram_get_n_entries (obj%h)
       end if
    case (AN_PLOT)
       wb = .false.;  if (present (within_bounds)) wb = within_bounds
       if (wb) then
          n = plot_get_n_entries_within_bounds (obj%plot)
       else
          n = plot_get_n_entries (obj%plot)
       end if
    case default
       n = 0
    end select
  end function analysis_object_get_n_entries

  function analysis_object_get_average (obj, within_bounds) result (avg)
    real(default) :: avg
    type(analysis_object_t), intent(in) :: obj
    logical, intent(in), optional :: within_bounds
    logical :: wb
    select case (obj%type)
    case (AN_OBSERVABLE)
       avg = observable_get_average (obj%obs)
    case (AN_HISTOGRAM)
       wb = .false.;  if (present (within_bounds)) wb = within_bounds
       if (wb) then
          avg = histogram_get_average_within_bounds (obj%h)
       else
          avg = histogram_get_average (obj%h)
       end if
    case default
       avg = 0
    end select
  end function analysis_object_get_average

  function analysis_object_get_error (obj, within_bounds) result (err)
    real(default) :: err
    type(analysis_object_t), intent(in) :: obj
    logical, intent(in), optional :: within_bounds
    logical :: wb
    select case (obj%type)
    case (AN_OBSERVABLE)
       err = observable_get_error (obj%obs)
    case (AN_HISTOGRAM)
       wb = .false.;  if (present (within_bounds)) wb = within_bounds
       if (wb) then
          err = histogram_get_error_within_bounds (obj%h)
       else
          err = histogram_get_error (obj%h)
       end if
    case default
       err = 0
    end select
  end function analysis_object_get_error

  function analysis_object_get_observable_ptr (obj) result (obs)
    type(observable_t), pointer :: obs
    type(analysis_object_t), intent(in) :: obj
    select case (obj%type)
    case (AN_OBSERVABLE);  obs => obj%obs
    case default;          obs => null ()
    end select
  end function analysis_object_get_observable_ptr

  function analysis_object_get_histogram_ptr (obj) result (h)
    type(histogram_t), pointer :: h
    type(analysis_object_t), intent(in) :: obj
    select case (obj%type)
    case (AN_HISTOGRAM);  h => obj%h
    case default;         h => null ()
    end select
  end function analysis_object_get_histogram_ptr

  function analysis_object_get_plot_ptr (obj) result (plot)
    type(plot_t), pointer :: plot
    type(analysis_object_t), intent(in) :: obj
    select case (obj%type)
    case (AN_PLOT);  plot => obj%plot
    case default;    plot => null ()
    end select
  end function analysis_object_get_plot_ptr

  function analysis_object_has_plot (obj) result (flag)
    logical :: flag
    type(analysis_object_t), intent(in) :: obj
    select case (obj%type)
    case (AN_HISTOGRAM);  flag = .true.
    case (AN_PLOT);       flag = .true.
    case default;         flag = .false.
    end select
  end function analysis_object_has_plot

  subroutine analysis_object_write (obj, unit)
    type(analysis_object_t), intent(in) :: obj
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  repeat ("#", 79)
    select case (obj%type)
    case (AN_OBSERVABLE)
       write (u, "(A)", advance="no")  "# Observable:"
    case (AN_HISTOGRAM)
       write (u, "(A)", advance="no")  "# Histogram: "
    case (AN_PLOT)
       write (u, "(A)", advance="no")  "# Plot: "
    case default
       write (u, "(A)") "# [undefined analysis object]"
       return
    end select
    write (u, "(1x,A)")  char (obj%id)
    select case (obj%type)
    case (AN_OBSERVABLE);  call observable_write (obj%obs, unit)
    case (AN_HISTOGRAM);   call histogram_write (obj%h, unit)
    case (AN_PLOT);        call plot_write (obj%plot, unit)
    end select
  end subroutine analysis_object_write

  subroutine analysis_object_write_driver (obj, filename, unit)
    type(analysis_object_t), intent(in) :: obj
    type(string_t), intent(in) :: filename
    integer, intent(in), optional :: unit
    select case (obj%type)
    case (AN_OBSERVABLE);  call observable_write_driver (obj%obs, unit)
    case (AN_HISTOGRAM);   call histogram_write_driver (obj%h, filename, unit)
    case (AN_PLOT);        call plot_write_driver (obj%plot, filename, unit)
    end select
  end subroutine analysis_object_write_driver

  subroutine analysis_final ()
    type(analysis_object_t), pointer :: current
    do while (associated (analysis_store%first))
       current => analysis_store%first
       analysis_store%first => current%next
       call analysis_object_final (current)
    end do
    analysis_store%last => null ()
  end subroutine analysis_final

  subroutine analysis_store_append_object (id, type)
    type(string_t), intent(in) :: id
    integer, intent(in) :: type
    type(analysis_object_t), pointer :: obj
    allocate (obj)
    call analysis_object_init (obj, id, type)
    if (associated (analysis_store%last)) then
       analysis_store%last%next => obj
    else
       analysis_store%first => obj
    end if
    analysis_store%last => obj
  end subroutine analysis_store_append_object

  function analysis_store_get_object_ptr (id) result (obj) 
    type(string_t), intent(in) :: id
    type(analysis_object_t), pointer :: obj
    obj => analysis_store%first
    do while (associated (obj))
       if (obj%id == id)  return
       obj => obj%next
    end do
  end function analysis_store_get_object_ptr

  subroutine analysis_store_init_object (id, type, obj)
    type(string_t), intent(in) :: id
    integer, intent(in) :: type
    type(analysis_object_t), pointer :: obj, next
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       next => analysis_object_get_next_ptr (obj)
       call analysis_object_final (obj)
       call analysis_object_init (obj, id, type)
       call analysis_object_set_next_ptr (obj, next)
    else
       call analysis_store_append_object (id, type)
       obj => analysis_store%last
    end if
  end subroutine analysis_store_init_object

  subroutine analysis_store_write_driver_all (filename_data, unit)
    type(string_t), intent(in) :: filename_data
    integer, intent(in), optional :: unit
    type(analysis_object_t), pointer :: obj
    call analysis_store_write_driver_header (unit)
    obj => analysis_store%first
    do while (associated (obj))
       call analysis_object_write_driver (obj, filename_data, unit)
       obj => obj%next
    end do
    call analysis_store_write_driver_footer (unit)
  end subroutine analysis_store_write_driver_all

  subroutine analysis_store_write_driver_obj (filename_data, id, unit)
    type(string_t), intent(in) :: filename_data
    type(string_t), dimension(:), intent(in) :: id
    integer, intent(in), optional :: unit
    type(analysis_object_t), pointer :: obj
    integer :: i
    call analysis_store_write_driver_header (unit)
    do i = 1, size (id)
       obj => analysis_store_get_object_ptr (id(i))
       if (associated (obj))  &
            call analysis_object_write_driver (obj, filename_data, unit)
    end do
    call analysis_store_write_driver_footer (unit)
  end subroutine analysis_store_write_driver_obj

  subroutine analysis_store_write_driver_header (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write(u, '(A)') "\documentclass[12pt]{article}"
    write(u, *)
    write(u, '(A)') "\usepackage{gamelan}"
    write(u, '(A)') "\usepackage{amsmath}"
    write(u, *)
    write(u, '(A)') "\begin{document}"
    write(u, '(A)') "\begin{gmlfile}"
    write(u, *)
    write(u, '(A)') "\begin{gmlcode}"
    write(u, '(A)') "  color col.default, col.excess;"
    write(u, '(A)') "  col.default = 0.9white;"
    write(u, '(A)') "  col.excess  = red;"
    write(u, '(A)') "  boolean show_excess;"
!    if (mcs(1)%plot_excess .and. mcs(1)%unweighted) then
!       write(u, '(A)') "  show_excess = true;"
!    else
    write(u, '(A)') "  show_excess = false;"
!    end if
    write(u, '(A)') "\end{gmlcode}"
    write(u, *)
  end subroutine analysis_store_write_driver_header

  subroutine analysis_store_write_driver_footer (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write(u, *)
    write(u, '(A)') "\end{gmlfile}"
    write(u, '(A)') "\end{document}"
  end subroutine analysis_store_write_driver_footer

  subroutine analysis_init_observable (id, label, physical_unit, plot_labels)
    type(string_t), intent(in) :: id
    type(string_t), intent(in), optional :: label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    type(analysis_object_t), pointer :: obj
    type(observable_t), pointer :: obs
    call analysis_store_init_object (id, AN_OBSERVABLE, obj)
    obs => analysis_object_get_observable_ptr (obj)
    call observable_init (obs, label, physical_unit, plot_labels)
  end subroutine analysis_init_observable

  subroutine analysis_init_histogram_n_bins &
       (id, lower_bound, upper_bound, n_bins, &
        label, physical_unit, plot_labels)
    type(string_t), intent(in) :: id
    real(default), intent(in) :: lower_bound, upper_bound
    integer, intent(in) :: n_bins
    type(string_t), intent(in), optional :: label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    type(analysis_object_t), pointer :: obj
    type(histogram_t), pointer :: h
    call analysis_store_init_object (id, AN_HISTOGRAM, obj)
    h => analysis_object_get_histogram_ptr (obj)
    call histogram_init (h, &
         lower_bound, upper_bound, n_bins, &
         label, physical_unit, plot_labels)
  end subroutine analysis_init_histogram_n_bins

  subroutine analysis_init_histogram_bin_width &
       (id, lower_bound, upper_bound, bin_width, &
        label, physical_unit, plot_labels)
    type(string_t), intent(in) :: id
    real(default), intent(in) :: lower_bound, upper_bound, bin_width
    type(string_t), intent(in), optional :: label, physical_unit
    type(plot_labels_t), intent(in), optional :: plot_labels
    type(analysis_object_t), pointer :: obj
    type(histogram_t), pointer :: h
    call analysis_store_init_object (id, AN_HISTOGRAM, obj)
    h => analysis_object_get_histogram_ptr (obj)
    call histogram_init (h, &
         lower_bound, upper_bound, bin_width, &
         label, physical_unit, plot_labels)
  end subroutine analysis_init_histogram_bin_width

  subroutine analysis_init_plot (id, lower_bound, upper_bound, plot_labels)
    type(string_t), intent(in) :: id
    real(default), intent(in) :: lower_bound, upper_bound
    type(plot_labels_t), intent(in), optional :: plot_labels
    type(analysis_object_t), pointer :: obj
    type(plot_t), pointer :: plot
    call analysis_store_init_object (id, AN_PLOT, obj)
    plot => analysis_object_get_plot_ptr (obj)
    call plot_init (plot, &
         lower_bound, upper_bound, plot_labels)
  end subroutine analysis_init_plot

  subroutine analysis_store_clear_obj (id)
    type(string_t), intent(in) :: id
    type(analysis_object_t), pointer :: obj
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       call analysis_object_clear (obj)
    end if
  end subroutine analysis_store_clear_obj

  subroutine analysis_store_clear_all ()
    type(analysis_object_t), pointer :: obj
    obj => analysis_store%first
    do while (associated (obj))
       call analysis_object_clear (obj)
       obj => obj%next
    end do
  end subroutine analysis_store_clear_all

  subroutine analysis_record_data (id, x, y, xerr, yerr, &
       weight, excess, success, exist)
    type(string_t), intent(in) :: id
    real(default), intent(in) :: x
    real(default), intent(in), optional :: y, xerr, yerr, weight, excess
    logical, intent(out), optional :: success, exist
    type(analysis_object_t), pointer :: obj
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       call analysis_object_record_data (obj, x, y, xerr, yerr, &
            weight, excess, success)
       if (present (exist))  exist = .true.
    else
       if (present (success))  success = .false.
       if (present (exist))  exist = .false.
    end if
  end subroutine analysis_record_data

  function analysis_get_n_entries (id, within_bounds) result (n)
    integer :: n
    type(string_t), intent(in) :: id
    logical, intent(in), optional :: within_bounds
    type(analysis_object_t), pointer :: obj
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       n = analysis_object_get_n_entries (obj, within_bounds)
    else
       n = 0
    end if
  end function analysis_get_n_entries

  function analysis_get_average (id, within_bounds) result (avg)
    real(default) :: avg
    type(string_t), intent(in) :: id
    type(analysis_object_t), pointer :: obj
    logical, intent(in), optional :: within_bounds
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       avg = analysis_object_get_average (obj, within_bounds)
    else
       avg = 0
    end if
  end function analysis_get_average

  function analysis_get_error (id, within_bounds) result (err)
    real(default) :: err
    type(string_t), intent(in) :: id
    type(analysis_object_t), pointer :: obj
    logical, intent(in), optional :: within_bounds
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       err = analysis_object_get_error (obj, within_bounds)
    else
       err = 0
    end if
  end function analysis_get_error

  function analysis_has_plots_any () result (flag)
    logical :: flag
    type(analysis_object_t), pointer :: obj
    flag = .false.
    obj => analysis_store%first
    do while (associated (obj))
       flag = analysis_object_has_plot (obj)
       if (flag)  return
    end do
  end function analysis_has_plots_any

  function analysis_has_plots_obj (id) result (flag)
    logical :: flag
    type(string_t), dimension(:), intent(in) :: id
    type(analysis_object_t), pointer :: obj
    integer :: i
    flag = .false.
    do i = 1, size (id)
       obj => analysis_store_get_object_ptr (id(i))
       if (associated (obj)) then
          flag = analysis_object_has_plot (obj)
          if (flag)  return
       end if
    end do
  end function analysis_has_plots_obj

  subroutine analysis_write_object (id, unit)
    type(string_t), intent(in) :: id
    integer, intent(in), optional :: unit
    type(analysis_object_t), pointer :: obj
    obj => analysis_store_get_object_ptr (id)
    if (associated (obj)) then
       call analysis_object_write (obj, unit)
    else
       call msg_error ("Analysis object '" // char (id) // "' not found")
    end if
  end subroutine analysis_write_object

  subroutine analysis_write_all (unit)
    integer, intent(in), optional :: unit
    type(analysis_object_t), pointer :: obj
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    obj => analysis_store%first
    do while (associated (obj))
       call analysis_object_write (obj, unit)
       obj => obj%next
    end do
  end subroutine analysis_write_all

  subroutine analysis_write_driver (filename_data, id, unit)
    type(string_t), intent(in) :: filename_data
    type(string_t), dimension(:), intent(in), optional :: id
    integer, intent(in), optional :: unit
    if (present (id)) then
       call analysis_store_write_driver_obj (filename_data, id, unit)
    else
       call analysis_store_write_driver_all (filename_data, unit)
    end if
  end subroutine analysis_write_driver

  subroutine analysis_compile_tex (file, has_gmlcode, os_data)
    type(string_t), intent(in) :: file
    logical, intent(in) :: has_gmlcode
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: setenv
    integer :: status
    if (os_data%event_analysis_ps) then
       BLOCK: do
          if (os_data%whizard_texpath /= "") then
             setenv = "TEXINPUTS=" // os_data%whizard_texpath // ":$TEXINPUTS "
          else
             setenv = ""
          end if
          call os_system_call (setenv // os_data%latex // " " // file, status)
          if (status /= 0)  exit BLOCK
          if (has_gmlcode) then
             call os_system_call (os_data%gml // " " // file, status)
             if (status /= 0)  exit BLOCK
             call os_system_call (setenv // os_data%latex // " " // file, &
                  status)
             if (status /= 0)  exit BLOCK
          end if
          call os_system_call (os_data%dvips // " " // file, status)
          if (status /= 0)  exit BLOCK
          if (os_data%event_analysis_pdf) then
             call os_system_call (os_data%ps2pdf // " " // file // ".ps", &
                                  status)
             if (status /= 0)  exit BLOCK
          end if
          exit BLOCK
       end do BLOCK
       if (status /= 0) then
          call msg_error ("Unable to compile analysis output file")
       end if
    end if
  end subroutine analysis_compile_tex

  subroutine analysis_test ()
    call analysis_test1 ()
    call analysis_final ()
  end subroutine analysis_test

  subroutine analysis_test1 ()
    type(string_t) :: id1, id2, id3, id4
    integer :: i
    id1 = "foo"
    id2 = "bar"
    id3 = "hist"
    id4 = "plot"
    call analysis_init_observable (id1)
    call analysis_init_observable (id2)
    call analysis_init_histogram_bin_width &
         (id3, 0.5_default, 5.5_default, 1._default)
    call analysis_init_plot (id4, 0.5_default, 5.5_default)
    do i = 1, 3
       print *, "data = ", real(i,default)
       call analysis_record_data (id1, real(i,default))
       call analysis_record_data (id2, real(i,default), &
                                        weight=real(i,default))
       call analysis_record_data (id3, real(i,default))
       call analysis_record_data (id4, real(i,default), real(i,default)**2)
    end do
1   format (A,10(1x,I5))
2   format (A,10(1x,F5.3))
    print 1, "n_entries = ", &
         analysis_get_n_entries (id1), &
         analysis_get_n_entries (id2), &
         analysis_get_n_entries (id3), &
         analysis_get_n_entries (id3, within_bounds = .true.), &
         analysis_get_n_entries (id4), &
         analysis_get_n_entries (id4, within_bounds = .true.)
    print 2, "average   = ", &
         analysis_get_average (id1), &
         analysis_get_average (id2), &
         analysis_get_average (id3), &
         analysis_get_average (id3, within_bounds = .true.)
    print 2, "error     = ", &
         analysis_get_error (id1), &
         analysis_get_error (id2), &
         analysis_get_error (id3), &
         analysis_get_error (id3, within_bounds = .true.)
    print *, "clear #2"
    call analysis_clear (id2)
    do i = 4, 6
       print *, "data = ", real(i,default)
       call analysis_record_data (id1, real(i,default))
       call analysis_record_data (id2, real(i,default), &
                                        weight=real(i,default))
       call analysis_record_data (id3, real(i,default))
       call analysis_record_data (id4, real(i,default), real(i,default)**2)
    end do
    print 1, "n_entries = ", &
         analysis_get_n_entries (id1), &
         analysis_get_n_entries (id2), &
         analysis_get_n_entries (id3), &
         analysis_get_n_entries (id3, within_bounds = .true.), &
         analysis_get_n_entries (id4), &
         analysis_get_n_entries (id4, within_bounds = .true.)
    print 2, "average   = ", &
         analysis_get_average (id1), &
         analysis_get_average (id2), &
         analysis_get_average (id3), &
         analysis_get_average (id3, within_bounds = .true.)
    print 2, "error     = ", &
         analysis_get_error (id1), &
         analysis_get_error (id2), &
         analysis_get_error (id3), &
         analysis_get_error (id3, within_bounds = .true.)
    print *
    call analysis_write ()
    call analysis_clear ()
  end subroutine analysis_test1


end module analysis
