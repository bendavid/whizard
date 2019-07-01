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

module vamp2

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: pac_fmt
  use format_utils, only: write_separator, write_indent
  use format_defs, only: FMT_12, FMT_14, FMT_17, FMT_19
  use diagnostics
  use rng_base

  use vegas

  implicit none
  private

  public :: vamp2_func_t
  public :: vamp2_config_t
  public :: vamp2_result_t
  public :: vamp2_t

  character(len=*), parameter, private :: &
     descr_fmt =         "(1X,A)", &
     integer_fmt =       "(1X,A18,1X,I15)", &
     integer_array_fmt = "(1X,I18,1X,I15)", &
     logical_fmt =       "(1X,A18,1X,L1)", &
     double_fmt =        "(1X,A18,1X," // FMT_17 // ")", &
     double_array_fmt =  "(1X,I18,1X," // FMT_17 // ")", &
     double_array2_fmt =  "(1X,2(1X,I8),1X," // FMT_17 // ")"

  type, abstract, extends(vegas_func_t) :: vamp2_func_t
     integer :: current_channel = 0
     integer :: n_dim = 0
     integer :: n_channel = 0
     integer :: n_calls = 0
     logical :: valid_x = .false.
     real(default), dimension(:, :), allocatable :: xi
     real(default), dimension(:), allocatable :: det
     real(default), dimension(:), allocatable :: wi
     real(default), dimension(:), allocatable :: gi
     type(vegas_grid_t), dimension(:), allocatable :: grids
     real(default) :: g = 0.
   contains
     procedure, public :: init => vamp2_func_init
     procedure, public :: set_channel => vamp2_func_set_channel
     procedure, public :: get_n_calls => vamp2_func_get_n_calls
     procedure, public :: reset_n_calls => vamp2_func_reset_n_calls
     procedure(vamp2_func_evaluate_maps), deferred :: evaluate_maps
     procedure, private :: evaluate_weight => vamp2_func_evaluate_weight
     procedure(vamp2_func_evaluate_func), deferred :: evaluate_func
     procedure, public :: evaluate => vamp2_func_evaluate
  end type vamp2_func_t

  type, extends(vegas_config_t) :: vamp2_config_t
     integer :: n_channel = 0
     integer :: n_calls_min_per_channel = 20
     integer :: n_calls_threshold = 10
     integer :: n_chains = 0
     logical :: stratified = .true.
     real(default) :: beta = 0.5_default
     real(default) :: accuracy_goal = 0._default
     real(default) :: error_goal = 0._default
     real(default) :: rel_error_goal = 0._default
   contains
     procedure, public :: write => vamp2_config_write
  end type vamp2_config_t

  type, extends(vegas_result_t) :: vamp2_result_t
   contains
     procedure, public :: write => vamp2_result_write
  end type vamp2_result_t

  type :: vamp2_t
     private
     type(vamp2_config_t) :: config
     type(vegas_t), dimension(:), allocatable :: integrator
     integer, dimension(:), allocatable :: chain
     real(default), dimension(:), allocatable :: weight
     real(default), dimension(:), allocatable :: integral
     real(default), dimension(:), allocatable :: variance
     real(default), dimension(:), allocatable :: efficiency
     type(vamp2_result_t) :: result
   contains
     procedure, public :: final => vamp2_final
     procedure, public :: write => vamp2_write
     procedure, public :: get_config => vamp2_get_config
     procedure, public :: set_config => vamp2_set_config
     procedure, public :: set_calls => vamp2_set_n_calls
     procedure, public :: set_limits => vamp2_set_limits
     procedure, public :: set_chain => vamp2_set_chain
     procedure, public :: get_integral => vamp2_get_integral
     procedure, public :: get_variance => vamp2_get_variance
     procedure, public :: get_efficiency => vamp2_get_efficiency
     procedure, private :: adapt_weights => vamp2_adapt_weights
     procedure, public :: reset_result => vamp2_reset_result
     procedure, public :: integrate => vamp2_integrate
     procedure, public :: generate_event => vamp2_generate_event
     procedure, public :: write_grids => vamp2_write_grids
     procedure, public :: read_grids => vamp2_read_grids
  end type vamp2_t


 abstract interface
    subroutine vamp2_func_evaluate_maps (self, x)
      import :: vamp2_func_t, default
      class(vamp2_func_t), intent(inout) :: self
      real(default), dimension(:), intent(in) :: x
    end subroutine vamp2_func_evaluate_maps
 end interface

  abstract interface
     real(default) function vamp2_func_evaluate_func (self, x) result (f)
       import :: vamp2_func_t, default
       class(vamp2_func_t), intent(in) :: self
       real(default), dimension(:), intent(in) :: x
     end function vamp2_func_evaluate_func
  end interface

  interface vamp2_t
     module procedure vamp2_init
  end interface vamp2_t


contains

  subroutine vamp2_func_init (self, n_dim, n_channel)
    class(vamp2_func_t), intent(out) :: self
    integer, intent(in) :: n_dim
    integer, intent(in) :: n_channel
    self%n_dim = n_dim
    self%n_channel = n_channel
    allocate (self%xi(n_dim, n_channel), source=0._default)
    allocate (self%det(n_channel), source=1._default)
    allocate (self%wi(n_channel), source=0._default)
    allocate (self%gi(n_channel), source=0._default)
    allocate (self%grids(n_channel))
  end subroutine vamp2_func_init

  subroutine vamp2_func_set_channel (self, channel)
    class(vamp2_func_t), intent(inout) :: self
    integer, intent(in) :: channel
    self%current_channel = channel
  end subroutine vamp2_func_set_channel

  integer function vamp2_func_get_n_calls (self) result (n_calls)
    class(vamp2_func_t), intent(in) :: self
    n_calls = self%n_calls
  end function vamp2_func_get_n_calls

  subroutine vamp2_func_reset_n_calls (self)
    class(vamp2_func_t), intent(inout) :: self
    self%n_calls = 0
  end subroutine vamp2_func_reset_n_calls

  subroutine vamp2_func_evaluate_weight (self)
    class(vamp2_func_t), intent(inout) :: self
    integer :: ch
    self%g = 0.
    self%gi = 0.
    do ch = 1, self%n_channel
       if (self%wi(ch) /= 0) then
          self%gi(ch) = self%grids(ch)%get_probability (self%xi(:, ch))
       end if
    end do
    if (self%gi(self%current_channel) /= 0) then
       do ch = 1, self%n_channel
          if (self%wi(ch) /= 0 .and. self%det(ch) /= 0) then
             self%g = self%g + self%wi(ch) * self%gi(ch) / self%det(ch)
          end if
       end do
       self%g = self%g / self%gi(self%current_channel)
    end if
  end subroutine vamp2_func_evaluate_weight

  real(default) function vamp2_func_evaluate (self, x) result (f)
    class(vamp2_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    call self%evaluate_maps (x)
    f = 0.
    self%gi = 0.
    self%g = 1
    if (self%valid_x) then
       call self%evaluate_weight ()
       f = self%evaluate_func (x) / self%g
    end if
    self%n_calls = self%n_calls + 1
  end function vamp2_func_evaluate

  subroutine vamp2_config_write (self, unit, indent)
    class(vamp2_config_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call self%vegas_config_t%write (unit, indent)
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of channels                               = ", self%n_channel
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Min. number of calls per channel (setting calls) = ", &
         & self%n_calls_min_per_channel
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Threshold number of calls (adapting weights)     = ", &
         & self%n_calls_threshold
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of chains                                 = ", self%n_chains
    call write_indent (u, ind)
    write (u, "(2x,A,L1)") &
         & "Stratified                                       = ", self%stratified
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Adaption power (beta)                            = ", self%beta
    if (self%accuracy_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "accuracy_goal                                 = ", self%accuracy_goal
    end if
    if (self%error_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "error_goal                                    = ", self%error_goal
    end if
    if (self%rel_error_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "rel_error_goal                                = ", self%rel_error_goal
    end if
  end subroutine vamp2_config_write

  subroutine vamp2_result_write (self, unit, indent)
    class(vamp2_result_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call self%vegas_result_t%write (unit, indent)
  end subroutine vamp2_result_write

  type(vamp2_t) function vamp2_init (n_channel, n_dim, alpha, beta, n_bins_max,&
       & n_calls_min_per_channel, iterations, mode) result (self)
    integer, intent(in) :: n_channel
    integer, intent(in) :: n_dim
    integer, intent(in), optional :: n_bins_max
    integer, intent(in), optional :: n_calls_min_per_channel
    real(default), intent(in), optional :: alpha
    real(default), intent(in), optional :: beta
    integer, intent(in), optional :: iterations
    integer, intent(in), optional :: mode
    integer :: ch
    self%config%n_dim = n_dim
    self%config%n_channel = n_channel
    if (present (n_bins_max)) self%config%n_bins_max = n_bins_max
    if (present (n_calls_min_per_channel)) self%config%n_calls_min_per_channel = n_calls_min_per_channel
    if (present (alpha)) self%config%alpha = alpha
    if (present (beta)) self%config%beta = beta
    if (present (iterations)) self%config%iterations = iterations
    if (present (mode)) self%config%mode = mode
    allocate (self%chain(n_channel), source=0)
    allocate (self%integrator(n_channel))
    allocate (self%weight(n_channel), source=0._default)
    do ch = 1, n_channel
       self%integrator(ch) = vegas_t (n_dim, alpha, n_bins_max, 1, mode)
    end do
    self%weight = 1. / self%config%n_channel
    call self%reset_result ()
  end function vamp2_init

  subroutine vamp2_final (self)
    class(vamp2_t), intent(inout) :: self
    integer :: ch
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%final ()
    end do
  end subroutine vamp2_final

  subroutine vamp2_write (self, unit, indent)
    class(vamp2_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind, ch
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(A)") "VAMP2: VEGAS AMPlified 2"
    call write_indent (u, ind)
    call self%config%write (unit, indent)
    call self%result%write (unit, indent)
  end subroutine vamp2_write

  subroutine vamp2_get_config (self, config)
    class(vamp2_t), intent(in) :: self
    type(vamp2_config_t), intent(out) :: config
    config = self%config
  end subroutine vamp2_get_config

  subroutine vamp2_set_config (self, config)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_config_t), intent(in) :: config
    integer :: ch
    self%config%n_calls_min_per_channel = config%n_calls_min_per_channel
    self%config%n_calls_threshold = config%n_calls_threshold
    self%config%n_calls_min = config%n_calls_min
    self%config%beta = config%beta
    self%config%accuracy_goal = config%accuracy_goal
    self%config%error_goal = config%error_goal
    self%config%rel_error_goal = config%rel_error_goal
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_config (config)
    end do
  end subroutine vamp2_set_config

  subroutine vamp2_set_n_calls (self, n_calls)
    class(vamp2_t), intent(inout) :: self
    integer, intent(in) :: n_calls
    integer :: ch
    self%config%n_calls_min = self%config%n_calls_min_per_channel &
         & * self%config%n_channel
    self%config%n_calls = max(n_calls, self%config%n_calls_min)
    if (self%config%n_calls > n_calls) then
       write (msg_buffer, "(A,I0)") "VAMP2: [set_calls] number of calls too few,&
            & reset to = ", self%config%n_calls
       call msg_message ()
    end if
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_calls (max (nint (self%config%n_calls *&
            & self%weight(ch)), self%config%n_calls_min_per_channel))
    end do
  end subroutine vamp2_set_n_calls

  subroutine vamp2_set_limits (self, x_upper, x_lower)
    class(vamp2_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x_upper
    real(default), dimension(:), intent(in) :: x_lower
    integer :: ch
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_limits (x_upper, x_lower)
    end do
  end subroutine vamp2_set_limits

  subroutine vamp2_set_chain (self, n_chains, chain)
    class(vamp2_t), intent(inout) :: self
    integer, intent(in) :: n_chains
    integer, dimension(:), intent(in) :: chain
    if (size (chain) /= self%config%n_channel) then
       call msg_bug ("[VAMP2] set chain: size of chain array does not match n_channel.")
    else
       call msg_message ("[VAMP2] set chain: use chained weights.")
    end if
    self%config%n_chains = n_chains
    self%chain = chain
  end subroutine vamp2_set_chain

  elemental real(default) function vamp2_get_integral (self) result (integral)
    class(vamp2_t), intent(in) :: self
    integral = 0.
    if (self%result%sum_wgts > 0.) then
       integral = self%result%sum_int_wgtd / self%result%sum_wgts
    end if
  end function vamp2_get_integral

  elemental real(default) function vamp2_get_variance (self) result (variance)
    class(vamp2_t), intent(in) :: self
    variance = 0.
    if (self%result%sum_wgts > 0.) then
       variance = 1.0 / self%result%sum_wgts
    end if
  end function vamp2_get_variance

  elemental real(default) function vamp2_get_efficiency (self) result (efficiency)
    class(vamp2_t), intent(in) :: self
    efficiency = 0.
    if (self%result%efficiency > 0.) then
       efficiency = self%result%efficiency
    end if
  end function vamp2_get_efficiency
  subroutine vamp2_adapt_weights (self)
    class(vamp2_t), intent(inout) :: self
    integer :: n_weights_underflow
    real(default) :: weight_min, sum_weights_underflow
    self%weight = self%weight * self%integrator%get_variance ()**self%config%beta
    if (sum (self%weight) == 0) self%weight = real(self%config%n_calls, default)
    if (self%config%n_chains > 0) then
       call chain_weights ()
    end if
    self%weight = self%weight / sum(self%weight)
    if (self%config%n_calls_threshold /= 0) then
       weight_min = real(self%config%n_calls_threshold, default) &
            & / self%config%n_calls
       sum_weights_underflow = sum (self%weight, self%weight < weight_min)
       n_weights_underflow = count (self%weight < weight_min)
       where (self%weight < weight_min)
          self%weight = weight_min
       elsewhere
          self%weight = self%weight * (1. - n_weights_underflow * weight_min) &
               & / (1. - sum_weights_underflow)
       end where
    end if
    call self%set_calls (self%config%n_calls)
  contains
    subroutine chain_weights ()
      integer :: ch
      real(default) :: average
      do ch = 1, self%config%n_chains
         average = max (sum (self%weight, self%chain == ch), 0._default)
         if (average /= 0) then
            average = average / count (self%chain == ch)
            where (self%chain == ch)
               self%weight = average
            end where
         end if
      end do
    end subroutine chain_weights

  end subroutine vamp2_adapt_weights

  subroutine vamp2_reset_result (self)
    class(vamp2_t), intent(inout) :: self
    self%result%sum_int_wgtd = 0.
    self%result%sum_wgts = 0.
    self%result%sum_chi = 0.
    self%result%it_num = 0
    self%result%samples = 0
    self%result%chi2 = 0
    self%result%efficiency = 0.
  end subroutine vamp2_reset_result

  subroutine vamp2_integrate (self, func, rng, iterations, opt_reset_result,&
       & opt_refine_grid, opt_adapt_weight, opt_verbose, result, abserr)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    integer, intent(in), optional :: iterations
    logical, intent(in), optional :: opt_reset_result
    logical, intent(in), optional :: opt_refine_grid
    logical, intent(in), optional :: opt_adapt_weight
    logical, intent(in), optional :: opt_verbose
    real(default), optional, intent(out) :: result, abserr
    integer :: it, ch
    real(default) :: total_integral, total_sq_integral, total_variance, chi, wgt
    real(default) :: cumulative_int, cumulative_std
    logical :: reset_result = .true.
    logical :: adapt_weight = .true.
    logical :: refine_grid = .true.
    logical :: verbose = .false.
    if (present (iterations)) self%config%iterations = iterations
    if (present (opt_reset_result)) reset_result = opt_reset_result
    if (present (opt_adapt_weight)) adapt_weight = opt_adapt_weight
    if (present (opt_refine_grid)) refine_grid = opt_refine_grid
    if (present (opt_verbose)) verbose = opt_verbose
    cumulative_int = 0.
    cumulative_std = 0.
    if (reset_result) call self%reset_result
    if (verbose) then
       call msg_message ("Results: [it, calls, integral, error, chi^2, eff.]")
    end if
    iteration: do it = 1, self%config%iterations
       total_integral = 0._default
       total_sq_integral = 0._default
       total_variance = 0._default
       do ch = 1, self%config%n_channel
          func%wi(ch) = self%weight(ch)
          func%grids(ch) = self%integrator(ch)%get_grid ()
       end do
       channel: do ch = 1, self%config%n_channel
          ! Oh, do me! Integrate me! Integrate me, soooo hard!
          call func%set_channel (ch)
          call self%integrator(ch)%integrate ( &
               & func, rng, iterations, opt_refine_grid = .false., opt_verbose = verbose)
          total_integral = total_integral &
               & + self%weight(ch) * self%integrator(ch)%get_integral ()
          total_sq_integral = total_sq_integral &
               & + self%weight(ch) * self%integrator(ch)%get_integral ()**2
          total_variance = total_variance &
               & + self%weight(ch)**2 * self%config%n_calls * self%integrator(ch)%get_variance ()
       end do channel
       associate (result => self%result)
         ! a**2 - b**2 = (a - b) * (a + b)
         total_variance = sqrt (total_variance + total_sq_integral)
         total_variance = 1. / self%config%n_calls * &
              & (total_variance + total_integral) * (total_variance - total_integral)
         ! Ensure variance is always positive and larger than zero
         if (total_variance < tiny (1._default) / epsilon (1._default) * max (total_integral**2, 1._default)) then
            total_variance = tiny (1._default) / epsilon (1._default) * max (total_integral**2, 1._default)
         end if
         wgt = 1. / total_variance
         result%result = total_integral
         result%std = sqrt (total_variance)
         result%samples = result%samples + 1
         if (result%samples == 1) then
            result%chi2 = 0._default
         else
            chi = total_integral
            if (result%sum_wgts > 0) chi = chi - result%sum_int_wgtd / result%sum_wgts
            result%chi2 = result%chi2 * (result%samples - 2.0_default)
            result%chi2 = (wgt / (1._default + (wgt / result%sum_wgts))) &
                 & * chi**2
            result%chi2 = result%chi2 / (result%samples - 1._default)
         end if
         result%sum_wgts = result%sum_wgts + wgt
         result%sum_int_wgtd = result%sum_int_wgtd + (total_integral * wgt)
         result%sum_chi = result%sum_chi + (total_sq_integral * wgt)
         result%max_abs_f = dot_product (self%weight * self%config%n_calls, &
              & self%integrator%get_max_abs_f ())
         result%max_abs_f_pos = dot_product (self%weight * self%config%n_calls, &
              & self%integrator%get_max_abs_f_pos ())
         result%max_abs_f_neg = dot_product (self%weight * self%config%n_calls, &
              & self%integrator%get_max_abs_f_neg ())
         result%efficiency = 0.
         if (result%max_abs_f > 0.) then
            result%efficiency = dot_product (self%weight, &
                 & (self%integrator%get_efficiency () * self%weight &
                 & * self%config%n_calls * self%integrator%get_max_abs_f ())) &
                 & / result%max_abs_f
            ! TODO pos. or. negative efficiency would be very nice.
         end if
         cumulative_int = result%sum_int_wgtd / result%sum_wgts
         cumulative_std = sqrt (1. / result%sum_wgts)
         if (verbose) then
            write (msg_buffer, "(I0,1x,I0,1x, 4(" // FMT_17 // ",1x))") &
                 & it, self%config%n_calls, cumulative_int, cumulative_std, &
                 & self%result%chi2, self%result%efficiency
            call msg_message ()
         end if
       end associate
       if (adapt_weight) then
          call self%adapt_weights ()
       end if
       if (refine_grid) then
          do ch = 1, self%config%n_channel
             call self%integrator(ch)%refine ()
          end do
       end if
    end do iteration
    if (present (result)) result = cumulative_int
    if (present (abserr)) abserr = abs (cumulative_std)
  end subroutine vamp2_integrate

  subroutine vamp2_generate_event (self, func, rng, x, opt_event_weight, opt_event_excess)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out)  :: x
    real(default), intent(out), optional :: opt_event_weight
    real(default), intent(out), optional :: opt_event_excess
    integer :: ch, i
    real(default) :: r, event_weight, max_abs_f
    real(default), dimension(self%config%n_channel) :: weight
    if (any (self%integrator%get_max_abs_f () > 0)) then
       weight = self%weight * self%integrator%get_max_abs_f ()
    else
       weight = self%weight
    end if
    weight = weight / sum (weight)
    call rng%generate (r)
    nchannel: do ch = 1, self%config%n_channel
       r = r - weight(ch)
       if (r <= 0._default) exit nchannel
    end do nchannel
    ch = min (ch, self%config%n_channel)
    call func%set_channel (ch)
    ! TODO move to a separat procedure and let this be done at initialisation
    do i = 1, self%config%n_channel
       func%wi(i) = self%weight(i)
       func%grids(i) = self%integrator(i)%get_grid ()
    end do
    if (present (opt_event_excess)) opt_event_excess = 0
    generate: do
       call self%integrator(ch)%generate_event (func, rng, x, event_weight)
       if (present (opt_event_weight)) then
          opt_event_weight = event_weight * self%weight(ch) / weight(ch)
          exit generate
       end if
       if (event_weight > 0.) then
          if (abs (event_weight) > self%integrator(ch)%get_max_abs_f_pos ()) then
             if (present (opt_event_excess)) then
                opt_event_excess = event_weight / self%integrator(ch)%get_max_abs_f_pos () - 1._default
             else
                write (msg_buffer, "(A,1X," // FMT_17 // ",A)") "[VAMP2] Event&
                     & generation: weight > 1 (", self%result%max_abs_f_pos, ")"
                call msg_warning ()
             end if
          end if
          max_abs_f = self%integrator(ch)%get_max_abs_f_pos ()
       else
          if (abs (event_weight) > self%integrator(ch)%get_max_abs_f_neg ()) then
             if (present (opt_event_excess)) then
                opt_event_excess = event_weight / self%integrator(ch)%get_max_abs_f_neg () - 1._default
             else
                write (msg_buffer, "(A,1X," // FMT_17 // ",A)") "[VAMP2] Event&
                     & generation: weight > 1 (", self%result%max_abs_f_neg, ")"
                call msg_warning ()
             end if
          end if
          max_abs_f = self%integrator(ch)%get_max_abs_f_neg ()
       end if
       call rng%generate (r)
       ! Do not use division, because max_abs_f could be zero.
       if (max_abs_f * r <= abs(event_weight)) then
          exit generate
       end if
    end do generate
  end subroutine vamp2_generate_event

  subroutine vamp2_write_grids (self, unit)
    class(vamp2_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer :: u
    integer :: ch
    u = given_output_unit (unit)
    write (u, descr_fmt) "begin type(vamp2_t)"
    write (u, integer_fmt) "n_channel =", self%config%n_channel
    write (u, integer_fmt) "n_dim =", self%config%n_dim
    write (u, integer_fmt) "n_calls_min_ch =", self%config%n_calls_min_per_channel
    write (u, integer_fmt) "n_calls_thres =", self%config%n_calls_threshold
    write (u, integer_fmt) "n_chains =", self%config%n_chains
    write (u, logical_fmt) "stratified =", self%config%stratified
    write (u, double_fmt) "alpha =", self%config%alpha
    write (u, double_fmt) "beta =", self%config%beta
    write (u, integer_fmt) "n_bins_max =", self%config%n_bins_max
    write (u, integer_fmt) "iterations =", self%config%iterations
    write (u, integer_fmt) "n_calls =", self%config%n_calls
    write (u, integer_fmt) "it_start =", self%result%it_start
    write (u, integer_fmt) "it_num =", self%result%it_num
    write (u, integer_fmt) "samples =", self%result%samples
    write (u, double_fmt) "sum_int_wgtd =", self%result%sum_int_wgtd
    write (u, double_fmt) "sum_wgts =", self%result%sum_wgts
    write (u, double_fmt) "sum_chi =", self%result%sum_chi
    write (u, double_fmt) "chi2 =", self%result%chi2
    write (u, double_fmt) "efficiency =", self%result%efficiency
    write (u, double_fmt) "efficiency =", self%result%efficiency_pos
    write (u, double_fmt) "efficiency =", self%result%efficiency_neg
    write (u, double_fmt) "max_abs_f =", self%result%max_abs_f
    write (u, double_fmt) "max_abs_f_pos =", self%result%max_abs_f_pos
    write (u, double_fmt) "max_abs_f_neg =", self%result%max_abs_f_neg
    write (u, double_fmt) "result =", self%result%result
    write (u, double_fmt) "std =", self%result%std
    write (u, descr_fmt) "begin weight"
    do ch = 1, self%config%n_channel
       write (u, double_array_fmt) ch, self%weight(ch)
    end do
    write (u, descr_fmt) "end weight"
    if (self%config%n_chains > 0) then
       write (u, descr_fmt) "begin chain"
       do ch = 1, self%config%n_channel
          write (u, integer_array_fmt) ch, self%chain(ch)
       end do
       write (u, descr_fmt) "end chain"
    end if
    write (u, descr_fmt) "begin integrator"
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%write_grid (unit)
    end do
    write (u, descr_fmt) "end integrator"
    write (u, descr_fmt) "end type(vamp2_t)"
  end subroutine vamp2_write_grids

  subroutine vamp2_read_grids (self, unit)
    class(vamp2_t), intent(out) :: self
    integer, intent(in), optional :: unit
    integer :: u
    integer :: ibuffer, jbuffer, ch
    character(len=80) :: buffer
    read (unit, descr_fmt) buffer
    read (unit, integer_fmt) buffer, ibuffer
    read (unit, integer_fmt) buffer, jbuffer
    select type (self)
    type is (vamp2_t)
       self = vamp2_t (n_channel = ibuffer, n_dim = jbuffer)
    end select
    read (unit, integer_fmt) buffer, self%config%n_calls_min_per_channel
    read (unit, integer_fmt) buffer, self%config%n_calls_threshold
    read (unit, integer_fmt) buffer, self%config%n_chains
    read (unit, logical_fmt) buffer, self%config%stratified
    read (unit, double_fmt) buffer, self%config%alpha
    read (unit, double_fmt) buffer, self%config%beta
    read (unit, integer_fmt) buffer, self%config%n_bins_max
    read (unit, integer_fmt) buffer, self%config%iterations
    read (unit, integer_fmt) buffer, self%config%n_calls
    read (unit, integer_fmt) buffer, self%result%it_start
    read (unit, integer_fmt) buffer, self%result%it_num
    read (unit, integer_fmt) buffer, self%result%samples
    read (unit, double_fmt) buffer, self%result%sum_int_wgtd
    read (unit, double_fmt) buffer, self%result%sum_wgts
    read (unit, double_fmt) buffer, self%result%sum_chi
    read (unit, double_fmt) buffer, self%result%chi2
    read (unit, double_fmt) buffer, self%result%efficiency
    read (unit, double_fmt) buffer, self%result%efficiency_pos
    read (unit, double_fmt) buffer, self%result%efficiency_neg
    read (unit, double_fmt) buffer, self%result%max_abs_f
    read (unit, double_fmt) buffer, self%result%max_abs_f_pos
    read (unit, double_fmt) buffer, self%result%max_abs_f_neg
    read (unit, double_fmt) buffer, self%result%result
    read (unit, double_fmt) buffer, self%result%std
    read (unit, descr_fmt) buffer
    do ch = 1, self%config%n_channel
       read (unit, double_array_fmt) ibuffer, self%weight(ch)
    end do
    read (unit, descr_fmt) buffer
    if (self%config%n_chains > 0) then
       read (unit, descr_fmt) buffer
       do ch = 1, self%config%n_channel
          read (unit, integer_array_fmt) ibuffer, self%chain(ch)
       end do
       read (unit, descr_fmt) buffer
    end if
    read (unit, descr_fmt) buffer
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%read_grid (unit)
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
  end subroutine vamp2_read_grids


end module vamp2
