! WHIZARD 2.6.0 Sep 08 2017
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

module event_base

  use kinds, only: default
  use kinds, only: i64
  use iso_varying_string, string_t => varying_string
  use io_units
  use string_utils, only: lower_case
  use diagnostics
  use model_data
  use particles

  implicit none
  private

  public :: generic_event_t
  public :: event_normalization_mode
  public :: event_normalization_string
  public :: event_normalization_update
  public :: event_callback_t
  public :: event_callback_nop_t

  integer, parameter, public :: NORM_UNDEFINED = 0
  integer, parameter, public :: NORM_UNIT = 1
  integer, parameter, public :: NORM_N_EVT = 2
  integer, parameter, public :: NORM_SIGMA = 3
  integer, parameter, public :: NORM_S_N = 4


  type, abstract :: generic_event_t
     !private
     logical :: particle_set_is_valid = .false.
     type(particle_set_t), pointer :: particle_set => null ()
     logical :: sqme_ref_known = .false.
     real(default) :: sqme_ref = 0
     logical :: sqme_prc_known = .false.
     real(default) :: sqme_prc = 0
     logical :: weight_ref_known = .false.
     real(default) :: weight_ref = 0
     logical :: weight_prc_known = .false.
     real(default) :: weight_prc = 0
     logical :: excess_prc_known = .false.
     real(default) :: excess_prc = 0
     integer :: n_alt = 0
     logical :: sqme_alt_known = .false.
     real(default), dimension(:), allocatable :: sqme_alt
     logical :: weight_alt_known = .false.
     real(default), dimension(:), allocatable :: weight_alt
   contains
     procedure :: base_init => generic_event_init
     procedure :: has_valid_particle_set => generic_event_has_valid_particle_set
     procedure :: accept_particle_set => generic_event_accept_particle_set
     procedure :: discard_particle_set => generic_event_discard_particle_set
     procedure :: get_particle_set_ptr => generic_event_get_particle_set_ptr
     procedure :: link_particle_set => generic_event_link_particle_set
     procedure :: sqme_prc_is_known => generic_event_sqme_prc_is_known
     procedure :: sqme_ref_is_known => generic_event_sqme_ref_is_known
     procedure :: sqme_alt_is_known => generic_event_sqme_alt_is_known
     procedure :: weight_prc_is_known => generic_event_weight_prc_is_known
     procedure :: weight_ref_is_known => generic_event_weight_ref_is_known
     procedure :: weight_alt_is_known => generic_event_weight_alt_is_known
     procedure :: excess_prc_is_known => generic_event_excess_prc_is_known
     procedure :: get_n_alt => generic_event_get_n_alt
     procedure :: get_sqme_prc => generic_event_get_sqme_prc
     procedure :: get_sqme_ref => generic_event_get_sqme_ref
     generic :: get_sqme_alt => &
          generic_event_get_sqme_alt_0, generic_event_get_sqme_alt_1
     procedure :: generic_event_get_sqme_alt_0
     procedure :: generic_event_get_sqme_alt_1
     procedure :: get_weight_prc => generic_event_get_weight_prc
     procedure :: get_weight_ref => generic_event_get_weight_ref
     generic :: get_weight_alt => &
          generic_event_get_weight_alt_0, generic_event_get_weight_alt_1
     procedure :: generic_event_get_weight_alt_0
     procedure :: generic_event_get_weight_alt_1
     procedure :: get_excess_prc => generic_event_get_excess_prc
     procedure :: set_sqme_prc => generic_event_set_sqme_prc
     procedure :: set_sqme_ref => generic_event_set_sqme_ref
     procedure :: set_sqme_alt => generic_event_set_sqme_alt
     procedure :: set_weight_prc => generic_event_set_weight_prc
     procedure :: set_weight_ref => generic_event_set_weight_ref
     procedure :: set_weight_alt => generic_event_set_weight_alt
     procedure :: set_excess_prc => generic_event_set_excess_prc
     procedure :: set => generic_event_set
     procedure (generic_event_write), deferred :: write
     procedure (generic_event_generate), deferred :: generate
     procedure (generic_event_set_hard_particle_set), deferred :: &
          set_hard_particle_set
     procedure (generic_event_handler), deferred :: evaluate_expressions
     procedure (generic_event_select), deferred :: select
     procedure (generic_event_get_model_ptr), deferred :: get_model_ptr
     procedure (generic_event_get_index), deferred :: get_index
     procedure (generic_event_get_fac_scale), deferred :: get_fac_scale
     procedure (generic_event_get_alpha_s), deferred :: get_alpha_s
     procedure (generic_event_get_sqrts), deferred :: get_sqrts
     procedure (generic_event_get_polarization), deferred :: get_polarization
     procedure (generic_event_get_beam_file), deferred :: get_beam_file
     procedure (generic_event_get_process_name), deferred :: &
          get_process_name
     procedure (generic_event_set_alpha_qcd_forced), deferred :: &
          set_alpha_qcd_forced
     procedure (generic_event_set_scale_forced), deferred :: &
          set_scale_forced
     procedure :: reset => generic_event_reset
     procedure :: base_reset => generic_event_reset
     procedure :: pacify_particle_set => generic_event_pacify_particle_set
  end type generic_event_t

  type, abstract :: event_callback_t
     private
   contains
     procedure(event_callback_write), deferred :: write
     procedure(event_callback_proc), deferred :: proc
  end type event_callback_t

  type, extends (event_callback_t) :: event_callback_nop_t
     private
   contains
     procedure :: write => event_callback_nop_write
     procedure :: proc => event_callback_nop
  end type event_callback_nop_t


  abstract interface
     subroutine generic_event_write (object, unit, &
          show_process, show_transforms, &
          show_decay, verbose, testflag)
       import
       class(generic_event_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: show_process
       logical, intent(in), optional :: show_transforms
       logical, intent(in), optional :: show_decay
       logical, intent(in), optional :: verbose
       logical, intent(in), optional :: testflag
     end subroutine generic_event_write
  end interface

  abstract interface
     subroutine generic_event_generate (event, i_mci, r, i_nlo)
       import
       class(generic_event_t), intent(inout) :: event
       integer, intent(in) :: i_mci
       real(default), dimension(:), intent(in), optional :: r
       integer, intent(in), optional :: i_nlo
     end subroutine generic_event_generate
  end interface

  abstract interface
     subroutine generic_event_set_hard_particle_set (event, particle_set)
       import
       class(generic_event_t), intent(inout) :: event
       type(particle_set_t), intent(in) :: particle_set
     end subroutine generic_event_set_hard_particle_set
  end interface

  abstract interface
     subroutine generic_event_handler (event)
       import
       class(generic_event_t), intent(inout) :: event
     end subroutine generic_event_handler
  end interface

  abstract interface
     subroutine generic_event_select (event,  i_mci, i_term, channel)
       import
       class(generic_event_t), intent(inout) :: event
       integer, intent(in) :: i_mci, i_term, channel
     end subroutine generic_event_select
  end interface

  abstract interface
     function generic_event_get_model_ptr (event) result (model)
       import
       class(generic_event_t), intent(in) :: event
       class(model_data_t), pointer :: model
     end function generic_event_get_model_ptr
  end interface

  abstract interface
     function generic_event_get_index (event) result (index)
       import
       class(generic_event_t), intent(in) :: event
       integer :: index
     end function generic_event_get_index
  end interface

  abstract interface
     function generic_event_get_fac_scale (event) result (fac_scale)
       import
       class(generic_event_t), intent(in) :: event
       real(default) :: fac_scale
     end function generic_event_get_fac_scale
  end interface

  abstract interface
     function generic_event_get_alpha_s (event) result (alpha_s)
       import
       class(generic_event_t), intent(in) :: event
       real(default) :: alpha_s
     end function generic_event_get_alpha_s
  end interface

  abstract interface
     function generic_event_get_sqrts (event) result (sqrts)
       import
       class(generic_event_t), intent(in) :: event
       real(default) :: sqrts
     end function generic_event_get_sqrts
  end interface

  abstract interface
     function generic_event_get_polarization (event) result (pol)
       import
       class(generic_event_t), intent(in) :: event
       real(default), dimension(2) :: pol
     end function generic_event_get_polarization
  end interface

  abstract interface
     function generic_event_get_beam_file (event) result (file)
       import
       class(generic_event_t), intent(in) :: event
       type(string_t) :: file
     end function generic_event_get_beam_file
  end interface

  abstract interface
     function generic_event_get_process_name (event) result (name)
       import
       class(generic_event_t), intent(in) :: event
       type(string_t) :: name
     end function generic_event_get_process_name
  end interface

  abstract interface
     subroutine generic_event_set_alpha_qcd_forced (event, alpha_qcd)
       import
       class(generic_event_t), intent(inout) :: event
       real(default), intent(in) :: alpha_qcd
     end subroutine generic_event_set_alpha_qcd_forced
  end interface

  abstract interface
     subroutine generic_event_set_scale_forced (event, scale)
       import
       class(generic_event_t), intent(inout) :: event
       real(default), intent(in) :: scale
     end subroutine generic_event_set_scale_forced
  end interface

  abstract interface
     subroutine event_callback_write (event_callback, unit)
       import
       class(event_callback_t), intent(in) :: event_callback
       integer, intent(in), optional :: unit
     end subroutine event_callback_write
  end interface

  abstract interface
     subroutine event_callback_proc (event_callback, i, event)
       import
       class(event_callback_t), intent(in) :: event_callback
       integer(i64), intent(in) :: i
       class(generic_event_t), intent(in) :: event
     end subroutine event_callback_proc
  end interface


contains

  subroutine generic_event_init (event, n_alt)
    class(generic_event_t), intent(out) :: event
    integer, intent(in) :: n_alt
    event%n_alt = n_alt
    allocate (event%sqme_alt (n_alt))
    allocate (event%weight_alt (n_alt))
  end subroutine generic_event_init

  function generic_event_has_valid_particle_set (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%particle_set_is_valid
  end function generic_event_has_valid_particle_set

  subroutine generic_event_accept_particle_set (event)
    class(generic_event_t), intent(inout) :: event
    event%particle_set_is_valid = .true.
  end subroutine generic_event_accept_particle_set

  subroutine generic_event_discard_particle_set (event)
    class(generic_event_t), intent(inout) :: event
    event%particle_set_is_valid = .false.
  end subroutine generic_event_discard_particle_set

  function generic_event_get_particle_set_ptr (event) result (ptr)
    class(generic_event_t), intent(in) :: event
    type(particle_set_t), pointer :: ptr
    ptr => event%particle_set
  end function generic_event_get_particle_set_ptr

  subroutine generic_event_link_particle_set (event, particle_set)
    class(generic_event_t), intent(inout) :: event
    type(particle_set_t), intent(in), target :: particle_set
    event%particle_set => particle_set
    call event%accept_particle_set ()
  end subroutine generic_event_link_particle_set

  function generic_event_sqme_prc_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%sqme_prc_known
  end function generic_event_sqme_prc_is_known

  function generic_event_sqme_ref_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%sqme_ref_known
  end function generic_event_sqme_ref_is_known

  function generic_event_sqme_alt_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%sqme_alt_known
  end function generic_event_sqme_alt_is_known

  function generic_event_weight_prc_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%weight_prc_known
  end function generic_event_weight_prc_is_known

  function generic_event_weight_ref_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%weight_ref_known
  end function generic_event_weight_ref_is_known

  function generic_event_weight_alt_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%weight_alt_known
  end function generic_event_weight_alt_is_known

  function generic_event_excess_prc_is_known (event) result (flag)
    class(generic_event_t), intent(in) :: event
    logical :: flag
    flag = event%excess_prc_known
  end function generic_event_excess_prc_is_known

  function generic_event_get_n_alt (event) result (n)
    class(generic_event_t), intent(in) :: event
    integer :: n
    n = event%n_alt
  end function generic_event_get_n_alt

  function generic_event_get_sqme_prc (event) result (sqme)
    class(generic_event_t), intent(in) :: event
    real(default) :: sqme
    if (event%sqme_prc_known) then
       sqme = event%sqme_prc
    else
       sqme = 0
    end if
  end function generic_event_get_sqme_prc

  function generic_event_get_sqme_ref (event) result (sqme)
    class(generic_event_t), intent(in) :: event
    real(default) :: sqme
    if (event%sqme_ref_known) then
       sqme = event%sqme_ref
    else
       sqme = 0
    end if
  end function generic_event_get_sqme_ref

  function generic_event_get_sqme_alt_0 (event, i) result (sqme)
    class(generic_event_t), intent(in) :: event
    integer, intent(in) :: i
    real(default) :: sqme
    if (event%sqme_alt_known) then
       sqme = event%sqme_alt(i)
    else
       sqme = 0
    end if
  end function generic_event_get_sqme_alt_0

  function generic_event_get_sqme_alt_1 (event) result (sqme)
    class(generic_event_t), intent(in) :: event
    real(default), dimension(event%n_alt) :: sqme
    sqme = event%sqme_alt
  end function generic_event_get_sqme_alt_1

  function generic_event_get_weight_prc (event) result (weight)
    class(generic_event_t), intent(in) :: event
    real(default) :: weight
    if (event%weight_prc_known) then
       weight = event%weight_prc
    else
       weight = 0
    end if
  end function generic_event_get_weight_prc

  function generic_event_get_weight_ref (event) result (weight)
    class(generic_event_t), intent(in) :: event
    real(default) :: weight
    if (event%weight_ref_known) then
       weight = event%weight_ref
    else
       weight = 0
    end if
  end function generic_event_get_weight_ref

  function generic_event_get_weight_alt_0 (event, i) result (weight)
    class(generic_event_t), intent(in) :: event
    integer, intent(in) :: i
    real(default) :: weight
    if (event%weight_alt_known) then
       weight = event%weight_alt(i)
    else
       weight = 0
    end if
  end function generic_event_get_weight_alt_0

  function generic_event_get_weight_alt_1 (event) result (weight)
    class(generic_event_t), intent(in) :: event
    real(default), dimension(event%n_alt) :: weight
    weight = event%weight_alt
  end function generic_event_get_weight_alt_1

  function generic_event_get_excess_prc (event) result (excess)
    class(generic_event_t), intent(in) :: event
    real(default) :: excess
    if (event%excess_prc_known) then
       excess = event%excess_prc
    else
       excess = 0
    end if
  end function generic_event_get_excess_prc

  subroutine generic_event_set_sqme_prc (event, sqme)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in) :: sqme
    event%sqme_prc = sqme
    event%sqme_prc_known = .true.
  end subroutine generic_event_set_sqme_prc

  subroutine generic_event_set_sqme_ref (event, sqme)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in) :: sqme
    event%sqme_ref = sqme
    event%sqme_ref_known = .true.
  end subroutine generic_event_set_sqme_ref

  subroutine generic_event_set_sqme_alt (event, sqme)
    class(generic_event_t), intent(inout) :: event
    real(default), dimension(:), intent(in) :: sqme
    event%sqme_alt = sqme
    event%sqme_alt_known = .true.
  end subroutine generic_event_set_sqme_alt

  subroutine generic_event_set_weight_prc (event, weight)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in) :: weight
    event%weight_prc = weight
    event%weight_prc_known = .true.
  end subroutine generic_event_set_weight_prc

  subroutine generic_event_set_weight_ref (event, weight)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in) :: weight
    event%weight_ref = weight
    event%weight_ref_known = .true.
  end subroutine generic_event_set_weight_ref

  subroutine generic_event_set_weight_alt (event, weight)
    class(generic_event_t), intent(inout) :: event
    real(default), dimension(:), intent(in) :: weight
    event%weight_alt = weight
    event%weight_alt_known = .true.
  end subroutine generic_event_set_weight_alt

  subroutine generic_event_set_excess_prc (event, excess)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in) :: excess
    event%excess_prc = excess
    event%excess_prc_known = .true.
  end subroutine generic_event_set_excess_prc

  subroutine generic_event_set (event, &
       weight_ref, weight_prc, weight_alt, &
       excess_prc, &
       sqme_ref, sqme_prc, sqme_alt)
    class(generic_event_t), intent(inout) :: event
    real(default), intent(in), optional :: weight_ref, weight_prc
    real(default), intent(in), optional :: sqme_ref, sqme_prc
    real(default), dimension(:), intent(in), optional :: sqme_alt, weight_alt
    real(default), intent(in), optional :: excess_prc
    if (present (sqme_prc)) then
       call event%set_sqme_prc (sqme_prc)
    end if
    if (present (sqme_ref)) then
       call event%set_sqme_ref (sqme_ref)
    end if
    if (present (sqme_alt)) then
       call event%set_sqme_alt (sqme_alt)
    end if
    if (present (weight_prc)) then
       call event%set_weight_prc (weight_prc)
    end if
    if (present (weight_ref)) then
       call event%set_weight_ref (weight_ref)
    end if
    if (present (weight_alt)) then
       call event%set_weight_alt (weight_alt)
    end if
    if (present (excess_prc)) then
       call event%set_excess_prc (excess_prc)
    end if
  end subroutine generic_event_set

  subroutine generic_event_reset (event)
    class(generic_event_t), intent(inout) :: event
    call event%discard_particle_set ()
    event%sqme_ref_known = .false.
    event%sqme_prc_known = .false.
    event%sqme_alt_known = .false.
    event%weight_ref_known = .false.
    event%weight_prc_known = .false.
    event%weight_alt_known = .false.
    event%excess_prc_known = .false.
  end subroutine generic_event_reset

  subroutine generic_event_pacify_particle_set (event)
    class(generic_event_t), intent(inout) :: event
    if (event%has_valid_particle_set ())  call pacify (event%particle_set)
  end subroutine generic_event_pacify_particle_set

  function event_normalization_mode (string, unweighted) result (mode)
    integer :: mode
    type(string_t), intent(in) :: string
    logical, intent(in) :: unweighted
    select case (lower_case (char (string)))
    case ("auto")
       if (unweighted) then
          mode = NORM_UNIT
       else
          mode = NORM_SIGMA
       end if
    case ("1")
       mode = NORM_UNIT
    case ("1/n")
       mode = NORM_N_EVT
    case ("sigma")
       mode = NORM_SIGMA
    case ("sigma/n")
       mode = NORM_S_N
    case default
       call msg_fatal ("Event normalization: unknown value '" &
            // char (string) // "'")
    end select
  end function event_normalization_mode

  function event_normalization_string (norm_mode) result (string)
    integer, intent(in) :: norm_mode
    type(string_t) :: string
    select case (norm_mode)
    case (NORM_UNDEFINED); string = "[undefined]"
    case (NORM_UNIT);      string = "'1'"
    case (NORM_N_EVT);     string = "'1/n'"
    case (NORM_SIGMA);     string = "'sigma'"
    case (NORM_S_N);       string = "'sigma/n'"
    case default;          string = "???"
    end select
  end function event_normalization_string

  subroutine event_normalization_update (weight, sigma, n, mode_new, mode_old)
    real(default), intent(inout) :: weight
    real(default), intent(in) :: sigma
    integer, intent(in) :: n
    integer, intent(in) :: mode_new, mode_old
    if (mode_new /= mode_old) then
       if (sigma > 0 .and. n > 0) then
          weight = weight / factor (mode_old) * factor (mode_new)
       else
          call msg_fatal ("Event normalization update: null sample")
       end if
    end if
  contains
    function factor (mode)
      real(default) :: factor
      integer, intent(in) :: mode
      select case (mode)
      case (NORM_UNIT);   factor = 1._default
      case (NORM_N_EVT);  factor = 1._default / n
      case (NORM_SIGMA);  factor = sigma
      case (NORM_S_N);    factor = sigma / n
      case default
         call msg_fatal ("Event normalization update: undefined mode")
      end select
    end function factor
  end subroutine event_normalization_update

  subroutine event_callback_nop_write (event_callback, unit)
    class(event_callback_nop_t), intent(in) :: event_callback
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "NOP"
  end subroutine event_callback_nop_write

  subroutine event_callback_nop (event_callback, i, event)
    class(event_callback_nop_t), intent(in) :: event_callback
    integer(i64), intent(in) :: i
    class(generic_event_t), intent(in) :: event
  end subroutine event_callback_nop


end module event_base
