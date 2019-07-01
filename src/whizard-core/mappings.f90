! WHIZARD 2.1.0 June 15 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module mappings

  use kinds, only: default !NODEP!
  use kinds, only: TC !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants, only: pi !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use md5
  use models
  use flavors

  implicit none
  private

  public :: mapping_defaults_t
  public :: mapping_defaults_md5sum
  public :: mapping_t
  public :: mapping_write
  public :: mapping_init
  public :: mapping_set_parameters
  public :: mapping_set_step_mapping_parameters
  public :: mapping_is_set
  public :: mapping_is_s_channel
  public :: mapping_get_mass
  public :: mapping_get_width
  public :: operator(==)
  public :: mapping_compute_msq_from_x
  public :: mapping_compute_x_from_msq
  public :: mapping_compute_ct_from_x
  public :: mapping_compute_x_from_ct

  integer, parameter :: &
       & EXTERNAL_PRT = -1, &
       & NO_MAPPING = 0, S_CHANNEL = 1, T_CHANNEL =  2, U_CHANNEL = 3, &
       & RADIATION = 4, COLLINEAR = 5, INFRARED = 6, &
       & STEP_MAPPING_E = 11, STEP_MAPPING_H = 12

  type :: mapping_defaults_t
     real(default) :: energy_scale = 10
     real(default) :: invariant_mass_scale = 10
     real(default) :: momentum_transfer_scale = 10
     logical :: step_mapping = .true.
     logical :: step_mapping_exp = .true.
     logical :: enable_s_mapping = .false.
  end type mapping_defaults_t

  type :: mapping_t
     private
     integer :: type = NO_MAPPING
     integer(TC) :: bincode
     type(flavor_t) :: flv
     real(default) :: mass = 0
     real(default) :: width = 0
     logical :: a_unknown = .true.
     real(default) :: a1 = 0
     real(default) :: a2 = 0
     real(default) :: a3 = 0
     logical :: b_unknown = .true.
     real(default) :: b1 = 0
     real(default) :: b2 = 0
     real(default) :: b3 = 0
     logical :: variable_limits = .true.
  end type mapping_t
  

  interface operator(==)
     module procedure mapping_equal
  end interface

contains

  function mapping_defaults_md5sum (mapping_defaults) result (md5sum_map)
    character(32) :: md5sum_map
    type(mapping_defaults_t), intent(in) :: mapping_defaults
    integer :: u
    u = free_unit ()
    open (u, status = "scratch")
    write (u, *)  mapping_defaults%energy_scale
    write (u, *)  mapping_defaults%invariant_mass_scale
    write (u, *)  mapping_defaults%momentum_transfer_scale
    write (u, *)  mapping_defaults%step_mapping
    write (u, *)  mapping_defaults%step_mapping_exp
    write (u, *)  mapping_defaults%enable_s_mapping
    rewind (u)
    md5sum_map = md5sum (u)
    close (u)
  end function mapping_defaults_md5sum

  subroutine mapping_write (map, unit, verbose)
    type(mapping_t), intent(in) :: map
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    character(len=9) :: str
    u = output_unit (unit);  if (u < 0)  return
    select case(map%type)
    case(S_CHANNEL); str = "s_channel"
    case(COLLINEAR); str = "collinear"
    case(INFRARED);  str = "infrared "
    case(RADIATION); str = "radiation"
    case(T_CHANNEL); str = "t_channel"
    case(U_CHANNEL); str = "u_channel"
    case(STEP_MAPPING_E);  str = "step_exp"
    case(STEP_MAPPING_H);  str = "step_hyp"
    end select
    if (map%type /= NO_MAPPING) then
       write (u, '(1x,A,I4,A)') &
            "Branch #", map%bincode, ":  " // &
            "Mapping (" // str // ") for particle " // &
            '"' // char (flavor_get_name (map%flv)) // '"'
       if (present (verbose)) then
          if (verbose) then
             select case (map%type)
             case (S_CHANNEL, RADIATION, STEP_MAPPING_E, STEP_MAPPING_H)
                write (u, *)  "  m/w    = ", map%mass, map%width
             case default
                write (u, *)  "  m      = ", map%mass
             end select
             select case (map%type)
             case (S_CHANNEL, T_CHANNEL, U_CHANNEL, &
                  STEP_MAPPING_E, STEP_MAPPING_H, &
                  COLLINEAR, INFRARED, RADIATION)
                write (u, *)  "  a1/2/3 = ", map%a1, map%a2, map%a3
             end select
             select case (map%type)
             case (T_CHANNEL, U_CHANNEL, COLLINEAR)
                write (u, *)  "  b1/2/3 = ", map%b1, map%b2, map%b3
             end select
          end if
       end if
    end if
  end subroutine mapping_write

  subroutine mapping_init (mapping, bincode, type, f, model)
    type(mapping_t), intent(inout) :: mapping
    integer(TC), intent(in) :: bincode
    type(string_t), intent(in) :: type
    integer, intent(in), optional :: f
    type(model_t), intent(in), optional, target :: model
    mapping%bincode = bincode
    select case (char (type))
    case ("s_channel");  mapping%type = S_CHANNEL
    case ("collinear");  mapping%type = COLLINEAR
    case ("infrared");   mapping%type = INFRARED
    case ("radiation");  mapping%type = RADIATION
    case ("t_channel");  mapping%type = T_CHANNEL
    case ("u_channel");  mapping%type = U_CHANNEL
    case ("step_exp");  mapping%type = STEP_MAPPING_E
    case ("step_hyp");  mapping%type = STEP_MAPPING_H
    end select
    if (present (f) .and. present (model)) &
         call flavor_init (mapping%flv, abs (f), model)
  end subroutine mapping_init

  subroutine mapping_set_parameters (map, mapping_defaults, variable_limits)
    type(mapping_t), intent(inout) :: map
    type(mapping_defaults_t), intent(in) :: mapping_defaults
    logical, intent(in) :: variable_limits
    if (map%type /= NO_MAPPING) then
       map%mass  = flavor_get_mass (map%flv)
       map%width = flavor_get_width (map%flv)
       map%variable_limits = variable_limits
       map%a_unknown = .true.
       map%b_unknown = .true.
       select case (map%type)
       case (S_CHANNEL)
          if (map%mass <= 0) then
             call mapping_write (map)
             call msg_fatal &
                  & (" S-channel resonance must have positive mass")
          else if (map%width <= 0) then
             call mapping_write (map)
             call msg_fatal &
                  & (" S-channel resonance must have positive width")
          end if
       case (RADIATION)
          map%width = max (map%width, mapping_defaults%energy_scale)
       case (INFRARED, COLLINEAR)
          map%mass = max (map%mass, mapping_defaults%invariant_mass_scale)
       case (T_CHANNEL, U_CHANNEL)
          map%mass = max (map%mass, mapping_defaults%momentum_transfer_scale)
       end select
    end if
  end subroutine mapping_set_parameters

  subroutine mapping_set_step_mapping_parameters (map, &
       mass, width, variable_limits)
    type(mapping_t), intent(inout) :: map
    real(default), intent(in) :: mass, width
    logical, intent(in) :: variable_limits
    select case (map%type)
    case (STEP_MAPPING_E, STEP_MAPPING_H)
       map%variable_limits = variable_limits
       map%a_unknown = .true.
       map%b_unknown = .true.
       map%mass = mass
       map%width = width
    end select
  end subroutine mapping_set_step_mapping_parameters

  function mapping_is_set (mapping) result (flag)
    logical :: flag
    type(mapping_t), intent(in) :: mapping
    flag = mapping%type /= NO_MAPPING
  end function mapping_is_set

  function mapping_is_s_channel (mapping) result (flag)
    logical :: flag
    type(mapping_t), intent(in) :: mapping
    flag = mapping%type == S_CHANNEL
  end function mapping_is_s_channel

  function mapping_get_mass (mapping) result (mass)
    real(default) :: mass
    type(mapping_t), intent(in) :: mapping
    mass = mapping%mass
  end function mapping_get_mass

  function mapping_get_width (mapping) result (width)
    real(default) :: width
    type(mapping_t), intent(in) :: mapping
    width = mapping%width
  end function mapping_get_width

  function mapping_equal (m1, m2) result (equal)
    type(mapping_t), intent(in) :: m1, m2
    logical :: equal
    if (m1%type == m2%type) then
       select case (m1%type)
       case (NO_MAPPING)
          equal = .true.
       case (S_CHANNEL, RADIATION, STEP_MAPPING_E, STEP_MAPPING_H)
          equal = (m1%mass == m2%mass) .and. (m1%width == m2%width)
       case default
          equal = (m1%mass == m2%mass)
       end select
    else
       equal = .false.
    end if
  end function mapping_equal

  subroutine mapping_compute_msq_from_x (map, s, msq_min, msq_max, msq, f, x)
    type(mapping_t), intent(inout) :: map
    real(default), intent(in) :: s, msq_min, msq_max
    real(default), intent(out) :: msq, f
    real(default), intent(in) :: x
    real(default) :: z, msq0, msq1, tmp
    integer :: type
    type = map%type
    if (s == 0) &
         call msg_fatal (" Applying msq mapping for zero energy")
    select case (type)
    case (S_CHANNEL, STEP_MAPPING_E, STEP_MAPPING_H)
       msq0 = map%mass**2
       if (msq0 < msq_min .or. msq0 > msq_max)  type = NO_MAPPING
    end select
    select case(type)
    case (NO_MAPPING)
       if (map%variable_limits .or. map%a_unknown) then
          map%a1 = 0
          map%a2 = msq_max - msq_min
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       msq = (1-x) * msq_min + x * msq_max
       f = map%a3
    case (S_CHANNEL)
       if (map%variable_limits .or. map%a_unknown) then
          msq0 = map%mass ** 2
          map%a1 = atan ((msq_min - msq0) / (map%mass * map%width))
          map%a2 = atan ((msq_max - msq0) / (map%mass * map%width))
          map%a3 = (map%a2 - map%a1) * (map%mass * map%width) / s 
          map%a_unknown = .false.
       end if
       z = (1-x) * map%a1 + x * map%a2
       if (-pi/2 < z .and. z < pi/2) then
          tmp = tan (z)
          msq = map%mass * (map%mass + map%width * tmp)
          f = map%a3 * (1 + tmp**2) 
       else
          msq = 0
          f = 0
       end if
    case (COLLINEAR, INFRARED, RADIATION)
       if (map%variable_limits .or. map%a_unknown) then
          if (type == RADIATION) then
             msq0 = map%width**2
          else
             msq0 = map%mass**2
          end if
          map%a1 = msq0
          map%a2 = log ((msq_max - msq_min) / msq0 + 1)
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       msq1 = map%a1 * exp (x * map%a2)
       msq = msq1 - map%a1 + msq_min 
       f = map%a3 * msq1
    case (T_CHANNEL, U_CHANNEL)
       if (map%variable_limits .or. map%a_unknown) then
          msq0 = map%mass**2
          map%a1 = msq0
          map%a2 = 2 * log ((msq_max - msq_min)/(2*msq0) + 1)
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       if (x < .5_default) then
          msq1 = map%a1 * exp (x * map%a2)
          msq = msq1 - map%a1 + msq_min
       else
          msq1 = map%a1 * exp ((1-x) * map%a2)
          msq = -(msq1 - map%a1) + msq_max
       end if
       f = map%a3 * msq1
    case (STEP_MAPPING_E)
       if (map%variable_limits .or. map%a_unknown) then
          map%a3 = max (2 * map%mass * map%width / (msq_max - msq_min), 0.01_default)
          map%a2 = exp (- (map%mass**2 - msq_min) / (msq_max - msq_min) &
                          / map%a3)
          map%a1 = 1 - map%a3 * log ((1 + map%a2 * exp (1 / map%a3)) / (1 + map%a2))
       end if
       tmp = exp (- x * map%a1 / map%a3) * (1 + map%a2)
       z = - map%a3 * log (tmp - map%a2)
       msq  = z * msq_max + (1 - z) * msq_min
       f = map%a1 / (1 - map%a2 / tmp) * (msq_max - msq_min) / s
    case (STEP_MAPPING_H)
       if (map%variable_limits .or. map%a_unknown) then
          map%a3 = (map%mass**2 - msq_min) / (msq_max - msq_min)
          map%a2 = max ((2 * map%mass * map%width / (msq_max - msq_min))**2 &
                        / map%a3, 1e-6_default)
          map%a1 = (1 + sqrt (1 + 4 * map%a2 / (1 - map%a3))) / 2
       end if
       z = map%a2 / (map%a1 - x) - map%a2 / map%a1 + map%a3 * x
       msq = z * msq_max + (1 - z) * msq_min
       f = (map%a2 / (map%a1 - x)**2 + map%a3) * (msq_max - msq_min) / s
    case default
       call msg_fatal ( " Attempt to apply undefined msq mapping")
    end select
  end subroutine mapping_compute_msq_from_x

  subroutine mapping_compute_x_from_msq (map, s, msq_min, msq_max, msq, f, x)
    type(mapping_t), intent(inout) :: map
    real(default), intent(in) :: s, msq_min, msq_max
    real(default), intent(in) :: msq
    real(default), intent(out) :: f, x
    real(default) :: msq0, msq1, tmp, z
    integer :: type
    type = map%type
    if (s == 0) &
         call msg_fatal (" Applying inverse msq mapping for zero energy")
    select case (type)
    case (S_CHANNEL, STEP_MAPPING_E, STEP_MAPPING_H)
       msq0 = map%mass**2
       if (msq0 < msq_min .or. msq0 > msq_max)  type = NO_MAPPING
    end select
    select case (type)
    case (NO_MAPPING)
       if (map%variable_limits .or. map%a_unknown) then
          map%a1 = 0
          map%a2 = msq_max - msq_min
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       if (map%a2 /= 0) then
          x = (msq - msq_min) / map%a2
       else
          x = 0
       end if
       f = map%a3
    case (S_CHANNEL)
       if (map%variable_limits .or. map%a_unknown) then
          msq0 = map%mass ** 2
          map%a1 = atan ((msq_min - msq0) / (map%mass * map%width))
          map%a2 = atan ((msq_max - msq0) / (map%mass * map%width))
          map%a3 = (map%a2 - map%a1) * (map%mass * map%width) / s 
          map%a_unknown = .false.
       end if
       tmp = (msq - msq0) / (map%mass * map%width)
       x = (atan (tmp) - map%a1) / (map%a2 - map%a1)
       f = map%a3 * (1 + tmp**2)
    case (COLLINEAR, INFRARED, RADIATION)
       if (map%variable_limits .or. map%a_unknown) then
          if (type == RADIATION) then
             msq0 = map%width**2
          else
             msq0 = map%mass**2
          end if
          map%a1 = msq0
          map%a2 = log ((msq_max - msq_min) / msq0 + 1)
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       msq1 = msq - msq_min + map%a1
       x = log (msq1 / map%a1) / map%a2
       f = map%a3 * msq1
    case (T_CHANNEL, U_CHANNEL)
       if (map%variable_limits .or. map%a_unknown) then
          msq0 = map%mass**2
          map%a1 = msq0
          map%a2 = 2 * log ((msq_max - msq_min)/(2*msq0) + 1)
          map%a3 = map%a2 / s
          map%a_unknown = .false.
       end if
       if (msq < (msq_max + msq_min)/2) then
          msq1 = msq - msq_min + map%a1
          x = log (msq1/map%a1) / map%a2
       else
          msq1 = msq_max - msq + map%a1
          x = 1 - log (msq1/map%a1) / map%a2
       end if
       f = map%a3 * msq1
    case (STEP_MAPPING_E)
       if (map%variable_limits .or. map%a_unknown) then
          map%a3 = max (2 * map%mass * map%width / (msq_max - msq_min), 0.01_default)
          map%a2 = exp (- (map%mass**2 - msq_min) / (msq_max - msq_min) &
                          / map%a3)
          map%a1 = 1 - map%a3 * log ((1 + map%a2 * exp (1 / map%a3)) / (1 + map%a2))
       end if
       z = (msq - msq_min) / (msq_max - msq_min)
       tmp = 1 + map%a2 * exp (z / map%a3)
       x = (z - map%a3 * log (tmp / (1 + map%a2))) &
           / map%a1
       f = map%a1 * tmp * (msq_max - msq_min) / s
    case (STEP_MAPPING_H)
       if (map%variable_limits .or. map%a_unknown) then
          map%a3 = (map%mass**2 - msq_min) / (msq_max - msq_min)
          map%a2 = max ((2 * map%mass * map%width / (msq_max - msq_min))**2 &
                        / map%a3, 1e-6_default)
          map%a1 = (1 + sqrt (1 + 4 * map%a2 / (1 - map%a3))) / 2
       end if
       z = (msq - msq_min) / (msq_max - msq_min)
       tmp = map%a2 / (map%a1 * map%a3)
       x = ((map%a1 + z / map%a3 + tmp) &
            - sqrt ((map%a1 - z / map%a3)**2 + 2 * tmp * (map%a1 + z / map%a3) &
                    + tmp**2)) / 2
       f = (map%a2 / (map%a1 - x)**2 + map%a3) * (msq_max - msq_min) / s
    case default
       call msg_fatal ( " Attempt to apply undefined msq mapping")
    end select
  end subroutine mapping_compute_x_from_msq

  subroutine mapping_compute_ct_from_x (map, s, ct, st, f, x)
    type(mapping_t), intent(inout) :: map
    real(default), intent(in) :: s
    real(default), intent(out) :: ct, st, f
    real(default), intent(in) :: x
    real(default) :: tmp, ct1
    select case (map%type)
    case (NO_MAPPING, S_CHANNEL, INFRARED, RADIATION, &
         STEP_MAPPING_E, STEP_MAPPING_H)
       tmp = 2 * (1-x)
       ct = 1 - tmp
       st = sqrt (tmp * (2-tmp))
       f = 1
    case (T_CHANNEL, U_CHANNEL, COLLINEAR)
       if (map%variable_limits .or. map%b_unknown) then
          map%b1 = map%mass**2 / s
          map%b2 = log ((map%b1 + 1) / map%b1)
          map%b3 = 0
          map%b_unknown = .false.
       end if
       if (x < .5_default) then
          ct1 = map%b1 * exp (2 * x * map%b2)
          ct = ct1 - map%b1 - 1
       else
          ct1 = map%b1 * exp (2 * (1-x) * map%b2)
          ct = -(ct1 - map%b1) + 1
       end if
       if (ct >= -1 .and. ct <= 1) then
          st = sqrt (1 - ct**2)
          f = ct1 * map%b2
       else
          ct = 1;  st = 0;  f = 0
       end if
    case default
       call msg_fatal (" Attempt to apply undefined ct mapping")
    end select
  end subroutine mapping_compute_ct_from_x

  subroutine mapping_compute_x_from_ct (map, s, ct, f, x)
    type(mapping_t), intent(inout) :: map
    real(default), intent(in) :: s
    real(default), intent(in) :: ct
    real(default), intent(out) :: f, x
    real(default) :: ct1
    select case (map%type)
    case (NO_MAPPING, S_CHANNEL, INFRARED, RADIATION, &
         STEP_MAPPING_E, STEP_MAPPING_H)
       x = (ct + 1) / 2
       f = 1
    case (T_CHANNEL, U_CHANNEL, COLLINEAR)
       if (map%variable_limits .or. map%b_unknown) then
          map%b1 = map%mass**2 / s
          map%b2 = log ((map%b1 + 1) / map%b1)
          map%b3 = 0
          map%b_unknown = .false.
       end if
       if (ct < 0) then
          ct1 = ct + map%b1 + 1
          x = log (ct1 / map%b1) / (2 * map%b2)
       else
          ct1 = -ct + map%b1 + 1
          x = 1 - log (ct1 / map%b1) / (2 * map%b2)
       end if
       f = ct1 * map%b2
    case default
       call msg_fatal (" Attempt to apply undefined inverse ct mapping")
    end select
  end subroutine mapping_compute_x_from_ct


end module mappings
