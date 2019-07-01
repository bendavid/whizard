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

module dispatch_rng

  use kinds, only: i16
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use variables

  use rng_base
  use rng_tao
  use rng_stream

  implicit none
  private

  public :: dispatch_rng_factory
  public :: dispatch_rng_factory_extra

  procedure (dispatch_rng_factory), pointer :: &
       dispatch_rng_factory_extra => null ()

contains

  subroutine dispatch_rng_factory (rng_factory, var_list_global, var_list_local)
    class(rng_factory_t), allocatable, intent(inout) :: rng_factory
    type(var_list_t), intent(inout) :: var_list_global
    type(var_list_t), intent(in), optional :: var_list_local
    type(var_list_t) :: local
    type(string_t) :: rng_method
    integer :: seed
    character(30) :: buffer
    integer(i16) :: s
    if (present (var_list_local)) then
       local = var_list_local
    else
       local = var_list_global
    end if
    rng_method = local%get_sval (var_str ("$rng_method"))
    seed = local%get_ival (var_str ("seed"))
    s = int (mod (seed, 32768), i16)
    select case (char (rng_method))
    case ("tao")
       allocate (rng_tao_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing TAO random-number generator")
    case ("rng_stream")
       allocate (rng_stream_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing RNG Stream random-number generator")
    case default
       if (associated (dispatch_rng_factory_extra)) then
          call dispatch_rng_factory_extra (rng_factory, var_list_global, var_list_local)
       end if
       if (.not. allocated (rng_factory)) then
          call msg_fatal ("Random-number generator '" &
               // char (rng_method) // "' not implemented")
       end if
    end select
    write (buffer, "(I0)")  s
    call msg_message ("RNG: Setting seed for random-number generator to " &
            // trim (buffer))
    call rng_factory%init (s)
    call var_list_global%set_int (var_str ("seed"), seed + 1, &
         is_known = .true.)
  end subroutine dispatch_rng_factory


end module dispatch_rng
