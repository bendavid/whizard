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

module resonances_uti

!!!  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use model_data, only: model_data_t

  use resonances, only: resonance_history_t

  use resonances

  implicit none
  private

  public :: resonances_1
  public :: resonances_2
  public :: resonances_3

contains

  subroutine resonances_1 (u)
    integer, intent(in) :: u
    type(resonance_info_t) :: res_info
    type(resonance_history_t) :: res_history
    type(model_data_t), target :: model

    write (u, "(A)")  "* Test output: resonances_1"
    write (u, "(A)")  "*   Purpose: test resonance history setup"
    write (u, "(A)")

    write (u, "(A)")  "* Read model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Empty resonance history"
    write (u, "(A)")

    call res_history%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Add resonance"
    write (u, "(A)")

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)


    write (u, "(A)")
    write (u, "(A)")  "* Add another resonance"
    write (u, "(A)")

    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Remove resonance"
    write (u, "(A)")

    call res_history%remove_resonance (1)
    call res_history%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: resonances_1"

  end subroutine resonances_1

  subroutine resonances_2 (u)
    integer, intent(in) :: u
    type(resonance_info_t) :: res_info
    type(resonance_history_t) :: res_history
    type(model_data_t), target :: model
    type(string_t) :: restrictions

    write (u, "(A)")  "* Test output: resonances_2"
    write (u, "(A)")  "*   Purpose: test OMega restrictions strings &
         &for resonance history"
    write (u, "(A)")

    write (u, "(A)")  "* Read model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Empty resonance history"
    write (u, "(A)")

    restrictions = res_history%as_omega_string (2)
    write (u, "(A,A,A)")  "restrictions = '", char (restrictions), "'"

    write (u, "(A)")
    write (u, "(A)")  "* Add resonance"
    write (u, "(A)")

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    restrictions = res_history%as_omega_string (2)
    write (u, "(A,A,A)")  "restrictions = '", char (restrictions), "'"

    write (u, "(A)")
    write (u, "(A)")  "* Add another resonance"
    write (u, "(A)")

    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    restrictions = res_history%as_omega_string (2)
    write (u, "(A,A,A)")  "restrictions = '", char (restrictions), "'"

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: resonances_2"

  end subroutine resonances_2

  subroutine resonances_3 (u)
    integer, intent(in) :: u
    type(resonance_info_t) :: res_info
    type(resonance_history_t) :: res_history
    type(resonance_history_t), dimension(:), allocatable :: res_histories
    type(resonance_history_set_t) :: res_set
    type(model_data_t), target :: model
    integer :: i

    write (u, "(A)")  "* Test output: resonances_3"
    write (u, "(A)")  "*   Purpose: test resonance history set"
    write (u, "(A)")

    write (u, "(A)")  "* Read model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Initialize resonance history set"
    write (u, "(A)")

    call res_set%init (initial_size = 2)

    write (u, "(A)")  "* Add resonance histories, one at a time"
    write (u, "(A)")

    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, "(A)")
    write (u, "(A)")  "* Result"
    write (u, "(A)")

    call res_set%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Result in array form"

    call res_set%to_array (res_histories)
    do i = 1, size (res_histories)
       write (u, *)
       call res_histories(i)%write (u)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Re-initialize resonance history set with filter n=2"
    write (u, "(A)")

    call res_set%init (n_filter = 2)

    write (u, "(A)")  "* Add resonance histories, one at a time"
    write (u, "(A)")

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (7, 23, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, *)

    call res_info%init (3, -24, model, 5)
    call res_history%add_resonance (res_info)
    call res_history%write (u)
    call res_set%enter (res_history)
    call res_history%clear ()

    write (u, "(A)")
    write (u, "(A)")  "* Result"
    write (u, "(A)")

    call res_set%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: resonances_3"

  end subroutine resonances_3


end module resonances_uti
