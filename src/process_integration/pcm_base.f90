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

module pcm_base

  use diagnostics

  use prc_core_def
  use prc_core

  implicit none
  private

  public :: pcm_t
  public :: pcm_instance_t

  type, abstract :: pcm_t
     logical :: initialized = .false.
  contains
    procedure(pcm_allocate_instance), deferred :: allocate_instance
    procedure(pcm_is_nlo), deferred :: is_nlo
    procedure(pcm_final), deferred :: final
  end type pcm_t

  type, abstract :: pcm_instance_t
    class(pcm_t), pointer :: config => null ()
  contains
    procedure(pcm_instance_final), deferred :: final
    procedure(pcm_instance_is_valid), deferred :: is_valid
    procedure :: link_config => pcm_instance_link_config
  end type pcm_instance_t


  abstract interface
     subroutine pcm_allocate_instance (pcm, instance)
       import
       class(pcm_t), intent(in) :: pcm
       class(pcm_instance_t), intent(inout), allocatable :: instance
     end subroutine pcm_allocate_instance
  end interface

  abstract interface
     function pcm_is_nlo (pcm) result (is_nlo)
        import
        logical :: is_nlo
        class(pcm_t), intent(in) :: pcm
     end function pcm_is_nlo
  end interface

  abstract interface
     subroutine pcm_final (pcm)
        import
        class(pcm_t), intent(inout) :: pcm
     end subroutine pcm_final
  end interface

  abstract interface
     subroutine pcm_instance_final (pcm_instance)
        import
        class(pcm_instance_t), intent(inout) :: pcm_instance
     end subroutine pcm_instance_final
  end interface

  abstract interface
     function pcm_instance_is_valid (pcm_instance) result (valid)
        import
        logical :: valid
        class(pcm_instance_t), intent(in) :: pcm_instance
     end function pcm_instance_is_valid
  end interface


contains

  subroutine pcm_instance_link_config (pcm_instance, config)
     class(pcm_instance_t), intent(inout) :: pcm_instance
     class(pcm_t), intent(in), target :: config
     pcm_instance%config => config
  end subroutine pcm_instance_link_config


end module pcm_base
