! WHIZARD 2.6.3 Feb 10 2018
!
! Copyright (C) 1999-2018 by
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
     logical :: has_pdfs = .false.
  contains
    procedure(pcm_allocate_instance), deferred :: allocate_instance
    procedure(pcm_is_nlo), deferred :: is_nlo
    procedure(pcm_final), deferred :: final
  end type pcm_t

  type, abstract :: pcm_instance_t
    class(pcm_t), pointer :: config => null ()
    logical :: bad_point = .false.
  contains
    procedure(pcm_instance_final), deferred :: final
    procedure :: link_config => pcm_instance_link_config
    procedure :: is_valid => pcm_instance_is_valid
    procedure :: set_bad_point => pcm_instance_set_bad_point
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


contains

  subroutine pcm_instance_link_config (pcm_instance, config)
     class(pcm_instance_t), intent(inout) :: pcm_instance
     class(pcm_t), intent(in), target :: config
     pcm_instance%config => config
  end subroutine pcm_instance_link_config

  function pcm_instance_is_valid (pcm_instance) result (valid)
    logical :: valid
    class(pcm_instance_t), intent(in) :: pcm_instance
    valid = .not. pcm_instance%bad_point
  end function pcm_instance_is_valid

  pure subroutine pcm_instance_set_bad_point (pcm_instance, bad_point)
    class(pcm_instance_t), intent(inout) :: pcm_instance
    logical, intent(in) :: bad_point
    pcm_instance%bad_point = pcm_instance%bad_point .or. bad_point
  end subroutine pcm_instance_set_bad_point


end module pcm_base
