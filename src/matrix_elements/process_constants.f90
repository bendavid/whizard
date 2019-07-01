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

module process_constants
  
  use kinds, only: default
  use iso_varying_string, string_t => varying_string

  implicit none
  private

  public :: process_constants_t

  type :: process_constants_t
     type(string_t) :: id
     type(string_t) :: model_name
     character(32) :: md5sum = ""
     logical :: openmp_supported = .false.
     integer :: n_in  = 0
     integer :: n_out = 0
     integer :: n_flv = 0
     integer :: n_hel = 0
     integer :: n_col = 0
     integer :: n_cin = 0
     integer :: n_cf  = 0
     integer, dimension(:,:), allocatable :: flv_state
     integer, dimension(:,:), allocatable :: hel_state
     integer, dimension(:,:,:), allocatable :: col_state
     logical, dimension(:,:), allocatable :: ghost_flag
     complex(default), dimension(:), allocatable :: color_factors
     integer, dimension(:,:), allocatable :: cf_index
  contains
    procedure :: get_flv_state => process_constants_get_flv_state
    procedure :: get_hel_state => process_constants_get_hel_state
    procedure :: get_col_state => process_constants_get_col_state
    procedure :: get_ghost_flag => process_constants_get_ghost_flag
    procedure :: get_color_factors => process_constants_get_color_factors
    procedure :: get_cf_index => process_constants_get_cf_index
    procedure :: set_flv_state => process_constants_set_flv_state
    procedure :: set_col_state => process_constants_set_col_state
    procedure :: set_cf_index => process_constants_set_cf_index
    procedure :: set_color_factors => process_constants_set_color_factors
    procedure :: set_ghost_flag => process_constants_set_ghost_flag
  end type process_constants_t


contains

  function process_constants_get_flv_state (prc_const) result (flv_state)
    class(process_constants_t), intent(in) :: prc_const
    integer, dimension(:,:), allocatable :: flv_state
    flv_state = prc_const%flv_state
  end function process_constants_get_flv_state

  subroutine process_constants_get_hel_state (prc_const, hel_state)
    class(process_constants_t), intent(in) :: prc_const
    integer, dimension(:,:), allocatable :: hel_state
    hel_state = prc_const%hel_state
  end subroutine process_constants_get_hel_state

  subroutine process_constants_get_col_state (prc_const, col_state)
    class(process_constants_t), intent(in) :: prc_const
    integer, dimension(:,:,:), allocatable :: col_state
    col_state = prc_const%col_state
  end subroutine process_constants_get_col_state

  function process_constants_get_ghost_flag (prc_const) result(ghost_flag)
   class(process_constants_t), intent(in) :: prc_const
   logical, dimension(:,:), allocatable :: ghost_flag
   ghost_flag = prc_const%ghost_flag
  end function process_constants_get_ghost_flag

  subroutine process_constants_get_color_factors (prc_const, col_facts)
   class(process_constants_t), intent(in) :: prc_const
   complex(default), intent(inout), dimension(:), allocatable :: col_facts
   col_facts = prc_const%color_factors
  end subroutine process_constants_get_color_factors

  subroutine process_constants_get_cf_index (prc_const, cf_index)
    class(process_constants_t), intent(in) :: prc_const
    integer, intent(inout), dimension(:,:), allocatable :: cf_index
    cf_index = prc_const%cf_index
  end subroutine process_constants_get_cf_index

  subroutine process_constants_set_flv_state (prc_const, flv_state)
    class(process_constants_t), intent(inout) :: prc_const
    integer, intent(in), dimension(:,:), allocatable :: flv_state
    prc_const%flv_state = flv_state
  end subroutine process_constants_set_flv_state

  subroutine process_constants_set_col_state (prc_const, col_state)
    class(process_constants_t), intent(inout) :: prc_const
    integer, intent(in), dimension(:,:,:), allocatable :: col_state
    prc_const%col_state = col_state
  end subroutine process_constants_set_col_state

  subroutine process_constants_set_cf_index (prc_const, cf_index)
    class(process_constants_t), intent(inout) :: prc_const
    integer, dimension(:,:), intent(in), allocatable :: cf_index
    prc_const%cf_index = cf_index
  end subroutine process_constants_set_cf_index

  subroutine process_constants_set_color_factors (prc_const, color_factors)
    class(process_constants_t), intent(inout) :: prc_const
    complex(default), dimension(:), intent(in), allocatable :: color_factors
    prc_const%color_factors = color_factors
  end subroutine process_constants_set_color_factors

  subroutine process_constants_set_ghost_flag (prc_const, ghost_flag)
    class(process_constants_t), intent(inout) :: prc_const
    logical, intent(in), dimension(:,:), allocatable :: ghost_flag
    prc_const%ghost_flag = ghost_flag
  end subroutine process_constants_set_ghost_flag

end module process_constants
