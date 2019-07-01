! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!

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
  end type process_constants_t


end module process_constants
