! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
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

module nlo_color_data_uti
  use physics_defs, only: NO_FACTORIZATION
  use nlo_color_data, only: nlo_color_data_t
  use fks_regions, only: region_data_t, setup_region_data_for_test

  implicit none
  private

  public :: nlo_color_data_1
  public :: nlo_color_data_2
  public :: nlo_color_data_3

contains

  subroutine nlo_color_data_1 (u)
    integer, intent(in) :: u
    type(nlo_color_data_t) :: color_data
    integer :: n_legs_born, n_legs_real
    integer :: n_flv_born, n_flv_real
    integer :: n_col_born, n_col_real
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(region_data_t) :: reg_data
    write (u, "(A)") "* Test output: nlo_color_data_1"
    write (u, "(A)") "* Purpose: Test internal computation of color-correlated matrix elements"
    write (u, "(A)") "* in the color-flow scheme"
    write (u, "(A)")
    write (u, "(A)") "* Process: e- e+ -> u u~"

    n_legs_born = 4; n_legs_real = 5
    n_flv_born = 1; n_flv_real = 1
    n_col_born = 1; n_col_real = 2
    allocate (color_data%col_state_born (2, n_legs_born, n_col_born))
    associate (col_state_born => color_data%col_state_born)
       col_state_born (:, 1, 1) = [0, 0]
       col_state_born (:, 2, 1) = [0, 0]
       col_state_born (:, 3, 1) = [1, 0]
       col_state_born (:, 4, 1) = [0, -1]
    end associate
    allocate (color_data%col_state_real (2, n_legs_real, n_col_real))
    associate (col_state_real => color_data%col_state_real)
       col_state_real (:, 1, 1) = [0, 0]
       col_state_real (:, 2, 1) = [0, 0]
       col_state_real (:, 1, 2) = [0, 0]
       col_state_real (:, 2, 2) = [0, 0]
       col_state_real (:, 3, 1) = [1, 0]
       col_state_real (:, 4, 1) = [0, -2]
       col_state_real (:, 5, 1) = [2, -1]
       col_state_real (:, 3, 2) = [1, 0]
       col_state_real (:, 4, 2) = [0, -1]
       col_state_real (:, 5, 2) = [0, 0]
   end associate
   allocate (color_data%cf_index_real (2, n_col_real))
   color_data%cf_index_real(:, 1) = [1, 1]
   color_data%cf_index_real(:, 2) = [2, 2]
   allocate (color_data%color_factors_born (n_col_born))
   color_data%color_factors_born (1) = cmplx (3, 0)
   color_data%n_col_born = n_col_born
   color_data%n_col_real = n_col_real

   n_in = 2
   allocate (flv_born (n_legs_born, n_flv_born))
   allocate (flv_real (n_legs_real, n_flv_real))
   flv_born (:, 1) = [11, -11, 2, -2]
   flv_real (:, 1) = [11, -11, 2, -2, 21]
   call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
   allocate (color_data%ghost_flag_born (n_legs_born, n_col_born))
   allocate (color_data%ghost_flag_real (n_legs_real, n_col_real))
   color_data%ghost_flag_born (:, 1) = .false.
   color_data%ghost_flag_real (:, 1) = .false.
   color_data%ghost_flag_real (1 : n_legs_born, 2) = .false.
   color_data%ghost_flag_real (n_legs_real, 2) = .true.
   allocate (color_data%included_color_structures (2))
   color_data%included_color_structures = [1, 2]
   call color_data%init_color (reg_data)
   call color_data%check_equivalences ()
   call color_data%compute_betaij (reg_data, NO_FACTORIZATION)
   call color_data%write (u, verbose = .true.)
  end subroutine nlo_color_data_1

  subroutine nlo_color_data_2 (u)
    integer, intent(in) :: u
    type(nlo_color_data_t) :: color_data
    integer :: n_legs_born, n_legs_real
    integer :: n_flv_born, n_flv_real
    integer :: n_col_born, n_col_real
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(region_data_t) :: reg_data
    integer :: i
    write (u, "(A)") "* Test output: nlo_color_data_2"
    write (u, "(A)") "* Purpose: Test internal computation of color-correlated matrix elements"
    write (u, "(A)") "* in the color-flow scheme"
    write (u, "(A)")
    write (u, "(A)") "* Process: u u~ -> Z Z"

    n_legs_born = 4; n_legs_real = 5
    n_flv_born = 1; n_flv_real = 3
    n_col_born = 1; n_col_real = 6
    allocate (color_data%col_state_born (2, n_legs_born, n_col_born))
    associate (col_state_born => color_data%col_state_born)
       col_state_born (:, 1, 1) = [1, 0]
       col_state_born (:, 2, 1) = [0, -1]
       col_state_born (:, 3, 1) = [0, 0]
       col_state_born (:, 4, 1) = [0, 0]
    end associate
    allocate (color_data%col_state_real (2, n_legs_real, n_col_real))
    associate (col_state_real => color_data%col_state_real)
       col_state_real (:, 1, 1) = [0, 0]
       col_state_real (:, 2, 1) = [0, -2]
       col_state_real (:, 3, 1) = [0, 0]
       col_state_real (:, 4, 1) = [0, 0]
       col_state_real (:, 5, 1) = [0, -2]
       col_state_real (:, 1, 2) = [1, 0]
       col_state_real (:, 2, 2) = [0, -1]
       col_state_real (:, 3, 2) = [0, 0]
       col_state_real (:, 4, 2) = [0, 0]
       col_state_real (:, 5, 2) = [0, 0]
       col_state_real (:, 1, 3) = [1, 0]
       col_state_real (:, 2, 3) = [2, -1]
       col_state_real (:, 3, 3) = [0, 0]
       col_state_real (:, 4, 3) = [0, 0]
       col_state_real (:, 5, 3) = [2, 0]
       col_state_real (:, 1, 4) = [2, -1]
       col_state_real (:, 2, 4) = [0, -2]
       col_state_real (:, 3, 4) = [0, 0]
       col_state_real (:, 4, 4) = [0, 0]
       col_state_real (:, 5, 4) = [0, -1]
       col_state_real (:, 1, 5) = [2, 0]
       col_state_real (:, 2, 5) = [0, 0]
       col_state_real (:, 3, 5) = [0, 0]
       col_state_real (:, 4, 5) = [0, 0]
       col_state_real (:, 5, 5) = [2, 0]
       col_state_real (:, 1, 6) = [2, 0]
       col_state_real (:, 2, 6) = [0, -1]
       col_state_real (:, 3, 6) = [0, 0]
       col_state_real (:, 4, 6) = [0, 0]
       col_state_real (:, 5, 6) = [2, -1]
   end associate
   allocate (color_data%cf_index_real (2, n_col_real))
   do i = 1, n_col_real
      color_data%cf_index_real(:, i) = [i, i]
   end do
   allocate (color_data%color_factors_born (n_col_born))
   color_data%color_factors_born (1) = cmplx (3, 0)
   color_data%n_col_born = n_col_born
   color_data%n_col_real = n_col_real

   n_in = 2
   allocate (flv_born (n_legs_born, n_flv_born))
   allocate (flv_real (n_legs_real, n_flv_real))
   flv_born (:, 1) = [2, -2, 23, 23]
   flv_real (:, 1) = [2, -2, 23, 23, 21]
   flv_real (:, 2) = [2, 21, 23, 23, 2]
   flv_real (:, 3) = [21, -2, 23, 23, -2]
   call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
   allocate (color_data%ghost_flag_born (n_legs_born, n_col_born))
   allocate (color_data%ghost_flag_real (n_legs_real, n_col_real))
   color_data%ghost_flag_born (:, 1) = .false.
   color_data%ghost_flag_real (1, 1) = .true.
   color_data%ghost_flag_real (2 : n_legs_real, 1) = .false.
   color_data%ghost_flag_real (1 : n_legs_born, 2) = .false.
   color_data%ghost_flag_real (n_legs_real, 2) = .true.
   color_data%ghost_flag_real (:, 3) = .false.
   color_data%ghost_flag_real (:, 4) = .false.
   color_data%ghost_flag_real (1, 5) = .false.
   color_data%ghost_flag_real (2, 5) = .false.
   color_data%ghost_flag_real (3 : n_legs_real, 5) = .true.
   color_data%ghost_flag_real (:, 6) = .false.
   allocate (color_data%included_color_structures (2))
   color_data%included_color_structures = [2, 6]
   call color_data%init_color (reg_data)
   call color_data%check_equivalences ()
   call color_data%compute_betaij (reg_data, NO_FACTORIZATION)
   call color_data%write (u, verbose = .true.)
  end subroutine nlo_color_data_2

  subroutine nlo_color_data_3 (u)
    integer, intent(in) :: u
    type(nlo_color_data_t) :: color_data
    integer :: n_legs_born, n_legs_real
    integer :: n_flv_born, n_flv_real
    integer :: n_col_born, n_col_real
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(region_data_t) :: reg_data
    integer :: i_col, i_flv, pdg1, pdg2
    write (u, "(A)") "* Test output: nlo_color_data_2"
    write (u, "(A)") "* Purpose: Test internal computation of color-correlated matrix elements"
    write (u, "(A)") "* in the color-flow scheme"
    write (u, "(A)")
    write (u, "(A)") "* Process: u u~ -> Z Z"

    n_legs_born = 4; n_legs_real = 5
    n_flv_born = 10; n_flv_real = 30
    n_col_born = 2; n_col_real = 12
    allocate (color_data%col_state_born (2, n_legs_born, n_col_born))
    associate (col_state_born => color_data%col_state_born)
       do i_col = 1, n_col_born
          col_state_born (:, 1, i_col) = [1, 0]
          col_state_born (:, 2, i_col) = [0, -1]
          col_state_born (:, 3, i_col) = [0, 0]
          col_state_born (:, 4, i_col) = [0, 0]
       end do
    end associate
    allocate (color_data%col_state_real (2, n_legs_real, n_col_real))
    associate (col_state_real => color_data%col_state_real)
       do i_col = 1, n_col_real, 6
          col_state_real (:, 1, i_col) = [0, 0]
          col_state_real (:, 2, i_col) = [0, -2]
          col_state_real (:, 3, i_col) = [0, 0]
          col_state_real (:, 4, i_col) = [0, 0]
          col_state_real (:, 5, i_col) = [0, -2]
          col_state_real (:, 1, i_col + 1) = [1, 0]
          col_state_real (:, 2, i_col + 1) = [0, -1]
          col_state_real (:, 3, i_col + 1) = [0, 0]
          col_state_real (:, 4, i_col + 1) = [0, 0]
          col_state_real (:, 5, i_col + 1) = [0, 0]
          col_state_real (:, 1, i_col + 2) = [1, 0]
          col_state_real (:, 2, i_col + 2) = [2, -1]
          col_state_real (:, 3, i_col + 2) = [0, 0]
          col_state_real (:, 4, i_col + 2) = [0, 0]
          col_state_real (:, 5, i_col + 2) = [2, 0]
          col_state_real (:, 1, i_col + 3) = [2, -1]
          col_state_real (:, 2, i_col + 3) = [0, -2]
          col_state_real (:, 3, i_col + 3) = [0, 0]
          col_state_real (:, 4, i_col + 3) = [0, 0]
          col_state_real (:, 5, i_col + 3) = [0, -1]
          col_state_real (:, 1, i_col + 4) = [2, 0]
          col_state_real (:, 2, i_col + 4) = [0, 0]
          col_state_real (:, 3, i_col + 4) = [0, 0]
          col_state_real (:, 4, i_col + 4) = [0, 0]
          col_state_real (:, 5, i_col + 4) = [2, 0]
          col_state_real (:, 1, i_col + 5) = [2, 0]
          col_state_real (:, 2, i_col + 5) = [0, -1]
          col_state_real (:, 3, i_col + 5) = [0, 0]
          col_state_real (:, 4, i_col + 5) = [0, 0]
          col_state_real (:, 5, i_col + 5) = [2, -1]
      end do
   end associate
   allocate (color_data%cf_index_real (2, n_col_real))
   do i_col = 1, n_col_real
      color_data%cf_index_real(:, i_col) = [i_col, i_col]
   end do
   allocate (color_data%color_factors_born (n_col_born))
   color_data%color_factors_born (:) = cmplx (3, 0)
   color_data%n_col_born = n_col_born
   color_data%n_col_real = n_col_real

   n_in = 2
   allocate (flv_born (n_legs_born, n_flv_born))
   allocate (flv_real (n_legs_real, n_flv_real))
   pdg1 = -5; pdg2 = 5
   do i_flv = 1, n_flv_born
      if (pdg1 == 0 .and. pdg2 == 0) then
         pdg1 = pdg1 + 1; pdg2 = pdg2 - 1
      end if
      flv_born (:, i_flv) = [pdg1, pdg2, 23, 23]
      pdg1 = pdg1 + 1; pdg2 = pdg2 - 1
   end do
   pdg1 = -5; pdg2 = 5
   do i_flv = 1, n_flv_real, 3
      if (pdg1 == 0 .and. pdg2 == 0) then
         pdg1 = pdg1 + 1; pdg2 = pdg2 - 1
      end if
      flv_real (:, i_flv) = [pdg1, pdg2, 23, 23, 21]
      flv_real (:, i_flv + 1) = [pdg1, 21, 23, 23, pdg1]
      flv_real (:, i_flv + 2) = [21, pdg2, 23, 23, pdg2]
      pdg1 = pdg1 + 1; pdg2 = pdg2 - 1
   end do
   call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
   allocate (color_data%ghost_flag_born (n_legs_born, n_col_born))
   allocate (color_data%ghost_flag_real (n_legs_real, n_col_real))
   do i_col = 1, n_col_born
      color_data%ghost_flag_born (:, i_col) = .false.
   end do
   do i_col = 1, n_col_real, 6
      color_data%ghost_flag_real (1, i_col) = .true.
      color_data%ghost_flag_real (2 : n_legs_real, i_col) = .false.
      color_data%ghost_flag_real (1 : n_legs_born, i_col + 1) = .false.
      color_data%ghost_flag_real (n_legs_real, i_col + 1) = .true.
      color_data%ghost_flag_real (:, i_col + 2) = .false.
      color_data%ghost_flag_real (:, i_col + 3) = .false.
      color_data%ghost_flag_real (1, i_col + 4) = .false.
      color_data%ghost_flag_real (2, i_col + 4) = .false.
      color_data%ghost_flag_real (3 : n_legs_real, i_col + 4) = .true.
      color_data%ghost_flag_real (:, i_col + 5) = .false.
   end do
   allocate (color_data%included_color_structures (2))
   color_data%included_color_structures = [2, 6]
   call color_data%init_color (reg_data)
   call color_data%check_equivalences ()
   call color_data%compute_betaij (reg_data, NO_FACTORIZATION)
   call color_data%write (u, verbose = .true.)
  end subroutine nlo_color_data_3


end module nlo_color_data_uti

