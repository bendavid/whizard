! WHIZARD 2.3.0 July 21 2016
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
!     Soyoung Shim <soyoung.shim@desy.de>
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

module fks_regions_uti

  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use os_interface
  use models

  use fks_regions

  implicit none
  private

  public :: fks_regions_1
  public :: fks_regions_2
  public :: fks_regions_3

contains

  subroutine fks_regions_1 (u)
    integer, intent(in) :: u
    type(flv_structure_t) :: flv_born, flv_real
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: test_model => null ()
    write (u, "(A)") "* Test output: fks_regions_1"
    write (u, "(A)") "* Purpose: Test utilities of flavor structure manipulation"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
       (var_str ("SM_rad"), var_str ("SM_rad.mdl"), os_data, test_model)

    flv_born = [11, -11, 2, -2]
    flv_real = [11, -11, 2, -2, 21]
    flv_born%n_in = 2; flv_real%n_in = 2
    write (u, "(A)") "* Valid splittings of ee -> uu"
    write (u, "(A)") "Born Flavors: "
    call flv_born%write (u)
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "3, 4 (2, -2) : ", flv_real%valid_pair (3, 4, flv_born, test_model)
    write (u, "(A,L1)") "4, 3 (-2, 2) : ", flv_real%valid_pair (4, 3, flv_born, test_model)
    write (u, "(A,L1)") "3, 5 (2, 21) : ", flv_real%valid_pair (3, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 3 (21, 2) : ", flv_real%valid_pair (5, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 5 (-2, 21): ", flv_real%valid_pair (4, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 4 (21, -2): ", flv_real%valid_pair (5, 4, flv_born, test_model)
    call write_separator (u)

    call flv_born%final ()
    call flv_real%final ()

    flv_born = [2, -2, 11, -11]
    flv_real = [2, -2, 11, -11, 21] 
    flv_born%n_in = 2; flv_real%n_in = 2
    write (u, "(A)") "* Valid splittings of uu -> ee"
    write (u, "(A)") "Born Flavors: "
    call flv_born%write (u)
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "1, 2 (2, -2) : " , flv_real%valid_pair (1, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 1 (-2, 2) : " , flv_real%valid_pair (2, 1, flv_born, test_model)
    write (u, "(A,L1)") "5, 2 (21, -2): " , flv_real%valid_pair (5, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 5 (-2, 21): " , flv_real%valid_pair (2, 5, flv_born, test_model)
    write (u, "(A,L1)") "1, 5 (21, 2) : " , flv_real%valid_pair (5, 1, flv_born, test_model)
    write (u, "(A,L1)") "5, 1 (2, 21) : " , flv_real%valid_pair (1, 5, flv_born, test_model)
    call flv_real%final ()
    flv_real = [21, -2, 11, -11, -2]  
    flv_real%n_in = 2
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "1, 2 (21, -2): " , flv_real%valid_pair (1, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 1 (-2, 21): " , flv_real%valid_pair (2, 1, flv_born, test_model)
    write (u, "(A,L1)") "5, 2 (-2, -2): " , flv_real%valid_pair (5, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 5 (-2, -2): " , flv_real%valid_pair (2, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 1 (-2, 21): " , flv_real%valid_pair (5, 1, flv_born, test_model)
    write (u, "(A,L1)") "1, 5 (21, -2): " , flv_real%valid_pair (1, 5, flv_born, test_model)
    call flv_real%final ()
    flv_real = [2, 21, 11, -11, 2]  
    flv_real%n_in = 2
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "1, 2 (2, 21) : " , flv_real%valid_pair (1, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 1 (21, 2) : " , flv_real%valid_pair (2, 1, flv_born, test_model)
    write (u, "(A,L1)") "5, 2 (2, 21) : " , flv_real%valid_pair (5, 2, flv_born, test_model)
    write (u, "(A,L1)") "2, 5 (21, 2) : " , flv_real%valid_pair (2, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 1 (2, 2)  : " , flv_real%valid_pair (5, 1, flv_born, test_model)
    write (u, "(A,L1)") "1, 5 (2, 2)  : " , flv_real%valid_pair (1, 5, flv_born, test_model)
    call write_separator (u)

    call flv_born%final ()
    call flv_real%final ()

    flv_born = [11, -11, 2, -2, 21]
    flv_real = [11, -11, 2, -2, 21, 21]
    flv_born%n_in = 2; flv_real%n_in = 2
    write (u, "(A)") "* Valid splittings of ee -> uug"
    write (u, "(A)") "Born Flavors: "
    call flv_born%write (u)
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "3, 4 (2, -2) : " , flv_real%valid_pair (3, 4, flv_born, test_model)
    write (u, "(A,L1)") "4, 3 (-2, 2) : " , flv_real%valid_pair (4, 3, flv_born, test_model)
    write (u, "(A,L1)") "3, 5 (2, 21) : " , flv_real%valid_pair (3, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 3 (21, 2) : " , flv_real%valid_pair (5, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 5 (-2, 21): " , flv_real%valid_pair (4, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 4 (21, -2): " , flv_real%valid_pair (5, 4, flv_born, test_model)
    write (u, "(A,L1)") "3, 6 (2, 21) : " , flv_real%valid_pair (3, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 3 (21, 2) : " , flv_real%valid_pair (6, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 6 (-2, 21): " , flv_real%valid_pair (4, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 4 (21, -2): " , flv_real%valid_pair (6, 4, flv_born, test_model)
    write (u, "(A,L1)") "5, 6 (21, 21): " , flv_real%valid_pair (5, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 5 (21, 21): " , flv_real%valid_pair (6, 5, flv_born, test_model)
    call flv_real%final ()
    flv_real = [11, -11, 2, -2, 1, -1]
    flv_real%n_in = 2
    write (u, "(A)") "Real Flavors (exemplary g -> dd splitting): "
    call flv_real%write (u)
    write (u, "(A,L1)") "3, 4 (2, -2) : " , flv_real%valid_pair (3, 4, flv_born, test_model)
    write (u, "(A,L1)") "4, 3 (-2, 2) : " , flv_real%valid_pair (4, 3, flv_born, test_model)
    write (u, "(A,L1)") "3, 5 (2, 1)  : " , flv_real%valid_pair (3, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 3 (1, 2)  : " , flv_real%valid_pair (5, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 5 (-2, 1) : " , flv_real%valid_pair (4, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 4 (1, -2) : " , flv_real%valid_pair (5, 4, flv_born, test_model)
    write (u, "(A,L1)") "3, 6 (2, -1) : " , flv_real%valid_pair (3, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 3 (-1, 2) : " , flv_real%valid_pair (6, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 6 (-2, -1): " , flv_real%valid_pair (4, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 4 (-1, -2): " , flv_real%valid_pair (6, 4, flv_born, test_model)
    write (u, "(A,L1)") "5, 6 (1, -1) : " , flv_real%valid_pair (5, 6, flv_born, test_model)
    write (u, "(A,L1)") "6, 5 (-1, 1) : " , flv_real%valid_pair (6, 5, flv_born, test_model)
    call write_separator (u)

    call flv_born%final ()
    call flv_real%final ()

    flv_born = [6, -5, 2, -1 ]
    flv_real = [6, -5, 2, -1, 21]
    flv_born%n_in = 1; flv_real%n_in = 1
    write (u, "(A)") "* Valid splittings of t -> b u d~"
    write (u, "(A)") "Born Flavors: "
    call flv_born%write (u)
    write (u, "(A)") "Real Flavors: "
    call flv_real%write (u)
    write (u, "(A,L1)") "1, 2 (6, -5) : " , flv_real%valid_pair (1, 2, flv_born, test_model)
    write (u, "(A,L1)") "1, 3 (6, 2)  : " , flv_real%valid_pair (1, 3, flv_born, test_model)
    write (u, "(A,L1)") "1, 4 (6, -1) : " , flv_real%valid_pair (1, 4, flv_born, test_model)
    write (u, "(A,L1)") "2, 1 (-5, 6) : " , flv_real%valid_pair (2, 1, flv_born, test_model)
    write (u, "(A,L1)") "3, 1 (2, 6)  : " , flv_real%valid_pair (3, 1, flv_born, test_model)
    write (u, "(A,L1)") "4, 1 (-1, 6) : " , flv_real%valid_pair (4, 1, flv_born, test_model)
    write (u, "(A,L1)") "2, 3 (-5, 2) : " , flv_real%valid_pair (2, 3, flv_born, test_model)
    write (u, "(A,L1)") "2, 4 (-5, -1): " , flv_real%valid_pair (2, 4, flv_born, test_model)
    write (u, "(A,L1)") "3, 2 (2, -5) : " , flv_real%valid_pair (3, 2, flv_born, test_model)
    write (u, "(A,L1)") "4, 2 (-1, -5): " , flv_real%valid_pair (4, 2, flv_born, test_model)
    write (u, "(A,L1)") "3, 4 (2, -1) : " , flv_real%valid_pair (3, 4, flv_born, test_model)
    write (u, "(A,L1)") "4, 3 (-1, 2) : " , flv_real%valid_pair (4, 3, flv_born, test_model)
    write (u, "(A,L1)") "1, 5 (6, 21) : " , flv_real%valid_pair (1, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 1 (21, 6) : " , flv_real%valid_pair (5, 1, flv_born, test_model)
    write (u, "(A,L1)") "2, 5 (-5, 21): " , flv_real%valid_pair (2, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 2 (21, 5) : " , flv_real%valid_pair (5, 2, flv_born, test_model)
    write (u, "(A,L1)") "3, 5 (2, 21) : " , flv_real%valid_pair (3, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 3 (21, 2) : " , flv_real%valid_pair (5, 3, flv_born, test_model)
    write (u, "(A,L1)") "4, 5 (-1, 21): " , flv_real%valid_pair (4, 5, flv_born, test_model)
    write (u, "(A,L1)") "5, 4 (21, -1): " , flv_real%valid_pair (5, 4, flv_born, test_model)

    call flv_born%final ()
    call flv_real%final ()

  end subroutine fks_regions_1

  subroutine fks_regions_2 (u)
    integer, intent(in) :: u
    integer :: n_flv_born, n_flv_real
    integer :: n_legs_born, n_legs_real
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(region_data_t) :: reg_data
    write (u, "(A)") "* Test output: fks_regions_2"
    write (u, "(A)") "* Create table of singular regions for ee -> qq"
    write (u, "(A)")

    n_flv_born = 1; n_flv_real = 1
    n_legs_born = 4; n_legs_real = 5
    n_in = 2

    allocate (flv_born (n_legs_born, n_flv_born))
    allocate (flv_real (n_legs_real, n_flv_real))
    flv_born (:, 1) = [11, -11, 2, -2]
    flv_real (:, 1) = [11, -11, 2, -2, 21]
    call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
    call reg_data%write (u)

    call write_separator (u)

    deallocate (flv_born, flv_real)
    call reg_data%final ()

    write (u, "(A)") "* Create table of singular regions for ee -> qqg"
    write (u, "(A)")
    n_flv_born = 1; n_flv_real = 2
    n_legs_born = 5; n_legs_real = 6
    n_in = 2
    allocate (flv_born (n_legs_born, n_flv_born))
    allocate (flv_real (n_legs_real, n_flv_real))

    flv_born (:, 1) = [11, -11, 2, -2, 21]
    flv_real (:, 1) = [11, -11, 2, -2, 21, 21]
    flv_real (:, 2) = [11, -11, 2, -2, 1, -1]

    call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
    call reg_data%write (u)

    
  end subroutine fks_regions_2

  subroutine fks_regions_3 (u)
    integer, intent(in) :: u
    integer :: n_flv_born, n_flv_real
    integer :: n_legs_born, n_legs_real
    integer :: n_in
    integer, dimension(:,:), allocatable :: flv_born, flv_real
    type(region_data_t) :: reg_data
    integer :: i, j
    integer, dimension(10) :: flavors
    write (u, "(A)") "* Test output: fks_regions_3"
    write (u, "(A)") "* Create table of singular regions for Drell Yan"
    write (u, "(A)")

    n_flv_born = 10; n_flv_real = 30
    n_legs_born = 4; n_legs_real = 5
    n_in = 2
    allocate (flv_born (n_legs_born, n_flv_born))
    allocate (flv_real (n_legs_real, n_flv_real))
    flavors = [-5, -4, -3, -2, -1, 1, 2, 3, 4, 5]
    do i = 1, n_flv_born
       flv_born (3:4, i) = [11, -11]
    end do
    do j = 1, n_flv_born
       flv_born (1, j) = flavors (j)
       flv_born (2, j) = -flavors (j)
    end do

    do i = 1, n_flv_real
       flv_real (3:4, i) = [11, -11]
    end do
    i = 1
    do j = 1, n_flv_real
       if (mod (j, 3) == 1) then
          flv_real (1, j) = flavors (i)
          flv_real (2, j) = -flavors (i)
          flv_real (5, j) = 21
       else if (mod (j, 3) == 2) then
          flv_real (1, j) = flavors (i)
          flv_real (2, j) = 21
          flv_real (5, j) = flavors (i)
       else
          flv_real (1, j) = 21
          flv_real (2, j) = -flavors (i)
          flv_real (5, j) = -flavors (i)
          i = i + 1
       end if
    end do
        
    call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
    call reg_data%write (u)

    call write_separator (u)

    deallocate (flv_born, flv_real)
    call reg_data%final ()

    write (u, "(A)") "* Create table of singular regions for hadronic top decay"
    write (u, "(A)")
    n_flv_born = 1; n_flv_real = 1
    n_legs_born = 4; n_legs_real = 5
    n_in = 1
    allocate (flv_born (n_legs_born, n_flv_born))
    allocate (flv_real (n_legs_real, n_flv_real))

    flv_born (:, 1) = [6, -5, 2, -1]
    flv_real (:, 1) = [6, -5, 2, -1, 21]

    call setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
    call reg_data%write (u)

  end subroutine fks_regions_3


end module fks_regions_uti
