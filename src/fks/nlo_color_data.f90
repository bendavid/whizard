! WHIZARD 2.5.0 May 06 2017
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

module nlo_color_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use numeric_utils
  use diagnostics
  use physics_defs
  use process_constants !NODEP!
  use sm_physics
  use os_interface
  use models
  use pdg_arrays
  use particle_specifiers
  use phs_base
  use phs_single
  use cascades
  use state_matrices
  use interactions
  use lorentz
  use prc_core
  use prc_user_defined
  use sf_base
  use colors
  use phs_fks
  use flavors
  use fks_regions
  use nlo_data
  use virtual
  use real_subtraction
  use dglap_remnant

  implicit none
  private

  public :: nlo_color_data_t

  type :: color_index_list_t
     integer, dimension(:), allocatable :: col
  end type color_index_list_t

  type :: ftuple_color_map_t
    integer :: index
    integer :: color_index
    type(ftuple_color_map_t), pointer :: next
    type(ftuple_color_map_t), pointer :: prev
  contains
    procedure :: init => ftuple_color_map_init
    procedure :: present => ftuple_color_map_present
    procedure :: append => ftuple_color_map_append
    procedure :: get_n_entries => ftuple_color_map_get_n_entries
    procedure :: get_index_array => ftuple_color_map_get_index_array
    procedure :: get_entry => ftuple_color_map_get_entry
    procedure :: create_map => ftuple_color_map_create_map
  end type ftuple_color_map_t

  type nlo_color_data_t
    type(ftuple_color_map_t), dimension(:), allocatable :: icm
    integer, dimension(:,:,:), allocatable :: col_state_born, col_state_real
    logical, dimension(:,:), allocatable :: ghost_flag_born, ghost_flag_real
    integer :: n_col_born, n_col_real
    type(color_t), dimension(:,:), allocatable :: color_real
    integer, dimension(:), allocatable :: col_born
    complex(default), dimension(:), allocatable :: color_factors_born
    integer, dimension(:,:), allocatable :: cf_index_real
    real(default), dimension(:,:,:), allocatable :: beta_ij
    logical :: beta_ij_evaluated = .false.
    logical :: color_is_conserved = .false.
    integer, dimension(:), allocatable :: equivalent_color_up_to_sign
    integer, dimension(:), allocatable :: included_color_structures

  contains
    procedure :: prepare => nlo_color_data_prepare
    procedure :: init_color_matrix => nlo_color_data_init_color_matrix
    procedure :: init_ghost_flags => nlo_color_data_init_ghost_flags
    procedure :: check_equivalences => nlo_color_data_check_equivalences
    procedure :: init_color => nlo_color_data_init_color
    procedure :: compute_betaij => nlo_color_data_compute_betaij
    procedure :: fill_betaij_matrix => nlo_color_data_fill_betaij_matrix
    procedure :: fill_betaij_matrix_threshold &
       => nlo_color_data_fill_betaij_matrix_threshold
    procedure :: compute_bij => nlo_color_data_compute_bij
    procedure :: write => nlo_color_data_write
  end type nlo_color_data_t


contains

  subroutine ftuple_color_map_init (icm)
    class(ftuple_color_map_t), intent(inout), target :: icm
    icm%index = 0
    icm%color_index = 0
    nullify (icm%next)
    nullify (icm%prev)
  end subroutine ftuple_color_map_init

  function ftuple_color_map_present (icm, color_index) result(pres)
    class(ftuple_color_map_t), intent(in), target :: icm
    integer, intent(in) :: color_index
    logical :: pres
    type(ftuple_color_map_t), pointer :: current
    pres = .false.
    select type (icm)
    type is (ftuple_color_map_t)
       current => icm
       do
          if (current%color_index == color_index) then
             pres = .true.
             exit
          else
             if (associated (current%next)) then
                current => current%next
             else
                exit
             end if
          end if
       end do
    end select
  end function ftuple_color_map_present

  subroutine ftuple_color_map_append (icm, val)
    class(ftuple_color_map_t), intent(inout), target :: icm
    integer, intent(in) :: val
    type(ftuple_color_map_t), pointer :: current
    select type (icm)
    type is (ftuple_color_map_t)
    if (.not. icm%present (val)) then
      if (icm%index == 0) then
        nullify(icm%next)
        icm%index = 1
        icm%color_index = val
      else
        current => icm
        do
          if (associated (current%next)) then
            current => current%next
          else
            allocate (current%next)
            nullify (current%next%next)
            current%next%prev => current
            current%next%index = current%index + 1
            current%next%color_index = val
            exit
          end if
        end do
      end if
    end if
    end select
  end subroutine ftuple_color_map_append

  function ftuple_color_map_get_n_entries (icm) result(n_entries)
    class(ftuple_color_map_t), intent(in), target :: icm
    integer :: n_entries
    type(ftuple_color_map_t), pointer :: current
    n_entries = 0
    select type (icm)
    type is (ftuple_color_map_t)
       current => icm
       do
          if (associated (current%next)) then
             current => current%next
          else
             n_entries = current%index
             exit
          end if
       end do
    end select
  end function ftuple_color_map_get_n_entries

  function ftuple_color_map_get_index_array (icm) result(iarr)
    class(ftuple_color_map_t), intent(in), target :: icm
    integer, dimension(:), allocatable :: iarr
    type(ftuple_color_map_t), pointer :: current
    integer :: n_entries
    integer :: i
    call msg_debug &
         (D_SUBTRACTION, "inside ftuple_color_map_get_index_array")
    select type (icm)
    type is (ftuple_color_map_t)
    n_entries = icm%get_n_entries ()
    call msg_debug (D_SUBTRACTION, "n_entries = ", n_entries)
    allocate (iarr(n_entries))
    do i = 1, n_entries
      if (i == 1) then
        current => icm
      else
        current => current%next
      end if
      iarr(i) = current%color_index
      if (debug_active (D_SUBTRACTION)) &
           print *, "i = ", i, "   iarr(i) = ", iarr(i)
    end do
    end select
  end function ftuple_color_map_get_index_array

  function ftuple_color_map_get_entry (icm, index) result(entry)
    class(ftuple_color_map_t), intent(in), target :: icm
    integer, intent(in) :: index
    integer :: entry
    type(ftuple_color_map_t), pointer :: current
    integer :: i
    entry = 0
    select type (icm)
    type is (ftuple_color_map_t)
       if (index <= icm%get_n_entries ()) then
          do i = 1, icm%get_n_entries ()
             if (i == 1) then
                current => icm
             else
                current => current%next
             end if
             if (i == index) entry = current%color_index
          end do
       end if
    end select
  end function ftuple_color_map_get_entry

  recursive subroutine ftuple_color_map_create_map (icm, flst, &
       emitter, allreg, color_states_born, color_states_real, &
       included_color_structures, p_rad_in)
    class(ftuple_color_map_t), intent(inout) :: icm
    type(flv_structure_t), intent(in) :: flst
    integer, intent(in) :: emitter
    type(ftuple_t), intent(in), dimension(:) :: allreg
    integer, intent(in), dimension(:,:,:) :: color_states_born
    integer, intent(in), dimension(:,:,:) :: color_states_real
    integer, intent(in), dimension(:) :: included_color_structures
    integer, intent(in), optional :: p_rad_in
    integer :: nreg, region
    integer :: p1, p2, p_rad
    integer :: flv_em, flv_rad
    integer :: n_col_real
    integer, dimension(2) :: col_em, col_rad
    integer :: i
    !!! splitting type: 1 - q -> qg
    !!!                 2 - g -> qq
    !!!                 3 - g -> gg
    integer :: splitting_type_flv, splitting_type_col
    if (debug_active (D_SUBTRACTION)) then
       print *, "CREATING THE COLOR MAP"
       print *, " ... and the emitter is: ", emitter   
       do i = 1, size (allreg)
          call allreg(i)%write ()
       end do
    end if
    nreg = size (allreg)
    call msg_debug (D_SUBTRACTION, "size of all regions = ", nreg)
    n_col_real = size (color_states_real (1,1,:))
    call msg_debug (D_SUBTRACTION, "number color flows real = ", n_col_real)
    do region = 1, nreg
      call allreg(region)%get (p1, p2)
      if (debug_active (D_SUBTRACTION)) then
         print *, "region = ", region
         print *, "   p1  = ", p1
         print *, "   p2  = ", p2
      end if
      if (p1 == emitter .or. p2 == emitter .or. present (p_rad_in)) then
        if (.not. present (p_rad_in)) then
          if (p1 == emitter) then
            p_rad = p2
          else
            p_rad = p1
          end if
        else
          p_rad = p_rad_in
        end if
        if (emitter /= 0) then
          flv_em = flst%flst (emitter)
          call msg_debug (D_SUBTRACTION, "flv(emitter) = ", flv_em)
        else
           !!! In the 3-jet example ee -> jjj this is never entered
          call icm%create_map &
               (flst, 1, allreg, color_states_born, color_states_real, &
               included_color_structures, p_rad)
          call icm%create_map &
               (flst, 2, allreg, color_states_born, color_states_real, &
               included_color_structures, p_rad)
          return
        end if
        flv_rad = flst%flst (p_rad)
        call msg_debug (D_SUBTRACTION, "flv(radiated) = ", flv_rad)
        !!! q -> qg or g -> qq
        if (is_quark (flv_em) .and. is_gluon (flv_rad) .or. &
            is_gluon (flv_em) .and. is_quark (flv_rad)) then
           splitting_type_flv = 1
        !!! q -> gq   
        else if (is_quark (flv_em) .and. flv_em + flv_rad == 0) then
           splitting_type_flv = 2
        !!! g -> gg   
        else if (is_gluon (flv_em) .and. is_gluon (flv_rad)) then
           splitting_type_flv = 3
        else
          splitting_type_flv = 0
        end if
        call msg_debug &
             (D_SUBTRACTION, "splitting_type = ", splitting_type_flv)
        do i = 1, n_col_real
           if (.not. (any (i == included_color_structures))) cycle
           col_em = color_states_real(:,emitter,i)
           col_rad = color_states_real(:,p_rad,i)
          if (is_color_singlet (col_em(1), col_em(2)) &
              .and. (is_color_doublet (col_rad(1), col_rad(2)) &
              .or. is_color_ghost (col_rad(1), col_rad(2)))) then
            splitting_type_col = 1
          else if (is_color_singlet (col_em(1), col_em(2)) .and. &
                   is_color_singlet (col_rad(1), col_rad(2))) then
            splitting_type_col = 2
          else if (is_color_doublet (col_em(1), col_em(2)) .and. &
                   is_color_doublet (col_rad(1), col_rad(2))) then
            splitting_type_col = 3
          else
            splitting_type_col = 0
          end if
          call msg_debug (D_SUBTRACTION, &
               "splitting_type_col = ", splitting_type_col)
          if (splitting_type_flv == splitting_type_col .and. &
              splitting_type_flv /= 0) then
             call msg_debug (D_SUBTRACTION, &
                  "we are appending the color structure to region i = ", i)
             call icm%append (i)
          end if
        end do
      end if
    end do
  contains
    function is_color_singlet (c1, c2) result (singlet)
      integer, intent(in) :: c1, c2
      logical :: singlet
      singlet = (c1 == 0 .and. c2 /= 0) .or. (c1 /= 0 .and. c2 == 0)
    end function is_color_singlet
    function is_color_doublet (c1, c2) result (doublet)
      integer, intent(in) :: c1, c2
      logical :: doublet
      doublet = c1 /= 0 .and. c2 /= 0
    end function is_color_doublet
    function is_color_ghost (c1, c2) result (ghost)
      integer, intent(in) :: c1, c2
      logical :: ghost
      ghost = c1 == 0 .and. c2 == 0
    end function is_color_ghost
  end subroutine ftuple_color_map_create_map

  subroutine nlo_color_data_prepare (color_data, reg_data, flavor_indices, color_indices)
    class(nlo_color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in), dimension(:) :: flavor_indices, color_indices
    integer :: i, i_born, n_flv
    integer, dimension(N_MAX_FLV) :: checked_uborns
    integer, dimension(:), allocatable :: i_flv, i_limit
    type(color_index_list_t), dimension(:), allocatable :: i_col
    call evaluate_flavors (n_flv, i_flv, i_limit)
    call evaluate_colors (n_flv, i_flv, i_limit, i_col)
    checked_uborns = 0
    do i = 1, size (i_flv)
       i_born = reg_data%map_real_to_born_index (i_flv(i))
       if (.not. any (checked_uborns == i_born)) then
          if (.not. allocated (color_data%included_color_structures)) then
             allocate (color_data%included_color_structures (size (i_col(i)%col)))
             color_data%included_color_structures = i_col(i)%col
          end if
          checked_uborns(i) = i_born
       end if
    end do
  contains
    subroutine evaluate_flavors (n_flv, i_flavor, i_limit)
      integer, intent(out) :: n_flv
      integer, intent(out), dimension(:), allocatable :: i_flavor
      integer, intent(out), dimension(:), allocatable :: i_limit
      integer :: i, current_flavor
      n_flv = 0; current_flavor = 0
      do i = 1, size (flavor_indices)
         if (flavor_indices (i) /= current_flavor) then
            n_flv = n_flv + 1
            current_flavor = flavor_indices(i)
         end if
      end do
      allocate (i_flavor (n_flv), i_limit (n_flv))
      current_flavor = 0; i_limit(1) = 1
      do i = 1, size (flavor_indices)
         if (flavor_indices(i) /= current_flavor) then
           i_limit(current_flavor + 1) = i
           current_flavor = flavor_indices(i)
           i_flavor(current_flavor) = current_flavor
         end if
      end do
    end subroutine evaluate_flavors

    subroutine evaluate_colors (n_flv, i_flavor, col_limit, i_color)
      integer, intent(in) :: n_flv
      integer, intent(in), dimension(:) :: i_flavor, col_limit
      type(color_index_list_t), intent(out), dimension(:), allocatable :: i_color
      integer :: i, j, k, col_first
      integer :: count_n_col
      allocate (i_color (n_flv))
      j = 1
      do i = 1, n_flv
         col_first = color_indices(col_limit (i))
         count_n_col = 1
         k = col_limit(i) + 1
         do while (color_indices(k) /= col_first)
            count_n_col = count_n_col + 1
            k = k + 1
         end do
         allocate (i_color(i)%col(count_n_col))
      end do
      do i = 1, n_flv
         j = 1
         col_first = color_indices(col_limit (i))
         i_color(i)%col(1) = color_indices (col_limit(i))
         k = col_limit(i) + 1
         do while (color_indices(k) /= col_first)
            j = j + 1
            i_color(i)%col(j) = color_indices (k)
            k = k + 1
         end do
      end do
    end subroutine evaluate_colors

  end subroutine nlo_color_data_prepare

  subroutine nlo_color_data_init_color_matrix (color_data, reg_data, prc_constants, &
     factorization_mode)
    class(nlo_color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    integer, intent(in) :: factorization_mode
    if (debug_active (D_SUBTRACTION))  call show_input_values ()
    call prc_constants(1)%get_col_state (color_data%col_state_born)
    call prc_constants(2)%get_col_state (color_data%col_state_real)
    call prc_constants(2)%get_cf_index (color_data%cf_index_real)
    call prc_constants(1)%get_color_factors (color_data%color_factors_born)
    color_data%n_col_born = size (color_data%col_state_born (1, 1, :))
    color_data%n_col_real = size (color_data%col_state_real (1 ,1, :))
    call color_data%init_ghost_flags (prc_constants)
    call color_data%init_color (reg_data)
    call color_data%check_equivalences ()
    call color_data%compute_betaij (reg_data, factorization_mode)
  contains
    subroutine show_input_values()
      integer :: i
      call msg_debug (D_SUBTRACTION, "color_data_init")
      call msg_debug (D_SUBTRACTION, "factorization_mode", factorization_mode)
      do i = 1, size(color_data%included_color_structures)
         call msg_debug (D_SUBTRACTION, "included_color_structures(i)", &
            color_data%included_color_structures(i))
      end do
    end subroutine show_input_values
  end subroutine nlo_color_data_init_color_matrix

  subroutine nlo_color_data_init_ghost_flags (color_data, prc_constants)
    class(nlo_color_data_t), intent(inout) :: color_data
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    call prc_constants(1)%get_ghost_flag (color_data%ghost_flag_born)
    call prc_constants(2)%get_ghost_flag (color_data%ghost_flag_real)
  end subroutine nlo_color_data_init_ghost_flags

  subroutine nlo_color_data_check_equivalences (color_data)
    class(nlo_color_data_t), intent(inout) :: color_data
    integer :: i_col1, i_col2
    if (allocated (color_data%equivalent_color_up_to_sign))  &
         deallocate (color_data%equivalent_color_up_to_sign)
    allocate (color_data%equivalent_color_up_to_sign (color_data%n_col_born))
    color_data%equivalent_color_up_to_sign (1) = 1
    do i_col1 = 1, color_data%n_col_born
       do i_col2 = 1, i_col1 - 1
          if (is_equivalent (color_data%col_state_born (:, :, i_col1), &
             color_data%col_state_born (:, :, i_col2))) then
             color_data%equivalent_color_up_to_sign (i_col1) = i_col2
          else
             color_data%equivalent_color_up_to_sign (i_col1) = i_col1
          end if
       end do
    end do
  contains
    function is_equivalent (col1, col2) result (equiv)
      logical :: equiv
      integer, intent(in), dimension(:,:) :: col1, col2
      integer :: leg
      integer, dimension(2) :: c1, c2, c3
      equiv = .true.
      do leg = 1, size (col1, dim = 2)
         c1 = col1(:, leg); c2 = col2(:, leg)
         c3(1) = -c2(2); c3(2) = -c2(1)
         equiv = equiv .and. (all (c1 == c2) .or. all (c1 == c3))
         if (.not. equiv) exit
      end do
    end function is_equivalent
  end subroutine nlo_color_data_check_equivalences

  subroutine nlo_color_data_init_color (color_data, reg_data)
    class(nlo_color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(in) :: reg_data
    integer :: i
    call msg_debug (D_SUBTRACTION, "nlo_color_data_init_color")
    if (allocated (color_data%color_real))  &
         deallocate (color_data%color_real)
    if (allocated (color_data%icm))  deallocate (color_data%icm)
    allocate (color_data%color_real (reg_data%n_legs_real, &
         color_data%n_col_real))
    allocate (color_data%icm (reg_data%n_regions))
    do i = 1, color_data%n_col_real
      call color_init_from_array (color_data%color_real (:, i), &
           color_data%col_state_real (:, :, i), &
           color_data%ghost_flag_real (:, i))
      call color_data%color_real (1 : reg_data%n_in, i)%invert ()
    end do
    do i = 1, reg_data%n_regions
      call color_data%icm(i)%init
      associate (region => reg_data%regions(i))
        if (debug_active (D_SUBTRACTION)) then
           print *, "index i = ", i
           print *, "creating map"
        end if
        call color_data%icm(i)%create_map (region%flst_real, region%emitter, &
             region%ftuples, color_data%col_state_born, &
             color_data%col_state_real, color_data%included_color_structures)
      end associate
    end do
  end subroutine nlo_color_data_init_color

  subroutine nlo_color_data_compute_betaij (color_data, reg_data, factorization_mode)
    class(nlo_color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: factorization_mode
    integer :: alr, i_born
    logical, dimension(reg_data%n_flv_born) :: checked_uborn
    if (debug_active (D_SUBTRACTION)) then
       call msg_print_color ("nlo_color_data_compute_betaij", COL_AQUA)
       call msg_print_color ("factorization_mode", factorization_mode, COL_AQUA)
    end if
    if (allocated (color_data%beta_ij)) &
         deallocate (color_data%beta_ij)
    allocate (color_data%beta_ij (reg_data%n_legs_born, &
         reg_data%n_legs_born, reg_data%n_flv_born))
    select case (factorization_mode)
    case (NO_FACTORIZATION)
       call msg_debug (D_SUBTRACTION, "No factorization")
       checked_uborn = .false.
       do alr = 1, reg_data%n_regions
          i_born = reg_data%regions(alr)%uborn_index
          if (checked_uborn (i_born)) cycle
          call color_data%fill_betaij_matrix (reg_data%n_legs_born, i_born, &
             reg_data%regions(alr)%flst_real, reg_data)
          checked_uborn (i_born) = .true.
       end do
    case (FACTORIZATION_THRESHOLD)
       call msg_debug (D_SUBTRACTION, "Threshold factorization")
       call color_data%fill_betaij_matrix_threshold ()
    end select
  end subroutine nlo_color_data_compute_betaij

  subroutine nlo_color_data_fill_betaij_matrix &
       (color_data, n_legs, uborn_index, flst_real, reg_data)
    class(nlo_color_data_t), intent(inout) :: color_data
    integer, intent(in) :: n_legs, uborn_index
    type(flv_structure_t), intent(in) :: flst_real
    type(region_data_t), intent(inout) :: reg_data
    integer :: em1, em2
    if (debug_active (D_SUBTRACTION)) then
       print *, "FILLING BETA_IJ MATRIX"
       print *, "n_legs = ", n_legs
       print *, "uborn_index = ", uborn_index
    end if
    associate (flv_born => reg_data%flv_born (uborn_index))
       do em1 = 1, n_legs
          do em2 = 1, n_legs
             if (debug2_active (D_SUBTRACTION)) then
                print *, "em1 = ", em1, " em2 = ", em2
                print *, "col1/2 = ", flv_born%colored(em1), flv_born%colored(em2)
             end if
             if (flv_born%colored(em1) .and. flv_born%colored(em2)) then
                if (em1 < em2) then
                   if (debug2_active (D_SUBTRACTION)) then
                      print *, "calling compute_bij for em1 < em2"
                   end if
                   color_data%beta_ij (em1, em2, uborn_index) &
                      = color_data%compute_bij &
                      (reg_data, uborn_index, flst_real, em1, em2)
                else if (em1 > em2) then
                   !!! B_ij is symmetric
                   color_data%beta_ij (em1, em2, uborn_index) = &
                      color_data%beta_ij (em2, em1, uborn_index)
                else
                   if (is_quark (abs (flv_born%flst (em1)))) then
                       color_data%beta_ij (em1, em2, uborn_index) = - cf
                   else
                      color_data%beta_ij (em1, em2, uborn_index) = - ca
                   end if
                end if
             else
                color_data%beta_ij (em1, em2, uborn_index) = zero
             end if
          end do
       end do
    end associate
    color_data%beta_ij_evaluated = .true.
    call check_color_conservation (color_data%beta_ij (:, :, uborn_index), &
         n_legs, color_data%color_is_conserved)
    if (debug_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "nlo_color_data_fill_betaij_matrix")
       do em1 = 1, n_legs
          do em2 = 1, n_legs
             print *, 'em1, em2, color_data%beta_ij(em1,em2,uborn_index) = ', &
                  em1, em2, color_data%beta_ij(em1, em2, uborn_index)
          end do
       end do
    end if
  contains
    subroutine check_color_conservation (bij_matrix, n_legs, success)
      real(default), intent(in), dimension(:,:) :: bij_matrix
      integer, intent(in) :: n_legs
      logical, intent(out) :: success
      logical, dimension(:), allocatable :: check
      integer :: i, j
      real(default) :: bcheck
      real(default), parameter :: tol = 0.0001_default
      allocate (check (n_legs))
      do i = 1, n_legs
         bcheck = 0.0
         do j = 1, n_legs
            if (i /= j) bcheck = bcheck + bij_matrix (i, j)
         end do
         if (is_quark (abs(flst_real%flst (i))) .or. &
             is_gluon (flst_real%flst (i))) then
            if (is_quark (abs(flst_real%flst (i))) .and. &
                 (bcheck - cf) < tol) then
               check (i) = .true.
            else if (is_gluon (flst_real%flst (i)) .and. &
                 (bcheck - ca) < tol) then
               check (i) = .true.
            else
               check (i) = .false.
            end if
         else
            if (bcheck < tol) then
               check (i) = .true.
            else
               check (i) = .false.
            end if
         end if
      end do
      success = all (check)
    end subroutine check_color_conservation
  end subroutine nlo_color_data_fill_betaij_matrix

  subroutine nlo_color_data_fill_betaij_matrix_threshold (color_data)
    class(nlo_color_data_t), intent(inout) :: color_data
    integer :: i, j
    color_data%beta_ij = zero
    associate (beta_ij => color_data%beta_ij (:,:,1))
       do i = 1, 4
          beta_ij (i,i) = -CF
       end do
       beta_ij (1,2) = CF; beta_ij (2,1) = CF
       beta_ij (3,4) = CF; beta_ij (4,3) = CF
    end associate
    if (debug_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "nlo_color_data_fill_betaij_matrix_threshold")
       do i = 1, size(color_data%beta_ij, dim=1)
          do j = 1, size(color_data%beta_ij, dim=1)
             print *, 'i, j, color_data%beta_ij(i,j,1) = ', &
                  i, j, color_data%beta_ij(i,j,1)
          end do
       end do
    end if
  end subroutine nlo_color_data_fill_betaij_matrix_threshold

  function nlo_color_data_compute_bij (color_data, reg_data, uborn_index, &
      flst_real, em1, em2) result (bij)
    real(default) :: bij
    class(nlo_color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: uborn_index
    type(flv_structure_t), intent(in) :: flst_real
    integer, intent(in) :: em1, em2
    logical, dimension(:,:), allocatable :: cf_present
    type(singular_region_t), dimension(2,100) :: reg
    integer ::  i, j, k, l
    integer :: alr, n_alr
    type(ftuple_color_map_t) :: icm1, icm2
    integer :: i1, i2
    real(default) :: color_factor, color_factor_born
    integer, dimension(2) :: i_reg
    logical , dimension(2) :: found
    integer, dimension(2,100) :: map_em_col_tmp
    integer, dimension(:), allocatable :: map_em_col1, map_em_col2
    integer, dimension(2) :: col1, col2
    integer, dimension(:), allocatable :: iarray1, iarray2
    integer, dimension(:), allocatable :: iisec1, iisec2
    integer :: sign
    integer, dimension(:), allocatable :: uborn_group, em1_group, em2_group
    integer, dimension(:), allocatable :: compute_alrs
    if (debug_active (D_SUBTRACTION)) then
       call color_data%write (verbose = .true.)       
       print *, "em1 = ", em1
       print *, "em2 = ", em2
    end if
    bij = zero
    color_factor = zero; color_factor_born = zero
    found = .false.
    allocate (uborn_group (reg_data%get_uborn_group_size (uborn_index)))
    if (debug_active (D_SUBTRACTION)) &
         print *, "size (uborn_group) = ", size (uborn_group)
    uborn_group = reg_data%get_uborn_group (uborn_index)
    if (debug_active (D_SUBTRACTION)) &
         print *, "uborn_group = ", uborn_group
    allocate (em1_group (reg_data%get_emitter_group_size (em1)))
    em1_group = reg_data%get_emitter_group (em1)
    if (debug_active (D_SUBTRACTION)) &
         print *, "em1_group = ", em1_group
    allocate (em2_group (reg_data%get_emitter_group_size (em2)))
    em2_group = reg_data%get_emitter_group (em2)
    if (debug_active (D_SUBTRACTION)) &
         print *, "em2_group = ", em2_group
    n_alr = 0
    do i = 1, size (uborn_group)
       n_alr = n_alr + count (uborn_group(i) == em1_group) &
            + count (uborn_group(i) == em2_group)
    end do
    if (n_alr == 0) return
    allocate (compute_alrs (n_alr))
    compute_alrs = &
       pack (uborn_group, [(any (uborn_group(i) == em1_group) .or. any (uborn_group(i) == em2_group), &
       i = 1, size (uborn_group))])
    !!! Include distinction between Born flavors
    do i = 1, size (color_data%color_factors_born)
       if (color_data%equivalent_color_up_to_sign (i) == i) &
          color_factor_born = color_factor_born + real (color_data%color_factors_born (i))
    end do
    i1 = 1; i2 = 1
    if (debug_active (D_SUBTRACTION)) &
         print *, "Number of color flows in the real = ", color_data%n_col_real
    do i = 1, color_data%n_col_real
       if (any (color_data%included_color_structures == i)) then
          col1 = color_data%col_state_real (:, em1, i)
          col2 = color_data%col_state_real (:, reg_data%n_legs_real, i)
          if (share_line (col1, col2)) then
             map_em_col_tmp(1, i1) = i
             i1 = i1 + 1
          end if
          col1 = color_data%col_state_real (:, em2, i)
          if (share_line (col1, col2)) then
             map_em_col_tmp(2, i2) = i
             i2 = i2 + 1
          end if
       end if
    end do
    if (debug_active (D_SUBTRACTION)) then
       print *, "i1 = ", i1
       print *, "i2 = ", i2   
    end if
    allocate (map_em_col1 (i1), map_em_col2 (i2))
    map_em_col1 = map_em_col_tmp (1, 1 : i1 - 1)
    map_em_col2 = map_em_col_tmp (2, 1 : i2 - 1)

    i_reg = 1

    if (debug_active (D_SUBTRACTION)) &
         print *, "Number of regions = ", reg_data%n_regions
    
    do alr = 1, reg_data%n_regions
        if (.not. any (compute_alrs == alr)) cycle
        if (em1 == reg_data%regions(alr)%emitter .or. &
           (em1 <= 2 .and. reg_data%regions(alr)%emitter == 0)) then
           reg(1, i_reg(1)) = reg_data%regions(alr)
           i_reg(1) = i_reg(1) + 1
           found(1) = .true.
        end if
        if (em2 == reg_data%regions(alr)%emitter .or. &
           (em2 <= 2 .and. reg_data%regions(alr)%emitter == 0)) then
           reg(2, i_reg(2)) = reg_data%regions(alr)
           i_reg(2) = i_reg(2) + 1
           found(2) = .true.
        end if
    end do
    if (.not. (found(1) .and. found(2))) then
       return
    end if

    if (debug_active (D_SUBTRACTION)) &
         print *, "i_reg = ", i_reg
    
    do i = 1, i_reg(1) - 1
       do j = 1, i_reg(2) - 1
          if (debug_active (D_SUBTRACTION)) then
             print *, "i=", i, " reg(1,i)%alr = ", reg(1,i)%alr
             print *, "i=", i, " reg(2,i)%alr = ", reg(2,i)%alr
          end if
          icm1 = color_data%icm (reg(1, i)%alr)
          icm2 = color_data%icm (reg(2, j)%alr)

          allocate (iarray1 (size (icm1%get_index_array ())))
          allocate (iarray2 (size (icm2%get_index_array ())))

          if (debug_active (D_SUBTRACTION)) then
             print *, "iarray1 = ", iarray1
             print *, "iarray2 = ", iarray2
          end if
                  
          iarray1 = icm1%get_index_array ()
          iarray2 = icm2%get_index_array ()

          allocate (iisec1 (count (iarray1 == map_em_col1)))
          allocate (iisec2 (count (iarray2 == map_em_col2)))

          iisec1 = pack (iarray1, [ (any(iarray1(i) == map_em_col1), &
               i = 1, size(iarray1)) ])
          iisec2 = pack (iarray2, [ (any(iarray2(i) == map_em_col2), &
               i = 1, size(iarray2)) ])

          allocate (cf_present (size (color_index_present &
             (color_data%cf_index_real), 1), size (color_index_present &
             (color_data%cf_index_real), 2)))

          cf_present = color_index_present (color_data%cf_index_real)

          do k = 1, size (iisec1)
             do l = 1, size (iisec2)
                i1 = iisec1(k)
                i2 = iisec2(l)
                if (cf_present (i1, i2)) then
                   if (is_gluon (flst_real%flst (em1)) .or. &
                      is_gluon (flst_real%flst (em2))) then
                      sign = get_sign (color_data%col_state_real (:, :, i1)) * &
                         get_sign (color_data%col_state_real (:, :, i2))
                   else
                      sign = 1
                   end if
                   color_factor = color_factor + sign * compute_color_factor &
                      (color_data%color_real(:, i1), &
                      color_data%color_real(:, i2))
                end if
             end do
          end do
       deallocate (iarray1, iarray2, iisec1, iisec2, cf_present)
       end do
    end do
    !!! The real color factor always differs from the Born one
    !!! by one vertex factor. Thus, apply the factor 1/2
    bij = color_factor / (two * color_factor_born)

  contains
    function share_line (col1, col2) result (share)
      integer, intent(in), dimension(2) :: col1, col2
      logical :: share
      logical :: id1, id2, id3
      id1 = (abs(col1(1)) == abs(col2(1)) .and. col1(1) /= 0) .or. &
            (abs(col1(2)) == abs(col2(2)) .and. col1(2) /= 0)
      id2 = (abs(col1(1)) == abs(col2(2)) .and. col1(1) /= 0) .or. &
            (abs(col1(2)) == abs(col2(1)) .and. col1(2) /= 0)
      id3 = col2(1) == 0 .and. col2(2) == 0
      if (id1 .or. id2 .or. id3) then
        share = .true.
      else
        share = .false.
      end if
    end function share_line

    function get_sign (col) result (sign)
      integer, intent(in), dimension(:,:) :: col
      integer :: sign
      integer, dimension(:), allocatable :: iref, iperm
      integer :: iref1, iperm1
      integer :: n, i, i_first, j
      integer :: i1, i2
      integer :: p1, p2
      p1 = 2; p2 = 2
      iref1 = 0; iperm1 = 0; i_first = 0; i1 = 0
      do i = 1, size(col(1,:))
        if (.not. all (col(:,i) == 0)) then
          if (col(1,i) == 0) then
            i1 = col(2,i)
            iref1 = i; iperm1 = i
            i_first = i
          else
            i1 = col(1,i)
            iref1 = i; iperm1 = i
            i_first = i
          end if
          exit
        end if
      end do
      if (iref1 == 0 .or. iperm1 == 0 .or. i_first == 0) &
         call msg_fatal ("Invalid color structure")
      n = size(col(1,:)) - i_first + 1
      allocate (iref(n), iperm(n))
      iref(1) = iref1; iperm(1) = iperm1
      do i = i_first+1, size(col(1,:))
        if (all (col(:,i) == 0)) cycle
        if (i == size(col(1,:))) then
          iref(p1) = i_first + 1
        else
          iref(p1) = i + 1
          p1 = p1 + 1
        end if
        do j = i_first+1, size(col(1,:))
          if (col(1,j) == -i1) then
            i1 = col(2,j)
            iperm(p2) = j
            p2 = p2 + 1
            exit
          else if (col(2,j) == -i1) then
            i1 = col(1,j)
            iperm(p2) = j
            p2 = p2 + 1
            exit
          end if
        end do
      end do
      sign = 1
      do i = 1, n
        if (iperm(i) == iref(i)) then
          cycle
        else
          do j = i+1, n
            if (iperm(j) == iref(i)) then
              i1 = j
              exit
            end if
          end do
          i2 = iperm(i)
          iperm(i) = iperm(i1)
          iperm(i1) = i2
          sign = -sign
        end if
      end do
    end function get_sign

    function color_index_present (cf_index) result (cf_present)
      integer, intent(in), dimension(:,:), allocatable :: cf_index
      logical, dimension(:,:), allocatable :: cf_present
      integer :: n_col
      integer :: c, i1, i2
      n_col = size (cf_index(1,:))
      allocate (cf_present (n_col, n_col))
      cf_present = .false.
      do c = 1, n_col
        i1 = cf_index (1, c)
        i2 = cf_index (2, c)
        cf_present (i1, i2) = .true.
        if (i1 /= i2) cf_present(i2, i1) = .true.
      end do
    end function color_index_present
  end function nlo_color_data_compute_bij

  subroutine nlo_color_data_write (color_data, unit, verbose)
    class(nlo_color_data_t), intent(in) :: color_data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i, i1, i2
    integer :: n_legs, n_up
    logical :: print_second_off_diagonal
    u = given_output_unit (unit); if (u < 0) return
    print_second_off_diagonal = .false.
    if (present (verbose)) print_second_off_diagonal = verbose
    n_legs = size (color_data%beta_ij, dim = 2)
    n_up = n_legs
    write (u, "(1x,A)") "Color information: "
    write (u, "(1x,A,1x,I1)") "Number of Born color states: ", &
         color_data%n_col_born
    write (u, "(1x,A,1x,I1)") "Number of real color states: ", &
         color_data%n_col_real
    if (.not. color_data%beta_ij_evaluated) return
    write (u, "(1x,A)") "Color correlation: "
    do i = 1, size (color_data%beta_ij, dim = 3)
      write (u, "(1x,A,1x,I1)") "State nr. ", i
      write (u, "(1x,A)") "-------------"
      write (u, "(1x,A,1x,A,1x,A)") "i1", "i2", "color factor"
      do i1 = 1, n_legs
        if (.not. print_second_off_diagonal) n_up = i1
        do i2 = 1, n_up
          write (u, "(1x,I1,1x,I1,1x,F5.2)") &
               i1, i2, color_data%beta_ij (i1, i2, i)
        end do
      end do
      write (u, "(1x,A)") "========================================"
    end do
    if (color_data%color_is_conserved) then
      write (u, "(1x,A)") "Color is conserved."
    else
      write (u, "(1x,A)") "Fatal error: Color conversation is violated."
    end if
  end subroutine nlo_color_data_write


end module nlo_color_data
