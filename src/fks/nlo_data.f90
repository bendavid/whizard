! WHIZARD 2.2.4 Feb 06 2015
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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: pi, twopi
  use unit_tests
  use diagnostics
  use physics_defs
  use process_constants !NODEP!
  use sm_physics
  use os_interface
  use model_data
  use parser
  use pdg_arrays
  use particle_specifiers
  use phs_single
  use state_matrices
  use interactions
  use lorentz
  use prc_core
  use pdg_arrays
  use sf_base
  use colors
  use flavors
  use fks_regions
  use phs_fks
  use virtual
  use real_subtraction

  implicit none
  private

  public :: fks_template_t
  public :: nlo_data_t
  public :: nlo_pointer_t

  type :: fks_template_t
    integer :: mapping_type
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
  contains
    procedure :: write => fks_template_write
    procedure :: set_dij_exp => fks_template_set_dij_exp
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
  end type fks_template_t

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

  type color_data_t
    type(ftuple_color_map_t), dimension(:), allocatable :: icm
    integer, dimension(:,:,:), allocatable :: col_state_born, col_state_real
    logical, dimension(:,:), allocatable :: ghost_flag_born, ghost_flag_real
    integer :: n_col_born, n_col_real
    type(color_t), dimension(:,:), allocatable :: color_real, color_born
    integer, dimension(:), allocatable :: col_born
    complex(default), dimension(:), allocatable :: color_factors_born
    integer, dimension(:,:), allocatable :: cf_index_real
    real(default), dimension(:,:,:), allocatable :: beta_ij
    logical :: color_is_conserved
  contains
    procedure :: init => color_data_init
    procedure :: init_betaij => color_data_init_betaij
    procedure :: fill_betaij_matrix => color_data_fill_betaij_matrix
    procedure :: compute_bij => color_data_compute_bij
    procedure :: write => color_data_write
  end type color_data_t

  type :: nlo_data_t
    type(string_t) :: nlo_type
    type(region_data_t) :: reg_data
    integer :: n_alr
    integer :: n_in
    integer :: n_out_born
    integer :: n_out_real
    integer :: n_allowed_born
    integer :: n_flv_born 
    integer :: n_flv_real
    integer :: active_emitter
    complex(default), dimension(:), allocatable :: amp_born
    type(color_data_t) :: color_data
    type(real_kinematics_t) :: real_kinematics
    type(virtual_t) :: virtual_terms
    type(real_subtraction_t) :: real_terms
    integer, dimension(:,:), allocatable :: flv_state_born
    integer, dimension(:,:), allocatable :: flv_state_real
    integer, dimension(:), allocatable :: flv_born
    integer, dimension(:), allocatable :: hel_born
    integer, dimension(:), allocatable :: col_born
    real(default) :: alpha_s_born
    logical :: alpha_s_born_set
    real(default), dimension(:), allocatable :: sqme_born
    real(default), dimension(:), allocatable :: sqme_real_non_sub
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    complex(default) :: me_sc
    real(default), public :: sqme_real
    real(default), public :: sqme_virt
    type(interaction_t), public :: int_born
    type(kinematics_counter_t), public :: counter
    logical, public :: counter_exists = .false.
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
  contains
    procedure :: compute_sqme_real_fin => nlo_data_compute_sqme_real_fin
    procedure :: has_massive_emitter => nlo_data_has_massive_emitter
    procedure :: init => nlo_data_init
    procedure :: copy_matrix_elements => nlo_data_copy_matrix_elements
    procedure :: write => nlo_data_write
    procedure :: init_born_amps => nlo_data_init_born_amps
    procedure :: set_internal_procedures => nlo_data_set_internal_procedures
    procedure :: init_virtual => nlo_data_init_virtual
    procedure :: set_nlo_type => nlo_data_set_nlo_type
    procedure :: get_nlo_type => nlo_data_get_nlo_type 
    procedure :: get_emitters => nlo_data_get_emitters
    procedure :: set_active_emitter => nlo_data_set_active_emitter
    procedure :: get_active_emitter => nlo_data_get_active_emitter
    procedure :: set_flv_born => nlo_data_set_flv_born
    procedure :: set_hel_born => nlo_data_set_hel_born
    procedure :: set_col_born => nlo_data_set_col_born
    procedure :: set_alpha_s_born => nlo_data_set_alpha_s_born
    procedure :: init_real_kinematics => nlo_data_init_real_kinematics
    procedure :: set_real_kinematics => nlo_data_set_real_kinematics 
    procedure :: get_real_kinematics => nlo_data_get_real_kinematics
    procedure :: set_real_momenta => nlo_data_set_real_momenta 
    procedure :: get_real_momenta => nlo_data_get_real_momenta
    procedure :: set_y_soft => nlo_data_set_y_soft
    procedure :: set_jacobian => nlo_data_set_jacobian
    procedure :: compute_virt => nlo_data_compute_virt
    procedure :: requires_spin_correlation => &
                    nlo_data_requires_spin_correlation
  end type nlo_data_t

  type :: nlo_pointer_t
    type(nlo_data_t), public, pointer :: nlo_data => null ()
  end type nlo_pointer_t


contains
 
  subroutine fks_template_write (object, unit)
    class(fks_template_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,'(1x,A)') 'FKS Template: '
    write (u,'(1x,A,I0)') 'Mapping Type: ', object%mapping_type
    write (u,'(1x,A,ES4.3,ES4.3)') 'd_ij exponentials: ', object%fks_dij_exp1, object%fks_dij_exp2
  end subroutine fks_template_write

  subroutine fks_template_set_dij_exp (object, exp1, exp2)
    class(fks_template_t), intent(inout) :: object
    real(default), intent(in) :: exp1, exp2
    object%fks_dij_exp1 = exp1
    object%fks_dij_exp2 = exp2
  end subroutine fks_template_set_dij_exp

  subroutine fks_template_set_mapping_type (object, val)
    class(fks_template_t), intent(inout) :: object
    integer, intent(in) :: val
    object%mapping_type = val
  end subroutine fks_template_set_mapping_type

  subroutine fks_template_set_counter (object)
    class(fks_template_t), intent(inout) :: object
    object%count_kinematics = .true.
  end subroutine fks_template_set_counter

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
    select type (icm)
    type is (ftuple_color_map_t)
    current => icm
    pres = .false.
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
    select type (icm)
    type is (ftuple_color_map_t)
    current => icm
    n_entries = 0
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
    select type (icm)
    type is (ftuple_color_map_t)
    n_entries = icm%get_n_entries ()
    allocate (iarr(n_entries))
    do i = 1, n_entries
      if (i == 1) then
        current => icm
      else
        current => current%next
      end if
      iarr(i) = current%color_index
    end do
    end select
  end function ftuple_color_map_get_index_array

  function ftuple_color_map_get_entry (icm, index) result(entry)
    class(ftuple_color_map_t), intent(in), target :: icm
    integer, intent(in) :: index
    integer :: entry
    type(ftuple_color_map_t), pointer :: current
    integer :: i
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
    else
      entry = 0
    end if
    end select 
  end function ftuple_color_map_get_entry

  recursive subroutine ftuple_color_map_create_map (icm, flst, &
       emitter, allreg, color_states_born, color_states_real, p_rad_in)
    class(ftuple_color_map_t), intent(inout) :: icm
    type(flv_structure_t), intent(in) :: flst
    integer, intent(in) :: emitter
    type(ftuple_t), intent(in), dimension(:) :: allreg
    integer, intent(in), dimension(:,:,:) :: color_states_born
    integer, intent(in), dimension(:,:,:) :: color_states_real
    integer, intent(in), optional :: p_rad_in
    integer :: nreg, region
    integer :: p1, p2, p_rad
    integer :: flv_em, flv_rad
    integer :: n_col_real, n_col_born
    integer, dimension(2) :: col_em, col_rad
    integer :: i
    !!! splitting type: 1 - q -> qg
    !!!                 2 - g -> qq
    !!!                 3 - g -> gg
    integer :: splitting_type_flv, splitting_type_col
    nreg = size (allreg)
    n_col_real = size (color_states_real (1,1,:))
    n_col_born = size (color_states_born (1,1,:))
    do region = 1, nreg
      call allreg(region)%get (p1, p2)
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
        else
          call icm%create_map &
               (flst, 1, allreg, color_states_born, color_states_real, p_rad)
          call icm%create_map &
               (flst, 2, allreg, color_states_born, color_states_real, p_rad)
        end if
        flv_rad = flst%flst (p_rad)
        if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
           splitting_type_flv = 1
        else if (is_quark (abs(flv_em)) .and. flv_em + flv_rad == 0) then
           splitting_type_flv = 2
        else if (is_gluon (flv_em) .and. is_gluon (flv_rad)) then
           splitting_type_flv = 3
        else 
          splitting_type_flv = 0
        end if 
        do i = 1, n_col_real
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
          if (splitting_type_flv == splitting_type_col .and. &
              splitting_type_flv /= 0) then
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

  subroutine color_data_init (color_data, reg_data, prc_constants)
    class(color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    type(process_constants_t), intent(inout), dimension(2) :: prc_constants
    integer :: nlegs_born, nlegs_real
    integer :: i
    nlegs_born = reg_data%nlegs_born
    nlegs_real = reg_data%nlegs_real
    call prc_constants(1)%get_col_state (color_data%col_state_born)
    call prc_constants(2)%get_col_state (color_data%col_state_real)
    call prc_constants(2)%get_cf_index (color_data%cf_index_real)
    call prc_constants(1)%get_color_factors (color_data%color_factors_born)
    color_data%n_col_born = size (color_data%col_state_born(1,1,:))
    color_data%n_col_real = size (color_data%col_state_real(1,1,:))
    color_data%ghost_flag_born = prc_constants(1)%get_ghost_flag ()
    color_data%ghost_flag_real = prc_constants(2)%get_ghost_flag ()
    allocate (color_data%color_real (nlegs_real, color_data%n_col_real))
    allocate (color_data%icm (reg_data%n_regions))
    do i = 1, color_data%n_col_real
      call color_init_from_array (color_data%color_real (:,i), &
           color_data%col_state_real (:,:,i), &
           color_data%ghost_flag_real (:,i))
    end do
    do i = 1, size(reg_data%regions)
      call color_data%icm(i)%init
      associate (region => reg_data%regions(i))
        call color_data%icm(i)%create_map (region%flst_real, region%emitter, &
             region%flst_allreg, color_data%col_state_born, &
             color_data%col_state_real)
      end associate
    end do
    call color_data%init_betaij (reg_data)
  end subroutine color_data_init

  subroutine color_data_init_betaij (color_data, reg_data)
    class(color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    integer :: i
    allocate (color_data%beta_ij (reg_data%nlegs_born, &
         reg_data%nlegs_born, reg_data%n_flv_born))
    do i = 1, reg_data%n_flv_born
      call color_data%fill_betaij_matrix (reg_data%nlegs_born, i, &
           reg_data%regions(1)%flst_real, reg_data)
    end do
  end subroutine color_data_init_betaij

  subroutine color_data_fill_betaij_matrix &
       (color_data, n_legs, uborn_index, flst_real, reg_data)
    class(color_data_t), intent(inout) :: color_data
    integer, intent(in) :: n_legs, uborn_index
    type(flv_structure_t), intent(in) :: flst_real
    type(region_data_t), intent(inout) :: reg_data
    integer :: em1, em2
    associate (flv_born => reg_data%flv_born (uborn_index))
    do em1 = 1, n_legs
      do em2 = 1, n_legs
        if (flv_born%colored(em1) .and. flv_born%colored(em2)) then
          if (em1 < em2) then
             color_data%beta_ij (em1, em2, uborn_index) &
                = color_data%compute_bij &
                     (reg_data, uborn_index, flst_real, em1, em2)
          else if (em1 > em2) then
             !!! B_ij is symmetric
             color_data%beta_ij (em1, em2, uborn_index) = &
                  color_data%beta_ij (em2, em1, uborn_index)
          else
            if (is_quark (abs (flv_born%flst (em1)))) then
              color_data%beta_ij (em1, em2, uborn_index) = -cf
            else
              color_data%beta_ij (em1, em2, uborn_index) = -ca
            end if
          end if
        else
          color_data%beta_ij (em1, em2, uborn_index) = 0.0
        end if
      end do
    end do
    end associate
    call check_color_conservation (color_data%beta_ij (:,:,uborn_index), &
         n_legs, color_data%color_is_conserved)
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
      if (.not. all (check)) then
        success = .false.
        ! call msg_fatal ("Color conservation violated!") 
      else
        success = .true.
      end if
    end subroutine check_color_conservation
  end subroutine color_data_fill_betaij_matrix

  function color_data_compute_bij &
       (color_data, reg_data, uborn_index, flst_real, em1, em2) result (bij)
    class(color_data_t), intent(inout) :: color_data
    type(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: uborn_index
    type(flv_structure_t), intent(in) :: flst_real
    integer, intent(in) :: em1, em2
    real(default) :: bij
    logical, dimension(:,:), allocatable :: cf_present
    type(singular_region_t), dimension(2,100) :: reg
    integer ::  i, j, k, l
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
    color_factor = 0.0; color_factor_born = 0.0
    found = .false.
    !!! Include distinction between Born flavors
    do i = 1, size (color_data%color_factors_born)
      color_factor_born = color_factor_born + color_data%color_factors_born (i)
    end do
    i1 = 1
    i2 = 1
    !!! Catch case em = 0
    if (em1 == 0 .or. em2 == 0) then
       !!! What to do?
       bij = 0.0
    else
       do i = 1, color_data%n_col_real
          col1 = color_data%col_state_real (:, em1, i)
          col2 = color_data%col_state_real (:, reg_data%nlegs_real, i)
          if (share_line (col1, col2)) then
             map_em_col_tmp(1,i1) = i
             i1 = i1+1
          end if
          col1 = color_data%col_state_real (:, em2, i)
          if (share_line (col1, col2)) then 
             map_em_col_tmp(2,i2) = i
             i2 = i2 + 1
          end if
       end do
       allocate (map_em_col1 (i1), map_em_col2 (i2))
       map_em_col1 = map_em_col_tmp (1,1:i1-1)
       map_em_col2 = map_em_col_tmp (2,1:i2-1)

       i_reg = 1
    
       do i = 1, reg_data%n_regions
           if (uborn_index == reg_data%regions(i)%uborn_index) then
             if (em1 == reg_data%regions(i)%emitter .or. &
                 (em1 <= 2 .and. reg_data%regions(i)%emitter == 0)) then
               reg(1,i_reg(1)) = reg_data%regions(i)
               i_reg(1) = i_reg(1)+1
               found(1) = .true.
             end if
             if (em2 == reg_data%regions(i)%emitter .or. &
                 (em2 <= 2 .and. reg_data%regions(i)%emitter == 0)) then
               reg(2,i_reg(2)) = reg_data%regions(i)
               i_reg(2) = i_reg(2)+1
               found(2) = .true.
             end if
           end if
       end do
       if (.not. (found(1).and.found(2))) then
         bij = 0
         return
       end if

       do i = 1, i_reg(1)-1
         do j = 1, i_reg(2)-1
           icm1 = color_data%icm (reg(1,i)%alr)
           icm2 = color_data%icm (reg(2,j)%alr)
       
           iarray1 = icm1%get_index_array ()
           iarray2 = icm2%get_index_array ()
       
           iisec1 = pack (iarray1, [ (any(iarray1(i) == map_em_col1), &
                i = 1, size(iarray1)) ])
           iisec2 = pack (iarray2, [ (any(iarray2(i) == map_em_col2), &
                i = 1, size(iarray2)) ])
       
           cf_present = color_index_present (color_data%cf_index_real)
       
           do k = 1, size (iisec1)
             do l = 1, size (iisec2)
               i1 = iisec1(k)
               i2 = iisec2(l)
               if (cf_present (i1, i2)) then
                 if (is_gluon (flst_real%flst (em1)) .or. &
                     is_gluon (flst_real%flst (em2))) then
                   sign = get_sign (color_data%col_state_real (:,:,i1)) * &
                        get_sign (color_data%col_state_real (:,:,i2))
                 else
                   sign = 1
                 end if
                   color_factor = color_factor + sign*compute_color_factor &
                         (color_data%color_real(:,i1), &
                          color_data%color_real(:,i2))
               end if
             end do
           end do       
         end do
       end do
       !!! The real color factor always differs from the Born one 
       !!! by one vertex factor. Thus, apply the factor 1/2  
       bij = color_factor / (2 * color_factor_born) 
    end if

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
      iref1 = 0; iperm1 = 0; i_first = 0
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
  end function color_data_compute_bij

  subroutine color_data_write (color_data, unit)
    class(color_data_t), intent(in) :: color_data
    integer, intent(in), optional :: unit
    integer :: u, i, i1, i2
    integer :: n_legs
    u = given_output_unit (unit); if (u < 0) return
    n_legs = size (color_data%beta_ij, dim=2)
    write (u, "(1x,A)") "Color information: "
    write (u, "(1x,A,1x,I1)") "Number of Born color states: ", &
         color_data%n_col_born
    write (u, "(1x,A,1x,I1)") "Number of real color states: ", &
         color_data%n_col_real
    write (u, "(1x,A)") "Color correlation: "
    do i = 1, size (color_data%beta_ij, dim=3)
      write (u, "(1x,A,1x,I1)") "State nr. ", i
      write (u, "(1x,A)") "-------------"
      write (u, "(1x,A,1x,A,1x,A)") "i1", "i2", "color factor"
      do i1 = 1, n_legs
        do i2 = 1, i1
          write (u, "(1x,I1,1x,I1,1x,F5.2)") &
               i1, i2, color_data%beta_ij (i1,i2,i)
        end do
      end do
      write (u, "(1x,A)") "========================================"
    end do
    if (color_data%color_is_conserved) then
      write (u, "(1x,A)") "Color is conserved."
    else
      write (u, "(1x,A)") "Fatal error: Color conversation is violated."
    end if
  end subroutine color_data_write

  function nlo_data_compute_sqme_real_fin (nlo_data, weight, p_real) result (sqme_fin)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in) :: weight
    type(vector4_t), intent(inout), dimension(:), allocatable :: p_real
    type(vector4_t), dimension(:), allocatable :: p_born
    real(default) :: sqme_fin
    integer :: emitter
    if (.not. nlo_data%alpha_s_born_set) &
      call msg_fatal ("Strong coupling not set for real calculation - abort")
    emitter = nlo_data%get_active_emitter ()
    p_born = interaction_get_momenta (nlo_data%int_born)
    call nlo_data%real_terms%set_momenta (p_born, p_real)
    call nlo_data%real_terms%set_fks_kinematics (nlo_data%real_kinematics)
    sqme_fin = nlo_data%real_terms%compute (emitter, &
                                            nlo_data%alpha_s_born)
    sqme_fin = sqme_fin * weight
  end function nlo_data_compute_sqme_real_fin

  function nlo_data_has_massive_emitter (nlo_data) result (val)
    class(nlo_data_t), intent(inout) :: nlo_data
    logical :: val
    integer :: n_tot, i
    val = .false.
    n_tot = nlo_data%n_in + nlo_data%n_out_born
    do i = nlo_data%n_in+1, n_tot
       if (any (i == nlo_data%reg_data%emitters)) &
          val = val .or. nlo_data%reg_data%flv_born(1)%massive(i)
    end do
  end function nlo_data_has_massive_emitter

  subroutine nlo_data_init (nlo_data, nlo_type, prc_constants, template, model)
     class(nlo_data_t), intent(inout) :: nlo_data
     type(string_t), intent(in) :: nlo_type
     type(process_constants_t), intent(inout), dimension(2) :: prc_constants
     type(fks_template_t), intent(in) :: template
     type(model_data_t), intent(in) :: model
     integer :: i
     nlo_data%nlo_type = nlo_type
     select case (char (nlo_type))
     case ('Real')
       nlo_data%flv_state_born = prc_constants(1)%get_flv_state ()
       nlo_data%flv_state_real = prc_constants(2)%get_flv_state ()
       call nlo_data%reg_data%init (model, &
            nlo_data%flv_state_born, &
            nlo_data%flv_state_real, &
            template%mapping_type)
       select type (mapping => nlo_data%reg_data%fks_mapping)
       type is (fks_mapping_default_t)
         call mapping%set_parameter (template%fks_dij_exp1, template%fks_dij_exp2)
       end select
       nlo_data%n_flv_born = nlo_data%reg_data%n_flv_born
       nlo_data%n_flv_real = nlo_data%reg_data%n_flv_real
       allocate (nlo_data%sqme_born (nlo_data%n_flv_born))
       allocate (nlo_data%sqme_real_non_sub (nlo_data%n_flv_real))
       nlo_data%sqme_born = 0.0
       nlo_data%n_in = prc_constants(2)%n_in
       nlo_data%n_out_born = prc_constants(1)%n_out
       nlo_data%n_out_real = prc_constants(2)%n_out
       allocate (nlo_data%sqme_born_cc (nlo_data%n_in + nlo_data%n_out_born, &
                                        nlo_data%n_in + nlo_data%n_out_born, &
                                        nlo_data%n_flv_born))
       if (nlo_data%use_internal_color_correlations) &
          call nlo_data%color_data%init (nlo_data%reg_data, prc_constants)
       nlo_data%alpha_s_born_set = .false.
       call nlo_data%init_real_kinematics
       call nlo_data%real_terms%init (nlo_data%reg_data, &
                                      nlo_data%n_in + nlo_data%n_out_born, &
                                      nlo_data%n_in + nlo_data%n_out_born)
       nlo_data%counter_exists = template%count_kinematics
       if (nlo_data%counter_exists) call nlo_data%counter%init(20)
     end select
   end subroutine nlo_data_init

  subroutine nlo_data_copy_matrix_elements (nlo_data)
    class(nlo_data_t), intent(inout) :: nlo_data
     nlo_data%real_terms%sqme_born = nlo_data%sqme_born
     nlo_data%real_terms%sqme_born_cc = nlo_data%sqme_born_cc
     nlo_data%real_terms%sqme_real_bare = nlo_data%sqme_real_non_sub
     nlo_data%real_terms%me_sc = nlo_data%me_sc
  end subroutine nlo_data_copy_matrix_elements

  !!! Incomplete
  subroutine nlo_data_write (nlo_data, unit)
    class(nlo_data_t), intent(in) :: nlo_data
    integer, intent(in), optional :: unit
    integer :: i, u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(1x,A,I0)") "n_alr          = ", nlo_data%n_alr
    write (u, "(1x,A,I0)") "n_in           = ", nlo_data%n_in
    write (u, "(1x,A,I0)") "n_out_born     = ", nlo_data%n_out_born
    write (u, "(1x,A,I0)") "n_out_real     = ", nlo_data%n_out_real
    write (u, "(1x,A,I0)") "n_allowed_born = ", nlo_data%n_allowed_born
  end subroutine nlo_data_write

  subroutine nlo_data_init_born_amps (nlo_data, n, internal_correlations)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in) :: n
    logical, intent(in) :: internal_correlations
    nlo_data%n_allowed_born = n
    allocate (nlo_data%amp_born (n))
  end subroutine nlo_data_init_born_amps

  subroutine nlo_data_set_internal_procedures (nlo_data, flag_color, flag_spin)
    class(nlo_data_t), intent(inout) :: nlo_data
    logical, intent(in) :: flag_color, flag_spin
    nlo_data%use_internal_color_correlations = flag_color
    nlo_data%real_terms%sub_soft%use_internal_color_correlations = flag_color
    nlo_data%virtual_terms%use_internal_color_correlations = flag_color
    nlo_data%use_internal_spin_correlations = flag_spin
  end subroutine nlo_data_set_internal_procedures

  subroutine nlo_data_init_virtual (nlo_data)
    class(nlo_data_t), intent(inout) :: nlo_data
    call nlo_data%virtual_terms%init (nlo_data%flv_state_born)
  end subroutine nlo_data_init_virtual

  subroutine nlo_data_set_nlo_type (nlo_data, nlo_type)
   class(nlo_data_t), intent(inout) :: nlo_data
   type(string_t), intent(in) :: nlo_type
   nlo_data%nlo_type = nlo_type
  end subroutine nlo_data_set_nlo_type

  pure function nlo_data_get_nlo_type (nlo_data) result(nlo_type)
    class(nlo_data_t), intent(in) :: nlo_data
    type(string_t) :: nlo_type
    nlo_type = nlo_data%nlo_type
  end function nlo_data_get_nlo_type

  function nlo_data_get_emitters (nlo_data) result(emitters)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, dimension(:), allocatable :: emitters
    emitters = nlo_data%reg_data%get_emitters ()
  end function nlo_data_get_emitters

  subroutine nlo_data_set_active_emitter (nlo_data, emitter)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in) :: emitter
    nlo_data%active_emitter = emitter
  end subroutine nlo_data_set_active_emitter

  function nlo_data_get_active_emitter (nlo_data) result(emitter)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer :: emitter
    emitter = nlo_data%active_emitter
  end function nlo_data_get_active_emitter

  subroutine nlo_data_set_flv_born (nlo_data, flv_in)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in), dimension(:), allocatable :: flv_in
    allocate (nlo_data%flv_born (size (flv_in)))
    nlo_data%flv_born = flv_in
  end subroutine nlo_data_set_flv_born 

  subroutine nlo_data_set_hel_born (nlo_data, hel_in)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in), dimension(:), allocatable :: hel_in
    allocate (nlo_data%hel_born (size (hel_in)))
    nlo_data%hel_born = hel_in
  end subroutine nlo_data_set_hel_born 

  subroutine nlo_data_set_col_born (nlo_data, col_in)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in), dimension(:), allocatable :: col_in
    allocate (nlo_data%col_born (size (col_in)))
    nlo_data%col_born = col_in
  end subroutine nlo_data_set_col_born

  subroutine nlo_data_set_alpha_s_born (nlo_data, as_born)
    class (nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in) :: as_born
    nlo_data%alpha_s_born = as_born
    nlo_data%alpha_s_born_set = .true.
  end subroutine nlo_data_set_alpha_s_born

  subroutine nlo_data_init_real_kinematics (nlo_data)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer :: n_tot
    n_tot = nlo_data%n_in + nlo_data%n_out_born
    allocate (nlo_data%real_kinematics%xi_max (n_tot))
    allocate (nlo_data%real_kinematics%y (n_tot))
    allocate (nlo_data%real_kinematics%y_soft (n_tot))
    allocate (nlo_data%real_kinematics%jac_rand (n_tot))
    nlo_data%real_kinematics%xi_tilde = 0
    nlo_data%real_kinematics%y = 0
    nlo_data%real_kinematics%xi_max = 0
    nlo_data%real_kinematics%phi = 0
  end subroutine nlo_data_init_real_kinematics

  subroutine nlo_data_set_real_kinematics (nlo_data, xi_tilde, y, phi, xi_max, &
                                           jac, jac_rand)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), dimension(:), allocatable :: xi_max, y
    real(default), intent(in) :: xi_tilde
    real(default), intent(in) :: phi
    real(default), intent(in), dimension(3) :: jac
    real(default), intent(in), dimension(:), allocatable :: jac_rand
    nlo_data%real_kinematics%xi_tilde = xi_tilde
    nlo_data%real_kinematics%y = y
    nlo_data%real_kinematics%phi = phi
    nlo_data%real_kinematics%xi_max = xi_max
    nlo_data%real_kinematics%jac = jac
    nlo_data%real_kinematics%jac_rand = jac_rand
  end subroutine nlo_data_set_real_kinematics

  subroutine nlo_data_get_real_kinematics &
       (nlo_data, em, xi_tilde, y, xi_max, jac, phi, jac_rand)
    class(nlo_data_t), intent(in) :: nlo_data
    integer, intent(in) :: em
    real(default), intent(out) :: xi_tilde, y, xi_max
    real(default), intent(out), dimension(3), optional :: jac
    !!! For most applications, phi is not relevant. Thus, it is not 
    !!! always transferred as a dummy-variable
    real(default), intent(out), optional :: phi
    real(default), intent(out), dimension(:), optional :: jac_rand
    xi_tilde = nlo_data%real_kinematics%xi_tilde
    y = nlo_data%real_kinematics%y(em)
    xi_max = nlo_data%real_kinematics%xi_max (em)
    if (present (jac)) jac = nlo_data%real_kinematics%jac
    if (present (phi)) phi = nlo_data%real_kinematics%phi
    if (present (jac_rand)) jac_rand = nlo_data%real_kinematics%jac_rand
  end subroutine nlo_data_get_real_kinematics

  subroutine nlo_data_set_real_momenta (nlo_data, p)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(vector4_t), intent(in), dimension(:), allocatable :: p
    nlo_data%real_kinematics%p_real = p
  end subroutine nlo_data_set_real_momenta

  function nlo_data_get_real_momenta (nlo_data) result (p)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(vector4_t), dimension(:), allocatable :: p
    p = nlo_data%real_kinematics%p_real
  end function nlo_data_get_real_momenta

  subroutine nlo_data_set_y_soft (nlo_data, y_soft, emitter)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in) :: y_soft
    integer, intent(in) :: emitter
    nlo_data%real_kinematics%y_soft(emitter) = y_soft
  end subroutine nlo_data_set_y_soft

  subroutine nlo_data_set_jacobian (nlo_data, jac)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in), dimension(3) :: jac
    nlo_data%real_kinematics%jac = jac
  end subroutine nlo_data_set_jacobian

  function nlo_data_compute_virt (nlo_data, i_flv, int_born) result(sqme_virt)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in) :: i_flv
    type(interaction_t), intent(in) :: int_born
    real(default) :: sqme_virt
    type(vector4_t), dimension(:), allocatable :: p_born
    p_born = interaction_get_momenta (int_born)
    if (nlo_data%use_internal_color_correlations) then
      call nlo_data%virtual_terms%evaluate (nlo_data%reg_data, &
                                            i_flv, &
                                            nlo_data%alpha_s_born, &
                                            p_born, &
                                            nlo_data%sqme_born(i_flv), &
                                            nlo_data%color_data%beta_ij)
    else
      call nlo_data%virtual_terms%evaluate (nlo_data%reg_data, &
                                            i_flv, &
                                            nlo_data%alpha_s_born, &
                                            p_born, &
                                            nlo_data%sqme_born(i_flv), &
                                            nlo_data%sqme_born_cc)
    end if
    sqme_virt = nlo_data%virtual_terms%sqme_virt
  end function nlo_data_compute_virt

  function nlo_data_requires_spin_correlation (nlo_data, i_flv) result (val)
    class(nlo_data_t), intent(in) :: nlo_data
    integer, intent(in) :: i_flv
    logical :: val
    val = nlo_data%real_terms%sc_required (i_flv)
  end function nlo_data_requires_spin_correlation


end module nlo_data
