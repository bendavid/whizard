! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module fks_calculation

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
  use variables
  use model_data
  use models
  use parser
  use eval_trees
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

  implicit none
  private

  public :: fks_template_t
  public :: nlo_data_t
  public :: nlo_pointer_t
  public :: fks_calculation_test

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

  type :: real_kinematics_t
    real(default) :: xi_tilde
    real(default) :: y
    real(default) :: phi
    real(default), dimension(:), allocatable :: xi_max
    real(default), dimension(3) :: jac
    type(vector4_t), dimension(:), allocatable :: p_real
  end type real_kinematics_t

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

  type :: soft_subtraction_t
    real(default), dimension(:), allocatable :: value
    type(region_data_t) :: reg_data
    integer :: nlegs_born, nlegs_real
    real(default), dimension(:,:), allocatable :: momentum_matrix
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
  contains
    procedure :: compute => soft_subtraction_compute
    procedure :: compute_momentum_matrix => &
         soft_subtraction_compute_momentum_matrix
    procedure :: create_softvec_fsr => soft_subtraction_create_softvec_fsr
  end type soft_subtraction_t
  
  type :: coll_subtraction_t
    real(default), dimension(:), allocatable :: value
    real(default), dimension(:), allocatable :: value_soft
    integer :: n_alr
    real(default), dimension(0:3,0:3) :: b_munu
    real(default) , dimension(0:3,0:3) :: k_perp_matrix
  contains
    procedure :: init => coll_subtraction_init
    procedure :: set_k_perp => coll_subtraction_set_k_perp
    procedure :: compute => coll_subtraction_compute
    procedure :: compute_soft_limit => coll_subtraction_compute_soft_limit
  end type coll_subtraction_t
  
  type :: virtual_t
    real(default) :: Q
    real(default), dimension(:,:), allocatable :: I
    real(default) :: vfin
    real(default) :: sqme_cc
    real(default) :: sqme_virt
    real(default), dimension(:), allocatable :: gamma_0, gamma_p, c_flv
    real(default) :: ren_scale2
    integer :: n_is_neutrinos = 0
    logical :: bad_point
    logical :: use_internal_color_correlations
  contains
    procedure :: init => virtual_init
    procedure :: init_constants => virtual_init_constants
    procedure :: set_ren_scale => virtual_set_ren_scale
    procedure :: compute_Q => virtual_compute_Q
    procedure :: compute_I => virtual_compute_I
    procedure :: compute_vfin_test => virtual_compute_vfin_test
    procedure :: evaluate => virtual_evaluate
    procedure :: set_vfin => virtual_set_vfin
    procedure :: set_bad_point => virtual_set_bad_point
  end type virtual_t
  
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
    type(soft_subtraction_t) :: sub_soft
    type(coll_subtraction_t) :: sub_coll
    type(virtual_t) :: virtual_terms
    integer, dimension(:,:), allocatable :: flv_state_born
    integer, dimension(:,:), allocatable :: hel_state_born
    integer, dimension(:,:), allocatable :: flv_state_real
    integer, dimension(:,:), allocatable :: hel_state_real
    integer, dimension(:), allocatable :: flv_born
    integer, dimension(:), allocatable :: hel_born
    integer, dimension(:), allocatable :: col_born
    real(default) :: alpha_s_born
    logical :: alpha_s_born_set
    real(default), dimension(:), allocatable :: sqme_born
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    real(default), dimension(:,:,:), allocatable :: sqme_born_sc
    real(default), public :: sqme_real
    real(default), public :: sqme_virt
    type(interaction_t), public :: int_born
    real(default), public :: jac_real
    type(kinematics_counter_t), public :: counter
    logical, public :: counter_exists = .false.
    logical, dimension(:), allocatable :: sc_required
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
  contains
    procedure :: compute_sqme_real_fin => nlo_data_compute_sqme_real_fin
    procedure :: init => nlo_data_init
    procedure :: write => nlo_data_write
    procedure :: init_born_amps => nlo_data_init_born_amps
    procedure :: init_soft => nlo_data_init_soft
    procedure :: init_coll => nlo_data_init_coll
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
    procedure :: set_jacobian => nlo_data_set_jacobian
    procedure :: compute_sub_soft => nlo_data_compute_sub_soft
    procedure :: compute_sub_coll => nlo_data_compute_sub_coll
    procedure :: compute_sub_coll_soft => nlo_data_compute_sub_coll_soft
    procedure :: compute_virt => nlo_data_compute_virt
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
    integer :: n_entries, val
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
    integer :: i, j
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
    allocate (color_data%icm (size (reg_data%regions)))
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
    integer :: i, j, k
    integer :: i_uborn
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
        if (is_qcd_particle (flv_born%flst(em1)) &
            .and. is_qcd_particle (flv_born%flst(em2))) then
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
    integer :: pref
    logical, dimension(:,:), allocatable :: cf_present
    type(singular_region_t), dimension(2,100) :: reg
    integer ::  i, j, k, l
    type(ftuple_color_map_t) :: icm1, icm2
    integer :: i1, i2
    real(default) :: color_factor, color_factor_born
    real(default) :: sqme_born
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
    
       do i = 1, reg_data%get_nregions ()
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

  function nlo_data_compute_sqme_real_fin (nlo_data, weight, p_real, sqme0) result (sqme_fin)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in) :: weight
    type(vector4_t), intent(inout), dimension(:), allocatable :: p_real
    real(default), intent(inout) :: sqme0
    real(default) :: sqme_fin
    integer :: em, alr
    integer :: iuborn
    real(default) :: xi, y, xi_max, phi
    real(default), dimension(3) :: jac
    real(default) :: xi_tilde
    real(default) :: s_alpha
    real(default) :: sqme_soft, sqme_coll, sqme_cs, sqme_remn
    sqme_fin = 0
    if (.not. nlo_data%alpha_s_born_set) &
      call msg_fatal ("Strong coupling not set for real calculation - abort")
    em = nlo_data%get_active_emitter ()
    call nlo_data%get_real_kinematics (em, xi_tilde, y, xi_max, jac = jac, phi = phi)
    call nlo_data%counter%record (xi_tilde = xi_tilde, xi_max = xi_max, &
                                  y = y, phi = phi)
    LOOP_OVER_ALPHA_REGIONS: do alr = 1, nlo_data%n_alr
      iuborn = nlo_data%reg_data%regions(alr)%uborn_index
      if (em == nlo_data%reg_data%regions(alr)%emitter .and. iuborn == 1) then
        xi = xi_tilde * xi_max
        call nlo_data%counter%record (xi = xi)
        sqme0 = sqme0 * xi**2/xi_tilde
        s_alpha = nlo_data%reg_data%get_svalue (p_real, alr, em)
        sqme0 = sqme0 * weight * s_alpha * jac(1)
        call nlo_data%compute_sub_soft (nlo_data%int_born, p_real, alr, em)
        call nlo_data%compute_sub_coll (nlo_data%int_born, alr)
        call nlo_data%compute_sub_coll_soft (nlo_data%int_born, alr)
        sqme_soft = nlo_data%sub_soft%value(alr)
        sqme_coll = nlo_data%sub_coll%value(alr)
        sqme_cs = nlo_data%sub_coll%value_soft(alr)
        sqme_soft = sqme_soft/(1-y)/xi_tilde*jac(2)
        sqme_coll = sqme_coll/(1-y)/xi_tilde*jac(3)
        sqme_cs = sqme_cs/(1-y)/xi_tilde*jac(2)
        sqme_remn = (sqme_soft - sqme_cs)*log(xi_max)*xi_tilde
        sqme_fin = sqme_fin + sqme0 - sqme_soft - sqme_coll + sqme_cs + sqme_remn
        if (sqme_fin /= sqme_fin) then
                         print *, 'emitter: ', em
                         print *, 'xi_max: ', xi_max
                         print *, 'xi: ', xi, 'y: ', y
                         print *, 'sqme_born: ', nlo_data%sqme_born(iuborn), &
                        'sqme_real: ', sqme0, &
                        'sqme_soft: ', sqme_soft, &
                        'sqme_coll: ', sqme_coll, &
                        'sqme_coll-soft: ', sqme_cs, &
                        'sqme_remn: ', sqme_remn
        else
          sqme_fin = sqme_fin * nlo_data%reg_data%regions(alr)%mult
        end if
      end if
    end do LOOP_OVER_ALPHA_REGIONS
  end function nlo_data_compute_sqme_real_fin

  subroutine soft_subtraction_compute (sub_soft, int_born, p_real, &
       sqme_born, born_ij, y, phi, alpha_s_born, alr, emitter)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(interaction_t), intent(in) :: int_born
    type(vector4_t), intent(in), dimension(:), allocatable :: p_real
    real(default), intent(in) :: sqme_born
    real(default), intent(in), dimension(:,:) :: born_ij
    real(default), intent(in) :: y, phi
    real(default), intent(in) :: alpha_s_born
    integer, intent(in) :: alr, emitter
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t) :: p_soft
    real(default) :: s_alpha_soft
    real(default) :: q02
    integer :: i, j
    allocate (p_born (sub_soft%nlegs_born))
    p_born = interaction_get_momenta (int_born)
    p_soft = sub_soft%create_softvec_fsr (p_born, y, phi, emitter)
    s_alpha_soft = sub_soft%reg_data%get_svalue_soft &
         (p_born, p_soft, alr, emitter)
    call sub_soft%compute_momentum_matrix (p_born, p_soft)
    if (sub_soft%use_internal_color_correlations) then
      sub_soft%value(alr) = 4*pi*alpha_s_born * sqme_born * s_alpha_soft
    else
      sub_soft%value(alr) = 4*pi*alpha_s_born * s_alpha_soft
    end if
    sub_soft%value(alr) = sub_soft%value(alr) * &
         fold_matrices(sub_soft%momentum_matrix, &
         born_ij, 1)
    q02 = 4* vector4_get_component (p_born(1), 0) * &
         vector4_get_component (p_born(2), 0)
    !!! Map emitter -> value_index    
    sub_soft%value(alr) = 4/q02 * (1-y) * sub_soft%value(alr) 
  contains
    subroutine exchange_color_particles (col_state, i1, i2)
      integer, intent(inout), dimension(:,:) :: col_state
      integer, intent(in) :: i1, i2
      integer, dimension(2) :: col_tmp
      col_tmp = col_state (:,i1)
      col_state (:,i1) = col_state (:,i2)
      col_state (:,i2) = col_tmp
    end subroutine exchange_color_particles    
  end subroutine soft_subtraction_compute

  subroutine soft_subtraction_compute_momentum_matrix &
       (sub_soft, p_born, p_soft)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), intent(in) :: p_soft
    real(default) :: num, deno1, deno2
    integer :: i, j
    do i = 1, sub_soft%nlegs_born
      do j = 1, sub_soft%nlegs_born
        if (i <= j) then
          num = p_born(i) * p_born(j)
          deno1 = p_born(i)*p_soft
          deno2 = p_born(j)*p_soft
          sub_soft%momentum_matrix(i,j) = num/(deno1*deno2)
        else
           !!! momentum matrix is symmetric.
          sub_soft%momentum_matrix(i,j) = sub_soft%momentum_matrix(j,i)
        end if
      end do
    end do
  end subroutine soft_subtraction_compute_momentum_matrix
  
  function soft_subtraction_create_softvec_fsr &
       (sub_soft, p_born, y, phi, emitter) result (p_soft)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(inout), dimension(:), allocatable :: p_born
    real(default), intent(in) :: y, phi
    integer, intent(in) :: emitter
    type(vector4_t) :: p_soft
    type(vector3_t) :: dir
    type(lorentz_transformation_t) :: rot
    p_soft = p_born(emitter) / vector4_get_component (p_born(emitter), 0)
    dir = create_orthogonal (space_part (p_born(emitter)))
    rot = rotation (y, sqrt(1-y**2), dir)
    p_soft = rot*p_soft
    if (phi /= 0) then
      dir = space_part (p_born(emitter)) / &
           vector4_get_component (p_born(emitter), 0)
      rot = rotation (cos(phi), sin(phi), dir)
      p_soft = rot*p_soft
    end if
  end function soft_subtraction_create_softvec_fsr
  
  function fold_matrices (m1, m2, i_i, i_final) result (res)
    real(default), intent(in), dimension(:,:) :: m1, m2
    integer, intent(in) :: i_i
    integer, intent(in), optional :: i_final
    real(default) :: res
    integer :: n1, n2
    integer :: i_f
    integer :: i, j
    res = 0
    n1 = size (m1,1)
    n2 = size (m2,1)
    if (n1 /= n2) then
      call msg_fatal ("Fold matrices: Matrices need to have identical shape!")
    else
      if (present (i_final)) then
        i_f = i_final
      else
        i_f = n1
      end if
    end if

    do i = i_i, i_f
      do j = i_i, i_f
        res = res + m1(i,j) * m2(i,j)
      end do
    end do
    !!! Matrices are symmetric
    ! res = 2*res
  end function fold_matrices
  
  subroutine coll_subtraction_init (coll_sub, n_alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    integer, intent(in) :: n_alr
    coll_sub%n_alr = n_alr
    allocate (coll_sub%value (n_alr))
    allocate (coll_sub%value_soft (n_alr))
  end subroutine coll_subtraction_init

  subroutine coll_subtraction_set_k_perp (coll_sub, p, em, phi)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: em
    real(default), intent(in) :: phi
    type(vector4_t) :: k_perp
    real(default) :: p1, p2, p3
    type(vector3_t) :: vec
    type(lorentz_transformation_t) :: rot
    integer :: i, j

    p1 = vector4_get_component (p(em), 1)
    p2 = vector4_get_component (p(em), 2)
    p3 = vector4_get_component (p(em), 3)

    call vector4_set_component (k_perp, 0, 0._default)
    call vector4_set_component (k_perp, 1, p1)
    call vector4_set_component (k_perp, 2, p2)
    call vector4_set_component (k_perp, 3, -(p1**2+p2**2)/p3)

    vec = create_unit_vector (p(em))
    rot = rotation (cos(phi), sin(phi), vec)
    k_perp = rot * k_perp

    do i = 0, 3
      do j = 0, i
        coll_sub%k_perp_matrix(i,j) = vector4_get_component (k_perp, i) * &
                                      vector4_get_component (k_perp, j)
        coll_sub%k_perp_matrix(j,i) = coll_sub%k_perp_matrix(i,j)
      end do
    end do

  end subroutine coll_subtraction_set_k_perp

  subroutine coll_subtraction_compute &
       (coll_sub, sregion, p_born, sqme_born, sqme_born_sc, &
        xi, alpha_s, alr, soft_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in), dimension(0:3,0:3) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    logical, intent(in), optional :: soft_in
    real(default) :: res
    real(default) :: q0, z, p0
    real(default) :: zoxi
    real(default) :: trB, BK
    real(default) :: pggz
    integer :: nlegs, emitter
    integer :: flv_em, flv_rad
    logical :: soft
    if (present (soft_in)) then
      soft = soft_in
    else
      soft = .false.
    end if
    nlegs = size (sregion%flst_real%flst)
    emitter = sregion%emitter
    flv_rad = sregion%flst_real%flst(nlegs)
    flv_em = sregion%flst_real%flst(emitter)
    p0 = vector4_get_component (p_born(emitter),0)
    if (sregion%emitter <= 2) then
      coll_sub%value(alr) = 0   
    else
      q0 = vector4_get_component (p_born(1), 0) + &
           vector4_get_component (p_born(2), 0)
      !!! Here, z corresponds to 1-z in the formulas of arXiv:1002.2581; 
      !!! the integrand is symmetric under this variable change
      zoxi = q0/(2*p0)
      z = xi*zoxi
      if (is_gluon(flv_em) .and. is_gluon(flv_rad)) then
         pggz = CA*(z**2*(1-z) - z**2/(1-z) + z-1)
         trB = compute_trB (); BK = compute_BK ()
         res = (trB*pggz + 4*BK*CA*z**2*(1-z))/zoxi
      else if (is_gluon(flv_em) .and. is_quark (abs(flv_rad))) then
         trB = compute_trB (); BK = compute_BK ()
         res = -TR*(trB*z + z**2*(1-z)*BK)/zoxi
      else if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
         res = sqme_born*CF*(1+(1-z)**2)/zoxi
      else
        stop 'Error: Impossible flavor structure in collinear counterterm!' 
      end if
    end if
    res = res /(p0**2*(1-z)**zoxi)
    res = res * 4*pi*alpha_s * sregion%mult

    if (soft) then
      coll_sub%value_soft (alr) = res
    else
      coll_sub%value (alr) = res
    end if
  contains
    function compute_trB () result (value)
      real(default) :: value
      value = sqme_born_sc(0,0) - sqme_born_sc(1,1) - sqme_born_sc(2,2) - sqme_born_sc(3,3)
    end function compute_trB
    
    function compute_BK () result (value)
      real(default) :: value
      BK = fold_matrices (sqme_born_sc, coll_sub%k_perp_matrix, 0, 3)
    end function compute_BK
  end subroutine coll_subtraction_compute
  
  subroutine coll_subtraction_compute_soft_limit &
       (coll_sub, sregion, p_born, sqme_born, &
        sqme_born_sc, xi, alpha_s, alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in), dimension(0:3,0:3) :: sqme_born_sc
    real(default) :: xi, alpha_s
    integer, intent(in) :: alr
    real(default) :: coll_soft_sub
    call coll_sub%compute (sregion, p_born, sqme_born, &
                           sqme_born_sc, xi, alpha_s, alr, .true.)
  end subroutine coll_subtraction_compute_soft_limit
  
 subroutine virtual_init (object, flv_born)
    class(virtual_t), intent(inout) :: object
    integer, intent(in), dimension(:) :: flv_born
    integer :: nlegs
    nlegs = size (flv_born)
    allocate (object%I (nlegs, nlegs))
    allocate (object%gamma_0 (nlegs), object%gamma_p (nlegs), &
         object%c_flv (nlegs))
    call object%init_constants (flv_born)
    if (is_neutrino (flv_born(1))) &
       object%n_is_neutrinos = object%n_is_neutrinos + 1
    if (is_neutrino (flv_born(2))) &
       object%n_is_neutrinos = object%n_is_neutrinos + 1 
  contains
    function is_neutrino (flv) result (neutrino)
      integer, intent(in) :: flv
      logical :: neutrino
      neutrino = (abs(flv)==12 .or. abs(flv)==14 .or. abs(flv)==16)
    end function is_neutrino
  end subroutine virtual_init

  subroutine virtual_init_constants (object, flv_born)
    class(virtual_t), intent(inout) :: object
    integer, intent(in), dimension(:) :: flv_born
    integer :: i
    integer, parameter :: nf = 1 !What is the proper choice of nf?
    do i = 1, size (flv_born)
      if (is_gluon (flv_born(i))) then
        object%gamma_0(i) = (11*ca - 2*nf)/6
        object%gamma_p(i) = (67.0/9 - 2*pi**2/3)*ca - 23.0/18*nf
        object%c_flv(i) = ca
      else if (is_quark (abs(flv_born(i)))) then
        object%gamma_0(i) = 1.5*cf
        object%gamma_p(i) = (6.5 - 2*pi**2/3)*cf
        object%c_flv(i) = cf
      else
        object%gamma_0(i) = 0
        object%gamma_p(i) = 0
        object%c_flv(i) = 0
      end if
    end do
  end subroutine virtual_init_constants
  
  subroutine virtual_set_ren_scale (object, int_born, ren_scale)
    class(virtual_t), intent(inout) :: object
    type(interaction_t), intent(in) :: int_born
    real(default), intent(in) :: ren_scale
    type(vector4_t), dimension(:), allocatable :: p_born
    if (ren_scale > 0) then
      object%ren_scale2 = ren_scale**2
    else
      p_born = interaction_get_momenta (int_born)
      object%ren_scale2 = (p_born(1)+p_born(2))**2
    end if
  end subroutine virtual_set_ren_scale
  
  subroutine virtual_compute_Q (object, p_born)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born
    real(default) :: Q
    real(default) :: sqrts, E
    real(default) :: s1, s2, s3, s4
    integer :: i
    sqrts = sqrt ((p_born(1)+p_born(2))**2)
    !!! ---- NOTE: Implementation only works for lepton collisions. 
    !!!            This implies that both the summand containing log(s/q**2) 
    !!!            and (γ_fp + γ…vfm) vanish. 
    !!!            Also, s = (p_born(1)+p_born(2))**2
    object%Q = 0
    do i = 1, size(p_born)
      s1 = object%gamma_p(i)
      s2 = 0; s3 = 0; s4 = 0
      E = vector4_get_component (p_born(i), 0)
      s2 = log(sqrts**2/object%ren_scale2)*&
           (object%gamma_0(i)-2 * object%c_flv(i) * log(2*E/sqrts))
      s3 = 2*log(2*E/sqrts)**2*object%c_flv(i)
      s4 = 2*log(2*E/sqrts)*object%gamma_0(i)
      object%Q = object%Q + s1 - s2 + s3 - s4
    end do
  end subroutine virtual_compute_Q
  
  subroutine virtual_compute_I (object, p_born, i, j)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born
    integer, intent(in) :: i, j
    type(vector4_t) :: pi, pj
    real(default) :: Ei, Ej
    real(default) :: pij, Eij
    real(default) :: s
    real(default) :: s1, s2, s3, s4, s5
    real(default) :: arglog
    real(default), parameter :: tiny_value = epsilon(1.0)
    !!! ----NOTE: As above, only lepton collisions. Therefore, the 
    !!!           first and second summand are not taken into account.
    s1 = 0; s2 = 0; s3 = 0; s4 = 0; s5 = 0
    s = (p_born(1)+p_born(2))**2
    pi = p_born(i); pj = p_born(j)
    Ei = vector4_get_component (pi, 0)
    Ej = vector4_get_component (pj, 0)
    pij = pi*pj; Eij = Ei*Ej
    s1 = 0.5*log(s/object%ren_scale2)**2
    s2 = log(s/object%ren_scale2)*log(pij/(2*Eij))
    s3 = Li2 (pij / (2*Eij))
    s4 = 0.5*log (pij / (2*Eij))**2
    arglog = 1 - pij/(2*Eij)
    if (arglog > tiny_value) then
      s5 = log(arglog) * log(pij / (2*Eij))
    else
      s5 = 0
    end if
    object%I(i,j) = s1 + s2 -s3 + s4 - s5
  end subroutine virtual_compute_I
  
  subroutine virtual_compute_vfin_test (object, p_born, sqme_born)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default) :: s, mu2
    s = (p_born(1)+p_born(2))**2
    !!! ----NOTE: Test implementation for e+ e- -> u ubar
    object%vfin = sqme_born * cf * &
         (pi**2 - 8 + 3*log(s/object%ren_scale2) - log(s/object%ren_scale2)**2)
    object%bad_point = .false.
  end subroutine virtual_compute_vfin_test

  subroutine virtual_evaluate &
       (object, reg_data, i_proc, alpha_s, p_born, born, b_ij)
    class(virtual_t), intent(inout) :: object
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: i_proc
    real(default), intent(in) :: alpha_s
    type(vector4_t), intent(inout), dimension(:), allocatable :: p_born
    real(default), intent(in) :: born
    real(default), intent(in), dimension(:,:,:), allocatable :: b_ij
    integer :: i, j, alr
    integer :: nlegs
    real(default) :: BI
    if (object%bad_point) then
       object%sqme_virt = 0
    else
       BI = 0
       nlegs = size (p_born)
       call object%compute_Q (p_born)
       alr = find_first_alr (reg_data, i_proc)
       associate (flst_born => reg_data%regions(alr)%flst_uborn%flst)
         do i = 1, nlegs
           do j = 1, nlegs
             if (i /= j) then
               if (abs(flst_born(i)) <= 6 .and. abs(flst_born(j)) <= 6) then
                  call object%compute_I (p_born, i, j)
                  BI = BI + b_ij (i,j,reg_data%regions(alr)%uborn_index) * &
                                  object%I(i,j)
               end if
             end if
           end do
         end do
       end associate
       if (object%use_internal_color_correlations) BI = BI*born
       object%sqme_virt = alpha_s/twopi * (object%Q*born + BI + object%vfin)
       if (object%n_is_neutrinos > 0) &
          object%sqme_virt = object%sqme_virt * object%n_is_neutrinos*2
    end if
  contains
    function find_first_alr (reg_data, i_proc) result (alr_out)
       type(region_data_t), intent(in) :: reg_data
       integer, intent(in) :: i_proc
       integer :: alr_out
       integer :: k
       alr_out = 0
       do k = 1, reg_data%get_nregions ()
          alr_out = alr_out+1
          if (reg_data%regions(k)%uborn_index == i_proc+1) exit
       end do
    end function find_first_alr
  end subroutine virtual_evaluate

  subroutine virtual_set_vfin (object, vfin)
    class(virtual_t), intent(inout) :: object
    real(default) :: vfin
    object%vfin = vfin
  end subroutine virtual_set_vfin

  subroutine virtual_set_bad_point (object, value)
     class(virtual_t), intent(inout) :: object
     logical, intent(in) :: value
     object%bad_point = value
  end subroutine virtual_set_bad_point

  subroutine nlo_data_init (nlo_data, nlo_type, prc_constants, template, model)
     class(nlo_data_t), intent(inout) :: nlo_data
     type(string_t), intent(in) :: nlo_type
     type(process_constants_t), intent(inout), dimension(2) :: prc_constants
     type(fks_template_t), intent(in) :: template
     class(model_data_t), intent(in) :: model
     integer :: i
     nlo_data%nlo_type = nlo_type
     select case (char (nlo_type))
     case ('Real')
       nlo_data%flv_state_born = prc_constants(1)%get_flv_state ()
       nlo_data%flv_state_real = prc_constants(2)%get_flv_state ()
       allocate (nlo_data%sc_required (size (nlo_data%flv_state_born, dim=2)))
       do i = 1, size (nlo_data%sc_required)
         if (any (nlo_data%flv_state_born (:,i) == GLUON)) then
            nlo_data%sc_required(i) = .true.
         else
            nlo_data%sc_required(i) = .false.
         end if
       end do
       ! call prc_constants(1)%get_hel_state (nlo_data%hel_state_born)
       ! call prc_constants(2)%get_hel_state (nlo_data%hel_state_real)
       call nlo_data%reg_data%init &
            (model, nlo_data%flv_state_born, nlo_data%flv_state_real, &
             template%mapping_type)
       select type (mapping => nlo_data%reg_data%fks_mapping)
       type is (fks_mapping_default_t)
         call mapping%set_parameter (template%fks_dij_exp1, template%fks_dij_exp2)
       end select
       nlo_data%n_flv_born = nlo_data%reg_data%n_flv_born
       nlo_data%n_flv_real = nlo_data%reg_data%n_flv_real
       allocate (nlo_data%sqme_born (nlo_data%n_flv_born))
       nlo_data%sqme_born = 0.0
       nlo_data%n_alr = size(nlo_data%reg_data%regions)
       nlo_data%n_in = prc_constants(2)%n_in
       nlo_data%n_out_born = prc_constants(1)%n_out
       nlo_data%n_out_real = prc_constants(2)%n_out
       nlo_data%alpha_s_born = 0._default
       nlo_data%alpha_s_born_set = .false.
       nlo_data%sqme_real = 0._default
       allocate (nlo_data%sqme_born_cc (nlo_data%n_in + nlo_data%n_out_born, &
                                        nlo_data%n_in + nlo_data%n_out_born, &
                                        nlo_data%n_flv_born))
       allocate (nlo_data%sqme_born_sc (0:3,0:3,nlo_data%n_flv_born))
       call nlo_data%color_data%init (nlo_data%reg_data, prc_constants)
       call nlo_data%init_real_kinematics
       call nlo_data%init_soft (prc_constants)
       call nlo_data%init_coll 
       nlo_data%counter_exists = template%count_kinematics
       if (nlo_data%counter_exists) call nlo_data%counter%init(20)
     end select
   end subroutine nlo_data_init

  subroutine nlo_data_write (nlo_data, unit)
    class(nlo_data_t), intent(in) :: nlo_data
    integer, intent(in), optional :: unit
    integer :: i, j, u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(1x,A,I0)") "n_alr          = ", nlo_data%n_alr
    write (u, "(1x,A,I0)") "n_in           = ", nlo_data%n_in
    write (u, "(1x,A,I0)") "n_out_born     = ", nlo_data%n_out_born
    write (u, "(1x,A,I0)") "n_out_real     = ", nlo_data%n_out_real
    write (u, "(1x,A,I0)") "n_allowed_born = ", nlo_data%n_allowed_born
    write (u, "(1x,A,I0)") "n_flv_born     = ", nlo_data%n_flv_born
    write (u, "(1x,A,I0)") "n_flv_real     = ", nlo_data%n_flv_real
    do i = 1, size (nlo_data%flv_born)
      write (u, "(3x,I0)") nlo_data%flv_born(i)
    end do
    do i = 1, size (nlo_data%col_born)
      write (u, "(3x,I0)") nlo_data%col_born(i)
    end do
  end subroutine nlo_data_write

  subroutine nlo_data_init_born_amps (nlo_data, n, internal_correlations)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in) :: n
    logical, intent(in) :: internal_correlations
    nlo_data%n_allowed_born = n
    allocate (nlo_data%amp_born (n))
    nlo_data%use_internal_color_correlations = internal_correlations
    nlo_data%sub_soft%use_internal_color_correlations = internal_correlations
    nlo_data%virtual_terms%use_internal_color_correlations = internal_correlations
    nlo_data%use_internal_spin_correlations = internal_correlations
  end subroutine nlo_data_init_born_amps

  subroutine nlo_data_init_soft (nlo_data, prc_constants)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(process_constants_t), intent(inout), dimension(2) :: prc_constants
    nlo_data%sub_soft%reg_data = nlo_data%reg_data
    nlo_data%sub_soft%nlegs_born = nlo_data%n_in + nlo_data%n_out_born
    nlo_data%sub_soft%nlegs_real = nlo_data%n_in + nlo_data%n_out_real
    allocate (nlo_data%sub_soft%value (nlo_data%reg_data%n_emitters))
    allocate (nlo_data%sub_soft%momentum_matrix &
             (nlo_data%sub_soft%nlegs_born, nlo_data%sub_soft%nlegs_born))
  end subroutine nlo_data_init_soft

  subroutine nlo_data_init_coll (nlo_data)
    class(nlo_data_t), intent(inout) :: nlo_data
    call nlo_data%sub_coll%init (nlo_data%n_alr)
  end subroutine nlo_data_init_coll

  subroutine nlo_data_init_virtual (nlo_data)
    class(nlo_data_t), intent(inout) :: nlo_data
    call nlo_data%virtual_terms%init (nlo_data%flv_state_born (:,1))
  end subroutine nlo_data_init_virtual

  subroutine nlo_data_set_nlo_type (nlo_data, nlo_type)
   class(nlo_data_t), intent(inout) :: nlo_data
   ! integer, intent(in) :: nlo_type
   type(string_t), intent(in) :: nlo_type
   nlo_data%nlo_type = nlo_type
  end subroutine nlo_data_set_nlo_type

  function nlo_data_get_nlo_type (nlo_data) result(nlo_type)
    class(nlo_data_t), intent(inout) :: nlo_data
    ! integer :: nlo_type
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
    nlo_data%real_kinematics%xi_tilde = 0
    nlo_data%real_kinematics%y = 0
    nlo_data%real_kinematics%xi_max = 0
    nlo_data%real_kinematics%phi = 0
  end subroutine nlo_data_init_real_kinematics

  subroutine nlo_data_set_real_kinematics (nlo_data, xi_tilde, y, phi, xi_max, jac)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), dimension(:), allocatable :: xi_max
    real(default), intent(in) :: xi_tilde
    real(default), intent(in) :: y, phi
    real(default), intent(in), dimension(3) :: jac
    nlo_data%real_kinematics%xi_tilde = xi_tilde
    nlo_data%real_kinematics%y = y
    nlo_data%real_kinematics%phi = phi
    nlo_data%real_kinematics%xi_max = xi_max
    nlo_data%real_kinematics%jac = jac
  end subroutine nlo_data_set_real_kinematics

  subroutine nlo_data_get_real_kinematics &
       (nlo_data, em, xi_tilde, y, xi_max, jac, phi)
    class(nlo_data_t), intent(in) :: nlo_data
    integer, intent(in) :: em
    real(default), intent(out) :: xi_tilde, y, xi_max
    real(default), intent(out), dimension(3), optional :: jac
    !!! For most applications, phi is not relevant. Thus, it is not 
    !!! always transferred as a dummy-variable
    real(default), intent(out), optional :: phi
    xi_tilde = nlo_data%real_kinematics%xi_tilde
    y = nlo_data%real_kinematics%y
    xi_max = nlo_data%real_kinematics%xi_max (em)
    if (present (jac)) jac = nlo_data%real_kinematics%jac
    if (present (phi)) phi = nlo_data%real_kinematics%phi
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

  subroutine nlo_data_set_jacobian (nlo_data, jac)
    class(nlo_data_t), intent(inout) :: nlo_data
    real(default), intent(in), dimension(3) :: jac
    nlo_data%real_kinematics%jac = jac
  end subroutine nlo_data_set_jacobian

  subroutine nlo_data_compute_sub_soft &
       (nlo_data, int_born, p_real, alr, emitter)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(interaction_t), intent(in) :: int_born
    type(vector4_t), intent(in), dimension(:), allocatable :: p_real
    integer, intent(in) :: alr, emitter
    associate (sregion => nlo_data%reg_data%regions(alr))
      if (nlo_data%use_internal_color_correlations) then
        call nlo_data%sub_soft%compute (int_born, p_real, &
                                    nlo_data%sqme_born(sregion%uborn_index), &
                                    nlo_data%color_data%beta_ij (:,:,sregion%uborn_index), &
                                    nlo_data%real_kinematics%y, &
                                    nlo_data%real_kinematics%phi, &
                                    nlo_data%alpha_s_born, &
                                    alr, emitter)
      else
        call nlo_data%sub_soft%compute (int_born, p_real, &
                                    nlo_data%sqme_born(sregion%uborn_index), &
                                    nlo_data%sqme_born_cc (:,:,sregion%uborn_index), &
                                    nlo_data%real_kinematics%y, &
                                    nlo_data%real_kinematics%phi, &
                                    nlo_data%alpha_s_born, &
                                    alr, emitter)
      end if
    end associate
  end subroutine nlo_data_compute_sub_soft

  subroutine nlo_data_compute_sub_coll (nlo_data, int_born, alr)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(interaction_t), intent(in) :: int_born
    integer, intent(in) :: alr
    type(vector4_t), dimension(:), allocatable :: p_born
    integer :: em
    real(default) :: xi
    p_born = interaction_get_momenta (int_born)
    em = nlo_data%get_active_emitter ()
    xi = nlo_data%real_kinematics%xi_tilde * nlo_data%real_kinematics%xi_max (em)
    associate (sregion => nlo_data%reg_data%regions(alr))
    call nlo_data%sub_coll%compute (sregion, p_born, &
                                      nlo_data%sqme_born(sregion%uborn_index), &
                                      nlo_data%sqme_born_sc (:,:,sregion%uborn_index), &
                                      xi, &
                                      nlo_data%alpha_s_born, alr)
    end associate
  end subroutine nlo_data_compute_sub_coll

  subroutine nlo_data_compute_sub_coll_soft (nlo_data, int_born, alr)
    class(nlo_data_t), intent(inout) :: nlo_data
    type(interaction_t), intent(in) :: int_born
    integer, intent(in) :: alr
    type(vector4_t), dimension(:), allocatable :: p_born
    real(default), parameter :: xi = 0
    p_born = interaction_get_momenta (int_born)
    associate (sregion => nlo_data%reg_data%regions(alr))
    call nlo_data%sub_coll%compute_soft_limit (sregion, p_born, &
         nlo_data%sqme_born(sregion%uborn_index), &
         nlo_data%sqme_born_sc (:,:,sregion%uborn_index), &
         xi, nlo_data%alpha_s_born, alr)
    end associate
  end subroutine nlo_data_compute_sub_coll_soft

  function nlo_data_compute_virt (nlo_data, i_proc, int_born) result(sqme_virt)
    class(nlo_data_t), intent(inout) :: nlo_data
    integer, intent(in) :: i_proc
    type(interaction_t), intent(in) :: int_born
    real(default) :: sqme_virt
    type(vector4_t), dimension(:), allocatable :: p_born
    p_born = interaction_get_momenta (int_born)
    if (nlo_data%use_internal_color_correlations) then
      call nlo_data%virtual_terms%evaluate (nlo_data%reg_data, &
                                            i_proc, &
                                            nlo_data%alpha_s_born, &
                                            p_born, &
                                            nlo_data%sqme_born(1), &
                                            nlo_data%color_data%beta_ij)
    else
      call nlo_data%virtual_terms%evaluate (nlo_data%reg_data, &
                                            i_proc, &
                                            nlo_data%alpha_s_born, &
                                            p_born, &
                                            nlo_data%sqme_born(1), &
                                            nlo_data%sqme_born_cc)
    end if
    sqme_virt = nlo_data%virtual_terms%sqme_virt
  end function nlo_data_compute_virt

  subroutine fks_calculation_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (fks_calculation_1, "fks_calculation_1", &
               "Check the creation of color-correlated matrix elements", &
                u, results)
  end subroutine fks_calculation_test

  subroutine fks_calculation_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(nlo_data_t) :: nlo_data
    type(process_constants_t), dimension(2) :: prc_const
    integer, dimension(:,:), allocatable :: flavor_born, flavor_real
    integer, dimension(:,:), allocatable :: cf_index
    complex(default), dimension(:), allocatable :: color_factors
    integer, dimension(:,:,:), allocatable :: col_state_born, col_state_real
    logical, dimension(:,:), allocatable :: ghost_flag_born, ghost_flag_real
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(fks_template_t) :: template

    call os_data_init (os_data)

    allocate (flavor_born (5,1))
    allocate (flavor_real (6,2))
    flavor_born (:,1) = [11, -11, 2, -2, 21]
    flavor_real (:,1) = [11, -11, 2, -2, 21, 21]
    flavor_real (:,2) = [11, -11, 2, -2, 2, -2]

    allocate (col_state_born (2,5,2))
    allocate (col_state_real (2,6,7))
    col_state_born(1,:,1) = [0,0,0,1,2]
    col_state_born(2,:,1) = [0,0,-2,0,-1]
    col_state_born(1,:,2) = [0,0,0,1,0]
    col_state_born(2,:,2) = [0,0,-1,0,0]
    col_state_real(1,:,1) = [0,0,0,1,0,3]
    col_state_real(2,:,1) = [0,0,-3,0,0,-1]
    col_state_real(1,:,2) = [0,0,0,1,2,3]
    col_state_real(2,:,2) = [0,0,-3,0,-1,-2]
    col_state_real(1,:,3) = [0,0,0,1,0,2]
    col_state_real(2,:,3) = [0,0,-2,0,-1,0]
    col_state_real(1,:,4) = [0,0,0,1,2,3]
    col_state_real(2,:,4) = [0,0,-2,0,-3,-1]
    col_state_real(1,:,5) = [0,0,0,1,2,0]
    col_state_real(2,:,5) = [0,0,-2,0,-1,0]
    col_state_real(1,:,6) = [0,0,0,1,0,0]
    col_state_real(2,:,6) = [0,0,-1,0,0,0]
    col_state_real(1,:,7) = [0,0,0,1,0,2]
    col_state_real(2,:,7) = [0,0,-1,0,-2,0]

    allocate (cf_index (2,11))
    cf_index (:,1) = [1,1]
    cf_index (:,2) = [2,2]
    cf_index (:,3) = [2,4]
    cf_index (:,4) = [3,3]
    cf_index (:,5) = [3,7]
    cf_index (:,6) = [4,2]
    cf_index (:,7) = [4,4]
    cf_index (:,8) = [5,5]
    cf_index (:,9) = [6,6]
    cf_index (:,10) = [7,3]
    cf_index (:,11) = [7,7]

    allocate (color_factors (2))
    color_factors(1) = (9,0)
    color_factors(2) = (-1,0)

    allocate (ghost_flag_born (5,2))
    ghost_flag_born = .false.
    
    allocate (ghost_flag_real (6,7))
    ghost_flag_real = .false.
    ghost_flag_real (5,1) = .true.
    ghost_flag_real (6,5) = .true.
    ghost_flag_real (5:6,6) = .true.

    call prc_const(1)%set_flv_state (flavor_born)
    call prc_const(2)%set_flv_state (flavor_real)
    call prc_const(1)%set_col_state (col_state_born)
    call prc_const(2)%set_col_state (col_state_real)
    call prc_const(2)%set_cf_index (cf_index)
    call prc_const(1)%set_color_factors (color_factors)
    call prc_const(1)%set_ghost_flag (ghost_flag_born)
    call prc_const(2)%set_ghost_flag (ghost_flag_real)
    
    call model_list%read_model (var_str ("SM"), var_str ("SM.mdl"), &
         os_data, model)

    call nlo_data%init (var_str ('Real'), prc_const, template, model)

    call nlo_data%color_data%write (u)

  end subroutine fks_calculation_1


end module fks_calculation
