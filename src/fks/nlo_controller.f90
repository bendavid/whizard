! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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

module nlo_controller

  use kinds, only: default
  use io_units
  use constants
  use unit_tests
  use diagnostics
  use physics_defs
  use process_constants !NODEP!
  use sm_physics
  use os_interface
  use model_data
  use pdg_arrays
  use particle_specifiers
  use phs_single
  use state_matrices
  use interactions
  use lorentz
  use prc_core
  use sf_base
  use colors
  use phs_fks
  use flavors
  use fks_regions
  use nlo_data
  use virtual
  use real_subtraction
  use pdf_subtraction

  implicit none
  private

  public :: nlo_controller_t

  public :: operator(*)

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

  type :: polarization_data_t
    logical :: valid = .false.
    real(default) :: value = zero
    integer :: h1 = 0
    integer :: h2 = 0
    !!real(default), dimension(:), allocatable :: pmatrix_diag
  contains
    procedure :: set_helicities => polarization_data_set_helicities
    procedure :: set_value => polarization_data_set_value
    procedure :: is_active => polarization_data_is_valid
    procedure :: write => polarization_data_write 
  end type polarization_data_t

  type :: nlo_controller_t
    logical :: needs_initialization = .true.
    type(region_data_t) :: reg_data
    type(nlo_particle_data_t) :: particle_data
    type(nlo_states_t) :: particle_states
    type(sqme_collector_t), pointer :: sqme_collector => null ()
    type(polarization_data_t), dimension(:), allocatable :: pol_density_matrix
    type(polarization_data_t), dimension(:), allocatable :: pol_sqme
    integer :: n_allowed_born
    integer :: active_emitter
    integer :: active_flavor_structure_real
    complex(default), dimension(:), allocatable :: amp_born
    type(color_data_t) :: color_data
    type(real_kinematics_t), pointer :: real_kinematics => null()
    type(isr_kinematics_t), pointer :: isr_kinematics => null()
    type(virtual_t) :: virtual_terms
    type(real_subtraction_t) :: real_terms
    type(pdf_subtraction_t) :: pdf_terms
    real(default) :: alpha_s_born
    logical :: alpha_s_born_set
    complex(default) :: me_sc
    type(interaction_t), public :: int_born
    type(sf_chain_instance_t) :: sf_born
    type(kinematics_counter_t), public :: counter
    logical, public :: counter_active = .false.
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
    class(powheg_damping_t), allocatable :: powheg_damping
  contains
    procedure :: compute_k_perp => nlo_controller_compute_k_perp
    procedure :: get_k_perp => nlo_controller_get_k_perp
    procedure :: compute_sqme_real_fin => nlo_controller_compute_sqme_real_fin
    procedure :: has_massive_emitter => nlo_controller_has_massive_emitter
    procedure :: get_mass_info => nlo_controller_get_mass_info
    procedure :: set_fixed_order_event_mode => nlo_controller_set_fixed_order_event_mode
    procedure :: set_powheg_mode => nlo_controller_set_powheg_mode
    procedure :: set_real_sqme_born_pointer => nlo_controller_set_real_sqme_born_pointer
    procedure :: init => nlo_controller_init
    procedure :: init_pol_density_matrix => nlo_controller_init_pol_density_matrix
    procedure :: init_polarized_sqmes => nlo_controller_init_polarized_sqmes
    procedure :: beams_are_polarized => nlo_controller_beams_are_polarized 
    procedure :: get_weighted_helicity_sum => nlo_controller_get_weighted_helicity_sum
    procedure :: set_flv_states => nlo_controller_set_flv_states
    procedure :: get_flv_state_real => nlo_controller_get_flv_state_real
    procedure :: set_particle_data => nlo_controller_set_particle_data
    procedure :: setup_matrix_elements => nlo_controller_setup_matrix_elements
    procedure :: setup_generator => nlo_controller_setup_generator
    procedure :: get_n_particles_real => nlo_controller_get_n_particles_real
    procedure :: get_n_particles => nlo_controller_get_n_particles
    procedure :: get_n_flv_born => nlo_controller_get_n_flv_born
    procedure :: get_n_flv_real => nlo_controller_get_n_flv_real
    procedure :: get_n_alr => nlo_controller_get_n_alr
    procedure :: get_n_in => nlo_controller_get_n_in
    procedure :: init_region_data => nlo_controller_init_region_data
    procedure :: get_xi_max => nlo_controller_get_xi_max
    procedure :: init_born_amps => nlo_controller_init_born_amps
    procedure :: set_internal_procedures => nlo_controller_set_internal_procedures
    procedure :: set_x_rad => nlo_controller_set_x_rad
    procedure :: init_virtual => nlo_controller_init_virtual
    procedure :: init_pdf_subtraction => nlo_controller_init_pdf_subtraction
    procedure :: pdf_subtraction_is_required => nlo_controller_pdf_subtraction_is_required
    procedure :: evaluate_pdf_subtraction => nlo_controller_evaluate_pdf_subtraction
    procedure :: get_emitter_list => nlo_controller_get_emitter_list
    procedure :: get_emitter => nlo_controller_get_emitter
    procedure :: set_active_emitter => nlo_controller_set_active_emitter
    procedure :: get_active_emitter => nlo_controller_get_active_emitter
    procedure :: disable_subtraction => nlo_controller_disable_subtraction
    procedure :: enable_subtraction => nlo_controller_enable_subtraction
    procedure :: is_subtraction_active => nlo_controller_is_subtraction_active
    procedure :: disable_sqme_np1 => nlo_controller_disable_sqme_np1
    procedure :: set_alr => nlo_controller_set_alr
    procedure :: set_flv_born => nlo_controller_set_flv_born
    procedure :: set_hel_born => nlo_controller_set_hel_born
    procedure :: set_col_born => nlo_controller_set_col_born
    procedure :: get_flv_born => nlo_controller_get_flv_born
    procedure :: get_hel_born => nlo_controller_get_hel_born
    procedure :: get_col_born => nlo_controller_get_col_born
    procedure :: set_alpha_s_born => nlo_controller_set_alpha_s_born
    procedure :: init_real_and_isr_kinematics &
       => nlo_controller_init_real_and_isr_kinematics
    procedure :: init_isr_kinematics => nlo_controller_init_isr_kinematics
    procedure :: set_real_kinematics => nlo_controller_set_real_kinematics
    procedure :: get_real_kinematics => nlo_controller_get_real_kinematics
    procedure :: set_momenta => nlo_controller_set_momenta
    procedure :: get_momenta => nlo_controller_get_momenta
    procedure :: set_fac_scale => nlo_controller_set_fac_scale
    procedure :: compute_virt => nlo_controller_compute_virt
    procedure :: requires_spin_correlation => &
                    nlo_controller_requires_spin_correlation
  end type nlo_controller_t


  interface operator(*)
     module procedure polarization_data_multiply
  end interface


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
    integer :: n_col_real
    integer, dimension(2) :: col_em, col_rad
    integer :: i
    !!! splitting type: 1 - q -> qg
    !!!                 2 - g -> qq
    !!!                 3 - g -> gg
    integer :: splitting_type_flv, splitting_type_col
    nreg = size (allreg)
    n_col_real = size (color_states_real (1,1,:))
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
          return
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
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    integer :: nlegs_real
    integer :: i, n_in
    nlegs_real = reg_data%nlegs_real
    call prc_constants(1)%get_col_state (color_data%col_state_born)
    call prc_constants(2)%get_col_state (color_data%col_state_real)
    call prc_constants(2)%get_cf_index (color_data%cf_index_real)
    call prc_constants(1)%get_color_factors (color_data%color_factors_born)
    color_data%n_col_born = size (color_data%col_state_born(1,1,:))
    color_data%n_col_real = size (color_data%col_state_real(1,1,:))
    allocate (color_data%ghost_flag_born &
         (size (prc_constants(1)%get_ghost_flag (), 1), &
          size (prc_constants(1)%get_ghost_flag ())))
    allocate (color_data%ghost_flag_real &
         (size (prc_constants(2)%get_ghost_flag (), 1), &
          size (prc_constants(2)%get_ghost_flag ())))    
    color_data%ghost_flag_born = prc_constants(1)%get_ghost_flag ()
    color_data%ghost_flag_real = prc_constants(2)%get_ghost_flag ()
    allocate (color_data%color_real (nlegs_real, color_data%n_col_real))
    allocate (color_data%icm (reg_data%n_regions))
    do i = 1, color_data%n_col_real
      call color_init_from_array (color_data%color_real (:,i), &
           color_data%col_state_real (:,:,i), &
           color_data%ghost_flag_real (:,i))
      n_in = prc_constants(1)%n_in
      call color_data%color_real (1:n_in,i)%invert ()
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
    color_factor = zero; color_factor_born = zero
    found = .false.
    !!! Include distinction between Born flavors
    do i = 1, size (color_data%color_factors_born)
       color_factor_born = color_factor_born + &
            real (color_data%color_factors_born (i))
    end do
    i1 = 1
    i2 = 1
    !!! Catch case em = 0
    if (em1 == 0 .or. em2 == 0) then
       !!! What to do?
       bij = zero
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

           allocate (iarray1 (size (icm1%get_index_array ())))
           allocate (iarray2 (size (icm2%get_index_array ())))           
           
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
                 if (cf_present (i1,i2)) then
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
     deallocate (iarray1, iarray2, iisec1, iisec2, cf_present)
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

  subroutine nlo_controller_compute_k_perp (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer :: emitter
    associate (real_kin => nlo_controller%real_kinematics)
       do emitter = 1, size (real_kin%p_born_cms)
          if (emitter <= 2) then
             call real_kin%compute_k_perp_isr (emitter)
          else
             call real_kin%compute_k_perp_fsr (emitter)
          end if
       end do
    end associate
  end subroutine nlo_controller_compute_k_perp

  function nlo_controller_get_k_perp (nlo_controller) result (k_perp)
    type(vector4_t), dimension(:), allocatable :: k_perp
    class(nlo_controller_t), intent(in) :: nlo_controller
    k_perp = nlo_controller%real_kinematics%k_perp
  end function nlo_controller_get_k_perp

  function nlo_controller_compute_sqme_real_fin &
       (nlo_controller, weight) result (sqme_fin)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    real(default), intent(in) :: weight
    type(vector4_t), dimension(:), allocatable :: p_born
    real(default) :: sqme_fin
    integer :: emitter, i_flv
    if (.not. nlo_controller%alpha_s_born_set) &
      call msg_fatal ("Strong coupling not set for real calculation")
    emitter = nlo_controller%get_active_emitter ()
    p_born = nlo_controller%int_born%get_momenta ()
    i_flv = nlo_controller%active_flavor_structure_real
    call nlo_controller%real_terms%set_real_kinematics &
         (nlo_controller%real_kinematics)
    call nlo_controller%real_terms%set_isr_kinematics &
         (nlo_controller%isr_kinematics)
    sqme_fin = nlo_controller%real_terms%compute &
         (emitter, i_flv, nlo_controller%alpha_s_born)
    sqme_fin = sqme_fin * weight
  end function nlo_controller_compute_sqme_real_fin

  function nlo_controller_has_massive_emitter (nlo_controller) result (val)
    class(nlo_controller_t), intent(in) :: nlo_controller
    logical :: val
    integer :: n_tot, i
    val = .false.
    associate (particle_data => nlo_controller%particle_data)
       n_tot = particle_data%n_in + particle_data%n_out_born
       do i = particle_data%n_in+1, n_tot
          if (any (i == nlo_controller%reg_data%emitters)) &
             val = val .or. nlo_controller%reg_data%flv_born(1)%massive(i)
       end do
    end associate
  end function nlo_controller_has_massive_emitter

  function nlo_controller_get_mass_info (nlo_controller, i_flv) result (massive)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i_flv
    logical, dimension(:), allocatable :: massive
    allocate (massive (size (nlo_controller%reg_data%flv_born(i_flv)%massive)))    
    massive = nlo_controller%reg_data%flv_born(i_flv)%massive
  end function nlo_controller_get_mass_info

  subroutine nlo_controller_set_fixed_order_event_mode (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    nlo_controller%real_terms%purpose = FIXED_ORDER_EVENTS
  end subroutine nlo_controller_set_fixed_order_event_mode

  subroutine nlo_controller_set_powheg_mode (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    nlo_controller%real_terms%purpose = POWHEG
  end subroutine nlo_controller_set_powheg_mode

  subroutine nlo_controller_set_real_sqme_born_pointer (nlo_controller, sqme_born_list)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    real(default), intent(in), dimension(:), target :: sqme_born_list
    nlo_controller%real_terms%sqme_born => sqme_born_list
  end subroutine nlo_controller_set_real_sqme_born_pointer

  function polarization_data_multiply (p1, p2) result (prod)
    real(default) :: prod
    type(polarization_data_t), dimension(:), intent(in) :: p1, p2
    integer :: i, j
    prod = zero
    do i = 1, size (p1)
       do j = 1, size (p2)
          if (p1(i)%h1 == p2(j)%h1 .and. p1(i)%h2 == p2(j)%h2) then
             prod = prod + p1(i)%value * p2(j)%value
             exit
          end if
       end do
    end do
  end function polarization_data_multiply

  subroutine polarization_data_set_helicities (pol_data, h1, h2)
    class(polarization_data_t), intent(inout) :: pol_data
    integer, intent(in) :: h1, h2
    pol_data%h1 = h1; pol_data%h2 = -h2
    pol_data%valid = (pol_data%h1 == 1 .or. pol_data%h1 == -1) &
                .and. (pol_data%h2 == 1 .or. pol_data%h2 == -1)
  end subroutine polarization_data_set_helicities

  subroutine polarization_data_set_value (pol_data, value)
     class(polarization_data_t), intent(inout) :: pol_data
     real(default), intent(in) :: value
     pol_data%value = value
  end subroutine polarization_data_set_value

  elemental function polarization_data_is_valid (pol_data) result (valid)
    logical :: valid
    class(polarization_data_t), intent(in) :: pol_data
    valid = pol_data%valid
  end function polarization_data_is_valid

  subroutine polarization_data_write (pol_data, unit)
    class(polarization_data_t), intent(in) :: pol_data
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)    
    write (u, "(A,1x,L1)") "valid: ", pol_data%valid
    write (u, "(A,1x,F7.5)") "value: ", pol_data%value
    write (u, "(A,1x,I0,1x,I0)") "helicities: ", pol_data%h1, pol_data%h2
  end subroutine polarization_data_write

  subroutine nlo_controller_init (nlo_controller, prc_constants, template, model)
     class(nlo_controller_t), intent(inout) :: nlo_controller
     type(process_constants_t), intent(in), dimension(2) :: prc_constants
     type(fks_template_t), intent(in) :: template
     type(model_data_t), intent(in) :: model
     integer :: n_in, n_tot_born, n_tot_real
     call nlo_controller%set_flv_states (prc_constants)
     call nlo_controller%set_particle_data (prc_constants)
     call nlo_controller%init_region_data (template, model)
     call nlo_controller%setup_matrix_elements ()
     if (nlo_controller%use_internal_color_correlations) &
        call nlo_controller%color_data%init (nlo_controller%reg_data, prc_constants)
     nlo_controller%alpha_s_born_set = .false.
     call nlo_controller%init_real_and_isr_kinematics ()
     associate (particle_data => nlo_controller%particle_data)
        n_in = particle_data%n_in
        n_tot_born = n_in + particle_data%n_out_born
        n_tot_real = n_in + particle_data%n_out_real
        call nlo_controller%real_terms%init (nlo_controller%reg_data, &
           n_in, n_tot_born, n_tot_real, nlo_controller%sqme_collector)
        if (template%subtraction_disabled) call nlo_controller%disable_subtraction ()
     end associate
     nlo_controller%counter_active = template%count_kinematics
     if (nlo_controller%counter_active) call nlo_controller%counter%init(20)
     allocate (powheg_damping_simple_t :: nlo_controller%powheg_damping)
   end subroutine nlo_controller_init

  subroutine nlo_controller_init_pol_density_matrix (nlo_controller, n_entries)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer :: n_entries
    allocate (nlo_controller%pol_density_matrix (n_entries))
  end subroutine nlo_controller_init_pol_density_matrix

  subroutine nlo_controller_init_polarized_sqmes (nlo_controller, helicities)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in), dimension(:,:) :: helicities
    integer :: i_sqme, n_sqme 
    integer :: h1, h2
    integer, dimension(2) :: flavor_sign

    associate (flv => nlo_controller%particle_states%flv_state_born (:,1))
       flavor_sign(1:2) = sign (1, flv(1:2)) 
    end associate

    n_sqme = size (helicities, dim=1)
    allocate (nlo_controller%pol_sqme (n_sqme))
    associate (pol_sqme => nlo_controller%pol_sqme)
       do i_sqme = 1, n_sqme
          h1 = flavor_sign(1) * helicities (i_sqme, 1)
          h2 = flavor_sign(2) * helicities (i_sqme, 2)
          call pol_sqme(i_sqme)%set_helicities (h1, h2)
       end do
    end associate
  end subroutine nlo_controller_init_polarized_sqmes

  elemental function nlo_controller_beams_are_polarized (nlo_controller) result (val)
    logical :: val
    class(nlo_controller_t), intent(in) :: nlo_controller
    val = allocated (nlo_controller%pol_density_matrix)
  end function nlo_controller_beams_are_polarized

  function nlo_controller_get_weighted_helicity_sum (nlo_controller) result (sqme)
     real(default) :: sqme
     class(nlo_controller_t), intent(in) :: nlo_controller
     sqme = four * (nlo_controller%pol_sqme * nlo_controller%pol_density_matrix)
  end function nlo_controller_get_weighted_helicity_sum

  subroutine nlo_controller_set_flv_states (nlo_controller, prc_constants)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    associate (states => nlo_controller%particle_states)
      allocate (states%flv_state_born &
           (size (prc_constants(1)%get_flv_state (), 1), &
            size (prc_constants(1)%get_flv_state (), 2)))
      allocate (states%flv_state_real &
           (size (prc_constants(2)%get_flv_state (), 1), &
            size (prc_constants(2)%get_flv_state (), 2)))      
      states%flv_state_born = prc_constants(1)%get_flv_state ()
      states%flv_state_real = prc_constants(2)%get_flv_state ()
    end associate
  end subroutine nlo_controller_set_flv_states

  function nlo_controller_get_flv_state_real (nlo_controller, i_uborn) result (flv_state)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i_uborn
    integer, dimension(:), allocatable :: flv_state
    allocate (flv_state (size (nlo_controller%particle_states%flv_state_real (:,i_uborn))))
    flv_state = nlo_controller%particle_states%flv_state_real (:,i_uborn)
  end function nlo_controller_get_flv_state_real

  subroutine nlo_controller_set_particle_data (nlo_controller, prc_constants)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    associate (particle_data => nlo_controller%particle_data)
       particle_data%n_flv_born = size (nlo_controller%particle_states%flv_state_born(1,:))
       particle_data%n_flv_real = size (nlo_controller%particle_states%flv_state_real(1,:))
       particle_data%n_in = prc_constants(2)%n_in
       particle_data%n_out_born = prc_constants(1)%n_out
       particle_data%n_out_real = prc_constants(2)%n_out
    end associate
  end subroutine nlo_controller_set_particle_data

  subroutine nlo_controller_setup_matrix_elements (nlo_controller, n_hel)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in), optional :: n_hel
    integer :: n_tot_born
    associate (collector => nlo_controller%sqme_collector, &
         particle_data => nlo_controller%particle_data)
      if (present (n_hel)) then
         allocate (collector%sqme_virt_born_list (particle_data%n_flv_born, n_hel))
         allocate (collector%sqme_virt_list (particle_data%n_flv_born, n_hel))
      else
         allocate (collector%sqme_virt_born_list (particle_data%n_flv_born, 1))
         allocate (collector%sqme_virt_list (particle_data%n_flv_born, 1))
      end if
      allocate (collector%sqme_born_list (particle_data%n_flv_born))
      allocate (collector%sqme_real_non_sub (particle_data%n_flv_real))
      allocate (collector%sqme_real_per_emitter &
           (nlo_controller%reg_data%n_flv_real, &
           particle_data%n_in + particle_data%n_out_born))
      n_tot_born = particle_data%n_in + particle_data%n_out_born
      allocate (collector%sqme_born_cc (n_tot_born, n_tot_born, particle_data%n_flv_born))
      allocate (collector%sqme_born_sc (particle_data%n_flv_born))
      collector%sqme_born_list = 0._default
      collector%sqme_real_non_sub = 0._default
      collector%sqme_real_per_emitter = 0._default
      collector%sqme_born_cc = 0._default
      collector%sqme_born_sc = cmplx (0._default, 0._default, kind=default)
      collector%current_sqme_real = 0._default
      collector%sqme_real_sum = 0._default
      collector%sqme_virt_born_list = 0._default
      collector%sqme_virt_list = 0._default
    end associate
  end subroutine nlo_controller_setup_matrix_elements

  subroutine nlo_controller_setup_generator &
         (nlo_controller, generator, sqrts, singular_jacobian)
    class(nlo_controller_t), intent(in) :: nlo_controller
    type(phs_fks_generator_t), intent(out) :: generator
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: singular_jacobian
    logical :: yorn
    yorn = .false.; if (present (singular_jacobian)) yorn = singular_jacobian
    call generator%connect_kinematics (nlo_controller%isr_kinematics, &
         nlo_controller%real_kinematics, &
         nlo_controller%has_massive_emitter ())
    generator%n_in = nlo_controller%particle_data%n_in
    call generator%set_beam_energy (sqrts)
    call generator%set_emitters (nlo_controller%reg_data%emitters)
    call generator%setup_masses (nlo_controller%particle_data%n_in + &
                           nlo_controller%particle_data%n_out_born)
    generator%is_massive = nlo_controller%get_mass_info(1)
    generator%singular_jacobian = yorn
  end subroutine nlo_controller_setup_generator

  pure function nlo_controller_get_n_particles_real (nlo_controller) result (n_particles)
    integer :: n_particles
    class(nlo_controller_t), intent(in) :: nlo_controller
    n_particles = nlo_controller%particle_data%n_in + nlo_controller%particle_data%n_out_real
  end function nlo_controller_get_n_particles_real

  elemental function nlo_controller_get_n_particles (nlo_controller) result (n)
    integer :: n
    class(nlo_controller_t), intent(in) :: nlo_controller
    associate (particle_data => nlo_controller%particle_data)
       n = particle_data%n_in + particle_data%n_out_born
    end associate
  end function nlo_controller_get_n_particles

  elemental function nlo_controller_get_n_flv_born (nlo_controller) result (n_flv)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer :: n_flv
    n_flv = nlo_controller%particle_data%n_flv_born
  end function nlo_controller_get_n_flv_born

  elemental function nlo_controller_get_n_flv_real (nlo_controller) result (n_flv)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer :: n_flv
    n_flv = nlo_controller%particle_data%n_flv_real
  end function nlo_controller_get_n_flv_real

  elemental function nlo_controller_get_n_alr (nlo_controller) result (n_alr)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer :: n_alr
    n_alr = nlo_controller%reg_data%n_regions
  end function nlo_controller_get_n_alr

  function nlo_controller_get_n_in (controller) result (n_in)
    integer :: n_in
    class(nlo_controller_t), intent(in) :: controller
    n_in = controller%particle_data%n_in
  end function nlo_controller_get_n_in

  subroutine nlo_controller_init_region_data (nlo_controller, template, model)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    type(fks_template_t), intent(in) :: template
    type(model_data_t), intent(in) :: model
    integer :: n_in
    associate (states => nlo_controller%particle_states)
       n_in = nlo_controller%particle_data%n_in
       call nlo_controller%reg_data%init (n_in, model, &
                     states%flv_state_born, states%flv_state_real, &
                     template%mapping_type)
       call nlo_controller%reg_data%write_to_file (template%id)
    end associate
    select type (mapping => nlo_controller%reg_data%fks_mapping)
    type is (fks_mapping_default_t)
       call mapping%set_parameter (n_in, template%fks_dij_exp1, template%fks_dij_exp2)
    end select
  end subroutine nlo_controller_init_region_data

  function nlo_controller_get_xi_max (nlo_controller, alr) result (xi_max)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: alr
    real(default) :: xi_max
    integer :: emitter
    emitter = nlo_controller%get_emitter (alr)
    xi_max = nlo_controller%real_kinematics%xi_max (emitter)
  end function nlo_controller_get_xi_max

  subroutine nlo_controller_init_born_amps (nlo_controller, n)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in) :: n
    nlo_controller%n_allowed_born = n
    if (.not. allocated (nlo_controller%amp_born)) &
       allocate (nlo_controller%amp_born (n))
  end subroutine nlo_controller_init_born_amps

  subroutine nlo_controller_set_internal_procedures (nlo_controller, flag_color, flag_spin)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    logical, intent(in) :: flag_color, flag_spin
    nlo_controller%use_internal_color_correlations = flag_color
    nlo_controller%real_terms%sub_soft%use_internal_color_correlations = flag_color
    nlo_controller%virtual_terms%use_internal_color_correlations = flag_color
    nlo_controller%use_internal_spin_correlations = flag_spin
  end subroutine nlo_controller_set_internal_procedures

  subroutine nlo_controller_set_x_rad (controller, x_rad)
    class(nlo_controller_t), intent(inout) :: controller
    real(default), intent(in), dimension(:) :: x_rad
    integer :: n_par
    n_par = size (x_rad)
    if (associated (controller%real_kinematics)) &
       controller%real_kinematics%x_rad = x_rad (n_par-2:n_par)
  end subroutine nlo_controller_set_x_rad

  subroutine nlo_controller_init_virtual (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    call nlo_controller%virtual_terms%init (nlo_controller%particle_states%flv_state_born, &
       nlo_controller%particle_data%n_in)
  end subroutine nlo_controller_init_virtual

  subroutine nlo_controller_init_pdf_subtraction (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    call nlo_controller%pdf_terms%init (nlo_controller%isr_kinematics, &
                                        nlo_controller%particle_states%flv_state_born, &
                                        nlo_controller%reg_data%n_regions, &
                                        nlo_controller%sqme_collector)
  end subroutine nlo_controller_init_pdf_subtraction

  function nlo_controller_pdf_subtraction_is_required (nlo_controller) result (required)
    class(nlo_controller_t), intent(in) :: nlo_controller
    logical :: required
    required = nlo_controller%pdf_terms%required
  end function nlo_controller_pdf_subtraction_is_required

  subroutine nlo_controller_evaluate_pdf_subtraction (nlo_controller, sqme)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    real(default), intent(inout) :: sqme
    if (.not. nlo_controller%alpha_s_born_set) &
        call msg_fatal ("Strong coupling not set for pdf subtraction")
    call nlo_controller%pdf_terms%evaluate (nlo_controller%alpha_s_born, sqme, 1)
  end subroutine nlo_controller_evaluate_pdf_subtraction

  pure function nlo_controller_get_emitter_list (nlo_controller) result(emitters)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, dimension(:), allocatable :: emitters
    allocate (emitters (size (nlo_controller%reg_data%get_emitter_list ())))
    emitters = nlo_controller%reg_data%get_emitter_list ()
  end function nlo_controller_get_emitter_list

  pure function nlo_controller_get_emitter (nlo_controller, alr) result (emitter)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: alr
    integer :: emitter
    emitter = nlo_controller%reg_data%get_emitter (alr)
  end function nlo_controller_get_emitter

  subroutine nlo_controller_set_active_emitter (nlo_controller, emitter)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in) :: emitter
    nlo_controller%active_emitter = emitter
  end subroutine nlo_controller_set_active_emitter

  function nlo_controller_get_active_emitter (nlo_controller) result(emitter)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer :: emitter
    emitter = nlo_controller%active_emitter
  end function nlo_controller_get_active_emitter

  subroutine nlo_controller_disable_subtraction (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    nlo_controller%real_terms%radiation_active = .true.
    nlo_controller%real_terms%subtraction_active = .false.
  end subroutine nlo_controller_disable_subtraction

  subroutine nlo_controller_enable_subtraction (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    nlo_controller%real_terms%subtraction_active = .true.
  end subroutine nlo_controller_enable_subtraction

  function nlo_controller_is_subtraction_active (nlo_controller) result (active)
    class(nlo_controller_t), intent(in) :: nlo_controller
    logical :: active
    active = nlo_controller%real_terms%subtraction_active
  end function nlo_controller_is_subtraction_active

  subroutine nlo_controller_disable_sqme_np1 (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    nlo_controller%real_terms%radiation_active = .false.
    nlo_controller%real_terms%subtraction_active = .true.
  end subroutine nlo_controller_disable_sqme_np1

  subroutine nlo_controller_set_alr (nlo_controller, alr)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in) :: alr
    call nlo_controller%real_terms%set_alr (alr)
  end subroutine nlo_controller_set_alr

  subroutine nlo_controller_set_flv_born (nlo_controller, flv_in)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in), dimension(:), allocatable :: flv_in
    associate (states => nlo_controller%particle_states)
       allocate (states%flv_born (size (flv_in)))
       states%flv_born = flv_in
    end associate
  end subroutine nlo_controller_set_flv_born

  subroutine nlo_controller_set_hel_born (nlo_controller, hel_in)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in), dimension(:), allocatable :: hel_in
    associate (states => nlo_controller%particle_states)
       allocate (states%hel_born (size (hel_in)))
       states%hel_born = hel_in
    end associate
  end subroutine nlo_controller_set_hel_born

  subroutine nlo_controller_set_col_born (nlo_controller, col_in)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in), dimension(:), allocatable :: col_in
    associate (states => nlo_controller%particle_states)
       allocate (states%col_born (size (col_in)))
       states%col_born = col_in
   end associate
  end subroutine nlo_controller_set_col_born

  elemental function nlo_controller_get_flv_born (nlo_controller, i) result  (flv)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i
    integer :: flv
    flv = nlo_controller%particle_states%flv_born(i)
  end function nlo_controller_get_flv_born

  elemental function nlo_controller_get_hel_born (nlo_controller, i) result (hel)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i
    integer :: hel
    hel = nlo_controller%particle_states%hel_born (i)
  end function nlo_controller_get_hel_born

  elemental function nlo_controller_get_col_born (nlo_controller, i) result (col)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i
    integer :: col
    col = nlo_controller%particle_states%col_born (i)
  end function nlo_controller_get_col_born

  subroutine nlo_controller_set_alpha_s_born (nlo_controller, as_born)
    class (nlo_controller_t), intent(inout) :: nlo_controller
    real(default), intent(in) :: as_born
    nlo_controller%alpha_s_born = as_born
    nlo_controller%alpha_s_born_set = .true.
  end subroutine nlo_controller_set_alpha_s_born

  subroutine nlo_controller_init_real_and_isr_kinematics (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer :: n_tot
    n_tot = nlo_controller%particle_data%n_in + &
            nlo_controller%particle_data%n_out_born
    allocate (nlo_controller%real_kinematics)
    allocate (nlo_controller%isr_kinematics)
    call nlo_controller%real_kinematics%init (n_tot)
    nlo_controller%isr_kinematics%n_in = nlo_controller%particle_data%n_in
  end subroutine nlo_controller_init_real_and_isr_kinematics

  subroutine nlo_controller_init_isr_kinematics (nlo_controller)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    if (.not. associated (nlo_controller%isr_kinematics)) &
       allocate (nlo_controller%isr_kinematics)
   end subroutine nlo_controller_init_isr_kinematics

  subroutine nlo_controller_set_real_kinematics (nlo_controller, xi_tilde, y, phi, xi_max, &
                                           jac, jac_rand)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    real(default), dimension(:), allocatable :: xi_max, y
    real(default), intent(in) :: xi_tilde
    real(default), intent(in) :: phi
    real(default), intent(in), dimension(4) :: jac
    real(default), intent(in), dimension(:), allocatable :: jac_rand
    nlo_controller%real_kinematics%xi_tilde = xi_tilde
    nlo_controller%real_kinematics%y = y
    nlo_controller%real_kinematics%phi = phi
    nlo_controller%real_kinematics%xi_max = xi_max
    nlo_controller%real_kinematics%jac(1)%jac = jac
    nlo_controller%real_kinematics%jac_rand = jac_rand
  end subroutine nlo_controller_set_real_kinematics

  subroutine nlo_controller_get_real_kinematics &
       (nlo_controller, em, xi_tilde, y, xi_max, jac, phi, jac_rand)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: em
    real(default), intent(out) :: xi_tilde, y, xi_max
    real(default), intent(out), dimension(4), optional :: jac
    !!! For most applications, phi is not relevant. Thus, it is not
    !!! always transferred as a dummy-variable
    real(default), intent(out), optional :: phi
    real(default), intent(out), dimension(:), optional :: jac_rand
    xi_tilde = nlo_controller%real_kinematics%xi_tilde
    y = nlo_controller%real_kinematics%y(em)
    xi_max = nlo_controller%real_kinematics%xi_max (em)
    if (present (jac)) jac = nlo_controller%real_kinematics%jac(1)%jac
    if (present (phi)) phi = nlo_controller%real_kinematics%phi
    if (present (jac_rand)) jac_rand = nlo_controller%real_kinematics%jac_rand
  end subroutine nlo_controller_get_real_kinematics

  subroutine nlo_controller_set_momenta (nlo_controller, p_born, p_real, cms)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    type(vector4_t), dimension(:), intent(in) :: p_born, p_real
    logical, intent(in), optional :: cms
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    associate (kinematics => nlo_controller%real_kinematics)
       if (yorn) then
          if (.not. allocated (kinematics%p_born_cms)) then
             allocate (kinematics%p_born_cms (size (p_born)))
          end if
          if (.not. allocated (kinematics%p_real_cms)) then
             allocate (kinematics%p_real_cms (size (p_real)))
          end if
          kinematics%p_born_cms = p_born
          kinematics%p_real_cms = p_real
       else
          if (.not. allocated (kinematics%p_born_lab)) then
             allocate (kinematics%p_born_lab (size (p_born)))
          end if
          if (.not. allocated (kinematics%p_real_lab)) then
             allocate (kinematics%p_real_lab (size (p_real)))
          end if
          kinematics%p_born_lab = p_born
          kinematics%p_real_lab = p_real
       end if
    end associate
  end subroutine nlo_controller_set_momenta

  function nlo_controller_get_momenta (nlo_controller, born_phsp, cms) result (p)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    logical, intent(in) :: born_phsp
    logical, intent(in), optional :: cms
    type(vector4_t), dimension(:), allocatable :: p
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    if (born_phsp) then
       if (yorn) then
          allocate (p(1:size(nlo_controller%real_kinematics%p_born_cms)), &
               source = nlo_controller%real_kinematics%p_born_cms)
       else
          allocate (p(1:size(nlo_controller%real_kinematics%p_born_lab)), &
               source = nlo_controller%real_kinematics%p_born_lab)
       end if
    else
       if (yorn) then
          allocate (p(1:size(nlo_controller%real_kinematics%p_real_cms)), &
               source = nlo_controller%real_kinematics%p_real_cms)
       else
          allocate (p(1:size(nlo_controller%real_kinematics%p_real_lab)), &
               source = nlo_controller%real_kinematics%p_real_lab)
       end if
    end if
  end function nlo_controller_get_momenta

  subroutine nlo_controller_set_fac_scale (nlo_controller, fac_scale)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    real(default), intent(in) :: fac_scale
    if (associated (nlo_controller%isr_kinematics)) &
       nlo_controller%isr_kinematics%fac_scale = fac_scale
  end subroutine nlo_controller_set_fac_scale

  function nlo_controller_compute_virt &
       (nlo_controller, i_flv, i_hel, p_born) result(sqme_virt)
    class(nlo_controller_t), intent(inout) :: nlo_controller
    integer, intent(in) :: i_flv, i_hel
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default) :: sqme_virt
    associate (collector => nlo_controller%sqme_collector)
       if (nlo_controller%use_internal_color_correlations) then
          call nlo_controller%virtual_terms%evaluate &
               (nlo_controller%reg_data, &
               i_flv, nlo_controller%alpha_s_born, &
               p_born, collector%sqme_virt_born_list (i_flv, i_hel), &
               nlo_controller%color_data%beta_ij)
       else
          call nlo_controller%virtual_terms%evaluate &
               (nlo_controller%reg_data, &
               i_flv, nlo_controller%alpha_s_born, &
               p_born, collector%sqme_virt_born_list (i_flv, i_hel), &
               collector%sqme_born_cc)
       end if
    end associate
    sqme_virt = nlo_controller%virtual_terms%sqme_virt
  end function nlo_controller_compute_virt

  function nlo_controller_requires_spin_correlation &
       (nlo_controller, i_flv) result (val)
    class(nlo_controller_t), intent(in) :: nlo_controller
    integer, intent(in) :: i_flv
    logical :: val
    val = nlo_controller%real_terms%sc_required (i_flv)
  end function nlo_controller_requires_spin_correlation


end module nlo_controller
