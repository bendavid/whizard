! WHIZARD 2.3.1 Aug 25 2016
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

module real_subtraction

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use string_utils
  use constants
  use numeric_utils
  use diagnostics
  use pdg_arrays
  use models
  use physics_defs
  use sm_physics
  use sf_lhapdf
  use pdf
  use lorentz
  use flavors
  use fks_regions
  use nlo_data
  use ttv_formfactors, only: THR_POS_B, THR_POS_BBAR
  use ttv_formfactors, only: THR_POS_WP, THR_POS_WM

  implicit none
  private

  public :: soft_mismatch_t
  public :: real_subtraction_t

  integer, parameter, public :: INTEGRATION = 0
  integer, parameter, public :: FIXED_ORDER_EVENTS = 1
  integer, parameter, public :: POWHEG = 2



  type :: soft_subtraction_t
    real(default), dimension(:), allocatable :: value
    type(region_data_t), pointer :: reg_data => null ()
    integer :: n_in, nlegs_born, nlegs_real
    real(default), dimension(:,:), allocatable :: momentum_matrix
    logical :: use_resonance_mappings = .false.
    type(vector4_t) :: p_soft = vector4_null
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
    type(pdf_container_t), pointer :: pdf_born_plus => null ()
    type(pdf_container_t), pointer :: pdf_born_minus => null ()
    logical :: xi2_expanded = .true.
    integer :: factorization_mode = NO_FACTORIZATION
  contains
    procedure :: init => soft_subtraction_init
    procedure :: create_softvec_fsr => soft_subtraction_create_softvec_fsr 
    procedure :: create_softvec_isr => soft_subtraction_create_softvec_isr
    procedure :: create_softvec_mismatch => &
       soft_subtraction_create_softvec_mismatch
    procedure :: compute => soft_subtraction_compute
    procedure :: evaluate_factorization_default => &
       soft_subtraction_evaluate_factorization_default
    procedure :: compute_momentum_matrix => &
         soft_subtraction_compute_momentum_matrix
    procedure :: evaluate_factorization_threshold => &
       soft_subtraction_evaluate_factorization_threshold
  end type soft_subtraction_t

  type :: soft_mismatch_t
    logical :: active = .true.
    type(region_data_t), pointer :: reg_data => null ()
    real(default), dimension(:), pointer :: sqme_born => null ()
    real(default), dimension(:,:,:), pointer :: sqme_born_cc => null ()
    type(real_kinematics_t), pointer :: real_kinematics => null ()
    type(soft_subtraction_t) :: sub_soft
  contains
    procedure :: init => soft_mismatch_init
    procedure :: evaluate => soft_mismatch_evaluate
    procedure :: compute => soft_mismatch_compute
  end type soft_mismatch_t

  type :: coll_subtraction_t
    real(default), dimension(:), allocatable :: value
    real(default), dimension(:), allocatable :: value_soft
    integer :: n_in, n_alr
    logical :: use_resonance_mappings = .false.
    type(pdf_container_t), pointer :: pdf_born_plus => null ()
    type(pdf_container_t), pointer :: pdf_born_minus => null ()
    type(pdf_container_t), pointer :: pdf_scaled_plus => null ()
    type(pdf_container_t), pointer :: pdf_scaled_minus => null ()
  contains
    procedure :: init => coll_subtraction_init
    procedure :: compute_fsr => coll_subtraction_compute_fsr
    procedure :: compute_soft_limit_fsr => coll_subtraction_compute_soft_limit_fsr
    procedure :: compute_isr => coll_subtraction_compute_isr
    procedure :: compute_soft_limit_isr => coll_subtraction_compute_soft_limit_isr
  end type coll_subtraction_t

  type :: real_subtraction_t
    type(region_data_t), pointer :: reg_data => null ()
    type(pdf_data_t) :: pdf_data
    type(real_kinematics_t), pointer :: real_kinematics => null ()
    type(isr_kinematics_t), pointer :: isr_kinematics => null ()
    type(real_scales_t) :: scales
    integer :: current_alr = 0
    integer :: current_i_res = 0
    real(default), dimension(:,:), pointer :: sqme_real_non_sub => null ()
    real(default), dimension(:), pointer :: sqme_born => null ()
    real(default), dimension(:,:,:), pointer :: sqme_born_cc => null ()
    complex(default), dimension(:), pointer :: sqme_born_sc => null ()
    type(soft_subtraction_t) :: sub_soft
    type(coll_subtraction_t) :: sub_coll
    logical, dimension(:), allocatable :: sc_required
    integer :: purpose = INTEGRATION
    logical :: radiation_active = .true.
    logical :: subtraction_active = .true.
    type(pdf_container_t), dimension(2) :: pdf_born, pdf_scaled, pdf_scaled_coll
    logical, dimension(:), pointer :: passed_real_cuts => null ()
    logical, pointer :: passed_born_cuts => null ()
    integer :: fixed_alr = 0
  contains
    procedure :: init => real_subtraction_init
    procedure :: init_pdfs => real_subtraction_init_pdfs
    procedure :: set_resonance_mappings => real_subtraction_set_resonance_mappings
    procedure :: set_real_kinematics => real_subtraction_set_real_kinematics
    procedure :: set_isr_kinematics => real_subtraction_set_isr_kinematics
    procedure :: set_alr => real_subtraction_set_alr
    procedure :: set_i_res => real_subtraction_set_i_res
    procedure :: compute => real_subtraction_compute
    procedure :: evaluate_emitter_region => real_subtraction_evaluate_emitter_region
    procedure :: evaluate_region_fsr => real_subtraction_evaluate_region_fsr
    procedure :: evaluate_region_isr => real_subtraction_evaluate_region_isr
    procedure :: evaluate_subtraction_terms_fsr => &
                         real_subtraction_evaluate_subtraction_terms_fsr
    procedure :: evaluate_subtraction_terms_isr => &
                         real_subtraction_evaluate_subtraction_terms_isr
    procedure :: get_phs_factor => real_subtraction_get_phs_factor
    procedure :: get_i_contributor => real_subtraction_get_i_contributor
    procedure :: compute_sub_soft => real_subtraction_compute_sub_soft
    procedure :: get_sc_matrix_element => real_subtraction_get_sc_matrix_element
    procedure :: compute_sub_coll => real_subtraction_compute_sub_coll
    procedure :: compute_sub_coll_soft => real_subtraction_compute_sub_coll_soft
    procedure :: compute_pdfs => real_subtraction_compute_pdfs
    procedure :: scale_pdfs_real => real_subtraction_scale_pdfs_real
    procedure :: scale_pdfs_collinear => real_subtraction_scale_pdfs_collinear
  end type real_subtraction_t


contains

  subroutine soft_subtraction_init (sub_soft, reg_data, &
      n_in, nlegs_born, nlegs_real)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(region_data_t), intent(in), target :: reg_data
    integer, intent(in) :: n_in, nlegs_born, nlegs_real
    call msg_debug (D_SUBTRACTION, "soft_subtraction_init")
    sub_soft%reg_data => reg_data
    sub_soft%n_in = n_in
    sub_soft%nlegs_born = nlegs_born
    sub_soft%nlegs_real = nlegs_real
    allocate (sub_soft%value (reg_data%n_regions))
    allocate (sub_soft%momentum_matrix (nlegs_born, nlegs_born))
  end subroutine soft_subtraction_init

  subroutine soft_subtraction_create_softvec_fsr &
     (sub_soft, p_born, y, phi, emitter, xi_ref_momentum)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: y, phi
    integer, intent(in) :: emitter
    type(vector4_t), intent(in) :: xi_ref_momentum
    type(vector3_t) :: dir
    type(vector4_t) :: p_em
    type(lorentz_transformation_t) :: rot
    type(lorentz_transformation_t) :: boost_to_rest_frame
    sub_soft%p_soft%p(0) = one
    if (sub_soft%use_resonance_mappings) then
       boost_to_rest_frame = inverse (boost (xi_ref_momentum, xi_ref_momentum**1))
       p_em = boost_to_rest_frame * p_born(emitter)
    else
       p_em = p_born(emitter)
    end if
    sub_soft%p_soft%p(1:3) = p_em%p(1:3) / space_part_norm (p_em)
    dir = create_orthogonal (space_part (p_em))
    rot = rotation (y, sqrt(one - y**2), dir)
    sub_soft%p_soft = rot * sub_soft%p_soft
    if (.not. vanishes (phi)) then
      dir = space_part (p_em) / space_part_norm (p_em)
      rot = rotation (cos(phi), sin(phi), dir)
      sub_soft%p_soft = rot * sub_soft%p_soft
    end if
    if (sub_soft%use_resonance_mappings) &
       sub_soft%p_soft = inverse (boost_to_rest_frame) * sub_soft%p_soft
  end subroutine soft_subtraction_create_softvec_fsr

  subroutine soft_subtraction_create_softvec_isr (sub_soft, y, phi)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    real(default), intent(in) :: y, phi
    real(default) :: sin_theta
    sin_theta = sqrt(one - y**2)
    associate (p => sub_soft%p_soft%p)
       p(0) = one
       p(1) = sin_theta * sin(phi)
       p(2) = sin_theta * cos(phi)
       p(3) = y
    end associate
  end subroutine soft_subtraction_create_softvec_isr

  subroutine soft_subtraction_create_softvec_mismatch (sub_soft, E, y, phi, p_em)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    real(default), intent(in) :: E, phi, y
    type(vector4_t), intent(in) :: p_em
    real(default) :: sin_theta
    type(lorentz_transformation_t) :: rot_em_off_3_axis
    sin_theta = sqrt (one - y**2)
    associate (p => sub_soft%p_soft%p)
       p(0) = E
       p(1) = E * sin_theta * sin(phi)
       p(2) = E * sin_theta * cos(phi)
       p(3) = E * y
    end associate
    rot_em_off_3_axis = rotation_to_2nd (3, space_part (p_em))
    sub_soft%p_soft = rot_em_off_3_axis * sub_soft%p_soft
  end subroutine soft_subtraction_create_softvec_mismatch

  subroutine soft_subtraction_compute (sub_soft, p_born, &
     born_ij, y, q2, alpha_s, alr, emitter, i_res)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in), dimension(:,:) :: born_ij
    real(default), intent(in) :: y
    real(default), intent(in) :: q2, alpha_s
    integer, intent(in) :: alr, emitter, i_res
    real(default) :: s_alpha_soft
    real(default) :: kb
    real(default) :: xi2_factor

    if (.not. vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true.)
       call msg_fatal ("Soft subtraction: phase space point must be in CMS")
    end if
    if (debug2_active (D_SUBTRACTION)) then
       print *, 'Compute soft subtraction using alpha_s = ', alpha_s
    end if

    s_alpha_soft = sub_soft%reg_data%get_svalue_soft &
        (p_born, sub_soft%p_soft, alr, emitter, i_res)
    if (debug2_active (D_SUBTRACTION)) &
       call msg_print_color ('s_alpha_soft', s_alpha_soft, COL_YELLOW)
    sub_soft%value(alr) = 4 * pi * alpha_s * s_alpha_soft
    select case (sub_soft%factorization_mode)
    case (NO_FACTORIZATION)
       kb = sub_soft%evaluate_factorization_default (p_born, born_ij)
    case (FACTORIZATION_THRESHOLD)
       kb = sub_soft%evaluate_factorization_threshold (p_born, born_ij)
    end select
    call msg_debug2 (D_SUBTRACTION, 'KB', kb)
    sub_soft%value(alr) = sub_soft%value(alr) * kb
    if (sub_soft%xi2_expanded) then
       xi2_factor = 4 / q2
    else
       xi2_factor = one
    end if
    if (emitter <= sub_soft%n_in) then
       sub_soft%value(alr) = xi2_factor * (one - y**2) * sub_soft%value(alr)
    else
       sub_soft%value(alr) = xi2_factor * (one - y) * sub_soft%value(alr)
    end if
  end subroutine soft_subtraction_compute

  function soft_subtraction_evaluate_factorization_default &
     (sub_soft, p, born_ij) result (kb)
    real(default) :: kb
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in), dimension(:,:) :: born_ij
    integer :: i, j
    kb = zero
    call sub_soft%compute_momentum_matrix (p)
    do i = 1, size (p)
       do j = 1, size (p)
          kb = kb + sub_soft%momentum_matrix (i, j) * born_ij (i, j)
       end do
    end do
  end function soft_subtraction_evaluate_factorization_default

  subroutine soft_subtraction_compute_momentum_matrix &
       (sub_soft, p_born)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default) :: num, deno1, deno2
    integer :: i, j
    do i = 1, sub_soft%nlegs_born
      do j = 1, sub_soft%nlegs_born
        if (i <= j) then
           num = p_born(i) * p_born(j)
           deno1 = p_born(i) * sub_soft%p_soft
           deno2 = p_born(j) * sub_soft%p_soft
           sub_soft%momentum_matrix(i,j) = num / (deno1 * deno2)
        else
           !!! momentum matrix is symmetric.
          sub_soft%momentum_matrix(i, j) = sub_soft%momentum_matrix(j, i)
        end if
      end do
    end do
  end subroutine soft_subtraction_compute_momentum_matrix

  function soft_subtraction_evaluate_factorization_threshold &
     (sub_soft, p_born, born_ij) result (kb)
    real(default) :: kb
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in), dimension(:,:) :: born_ij
    type(vector4_t), dimension(4) :: p
    p(1) = p_born(THR_POS_WP) + p_born(THR_POS_B)
    p(2) = p_born(THR_POS_B)
    p(3) = p_born(THR_POS_WM) + p_born(THR_POS_BBAR)
    p(4) = p_born(THR_POS_BBAR)
    kb = evaluate_leg_pair (1) + evaluate_leg_pair (3)
    if (debug2_active (D_SUBTRACTION))  call show_debug ()

  contains

    function evaluate_leg_pair (i_start) result (kbb)
      real(default) :: kbb
      integer, intent(in) :: i_start
      integer :: i1, i2
      real(default) :: numerator, deno1, deno2
      kbb = zero
      do i1 = i_start, i_start + 1
         do i2 = i_start, i_start + 1
            numerator = p(i1) * p(i2)
            deno1 = p(i1) * sub_soft%p_soft
            deno2 = p(i2) * sub_soft%p_soft
            kbb = kbb +  numerator * born_ij (i1, i2) / deno1 / deno2
         end do
      end do
      if (debug2_active (D_SUBTRACTION)) then
      do i1 = i_start, i_start + 1
         do i2 = i_start, i_start + 1
            call msg_print_color('i1', i1, COL_PEACH)
            call msg_print_color('i2', i2, COL_PEACH)
            call msg_print_color('born_ij(i1,i2)', born_ij(i1,i2), COL_PINK)
            numerator = p(i1) * p(i2)
            deno1 = p(i1) * sub_soft%p_soft
            deno2 = p(i2) * sub_soft%p_soft
            kbb = kbb +  numerator * born_ij (i1, i2) / deno1 / deno2
         end do
      end do
      end if
    end function evaluate_leg_pair

    subroutine show_debug ()
      integer :: i
      call msg_print_color ('soft_subtraction_evaluate_factorization_threshold', COL_GREEN)
      do i = 1, 4
         print *, 'sqrt(p(i)**2) =    ', sqrt(p(i)**2)
      end do
      print *, 'p_soft =    ', sub_soft%p_soft
    end subroutine show_debug

  end function soft_subtraction_evaluate_factorization_threshold

  subroutine soft_mismatch_init (soft_mismatch, n_in, nlegs_born, &
     reg_data, sqme_collector, real_kinematics)
    class(soft_mismatch_t), intent(inout) :: soft_mismatch
    integer, intent(in) :: n_in, nlegs_born
    type(region_data_t), intent(in), target :: reg_data
    type(sqme_collector_t), intent(in), target :: sqme_collector
    type(real_kinematics_t), intent(in), target :: real_kinematics
    select type (mapping => reg_data%fks_mapping)
    type is (fks_mapping_default_t)
       soft_mismatch%active = .false.
    type is (fks_mapping_resonances_t)
       soft_mismatch%reg_data => reg_data
       soft_mismatch%sqme_born => sqme_collector%sqme_subtraction_born_list
       soft_mismatch%sqme_born_cc => sqme_collector%sqme_born_cc
       call soft_mismatch%sub_soft%init (reg_data, n_in, nlegs_born, nlegs_born + 1)
       soft_mismatch%sub_soft%xi2_expanded = .false.
       soft_mismatch%real_kinematics => real_kinematics
    end select
  end subroutine soft_mismatch_init

  function soft_mismatch_evaluate (soft_mismatch, alpha_s) result (sqme_mismatch)
    real(default) :: sqme_mismatch
    class(soft_mismatch_t), intent(inout) :: soft_mismatch
    real(default), intent(in) :: alpha_s
    integer :: alr, i_uborn, emitter, i_res, i_phs, i_con
    real(default) :: xi, y, q2, s
    real(default) :: E_gluon
    type(vector4_t) :: p_em
    real(default) :: sqme_alr
    sqme_mismatch = zero
    if (.not. soft_mismatch%active) return
    associate (real_kinematics => soft_mismatch%real_kinematics)
       xi = real_kinematics%xi_mismatch
       y = real_kinematics%y_mismatch
       s = real_kinematics%cms_energy2
       E_gluon = sqrt (s) * xi / two

       if (debug_active (D_MISMATCH)) then
          print *, 'Evaluating soft mismatch: '
          print *, 'Phase space: '
          call vector4_write_set (real_kinematics%p_born_cms%get_momenta(1), &
             show_mass = .true.)
          print *, 'xi: ', xi, 'y: ', y, 's: ', s, 'E_gluon: ', E_gluon
       end if

       do alr = 1, soft_mismatch%reg_data%n_regions

          i_phs = real_kinematics%alr_to_i_phs (alr)
          i_con = soft_mismatch%reg_data%alr_to_i_contributor (alr)
          q2 = real_kinematics%xi_ref_momenta(i_con)**2
          emitter = soft_mismatch%reg_data%regions(alr)%emitter
          p_em = real_kinematics%p_born_cms%get_momentum (1, emitter)
          i_res = soft_mismatch%reg_data%regions(alr)%i_res
          i_uborn = soft_mismatch%reg_data%regions(alr)%uborn_index

          call print_debug_alr ()

          call soft_mismatch%sub_soft%create_softvec_mismatch &
             (E_gluon, y, real_kinematics%phi, p_em)
          if (debug_active (D_MISMATCH)) &
             print *, 'Created soft vector: ', soft_mismatch%sub_soft%p_soft%p

          select type (fks_mapping => soft_mismatch%reg_data%fks_mapping)
          type is (fks_mapping_resonances_t)
             call fks_mapping%set_resonance_momentum &
                (real_kinematics%xi_ref_momenta(i_con))
          end select

          call soft_mismatch%sub_soft%compute (real_kinematics%p_born_cms%get_momenta(1), &
             soft_mismatch%sqme_born_cc(:,:,i_uborn), y, q2, alpha_s, alr, emitter, i_res)

          sqme_alr = soft_mismatch%compute (alr, xi, y, p_em, &
             real_kinematics%xi_ref_momenta(i_con), soft_mismatch%sub_soft%p_soft, &
             soft_mismatch%sqme_born(i_uborn), soft_mismatch%sub_soft%value(alr), &
             alpha_s, s)

          call msg_debug (D_MISMATCH, 'sqme_alr: ', sqme_alr)
          sqme_mismatch = sqme_mismatch + sqme_alr

       end do
    end associate
  contains
    subroutine print_debug_alr ()
      if (debug_active (D_MISMATCH)) then
         print *, 'alr: ', alr
         print *, 'i_phs: ', i_phs, 'i_con: ', i_con, 'i_res: ', i_res
         print *, 'emitter: ', emitter, 'i_uborn: ', i_uborn
         print *, 'emitter momentum: ', p_em%p
         print *, 'resonance momentum: ', &
            soft_mismatch%real_kinematics%xi_ref_momenta(i_con)%p
         print *, 'q2: ', q2
      end if
    end subroutine print_debug_alr
  end function soft_mismatch_evaluate

  function soft_mismatch_compute (soft_mismatch, alr, xi, y, p_em, p_res, p_soft, &
     sqme_born, sqme_soft, alpha_s, s) result (sqme_mismatch)
    real(default) :: sqme_mismatch
    class(soft_mismatch_t), intent(in) :: soft_mismatch
    integer, intent(in) :: alr
    real(default), intent(in) :: xi, y
    type(vector4_t), intent(in) :: p_em, p_res, p_soft
    real(default), intent(in) :: sqme_born, sqme_soft
    real(default), intent(in) :: alpha_s, s
    real(default) :: q2, expo, sm1, sm2, jacobian

    q2 = p_res**2
    expo = - two * p_soft * p_res / q2
    !!! Divide by 1 - y to factor out the corresponding
    !!! factor in the soft matrix element
    sm1 = sqme_soft / (one - y) * ( exp(expo) - exp(- xi) )
    call msg_debug2 (D_MISMATCH, 'sqme_soft in mismatch ', sqme_soft)

    sm2 = zero
    if (soft_mismatch%reg_data%regions(alr)%has_collinear_divergence ()) then
       expo = - two * p_em * p_res / q2 * &
          p_soft%p(0) / p_em%p(0)
       sm2 = 32 * pi * alpha_s * cf / (s * xi**2) * sqme_born * &
          ( exp(expo) - exp(- xi) ) / (one - y)
    end if

    jacobian = soft_mismatch%real_kinematics%jac_mismatch * s * xi / (8 * twopi3)
    sqme_mismatch = (sm1 - sm2) * jacobian

  end function soft_mismatch_compute

  subroutine coll_subtraction_init (coll_sub, n_alr, n_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    integer, intent(in) :: n_alr, n_in
    coll_sub%n_in = n_in
    coll_sub%n_alr = n_alr
    allocate (coll_sub%value (n_alr))
    allocate (coll_sub%value_soft (n_alr))
  end subroutine coll_subtraction_init

  subroutine coll_subtraction_compute_fsr &
       (coll_sub, sregion, p_res, p_born, sqme_born, sqme_born_sc, &
        xi, alpha_s, alr, soft_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in) :: p_res
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born, sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    logical, intent(in), optional :: soft_in
    real(default) :: value
    real(default) :: q0, z, p0
    real(default) :: z_o_xi, onemz
    real(default) :: pggz, pqgz
    integer :: nlegs, emitter
    integer :: flv_em, flv_rad
    logical :: soft

    if (.not. vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true., n_in = coll_sub%n_in)
       call msg_fatal ("Collinear subtraction, FSR: Phase space point &
          &must be in CMS")
    end if

    if (present (soft_in)) then
      soft = soft_in
    else
      soft = .false.
    end if
    nlegs = size (sregion%flst_real%flst)
    emitter = sregion%emitter
    flv_rad = sregion%flst_real%flst(nlegs)
    flv_em = sregion%flst_real%flst(emitter)
    q0 = p_res**1
    p0 = p_res * p_born(emitter) / q0
    !!! Here, z corresponds to 1-z in the formulas of arXiv:1002.2581;
    !!! the integrand is symmetric under this variable change
    z_o_xi = q0 / (two * p0)
    z = xi * z_o_xi; onemz = one - z
    if (is_gluon(flv_em) .and. is_gluon(flv_rad)) then
       !!! Implementation of equation (\ref{coll1}). Note that an
       !!! additional factor $z$, so that in the last step, the whole
       !!! expression is divided by $z/\xi$.
       pggz = two * CA * (z**2 * onemz + z**2 / onemz + onemz)
       value = pggz * sqme_born - 4 * CA * z**2 * onemz * sqme_born_sc
       value = value / z_o_xi
    else if (is_quark(abs(flv_em)) .and. is_quark (abs(flv_rad))) then
       !!! Equation \ref{coll2}
       pqgz = TR * z * (one - two * z * onemz)
       value = pqgz * sqme_born + 4 * TR * z**2 * onemz * sqme_born_sc
       value = value / z_o_xi
    else if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
       value = sqme_born * CF * (one + onemz**2) / z_o_xi
    else
       value = zero
       call msg_fatal ('Impossible flavor structure in collinear counterterm!')
    end if
    value = value / (p0**2 * onemz * z_o_xi)
    value = value * 4 * pi * alpha_s

    if (soft) then
      coll_sub%value_soft (alr) = value
    else
      coll_sub%value (alr) = value
    end if
  end subroutine coll_subtraction_compute_fsr

  subroutine coll_subtraction_compute_soft_limit_fsr &
       (coll_sub, sregion, p_res, p_born, sqme_born, &
        sqme_born_sc, xi, alpha_s, alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in) :: p_res
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    call coll_sub%compute_fsr (sregion, p_res, p_born, sqme_born, &
       sqme_born_sc, xi, alpha_s, alr, .true.)
  end subroutine coll_subtraction_compute_soft_limit_fsr

  subroutine coll_subtraction_compute_isr &
    (coll_sub, sregion, p_born, sqme_born, sqme_born_sc, &
     xi, alpha_s, alr, isr_mode, soft_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr, isr_mode
    logical, intent(in), optional :: soft_in
    logical :: soft
    real(default) :: z, onemz
    real(default) :: p02
    integer :: flv_em, flv_rad
    integer :: nlegs
    real(default) :: res

    if (vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true.)
       call msg_fatal ("Collinear subtraction, ISR: Phase space point &
          &must be in lab frame")
    end if

    if (present (soft_in)) then
      soft = soft_in
    else
      soft = .false.
    end if

    nlegs = size (sregion%flst_real%flst)
    flv_rad = sregion%flst_real%flst(nlegs)
    flv_em = sregion%flst_real%flst(isr_mode)
    !!! No need to pay attention to n_in = 1, because this case always has a
    !!! massive initial-state particle and thus no collinear divergence.
    p02 = p_born(1)%p(0) * p_born(2)%p(0) / two
    z = one - xi; onemz = xi

    if (is_quark(flv_em) .and. is_gluon(flv_rad)) then
       res = CF * (one + z**2) * sqme_born
    else if (is_gluon(flv_em) .and. is_quark (flv_rad)) then
       res = TR * (z**2 + onemz**2) * onemz * sqme_born
    else
       res = zero
       call msg_bug ("coll_subtraction_compute_isr: result undefined")
    end if
    res = res * z / p02
    res = res * 4 * pi * alpha_s

    if (soft) then
       coll_sub%value_soft(alr) = res
    else
       coll_sub%value(alr) = res
    end if
  end subroutine coll_subtraction_compute_isr

  subroutine coll_subtraction_compute_soft_limit_isr &
     (coll_sub, sregion, p_born, sqme_born, sqme_born_sc, &
      xi, alpha_s, alr, isr_mode)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born, sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr, isr_mode
    call coll_sub%compute_isr (sregion, p_born, sqme_born, sqme_born_sc, &
       zero, alpha_s, alr, isr_mode, .true. )
  end subroutine coll_subtraction_compute_soft_limit_isr

  subroutine real_subtraction_init (rsub, reg_data, n_in, &
      nlegs_born, nlegs_real, sqme_collector, &
      nlo_cuts)
    class(real_subtraction_t), intent(inout), target :: rsub
    type(region_data_t), intent(in), target :: reg_data
    integer, intent(in) :: n_in, nlegs_born, nlegs_real
    type(sqme_collector_t), intent(in), target :: sqme_collector
    type(nlo_cuts_t), intent(in), target :: nlo_cuts
    integer :: alr, i_uborn
    call msg_debug (D_SUBTRACTION, "real_subtraction_init")
    call msg_debug (D_SUBTRACTION, "n_in", n_in)
    call msg_debug (D_SUBTRACTION, "nlegs_born", nlegs_born)
    call msg_debug (D_SUBTRACTION, "nlegs_real", nlegs_real)
    call msg_debug (D_SUBTRACTION, "reg_data%n_regions", reg_data%n_regions)

    if (debug2_active (D_SUBTRACTION))  &
         call reg_data%write ()
    rsub%reg_data => reg_data
    rsub%sqme_real_non_sub => sqme_collector%sqme_real_non_sub
    rsub%sqme_born => sqme_collector%sqme_subtraction_born_list
    rsub%sqme_born_cc => sqme_collector%sqme_born_cc
    rsub%sqme_born_sc => sqme_collector%sqme_born_sc
    allocate (rsub%sc_required (reg_data%n_regions))
    do alr = 1, reg_data%n_regions
       i_uborn = reg_data%regions(alr)%uborn_index
       rsub%sc_required(alr) = &
          reg_data%flv_born(i_uborn)%count_particle (GLUON) > 0
    end do

    rsub%passed_born_cuts => nlo_cuts%passed_born
    rsub%passed_real_cuts => nlo_cuts%passed_real

    call rsub%sub_soft%init (reg_data, n_in, nlegs_born, nlegs_real)
    call rsub%sub_coll%init (reg_data%n_regions, n_in)

    if (rsub%reg_data%n_in > 1 .and. any (rsub%reg_data%get_emitter_list () <= 2)) then
       call rsub%init_pdfs ()
       rsub%sub_soft%pdf_born_plus => rsub%pdf_born(I_PLUS)
       rsub%sub_soft%pdf_born_minus => rsub%pdf_born(I_MINUS)
       rsub%sub_coll%pdf_born_plus => rsub%pdf_born(I_PLUS)
       rsub%sub_coll%pdf_born_minus => rsub%pdf_born(I_MINUS)
       rsub%sub_coll%pdf_scaled_plus => rsub%pdf_scaled(I_PLUS)
       rsub%sub_coll%pdf_scaled_minus => rsub%pdf_scaled(I_MINUS)
    end if
  end subroutine real_subtraction_init

  subroutine real_subtraction_init_pdfs (rsub)
    class(real_subtraction_t), intent(inout) :: rsub
    type(string_t) :: lhapdf_dir, lhapdf_file
    integer :: lhapdf_member
    call msg_debug (D_SUBTRACTION, "real_subtraction_init_pdfs")
    lhapdf_dir = ""
    lhapdf_file = ""
    lhapdf_member = 0
    if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
          (1, lhapdf_dir, lhapdf_file, lhapdf_member, rsub%pdf_data%pdf)
       associate (pdf_data => rsub%pdf_data)
          pdf_data%type = STRF_LHAPDF6
          pdf_data%xmin = pdf_data%pdf%getxmin ()
          pdf_data%xmax = pdf_data%pdf%getxmax ()
          pdf_data%qmin = sqrt (pdf_data%pdf%getq2min ())
          pdf_data%qmax = sqrt (pdf_data%pdf%getq2max ())
       end associate
    else
       call msg_fatal ("Real subtraction: PDF method must be LHAPDF6")
    end if
  end subroutine real_subtraction_init_pdfs

  subroutine real_subtraction_set_resonance_mappings (rsub, use_mappings)
    class(real_subtraction_t), intent(inout) :: rsub
    logical, intent(in) :: use_mappings
    rsub%sub_soft%use_resonance_mappings = use_mappings
    rsub%sub_coll%use_resonance_mappings = use_mappings
  end subroutine real_subtraction_set_resonance_mappings

  subroutine real_subtraction_set_real_kinematics (rsub, real_kinematics)
    class(real_subtraction_t), intent(inout) :: rsub
    type(real_kinematics_t), intent(in), target :: real_kinematics
    rsub%real_kinematics => real_kinematics
  end subroutine real_subtraction_set_real_kinematics

  subroutine real_subtraction_set_isr_kinematics (rsub, fractions)
    class(real_subtraction_t), intent(inout) :: rsub
    type(isr_kinematics_t), intent(in), target :: fractions
    rsub%isr_kinematics => fractions
  end subroutine real_subtraction_set_isr_kinematics

  subroutine real_subtraction_set_alr (rsub, alr)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr
    rsub%current_alr = alr
  end subroutine real_subtraction_set_alr

  subroutine real_subtraction_set_i_res (rsub, alr)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr
    select type (fks_mapping => rsub%reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       rsub%current_i_res = fks_mapping%res_map%alr_to_i_res (alr)
    class default
       rsub%current_i_res = 0
    end select
  end subroutine real_subtraction_set_i_res

  function real_subtraction_compute (rsub, emitter, i_phs, i_flv, alpha_s) result (sqme)
    real(default) :: sqme
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs, i_flv
    real(default) :: sqme_alr, alpha_s
    integer :: alr, i_con
    logical :: same_emitter
    sqme = zero
    do alr = 1, rsub%reg_data%n_regions
       if (rsub%fixed_alr > 0 .and. rsub%fixed_alr /= alr) cycle
       sqme_alr = zero
       if (emitter > rsub%isr_kinematics%n_in) then
          same_emitter = emitter == rsub%reg_data%regions(alr)%emitter
       else
          same_emitter = rsub%reg_data%regions(alr)%emitter <= rsub%isr_kinematics%n_in
       end if
       if (same_emitter .and. i_phs == rsub%real_kinematics%alr_to_i_phs (alr) .and. &
          i_flv == rsub%reg_data%regions(alr)%real_index) then
          call rsub%set_alr (alr)
          call rsub%set_i_res (alr)
          sqme_alr = rsub%evaluate_emitter_region (rsub%reg_data%regions(alr)%emitter, &
             i_phs, alpha_s)
          i_con = rsub%get_i_contributor (alr)
          if (rsub%purpose == INTEGRATION .or. rsub%purpose == FIXED_ORDER_EVENTS) &
             sqme_alr = sqme_alr * rsub%get_phs_factor (i_con)
       end if
       sqme = sqme + sqme_alr
    end do
  end function real_subtraction_compute

  function real_subtraction_evaluate_emitter_region (rsub, emitter, i_phs, alpha_s) &
     result (sqme)
    real(default) :: sqme
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    integer :: i_con
    if (emitter <= rsub%isr_kinematics%n_in) then
       sqme = rsub%evaluate_region_isr (emitter, i_phs, alpha_s)
    else
       select type (fks_mapping => rsub%reg_data%fks_mapping)
       type is (fks_mapping_resonances_t)
          i_con = rsub%reg_data%alr_to_i_contributor (rsub%current_alr)
          call fks_mapping%set_resonance_momenta &
             (rsub%real_kinematics%xi_ref_momenta )
          sqme = rsub%evaluate_region_fsr (emitter, i_phs, alpha_s)
       class default
          sqme = rsub%evaluate_region_fsr (emitter, i_phs, alpha_s)
       end select
    end if
  end function real_subtraction_evaluate_emitter_region

  function real_subtraction_evaluate_region_fsr (rsub, emitter, i_phs, &
     alpha_s) result (sqme_tot)
    real(default) :: sqme_tot
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    integer :: i_real
    real(default) :: sqme_rad, sqme_soft, sqme_coll, sqme_cs, sqme_remn
    sqme_rad = zero; sqme_soft = zero; sqme_coll = zero
    sqme_cs = zero; sqme_remn = zero
    associate (region => rsub%reg_data%regions(rsub%current_alr))
      if (rsub%radiation_active .and. rsub%passed_real_cuts (i_phs)) then
         i_real = region%real_index
         sqme_rad = rsub%sqme_real_non_sub (i_real, i_phs)
         call evaluate_fks_factors (sqme_rad, rsub%reg_data, rsub%real_kinematics, rsub%current_alr, &
            i_phs, emitter, rsub%current_i_res)
         call apply_kinematic_factors_radiation (sqme_rad, rsub%purpose, rsub%real_kinematics, i_phs, .false.)
      end if
      if (rsub%subtraction_active .and. rsub%passed_born_cuts) then
         call rsub%evaluate_subtraction_terms_fsr (emitter, i_phs, alpha_s, &
            sqme_soft, sqme_coll, sqme_cs)
         call apply_kinematic_factors_subtraction_fsr (sqme_soft, sqme_coll, sqme_cs, &
            rsub%real_kinematics, i_phs)
         sqme_remn = compute_sqme_remnant_fsr (sqme_soft, sqme_cs, rsub%real_kinematics, i_phs)
         select case (rsub%purpose)
         case (INTEGRATION)
            sqme_tot = sqme_rad - sqme_soft - sqme_coll + sqme_cs + sqme_remn
         case (FIXED_ORDER_EVENTS)
            sqme_tot = - sqme_soft - sqme_coll + sqme_cs + sqme_remn
         case default
            sqme_tot = zero
            call msg_bug ("real_subtraction_evaluate_region_fsr: " // &
                 "Undefined rsub%purpose")
         end select
      else
         sqme_tot = sqme_rad
      end if
      sqme_tot = sqme_tot * rsub%real_kinematics%jac_rand(i_phs)
    end associate

    call debug_output ()

  contains

    subroutine debug_output ()
       logical :: print_all, soft, collinear
       type(vector4_t) :: p_gluon
       integer, save :: n_soft = 0, passed_soft = 0, n_coll = 0, passed_coll = 0
       logical :: write_histo = .true.
       real(default) :: y
       y = rsub%real_kinematics%y(i_phs)
       if (debug_active (D_SUBTRACTION)) then
          print_all = debug2_active (D_SUBTRACTION)
          if (print_all) then
             call msg_debug2 (D_SUBTRACTION, "real_subtraction_evaluate_region_fsr")
             call write_computation_status ()
          else
             associate (p_real => rsub%real_kinematics%p_real_cms)
                p_gluon = p_real%get_momentum (i_phs, p_real%get_n_momenta (i_phs))
                soft = p_gluon%p(0) < 0.1_default
             end associate
             collinear = abs (rsub%real_kinematics%y (i_phs)- one) < 0.01_default
             if (soft) then
                if (write_histo)  call write_point_to_file (p_gluon%p(0), sqme_rad, sqme_soft)
                n_soft = n_soft + 1
                if (sqme_soft < zero)  call msg_warning ("Soft < 0")
                if (abs (sqme_rad - sqme_soft) > sqme_rad .and. sqme_soft > tiny_10 &
                   .and. sqme_rad > tiny_10) then
                   call msg_warning ("Soft MEs do not match in this soft region")
                   call write_computation_status (passed_soft, n_soft, "soft")
                else
                   passed_soft = passed_soft + 1
                end if
             end if
             if (collinear) then
                n_coll = n_coll + 1
                if (abs (sqme_rad - sqme_coll) > sqme_rad .and. &
                   sqme_coll > tiny_10 .and. sqme_rad > tiny_10) then
                   call msg_warning ("Collinear MEs do not match in this collinear region")
                   call write_computation_status (passed_coll, n_coll, "collinear")
                else
                   passed_coll = passed_coll + 1
                end if
             end if
          end if
       end if
    end subroutine debug_output

    subroutine write_computation_status (passed, total, region_type)
       integer, intent(in), optional :: passed, total
       character(*), intent(in), optional :: region_type
       integer :: i_uborn
       integer :: u
       real(default) :: xi
       u = given_output_unit (); if (u < 0) return
       i_uborn = rsub%reg_data%regions(rsub%current_alr)%uborn_index
       xi = rsub%real_kinematics%xi_max (i_phs) * rsub%real_kinematics%xi_tilde
       write (u,'(A,I2)') 'rsub%purpose: ', rsub%purpose
       write (u,'(A,I3)') 'alr: ', rsub%current_alr
       write (u,'(A,I3)') 'emitter: ', emitter
       write (u,'(A,I3)') 'i_phs: ', i_phs 
       write (u,'(A,F6.4)') 'xi_max: ', rsub%real_kinematics%xi_max (i_phs)
       write (u,'(A,F6.4,2X,A,F6.4)') 'xi: ', xi, 'y: ', rsub%real_kinematics%y (i_phs)
       write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_uborn)
       write (u,'(A,ES16.9)')  'sqme_real: ', sqme_rad
       write (u,'(A,ES16.9)')  'sqme_soft: ', sqme_soft
       write (u,'(A,ES16.9)')  'sqme_coll: ', sqme_coll
       write (u,'(A,ES16.9)')  'sqme_coll-soft: ', sqme_cs
       write (u,'(A,ES16.9)')  'sqme_remn: ', sqme_remn
       write (u,'(A,ES16.9)')  'sqme_tot: ', sqme_tot
       if (present (passed) .and. present (total) .and. &
            present (region_type)) &
          write (u,'(A)') char (str (passed) // " of " // str (total) // &
               " " // region_type // " points passed in total")
    end subroutine write_computation_status

    subroutine write_point_to_file (E_gluon, sqme_rad, sqme_soft)
      real(default), intent(in) :: E_gluon, sqme_rad, sqme_soft
      integer, save :: funit = 0
      type(string_t) :: filename
      filename = var_str ("soft.log")
      if (funit == 0) then
         funit = free_unit ()
         open (funit, file=char(filename), action = "write", status="replace")
         write (funit, "(A,5X,A,5X,A)") "# E_gluon", "Real", "Soft Approx"
      end if
      write (funit,'(3(ES16.9,1X))') E_gluon, sqme_rad, sqme_soft
    end subroutine write_point_to_file

  end function real_subtraction_evaluate_region_fsr

  function real_subtraction_evaluate_region_isr (rsub, emitter, i_phs, alpha_s) &
     result (sqme_tot)
    real(default) :: sqme_tot
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    integer :: i_real
    real(default) :: sqme_rad, sqme_soft, sqme_coll_plus, sqme_coll_minus
    real(default) :: sqme_cs_plus, sqme_cs_minus
    real(default) :: sqme_remn
    real(default) :: onemy, onepy
    logical :: proc_scatter

    proc_scatter = rsub%isr_kinematics%n_in == 2

    sqme_rad = zero; sqme_soft = zero;
    sqme_coll_plus = zero; sqme_coll_minus = zero
    sqme_cs_plus = zero; sqme_cs_minus = zero
    sqme_remn = zero

    if (proc_scatter) call rsub%compute_pdfs ()
    associate (region => rsub%reg_data%regions(rsub%current_alr))
      i_real = region%real_index
      if (rsub%radiation_active .and. rsub%passed_real_cuts (i_phs)) then 
         sqme_rad = rsub%sqme_real_non_sub (i_real, i_phs)
         call evaluate_fks_factors (sqme_rad, rsub%reg_data, rsub%real_kinematics, &
            rsub%current_alr, i_phs, emitter, rsub%current_i_res)
         if (proc_scatter) then
            call rsub%scale_pdfs_real (sqme_rad, i_real, I_PLUS)
            call rsub%scale_pdfs_real (sqme_rad, i_real, I_MINUS)
         end if

         call apply_kinematic_factors_radiation (sqme_rad, rsub%purpose, rsub%real_kinematics, &
            i_phs, .true.)
      end if
      if (rsub%subtraction_active .and. rsub%passed_born_cuts) then
         call rsub%evaluate_subtraction_terms_isr (emitter, i_phs, alpha_s, &
            sqme_soft, sqme_coll_plus, sqme_coll_minus, sqme_cs_plus, sqme_cs_minus)
         if (proc_scatter) then
            call rsub%scale_pdfs_collinear (sqme_coll_plus, i_real, &
               region%uborn_index, I_PLUS)
            call rsub%scale_pdfs_collinear (sqme_coll_minus, i_real, &
               region%uborn_index, I_MINUS)
         end if
         call apply_kinematic_factors_subtraction_isr (sqme_soft, sqme_coll_plus, &
            sqme_coll_minus, sqme_cs_plus, sqme_cs_minus, rsub%real_kinematics, i_phs)
         sqme_remn = compute_sqme_remnant_isr (proc_scatter, sqme_soft, sqme_cs_plus, &
            sqme_cs_minus, rsub%isr_kinematics, rsub%real_kinematics, i_phs)

         sqme_tot = sqme_rad - sqme_soft - sqme_coll_plus - sqme_coll_minus &
              + sqme_cs_plus + sqme_cs_minus + sqme_remn
      else
         sqme_tot = sqme_rad
      end if
    end associate

    sqme_tot = sqme_tot * rsub%real_kinematics%jac_rand (i_phs)

    call debug_output ()

  contains

    subroutine debug_output ()
       logical :: soft, collinear
       type(vector4_t) :: p_gluon
       if (debug_active (D_SUBTRACTION)) then
          call msg_debug2 (D_SUBTRACTION, "real_subtraction_evaluate_region_isr")
          if (.not. rsub%passed_born_cuts) &
             call msg_print_color ("PHS Point did not pass Born cuts", COL_RED)
          if (.not. any (rsub%passed_real_cuts)) &
             call msg_print_color ("PHS Point did not pass Real cuts", COL_RED)
          if (.not. rsub%passed_born_cuts .or. (.not. any (rsub%passed_real_cuts))) return
          if (debug2_active (D_SUBTRACTION)) then
             call write_computation_status ()
          else
             associate (p_real => rsub%real_kinematics%p_real_cms)
                p_gluon = p_real%get_momentum (i_phs, p_real%get_n_momenta (i_phs))
                soft = p_gluon%p(0) < 2.0_default
             end associate
             collinear = abs (rsub%real_kinematics%y (i_phs) - one) < 0.01_default
             if (soft) then
                if (abs (sqme_rad - sqme_soft) > sqme_rad .and. sqme_soft > tiny_10) then
                   call msg_warning ("Soft MEs do not match in soft region")
                   call write_computation_status ()
                end if
             end if
             ! TODO: (bcn 2016-01-13) check coll_plus and coll_minus
             !if (collinear) then
                !if (abs (sqme_rad - sqme_coll) > sqme_rad .and. sqme_coll > tiny_10) then
                   !call msg_warning ("Collinear MEs do not match in collinear region")
                   !call write_computation_status ()
                !end if
             !end if
          end if
       end if
    end subroutine debug_output

    subroutine write_computation_status (unit)
       integer, intent(in), optional :: unit
       integer :: i_uborn
       integer :: u
       real(default) :: xi
       u = given_output_unit (unit); if (u < 0) return
       i_uborn = rsub%reg_data%regions(rsub%current_alr)%uborn_index
       xi = rsub%real_kinematics%xi_max (i_phs) * rsub%real_kinematics%xi_tilde
       write (u,'(A,I2)') 'alr: ', rsub%current_alr
       write (u,'(A,I2)') 'emitter: ', emitter
       write (u,'(A,F4.2)') 'xi_max: ', rsub%real_kinematics%xi_max (i_phs)
       print *, 'xi: ', xi, 'y: ', rsub%real_kinematics%y (i_phs)
       print *, 'xb1: ', rsub%isr_kinematics%x(1), 'xb2: ', rsub%isr_kinematics%x(2)
       print *, 'random jacobian: ', rsub%real_kinematics%jac_rand (i_phs)
       write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_uborn)
       write (u,'(A,ES16.9)')  'sqme_real: ', sqme_rad
       write (u,'(A,ES16.9)')  'sqme_soft: ', sqme_soft
       write (u,'(A,ES16.9)')  'sqme_coll_plus: ', sqme_coll_plus
       write (u,'(A,ES16.9)')  'sqme_coll_minus: ', sqme_coll_minus
       write (u,'(A,ES16.9)')  'sqme_cs_plus: ', sqme_cs_plus
       write (u,'(A,ES16.9)')  'sqme_cs_minus: ', sqme_cs_minus
       write (u,'(A,ES16.9)')  'sqme_remn: ', sqme_remn
       write (u,'(A,ES16.9)')  'sqme_tot: ', sqme_tot
    end subroutine write_computation_status

  end function real_subtraction_evaluate_region_isr

  subroutine real_subtraction_evaluate_subtraction_terms_fsr (rsub, &
                  emitter, i_phs, alpha_s, &
                  sqme_soft, sqme_coll, sqme_cs)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft, sqme_coll, sqme_cs
    integer :: alr
    alr = rsub%current_alr
    call rsub%compute_sub_soft (emitter, i_phs, alpha_s)
    call rsub%compute_sub_coll (emitter, i_phs, alpha_s)
    call rsub%compute_sub_coll_soft (emitter, alpha_s)
    sqme_soft = rsub%sub_soft%value(alr)
    sqme_coll = rsub%sub_coll%value(alr)
    sqme_cs = rsub%sub_coll%value_soft(alr)
  end subroutine real_subtraction_evaluate_subtraction_terms_fsr

  subroutine evaluate_fks_factors &
     (sqme, reg_data, real_kinematics, alr, i_phs, emitter, i_res)
    real(default), intent(inout) :: sqme
    type(region_data_t), intent(inout) :: reg_data
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: alr, i_phs, emitter, i_res
    real(default) :: s_alpha
    s_alpha = reg_data%get_svalue (real_kinematics%p_real_cms%get_momenta(i_phs), &
        alr, emitter, i_res)
    if (debug2_active (D_SUBTRACTION)) call msg_print_color('s_alpha', s_alpha, COL_YELLOW)
    sqme = sqme * s_alpha
    associate (region => reg_data%regions(alr))
       sqme = sqme * region%mult
       if (emitter > reg_data%n_in) &
          sqme = sqme * region%double_fsr_factor (real_kinematics%p_real_cms%get_momenta(i_phs))
    end associate
  end subroutine evaluate_fks_factors

  subroutine apply_kinematic_factors_radiation (sqme, purpose, real_kinematics, i_phs, isr)
    real(default), intent(inout) :: sqme
    integer, intent(in) :: purpose
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    logical, intent(in) :: isr
    real(default) :: xi, xi_tilde, s
    xi_tilde = real_kinematics%xi_tilde
    xi = xi_tilde * real_kinematics%xi_max (i_phs)
    select case (purpose)
    case (INTEGRATION, FIXED_ORDER_EVENTS)
       sqme = sqme * xi**2 / xi_tilde * real_kinematics%jac(i_phs)%jac(1)
    case (POWHEG)
       if (.not. isr) then
          s = real_kinematics%cms_energy2
          sqme = sqme * real_kinematics%jac(i_phs)%jac(1) * s / (8 * twopi3) * xi
       else
          call msg_fatal ("POWHEG with initial-state radiation not implemented yet")
       end if
    end select
  end subroutine apply_kinematic_factors_radiation

  subroutine apply_kinematic_factors_subtraction_fsr &
     (sqme_soft, sqme_coll, sqme_cs, real_kinematics, i_phs)
    real(default), intent(inout) :: sqme_soft, sqme_coll, sqme_cs
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    real(default) :: xi_tilde, onemy
    xi_tilde = real_kinematics%xi_tilde
    onemy = one - real_kinematics%y(i_phs)  
    sqme_soft = sqme_soft / onemy / xi_tilde
    sqme_coll = sqme_coll / onemy / xi_tilde
    sqme_cs = sqme_cs / onemy / xi_tilde
    associate (jac => real_kinematics%jac(i_phs)%jac)
       sqme_soft = sqme_soft * jac(2)
       sqme_coll = sqme_coll * jac(3)
       sqme_cs = sqme_cs * jac(2)
    end associate
  end subroutine apply_kinematic_factors_subtraction_fsr

  function compute_sqme_remnant_fsr (sqme_soft, sqme_cs, &
     real_kinematics, i_phs) result (sqme_remn)
    real(default) :: sqme_remn
    real(default), intent(in) :: sqme_soft, sqme_cs
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    sqme_remn = (sqme_soft - sqme_cs) * &
       log (real_kinematics%xi_max (i_phs)) * real_kinematics%xi_tilde
  end function compute_sqme_remnant_fsr

  subroutine apply_kinematic_factors_subtraction_isr &
     (sqme_soft, sqme_coll_plus, sqme_coll_minus, sqme_cs_plus, &
      sqme_cs_minus, real_kinematics, i_phs)
    real(default), intent(inout) :: sqme_soft, sqme_coll_plus, sqme_coll_minus
    real(default), intent(inout) :: sqme_cs_plus, sqme_cs_minus
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    real(default) :: xi_tilde, y, onemy, onepy
    xi_tilde = real_kinematics%xi_tilde
    y = real_kinematics%y (i_phs)
    onemy = one - y; onepy = one + y
    associate (jac => real_kinematics%jac(i_phs)%jac)
       sqme_soft = sqme_soft / (one - y**2) / xi_tilde * jac(2)
       sqme_coll_plus = sqme_coll_plus / onemy / xi_tilde / two * jac(3)
       sqme_coll_minus = sqme_coll_minus / onepy / xi_tilde / two * jac(4)
       sqme_cs_plus = sqme_cs_plus / onemy / xi_tilde / two * jac(2)
       sqme_cs_minus = sqme_cs_minus / onepy / xi_tilde / two * jac(2)
    end associate
  end subroutine apply_kinematic_factors_subtraction_isr

  function compute_sqme_remnant_isr (proc_scatter, sqme_soft, sqme_cs_plus, sqme_cs_minus, &
     isr_kinematics, real_kinematics, i_phs) result (sqme_remn)
    real(default) :: sqme_remn
    logical, intent(in) :: proc_scatter
    real(default), intent(in) :: sqme_soft, sqme_cs_plus, sqme_cs_minus
    type(isr_kinematics_t), intent(in) :: isr_kinematics
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    real(default) :: xi_tilde, xi_max, xi_max_plus, xi_max_minus
    xi_max = real_kinematics%xi_max (i_phs)
    if (proc_scatter) then
       xi_max_plus = one - isr_kinematics%x(I_PLUS)
       xi_max_minus = one - isr_kinematics%x(I_MINUS)
    else
       xi_max_plus = real_kinematics%xi_max (i_phs)
       xi_max_minus = real_kinematics%xi_max (i_phs)
    end if
    xi_tilde = real_kinematics%xi_tilde
    sqme_remn = log(xi_max) * xi_tilde * sqme_soft
    sqme_remn = sqme_remn - log (xi_max_plus) * xi_tilde * sqme_cs_plus &
                          - log (xi_max_minus) * xi_tilde * sqme_cs_minus
  end function compute_sqme_remnant_isr

  subroutine real_subtraction_evaluate_subtraction_terms_isr (rsub, &
    emitter, i_phs, alpha_s, sqme_soft, sqme_coll_plus, sqme_coll_minus, &
    sqme_cs_plus, sqme_cs_minus)

    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft
    real(default), intent(out) :: sqme_coll_plus, sqme_coll_minus
    real(default), intent(out) :: sqme_cs_plus, sqme_cs_minus
    integer :: alr
    alr = rsub%current_alr
    call rsub%compute_sub_soft (emitter, i_phs, alpha_s)
    sqme_soft = rsub%sub_soft%value(alr)
    if (emitter /= 2) then
       call rsub%compute_sub_coll (1, i_phs, alpha_s)
       call rsub%compute_sub_coll_soft (1, alpha_s)
       sqme_coll_plus = rsub%sub_coll%value(alr)
       sqme_cs_plus = rsub%sub_coll%value_soft(alr)
    else
       sqme_coll_plus = zero
       sqme_cs_plus = zero
    end if
    if (emitter /= 1) then
       call rsub%compute_sub_coll (2, i_phs, alpha_s)
       call rsub%compute_sub_coll_soft (2, alpha_s)
       sqme_coll_minus = rsub%sub_coll%value(alr)
       sqme_cs_minus = rsub%sub_coll%value_soft(alr)
    else
       sqme_coll_minus = zero
       sqme_cs_minus = zero
    end if
  end subroutine real_subtraction_evaluate_subtraction_terms_isr

  function real_subtraction_get_phs_factor (rsub, i_con) result (factor)
    real(default) :: factor
    class(real_subtraction_t), intent(in) :: rsub
    integer, intent(in) :: i_con
    real(default) :: s
    s = rsub%real_kinematics%xi_ref_momenta (i_con)**2
    factor = s / (8 * twopi3)
  end function real_subtraction_get_phs_factor

  function real_subtraction_get_i_contributor (rsub, alr) result (i_con)
    integer :: i_con
    class(real_subtraction_t), intent(in) :: rsub
    integer, intent(in) :: alr
    if (allocated (rsub%reg_data%alr_to_i_contributor)) then
       i_con = rsub%reg_data%alr_to_i_contributor (alr)
    else
       i_con = 1
    end if
  end function real_subtraction_get_i_contributor

  subroutine real_subtraction_compute_sub_soft &
     (rsub, emitter, i_phs, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    real(default), intent(in) :: alpha_s
    integer :: alr, i_con, i_uborn
    real(default) :: q2

    alr = rsub%current_alr
       associate (real_kinematics => rsub%real_kinematics)
       if (rsub%reg_data%regions(alr)%has_soft_divergence ()) then
          if (rsub%sub_soft%use_resonance_mappings) then
             i_con = rsub%reg_data%alr_to_i_contributor (alr)
          else
             i_con = 1
          end if
          q2 = real_kinematics%xi_ref_momenta (i_con)**2
          if (emitter > rsub%sub_soft%n_in) then
             call rsub%sub_soft%create_softvec_fsr &
                (real_kinematics%p_born_cms%get_momenta(1), &
                real_kinematics%y_soft (i_phs), &
                real_kinematics%phi, emitter, &
                real_kinematics%xi_ref_momenta(i_con))
          else
             call rsub%sub_soft%create_softvec_isr &
                (real_kinematics%y_soft(i_phs), real_kinematics%phi)
          end if
          i_uborn = rsub%reg_data%regions(alr)%uborn_index
          call rsub%sub_soft%compute (real_kinematics%p_born_cms%get_momenta(1), &
             rsub%sqme_born_cc(:,:,i_uborn), real_kinematics%y(i_phs), &
             q2, alpha_s, alr, emitter, rsub%current_i_res)
       else
          rsub%sub_soft%value(alr) = zero
       end if
    end associate
  end subroutine real_subtraction_compute_sub_soft

  function real_subtraction_get_sc_matrix_element (rsub, alr, em, uborn_index) result (sqme_sc)
    class(real_subtraction_t), intent(in) :: rsub
    integer, intent(in) :: alr, em, uborn_index
    real(default) :: sqme_sc
    complex(default) :: prod1, prod2

    if (rsub%sc_required(alr)) then
       associate (p => rsub%real_kinematics%p_real_cms%phs_point(1)%p)
          call spinor_product (p(em), p(rsub%reg_data%n_legs_real), prod1, prod2)
       end associate
       sqme_sc = real (prod1 / prod2 * rsub%sqme_born_sc(uborn_index))
    else
       sqme_sc = zero
    end if
  end function real_subtraction_get_sc_matrix_element

  subroutine real_subtraction_compute_sub_coll (rsub, em, i_phs, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: em, i_phs
    real(default), intent(in) :: alpha_s
    real(default) :: xi, xi_max, xi_max_pm
    real(default) :: sqme_sc
    integer :: alr, i_con
    alr = rsub%current_alr
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          xi = rsub%real_kinematics%xi_tilde * rsub%real_kinematics%xi_max (i_phs)
          if (rsub%sub_coll%use_resonance_mappings) then
             i_con = rsub%reg_data%alr_to_i_contributor (alr)
          else
             i_con = 1
          end if
          sqme_sc = rsub%get_sc_matrix_element (alr, em, sregion%uborn_index)
          if (em <= rsub%sub_coll%n_in) then
             xi_max_pm = one - rsub%isr_kinematics%x(em)
             xi = rsub%real_kinematics%xi_tilde * xi_max_pm
             call rsub%sub_coll%compute_isr (sregion, &
                  rsub%real_kinematics%p_born_lab%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr, em)
          else
             call rsub%sub_coll%compute_fsr (sregion, &
                  rsub%real_kinematics%xi_ref_momenta (i_con), &
                  rsub%real_kinematics%p_born_cms%get_momenta(1), &
                  rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr)
          end if
       else
          rsub%sub_coll%value(alr) = zero
       end if
    end associate
  end subroutine real_subtraction_compute_sub_coll

  subroutine real_subtraction_compute_sub_coll_soft (rsub, em, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: em
    real(default), intent(in) :: alpha_s
    real(default) :: sqme_sc
    real(default) :: xi
    integer :: alr, i_con
    alr = rsub%current_alr
    xi = zero
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          if (rsub%sub_coll%use_resonance_mappings) then
             i_con = rsub%reg_data%alr_to_i_contributor (alr)
          else
             i_con = 1
          end if
          sqme_sc = rsub%get_sc_matrix_element (alr, em, sregion%uborn_index)
          if (em <= rsub%sub_coll%n_in) then
             call rsub%sub_coll%compute_soft_limit_isr (sregion, &
                  rsub%real_kinematics%p_born_lab%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr, em)
          else
             call rsub%sub_coll%compute_soft_limit_fsr &
                  (sregion, rsub%real_kinematics%xi_ref_momenta(i_con), &
                  rsub%real_kinematics%p_born_cms%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr)
          end if
       else
          rsub%sub_coll%value_soft(alr) = zero
       end if
    end associate
  end subroutine real_subtraction_compute_sub_coll_soft

  subroutine real_subtraction_compute_pdfs (rsub)
    class(real_subtraction_t), intent(inout) :: rsub
    integer :: i
    real(default) :: z, z_coll, x, Q
    real(default) :: x_scaled, x_scaled_coll
    real(double), dimension(-6:6) :: f_dble = 0._double
    Q = rsub%isr_kinematics%fac_scale
    do i = 1, 2
       x = rsub%isr_kinematics%x(i)
       z = rsub%isr_kinematics%z(i)
       z_coll = rsub%isr_kinematics%z_coll(i)
       x_scaled = x * z
       x_scaled_coll = x * z_coll
       call rsub%pdf_data%evolve (dble(x), dble(Q), f_dble)
       rsub%pdf_born(i)%f = f_dble / dble(x)
       call rsub%pdf_data%evolve (dble(x_scaled), dble(Q), f_dble)
       rsub%pdf_scaled(i)%f = f_dble / dble(x_scaled)
       call rsub%pdf_data%evolve (dble(x_scaled_coll), dble(Q), f_dble)
       rsub%pdf_scaled_coll(i)%f = f_dble / dble(x_scaled_coll) 
    end do
  end subroutine real_subtraction_compute_pdfs

  subroutine real_subtraction_scale_pdfs_real (rsub, sqme, i_real, i_part)
    class(real_subtraction_t), intent(inout) :: rsub
    real(default), intent(inout) :: sqme
    integer, intent(in) :: i_real, i_part
    integer :: flv
    real(default) :: pdfs, pdfb
    flv = rsub%reg_data%flv_real(i_real)%flst(i_part)
    !!! Gluon has index 0 in the pdf array
    if (flv == GLUON) flv = 0
    pdfb = rsub%pdf_born(i_part)%f(flv)
    pdfs = rsub%pdf_scaled(i_part)%f(flv)
    sqme = sqme * pdfs / pdfb
  end subroutine real_subtraction_scale_pdfs_real

  subroutine real_subtraction_scale_pdfs_collinear &
     (rsub, sqme, i_real, i_born, i_part)
    class(real_subtraction_t), intent(inout) :: rsub
    real(default), intent(inout) :: sqme
    integer, intent(in) :: i_real, i_born, i_part
    integer :: flv_born, flv_real
    real(default) :: pdfs, pdfb
    flv_born = rsub%reg_data%flv_born(i_born)%flst(i_part)
    flv_real = rsub%reg_data%flv_real(i_real)%flst(i_part)
    if (flv_born == GLUON) flv_born = 0
    if (flv_real == GLUON) flv_real = 0
    pdfb = rsub%pdf_born(i_part)%f(flv_born)
    pdfs = rsub%pdf_scaled_coll(i_part)%f(flv_real)
    sqme = sqme * pdfs / pdfb
  end subroutine real_subtraction_scale_pdfs_collinear


end module real_subtraction
