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

module real_subtraction

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use format_defs, only: FMT_15
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
  use phs_fks, only: real_kinematics_t, isr_kinematics_t
  use phs_fks, only: I_PLUS, I_MINUS
  use phs_fks, only: phs_point_set_t
  use ttv_formfactors, only: m1s_to_mpole

  use fks_regions
  use nlo_data

  implicit none
  private

  public :: this_purpose
  public :: soft_mismatch_t
  public :: real_subtraction_t
  public :: real_partition_t
  public :: powheg_damping_simple_t
  public :: real_partition_fixed_order_t

  integer, parameter, public :: INTEGRATION = 0
  integer, parameter, public :: FIXED_ORDER_EVENTS = 1
  integer, parameter, public :: POWHEG = 2


  type :: soft_subtraction_t
    type(region_data_t), pointer :: reg_data => null ()
    real(default), dimension(:,:), allocatable :: momentum_matrix
    logical :: use_resonance_mappings = .false.
    type(vector4_t) :: p_soft = vector4_null
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
    logical :: xi2_expanded = .true.
    integer :: factorization_mode = NO_FACTORIZATION
  contains
    procedure :: init => soft_subtraction_init
    procedure :: requires_boost => soft_subtraction_requires_boost
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
    procedure :: i_xi_ref => soft_subtraction_i_xi_ref
    procedure :: final => soft_subtraction_final
  end type soft_subtraction_t

  type :: soft_mismatch_t
    type(region_data_t), pointer :: reg_data => null ()
    real(default), dimension(:), allocatable :: sqme_born
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    type(real_kinematics_t), pointer :: real_kinematics => null ()
    type(soft_subtraction_t) :: sub_soft
  contains
    procedure :: init => soft_mismatch_init
    procedure :: evaluate => soft_mismatch_evaluate
    procedure :: compute => soft_mismatch_compute
    procedure :: final => soft_mismatch_final
  end type soft_mismatch_t

  type :: coll_subtraction_t
    integer :: n_in, n_alr
    logical :: use_resonance_mappings = .false.
  contains
    procedure :: init => coll_subtraction_init
    procedure :: compute_fsr => coll_subtraction_compute_fsr
    procedure :: compute_isr => coll_subtraction_compute_isr
    procedure :: final => coll_subtraction_final
  end type coll_subtraction_t

  type :: real_subtraction_t
    type(region_data_t), pointer :: reg_data => null ()
    type(pdf_data_t) :: pdf_data
    type(real_kinematics_t), pointer :: real_kinematics => null ()
    type(isr_kinematics_t), pointer :: isr_kinematics => null ()
    type(real_scales_t) :: scales
    real(default), dimension(:,:), allocatable :: sqme_real_non_sub
    real(default), dimension(:), allocatable :: sqme_born
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    complex(default), dimension(:,:,:,:), allocatable :: sqme_born_sc
    type(soft_subtraction_t) :: sub_soft
    type(coll_subtraction_t) :: sub_coll
    logical, dimension(:), allocatable :: sc_required
    logical :: subtraction_deactivated = .false.
    integer :: purpose = INTEGRATION
    logical :: radiation_event = .true.
    logical :: subtraction_event = .false.
    type(pdf_container_t), dimension(2) :: pdf_born, pdf_scaled, pdf_scaled_coll
    integer, dimension(:), allocatable :: selected_alr
  contains
    procedure :: init => real_subtraction_init
    procedure :: init_pdfs => real_subtraction_init_pdfs
    procedure :: set_resonance_mappings => real_subtraction_set_resonance_mappings
    procedure :: set_real_kinematics => real_subtraction_set_real_kinematics
    procedure :: set_isr_kinematics => real_subtraction_set_isr_kinematics
    procedure :: get_i_res => real_subtraction_get_i_res
    procedure :: compute => real_subtraction_compute
    procedure :: evaluate_emitter_region => real_subtraction_evaluate_emitter_region
    procedure :: evaluate_emitter_region_debug &
         => real_subtraction_evaluate_emitter_region_debug
    procedure :: evaluate_region_fsr => real_subtraction_evaluate_region_fsr
    procedure :: evaluate_region_isr => real_subtraction_evaluate_region_isr
    procedure :: evaluate_subtraction_terms_fsr => &
         real_subtraction_evaluate_subtraction_terms_fsr
    procedure :: evaluate_subtraction_terms_isr => &
        real_subtraction_evaluate_subtraction_terms_isr
    procedure :: get_phs_factor => real_subtraction_get_phs_factor
    procedure :: get_i_contributor => real_subtraction_get_i_contributor
    procedure :: compute_sub_soft => real_subtraction_compute_sub_soft
    procedure :: get_spin_correlation_term => real_subtraction_get_spin_correlation_term
    procedure :: compute_sub_coll => real_subtraction_compute_sub_coll
    procedure :: compute_sub_coll_soft => real_subtraction_compute_sub_coll_soft
    procedure :: compute_pdfs => real_subtraction_compute_pdfs
    procedure :: scale_pdfs_real => real_subtraction_scale_pdfs_real
    procedure :: scale_pdfs_collinear => real_subtraction_scale_pdfs_collinear
    procedure :: requires_spin_correlations => &
         real_subtraction_requires_spin_correlations
    procedure :: final => real_subtraction_final
  end type real_subtraction_t

  type, abstract :: real_partition_t
  contains
    procedure (real_partition_init), deferred :: init
    procedure (real_partition_write), deferred :: write
    procedure (real_partition_get_f), deferred :: get_f
  end type real_partition_t

  type, extends (real_partition_t) :: powheg_damping_simple_t
     real(default) :: h2 = 5._default
     integer :: emitter
  contains
    procedure :: get_f => powheg_damping_simple_get_f
    procedure :: init => powheg_damping_simple_init
    procedure :: write => powheg_damping_simple_write
  end type powheg_damping_simple_t

  type, extends (real_partition_t) :: real_partition_fixed_order_t
     real(default) :: scale
     type(ftuple_t), dimension(:), allocatable :: fks_pairs
  contains
    procedure :: init => real_partition_fixed_order_init
    procedure :: write => real_partition_fixed_order_write
    procedure :: get_f => real_partition_fixed_order_get_f
  end type real_partition_fixed_order_t


  abstract interface
     subroutine real_partition_init (partition, scale, reg_data)
       import
       class(real_partition_t), intent(out) :: partition
       real(default), intent(in) :: scale
       type(region_data_t), intent(in) :: reg_data
     end subroutine real_partition_init
  end interface

  abstract interface
     subroutine real_partition_write (partition, unit)
       import
       class(real_partition_t), intent(in) :: partition
       integer, intent(in), optional :: unit
     end subroutine real_partition_write
  end interface

  abstract interface
    function real_partition_get_f (partition, p) result (f)
       import
       real(default) :: f
       class(real_partition_t), intent(in) :: partition
       type(vector4_t), intent(in), dimension(:) :: p
    end function real_partition_get_f
  end interface


contains

  function this_purpose (purpose)
    type(string_t) :: this_purpose
    integer, intent(in) :: purpose
    select case (purpose)
    case (INTEGRATION)
       this_purpose = var_str ("Integration")
    case (FIXED_ORDER_EVENTS)
       this_purpose = var_str ("Fixed order NLO events")
    case (POWHEG)
       this_purpose = var_str ("Powheg events")
    case default
       this_purpose = var_str ("Undefined!")
    end select
  end function this_purpose

  subroutine soft_subtraction_init (sub_soft, reg_data)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(region_data_t), intent(in), target :: reg_data
    sub_soft%reg_data => reg_data
    allocate (sub_soft%momentum_matrix (reg_data%n_legs_born, &
         reg_data%n_legs_born))
  end subroutine soft_subtraction_init

  function soft_subtraction_requires_boost (sub_soft, sqrts) result (requires_boost)
    logical :: requires_boost
    class(soft_subtraction_t), intent(in) :: sub_soft
    real(default), intent(in) :: sqrts
    real(default) :: mtop
    logical :: above_threshold
    if (sub_soft%factorization_mode == FACTORIZATION_THRESHOLD) then
       mtop = m1s_to_mpole (sqrts)
       above_threshold = sqrts**2 - four * mtop**2 > zero
    else
       above_threshold = .false.
    end if
    requires_boost = sub_soft%use_resonance_mappings .or. above_threshold
  end function soft_subtraction_requires_boost

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
    logical :: requires_boost
    associate (p_soft => sub_soft%p_soft)
       p_soft%p(0) = one
       requires_boost = sub_soft%requires_boost (two * p_born(1)%p(0))
       if (requires_boost) then
          boost_to_rest_frame = inverse (boost (xi_ref_momentum, xi_ref_momentum**1))
          p_em = boost_to_rest_frame * p_born(emitter)
       else
          p_em = p_born(emitter)
       end if
       p_soft%p(1:3) = p_em%p(1:3) / space_part_norm (p_em)
       dir = create_orthogonal (space_part (p_em))
       rot = rotation (y, sqrt(one - y**2), dir)
       p_soft = rot * p_soft
       if (.not. vanishes (phi)) then
         dir = space_part (p_em) / space_part_norm (p_em)
         rot = rotation (cos(phi), sin(phi), dir)
         p_soft = rot * p_soft
       end if
       if (requires_boost) p_soft = inverse (boost_to_rest_frame) * p_soft
    end associate
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

  function soft_subtraction_compute (sub_soft, p_born, &
     born_ij, y, q2, alpha_s, alr, emitter, i_res) result (sqme)
    real(default) :: sqme
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

    s_alpha_soft = sub_soft%reg_data%get_svalue_soft (p_born, &
         sub_soft%p_soft, alr, emitter, i_res)
    if (s_alpha_soft > one + tiny_07) call msg_fatal ("s_alpha_soft > 1!")
    if (debug_active (D_SUBTRACTION)) &
         call msg_print_color ('s_alpha_soft', s_alpha_soft, COL_YELLOW)
    select case (sub_soft%factorization_mode)
    case (NO_FACTORIZATION)
       kb = sub_soft%evaluate_factorization_default (p_born, born_ij)
    case (FACTORIZATION_THRESHOLD)
       kb = sub_soft%evaluate_factorization_threshold (thr_leg(emitter), p_born, born_ij)
    end select
    call msg_debug2 (D_SUBTRACTION, 'KB', kb)
    sqme = four * pi * alpha_s * s_alpha_soft * kb
    if (sub_soft%xi2_expanded) then
       xi2_factor = four / q2
    else
       xi2_factor = one
    end if
    if (emitter <= sub_soft%reg_data%n_in) then
       sqme = xi2_factor * (one - y**2) * sqme
    else
       sqme = xi2_factor * (one - y) * sqme
    end if
  end function soft_subtraction_compute

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
    do i = 1, sub_soft%reg_data%n_legs_born
       do j = 1, sub_soft%reg_data%n_legs_born
          if (i <= j) then
             num = p_born(i) * p_born(j)
             deno1 = p_born(i) * sub_soft%p_soft
             deno2 = p_born(j) * sub_soft%p_soft
             sub_soft%momentum_matrix(i, j) = num / (deno1 * deno2)
          else
             !!! momentum matrix is symmetric.
            sub_soft%momentum_matrix(i, j) = sub_soft%momentum_matrix(j, i)
          end if
       end do
    end do
  end subroutine soft_subtraction_compute_momentum_matrix

  function soft_subtraction_evaluate_factorization_threshold &
     (sub_soft, leg, p_born, born_ij) result (kb)
    real(default) :: kb
    class(soft_subtraction_t), intent(inout) :: sub_soft
    integer, intent(in) :: leg
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in), dimension(:,:) :: born_ij
    type(vector4_t), dimension(4) :: p
    p = get_threshold_momenta (p_born)
    kb = evaluate_leg_pair (ASSOCIATED_LEG_PAIR (leg))
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
               call msg_print_color('born_ij (i1,i2)', born_ij (i1,i2), COL_PINK)
               print *, 'Top momentum: ', p(1)%p
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
    end subroutine show_debug

  end function soft_subtraction_evaluate_factorization_threshold

  function soft_subtraction_i_xi_ref (sub_soft, alr, i_phs) result (i_xi_ref)
    integer :: i_xi_ref
    class(soft_subtraction_t), intent(in) :: sub_soft
    integer, intent(in) :: alr, i_phs
    if (sub_soft%use_resonance_mappings) then
       i_xi_ref = sub_soft%reg_data%alr_to_i_contributor (alr)
    else if (sub_soft%factorization_mode == FACTORIZATION_THRESHOLD) then
       i_xi_ref = i_phs
    else
       i_xi_ref = 1
    end if
  end function soft_subtraction_i_xi_ref

  subroutine soft_subtraction_final (sub_soft)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    if (associated (sub_soft%reg_data)) nullify (sub_soft%reg_data)
    if (allocated (sub_soft%momentum_matrix)) deallocate (sub_soft%momentum_matrix)
  end subroutine soft_subtraction_final

  subroutine soft_mismatch_init (soft_mismatch, reg_data, &
       real_kinematics, factorization_mode)
    class(soft_mismatch_t), intent(inout) :: soft_mismatch
    type(region_data_t), intent(in), target :: reg_data
    type(real_kinematics_t), intent(in), target :: real_kinematics
    integer, intent(in) :: factorization_mode
    soft_mismatch%reg_data => reg_data
    allocate (soft_mismatch%sqme_born (reg_data%n_flv_born))
    allocate (soft_mismatch%sqme_born_cc (reg_data%n_legs_born, &
         reg_data%n_legs_born, reg_data%n_flv_born))
    call soft_mismatch%sub_soft%init (reg_data)
    soft_mismatch%sub_soft%xi2_expanded = .false.
    soft_mismatch%real_kinematics => real_kinematics
    soft_mismatch%sub_soft%factorization_mode = factorization_mode
  end subroutine soft_mismatch_init

  function soft_mismatch_evaluate (soft_mismatch, alpha_s) result (sqme_mismatch)
    real(default) :: sqme_mismatch
    class(soft_mismatch_t), intent(inout) :: soft_mismatch
    real(default), intent(in) :: alpha_s
    integer :: alr, i_born, emitter, i_res, i_phs, i_con
    real(default) :: xi, y, q2, s
    real(default) :: E_gluon
    type(vector4_t) :: p_em
    real(default) :: sqme_alr, sqme_soft
    type(vector4_t), dimension(:), allocatable :: p_born
    sqme_mismatch = zero
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
          
       allocate (p_born (soft_mismatch%reg_data%n_legs_born))

       do alr = 1, soft_mismatch%reg_data%n_regions

           i_phs = real_kinematics%alr_to_i_phs (alr)
           if (soft_mismatch%reg_data%has_pseudo_isr ()) then
              i_con = 1
              p_born = soft_mismatch%real_kinematics%p_born_onshell%get_momenta(1)
           else
              i_con = soft_mismatch%reg_data%alr_to_i_contributor (alr)
              p_born = soft_mismatch%real_kinematics%p_born_cms%get_momenta(1)
           end if
           q2 = real_kinematics%xi_ref_momenta(i_con)**2
           emitter = soft_mismatch%reg_data%regions(alr)%emitter
           p_em = p_born (emitter)
           i_res = soft_mismatch%reg_data%regions(alr)%i_res
           i_born = soft_mismatch%reg_data%regions(alr)%uborn_index

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

           sqme_soft = soft_mismatch%sub_soft%compute &
                (p_born, soft_mismatch%sqme_born_cc(:,:,i_born), y, &
                q2, alpha_s, alr, emitter, i_res)

           sqme_alr = soft_mismatch%compute (alr, xi, y, p_em, &
                real_kinematics%xi_ref_momenta(i_con), soft_mismatch%sub_soft%p_soft, &
                soft_mismatch%sqme_born(i_born), sqme_soft, &
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
         print *, 'emitter: ', emitter, 'i_born: ', i_born
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

  subroutine soft_mismatch_final (soft_mismatch)
    class(soft_mismatch_t), intent(inout) :: soft_mismatch
    call soft_mismatch%sub_soft%final ()
    if (associated (soft_mismatch%reg_data)) nullify (soft_mismatch%reg_data)
    if (allocated (soft_mismatch%sqme_born)) deallocate (soft_mismatch%sqme_born)
    if (allocated (soft_mismatch%sqme_born_cc)) deallocate (soft_mismatch%sqme_born_cc)
    if (associated (soft_mismatch%real_kinematics)) nullify (soft_mismatch%real_kinematics)
  end subroutine soft_mismatch_final

  subroutine coll_subtraction_init (coll_sub, n_alr, n_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    integer, intent(in) :: n_alr, n_in
    coll_sub%n_in = n_in
    coll_sub%n_alr = n_alr
  end subroutine coll_subtraction_init

  function coll_subtraction_compute_fsr &
     (coll_sub, sregion, p_res, p_born, sqme_born, mom_times_sqme_born_sc, &
     xi, alpha_s, alr, double_fsr) result (sqme)
    real(default) :: sqme
    class(coll_subtraction_t), intent(in) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in) :: p_res
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born, mom_times_sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    logical, intent(in) :: double_fsr
    real(default) :: q0, z, p0
    real(default) :: z_o_xi, onemz
    real(default) :: pggz
    integer :: nlegs, emitter
    integer :: flv_em, flv_rad

    if (.not. vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
            check_conservation = .true., n_in = coll_sub%n_in)
       call msg_fatal ("Collinear subtraction, FSR: Phase space point &
            &must be in CMS")
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
       pggz = two * CA * (xi * z / onemz + onemz / z_o_xi)
       sqme = pggz * sqme_born + four * CA * z * onemz * xi * mom_times_sqme_born_sc
    else if (is_quark(abs(flv_em)) .and. is_quark (abs(flv_rad))) then
       sqme = TR * xi * (sqme_born - four * z * onemz * mom_times_sqme_born_sc)
    else if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
       sqme = sqme_born * CF * (one + onemz**2) / z_o_xi
    else
       sqme = zero
    end if
    sqme = sqme / (p0**2 * onemz * z_o_xi)
    sqme = sqme * four * pi * alpha_s
    if (double_fsr) sqme = sqme * onemz

  end function coll_subtraction_compute_fsr

  function coll_subtraction_compute_isr &
     (coll_sub, sregion, p_born, sqme_born, sqme_born_sc, &
     xi, alpha_s, alr, isr_mode) result (sqme)
    real(default) :: sqme
    class(coll_subtraction_t), intent(in) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr, isr_mode
    real(default) :: z, onemz
    real(default) :: p02
    integer :: flv_em, flv_rad
    integer :: nlegs

    if (vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true.)
       call msg_fatal ("Collinear subtraction, ISR: Phase space point &
          &must be in lab frame")
    end if

    nlegs = size (sregion%flst_real%flst)
    flv_rad = sregion%flst_real%flst(nlegs)
    flv_em = sregion%flst_real%flst(isr_mode)
    !!! No need to pay attention to n_in = 1, because this case always has a
    !!! massive initial-state particle and thus no collinear divergence.
    p02 = p_born(1)%p(0) * p_born(2)%p(0) / two
    z = one - xi; onemz = xi

    if (is_quark(flv_em) .and. is_gluon(flv_rad)) then
       sqme = CF * (one + z**2) * sqme_born
    else if (is_gluon(flv_em) .and. is_quark (flv_rad)) then
       sqme = TR * (z**2 + onemz**2) * onemz * sqme_born
    else
       sqme = zero
    end if
    sqme = sqme * z / p02
    sqme = sqme * four * pi * alpha_s
  end function coll_subtraction_compute_isr

  subroutine coll_subtraction_final (sub_coll)
    class(coll_subtraction_t), intent(inout) :: sub_coll
    sub_coll%use_resonance_mappings = .false.
  end subroutine coll_subtraction_final

  subroutine real_subtraction_init (rsub, reg_data)
    class(real_subtraction_t), intent(inout), target :: rsub
    type(region_data_t), intent(in), target :: reg_data
    integer :: alr, i_born
    call msg_debug (D_SUBTRACTION, "real_subtraction_init")
    call msg_debug (D_SUBTRACTION, "n_in", reg_data%n_in)
    call msg_debug (D_SUBTRACTION, "nlegs_born", reg_data%n_legs_born)
    call msg_debug (D_SUBTRACTION, "nlegs_real", reg_data%n_legs_real)
    call msg_debug (D_SUBTRACTION, "reg_data%n_regions", reg_data%n_regions)

    if (debug2_active (D_SUBTRACTION))  call reg_data%write ()
    rsub%reg_data => reg_data
    allocate (rsub%sqme_born (reg_data%n_flv_born))
    rsub%sqme_born = zero
    allocate (rsub%sqme_born_cc (reg_data%n_legs_born, reg_data%n_legs_born, &
         reg_data%n_flv_born))
    allocate (rsub%sqme_real_non_sub (reg_data%n_flv_real, reg_data%n_phs))
    allocate (rsub%sc_required (reg_data%n_regions))
    do alr = 1, reg_data%n_regions
       rsub%sc_required(alr) = any (reg_data%regions(alr)%flst_uborn%flst == GLUON)
    end do
    if (rsub%requires_spin_correlations ()) then
       allocate (rsub%sqme_born_sc (0:3, 0:3, reg_data%n_legs_born, reg_data%n_flv_born))
       rsub%sqme_born_sc = zero
    end if

    call rsub%sub_soft%init (reg_data)
    call rsub%sub_coll%init (reg_data%n_regions, reg_data%n_in)

    if (rsub%reg_data%n_in > 1 .and. any (rsub%reg_data%get_emitter_list () <= 2)) then
       call rsub%init_pdfs ()
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

  function real_subtraction_get_i_res (rsub, alr) result (i_res)
    integer :: i_res
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr
    select type (fks_mapping => rsub%reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       i_res = fks_mapping%res_map%alr_to_i_res (alr)
    class default
       i_res = 0
    end select
  end function real_subtraction_get_i_res

  subroutine real_subtraction_compute (rsub, emitter, i_phs, alpha_s, &
           separate_alrs, sqme)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_phs
    logical, intent(in) :: separate_alrs
    real(default), intent(inout), dimension(:) :: sqme
    real(default) :: sqme_alr, alpha_s
    integer :: alr, i_con, i_res, this_emitter
    logical :: same_emitter
    do alr = 1, rsub%reg_data%n_regions
       if (allocated (rsub%selected_alr)) then
          if (.not. any (rsub%selected_alr == alr)) cycle
       end if
       sqme_alr = zero
       if (emitter > rsub%isr_kinematics%n_in) then
          same_emitter = emitter == rsub%reg_data%regions(alr)%emitter
       else
          same_emitter = rsub%reg_data%regions(alr)%emitter <= rsub%isr_kinematics%n_in
       end if
       if (same_emitter .and. i_phs == rsub%real_kinematics%alr_to_i_phs (alr)) then
          i_res = rsub%get_i_res (alr)
          this_emitter = rsub%reg_data%regions(alr)%emitter
          sqme_alr = rsub%evaluate_emitter_region (alr, this_emitter, i_phs, i_res, alpha_s)
          if (rsub%purpose == INTEGRATION .or. rsub%purpose == FIXED_ORDER_EVENTS) then
             i_con = rsub%get_i_contributor (alr)
             sqme_alr = sqme_alr * rsub%get_phs_factor (i_con)
          end if
       end if
       if (separate_alrs) then
          sqme(alr) = sqme(alr) + sqme_alr
       else
          sqme(1) = sqme(1) + sqme_alr
       end if
    end do
    if (debug2_active (D_SUBTRACTION)) call check_s_alpha_consistency ()
  contains
    subroutine check_s_alpha_consistency ()
      real(default) :: sum_s_alpha, sum_s_alpha_soft
      integer :: i_reg, i1, i2
      call msg_debug2 (D_SUBTRACTION, "Check consistency of s_alpha: ")
      do i_reg = 1, rsub%reg_data%n_regions
         sum_s_alpha = zero; sum_s_alpha_soft = zero
         do alr = 1, rsub%reg_data%regions(i_reg)%nregions
            call rsub%reg_data%regions(i_reg)%ftuples(alr)%get (i1, i2)
            call rsub%evaluate_emitter_region_debug (i_reg, alr, i1, i2, i_phs, &
                 sum_s_alpha, sum_s_alpha_soft)
         end do
      end do
    end subroutine check_s_alpha_consistency
  end subroutine real_subtraction_compute

  function real_subtraction_evaluate_emitter_region (rsub, alr, emitter, &
      i_phs, i_res, alpha_s) result (sqme)
    real(default) :: sqme
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    if (emitter <= rsub%isr_kinematics%n_in) then
       sqme = rsub%evaluate_region_isr (alr, emitter, i_phs, i_res, alpha_s)
    else
       select type (fks_mapping => rsub%reg_data%fks_mapping)
       type is (fks_mapping_resonances_t)
          call fks_mapping%set_resonance_momenta &
               (rsub%real_kinematics%xi_ref_momenta)
       end select
       sqme = rsub%evaluate_region_fsr (alr, emitter, i_phs, i_res, alpha_s)
    end if
  end function real_subtraction_evaluate_emitter_region

  subroutine real_subtraction_evaluate_emitter_region_debug (rsub, i_reg, alr, i1, i2, &
       i_phs, sum_s_alpha, sum_s_alpha_soft)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: i_reg, alr, i1, i2, i_phs
    real(default), intent(inout) :: sum_s_alpha, sum_s_alpha_soft
    type(vector4_t), dimension(:), allocatable :: p_real, p_born
    integer :: this_emitter, i_res
    allocate (p_real (rsub%reg_data%n_legs_real))
    allocate (p_born (rsub%reg_data%n_legs_born))
    if (rsub%reg_data%has_pseudo_isr ()) then
       p_real = rsub%real_kinematics%p_real_onshell(i_phs)%get_momenta (i_phs)
       p_born = rsub%real_kinematics%p_born_onshell%get_momenta (1)
    else
       p_real = rsub%real_kinematics%p_real_cms%get_momenta (i_phs)
       p_born = rsub%real_kinematics%p_born_cms%get_momenta (1)
    end if
    i_res = rsub%get_i_res (i_reg)
    sum_s_alpha = sum_s_alpha + rsub%reg_data%get_svalue (p_real, i_reg, i1, i2, i_res)
    associate (r => rsub%real_kinematics)
       call rsub%sub_soft%create_softvec_fsr (p_born, r%y_soft(i_phs), r%phi, &
            i1, r%xi_ref_momenta(rsub%sub_soft%i_xi_ref (i_reg, i_phs)))
    end associate
    sum_s_alpha_soft = sum_s_alpha_soft + rsub%reg_data%get_svalue_soft &
         (p_born, rsub%sub_soft%p_soft, i_reg, i1, i_res)
  end subroutine real_subtraction_evaluate_emitter_region_debug

  function real_subtraction_evaluate_region_fsr (rsub, alr, emitter, i_phs, &
       i_res, alpha_s) result (sqme_tot)
    real(default) :: sqme_tot
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    real(default) :: sqme_rad, sqme_soft, sqme_coll, sqme_cs, sqme_remn
    sqme_rad = zero; sqme_soft = zero; sqme_coll = zero
    sqme_cs = zero; sqme_remn = zero
    associate (region => rsub%reg_data%regions(alr))
      if (rsub%radiation_event) then
         sqme_rad = rsub%sqme_real_non_sub (rsub%reg_data%get_matrix_element_index (alr), i_phs)
         call evaluate_fks_factors (sqme_rad, rsub%reg_data, rsub%real_kinematics, &
              alr, i_phs, emitter, i_res)
         call apply_kinematic_factors_radiation (sqme_rad, rsub%purpose, &
              rsub%real_kinematics, i_phs, .false., rsub%reg_data%has_pseudo_isr (), &
              emitter)
      end if
      if (rsub%subtraction_event .and. .not. rsub%subtraction_deactivated) then
         call rsub%evaluate_subtraction_terms_fsr (alr, emitter, i_phs, i_res, alpha_s, &
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

    if (debug_active (D_SUBTRACTION) .and. .not. debug2_active (D_SUBTRACTIOn)) then
       call register_debug_sqme ()
    else if (debug2_active (D_SUBTRACTION)) then
       call write_computation_status ()
    end if

  contains

    subroutine register_debug_sqme ()
      real(default), dimension(:), allocatable, save :: sqme_rad_store
      logical :: soft, collinear
      real(default), parameter :: soft_threshold = 0.1_default
      real(default), parameter :: coll_threshold = 0.1_default
      real(default) :: this_sqme_rad, s_alpha, E_gluon
      logical, dimension(:), allocatable, save :: count_alr
      !!! TODO (cw-2017-02-18): Need to be able to set this (?)
      logical :: write_histo = .true.
      if (.not. allocated (sqme_rad_store)) then
         allocate (sqme_rad_store (rsub%reg_data%n_regions))
         sqme_rad_store = zero
      end if
      if (rsub%radiation_event) then
         sqme_rad_store(alr) = sqme_rad
      else
         if (.not. allocated (count_alr)) then
            allocate (count_alr (rsub%reg_data%n_regions))
            count_alr = .false.
         end if
         associate (p_real => rsub%real_kinematics%p_real_cms)
            E_gluon = p_real%get_energy (i_phs, rsub%reg_data%n_legs_real)
            s_alpha = rsub%reg_data%get_svalue (p_real%get_momenta(i_phs), alr, emitter, i_res)
         end associate
         soft = E_gluon < soft_threshold
         collinear = abs (s_alpha - one) < coll_threshold
         this_sqme_rad = sqme_rad_store(alr)
         if (soft) then
            !!! Do not write sqme_rad twice
            if (write_histo .and. .not. rsub%radiation_event) &
                 call write_point_to_file (E_gluon, this_sqme_rad, sqme_soft)
            if (abs (this_sqme_rad - sqme_soft) > min (this_sqme_rad, sqme_soft) .and. &
                 sqme_soft > tiny_10 .and. this_sqme_rad > tiny_10) then
               call msg_print_color ("Soft MEs do not match", COL_RED)
            else
               call msg_print_color (char ("sqme_soft OK in region " // str (alr)), COL_GREEN)
            end if
         end if
         if (collinear) then
            if (abs (this_sqme_rad - sqme_coll) > min (this_sqme_rad, sqme_coll) .and. &
                 sqme_coll > tiny_10 .and. this_sqme_rad > tiny_10) then
               call msg_print_color ("Collinear MEs do not match", COL_RED)
               print *, 'alr: ', alr, 'sqme_rad: ', this_sqme_rad, 'sqme_coll: ', sqme_coll
            else
               call msg_print_color (char ("sqme_coll OK in region " // str (alr)), COL_GREEN)
            end if
         end if
         if (soft .and. collinear) then
            if (abs (this_sqme_rad - sqme_cs) > min (this_sqme_rad, sqme_cs) .and. &
                 sqme_cs > tiny_10 .and. this_sqme_rad > tiny_10) then
               call msg_print_color ("Soft-collinear MEs do not match", COL_RED)
            else
               call msg_print_color (char ("sqme_cs OK in region " // str (alr)), COL_GREEN)
            end if
         end if
         count_alr (alr) = .true.
         if (all (count_alr)) then
            deallocate (count_alr)
            deallocate (sqme_rad_store)
         end if
      end if
    end subroutine register_debug_sqme

    subroutine write_computation_status (passed, total, region_type)
      integer, intent(in), optional :: passed, total
      character(*), intent(in), optional :: region_type
      integer :: i_born
      integer :: u
      real(default) :: xi
      call msg_debug (D_SUBTRACTION, "real_subtraction_evaluate_region_fsr")
      u = given_output_unit (); if (u < 0) return
      i_born = rsub%reg_data%regions(alr)%uborn_index
      xi = rsub%real_kinematics%xi_max (i_phs) * rsub%real_kinematics%xi_tilde
      write (u,'(A,I2)') 'rsub%purpose: ', rsub%purpose
      write (u,'(A,I3)') 'alr: ', alr
      write (u,'(A,I3)') 'emitter: ', emitter
      write (u,'(A,I3)') 'i_phs: ', i_phs
      write (u,'(A,F6.4)') 'xi_max: ', rsub%real_kinematics%xi_max (i_phs)
      write (u,'(A,F6.4,2X,A,F6.4)') 'xi: ', xi, 'y: ', rsub%real_kinematics%y (i_phs)
      write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_born)
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

  function real_subtraction_evaluate_region_isr (rsub, alr, emitter, i_phs, i_res, alpha_s) &
       result (sqme_tot)
    real(default) :: sqme_tot
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    integer :: i_real
    real(default) :: sqme_rad, sqme_soft, sqme_coll_plus, sqme_coll_minus
    real(default) :: sqme_cs_plus, sqme_cs_minus
    real(default) :: sqme_remn
    logical :: proc_scatter

    proc_scatter = rsub%isr_kinematics%n_in == 2

    sqme_rad = zero; sqme_soft = zero;
    sqme_coll_plus = zero; sqme_coll_minus = zero
    sqme_cs_plus = zero; sqme_cs_minus = zero
    sqme_remn = zero

    if (proc_scatter) call rsub%compute_pdfs ()
    associate (region => rsub%reg_data%regions(alr))
      i_real = region%real_index
      if (rsub%radiation_event) then
         sqme_rad = rsub%sqme_real_non_sub (1, i_phs)
         call evaluate_fks_factors (sqme_rad, rsub%reg_data, rsub%real_kinematics, &
              alr, i_phs, emitter, i_res)
         if (proc_scatter) then
            call rsub%scale_pdfs_real (sqme_rad, i_real, I_PLUS)
            call rsub%scale_pdfs_real (sqme_rad, i_real, I_MINUS)
         end if

         call apply_kinematic_factors_radiation (sqme_rad, rsub%purpose, rsub%real_kinematics, &
            i_phs, .true., .false.)
      end if
      if (rsub%subtraction_event .and. .not. rsub%subtraction_deactivated) then
         call rsub%evaluate_subtraction_terms_isr (alr, emitter, i_phs, i_res, alpha_s, &
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
          call msg_debug (D_SUBTRACTION, "real_subtraction_evaluate_region_isr")
          if (debug_active (D_SUBTRACTION)) then
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
       integer :: i_born
       integer :: u
       real(default) :: xi
       u = given_output_unit (unit); if (u < 0) return
       i_born = rsub%reg_data%regions(alr)%uborn_index
       xi = rsub%real_kinematics%xi_max (i_phs) * rsub%real_kinematics%xi_tilde
       write (u,'(A,I2)') 'alr: ', alr
       write (u,'(A,I2)') 'emitter: ', emitter
       write (u,'(A,F4.2)') 'xi_max: ', rsub%real_kinematics%xi_max (i_phs)
       print *, 'xi: ', xi, 'y: ', rsub%real_kinematics%y (i_phs)
       print *, 'xb1: ', rsub%isr_kinematics%x(1), 'xb2: ', rsub%isr_kinematics%x(2)
       print *, 'random jacobian: ', rsub%real_kinematics%jac_rand (i_phs)
       write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_born)
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
       alr, emitter, i_phs, i_res, alpha_s, sqme_soft, sqme_coll, sqme_cs)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft, sqme_coll, sqme_cs
    sqme_soft = rsub%compute_sub_soft (alr, emitter, i_phs, i_res, alpha_s)
    sqme_coll = rsub%compute_sub_coll (alr, emitter, i_phs, alpha_s)
    sqme_cs = rsub%compute_sub_coll_soft (alr, emitter, alpha_s)
  end subroutine real_subtraction_evaluate_subtraction_terms_fsr

  subroutine evaluate_fks_factors (sqme, reg_data, real_kinematics, &
      alr, i_phs, emitter, i_res)
    real(default), intent(inout) :: sqme
    type(region_data_t), intent(inout) :: reg_data
    type(real_kinematics_t), intent(in), target :: real_kinematics
    integer, intent(in) :: alr, i_phs, emitter, i_res
    real(default) :: s_alpha
    type(phs_point_set_t), pointer :: p_real => null ()
    if (reg_data%has_pseudo_isr ()) then
       p_real => real_kinematics%p_real_onshell (i_phs)
    else
       p_real => real_kinematics%p_real_cms
    end if
    s_alpha = reg_data%get_svalue (p_real%get_momenta(i_phs), alr, emitter, i_res)
    if (debug_active (D_SUBTRACTION)) call msg_print_color('s_alpha', s_alpha, COL_YELLOW)
    if (s_alpha > one + tiny_07) call msg_fatal ("s_alpha > 1!")
    sqme = sqme * s_alpha
    associate (region => reg_data%regions(alr))
       sqme = sqme * region%mult
       if (emitter > reg_data%n_in) &
            sqme = sqme * region%double_fsr_factor (p_real%get_momenta(i_phs))
    end associate
  end subroutine evaluate_fks_factors

  subroutine apply_kinematic_factors_radiation (sqme, purpose, real_kinematics, &
         i_phs, isr, threshold, emitter)
    real(default), intent(inout) :: sqme
    integer, intent(in) :: purpose
    type(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: i_phs
    logical, intent(in) :: isr, threshold
    integer, intent(in), optional :: emitter
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
      alr, emitter, i_phs, i_res, alpha_s, sqme_soft, sqme_coll_plus, &
      sqme_coll_minus, sqme_cs_plus, sqme_cs_minus)

    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft
    real(default), intent(out) :: sqme_coll_plus, sqme_coll_minus
    real(default), intent(out) :: sqme_cs_plus, sqme_cs_minus
    sqme_soft = rsub%compute_sub_soft (alr, emitter, i_phs, i_res, alpha_s)
    if (emitter /= 2) then
       sqme_coll_plus = rsub%compute_sub_coll (alr, 1, i_phs, alpha_s)
       sqme_cs_plus = rsub%compute_sub_coll_soft (alr, 1, alpha_s)
    else
       sqme_coll_plus = zero
       sqme_cs_plus = zero
    end if
    if (emitter /= 1) then
       sqme_coll_minus = rsub%compute_sub_coll (alr, 2, i_phs, alpha_s)
       sqme_cs_minus = rsub%compute_sub_coll_soft (alr, 2, alpha_s)
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

  function real_subtraction_compute_sub_soft (rsub, alr, emitter, &
       i_phs, i_res, alpha_s) result (sqme_soft)
    real(default) :: sqme_soft
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, emitter, i_phs, i_res
    real(default), intent(in) :: alpha_s
    integer :: i_xi_ref, i_born
    real(default) :: q2
    type(vector4_t), dimension(:), allocatable :: p_born
    associate (real_kinematics => rsub%real_kinematics)
       if (rsub%reg_data%regions(alr)%has_soft_divergence ()) then
          i_xi_ref = rsub%sub_soft%i_xi_ref (alr, i_phs)
          q2 = real_kinematics%xi_ref_momenta (i_xi_ref)**2
          allocate (p_born (rsub%reg_data%n_legs_born))
          if (rsub%reg_data%has_pseudo_isr ()) then
             p_born = real_kinematics%p_born_onshell%get_momenta(1)
          else
             p_born = real_kinematics%p_born_cms%get_momenta(1)
          end if
          if (emitter > rsub%sub_soft%reg_data%n_in) then
             call rsub%sub_soft%create_softvec_fsr &
                  (p_born, real_kinematics%y_soft(i_phs), &
                  real_kinematics%phi, emitter, &
                  real_kinematics%xi_ref_momenta(i_xi_ref))
          else
             call rsub%sub_soft%create_softvec_isr &
                (real_kinematics%y_soft(i_phs), real_kinematics%phi)
          end if
          i_born = rsub%reg_data%regions(alr)%uborn_index
          sqme_soft = rsub%sub_soft%compute &
               (p_born, rsub%sqme_born_cc(:,:,i_born), &
               real_kinematics%y(i_phs), &
               q2, alpha_s, alr, emitter, i_res)
       else
          sqme_soft = zero
       end if
    end associate
    if (debug2_active (D_SUBTRACTION)) call check_soft_vector ()
  contains
    subroutine check_soft_vector ()
      type(vector4_t) :: p_gluon
      call msg_debug2 (D_SUBTRACTION, "Compare soft vector: ")
      print *, 'p_soft: ', rsub%sub_soft%p_soft%p
      print *, 'Normalized gluon momentum: '
      if (rsub%reg_data%has_pseudo_isr ()) then
         p_gluon = rsub%real_kinematics%p_real_onshell(thr_leg(emitter))%get_momentum &
              (i_phs, rsub%reg_data%n_legs_real)
      else
         p_gluon = rsub%real_kinematics%p_real_cms%get_momentum &
              (i_phs, rsub%reg_data%n_legs_real)
      end if
      call vector4_write (p_gluon / p_gluon%p(0), show_mass = .true.)
    end subroutine check_soft_vector
  end function real_subtraction_compute_sub_soft

  function real_subtraction_get_spin_correlation_term (rsub, alr, i_born, emitter) &
        result (mom_times_sqme)
    real(default) :: mom_times_sqme
    class(real_subtraction_t), intent(in) :: rsub
    integer, intent(in) :: alr, i_born, emitter
    real(default), dimension(0:3) :: k_perp
    integer :: i, mu, nu

    if (rsub%sc_required(alr)) then
       if (debug2_active(D_SUBTRACTION)) call check_me_consistency ()
       associate (real_kin => rsub%real_kinematics)
          if (emitter > rsub%reg_data%n_in) then
             k_perp = k_perp_fsr (real_kin%p_born_cms%get_momentum(1, emitter), &
                  rsub%real_kinematics%phi)
          else
             k_perp = k_perp_isr (real_kin%p_born_cms%get_momentum(1, emitter), &
                  rsub%real_kinematics%phi)
          end if
       end associate
       mom_times_sqme = zero
       do mu = 0, 3
          do nu = 0, 3
             mom_times_sqme = mom_times_sqme + &
                  k_perp(mu) * k_perp(nu) * rsub%sqme_born_sc (mu, nu, emitter, i_born)
          end do
       end do
    else
       mom_times_sqme = zero
    end if
  contains
    subroutine check_me_consistency ()
      real(default) ::  sqme_sum
      call msg_debug2 (D_SUBTRACTION, "Spin-correlation: Consistency check")
      sqme_sum = rsub%sqme_born_sc(0,0,emitter,i_born) &
               - rsub%sqme_born_sc(1,1,emitter,i_born) &
               - rsub%sqme_born_sc(2,2,emitter,i_born) &
               - rsub%sqme_born_sc(3,3,emitter,i_born)
      if (.not. nearly_equal (sqme_sum, -rsub%sqme_born(i_born), 0.0001_default)) then
         print *, 'Spin-correlated matrix elements are not consistent: '
         print *, 'emitter: ', emitter
         print *, 'g^{mu,nu} B_{mu,nu}: ', -sqme_sum
         print *, 'all Born matrix elements: ', rsub%sqme_born
         call msg_fatal ("FAIL")
      else
         call msg_print_color ("Success", COL_GREEN)
      end if
    end subroutine check_me_consistency

    function k_perp_fsr (p, phi)
      real(default), dimension(0:3) :: k_perp_fsr
      type(vector4_t), intent(in) :: p
      real(default), intent(in) :: phi
      type(vector4_t) :: k
      type(vector3_t) :: vec
      type(lorentz_transformation_t) :: rot
      vec = p%p(1:3) / p%p(0)
      k%p(0) = zero
      k%p(1) = p%p(1); k%p(2) = p%p(2)
      k%p(3) = - (p%p(1)**2 + p%p(2)**2) / p%p(3)
      rot = rotation (cos(phi), sin(phi), vec)
      k = rot * k
      k%p(1:3) = k%p(1:3) / space_part_norm (k)
      k_perp_fsr = k%p
    end function k_perp_fsr

    function k_perp_isr (p, phi)
      real(default), dimension(0:3) :: k_perp_isr
      type(vector4_t), intent(in) :: p
      real(default), intent(in) :: phi
      k_perp_isr(0) = zero
      k_perp_isr(1) = cos(phi)
      k_perp_isr(2) = sin(phi)
      k_perp_isr(3) = zero
    end function k_perp_isr
  end function real_subtraction_get_spin_correlation_term

  function real_subtraction_compute_sub_coll (rsub, alr, em, i_phs, alpha_s) result (sqme_coll)
    real(default) :: sqme_coll
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, em, i_phs
    real(default), intent(in) :: alpha_s
    real(default) :: xi, xi_max_pm
    real(default) :: mom_times_sqme_sc
    integer :: i_con
    real(default) :: pfr
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          xi = rsub%real_kinematics%xi_tilde * rsub%real_kinematics%xi_max (i_phs)
          if (rsub%sub_coll%use_resonance_mappings) then
             i_con = rsub%reg_data%alr_to_i_contributor (alr)
          else
             i_con = 1
          end if
          mom_times_sqme_sc = rsub%get_spin_correlation_term (alr, sregion%uborn_index, em)
          if (em <= rsub%sub_coll%n_in) then
             xi_max_pm = one - rsub%isr_kinematics%x(em)
             xi = rsub%real_kinematics%xi_tilde * xi_max_pm
             sqme_coll = rsub%sub_coll%compute_isr (sregion, &
                  rsub%real_kinematics%p_born_lab%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), mom_times_sqme_sc, &
                  xi, alpha_s, alr, em)
          else
             sqme_coll = rsub%sub_coll%compute_fsr (sregion, &
                    rsub%real_kinematics%xi_ref_momenta (i_con), &
                    rsub%real_kinematics%p_born_cms%get_momenta(1), &
                    rsub%sqme_born(sregion%uborn_index), mom_times_sqme_sc, &
                    xi, alpha_s, alr, sregion%double_fsr)
             if (rsub%sub_coll%use_resonance_mappings) then
                select type (fks_mapping => rsub%reg_data%fks_mapping)
                type is (fks_mapping_resonances_t)
                   pfr = fks_mapping%get_resonance_weight (alr, &
                         rsub%real_kinematics%p_born_cms%get_momenta(1))
                end select
                sqme_coll = sqme_coll * pfr
             end if
          end if
       else
          sqme_coll = zero
       end if
    end associate
  end function real_subtraction_compute_sub_coll

  function real_subtraction_compute_sub_coll_soft (rsub, alr, em, alpha_s) result (sqme_cs)
    real(default) :: sqme_cs
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: alr, em
    real(default), intent(in) :: alpha_s
    real(default) :: mom_times_sqme_sc
    integer :: i_con
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          if (rsub%sub_coll%use_resonance_mappings) then
             i_con = rsub%reg_data%alr_to_i_contributor (alr)
          else
             i_con = 1
          end if
          mom_times_sqme_sc = rsub%get_spin_correlation_term (alr, sregion%uborn_index, em)
          if (em <= rsub%sub_coll%n_in) then
             sqme_cs = rsub%sub_coll%compute_isr (sregion, &
                  rsub%real_kinematics%p_born_lab%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), mom_times_sqme_sc, &
                  zero, alpha_s, alr, em)
          else
             sqme_cs = rsub%sub_coll%compute_fsr (sregion, &
                  rsub%real_kinematics%xi_ref_momenta(i_con), &
                  rsub%real_kinematics%p_born_cms%phs_point(1)%p, &
                  rsub%sqme_born(sregion%uborn_index), mom_times_sqme_sc, &
                  zero, alpha_s, alr, sregion%double_fsr)
          end if
       else
          sqme_cs = zero
       end if
    end associate
  end function real_subtraction_compute_sub_coll_soft

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

  function real_subtraction_requires_spin_correlations (rsub) result (val)
    logical :: val
    class(real_subtraction_t), intent(in) :: rsub
    val = any (rsub%sc_required)
  end function real_subtraction_requires_spin_correlations

  subroutine real_subtraction_final (rsub)
    class(real_subtraction_t), intent(inout) :: rsub
    call rsub%sub_soft%final ()
    call rsub%sub_coll%final ()
    !!! Finalization of region data is done in pcm_nlo_final
    if (associated (rsub%reg_data)) nullify (rsub%reg_data)
    !!! Finalization of real kinematics is done in pcm_instance_nlo_final
    if (associated (rsub%real_kinematics)) nullify (rsub%real_kinematics)
    if (associated (rsub%isr_kinematics)) nullify (rsub%isr_kinematics)
    if (allocated (rsub%sqme_real_non_sub)) deallocate (rsub%sqme_real_non_sub)
    if (allocated (rsub%sqme_born)) deallocate (rsub%sqme_born)
    if (allocated (rsub%sqme_born_cc)) deallocate (rsub%sqme_born_cc)
    if (allocated (rsub%sc_required)) deallocate (rsub%sc_required)
    if (allocated (rsub%selected_alr)) deallocate (rsub%selected_alr)
  end subroutine real_subtraction_final

  function powheg_damping_simple_get_f (partition, p) result (f)
    real(default) :: f
    class(powheg_damping_simple_t), intent(in) :: partition
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: pt2
    !!! TODO (cw-2017-03-01) Compute pt2 from emitter)
    f = partition%h2 / (pt2 + partition%h2)
  end function powheg_damping_simple_get_f

  subroutine powheg_damping_simple_init (partition, scale, reg_data)
    class(powheg_damping_simple_t), intent(out) :: partition
    real(default), intent(in) :: scale
    type(region_data_t), intent(in) :: reg_data
    partition%h2 = scale**2
  end subroutine powheg_damping_simple_init

  subroutine powheg_damping_simple_write (partition, unit)
    class(powheg_damping_simple_t), intent(in) :: partition
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(1x,A)") "Powheg damping simple: "
    write (u, "(1x,A, "// FMT_15 // ")") "scale h2: ", partition%h2
  end subroutine powheg_damping_simple_write

  subroutine real_partition_fixed_order_init (partition, scale, reg_data)
    class(real_partition_fixed_order_t), intent(out) :: partition
    real(default), intent(in) :: scale
    type(region_data_t), intent(in) :: reg_data
  end subroutine real_partition_fixed_order_init

  subroutine real_partition_fixed_order_write (partition, unit)
    class(real_partition_fixed_order_t), intent(in) :: partition
    integer, intent(in), optional :: unit
  end subroutine real_partition_fixed_order_write

  function real_partition_fixed_order_get_f (partition, p) result (f)
    real(default) :: f
    class(real_partition_fixed_order_t), intent(in) :: partition
    type(vector4_t), intent(in), dimension(:) :: p
    integer :: i
    f = zero
    do i = 1, size (partition%fks_pairs)
       associate (ii => partition%fks_pairs(i)%ireg)
          if ((p(ii(1)) + p(ii(2)))**1 < p(ii(1))**1 + p(ii(2))**1 + partition%scale) then
             f = one
             exit 
          end if
       end associate
    end do
  end function real_partition_fixed_order_get_f


end module real_subtraction
