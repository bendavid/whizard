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

module real_subtraction

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units, only: given_output_unit
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use constants
  use unit_tests
  use diagnostics
  use pdg_arrays
  use model_data
  use physics_defs
  use sm_physics
  use sf_lhapdf
  use pdf
  use lorentz
  use flavors
  use fks_regions
  use nlo_data

  implicit none
  private

  public :: real_subtraction_t

  integer, parameter, public :: INTEGRATION = 0
  integer, parameter, public :: FIXED_ORDER_EVENTS = 1
  integer, parameter, public :: POWHEG = 2


  type :: soft_subtraction_t
    real(default), dimension(:), allocatable :: value
    type(region_data_t) :: reg_data
    integer :: n_in, nlegs_born, nlegs_real
    real(default), dimension(:,:), allocatable :: momentum_matrix
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
    type(pdf_container_t), pointer :: pdf_born_plus => null ()
    type(pdf_container_t), pointer :: pdf_born_minus => null ()
  contains
    procedure :: init => soft_subtraction_init
    procedure :: compute => soft_subtraction_compute
    procedure :: compute_momentum_matrix => &
         soft_subtraction_compute_momentum_matrix
  end type soft_subtraction_t

  type :: coll_subtraction_t
    real(default), dimension(:), allocatable :: value
    real(default), dimension(:), allocatable :: value_soft
    integer :: n_in, n_alr
    real(default), dimension(0:3,0:3) :: b_munu
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
    type(region_data_t) :: reg_data
    type(pdf_data_t) :: pdf_data
    type(real_kinematics_t), pointer :: real_kinematics => null()
    type(isr_kinematics_t), pointer :: isr_kinematics => null()
    integer :: current_alr = 0
    real(default), dimension(:), pointer :: sqme_real_non_sub => null ()
    real(default), dimension(:), pointer :: sqme_born => null ()
    real(default), dimension(:,:,:), pointer :: sqme_born_cc => null ()
    complex(default), dimension(:), pointer :: sqme_born_sc => null()
    type(soft_subtraction_t) :: sub_soft
    type(coll_subtraction_t) :: sub_coll
    logical, dimension(:), allocatable :: sc_required
    integer :: purpose = INTEGRATION
    logical :: radiation_active = .true.
    logical :: subtraction_active = .true.
    type(pdf_container_t), dimension(2) :: pdf_born, pdf_scaled
  contains
    procedure :: init => real_subtraction_init
    procedure :: init_pdfs => real_subtraction_init_pdfs
    procedure :: set_real_kinematics => real_subtraction_set_real_kinematics
    procedure :: set_isr_kinematics => real_subtraction_set_isr_kinematics
    procedure :: set_alr => real_subtraction_set_alr
    procedure :: compute => real_subtraction_compute
    procedure :: evaluate_region_fsr => real_subtraction_evaluate_region_fsr
    procedure :: evaluate_region_isr => real_subtraction_evaluate_region_isr
    procedure :: evaluate_subtraction_terms_fsr => &
                         real_subtraction_evaluate_subtraction_terms_fsr
    procedure :: evaluate_subtraction_terms_isr => &
                         real_subtraction_evaluate_subtraction_terms_isr
    procedure :: get_phs_factor => real_subtraction_get_phs_factor
    procedure :: compute_sub_soft => real_subtraction_compute_sub_soft
    procedure :: get_sc_matrix_element => real_subtraction_get_sc_matrix_element
    procedure :: compute_sub_coll => real_subtraction_compute_sub_coll
    procedure :: compute_sub_coll_soft => real_subtraction_compute_sub_coll_soft
    procedure :: compute_pdfs => real_subtraction_compute_pdfs
    procedure :: reweight_pdfs => real_subtraction_reweight_pdfs
  end type real_subtraction_t


contains

  subroutine soft_subtraction_init (sub_soft, reg_data, &
      n_in, nlegs_born, nlegs_real)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: n_in, nlegs_born, nlegs_real
    sub_soft%reg_data = reg_data
    sub_soft%n_in = n_in
    sub_soft%nlegs_born = nlegs_born
    sub_soft%nlegs_real = nlegs_real
    allocate (sub_soft%value (reg_data%n_regions))
    allocate (sub_soft%momentum_matrix &
              (nlegs_born, nlegs_born))
  end subroutine soft_subtraction_init

  function create_softvec_fsr (p_born, y, phi, emitter) result (p_soft)
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: y, phi
    integer, intent(in) :: emitter
    type(vector4_t) :: p_soft
    type(vector3_t) :: dir
    type(lorentz_transformation_t) :: rot
    p_soft%p(0) = one
    p_soft%p(1:3) = p_born(emitter)%p(1:3) / space_part_norm (p_born(emitter))
    dir = create_orthogonal (space_part (p_born(emitter)))
    rot = rotation (y, sqrt(one - y**2), dir)
    p_soft = rot * p_soft
    if (.not. vanishes (phi)) then
      dir = space_part (p_born(emitter)) / &
            space_part_norm (p_born(emitter))
      rot = rotation (cos(phi), sin(phi), dir)
      p_soft = rot * p_soft
    end if
  end function create_softvec_fsr

  function create_softvec_isr (y, phi) result (p_soft)
    real(default), intent(in) :: y, phi
    type(vector4_t) :: p_soft
    real(default) :: sin_theta
    sin_theta = sqrt(one - y**2)
    p_soft%p(0) = one
    p_soft%p(1) = sin_theta * sin(phi)
    p_soft%p(2) = sin_theta * cos(phi)
    p_soft%p(3) = y
  end function create_softvec_isr

  subroutine soft_subtraction_compute (sub_soft, p_born, &
             born_ij, y, y_soft, phi, alpha_s_born, alr, emitter)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in), dimension(:,:) :: born_ij
    real(default), intent(in) :: y, y_soft, phi
    real(default), intent(in) :: alpha_s_born
    integer, intent(in) :: alr, emitter
    type(vector4_t) :: p_soft
    real(default) :: s_alpha_soft
    real(default) :: q2
    real(default) :: kb
    integer :: i, j

    if (.not. vector_set_is_cms (p_born)) then
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true.)
       call msg_fatal ("Soft subtraction: phase space point must be in CMS")
    end if

    if (emitter > sub_soft%n_in) then
       p_soft = create_softvec_fsr (p_born, y_soft, phi, emitter)
    else
       p_soft = create_softvec_isr (y_soft, phi)
    end if
    s_alpha_soft = sub_soft%reg_data%get_svalue_soft &
         (p_born, p_soft, alr, emitter)
    call sub_soft%compute_momentum_matrix (p_born, p_soft)
    sub_soft%value(alr) = 4*pi * alpha_s_born * s_alpha_soft
    kb = zero
    do i = 1, size (p_born)
       do j = 1, size (p_born)
          kb = kb + sub_soft%momentum_matrix (i,j) * &
             born_ij (i,j)
       end do
    end do
    if (debug_active (D_SUBTRACTION)) &
       call msg_debug (D_SUBTRACTION, 'KB', kb)
    sub_soft%value(alr) = sub_soft%value(alr)*kb
    select case (sub_soft%n_in)
    case (1) 
       q2 = p_born(1)%p(0)**2
    case (2)
       q2 = 4 * p_born(1)%p(0) * p_born(2)%p(0)
    end select
    if (emitter <= sub_soft%n_in) then
       sub_soft%value(alr) = 4/q2 * (one-y**2) * sub_soft%value(alr)
    else
       sub_soft%value(alr) = 4/q2 * (one-y) * sub_soft%value(alr)
    end if
  end subroutine soft_subtraction_compute

  subroutine soft_subtraction_compute_momentum_matrix &
       (sub_soft, p_born, p_soft)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    real(default) :: num, deno1, deno2
    integer :: i, j
    do i = 1, sub_soft%nlegs_born
      do j = 1, sub_soft%nlegs_born
        if (i <= j) then
          num = p_born(i) * p_born(j)
          deno1 = p_born(i) * p_soft
          deno2 = p_born(j) * p_soft
          sub_soft%momentum_matrix(i,j) = num / (deno1 * deno2)
        else
           !!! momentum matrix is symmetric.
          sub_soft%momentum_matrix(i,j) = sub_soft%momentum_matrix(j,i)
        end if
      end do
    end do
  end subroutine soft_subtraction_compute_momentum_matrix

  subroutine coll_subtraction_init (coll_sub, n_alr, n_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    integer, intent(in) :: n_alr, n_in
    coll_sub%n_in = n_in
    coll_sub%n_alr = n_alr
    allocate (coll_sub%value (n_alr))
    allocate (coll_sub%value_soft (n_alr))
  end subroutine coll_subtraction_init

  subroutine coll_subtraction_compute_fsr &
       (coll_sub, sregion, p_born, sqme_born, sqme_born_sc, &
        xi, alpha_s, alr, soft_in)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    logical, intent(in), optional :: soft_in
    real(default) :: res
    real(default) :: q0, z, p0
    real(default) :: zoxi, onemz
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
    p0 = p_born(emitter)%p(0)
    select case (coll_sub%n_in)
    case (1)
       q0 = p_born(1)%p(0)
    case (2)
       q0 = p_born(1)%p(0) + p_born(2)%p(0)
    end select
    !!! Here, z corresponds to 1-z in the formulas of arXiv:1002.2581;
    !!! the integrand is symmetric under this variable change
    zoxi = q0 / (two * p0)
    z = xi * zoxi; onemz = one - z 

    if (is_gluon(flv_em) .and. is_gluon(flv_rad)) then
       pggz = two * CA * (z**2 * onemz + z**2 / onemz + onemz)
       res = pggz * sqme_born - 4 * CA * z**2 * onemz * sqme_born_sc
       res = res / zoxi
    else if (is_quark(abs(flv_em)) .and. is_quark (abs(flv_rad))) then
       pqgz = TR * z * (one - two * z * onemz)
       res = pqgz * sqme_born + 4 * TR * z**2 * onemz * sqme_born_sc
       res = res / zoxi
    else if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
       res = sqme_born * CF * (one + onemz**2) / zoxi
    else
       call msg_fatal ('Impossible flavor structure in collinear counterterm!')
    end if
    res = res / (p0**2 * onemz * zoxi)
    res = res * 4*pi * alpha_s

    if (soft) then
      coll_sub%value_soft (alr) = res
    else
      coll_sub%value (alr) = res
    end if
  end subroutine coll_subtraction_compute_fsr

  subroutine coll_subtraction_compute_soft_limit_fsr &
       (coll_sub, sregion, p_born, sqme_born, &
        sqme_born_sc, xi, alpha_s, alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default), intent(in) :: xi, alpha_s
    integer, intent(in) :: alr
    call coll_sub%compute_fsr (sregion, p_born, sqme_born, &
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

    if (is_quark(abs(flv_em)) .and. is_gluon(flv_rad)) then
       res = CF * (one + z**2) * sqme_born
    else if (is_gluon(flv_em) .and. is_quark (abs(flv_rad))) then
       res = TR* (z**2 + onemz**2) * onemz * sqme_born
    end if
    res = res * z/p02
    res = res * 4*pi*alpha_s

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
      nlegs_born, nlegs_real, sqme_collector)
    class(real_subtraction_t), intent(inout), target :: rsub
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: n_in, nlegs_born, nlegs_real
    type(sqme_collector_t), intent(in), target :: sqme_collector
    integer :: alr, i_uborn
    rsub%reg_data = reg_data
    rsub%sqme_real_non_sub => sqme_collector%sqme_real_non_sub
    rsub%sqme_born => sqme_collector%sqme_born_list
    rsub%sqme_born_cc => sqme_collector%sqme_born_cc
    rsub%sqme_born_sc => sqme_collector%sqme_born_sc
    allocate (rsub%sc_required (reg_data%n_regions))
    do alr = 1, reg_data%n_regions
       i_uborn = reg_data%regions(alr)%uborn_index
       rsub%sc_required(alr) = &
          reg_data%flv_born(i_uborn)%count_particle (GLUON) > 0
    end do

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
       call msg_fatal ("Real subtraction: PDFs must be initialized")
    end if
  end subroutine real_subtraction_init_pdfs

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

  function real_subtraction_compute (rsub, emitter, i_flv, alpha_s) result (sqme)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter, i_flv
    real(default) :: alpha_s
    real(default) :: sqme
    integer :: alr

    sqme = zero
    do alr = 1, size (rsub%reg_data%regions)
        if (emitter == rsub%reg_data%regions(alr)%emitter .and. &
            i_flv == rsub%reg_data%regions(alr)%real_index) then
            call rsub%set_alr (alr)
            if (emitter <= rsub%isr_kinematics%n_in) then
               sqme = sqme + rsub%evaluate_region_isr (emitter, alpha_s)
            else
               sqme = sqme + rsub%evaluate_region_fsr (emitter, alpha_s)
            end if
        end if
    end do
    if (rsub%purpose == INTEGRATION .or. rsub%purpose == FIXED_ORDER_EVENTS) &
        sqme = sqme * rsub%get_phs_factor ()
  end function real_subtraction_compute

  function real_subtraction_evaluate_region_fsr (rsub, emitter, &
                                             alpha_s) result (sqme)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    real(default) :: sqme
    integer :: i_real
    real(default) :: sqme0, sqme_soft, sqme_coll, sqme_cs, sqme_remn
    real(default) :: s_alpha
    real(default) :: xi, xi_max, xi_tilde, y, onemy, phi
    real(default) :: s
    sqme0 = zero; sqme_soft = zero; sqme_coll = zero
    sqme_cs = zero; sqme_remn = zero
    xi_tilde = rsub%real_kinematics%xi_tilde
    xi_max = rsub%real_kinematics%xi_max(emitter)
    xi = xi_tilde * xi_max
    y = rsub%real_kinematics%y(emitter)
    onemy = one-y
    phi = rsub%real_kinematics%phi
    associate (region => rsub%reg_data%regions(rsub%current_alr))
      if (rsub%radiation_active) then
         i_real = region%real_index
         sqme0 = rsub%sqme_real_non_sub (i_real)
         s_alpha = rsub%reg_data%get_svalue (rsub%real_kinematics%p_real_cms, rsub%current_alr, emitter)
         sqme0 = sqme0 * s_alpha
         sqme0 = sqme0 * region%mult
         sqme0 = sqme0 * region%double_fsr_factor (rsub%real_kinematics%p_real_cms)
         select case (rsub%purpose)
         case (INTEGRATION, FIXED_ORDER_EVENTS)
            sqme0 = sqme0 * xi**2/xi_tilde * rsub%real_kinematics%jac(emitter)%jac(1)
         case (POWHEG)
            s = rsub%real_kinematics%cms_energy2
            sqme0 = sqme0*rsub%real_kinematics%jac(emitter)%jac(1)*s/(8*twopi3)*xi
         end select
      end if
      if (rsub%subtraction_active) then
         call rsub%evaluate_subtraction_terms_fsr (emitter, alpha_s, &
                   sqme_soft, sqme_coll, sqme_cs)
         sqme_soft = sqme_soft / onemy / xi_tilde
         sqme_coll = sqme_coll / onemy / xi_tilde
         sqme_cs = sqme_cs / onemy / xi_tilde
         associate (jac => rsub%real_kinematics%jac)
            sqme_soft = sqme_soft * jac(emitter)%jac(2)
            sqme_coll = sqme_coll * jac(emitter)%jac(3)
            sqme_cs = sqme_cs * jac(emitter)%jac(2)
         end associate
         sqme_remn = (sqme_soft - sqme_cs) * log(xi_max) * xi_tilde
         select case (rsub%purpose)
         case (INTEGRATION)
            sqme = sqme0 - sqme_soft - sqme_coll + sqme_cs + sqme_remn
         case (FIXED_ORDER_EVENTS)
            sqme = -sqme_soft - sqme_coll + sqme_cs + sqme_remn
         end select
      else
         sqme = sqme0
      end if
      sqme = sqme * rsub%real_kinematics%jac_rand(emitter)
    end associate

    if (debug_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "real_subtraction_evaluate_region_fsr")
       call write_computation_status ()
    end if

  contains
    subroutine write_computation_status (unit)
       integer, intent(in), optional :: unit
       integer :: i_uborn
       integer :: u
       u = given_output_unit (unit); if (u < 0) return
       i_uborn = rsub%reg_data%regions(rsub%current_alr)%uborn_index
       write (u,'(A,I2)') 'rsub%purpose: ', rsub%purpose
       write (u,'(A,I3)') 'alr: ', rsub%current_alr
       write (u,'(A,I3)') 'emitter: ', emitter
       write (u,'(A,F4.2)') 'xi_max: ', xi_max
       write (u,'(A,F4.2,2X,A,F4.2)') 'xi: ', xi, 'y: ', y
       write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_uborn)
       write (u,'(A,ES16.9)')  'sqme_real: ', sqme0
       write (u,'(A,ES16.9)')  'sqme_soft: ', sqme_soft
       write (u,'(A,ES16.9)')  'sqme_coll: ', sqme_coll
       write (u,'(A,ES16.9)')  'sqme_coll-soft: ', sqme_cs
       write (u,'(A,ES16.9)')  'sqme_remn: ', sqme_remn
    end subroutine write_computation_status

  end function real_subtraction_evaluate_region_fsr

  function real_subtraction_evaluate_region_isr (rsub, emitter, alpha_s) result (sqme)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    real(default) :: sqme
    real(default) :: xi_max, xi_max_plus, xi_max_minus
    real(default) :: xi_tilde, xi, xi_plus, xi_minus
    real(default) :: y, phi
    integer :: i_real
    real(default) :: sqme0, sqme_soft, sqme_coll_plus, sqme_coll_minus
    real(default) :: sqme_cs_plus, sqme_cs_minus
    real(default) :: sqme_remn
    real(default) :: s_alpha
    real(default) :: onemy, onepy
    logical :: proc_scatter

    proc_scatter = rsub%isr_kinematics%n_in == 2

    xi_tilde = rsub%real_kinematics%xi_tilde
    xi_max = rsub%real_kinematics%xi_max(1)
    xi = xi_tilde * xi_max
    if (proc_scatter) then 
       xi_max_plus = one - rsub%isr_kinematics%x(I_PLUS)
       xi_max_minus = one - rsub%isr_kinematics%x(I_MINUS)
       xi_plus = xi_max_plus * xi_tilde
       xi_minus = xi_max_minus * xi_tilde
    else 
       xi_max_plus = xi_max
       xi_max_minus = xi_max
       xi_plus =  xi
       xi_minus = xi
    end if
    y = rsub%real_kinematics%y(1)
    onemy = one - y; onepy = one + y
    phi = rsub%real_kinematics%phi

    if (proc_scatter) call rsub%compute_pdfs ()

    associate (region => rsub%reg_data%regions(rsub%current_alr))
      i_real = region%real_index
      sqme0 = rsub%sqme_real_non_sub (i_real)
      s_alpha = rsub%reg_data%get_svalue (rsub%real_kinematics%p_real_cms, rsub%current_alr, emitter)
      sqme0 = sqme0 * s_alpha
      sqme0 = sqme0 * region%mult
      if (proc_scatter) then
         call rsub%reweight_pdfs (sqme0, i_real, I_PLUS)
         call rsub%reweight_pdfs (sqme0, i_real, I_MINUS)
      end if

      select case (rsub%purpose)
      case (INTEGRATION, FIXED_ORDER_EVENTS)
         sqme0 = sqme0 * xi**2/xi_tilde * rsub%real_kinematics%jac(emitter)%jac(1)
      case (POWHEG)
         call msg_fatal ("POWHEG with initial-state radiation not implemented yet")
      end select

      if (rsub%subtraction_active) then
         call rsub%evaluate_subtraction_terms_isr (emitter, alpha_s, &
            sqme_soft, sqme_coll_plus, sqme_coll_minus, sqme_cs_plus, sqme_cs_minus)
         if (proc_scatter) then
            call rsub%reweight_pdfs (sqme_coll_plus, i_real, I_PLUS)
            call rsub%reweight_pdfs (sqme_coll_minus, i_real, I_MINUS)
         end if
         associate (jac => rsub%real_kinematics%jac)
           sqme_soft = sqme_soft / (one - y**2) / xi_tilde * jac(1)%jac(2)
           sqme_coll_plus = sqme_coll_plus / onemy / xi_tilde / two * jac(1)%jac(3)
           sqme_coll_minus = sqme_coll_minus / onepy / xi_tilde / two * jac(1)%jac(4)
           sqme_cs_plus = sqme_cs_plus / onemy / xi_tilde / two * jac(1)%jac(2)
           sqme_cs_minus = sqme_cs_minus / onepy / xi_tilde / two * jac(1)%jac(2)
         end associate
         sqme_remn = log(xi_max) * xi_tilde * sqme_soft
         sqme_remn = sqme_remn - log (xi_max_plus) * xi_tilde * sqme_cs_plus &
                               - log (xi_max_minus) * xi_tilde * sqme_cs_minus

         sqme = sqme0 - sqme_soft - sqme_coll_plus - sqme_coll_minus &
              + sqme_cs_plus + sqme_cs_minus + sqme_remn
      else
         sqme = sqme0
      end if
    end associate
         
    sqme = sqme * rsub%real_kinematics%jac_rand (1)

    if (debug_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "real_subtraction_evaluate_region_isr")
       call write_computation_status ()
    end if

  contains
    subroutine write_computation_status (unit)
       integer, intent(in), optional :: unit
       integer :: i_uborn
       integer :: u
       u = given_output_unit (unit); if (u < 0) return
       i_uborn = rsub%reg_data%regions(rsub%current_alr)%uborn_index
       write (u,'(A,I2)') 'alr: ', rsub%current_alr
       write (u,'(A,I2)') 'emitter: ', emitter
       write (u,'(A,F4.2)') 'xi_max: ', xi_max
       print *, 'xi: ', xi, 'y: ', y
       print *, 'phi: ', phi
       print *, 'xb1: ', rsub%isr_kinematics%x(1), 'xb2: ', rsub%isr_kinematics%x(2)
       write (u,'(A,ES16.9)')  'sqme_born: ', rsub%sqme_born(i_uborn)
       write (u,'(A,ES16.9)')  'sqme_real: ', sqme0
       write (u,'(A,ES16.9)')  'sqme_soft: ', sqme_soft
       write (u,'(A,ES16.9)')  'sqme_coll_plus: ', sqme_coll_plus
       write (u,'(A,ES16.9)')  'sqme_coll_minus: ', sqme_coll_minus
       write (u,'(A,ES16.9)')  'sqme_cs_plus: ', sqme_cs_plus
       write (u,'(A,ES16.9)')  'sqme_cs_minus: ', sqme_cs_minus
       write (u,'(A,ES16.9)')  'sqme_remn: ', sqme_remn
    end subroutine write_computation_status

  end function real_subtraction_evaluate_region_isr

  subroutine real_subtraction_evaluate_subtraction_terms_fsr (rsub, &
                  emitter, alpha_s, &
                  sqme_soft, sqme_coll, sqme_cs)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft, sqme_coll, sqme_cs
    integer :: alr
    real(default) :: xi
    alr = rsub%current_alr
    call rsub%compute_sub_soft (emitter, alpha_s)
    call rsub%compute_sub_coll (emitter, alpha_s)
    call rsub%compute_sub_coll_soft (emitter, alpha_s)
    sqme_soft = rsub%sub_soft%value(alr)
    sqme_coll = rsub%sub_coll%value(alr)
    sqme_cs = rsub%sub_coll%value_soft(alr)
  end subroutine real_subtraction_evaluate_subtraction_terms_fsr

  subroutine real_subtraction_evaluate_subtraction_terms_isr (rsub, &
    emitter, alpha_s, sqme_soft, sqme_coll_plus, sqme_coll_minus, &
    sqme_cs_plus, sqme_cs_minus)

    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    real(default), intent(out) :: sqme_soft
    real(default), intent(out) :: sqme_coll_plus, sqme_coll_minus
    real(default), intent(out) :: sqme_cs_plus, sqme_cs_minus
    integer :: alr
    alr = rsub%current_alr
    call rsub%compute_sub_soft (emitter, alpha_s)
    sqme_soft = rsub%sub_soft%value(alr)
    if (emitter /= 2) then
       call rsub%compute_sub_coll (1, alpha_s)
       call rsub%compute_sub_coll_soft (1, alpha_s)
       sqme_coll_plus = rsub%sub_coll%value(alr)
       sqme_cs_plus = rsub%sub_coll%value_soft(alr)
    else
       sqme_coll_plus = zero
       sqme_cs_plus = zero
    end if
    if (emitter /= 1) then
       call rsub%compute_sub_coll (2, alpha_s)
       call rsub%compute_sub_coll_soft (2, alpha_s)
       sqme_coll_minus = rsub%sub_coll%value(alr)
       sqme_cs_minus = rsub%sub_coll%value_soft(alr)
    else
       sqme_coll_minus = zero
       sqme_cs_minus = zero
    end if
  end subroutine real_subtraction_evaluate_subtraction_terms_isr

  function real_subtraction_get_phs_factor (rsub) result (factor)
    class(real_subtraction_t), intent(in) :: rsub
    real(default) :: factor
    real(default) :: s
    associate (real_kin => rsub%real_kinematics)
       !!! Lorentz invariant, does not matter whether cm or lab frame is used
       select case (rsub%isr_kinematics%n_in)
       case (1) 
          s = real_kin%p_born_cms(1)**2
       case (2) 
          s = (real_kin%p_born_cms(1) + real_kin%p_born_cms(2))**2
       end select
    end associate
    factor = s / (8*twopi3)
  end function real_subtraction_get_phs_factor

  subroutine real_subtraction_compute_sub_soft &
                             (rsub, emitter, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    integer :: alr
    integer :: y_index

    y_index = emitter; if (emitter == 0) y_index = 1
    alr = rsub%current_alr
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_soft_divergence ()) then
          !!! Need to use Born momenta in the CMS, because xi, y, phi are defined there
          call rsub%sub_soft%compute (rsub%real_kinematics%p_born_cms, &
                                      rsub%sqme_born_cc(:,:,sregion%uborn_index), &
                                      rsub%real_kinematics%y(y_index), &
                                      rsub%real_kinematics%y_soft(y_index), &
                                      rsub%real_kinematics%phi, &
                                      alpha_s, alr, emitter)
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
       associate (p => rsub%real_kinematics%p_real_cms)
          call spinor_product (p(em), p(rsub%reg_data%nlegs_real), prod1, prod2)
       end associate
       sqme_sc = real (prod1/prod2*rsub%sqme_born_sc(uborn_index))
    else
       sqme_sc = zero
    end if
  end function real_subtraction_get_sc_matrix_element

  subroutine real_subtraction_compute_sub_coll (rsub, em, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: em
    real(default), intent(in) :: alpha_s
    real(default) :: xi
    real(default) :: sqme_sc
    integer :: alr
    alr = rsub%current_alr
    xi = rsub%real_kinematics%xi_tilde * rsub%real_kinematics%xi_max (em)
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          sqme_sc = rsub%get_sc_matrix_element (alr, em, sregion%uborn_index)
          if (em <= rsub%sub_coll%n_in) then
             call rsub%sub_coll%compute_isr (sregion, rsub%real_kinematics%p_born_lab, &
                rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr, em)
          else
             call rsub%sub_coll%compute_fsr (sregion, rsub%real_kinematics%p_born_cms, &
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
    integer :: alr
    alr = rsub%current_alr
    xi = zero
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          sqme_sc = rsub%get_sc_matrix_element (alr, em, sregion%uborn_index)
          if (em <= rsub%sub_coll%n_in) then
             call rsub%sub_coll%compute_soft_limit_isr (sregion, rsub%real_kinematics%p_born_lab, &
                rsub%sqme_born(sregion%uborn_index), sqme_sc, xi, alpha_s, alr, em)
          else
             call rsub%sub_coll%compute_soft_limit_fsr (sregion, rsub%real_kinematics%p_born_cms, &
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
    real(default) :: z, x, Q
    real(default) :: x_scaled
    real(double), dimension(-6:6) :: f_dble = 0._double
    Q = rsub%isr_kinematics%fac_scale
    do i = 1, 2
       x = rsub%isr_kinematics%x(i)
       z = rsub%isr_kinematics%z(i)
       x_scaled = x / z
       call rsub%pdf_data%evolve (dble(x), dble(Q), f_dble)
       rsub%pdf_born(i)%f = f_dble / dble(x)
       call rsub%pdf_data%evolve (dble(x_scaled), dble(Q), f_dble)
       rsub%pdf_scaled(i)%f = f_dble / dble(x_scaled)
    end do
  end subroutine real_subtraction_compute_pdfs

  subroutine real_subtraction_reweight_pdfs (rsub, sqme, i_real, i_part)
    class(real_subtraction_t), intent(inout) :: rsub
    real(default), intent(inout) :: sqme
    integer, intent(in) :: i_part, i_real
    integer :: flv
    real(default) :: pdfs, pdfb
    flv = rsub%reg_data%flv_real(i_real)%flst(i_part)
    if (flv == GLUON) flv = 0
    pdfb = rsub%pdf_born(i_part)%f(flv)
    pdfs = rsub%pdf_scaled(i_part)%f(flv)
    sqme = sqme*pdfs/pdfb
  end subroutine real_subtraction_reweight_pdfs


end module real_subtraction
