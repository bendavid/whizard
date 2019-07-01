! WHIZARD 2.2.6 May 02 2015
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

module real_subtraction

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units, only: given_output_unit
  use constants
  use unit_tests
  use diagnostics
  use pdg_arrays
  use model_data
  use physics_defs
  use sm_physics
  use lorentz
  use flavors
  use fks_regions
  use nlo_data

  implicit none
  private

  public :: real_subtraction_t

  type :: soft_subtraction_t
    real(default), dimension(:), allocatable :: value
    type(region_data_t) :: reg_data
    integer :: nlegs_born, nlegs_real
    real(default), dimension(:,:), allocatable :: momentum_matrix
    logical :: use_internal_color_correlations = .true.
    logical :: use_internal_spin_correlations = .false.
  contains
    procedure :: init => soft_subtraction_init
    procedure :: compute => soft_subtraction_compute
    procedure :: compute_momentum_matrix => &
         soft_subtraction_compute_momentum_matrix
  end type soft_subtraction_t
  
  type :: coll_subtraction_t
    real(default), dimension(:), allocatable :: value
    real(default), dimension(:), allocatable :: value_soft
    integer :: n_alr
    real(default), dimension(0:3,0:3) :: b_munu
    real(default) , dimension(0:3,0:3) :: k_perp_matrix
  contains
    procedure :: init => coll_subtraction_init
    procedure :: compute => coll_subtraction_compute
    procedure :: compute_soft_limit => coll_subtraction_compute_soft_limit
  end type coll_subtraction_t
  
  type :: real_subtraction_t
    type(region_data_t) :: reg_data
    type(vector4_t), dimension(:), allocatable :: p_born, p_real
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
    logical :: sqme_np1_active = .true.
    logical :: subtraction_active = .true.
  contains
    procedure :: init => real_subtraction_init
    procedure :: set_momenta => real_subtraction_set_momenta
    procedure :: set_real_kinematics => real_subtraction_set_real_kinematics
    procedure :: set_isr_kinematics => real_subtraction_set_isr_kinematics
    procedure :: set_alr => real_subtraction_set_alr
    procedure :: compute => real_subtraction_compute
    procedure :: evaluate_region_fsr => real_subtraction_evaluate_region_fsr
    procedure :: evaluate_region_isr => real_subtraction_evaluate_region_isr
    procedure :: evaluate_subtraction_terms => &
                         real_subtraction_evaluate_subtraction_terms
    procedure :: get_phs_factor => real_subtraction_get_phs_factor
    procedure :: compute_sub_soft => real_subtraction_compute_sub_soft
    procedure :: compute_sub_coll => real_subtraction_compute_sub_coll 
    procedure :: compute_sub_coll_soft => real_subtraction_compute_sub_coll_soft
  end type real_subtraction_t


contains
 
  subroutine soft_subtraction_init (sub_soft, reg_data, nlegs_born, &
                                    nlegs_real)
    class(soft_subtraction_t), intent(inout) :: sub_soft
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: nlegs_born, nlegs_real
    sub_soft%reg_data = reg_data
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
    p_soft%p(0) = 1._default
    p_soft%p(1:3) = p_born(emitter)%p(1:3) / space_part_norm (p_born(emitter))
    dir = create_orthogonal (space_part (p_born(emitter)))
    rot = rotation (y, sqrt(1-y**2), dir)
    p_soft = rot*p_soft
    if (.not. vanishes (phi)) then
      dir = space_part (p_born(emitter)) / &
            space_part_norm (p_born(emitter))
      rot = rotation (cos(phi), sin(phi), dir)
      p_soft = rot*p_soft
    end if
  end function create_softvec_fsr
  
  function create_softvec_isr (y, phi) result (p_soft)
    real(default), intent(in) :: y, phi
    type(vector4_t) :: p_soft
    real(default) :: sin_theta
    sin_theta = sqrt(1-y**2)
    p_soft%p(0) = 1._default
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
    real(default) :: q02
    real(default) :: kb
    integer :: i, j

    if (emitter > 2) then
       p_soft = create_softvec_fsr (p_born, y_soft, phi, emitter)
    else
       p_soft = create_softvec_isr (y_soft, phi)
    end if
    s_alpha_soft = sub_soft%reg_data%get_svalue_soft &
         (p_born, p_soft, alr, emitter)
    call sub_soft%compute_momentum_matrix (p_born, p_soft)
    sub_soft%value(alr) = 4*pi*alpha_s_born * s_alpha_soft
    kb = 0._default
    do i = 1, size (p_born)
       do j = 1, size (p_born)
          kb = kb + sub_soft%momentum_matrix (i,j) * &
               born_ij (i,j)
       end do 
    end do
    sub_soft%value(alr) = sub_soft%value(alr)*kb
    q02 = 4* vector4_get_component (p_born(1), 0) * &
         vector4_get_component (p_born(2), 0)
    sub_soft%value(alr) = 4/q02 * (1-y) * sub_soft%value(alr) 
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
  
  subroutine coll_subtraction_init (coll_sub, n_alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    integer, intent(in) :: n_alr
    coll_sub%n_alr = n_alr
    allocate (coll_sub%value (n_alr))
    allocate (coll_sub%value_soft (n_alr))
  end subroutine coll_subtraction_init

  subroutine coll_subtraction_compute &
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
    q0 = vector4_get_component (p_born(1), 0) + &
         vector4_get_component (p_born(2), 0)
    !!! Here, z corresponds to 1-z in the formulas of arXiv:1002.2581; 
    !!! the integrand is symmetric under this variable change
    zoxi = q0/(2*p0)
    z = xi*zoxi; onemz = 1-z

    if (sregion%emitter <= 2) then
      !!! Initial-state: No spin-correlations up to now
      if (is_quark(abs (flv_em)) .and. is_gluon(flv_rad)) then
        res = sqme_born*CF*(z+z**3)/onemz/zoxi 
      else if (is_gluon(flv_em) .and. is_quark(abs (flv_rad))) then
        res = sqme_born*TR*(z**2+(1-z)**2)*z/zoxi
      end if
    else
      if (is_gluon(flv_em) .and. is_gluon(flv_rad)) then
         pggz = 2*CA*(z**2*onemz + z**2/onemz + onemz)
         res = pggz*sqme_born - 4*CA*z**2*onemz*sqme_born_sc
         res = res/zoxi
      else if (is_quark(abs(flv_em)) .and. is_quark (abs(flv_rad))) then
         pqgz = TR*z*(1-2*z*onemz)
         res = pqgz*sqme_born + 4*TR*z**2*onemz*sqme_born_sc
         res = res/zoxi
      else if (is_quark (abs(flv_em)) .and. is_gluon (flv_rad)) then
         res = sqme_born*CF*(1+onemz**2)/zoxi
      else
         call msg_fatal ('Impossible flavor structure in collinear counterterm!') 
      end if
    end if
    res = res /(p0**2*onemz*zoxi)
    res = res * 4*pi*alpha_s 

    if (soft) then
      coll_sub%value_soft (alr) = res
    else
      coll_sub%value (alr) = res
    end if
  end subroutine coll_subtraction_compute
  
  subroutine coll_subtraction_compute_soft_limit &
       (coll_sub, sregion, p_born, sqme_born, &
        sqme_born_sc, xi, alpha_s, alr)
    class(coll_subtraction_t), intent(inout) :: coll_sub
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default), intent(in) :: sqme_born_sc
    real(default) :: xi, alpha_s
    integer, intent(in) :: alr
    call coll_sub%compute (sregion, p_born, sqme_born, &
                           sqme_born_sc, xi, alpha_s, alr, .true.)
  end subroutine coll_subtraction_compute_soft_limit
  
  subroutine real_subtraction_init (rsub, reg_data, nlegs_born, &
                                    nlegs_real, sqme_collector)
    class(real_subtraction_t), intent(inout) :: rsub
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: nlegs_born, nlegs_real
    type(sqme_collector_t), intent(in), target :: sqme_collector
    integer :: alr, i_uborn
    rsub%reg_data = reg_data
    allocate (rsub%p_born (nlegs_born), rsub%p_real (nlegs_real))
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
     
    call rsub%sub_soft%init (reg_data, nlegs_born, nlegs_real)
    call rsub%sub_coll%init (reg_data%n_regions)
  end subroutine real_subtraction_init

  subroutine real_subtraction_set_momenta (rsub, p_born, p_real)
    class(real_subtraction_t), intent(inout) :: rsub
    type(vector4_t), intent(in), dimension(:) :: p_born, p_real
    rsub%p_born = p_born; rsub%p_real = p_real
  end subroutine real_subtraction_set_momenta

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

  function real_subtraction_compute (rsub, emitter, alpha_s) result (sqme)
    class(real_subtraction_t), intent(inout) :: rsub 
    integer, intent(in) :: emitter
    real(default) :: alpha_s
    real(default) :: sqme
    integer :: alr

    sqme = 0._default
    !!! Loop over all singular regions
    do alr = 1, size (rsub%reg_data%regions)
         if (emitter == rsub%reg_data%regions(alr)%emitter) then
            call rsub%set_alr (alr)
            if (emitter <= 2) then
               sqme = sqme + rsub%evaluate_region_isr (emitter, alpha_s)
            else
               sqme = sqme + rsub%evaluate_region_fsr (emitter, alpha_s) 
            end if
         end if
    end do
    if (rsub%subtraction_active) sqme = sqme*rsub%get_phs_factor ()
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
    real(default) :: xi, xi_max, xi_tilde, y, phi
    real(default) :: s
    xi_tilde = rsub%real_kinematics%xi_tilde
    xi_max = rsub%real_kinematics%xi_max(emitter)
    xi = xi_tilde * xi_max
    y = rsub%real_kinematics%y(emitter)
    phi = rsub%real_kinematics%phi
    associate (region => rsub%reg_data%regions(rsub%current_alr))
      i_real = region%real_index
      sqme0 = rsub%sqme_real_non_sub (i_real)
      s_alpha = rsub%reg_data%get_svalue (rsub%p_real, rsub%current_alr, emitter)
      sqme0 = sqme0 * s_alpha
      sqme0 = sqme0 * region%mult
      sqme0 = sqme0 * region%double_fsr_factor (rsub%p_real)
      if (rsub%subtraction_active) then
         sqme0 = sqme0 * xi**2/xi_tilde * rsub%real_kinematics%jac(emitter)%jac(1)
      else 
         s = rsub%real_kinematics%cms_energy2
         sqme0 = sqme0*rsub%real_kinematics%jac(emitter)%jac(1)*s/(8*twopi3)*xi
      end if
      if (rsub%subtraction_active) then 
         call rsub%evaluate_subtraction_terms (emitter, alpha_s, &
                   sqme_soft, sqme_coll, sqme_cs)
         associate (jac => rsub%real_kinematics%jac)
            sqme_soft = sqme_soft/(1-y)/xi_tilde * jac(emitter)%jac(2)
            sqme_coll = sqme_coll/(1-y)/xi_tilde * jac(emitter)%jac(3)
            sqme_cs = sqme_cs/(1-y)/xi_tilde * jac(emitter)%jac(2)
         end associate
         sqme_remn = (sqme_soft - sqme_cs)*log(xi_max)*xi_tilde
         sqme = sqme0 - sqme_soft - sqme_coll + sqme_cs + sqme_remn
         sqme = sqme * rsub%real_kinematics%jac_rand (emitter)
      else
         sqme = sqme0
      end if
    end associate
    !!! This occurs if the result is NaN.
    if (sqme /= sqme) & 
       call write_computation_status ()
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
       write (u,'(A,F4.2,A,F4.2)') 'xi: ', xi, 'y: ', y
       print *,  'sqme_born: ', rsub%sqme_born(i_uborn)
       print *,  'sqme_real: ', sqme0
       print *,  'sqme_soft: ', sqme_soft
       print *,  'sqme_coll: ', sqme_coll
       print *,  'sqme_coll-soft: ', sqme_cs
       print *,  'sqme_remn: ', sqme_remn
    end subroutine write_computation_status

  end function real_subtraction_evaluate_region_fsr

  function real_subtraction_evaluate_region_isr (rsub, emitter, alpha_s) result (sqme)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    real(default) :: sqme
  end function real_subtraction_evaluate_region_isr

  subroutine real_subtraction_evaluate_subtraction_terms (rsub, &
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
  end subroutine real_subtraction_evaluate_subtraction_terms

  function real_subtraction_get_phs_factor (rsub) result (factor)
    class(real_subtraction_t), intent(in) :: rsub
    real(default) :: factor
    real(default) :: s
    s = (rsub%p_born(1)+rsub%p_born(2))**2
    factor = s / (8*twopi3)
  end function real_subtraction_get_phs_factor

  subroutine real_subtraction_compute_sub_soft &
                             (rsub, emitter, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: emitter
    real(default), intent(in) :: alpha_s
    integer :: alr
    alr = rsub%current_alr
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_soft_divergence ()) then
             call rsub%sub_soft%compute (rsub%p_born, &
                                    rsub%sqme_born_cc(:,:,sregion%uborn_index), &
                                    rsub%real_kinematics%y(emitter), &
                                    rsub%real_kinematics%y_soft(emitter), &
                                    rsub%real_kinematics%phi, &
                                    alpha_s, alr, emitter)
       else
          rsub%sub_soft%value(alr) = 0._default
       end if
    end associate
  end subroutine real_subtraction_compute_sub_soft

  subroutine real_subtraction_compute_sub_coll (rsub, em, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: em
    real(default), intent(in) :: alpha_s
    real(default) :: xi
    real(default) :: sqme_sc
    complex(default) :: prod1, prod2
    integer :: alr
    alr = rsub%current_alr
    xi = rsub%real_kinematics%xi_tilde * rsub%real_kinematics%xi_max (em)
    associate (sregion => rsub%reg_data%regions(alr)) 
       if (sregion%has_collinear_divergence ()) then
          if (rsub%sc_required(alr)) then
             call spinor_product (rsub%p_real(em), rsub%p_real(rsub%reg_data%nlegs_real), &
                                  prod1, prod2)
             sqme_sc = real (prod1/prod2*rsub%sqme_born_sc(sregion%uborn_index))
          else
             sqme_sc = 0._default
          end if
          call rsub%sub_coll%compute (sregion, rsub%p_born, &
                                      rsub%sqme_born(sregion%uborn_index), &
                                      sqme_sc, xi, alpha_s, alr)
       else
          rsub%sub_coll%value(alr) = 0._default
       end if
    end associate
  end subroutine real_subtraction_compute_sub_coll

  subroutine real_subtraction_compute_sub_coll_soft (rsub, em, alpha_s)
    class(real_subtraction_t), intent(inout) :: rsub
    integer, intent(in) :: em
    real(default), intent(in) :: alpha_s
    complex(default) :: prod1, prod2
    real(default) :: sqme_sc
    real(default), parameter :: xi = 0
    integer :: alr
    alr = rsub%current_alr
    associate (sregion => rsub%reg_data%regions(alr))
       if (sregion%has_collinear_divergence ()) then
          if (rsub%sc_required(alr)) then
             call spinor_product (rsub%p_real(em), rsub%p_real(rsub%reg_data%nlegs_real), &
                                  prod1, prod2)
             sqme_sc = real (prod1/prod2*rsub%sqme_born_sc(sregion%uborn_index))
          else
             sqme_sc = 0._default
          end if
          call rsub%sub_coll%compute_soft_limit (sregion, rsub%p_born, &
                             rsub%sqme_born(sregion%uborn_index), &
                             sqme_sc, xi, alpha_s, alr)
       else
          rsub%sub_coll%value_soft(alr) = 0._default
       end if
    end associate
  end subroutine real_subtraction_compute_sub_coll_soft


end module real_subtraction
