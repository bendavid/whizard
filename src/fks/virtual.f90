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

module virtual

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use numeric_utils
  use constants
  use diagnostics
  use pdg_arrays
  use models
  use physics_defs
  use sm_physics
  use lorentz
  use flavors
  use nlo_data, only: NO_FACTORIZATION, FACTORIZATION_THRESHOLD
  use ttv_formfactors, only: THR_POS_B, THR_POS_BBAR
  use ttv_formfactors, only: THR_POS_WP, THR_POS_WM
  use fks_regions

  implicit none
  private

  public :: virtual_t

  type :: virtual_t
    real(default) :: Q
    real(default), dimension(:,:), allocatable :: I
    real(default) :: vfin
    real(default) :: sqme_cc
    real(default) :: sqme_virt
    real(default), dimension(:,:), allocatable :: gamma_0, gamma_p, c_flv
    real(default) :: ren_scale2, fac_scale, es_scale2
    integer, dimension(:), allocatable :: n_is_neutrinos
    integer :: n_in, n_legs, n_flv
    logical :: bad_point
    logical :: use_internal_color_correlations
    logical :: with_subtraction = .true.
    integer :: factorization_mode = NO_FACTORIZATION
  contains
    procedure :: init => virtual_init
    procedure :: init_constants => virtual_init_constants
    procedure :: set_ren_scale => virtual_set_ren_scale
    procedure :: set_fac_scale => virtual_set_fac_scale
    procedure :: set_ellis_sexton_scale => virtual_set_ellis_sexton_scale
    procedure :: evaluate => virtual_evaluate
    procedure :: compute_eikonals => virtual_compute_eikonals
    procedure :: compute_eikonals_threshold => virtual_compute_eikonals_threshold
    procedure :: set_vfin => virtual_set_vfin
    procedure :: set_bad_point => virtual_set_bad_point
    procedure :: evaluate_initial_state => virtual_evaluate_initial_state
    procedure :: compute_collinear_contribution &
       => virtual_compute_collinear_contribution
    procedure :: compute_massive_self_eikonals => virtual_compute_massive_self_eikonals
    procedure :: compute_eikonal_factor => virtual_compute_eikonal_factor
  end type virtual_t


contains

 subroutine virtual_init (virt, flv_born, n_in)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in), dimension(:,:) :: flv_born
    integer, intent(in) :: n_in
    integer :: i_flv
    virt%n_legs = size (flv_born, 1); virt%n_flv = size (flv_born, 2)
    virt%n_in = n_in
    allocate (virt%I (virt%n_legs, virt%n_legs))
    allocate (virt%gamma_0 (virt%n_legs, virt%n_flv), &
       virt%gamma_p (virt%n_legs, virt%n_flv), &
       virt%c_flv (virt%n_legs, virt%n_flv))
    call virt%init_constants (flv_born)
    allocate (virt%n_is_neutrinos (virt%n_flv))
    virt%n_is_neutrinos = 0
    do i_flv = 1, virt%n_flv
       if (is_neutrino (flv_born(1, i_flv))) &
          virt%n_is_neutrinos(i_flv) = virt%n_is_neutrinos(i_flv) + 1
       if (is_neutrino (flv_born(2, i_flv))) &
          virt%n_is_neutrinos(i_flv) = virt%n_is_neutrinos(i_flv) + 1
    end do
  contains
    function is_neutrino (flv) result (neutrino)
      integer, intent(in) :: flv
      logical :: neutrino
      neutrino = (abs(flv) == 12 .or. abs(flv) == 14 .or. abs(flv) == 16)
    end function is_neutrino
  end subroutine virtual_init

  subroutine virtual_init_constants (virt, flv_born)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in), dimension(:,:) :: flv_born
    integer :: i_part, i_flv
    integer, parameter :: nf = 1
    do i_flv = 1, size (flv_born, 2)
       do i_part = 1, size (flv_born, 1)
          if (is_gluon (flv_born(i_part, i_flv))) then
             virt%gamma_0(i_part, i_flv) = (11 * ca - 2 * nf) / 6
             virt%gamma_p(i_part, i_flv) = (67.0 / 9 - 2 * pi**2 / 3) * ca &
                - 23.0 / 18 * nf
             virt%c_flv(i_part, i_flv) = ca
          else if (is_quark (flv_born(i_part, i_flv))) then
             virt%gamma_0(i_part, i_flv) = 1.5 * cf
             virt%gamma_p(i_part, i_flv) = (6.5 - 2 * pi**2 / 3) * cf
             virt%c_flv(i_part, i_flv) = cf
          else
             virt%gamma_0(i_part, i_flv) = zero
             virt%gamma_p(i_part, i_flv) = zero
             virt%c_flv(i_part, i_flv) = zero
          end if
       end do
    end do
  end subroutine virtual_init_constants

  subroutine virtual_set_ren_scale (virt, p, ren_scale)
    class(virtual_t), intent(inout) :: virt
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale
    if (ren_scale > 0) then
      virt%ren_scale2 = ren_scale**2
    else
      virt%ren_scale2 = (p(1) + p(2))**2
    end if
  end subroutine virtual_set_ren_scale

  subroutine virtual_set_fac_scale (virt, p, fac_scale)
    class(virtual_t), intent(inout) :: virt
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), optional :: fac_scale
    if (present (fac_scale)) then
       virt%fac_scale = fac_scale
    else
       virt%fac_scale = (p(1) + p(2))**1
    end if
  end subroutine virtual_set_fac_scale

  subroutine virtual_set_ellis_sexton_scale (virt, Q2)
    class(virtual_t), intent(inout) :: virt
    real(default), intent(in), optional :: Q2
    if (present (Q2)) then
       virt%es_scale2 = Q2
    else
       virt%es_scale2 = virt%ren_scale2
    end if
  end subroutine virtual_set_ellis_sexton_scale

  subroutine virtual_evaluate &
       (virt, reg_data, i_flv, alpha_s, p_born, born, b_ij)
    class(virtual_t), intent(inout) :: virt
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: i_flv
    real(default), intent(in) :: alpha_s
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in) :: born
    real(default), intent(in), dimension(:,:,:), allocatable :: b_ij
    integer :: alr
    real(default) :: s, BI, s_o_Q2
    virt%sqme_virt = zero
    virt%Q = zero
    if (virt%bad_point) return
    alr = find_first_matching_uborn (reg_data, i_flv)
    if (debug2_active (D_VIRTUAL)) then
       print *, 'Compute virtual component using phase space point: '
       call vector4_write_set (p_born, show_mass = .true., &
          check_conservation = .true.)
       print *, 'Compute virtual component using alpha_s = ', alpha_s
    end if
    associate (flst_born => reg_data%regions(alr)%flst_uborn)
       s = sum (p_born(1 : virt%n_in))**2
       s_o_Q2 = s / virt%es_scale2
       call virt%evaluate_initial_state (sqrt(s), i_flv)
       call virt%compute_collinear_contribution &
          (p_born, sqrt(s), i_flv, flst_born%massive) 

       select case (virt%factorization_mode)
       case (FACTORIZATION_THRESHOLD)
          BI = virt%compute_eikonals_threshold (p_born, s, s_o_Q2, &
               i_flv, alr, reg_data, flst_born, b_ij)
       case default
          BI = virt%compute_eikonals (p_born, s, s_o_Q2, &
               i_flv, alr, reg_data, flst_born, b_ij)
       end select

       if (debug2_active (D_VIRTUAL)) then
         call msg_debug2 (D_VIRTUAL, "Compute Q")
         print *, 'massive flavors: ', flst_born%massive
         print *, 'Q: ', virt%Q
       end if
    end associate
    if (virt%use_internal_color_correlations) BI = BI * born
    virt%sqme_virt = virt%vfin
    !!! A factor of alpha_s/twopi is assumed to be included in vfin
    if (virt%with_subtraction) &
       virt%sqme_virt = virt%sqme_virt + alpha_s / twopi * (virt%Q * born + BI)

    if (debug2_active (D_VIRTUAL)) then
       call msg_debug2 (D_VIRTUAL, "virtual-subtracted matrix element: ")
       print *, 'Q * born: ', virt%Q * born
       print *, 'BI: ', BI
       print *, 'vfin: ', virt%vfin
       print *, 'Result: ', virt%sqme_virt
    end if

    if (virt%n_is_neutrinos(i_flv) > 0) &
        virt%sqme_virt = virt%sqme_virt * virt%n_is_neutrinos(i_flv) * two

  contains

    function find_first_matching_uborn (reg_data, i_proc) result (alr_out)
       type(region_data_t), intent(in) :: reg_data
       integer, intent(in) :: i_proc
       integer :: alr_out
       integer :: k
       alr_out = 0
       do k = 1, reg_data%n_regions
          alr_out = alr_out + 1
          if (reg_data%regions(k)%uborn_index == i_proc) exit
       end do
    end function find_first_matching_uborn

  end subroutine virtual_evaluate

  function virtual_compute_eikonals (virtual, p_born, s, s_o_Q2, i_flv, &
         alr, reg_data, flst_born, b_ij) result (BI)
    real(default) :: BI
    class(virtual_t), intent(inout) :: virtual
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in) :: s, s_o_Q2
    integer, intent(in) :: i_flv, alr
    type(region_data_t), intent(in) :: reg_data
    type(flv_structure_t), intent(in) :: flst_born
    real(default), intent(in), dimension(:,:,:), allocatable :: b_ij
    integer :: i, j
    BI = zero
    call virtual%compute_massive_self_eikonals &
         (p_born, s, i_flv, flst_born%massive)
    do i = 1, virtual%n_legs
       do j = 1, virtual%n_legs
          if (i /= j) then
             if (flst_born%colored(i) .and. flst_born%colored(j)) then
                call virtual%compute_eikonal_factor (p_born, flst_born%massive, i, j, s_o_Q2)
                BI = BI + b_ij (i, j, reg_data%regions(alr)%uborn_index) * virtual%I(i, j)
                if (debug2_active (D_VIRTUAL)) &
                   print *, 'b_ij: ', b_ij (i,j, reg_data%regions(alr)%uborn_index), &
                          'I_ij: ', virtual%I(i,j)
             end if
          end if
       end do
    end do
  end function virtual_compute_eikonals

  function virtual_compute_eikonals_threshold (virtual, p_born, s, &
         s_o_Q2, i_flv, alr, reg_data, flst_born, b_ij) result (BI)
    real(default) :: BI
    class(virtual_t), intent(inout) :: virtual
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in) :: s, s_o_Q2
    integer, intent(in) :: i_flv, alr
    type(region_data_t), intent(in) :: reg_data
    type(flv_structure_t), intent(in) :: flst_born
    real(default), intent(in), dimension(:,:,:), allocatable :: b_ij
    type(vector4_t), dimension(4) :: p
    integer :: i, j
    p(1) = p_born(THR_POS_WP) + p_born(THR_POS_B)
    p(2) = p_born(THR_POS_B)
    p(3) = p_born(THR_POS_WM) + p_born(THR_POS_BBAR)
    p(4) = p_born(THR_POS_BBAR)
    BI = evaluate_leg_pair (1) + evaluate_leg_pair (3)

  contains

    function evaluate_leg_pair (i_start) result (b_ij_times_I)
      real(default) :: b_ij_times_I
      integer, intent(in) :: i_start
      real(default) :: term1, term2
      b_ij_times_I = zero
      do i = i_start, i_start + 1
         do j = i_start, i_start + 1
            if (i /= j) then
               call virtual%compute_eikonal_factor (p, [.true., .true., .true., .true.], i, j, s_o_Q2)
               b_ij_times_I = b_ij_times_I + b_ij (i, j, reg_data%regions(alr)%uborn_index) * virtual%I(i, j)
               if (debug2_active (D_VIRTUAL)) &
                  print *, 'b_ij: ', b_ij (i,j, reg_data%regions(alr)%uborn_index), &
                         'I_ij: ', virtual%I(i,j)
            else
               !!! massive self eikonals
               term1 = log(s_o_Q2)
               term2 = 0.5_default * I_m_eps (p(i))
               virtual%Q = virtual%Q - cf * (term1 - term2)
            end if
         end do
      end do
      if (debug2_active (D_VIRTUAL)) then
         print *, 'b_ij_times_I =    ', b_ij_times_I
         print *, 'virtual%Q =    ', virtual%Q
      end if
    end function evaluate_leg_pair

  end function virtual_compute_eikonals_threshold

  subroutine virtual_set_vfin (virt, vfin)
    class(virtual_t), intent(inout) :: virt
    real(default) :: vfin
    virt%vfin = vfin
  end subroutine virtual_set_vfin

  subroutine virtual_set_bad_point (virt, value)
     class(virtual_t), intent(inout) :: virt
     logical, intent(in) :: value
     virt%bad_point = value
  end subroutine virtual_set_bad_point

  subroutine virtual_evaluate_initial_state (virt, sqrts, i_flv)
    class(virtual_t), intent(inout) :: virt
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i_flv
    integer :: i
    if (virt%n_in == 2) then
       do i = 1, virt%n_in
          virt%Q = virt%Q - virt%gamma_0 (i, i_flv) * &
             log(virt%fac_scale**2 / virt%es_scale2)
       end do
    end if
  end subroutine virtual_evaluate_initial_state

  subroutine virtual_compute_collinear_contribution (virt, p_born, sqrts, i_flv, massive)
    class(virtual_t), intent(inout) :: virt
    type(vector4_t), dimension(:), intent(in) :: p_born
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i_flv
    logical, dimension(:), intent(in) :: massive
    real(default) :: s1, s2, s3, s4
    integer :: i
    real(default) :: twoE

    do i = virt%n_in + 1, virt%n_legs
       !!! Not a colored particle
       if (vanishes (virt%c_flv(i, i_flv))) cycle
       !!! Collinear terms only for massless particles
       if (massive(i)) cycle
       s1 = virt%gamma_p(i, i_flv)
       twoE = two * p_born(i)%p(0)
       s2 = log (sqrts**2 / virt%es_scale2)* &
          (virt%gamma_0(i, i_flv) - &
           two * virt%c_flv(i, i_flv) * log (twoE / sqrts))
       s3 = two * log(twoE / sqrts)**2 * virt%c_flv(i, i_flv)
       s4 = two * log(twoE / sqrts) * virt%gamma_0(i, i_flv)
       virt%Q = virt%Q + s1 - s2 + s3 - s4
    end do
  end subroutine virtual_compute_collinear_contribution

  subroutine virtual_compute_massive_self_eikonals (virt, p_born, s, i_flv, massive)
    class(virtual_t), intent(inout) :: virt
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: s
    integer, intent(in) :: i_flv
    logical, intent(in), dimension(:) :: massive
    real(default) :: term1, term2
    integer :: i
    do i = 1, virt%n_legs
       if (massive(i)) then
          term1 = log(s / virt%es_scale2)
          term2 = 0.5_default * I_m_eps (p_born(i))
          virt%Q = virt%Q - virt%c_flv (i, i_flv) * (term1 - term2)
       end if
    end do
  end subroutine virtual_compute_massive_self_eikonals

  subroutine virtual_compute_eikonal_factor (virt, p_born, massive, i, j, s_o_Q2)
    class(virtual_t), intent(inout) :: virt
    type(vector4_t), intent(in), dimension(:) :: p_born
    logical, dimension(:), intent(in) :: massive
    integer, intent(in) :: i, j
    real(default), intent(in) :: s_o_Q2
    if (massive(i) .and. massive(j)) then
       virt%I(i,j) = compute_Imm (p_born(i), p_born(j), s_o_Q2)
    else if (.not. massive(i) .and. massive(j)) then
       virt%I(i,j) = compute_I0m (p_born(i), p_born(j), s_o_Q2)
    else if (massive(i) .and. .not. massive(j)) then
       virt%I(i,j) = compute_I0m (p_born(j), p_born(i), s_o_Q2)
    else
       virt%I(i,j) = compute_I00 (p_born(i), p_born(j), s_o_Q2)
    end if
  end subroutine virtual_compute_eikonal_factor

  function compute_I00 (pi, pj, s_o_Q2) result (I)
    type(vector4_t), intent(in) :: pi, pj
    real(default), intent(in) :: s_o_Q2
    real(default) :: I
    real(default) :: Ei, Ej
    real(default) :: pij, Eij
    real(default) :: s1, s2, s3, s4, s5
    real(default) :: arglog
    real(default), parameter :: tiny_value = epsilon(1.0)
    s1 = 0; s2 = 0; s3 = 0; s4 = 0; s5 = 0
    Ei = pi%p(0); Ej = pj%p(0)
    pij = pi * pj; Eij = Ei * Ej
    s1 = 0.5 * log(s_o_Q2)**2
    s2 = log(s_o_Q2) * log(pij / (two * Eij))
    s3 = Li2 (pij / (two * Eij))
    s4 = 0.5 * log (pij / (two * Eij))**2
    arglog = one - pij / (2*Eij)
    if (arglog > tiny_value) then
      s5 = log(arglog) * log(pij / (two * Eij))
    else
      s5 = 0
    end if
    I = s1 + s2 - s3 + s4 - s5
  end function compute_I00

  function compute_I0m (ki, kj, s_o_Q2) result (I)
    type(vector4_t), intent(in) :: ki, kj
    real(default), intent(in) :: s_o_Q2
    real(default) :: I
    real(default) :: logsomu
    real(default) :: s1, s2, s3
    s1 = 0; s2 = 0; s3 = 0
    logsomu = log(s_o_Q2)
    s1 = 0.5 * (0.5 * logsomu**2 - pi**2 / 6)
    s2 = 0.5 * I_0m_0 (ki, kj) * logsomu
    s3 = 0.5 * I_0m_eps (ki, kj)
    I = s1 + s2 - s3
  end function compute_I0m

  function compute_Imm (pi, pj, s_o_Q2) result (I)
    type(vector4_t), intent(in) :: pi, pj
    real(default), intent(in) :: s_o_Q2
    real(default) :: I
    real(default) :: s1, s2
    s1 = 0.5 * log(s_o_Q2) * I_mm_0(pi, pj)
    s2 = 0.5 * I_mm_eps(pi, pj)
    I = s1 - s2
  end function compute_Imm

  function I_m_eps (p) result (I)
    type(vector4_t), intent(in) :: p
    real(default) :: I
    real(default) :: beta
    beta = space_part_norm (p)/p%p(0)
    if (beta < tiny_07) then
       I = four * (one + beta**2/3 + beta**4/5 + beta**6/7)
    else
       I = two * log((one + beta) / (one - beta)) / beta
    end if
  end function I_m_eps

  function I_0m_eps (p, k) result (I)
    type(vector4_t), intent(in) :: p, k
    real(default) :: I
    type(vector4_t) :: pp, kp
    real(default) :: beta

    pp = p / p%p(0); kp = k / k%p(0)

    beta = sqrt (one - kp*kp)
    I = -2*(log((one - beta) / (one + beta))**2/4 + log((pp*kp) / (one + beta))*log((pp*kp) / (one - beta)) &
        + Li2(one - (pp*kp) / (one + beta)) + Li2(one - (pp*kp) / (one - beta)))
  end function I_0m_eps

  function I_0m_0 (p, k) result (I)
    type(vector4_t), intent(in) :: p, k
    real(default) :: I
    type(vector4_t) :: pp, kp

    pp = p / p%p(0); kp = k / k%p(0)
    I = log((pp*kp)**2 / kp**2)
  end function I_0m_0

  function I_mm_eps (p1, p2) result (I)
    type(vector4_t), intent(in) :: p1, p2
    real(default) :: I
    type(vector3_t) :: beta1, beta2
    real(default) :: a, b, b2
    real(default) :: zp, zm, z1, z2, x1, x2
    real(default) :: zmb, z1b
    real(default) :: K1, K2

    beta1 = space_part (p1) / energy(p1)
    beta2 = space_part (p2) / energy(p2)
    a = beta1**2 + beta2**2 - 2 * beta1 * beta2
    b = beta1**2 * beta2**2 - (beta1 * beta2)**2
    if (beta1**1 > beta2**1) call switch_beta (beta1, beta2)
    if (beta1 == vector3_null) then
       b2 = beta2**1
       I = (-0.5 * log ((one - b2) / (one + b2))**2 - two * Li2 (-two * b2 / (one - b2))) &
           * one / sqrt (a - b)
       return
    end if
    x1 = beta1**2 - beta1 * beta2
    x2 = beta2**2 - beta1 * beta2
    zp = sqrt (a) + sqrt (a - b)
    zm = sqrt (a) - sqrt (a - b)
    zmb = one  / zp
    z1 = sqrt (x1**2 + b) - x1
    z2 = sqrt (x2**2 + b) + x2
    z1b = one / (sqrt (x1**2 + b) + x1)
    K1 = - 0.5 * log (((z1b - zmb) * (zp - z1)) / ((zp + z1) * (z1b + zmb)))**2 &
          - two * Li2 ((two * zmb * (zp - z1)) / ((zp - zm) * (zmb + z1b))) &
          - two * Li2 ((-two * zp * (zm + z1)) / ((zp - zm) * (zp - z1)))
    K2 = - 0.5 * log ((( z2 - zm) * (zp - z2)) / ((zp + z2) * (z2 + zm)))**2 &
          - two * Li2 ((two * zm * (zp - z2)) / ((zp - zm) * (zm + z2))) &
          - two * Li2 ((-two * zp * (zm + z2)) / ((zp - zm) * (zp - z2)))
    I = (K2 - K1) * (one - beta1 * beta2) / sqrt (a - b)
  contains
    subroutine switch_beta (beta1, beta2)
      type(vector3_t), intent(inout) :: beta1, beta2
      type(vector3_t) :: beta_tmp
      beta_tmp = beta1
      beta1 = beta2
      beta2 = beta_tmp
    end subroutine switch_beta
  end function I_mm_eps

  function I_mm_0 (k1, k2) result (I)
    type(vector4_t), intent(in) :: k1, k2
    real(default) :: I
    real(default) :: beta
    beta = sqrt (one - k1**2 * k2**2 / (k1 * k2)**2)
    I = log ((one + beta) / (one - beta)) / beta
  end function I_mm_0


end module virtual
