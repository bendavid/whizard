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

module virtual

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use pdg_arrays
  use model_data
  use physics_defs
  use sm_physics
  use lorentz
  use flavors
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
    real(default) :: ren_scale2, fac_scale
    integer, dimension(:), allocatable :: n_is_neutrinos
    integer :: n_in, nlegs, nflv
    logical :: bad_point
    logical :: use_internal_color_correlations
  contains
    procedure :: init => virtual_init
    procedure :: init_constants => virtual_init_constants
    procedure :: set_ren_scale => virtual_set_ren_scale
    procedure :: set_fac_scale => virtual_set_fac_scale
    procedure :: evaluate => virtual_evaluate
    procedure :: compute_vfin_test => virtual_compute_vfin_test
    procedure :: set_vfin => virtual_set_vfin
    procedure :: set_bad_point => virtual_set_bad_point
    procedure :: compute_Q => virtual_compute_Q
    procedure :: compute_I => virtual_compute_I
  end type virtual_t


contains

 subroutine virtual_init (object, flv_born, n_in)
    class(virtual_t), intent(inout) :: object
    integer, intent(in), dimension(:,:) :: flv_born
    integer, intent(in) :: n_in
    integer :: i_flv
    object%nlegs = size (flv_born, 1); object%nflv = size (flv_born, 2)
    object%n_in = n_in
    allocate (object%I (object%nlegs, object%nlegs))
    allocate (object%gamma_0 (object%nlegs, object%nflv), &
              object%gamma_p (object%nlegs, object%nflv), &
              object%c_flv (object%nlegs, object%nflv))
    call object%init_constants (flv_born)
    allocate (object%n_is_neutrinos (object%nflv))
    object%n_is_neutrinos = 0
    do i_flv = 1, object%nflv
       if (is_neutrino (flv_born(1, i_flv))) &
          object%n_is_neutrinos(i_flv) = object%n_is_neutrinos(i_flv) + 1
       if (is_neutrino (flv_born(2, i_flv))) &
          object%n_is_neutrinos(i_flv) = object%n_is_neutrinos(i_flv) + 1
    end do
  contains
    function is_neutrino (flv) result (neutrino)
      integer, intent(in) :: flv
      logical :: neutrino
      neutrino = (abs(flv)==12 .or. abs(flv)==14 .or. abs(flv)==16)
    end function is_neutrino
  end subroutine virtual_init

  subroutine virtual_init_constants (object, flv_born)
    class(virtual_t), intent(inout) :: object
    integer, intent(in), dimension(:,:) :: flv_born
    integer :: i_part, i_flv
    integer, parameter :: nf = 1
    do i_flv = 1, size (flv_born, 2)
       do i_part = 1, size (flv_born, 1)
          if (is_gluon (flv_born(i_part, i_flv))) then
             object%gamma_0(i_part, i_flv) = (11 * ca - 2 * nf) / 6
             object%gamma_p(i_part, i_flv) = (67.0 / 9 - 2 * pi**2 / 3) * ca - 23.0 / 18 * nf
             object%c_flv(i_part, i_flv) = ca
          else if (is_quark (abs(flv_born(i_part, i_flv)))) then
             object%gamma_0(i_part, i_flv) = 1.5 * cf
             object%gamma_p(i_part, i_flv) = (6.5 - 2 * pi**2 / 3) * cf
             object%c_flv(i_part, i_flv) = cf
          else
             object%gamma_0(i_part, i_flv) = zero
             object%gamma_p(i_part, i_flv) = zero
             object%c_flv(i_part, i_flv) = zero
          end if
       end do
    end do
  end subroutine virtual_init_constants

  subroutine virtual_set_ren_scale (object, p, ren_scale)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), intent(in) :: ren_scale
    if (ren_scale > 0) then
      object%ren_scale2 = ren_scale**2
    else
      object%ren_scale2 = (p(1)+p(2))**2
    end if
  end subroutine virtual_set_ren_scale

  subroutine virtual_set_fac_scale (object, p, fac_scale)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), optional :: fac_scale
    if (present (fac_scale)) then
       object%fac_scale = fac_scale
    else
       object%fac_scale = (p(1)+p(2))**1
    end if
  end subroutine virtual_set_fac_scale

  subroutine virtual_evaluate &
       (object, reg_data, i_flv, alpha_s, p_born, born, b_ij)
    class(virtual_t), intent(inout) :: object
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: i_flv
    real(default), intent(in) :: alpha_s
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in) :: born
    real(default), intent(in), dimension(:,:,:), allocatable :: b_ij
    integer :: i, j, alr
    real(default) :: BI
    object%sqme_virt = 0._default
    if (object%bad_point) return
    BI = 0
    alr = find_first_matching_uborn (reg_data, i_flv)

    if (debug_active (D_VIRTUAL)) &
       print *, 'Compute virtual component using alpha_s = ', alpha_s

    associate (flst_born => reg_data%regions(alr)%flst_uborn)
       call object%compute_Q (p_born, i_flv, flst_born%massive)

       if (debug_active (D_VIRTUAL)) then
         call msg_debug (D_VIRTUAL, "Compute Q")
         print *, 'massive flavors: ', flst_born%massive
         print *, 'Q: ', object%Q
      end if

      do i = 1, object%nlegs
        do j = 1, object%nlegs
          if (i /= j) then
             if (flst_born%colored(i) .and. flst_born%colored(j)) then
               call object%compute_I (p_born, flst_born%massive, i, j)
               BI = BI + b_ij (i,j,reg_data%regions(alr)%uborn_index) * &
                               object%I(i,j)

               if (debug_active (D_VIRTUAL)) &
                   print *, 'b_ij: ', b_ij (i,j, reg_data%regions(alr)%uborn_index), &
                            'I_ij: ', object%I(i,j)
             end if
          end if
        end do
      end do
    end associate
    if (object%use_internal_color_correlations) BI = BI*born
    !!! A factor of alpha_s/twopi is assumed to be included in vfin
    object%sqme_virt = alpha_s/twopi * (object%Q*born + BI) + object%vfin

    if (debug_active (D_VIRTUAL)) then
       call msg_debug (D_VIRTUAL, "virtual-subtracted matrix element: ")
       print *, 'Q*born: ', object%Q*born
       print *, 'BI: ', BI
       print *, 'vfin: ', object%vfin
       print *, 'Result: ', object%sqme_virt
    end if

    if (object%n_is_neutrinos(i_flv) > 0) &
        object%sqme_virt = object%sqme_virt * object%n_is_neutrinos(i_flv) * two
  contains
    function find_first_matching_uborn (reg_data, i_proc) result (alr_out)
       type(region_data_t), intent(in) :: reg_data
       integer, intent(in) :: i_proc
       integer :: alr_out
       integer :: k
       alr_out = 0
       do k = 1, reg_data%n_regions
          alr_out = alr_out+1
          if (reg_data%regions(k)%uborn_index == i_proc) exit
       end do
    end function find_first_matching_uborn
  end subroutine virtual_evaluate

  subroutine virtual_compute_vfin_test (object, p_born, sqme_born)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: sqme_born
    real(default) :: s
    s = (p_born(1)+p_born(2))**2
    !!! ----NOTE: Test implementation for e+ e- -> u ubar
    object%vfin = sqme_born * cf * &
         (pi**2 - 8 + 3*log(s/object%ren_scale2) - log(s/object%ren_scale2)**2)
    object%bad_point = .false.
  end subroutine virtual_compute_vfin_test

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

  subroutine virtual_compute_Q (object, p_born, i_flv, massive)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), dimension(:), intent(in) :: p_born
    integer, intent(in) :: i_flv
    logical, dimension(:), intent(in) :: massive
    real(default) :: sqrts, E
    real(default) :: s1, s2, s3, s4
    integer :: i
    real(default) :: twoE
    sqrts = sum (p_born(1:object%n_in))**1
    object%Q = 0

    if (object%n_in == 2) then
       do i = 1, object%n_in
          object%Q = object%Q - object%gamma_0(i, i_flv) * &
            two * log(object%fac_scale / sqrts)
       end do
    end if

    do i = 1, object%nlegs
       !!! Not a colored particle
       if (object%c_flv(i, i_flv) == 0) cycle
       if (.not. massive (i)) then
          s1 = object%gamma_p(i, i_flv)
          E = p_born(i)%p(0); twoE = two * E
          s2 = log (sqrts**2 / object%ren_scale2)* &
               (object%gamma_0(i, i_flv) - &
                two * object%c_flv(i, i_flv) * log (twoE / sqrts))
          s3 = two * log(twoE / sqrts)**2 * object%c_flv(i, i_flv)
          s4 = two * log(twoE / sqrts) * object%gamma_0(i, i_flv)
          object%Q = object%Q + s1 - s2 + s3 - s4
       else
          s1 = log(sqrts**2 / object%ren_scale2)
          s2 = 0.5 * I_m_eps (p_born(i))
          object%Q = object%Q - object%c_flv(i, i_flv) * (s1 - s2)
       end if
    end do
  end subroutine virtual_compute_Q

  subroutine virtual_compute_I (object, p_born, massive, i, j)
    class(virtual_t), intent(inout) :: object
    type(vector4_t), intent(in), dimension(:) :: p_born
    logical, dimension(:), intent(in) :: massive
    integer, intent(in) :: i, j
    real(default) :: somu2
    somu2  = sum (p_born(1:object%n_in))**2 / object%ren_scale2
    if (massive(i) .and. massive(j)) then
       object%I(i,j) = compute_Imm (p_born(i), p_born(j), somu2)
    else if (.not.massive(i) .and. massive(j)) then
       object%I(i,j) = compute_I0m (p_born(i), p_born(j), somu2)
    else if (massive(i) .and. .not.massive(j)) then
       object%I(i,j) = compute_I0m (p_born(j), p_born(i), somu2)
    else
       object%I(i,j) = compute_I00 (p_born(i), p_born(j), somu2)
    end if
  end subroutine virtual_compute_I

  function compute_I00 (pi, pj, somu2) result (I)
    type(vector4_t), intent(in) :: pi, pj
    real(default), intent(in) :: somu2
    real(default) :: I
    real(default) :: Ei, Ej
    real(default) :: pij, Eij
    real(default) :: s1, s2, s3, s4, s5
    real(default) :: arglog
    real(default), parameter :: tiny_value = epsilon(1.0)

    s1 = 0; s2 = 0; s3 = 0; s4 = 0; s5 = 0
    Ei = pi%p(0); Ej = pj%p(0)
    pij = pi * pj; Eij = Ei * Ej
    s1 = 0.5 * log(somu2)**2
    s2 = log(somu2) * log(pij / (two * Eij))
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

  function compute_I0m (ki, kj, somu2) result (I)
    type(vector4_t), intent(in) :: ki, kj
    real(default), intent(in) :: somu2
    real(default) :: I
    real(default) :: logsomu
    real(default) :: s1, s2, s3
    s1 = 0; s2 = 0; s3 = 0
    logsomu = log(somu2)
    s1 = 0.5 * (0.5 * logsomu**2 - pi**2 / 6)
    s2 = 0.5 * I_0m_0 (ki, kj) * logsomu
    s3 = 0.5 * I_0m_eps (ki, kj)
    I = s1 + s2 - s3
  end function compute_I0m

  function compute_Imm (pi, pj, somu2) result (I)
    type(vector4_t), intent(in) :: pi, pj
    real(default), intent(in) :: somu2
    real(default) :: I
    real(default) :: s1, s2
    s1 = 0.5 * log(somu2) * I_mm_0(pi, pj)
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

    beta1 = space_part (p1)/energy(p1)
    beta2 = space_part (p2)/energy(p2)
    a = beta1**2 + beta2**2 - 2*beta1*beta2
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
          - two *Li2 ((-two * zp * (zm + z1)) / ((zp - zm) * (zp - z1)))
    K2 = - 0.5 * log ((( z2 - zm) * (zp - z2)) / ((zp + z2) * (z2 + zm)))**2 &
          - two *Li2 ((two * zm * (zp - z2)) / ((zp - zm) * (zm + z2))) &
          -two *Li2 ((-two * zp * (zm + z2)) / ((zp - zm) * (zp - z2)))
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
