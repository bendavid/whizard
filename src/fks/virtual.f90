! WHIZARD 2.6.0 Sep 08 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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
  use model_data, only: model_data_t
  use physics_defs
  use sm_physics
  use lorentz
  use flavors
  use blha_olp_interfaces, only: blha_loop_positions
  use nlo_data, only: get_threshold_momenta
  use nlo_data, only: ASSOCIATED_LEG_PAIR
  use fks_regions

  implicit none
  private

  public :: virtual_t



  type :: virtual_t
    real(default), dimension(:,:), allocatable :: gamma_0, gamma_p, c_flv
    real(default) :: ren_scale2, fac_scale, es_scale2
    integer, dimension(:), allocatable :: n_is_neutrinos
    integer :: n_in, n_legs, n_flv
    logical :: bad_point = .false.
    type(string_t) :: selection
    integer :: factorization_mode = NO_FACTORIZATION
    real(default), dimension(:,:,:), allocatable :: sqme_color_c
    real(default), dimension(:,:,:), allocatable :: sqme_charge_c
    logical :: use_internal_color_c = .false.
    integer, dimension(:), allocatable :: me_index
    logical :: collinear_resonance_aware = .true.
    logical :: has_pdfs = .false.
  contains
    procedure :: init => virtual_init
    procedure :: init_constants => virtual_init_constants
    procedure :: set_ren_scale => virtual_set_ren_scale
    procedure :: set_fac_scale => virtual_set_fac_scale
    procedure :: set_ellis_sexton_scale => virtual_set_ellis_sexton_scale
    procedure :: compute_n_sub => virtual_compute_n_sub
    procedure :: evaluate => virtual_evaluate
    procedure :: get_i_virt => virtual_get_i_virt
    procedure :: get_i_born_from_i_virt => virtual_get_i_born_from_i_virt
    procedure :: compute_eikonals => virtual_compute_eikonals
    procedure :: compute_eikonals_threshold => virtual_compute_eikonals_threshold
    procedure :: set_bad_point => virtual_set_bad_point
    procedure :: evaluate_initial_state => virtual_evaluate_initial_state
    procedure :: compute_collinear_contribution &
       => virtual_compute_collinear_contribution
    procedure :: compute_massive_self_eikonals => virtual_compute_massive_self_eikonals
    procedure :: final => virtual_final
  end type virtual_t


contains

 subroutine virtual_init (virt, flv_born, n_in, n_f, &
        use_internal_color_c, selection, resonance_aware, &
        nlo_corr_type, model)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in), dimension(:,:) :: flv_born
    integer, intent(in) :: n_in, n_f
    logical, intent(in) :: use_internal_color_c
    type(string_t), intent(in) :: selection, nlo_corr_type
    logical, intent(in) :: resonance_aware
    class(model_data_t), intent(in) :: model
    integer :: i_flv
    virt%n_legs = size (flv_born, 1); virt%n_flv = size (flv_born, 2)
    virt%n_in = n_in
    allocate (virt%sqme_color_c (virt%n_legs, virt%n_legs, virt%n_flv))
    allocate (virt%sqme_charge_c (virt%n_legs, virt%n_legs, virt%n_flv))
    allocate (virt%gamma_0 (virt%n_legs, virt%n_flv), &
       virt%gamma_p (virt%n_legs, virt%n_flv), &
       virt%c_flv (virt%n_legs, virt%n_flv))
    call virt%init_constants (flv_born, n_f, nlo_corr_type, model)
    allocate (virt%n_is_neutrinos (virt%n_flv))
    virt%n_is_neutrinos = 0
    do i_flv = 1, virt%n_flv
       if (is_neutrino (flv_born(1, i_flv))) &
          virt%n_is_neutrinos(i_flv) = virt%n_is_neutrinos(i_flv) + 1
       if (is_neutrino (flv_born(2, i_flv))) &
          virt%n_is_neutrinos(i_flv) = virt%n_is_neutrinos(i_flv) + 1
    end do
    virt%use_internal_color_c = use_internal_color_c
    allocate (virt%me_index (virt%n_flv))
    select case (char (selection))
    case ("Full", "OLP", "Subtraction")
       virt%selection = selection
    case default
       call msg_fatal ('Virtual selection: Possible values are "Full", "OLP" or "Subtraction')
    end select
    virt%collinear_resonance_aware = resonance_aware
  contains

    function is_neutrino (flv) result (neutrino)
      integer, intent(in) :: flv
      logical :: neutrino
      neutrino = (abs(flv) == 12 .or. abs(flv) == 14 .or. abs(flv) == 16)
    end function is_neutrino

  end subroutine virtual_init

  subroutine virtual_init_constants (virt, flv_born, nf_input, nlo_corr_type, model)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in), dimension(:,:) :: flv_born
    integer, intent(in) :: nf_input
    type(string_t), intent(in) :: nlo_corr_type
    class(model_data_t), intent(in) :: model
    integer :: i_part, i_flv, nf
    real(default) :: CA_factor
    real(default), dimension(:,:), allocatable :: CF_factor, TR_factor
    type(flavor_t) :: flv
    allocate (CF_factor (size (flv_born, 1), size (flv_born, 2)), &
         TR_factor (size (flv_born, 1), size (flv_born, 2)))
    if (nlo_corr_type == "QCD") then
       CA_factor = CA; CF_factor = CF; TR_factor = TR
       nf = nf_input
    else if (nlo_corr_type == "QED") then
       CA_factor = zero
       do i_flv = 1, size (flv_born, 2)
          do i_part = 1, size (flv_born, 1)
             call flv%init (flv_born(i_part, i_flv), model)
             CF_factor(i_part, i_flv) = (flv%get_charge ())**2
             TR_factor(i_part, i_flv) = (flv%get_charge ())**2
          end do
       end do
       nf = 4 !!! for testing only, needs dynamical treatment!
    end if
    do i_flv = 1, size (flv_born, 2)
       do i_part = 1, size (flv_born, 1)
          if (is_corresponding_vector (flv_born(i_part, i_flv), nlo_corr_type)) then
             virt%gamma_0(i_part, i_flv) = 11 / 6 * CA_factor - 2 / 3 &
                  * TR_factor(i_part, i_flv) * nf
             virt%gamma_p(i_part, i_flv) = (67.0 / 9 - 2 * pi**2 / 3) * CA_factor &
                - 23.0 / 9 * TR_factor(i_part, i_flv) * nf
             virt%c_flv(i_part, i_flv) = CA_factor
          else if (is_corresponding_fermion (flv_born(i_part, i_flv), nlo_corr_type)) then
             virt%gamma_0(i_part, i_flv) = 1.5 * CF_factor(i_part, i_flv)
             virt%gamma_p(i_part, i_flv) = (6.5 - 2 * pi**2 / 3) * CF_factor(i_part, i_flv)
             virt%c_flv(i_part, i_flv) = CF_factor(i_part, i_flv)
          else
             virt%gamma_0(i_part, i_flv) = zero
             virt%gamma_p(i_part, i_flv) = zero
             virt%c_flv(i_part, i_flv) = zero
          end if
       end do
    end do
  contains
    function is_corresponding_vector (pdg_nr, nlo_corr_type)
      logical :: is_corresponding_vector
      integer, intent(in) :: pdg_nr
      type(string_t), intent(in) :: nlo_corr_type
      is_corresponding_vector = .false.
      if (nlo_corr_type == "QCD") then
         is_corresponding_vector = is_gluon (pdg_nr)
      else if (nlo_corr_type == "QED") then
         is_corresponding_vector = is_photon (pdg_nr)
      end if
    end function is_corresponding_vector
    function is_corresponding_fermion (pdg_nr, nlo_corr_type)
      logical :: is_corresponding_fermion
      integer, intent(in) :: pdg_nr
      type(string_t), intent(in) :: nlo_corr_type
      is_corresponding_fermion = .false.
      if (nlo_corr_type == "QCD") then
         is_corresponding_fermion = is_quark (pdg_nr)
      else if (nlo_corr_type == "QED") then
         is_corresponding_fermion = is_fermion (pdg_nr)
      end if
    end function is_corresponding_fermion
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

  function virtual_compute_n_sub (virt) result (n_sub)
    integer :: n_sub
    class(virtual_t), intent(in) :: virt
    n_sub = 1
    if (.not. virt%use_internal_color_c) &
         n_sub = n_sub + virt%n_legs * (virt%n_legs - 1 ) / 2
  end function virtual_compute_n_sub

  subroutine virtual_evaluate (virt, reg_data, alpha_coupling, &
         p_born, sqme, separate_alrs, sqme_virt)
    class(virtual_t), intent(inout) :: virt
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in) :: alpha_coupling
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in), dimension(:) :: sqme
    logical, intent(in) :: separate_alrs
    real(default), dimension(:), intent(inout) :: sqme_virt
    real(default) :: s, s_o_Q2
    real(default), dimension(reg_data%n_flv_born) :: QB, BI
    integer :: i_flv, ii_flv, i_virt
    QB = zero; BI = zero
    if (virt%bad_point) return
    if (debug2_active (D_VIRTUAL)) then
       print *, 'Compute virtual component using alpha = ', alpha_coupling
       print *, 'Virtual selection: ', char (virt%selection)
       print *, 'virt%es_scale2 =    ', virt%es_scale2 !!! Debugging
    end if
    s = sum (p_born(1 : virt%n_in))**2
    if (virt%factorization_mode == FACTORIZATION_THRESHOLD) &
         call set_s_for_threshold ()
    s_o_Q2 = s / virt%es_scale2
    do i_flv = 1, reg_data%n_flv_born

       if (separate_alrs) then
          ii_flv = i_flv
       else
          ii_flv = 1
       end if

       i_virt = virt%get_i_virt (i_flv)
       if (virt%selection == var_str ("Full") .or. virt%selection == var_str ("OLP")) then
          !!! A factor of alpha_coupling/twopi is assumed to be included in vfin
          sqme_virt(ii_flv) = sqme_virt(ii_flv) + sqme(i_virt)
       end if

       if (virt%selection == var_str ("Full") .or. virt%selection == var_str ("Subtraction")) then
          call virt%evaluate_initial_state (i_flv, i_virt, sqrt(s), reg_data, sqme, QB)
          call virt%compute_collinear_contribution (i_flv, i_virt, p_born, &
               sqrt(s), reg_data, sqme, QB)

          select case (virt%factorization_mode)
          case (FACTORIZATION_THRESHOLD)
             call virt%compute_eikonals_threshold (i_flv, p_born, s, s_o_Q2, reg_data, sqme, QB, BI)
          case default
             call virt%compute_massive_self_eikonals (i_flv, i_virt, p_born, s, reg_data, sqme, QB)
             call virt%compute_eikonals (i_flv, i_virt, p_born, s, s_o_Q2, reg_data, sqme, BI)
          end select

          if (debug2_active (D_VIRTUAL)) then
             print *, 'Evaluate i_flv: ', i_flv
             print *, 'sqme_born: ', sqme (i_virt + 1)
             print *, 'Q * sqme_born: ', QB(i_flv)
             print *, 'BI: ', BI(i_flv)
             print *, 'vfin: ', sqme (i_virt)
          end if
          sqme_virt(ii_flv) = &
               sqme_virt(ii_flv) + alpha_coupling / twopi * (QB(i_flv) + BI(i_flv))
       end if
    end do

    if (debug2_active (D_VIRTUAL)) then
       call msg_debug2 (D_VIRTUAL, "virtual-subtracted matrix element(s): ")
       print *, sqme_virt
    end if

    do i_flv = 1, reg_data%n_flv_born
       if (virt%n_is_neutrinos(i_flv) > 0) &
            sqme_virt = sqme_virt * virt%n_is_neutrinos(i_flv) * two
    end do
  contains
    subroutine set_s_for_threshold ()
      use ttv_formfactors, only: m1s_to_mpole
      real(default) :: mtop2
      mtop2 = m1s_to_mpole (sqrt(s))**2
      if (s < four * mtop2) s = four * mtop2
    end subroutine set_s_for_threshold

  end subroutine virtual_evaluate

  function virtual_get_i_virt (virt, i_flv) result (i_virt)
    integer :: i_virt
    class(virtual_t), intent(in) :: virt
    integer, intent(in) :: i_flv
    if (virt%has_pdfs) then
       i_virt = 1 + (virt%me_index(i_flv) - 1) * (virt%compute_n_sub () + 1)
    else
       i_virt = i_flv
    end if
  end function virtual_get_i_virt

  function virtual_get_i_born_from_i_virt (virt, i_virt) result (i_born)
    integer :: i_born
    class(virtual_t), intent(in) :: virt
    integer, intent(in) :: i_virt
    if (virt%has_pdfs) then
       i_born = i_virt + 1
    else
       i_born = i_virt + virt%n_flv
    end if
  end function virtual_get_i_born_from_i_virt

  subroutine virtual_compute_eikonals (virtual, i_flv, i_virt, &
           p_born, s, s_o_Q2, reg_data, sqme, BI)
    class(virtual_t), intent(inout) :: virtual
    integer, intent(in) :: i_flv, i_virt
    type(vector4_t), intent(in), dimension(:)  :: p_born
    real(default), intent(in) :: s, s_o_Q2
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in), dimension(:) :: sqme
    real(default), intent(inout), dimension(:) :: BI
    integer :: i_born
    integer :: i, j
    real(default) :: I_ij, BI_tmp
    BI_tmp = zero
    i_born = virtual%get_i_born_from_i_virt (i_virt)
    associate (flst_born => reg_data%flv_born(i_flv), &
           nlo_corr_type => reg_data%regions(1)%nlo_correction_type)
       do i = 1, virtual%n_legs
          do j = 1, virtual%n_legs
             if (i /= j) then
                if (nlo_corr_type == "QCD") then
                   if (flst_born%colored(i) .and. flst_born%colored(j)) then
                      I_ij = compute_eikonal_factor (p_born, flst_born%massive, &
                           i, j, s_o_Q2)
                      BI_tmp = BI_tmp + virtual%sqme_color_c (i, j, i_flv) * I_ij
                      if (debug2_active (D_VIRTUAL)) &
                           print *, 'b_ij: ', i, j, virtual%sqme_color_c (i, j, i_flv), 'I_ij: ', I_ij
                   end if
                else if (nlo_corr_type == "QED") then
                   I_ij = compute_eikonal_factor (p_born, flst_born%massive, &
                        i, j, s_o_Q2)
                   BI_tmp = BI_tmp + virtual%sqme_charge_c (i, j, i_flv) * I_ij
                   if (debug2_active (D_VIRTUAL)) &
                        print *, 'b_ij: ', virtual%sqme_charge_c (i, j, i_flv), 'I_ij: ', I_ij
                end if
             end if
          end do
       end do
       if (virtual%use_internal_color_c .or. nlo_corr_type == "QED") &
            BI_tmp = BI_tmp * sqme (i_born)
    end associate
    BI(i_flv) = BI(i_flv) + BI_tmp
  end subroutine virtual_compute_eikonals

  subroutine virtual_compute_eikonals_threshold (virtual, i_flv, &
         p_born, s, s_o_Q2, reg_data, sqme, QB, BI)
    class(virtual_t), intent(in) :: virtual
    integer, intent(in) :: i_flv
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: s, s_o_Q2
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in), dimension(:) :: sqme
    real(default), intent(inout), dimension(:) :: QB
    real(default), intent(inout), dimension(:) :: BI
    type(vector4_t), dimension(4) :: p_thr
    integer :: i_born, leg
    BI = zero; p_thr = get_threshold_momenta (p_born)
    i_born = virtual%get_i_born_from_i_virt (virtual%get_i_virt (i_flv))
    call compute_massive_self_eikonals (sqme (i_born), QB(i_flv))
    do leg = 1, 2
       BI(i_flv) = BI(i_flv) + evaluate_leg_pair (ASSOCIATED_LEG_PAIR(leg), i_flv, i_born)
    end do
  contains

    subroutine compute_massive_self_eikonals (sqme_born, QB)
      real(default), intent(in) :: sqme_born
      real(default), intent(inout) :: QB
      real(default) :: term_1, term_2
      integer :: i
      call msg_debug2 (D_VIRTUAL, "compute_massive_self_eikonals")
      call msg_debug2 (D_VIRTUAL, "s_o_Q2", s_o_Q2)
      call msg_debug2 (D_VIRTUAL, "log (s_o_Q2)", log (s_o_Q2))
      do i = 1, 4
         term_1 = log (s_o_Q2)
         term_2 = 0.5_default * I_m_eps (p_thr(i))
         QB = QB - (cf * (term_1 - term_2)) * sqme_born
      end do
    end subroutine compute_massive_self_eikonals

    function evaluate_leg_pair (i_start, i_flv, i_born) result (b_ij_times_I)
      real(default) :: b_ij_times_I
      integer, intent(in) :: i_start, i_flv, i_born
      real(default) :: I_ij
      integer :: i, j
      b_ij_times_I = zero
      do i = i_start, i_start + 1
         do j = i_start, i_start + 1
            if (i /= j) then
               I_ij = compute_eikonal_factor &
                    (p_thr, [.true., .true., .true., .true.], i, j, s_o_Q2)
               b_ij_times_I = b_ij_times_I + &
                    virtual%sqme_color_c (i, j, i_flv) * I_ij
               if (debug2_active (D_VIRTUAL)) &
                  print *, 'b_ij: ', virtual%sqme_color_c (i, j, i_flv), 'I_ij: ', I_ij
            end if
         end do
      end do
      if (virtual%use_internal_color_c) b_ij_times_I = b_ij_times_I * sqme (i_born)
      if (debug2_active (D_VIRTUAL)) then
         print *, 'internal color: ', virtual%use_internal_color_c
         print *, 'b_ij_times_I =    ', b_ij_times_I
         print *, 'QB           =    ', QB
      end if
    end function evaluate_leg_pair
  end subroutine virtual_compute_eikonals_threshold

  subroutine virtual_set_bad_point (virt, value)
     class(virtual_t), intent(inout) :: virt
     logical, intent(in) :: value
     virt%bad_point = value
  end subroutine virtual_set_bad_point

  subroutine virtual_evaluate_initial_state (virt, i_flv, i_virt, sqrts, reg_data, sqme, QB)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in) :: i_flv, i_virt
    real(default), intent(in) :: sqrts
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in), dimension(:) :: sqme
    real(default), intent(inout), dimension(:) :: QB
    integer :: i, i_sqme
    if (virt%n_in == 2) then
       i_sqme = virt%get_i_born_from_i_virt (i_virt)
       do i = 1, virt%n_in
          QB(i_flv) = QB(i_flv) - virt%gamma_0 (i, i_flv) * &
               log(virt%fac_scale**2 / virt%es_scale2) * sqme (i_sqme)
       end do
    end if
  end subroutine virtual_evaluate_initial_state

  subroutine virtual_compute_collinear_contribution (virt, i_flv, i_virt, &
           p_born, sqrts, reg_data, sqme, QB)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in) :: i_flv, i_virt
    type(vector4_t), dimension(:), intent(in) :: p_born
    real(default), intent(in) :: sqrts
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in), dimension(:) :: sqme
    real(default), intent(inout), dimension(:) :: QB
    real(default) :: s1, s2, s3, s4, s5
    integer :: alr, em, i_sqme
    real(default) :: E_em, log_xi_max, E_tot2
    logical :: massive
    logical, dimension(virt%n_flv, virt%n_legs) :: evaluated
    integer :: i_contr
    type(vector4_t) :: k_res
    type(lorentz_transformation_t) :: L_to_resonance
    evaluated = .false.
    do alr = 1, reg_data%n_regions
       if (i_flv /= reg_data%regions(alr)%uborn_index) cycle
       em = reg_data%regions(alr)%emitter
       if (em == 0) cycle
       if (evaluated(i_flv, em)) cycle
       massive = reg_data%regions(alr)%flst_uborn%massive(em)
       !!! Collinear terms only for massless particles
       if (massive) cycle
       E_em = p_born(em)%p(0)
       i_sqme = virt%get_i_born_from_i_virt (i_virt)
       if (allocated (reg_data%alr_contributors)) then
          i_contr = reg_data%alr_to_i_contributor (alr)
          k_res = get_resonance_momentum (p_born, reg_data%alr_contributors(i_contr)%c)
          E_tot2 = k_res%p(0)**2
          L_to_resonance = inverse (boost (k_res, k_res**1))
          log_xi_max = log (two * space_part_norm (L_to_resonance * p_born(em)) / k_res%p(0))
       else
          E_tot2 = sqrts**2
          log_xi_max = log (two * E_em / sqrts)
       end if
       if (virt%collinear_resonance_aware) then
          if (debug_active (D_VIRTUAL)) &
              call msg_debug (D_VIRTUAL, "Using resonance-aware collinear subtraction")
          s1 = virt%gamma_p(em, i_flv)
          s2 = two * (log (sqrts / (two * E_em)) + log_xi_max) * &
               (log (sqrts / (two * E_em)) + log_xi_max + log (virt%es_scale2 / sqrts**2)) &
             * virt%c_flv(em, i_flv)
          s3 = two * log_xi_max * &
               (log_xi_max - log (virt%es_scale2 / E_tot2)) * virt%c_flv(em, i_flv)
          s4 = (log (virt%es_scale2 / E_tot2) - two * log_xi_max) * virt%gamma_0(em, i_flv)
          QB(i_flv) = QB(i_flv) + (s1 + s2 + s3 + s4) * sqme(i_sqme)
       else
          if (debug_active (D_VIRTUAL)) &
              call msg_debug (D_VIRTUAL, "Using old-fashioned collinear subtraction")
          s1 = virt%gamma_p(em, i_flv)
          s2 = log (sqrts**2 / virt%es_scale2) * virt%gamma_0(em,i_flv)
          s3 = log (sqrts**2 / virt%es_scale2) * two * virt%c_flv(em,i_flv) * &
               log (two * E_em / sqrts)
          s4 = two * virt%c_flv(em,i_flv) * log (two * E_em / sqrts)**2
          s5 = two * virt%gamma_0(em,i_flv) * log (two * E_em / sqrts)
          QB(i_flv) = QB(i_flv) + (s1 - s2 + s3 + s4 - s5) * sqme(i_sqme)
       end if
       evaluated(i_flv, em) = .true.
    end do
  end subroutine virtual_compute_collinear_contribution

  subroutine virtual_compute_massive_self_eikonals (virt, i_flv, i_virt, &
           p_born, s, reg_data, sqme, QB)
    class(virtual_t), intent(inout) :: virt
    integer, intent(in) :: i_flv, i_virt
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: s
    type(region_data_t), intent(in) :: reg_data
    real(default), intent(in), dimension(:) :: sqme
    real(default), intent(inout), dimension(:) :: QB
    real(default) :: term1, term2
    integer :: i, i_born
    logical :: massive
    i_born = virt%get_i_born_from_i_virt (i_virt)
    do i = 1, virt%n_legs
       massive = reg_data%flv_born(i_flv)%massive(i)
       if (massive) then
          term1 = log(s / virt%es_scale2)
          term2 = 0.5_default * I_m_eps (p_born(i))
          QB(i_flv) = QB(i_flv) - (virt%c_flv (i, i_flv) * (term1 - term2)) * sqme (i_born)
       end if
    end do
  end subroutine virtual_compute_massive_self_eikonals

  function compute_eikonal_factor (p_born, massive, i, j, s_o_Q2) result (I_ij)
    real(default) :: I_ij
    type(vector4_t), intent(in), dimension(:) :: p_born
    logical, dimension(:), intent(in) :: massive
    integer, intent(in) :: i, j
    real(default), intent(in) :: s_o_Q2
    if (massive(i) .and. massive(j)) then
       I_ij = compute_Imm (p_born(i), p_born(j), s_o_Q2)
    else if (.not. massive(i) .and. massive(j)) then
       I_ij = compute_I0m (p_born(i), p_born(j), s_o_Q2)
    else if (massive(i) .and. .not. massive(j)) then
       I_ij = compute_I0m (p_born(j), p_born(i), s_o_Q2)
    else
       I_ij = compute_I00 (p_born(i), p_born(j), s_o_Q2)
    end if
  end function compute_eikonal_factor

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

  subroutine virtual_final (virtual)
    class(virtual_t), intent(inout) :: virtual
    if (allocated (virtual%gamma_0)) deallocate (virtual%gamma_0)
    if (allocated (virtual%gamma_p)) deallocate (virtual%gamma_p)
    if (allocated (virtual%c_flv)) deallocate (virtual%c_flv)
    if (allocated (virtual%n_is_neutrinos)) deallocate (virtual%n_is_neutrinos)
  end subroutine virtual_final


end module virtual
