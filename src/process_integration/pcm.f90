! WHIZARD 2.4.0 Nov 28 2016
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

module pcm

  use kinds, only: default
  use constants, only: zero, two
  use diagnostics
  use lorentz, only: vector4_t, lorentz_transformation_t, identity
  use process_constants, only: process_constants_t
  use physics_defs
  use nlo_data, only: nlo_settings_t
  use nlo_data, only: powheg_damping_t, powheg_damping_simple_t
  use phs_fks, only: isr_kinematics_t, real_kinematics_t
  use phs_fks, only: phs_identifier_t
  use nlo_color_data, only: nlo_color_data_t
  use fks_regions, only: region_data_t
  use phs_fks, only: phs_fks_generator_t
  use phs_fks, only: get_boost_for_threshold_projection
  use phs_fks, only: threshold_projection_born
  use real_subtraction, only: real_subtraction_t, soft_mismatch_t
  use real_subtraction, only: FIXED_ORDER_EVENTS, POWHEG
  use virtual, only: virtual_t
  use dglap_remnant, only: dglap_remnant_t
  use ttv_formfactors, only: m1s_to_mpole

  use pcm_base

  implicit none
  private

  public :: pcm_default_t
  public :: pcm_nlo_t
  public :: pcm_instance_nlo_t

  type, extends (pcm_t) :: pcm_default_t
   contains
     procedure :: allocate_instance => pcm_default_allocate_instance
     procedure :: final => pcm_default_final
     procedure :: is_nlo => pcm_default_is_nlo
  end type pcm_default_t

  type, extends (pcm_instance_t) :: pcm_instance_default_t
  contains
    procedure :: final => pcm_instance_default_final
    procedure :: is_valid => pcm_instance_default_is_valid
  end type pcm_instance_default_t

  type, extends (pcm_t) :: pcm_nlo_t
    type(nlo_settings_t) :: settings
    type(region_data_t) :: region_data
    type(nlo_color_data_t) :: color_data
    class(powheg_damping_t), allocatable :: powheg_damping
  contains
    procedure :: get_n_sub => pcm_nlo_get_n_sub
    procedure :: get_n_alr => pcm_nlo_get_n_alr
    procedure :: has_massive_emitter => pcm_nlo_has_massive_emitter
    procedure :: get_mass_info => pcm_nlo_get_mass_info
    procedure :: allocate_instance => pcm_nlo_allocate_instance
    procedure :: init_color_data => pcm_nlo_init_color_data
    procedure :: allocate_ps_matching => pcm_nlo_allocate_ps_matching
    procedure :: setup_phs_generator => pcm_nlo_setup_phs_generator
    procedure :: final => pcm_nlo_final
    procedure :: is_nlo => pcm_nlo_is_nlo
  end type pcm_nlo_t

  type, extends (pcm_instance_t) :: pcm_instance_nlo_t
     integer :: active_real_component = 1
     logical :: use_internal_color_correlation = .true.
     type(real_kinematics_t), pointer :: real_kinematics => null ()
     type(isr_kinematics_t), pointer :: isr_kinematics => null ()
     type(real_subtraction_t) :: real_sub
     type(virtual_t) :: virtual
     type(soft_mismatch_t) :: soft_mismatch
     type(dglap_remnant_t) :: dglap_remnant
     logical, dimension(:), allocatable :: bad_blha_point
  contains
    procedure :: disable_subtraction => pcm_instance_nlo_disable_subtraction
    procedure :: disable_sqme_np1 => pcm_instance_nlo_disable_sqme_np1
    procedure :: init_config => pcm_instance_nlo_init_config
    procedure :: setup_real_component => pcm_instance_nlo_setup_real_component
    procedure :: init_real_and_isr_kinematics => &
         pcm_instance_nlo_init_real_and_isr_kinematics
    procedure :: set_real_and_isr_kinematics => &
        pcm_instance_nlo_set_real_and_isr_kinematics
    procedure :: init_real_subtraction => pcm_instance_nlo_init_real_subtraction
    procedure :: set_momenta_and_scales_virtual => &
       pcm_instance_nlo_set_momenta_and_scales_virtual
    procedure :: set_fac_scale => pcm_instance_nlo_set_fac_scale
    procedure :: set_momenta => pcm_instance_nlo_set_momenta
    procedure :: get_momenta => pcm_instance_nlo_get_momenta
    procedure :: get_xi_max => pcm_instance_nlo_get_xi_max
    procedure :: get_n_born => pcm_instance_nlo_get_n_born
    procedure :: get_n_real => pcm_instance_nlo_get_n_real
    procedure :: set_x_rad => pcm_instance_nlo_set_x_rad
    procedure :: init_virtual => pcm_instance_nlo_init_virtual
    procedure :: disable_virtual_subtraction => pcm_instance_nlo_disable_virtual_subtraction
    procedure :: compute_sqme_virt => pcm_instance_nlo_compute_sqme_virt
    procedure :: compute_sqme_mismatch => pcm_instance_nlo_compute_sqme_mismatch
    procedure :: compute_sqme_dglap_remnant => pcm_instance_nlo_compute_sqme_dglap_remnant
    procedure :: set_fixed_order_event_mode => pcm_instance_nlo_set_fixed_order_event_mode
    procedure :: set_powheg_mode => pcm_instance_nlo_set_powheg_mode
    procedure :: init_dglap_remnant => pcm_instance_nlo_init_dglap_remnant
    procedure :: is_fixed_order_nlo_events &
         => pcm_instance_nlo_is_fixed_order_nlo_events
    procedure :: final => pcm_instance_nlo_final
    procedure :: is_valid => pcm_instance_nlo_is_valid
  end type pcm_instance_nlo_t


contains

  subroutine pcm_default_allocate_instance (pcm, instance)
    class(pcm_default_t), intent(in) :: pcm
    class(pcm_instance_t), intent(inout), allocatable :: instance
    allocate (pcm_instance_default_t :: instance)
  end subroutine pcm_default_allocate_instance

  subroutine pcm_default_final (pcm)
    class(pcm_default_t), intent(inout) :: pcm
  end subroutine pcm_default_final

  function pcm_default_is_nlo (pcm) result (is_nlo)
    logical :: is_nlo
    class(pcm_default_t), intent(in) :: pcm
    is_nlo = .false.
  end function pcm_default_is_nlo

  subroutine pcm_instance_default_final (pcm_instance)
    class(pcm_instance_default_t), intent(inout) :: pcm_instance
  end subroutine pcm_instance_default_final

  function pcm_instance_default_is_valid (pcm_instance) result (valid)
    logical :: valid
    class(pcm_instance_default_t), intent(in) :: pcm_instance
    valid = .true.
  end function pcm_instance_default_is_valid

  function pcm_nlo_get_n_sub (pcm_nlo) result (n_sub)
    integer :: n_sub
    class(pcm_nlo_t), intent(in) :: pcm_nlo
    integer :: n_tot
    n_sub = 1
    if (.not. pcm_nlo%settings%use_internal_color_correlations) then
       n_tot = pcm_nlo%region_data%n_legs_born
       n_sub = n_sub + n_tot * (n_tot - 1) / 2
    end if
  end function pcm_nlo_get_n_sub

  function pcm_nlo_get_n_alr (pcm) result (n_alr)
    integer :: n_alr
    class(pcm_nlo_t), intent(in) :: pcm
    n_alr = pcm%region_data%n_regions
  end function pcm_nlo_get_n_alr

  function pcm_nlo_has_massive_emitter (pcm) result (val)
    logical :: val
    class(pcm_nlo_t), intent(in) :: pcm
    integer :: i
    val = .false.
    associate (reg_data => pcm%region_data)
       do i = reg_data%n_in + 1, reg_data%n_legs_born
          if (any (i == reg_data%emitters)) &
             val = val .or. reg_data%flv_born(1)%massive(i)
       end do
    end associate
  end function pcm_nlo_has_massive_emitter

  function pcm_nlo_get_mass_info (pcm, i_flv) result (massive)
    class(pcm_nlo_t), intent(in) :: pcm
    integer, intent(in) :: i_flv
    logical, dimension(:), allocatable :: massive
    allocate (massive (size (pcm%region_data%flv_born(i_flv)%massive)))
    massive = pcm%region_data%flv_born(i_flv)%massive
  end function pcm_nlo_get_mass_info

  subroutine pcm_nlo_allocate_instance (pcm, instance)
    class(pcm_nlo_t), intent(in) :: pcm
    class(pcm_instance_t), intent(inout), allocatable :: instance
    allocate (pcm_instance_nlo_t :: instance)
  end subroutine pcm_nlo_allocate_instance

  subroutine pcm_nlo_init_color_data (pcm, prc_constants, &
     flavor_indices, color_indices)
    class(pcm_nlo_t), intent(inout) :: pcm
    type(process_constants_t), intent(in), dimension(2) :: prc_constants
    integer, intent(in), dimension(:) :: flavor_indices, color_indices
    call pcm%color_data%prepare (pcm%region_data, flavor_indices, color_indices)
    call pcm%color_data%init_color_matrix (pcm%region_data, prc_constants, &
       pcm%settings%factorization_mode)
  end subroutine pcm_nlo_init_color_data

  subroutine pcm_nlo_allocate_ps_matching (pcm)
    class(pcm_nlo_t), intent(inout) :: pcm
    if (.not. allocated (pcm%powheg_damping)) then
       allocate (powheg_damping_simple_t :: pcm%powheg_damping)
    end if
  end subroutine pcm_nlo_allocate_ps_matching

  subroutine pcm_nlo_setup_phs_generator (pcm, pcm_instance, generator, &
     sqrts, mode, singular_jacobian)
    class(pcm_nlo_t), intent(in) :: pcm
    type(phs_fks_generator_t), intent(inout) :: generator
    type(pcm_instance_nlo_t), intent(in), target :: pcm_instance
    real(default), intent(in) :: sqrts
    integer, intent(in), optional:: mode
    logical, intent(in), optional :: singular_jacobian
    logical :: yorn
    yorn = .false.; if (present (singular_jacobian)) yorn = singular_jacobian
    call generator%connect_kinematics (pcm_instance%isr_kinematics, &
       pcm_instance%real_kinematics, pcm%has_massive_emitter ())
    generator%n_in = pcm%region_data%n_in
    call generator%set_sqrts_hat (sqrts)
    call generator%set_emitters (pcm%region_data%emitters)
    call generator%setup_masses (pcm%region_data%n_legs_born)
    generator%is_massive = pcm%get_mass_info (1)
    generator%singular_jacobian = yorn
    if (present (mode)) generator%mode = mode
  end subroutine pcm_nlo_setup_phs_generator

  subroutine pcm_nlo_final (pcm)
    class(pcm_nlo_t), intent(inout) :: pcm
    if (allocated (pcm%powheg_damping)) deallocate (pcm%powheg_damping)
  end subroutine pcm_nlo_final

  function pcm_nlo_is_nlo (pcm) result (is_nlo)
    logical :: is_nlo
    class(pcm_nlo_t), intent(in) :: pcm
    is_nlo = .true.
  end function pcm_nlo_is_nlo

  subroutine pcm_instance_nlo_disable_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%radiation_active = .true.
    pcm_instance%real_sub%subtraction_active = .false.
  end subroutine pcm_instance_nlo_disable_subtraction

  subroutine pcm_instance_nlo_disable_sqme_np1 (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%radiation_active = .false.
    pcm_instance%real_sub%subtraction_active = .true.
  end subroutine pcm_instance_nlo_disable_sqme_np1

  subroutine pcm_instance_nlo_init_config (pcm_instance, active_components, &
     nlo_types, sqrts, i_real_fin)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    logical, intent(in), dimension(:) :: active_components
    integer, intent(in), dimension(:) :: nlo_types
    real(default), intent(in) :: sqrts
    integer, intent(in) :: i_real_fin
    integer :: i_component
    select type (pcm => pcm_instance%config)
    type is (pcm_nlo_t)
       do i_component = 1, size (active_components)
          if (active_components(i_component)) then
             select case (nlo_types(i_component))
             case (NLO_REAL)
                pcm_instance%active_real_component = i_component
                if (i_component /= i_real_fin) then
                   call pcm_instance%setup_real_component &
                      (pcm%settings%fks_template%subtraction_disabled, sqrts)
                end if
             case (NLO_VIRTUAL)
                call pcm_instance%init_virtual ()
                if (.not. pcm%settings%with_virtual_subtraction) &
                   call pcm_instance%disable_virtual_subtraction ()
             case (NLO_DGLAP)
                call pcm_instance%init_dglap_remnant ()
             end select
          end if
       end do
       allocate (pcm_instance%bad_blha_point (size (active_components)))
       pcm_instance%bad_blha_point = .false.
    end select
  end subroutine pcm_instance_nlo_init_config

  subroutine pcm_instance_nlo_setup_real_component (pcm_instance, &
     subtraction_disabled, sqrts)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    logical, intent(in) :: subtraction_disabled
    real(default), intent(in) :: sqrts
    call pcm_instance%init_real_and_isr_kinematics (sqrts)
    call pcm_instance%init_real_subtraction ()
    if (subtraction_disabled) call pcm_instance%disable_subtraction ()
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          call pcm_instance%soft_mismatch%init (region_data, &
             pcm_instance%real_kinematics)
       end associate
    end select
  end subroutine pcm_instance_nlo_setup_real_component

  subroutine pcm_instance_nlo_init_real_and_isr_kinematics (pcm_instance, sqrts)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default) :: sqrts
    integer :: n_contr
    allocate (pcm_instance%real_kinematics)
    allocate (pcm_instance%isr_kinematics)
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          if (allocated (region_data%alr_contributors)) then
             n_contr = size (region_data%alr_contributors)
          else if (config%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
             n_contr = 2
          else
             n_contr = 1
          end if
          call pcm_instance%real_kinematics%init &
               (region_data%n_legs_real, region_data%n_phs, &
               region_data%n_regions, n_contr)
          if (config%settings%factorization_mode == FACTORIZATION_THRESHOLD) &
             call pcm_instance%real_kinematics%init_onshell &
                  (region_data%n_legs_real, region_data%n_phs)
          pcm_instance%isr_kinematics%n_in = region_data%n_in
       end associate
    end select
    pcm_instance%isr_kinematics%beam_energy = sqrts / two
  end subroutine pcm_instance_nlo_init_real_and_isr_kinematics

  subroutine pcm_instance_nlo_set_real_and_isr_kinematics (pcm_instance, phs_identifiers, sqrts)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers
    real(default), intent(in) :: sqrts
    integer :: i_phs, emitter, leg
    logical :: boost_to_cms = .false.
    real(default) :: mtop
    type(lorentz_transformation_t) :: L
    call pcm_instance%real_sub%set_real_kinematics &
         (pcm_instance%real_kinematics)
    call pcm_instance%real_sub%set_isr_kinematics &
         (pcm_instance%isr_kinematics)
  end subroutine pcm_instance_nlo_set_real_and_isr_kinematics

  subroutine pcm_instance_nlo_init_real_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout), target :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          call pcm_instance%real_sub%init (region_data)
          call pcm_instance%real_sub%set_resonance_mappings &
             (config%settings%use_resonance_mappings)
          pcm_instance%real_sub%fixed_alr = config%settings%fixed_alr
          pcm_instance%real_sub%sub_soft%factorization_mode &
             = config%settings%factorization_mode
       end associate
    end select
  end subroutine pcm_instance_nlo_init_real_subtraction

  subroutine pcm_instance_nlo_set_momenta_and_scales_virtual (pcm_instance, p, &
     ren_scale, fac_scale)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: ren_scale, fac_scale
    real(default) :: sqrts, mtop
    type(lorentz_transformation_t) :: L_to_cms
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       if (config%settings%factorization_mode == FACTORIZATION_THRESHOLD) then
          !!! Threshold implementation only for lepton collisions, so nothing to
          !!! worry about here.
          sqrts = two * p(1)%p(0); mtop = m1s_to_mpole (sqrts)
          L_to_cms = get_boost_for_threshold_projection (p, sqrts, mtop)
          associate (p_onshell => pcm_instance%real_kinematics%p_born_onshell%phs_point(1)%p)
             call threshold_projection_born (mtop, L_to_cms, p, p_onshell)
          end associate
       end if
       associate (virtual => pcm_instance%virtual)
          call virtual%set_ren_scale (p, ren_scale)
          call virtual%set_fac_scale (p, fac_scale)
          call virtual%set_ellis_sexton_scale ()
       end associate
    end select
  end subroutine pcm_instance_nlo_set_momenta_and_scales_virtual

  subroutine pcm_instance_nlo_set_fac_scale (pcm_instance, fac_scale)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in) :: fac_scale
    pcm_instance%isr_kinematics%fac_scale = fac_scale
  end subroutine pcm_instance_nlo_set_fac_scale

  subroutine pcm_instance_nlo_set_momenta (pcm_instance, p_born, p_real, i_phs, cms)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), dimension(:), intent(in) :: p_born, p_real
    integer, intent(in) :: i_phs
    logical, intent(in), optional :: cms
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    associate (kinematics => pcm_instance%real_kinematics)
       if (yorn) then
          if (.not. kinematics%p_born_cms%initialized) &
             call kinematics%p_born_cms%init (size (p_born), 1)
          if (.not. kinematics%p_real_cms%initialized) &
             call kinematics%p_real_cms%init (size (p_real), 1)
          kinematics%p_born_cms%phs_point(1)%p = p_born
          kinematics%p_real_cms%phs_point(i_phs)%p = p_real
       else
          if (.not. kinematics%p_born_lab%initialized) &
             call kinematics%p_born_lab%init (size (p_born), 1)
          if (.not. kinematics%p_real_lab%initialized) &
             call kinematics%p_real_lab%init (size (p_real), 1)
          kinematics%p_born_lab%phs_point(1)%p = p_born
          kinematics%p_real_lab%phs_point(i_phs)%p = p_real
       end if
    end associate
  end subroutine pcm_instance_nlo_set_momenta

  function pcm_instance_nlo_get_momenta (pcm_instance, i_phs, born_phsp, cms) result (p)
    type(vector4_t), dimension(:), allocatable :: p
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    integer, intent(in) :: i_phs
    logical, intent(in) :: born_phsp
    logical, intent(in), optional :: cms
    logical :: yorn
    yorn = .false.; if (present (cms)) yorn = cms
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       if (born_phsp) then
          if (yorn) then
             allocate (p (1 : config%region_data%n_legs_born), &
                source = pcm_instance%real_kinematics%p_born_cms%phs_point(1)%p)
          else
             allocate (p (1 : config%region_data%n_legs_born), &
                source = pcm_instance%real_kinematics%p_born_lab%phs_point(1)%p)
          end if
       else
          if (yorn) then
             allocate (p (1 : config%region_data%n_legs_real), &
                source = pcm_instance%real_kinematics%p_real_cms%phs_point(i_phs)%p)
          else
             allocate (p ( 1 : config%region_data%n_legs_real), &
                  source = pcm_instance%real_kinematics%p_real_lab%phs_point(i_phs)%p)
          end if
       end if
    end select
  end function pcm_instance_nlo_get_momenta

  function pcm_instance_nlo_get_xi_max (pcm_instance, alr) result (xi_max)
    real(default) :: xi_max
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    integer, intent(in) :: alr
    integer :: i_phs
    i_phs = pcm_instance%real_kinematics%alr_to_i_phs (alr)
    xi_max = pcm_instance%real_kinematics%xi_max (i_phs)
  end function pcm_instance_nlo_get_xi_max

  function pcm_instance_nlo_get_n_born (pcm_instance) result (n_born)
    integer :: n_born
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       n_born = config%region_data%n_legs_born
    end select
  end function pcm_instance_nlo_get_n_born

  function pcm_instance_nlo_get_n_real (pcm_instance) result (n_real)
    integer :: n_real
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       n_real = config%region_data%n_legs_real
    end select
  end function pcm_instance_nlo_get_n_real

  subroutine pcm_instance_nlo_set_x_rad (pcm_instance, x_tot)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in), dimension(:) :: x_tot
    integer :: n_par
    n_par = size (x_tot)
    if (n_par < 3) then
       pcm_instance%real_kinematics%x_rad = zero
    else
       pcm_instance%real_kinematics%x_rad = x_tot (n_par - 2 : n_par)
    end if
  end subroutine pcm_instance_nlo_set_x_rad

  subroutine pcm_instance_nlo_init_virtual (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       associate (region_data => config%region_data)
          call pcm_instance%virtual%init (region_data%get_flv_states_born (), &
             region_data%n_in, config%settings%use_internal_color_correlations)
          pcm_instance%virtual%factorization_mode = config%settings%factorization_mode
       end associate
    end select
  end subroutine pcm_instance_nlo_init_virtual

  subroutine pcm_instance_nlo_disable_virtual_subtraction (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%virtual%with_subtraction = .false.
  end subroutine pcm_instance_nlo_disable_virtual_subtraction

  function pcm_instance_nlo_compute_sqme_virt (pcm_instance, p, alpha_s, me) result (sqme_virt)
    real(default) :: sqme_virt
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    type(vector4_t), intent(in), dimension(:) :: p
    real(default), intent(in) :: alpha_s
    complex(default), intent(in), dimension(:) :: me
    type(vector4_t), dimension(:), allocatable :: pp
    associate (virtual => pcm_instance%virtual)
       allocate (pp (size (p)))
       if (virtual%factorization_mode == FACTORIZATION_THRESHOLD) then
          pp = pcm_instance%real_kinematics%p_born_onshell%get_momenta (1)
       else
          pp = p
       end if
       select type (config => pcm_instance%config)
       type is (pcm_nlo_t)
          sqme_virt = virtual%evaluate (config%region_data, &
               alpha_s, pp, real(me))
       end select
    end associate
  end function pcm_instance_nlo_compute_sqme_virt

  function pcm_instance_nlo_compute_sqme_mismatch (pcm_instance, alpha_s) result (sqme_mism)
    real(default) :: sqme_mism
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default) :: alpha_s
    sqme_mism = pcm_instance%soft_mismatch%evaluate (alpha_s)
  end function pcm_instance_nlo_compute_sqme_mismatch

  function pcm_instance_nlo_compute_sqme_dglap_remnant (pcm_instance, alpha_s, sqme_born) &
     result (sqme_dglap)
    real(default) :: sqme_dglap
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    real(default), intent(in) :: alpha_s, sqme_born
    sqme_dglap = pcm_instance%dglap_remnant%evaluate (alpha_s, sqme_born)
  end function pcm_instance_nlo_compute_sqme_dglap_remnant

  subroutine pcm_instance_nlo_set_fixed_order_event_mode (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%purpose = FIXED_ORDER_EVENTS
  end subroutine pcm_instance_nlo_set_fixed_order_event_mode

  subroutine pcm_instance_nlo_set_powheg_mode (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    pcm_instance%real_sub%purpose = POWHEG
  end subroutine pcm_instance_nlo_set_powheg_mode

  subroutine pcm_instance_nlo_init_dglap_remnant (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    select type (config => pcm_instance%config)
    type is (pcm_nlo_t)
       call pcm_instance%dglap_remnant%init (pcm_instance%isr_kinematics, &
          config%region_data%get_flv_states_born (), config%get_n_alr ())
    end select
  end subroutine pcm_instance_nlo_init_dglap_remnant

  function pcm_instance_nlo_is_fixed_order_nlo_events (pcm_instance) result (is_nlo)
    logical :: is_nlo
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    is_nlo = pcm_instance%real_sub%purpose == FIXED_ORDER_EVENTS
  end function pcm_instance_nlo_is_fixed_order_nlo_events

  subroutine pcm_instance_nlo_final (pcm_instance)
    class(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    call pcm_instance%real_sub%final ()
    call pcm_instance%virtual%final ()
    call pcm_instance%soft_mismatch%final ()
    call pcm_instance%dglap_remnant%final ()
    if (associated (pcm_instance%real_kinematics)) then
       call pcm_instance%real_kinematics%final ()
       nullify (pcm_instance%real_kinematics)
    end if
    if (associated (pcm_instance%isr_kinematics)) then
       nullify (pcm_instance%isr_kinematics)
    end if
  end subroutine pcm_instance_nlo_final

  function pcm_instance_nlo_is_valid (pcm_instance) result (valid)
    logical :: valid
    class(pcm_instance_nlo_t), intent(in) :: pcm_instance
    valid = .not. any (pcm_instance%bad_blha_point)
  end function pcm_instance_nlo_is_valid


end module pcm
