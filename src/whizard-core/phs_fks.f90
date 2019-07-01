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

module phs_fks
  
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use lorentz
!  use models
  use model_data
  use flavors
  use sf_mappings
  use sf_base
  use phs_base
  use phs_wood
  use process_constants

  implicit none
  private

  public :: phs_fks_config_t
  public :: kinematics_counter_t
  public :: phs_fks_t
  public :: fks_born_to_real_fsr
  public :: fks_get_xi_max_fsr
  public :: fks_born_to_real_isr

  type, extends (phs_wood_config_t) :: phs_fks_config_t
  contains
    procedure :: final => phs_fks_config_final
    procedure :: write => phs_fks_config_write
    procedure :: configure => phs_fks_config_configure
    procedure :: startup_message => phs_fks_config_startup_message
    procedure, nopass :: allocate_instance => phs_fks_config_allocate_instance
    procedure :: set_born_config => phs_fks_config_set_born_config
  end type phs_fks_config_t

  type :: kinematics_counter_t
     integer :: n_bins = 0
     integer, dimension(:), allocatable :: histo_xi
     integer, dimension(:), allocatable :: histo_xi_tilde
     integer, dimension(:), allocatable :: histo_xi_max 
     integer, dimension(:), allocatable :: histo_y
     integer, dimension(:), allocatable :: histo_phi
  contains
  procedure :: init => kinematics_counter_init
  procedure :: record => kinematics_counter_record
  procedure :: display => kinematics_counter_display
  end type kinematics_counter_t

  type, extends (phs_wood_t) :: phs_fks_t
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: q_born
    type(vector4_t), dimension(:), allocatable :: p_real
    type(vector4_t), dimension(:), allocatable :: q_real
    real(default), dimension(3) :: r_real
    real(default) :: xi_tilde, y, phi
    real(default), dimension(:), allocatable :: xi_max
    real(default), dimension(3) :: jac
    real(default) :: jac_rand
    integer, dimension(:), allocatable :: emitters
    type(kinematics_counter_t) :: counter
  contains
  procedure :: init => phs_fks_init
  procedure :: final => phs_fks_final
  procedure :: init_momenta => phs_fks_init_momenta
  procedure :: evaluate_selected_channel => phs_fks_evaluate_selected_channel
  procedure :: evaluate_other_channels => phs_fks_evaluate_other_channels
  procedure :: get_mcpar => phs_fks_get_mcpar
  procedure :: get_real_kinematics => phs_fks_get_real_kinematics
  procedure :: set_emitters => phs_fks_set_emitters
  procedure :: get_born_momenta => phs_fks_get_born_momenta
  procedure :: get_outgoing_momenta => phs_fks_get_outgoing_momenta
  procedure :: get_incoming_momenta => phs_fks_get_incoming_momenta
!!!  procedure :: get_ch_to_em => phs_fks_get_ch_to_em
  procedure :: display_kinematics => phs_fks_display_kinematics
  end type phs_fks_t


contains

  subroutine phs_fks_config_final (object)
    class(phs_fks_config_t), intent(inout) :: object
!    call object%phs_wood_config_t%final ()
  end subroutine phs_fks_config_final
 
  subroutine phs_fks_config_write (object, unit)
    class(phs_fks_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call object%phs_wood_config_t%write
  end subroutine phs_fks_config_write

  subroutine phs_fks_config_configure (phs_config, sqrts, &
        sqrts_fixed, cm_frame, azimuthal_dependence, rebuild, &
        ignore_mismatch, nlo_type)
    class(phs_fks_config_t), intent(inout) :: phs_config
    real(default), intent(in) :: sqrts
    logical, intent(in), optional :: sqrts_fixed
    logical, intent(in), optional :: cm_frame
    logical, intent(in), optional :: azimuthal_dependence
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    type(string_t), intent(inout), optional :: nlo_type
    if (present (nlo_type)) then
      if (nlo_type /= 'Real') & 
        call msg_fatal ("FKS config has to be called with nlo_type = 'Real'")
    end if 
    phs_config%n_par = phs_config%n_par + 3
!!! Channel equivalences not accessible yet
    phs_config%provides_equivalences = .false.
  end subroutine phs_fks_config_configure

  subroutine phs_fks_config_startup_message (phs_config, unit)
    class(phs_fks_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    call phs_config%phs_wood_config_t%startup_message
  end subroutine phs_fks_config_startup_message

  subroutine phs_fks_config_allocate_instance (phs)
    class(phs_t), intent(inout), pointer :: phs
    allocate (phs_fks_t :: phs)
  end subroutine phs_fks_config_allocate_instance

  subroutine phs_fks_config_set_born_config (phs_config, phs_cfg_born)
    class(phs_fks_config_t), intent(inout) :: phs_config
    type(phs_wood_config_t), intent(in), target :: phs_cfg_born
    phs_config%forest = phs_cfg_born%forest
    phs_config%n_channel = phs_cfg_born%n_channel
    phs_config%n_par = phs_cfg_born%n_par
    phs_config%sqrts = phs_cfg_born%sqrts
    phs_config%par = phs_cfg_born%par
    phs_config%sqrts_fixed = phs_cfg_born%sqrts_fixed
    phs_config%azimuthal_dependence = phs_cfg_born%azimuthal_dependence
    phs_config%provides_chains = phs_cfg_born%provides_chains
    phs_config%chain = phs_cfg_born%chain
  end subroutine phs_fks_config_set_born_config

  subroutine kinematics_counter_init (counter, n_bins)
    class(kinematics_counter_t), intent(inout) :: counter
    integer, intent(in) :: n_bins
    counter%n_bins = n_bins
    allocate (counter%histo_xi (n_bins), counter%histo_xi_tilde (n_bins))
    allocate (counter%histo_y (n_bins), counter%histo_phi (n_bins))
    allocate (counter%histo_xi_max (n_bins))
    counter%histo_xi = 0
    counter%histo_xi_tilde = 0
    counter%histo_xi_max = 0
    counter%histo_y = 0
    counter%histo_phi = 0
  end subroutine kinematics_counter_init

  subroutine kinematics_counter_record (counter, xi, xi_tilde, &
                                        xi_max, y, phi)
     class(kinematics_counter_t), intent(inout) :: counter
     real(default), intent(in), optional :: xi, xi_tilde, xi_max
     real(default), intent(in), optional :: y, phi
 
     if (counter%n_bins > 0) then
       if (present (xi)) then
          call fill_histogram (counter%histo_xi, xi, &
                               0.0_default, 1.0_default)
       end if
       if (present (xi_tilde)) then
          call fill_histogram (counter%histo_xi_tilde, xi_tilde, &
                               0.0_default, 1.0_default)
       end if
       if (present (xi_max)) then
          call fill_histogram (counter%histo_xi_max, xi_max, &
                               0.0_default, 1.0_default)
       end if
       if (present (y)) then
          call fill_histogram (counter%histo_y, y, -1.0_default, 1.0_default)
       end if
       if (present (phi)) then
          call fill_histogram (counter%histo_phi, phi, 0.0_default, twopi)
       end if
     end if
  contains
     subroutine fill_histogram (histo, value, val_min, val_max)
        integer, dimension(:), allocatable :: histo
        real(default), intent(in) :: value, val_min, val_max
        real(default) :: step, lo, hi
        integer :: bin
        step = (val_max-val_min) / counter%n_bins
        do bin = 1, counter%n_bins
           lo = (bin-1) * step
           hi = bin * step
           if (value >= lo .and. value < hi) then
               histo (bin) = histo (bin) + 1
               exit
           end if
        end do
     end subroutine fill_histogram
  end subroutine kinematics_counter_record

  subroutine kinematics_counter_display (counter)
     class(kinematics_counter_t), intent(in) :: counter
     print *, 'xi: ', counter%histo_xi
     print *, 'xi_tilde: ', counter%histo_xi_tilde
     print *, 'xi_max: ', counter%histo_xi_max
     print *, 'y: ', counter%histo_y
     print *, 'phi: ', counter%histo_phi
  end subroutine kinematics_counter_display

  subroutine phs_fks_init (phs, phs_config)
    class(phs_fks_t), intent(out) :: phs
    class(phs_config_t), intent(in), target :: phs_config


    call phs%base_init (phs_config)
    select type (phs_config)
    type is (phs_fks_config_t)
       phs%config => phs_config
       phs%forest = phs_config%forest
    end select
!!!    allocate (phs%f (phs%config%n_channel)); phs%f = 0._default
    deallocate (phs%r)
    allocate (phs%r (phs%config%n_par-3, phs%config%n_channel)); phs%r = 0

    select type(phs)
    type is (phs_fks_t)
      call phs%init_momenta (phs_config)
      allocate (phs%xi_max (phs_config%n_tot))
      phs%xi_max = 0._default
      phs%jac_rand = 1._default
    end select
  end subroutine phs_fks_init

  subroutine phs_fks_final (object)
    class(phs_fks_t), intent(inout) :: object
  end subroutine phs_fks_final

  subroutine phs_fks_init_momenta (phs, phs_config)
    class(phs_fks_t), intent(inout) :: phs
    class(phs_config_t), intent(in) :: phs_config
    allocate (phs%p_born (phs_config%n_in))
    allocate (phs%q_born (phs_config%n_out-1))
    allocate (phs%p_real (phs_config%n_in))
    allocate (phs%q_real (phs_config%n_out-1))
  end subroutine phs_fks_init_momenta

  subroutine phs_fks_evaluate_selected_channel (phs, c_in, r_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    real(default), intent(in), dimension(:) :: r_in
    integer :: em 
    type(vector4_t), dimension(:), allocatable :: p_born_tot
    real(default), dimension(:), allocatable :: r_born
    integer :: n_r_born
    type (vector4_t), dimension(:), allocatable :: p_real_tot
    real(default) :: phi
    real(default) :: sqrts_beam
    integer :: n_in, n_out, n_tot
    real(default), parameter :: xi_min = 1d-5, y_max = 1.0
    real(default), parameter :: tiny_y = 1d-5
 
    n_r_born = size(r_in) - 3
    allocate (r_born (n_r_born))
    r_born = r_in(1:n_r_born)
    call phs%phs_wood_t%evaluate_selected_channel (c_in, r_born)
    phs%p_born = phs%phs_wood_t%p
    phs%q_born = phs%phs_wood_t%q
    n_in = size (phs%p_born)
    n_out = size(phs%q_born)
    n_tot = n_in + n_out
    allocate (p_born_tot (n_tot))
    p_born_tot (1:n_in) = phs%p_born
    p_born_tot (n_in+1:) = phs%q_born

    !!! Jacobian corresponding to the transformation rand -> (xi, y, phi)
    phs%jac_rand = 1.0
    !!! Produce real momentum
    phs%y = (1 -2*r_in (n_r_born+2))*y_max
    phs%jac_rand = phs%jac_rand * 3 * (1-phs%y**2)
    phs%y = 1.5_default * (phs%y - phs%y**3/3)
    phs%phi = r_in (n_r_born+3)*twopi
    phs%xi_tilde = xi_min + r_in (n_r_born+1)*(1-xi_min)
    phs%jac_rand = phs%jac_rand * (1-xi_min)       
    do em = 1, phs%config%n_tot
      if (any (phs%emitters == em)) then
         phs%xi_max (em) = fks_get_xi_max_fsr (p_born_tot, em)
      end if
    end do
    phs%f(c_in) = phs%f(c_in) * phs%jac_rand

    phs%volume = phs%volume * phs%config%sqrts**2 / (8*twopi2)
    phs%r(:,c_in) = r_in(1:n_r_born)
    phs%r_real = r_in (n_r_born+1:)
  end subroutine phs_fks_evaluate_selected_channel

  subroutine phs_fks_evaluate_other_channels (phs, c_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    integer :: c, em
    call phs%phs_wood_t%evaluate_other_channels (c_in)
    do c = 1, size (phs%f)
      if (c == c_in) cycle
      phs%f(c) = phs%f(c) * phs%jac_rand
    end do
    phs%r_defined = .true.
  end subroutine phs_fks_evaluate_other_channels

  subroutine phs_fks_get_mcpar (phs, c, r)
    class(phs_fks_t), intent(in) :: phs
    integer, intent(in) :: c
    real(default), dimension(:), intent(out) :: r
    integer :: n_r_born 
    n_r_born = size (phs%r(:,c))
    r(1:n_r_born) = phs%r(:,c)
    r(n_r_born+1:) = phs%r_real
  end subroutine phs_fks_get_mcpar

  subroutine phs_fks_get_real_kinematics (phs, xit, y, phi, xi_max, jac)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(out), dimension(:), allocatable :: xi_max
    real(default), intent(out) :: xit
    real(default), intent(out) :: y, phi
    real(default), intent(out), dimension(3) :: jac
    xit = phs%xi_tilde
    y = phs%y
    phi = phs%phi
    xi_max = phs%xi_max
    jac = phs%jac
  end subroutine phs_fks_get_real_kinematics

  subroutine phs_fks_set_emitters (phs, emitters)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in), dimension(:), allocatable :: emitters
    phs%emitters = emitters
  end subroutine phs_fks_set_emitters

  subroutine phs_fks_get_born_momenta (phs, p)
    class(phs_fks_t), intent(inout) :: phs
    type(vector4_t), intent(out), dimension(:) :: p
    p(1:phs%config%n_in) = phs%p_born
    p(phs%config%n_in+1:) = phs%q_born
  end subroutine phs_fks_get_born_momenta

  subroutine phs_fks_get_outgoing_momenta (phs, q)
    class(phs_fks_t), intent(in) :: phs
    type(vector4_t), intent(out), dimension(:) :: q
    q = phs%q_real
  end subroutine phs_fks_get_outgoing_momenta

  subroutine phs_fks_get_incoming_momenta (phs, p)
    class(phs_fks_t), intent(in) :: phs
    type(vector4_t), intent(inout), dimension(:), allocatable :: p
    p = phs%p_real
  end subroutine phs_fks_get_incoming_momenta

!!!  function phs_fks_get_ch_to_em (phs) result (ch_to_em)
!!!    class(phs_fks_t), intent(inout) :: phs
!!!    integer, dimension(:), allocatable :: ch_to_em
!!!    ch_to_em = phs%ch_to_em
!!!  end function phs_fks_get_ch_to_em

  subroutine phs_fks_display_kinematics (phs)
     class(phs_fks_t), intent(in) :: phs
!     call phs%counter%display ()
  end subroutine phs_fks_display_kinematics

  subroutine fks_born_to_real_fsr (emitter, xi, y, phi, p_born, p_real, jac)
    !!! Important: Momenta must be input in the center-of-mass frame
    integer, intent(in) :: emitter
    type(vector4_t), intent(inout), dimension(:), allocatable :: p_born 
    real(default), intent(in) :: xi, y, phi
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    real(default), intent(out), dimension(3), optional :: jac
    integer nlegborn, nlegreal
    type(vector4_t) :: k_rec, q
    real(default) :: q0, q2, k0_np1, uk_np1, uk_n
    real(default) :: uk_rec, k_rec0
    type(vector3_t) :: k_n_born, k_real, k
    real(default) :: k_real0, uk_real
    real(default) :: uk_n_born
    real(default) :: mrec2
    real(default) :: uk, k2
    real(default) :: cpsi, beta
    type(vector3_t) :: vec, vec_orth
    type(lorentz_transformation_t) :: rot, lambda
    integer :: i
    real(default) :: pb
    nlegborn = size(p_born)
    if (emitter <= 2 .or. emitter > nlegborn) then
      call msg_fatal ("fks_born_to_real: emitter must be larger than 2.")
      return
    end if
    nlegreal = nlegborn + 1
    allocate (p_real (nlegreal))

    p_real(1) = p_born(1)
    p_real(2) = p_born(2)
    q = p_born(1) + p_born(2)
    q0 = vector4_get_component (q, 0)
    q2 = q**2

    k0_np1 = q0*xi/2
    uk_np1 = k0_np1
    k_n_born = space_part (p_born(emitter))
    uk_n_born = space_part_norm (p_born(emitter))

    mrec2 = (q-p_born(emitter))**2
    uk_n = (q2 - mrec2 - 2*q0*uk_np1) / (2*(q0 - uk_np1*(1-y)))
    uk = sqrt (uk_n**2 + uk_np1**2 + 2*uk_n*uk_np1*y)
    vec = uk_n / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    call vector4_set_component (p_real(emitter), 0, uk_n)
    do i = 1, 3
       call vector4_set_component (p_real(emitter), i, &
            vector3_get_component (vec, i))
    end do
    cpsi = (uk_n**2 + uk**2 - uk_np1**2) / (2*(uk_n * uk))
    !!! This is to catch the case where cpsi = 1, but numerically 
    !!! turns out to be slightly larger than 1. 
    if (cpsi > 1._default) cpsi = 1._default
    rot = rotation (cpsi, -sqrt (1-cpsi**2), vec_orth)
    p_real(emitter) = rot*p_real(emitter)
    vec = uk_np1 / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    call vector4_set_component (p_real(nlegreal), 0, uk_np1)
    do i = 1, 3
       call vector4_set_component (p_real(nlegreal), i, &
            vector3_get_component (vec,i))
    end do
    cpsi = (uk_np1**2 + uk**2 - uk_n**2) / (2*(uk_np1 * uk))
    rot = rotation (cpsi, sqrt (1-cpsi**2), vec_orth)
    p_real(nlegreal) = rot*p_real(nlegreal)
    k_rec0 = q0 - vector4_get_component (p_real(emitter), 0) - &
                  vector4_get_component (p_real(nlegreal), 0)
    uk_rec = sqrt (k_rec0**2 - mrec2)
    beta = (q2 - (k_rec0 + uk_rec)**2) / (q2 + (k_rec0 + uk_rec)**2)
    k = space_part (p_real(emitter) + p_real(nlegreal))
    do i = 1, 3
      call vector3_set_component (vec, i, 1 / uk*vector3_get_component(k,i))
    end do
    lambda = boost (beta/sqrt(1-beta**2), vec)
    do i = 3, nlegborn
      if (i /= emitter) then
        p_real(i) = lambda * p_born(i)
      end if
    end do
    do i = 1, 3
      pb = vector4_get_component (p_born(emitter), i)
      call vector3_set_component (vec, i, pb/uk_n_born)
    end do
    rot = rotation (cos(phi), sin(phi), vec)
    p_real(nlegreal) = rot * p_real(nlegreal)
    p_real(emitter) = rot * p_real(emitter)
    k2 = 2*uk_n*uk_np1*(1-y)
    if (present (jac)) then
      jac(1) = 1.0*uk_n**2/uk_n_born / (uk_n - k2/(2*q0))
      !!! Soft jacobian
      jac(2) = 1.0
      !!! Collinear jacobian
      jac(3) = 1.0*(1-xi/2*q0/uk_n_born)
    end if
  end subroutine fks_born_to_real_fsr

  function fks_get_xi_max_fsr (p_born, emitter) result (xi_max)
    type(vector4_t), intent(in), dimension(:), allocatable :: p_born
    integer, intent(in) :: emitter
    real(default) :: xi_max
    real(default) :: uk_n_born, q0
    q0 = vector4_get_component (p_born(1) + p_born(2), 0)
    uk_n_born = space_part_norm (p_born(emitter))
    xi_max = 2*uk_n_born / q0
  end function fks_get_xi_max_fsr
  
  subroutine fks_born_to_real_isr &
       (emitter, xi, y, phi, p_born, p_real, sqrts_beam, jac)
    !!! Important: Import momenta in the lab frame
    integer, intent(in) :: emitter
    type(vector4_t), intent(in) , dimension(:), allocatable :: p_born
    real(default), intent(in) :: xi, y, phi
    real(default), intent(in) :: sqrts_beam
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    real(default), intent(out), dimension(3) :: jac
    integer :: nlegborn, nlegreal
    real(default) :: sqrts_born
    real(default) :: k0_np1
    type(vector3_t) :: beta_l, vec_t
    real(default) :: beta_t, beta_gamma_l, beta_gamma_t
    real(default) :: k_tmp, k_p, k_m, k_p0, k_m0
    real(default) :: pt_rad
    type(lorentz_transformation_t) :: lambda_t, lambda_l, lambda_l_inv
    real(default) :: x_plus, x_minus, barx_plus, barx_minus
    integer :: i
    nlegborn = size (p_born)
    nlegreal = nlegborn  + 1
    sqrts_born = sqrt ((p_born(1) + p_born(2))**2)
    allocate (p_real (nlegreal))
    !!! Create radiation momentum
    k0_np1 = sqrts_born*xi/2 
    !!! There must be the real cm-energy, not the Born one! 
    !!!    s_real = s_born / (1-xi)
    !!! Build radiation momentum in the rest frame of the real momenta
    call vector4_set_component &
         (p_real(nlegreal), 0, k0_np1)
    call vector4_set_component &
         (p_real(nlegreal), 1, k0_np1*sqrt(1-y**2)*sin(phi))
    call vector4_set_component &
         (p_real(nlegreal), 2, k0_np1*sqrt(1-y**2)*cos(phi))
    call vector4_set_component (p_real(nlegreal), 3, k0_np1*y)
    !!! Boost to lab frame missing
    pt_rad = transverse_part (p_real(nlegreal))
    beta_t = sqrt (1 + sqrts_born**2 * (1-xi) / pt_rad**2)
    beta_gamma_t = 1/sqrt(beta_t)
    k_p0 = vector4_get_component (p_born(1), 0)
    k_m0 = vector4_get_component (p_born(2), 0)
    do i = 1,3
        k_p = vector4_get_component (p_born(1), i)
        k_m = vector4_get_component (p_born(2), i)
        k_tmp = (k_p + k_m) / (k_p0 + k_m0)
        call vector3_set_component (beta_l, i, k_tmp)
    end do
    beta_gamma_l = beta_l**1
    beta_l = beta_l / beta_gamma_l
    beta_gamma_l = beta_gamma_l / sqrt (1 - beta_gamma_l**2)
    call vector3_set_component &
         (vec_t, 1, vector4_get_component (p_real(nlegreal), 1))
    call vector3_set_component &
         (vec_t, 2, vector4_get_component (p_real(nlegreal), 2))
    call vector3_set_component (vec_t, 3, 0._default)
    call normalize (vec_t)
    lambda_l = boost(beta_gamma_l, beta_l)
    lambda_t = boost(-beta_gamma_t, vec_t)
    lambda_l_inv = boost(-beta_gamma_l, beta_l)
    forall (i=3:nlegborn) &
         p_real(i) = lambda_l_inv * lambda_t * lambda_l * p_born(i)
    !!! Now need access to the x-variables of the IS-partons
    barx_plus = 2*vector4_get_component(p_born(1), 0)/sqrts_beam
    barx_minus = 2*vector4_get_component(p_born(2), 0)/sqrts_beam
    x_plus = barx_plus/sqrt(1-xi) * sqrt ((2-xi*(1-y)) / (2-xi*(1+y)))
    x_minus = barx_minus/sqrt(1-xi) * sqrt ((2-xi*(1+y)) / (2-xi*(1-y)))
    p_real(1) = x_plus/barx_plus * p_born(1)
    p_real(2) = x_minus/barx_minus * p_born(2)
    !!! Total nonsense
    jac(1) = 1
  end subroutine fks_born_to_real_isr

end module phs_fks

