! WHIZARD 2.2.4 Feb 06 2015
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

module phs_fks
  
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use lorentz
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

  type, extends (phs_wood_config_t) :: phs_fks_config_t
  contains
    procedure :: final => phs_fks_config_final
    procedure :: write => phs_fks_config_write
    procedure :: configure => phs_fks_config_configure
    procedure :: startup_message => phs_fks_config_startup_message
    procedure :: increase_n_par => phs_fks_config_increase_n_par
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
    type(vector4_t), dimension(:), allocatable :: p_born_tot
    real(default) :: E_beam_tot
    real(default) :: E_gluon
    real(default) :: mrec2
    real(default), dimension(:), allocatable :: m2
    real(default) :: xi_tilde, phi
    real(default), dimension(:), allocatable :: xi_max
    real(default), dimension(:), allocatable :: y
    real(default) :: xi_min, y_max
    real(default) :: y_soft
    real(default), dimension(:), allocatable :: jac_rand
    real(default), dimension(3) :: jac
    integer, dimension(:), allocatable :: emitters
    type(kinematics_counter_t) :: counter
    logical :: massive_phsp = .false.
    logical :: perform_generation = .true.

  contains
    procedure :: init => phs_fks_init
    procedure :: final => phs_fks_final
    procedure :: init_momenta => phs_fks_init_momenta
    procedure :: evaluate_selected_channel => phs_fks_evaluate_selected_channel
    procedure :: evaluate_other_channels => phs_fks_evaluate_other_channels
    procedure :: get_mcpar => phs_fks_get_mcpar
    procedure :: get_real_kinematics => phs_fks_get_real_kinematics
    procedure :: enable_massive_emitters => &
                       phs_fks_enable_massive_emitters
    procedure :: set_emitters => phs_fks_set_emitters
    procedure :: get_born_momenta => phs_fks_get_born_momenta
    procedure :: get_outgoing_momenta => phs_fks_get_outgoing_momenta
    procedure :: get_incoming_momenta => phs_fks_get_incoming_momenta
    procedure :: display_kinematics => phs_fks_display_kinematics
    procedure :: generate_real_kinematics => phs_fks_generate_real_kinematics
    procedure :: generate_fsr => phs_fks_generate_fsr
    generic :: compute_emitter_kinematics => &
                      compute_emitter_kinematics_massless, &
                      compute_emitter_kinematics_massive 
    procedure :: compute_emitter_kinematics_massless => &
                      phs_fks_compute_emitter_kinematics_massless
    procedure :: compute_emitter_kinematics_massive => &
                      phs_fks_compute_emitter_kinematics_massive
    procedure :: generate_isr => phs_fks_generate_isr
  end type phs_fks_t



  interface compute_beta
    module procedure compute_beta_massless
    module procedure compute_beta_massive
  end interface

  interface get_xi_max_fsr
    module procedure get_xi_max_fsr_massless
    module procedure get_xi_max_fsr_massive
  end interface


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
    if (.not. phs_config%extended_phs) &
       phs_config%n_par = phs_config%n_par + 3
!!! Channel equivalences not accessible yet
    phs_config%provides_equivalences = .false.
  end subroutine phs_fks_config_configure

  subroutine phs_fks_config_startup_message (phs_config, unit)
    class(phs_fks_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    call phs_config%phs_wood_config_t%startup_message
  end subroutine phs_fks_config_startup_message

  subroutine phs_fks_config_increase_n_par (phs_config)
    class(phs_fks_config_t), intent(inout) :: phs_config
    phs_config%n_par = phs_config%n_par + 3
  end subroutine phs_fks_config_increase_n_par

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

    select type(phs)
    type is (phs_fks_t)
      phs%n_r_born = phs%config%n_par - 3
      call phs%init_momenta (phs_config)
      allocate (phs%m2 (phs_config%n_tot))
      allocate (phs%xi_max (phs_config%n_tot))
      allocate (phs%y (phs_config%n_tot))
      allocate (phs%jac_rand (phs_config%n_tot))
      phs%xi_max = 0._default
      phs%y = 0._default
      phs%jac_rand = 1._default
      phs%jac = 1._default
      phs%xi_min = 1d-5
      phs%y_max = 1._default
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
    allocate (phs%p_born_tot (phs%config%n_in + phs%config%n_out-1))
  end subroutine phs_fks_init_momenta

  subroutine phs_fks_evaluate_selected_channel (phs, c_in, r_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    real(default), intent(in), dimension(:) :: r_in
    integer :: em 
    real(default), dimension(:), allocatable :: r_born
    integer :: n_r_born
    integer :: n_in
 
    n_in = phs%config%n_in
    call phs%phs_wood_t%evaluate_selected_channel (c_in, r_in)
    phs%p_born = phs%phs_wood_t%p
    phs%q_born = phs%phs_wood_t%q
    phs%p_born_tot (1:n_in) = phs%p_born
    phs%p_born_tot (n_in+1:) = phs%q_born
    phs%r(:,c_in) = r_in

    if (phs%perform_generation) call phs%generate_real_kinematics &
                                             (r_in(phs%n_r_born+1:phs%n_r_born+3))
  end subroutine phs_fks_evaluate_selected_channel

  subroutine phs_fks_evaluate_other_channels (phs, c_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    integer :: c
    call phs%phs_wood_t%evaluate_other_channels (c_in)
    phs%r_defined = .true.
  end subroutine phs_fks_evaluate_other_channels

  subroutine phs_fks_get_mcpar (phs, c, r)
    class(phs_fks_t), intent(in) :: phs
    integer, intent(in) :: c
    real(default), dimension(:), intent(out) :: r
    integer :: n_r_born 
    r(1:phs%n_r_born) = phs%r(1:phs%n_r_born,c)
    r(phs%n_r_born+1:) = phs%r_real
  end subroutine phs_fks_get_mcpar

  subroutine phs_fks_get_real_kinematics (phs, xit, y, phi, xi_max, jac, jac_rand)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(out), dimension(:), allocatable :: xi_max
    real(default), intent(out) :: xit
    real(default), intent(out), dimension(:), allocatable :: y
    real(default), intent(out) :: phi
    real(default), intent(out), dimension(3) :: jac
    real(default), intent(out), dimension(:), allocatable :: jac_rand
    xit = phs%xi_tilde
    y = phs%y
    phi = phs%phi
    xi_max = phs%xi_max
    jac = phs%jac
    jac_rand = phs%jac_rand
  end subroutine phs_fks_get_real_kinematics

  subroutine phs_fks_enable_massive_emitters (phs_fks)
    class(phs_fks_t), intent(inout) :: phs_fks
    phs_fks%massive_phsp = .true.
  end subroutine phs_fks_enable_massive_emitters

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

  subroutine phs_fks_display_kinematics (phs)
     class(phs_fks_t), intent(in) :: phs
!     call phs%counter%display ()
  end subroutine phs_fks_display_kinematics

  subroutine phs_fks_generate_real_kinematics (phs, r_in)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(in), dimension(:) :: r_in
    integer :: em
    real(default) :: beta

    if (size (r_in) /= 3) call msg_fatal &
                        ("Real kinematics need to be generated using three random numbers!")
    phs%E_beam_tot = phs%p_born(1)%p(0) + phs%p_born(2)%p(0)

    !!! Jacobian corresponding to the transformation rand -> (xi, y, phi)
    phs%jac_rand = 1.0
    !!! Produce real momentum
    phs%phi = r_in(3)*twopi
    phs%xi_tilde = phs%xi_min + r_in(1)*(1-phs%xi_min)
    phs%jac_rand = phs%jac_rand * (1-phs%xi_min)       
    do em = 1, phs%config%n_tot
      if (any (phs%emitters == em)) then
         if (phs%massive_phsp) then
            phs%m2(em) = phs%p_born_tot(em)**2
            beta = beta_emitter (phs%E_beam_tot, phs%p_born_tot(em))
            phs%y(em) = 1.0/beta * (1-(1+beta) * &
                            exp(-r_in(2)*log((1+beta)/(1-beta))))
            phs%jac_rand(em) = phs%jac_rand(em) * &
                               (1-beta*phs%y(em))*log((1+beta)/(1-beta))/beta
            phs%xi_max (em) = get_xi_max_fsr &
                                   (phs%p_born_tot, em, phs%m2(em), phs%y(em))
         else
            phs%m2(em) = 0._default
            phs%xi_max (em) = get_xi_max_fsr (phs%p_born_tot, em)
         end if
      end if
    end do
    if (.not. phs%massive_phsp) then
       phs%y = (1-2*r_in(2))*phs%y_max
       phs%jac_rand = phs%jac_rand*3*(1-phs%y(1)**2)
       phs%y = 1.5_default*(phs%y - phs%y**3/3)
    end if

    phs%r_real = r_in
  contains
    function beta_emitter (q0, p) result (beta)
      real(default), intent(in) :: q0
      type(vector4_t), intent(in) :: p
      real(default) :: beta
      real(default) :: m2, mrec2, k0_max
      m2 = p**2
      mrec2 = (q0-p%p(0))**2 - p%p(1)**2 - p%p(2)**2 - p%p(3)**2
      k0_max = (q0**2-mrec2+m2)/(2*q0)
      beta = sqrt(1-m2/k0_max**2)
    end function beta_emitter 
  end subroutine phs_fks_generate_real_kinematics

  subroutine phs_fks_generate_fsr (phs, emitter, p_born, p_real)
    !!! Important: Momenta must be input in the center-of-mass frame
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p_born 
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    integer nlegborn, nlegreal
    type(vector4_t) :: q
    real(default) :: q0, q2, uk_np1, uk_n
    real(default) :: uk_rec, k_rec0
    type(vector3_t) :: k_n_born, k
    real(default) :: uk_n_born
    real(default) :: uk, k2, k0_n
    real(default) :: cpsi, beta
    type(vector3_t) :: vec, vec_orth
    type(lorentz_transformation_t) :: rot, lambda
    integer :: i
    real(default) :: xi, y, phi

    xi = phs%xi_tilde * phs%xi_max(emitter)
    y = phs%y(emitter)
    phi = phs%phi
    nlegreal = phs%config%n_in + phs%config%n_out
    nlegborn = nlegreal-1
    if (emitter <= 2 .or. emitter > nlegborn) then
      call msg_fatal ("Generate FSR phase space: Invalid emitter!")
    end if
    allocate (p_real (nlegreal))

    p_real(1) = p_born(1)
    p_real(2) = p_born(2)
    q = p_born(1) + p_born(2)
    q0 = phs%E_beam_tot
    q2 = q**2

    phs%E_gluon = q0*xi/2
    uk_np1 = phs%E_gluon
    k_n_born = p_born(emitter)%p(1:3)
    uk_n_born = k_n_born**1

    phs%mrec2 = (q-p_born(emitter))**2
    if (phs%massive_phsp) then
       call phs%compute_emitter_kinematics (emitter, k0_n, uk_n, uk)
    else
       call phs%compute_emitter_kinematics (emitter, uk_n, uk)
       phs%y_soft = y
       k0_n = uk_n
    end if
    
    vec = uk_n / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real(emitter)%p(0) = k0_n
    p_real(emitter)%p(1:3) = vec%p(1:3)
    cpsi = (uk_n**2 + uk**2 - uk_np1**2) / (2*(uk_n * uk))
    !!! This is to catch the case where cpsi = 1, but numerically 
    !!! turns out to be slightly larger than 1. 
    if (cpsi > 1._default) cpsi = 1._default
    rot = rotation (cpsi, -sqrt (1-cpsi**2), vec_orth)
    p_real(emitter) = rot*p_real(emitter)
    vec = uk_np1 / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real(nlegreal)%p(0) = uk_np1
    p_real(nlegreal)%p(1:3) = vec%p(1:3)
    cpsi = (uk_np1**2 + uk**2 - uk_n**2) / (2*(uk_np1 * uk))
    rot = rotation (cpsi, sqrt (1-cpsi**2), vec_orth)
    p_real(nlegreal) = rot*p_real(nlegreal)
    k_rec0 = q0 - p_real(emitter)%p(0) - p_real(nlegreal)%p(0)
    uk_rec = sqrt (k_rec0**2 - phs%mrec2)
    if (phs%massive_phsp) then
       beta = compute_beta (q2, k_rec0, uk_rec, &
                            p_born(emitter)%p(0), uk_n_born)
    else
       beta = compute_beta (q2, k_rec0, uk_rec)
    end if   
    k = p_real(emitter)%p(1:3) + p_real(nlegreal)%p(1:3)
    vec%p(1:3) = 1/uk*k%p(1:3)
    lambda = boost (beta/sqrt(1-beta**2), vec)
    do i = 3, nlegborn
      if (i /= emitter) then
        p_real(i) = lambda * p_born(i)
      end if
    end do
    vec%p(1:3) = p_born(emitter)%p(1:3)/uk_n_born
    rot = rotation (cos(phi), sin(phi), vec)
    p_real(nlegreal) = rot * p_real(nlegreal)
    p_real(emitter) = rot * p_real(emitter)
    if (phs%massive_phsp) then 
       phs%jac(1) = phs%jac(1)*4/q0/uk_n_born/xi
    else
       k2 = 2*uk_n*uk_np1*(1-y)
       phs%jac(1) = uk_n**2/uk_n_born / (uk_n - k2/(2*q0))
    end if
    !!! Soft jacobian
    phs%jac(2) = 1.0
    !!! Collinear jacobian
    phs%jac(3) = 1-xi/2*q0/uk_n_born

  end subroutine phs_fks_generate_fsr

  subroutine phs_fks_compute_emitter_kinematics_massless &
                                    (phs, em, uk_em, uk)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: em
    real(default), intent(out) :: uk_em, uk
    real(default) :: y, k0_np1, q0, q2

    y = phs%y(em); k0_np1 = phs%E_gluon
    q0 = phs%E_beam_tot; q2 = q0**2

    uk_em = (q2 - phs%mrec2 - 2*q0*k0_np1) / (2*(q0 - k0_np1*(1-y))) 
    uk = sqrt (uk_em**2 + k0_np1**2 + 2*uk_em*k0_np1*y)
  end subroutine phs_fks_compute_emitter_kinematics_massless

  subroutine phs_fks_compute_emitter_kinematics_massive &
                                    (phs, em, k0_em, uk_em, uk)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: em
    real(default), intent(inout) :: k0_em, uk_em, uk
    real(default) :: y, k0_np1, q0, q2, mrec2, m2
    real(default) :: k0_rec_max, k0_em_max, k0_rec, uk_rec
    real(default) :: z, z1, z2

    y = phs%y(em); k0_np1 = phs%E_gluon
    q0 = phs%E_beam_tot; q2 = q0**2
    mrec2 = phs%mrec2; m2 = phs%m2(em)

    k0_rec_max = (q2-m2+mrec2)/(2*q0)
    k0_em_max = (q2+m2-mrec2)/(2*q0)
    z1 = (k0_rec_max+sqrt (k0_rec_max**2-mrec2))/q0
    z2 = (k0_rec_max-sqrt (k0_rec_max**2-mrec2))/q0
    z = z2 - (z2-z1)*(1+y)/2
    k0_em = k0_em_max - k0_np1*z
    k0_rec = q0 - k0_np1 - k0_em
    uk_em = sqrt(k0_em**2-m2)
    uk_rec = sqrt(k0_rec**2 - mrec2)
    uk = uk_rec
    phs%jac = q0*(z1-z2)/4*k0_np1
    phs%y_soft = (2*q2*z-q2-mrec2+m2)/(sqrt(k0_em_max**2-m2)*q0)/2
  end subroutine phs_fks_compute_emitter_kinematics_massive

  function compute_beta_massless (q2, k0_rec, uk_rec) result (beta)
    real(default), intent(in) :: q2, k0_rec, uk_rec
    real(default) :: beta
    beta = (q2 - (k0_rec + uk_rec)**2) / (q2 + (k0_rec + uk_rec)**2)
  end function compute_beta_massless

  function compute_beta_massive (q2, k0_rec, uk_rec, &
                                 k0_em_born, uk_em_born) result (beta)
    real(default), intent(in) :: q2, k0_rec, uk_rec
    real(default), intent(in) :: k0_em_born, uk_em_born
    real(default) :: beta
    real(default) :: k0_rec_born, uk_rec_born, alpha
    k0_rec_born = sqrt(q2) - k0_em_born
    uk_rec_born = uk_em_born
    alpha = (k0_rec+uk_rec)/(k0_rec_born+uk_rec_born)
    beta = (1-alpha**2)/(1+alpha**2)
  end function compute_beta_massive

  function get_xi_max_fsr_massless (p_born, emitter) result (xi_max)
    type(vector4_t), intent(in), dimension(:) :: p_born
    integer, intent(in) :: emitter
    real(default) :: xi_max
    real(default) :: uk_n_born, q0
    q0 = vector4_get_component (p_born(1) + p_born(2), 0)
    uk_n_born = space_part_norm (p_born(emitter))
    xi_max = 2*uk_n_born / q0
  end function get_xi_max_fsr_massless
  
  function get_xi_max_fsr_massive (p_born, emitter, m2, y) result (xi_max)
    type(vector4_t), intent(in), dimension(:) :: p_born
    integer, intent(in) :: emitter
    real(default), intent(in) :: m2, y
    real(default) :: xi_max
    real(default) :: q0, mrec2, k0_rec_max
    real(default) :: z, z1, z2
    real(default) :: k_np1_max
    q0 = 2*p_born(1)%p(0)
    associate (p => p_born(emitter)%p)
       mrec2 = (q0-p(0))**2 - p(1)**2 - p(2)**2 - p(3)**2
    end associate
    k0_rec_max = (q0**2-m2+mrec2)/(2*q0)
    z1 = (k0_rec_max+sqrt(k0_rec_max**2-mrec2))/q0
    z2 = (k0_rec_max-sqrt(k0_rec_max**2-mrec2))/q0
    z = z2 - (z2-z1)*(1+y)/2
    k_np1_max = -(q0**2*z**2 - 2*q0*k0_rec_max*z + mrec2)/(2*q0*z*(1-z))
    xi_max = 2*k_np1_max/q0
  end function get_xi_max_fsr_massive

  subroutine phs_fks_generate_isr &
       (phs, emitter, p_born, p_real)
    !!! Important: Import momenta in the lab frame
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: emitter
    type(vector4_t), intent(in) , dimension(:), allocatable :: p_born
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    real(default) :: xi, y, phi
    integer :: nlegborn, nlegreal
    real(default) :: sqrts_beam
    real(default) :: sqrts_born
    real(default) :: k0_np1
    type(vector3_t) :: beta_l, vec_t
    real(default) :: beta_t, beta_gamma_l, beta_gamma_t
    real(default) :: k_tmp, k_p, k_m, k_p0, k_m0
    real(default) :: pt_rad
    type(lorentz_transformation_t) :: lambda_t, lambda_l, lambda_l_inv
    real(default) :: x_plus, x_minus, barx_plus, barx_minus
    integer :: i

    xi = phs%xi_tilde * phs%xi_max(emitter)
    y = phs%y(emitter)
    phi = phs%phi
    nlegreal = phs%config%n_in + phs%config%n_out
    nlegborn = nlegreal - 1
    sqrts_beam = phs%config%sqrts
    sqrts_born = sqrt ((p_born(1) + p_born(2))**2)
    allocate (p_real (nlegreal))
    !!! Create radiation momentum
    k0_np1 = sqrts_born*xi/2 
    !!! There must be the real cm-energy, not the Born one! 
    !!!    s_real = s_born / (1-xi)
    !!! Build radiation momentum in the rest frame of the real momenta
    p_real(nlegreal)%p(0) = k0_np1
    p_real(nlegreal)%p(1) = k0_np1*sqrt(1-y**2)*sin(phi)
    p_real(nlegreal)%p(2) = k0_np1*sqrt(1-y**2)*cos(phi)
    p_real(nlegreal)%p(3) = k0_np1*y

    !!! Boost to lab frame missing
    pt_rad = transverse_part (p_real(nlegreal))
    beta_t = sqrt (1 + sqrts_born**2 * (1-xi) / pt_rad**2)
    beta_gamma_t = 1/sqrt(beta_t)
    k_p0 = vector4_get_component (p_born(1), 0)
    k_m0 = vector4_get_component (p_born(2), 0)
    
    beta_l%p(1:3) = (p_born(1)%p(1:3) + p_born(2)%p(1:3)) / &
                    (p_born(1)%p(0) + p_born(2)%p(0))
    beta_gamma_l = beta_l**1
    beta_l = beta_l / beta_gamma_l
    beta_gamma_l = beta_gamma_l / sqrt (1 - beta_gamma_l**2)
    vec_t%p(1:2) = p_real(nlegreal)%p(1:2)
    vec_t%p(3) = 0._default
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
    phs%jac(1) = 1
  end subroutine phs_fks_generate_isr

end module phs_fks

