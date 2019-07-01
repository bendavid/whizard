! WHIZARD 2.3.0 July 21 2016
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

module phs_fks

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use io_units, only: given_output_unit
  use format_utils, only: write_separator
  use lorentz
  use physics_defs
  use flavors
  use sf_mappings
  use sf_base
  use phs_base
  use phs_wood
  use cascades
  use process_constants
  use process_libraries
  use ttv_formfactors, only: THR_POS_WP, THR_POS_WM
  use ttv_formfactors, only: THR_POS_B, THR_POS_BBAR
  use nlo_data
  use fks_regions, only: region_data_t

  implicit none
  private

  public :: phs_fks_config_t
  public :: phs_fks_generator_t
  public :: phs_fks_t
  public :: beta_emitter

  integer, parameter, public :: I_XI = 1
  integer, parameter, public :: I_Y = 2
  integer, parameter, public :: I_PHI = 3

  integer, parameter, public :: PHS_MODE_UNDEFINED = 0
  integer, parameter, public :: PHS_MODE_ADDITIONAL_PARTICLE = 1
  integer, parameter, public :: PHS_MODE_COLLINEAR_REMNANT = 2

  integer, parameter, public :: GEN_REAL_PHASE_SPACE = 1
  integer, parameter, public :: GEN_SOFT_MISMATCH = 2
  integer, parameter, public :: GEN_SOFT_LIMIT_TEST = 3
  integer, parameter, public :: GEN_COLL_LIMIT_TEST = 4
  integer, parameter, public :: GEN_ANTI_COLL_LIMIT_TEST = 5
  integer, parameter, public :: GEN_SOFT_COLL_LIMIT_TEST = 6
  integer, parameter, public :: GEN_SOFT_ANTI_COLL_LIMIT_TEST = 7

  real(default), parameter :: xi_tilde_test_soft = 0.0001_default
  real(default), parameter :: xi_tilde_test_coll = 0.5_default
  real(default), parameter :: y_test_soft = 0.5_default
  real(default), parameter :: y_test_coll = 0.999_default


  type, extends (phs_wood_config_t) :: phs_fks_config_t
    integer :: mode = PHS_MODE_UNDEFINED
  contains
    procedure :: final => phs_fks_config_final
    procedure :: write => phs_fks_config_write
    procedure :: set_mode => phs_fks_config_set_mode
    procedure :: configure => phs_fks_config_configure
    procedure :: startup_message => phs_fks_config_startup_message
    procedure, nopass :: allocate_instance => phs_fks_config_allocate_instance
    procedure :: generate_phase_space_extra => phs_fks_config_generate_phase_space_extra
    procedure :: set_born_config => phs_fks_config_set_born_config
    procedure :: get_resonance_histories => phs_fks_config_get_resonance_histories
  end type phs_fks_config_t

  type :: phs_fks_generator_t
    integer, dimension(:), allocatable :: emitters
    type(real_kinematics_t), pointer :: real_kinematics => null()
    type(isr_kinematics_t), pointer :: isr_kinematics => null()
    integer :: n_in
    real(default) :: xi_min = tiny_07
    real(default) :: y_max = one
    real(default) :: sqrts
    real(default) :: E_gluon
    real(default) :: mrec2
    real(default), dimension(:), allocatable :: m2
    logical :: massive_phsp = .false.
    logical, dimension(:), allocatable :: is_massive
    logical :: singular_jacobian = .false.
    integer :: i_fsr_first = -1
    type(resonance_contributors_t), dimension(:), allocatable :: resonance_contributors !!! Put somewhere else?
    integer :: mode = GEN_REAL_PHASE_SPACE 
  contains
    procedure :: connect_kinematics => phs_fks_generator_connect_kinematics
    procedure :: compute_isr_kinematics => phs_fks_generator_compute_isr_kinematics
    generic :: generate_fsr => generate_fsr_default, generate_fsr_resonances
    procedure :: generate_fsr_default => phs_fks_generator_generate_fsr_default
    procedure :: generate_fsr_resonances => phs_fks_generator_generate_fsr_resonances
    procedure :: generate_fsr_in => phs_fks_generator_generate_fsr_in
    procedure :: generate_fsr_out => phs_fks_generator_generate_fsr_out
    generic :: compute_emitter_kinematics => &
       compute_emitter_kinematics_massless, &
       compute_emitter_kinematics_massive
    procedure :: compute_emitter_kinematics_massless => &
       phs_fks_generator_compute_emitter_kinematics_massless
    procedure :: compute_emitter_kinematics_massive => &
       phs_fks_generator_compute_emitter_kinematics_massive
    procedure :: generate_isr_decay => phs_fks_generator_generate_isr_decay
    procedure :: generate_isr_factorized => phs_fks_generator_generate_isr_factorized
    procedure :: generate_isr => phs_fks_generator_generate_isr
    procedure :: set_sqrts_hat => phs_fks_generator_set_sqrts_hat
    procedure :: set_emitters => phs_fks_generator_set_emitters
    procedure :: setup_masses => phs_fks_generator_setup_masses
    procedure :: set_isr_kinematics => phs_fks_generator_set_isr_kinematics
    procedure :: generate_radiation_variables => &
       phs_fks_generator_generate_radiation_variables
    procedure :: compute_xi_ref_momenta => phs_fks_generator_compute_xi_ref_momenta
    procedure :: compute_cms_energy => phs_fks_generator_compute_cms_energy
    procedure :: compute_xi_max => phs_fks_generator_compute_xi_max
    procedure :: compute_xi_max_isr_factorized &
       => phs_fks_generator_compute_xi_max_isr_factorized
    procedure :: set_masses => phs_fks_generator_set_masses
    procedure :: compute_y => phs_fks_generator_compute_y
    procedure :: compute_xi_tilde => phs_fks_generator_compute_xi_tilde
    procedure :: prepare_generation => phs_fks_generator_prepare_generation
    procedure :: generate_fsr_from_xi_and_y => &
       phs_fks_generator_generate_fsr_from_xi_and_y
    procedure :: get_radiation_variables => &
       phs_fks_generator_get_radiation_variables
    procedure :: get_jacobian => phs_fks_generator_get_jacobian
    procedure :: write => phs_fks_generator_write
  end type phs_fks_generator_t

  type, extends (phs_wood_t) :: phs_fks_t
    integer :: mode = PHS_MODE_UNDEFINED
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: q_born
    type(vector4_t), dimension(:), allocatable :: p_real
    type(vector4_t), dimension(:), allocatable :: q_real
    type(vector4_t), dimension(:), allocatable :: p_born_tot
    type(phs_fks_generator_t) :: generator
    logical :: perform_generation = .true.
    !!! Not entirley suited for combined integration
    !!! TODO: Modifiy global r_real-array
    real(default) :: r_isr
    type(phs_identifier_t), dimension(:), allocatable :: phs_identifiers

  contains
    procedure :: init => phs_fks_init
    procedure :: final => phs_fks_final
    procedure :: init_momenta => phs_fks_init_momenta
    procedure :: set_incoming_momenta => phs_fks_set_incoming_momenta
    procedure :: evaluate_selected_channel => phs_fks_evaluate_selected_channel
    procedure :: evaluate_other_channels => phs_fks_evaluate_other_channels
    procedure :: get_mcpar => phs_fks_get_mcpar
    procedure :: set_beam_energy => phs_fks_set_beam_energy
    procedure :: set_emitters => phs_fks_set_emitters
    procedure :: setup_masses => phs_fks_setup_masses
    procedure :: get_born_momenta => phs_fks_get_born_momenta
    procedure :: get_outgoing_momenta => phs_fks_get_outgoing_momenta
    procedure :: get_incoming_momenta => phs_fks_get_incoming_momenta
    procedure :: set_isr_kinematics => phs_fks_set_isr_kinematics
    procedure :: generate_radiation_variables => &
                         phs_fks_generate_radiation_variables
    procedure :: compute_xi_ref_momenta => phs_fks_compute_xi_ref_momenta
    procedure :: compute_cms_energy => phs_fks_compute_cms_energy
    procedure :: set_reference_frames => phs_fks_set_reference_frames
    procedure :: init_phs_identifiers => phs_fks_init_phs_identifiers
    procedure :: i_phs_is_isr => phs_fks_i_phs_is_isr
    procedure :: generate_fsr => phs_fks_generate_fsr
    procedure :: generate_isr => phs_fks_generate_isr
    procedure :: compute_isr_kinematics => phs_fks_compute_isr_kinematics
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
  end subroutine phs_fks_config_final

  subroutine phs_fks_config_write (object, unit)
    class(phs_fks_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call object%phs_wood_config_t%write (unit)
  end subroutine phs_fks_config_write

  subroutine phs_fks_config_set_mode (phs_config, mode)
    class(phs_fks_config_t), intent(inout) :: phs_config
    integer, intent(in) :: mode
    select case (mode)
    case (NLO_REAL, NLO_MISMATCH)
       phs_config%mode = PHS_MODE_ADDITIONAL_PARTICLE
    case (NLO_DGLAP)
       phs_config%mode = PHS_MODE_COLLINEAR_REMNANT
    end select
  end subroutine phs_fks_config_set_mode

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
    integer, intent(in), optional :: nlo_type
    if (.not. phs_config%extended_phs) then
       select case (phs_config%mode)
       case (PHS_MODE_ADDITIONAL_PARTICLE)
          phs_config%n_par = phs_config%n_par + 3
       case (PHS_MODE_COLLINEAR_REMNANT)
          phs_config%n_par = phs_config%n_par + 1
       end select
    end if
!!! Channel equivalences not accessible yet
    phs_config%provides_equivalences = .false.
  end subroutine phs_fks_config_configure

  subroutine phs_fks_config_startup_message (phs_config, unit)
    class(phs_fks_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    call phs_config%phs_wood_config_t%startup_message (unit)
  end subroutine phs_fks_config_startup_message

  subroutine phs_fks_config_allocate_instance (phs)
    class(phs_t), intent(inout), pointer :: phs
    allocate (phs_fks_t :: phs)
  end subroutine phs_fks_config_allocate_instance

  subroutine phs_fks_config_generate_phase_space_extra (phs_config)
    class(phs_fks_config_t), intent(inout) :: phs_config
    integer :: off_shell, extra_off_shell
    type(flavor_t), dimension(:,:), allocatable :: flv_born
    integer :: i, j
    integer :: n_state, n_flv_born
    allocate (phs_config%cascade_set)
    n_flv_born = size (phs_config%flv, 1) - 1
    n_state = size (phs_config%flv, 2)
    allocate (flv_born (n_flv_born, n_state))
    do i = 1, n_flv_born
       do j = 1, n_state
          flv_born(i, j) = phs_config%flv(i, j)
       end do
    end do
    off_shell = phs_config%par%off_shell
    do extra_off_shell = 0, max (n_flv_born - 2, 0)
       phs_config%par%off_shell = off_shell + extra_off_shell
       call cascade_set_generate (phs_config%cascade_set, &
          phs_config%model, phs_config%n_in, phs_config%n_out - 1, &
          flv_born, phs_config%par, phs_config%fatal_beam_decay)
       if (cascade_set_is_valid (phs_config%cascade_set)) exit
    end do
    if (.not. cascade_set_is_valid (phs_config%cascade_set)) &
       call msg_fatal ("Resonance extraction: Phase space generation failed")
  end subroutine phs_fks_config_generate_phase_space_extra

  subroutine phs_fks_config_set_born_config (phs_config, phs_cfg_born)
    class(phs_fks_config_t), intent(inout) :: phs_config
    type(phs_wood_config_t), intent(in), target :: phs_cfg_born
    call msg_debug (D_PHASESPACE, "phs_fks_config_set_born_config")
    phs_config%forest = phs_cfg_born%forest
    phs_config%n_channel = phs_cfg_born%n_channel
    allocate (phs_config%channel (phs_config%n_channel))
    phs_config%channel = phs_cfg_born%channel
    phs_config%n_par = phs_cfg_born%n_par
    phs_config%n_state = phs_cfg_born%n_state
    phs_config%sqrts = phs_cfg_born%sqrts
    phs_config%par = phs_cfg_born%par
    phs_config%sqrts_fixed = phs_cfg_born%sqrts_fixed
    phs_config%azimuthal_dependence = phs_cfg_born%azimuthal_dependence
    phs_config%provides_chains = phs_cfg_born%provides_chains
    phs_config%cm_frame = phs_cfg_born%cm_frame
    phs_config%vis_channels = phs_cfg_born%vis_channels
    allocate (phs_config%chain (size (phs_cfg_born%chain)))
    phs_config%chain = phs_cfg_born%chain
    phs_config%model => phs_cfg_born%model
    if (allocated (phs_cfg_born%cascade_set)) then
       allocate (phs_config%cascade_set)
       phs_config%cascade_set = phs_cfg_born%cascade_set
    end if
  end subroutine phs_fks_config_set_born_config

  function phs_fks_config_get_resonance_histories (phs_config) result (resonance_histories)
    type(resonance_history_t), dimension(:), allocatable :: resonance_histories
    class(phs_fks_config_t), intent(inout) :: phs_config
    if (allocated (phs_config%cascade_set)) then
       call cascade_set_get_resonance_histories &
          (phs_config%cascade_set, n_filter = 2, res_hists = resonance_histories)
    else
       call msg_debug (D_PHASESPACE, "Have to rebuild phase space for resonance histories")
       call phs_config%generate_phase_space_extra ()
       call cascade_set_get_resonance_histories &
          (phs_config%cascade_set, n_filter = 2, res_hists = resonance_histories)
    end if
  end function phs_fks_config_get_resonance_histories

  subroutine phs_fks_generator_connect_kinematics &
         (generator, isr_kinematics, real_kinematics, massive_phsp)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(isr_kinematics_t), intent(in), pointer :: isr_kinematics
    type(real_kinematics_t), intent(in), pointer :: real_kinematics
    logical, intent(in) :: massive_phsp
    generator%real_kinematics => real_kinematics
    generator%isr_kinematics => isr_kinematics
    generator%massive_phsp = massive_phsp
  end subroutine phs_fks_generator_connect_kinematics

  subroutine phs_fks_generator_compute_isr_kinematics (generator, r, p_in)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r
    type(vector4_t), dimension(2), intent(in), optional :: p_in
    integer :: em
    type(vector4_t), dimension(2) :: p

    if (present (p_in)) then
       p = p_in
    else
       p = generator%real_kinematics%p_born_lab%phs_point(1)%p(1:2)
    end if

    associate (isr => generator%isr_kinematics)
       do em = 1, 2
          isr%x(em) = p(em)%p(0) / isr%beam_energy
          isr%z(em) = one - (one - isr%x(em)) * r
          isr%jacobian(em) =  one - isr%x(em)
       end do
       isr%sqrts_born = (p(1) + p(2))**1
    end associate
  end subroutine phs_fks_generator_compute_isr_kinematics

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
      select type (phs_config)
      type is (phs_fks_config_t)
         phs%mode = phs_config%mode
      end select

      select case (phs%mode)
      case (PHS_MODE_ADDITIONAL_PARTICLE)
         phs%n_r_born = phs%config%n_par - 3
      case (PHS_MODE_COLLINEAR_REMNANT)
         phs%n_r_born = phs%config%n_par - 1
      end select
      call phs%init_momenta (phs_config)
    end select
  end subroutine phs_fks_init

  subroutine phs_fks_final (object)
    class(phs_fks_t), intent(inout) :: object
  end subroutine phs_fks_final

  subroutine phs_fks_init_momenta (phs, phs_config)
    class(phs_fks_t), intent(inout) :: phs
    class(phs_config_t), intent(in) :: phs_config
    allocate (phs%p_born (phs_config%n_in))
    allocate (phs%p_real (phs_config%n_in))
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       allocate (phs%q_born (phs_config%n_out-1))
       allocate (phs%q_real (phs_config%n_out-1))
       allocate (phs%p_born_tot (phs%config%n_in + phs%config%n_out-1))
    end select
  end subroutine phs_fks_init_momenta

  subroutine phs_fks_set_incoming_momenta (phs, p)
    class(phs_fks_t), intent(inout) :: phs
    type(vector4_t), dimension(:), intent(in) :: p
    call phs%phs_wood_t%set_incoming_momenta(p)
  end subroutine phs_fks_set_incoming_momenta

  subroutine phs_fks_evaluate_selected_channel (phs, c_in, r_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    real(default), intent(in), dimension(:) :: r_in
    integer :: n_in

    call phs%phs_wood_t%evaluate_selected_channel (c_in, r_in)
    phs%r(:,c_in) = r_in

    phs%q_defined = phs%phs_wood_t%q_defined
    if (.not. phs%q_defined) return

    if (phs%perform_generation) then
       select case (phs%mode)
       case (PHS_MODE_ADDITIONAL_PARTICLE)
          n_in = phs%config%n_in
          phs%p_born = phs%phs_wood_t%p
          phs%q_born = phs%phs_wood_t%q
          phs%p_born_tot (1: n_in) = phs%p_born
          phs%p_born_tot (n_in + 1 :) = phs%q_born
          call phs%set_reference_frames ()
          call phs%set_isr_kinematics ()
       case (PHS_MODE_COLLINEAR_REMNANT)
          call phs%compute_isr_kinematics (r_in(phs%n_r_born + 1))
          phs%r_isr = r_in(phs%n_r_born + 1)
       end select
    end if
  end subroutine phs_fks_evaluate_selected_channel

  subroutine phs_fks_evaluate_other_channels (phs, c_in)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: c_in
    call phs%phs_wood_t%evaluate_other_channels (c_in)
    phs%r_defined = .true.
  end subroutine phs_fks_evaluate_other_channels

  subroutine phs_fks_get_mcpar (phs, c, r)
    class(phs_fks_t), intent(in) :: phs
    integer, intent(in) :: c
    real(default), dimension(:), intent(out) :: r
    r(1 : phs%n_r_born) = phs%r(1 : phs%n_r_born,c)
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       r(phs%n_r_born + 1 :) = phs%r_real
    case (PHS_MODE_COLLINEAR_REMNANT)
       r(phs%n_r_born + 1 :) = phs%r_isr
    end select
  end subroutine phs_fks_get_mcpar

  subroutine phs_fks_set_beam_energy (phs)
    class(phs_fks_t), intent(inout) :: phs
    call phs%generator%set_sqrts_hat (phs%config%sqrts)
  end subroutine phs_fks_set_beam_energy

  subroutine phs_fks_set_emitters (phs, emitters)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in), dimension(:), allocatable :: emitters
    call phs%generator%set_emitters (emitters)
  end subroutine phs_fks_set_emitters

  subroutine phs_fks_setup_masses (phs, n_tot)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: n_tot
    call phs%generator%setup_masses (n_tot)
  end subroutine phs_fks_setup_masses

  subroutine phs_fks_get_born_momenta (phs, p)
    class(phs_fks_t), intent(inout) :: phs
    type(vector4_t), intent(out), dimension(:) :: p
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       p(1 : phs%config%n_in) = phs%p_born
       p(phs%config%n_in + 1 :) = phs%q_born
    case (PHS_MODE_COLLINEAR_REMNANT)
       p(1:phs%config%n_in) = phs%phs_wood_t%p
       p(phs%config%n_in + 1 : ) = phs%phs_wood_t%q
    end select
    if (.not. phs%config%cm_frame) p = phs%lt_cm_to_lab * p
  end subroutine phs_fks_get_born_momenta

  subroutine phs_fks_get_outgoing_momenta (phs, q)
    class(phs_fks_t), intent(in) :: phs
    type(vector4_t), intent(out), dimension(:) :: q
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       q = phs%q_real
    case (PHS_MODE_COLLINEAR_REMNANT)
       q = phs%phs_wood_t%q
    end select
  end subroutine phs_fks_get_outgoing_momenta

  subroutine phs_fks_get_incoming_momenta (phs, p)
    class(phs_fks_t), intent(in) :: phs
    type(vector4_t), intent(inout), dimension(:), allocatable :: p
    p = phs%p_real
  end subroutine phs_fks_get_incoming_momenta

  subroutine phs_fks_set_isr_kinematics (phs, p_born)
    class(phs_fks_t), intent(inout) :: phs
    type(vector4_t), dimension(2), intent(in), optional :: p_born
    call phs%generator%set_isr_kinematics (p_born)
  end subroutine phs_fks_set_isr_kinematics

  subroutine phs_fks_generate_radiation_variables (phs, r_in)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(in), dimension(:) :: r_in
    if (size (r_in) /= 3) call msg_fatal &
         ("Real kinematics need to be generated using three random numbers!")
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       call phs%generator%generate_radiation_variables (r_in, phs%p_born_tot, &
          phs%phs_identifiers)
       phs%r_real = r_in
    end select
  end subroutine phs_fks_generate_radiation_variables

  subroutine phs_fks_compute_xi_ref_momenta (phs, contributors)
    class(phs_fks_t), intent(inout) :: phs
    type(resonance_contributors_t), intent(in), dimension(:), optional :: contributors
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       call phs%generator%compute_xi_ref_momenta (phs%p_born_tot, contributors)
    end select
  end subroutine phs_fks_compute_xi_ref_momenta
    
  subroutine phs_fks_compute_cms_energy (phs)
    class(phs_fks_t), intent(inout) :: phs
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       call phs%generator%compute_cms_energy (phs%p_born_tot)
    end select
  end subroutine phs_fks_compute_cms_energy

  subroutine phs_fks_set_reference_frames (phs)
    class(phs_fks_t), intent(inout) :: phs
    type(lorentz_transformation_t) :: lt_cm_to_lab
    associate (real_kinematics => phs%generator%real_kinematics)
       real_kinematics%p_born_cms%phs_point(1)%p = phs%p_born_tot
       if (.not. phs%config%cm_frame) then
          !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
          lt_cm_to_lab = phs%lt_cm_to_lab
          real_kinematics%p_born_lab%phs_point(1)%p = &
             lt_cm_to_lab * phs%p_born_tot
       else
          real_kinematics%p_born_lab%phs_point(1)%p = phs%p_born_tot
       end if
    end associate
  end subroutine phs_fks_set_reference_frames

  subroutine phs_fks_init_phs_identifiers (phs, reg_data)
    class(phs_fks_t), intent(inout) :: phs
    type(region_data_t), intent(in) :: reg_data
    integer :: i_em, i_res, i_phs
    integer :: emitter
    type(resonance_contributors_t) :: contributors
    logical :: share_emitter, phs_exist
    allocate (phs%phs_identifiers (reg_data%n_phs))
    do i_em = 1, size (reg_data%emitters)
       emitter = reg_data%emitters(i_em)
       if (allocated (reg_data%resonances)) then
          do i_res = 1, size (reg_data%resonances)
             !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
             call reg_data%get_contributors (i_res, emitter, contributors%c, share_emitter)
             if (.not. share_emitter) cycle
             call check_for_phs_identifier &
                (phs%phs_identifiers, phs%config%n_in, emitter, contributors%c, phs_exist, i_phs)
             if (.not. phs_exist) &
                call phs%phs_identifiers(i_phs)%init (emitter, contributors%c)
             if (allocated (contributors%c)) deallocate (contributors%c)
          end do
       else
          call check_for_phs_identifier &
             (phs%phs_identifiers, phs%config%n_in, emitter, &
             phs_exist = phs_exist, i_phs = i_phs)
          if (.not. phs_exist) &
             call phs%phs_identifiers(i_phs)%init (emitter)
       end if
    end do
  end subroutine phs_fks_init_phs_identifiers

  function phs_fks_i_phs_is_isr (phs, i_phs) result (is_isr)
    logical :: is_isr
    class(phs_fks_t), intent(in) :: phs
    integer, intent(in) :: i_phs
    is_isr = phs%phs_identifiers(i_phs)%emitter <= phs%generator%n_in
  end function phs_fks_i_phs_is_isr

  subroutine phs_fks_generator_generate_fsr_default (generator, emitter, i_phs, p_born, p_real)
    !!! Important: Momenta must be input in the center-of-mass frame
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: emitter, i_phs
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    real(default) :: q0
    integer :: i, nlegreal

    nlegreal = size (p_born) + 1
    call generator%generate_fsr_in (p_born, p_real)
    q0 = generator%real_kinematics%xi_ref_momenta(1)**1

    generator%i_fsr_first = generator%n_in + 1
    call generator%generate_fsr_out (emitter, i_phs, p_born, p_real, q0)
    if (debug_active (D_PHASESPACE)) then
       call vector4_check_momentum_conservation (p_real%p, generator%n_in, &
           rel_smallness = 1000 * tiny_07, abs_smallness = tiny_07)
    end if
  end subroutine phs_fks_generator_generate_fsr_default

  subroutine phs_fks_generator_generate_fsr_resonances (generator, &
       emitter, i_phs, i_con, p_born, p_real)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: emitter, i_phs
    integer, intent(in) :: i_con
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    integer, dimension(:), allocatable :: resonance_list
    integer, dimension(size(p_born)) :: inv_resonance_list
    type(vector4_t), dimension(:), allocatable :: p_tmp_born
    type(phs_point_t) :: p_tmp_real
    type(vector4_t) :: p_resonance
    real(default) :: q0
    integer :: i, j, nlegborn, nlegreal
    integer :: i_emitter
    type(lorentz_transformation_t) :: boost_to_resonance
    integer :: n_resonant_particles
    call msg_debug2 (D_PHASESPACE, "phs_fks_generator_generate_fsr_resonances")
    nlegborn = size (p_born); nlegreal = nlegborn + 1
    allocate (resonance_list (size (generator%resonance_contributors(i_con)%c)))
    resonance_list = generator%resonance_contributors(i_con)%c
    n_resonant_particles = size (resonance_list)

    if (.not. any (resonance_list == emitter)) then
       call msg_fatal ("Emitter must be included in the resonance list!")
    else
       do i = 1, n_resonant_particles
          if (resonance_list (i) == emitter) i_emitter = i
       end do
    end if

    inv_resonance_list = &
       create_inverse_resonance_list (nlegborn, resonance_list)

    p_tmp_real = n_resonant_particles + 1
    allocate (p_tmp_born (n_resonant_particles))
    p_tmp_born = vector4_null
    j = 1
    do i = 1, n_resonant_particles
       p_tmp_born(j) = p_born (resonance_list(i))
       j = j + 1
    end do

    call generator%generate_fsr_in (p_born, p_real)

    p_resonance = generator%real_kinematics%xi_ref_momenta(i_con)
    q0 = p_resonance**1

    boost_to_resonance = inverse (boost (p_resonance, q0))
    p_tmp_born = boost_to_resonance * p_tmp_born

    generator%i_fsr_first = 1
    call generator%generate_fsr_out (emitter, i_phs, p_tmp_born, p_tmp_real, q0, i_emitter)
    p_tmp_real = inverse (boost_to_resonance) * p_tmp_real

    do i = generator%n_in + 1, nlegborn
       if (any (resonance_list == i)) then
          p_real%p(i) = p_tmp_real%p(inv_resonance_list (i))
       else
          p_real%p(i) = p_born (i)
       end if
    end do
    p_real%p(nlegreal) = p_tmp_real%p (n_resonant_particles + 1)

    if (debug_active (D_PHASESPACE)) then
       call vector4_check_momentum_conservation (p_real%p, generator%n_in, &
           rel_smallness = 1000 * tiny_07, abs_smallness = tiny_07)
    end if

  contains

    function create_inverse_resonance_list (nlegborn, resonance_list) &
       result (inv_resonance_list)
       integer, intent(in) :: nlegborn
       integer, intent(in), dimension(:) :: resonance_list
       integer, dimension(nlegborn) :: inv_resonance_list
       integer :: i, j
       inv_resonance_list = 0
       j = 1
       do i = 1, nlegborn
          if (any (i == resonance_list)) then
             inv_resonance_list (i) = j
             j = j + 1
          end if
       end do
    end function create_inverse_resonance_list

    function boosted_energy () result (E)
      real(default) :: E
      type(vector4_t) :: p_boost
      p_boost = boost_to_resonance * p_resonance
      E = p_boost%p(0)
    end function boosted_energy
  end subroutine phs_fks_generator_generate_fsr_resonances

  subroutine phs_fks_generator_generate_fsr_in (generator, p_born, p_real)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    integer :: i
    do i = 1, generator%n_in
       p_real%p(i) = p_born(i)
    end do
  end subroutine phs_fks_generator_generate_fsr_in

  subroutine phs_fks_generator_generate_fsr_out (generator, &
      emitter, i_phs, p_born, p_real, q0, p_emitter_index)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: emitter, i_phs
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    real(default), intent(in) :: q0
    integer, intent(in), optional :: p_emitter_index
    real(default) :: xi, y, phi
    integer :: nlegborn, nlegreal
    real(default) :: uk_np1, uk_n
    real(default) :: uk_rec, k_rec0
    type(vector3_t) :: k_n_born, k
    real(default) :: uk_n_born, uk, k2, k0_n
    real(default) :: cpsi, beta
    type(vector3_t) :: vec, vec_orth
    type(lorentz_transformation_t) :: rot, lambda
    integer :: i
    integer :: p_em
    p_em = emitter; if (present (p_emitter_index)) p_em = p_emitter_index
    if (generator%i_fsr_first < 0) &
       call msg_fatal ("FSR generator is called for outgoing particles but "&
          &"i_fsr_first is not set!")

    associate (rad_var => generator%real_kinematics)
       xi = rad_var%xi_tilde
       if (rad_var%supply_xi_max) xi = xi*rad_var%xi_max(i_phs)
       y = rad_var%y(i_phs)
       phi = rad_var%phi
    end associate

    nlegborn = size (p_born)
    nlegreal = nlegborn + 1
    generator%E_gluon = q0 * xi / two
    uk_np1 = generator%E_gluon
    k_n_born = p_born(p_em)%p(1:3)
    uk_n_born = k_n_born**1

    generator%mrec2 = (q0 - p_born(p_em)%p(0))**2 &
       - space_part_norm(p_born(p_em))**2
    if (generator%is_massive(emitter)) then
       call generator%compute_emitter_kinematics &
          (y, emitter, i_phs, q0, k0_n, uk_n, uk)
    else
       call generator%compute_emitter_kinematics (y, q0, uk_n, uk)
       generator%real_kinematics%y_soft(i_phs) = y
       k0_n = uk_n
    end if

    call msg_debug2 (D_PHASESPACE, "phs_fks_generator_generate_fsr_out")
    call debug_input_values ()

    vec = uk_n / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real%p(p_em)%p(0) = k0_n
    p_real%p(p_em)%p(1:3) = vec%p(1:3)
    cpsi = (uk_n**2 + uk**2 - uk_np1**2) / (two * uk_n * uk)
    !!! This is to catch the case where cpsi = 1, but numerically
    !!! turns out to be slightly larger than 1.
    call check_cpsi_bound (cpsi)
    rot = rotation (cpsi, - sqrt (one - cpsi**2), vec_orth)
    p_real%p(p_em) = rot * p_real%p(p_em)
    vec = uk_np1 / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real%p(nlegreal)%p(0) = uk_np1
    p_real%p(nlegreal)%p(1:3) = vec%p(1:3)
    cpsi = (uk_np1**2 + uk**2 - uk_n**2) / (two * uk_np1 * uk)
    call check_cpsi_bound (cpsi)
    rot = rotation (cpsi, sqrt (one - cpsi**2), vec_orth)
    p_real%p(nlegreal) = rot * p_real%p(nlegreal)
    call construct_recoiling_momenta ()
    call generate_jacobians ()

  contains

  subroutine debug_input_values ()
    if (debug2_active (D_PHASESPACE)) then
       call generator%write ()
       print *, 'emitter =    ', emitter
       print *, 'p_born:'
       call vector4_write_set (p_born)
       print *, 'p_real:'
       call p_real%write()
       print *, 'q0 =    ', q0
       if (present(p_emitter_index)) then
          print *, 'p_emitter_index =    ', p_emitter_index
       else
          print *, 'p_emitter_index not given'
       end if
    end if
  end subroutine debug_input_values

  subroutine check_cpsi_bound (cpsi)
    real(default), intent(inout) :: cpsi
    if (cpsi > one) then
       cpsi = one
    else if (cpsi < -one) then
       cpsi = - one
    end if
  end subroutine check_cpsi_bound

  subroutine construct_recoiling_momenta ()
    k_rec0 = q0 - p_real%p(p_em)%p(0) - p_real%p(nlegreal)%p(0)
    uk_rec = sqrt (k_rec0**2 - generator%mrec2)
    if (generator%is_massive(emitter)) then
       beta = compute_beta (q0**2, k_rec0, uk_rec, &
          p_born(p_em)%p(0), uk_n_born)
    else
       beta = compute_beta (q0**2, k_rec0, uk_rec)
    end if
    k = p_real%p(p_em)%p(1:3) + p_real%p(nlegreal)%p(1:3)
    vec%p(1:3) = one / uk * k%p(1:3)
    lambda = boost (beta / sqrt(one - beta**2), vec)
    do i = generator%i_fsr_first, nlegborn
      if (i /= p_em) then
         p_real%p(i) = lambda * p_born(i)
      end if
    end do
    vec%p(1:3) = p_born(p_em)%p(1:3) / uk_n_born
    rot = rotation (cos(phi), sin(phi), vec)
    p_real%p(nlegreal) = rot * p_real%p(nlegreal)
    p_real%p(p_em) = rot * p_real%p(p_em)
  end subroutine construct_recoiling_momenta

  subroutine generate_jacobians ()
    associate (jac => generator%real_kinematics%jac(i_phs))
       if (generator%is_massive(emitter)) then
          jac%jac(1) = jac%jac(1) * 4 / q0 / uk_n_born / xi
       else
          k2 = two * uk_n * uk_np1* (one - y)
          jac%jac(1) = uk_n**2 / uk_n_born / (uk_n - k2 / (two * q0))
       end if
       jac%jac(2) = one
       jac%jac(3) = one - xi / two * q0 / uk_n_born
    end associate
  end subroutine generate_jacobians


  end subroutine phs_fks_generator_generate_fsr_out

  subroutine phs_fks_generate_fsr (phs, emitter, i_phs, p_real, i_con)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: emitter, i_phs
    type(phs_point_t), intent(inout) :: p_real
    integer, intent(in), optional :: i_con
    type(vector4_t), dimension(:), allocatable :: p
    associate (generator => phs%generator)
       allocate (p (1 : generator%real_kinematics%p_born_cms%get_n_particles()), &
          source = generator%real_kinematics%p_born_cms%phs_point(1)%p)
       generator%real_kinematics%supply_xi_max = .true.
       call generator%compute_xi_max (emitter, i_phs, p, i_con)
       if (present (i_con)) then
          call generator%generate_fsr (emitter, i_phs, i_con, p, p_real)
       else
          call generator%generate_fsr (emitter, i_phs, p, p_real)
       end if
       generator%real_kinematics%p_real_cms%phs_point(i_phs)%p = p_real%p
       if (.not. phs%config%cm_frame) p_real = phs%lt_cm_to_lab * p_real
       generator%real_kinematics%p_real_lab%phs_point(i_phs)%p = p_real%p
    end associate
  end subroutine phs_fks_generate_fsr

  subroutine phs_fks_generator_compute_emitter_kinematics_massless &
     (generator, y, q0, uk_em, uk)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: y, q0
    real(default), intent(out) :: uk_em, uk
    real(default) :: k0_np1, q2

    k0_np1 = generator%E_gluon
    q2 = q0**2

    uk_em = (q2 - generator%mrec2 - two * q0 * k0_np1) / (two * (q0 - k0_np1 * (one - y)))
    uk = sqrt (uk_em**2 + k0_np1**2 + two * uk_em * k0_np1 * y)
  end subroutine phs_fks_generator_compute_emitter_kinematics_massless

  subroutine phs_fks_generator_compute_emitter_kinematics_massive &
                                    (generator, y, em, i_phs, q0, k0_em, uk_em, uk)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: y
    integer, intent(in) :: em, i_phs
    real(default), intent(in) :: q0
    real(default), intent(inout) :: k0_em, uk_em, uk
    real(default) :: k0_np1, q2, mrec2, m2
    real(default) :: k0_rec_max, k0_em_max, k0_rec, uk_rec
    real(default) :: z, z1, z2

    k0_np1 = generator%E_gluon
    q2 = q0**2
    mrec2 = generator%mrec2
    m2 = generator%m2(em)

    k0_rec_max = (q2 - m2 + mrec2) / (two * q0)
    k0_em_max = (q2 + m2 - mrec2)  /(two * q0)
    z1 = (k0_rec_max + sqrt (k0_rec_max**2 - mrec2)) / q0
    z2 = (k0_rec_max - sqrt (k0_rec_max**2 - mrec2)) / q0
    z = z2 - (z2 - z1) * (one + y) / 2
    k0_em = k0_em_max - k0_np1 * z
    k0_rec = q0 - k0_np1 - k0_em
    uk_em = sqrt(k0_em**2 - m2)
    uk_rec = sqrt(k0_rec**2 - mrec2)
    uk = uk_rec
    generator%real_kinematics%jac(i_phs)%jac = q0 * (z1 - z2) / 4 * k0_np1
    generator%real_kinematics%y_soft(i_phs) = &
       (two * q2 * z - q2 - mrec2 + m2) / (sqrt(k0_em_max**2 - m2) * q0) / two
  end subroutine phs_fks_generator_compute_emitter_kinematics_massive

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
    alpha = (k0_rec + uk_rec) / (k0_rec_born + uk_rec_born)
    beta = (one - alpha**2) / (one + alpha**2)
  end function compute_beta_massive

  pure function get_xi_max_fsr_massless (p_born, q0, emitter) result (xi_max)
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: q0
    integer, intent(in) :: emitter
    real(default) :: xi_max
    real(default) :: uk_n_born
    uk_n_born = space_part_norm (p_born(emitter))
    xi_max = two * uk_n_born / q0
  end function get_xi_max_fsr_massless

  pure function get_xi_max_fsr_massive (p_born, q0, emitter, m2, y) result (xi_max)
    real(default) :: xi_max
    type(vector4_t), intent(in), dimension(:) :: p_born
    real(default), intent(in) :: q0
    integer, intent(in) :: emitter
    real(default), intent(in) :: m2, y
    real(default) :: mrec2
    real(default) :: k0_rec_max
    real(default) :: z, z1, z2
    real(default) :: k_np1_max
    associate (p => p_born(emitter)%p)
       mrec2 = (q0 - p(0))**2 - p(1)**2 - p(2)**2 - p(3)**2
    end associate
    call compute_dalitz_bounds (q0, m2, mrec2, z1, z2, k0_rec_max)
    z = z2 - (z2 - z1) * (one + y) / two
    k_np1_max = - (q0**2 * z**2 - two * q0 * k0_rec_max * z + mrec2) &
       / (two * q0 * z * (one - z))
    xi_max = two * k_np1_max / q0
  end function get_xi_max_fsr_massive

  function get_xi_max_isr (xb, y) result (xi_max)
    real(default), dimension(2), intent(in) :: xb
    real(default), intent(in) :: y
    real(default) :: xb_plus, xb_minus
    real(default) :: xi_max
    real(default) :: plus_val, minus_val
    real(default) :: onepy, onemy

    xb_plus = xb(I_PLUS); xb_minus = xb(I_MINUS)
    onepy = one + y; onemy = one - y

    plus_val = two * onepy * xb_plus**2 / &
               (sqrt ((one + xb_plus**2)**2 * onemy**2 + 16 * y * xb_plus**2) &
               + onemy * (one - xb_plus**2))
    minus_val = two * onemy * xb_minus**2 / &
                (sqrt ((one + xb_minus**2)**2 * onepy**2 - 16 * y * xb_minus**2) &
               + onepy * (one - xb_minus**2))
    xi_max = one - max (plus_val, minus_val)
  end function get_xi_max_isr

  recursive function get_xi_max_isr_decay (p) result (xi_max)
     real(default) :: xi_max
     type(vector4_t), dimension(:), intent(in) :: p
     integer :: n_tot
     type(vector4_t), dimension(:), allocatable :: p_dec_new
     n_tot = size (p)
     if (n_tot == 3) then
        xi_max = xi_max_one_to_two (p(1), p(2), p(3))
     else
        allocate (p_dec_new (n_tot - 1))
        p_dec_new(1) = sum (p (3 : ))
        p_dec_new(2 : n_tot - 1) = p (3 : n_tot)
        xi_max = min (xi_max_one_to_two (p(1), p(2), sum(p(3 : ))), &
           get_xi_max_isr_decay (p_dec_new))
     end if 
  contains
    function xi_max_one_to_two (p_in, p_out1, p_out2) result (xi_max)
      real(default) :: xi_max
      type(vector4_t), intent(in) :: p_in, p_out1, p_out2
      real(default) :: m_in, m_out1, m_out2
      m_in = p_in**1
      m_out1 = p_out1**1; m_out2 = p_out2**1
      xi_max = one - (m_out1 + m_out2)**2 / m_in**2
    end function xi_max_one_to_two
  end function get_xi_max_isr_decay

  subroutine phs_fks_generate_isr (phs, i_phs, p_born, p_real)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: i_phs
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    type(vector4_t) :: p0, p1
    type(lorentz_transformation_t) :: lt
    real(default) :: sqrts_hat

    associate (generator => phs%generator)
       select case (generator%n_in)
       case (1)
          call generator%compute_xi_max (1, i_phs, p_born)
          call generator%generate_isr_decay (i_phs, p_born, p_real)
          phs%config%cm_frame = .true.
       case (2)
          call generator%compute_xi_max (1, i_phs, p_born)
          call generator%compute_xi_max (2, i_phs, p_born)
          call generator%generate_isr (i_phs, p_born, p_real)
       end select
       phs%generator%real_kinematics%p_real_lab%phs_point(i_phs)%p = p_real%p
       if (.not. phs%config%cm_frame) then
          sqrts_hat = (p_real%p(1) + p_real%p(2))**1
          p0 = p_real%p(1) + p_real%p(2)
          lt = boost (p0, sqrts_hat)
          p1 = inverse(lt) * p_real%p(1)
          lt = lt * rotation_to_2nd (3, space_part (p1))
          phs%generator%real_kinematics%p_real_cms%phs_point(i_phs)%p = &
               inverse (lt) * p_real%p
       else
          phs%generator%real_kinematics%p_real_cms%phs_point(i_phs)%p = p_real%p
       end if
     end associate
  end subroutine phs_fks_generate_isr

  subroutine phs_fks_generator_generate_isr_decay (generator, i_phs, p_born, p_real)
     class(phs_fks_generator_t), intent(inout) :: generator
     integer, intent(in) :: i_phs
     type(vector4_t), intent(in), dimension(:) :: p_born
     type(phs_point_t), intent(inout) :: p_real
     real(default) :: xi_max, xi, y, phi
     integer :: nlegborn, nlegreal
     real(default) :: k0_np1
     real(default) :: msq_in
     real(default) :: msq, msq1, msq2, m, p, E
     real(default) :: rlda, rlda_soft
     type(vector4_t) :: p_virt
     real(default) :: theta_born, phi_born
     type(lorentz_transformation_t) :: L, rotation

    associate (rad_var => generator%real_kinematics)
      xi_max = rad_var%xi_max(i_phs)
      xi = rad_var%xi_tilde * xi_max
      y = rad_var%y(i_phs)
      phi = rad_var%phi
      rad_var%y_soft(i_phs) = y
    end associate

    nlegborn = size (p_born)
    nlegreal = nlegborn + 1
         
    msq_in = p_born(1)**2
    generator%real_kinematics%jac(i_phs)%jac = one

    p_real%p(1) = p_born(1)
    k0_np1 = p_real%p(1)%p(0) * xi / two
    p_real%p(nlegreal)%p(0) = k0_np1
    p_real%p(nlegreal)%p(1) = k0_np1 * sqrt(one - y**2) * sin(phi)
    p_real%p(nlegreal)%p(2) = k0_np1 * sqrt(one - y**2) * cos(phi)
    p_real%p(nlegreal)%p(3) = k0_np1 * y

    p_virt = p_real%p(1) - p_real%p(nlegreal)
    call evaluate_splittings (p_virt, p_born(2 : nlegborn), &
       p_real%p(2 : nlegreal - 1), 1)

  contains

    recursive subroutine evaluate_splittings (p_dec, p_in, p_out, i_real)
      type(vector4_t), intent(in) :: p_dec
      type(vector4_t), intent(in), dimension(:) :: p_in
      type(vector4_t), intent(inout), dimension(:) :: p_out
      integer, intent(in) :: i_real
      type(vector4_t) :: p_dec_new 
      integer :: n_recoil
      n_recoil = size (p_in) - 1
      if (n_recoil > 1) then
         call evaluate_one_to_two_splitting (p_dec, p_in(1), sum (p_in (2 : n_recoil + 1)), &
            p_out(i_real), p_dec_new)
         call evaluate_splittings (p_dec_new, p_in (2 : ), p_out, i_real + 1) 
      else
         call evaluate_one_to_two_splitting (p_dec, p_in(1), p_in(2), &
            p_out(i_real), p_out(i_real + 1))
      end if
    end subroutine evaluate_splittings
    
    subroutine evaluate_one_to_two_splitting (p_origin, p1_in, p2_in, p1_out, p2_out)
      type(vector4_t), intent(in) :: p_origin
      type(vector4_t), intent(in) :: p1_in, p2_in
      type(vector4_t), intent(inout) :: p1_out, p2_out
      type(lorentz_transformation_t) :: L, L_rest
      type(vector4_t) :: p1_rest, p2_rest
      real(default) :: msq, msq1, msq2, m
      real(default) :: E, p
      type(vector3_t) :: vec

      L = boost (p_origin, p_origin**1)
      L_rest = boost (p1_in + p2_in, (p1_in + p2_in)**1)

      p1_rest = inverse (L_rest) * p1_in
      p2_rest = inverse (L_rest) * p2_in

      msq = p_origin**2; m = sqrt(msq)
      msq1 = p1_in**2
      msq2 = p2_in**2
      rlda = sqrt (lambda (msq, msq1, msq2))
      p = rlda / (two * m)

      E = sqrt (msq1 + p**2)
      vec = p1_rest%p(1:3) / space_part_norm (p1_rest)
      p1_out = vector4_moving (E, p * vec) 
  
      E = sqrt (msq2 + p**2)
      vec = p2_rest%p(1:3) / space_part_norm (p2_rest)
      p2_out = vector4_moving (E, p * vec) 

      p1_out = L  * p1_out
      p2_out = L  * p2_out

      associate (jac => generator%real_kinematics%jac(i_phs))
         jac%jac(1) = jac%jac(1) * rlda / msq
         rlda_soft = sqrt (lambda (msq_in, msq1, msq2))
         !!! We have to undo the Jacobian which has already been
         !!! supplied by the Born phase space.
         jac%jac(1) = jac%jac(1) * msq_in / rlda_soft
         jac%jac(2) = one
      end associate

    end subroutine evaluate_one_to_two_splitting
  end subroutine phs_fks_generator_generate_isr_decay

  subroutine phs_fks_generator_generate_isr_factorized (generator, i_phs, emitter, p_born, p_real)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: i_phs, emitter
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    type(vector4_t), dimension(:), allocatable :: p_tmp_born
    type(phs_point_t) :: p_tmp_real
    type(vector4_t) :: p_top
    type(lorentz_transformation_t) :: boost_to_rest_frame
    integer, parameter :: nlegreal = 7 !!! Factorized phase space so far only required for ee -> bwbw
     
    allocate (p_tmp_born (3)); p_tmp_born = vector4_null
    p_tmp_real = 4
    p_real%p(1:2) = p_born(1:2)
    if (emitter == THR_POS_B) then
       p_top = p_born (THR_POS_WP) + p_born (THR_POS_B)
       p_tmp_born(2) = p_born (THR_POS_WP)
       p_tmp_born(3) = p_born (THR_POS_B)
    else if (emitter == THR_POS_BBAR) then
       p_top = p_born (THR_POS_WM) + p_born (THR_POS_BBAR) 
       p_tmp_born(2) = p_born (THR_POS_WM)
       p_tmp_born(3) = p_born (THR_POS_BBAR)
    else
       call msg_fatal ("Threshold computation requires emitters to be at position 5 and 6 " // &
          "Please check if your process specification fulfills this requirement.")
    end if
    p_tmp_born (1) = p_top
    boost_to_rest_frame = inverse (boost (p_top, p_top**1))
    p_tmp_born = boost_to_rest_frame * p_tmp_born
    call generator%compute_xi_max_isr_factorized (i_phs, p_tmp_born)
    call generator%generate_isr_decay (i_phs, p_tmp_born, p_tmp_real)
    p_tmp_real = inverse (boost_to_rest_frame) * p_tmp_real
    if (emitter == THR_POS_B) then
       p_real%p(THR_POS_WP) = p_tmp_real%p (2)
       p_real%p(THR_POS_B) = p_tmp_real%p (3)
       p_real%p(THR_POS_WM) = p_born (THR_POS_WM)
       p_real%p(THR_POS_BBAR) = p_born (THR_POS_BBAR)
    !!! Exception has been handled above
    else
       p_real%p(THR_POS_WM) = p_tmp_real%p (2)
       p_real%p(THR_POS_BBAR) = p_tmp_real%p (3)
       p_real%p(THR_POS_WP) = p_born (THR_POS_WP)
       p_real%p(THR_POS_B) = p_born (THR_POS_B)
    end if
    p_real%p(nlegreal) = p_tmp_real%p (4)
  end subroutine phs_fks_generator_generate_isr_factorized
       
  subroutine phs_fks_generator_generate_isr &
       (generator, i_phs, p_born, p_real)
    !!! Important: Import momenta in the lab frame
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: i_phs
    type(vector4_t), intent(in) , dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    real(default) :: xi_max, xi_tilde, xi, y, phi
    integer :: nlegborn, nlegreal
    real(default) :: sqrts_real
    real(default) :: k0_np1
    type(lorentz_transformation_t) :: lambda_transv, lambda_longit, lambda_longit_inv
    real(default) :: x_plus, x_minus, xb_plus, xb_minus
    real(default) :: onemy, onepy
    integer :: i
    real(default) :: xi_plus, xi_minus
    real(default) :: beta_gamma
    type(vector3_t) :: beta_vec

    associate (rad_var => generator%real_kinematics)
      xi_max = rad_var%xi_max(i_phs)
      xi_tilde = rad_var%xi_tilde
      xi = xi_tilde * xi_max
      y = rad_var%y(i_phs)
      onemy = one - y; onepy = one + y
      phi = rad_var%phi
      rad_var%y_soft(i_phs) = y
    end associate

    nlegborn = size (p_born)
    nlegreal = nlegborn + 1
    generator%isr_kinematics%sqrts_born = (p_born(1) + p_born(2))**1

    !!! Initial state real momenta
    xb_plus = generator%isr_kinematics%x(I_PLUS)
    xb_minus = generator%isr_kinematics%x(I_MINUS)
    x_plus = xb_plus / sqrt(one - xi) * sqrt ((two - xi * onemy) / (two - xi * onepy))
    x_minus = xb_minus / sqrt(one - xi) * sqrt ((two - xi * onepy) / (two - xi * onemy))
    xi_plus = xi_tilde * (one - xb_plus)
    xi_minus = xi_tilde * (one - xb_minus)
    p_real%p(I_PLUS) = x_plus / xb_plus * p_born(I_PLUS)
    p_real%p(I_MINUS) = x_minus / xb_minus * p_born(I_MINUS)
    generator%isr_kinematics%z(I_PLUS) = x_plus / xb_plus
    generator%isr_kinematics%z(I_MINUS) = x_minus / xb_minus
    generator%isr_kinematics%z_coll(I_PLUS) = one / (one - xi_plus)
    generator%isr_kinematics%z_coll(I_MINUS) = one / (one - xi_minus)

    !!! Create radiation momentum
    sqrts_real = generator%isr_kinematics%sqrts_born / sqrt (one - xi)
    k0_np1 = sqrts_real * xi / two
    p_real%p(nlegreal)%p(0) = k0_np1
    p_real%p(nlegreal)%p(1) = k0_np1 * sqrt (one - y**2) * sin(phi)
    p_real%p(nlegreal)%p(2) = k0_np1 * sqrt (one - y**2) * cos(phi)
    p_real%p(nlegreal)%p(3) = k0_np1 * y

    call get_boost_parameters (p_real%p, beta_gamma, beta_vec)
    lambda_longit = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .true.)
    p_real%p(nlegreal) = lambda_longit * p_real%p(nlegreal)

    call get_boost_parameters (p_born, beta_gamma, beta_vec)
    lambda_longit = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .false.)
    forall (i = 3 : nlegborn) &
        p_real%p(i) = lambda_longit * p_born(i)

    lambda_transv = create_transversal_boost (p_real%p(nlegreal), xi, sqrts_real)
    forall (i = 3 : nlegborn) &
         p_real%p(i) = lambda_transv * p_real%p(i)

    lambda_longit_inv = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .true.)
    forall (i = 3 : nlegborn) &
         p_real%p(i) = lambda_longit_inv * p_real%p(i)

    !!! Compute jacobians
    associate (jac => generator%real_kinematics%jac(i_phs))
       !!! Additional 1 / (1 -xi) factor because in the real jacobian,
       !!! there is s_real in the numerator
       !!! We also have to adapt the flux factor, which is 1/2s_real for the real component
       !!! The reweighting factor is s_born / s_real, cancelling the (1-x) factor from above
       jac%jac(1) = one / (one - xi)
       jac%jac(2) = one
       jac%jac(3) = one / (one - xi_plus)**2
       jac%jac(4) = one / (one - xi_minus)**2
    end associate
  contains
    subroutine get_boost_parameters (p, beta_gamma, beta_vec)
       type(vector4_t), intent(in), dimension(:) :: p
       real(default), intent(out) :: beta_gamma
       type(vector3_t), intent(out) :: beta_vec
       beta_vec = (p(1)%p(1:3) + p(2)%p(1:3)) / (p(1)%p(0) + p(2)%p(0))
       beta_gamma = beta_vec**1 / sqrt (one - beta_vec**2)
       beta_vec = beta_vec / beta_vec**1
    end subroutine get_boost_parameters

    function create_longitudinal_boost (beta_gamma, beta_vec, inverse) result (lambda)
       real(default), intent(in) :: beta_gamma
       type(vector3_t), intent(in) :: beta_vec
       logical, intent(in) :: inverse
       type(lorentz_transformation_t) :: lambda
       if (inverse) then
          lambda = boost (beta_gamma, beta_vec)
       else
          lambda = boost (-beta_gamma, beta_vec)
       end if
    end function create_longitudinal_boost

    function create_transversal_boost (p_rad, xi, sqrts_real) result (lambda)
       type(vector4_t), intent(in) :: p_rad
       real(default), intent(in) :: xi, sqrts_real
       type(lorentz_transformation_t) :: lambda
       type(vector3_t) :: vec_transverse
       real(default) :: pt2, beta, beta_gamma
       pt2 = transverse_part (p_rad)**2
       beta = one / sqrt (one + sqrts_real**2 * (one - xi) / pt2)
       beta_gamma = beta / sqrt (one - beta**2)
       vec_transverse%p(1:2) = p_rad%p(1:2)
       vec_transverse%p(3) = zero
       call normalize (vec_transverse)
       lambda = boost (-beta_gamma, vec_transverse)
    end function create_transversal_boost
  end subroutine phs_fks_generator_generate_isr

  subroutine phs_fks_generator_set_sqrts_hat (generator, sqrts)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: sqrts
    generator%sqrts = sqrts
  end subroutine phs_fks_generator_set_sqrts_hat

  subroutine phs_fks_generator_set_emitters (generator, emitters)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in), dimension(:), allocatable ::  emitters
    allocate (generator%emitters (size (emitters)))
    generator%emitters = emitters
  end subroutine phs_fks_generator_set_emitters

  subroutine phs_fks_generator_setup_masses (generator, n_tot)
    class (phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: n_tot
    if (.not. allocated (generator%m2)) then
       allocate (generator%is_massive (n_tot))
       allocate (generator%m2 (n_tot))
       generator%is_massive = .false.
       generator%m2 = zero
    end if
  end subroutine phs_fks_generator_setup_masses

  subroutine phs_fks_generator_set_isr_kinematics (generator, p_born)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), dimension(2), intent(in), optional :: p_born
    type(vector4_t), dimension(2) :: p

    if (present (p_born)) then
       p = p_born
    else
       p = generator%real_kinematics%p_born_lab%phs_point(1)%p(1:2)
    end if

    !generator%isr_kinematics%x = p%p(0) / (generator%sqrts / two)
    generator%isr_kinematics%x = p%p(0) / generator%isr_kinematics%beam_energy
  end subroutine phs_fks_generator_set_isr_kinematics

  subroutine phs_fks_generator_generate_radiation_variables &
                    (generator, r_in, p_born, phs_identifiers)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in), dimension(:) :: r_in
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers

    if (any (generator%emitters <= 2) .and. generator%n_in > 1) &
        call generator%set_isr_kinematics &
           (generator%real_kinematics%p_born_lab%phs_point(1)%p(1:2))

    associate (rad_var => generator%real_kinematics)
       rad_var%phi = r_in (I_PHI) * twopi
       select case (generator%mode)
       case (GEN_REAL_PHASE_SPACE)
          rad_var%jac_rand = twopi
       case (GEN_SOFT_MISMATCH)
          rad_var%jac_mismatch = twopi
       end select
       call generator%compute_xi_tilde (r_in(I_XI))
       call generator%set_masses (p_born, phs_identifiers)
       call generator%compute_y (r_in(I_Y), p_born, phs_identifiers)
    end associate
  end subroutine phs_fks_generator_generate_radiation_variables

  pure subroutine phs_fks_generator_compute_xi_ref_momenta &
         (generator, p_born, resonance_contributors)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(resonance_contributors_t), intent(in), dimension(:), optional &
       :: resonance_contributors
    integer :: i_con, n_contributors
    if (present (resonance_contributors)) then
       n_contributors = size (resonance_contributors)
       if (.not. allocated (generator%resonance_contributors)) &
          allocate (generator%resonance_contributors (n_contributors))
       do i_con = 1, n_contributors
          generator%real_kinematics%xi_ref_momenta(i_con) = &
               get_resonance_momentum (p_born, resonance_contributors(i_con)%c)
          generator%resonance_contributors(i_con) = resonance_contributors(i_con)
       end do
    else
       generator%real_kinematics%xi_ref_momenta(1) = sum (p_born(1:generator%n_in))
    end if
  end subroutine phs_fks_generator_compute_xi_ref_momenta

  subroutine phs_fks_generator_compute_cms_energy (generator, p_born)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t) :: p_sum
    p_sum = sum (p_born (1 : generator%n_in))
    generator%real_kinematics%cms_energy2 = p_sum**2
  end subroutine phs_fks_generator_compute_cms_energy

  subroutine phs_fks_generator_compute_xi_max (generator, emitter, i_phs, p, i_con)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: i_phs, emitter
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in), optional :: i_con
    real(default) :: q0
    type(vector4_t), dimension(:), allocatable :: pp
    type(vector4_t) :: p_res
    type(lorentz_transformation_t) :: L_to_resonance
    integer :: ii_con
    if (.not. any (generator%emitters == emitter)) return
    ii_con = 1; if (present (i_con)) ii_con = i_con
    allocate (pp (size (p)))
    associate (rad_var => generator%real_kinematics)
       q0 = rad_var%xi_ref_momenta(ii_con)**1 
       select case (generator%n_in)
       case (1)
          if (emitter > 1) then
             if (generator%is_massive(emitter)) then
                rad_var%xi_max(i_phs) = get_xi_max_fsr &
                   (p, q0, emitter, generator%m2(emitter), rad_var%y(i_phs))
             else
                rad_var%xi_max(i_phs) = get_xi_max_fsr (p, q0, emitter)
             end if
          else
             rad_var%xi_max(i_phs) = get_xi_max_isr_decay (p)
          end if
       case (2)
          if (present (i_con)) then
             p_res = rad_var%xi_ref_momenta(ii_con)
             L_to_resonance = inverse (boost (p_res, q0))
             pp = L_to_resonance * p
          else
             pp = p
          end if
          if (emitter <= 2) then
             rad_var%xi_max(i_phs) = get_xi_max_isr &
                (generator%isr_kinematics%x, rad_var%y(i_phs))
          else
             if (generator%is_massive(emitter)) then
                rad_var%xi_max(i_phs) = get_xi_max_fsr &
                   (pp, q0, emitter, generator%m2(emitter), rad_var%y(i_phs)) 
             else
                rad_var%xi_max(i_phs) = get_xi_max_fsr (pp, q0, emitter)
             end if
          end if
       case default
          call msg_fatal ("Real phase space: " // &
             "Only 1 or 2 initial particles supported")
       end select
    end associate
  end subroutine phs_fks_generator_compute_xi_max

  subroutine phs_fks_generator_compute_xi_max_isr_factorized &
     (generator, i_phs, p)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: i_phs
    type(vector4_t), intent(in), dimension(:) :: p
    generator%real_kinematics%xi_max(i_phs) = get_xi_max_isr_decay (p)
  end subroutine phs_fks_generator_compute_xi_max_isr_factorized

  subroutine phs_fks_generator_set_masses (generator, p, phs_identifiers)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers
    type(vector4_t), intent(in), dimension(:) :: p
    integer :: emitter, i_phs
    do i_phs = 1, size (phs_identifiers)
       emitter = phs_identifiers(i_phs)%emitter
       if (any (generator%emitters == emitter) .and. emitter > 0) then
          if (generator%is_massive (emitter) .and. emitter > generator%n_in) &
             generator%m2(emitter) = p(emitter)**2
       end if
    end do
  end subroutine phs_fks_generator_set_masses

  subroutine phs_fks_generator_compute_y (generator, r_y, p, phs_identifiers)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r_y
    type(vector4_t), intent(in), dimension(:) :: p
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers
    integer :: emitter, i_phs
    real(default) :: beta, one_p_beta, one_m_beta
    type(lorentz_transformation_t) :: boost_to_resonance
    real(default) :: q0
    type(vector4_t) :: p_res, p_em
    integer :: i
    real(default), parameter :: tiny_scale = 0.01_default
    real(default) :: theta2_min
    logical :: construct_massive_fsr = .false.
    associate (rad_var => generator%real_kinematics)
       select case (generator%mode)
       case (GEN_REAL_PHASE_SPACE)
          do i_phs = 1, size (phs_identifiers)
             emitter = phs_identifiers(i_phs)%emitter 
             p_res = vector4_null
             if (any (generator%emitters == emitter)) then
                construct_massive_fsr = emitter > generator%n_in
                if (construct_massive_fsr) construct_massive_fsr = &
                   construct_massive_fsr .and. generator%is_massive (emitter)
                if (construct_massive_fsr) then
                   if (allocated (phs_identifiers(i_phs)%contributors)) then
                      associate (contributors => phs_identifiers(i_phs)%contributors)
                         do i = 1, size (contributors)
                            p_res = p_res + p(contributors(i))
                         end do
                         q0 = p_res**1
                         boost_to_resonance = inverse (boost (p_res, q0))
                         p_em = boost_to_resonance * p(emitter)
                      end associate
                   else
                       q0 = generator%sqrts
                       p_em = p(emitter)
                   end if
                   beta = beta_emitter (q0, p_em)
                   one_m_beta = one - beta
                   one_p_beta = one + beta
                   rad_var%y(i_phs) = one / beta * (one - one_p_beta * &
                       exp ( - r_y * log(one_p_beta / one_m_beta)))
                   rad_var%jac_rand(i_phs) = rad_var%jac_rand(i_phs) * &
                       (one - beta * rad_var%y(i_phs)) * log(one_p_beta / one_m_beta) / beta
                else
                   rad_var%y(i_phs) = (one - two * r_y) * generator%y_max
                   rad_var%jac_rand(i_phs) = rad_var%jac_rand(i_phs) * 3 * (one - rad_var%y(i_phs)**2)
                   rad_var%y(i_phs) = 1.5_default * (rad_var%y(i_phs) - rad_var%y(i_phs)**3 / 3)
                end if
             end if
          end do
       case (GEN_SOFT_MISMATCH)
          rad_var%y_mismatch = (one - two * r_y) * generator%y_max
          rad_var%jac_mismatch = rad_var%jac_mismatch * 3 * (one - rad_var%y_mismatch**2)
          rad_var%y_mismatch = 1.5_default * (rad_var%y_mismatch - rad_var%y_mismatch**3 / 3)
          rad_var%y_soft = rad_var%y_mismatch
       case (GEN_SOFT_LIMIT_TEST)
          rad_var%y = y_test_soft
       case (GEN_COLL_LIMIT_TEST)
          rad_var%y = y_test_coll
       case (GEN_ANTI_COLL_LIMIT_TEST)
          rad_var%y = - y_test_coll
       case (GEN_SOFT_COLL_LIMIT_TEST)
          rad_var%y = y_test_coll
       case (GEN_SOFT_ANTI_COLL_LIMIT_TEST)
          rad_var%y = - y_test_coll
       end select
    end associate
  end subroutine phs_fks_generator_compute_y

  pure function beta_emitter (q0, p) result (beta)
    real(default), intent(in) :: q0
    type(vector4_t), intent(in) :: p
    real(default) :: beta
    real(default) :: m2, mrec2, k0_max
    m2 = p**2
    mrec2 = (q0 - p%p(0))**2 - p%p(1)**2 - p%p(2)**2 - p%p(3)**2
    k0_max = (q0**2 - mrec2 + m2) / (two * q0)
    beta = sqrt(one - m2 / k0_max**2)
  end function beta_emitter

  pure subroutine phs_fks_generator_compute_xi_tilde (generator, r)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r
    real(default) :: deno
    associate (rad_var => generator%real_kinematics)
       select case (generator%mode)
       case (GEN_REAL_PHASE_SPACE) 
          if (generator%singular_jacobian) then
             rad_var%xi_tilde = (one - generator%xi_min) - (one - r)**2 *&
                (one - two * generator%xi_min)
             rad_var%jac_rand = rad_var%jac_rand * two * (one - r) * &
                (one - two * generator%xi_min)
          else
             rad_var%xi_tilde = generator%xi_min + r * (one - generator%xi_min)
             rad_var%jac_rand = rad_var%jac_rand * (one - generator%xi_min)
          end if
       case (GEN_SOFT_MISMATCH)
          deno = one - r
          if (deno < tiny_13) deno = tiny_13
          rad_var%xi_mismatch = generator%xi_min + r / deno
          rad_var%jac_mismatch = rad_var%jac_mismatch / deno**2
       case (GEN_SOFT_LIMIT_TEST)
          rad_var%xi_tilde = r * two * xi_tilde_test_soft
          rad_var%jac_rand = two * xi_tilde_test_soft
       case (GEN_COLL_LIMIT_TEST)
          rad_var%xi_tilde = xi_tilde_test_coll
          rad_var%jac_rand = xi_tilde_test_coll
       case (GEN_ANTI_COLL_LIMIT_TEST)
          rad_var%xi_tilde = xi_tilde_test_coll
          rad_var%jac_rand = xi_tilde_test_coll
       case (GEN_SOFT_COLL_LIMIT_TEST)
          rad_var%xi_tilde = r * two * xi_tilde_test_soft
          rad_var%jac_rand = two * xi_tilde_test_soft
       case (GEN_SOFT_ANTI_COLL_LIMIT_TEST)
          rad_var%xi_tilde = r * two * xi_tilde_test_soft
          rad_var%jac_rand = two * xi_tilde_test_soft
       end select
    end associate
  end subroutine phs_fks_generator_compute_xi_tilde

  subroutine phs_fks_generator_prepare_generation (generator, r_in, i_phs, &
     emitter, p_born, phs_identifiers, contributors, i_con)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), dimension(3), intent(in) :: r_in
    integer, intent(in) :: i_phs, emitter
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_identifier_t), intent(in), dimension(:) :: phs_identifiers
    type(resonance_contributors_t), intent(in), dimension(:), optional :: contributors
    integer, intent(in), optional :: i_con
    call generator%generate_radiation_variables (r_in, p_born, phs_identifiers)
    call generator%compute_xi_ref_momenta (p_born, contributors)
    call generator%compute_xi_max (emitter, i_phs, p_born, i_con) 
  end subroutine phs_fks_generator_prepare_generation

  subroutine phs_fks_generator_generate_fsr_from_xi_and_y (generator, xi, y, &
     phi, emitter, i_phs, p_born, p_real)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: xi, y, phi
    integer, intent(in) :: emitter, i_phs
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(phs_point_t), intent(inout) :: p_real
    associate (rad_var => generator%real_kinematics)
       rad_var%supply_xi_max = .false.
       rad_var%xi_tilde = xi
       rad_var%y(i_phs) = y
       rad_var%phi = phi
    end associate
    call generator%set_sqrts_hat (p_born(1)%p(0) + p_born(2)%p(0))
    call generator%generate_fsr (emitter, i_phs, p_born, p_real)
  end subroutine phs_fks_generator_generate_fsr_from_xi_and_y

  pure subroutine phs_fks_generator_get_radiation_variables (generator, &
     i_phs, xi, y, phi)
    class(phs_fks_generator_t), intent(in) :: generator
    integer, intent(in) :: i_phs
    real(default), intent(out) :: xi, y
    real(default), intent(out), optional :: phi
    associate (rad_var => generator%real_kinematics)
       xi = rad_var%xi_max(i_phs) * rad_var%xi_tilde
       y = rad_var%y(i_phs)
       if (present (phi)) phi = rad_var%phi
    end associate
  end subroutine phs_fks_generator_get_radiation_variables

  subroutine phs_fks_generator_get_jacobian (generator, emitter, jac)
    class(phs_fks_generator_t), intent(in) :: generator
    integer, intent(in) :: emitter
    real(default) :: jac
    associate (rad_var => generator%real_kinematics)
       jac = rad_var%jac_rand (emitter) * rad_var%jac(emitter)%jac(1)
    end associate
  end subroutine phs_fks_generator_get_jacobian

  subroutine phs_fks_generator_write (generator, unit)
    class(phs_fks_generator_t), intent(in) :: generator
    integer, intent(in), optional :: unit
    integer :: u
    type(string_t) :: massive_phsp
    u = given_output_unit (unit); if (u < 0) return
    if (generator%massive_phsp) then
       massive_phsp = " massive "
    else
       massive_phsp = " massless "
    end if
    write (u, "(A)") char ("This is a generator for a" &
         // massive_phsp // "phase space")
    if (associated (generator%real_kinematics)) then
       call generator%real_kinematics%write ()
    else
       write (u, "(A)") "Warning: There are no real " // &
            "kinematics associated with this generator"
    end if
    call write_separator (u)
    write (u, "(A,F5.3)") "sqrts: ", generator%sqrts
    write (u, "(A,F5.3)") "E_gluon: ", generator%E_gluon
    write (u, "(A,F5.3)") "mrec2: ", generator%mrec2
  end subroutine phs_fks_generator_write

  subroutine phs_fks_compute_isr_kinematics (phs, r)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(in) :: r
    if (.not. phs%config%cm_frame) then
      call phs%generator%compute_isr_kinematics (r, phs%lt_cm_to_lab * phs%phs_wood_t%p)
    else
       call phs%generator%compute_isr_kinematics (r, phs%phs_wood_t%p)
    end if
  end subroutine phs_fks_compute_isr_kinematics


end module phs_fks

