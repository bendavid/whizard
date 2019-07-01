! WHIZARD 2.2.7 Aug 11 2015
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
  use io_units, only: given_output_unit
  use format_utils, only: write_separator
  use lorentz
  use physics_defs
  use flavors
  use sf_mappings
  use sf_base
  use phs_base
  use phs_wood
  use process_constants
  use process_libraries
  use nlo_data

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


  type, extends (phs_wood_config_t) :: phs_fks_config_t
    integer :: mode = PHS_MODE_UNDEFINED
  contains
    procedure :: final => phs_fks_config_final
    procedure :: write => phs_fks_config_write
    procedure :: set_mode => phs_fks_config_set_mode
    procedure :: configure => phs_fks_config_configure
    procedure :: startup_message => phs_fks_config_startup_message
    procedure, nopass :: allocate_instance => phs_fks_config_allocate_instance
    procedure :: set_born_config => phs_fks_config_set_born_config
  end type phs_fks_config_t

  type :: phs_fks_generator_t
    integer, dimension(:), allocatable :: emitters
    type(real_kinematics_t), pointer :: real_kinematics => null()
    type(isr_kinematics_t), pointer :: isr_kinematics => null()
    real(default) :: xi_min = tiny_07
    real(default) :: y_max = 1._default
    real(default) :: sqrts
    real(default) :: E_gluon
    real(default) :: mrec2
    real(default), dimension(:), allocatable :: m2
    logical :: massive_phsp = .false.
    logical, dimension(:), allocatable :: is_massive
    logical :: singular_jacobian = .false.
  contains
    procedure :: connect_kinematics => phs_fks_generator_connect_kinematics
    procedure :: get_real_kinematics => phs_fks_generator_get_real_kinematics
    procedure :: compute_isr_kinematics => phs_fks_generator_compute_isr_kinematics
    procedure :: generate_fsr => phs_fks_generator_generate_fsr
    generic :: compute_emitter_kinematics => &
                      compute_emitter_kinematics_massless, &
                      compute_emitter_kinematics_massive
    procedure :: compute_emitter_kinematics_massless => &
                      phs_fks_generator_compute_emitter_kinematics_massless
    procedure :: compute_emitter_kinematics_massive => &
                      phs_fks_generator_compute_emitter_kinematics_massive
    procedure :: generate_isr => phs_fks_generator_generate_isr
    procedure :: generate_isr_from_x => phs_fks_generator_generate_isr_from_x
    procedure :: set_beam_energy => phs_fks_generator_set_beam_energy
    procedure :: set_emitters => phs_fks_generator_set_emitters
    procedure :: setup_masses => phs_fks_generator_setup_masses
    procedure :: set_isr_kinematics => phs_fks_generator_set_isr_kinematics
    procedure :: generate_radiation_variables => &
                         phs_fks_generator_generate_radiation_variables
    procedure :: compute_y => phs_fks_generator_compute_y
    procedure :: compute_xi_tilde => phs_fks_generator_compute_xi_tilde
    procedure :: generate_fsr_from_x => phs_fks_generator_generate_fsr_from_x
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
    type(kinematics_counter_t) :: counter
    logical :: perform_generation = .true.
    !!! Not entirley suited for combined integration
    !!! TODO: Modifiy global r_real-array
    real(default) :: r_isr

  contains
    procedure :: init => phs_fks_init
    procedure :: final => phs_fks_final
    procedure :: init_momenta => phs_fks_init_momenta
    procedure :: set_incoming_momenta => phs_fks_set_incoming_momenta
    procedure :: evaluate_selected_channel => phs_fks_evaluate_selected_channel
    procedure :: evaluate_other_channels => phs_fks_evaluate_other_channels
    procedure :: get_mcpar => phs_fks_get_mcpar
    procedure :: get_real_kinematics => phs_fks_get_real_kinematics
    procedure :: set_beam_energy => phs_fks_set_beam_energy
    procedure :: set_emitters => phs_fks_set_emitters
    procedure :: setup_masses => phs_fks_setup_masses
    procedure :: get_born_momenta => phs_fks_get_born_momenta
    procedure :: get_outgoing_momenta => phs_fks_get_outgoing_momenta
    procedure :: get_incoming_momenta => phs_fks_get_incoming_momenta
    procedure :: display_kinematics => phs_fks_display_kinematics
    procedure :: set_isr_kinematics => phs_fks_set_isr_kinematics
    procedure :: generate_radiation_variables => &
                         phs_fks_generate_radiation_variables
    procedure :: set_reference_frames => phs_fks_set_reference_frames
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
!    call object%phs_wood_config_t%final ()
  end subroutine phs_fks_config_final

  subroutine phs_fks_config_write (object, unit)
    class(phs_fks_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call object%phs_wood_config_t%write
  end subroutine phs_fks_config_write

  subroutine phs_fks_config_set_mode (phs_config, mode)
    class(phs_fks_config_t), intent(inout) :: phs_config
    integer, intent(in) :: mode
    select case (mode)
    case (NLO_REAL)
       phs_config%mode = PHS_MODE_ADDITIONAL_PARTICLE
    case (NLO_PDF)
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
    integer, intent(inout), optional :: nlo_type
    if (present (nlo_type)) then
      if (.not. (nlo_type == NLO_REAL .or. nlo_type == NLO_PDF)) &
        call msg_fatal ("FKS config has to be called with nlo_type = 'Real' or nlo_type = 'Pdf'")
    end if
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
  end subroutine phs_fks_config_set_born_config

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

  pure subroutine phs_fks_generator_get_real_kinematics &
         (generator, xi_tilde, y, phi, xi_max, jac, jac_rand)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(out), dimension(:), allocatable :: xi_max
    real(default), intent(out) :: xi_tilde
    real(default), intent(out), dimension(:), allocatable :: y
    real(default), intent(out) :: phi
    real(default), intent(out), dimension(4) :: jac
    real(default), intent(out), dimension(:), allocatable :: jac_rand
    associate (real_kinematics => generator%real_kinematics)
      xi_tilde = real_kinematics%xi_tilde
      y = real_kinematics%y
      phi = real_kinematics%phi
      xi_max = real_kinematics%xi_max
      jac = real_kinematics%jac(1)%jac
      jac_rand = real_kinematics%jac_rand
    end associate
  end subroutine phs_fks_generator_get_real_kinematics

  pure subroutine phs_fks_generator_compute_isr_kinematics (generator, r, p_in)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r
    type(vector4_t), dimension(2), intent(in), optional :: p_in
    integer :: em
    type(vector4_t), dimension(2) :: p

    if (present (p_in)) then
       p = p_in
    else
       p = generator%real_kinematics%p_born_lab
    end if

    associate (isr => generator%isr_kinematics)
       do em = 1, 2
          isr%x(em) = p(em)%p(0) / isr%beam_energy
          isr%z(em) = one - (one - isr%x(em)) * r
          isr%jacobian(em) =  isr%jacobian(em) * (one - isr%x(em))
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
          phs%p_born_tot (1:n_in) = phs%p_born
          phs%p_born_tot (n_in+1:) = phs%q_born
          call phs%set_reference_frames ()
          call phs%set_isr_kinematics ()
          call phs%generate_radiation_variables (r_in(phs%n_r_born+1:phs%n_r_born+3))
       case (PHS_MODE_COLLINEAR_REMNANT)
          call phs%compute_isr_kinematics (r_in(phs%n_r_born+1))
          phs%r_isr = r_in(phs%n_r_born+1)
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
    r(1:phs%n_r_born) = phs%r(1:phs%n_r_born,c)
    select case (phs%mode)
    case (PHS_MODE_ADDITIONAL_PARTICLE)
       r(phs%n_r_born+1:) = phs%r_real
    case (PHS_MODE_COLLINEAR_REMNANT)
       r(phs%n_r_born+1:) = phs%r_isr
    end select
  end subroutine phs_fks_get_mcpar

  subroutine phs_fks_get_real_kinematics (phs, xi_tilde, y, phi, xi_max, jac, jac_rand)
    class(phs_fks_t), intent(inout) :: phs
    real(default), intent(out), dimension(:), allocatable :: xi_max
    real(default), intent(out) :: xi_tilde
    real(default), intent(out), dimension(:), allocatable :: y
    real(default), intent(out) :: phi
    real(default), intent(out), dimension(4) :: jac
    real(default), intent(out), dimension(:), allocatable :: jac_rand
    call phs%generator%get_real_kinematics (xi_tilde, y, phi, xi_max, jac, jac_rand)
  end subroutine phs_fks_get_real_kinematics

  subroutine phs_fks_set_beam_energy (phs)
    class(phs_fks_t), intent(inout) :: phs
    call phs%generator%set_beam_energy (phs%config%sqrts)
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
       p(1:phs%config%n_in) = phs%p_born
       p(phs%config%n_in+1:) = phs%q_born
    case (PHS_MODE_COLLINEAR_REMNANT)
       p(1:phs%config%n_in) = phs%phs_wood_t%p
       p(phs%config%n_in+1:) = phs%phs_wood_t%q
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

  subroutine phs_fks_display_kinematics (phs)
     class(phs_fks_t), intent(in) :: phs
!     call phs%counter%display ()
  end subroutine phs_fks_display_kinematics

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
    call phs%generator%generate_radiation_variables (r_in, phs%p_born_tot)
    phs%r_real = r_in
  end subroutine phs_fks_generate_radiation_variables

  subroutine phs_fks_set_reference_frames (phs)
    class(phs_fks_t), intent(inout) :: phs
    type(lorentz_transformation_t) :: lt_cm_to_lab
    associate (real_kinematics => phs%generator%real_kinematics)
       real_kinematics%p_born_cms = phs%p_born_tot
       if (.not. phs%config%cm_frame) then
          !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
          lt_cm_to_lab = phs%lt_cm_to_lab
          real_kinematics%p_born_lab = lt_cm_to_lab * phs%p_born_tot
       else
          real_kinematics%p_born_lab = phs%p_born_tot
       end if
    end associate
  end subroutine phs_fks_set_reference_frames

  subroutine phs_fks_generator_generate_fsr (generator, emitter, p_born, p_real)
    !!! Important: Momenta must be input in the center-of-mass frame
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    integer :: nlegborn, nlegreal
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

    associate (rad_var => generator%real_kinematics)
       xi = rad_var%xi_tilde
       if (rad_var%supply_xi_max) xi = xi*rad_var%xi_max(emitter)
       y = rad_var%y(emitter)
       phi = rad_var%phi
    end associate
    nlegborn = size (p_born)
    nlegreal = nlegborn+1
    if (emitter <= 2 .or. emitter > nlegborn) then
      call msg_fatal ("Generate FSR phase space: Invalid emitter!")
    end if
    allocate (p_real (nlegreal))

    p_real(1) = p_born(1)
    p_real(2) = p_born(2)
    q = p_born(1) + p_born(2)
    q0 = q%p(0)
    q2 = q**2
    generator%real_kinematics%cms_energy2 = q2

    generator%E_gluon = q0*xi/2
    uk_np1 = generator%E_gluon
    k_n_born = p_born(emitter)%p(1:3)
    uk_n_born = k_n_born**1

    generator%mrec2 = (q-p_born(emitter))**2
    if (generator%is_massive(emitter)) then
       call generator%compute_emitter_kinematics (emitter, q0, k0_n, uk_n, uk)
    else
       call generator%compute_emitter_kinematics (emitter, q0, uk_n, uk)
       generator%real_kinematics%y_soft = y
       k0_n = uk_n
    end if

    vec = uk_n / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real(emitter)%p(0) = k0_n
    p_real(emitter)%p(1:3) = vec%p(1:3)
    cpsi = (uk_n**2 + uk**2 - uk_np1**2) / (2*(uk_n * uk))
    !!! This is to catch the case where cpsi = 1, but numerically
    !!! turns out to be slightly larger than 1.
    call check_cpsi_bound (cpsi)
    rot = rotation (cpsi, -sqrt (1._default-cpsi**2), vec_orth)
    p_real(emitter) = rot*p_real(emitter)
    vec = uk_np1 / uk_n_born * k_n_born
    vec_orth = create_orthogonal (vec)
    p_real(nlegreal)%p(0) = uk_np1
    p_real(nlegreal)%p(1:3) = vec%p(1:3)
    cpsi = (uk_np1**2 + uk**2 - uk_n**2) / (2*(uk_np1 * uk))
    call check_cpsi_bound (cpsi)
    rot = rotation (cpsi, sqrt (1._default-cpsi**2), vec_orth)
    p_real(nlegreal) = rot*p_real(nlegreal)
    k_rec0 = q0 - p_real(emitter)%p(0) - p_real(nlegreal)%p(0)
    uk_rec = sqrt (k_rec0**2 - generator%mrec2)
    if (generator%is_massive(emitter)) then
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
    associate (jac => generator%real_kinematics%jac(emitter))
       if (generator%is_massive(emitter)) then
          jac%jac(1) = jac%jac(1)*4/q0/uk_n_born/xi
       else
          k2 = 2*uk_n*uk_np1*(1-y)
          jac%jac(1) = uk_n**2/uk_n_born / (uk_n - k2/(2*q0))
       end if
       !!! Soft jacobian
       jac%jac(2) = 1._default
       !!! Collinear jacobian
       jac%jac(3) = 1-xi/2*q0/uk_n_born
    end associate
  contains
    subroutine check_cpsi_bound (cpsi)
      real(default), intent(inout) :: cpsi
      if (cpsi > 1._default) cpsi = 1._default
    end subroutine check_cpsi_bound
  end subroutine phs_fks_generator_generate_fsr

  subroutine phs_fks_generate_fsr (phs, emitter, p_born, p_real)
    class(phs_fks_t), intent(inout) :: phs
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    type(vector4_t), dimension(:), allocatable :: p
    integer :: i
    allocate (p(1:size (phs%generator%real_kinematics%p_born_cms)), &
         source = phs%generator%real_kinematics%p_born_cms)
    phs%generator%real_kinematics%supply_xi_max = .true.
    call phs%generator%generate_fsr (emitter, p, p_real)
    phs%generator%real_kinematics%p_real_cms = p_real
    !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug    
    if (.not. phs%config%cm_frame) then
       do i = 1, size (p_real)
          p_real(i) = phs%lt_cm_to_lab * p_real(i)
       end do
    end if
    phs%generator%real_kinematics%p_real_lab = p_real
  end subroutine phs_fks_generate_fsr

  subroutine phs_fks_generator_compute_emitter_kinematics_massless &
                                    (generator, em, q0, uk_em, uk)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: em
    real(default), intent(in) :: q0
    real(default), intent(out) :: uk_em, uk
    real(default) :: y, k0_np1, q2

    y = generator%real_kinematics%y(em)
    k0_np1 = generator%E_gluon
    q2 = q0**2

    uk_em = (q2 - generator%mrec2 - 2*q0*k0_np1) / (2*(q0 - k0_np1*(1-y)))
    uk = sqrt (uk_em**2 + k0_np1**2 + 2*uk_em*k0_np1*y)
  end subroutine phs_fks_generator_compute_emitter_kinematics_massless

  subroutine phs_fks_generator_compute_emitter_kinematics_massive &
                                    (generator, em, q0, k0_em, uk_em, uk)
    class(phs_fks_generator_t), intent(inout) :: generator
    integer, intent(in) :: em
    real(default), intent(in) :: q0
    real(default), intent(inout) :: k0_em, uk_em, uk
    real(default) :: y, k0_np1, q2, mrec2, m2
    real(default) :: k0_rec_max, k0_em_max, k0_rec, uk_rec
    real(default) :: z, z1, z2

    y = generator%real_kinematics%y(em)
    k0_np1 = generator%E_gluon
    q2 = q0**2
    mrec2 = generator%mrec2
    m2 = generator%m2(em)

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
    generator%real_kinematics%cms_energy2 = q2
    generator%real_kinematics%jac(em)%jac = q0*(z1-z2)/4*k0_np1
    generator%real_kinematics%y_soft = &
                 (2*q2*z-q2-mrec2+m2)/(sqrt(k0_em_max**2-m2)*q0)/2
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
    alpha = (k0_rec+uk_rec)/(k0_rec_born+uk_rec_born)
    beta = (1-alpha**2)/(1+alpha**2)
  end function compute_beta_massive

  pure function get_xi_max_fsr_massless (p_born, emitter) result (xi_max)
    type(vector4_t), intent(in), dimension(:) :: p_born
    integer, intent(in) :: emitter
    real(default) :: xi_max
    real(default) :: uk_n_born, q0
    q0 = p_born(1)%p(0) + p_born(2)%p(0)
    uk_n_born = space_part_norm (p_born(emitter))
    xi_max = 2*uk_n_born / q0
  end function get_xi_max_fsr_massless

  pure function get_xi_max_fsr_massive (p_born, emitter, m2, y) result (xi_max)
    type(vector4_t), intent(in), dimension(:) :: p_born
    integer, intent(in) :: emitter
    real(default), intent(in) :: m2, y
    real(default) :: xi_max
    real(default) :: q0, mrec2
    real(default) :: k0_rec_max
    real(default) :: z, z1, z2
    real(default) :: k_np1_max
    q0 = 2*p_born(1)%p(0)
    associate (p => p_born(emitter)%p)
       mrec2 = (q0-p(0))**2 - p(1)**2 - p(2)**2 - p(3)**2
    end associate
    call compute_dalitz_bounds (q0, m2, mrec2, z1, z2, k0_rec_max)
    z = z2 - (z2-z1)*(1+y)/2
    k_np1_max = -(q0**2*z**2 - 2*q0*k0_rec_max*z + mrec2)/(2*q0*z*(1-z))
    xi_max = 2*k_np1_max/q0
  end function get_xi_max_fsr_massive

  function get_xi_max_isr (xb, y) result (xi_max)
    real(default), dimension(2), intent(in) :: xb
    real(default), intent(in) :: y
    real(default) :: xb_plus, xb_minus
    real(default) :: xi_max
    real(default) :: plus_val, minus_val

    xb_plus = xb(I_PLUS); xb_minus = xb(I_MINUS)

    plus_val = 2*(1+y)*xb_plus**2 / &
               (sqrt ((1+xb_plus**2)**2*(1-y)**2 + 16*y*xb_plus**2) &
               + (1-y)*(1-xb_plus**2))
    minus_val = 2*(1-y)*xb_minus**2 / &
                (sqrt ((1+xb_minus**2)**2*(1+y)**2 - 16*y*xb_minus**2) &
               + (1-y)*(1-xb_minus**2))
    xi_max = one - max (plus_val, minus_val)
  end function get_xi_max_isr

  subroutine phs_fks_generate_isr &
       (phs, p_born, p_real)
    class(phs_fks_t), intent(inout) :: phs
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    type(vector4_t) :: p0, p1
    type(lorentz_transformation_t) :: lt
    real(default) :: sqrts_hat
    call phs%generator%generate_isr (p_born, p_real)
    phs%generator%real_kinematics%p_real_lab = p_real
    if (.not. phs%config%cm_frame) then
       sqrts_hat = (p_real(1)+p_real(2))**1
       p0 = p_real(1) + p_real(2)
       lt = boost (p0, sqrts_hat)
       p1 = inverse(lt) * p_real(1)
       lt = lt * rotation_to_2nd (3, space_part (p1))
       phs%generator%real_kinematics%p_real_cms = inverse (lt) * p_real
    end if
  end subroutine phs_fks_generate_isr

  subroutine phs_fks_generator_generate_isr &
       (generator, p_born, p_real)
    !!! Important: Import momenta in the lab frame
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), intent(in) , dimension(:) :: p_born
    type(vector4_t), intent(out), dimension(:), allocatable :: p_real
    real(default) :: xi_max, xi, y, phi
    integer :: nlegborn, nlegreal
    real(default) :: sqrts_real
    real(default) :: k0_np1
    type(lorentz_transformation_t) :: lambda_transv, lambda_longit, lambda_longit_inv
    real(default) :: x_plus, x_minus, xb_plus, xb_minus
    integer :: i
    real(default) :: xi_plus, xi_minus
    real(default) :: beta_gamma
    type(vector3_t) :: beta_vec

    associate (rad_var => generator%real_kinematics)
      xi_max = rad_var%xi_max(1)
      xi = rad_var%xi_tilde * xi_max
      y = rad_var%y(1)
      phi = rad_var%phi
      rad_var%y_soft = y
    end associate

    nlegborn = size (p_born)
    nlegreal = nlegborn+1
    generator%isr_kinematics%sqrts_born = sqrt ((p_born(1) + p_born(2))**2)
    allocate (p_real (nlegreal))

    !!! Initial state real momenta
    xb_plus = generator%isr_kinematics%x(I_PLUS)
    xb_minus = generator%isr_kinematics%x(I_MINUS)
    x_plus = xb_plus/sqrt(1-xi) * sqrt ((2-xi*(1-y)) / (2-xi*(1+y)))
    x_minus = xb_minus/sqrt(1-xi) * sqrt ((2-xi*(1+y)) / (2-xi*(1-y)))
    p_real(I_PLUS) = x_plus/xb_plus * p_born(I_PLUS)
    p_real(I_MINUS) = x_minus/xb_minus * p_born(I_MINUS)
    generator%isr_kinematics%z(I_PLUS) = x_plus/generator%isr_kinematics%x(I_PLUS)
    generator%isr_kinematics%z(I_MINUS) = x_minus/generator%isr_kinematics%x(I_MINUS)

    !!! Create radiation momentum
    sqrts_real = generator%isr_kinematics%sqrts_born / sqrt(1-xi)
    k0_np1 = sqrts_real*xi/2
    p_real(nlegreal)%p(0) = k0_np1
    p_real(nlegreal)%p(1) = k0_np1*sqrt(1-y**2)*sin(phi)
    p_real(nlegreal)%p(2) = k0_np1*sqrt(1-y**2)*cos(phi)
    p_real(nlegreal)%p(3) = k0_np1*y

    call get_boost_parameters (p_real, beta_gamma, beta_vec)
    !!!lambda_longit = create_longitudinal_boost (p_real, inverse = .true.)
    lambda_longit = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .true.)
    p_real(nlegreal) = lambda_longit * p_real(nlegreal)

    !!!lambda_longit = create_longitudinal_boost (p_born, inverse = .false.)
    call get_boost_parameters (p_born, beta_gamma, beta_vec)
    lambda_longit = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .false.)
    forall (i=3:nlegborn) &
        p_real(i) = lambda_longit * p_born(i)

    lambda_transv = create_transversal_boost (p_real(nlegreal), xi, sqrts_real)
    forall (i=3:nlegborn) &
         p_real(i) = lambda_transv * p_real(i)

    !!!lambda_longit_inv = create_longitudinal_boost (p_real, inverse = .true.)
    lambda_longit_inv = create_longitudinal_boost (beta_gamma, beta_vec, inverse = .true.)
    forall (i=3:nlegborn) &
         p_real(i) = lambda_longit_inv * p_real(i)

    !!! Compute jacobians
    do i = 1, 2
       associate (jac => generator%real_kinematics%jac(i))
          xi_plus = xi_max * (one-xb_plus)
          xi_minus = xi_max * (one-xb_minus)
          jac%jac(1) = one / (one-xi)
          jac%jac(2) = one
          jac%jac(3) = xi_plus / (one-xi_plus)
          jac%jac(4) = xi_minus / (one-xi_minus)
       end associate
    end do
  contains
    subroutine get_boost_parameters (p, beta_gamma, beta_vec)
       type(vector4_t), intent(in), dimension(:) :: p
       real(default), intent(out) :: beta_gamma
       type(vector3_t), intent(out) :: beta_vec
       beta_vec = (p(1)%p(1:3) + p(2)%p(1:3)) / (p(1)%p(0) + p(2)%p(0))
       beta_gamma = beta_vec**1 / sqrt (1-beta_vec**2)
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
       pt2 = transverse_part(p_rad)**2
       beta = 1.0 / sqrt (1 + sqrts_real**2 * (1-xi)/pt2)
       beta_gamma = beta / sqrt (1-beta**2)
       vec_transverse%p(1:2) = p_rad%p(1:2)
       vec_transverse%p(3) = 0._default
       call normalize (vec_transverse)
       lambda = boost (-beta_gamma, vec_transverse)
    end function create_transversal_boost
  end subroutine phs_fks_generator_generate_isr

  function phs_fks_generator_generate_isr_from_x (generator, &
                    r_in, p_born) result  (p_real)
    type(vector4_t), dimension(:), allocatable :: p_real
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in), dimension(:) :: r_in
    type(vector4_t), intent(in), dimension(:) :: p_born

    call generator%generate_radiation_variables (r_in, p_born)
    call generator%generate_isr (p_born, p_real)
  end function phs_fks_generator_generate_isr_from_x

  pure subroutine phs_fks_generator_set_beam_energy (generator, sqrts)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: sqrts
    generator%sqrts = sqrts
    generator%isr_kinematics%beam_energy = sqrts / 2
  end subroutine phs_fks_generator_set_beam_energy

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
       generator%m2 = 0._default
    end if
  end subroutine phs_fks_generator_setup_masses

  subroutine phs_fks_generator_set_isr_kinematics (generator, p_born)
    class(phs_fks_generator_t), intent(inout) :: generator
    type(vector4_t), dimension(2), intent(in), optional :: p_born
    type(vector4_t), dimension(2) :: p

    if (present (p_born)) then
       p = p_born
    else
       p = generator%real_kinematics%p_born_lab(1:2)
    end if

    generator%isr_kinematics%x = p%p(0) / (generator%sqrts/2)
  end subroutine phs_fks_generator_set_isr_kinematics

  subroutine phs_fks_generator_generate_radiation_variables &
                    (generator, r_in, p_born)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in), dimension(:) :: r_in
    type(vector4_t), intent(in), dimension(:) :: p_born
    integer :: em

    if (any (generator%emitters <= 2)) &
        call generator%set_isr_kinematics (generator%real_kinematics%p_born_lab)

    associate (rad_var => generator%real_kinematics)
       rad_var%jac_rand = 1.0
       call generator%compute_xi_tilde (r_in(I_XI))
       rad_var%phi = r_in (I_PHI)*twopi
       rad_var%jac_rand = rad_var%jac_rand*twopi
       call generator%compute_y (r_in(I_Y), p_born)
       do em = 1, size (p_born)
          if (any (generator%emitters == em)) then
                if (generator%is_massive(em)) then
                   if (em <= 2) then
                      call msg_fatal ("Massive emitters incompatible with IS phase space")
                   else
                     rad_var%xi_max (em) = get_xi_max_fsr &
                                   (p_born, em, generator%m2(em), rad_var%y(em))
                   end if
                else
                   if (em <= 2) then
                      rad_var%xi_max(em) = get_xi_max_isr (generator%isr_kinematics%x, rad_var%y(em))
                   else
                      rad_var%xi_max(em) = get_xi_max_fsr (p_born, em)
                   end if
                end if
          end if
       end do
    end associate
  end subroutine phs_fks_generator_generate_radiation_variables

  subroutine phs_fks_generator_compute_y (generator, r_y, p)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r_y
    type(vector4_t), dimension(:) :: p
    integer :: em
    real(default) :: beta
    associate (rad_var => generator%real_kinematics)
       do em = 1, size (p)
          if (any (generator%emitters == em)) then
             if (generator%is_massive (em)) then
                generator%m2(em) = p(em)**2
                beta = beta_emitter (generator%sqrts, p(em))
                rad_var%y(em) = 1.0/beta * (1-(1+beta) * &
                    exp(-r_y*log((1+beta)/(1-beta))))
                rad_var%jac_rand(em) = rad_var%jac_rand(em) * &
                    (1-beta*rad_var%y(em))*log((1+beta)/(1-beta))/beta
             else
                rad_var%y(em) = (1-2*r_y)*generator%y_max
                rad_var%jac_rand(em) = rad_var%jac_rand(em)*3*(1-rad_var%y(em)**2)
                rad_var%y(em) = 1.5_default*(rad_var%y(em) - rad_var%y(em)**3/3)
             end if
          end if
       end do
    end associate
  end subroutine phs_fks_generator_compute_y

  pure function beta_emitter (q0, p) result (beta)
    real(default), intent(in) :: q0
    type(vector4_t), intent(in) :: p
    real(default) :: beta
    real(default) :: m2, mrec2, k0_max
    m2 = p**2
    mrec2 = (q0-p%p(0))**2 - p%p(1)**2 - p%p(2)**2 - p%p(3)**2
    k0_max = (q0**2-mrec2+m2)/(2*q0)
    beta = sqrt(1-m2/k0_max**2)
  end function beta_emitter

  pure subroutine phs_fks_generator_compute_xi_tilde (generator, r)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: r
    associate (rad_var => generator%real_kinematics)
       if (generator%singular_jacobian) then
          rad_var%xi_tilde = (1-generator%xi_min) - (1-r)**2*(1-2*generator%xi_min)
          rad_var%jac_rand = rad_var%jac_rand * 2*(1-r)*(1-2*generator%xi_min)
       else
          rad_var%xi_tilde = generator%xi_min + r*(1-generator%xi_min)
          rad_var%jac_rand = rad_var%jac_rand *(1-generator%xi_min)
       end if
    end associate
  end subroutine phs_fks_generator_compute_xi_tilde

  function phs_fks_generator_generate_fsr_from_x (generator, &
                    r_in, emitter, p_born) result  (p_real)
    type(vector4_t), dimension(:), allocatable :: p_real
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in), dimension(:) :: r_in
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p_born

    call generator%generate_radiation_variables (r_in, p_born)
    call generator%generate_fsr (emitter, p_born, p_real)
  end function phs_fks_generator_generate_fsr_from_x

  function phs_fks_generator_generate_fsr_from_xi_and_y (generator, xi, y, &
                                         phi, emitter, p_born) result (p_real)
    class(phs_fks_generator_t), intent(inout) :: generator
    real(default), intent(in) :: xi, y, phi
    integer, intent(in) :: emitter
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), dimension(:), allocatable :: p_real
    associate (rad_var => generator%real_kinematics)
       rad_var%supply_xi_max = .false.
       rad_var%xi_tilde = xi
       rad_var%y(emitter) = y
       rad_var%phi = phi
    end associate
    call generator%set_beam_energy (p_born(1)%p(0) + p_born(2)%p(0))
    call generator%generate_fsr (emitter, p_born, p_real)
  end function phs_fks_generator_generate_fsr_from_xi_and_y

  pure subroutine phs_fks_generator_get_radiation_variables (generator, &
                              emitter, xi, y, phi)
    class(phs_fks_generator_t), intent(in) :: generator
    integer, intent(in) :: emitter
    real(default), intent(out) :: xi, y
    real(default), intent(out), optional :: phi
    associate (rad_var => generator%real_kinematics)
       xi = rad_var%xi_max(emitter) * rad_var%xi_tilde
       y = rad_var%y(emitter)
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
    call phs%generator%compute_isr_kinematics (r)
  end subroutine phs_fks_compute_isr_kinematics


end module phs_fks

