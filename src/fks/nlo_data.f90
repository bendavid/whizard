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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use constants, only: zero, one, two, twopi
  use io_units
  use lorentz
  use variables, only: var_list_t
  use format_defs, only: FMT_15

  implicit none
  private

  public :: fks_template_t
  public :: phs_identifier_t
  public :: check_for_phs_identifier
  public :: phs_point_set_t
  public :: real_kinematics_t
  public :: compute_dalitz_bounds
  public :: real_scales_t
  public :: isr_kinematics_t
  public :: pdf_container_t
  public :: powheg_damping_t
  public :: powheg_damping_simple_t
  public :: nlo_settings_t
  public :: nlo_particle_data_t
  public :: nlo_states_t
  public :: sqme_collector_t
  public :: nlo_cuts_t

  integer, parameter, public :: I_PLUS = 1
  integer, parameter, public :: I_MINUS = 2

  integer, parameter, public :: FKS_DEFAULT = 1
  integer, parameter, public :: FKS_RESONANCES = 2

  integer, parameter, public :: NO_FACTORIZATION = 0
  integer, parameter, public :: FACTORIZATION_THRESHOLD = 1

  integer, parameter, public :: FSR_SIMPLE = 1
  integer, parameter, public :: FSR_MASSIVE = 2
  integer, parameter, public :: FSR_MASSLESS_RECOILER = 3

  type :: fks_template_t
    type(string_t) :: id
    logical :: subtraction_disabled = .false.
    integer :: mapping_type = FKS_DEFAULT
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
    type(string_t), dimension(:), allocatable :: excluded_resonances
  contains
    procedure :: write => fks_template_write
    procedure :: set_dij_exp => fks_template_set_dij_exp
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
    procedure :: disable_subtraction => fks_template_disable_subtraction
  end type fks_template_t

  type :: phs_identifier_t
     integer, dimension(:), allocatable :: contributors
     integer :: emitter = -1
     logical :: evaluated = .false.
  contains
    generic :: init => init_from_emitter, init_from_emitter_and_contributors
    procedure :: init_from_emitter => phs_identifier_init_from_emitter
    procedure :: init_from_emitter_and_contributors &
       => phs_identifier_init_from_emitter_and_contributors
    procedure :: check => phs_identifier_check
    procedure :: write => phs_identifier_write
  end type phs_identifier_t

  type :: phs_point_set_t
     type(phs_point_t), dimension(:), allocatable :: phs_point
     logical :: initialized = .false.
  contains
    procedure :: init => phs_point_set_init
    procedure :: write => phs_point_set_write
    procedure :: get_n_momenta => phs_point_set_get_n_momenta
    procedure :: get_momenta => phs_point_set_get_momenta
    procedure :: get_momentum => phs_point_set_get_momentum
    procedure :: get_energy => phs_point_set_get_energy
    procedure :: set_momenta => phs_point_set_set_momenta
    procedure :: get_n_particles => phs_point_set_get_n_particles
    procedure :: get_n_phs => phs_point_set_get_n_phs
    procedure :: get_invariant_mass => phs_point_set_get_invariant_mass
    procedure :: write_phs_point => phs_point_set_write_phs_point
  end type phs_point_set_t

  type :: real_jacobian_t
    real(default), dimension(4) :: jac = 1._default
  contains
  
  end type real_jacobian_t

  type :: real_kinematics_t
    logical :: supply_xi_max = .true.
    real(default) :: xi_tilde
    real(default) :: phi
    real(default), dimension(:), allocatable :: xi_max, y
    real(default) :: xi_mismatch, y_mismatch
    type(real_jacobian_t), dimension(:), allocatable :: jac
    real(default) :: jac_mismatch
    type(phs_point_set_t) :: p_born_cms
    type(phs_point_set_t) :: p_born_lab
    type(phs_point_set_t) :: p_real_cms
    type(phs_point_set_t) :: p_real_lab
    integer, dimension(:), allocatable :: alr_to_i_phs
    real(default), dimension(3) :: x_rad
    real(default), dimension(:), allocatable :: jac_rand
    real(default), dimension(:), allocatable :: y_soft
    real(default) :: cms_energy2
    type(vector4_t), dimension(:), allocatable :: k_perp
    type(vector4_t), dimension(:), allocatable :: xi_ref_momenta
  contains
    procedure :: init => real_kinematics_init
    procedure :: write => real_kinematics_write
    procedure :: kt2 => real_kinematics_kt2
    procedure :: compute_k_perp_isr => real_kinematics_compute_k_perp_isr
    procedure :: compute_k_perp_fsr => real_kinematics_compute_k_perp_fsr
  end type real_kinematics_t

  type :: real_scales_t
     real(default) :: scale
     real(default) :: ren_scale
     real(default) :: fac_scale
     real(default) :: scale_born
     real(default) :: fac_scale_born
     real(default) :: ren_scale_born
  end type real_scales_t

  type :: isr_kinematics_t
    integer :: n_in
    real(default), dimension(2) :: x = one
    real(default), dimension(2) :: z = zero
    real(default), dimension(2) :: z_coll = zero
    real(default) :: sqrts_born = zero
    real(default) :: beam_energy = zero
    real(default) :: fac_scale = zero
    real(default), dimension(2) :: jacobian = one
  end type isr_kinematics_t

  type :: pdf_container_t
     real(default), dimension(-6:6) :: f
  contains
  
  end type pdf_container_t

  type, abstract :: powheg_damping_t
  contains
    procedure (powheg_damping_init), deferred :: init
    procedure (powheg_damping_write), deferred :: write
    procedure (powheg_damping_get_f), deferred :: get_f
  end type powheg_damping_t

  type, extends (powheg_damping_t) :: powheg_damping_simple_t
     real(default) :: h2 = 5._default
  contains
    procedure :: get_f => powheg_damping_simple_get_f
    procedure :: init => powheg_damping_simple_init
    procedure :: write => powheg_damping_simple_write
  end type powheg_damping_simple_t

  type :: nlo_settings_t
     logical :: use_internal_color_correlations = .true.
     logical :: use_internal_spin_correlations = .false.
     logical :: use_resonance_mappings = .false.
     logical :: combined_integration = .false.
     logical :: with_virtual_subtraction = .true.
     logical :: test_soft_limit = .false.
     logical :: test_coll_limit = .false.
     logical :: test_anti_coll_limit = .false.
     integer :: fixed_alr = 0
     integer :: factorization_mode = NO_FACTORIZATION
     !!! Probably not the right place for this. Revisit after refactoring
     real(default) :: powheg_damping_scale = zero
  contains
  procedure :: init => nlo_settings_init
    procedure :: write => nlo_settings_write
  end type nlo_settings_t

  type :: nlo_particle_data_t
    integer :: n_in
    integer :: n_out_born, n_out_real
    integer :: n_flv_born, n_flv_real
  end type nlo_particle_data_t

  type :: nlo_states_t
    integer, dimension(:,:), allocatable :: flv_state_born
    integer, dimension(:,:), allocatable :: flv_state_real
    integer, dimension(:), allocatable :: flv_born
    integer, dimension(:), allocatable :: hel_born
    integer, dimension(:), allocatable :: col_born
  end type nlo_states_t

  type :: sqme_collector_t
    real(default) :: current_sqme_real
    real(default), dimension(:,:), allocatable :: sqme_real_per_phs
    real(default), dimension(:,:), allocatable :: sqme_real_non_sub
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    complex(default), dimension(:), allocatable :: sqme_born_sc
    real(default) :: sqme_real_sum
    real(default), dimension(:), allocatable :: sqme_born_list
    real(default), dimension(:), allocatable :: sqme_subtraction_born_list
    real(default), dimension(:,:), allocatable :: sqme_virt_born_list
    real(default), dimension(:,:), allocatable :: sqme_virt_list
    real(default) :: sqme_mismatch
    real(default), dimension(:), allocatable :: sqme_dglap_list
  contains
    procedure :: get_sqme_sum => sqme_collector_get_sqme_sum
    procedure :: get_sqme_born => sqme_collector_get_sqme_born
    procedure :: reset => sqme_collector_reset
    procedure :: write => sqme_collector_write
  end type sqme_collector_t

  type :: nlo_cuts_t
    logical :: passed_born
    logical, dimension(:), allocatable :: passed_real
  end type nlo_cuts_t


  abstract interface
     subroutine powheg_damping_init (damping, scale)
       import
       class(powheg_damping_t), intent(out) :: damping
       real(default), intent(in) :: scale
     end subroutine powheg_damping_init
  end interface

  abstract interface
     subroutine powheg_damping_write (damping, unit)
       import
       class(powheg_damping_t), intent(in) :: damping
       integer, intent(in), optional :: unit
     end subroutine powheg_damping_write
  end interface

  abstract interface
    function powheg_damping_get_f (damping, pt2) result (f)
       import
       real(default) :: f
       class(powheg_damping_t), intent(in) :: damping
       real(default), intent(in) :: pt2
    end function powheg_damping_get_f
  end interface


contains

  subroutine fks_template_write (template, unit)
    class(fks_template_t), intent(in) :: template
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,'(1x,A)') 'FKS Template: '
    write (u,'(1x,A,I0)') 'Mapping Type: ', template%mapping_type
    write (u,'(1x,A,ES4.3,ES4.3)') 'd_ij exponentials: ', &
       template%fks_dij_exp1, template%fks_dij_exp2
  end subroutine fks_template_write

  subroutine fks_template_set_dij_exp (template, exp1, exp2)
    class(fks_template_t), intent(inout) :: template
    real(default), intent(in) :: exp1, exp2
    template%fks_dij_exp1 = exp1
    template%fks_dij_exp2 = exp2
  end subroutine fks_template_set_dij_exp

  subroutine fks_template_set_mapping_type (template, val)
    class(fks_template_t), intent(inout) :: template
    integer, intent(in) :: val
    template%mapping_type = val
  end subroutine fks_template_set_mapping_type

  subroutine fks_template_set_counter (template)
    class(fks_template_t), intent(inout) :: template
    template%count_kinematics = .true.
  end subroutine fks_template_set_counter

  subroutine fks_template_disable_subtraction (template)
    class(fks_template_t), intent(inout) :: template
    template%subtraction_disabled = .true.
  end subroutine fks_template_disable_subtraction

  subroutine phs_identifier_init_from_emitter (phs_id, emitter)
    class(phs_identifier_t), intent(out) :: phs_id
    integer, intent(in) :: emitter
    phs_id%emitter = emitter
  end subroutine phs_identifier_init_from_emitter

  subroutine phs_identifier_init_from_emitter_and_contributors &
     (phs_id, emitter, contributors)
     class(phs_identifier_t), intent(out) :: phs_id
     integer, intent(in) :: emitter
     integer, intent(in), dimension(:) :: contributors
     allocate (phs_id%contributors (size (contributors)))
     phs_id%contributors = contributors
     phs_id%emitter = emitter
  end subroutine phs_identifier_init_from_emitter_and_contributors
  function phs_identifier_check (phs_id, emitter, contributors) result (check)
    logical :: check
    class(phs_identifier_t), intent(in) :: phs_id
    integer, intent(in) :: emitter
    integer, intent(in), dimension(:), optional :: contributors
    check = phs_id%emitter == emitter
    if (present (contributors)) then
       if (.not. allocated (phs_id%contributors)) &
          call msg_fatal ("Phs identifier: contributors not allocated!")
       check = check .and. all (phs_id%contributors == contributors)
    end if
  end function phs_identifier_check

  subroutine phs_identifier_write (phs_id, unit)
    class(phs_identifier_t), intent(in) :: phs_id
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit); if (u < 0) return
    write (u, '(A)') 'phs_identifier: '
    write (u, '(A,1X,I1)') 'Emitter: ', phs_id%emitter
    if (allocated (phs_id%contributors)) then
       write (u, '(A)', advance = 'no') 'Resonance contributors: '
       do i = 1, size (phs_id%contributors)
          write (u, '(I1,1X)', advance = 'no') phs_id%contributors(i)
       end do
    else
       write (u, '(A)') 'No Contributors allocated'
    end if
  end subroutine phs_identifier_write

  subroutine check_for_phs_identifier (phs_id, n_in, emitter, contributors, phs_exist, i_phs)
     type(phs_identifier_t), intent(in), dimension(:) :: phs_id
     integer, intent(in) :: n_in, emitter
     integer, intent(in), dimension(:), optional :: contributors
     logical, intent(out) :: phs_exist
     integer, intent(out) :: i_phs
     integer :: i
     phs_exist = .false.
     i_phs = -1
     do i = 1, size (phs_id)
        if (phs_id(i)%emitter < 0) then
           i_phs = i
           exit
        end if 
        if (emitter > n_in) then
           phs_exist = phs_id(i)%emitter == emitter
        else
           phs_exist = phs_id(i)%emitter <= n_in
        end if
        if (present (contributors)) &
           phs_exist = phs_exist .and. all (phs_id(i)%contributors == contributors)
        if (phs_exist) then
           i_phs = i
           exit
        end if
     end do
  end subroutine check_for_phs_identifier

  subroutine phs_point_set_init (phs_point_set, n_particles, n_phs)
    class(phs_point_set_t), intent(out) :: phs_point_set
    integer, intent(in) :: n_particles, n_phs
    integer :: i_phs
    allocate (phs_point_set%phs_point (n_phs))
    do i_phs = 1, n_phs
       phs_point_set%phs_point(i_phs) = n_particles
    end do
    phs_point_set%initialized = .true.
  end subroutine phs_point_set_init

  subroutine phs_point_set_write (phs_point_set, i_phs, contributors, unit)
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in), optional :: i_phs
    integer, intent(in), dimension(:), optional :: contributors
    integer, intent(in), optional :: unit
    integer :: i, u
    type(vector4_t) :: p_sum
    u = given_output_unit (unit); if (u < 0) return
    if (present (i_phs)) then
       call phs_point_set%phs_point(i_phs)%write (u, show_mass = .true.)
    else
       do i = 1, size(phs_point_set%phs_point)
          call phs_point_set%phs_point(i_phs)%write (u, show_mass = .true.)
       end do
    end if
    if (present (contributors)) then
       p_sum = vector4_null
       call msg_debug (D_SUBTRACTION, "Invariant masses for real emission: ")
       associate (p => phs_point_set%phs_point(i_phs)%p)
          do i = 1, size (contributors)
             p_sum = p_sum + p(contributors(i))
          end do
          p_sum = p_sum + p(size(p))
       end associate
       if (debug_active (D_SUBTRACTION)) call vector4_write (p_sum, show_mass = .true.)
    end if
  end subroutine phs_point_set_write

  elemental function phs_point_set_get_n_momenta (phs_point_set, i_res) result (n)
    integer :: n
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_res
    n = phs_point_set%phs_point(i_res)%n_momenta
  end function phs_point_set_get_n_momenta

  function phs_point_set_get_momenta (phs_point_set, i_phs) result (p)
    type(vector4_t), dimension(:), allocatable :: p
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_phs
    allocate (p (phs_point_set%phs_point(i_phs)%n_momenta), &
       source = phs_point_set%phs_point(i_phs)%p)
  end function phs_point_set_get_momenta

  pure function phs_point_set_get_momentum (phs_point_set, i_phs, i_mom) result (p)
    type(vector4_t) :: p
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_phs, i_mom
    p = phs_point_set%phs_point(i_phs)%p(i_mom)
  end function phs_point_set_get_momentum

  pure function phs_point_set_get_energy (phs_point_set, i_phs, i_mom) result (E)
    real(default) :: E
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_phs, i_mom
    E = phs_point_set%phs_point(i_phs)%p(i_mom)%p(0)
  end function phs_point_set_get_energy

  subroutine phs_point_set_set_momenta (phs_point_set, i_phs, p)
    class(phs_point_set_t), intent(inout) :: phs_point_set
    integer, intent(in) :: i_phs
    type(vector4_t), intent(in), dimension(:) :: p
    phs_point_set%phs_point(i_phs)%p = p
  end subroutine phs_point_set_set_momenta

  function phs_point_set_get_n_particles (phs_point_set, i) result (n_particles)
    integer :: n_particles
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in), optional :: i
    integer :: j
    j = 1; if (present (i)) j = i
    n_particles = size (phs_point_set%phs_point(j)%p)
  end function phs_point_set_get_n_particles

  function phs_point_set_get_n_phs (phs_point_set) result (n_phs)
    integer :: n_phs
    class(phs_point_set_t), intent(in) :: phs_point_set
    n_phs = size (phs_point_set%phs_point)
  end function phs_point_set_get_n_phs

  function phs_point_set_get_invariant_mass (phs_point_set, i_phs, i_part) result (m2)
    real(default) :: m2
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_phs
    integer, intent(in), dimension(:) :: i_part
    type(vector4_t) :: p
    integer :: i
    p = vector4_null
    do i = 1, size (i_part)
       p = p + phs_point_set%phs_point(i_phs)%p(i_part(i))
    end do
    m2 = p**2
  end function phs_point_set_get_invariant_mass

  subroutine phs_point_set_write_phs_point (phs_point_set, i_phs, unit, show_mass, &
     testflag, check_conservation, ultra, n_in)
    class(phs_point_set_t), intent(in) :: phs_point_set
    integer, intent(in) :: i_phs
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_mass
    logical, intent(in), optional :: testflag, ultra
    logical, intent(in), optional :: check_conservation
    integer, intent(in), optional :: n_in
    call phs_point_set%phs_point(i_phs)%write (unit, show_mass, testflag, &
       check_conservation, ultra, n_in)
  end subroutine phs_point_set_write_phs_point

  subroutine real_kinematics_init (r, n_tot, n_phs, n_alr, n_contr)
    class(real_kinematics_t), intent(inout) :: r
    integer, intent(in) :: n_tot, n_phs, n_alr, n_contr
    allocate (r%xi_max (n_phs))
    allocate (r%y (n_phs))
    allocate (r%y_soft (n_phs))
    call r%p_born_cms%init (n_tot, 1)
    call r%p_born_lab%init (n_tot, 1)
    call r%p_real_cms%init (n_tot + 1, n_phs)
    call r%p_real_lab%init (n_tot + 1, n_phs)
    allocate (r%jac (n_phs), r%jac_rand (n_phs))
    allocate (r%k_perp (n_tot))
    allocate (r%alr_to_i_phs (n_alr))
    allocate (r%xi_ref_momenta (n_contr))
    r%alr_to_i_phs = 0
    r%xi_tilde = zero; r%xi_mismatch = zero
    r%xi_max = zero
    r%y = zero; r%y_mismatch = zero
    r%y_soft = zero
    r%phi = zero
    r%cms_energy2 = zero
    r%xi_ref_momenta = vector4_null
    r%jac_mismatch = one
    r%jac_rand = one
  end subroutine real_kinematics_init

  subroutine real_kinematics_write (r, unit)
    class(real_kinematics_t), intent(in) :: r
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit); if (u < 0) return
    write (u,"(A)") "Real kinematics: "
    write (u,"(A,F5.3)") "xi_tilde: ", r%xi_tilde
    write (u,"(A,F5.3)") "phi: ", r%phi
    do i = 1, size (r%xi_max)
       write (u,"(A,I1,1X)") "i_phs: ", i
       write (u,"(A,100F5.3,1X)") "xi_max: ", r%xi_max(i)
       write (u,"(A,100F5.3,1X)") "y: ", r%y(i)
       write (u,"(A,100F5.3,1X)") "jac_rand: ", r%jac_rand(i)
       write (u,"(A,100F5.3,1X)") "y_soft: ", r%y_soft(i)
    end do
  end subroutine real_kinematics_write

  pure subroutine compute_dalitz_bounds (q0, m2, mrec2, z1, z2, k0_rec_max)
    real(default), intent(in) :: q0, m2, mrec2
    real(default), intent(out) :: z1, z2, k0_rec_max
    k0_rec_max = (q0**2 - m2 + mrec2) / (two * q0)
    z1 = (k0_rec_max + sqrt(k0_rec_max**2 - mrec2)) / q0
    z2 = (k0_rec_max - sqrt(k0_rec_max**2 - mrec2)) / q0
  end subroutine compute_dalitz_bounds

  function real_kinematics_kt2 &
     (real_kinematics, i_phs, emitter, kt2_type, xi, y) result (kt2)
    real(default) :: kt2
    class(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: emitter, i_phs, kt2_type
    real(default), intent(in), optional :: xi, y
    real(default) :: xii, yy
    real(default) :: q, E_em, z, z1, z2, m2, mrec2, k0_rec_max
    type(vector4_t) :: p_emitter
    if (present (y)) then
       yy = y
    else
       yy = real_kinematics%y (i_phs)
    end if
    if (present (xi)) then
       xii = xi
    else
       xii = real_kinematics%xi_tilde * real_kinematics%xi_max (i_phs)
    end if
    select case (kt2_type)
    case (FSR_SIMPLE)
       kt2 = real_kinematics%cms_energy2 / two * xii**2 * (1 - yy)
    case (FSR_MASSIVE)
       q = sqrt (real_kinematics%cms_energy2)
       p_emitter = real_kinematics%p_born_cms%phs_point(1)%p(emitter)
       mrec2 = (q - p_emitter%p(0))**2 - sum (p_emitter%p(1:3)**2)
       m2 = p_emitter**2
       E_em = energy (p_emitter)
       call compute_dalitz_bounds (q, m2, mrec2, z1, z2, k0_rec_max)
       z = z2 - (z2 - z1) * (one + yy) / two
       kt2 = xii**2 * q**3 * (one - z) / &
          (two * E_em - z * xii * q)
    case (FSR_MASSLESS_RECOILER)
       kt2 = real_kinematics%cms_energy2 / two * xii**2 * (1 - yy**2) / two
    case default
       kt2 = zero
       call msg_bug ("kt2_type must be set to a known value")
    end select
  end function real_kinematics_kt2

  subroutine real_kinematics_compute_k_perp_isr (real_kin, emitter)
    class(real_kinematics_t), intent(inout) :: real_kin
    integer, intent(in) :: emitter
    associate (k => real_kin%k_perp(emitter))
       k%p(0) = zero
       k%p(1) = cos(real_kin%phi)
       k%p(2) = sin(real_kin%phi)
       k%p(3) = zero
    end associate
  end subroutine real_kinematics_compute_k_perp_isr

  subroutine real_kinematics_compute_k_perp_fsr (real_kin, emitter)
    class(real_kinematics_t), intent(inout) :: real_kin
    integer, intent(in) :: emitter
    type(vector3_t) :: vec
    type(lorentz_transformation_t) :: rot
    associate (p => real_kin%p_born_cms%phs_point(1)%p(emitter), k => real_kin%k_perp(emitter))
       vec = p%p(1:3) / p%p(0)
       k%p(0) = zero
       k%p(1) = p%p(1); k%p(2) = p%p(2)
       k%p(3) = - (p%p(1)**2 + p%p(2)**2) / p%p(3)
       rot = rotation (cos(real_kin%phi), sin(real_kin%phi), vec)
       k = rot * k
       k%p(1:3) = k%p(1:3) / space_part_norm (k)
    end associate
  end subroutine real_kinematics_compute_k_perp_fsr

  function powheg_damping_simple_get_f (damping, pt2) result (f)
    real(default) :: f
    class(powheg_damping_simple_t), intent(in) :: damping
    real(default), intent(in) :: pt2
    f = damping%h2 / (pt2 + damping%h2)
  end function powheg_damping_simple_get_f

  subroutine powheg_damping_simple_init (damping, scale)
    class(powheg_damping_simple_t), intent(out) :: damping
    real(default), intent(in) :: scale
    damping%h2 = scale**2
  end subroutine powheg_damping_simple_init

  subroutine powheg_damping_simple_write (damping, unit)
    class(powheg_damping_simple_t), intent(in) :: damping
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(1x,A)") "Powheg damping simple: "
    write (u, "(1x,A, "// FMT_15 // ")") "scale h2: ", damping%h2
  end subroutine powheg_damping_simple_write

  subroutine nlo_settings_init (nlo_settings, var_list, combined_integration)
    class(nlo_settings_t), intent(out) :: nlo_settings
    type(var_list_t), intent(in) :: var_list
    logical, intent(in), optional :: combined_integration
    type(string_t) :: color_method
    color_method = var_list%get_sval (var_str ('$correlation_me_method'))
    nlo_settings%use_internal_color_correlations = color_method == 'omega' & 
       .or. color_method == 'threshold'
    if (present (combined_integration)) then
       nlo_settings%combined_integration = combined_integration
    else
       nlo_settings%combined_integration = &
             var_list%get_lval (var_str("?combined_nlo_integration"))
    end if
    nlo_settings%test_soft_limit = var_list%get_lval (var_str ('?test_soft_limit'))
    nlo_settings%test_coll_limit = var_list%get_lval (var_str ('?test_coll_limit'))
    nlo_settings%test_anti_coll_limit = var_list%get_lval (var_str ('?test_anti_coll_limit'))
    nlo_settings%fixed_alr = var_list%get_ival (var_str ('fixed_alpha_region'))
    nlo_settings%with_virtual_subtraction = &
       .not. var_list%get_lval (var_str ('?switch_off_virtual_subtraction'))
    nlo_settings%powheg_damping_scale = &
         var_list%get_rval (var_str ('powheg_damping_scale'))
  end subroutine nlo_settings_init

  subroutine nlo_settings_write (nlo_settings, unit)
    class(nlo_settings_t), intent(in) :: nlo_settings
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, '(A)') 'nlo_settings:'
    write (u, '(3X,A,L1)') 'internal_color_correlations = ', &
         nlo_settings%use_internal_color_correlations
    write (u, '(3X,A,L1)') 'internal_spin_correlations = ', &
         nlo_settings%use_internal_spin_correlations
    write (u, '(3X,A,L1)') 'use_resonance_mappings = ', &
         nlo_settings%use_resonance_mappings
    write (u, '(3X,A,L1)') 'combined_integration = ', &
         nlo_settings%combined_integration
    write (u, '(3X,A,L1)') 'with_virtual_subtraction = ', &
         nlo_settings%with_virtual_subtraction
    write (u, '(3X,A,L1)') 'test_soft_limit = ', &
         nlo_settings%test_soft_limit
    write (u, '(3X,A,L1)') 'test_coll_limit = ', &
         nlo_settings%test_coll_limit
    write (u, '(3X,A,L1)') 'test_anti_coll_limit = ', &
         nlo_settings%test_anti_coll_limit
    write (u, '(3X,A,I5)') 'fixed_alr = ', &
         nlo_settings%fixed_alr
    write (u, '(3X,A,I2)') 'factorization_mode = ', &
         nlo_settings%factorization_mode
    write (u, '(3X,A,' // FMT_15 // ')') 'powheg_damping_scale = ', &
         nlo_settings%powheg_damping_scale
  end subroutine nlo_settings_write

  function sqme_collector_get_sqme_sum (collector) result (sqme)
    class(sqme_collector_t), intent(in) :: collector
    real(default) :: sqme
    sqme = sum (collector%sqme_born_list) + &
           collector%sqme_real_sum + &
           sum (collector%sqme_virt_list) + &
           collector%sqme_mismatch + &
           sum (collector%sqme_dglap_list)
    if (debug2_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "Get content of sqme lists: ")
       call collector%write ()
       print *, 'Sum: ', sqme
    end if
  end function sqme_collector_get_sqme_sum

  function sqme_collector_get_sqme_born (collector, i_flv) result (sqme)
    real(default) :: sqme
    class(sqme_collector_t), intent(in) :: collector
    integer, intent(in) :: i_flv
    sqme = collector%sqme_born_list (i_flv)
  end function sqme_collector_get_sqme_born

  subroutine sqme_collector_reset (collector)
    class(sqme_collector_t), intent(inout) :: collector
       collector%sqme_born_list = zero
       collector%sqme_real_sum = zero
       collector%sqme_virt_list = zero
       collector%sqme_mismatch = zero
       collector%sqme_dglap_list = zero
  end subroutine sqme_collector_reset

  subroutine sqme_collector_write (collector)
    class(sqme_collector_t), intent(in) :: collector
    print *, 'Born: ', collector%sqme_born_list
    print *, 'Real: ', collector%sqme_real_sum
    print *, 'Virt: ', collector%sqme_virt_list
    print *, 'Dglap: ', collector%sqme_dglap_list
    print *, 'Mismatch: ', collector%sqme_mismatch
  end subroutine sqme_collector_write


end module nlo_data

