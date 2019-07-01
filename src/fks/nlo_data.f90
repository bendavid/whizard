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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use constants, only: zero, one, two, twopi
  use io_units
  use lorentz

  implicit none
  private

  public :: fks_template_t
  public :: real_kinematics_t
  public :: compute_dalitz_bounds
  public :: isr_kinematics_t
  public :: kinematics_counter_t
  public :: pdf_container_t
  public :: powheg_damping_t
  public :: powheg_damping_simple_t
  public :: nlo_particle_data_t
  public :: nlo_states_t
  public :: sqme_collector_t

  integer, parameter, public :: I_PLUS = 1
  integer, parameter, public :: I_MINUS = 2

  integer, parameter, public :: FSR_SIMPLE = 1
  integer, parameter, public :: FSR_MASSIVE = 2
  integer, parameter, public :: FSR_MASSLESS_RECOILER = 3

  type :: fks_template_t
    type(string_t) :: id
    logical :: subtraction_disabled = .false.
    integer :: mapping_type
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
  contains
    procedure :: write => fks_template_write
    procedure :: set_dij_exp => fks_template_set_dij_exp
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
    procedure :: disable_subtraction => fks_template_disable_subtraction
  end type fks_template_t

  type :: real_jacobian_t
    real(default), dimension(4) :: jac = 1._default
  contains
  
  end type real_jacobian_t

  type :: real_kinematics_t
    logical :: supply_xi_max = .true.
    real(default) :: xi_tilde
    real(default) :: phi
    real(default), dimension(:), allocatable :: xi_max, y
    type(real_jacobian_t), dimension(:), allocatable :: jac
    type(vector4_t), dimension(:), allocatable :: p_born_cms
    type(vector4_t), dimension(:), allocatable :: p_born_lab
    type(vector4_t), dimension(:), allocatable :: p_real_cms
    type(vector4_t), dimension(:), allocatable :: p_real_lab
    real(default), dimension(3) :: x_rad
    real(default), dimension(:), allocatable :: jac_rand
    real(default), dimension(:), allocatable :: y_soft
    real(default) :: cms_energy2
    type(vector4_t), dimension(:), allocatable :: k_perp
  contains
    procedure :: init => real_kinematics_init
    procedure :: write => real_kinematics_write
    procedure :: kt2 => real_kinematics_kt2
    procedure :: compute_k_perp_isr => real_kinematics_compute_k_perp_isr
    procedure :: compute_k_perp_fsr => real_kinematics_compute_k_perp_fsr
  end type real_kinematics_t

  type :: isr_kinematics_t
    integer :: n_in
    real(default), dimension(2) :: x = 1._default
    real(default), dimension(2) :: z = 0._default
    real(default) :: sqrts_born = 0._default
    real(default) :: beam_energy = 0._default
    real(default) :: fac_scale = 0._default
    real(default), dimension(2) :: jacobian = 1._default
  end type isr_kinematics_t

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

  type :: pdf_container_t
     real(default), dimension(-6:6) :: f
  contains
  
  end type pdf_container_t

  type, abstract :: powheg_damping_t
  contains
    procedure (powheg_damping_get_f), deferred :: get_f
  end type powheg_damping_t

  type, extends (powheg_damping_t) :: powheg_damping_simple_t
     real(default) :: h2 = 5._default
  contains
    procedure :: get_f => powheg_damping_simple_get_f
  end type powheg_damping_simple_t

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
    real(default), dimension(:,:), allocatable :: sqme_real_per_emitter
    real(default), dimension(:), allocatable :: sqme_real_non_sub
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    complex(default), dimension(:), allocatable :: sqme_born_sc
    real(default) :: sqme_real_sum
    real(default), dimension(:), allocatable :: sqme_born_list
    real(default), dimension(:,:), allocatable :: sqme_virt_born_list
    real(default), dimension(:,:), allocatable :: sqme_virt_list
  contains
    procedure :: get_sqme_sum => sqme_collector_get_sqme_sum
    procedure :: get_sqme_born => sqme_collector_get_sqme_born
    procedure :: setup_sqme_real => sqme_collector_setup_sqme_real
    procedure :: reset => sqme_collector_reset
  end type sqme_collector_t


  abstract interface
    function powheg_damping_get_f (damping, pt2) result (f)
       import
       real(default) :: f
       class(powheg_damping_t), intent(in) :: damping
       real(default), intent(in) :: pt2
    end function powheg_damping_get_f
  end interface


contains

  subroutine fks_template_write (object, unit)
    class(fks_template_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u,'(1x,A)') 'FKS Template: '
    write (u,'(1x,A,I0)') 'Mapping Type: ', object%mapping_type
    write (u,'(1x,A,ES4.3,ES4.3)') 'd_ij exponentials: ', object%fks_dij_exp1, object%fks_dij_exp2
  end subroutine fks_template_write

  subroutine fks_template_set_dij_exp (object, exp1, exp2)
    class(fks_template_t), intent(inout) :: object
    real(default), intent(in) :: exp1, exp2
    object%fks_dij_exp1 = exp1
    object%fks_dij_exp2 = exp2
  end subroutine fks_template_set_dij_exp

  subroutine fks_template_set_mapping_type (object, val)
    class(fks_template_t), intent(inout) :: object
    integer, intent(in) :: val
    object%mapping_type = val
  end subroutine fks_template_set_mapping_type

  subroutine fks_template_set_counter (object)
    class(fks_template_t), intent(inout) :: object
    object%count_kinematics = .true.
  end subroutine fks_template_set_counter

  subroutine fks_template_disable_subtraction (object)
    class(fks_template_t), intent(inout) :: object
    object%subtraction_disabled = .true.
  end subroutine fks_template_disable_subtraction

  subroutine real_kinematics_init (r, n_tot)
    class(real_kinematics_t), intent(inout) :: r
    integer, intent(in) :: n_tot
    allocate (r%xi_max (n_tot))
    allocate (r%y (n_tot))
    allocate (r%y_soft (n_tot))
    allocate (r%p_born_cms (n_tot), &
              r%p_born_lab (n_tot), &
              r%p_real_cms (n_tot+1), &
              r%p_real_lab (n_tot+1))
    allocate (r%jac (n_tot), r%jac_rand (n_tot))
    allocate (r%k_perp (n_tot))
    r%xi_tilde = zero
    r%xi_max = zero
    r%y = zero
    r%phi = zero
    r%cms_energy2 = zero
  end subroutine real_kinematics_init
    
  subroutine real_kinematics_write (r, unit)
    class(real_kinematics_t), intent(in) :: r
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    write (u,"(A)") "Real kinematics: "
    write (u,"(A,F5.3)") "xi_tilde: ", r%xi_tilde
    write (u,"(A,F5.3)") "phi: ", r%phi
    write (u,"(A,100F5.3,1X)") "xi_max: ", r%xi_max
    write (u,"(A,100F5.3,1X)") "y: ", r%y
    write (u,"(A,100F5.3,1X)") "jac_rand: ", r%jac_rand
    write (u,"(A,100F5.3,1X)") "y_soft: ", r%y_soft
  end subroutine real_kinematics_write

  pure subroutine compute_dalitz_bounds (q0, m2, mrec2, z1, z2, k0_rec_max)
    real(default), intent(in) :: q0, m2, mrec2
    real(default), intent(out) :: z1, z2, k0_rec_max
    k0_rec_max = (q0**2-m2+mrec2)/(2*q0)
    z1 = (k0_rec_max+sqrt(k0_rec_max**2-mrec2))/q0
    z2 = (k0_rec_max-sqrt(k0_rec_max**2-mrec2))/q0
  end subroutine compute_dalitz_bounds

  function real_kinematics_kt2 &
         (real_kinematics, emitter, kt2_type, xi, y) result (kt2)
    real(default) :: kt2
    class(real_kinematics_t), intent(in) :: real_kinematics
    integer, intent(in) :: emitter, kt2_type
    real(default), intent(in), optional :: xi, y
    real(default) :: xii, yy
    real(default) :: q, E_em, z, z1, z2, m2, mrec2, k0_rec_max
    type(vector4_t) :: p_emitter
    if (present (y)) then
       yy = y
    else
       yy = real_kinematics%y (emitter)
    end if
    if (present (xi)) then
       xii = xi
    else
       xii = real_kinematics%xi_tilde * real_kinematics%xi_max (emitter)
    end if
    select case (kt2_type)
    case (FSR_SIMPLE)
       kt2 = real_kinematics%cms_energy2 / 2 * xii**2 * (1 - yy)
    case (FSR_MASSIVE)
       q = sqrt (real_kinematics%cms_energy2)
       p_emitter = real_kinematics%p_born_cms(emitter)
       mrec2 = (q - p_emitter%p(0))**2 - sum (p_emitter%p(1:3)**2)
       m2 = p_emitter**2
       E_em = energy (p_emitter)
       call compute_dalitz_bounds (q, m2, mrec2, z1, z2, k0_rec_max)
       z = z2 - (z2 - z1) * (one + yy) / two
       kt2 = xii**2 * q**3 * (one - z) / &
            (2 * E_em - z * xii * q)
    case (FSR_MASSLESS_RECOILER)
       kt2 = real_kinematics%cms_energy2 / 2 * xii**2 * (1 - yy**2) / 2
    case default
       kt2 = 0.0
       call msg_bug ("kt2_type must be set to a known value")
    end select
  end function real_kinematics_kt2

  subroutine real_kinematics_compute_k_perp_isr (real_kin, emitter)
    class(real_kinematics_t), intent(inout) :: real_kin
    integer, intent(in) :: emitter
    associate (k => real_kin%k_perp(emitter))
       k%p(0) = 0._default
       k%p(1) = cos(real_kin%phi)
       k%p(2) = sin(real_kin%phi)
       k%p(3) = 0._default
    end associate
  end subroutine real_kinematics_compute_k_perp_isr

  subroutine real_kinematics_compute_k_perp_fsr (real_kin, emitter)
    class(real_kinematics_t), intent(inout) :: real_kin
    integer, intent(in) :: emitter
    type(vector3_t) :: vec
    type(lorentz_transformation_t) :: rot
    associate (p => real_kin%p_born_cms(emitter), k => real_kin%k_perp(emitter))
       vec = p%p(1:3)/p%p(0)
       k%p(0) = 0._default
       k%p(1) = p%p(1); k%p(2) = p%p(2)
       k%p(3) = -(p%p(1)**2 + p%p(2)**2) / p%p(3)
       rot = rotation (cos(real_kin%phi), sin(real_kin%phi), vec)
       k = rot*k
       k%p(1:3) = k%p(1:3) / space_part_norm (k)
    end associate
  end subroutine real_kinematics_compute_k_perp_fsr

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

  function powheg_damping_simple_get_f (damping, pt2) result (f)
    real(default) :: f
    class(powheg_damping_simple_t), intent(in) :: damping
    real(default), intent(in) :: pt2
    f = damping%h2 / (pt2 + damping%h2)
  end function powheg_damping_simple_get_f

  function sqme_collector_get_sqme_sum (collector) result (sqme)
    class(sqme_collector_t), intent(in) :: collector
    real(default) :: sqme
    sqme = sum (collector%sqme_born_list) + &
           collector%sqme_real_sum + &
           sum (collector%sqme_virt_list)
    if (debug_active (D_SUBTRACTION)) then
       call msg_debug (D_SUBTRACTION, "Get content of sqme lists: ")
       print *, 'Born: ', collector%sqme_born_list
       print *, 'Real: ', collector%sqme_real_sum
       print *, 'Virt: ', collector%sqme_virt_list
       print *, 'Sum: ', sqme
    end if
  end function sqme_collector_get_sqme_sum

  function sqme_collector_get_sqme_born (collector, i_flv) result (sqme)
    real(default) :: sqme
    class(sqme_collector_t), intent(in) :: collector
    integer, intent(in) :: i_flv
    sqme = collector%sqme_born_list (i_flv)
  end function sqme_collector_get_sqme_born

  subroutine sqme_collector_setup_sqme_real (collector, n_flv, n_particles)
    class(sqme_collector_t), intent(inout) :: collector
    integer, intent(in) :: n_flv, n_particles
    if (.not. allocated (collector%sqme_real_per_emitter)) &
       allocate (collector%sqme_real_per_emitter (n_flv, n_particles))
    collector%sqme_real_per_emitter = 0._default
  end subroutine sqme_collector_setup_sqme_real

  subroutine sqme_collector_reset (collector)
    class(sqme_collector_t), intent(inout) :: collector
       collector%sqme_born_list = 0._default
       collector%sqme_real_sum = 0._default
       collector%sqme_virt_list = 0._default
  end subroutine sqme_collector_reset


end module nlo_data

