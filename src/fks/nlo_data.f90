! WHIZARD 2.2.6 May 02 2015
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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use lorentz

  implicit none
  private

  public :: fks_template_t
  public :: real_kinematics_t
  public :: isr_kinematics_t
  public :: nlo_particle_data_t
  public :: nlo_states_t
  public :: sqme_collector_t

  type :: fks_template_t
    type(string_t) :: id
    integer :: mapping_type
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
  contains
    procedure :: write => fks_template_write
    procedure :: set_dij_exp => fks_template_set_dij_exp
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
  end type fks_template_t

  type :: real_jacobian_t
    real(default), dimension(3) :: jac = 1._default
  contains
  
  end type real_jacobian_t

  type :: real_kinematics_t
    logical :: supply_xi_max = .true.
    real(default) :: xi_tilde
    real(default) :: phi
    real(default), dimension(:), allocatable :: xi_max, y
    type(real_jacobian_t), dimension(:), allocatable :: jac
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: p_real
    real(default), dimension(:), allocatable :: jac_rand
    real(default), dimension(:), allocatable :: y_soft
    real(default) :: cms_energy2
  contains
    procedure :: write => real_kinematics_write
  end type real_kinematics_t

  type :: isr_kinematics_t
    real(default), dimension(2) :: x = 1._default
    real(default), dimension(2) :: z
    real(default) :: sqrts_born
    real(default) :: beam_energy
    real(default) :: fac_scale
    real(default), dimension(2) :: jacobian = 1._default
  end type isr_kinematics_t

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
    real(default) :: current_sqme_born
    real(default) :: current_sqme_real
    real(default), dimension(:), allocatable :: sqme_real_per_emitter
    real(default), dimension(:), allocatable :: sqme_real_non_sub
    real(default), dimension(:,:,:), allocatable :: sqme_born_cc
    complex(default), dimension(:), allocatable :: sqme_born_sc
    real(default) :: sqme_real_sum
    real(default) :: current_sqme_virt
    real(default), dimension(:), allocatable :: sqme_born_list
  contains
    procedure :: get_sqme_sum => sqme_collector_get_sqme_sum
    procedure :: get_sqme_born => sqme_collector_get_sqme_born
    procedure :: setup_sqme_real => sqme_collector_setup_sqme_real
  end type sqme_collector_t


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

  function sqme_collector_get_sqme_sum (collector) result (sqme)
    class(sqme_collector_t), intent(in) :: collector
    real(default) :: sqme
    sqme = collector%current_sqme_born + &
           collector%sqme_real_sum + &
           collector%current_sqme_virt
  end function sqme_collector_get_sqme_sum

  function sqme_collector_get_sqme_born (collector, i_flv) result (sqme)
    class(sqme_collector_t), intent(in) :: collector
    integer, intent(in), optional :: i_flv
    real(default) :: sqme
    if (present (i_flv)) then
       sqme = collector%sqme_born_list (i_flv)
    else
       sqme = collector%current_sqme_born
    end if
  end function sqme_collector_get_sqme_born

  subroutine sqme_collector_setup_sqme_real (collector, n_particles)
    class(sqme_collector_t), intent(inout) :: collector
    integer, intent(in) :: n_particles
    if (.not. allocated (collector%sqme_real_per_emitter)) &
       allocate (collector%sqme_real_per_emitter (n_particles))
    collector%sqme_real_per_emitter = 0._default
  end subroutine sqme_collector_setup_sqme_real


end module nlo_data

