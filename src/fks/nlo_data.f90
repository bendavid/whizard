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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use constants, only: zero
  use io_units
  use lorentz
  use variables, only: var_list_t
  use format_defs, only: FMT_15
  use physics_defs, only: THR_POS_WP, THR_POS_WM
  use physics_defs, only: THR_POS_B, THR_POS_BBAR
  use physics_defs, only: NO_FACTORIZATION, FACTORIZATION_THRESHOLD

  implicit none
  private

  public :: fks_template_t
  public :: real_scales_t
  public :: pdf_container_t
  public :: powheg_damping_t
  public :: powheg_damping_simple_t
  public :: get_threshold_momenta
  public :: nlo_settings_t

  integer, parameter, public :: FKS_DEFAULT = 1
  integer, parameter, public :: FKS_RESONANCES = 2


  type :: fks_template_t
    logical :: subtraction_disabled = .false.
    integer :: mapping_type = FKS_DEFAULT
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
    real(default) :: xi_min
    real(default) :: y_max
    type(string_t), dimension(:), allocatable :: excluded_resonances
  contains
    procedure :: write => fks_template_write
    procedure :: set_dij_exp => fks_template_set_dij_exp
    procedure :: set_xi_and_y_bounds => fks_template_set_xi_and_y_bounds
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
    procedure :: disable_subtraction => fks_template_disable_subtraction
  end type fks_template_t

  type :: real_scales_t
     real(default) :: scale
     real(default) :: ren_scale
     real(default) :: fac_scale
     real(default) :: scale_born
     real(default) :: fac_scale_born
     real(default) :: ren_scale_born
  end type real_scales_t

  type :: pdf_container_t
     real(default), dimension(-6:6) :: f
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
     logical :: fixed_order_nlo = .false.
     logical :: with_virtual_subtraction = .true.
     logical :: test_soft_limit = .false.
     logical :: test_coll_limit = .false.
     logical :: test_anti_coll_limit = .false.
     integer :: fixed_alr = 0
     integer :: factorization_mode = NO_FACTORIZATION
     !!! Probably not the right place for this. Revisit after refactoring
     real(default) :: powheg_damping_scale = zero
     type(fks_template_t) :: fks_template
  contains
  procedure :: init => nlo_settings_init
    procedure :: write => nlo_settings_write
  end type nlo_settings_t


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
    write (u,'(1x,A)', advance = 'no') 'Mapping Type: '
    select case (template%mapping_type)
    case (FKS_DEFAULT)
       write (u,'(A)') 'Default'
    case (FKS_RESONANCES)
       write (u,'(A)') 'Resonances'
    case default
       write (u,'(A)') 'Unkown'
    end select
    write (u,'(1x,A,ES4.3,ES4.3)') 'd_ij exponentials: ', &
       template%fks_dij_exp1, template%fks_dij_exp2
  end subroutine fks_template_write

  subroutine fks_template_set_dij_exp (template, exp1, exp2)
    class(fks_template_t), intent(inout) :: template
    real(default), intent(in) :: exp1, exp2
    template%fks_dij_exp1 = exp1
    template%fks_dij_exp2 = exp2
  end subroutine fks_template_set_dij_exp

  subroutine fks_template_set_xi_and_y_bounds (template, xi_min, y_max)
    class(fks_template_t), intent(inout) :: template
    real(default), intent(in) :: xi_min, y_max
    template%xi_min = xi_min
    template%y_max = y_max
  end subroutine fks_template_set_xi_and_y_bounds

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

  function get_threshold_momenta (p) result (p_thr)
    type(vector4_t), dimension(4) :: p_thr
    type(vector4_t), intent(in), dimension(:) :: p
    p_thr(1) = p(THR_POS_WP) + p(THR_POS_B)
    p_thr(2) = p(THR_POS_B)
    p_thr(3) = p(THR_POS_WM) + p(THR_POS_BBAR)
    p_thr(4) = p(THR_POS_BBAR)
  end function get_threshold_momenta

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


end module nlo_data

