! WHIZARD 2.7.1 Mar 27 2019
!
! Copyright (C) 1999-2019 by
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

module nlo_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use constants, only: zero
  use string_utils, only: split_string, read_ival, string_contains_word
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
  public :: get_threshold_momenta
  public :: nlo_settings_t

  integer, parameter, public :: FKS_DEFAULT = 1
  integer, parameter, public :: FKS_RESONANCES = 2

  integer, dimension(2), parameter, public :: ASSOCIATED_LEG_PAIR = [1, 3]


  type :: fks_template_t
    logical :: subtraction_disabled = .false.
    integer :: mapping_type = FKS_DEFAULT
    logical :: count_kinematics = .false.
    real(default) :: fks_dij_exp1
    real(default) :: fks_dij_exp2
    real(default) :: xi_min
    real(default) :: y_max
    real(default) :: xi_cut, delta_o, delta_i
    type(string_t), dimension(:), allocatable :: excluded_resonances
    integer :: n_f
  contains
    procedure :: write => fks_template_write
    procedure :: set_parameters => fks_template_set_parameters
    procedure :: set_mapping_type => fks_template_set_mapping_type
    procedure :: set_counter => fks_template_set_counter
  end type fks_template_t

  type :: real_scales_t
     real(default) :: scale
     real(default) :: ren_scale
     real(default) :: fac_scale
     real(default) :: scale_born
     real(default) :: fac_scale_born
     real(default) :: ren_scale_born
  end type real_scales_t

  type :: nlo_settings_t
     logical :: use_internal_color_correlations = .true.
     logical :: use_internal_spin_correlations = .false.
     logical :: use_resonance_mappings = .false.
     logical :: combined_integration = .false.
     logical :: fixed_order_nlo = .false.
     logical :: test_soft_limit = .false.
     logical :: test_coll_limit = .false.
     logical :: test_anti_coll_limit = .false.
     integer, dimension(:), allocatable :: selected_alr
     integer :: factorization_mode = NO_FACTORIZATION
     !!! Probably not the right place for this. Revisit after refactoring
     real(default) :: powheg_damping_scale = zero
     type(fks_template_t) :: fks_template
     type(string_t) :: virtual_selection
     logical :: virtual_resonance_aware_collinear = .true.
     logical :: use_born_scale = .true.
     logical :: cut_all_sqmes = .true.
     type(string_t) :: nlo_correction_type
  contains
  procedure :: init => nlo_settings_init
    procedure :: write => nlo_settings_write
  end type nlo_settings_t



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
    write (u, '(1x,A,ES4.3,ES4.3)') 'xi_cut: ', &
       template%xi_cut
    write (u, '(1x,A,ES4.3,ES4.3)') 'delta_o: ', &
       template%delta_o
    write (u, '(1x,A,ES4.3,ES4.3)') 'delta_i: ', &
         template%delta_i
  end subroutine fks_template_write

  subroutine fks_template_set_parameters (template, exp1, exp2, xi_min, &
    y_max, xi_cut, delta_o, delta_i)
    class(fks_template_t), intent(inout) :: template
    real(default), intent(in) :: exp1, exp2
    real(default), intent(in) :: xi_min, y_max, &
         xi_cut, delta_o, delta_i
    template%fks_dij_exp1 = exp1
    template%fks_dij_exp2 = exp2
    template%xi_min = xi_min
    template%y_max = y_max
    template%xi_cut = xi_cut
    template%delta_o = delta_o
    template%delta_i = delta_i
  end subroutine fks_template_set_parameters

  subroutine fks_template_set_mapping_type (template, val)
    class(fks_template_t), intent(inout) :: template
    integer, intent(in) :: val
    template%mapping_type = val
  end subroutine fks_template_set_mapping_type

  subroutine fks_template_set_counter (template)
    class(fks_template_t), intent(inout) :: template
    template%count_kinematics = .true.
  end subroutine fks_template_set_counter

  function get_threshold_momenta (p) result (p_thr)
    type(vector4_t), dimension(4) :: p_thr
    type(vector4_t), intent(in), dimension(:) :: p
    p_thr(1) = p(THR_POS_WP) + p(THR_POS_B)
    p_thr(2) = p(THR_POS_B)
    p_thr(3) = p(THR_POS_WM) + p(THR_POS_BBAR)
    p_thr(4) = p(THR_POS_BBAR)
  end function get_threshold_momenta

  subroutine nlo_settings_init (nlo_settings, var_list, fks_template)
    class(nlo_settings_t), intent(inout) :: nlo_settings
    type(var_list_t), intent(in) :: var_list
    type(fks_template_t), intent(in), optional :: fks_template
    type(string_t) :: color_method
    if (present (fks_template)) nlo_settings%fks_template = fks_template
    color_method = var_list%get_sval (var_str ('$correlation_me_method'))
    if (color_method == "")  color_method = var_list%get_sval (var_str ('$method'))
    nlo_settings%use_internal_color_correlations = color_method == 'omega' &
           .or. color_method == 'threshold'
    nlo_settings%combined_integration = var_list%get_lval &
           (var_str ("?combined_nlo_integration"))
    nlo_settings%fixed_order_nlo = var_list%get_lval &
           (var_str ("?fixed_order_nlo_events"))
    nlo_settings%test_soft_limit = var_list%get_lval (var_str ('?test_soft_limit'))
    nlo_settings%test_coll_limit = var_list%get_lval (var_str ('?test_coll_limit'))
    nlo_settings%test_anti_coll_limit = var_list%get_lval (var_str ('?test_anti_coll_limit'))
    call setup_alr_selection ()
    nlo_settings%virtual_selection = var_list%get_sval (var_str ('$virtual_selection'))
    nlo_settings%virtual_resonance_aware_collinear = &
         var_list%get_lval (var_str ('?virtual_collinear_resonance_aware'))
    nlo_settings%powheg_damping_scale = &
         var_list%get_rval (var_str ('powheg_damping_scale'))
    nlo_settings%use_born_scale = &
         var_list%get_lval (var_str ("?nlo_use_born_scale"))
    nlo_settings%cut_all_sqmes = &
         var_list%get_lval (var_str ("?nlo_cut_all_sqmes"))
    nlo_settings%nlo_correction_type = var_list%get_sval (var_str ('$nlo_correction_type'))
  contains
    subroutine setup_alr_selection ()
      type(string_t) :: alr_selection
      type(string_t), dimension(:), allocatable :: alr_split
      integer :: i, i1, i2
      alr_selection = var_list%get_sval (var_str ('$select_alpha_regions'))
      if (string_contains_word (alr_selection, var_str (","))) then
         call split_string (alr_selection, var_str (","), alr_split)
         allocate (nlo_settings%selected_alr (size (alr_split)))
         do i = 1, size (alr_split)
            nlo_settings%selected_alr(i) = read_ival(alr_split(i))
         end do
      else if (string_contains_word (alr_selection, var_str (":"))) then
         call split_string (alr_selection, var_str (":"), alr_split)
         if (size (alr_split) == 2) then
            i1 = read_ival (alr_split(1))
            i2 = read_ival (alr_split(2))
            allocate (nlo_settings%selected_alr (i2 - i1 + 1))
            do i = 1, i2 - i1 + 1
               nlo_settings%selected_alr(i) = read_ival (alr_split(i))
            end do
         else
            call msg_fatal ("select_alpha_regions: ':' specifies a range!")
         end if
      else if (len(alr_selection) == 1) then
         allocate (nlo_settings%selected_alr (1))
         nlo_settings%selected_alr(1) = read_ival (alr_selection)
      end if
      if (allocated (alr_split)) deallocate (alr_split)
    end subroutine setup_alr_selection
  end subroutine nlo_settings_init

  subroutine nlo_settings_write (nlo_settings, unit)
    class(nlo_settings_t), intent(in) :: nlo_settings
    integer, intent(in), optional :: unit
    integer :: i, u
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
    write (u, '(3X,A,L1)') 'test_soft_limit = ', &
         nlo_settings%test_soft_limit
    write (u, '(3X,A,L1)') 'test_coll_limit = ', &
         nlo_settings%test_coll_limit
    write (u, '(3X,A,L1)') 'test_anti_coll_limit = ', &
         nlo_settings%test_anti_coll_limit
    if (allocated (nlo_settings%selected_alr)) then
       write (u, '(3x,A)', advance = "no") 'selected alpha regions = ['
       do i = 1, size (nlo_settings%selected_alr)
          write (u, '(A,I0)', advance = "no") ",", nlo_settings%selected_alr(i)
       end do
       write (u, '(A)') "]"
    end if
    write (u, '(3X,A,' // FMT_15 // ')') 'powheg_damping_scale = ', &
         nlo_settings%powheg_damping_scale
    write (u, '(3X,A,A)') 'virtual_selection = ', &
         char (nlo_settings%virtual_selection)
    write (u, '(3X,A,A)') 'Real factorization mode = ', &
         char (factorization_mode (nlo_settings%factorization_mode))
  contains
    function factorization_mode (fm)
      type(string_t) :: factorization_mode
      integer, intent(in) :: fm
      select case (fm)
      case (NO_FACTORIZATION)
         factorization_mode = var_str ("None")
      case (FACTORIZATION_THRESHOLD)
         factorization_mode = var_str ("Threshold")
      case default
         factorization_mode = var_str ("Undefined!")
      end select
    end function factorization_mode
  end subroutine nlo_settings_write


end module nlo_data

