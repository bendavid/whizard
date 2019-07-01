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

module fks_regions

  use kinds, only: default
  use format_utils, only: write_separator
  use numeric_utils, only: remove_duplicates_from_list
  use io_units
  use os_interface, only: os_data_t, os_data_init
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use flavors
  use process_constants
  use lorentz
  use pdg_arrays
  use models
  use physics_defs
  use cascades
  use nlo_data, only: phs_identifier_t, check_for_phs_identifier
  use nlo_data, only: FKS_DEFAULT, FKS_RESONANCES
  use nlo_data, only: NO_FACTORIZATION, FACTORIZATION_THRESHOLD

  use ttv_formfactors, only: THR_POS_B, THR_POS_BBAR
  use ttv_formfactors, only: THR_POS_WP, THR_POS_WM


  implicit none
  private

  public :: ftuple_t
  public :: flv_structure_t
  public :: singular_region_t
  public :: fks_mapping_default_t
  public :: fks_mapping_resonances_t
  public :: region_data_t
  public :: assignment(=)
  public :: create_resonance_histories_for_threshold
  public :: setup_region_data_for_test

  integer, parameter, public :: N_MAX_ALR = 200
  integer, parameter, public :: N_MAX_FLV = 50

  integer, parameter :: UNDEFINED_SPLITTING = 0
  integer, parameter :: Q_TO_QG = 1
  integer, parameter :: G_TO_GG = 2
  integer, parameter :: G_TO_QQ = 3


  type :: ftuple_t
    integer, dimension(2) :: ireg
    integer :: i_res = 0
    integer :: splitting_type
    logical :: pseudo_isr = .false.
  contains
    procedure :: write => ftuple_write
    procedure :: get => ftuple_get
    procedure :: set => ftuple_set
    procedure :: determine_splitting_type_fsr => ftuple_determine_splitting_type_fsr
    procedure :: determine_splitting_type_isr => ftuple_determine_splitting_type_isr
  end type ftuple_t

  type :: ftuple_list_t
    integer :: index = 0
    type(ftuple_t) :: ftuple
    type(ftuple_list_t), pointer :: next => null ()
    type(ftuple_list_t), pointer :: prev => null ()
    type(ftuple_list_t), pointer :: equiv => null ()
  contains
     procedure :: write => ftuple_list_write
     procedure :: append => ftuple_list_append
     procedure :: get_n_tuples => ftuple_list_get_n_tuples
     procedure :: get_entry => ftuple_list_get_entry
     procedure :: get_ftuple => ftuple_list_get_ftuple
     procedure :: set_equiv => ftuple_list_set_equiv
     procedure :: check_equiv => ftuple_list_check_equiv
  end type ftuple_list_t

  type :: flv_structure_t
    integer, dimension(:), allocatable :: flst
    integer :: nlegs = 0
    integer :: n_in = 0
    logical, dimension(:), allocatable :: massive
    logical, dimension(:), allocatable :: colored
  contains
    procedure :: valid_pair => flv_structure_valid_pair
    procedure :: remove_particle => flv_structure_remove_particle
    procedure :: insert_particle_fsr => flv_structure_insert_particle_fsr
    procedure :: insert_particle_isr => flv_structure_insert_particle_isr
    procedure :: insert_particle => flv_structure_insert_particle
    procedure :: get_nlegs => flv_structure_get_nlegs
    procedure :: count_particle => flv_structure_count_particle
    procedure :: init => flv_structure_init
    procedure :: write => flv_structure_write
    procedure :: create_uborn => flv_structure_create_uborn
    procedure :: init_mass_and_color => flv_structure_init_mass_and_color
    procedure :: final => flv_structure_final
  end type flv_structure_t

  type :: singular_region_t
    integer :: alr
    integer :: i_res
    type(flv_structure_t) :: flst_real
    type(flv_structure_t) :: flst_uborn
    integer :: mult
    integer :: emitter
    integer :: nregions
    integer :: real_index
    type(ftuple_t), dimension(:), allocatable :: ftuples
    integer :: uborn_index
    logical :: double_fsr
    logical :: soft_divergence
    logical :: coll_divergence
    integer, dimension(:), allocatable :: i_reg_to_i_con
    logical :: pseudo_isr = .false.
  contains
    procedure :: init => singular_region_init
    procedure :: write => singular_region_write
    procedure :: set_splitting_info => singular_region_set_splitting_info
    procedure :: double_fsr_factor => singular_region_double_fsr_factor
    procedure :: has_soft_divergence => singular_region_has_soft_divergence
    procedure :: has_collinear_divergence => &
              singular_region_has_collinear_divergence
  end type singular_region_t

  type :: resonance_mapping_t
    type(resonance_history_t), dimension(:), allocatable :: res_histories
    integer, dimension(:), allocatable :: alr_to_i_res
    integer, dimension(:,:), allocatable :: i_res_to_alr
    type(vector4_t), dimension(:), allocatable :: p_res
  contains
    procedure :: init => resonance_mapping_init
    procedure :: set_alr_to_i_res => resonance_mapping_set_alr_to_i_res
    procedure :: get_resonance_history => resonance_mapping_get_resonance_history
    procedure :: write => resonance_mapping_write
    procedure :: get_resonance_value => resonance_mapping_get_resonance_value
    procedure :: get_resonance_all => resonance_mapping_get_resonance_all
    procedure :: get_resonance_alr => resonance_mapping_get_resonance_alr
  end type resonance_mapping_t

  type, abstract :: fks_mapping_t
     real(default) :: sumdij
     real(default) :: sumdij_soft
  contains
    procedure (fks_mapping_dij), deferred :: dij
    procedure (fks_mapping_compute_sumdij), deferred :: compute_sumdij
    procedure (fks_mapping_svalue), deferred :: svalue
    procedure (fks_mapping_dij_soft), deferred :: dij_soft
    procedure (fks_mapping_compute_sumdij_soft), deferred :: compute_sumdij_soft
    procedure (fks_mapping_svalue_soft), deferred :: svalue_soft
  end type fks_mapping_t

  type, extends (fks_mapping_t) :: fks_mapping_default_t
    real(default) :: exp_1, exp_2
    integer :: n_in
  contains
    procedure :: set_parameter => fks_mapping_default_set_parameter
    procedure :: dij => fks_mapping_default_dij
    procedure :: compute_sumdij => fks_mapping_default_compute_sumdij
    procedure :: svalue => fks_mapping_default_svalue
    procedure :: dij_soft => fks_mapping_default_dij_soft
    procedure :: compute_sumdij_soft => fks_mapping_default_compute_sumdij_soft
    procedure :: svalue_soft => fks_mapping_default_svalue_soft
  end type fks_mapping_default_t

  type, extends (fks_mapping_t) :: fks_mapping_resonances_t
    real(default) :: exp_1, exp_2
    type(resonance_mapping_t) :: res_map
    integer :: i_con = 0
    logical :: pseudo_isr = .false.
  contains
    procedure :: dij => fks_mapping_resonances_dij
    procedure :: compute_sumdij => fks_mapping_resonances_compute_sumdij
    procedure :: svalue => fks_mapping_resonances_svalue
    procedure :: dij_soft => fks_mapping_resonances_dij_soft
    procedure :: compute_sumdij_soft => fks_mapping_resonances_compute_sumdij_soft
    procedure :: svalue_soft => fks_mapping_resonances_svalue_soft
    procedure :: set_resonance_momentum => fks_mapping_resonances_set_resonance_momentum
    procedure :: set_resonance_momenta => fks_mapping_resonances_set_resonance_momenta
  end type fks_mapping_resonances_t

  type :: region_data_t
    type(singular_region_t), dimension(:), allocatable :: regions
    type(flv_structure_t), dimension(:), allocatable :: flv_born
    type(flv_structure_t), dimension(:), allocatable :: flv_real
    integer, dimension(:), allocatable :: emitters
    integer :: n_regions
    integer :: n_emitters
    integer :: n_flv_born, n_flv_real
    integer :: n_in
    integer :: n_legs_born, n_legs_real
    integer, dimension(:), allocatable :: underlying_borns
    type(flavor_t) :: flv_extra
    class(fks_mapping_t), allocatable :: fks_mapping
    integer, dimension(:), allocatable :: resonances
    integer :: n_phs
    type(resonance_contributors_t), dimension(:), allocatable :: alr_contributors
    integer, dimension(:), allocatable :: alr_to_i_contributor
  contains
    procedure :: allocate_fks_mappings => region_data_allocate_fks_mappings
    procedure :: init => region_data_init
    procedure :: init_resonance_information => region_data_init_resonance_information
    procedure :: enlarge_singular_regions_with_resonances &
       => region_data_enlarge_singular_regions_with_resonances
    procedure :: set_isr_pseudo_regions => region_data_set_isr_pseudo_regions
    procedure :: evaluate_flavors => region_data_evaluate_flavors
    procedure :: uses_resonances => region_data_uses_resonances
    procedure :: get_emitter_list => region_data_get_emitter_list
    procedure :: get_associated_resonances => region_data_get_associated_resonances
    procedure :: emitter_is_compatible_with_resonance => &
       region_data_emitter_is_compatible_with_resonance
    procedure :: emitter_is_in_resonance => region_data_emitter_is_in_resonance
    procedure :: get_contributors => region_data_get_contributors
    procedure :: get_emitter => region_data_get_emitter
    procedure :: get_underlying_born_index => region_data_get_underlying_born_index
    procedure :: get_uborn_group_size => region_data_get_uborn_group_size
    procedure :: get_uborn_group => region_data_get_uborn_group
    procedure :: get_emitter_group_size => region_data_get_emitter_group_size
    procedure :: get_emitter_group => region_data_get_emitter_group
    procedure :: get_svalue => region_data_get_svalue
    procedure :: get_svalue_soft => region_data_get_svalue_soft
    procedure :: find_regions => region_data_find_regions
    procedure :: init_singular_regions => region_data_init_singular_regions
    procedure :: find_emitters => region_data_find_emitters
    procedure :: find_resonances => region_data_find_resonances
    procedure :: set_contributors => region_data_set_contributors
    procedure :: extend_ftuples => region_data_extend_ftuples
    procedure :: set_underlying_borns => region_data_set_underlying_borns
    procedure :: compute_number_of_phase_spaces &
       => region_data_compute_number_of_phase_spaces
    procedure :: set_splitting_info => region_data_set_splitting_info
    procedure :: write_to_file => region_data_write_to_file
    procedure :: write => region_data_write
    procedure :: final => region_data_final
  end type region_data_t


  interface assignment(=)
     module procedure singular_region_assign
  end interface
 
  interface assignment(=)
     module procedure resonance_mapping_assign
  end interface

  interface operator(==)
    module procedure flv_structure_equivalent
  end interface
  
  interface assignment(=)
    module procedure flv_structure_assign_flv
    module procedure flv_structure_assign_integer
  end interface

  interface assignment(=)
     module procedure region_data_assign
  end interface

  abstract interface
    function fks_mapping_dij (map, p, i, j, i_con) result (d)
      import
      real(default) :: d
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p
      integer, intent(in) :: i, j
      integer, intent(in), optional :: i_con
    end function fks_mapping_dij
  end interface

  abstract interface
    subroutine fks_mapping_compute_sumdij (map, sregion, p)
      import
      class(fks_mapping_t), intent(inout) :: map
      type(singular_region_t), intent(in) :: sregion
      type(vector4_t), intent(in), dimension(:) :: p
    end subroutine fks_mapping_compute_sumdij
  end interface

  abstract interface
    function fks_mapping_svalue (map, p, i, j, i_res) result (value)
      import
      real(default) :: value
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p
      integer, intent(in) :: i, j
      integer, intent(in), optional :: i_res
    end function fks_mapping_svalue
  end interface

  abstract interface
    function fks_mapping_dij_soft (map, p_born, p_soft, em, i_con) result (d)
      import
      real(default) :: d
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
      integer, intent(in) :: em
      integer, intent(in), optional :: i_con
    end function fks_mapping_dij_soft
  end interface

  abstract interface
    subroutine fks_mapping_compute_sumdij_soft (map, sregion, p_born, p_soft)
      import
      class(fks_mapping_t), intent(inout) :: map
      type(singular_region_t), intent(in) :: sregion
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
    end subroutine fks_mapping_compute_sumdij_soft
  end interface
  abstract interface
    function fks_mapping_svalue_soft (map, p_born, p_soft, em, i_res) result (value)
      import
      real(default) :: value
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
      integer, intent(in) :: em
      integer, intent(in), optional :: i_res
    end function fks_mapping_svalue_soft
  end interface

  interface assignment(=)
     module procedure fks_mapping_default_assign
  end interface

  interface assignment(=)
     module procedure fks_mapping_resonances_assign
  end interface


contains

  subroutine ftuple_write (ftuple, unit)
    class(ftuple_t), intent(in) :: ftuple
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    if (ftuple%i_res > 0) then
       print *, 'Check : ', ftuple%i_res
       write (u, "(A1,I1,A1,I1,A1,I1,A1)") &
         '(', ftuple%ireg(1), ',', ftuple%ireg(2), ';', ftuple%i_res, ')'
    else
       write (u, "(A1,I1,A1,I1,A1)") &
         '(', ftuple%ireg(1), ',', ftuple%ireg(2), ')'
    end if
  end subroutine ftuple_write

  subroutine ftuple_get (ftuple, pos1, pos2)
    class(ftuple_t), intent(in) :: ftuple
    integer, intent(out) :: pos1, pos2
    pos1 = ftuple%ireg(1)
    pos2 = ftuple%ireg(2)
  end subroutine ftuple_get

  subroutine ftuple_set (ftuple, pos1, pos2)
    class(ftuple_t), intent(inout) :: ftuple
    integer, intent(in) ::  pos1, pos2
    ftuple%ireg(1) = pos1
    ftuple%ireg(2) = pos2
  end subroutine ftuple_set

  subroutine ftuple_determine_splitting_type_fsr (ftuple, flv, i, j)
    class(ftuple_t), intent(inout) :: ftuple
    type(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i, j
    associate (flst => flv%flst)
       if (flst(i) == GLUON .and. flst(j) == GLUON) then
          ftuple%splitting_type = G_TO_GG
       else if (flst(i)+flst(j) == 0 &
             .and. is_quark (abs(flst(i)))) then
          ftuple%splitting_type = G_TO_QQ
       else if (is_quark(abs(flst(i))) .and. flst(j) == GLUON &
             .or. is_quark(abs(flst(j))) .and. flst(i) == GLUON) then
          ftuple%splitting_type = Q_TO_QG
       else
          ftuple%splitting_type = UNDEFINED_SPLITTING
       end if
    end associate
  end subroutine ftuple_determine_splitting_type_fsr

  subroutine ftuple_determine_splitting_type_isr (ftuple, flv, i, j)
    class(ftuple_t), intent(inout) :: ftuple
    type(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i, j
    integer :: em
    em = i; if (i == 0) em = 1
    associate (flst => flv%flst)
       if (flst(em) == GLUON .and. flst(j) == GLUON) then
          ftuple%splitting_type = G_TO_GG
       else if (flst(em) == GLUON .and. is_quark(abs(flst(j)))) then
          ftuple%splitting_type = G_TO_QQ
       else if (is_quark(abs(flst(em))) .and. flst(j) == GLUON) then
          ftuple%splitting_type = Q_TO_QG
       else
          ftuple%splitting_type = UNDEFINED_SPLITTING
       end if
    end associate
  end subroutine ftuple_determine_splitting_type_isr

  subroutine ftuple_list_write (list)
    class(ftuple_list_t), intent(in), target :: list
    type(ftuple_list_t), pointer :: current
    select type (list)
    type is (ftuple_list_t)
       current => list
       do
          call current%ftuple%write
          if (associated (current%next)) then
             current => current%next
          else
             exit
          end if
       end do
    end select
  end subroutine ftuple_list_write

  subroutine ftuple_list_append (list, ftuple)
   class(ftuple_list_t), intent(inout), target :: list
   type(ftuple_t), intent(in) :: ftuple
   type(ftuple_list_t), pointer :: current

   select type (list)
   type is (ftuple_list_t)
   if (list%index == 0) then
      nullify(list%next)
      list%index = 1
      list%ftuple = ftuple
   else
      current => list
      do
       if (associated (current%next)) then
         current => current%next
       else
         allocate (current%next)
         nullify (current%next%next)
         nullify (current%next%equiv)
         current%next%prev => current
         current%next%index = current%index + 1
         current%next%ftuple = ftuple
         exit
       end if
     end do
   end if
   end select
  end subroutine ftuple_list_append

  function ftuple_list_get_n_tuples (list) result(n_tuples)
    class(ftuple_list_t), intent(in), target :: list
    integer :: n_tuples
    type(ftuple_list_t), pointer :: current
    n_tuples = 0
    select type (list)
    type is (ftuple_list_t)
       current => list
       if (current%index > 0) then
          n_tuples = 1
          do
             if (associated (current%next)) then
                current => current%next
                n_tuples = n_tuples + 1
             else
                exit
             end if
          end do
       end if
     end select
  end function ftuple_list_get_n_tuples

  function ftuple_list_get_entry (list, index) result (entry)
   class(ftuple_list_t), intent(in), target :: list
   integer, intent(in) :: index
   type(ftuple_list_t), pointer :: entry
   type(ftuple_list_t), pointer :: current
   integer :: i
   entry => null()
   select type (list)
   type is (ftuple_list_t)
      current => list
      if (index <= list%get_n_tuples ()) then
         if (index == 1) then
            entry => current
         else
            do i = 1, index - 1
               current => current%next
            end do
            entry => current
         end if
      else
         call msg_fatal &
              ("Index must be smaller or equal than the total number of regions!")
      end if
   end select
 end function ftuple_list_get_entry

  function ftuple_list_get_ftuple (list, index)  result (ftuple)
    class(ftuple_list_t), intent(in), target :: list
    integer, intent(in) :: index
    type(ftuple_t) :: ftuple
    type(ftuple_list_t), pointer :: entry
    entry => list%get_entry (index)
    ftuple = entry%ftuple
  end function ftuple_list_get_ftuple

  subroutine ftuple_list_set_equiv (list, i1, i2)
    class(ftuple_list_t), intent(in) :: list
    integer, intent(in) :: i1, i2
    type(ftuple_list_t), pointer :: list1, list2
    select type (list)
    type is (ftuple_list_t)
       list1 => list%get_entry (i1)
       list2 => list%get_entry (i2)
       do
          if (associated (list1%equiv)) then
             list1 => list1%equiv
          else
             exit
          end if
       end do
       list1%equiv => list2
    end select
  end subroutine ftuple_list_set_equiv

  function ftuple_list_check_equiv(list, i1, i2) result(eq)
    class(ftuple_list_t), intent(in) :: list
    integer, intent(in) :: i1, i2
    logical :: eq
    type(ftuple_list_t), pointer :: current
    eq = .false.
    select type (list)
    type is (ftuple_list_t)
       current => list%get_entry (i1)
       do
          if (associated (current%equiv)) then
             current => current%equiv
             if (current%index == i2) then
                eq = .true.
                exit
             end if
          else
             exit
          end if
       end do
    end select
  end function ftuple_list_check_equiv

  function flv_structure_valid_pair &
     (flv, i, j, flv_ref, model) result (valid)
    logical :: valid
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i,j
    type(flv_structure_t), intent(in) :: flv_ref
    type(model_t), intent(in) :: model
    integer :: k, n_orig
    type(flv_structure_t) :: flv_test
    integer, dimension(:), allocatable :: flv_orig
    valid = .false.
    if (all ([i, j] <= flv%n_in)) return
    call model%match_vertex &
         (flv%flst(i), flv%flst(j), flv_orig)
    n_orig = size (flv_orig)
    if (n_orig == 0) then
      return
    else
      do k = 1, n_orig
         if (any ([i, j] <= flv%n_in)) then
            flv_test = flv%insert_particle_isr (i, j, flv_orig(k))
         else
            flv_test = flv%insert_particle_fsr (i, j, flv_orig(k))
         end if
         valid = flv_ref == flv_test
         call flv_test%final ()
         if (valid) return
      end do
    end if
    deallocate (flv_orig)
  end function flv_structure_valid_pair

  function flv_structure_equivalent (flv1, flv2) result(equiv)
    logical :: equiv
    type(flv_structure_t), intent(in) :: flv1, flv2
    integer :: i, j, n
    logical, dimension(:), allocatable :: present, checked
    n = size (flv1%flst)
    equiv = .true.
    if (n /= size (flv2%flst)) then
       call msg_fatal &
          ('flv_structure_equivalent: flavor arrays do not have equal lengths')
    else if (flv1%n_in /= flv2%n_in) then
       call msg_fatal &
          ('flv_structure_equivalent: flavor arrays do not have equal n_in')
    else
       allocate (present(n))
       allocate (checked(n))
       present = .false.; checked = .false.
       do i = 1, flv1%n_in
          if (flv1%flst(i) /= flv2%flst(i)) then
             equiv = .false.
             return
          end if
       end do
       do i = flv1%n_in + 1 , n
          do j = flv1%n_in + 1 ,n
             if (flv1%flst(i) == flv2%flst(j) .and. .not. checked(j)) then
                present(i) = .true.
                checked(j) = .true.
                exit
             end if
          end do
       end do
       do i = flv1%n_in + 1 , n
          if (.not.present(i)) equiv = .false.
       end do
    end if
  end function flv_structure_equivalent

  subroutine flv_structure_assign_flv (flv_out, flv_in)
    type(flv_structure_t), intent(out) :: flv_out
    type(flv_structure_t), intent(in) :: flv_in
    flv_out%nlegs = flv_in%nlegs
    flv_out%n_in = flv_in%n_in
    if (allocated (flv_in%flst)) then
       allocate (flv_out%flst (size (flv_in%flst)))
       flv_out%flst = flv_in%flst
    end if
    if (allocated (flv_in%massive)) then
       allocate (flv_out%massive (size (flv_in%massive)))
       flv_out%massive = flv_in%massive
    end if
    if (allocated (flv_in%colored)) then
       allocate (flv_out%colored (size (flv_in%colored)))
       flv_out%colored = flv_in%colored
    end if
  end subroutine flv_structure_assign_flv

  subroutine flv_structure_assign_integer (flv_out, iarray)
    type(flv_structure_t), intent(out) :: flv_out
    integer, intent(in), dimension(:) :: iarray
    flv_out%nlegs = size (iarray)
    allocate (flv_out%flst (flv_out%nlegs))
    flv_out%flst = iarray
  end subroutine flv_structure_assign_integer

  function flv_structure_remove_particle (flv, index) result(flv_new)
    type(flv_structure_t) :: flv_new
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: index
    integer :: n1, n2
    n1 = size (flv%flst)
    n2 = n1 - 1
    allocate (flv_new%flst (n2))
    flv_new%nlegs = n2
    flv_new%n_in = flv%n_in
    if (index == 1) then
      flv_new%flst(1 : n2) = flv%flst(2 : n1)
    else if (index == n1) then
      flv_new%flst(1 : n2) = flv%flst(1 : n2)
    else
      flv_new%flst(1 : index - 1) = flv%flst(1 : index - 1)
      flv_new%flst(index : n2) = flv%flst(index + 1 : n1)
    end if
  end function flv_structure_remove_particle

  function flv_structure_insert_particle_fsr (flv, i1, i2, flv_add) result (flv_new)
    type(flv_structure_t) :: flv_new
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i1, i2, flv_add
    if (flv%flst(i1) + flv_add == 0 .or. flv%flst(i2) + flv_add == 0) then
       flv_new = flv%insert_particle (i1, i2, -flv_add)
    else
       flv_new = flv%insert_particle (i1, i2, flv_add)
    end if
  end function flv_structure_insert_particle_fsr
    
  function flv_structure_insert_particle_isr (flv, i_in, i_out, flv_add) result (flv_new)
    type(flv_structure_t) :: flv_new
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i_in, i_out, flv_add
    if (flv%flst(i_in) + flv_add == 0) then
       flv_new = flv%insert_particle (i_in, i_out, -flv_add)
    else
       flv_new = flv%insert_particle (i_in, i_out, flv_add)
    end if
  end function flv_structure_insert_particle_isr
    
  function flv_structure_insert_particle (flv, i1, i2, particle) result (flv_new)
    type(flv_structure_t) :: flv_new
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: i1, i2, particle
    type(flv_structure_t) :: flv_tmp
    integer :: n1, n2
    n1 = size (flv%flst)
    n2 = n1 - 1
    allocate (flv_new%flst(n2))
    flv_new%nlegs = n2
    flv_new%n_in = flv%n_in
    if (i1 < i2) then
      flv_tmp = flv%remove_particle (i1)
      flv_tmp = flv_tmp%remove_particle (i2 - 1)
    else if(i2 < i1) then
      flv_tmp = flv%remove_particle(i2)
      flv_tmp = flv_tmp%remove_particle(i1 - 1)
    else
      call msg_fatal ("flv_structure_insert_particle: Indices are identical!")
    end if
    if (i1 == 1) then
      flv_new%flst(1) = particle
      flv_new%flst(2 : n2) = flv_tmp%flst(1 : n2 - 1)
    else if (i1 == n1 .or. i1 == n2) then
      flv_new%flst(1 : n2 - 1) = flv_tmp%flst(1 : n2 - 1)
      flv_new%flst(n2) = particle
    else
      flv_new%flst(1 : i1 - 1) = flv_tmp%flst(1 : i1 - 1)
      flv_new%flst(i1) = particle
      flv_new%flst(i1 + 1 : n2) = flv_tmp%flst(i1 : n2 - 1)
    end if
  end function flv_structure_insert_particle

  function flv_structure_get_nlegs (flv) result(n)
    class(flv_structure_t), intent(in) :: flv
    integer :: n
    n = size (flv%flst)
  end function flv_structure_get_nlegs

  function flv_structure_count_particle (flv, part) result (n)
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: part
    integer :: n
    n = count (flv%flst == part)
  end function flv_structure_count_particle

  subroutine flv_structure_init (flv, aval, n_in)
    class(flv_structure_t), intent(inout) :: flv
    integer, intent(in), dimension(:) :: aval
    integer, intent(in) :: n_in
    integer :: n
    n = size (aval)
    allocate (flv%flst (n))
    flv%flst(1 : n) = aval(1 : n)
    flv%nlegs = n
    flv%n_in = n_in
  end subroutine flv_structure_init

  subroutine flv_structure_write (flv, unit)
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in), optional :: unit
    integer :: i, u
    u = given_output_unit (unit); if (u < 0) return
    write (u, '(A1)',advance = 'no') '['
    do i = 1, size(flv%flst) - 1
      write (u, '(I3,A1)', advance = 'no') flv%flst(i), ','
    end do
    write (u, '(I3,A1)') flv%flst(i), ']'
  end subroutine flv_structure_write

  function flv_structure_create_uborn (flv, emitter) result(flv_uborn)
    type(flv_structure_t) :: flv_uborn
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in) :: emitter
    integer n_legs
    integer :: f1, f2
    n_legs = size(flv%flst)
    allocate (flv_uborn%flst (n_legs - 1))
    if (emitter > flv%n_in) then
       f1 = flv%flst(n_legs); f2 = flv%flst(n_legs - 1)
       if (f1 == 21) then
          !!! Emitted particle is a gluon => just remove it
          flv_uborn = flv%remove_particle(n_legs)
       else if (is_quark (f1) .and. is_quark (f2) .and. f1 + f2 == 0) then
          !!! Emission type is a gluon splitting into two quars
          flv_uborn = flv%insert_particle(n_legs - 1, n_legs, 21)
       else
          call msg_fatal ("Create underlying Born: Unsupported splitting type.")
       end if
    else if (emitter > 0) then
       f1 = flv%flst(n_legs); f2 = flv%flst(emitter)
       if (f1 == 21) then
          flv_uborn = flv%remove_particle(n_legs)
       else if (is_quark (f1) .and. is_gluon (f2)) then
          flv_uborn = flv%insert_particle (emitter, n_legs, -f1)
       else if (is_quark (f1) .and. is_quark (f2) .and. f1 == f2) then
          flv_uborn = flv%insert_particle(emitter, n_legs, 21)
       end if
    else
       flv_uborn = flv%remove_particle (n_legs)
    end if
  end function flv_structure_create_uborn

  subroutine flv_structure_init_mass_and_color (flv, model)
    class(flv_structure_t), intent(inout) :: flv
    type(model_t), intent(in) :: model
    integer :: i
    type(flavor_t) :: flavor
    allocate (flv%massive (flv%nlegs), flv%colored(flv%nlegs))
    do i = 1, flv%nlegs
       call flavor%init (flv%flst(i), model)
       flv%massive(i) = flavor%get_mass () > 0
       flv%colored(i) = &
          is_quark (flv%flst(i)) .or. is_gluon (flv%flst(i))
    end do
  end subroutine flv_structure_init_mass_and_color

  subroutine flv_structure_final (flv)
    class(flv_structure_t), intent(inout) :: flv
    if (allocated (flv%flst)) deallocate (flv%flst)
    if (allocated (flv%flst)) deallocate (flv%massive)
    if (allocated (flv%flst)) deallocate (flv%colored)
  end subroutine flv_structure_final

  subroutine singular_region_init (sregion, alr, mult, i_res, &
         flst_real, flst_uborn, flv_born, emitter, ftuples, index)
    class(singular_region_t), intent(out) :: sregion
    integer, intent(in) :: alr, mult, i_res
    type(flv_structure_t), intent(in) :: flst_real
    type(flv_structure_t), intent(in) :: flst_uborn
    type(flv_structure_t), dimension(:), intent(in) :: flv_born
    integer, intent(in) :: emitter
    type(ftuple_list_t), intent(in), dimension(:), target :: ftuples
    integer, dimension(:), intent(in) :: index
    type(ftuple_list_t), pointer :: current_region
    integer :: i, i1, i2, nlegs
    call msg_debug (D_SUBTRACTION, "singular_region_init")
    call debug_input_values ()
    sregion%alr = alr
    sregion%mult = mult
    sregion%i_res = i_res
    sregion%flst_real = flst_real
    sregion%flst_uborn = flst_uborn
    sregion%emitter = emitter
    sregion%nregions = ftuples (index(alr))%get_n_tuples ()
    nlegs = size (flst_real%flst)
    allocate (sregion%ftuples (sregion%nregions))
    do i = 1, sregion%nregions
       current_region => ftuples (index(alr))%get_entry (i)
       if (.not. associated (current_region%equiv)) then
          call current_region%ftuple%get (i1, i2)
          if (i2 /= nlegs) call current_region%ftuple%set (i1, nlegs)
       end if
       sregion%ftuples (i) = current_region%ftuple
    end do
    do i = 1, size(flv_born)
       if (flv_born (i) == sregion%flst_uborn) then
          sregion%uborn_index = i
          exit
       end if
    end do
  contains
    subroutine debug_input_values()
      if (debug2_active (D_SUBTRACTION)) then
         print *, 'alr =    ', alr
         print *, 'mult =    ', mult
         print *, 'i_res =    ', i_res
         call flst_real%write ()
         call flst_uborn%write ()
         do i = 1, size (flv_born)
            call flv_born(i)%write ()
         end do
         print *, 'emitter =    ', emitter
         do i = 1, size (ftuples)
            call ftuples(i)%write ()
         end do
         print *, 'index =    ', index
      end if
    end subroutine debug_input_values
  end subroutine singular_region_init

  subroutine singular_region_write (sregion, unit, maxnregions)
    class(singular_region_t), intent(in) :: sregion
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: maxnregions
    character(len=7), parameter :: flst_format = "(I3,A1)"
    character(len=16), parameter :: ireg_format = "(A1,I1,A1,I1,A3)"
    character(len=22), parameter :: &
       ireg_format_resonant = "(A1,I1,A1,I1,A1,I1,A3)"
    character(len=7), parameter :: ireg_space_format = "(7X,A1)"
    integer :: nreal, nborn, i, u, mr, i1, i2
    integer :: nleft, nright, nreg, nreg_diff
    integer :: i_res
    character(len=3) :: closing_bracket
    u = given_output_unit (unit); if (u < 0) return
    mr = sregion%nregions; if (present (maxnregions))  mr = maxnregions
    nreal = size (sregion%flst_real%flst)
    nborn = size (sregion%flst_uborn%flst)
    call write_vline (u)
    write (u, '(A1)', advance = 'no') '['
    do i = 1, nreal - 1
       write (u, flst_format, advance = 'no') sregion%flst_real%flst(i), ','
    end do
    write (u, flst_format, advance = 'no') sregion%flst_real%flst(nreal), ']'
    call write_vline (u)
    write (u, '(I3)', advance = 'no') sregion%emitter
    call write_vline (u)
    write (u, '(I3)', advance = 'no') sregion%mult
    call write_vline (u)
    write (u, '(I4)', advance = 'no') sregion%nregions
    call write_vline (u)
    if (sregion%i_res > 0) then
       write (u, '(I3)', advance = 'no') sregion%i_res
       call write_vline (u)
    end if
    nreg = sregion%nregions
    if (nreg == mr) then
       nleft = 0
       nright = 0
    else
       nreg_diff = mr - nreg
       nleft = nreg_diff / 2
       if (mod(nreg_diff , 2) == 0) then
          nright = nleft
       else
          nright = nleft + 1
       end if
    end if
    if (nleft > 0) then
       do i = 1, nleft
          write(u, ireg_space_format, advance='no') ' '
       end do
    end if
    write (u,'(A1)', advance = 'no') '{'
    if (nreg > 1) then
       do i = 1, nreg - 1
          call sregion%ftuples(i)%get (i1, i2)
          if (sregion%ftuples(i)%pseudo_isr) then
             closing_bracket = ')*'
          else
             closing_bracket = ')'
          end if
          i_res = sregion%ftuples(i)%i_res
          if (i_res > 0) then
             write (u, ireg_format_resonant, advance = 'no') &
                '(', i1, ',', i2, ';', i_res, closing_bracket
          else
             write(u, ireg_format, advance = 'no') '(', i1, ',', i2, closing_bracket
          end if
       end do
    end if
    call sregion%ftuples(nreg)%get (i1, i2)
    i_res = sregion%ftuples(nreg)%i_res
    if (sregion%ftuples(nreg)%pseudo_isr) then
       closing_bracket = ')*}'
    else
       closing_bracket = ')}'
    end if
    if (i_res > 0) then
       write (u, ireg_format_resonant, advance = 'no') &
          '(', i1, ',', i2, '; ', i_res, closing_bracket
    else
       write (u, ireg_format, advance = 'no') '(', i1, ',', i2, closing_bracket
    end if
    if (nright > 0) then
       do i = 1, nright
          write(u, ireg_space_format, advance='no') ' '
       end do
    end if
    call write_vline (u)
    write (u,'(A1)',advance = 'no') '['
    do i = 1, nborn - 1
       write(u, flst_format, advance = 'no') sregion%flst_uborn%flst(i), ','
    end do
    write (u, flst_format, advance = 'no') sregion%flst_uborn%flst(nborn), ']'
    call write_vline (u)
    write (u, '(I7)', advance = 'no') sregion%uborn_index
    write (u, '(A)')
  end subroutine singular_region_write

  subroutine singular_region_set_splitting_info (region)
    class(singular_region_t), intent(inout) :: region
    integer :: i1, i2
    integer :: reg
    region%double_fsr = .false.
    associate (ftuple => region%ftuples)
       do reg = 1, region%nregions
          call ftuple(reg)%get (i1, i2)
          if (i1 /= region%emitter) then
             cycle
          else
             region%soft_divergence = &
                ftuple(reg)%splitting_type /= G_TO_QQ

             if (i1 == 0) then
               region%coll_divergence = .true.
             else
               region%coll_divergence = &
                    .not. region%flst_real%massive(i1)
             end if

             if (ftuple(reg)%splitting_type > 1) then
                region%double_fsr = .true.
                exit
             else if (ftuple(reg)%splitting_type == UNDEFINED_SPLITTING) then
                call msg_fatal ("All splittings should be defined!")
             end if
          end if
       end do
     end associate
  end subroutine singular_region_set_splitting_info

  function singular_region_double_fsr_factor (region, p) result (val)
    class(singular_region_t), intent(in) :: region
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: val
    real(default) :: E_rad, E_em
    if (region%double_fsr) then
      E_em = energy (p(region%emitter))
      E_rad = energy (p(region%flst_real%get_nlegs()))
      val = two * E_em / (E_em + E_rad)
    else
      val = one
    end if
  end function singular_region_double_fsr_factor

  function singular_region_has_soft_divergence (region) result (div)
     class(singular_region_t), intent(in) :: region
     logical :: div
     div = region%soft_divergence
  end function singular_region_has_soft_divergence

  function singular_region_has_collinear_divergence (region) result (div)
    class(singular_region_t), intent(in) :: region
    logical :: div
    div = region%coll_divergence
  end function singular_region_has_collinear_divergence

  subroutine singular_region_assign (reg_out, reg_in)
    type(singular_region_t), intent(out) :: reg_out
    type(singular_region_t), intent(in) :: reg_in
    reg_out%alr = reg_in%alr
    reg_out%flst_real = reg_in%flst_real
    reg_out%flst_uborn = reg_in%flst_uborn
    reg_out%mult = reg_in%mult
    reg_out%emitter = reg_in%emitter
    reg_out%nregions = reg_in%nregions
    reg_out%real_index = reg_in%real_index
    reg_out%uborn_index = reg_in%uborn_index
    reg_out%double_fsr = reg_in%double_fsr
    reg_out%soft_divergence = reg_in%soft_divergence
    reg_out%coll_divergence = reg_in%coll_divergence
    if (allocated (reg_in%ftuples)) then
       allocate (reg_out%ftuples (size (reg_in%ftuples)))
       reg_out%ftuples = reg_in%ftuples
    else
       call msg_bug ("singular_region_assign: Trying to copy a singular region without allocated ftuples!")
    end if
  end subroutine singular_region_assign

  subroutine resonance_mapping_init (res_map, res_hist)
    class(resonance_mapping_t), intent(inout) :: res_map
    type(resonance_history_t), intent(in), dimension(:) :: res_hist
    integer :: nleg_out
    integer :: n_hist, i_hist1, i_hist2, n_contributors
    n_contributors = 0
    n_hist = size (res_hist)
    allocate (res_map%res_histories (n_hist))
    do i_hist1 = 1, n_hist
       if (i_hist1 + 1 <= n_hist) then
          do i_hist2 = i_hist1 + 1, n_hist
             if (.not. res_hist(i_hist1) > res_hist(i_hist2)) &
                n_contributors = n_contributors + res_hist(i_hist2)%n_resonances
          end do
       else
          n_contributors = n_contributors + res_hist(i_hist1)%n_resonances
       end if
    end do
    allocate (res_map%p_res (n_contributors))
    res_map%res_histories = res_hist
    res_map%p_res = vector4_null
  end subroutine resonance_mapping_init

  subroutine resonance_mapping_set_alr_to_i_res (res_map, regions, alr_new_to_old)
     class(resonance_mapping_t), intent(inout) :: res_map
     type(singular_region_t), intent(in), dimension(:) :: regions
     integer, intent(out), dimension(:), allocatable :: alr_new_to_old
     integer :: alr, i_res
     integer :: alr_new, n_alr_res
     integer :: k
     call msg_debug (D_SUBTRACTION, "resonance_mapping_set_alr_to_i_res")
     n_alr_res = 0
     do alr = 1, size (regions)
        do i_res = 1, size (res_map%res_histories)
           if (res_map%res_histories(i_res)%contains_leg (regions(alr)%emitter)) &
              n_alr_res = n_alr_res + 1
        end do
     end do

     allocate (res_map%alr_to_i_res (n_alr_res))
     allocate (res_map%i_res_to_alr (size (res_map%res_histories), 10))
     res_map%i_res_to_alr = 0
     allocate (alr_new_to_old (n_alr_res))
     alr_new = 1
     do alr = 1, size (regions)
        do i_res = 1, size (res_map%res_histories)
           if (res_map%res_histories(i_res)%contains_leg (regions(alr)%emitter)) then
              res_map%alr_to_i_res (alr_new) = i_res
              alr_new_to_old (alr_new) = alr
              alr_new = alr_new  + 1
           end if
        end do
     end do

     do i_res = 1, size (res_map%res_histories)
        k = 1
        do alr = 1, size (regions)
           if (res_map%res_histories(i_res)%contains_leg (regions(alr)%emitter)) then
              res_map%i_res_to_alr (i_res, k) = alr
              k = k + 1
           end if
        end do
     end do
     if (debug_active (D_SUBTRACTION)) then
        print *, 'i_res_to_alr:'
        do i_res = 1, size(res_map%i_res_to_alr, dim=1)
           print *, res_map%i_res_to_alr (i_res, :)
        end do
        print *, 'alr_new_to_old:', alr_new_to_old
     end if
  end subroutine resonance_mapping_set_alr_to_i_res

  function resonance_mapping_get_resonance_history (res_map, alr) result (res_hist)
     type(resonance_history_t) :: res_hist
     class(resonance_mapping_t), intent(in) :: res_map
     integer, intent(in) :: alr
     res_hist = res_map%res_histories(res_map%alr_to_i_res (alr))
  end function resonance_mapping_get_resonance_history

  subroutine resonance_mapping_write (res_map)
    class(resonance_mapping_t), intent(in) :: res_map
    integer :: i_res
    do i_res = 1, size (res_map%res_histories)
       call res_map%res_histories(i_res)%write ()
    end do
  end subroutine resonance_mapping_write

  function resonance_mapping_get_resonance_value (res_map, i_res, p, i_gluon) result (p_map)
    real(default) :: p_map
    class(resonance_mapping_t), intent(in) :: res_map
    integer, intent(in) :: i_res
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in), optional :: i_gluon
    p_map = res_map%res_histories(i_res)%mapping (p, i_gluon)
  end function resonance_mapping_get_resonance_value

  function resonance_mapping_get_resonance_all (res_map, alr, p, i_gluon) result (p_map)
    real(default) :: p_map
    class(resonance_mapping_t), intent(in) :: res_map
    integer, intent(in) :: alr
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in), optional :: i_gluon
    integer :: i_res
    p_map = zero
    do i_res = 1, size (res_map%res_histories)
       associate (res => res_map%res_histories(i_res))
          if (any (res_map%i_res_to_alr (i_res, :) == alr)) &
             p_map = p_map + res%mapping (p, i_gluon)
       end associate
    end do
  end function resonance_mapping_get_resonance_all

  function resonance_mapping_get_resonance_alr (res_map, alr, p, i_gluon) result (p_map)
    real(default) :: p_map
    class(resonance_mapping_t), intent(in) :: res_map
    integer, intent(in) :: alr
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in), optional :: i_gluon
    integer :: i_res
    i_res = res_map%alr_to_i_res (alr)
    p_map = res_map%res_histories(i_res)%mapping (p, i_gluon)
  end function resonance_mapping_get_resonance_alr

  subroutine resonance_mapping_assign (res_map_out, res_map_in)
    type(resonance_mapping_t), intent(out) :: res_map_out
    type(resonance_mapping_t), intent(in) :: res_map_in
    if (allocated (res_map_in%res_histories)) then
       allocate (res_map_out%res_histories (size (res_map_in%res_histories)))
       res_map_out%res_histories = res_map_in%res_histories
    end if
    if (allocated (res_map_in%alr_to_i_res)) then
       allocate (res_map_out%alr_to_i_res (size (res_map_in%alr_to_i_res)))
       res_map_out%alr_to_i_res = res_map_in%alr_to_i_res
    end if
    if (allocated (res_map_in%i_res_to_alr)) then
       allocate (res_map_out%i_res_to_alr &
          (size (res_map_in%i_res_to_alr, 1), size (res_map_in%i_res_to_alr, 2)))
       res_map_out%i_res_to_alr = res_map_in%i_res_to_alr
    end if
    if (allocated (res_map_in%p_res)) then
       allocate (res_map_out%p_res (size (res_map_in%p_res)))
       res_map_out%p_res = res_map_in%p_res
    end if
  end subroutine resonance_mapping_assign

  subroutine region_data_allocate_fks_mappings (reg_data, mapping_type)
    class(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: mapping_type

    select case (mapping_type)
    case (FKS_DEFAULT)
       allocate (fks_mapping_default_t :: reg_data%fks_mapping)
    case (FKS_RESONANCES)
       allocate (fks_mapping_resonances_t :: reg_data%fks_mapping)
    case default
       call msg_fatal ("Init region_data: FKS mapping not implemented!")
    end select
  end subroutine region_data_allocate_fks_mappings

  subroutine region_data_init (reg_data, n_in, model, flavor_born, &
    flavor_real)
    class(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: n_in
    type(model_t), intent(in) :: model
    integer, intent(in), dimension(:,:) :: flavor_born, flavor_real
    type(ftuple_list_t), dimension(:), allocatable :: ftuples
    integer, dimension(:), allocatable :: emitter
    type(flv_structure_t), dimension(:), allocatable :: flst_alr
    integer :: i
    reg_data%n_in = n_in
    reg_data%n_flv_born = size (flavor_born, dim = 2)
    reg_data%n_flv_real = size (flavor_real, dim = 2)
    reg_data%n_legs_born = size (flavor_born, dim = 1)
    reg_data%n_legs_real = reg_data%n_legs_born + 1
    allocate (reg_data%flv_born (reg_data%n_flv_born))
    allocate (reg_data%flv_real (reg_data%n_flv_real))
    do i = 1, reg_data%n_flv_born
       call reg_data%flv_born(i)%init (flavor_born (:, i), n_in)
    end do
    do i = 1, reg_data%n_flv_real
       call reg_data%flv_real(i)%init (flavor_real (:, i), n_in)
    end do

    call reg_data%flv_extra%init &
       (reg_data%flv_real(1)%flst(reg_data%n_legs_real), model)
    call reg_data%find_regions (model, ftuples, emitter, flst_alr)
    call reg_data%init_singular_regions (ftuples, emitter, flst_alr)
    call reg_data%find_emitters ()
    call reg_data%set_underlying_borns ()
    call reg_data%evaluate_flavors (model)
    call reg_data%set_splitting_info ()
  end subroutine region_data_init

  subroutine region_data_init_resonance_information (reg_data, factorization_mode)
    class(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: factorization_mode
    call reg_data%enlarge_singular_regions_with_resonances ()
    call reg_data%find_resonances ()
    select type (map => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       call map%res_map%write ()
    end select
    if (factorization_mode == FACTORIZATION_THRESHOLD) &
       call reg_data%set_isr_pseudo_regions ()
  end subroutine region_data_init_resonance_information

  subroutine region_data_enlarge_singular_regions_with_resonances (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: alr
    integer, dimension(:), allocatable :: alr_new_to_old
    integer :: n_alr_new
    integer, parameter :: n_max_resonances = 10
    type(singular_region_t), dimension(:), allocatable :: save_regions
    call msg_debug (D_SUBTRACTION, "region_data_enlarge_singular_regions_with_resonances")
    call debug_input_values ()
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_default_t)
       return
    type is (fks_mapping_resonances_t) 
       allocate (save_regions (reg_data%n_regions))
       do alr = 1, reg_data%n_regions
          save_regions(alr) = reg_data%regions(alr)
       end do

       associate (res_map => fks_mapping%res_map)
          call res_map%set_alr_to_i_res (reg_data%regions, alr_new_to_old)
          deallocate (reg_data%regions)
          n_alr_new = size (alr_new_to_old)
          reg_data%n_regions = n_alr_new
          allocate (reg_data%regions (n_alr_new))
          do alr = 1, n_alr_new
             reg_data%regions(alr) = save_regions(alr_new_to_old (alr))
             reg_data%regions(alr)%i_res = res_map%alr_to_i_res (alr)
          end do
       end associate
    end select

  contains

    subroutine debug_input_values ()
      if (debug2_active (D_SUBTRACTION)) then
         call reg_data%write ()
      end if
    end subroutine debug_input_values

  end subroutine region_data_enlarge_singular_regions_with_resonances

  subroutine region_data_set_isr_pseudo_regions (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: alr
    integer :: n_alr_new
    !!! Subroutine called for threshold factorization ->
    !!! Size of singular regions at this point is fixed
    type(singular_region_t), dimension(2) :: save_regions 
    integer, dimension(4) :: alr_new_to_old
    integer, dimension(2) :: alr_to_i_con_old
    do alr = 1, reg_data%n_regions
       save_regions(alr) = reg_data%regions(alr)
    end do
    n_alr_new = reg_data%n_regions * 2
    alr_new_to_old = [1, 1, 2, 2]
    alr_to_i_con_old = reg_data%alr_to_i_contributor
    deallocate (reg_data%regions)
    deallocate (reg_data%alr_to_i_contributor)
    allocate (reg_data%regions (n_alr_new))
    allocate (reg_data%alr_to_i_contributor (n_alr_new))
    reg_data%n_regions = n_alr_new
    do alr = 1, n_alr_new
       reg_data%regions(alr) = save_regions(alr_new_to_old (alr))
       call add_pseudo_emitters (reg_data%regions(alr))
       if (mod (alr, 2) == 0) reg_data%regions(alr)%pseudo_isr = .true.
       reg_data%regions(alr)%i_res = 1
       reg_data%alr_to_i_contributor(alr) = alr_to_i_con_old (alr_new_to_old (alr))
       allocate (reg_data%regions(alr)%i_reg_to_i_con(4))
       reg_data%regions(alr)%i_reg_to_i_con = [1, 1, 2, 2]
    end do
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       deallocate (fks_mapping%res_map%alr_to_i_res)
       allocate (fks_mapping%res_map%alr_to_i_res (4))
       fks_mapping%res_map%alr_to_i_res = [1, 1, 1, 1]
    end select
  contains
    subroutine add_pseudo_emitters (sregion)
      type(singular_region_t), intent(inout) :: sregion
      type(ftuple_t), dimension(2) :: ftuples_save
      integer :: alr
      do alr = 1, 2
         ftuples_save(alr) = sregion%ftuples(alr)
      end do
      deallocate (sregion%ftuples)
      sregion%nregions = sregion%nregions * 2
      allocate (sregion%ftuples (sregion%nregions))
      do alr = 1, sregion%nregions
         sregion%ftuples(alr) = ftuples_save (alr_new_to_old(alr))
         if (mod (alr, 2) == 0) sregion%ftuples(alr)%pseudo_isr = .true.
      end do
    end subroutine add_pseudo_emitters
  end subroutine region_data_set_isr_pseudo_regions

  subroutine region_data_evaluate_flavors (reg_data, model)
    class(region_data_t), intent(inout) :: reg_data
    type(model_t), intent(in) :: model
    integer :: i
    do i = 1, reg_data%n_regions
       associate (region => reg_data%regions(i))
          call region%flst_uborn%init_mass_and_color (model)
          call region%flst_real%init_mass_and_color (model)
       end associate
    end do
    do i = 1, reg_data%n_flv_born
       call reg_data%flv_born(i)%init_mass_and_color (model)
    end do
    do i = 1, reg_data%n_flv_real
       call reg_data%flv_real(i)%init_mass_and_color (model)
    end do
  end subroutine region_data_evaluate_flavors

  function region_data_uses_resonances (reg_data) result (val)
    logical :: val
    class(region_data_t), intent(in) :: reg_data
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       val = .true.
    class default
       val = .false.
    end select
  end function region_data_uses_resonances

  pure function region_data_get_emitter_list (reg_data) result(emitters)
    class(region_data_t), intent(in) :: reg_data
    integer, dimension(:), allocatable :: emitters
    integer :: i
    allocate (emitters (reg_data%n_regions))
    do i = 1, reg_data%n_regions
       emitters(i) = reg_data%regions(i)%emitter
    end do
  end function region_data_get_emitter_list

  function region_data_get_associated_resonances (reg_data, emitter) result (res)
    integer, dimension(:), allocatable :: res
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: emitter
    integer :: alr, i
    integer :: n_res
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       n_res = 0

       do alr = 1, reg_data%n_regions
          if (reg_data%regions(alr)%emitter == emitter) &
             n_res = n_res + 1
       end do

       if (n_res > 0) then
          allocate (res (n_res))
       else
          return
       end if
       i = 1

       do alr = 1, reg_data%n_regions
          if (reg_data%regions(alr)%emitter == emitter) then
             res (i) = fks_mapping%res_map%alr_to_i_res (alr)
             i = i + 1
          end if
       end do
    end select
  end function region_data_get_associated_resonances

  function region_data_emitter_is_compatible_with_resonance &
     (reg_data, i_res, emitter) result (compatible)
     logical :: compatible
     class(region_data_t), intent(in) :: reg_data
     integer, intent(in) :: i_res, emitter
     integer :: i_res_alr, alr
     compatible = .false.
     select type (fks_mapping => reg_data%fks_mapping)
     type is (fks_mapping_resonances_t)
        do alr = 1, reg_data%n_regions
           i_res_alr = fks_mapping%res_map%alr_to_i_res (alr)
           if (i_res_alr == i_res .and. reg_data%get_emitter(alr) == emitter) then
              compatible = .true.
              exit
           end if
        end do
     end select
  end function region_data_emitter_is_compatible_with_resonance

  function region_data_emitter_is_in_resonance (reg_data, i_res, emitter) result (exist)
    logical :: exist
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: i_res, emitter
    integer :: i
    exist = .false.
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       associate (res_history => fks_mapping%res_map%res_histories(i_res))
          do i = 1, res_history%n_resonances
             exist = exist .or. any (res_history%resonances(i)%contributors%c == emitter)
          end do
      end associate
    end select
  end function region_data_emitter_is_in_resonance

  subroutine region_data_get_contributors (reg_data, i_res, emitter, c, success)
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: i_res, emitter
    integer, intent(inout), dimension(:), allocatable :: c
    logical, intent(out) :: success
    integer :: i
    success = .false.
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       associate (res_history => fks_mapping%res_map%res_histories (i_res))
          do i = 1, res_history%n_resonances
             if (any (res_history%resonances(i)%contributors%c == emitter)) then
                allocate (c (size (res_history%resonances(i)%contributors%c)))
                c = res_history%resonances(i)%contributors%c
                success = .true.
                exit
             end if
          end do
       end associate
    end select
  end subroutine region_data_get_contributors

  pure function region_data_get_emitter (reg_data, alr) result (emitter)
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: alr
    integer :: emitter
    emitter = reg_data%regions(alr)%emitter
  end function region_data_get_emitter

  function region_data_get_underlying_born_index (reg_data, real_index) result (uborn_index)
    integer :: uborn_index
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: real_index
    integer :: alr
    uborn_index = 0
    do alr = 1, size (reg_data%regions)
       if (reg_data%regions(alr)%real_index == real_index) then
          uborn_index = reg_data%regions(alr)%uborn_index
          exit
       end if
    end do
  end function region_data_get_underlying_born_index

  function region_data_get_uborn_group_size (reg_data, uborn_index) result (n_uborn)
    integer :: n_uborn
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: uborn_index
    integer :: alr
    n_uborn = 0
    do alr = 1, reg_data%n_regions
       if (reg_data%regions(alr)%i_res > 1) cycle
       if (reg_data%regions(alr)%uborn_index == uborn_index) n_uborn = n_uborn + 1
    end do
  end function region_data_get_uborn_group_size

  function region_data_get_uborn_group (reg_data, uborn_index) result (uborn_group)
    integer, dimension(:), allocatable :: uborn_group
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: uborn_index
    integer :: alr, n_uborn, i_uborn
    n_uborn = reg_data%get_uborn_group_size (uborn_index)
    if (n_uborn > 0) then
       allocate (uborn_group (n_uborn))
       i_uborn = 1
       do alr = 1, reg_data%n_regions
          if (reg_data%regions(alr)%i_res > 1) cycle
          if (reg_data%regions(alr)%uborn_index == uborn_index) then
             uborn_group (i_uborn) = alr
             i_uborn = i_uborn + 1
          end if
       end do
    end if
  end function region_data_get_uborn_group

  function region_data_get_emitter_group_size (reg_data, emitter) result (n_emitter)
    integer :: n_emitter
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: emitter
    integer :: alr
    n_emitter = 0
    do alr = 1, reg_data%n_regions
       if (reg_data%regions(alr)%i_res > 1) cycle
       if (reg_data%regions(alr)%emitter == emitter) n_emitter = n_emitter + 1
    end do
  end function region_data_get_emitter_group_size

  function region_data_get_emitter_group (reg_data, emitter) result (emitter_group)
    integer, dimension(:), allocatable :: emitter_group
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: emitter
    integer :: alr, n_emitter, i_emitter
    n_emitter = reg_data%get_emitter_group_size (emitter)
    if (n_emitter > 0) then
       allocate (emitter_group (n_emitter))
       i_emitter = 1
       do alr = 1, reg_data%n_regions
          if (reg_data%regions(alr)%i_res > 1) cycle
          if (reg_data%regions(alr)%emitter == emitter) then
             emitter_group (i_emitter) = alr
             i_emitter = i_emitter + 1
          end if
       end do
    end if
  end function region_data_get_emitter_group

  function region_data_get_svalue (reg_data, p, alr, emitter, i_res) result (sval)
    class(region_data_t), intent(inout) :: reg_data
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: alr, emitter
    integer, intent(in) :: i_res
    real(default) :: sval
    associate (map => reg_data%fks_mapping)
       call map%compute_sumdij (reg_data%regions(alr), p)
       select type (map)
       type is (fks_mapping_resonances_t)
          map%i_con = reg_data%alr_to_i_contributor (alr)
          map%pseudo_isr = reg_data%regions(alr)%pseudo_isr
       end select
       sval = map%svalue (p, emitter, reg_data%n_legs_real, i_res)
    end associate
  end function region_data_get_svalue

  function region_data_get_svalue_soft &
       (reg_data, p, p_soft, alr, emitter, i_res) result (sval)
    class(region_data_t), intent(inout) :: reg_data
    type(vector4_t), intent(in), dimension(:) :: p
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: alr, emitter, i_res
    real(default) :: sval
    associate (map => reg_data%fks_mapping)
       call map%compute_sumdij_soft (reg_data%regions(alr), p, p_soft)
       select type (map)
       type is (fks_mapping_resonances_t)
          map%i_con = reg_data%alr_to_i_contributor (alr)
          map%pseudo_isr = reg_data%regions(alr)%pseudo_isr
       end select
       sval = map%svalue_soft (p, p_soft, emitter, i_res)
    end associate
  end function region_data_get_svalue_soft

  subroutine region_data_find_regions &
       (reg_data, model, ftuples, emitter, flst_alr)
    class(region_data_t), intent(in) :: reg_data
    type(model_t), intent(in) :: model
    type(ftuple_list_t), intent(out), dimension(:), allocatable :: ftuples
    integer, intent(out), dimension(:), allocatable :: emitter
    type(flv_structure_t), intent(out), dimension(:), allocatable :: flst_alr
    type(ftuple_t) :: current_ftuple
    integer, dimension(:), allocatable :: emitter_tmp
    type(flv_structure_t), dimension(:), allocatable :: flst_alr_tmp
    integer :: nreg, nborn, nreal
    integer :: nlegreal
    integer, parameter :: maxnregions = 200
    integer :: i_real

    nborn = size (reg_data%flv_born)
    nreal = size (reg_data%flv_real)
    nlegreal = size (reg_data%flv_real(1)%flst)
    allocate (ftuples (nreal))
    allocate (emitter_tmp (maxnregions))
    allocate (flst_alr_tmp (maxnregions))
    nreg = 0

    do i_real = 1, nreal
       call check_final_state_emissions (i_real, nreg)
       call check_initial_state_emissions (i_real, nreg)
    end do

    allocate (flst_alr (nreg))
    allocate (emitter (nreg))
    flst_alr(1 : nreg) = flst_alr_tmp(1 : nreg)
    emitter(1 : nreg) = emitter_tmp(1 : nreg)
       
  contains
    subroutine check_final_state_emissions (i_real, i_reg)
      integer, intent(in) :: i_real
      integer, intent(inout) :: i_reg
      integer :: leg1, leg2, i_born
      type(flv_structure_t) :: born_flavor
      logical :: valid1, valid2
      do leg1 = reg_data%n_in + 1, nlegreal
         do leg2 = leg1 + 1, nlegreal
            do i_born = 1, nborn
               born_flavor = reg_data%flv_born(i_born)
               associate (flv_real => reg_data%flv_real(i_real))
                  valid1 = flv_real%valid_pair(leg1, leg2, born_flavor, model)
                  valid2 = flv_real%valid_pair(leg2, leg1, born_flavor, model)
                  if ( valid1 .or. valid2) then
                     i_reg = i_reg + 1
                     if(valid1) then
                        flst_alr_tmp(i_reg) = &
                           create_alr (flv_real, reg_data%n_in, leg1, leg2)
                     else
                        flst_alr_tmp(i_reg) = &
                           create_alr (flv_real, reg_data%n_in, leg2, leg1)
                     end if
                     call current_ftuple%set (leg1, leg2)
                     call current_ftuple%determine_splitting_type_fsr &
                        (flv_real, leg1, leg2)
                     call ftuples(i_real)%append (current_ftuple)
                     emitter_tmp(i_reg) = nlegreal - 1
                     exit
                  end if
               end associate
            end do
         end do
      end do
    end subroutine check_final_state_emissions

    subroutine check_initial_state_emissions (i_real, i_reg)
      integer, intent(in) :: i_real
      integer, intent(inout) :: i_reg
      integer :: leg, i_born, emitter
      type(flv_structure_t) :: born_flavor
      logical :: valid1, valid2
      do leg = reg_data%n_in + 1, nlegreal
         do i_born = 1, nborn 
            born_flavor = reg_data%flv_born (i_born)
            associate (flv_real => reg_data%flv_real(i_real))
               valid1 = flv_real%valid_pair(1, leg, born_flavor, model)
               if (reg_data%n_in > 1) then
                  valid2 = flv_real%valid_pair(2, leg, born_flavor, model)
               else
                  valid2 = .false.
               end if
               if (valid1 .and. valid2) then
                  emitter = 0
               else if (valid1 .and. .not. valid2) then
                  emitter = 1
               else if (.not. valid1 .and. valid2) then
                  emitter = 2
               else
                  emitter = -1
               end if
               if (valid1 .or. valid2) then
                  i_reg = i_reg + 1
                  call current_ftuple%set(emitter, leg)
                  call current_ftuple%determine_splitting_type_isr &
                     (flv_real, emitter, leg)
                  call ftuples(i_real)%append (current_ftuple)
                  emitter_tmp(i_reg) = emitter
                  flst_alr_tmp(i_reg) = &
                     create_alr (flv_real, reg_data%n_in, emitter, leg)
               end if
            end associate
         end do
      end do
    end subroutine check_initial_state_emissions
  end subroutine region_data_find_regions

  subroutine region_data_init_singular_regions &
         (reg_data, ftuples, emitter, flv_alr)
    class(region_data_t), intent(inout) :: reg_data
    type(ftuple_list_t), intent(inout), dimension(:), allocatable :: ftuples
    integer :: n_valid_ftuples
    integer, intent(in), dimension(:) :: emitter
    type(flv_structure_t), intent(in), dimension(:) :: flv_alr
    type(flv_structure_t), dimension(:), allocatable :: flv_uborn, flv_alr2
    integer, dimension(:), allocatable :: mult
    integer, dimension(:), allocatable :: flst_emitter
    integer :: nregions, maxregions
    integer, dimension(:,:), allocatable :: perm_list
    integer, dimension(:), allocatable :: index
    integer :: i, j, k, l
    integer :: nlegs
    logical :: equiv
    integer :: i_first, j_first, i_res
    integer, dimension(:), allocatable :: &
         region_to_ftuple, ftuple_limits, k_index
    type(flv_structure_t) :: flv_save

    maxregions = size (emitter)
    nlegs = flv_alr(1)%nlegs

    allocate (flv_uborn (maxregions))
    allocate (flv_alr2 (maxregions))
    allocate (mult (maxregions))
    allocate (flst_emitter (maxregions))
    allocate (index (maxregions))
    allocate (region_to_ftuple (maxregions))
    allocate (k_index (maxregions))

    mult = 0
    n_valid_ftuples = 0
    do i = 1, size (ftuples)
       if (ftuples(i)%get_n_tuples() > 0) &
          n_valid_ftuples = n_valid_ftuples + 1
    end do
    allocate (ftuple_limits (n_valid_ftuples))

    j = 1
    do i = 1, size (ftuples)
       if (ftuples(i)%get_n_tuples() > 0) then
          ftuple_limits(j) = ftuples(i)%get_n_tuples ()
          j = j + 1
       end if
    end do
    if (.not. (sum (ftuple_limits) == maxregions)) &
       call msg_fatal ("Too many regions!")
    k = 1
    do j = 1, n_valid_ftuples
       do i = 1, ftuple_limits(j)
          region_to_ftuple(k) = i
          k = k + 1
       end do
    end do
    i_first = 1
    j_first = 1
    j = 1
    SCAN_REGIONS: do l = 1, n_valid_ftuples
       SCAN_FTUPLES: do i = i_first, i_first + ftuple_limits (l) - 1
          equiv = .false.
          if (i == i_first) then
             flv_alr2(j) = flv_alr(i)
             mult(j) = mult(j) + 1
             flv_uborn(j) = flv_alr(i)%create_uborn (emitter(i))
             flst_emitter(j) = emitter(i)
             index (j) = region_to_index(ftuples, i)
             k_index (j) = region_to_ftuple(i)
             j = j + 1
          else
             !!! Check for equivalent flavor structures
             do k = j_first, j - 1
                if (emitter(i) == emitter(k) .and. emitter(i) > reg_data%n_in) then
                   if (flv_alr(i) == flv_alr2(k) .and. &
                      flv_alr(i)%flst(nlegs - 1) == flv_alr2(k)%flst(nlegs - 1) &
                      .and. flv_alr(i)%flst(nlegs) == flv_alr2(k)%flst(nlegs)) then
                      mult(k) = mult(k) + 1
                      equiv = .true.
                      call ftuples (region_to_index(ftuples, i))%set_equiv &
                         (k_index(k), region_to_ftuple(i))
                      exit
                   end if
                else if (emitter(i) == emitter(k) .and. emitter(i) <= reg_data%n_in) then
                   if (flv_alr(i) == flv_alr2(k)) then
                      mult(k) = mult(k) + 1
                      equiv = .true.
                      call ftuples (region_to_index(ftuples,i))%set_equiv &
                         (k_index(k), region_to_ftuple(i))
                      exit
                   end if
                end if
             end do
             if (.not. equiv) then
                flv_alr2(j) = flv_alr(i)
                mult(j) = mult(j) + 1
                flv_uborn(j) = flv_alr(i)%create_uborn (emitter(i))
                flst_emitter(j) = emitter(i)
                index (j) = region_to_index (ftuples, i)
                k_index (j) = region_to_ftuple(i)
                j = j + 1
             end if
          end if
       end do SCAN_FTUPLES
       i_first = i_first + ftuple_limits(l)
       j_first = j_first + j - 1
    end do SCAN_REGIONS
    nregions = j - 1
    allocate (reg_data%regions (nregions))
    reg_data%n_regions = nregions
    do j = 1, nregions
       do i = 1, reg_data%n_flv_born
          if (reg_data%flv_born (i) == flv_uborn (j)) then
             if (allocated (perm_list)) deallocate (perm_list)
             call fks_permute_born &
                (reg_data%flv_born (i), reg_data%n_in, flv_uborn (j), perm_list)
             call fks_apply_perm (flv_alr2(j), flst_emitter(j), perm_list)
          end if
       end do
    end do
    do i = 1, nregions
       call reg_data%regions(i)%init (i, mult(i), 0, flv_alr2(i), &
          flv_uborn(i), reg_data%flv_born, flst_emitter(i), ftuples, &
          index)
    end do
    ! TODO: (bcn 2015-12-15) can this be put in singular_region_init?
    k = 1
    associate (regions => reg_data%regions)
       do i = 1, nregions
          if (i == 1) then
             regions(i)%real_index = 1
             flv_save = flv_alr2(1)
             cycle
          end if
          if (flv_alr2(i) == flv_save) then
             regions(i)%real_index = k
          else
             k = k + 1
             regions(i)%real_index = k
             flv_save = flv_alr2(i)
          end if
       end do
    end associate

  end subroutine region_data_init_singular_regions

  subroutine region_data_find_emitters (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: alr, j, n_em, em
    integer, dimension(N_MAX_ALR) :: em_count
    em_count = -1
    n_em = 0

    !!!Count the number of different emitters
    do alr = 1, reg_data%n_regions
       em = reg_data%regions(alr)%emitter
       if (.not. any (em_count == em)) then
          n_em = n_em + 1
          em_count(alr) = em
       end if
    end do

    if (n_em < 1) call msg_fatal ("region_data_find_emitters: No emitters found!")
    reg_data%n_emitters = n_em
    allocate (reg_data%emitters (reg_data%n_emitters))
    reg_data%emitters = -1

    j = 1
    do alr = 1, size (reg_data%regions)
       em = reg_data%regions(alr)%emitter
       if (.not. any (reg_data%emitters == em)) then
          reg_data%emitters(j) = em
          j = j + 1
       end if
    end do
  end subroutine region_data_find_emitters

  subroutine region_data_find_resonances (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: alr, j, k, n_res, n_contr
    integer :: res
    integer, dimension(10) :: res_count
    type(resonance_contributors_t), dimension(10) :: contributors_count
    type(resonance_contributors_t) :: contributors
    integer :: i_res, emitter
    logical :: share_emitter
    res_count = -1
    n_res = 0; n_contr = 0

    !!! Count the number of different resonances
    do alr = 1, reg_data%n_regions
       select type (fks_mapping => reg_data%fks_mapping)
       type is (fks_mapping_resonances_t)
          res = fks_mapping%res_map%alr_to_i_res (alr)
          if (.not. any (res_count == res)) then
             n_res = n_res + 1
             res_count(alr) = res
          end if
       end select
    end do

    if (n_res > 0) allocate (reg_data%resonances (n_res))

    j = 1
    select type (fks_mapping => reg_data%fks_mapping)
    type is (fks_mapping_resonances_t)
       do alr = 1, size (reg_data%regions)
          res = fks_mapping%res_map%alr_to_i_res (alr)
          if (.not. any (reg_data%resonances == res)) then
             reg_data%resonances(j) = res
             j = j + 1
          end if
       end do

       allocate (reg_data%alr_to_i_contributor (size (reg_data%regions)))
       do alr = 1, size (reg_data%regions)
          i_res = fks_mapping%res_map%alr_to_i_res (alr)
          emitter = reg_data%regions(alr)%emitter
          !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
          call reg_data%get_contributors (i_res, emitter, contributors%c, share_emitter)
          if (.not. share_emitter) cycle
          if (.not. any (contributors_count == contributors)) then 
             n_contr = n_contr + 1
             contributors_count(alr) = contributors
          end if
          if (allocated (contributors%c)) deallocate (contributors%c)
       end do
       allocate (reg_data%alr_contributors (n_contr))
       j = 1
       do alr = 1, size (reg_data%regions)
          i_res = fks_mapping%res_map%alr_to_i_res (alr)
          emitter = reg_data%regions(alr)%emitter
          !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
          call reg_data%get_contributors (i_res, emitter, contributors%c, share_emitter)
          if (.not. share_emitter) cycle
          if (.not. any (reg_data%alr_contributors == contributors)) then
             reg_data%alr_contributors(j) = contributors
             reg_data%alr_to_i_contributor (alr) = j
             j = j + 1
          else
             do k = 1, size (reg_data%alr_contributors)
                if (reg_data%alr_contributors(k) == contributors) exit
             end do
             reg_data%alr_to_i_contributor (alr) = k
          end if
          if (allocated (contributors%c)) deallocate (contributors%c)
       end do
    end select
    call reg_data%extend_ftuples (n_res)
    call reg_data%set_contributors ()
    
  end subroutine region_data_find_resonances

  subroutine region_data_set_contributors (reg_data)
     class(region_data_t), intent(inout) :: reg_data
     integer :: alr, i_res, i_reg, i_con
     integer :: i1, i2, i_em
     integer, dimension(:), allocatable :: contributors 
     logical :: share_emitter
     do alr = 1, size (reg_data%regions)
        associate (sregion => reg_data%regions(alr))
           allocate (sregion%i_reg_to_i_con (sregion%nregions))
           do i_reg = 1, sregion%nregions
              call sregion%ftuples(i_reg)%get (i1, i2)
              i_em = get_emitter_index (i1, i2, reg_data%n_legs_real)
              i_res = sregion%ftuples(i_reg)%i_res
              call reg_data%get_contributors (i_res, i_em, contributors, share_emitter)
              !!! Lookup contributor index
              do i_con = 1, size (reg_data%alr_contributors)
                 if (all (reg_data%alr_contributors(i_con)%c == contributors)) then
                    sregion%i_reg_to_i_con (i_reg) = i_con
                    exit
                 end if
              end do
              deallocate (contributors)
           end do
        end associate
     end do
  contains
     function get_emitter_index (i1, i2, n) result (i_em)
       integer :: i_em
       integer, intent(in) :: i1, i2, n
       if (i1 == n) then
          i_em = i2
       else
          i_em = i1
       end if 
     end function get_emitter_index
  end subroutine region_data_set_contributors

  subroutine region_data_extend_ftuples (reg_data, n_res)
    class(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: n_res
    integer :: alr, n_reg_save
    integer :: i_reg, i_res, i_em, k
    type(ftuple_t), dimension(:), allocatable :: ftuple_save
    integer :: n_new
    do alr = 1, size (reg_data%regions)
       associate (sregion => reg_data%regions(alr))
          n_reg_save = sregion%nregions
          allocate (ftuple_save (n_reg_save))
          ftuple_save = sregion%ftuples 
          n_new = count_n_new_ftuples (sregion, n_res)
          deallocate (sregion%ftuples)
          sregion%nregions = n_new
          allocate (sregion%ftuples (n_new))
          k = 1
          do i_res = 1, n_res
             do i_reg = 1, n_reg_save
                associate (ftuple_new => sregion%ftuples(k))
                   i_em = ftuple_save(i_reg)%ireg(1)
                   if (reg_data%emitter_is_in_resonance (i_res, i_em)) then
                      call ftuple_new%set (i_em, ftuple_save(i_reg)%ireg(2))
                      ftuple_new%i_res = i_res
                      ftuple_new%splitting_type = ftuple_save(i_reg)%splitting_type
                      k = k + 1
                   end if
                end associate
             end do
          end do    
       end associate
       deallocate (ftuple_save)
    end do
  contains
    function count_n_new_ftuples (sregion, n_res) result (n_new)
      integer :: n_new
      type(singular_region_t), intent(in) :: sregion
      integer, intent(in) :: n_res
      integer :: i_reg, i_res, i_em
      n_new = 0
      do i_reg = 1, sregion%nregions
         do i_res = 1, n_res 
            i_em = sregion%ftuples(i_reg)%ireg(1)
            if (reg_data%emitter_is_in_resonance (i_res, i_em)) &
               n_new = n_new + 1
         end do
      end do
    end function count_n_new_ftuples
  end subroutine region_data_extend_ftuples

  subroutine region_data_set_underlying_borns (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: i, alr
    integer, dimension(:), allocatable :: flst_real
    allocate (reg_data%underlying_borns (reg_data%n_flv_real))
    do i = 1, reg_data%n_flv_real
       if (allocated (flst_real))  deallocate (flst_real)
       allocate (flst_real (size (reg_data%flv_real(i)%flst)))
       flst_real = reg_data%flv_real(i)%flst
       do alr = 1, reg_data%n_regions
          if (all (reg_data%regions(alr)%flst_real%flst == flst_real)) then
             reg_data%underlying_borns(i) = reg_data%regions(alr)%uborn_index
             exit
          end if
       end do
    end do
  end subroutine region_data_set_underlying_borns

  subroutine region_data_compute_number_of_phase_spaces (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: i_em, i_res, i_phs
    integer :: emitter
    type(resonance_contributors_t) :: contributors
    integer, parameter :: n_max_phs = 10
    type(phs_identifier_t), dimension(n_max_phs) :: phs_id_tmp
    logical :: share_emitter, phs_exist
    if (allocated (reg_data%resonances)) then
       reg_data%n_phs = 0
       do i_em = 1, size (reg_data%emitters)
          emitter = reg_data%emitters(i_em)
          do i_res = 1, size (reg_data%resonances)
             if (reg_data%emitter_is_compatible_with_resonance (i_res, emitter)) then 
                !!! !!! !!! Workaround for ifort 16.0 standard-semantics bug
                call reg_data%get_contributors (i_res, emitter, contributors%c, share_emitter)
                if (.not. share_emitter) cycle
                call check_for_phs_identifier &
                   (phs_id_tmp, reg_data%n_in, emitter, contributors%c, phs_exist, i_phs)
                if (.not. phs_exist) then
                   reg_data%n_phs = reg_data%n_phs + 1
                   if (reg_data%n_phs > n_max_phs) call msg_fatal &
                      ("Buffer of phase space identifieres: Too much phase spaces!")
                   call phs_id_tmp(i_phs)%init (emitter, contributors%c)
                end if
             end if
             if (allocated (contributors%c)) deallocate (contributors%c)
          end do
       end do
    else
       reg_data%n_phs = count (remove_duplicates_from_list (reg_data%emitters) > reg_data%n_in)
       if (any (reg_data%emitters <= reg_data%n_in)) reg_data%n_phs = reg_data%n_phs + 1
    end if
  end subroutine region_data_compute_number_of_phase_spaces

  subroutine region_data_set_splitting_info (reg_data)
     class(region_data_t), intent(inout) :: reg_data
     integer :: alr
     do alr = 1, reg_data%n_regions
        call reg_data%regions(alr)%set_splitting_info ()
     end do
   end subroutine region_data_set_splitting_info

  subroutine region_data_write_to_file (reg_data, proc_id)
     class(region_data_t), intent(inout) :: reg_data
     type(string_t), intent(in) :: proc_id
     type(string_t) :: filename
     integer :: u

     filename = proc_id // "_fks_regions.log"
     u = free_unit ()
     open (u, file=char(filename), action = "write", status="replace")
     call reg_data%write (u)
     close (u)
  end subroutine region_data_write_to_file

  subroutine region_data_write (reg_data, unit)
     class(region_data_t), intent(in) :: reg_data
     integer, intent(in), optional :: unit
     integer :: j
     integer :: maxnregions
     type(string_t) :: flst_title, ftuple_title
     character(len=7), parameter :: flst_format = "(I3,A1)"
     character(len=16), parameter :: ireg_format = "(A1,I3,A1,I3,A2)"
     character(len=7), parameter :: ireg_space_format = "(7X,A1)"
     integer :: n_res, u
     u = given_output_unit (unit); if (u < 0) return
     maxnregions = 1
     do j = 1, reg_data%n_regions
        if (size (reg_data%regions(j)%ftuples) > maxnregions) &
             maxnregions = reg_data%regions(j)%nregions
     end do
     flst_title = '(A' // flst_title_format(reg_data%n_legs_real) // ')'
     ftuple_title = '(A' // ftuple_title_format() // ')'
     write (u,'(A,1X,I2)') 'Total number of regions: ', size(reg_data%regions)
     write (u, '(A3)', advance = 'no') 'alr'
     call write_vline (u)
     write (u, char (flst_title), advance = 'no') 'flst_real'
     call write_vline (u)
     write (u, '(A3)', advance = 'no') 'em'
     call write_vline (u)
     write (u, '(A3)', advance = 'no') 'mult'
     call write_vline (u)
     write (u, '(A4)', advance = 'no') 'nreg'
     call write_vline (u)
     if (allocated (reg_data%fks_mapping)) then
        select type (fks_mapping => reg_data%fks_mapping)
        type is (fks_mapping_resonances_t)
           write (u, '(A3)', advance = 'no') 'res'
           call write_vline (u)
        end select
     end if
     write (u, char (ftuple_title), advance = 'no') 'ftuples'
     call write_vline (u)
     flst_title = '(A' // flst_title_format(reg_data%n_legs_born) // ')'
     write (u, char (flst_title), advance = 'no') 'flst_born'
     call write_vline (u)
     write (u, '(A7)') 'i_uborn'
     do j = 1, reg_data%n_regions
        write (u, '(I3)', advance = 'no') j
        call reg_data%regions(j)%write (u, maxnregions)
     end do
     call write_separator (u)
     if (allocated (reg_data%fks_mapping)) then
        select type (fks_mapping => reg_data%fks_mapping)
        type is (fks_mapping_resonances_t)
           write (u, '(A)')
           write (u, '(A)') "The FKS regions are combined with resonance information: "
           n_res = size (fks_mapping%res_map%res_histories)
           write (u, '(A,1X,I1)') "Number of QCD resonance histories: ", n_res
           do j = 1, n_res
              write (u, '(A,1X,I1)') "i_res = ", j
              call fks_mapping%res_map%res_histories(j)%write (u)
              call write_separator (u)
           end do
        end select
     end if

   contains

     function flst_title_format (n) result (frmt)
       integer, intent(in) :: n
       type(string_t) :: frmt
       character(len=2) :: frmt_char
       write (frmt_char, '(I2)') 4 * n + 1
       frmt = var_str (frmt_char)
     end function flst_title_format

    function ftuple_title_format () result (frmt)
       type(string_t) :: frmt
       character(len=2) :: frmt_char
       write (frmt_char, '(I2)') 10 * maxnregions + 1
       frmt = var_str (frmt_char)
    end function ftuple_title_format

  end subroutine region_data_write

  subroutine write_vline (u)
    integer, intent(in) :: u
    character(len=10), parameter :: sep_format = "(1X,A2,1X)"
    write (u, sep_format, advance = 'no') '||'
  end subroutine write_vline

  subroutine region_data_assign (reg_data_out, reg_data_in)
    type(region_data_t), intent(out) :: reg_data_out
    type(region_data_t), intent(in) :: reg_data_in
    integer :: i
    if (allocated (reg_data_in%regions)) then
       allocate (reg_data_out%regions (size (reg_data_in%regions)))
       do i = 1, size (reg_data_in%regions)
          reg_data_out%regions(i) = reg_data_in%regions(i)
       end do
    else
       call msg_warning ("Copying region data without allocated singular regions!")
    end if
    if (allocated (reg_data_in%flv_born)) then
       allocate (reg_data_out%flv_born (size (reg_data_in%flv_born)))
       do i = 1, size (reg_data_in%flv_born)
          reg_data_out%flv_born(i) = reg_data_in%flv_born(i)
       end do
    else
       call msg_warning ("Copying region data without allocated born flavor structure!")
    end if
    if (allocated (reg_data_in%flv_real)) then
       allocate (reg_data_out%flv_real (size (reg_data_in%flv_real)))
       do i = 1, size (reg_data_in%flv_real)
          reg_data_out%flv_real(i) = reg_data_in%flv_real(i)
       end do
    else
       call msg_warning ("Copying region data without allocated real flavor structure!")
    end if
    if (allocated (reg_data_in%emitters)) then
       allocate (reg_data_out%emitters (size (reg_data_in%emitters)))
       do i = 1, size (reg_data_in%emitters)
          reg_data_out%emitters(i) = reg_data_in%emitters(i)
       end do
    else
       call msg_warning ("Copying region data without allocated emitters!")
    end if
    reg_data_out%n_regions = reg_data_in%n_regions
    reg_data_out%n_emitters = reg_data_in%n_emitters
    reg_data_out%n_flv_born = reg_data_in%n_flv_born
    reg_data_out%n_flv_real = reg_data_in%n_flv_real
    reg_data_out%n_in = reg_data_in%n_in
    reg_data_out%n_legs_born = reg_data_in%n_legs_born
    reg_data_out%n_legs_real = reg_data_in%n_legs_real
    if (allocated (reg_data_in%underlying_borns)) then
       allocate (reg_data_out%underlying_borns (size (reg_data_in%underlying_borns)))
       reg_data_out%underlying_borns = reg_data_in%underlying_borns
    else
       call msg_warning ("Copying region data without allocated underlying born flavor indices!")
    end if
    reg_data_out%flv_extra = reg_data_in%flv_extra
    if (allocated (reg_data_in%fks_mapping)) then
       select type (fks_mapping_in => reg_data_in%fks_mapping)
       type is (fks_mapping_default_t)
          allocate (fks_mapping_default_t :: reg_data_out%fks_mapping)
          select type (fks_mapping_out => reg_data_out%fks_mapping)
          type is (fks_mapping_default_t)
             fks_mapping_out = fks_mapping_in
          end select
       type is (fks_mapping_resonances_t)
          allocate (fks_mapping_resonances_t :: reg_data_out%fks_mapping)
          select type (fks_mapping_out => reg_data_out%fks_mapping)
          type is (fks_mapping_resonances_t)
             fks_mapping_out = fks_mapping_in
          end select
       end select
    else
       call msg_warning ("Copying region data without allocated FKS regions!")
    end if
    if (allocated (reg_data_in%resonances)) then
       allocate (reg_data_out%resonances (size (reg_data_in%resonances)))
       reg_data_out%resonances = reg_data_in%resonances
    end if
    reg_data_out%n_phs = reg_data_in%n_phs
    if (allocated (reg_data_in%alr_contributors)) then
       allocate (reg_data_out%alr_contributors (size (reg_data_in%alr_contributors)))
       reg_data_out%alr_contributors = reg_data_in%alr_contributors
    end if
    if (allocated (reg_data_in%alr_to_i_contributor)) then
       allocate (reg_data_out%alr_to_i_contributor &
          (size (reg_data_in%alr_to_i_contributor)))
       reg_data_out%alr_to_i_contributor = reg_data_in%alr_to_i_contributor
    end if
  end subroutine region_data_assign

  function region_to_index (list, i) result(index)
    type(ftuple_list_t), intent(inout), dimension(:), allocatable :: list
    integer, intent(in) :: i
    integer :: index, nlist, j
    integer, dimension(:), allocatable :: nreg
    nlist = size(list)
    allocate (nreg (nlist))
    index = 0
    do j = 1, nlist
       if (j == 1) then
          nreg(j) = list(j)%get_n_tuples ()
       else
          nreg(j) = nreg(j - 1) + list(j)%get_n_tuples ()
       end if
    end do
    do j = 1, nlist
       if (j == 1) then
          if (i <= nreg(j)) then
             index = j
             exit
          end if
       else
          if (i > nreg(j - 1) .and. i <= nreg(j)) then
             index = j
             exit
          end if
       end if
    end do
  end function region_to_index

  function create_alr (flv1, n_in, i_em, i_rad) result(flv2)
    type(flv_structure_t), intent(in) :: flv1
    integer, intent(in) :: n_in
    integer, intent(in) :: i_em, i_rad
    type(flv_structure_t) :: flv2
    integer :: n
    n = size (flv1%flst)
    allocate (flv2%flst (n))
    flv2%nlegs = n
    flv2%n_in = n_in
    if (i_em > n_in) then
       flv2%flst(1 : 2) = flv1%flst(1 : 2)
       flv2%flst(n - 1) = flv1%flst(i_em)
       flv2%flst(n) = flv1%flst(i_rad)
       call fill_remaining_flavors (n_in, .true.)
    else
       flv2%flst(1 : 2) = flv1%flst(1 : 2)
       flv2%flst(n) = flv1%flst(i_rad)
       call fill_remaining_flavors (n_in, .false.)
    end if
  contains
    subroutine fill_remaining_flavors (n_in, final_final)
      integer, intent(in) :: n_in
      logical, intent(in) :: final_final
      integer :: i, j
      logical :: check
      j = n_in + 1
      do i = n_in + 1, n
         if (final_final) then
            check = (i /= i_em .and. i /= i_rad)
         else
            check = (i /= i_rad)
         end if
         if (check) then
            flv2%flst(j) = flv1%flst(i)
            j = j + 1
         end if
      end do
    end subroutine fill_remaining_flavors
  end function create_alr

  subroutine fks_permute_born (flv_in, n_in, flv_out, perm_list)
    type(flv_structure_t), intent(in) :: flv_in
    integer, intent(in) :: n_in
    type(flv_structure_t), intent(inout) :: flv_out
    integer, intent(out), dimension(:,:), allocatable :: perm_list
    integer, dimension(:,:), allocatable :: perm_list_tmp
    integer :: n_perms, n_perms_max
    integer :: nlegs
    integer :: flv1, flv2, tmp
    integer :: i, j, j_min
    n_perms_max = 100
    !!! actually (n-1)!, but there seems to be no intrinsic function
    !!! of this type in fortran
    if (allocated (perm_list_tmp)) deallocate (perm_list_tmp)
    allocate (perm_list_tmp (n_perms_max,2))
    n_perms = 0
    j_min = n_in + 1
    nlegs = size (flv_in%flst)
    do i = n_in + 1, nlegs
       flv1 = flv_in%flst(i)
       do j = j_min, nlegs
          flv2 = flv_out%flst(j)
          if (flv1 == flv2 .and. i /= j) then
             n_perms = n_perms + 1
             tmp = flv_out%flst(i)
             flv_out%flst(i) = flv2
             flv_out%flst(j) = tmp
             perm_list_tmp (n_perms, 1) = j
             perm_list_tmp (n_perms, 2) = i
             j_min = j_min + 1
             exit
          end if
       end do
    end do
    allocate (perm_list (n_perms, 2))
    perm_list (1:n_perms, :) = perm_list_tmp (1:n_perms, :)
  end subroutine fks_permute_born

  subroutine fks_apply_perm (flv, emitter, perm_list)
    type(flv_structure_t), intent(inout) :: flv
    integer, intent(inout) :: emitter
    integer, intent(in), dimension(:,:), allocatable :: perm_list
    integer :: i
    integer :: i1, i2
    integer :: tmp
    do i = 1, size (perm_list (:,1))
       i1 = perm_list (i,1)
       i2 = perm_list (i,2)
       tmp = flv%flst (i1)
       flv%flst (i1) = flv%flst (i2)
       flv%flst (i2) = tmp
       if (i1 == emitter) emitter = i2
    end do
  end subroutine fks_apply_perm

  subroutine region_data_final (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    if (allocated (reg_data%regions)) deallocate (reg_data%regions)
    if (allocated (reg_data%flv_born)) deallocate (reg_data%flv_born)
    if (allocated (reg_data%flv_real)) deallocate (reg_data%flv_real)
    if (allocated (reg_data%emitters)) deallocate (reg_data%emitters)
    if (allocated (reg_data%underlying_borns)) deallocate (reg_data%underlying_borns)
    if (allocated (reg_data%fks_mapping)) deallocate (reg_data%fks_mapping)
    if (allocated (reg_data%resonances)) deallocate (reg_data%resonances)
    if (allocated (reg_data%alr_contributors)) deallocate (reg_data%alr_contributors)
    if (allocated (reg_data%alr_to_i_contributor)) deallocate (reg_data%alr_to_i_contributor)
  end subroutine region_data_final

  subroutine fks_mapping_default_set_parameter (map, n_in, dij_exp1, dij_exp2)
    class(fks_mapping_default_t), intent(inout) :: map
    integer, intent(in) :: n_in
    real(default), intent(in) :: dij_exp1, dij_exp2
    map%n_in = n_in
    map%exp_1 = dij_exp1
    map%exp_2 = dij_exp2
  end subroutine fks_mapping_default_set_parameter

  function fks_mapping_default_dij (map, p, i, j, i_con) result (d)
    real(default) :: d
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    integer, intent(in), optional :: i_con
    real(default) :: y, E1, E2
    d = 0
    !!! FSR Region
    if (i /= j) then
        if (i > map%n_in .and. j > map%n_in) then
           E1 = p(i)%p(0); E2 = p(j)%p(0)
           d = (two * p(i) * p(j) * E1 * E2 / (E1 + E2)**2)**map%exp_1
        else
           select case (map%n_in)
           case (1)
              call get_emitter_variables (1, i, j, p, E1, y)
              d = ( E1**2 * (one - y**2) )**map%exp_2
           case (2)
              if ((i == 0 .and. j > 2) .or. (j == 0 .and. i > 2)) then
                 call get_emitter_variables (0, i, j, p, E1, y)
                 d = ( E1**2 * (one - y**2) )**map%exp_2
              else if ((i == 1 .and. j > 2) .or. (j == 1 .and. i > 2)) then
                 call get_emitter_variables (1, i, j, p, E1, y)
                 d = ( 2 * E1**2 * (one - y) )**map%exp_2
              else if ((i == 2 .and. j > 2) .or. (j == 2 .and. i > 2)) then
                 call get_emitter_variables (2, i, j, p, E1, y)
                 d = (2 * E1**2 * (one + y) )**map%exp_2
              else
                 call msg_fatal ("FKS: Region with i, j <= 2 encountered")
              end if
           end select
        end if
    else
      call msg_fatal ("Invalid FKS region: Emitter equals FKS parton!")
    end if
  contains
    subroutine get_emitter_variables (i_check, i, j, p, E, y)
       integer, intent(in) :: i_check, i, j
       type(vector4_t), intent(in), dimension(:) :: p
       real(default), intent(out) :: E, y
       if (j == i_check) then
           E = energy (p(i))
           y = polar_angle_ct (p(i))
       else
           E = energy (p(j))
           y = polar_angle_ct(p(j))
       end if
    end subroutine get_emitter_variables

  end function fks_mapping_default_dij

  subroutine fks_mapping_default_compute_sumdij (map, sregion, p)
    class(fks_mapping_default_t), intent(inout) :: map
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: d, dij
    integer :: alr, i, j
    integer :: nlegreal

    associate (ftuples => sregion%ftuples)
      d = zero
      do alr = 1, sregion%nregions
        call ftuples(alr)%get (i, j)
        dij = map%dij (p, i, j)
        d = d + one / dij
      end do
    end associate
    map%sumdij = d
  end subroutine fks_mapping_default_compute_sumdij

  function fks_mapping_default_svalue (map, p, i, j, i_res) result (value)
    real(default) :: value
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    integer, intent(in), optional :: i_res
    value = one / (map%dij (p, i, j) * map%sumdij)
  end function fks_mapping_default_svalue

  function fks_mapping_default_dij_soft (map, p_born, p_soft, em, i_con) result (d)
    real(default) :: d
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: em
    integer, intent(in), optional :: i_con
    real(default) :: y
    if (em <= map%n_in) then
       y = polar_angle_ct (p_soft)
       select case (map%n_in)
       case (1)
          d = one - y**2
       case (2)
          select case (em)
          case (0)
             d = one - y**2
          case (1)
             d = two * (one - y)
          case (2)
             d = two * (one + y)
          case default
             d = zero
             call msg_fatal ("fks_mappings_default_dij_soft: n_in > 2")
          end select
       case default
          d = zero
          call msg_fatal ("fks_mappings_default_dij_soft: n_in > 2")
       end select
       d = d**map%exp_2
    else
       d = (two * p_born(em) * p_soft / p_born(em)%p(0))**map%exp_1
    end if
  end function fks_mapping_default_dij_soft

  subroutine fks_mapping_default_compute_sumdij_soft (map, sregion, p_born, p_soft)
    class(fks_mapping_default_t), intent(inout) :: map
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    real(default) :: d, dij
    integer :: alr, i, j
    integer :: nlegs
    d = zero
    nlegs = size (sregion%flst_real%flst)
    associate (ftuples => sregion%ftuples)
      do alr = 1, sregion%nregions
        call ftuples(alr)%get (i ,j)
        if (j == nlegs) then
          dij = map%dij_soft (p_born, p_soft, i)
          d = d + one / dij
        end if
      end do
    end associate
    map%sumdij_soft = d
  end subroutine fks_mapping_default_compute_sumdij_soft

  function fks_mapping_default_svalue_soft (map, p_born, p_soft, em, i_res) result (value)
    real(default) :: value
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: em
    integer, intent(in), optional :: i_res
    value = one / (map%sumdij_soft * map%dij_soft (p_born, p_soft, em))
  end function fks_mapping_default_svalue_soft

  subroutine fks_mapping_default_assign (fks_map_out, fks_map_in)
    type(fks_mapping_default_t), intent(out) :: fks_map_out
    type(fks_mapping_default_t), intent(in) :: fks_map_in
    fks_map_out%exp_1 = fks_map_in%exp_1
    fks_map_out%exp_2 = fks_map_in%exp_2
    fks_map_out%n_in = fks_map_in%n_in
  end subroutine fks_mapping_default_assign

  function fks_mapping_resonances_dij (map, p, i, j, i_con) result (d)
    real(default) :: d
    class(fks_mapping_resonances_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    integer, intent(in), optional :: i_con
    real(default) :: E1, E2
    integer :: ii_con
    if (present (i_con)) then
       ii_con = i_con
    else
       call msg_fatal ("Resonance mappings require resonance index as input!")
    end if
    d = 0
    if (map%pseudo_isr) then
       associate (p_res => map%res_map%p_res (ii_con))
          E1 = p_res**2; E2 = p(j) * p_res
          d = two * p_res * p(j) * E1 * E2 / (E1 + E2)**2
       end associate
    else
       if (i /= j) then
          if (i > 2 .and. j > 2) then
             associate (p_res => map%res_map%p_res (ii_con))
                E1 = p(i) * p_res
                E2 = p(j) * p_res
                d = two * p(i) * p(j) * E1 * E2 / (E1 + E2)**2 
             end associate
          else
             call msg_fatal ("Resonance mappings are not implemented for ISR")
          end if
       end if
    end if
  end function fks_mapping_resonances_dij

  subroutine fks_mapping_resonances_compute_sumdij (map, sregion, p)
    class(fks_mapping_resonances_t), intent(inout) :: map
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: d, dij, pfr
    integer :: i_res, i_reg, i, j, i_con
    integer :: nlegreal

    nlegreal = size (p)
    d = zero
    do i_reg = 1, sregion%nregions
       associate (ftuple => sregion%ftuples(i_reg))
          call ftuple%get (i, j)
          i_res = ftuple%i_res
       end associate
       pfr = map%res_map%get_resonance_value (i_res, p, nlegreal) 
       i_con = sregion%i_reg_to_i_con (i_reg) 
       map%pseudo_isr = sregion%pseudo_isr
       d = d + pfr / map%dij (p, i, j, i_con)
    end do
    map%sumdij = d
  end subroutine fks_mapping_resonances_compute_sumdij

  function fks_mapping_resonances_svalue (map, p, i, j, i_res) result (value)
    real(default) :: value
    class(fks_mapping_resonances_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    integer, intent(in), optional :: i_res
    real(default) :: pfr
    integer :: i_gluon
    i_gluon = size (p)
    pfr = map%res_map%get_resonance_value (i_res, p, i_gluon)
    value = pfr / (map%dij (p, i, j, map%i_con) * map%sumdij)
  end function fks_mapping_resonances_svalue

  function fks_mapping_resonances_dij_soft (map, p_born, p_soft, em, i_con) result (d)
     real(default) :: d
     class(fks_mapping_resonances_t), intent(in) :: map
     type(vector4_t), intent(in), dimension(:) :: p_born
     type(vector4_t), intent(in) :: p_soft
     integer, intent(in) :: em
     integer, intent(in), optional :: i_con
     real(default) :: E1, E2
     integer :: ii_con
     integer :: i
     type(vector4_t) :: pb
     if (present (i_con)) then
        ii_con = i_con
     else
        call msg_fatal ("fks_mapping_resonances requires resonance index")
     end if
     associate (p_res => map%res_map%p_res(ii_con))
        if (map%pseudo_isr) then
           pb = p_res
        else
           pb = p_born(em)
        end if
        E1 = pb * p_res
        E2 = p_soft * p_res
        d = two * pb * p_soft * E1 * E2 / E1**2
     end associate
  end function fks_mapping_resonances_dij_soft

  subroutine fks_mapping_resonances_compute_sumdij_soft (map, sregion, p_born, p_soft)
    class(fks_mapping_resonances_t), intent(inout) :: map
    type(singular_region_t), intent(in) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    real(default) :: d
    real(default) :: pfr
    integer :: i_res, i_alr, i, j, i_reg, i_con
    integer :: nlegs

    d = zero
    nlegs = size (sregion%flst_real%flst)
    do i_reg = 1, sregion%nregions
       associate (ftuple => sregion%ftuples(i_reg))
          call ftuple%get(i, j)
          i_res = ftuple%i_res
       end associate
       pfr = map%res_map%get_resonance_value (i_res, p_born) 
       i_con = sregion%i_reg_to_i_con (i_reg)
       map%pseudo_isr = sregion%pseudo_isr
       if (j == nlegs) d = d + pfr / map%dij_soft (p_born, p_soft, i, i_con)
    end do
    map%sumdij_soft = d
  end subroutine fks_mapping_resonances_compute_sumdij_soft

  function fks_mapping_resonances_svalue_soft (map, p_born, p_soft, em, i_res) result (value)
    real(default) :: value
    class(fks_mapping_resonances_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: em
    integer, intent(in), optional :: i_res
    real(default) :: pfr
    pfr = map%res_map%get_resonance_value (i_res, p_born)
    value = pfr / (map%sumdij_soft * map%dij_soft (p_born, p_soft, em, map%i_con))
  end function fks_mapping_resonances_svalue_soft

  subroutine fks_mapping_resonances_set_resonance_momentum (map, p)
    class(fks_mapping_resonances_t), intent(inout) :: map
    type(vector4_t), intent(in) :: p
    map%res_map%p_res = p
  end subroutine fks_mapping_resonances_set_resonance_momentum

  subroutine fks_mapping_resonances_set_resonance_momenta (map, p)
    class(fks_mapping_resonances_t), intent(inout) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    map%res_map%p_res = p
  end subroutine fks_mapping_resonances_set_resonance_momenta

  subroutine fks_mapping_resonances_assign (fks_map_out, fks_map_in)
    type(fks_mapping_resonances_t), intent(out) :: fks_map_out
    type(fks_mapping_resonances_t), intent(in) :: fks_map_in
    fks_map_out%exp_1 = fks_map_in%exp_1
    fks_map_out%exp_2 = fks_map_in%exp_2
    fks_map_out%res_map = fks_map_in%res_map
  end subroutine fks_mapping_resonances_assign

  function create_resonance_histories_for_threshold () result (res_history)
    type(resonance_history_t) :: res_history
    res_history%n_resonances = 2  
    allocate (res_history%resonances (2))
    allocate (res_history%resonances(1)%contributors%c(2))
    allocate (res_history%resonances(2)%contributors%c(2))
    res_history%resonances(1)%contributors%c = [THR_POS_WP, THR_POS_B]
    res_history%resonances(2)%contributors%c = [THR_POS_WM, THR_POS_BBAR]
  end function create_resonance_histories_for_threshold
    
  subroutine setup_region_data_for_test (n_in, flv_born, flv_real, reg_data)
    integer, intent(in) :: n_in
    integer, intent(in), dimension(:,:) :: flv_born, flv_real
    type(region_data_t), intent(out) :: reg_data
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: test_model => null ()
    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
       (var_str ("SM_rad"), var_str ("SM_rad.mdl"), os_data, test_model)
    call reg_data%init (n_in, test_model, flv_born, flv_real)
  end subroutine setup_region_data_for_test


end module fks_regions
