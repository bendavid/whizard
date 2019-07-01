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

module fks_regions

  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use constants
  use diagnostics
  use flavors
  use process_constants
  use lorentz
  use pdg_arrays
  use model_data
  use physics_defs

  implicit none
  private

  public :: ftuple_t
  public :: flv_structure_t
  public :: singular_region_t
  public :: fks_mapping_default_t
  public :: region_data_t

  integer, parameter :: UNDEFINED_SPLITTING = 0
  integer, parameter :: Q_TO_QG = 1
  integer, parameter :: G_TO_GG = 2
  integer, parameter :: G_TO_QQ = 3


  type :: ftuple_t
    integer, dimension(2) :: ireg
    integer :: splitting_type
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
    integer :: nlegs
    logical, dimension(:), allocatable :: massive
    logical, dimension(:), allocatable :: colored
  contains
    procedure :: valid_pair => flv_structure_valid_pair
    procedure :: remove_particle => flv_structure_remove_particle
    procedure :: insert_particle => flv_structure_insert_particle
    procedure :: get_nlegs => flv_structure_get_nlegs
    procedure :: count_particle => flv_structure_count_particle
    procedure :: init => flv_structure_init
    procedure :: write => flv_structure_write
    procedure :: create_uborn => flv_structure_create_uborn
    procedure :: evaluate => flv_structure_evaluate
  end type flv_structure_t

  type :: singular_region_t
    integer :: alr
    type(flv_structure_t) :: flst_real
    type(flv_structure_t) :: flst_uborn
    integer :: mult
    integer :: emitter
    integer :: nregions
    integer :: real_index
    type(ftuple_t), dimension(:), allocatable :: flst_allreg
    integer :: uborn_index
    logical :: double_fsr
    logical :: soft_divergence
    logical :: coll_divergence
  contains
    procedure :: set_splitting_info => singular_region_set_splitting_info
    procedure :: double_fsr_factor => singular_region_double_fsr_factor
    procedure :: has_soft_divergence => singular_region_has_soft_divergence
    procedure :: has_collinear_divergence => &
              singular_region_has_collinear_divergence
  end type singular_region_t

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

  type :: region_data_t
    type(singular_region_t), dimension(:), allocatable :: regions
    type(flv_structure_t), dimension(:), allocatable :: flv_born
    type(flv_structure_t), dimension(:), allocatable :: flv_real
    integer, dimension(:), allocatable :: emitters
    integer :: n_regions
    integer :: n_emitters
    integer :: n_flv_born
    integer :: n_flv_real
    integer :: n_in
    integer :: nlegs_born
    integer :: nlegs_real
    integer, dimension(:), allocatable :: underlying_borns
    type(flavor_t) :: flv_extra
    class(fks_mapping_t), allocatable :: fks_mapping
  contains
    procedure :: init => region_data_init
    procedure :: evaluate_flavors => region_data_evaluate_flavors
    procedure :: get_emitter_list => region_data_get_emitter_list
    procedure :: get_emitter => region_data_get_emitter
    procedure :: get_svalue => region_data_get_svalue
    procedure :: get_svalue_soft => region_data_get_svalue_soft
    procedure :: find_regions => region_data_find_regions
    procedure :: init_regions => region_data_init_singular_regions
    procedure :: find_emitters => region_data_find_emitters
    procedure :: set_underlying_borns => region_data_set_underlying_borns
    procedure :: set_splitting_info => region_data_set_splitting_info
    procedure :: write_to_file => region_data_write_to_file
    procedure :: write => region_data_write
  end type region_data_t


  interface operator(==)
    module procedure flv_structure_equivalent
  end interface

  abstract interface
    function fks_mapping_dij (map, p, i, j) result (d)
      import
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p
      integer, intent(in) :: i, j
      real(default) :: d
    end function fks_mapping_dij
  end interface

  abstract interface
    function fks_mapping_compute_sumdij (map, sregion, p) result (d)
      import
      class(fks_mapping_t), intent(in) :: map
      type(singular_region_t), intent(inout) :: sregion
      type(vector4_t), intent(in), dimension(:) :: p
      real(default) :: d
    end function fks_mapping_compute_sumdij
  end interface

  abstract interface
    function fks_mapping_svalue (map, p, i, j) result (value)
      import
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p
      integer, intent(in) :: i, j
      real(default) :: value
    end function fks_mapping_svalue
  end interface

  abstract interface
    function fks_mapping_dij_soft (map, p_born, p_soft, em) result (d)
      import
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
      integer, intent(in) :: em
      real(default) :: d
    end function fks_mapping_dij_soft
  end interface

  abstract interface
    function fks_mapping_compute_sumdij_soft (map, sregion, p_born, p_soft) result (d)
      import
      class(fks_mapping_t), intent(in) :: map
      type(singular_region_t), intent(inout) :: sregion
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
      real(default) :: d
    end function
  end interface
  abstract interface
    function fks_mapping_svalue_soft (map, p_born, p_soft, em) result (value)
      import
      class(fks_mapping_t), intent(in) :: map
      type(vector4_t), intent(in), dimension(:) :: p_born
      type(vector4_t), intent(in) :: p_soft
      integer, intent(in) :: em
      real(default) :: value
    end function fks_mapping_svalue_soft
  end interface


contains

  subroutine ftuple_write (ftuple, unit)
    class(ftuple_t), intent(in) :: ftuple
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(A1,I1,A1,I1,A1)") &
         '(', ftuple%ireg(1), ',', ftuple%ireg(2), ')'
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
    class(ftuple_list_t), intent(inout), target :: list
    integer :: n_tuples
    type(ftuple_list_t), pointer :: current
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
       else
          n_tuples = 0
       end if
     end select
  end function ftuple_list_get_n_tuples

  function ftuple_list_get_entry(list, index) result(entry)
   class(ftuple_list_t), intent(inout), target :: list
   integer, intent(in) :: index
   type(ftuple_list_t), pointer :: entry
   type(ftuple_list_t), pointer :: current
   integer :: i
   select type (list)
   type is (ftuple_list_t)
   current => list
   if (index <= list%get_n_tuples ()) then
   if (index == 1) then
     entry => current
   else
     do i=1,index-1
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
    class(ftuple_list_t), intent(inout) :: list
    integer, intent(in) :: index
    type(ftuple_t) :: ftuple
    type(ftuple_list_t) :: entry
    entry = list%get_entry (index)
    ftuple = entry%ftuple
  end function ftuple_list_get_ftuple

  subroutine ftuple_list_set_equiv (list, i1, i2)
    class(ftuple_list_t), intent(inout) :: list
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
    class(ftuple_list_t), intent(inout) :: list
    integer, intent(in) :: i1, i2
    logical :: eq
    type(ftuple_list_t), pointer :: current
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
          eq = .false.
          exit
        end if
      end do
    end select
  end function ftuple_list_check_equiv

  function flv_structure_valid_pair &
       (flv_real,i,j, flv_born, model) result (valid)
    class(flv_structure_t), intent(in) :: flv_real
    integer, intent(in) :: i,j
    type(flv_structure_t), intent(in) :: flv_born
    type(model_data_t), intent(in) :: model
    logical :: valid
    integer :: k, n_orig
    type(flv_structure_t) :: flv_test
    integer, dimension(:), allocatable :: flv_orig, flv_orig2
    valid = .false.
    call model%match_vertex &
         (flv_real%flst(i), flv_real%flst(j), flv_orig)
    n_orig = size (flv_orig)
    if (n_orig == 0) then
      return
    else
      allocate (flv_orig2 (2*n_orig))
      flv_orig2 (1:n_orig) = flv_orig
      flv_orig2 (n_orig+1:2*n_orig) = -flv_orig
      do k = 1, 2*n_orig
        flv_test = flv_real%insert_particle (i,j,flv_orig2(k))
        valid = flv_born == flv_test
        if (valid) return
      end do
    end if
  end function flv_structure_valid_pair

  function flv_structure_equivalent (flv1, flv2) result(equiv)
    type(flv_structure_t), intent(in) :: flv1, flv2
    logical :: equiv
    integer :: i, j, n
    logical, dimension(:), allocatable :: present, checked
    n = size (flv1%flst)
    equiv = .true.
    if (n /= size (flv2%flst)) then
       call msg_fatal &
            ('flv_structure_equivalent: flavor arrays do not have equal lengths')
    else
       allocate (present(n))
       allocate (checked(n))
       do i=1,n
          present(i) = .false.
          checked(i) = .false.
       end do
       do i=1,n
          do j=1,n
             if (flv1%flst(i) == flv2%flst(j) .and. .not. checked(j)) then
                present(i) = .true.
                checked(j) = .true.
                exit
             end if
          end do
       end do
       do i=1,n
          if(.not.present(i)) equiv = .false.
       end do
    end if
  end function flv_structure_equivalent

  function flv_structure_remove_particle (flv1, index) result(flv2)
    class(flv_structure_t), intent(in) :: flv1
    integer, intent(in) :: index
    type(flv_structure_t) :: flv2
    integer :: n1, n2
    n1 = size (flv1%flst)
    n2 = n1-1
    if (allocated (flv2%flst)) then
      deallocate (flv2%flst)
    end if
    allocate (flv2%flst (n2))
    if (index == 1) then
      flv2%flst(1:n2) = flv1%flst(2:n1)
    else if (index == n1) then
      flv2%flst(1:n2) = flv1%flst(1:n2)
    else
      flv2%flst(1:index-1) = flv1%flst(1:index-1)
      flv2%flst(index:n2) = flv1%flst(index+1:n1)
    end if
  end function flv_structure_remove_particle

  function flv_structure_insert_particle (flv1, i1, i2, particle) result (flv2)
    class(flv_structure_t), intent(in) :: flv1
    integer, intent(in) :: i1, i2, particle
    type(flv_structure_t) :: flv2
    type(flv_structure_t) :: flv_tmp
    integer :: n1, n2
    n1 = size (flv1%flst)
    n2 = n1-1
    allocate (flv2%flst(n2))
    if (i1 < i2) then
      flv_tmp = flv1%remove_particle (i1)
      flv_tmp = flv_tmp%remove_particle (i2-1)
    else if(i2 < i1) then
      flv_tmp = flv1%remove_particle(i2)
      flv_tmp = flv_tmp%remove_particle(i1-1)
    else
      call msg_fatal ("Trying to set ftuple with i1 = i2!")
    end if
    if (i1 == 1) then
      flv2%flst(1) = particle
      flv2%flst(2:n2) = flv_tmp%flst(1:n2-1)
    else if (i1 == n1 .or. i1 == n2) then
      flv2%flst(1:n2-1) = flv_tmp%flst(1:n2-1)
      flv2%flst(n2) = particle
    else
      flv2%flst(1:i1-1) = flv_tmp%flst(1:i1-1)
      flv2%flst(i1) = particle
      flv2%flst(i1+1:n2) = flv_tmp%flst(i1:n2-1)
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

  subroutine flv_structure_init (flv, aval)
    class(flv_structure_t), intent(inout) :: flv
    integer, intent(in), dimension(:) :: aval
    integer :: n
    n = size (aval)
    allocate (flv%flst (n))
    flv%flst(1:n) = aval(1:n)
    flv%nlegs = n
  end subroutine flv_structure_init

  subroutine flv_structure_write (flv, unit)
    class(flv_structure_t), intent(in) :: flv
    integer, intent(in), optional :: unit
    integer :: i, u
    u = given_output_unit (unit); if (u < 0) return
    write (u, '(A1)',advance = 'no') '['
    do i = 1, size(flv%flst)-1
      write (u, '(I3,A1)', advance = 'no') flv%flst(i), ','
    end do
    write (u, '(I3,A1)') flv%flst(i), ']'
  end subroutine flv_structure_write

  function flv_structure_create_uborn (flst_alr, emitter) result(flst_uborn)
    class(flv_structure_t), intent(in) :: flst_alr
    integer, intent(in) :: emitter
    type(flv_structure_t) :: flst_uborn
    integer n_alr, n_uborn
    n_alr = size(flst_alr%flst)
    n_uborn = n_alr-1
    allocate (flst_uborn%flst (n_uborn))
    if (emitter > 2) then
      if (flst_alr%flst(n_alr) == 21) then
         !!! Emitted particle is a gluon => just remove it
         flst_uborn = flst_alr%remove_particle(n_alr)
         !!! Emission type is a gluon splitting into two quars
      else if (is_quark (abs(flst_alr%flst(n_alr))) .and. &
               is_quark (abs(flst_alr%flst(n_alr-1))) .and. &
               flst_alr%flst(n_alr) + flst_alr%flst(n_alr-1) == 0) then
         flst_uborn = flst_alr%insert_particle(n_alr-1,n_alr,21)
      else
         call msg_fatal ("Create underlying Born: Unsupported splitting type.")    
      end if
     else
        if (flst_alr%flst(n_alr) == 21) then
           flst_uborn = flst_alr%remove_particle(n_alr)
        else if (is_quark (abs(flst_alr%flst(n_alr))) .and. &
                  is_gluon (abs(flst_alr%flst(emitter)))) then
           flst_uborn = &
                flst_alr%insert_particle (emitter,n_alr,-flst_alr%flst(n_alr))
        else if (is_quark (abs(flst_alr%flst(n_alr))) .and. &
                  is_quark (abs(flst_alr%flst(emitter))) .and. &
                  flst_alr%flst(n_alr) == flst_alr%flst(emitter)) then
           flst_uborn = flst_alr%insert_particle(emitter,n_alr,21)
        end if
     end if
  end function flv_structure_create_uborn

  subroutine flv_structure_evaluate (flv, n, model)
    class(flv_structure_t), intent(inout) :: flv
    integer, intent(in) :: n
    type(model_data_t), intent(in) :: model
    integer :: i
    type(flavor_t) :: flavor
    allocate (flv%massive (n), flv%colored(n))
    do i = 1, n
       call flavor%init (flv%flst(i), model)
       flv%massive(i) = flavor%get_mass () > 0
       flv%colored(i) = is_quark (abs(flv%flst(i))) .or. &
                        is_gluon (flv%flst(i))
    end do
  end subroutine flv_structure_evaluate

  subroutine singular_region_set_splitting_info (region)
    class(singular_region_t), intent(inout) :: region
    integer :: i1, i2
    integer :: reg
    region%double_fsr = .false.
    associate (ftuple => region%flst_allreg)
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
      val = 2*E_em / (E_em + E_rad)
    else
      val = 1._default
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

  subroutine region_data_init (reg_data, n_in, model, flavor_born, &
    flavor_real, mapping_type)
    class(region_data_t), intent(inout) :: reg_data
    integer, intent(in) :: n_in
    type(model_data_t), intent(in) :: model
    integer, intent(inout), dimension(:,:), allocatable :: &
         flavor_born, flavor_real
    integer, intent(in) :: mapping_type
    integer, dimension(:), allocatable :: current_flavor
    type(ftuple_list_t), dimension(:), allocatable :: ftuples
    integer, dimension(:), allocatable :: emitter
    type(flv_structure_t), dimension(:), allocatable :: flst_alr
    integer :: i
    reg_data%n_in = n_in
    reg_data%n_flv_born = size(flavor_born(1,:))
    reg_data%n_flv_real = size(flavor_real(1,:))
    reg_data%nlegs_born = size(flavor_born(:,1))
    reg_data%nlegs_real = reg_data%nlegs_born + 1
    allocate (reg_data%flv_born (reg_data%n_flv_born))
    allocate (reg_data%flv_real (reg_data%n_flv_real))
    allocate (current_flavor (reg_data%nlegs_born))
    do i = 1, reg_data%n_flv_born
       current_flavor = flavor_born(:,i)
       call reg_data%flv_born(i)%init (current_flavor)
    end do
    deallocate (current_flavor)
    allocate (current_flavor (reg_data%nlegs_real))
    do i = 1, reg_data%n_flv_real
       current_flavor = flavor_real(:,i)
       call reg_data%flv_real(i)%init (current_flavor)
    end do

    select case (mapping_type)
    case (1)
       allocate (fks_mapping_default_t :: reg_data%fks_mapping)
    case default
       call msg_fatal ("Init region_data: FKS mapping not implemented!")
    end select

    call reg_data%flv_extra%init &
         (reg_data%flv_real(1)%flst(reg_data%nlegs_real), &
         model)
    call reg_data%find_regions (model, ftuples, emitter, flst_alr)
    call reg_data%init_regions (ftuples, emitter, flst_alr)
    call reg_data%evaluate_flavors (model)
    call reg_data%set_splitting_info ()
    call reg_data%find_emitters ()
    call reg_data%set_underlying_borns ()
  end subroutine region_data_init

  subroutine region_data_evaluate_flavors (reg_data, model)
    class(region_data_t), intent(inout) :: reg_data
    type(model_data_t), intent(in) :: model
    integer :: i
    do i = 1, reg_data%n_regions
       associate (region => reg_data%regions(i))
          call region%flst_uborn%evaluate (reg_data%nlegs_born, model)
          call region%flst_real%evaluate (reg_data%nlegs_real, model)
       end associate
    end do
    do i = 1, reg_data%n_flv_born
       call reg_data%flv_born(i)%evaluate (reg_data%nlegs_born, model)
    end do
    do i = 1, reg_data%n_flv_real
       call reg_data%flv_real(i)%evaluate (reg_data%nlegs_real, model)
    end do
  end subroutine region_data_evaluate_flavors

  pure function region_data_get_emitter_list (reg_data) result(emitters)
    class(region_data_t), intent(in) :: reg_data
    integer, dimension(:), allocatable :: emitters
    integer :: i
    allocate (emitters (reg_data%n_regions))
    do i = 1, reg_data%n_regions
       emitters(i) = reg_data%regions(i)%emitter
    end do
  end function region_data_get_emitter_list

  pure function region_data_get_emitter (reg_data, alr) result (emitter)
    class(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: alr
    integer :: emitter
    emitter = reg_data%regions(alr)%emitter
  end function region_data_get_emitter

  function region_data_get_svalue (reg_data, p, alr, emitter) result (sval)
    class(region_data_t), intent(inout) :: reg_data
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: alr, emitter
    real(default) :: sval
    associate (map => reg_data%fks_mapping)
      map%sumdij = map%compute_sumdij (reg_data%regions(alr), p)
      sval = map%svalue (p, emitter, reg_data%nlegs_real)
    end associate
  end function region_data_get_svalue

  function region_data_get_svalue_soft &
       (reg_data, p, p_soft, alr, emitter) result (sval)
    class(region_data_t), intent(inout) :: reg_data
    type(vector4_t), intent(in), dimension(:) :: p
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: alr, emitter
    real(default) :: sval
    associate (map => reg_data%fks_mapping)
      map%sumdij_soft = &
         map%compute_sumdij_soft (reg_data%regions(alr), p, p_soft)
      sval = map%svalue_soft (p, p_soft, emitter)
    end associate
  end function region_data_get_svalue_soft

  subroutine region_data_find_regions &
       (reg_data, model, ftuples, emitter, flst_alr)
    class(region_data_t), intent(in) :: reg_data
    type(model_data_t), intent(in) :: model
    type(ftuple_list_t), intent(out), dimension(:), allocatable :: ftuples
    integer, intent(out), dimension(:), allocatable :: emitter
    type(flv_structure_t), intent(out), dimension(:), allocatable :: flst_alr
    type(ftuple_t) :: current_ftuple
    integer, dimension(:), allocatable :: emitter_tmp
    type(flv_structure_t), dimension(:), allocatable :: flst_alr_tmp
    integer :: nreg, nborn, nreal
    integer :: nlegreal
    integer, parameter :: maxnregions = 200
    integer :: i, j, k, l, m, n
    logical :: valid1, valid2

    associate (flv_born => reg_data%flv_born)
      associate (flv_real => reg_data%flv_real)
        nborn = size (flv_born)
        nreal = size (flv_real)
        nlegreal = size (flv_real(1)%flst)
        allocate (ftuples (nreal))
        allocate (emitter_tmp (maxnregions))
        allocate (flst_alr_tmp (maxnregions))
        n = 0

        ITERATE_REAL_FLAVOR: do l = 1, nreal
           do i = reg_data%n_in + 1, nlegreal
              do j = i+1, nlegreal
                 do k = 1, nborn
                    if (flv_real(l)%valid_pair(i, j, flv_born(k), model) &
                         .or. flv_real(l)%valid_pair(j, i, flv_born(k), model)) then
                       n = n+1
                       if(flv_real(l)%valid_pair(i, j, flv_born(k), model)) then
                          flst_alr_tmp(n) = create_alr (flv_real(l), reg_data%n_in, i, j)
                       else
                          flst_alr_tmp(n) = create_alr (flv_real(l), reg_data%n_in, j, i)
                       end if
                       call current_ftuple%set (i, j)
                       call current_ftuple%determine_splitting_type_fsr (flv_real(l), i, j)
                       call ftuples(l)%append (current_ftuple)
                       emitter_tmp(n) = nlegreal - 1
                       exit
                    end if
                 end do
              end do
           end do
           do i = reg_data%n_in + 1, nlegreal
              do k = 1, nborn
                 valid1 = flv_real(l)%valid_pair(1, i, flv_born(k), model)
                 if (reg_data%n_in > 1) then
                    valid2 = flv_real(l)%valid_pair(2, i, flv_born(k), model)
                 else
                    valid2 = .false.
                 end if
                 if (valid1 .and. valid2) then
                    m = 0
                 else if (valid1 .and. .not. valid2) then
                    m = 1
                 else if (.not. valid1 .and. valid2) then
                    m = 2
                 end if
                 if (valid1 .or. valid2) then
                    n = n + 1
                    call current_ftuple%set(m, i)
                    call current_ftuple%determine_splitting_type_isr (flv_real(l), m, i)
                    call ftuples(l)%append (current_ftuple)
                    emitter_tmp(n) = m
                    flst_alr_tmp(n) = create_alr (flv_real(l), reg_data%n_in, m, i)
                    exit
                 end if
              end do
           end do
        end do ITERATE_REAL_FLAVOR

        nreg = n

      end associate
    end associate

    allocate (flst_alr (nreg))
    allocate (emitter (nreg))
    flst_alr(1:nreg) = flst_alr_tmp(1:nreg)
    emitter(1:nreg) = emitter_tmp(1:nreg)
  end subroutine region_data_find_regions

  subroutine region_data_init_singular_regions &
     (reg_data, ftuples, emitter, flst_alr)
     class(region_data_t), intent(inout) :: reg_data
     type(ftuple_list_t), intent(inout), dimension(:), allocatable :: ftuples
     integer :: n_valid_ftuples
     type(ftuple_list_t) :: current_region
     integer, intent(in), dimension(:), allocatable :: emitter
     type(flv_structure_t), intent(in), dimension(:), allocatable :: flst_alr
     type(flv_structure_t), dimension(:), allocatable :: flst_uborn, flst_alr2
     integer, dimension(:), allocatable :: mult
     integer, dimension(:), allocatable :: flst_emitter
     integer :: nregions, maxregions
     integer, dimension(:,:), allocatable :: perm_list
     integer, dimension(:), allocatable :: index
     integer :: i, j, k, l
     integer :: nlegs
     logical :: equiv
     integer :: nreg, i1, i2
     integer :: i_first, j_first
     integer, dimension(:), allocatable :: &
        region_to_ftuple, ftuple_limits, k_index
     type(flv_structure_t) :: flst_save

     maxregions = size (emitter)
     nlegs = size (flst_alr(1)%flst)

     allocate (flst_uborn (maxregions))
     allocate (flst_alr2 (maxregions))
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
           j = j+ 1
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
        SCAN_FTUPLES: do i = i_first, i_first + ftuple_limits (l) -1
           equiv = .false.
           if (i==i_first) then
              if (allocated (flst_alr2(j)%flst)) &
                   deallocate (flst_alr2(j)%flst)
              allocate (flst_alr2(j)%flst (size (flst_alr(i)%flst)))
              flst_alr2(j)%flst = flst_alr(i)%flst
              mult(j) = mult(j) + 1
              flst_uborn(j) = flst_alr(i)%create_uborn (emitter(i))
              flst_emitter(j) = emitter(i)
              index (j) = region_to_index(ftuples, i)
              k_index (j) = region_to_ftuple(i)
              j = j+1
           else
              !!! Check for equivalent flavor structures
              do k = j_first, j - 1
                 if (emitter(i) == emitter(k) .and. emitter(i) > reg_data%n_in) then
                    if (flst_alr(i) == flst_alr2(k) .and. &
                         flst_alr(i)%flst(nlegs-1) == flst_alr2(k)%flst(nlegs-1) &
                         .and. flst_alr(i)%flst(nlegs) == flst_alr2(k)%flst(nlegs)) then
                    mult(k) = mult(k) + 1
                    equiv = .true.
                    call ftuples (region_to_index(ftuples, i))%set_equiv &
                         (k_index(k), region_to_ftuple(i))
                    exit
                 end if
              else if (emitter(i) == emitter(k) .and. emitter(i) <= reg_data%n_in) then
                 if (flst_alr(i) == flst_alr2(k)) then
                    mult(k) = mult(k) + 1
                    equiv = .true.
                    call ftuples (region_to_index(ftuples,i))%set_equiv &
                         (k_index(k), region_to_ftuple(i))
                    exit
                 end if
              end if
           end do
           if (.not. equiv) then
              if (allocated (flst_alr2(j)%flst)) &
                   deallocate (flst_alr2(j)%flst)
              allocate (flst_alr2(j)%flst (size (flst_alr(i)%flst)))
              flst_alr2(j)%flst = flst_alr(i)%flst
              mult(j) = mult(j) + 1
              flst_uborn(j) = flst_alr(i)%create_uborn (emitter(i))
              flst_emitter(j) = emitter(i)
              index (j) = region_to_index (ftuples, i)
              k_index (j) = region_to_ftuple(i)
              j = j+1
           end if
        end if
     end do SCAN_FTUPLES
     i_first = i_first + ftuple_limits(l)
     j_first = j_first + j - 1
  end do SCAN_REGIONS
     nregions = j - 1
     allocate (reg_data%regions (nregions))
     do j = 1, nregions
        do i = 1, reg_data%n_flv_born
           if (reg_data%flv_born (i) == flst_uborn (j)) then
              if (allocated (perm_list)) deallocate (perm_list)
              call fks_permute_born &
                   (reg_data%flv_born (i), reg_data%n_in, flst_uborn (j), perm_list)
              call fks_apply_perm (flst_alr2(j), flst_emitter(j), perm_list)
           end if
        end do
     end do
     !!! Check if new emitters require a rearrangement of ftuples
     do i = 1, nregions
        reg_data%regions(i)%alr = i
        reg_data%regions(i)%flst_real = flst_alr2(i)
        reg_data%regions(i)%mult = mult(i)
        reg_data%regions(i)%flst_uborn = flst_uborn(i)
        reg_data%regions(i)%emitter = flst_emitter(i)
        nreg = ftuples (index(i))%get_n_tuples ()
        reg_data%regions(i)%nregions = nreg
        allocate (reg_data%regions(i)%flst_allreg (nreg))
        do j = 1, nreg
           current_region = ftuples (index(i))%get_entry (j)
           if (.not. associated (current_region%equiv)) then
              call current_region%ftuple%get (i1, i2)
              if (i2 /= nlegs) &
                   call current_region%ftuple%set (i1, nlegs)
           end if
           reg_data%regions(i)%flst_allreg (j) = current_region%ftuple
        end do
     end do
     !!! Find underlying Born index
     do j = 1, nregions
        do i = 1, reg_data%n_flv_born
           if (reg_data%flv_born (i) == reg_data%regions(j)%flst_uborn) then
              reg_data%regions(j)%uborn_index = i
              exit
           end if
        end do
     end do

     k = 1
     associate (regions => reg_data%regions)
        do i = 1, nregions
           if (i==1) then
              regions(i)%real_index = 1
              flst_save = flst_alr2(1)
              cycle
           end if
           if (flst_alr2(i) == flst_save) then
              regions(i)%real_index = k
           else
              k = k+1
              regions(i)%real_index = k
              flst_save = flst_alr2(i)
           end if
        end do
     end associate
     reg_data%n_regions = size (reg_data%regions)

  end subroutine region_data_init_singular_regions

  subroutine region_data_find_emitters (reg_data)
    class(region_data_t), intent(inout) :: reg_data
    integer :: i, j, n
    integer :: em
    integer, dimension(10) :: em_count
    em_count = -1
    n = 0

    !!! Count the number of different emitters
    do i = 1, reg_data%n_regions
       em = reg_data%regions(i)%emitter
       if (.not. any (em_count == em)) then
          n = n+1
          em_count(i) = em
       end if
    end do

    if (n < 1) call msg_fatal ("region_data_find_emitters: No emitters found")
    reg_data%n_emitters = n
    allocate (reg_data%emitters (reg_data%n_emitters))
    reg_data%emitters = -1

    j = 1
    do i = 1, size(reg_data%regions)
       em = reg_data%regions(i)%emitter
       if (.not. any (reg_data%emitters == em)) then
          reg_data%emitters(j) = em
          j = j+1
       end if
    end do
  end subroutine region_data_find_emitters

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

  subroutine region_data_write (reg_data, u)
     class(region_data_t), intent(inout) :: reg_data
     integer, intent(in) :: u
     integer :: i, j
     integer :: nreal, nborn
     integer :: i1, i2, nreg
     integer :: maxnregions, nreg_diff
     integer :: nleft, nright
     type(singular_region_t) :: region
     type(string_t) :: flst_title, ftuple_title
     character(len=7) :: flst_format = "(I3,A1)"
     character(len=16) :: ireg_format = "(A1,I3,A1,I3,A2)"
     character(len=7) :: ireg_space_format = "(7X,A1)"

     maxnregions = 1
     do j = 1, reg_data%n_regions
        if (size (reg_data%regions(j)%flst_allreg) > maxnregions) &
             maxnregions = reg_data%regions(j)%nregions
     end do
     flst_title = '(A' // flst_title_format(reg_data%nlegs_real) // ')'
     ftuple_title = '(A' // ftuple_title_format() // ')'
     write (u,*) 'Total number of regions: ', size(reg_data%regions)
     write (u, '(A3)', advance = 'no') 'alr'
     call write_separator ()
     write (u, char (flst_title), advance = 'no') 'flst_real'
     call write_separator ()
     write (u, '(A3)', advance = 'no') 'em'
     call write_separator ()
     write (u, '(A3)', advance = 'no') 'mult'
     call write_separator ()
     write (u, '(A4)', advance = 'no') 'nreg'
     call write_separator ()
     write (u, char (ftuple_title), advance = 'no') 'ftuples'
     call write_separator ()
     flst_title = '(A' // flst_title_format(reg_data%nlegs_born) // ')'
     write (u, char (flst_title), advance = 'no') 'flst_born'
     call write_separator ()
     write (u, '(A7)') 'i_uborn'
     do j = 1, reg_data%n_regions
        region = reg_data%regions(j)
        nreal = size (region%flst_real%flst)
        nborn = size (region%flst_uborn%flst)
        write (u, '(I3)', advance = 'no') j
        call write_separator ()
        write (u, '(A1)', advance = 'no') '['
        do i = 1, nreal-1
           write (u, flst_format, advance = 'no') region%flst_real%flst(i), ','
        end do
        write (u, flst_format, advance = 'no') region%flst_real%flst(nreal), ']'
        call write_separator ()
        write (u, '(I3)', advance = 'no') region%emitter
        call write_separator ()
        write (u, '(I3)', advance = 'no') region%mult
        call write_separator ()
        write (u, '(I4)', advance = 'no') region%nregions
        call write_separator ()
        nreg = region%nregions
        if (nreg == maxnregions) then
           nleft = 0
           nright = 0
        else
           nreg_diff = maxnregions - nreg
           nleft = nreg_diff/2
           if (mod(nreg_diff,2) == 0) then
              nright = nleft
           else
              nright = nleft + 1
           end if
        end if
        if (nleft > 0) then
           do i=1,nleft
              write(u,ireg_space_format, advance='no') ' '
           end do
        end if
        write (u,'(A1)', advance = 'no') '{'
        if (nreg > 1) then
           do i=1,nreg-1
              call region%flst_allreg(i)%get (i1, i2)
              write(u,ireg_format,advance = 'no') '(', i1, ',', i2, '),'
           end do
        end if
        call region%flst_allreg(nreg)%get (i1, i2)
        write (u,ireg_format,advance = 'no') '(', i1, ',', i2, ')}'
        if (nright > 0) then
           do i=1,nright
              write(u,ireg_space_format, advance='no') ' '
           end do
        end if
        call write_separator ()
        write (u,'(A1)',advance = 'no') '['
        do i=1,nborn-1
           write(u,flst_format,advance = 'no') region%flst_uborn%flst(i), ','
        end do
        write (u,flst_format, advance = 'no') region%flst_uborn%flst(nborn), ']'
        call write_separator ()
        write (u, '(I7)', advance = 'no') region%uborn_index
        write(u,*) ''
     end do

   contains

     function flst_title_format (n) result (frmt)
       integer, intent(in) :: n
       type(string_t) :: frmt
       character(len=2) :: frmt_char
       write (frmt_char, '(I2)') 4*n+1
       frmt = var_str (frmt_char)
     end function flst_title_format

    function ftuple_title_format () result (frmt)
       type(string_t) :: frmt
       character(len=2) :: frmt_char
       write (frmt_char, '(I2)') 10*maxnregions+1
       frmt = var_str (frmt_char)
    end function ftuple_title_format

    subroutine write_separator ()
       character(len=10) :: sep_format = "(1X,A2,1X)"
       write (u, sep_format, advance = 'no') '||'
    end subroutine write_separator

  end subroutine region_data_write

  function region_to_index (list, i) result(index)
    type(ftuple_list_t), intent(inout), dimension(:), allocatable :: list
    integer, intent(in) :: i
    integer :: index
    integer :: nlist
    integer :: j
    integer, dimension(:), allocatable :: nreg
    nlist = size(list)
    allocate (nreg (nlist))
    do j = 1, nlist
       if (j == 1) then
          nreg(j) = list(j)%get_n_tuples ()
       else
          nreg(j) = nreg(j-1) + list(j)%get_n_tuples ()
       end if
    end do
    do j = 1, nlist
       if (j == 1) then
          if (i <= nreg(j)) then
             index = j
             exit
          end if
       else
          if (i > nreg(j-1) .and. i <= nreg(j)) then
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
    if (i_em > n_in) then
       flv2%flst(1:2) = flv1%flst(1:2)
       flv2%flst(n-1) = flv1%flst(i_em)
       flv2%flst(n) = flv1%flst(i_rad)
       call fill_remaining_flavors (n_in, .true.)
    else
       flv2%flst(1:2) = flv1%flst(1:2)
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
            j = j+1
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

  subroutine fks_mapping_default_set_parameter (map, n_in, dij_exp1, dij_exp2)
    class(fks_mapping_default_t), intent(inout) :: map
    integer, intent(in) :: n_in
    real(default), intent(in) :: dij_exp1, dij_exp2
    map%n_in = n_in
    map%exp_1 = dij_exp1
    map%exp_2 = dij_exp2
  end subroutine fks_mapping_default_set_parameter

  function fks_mapping_default_dij (map, p, i, j) result (d)
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    real(default) :: d
    real(default) :: y
    real(default) :: E1, E2

    !!! FSR Region
    if (i /= j) then
        if (i > map%n_in .and. j > map%n_in) then
           E1 = p(i)%p(0); E2 = p(j)%p(0)
           d = (2 * p(i) * p(j) * E1*E2 / (E1 + E2)**2)**map%exp_1
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
                 !!! case i,j <= 2 for scattering not yet implemented
                 d = 0
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

  function fks_mapping_default_compute_sumdij (map, sregion, p) result (d)
    class(fks_mapping_default_t), intent(in) :: map
    type(singular_region_t), intent(inout) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p
    real(default) :: d
    integer :: i, k, l

    associate (ftuples => sregion%flst_allreg)
      d = zero
      do i = 1, sregion%nregions
        call ftuples(i)%get (k, l)
        d = d + one / map%dij (p, k, l)
      end do
    end associate

  end function fks_mapping_default_compute_sumdij

  function fks_mapping_default_svalue (map, p, i, j) result (value)
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in) :: i, j
    real(default) :: value
    value = one / (map%dij (p, i, j) * map%sumdij)
  end function fks_mapping_default_svalue

  function fks_mapping_default_dij_soft (map, p_born, p_soft, em) result (d)
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: em
    real(default) :: d
    real(default) :: y
    if (em <= map%n_in) then
       y = polar_angle_ct (p_soft)
       select case (map%n_in)
       case (1)
          d = one - y**2
          !d = two * (one - y)
       case (2)
          select case (em)
          case (0)
             d = one - y**2
          case (1)
             d = two * (one - y)
          case (2)
             d = two * (one + y)
          end select
       end select
       d = d**map%exp_2
    else
       d = (two * p_born(em)*p_soft / energy(p_born(em)))**map%exp_1
    end if
  end function fks_mapping_default_dij_soft

  function fks_mapping_default_compute_sumdij_soft (map, sregion, p_born, p_soft) result (d)
    class(fks_mapping_default_t), intent(in) :: map
    type(singular_region_t), intent(inout) :: sregion
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    real(default) :: d
    integer :: i, k, l
    integer :: nlegs
    d = zero
    nlegs = size (sregion%flst_real%flst)
    associate (ftuples => sregion%flst_allreg)
      do i = 1, sregion%nregions
        call ftuples(i)%get (k,l)
        if (l == nlegs) then
          d = d + one / map%dij_soft (p_born, p_soft, k)
        end if
      end do
    end associate
  end function fks_mapping_default_compute_sumdij_soft

  function fks_mapping_default_svalue_soft (map, p_born, p_soft, em) result (value)
    class(fks_mapping_default_t), intent(in) :: map
    type(vector4_t), intent(in), dimension(:) :: p_born
    type(vector4_t), intent(in) :: p_soft
    integer, intent(in) :: em
    real(default) :: value
    value = one / (map%sumdij_soft*map%dij_soft (p_born, p_soft, em))
  end function fks_mapping_default_svalue_soft


end module fks_regions
