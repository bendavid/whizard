! WHIZARD 2.2.5 Feb 27 2015
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

module pdg_arrays

  use io_units
  use unit_tests
  use sorting
  use physics_defs, only: UNDEFINED

  implicit none
  private

  public :: pdg_array_t
  public :: pdg_array_write
  public :: assignment(=)
  public :: pdg_array_init
  public :: pdg_array_delete
  public :: pdg_array_merge
  public :: pdg_array_get_length
  public :: pdg_array_get
  public :: pdg_array_replace
  public :: operator(//)
  public :: operator(.match.)
  public :: is_quark
  public :: is_gluon
  public :: is_lepton
  public :: is_massless_vector
  public :: is_massive_vector
  public :: is_qcd_particle
  public :: operator(<)
  public :: operator(>)
  public :: operator(<=)
  public :: operator(>=)
  public :: operator(==)
  public :: operator(/=)
  public :: operator(.eqv.)
  public :: operator(.neqv.)
  public :: sort_abs
  public :: pdg_list_t
  public :: pdg_arrays_test 

  type :: pdg_array_t
     private
     integer, dimension(:), allocatable :: pdg
   contains
     procedure :: write => pdg_array_write
     procedure :: get_length => pdg_array_get_length
     procedure :: get => pdg_array_get
     procedure :: set => pdg_array_set
     procedure :: replace => pdg_array_replace
     procedure :: sort_abs => pdg_array_sort_abs
     procedure :: intersect => pdg_array_intersect
     procedure :: search_for_particle => pdg_array_search_for_particle
  end type pdg_array_t

  type :: pdg_list_t
     type(pdg_array_t), dimension(:), allocatable :: a
   contains
     procedure :: write => pdg_list_write
     generic :: init => pdg_list_init_size
     procedure, private :: pdg_list_init_size
     generic :: init => pdg_list_init_int_array
     procedure, private :: pdg_list_init_int_array
     generic :: set => pdg_list_set_int
     generic :: set => pdg_list_set_int_array
     generic :: set => pdg_list_set_pdg_array
     procedure, private :: pdg_list_set_int
     procedure, private :: pdg_list_set_int_array
     procedure, private :: pdg_list_set_pdg_array
     procedure :: get_size => pdg_list_get_size
     procedure :: get => pdg_list_get
     procedure :: is_regular => pdg_list_is_regular
     procedure :: sort_abs => pdg_list_sort_abs
     generic :: operator (==) => pdg_list_eq
     procedure, private :: pdg_list_eq
     generic :: operator (<) => pdg_list_lt
     procedure, private :: pdg_list_lt
     procedure :: replace => pdg_list_replace
     procedure :: match_replace => pdg_list_match_replace
     generic :: operator (.match.) => pdg_list_match_pdg_array
     procedure, private :: pdg_list_match_pdg_array
     procedure :: find_match => pdg_list_find_match_pdg_array
     procedure :: create_pdg_array => pdg_list_create_pdg_array
     procedure :: search_for_particle => pdg_list_search_for_particle
  end type pdg_list_t
  

  interface assignment(=)
     module procedure pdg_array_from_int_array
     module procedure pdg_array_from_int
     module procedure int_array_from_pdg_array
  end interface

  interface operator(//)
     module procedure concat_pdg_arrays
  end interface

  interface operator(.match.)
     module procedure pdg_array_match_integer
     module procedure pdg_array_match_pdg_array
  end interface

  interface operator(<)
     module procedure pdg_array_lt
  end interface
  interface operator(>)
     module procedure pdg_array_gt
  end interface
  interface operator(<=)
     module procedure pdg_array_le
  end interface
  interface operator(>=)
     module procedure pdg_array_ge
  end interface
  interface operator(==)
     module procedure pdg_array_eq
  end interface
  interface operator(/=)
     module procedure pdg_array_ne
  end interface

  interface operator(.eqv.)
     module procedure pdg_array_equivalent
  end interface
  interface operator(.neqv.)
     module procedure pdg_array_inequivalent
  end interface

  interface sort_abs
     module procedure pdg_array_sort_abs
  end interface
  

contains

  subroutine pdg_array_write (aval, unit)
    class(pdg_array_t), intent(in) :: aval
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no")  "PDG("
    if (allocated (aval%pdg)) then
       do i = 1, size (aval%pdg)
          if (i > 1)  write (u, "(A)", advance="no")  ", "
          write (u, "(I0)", advance="no")  aval%pdg(i)
       end do
    end if
    write (u, "(A)", advance="no")  ")"
  end subroutine pdg_array_write

  subroutine pdg_array_from_int_array (aval, iarray)
    type(pdg_array_t), intent(out) :: aval
    integer, dimension(:), intent(in) :: iarray
    allocate (aval%pdg (size (iarray)))
    aval%pdg = iarray
  end subroutine pdg_array_from_int_array

  elemental subroutine pdg_array_from_int (aval, int)
    type(pdg_array_t), intent(out) :: aval
    integer, intent(in) :: int
    allocate (aval%pdg (1))
    aval%pdg = int
  end subroutine pdg_array_from_int

  subroutine int_array_from_pdg_array (iarray, aval)
    integer, dimension(:), allocatable, intent(out) :: iarray
    type(pdg_array_t), intent(in) :: aval
    if (allocated (aval%pdg)) then
       allocate (iarray (size (aval%pdg)))
       iarray = aval%pdg
    else
       allocate (iarray (0))
    end if
  end subroutine int_array_from_pdg_array

   subroutine pdg_array_init (aval, n_elements)
     type(pdg_array_t), intent(inout) :: aval
     integer, intent(in) :: n_elements
     allocate(aval%pdg(n_elements))
   end subroutine pdg_array_init

  subroutine pdg_array_delete (aval)
    type(pdg_array_t), intent(inout) :: aval
    if (allocated (aval%pdg)) deallocate (aval%pdg)
  end subroutine pdg_array_delete

  subroutine pdg_array_merge (aval1, aval2)
    type(pdg_array_t), intent(inout) :: aval1
    type(pdg_array_t), intent(in) :: aval2
    type(pdg_array_t) :: aval
    if (allocated (aval1%pdg) .and. allocated (aval2%pdg)) then
      if (.not. any (aval1%pdg == aval2%pdg)) aval = aval1 // aval2
    else if (allocated (aval1%pdg)) then
      aval = aval1
    else if (allocated (aval2%pdg)) then
      aval = aval2
    end if
    call pdg_array_delete (aval1)
    aval1 = aval%pdg
  end subroutine pdg_array_merge

  elemental function pdg_array_get_length (aval) result (n)
    class(pdg_array_t), intent(in) :: aval
    integer :: n
    if (allocated (aval%pdg)) then
       n = size (aval%pdg)
    else
       n = 0
    end if
  end function pdg_array_get_length

  elemental function pdg_array_get (aval, i) result (pdg)
    class(pdg_array_t), intent(in) :: aval
    integer, intent(in), optional :: i
    integer :: pdg
    if (present (i)) then
       pdg = aval%pdg(i)
    else 
       pdg = aval%pdg(1)
    end if
  end function pdg_array_get

  subroutine pdg_array_set (aval, i, pdg)
    class(pdg_array_t), intent(inout) :: aval
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    aval%pdg(i) = pdg
  end subroutine pdg_array_set

  function pdg_array_replace (aval, i, pdg_new) result (aval_new)
    class(pdg_array_t), intent(in) :: aval
    integer, intent(in) :: i
    integer, dimension(:), intent(in) :: pdg_new
    type(pdg_array_t) :: aval_new
    integer :: n, l
    n = size (aval%pdg)
    l = size (pdg_new)
    allocate (aval_new%pdg (n + l - 1))
    aval_new%pdg(:i-1) = aval%pdg(:i-1)
    aval_new%pdg(i:i+l-1) = pdg_new
    aval_new%pdg(i+l:) = aval%pdg(i+1:)
  end function pdg_array_replace
    
  function concat_pdg_arrays (aval1, aval2) result (aval)
    type(pdg_array_t) :: aval
    type(pdg_array_t), intent(in) :: aval1, aval2
    integer :: n1, n2
    if (allocated (aval1%pdg) .and. allocated (aval2%pdg)) then
       n1 = size (aval1%pdg)
       n2 = size (aval2%pdg)
       allocate (aval%pdg (n1 + n2))
       aval%pdg(:n1) = aval1%pdg
       aval%pdg(n1+1:) = aval2%pdg
    else if (allocated (aval1%pdg)) then
       aval = aval1
    else if (allocated (aval2%pdg)) then
       aval = aval2
    end if
  end function concat_pdg_arrays

  elemental function pdg_array_match_integer (aval, pdg) result (flag)
    logical :: flag
    type(pdg_array_t), intent(in) :: aval
    integer, intent(in) :: pdg
    if (allocated (aval%pdg)) then
       flag = pdg == UNDEFINED &
            .or. any (aval%pdg == UNDEFINED) &
            .or. any (aval%pdg == pdg)
    else
       flag = .false.
    end if
  end function pdg_array_match_integer

  function is_quark (pdg_nr) result(res)
      integer, intent(in) :: pdg_nr
      logical :: res
      if (pdg_nr >= 1 .and. pdg_nr <= 6) then 
        res = .true.
      else
        res = .false.
      end if
   end function is_quark

   function is_gluon (pdg_nr) result(res)
     integer, intent(in) :: pdg_nr
     logical :: res
     if (pdg_nr == 21) then
       res = .true.
     else
       res = .false.
     end if
   end function is_gluon

  function is_lepton (pdg_nr) result(res)
    integer, intent(in) :: pdg_nr
    logical :: res
    if (pdg_nr >= 11 .and. pdg_nr <= 16) then
      res = .true.
    else
      res = .false.
    end if
  end function is_lepton

  function is_massless_vector (pdg_nr) result (res)
    integer, intent(in) :: pdg_nr
    logical :: res
    if (pdg_nr == 21 .or. pdg_nr == 22) then
      res = .true.
    else
      res = .false.
    end if
  end function is_massless_vector

  function is_massive_vector (pdg_nr) result (res)
    integer, intent(in) :: pdg_nr
    logical :: res
    if (pdg_nr == 23 .or. pdg_nr == 24) then
      res = .true.
    else
      res = .false.
    end if
  end function is_massive_vector

  function is_qcd_particle (pdg_nr) result (res)
    integer, intent(in) :: pdg_nr
    logical :: res
    res = .false.
    if (is_quark (abs (pdg_nr)) .or. is_gluon (pdg_nr)) &
      res = .true.
  end function is_qcd_particle

  function pdg_array_match_pdg_array (aval1, aval2) result (flag)
    logical :: flag
    type(pdg_array_t), intent(in) :: aval1, aval2
    if (allocated (aval1%pdg) .and. allocated (aval2%pdg)) then
       flag = any (aval1 .match. aval2%pdg)
    else
       flag = .false.
    end if
  end function pdg_array_match_pdg_array

  elemental function pdg_array_lt (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    integer :: i
    if (size (aval1%pdg) /= size (aval2%pdg)) then
       flag = size (aval1%pdg) < size (aval2%pdg)
    else
       do i = 1, size (aval1%pdg)
          if (abs (aval1%pdg(i)) /= abs (aval2%pdg(i))) then
             flag = abs (aval1%pdg(i)) < abs (aval2%pdg(i))
             return
          end if
       end do
       do i = 1, size (aval1%pdg)
          if (aval1%pdg(i) /= aval2%pdg(i)) then
             flag = aval1%pdg(i) > aval2%pdg(i)
             return
          end if
       end do
       flag = .false.
    end if
  end function pdg_array_lt

  elemental function pdg_array_gt (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    flag = .not. (aval1 < aval2 .or. aval1 == aval2)
  end function pdg_array_gt

  elemental function pdg_array_le (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    flag = aval1 < aval2 .or. aval1 == aval2
  end function pdg_array_le

  elemental function pdg_array_ge (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    flag = .not. (aval1 < aval2)
  end function pdg_array_ge

  elemental function pdg_array_eq (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    if (size (aval1%pdg) /= size (aval2%pdg)) then
       flag = .false.
    else
       flag = all (aval1%pdg == aval2%pdg)
    end if
  end function pdg_array_eq

  elemental function pdg_array_ne (aval1, aval2) result (flag)
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical :: flag
    flag = .not. (aval1 == aval2)
  end function pdg_array_ne

  elemental function pdg_array_equivalent (aval1, aval2) result (eq)
    logical :: eq
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical, dimension(:), allocatable :: match1, match2
    integer :: i
    if (allocated (aval1%pdg) .and. allocated (aval2%pdg)) then
       eq = any (aval1%pdg == UNDEFINED) &
            .or. any (aval2%pdg == UNDEFINED)
       if (.not. eq) then
          allocate (match1 (size (aval1%pdg)))
          allocate (match2 (size (aval2%pdg)))
          match1 = .false.
          match2 = .false.
          do i = 1, size (aval1%pdg)
             match2 = match2 .or. aval1%pdg(i) == aval2%pdg
          end do
          do i = 1, size (aval2%pdg)
             match1 = match1 .or. aval2%pdg(i) == aval1%pdg
          end do
          eq = all (match1) .and. all (match2)
       end if
    else
       eq = .false.
    end if
  end function pdg_array_equivalent

  elemental function pdg_array_inequivalent (aval1, aval2) result (neq)
    logical :: neq
    type(pdg_array_t), intent(in) :: aval1, aval2
    neq = .not. pdg_array_equivalent (aval1, aval2)
  end function pdg_array_inequivalent

  function pdg_array_sort_abs (aval1, unique) result (aval2)
    class(pdg_array_t), intent(in) :: aval1
    logical, intent(in), optional :: unique
    type(pdg_array_t) :: aval2
    integer, dimension(:), allocatable :: tmp
    logical, dimension(:), allocatable :: mask
    integer :: i, n
    logical :: uni
    uni = .false.;  if (present (unique))  uni = unique
    n = size (aval1%pdg)
    if (uni) then
       allocate (tmp (n), mask(n))
       tmp = sort_abs (aval1%pdg)
       mask(1) = .true.
       do i = 2, n
          mask(i) = tmp(i) /= tmp(i-1)
       end do
       allocate (aval2%pdg (count (mask)))
       aval2%pdg = pack (tmp, mask)
    else
       allocate (aval2%pdg (n))
       aval2%pdg = sort_abs (aval1%pdg)
    end if
  end function pdg_array_sort_abs

  function pdg_array_intersect (aval1, match) result (aval2)
   class(pdg_array_t), intent(in) :: aval1
   integer, dimension(:) :: match
   type(pdg_array_t) :: aval2
   integer, dimension(:), allocatable :: isec
   integer :: i
   isec = pack (aval1%pdg, [(any(aval1%pdg(i) == match), i=1,size(aval1%pdg))])
   aval2 = isec
  end function pdg_array_intersect 

  function pdg_array_search_for_particle (pdg, i_part) result (found)
    class(pdg_array_t), intent(in) :: pdg
    integer, intent(in) :: i_part
    logical :: found
    found = any (pdg%pdg == i_part)
  end function pdg_array_search_for_particle

  subroutine pdg_list_write (object, unit)
    class(pdg_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    if (allocated (object%a)) then
       do i = 1, size (object%a)
          if (i > 1)  write (u, "(A)", advance="no")  ", "
          call object%a(i)%write (u)
       end do
    end if
  end subroutine pdg_list_write
    
  subroutine pdg_list_init_size (pl, n)
    class(pdg_list_t), intent(out) :: pl
    integer, intent(in) :: n
    allocate (pl%a (n))
  end subroutine pdg_list_init_size
  
  subroutine pdg_list_init_int_array (pl, pdg)
    class(pdg_list_t), intent(out) :: pl
    integer, dimension(:), intent(in) :: pdg
    integer :: i
    allocate (pl%a (size (pdg)))
    do i = 1, size (pdg)
       pl%a(i) = pdg(i)
    end do
  end subroutine pdg_list_init_int_array
  
  subroutine pdg_list_set_int (pl, i, pdg)
    class(pdg_list_t), intent(inout) :: pl
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    pl%a(i) = pdg
  end subroutine pdg_list_set_int
  
  subroutine pdg_list_set_int_array (pl, i, pdg)
    class(pdg_list_t), intent(inout) :: pl
    integer, intent(in) :: i
    integer, dimension(:), intent(in) :: pdg
    pl%a(i) = pdg
  end subroutine pdg_list_set_int_array
  
  subroutine pdg_list_set_pdg_array (pl, i, pa)
    class(pdg_list_t), intent(inout) :: pl
    integer, intent(in) :: i
    type(pdg_array_t), intent(in) :: pa
    pl%a(i) = pa
  end subroutine pdg_list_set_pdg_array
  
  function pdg_list_get_size (pl) result (n)
    class(pdg_list_t), intent(in) :: pl
    integer :: n
    if (allocated (pl%a)) then
       n = size (pl%a)
    else
       n = 0
    end if
  end function pdg_list_get_size
  
  function pdg_list_get (pl, i) result (pa)
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: i
    type(pdg_array_t) :: pa
    pa = pl%a(i)
  end function pdg_list_get
  
  function pdg_list_is_regular (pl) result (flag)
    class(pdg_list_t), intent(in) :: pl
    logical :: flag
    integer :: i, j, s
    s = pl%get_size ()
    flag = .true.
    do i = 1, s
       do j = i + 1, s
          if (pl%a(i) .match. pl%a(j)) then
             if (pl%a(i) /= pl%a(j)) then
                flag = .false.
                return
             end if
          end if
       end do
    end do
  end function pdg_list_is_regular
  
  function pdg_list_sort_abs (pl, n_in) result (pl_sorted)
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in), optional :: n_in
    type(pdg_list_t) :: pl_sorted
    type(pdg_array_t), dimension(:), allocatable :: pa
    integer, dimension(:), allocatable :: pdg, map
    integer :: i, n0
    call pl_sorted%init (pl%get_size ())
    if (allocated (pl%a)) then
       allocate (pa (size (pl%a)))
       do i = 1, size (pl%a)
          pa(i) = pl%a(i)%sort_abs (unique = .true.)
       end do
       allocate (pdg (size (pa)), source = 0)
       do i = 1, size (pa)
          if (allocated (pa(i)%pdg)) then
             if (size (pa(i)%pdg) > 0) then
                pdg(i) = pa(i)%pdg(1)
             end if
          end if
       end do
       if (present (n_in)) then
          n0 = n_in
       else
          n0 = 0
       end if
       allocate (map (size (pdg)))
       map(:n0) = [(i, i = 1, n0)]
       map(n0+1:) = n0 + order_abs (pdg(n0+1:))
       do i = 1, size (pa)
          call pl_sorted%set (i, pa(map(i)))
       end do
    end if
  end function pdg_list_sort_abs
    
  function pdg_list_eq (pl1, pl2) result (flag)
    class(pdg_list_t), intent(in) :: pl1, pl2
    logical :: flag
    integer :: i
    flag = .false.
    if (allocated (pl1%a) .and. allocated (pl2%a)) then
       if (size (pl1%a) == size (pl2%a)) then
          do i = 1, size (pl1%a)
             associate (a1 => pl1%a(i), a2 => pl2%a(i))
               if (allocated (a1%pdg) .and. allocated (a2%pdg)) then
                  if (size (a1%pdg) == size (a2%pdg)) then
                     if (size (a1%pdg) > 0) then
                        if (a1%pdg(1) /= a2%pdg(1)) return
                     end if
                  else
                     return
                  end if
               else
                  return
               end if
             end associate
          end do
          flag = .true.
       end if
    end if
  end function pdg_list_eq
  
  function pdg_list_lt (pl1, pl2) result (flag)
    class(pdg_list_t), intent(in) :: pl1, pl2
    logical :: flag
    integer :: i
    flag = .false.
    if (allocated (pl1%a) .and. allocated (pl2%a)) then
       if (size (pl1%a) < size (pl2%a)) then
          flag = .true.;  return
       else if (size (pl1%a) > size (pl2%a)) then
          return
       else
          do i = 1, size (pl1%a)
             associate (a1 => pl1%a(i), a2 => pl2%a(i))
               if (allocated (a1%pdg) .and. allocated (a2%pdg)) then
                  if (size (a1%pdg) < size (a2%pdg)) then
                     flag = .true.;  return
                  else if (size (a1%pdg) > size (a2%pdg)) then
                     return
                  else
                     if (size (a1%pdg) > 0) then
                        if (abs (a1%pdg(1)) < abs (a2%pdg(1))) then
                           flag = .true.;  return
                        else if (abs (a1%pdg(1)) > abs (a2%pdg(1))) then
                           return
                        else if (a1%pdg(1) > 0 .and. a2%pdg(1) < 0) then
                           flag = .true.;  return
                        else if (a1%pdg(1) < 0 .and. a2%pdg(1) > 0) then
                           return
                        end if
                     end if
                  end if
               else
                  return
               end if
             end associate
          end do
          flag = .false.
       end if
    end if
  end function pdg_list_lt
  
  function pdg_list_replace (pl, i, pl_insert, n_in) result (pl_out)
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: i
    class(pdg_list_t), intent(in) :: pl_insert
    integer, intent(in), optional :: n_in
    type(pdg_list_t) :: pl_out
    integer :: n, n_insert, n_out, k
    n = pl%get_size ()
    n_insert = pl_insert%get_size ()
    n_out = n + n_insert - 1
    call pl_out%init (n_out)
!    if (allocated (pl%a)) then
       do k = 1, i - 1
          pl_out%a(k) = pl%a(k)
       end do
!    end if
    if (present (n_in)) then
       pl_out%a(i) = pl_insert%a(1)
       do k = i + 1, n_in
          pl_out%a(k) = pl%a(k)
       end do
       do k = 1, n_insert - 1
          pl_out%a(n_in+k) = pl_insert%a(1+k)
       end do
       do k = 1, n - n_in
          pl_out%a(n_in+k+n_insert-1) = pl%a(n_in+k)
       end do
    else
!       if (allocated (pl_insert%a)) then
          do k = 1, n_insert
             pl_out%a(i-1+k) = pl_insert%a(k)
          end do
!       end if
!       if (allocated (pl%a)) then
          do k = 1, n - i
             pl_out%a(i+n_insert-1+k) = pl%a(i+k)
          end do
       end if
!    end if
  end function pdg_list_replace
    
  subroutine pdg_list_match_replace (pl, pl_match, success)
    class(pdg_list_t), intent(inout) :: pl
    class(pdg_list_t), intent(in) :: pl_match
    logical, intent(out) :: success
    integer :: i, j
    success = .true.
    SCAN_ENTRIES: do i = 1, size (pl%a)
       do j = 1, size (pl_match%a)
          if (pl%a(i) .match. pl_match%a(j)) then
             pl%a(i) = pl_match%a(j)
             cycle SCAN_ENTRIES
          end if
       end do
       success = .false.
       return
    end do SCAN_ENTRIES
  end subroutine pdg_list_match_replace
 
  function pdg_list_match_pdg_array (pl, pa) result (flag)
    class(pdg_list_t), intent(in) :: pl
    type(pdg_array_t), intent(in) :: pa
    logical :: flag
    flag = pl%find_match (pa) /= 0
  end function pdg_list_match_pdg_array
  
  function pdg_list_find_match_pdg_array (pl, pa, mask) result (i)
    class(pdg_list_t), intent(in) :: pl
    type(pdg_array_t), intent(in) :: pa
    logical, dimension(:), intent(in), optional :: mask
    integer :: i
    do i = 1, size (pl%a)
       if (present (mask)) then
          if (.not. mask(i))  cycle
       end if
       if (pl%a(i) .match. pa)  return
    end do
    i = 0
  end function pdg_list_find_match_pdg_array
  
  subroutine pdg_list_create_pdg_array (pl, pdg)
    class(pdg_list_t), intent(in) :: pl
    type(pdg_array_t), dimension(:), intent(inout), allocatable :: pdg
    integer :: n_elements
    integer :: i
    associate (a => pl%a)
      n_elements = size (a)
      if (allocated (pdg))  deallocate (pdg)
      allocate (pdg (n_elements))
      do i = 1, n_elements
         pdg(i) = a(i)
      end do
    end associate
  end subroutine pdg_list_create_pdg_array

  function pdg_list_search_for_particle (pl, i_part) result (found)
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: i_part
    logical :: found
    integer :: i_pl
    do i_pl = 1, size (pl%a)
       found = pl%a(i_pl)%search_for_particle (i_part)
       if (found) return
    end do
  end function pdg_list_search_for_particle

  subroutine pdg_arrays_test (u, results)
    integer, intent(in) :: u
    type (test_results_t), intent(inout) :: results
    call test (pdg_arrays_1, "pdg_arrays_1", &
         "create and sort PDG array", &
         u, results) 
    call test (pdg_arrays_2, "pdg_arrays_2", &
         "create and sort PDG lists", &
         u, results) 
    call test (pdg_arrays_3, "pdg_arrays_3", &
         "check PDG lists", &
         u, results) 
    call test (pdg_arrays_4, "pdg_arrays_4", &
         "compare PDG lists", &
         u, results) 
    call test (pdg_arrays_5, "pdg_arrays_5", &
         "match PDG lists", &
         u, results)   
  end subroutine pdg_arrays_test


  subroutine pdg_arrays_1 (u)
    integer, intent(in) :: u

    type(pdg_array_t) :: pa, pa1, pa2, pa3, pa4, pa5, pa6
    integer, dimension(:), allocatable :: pdg

    write (u, "(A)")  "* Test output: pdg_arrays_1"
    write (u, "(A)")  "*   Purpose: create and sort PDG arrays"
    write (u, "(A)")
    
    write (u, "(A)")  "* Assignment"
    write (u, "(A)")
    
    call pa%write (u)
    write (u, *)
    write (u, "(A,I0)")  "length = ", pa%get_length ()
    pdg = pa
    write (u, "(A,3(1x,I0))")  "contents = ", pdg
    
    write (u, *)
    pa = 1
    call pa%write (u)
    write (u, *)
    write (u, "(A,I0)")  "length = ", pa%get_length ()
    pdg = pa
    write (u, "(A,3(1x,I0))")  "contents = ", pdg
    
    write (u, *)
    pa = [1, 2, 3]
    call pa%write (u)
    write (u, *)
    write (u, "(A,I0)")  "length = ", pa%get_length ()
    pdg = pa
    write (u, "(A,3(1x,I0))")  "contents = ", pdg
    write (u, "(A,I0)")  "element #2 = ", pa%get (2)
    
    write (u, *)
    write (u, "(A)")  "* Replace"
    write (u, *)

    pa = pa%replace (2, [-5, 5, -7])
    call pa%write (u)
    write (u, *)
    
    write (u, *)
    write (u, "(A)")  "* Sort"
    write (u, *)

    pa = [1, -7, 3, -5, 5, 3]
    call pa%write (u)
    write (u, *)
    pa1 = pa%sort_abs ()
    pa2 = pa%sort_abs (unique = .true.)
    call pa1%write (u)
    write (u, *)
    call pa2%write (u)
    write (u, *)
    
    write (u, *)
    write (u, "(A)")  "* Compare"
    write (u, *)

    pa1 = [1, 3]
    pa2 = [1, 2, -2]
    pa3 = [1, 2, 4]
    pa4 = [1, 2, 4]
    pa5 = [1, 2, -4]
    pa6 = [1, 2, -3]
    
    write (u, "(A,6(1x,L1))")  "< ", &
         pa1 < pa2, pa2 < pa3, pa3 < pa4, pa4 < pa5, pa5 < pa6, pa6 < pa1
    write (u, "(A,6(1x,L1))")  "> ", &
         pa1 > pa2, pa2 > pa3, pa3 > pa4, pa4 > pa5, pa5 > pa6, pa6 > pa1
    write (u, "(A,6(1x,L1))")  "<=", &
         pa1 <= pa2, pa2 <= pa3, pa3 <= pa4, pa4 <= pa5, pa5 <= pa6, pa6 <= pa1
    write (u, "(A,6(1x,L1))")  ">=", &
         pa1 >= pa2, pa2 >= pa3, pa3 >= pa4, pa4 >= pa5, pa5 >= pa6, pa6 >= pa1
    write (u, "(A,6(1x,L1))")  "==", &
         pa1 == pa2, pa2 == pa3, pa3 == pa4, pa4 == pa5, pa5 == pa6, pa6 == pa1
    write (u, "(A,6(1x,L1))")  "/=", &
         pa1 /= pa2, pa2 /= pa3, pa3 /= pa4, pa4 /= pa5, pa5 /= pa6, pa6 /= pa1
   
    write (u, *)
    pa1 = [0]
    pa2 = [1, 2]
    pa3 = [1, -2]
    
    write (u, "(A,6(1x,L1))")  "eqv ", &
         pa1 .eqv. pa1, pa1 .eqv. pa2, &
         pa2 .eqv. pa2, pa2 .eqv. pa3
    
    write (u, "(A,6(1x,L1))")  "neqv", &
         pa1 .neqv. pa1, pa1 .neqv. pa2, &
         pa2 .neqv. pa2, pa2 .neqv. pa3
    

    write (u, *)
    write (u, "(A,6(1x,L1))")  "match", &
         pa1 .match. 0, pa1 .match. 1, &
         pa2 .match. 0, pa2 .match. 1, pa2 .match. 3

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: pdg_arrays_1"        
    
  end subroutine pdg_arrays_1
  
  subroutine pdg_arrays_2 (u)
    integer, intent(in) :: u

    type(pdg_array_t) :: pa
    type(pdg_list_t) :: pl, pl1

    write (u, "(A)")  "* Test output: pdg_arrays_2"
    write (u, "(A)")  "*   Purpose: create and sort PDG lists"
    write (u, "(A)")
    
    write (u, "(A)")  "* Assignment"
    write (u, "(A)")
    
    call pl%init (3)
    call pl%set (1, 42)
    call pl%set (2, [3, 2])
    pa = [5, -5]
    call pl%set (3, pa)
    call pl%write (u)
    write (u, *)
    write (u, "(A,I0)")  "size = ", pl%get_size ()

    write (u, "(A)")
    write (u, "(A)")  "* Sort"
    write (u, "(A)")
    
    pl = pl%sort_abs ()
    call pl%write (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Extract item #3"
    write (u, "(A)")
    
    pa = pl%get (3)
    call pa%write (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Replace item #3"
    write (u, "(A)")
    
    call pl1%init (2)
    call pl1%set (1, [2, 4])
    call pl1%set (2, -7)
    
    pl = pl%replace (3, pl1)
    call pl%write (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: pdg_arrays_2"        
    
  end subroutine pdg_arrays_2
  
  subroutine pdg_arrays_3 (u)
    integer, intent(in) :: u

    type(pdg_list_t) :: pl

    write (u, "(A)")  "* Test output: pdg_arrays_3"
    write (u, "(A)")  "*   Purpose: check for regular PDG lists"
    write (u, "(A)")
    
    write (u, "(A)")  "* Regular list"
    write (u, "(A)")
    
    call pl%init (4)
    call pl%set (1, [1, 2])
    call pl%set (2, [1, 2])
    call pl%set (3, [5, -5])
    call pl%set (4, 42)
    call pl%write (u)
    write (u, *)
    write (u, "(L1)") pl%is_regular ()

    write (u, "(A)")
    write (u, "(A)")  "* Irregular list"
    write (u, "(A)")
    
    call pl%init (4)
    call pl%set (1, [1, 2])
    call pl%set (2, [1, 2])
    call pl%set (3, [2, 5, -5])
    call pl%set (4, 42)
    call pl%write (u)
    write (u, *)
    write (u, "(L1)") pl%is_regular ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: pdg_arrays_3"        
    
  end subroutine pdg_arrays_3
  
  subroutine pdg_arrays_4 (u)
    integer, intent(in) :: u

    type(pdg_list_t) :: pl1, pl2, pl3

    write (u, "(A)")  "* Test output: pdg_arrays_4"
    write (u, "(A)")  "*   Purpose: check for regular PDG lists"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create lists"
    write (u, "(A)")
    
    call pl1%init (4)
    call pl1%set (1, [1, 2])
    call pl1%set (2, [1, 2])
    call pl1%set (3, [5, -5])
    call pl1%set (4, 42)
    write (u, "(I1,1x)", advance = "no")  1
    call pl1%write (u)
    write (u, *)

    call pl2%init (2)
    call pl2%set (1, 3)
    call pl2%set (2, [5, -5])
    write (u, "(I1,1x)", advance = "no")  2
    call pl2%write (u)
    write (u, *)

    call pl3%init (2)
    call pl3%set (1, 4)
    call pl3%set (2, [5, -5])
    write (u, "(I1,1x)", advance = "no")  3
    call pl3%write (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* a == b"
    write (u, "(A)")
    
    write (u, "(2x,A)")  "123"
    write (u, *)
    write (u, "(I1,1x,4L1)")  1, pl1 == pl1, pl1 == pl2, pl1 == pl3
    write (u, "(I1,1x,4L1)")  2, pl2 == pl1, pl2 == pl2, pl2 == pl3
    write (u, "(I1,1x,4L1)")  3, pl3 == pl1, pl3 == pl2, pl3 == pl3

    write (u, "(A)")
    write (u, "(A)")  "* a < b"
    write (u, "(A)")
    
    write (u, "(2x,A)")  "123"
    write (u, *)
    write (u, "(I1,1x,4L1)")  1, pl1 < pl1, pl1 < pl2, pl1 < pl3
    write (u, "(I1,1x,4L1)")  2, pl2 < pl1, pl2 < pl2, pl2 < pl3
    write (u, "(I1,1x,4L1)")  3, pl3 < pl1, pl3 < pl2, pl3 < pl3

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: pdg_arrays_4"        
    
  end subroutine pdg_arrays_4
  
  subroutine pdg_arrays_5 (u)
    integer, intent(in) :: u

    type(pdg_list_t) :: pl1, pl2, pl3
    logical :: success

    write (u, "(A)")  "* Test output: pdg_arrays_5"
    write (u, "(A)")  "*   Purpose: match-replace"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create lists"
    write (u, "(A)")
    
    call pl1%init (2)
    call pl1%set (1, [1, 2])
    call pl1%set (2, 42)
    call pl1%write (u)
    write (u, *)
    call pl3%init (2)
    call pl3%set (1, [42, -42])
    call pl3%set (2, [1, 2, 3, 4])
    call pl1%match_replace (pl3, success)
    call pl3%write (u)
    write (u, "(1x,A,1x,L1,':',1x)", advance="no")  "=>", success
    call pl1%write (u)
    write (u, *)

    write (u, *)

    call pl2%init (2)
    call pl2%set (1, 9)
    call pl2%set (2, 42)
    call pl2%write (u)
    write (u, *)
    call pl2%match_replace (pl3, success)
    call pl3%write (u)
    write (u, "(1x,A,1x,L1,':',1x)", advance="no")  "=>", success
    call pl2%write (u)
    write (u, *)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: pdg_arrays_5"        
    
  end subroutine pdg_arrays_5
  

end module pdg_arrays
