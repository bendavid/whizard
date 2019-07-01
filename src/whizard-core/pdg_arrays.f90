! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  use file_utils !NODEP!
  use sorting
  use unit_tests

  implicit none
  private

  public :: pdg_array_t
  public :: pdg_array_write
  public :: assignment(=)
  public :: pdg_array_get_length
  public :: pdg_array_get
  public :: pdg_array_replace
  public :: operator(//)
  public :: operator(.match.)
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

  integer, parameter, public :: UNDEFINED = 0

  type :: pdg_array_t
     private
     integer, dimension(:), allocatable :: pdg
   contains
     procedure :: write => pdg_array_write
     procedure :: get_length => pdg_array_get_length
     procedure :: get => pdg_array_get
     procedure :: replace => pdg_array_replace
     procedure :: sort_abs => pdg_array_sort_abs
  end type pdg_array_t

  type :: pdg_list_t
     type(pdg_array_t), dimension(:), allocatable :: a
   contains
     procedure :: write => pdg_list_write
     procedure :: init => pdg_list_init
     generic :: set => pdg_list_set_int
     generic :: set => pdg_list_set_int_array
     generic :: set => pdg_list_set_pdg_array
     procedure, private :: pdg_list_set_int
     procedure, private :: pdg_list_set_int_array
     procedure, private :: pdg_list_set_pdg_array
     procedure :: get_size => pdg_list_get_size
     procedure :: get => pdg_list_get
     procedure :: sort_abs => pdg_list_sort_abs
     procedure :: replace => pdg_list_replace
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
    u = output_unit (unit);  if (u < 0)  return
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
    integer, intent(in) :: i
    integer :: pdg
    pdg = aval%pdg(i)
  end function pdg_array_get

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

  subroutine pdg_list_write (object, unit)
    class(pdg_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    if (allocated (object%a)) then
       do i = 1, size (object%a)
          if (i > 1)  write (u, "(A)", advance="no")  ", "
          call object%a(i)%write (u)
       end do
    end if
  end subroutine pdg_list_write
    
  subroutine pdg_list_init (pl, n)
    class(pdg_list_t), intent(out) :: pl
    integer, intent(in) :: n
    allocate (pl%a (n))
  end subroutine pdg_list_init
  
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
  
  function pdg_list_sort_abs (pl) result (pl_sorted)
    class(pdg_list_t), intent(in) :: pl
    type(pdg_list_t) :: pl_sorted
    type(pdg_array_t), dimension(:), allocatable :: pa
    integer, dimension(:), allocatable :: pdg, map
    integer :: i
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
       allocate (map (size (pdg)))
       map = order_abs (pdg)
       do i = 1, size (pa)
          call pl_sorted%set (i, pa(map(i)))
       end do
    end if
  end function pdg_list_sort_abs
    
  function pdg_list_replace (pl, i, pl_insert) result (pl_out)
    class(pdg_list_t), intent(in) :: pl
    integer, intent(in) :: i
    class(pdg_list_t), intent(in) :: pl_insert
    type(pdg_list_t) :: pl_out
    integer :: n, n_insert, n_out, k
    n = pl%get_size ()
    n_insert = pl_insert%get_size ()
    n_out = n + n_insert - 1
    call pl_out%init (n_out)
    if (allocated (pl%a)) then
       do k = 1, i - 1
          pl_out%a(k) = pl%a(k)
       end do
    end if
    if (allocated (pl_insert%a)) then
       do k = 1, n_insert
          pl_out%a(i-1+k) = pl_insert%a(k)
       end do
    end if
    if (allocated (pl%a)) then
       do k = 1, n - i
          pl_out%a(i+n_insert-1+k) = pl%a(i+k)
       end do
    end if
  end function pdg_list_replace
    
  subroutine pdg_arrays_test (u, results)
    integer, intent(in) :: u
    type (test_results_t), intent(inout) :: results
    call test (pdg_arrays_1, "pdg_arrays_1", &
         "create and sort PDG array", &
         u, results) 
    call test (pdg_arrays_2, "pdg_arrays_2", &
         "create and sort PDG array", &
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
  

end module pdg_arrays
