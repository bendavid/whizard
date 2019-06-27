! WHIZARD 2.0.0 Mon Apr 12 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module colors

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!

  implicit none
  private

  public :: color_t
  public :: color_init
  public :: color_init_col_acl
  public :: color_init_from_array
  public :: color_set_ghost
  public :: color_undefine
  public :: color_write
  public :: color_write_raw
  public :: color_read_raw
  public :: color_is_defined
  public :: color_is_diagonal
  public :: color_is_ghost
  public :: color_get_col
  public :: color_get_acl
  public :: color_get_max_value
  !!! ifort 11.1 rev5 bug
  public :: color_get_max_value0
  public :: color_get_max_value1
  public :: color_get_max_value2
  public :: operator(.match.)
  public :: operator(==)
  public :: operator(/=)
  public :: color_add_offset
  public :: color_canonicalize
  public :: color_array_make_contractions
  public :: color_invert
  public :: set_color_map
  public :: color_translate
  public :: operator(.merge.)
  public :: compute_color_factor
  public :: count_color_loops
  public :: color_test

  type :: color_t
     private
!     integer, dimension(:), allocatable :: c1, c2
     integer, dimension(2) :: c1 = 0, c2 = 0
     logical :: ghost = .false.
  end type color_t


  interface color_init
     module procedure color_init_undefined, color_init_undefined_ghost
     module procedure color_init_array, color_init_array_ghost
     module procedure color_init_arrays, color_init_arrays_ghost
  end interface

  interface color_init_from_array
     module procedure color_init_from_array1, color_init_from_array1g
     module procedure color_init_from_array2, color_init_from_array2g
  end interface

  interface color_write
     module procedure color_write_single
     module procedure color_write_array
  end interface
  interface color_ghost_parity
     module procedure color_ghost_parity0
     module procedure color_ghost_parity1
  end interface
  interface color_get_max_value
     module procedure color_get_max_value0
     module procedure color_get_max_value1
     module procedure color_get_max_value2
  end interface

  interface operator(.match.)
     module procedure color_match
  end interface
  interface operator(==)
     module procedure color_eq
  end interface
  interface operator(/=)
     module procedure color_neq
  end interface
  interface color_translate
     module procedure color_translate0
     module procedure color_translate0_offset
     module procedure color_translate1
  end interface

  interface operator(.merge.)
     module procedure merge_colors
  end interface

contains

  pure subroutine color_init_undefined (col)
    type(color_t), intent(out) :: col
  end subroutine color_init_undefined

  pure subroutine color_init_undefined_ghost (col, ghost)
    type(color_t), intent(out) :: col
    logical, intent(in) :: ghost
    col%ghost = ghost
  end subroutine color_init_undefined_ghost

  pure subroutine color_init_array (col, c1)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1
!    allocate (col%c1 (size (c1)))
!    allocate (col%c2 (size (c1)))
    col%c1 = pack (c1, c1 /= 0, col%c1)
    col%c2 = col%c1
  end subroutine color_init_array

  pure subroutine color_init_array_ghost (col, c1, ghost)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1
    logical, intent(in) :: ghost
    call color_init_array (col, c1)
    col%ghost = ghost
  end subroutine color_init_array_ghost

  pure subroutine color_init_arrays (col, c1, c2)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1, c2
    if (size (c1) == size (c2)) then
!       allocate (col%c1 (size (c1)))
!       allocate (col%c2 (size (c2)))
       col%c1 = pack (c1, c1 /= 0, col%c1)
       col%c2 = pack (c2, c2 /= 0, col%c2)
    else if (size (c1) /= 0) then
!       allocate (col%c1 (size (c1)))
!       allocate (col%c2 (size (c1)))
       col%c1 = pack (c1, c1 /= 0, col%c1)
       col%c2 = col%c1
    else if (size (c2) /= 0) then
!       allocate (col%c1 (size (c2)))
!       allocate (col%c2 (size (c2)))
       col%c1 = pack (c2, c2 /= 0, col%c2)
       col%c2 = col%c1
    end if
  end subroutine color_init_arrays

  pure subroutine color_init_arrays_ghost (col, c1, c2, ghost)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1, c2
    logical, intent(in) :: ghost
    call color_init_arrays (col, c1, c2)
    col%ghost = ghost
  end subroutine color_init_arrays_ghost

  elemental subroutine color_init_col_acl (col, col_in, acl_in)
    type(color_t), intent(out) :: col
    integer, intent(in) :: col_in, acl_in
    integer, dimension(0) :: null_array
    select case (col_in)
    case (0)
       select case (acl_in)
       case (0)
          call color_init_array (col, null_array)
       case default
          call color_init_array (col, (/ -acl_in /))
       end select
    case default
       select case (acl_in)
       case (0)
          call color_init_array (col, (/ col_in /))
       case default
          call color_init_array (col, (/ col_in, -acl_in /))
       end select
    end select
  end subroutine color_init_col_acl

  pure subroutine color_init_from_array1 (col, c1)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1
    logical, dimension(size(c1)) :: mask
    mask = c1 /= 0
!    allocate (col%c1 (count (mask)))
!    allocate (col%c2 (size (col%c1)))
    col%c1 = pack (c1, mask, col%c1)
    col%c2 = col%c1
  end subroutine color_init_from_array1

  pure subroutine color_init_from_array1g (col, c1, ghost)
    type(color_t), intent(out) :: col
    integer, dimension(:), intent(in) :: c1
    logical, intent(in) :: ghost
    call color_init_from_array1 (col, c1)
    col%ghost = ghost
  end subroutine color_init_from_array1g

  pure subroutine color_init_from_array2 (col, c1)
    integer, dimension(:,:), intent(in) :: c1
    type(color_t), dimension(size(c1,2)), intent(out) :: col
    integer :: i
    do i = 1, size (c1,2)
       call color_init_from_array1 (col(i), c1(:,i))
    end do
  end subroutine color_init_from_array2

  pure subroutine color_init_from_array2g (col, c1, ghost)
    integer, dimension(:,:), intent(in) :: c1
    type(color_t), dimension(size(c1,2)), intent(out) :: col
    logical, intent(in), dimension(:) :: ghost
    call color_init_from_array2 (col, c1)
    col%ghost = ghost
  end subroutine color_init_from_array2g

  elemental subroutine color_set_ghost (col, ghost)
    type(color_t), intent(inout) :: col
    logical, intent(in) :: ghost
    col%ghost = ghost
  end subroutine color_set_ghost

  elemental subroutine color_undefine (col, undefine_ghost)
    type(color_t), intent(inout) :: col
    logical, intent(in), optional :: undefine_ghost
!    if (allocated (col%c1))  deallocate (col%c1)
!    if (allocated (col%c2))  deallocate (col%c2)
    col%c1 = 0
    col%c2 = 0
    if (present (undefine_ghost)) then
       if (undefine_ghost)  col%ghost = .false.
    else
       col%ghost = .false.
    end if
  end subroutine color_undefine

  subroutine color_write_single (col, unit)
    type(color_t), intent(in) :: col
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (color_is_defined (col)) then
       write (u, "(A)", advance="no")  "c("
       if (col%c1(1) /= 0)  write (u, "(I0)", advance="no")  col%c1(1)
       if (any (col%c1 /= 0))  write (u, "(1x)", advance="no")
       if (col%c1(2) /= 0)  write (u, "(I0)", advance="no")  col%c1(2)
       if (.not. color_is_diagonal (col)) then
          write (u, "(A)", advance="no")  "|"
          if (col%c2(1) /= 0)  write (u, "(I0)", advance="no")  col%c2(1)
          if (any (col%c2 /= 0))  write (u, "(1x)", advance="no")
          if (col%c2(2) /= 0)  write (u, "(I0)", advance="no")  col%c2(2)
       end if
       write (u, "(A)", advance="no") ")"
    else if (col%ghost) then
       write (u, "(A)", advance="no")  "c*"
    end if
  end subroutine color_write_single

  subroutine color_write_array (col, unit)
    type(color_t), dimension(:), intent(in) :: col
    integer, intent(in), optional :: unit
    integer :: u
    integer :: i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no") "["
    do i = 1, size (col)
       if (i > 1)  write (u, "(1x)", advance="no")
       call color_write_single (col(i), u)
    end do
    write (u, "(A)", advance="no") "]"
  end subroutine color_write_array

  subroutine color_write_raw (col, u)
    type(color_t), intent(in) :: col
    integer, intent(in) :: u
    logical :: defined
    defined = color_is_defined (col) .or. color_is_ghost (col)
    write (u) defined
    if (defined) then
       write (u) col%c1, col%c2
       write (u) col%ghost
    end if
  end subroutine color_write_raw
    
  subroutine color_read_raw (col, u, iostat)
    type(color_t), intent(out) :: col
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    logical :: defined
    read (u, iostat=iostat) defined
    if (defined) then
       read (u, iostat=iostat) col%c1, col%c2
       read (u, iostat=iostat) col%ghost
    end if
  end subroutine color_read_raw

  elemental function color_is_defined (col) result (defined)
    logical :: defined
    type(color_t), intent(in) :: col
!    defined = allocated (col%c1)
    defined = any (col%c1 /= 0)
  end function color_is_defined

  elemental function color_is_diagonal (col) result (diagonal)
    logical :: diagonal
    type(color_t), intent(in) :: col
    if (color_is_defined (col)) then
       diagonal = all (col%c1 == col%c2)
    else
       diagonal = .true.
    end if
  end function color_is_diagonal

  elemental function color_is_ghost (col) result (ghost)
    logical :: ghost
    type(color_t), intent(in) :: col
    ghost = col%ghost
  end function color_is_ghost

  pure function color_ghost_parity0 (col) result (parity)
    type(color_t), intent(in) :: col
    logical :: parity
    parity = color_is_ghost (col)
  end function color_ghost_parity0

  pure function color_ghost_parity1 (col) result (parity)
    type(color_t), dimension(:), intent(in) :: col
    logical :: parity
    logical, dimension(size(col)) :: p
    integer :: i
    forall (i = 1:size(col))
       p(i) = color_ghost_parity0 (col(i))
    end forall
    parity = mod (count (p), 2) == 1
  end function color_ghost_parity1

  elemental function color_number (col) result (n)
    integer :: n
    type(color_t), intent(in) :: col
!    n = size (col%c1)
    n = count (col%c1 /= 0)
  end function color_number

  function color_get_col (col) result (c)
    integer :: c
    type(color_t), intent(in) :: col
    integer :: i
    do i = 1, size (col%c1)
       if (col%c1(i) > 0) then
          c = col%c1(i)
          return
       end if
    end do
    c = 0
  end function color_get_col

  function color_get_acl (col) result (c)
    integer :: c
    type(color_t), intent(in) :: col
    integer :: i
    do i = 1, size (col%c1)
       if (col%c1(i) < 0) then
          c = - col%c1(i)
          return
       end if
    end do
    c = 0
  end function color_get_acl

  elemental function color_get_max_value0 (col) result (cmax)
    integer :: cmax
    type(color_t), intent(in) :: col
    cmax = maxval (abs (col%c1))
  end function color_get_max_value0

  pure function color_get_max_value1 (col) result (cmax)
    integer :: cmax
    type(color_t), dimension(:), intent(in) :: col
    cmax = maxval (color_get_max_value0 (col))
  end function color_get_max_value1

  function color_get_max_value2 (col) result (cmax)
    integer :: cmax
    type(color_t), dimension(:,:), intent(in) :: col
    integer, dimension(size(col, 2)) :: cm
    integer :: i
    forall (i = 1:size(col, 2))
       cm(i) = color_get_max_value1 (col(:,i))
    end forall
    cmax = maxval (cm)
  end function color_get_max_value2

  elemental function color_match (col1, col2) result (eq)
    logical :: eq
    type(color_t), intent(in) :: col1, col2
    if (color_is_defined (col1) .and. color_is_defined (col2)) then
!       if (size (col1%c1) == size (col2%c1)) then
          eq = all (col1%c1 == col2%c1) .and. all (col1%c2 == col2%c2)
!       else
!          eq = .false.
!       end if
    else
       eq = .true.
    end if
  end function color_match

  elemental function color_eq (col1, col2) result (eq)
    logical :: eq
    type(color_t), intent(in) :: col1, col2
    if (color_is_defined (col1) .and. color_is_defined (col2)) then
!       if (size (col1%c1) == size (col2%c1)) then
          eq = all (col1%c1 == col2%c1) .and. all (col1%c2 == col2%c2) &
               .and. (col1%ghost .eqv. col2%ghost)
!       else
!          eq = .false.
!       end if
    else if (.not. color_is_defined (col1) &
       .and. .not. color_is_defined (col2)) then
       eq = col1%ghost .eqv. col2%ghost
    else
       eq = .false.
    end if
  end function color_eq

  elemental function color_neq (col1, col2) result (neq)
    logical :: neq
    type(color_t), intent(in) :: col1, col2
    if (color_is_defined (col1) .and. color_is_defined (col2)) then
!       if (size (col1%c1) == size (col2%c1)) then
          neq = any (col1%c1 /= col2%c1) .or. any (col1%c2 /= col2%c2) &
               .or. (col1%ghost .neqv. col2%ghost)
!       else
!          neq = .true.
!       end if
    else if (.not. color_is_defined (col1) &
       .and. .not. color_is_defined (col2)) then
       neq = col1%ghost .neqv. col2%ghost
    else
       neq = .true.
    end if
  end function color_neq

  elemental subroutine color_add_offset (col, offset)
    type(color_t), intent(inout) :: col
    integer, intent(in) :: offset
    where (col%c1 /= 0)  col%c1 = col%c1 + sign (offset, col%c1)
    where (col%c2 /= 0)  col%c2 = col%c2 + sign (offset, col%c2)
!    if (allocated (col%c1)) then
!       col%c1 = col%c1 + sign (offset, col%c1)
!       col%c2 = col%c2 + sign (offset, col%c2)
!    end if
  end subroutine color_add_offset

  subroutine color_canonicalize (col)
    type(color_t), dimension(:), intent(inout) :: col
    integer, dimension(2*size(col)) :: map
    integer :: n_col, i, j, k
    n_col = 0
    do i = 1, size (col)
       do j = 1, size (col(i)%c1)
          if (col(i)%c1(j) /= 0) then
             k = find (abs (col(i)%c1(j)), map(:n_col))
             if (k == 0) then
                n_col = n_col + 1
                map(n_col) = abs (col(i)%c1(j))
                k = n_col
             end if
             col(i)%c1(j) = sign (k, col(i)%c1(j))
          end if
          if (col(i)%c2(j) /= 0) then
             k = find (abs (col(i)%c2(j)), map(:n_col))
             if (k == 0) then
                n_col = n_col + 1
                map(n_col) = abs (col(i)%c2(j))
                k = n_col
             end if
             col(i)%c2(j) = sign (k, col(i)%c2(j))
          end if
       end do
    end do
  contains
    function find (c, array) result (k)
      integer :: k
      integer, intent(in) :: c
      integer, dimension(:), intent(in) :: array
      integer :: i
      k = 0
      do i = 1, size (array)
         if (c == array (i)) then
            k = i
            return
         end if
      end do
    end function find
  end subroutine color_canonicalize

  subroutine extract_color_line_indices (col, c_index, col_pos)
    type(color_t), dimension(:), intent(in) :: col
    integer, dimension(:), intent(out), allocatable :: c_index
    type(color_t), dimension(size(col)), intent(out) :: col_pos
    integer, dimension(:), allocatable :: c_tmp
    integer :: i, j, k, n, c
    allocate (c_tmp (sum (color_number (col))))
    n = 0
    SCAN1: do i = 1, size (col)
       SCAN2: do j = 1, 2
          c = abs (col(i)%c1(j))
          if (c /= 0) then
             do k = 1, n
                if (c_tmp(k) == c) then
                   col_pos(i)%c1(j) = k
                   cycle SCAN2
                end if
             end do
             n = n + 1
             c_tmp(n) = c
             col_pos(i)%c1(j) = n
          end if
       end do SCAN2
    end do SCAN1
    allocate (c_index (n))
    c_index = c_tmp(1:n)
  end subroutine extract_color_line_indices

  subroutine color_array_make_contractions (col_in, col_out)
    type(color_t), dimension(:), intent(in) :: col_in
    type(color_t), dimension(:,:), intent(out), allocatable :: col_out
    type :: entry_t
       integer, dimension(:), allocatable :: map
       type(color_t), dimension(:), allocatable :: col
       type(entry_t), pointer :: next => null ()
    end type entry_t
    type :: list_t
       integer :: n = 0
       type(entry_t), pointer :: first => null ()
       type(entry_t), pointer :: last => null ()
    end type list_t
    type(list_t) :: list
    type(entry_t), pointer :: entry
    integer, dimension(:), allocatable :: c_index
    type(color_t), dimension(size(col_in)) :: col_pos
    integer :: n_prt, n_c_index
    integer, dimension(:), allocatable :: map
    integer :: i, j, c
    n_prt = size (col_in)
    call extract_color_line_indices (col_in, c_index, col_pos)
    ! print *, c_index
    n_c_index = size (c_index)
    allocate (map (n_c_index))
    map = 0
    call list_append_if_valid (list, map)
    entry => list%first
    do while (associated (entry))
       do i = 1, n_c_index
          if (entry%map(i) == 0) then
             c = c_index(i)
             do j = i + 1, n_c_index
                if (entry%map(j) == 0) then
                   map = entry%map
                   map(i) = c
                   map(j) = c
                   call list_append_if_valid (list, map)
                end if
             end do
          end if
       end do
       entry => entry%next
    end do
    call list_to_array (list, col_out)
  contains
    subroutine list_append_if_valid (list, map)
      type(list_t), intent(inout) :: list
      integer, dimension(:), intent(in) :: map
      type(entry_t), pointer :: entry
      integer :: i, j, c, p
      entry => list%first
      do while (associated (entry))
         if (all (map == entry%map))  return
         entry => entry%next
      end do
      allocate (entry)
      allocate (entry%map (n_c_index))
      entry%map = map
      allocate (entry%col (n_prt))
      do i = 1, n_prt
         do j = 1, 2
            c = col_in(i)%c1(j)
            if (c /= 0) then
               p = col_pos(i)%c1(j)
               if (map(p) /= 0) then
                  entry%col(i)%c1(j) = sign (map(p), c)
               else
                  entry%col(i)%c1(j) = c
               endif
               entry%col(i)%c2(j) = entry%col(i)%c1(j)
            end if
         end do
         if (any (entry%col(i)%c1 /= 0) .and. &
              entry%col(i)%c1(1) == - entry%col(i)%c1(2))  return
      end do
      ! call color_write (entry%col); print *, map
      if (associated (list%last)) then
         list%last%next => entry
      else
         list%first => entry
      end if
      list%last => entry
      list%n = list%n + 1
    end subroutine list_append_if_valid
    subroutine list_to_array (list, col)
      type(list_t), intent(inout) :: list
      type(color_t), dimension(:,:), intent(out), allocatable :: col
      type(entry_t), pointer :: entry
      integer :: i
      allocate (col (n_prt, list%n - 1))
      do i = 0, list%n - 1
         entry => list%first
         list%first => list%first%next
         if (i /= 0)  col(:,i) = entry%col
         deallocate (entry)
      end do
      list%last => null ()
    end subroutine list_to_array
  end subroutine color_array_make_contractions

  elemental subroutine color_invert (col)
    type(color_t), intent(inout) :: col
    col%c1 = - col%c1
    col%c2 = - col%c2
    if (col%c1(1) < 0 .and. col%c1(2) > 0) then
       col%c1 = col%c1(2:1:-1)
       col%c2 = col%c2(2:1:-1)
    end if
  end subroutine color_invert

  subroutine set_color_map (map, col1, col2)
    integer, dimension(:,:), intent(out), allocatable :: map
    type(color_t), dimension(:), intent(in) :: col1, col2
    integer, dimension(:,:), allocatable :: map1
    integer :: i, j, k
    allocate (map1 (2, 2 * sum (color_number (col1))))
    k = 0
    do i = 1, size (col1)
       do j = 1, size (col1(i)%c1)
          if (col1(i)%c1(j) /= 0 &
               .and. all (map1(1,:k) /= abs (col1(i)%c1(j)))) then
             k = k + 1
             map1(1,k) = abs (col1(i)%c1(j))
             map1(2,k) = abs (col2(i)%c1(j))
          end if
          if (col1(i)%c2(j) /= 0 &
               .and. all (map1(1,:k) /= abs (col1(i)%c2(j)))) then
             k = k + 1
             map1(1,k) = abs (col1(i)%c2(j))
             map1(2,k) = abs (col2(i)%c2(j))
          end if
       end do
    end do
    allocate (map (2, k))
    map(:,:) = map1(:,:k)
  end subroutine set_color_map

  subroutine color_translate0 (col, map)
    type(color_t), intent(inout) :: col
    integer, dimension(:,:), intent(in) :: map
    type(color_t) :: col_tmp
    integer :: i
    col_tmp = col
    do i = 1, size (map,2)
       where (abs (col%c1) == map(1,i))  
          col_tmp%c1 = sign (map(2,i), col%c1)
       end where
       where (abs (col%c2) == map(1,i))  
          col_tmp%c2 = sign (map(2,i), col%c2)
       end where
    end do
    col = col_tmp
  end subroutine color_translate0

  subroutine color_translate0_offset (col, map, offset)
    type(color_t), intent(inout) :: col
    integer, dimension(:,:), intent(in) :: map
    integer, intent(in) :: offset
    logical, dimension(size(col%c1)) :: mask1, mask2
    type(color_t) :: col_tmp
    integer :: i
    col_tmp = col
    mask1 = col%c1 /= 0
    mask2 = col%c2 /= 0
    do i = 1, size (map,2)
       where (abs (col%c1) == map(1,i))  
          col_tmp%c1 = sign (map(2,i), col%c1)
          mask1 = .false.
       end where
       where (abs (col%c2) == map(1,i))  
          col_tmp%c2 = sign (map(2,i), col%c2)
          mask2 = .false.
       end where
    end do
    col = col_tmp
    where (mask1)  col%c1 = sign (abs (col%c1) + offset, col%c1)
    where (mask2)  col%c2 = sign (abs (col%c2) + offset, col%c2)
  end subroutine color_translate0_offset

  subroutine color_translate1 (col, map, offset)
    type(color_t), dimension(:), intent(inout) :: col
    integer, dimension(:,:), intent(in) :: map
    integer, intent(in), optional :: offset
    integer :: i
    if (present (offset)) then
       do i = 1, size (col)
          call color_translate0_offset (col(i), map, offset)
       end do
    else
       do i = 1, size (col)
          call color_translate0 (col(i), map)
       end do
    end if
  end subroutine color_translate1

  elemental function merge_colors (col1, col2) result (col)
    type(color_t) :: col
    type(color_t), intent(in) :: col1, col2
    if (color_is_defined (col1) .and. color_is_defined (col2)) then
       call color_init_arrays (col, col1%c1, col2%c1)
    else if (color_is_defined (col1)) then
       col = col1
    else if (color_is_defined (col2)) then
       col = col2
    else if (color_is_ghost (col1)) then
       col = col1
    else if (color_is_ghost (col2)) then
       col = col2
    end if
  end function merge_colors

  function compute_color_factor (col1, col2, nc) result (factor)
    real(default) :: factor
    type(color_t), dimension(:), intent(in) :: col1, col2
    integer, intent(in), optional :: nc
    type(color_t), dimension(size(col1)) :: col
    integer :: ncol, nloops, nghost
    ncol = 3;  if (present (nc))  ncol = nc
    col = col1 .merge. col2
    nloops = count_color_loops (col)
    nghost = count (color_is_ghost (col))
    factor = real (ncol, default) ** (nloops - nghost)
    if (color_ghost_parity (col))  factor = - factor
  end function compute_color_factor

  function count_color_loops (col) result (count)
    integer :: count
    type(color_t), dimension(:), intent(in) :: col
    type(color_t), dimension(size(col)) :: cc
    integer :: i, n, offset
!    print *, "Count color loops:"
!    call color_write (col); print *
    cc = col
    n = size (cc)
    offset = n
    call color_add_offset (cc, offset)
!    print *, offset
!    call color_write (cc); print *
    count = 0
    SCAN_LOOPS: do
       do i = 1, n
!          print *, i, ':', cc(i)%c1
          if (color_is_defined (cc(i))) then
             if (any (cc(i)%c1 > offset)) then
!                print *, 'start', i
                count = count + 1
                call follow_line1 (pick_new_line (cc(i)%c1, count, 1))
                cycle SCAN_LOOPS
             end if
          end if
       end do
       exit SCAN_LOOPS
    end do SCAN_LOOPS
  contains
    function pick_new_line (c, reset_val, sgn) result (line)
      integer :: line
      integer, dimension(:), intent(inout) :: c
      integer, intent(in) :: reset_val
      integer, intent(in) :: sgn
      integer :: i
      if (any (c == count)) then
         line = count
      else
         do i = 1, size (c)
            if (sign (1, c(i)) == sgn .and. abs (c(i)) > offset) then
               line = c(i)
               c(i) = reset_val
               return
            end if
         end do
         call color_mismatch
      end if
    end function pick_new_line
    subroutine reset_line (c, line)
      integer, dimension(:), intent(inout) :: c
      integer, intent(in) :: line
      integer :: i
      do i = 1, size (c)
         if (c(i) == line) then
            c(i) = 0
            return
         end if
      end do
    end subroutine reset_line
    recursive subroutine follow_line1 (line)
      integer, intent(in) :: line
      integer :: i
!      print *, 'follow line 1:', line
      if (line == count) then
!         print *, 'loop closed'
         return
      end if
      do i = 1, n
         if (any (cc(i)%c1 == -line)) then
            call reset_line (cc(i)%c1, -line)
!            print *, 'found', -line, ' resetting c1:'
!            call color_write (cc); print *
            call follow_line2 (pick_new_line (cc(i)%c2, 0, sign (1, -line)))
            return
         end if
      end do
      call color_mismatch ()
    end subroutine follow_line1
    recursive subroutine follow_line2 (line)
      integer, intent(in) :: line
      integer :: i
!      print *, 'follow line 2:', line
      do i = 1, n
         if (any (cc(i)%c2 == -line)) then
            call reset_line (cc(i)%c2, -line)
!            print *, 'found', -line, ' resetting c2:'
!            call color_write (cc); print *
            call follow_line1 (pick_new_line (cc(i)%c1, 0, sign (1, -line)))
            return
         end if
      end do
      call color_mismatch ()
    end subroutine follow_line2
    subroutine color_mismatch ()
      call color_write (col)
      print *
      call msg_bug (" Color flow mismatch (color loops should be closed)")
    end subroutine color_mismatch
  end function count_color_loops

  subroutine color_test ()
    type(color_t), dimension(4) :: col1, col2, col
    type(color_t), dimension(:), allocatable :: col3
    type(color_t), dimension(:,:), allocatable :: col_array
    integer :: count, i
    call color_init_col_acl (col1, (/ 1, 0, 2, 3 /), (/ 0, 1, 3, 2 /))
    col2 = col1
    call color_write (col1); print *
    call color_write (col2); print *
    col = col1 .merge. col2
    call color_write (col); print *
    count = count_color_loops (col)
    print *, "Number of color loops (3): ", count
    call color_init_col_acl (col2, (/ 1, 0, 2, 3 /), (/ 0, 2, 3, 1 /))
    call color_write (col1); print *
    call color_write (col2); print *
    col = col1 .merge. col2
    call color_write (col); print *
    count = count_color_loops (col)
    print *, "Number of color loops (2): ", count
    print *
    allocate (col3 (4))
    call color_init_from_array (col3, &
         reshape ((/ 1, 0,   0, -1,  2, -3,  3, -2 /), & 
                  (/ 2, 4 /)))
    call color_write (col3); print *
    call color_array_make_contractions (col3, col_array)
    print *, "Contractions:"
    do i = 1, size (col_array, 2)
       call color_write (col_array(:,i)); print *
    end do
    deallocate (col3)
    print *
    allocate (col3 (6))
    call color_init_from_array (col3, &
         reshape ((/ 1, -2,   3, 0,  0, -1,  2, -4,  -3, 0,  4, 0 /), & 
                  (/ 2, 6 /)))
    call color_write (col3); print *
    call color_array_make_contractions (col3, col_array)
    print *, "Contractions:"
    do i = 1, size (col_array, 2)
       call color_write (col_array(:,i)); print *
    end do
  end subroutine color_test


end module colors
