! WHIZARD 2.0.1 Sun Apr 25 2010
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

module prt_lists

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use lorentz !NODEP!
  use sorting
  use pdg_arrays

  implicit none
  private

  public :: prt_t
  public :: prt_init_combine
  public :: prt_get_pdg
  public :: prt_get_momentum
  public :: prt_get_msq
  public :: prt_is_polarized
  public :: prt_get_helicity
  public :: prt_write
  public :: are_disjoint
  public :: prt_list_t
  public :: prt_list_init
  public :: prt_list_reset
  public :: prt_list_write
  public :: prt_list_set_incoming
  public :: prt_list_set_outgoing
  public :: prt_list_set_composite
  public :: prt_list_set_p_incoming
  public :: prt_list_set_p_outgoing
  public :: prt_list_polarize
  public :: prt_list_is_nonempty
  public :: prt_list_get_length
  public :: prt_list_get_prt
  public :: prt_list_join
  public :: prt_list_combine
  public :: prt_list_collect
  public :: prt_list_select
  public :: prt_list_extract
  public :: prt_list_sort
  public :: prt_list_select_pdg_code

  integer, parameter, public :: PRT_UNDEFINED = 0 
  integer, parameter, public :: PRT_BEAM = -9
  integer, parameter, public :: PRT_INCOMING = 1
  integer, parameter, public :: PRT_OUTGOING = 2
  integer, parameter, public :: PRT_COMPOSITE = 3
  integer, parameter, public :: PRT_VIRTUAL = 4
  integer, parameter, public :: PRT_RESONANT = 5
  integer, parameter, public :: PRT_BEAM_REMNANT = 9


  type :: prt_t
     private
     integer :: type = PRT_UNDEFINED
     integer :: pdg
     logical :: polarized = .false.
     integer :: h
     type(vector4_t) :: p
     real(default) :: p2
     integer, dimension(:), allocatable :: src
  end type prt_t

  type :: prt_list_t
     private
     integer :: n = 0
     type(prt_t), dimension(:), allocatable :: prt
  end type prt_list_t


  interface operator(.match.)
     module procedure prt_match
  end interface

  interface assignment(=)
     module procedure prt_list_assign
  end interface

  interface prt_list_sort
     module procedure prt_list_sort_pdg
     module procedure prt_list_sort_int
     module procedure prt_list_sort_real
  end interface


contains

  subroutine prt_init_incoming (prt, pdg, p, p2, src)
    type(prt_t), intent(out) :: prt
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in) :: src
    prt%type = PRT_INCOMING
    call prt_set (prt, pdg, - p, p2, src)
  end subroutine prt_init_incoming
    
  subroutine prt_init_outgoing (prt, pdg, p, p2, src)
    type(prt_t), intent(out) :: prt
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in) :: src
    prt%type = PRT_OUTGOING
    call prt_set (prt, pdg, p, p2, src)
  end subroutine prt_init_outgoing
    
  subroutine prt_init_composite (prt, p, src)
    type(prt_t), intent(out) :: prt
    type(vector4_t), intent(in) :: p
    integer, dimension(:), intent(in) :: src
    prt%type = PRT_COMPOSITE
    call prt_set (prt, 0, p, p**2, src)
  end subroutine prt_init_composite

  subroutine prt_init_combine (prt, prt1, prt2)
    type(prt_t), intent(out) :: prt
    type(prt_t), intent(in) :: prt1, prt2
    type(vector4_t) :: p
    integer, dimension(0) :: src
    prt%type = PRT_COMPOSITE
    p = prt1%p + prt2%p
    call prt_set (prt, 0, p, p**2, src)
  end subroutine prt_init_combine

  elemental function prt_get_pdg (prt) result (pdg)
    integer :: pdg
    type(prt_t), intent(in) :: prt
    pdg = prt%pdg
  end function prt_get_pdg

  elemental function prt_get_momentum (prt) result (p)
    type(vector4_t) :: p
    type(prt_t), intent(in) :: prt
    p = prt%p
  end function prt_get_momentum

  elemental function prt_get_msq (prt) result (msq)
    real(default) :: msq
    type(prt_t), intent(in) :: prt
    msq = prt%p2
  end function prt_get_msq

  elemental function prt_is_polarized (prt) result (flag)
    logical :: flag
    type(prt_t), intent(in) :: prt
    flag = prt%polarized
  end function prt_is_polarized

  elemental function prt_get_helicity (prt) result (h)
    integer :: h
    type(prt_t), intent(in) :: prt
    h = prt%h
  end function prt_get_helicity

  subroutine prt_set (prt, pdg, p, p2, src)
    type(prt_t), intent(inout) :: prt
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in) :: src
    prt%pdg = pdg
    prt%p = p
    prt%p2 = p2
    if (allocated (prt%src)) then
       if (size (src) /= size (prt%src)) then
          deallocate (prt%src)
          allocate (prt%src (size (src)))
       end if
    else
       allocate (prt%src (size (src)))
    end if
    prt%src = src
  end subroutine prt_set

  elemental subroutine prt_set_p (prt, p)
    type(prt_t), intent(inout) :: prt
    type(vector4_t), intent(in) :: p
    prt%p = p
  end subroutine prt_set_p

  subroutine prt_polarize (prt, h)
    type(prt_t), intent(inout) :: prt
    integer, intent(in) :: h
    prt%polarized = .true.
    prt%h = h
  end subroutine prt_polarize

  subroutine prt_write (prt, unit)
    type(prt_t), intent(in) :: prt
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)", advance="no")  "prt("
    select case (prt%type)
    case (PRT_UNDEFINED);    write (u, "('?')", advance="no")
    case (PRT_INCOMING);     write (u, "('i:')", advance="no")
    case (PRT_OUTGOING);     write (u, "('o:')", advance="no")
    case (PRT_COMPOSITE);    write (u, "('c:')", advance="no")
    end select
    select case (prt%type)
    case (PRT_INCOMING, PRT_OUTGOING)
       if (prt%polarized) then
          write (u, "(I0,'/',I0,'|')", advance="no")  prt%pdg, prt%h
       else
          write (u, "(I0,'|')", advance="no") prt%pdg
       end if
    end select
    select case (prt%type)
    case (PRT_INCOMING, PRT_OUTGOING, PRT_COMPOSITE)
       write (u, "(1PE12.5,';',1PE12.5,',',1PE12.5,',',1PE12.5)", advance="no") &
            array_from_vector4 (prt%p)
       write (u, "('|',1PE12.5)", advance="no")  prt%p2
    end select
    if (allocated (prt%src)) then
       write (u, "('|')", advance="no")
       do i = 1, size (prt%src)
          write (u, "(1x,I0)", advance="no")  prt%src(i)
       end do
    end if
    write (u, "(A)")  ")"
  end subroutine prt_write

  elemental function prt_match (prt1, prt2) result (match)
    logical :: match
    type(prt_t), intent(in) :: prt1, prt2
    if (size (prt1%src) == size (prt2%src)) then
       match = all (prt1%src == prt2%src)
    else
       match = .false.
    end if
  end function prt_match

  subroutine prt_combine (prt, prt_in1, prt_in2, ok)
    type(prt_t), intent(inout) :: prt
    type(prt_t), intent(in) :: prt_in1, prt_in2
    logical :: ok
    integer, dimension(:), allocatable :: src
    call combine_index_lists (src, prt_in1%src, prt_in2%src)
    ok = allocated (src)
    if (ok)  call prt_init_composite (prt, prt_in1%p + prt_in2%p, src)
  end subroutine prt_combine

  function are_disjoint (prt_in1, prt_in2) result (flag)
    logical :: flag
    type(prt_t), intent(in) :: prt_in1, prt_in2
    flag = index_lists_are_disjoint (prt_in1%src, prt_in2%src)
  end function are_disjoint

  subroutine combine_index_lists (res, src1, src2)
    integer, dimension(:), intent(in) :: src1, src2
    integer, dimension(:), allocatable :: res
    integer :: i1, i2, i
    allocate (res (size (src1) + size (src2)))
    i1 = 1
    i2 = 1
    LOOP: do i = 1, size (res)
       if (src1(i1) < src2(i2)) then
          res(i) = src1(i1);  i1 = i1 + 1
          if (i1 > size (src1)) then
             res(i+1:) = src2(i2:)
             exit LOOP
          end if
       else if (src1(i1) > src2(i2)) then
          res(i) = src2(i2);  i2 = i2 + 1
          if (i2 > size (src2)) then
             res(i+1:) = src1(i1:)
             exit LOOP
          end if
       else
          deallocate (res)
          exit LOOP
       end if
    end do LOOP
  end subroutine combine_index_lists

  function index_lists_are_disjoint (src1, src2) result (flag)
    logical :: flag
    integer, dimension(:), intent(in) :: src1, src2
    integer :: i1, i2, i
    flag = .true.
    i1 = 1
    i2 = 1
    LOOP: do i = 1, size (src1) + size (src2)
       if (src1(i1) < src2(i2)) then
          i1 = i1 + 1
          if (i1 > size (src1)) then
             exit LOOP
          end if
       else if (src1(i1) > src2(i2)) then
          i2 = i2 + 1
          if (i2 > size (src2)) then
             exit LOOP
          end if
       else
          flag = .false.
          exit LOOP
       end if
    end do LOOP
  end function index_lists_are_disjoint

  subroutine prt_list_init (prt_list, n)
    type(prt_list_t), intent(out) :: prt_list
    integer, intent(in), optional :: n
    if (present (n)) then
       allocate (prt_list%prt (n))
       prt_list%n = n
    else
       allocate (prt_list%prt (0))
    end if
  end subroutine prt_list_init

  subroutine prt_list_reset (prt_list, n)
    type(prt_list_t), intent(inout) :: prt_list
    integer, intent(in) :: n
    if (n > size (prt_list%prt)) then
       deallocate (prt_list%prt)
       allocate (prt_list%prt (n))
    end if
    prt_list%n = n
  end subroutine prt_list_reset

  subroutine prt_list_write (prt_list, unit, prefix)
    type(prt_list_t), intent(in) :: prt_list
    integer, intent(in), optional :: unit
    character(*), intent(in), optional :: prefix
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Particle list:"
    do i = 1, prt_list%n
       if (present (prefix))  write (u, "(A)", advance="no") prefix
       write (u, "(1x,I0)", advance="no")  i
       call prt_write (prt_list%prt(i), unit)
    end do
  end subroutine prt_list_write

  subroutine prt_list_assign (prt_list, prt_list_in)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: prt_list_in
    integer :: i
    if (.not. allocated (prt_list%prt))  call prt_list_init (prt_list)
    call prt_list_reset (prt_list, prt_list_in%n)
    do i = 1, prt_list%n
       prt_list%prt(i) = prt_list_in%prt(i)
    end do
  end subroutine prt_list_assign

  subroutine prt_list_set_incoming (prt_list, i, pdg, p, p2, src)
    type(prt_list_t), intent(inout) :: prt_list
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in), optional :: src
    if (present (src)) then
       call prt_init_incoming (prt_list%prt(i), pdg, p, p2, src)
    else
       call prt_init_incoming (prt_list%prt(i), pdg, p, p2, (/ i /))
    end if
  end subroutine prt_list_set_incoming

  subroutine prt_list_set_outgoing (prt_list, i, pdg, p, p2, src)
    type(prt_list_t), intent(inout) :: prt_list
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in), optional :: src
    if (present (src)) then
       call prt_init_outgoing (prt_list%prt(i), pdg, p, p2, src)
    else
       call prt_init_outgoing (prt_list%prt(i), pdg, p, p2, (/ i /))
    end if
  end subroutine prt_list_set_outgoing

  subroutine prt_list_set_composite (prt_list, i, p, src)
    type(prt_list_t), intent(inout) :: prt_list
    integer, intent(in) :: i
    type(vector4_t), intent(in) :: p
    integer, dimension(:), intent(in) :: src
    call prt_init_composite (prt_list%prt(i), p, src)
  end subroutine prt_list_set_composite

  subroutine prt_list_set_p_incoming (prt_list, p)
    type(prt_list_t), intent(inout) :: prt_list
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: n_in
    n_in = size (p)
    call prt_set_p (prt_list%prt(:n_in), p)
  end subroutine prt_list_set_p_incoming

  subroutine prt_list_set_p_outgoing (prt_list, p)
    type(prt_list_t), intent(inout) :: prt_list
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: n_in, n_out
    n_out = size (p)
    n_in = prt_list%n - n_out
    call prt_set_p (prt_list%prt(n_in+1:), p)
  end subroutine prt_list_set_p_outgoing


  subroutine prt_list_polarize (prt_list, i, h)
    type(prt_list_t), intent(inout) :: prt_list
    integer, intent(in) :: i, h
    call prt_polarize (prt_list%prt(i), h)
  end subroutine prt_list_polarize

  function prt_list_is_nonempty (prt_list) result (flag)
    logical :: flag
    type(prt_list_t), intent(in) :: prt_list
    flag = prt_list%n /= 0
  end function prt_list_is_nonempty

  function prt_list_get_length (prt_list) result (length)
    integer :: length
    type(prt_list_t), intent(in) :: prt_list
    length = prt_list%n
  end function prt_list_get_length

  function prt_list_get_prt (prt_list, i) result (prt)
    type(prt_t) :: prt
    type(prt_list_t), intent(in) :: prt_list
    integer, intent(in) :: i
    prt = prt_list%prt(i)
  end function prt_list_get_prt

  subroutine prt_list_join (prt_list, pl1, pl2, mask2)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl1, pl2
    logical, dimension(:), intent(in), optional :: mask2
    integer :: n1, n2, i, n
    n1 = pl1%n
    n2 = pl2%n
    call prt_list_reset (prt_list, n1 + n2)
    prt_list%prt(:n1)   = pl1%prt(:n1)
    n = n1
    if (present (mask2)) then
       do i = 1, pl2%n
          if (mask2(i) &
               .and. .not. any (pl2%prt(i) .match. pl1%prt(:pl1%n))) then
             n = n + 1
             prt_list%prt(n) = pl2%prt(i)
          end if
       end do
    else
       do i = 1, pl2%n
          if (any (pl2%prt(i) .match. pl1%prt(:pl1%n))) then
             n = n + 1
             prt_list%prt(n) = pl2%prt(i)
          end if
       end do
    end if
    prt_list%n = n
  end subroutine prt_list_join

  subroutine prt_list_combine (prt_list, pl1, pl2, mask12)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl1, pl2
    logical, dimension(:,:), intent(in), optional :: mask12
    integer :: n1, n2, i1, i2, n, j
    logical :: ok
    n1 = pl1%n
    n2 = pl2%n
    call prt_list_reset (prt_list, n1 * n2)
    n = 1
    do i1 = 1, n1
       do i2 = 1, n2
          if (present (mask12)) then
             ok = mask12(i1,i2)
          else
             ok = .true.
          end if
          if (ok)  call prt_combine &
               (prt_list%prt(n), pl1%prt(i1), pl2%prt(i2), ok)
          if (ok) then
             CHECK_DOUBLES: do j = 1, n - 1
                if (prt_list%prt(n) .match. prt_list%prt(j)) then
                   ok = .false.;  exit CHECK_DOUBLES
                end if
             end do CHECK_DOUBLES
             if (ok)  n = n + 1
          end if
       end do
    end do
    prt_list%n = n - 1
  end subroutine prt_list_combine

  subroutine prt_list_collect (prt_list, pl1, mask1)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl1
    logical, dimension(:), intent(in) :: mask1
    type(prt_t) :: prt
    integer :: i
    logical :: ok
    call prt_list_reset (prt_list, 1)
    prt_list%n = 0
    do i = 1, pl1%n
       if (mask1(i)) then
          if (prt_list%n == 0) then
             prt_list%n = 1
             prt_list%prt(1) = pl1%prt(i)
          else
             call prt_combine (prt, prt_list%prt(1), pl1%prt(i), ok)
             if (ok)  prt_list%prt(1) = prt
          end if
       end if
    end do
  end subroutine prt_list_collect

  subroutine prt_list_select (prt_list, pl, mask1)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl
    logical, dimension(:), intent(in) :: mask1
    integer :: i, n
    call prt_list_reset (prt_list, pl%n)
    n = 0
    do i = 1, pl%n
       if (mask1(i)) then
          n = n + 1
          prt_list%prt(n) = pl%prt(i)
       end if
    end do
    prt_list%n = n
  end subroutine prt_list_select

  subroutine prt_list_extract (prt_list, pl, index)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl
    integer, intent(in) :: index
    if (index > 0) then
       if (index <= pl%n) then
          call prt_list_reset (prt_list, 1)
          prt_list%prt(1) = pl%prt(index)
       else
          call prt_list_reset (prt_list, 0)
       end if
    else if (index < 0) then
       if (abs (index) <= pl%n) then
          call prt_list_reset (prt_list, 1)
          prt_list%prt(1) = pl%prt(pl%n + 1 + index)
       else
          call prt_list_reset (prt_list, 0)
       end if
    else
       call prt_list_reset (prt_list, 0)
    end if
  end subroutine prt_list_extract

  subroutine prt_list_sort_pdg (prt_list, pl)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl
    integer :: n
    n = prt_list%n
    call prt_list_sort_int (prt_list, pl, abs (3 * prt_list%prt(:n)%pdg - 1))
  end subroutine prt_list_sort_pdg

  subroutine prt_list_sort_int (prt_list, pl, ival)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl
    integer, dimension(:), intent(in) :: ival
    call prt_list_reset (prt_list, pl%n)
    prt_list%n = pl%n
    prt_list%prt = pl%prt( order (ival) )
  end subroutine prt_list_sort_int

  subroutine prt_list_sort_real (prt_list, pl, rval)
    type(prt_list_t), intent(inout) :: prt_list
    type(prt_list_t), intent(in) :: pl
    real(default), dimension(:), intent(in) :: rval
    call prt_list_reset (prt_list, pl%n)
    prt_list%n = pl%n
    prt_list%prt = pl%prt( order (rval) )
  end subroutine prt_list_sort_real

  subroutine prt_list_select_pdg_code (prt_list, aval, prt_list_in, prt_type)
    type(prt_list_t), intent(inout) :: prt_list
    type(pdg_array_t), intent(in) :: aval
    type(prt_list_t), intent(in) :: prt_list_in
    integer, intent(in), optional :: prt_type
    integer :: n_tot, n_match
    logical, dimension(:), allocatable :: mask
    integer :: i, j
    n_tot = prt_list_in%n
    allocate (mask (n_tot))
    forall (i = 1:n_tot) &
         mask(i) = aval .match. prt_list_in%prt(i)%pdg
    if (present (prt_type)) &
         mask = mask .and. prt_list_in%prt(:n_tot)%type == prt_type
    n_match = count (mask)
    call prt_list_reset (prt_list, n_match)
!     prt_list%prt(:n_match) = pack (prt_list_in%prt(:n_tot), mask)
    j = 0
    do i = 1, n_tot
       if (mask(i)) then
          j = j + 1
          prt_list%prt(j) = prt_list_in%prt(i)
       end if
    end do
  end subroutine prt_list_select_pdg_code


end module prt_lists
