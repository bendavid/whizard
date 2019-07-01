! WHIZARD 2.0.5 Tue May 10 2011
! 
! Copyright (C) 1999-2011 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module subevents

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
  public :: subevt_t
  public :: subevt_init
  public :: subevt_reset
  public :: subevt_write
  public :: subevt_set_beam
  public :: subevt_set_incoming
  public :: subevt_set_outgoing
  public :: subevt_set_composite
  public :: subevt_set_pdg_beam
  public :: subevt_set_pdg_incoming
  public :: subevt_set_pdg_outgoing
  public :: subevt_set_p_beam
  public :: subevt_set_p_incoming
  public :: subevt_set_p_outgoing
  public :: subevt_polarize
  public :: subevt_is_nonempty
  public :: subevt_get_length
  public :: subevt_get_prt
  public :: subevt_join
  public :: subevt_combine
  public :: subevt_collect
  public :: subevt_select
  public :: subevt_extract
  public :: subevt_sort
  public :: subevt_select_pdg_code

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

  type :: subevt_t
     private
     integer :: n_tot = 0
     type(prt_t), dimension(:), allocatable :: prt
  end type subevt_t


  interface operator(.match.)
     module procedure prt_match
  end interface

  interface assignment(=)
     module procedure subevt_assign
  end interface

  interface subevt_sort
     module procedure subevt_sort_pdg
     module procedure subevt_sort_int
     module procedure subevt_sort_real
  end interface


contains

  subroutine prt_init_beam (prt, pdg, p, p2, src)
    type(prt_t), intent(out) :: prt
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in) :: src
    prt%type = PRT_BEAM
    call prt_set (prt, pdg, - p, p2, src)
  end subroutine prt_init_beam
    
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

  elemental subroutine prt_set_pdg (prt, pdg)
    type(prt_t), intent(inout) :: prt
    integer, intent(in) :: pdg
    prt%pdg = pdg
  end subroutine prt_set_pdg

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
    case (PRT_BEAM);         write (u, "('b:')", advance="no")
    case (PRT_INCOMING);     write (u, "('i:')", advance="no")
    case (PRT_OUTGOING);     write (u, "('o:')", advance="no")
    case (PRT_COMPOSITE);    write (u, "('c:')", advance="no")
    end select
    select case (prt%type)
    case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING)
       if (prt%polarized) then
          write (u, "(I0,'/',I0,'|')", advance="no")  prt%pdg, prt%h
       else
          write (u, "(I0,'|')", advance="no") prt%pdg
       end if
    end select
    select case (prt%type)
    case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING, PRT_COMPOSITE)
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

  subroutine subevt_init (subevt, n_tot)
    type(subevt_t), intent(out) :: subevt
    integer, intent(in), optional :: n_tot
    if (present (n_tot))  subevt%n_tot = n_tot
    allocate (subevt%prt (subevt%n_tot))
  end subroutine subevt_init

  subroutine subevt_reset (subevt, n_tot)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: n_tot
    subevt%n_tot = n_tot
    if (subevt%n_tot > size (subevt%prt)) then
       deallocate (subevt%prt)
       allocate (subevt%prt (subevt%n_tot))
    end if
  end subroutine subevt_reset

  subroutine subevt_write (subevt, unit, prefix)
    type(subevt_t), intent(in) :: subevt
    integer, intent(in), optional :: unit
    character(*), intent(in), optional :: prefix
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "subevent:"
    do i = 1, subevt%n_tot
       if (present (prefix))  write (u, "(A)", advance="no") prefix
       write (u, "(1x,I0)", advance="no")  i
       call prt_write (subevt%prt(i), unit)
    end do
  end subroutine subevt_write

  subroutine subevt_assign (subevt, subevt_in)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: subevt_in
    if (.not. allocated (subevt%prt)) then
       call subevt_init (subevt, subevt_in%n_tot)
    else
       call subevt_reset (subevt, subevt_in%n_tot)
    end if
    subevt%prt(:subevt%n_tot) = subevt_in%prt(:subevt%n_tot)
  end subroutine subevt_assign

  subroutine subevt_set_beam (subevt, i, pdg, p, p2, src)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in), optional :: src
    if (present (src)) then
       call prt_init_beam (subevt%prt(i), pdg, p, p2, src)
    else
       call prt_init_beam (subevt%prt(i), pdg, p, p2, (/ i /))
    end if
  end subroutine subevt_set_beam

  subroutine subevt_set_incoming (subevt, i, pdg, p, p2, src)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in), optional :: src
    if (present (src)) then
       call prt_init_incoming (subevt%prt(i), pdg, p, p2, src)
    else
       call prt_init_incoming (subevt%prt(i), pdg, p, p2, (/ i /))
    end if
  end subroutine subevt_set_incoming

  subroutine subevt_set_outgoing (subevt, i, pdg, p, p2, src)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: i
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: p2
    integer, dimension(:), intent(in), optional :: src
    if (present (src)) then
       call prt_init_outgoing (subevt%prt(i), pdg, p, p2, src)
    else
       call prt_init_outgoing (subevt%prt(i), pdg, p, p2, (/ i /))
    end if
  end subroutine subevt_set_outgoing

  subroutine subevt_set_composite (subevt, i, p, src)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: i
    type(vector4_t), intent(in) :: p
    integer, dimension(:), intent(in) :: src
    call prt_init_composite (subevt%prt(i), p, src)
  end subroutine subevt_set_composite

  subroutine subevt_set_pdg_beam (subevt, pdg)
    type(subevt_t), intent(inout) :: subevt
    integer, dimension(:), intent(in) :: pdg
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_BEAM) then
          call prt_set_pdg (subevt%prt(i), pdg(j))
          j = j + 1
          if (j > size (pdg))  exit
       end if
    end do
  end subroutine subevt_set_pdg_beam

  subroutine subevt_set_pdg_incoming (subevt, pdg)
    type(subevt_t), intent(inout) :: subevt
    integer, dimension(:), intent(in) :: pdg
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_INCOMING) then
          call prt_set_pdg (subevt%prt(i), pdg(j))
          j = j + 1
          if (j > size (pdg))  exit
       end if
    end do
  end subroutine subevt_set_pdg_incoming

  subroutine subevt_set_pdg_outgoing (subevt, pdg)
    type(subevt_t), intent(inout) :: subevt
    integer, dimension(:), intent(in) :: pdg
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_OUTGOING) then
          call prt_set_pdg (subevt%prt(i), pdg(j))
          j = j + 1
          if (j > size (pdg))  exit
       end if
    end do
  end subroutine subevt_set_pdg_outgoing

  subroutine subevt_set_p_beam (subevt, p)
    type(subevt_t), intent(inout) :: subevt
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_BEAM) then
          call prt_set_p (subevt%prt(i), p(j))
          j = j + 1
          if (j > size (p))  exit
       end if
    end do
  end subroutine subevt_set_p_beam

  subroutine subevt_set_p_incoming (subevt, p)
    type(subevt_t), intent(inout) :: subevt
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_INCOMING) then
          call prt_set_p (subevt%prt(i), p(j))
          j = j + 1
          if (j > size (p))  exit
       end if
    end do
  end subroutine subevt_set_p_incoming

  subroutine subevt_set_p_outgoing (subevt, p)
    type(subevt_t), intent(inout) :: subevt
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: i, j
    j = 1
    do i = 1, subevt%n_tot
       if (subevt%prt(i)%type == PRT_OUTGOING) then
          call prt_set_p (subevt%prt(i), p(j))
          j = j + 1
          if (j > size (p))  exit
       end if
    end do
  end subroutine subevt_set_p_outgoing

  subroutine subevt_polarize (subevt, i, h)
    type(subevt_t), intent(inout) :: subevt
    integer, intent(in) :: i, h
    call prt_polarize (subevt%prt(i), h)
  end subroutine subevt_polarize

  function subevt_is_nonempty (subevt) result (flag)
    logical :: flag
    type(subevt_t), intent(in) :: subevt
    flag = subevt%n_tot /= 0
  end function subevt_is_nonempty

  function subevt_get_length (subevt) result (length)
    integer :: length
    type(subevt_t), intent(in) :: subevt
    length = subevt%n_tot
  end function subevt_get_length

  function subevt_get_prt (subevt, i) result (prt)
    type(prt_t) :: prt
    type(subevt_t), intent(in) :: subevt
    integer, intent(in) :: i
    prt = subevt%prt(i)
  end function subevt_get_prt

  subroutine subevt_join (subevt, pl1, pl2, mask2)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl1, pl2
    logical, dimension(:), intent(in), optional :: mask2
    integer :: n1, n2, i, n
    n1 = pl1%n_tot
    n2 = pl2%n_tot
    call subevt_reset (subevt, n1 + n2)
    subevt%prt(:n1)   = pl1%prt(:n1)
    n = n1
    if (present (mask2)) then
       do i = 1, pl2%n_tot
          if (mask2(i) &
               .and. .not. any (pl2%prt(i) .match. pl1%prt(:pl1%n_tot))) then
             n = n + 1
             subevt%prt(n) = pl2%prt(i)
          end if
       end do
    else
       do i = 1, pl2%n_tot
          if (any (pl2%prt(i) .match. pl1%prt(:pl1%n_tot))) then
             n = n + 1
             subevt%prt(n) = pl2%prt(i)
          end if
       end do
    end if
    subevt%n_tot = n
  end subroutine subevt_join

  subroutine subevt_combine (subevt, pl1, pl2, mask12)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl1, pl2
    logical, dimension(:,:), intent(in), optional :: mask12
    integer :: n1, n2, i1, i2, n, j
    logical :: ok
    n1 = pl1%n_tot
    n2 = pl2%n_tot
    call subevt_reset (subevt, n1 * n2)
    n = 1
    do i1 = 1, n1
       do i2 = 1, n2
          if (present (mask12)) then
             ok = mask12(i1,i2)
          else
             ok = .true.
          end if
          if (ok)  call prt_combine &
               (subevt%prt(n), pl1%prt(i1), pl2%prt(i2), ok)
          if (ok) then
             CHECK_DOUBLES: do j = 1, n - 1
                if (subevt%prt(n) .match. subevt%prt(j)) then
                   ok = .false.;  exit CHECK_DOUBLES
                end if
             end do CHECK_DOUBLES
             if (ok)  n = n + 1
          end if
       end do
    end do
    subevt%n_tot = n - 1
  end subroutine subevt_combine

  subroutine subevt_collect (subevt, pl1, mask1)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl1
    logical, dimension(:), intent(in) :: mask1
    type(prt_t) :: prt
    integer :: i
    logical :: ok
    call subevt_reset (subevt, 1)
    subevt%n_tot = 0
    do i = 1, pl1%n_tot
       if (mask1(i)) then
          if (subevt%n_tot == 0) then
             subevt%n_tot = 1
             subevt%prt(1) = pl1%prt(i)
          else
             call prt_combine (prt, subevt%prt(1), pl1%prt(i), ok)
             if (ok)  subevt%prt(1) = prt
          end if
       end if
    end do
  end subroutine subevt_collect

  subroutine subevt_select (subevt, pl, mask1)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl
    logical, dimension(:), intent(in) :: mask1
    integer :: i, n
    call subevt_reset (subevt, pl%n_tot)
    n = 0
    do i = 1, pl%n_tot
       if (mask1(i)) then
          n = n + 1
          subevt%prt(n) = pl%prt(i)
       end if
    end do
    subevt%n_tot = n
  end subroutine subevt_select

  subroutine subevt_extract (subevt, pl, index)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl
    integer, intent(in) :: index
    if (index > 0) then
       if (index <= pl%n_tot) then
          call subevt_reset (subevt, 1)
          subevt%prt(1) = pl%prt(index)
       else
          call subevt_reset (subevt, 0)
       end if
    else if (index < 0) then
       if (abs (index) <= pl%n_tot) then
          call subevt_reset (subevt, 1)
          subevt%prt(1) = pl%prt(pl%n_tot + 1 + index)
       else
          call subevt_reset (subevt, 0)
       end if
    else
       call subevt_reset (subevt, 0)
    end if
  end subroutine subevt_extract

  subroutine subevt_sort_pdg (subevt, pl)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl
    integer :: n
    n = subevt%n_tot
    call subevt_sort_int (subevt, pl, abs (3 * subevt%prt(:n)%pdg - 1))
  end subroutine subevt_sort_pdg

  subroutine subevt_sort_int (subevt, pl, ival)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl
    integer, dimension(:), intent(in) :: ival
    call subevt_reset (subevt, pl%n_tot)
    subevt%n_tot = pl%n_tot
    subevt%prt = pl%prt( order (ival) )
  end subroutine subevt_sort_int

  subroutine subevt_sort_real (subevt, pl, rval)
    type(subevt_t), intent(inout) :: subevt
    type(subevt_t), intent(in) :: pl
    real(default), dimension(:), intent(in) :: rval
    call subevt_reset (subevt, pl%n_tot)
    subevt%n_tot = pl%n_tot
    subevt%prt = pl%prt( order (rval) )
  end subroutine subevt_sort_real

  subroutine subevt_select_pdg_code (subevt, aval, subevt_in, prt_type)
    type(subevt_t), intent(inout) :: subevt
    type(pdg_array_t), intent(in) :: aval
    type(subevt_t), intent(in) :: subevt_in
    integer, intent(in), optional :: prt_type
    integer :: n_tot, n_match
    logical, dimension(:), allocatable :: mask
    integer :: i, j
    n_tot = subevt_in%n_tot
    allocate (mask (n_tot))
    forall (i = 1:n_tot) &
         mask(i) = aval .match. subevt_in%prt(i)%pdg
    if (present (prt_type)) &
         mask = mask .and. subevt_in%prt(:n_tot)%type == prt_type
    n_match = count (mask)
    call subevt_reset (subevt, n_match)
!     subevt%prt(:n_match) = pack (subevt_in%prt(:n_tot), mask)
    j = 0
    do i = 1, n_tot
       if (mask(i)) then
          j = j + 1
          subevt%prt(j) = subevt_in%prt(i)
       end if
    end do
  end subroutine subevt_select_pdg_code


end module subevents
