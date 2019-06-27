! WHIZARD 2.0.2 Tue May 18 2010
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

module pdg_arrays

  use file_utils !NODEP!

  implicit none
  private

  public :: pdg_array_t
  public :: pdg_array_write
  public :: assignment(=)
  public :: operator(//)
  public :: operator(.match.)
  public :: operator(.eqv.)
  public :: operator(.neqv.)

  integer, parameter, public :: UNDEFINED = 0

  type :: pdg_array_t
     private
     integer, dimension(:), allocatable :: pdg
  end type pdg_array_t


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

  interface operator(.eqv.)
     module procedure pdg_array_equivalent
  end interface
  interface operator(.neqv.)
     module procedure pdg_array_inequivalent
  end interface


contains

  subroutine pdg_array_write (aval, unit)
    type(pdg_array_t), intent(in) :: aval
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

  function pdg_array_equivalent (aval1, aval2) result (eq)
    logical :: eq
    type(pdg_array_t), intent(in) :: aval1, aval2
    logical, dimension(:), allocatable :: match1, match2
    integer :: i
    if (allocated (aval1%pdg) .and. allocated (aval2%pdg)) then
       eq = any (aval1%pdg == UNDEFINED) &
            .and. any (aval1%pdg == UNDEFINED)
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

  function pdg_array_inequivalent (aval1, aval2) result (neq)
    logical :: neq
    type(pdg_array_t), intent(in) :: aval1, aval2
    neq = .not. pdg_array_equivalent (aval1, aval2)
  end function pdg_array_inequivalent


end module pdg_arrays
