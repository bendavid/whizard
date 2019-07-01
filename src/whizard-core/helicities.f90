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

module helicities

  use file_utils !NODEP!

  implicit none
  private

  public :: helicity_t
  public :: helicity_init
  public :: helicity_set_ghost
  public :: helicity_undefine
  public :: helicity_diagonalize
  public :: helicity_write
  public :: helicity_write_raw
  public :: helicity_read_raw
  public :: helicity_is_defined
  public :: helicity_is_diagonal
  public :: helicity_is_ghost
  public :: helicity_get
  public :: operator(.match.)
  public :: operator(.dmatch.)
  public :: operator(==)
  public :: operator(/=)
  public :: operator(.merge.)

  type :: helicity_t
     private
     logical :: defined = .false.
     integer :: h1, h2
     logical :: ghost = .false.
  end type helicity_t


  interface helicity_init
     module procedure helicity_init0, helicity_init0g
     module procedure helicity_init1, helicity_init1g
     module procedure helicity_init2, helicity_init2g
  end interface
  interface operator(.match.)
     module procedure helicity_match
  end interface
  interface operator(.dmatch.)
     module procedure helicity_match_diagonal
  end interface
  interface operator(==)
     module procedure helicity_eq
  end interface
  interface operator(/=)
     module procedure helicity_neq
  end interface
  interface operator(.merge.)
     module procedure merge_helicities
  end interface

contains

  elemental subroutine helicity_init0 (hel)
    type(helicity_t), intent(out) :: hel
  end subroutine helicity_init0

  elemental subroutine helicity_init0g (hel, ghost)
    type(helicity_t), intent(out) :: hel
    logical, intent(in) :: ghost
    hel%ghost = ghost
  end subroutine helicity_init0g

  elemental subroutine helicity_init1 (hel, h)
    type(helicity_t), intent(out) :: hel
    integer, intent(in) :: h
    hel%defined = .true.
    hel%h1 = h
    hel%h2 = h
  end subroutine helicity_init1

  elemental subroutine helicity_init1g (hel, h, ghost)
    type(helicity_t), intent(out) :: hel
    integer, intent(in) :: h
    logical, intent(in) :: ghost
    call helicity_init1 (hel, h)
    hel%ghost = ghost
  end subroutine helicity_init1g

  elemental subroutine helicity_init2 (hel, h2, h1)
    type(helicity_t), intent(out) :: hel
    integer, intent(in) :: h1, h2
    hel%defined = .true.
    hel%h2 = h2
    hel%h1 = h1
  end subroutine helicity_init2

  elemental subroutine helicity_init2g (hel, h2, h1, ghost)
    type(helicity_t), intent(out) :: hel
    integer, intent(in) :: h1, h2
    logical, intent(in) :: ghost
    call helicity_init2 (hel, h2, h1)
    hel%ghost = ghost
  end subroutine helicity_init2g

  elemental subroutine helicity_set_ghost (hel, ghost)
    type(helicity_t), intent(inout) :: hel
    logical, intent(in) :: ghost
    hel%ghost = ghost
  end subroutine helicity_set_ghost

  elemental subroutine helicity_undefine (hel)
    type(helicity_t), intent(inout) :: hel
    hel%defined = .false.
    hel%ghost = .false.
  end subroutine helicity_undefine

  elemental subroutine helicity_diagonalize (hel)
    type(helicity_t), intent(inout) :: hel
    hel%h2 = hel%h1
  end subroutine helicity_diagonalize

  subroutine helicity_write (hel, unit)
    type(helicity_t), intent(in) :: hel
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (hel%defined) then
       if (hel%ghost) then
          write (u, "(A)", advance="no")  "h*("
       else
          write (u, "(A)", advance="no")  "h("
       end if
       write (u, "(I0)", advance="no")  hel%h1
       if (hel%h1 /= hel%h2) then
          write (u, "(A)", advance="no") "|"
          write (u, "(I0)", advance="no")  hel%h2
       end if
       write (u, "(A)", advance="no")  ")"
    else if (hel%ghost) then
       write (u, "(A)", advance="no")  "h*"
    end if
  end subroutine helicity_write

  subroutine helicity_write_raw (hel, u)
    type(helicity_t), intent(in) :: hel
    integer, intent(in) :: u
    write (u) hel%defined
    if (hel%defined) then
       write (u) hel%h1, hel%h2
       write (u) hel%ghost
    end if
  end subroutine helicity_write_raw

  subroutine helicity_read_raw (hel, u, iostat)
    type(helicity_t), intent(out) :: hel
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    read (u, iostat=iostat) hel%defined
    if (hel%defined) then
       read (u, iostat=iostat) hel%h1, hel%h2
       read (u, iostat=iostat) hel%ghost
    end if
  end subroutine helicity_read_raw

  elemental function helicity_is_defined (hel) result (defined)
    logical :: defined
    type(helicity_t), intent(in) :: hel
    defined = hel%defined
  end function helicity_is_defined

  elemental function helicity_is_diagonal (hel) result (diagonal)
    logical :: diagonal
    type(helicity_t), intent(in) :: hel
    if (hel%defined) then
       diagonal = hel%h1 == hel%h2
    else
       diagonal = .true.
    end if
  end function helicity_is_diagonal

  elemental function helicity_is_ghost (hel) result (ghost)
    logical :: ghost
    type(helicity_t), intent(in) :: hel
    ghost = hel%ghost
  end function helicity_is_ghost

  pure function helicity_get (hel) result (h)
    integer, dimension(2) :: h
    type(helicity_t), intent(in) :: hel
    h(1) = hel%h2
    h(2) = hel%h1
  end function helicity_get

  elemental function helicity_match (hel1, hel2) result (eq)
    logical :: eq
    type(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       eq = (hel1%h1 == hel2%h1) .and. (hel1%h2 == hel2%h2)
    else
       eq = .true.
    end if
  end function helicity_match

  elemental function helicity_match_diagonal (hel1, hel2) result (eq)
    logical :: eq
    type(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       eq = (hel1%h1 == hel2%h1) .and. (hel1%h2 == hel2%h2)
    else if (hel1%defined) then
       eq = hel1%h1 == hel1%h2
    else if (hel2%defined) then
       eq = hel2%h1 == hel2%h2
    else
       eq = .true.
    end if
  end function helicity_match_diagonal

  elemental function helicity_eq (hel1, hel2) result (eq)
    logical :: eq
    type(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       eq = (hel1%h1 == hel2%h1) .and. (hel1%h2 == hel2%h2) &
            .and. (hel1%ghost .eqv. hel2%ghost)
    else if (.not. hel1%defined .and. .not. hel2%defined) then
       eq = hel1%ghost .eqv. hel2%ghost
    else
       eq = .false.
    end if
  end function helicity_eq

  elemental function helicity_neq (hel1, hel2) result (neq)
    logical :: neq
    type(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       neq = (hel1%h1 /= hel2%h1) .or. (hel1%h2 /= hel2%h2) &
            .or. (hel1%ghost .neqv. hel2%ghost)
    else if (.not. hel1%defined .and. .not. hel2%defined) then
       neq = hel1%ghost .neqv. hel2%ghost
    else
       neq = .true.
    end if
  end function helicity_neq

  elemental function merge_helicities (hel1, hel2) result (hel)
    type(helicity_t) :: hel
    type(helicity_t), intent(in) :: hel1, hel2
    if (helicity_is_defined (hel1) .and. helicity_is_defined (hel2)) then
       call helicity_init2g (hel, hel2%h1, hel1%h1, hel1%ghost)
    else if (helicity_is_defined (hel1)) then
       hel = hel1
    else if (helicity_is_defined (hel2)) then
       hel = hel2
    end if
  end function merge_helicities


end module helicities
