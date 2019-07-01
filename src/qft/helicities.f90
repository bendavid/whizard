! WHIZARD 2.4.1 Mar 24 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module helicities

  use io_units

  implicit none
  private

  public :: helicity_t
  public :: helicity

  type :: helicity_t
     private
     logical :: defined = .false.
     integer :: h1, h2
   contains
     generic :: init => helicity_init_empty, helicity_init_same, helicity_init_different
     procedure, private :: helicity_init_empty
     procedure, private :: helicity_init_same
     procedure, private :: helicity_init_different
     procedure :: undefine => helicity_undefine
     procedure :: diagonalize => helicity_diagonalize
     procedure :: get_indices => helicity_get_indices
     procedure :: write => helicity_write
     procedure :: write_raw => helicity_write_raw
     procedure :: read_raw => helicity_read_raw
     procedure :: is_defined => helicity_is_defined
     procedure :: is_diagonal => helicity_is_diagonal
     procedure :: to_pair => helicity_to_pair
     generic :: operator(.match.) => helicity_match
     generic :: operator(.dmatch.) => helicity_match_diagonal
     generic :: operator(==) => helicity_eq
     generic :: operator(/=) => helicity_neq
     procedure, private ::  helicity_match
     procedure, private ::  helicity_match_diagonal
     procedure, private ::  helicity_eq
     procedure, private ::  helicity_neq
     generic :: operator(.merge.) => merge_helicities
     procedure, private ::  merge_helicities
  end type helicity_t


  interface helicity
     module procedure helicity0, helicity1, helicity2
  end interface helicity


contains

  pure function helicity0 () result (hel)
    type(helicity_t) :: hel
  end function helicity0

  elemental function helicity1 (h) result (hel)
    type(helicity_t) :: hel
    integer, intent(in) :: h
    call hel%init (h)
  end function helicity1

  elemental function helicity2 (h2, h1) result (hel)
    type(helicity_t) :: hel
    integer, intent(in) :: h1, h2
    call hel%init (h2, h1)
  end function helicity2

  elemental subroutine helicity_init_empty (hel)
    class(helicity_t), intent(inout) :: hel
    hel%defined = .false.
  end subroutine helicity_init_empty

  elemental subroutine helicity_init_same (hel, h)
    class(helicity_t), intent(inout) :: hel
    integer, intent(in) :: h
    hel%defined = .true.
    hel%h1 = h
    hel%h2 = h
  end subroutine helicity_init_same

  elemental subroutine helicity_init_different (hel, h2, h1)
    class(helicity_t), intent(inout) :: hel
    integer, intent(in) :: h1, h2
    hel%defined = .true.
    hel%h2 = h2
    hel%h1 = h1
  end subroutine helicity_init_different

  elemental subroutine helicity_undefine (hel)
    class(helicity_t), intent(inout) :: hel
    hel%defined = .false.
  end subroutine helicity_undefine

  elemental subroutine helicity_diagonalize (hel)
    class(helicity_t), intent(inout) :: hel
    hel%h2 = hel%h1
  end subroutine helicity_diagonalize

  subroutine helicity_get_indices (hel, h1, h2)
    class(helicity_t), intent(in) :: hel
    integer, intent(out) :: h1, h2
    h1 = hel%h1; h2 = hel%h2
   end subroutine helicity_get_indices

  subroutine helicity_write (hel, unit)
    class(helicity_t), intent(in) :: hel
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    if (hel%defined) then
       write (u, "(A)", advance="no")  "h("
       write (u, "(I0)", advance="no")  hel%h1
       if (hel%h1 /= hel%h2) then
          write (u, "(A)", advance="no") "|"
          write (u, "(I0)", advance="no")  hel%h2
       end if
       write (u, "(A)", advance="no")  ")"
    end if
  end subroutine helicity_write

  subroutine helicity_write_raw (hel, u)
    class(helicity_t), intent(in) :: hel
    integer, intent(in) :: u
    write (u) hel%defined
    if (hel%defined) then
       write (u) hel%h1, hel%h2
    end if
  end subroutine helicity_write_raw

  subroutine helicity_read_raw (hel, u, iostat)
    class(helicity_t), intent(out) :: hel
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    read (u, iostat=iostat) hel%defined
    if (hel%defined) then
       read (u, iostat=iostat) hel%h1, hel%h2
    end if
  end subroutine helicity_read_raw

  elemental function helicity_is_defined (hel) result (defined)
    logical :: defined
    class(helicity_t), intent(in) :: hel
    defined = hel%defined
  end function helicity_is_defined

  elemental function helicity_is_diagonal (hel) result (diagonal)
    logical :: diagonal
    class(helicity_t), intent(in) :: hel
    if (hel%defined) then
       diagonal = hel%h1 == hel%h2
    else
       diagonal = .true.
    end if
  end function helicity_is_diagonal

  pure function helicity_to_pair (hel) result (h)
    integer, dimension(2) :: h
    class(helicity_t), intent(in) :: hel
    h(1) = hel%h2
    h(2) = hel%h1
  end function helicity_to_pair

  elemental function helicity_match (hel1, hel2) result (eq)
    logical :: eq
    class(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       eq = (hel1%h1 == hel2%h1) .and. (hel1%h2 == hel2%h2)
    else
       eq = .true.
    end if
  end function helicity_match

  elemental function helicity_match_diagonal (hel1, hel2) result (eq)
    logical :: eq
    class(helicity_t), intent(in) :: hel1, hel2
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
    class(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       eq = (hel1%h1 == hel2%h1) .and. (hel1%h2 == hel2%h2)
    else if (.not. hel1%defined .and. .not. hel2%defined) then
       eq = .true.
    else
       eq = .false.
    end if
  end function helicity_eq

  elemental function helicity_neq (hel1, hel2) result (neq)
    logical :: neq
    class(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       neq = (hel1%h1 /= hel2%h1) .or. (hel1%h2 /= hel2%h2)
    else if (.not. hel1%defined .and. .not. hel2%defined) then
       neq = .false.
    else
       neq = .true.
    end if
  end function helicity_neq

  elemental function merge_helicities (hel1, hel2) result (hel)
    type(helicity_t) :: hel
    class(helicity_t), intent(in) :: hel1, hel2
    if (hel1%defined .and. hel2%defined) then
       call hel%init (hel2%h1, hel1%h1)
    else if (hel1%defined) then
       call hel%init (hel1%h2, hel1%h1)
    else if (hel2%defined) then
       call hel%init (hel2%h2, hel2%h1)
    end if
  end function merge_helicities


end module helicities
