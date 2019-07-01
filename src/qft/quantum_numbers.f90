! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module quantum_numbers

  use io_units
  use model_data
  use helicities
  use colors
  use flavors

  implicit none
  private

  public :: quantum_numbers_t
  public :: quantum_numbers_init
  public :: quantum_numbers_write
  public :: quantum_numbers_write_raw
  public :: quantum_numbers_read_raw
  public :: quantum_numbers_get_flavor
  public :: quantum_numbers_get_color
  public :: quantum_numbers_get_helicity
  public :: quantum_numbers_set_flavor
  public :: quantum_numbers_set_color
  public :: quantum_numbers_set_helicity
  public :: quantum_numbers_set_color_ghost
  public :: quantum_numbers_set_helicity_ghost
  public :: quantum_numbers_set_model
  public :: quantum_numbers_get_color_type
  public :: quantum_numbers_are_valid
  public :: quantum_numbers_are_associated
  public :: quantum_numbers_are_diagonal
  public :: quantum_numbers_is_color_ghost
  public :: quantum_numbers_is_helicity_ghost
  public :: operator(.match.)
  public :: operator(.fmatch.)
  public :: operator(.fhmatch.)
  public :: operator(.dhmatch.)
  public :: operator(==)
  public :: operator(/=)
  public :: quantum_numbers_are_compatible
  public :: quantum_numbers_are_physical
  public :: quantum_numbers_canonicalize_color
  public :: quantum_numbers_set_color_map
  public :: quantum_numbers_translate_color
  public :: quantum_numbers_get_max_color_value
  public :: quantum_numbers_add_color_offset
  public :: quantum_number_array_make_color_contractions
  public :: quantum_numbers_invert_color
  public :: operator(.merge.)
  public :: quantum_numbers_mask_t
  public :: new_quantum_numbers_mask
  public :: quantum_numbers_mask_init
  public :: quantum_numbers_mask_write
  public :: quantum_numbers_mask_set_flavor
  public :: quantum_numbers_mask_set_color
  public :: quantum_numbers_mask_set_helicity
  public :: quantum_numbers_mask_assign
  public :: any
  public :: operator(.or.)
  public :: operator(.eqv.)
  public :: operator(.neqv.)
  public :: quantum_numbers_undefine
  public :: quantum_numbers_undefined
  public :: quantum_numbers_are_redundant
  public :: quantum_numbers_mask_diagonal_helicity

  type :: quantum_numbers_t
     private
     type(flavor_t) :: f
     type(color_t) :: c
     type(helicity_t) :: h
  end type quantum_numbers_t

  type :: quantum_numbers_mask_t
     private
     logical :: f = .false.
     logical :: c = .false.
     logical :: cg = .false.
     logical :: h = .false.
     logical :: hd = .false.
  end type quantum_numbers_mask_t


  interface quantum_numbers_init
     module procedure quantum_numbers_init0_f
     module procedure quantum_numbers_init0_c
     module procedure quantum_numbers_init0_h
     module procedure quantum_numbers_init0_fc
     module procedure quantum_numbers_init0_fh
     module procedure quantum_numbers_init0_ch
     module procedure quantum_numbers_init0_fch
     module procedure quantum_numbers_init1_f
     module procedure quantum_numbers_init1_c
     module procedure quantum_numbers_init1_h
     module procedure quantum_numbers_init1_fc
     module procedure quantum_numbers_init1_fh
     module procedure quantum_numbers_init1_ch
     module procedure quantum_numbers_init1_fch
  end interface

  interface quantum_numbers_write
     module procedure quantum_numbers_write_single
     module procedure quantum_numbers_write_array
  end interface
  interface quantum_numbers_get_flavor
     module procedure quantum_numbers_get_flavor0
     module procedure quantum_numbers_get_flavor1
  end interface

  interface quantum_numbers_set_flavor
     module procedure quantum_numbers_set_flavor0
     module procedure quantum_numbers_set_flavor1
  end interface

  interface quantum_numbers_set_model
     module procedure quantum_numbers_set_model_single
     module procedure quantum_numbers_set_model_array
  end interface

  interface operator(.match.)
     module procedure quantum_numbers_match
  end interface
  interface operator(.fmatch.)
     module procedure quantum_numbers_match_f
  end interface
  interface operator(.fhmatch.)
     module procedure quantum_numbers_match_fh
  end interface
  interface operator(.dhmatch.)
     module procedure quantum_numbers_match_hel_diag
  end interface
  interface operator(==)
     module procedure quantum_numbers_eq
  end interface
  interface operator(/=)
     module procedure quantum_numbers_neq
  end interface
  interface quantum_numbers_translate_color
     module procedure quantum_numbers_translate_color0
     module procedure quantum_numbers_translate_color1
  end interface

  interface quantum_numbers_get_max_color_value
     module procedure quantum_numbers_get_max_color_value0
     module procedure quantum_numbers_get_max_color_value1
     module procedure quantum_numbers_get_max_color_value2
  end interface

  interface operator(.merge.)
     module procedure merge_quantum_numbers0
     module procedure merge_quantum_numbers1
  end interface

  interface quantum_numbers_mask_write
     module procedure quantum_numbers_mask_write_single
     module procedure quantum_numbers_mask_write_array
  end interface
  interface any
     module procedure quantum_numbers_mask_any
  end interface
  interface operator(.or.)
     module procedure quantum_numbers_mask_or
  end interface

  interface operator(.eqv.)
     module procedure quantum_numbers_mask_eqv
  end interface
  interface operator(.neqv.)
     module procedure quantum_numbers_mask_neqv
  end interface
  interface quantum_numbers_undefined
     module procedure quantum_numbers_undefined0
     module procedure quantum_numbers_undefined1
     module procedure quantum_numbers_undefined11
  end interface


contains

  subroutine quantum_numbers_init0_f (qn, flv)
    type(quantum_numbers_t), intent(out) :: qn
    type(flavor_t), intent(in) :: flv
    qn%f = flv
  end subroutine quantum_numbers_init0_f

  subroutine quantum_numbers_init0_c (qn, col)
    type(quantum_numbers_t), intent(out) :: qn
    type(color_t), intent(in) :: col
    qn%c = col
  end subroutine quantum_numbers_init0_c

  subroutine quantum_numbers_init0_h (qn, hel)
    type(quantum_numbers_t), intent(out) :: qn
    type(helicity_t), intent(in) :: hel
    qn%h = hel
  end subroutine quantum_numbers_init0_h

  subroutine quantum_numbers_init0_fc (qn, flv, col)
    type(quantum_numbers_t), intent(out) :: qn
    type(flavor_t), intent(in) :: flv
    type(color_t), intent(in) :: col
    qn%f = flv
    qn%c = col
  end subroutine quantum_numbers_init0_fc

  subroutine quantum_numbers_init0_fh (qn, flv, hel)
    type(quantum_numbers_t), intent(out) :: qn
    type(flavor_t), intent(in) :: flv
    type(helicity_t), intent(in) :: hel
    qn%f = flv
    qn%h = hel
  end subroutine quantum_numbers_init0_fh

  subroutine quantum_numbers_init0_ch (qn, col, hel)
    type(quantum_numbers_t), intent(out) :: qn
    type(color_t), intent(in) :: col
    type(helicity_t), intent(in) :: hel
    qn%c = col
    qn%h = hel
  end subroutine quantum_numbers_init0_ch

  subroutine quantum_numbers_init0_fch (qn, flv, col, hel)
    type(quantum_numbers_t), intent(out) :: qn
    type(flavor_t), intent(in) :: flv
    type(color_t), intent(in) :: col
    type(helicity_t), intent(in) :: hel
    qn%f = flv
    qn%c = col
    qn%h = hel
  end subroutine quantum_numbers_init0_fch

  subroutine quantum_numbers_init1_f (qn, flv)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(flavor_t), dimension(:), intent(in) :: flv
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_f (qn(i), flv(i))
    end do
  end subroutine quantum_numbers_init1_f

  subroutine quantum_numbers_init1_c (qn, col)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(color_t), dimension(:), intent(in) :: col
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_c (qn(i), col(i))
    end do
  end subroutine quantum_numbers_init1_c

  subroutine quantum_numbers_init1_h (qn, hel)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(helicity_t), dimension(:), intent(in) :: hel
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_h (qn(i), hel(i))
    end do
  end subroutine quantum_numbers_init1_h

  subroutine quantum_numbers_init1_fc (qn, flv, col)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(flavor_t), dimension(:), intent(in) :: flv
    type(color_t), dimension(:), intent(in) :: col
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_fc (qn(i), flv(i), col(i))
    end do
  end subroutine quantum_numbers_init1_fc

  subroutine quantum_numbers_init1_fh (qn, flv, hel)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(flavor_t), dimension(:), intent(in) :: flv
    type(helicity_t), dimension(:), intent(in) :: hel
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_fh (qn(i), flv(i), hel(i))
    end do
  end subroutine quantum_numbers_init1_fh

  subroutine quantum_numbers_init1_ch (qn, col, hel)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(color_t), dimension(:), intent(in) :: col
    type(helicity_t), dimension(:), intent(in) :: hel
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_ch (qn(i), col(i), hel(i))
    end do
  end subroutine quantum_numbers_init1_ch

  subroutine quantum_numbers_init1_fch (qn, flv, col, hel)
    type(quantum_numbers_t), dimension(:), intent(out) :: qn
    type(flavor_t), dimension(:), intent(in) :: flv
    type(color_t), dimension(:), intent(in) :: col
    type(helicity_t), dimension(:), intent(in) :: hel
    integer :: i
    do i = 1, size (qn)
       call quantum_numbers_init0_fch (qn(i), flv(i), col(i), hel(i))
    end do
  end subroutine quantum_numbers_init1_fch

  subroutine quantum_numbers_write_single (qn, unit)
    type(quantum_numbers_t), intent(in) :: qn
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no")  "["
    if (flavor_is_defined (qn%f)) then
       call flavor_write (qn%f, u)
       if (color_is_defined (qn%c) .or. helicity_is_defined (qn%h)) &
            write (u, "(1x)", advance="no")
    end if
    if (color_is_defined (qn%c) .or. color_is_ghost (qn%c)) then
       call color_write (qn%c, u)
       if (helicity_is_defined (qn%h))  write (u, "(1x)", advance="no")
    end if
    if (helicity_is_defined (qn%h)) then
       call helicity_write (qn%h, u)
    end if
    write (u, "(A)", advance="no")  "]"
  end subroutine quantum_numbers_write_single

  subroutine quantum_numbers_write_array (qn, unit)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    integer, intent(in), optional :: unit
    integer :: i
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no")  "["
    do i = 1, size (qn)
       if (i > 1)  write (u, "(A)", advance="no")  " / "
       if (flavor_is_defined (qn(i)%f)) then
          call flavor_write (qn(i)%f, u)
          if (color_is_defined (qn(i)%c) .or. helicity_is_defined (qn(i)%h)) &
               write (u, "(1x)", advance="no")
       end if
       if (color_is_defined (qn(i)%c) .or. color_is_ghost (qn(i)%c)) then
          call color_write (qn(i)%c, u)
          if (helicity_is_defined (qn(i)%h))  write (u, "(1x)", advance="no")
       end if
       if (helicity_is_defined (qn(i)%h)) then
          call helicity_write (qn(i)%h, u)
       end if
    end do
    write (u, "(A)", advance="no")  "]"
  end subroutine quantum_numbers_write_array

  subroutine quantum_numbers_write_raw (qn, u)
    type(quantum_numbers_t), intent(in) :: qn
    integer, intent(in) :: u
    call flavor_write_raw (qn%f, u)
    call color_write_raw (qn%c, u)
    call helicity_write_raw (qn%h, u)
  end subroutine quantum_numbers_write_raw

  subroutine quantum_numbers_read_raw (qn, u, iostat)
    type(quantum_numbers_t), intent(out) :: qn
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    call flavor_read_raw (qn%f, u, iostat=iostat)
    call color_read_raw (qn%c, u, iostat=iostat)
    call helicity_read_raw (qn%h, u, iostat=iostat)
  end subroutine quantum_numbers_read_raw

  function quantum_numbers_get_flavor0 (qn) result (flv)
    type(flavor_t) :: flv
    type(quantum_numbers_t), intent(in) :: qn
    flv = qn%f
  end function quantum_numbers_get_flavor0

  function quantum_numbers_get_flavor1 (qn) result (flv)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    type(flavor_t), dimension(size(qn)) :: flv
    integer :: i
    do i = 1, size (qn)
       flv(i) = qn(i)%f
    end do
  end function quantum_numbers_get_flavor1

  elemental function quantum_numbers_get_color (qn) result (col)
    type(color_t) :: col
    type(quantum_numbers_t), intent(in) :: qn
    col = qn%c
  end function quantum_numbers_get_color

  elemental function quantum_numbers_get_helicity (qn) result (hel)
    type(helicity_t) :: hel
    type(quantum_numbers_t), intent(in) :: qn
    hel = qn%h
  end function quantum_numbers_get_helicity

  subroutine quantum_numbers_set_flavor0 (qn, flv)
    type(quantum_numbers_t), intent(inout) :: qn
    type(flavor_t), intent(in) :: flv
    qn%f = flv
  end subroutine quantum_numbers_set_flavor0

  subroutine quantum_numbers_set_flavor1 (qn, flv)
    type(quantum_numbers_t), dimension(:), intent(inout) :: qn
    type(flavor_t), dimension(:), intent(in) :: flv
    integer :: i
    do i = 1, size (flv)
       qn(i)%f = flv(i)
    end do
  end subroutine quantum_numbers_set_flavor1

  elemental subroutine quantum_numbers_set_color (qn, col)
    type(quantum_numbers_t), intent(inout) :: qn
    type(color_t), intent(in) :: col
    qn%c = col
  end subroutine quantum_numbers_set_color

  elemental subroutine quantum_numbers_set_helicity (qn, hel)
    type(quantum_numbers_t), intent(inout) :: qn
    type(helicity_t), intent(in) :: hel
    qn%h = hel
  end subroutine quantum_numbers_set_helicity

  elemental subroutine quantum_numbers_set_color_ghost (qn, ghost)
    type(quantum_numbers_t), intent(inout) :: qn
    logical, intent(in) :: ghost
    call color_set_ghost (qn%c, ghost)
  end subroutine quantum_numbers_set_color_ghost

  elemental subroutine quantum_numbers_set_helicity_ghost (qn, ghost)
    type(quantum_numbers_t), intent(inout) :: qn
    logical, intent(in) :: ghost
    call helicity_set_ghost (qn%h, ghost)
  end subroutine quantum_numbers_set_helicity_ghost

  subroutine quantum_numbers_set_model_single (qn, model)
    type(quantum_numbers_t), intent(inout) :: qn
    class(model_data_t), intent(in), target :: model
    call flavor_set_model (qn%f, model)
  end subroutine quantum_numbers_set_model_single

  subroutine quantum_numbers_set_model_array (qn, model)
    type(quantum_numbers_t), dimension(:), intent(inout) :: qn
    class(model_data_t), intent(in), target :: model
    call flavor_set_model (qn%f, model)
  end subroutine quantum_numbers_set_model_array

  elemental function quantum_numbers_get_color_type (qn) result (color_type)
    integer :: color_type
    type(quantum_numbers_t), intent(in) :: qn
    color_type = flavor_get_color_type (qn%f)
  end function quantum_numbers_get_color_type

  elemental function quantum_numbers_are_valid (qn) result (valid)
    logical :: valid
    type(quantum_numbers_t), intent(in) :: qn
    valid = flavor_is_valid (qn%f)
  end function quantum_numbers_are_valid

  elemental function quantum_numbers_are_associated (qn) result (flag)
    logical :: flag
    type(quantum_numbers_t), intent(in) :: qn
    flag = flavor_is_associated (qn%f)
  end function quantum_numbers_are_associated

  elemental function quantum_numbers_are_diagonal (qn) result (diagonal)
    logical :: diagonal
    type(quantum_numbers_t), intent(in) :: qn
    diagonal = helicity_is_diagonal (qn%h) .and. color_is_diagonal (qn%c)
  end function quantum_numbers_are_diagonal

  elemental function quantum_numbers_is_color_ghost (qn) result (ghost)
    logical :: ghost
    type(quantum_numbers_t), intent(in) :: qn
    ghost = color_is_ghost (qn%c)
  end function quantum_numbers_is_color_ghost

  elemental function quantum_numbers_is_helicity_ghost (qn) result (ghost)
    logical :: ghost
    type(quantum_numbers_t), intent(in) :: qn
    ghost = helicity_is_ghost (qn%h)
  end function quantum_numbers_is_helicity_ghost

  elemental function quantum_numbers_match (qn1, qn2) result (match)
    logical :: match
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    match = (qn1%f .match. qn2%f) .and. &
         (qn1%c .match. qn2%c) .and. &
         (qn1%h .match. qn2%h)
  end function quantum_numbers_match

  elemental function quantum_numbers_match_f (qn1, qn2) result (match)
    logical :: match
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    match = (qn1%f .match. qn2%f)
  end function quantum_numbers_match_f

  elemental function quantum_numbers_match_fh (qn1, qn2) result (match)
    logical :: match
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    match = (qn1%f .match. qn2%f) .and. &
         (qn1%h .match. qn2%h)
  end function quantum_numbers_match_fh

  elemental function quantum_numbers_match_hel_diag (qn1, qn2) result (match)
    logical :: match
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    match = (qn1%f .match. qn2%f) .and. &
         (qn1%c .match. qn2%c) .and. &
         (qn1%h .dmatch. qn2%h)
  end function quantum_numbers_match_hel_diag

  elemental function quantum_numbers_eq (qn1, qn2) result (eq)
    logical :: eq
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    eq = (qn1%f == qn2%f) .and. &
         (qn1%c == qn2%c) .and. &
         (qn1%h == qn2%h)
  end function quantum_numbers_eq

  elemental function quantum_numbers_neq (qn1, qn2) result (neq)
    logical :: neq
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    neq = (qn1%f /= qn2%f) .or. &
         (qn1%c /= qn2%c) .or. &
         (qn1%h /= qn2%h)
  end function quantum_numbers_neq

  elemental function quantum_numbers_are_compatible (qn1, qn2, mask) &
      result (flag)
    logical :: flag
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    type(quantum_numbers_mask_t), intent(in) :: mask
    if (mask%h .or. mask%hd) then
       flag = (qn1%f .match. qn2%f) .and. (qn1%h .match. qn2%h)
    else
       flag = (qn1%f .match. qn2%f)
    end if
    if (mask%c) then
       flag = flag .and. (color_is_ghost (qn1%c) .eqv. color_is_ghost (qn2%c))
    else
       flag = flag .and. &
            .not. (color_is_ghost (qn1%c) .or. color_is_ghost (qn2%c)) .and. &
            (qn1%c == qn2%c)
    end if
  end function quantum_numbers_are_compatible

  elemental function quantum_numbers_are_physical (qn, mask) result (flag)
    logical :: flag
    type(quantum_numbers_t), intent(in) :: qn
    type(quantum_numbers_mask_t), intent(in) :: mask
    if (mask%c) then
       flag = .true.
    else
       flag = .not. color_is_ghost (qn%c)
    end if
  end function quantum_numbers_are_physical

  subroutine quantum_numbers_canonicalize_color (qn)
    type(quantum_numbers_t), dimension(:), intent(inout) :: qn
    call color_canonicalize (qn%c)
  end subroutine quantum_numbers_canonicalize_color

  subroutine quantum_numbers_set_color_map (map, qn1, qn2)
    integer, dimension(:,:), intent(out), allocatable :: map
    type(quantum_numbers_t), dimension(:), intent(in) :: qn1, qn2
    call set_color_map (map, qn1%c, qn2%c)
  end subroutine quantum_numbers_set_color_map

  subroutine quantum_numbers_translate_color0 (qn, map, offset)
    type(quantum_numbers_t), intent(inout) :: qn
    integer, dimension(:,:), intent(in) :: map
    integer, intent(in), optional :: offset
    call color_translate (qn%c, map, offset)
  end subroutine quantum_numbers_translate_color0
  
  subroutine quantum_numbers_translate_color1 (qn, map, offset)
    type(quantum_numbers_t), dimension(:), intent(inout) :: qn
    integer, dimension(:,:), intent(in) :: map
    integer, intent(in), optional :: offset
    call color_translate (qn%c, map, offset)
  end subroutine quantum_numbers_translate_color1

  function quantum_numbers_get_max_color_value0 (qn) result (cmax)
    integer :: cmax
    type(quantum_numbers_t), intent(in) :: qn
    cmax = color_get_max_value (qn%c)
  end function quantum_numbers_get_max_color_value0
    
  function quantum_numbers_get_max_color_value1 (qn) result (cmax)
    integer :: cmax
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    cmax = color_get_max_value (qn%c)
  end function quantum_numbers_get_max_color_value1
    
  function quantum_numbers_get_max_color_value2 (qn) result (cmax)
    integer :: cmax
    type(quantum_numbers_t), dimension(:,:), intent(in) :: qn
    cmax = color_get_max_value (qn%c)
  end function quantum_numbers_get_max_color_value2
    
  elemental subroutine quantum_numbers_add_color_offset (qn, offset)
    type(quantum_numbers_t), intent(inout) :: qn
    integer, intent(in) :: offset
    call color_add_offset (qn%c, offset)
  end subroutine quantum_numbers_add_color_offset

  subroutine quantum_number_array_make_color_contractions (qn_in, qn_out)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn_in
    type(quantum_numbers_t), dimension(:,:), intent(out), allocatable :: qn_out
    type(color_t), dimension(:,:), allocatable :: col
    integer :: i
    call color_array_make_contractions (qn_in%c, col)
    allocate (qn_out (size (col, 1), size (col, 2)))
    do i = 1, size (qn_out, 2)
       qn_out(:,i)%f = qn_in%f
       qn_out(:,i)%c = col(:,i)
       qn_out(:,i)%h = qn_in%h
    end do
  end subroutine quantum_number_array_make_color_contractions

  elemental subroutine quantum_numbers_invert_color (qn)
    type(quantum_numbers_t), intent(inout) :: qn
    call color_invert (qn%c)
  end subroutine quantum_numbers_invert_color

  function merge_quantum_numbers0 (qn1, qn2) result (qn3)
    type(quantum_numbers_t) :: qn3
    type(quantum_numbers_t), intent(in) :: qn1, qn2
    qn3%f = qn1%f .merge. qn2%f
    qn3%c = qn1%c .merge. qn2%c
    qn3%h = qn1%h .merge. qn2%h
  end function merge_quantum_numbers0

  function merge_quantum_numbers1 (qn1, qn2) result (qn3)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn1, qn2
    type(quantum_numbers_t), dimension(size(qn1)) :: qn3
    qn3%f = qn1%f .merge. qn2%f
    qn3%c = qn1%c .merge. qn2%c
    qn3%h = qn1%h .merge. qn2%h
  end function merge_quantum_numbers1

  elemental function new_quantum_numbers_mask &
       (mask_f, mask_c, mask_h, mask_cg, mask_hd) result (mask)
    type(quantum_numbers_mask_t) :: mask
    logical, intent(in) :: mask_f, mask_c, mask_h
    logical, intent(in), optional :: mask_cg
    logical, intent(in), optional :: mask_hd
    call quantum_numbers_mask_init &
         (mask, mask_f, mask_c, mask_h, mask_cg, mask_hd)
  end function new_quantum_numbers_mask

  elemental subroutine quantum_numbers_mask_init &
       (mask, mask_f, mask_c, mask_h, mask_cg, mask_hd)
    type(quantum_numbers_mask_t), intent(out) :: mask
    logical, intent(in) :: mask_f, mask_c, mask_h
    logical, intent(in), optional :: mask_cg, mask_hd
    mask%f = mask_f
    mask%c = mask_c
    mask%h = mask_h
    if (present (mask_cg)) then
       if (mask%c)  mask%cg = mask_cg
    else
       mask%cg = mask_c
    end if
    if (present (mask_hd)) then
       if (.not. mask%h)  mask%hd = mask_hd
    end if
  end subroutine quantum_numbers_mask_init

  subroutine quantum_numbers_mask_write_single (mask, unit)
    type(quantum_numbers_mask_t), intent(in) :: mask
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no") "["
    write (u, "(L1)", advance="no")  mask%f
    write (u, "(L1)", advance="no")  mask%c
    if (.not.mask%cg)  write (u, "('g')", advance="no")
    write (u, "(L1)", advance="no")  mask%h
    if (mask%hd)  write (u, "('d')", advance="no")
    write (u, "(A)", advance="no") "]"
  end subroutine quantum_numbers_mask_write_single

  subroutine quantum_numbers_mask_write_array (mask, unit)
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no") "["
    do i = 1, size (mask)
       if (i > 1)  write (u, "(A)", advance="no")  "/"
       write (u, "(L1)", advance="no")  mask(i)%f
       write (u, "(L1)", advance="no")  mask(i)%c
       if (.not.mask(i)%cg)  write (u, "('g')", advance="no")
       write (u, "(L1)", advance="no")  mask(i)%h
       if (mask(i)%hd)  write (u, "('d')", advance="no")
    end do
    write (u, "(A)", advance="no") "]"
  end subroutine quantum_numbers_mask_write_array

  elemental subroutine quantum_numbers_mask_set_flavor (mask, mask_f)
    type(quantum_numbers_mask_t), intent(inout) :: mask
    logical, intent(in) :: mask_f
    mask%f = mask_f
  end subroutine quantum_numbers_mask_set_flavor

  elemental subroutine quantum_numbers_mask_set_color (mask, mask_c, mask_cg)
    type(quantum_numbers_mask_t), intent(inout) :: mask
    logical, intent(in) :: mask_c
    logical, intent(in), optional :: mask_cg
    mask%c = mask_c
    if (present (mask_cg)) then
       if (mask%c)  mask%cg = mask_cg
    else
       mask%cg = mask_c
    end if
  end subroutine quantum_numbers_mask_set_color

  elemental subroutine quantum_numbers_mask_set_helicity (mask, mask_h, mask_hd)
    type(quantum_numbers_mask_t), intent(inout) :: mask
    logical, intent(in) :: mask_h
    logical, intent(in), optional :: mask_hd
    mask%h = mask_h
    if (present (mask_hd)) then
       if (.not. mask%h)  mask%hd = mask_hd
    end if
  end subroutine quantum_numbers_mask_set_helicity

  elemental subroutine quantum_numbers_mask_assign &
       (mask, mask_in, flavor, color, helicity)
    type(quantum_numbers_mask_t), intent(inout) :: mask
    type(quantum_numbers_mask_t), intent(in) :: mask_in
    logical, intent(in), optional :: flavor, color, helicity
    if (present (flavor)) then
       if (flavor) then
          mask%f = mask_in%f
       end if
    end if
    if (present (color)) then
       if (color) then
          mask%c = mask_in%c
          mask%cg = mask_in%cg
       end if
    end if
    if (present (helicity)) then
       if (helicity) then
          mask%h = mask_in%h
          mask%hd = mask_in%hd
       end if
    end if
  end subroutine quantum_numbers_mask_assign

  function quantum_numbers_mask_any (mask) result (match)
    logical :: match
    type(quantum_numbers_mask_t), intent(in) :: mask
    match = mask%f .or. mask%c .or. mask%h .or. mask%hd
  end function quantum_numbers_mask_any

  elemental function quantum_numbers_mask_or (mask1, mask2) result (mask)
     type(quantum_numbers_mask_t) :: mask
     type(quantum_numbers_mask_t), intent(in) :: mask1, mask2
     mask%f = mask1%f .or. mask2%f
     mask%c = mask1%c .or. mask2%c
     if (mask%c)  mask%cg = mask1%cg .or. mask2%cg
     mask%h = mask1%h .or. mask2%h
     if (.not. mask%h)  mask%hd = mask1%hd .or. mask2%hd
   end function quantum_numbers_mask_or

  elemental function quantum_numbers_mask_eqv (mask1, mask2) result (eqv)
    logical :: eqv
    type(quantum_numbers_mask_t), intent(in) :: mask1, mask2
    eqv = (mask1%f .eqv. mask2%f) .and. &
         (mask1%c .eqv. mask2%c) .and. &
         (mask1%cg .eqv. mask2%cg) .and. &
         (mask1%h .eqv. mask2%h) .and. &
         (mask1%hd .eqv. mask2%hd)
  end function quantum_numbers_mask_eqv

  elemental function quantum_numbers_mask_neqv (mask1, mask2) result (neqv)
    logical :: neqv
    type(quantum_numbers_mask_t), intent(in) :: mask1, mask2
    neqv = (mask1%f .neqv. mask2%f) .or. &
         (mask1%c .neqv. mask2%c) .or. &
         (mask1%cg .neqv. mask2%cg) .or. &
         (mask1%h .neqv. mask2%h) .or. &
         (mask1%hd .neqv. mask2%hd)
  end function quantum_numbers_mask_neqv

  elemental subroutine quantum_numbers_undefine (qn, mask)
    type(quantum_numbers_t), intent(inout) :: qn
    type(quantum_numbers_mask_t), intent(in) :: mask
    if (mask%f)  call flavor_undefine (qn%f)
    if (mask%c)  call color_undefine (qn%c, undefine_ghost=mask%cg)
    if (mask%h) then
       call helicity_undefine (qn%h)
    else if (mask%hd) then
       if (.not. helicity_is_diagonal (qn%h)) then
          call helicity_diagonalize (qn%h)
       end if
    end if
  end subroutine quantum_numbers_undefine

  function quantum_numbers_undefined0 (qn, mask) result (qn_new)
    type(quantum_numbers_t), intent(in) :: qn
    type(quantum_numbers_mask_t), intent(in) :: mask
    type(quantum_numbers_t) :: qn_new
    qn_new = qn
    call quantum_numbers_undefine (qn_new, mask)
  end function quantum_numbers_undefined0

  function quantum_numbers_undefined1 (qn, mask) result (qn_new)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    type(quantum_numbers_mask_t), intent(in) :: mask
    type(quantum_numbers_t), dimension(size(qn)) :: qn_new
    qn_new = qn
    call quantum_numbers_undefine (qn_new, mask)
  end function quantum_numbers_undefined1

  function quantum_numbers_undefined11 (qn, mask) result (qn_new)
    type(quantum_numbers_t), dimension(:), intent(in) :: qn
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    type(quantum_numbers_t), dimension(size(qn)) :: qn_new
    qn_new = qn
    call quantum_numbers_undefine (qn_new, mask)
  end function quantum_numbers_undefined11

  elemental function quantum_numbers_are_redundant (qn, mask) &
       result (redundant)
    logical :: redundant
    type(quantum_numbers_t), intent(in) :: qn
    type(quantum_numbers_mask_t), intent(in) :: mask
    redundant = .false.
    if (mask%f) then
       redundant = flavor_is_defined (qn%f)
    end if
    if (mask%c) then
       redundant = color_is_defined (qn%c)
    end if
    if (mask%h) then
       redundant = helicity_is_defined (qn%h)
    else if (mask%hd) then
       redundant = .not. helicity_is_diagonal (qn%h)
    end if
  end function quantum_numbers_are_redundant

  elemental function quantum_numbers_mask_diagonal_helicity (mask) &
       result (flag)
    logical :: flag
    type(quantum_numbers_mask_t), intent(in) :: mask
    flag = mask%h .or. mask%hd
  end function quantum_numbers_mask_diagonal_helicity


end module quantum_numbers
