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

module resonances

  use kinds, only: default
!   use kinds, only: TC
  use iso_varying_string, string_t => varying_string
  use string_utils, only: str
  use io_units
  use diagnostics
  use lorentz, only: vector4_t, compute_resonance_mass
  use constants, only: one
  use model_data, only: model_data_t
  use flavors, only: flavor_t

  implicit none
  private

  public :: resonance_contributors_t
  public :: resonance_info_t
  public :: resonance_history_t
  public :: resonance_history_set_t

  integer, parameter :: n_max_resonances = 10
  integer, parameter :: resonance_history_set_initial_size = 16

  type :: resonance_contributors_t
     integer, dimension(:), allocatable :: c
   contains
     procedure, private :: resonance_contributors_equal
     generic :: operator(==) => resonance_contributors_equal
     procedure, private :: resonance_contributors_assign
     generic :: assignment(=) => resonance_contributors_assign
  end type resonance_contributors_t

  type :: resonance_info_t
     type(flavor_t) :: flavor
     type(resonance_contributors_t) :: contributors
  contains
     procedure :: write => resonance_info_write
     procedure, private :: resonance_info_init_pdg
     procedure, private :: resonance_info_init_flv
     generic :: init => resonance_info_init_pdg, resonance_info_init_flv
     procedure, private :: resonance_info_equal
     generic :: operator(==) => resonance_info_equal
     procedure :: mapping => resonance_info_mapping
     procedure :: as_omega_string => resonance_info_as_omega_string
  end type resonance_info_t

  type :: resonance_history_t
     type(resonance_info_t), dimension(:), allocatable :: resonances
     integer :: n_resonances = 0
  contains
     procedure :: clear => resonance_history_clear
     procedure :: write => resonance_history_write
     procedure, private :: resonance_history_equal
     generic :: operator(==) => resonance_history_equal
     procedure, private :: resonance_history_contains
     generic :: operator(.contains.) => resonance_history_contains
     procedure :: add_resonance => resonance_history_add_resonance
     procedure :: remove_resonance => resonance_history_remove_resonance
     procedure :: add_offset => resonance_history_add_offset
     procedure :: contains_leg => resonance_history_contains_leg
     procedure :: mapping => resonance_history_mapping
     procedure :: only_has_n_contributors => resonance_history_only_has_n_contributors
     procedure :: has_flavor => resonance_history_has_flavor
     procedure :: as_omega_string => resonance_history_as_omega_string
  end type resonance_history_t

  type :: resonance_history_set_t
     integer :: n_filter = 0
     type(resonance_history_t), dimension(:), allocatable :: history
     integer :: last = 0
   contains
     procedure :: write => resonance_history_set_write
     procedure :: init => resonance_history_set_init
     procedure :: enter => resonance_history_set_enter
     procedure :: to_array => resonance_history_set_to_array
     procedure, private :: expand => resonance_history_set_expand
  end type resonance_history_set_t


contains

  elemental function resonance_contributors_equal (c1, c2) result (equal)
    logical :: equal
    class(resonance_contributors_t), intent(in) :: c1, c2
    equal = allocated (c1%c) .and. allocated (c2%c)
    if (equal) equal = size (c1%c) == size (c2%c)
    if (equal) equal = all (c1%c == c2%c)
  end function resonance_contributors_equal

  pure subroutine resonance_contributors_assign (contributors_out, contributors_in)
    class(resonance_contributors_t), intent(inout) :: contributors_out
    class(resonance_contributors_t), intent(in) :: contributors_in
    if (allocated (contributors_out%c))  deallocate (contributors_out%c)
    if (allocated (contributors_in%c)) then
       allocate (contributors_out%c (size (contributors_in%c)))
       contributors_out%c = contributors_in%c
    end if
  end subroutine resonance_contributors_assign

  subroutine resonance_info_write (resonance, unit)
    class(resonance_info_t), intent(in) :: resonance
    integer, optional, intent(in) :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, '(A)', advance='no') "Resonance contributors: "
    if (allocated (resonance%contributors%c)) then
       do i = 1, size(resonance%contributors%c)
          write (u, '(I0,1X)', advance='no') resonance%contributors%c(i)
       end do
    else
       write (u, "(A)", advance="no")  "[not allocated]"
    end if
    if (resonance%flavor%is_defined ()) call resonance%flavor%write (u)
    write (u, '(A)')
  end subroutine resonance_info_write

  subroutine resonance_info_init_pdg (resonance, mom_id, pdg, model, n_out)
    class(resonance_info_t), intent(out) :: resonance
    integer, intent(in) :: mom_id
    integer, intent(in) :: pdg, n_out
    class(model_data_t), intent(in), target :: model
    type(flavor_t) :: flv
    call msg_debug (D_PHASESPACE, "resonance_info_init_pdg")
    call flv%init (pdg, model)
    call resonance%init (mom_id, flv, n_out)
  end subroutine resonance_info_init_pdg

  subroutine resonance_info_init_flv (resonance, mom_id, flv, n_out)
    class(resonance_info_t), intent(out) :: resonance
    integer, intent(in) :: mom_id
    type(flavor_t), intent(in) :: flv
    integer, intent(in) :: n_out
    integer :: i
    logical, dimension(n_out) :: contrib
    integer, dimension(n_out) :: tmp
    call msg_debug (D_PHASESPACE, "resonance_info_init_flv")
    resonance%flavor = flv
    do i = 1, n_out
       tmp(i) = i
    end do
    contrib = btest (mom_id, tmp - 1)
    allocate (resonance%contributors%c (count (contrib)))
    resonance%contributors%c = pack (tmp, contrib)
  end subroutine resonance_info_init_flv

  elemental function resonance_info_equal (r1, r2) result (equal)
    logical :: equal
    class(resonance_info_t), intent(in) :: r1, r2
    equal = r1%flavor == r2%flavor .and. r1%contributors == r2%contributors
  end function resonance_info_equal

  function resonance_info_mapping (resonance, s) result (bw)
    real(default) :: bw
    class(resonance_info_t), intent(in) :: resonance
    real(default), intent(in) :: s
    real(default) :: m, gamma
    if (resonance%flavor%is_defined ()) then
       m = resonance%flavor%get_mass ()
       gamma = resonance%flavor%get_width ()
       bw = m**4 / ((s - m**2)**2 + gamma**2 * m**2)
    else
       bw = one
    end if
  end function resonance_info_mapping

  subroutine resonance_history_clear (res_hist)
    class(resonance_history_t), intent(out) :: res_hist
  end subroutine resonance_history_clear

  subroutine resonance_history_write (res_hist, unit)
    class(resonance_history_t), intent(in) :: res_hist
    integer, optional, intent(in) :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write(u, '(A,I0,A)') "Resonance history with ", &
         res_hist%n_resonances, " resonances:"
    do i = 1, res_hist%n_resonances
       call res_hist%resonances(i)%write (u)
    end do
  end subroutine resonance_history_write

  elemental function resonance_history_equal (rh1, rh2) result (equal)
    logical :: equal
    class(resonance_history_t), intent(in) :: rh1, rh2
    integer :: i
    equal = .false.
    if (rh1%n_resonances == rh2%n_resonances) then
       if (size (rh1%resonances) == size (rh2%resonances)) then
          do i = 1, rh1%n_resonances
             if (.not. rh1%resonances(i) == rh2%resonances(i)) then
                return
             end if
          end do
          equal = .true.
       end if
    end if
  end function resonance_history_equal

  elemental function resonance_history_contains (rh1, rh2) result (is_sub_res)
    logical :: is_sub_res
    class(resonance_history_t), intent(in) :: rh1, rh2
    integer :: i_res
    is_sub_res = .false.
    if (rh1%n_resonances > rh2%n_resonances) then
       do i_res = 1, rh1%n_resonances
          is_sub_res = is_sub_res &
               .or. any (rh1%resonances(i_res) == rh2%resonances)
       end do
    end if
  end function resonance_history_contains

  subroutine resonance_history_add_resonance (res_hist, resonance)
    class(resonance_history_t), intent(inout) :: res_hist
    type(resonance_info_t), intent(in) :: resonance
    type(resonance_info_t), dimension(:), allocatable :: tmp
    integer :: n
    call msg_debug (D_PHASESPACE, "resonance_history_add_resonance")
    if (.not. allocated (res_hist%resonances)) then
       n = 0
       allocate (res_hist%resonances (n_max_resonances))
    else
       n = res_hist%n_resonances
       if (n >= size (res_hist%resonances)) then
          allocate (tmp (n + n_max_resonances))
          tmp(1:n) = res_hist%resonances(1:n)
          call move_alloc (from=tmp, to=res_hist%resonances)
       end if
    end if
    res_hist%resonances(n+1) = resonance
    res_hist%n_resonances = n + 1
    call msg_debug &
         (D_PHASESPACE, "res_hist%n_resonances", res_hist%n_resonances)
  end subroutine resonance_history_add_resonance

  subroutine resonance_history_remove_resonance (res_hist, i_res)
    class(resonance_history_t), intent(inout) :: res_hist
    integer, intent(in) :: i_res
    integer :: i
    res_hist%n_resonances = res_hist%n_resonances - 1
    if (res_hist%n_resonances == 0) then
       deallocate (res_hist%resonances)
    else
       do i = i_res + 1, size (res_hist%resonances)
          res_hist%resonances (i - 1) = res_hist%resonances (i)
       end do
    end if
  end subroutine resonance_history_remove_resonance

  subroutine resonance_history_add_offset (res_hist, n)
    class(resonance_history_t), intent(inout) :: res_hist
    integer, intent(in) :: n
    integer :: i_res
    do i_res = 1, res_hist%n_resonances
       associate (contributors => res_hist%resonances(i_res)%contributors%c)
          contributors = contributors + n
       end associate
    end do
  end subroutine resonance_history_add_offset

  function resonance_history_contains_leg (res_hist, i_leg) result (val)
    logical :: val
    class(resonance_history_t), intent(in) :: res_hist
    integer, intent(in) :: i_leg
    integer :: i_res
    val = .false.
    do i_res = 1, res_hist%n_resonances
       if (any (res_hist%resonances(i_res)%contributors%c == i_leg)) then
          val = .true.
          exit
       end if
    end do
  end function resonance_history_contains_leg

  function resonance_history_mapping (res_hist, p, i_gluon) result (p_map)
    real(default) :: p_map
    class(resonance_history_t), intent(in) :: res_hist
    type(vector4_t), intent(in), dimension(:) :: p
    integer, intent(in), optional :: i_gluon
    integer :: i_res
    real(default) :: s
    p_map = one
    do i_res = 1, res_hist%n_resonances
       associate (res => res_hist%resonances(i_res))
          s = compute_resonance_mass (p, res%contributors%c, i_gluon)**2
          p_map = p_map * res%mapping (s)
       end associate
    end do
  end function resonance_history_mapping

  function resonance_history_only_has_n_contributors (res_hist, n) result (value)
    logical :: value
    class(resonance_history_t), intent(in) :: res_hist
    integer, intent(in) :: n
    integer :: i_res
    value = .true.
    do i_res = 1, res_hist%n_resonances
       associate (res => res_hist%resonances(i_res))
          value = value .and. size (res%contributors%c) == n
       end associate
    end do
  end function resonance_history_only_has_n_contributors

  function resonance_history_has_flavor (res_hist, flv) result (has_flv)
    logical :: has_flv
    class(resonance_history_t), intent(in) :: res_hist
    type(flavor_t), intent(in) :: flv
    integer :: i
    has_flv = .false.
    do i = 1, res_hist%n_resonances
       has_flv = has_flv .or. res_hist%resonances(i)%flavor == flv
    end do
  end function resonance_history_has_flavor

  function resonance_info_as_omega_string (res_info, n_in) result (string)
    class(resonance_info_t), intent(in) :: res_info
    integer, intent(in) :: n_in
    type(string_t) :: string
    integer :: i
    string = ""
    if (allocated (res_info%contributors%c)) then
       do i = 1, size (res_info%contributors%c)
          if (i > 1)  string = string // "+"
          string = string // str (res_info%contributors%c(i) + n_in)
       end do
       string = string // "~" // res_info%flavor%get_name ()
    end if
  end function resonance_info_as_omega_string

  function resonance_history_as_omega_string (res_hist, n_in) result (string)
    class(resonance_history_t), intent(in) :: res_hist
    integer, intent(in) :: n_in
    type(string_t) :: string
    integer :: i
    string = ""
    do i = 1, res_hist%n_resonances
       if (i > 1)  string = string // " && "
       string = string // res_hist%resonances(i)%as_omega_string (n_in)
    end do
  end function resonance_history_as_omega_string

  subroutine resonance_history_set_write (res_set, unit)
    class(resonance_history_set_t), intent(in) :: res_set
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(A)")  "Resonance history set:"
    do i = 1, res_set%last
       write (u, "('* ')", advance="no")
       call res_set%history(i)%write (u)
    end do
  end subroutine resonance_history_set_write

  subroutine resonance_history_set_init (res_set, n_filter, initial_size)
    class(resonance_history_set_t), intent(out) :: res_set
    integer, intent(in), optional :: n_filter
    integer, intent(in), optional :: initial_size
    if (present (n_filter))  res_set%n_filter = n_filter
    if (present (initial_size)) then
       allocate (res_set%history (initial_size))
    else
       allocate (res_set%history (resonance_history_set_initial_size))
    end if
  end subroutine resonance_history_set_init

  subroutine resonance_history_set_enter (res_set, res_history)
    class(resonance_history_set_t), intent(inout) :: res_set
    type(resonance_history_t), intent(in) :: res_history
    integer :: i, new
    if (res_history%n_resonances == 0)  return
    if (res_set%n_filter > 0) then
       if (.not. res_history%only_has_n_contributors (res_set%n_filter))  return
    end if
    do i = 1, res_set%last
       if (res_set%history(i) == res_history)  return
    end do 
    new = res_set%last + 1
    if (new > size (res_set%history))  call res_set%expand ()
    res_set%history(new) = res_history
    res_set%last = new
  end subroutine resonance_history_set_enter

  subroutine resonance_history_set_to_array (res_set, res_history)
    class(resonance_history_set_t), intent(inout) :: res_set
    type(resonance_history_t), dimension(:), allocatable, intent(out) :: res_history
    allocate (res_history (res_set%last))
    res_history(:) = res_set%history(1:res_set%last)
  end subroutine resonance_history_set_to_array

  subroutine resonance_history_set_expand (res_set)
    class(resonance_history_set_t), intent(inout) :: res_set
    type(resonance_history_t), dimension(:), allocatable :: history_new
    integer :: s
    s = size (res_set%history)
    allocate (history_new (2 * s))
    history_new(1:s) = res_set%history(1:s)
    call move_alloc (history_new, res_set%history)
  end subroutine resonance_history_set_expand


end module resonances
