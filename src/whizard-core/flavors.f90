! WHIZARD 2.0.4 Tue Oct 26 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

module flavors

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use pdg_arrays
  use colors, only: color_t, color_init
  use models

  implicit none
  private

  public :: flavor_t
  public :: flavor_init
  public :: flavor_undefine
  public :: flavor_write
  public :: flavor_write_raw
  public :: flavor_read_raw
  public :: flavor_set_model
  public :: flavor_is_defined
  public :: flavor_is_valid
  public :: flavor_is_associated
  public :: flavor_get_pdg
  public :: flavor_get_pdg_anti
  public :: flavor_get_pdg_abs
  public :: flavor_is_visible
  public :: flavor_is_parton
  public :: flavor_is_beam_remnant
  public :: flavor_is_gauge
  public :: flavor_is_left_handed
  public :: flavor_is_right_handed
  public :: flavor_is_antiparticle
  public :: flavor_has_antiparticle
  public :: flavor_is_stable
  public :: flavor_decays_isotropically
  public :: flavor_decays_diagonal
  public :: flavor_is_polarized
  public :: flavor_get_name
  public :: flavor_get_tex_name
  public :: flavor_get_spin_type
  public :: flavor_get_multiplicity
  public :: flavor_get_isospin_type
  public :: flavor_get_charge_type
  public :: flavor_get_color_type
  public :: flavor_get_charge
  public :: flavor_get_mass
  public :: flavor_get_width
  public :: flavor_get_isospin
  public :: operator(.match.)
  public :: operator(==)
  public :: operator(/=)
  public :: operator(.merge.)
  public :: color_from_flavor
  public :: flavor_anti

  type :: flavor_t
     private
     integer :: f = UNDEFINED
     type(particle_data_t), pointer :: prt => null ()
  end type flavor_t


  interface flavor_init
     module procedure flavor_init0_empty
     module procedure flavor_init0
     module procedure flavor_init0_particle_data
     module procedure flavor_init0_model
     module procedure flavor_init0_name_model
     module procedure flavor_init1_model
     module procedure flavor_init1_name_model
     module procedure flavor_init2_model
     module procedure flavor_init_aval_model
  end interface
  interface flavor_set_model
     module procedure flavor_set_model_single
     module procedure flavor_set_model_array
  end interface
  interface operator(.match.)
     module procedure flavor_match
  end interface
  interface operator(==)
     module procedure flavor_eq
  end interface
  interface operator(/=)
     module procedure flavor_neq
  end interface
  interface operator(.merge.)
     module procedure merge_flavors0
     module procedure merge_flavors1
  end interface

  interface color_from_flavor
     module procedure color_from_flavor0
     module procedure color_from_flavor1
  end interface
       
contains

  elemental subroutine flavor_init0_empty (flv)
    type(flavor_t), intent(out) :: flv
  end subroutine flavor_init0_empty

  elemental subroutine flavor_init0 (flv, f)
    type(flavor_t), intent(out) :: flv
    integer, intent(in) :: f
    flv%f = f
  end subroutine flavor_init0

  subroutine flavor_init0_particle_data (flv, particle_data)
    type(flavor_t), intent(out) :: flv
    type(particle_data_t), intent(in), target :: particle_data
    flv%f = particle_data_get_pdg (particle_data)
    flv%prt => particle_data
  end subroutine flavor_init0_particle_data

  subroutine flavor_init0_model (flv, f, model)
    type(flavor_t), intent(out) :: flv
    integer, intent(in) :: f
    type(model_t), intent(in), target :: model
    flv%f = f
    flv%prt => model_get_particle_ptr (model, f)
  end subroutine flavor_init0_model

  subroutine flavor_init1_model (flv, f, model)
    type(flavor_t), dimension(:), intent(out) :: flv
    integer, dimension(:), intent(in) :: f
    type(model_t), intent(in), target :: model
    integer :: i
    do i = 1, size (f)
       call flavor_init0_model (flv(i), f(i), model)
    end do
  end subroutine flavor_init1_model

  subroutine flavor_init2_model (flv, f, model)
    type(flavor_t), dimension(:,:), intent(out) :: flv
    integer, dimension(:,:), intent(in) :: f
    type(model_t), intent(in), target :: model
    integer :: i
    do i = 1, size (f, 2)
       call flavor_init1_model (flv(:,i), f(:,i), model)
    end do
  end subroutine flavor_init2_model

  subroutine flavor_init0_name_model (flv, name, model)
    type(flavor_t), intent(out) :: flv
    type(string_t), intent(in) :: name
    type(model_t), intent(in), target :: model
    flv%f = model_get_particle_pdg (model, name)
    flv%prt => model_get_particle_ptr (model, flv%f)
  end subroutine flavor_init0_name_model

  subroutine flavor_init1_name_model (flv, name, model)
    type(flavor_t), dimension(:), intent(out) :: flv
    type(string_t), dimension(:), intent(in) :: name
    type(model_t), intent(in), target :: model
    integer :: i
    do i = 1, size (name)
       call flavor_init0_name_model (flv(i), name(i), model)
    end do
  end subroutine flavor_init1_name_model

  subroutine flavor_init_aval_model (flv, aval, model)
    type(flavor_t), dimension(:), intent(out), allocatable :: flv
    type(pdg_array_t), intent(in) :: aval
    type(model_t), intent(in), target :: model
    integer, dimension(:), allocatable :: pdg
    pdg = aval
    allocate (flv (size (pdg)))
    call flavor_init (flv, pdg, model)
  end subroutine flavor_init_aval_model

  elemental subroutine flavor_undefine (flv)
    type(flavor_t), intent(inout) :: flv
    flv%f = UNDEFINED
    flv%prt => null ()
  end subroutine flavor_undefine

  subroutine flavor_write (flv, unit)
    type(flavor_t), intent(in) :: flv
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (associated (flv%prt)) then
       write (u, "(A)", advance="no")  "f("
    else
       write (u, "(A)", advance="no")  "p("
    end if
    write (u, "(I0)", advance="no")  flv%f
    write (u, "(A)", advance="no")  ")"
  end subroutine flavor_write

  subroutine flavor_write_raw (flv, u)
    type(flavor_t), intent(in) :: flv
    integer, intent(in) :: u
    write (u) flv%f
  end subroutine flavor_write_raw

  subroutine flavor_read_raw (flv, u, iostat)
    type(flavor_t), intent(out) :: flv
    integer, intent(in) :: u
    integer, intent(out), optional :: iostat
    read (u, iostat=iostat) flv%f
  end subroutine flavor_read_raw

  subroutine flavor_set_model_single (flv, model)
    type(flavor_t), intent(inout) :: flv
    type(model_t), intent(in), target :: model
    if (flv%f /= UNDEFINED) &
         flv%prt => model_get_particle_ptr (model, flv%f)
  end subroutine flavor_set_model_single

  subroutine flavor_set_model_array (flv, model)
    type(flavor_t), dimension(:), intent(inout) :: flv
    type(model_t), intent(in), target :: model
    integer :: i
    do i = 1, size (flv)
       if (flv(i)%f /= UNDEFINED) &
            flv(i)%prt => model_get_particle_ptr (model, flv(i)%f)
    end do
  end subroutine flavor_set_model_array

  elemental function flavor_is_defined (flv) result (defined)
    logical :: defined
    type(flavor_t), intent(in) :: flv
    defined = flv%f /= UNDEFINED
  end function flavor_is_defined

  elemental function flavor_is_valid (flv) result (valid)
    logical :: valid
    type(flavor_t), intent(in) :: flv
    valid = flv%f /= INVALID
  end function flavor_is_valid

  elemental function flavor_is_associated (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    flag = associated (flv%prt)
  end function flavor_is_associated

  elemental function flavor_get_pdg (flv) result (f)
    integer :: f
    type(flavor_t), intent(in) :: flv
    f = flv%f
  end function flavor_get_pdg

  elemental function flavor_get_pdg_anti (flv) result (f)
    integer :: f
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (particle_data_has_antiparticle (flv%prt)) then
          f = -flv%f
       else
          f = flv%f
       end if
    else
       f = 0
    end if
  end function flavor_get_pdg_anti

  elemental function flavor_get_pdg_abs (flv) result (f)
    integer :: f
    type(flavor_t), intent(in) :: flv
    f = abs (flv%f)
  end function flavor_get_pdg_abs

  elemental function flavor_is_visible (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_is_visible (flv%prt)
    else
       flag = .false.
    end if
  end function flavor_is_visible

  elemental function flavor_is_parton (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_is_parton (flv%prt)
    else
       flag = .false.
    end if
  end function flavor_is_parton

  elemental function flavor_is_beam_remnant (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    select case (abs (flv%f))
    case (HADRON_REMNANT, &
         HADRON_REMNANT_SINGLET, HADRON_REMNANT_TRIPLET, HADRON_REMNANT_OCTET)
       flag = .true.
    case default
       flag = .false.
    end select
  end function flavor_is_beam_remnant

  elemental function flavor_is_gauge (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_is_gauge (flv%prt)
    else
       flag = .false.
    end if
  end function flavor_is_gauge

  elemental function flavor_is_left_handed (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (flv%f > 0) then
          flag = particle_data_is_left_handed (flv%prt)
       else
          flag = particle_data_is_right_handed (flv%prt)
       end if
    else
       flag = .false.
    end if
  end function flavor_is_left_handed

  elemental function flavor_is_right_handed (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (flv%f > 0) then
          flag = particle_data_is_right_handed (flv%prt)
       else
          flag = particle_data_is_left_handed (flv%prt)
       end if
    else
       flag = .false.
    end if
  end function flavor_is_right_handed

  elemental function flavor_is_antiparticle (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    flag = flv%f < 0
  end function flavor_is_antiparticle

  elemental function flavor_has_antiparticle (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_has_antiparticle (flv%prt)
    else
       flag = .false.
    end if
  end function flavor_has_antiparticle

  elemental function flavor_is_stable (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_is_stable (flv%prt, anti = flv%f < 0)
    else
       flag = .true.
    end if
  end function flavor_is_stable

  elemental function flavor_decays_isotropically (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_decays_isotropically (flv%prt, anti = flv%f < 0)
    else
       flag = .true.
    end if
  end function flavor_decays_isotropically

  elemental function flavor_decays_diagonal (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_decays_diagonal (flv%prt, anti = flv%f < 0)
    else
       flag = .true.
    end if
  end function flavor_decays_diagonal

  elemental function flavor_is_polarized (flv) result (flag)
    logical :: flag
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       flag = particle_data_is_polarized (flv%prt, anti = flv%f < 0)
    else
       flag = .false.
    end if
  end function flavor_is_polarized

  elemental function flavor_get_name (flv) result (name)
    type(string_t) :: name
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       name = particle_data_get_name (flv%prt, flv%f < 0)
    else
       name = "?"
    end if
  end function flavor_get_name
       
  elemental function flavor_get_tex_name (flv) result (name)
    type(string_t) :: name
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       name = particle_data_get_tex_name (flv%prt, flv%f < 0)
    else
       name = "?"
    end if
  end function flavor_get_tex_name
       
  elemental function flavor_get_spin_type (flv) result (type)
    integer :: type
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       type = particle_data_get_spin_type (flv%prt)
    else
       type = 1
    end if
  end function flavor_get_spin_type

  elemental function flavor_get_multiplicity (flv) result (type)
    integer :: type
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       type = particle_data_get_multiplicity (flv%prt)
    else
       type = 1
    end if
  end function flavor_get_multiplicity

  elemental function flavor_get_isospin_type (flv) result (type)
    integer :: type
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       type = particle_data_get_isospin_type (flv%prt)
    else
       type = 1
    end if
  end function flavor_get_isospin_type

  elemental function flavor_get_charge_type (flv) result (type)
    integer :: type
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       type = particle_data_get_charge_type (flv%prt)
    else
       type = 1
    end if
  end function flavor_get_charge_type

  elemental function flavor_get_color_type (flv) result (type)
    integer :: type
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (flavor_is_antiparticle (flv)) then
          type = - particle_data_get_color_type (flv%prt)
       else
          type = particle_data_get_color_type (flv%prt)
       end if
    else
       type = 1
    end if
  end function flavor_get_color_type

  elemental function flavor_get_charge (flv) result (charge)
    real(default) :: charge
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (flavor_is_antiparticle (flv)) then
          charge = particle_data_get_charge (flv%prt)
       else
          charge = - particle_data_get_charge (flv%prt)
       end if
    else
       charge = 0
    end if
  end function flavor_get_charge

  elemental function flavor_get_mass (flv) result (mass)
    real(default) :: mass
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       mass = particle_data_get_mass (flv%prt)
    else
       mass = 0
    end if
  end function flavor_get_mass

  elemental function flavor_get_width (flv) result (width)
    real(default) :: width
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       width = particle_data_get_width (flv%prt)
    else
       width = 0
    end if
  end function flavor_get_width

  elemental function flavor_get_isospin (flv) result (isospin)
    real(default) :: isospin
    type(flavor_t), intent(in) :: flv
    if (associated (flv%prt)) then
       if (flavor_is_antiparticle (flv)) then
          isospin = particle_data_get_isospin (flv%prt)
       else
          isospin = - particle_data_get_isospin (flv%prt)
       end if
    else
       isospin = 0
    end if
  end function flavor_get_isospin

  elemental function flavor_match (flv1, flv2) result (eq)
    logical :: eq
    type(flavor_t), intent(in) :: flv1, flv2
    if (flv1%f /= UNDEFINED .and. flv2%f /= UNDEFINED) then
       eq = flv1%f == flv2%f
    else
       eq = .true.
    end if
  end function flavor_match

  elemental function flavor_eq (flv1, flv2) result (eq)
    logical :: eq
    type(flavor_t), intent(in) :: flv1, flv2
    if (flv1%f /= UNDEFINED .and. flv2%f /= UNDEFINED) then
       eq = flv1%f == flv2%f
    else if (flv1%f == UNDEFINED .and. flv2%f == UNDEFINED) then
       eq = .true.
    else
       eq = .false.
    end if
  end function flavor_eq

  elemental function flavor_neq (flv1, flv2) result (neq)
    logical :: neq
    type(flavor_t), intent(in) :: flv1, flv2
    if (flv1%f /= UNDEFINED .and. flv2%f /= UNDEFINED) then
       neq = flv1%f /= flv2%f
    else if (flv1%f == UNDEFINED .and. flv2%f == UNDEFINED) then
       neq = .false.
    else
       neq = .true.
    end if
  end function flavor_neq

  function merge_flavors0 (flv1, flv2) result (flv)
    type(flavor_t) :: flv
    type(flavor_t), intent(in) :: flv1, flv2
    if (flavor_is_defined (flv1) .and. flavor_is_defined (flv2)) then
       if (flv1 == flv2) then
          flv = flv1
       else
          flv%f = INVALID
       end if
    else if (flavor_is_defined (flv1)) then
       flv = flv1
    else if (flavor_is_defined (flv2)) then
       flv = flv2
    end if
  end function merge_flavors0

  function merge_flavors1 (flv1, flv2) result (flv)
    type(flavor_t), dimension(:), intent(in) :: flv1, flv2
    type(flavor_t), dimension(size(flv1)) :: flv
    integer :: i
    do i = 1, size (flv1)
       flv(i) = flv1(i) .merge. flv2(i)
    end do
  end function merge_flavors1

  function color_from_flavor0 (flv, c_seed, reverse) result (col)
    type(color_t) :: col
    type(flavor_t), intent(in) :: flv
    integer, intent(in), optional :: c_seed
    logical, intent(in), optional :: reverse
    integer, save :: c = 1
    logical :: rev
    if (present (c_seed))  c = c_seed
    rev = .false.;  if (present (reverse)) rev = reverse
    select case (flavor_get_color_type (flv))
    case (1)
    case (3)
       call color_init (col, (/ c /));  c = c + 1
    case (-3)
       call color_init (col, (/-c /));  c = c + 1
    case (8)
       if (rev) then
          call color_init (col, (/ c+1, -c /));  c = c + 2
       else
          call color_init (col, (/ c, -(c+1) /));  c = c + 2
       end if
    end select
  end function color_from_flavor0

  function color_from_flavor1 (flv, c_seed, reverse) result (col)
    type(flavor_t), dimension(:), intent(in) :: flv
    integer, intent(in), optional :: c_seed
    logical, intent(in), optional :: reverse
    type(color_t), dimension(size(flv)) :: col
    integer :: i
    col(1) = color_from_flavor0 (flv(1), c_seed, reverse)
    do i = 2, size (flv)
       col(i) = color_from_flavor0 (flv(i), reverse=reverse)
    end do
  end function color_from_flavor1

  function flavor_anti (flv) result (aflv)
    type(flavor_t) :: aflv
    type(flavor_t), intent(in) :: flv
    if (flavor_has_antiparticle (flv)) then
       aflv%f = - flv%f
    else
       aflv%f = flv%f
    end if
    aflv%prt => flv%prt
  end function flavor_anti


end module flavors
