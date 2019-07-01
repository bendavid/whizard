! WHIZARD 2.1.0 June 15 2012
! 
! Copyright (C) 1999-2012 by 
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

module models

  use iso_c_binding !NODEP!
  use kinds, only: default !NODEP!
  use kinds, only: i8, i32 !NODEP!
  use kinds, only: c_default_float !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: VERTEX_TABLE_SCALE_FACTOR !NODEP!
  use file_utils !NODEP!
  use md5
  use os_interface
  use hashes, only: hash
  use diagnostics !NODEP!
  use ifiles
  use syntax_rules
  use lexers
  use parser
  use pdg_arrays
  use variables
  use expressions

  implicit none
  private

  public :: parameter_t
  public :: particle_data_t
  public :: particle_data_init
  public :: particle_data_set
  public :: particle_data_freeze
  public :: particle_data_get_pdg
  public :: particle_data_get_pdg_anti
  public :: particle_data_is_visible
  public :: particle_data_is_parton
  public :: particle_data_is_gauge
  public :: particle_data_is_left_handed
  public :: particle_data_is_right_handed
  public :: particle_data_has_antiparticle
  public :: particle_data_is_stable
  public :: particle_data_decays_isotropically
  public :: particle_data_decays_diagonal
  public :: particle_data_is_polarized
  public :: particle_data_get_name
  public :: particle_data_get_tex_name
  public :: particle_data_get_spin_type
  public :: particle_data_get_multiplicity
  public :: particle_data_get_isospin_type
  public :: particle_data_get_charge_type
  public :: particle_data_get_color_type
  public :: particle_data_get_charge
  public :: particle_data_get_mass
  public :: particle_data_get_mass_sign
  public :: particle_data_get_width
  public :: particle_data_get_isospin
  public :: model_t
  public :: model_final
  public :: model_write
  public :: model_get_name
  public :: model_get_md5sum
  public :: model_get_parameters_md5sum
  public :: model_get_polarized_md5sum
  public :: model_get_parameter_value
  public :: model_get_n_parameters
  public :: model_parameters_to_array
  public :: model_parameters_update
  public :: model_get_particle_ptr
  public :: model_test_particle
  public :: model_set_particle_mass
  public :: model_set_particle_width
  public :: model_get_particle_pdg
  public :: model_get_var_list_ptr
  public :: model_match_vertex
  public :: model_check_vertex
  public :: syntax_model_file_init
  public :: syntax_model_file_final
  public :: syntax_model_file_write
  public :: model_read
  public :: model_list_write
  public :: model_list_read_model
  public :: model_list_model_exists
  public :: model_list_get_model_ptr
  public :: model_list_final
  public :: models_test

  integer, parameter :: PAR_NONE = 0
  integer, parameter :: PAR_INDEPENDENT = 1, PAR_DERIVED = 2
  integer, parameter :: PAR_EXTERNAL = 3


  type :: parameter_t
     private
     integer :: type  = PAR_NONE
     type(string_t) :: name
     real(default) :: value = 0
     type(eval_tree_t) :: eval_tree
  end type parameter_t

  integer, parameter, public :: ELECTRON = 11

  integer, parameter, public :: GLUON = 21
  integer, parameter, public :: PHOTON = 22
  integer, parameter, public :: Z_BOSON = 23
  integer, parameter, public :: W_BOSON = 24

  integer, parameter, public :: PROTON = 2212 
  integer, parameter, public :: PION = 111
  integer, parameter, public :: PIPLUS = 211
  integer, parameter, public :: PIMINUS = - PIPLUS

  integer, parameter, public :: HADRON_REMNANT = 90
  integer, parameter, public :: HADRON_REMNANT_SINGLET = 91
  integer, parameter, public :: HADRON_REMNANT_TRIPLET = 92
  integer, parameter, public :: HADRON_REMNANT_OCTET = 93

  integer, parameter, public :: PRT_ANY = 81
  integer, parameter, public :: PRT_VISIBLE = 82
  integer, parameter, public :: PRT_CHARGED = 83
  integer, parameter, public :: PRT_COLORED = 84

  integer, parameter, public :: INVALID = 97
  integer, parameter, public :: KEYSTONE = 98
  integer, parameter, public :: COMPOSITE = 99

  integer, parameter, public :: UNKNOWN=0
  integer, parameter, public :: SCALAR=1, SPINOR=2, VECTOR=3, &
        VECTORSPINOR=4, TENSOR=5
  type :: particle_data_t
     private
     type(string_t) :: longname
     integer :: pdg = UNDEFINED
     logical :: is_visible = .true.
     logical :: is_parton = .false.
     logical :: is_gauge = .false.
     logical :: is_left_handed = .false.
     logical :: is_right_handed = .false.
     logical :: has_antiparticle = .false.
     logical :: p_is_stable = .true.
     logical :: p_decays_isotropically = .false.
     logical :: p_decays_diagonal = .false.
     logical :: a_is_stable = .true.
     logical :: a_decays_isotropically = .false.
     logical :: a_decays_diagonal = .false.
     logical :: p_polarized = .false.
     logical :: a_polarized = .false.
     type(string_t), dimension(:), allocatable :: name, anti
     type(string_t) :: tex_name, tex_anti
     integer :: spin_type = UNDEFINED
     integer :: isospin_type = 1
     integer :: charge_type = 1
     integer :: color_type = 1
     real(default), pointer :: mass_val => null ()
     type(parameter_t), pointer :: mass_src => null ()
     real(default), pointer :: width_val => null ()
     type(parameter_t), pointer :: width_src => null ()
     integer :: multiplicity = 1
  end type particle_data_t

  type :: particle_p
     type(particle_data_t), pointer :: p => null ()
  end type particle_p

  type :: vertex_t
     logical :: trilinear
     integer, dimension(:), allocatable :: pdg
     type(particle_p), dimension(:), allocatable :: prt
  end type vertex_t

  type :: vertex_table_entry_t
     integer :: pdg1 = 0, pdg2 = 0
     integer :: n = 0
     integer, dimension(:), allocatable :: pdg3
  end type vertex_table_entry_t

  type :: vertex_table_t
     type(vertex_table_entry_t), dimension(:), allocatable :: entry
     integer :: n_collisions = 0
     integer(i32) :: mask
  end type vertex_table_t

  type :: model_t
     private
     type(string_t) :: name
     character(32) :: md5sum = ""
     type(parameter_t), dimension(:), allocatable :: par
     type(particle_data_t), dimension(:), allocatable :: prt
     type(vertex_t), dimension(:), allocatable :: vtx
     type(vertex_table_t) :: vt
     type(var_list_t) :: var_list
     type(string_t) :: dlname
     procedure(model_init_external_parameters), nopass, pointer :: &
          init_external_parameters => null ()
     type(dlaccess_t) :: dlaccess
  end type model_t

  type :: model_entry_t
     type(model_t) :: model
     type(model_entry_t), pointer :: next => null ()
  end type model_entry_t

  type :: model_list_t
     type(model_entry_t), pointer :: first => null ()
     type(model_entry_t), pointer :: last => null ()
  end type model_list_t


  abstract interface
     subroutine model_init_external_parameters (par) bind (C)
       import
       real(c_default_float), dimension(*), intent(inout) :: par
     end subroutine model_init_external_parameters
  end interface

  interface model_set_parameter
     module procedure model_set_parameter_constant
     module procedure model_set_parameter_parse_node
  end interface

  interface model_set_vertex
     module procedure model_set_vertex_pdg
     module procedure model_set_vertex_names
  end interface

  type(syntax_t), target, save :: syntax_model_file

  type(model_list_t), target, save :: model_list


contains

  subroutine parameter_init_independent_value (par, name, value)
    type(parameter_t), intent(out) :: par
    type(string_t), intent(in) :: name
    real(default), intent(in) :: value
    par%type = PAR_INDEPENDENT
    par%name = name
    par%value = value
  end subroutine parameter_init_independent_value

  subroutine parameter_init_independent (par, name, pn)
    type(parameter_t), intent(out) :: par
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    par%type = PAR_INDEPENDENT
    par%name = name
    call eval_tree_init_numeric_value (par%eval_tree, pn)
    par%value = eval_tree_get_real (par%eval_tree)
  end subroutine parameter_init_independent

  subroutine parameter_init_derived (par, name, pn, var_list)
    type(parameter_t), intent(out) :: par
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    type(var_list_t), intent(in), target :: var_list
    par%type = PAR_DERIVED
    par%name = name
    call eval_tree_init_expr (par%eval_tree, pn, var_list=var_list)
    call parameter_reset_derived (par)
  end subroutine parameter_init_derived

  subroutine parameter_init_external (par, name)
    type(parameter_t), intent(out) :: par
    type(string_t), intent(in) :: name
    par%type = PAR_EXTERNAL
    par%name = name
  end subroutine parameter_init_external

  subroutine parameter_final (par)
    type(parameter_t), intent(inout) :: par
    call eval_tree_final (par%eval_tree)
  end subroutine parameter_final

  subroutine parameter_reset_derived (par)
    type(parameter_t), intent(inout) :: par
    select case (par%type)
    case (PAR_DERIVED)
       call eval_tree_evaluate (par%eval_tree)
       par%value = eval_tree_get_real (par%eval_tree)
    end select
  end subroutine parameter_reset_derived

  function parameter_get_value_ptr (par) result (val)
    real(default), pointer :: val
    type(parameter_t), intent(in), target :: par
    val => par%value
  end function parameter_get_value_ptr

  subroutine parameter_write (par, unit, write_defs)
    type(parameter_t), intent(in) :: par
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: write_defs
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(3x,A)", advance="no")  "parameter"
    write (u, "(1x,A,1x,A)", advance="no")  char (par%name), "="
    write (u, "(G17.10)", advance="no") par%value
    select case (par%type)
    case (PAR_DERIVED)
       write (u, *) "  ! derived"
       if (present (write_defs)) then
          if (write_defs) then
             call eval_tree_write (par%eval_tree, unit)
          end if
       end if
    case (PAR_EXTERNAL)
       write (u, *) "  ! external"
    end select
  end subroutine parameter_write

  subroutine particle_data_init (prt, longname, pdg)
    type(particle_data_t), intent(out) :: prt
    type(string_t), intent(in) :: longname
    integer, intent(in) :: pdg
    prt%longname = longname
    prt%pdg = pdg
    prt%tex_name = ""
    prt%tex_anti = ""
  end subroutine particle_data_init
    
  subroutine particle_data_copy (prt, prt_src)
    type(particle_data_t), intent(inout) :: prt
    type(particle_data_t), intent(in) :: prt_src
    prt%is_visible = prt_src%is_visible
    prt%is_parton = prt_src%is_parton
    prt%is_gauge = prt_src%is_gauge
    prt%is_left_handed = prt_src%is_left_handed
    prt%is_right_handed = prt_src%is_right_handed
    prt%spin_type = prt_src%spin_type
    prt%isospin_type = prt_src%isospin_type
    prt%charge_type = prt_src%charge_type
    prt%color_type = prt_src%color_type
    call particle_data_set_multiplicity (prt)
  end subroutine particle_data_copy

  subroutine particle_data_set (prt, &
       is_visible, is_parton, is_gauge, is_left_handed, is_right_handed, &
       p_is_stable, p_decays_isotropically, p_decays_diagonal, &
       a_is_stable, a_decays_isotropically, a_decays_diagonal, &
       p_polarized, a_polarized, &
       name, anti, tex_name, tex_anti, &
       spin_type, isospin_type, charge_type, color_type, &
       mass_src, width_src)
    type(particle_data_t), intent(inout) :: prt
    logical, intent(in), optional :: is_visible, is_parton, is_gauge
    logical, intent(in), optional :: is_left_handed, is_right_handed
    logical, intent(in), optional :: p_is_stable
    logical, intent(in), optional :: p_decays_isotropically, p_decays_diagonal
    logical, intent(in), optional :: a_is_stable
    logical, intent(in), optional :: a_decays_isotropically, a_decays_diagonal
    logical, intent(in), optional :: p_polarized, a_polarized
    type(string_t), dimension(:), intent(in), optional :: name, anti
    type(string_t), intent(in), optional :: tex_name, tex_anti
    integer, intent(in), optional :: spin_type, isospin_type
    integer, intent(in), optional :: charge_type, color_type
    type(parameter_t), intent(in), target, optional :: mass_src, width_src
    if (present (is_visible))  prt%is_visible = is_visible
    if (present (is_parton))  prt%is_parton = is_parton
    if (present (is_gauge))  prt%is_gauge = is_gauge
    if (present (is_left_handed))  prt%is_left_handed = is_left_handed
    if (present (is_right_handed))  prt%is_right_handed = is_right_handed
    if (present (p_is_stable))  prt%p_is_stable = p_is_stable
    if (present (p_decays_isotropically)) &
          prt%p_decays_isotropically = p_decays_isotropically
    if (present (p_decays_diagonal)) &
          prt%p_decays_diagonal = p_decays_diagonal
    if (present (a_is_stable))  prt%a_is_stable = a_is_stable
    if (present (a_decays_isotropically)) &
          prt%a_decays_isotropically = a_decays_isotropically
    if (present (a_decays_diagonal)) &
          prt%a_decays_diagonal = a_decays_diagonal
    if (present (p_polarized)) prt%p_polarized = p_polarized
    if (present (a_polarized)) prt%a_polarized = a_polarized
    if (present (name)) then
       allocate (prt%name (size (name)))
       prt%name = name
    end if
    if (present (anti)) then
       allocate (prt%anti (size (anti)))
       prt%anti = anti
       prt%has_antiparticle = .true.
    end if
    if (present (tex_name))  prt%tex_name = tex_name
    if (present (tex_anti))  prt%tex_anti = tex_anti
    if (present (spin_type))  prt%spin_type = spin_type
    if (present (isospin_type))  prt%isospin_type = isospin_type
    if (present (charge_type))  prt%charge_type = charge_type
    if (present (color_type))  prt%color_type = color_type
    if (present (mass_src)) then
       prt%mass_src => mass_src
       prt%mass_val => parameter_get_value_ptr (mass_src)
    end if
    if (present (width_src)) then
       prt%width_src => width_src
       prt%width_val => parameter_get_value_ptr (width_src)
    end if
    if (present (spin_type) .or. present (mass_src)) then
       call particle_data_set_multiplicity (prt)
    end if
  end subroutine particle_data_set

  subroutine particle_data_set_multiplicity (prt)
    type(particle_data_t), intent(inout) :: prt
    if (prt%spin_type /= SCALAR) then
       if (associated (prt%mass_src)) then
          prt%multiplicity = prt%spin_type
       else if (prt%is_left_handed .or. prt%is_right_handed) then
          prt%multiplicity = 1
       else
          prt%multiplicity = 2
       end if
    end if
  end subroutine particle_data_set_multiplicity

  subroutine particle_data_set_mass (prt, mass)
    type(particle_data_t), intent(inout) :: prt
    real(default), intent(in) :: mass
    if (associated (prt%mass_val))  prt%mass_val = mass
  end subroutine particle_data_set_mass
    
  subroutine particle_data_set_width (prt, width)
    type(particle_data_t), intent(inout) :: prt
    real(default), intent(in) :: width
    if (associated (prt%width_val))  prt%width_val = width
  end subroutine particle_data_set_width
    
  subroutine particle_data_freeze (prt)
    type(particle_data_t), intent(inout) :: prt
    if (.not. allocated (prt%name))  allocate (prt%name (0))
    if (.not. allocated (prt%anti))  allocate (prt%anti (0))
  end subroutine particle_data_freeze

  subroutine particle_data_write (prt, unit)
    type(particle_data_t), intent(in) :: prt
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(3x,A,1x,A)", advance="no") "particle", char (prt%longname)
    write (u, "(1x,I7)", advance="no") prt%pdg
    if (.not. prt%is_visible) write (u, "(3x,A)", advance="no") "invisible"
    if (prt%is_parton)  write (u, "(3x,A)", advance="no") "parton"
    if (prt%is_gauge)  write (u, "(3x,A)", advance="no") "gauge"
    if (prt%is_left_handed)  write (u, "(3x,A)", advance="no") "left"
    if (prt%is_right_handed)  write (u, "(3x,A)", advance="no") "right"
    write (u, *)
    write (u, "(5x,A)", advance="no") "name"
    if (allocated (prt%name)) then
       do i = 1, size (prt%name)
          write (u, "(1x,A)", advance="no")  '"' // char (prt%name(i)) // '"'
       end do
       write (u, *)
       if (prt%has_antiparticle) then
          write (u, "(5x,A)", advance="no") "anti"
          do i = 1, size (prt%anti)
             write (u, "(1x,A)", advance="no")  '"' // char (prt%anti(i)) // '"'
          end do
          write (u, *)
       end if
       if (prt%tex_name /= "") then
          write (u, "(5x,A)")  &
               "tex_name " // '"' // char (prt%tex_name) // '"'
       end if
       if (prt%has_antiparticle .and. prt%tex_anti /= "") then
          write (u, "(5x,A)")  &
               "tex_anti " // '"' // char (prt%tex_anti) // '"'
       end if
    else
       write (u, "(A)") "???"
    end if
    write (u, "(5x,A)", advance="no") "spin "
    select case (mod (prt%spin_type - 1, 2))
    case (0);  write (u, "(I1)", advance="no") (prt%spin_type-1) / 2
    case default;  write (u, "(I1,A)", advance="no") prt%spin_type-1, "/2"
    end select
!    write (u, "(3x,A,I1,A)") "! [multiplicity = ", prt%multiplicity, "]"
    if (abs (prt%isospin_type) /= 1) then
       write (u, "(5x,A)", advance="no") "isospin "
       select case (mod (abs (prt%isospin_type) - 1, 2))
       case (0);  write (u, "(I2)", advance="no") &
            sign (abs (prt%isospin_type) - 1, prt%isospin_type) / 2
       case default;  write (u, "(I2,A)", advance="no") &
            sign (abs (prt%isospin_type) - 1, prt%isospin_type), "/2"
       end select
    end if
    if (abs (prt%charge_type) /= 1) then
       write (u, "(5x,A)", advance="no") "charge "
       select case (mod (abs (prt%charge_type) - 1, 3))
       case (0);  write (u, "(I2)", advance="no") &
            sign (abs (prt%charge_type) - 1, prt%charge_type) / 3
       case default;  write (u, "(I2,A)", advance="no") &
            sign (abs (prt%charge_type) - 1, prt%charge_type), "/3"
       end select
    end if
    if (prt%color_type /= 1) then
       write (u, "(5x,A,I2)", advance="no") "color ", prt%color_type
    end if
    write (u, *)
    if (associated (prt%mass_src)) then
       write (u, "(5x,A)") "mass " // char (prt%mass_src%name)
       if (associated (prt%width_src)) then
          write (u, "(5x,A)") "width " // char (prt%width_src%name)
       end if
    end if
  end subroutine particle_data_write

  elemental function particle_data_get_pdg (prt) result (pdg)
    integer :: pdg
    type(particle_data_t), intent(in) :: prt
    pdg = prt%pdg
  end function particle_data_get_pdg

  elemental function particle_data_get_pdg_anti (prt) result (pdg)
    integer :: pdg
    type(particle_data_t), intent(in) :: prt
    if (prt%has_antiparticle) then
       pdg = - prt%pdg
    else
       pdg = prt%pdg
    end if
  end function particle_data_get_pdg_anti

  elemental function particle_data_is_visible (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%is_visible
  end function particle_data_is_visible

  elemental function particle_data_is_parton (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%is_parton
  end function particle_data_is_parton

  elemental function particle_data_is_gauge (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%is_gauge
  end function particle_data_is_gauge

  elemental function particle_data_is_left_handed (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%is_left_handed
  end function particle_data_is_left_handed

  elemental function particle_data_is_right_handed (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%is_right_handed
  end function particle_data_is_right_handed

  elemental function particle_data_has_antiparticle (prt) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    flag = prt%has_antiparticle
  end function particle_data_has_antiparticle

  elemental function particle_data_is_stable (prt, anti) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    logical, intent(in), optional :: anti
    if (present (anti)) then
       if (anti) then
          flag = prt%a_is_stable
       else
          flag = prt%p_is_stable
       end if
    else
       flag = prt%p_is_stable
    end if
  end function particle_data_is_stable

  elemental function particle_data_decays_isotropically &
       (prt, anti) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    logical, intent(in), optional :: anti
    if (present (anti)) then
       if (anti) then
          flag = prt%a_decays_isotropically
       else
          flag = prt%p_decays_isotropically
       end if
    else
       flag = prt%p_decays_isotropically
    end if
  end function particle_data_decays_isotropically

  elemental function particle_data_decays_diagonal &
       (prt, anti) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    logical, intent(in), optional :: anti
    if (present (anti)) then
       if (anti) then
          flag = prt%a_decays_diagonal
       else
          flag = prt%p_decays_diagonal
       end if
    else
       flag = prt%p_decays_diagonal
    end if
  end function particle_data_decays_diagonal

  elemental function particle_data_is_polarized (prt, anti) result (flag)
    logical :: flag
    type(particle_data_t), intent(in) :: prt
    logical, intent(in), optional :: anti
    logical :: a
    if (present (anti)) then
       a = anti
    else
       a = .false.
    end if
    if (a) then
       flag = prt%a_polarized
    else
       flag = prt%p_polarized
    end if
  end function particle_data_is_polarized
       
  elemental function particle_data_get_name &
       (prt, is_antiparticle) result (name)
    type(string_t) :: name
    type(particle_data_t), intent(in) :: prt
    logical, intent(in) :: is_antiparticle
    name = "???"
    if (is_antiparticle) then
       if (prt%has_antiparticle) then
          if (allocated (prt%anti)) then
             if (size(prt%anti) > 0) name = prt%anti(1)
          end if
       else
          if (allocated (prt%name)) then
             if (size (prt%name) > 0) name = prt%name(1)
          end if
       end if
    else
       if (allocated (prt%name)) then
          if (size (prt%name) > 0) name = prt%name(1)
       end if
    end if
  end function particle_data_get_name

  elemental function particle_data_get_tex_name &
       (prt, is_antiparticle) result (name)
    type(string_t) :: name
    type(particle_data_t), intent(in) :: prt
    logical, intent(in) :: is_antiparticle
    if (is_antiparticle) then
       if (prt%has_antiparticle) then
          name = prt%tex_anti
       else
          name = prt%tex_name
       end if
    else
       name = prt%tex_name
    end if
    if (name == "")  name = particle_data_get_name (prt, is_antiparticle)
  end function particle_data_get_tex_name

  elemental function particle_data_get_spin_type (prt) result (type)
    integer :: type
    type(particle_data_t), intent(in) :: prt
    type = prt%spin_type
  end function particle_data_get_spin_type

  elemental function particle_data_get_multiplicity (prt) result (type)
    integer :: type
    type(particle_data_t), intent(in) :: prt
    type = prt%multiplicity
  end function particle_data_get_multiplicity

  elemental function particle_data_get_isospin_type (prt) result (type)
    integer :: type
    type(particle_data_t), intent(in) :: prt
    type = prt%isospin_type
  end function particle_data_get_isospin_type

  elemental function particle_data_get_charge_type (prt) result (type)
    integer :: type
    type(particle_data_t), intent(in) :: prt
    type = prt%charge_type
  end function particle_data_get_charge_type

  elemental function particle_data_get_color_type (prt) result (type)
    integer :: type
    type(particle_data_t), intent(in) :: prt
    type = prt%color_type
  end function particle_data_get_color_type

  elemental function particle_data_get_charge (prt) result (charge)
    real(default) :: charge
    type(particle_data_t), intent(in) :: prt
    if (prt%charge_type /= 0) then
       charge = real (sign ((abs(prt%charge_type) - 1), &
             prt%charge_type), default) / 3
    else
       charge = 0
    end if
  end function particle_data_get_charge

  elemental function particle_data_get_mass (prt) result (mass)
    real(default) :: mass
    type(particle_data_t), intent(in) :: prt
    if (associated (prt%mass_val)) then
       mass = abs (prt%mass_val)
    else
       mass = 0
    end if
  end function particle_data_get_mass

  elemental function particle_data_get_mass_sign (prt) result (sgn)
    integer :: sgn
    type(particle_data_t), intent(in) :: prt
    if (associated (prt%mass_val)) then
       sgn = sign (1._default, prt%mass_val)
    else
       sgn = 0
    end if
  end function particle_data_get_mass_sign

  elemental function particle_data_get_width (prt) result (width)
    real(default) :: width
    type(particle_data_t), intent(in) :: prt
    if (associated (prt%width_val)) then
       width = prt%width_val
    else
       width = 0
    end if
  end function particle_data_get_width
  
  elemental function particle_data_get_isospin (prt) result (isospin)
    real(default) :: isospin
    type(particle_data_t), intent(in) :: prt
    if (prt%isospin_type /= 0) then
       isospin = real (sign (abs(prt%isospin_type) - 1, &
            prt%isospin_type), default) / 2
    else
       isospin = 0
    end if
  end function particle_data_get_isospin

  function particle_data_get_charged_pdg (prt) result (aval)
    type(pdg_array_t) :: aval, aval_p, aval_a
    type(particle_data_t), dimension(:), intent(in) :: prt
    aval_p = pack ( prt%pdg, abs (prt%charge_type) > 1) 
    aval_a = pack (-prt%pdg, abs (prt%charge_type) > 1 &
         .and. prt%has_antiparticle ) 
    aval = aval_p // aval_a
  end function particle_data_get_charged_pdg

  function particle_data_get_colored_pdg (prt) result (aval)
    type(pdg_array_t) :: aval, aval_p, aval_a
    type(particle_data_t), dimension(:), intent(in) :: prt
    aval_p = pack ( prt%pdg, abs (prt%color_type) > 1) 
    aval_a = pack (-prt%pdg, abs (prt%color_type) > 1 & 
         .and. prt%has_antiparticle )  
    aval = aval_p // aval_a
  end function particle_data_get_colored_pdg

  subroutine vertex_init (vtx, pdg, model)
    type(vertex_t), intent(out) :: vtx
    integer, dimension(:), intent(in) :: pdg
    type(model_t), intent(in), target, optional :: model
    integer :: i
    allocate (vtx%pdg (size (pdg)))
    allocate (vtx%prt (size (pdg)))
    vtx%trilinear = size (pdg) == 3
    vtx%pdg = pdg
    if (present (model)) then
       do i = 1, size (pdg)
          vtx%prt(i)%p => model_get_particle_ptr (model, pdg(i))
       end do
    end if
  end subroutine vertex_init

  subroutine vertex_write (vtx, unit)
    type(vertex_t), intent(in) :: vtx
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(3x,A)", advance="no")  "vertex"
    do i = 1, size (vtx%prt)
       if (associated (vtx%prt(i)%p)) then
          write (u, "(1x,A)", advance="no") &
               '"' // char (particle_data_get_name &
                              (vtx%prt(i)%p, vtx%pdg(i) < 0)) &
                   // '"'
       else
          write (u, "(1x,I7)", advance="no") vtx%pdg(i)
       end if
    end do
    write (u, *)
  end subroutine vertex_write

  function vertex_table_size (n_vtx) result (n)
    integer(i32) :: n
    integer, intent(in) :: n_vtx
    integer :: i, s
    s = VERTEX_TABLE_SCALE_FACTOR * n_vtx
    n = 1
    do i = 1, 31
       n = ishft (n, 1)
       s = ishft (s,-1)
       if (s == 0)  exit
    end do
  end function vertex_table_size

  function hash2 (pdg1, pdg2)
    integer(i32) :: hash2
    integer, intent(in) :: pdg1, pdg2
    integer(i8), dimension(1) :: mold
    hash2 = hash (transfer ((/pdg1, pdg2/), mold))
  end function hash2

  subroutine vertex_table_init (vt, prt, vtx)
    type(vertex_table_t), intent(out) :: vt
    type(particle_data_t), dimension(:), intent(in) :: prt
    type(vertex_t), dimension(:), intent(in) :: vtx
    integer :: n_prt, n_vtx, vt_size, i, p1, p2, p3
    integer, dimension(3) :: p
    n_prt = size (prt)
    n_vtx = size (vtx)
    vt_size = vertex_table_size (count (vtx%trilinear))
    vt%mask = vt_size - 1
    allocate (vt%entry (0:vt_size-1))
    do i = 1, n_vtx
       if (vtx(i)%trilinear) then
          p = vtx(i)%pdg
          p1 = p(1);  p2 = p(2)
          call create (hash2 (p1, p2))
          if (p(2) /= p(3)) then
             p2 = p(3)
             call create (hash2 (p1, p2))
          end if
          if (p(1) /= p(2)) then
             p1 = p(2);  p2 = p(1)
             call create (hash2 (p1, p2))
             if (p(1) /= p(3)) then
                p2 = p(3)
                call create (hash2 (p1, p2))
             end if
          end if
          if (p(1) /= p(3)) then
             p1 = p(3);  p2 = p(1)
             call create (hash2 (p1, p2))
             if (p(1) /= p(2)) then
                p2 = p(2)
                call create (hash2 (p1, p2))
             end if
          end if
       end if
    end do
    do i = 0, vt_size - 1
       allocate (vt%entry(i)%pdg3 (vt%entry(i)%n))
    end do
    vt%entry%n = 0
    do i = 1, n_vtx
       if (vtx(i)%trilinear) then
          p = vtx(i)%pdg
          p1 = p(1);  p2 = p(2);  p3 = p(3)
          call register (hash2 (p1, p2))
          if (p(2) /= p(3)) then
             p2 = p(3);  p3 = p(2)
             call register (hash2 (p1, p2))
          end if
          if (p(1) /= p(2)) then
             p1 = p(2);  p2 = p(1);  p3 = p(3)
             call register (hash2 (p1, p2))
             if (p(1) /= p(3)) then
                p2 = p(3);  p3 = p(1)
                call register (hash2 (p1, p2))
             end if
          end if
          if (p(1) /= p(3)) then
             p1 = p(3);  p2 = p(1);  p3 = p(2)
             call register (hash2 (p1, p2))
             if (p(1) /= p(2)) then
                p2 = p(2);  p3 = p(1)
                call register (hash2 (p1, p2))
             end if
          end if
       end if
    end do
  contains
    recursive subroutine create (hashval)
      integer(i32), intent(in) :: hashval
      integer :: h
      h = iand (hashval, vt%mask)
      if (vt%entry(h)%n == 0) then
         vt%entry(h)%pdg1 = p1
         vt%entry(h)%pdg2 = p2
         vt%entry(h)%n = 1
      else if (vt%entry(h)%pdg1 == p1 .and. vt%entry(h)%pdg2 == p2) then
         vt%entry(h)%n = vt%entry(h)%n + 1
      else
         vt%n_collisions = vt%n_collisions + 1
         call create (hashval + 1)
      end if
    end subroutine create
    recursive subroutine register (hashval)
      integer(i32), intent(in) :: hashval
      integer :: h
      h = iand (hashval, vt%mask)
      if (vt%entry(h)%pdg1 == p1 .and. vt%entry(h)%pdg2 == p2) then
         vt%entry(h)%n = vt%entry(h)%n + 1
         vt%entry(h)%pdg3(vt%entry(h)%n) = p3
      else
         call register (hashval + 1)
      end if
    end subroutine register
  end subroutine vertex_table_init

  subroutine vertex_table_write (vt, unit)
    type(vertex_table_t), intent(in) :: vt
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "vertex hash table:"
    write (u, *) "  size = ", size (vt%entry)
    write (u, *) "  used = ", count (vt%entry%n /= 0)
    write (u, *) "  coll = ", vt%n_collisions
    do i = lbound (vt%entry, 1), ubound (vt%entry, 1)
       if (vt%entry(i)%n /= 0) then
          write (u, *)  "  ", i, ":", &
               vt%entry(i)%pdg1, vt%entry(i)%pdg2, "->", vt%entry(i)%pdg3
       end if
    end do
  end subroutine vertex_table_write

  subroutine vertex_table_match (vt, pdg1, pdg2, pdg3)
    type(vertex_table_t), intent(in) :: vt
    integer, intent(in) :: pdg1, pdg2
    integer, dimension(:), allocatable, intent(out) :: pdg3
    integer :: vt_size
    vt_size = size (vt%entry)
    call match (hash2 (pdg1, pdg2))
  contains
    recursive subroutine match (hashval)
      integer(i32), intent(in) :: hashval
      integer :: h
      h = iand (hashval, vt%mask)
      if (vt%entry(h)%n == 0) then
         allocate (pdg3 (0))
      else if (vt%entry(h)%pdg1 == pdg1 .and. vt%entry(h)%pdg2 == pdg2) then
         allocate (pdg3 (size (vt%entry(h)%pdg3)))
         pdg3 = vt%entry(h)%pdg3
      else
         call match (hashval + 1)
      end if
    end subroutine match
  end subroutine vertex_table_match

  function vertex_table_check (vt, pdg1, pdg2, pdg3) result (flag)
    type(vertex_table_t), intent(in) :: vt
    integer, intent(in) :: pdg1, pdg2, pdg3
    logical :: flag
    integer :: vt_size
    vt_size = size (vt%entry)
    flag = check (hash2 (pdg1, pdg2))
  contains
    recursive function check (hashval) result (flag)
      integer(i32), intent(in) :: hashval
      integer :: h
      logical :: flag
      h = iand (hashval, vt%mask)
      if (vt%entry(h)%n == 0) then
         flag = .false.
      else if (vt%entry(h)%pdg1 == pdg1 .and. vt%entry(h)%pdg2 == pdg2) then
         flag = any (vt%entry(h)%pdg3 == pdg3)
      else
         flag = check (hashval + 1)
      end if
    end function check
  end function vertex_table_check

  subroutine model_init (model, name, libname, os_data, n_par, n_prt, n_vtx)
    type(model_t), intent(out) :: model
    type(string_t), intent(in) :: name, libname
    type(os_data_t), intent(in) :: os_data
    integer, intent(in) :: n_par, n_prt, n_vtx
    type(c_funptr) :: c_fptr
    type(string_t) :: libpath
    model%name = name
    allocate (model%par (n_par))
    allocate (model%prt (n_prt))
    allocate (model%vtx (n_vtx))
    if (libname /= "") then
       if (.not. os_data%use_testfiles) then
          libpath = os_data%whizard_models_libpath_local
          model%dlname = os_get_dlname ( &
            libpath // "/" // libname, os_data, ignore=.true., silent=.true.)
       end if
       if (model%dlname == "") then
          libpath = os_data%whizard_models_libpath
          model%dlname = os_get_dlname (libpath // "/" // libname, os_data)
       end if
    else
       model%dlname = ""
    end if
    if (model%dlname /= "") then
       if (.not. dlaccess_is_open (model%dlaccess)) then
          call msg_message ("Loading model auxiliary library '" &
               // char (libpath) // "/" // char (model%dlname) // "'")
          call dlaccess_init (model%dlaccess, os_data%whizard_models_libpath, &
               model%dlname, os_data)
          if (dlaccess_has_error (model%dlaccess)) then
             call msg_message (char (dlaccess_get_error (model%dlaccess)))
             call msg_fatal ("Loading model auxiliary library '" &
                  // char (model%dlname) // "' failed")
             return
          end if
          c_fptr = dlaccess_get_c_funptr (model%dlaccess, &
               var_str ("init_external_parameters"))
          if (dlaccess_has_error (model%dlaccess)) then
             call msg_message (char (dlaccess_get_error (model%dlaccess)))
             call msg_fatal ("Loading function from auxiliary library '" &
                  // char (model%dlname) // "' failed")
             return
          end if
          call c_f_procpointer (c_fptr, model% init_external_parameters)
       end if
    end if
  end subroutine model_init

  subroutine model_final (model)
    type(model_t), intent(inout) :: model
    call var_list_final (model%var_list)
    if (model%dlname /= "")  call dlaccess_final (model%dlaccess)
  end subroutine model_final

  subroutine model_write (model, unit, verbose)
    type(model_t), intent(in) :: model
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) 'model "', char (model%name), '"'
    write (u, *) '! md5sum = "', model%md5sum, '"'
    do i = 1, size (model%par)
       call parameter_write (model%par(i), unit, verbose)
       write (u, *)
    end do
    do i = 1, size (model%prt)
       call particle_data_write (model%prt(i), unit)
    end do
    do i = 1, size (model%vtx)
       call vertex_write (model%vtx(i), unit)
    end do
    if (present (verbose)) then
       if (verbose) then
          call vertex_table_write (model%vt, unit)
          call var_list_write (model%var_list, unit)
       end if
    end if
  end subroutine model_write

  function model_get_name (model) result (name)
    type(string_t) :: name
    type(model_t), intent(in) :: model
    name = model%name
  end function model_get_name

  function model_get_md5sum (model) result (md5sum)
    character(32) :: md5sum
    type(model_t), intent(in) :: model
    md5sum = model%md5sum
  end function model_get_md5sum

  function model_get_parameters_md5sum (model) result (par_md5sum)
    character(32) :: par_md5sum
    type(model_t), intent(in) :: model
    real(default), dimension(:), allocatable :: par
    integer :: unit
    call model_parameters_to_array (model, par)
    unit = free_unit ()
    open (unit, status="scratch", action="readwrite")
    write (unit, *)  par
    rewind (unit)
    par_md5sum = md5sum (unit)
    close (unit)
  end function model_get_parameters_md5sum

  function model_get_polarized_md5sum (model) result (pol_md5sum)
    character(32) :: pol_md5sum
    type(model_t), intent(in) :: model
    integer :: unit, i
    unit = free_unit ()
    open (unit, status="scratch", action="readwrite")
    if (size (model%prt) > 0) then
       do i = 1, size (model%prt)
          write (unit, *) &
             char (particle_data_get_name (model%prt(i), .false.)), " ", &
             particle_data_is_polarized (model%prt(i)), " "
          if (particle_data_has_antiparticle (model%prt(i))) write (unit, *) &
             char (particle_data_get_name (model%prt(i), .true.)), " ", &
             particle_data_is_polarized (model%prt(i), .true.), " "
       end do
    end if
    rewind (unit)
    pol_md5sum = md5sum (unit)
    close (unit)
  end function model_get_polarized_md5sum

  subroutine model_set_parameter_constant (model, i, name, value)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    real(default), intent(in) :: value
    logical, save, target :: known = .true.
    call parameter_init_independent_value (model%par(i), name, value)
    call var_list_append_real_ptr &
         (model%var_list, name, parameter_get_value_ptr (model%par(i)), known, &
         intrinsic=.true.)
  end subroutine model_set_parameter_constant

  subroutine model_set_parameter_parse_node (model, i, name, pn, constant)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    logical, intent(in) :: constant
    logical, save, target :: known = .true.
    if (constant) then
       call parameter_init_independent (model%par(i), name, pn)
    else
       call parameter_init_derived (model%par(i), name, pn, model%var_list)
    end if
    call var_list_append_real_ptr &
         (model%var_list, name, parameter_get_value_ptr (model%par(i)), &
          is_known=known, locked=.not.constant, intrinsic=.true.)
  end subroutine model_set_parameter_parse_node

  subroutine model_set_parameter_external (model, i, name)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    logical, save, target :: known = .true.
    call parameter_init_external (model%par(i), name)
    call var_list_append_real_ptr &
         (model%var_list, name, parameter_get_value_ptr (model%par(i)), &
          is_known=known, locked=.true., intrinsic=.true.)
  end subroutine model_set_parameter_external

  function model_get_parameter_ptr (model, par_name) result (par)
    type(parameter_t), pointer :: par
    type(model_t), intent(in), target :: model
    type(string_t), intent(in), optional :: par_name
    integer :: i
    par => null ()
    if (present (par_name)) then
       do i = 1, size (model%par)
          if (model%par(i)%name == par_name) then
             par => model%par(i);  exit
          end if
       end do
       if (.not. associated (par)) then
          call msg_fatal (" Model '" // char (model%name) // "'" // &
               " has no parameter '" // char (par_name) // "'")
       end if
    end if
  end function model_get_parameter_ptr

  function model_get_parameter_value (model, par_name) result (val)
    real(default) :: val
    type(model_t), intent(in), target :: model
    type(string_t), intent(in), optional :: par_name
    val = parameter_get_value_ptr (model_get_parameter_ptr (model, par_name))
  end function model_get_parameter_value

  function model_get_n_parameters (model) result (n)
    integer :: n
    type(model_t), intent(in) :: model
    n = size (model%par)
  end function model_get_n_parameters

  subroutine model_parameters_to_array (model, array)
    type(model_t), intent(in) :: model
    real(default), dimension(:), allocatable :: array
    integer :: i
    if (allocated (array))  deallocate (array)
    allocate (array (size (model%par)))
    do i = 1, size (model%par)
       array(i) = model%par(i)%value
    end do
  end subroutine model_parameters_to_array

  subroutine model_parameters_to_c_array (model, array)
    type(model_t), intent(in) :: model
    real(c_default_float), dimension(:), allocatable :: array
    allocate (array (size (model%par)))
    array = model%par%value
  end subroutine model_parameters_to_c_array

  subroutine model_parameters_from_c_array (model, array)
    type(model_t), intent(inout) :: model
    real(c_default_float), dimension(:), intent(in) :: array
    if (size (array) == size (model%par)) then
       model%par%value = array
    else
       call msg_bug ("Model '" // char (model%name) // "': size mismatch " &
            // "in parameter array")
    end if
  end subroutine model_parameters_from_c_array

  subroutine model_parameters_update (model)
    type(model_t), intent(inout) :: model
    integer :: i
    real(default), dimension(:), allocatable :: par
    do i = 1, size (model%par)
       call parameter_reset_derived (model%par(i))
    end do
    if (associated (model% init_external_parameters)) then
       call model_parameters_to_c_array (model, par)
       call model% init_external_parameters (par)
       call model_parameters_from_c_array (model, par)
    end if
  end subroutine model_parameters_update

  subroutine model_init_particle (model, i, longname, pdg)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: longname
    integer, intent(in) :: pdg
    type(pdg_array_t) :: aval
    call particle_data_init (model%prt(i), longname, pdg)
    aval = pdg
    call var_list_append_pdg_array &
         (model%var_list, longname, aval, locked=.true., intrinsic=.true.)
  end subroutine model_init_particle
    
  subroutine model_copy_particle_data (model, i, name_src)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name_src
    call particle_data_copy (model%prt(i), &
         model_get_particle_ptr (model, &
            model_get_particle_pdg (model, name_src)))
  end subroutine model_copy_particle_data

  subroutine model_set_particle_data (model, i, &
       is_visible, is_parton, is_gauge, is_left_handed, is_right_handed, &
       name, anti, tex_name, tex_anti, &
       spin_type, isospin_type, charge_type, color_type, &
       mass_src, width_src)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    logical, intent(in), optional :: is_visible, is_parton, is_gauge
    logical, intent(in), optional :: is_left_handed, is_right_handed
    type(string_t), dimension(:), intent(in), optional :: name, anti
    type(string_t), intent(in), optional :: tex_name, tex_anti
    integer, intent(in), optional :: spin_type, isospin_type
    integer, intent(in), optional :: charge_type, color_type
    type(parameter_t), intent(in), optional, target :: mass_src, width_src
    integer :: j
    type(pdg_array_t) :: aval
    logical, parameter :: is_stable = .true.
    logical, parameter :: decays_isotropically = .false.
    logical, parameter :: decays_diagonal = .false.
    logical, parameter :: p_polarized = .false.
    logical, parameter :: a_polarized = .false.
    call particle_data_set (model%prt(i), &
       is_visible, is_parton, is_gauge, is_left_handed, is_right_handed, &
       is_stable, decays_isotropically, decays_diagonal, &
       is_stable, decays_isotropically, decays_diagonal, &
       p_polarized, a_polarized, &
       name, anti, tex_name, tex_anti, &
       spin_type, isospin_type, charge_type, color_type, &
       mass_src, width_src)
    if (present (name)) then
       aval = particle_data_get_pdg (model%prt(i))
       do j = 1, size (name)
          call var_list_append_pdg_array &
               (model%var_list, name(j), aval, locked=.true., intrinsic=.true.)
       end do
    end if
    if (present (anti)) then
       aval = - particle_data_get_pdg (model%prt(i))
       do j = 1, size (anti)
          call var_list_append_pdg_array &
               (model%var_list, anti(j), aval, locked=.true., intrinsic=.true.)
       end do
    end if
  end subroutine model_set_particle_data

  subroutine model_freeze_particle_data (model, i)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    call particle_data_freeze (model%prt(i))
  end subroutine model_freeze_particle_data

  function model_get_particle_ptr (model, pdg) result (prt)
    type(particle_data_t), pointer :: prt
    type(model_t), intent(in), target :: model
    integer, intent(in) :: pdg
    integer :: i
    prt => null ()
    if (pdg /= UNDEFINED) then
       do i = 1, size (model%prt)
          if (model%prt(i)%pdg == abs (pdg)) then
             prt => model%prt(i);  exit
          end if
       end do
       if (.not. associated (prt)) then
          write (msg_buffer, "(1x,A,1x,I0)")  "PDG code =", pdg
          call msg_message
          call msg_fatal (" Model '" // char (model%name) // "'" // &
               " has no particle with this PDG code")
       end if
    end if
  end function model_get_particle_ptr

  function model_test_particle (model, pdg) result (exists)
    logical :: exists
    type(particle_data_t), pointer :: prt
    type(model_t), intent(in), target :: model
    integer, intent(in) :: pdg
    integer :: i
    prt => null ()
    if (pdg /= UNDEFINED) then
       do i = 1, size (model%prt)
          if (model%prt(i)%pdg == abs (pdg)) then
             prt => model%prt(i);  exit
          end if
       end do
       exists = associated(prt)
    else
       exists = .false.
    end if
  end function model_test_particle

  subroutine model_set_particle_mass (model, pdg, mass)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: pdg
    real(default), intent(in) :: mass
    type(particle_data_t), pointer :: prt
    prt => model_get_particle_ptr (model, pdg)
    if (associated (prt))  call particle_data_set_mass (prt, mass)
  end subroutine model_set_particle_mass

  subroutine model_set_particle_width (model, pdg, width)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: pdg
    real(default), intent(in) :: width
    type(particle_data_t), pointer :: prt
    prt => model_get_particle_ptr (model, pdg)
    if (associated (prt))  call particle_data_set_width (prt, width)
  end subroutine model_set_particle_width

  function model_get_particle_pdg (model, name) result (pdg)
    integer :: pdg
    type(model_t), intent(in), target :: model
    type(string_t), intent(in) :: name
    integer :: i
    pdg = UNDEFINED
    do i = 1, size (model%prt)
       if (model%prt(i)%longname == name) then
          pdg = particle_data_get_pdg (model%prt(i));  exit
       else if (any (model%prt(i)%name == name)) then
          pdg = particle_data_get_pdg (model%prt(i));  exit
       else if (any (model%prt(i)%anti == name)) then
          pdg = - particle_data_get_pdg (model%prt(i));  exit
       end if
    end do
    if (pdg == UNDEFINED) then
       write (msg_buffer, "(1x,A,1x,A)")  "Particle name =", char (name)
       call msg_message
       call msg_fatal (" Model '" // char (model%name) // "'" // &
            " has no particle with this name")
    end if
  end function model_get_particle_pdg

  function model_get_var_list_ptr (model) result (var_list)
    type(var_list_t), pointer :: var_list
    type(model_t), intent(in), target :: model
    var_list => model%var_list
  end function model_get_var_list_ptr

  subroutine model_set_vertex_pdg (model, i, pdg)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    integer, dimension(:), intent(in) :: pdg
    call vertex_init (model%vtx(i), pdg, model)
  end subroutine model_set_vertex_pdg

  subroutine model_set_vertex_names (model, i, name)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), dimension(:), intent(in) :: name
    integer, dimension(size(name)) :: pdg
    integer :: j
    do j = 1, size (name)
       pdg(j) = model_get_particle_pdg (model, name(j))
    end do
    call vertex_init (model%vtx(i), pdg, model)
  end subroutine model_set_vertex_names

  subroutine model_match_vertex (model, pdg1, pdg2, pdg3)
    type(model_t), intent(in) :: model
    integer, intent(in) :: pdg1, pdg2
    integer, dimension(:), allocatable, intent(out) :: pdg3
    call vertex_table_match (model%vt, pdg1, pdg2, pdg3)
  end subroutine model_match_vertex

  function model_check_vertex (model, pdg1, pdg2, pdg3) result (flag)
    logical :: flag
    type(model_t), intent(in) :: model
    integer, intent(in) :: pdg1, pdg2, pdg3
    flag = vertex_table_check (model%vt, pdg1, pdg2, pdg3)
  end function model_check_vertex

  subroutine define_model_file_syntax (ifile)
    type(ifile_t), intent(inout) :: ifile
    call ifile_append (ifile, "SEQ model_def = model_name_def " // &
         "parameters derived_pars external_pars particles vertices")
    call ifile_append (ifile, "SEQ model_name_def = model model_name")
    call ifile_append (ifile, "KEY model")
    call ifile_append (ifile, "QUO model_name = '""'...'""'")
    call ifile_append (ifile, "SEQ parameters = parameter_def*")
    call ifile_append (ifile, "SEQ parameter_def = parameter par_name " // &
         "'=' any_real_value")
    call ifile_append (ifile, "ALT any_real_value = " &
         // "neg_real_value | pos_real_value | real_value")
    call ifile_append (ifile, "SEQ neg_real_value = '-' real_value")
    call ifile_append (ifile, "SEQ pos_real_value = '+' real_value")
    call ifile_append (ifile, "KEY parameter")
    call ifile_append (ifile, "IDE par_name")
!    call ifile_append (ifile, "KEY '='")
!    call ifile_append (ifile, "REA par_value")
    call ifile_append (ifile, "SEQ derived_pars = derived_def*")
    call ifile_append (ifile, "SEQ derived_def = derived par_name " // &
         "'=' expr")
    call ifile_append (ifile, "KEY derived")
    call ifile_append (ifile, "SEQ external_pars = external_def*")
    call ifile_append (ifile, "SEQ external_def = external par_name")
    call ifile_append (ifile, "KEY external")
    call ifile_append (ifile, "SEQ particles = particle_def*")
    call ifile_append (ifile, "SEQ particle_def = particle prt_longname " // &
         "prt_pdg prt_details")
    call ifile_append (ifile, "KEY particle")
    call ifile_append (ifile, "IDE prt_longname")
    call ifile_append (ifile, "INT prt_pdg")
    call ifile_append (ifile, "ALT prt_details = prt_src | prt_properties")
    call ifile_append (ifile, "SEQ prt_src = like prt_longname prt_properties")
    call ifile_append (ifile, "KEY like")
    call ifile_append (ifile, "SEQ prt_properties = prt_property*")
    call ifile_append (ifile, "ALT prt_property = " // & 
         "parton | invisible | gauge | left | right | " // &
         "prt_name | prt_anti | prt_tex_name | prt_tex_anti | " // &
         "prt_spin | prt_isospin | prt_charge | " // &
         "prt_color | prt_mass | prt_width")
    call ifile_append (ifile, "KEY parton")
    call ifile_append (ifile, "KEY invisible")
    call ifile_append (ifile, "KEY gauge")
    call ifile_append (ifile, "KEY left")
    call ifile_append (ifile, "KEY right")
    call ifile_append (ifile, "SEQ prt_name = name name_def+")
    call ifile_append (ifile, "SEQ prt_anti = anti name_def+")
    call ifile_append (ifile, "SEQ prt_tex_name = tex_name name_def")
    call ifile_append (ifile, "SEQ prt_tex_anti = tex_anti name_def")
    call ifile_append (ifile, "KEY name")
    call ifile_append (ifile, "KEY anti")
    call ifile_append (ifile, "KEY tex_name")
    call ifile_append (ifile, "KEY tex_anti")
    call ifile_append (ifile, "ALT name_def = name_string | name_id")
    call ifile_append (ifile, "QUO name_string = '""'...'""'")
    call ifile_append (ifile, "IDE name_id")
    call ifile_append (ifile, "SEQ prt_spin = spin frac")
    call ifile_append (ifile, "KEY spin")
!     call ifile_append (ifile, "SEQ frac = signed_int div?")
!     call ifile_append (ifile, "ALT signed_int = " &
!          // "neg_int | pos_int | integer_literal")
!     call ifile_append (ifile, "SEQ neg_int = '-' integer_literal")
!     call ifile_append (ifile, "SEQ pos_int = '+' integer_literal")
!    call ifile_append (ifile, "KEY '-'")
!    call ifile_append (ifile, "KEY '+'")
!    call ifile_append (ifile, "INT int")
!     call ifile_append (ifile, "SEQ div = '/' integer_literal")
!    call ifile_append (ifile, "KEY '/'")
    call ifile_append (ifile, "SEQ prt_isospin = isospin frac")
    call ifile_append (ifile, "KEY isospin")
    call ifile_append (ifile, "SEQ prt_charge = charge frac")
    call ifile_append (ifile, "KEY charge")
    call ifile_append (ifile, "SEQ prt_color = color integer_literal")
    call ifile_append (ifile, "KEY color")
    call ifile_append (ifile, "SEQ prt_mass = mass par_name")
    call ifile_append (ifile, "KEY mass")
    call ifile_append (ifile, "SEQ prt_width = width par_name")
    call ifile_append (ifile, "KEY width")
    call ifile_append (ifile, "SEQ vertices = vertex_def*")
    call ifile_append (ifile, "SEQ vertex_def = vertex name_def+")
    call ifile_append (ifile, "KEY vertex")
    call define_expr_syntax (ifile, particles=.false., analysis=.false.)
  end subroutine define_model_file_syntax

  subroutine syntax_model_file_init ()
    type(ifile_t) :: ifile
    call define_model_file_syntax (ifile)
    call syntax_init (syntax_model_file, ifile)
    call ifile_final (ifile)
  end subroutine syntax_model_file_init

  subroutine lexer_init_model_file (lexer)
    type(lexer_t), intent(out) :: lexer
    call lexer_init (lexer, &
         comment_chars = "#!", &
         quote_chars = '"{', &
         quote_match = '"}', &
         single_chars = ":()", &
         special_class = (/ "+-*/^", "<>=  " /) , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_model_file))
  end subroutine lexer_init_model_file

  subroutine syntax_model_file_final ()
    call syntax_final (syntax_model_file)
  end subroutine syntax_model_file_final

  subroutine syntax_model_file_write (unit)
    integer, intent(in), optional :: unit
    call syntax_write (syntax_model_file, unit)
  end subroutine syntax_model_file_write

  subroutine model_read (model, filename, os_data, exist)
    type(model_t), intent(out), target :: model
    type(string_t), intent(in) :: filename
    type(os_data_t), intent(in) :: os_data
    logical, intent(out) :: exist
    type(string_t) :: file
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    integer :: unit
    character(32) :: model_md5sum
    type(parse_tree_t) :: parse_tree
    type(parse_node_t), pointer :: nd_model_def, nd_model_name_def
    type(parse_node_t), pointer :: nd_model_arg
    type(parse_node_t), pointer :: nd_parameters, nd_derived_pars
    type(parse_node_t), pointer :: nd_external_pars
    type(parse_node_t), pointer :: nd_particles, nd_vertices
    type(string_t) :: model_name, lib_name
    integer :: n_par, n_der, n_ext, n_prt, n_vtx
    real(c_default_float), dimension(:), allocatable :: par
    integer :: i
    type(parse_node_t), pointer :: nd_par_def
    type(parse_node_t), pointer :: nd_der_def
    type(parse_node_t), pointer :: nd_ext_def
    type(parse_node_t), pointer :: nd_prt
    type(parse_node_t), pointer :: nd_vtx
    type(pdg_array_t) :: prt_undefined
    file = filename
    inquire (file=char(file), exist=exist)
    if ((.not. exist) .and. (.not. os_data%use_testfiles)) then
       file = os_data%whizard_modelpath_local // "/" // filename
       inquire (file = char (file), exist = exist)
    end if
    if (.not. exist) then
       file = os_data%whizard_modelpath // "/" // filename
       inquire (file = char (file), exist = exist)
    end if
    if (.not. exist) then
       call msg_fatal ("Model file '" // char (filename) // "' not found")
       return
    end if
    call msg_message ("Reading model file '" // char (file) // "'")
    call lexer_init_model_file (lexer)
    unit = free_unit ()
    open (file=char(file), unit=unit, action="read", status="old")
    model_md5sum = md5sum (unit)
    close (unit)
    call stream_init (stream, char (file))
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_model_file, lexer)
    call stream_final (stream)
    call lexer_final (lexer)
!     call parse_tree_write (parse_tree)
    nd_model_def => parse_tree_get_root_ptr (parse_tree)
    nd_model_name_def => parse_node_get_sub_ptr (nd_model_def)
    model_name = parse_node_get_string &
         (parse_node_get_sub_ptr (nd_model_name_def, 2))
    nd_parameters => parse_node_get_next_ptr (nd_model_name_def)
    if (associated (nd_parameters)) then
       if (parse_node_get_rule_key (nd_parameters) == "parameters") then
          n_par = parse_node_get_n_sub (nd_parameters)
          nd_par_def => parse_node_get_sub_ptr (nd_parameters)
          nd_derived_pars => parse_node_get_next_ptr (nd_parameters)
       else
          n_par = 0
          nd_derived_pars => nd_parameters
          nd_parameters => null ()
       end if
    else
       n_par = 0
       nd_derived_pars => null ()
    end if
    if (associated (nd_derived_pars)) then
       if (parse_node_get_rule_key (nd_derived_pars) == "derived_pars") then
          n_der = parse_node_get_n_sub (nd_derived_pars)
          nd_der_def => parse_node_get_sub_ptr (nd_derived_pars)
          nd_external_pars => parse_node_get_next_ptr (nd_derived_pars)
       else
          n_der = 0
          nd_external_pars => nd_derived_pars
          nd_derived_pars => null ()
       end if
    else
       n_der = 0
       nd_external_pars => null ()
    end if
    if (associated (nd_external_pars)) then
       if (parse_node_get_rule_key (nd_external_pars) == "external_pars") then
          n_ext = parse_node_get_n_sub (nd_external_pars)
          lib_name = "external." // model_name
          nd_ext_def => parse_node_get_sub_ptr (nd_external_pars)
          nd_particles => parse_node_get_next_ptr (nd_external_pars)
       else
          n_ext = 0
          lib_name = ""
          nd_particles => nd_external_pars
          nd_external_pars => null ()
       end if
    else
       n_ext = 0
       lib_name = ""
       nd_particles => null ()
    end if
    if (associated (nd_particles)) then
       if (parse_node_get_rule_key (nd_particles) == "particles") then
          n_prt = parse_node_get_n_sub (nd_particles)
          nd_prt => parse_node_get_sub_ptr (nd_particles)
          nd_vertices => parse_node_get_next_ptr (nd_particles)
       else
          n_prt = 0
          nd_vertices => nd_particles
          nd_particles => null ()
       end if
    else
       n_prt = 0
       nd_vertices => null ()
    end if
    if (associated (nd_vertices)) then
       n_vtx = parse_node_get_n_sub (nd_vertices)
       nd_vtx => parse_node_get_sub_ptr (nd_vertices)
    else
       n_vtx = 0
    end if
    call model_init (model, model_name, lib_name, os_data, &
         n_par + n_der + n_ext, n_prt, n_vtx)
    model%md5sum = model_md5sum
    do i = 1, n_par
       call model_read_parameter (model, i, nd_par_def)
       nd_par_def => parse_node_get_next_ptr (nd_par_def)
    end do
    do i = n_par + 1, n_par + n_der
       call model_read_derived (model, i, nd_der_def)
       nd_der_def => parse_node_get_next_ptr (nd_der_def)
    end do
    do i = n_par + n_der + 1, n_par + n_der + n_ext
       call model_read_external (model, i, nd_ext_def)
       nd_ext_def => parse_node_get_next_ptr (nd_ext_def)
    end do
    if (associated (model% init_external_parameters)) then
       call model_parameters_to_c_array (model, par)
       call model% init_external_parameters (par)
       call model_parameters_from_c_array (model, par)
    end if
    prt_undefined = UNDEFINED
    call var_list_append_pdg_array &
         (model%var_list, var_str ("particle"), &
          prt_undefined, locked = .true., intrinsic=.true.)
    do i = 1, n_prt
       call model_read_particle (model, i, nd_prt)
       nd_prt => parse_node_get_next_ptr (nd_prt)
    end do
    do i = 1, n_vtx
       call model_read_vertex (model, i, nd_vtx)
       nd_vtx => parse_node_get_next_ptr (nd_vtx)
    end do
    call parse_tree_final (parse_tree)
    call var_list_append_pdg_array &
         (model%var_list, var_str ("charged"), &
          particle_data_get_charged_pdg (model%prt), locked = .true., &
          intrinsic=.true.)
    call var_list_append_pdg_array &
         (model%var_list, var_str ("colored"), &
          particle_data_get_colored_pdg (model%prt), locked = .true., &
          intrinsic=.true.)
  end subroutine model_read

  subroutine model_read_parameter (model, i, node)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(parse_node_t), pointer :: node_name, node_val
    type(string_t) :: name
    node_name => parse_node_get_sub_ptr (node, 2)
    name = parse_node_get_string (node_name)
    node_val => parse_node_get_next_ptr (node_name, 2)
    call model_set_parameter (model, i, name, node_val, constant=.true.)
  end subroutine model_read_parameter

  subroutine model_read_derived (model, i, node)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(string_t) :: name
    type(parse_node_t), pointer :: pn_expr
    name = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    pn_expr => parse_node_get_sub_ptr (node, 4)
    call model_set_parameter (model, i, name, pn_expr, constant=.false.)
  end subroutine model_read_derived

  subroutine model_read_external (model, i, node)
    type(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(string_t) :: name
    name = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    call model_set_parameter_external (model, i, name)
  end subroutine model_read_external

  subroutine model_read_particle (model, i, node)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in) :: node
    type(parse_node_t), pointer :: nd_src, nd_props, nd_prop
    type(string_t) :: longname
    integer :: pdg
    type(string_t) :: name_src
    type(string_t), dimension(:), allocatable :: name
    longname = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    pdg = parse_node_get_integer (parse_node_get_sub_ptr (node, 3)) 
    call model_init_particle (model, i, longname, pdg)
    nd_src => parse_node_get_sub_ptr (node, 4)
    if (associated (nd_src)) then
       if (parse_node_get_rule_key (nd_src) == "prt_src") then
          name_src = parse_node_get_string (parse_node_get_sub_ptr (nd_src, 2))
          call model_copy_particle_data (model, i, name_src)
          nd_props => parse_node_get_sub_ptr (nd_src, 3)
       else
          nd_props => nd_src
       end if
       nd_prop => parse_node_get_sub_ptr (nd_props)
       do while (associated (nd_prop))
          select case (char (parse_node_get_rule_key (nd_prop)))
          case ("invisible")
             call model_set_particle_data (model, i, is_visible=.false.)
          case ("parton")
             call model_set_particle_data (model, i, is_parton=.true.)
          case ("gauge")
             call model_set_particle_data (model, i, is_gauge=.true.)
          case ("left")
             call model_set_particle_data (model, i, is_left_handed=.true.)
          case ("right")
             call model_set_particle_data (model, i, is_right_handed=.true.)
          case ("prt_name")
             call read_names (nd_prop, name)
             call model_set_particle_data (model, i, name=name)
          case ("prt_anti")
             call read_names (nd_prop, name)
             call model_set_particle_data (model, i, anti=name)
          case ("prt_tex_name")
             call model_set_particle_data (model, i, &
                  tex_name = parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_tex_anti")
             call model_set_particle_data (model, i, &
                  tex_anti = parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_spin")
             call model_set_particle_data (model, i, &
                  spin_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 2))
          case ("prt_isospin")
             call model_set_particle_data (model, i, &
                  isospin_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 2))
          case ("prt_charge")
             call model_set_particle_data (model, i, &
                  charge_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 3))
          case ("prt_color")
             call model_set_particle_data (model, i, &
                  color_type = parse_node_get_integer &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_mass")
             call model_set_particle_data (model, i, &
                  mass_src = model_get_parameter_ptr &
                  (model, parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2))))
          case ("prt_width")
             call model_set_particle_data (model, i, &
                  width_src = model_get_parameter_ptr &
                  (model, parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2))))
          case default
             call msg_bug (" Unknown particle property '" &
                  // char (parse_node_get_rule_key (nd_prop)) // "'")
          end select
          if (allocated (name))  deallocate (name)
          nd_prop => parse_node_get_next_ptr (nd_prop)
       end do
    end if
    call model_freeze_particle_data (model, i)
  end subroutine model_read_particle

  subroutine model_read_vertex (model, i, node)
    type(model_t), intent(inout) :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in) :: node
    type(string_t), dimension(:), allocatable :: name
    call read_names (node, name)
    call model_set_vertex (model, i, name)
  end subroutine model_read_vertex

  subroutine read_names (node, name)
    type(parse_node_t), intent(in) :: node
    type(string_t), dimension(:), allocatable, intent(inout) :: name
    type(parse_node_t), pointer :: nd_name
    integer :: n_names, i
    n_names = parse_node_get_n_sub (node) - 1
    allocate (name (n_names))
    nd_name => parse_node_get_sub_ptr (node, 2)
    do i = 1, n_names
       name(i) = parse_node_get_string (nd_name)
       nd_name => parse_node_get_next_ptr (nd_name)
    end do
  end subroutine read_names

  function read_frac (nd_frac, base) result (qn_type)
    integer :: qn_type
    type(parse_node_t), intent(in) :: nd_frac
    integer, intent(in) :: base
    type(parse_node_t), pointer :: nd_num, nd_den
    integer :: num, den
    nd_num => parse_node_get_sub_ptr (nd_frac)
    nd_den => parse_node_get_next_ptr (nd_num)
    select case (char (parse_node_get_rule_key (nd_num)))
    case ("integer_literal")
       num = parse_node_get_integer (nd_num)
    case ("neg_int")
       num = - parse_node_get_integer (parse_node_get_sub_ptr (nd_num, 2))
    case ("pos_int")
       num = parse_node_get_integer (parse_node_get_sub_ptr (nd_num, 2))
    case default
       call parse_tree_bug (nd_num, "int|neg_int|pos_int")
    end select
    if (associated (nd_den)) then
       den = parse_node_get_integer (parse_node_get_sub_ptr (nd_den, 2))
    else
       den = 1
    end if
    if (den == 1) then
       qn_type = sign (1 + abs (num) * base, num)
    else if (den == base) then
       qn_type = sign (abs (num) + 1, num)
    else
       call parse_node_write_rec (nd_frac)
       call msg_fatal (" Fractional quantum number: wrong denominator")
    end if
  end function read_frac

  subroutine model_list_write (unit, verbose)
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    type(model_entry_t), pointer :: current
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "List of models:"
    current => model_list%first
    if (associated (current)) then
       do while (associated (current))
          write (u, *)
          call model_write (current%model, unit, verbose)
          current => current%next
       end do
    else
       write (u, *) "  [empty]"
    end if
  end subroutine model_list_write

  subroutine model_list_add (name, os_data, n_par, n_prt, n_vtx, model)
    type(string_t), intent(in) :: name
    type(os_data_t), intent(in) :: os_data
    integer, intent(in) :: n_par, n_prt, n_vtx
    type(model_t), pointer :: model
    type(model_entry_t), pointer :: current
    if (model_list_model_exists (name)) then
       model => null ()
    else
       allocate (current)
       if (associated (model_list%first)) then
          model_list%last%next => current
       else
          model_list%first => current
       end if
       model_list%last => current
       model => current%model
       call model_init (model, name, var_str (""), os_data, &
            n_par, n_prt, n_vtx)
    end if
  end subroutine model_list_add

  subroutine model_list_read_model (name, filename, os_data, model)
    type(string_t), intent(in) :: name, filename
    type(os_data_t), intent(in) :: os_data
    type(model_t), pointer :: model
    type(model_entry_t), pointer :: current
    logical :: exist
    if (.not. model_list_model_exists (name)) then
       allocate (current)
       call model_read (current%model, filename, os_data, exist)
       if (.not. exist)  return
       if (current%model%name /= name) then
          call msg_fatal ("Model file '" // char (filename) // &
               "' contains model '" // char (current%model%name) // &
               "' instead of '" // char (name) // "'")
          call model_final (current%model);  deallocate (current)
          return
       end if
       if (associated (model_list%first)) then
          model_list%last%next => current
       else
          model_list%first => current
       end if
       model_list%last => current
       call vertex_table_init &
            (current%model%vt, current%model%prt, current%model%vtx)
       model => current%model
    else
       model => model_list_get_model_ptr (name)
    end if
  end subroutine model_list_read_model

  function model_list_model_exists (name) result (exists)
    logical :: exists
    type(string_t), intent(in) :: name
    type(model_entry_t), pointer :: current
    current => model_list%first
    do while (associated (current))
       if (current%model%name == name) then
          exists = .true.
          return
       end if
       current => current%next
    end do
    exists = .false.
  end function model_list_model_exists

  function model_list_get_model_ptr (name) result (model)
    type(model_t), pointer :: model
    type(string_t), intent(in) :: name
    type(model_entry_t), pointer :: current
    current => model_list%first
    do while (associated (current))
       if (current%model%name == name) then
          model => current%model
          return
       end if
       current => current%next
    end do
    model => null ()
  end function model_list_get_model_ptr

  subroutine model_list_final ()
    type(model_entry_t), pointer :: current
    model_list%last => null ()
    do while (associated (model_list%first))
       current => model_list%first
       model_list%first => model_list%first%next
       call model_final (current%model)
       deallocate (current)
    end do
  end subroutine model_list_final

  subroutine models_test ()
    type(os_data_t), pointer :: os_data => null ()
    call syntax_model_file_init ()
    call syntax_write (syntax_model_file)
    print *
    allocate (os_data)
    call os_data_init (os_data)
    call models_test1 (os_data)
    call models_test2 (os_data)
    !!! Try to cath the gfortran 4.5.0+(?) seg fault
    !!! call model_list_write (verbose=.true.)
    call model_list_write ()
    call model_list_final ()
    call syntax_model_file_final ()
    deallocate (os_data)
  end subroutine models_test

  subroutine models_test1 (os_data)
    type(os_data_t), intent(in) :: os_data
    type(model_t), pointer :: model
    type(string_t) :: model_name
    type(string_t) :: x_longname
    type(string_t), dimension(2) :: parname
    type(string_t), dimension(2) :: x_name
    type(string_t), dimension(1) :: x_anti
    type(string_t) :: x_tex_name, x_tex_anti
    type(string_t) :: y_longname
    type(string_t), dimension(2) :: y_name
    type(string_t) :: y_tex_name
    model_name = "Test model"
    call model_list_add (model_name, os_data, 2, 2, 3, model)
    parname(1) = "mx"
    parname(2) = "coup"
    call model_set_parameter (model, 1, parname(1), 10._default)
    call model_set_parameter (model, 2, parname(2), 1.3_default)
    print *
    x_longname = "X_LEPTON"
    x_name(1) = "X"
    x_name(2) = "x"
    x_anti(1) = "Xbar"
    x_tex_name = "X^+"
    x_tex_anti = "X^-"
    call model_init_particle (model, 1, x_longname, 99)
    call model_set_particle_data (model, 1, &
         .true., .false., .false., .false., .false., &
         x_name, x_anti, x_tex_name, x_tex_anti, &
         SPINOR, -3, 2, 1, model_get_parameter_ptr (model, parname(1)))
    y_longname = "Y_COLORON"
    y_name(1) = "Y"
    y_name(2) = "yc"
    y_tex_name = "Y^0"
    call model_init_particle (model, 2, y_longname, 97)
    call model_set_particle_data (model, 2,  &
          .false., .false., .true., .false., .false., &
          name=y_name, tex_name=y_tex_name, &
          spin_type=SCALAR, isospin_type=2, charge_type=1, color_type=8)
    call model_set_vertex (model, 1, (/ 99, 99, 99 /))
    call model_set_vertex (model, 2, (/ 99, 99, 99, 99 /))
    call model_set_vertex (model, 3, (/ 99, 97, 99 /))
  end subroutine models_test1

  subroutine models_test2 (os_data)
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: name, filename
    type(model_t), pointer :: model
    name = "SM"
    filename = "SM.mdl"
    call model_list_read_model (name, filename, os_data, model)
  end subroutine models_test2


end module models
