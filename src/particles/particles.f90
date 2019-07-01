! WHIZARD 2.6.0 Sep 08 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module particles

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_compressed_integer_array, write_separator
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_16, FMT_19
  use numeric_utils
  use diagnostics
  use lorentz
  use model_data
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices
  use interactions
  use subevents
  use polarizations
  use pdg_arrays, only: is_quark, is_gluon

  implicit none
  private

  public :: particle_t
  public :: particle_set_t
  public :: pacify

  integer, parameter, public :: PRT_UNPOLARIZED = 0
  integer, parameter, public :: PRT_DEFINITE_HELICITY = 1
  integer, parameter, public :: PRT_GENERIC_POLARIZATION = 2


  type :: particle_t
     !private
     integer :: status = PRT_UNDEFINED
     integer :: polarization = PRT_UNPOLARIZED
     type(flavor_t) :: flv
     type(color_t) :: col
     type(helicity_t) :: hel
     type(polarization_t) :: pol
     type(vector4_t) :: p = vector4_null
     real(default) :: p2 = 0
     type(vector4_t), allocatable :: vertex
     real(default), allocatable :: lifetime
     integer, dimension(:), allocatable :: parent
     integer, dimension(:), allocatable :: child
   contains
    generic :: init => init_particle
    procedure :: init_particle => particle_init_particle
    generic :: init => init_external
    procedure :: init_external => particle_init_external
    generic :: init => init_state
    procedure :: init_state => particle_init_state
    procedure :: final => particle_final
    procedure :: write => particle_write
    procedure :: write_raw => particle_write_raw
    procedure :: read_raw => particle_read_raw
    procedure :: reset_status => particle_reset_status
    procedure :: set_color => particle_set_color
    procedure :: set_flavor => particle_set_flavor
    procedure :: set_helicity => particle_set_helicity
    procedure :: set_pol => particle_set_pol
    procedure :: set_model => particle_set_model
    procedure :: set_momentum => particle_set_momentum
    procedure :: set_children => particle_set_children
    procedure :: set_parents => particle_set_parents
    procedure :: add_child => particle_add_child
    procedure :: add_children => particle_add_children
    procedure :: set_status => particle_set_status
    procedure :: set_polarization => particle_set_polarization
    generic :: set_vertex => set_vertex_from_vector3, set_vertex_from_xyz, &
         set_vertex_from_vector4, set_vertex_from_xyzt
    procedure :: set_vertex_from_vector4 => particle_set_vertex_from_vector4
    procedure :: set_vertex_from_vector3 => particle_set_vertex_from_vector3
    procedure :: set_vertex_from_xyzt => particle_set_vertex_from_xyzt
    procedure :: set_vertex_from_xyz => particle_set_vertex_from_xyz
    procedure :: set_lifetime => particle_set_lifetime
    procedure :: get_status => particle_get_status
    procedure :: is_real => particle_is_real
    procedure :: is_colored => particle_is_colored
    procedure :: is_hadronic_beam_remnant => particle_is_hadronic_beam_remnant
    procedure :: is_beam_remnant => particle_is_beam_remnant
    procedure :: get_polarization_status => particle_get_polarization_status
    procedure :: get_pdg => particle_get_pdg
    procedure :: get_color => particle_get_color
    procedure :: get_polarization => particle_get_polarization
    procedure :: get_flv => particle_get_flv
    procedure :: get_col => particle_get_col
    procedure :: get_hel => particle_get_hel
    procedure :: get_helicity => particle_get_helicity
    procedure :: get_n_parents => particle_get_n_parents
    procedure :: get_n_children => particle_get_n_children
    procedure :: get_parents => particle_get_parents
    procedure :: get_children => particle_get_children
    procedure :: has_children => particle_has_children
    procedure :: has_parents => particle_has_parents
    procedure :: get_momentum => particle_get_momentum
    procedure :: get_p2 => particle_get_p2
    procedure :: get_vertex => particle_get_vertex
    procedure :: get_lifetime => particle_get_lifetime
    procedure :: momentum_to_pythia6 => particle_momentum_to_pythia6
  end type particle_t

  type :: particle_set_t
     ! private !!! 
     integer :: n_beam = 0
     integer :: n_in  = 0
     integer :: n_vir = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     integer :: factorization_mode = FM_IGNORE_HELICITY
     type(particle_t), dimension(:), allocatable :: prt
     type(state_matrix_t) :: correlated_state
   contains
     generic :: init => init_interaction
     procedure :: init_interaction => particle_set_init_interaction
     generic :: init => init_particle_set
     procedure :: init_particle_set => particle_set_init_particle_set
     procedure :: set_model => particle_set_set_model
     procedure :: final => particle_set_final
     procedure :: basic_init => particle_set_basic_init
     procedure :: init_direct => particle_set_init_direct
     procedure :: transfer => particle_set_transfer
     procedure :: insert => particle_set_insert
     procedure :: recover_color => particle_set_recover_color
     generic :: get_color => get_color_all
     generic :: get_color => get_color_indices
     procedure :: get_color_all => particle_set_get_color_all
     procedure :: get_color_indices => particle_set_get_color_indices
     generic :: set_color => set_color_single
     generic :: set_color => set_color_indices
     generic :: set_color => set_color_all
     procedure :: set_color_single => particle_set_set_color_single
     procedure :: set_color_indices => particle_set_set_color_indices
     procedure :: set_color_all => particle_set_set_color_all
     procedure :: find_prt_invalid_color => particle_set_find_prt_invalid_color
     generic :: get_momenta => get_momenta_all
     generic :: get_momenta => get_momenta_indices
     procedure :: get_momenta_all => particle_set_get_momenta_all
     procedure :: get_momenta_indices => particle_set_get_momenta_indices
     generic :: set_momentum => set_momentum_single
     generic :: set_momentum => set_momentum_indices
     generic :: set_momentum => set_momentum_all
     procedure :: set_momentum_single => particle_set_set_momentum_single
     procedure :: set_momentum_indices => particle_set_set_momentum_indices
     procedure :: set_momentum_all => particle_set_set_momentum_all
     procedure :: recover_momentum => particle_set_recover_momentum
     procedure :: replace_incoming_momenta => particle_set_replace_incoming_momenta
     procedure :: replace_outgoing_momenta => particle_set_replace_outgoing_momenta
     procedure :: get_outgoing_momenta => particle_set_get_outgoing_momenta
     procedure :: parent_add_child => particle_set_parent_add_child
     procedure :: build_radiation => particle_set_build_radiation
     procedure :: write => particle_set_write
     procedure :: write_raw => particle_set_write_raw
     procedure :: read_raw => particle_set_read_raw
     procedure :: get_real_parents => particle_set_get_real_parents
     procedure :: get_real_children => particle_set_get_real_children
     procedure :: get_n_beam => particle_set_get_n_beam
     procedure :: get_n_in => particle_set_get_n_in
     procedure :: get_n_vir => particle_set_get_n_vir
     procedure :: get_n_out => particle_set_get_n_out
     procedure :: get_n_tot => particle_set_get_n_tot
     procedure :: get_n_remnants => particle_set_get_n_remnants
     procedure :: get_particle => particle_set_get_particle
     procedure :: get_indices => particle_set_get_indices
     procedure :: get_in_and_out_momenta => particle_set_get_in_and_out_momenta
     procedure :: without_hadronic_remnants => &
          particle_set_without_hadronic_remnants
     procedure :: without_remnants => particle_set_without_remnants
     procedure :: find_particle => particle_set_find_particle
     procedure :: reverse_find_particle => particle_set_reverse_find_particle
     procedure :: remove_duplicates => particle_set_remove_duplicates
     procedure :: reset_status => particle_set_reset_status
     procedure :: reduce => particle_set_reduce
     procedure :: filter_particles => particle_set_filter_particles
     procedure :: to_hepevt_form => particle_set_to_hepevt_form
     procedure :: fill_interaction => particle_set_fill_interaction
     procedure :: assign_vertices => particle_set_assign_vertices
     procedure :: to_subevt => particle_set_to_subevt
     procedure :: replace => particle_set_replace
     procedure :: order_color_lines => particle_set_order_color_lines
  end type particle_set_t


  interface assignment(=)
     module procedure particle_set_init_particle_set
  end interface

  interface pacify
     module procedure pacify_particle
     module procedure pacify_particle_set
  end interface pacify


contains

  subroutine particle_init_particle (prt_out, prt_in)
    class(particle_t), intent(out) :: prt_out
    type(particle_t), intent(in) :: prt_in
    prt_out%status = prt_in%status
    prt_out%polarization = prt_in%polarization
    prt_out%flv = prt_in%flv
    prt_out%col = prt_in%col
    prt_out%hel = prt_in%hel
    prt_out%pol = prt_in%pol
    prt_out%p = prt_in%p
    prt_out%p2 = prt_in%p2
    if (allocated (prt_in%vertex))  &
       allocate (prt_out%vertex, source=prt_in%vertex)
    if (allocated (prt_in%lifetime))  &
         allocate (prt_out%lifetime, source=prt_in%lifetime)
  end subroutine particle_init_particle

  subroutine particle_init_external &
         (particle, status, pdg, model, col, anti_col, mom)
    class(particle_t), intent(out) :: particle
    integer, intent(in) :: status, pdg, col, anti_col
    class(model_data_t), pointer, intent(in) :: model
    type(vector4_t), intent(in) :: mom
    type(flavor_t) :: flavor
    type(color_t) :: color
    call flavor%init (pdg, model)
    call particle%set_flavor (flavor)
    call color%init_col_acl (col, anti_col)
    call particle%set_color (color)
    call particle%set_status (status)
    call particle%set_momentum (mom)
  end subroutine particle_init_external

  subroutine particle_init_state (prt, state, status, mode)
    class(particle_t), intent(out) :: prt
    type(state_matrix_t), intent(in), target :: state
    integer, intent(in) :: status, mode
    type(state_iterator_t) :: it
    prt%status = status
    call it%init (state)
    prt%flv = it%get_flavor (1)
    if (prt%flv%is_radiated ())  prt%status = PRT_BEAM_REMNANT
    prt%col = it%get_color (1)
    select case (mode)
    case (FM_SELECT_HELICITY)
       prt%hel = it%get_helicity (1)
       if (prt%hel%is_defined ()) then
          prt%polarization = PRT_DEFINITE_HELICITY
       end if
    case (FM_FACTOR_HELICITY)
       call prt%pol%init_state_matrix (state)
       prt%polarization = PRT_GENERIC_POLARIZATION
    end select
  end subroutine particle_init_state

  subroutine particle_final (prt)
    class(particle_t), intent(inout) :: prt
    if (allocated (prt%vertex))  deallocate (prt%vertex)
    if (allocated (prt%lifetime))  deallocate (prt%lifetime)
  end subroutine particle_final

  subroutine particle_write (prt, unit, testflag, compressed, polarization)
    class(particle_t), intent(in) :: prt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag, compressed, polarization
    logical :: comp, pacified, pol
    integer :: u, h1, h2
    real(default) :: pp2
    character(len=7) :: fmt
    character(len=20) :: buffer
    comp = .false.; if (present (compressed))  comp = compressed
    pacified = .false.;  if (present (testflag))  pacified = testflag
    pol = .true.;  if (present (polarization))  pol = polarization
    call pac_fmt (fmt, FMT_19, FMT_16, testflag)
    u = given_output_unit (unit);  if (u < 0)  return
    pp2 = prt%p2
    if (pacified)  call pacify (pp2, tolerance = 1E-10_default)
    select case (prt%status)
    case (PRT_UNDEFINED);    write (u, "(1x, A)", advance="no")  "[-]"
    case (PRT_BEAM);         write (u, "(1x, A)", advance="no")  "[b]"
    case (PRT_INCOMING);     write (u, "(1x, A)", advance="no")  "[i]"
    case (PRT_OUTGOING);     write (u, "(1x, A)", advance="no")  "[o]"
    case (PRT_VIRTUAL);      write (u, "(1x, A)", advance="no")  "[v]"
    case (PRT_RESONANT);     write (u, "(1x, A)", advance="no")  "[r]"
    case (PRT_BEAM_REMNANT); write (u, "(1x, A)", advance="no")  "[x]"
    end select
    write (u, "(1x)", advance="no")
    if (comp) then
       write (u, "(A7,1X)", advance="no") char (prt%flv%get_name ())
       if (pol) then
          select case (prt%polarization)
          case (PRT_DEFINITE_HELICITY)
             ! Integer helicity, assumed diagonal
             call prt%hel%get_indices (h1, h2)
             write (u, "(I2,1X)", advance="no") h1
          case (PRT_GENERIC_POLARIZATION)
             ! No space for full density matrix here
             write (u, "(A2,1X)", advance="no") "*"
          case default
             ! Blank entry if helicity is undefined
             write (u, "(A2,1X)", advance="no") " "
          end select
       end if
       write (u, "(2(I4,1X))", advance="no") &
            prt%col%get_col (), prt%col%get_acl ()
       call write_compressed_integer_array (buffer, prt%parent)
       write (u, "(A,1X)", advance="no") buffer
       call write_compressed_integer_array (buffer, prt%child)
       write (u, "(A,1X)", advance="no") buffer
       call prt%p%write(u, testflag = testflag, compressed = comp)
       write (u, "(F12.3)") pp2
    else
       call prt%flv%write (unit)
       if (prt%col%is_nonzero ()) then
          call color_write (prt%col, unit)
       end if
       if (pol) then
          select case (prt%polarization)
          case (PRT_DEFINITE_HELICITY)
             call prt%hel%write (unit)
             write (u, *)
          case (PRT_GENERIC_POLARIZATION)
             write (u, *)
             call prt%pol%write (unit, state_matrix = .true.)
          case default
             write (u, *)
          end select
       else
          write (u, *)
       end if
       call prt%p%write (unit, testflag = testflag)
       write (u, "(1x,A,1x," // fmt // ")")  "T = ", pp2
       if (allocated (prt%parent)) then
          if (size (prt%parent) /= 0) then
             write (u, "(1x,A,40(1x,I0))")  "Parents: ", prt%parent
          end if
       end if
       if (allocated (prt%child)) then
          if (size (prt%child) /= 0) then
             write (u, "(1x,A,40(1x,I0))")  "Children:", prt%child
          end if
       end if
       if (allocated (prt%vertex)) then
          write (u, "(1x,A,1x," // fmt // ")")  "Vtx t = ", prt%vertex%p(0)
          write (u, "(1x,A,1x," // fmt // ")")  "Vtx x = ", prt%vertex%p(1)
          write (u, "(1x,A,1x," // fmt // ")")  "Vtx y = ", prt%vertex%p(2)
          write (u, "(1x,A,1x," // fmt // ")")  "Vtx z = ", prt%vertex%p(3)
       end if
       if (allocated (prt%lifetime)) then
          write (u, "(1x,A,1x," // fmt // ")")  "Lifetime = ", &
               prt%lifetime
       end if
    end if
  end subroutine particle_write

  subroutine particle_write_raw (prt, u)
    class(particle_t), intent(in) :: prt
    integer, intent(in) :: u
    write (u) prt%status, prt%polarization
    call prt%flv%write_raw (u)
    call prt%col%write_raw (u)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call prt%hel%write_raw (u)
    case (PRT_GENERIC_POLARIZATION)
       call prt%pol%write_raw (u)
    end select
    call vector4_write_raw (prt%p, u)
    write (u) prt%p2
    write (u) allocated (prt%parent)
    if (allocated (prt%parent)) then
       write (u) size (prt%parent)
       write (u) prt%parent
    end if
    write (u) allocated (prt%child)
    if (allocated (prt%child)) then
       write (u) size (prt%child)
       write (u) prt%child
    end if
    write (u) allocated (prt%vertex)
    if (allocated (prt%vertex)) then
       call vector4_write_raw (prt%vertex, u)
    end if
    write (u) allocated (prt%lifetime)
    if (allocated (prt%lifetime)) then
       write (u) prt%lifetime
    end if
  end subroutine particle_write_raw

  subroutine particle_read_raw (prt, u, iostat)
    class(particle_t), intent(out) :: prt
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    logical :: allocated_parent, allocated_child
    logical :: allocated_vertex, allocated_lifetime
    integer :: size_parent, size_child
    read (u, iostat=iostat) prt%status, prt%polarization
    call prt%flv%read_raw (u, iostat=iostat)
    call prt%col%read_raw (u, iostat=iostat)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call prt%hel%read_raw (u, iostat=iostat)
    case (PRT_GENERIC_POLARIZATION)
       call prt%pol%read_raw (u, iostat=iostat)
    end select
    call vector4_read_raw (prt%p, u, iostat=iostat)
    read (u, iostat=iostat) prt%p2
    read (u, iostat=iostat) allocated_parent
    if (allocated_parent) then
       read (u, iostat=iostat) size_parent
       allocate (prt%parent (size_parent))
       read (u, iostat=iostat) prt%parent
    end if
    read (u, iostat=iostat) allocated_child
    if (allocated_child) then
       read (u, iostat=iostat) size_child
       allocate (prt%child (size_child))
       read (u, iostat=iostat) prt%child
    end if
    read (u, iostat=iostat) allocated_vertex
    if (allocated_vertex) then
       allocate (prt%vertex)
       read (u, iostat=iostat) prt%vertex%p
    end if
    read (u, iostat=iostat) allocated_lifetime
    if (allocated_lifetime) then
       allocate (prt%lifetime)
       read (u, iostat=iostat) prt%lifetime
    end if
  end subroutine particle_read_raw

  elemental subroutine particle_reset_status (prt, status)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: status
    prt%status = status
    select case (status)
    case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING)
       prt%p2 = prt%flv%get_mass () ** 2
    end select
  end subroutine particle_reset_status

  elemental subroutine particle_set_color (prt, col)
    class(particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    prt%col = col
  end subroutine particle_set_color

  subroutine particle_set_flavor (prt, flv)
    class(particle_t), intent(inout) :: prt
    type(flavor_t), intent(in) :: flv
    prt%flv = flv
  end subroutine particle_set_flavor

  subroutine particle_set_helicity (prt, hel)
    class(particle_t), intent(inout) :: prt
    type(helicity_t), intent(in) :: hel
    prt%hel = hel
  end subroutine particle_set_helicity

  subroutine particle_set_pol (prt, pol)
    class(particle_t), intent(inout) :: prt
    type(polarization_t), intent(in) :: pol
    prt%pol = pol
  end subroutine particle_set_pol

  subroutine particle_set_model (prt, model)
    class(particle_t), intent(inout) :: prt
    class(model_data_t), intent(in), target :: model
    call prt%flv%set_model (model)
  end subroutine particle_set_model

  elemental subroutine particle_set_momentum (prt, p, p2, on_shell)
    class(particle_t), intent(inout) :: prt
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    prt%p = p
    if (present (on_shell)) then
       if (on_shell) then
          if (prt%flv%is_associated ()) then
             prt%p2 = prt%flv%get_mass () ** 2
             return
          end if
       end if
    end if
    if (present (p2)) then
       prt%p2 = p2
    else
       prt%p2 = p ** 2
    end if
  end subroutine particle_set_momentum

  elemental subroutine particle_set_resonance_flag (prt, resonant)
    class(particle_t), intent(inout) :: prt
    logical, intent(in) :: resonant
    select case (prt%status)
    case (PRT_VIRTUAL)
       if (resonant)  prt%status = PRT_RESONANT
    end select
  end subroutine particle_set_resonance_flag

  subroutine particle_set_children (prt, idx)
    class(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%child))  deallocate (prt%child)
    allocate (prt%child (count (idx /= 0)))
    prt%child = pack (idx, idx /= 0)
  end subroutine particle_set_children

  subroutine particle_set_parents (prt, idx)
    class(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%parent))  deallocate (prt%parent)
    allocate (prt%parent (count (idx /= 0)))
    prt%parent = pack (idx, idx /= 0)
  end subroutine particle_set_parents

  subroutine particle_add_child (prt, new_child)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: new_child
    integer, dimension(:), allocatable :: idx
    integer :: n, i
    n = prt%get_n_children()
    if (n == 0) then
       call prt%set_children ([new_child])
    else
       do i = 1, n
          if (prt%child(i) == new_child) then
             return
          end if
       end do
       allocate (idx (1:n+1))
       idx(1:n) = prt%get_children ()
       idx(n+1) = new_child
       call prt%set_children (idx)
    end if
  end subroutine particle_add_child

  subroutine particle_add_children (prt, new_child)
    class(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: new_child
    integer, dimension(:), allocatable :: idx
    integer :: n
    n = prt%get_n_children()
    if (n == 0) then
       call prt%set_children (new_child)
    else
       allocate (idx (1:n+size(new_child)))
       idx(1:n) = prt%get_children ()
       idx(n+1:n+size(new_child)) = new_child
       call prt%set_children (idx)
    end if
  end subroutine particle_add_children

  elemental subroutine particle_set_status (prt, status)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: status
    prt%status = status
  end subroutine particle_set_status

  subroutine particle_set_polarization (prt, polarization)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: polarization
    prt%polarization = polarization
  end subroutine particle_set_polarization

  subroutine particle_set_vertex_from_vector4 (prt, vertex)
    class(particle_t), intent(inout) :: prt
    type(vector4_t), intent(in) :: vertex
    if (allocated (prt%vertex)) deallocate (prt%vertex)
    allocate (prt%vertex, source=vertex)
  end subroutine particle_set_vertex_from_vector4

  subroutine particle_set_vertex_from_vector3 (prt, vertex)
    class(particle_t), intent(inout) :: prt
    type(vector3_t), intent(in) :: vertex
    type(vector4_t) :: vtx
    vtx = vector4_moving (0._default, vertex)
    if (allocated (prt%vertex)) deallocate (prt%vertex)
    allocate (prt%vertex, source=vtx)
  end subroutine particle_set_vertex_from_vector3

  subroutine particle_set_vertex_from_xyzt (prt, vx, vy, vz, t)
    class(particle_t), intent(inout) :: prt
    real(default), intent(in) :: vx, vy, vz, t
    type(vector4_t) :: vertex
    if (allocated (prt%vertex)) deallocate (prt%vertex)
    vertex = vector4_moving (t, vector3_moving ([vx, vy, vz]))
    allocate (prt%vertex, source=vertex)
  end subroutine particle_set_vertex_from_xyzt

  subroutine particle_set_vertex_from_xyz (prt, vx, vy, vz)
    class(particle_t), intent(inout) :: prt
    real(default), intent(in) :: vx, vy, vz
    type(vector4_t) :: vertex
    if (allocated (prt%vertex)) deallocate (prt%vertex)
    vertex = vector4_moving (0._default, vector3_moving ([vx, vy, vz]))
    allocate (prt%vertex, source=vertex)
  end subroutine particle_set_vertex_from_xyz

  elemental subroutine particle_set_lifetime (prt, lifetime)
    class(particle_t), intent(inout) :: prt
    real(default), intent(in) :: lifetime
    if (allocated (prt%lifetime))  deallocate (prt%lifetime)
    allocate (prt%lifetime, source=lifetime)
  end subroutine particle_set_lifetime

  elemental function particle_get_status (prt) result (status)
    integer :: status
    class(particle_t), intent(in) :: prt
    status = prt%status
  end function particle_get_status

  elemental function particle_is_real (prt, keep_beams) result (flag)
    logical :: flag, kb
    class(particle_t), intent(in) :: prt
    logical, intent(in), optional :: keep_beams
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    select case (prt%status)
    case (PRT_INCOMING, PRT_OUTGOING, PRT_RESONANT)
       flag = .true.
    case (PRT_BEAM)
       flag = kb
    case default
       flag = .false.
    end select
  end function particle_is_real

  elemental function particle_is_colored (particle) result (flag)
    logical :: flag
    class(particle_t), intent(in) :: particle
    flag = particle%col%is_nonzero ()
  end function particle_is_colored

  elemental function particle_is_hadronic_beam_remnant (particle) result (flag)
    class(particle_t), intent(in) :: particle
    logical :: flag
    integer :: pdg
    pdg = particle%flv%get_pdg ()
    flag = particle%status == PRT_BEAM_REMNANT .and. &
         abs(pdg) >= 90 .and. abs(pdg) <= 100
  end function particle_is_hadronic_beam_remnant

  elemental function particle_is_beam_remnant (particle) result (flag)
    class(particle_t), intent(in) :: particle
    logical :: flag
    flag = particle%status == PRT_BEAM_REMNANT
  end function particle_is_beam_remnant

  elemental function particle_get_polarization_status (prt) result (status)
    integer :: status
    class(particle_t), intent(in) :: prt
    status = prt%polarization
  end function particle_get_polarization_status

  elemental function particle_get_pdg (prt) result (pdg)
    integer :: pdg
    class(particle_t), intent(in) :: prt
    pdg = prt%flv%get_pdg ()
  end function particle_get_pdg

  pure function particle_get_color (prt) result (col)
    integer, dimension(2) :: col
    class(particle_t), intent(in) :: prt
    col(1) = prt%col%get_col ()
    col(2) = prt%col%get_acl ()
  end function particle_get_color

  function particle_get_polarization (prt) result (pol)
    class(particle_t), intent(in) :: prt
    type(polarization_t) :: pol
    pol = prt%pol
  end function particle_get_polarization

  function particle_get_flv (prt) result (flv)
    class(particle_t), intent(in) :: prt
    type(flavor_t) :: flv
    flv = prt%flv
  end function particle_get_flv

  function particle_get_col (prt) result (col)
    class(particle_t), intent(in) :: prt
    type(color_t) :: col
    col = prt%col
  end function particle_get_col

  function particle_get_hel (prt) result (hel)
    class(particle_t), intent(in) :: prt
    type(helicity_t) :: hel
    hel = prt%hel
  end function particle_get_hel

  elemental function particle_get_helicity (prt) result (hel)
    integer :: hel
    integer, dimension(2) :: hel_arr
    class(particle_t), intent(in) :: prt
    hel = 0
    if (prt%hel%is_defined () .and. prt%hel%is_diagonal ()) then
       hel_arr = prt%hel%to_pair ()
       hel = hel_arr (1)
    end if
  end function particle_get_helicity

  elemental function particle_get_n_parents (prt) result (n)
    integer :: n
    class(particle_t), intent(in) :: prt
    if (allocated (prt%parent)) then
       n = size (prt%parent)
    else
       n = 0
    end if
  end function particle_get_n_parents

  elemental function particle_get_n_children (prt) result (n)
    integer :: n
    class(particle_t), intent(in) :: prt
    if (allocated (prt%child)) then
       n = size (prt%child)
    else
       n = 0
    end if
  end function particle_get_n_children

  function particle_get_parents (prt) result (parent)
    class(particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: parent
    if (allocated (prt%parent)) then
       allocate (parent (size (prt%parent)))
       parent = prt%parent
    else
       allocate (parent (0))
    end if
  end function particle_get_parents

  function particle_get_children (prt) result (child)
    class(particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: child
    if (allocated (prt%child)) then
       allocate (child (size (prt%child)))
       child = prt%child
    else
       allocate (child (0))
    end if
  end function particle_get_children

  elemental function particle_has_children (prt) result (has_children)
    logical :: has_children
    class(particle_t), intent(in) :: prt
    has_children = .false.
    if (allocated (prt%child)) then
       has_children = size (prt%child) > 0
    end if
  end function particle_has_children

  elemental function particle_has_parents (prt) result (has_parents)
    logical :: has_parents
    class(particle_t), intent(in) :: prt
    has_parents = .false.
    if (allocated (prt%parent)) then
       has_parents = size (prt%parent) > 0
    end if
  end function particle_has_parents

  elemental function particle_get_momentum (prt) result (p)
    type(vector4_t) :: p
    class(particle_t), intent(in) :: prt
    p = prt%p
  end function particle_get_momentum

  elemental function particle_get_p2 (prt) result (p2)
    real(default) :: p2
    class(particle_t), intent(in) :: prt
    p2 = prt%p2
  end function particle_get_p2

  elemental function particle_get_vertex (prt) result (vtx)
    type(vector4_t) :: vtx
    class(particle_t), intent(in) :: prt
    if (allocated (prt%vertex)) then
       vtx = prt%vertex
    else
       vtx = vector4_null
    end if
  end function particle_get_vertex

  elemental function particle_get_lifetime (prt) result (lifetime)
    real(default) :: lifetime
    class(particle_t), intent(in) :: prt
    if (allocated (prt%lifetime)) then
       lifetime = prt%lifetime
    else
       lifetime = 0
    end if
  end function particle_get_lifetime

  pure function particle_momentum_to_pythia6 (prt) result (p)
    real(double), dimension(1:5) :: p
    class(particle_t), intent(in) :: prt
    p = prt%p%to_pythia6 (sqrt (prt%p2))
  end function particle_momentum_to_pythia6

  subroutine particle_set_init_interaction &
       (particle_set, is_valid, int, int_flows, mode, x, &
        keep_correlations, keep_virtual, n_incoming, qn_select)
    class(particle_set_t), intent(out) :: particle_set
    logical, intent(out) :: is_valid
    type(interaction_t), intent(in), target :: int, int_flows
    integer, intent(in) :: mode
    real(default), dimension(2), intent(in) :: x
    logical, intent(in) :: keep_correlations, keep_virtual
    integer, intent(in), optional :: n_incoming
    type(quantum_numbers_t), dimension(:), intent(in), optional :: qn_select
    type(state_matrix_t), dimension(:), allocatable, target :: flavor_state
    type(state_matrix_t), dimension(:), allocatable, target :: single_state
    integer :: n_in, n_vir, n_out, n_tot
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    logical :: ok
    integer :: i, j
    if (present (n_incoming)) then
       n_in  = n_incoming
       n_vir = int%get_n_vir () - n_incoming
    else
       n_in  = int%get_n_in  ()
       n_vir = int%get_n_vir ()
    end if
    n_out = int%get_n_out ()
    n_tot = int%get_n_tot ()
    particle_set%n_in  = n_in
    particle_set%n_out = n_out
    if (keep_virtual) then
       particle_set%n_vir = n_vir
       particle_set%n_tot = n_tot
    else
       particle_set%n_vir = 0
       particle_set%n_tot = n_in + n_out
    end if
    particle_set%factorization_mode = mode
    allocate (qn (n_tot, 1))
    if (.not. present (qn_select)) then
       call int%factorize &
            (FM_IGNORE_HELICITY, x(1), is_valid, flavor_state)
       do i = 1, n_tot
          qn(i,:) = flavor_state(i)%get_quantum_number (1)
       end do
    else
       do i = 1, n_tot
          qn(i,:) = qn_select(i)
       end do
    end if
    if (keep_correlations .and. keep_virtual) then
       call particle_set%correlated_state%final ()
       call int_flows%factorize (mode, x(2), ok, &
            single_state, particle_set%correlated_state, qn(:,1))
    else
       call int_flows%factorize (mode, x(2), ok, &
            single_state, qn_in=qn(:,1))
    end if
    is_valid = is_valid .and. ok
    allocate (particle_set%prt (particle_set%n_tot))
    j = 1
    do i = 1, n_tot
       if (i <= n_in) then
          call particle_set%prt(j)%init (single_state(i), PRT_INCOMING, mode)
          call particle_set%prt(j)%set_momentum (int%get_momentum (i))
       else if (i <= n_in + n_vir) then
          if (.not. keep_virtual)  cycle
          call particle_set%prt(j)%init &
               (single_state(i), PRT_VIRTUAL, mode)
          call particle_set%prt(j)%set_momentum (int%get_momentum (i))
       else
          call particle_set%prt(j)%init (single_state(i), PRT_OUTGOING, mode)
          call particle_set%prt(j)%set_momentum &
               (int%get_momentum (i), on_shell = .true.)
       end if
       if (keep_virtual) then
          call particle_set%prt(j)%set_children &
               (interaction_get_children (int, i))
          call particle_set%prt(j)%set_parents &
               (interaction_get_parents (int, i))
       end if
       j = j + 1
    end do
    if (keep_virtual) then
       call particle_set_resonance_flag &
            (particle_set%prt, int%get_resonance_flags ())
    end if
    if (allocated (flavor_state)) then
       do i = 1, size(flavor_state)
          call flavor_state(i)%final ()
       end do
    end if
    do i = 1, size(single_state)
       call single_state(i)%final ()
    end do
  end subroutine particle_set_init_interaction

  subroutine particle_set_init_particle_set (pset_out, pset_in)
    class(particle_set_t), intent(out) :: pset_out
    type(particle_set_t), intent(in) :: pset_in
    integer :: i
    pset_out%n_beam = pset_in%n_beam
    pset_out%n_in   = pset_in%n_in
    pset_out%n_vir  = pset_in%n_vir
    pset_out%n_out  = pset_in%n_out
    pset_out%n_tot  = pset_in%n_tot
    pset_out%factorization_mode = pset_in%factorization_mode
    if (allocated (pset_in%prt)) then
       allocate (pset_out%prt (size (pset_in%prt)))
       do i = 1, size (pset_in%prt)
          pset_out%prt(i) = pset_in%prt(i)
       end do
    end if
    pset_out%correlated_state = pset_in%correlated_state
  end subroutine particle_set_init_particle_set

  subroutine particle_set_set_model (particle_set, model)
    class(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    integer :: i
    do i = 1, particle_set%n_tot
       call particle_set%prt(i)%set_model (model)
    end do
    call particle_set%correlated_state%set_model (model)
  end subroutine particle_set_set_model

  subroutine particle_set_final (particle_set)
    class(particle_set_t), intent(inout) :: particle_set
    integer :: i
    if (allocated (particle_set%prt)) then
       do i = 1, size(particle_set%prt)
          call particle_set%prt(i)%final ()
       end do
       deallocate (particle_set%prt)
    end if
    call particle_set%correlated_state%final ()
  end subroutine particle_set_final

  subroutine particle_set_basic_init (particle_set, n_beam, n_in, n_vir, n_out)
    class(particle_set_t), intent(out) :: particle_set
    integer, intent(in) :: n_beam, n_in, n_vir, n_out
    particle_set%n_beam = n_beam
    particle_set%n_in = n_in
    particle_set%n_vir = n_vir
    particle_set%n_out = n_out
    particle_set%n_tot = n_beam + n_in + n_vir + n_out
    allocate (particle_set%prt (particle_set%n_tot))
  end subroutine particle_set_basic_init

  subroutine particle_set_init_direct (particle_set, &
       n_beam, n_in, n_vir, n_out, pdg, model)
    class(particle_set_t), intent(out) :: particle_set
    integer, intent(in) :: n_beam
    integer, intent(in) :: n_in
    integer, intent(in) :: n_vir
    integer, intent(in) :: n_out
    integer, dimension(:), intent(in) :: pdg
    class(model_data_t), intent(in), target :: model
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i, k, n
    call particle_set%basic_init (n_beam, n_in, n_vir, n_out)
    n = 0
    call particle_set%prt(n+1:n+n_beam)%reset_status (PRT_BEAM)
    do i = n+1, n+n_beam
       call particle_set%prt(i)%set_children &
            ([(k, k=n+n_beam+1, n+n_beam+n_in+n_vir)])
    end do
    n = n + n_beam
    call particle_set%prt(n+1:n+n_in)%reset_status (PRT_INCOMING)
    do i = n+1, n+n_in
       call particle_set%prt(i)%set_parents &
            ([(k, k=n-n_beam+1, n)])
       call particle_set%prt(i)%set_children &
            ([(k, k=n+n_in+n_vir+1, n+n_in+n_vir+n_out)])
    end do
    n = n + n_in
    call particle_set%prt(n+1:n+n_vir)%reset_status (PRT_VIRTUAL)
    do i = n+1, n+n_vir
       call particle_set%prt(i)%set_parents &
            ([(k, k=n-n_in-n_beam+1, n-n_in)])
    end do
    n = n + n_vir
    call particle_set%prt(n+1:n+n_out)%reset_status (PRT_OUTGOING)
    do i = n+1, n+n_out
       call particle_set%prt(i)%set_parents &
            ([(k, k=n-n_vir-n_in+1, n-n_vir)])
    end do
    allocate (flv (particle_set%n_tot))
    call flv%init (pdg, model)
    do i = 1, particle_set%n_tot
       call particle_set%prt(i)%set_flavor (flv(i))
    end do
  end subroutine particle_set_init_direct

  subroutine particle_set_transfer (pset, source, n_new, map)
    class(particle_set_t), intent(out) :: pset
    class(particle_set_t), intent(in) :: source
    integer, intent(in) :: n_new
    integer, dimension(:), intent(in) :: map
    integer :: i
    call pset%basic_init &
         (source%n_beam, source%n_in, source%n_vir + n_new, source%n_out)
    do i = 1, source%n_tot
       call pset%prt(map(i))%reset_status (source%prt(i)%get_status ())
       call pset%prt(map(i))%set_flavor (source%prt(i)%get_flv ())
       call pset%prt(map(i))%set_color (source%prt(i)%get_col ())
       call pset%prt(map(i))%set_parents (map (source%prt(i)%get_parents ()))
       call pset%prt(map(i))%set_children (map (source%prt(i)%get_children ()))
    end do
  end subroutine particle_set_transfer
  
  subroutine particle_set_insert (pset, i, status, flv, child)
    class(particle_set_t), intent(inout) :: pset
    integer, intent(in) :: i
    integer, intent(in) :: status
    type(flavor_t), intent(in) :: flv
    integer, dimension(:), intent(in) :: child
    integer, dimension(:), allocatable :: p_child, parent
    integer :: j, k, c, n_parent
    logical :: no_match
    call pset%prt(i)%reset_status (status)
    call pset%prt(i)%set_flavor (flv)
    call pset%prt(i)%set_children (child)
    n_parent = pset%prt(i)%get_n_parents ()
    do j = 1, i - 1
       p_child = pset%prt(j)%get_children ()
       no_match = .true.
       do k = 1, size (p_child)
          if (any (p_child(k) == child)) then
             if (n_parent == 0 .and. no_match) then
                if (.not. allocated (parent)) then
                   parent = [j]
                else
                   parent = [parent, j]
                end if
                p_child(k) = i
             else
                p_child(k) = 0
             end if
             no_match = .false.
          end if
       end do
       if (.not. no_match) then
          p_child = pack (p_child, p_child /= 0)
          call pset%prt(j)%set_children (p_child)
       end if
    end do
    if (n_parent == 0) then
       call pset%prt(i)%set_parents (parent)
    end if
    do j = 1, size (child)
       c = child(j)
       call pset%prt(c)%set_parents ([i])
    end do
  end subroutine particle_set_insert
  
  subroutine particle_set_recover_color (pset, i)
    class(particle_set_t), intent(inout) :: pset
    integer, intent(in) :: i
    type(color_t) :: col
    integer, dimension(:), allocatable :: child
    integer :: j
    child = pset%prt(i)%get_children ()
    if (size (child) > 0) then
       col = pset%prt(child(1))%get_col ()
       do j = 2, size (child)
          col = col .fuse. pset%prt(child(j))%get_col ()
       end do
       call pset%prt(i)%set_color (col)
    end if
  end subroutine particle_set_recover_color
       
  function particle_set_get_color_all (particle_set) result (col)
    class(particle_set_t), intent(in) :: particle_set
    type(color_t), dimension(:), allocatable :: col
    allocate (col (size (particle_set%prt)))
    col = particle_set%prt%col
  end function particle_set_get_color_all

  function particle_set_get_color_indices (particle_set, indices) result (col)
     type(color_t), dimension(:), allocatable :: col
     class(particle_set_t), intent(in) :: particle_set
     integer, intent(in), dimension(:), allocatable :: indices
     integer :: i
     allocate (col (size (indices)))
     do i = 1, size (indices)
        col(i) = particle_set%prt(indices(i))%col
     end do
  end function particle_set_get_color_indices

  subroutine particle_set_set_color_single (particle_set, i, col)
    class(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: i
    type(color_t), intent(in) :: col
    call particle_set%prt(i)%set_color (col)
  end subroutine particle_set_set_color_single

  subroutine particle_set_set_color_indices (particle_set, indices, col)
    class(particle_set_t), intent(inout) :: particle_set
    integer, dimension(:), intent(in) :: indices
    type(color_t), dimension(:), intent(in) :: col
    integer :: i
    do i = 1, size (indices)
       call particle_set%prt(indices(i))%set_color (col(i))
    end do
  end subroutine particle_set_set_color_indices

  subroutine particle_set_set_color_all (particle_set, col)
    class(particle_set_t), intent(inout) :: particle_set
    type(color_t), dimension(:), intent(in) :: col
    call particle_set%prt%set_color (col)
  end subroutine particle_set_set_color_all

  subroutine particle_set_find_prt_invalid_color (particle_set, index, prt)
    class(particle_set_t), intent(in) :: particle_set
    integer, dimension(:), allocatable, intent(out) :: index
    type(particle_t), dimension(:), allocatable, intent(out), optional :: prt
    type(flavor_t) :: flv
    type(color_t) :: col
    logical, dimension(:), allocatable :: mask
    integer :: i, n, n_invalid
    n = size (particle_set%prt)
    allocate (mask (n))
    do i = 1, n
       associate (prt => particle_set%prt(i))
         flv = prt%get_flv ()
         col = prt%get_col ()
         mask(i) = flv%get_color_type () /= col%get_type ()
       end associate
    end do
    index = pack ([(i, i = 1, n)], mask)
    if (present (prt))  prt = pack (particle_set%prt, mask)
  end subroutine particle_set_find_prt_invalid_color
  
  function particle_set_get_momenta_all (particle_set) result (p)
    class(particle_set_t), intent(in) :: particle_set
    type(vector4_t), dimension(:), allocatable :: p
    allocate (p (size (particle_set%prt)))
    p = particle_set%prt%p
  end function particle_set_get_momenta_all

  function particle_set_get_momenta_indices (particle_set, indices) result (p)
     type(vector4_t), dimension(:), allocatable :: p
     class(particle_set_t), intent(in) :: particle_set
     integer, intent(in), dimension(:), allocatable :: indices
     integer :: i
     allocate (p (size (indices)))
     do i = 1, size (indices)
        p(i) = particle_set%prt(indices(i))%p
     end do
  end function particle_set_get_momenta_indices

  subroutine particle_set_set_momentum_single &
       (particle_set, i, p, p2, on_shell)
    class(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: i
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    call particle_set%prt(i)%set_momentum (p, p2, on_shell)
  end subroutine particle_set_set_momentum_single

  subroutine particle_set_set_momentum_indices &
       (particle_set, indices, p, p2, on_shell)
    class(particle_set_t), intent(inout) :: particle_set
    integer, dimension(:), intent(in) :: indices
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), dimension(:), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    integer :: i
    if (present (p2)) then
       do i = 1, size (indices)
          call particle_set%prt(indices(i))%set_momentum (p(i), p2(i), on_shell)
       end do
    else
       do i = 1, size (indices)
          call particle_set%prt(indices(i))%set_momentum &
               (p(i), on_shell=on_shell)
       end do
    end if
  end subroutine particle_set_set_momentum_indices

  subroutine particle_set_set_momentum_all (particle_set, p, p2, on_shell)
    class(particle_set_t), intent(inout) :: particle_set
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), dimension(:), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    call particle_set%prt%set_momentum (p, p2, on_shell)
  end subroutine particle_set_set_momentum_all

  subroutine particle_set_recover_momentum (particle_set, i)
    class(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: i
    type(vector4_t), dimension(:), allocatable :: p
    integer, dimension(:), allocatable :: index
    index = particle_set%prt(i)%get_children ()
    p = particle_set%get_momenta (index)
    call particle_set%set_momentum (i, sum (p))
  end subroutine particle_set_recover_momentum
  
  subroutine particle_set_replace_incoming_momenta (particle_set, p)
    class(particle_set_t), intent(inout) :: particle_set
    type(vector4_t), intent(in), dimension(:) :: p
    integer :: i, j
    i = 1
    do j = 1, particle_set%get_n_tot ()
       if (particle_set%prt(j)%get_status () == PRT_INCOMING) then
          particle_set%prt(j)%p = p(i)
          i = i + 1
          if (i > particle_set%n_in) exit
       end if
    end do
  end subroutine particle_set_replace_incoming_momenta

  subroutine particle_set_replace_outgoing_momenta (particle_set, p)
    class(particle_set_t), intent(inout) :: particle_set
    type(vector4_t), intent(in), dimension(:) :: p
    integer :: i, j
    i = particle_set%n_in + 1
    do j = 1, particle_set%n_tot
       if (particle_set%prt(j)%get_status () == PRT_OUTGOING) then
          particle_set%prt(j)%p = p(i)
          i = i + 1
       end if
    end do
  end subroutine particle_set_replace_outgoing_momenta

  function particle_set_get_outgoing_momenta (particle_set) result (p)
    class(particle_set_t), intent(in) :: particle_set
    type(vector4_t), dimension(:), allocatable :: p
    integer :: i, k
    allocate (p (count (particle_set%prt%get_status () == PRT_OUTGOING)))
    k = 0
    do i = 1, size (particle_set%prt)
       if (particle_set%prt(i)%get_status () == PRT_OUTGOING) then
          k = k + 1
          p(k) = particle_set%prt(i)%get_momentum ()
       end if
    end do
  end function particle_set_get_outgoing_momenta

  subroutine particle_set_parent_add_child (particle_set, parent, child)
    class(particle_set_t), intent(inout) :: particle_set
    integer, intent(in) :: parent, child
    call particle_set%prt(child)%set_parents ([parent])
    call particle_set%prt(parent)%add_child (child)
  end subroutine particle_set_parent_add_child

  subroutine particle_set_build_radiation (particle_set, p_radiated, &
     emitter, flv_radiated, model, r_color)
     class(particle_set_t), intent(inout) :: particle_set
     type(vector4_t), intent(in), dimension(:) :: p_radiated
     integer, intent(in) :: emitter
     integer, intent(in), dimension(:) :: flv_radiated
     class(model_data_t), intent(in), target :: model
     real(default), intent(in) :: r_color
     type(particle_set_t) :: new_particle_set
     type(particle_t) :: new_particle
     integer :: i
     integer :: pdg_index_emitter, pdg_index_radiation
     integer, dimension(:), allocatable :: parents, children
     type(flavor_t) :: new_flv
     logical, dimension(:), allocatable :: status_mask
     integer, dimension(:), allocatable :: &
          i_in1, i_beam1, i_remnant1, i_virt1, i_out1
     integer, dimension(:), allocatable :: &
          i_in2, i_beam2, i_remnant2, i_virt2, i_out2
     integer :: n_in1, n_beam1, n_remnant1, n_virt1, n_out1
     integer :: n_in2, n_beam2, n_remnant2, n_virt2, n_out2
     integer :: n, n_tot
     integer :: i_emitter

     n = particle_set%get_n_tot ()
     allocate (status_mask (n))
     do i = 1, n
        status_mask(i) = particle_set%prt(i)%get_status () == PRT_INCOMING
     end do
     n_in1 = count (status_mask)
     allocate (i_in1 (n_in1))
     i_in1 = particle_set%get_indices (status_mask)
     do i = 1, n
        status_mask(i) = particle_set%prt(i)%get_status () == PRT_BEAM
     end do
     n_beam1 = count (status_mask)
     allocate (i_beam1 (n_beam1))
     i_beam1 = particle_set%get_indices (status_mask)
     do i = 1, n
        status_mask(i) = particle_set%prt(i)%get_status () == PRT_BEAM_REMNANT
     end do
     n_remnant1 = count (status_mask)
     allocate (i_remnant1 (n_remnant1))
     i_remnant1 = particle_set%get_indices (status_mask)
     do i = 1, n
        status_mask(i) = particle_set%prt(i)%get_status () == PRT_VIRTUAL
     end do
     n_virt1 = count (status_mask)
     allocate (i_virt1 (n_virt1))
     i_virt1 = particle_set%get_indices (status_mask)
     do i = 1, n
        status_mask(i) = particle_set%prt(i)%get_status () == PRT_OUTGOING
     end do
     n_out1 = count (status_mask)
     allocate (i_out1 (n_out1))
     i_out1 = particle_set%get_indices (status_mask)

     n_in2 = n_in1; n_beam2 = n_beam1; n_remnant2 = n_remnant1
     n_virt2 = n_virt1 + n_out1
     n_out2 = n_out1 + 1
     n_tot = n_in2 + n_beam2 + n_remnant2 + n_virt2 + n_out2

     allocate (i_in2 (n_in2), i_beam2 (n_beam2), i_remnant2 (n_remnant2))
     i_in2 = i_in1; i_beam2 = i_beam1; i_remnant2 = i_remnant1

     allocate (i_virt2 (n_virt2))
     i_virt2(1 : n_virt1) = i_virt1
     i_virt2(n_virt1 + 1 : n_virt2) = i_out1

     allocate (i_out2 (n_out2))
     i_out2(1 : n_out1) = i_out1(1 : n_out1) + n_out1
     i_out2(n_out2) = n_tot

     new_particle_set%n_beam = n_beam2
     new_particle_set%n_in = n_in2
     new_particle_set%n_vir = n_virt2
     new_particle_set%n_out = n_out2
     new_particle_set%n_tot = n_tot
     new_particle_set%correlated_state = particle_set%correlated_state
     allocate (new_particle_set%prt (n_tot))
     if (size (i_beam1) > 0) new_particle_set%prt(i_beam2) = particle_set%prt(i_beam1)
     if (size (i_remnant1) > 0) new_particle_set%prt(i_remnant2) = particle_set%prt(i_remnant1)
     do i = 1, n_virt1
        new_particle_set%prt(i_virt2(i)) = particle_set%prt(i_virt1(i))
     end do

     do i = n_virt1 + 1, n_virt2
        new_particle_set%prt(i_virt2(i)) = particle_set%prt(i_out1(i - n_virt1))
        call new_particle_set%prt(i_virt2(i))%reset_status (PRT_VIRTUAL)
     end do

     do i = 1, n_in2
        new_particle_set%prt(i_in2(i)) = particle_set%prt(i_in1(i))
        new_particle_set%prt(i_in2(i))%p = p_radiated (i)
     end do

     do i = 1, n_out2 - 1
        new_particle_set%prt(i_out2(i)) = particle_set%prt(i_out1(i))
        new_particle_set%prt(i_out2(i))%p = p_radiated(i + n_in2)
        call new_particle_set%prt(i_out2(i))%reset_status (PRT_OUTGOING)
     end do

     call new_particle%reset_status (PRT_OUTGOING)
     call new_particle%set_momentum (p_radiated (n_in2 + n_out2))

     !!! Helicity and polarization handling is missing at this point
     !!! Also, no helicities or polarizations yet
     pdg_index_emitter = flv_radiated (emitter)
     pdg_index_radiation = flv_radiated (n_in2 + n_out2)
     call new_flv%init (pdg_index_radiation, model)
     i_emitter = emitter + n_virt2 + n_remnant2 + n_beam2

     call reassign_colors (new_particle, new_particle_set%prt(i_emitter), &
        pdg_index_radiation, pdg_index_emitter, r_color)

     call new_particle%set_flavor (new_flv)
     new_particle_set%prt(n_tot) = new_particle

     allocate (children (n_out2))
     children = i_out2
     do i = n_in2 + n_beam2 + n_remnant2 + n_virt1 + 1, n_in2 + n_beam2 + n_remnant2 + n_virt2
        call new_particle_set%prt(i)%set_children (children)
     end do

     !!! Set proper parents for outgoing particles
     allocate (parents (n_out1))
     parents = i_out1
     do i = n_in2 + n_beam2 + n_remnant2 + n_virt2 + 1, n_tot
        call new_particle_set%prt(i)%set_parents (parents)
     end do
     call particle_set%init (new_particle_set)
  contains

      subroutine set_color_offset (particle_set)
        type(particle_set_t), intent(inout) :: particle_set
        integer, dimension(2) :: color
        integer :: i, i_color_max
        type(color_t) :: new_color

        i_color_max = 0
        do i = 1, size (particle_set%prt)
           associate (prt => particle_set%prt(i))
              if (prt%get_status () <= PRT_INCOMING) cycle
              color = prt%get_color ()
              i_color_max = maxval([i_color_max, color(1), color(2)])
           end associate
        end do


        do i = 1, size (particle_set%prt)
           associate (prt => particle_set%prt(i))
              if (prt%get_status () /= PRT_OUTGOING) cycle
              color = prt%get_color ()
              where (color /= 0) color = color + i_color_max
              call new_color%init_col_acl (color(1), color(2))
              call prt%set_color (new_color)
           end associate
        end do
      end subroutine set_color_offset

    subroutine reassign_colors (prt_radiated, prt_emitter, i_rad, i_em, r_col)
      type(particle_t), intent(inout) :: prt_radiated, prt_emitter
      integer, intent(in) :: i_rad, i_em
      real(default), intent(in) :: r_col
      type(color_t) :: col_rad, col_em
      if (is_quark (i_em) .and. is_gluon (i_rad)) then
         call reassign_colors_qg (prt_emitter, col_rad, col_em)
      else if (is_gluon (i_em) .and. is_gluon (i_rad)) then
         call reassign_colors_gg (prt_emitter, r_col, col_rad, col_em)
      else if (is_gluon (i_em) .and. is_quark (i_rad)) then
         call reassign_colors_qq (prt_emitter, i_em, col_rad, col_em)
      else
         call msg_fatal ("Invalid splitting")
      end if
      call prt_emitter%set_color (col_em)
      call prt_radiated%set_color (col_rad)
    end subroutine reassign_colors

    subroutine reassign_colors_qg (prt_emitter, col_rad, col_em)
      type(particle_t), intent(in) :: prt_emitter
      type(color_t), intent(out) :: col_rad, col_em
      integer, dimension(2) :: color_rad, color_em
      integer :: i1, i2
      integer :: new_color_index
      logical :: is_anti_quark

      color_em = prt_emitter%get_color ()
      i1 = 1; i2 = 2
      is_anti_quark = color_em(2) /= 0
      if (is_anti_quark) then
         i1 = 2; i2 = 1
      end if
      new_color_index = color_em(i1)+1
      color_rad(i1) = color_em(i1)
      color_rad(i2) = new_color_index
      color_em(i1) = new_color_index
      call col_em%init_col_acl (color_em(1), color_em(2))
      call col_rad%init_col_acl (color_rad(1), color_rad(2))
    end subroutine reassign_colors_qg

    subroutine reassign_colors_gg (prt_emitter, random, col_rad, col_em)
      !!! NOT TESTED YET
      type(particle_t), intent(in) :: prt_emitter
      real(default), intent(in) :: random
      type(color_t), intent(out) :: col_rad, col_em
      integer, dimension(2) :: color_rad, color_em
      integer :: i1, i2
      integer :: new_color_index

      color_em = prt_emitter%get_color ()
      new_color_index = maxval (abs (color_em))
      i1 = 1; i2 = 2
      if (random < 0.5) then
         i1 = 2; i2 = 1
      end if
      color_rad(i1) = new_color_index
      color_rad(i2) = color_em(i2)
      color_em(i2) = new_color_index
      call col_em%init_col_acl (color_em(1), color_em(2))
      call col_rad%init_col_acl (color_rad(1), color_rad(2))
    end subroutine reassign_colors_gg

    subroutine reassign_colors_qq (prt_emitter, pdg_emitter, col_rad, col_em)
      !!! NOT TESTED YET
      type(particle_t), intent(in) :: prt_emitter
      integer, intent(in) :: pdg_emitter
      type(color_t), intent(out) :: col_rad, col_em
      integer, dimension(2) :: color_rad, color_em
      integer :: i1, i2
      logical :: is_anti_quark

      color_em = prt_emitter%get_color ()
      i1 = 1; i2 = 2
      is_anti_quark = pdg_emitter < 0
      if (is_anti_quark) then
         i1 = 2; i1 = 1
      end if
      color_em(i2) = 0
      color_rad(i1) = 0
      color_rad(i2) = color_em(i1)
      call col_em%init_col_acl (color_em(1), color_em(2))
      call col_rad%init_col_acl (color_rad(1), color_rad(2))
    end subroutine reassign_colors_qq
  end subroutine particle_set_build_radiation

  subroutine particle_set_write &
    (particle_set, unit, testflag, summary, compressed)
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag, summary, compressed
    logical :: summ, comp, pol
    type(vector4_t) :: sum_vec
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    summ = .false.; if (present (summary)) summ = summary
    comp = .false.; if (present (compressed)) comp = compressed
    pol = particle_set%factorization_mode /= FM_IGNORE_HELICITY
    write (u, "(1x,A)") "Particle set:"
    call write_separator (u)
    if (comp) then
       if (pol) then
          write (u, &
               "((A4,1X),(A6,1X),(A7,1X),(A3),2(A4,1X),2(A20,1X),5(A12,1X))") &
               "Nr", "Status", "Flavor", "Hel", "Col", "ACol", &
               "Parents", "Children", &
               "P(0)", "P(1)", "P(2)", "P(3)", "P^2"
       else
          write (u, &
               "((A4,1X),(A6,1X),(A7,1X),2(A4,1X),2(A20,1X),5(A12,1X))") &
               "Nr", "Status", "Flavor", "Col", "ACol", &
               "Parents", "Children", &
               "P(0)", "P(1)", "P(2)", "P(3)", "P^2"
       end if
    end if
    if (particle_set%n_tot /= 0) then
       do i = 1, particle_set%n_tot
          if (comp) then
             write (u, "(I4,1X,2X)", advance="no") i
          else
             write (u, "(1x,A,1x,I0)", advance="no") "Particle", i
          end if
          call particle_set%prt(i)%write (u, testflag = testflag, &
               compressed = comp, polarization = pol)
       end do
       if (particle_set%correlated_state%is_defined ()) then
          call write_separator (u)
          write (u, *) "Correlated state density matrix:"
          call particle_set%correlated_state%write (u)
       end if
       if (summ) then
          call write_separator (u)
          write (u, "(A)", advance="no") &
               "Sum of incoming momenta: p(0:3) =     "
          sum_vec = sum (particle_set%prt%p, &
               mask=particle_set%prt%get_status () == PRT_INCOMING)
          call pacify (sum_vec, tolerance = 1E-3_default)
          call sum_vec%write (u, compressed=.true.)
          write (u, *)
          write (u, "(A)", advance="no") &
               "Sum of beam remnant momenta: p(0:3) = "
          sum_vec = sum (particle_set%prt%p, &
               mask=particle_set%prt%get_status () == PRT_BEAM_REMNANT)
          call pacify (sum_vec, tolerance = 1E-3_default)
          call sum_vec%write (u, compressed=.true.)
          write (u, *)
          write (u, "(A)", advance="no") &
               "Sum of outgoing momenta: p(0:3) =     "
          sum_vec = sum (particle_set%prt%p, &
               mask=particle_set%prt%get_status () == PRT_OUTGOING)
          call pacify (sum_vec, tolerance = 1E-3_default)
          call sum_vec%write (u, compressed=.true.)
          write (u, "(A)") ""
       end if
    else
       write (u, "(3x,A)") "[empty]"
    end if
  end subroutine particle_set_write

  subroutine particle_set_write_raw (particle_set, u)
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: u
    integer :: i
    write (u) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    write (u) particle_set%factorization_mode
    write (u) particle_set%n_tot
    do i = 1, particle_set%n_tot
       call particle_set%prt(i)%write_raw (u)
    end do
    call particle_set%correlated_state%write_raw (u)
  end subroutine particle_set_write_raw

  subroutine particle_set_read_raw (particle_set, u, iostat)
    class(particle_set_t), intent(out) :: particle_set
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    integer :: i
    read (u, iostat=iostat) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    read (u, iostat=iostat) particle_set%factorization_mode
    read (u, iostat=iostat) particle_set%n_tot
    allocate (particle_set%prt (particle_set%n_tot))
    do i = 1, size (particle_set%prt)
       call particle_set%prt(i)%read_raw (u, iostat=iostat)
    end do
    call particle_set%correlated_state%read_raw (u, iostat=iostat)
  end subroutine particle_set_read_raw

  function particle_set_get_real_parents (pset, i, keep_beams) result (parent)
    integer, dimension(:), allocatable :: parent
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, intent(in), optional :: keep_beams
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_parent, is_real_parent
    logical :: kb
    integer :: j, k
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = pset%prt%is_real (kb)
    allocate (is_parent (pset%n_tot), is_real_parent (pset%n_tot))
    is_real_parent = .false.
    is_parent = .false.
    is_parent(pset%prt(i)%get_parents()) = .true.
    do while (any (is_parent))
       where (is_real .and. is_parent)
          is_real_parent = .true.
          is_parent = .false.
       end where
       mark_next_parent: do j = size (is_parent), 1, -1
          if (is_parent(j)) then
             is_parent(pset%prt(j)%get_parents()) = .true.
             is_parent(j) = .false.
             exit mark_next_parent
          end if
       end do mark_next_parent
    end do
    allocate (parent (count (is_real_parent)))
    j = 0
    do k = 1, size (is_parent)
       if (is_real_parent(k)) then
          j = j + 1
          parent(j) = k
       end if
    end do
  end function particle_set_get_real_parents

  function particle_set_get_real_children (pset, i, keep_beams) result (child)
    integer, dimension(:), allocatable :: child
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_child, is_real_child
    logical, intent(in), optional :: keep_beams
    integer :: j, k
    logical :: kb
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = pset%prt%is_real (kb)
    is_real = pset%prt%is_real (kb)
    allocate (is_child (pset%n_tot), is_real_child (pset%n_tot))
    is_real_child = .false.
    is_child = .false.
    is_child(pset%prt(i)%get_children()) = .true.
    do while (any (is_child))
       where (is_real .and. is_child)
          is_real_child = .true.
          is_child = .false.
       end where
       mark_next_child: do j = 1, size (is_child)
          if (is_child(j)) then
             is_child(pset%prt(j)%get_children()) = .true.
             is_child(j) = .false.
             exit mark_next_child
          end if
       end do mark_next_child
    end do
    allocate (child (count (is_real_child)))
    j = 0
    do k = 1, size (is_child)
       if (is_real_child(k)) then
          j = j + 1
          child(j) = k
       end if
    end do
  end function particle_set_get_real_children

  function particle_set_get_n_beam (pset) result (n_beam)
     class(particle_set_t), intent(in) :: pset
     integer :: n_beam
     n_beam = pset%n_beam
  end function particle_set_get_n_beam

  function particle_set_get_n_in (pset) result (n_in)
     class(particle_set_t), intent(in) :: pset
     integer :: n_in
     n_in = pset%n_in
  end function particle_set_get_n_in

  function particle_set_get_n_vir (pset) result (n_vir)
     class(particle_set_t), intent(in) :: pset
     integer :: n_vir
     n_vir = pset%n_vir
   end function particle_set_get_n_vir

  function particle_set_get_n_out (pset) result (n_out)
     class(particle_set_t), intent(in) :: pset
     integer :: n_out
     n_out = pset%n_out
  end function particle_set_get_n_out

  function particle_set_get_n_tot (pset) result (n_tot)
     class(particle_set_t), intent(in) :: pset
     integer :: n_tot
     n_tot = pset%n_tot
  end function particle_set_get_n_tot

  function particle_set_get_n_remnants (pset) result (n_remn)
    class(particle_set_t), intent(in) :: pset
    integer :: n_remn
    if (allocated (pset%prt)) then
       n_remn = count (pset%prt%get_status () == PRT_BEAM_REMNANT)
    else
       n_remn = 0
    end if
  end function particle_set_get_n_remnants

  function particle_set_get_particle (pset, index) result (particle)
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: index
    type(particle_t) :: particle
    particle = pset%prt(index)
  end function particle_set_get_particle

  pure function particle_set_get_indices (pset, mask) result (finals)
    integer, dimension(:), allocatable :: finals
    class(particle_set_t), intent(in) :: pset
    logical, dimension(:), intent(in) :: mask
    integer, dimension(size(mask)) :: indices
    integer :: i
    allocate (finals (count (mask)))
    indices = [(i, i=1, pset%n_tot)]
    finals = pack (indices, mask)
  end function particle_set_get_indices

  function particle_set_get_in_and_out_momenta (pset) result (phs_point)
    type(phs_point_t) :: phs_point
    class(particle_set_t), intent(in) :: pset
    logical, dimension(:), allocatable :: mask
    integer, dimension(:), allocatable :: indices
    type(vector4_t), dimension(:), allocatable :: p
    allocate (mask (pset%get_n_tot ()))
    allocate (p (size (pset%prt)))
    mask = pset%prt%status == PRT_INCOMING .or. &
           pset%prt%status == PRT_OUTGOING
    allocate (indices (count (mask)))
    indices = pset%get_indices (mask)
    phs_point = pset%get_momenta (indices)
  end function particle_set_get_in_and_out_momenta

  subroutine particle_set_without_hadronic_remnants &
         (particle_set, particles, n_particles, n_extra)
    class(particle_set_t), intent(inout) :: particle_set
    type(particle_t), dimension(:), allocatable, intent(out) :: particles
    integer, intent(out) :: n_particles
    integer, intent(in) :: n_extra
    logical, dimension(:), allocatable :: no_hadronic_remnants, &
         no_hadronic_children
    integer, dimension(:), allocatable :: children, new_children
    integer :: i, j, k, first_remnant
    first_remnant = particle_set%n_tot
    do i = 1, particle_set%n_tot
       if (particle_set%prt(i)%is_hadronic_beam_remnant ()) then
          first_remnant = i
          exit
       end if
    end do
    n_particles = count (.not. particle_set%prt%is_hadronic_beam_remnant ())
    allocate (no_hadronic_remnants (particle_set%n_tot))
    no_hadronic_remnants = .not. particle_set%prt%is_hadronic_beam_remnant ()
    allocate (particles (n_particles + n_extra))
    k = 1
    do i = 1, particle_set%n_tot
       if (no_hadronic_remnants(i)) then
          particles(k) = particle_set%prt(i)
          k = k + 1
       end if
    end do
    if (n_particles /= particle_set%n_tot) then
       do i = 1, n_particles
          select case (particles(i)%get_status ())
          case (PRT_BEAM)
             if (allocated (children))  deallocate (children)
             allocate (children (particles(i)%get_n_children ()))
             children = particles(i)%get_children ()
             if (allocated (no_hadronic_children)) &
                  deallocate (no_hadronic_children)
             allocate (no_hadronic_children (particles(i)%get_n_children ()))
             no_hadronic_children = .not. &
                  particle_set%prt(children)%is_hadronic_beam_remnant ()
             if (allocated (new_children))  deallocate (new_children)
             allocate (new_children (count (no_hadronic_children)))
             new_children = pack (children, no_hadronic_children)
             call particles(i)%set_children (new_children)
          case (PRT_INCOMING, PRT_RESONANT)
             if (allocated (children))  deallocate (children)
             allocate (children (particles(i)%get_n_children ()))
             children = particles(i)%get_children ()
             do j = 1, size (children)
                if (children(j) > first_remnant) then
                   children(j) = children (j) - &
                        (particle_set%n_tot - n_particles)
                end if
             end do
             call particles(i)%set_children (children)
          case (PRT_OUTGOING, PRT_BEAM_REMNANT)
          case default
          end select
       end do
    end if
  end subroutine particle_set_without_hadronic_remnants

  subroutine particle_set_without_remnants &
         (particle_set, particles, n_particles, n_extra)
    class(particle_set_t), intent(inout) :: particle_set
    type(particle_t), dimension(:), allocatable, intent(out) :: particles
    integer, intent(in) :: n_extra
    integer, intent(out) :: n_particles
    logical, dimension(:), allocatable :: no_remnants, no_children
    integer, dimension(:), allocatable :: children, new_children
    integer :: i,j, k, first_remnant
    first_remnant = particle_set%n_tot
    do i = 1, particle_set%n_tot
       if (particle_set%prt(i)%is_beam_remnant ()) then
          first_remnant = i
          exit
       end if
    end do
    allocate (no_remnants (particle_set%n_tot))
    no_remnants = .not. (particle_set%prt%is_beam_remnant ())
    n_particles = count (no_remnants)
    allocate (particles (n_particles + n_extra))
    k = 1
    do i = 1, particle_set%n_tot
       if (no_remnants(i)) then
          particles(k) = particle_set%prt(i)
          k = k + 1
       end if
    end do
    if (n_particles /= particle_set%n_tot) then
       do i = 1, n_particles
          select case (particles(i)%get_status ())
          case (PRT_BEAM)
             if (allocated (children))  deallocate (children)
             allocate (children (particles(i)%get_n_children ()))
             children = particles(i)%get_children ()
             if (allocated (no_children))  deallocate (no_children)
             allocate (no_children (particles(i)%get_n_children ()))
             no_children = .not. (particle_set%prt(children)%is_beam_remnant ())
             if (allocated (new_children))  deallocate (new_children)
             allocate (new_children (count (no_children)))
             new_children = pack (children, no_children)
             call particles(i)%set_children (new_children)
          case (PRT_INCOMING, PRT_RESONANT)
             if (allocated (children))  deallocate (children)
             allocate (children (particles(i)%get_n_children ()))
             children = particles(i)%get_children ()
             do j = 1, size (children)
                if (children(j) > first_remnant) then
                   children(j) = children (j) - &
                        (particle_set%n_tot - n_particles)
                end if
             end do
             call particles(i)%set_children (children)
          case (PRT_OUTGOING, PRT_BEAM_REMNANT)
          case default
          end select
       end do
    end if
  end subroutine particle_set_without_remnants

  pure function particle_set_find_particle &
         (particle_set, pdg, momentum, abs_smallness, rel_smallness) result (idx)
    integer :: idx
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: momentum
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    integer :: i, j
    logical, dimension(0:3) :: equals
    idx = 0
    do i = 1, size (particle_set%prt)
       if (particle_set%prt(i)%flv%get_pdg () == pdg) then
          !!! Workaround for gfortran 4.8.3 with overloaded elemental function
          do j = 0, 3
            equals(j) = nearly_equal (particle_set%prt(i)%p%p(j), momentum%p(j), &
                                   abs_smallness, rel_smallness)
          end do
          if (all (equals)) then
             idx = i
             return
          end if
       end if
    end do
  end function particle_set_find_particle

  pure function particle_set_reverse_find_particle &
         (particle_set, pdg, momentum, abs_smallness, rel_smallness) result (idx)
    integer :: idx
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: pdg
    type(vector4_t), intent(in) :: momentum
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    integer :: i
    idx = 0
    do i = size (particle_set%prt), 1, -1
       if (particle_set%prt(i)%flv%get_pdg () == pdg) then
          if (all (nearly_equal (particle_set%prt(i)%p%p, momentum%p, &
               abs_smallness, rel_smallness))) then
             idx = i
             return
          end if
       end if
    end do
  end function particle_set_reverse_find_particle

  subroutine particle_set_remove_duplicates (particle_set, smallness)
    class(particle_set_t), intent(inout) :: particle_set
    real(default), intent(in) :: smallness
    integer :: n_removals
    integer, dimension(particle_set%n_tot) :: to_remove
    type(particle_t), dimension(:), allocatable :: particles
    type(vector4_t) :: p_i
    integer, dimension(:), allocatable :: map
    to_remove = 0
    call find_duplicates ()
    n_removals = count (to_remove > 0)
    if (n_removals > 0) then
       call strip_duplicates (particles)
       call particle_set%replace (particles)
    end if

  contains

    subroutine find_duplicates ()
      integer :: pdg_i, child_i, i, j
      OUTER: do i = 1, particle_set%n_tot
         if (particle_set%prt(i)%status == PRT_OUTGOING .or. &
              particle_set%prt(i)%status == PRT_VIRTUAL .or. &
              particle_set%prt(i)%status == PRT_RESONANT) then
            if (allocated (particle_set%prt(i)%child)) then
               if (size (particle_set%prt(i)%child) > 1) cycle OUTER
               if (size (particle_set%prt(i)%child) == 1) then
                  child_i = particle_set%prt(i)%child(1)
               else
                  child_i = 0
               end if
            else
               child_i = 0
            end if
            pdg_i = particle_set%prt(i)%flv%get_pdg ()
            p_i = particle_set%prt(i)%p
            do j = i + 1, particle_set%n_tot
               if (pdg_i == particle_set%prt(j)%flv%get_pdg ()) then
                  if (all (nearly_equal (particle_set%prt(j)%p%p, p_i%p, &
                       abs_smallness = smallness, &
                       rel_smallness = 1E4_default * smallness))) then
                     if (child_i == 0 .or. j == child_i) then
                        to_remove(j) = i
                        call msg_debug2 (D_PARTICLES, &
                             "Particles: Will remove duplicate of i", i)
                        call msg_debug2 (D_PARTICLES, &
                             "Particles: j", j)
                     end if
                     cycle OUTER
                  end if
               end if
            end do
         end if
      end do OUTER
    end subroutine find_duplicates

    recursive function get_alive_index (try) result (alive)
      integer :: alive
      integer :: try
      if (map(try) > 0) then
         alive = map(try)
      else
         alive = get_alive_index (to_remove(try))
      end if
    end function get_alive_index

    subroutine strip_duplicates (particles)
      type(particle_t), dimension(:), allocatable, intent(out) :: particles
      integer :: kept, removed, i, j
      integer, dimension(:), allocatable :: old_children
      logical, dimension(:), allocatable :: parent_set
      call msg_debug (D_PARTICLES, "Particles: Removing duplicates")
      call msg_debug (D_PARTICLES, "Particles: n_removals", n_removals)
      if (debug2_active (D_PARTICLES)) then
         call msg_debug2 (D_PARTICLES, "Particles: Given set before removing:")
         call particle_set%write (summary=.true., compressed=.true.)
      end if
      allocate (particles (particle_set%n_tot - n_removals))
      allocate (map (particle_set%n_tot))
      allocate (parent_set (particle_set%n_tot))
      parent_set = .false.
      map = 0
      j = 0
      do i = 1, particle_set%n_tot
         if (to_remove(i) == 0) then
            j = j + 1
            map(i) = j
            call particles(j)%init (particle_set%prt(i))
         end if
      end do
      do i = 1, particle_set%n_tot
         if (map(i) /= 0) then
            if (.not. parent_set(map(i))) then
               call particles(map(i))%set_parents &
                    (map (particle_set%prt(i)%get_parents ()))
            end if
            call particles(map(i))%set_children &
                 (map (particle_set%prt(i)%get_children ()))
         else
            removed = i
            kept = to_remove(i)
            if (particle_set%prt(removed)%has_children ()) then
               old_children = particle_set%prt(removed)%get_children ()
               do j = 1, size (old_children)
                  if (map(old_children(j)) > 0) then
                     call particles(map(old_children(j)))%set_parents &
                          ([get_alive_index (kept)])
                     parent_set(map(old_children(j))) = .true.
                     call particles(get_alive_index (kept))%add_child &
                          (map(old_children(j)))
                  end if
               end do
               particles(get_alive_index (kept))%status = PRT_RESONANT
            else
               particles(get_alive_index (kept))%status = PRT_OUTGOING
            end if
         end if
      end do
    end subroutine strip_duplicates


  end subroutine particle_set_remove_duplicates

  subroutine particle_set_reset_status (particle_set, index, status)
    class(particle_set_t), intent(inout) :: particle_set
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: status
    integer :: i
    if (allocated (particle_set%prt)) then
       do i = 1, size (index)
          call particle_set%prt(index(i))%reset_status (status)
       end do
    end if
    particle_set%n_beam  = &
         count (particle_set%prt%get_status () == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_set%prt%get_status () == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_set%prt%get_status () == PRT_OUTGOING)
    particle_set%n_vir = particle_set%n_tot &
         - particle_set%n_beam - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_reset_status

  subroutine particle_set_reduce (pset_in, pset_out, keep_beams)
    class(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams
    integer, dimension(:), allocatable :: status, map
    integer :: i, j
    logical :: kb
    kb = .false.;  if (present (keep_beams))  kb = keep_beams
    allocate (status (pset_in%n_tot))
    pset_out%factorization_mode = pset_in%factorization_mode
    status = pset_in%prt%get_status ()
    if (kb)  pset_out%n_beam  = count (status == PRT_BEAM)
    pset_out%n_in  = count (status == PRT_INCOMING)
    pset_out%n_vir = count (status == PRT_RESONANT)
    pset_out%n_out = count (status == PRT_OUTGOING)
    pset_out%n_tot = &
         pset_out%n_beam + pset_out%n_in + pset_out%n_vir + pset_out%n_out
    allocate (pset_out%prt (pset_out%n_tot))
    allocate (map (pset_in%n_tot))
    map = 0
    j = 0
    if (kb) call copy_particles (PRT_BEAM)
    call copy_particles (PRT_INCOMING)
    call copy_particles (PRT_RESONANT)
    call copy_particles (PRT_OUTGOING)
    do i = 1, pset_in%n_tot
       if (map(i) == 0)  cycle
       !!! !!! triggers nagfor bug!
       !!!  call particle_set_parents (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_parents (pset_in, i)))
       !!!  call particle_set_children (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_children (pset_in, i)))
       !!! !!! workaround:
       call pset_out%prt(map(i))%set_parents &
            (pset_in%get_real_parents (i, kb))
       call pset_out%prt(map(i))%set_parents &
            (map (pset_out%prt(map(i))%parent))
       call pset_out%prt(map(i))%set_children &
            (pset_in%get_real_children (i, kb))
       call pset_out%prt(map(i))%set_children &
            (map (pset_out%prt(map(i))%child))
    end do
  contains
    subroutine copy_particles (stat)
      integer, intent(in) :: stat
      integer :: i
      do i = 1, pset_in%n_tot
         if (status(i) == stat) then
            j = j + 1
            map(i) = j
            call particle_init_particle (pset_out%prt(j), pset_in%prt(i))
         end if
      end do
    end subroutine copy_particles
  end subroutine particle_set_reduce

  subroutine particle_set_filter_particles &
       (pset_in, pset_out, keep_beams, real_parents, keep_virtuals)
    class(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams, real_parents, keep_virtuals
    integer, dimension(:), allocatable :: status, map
    logical, dimension(:), allocatable :: filter
    integer :: i, j
    logical :: kb, rp, kv
    kb = .false.;  if (present (keep_beams))  kb = keep_beams
    rp = .false.; if (present (real_parents)) rp = real_parents
    kv = .true.; if (present (keep_virtuals)) kv = keep_virtuals
    call msg_debug (D_PARTICLES, "filter_particles")
    if (debug2_active (D_PARTICLES)) then
       print *, 'keep_beams =    ', kb
       print *, 'real_parents =    ', rp
       print *, 'keep_virtuals =    ', kv
       print *, '>>> pset_in : '
       call pset_in%write(compressed=.true.)
    end if
    call count_and_allocate()
    map = 0
    j = 0
    filter = .false.
    if (.not. kb) filter = status == PRT_BEAM .or. status == PRT_BEAM_REMNANT
    if (.not. kv) filter = filter .or. status == PRT_VIRTUAL
    call copy_particles ()
    do i = 1, pset_in%n_tot
       if (map(i) == 0)  cycle
       if (rp) then
          call pset_out%prt(map(i))%set_parents &
               (map (pset_in%get_real_parents (i, kb)))
          call pset_out%prt(map(i))%set_children &
               (map (pset_in%get_real_children (i, kb)))
       else
          call pset_out%prt(map(i))%set_parents &
               (map (pset_in%prt(i)%get_parents ()))
          call pset_out%prt(map(i))%set_children &
               (map (pset_in%prt(i)%get_children ()))
       end if
    end do
    if (debug2_active (D_PARTICLES)) then
       print *, '>>> pset_out : '
       call pset_out%write(compressed=.true.)
    end if
  contains
        subroutine copy_particles ()
          integer :: i
          do i = 1, pset_in%n_tot
             if (.not. filter(i)) then
                j = j + 1
                map(i) = j
                call particle_init_particle (pset_out%prt(j), pset_in%prt(i))
             end if
          end do
        end subroutine copy_particles

      subroutine count_and_allocate
        allocate (status (pset_in%n_tot))
        status = particle_get_status (pset_in%prt)
        if (kb)  pset_out%n_beam  = count (status == PRT_BEAM)
        pset_out%n_in  = count (status == PRT_INCOMING)
        if (kb .and. kv) then
           pset_out%n_vir = count (status == PRT_VIRTUAL) + &
                count (status == PRT_RESONANT) + &
                count (status == PRT_BEAM_REMNANT)
        else if (kb .and. .not. kv) then
           pset_out%n_vir = count (status == PRT_RESONANT) + &
                count (status == PRT_BEAM_REMNANT)
        else if (.not. kb .and. kv) then
           pset_out%n_vir = count (status == PRT_VIRTUAL) + &
                count (status == PRT_RESONANT)
        else
           pset_out%n_vir = count (status == PRT_RESONANT)
        end if
        pset_out%n_out = count (status == PRT_OUTGOING)
        pset_out%n_tot = &
             pset_out%n_beam + pset_out%n_in + pset_out%n_vir + pset_out%n_out
        allocate (pset_out%prt (pset_out%n_tot))
        allocate (map (pset_in%n_tot))
        allocate (filter (pset_in%n_tot))
      end subroutine count_and_allocate

  end subroutine particle_set_filter_particles

  subroutine particle_set_to_hepevt_form (pset_in, pset_out)
    class(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    type :: particle_entry_t
       integer :: src = 0
       integer :: status = 0
       integer :: orig = 0
       integer :: copy = 0
    end type particle_entry_t
    type(particle_entry_t), dimension(:), allocatable :: prt
    integer, dimension(:), allocatable :: map1, map2
    integer, dimension(:), allocatable :: parent, child
    integer :: n_tot, n_parents, n_children, i, j, c, n

    n_tot = pset_in%n_tot
    allocate (prt (4 * n_tot))
    allocate (map1(4 * n_tot))
    allocate (map2(4 * n_tot))
    map1 = 0
    map2 = 0
    allocate (child (n_tot))
    allocate (parent (n_tot))
    n = 0
    do i = 1, n_tot
       if (pset_in%prt(i)%get_n_parents () == 0) then
          call append (i)
       end if
    end do
    do i = 1, n_tot
       n_children = pset_in%prt(i)%get_n_children ()
       if (n_children > 0) then
          child(1:n_children) = pset_in%prt(i)%get_children ()
          c = child(1)
          if (map1(c) == 0) then
             n_parents = pset_in%prt(c)%get_n_parents ()
             if (n_parents > 1) then
                parent(1:n_parents) = pset_in%prt(c)%get_parents ()
                if (i == parent(1) .and. &
                    any( [(map1(i)+j-1, j=1,n_parents)] /= &
                           map1(parent(1:n_parents)))) then
                   do j = 1, n_parents
                      call append (parent(j))
                   end do
                end if
             else if (map1(i) == 0) then
                call append (i)
             end if
             do j = 1, n_children
                call append (child(j))
             end do
          end if
       else if (map1(i) == 0) then
          call append (i)
       end if
    end do
    do i = n, 1, -1
       if (prt(i)%status /= PRT_OUTGOING) then
          do j = 1, i-1
             if (prt(j)%status == PRT_OUTGOING) then
                call append(prt(j)%src)
             end if
          end do
          exit
       end if
    end do
    pset_out%n_beam = count (prt(1:n)%status == PRT_BEAM)
    pset_out%n_in   = count (prt(1:n)%status == PRT_INCOMING)
    pset_out%n_vir  = count (prt(1:n)%status == PRT_RESONANT)
    pset_out%n_out  = count (prt(1:n)%status == PRT_OUTGOING)
    pset_out%n_tot = n
    allocate (pset_out%prt (n))
    do i = 1, n
       call particle_init_particle (pset_out%prt(i), pset_in%prt(prt(i)%src))
       call pset_out%prt(i)%reset_status (prt(i)%status)
       if (prt(i)%orig == 0) then
          call pset_out%prt(i)%set_parents &
               (map2 (pset_in%prt(prt(i)%src)%get_parents ()))
       else
          call pset_out%prt(i)%set_parents ([ prt(i)%orig ])
       end if
       if (prt(i)%copy == 0) then
          call pset_out%prt(i)%set_children &
               (map1 (pset_in%prt(prt(i)%src)%get_children ()))
       else
          call pset_out%prt(i)%set_children ([ prt(i)%copy ])
       end if
    end do
  contains
    subroutine append (i)
      integer, intent(in) :: i
      n = n + 1
      if (n > size (prt)) &
           call msg_bug ("Particle set transform to HEPEVT: insufficient space")
      prt(n)%src = i
      prt(n)%status = pset_in%prt(i)%get_status ()
      if (map1(i) == 0) then
         map1(i) = n
      else
         prt(map2(i))%status = PRT_VIRTUAL
         prt(map2(i))%copy = n
         prt(n)%orig = map2(i)
      end if
      map2(i) = n
    end subroutine append
  end subroutine particle_set_to_hepevt_form

  subroutine particle_set_fill_interaction &
       (pset, int, n_in, recover_beams, check_match, state_flv)
    class(particle_set_t), intent(in) :: pset
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: n_in
    logical, intent(in), optional :: recover_beams, check_match
    type(state_flv_content_t), intent(in), optional :: state_flv
    integer, dimension(:), allocatable :: map, pdg
    integer, dimension(:), allocatable :: i_in, i_out, p_in, p_out
    logical, dimension(:), allocatable :: i_set
    integer :: n_out, i, p
    logical :: r_beams, check
    r_beams = .false.;  if (present (recover_beams))  r_beams = recover_beams
    check = .true.;  if (present (check_match))  check = check_match
    if (check) then
       call find_hard_process_in_int  (i_in, i_out)
       call find_hard_process_in_pset (p_in, p_out)
       n_out = size (i_out)
       if (size (i_in) /= n_in)  call err_int_n_in
       if (size (p_in) /= n_in)  call err_pset_n_in
       if (size (p_out) /= n_out)  call err_pset_n_out
       call extract_hard_process_from_pset (pdg)
       call determine_map_for_hard_process (map, state_flv)
       if (.not. r_beams) then
          select case (n_in)
          case (1)
             call recover_parents (p_in(1), map)
          case (2)
             do i = 1, 2
                call recover_parents (p_in(i), map)
             end do
             do p = 1, 2
                call recover_radiation (p, map)
             end do
          end select
       end if
    else
       allocate (map (int%get_n_tot ()))
       map = [(i, i = 1, size (map))]
       r_beams = .false.
    end if
    allocate (i_set (int%get_n_tot ()), source = .false.)
    do p = 1, size (map)
       if (map(p) /= 0) then
          i_set(map(p)) = .true.
          call int%set_momentum &
               (pset%prt(p)%get_momentum (), map(p))
       end if
    end do
    if (r_beams) then
       do i = 1, n_in
          call reconstruct_beam_and_radiation (i, i_set)
       end do
    end if
    if (any (.not. i_set))  call err_map
  contains
    subroutine find_hard_process_in_pset (p_in, p_out)
      integer, dimension(:), allocatable, intent(out) :: p_in, p_out
      integer, dimension(:), allocatable :: p_status, p_idx
      integer :: n_out_p
      integer :: i
      allocate (p_status (pset%n_tot), p_idx (pset%n_tot))
      p_status = pset%prt%get_status ()
      p_idx = [(i, i = 1, pset%n_tot)]
      allocate (p_in (n_in))
      p_in = pack (p_idx, p_status == PRT_INCOMING)
      if (size (p_in) == 0)  call err_pset_hard
      i = p_in(1)
      n_out_p = particle_get_n_children (pset%prt(i))
      allocate (p_out (n_out_p))
      p_out = particle_get_children (pset%prt(i))
    end subroutine find_hard_process_in_pset
    subroutine find_hard_process_in_int (i_in, i_out)
      integer, dimension(:), allocatable, intent(out) :: i_in, i_out
      integer :: n_in_i
      integer :: i
      i = int%get_n_tot ()
      n_in_i = interaction_get_n_parents (int, i)
      if (n_in_i /= n_in)  call err_int_n_in
      allocate (i_in (n_in))
      i_in = interaction_get_parents (int, i)
      i = i_in(1)
      n_out = interaction_get_n_children (int, i)
      allocate (i_out (n_out))
      i_out = interaction_get_children (int, i)
    end subroutine find_hard_process_in_int
    subroutine extract_hard_process_from_pset (pdg)
      integer, dimension(:), allocatable, intent(out) :: pdg
      integer, dimension(:), allocatable :: pdg_p
      logical, dimension(:), allocatable :: mask_p
      integer :: i
      allocate (pdg_p (pset%n_tot))
      pdg_p = pset%prt%get_pdg ()
      allocate (mask_p (pset%n_tot), source = .false.)
      mask_p (p_in) = .true.
      mask_p (p_out) = .true.
      allocate (pdg (n_in + n_out))
      pdg = pack (pdg_p, mask_p)
    end subroutine extract_hard_process_from_pset
    subroutine determine_map_for_hard_process (map, state_flv)
      integer, dimension(:), allocatable, intent(out) :: map
      type(state_flv_content_t), intent(in), optional :: state_flv
      integer, dimension(:), allocatable :: pdg_i, map_i
      integer :: n_tot
      logical, dimension(:), allocatable :: mask_i, mask_p
      logical :: success
      n_tot = int%get_n_tot ()
      if (present (state_flv)) then
         allocate (mask_i (n_tot), source = .false.)
         mask_i (i_in) = .true.
         mask_i (i_out) = .true.
         allocate (pdg_i (n_tot), map_i (n_tot))
         pdg_i = unpack (pdg, mask_i, 0)
         call state_flv%match (pdg_i, success, map_i)
         allocate (mask_p (pset%n_tot), source = .false.)
         mask_p (p_in) = .true.
         mask_p (p_out) = .true.
         allocate (map (size (mask_p)), &
              source = unpack (pack (map_i, mask_i), mask_p, 0))
         if (.not. success)  call err_mismatch
      else
         allocate (map (n_tot), source = 0)
         map(p_in) = i_in
         map(p_out) = i_out
      end if
    end subroutine determine_map_for_hard_process
    recursive subroutine recover_parents (p, map)
      integer, intent(in) :: p
      integer, dimension(:), intent(inout) :: map
      integer :: i, n, n_p, q, k
      integer, dimension(:), allocatable :: i_parents, p_parents
      integer, dimension(1) :: pp
      i = map(p)
      n = interaction_get_n_parents (int, i)
      q = p
      n_p = particle_get_n_parents (pset%prt(q))
      do while (n_p == 1)
         pp = particle_get_parents (pset%prt(q))
         if (pset%prt(pp(1))%get_n_children () > 1)  exit
         q = pp(1)
         n_p = pset%prt(q)%get_n_parents ()
      end do
      if (n_p /= n)  call err_map
      allocate (i_parents (n), p_parents (n))
      i_parents = interaction_get_parents (int, i)
      p_parents = pset%prt(q)%get_parents ()
      do k = 1, n
         q = p_parents(k)
         if (map(q) == 0) then
            map(q) = i_parents(k)
            call recover_parents (q, map)
         end if
      end do
    end subroutine recover_parents
    recursive subroutine recover_radiation (p, map)
      integer, intent(in) :: p
      integer, dimension(:), intent(inout) :: map
      integer :: i, n, n_p, q, k
      integer, dimension(:), allocatable :: i_children, p_children
      if (particle_get_status (pset%prt(p)) == PRT_INCOMING)  return
      i = map(p)
      n = interaction_get_n_children (int, i)
      n_p = pset%prt(p)%get_n_children ()
      if (n_p /= n)  call err_map
      allocate (i_children (n), p_children (n))
      i_children = interaction_get_children (int, i)
      p_children = pset%prt(p)%get_children ()
      do k = 1, n
         q = p_children(k)
         if (map(q) == 0) then
            i = i_children(k)
            if (interaction_get_n_children (int, i) == 0) then
               map(q) = i
            else
               select case (n)
               case (2)
                  select case (k)
                  case (1);  map(q) = i_children(2)
                  case (2);  map(q) = i_children(1)
                  end select
               case (4)
                  select case (k)
                  case (1);  map(q) = i_children(3)
                  case (2);  map(q) = i_children(4)
                  case (3);  map(q) = i_children(1)
                  case (4);  map(q) = i_children(2)
                  end select
               case default
                  call err_radiation
               end select
            end if
         else
            call recover_radiation (q, map)
         end if
      end do
    end subroutine recover_radiation
    subroutine reconstruct_beam_and_radiation (k, i_set)
      integer, intent(in) :: k
      logical, dimension(:), intent(inout) :: i_set
      integer :: k_src, k_in, k_rad
      type(interaction_t), pointer :: int_src
      integer, dimension(2) :: i_child
      call int%find_source (k, int_src, k_src)
      call int%set_momentum (int_src%get_momentum (k_src), k)
      i_set(k) = .true.
      if (n_in == 2) then
         i_child = interaction_get_children (int, k)
         if (interaction_get_n_children (int, i_child(1)) > 0) then
            k_in = i_child(1);  k_rad = i_child(2)
         else
            k_in = i_child(2);  k_rad = i_child(1)
         end if
         if (.not. i_set(k_in))  call err_beams
         call int%set_momentum &
              (int%get_momentum (k) - int%get_momentum (k_in), k_rad)
         i_set(k_rad) = .true.
      end if
    end subroutine reconstruct_beam_and_radiation
    subroutine err_pset_hard
      call msg_fatal ("Reading particle set: no particles marked as incoming")
    end subroutine err_pset_hard
    subroutine err_int_n_in
      integer :: n
      if (allocated (i_in)) then
         n = size (i_in)
      else
         n = 0
      end if
      write (msg_buffer, "(A,I0,A,I0)") &
           "Filling hard process from particle set: expect ", n_in, &
           " incoming particle(s), found ", n
      call msg_bug
    end subroutine err_int_n_in
    subroutine err_pset_n_in
      write (msg_buffer, "(A,I0,A,I0)") &
           "Reading hard-process particle set: should contain ", n_in, &
           " incoming particle(s), found ", size (p_in)
      call msg_fatal
    end subroutine err_pset_n_in
    subroutine err_pset_n_out
      write (msg_buffer, "(A,I0,A,I0)") &
           "Reading hard-process particle set: should contain ", n_out, &
           " outgoing particle(s), found ", size (p_out)
      call msg_fatal
    end subroutine err_pset_n_out
    subroutine err_mismatch
      call pset%write ()
      call state_flv%write ()
      call msg_fatal ("Reading particle set: Flavor combination " &
           // "does not match requested process")
    end subroutine err_mismatch
    subroutine err_map
      call pset%write ()
      call int%basic_write ()
      call msg_fatal ("Reading hard-process particle set: " &
           // "Incomplete mapping from particle set to interaction")
    end subroutine err_map
    subroutine err_beams
      call pset%write ()
      call int%basic_write ()
      call msg_fatal ("Reading particle set: Beam structure " &
           // "does not match requested process")
    end subroutine err_beams
    subroutine err_radiation
      call int%basic_write ()
      call msg_bug ("Reading particle set: Interaction " &
           // "contains inconsistent radiation pattern.")
    end subroutine err_radiation
  end subroutine particle_set_fill_interaction

  subroutine particle_set_assign_vertices &
       (particle_set, v_from, v_to, n_vertices)
    class(particle_set_t), intent(in) :: particle_set
    integer, dimension(:), intent(out) :: v_from, v_to
    integer, intent(out) :: n_vertices
    integer, dimension(:), allocatable :: parent, child
    integer :: n_parents, n_children, vf, vt
    integer :: i, j, v
    v_from = 0
    v_to = 0
    vf = 0
    vt = 0
    do i = 1, particle_set%n_tot
       n_parents = particle_set%prt(i)%get_n_parents ()
       if (n_parents /= 0) then
          allocate (parent (n_parents))
          parent = particle_set%prt(i)%get_parents ()
          SCAN_PARENTS: do j = 1, size (parent)
             v = v_to(parent(j))
             if (v /= 0) then
                v_from(i) = v;  exit SCAN_PARENTS
             end if
          end do SCAN_PARENTS
          if (v_from(i) == 0) then
             vf = vf + 1;  v_from(i) = vf
             v_to(parent) = vf
          end if
          deallocate (parent)
       end if
       n_children = particle_set%prt(i)%get_n_children ()
       if (n_children /= 0) then
          allocate (child (n_children))
          child = particle_set%prt(i)%get_children ()
          SCAN_CHILDREN: do j = 1, size (child)
             v = v_from(child(j))
             if (v /= 0) then
                v_to(i) = v;  exit SCAN_CHILDREN
             end if
          end do SCAN_CHILDREN
          if (v_to(i) == 0) then
             vt = vt + 1;  v_to(i) = vt
             v_from(child) = vt
          end if
          deallocate (child)
       end if
    end do
    n_vertices = max (vf, vt)
  end subroutine particle_set_assign_vertices

  subroutine particle_set_to_subevt (particle_set, subevt)
    class(particle_set_t), intent(in) :: particle_set
    type(subevt_t), intent(out) :: subevt
    integer :: n_tot, n_beam, n_in, n_out, n_rad
    integer :: i, k, n_active
    integer, dimension(2) :: hel
    logical :: keep
    n_tot  = particle_set_get_n_tot      (particle_set)
    n_beam = particle_set_get_n_beam     (particle_set)
    n_in   = particle_set_get_n_in       (particle_set)
    n_out  = particle_set_get_n_out      (particle_set)
    n_rad  = particle_set_get_n_remnants (particle_set)
    call subevt_init (subevt, n_beam + n_rad + n_in + n_out)
    k = 0
    do i = 1, n_tot
       associate (prt => particle_set%prt(i))
         keep = .false.
         select case (particle_get_status (prt))
         case (PRT_BEAM)
            k = k + 1
            call subevt_set_beam (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_INCOMING)
            k = k + 1
            call subevt_set_incoming (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_OUTGOING)
            k = k + 1
            call subevt_set_outgoing (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_BEAM_REMNANT)
            if (particle_get_n_children (prt) == 0) then
               k = k + 1
               call subevt_set_outgoing (subevt, k, &
                    particle_get_pdg (prt), &
                    particle_get_momentum (prt), &
                    particle_get_p2 (prt))
               keep = .true.
            end if
         end select
         if (keep) then
            if (prt%polarization == PRT_DEFINITE_HELICITY) then
               if (prt%hel%is_diagonal ()) then
                  hel = prt%hel%to_pair ()
                  call subevt_polarize (subevt, k, hel(1))
               end if
            end if
         end if
       end associate
       n_active = k
    end do
    call subevt_reset (subevt, n_active)
  end subroutine particle_set_to_subevt

  subroutine particle_set_replace (particle_set, newprt)
    class(particle_set_t), intent(inout) :: particle_set
    type(particle_t), intent(in), dimension(:), allocatable :: newprt
    if (allocated (particle_set%prt))  deallocate (particle_set%prt)
    allocate (particle_set%prt(size (newprt)))
    particle_set%prt = newprt
    particle_set%n_tot = size (newprt)
    particle_set%n_beam = count (particle_get_status (newprt) == PRT_BEAM)
    particle_set%n_in = count (particle_get_status (newprt) == PRT_INCOMING)
    particle_set%n_out = count (particle_get_status (newprt) == PRT_OUTGOING)
    particle_set%n_vir = particle_set%n_tot &
         - particle_set%n_beam - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_replace

  subroutine particle_set_order_color_lines (pset_out, pset_in)
    class(particle_set_t), intent(inout) :: pset_out
    type(particle_set_t), intent(in) :: pset_in
    integer :: i, n, n_col_rem
    n_col_rem = 0
    do i = 1, pset_in%n_tot
       if (pset_in%prt(i)%get_status () == PRT_BEAM_REMNANT .and. &
            any (pset_in%prt(i)%get_color () /= 0)) then
          n_col_rem = n_col_rem + 1
       end if
    end do
    pset_out%n_beam = pset_in%n_beam
    pset_out%n_in   = pset_in%n_in
    pset_out%n_vir  = pset_in%n_vir + pset_in%n_out + n_col_rem
    pset_out%n_out  = pset_in%n_out
    pset_out%n_tot  = pset_in%n_tot + pset_in%n_out + n_col_rem
    pset_out%correlated_state = pset_in%correlated_state
    pset_out%factorization_mode = pset_in%factorization_mode
    allocate (pset_out%prt (pset_out%n_tot))
    do i = 1, pset_in%n_tot
       call pset_out%prt(i)%init (pset_in%prt(i))
       call pset_out%prt(i)%set_children (pset_in%prt(i)%child)
       call pset_out%prt(i)%set_parents (pset_in%prt(i)%parent)
    end do
    n = pset_in%n_tot
    do i = 1, pset_in%n_tot
       if (pset_out%prt(i)%get_status () == PRT_OUTGOING .and. &
           all (pset_out%prt(i)%get_color () == 0) .and. &
           .not. pset_out%prt(i)%has_children ()) then
          n = n + 1
          call pset_out%prt(n)%init (pset_out%prt(i))
          call pset_out%prt(i)%reset_status (PRT_VIRTUAL)
          call pset_out%prt(i)%add_child (n)
          call pset_out%prt(i)%set_parents ([i])
       end if
    end do
    if (n_col_rem > 0) then
       do i = 1, n_col_rem
       end do
    end if
  end subroutine particle_set_order_color_lines

  subroutine pacify_particle (prt)
    class(particle_t), intent(inout) :: prt
    real(default) :: e
    e = epsilon (1._default) * energy (prt%p)
    call pacify (prt%p, 10 * e)
    call pacify (prt%p2, 1e4 * e)
  end subroutine pacify_particle

  subroutine pacify_particle_set (pset)
    class(particle_set_t), intent(inout) :: pset
    integer :: i
    do i = 1, pset%n_tot
       call pacify (pset%prt(i))
    end do
  end subroutine pacify_particle_set


end module particles
