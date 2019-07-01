! WHIZARD 2.5.0 May 06 2017
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

module phs_base

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: TWOPI, TWOPI4
  use string_utils, only: split_string
  use format_defs, only: FMT_19
  use numeric_utils
  use diagnostics
  use md5
  use physics_defs
  use lorentz
  use model_data
  use flavors
  use process_constants

  implicit none
  private

  public :: channel_prop_t
  public :: resonance_t
  public :: on_shell_t
  public :: phs_channel_t
  public :: phs_channel_collection_t
  public :: phs_config_t
  public :: phs_t
  public :: compute_kinematics_solid_angle
  public :: inverse_kinematics_solid_angle
  public :: pacify

  type, abstract :: channel_prop_t
   contains
     procedure (channel_prop_to_string), deferred :: to_string
     generic :: operator (==) => is_equal
     procedure (channel_eq), deferred :: is_equal
  end type channel_prop_t

  type, extends (channel_prop_t) :: resonance_t
     real(default) :: mass = 0
     real(default) :: width = 0
   contains
     procedure :: to_string => resonance_to_string
     procedure :: is_equal => resonance_is_equal
  end type resonance_t

  type, extends (channel_prop_t) :: on_shell_t
     real(default) :: mass = 0
   contains
     procedure :: to_string => on_shell_to_string
     procedure :: is_equal => on_shell_is_equal
  end type on_shell_t

  type :: phs_equivalence_t
     integer :: c = 0
     integer, dimension(:), allocatable :: perm
     integer, dimension(:), allocatable :: mode
   contains
     procedure :: write => phs_equivalence_write
     procedure :: init => phs_equivalence_init
  end type phs_equivalence_t

  integer, parameter, public :: &
       EQ_IDENTITY = 0, EQ_INVERT = 1, EQ_SYMMETRIC = 2, EQ_INVARIANT = 3

  character, dimension(0:3), parameter :: TAG = ["+", "-", ":", "x"]

  type :: phs_channel_t
     class(channel_prop_t), allocatable :: prop
     integer :: sf_channel = 1
     type(phs_equivalence_t), dimension(:), allocatable :: eq
   contains
     procedure :: write => phs_channel_write
     procedure :: set_resonant => channel_set_resonant
     procedure :: set_on_shell => channel_set_on_shell
  end type phs_channel_t

  type :: prop_entry_t
     integer :: i = 0
     class(channel_prop_t), allocatable :: prop
     type(prop_entry_t), pointer :: next => null ()
  end type prop_entry_t

  type :: phs_channel_collection_t
     integer :: n = 0
     type(prop_entry_t), pointer :: first => null ()
   contains
     procedure :: final => phs_channel_collection_final
     procedure :: write => phs_channel_collection_write
     procedure :: push => phs_channel_collection_push
     procedure :: get_n => phs_channel_collection_get_n
     procedure :: get_entry => phs_channel_collection_get_entry
  end type phs_channel_collection_t

  type, abstract :: phs_config_t
     ! private
     type(string_t) :: id
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     integer :: n_state = 0
     integer :: n_par = 0
     integer :: n_channel = 0
     real(default) :: sqrts = 0
     logical :: sqrts_fixed = .true.
     logical :: cm_frame = .true.
     logical :: azimuthal_dependence = .false.
     integer, dimension(:), allocatable :: dim_flat
     logical :: provides_equivalences = .false.
     logical :: provides_chains = .false.
     logical :: vis_channels = .false.
     integer, dimension(:), allocatable :: chain
     class(model_data_t), pointer :: model => null ()
     type(flavor_t), dimension(:,:), allocatable :: flv
     type(phs_channel_t), dimension(:), allocatable :: channel
     character(32) :: md5sum_process = ""
     character(32) :: md5sum_model_par = ""
     character(32) :: md5sum_phs_config = ""
     integer :: nlo_type
   contains
     procedure (phs_config_final), deferred :: final
     procedure (phs_config_write), deferred :: write
     procedure :: base_write => phs_config_write
     procedure :: init => phs_config_init
     procedure :: set_component_index => phs_config_set_component_index
     procedure (phs_config_configure), deferred :: configure
     procedure :: set_sf_channel => phs_config_set_sf_channel
     procedure :: collect_channels => phs_config_collect_channels
     procedure :: compute_md5sum => phs_config_compute_md5sum
     procedure (phs_startup_message), deferred :: startup_message
     procedure :: base_startup_message => phs_startup_message
     procedure (phs_config_allocate_instance), nopass, deferred :: &
          allocate_instance
     procedure :: get_n_par => phs_config_get_n_par
     procedure :: get_flat_dimensions => phs_config_get_flat_dimensions
     procedure :: get_n_channel => phs_config_get_n_channel
     procedure :: get_sf_channel => phs_config_get_sf_channel
     procedure :: get_masses_in => phs_config_get_masses_in
     procedure :: get_md5sum => phs_config_get_md5sum
  end type phs_config_t

  type, abstract :: phs_t
     class(phs_config_t), pointer :: config => null ()
     logical :: r_defined = .false.
     integer :: selected_channel = 0
     logical, dimension(:), allocatable :: active_channel
     real(default), dimension(:,:), allocatable :: r
     real(default), dimension(:), allocatable :: f
     real(default), dimension(:), allocatable :: m_in
     real(default), dimension(:), allocatable :: m_out
     real(default) :: flux = 0
     real(default) :: volume = 0
     type(lorentz_transformation_t) :: lt_cm_to_lab
     logical :: p_defined = .false.
     real(default) :: sqrts_hat = 0
     type(vector4_t), dimension(:), allocatable :: p
     logical :: q_defined = .false.
     type(vector4_t), dimension(:), allocatable :: q
   contains
     procedure (phs_write), deferred :: write
     procedure :: base_write => phs_base_write
     procedure (phs_final), deferred :: final
     procedure (phs_init), deferred :: init
     procedure :: base_init => phs_base_init
     procedure :: select_channel => phs_base_select_channel
     procedure :: set_incoming_momenta => phs_set_incoming_momenta
     procedure :: set_outgoing_momenta => phs_set_outgoing_momenta
     procedure :: get_outgoing_momenta => phs_get_outgoing_momenta
     procedure :: is_cm_frame => phs_is_cm_frame
     procedure :: get_n_tot => phs_get_n_tot
     procedure :: set_lorentz_transformation => phs_set_lorentz_transformation
     procedure :: get_lorentz_transformation => phs_get_lorentz_transformation
     procedure :: get_mcpar => phs_get_mcpar
     procedure :: get_f => phs_get_f
     procedure :: get_overall_factor => phs_get_overall_factor
     procedure :: compute_flux => phs_compute_flux
     procedure (phs_evaluate_selected_channel), deferred :: &
          evaluate_selected_channel
     procedure (phs_evaluate_other_channels), deferred :: &
          evaluate_other_channels
     procedure (phs_inverse), deferred :: inverse
     procedure :: get_sqrts => phs_get_sqrts
  end type phs_t


  abstract interface
     function channel_prop_to_string (object) result (string)
       import
       class(channel_prop_t), intent(in) :: object
       type(string_t) :: string
     end function channel_prop_to_string
  end interface

  abstract interface
     function channel_eq (prop1, prop2) result (flag)
       import
       class(channel_prop_t), intent(in) :: prop1, prop2
       logical :: flag
     end function channel_eq
  end interface

  abstract interface
     subroutine phs_config_final (object)
       import
       class(phs_config_t), intent(inout) :: object
     end subroutine phs_config_final
  end interface

  abstract interface
     subroutine phs_config_configure (phs_config, sqrts, &
          sqrts_fixed, cm_frame, azimuthal_dependence, rebuild, ignore_mismatch, &
          nlo_type)
       import
       class(phs_config_t), intent(inout) :: phs_config
       real(default), intent(in) :: sqrts
       logical, intent(in), optional :: sqrts_fixed
       logical, intent(in), optional :: cm_frame
       logical, intent(in), optional :: azimuthal_dependence
       logical, intent(in), optional :: rebuild
       logical, intent(in), optional :: ignore_mismatch
       integer, intent(in), optional :: nlo_type
     end subroutine phs_config_configure
  end interface

  abstract interface
     subroutine phs_config_allocate_instance (phs)
       import
       class(phs_t), intent(inout), pointer :: phs
     end subroutine phs_config_allocate_instance
  end interface

  abstract interface
     subroutine phs_write (object, unit, verbose)
       import
       class(phs_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: verbose
     end subroutine phs_write
  end interface

  abstract interface
     subroutine phs_final (object)
       import
       class(phs_t), intent(inout) :: object
     end subroutine phs_final
  end interface

  abstract interface
     subroutine phs_init (phs, phs_config)
       import
       class(phs_t), intent(out) :: phs
       class(phs_config_t), intent(in), target :: phs_config
     end subroutine phs_init
  end interface

  abstract interface
     subroutine phs_evaluate_selected_channel (phs, c_in, r_in)
       import
       class(phs_t), intent(inout) :: phs
       integer, intent(in) :: c_in
       real(default), dimension(:), intent(in) :: r_in
     end subroutine phs_evaluate_selected_channel
  end interface

  abstract interface
     subroutine phs_evaluate_other_channels (phs, c_in)
       import
       class(phs_t), intent(inout) :: phs
       integer, intent(in) :: c_in
     end subroutine phs_evaluate_other_channels
  end interface

  abstract interface
     subroutine phs_inverse (phs)
       import
       class(phs_t), intent(inout) :: phs
     end subroutine phs_inverse
  end interface

  interface pacify
     module procedure pacify_phs
  end interface pacify


contains

  function resonance_to_string (object) result (string)
    class(resonance_t), intent(in) :: object
    type(string_t) :: string
    character(32) :: buffer
    string = "resonant: m ="
    write (buffer, "(" // FMT_19 // ")")  object%mass
    string = string // trim (buffer) // " GeV, w ="
    write (buffer, "(" // FMT_19 // ")")  object%width
    string = string // trim (buffer) // " GeV"
  end function resonance_to_string

  function resonance_is_equal (prop1, prop2) result (flag)
    class(resonance_t), intent(in) :: prop1
    class(channel_prop_t), intent(in) :: prop2
    logical :: flag
    select type (prop2)
    type is (resonance_t)
       flag = prop1%mass == prop2%mass .and. prop1%width == prop2%width
    class default
       flag = .false.
    end select
  end function resonance_is_equal

  function on_shell_to_string (object) result (string)
    class(on_shell_t), intent(in) :: object
    type(string_t) :: string
    character(32) :: buffer
    string = "on shell: m ="
    write (buffer, "(" // FMT_19 // ")")  object%mass
    string = string // trim (buffer) // " GeV"
  end function on_shell_to_string

  function on_shell_is_equal (prop1, prop2) result (flag)
    class(on_shell_t), intent(in) :: prop1
    class(channel_prop_t), intent(in) :: prop2
    logical :: flag
    select type (prop2)
    type is (on_shell_t)
       flag = prop1%mass == prop2%mass
    class default
       flag = .false.
    end select
  end function on_shell_is_equal

  subroutine phs_equivalence_write (object, unit)
    class(phs_equivalence_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, j
    u = given_output_unit (unit)
    write (u, "(5x,'=',1x,I0,1x)", advance = "no")  object%c
    if (allocated (object%perm)) then
       write (u, "(A)", advance = "no")  "("
       do j = 1, size (object%perm)
          if (j > 1)  write (u, "(1x)", advance = "no")
          write (u, "(I0,A1)", advance = "no") &
               object%perm(j), TAG(object%mode(j))
       end do
       write (u, "(A)")  ")"
    else
       write (u, "(A)")
    end if
  end subroutine phs_equivalence_write

  subroutine phs_equivalence_init (eq, n_dim)
    class(phs_equivalence_t), intent(out) :: eq
    integer, intent(in) :: n_dim
    allocate (eq%perm (n_dim), source = 0)
    allocate (eq%mode (n_dim), source = EQ_IDENTITY)
  end subroutine phs_equivalence_init

  subroutine phs_channel_write (object, unit)
    class(phs_channel_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, j
    u = given_output_unit (unit)
    write (u, "(1x,I0)", advance="no") object%sf_channel
    if (allocated (object%prop)) then
       write (u, "(1x,A)")  char (object%prop%to_string ())
    else
       write (u, *)
    end if
    if (allocated (object%eq)) then
       do j = 1, size (object%eq)
          call object%eq(j)%write (u)
       end do
    end if
  end subroutine phs_channel_write

  subroutine channel_set_resonant (channel, mass, width)
    class(phs_channel_t), intent(inout) :: channel
    real(default), intent(in) :: mass, width
    allocate (resonance_t :: channel%prop)
    select type (prop => channel%prop)
    type is (resonance_t)
       prop%mass = mass
       prop%width = width
    end select
  end subroutine channel_set_resonant

  subroutine channel_set_on_shell (channel, mass)
    class(phs_channel_t), intent(inout) :: channel
    real(default), intent(in) :: mass
    allocate (on_shell_t :: channel%prop)
    select type (prop => channel%prop)
    type is (on_shell_t)
       prop%mass = mass
    end select
  end subroutine channel_set_on_shell

  subroutine phs_channel_collection_final (object)
    class(phs_channel_collection_t), intent(inout) :: object
    type(prop_entry_t), pointer :: entry
    do while (associated (object%first))
       entry => object%first
       object%first => entry%next
       deallocate (entry)
    end do
  end subroutine phs_channel_collection_final

  subroutine phs_channel_collection_write (object, unit)
    class(phs_channel_collection_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(prop_entry_t), pointer :: entry
    type(string_t) :: string
    integer :: u
    u = given_output_unit (unit)
    entry => object%first
    do while (associated (entry))
       if (allocated (entry%prop)) then
          string = entry%prop%to_string ()
          write (u, "(1x,I0,1x,A)")  entry%i, char (string)
       else
          write (u, "(1x,I0)")  entry%i
       end if
       entry => entry%next
    end do
  end subroutine phs_channel_collection_write

  subroutine phs_channel_collection_push (coll, channel)
    class(phs_channel_collection_t), intent(inout) :: coll
    type(phs_channel_t), intent(inout) :: channel
    type(prop_entry_t), pointer :: entry, new
    if (associated (coll%first)) then
       entry => coll%first
       do
          if (allocated (entry%prop)) then
             if (allocated (channel%prop)) then
                if (entry%prop == channel%prop) then
                   channel%sf_channel = entry%i
                   return
                end if
             end if
          else if (.not. allocated (channel%prop)) then
             channel%sf_channel = entry%i
             return
          end if
          if (associated (entry%next)) then
             entry => entry%next
          else
             exit
          end if
       end do
       allocate (new)
       entry%next => new
    else
       allocate (new)
       coll%first => new
    end if
    coll%n = coll%n + 1
    new%i = coll%n
    channel%sf_channel = new%i
    if (allocated (channel%prop)) then
       allocate (new%prop, source = channel%prop)
    end if
  end subroutine phs_channel_collection_push

  function phs_channel_collection_get_n (coll) result (n)
    class(phs_channel_collection_t), intent(in) :: coll
    integer :: n
    n = coll%n
  end function phs_channel_collection_get_n

  subroutine phs_channel_collection_get_entry (coll, i, prop)
    class(phs_channel_collection_t), intent(in) :: coll
    integer, intent(in) :: i
    class(channel_prop_t), intent(out), allocatable :: prop
    type(prop_entry_t), pointer :: entry
    integer :: k
    if (i > 0 .and. i <= coll%n) then
       entry => coll%first
       do k = 2, i
          entry => entry%next
       end do
       if (allocated (entry%prop)) then
          if (allocated (prop))  deallocate (prop)
          allocate (prop, source = entry%prop)
       end if
    else
       call msg_bug ("PHS channel collection: get entry: illegal index")
    end if
  end subroutine phs_channel_collection_get_entry

  subroutine phs_config_write (object, unit, include_id)
    class(phs_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: include_id
    integer :: u, i, j
    integer :: n_tot_flv
    logical :: use_id
    n_tot_flv = object%n_tot
    u = given_output_unit (unit)
    use_id = .true.; if (present (include_id)) use_id = include_id
    if (use_id) write (u, "(3x,A,A,A)") "ID        = '", char (object%id), "'"
    write (u, "(3x,A,I0)")  "n_in      = ", object%n_in
    write (u, "(3x,A,I0)")  "n_out     = ", object%n_out
    write (u, "(3x,A,I0)")  "n_tot     = ", object%n_tot
    write (u, "(3x,A,I0)")  "n_state   = ", object%n_state
    write (u, "(3x,A,I0)")  "n_par     = ", object%n_par
    write (u, "(3x,A,I0)")  "n_channel = ", object%n_channel
    write (u, "(3x,A," // FMT_19 // ")")  "sqrts     = ", object%sqrts
    write (u, "(3x,A,L1)")  "s_fixed   = ", object%sqrts_fixed
    write (u, "(3x,A,L1)")  "cm_frame  = ", object%cm_frame
    write (u, "(3x,A,L1)")  "azim.dep. = ", object%azimuthal_dependence
    if (allocated (object%dim_flat)) then
       write (u, "(3x,A,I0)")  "flat dim. = ", object%dim_flat
    end if
    write (u, "(1x,A)")  "Flavor combinations:"
    do i = 1, object%n_state
       write (u, "(3x,I0,':')", advance="no")  i
!       do j = 1, object%n_tot
       do j = 1, n_tot_flv
          write (u, "(1x,A)", advance="no")  char (object%flv(j,i)%get_name ())
       end do
       write (u, "(A)")
    end do
    if (allocated (object%channel)) then
       write (u, "(1x,A)")  "Phase-space / structure-function channels:"
       do i = 1, object%n_channel
          write (u, "(3x,I0,':')", advance="no") i
          call object%channel(i)%write (u)
       end do
    end if
    if (object%md5sum_process /= "") then
       write (u, "(3x,A,A,A)") "MD5 sum (process)    = '", &
            object%md5sum_process, "'"
    end if
    if (object%md5sum_model_par /= "") then
       write (u, "(3x,A,A,A)") "MD5 sum (model par)  = '", &
            object%md5sum_model_par, "'"
    end if
    if (object%md5sum_phs_config /= "") then
       write (u, "(3x,A,A,A)") "MD5 sum (phs config) = '", &
            object%md5sum_phs_config, "'"
    end if
  end subroutine phs_config_write

  subroutine phs_config_init (phs_config, data, model)
    class(phs_config_t), intent(inout) :: phs_config
    type(process_constants_t), intent(in) :: data
    class(model_data_t), intent(in), target :: model
    integer :: i, j
    phs_config%id = data%id
    phs_config%n_in  = data%n_in
    phs_config%n_out = data%n_out
    phs_config%n_tot = data%n_in + data%n_out
    phs_config%n_state = data%n_flv
    if (data%model_name == model%get_name ()) then
       phs_config%model => model
    else
       call msg_bug ("phs_config_init: model name mismatch")
    end if
    allocate (phs_config%flv (phs_config%n_tot, phs_config%n_state))
    do i = 1, phs_config%n_state
       do j = 1, phs_config%n_tot
          call phs_config%flv(j,i)%init (data%flv_state(j,i), &
               phs_config%model)
       end do
    end do
    phs_config%md5sum_process = data%md5sum
  end subroutine phs_config_init

  subroutine phs_config_set_component_index (phs_config, index)
    class(phs_config_t), intent(inout) :: phs_config
    integer, intent(in) :: index
    type(string_t), dimension(:), allocatable :: id
    type(string_t) :: suffix
    integer :: i, n
    suffix = var_str ('i') // int2string (index)
    call split_string (phs_config%id, var_str ('_'), id)
    phs_config%id = var_str ('')
    n = size (id) - 1
    do i = 1, n
       phs_config%id = phs_config%id // id(i) // var_str ('_')
    end do
    phs_config%id = phs_config%id // suffix
  end subroutine phs_config_set_component_index

  subroutine phs_config_set_sf_channel (phs_config, sf_channel)
    class(phs_config_t), intent(inout) :: phs_config
    integer, dimension(:), intent(in) :: sf_channel
    phs_config%channel%sf_channel = sf_channel
  end subroutine phs_config_set_sf_channel

  subroutine phs_config_collect_channels (phs_config, coll)
    class(phs_config_t), intent(inout) :: phs_config
    type(phs_channel_collection_t), intent(inout) :: coll
    integer :: c
    do c = 1, phs_config%n_channel
       call coll%push (phs_config%channel(c))
    end do
  end subroutine phs_config_collect_channels

  subroutine phs_config_compute_md5sum (phs_config, include_id)
    class(phs_config_t), intent(inout) :: phs_config
    logical, intent(in), optional :: include_id
    integer :: u
    phs_config%md5sum_model_par = phs_config%model%get_parameters_md5sum ()
    phs_config%md5sum_phs_config = ""
    u = free_unit ()
    open (u, status = "scratch", action = "readwrite")
    call phs_config%write (u, include_id)
    rewind (u)
    phs_config%md5sum_phs_config = md5sum (u)
    close (u)
  end subroutine phs_config_compute_md5sum

  subroutine phs_startup_message (phs_config, unit)
    class(phs_config_t), intent(in) :: phs_config
    integer, intent(in), optional :: unit
    write (msg_buffer, "(A,3(1x,I0,1x,A))") &
         "Phase space:", &
         phs_config%n_channel, "channels,", &
         phs_config%n_par, "dimensions"
    call msg_message (unit = unit)
  end subroutine phs_startup_message

  function phs_config_get_n_par (phs_config) result (n)
    class(phs_config_t), intent(in) :: phs_config
    integer :: n
    n = phs_config%n_par
  end function phs_config_get_n_par

  function phs_config_get_flat_dimensions (phs_config) result (dim_flat)
    class(phs_config_t), intent(in) :: phs_config
    integer, dimension(:), allocatable :: dim_flat
    if (allocated (phs_config%dim_flat)) then
       allocate (dim_flat (size (phs_config%dim_flat)))
       dim_flat = phs_config%dim_flat
    else
       allocate (dim_flat (0))
    end if
  end function phs_config_get_flat_dimensions

  function phs_config_get_n_channel (phs_config) result (n)
    class(phs_config_t), intent(in) :: phs_config
    integer :: n
    n = phs_config%n_channel
  end function phs_config_get_n_channel

  function phs_config_get_sf_channel (phs_config, c) result (c_sf)
    class(phs_config_t), intent(in) :: phs_config
    integer, intent(in) :: c
    integer :: c_sf
    if (allocated (phs_config%channel)) then
       c_sf = phs_config%channel(c)%sf_channel
    else
       c_sf = 0
    end if
  end function phs_config_get_sf_channel

  subroutine phs_config_get_masses_in (phs_config, m)
    class(phs_config_t), intent(in) :: phs_config
    real(default), dimension(:), intent(out) :: m
    integer :: i
    do i = 1, phs_config%n_in
       m(i) = phs_config%flv(i,1)%get_mass ()
    end do
  end subroutine phs_config_get_masses_in

  function phs_config_get_md5sum (phs_config) result (md5sum)
    class(phs_config_t), intent(in) :: phs_config
    character(32) :: md5sum
    md5sum = phs_config%md5sum_phs_config
  end function phs_config_get_md5sum

  subroutine phs_base_write (object, unit)
    class(phs_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, c, i
    u = given_output_unit (unit)
    write (u, "(1x,A)", advance="no")  "Partonic phase space: parameters"
    if (object%r_defined) then
       write (u, *)
    else
       write (u, "(1x,A)")  "[undefined]"
    end if
    write (u, "(3x,A,999(1x," // FMT_19 // "))") "m_in    =", object%m_in
    write (u, "(3x,A,999(1x," // FMT_19 // "))") "m_out   =", object%m_out
    write (u, "(3x,A," // FMT_19 // ")")  "Flux   = ", object%flux
    write (u, "(3x,A," // FMT_19 // ")")  "Volume = ", object%volume
    if (allocated (object%f)) then
       do c = 1, size (object%r, 2)
          write (u, "(1x,A,I0,A)", advance="no")  "Channel #", c, ":"
          if (c == object%selected_channel) then
             write (u, "(1x,A)")  "[selected]"
          else
             write (u, *)
          end if
          write (u, "(3x,A)", advance="no")  "r ="
          do i = 1, size (object%r, 1)
             write (u, "(1x,F9.7)", advance="no")  object%r(i,c)
          end do
          write (u, *)
          write (u, "(3x,A,1x,ES13.7)")  "f =", object%f(c)
       end do
    end if
    write (u, "(1x,A)")  "Partonic phase space: momenta"
    if (object%p_defined) then
       write (u, "(3x,A," // FMT_19 // ")")  "sqrts  = ", object%sqrts_hat
    end if
    write (u, "(1x,A)", advance="no")  "Incoming:"
    if (object%p_defined) then
       write (u, *)
    else
       write (u, "(1x,A)")  "[undefined]"
    end if
    if (allocated (object%p)) then
       do i = 1, size (object%p)
          call vector4_write (object%p(i), u)
       end do
    end if
    write (u, "(1x,A)", advance="no")  "Outgoing:"
    if (object%q_defined) then
       write (u, *)
    else
       write (u, "(1x,A)")  "[undefined]"
    end if
    if (allocated (object%q)) then
       do i = 1, size (object%q)
          call vector4_write (object%q(i), u)
       end do
    end if
    if (object%p_defined .and. .not. object%config%cm_frame) then
       write (u, "(1x,A)")  "Transformation c.m -> lab frame"
       call lorentz_transformation_write (object%lt_cm_to_lab, u)
    end if
  end subroutine phs_base_write

  subroutine phs_base_init (phs, phs_config)
    class(phs_t), intent(out) :: phs
    class(phs_config_t), intent(in), target :: phs_config
    real(default), dimension(phs_config%n_in) :: m_in
    real(default), dimension(phs_config%n_out) :: m_out
    phs%config => phs_config
    allocate (phs%active_channel (phs%config%n_channel))
    phs%active_channel = .true.
    allocate (phs%r (phs%config%n_par, phs%config%n_channel));  phs%r = 0
    allocate (phs%f (phs%config%n_channel));                    phs%f = 0
    allocate (phs%p (phs%config%n_in))
    !!! !!! !!! Workaround for gfortran 5.0 ICE
    m_in  = phs_config%flv(:phs_config%n_in, 1)%get_mass ()
    m_out = phs_config%flv(phs_config%n_in+1:, 1)%get_mass ()
    allocate (phs%m_in  (phs%config%n_in), source = m_in)
    !!! allocate (phs%m_in  (phs%config%n_in), &
    !!!      source = phs_config%flv(:phs_config%n_in, 1)%get_mass ())
    allocate (phs%q (phs%config%n_out))
    allocate (phs%m_out (phs%config%n_out), source = m_out)
    !!! allocate (phs%m_out (phs%config%n_out), &
    !!!      source = phs_config%flv(phs_config%n_in+1:, 1)%get_mass ())
    call phs%compute_flux ()
  end subroutine phs_base_init

  subroutine phs_base_select_channel (phs, channel)
    class(phs_t), intent(inout) :: phs
    integer, intent(in), optional :: channel
    if (present (channel)) then
       phs%selected_channel = channel
    else
       phs%selected_channel = 0
    end if
  end subroutine phs_base_select_channel

  subroutine phs_set_incoming_momenta (phs, p)
    class(phs_t), intent(inout) :: phs
    type(vector4_t), dimension(:), intent(in) :: p
    type(vector4_t) :: p0, p1
    type(lorentz_transformation_t) :: lt0, lt0_inv, lt_inv
    !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
    integer :: i
    phs%p = p
    if (phs%config%cm_frame) then
       phs%sqrts_hat = phs%config%sqrts
       phs%p = p
       phs%lt_cm_to_lab = identity
    else
       p0 = sum (p)
       if (phs%config%sqrts_fixed) then
          phs%sqrts_hat = phs%config%sqrts
       else
          phs%sqrts_hat = p0 ** 1
       end if
       lt0 = boost (p0, phs%sqrts_hat)
       select case (phs%config%n_in)
       case (1)
          phs%lt_cm_to_lab = lt0
       case (2)
          !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
          lt0_inv = inverse (lt0)
          p1 = lt0_inv * p(1)
          !!! p1 = inverse (lt0) * p(1)
          phs%lt_cm_to_lab = lt0 * rotation_to_2nd (3, space_part (p1))
       end select
       !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
       lt_inv = inverse (phs%lt_cm_to_lab)
       do i = 1, size (p)
          phs%p(i) = lt_inv * p(i)
       end do
       !!! phs%p = inverse (phs%lt_cm_to_lab) * p
    end if
    phs%p_defined = .true.
  end subroutine phs_set_incoming_momenta

  subroutine phs_set_outgoing_momenta (phs, q)
    class(phs_t), intent(inout) :: phs
    type(vector4_t), dimension(:), intent(in) :: q
    !!! !!! !!! Workaround for standard-semantics ifort 16.0 bug
    type(lorentz_transformation_t) :: l_inv
    integer :: i
    if (phs%p_defined) then
       if (phs%config%cm_frame) then
          phs%q = q
       else
          l_inv = inverse (phs%lt_cm_to_lab)
          do i = 1, size (q)
             phs%q(i) = l_inv * q(i)
          end do
          !!! phs%q = inverse (phs%lt_cm_to_lab) * q
       end if
       phs%q_defined = .true.
    end if
  end subroutine phs_set_outgoing_momenta

  subroutine phs_get_outgoing_momenta (phs, q)
    class(phs_t), intent(in) :: phs
    type(vector4_t), dimension(:), intent(out) :: q
    if (phs%p_defined .and. phs%q_defined) then
       if (phs%config%cm_frame) then
          q = phs%q
       else
          q = phs%lt_cm_to_lab * phs%q
       end if
    else
       q = vector4_null
    end if
  end subroutine phs_get_outgoing_momenta

  function phs_is_cm_frame (phs) result (cm_frame)
    logical :: cm_frame
    class(phs_t), intent(in) :: phs
    cm_frame = phs%config%cm_frame
  end function phs_is_cm_frame

  elemental function phs_get_n_tot (phs) result (n_tot)
    integer :: n_tot
    class(phs_t), intent(in) :: phs
    n_tot = phs%config%n_tot
  end function phs_get_n_tot

  subroutine phs_set_lorentz_transformation (phs, lt)
    class(phs_t), intent(inout) :: phs
    type(lorentz_transformation_t), intent(in) :: lt
    phs%lt_cm_to_lab = lt
  end subroutine phs_set_lorentz_transformation

  function phs_get_lorentz_transformation (phs) result (lt)
    type(lorentz_transformation_t) :: lt
    class(phs_t), intent(in) :: phs
    lt = phs%lt_cm_to_lab
  end function phs_get_lorentz_transformation

  subroutine phs_get_mcpar (phs, c, r)
    class(phs_t), intent(in) :: phs
    integer, intent(in) :: c
    real(default), dimension(:), intent(out) :: r
    if (phs%r_defined) then
       r = phs%r(:,c)
    else
       r = 0
    end if
  end subroutine phs_get_mcpar

  function phs_get_f (phs, c) result (f)
    class(phs_t), intent(in) :: phs
    integer, intent(in) :: c
    real(default) :: f
    if (phs%r_defined) then
       f = phs%f(c)
    else
       f = 0
    end if
  end function phs_get_f

  function phs_get_overall_factor (phs) result (f)
    class(phs_t), intent(in) :: phs
    real(default) :: f
    f = phs%flux * phs%volume
  end function phs_get_overall_factor

  subroutine phs_compute_flux (phs)
    class(phs_t), intent(inout) :: phs
    real(default) :: s_hat, lda
    select case (phs%config%n_in)
    case (1)
       if (.not. phs%p_defined) then
          phs%flux = twopi4 / (2 * phs%m_in(1))
       end if
    case (2)
       if (phs%p_defined) then
          if (phs%config%sqrts_fixed) then
             return
          else
             s_hat = sum (phs%p) ** 2
          end if
       else
          if (phs%config%sqrts_fixed) then
             s_hat = phs%config%sqrts ** 2
          else
             return
          end if
       end if
       select case (phs%config%n_out)
       case (2:)
          lda = lambda (s_hat, phs%m_in(1) ** 2, phs%m_in(2) ** 2)
          if (lda > 0) then
             phs%flux = conv * twopi4 / (2 * sqrt (lda))
          else
             phs%flux = 0
          end if
       case (1)
          phs%flux = conv * twopi &
               / (2 * phs%config%sqrts ** 2 * phs%m_out(1) ** 2)
       case default
          phs%flux = 0
       end select
    end select
  end subroutine phs_compute_flux

  function phs_get_sqrts (phs) result (sqrts)
    real(default) :: sqrts
    class(phs_t), intent(in) :: phs
    sqrts = phs%config%sqrts
  end function phs_get_sqrts

  subroutine compute_kinematics_solid_angle (p, q, x)
    type(vector4_t), dimension(2), intent(in) :: p
    type(vector4_t), dimension(2), intent(out) :: q
    real(default), dimension(2), intent(in) :: x
    real(default) :: ct, st, phi
    type(lorentz_transformation_t) :: rot
    integer :: i
    ct = 1 - 2*x(1)
    st = sqrt (1 - ct**2)
    phi = twopi * x(2)
    rot = rotation (phi, 3) * rotation (ct, st, 2)
    do i = 1, 2
       q(i) = rot * p(i)
    end do
  end subroutine compute_kinematics_solid_angle

  subroutine inverse_kinematics_solid_angle (p, q, x)
    type(vector4_t), dimension(:), intent(in) :: p
    type(vector4_t), dimension(2), intent(in) :: q
    real(default), dimension(2), intent(out) :: x
    real(default) :: ct, phi
    ct = polar_angle_ct (q(1))
    phi = azimuthal_angle (q(1))
    x(1) = (1 - ct) / 2
    x(2) = phi / twopi
  end subroutine inverse_kinematics_solid_angle

  subroutine pacify_phs (phs)
    class(phs_t), intent(inout) :: phs
    if (phs%p_defined) then
       call pacify (phs%p, 30 * epsilon (1._default) * phs%config%sqrts)
       call pacify (phs%lt_cm_to_lab, 30 * epsilon (1._default))
    end if
    if (phs%q_defined) then
       call pacify (phs%q, 30 * epsilon (1._default) * phs%config%sqrts)
    end if
  end subroutine pacify_phs


end module phs_base
