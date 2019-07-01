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

module process_config

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use io_units
  use md5
  use os_interface
  use diagnostics
  use sf_base
  use sf_mappings
  use sm_qcd
  use physics_defs
  use integration_results
  use model_data
  use interactions
  use quantum_numbers
  use flavors
  use helicities
  use colors
  use rng_base
  use state_matrices
  use process_libraries
  use process_constants
  use prc_core
  use prc_user_defined
  use prc_openloops, only: prc_openloops_t
  use prc_recola, only: prc_recola_t
  use prc_threshold, only: prc_threshold_t
  use beams
  use mci_base
  use beam_structures
  use phs_base
  use variables
  use expr_base

  implicit none
  private

  public :: process_config_data_t
  public :: process_metadata_t
  public :: process_beam_config_t
  public :: process_component_t
  public :: process_term_t

  integer, parameter, public :: COMP_DEFAULT = 0
  integer, parameter, public :: COMP_REAL_FIN = 1
  integer, parameter, public :: COMP_MASTER = 2
  integer, parameter, public :: COMP_VIRT = 3
  integer, parameter, public :: COMP_REAL = 4
  integer, parameter, public :: COMP_REAL_SING = 5
  integer, parameter, public :: COMP_MISMATCH = 6
  integer, parameter, public :: COMP_PDF = 7
  integer, parameter, public :: COMP_SUB = 8
  integer, parameter, public :: COMP_RESUM = 9


  type :: process_config_data_t
     integer :: n_in = 0
     integer :: n_components = 0
     integer :: n_terms = 0
     integer :: n_mci = 0
     type(os_data_t) :: os_data
     class(rng_factory_t), allocatable :: rng_factory
     type(string_t) :: model_name
     class(model_data_t), pointer :: model => null ()
     type(qcd_t) :: qcd
     class(expr_factory_t), allocatable :: ef_cuts
     class(expr_factory_t), allocatable :: ef_scale
     class(expr_factory_t), allocatable :: ef_fac_scale
     class(expr_factory_t), allocatable :: ef_ren_scale
     class(expr_factory_t), allocatable :: ef_weight
     character(32) :: md5sum = ""
   contains
     procedure :: write => process_config_data_write
     procedure :: init => process_config_data_init
     procedure :: final => process_config_data_final
     procedure :: compute_md5sum => process_config_data_compute_md5sum
     procedure :: get_md5sum => process_config_data_get_md5sum
  end type process_config_data_t

  type :: process_metadata_t
     integer :: type = PRC_UNKNOWN
     type(string_t) :: id
     integer :: num_id = 0
     type(string_t) :: run_id
     type(var_list_t) :: var_list
     type(process_library_t), pointer :: lib => null ()
     integer :: lib_update_counter = 0
     integer :: lib_index = 0
     integer :: n_components = 0
     type(string_t), dimension(:), allocatable :: component_id
     type(string_t), dimension(:), allocatable :: component_description
     logical, dimension(:), allocatable :: active
   contains
     procedure :: final => process_metadata_final
     procedure :: write => process_metadata_write
     procedure :: show => process_metadata_show
     procedure :: init => process_metadata_init
     procedure :: deactivate_component => process_metadata_deactivate_component
  end type process_metadata_t

  type :: process_beam_config_t
     type(beam_data_t) :: data
     integer :: n_strfun = 0
     integer :: n_channel = 1
     integer :: n_sfpar = 0
     type(sf_config_t), dimension(:), allocatable :: sf
     type(sf_channel_t), dimension(:), allocatable :: sf_channel
     logical :: azimuthal_dependence = .false.
     logical :: lab_is_cm_frame = .true.
     character(32) :: md5sum = ""
     logical :: sf_trace = .false.
     type(string_t) :: sf_trace_file
   contains
     procedure :: write => process_beam_config_write
     procedure :: final => process_beam_config_final
     procedure :: init_beam_structure => process_beam_config_init_beam_structure
     procedure :: init_scattering => process_beam_config_init_scattering
     procedure :: init_decay => process_beam_config_init_decay
     procedure :: startup_message => process_beam_config_startup_message
     procedure :: init_sf_chain => process_beam_config_init_sf_chain
     procedure :: allocate_sf_channels => process_beam_config_allocate_sf_channels
     procedure :: set_sf_channel => process_beam_config_set_sf_channel
     procedure :: sf_startup_message => process_beam_config_sf_startup_message
     procedure :: get_pdf_set => process_beam_config_get_pdf_set
     procedure :: get_beam_file => process_beam_config_get_beam_file
     procedure :: compute_md5sum => process_beam_config_compute_md5sum
     procedure :: get_md5sum => process_beam_config_get_md5sum
  end type process_beam_config_t

  type :: process_component_t
     type(process_component_def_t), pointer :: config => null ()
     integer :: index = 0
     logical :: active = .false.
     class(mci_t), allocatable :: mci_template
     integer, dimension(:), allocatable :: i_term
     integer :: i_mci = 0
     class(phs_config_t), allocatable :: phs_config
     character(32) :: md5sum_phs = ""
     integer :: component_type = COMP_DEFAULT
   contains
     procedure :: final => process_component_final
     procedure :: write => process_component_write
     procedure :: init => process_component_init
     procedure :: is_active => process_component_is_active
     procedure :: has_mci_template => process_component_has_mci_template
     procedure :: extract_mci_template => process_component_extract_mci_template
     procedure :: configure_phs => process_component_configure_phs
     procedure :: compute_md5sum => process_component_compute_md5sum
     procedure :: collect_channels => process_component_collect_channels
     procedure :: get_config => process_component_get_config
     procedure :: get_md5sum => process_component_get_md5sum
     procedure :: get_n_phs_par => process_component_get_n_phs_par
     procedure :: get_phs_config => process_component_get_phs_config
     procedure :: get_nlo_type => process_component_get_nlo_type
     procedure :: needs_mci_entry => process_component_needs_mci_entry
     procedure :: can_be_integrated => process_component_can_be_integrated
  end type process_component_t

  type :: process_term_t
     integer :: i_term_global = 0
     integer :: i_component = 0
     integer :: i_term = 0
     integer :: i_sub = 0
     integer :: i_core = 0
     integer :: n_allowed = 0
     type(process_constants_t) :: data
     real(default) :: alpha_s = 0
     integer, dimension(:), allocatable :: flv, hel, col
     type(interaction_t) :: int
     type(interaction_t), pointer :: int_eff => null ()
   contains
     procedure :: write => process_term_write
     procedure :: write_state_summary => process_term_write_state_summary
     procedure :: final => process_term_final
     procedure :: init => process_term_init
     procedure :: setup_interaction => process_term_setup_interaction
     procedure :: get_process_constants => process_term_get_process_constants
  end type process_term_t


contains

  subroutine process_config_data_write (config, u, &
       counters, os_data, rng_factory, model, expressions)
    class(process_config_data_t), intent(in) :: config
    integer, intent(in) :: u
    logical, intent(in) :: counters
    logical, intent(in) :: os_data
    logical, intent(in) :: rng_factory
    logical, intent(in) :: model
    logical, intent(in) :: expressions
    write (u, "(1x,A)") "Configuration data:"
    if (counters) then
       write (u, "(3x,A,I0)") "Number of incoming particles = ", &
            config%n_in
       write (u, "(3x,A,I0)") "Number of process components = ", &
            config%n_components
       write (u, "(3x,A,I0)") "Number of process terms      = ", &
            config%n_terms
       write (u, "(3x,A,I0)") "Number of MCI configurations = ", &
            config%n_mci
    end if
    if (os_data) then
       call os_data_write (config%os_data, u)
    end if
    if (associated (config%model)) then
       write (u, "(3x,A,A)")  "Model = ", char (config%model_name)
       if (model) then
          call write_separator (u)
          call config%model%write (u)
          call write_separator (u)
       end if
    else
       write (u, "(3x,A,A,A)")  "Model = ", char (config%model_name), &
            " [not associated]"
    end if
    call config%qcd%write (u, show_md5sum = .false.)
    if (rng_factory) then
       if (allocated (config%rng_factory)) then
          write (u, "(2x)", advance = "no")
          call config%rng_factory%write (u)
       end if
    end if
    call write_separator (u)
    if (expressions) then
       if (allocated (config%ef_cuts)) then
          call write_separator (u)
          write (u, "(3x,A)") "Cut expression:"
          call config%ef_cuts%write (u)
       end if
       if (allocated (config%ef_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Scale expression:"
          call config%ef_scale%write (u)
       end if
       if (allocated (config%ef_fac_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Factorization scale expression:"
          call config%ef_fac_scale%write (u)
       end if
       if (allocated (config%ef_ren_scale)) then
          call write_separator (u)
          write (u, "(3x,A)") "Renormalization scale expression:"
          call config%ef_ren_scale%write (u)
       end if
       if (allocated (config%ef_weight)) then
          call write_separator (u)
          write (u, "(3x,A)") "Weight expression:"
          call config%ef_weight%write (u)
       end if
    else
       call write_separator (u)
       write (u, "(3x,A)") "Expressions (cut, scales, weight): [not shown]"
    end if
    if (config%md5sum /= "") then
       call write_separator (u)
       write (u, "(3x,A,A,A)")  "MD5 sum (config)  = '", config%md5sum, "'"
    end if
  end subroutine process_config_data_write

  subroutine process_config_data_init &
       (config, meta, os_data, qcd, rng_factory, model)
    class(process_config_data_t), intent(out) :: config
    type(process_metadata_t), intent(in) :: meta
    type(os_data_t), intent(in) :: os_data
    type(qcd_t), intent(in) :: qcd
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    class(model_data_t), intent(inout), pointer :: model
    config%n_in = meta%lib%get_n_in (meta%id)
    config%n_components = size (meta%component_id)
    config%os_data = os_data
    config%qcd = qcd
    call move_alloc (from = rng_factory, to = config%rng_factory)
    config%model_name = model%get_name ()
    config%model => model
    model => null ()
  end subroutine process_config_data_init

  subroutine process_config_data_final (config)
    class(process_config_data_t), intent(inout) :: config
    if (associated (config%model)) then
       call config%model%final ()
       deallocate (config%model)
    end if
  end subroutine process_config_data_final

  subroutine process_config_data_compute_md5sum (config)
    class(process_config_data_t), intent(inout) :: config
    integer :: u
    if (config%md5sum == "") then
       u = free_unit ()
       open (u, status = "scratch", action = "readwrite")
       call config%write (u, counters = .false., os_data = .false., &
            rng_factory = .false., model = .true., expressions = .true.)
       rewind (u)
       config%md5sum = md5sum (u)
       close (u)
    end if
  end subroutine process_config_data_compute_md5sum

  pure function process_config_data_get_md5sum (config) result (md5)
    character(32) :: md5
    class(process_config_data_t), intent(in) :: config
    md5 = config%md5sum
  end function process_config_data_get_md5sum

  subroutine process_metadata_final (meta)
    class(process_metadata_t), intent(inout) :: meta
    call meta%var_list%final (follow_link=.true.)
  end subroutine process_metadata_final

  subroutine process_metadata_write (meta, u, var_list, screen)
    class(process_metadata_t), intent(in) :: meta
    integer, intent(in) :: u
    logical, intent(in) :: var_list, screen
    integer :: i
    select case (meta%type)
    case (PRC_UNKNOWN)
       if (screen) then
          write (msg_buffer, "(A)") "Process [undefined]"
       else
          write (u, "(1x,A)") "Process [undefined]"
       end if
       return
    case (PRC_DECAY)
       if (screen) then
          write (msg_buffer, "(A,1x,A,A,A)") "Process [decay]:", &
               "'", char (meta%id), "'"
       else
          write (u, "(1x,A)", advance="no") "Process [decay]:"
       end if
    case (PRC_SCATTERING)
       if (screen) then
          write (msg_buffer, "(A,1x,A,A,A)") "Process [scattering]:", &
               "'", char (meta%id), "'"
       else
          write (u, "(1x,A)", advance="no") "Process [scattering]:"
       end if
    case default
       call msg_bug ("process_write: undefined process type")
    end select
    if (screen)  then
       call msg_message ()
    else
       write (u, "(1x,A,A,A)") "'", char (meta%id), "'"
    end if
    if (meta%num_id /= 0) then
       if (screen) then
          write (msg_buffer, "(2x,A,I0)") "ID (num)      = ", meta%num_id
          call msg_message ()
       else
          write (u, "(3x,A,I0)") "ID (num)      = ", meta%num_id
       end if
    end if
    if (screen) then
       if (meta%run_id /= "") then
          write (msg_buffer, "(2x,A,A,A)") "Run ID        = '", &
               char (meta%run_id), "'"
          call msg_message ()
       end if
    else
       write (u, "(3x,A,A,A)") "Run ID        = '", char (meta%run_id), "'"
    end if
    if (associated (meta%lib)) then
       if (screen) then
          write (msg_buffer, "(2x,A,A,A)")  "Library name  = '", &
               char (meta%lib%get_name ()), "'"
          call msg_message ()
       else
          write (u, "(3x,A,A,A)")  "Library name  = '", &
               char (meta%lib%get_name ()), "'"
       end if
    else
       if (screen) then
          write (msg_buffer, "(2x,A)")  "Library name  = [not associated]"
          call msg_message ()
       else
          write (u, "(3x,A)")  "Library name  = [not associated]"
       end if
    end if
    if (screen) then
       write (msg_buffer, "(2x,A,I0)")  "Process index = ", meta%lib_index
       call msg_message ()
    else
       write (u, "(3x,A,I0)")  "Process index = ", meta%lib_index
    end if
    if (allocated (meta%component_id)) then
       if (screen) then
          if (any (meta%active)) then
             write (msg_buffer, "(2x,A)")  "Process components:"
          else
             write (msg_buffer, "(2x,A)")  "Process components: [none]"
          end if
          call msg_message ()
       else
          write (u, "(3x,A)")  "Process components:"
       end if
       do i = 1, size (meta%component_id)
          if (.not. meta%active(i))  cycle
          if (screen) then
             write (msg_buffer, "(4x,I0,9A)")  i, ": '", &
                  char (meta%component_id (i)), "':   ", &
                  char (meta%component_description (i))
             call msg_message ()
          else
             write (u, "(5x,I0,9A)")  i, ": '", &
                  char (meta%component_id (i)), "':   ", &
                  char (meta%component_description (i))
          end if
       end do
    end if
    if (screen) then
       write (msg_buffer, "(A)")  repeat ("-", 72)
       call msg_message ()
    else
       call write_separator (u)
    end if
    if (screen)  return
    if (var_list) then
       write (u, "(1x,A)")  "Variable list:"
       call write_separator (u)
       call var_list_write (meta%var_list, u)
    else
       write (u, "(1x,A)")  "Variable list: [not shown]"
    end if
  end subroutine process_metadata_write

  subroutine process_metadata_show (meta, u, model_name)
    class(process_metadata_t), intent(in) :: meta
    integer, intent(in) :: u
    type(string_t), intent(in) :: model_name
    integer :: i
    select case (meta%type)
    case (PRC_UNKNOWN)
       write (u, "(A)") "Process: [undefined]"
       return
    case default
       write (u, "(A)", advance="no") "Process:"
    end select
    write (u, "(1x,A)", advance="no") char (meta%id)
    select case (meta%num_id)
    case (0)
    case default
       write (u, "(1x,'(',I0,')')", advance="no") meta%num_id
    end select
    select case (char (model_name))
    case ("")
    case default
       write (u, "(1x,'[',A,']')", advance="no")  char (model_name)
    end select
    write (u, *)
    if (allocated (meta%component_id)) then
       do i = 1, size (meta%component_id)
          if (meta%active(i)) then
             write (u, "(2x,I0,':',1x,A)")  i, &
                  char (meta%component_description (i))
          end if
       end do
    end if
  end subroutine process_metadata_show

  subroutine process_metadata_init (meta, id, run_id, lib)
    class(process_metadata_t), intent(out) :: meta
    type(string_t), intent(in) :: id
    type(string_t), intent(in) :: run_id
    type(process_library_t), intent(in), target :: lib
    select case (lib%get_n_in (id))
    case (1);  meta%type = PRC_DECAY
    case (2);  meta%type = PRC_SCATTERING
    case default
       call msg_bug ("Process '" // char (id) // "': impossible n_in")
    end select
    meta%id = id
    meta%run_id = run_id
    meta%lib => lib
    meta%lib_update_counter = lib%get_update_counter ()
    meta%lib_index = lib%get_entry_index (id)
    meta%num_id = lib%get_num_id (id)
    call lib%get_component_list (id, meta%component_id)
    meta%n_components = size (meta%component_id)
    call lib%get_component_description_list (id, meta%component_description)
    allocate (meta%active (meta%n_components), source = .true.)
  end subroutine process_metadata_init

  subroutine process_metadata_deactivate_component (meta, i)
    class(process_metadata_t), intent(inout) :: meta
    integer, intent(in) :: i
    call msg_message ("Process component '" &
         // char (meta%component_id(i)) // "': matrix element vanishes")
    meta%active(i) = .false.
  end subroutine process_metadata_deactivate_component

  subroutine process_beam_config_write (object, unit, verbose)
    class(process_beam_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i, c
    u = given_output_unit (unit)
    call object%data%write (u, verbose = verbose)
    if (object%data%initialized) then
       write (u, "(3x,A,L1)")  "Azimuthal dependence    = ", &
            object%azimuthal_dependence
       write (u, "(3x,A,L1)")  "Lab frame is c.m. frame = ", &
            object%lab_is_cm_frame
       if (object%md5sum /= "") then
          write (u, "(3x,A,A,A)")  "MD5 sum (beams/strf) = '", &
               object%md5sum, "'"
       end if
       if (allocated (object%sf)) then
          do i = 1, size (object%sf)
             call object%sf(i)%write (u)
          end do
          if (any_sf_channel_has_mapping (object%sf_channel)) then
             write (u, "(1x,A,L1)")  "Structure-function mappings per channel:"
             do c = 1, object%n_channel
                write (u, "(3x,I0,':')", advance="no")  c
                call object%sf_channel(c)%write (u)
             end do
          end if
       end if
    end if
  end subroutine process_beam_config_write

  subroutine process_beam_config_final (object)
    class(process_beam_config_t), intent(inout) :: object
    call object%data%final ()
  end subroutine process_beam_config_final

  subroutine process_beam_config_init_beam_structure &
       (beam_config, beam_structure, sqrts, model, decay_rest_frame)
    class(process_beam_config_t), intent(out) :: beam_config
    type(beam_structure_t), intent(in) :: beam_structure
    logical, intent(in), optional :: decay_rest_frame
    real(default), intent(in) :: sqrts
    class(model_data_t), intent(in), target :: model
    call beam_config%data%init_structure (beam_structure, &
         sqrts, model, decay_rest_frame)
    beam_config%lab_is_cm_frame = beam_config%data%cm_frame ()
  end subroutine process_beam_config_init_beam_structure

  subroutine process_beam_config_init_scattering &
       (beam_config, flv_in, sqrts, beam_structure)
    class(process_beam_config_t), intent(out) :: beam_config
    type(flavor_t), dimension(2), intent(in) :: flv_in
    real(default), intent(in) :: sqrts
    type(beam_structure_t), intent(in), optional :: beam_structure
    if (present (beam_structure)) then
       if (beam_structure%polarized ()) then
          call beam_config%data%init_sqrts (sqrts, flv_in, &
               beam_structure%get_smatrix (), beam_structure%get_pol_f ())
       else
          call beam_config%data%init_sqrts (sqrts, flv_in)
       end if
    else
       call beam_config%data%init_sqrts (sqrts, flv_in)
    end if
  end subroutine process_beam_config_init_scattering

  subroutine process_beam_config_init_decay &
       (beam_config, flv_in, rest_frame, beam_structure)
    class(process_beam_config_t), intent(out) :: beam_config
    type(flavor_t), dimension(1), intent(in) :: flv_in
    logical, intent(in), optional :: rest_frame
    type(beam_structure_t), intent(in), optional :: beam_structure
    if (present (beam_structure)) then
       if (beam_structure%polarized ()) then
          call beam_config%data%init_decay (flv_in, &
               beam_structure%get_smatrix (), beam_structure%get_pol_f (), &
               rest_frame = rest_frame)
       else
          call beam_config%data%init_decay (flv_in, rest_frame = rest_frame)
       end if
    else
       call beam_config%data%init_decay (flv_in, &
            rest_frame = rest_frame)
    end if
    beam_config%lab_is_cm_frame = beam_config%data%cm_frame ()
  end subroutine process_beam_config_init_decay

  subroutine process_beam_config_startup_message &
       (beam_config, unit, beam_structure)
    class(process_beam_config_t), intent(in) :: beam_config
    integer, intent(in), optional :: unit
    type(beam_structure_t), intent(in), optional :: beam_structure
    integer :: u
    u = free_unit ()
    open (u, status="scratch", action="readwrite")
    if (present (beam_structure)) then
       call beam_structure%write (u)
    end if
    call beam_config%data%write (u)
    rewind (u)
    do
       read (u, "(1x,A)", end=1)  msg_buffer
       call msg_message ()
    end do
1   continue
    close (u)
  end subroutine process_beam_config_startup_message

  subroutine process_beam_config_init_sf_chain &
       (beam_config, sf_config, sf_trace_file)
    class(process_beam_config_t), intent(inout) :: beam_config
    type(sf_config_t), dimension(:), intent(in) :: sf_config
    type(string_t), intent(in), optional :: sf_trace_file
    integer :: i
    beam_config%n_strfun = size (sf_config)
    allocate (beam_config%sf (beam_config%n_strfun))
    do i = 1, beam_config%n_strfun
       associate (sf => sf_config(i))
         call beam_config%sf(i)%init (sf%i, sf%data)
         if (.not. sf%data%is_generator ()) then
            beam_config%n_sfpar = beam_config%n_sfpar + sf%data%get_n_par ()
         end if
       end associate
    end do
    if (present (sf_trace_file)) then
       beam_config%sf_trace = .true.
       beam_config%sf_trace_file = sf_trace_file
    end if
  end subroutine process_beam_config_init_sf_chain

  subroutine process_beam_config_allocate_sf_channels (beam_config, n_channel)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer, intent(in) :: n_channel
    beam_config%n_channel = n_channel
    call allocate_sf_channels (beam_config%sf_channel, &
         n_channel = n_channel, &
         n_strfun = beam_config%n_strfun)
  end subroutine process_beam_config_allocate_sf_channels

  subroutine process_beam_config_set_sf_channel (beam_config, c, sf_channel)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer, intent(in) :: c
    type(sf_channel_t), intent(in) :: sf_channel
    beam_config%sf_channel(c) = sf_channel
  end subroutine process_beam_config_set_sf_channel

  subroutine process_beam_config_sf_startup_message &
       (beam_config, sf_string, unit)
    class(process_beam_config_t), intent(in) :: beam_config
    type(string_t), intent(in) :: sf_string
    integer, intent(in), optional :: unit
    if (beam_config%n_strfun > 0) then
       call msg_message ("Beam structure: " // char (sf_string), unit = unit)
       write (msg_buffer, "(A,3(1x,I0,1x,A))") &
            "Beam structure:", &
            beam_config%n_channel, "channels,", &
            beam_config%n_sfpar, "dimensions"
       call msg_message (unit = unit)
       if (beam_config%sf_trace) then
          call msg_message ("Beam structure: tracing &
               &values in '" // char (beam_config%sf_trace_file) // "'")
       end if
    end if
  end subroutine process_beam_config_sf_startup_message

  function process_beam_config_get_pdf_set (beam_config) result (pdf_set)
    class(process_beam_config_t), intent(in) :: beam_config
    integer :: pdf_set
    integer :: i
    pdf_set = 0
    if (allocated (beam_config%sf)) then
       do i = 1, size (beam_config%sf)
          pdf_set = beam_config%sf(i)%get_pdf_set ()
          if (pdf_set /= 0)  return
       end do
    end if
  end function process_beam_config_get_pdf_set

  function process_beam_config_get_beam_file (beam_config) result (file)
    class(process_beam_config_t), intent(in) :: beam_config
    type(string_t) :: file
    integer :: i
    file = ""
    if (allocated (beam_config%sf)) then
       do i = 1, size (beam_config%sf)
          file = beam_config%sf(i)%get_beam_file ()
          if (file /= "")  return
       end do
    end if
  end function process_beam_config_get_beam_file

  subroutine process_beam_config_compute_md5sum (beam_config)
    class(process_beam_config_t), intent(inout) :: beam_config
    integer :: u
    if (beam_config%md5sum == "") then
       u = free_unit ()
       open (u, status = "scratch", action = "readwrite")
       call beam_config%write (u, verbose=.true.)
       rewind (u)
       beam_config%md5sum = md5sum (u)
       close (u)
    end if
  end subroutine process_beam_config_compute_md5sum

  pure function process_beam_config_get_md5sum (beam_config) result (md5)
    character(32) :: md5
    class(process_beam_config_t), intent(in) :: beam_config
    md5 = beam_config%md5sum
  end function process_beam_config_get_md5sum

  subroutine process_component_final (object)
    class(process_component_t), intent(inout) :: object
    if (allocated (object%mci_template)) then
       call object%mci_template%final ()
    end if
    if (allocated (object%phs_config)) then
       call object%phs_config%final ()
    end if
  end subroutine process_component_final

  subroutine process_component_write (object, unit)
    class(process_component_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%config)) then
       write (u, "(1x,A,I0)")  "Component #", object%index
       call object%config%write (u)
       if (object%md5sum_phs /= "") then
          write (u, "(3x,A,A,A)")  "MD5 sum (phs)       = '", &
               object%md5sum_phs, "'"
       end if
    else
       write (u, "(1x,A)") "Process component: [not allocated]"
    end if
    if (.not. object%active) then
       write (u, "(1x,A)") "[Inactive]"
       return
    end if
    write (u, "(1x,A)") "Referenced data:"
    if (allocated (object%i_term)) then
       write (u, "(3x,A,999(1x,I0))") "Terms                    =", &
            object%i_term
    else
       write (u, "(3x,A)") "Terms                    = [undefined]"
    end if
    if (object%i_mci /= 0) then
       write (u, "(3x,A,I0)") "MC dataset               = ", object%i_mci
    else
       write (u, "(3x,A)") "MC dataset               = [undefined]"
    end if
    if (allocated (object%phs_config)) then
       call object%phs_config%write (u)
    end if
  end subroutine process_component_write

  subroutine process_component_init (component, &
       i_component, meta, config, &
       active, data, &
       mci_template, phs_config_template)
    class(process_component_t), intent(out) :: component
    integer, intent(in) :: i_component
    type(process_metadata_t), intent(in), target :: meta
    type(process_config_data_t), intent(in) :: config
    logical, intent(in) :: active
    type(process_constants_t), intent(in) :: data
    class(mci_t), intent(in), allocatable :: mci_template
    class(phs_config_t), intent(in), allocatable :: phs_config_template

    component%index = i_component
    component%config => meta%lib%get_component_def_ptr (meta%id, i_component)

    component%active = active
    if (component%active) then
       if (allocated (mci_template)) &
          allocate (component%mci_template, source = mci_template)
       allocate (component%phs_config, source = phs_config_template)
       call component%phs_config%init (data, config%model)
       call component%phs_config%set_component_index (component%index)
    end if
  end subroutine process_component_init

  elemental function process_component_is_active (component) result (active)
    logical :: active
    class(process_component_t), intent(in) :: component
    active = component%active
  end function process_component_is_active

  pure function process_component_has_mci_template (component) &
         result (is_allocated)
    logical :: is_allocated
    class(process_component_t), intent(in) :: component
    is_allocated = allocated (component%mci_template)
  end function process_component_has_mci_template

  function process_component_extract_mci_template (component) &
         result (mci_template)
    class(mci_t), allocatable :: mci_template
    class(process_component_t), intent(in) :: component
    if (allocated (component%mci_template)) &
       allocate (mci_template, source = component%mci_template)
  end function process_component_extract_mci_template

  subroutine process_component_configure_phs &
       (component, sqrts, beam_config, rebuild, &
        ignore_mismatch)
    class(process_component_t), intent(inout) :: component
    real(default), intent(in) :: sqrts
    type(process_beam_config_t), intent(in) :: beam_config
    logical, intent(in), optional :: rebuild
    logical, intent(in), optional :: ignore_mismatch
    logical :: no_strfun
    integer :: nlo_type
    no_strfun = beam_config%n_strfun == 0
    nlo_type = component%config%get_nlo_type ()
    call component%phs_config%configure (sqrts, &
         azimuthal_dependence = beam_config%azimuthal_dependence, &
         sqrts_fixed = no_strfun, &
         cm_frame = beam_config%lab_is_cm_frame .and. no_strfun, &
         rebuild = rebuild, ignore_mismatch = ignore_mismatch, &
         nlo_type = nlo_type)
  end subroutine process_component_configure_phs

  subroutine process_component_compute_md5sum (component)
    class(process_component_t), intent(inout) :: component
    component%md5sum_phs = component%phs_config%get_md5sum ()
  end subroutine process_component_compute_md5sum

  subroutine process_component_collect_channels (component, coll)
    class(process_component_t), intent(inout) :: component
    type(phs_channel_collection_t), intent(inout) :: coll
    call component%phs_config%collect_channels (coll)
  end subroutine process_component_collect_channels

  function process_component_get_config (component) &
         result (config)
    type(process_component_def_t) :: config
    class(process_component_t), intent(in) :: component
    config = component%config
  end function process_component_get_config

  pure function process_component_get_md5sum (component) result (md5)
    type(string_t) :: md5
    class(process_component_t), intent(in) :: component
    md5 = component%config%get_md5sum () // component%md5sum_phs
  end function process_component_get_md5sum

  function process_component_get_n_phs_par (component) result (n_par)
    class(process_component_t), intent(in) :: component
    integer :: n_par
    n_par = component%phs_config%get_n_par ()
  end function process_component_get_n_phs_par

  subroutine process_component_get_phs_config (component, phs_config)
    class(process_component_t), intent(in), target :: component
    class(phs_config_t), intent(out), pointer :: phs_config
    phs_config => component%phs_config
  end subroutine process_component_get_phs_config

  elemental function process_component_get_nlo_type (component) result (nlo_type)
     integer :: nlo_type
     class(process_component_t), intent(in) :: component
     nlo_type = component%config%get_nlo_type ()
  end function process_component_get_nlo_type

  function process_component_needs_mci_entry (component, combined_integration) result (value)
    logical :: value
    class(process_component_t), intent(in) :: component
    logical, intent(in), optional :: combined_integration
    value = component%active
    if (present (combined_integration)) then
       if (combined_integration) &
            value = value .and. component%component_type <= COMP_MASTER
    end if
  end function process_component_needs_mci_entry

  elemental function process_component_can_be_integrated (component) result (active)
    logical :: active
    class(process_component_t), intent(in) :: component
    active = component%config%can_be_integrated ()
  end function process_component_can_be_integrated

  subroutine process_term_write (term, unit)
    class(process_term_t), intent(in) :: term
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A,I0)")  "Term #", term%i_term_global
    write (u, "(3x,A,I0)")  "Process component index      = ", &
         term%i_component
    write (u, "(3x,A,I0)")  "Term index w.r.t. component  = ", &
         term%i_term
    call write_separator (u)
    write (u, "(1x,A)")  "Hard interaction:"
    call write_separator (u)
    call term%int%basic_write (u)
  end subroutine process_term_write

  subroutine process_term_write_state_summary (term, core, unit)
    class(process_term_t), intent(in) :: term
    class(prc_core_t), intent(in) :: core
    integer, intent(in), optional :: unit
    integer :: u, i, f, h, c
    type(state_iterator_t) :: it
    character :: sgn
    u = given_output_unit (unit)
    write (u, "(1x,A,I0)")  "Term #", term%i_term_global
    call it%init (term%int%get_state_matrix_ptr ())
    do while (it%is_valid ())
       i = it%get_me_index ()
       f = term%flv(i)
       h = term%hel(i)
       if (allocated (term%col)) then
          c = term%col(i)
       else
          c = 1
       end if
       if (core%is_allowed (term%i_term, f, h, c)) then
          sgn = "+"
       else
          sgn = " "
       end if
       write (u, "(1x,A1,1x,I0,2x)", advance="no")  sgn, i
       call quantum_numbers_write (it%get_quantum_numbers (), u)
       write (u, *)
       call it%advance ()
    end do
  end subroutine process_term_write_state_summary

  subroutine process_term_final (term)
    class(process_term_t), intent(inout) :: term
    call term%int%final ()
  end subroutine process_term_final

  subroutine process_term_init &
       (term, i_term_global, i_component, i_term, core, model, &
        nlo_type, use_beam_pol, subtraction_method)
    class(process_term_t), intent(inout), target :: term
    integer, intent(in) :: i_term_global
    integer, intent(in) :: i_component
    integer, intent(in) :: i_term
    class(prc_core_t), intent(inout) :: core
    class(model_data_t), intent(in), target :: model
    integer, intent(in), optional :: nlo_type
    logical, intent(in), optional :: use_beam_pol
    type(string_t), intent(in), optional :: subtraction_method
    class(modelpar_data_t), pointer :: alpha_s_ptr
    logical :: use_internal_color
    term%i_term_global = i_term_global
    term%i_component = i_component
    term%i_term = i_term
    call core%get_constants (term%data, i_term)
    alpha_s_ptr => model%get_par_data_ptr (var_str ("alphas"))
    if (associated (alpha_s_ptr)) then
       term%alpha_s = alpha_s_ptr%get_real ()
    else
       term%alpha_s = -1
    end if
    use_internal_color = .false.
    if (present (subtraction_method)) &
         use_internal_color = (char (subtraction_method) == 'omega') &
         .or. (char (subtraction_method) == 'threshold')
    call term%setup_interaction (core, model, nlo_type = nlo_type, &
         pol_beams = use_beam_pol, use_internal_color = use_internal_color)
  end subroutine process_term_init

  subroutine process_term_setup_interaction (term, core, model, &
     nlo_type, pol_beams, use_internal_color)
    class(process_term_t), intent(inout) :: term
    class(prc_core_t), intent(inout) :: core
    class(model_data_t), intent(in), target :: model
    logical, intent(in), optional :: pol_beams
    integer, intent(in), optional :: nlo_type
    logical, intent(in), optional :: use_internal_color
    integer :: n, n_tot
    type(flavor_t), dimension(:), allocatable :: flv
    type(color_t), dimension(:), allocatable :: col
    type(helicity_t), dimension(:), allocatable :: hel
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    logical :: is_pol, use_color
    integer :: nlo_t, n_sub
    is_pol = .false.; if (present (pol_beams)) is_pol = pol_beams
    nlo_t = BORN; if (present (nlo_type)) nlo_t = nlo_type
    n_tot = term%data%n_in + term%data%n_out
    call count_number_of_states ()
    term%n_allowed = n
    call compute_n_sub ()
    call fill_quantum_numbers ()
    call term%int%basic_init &
         (term%data%n_in, 0, term%data%n_out, set_relations = .true.)
    select type (core)
    type is (prc_openloops_t)
       call setup_states_openloops ()
    type is (prc_threshold_t)
       call setup_states_threshold ()
    class is (prc_user_defined_base_t)
       call setup_states_other_user_defined ()
    class default
       call setup_states_omega ()
    end select
    call term%int%freeze ()
  contains
    subroutine count_number_of_states ()
      integer :: f, h, c
      n = 0
      select type (core)
      class is (prc_user_defined_base_t)
         do f = 1, term%data%n_flv
            do h = 1, term%data%n_hel
               do c = 1, term%data%n_col
                  n = n + 1
               end do
            end do
         end do
      class default !!! Omega and all test cores
         do f = 1, term%data%n_flv
            do h = 1, term%data%n_hel
               do c = 1, term%data%n_col
                  if (core%is_allowed (term%i_term, f, h, c))  n = n + 1
               end do
            end do
         end do
      end select
    end subroutine count_number_of_states

    subroutine compute_n_sub ()
      use_color = .false.; if (present (use_internal_color)) &
           use_color = use_internal_color
      if (nlo_t == NLO_VIRTUAL) then
         n_sub = 1
         if (.not. use_color) n_sub = n_sub + n_tot * (n_tot - 1) / 2
      else if (nlo_t == NLO_REAL) then
         if (.not. use_color .and. term%i_term_global == term%i_sub) then
            n_sub = n_tot * (n_tot - 1) / 2
         else
            n_sub = 0
         end if
      else
         n_sub = 0
      end if
    end subroutine compute_n_sub

    subroutine fill_quantum_numbers ()
      integer :: nn
      if (nlo_t == NLO_VIRTUAL) then
         nn = (n_sub + 1) * n
      else if (nlo_t == NLO_REAL) then
         if (term%i_term_global == term%i_sub) then
            nn = (n_sub + 1) * n
         else
            nn = n
         end if
      else
         nn = n
      end if
      allocate (term%flv (nn), term%col (nn), term%hel (nn))
      allocate (flv (n_tot), col (n_tot), hel (n_tot))
      allocate (qn (n_tot))
    end subroutine fill_quantum_numbers

    subroutine setup_states_openloops ()
      integer :: s, f, c, h, i
      i = 0
      associate (data => term%data)
         do s = 0, n_sub
             do f = 1, data%n_flv
                do h = 1, data%n_hel
                   do c = 1, data%n_col
                      i = i + 1
                      term%flv(i) = f
                      term%hel(i) = h
                      !!! Dummy-initialization of color
                      term%col(i) = c
                      call flv%init (data%flv_state (:,f), model)
                      call color_init_from_array (col, &
                           data%col_state(:,:,c), data%ghost_flag(:,c))
                      call col(1:data%n_in)%invert ()
                      if (is_pol) then
                         call hel%init (data%hel_state (:,h))
                         call qn%init (flv, hel, col, s)
                      else
                         call qn%init (flv, col, s)
                      end if
                      call term%int%add_state (qn)
                  end do
               end do
             end do
         end do
      end associate
    end subroutine setup_states_openloops

    subroutine setup_states_threshold ()
      integer :: s, f, c, h, i
      if (is_pol) &
           call msg_fatal ("Polarized beams only supported by OpenLoops")
      i = 0
      n_sub = 0; if (nlo_t == NLO_VIRTUAL) n_sub = 1
      associate (data => term%data)
         do s = 0, n_sub
            do f = 1, term%data%n_flv
               do h = 1, data%n_hel
                  do c = 1, data%n_col
                     i = i + 1
                     term%flv(i) = f
                     !!! Dumy initialization of helicity
                     term%hel(i) = 1
                     !!! Dummy-initialization of color
                     term%col(i) = 1
                     call flv%init (term%data%flv_state (:,f), model)
                     call qn%init (flv, s)
                     call term%int%add_state (qn)
                  end do
               end do
            end do
         end do
      end associate
    end subroutine setup_states_threshold

    subroutine setup_states_other_user_defined ()
      integer :: s, f, i
      integer :: n_sub
      if (is_pol) &
         call msg_fatal ("Polarized beams only supported by OpenLoops")
      i = 0
      n_sub = 0; if (nlo_t == NLO_VIRTUAL) n_sub = 1
      do s = 0, n_sub
         do f = 1, term%data%n_flv
            i = i + 1
            term%flv(i) = f
            term%hel(i) = 1
            term%col(i) = 1
            call flv%init (term%data%flv_state (:,f), model)
            call qn%init (flv, s)
            call term%int%add_state (qn)
         end do
      end do
    end subroutine setup_states_other_user_defined

    subroutine setup_states_omega ()
      integer :: f, h, c, i
      i = 0
      associate (data => term%data)
         do f = 1, data%n_flv
            do h = 1, data%n_hel
              do c = 1, data%n_col
                 if (core%is_allowed (term%i_term, f, h, c)) then
                    i = i + 1
                    term%flv(i) = f
                    term%hel(i) = h
                    term%col(i) = c
                    call flv%init (data%flv_state(:,f), model)
                    call color_init_from_array (col, &
                         data%col_state(:,:,c), &
                         data%ghost_flag(:,c))
                    call col(:data%n_in)%invert ()
                    call hel%init (data%hel_state(:,h))
                    call qn%init (flv, col, hel)
                    call term%int%add_state (qn)
                 end if
              end do
            end do
         end do
      end associate
    end subroutine setup_states_omega

  end subroutine process_term_setup_interaction

   subroutine process_term_get_process_constants &
       (term, prc_constants)
    class(process_term_t), intent(inout) :: term
    type(process_constants_t), intent(out) :: prc_constants
    prc_constants = term%data
  end subroutine process_term_get_process_constants


end module process_config
