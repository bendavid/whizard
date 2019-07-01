! WHIZARD 2.2.4 Feb 06 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module process_libraries

  use iso_c_binding !NODEP!
  use kinds
  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use diagnostics
  use md5
  use os_interface
  use lexers
  use model_data
  use particle_specifiers
  use process_constants
  use prclib_interfaces
  use prc_core_def

  implicit none
  private

  public :: process_component_def_t
  public :: process_def_t
  public :: process_def_entry_t
  public :: process_library_t
  public :: process_libraries_test

  integer, parameter :: STAT_UNKNOWN = 0
  integer, parameter :: STAT_OPEN = 1
  integer, parameter :: STAT_CONFIGURED = 2
  integer, parameter :: STAT_SOURCE = 3
  integer, parameter :: STAT_COMPILED = 4
  integer, parameter :: STAT_LINKED = 5
  integer, parameter :: STAT_ACTIVE = 6

  character, dimension(0:6), parameter :: STATUS_LETTER = &
       ["?", "o", "f", "s", "c", "l", "a"]


  type :: process_component_def_t
     private
     type(string_t) :: basename
     logical :: initial = .false.
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     type(prt_spec_t), dimension(:), allocatable :: prt_in
     type(prt_spec_t), dimension(:), allocatable :: prt_out
     type(string_t) :: method
     type(string_t) :: description
     class(prc_core_def_t), allocatable :: core_def
     character(32) :: md5sum = ""
     type(string_t) :: nlo_type
     integer :: associated_born = 0
     integer :: associated_real = 0
     integer :: associated_virt = 0
     integer :: associated_sub = 0
     logical :: active_nlo_component
   contains
     procedure :: write => process_component_def_write
     procedure :: read => process_component_def_read
     procedure :: show => process_component_def_show
     procedure :: compute_md5sum => process_component_def_compute_md5sum
     procedure :: allocate_driver => process_component_def_allocate_driver
     procedure :: needs_code => process_component_def_needs_code
     procedure :: get_writer_ptr => process_component_def_get_writer_ptr
     procedure :: get_features => process_component_def_get_features
     procedure :: connect => process_component_def_connect
     procedure :: get_core_def_ptr => process_component_get_core_def_ptr
     procedure :: get_n_in  => process_component_def_get_n_in
     procedure :: get_n_out => process_component_def_get_n_out
     procedure :: get_n_tot => process_component_def_get_n_tot
     procedure :: get_prt_in => process_component_def_get_prt_in
     procedure :: get_prt_out => process_component_def_get_prt_out
     procedure :: get_md5sum => process_component_def_get_md5sum
     procedure :: get_nlo_type => process_component_def_get_nlo_type
     procedure :: get_associated_born &
                  => process_component_def_get_associated_born
     procedure :: get_association_list &
                  => process_component_def_get_association_list
     procedure :: is_active_nlo_component &
                  => process_component_def_is_active_nlo_component
  end type process_component_def_t
  
  type :: process_def_t
     private
     type(string_t) :: id
     integer :: num_id = 0
     class(model_data_t), pointer :: model => null ()
     type(string_t) :: model_name
     integer :: n_in  = 0
     integer :: n_initial = 0
     integer :: n_extra = 0
     type(process_component_def_t), dimension(:), allocatable :: initial
     type(process_component_def_t), dimension(:), allocatable :: extra
     character(32) :: md5sum = ""
     logical :: nlo_process = .false.
     logical :: combined_nlo_integration = .false.
   contains
     procedure :: write => process_def_write
     procedure :: read => process_def_read
     procedure :: show => process_def_show
     procedure :: init => process_def_init
     procedure :: import_component => process_def_import_component
     procedure :: get_n_components => process_def_get_n_components
     procedure :: set_associated_components => &
                      process_def_set_associated_components
     procedure :: compute_md5sum => process_def_compute_md5sum
     procedure :: get_md5sum => process_def_get_md5sum
     procedure :: needs_code => process_def_needs_code
  end type process_def_t

  type, extends (process_def_t) :: process_def_entry_t
     private
     type(process_def_entry_t), pointer :: next => null ()
  end type process_def_entry_t
  
  type :: process_def_list_t
     private
     type(process_def_entry_t), pointer :: first => null ()
     type(process_def_entry_t), pointer :: last => null ()
   contains
     procedure :: final => process_def_list_final
     procedure :: write => process_def_list_write
     procedure :: show => process_def_list_show
     procedure :: read => process_def_list_read
     procedure :: append => process_def_list_append
     procedure :: get_n_processes => process_def_list_get_n_processes
     procedure :: get_process_id_list => process_def_list_get_process_id_list
     procedure :: contains => process_def_list_contains
     procedure :: get_entry_index => process_def_list_get_entry_index
     procedure :: get_num_id => process_def_list_get_num_id
     procedure :: get_model_name => process_def_list_get_model_name
     procedure :: get_n_in => process_def_list_get_n_in
     procedure :: get_n_components => process_def_list_get_n_components
     procedure :: get_component_def_ptr => process_def_list_get_component_def_ptr
     procedure :: get_component_list => process_def_list_get_component_list
     procedure :: get_component_description_list => &
          process_def_list_get_component_description_list
     procedure :: get_nlo_process => process_def_list_get_nlo_process
  end type process_def_list_t

  type :: process_library_entry_t
     private
     integer :: status = STAT_UNKNOWN
     type(process_def_t), pointer :: def => null ()
     integer :: i_component = 0
     integer :: i_external = 0
     class(prc_core_driver_t), allocatable :: driver
   contains
     procedure :: to_string => process_library_entry_to_string
     procedure :: init => process_library_entry_init
     procedure :: connect => process_library_entry_connect
     procedure :: fill_constants => process_library_entry_fill_constants
  end type process_library_entry_t

  type, extends (process_def_list_t) :: process_library_t
     private
     type(string_t) :: basename
     integer :: n_entries = 0
     logical :: external = .false.
     integer :: status = STAT_UNKNOWN
     logical :: static = .false.
     logical :: driver_exists = .false.
     logical :: makefile_exists = .false.
     type(process_library_entry_t), dimension(:), allocatable :: entry
     class(prclib_driver_t), allocatable :: driver
     character(32) :: md5sum = ""
   contains
     procedure :: write => process_library_write
     procedure :: show => process_library_show
     procedure :: init => process_library_init
     procedure :: init_static => process_library_init_static
     procedure :: configure => process_library_configure
     procedure :: compute_md5sum => process_library_compute_md5sum
     procedure :: write_makefile => process_library_write_makefile
     procedure :: write_driver => process_library_write_driver
     procedure :: update_status => process_library_update_status
     procedure :: make_source => process_library_make_source
     procedure :: make_compile => process_library_make_compile
     procedure :: make_link => process_library_make_link
     procedure :: load => process_library_load
     procedure :: load_entries => process_library_load_entries
     procedure :: unload => process_library_unload
     procedure :: clean => process_library_clean
     procedure :: open => process_library_open
     procedure :: get_name => process_library_get_name
     procedure :: is_active => process_library_is_active
     procedure :: connect_process => process_library_connect_process
     procedure :: get_modellibs_ldflags => process_library_get_modellibs_ldflags
     procedure :: get_static_modelname => process_library_get_static_modelname
  end type process_library_t

  type, extends (process_driver_internal_t) :: prctest_2_t
   contains
     procedure, nopass :: type_name => prctest_2_type_name
     procedure :: fill_constants => prctest_2_fill_constants
  end type prctest_2_t
  
  type, extends (prc_core_driver_t) :: prctest_5_t
   contains
     procedure, nopass :: type_name => prctest_5_type_name
  end type prctest_5_t
  
  type, extends (prc_core_driver_t) :: prctest_6_t
     procedure(proc1_t), nopass, pointer :: proc1 => null ()
   contains
     procedure, nopass :: type_name => prctest_6_type_name
  end type prctest_6_t
  

  abstract interface
     subroutine proc1_t (n) bind(C)
       import
       integer(c_int), intent(out) :: n
     end subroutine proc1_t
  end interface


  type, extends (prc_core_def_t) :: prcdef_2_t
     integer :: data = 0
     logical :: file = .false.
   contains
     procedure, nopass :: type_string => prcdef_2_type_string
     procedure :: write => prcdef_2_write
     procedure :: read => prcdef_2_read
     procedure, nopass :: get_features => prcdef_2_get_features
     procedure :: generate_code => prcdef_2_generate_code
     procedure :: allocate_driver => prcdef_2_allocate_driver
     procedure :: connect => prcdef_2_connect
  end type prcdef_2_t
  
  type, extends (prc_core_def_t) :: prcdef_5_t
   contains
     procedure, nopass :: type_string => prcdef_5_type_string
     procedure :: init => prcdef_5_init
     procedure :: write => prcdef_5_write
     procedure :: read => prcdef_5_read
     procedure :: allocate_driver => prcdef_5_allocate_driver
     procedure, nopass :: needs_code => prcdef_5_needs_code
     procedure, nopass :: get_features => prcdef_5_get_features
     procedure :: connect => prcdef_5_connect
  end type prcdef_5_t
  
  type, extends (prc_core_def_t) :: prcdef_6_t
   contains
     procedure, nopass :: type_string => prcdef_6_type_string
     procedure :: init => prcdef_6_init
     procedure :: write => prcdef_6_write
     procedure :: read => prcdef_6_read
     procedure :: allocate_driver => prcdef_6_allocate_driver
     procedure, nopass :: needs_code => prcdef_6_needs_code
     procedure, nopass :: get_features => prcdef_6_get_features
     procedure :: connect => prcdef_6_connect
  end type prcdef_6_t
  

contains

  subroutine strip_prefix (buffer)
    character(*), intent(inout) :: buffer
    type(string_t) :: string, prefix
    string = buffer
    call split (string, prefix, "=")
    buffer = string
  end subroutine strip_prefix
    
  subroutine process_component_def_write (object, unit)
    class(process_component_def_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A,A)")  "Component ID        = ", char (object%basename)
    write (u, "(3x,A,L1)") "Initial component   = ", object%initial
    write (u, "(3x,A,I0,1x,I0,1x,I0)") "N (in, out, tot)    = ", &
         object%n_in, object%n_out, object%n_tot
    write (u, "(3x,A)", advance="no") "Particle content    = "
    if (allocated (object%prt_in)) then
       call prt_spec_write (object%prt_in, u, advance="no")
    else
       write (u, "(A)", advance="no")  "[undefined]"
    end if
    write (u, "(A)", advance="no") " => "
    if (allocated (object%prt_out)) then
       call prt_spec_write (object%prt_out, u, advance="no")
    else
       write (u, "(A)", advance="no")  "[undefined]"
    end if
    write (u, "(A)")
    if (object%method /= "") then
       write (u, "(3x,A,A)")  "Method              = ", &
            char (object%method)
    else
       write (u, "(3x,A)")  "Method              = [undefined]"
    end if
    if (allocated (object%core_def)) then
       write (u, "(3x,A,A)")  "Process variant     = ", &
            char (object%core_def%type_string ())
       call object%core_def%write (u)
    else
       write (u, "(3x,A)")  "Process variant     = [undefined]"
    end if
    write (u, "(3x,A,A,A)") "MD5 sum (def)       = '", object%md5sum, "'"
  end subroutine process_component_def_write
    
  subroutine process_component_def_read (component, unit, core_def_templates)
    class(process_component_def_t), intent(out) :: component
    integer, intent(in) :: unit
    type(prc_template_t), dimension(:), intent(in) :: core_def_templates
    character(80) :: buffer
    type(string_t) :: var_buffer, prefix, in_state, out_state
    type(string_t) :: variant_type
    
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    component%basename = trim (adjustl (buffer))
    
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer, *)  component%initial
    
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer, *)  component%n_in, component%n_out, component%n_tot
    
    call get (unit, var_buffer)
    call split (var_buffer, prefix, "=")   ! keeps 'in => out'
    call split (var_buffer, prefix, "=")   ! actually: separator is '=>'

    in_state = prefix
    if (component%n_in > 0) then
       call prt_spec_read (component%prt_in, in_state)
    end if

    out_state = extract (var_buffer, 2)
    if (component%n_out > 0) then
       call prt_spec_read (component%prt_out, out_state)
    end if
    
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    component%method = trim (adjustl (buffer))
    if (component%method == "[undefined]") &
         component%method = ""
    
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    variant_type = trim (adjustl (buffer))
    call allocate_core_def &
         (core_def_templates, variant_type, component%core_def)
    if (allocated (component%core_def)) then
       call component%core_def%read (unit)
    end if

    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer(3:34), "(A32)")  component%md5sum
    
  end subroutine process_component_def_read
    
  subroutine process_component_def_show (object, unit)
    class(process_component_def_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(6x,A)", advance="no")  char (object%basename)
    if (.not. object%initial) &
         write (u, "('*')", advance="no")
    write (u, "(':',1x)", advance="no")
    if (allocated (object%prt_in)) then
       call prt_spec_write (object%prt_in, u, advance="no")
    else
       write (u, "(A)", advance="no")  "[undefined]"
    end if
    write (u, "(A)", advance="no") " => "
    if (allocated (object%prt_out)) then
       call prt_spec_write (object%prt_out, u, advance="no")
    else
       write (u, "(A)", advance="no")  "[undefined]"
    end if
    if (object%method /= "") then
       write (u, "(2x,'[',A,']')")  char (object%method)
    else
       write (u, *)
    end if
  end subroutine process_component_def_show
    
  subroutine process_component_def_compute_md5sum (component, model)
    class(process_component_def_t), intent(inout) :: component
    class(model_data_t), intent(in), optional, target :: model
    integer :: u
    component%md5sum = ""
    u = free_unit ()
    open (u, status = "scratch", action = "readwrite")
    if (present (model))  write (u, "(A32)")  model%get_md5sum ()
    call component%write (u)
    rewind (u)
    component%md5sum = md5sum (u)
    close (u)
    if (allocated (component%core_def)) then
       call component%core_def%set_md5sum (component%md5sum)
    end if
  end subroutine process_component_def_compute_md5sum

  subroutine process_component_def_allocate_driver (component, driver)
    class(process_component_def_t), intent(in) :: component
    class(prc_core_driver_t), intent(out), allocatable :: driver
    if (allocated (component%core_def)) then
       call component%core_def%allocate_driver (driver, component%basename)
    end if
  end subroutine process_component_def_allocate_driver
    
  function process_component_def_needs_code (component) result (flag)
    class(process_component_def_t), intent(in) :: component
    logical :: flag
    flag = component%core_def%needs_code ()
  end function process_component_def_needs_code
  
  function process_component_def_get_writer_ptr (component) result (writer)
    class(process_component_def_t), intent(in), target :: component
    class(prc_writer_t), pointer :: writer
    writer => component%core_def%writer
  end function process_component_def_get_writer_ptr
  
  function process_component_def_get_features (component) result (features)
    class(process_component_def_t), intent(in) :: component
    type(string_t), dimension(:), allocatable :: features
    call component%core_def%get_features (features)
  end function process_component_def_get_features

  subroutine process_component_def_connect &
       (component, lib_driver, i, proc_driver)
    class(process_component_def_t), intent(in) :: component
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
    select type (proc_driver)
    class is (process_driver_internal_t)
       ! nothing to do
    class default
       call component%core_def%connect (lib_driver, i, proc_driver)
    end select
  end subroutine process_component_def_connect

  function process_component_get_core_def_ptr (component) result (ptr)
    class(process_component_def_t), intent(in), target :: component
    class(prc_core_def_t), pointer :: ptr
    ptr => component%core_def
  end function process_component_get_core_def_ptr
  
  function process_component_def_get_n_in (component) result (n_in)
    class(process_component_def_t), intent(in) :: component
    integer :: n_in
    n_in = component%n_in
  end function process_component_def_get_n_in
  
  function process_component_def_get_n_out (component) result (n_out)
    class(process_component_def_t), intent(in) :: component
    integer :: n_out
    n_out = component%n_out
  end function process_component_def_get_n_out
  
  function process_component_def_get_n_tot (component) result (n_tot)
    class(process_component_def_t), intent(in) :: component
    integer :: n_tot
    n_tot = component%n_tot
  end function process_component_def_get_n_tot
  
  subroutine process_component_def_get_prt_in (component, prt)
    class(process_component_def_t), intent(in) :: component
    type(string_t), dimension(:), intent(out), allocatable :: prt
    integer :: i
    allocate (prt (component%n_in))
    do i = 1, component%n_in
       prt(i) = component%prt_in(i)%to_string ()
    end do
  end subroutine process_component_def_get_prt_in
  
  subroutine process_component_def_get_prt_out (component, prt)
    class(process_component_def_t), intent(in) :: component
    type(string_t), dimension(:), intent(out), allocatable :: prt
    integer :: i
    allocate (prt (component%n_out))
    do i = 1, component%n_out
       prt(i) = component%prt_out(i)%to_string ()
    end do
  end subroutine process_component_def_get_prt_out
  
  function process_component_def_get_md5sum (component) result (md5sum)
    class(process_component_def_t), intent(in) :: component
    character(32) :: md5sum
    md5sum = component%md5sum
  end function process_component_def_get_md5sum
  
  function process_component_def_get_nlo_type (component) result (nlo_type)
    class(process_component_def_t), intent(in) :: component
    type(string_t) :: nlo_type
    if (component%nlo_type == "") then
      nlo_type = 'Born'
    else
      nlo_type = component%nlo_type
    end if
  end function process_component_def_get_nlo_type

  function process_component_def_get_associated_born (component) result (i_born)
    class(process_component_def_t), intent(in) :: component
    integer :: i_born
    i_born = component%associated_born
  end function process_component_def_get_associated_born

  function process_component_def_is_active_nlo_component (component) result (active)
    class(process_component_def_t), intent(in) :: component
    logical :: active
    active = component%active_nlo_component
  end function process_component_def_is_active_nlo_component

  function process_component_def_get_association_list (component) result (list)
    class(process_component_def_t), intent(in) :: component
    integer, dimension(4) :: list
    list(1) = component%associated_born
    list(2) = component%associated_real
    list(3) = component%associated_virt
    list(4) = component%associated_sub
  end function process_component_def_get_association_list

  subroutine process_def_write (object, unit)
    class(process_def_t), intent(in) :: object
    integer, intent(in) :: unit
    integer :: i
    write (unit, "(1x,A,A,A)") "ID = '", char (object%id), "'"
    if (object%num_id /= 0) &
         write (unit, "(1x,A,I0)")  "ID(num) = ", object%num_id
    select case (object%n_in)
    case (1);  write (unit, "(1x,A)")  "Decay"
    case (2);  write (unit, "(1x,A)")  "Scattering"
    case default
       write (unit, "(1x,A)")  "[Undefined process]"
       return
    end select
    if (object%model_name /= "") then
       write (unit, "(1x,A,A)")  "Model = ", char (object%model_name)
    else
       write (unit, "(1x,A)")  "Model = [undefined]"
    end if
    write (unit, "(1x,A,I0)")  "Initially defined component(s) = ", &
         object%n_initial
    write (unit, "(1x,A,I0)")  "Extra generated component(s)   = ", &
         object%n_extra
    write (unit, "(1x,A,A,A)") "MD5 sum   = '", object%md5sum, "'"
    if (allocated (object%initial)) then
       do i = 1, size (object%initial)
          write (unit, "(1x,A,I0)")  "Component #", i
          call object%initial(i)%write (unit)
       end do
    end if
    if (allocated (object%extra)) then
       do i = 1, size (object%extra)
          write (unit, "(1x,A,I0)")  "Component #", object%n_initial + i
          call object%extra(i)%write (unit)
       end do
    end if
  end subroutine process_def_write
    
  subroutine process_def_read (object, unit, core_def_templates)
    class(process_def_t), intent(out) :: object
    integer, intent(in) :: unit
    type(prc_template_t), dimension(:), intent(in) :: core_def_templates
    integer :: i, i1, i2
    character(80) :: buffer, ref
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    i1 = scan (buffer, "'")
    i2 = scan (buffer, "'", back=.true.)
    if (i2 > i1) then
       object%id = buffer(i1+1:i2-1)
    else
       object%id = ""
    end if

    read (unit, "(A)")  buffer
    select case (buffer(2:11))
    case ("Decay     "); object%n_in = 1
    case ("Scattering"); object%n_in = 2
    case default
       return
    end select

    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    object%model_name = trim (adjustl (buffer))
    if (object%model_name == "[undefined]")  object%model_name = ""
          
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer, *)  object%n_initial

    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer, *)  object%n_extra

    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer(3:34), "(A32)")  object%md5sum
    
    if (object%n_initial > 0) then
       allocate (object%initial (object%n_initial))
       do i = 1, object%n_initial
          read (unit, "(A)")  buffer
          write (ref, "(1x,A,I0)")  "Component #", i
          if (buffer /= ref)  return                ! Wrong component header
          call object%initial(i)%read (unit, core_def_templates)
       end do
    end if
    
  end subroutine process_def_read
    
  subroutine process_def_show (object, unit)
    class(process_def_t), intent(in) :: object
    integer, intent(in) :: unit
    integer :: i
    write (unit, "(4x,A)", advance="no") char (object%id)
    if (object%num_id /= 0) &
         write (unit, "(1x,'(',I0,')')", advance="no")  object%num_id
    if (object%model_name /= "") &
         write (unit, "(1x,'[',A,']')")  char (object%model_name)
    if (allocated (object%initial)) then
       do i = 1, size (object%initial)
          call object%initial(i)%show (unit)
       end do
    end if
    if (allocated (object%extra)) then
       do i = 1, size (object%extra)
          call object%extra(i)%show (unit)
       end do
    end if
  end subroutine process_def_show
    
  subroutine process_def_init (def, id, &
       model, model_name, n_in, n_components, num_id, nlo_process)
    class(process_def_t), intent(out) :: def
    type(string_t), intent(in), optional :: id
    class(model_data_t), intent(in), optional, target :: model
    type(string_t), intent(in), optional :: model_name
    integer, intent(in), optional :: n_in
    integer, intent(in), optional :: n_components
    integer, intent(in), optional :: num_id
    logical, intent(in), optional :: nlo_process
    character(16) :: suffix
    integer :: i
    if (present (id)) then
       def%id = id
    else
       def%id = ""
    end if
    if (present (num_id)) then
       def%num_id = num_id
    end if
    if (present (model)) then
       def%model => model
       def%model_name = model%get_name ()
    else
       def%model => null ()
       if (present (model_name)) then
          def%model_name = model_name
       else
          def%model_name = ""
       end if
    end if
    if (present (n_in))  def%n_in = n_in
    if (present (n_components)) then
       def%n_initial = n_components
       allocate (def%initial (n_components))
    end if
    if (present (nlo_process)) def%nlo_process = nlo_process
    def%initial%initial = .true.
    def%initial%method     = ""
    do i = 1, def%n_initial
       write (suffix, "(A,I0)")  "_i", i
       def%initial(i)%basename = def%id // trim (suffix)
    end do
    def%initial%description = ""
  end subroutine process_def_init
  
  subroutine process_def_import_component (def, &
       i, n_out, prt_in, prt_out, method, variant, &
       nlo_type, active)
    class(process_def_t), intent(inout) :: def
    integer, intent(in) :: i
    integer, intent(in), optional :: n_out
    type(prt_spec_t), dimension(:), intent(in), optional :: prt_in
    type(prt_spec_t), dimension(:), intent(in), optional :: prt_out
    type(string_t), intent(in), optional :: method
    type(string_t), intent(in), optional :: nlo_type
    logical, intent(in), optional :: active
    class(prc_core_def_t), &
         intent(inout), allocatable, optional :: variant
    integer :: p
    associate (comp => def%initial(i))
      if (present (n_out)) then
         comp%n_in  = def%n_in
         comp%n_out = n_out
         comp%n_tot = def%n_in + n_out
      end if
      if (present (prt_in)) then
         allocate (comp%prt_in (size (prt_in)))
         comp%prt_in = prt_in
      end if
      if (present (prt_out)) then
         allocate (comp%prt_out (size (prt_out)))
         comp%prt_out = prt_out
      end if
      if (present (method))  comp%method = method
      if (present (variant)) then
         call move_alloc (variant, comp%core_def)
      end if
      if (present (nlo_type)) then
        comp%nlo_type = nlo_type
      end if
      if (present (active)) then
         comp%active_nlo_component = active
      else
         comp%active_nlo_component = .true.
      end if
      if (allocated (comp%prt_in) .and. allocated (comp%prt_out)) then
         associate (d => comp%description)
           d = ""
           do p = 1, size (prt_in)
              if (p > 1)  d = d // ", "
              d = d // comp%prt_in(p)%to_string ()
           end do
           d = d // " => "
           do p = 1, size (prt_out)
              if (p > 1)  d = d // ", "
              d = d // comp%prt_out(p)%to_string ()
           end do
           if (comp%method /= "") then
              d = d // " [" // comp%method // "]"
           end if
           if (comp%nlo_type /= "") then
             d = d // ", [" // comp%nlo_type // "]"
           end if
         end associate
      end if
    end associate
  end subroutine process_def_import_component

  function process_def_get_n_components (def) result (n)
    class(process_def_t), intent(in) :: def
    integer :: n
    n = size (def%initial)
  end function process_def_get_n_components

  subroutine process_def_set_associated_components (def, i, &
                     i_born, i_real, i_virt, i_sub)
    class(process_def_t), intent(inout) :: def
    integer, intent(in) :: i
    integer, intent(in) :: i_born, i_real
    integer, intent(in) :: i_virt, i_sub
    associate (comp => def%initial(i))
       comp%associated_born = i_born
       comp%associated_real = i_real
       comp%associated_virt = i_virt
       comp%associated_sub = i_sub
    end associate
  end subroutine process_def_set_associated_components

  subroutine process_def_compute_md5sum (def, model)
    class(process_def_t), intent(inout) :: def
    class(model_data_t), intent(in), optional, target :: model
    integer :: i
    type(string_t) :: buffer
    buffer = def%model_name
    do i = 1, def%n_initial
       call def%initial(i)%compute_md5sum (model)
       buffer = buffer // def%initial(i)%md5sum
    end do
    do i = 1, def%n_extra
       call def%extra(i)%compute_md5sum (model)
       buffer = buffer // def%initial(i)%md5sum
    end do
    def%md5sum = md5sum (char (buffer))
  end subroutine process_def_compute_md5sum
    
  function process_def_get_md5sum (def, i_component) result (md5sum)
    class(process_def_t), intent(in) :: def
    integer, intent(in), optional :: i_component
    character(32) :: md5sum
    if (present (i_component)) then
       md5sum = def%initial(i_component)%md5sum
    else
       md5sum = def%md5sum
    end if
  end function process_def_get_md5sum
  
  function process_def_needs_code (def, i_component) result (flag)
    class(process_def_t), intent(in) :: def
    integer, intent(in) :: i_component
    logical :: flag
    flag = def%initial(i_component)%needs_code ()
  end function process_def_needs_code
  
  subroutine process_def_list_final (list)
    class(process_def_list_t), intent(inout) :: list
    type(process_def_entry_t), pointer :: current
    nullify (list%last)
    do while (associated (list%first))
       current => list%first
       list%first => current%next
       deallocate (current)
    end do
  end subroutine process_def_list_final
  
  subroutine process_def_list_write (object, unit, libpath)
    class(process_def_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: libpath
    type(process_def_entry_t), pointer :: entry
    integer :: i, u
    u = given_output_unit (unit)
    if (associated (object%first)) then
       i = 1
       entry => object%first
       do while (associated (entry))
          write (u, "(1x,A,I0,A)")  "Process #", i, ":"
          call entry%write (u)
          i = i + 1
          entry => entry%next
          if (associated (entry))  write (u, *)
       end do
    else
       write (u, "(1x,A)")  "Process definition list: [empty]"
    end if
  end subroutine process_def_list_write

  subroutine process_def_list_show (object, unit)
    class(process_def_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(process_def_entry_t), pointer :: entry
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%first)) then
       write (u, "(2x,A)")  "Processes:"
       entry => object%first
       do while (associated (entry))
          call entry%show (u)
          entry => entry%next
       end do
    else
       write (u, "(2x,A)")  "Processes: [empty]"
    end if
  end subroutine process_def_list_show

  subroutine process_def_list_read (object, unit, core_def_templates)
    class(process_def_list_t), intent(out) :: object
    integer, intent(in) :: unit
    type(prc_template_t), dimension(:), intent(in) :: core_def_templates
    type(process_def_entry_t), pointer :: entry
    character(80) :: buffer, ref
    integer :: i
    read (unit, "(A)")  buffer
    write (ref, "(1x,A)")  "Process definition list: [empty]"  
    if (buffer == ref)  return         ! OK: empty library
    backspace (unit)
    READ_ENTRIES: do i = 1, huge (0)
       if (i > 1) read (unit, *, end=1)
       read (unit, "(A)")  buffer

       write (ref, "(1x,A,I0,A)")  "Process #", i, ":"
       if (buffer /= ref)  return      ! Wrong process header: done.
       allocate (entry)
       call entry%read (unit, core_def_templates)
       call object%append (entry)
    end do READ_ENTRIES
1   continue                           ! EOF: done
  end subroutine process_def_list_read

  subroutine process_def_list_append (list, entry)
    class(process_def_list_t), intent(inout) :: list
    type(process_def_entry_t), intent(inout), pointer :: entry
    if (list%contains (entry%id)) then
       call msg_fatal ("Recording process: '" // char (entry%id) &
            // "' has already been defined")
    end if
    if (associated (list%first)) then
       list%last%next => entry
    else
       list%first => entry
    end if
    list%last => entry
    entry => null ()
  end subroutine process_def_list_append
  
  function process_def_list_get_n_processes (list) result (n)
    integer :: n
    class(process_def_list_t), intent(in) :: list
    type(process_def_entry_t), pointer :: current
    n = 0
    current => list%first
    do while (associated (current))
       n = n + 1
       current => current%next
    end do
  end function process_def_list_get_n_processes
  
  subroutine process_def_list_get_process_id_list (list, id)
    class(process_def_list_t), intent(in) :: list
    type(string_t), dimension(:), allocatable, intent(out) :: id
    type(process_def_entry_t), pointer :: current
    integer :: i
    allocate (id (list%get_n_processes ()))
    i = 0
    current => list%first
    do while (associated (current))
       i = i + 1
       id(i) = current%id
       current => current%next
    end do
  end subroutine process_def_list_get_process_id_list
  
  function process_def_list_contains (list, id) result (flag)
    logical :: flag
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          flag = .true.;  return
       end if
       current => current%next
    end do
    flag = .false.
  end function process_def_list_contains

  function process_def_list_get_entry_index (list, id) result (n)
    integer :: n
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    n = 0
    current => list%first
    do while (associated (current))
       n = n + 1
       if (id == current%id) then
          return
       end if
       current => current%next
    end do
    n = 0
  end function process_def_list_get_entry_index
    
  function process_def_list_get_num_id (list, id) result (num_id)
    integer :: num_id
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          num_id = current%num_id
          return
       end if
       current => current%next
    end do
    num_id = 0
  end function process_def_list_get_num_id
    
  function process_def_list_get_model_name (list, id) result (model_name)
    type(string_t) :: model_name
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          model_name = current%model_name
          return
       end if
       current => current%next
    end do
    model_name = ""
  end function process_def_list_get_model_name
  
  function process_def_list_get_n_in (list, id) result (n)
    integer :: n
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          n = current%n_in
          return
       end if
       current => current%next
    end do
  end function process_def_list_get_n_in
  
  function process_def_list_get_n_components (list, id) result (n)
    integer :: n
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          n = current%n_initial + current%n_extra
          return
       end if
       current => current%next
    end do
  end function process_def_list_get_n_components
  
  function process_def_list_get_component_def_ptr (list, id, i) result (ptr)
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    integer, intent(in) :: i
    type(process_component_def_t), pointer :: ptr
    type(process_def_entry_t), pointer :: current
    ptr => null ()
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          if (i <= current%n_initial) then
             ptr => current%initial(i)
          else if (i <= current%n_initial + current%n_extra) then
             ptr => current%extra(i-current%n_initial)
          end if
          return
       end if
       current => current%next
    end do
  end function process_def_list_get_component_def_ptr
  
  subroutine process_def_list_get_component_list (list, id, cid)
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(string_t), dimension(:), allocatable, intent(out) :: cid
    type(process_def_entry_t), pointer :: current
    integer :: i, n
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          allocate (cid (current%n_initial + current%n_extra))
          do i = 1, current%n_initial
             cid(i) = current%initial(i)%basename
          end do
          n = current%n_initial
          do i = 1, current%n_extra
             cid(n + i) = current%extra(i)%basename
          end do
          return
       end if
       current => current%next
    end do
  end subroutine process_def_list_get_component_list
  
  subroutine process_def_list_get_component_description_list &
       (list, id, description)
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    type(string_t), dimension(:), allocatable, intent(out) :: description
    type(process_def_entry_t), pointer :: current
    integer :: i, n
    current => list%first
    do while (associated (current))
       if (id == current%id) then
          allocate (description (current%n_initial + current%n_extra))
          do i = 1, current%n_initial
             description(i) = current%initial(i)%description
          end do
          n = current%n_initial
          do i = 1, current%n_extra
             description(n + i) = current%extra(i)%description
          end do
          return
       end if
       current => current%next
    end do
  end subroutine process_def_list_get_component_description_list
  
  function process_def_list_get_nlo_process (list, id) result (nlo)
    class(process_def_list_t), intent(in) :: list
    type(string_t), intent(in) :: id
    logical :: nlo
    type(process_def_entry_t), pointer :: current
    current => list%first
    do while (associated (current))
      if (id == current%id) then
        nlo = current%nlo_process
        return
      end if
      current => current%next
    end do
  end function process_def_list_get_nlo_process

  function process_library_entry_to_string (object) result (string)
    type(string_t) :: string
    class(process_library_entry_t), intent(in) :: object
    character(32) :: buffer
    string = "[" // STATUS_LETTER(object%status) // "]"
    select case (object%status)
    case (STAT_UNKNOWN)
    case default
       if (associated (object%def)) then
          write (buffer, "(I0)")  object%i_component
          string = string // " " // object%def%id // "." // trim (buffer)
       end if
       if (object%i_external /= 0) then
          write (buffer, "(I0)")  object%i_external
          string = string // " = ext:" // trim (buffer)
       else
          string = string // " = int"
       end if
       if (allocated (object%driver)) then
          string = string // " (" // object%driver%type_name () // ")"
       end if
    end select
  end function process_library_entry_to_string
  
  subroutine process_library_entry_init (object, &
       status, def, i_component, i_external)
    class(process_library_entry_t), intent(out) :: object
    integer, intent(in) :: status
    type(process_def_t), target, intent(in) :: def
    integer, intent(in) :: i_component
    integer, intent(in) :: i_external
    object%status = status
    object%def => def
    object%i_component = i_component
    object%i_external = i_external
  end subroutine process_library_entry_init
  
  subroutine process_library_entry_connect (entry, lib_driver, i)
    class(process_library_entry_t), intent(inout) :: entry
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    call entry%def%initial(entry%i_component)%connect &
         (lib_driver, i, entry%driver)
  end subroutine process_library_entry_connect

  subroutine process_library_write (object, unit, libpath)
    class(process_library_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: libpath
    integer :: i, u
    u = given_output_unit (unit)
    write (u, "(1x,A,A)")  "Process library: ", char (object%basename)
    write (u, "(3x,A,L1)")   "external        = ", object%external
    write (u, "(3x,A,L1)")   "makefile exists = ", object%makefile_exists
    write (u, "(3x,A,L1)")   "driver exists   = ", object%driver_exists
    write (u, "(3x,A,A1)")   "code status     = ", &
         STATUS_LETTER (object%status)
    write (u, *)
    if (allocated (object%entry)) then
       write (u, "(1x,A)", advance="no")  "Process library entries:"
       write (u, "(1x,I0)")  object%n_entries
       do i = 1, size (object%entry)
          write (u, "(1x,A,I0,A,A)")  "Entry #", i, ": ", &
               char (object%entry(i)%to_string ())
       end do
       write (u, *)
    end if
    if (object%external) then
       call object%driver%write (u, libpath)
       write (u, *)
    end if
    call object%process_def_list_t%write (u)
  end subroutine process_library_write
         
  subroutine process_library_show (object, unit)
    class(process_library_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A,A)")  "Process library: ", char (object%basename)
    write (u, "(2x,A,L1)")   "external        = ", object%external
    if (object%static) then
       write (u, "(2x,A,L1)")   "static          = ", .true.
    else
       write (u, "(2x,A,L1)")   "makefile exists = ", object%makefile_exists
       write (u, "(2x,A,L1)")   "driver exists   = ", object%driver_exists
    end if
    write (u, "(2x,A,A1)", advance="no")   "code status     = "
    select case (object%status)
    case (STAT_UNKNOWN);    write (u, "(A)")  "[unknown]"
    case (STAT_OPEN);       write (u, "(A)")  "open"
    case (STAT_CONFIGURED); write (u, "(A)")  "configured"
    case (STAT_SOURCE);     write (u, "(A)")  "source code exists"
    case (STAT_COMPILED);   write (u, "(A)")  "compiled"
    case (STAT_LINKED);     write (u, "(A)")  "linked"
    case (STAT_ACTIVE);     write (u, "(A)")  "active"
    end select
    call object%process_def_list_t%show (u)
  end subroutine process_library_show
         
  subroutine process_library_init (lib, basename)
    class(process_library_t), intent(out) :: lib
    type(string_t), intent(in) :: basename
    lib%basename = basename
    lib%status = STAT_OPEN
    call msg_message ("Process library '" // char (basename) &
         // "': initialized")
  end subroutine process_library_init
  
  subroutine process_library_init_static (lib, basename)
    class(process_library_t), intent(out) :: lib
    type(string_t), intent(in) :: basename
    lib%basename = basename
    lib%status = STAT_OPEN
    lib%static = .true.
    call msg_message ("Static process library '" // char (basename) &
         // "': initialized")
  end subroutine process_library_init_static
  
  subroutine process_library_configure (lib, os_data)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    type(process_def_entry_t), pointer :: def_entry
    integer :: n_entries, n_external, i_entry, i_external
    type(string_t) :: model_name
    integer :: i_component

    n_entries = 0
    n_external = 0
    if (allocated (lib%entry))  deallocate (lib%entry)
    
    def_entry => lib%first
    do while (associated (def_entry))
       do i_component = 1, def_entry%n_initial
          n_entries = n_entries + 1
          if (def_entry%initial(i_component)%needs_code ()) then
             n_external = n_external + 1
             lib%external = .true.
          end if
       end do
       def_entry => def_entry%next
    end do
    lib%n_entries = n_entries

    allocate (lib%entry (n_entries))
    i_entry = 0
    i_external = 0
    def_entry => lib%first
    do while (associated (def_entry))
       do i_component = 1, def_entry%n_initial
          i_entry = i_entry + 1
          associate (lib_entry => lib%entry(i_entry))
            lib_entry%status = STAT_CONFIGURED
            lib_entry%def => def_entry%process_def_t
            lib_entry%i_component = i_component
            if (def_entry%initial(i_component)%needs_code ()) then
               i_external = i_external + 1
               lib_entry%i_external = i_external
            end if
            call def_entry%initial(i_component)%allocate_driver &
                 (lib_entry%driver)
          end associate
       end do
       def_entry => def_entry%next
    end do

    call dispatch_prclib_driver (lib%driver, &
         lib%basename, lib%get_modellibs_ldflags (os_data))
    call lib%driver%init (n_external)
    do i_entry = 1, n_entries
       associate (lib_entry => lib%entry(i_entry))
         i_component = lib_entry%i_component
         model_name = lib_entry%def%model_name
         associate (def => lib_entry%def%initial(i_component))
           if (def%needs_code ()) then
              call lib%driver%set_record (lib_entry%i_external, &
                   def%basename, &
                   model_name, &
                   def%get_features (), def%get_writer_ptr ())
           end if
         end associate
       end associate
    end do
    
    if (lib%static) then
       if (lib%n_entries /= 0)  lib%entry%status = STAT_LINKED
       lib%status = STAT_LINKED
    else if (lib%external) then
       where (lib%entry%i_external == 0)  lib%entry%status = STAT_LINKED
       lib%status = STAT_CONFIGURED
       lib%makefile_exists = .false.
       lib%driver_exists = .false.
    else
       if (lib%n_entries /= 0)  lib%entry%status = STAT_LINKED
       lib%status = STAT_LINKED
    end if
  end subroutine process_library_configure
  
  subroutine process_library_compute_md5sum (lib, model)
    class(process_library_t), intent(inout) :: lib
    class(model_data_t), intent(in), optional, target :: model
    type(process_def_entry_t), pointer :: def_entry
    type(string_t) :: buffer
    buffer = lib%basename
    def_entry => lib%first
    do while (associated (def_entry))
       call def_entry%compute_md5sum (model)
       buffer = buffer // def_entry%md5sum
       def_entry => def_entry%next
    end do
    lib%md5sum = md5sum (char (buffer))
    call lib%driver%set_md5sum (lib%md5sum)
  end subroutine process_library_compute_md5sum
  
  subroutine process_library_write_makefile (lib, os_data, force, testflag)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: force
    logical, intent(in), optional :: testflag
    character(32) :: md5sum_file
    logical :: generate
    integer :: unit
    if (lib%external .and. .not. lib%static) then
       generate = .true.
       if (.not. force) then
          md5sum_file = lib%driver%get_md5sum_makefile ()
          if (lib%md5sum == md5sum_file) then
             call msg_message ("Process library '" // char (lib%basename) &
                  // "': keeping makefile")
             generate = .false.
          end if
       end if
       if (generate) then
          call msg_message ("Process library '" // char (lib%basename) &
               // "': writing makefile")
          unit = free_unit ()
          open (unit, file = char (lib%driver%basename // ".makefile"), &
               status="replace", action="write")
          call lib%driver%generate_makefile (unit, os_data, testflag)
          close (unit)
       end if
       lib%makefile_exists = .true.
    end if
  end subroutine process_library_write_makefile
  
  subroutine process_library_write_driver (lib, force)
    class(process_library_t), intent(inout) :: lib
    logical, intent(in) :: force
    character(32) :: md5sum_file
    logical :: generate
    integer :: unit
    if (lib%external .and. .not. lib%static) then
       generate = .true.
       if (.not. force) then
          md5sum_file = lib%driver%get_md5sum_driver ()
          if (lib%md5sum == md5sum_file) then
             call msg_message ("Process library '" // char (lib%basename) &
                  // "': keeping driver")
             generate = .false.
          end if
       end if
       if (generate) then
          call msg_message ("Process library '" // char (lib%basename) &
               // "': writing driver")
          unit = free_unit ()
          open (unit, file = char (lib%driver%basename // ".f90"), &
               status="replace", action="write")
          call lib%driver%generate_driver_code (unit)
          close (unit)
       end if
       lib%driver_exists = .true.
    end if
  end subroutine process_library_write_driver

  subroutine process_library_update_status (lib, os_data)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    character(32) :: md5sum_file
    integer :: i, i_external, i_component
    if (lib%external) then
       select case (lib%status)
       case (STAT_CONFIGURED:STAT_LINKED)
          call lib%driver%load (os_data, noerror=.true.)
       end select
       if (lib%driver%loaded) then
          md5sum_file = lib%driver%get_md5sum (0)
          if (lib%md5sum == md5sum_file) then
             call lib%load_entries ()
             lib%entry%status = STAT_ACTIVE
             lib%status = STAT_ACTIVE
             call msg_message ("Process library '" // char (lib%basename) &
                  // "': active")
          else
             do i = 1, lib%n_entries
                associate (entry => lib%entry(i))
                  i_external = entry%i_external
                  i_component = entry%i_component
                  if (i_external /= 0) then
                     md5sum_file = lib%driver%get_md5sum (i_external)
                     if (entry%def%get_md5sum (i_component) == md5sum_file) then
                        entry%status = STAT_COMPILED
                     else
                        entry%status = STAT_CONFIGURED
                     end if
                  end if
                end associate
             end do
             call lib%driver%unload ()
             lib%status = STAT_CONFIGURED
          end if
       end if
       select case (lib%status)
       case (STAT_CONFIGURED)
          do i = 1, lib%n_entries
             associate (entry => lib%entry(i))
               i_external = entry%i_external
               i_component = entry%i_component
               if (i_external /= 0) then
                  select case (entry%status)
                  case (STAT_CONFIGURED)
                     md5sum_file = lib%driver%get_md5sum_source (i_external)
                     if (entry%def%get_md5sum (i_component) == md5sum_file) then
                        entry%status = STAT_SOURCE
                     end if
                  end select
               end if
             end associate
          end do
          if (all (lib%entry%status >= STAT_SOURCE)) then
             md5sum_file = lib%driver%get_md5sum_driver ()
             if (lib%md5sum == md5sum_file) then
                lib%status = STAT_SOURCE
             end if
          end if
       end select
    end if
  end subroutine process_library_update_status

  subroutine process_library_make_source (lib, os_data, keep_old_source)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: keep_old_source
    logical :: keep_old
    integer :: i, i_external
    keep_old = .false.
    if (present (keep_old_source))  keep_old = keep_old_source
    if (lib%external .and. .not. lib%static) then
       select case (lib%status)
       case (STAT_CONFIGURED)
          if (keep_old) then
             call msg_message ("Process library '" // char (lib%basename) &
                  // "': keeping source code")
          else
             call msg_message ("Process library '" // char (lib%basename) &
                  // "': creating source code")
             do i = 1, size (lib%entry)
                associate (entry => lib%entry(i))
                  i_external = entry%i_external
                  if (i_external /= 0 &
                       .and. lib%entry(i)%status == STAT_CONFIGURED) then
                     call lib%driver%clean_proc (i_external, os_data)
                  end if
                end associate
                if (signal_is_pending ())  return
             end do
             call lib%driver%make_source (os_data)
          end if
          lib%status = STAT_SOURCE
          where (lib%entry%i_external /= 0 &
               .and. lib%entry%status == STAT_CONFIGURED)
             lib%entry%status = STAT_SOURCE
          end where
          lib%status = STAT_SOURCE
       end select
    end if
  end subroutine process_library_make_source
  
  subroutine process_library_make_compile (lib, os_data, keep_old_source)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: keep_old_source
    if (lib%external .and. .not. lib%static) then
       select case (lib%status)
       case (STAT_CONFIGURED)
          call lib%make_source (os_data, keep_old_source)
       end select
       if (signal_is_pending ())  return
       select case (lib%status)
       case (STAT_SOURCE)
          call msg_message ("Process library '" // char (lib%basename) &
               // "': compiling sources")
          call lib%driver%make_compile (os_data)
          where (lib%entry%i_external /= 0 &
               .and. lib%entry%status == STAT_SOURCE)
             lib%entry%status = STAT_COMPILED
          end where
          lib%status = STAT_COMPILED
       end select
    end if
  end subroutine process_library_make_compile
  
  subroutine process_library_make_link (lib, os_data, keep_old_source)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: keep_old_source
    if (lib%external .and. .not. lib%static) then
       select case (lib%status)
       case (STAT_CONFIGURED:STAT_SOURCE)
          call lib%make_compile (os_data, keep_old_source)
       end select
       if (signal_is_pending ())  return
       select case (lib%status)
       case (STAT_COMPILED)
          call msg_message ("Process library '" // char (lib%basename) &
               // "': linking")
          call lib%driver%make_link (os_data)
          lib%entry%status = STAT_LINKED
          lib%status = STAT_LINKED
       end select
    end if
  end subroutine process_library_make_link
  
  subroutine process_library_load (lib, os_data, keep_old_source)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: keep_old_source
    select case (lib%status)
    case (STAT_CONFIGURED:STAT_COMPILED)
       call lib%make_link (os_data, keep_old_source)
    end select
    if (signal_is_pending ())  return
    select case (lib%status)
    case (STAT_LINKED)
       if (lib%external) then
          call msg_message ("Process library '" // char (lib%basename) &
               // "': loading")
          call lib%driver%load (os_data)
          call lib%load_entries ()
       end if
       lib%entry%status = STAT_ACTIVE
       lib%status = STAT_ACTIVE
    end select
  end subroutine process_library_load
  
  subroutine process_library_load_entries (lib)
    class(process_library_t), intent(inout) :: lib
    integer :: i
    do i = 1, size (lib%entry)
       associate (entry => lib%entry(i))
         if (entry%i_external /= 0) then
            call entry%connect (lib%driver, entry%i_external)
         end if
       end associate
    end do
  end subroutine process_library_load_entries

  subroutine process_library_unload (lib)
    class(process_library_t), intent(inout) :: lib
    select case (lib%status)
    case (STAT_ACTIVE)
       if (lib%external) then
          call msg_message ("Process library '" // char (lib%basename) &
               // "': unloading")
          call lib%driver%unload ()
       end if
       lib%entry%status = STAT_LINKED
       lib%status = STAT_LINKED
    end select
  end subroutine process_library_unload

  subroutine process_library_clean (lib, os_data, distclean)
    class(process_library_t), intent(inout) :: lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: distclean
    call lib%unload ()
    if (lib%external .and. .not. lib%static) then
       call msg_message ("Process library '" // char (lib%basename) &
            // "': removing old files")
       if (distclean) then
          call lib%driver%distclean (os_data)
       else
          call lib%driver%clean (os_data)
       end if
    end if
    where (lib%entry%i_external /= 0)
       lib%entry%status = STAT_CONFIGURED
    elsewhere
       lib%entry%status = STAT_LINKED
    end where
    if (lib%external) then
       lib%status = STAT_CONFIGURED
    else
       lib%status = STAT_LINKED
    end if
  end subroutine process_library_clean
  
  subroutine process_library_open (lib)
    class(process_library_t), intent(inout) :: lib
    select case (lib%status)
    case (STAT_OPEN)
    case default
       call lib%unload ()
       if (.not. lib%static) then
          lib%entry%status = STAT_OPEN
          lib%status = STAT_OPEN
          call msg_message ("Process library '" // char (lib%basename) &
               // "': open")
       else
          call msg_error ("Static process library '" // char (lib%basename) &
               // "': processes can't be appended")
       end if
    end select
  end subroutine process_library_open
  
  function process_library_get_name (lib) result (name)
    class(process_library_t), intent(in) :: lib
    type(string_t) :: name
    name = lib%basename
  end function process_library_get_name
  
  function process_library_is_active (lib) result (flag)
    logical :: flag
    class(process_library_t), intent(in) :: lib
    flag = lib%status == STAT_ACTIVE
  end function process_library_is_active
  
  subroutine process_library_entry_fill_constants (entry, driver, data)
    class(process_library_entry_t), intent(in) :: entry
    class(prclib_driver_t), intent(in) :: driver
    type(process_constants_t), intent(out) :: data
    integer :: i
    if (entry%i_external /= 0) then
       i = entry%i_external
       data%id         = driver%get_process_id (i)
       data%model_name = driver%get_model_name (i)
       data%md5sum     = driver%get_md5sum (i)
       data%openmp_supported = driver%get_openmp_status (i)
       data%n_in  = driver%get_n_in  (i)
       data%n_out = driver%get_n_out (i)
       data%n_flv = driver%get_n_flv (i)
       data%n_hel = driver%get_n_hel (i)
       data%n_col = driver%get_n_col (i)
       data%n_cin = driver%get_n_cin (i)
       data%n_cf  = driver%get_n_cf  (i)
       call driver%set_flv_state (i, data%flv_state)
       call driver%set_hel_state (i, data%hel_state)
       call driver%set_col_state (i, data%col_state, data%ghost_flag)
       call driver%set_color_factors (i, data%color_factors, data%cf_index)
    else
       select type (proc_driver => entry%driver)
       class is (process_driver_internal_t)
          call proc_driver%fill_constants (data)
       end select
    end if
  end subroutine process_library_entry_fill_constants
  
  subroutine process_library_connect_process &
       (lib, id, i_component, data, proc_driver)
    class(process_library_t), intent(in) :: Lib
    type(string_t), intent(in) :: id
    integer, intent(in) :: i_component
    type(process_constants_t), intent(out) :: data
    class(prc_core_driver_t), allocatable, intent(out) :: proc_driver
    integer :: i
    do i = 1, size (lib%entry)
       associate (entry => lib%entry(i))
         if (entry%def%id == id .and. entry%i_component == i_component) then
            call entry%fill_constants (lib%driver, data)
            allocate (proc_driver, source=entry%driver)
            return
         end if
       end associate
    end do
    call msg_fatal ("Process library '" // char (lib%basename) &
               // "': process '" // char (id) // "' not found")
  end subroutine process_library_connect_process

  function process_library_get_modellibs_ldflags (prc_lib, os_data) result (flags)
    class(process_library_t), intent(in) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: flags
    type(string_t), dimension(:), allocatable :: models
    type(string_t) :: modelname, modellib, modellib_full
    logical :: exist
    integer :: i, j, mi
    flags = " -lomega"
    if ((.not. os_data%use_testfiles) .and. &
               os_dir_exist (os_data%whizard_models_libpath_local)) &
                 flags = flags // " -L" // os_data%whizard_models_libpath_local
    flags = flags // " -L" // os_data%whizard_models_libpath
    allocate (models(prc_lib%n_entries + 1))
    models = ""
    mi = 1
    if (allocated (prc_lib%entry)) then
       SCAN: do i = 1, prc_lib%n_entries
          if (associated (prc_lib%entry(i)%def)) then
             if (prc_lib%entry(i)%def%model_name /= "") then
                modelname = prc_lib%entry(i)%def%model_name
             else
                cycle SCAN
             end if
          else
             cycle SCAN
          end if
          do j = 1, mi
             if (models(mi) == modelname) cycle SCAN
          end do
          models(mi) = modelname
          mi = mi + 1
          if (os_data%use_libtool) then
             modellib = "libparameters_" // modelname // ".la"
          else
             modellib = "libparameters_" // modelname // ".a"
          end if          
          exist = .false.
          if (.not. os_data%use_testfiles) then
             modellib_full = os_data%whizard_models_libpath_local &
                  // "/" // modellib
             inquire (file=char (modellib_full), exist=exist)
          end if
          if (.not. exist) then
             modellib_full = os_data%whizard_models_libpath &
                  // "/" // modellib
             inquire (file=char (modellib_full), exist=exist)
          end if
          if (exist) flags = flags // " -lparameters_" // modelname
       end do SCAN
    end if
    deallocate (models)
    flags = flags // " -lwhizard"
  end function process_library_get_modellibs_ldflags

  function process_library_get_static_modelname (prc_lib, os_data) result (name)
    class(process_library_t), intent(in) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: name
    type(string_t), dimension(:), allocatable :: models
    type(string_t) :: modelname, modellib, modellib_full
    logical :: exist
    integer :: i, j, mi
    name = ""
    allocate (models(prc_lib%n_entries + 1))
    models = ""
    mi = 1
    if (allocated (prc_lib%entry)) then
       SCAN: do i = 1, prc_lib%n_entries
          if (associated (prc_lib%entry(i)%def)) then
             if (prc_lib%entry(i)%def%model_name /= "") then
                modelname = prc_lib%entry(i)%def%model_name
             else
                cycle SCAN
             end if
          else
             cycle SCAN
          end if
          do j = 1, mi
             if (models(mi) == modelname) cycle SCAN
          end do
          models(mi) = modelname
          mi = mi + 1
          modellib = "libparameters_" // modelname // ".a"
          exist = .false.
          if (.not. os_data%use_testfiles) then
             modellib_full = os_data%whizard_models_libpath_local &
                  // "/" // modellib
             inquire (file=char (modellib_full), exist=exist)
          end if
          if (.not. exist) then
             modellib_full = os_data%whizard_models_libpath &
                  // "/" // modellib
             inquire (file=char (modellib_full), exist=exist)
          end if
          if (exist) name = name // " " // modellib_full
       end do SCAN
    end if
    deallocate (models)
  end function process_library_get_static_modelname

  function prctest_2_type_name () result (type)
    type(string_t) :: type
    type = "test"
  end function prctest_2_type_name
  
  subroutine prctest_2_fill_constants (driver, data)
    class(prctest_2_t), intent(in) :: driver
    type(process_constants_t), intent(out) :: data
  end subroutine prctest_2_fill_constants


  subroutine process_libraries_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (process_libraries_1, "process_libraries_1", &
         "empty process list", &
         u, results)
    call test (process_libraries_2, "process_libraries_2", &
         "process definition list", &
         u, results)
    call test (process_libraries_3, "process_libraries_3", &
         "recover process definition list from file", &
         u, results)
    call test (process_libraries_4, "process_libraries_4", &
         "build and load internal process library", &
         u, results)
    call test (process_libraries_5, "process_libraries_5", &
         "build external process library", &
         u, results)
    call test (process_libraries_6, "process_libraries_6", &
         "build and load external process library", &
         u, results)
    call test (process_libraries_7, "process_libraries_7", &
         "process definition list", &
         u, results)
    call test (process_libraries_8, "process_libraries_8", &
         "library status checks", &
         u, results)
  end subroutine process_libraries_test

  subroutine process_libraries_1 (u)
    integer, intent(in) :: u
    type(process_def_list_t) :: process_def_list

    write (u, "(A)")  "* Test output: process_libraries_1"
    write (u, "(A)")  "*   Purpose: Display an empty process definition list"
    write (u, "(A)")

    call process_def_list%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_1"
  end subroutine process_libraries_1
  
  function prcdef_2_type_string () result (string)
    type(string_t) :: string
    string = "test"
  end function prcdef_2_type_string

  subroutine prcdef_2_write (object, unit)
    class(prcdef_2_t), intent(in) :: object
    integer, intent(in) :: unit
    write (unit, "(3x,A,I0)")  "Test data         = ", object%data
  end subroutine prcdef_2_write
  
  subroutine prcdef_2_read (object, unit)
    class(prcdef_2_t), intent(out) :: object
    integer, intent(in) :: unit
    character(80) :: buffer
    read (unit, "(A)")  buffer
    call strip_prefix (buffer)
    read (buffer, *)  object%data
  end subroutine prcdef_2_read
  
  subroutine prcdef_2_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (0))
  end subroutine prcdef_2_get_features

  subroutine prcdef_2_generate_code (object, &
       basename, model_name, prt_in, prt_out)
    class(prcdef_2_t), intent(in) :: object
    type(string_t), intent(in) :: basename
    type(string_t), intent(in) :: model_name
    type(string_t), dimension(:), intent(in) :: prt_in
    type(string_t), dimension(:), intent(in) :: prt_out
  end subroutine prcdef_2_generate_code
  
  subroutine prcdef_2_allocate_driver (object, driver, basename)
    class(prcdef_2_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    allocate (prctest_2_t :: driver)
  end subroutine prcdef_2_allocate_driver
  
  subroutine prcdef_2_connect (def, lib_driver, i, proc_driver)
    class(prcdef_2_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
  end subroutine prcdef_2_connect

  subroutine process_libraries_2 (u)
    integer, intent(in) :: u
    type(prc_template_t), dimension(:), allocatable :: process_core_templates
    type(process_def_list_t) :: process_def_list
    type(process_def_entry_t), pointer :: entry => null ()
    class(prc_core_def_t), allocatable :: test_def
    integer :: scratch_unit

    write (u, "(A)")  "* Test output: process_libraries_2"
    write (u, "(A)")  "* Purpose: Construct a process definition list,"
    write (u, "(A)")  "*          write it to file and reread it"
    write (u, "(A)")  ""
    write (u, "(A)")  "* Construct a process definition list"
    write (u, "(A)")  "*   First process definition: empty"
    write (u, "(A)")  "*   Second process definition: two components"
    write (u, "(A)")  "*     First component: empty"
    write (u, "(A)")  "*     Second component: test data"
    write (u, "(A)")  "*   Third process definition:"
    write (u, "(A)")  "*     Embedded decays and polarization"
    write (u, "(A)")

    allocate (process_core_templates (1))
    allocate (prcdef_2_t :: process_core_templates(1)%core_def)
    
    allocate (entry)
    call entry%init (var_str ("first"), n_in = 0, n_components = 0)
    call entry%compute_md5sum ()
    call process_def_list%append (entry)
    
    allocate (entry)
    call entry%init (var_str ("second"), model_name = var_str ("Test"), &
         n_in = 1, n_components = 2)
    allocate (prcdef_2_t :: test_def)
    select type (test_def)
    type is (prcdef_2_t);  test_def%data = 42
    end select
    call entry%import_component (2, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c")]), &
         method  = var_str ("test"), &
         variant = test_def)
    call entry%compute_md5sum ()
    call process_def_list%append (entry)

    allocate (entry)
    call entry%init (var_str ("third"), model_name = var_str ("Test"), &
         n_in = 2, n_components = 1)
    allocate (prcdef_2_t :: test_def)
    call entry%import_component (1, n_out = 3, &
         prt_in  = &
           new_prt_spec ([var_str ("a"), var_str ("b")]), &
         prt_out = &
           [new_prt_spec (var_str ("c")), &
            new_prt_spec (var_str ("d"), .true.), &
            new_prt_spec (var_str ("e"), [var_str ("e_decay")])], &
         method  = var_str ("test"), &
         variant = test_def)
    call entry%compute_md5sum ()
    call process_def_list%append (entry)
    call process_def_list%write (u)

    write (u, "(A)")  ""
    write (u, "(A)")  "* Write the process definition list to (scratch) file"

    scratch_unit = free_unit ()
    open (unit = scratch_unit, status="scratch", action = "readwrite")
    call process_def_list%write (scratch_unit)
    call process_def_list%final ()
    
    write (u, "(A)")  "* Reread it"
    write (u, "(A)")  ""

    rewind (scratch_unit)
    call process_def_list%read (scratch_unit, process_core_templates)
    close (scratch_unit)
    
    call process_def_list%write (u)
    call process_def_list%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_2"
  end subroutine process_libraries_2
  
  subroutine process_libraries_3 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    type(process_def_entry_t), pointer :: entry

    write (u, "(A)")  "* Test output: process_libraries_3"
    write (u, "(A)")  "* Purpose: Construct a process library object &
         &with entries"
    write (u, "(A)")  ""
    write (u, "(A)")  "* Construct and display a process library object"
    write (u, "(A)")  "*   with 5 entries"
    write (u, "(A)")  "*   associated with 3 matrix element codes"
    write (u, "(A)")  "*   corresponding to 3 process definitions"
    write (u, "(A)")  "*   with 2, 1, 1 components, respectively"
    write (u, "(A)")

    call lib%init (var_str ("testlib"))

    lib%status = STAT_ACTIVE
    lib%n_entries = 5
    allocate (lib%entry (lib%n_entries))
   
    allocate (entry)
    call entry%init (var_str ("test_a"), n_in = 2, n_components = 2)
    call lib%entry(3)%init (STAT_SOURCE, entry%process_def_t, 2, 2)
    allocate (prctest_2_t :: lib%entry(3)%driver)
    call lib%entry(4)%init (STAT_COMPILED, entry%process_def_t, 1, 0)
    call lib%append (entry)

    allocate (entry)
    call entry%init (var_str ("test_b"), n_in = 2, n_components = 1)
    call lib%entry(2)%init (STAT_CONFIGURED, entry%process_def_t, 1, 1)
    call lib%append (entry)

    allocate (entry)
    call entry%init (var_str ("test_c"), n_in = 2, n_components = 1)
    call lib%entry(5)%init (STAT_LINKED, entry%process_def_t, 1, 3)
    allocate (prctest_2_t :: lib%entry(5)%driver)
    call lib%append (entry)
    
    call lib%write (u)
    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_3"
  end subroutine process_libraries_3
  
  subroutine process_libraries_4 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    type(process_def_entry_t), pointer :: entry
    class(prc_core_def_t), allocatable :: core_def
    type(os_data_t) :: os_data

    write (u, "(A)")  "* Test output: process_libraries_4"
    write (u, "(A)")  "* Purpose: build a process library with an &
         &internal (pseudo) matrix element"
    write (u, "(A)")  "*          No Makefile or code should be generated"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry &
         &(no external code)"
    write (u, "(A)")
    call os_data_init (os_data)
    call lib%init (var_str ("proclibs4"))

    allocate (prcdef_2_t :: core_def)

    allocate (entry)
    call entry%init (var_str ("proclibs4_a"), n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = 2, variant = core_def)
    call lib%append (entry)

    write (u, "(A)")  "* Configure library"
    write (u, "(A)")
    call lib%configure (os_data)

    write (u, "(A)")  "* Compute MD5 sum"
    write (u, "(A)")
    call lib%compute_md5sum ()

    write (u, "(A)")  "* Write makefile (no-op)"
    write (u, "(A)")
    call lib%write_makefile (os_data, force = .true.)

    write (u, "(A)")  "* Write driver source code (no-op)"
    write (u, "(A)")
    call lib%write_driver (force = .true.)

    write (u, "(A)")  "* Write process source code (no-op)"
    write (u, "(A)")
    call lib%make_source (os_data)

    write (u, "(A)")  "* Compile (no-op)"
    write (u, "(A)")
    call lib%make_compile (os_data)

    write (u, "(A)")  "* Link (no-op)"
    write (u, "(A)")
    call lib%make_link (os_data)

    write (u, "(A)")  "* Load (no-op)"
    write (u, "(A)")
    call lib%load (os_data)

    call lib%write (u)
    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_4"
  end subroutine process_libraries_4
  
  function prcdef_5_type_string () result (string)
    type(string_t) :: string
    string = "test_file"
  end function prcdef_5_type_string

  subroutine prcdef_5_init (object)
    class(prcdef_5_t), intent(out) :: object
    allocate (test_writer_4_t :: object%writer)
  end subroutine prcdef_5_init
  
  subroutine prcdef_5_write (object, unit)
    class(prcdef_5_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine prcdef_5_write
  
  subroutine prcdef_5_read (object, unit)
    class(prcdef_5_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine prcdef_5_read
  
  subroutine prcdef_5_allocate_driver (object, driver, basename)
    class(prcdef_5_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    allocate (prctest_5_t :: driver)
  end subroutine prcdef_5_allocate_driver
  
  function prcdef_5_needs_code () result (flag)
    logical :: flag
    flag = .true.
  end function prcdef_5_needs_code
  
  subroutine prcdef_5_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (1))
    features = [ var_str ("proc1") ]
  end subroutine prcdef_5_get_features

  subroutine prcdef_5_connect (def, lib_driver, i, proc_driver)
    class(prcdef_5_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
  end subroutine prcdef_5_connect

  function prctest_5_type_name () result (type)
    type(string_t) :: type
    type = "test_file"
  end function prctest_5_type_name
  
  subroutine process_libraries_5 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    type(process_def_entry_t), pointer :: entry
    class(prc_core_def_t), allocatable :: core_def
    type(os_data_t) :: os_data

    write (u, "(A)")  "* Test output: process_libraries_5"
    write (u, "(A)")  "* Purpose: build a process library with an &
         &external (pseudo) matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry"
    write (u, "(A)")
    call lib%init (var_str ("proclibs5"))
    call os_data_init (os_data)

    allocate (prcdef_5_t :: core_def)
    select type (core_def)
    type is (prcdef_5_t)
       call core_def%init ()
    end select

    allocate (entry)
    call entry%init (var_str ("proclibs5_a"), &
         model_name = var_str ("Test_Model"), &
         n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c")]), &
         method  = var_str ("test"), &
         variant = core_def)
    call lib%append (entry)
    
    write (u, "(A)")  "* Configure library"
    write (u, "(A)")
    call lib%configure (os_data)

    write (u, "(A)")  "* Compute MD5 sum"
    write (u, "(A)")
    call lib%compute_md5sum ()

    write (u, "(A)")  "* Write makefile"
    write (u, "(A)")
    call lib%write_makefile (os_data, force = .true.)
    
    write (u, "(A)")  "* Write driver source code"
    write (u, "(A)")
    call lib%write_driver (force = .true.)

    write (u, "(A)")  "* Write process source code"
    write (u, "(A)")
    call lib%make_source (os_data)

    write (u, "(A)")  "* Compile"
    write (u, "(A)")
    call lib%make_compile (os_data)

    write (u, "(A)")  "* Link"
    write (u, "(A)")
    call lib%make_link (os_data)

    call lib%write (u, libpath = .false.)
    
    call lib%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_5"
  end subroutine process_libraries_5
  
  function prcdef_6_type_string () result (string)
    type(string_t) :: string
    string = "test_file"
  end function prcdef_6_type_string

  subroutine prcdef_6_init (object)
    class(prcdef_6_t), intent(out) :: object
    allocate (test_writer_4_t :: object%writer)
    call object%writer%init_test ()
  end subroutine prcdef_6_init
  
  subroutine prcdef_6_write (object, unit)
    class(prcdef_6_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine prcdef_6_write
  
  subroutine prcdef_6_read (object, unit)
    class(prcdef_6_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine prcdef_6_read
  
  subroutine prcdef_6_allocate_driver (object, driver, basename)
    class(prcdef_6_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    allocate (prctest_6_t :: driver)
  end subroutine prcdef_6_allocate_driver
  
  function prcdef_6_needs_code () result (flag)
    logical :: flag
    flag = .true.
  end function prcdef_6_needs_code
  
  subroutine prcdef_6_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (1))
    features = [ var_str ("proc1") ]
  end subroutine prcdef_6_get_features

  subroutine prcdef_6_connect (def, lib_driver, i, proc_driver)
    class(prcdef_6_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
    integer(c_int) :: pid, fid
    type(c_funptr) :: fptr
    select type (proc_driver)
    type is  (prctest_6_t)
       pid = i
       fid = 1
       call lib_driver%get_fptr (pid, fid, fptr)
       call c_f_procpointer (fptr, proc_driver%proc1)
    end select
  end subroutine prcdef_6_connect

  function prctest_6_type_name () result (type)
    type(string_t) :: type
    type = "test_file"
  end function prctest_6_type_name
  
  subroutine process_libraries_6 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    type(process_def_entry_t), pointer :: entry
    class(prc_core_def_t), allocatable :: core_def
    type(os_data_t) :: os_data
    type(string_t), dimension(:), allocatable :: name_list
    type(process_constants_t) :: data
    class(prc_core_driver_t), allocatable :: proc_driver
    integer :: i
    integer(c_int) :: n

    write (u, "(A)")  "* Test output: process_libraries_6"
    write (u, "(A)")  "* Purpose: build and load a process library"
    write (u, "(A)")  "*          with an external (pseudo) matrix element"
    write (u, "(A)")  "*          Check single-call linking"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry"
    write (u, "(A)")
    call lib%init (var_str ("proclibs6"))
    call os_data_init (os_data)

    allocate (prcdef_6_t :: core_def)
    select type (core_def)
    type is (prcdef_6_t)
       call core_def%init ()
    end select

    allocate (entry)
    call entry%init (var_str ("proclibs6_a"), &
         model_name = var_str ("Test_model"), &
         n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c")]), &
         method  = var_str ("test"), &
         variant = core_def)
    call lib%append (entry)

    write (u, "(A)")  "* Configure library"
    write (u, "(A)")
    call lib%configure (os_data)

    write (u, "(A)")  "* Write makefile"
    write (u, "(A)")
    call lib%write_makefile (os_data, force = .true.)
    
    write (u, "(A)")  "* Write driver source code"
    write (u, "(A)")
    call lib%write_driver (force = .true.)

    write (u, "(A)")  "* Write process source code, compile, link, load"
    write (u, "(A)")
    call lib%load (os_data)

    call lib%write (u, libpath = .false.)
    
    write (u, "(A)")
    write (u, "(A)")  "* Probe library API:"
    write (u, "(A)")
       
    write (u, "(1x,A,A,A)")  "name                      = '", &
         char (lib%get_name ()), "'"
    write (u, "(1x,A,L1)")  "is active                 = ", &
         lib%is_active ()
    write (u, "(1x,A,I0)")  "n_processes               = ", &
         lib%get_n_processes ()
    write (u, "(1x,A)", advance="no")  "processes                 ="
    call lib%get_process_id_list (name_list)
    do i = 1, size (name_list)
       write (u, "(1x,A)", advance="no")  char (name_list(i))
    end do
    write (u, *)
    write (u, "(1x,A,L1)")  "proclibs6_a is process    = ", &
         lib%contains (var_str ("proclibs6_a"))
    write (u, "(1x,A,I0)")  "proclibs6_a has index     = ", &
         lib%get_entry_index (var_str ("proclibs6_a"))
    write (u, "(1x,A,L1)")  "foobar is process         = ", &
         lib%contains (var_str ("foobar"))
    write (u, "(1x,A,I0)")  "foobar has index          = ", &
         lib%get_entry_index (var_str ("foobar"))
    write (u, "(1x,A,I0)")  "n_in(proclibs6_a)         = ", &
         lib%get_n_in (var_str ("proclibs6_a"))
    write (u, "(1x,A,A)")   "model_name(proclibs6_a)   = ", &
         char (lib%get_model_name (var_str ("proclibs6_a")))
    write (u, "(1x,A,I0)")  "n_components(proclibs6_a) = ", &
         lib%get_n_components (var_str ("proclibs6_a"))
    write (u, "(1x,A)", advance="no")  "components(proclibs6_a)   ="
    call lib%get_component_list (var_str ("proclibs6_a"), name_list)
    do i = 1, size (name_list)
       write (u, "(1x,A)", advance="no")  char (name_list(i))
    end do
    write (u, *)
    
    write (u, "(A)")
    write (u, "(A)")  "* Constants of proclibs6_a_i1:"
    write (u, "(A)")

    call lib%connect_process (var_str ("proclibs6_a"), 1, data, proc_driver)

    write (u, "(1x,A,A)")  "component ID     = ", char (data%id)
    write (u, "(1x,A,A)")  "model name       = ", char (data%model_name)
    write (u, "(1x,A,A,A)")  "md5sum           = '", data%md5sum, "'"
    write (u, "(1x,A,L1)") "openmp supported = ", data%openmp_supported
    write (u, "(1x,A,I0)") "n_in  = ", data%n_in
    write (u, "(1x,A,I0)") "n_out = ", data%n_out
    write (u, "(1x,A,I0)") "n_flv = ", data%n_flv
    write (u, "(1x,A,I0)") "n_hel = ", data%n_hel
    write (u, "(1x,A,I0)") "n_col = ", data%n_col
    write (u, "(1x,A,I0)") "n_cin = ", data%n_cin
    write (u, "(1x,A,I0)") "n_cf  = ", data%n_cf
    write (u, "(1x,A,10(1x,I0))") "flv state =", data%flv_state
    write (u, "(1x,A,10(1x,I0))") "hel state =", data%hel_state
    write (u, "(1x,A,10(1x,I0))") "col state =", data%col_state
    write (u, "(1x,A,10(1x,L1))") "ghost flag =", data%ghost_flag
    write (u, "(1x,A,10(1x,F5.3))") "color factors =", data%color_factors
    write (u, "(1x,A,10(1x,I0))") "cf index =", data%cf_index
       
    write (u, "(A)")
    write (u, "(A)")  "* Call feature of proclibs6_a:"
    write (u, "(A)")

    select type (proc_driver)
    type is (prctest_6_t)
       call proc_driver%proc1 (n)
       write (u, "(1x,A,I0)") "proc1 = ", n
    end select

    call lib%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_6"
  end subroutine process_libraries_6
  
  subroutine process_libraries_7 (u)
    integer, intent(in) :: u
    type(prc_template_t), dimension(:), allocatable :: process_core_templates
    type(process_def_entry_t) :: entry
    class(prc_core_def_t), allocatable :: test_def

    write (u, "(A)")  "* Test output: process_libraries_7"
    write (u, "(A)")  "* Purpose: Construct a process definition list &
         &and check MD5 sums"
    write (u, "(A)")
    write (u, "(A)")  "* Construct a process definition list"
    write (u, "(A)")  "*   Process: two components"
    write (u, "(A)")

    allocate (process_core_templates (1))
    allocate (prcdef_2_t :: process_core_templates(1)%core_def)
    
    call entry%init (var_str ("first"), model_name = var_str ("Test"), &
         n_in = 1, n_components = 2)
    allocate (prcdef_2_t :: test_def)
    select type (test_def)
    type is (prcdef_2_t);  test_def%data = 31
    end select
    call entry%import_component (1, n_out = 3, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c"), &
                                  var_str ("e")]), &
         method  = var_str ("test"), &
         variant = test_def)
    allocate (prcdef_2_t :: test_def)
    select type (test_def)
    type is (prcdef_2_t);  test_def%data = 42
    end select
    call entry%import_component (2, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c")]), &
         method  = var_str ("test"), &
         variant = test_def)
    call entry%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Compute MD5 sums"
    write (u, "(A)")

    call entry%compute_md5sum ()
    call entry%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recalculate MD5 sums (should be identical)"
    write (u, "(A)")

    call entry%compute_md5sum ()
    call entry%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Modify a component and recalculate MD5 sums"
    write (u, "(A)")

    select type (test_def => entry%initial(2)%core_def)
    type is (prcdef_2_t)
       test_def%data = 54
    end select
    call entry%compute_md5sum ()
    call entry%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Modify the model and recalculate MD5 sums"
    write (u, "(A)")

    entry%model_name = "foo"
    call entry%compute_md5sum ()
    call entry%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_7"
  end subroutine process_libraries_7
  
  subroutine process_libraries_8 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    type(process_def_entry_t), pointer :: entry
    class(prc_core_def_t), allocatable :: core_def
    type(os_data_t) :: os_data

    write (u, "(A)")  "* Test output: process_libraries_8"
    write (u, "(A)")  "* Purpose: build and load a process library"
    write (u, "(A)")  "*          with an external (pseudo) matrix element"
    write (u, "(A)")  "*          Check status updates"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry"
    write (u, "(A)")
    call lib%init (var_str ("proclibs8"))
    call os_data_init (os_data)

    allocate (prcdef_6_t :: core_def)
    select type (core_def)
    type is (prcdef_6_t)
       call core_def%init ()
    end select

    allocate (entry)
    call entry%init (var_str ("proclibs8_a"), &
         model_name = var_str ("Test_model"), &
         n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("c")]), &
         method  = var_str ("test"), &
         variant = core_def)
    call lib%append (entry)

    write (u, "(A)")  "* Configure library"
    write (u, "(A)")

    call lib%configure (os_data)
    call lib%compute_md5sum ()
    
    associate (def => lib%entry(1)%def%initial(1))
      select type (writer => lib%driver%record(1)%writer)
      type is (test_writer_4_t)
         writer%md5sum = def%md5sum
      end select
    end associate

    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status

    write (u, "(A)")
    write (u, "(A)")  "* Write makefile"
    write (u, "(A)")
    call lib%write_makefile (os_data, force = .true.)
    
    write (u, "(A)")  "* Update status"
    write (u, "(A)")
    
    call lib%update_status (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Write driver source code"
    write (u, "(A)")
    call lib%write_driver (force = .false.)

    write (u, "(A)")  "* Write process source code"
    write (u, "(A)")
    call lib%make_source (os_data)

    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status

    write (u, "(A)")
    write (u, "(A)")  "* Compile and load"
    write (u, "(A)")

    call lib%load (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status

    write (u, "(A)")
    write (u, "(A)")  "* Append process and reconfigure"
    write (u, "(A)")
    
    allocate (prcdef_6_t :: core_def)
    select type (core_def)
    type is (prcdef_6_t)
       call core_def%init ()
    end select

    allocate (entry)
    call entry%init (var_str ("proclibs8_b"), &
         model_name = var_str ("Test_model"), &
         n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = 2, &
         prt_in  = new_prt_spec ([var_str ("a")]), &
         prt_out = new_prt_spec ([var_str ("b"), var_str ("d")]), &
         method  = var_str ("test"), &
         variant = core_def)
    call lib%append (entry)

    call lib%configure (os_data)
    call lib%compute_md5sum ()
    associate (def => lib%entry(2)%def%initial(1))
      select type (writer => lib%driver%record(2)%writer)
      type is (test_writer_4_t)
         writer%md5sum = def%md5sum
      end select
    end associate
    call lib%write_makefile (os_data, force = .false.)
    call lib%write_driver (force = .false.)

    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status

    write (u, "(A)")
    write (u, "(A)")  "* Update status"
    write (u, "(A)")
    
    call lib%update_status (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Write source code"
    write (u, "(A)")
    
    call lib%make_source (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset status"
    write (u, "(A)")
    
    lib%status = STAT_CONFIGURED
    lib%entry%status = STAT_CONFIGURED
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Update status"
    write (u, "(A)")
    
    call lib%update_status (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Partial cleanup"
    write (u, "(A)")

    call lib%clean (os_data, distclean = .false.)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    

    write (u, "(A)")
    write (u, "(A)")  "* Update status"
    write (u, "(A)")
    
    call lib%update_status (os_data)
    write (u, "(1x,A,L1)")  "library loaded = ", lib%driver%loaded
    write (u, "(1x,A,I0)")  "lib status   = ", lib%status
    write (u, "(1x,A,I0)")  "proc1 status = ", lib%entry(1)%status
    write (u, "(1x,A,I0)")  "proc2 status = ", lib%entry(2)%status
    
    write (u, "(A)")
    write (u, "(A)")  "* Complete cleanup"

    call lib%clean (os_data, distclean = .true.)
    call lib%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_libraries_8"
  end subroutine process_libraries_8
  

end module process_libraries
