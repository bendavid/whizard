! WHIZARD 2.0.1 Sun Apr 25 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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
  use kinds !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use md5
  use os_interface
  use lexers
  use variables
  use models
  use flavors
  use prclib_interfaces

  implicit none
  private

  public :: process_configuration_t
  public :: process_library_t
  public :: process_library_init
  public :: process_library_final
  public :: process_library_advance
  public :: process_library_write
  public :: process_library_set_static
  public :: process_library_is_static
  public :: process_library_is_compiled
  public :: process_library_is_loaded
  public :: process_library_get_name
  public :: process_library_get_n_processes
  public :: process_library_check_name_consistency
  public :: process_library_get_process_index
  public :: process_library_append
  public :: process_library_update_status
  public :: process_library_get_process_id
  public :: process_library_get_process_pid
  public :: process_library_get_process_md5sum
  public :: process_library_get_process_model_name
  public :: process_library_generate_code
  public :: process_library_write_driver
  public :: write_library_manager
  public :: get_modellibs_flags
  public :: process_library_compile
  public :: process_library_link
  public :: compile_library_manager
  public :: link_executable
  public :: process_library_load
  public :: process_library_unload
  public :: process_library_set_unload_hook
  public :: process_library_set_reload_hook
  public :: process_library_store_append
  public :: process_library_store_final
  public :: process_library_store_load
  public :: process_library_store_get_ptr
  public :: process_library_store_get_first
  public :: process_library_store_load_static
  public :: process_library_record_integral
  public :: process_library_get_n_calls
  public :: process_library_get_integral
  public :: process_library_get_error
  public :: process_library_get_accuracy
  public :: process_library_get_chi2
  public :: process_library_get_efficiency
  public :: process_libraries_test

  integer, parameter, public :: PRC_UNDEFINED = 0
  integer, parameter, public :: PRC_OMEGA = 1
  integer, parameter, public :: PRC_TEST = 2
  integer, parameter, public :: PRC_UNIT = 3
  integer, parameter, public :: PRC_EXTERNAL = 4
  integer, parameter, public :: PRC_DIPOLE = 5
  integer, parameter :: STAT_UNKNOWN = 0
  integer, parameter :: STAT_CONFIGURED = 1
  integer, parameter :: STAT_CODE_GENERATED = 2
  integer, parameter :: STAT_COMPILED = 3
  integer, parameter :: STAT_LOADED = 4
  integer, parameter :: STAT_INTEGRATED = 5


  type :: process_configuration_t
     private
     integer :: status = STAT_UNKNOWN
     integer :: method = PRC_UNDEFINED
     type(string_t) :: id
     type(model_t), pointer :: model => null ()
     integer :: n_in  = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     type(string_t), dimension(:), allocatable :: prt_in, prt_out
     type(string_t) :: restrictions
     character(32) :: md5sum = ""
     logical :: result_is_known = .false.
     integer :: n_calls = 0
     real(default) :: integral = 0
     real(default) :: error = 0
     real(default) :: accuracy = 0
     real(default) :: chi2 = 0
     real(default) :: efficiency = 0
     type(process_configuration_t), pointer :: next => null ()
  end type process_configuration_t

  type :: process_library_t
     ! private
     logical :: static = .false.
     integer :: status = STAT_UNKNOWN
     type(string_t) :: basename
     type(string_t) :: srcname
     type(string_t) :: libname
     integer :: n_prc = 0
     type(process_configuration_t), pointer :: prc_first => null ()
     type(process_configuration_t), pointer :: prc_last => null ()
     type(dlaccess_t) :: dlaccess
     procedure(prc_get_n_processes), nopass, pointer :: get_n_prc => null ()
     procedure(prc_get_stringptr), nopass, pointer :: get_process_id => null ()
     procedure(prc_get_stringptr), nopass, pointer :: get_model_name => null ()
     procedure(prc_get_stringptr), nopass, pointer :: &
          get_restrictions => null ()
     procedure(prc_get_stringptr), nopass, pointer :: get_md5sum => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_in  => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_out => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_flv => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_hel => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_col => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_cin => null ()
     procedure(prc_get_int), nopass, pointer :: get_n_cf  => null ()
     procedure(prc_set_int_tab1), nopass, pointer :: set_flv_state => null ()
     procedure(prc_set_int_tab1), nopass, pointer :: set_hel_state => null ()
     procedure(prc_set_int_tab2), nopass, pointer :: set_col_state => null ()
     procedure(prc_set_cf_tab), nopass, pointer :: set_cf_table => null ()
     procedure(prc_get_fptr), nopass, pointer :: init_get_fptr  => null()
     procedure(prc_get_fptr), nopass, pointer :: final_get_fptr => null()
     procedure(prc_get_fptr), nopass, pointer :: &
          update_alpha_s_get_fptr => null ()
     procedure(prc_get_fptr), nopass, pointer :: &
          reset_helicity_selection_get_fptr => null ()
     procedure(prc_get_fptr), nopass, pointer :: new_event_get_fptr => null ()
     procedure(prc_get_fptr), nopass, pointer :: is_allowed_get_fptr => null ()
     procedure(prc_get_fptr), nopass, pointer :: get_amplitude_get_fptr &
          => null ()
     procedure(prclib_unload_hook), nopass, pointer :: unload_hook => null ()
     procedure(prclib_reload_hook), nopass, pointer :: reload_hook => null ()
     type(process_library_t), pointer :: next => null ()
  end type process_library_t

  type :: process_library_store_t
     private
     type(process_library_t), pointer :: first => null ()
     type(process_library_t), pointer :: last => null ()
  end type process_library_store_t


  interface
     function libmanager_get_n_libs () result (n)
       integer :: n
     end function libmanager_get_n_libs
  end interface

  interface
     function libmanager_get_libname (i) result (name)
       use iso_varying_string, string_t => varying_string !NODEP!
       type(string_t) :: name
       integer, intent(in) :: i
     end function libmanager_get_libname
  end interface

  interface
     function libmanager_get_c_funptr (libname, fname) result (c_fptr)
       use iso_c_binding !NODEP!
       type(c_funptr) :: c_fptr
       character(*), intent(in) :: libname, fname
     end function libmanager_get_c_funptr
  end interface


  type(process_library_store_t), save :: process_library_store

contains

  subroutine process_configuration_init &
       (prc_conf, prc_id, model, prt_in, prt_out, method, status, &
        restrictions, known_md5sum)
    type(process_configuration_t), intent(inout) :: prc_conf
    type(string_t), intent(in) :: prc_id
    type(model_t), intent(in), target :: model
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    integer, intent(in), optional :: status
    integer, intent(in), optional :: method
    type(string_t), intent(in), optional :: restrictions
    character(32), intent(in), optional :: known_md5sum
    prc_conf%id = prc_id
    prc_conf%model => model
    prc_conf%n_in  = size (prt_in)
    prc_conf%n_out = size (prt_out)
    prc_conf%n_tot = prc_conf%n_in + prc_conf%n_out
    if (allocated (prc_conf%prt_in))  deallocate (prc_conf%prt_in)
    allocate (prc_conf%prt_in  (prc_conf%n_in))
    if (allocated (prc_conf%prt_out)) deallocate (prc_conf%prt_out)
    allocate (prc_conf%prt_out (prc_conf%n_out))
    prc_conf%prt_in  = prt_in
    prc_conf%prt_out = prt_out
    prc_conf%result_is_known = .false.
    prc_conf%n_calls = 0
    prc_conf%integral = 0
    prc_conf%error = 0
    prc_conf%accuracy = 0
    prc_conf%chi2 = 0
    prc_conf%efficiency = 0
    if (present (status)) then
       prc_conf%status = status
    else
       prc_conf%status = STAT_CONFIGURED
    end if
    if (present (method)) then
       prc_conf%method = method
    else
       prc_conf%method = PRC_OMEGA
    end if
    if (present (restrictions)) then
       prc_conf%restrictions = canonicalize_restrictions (restrictions, model)
    else
       prc_conf%restrictions = ""
    end if
    if (present (known_md5sum)) then
       prc_conf%md5sum = known_md5sum
    else
       call process_configuration_compute_md5sum (prc_conf)
    end if
  end subroutine process_configuration_init

  subroutine process_configuration_compute_md5sum (prc_conf)
    type(process_configuration_t), intent(inout) :: prc_conf
    integer :: u, i
    u = free_unit ()
    open (unit=u, status="scratch")
    write (u, "(A)")  char (model_get_name (prc_conf%model))
    write (u, "(I0)")  prc_conf%n_in
    write (u, "(I0)")  prc_conf%n_out
    write (u, "(I0)")  prc_conf%n_tot
    do i = 1, size (prc_conf%prt_in)
       write (u, "(A)")  char (prc_conf%prt_in(i))
    end do
    do i = 1, size (prc_conf%prt_out)
       write (u, "(A)")  char (prc_conf%prt_out(i))
    end do
    if (prc_conf%restrictions /= "") then
       write (u, "(A)")  char (prc_conf%restrictions)
    end if
    rewind (u)
    prc_conf%md5sum = md5sum (u)
    close (u)
  end subroutine process_configuration_compute_md5sum
    
  subroutine process_configuration_record_integral &
       (prc_conf, n_calls, integral, error, accuracy, chi2, efficiency)
    type(process_configuration_t), intent(inout) :: prc_conf
    integer, intent(in) :: n_calls
    real(default), intent(in) :: integral, error, accuracy, chi2, efficiency
    prc_conf%n_calls = n_calls
    prc_conf%integral = integral
    prc_conf%error = error
    prc_conf%accuracy = accuracy
    prc_conf%chi2 = chi2
    prc_conf%efficiency = efficiency
    prc_conf%result_is_known = .true.
    prc_conf%status = STAT_INTEGRATED
  end subroutine process_configuration_record_integral

  subroutine process_configuration_write (prc_conf, unit)
    type(process_configuration_t), intent(in) :: prc_conf
    integer, intent(in), optional :: unit
    character :: status
    type(string_t) :: in_state, out_state
    integer :: i
    select case (prc_conf%status)
    case (STAT_UNKNOWN);         status = "?"
    case (STAT_CONFIGURED);      status = "O"
    case (STAT_CODE_GENERATED);  status = "G"
    case (STAT_COMPILED);        status = "C"
    case (STAT_LOADED);          status = "L"
    case (STAT_INTEGRATED);      status = "I"
    end select
    in_state = prc_conf%prt_in(1)
    do i = 2, size (prc_conf%prt_in)
       in_state = in_state // ", " // prc_conf%prt_in(i)
    end do
    out_state = prc_conf%prt_out(1)
    do i = 2, size (prc_conf%prt_out)
       out_state = out_state // ", " // prc_conf%prt_out(i)
    end do
    if (prc_conf%restrictions == "") then
       call msg_message (" [" // status // "] " // char (prc_conf%id) // " = " &
            // char (in_state) // " => " // char (out_state), unit)
    else
       call msg_message (" [" // status // "] " // char (prc_conf%id) // " = " &
            // char (in_state) // " => " // char (out_state) &
            // " { $restrictions = " // '"' // char (prc_conf%restrictions) &
            // '"' // " }", unit)  ! $
    end if
  end subroutine process_configuration_write

  function canonicalize_restrictions (string, model) result (newstring)
    type(string_t) :: newstring
    type(string_t), intent(in) :: string
    type(model_t), intent(in), target :: model
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    type(lexeme_t) :: lexeme
    type(string_t) :: token
    newstring = ""
    if (string == "")  return
    call lexer_init (lexer, &
         comment_chars = "", &
         quote_chars = "'", &
         quote_match = "'", &
         single_chars = "+~", &
         special_class = (/ "&" /), &
         keyword_list = null ())
    call stream_init (stream, string)
    call lexer_assign_stream (lexer, stream)
    TRANSFORM_TOKENS: do
       call lex (lexeme, lexer)
       if (lexeme_is_eof (lexeme))  exit TRANSFORM_TOKENS
       if (lexeme_is_break (lexeme)) then
          call msg_message ("Restriction string = " &
               // '"' // char (string) // '"')
          call msg_fatal ("Syntax error in restrictions specification")
          exit TRANSFORM_TOKENS
       end if
       token = lexeme_get_contents (lexeme)
       select case (lexeme_get_type (lexeme))
       case (T_NUMERIC)
          newstring = newstring // token
       case (T_IDENTIFIER)
          select case (char (extract (token, 1, 1)))
          case ("+", "~", "&")
             newstring = newstring // token
          case default
             newstring = newstring // canonicalize_prt (token, model)
          end select
       case (T_QUOTED)
          newstring = newstring // canonicalize_prt (token, model)
       case default
          call msg_bug ("Token type error in restrictions specification")
       end select
    end do TRANSFORM_TOKENS
    call stream_final (stream)
  end function canonicalize_restrictions

  function canonicalize_prt (string, model) result (newstring)
    type(string_t) :: newstring
    type(string_t), intent(in) :: string
    type(model_t), intent(in), target :: model
    type(flavor_t) :: flv
    integer :: pdg
    pdg = model_get_particle_pdg (model, string)
    if (pdg == 0) then
       call msg_fatal ("Undefined particle in restrictions specification")
    end if
    call flavor_init (flv, pdg, model)
    newstring = flavor_get_name (flv)
  end function canonicalize_prt

  subroutine process_library_init (prc_lib, name, os_data)
    type(process_library_t), intent(out) :: prc_lib
    type(string_t), intent(in) :: name
    type(os_data_t), intent(in) :: os_data
    prc_lib%basename = name
    prc_lib%srcname = name // os_data%fc_src_ext
    prc_lib%status = STAT_CONFIGURED
  end subroutine process_library_init

  subroutine process_library_clear_configuration (prc_lib)
    type(process_library_t), intent(inout) :: prc_lib
    type(process_configuration_t), pointer :: current
    do while (associated (prc_lib%prc_first))
       current => prc_lib%prc_first
       prc_lib%prc_first => current%next
       deallocate (current)
    end do
    prc_lib%prc_last => null ()
    prc_lib%n_prc = 0
  end subroutine process_library_clear_configuration

  subroutine process_library_final (prc_lib)
    type(process_library_t), intent(inout) :: prc_lib
    if (.not. prc_lib%static)  call dlaccess_final (prc_lib%dlaccess)
    call process_library_clear_configuration (prc_lib)
  end subroutine process_library_final

  subroutine process_library_advance (prc_lib)
    type(process_library_t), pointer :: prc_lib
    prc_lib => prc_lib%next
  end subroutine process_library_advance

  subroutine process_library_write (prc_lib, unit)
    type(process_library_t), intent(in) :: prc_lib
    integer, intent(in), optional :: unit
    type(string_t) :: status
    type(process_configuration_t), pointer :: current
    select case (prc_lib%status)
    case (STAT_UNKNOWN)
       status = "[unknown]"
    case (STAT_CONFIGURED)
       status = "[open]"
    case (STAT_CODE_GENERATED)
       status = "[generated code]"
    case (STAT_COMPILED)
       status = "[compiled]"
    case (STAT_LOADED)
       if (prc_lib%static) then
          status = "[static]"
       else
          status = "[loaded]"
       end if
    end select
    call msg_message ("Process library: " // char (prc_lib%basename) &
         // " " // char (status), unit)
    current => prc_lib%prc_first
    do while (associated (current))
       call process_configuration_write (current, unit)
       current => current%next
    end do
  end subroutine process_library_write

  subroutine process_library_set_static (prc_lib, flag)
    type(process_library_t), intent(inout) :: prc_lib
    logical, intent(in) :: flag
    prc_lib%static = flag
  end subroutine process_library_set_static

  function process_library_is_static (prc_lib) result (flag)
    logical :: flag
    type(process_library_t), intent(in) :: prc_lib
    flag = prc_lib%static
  end function process_library_is_static

  function process_library_is_compiled (prc_lib) result (flag)
    logical :: flag
    type(process_library_t), intent(in) :: prc_lib
    flag = prc_lib%status >= STAT_COMPILED
  end function process_library_is_compiled

  function process_library_is_loaded (prc_lib) result (flag)
    logical :: flag
    type(process_library_t), intent(in) :: prc_lib
    flag = prc_lib%status >= STAT_LOADED
  end function process_library_is_loaded

  function process_library_get_name (prc_lib) result (name)
    type(string_t) :: name
    type(process_library_t), intent(in) :: prc_lib
    name = prc_lib%basename
  end function process_library_get_name

  function process_library_get_n_processes (prc_lib) result (n)
    integer :: n
    type(process_library_t), intent(in) :: prc_lib
    n = prc_lib%n_prc
  end function process_library_get_n_processes

  function process_library_get_process_ptr (prc_lib, prc_id) result (current)
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: current
    current => prc_lib%prc_first
    do while (associated (current))
       if (current%id == prc_id)  return
       current => current%next
    end do
  end function process_library_get_process_ptr

  subroutine process_library_check_name_consistency (prc_id, prc_lib) 
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    if (char (prc_id) == 'prc') & 
         call msg_fatal ("The name 'prc' cannot " // &
                  "be chosen as a valid process name.")  
    if (prc_id == prc_lib%basename) &
        call msg_fatal ("Process and library names must not be identical ('" &
                // char (prc_id) // "').")
  end subroutine process_library_check_name_consistency

  function process_library_get_process_index (prc_lib, prc_id) result (index)
    integer :: index
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: current
    index = 0
    current => prc_lib%prc_first
    do while (associated (current))
       index = index + 1
       if (current%id == prc_id)  return
       current => current%next
    end do
    index = 0
  end function process_library_get_process_index

  subroutine process_library_append &
       (prc_lib, prc_id, model, prt_in, prt_out, method, &
        status, restrictions, rebuild_library, message, known_md5sum)
    type(process_library_t), intent(inout), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(model_t), intent(in), target :: model
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    integer, intent(in), optional :: status, method
    type(string_t), intent(in), optional :: restrictions
    logical, intent(in), optional :: rebuild_library, message
    character(32), intent(in), optional :: known_md5sum
    type(process_configuration_t), pointer :: current
    character(32) :: old_md5sum
    integer :: old_status
    integer :: old_method
    logical :: keep_status
    logical :: msg
    keep_status = .true.;  if (present (rebuild_library))  keep_status = .not. rebuild_library
    msg = .false.;  if (present (message))  msg = message
    current => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (current)) then
       old_md5sum = current%md5sum
       old_status = current%status
       old_method = current%method
       call process_configuration_init &
            (current, prc_id, model, prt_in, prt_out, old_method, status, &
             restrictions, known_md5sum)
       if (size (prt_in) == 0) then
          call msg_warning ("Process '" // char (prc_id) &
               // "': matrix element vanishes in selected model '" &
               // char (model_get_name (model)) // "'")
       else if (keep_status) then
          if (current%md5sum == old_md5sum) then
             if (current%status <= old_status) then
                 call msg_message ("Process '" // char (prc_id) &
                      // "': keeping configuration")
                current%status = old_status
             else
                 call msg_message ("Process '" // char (prc_id) &
                      // "': updating configuration")
             end if
          else
             call msg_warning ("Process '" // char (prc_id) &
                  // "': configuration changed, overwriting.")
          end if
       else
          if (current%md5sum /= old_md5sum) then
             call msg_message ("Process '" // char (prc_id) &
                  // "': ignoring previous configuration")
          end if
       end if
    else
       allocate (current)
       if (associated (prc_lib%prc_last)) then
          prc_lib%prc_last%next => current
       else
          prc_lib%prc_first => current
       end if
       prc_lib%prc_last => current
       prc_lib%n_prc = prc_lib%n_prc + 1
       call process_library_check_name_consistency (prc_id, prc_lib)
       call process_configuration_init &
            (current, prc_id, model, prt_in, prt_out, method, status, &
             restrictions, known_md5sum)
       call process_update_code_status (current, keep_status)
       if (msg)  call msg_message &
            ("Added process to library '" // char (prc_lib%basename) // "':")
    end if
    if (msg)  call process_configuration_write (current)
  end subroutine process_library_append

  subroutine process_update_code_status (prc_conf, keep_status)
    type(process_configuration_t), intent(inout) :: prc_conf
    logical, intent(in) :: keep_status
    type(string_t) :: filename
    logical :: exist, found
    integer :: u, iostat
    character(80) :: buffer
    character(32) :: md5sum 
    filename = prc_conf%id // ".f90"
    inquire (file=char(filename), exist=exist)
    if (exist) then
       found = .false.
       u = free_unit ()
       open (u, file=char(filename), action="read")
       SCAN_FILE: do
          read (u, "(A)", iostat=iostat)  buffer
          select case (iostat)
          case (0)
             select case (buffer(1:12))
             case ("    md5sum =")
                md5sum = buffer(15:47)
                if (keep_status) then
                   if (prc_conf%status < STAT_CODE_GENERATED) then
                      if (md5sum == prc_conf%md5sum) then
                         call msg_message ("Process '" // char (prc_conf%id) &
                              // "': using existing source code")
                         prc_conf%status = STAT_CODE_GENERATED
                      else
                         call msg_warning ("Process '" // char (prc_conf%id) &
                              // "': will overwrite existing source code")
                      end if
                   else if (md5sum /= prc_conf%md5sum) then
                      call msg_warning ("Process '" // char (prc_conf%id) &
                           // "': source code and loaded checksums differ")
                   end if
                else if (prc_conf%status < STAT_CODE_GENERATED) then
                   call msg_message ("Process '" // char (prc_conf%id) &
                        // "': ignoring existing source code")
                end if
                found = .true.
                exit SCAN_FILE
             end select
          case default
             exit SCAN_FILE
          end select
       end do SCAN_FILE
       close (u)
       if (.not. found) &
            call msg_warning ("Process '" // char (prc_conf%id) &
            // "': No MD5 sum found in source code")
    end if
  end subroutine process_update_code_status

  subroutine process_library_update_status (prc_lib)
    type(process_library_t), intent(inout), target :: prc_lib
    type(process_configuration_t), pointer :: prc_conf
    integer :: initial_status
    initial_status = prc_lib%status
    prc_conf => prc_lib%prc_first
    do while (associated (prc_conf))
       prc_lib%status = min (prc_lib%status, prc_conf%status)
       prc_conf => prc_conf%next
    end do
    if (initial_status == STAT_LOADED .and. prc_lib%status < STAT_LOADED) &
         call process_library_unload (prc_lib)
  end subroutine process_library_update_status

  subroutine process_library_load_configuration &
       (prc_lib, os_data, model)
    type(process_library_t), intent(inout), target :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(model_t), pointer :: model
    integer :: n_prc, p, n_flv, n_in, n_out, n_tot, i
    integer(c_int) :: pid
    integer, dimension(:,:), allocatable :: flv_state
    integer(c_int), dimension(:,:), allocatable, target :: flv_state_tmp
    type(string_t) :: prc_id, model_name, filename, restrictions
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    character(32) :: md5sum
    n_prc = prc_lib% get_n_prc ()
    SCAN_PROCESSES: do p = 1, n_prc
       pid = p
       prc_id = process_library_get_process_id (prc_lib, pid)
       md5sum = process_library_get_process_md5sum (prc_lib, pid)
       model_name = process_library_get_process_model_name (prc_lib, pid)
       restrictions = process_library_get_process_restrictions (prc_lib, pid)
       filename = model_name // ".mdl"
       model => null ()
       call model_list_read_model (model_name, filename, os_data, model)
       if (.not. associated (model)) then
          call msg_error ("Process library '" // char (prc_lib%basename) &
               // "', process '" // char (prc_id) // "': " &
               // "model unavailable, process skipped")
          cycle SCAN_PROCESSES
       end if
       n_in  = prc_lib% get_n_in  (pid)
       n_out = prc_lib% get_n_out (pid)
       n_tot = n_in + n_out
       n_flv = prc_lib% get_n_flv (pid)
       allocate (flv_state (n_tot, n_flv))
       allocate (flv_state_tmp (n_tot, n_flv))
       allocate (prt_in  (n_in ))
       allocate (prt_out (n_out))
       call prc_lib% set_flv_state (pid, &
            c_loc (flv_state_tmp), &
            int((/n_tot, n_flv/), kind=c_int))
       flv_state = flv_state_tmp
       do i = 1, n_in
          prt_in(i) = particle_name_string (flv_state (i, :), model)
       end do
       do i = 1, n_out
          prt_out(i) = particle_name_string (flv_state (n_in+i, :), model)
       end do
       call process_library_append &
            (prc_lib, prc_id, model, prt_in, prt_out, &
             status=STAT_LOADED, restrictions=restrictions, &
             known_md5sum=md5sum)
       deallocate (prt_in, prt_out, flv_state, flv_state_tmp)
    end do SCAN_PROCESSES
  contains
    function particle_name_string (ff, model) result (prt)
      type(string_t) :: prt
      integer, dimension(:), intent(in) :: ff
      type(model_t), intent(in), target :: model
      type(flavor_t) :: flv
      integer :: i
      prt = ""
      do i = 1, size (ff)
         if (all (ff(i) /= ff(:i-1))) then
            call flavor_init (flv, ff(i), model)
            if (prt /= "")  prt = prt // ":"
            prt = prt // flavor_get_name (flv)
         end if
      end do
    end function particle_name_string
  end subroutine process_library_load_configuration

  function process_library_get_process_id (prc_lib, pid) result (process_id)
    type(string_t) :: process_id
    type(process_library_t), intent(in), target :: prc_lib
    integer(c_int), intent(in) :: pid
    type(c_ptr) :: cptr
    integer(c_int) :: len
    character(kind=c_char), dimension(:), pointer :: char_array
    integer, dimension(1) :: shape
    call prc_lib% get_process_id (pid, cptr, len)
    if (c_associated (cptr)) then
       shape(1) = len
       call c_f_pointer (cptr, char_array, shape)
       process_id = char_from_array (char_array)
       call prc_lib% get_process_id (0_c_int, cptr, len)
    else
       process_id = ""
    end if
  end function process_library_get_process_id

  function process_library_get_process_pid (prc_lib, id) result (process_pid)
     type(process_library_t), intent(in) :: prc_lib
     type(string_t), intent(in) :: id
     integer :: process_pid, pid, n_proc
     process_pid = -1
     n_proc = process_library_get_n_processes (prc_lib)
     if (n_proc <= 0) return
     do pid = 1, n_proc
        if (process_library_get_process_id (prc_lib, pid) == id) then
           process_pid = pid
           return
        end if
     end do
  end function process_library_get_process_pid

  function process_library_get_process_model_name &
       (prc_lib, pid) result (model_name)
    type(string_t) :: model_name
    type(process_library_t), intent(in), target :: prc_lib
    integer(c_int), intent(in) :: pid
    type(c_ptr) :: cptr
    integer(c_int) :: len
    character(kind=c_char), dimension(:), pointer :: char_array
    integer, dimension(1) :: shape
    call prc_lib% get_model_name (pid, cptr, len)
    if (c_associated (cptr)) then
       shape(1) = len
       call c_f_pointer (cptr, char_array, shape)
       model_name = char_from_array (char_array)
       call prc_lib% get_model_name (0_c_int, cptr, len)
    else
       model_name = ""
    end if
  end function process_library_get_process_model_name

  function process_library_get_process_restrictions &
       (prc_lib, pid) result (restrictions)
    type(string_t) :: restrictions
    type(process_library_t), intent(in), target :: prc_lib
    integer(c_int), intent(in) :: pid
    type(c_ptr) :: cptr
    integer(c_int) :: len
    character(kind=c_char), dimension(:), pointer :: char_array
    integer, dimension(1) :: shape
    call prc_lib% get_restrictions (pid, cptr, len)
    if (c_associated (cptr)) then
       shape(1) = len
       call c_f_pointer (cptr, char_array, shape)
       restrictions = char_from_array (char_array)
       call prc_lib% get_restrictions (0_c_int, cptr, len)
    else
       restrictions = ""
    end if
  end function process_library_get_process_restrictions

  function process_library_get_process_md5sum (prc_lib, pid) result (md5sum)
    type(string_t) :: md5sum
    type(process_library_t), intent(in), target :: prc_lib
    integer(c_int), intent(in) :: pid
    type(c_ptr) :: cptr
    integer(c_int) :: len
    character(kind=c_char), dimension(:), pointer :: char_array
    integer, dimension(1) :: shape
    call prc_lib% get_md5sum (pid, cptr, len)
    if (c_associated (cptr)) then
       shape(1) = len
       call c_f_pointer (cptr, char_array, shape)
       md5sum = char_from_array (char_array)
       call prc_lib% get_md5sum (0_c_int, cptr, len)
    else
       md5sum = ""
    end if
  end function process_library_get_process_md5sum

  function char_from_array (a) result (char)
    character(kind=c_char), dimension(:), intent(in) :: a
    character(len=size(a)) :: char
    integer :: i
    do i = 1, len (char)
       char(i:i) = a(i)
    end do
  end function char_from_array

  subroutine process_library_generate_code (prc_lib, os_data, simulate)
    type(process_library_t), intent(in) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: simulate
    type(process_configuration_t), pointer :: current
    integer :: status
    call msg_message ("Generating code for process library '" &
         // char (process_library_get_name (prc_lib)) // "'")
    current => prc_lib%prc_first
    SCAN_PROCESSES: do while (associated (current))
       select case (current%status)
       case (STAT_CONFIGURED)
          select case (current%method)
          case (PRC_OMEGA)
             call call_omega (current, os_data, status, simulate)
             if (status == 0) then
                current%status = STAT_CODE_GENERATED
             else
                call msg_error ("Process '" // char (current%id) &
                     // "': code generation failed")
             end if
          case (PRC_TEST)
             call write_unit_matrix_element (current, os_data, status, unit=.false.)
             if (status == 0) then
                current%status = STAT_CODE_GENERATED
             else
                call msg_error ("Process '" // char (current%id) &
                     // "': code generation failed")
             end if          
          case (PRC_UNIT)
             call write_unit_matrix_element (current, os_data, status, unit=.true.)
             if (status == 0) then
                current%status = STAT_CODE_GENERATED
             else
                call msg_error ("Process '" // char (current%id) &
                     // "': code generation failed")
             end if          
          case default
             call msg_fatal ("These methods are not yet implemented.")
          end select
       case (STAT_CODE_GENERATED:)
          call msg_message ("Skipping process '" // char (current%id) &
               // "' (source code exists)")
       case default
          call msg_message ("Skipping process '" // char (current%id) &
               // "' (undefined configuration)")
       end select
       current => current%next
    end do SCAN_PROCESSES
  end subroutine process_library_generate_code

  subroutine call_omega (prc_conf, os_data, status, simulate)
    type(process_configuration_t), intent(in) :: prc_conf
    type(os_data_t), intent(in) :: os_data
    integer, intent(out) :: status
    logical, intent(in), optional :: simulate
    type(string_t) :: command_string, binary_name
    type(string_t) :: model_id, omega_mode, omega_cascade
    integer :: j
    logical :: sim, binary_found
    sim = .false.;  if (present (simulate))  sim = simulate
    call msg_message ("Calling O'Mega for process '" &
         // char (prc_conf%id) // "'")
    model_id = model_get_name (prc_conf%model)
    binary_name = "omega_" // model_id // ".opt"
    binary_found = .false.
    if (.not. os_data%use_testfiles) then
       command_string = os_data%whizard_omega_binpath_local &
          // "/" // binary_name
       inquire (file=char (command_string), exist=binary_found)
    end if
    if (.not. binary_found) then
       command_string = os_data%whizard_omega_binpath // "/" // binary_name
       inquire (file=char (command_string), exist=binary_found)
    end if
    if (.not. binary_found) &
       call msg_fatal ("O'Mega binary """ // char (binary_name) // """ not found")
    select case (prc_conf%n_in)
    case (1);  omega_mode = "-decay"
    case (2);  omega_mode = "-scatter"
    end select
    if (prc_conf%restrictions == "") then
       omega_cascade = ""
    else
       omega_cascade = " -cascade '" // prc_conf%restrictions // "'"
    end if
    command_string = command_string &
         // " -o " // prc_conf%id // ".f90" &
         // " -target:whizard" &
         // " -target:parameter_module parameters_" // model_id &
         // " -target:module " // prc_conf%id &
         // " -target:md5sum " // prc_conf%md5sum &
         // omega_cascade &
         // " -fusion:progress" &
         // " " // omega_mode
    command_string = command_string // " "
    do j = 1, prc_conf%n_in
       if (j == 1) then
          command_string = command_string // "'"
       else
          command_string = command_string // " "
       end if
       command_string = command_string // prc_conf%prt_in(j)
    end do
    command_string = command_string // " ->"
    do j = 1, prc_conf%n_out
       command_string = command_string &
            // " " // prc_conf%prt_out(j)
    end do
    command_string = command_string // "'"
    if (sim) then
       command_string = "cp " // os_data%whizard_testdatapath // "/" &
            // prc_conf%id // ".f90 ."
       call msg_message ("[call not executed, instead: copy file from " &
            // char (os_data%whizard_testdatapath) // "]")
    end if
    call os_system_call (command_string, status, verbose=.true.)
  end subroutine call_omega

  subroutine write_unit_matrix_element (prc_conf, os_data, status, unit)
    type(process_configuration_t), intent(in) :: prc_conf
    type(os_data_t), intent(in) :: os_data
    integer, intent(out) :: status
    logical, intent(in) :: unit
    integer, dimension(prc_conf%n_in) :: prt_in, mult_in
    type(flavor_t), dimension(1:prc_conf%n_in) :: flv_in    
    integer, dimension(prc_conf%n_out) :: prt_out, mult_out
    integer, dimension(prc_conf%n_tot) :: prt, mult
    integer, dimension(:,:), allocatable :: sxxx
    integer :: dummy
    type(flavor_t), dimension(1:prc_conf%n_out) :: flv_out    
    type(string_t) :: proc_str, comment_str
    integer :: u, i, j, count
    integer :: hel, hel_in, hel_out, fac, factor
    type(string_t) :: filename
    comment_str = ""
    do i = 1, prc_conf%n_in
       comment_str = comment_str // prc_conf%prt_in(i) // " " 
    end do   
    do j = 1, prc_conf%n_out
       comment_str = comment_str // prc_conf%prt_out(j) // " " 
    end do       
    do i = 1, prc_conf%n_in
       prt_in(i) = model_get_particle_pdg (prc_conf%model, prc_conf%prt_in(i))
       call flavor_init (flv_in(i), prt_in(i), prc_conf%model)
       mult_in(i) = flavor_get_multiplicity (flv_in(i))
       mult(i) = mult_in(i)
       end do
    do j = 1, prc_conf%n_out
       prt_out(j) = model_get_particle_pdg (prc_conf%model, prc_conf%prt_out(j))    
       call flavor_init (flv_out(j), prt_out(j), prc_conf%model)       
       mult_out(j) = flavor_get_multiplicity (flv_out(j))       
       mult(prc_conf%n_in + j) = mult_out(j)
       end do
    prt(1:prc_conf%n_in) = prt_in(1:prc_conf%n_in)
    prt(prc_conf%n_in+1:prc_conf%n_tot) = prt_out(1:prc_conf%n_out)
    proc_str = converter (prt)
    hel_in = product (mult_in)
    hel_out = product (mult_out)
    hel = hel_in * hel_out
    fac = hel
    dummy = 1
    factor = 1
    if (prc_conf%n_out >= 3) then
       do i = 3, prc_conf%n_out
          factor = factor * (i - 2) * (i - 1)
       end do
    end if    
    allocate (sxxx(1:hel,1:prc_conf%n_tot))
    call create_spin_table (dummy,hel,fac,mult,sxxx)
    call msg_message ("Writing test matrix element for process '" &
         // char (prc_conf%id) // "'")
    filename = prc_conf%id // ".f90"
    u = free_unit ()
    open (unit=u, file=char(filename), action="write")
    write (u, "(A)") "! File generated automatically by WHIZARD"   
    write (u, "(A)") "!                                        "
    write (u, "(A)") "! Note that irresp. of what you demanded WHIZARD"
    write (u, "(A)") "! treats this as colorless process       "    
    write (u, "(A)") "!                                        "
    write (u, "(A)") "module " // char(prc_conf%id)
    write (u, "(A)") "                                         "
    write (u, "(A)") "  use kinds"    
    write (u, "(A)") "  use omega_color, OCF => omega_color_factor"        
    write (u, "(A)") "                                         "
    write (u, "(A)") "  implicit none"        
    write (u, "(A)") "  private"            
    write (u, "(A)") "                                         "    
    write (u, "(A)") "  public :: md5sum"        
    write (u, "(A)") "  public :: number_particles_in, number_particles_out"       
    write (u, "(A)") "  public :: number_spin_states, spin_states"
    write (u, "(A)") "  public :: number_flavor_states, flavor_states"
    write (u, "(A)") "  public :: number_color_flows, color_flows"
    write (u, "(A)") "  public :: number_color_indices, number_color_factors, &"
    write (u, "(A)") "     color_factors, color_sum"
    write (u, "(A)") "  public :: init, final"  
    write (u, "(A)") "  public :: reset_helicity_selection"
    write (u, "(A)") "                                         "    
    write (u, "(A)") "  public :: new_event, is_allowed, get_amplitude"        
    write (u, "(A)") "       "      
    write (u, "(A)") "  real(default), parameter :: &"
    write (u, "(A)") "       & conv = 0.38937966e12_default"
    write (u, "(A)") "       "
    write (u, "(A)") "  real(default), parameter :: &"
    write (u, "(A)") "       & pi = 3.1415926535897932384626433832795028841972_default"
    write (u, "(A)") "       "
    write (u, "(A)") "  real(default), parameter :: &"
    if (unit) then
       write (u, "(A)") "                   & const = 1"
    else 
       write (u, "(A,1x,I0,A)") "       & const = (16 * pi / conv) * " &
          // "(16 * pi**2)**(", prc_conf%n_out, "-2) " 
    end if
    write (u, "(A)") "       "    
    write (u, "(A,1x,I0)") "  integer, parameter, private :: n_prt =  ", &
       prc_conf%n_tot
    write (u, "(A,1x,I0)") "  integer, parameter, private :: n_in = ", &
       prc_conf%n_in
    write (u, "(A,1x,I0)") "  integer, parameter, private :: n_out = ", &
       prc_conf%n_out
    write (u, "(A)") "  integer, parameter, private :: n_cflow = 1"
    write (u, "(A)") "  integer, parameter, private :: n_cindex = 2"
    write (u, "(A)") "  !!! We ignore tensor products and take only one flavor state."
    write (u, "(A)") "  integer, parameter, private :: n_flv = 1"
    write (u, "(A,1x,I0)") "  integer, parameter, private :: n_hel = ", hel
    write (u, "(A)") "                                           "
    write (u, "(A)") "  logical, parameter, private :: T = .true."
    write (u, "(A)") "  logical, parameter, private :: F = .false."
    write (u, "(A)") "                                           "    
    do i = 1, hel
       write (u, "(A)") "  integer, dimension(n_prt), parameter, private :: &"
       write (u, "(A)") "    " // s_conv(i) // " = (/ " // char(converter(sxxx(i,1:prc_conf%n_tot))) // " /)"
    end do 
    write (u, "(A)") "  integer, dimension(n_prt,n_hel), parameter, private :: table_spin_states = &"
    write (u, "(A)") "    reshape ( (/ & "
    do i = 1, hel-1
       write (u, "(A)") "                 " // s_conv(i) // ", & " 
    end do 
    write (u, "(A)") "                 " // s_conv(hel) // " & "     
    write (u, "(A)") "              /), (/ n_prt, n_hel /) )"
    write (u, "(A)") "                                                 "
    write (u, "(A)") "  integer, dimension(n_prt), parameter, private :: &"
    write (u, "(A)") "    f0001 = (/ " // char(proc_str) // " /)   !  " // char(comment_str)
    write (u, "(A)") "  integer, dimension(n_prt,n_flv), parameter, private :: table_flavor_states = &"
    write (u, "(A)") "    reshape ( (/ f0001 /), (/ n_prt, n_flv /) )"
    write (u, "(A)") "                                                 " 
    write (u, "(A)") "  integer, dimension(n_cindex, n_prt), parameter, private :: &"
    write (u, "(A)") "    c0001 = reshape ( (/ " // (repeat ("0,0, ", prc_conf%n_tot-1)) &
                             // "0,0 /), " // " (/ n_cindex, n_prt /) )"
    write (u, "(A)") "  integer, dimension(n_cindex, n_prt, n_cflow), parameter, private :: &"
    write (u, "(A)") "  table_color_flows = reshape ( (/ c0001 /), (/ n_cindex, n_prt, n_cflow /) )"
    write (u, "(A)") "                                           "   
    write (u, "(A)") "  logical, dimension(n_prt), parameter, private :: & "
    write (u, "(A)") "    g0001 = (/ "  // (repeat ("F, ", prc_conf%n_tot-1)) // "F /) "
    write (u, "(A)") "  logical, dimension(n_prt, n_cflow), parameter, private :: table_ghost_flags = &"
    write (u, "(A)") "    reshape ( (/ g0001 /), (/ n_prt, n_cflow /) )"
    write (u, "(A)") "                                           "   
    write (u, "(A)") "  integer, parameter, private :: n_cfactors = 1"
    write (u, "(A)") "  type(OCF), dimension(n_cfactors), parameter, private :: &"
    write (u, "(A)") "    table_color_factors = (/  OCF(1,1,+1._default) /)"
    write (u, "(A)") "                                           "   
    write (u, "(A)") "  logical, dimension(n_flv), parameter, private :: a0001 = (/ T /)"   
    write (u, "(A)") "  logical, dimension(n_flv, n_cflow), parameter, private :: &"   
    write (u, "(A)") "    flv_col_is_allowed = reshape ( (/ a0001 /), (/ n_flv, n_cflow /) )"   
    write (u, "(A)") "                                           "   
    write (u, "(A)") "  complex(default), dimension (n_flv, n_hel, n_cflow), private, save :: amp"    
    write (u, "(A)") "                                           "
    write (u, "(A)") "  logical, dimension(n_hel), private, save :: hel_is_allowed = T"
    write (u, "(A)") "                                           "
    write (u, "(A)") "contains"          
    write (u, "(A)") "                                           "      
    write (u, "(A)") "  pure function md5sum ()"
    write (u, "(A)") "    character(len=32) :: md5sum"    
    write (u, "(A)") "    ! DON'T EVEN THINK of modifying the following line!"        
    write (u, "(A)") "    md5sum = """ // prc_conf%md5sum // """"
    write (u, "(A)") "  end function md5sum"
    write (u, "(A)") "                                           "          
    write (u, "(A)") "  subroutine init (par)"
    write (u, "(A)") "    real(default), dimension(*), intent(in) :: par"    
    write (u, "(A)") "  end subroutine init"    
    write (u, "(A)") "                                           " 
    write (u, "(A)") "  subroutine final ()" 
    write (u, "(A)") "  end subroutine final" 
    write (u, "(A)") "                                           " 
    write (u, "(A)") "  pure function number_particles_in () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = n_in"
    write (u, "(A)") "  end function number_particles_in"
    write (u, "(A)") "                                           "              
    write (u, "(A)") "  pure function number_particles_out () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = n_out"
    write (u, "(A)") "  end function number_particles_out"
    write (u, "(A)") "                                           "                  
    write (u, "(A)") "  pure function number_spin_states () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = size (table_spin_states, dim=2)"
    write (u, "(A)") "  end function number_spin_states"
    write (u, "(A)") "                                           "                      
    write (u, "(A)") "  pure subroutine spin_states (a)"
    write (u, "(A)") "    integer, dimension(:,:), intent(out) :: a"    
    write (u, "(A)") "    a = table_spin_states"
    write (u, "(A)") "  end subroutine spin_states"    
    write (u, "(A)") "                                           "                          
    write (u, "(A)") "  pure function number_flavor_states () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = 1"
    write (u, "(A)") "  end function number_flavor_states"
    write (u, "(A)") "                                           "                      
    write (u, "(A)") "  pure subroutine flavor_states (a)"
    write (u, "(A)") "    integer, dimension(:,:), intent(out) :: a"    
    write (u, "(A)") "    a = table_flavor_states"
    write (u, "(A)") "  end subroutine flavor_states"
    write (u, "(A)") "                                           "                          
    write (u, "(A)") "  pure function number_color_indices () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = size(table_color_flows, dim=1)"
    write (u, "(A)") "  end function number_color_indices"
    write (u, "(A)") "                                           "                          
    write (u, "(A)") "  pure subroutine color_factors (cf)"
    write (u, "(A)") "    type(OCF), dimension(:), intent(out) :: cf"    
    write (u, "(A)") "    cf = table_color_factors"
    write (u, "(A)") "  end subroutine color_factors"
    write (u, "(A)") "                                           "                              
    write (u, "(A)") "  pure function color_sum (flv, hel) result (amp2)"
    write (u, "(A)") "    integer, intent(in) :: flv, hel"
    write (u, "(A)") "    real(kind=default) :: amp2"
    write (u, "(A)") "    amp2 = real (omega_color_sum (flv, hel, amp, table_color_factors))"
    write (u, "(A)") "  end function color_sum"
    write (u, "(A)") "                                           "       
    write (u, "(A)") "  pure function number_color_flows () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = size (table_color_flows, dim=3)"
    write (u, "(A)") "  end function number_color_flows"
    write (u, "(A)") "                                           "                                  
    write (u, "(A)") "  pure subroutine color_flows (a, g)"
    write (u, "(A)") "    integer, dimension(:,:,:), intent(out) :: a"
    write (u, "(A)") "    logical, dimension(:,:), intent(out) :: g"
    write (u, "(A)") "    a = table_color_flows"
    write (u, "(A)") "    g = table_ghost_flags"
    write (u, "(A)") "  end subroutine color_flows"    
    write (u, "(A)") "                                           "                              
    write (u, "(A)") "  pure function number_color_factors () result (n)"
    write (u, "(A)") "    integer :: n"    
    write (u, "(A)") "    n = size (table_color_factors)"
    write (u, "(A)") "  end function number_color_factors"
    write (u, "(A)") "                                           "                                  
    write (u, "(A)") "  subroutine new_event (p)"
    write (u, "(A)") "    real(default), dimension(0:3,*), intent(in) :: p"    
    write (u, "(A)") "    call calculate_amplitudes (amp, p)"        
    write (u, "(A)") "  end subroutine new_event"
    write (u, "(A)") "                                           "              
    write (u, "(A)") "  subroutine reset_helicity_selection (threshold, cutoff)"
    write (u, "(A)") "    real(default), intent(in) :: threshold"    
    write (u, "(A)") "    integer, intent(in) :: cutoff"
    write (u, "(A)") "  end subroutine reset_helicity_selection"
    write (u, "(A)") "                                           "                  
    write (u, "(A)") "  pure function is_allowed (flv, hel, col) result (yorn)"                  
    write (u, "(A)") "    logical :: yorn"                  
    write (u, "(A)") "    integer, intent(in) :: flv, hel, col"                  
    write (u, "(A)") "    yorn = hel_is_allowed(hel) .and. flv_col_is_allowed(flv,col)"                  
    write (u, "(A)") "  end function is_allowed"                     
    write (u, "(A)") "                                           "                  
    write (u, "(A)") "  pure function get_amplitude (flv, hel, col) result (amp_result)"
    write (u, "(A)") "    complex(default) :: amp_result"    
    write (u, "(A)") "    integer, intent(in) :: flv, hel, col"            
    write (u, "(A)") "    amp_result = amp (flv, hel, col)"        
    write (u, "(A)") "  end function get_amplitude"
    write (u, "(A)") "                                           "                  
    write (u, "(A)") "  pure subroutine calculate_amplitudes (amp, k)"
    write (u, "(A)") "    complex(default), dimension(:,:,:), intent(out) :: amp"    
    write (u, "(A)") "    real(default), dimension(0:3,*), intent(in) :: k"    
    write (u, "(A)") "    real(default) :: fac"        
    write (u, "(A)") "    integer :: i"            
    write (u, "(A)") "    ! We give all helicities the same weight!"            
    if (unit) then 
       write (u, "(A)") "    amp = const"
    else
       write (u, "(A,1x,I0,1x,A)") "    fac = ", factor 
       write (u, "(A)") "    amp = sqrt((2 * (k(0,1)*k(0,2) &"
       write (u, "(A,1x,I0,A)") "         - dot_product (k(1:,1), k(1:,2)))) ** (3-", &
                                  prc_conf%n_out, ")) * sqrt(const * fac)"
    end if                                  
    write (u, "(A,1x,I0,A)") "    amp = amp / sqrt(", hel_out, "._default)"
    write (u, "(A)") "  end subroutine calculate_amplitudes"
    write (u, "(A)") "                                           "                  
    write (u, "(A)") "end module " // char(prc_conf%id)    
    close (u, iostat=status)
    deallocate (sxxx)
  contains
    function s_conv (num) result (chrt)
      integer, intent(in) :: num
      character(len=10) :: chrt
      write (chrt, "(I10)") num
      chrt = trim(adjustl(chrt))
      if (num < 10) then
         chrt = "s000" // chrt
      else if (num < 100) then
         chrt = "s00" // chrt
      else if (num < 1000) then 
         chrt = "s0" // chrt     
      else
         chrt = "s" // chrt            
      end if             
    end function s_conv
    function converter (flv) result (str)
      integer, dimension(:), intent(in) :: flv
      type(string_t) :: str
      character(len=150), dimension(size(flv)) :: chrt
      integer :: i
      str = ""
      do i = 1, size(flv) - 1
         write (chrt(i), "(I10)") flv(i)
         str = str // var_str(trim(adjustl(chrt(i)))) // ", "
      end do    
      write (chrt(size(flv)), "(I10)") flv(size(flv))
      str = str // trim(adjustl(chrt(size(flv))))
    end function converter
    integer function sj (j,m)
      integer, intent(in) :: j, m
      if (((j == 1) .and. (m == 1)) .or. &
          ((j == 2) .and. (m == 2)) .or. &
          ((j == 3) .and. (m == 3)) .or. &
          ((j == 4) .and. (m == 3)) .or. &
          ((j == 5) .and. (m == 4))) then
         sj = 1
      else if (((j == 2) .and. (m == 1)) .or. &
          ((j == 3) .and. (m == 1)) .or. &         
          ((j == 4) .and. (m == 2)) .or. &
          ((j == 5) .and. (m == 2))) then
         sj = -1
      else if (((j == 3) .and. (m == 2)) .or. &
          ((j == 5) .and. (m == 3))) then
         sj = 0
      else if (((j == 4) .and. (m == 1)) .or. &
          ((j == 5) .and. (m == 1))) then         
         sj = -2
      else if (((j == 4) .and. (m == 4)) .or. &
          ((j == 5) .and. (m == 5))) then         
         sj = 2
      else
         call msg_fatal ("Write_unit_matrix_element: Wrong spin type")
      end if
    end function sj    
    recursive subroutine create_spin_table (index, nhel, fac, mult, inta)
      integer, intent(inout) :: index, fac
      integer, intent(in) :: nhel
      integer, dimension(:), intent(in) :: mult
      integer, dimension(nhel,size(mult)), intent(out) :: inta    
      integer :: i, j
      if (index > size(mult)) return
      fac = fac / mult(index)
      do j = 1, nhel 
         inta(j,index) = sj (mult(index),mod(((j-1)/fac),mult(index))+1)
      end do   
      index = index + 1
      call create_spin_table (index, nhel, fac, mult, inta)
    end subroutine create_spin_table    
  end subroutine write_unit_matrix_element

  subroutine process_library_write_driver (prc_lib)

    type(process_library_t), intent(inout) :: prc_lib
    type(string_t) :: filename, prefix
    type(string_t), dimension(:), allocatable :: prc_id, model, restrictions
    integer, dimension(:), allocatable :: n_par
    character(32), dimension(:), allocatable :: md5sum
    type(process_configuration_t), pointer :: current
    integer :: u, i, n_prc

    call msg_message ("Writing interface code for process library '" // &
         char (process_library_get_name (prc_lib)) // "'")
    prefix = prc_lib%basename // "_"

    n_prc = prc_lib%n_prc
    allocate (prc_id (n_prc), model (n_prc), restrictions (n_prc))
    allocate (n_par (n_prc), md5sum (n_prc))
    current => prc_lib%prc_first
    do i = 1, n_prc
       prc_id(i) = current%id
       model(i) = model_get_name (current%model)
       restrictions(i) = current%restrictions
       n_par(i) = model_get_n_parameters (current%model)
       md5sum(i) = current%md5sum
       current => current%next
    end do
    filename = prc_lib%basename // "_interface.f90"
    u = free_unit ()
    open (unit=u, file=char(prc_lib%basename // ".f90"), action="write")
    write (u, "(A)")  "! WHIZARD process interface"
    write (u, "(A)")  "!"
    write (u, "(A)")  "! Automatically generated file, do not edit"
    call write_get_n_processes_fun ()
    call write_get_process_id_fun ()
    call write_get_model_name_fun ()
    call write_get_restrictions_fun ()
    call write_get_md5sum_fun ()
    call write_string_to_array_fun ()
    call write_get_int_fun ("n_in",  "number_particles_in")
    call write_get_int_fun ("n_out", "number_particles_out")
    call write_get_int_fun ("n_flv", "number_flavor_states")
    call write_get_int_fun ("n_hel", "number_spin_states")
    call write_get_int_fun ("n_col", "number_color_flows")
    call write_get_int_fun ("n_cin", "number_color_indices")
    call write_get_int_fun ("n_cf",  "number_color_factors")
    call write_set_int_sub1 ("flv_state", "flavor_states")
    call write_set_int_sub1 ("hel_state", "spin_states")
    call write_set_int_sub2 ("col_state", "color_flows", "ghost_flag")
    call write_set_cf_tab_sub ()
    call write_init_get_fptr ()
    call write_final_get_fptr ()
    call write_update_alpha_s_get_fptr ()
    call write_reset_helicity_selection_get_fptr ()
    call write_new_event_get_fptr ()
    call write_is_allowed_get_fptr ()
    call write_get_amplitude_get_fptr ()
    close (u)

    prc_lib%status = max (prc_lib%status, STAT_CODE_GENERATED)

  contains
    
    subroutine write_get_n_processes_fun ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the number of processes in this library"
      write (u, "(A)")  "function " // char (prefix) &
           // "get_n_processes () result (n) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int) :: n"
      write (u, "(A,I0)")  "  n = ", n_prc
      write (u, "(A)")  "end function " // char (prefix) &
           // "get_n_processes"
    end subroutine write_get_n_processes_fun

    subroutine write_get_process_id_fun ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the process ID of process #i (as a C pointer to a character array)"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "get_process_id (i, cptr, len) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: i"
      write (u, "(A)")  "  type(c_ptr), intent(inout) :: cptr"
      write (u, "(A)")  "  integer(c_int), intent(out) :: len"
      write (u, "(A)")  "  character(kind=c_char), dimension(:), allocatable, target, save :: a"
      call write_string_to_array_interface ()
      write (u, "(A)")  "  select case (i)"
      write (u, "(A)")  "  case (0);  if (allocated (a))  deallocate (a)"
      do i = 1, n_prc
         write (u, "(A,I0,A)")  "  case (", i, ");  " &
              // "call " // char (prefix) &
              // "string_to_array ('" // char (prc_id(i)) // "', a)"
      end do
      write (u, "(A)")  "  end select"
      write (u, "(A)")  "  if (allocated (a)) then"
      write (u, "(A)")  "     cptr = c_loc (a)"
      write (u, "(A)")  "     len = size (a)"
      write (u, "(A)")  "  else"
      write (u, "(A)")  "     cptr = c_null_ptr"
      write (u, "(A)")  "     len = 0"
      write (u, "(A)")  "  end if"
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "get_process_id"
    end subroutine write_get_process_id_fun

    subroutine write_get_model_name_fun ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the model name for process #i (as a C pointer to a character array)"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "get_model_name (i, cptr, len) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: i"
      write (u, "(A)")  "  type(c_ptr), intent(inout) :: cptr"
      write (u, "(A)")  "  integer(c_int), intent(out) :: len"
      write (u, "(A)")  "  character(kind=c_char), dimension(:), allocatable, target, save :: a"
      call write_string_to_array_interface ()
      write (u, "(A)")  "  select case (i)"
      write (u, "(A)")  "  case (0);  if (allocated (a))  deallocate (a)"
      do i = 1, n_prc
         write (u, "(A,I0,A)")  "  case (", i, ");  " &
              // "call " // char (prefix) &
              // "string_to_array ('" // char (model(i)) // "', a)"
      end do
      write (u, "(A)")  "  end select"
      write (u, "(A)")  "  if (allocated (a)) then"
      write (u, "(A)")  "     cptr = c_loc (a)"
      write (u, "(A)")  "     len = size (a)"
      write (u, "(A)")  "  else"
      write (u, "(A)")  "     cptr = c_null_ptr"
      write (u, "(A)")  "     len = 0"
      write (u, "(A)")  "  end if"
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "get_model_name"
    end subroutine write_get_model_name_fun

    subroutine write_get_restrictions_fun ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the model name for process #i (as a C pointer to a character array)"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "get_restrictions (i, cptr, len) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: i"
      write (u, "(A)")  "  type(c_ptr), intent(inout) :: cptr"
      write (u, "(A)")  "  integer(c_int), intent(out) :: len"
      write (u, "(A)")  "  character(kind=c_char), dimension(:), allocatable, target, save :: a"
      call write_string_to_array_interface ()
      write (u, "(A)")  "  select case (i)"
      write (u, "(A)")  "  case (0);  if (allocated (a))  deallocate (a)"
      do i = 1, n_prc
         write (u, "(A,I0,A)")  "  case (", i, ");  " &
              // "call " // char (prefix) &
              // "string_to_array ('" // char (restrictions(i)) // "', a)"
      end do
      write (u, "(A)")  "  end select"
      write (u, "(A)")  "  if (allocated (a)) then"
      write (u, "(A)")  "     cptr = c_loc (a)"
      write (u, "(A)")  "     len = size (a)"
      write (u, "(A)")  "  else"
      write (u, "(A)")  "     cptr = c_null_ptr"
      write (u, "(A)")  "     len = 0"
      write (u, "(A)")  "  end if"
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "get_restrictions"
    end subroutine write_get_restrictions_fun

    subroutine write_get_md5sum_fun ()
      integer :: i
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the MD5 sum for the process configuration (as a C pointer to a character array)"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "get_md5sum (i, cptr, len) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      call write_use_lines ("md5sum", "md5sum")
      write (u, "(A)")  "  integer(c_int), intent(in) :: i"
      write (u, "(A)")  "  type(c_ptr), intent(inout) :: cptr"
      write (u, "(A)")  "  integer(c_int), intent(out) :: len"
      write (u, "(A)")  "  character(kind=c_char), dimension(:), allocatable, target, save :: a"
      call write_string_to_array_interface ()
      write (u, "(A)")  "  select case (i)"
      write (u, "(A)")  "  case (0);  if (allocated (a))  deallocate (a)"
      do i = 1, n_prc
         write (u, "(A,I0,A)")  "  case (", i, ");  " &
              // "call " // char (prefix) &
              // "string_to_array (" // char (prc_id(i)) &
              // "_md5sum (), a)"
      end do
      write (u, "(A)")  "  end select"
      write (u, "(A)")  "  if (allocated (a)) then"
      write (u, "(A)")  "     cptr = c_loc (a)"
      write (u, "(A)")  "     len = size (a)"
      write (u, "(A)")  "  else"
      write (u, "(A)")  "     cptr = c_null_ptr"
      write (u, "(A)")  "     len = 0"
      write (u, "(A)")  "  end if"
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "get_md5sum"
    end subroutine write_get_md5sum_fun

    subroutine write_string_to_array_interface ()
      write (u, "(2x,A)")  "interface"
      write (u, "(5x,A)")  "subroutine " // char (prefix) &
           // "string_to_array (string, a)"
      write (u, "(5x,A)")  "  use iso_c_binding"
      write (u, "(5x,A)")  "  character(*), intent(in) :: string"
      write (u, "(5x,A)")  "  character(kind=c_char), dimension(:), allocatable, intent(out) :: a"
      write (u, "(5x,A)")  "end subroutine " // char (prefix) &
           // "string_to_array"
      write (u, "(2x,A)")  "end interface"
    end subroutine write_string_to_array_interface

    subroutine write_string_to_array_fun ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Auxiliary: convert character string to array pointer"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "string_to_array (string, a)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  character(*), intent(in) :: string"
      write (u, "(A)")  "  character(kind=c_char), dimension(:), allocatable, intent(out) :: a"
      write (u, "(A)")  "  integer :: i"
      write (u, "(A)")  "  allocate (a (len (string)))"
      write (u, "(A)")  "  do i = 1, size (a)"
      write (u, "(A)")  "     a(i) = string(i:i)"
      write (u, "(A)")  "  end do"
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "string_to_array"
    end subroutine write_string_to_array_fun

    subroutine write_get_int_fun (vname, fname)
      character(*), intent(in) :: vname, fname
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return the value of " // vname
      write (u, "(A)")  "function " // char (prefix) &
           // "get_" // vname // " (pid)" &
           // " result (" // vname // ") bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      call write_use_lines (vname, fname)
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  integer(c_int) :: " // vname
      call write_case_lines (vname // " = ", "_" // vname // " ()")
      write (u, "(A)")  "end function " // char (prefix) &
           // "get_" // vname
    end subroutine write_get_int_fun

    subroutine write_set_int_sub1 (vname, fname)
      character(*), intent(in) :: vname, fname
      write (u, "(A)")  ""
      write (u, "(A)")  "! Set table: " // vname
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "set_" // vname &
           // " (pid, cptr, shape) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      call write_use_lines (vname, fname)
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_ptr), intent(in) :: cptr"
      write (u, "(A)")  "  integer(c_int), dimension(2), intent(in) :: shape"
      write (u, "(A)")  "  integer(c_int), dimension(:,:), pointer :: " // vname
      if (kind(1) /= c_int) then
         write (u, "(A)")  "  integer, dimension(:,:), allocatable :: " &
              // vname // "_tmp"
      end if
      write (u, "(A)")  "  call c_f_pointer (cptr, " // vname // ", shape)"
      if (kind(1) == c_int) then
         call write_case_lines ("call ", "_" // vname // " (" // vname // ")")
      else
         write (u, "(A)")  "  allocate (" &
              // vname // "_tmp (shape(1), shape(2)))"
         call write_case_lines ("call ", &
              "_" // vname // " (" // vname // "_tmp)")
         write (u, "(A)")  "  " // vname // " = " // vname // "_tmp"
      end if
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "set_" // vname
    end subroutine write_set_int_sub1

    subroutine write_set_int_sub2 (vname, fname, lname)
      character(*), intent(in) :: vname, fname, lname
      write (u, "(A)")  ""
      write (u, "(A)")  "! Set tables: " // vname // ", " // lname
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "set_" // vname &
           // " (pid, cptr, shape, lcptr, lshape) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      call write_use_lines (vname, fname)
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_ptr), intent(in) :: cptr"
      write (u, "(A)")  "  integer(c_int), dimension(3), intent(in) :: shape"
      write (u, "(A)")  "  type(c_ptr), intent(in) :: lcptr"
      write (u, "(A)")  "  integer(c_int), dimension(2), intent(in) :: lshape"
      write (u, "(A)")  "  integer(c_int), dimension(:,:,:), pointer :: " &
           // vname
      write (u, "(A)")  "  logical(c_bool), dimension(:,:), pointer :: " &
           // lname
      if (kind(1) /= c_int) then
         write (u, "(A)")  "  integer, dimension(:,:), allocatable :: " &
              // vname // "_tmp"
      end if
      if (kind(.true.) /= c_bool) then
         write (u, "(A)")  "  logical, dimension(:,:), allocatable :: " &
              // lname // "_tmp"
      end if
      write (u, "(A)")  "  call c_f_pointer (cptr, " // vname // ", shape)"
      write (u, "(A)")  "  call c_f_pointer (lcptr, " // lname // ", lshape)"
      if (kind(1) /= c_int) then
         write (u, "(A)")  "  allocate (" &
              // vname // "_tmp (shape(1), shape(2), shape(3)))"
      end if
      if (kind(.true.) /= c_bool) then
         write (u, "(A)")  "  allocate (" &
              // lname // "_tmp (lshape(1), lshape(2)))"
      end if
      if (kind(1) == c_int) then
         if (kind(.true.) == c_bool) then
            call write_case_lines ("call ", &
                 "_" // vname // " (" // vname // ", " // lname // ")")
         else
            call write_case_lines ("call ", &
                 "_" // vname // " (" // vname // ", " // lname // "_tmp)")
            write (u, "(A)")  "  " // lname // " = " // lname // "_tmp"
         end if
      else
         if (kind(.true.) == c_bool) then
            call write_case_lines ("call ", &
                 "_" // vname // " (" // vname // "_tmp, " // lname // ")")
         else
            call write_case_lines ("call ", &
                 "_" // vname // " (" // vname // "_tmp, " // lname // "_tmp)")
            write (u, "(A)")  "  " // lname // " = " // lname // "_tmp"
         end if
         write (u, "(A)")  "  " // vname // " = " // vname // "_tmp"
      end if
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "set_" // vname
    end subroutine write_set_int_sub2

    subroutine write_set_cf_tab_sub ()
      write (u, "(A)")  ""
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "set_cf_table (pid, iptr1, iptr2, cptr, shape) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  use kinds"
      write (u, "(A)")  "  use omega_color"
      call write_use_lines ("color_factors", "color_factors")
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_ptr), intent(in) :: iptr1, iptr2, cptr"
      write (u, "(A)")  "  integer(c_int), dimension(1), intent(in) :: shape"
      write (u, "(A)")  "  integer(c_int), dimension(:), pointer :: " &
           // "cf_index1, cf_index2"
      write (u, "(A)")  "  complex(c_default_complex), dimension(:), " &
           // "pointer :: col_factor"
      write (u, "(A)")  "  type(omega_color_factor), dimension(:), " &
           // "allocatable :: cf"
      write (u, "(A)")  "  call c_f_pointer (iptr1, cf_index1, shape)"
      write (u, "(A)")  "  call c_f_pointer (iptr2, cf_index2, shape)"
      write (u, "(A)")  "  call c_f_pointer (cptr, col_factor, shape)"
      write (u, "(A)")  "  allocate (cf (shape(1)))"
      call write_case_lines ("call ", "_color_factors (cf)")
      write (u, "(A)")  "  cf_index1 = cf%i1"
      write (u, "(A)")  "  cf_index2 = cf%i2"
      write (u, "(A)")  "  col_factor = cf%factor"
      write (u, "(A)")  "end subroutine " // char (prefix) // "set_cf_table"
    end subroutine write_set_cf_tab_sub

    subroutine write_init_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'init'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "init_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     subroutine prc_init (par) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       real(c_default_float), dimension(*), " &
           // "intent(in) :: par"
      write (u, "(A)")  "     end subroutine prc_init" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_init), bind(C) :: " &
              // char (prc_id(i)) // "_init"
      end do
      call write_case_lines ("fptr = c_funloc (", "_init)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "init_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "subroutine " // char (prc_id(i)) &
              // "_init (par) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  real(c_default_float), dimension(*), " &
              // "intent(in) :: par"
         if (c_default_float == default) then
            write (u, "(A)")  "  call init (par)"
         else
            write (u, "(A, I0)")  "  integer, parameter :: n_par = ", n_par(i)
            write (u, "(A)")  "  real(default), dimension(n_par) :: fpar"
            write (u, "(A)")  "  fpar = par"
            write (u, "(A)")  "  call init (fpar)"
         end if
         write (u, "(A)")  "end subroutine " // char (prc_id(i)) // "_init"
      end do
    end subroutine write_init_get_fptr

    subroutine write_final_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'final'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "final_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     subroutine prc_final () bind(C)"
      write (u, "(A)")  "     end subroutine prc_final" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_final), bind(C) :: " &
              // char (prc_id(i)) // "_final"
      end do
      call write_case_lines ("fptr = c_funloc (", "_final)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "final_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "subroutine " // char (prc_id(i)) &
              // "_final () bind(C)"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  call final ()"
         write (u, "(A)")  "end subroutine " // char (prc_id(i)) // "_final"
      end do
    end subroutine write_final_get_fptr

    subroutine write_update_alpha_s_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'update_alpha_s'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "update_alpha_s_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     subroutine prc_update_alpha_s (alpha_s) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       real(c_default_float), " &
           // "intent(in) :: alpha_s"
      write (u, "(A)")  "     end subroutine prc_update_alpha_s" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_update_alpha_s), bind(C) :: " &
              // char (prc_id(i)) // "_update_alpha_s"
      end do
      call write_case_lines ("fptr = c_funloc (", "_update_alpha_s)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "update_alpha_s_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "subroutine " // char (prc_id(i)) &
              // "_update_alpha_s (alpha_s) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  real(c_default_float), " &
              // "intent(in) :: alpha_s"
         if (c_default_float == default) then
            write (u, "(A)")  "  call update_alpha_s (alpha_s)"
         else
            write (u, "(A)")  "  call update_alpha_s " &
                 // "(real (alpha_s, c_default_float))"
         end if
         write (u, "(A)")  "end subroutine " // char (prc_id(i)) &
              // "_update_alpha_s"
      end do
    end subroutine write_update_alpha_s_get_fptr

    subroutine write_reset_helicity_selection_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: " &
           // "'reset_helicity_selection'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "reset_helicity_selection_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     subroutine " &
           // "prc_reset_helicity_selection (threshold, cutoff) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       real(c_default_float), " &
           // "intent(in) :: threshold"
      write (u, "(A)")  "       integer(c_int), " &
           // "intent(in) :: cutoff"
      write (u, "(A)")  "     end subroutine prc_reset_helicity_selection" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_reset_helicity_selection), " &
              // "bind(C) :: " &
              // char (prc_id(i)) // "_reset_helicity_selection"
      end do
      call write_case_lines ("fptr = c_funloc (", "_reset_helicity_selection)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "reset_helicity_selection_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "subroutine " // char (prc_id(i)) &
              // "_reset_helicity_selection (threshold, cutoff) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  real(c_default_float), " &
              // "intent(in) :: threshold"
         write (u, "(A)")  "  integer(c_int), " &
              // "intent(in) :: cutoff"
         write (u, "(A)")  "  real(default) :: rthreshold"
         write (u, "(A)")  "  integer :: icutoff"
         write (u, "(A)")  "  rthreshold = threshold"
         write (u, "(A)")  "  icutoff = cutoff"
         write (u, "(A)")  "  call reset_helicity_selection " &
              // "(rthreshold, icutoff)"
         write (u, "(A)")  "end subroutine " // char (prc_id(i)) &
              // "_reset_helicity_selection"
      end do
    end subroutine write_reset_helicity_selection_get_fptr

    subroutine write_new_event_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'new_event'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "new_event_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     subroutine prc_new_event (p) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       real(c_default_float), dimension(0:3,*), " &
           // "intent(in) :: p"
      write (u, "(A)")  "     end subroutine prc_new_event" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_new_event), bind(C) :: " &
              // char (prc_id(i)) // "_new_event"
      end do
      call write_case_lines ("fptr = c_funloc (", "_new_event)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "new_event_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "subroutine " // char (prc_id(i)) &
              // "_new_event (p) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  real(c_default_float), dimension(0:3,*), " &
              // "intent(in) :: p"
         if (c_default_float == default) then
            write (u, "(A)")  "  call new_event (p)"
         else
            write (u, "(A)")  "  integer :: n_tot"
            write (u, "(A)")  "  real(default), dimension(:,:), " &
                 // "allocatable :: k"
            write (u, "(A)")  "  n_tot = " &
                 // "number_particles_in () + number_particles_out ()"
            write (u, "(A)")  "  allocate (k (0:3,n_tot))"
            write (u, "(A)")  "  k = p"
            write (u, "(A)")  "  call new_event (k)"
         end if
         write (u, "(A)")  "end subroutine " // char (prc_id(i)) // "_new_event"
      end do
    end subroutine write_new_event_get_fptr

    subroutine write_is_allowed_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'is_allowed'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "is_allowed_get_fptr (pid, fptr) bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     function " &
           // "prc_is_allowed (flv, hel, col) result (flag) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       logical(c_bool) :: flag"
      write (u, "(A)")  "       integer(c_int), intent(in) :: flv, hel, col"
      write (u, "(A)")  "     end function prc_is_allowed" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_is_allowed), bind(C) :: " &
              // char (prc_id(i)) // "_is_allowed"
      end do
      call write_case_lines ("fptr = c_funloc (", "_is_allowed)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "is_allowed_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "function " // char (prc_id(i)) &
              // "_is_allowed (flv, hel, col) result (flag) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  logical(c_bool) :: flag"
         write (u, "(A)")  "  integer(c_int), intent(in) :: flv, hel, col"
         if (c_int == kind(1)) then
            write (u, "(A)")  "  flag = is_allowed (flv, hel, col)"
         else
            write (u, "(A)")  "  integer :: iflv, ihel, icol"
            write (u, "(A)")  "  iflv = flv;  ihel = hel;  icol = col"
            write (u, "(A)")  "  flag = is_allowed (iflv, ihel, icol)"
         end if
         write (u, "(A)")  "end function " // char (prc_id(i)) &
              // "_is_allowed"
      end do
    end subroutine write_is_allowed_get_fptr

    subroutine write_get_amplitude_get_fptr ()
      write (u, "(A)")  ""
      write (u, "(A)")  "! Return pointer to function: 'get_amplitude'"
      write (u, "(A)")  "subroutine " // char (prefix) &
           // "get_amplitude_get_fptr (pid, fptr) " &
           // "bind(C)"
      write (u, "(A)")  "  use iso_c_binding"
      write (u, "(A)")  "  integer(c_int), intent(in) :: pid"
      write (u, "(A)")  "  type(c_funptr), intent(out) :: fptr"
      write (u, "(A)")  "  abstract interface"
      write (u, "(A)")  "     function " &
           // "prc_get_amplitude (flv, hel, col) result (amp) bind(C)"
      write (u, "(A)")  "       use iso_c_binding"
      write (u, "(A)")  "       use kinds"
      write (u, "(A)")  "       complex(c_default_complex) :: amp"
      write (u, "(A)")  "       integer(c_int), intent(in) :: flv, hel, col"
      write (u, "(A)")  "     end function prc_get_amplitude" 
      write (u, "(A)")  "  end interface"
      do i = 1, n_prc
         write (u, "(2x,A)")  "procedure(prc_get_amplitude), bind(C) :: " &
              // char (prc_id(i)) // "_get_amplitude"
      end do
      call write_case_lines ("fptr = c_funloc (", "_get_amplitude)")
      write (u, "(A)")  "end subroutine " // char (prefix) &
           // "get_amplitude_get_fptr"
      do i = 1, n_prc
         write (u, *)
         write (u, "(A)")  "function " // char (prc_id(i)) &
              // "_get_amplitude (flv, hel, col) result (amp) bind(C)"
         write (u, "(A)")  "  use iso_c_binding"
         write (u, "(A)")  "  use kinds"
         write (u, "(A)")  "  use " // char (prc_id(i))
         write (u, "(A)")  "  complex(c_default_complex) :: amp"
         write (u, "(A)")  "  integer(c_int), intent(in) :: flv, hel, col"
         if (c_int == kind(1)) then
            write (u, "(A)")  "  amp = get_amplitude (flv, hel, col)"
         else
            write (u, "(A)")  "  integer :: iflv, ihel, icol"
            write (u, "(A)")  "  iflv = flv;  ihel = hel;  icol = col"
            write (u, "(A)")  "  amp = get_amplitude (iflv, ihel, icol)"
         end if
         write (u, "(A)")  "end function " // char (prc_id(i)) &
              // "_get_amplitude"
      end do
    end subroutine write_get_amplitude_get_fptr

    subroutine write_use_lines (vname, fname)
      character(*), intent(in) :: vname, fname
      integer :: i
      do i = 1, n_prc
         write (u, "(2x,A)")  "use " // char (prc_id(i)) // ", only: " &
              // char (prc_id(i)) // "_" // vname // " => " // fname
      end do
    end subroutine write_use_lines

    subroutine write_case_lines (cmd1, cmd2)
      character(*), intent(in) :: cmd1, cmd2
      integer :: i
      write (u, "(A)")  "  select case (pid)"
      do i = 1, n_prc
         write (u, "(2x,A,I0,A)")  "case(", i, ");  " &
              // cmd1 // char (prc_id(i)) // cmd2
      end do
      write (u, "(A)")  "  end select"
    end subroutine write_case_lines

  end subroutine process_library_write_driver

  subroutine write_library_manager (libname)

    type(string_t), dimension(:), intent(in) :: libname
    integer :: u, i

    call msg_message ("Writing library manager code")
    u = free_unit ()
    open (unit=u, file="libmanager.f90", action="write", status="replace")
    write (u, "(A)")  "! WHIZARD library manager"
    write (u, "(A)")  "!"
    write (u, "(A)")  "! Automatically generated file, do not edit"
    write (u, "(A)")  ""
    write (u, "(A)")  "function libmanager_get_n_libs () result (n)"
    write (u, "(A)")  "  integer :: n"
    write (u, "(A,1x,I0)")  "  n =", size (libname)
    write (u, "(A)")  "end function libmanager_get_n_libs"
    write (u, "(A)")  ""
    write (u, "(A)")  "function libmanager_get_libname (i) result (name)"
    write (u, "(A)")  "  use iso_varying_string, string_t => varying_string"
    write (u, "(A)")  "  type(string_t) :: name"
    write (u, "(A)")  "  integer, intent(in) :: i"
    write (u, "(A)")  "  select case (i)"
    do i = 1, size (libname)
       call write_lib_name (i, libname(i))
    end do
    write (u, "(A)")  "  case default;  name = ''"
    write (u, "(A)")  "  end select"
    write (u, "(A)")  "end function libmanager_get_libname"
    write (u, "(A)")  ""
    write (u, "(A)")  "function libmanager_get_c_funptr (libname, fname) " &
         // "result (c_fptr)"
    write (u, "(A)")  "  use iso_c_binding"
    write (u, "(A)")  "  use prclib_interfaces"
    write (u, "(A)")  "  type(c_funptr) :: c_fptr"
    write (u, "(A)")  "  character(*), intent(in) :: libname, fname"
    do i = 1, size (libname)
       call write_lib_declarations (libname(i))
    end do
    write (u, "(A)")  "  select case (libname)"
    do i = 1, size (libname)
       call write_lib_code (libname(i))
    end do
    write (u, "(A)")  "  case default"
    write (u, "(A)")  "     c_fptr = c_null_funptr"
    write (u, "(A)")  "  end select"
    write (u, "(A)")  "end function libmanager_get_c_funptr"
    close (u)

  contains
    
    subroutine write_lib_name (i, libname)
      integer, intent(in) :: i
      type(string_t), intent(in) :: libname
      write (u, "(A,I0,A)")  "  case (", i, ");  name = '" // char (libname) &
           // "'"
    end subroutine write_lib_name

    subroutine write_lib_declarations (libname)
      type(string_t), intent(in) :: libname
      write (u, "(A)")  "  procedure(prc_get_n_processes), bind(C) :: " &
           // char (libname)// "_" //  "get_n_processes"
      write (u, "(A)")  "  procedure(prc_get_stringptr), bind(C) :: " &
           // char (libname)// "_" //  "get_process_id"
      write (u, "(A)")  "  procedure(prc_get_stringptr), bind(C) :: " &
           // char (libname)// "_" //  "get_model_name"
      write (u, "(A)")  "  procedure(prc_get_stringptr), bind(C) :: " &
           // char (libname)// "_" //  "get_restrictions"
      write (u, "(A)")  "  procedure(prc_get_stringptr), bind(C) :: " &
           // char (libname)// "_" //  "get_md5sum"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_in"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_out"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_flv"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_hel"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_col"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_cin"
      write (u, "(A)")  "  procedure(prc_get_int), bind(C) :: " &
           // char (libname)// "_" //  "get_n_cf"
      write (u, "(A)")  "  procedure(prc_set_int_tab1), bind(C) :: " &
           // char (libname)// "_" //  "set_flv_state"
      write (u, "(A)")  "  procedure(prc_set_int_tab1), bind(C) :: " &
           // char (libname)// "_" //  "set_hel_state"
      write (u, "(A)")  "  procedure(prc_set_int_tab2), bind(C) :: " &
           // char (libname)// "_" //  "set_col_state"
      write (u, "(A)")  "  procedure(prc_set_cf_tab), bind(C) :: " &
           // char (libname)// "_" //  "set_cf_table"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "init_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "final_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "update_alpha_s_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "new_event_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "reset_helicity_selection_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "is_allowed_get_fptr"
      write (u, "(A)")  "  procedure(prc_get_fptr), bind(C) :: " &
           // char (libname)// "_" //  "get_amplitude_get_fptr"
    end subroutine write_lib_declarations

    subroutine write_lib_code (libname)
      type(string_t), intent(in) :: libname
      write (u, "(2x,A)")  "case ('" // char (libname) // "')"
      write (u, "(2x,A)")  "   select case (fname)"
      call write_fun_code (char (libname), "get_n_processes")
      call write_fun_code (char (libname), "get_process_id")
      call write_fun_code (char (libname), "get_model_name")
      call write_fun_code (char (libname), "get_restrictions")
      call write_fun_code (char (libname), "get_md5sum")
      call write_fun_code (char (libname), "get_n_in")
      call write_fun_code (char (libname), "get_n_out")
      call write_fun_code (char (libname), "get_n_flv")
      call write_fun_code (char (libname), "get_n_hel")
      call write_fun_code (char (libname), "get_n_col")
      call write_fun_code (char (libname), "get_n_cin")
      call write_fun_code (char (libname), "get_n_cf")
      call write_fun_code (char (libname), "set_flv_state")
      call write_fun_code (char (libname), "set_hel_state")
      call write_fun_code (char (libname), "set_col_state")
      call write_fun_code (char (libname), "set_cf_table")
      call write_fun_code (char (libname), "init_get_fptr")
      call write_fun_code (char (libname), "final_get_fptr")
      call write_fun_code (char (libname), "update_alpha_s_get_fptr")
      call write_fun_code (char (libname), "reset_helicity_selection_get_fptr")
      call write_fun_code (char (libname), "new_event_get_fptr")
      call write_fun_code (char (libname), "is_allowed_get_fptr")
      call write_fun_code (char (libname), "get_amplitude_get_fptr")
      write (u, "(2x,A)")  "   case default"
      write (u, "(2x,A)")  "      print *, fname"
      write (u, "(2x,A)")  "      stop 'WHIZARD bug: " &
           // "libmanager cannot handle this function'"
      write (u, "(2x,A)")  "   end select"
    end subroutine write_lib_code

    subroutine write_fun_code (prefix, fname)
      character(*), intent(in) :: prefix, fname
      write (u, "(5x,A)")  "case ('" // fname // "')"
      write (u, "(5x,A)")  "   c_fptr = c_funloc (" // prefix &
           // "_" // fname // ")"
    end subroutine write_fun_code

  end subroutine write_library_manager

  function get_modellibs_flags (prc_lib, os_data) result (flags)
    type(process_library_t), intent(in) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: flags
    type(string_t), dimension(:), allocatable :: models
    type(string_t) :: modelname, modellib, modellib_full
    logical :: exist
    type(process_configuration_t), pointer :: current
    integer :: i, j, mi
    flags = ""
    if ((.not. os_data%use_testfiles) .and. &
               os_dir_exist (os_data%whizard_models_libpath_local)) &
                 flags = flags // " -L" // os_data%whizard_models_libpath_local
    flags = flags // " -L" // os_data%whizard_models_libpath
    allocate (models(prc_lib%n_prc + 1))
    models = ""
    mi = 1
    current => prc_lib%prc_first
    SCAN: do i = 1, prc_lib%n_prc
       modelname = model_get_name (current%model)
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
       current => current%next
    end do SCAN
    deallocate (models)
  end function get_modellibs_flags
  subroutine process_library_compile &
       (prc_lib, os_data, recompile_library, objlist_link)
    type(process_library_t), intent(inout) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: recompile_library
    type(string_t), intent(out) :: objlist_link
    type(string_t) :: objlist_comp
    type(process_configuration_t), pointer :: current
    type(string_t) :: ext
    integer :: i
    if (prc_lib%status == STAT_LOADED)  call process_library_unload (prc_lib)
    call msg_message ("Compiling process library '" // &
         char (process_library_get_name (prc_lib)) // "'")
    objlist_comp = ""
    objlist_link = ""
    if (os_data%use_libtool) then
       ext = ".lo"
    else
       ext = os_data%obj_ext
    end if
    current => prc_lib%prc_first
    SCAN_PROCESSES: do i = 1, prc_lib%n_prc
       objlist_link = objlist_link // " " // current%id // ext
       if (recompile_library) &
            current%status = min (STAT_CODE_GENERATED, current%status)
       if (current%status == STAT_CODE_GENERATED) then
          objlist_comp = objlist_comp // " " // current%id // ext
          call os_compile_shared (current%id, os_data)
          current%status = STAT_COMPILED
       else
          call msg_message ("Skipping process '" // char (current%id) &
               // "' (object code exists)")
       end if
       current => current%next
    end do SCAN_PROCESSES
    if (objlist_comp /= "") then
       call os_compile_shared (prc_lib%basename, os_data)
       objlist_link = objlist_link // " " // prc_lib%basename // ext
    else
       call msg_message ("Skipping library '" &
            // char (prc_lib%basename) &
            // "' (no processes have been recompiled)")
       objlist_link = ""
    end if
    prc_lib%status = STAT_COMPILED
  end subroutine process_library_compile

  subroutine process_library_link (prc_lib, os_data, objlist)
    type(process_library_t), intent(in) :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(string_t), intent(in) :: objlist
    if (objlist /= "") then
       call os_link_shared (objlist // " " // &
            os_data%whizard_ldflags // " " // os_data%ldflags // &
            get_modellibs_flags (prc_lib, os_data),  prc_lib%basename, os_data)
    end if
  end subroutine process_library_link

  subroutine compile_library_manager (os_data)
    type(os_data_t), intent(in) :: os_data
    call msg_message ("Compiling library manager")
    call os_compile_shared (var_str ("libmanager"), os_data)
  end subroutine compile_library_manager

  subroutine link_executable (libname, exec_name, flags, os_data)
    type(string_t), dimension(:), intent(in) :: libname
    type(string_t), intent(in) :: exec_name, flags
    type(os_data_t), intent(in) :: os_data
    type(string_t) :: objlist, ext_o, ext_a
    integer :: i
    if (os_data%use_libtool) then
       ext_o = ".lo"
       ext_a = ".la"
    else
       ext_o = ".o"
       ext_a = ".a"
    end if
    objlist = "libmanager" // ext_o
    do i = 1, size (libname)
       objlist = objlist // " " // libname(i) // ext_a
    end do
    print *, char (flags)
    call os_link_static (objlist // flags, exec_name, os_data)
  end subroutine link_executable

  subroutine process_library_load (prc_lib, os_data, model, var_list, ignore)
    type(process_library_t), intent(inout), target :: prc_lib
    type(os_data_t), intent(in) :: os_data
    type(model_t), pointer, optional :: model
    type(var_list_t), intent(inout), optional :: var_list
    logical, intent(in), optional :: ignore
    type(c_funptr) :: c_fptr
    type(model_t), pointer :: mdl
    type(string_t) :: prefix
    logical :: ignore_error
    ignore_error = .false.;  if (present (ignore))  ignore_error = ignore
    if (prc_lib%status == STAT_LOADED) then
       if (.not. ignore_error) then
          call msg_message ("Process library '" // char (prc_lib%basename) &
               // "' is already loaded")
       end if
       return
    end if
    if (prc_lib%static) then
       call msg_message ("Loading static process library '" &
            // char (prc_lib%basename) // "'")
    else
       call msg_message ("Loading process library '" &
            // char (prc_lib%basename) // "'")
       prc_lib%libname = os_get_dlname (prc_lib%basename, os_data, ignore)
       if (prc_lib%libname == "")  return
       call dlaccess_init (prc_lib%dlaccess, var_str ("."), &
            prc_lib%libname, os_data)
       call process_library_check_dlerror (prc_lib)
    end if
    prefix = prc_lib%basename
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_processes"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_prc)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_process_id"))
    call c_f_procpointer (c_fptr, prc_lib%get_process_id)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_model_name"))
    call c_f_procpointer (c_fptr, prc_lib%get_model_name)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_restrictions"))
    call c_f_procpointer (c_fptr, prc_lib%get_restrictions)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_md5sum"))
    call c_f_procpointer (c_fptr, prc_lib%get_md5sum)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_in"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_in)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_out"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_out)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_flv"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_flv)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_hel"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_hel)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_col"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_col)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_cin"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_cin)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_n_cf"))
    call c_f_procpointer (c_fptr, prc_lib%get_n_cf)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("set_flv_state"))
    call c_f_procpointer (c_fptr, prc_lib%set_flv_state)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("set_hel_state"))
    call c_f_procpointer (c_fptr, prc_lib%set_hel_state)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("set_col_state"))
    call c_f_procpointer (c_fptr, prc_lib%set_col_state)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("set_cf_table"))
    call c_f_procpointer (c_fptr, prc_lib%set_cf_table)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("init_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%init_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("final_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%final_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("update_alpha_s_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%update_alpha_s_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("new_event_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%new_event_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("reset_helicity_selection_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%reset_helicity_selection_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("is_allowed_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%is_allowed_get_fptr)
    c_fptr = process_library_get_c_funptr &
         (prc_lib, prefix, var_str ("get_amplitude_get_fptr"))
    call c_f_procpointer (c_fptr, prc_lib%get_amplitude_get_fptr)
    call process_library_load_configuration (prc_lib, os_data, mdl)
    prc_lib%status = STAT_LOADED
    if (associated (prc_lib%reload_hook)) &
       call prc_lib%reload_hook (process_library_get_name (prc_lib))
    call var_list_set_string (var_list, var_str ("$library_name"), &
         process_library_get_name (prc_lib), is_known=.true.)  ! $
    if (present (model))  model => mdl
  end subroutine process_library_load

  subroutine process_library_unload (prc_lib)
    type(process_library_t), intent(inout) :: prc_lib
    call msg_message ("Unloading process library '" // &
         char (process_library_get_name (prc_lib)) // "'")
    if (associated (prc_lib%unload_hook)) &
       call prc_lib%unload_hook (process_library_get_name(prc_lib))
    call dlaccess_final (prc_lib%dlaccess)
    prc_lib%status = STAT_CODE_GENERATED
  end subroutine process_library_unload

  subroutine process_library_set_unload_hook (prc_lib, hook)
    type(process_library_t), intent(inout), target :: prc_lib
    procedure(prclib_unload_hook), pointer, intent(in) :: hook
    prc_lib%unload_hook => hook
  end subroutine process_library_set_unload_hook

  subroutine process_library_set_reload_hook (prc_lib, hook)
    type(process_library_t), intent(inout), target :: prc_lib
    procedure(prclib_reload_hook), pointer, intent(in) :: hook
    prc_lib%reload_hook => hook
  end subroutine process_library_set_reload_hook

  function process_library_get_c_funptr &
       (prc_lib, prefix, fname) result (c_fptr)
    type(c_funptr) :: c_fptr
    type(process_library_t), intent(inout) :: prc_lib
    type(string_t), intent(in) :: prefix, fname
    type(string_t) :: full_name
    full_name = prefix // "_" // fname
    if (prc_lib%static) then
       c_fptr = libmanager_get_c_funptr (char (prefix), char (fname))
    else
       c_fptr = dlaccess_get_c_funptr (prc_lib%dlaccess, full_name)
       call process_library_check_dlerror (prc_lib)
    end if
  end function process_library_get_c_funptr

  subroutine process_library_check_dlerror (prc_lib)
    type(process_library_t), intent(in) :: prc_lib
    if (dlaccess_has_error (prc_lib%dlaccess)) then
       call msg_fatal (char (dlaccess_get_error (prc_lib%dlaccess)))
    end if
  end subroutine process_library_check_dlerror

  subroutine process_library_store_append (name, os_data, prc_lib)
    type(string_t), intent(in) :: name
    type(os_data_t), intent(in) :: os_data
    type(process_library_t), pointer :: prc_lib
    prc_lib => process_library_store_get_ptr (name)
    if (.not. associated (prc_lib)) then
       call msg_message &
            ("Initializing process library '" // char (name) // "'")
       allocate (prc_lib)
       call process_library_init (prc_lib, name, os_data)
       if (associated (process_library_store%last)) then
          process_library_store%last%next => prc_lib
       else
          process_library_store%first => prc_lib
       end if
       process_library_store%last => prc_lib
    end if
  end subroutine process_library_store_append

  subroutine process_library_store_final ()
    type(process_library_t), pointer :: current
    do while (associated (process_library_store%first))
       current => process_library_store%first
       process_library_store%first => current%next
       call process_library_final (current)
       deallocate (current)
    end do
  end subroutine process_library_store_final

  subroutine process_library_store_load (os_data, var_list)
    type(os_data_t), intent(in) :: os_data
    type(var_list_t), intent(inout), optional :: var_list
    type(process_library_t), pointer :: current
    current => process_library_store%first
    do while (associated (current))
       call process_library_load (current, os_data, var_list=var_list)
       current => current%next
    end do
  end subroutine process_library_store_load

  function process_library_store_get_ptr (name) result (prc_lib)
    type(process_library_t), pointer :: prc_lib
    type(string_t), intent(in) :: name
    prc_lib => process_library_store%first
    do while (associated (prc_lib))
       if (prc_lib%basename == name)  exit
       prc_lib => prc_lib%next
    end do
  end function process_library_store_get_ptr

  function process_library_store_get_first () result (prc_lib)
    type(process_library_t), pointer :: prc_lib
    prc_lib => process_library_store%first
  end function process_library_store_get_first

  subroutine process_library_store_load_static &
       (os_data, prc_lib, model, var_list)
    type(os_data_t), intent(in) :: os_data
    type(process_library_t), pointer :: prc_lib
    type(model_t), pointer :: model
    type(var_list_t), intent(inout) :: var_list
    integer :: n, i
    type(string_t), dimension(:), allocatable :: libname
    n = libmanager_get_n_libs ()
    allocate (libname (n))
    do i = 1, n
       libname(i) = libmanager_get_libname (i)
    end do
    do i = 1, n
       call process_library_store_append (libname(i), os_data, prc_lib)
       call process_library_set_static (prc_lib, .true.)
       call process_library_load (prc_lib, os_data, model, var_list)
    end do
  end subroutine process_library_store_load_static

  subroutine process_library_record_integral &
       (prc_lib, prc_id, n_calls, integral, error, accuracy, chi2, efficiency)
    type(process_library_t), intent(inout), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    integer, intent(in) :: n_calls
    real(default), intent(in) :: integral, error, accuracy, chi2, efficiency
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       if (prc_conf%status >= STAT_LOADED) then
          call process_configuration_record_integral &
               (prc_conf, n_calls, integral, error, accuracy, chi2, efficiency)
       else
          call msg_bug ("Process '" // char (prc_id) // "': not loaded, " &
               // "can't record integral")
       end if
    else
       call msg_bug ("Process '" // char (prc_id) // "': not associated, " &
            // "can't record integral")
    end if
  end subroutine process_library_record_integral

  function process_library_get_n_calls (prc_lib, prc_id) result (n_calls)
    integer :: n_calls
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       n_calls = prc_conf%n_calls
    else
       n_calls = 0
    end if
  end function process_library_get_n_calls

  function process_library_get_integral (prc_lib, prc_id) result (integral)
    real(default) :: integral
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       integral = prc_conf%integral
    else
       integral = 0
    end if
  end function process_library_get_integral

  function process_library_get_error (prc_lib, prc_id) result (error)
    real(default) :: error
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       error = prc_conf%error
    else
       error = 0
    end if
  end function process_library_get_error

  function process_library_get_accuracy (prc_lib, prc_id) result (accuracy)
    real(default) :: accuracy
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       accuracy = prc_conf%accuracy
    else
       accuracy = 0
    end if
  end function process_library_get_accuracy

  function process_library_get_chi2 (prc_lib, prc_id) result (chi2)
    real(default) :: chi2
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       chi2 = prc_conf%chi2
    else
       chi2 = 0
    end if
  end function process_library_get_chi2

  function process_library_get_efficiency (prc_lib, prc_id) result (efficiency)
    real(default) :: efficiency
    type(process_library_t), intent(in), target :: prc_lib
    type(string_t), intent(in) :: prc_id
    type(process_configuration_t), pointer :: prc_conf
    prc_conf => process_library_get_process_ptr (prc_lib, prc_id)
    if (associated (prc_conf)) then
       efficiency = prc_conf%efficiency
    else
       efficiency = 0
    end if
  end function process_library_get_efficiency

  subroutine process_libraries_test ()
    type(model_t), pointer :: model
    type(process_library_t), pointer :: prc_lib
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(os_data_t) :: os_data
    type(string_t) :: objlist
    call os_data_init (os_data)
    os_data%fcflags = "-gline -C=all"
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("QCD"), var_str("test.mdl"), os_data, model)
    call syntax_model_file_final ()
    print *, "*** Create library 'proc' with two processes"
    print *, "* Setup process configuration"
    print *, "  [temporary: include zero processes because of references"
    print *, "   to omegalib, which we also need as a .so version]"
    print *, "  [iso_varying_string included in libproc.so for the same reason"
    call process_library_store_append (var_str ("proc"), os_data, prc_lib)
    allocate (prt_in (1), prt_out (2))
    prt_in(1) = "Z"
    prt_out(1) = "e1"
    prt_out(2) = "E1"
    call process_library_append &
         (prc_lib, var_str ("zee"), model, prt_in, prt_out)
    deallocate (prt_in, prt_out)
    allocate (prt_in (2), prt_out (2))
    prt_in(1) = "g"
    prt_in(2) = "g"
    prt_out(1) = "u"
    prt_out(2) = "U"
    call process_library_append &
         (prc_lib, var_str ("uu"), model, prt_in, prt_out)
    print *
    print *, "* Generate code"
    call process_library_generate_code (prc_lib, os_data)
    print *
    print *, "* Write driver file 'proc_interface.f90'"
    call process_library_write_driver (prc_lib)
    print *
    print *, "* Compile and link as 'libproc.so'"
    call process_library_compile (prc_lib, os_data, .false., objlist)
    call process_library_link (prc_lib, os_data, objlist)
    print *
    print *, "* Load shared libraries"
    call process_library_store_load (os_data)
    print *
    print *, "* Execute 'get_n_processes' from the shared library named 'proc'"
    print *
    prc_lib => process_library_store_get_ptr (var_str ("proc"))
    print *, "n_prc = ", prc_lib% get_n_prc ()
    print *
    print *, "* Cleanup"
    call process_library_store_final
  end subroutine process_libraries_test


end module process_libraries
