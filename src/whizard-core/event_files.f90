! WHIZARD 2.1.1 September 18 2012
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

module event_files

  use kinds, only: default !NODEP!
  use kinds, only: i64 !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use variables
  use expressions
  use flavors
  use event_formats
  use processes
  use stdhep_interface
  use hepmc_interface
  use events
  use decays

  implicit none
  private

  public :: event_file_get_format
  public :: input_event_stream_t
  public :: input_event_stream_init
  public :: input_event_stream_read_event
  public :: input_event_stream_final 
  public :: event_file_list_t
  public :: event_file_list_append_file_spec
  public :: event_file_list_is_filename
  public :: event_file_list_open
  public :: event_file_list_write_event
  public :: event_file_list_close
  public :: event_format_code

  integer, parameter, public :: FMT_NONE = 0
  integer, parameter, public :: FMT_RAW = -1
  integer, parameter, public :: FMT_DEFAULT = 1
  integer, parameter, public :: FMT_DEBUG = 2
  integer, parameter, public :: FMT_HEPMC = 10
  integer, parameter, public :: FMT_LHEF = 20
  integer, parameter, public :: FMT_LHA = 21
  integer, parameter, public :: FMT_LHA_VERB = 29
  integer, parameter, public :: FMT_HEPEVT = 30
  integer, parameter, public :: FMT_ASCII_SHORT = 31
  integer, parameter, public :: FMT_ASCII_LONG = 32
  integer, parameter, public :: FMT_ATHENA = 33
  integer, parameter, public :: FMT_HEPEVT_VERB = 39
  integer, parameter, public :: FMT_STDHEP = 40
  integer, parameter, public :: FMT_STDHEP_UP = 41


  type :: input_event_stream_t
    integer :: fmt = FMT_NONE
    integer :: polarization_mode = FM_IGNORE_HELICITY
    type(hepmc_iostream_t), pointer :: iostream => null ()
  end type input_event_stream_t

  type :: file_spec_t
     private
     type(string_t) :: name
     integer :: format = FMT_NONE
     type(hepmc_iostream_t), pointer :: iostream => null ()
     integer :: unit = 0
     type(flavor_t), dimension(:), allocatable :: beam_flv
     real(default), dimension(:), allocatable :: beam_energy
     real(default), dimension(:), allocatable :: integral
     real(default), dimension(:), allocatable :: error
     integer :: n_processes = 0
     logical :: unweighted = .true.
     logical :: negative_weights = .false.
     logical :: keep_beams = .false.
     type(file_spec_t), pointer :: next => null ()
  end type file_spec_t

  type :: event_file_list_t
     private
     type(file_spec_t), pointer :: first => null ()
     type(file_spec_t), pointer :: last => null ()
  end type event_file_list_t


contains

  function event_file_get_format (file) result (fmt)
    integer :: fmt
    type(string_t), intent(in) :: file
    if (is_raw_fmt (file)) then
       fmt = FMT_RAW
    else if (is_hepmc_fmt (file)) then
       fmt = FMT_HEPMC
    else
       fmt = FMT_NONE
    end if
  end function event_file_get_format

  function is_raw_fmt (file) result (flag)
    logical :: flag
    type(string_t), intent(in) :: file
    integer :: u, iostat
    u = free_unit ()
    open (unit=u, file=char(file), action="read", status="old", &
          form="unformatted", iostat=iostat)
    if (iostat == 0) then
       flag = is_raw_event_file (u)
       close (u)
    else
       flag = .false.
    end if
  end function is_raw_fmt

  function is_hepmc_fmt (file)  result (flag)
    logical :: flag
    type(string_t), intent(in) :: file
    integer :: u, iostat
    open (unit=u, file=char(file), action="read", status="old", iostat=iostat)
    if (iostat == 0) then
       flag = is_hepmc_event_file (u)
       close (u)
    else
       flag = .false.
    end if
  end function is_hepmc_fmt

  subroutine input_event_stream_init (input_stream, file, fmt)
    type(input_event_stream_t), intent(out) :: input_stream
    type(string_t), intent(in) :: file
    integer, intent(in) :: fmt
    input_stream%fmt = fmt
    select case (input_stream%fmt)
    case (FMT_HEPMC)
       if (hepmc_is_available ()) then
          allocate (input_stream%iostream)
          call hepmc_iostream_open_in (input_stream%iostream, file)
       else
          call msg_fatal ("HepMC event reading is disabled " &
               // "because HepMC library is not linked.")
          input_stream%fmt = FMT_NONE
       end if
    case default
       call msg_bug ("Unsupported file format selected for reading events.")
    end select
  end subroutine input_event_stream_init

  subroutine input_event_stream_read_event (input_stream, event, &
       event_vars, prc_array, ok, num_id_array)
    type(input_event_stream_t), intent(inout) :: input_stream
    type(event_t), intent(out) :: event
    type(event_vars_t), intent(inout), target :: event_vars
    type(process_p), dimension(:), intent(in) :: prc_array
    logical, intent(out) :: ok
    integer, dimension(:), intent(in), optional :: num_id_array
    type(hepmc_event_t) :: hepmc_event
    select case (input_stream%fmt)
    case (FMT_HEPMC)
       call hepmc_event_init (hepmc_event)
       call hepmc_iostream_read_event (input_stream%iostream, hepmc_event, ok)
       if (ok) then
          call event_read_from_hepmc &
               (event, hepmc_event, input_stream%polarization_mode, &
                event_vars, prc_array, num_id_array)
          ! call hepmc_event_print (hepmc_event)
       end if
       call hepmc_event_final (hepmc_event)
    end select
  end subroutine input_event_stream_read_event

  subroutine input_event_stream_final (input_stream)
    type(input_event_stream_t), intent(inout) :: input_stream
    select case (input_stream%fmt)
    case (FMT_HEPMC)
       call hepmc_iostream_close (input_stream%iostream)
       deallocate (input_stream%iostream)
    end select
    input_stream%fmt = FMT_NONE
  end subroutine input_event_stream_final

  subroutine event_file_list_append_file_spec &
       (event_file_list, basename, var_list, format, beam_flv, beam_energy, &
        n_processes) 
       ! unweighted, negative_weights, &
    type(event_file_list_t), intent(inout) :: event_file_list
    type(string_t), intent(in) :: basename
    type(var_list_t), intent(in) :: var_list
    integer, intent(in) :: format
    type(flavor_t), dimension(:), intent(in) :: beam_flv
    real(default), dimension(:), intent(in) :: beam_energy
    integer, intent(in) :: n_processes
!     logical, intent(in) :: unweighted, negative_weights
    type(file_spec_t), pointer :: current
    allocate (current)
    select case (format)
    case (FMT_DEFAULT);     current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_default"))
    case (FMT_DEBUG);       current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_debug"))
    case (FMT_HEPMC);       current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_hepmc"))
    case (FMT_LHEF);        current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_lhef"))
    case (FMT_LHA);         current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_lha"))
    case (FMT_HEPEVT);      current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_hepevt"))
    case (FMT_ASCII_SHORT); current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_ascii_short"))
    case (FMT_ASCII_LONG);  current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_ascii_long"))
    case (FMT_ATHENA);      current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_athena"))
    case (FMT_STDHEP);      current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_stdhep"))
    case (FMT_STDHEP_UP);   current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_stdhep_up"))
    case (FMT_HEPEVT_VERB); current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_hepevt_verbose"))
    case (FMT_LHA_VERB);    current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_lha_verbose"))
    case default;           current%name = basename // "." // var_list_get_sval &
            (var_list, var_str ("$extension_default"))
    end select          
    current%format = format
    allocate (current%beam_flv (size (beam_flv)))
    current%beam_flv = beam_flv
    allocate (current%beam_energy (size (beam_energy)))
    current%beam_energy = beam_energy
    current%n_processes = n_processes
    current%keep_beams = var_list_get_lval (var_list, var_str ("?keep_beams"))
    if (associated (event_file_list%last)) then
       event_file_list%last%next => current
    else
       event_file_list%first => current
    end if
    event_file_list%last => current
  end subroutine event_file_list_append_file_spec

  subroutine event_file_list_final (event_file_list)
    type(event_file_list_t), intent(inout) :: event_file_list
    type(file_spec_t), pointer :: current
    do while (associated (event_file_list%first))
       current => event_file_list%first
       event_file_list%first => current%next
       deallocate (current)
    end do
    event_file_list%last => null ()
  end subroutine event_file_list_final

  function event_file_list_is_filename (event_file_list, filename) result (flag)
    logical :: flag
    type(event_file_list_t), intent(in) :: event_file_list
    type(string_t), intent(in) :: filename
    type(file_spec_t), pointer :: current
    current => event_file_list%first
    do while (associated (current))
       if (current%name == filename) then
          flag = .true.
          return
       end if
       current => current%next
    end do
    flag = .false.
  end function event_file_list_is_filename

  subroutine event_file_list_open (event_file_list, process_id, n_events, var_list)
    type(event_file_list_t), intent(inout), target :: event_file_list
    type(string_t), dimension(:), intent(in) :: process_id
    integer, intent(in) :: n_events
    real(default), dimension(:), allocatable :: integral, error
    type(var_list_t), intent(in) :: var_list
    type(process_t), pointer :: process 
    type(file_spec_t), pointer :: current
    integer :: i, n_proc
    integer(i64) :: n_events_expected    
    n_proc = size (process_id)
    current => event_file_list%first
    allocate (integral (n_proc), error (n_proc))
    do i = 1, n_proc
       process => process_store_get_process_ptr (process_id(i))
       if (associated (process)) then
          integral(i) = process_get_integral (process)
          error(i) = process_get_error (process)
       else
          integral(i) = 0
          error(i) = 0
       end if
    end do
    n_events_expected = n_events
    do while (associated (current))
       select case (current%format)
       case (FMT_DEFAULT)
          call msg_message ("Writing events in human-readable format " &
               // "to file '" // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_DEBUG)
          call msg_message ("Writing events in verbose format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_HEPMC)
          call msg_message ("Writing events in HepMC format to file '" &
               // char (current%name) // "'")
          if (hepmc_is_available ()) then
             allocate (current%iostream)
             call hepmc_iostream_open_out (current%iostream, current%name)
          else
             call msg_error ("HepMC event writing is disabled " &
                  // "because HepMC library is not linked.")
          end if
       case (FMT_HEPEVT)   
          call msg_message ("Writing events in HEPEVT format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_ASCII_SHORT)   
          call msg_message ("Writing events in short ASCII format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_ASCII_LONG)   
          call msg_message ("Writing events in long ASCII format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_ATHENA)   
          call msg_message ("Writing events in ATHENA format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_LHEF)
          call msg_message ("Writing events in LHEF format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
          call les_houches_events_write_header (current%unit)
          call heprup_init &
               (flavor_get_pdg (current%beam_flv), &
                current%beam_energy, &
                n_processes = current%n_processes, &
                unweighted = current%unweighted, &
                negative_weights = current%negative_weights)            
          do i = 1, n_proc
             call heprup_set_process_parameters (i = i, process_id = &
                 i, cross_section = integral(i), error = error(i))
          end do
          call heprup_write_lhef (current%unit)
       case (FMT_LHA)
          call msg_message ("Writing events in (old) LHA format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
          call heprup_init &
               (flavor_get_pdg (current%beam_flv), &
                current%beam_energy, &
                n_processes = current%n_processes, &
                unweighted = current%unweighted, &
                negative_weights = current%negative_weights)            
          do i = 1, n_proc
             call heprup_set_process_parameters (i = i, process_id = &
                 i, cross_section = integral(i), error = error(i))
          end do
       case (FMT_STDHEP)
          call msg_message ("Writing events in binary STDHEP/HEPEVT format to file '" &
               // char (current%name) // "'")
          call stdhep_init (char(current%name), "WHIZARD event sample", &
               n_events_expected)     
       case (FMT_STDHEP_UP)
          call msg_message ("Writing events in binary STDHEP/HEPRUP/HEPEUP format to file '" &
               // char (current%name) // "'")
          call heprup_init &
               (flavor_get_pdg (current%beam_flv), &
                current%beam_energy, &
                n_processes = current%n_processes, &
                unweighted = current%unweighted, &
                negative_weights = current%negative_weights)                           
          do i = 1, n_proc
             call heprup_set_process_parameters (i = i, process_id = &
                 i, cross_section = integral(i), error = error(i))
          end do               
          call stdhep_init (char(current%name), "WHIZARD event sample", &
               n_events_expected)     
          call stdhep_write (STDHEP_HEPRUP)
       case (FMT_HEPEVT_VERB)
          call msg_message ("Writing events in verbose HEPEVT format to file '" &
               // char (current%name) // "'")
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
       case (FMT_LHA_VERB)
          call msg_message ("Writing events in verbose HEPRUP/HEPEUP format to file '" &
               // char (current%name) // "'")
          call heprup_init &
               (flavor_get_pdg (current%beam_flv), &
                current%beam_energy, &
                n_processes = current%n_processes, &
                unweighted = current%unweighted, &
                negative_weights = current%negative_weights)
          do i = 1, n_proc
             call heprup_set_process_parameters (i = i, process_id = &
                 i, cross_section = integral(i), error = error(i))
          end do               
          current%unit = free_unit ()
          open (unit=current%unit, file=char(current%name), &
               action="write", status="replace")
          call heprup_write_verbose (current%unit)
       end select
       current => current%next
    end do
  end subroutine event_file_list_open

  subroutine event_file_list_write_event &
       (event_file_list, event, integral_sum, error_sum, analysis_expr, i_evt)
    type(event_file_list_t), intent(in), target :: event_file_list
    type(event_t), intent(in), target :: event
    real(default), intent(in) :: integral_sum, error_sum
    type(eval_tree_t), intent(in) :: analysis_expr
    integer, intent(in) :: i_evt
    type(file_spec_t), pointer :: current
    type(hepmc_event_t) :: hepmc_event
    current => event_file_list%first
    do while (associated (current))
       select case (current%format)
       case (FMT_DEFAULT)
          call event_write (event, unit=current%unit, verbose=.false.)
       case (FMT_DEBUG)
          call event_write (event, analysis_expr=analysis_expr, &
               unit=current%unit, verbose=.true.)
       case (FMT_HEPMC)
          if (hepmc_is_available ()) then
             call hepmc_event_init (hepmc_event, event_id=i_evt)
             call hepmc_event_set_cross_section (hepmc_event, &
                  integral_sum, error_sum)
             call event_write_to_hepmc (event, hepmc_event)
            ! call hepmc_event_print (hepmc_event)
             call hepmc_iostream_write_event (current%iostream, hepmc_event)
             call hepmc_event_final (hepmc_event)
          end if
       case (FMT_HEPEVT)
          call event_write_to_hepevt (event, current%keep_beams)
          call hepevt_write_hepevt (current%unit)          
       case (FMT_ASCII_SHORT)
          call event_write_to_hepevt (event, current%keep_beams)
          call hepevt_write_ascii (current%unit, .false.)                 
       case (FMT_ASCII_LONG)
          call event_write_to_hepevt (event, current%keep_beams)
          call hepevt_write_ascii (current%unit, .true.)                  
       case (FMT_ATHENA)
          call event_write_to_hepevt (event, current%keep_beams)
          call hepevt_write_athena (unit=current%unit, i_evt=i_evt)
       case (FMT_LHEF)
          call event_write_to_hepeup (event, current%keep_beams)
          call hepeup_write_lhef (current%unit)
       case (FMT_LHA)
          call event_write_to_hepeup (event, current%keep_beams)
          call hepeup_write_lha (current%unit)
       case (FMT_STDHEP)
          call event_write_to_hepevt (event, current%keep_beams)
          call stdhep_write (STDHEP_HEPEVT)
       case (FMT_STDHEP_UP)
          call event_write_to_hepeup (event, current%keep_beams)
          call stdhep_write (STDHEP_HEPEUP)
       case (FMT_HEPEVT_VERB)
          call event_write_to_hepevt (event, current%keep_beams)
          call hepevt_write_verbose (current%unit)
       case (FMT_LHA_VERB)
          call event_write_to_hepeup (event, current%keep_beams)
          call hepeup_write_verbose (current%unit)
       end select
       current => current%next
    end do
  end subroutine event_file_list_write_event

  subroutine event_file_list_close (event_file_list)
    type(event_file_list_t), intent(inout), target :: event_file_list
    type(file_spec_t), pointer :: current
    current => event_file_list%first
    do while (associated (current))
       select case (current%format)
       case (FMT_HEPMC)
          if (hepmc_is_available ()) then
             call hepmc_iostream_close (current%iostream)
             deallocate (current%iostream)
          end if
       case (FMT_LHEF)          
          call les_houches_events_write_footer (current%unit)
          close (current%unit)
       case (FMT_STDHEP)
          call stdhep_end
       case (FMT_STDHEP_UP)
          call stdhep_end
       case default
          close (current%unit)
       end select
       current => current%next
    end do
  end subroutine event_file_list_close

  elemental function event_format_code (format) result (fmt)
    integer :: fmt
    type(string_t), intent(in) :: format
    select case (char (format))
    case ("ascii")
       fmt = FMT_DEFAULT
    case ("debug")
       fmt = FMT_DEBUG
    case ("hepmc")
       fmt = FMT_HEPMC
    case ("hepevt")
       fmt = FMT_HEPEVT
    case ("short")
       fmt = FMT_ASCII_SHORT
    case ("long")
       fmt = FMT_ASCII_LONG
    case ("athena")
       fmt = FMT_ATHENA
    case ("lhef")
       fmt = FMT_LHEF
    case ("lha")
       fmt = FMT_LHA
    case ("stdhep")
       fmt = FMT_STDHEP
    case ("stdhep_up")
       fmt = FMT_STDHEP_UP
    case ("hepevt_verbose")
       fmt = FMT_HEPEVT_VERB
    case ("lha_verbose")
       fmt = FMT_LHA_VERB
    case default
       fmt = FMT_NONE
    end select
  end function event_format_code


end module event_files
