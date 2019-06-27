! WHIZARD 2.0.0 Mon Apr 12 2010
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

program main

  use iso_varying_string, string_t => varying_string !NODEP!
  use system_dependencies !NODEP!
  use limits, only: CMDLINE_ARG_LEN !NODEP!
  use diagnostics !NODEP!
  use os_interface
  use whizard

  implicit none

  ! Main program variable declarations
  character(CMDLINE_ARG_LEN) :: arg
  character(2) :: option
  type(string_t) :: long_option, value
  integer :: i, j, arg_len, arg_status
  logical :: look_for_options
  logical :: interactive
  type(string_t) :: files, this, model, libname, library, libraries, logfile
  type(string_t) :: check, checks
  type(paths_t) :: paths
  logical :: rebuild_library, rebuild_phs, rebuild_grids, rebuild_events
  logical :: recompile_library
  logical :: time_estimate

  ! Exit status
  logical :: quit = .false.
  integer :: quit_code = 0

  ! Initial values
  look_for_options = .true.
  interactive = .false.
  files = ""
  model = "SM"
  libname = "processes"
  library = ""
  logfile = "whizard.log"
  libraries = ""
  check = ""
  checks = ""
  rebuild_library = .false.
  rebuild_phs = .false.
  rebuild_grids = .false.
  rebuild_events = .false.
  recompile_library = .false.
  time_estimate = .true.
  call paths_init (paths)

  ! Read and process options
  i = 0
  SCAN_CMDLINE: do
     i = i + 1
     call get_command_argument (i, arg, arg_len, arg_status)
     select case (arg_status)
     case (0)
     case (-1)
        call msg_error (" Command argument truncated: '" // arg // "'")
     case default
        exit SCAN_CMDLINE
     end select
     if (look_for_options) then
        select case (arg(1:2))
        case ("--")
           value = trim (arg)
           call split (value, long_option, "=")
           select case (char (long_option))
           case ("--version")
              call no_option_value (long_option, value)
              call print_version (); stop
           case ("--help")
              call no_option_value (long_option, value)
              call print_usage (); stop
           case ("--prefix")
              paths%prefix = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--exec-prefix")
              paths%exec_prefix = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--bindir")
              paths%bindir = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--libdir")
              paths%libdir = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--includedir")
              paths%includedir = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--datarootdir")
              paths%datarootdir = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--check")
              check = get_option_value (i, long_option, value)
              checks = checks // " " // check
              cycle SCAN_CMDLINE
           case ("--interactive")
              call no_option_value (long_option, value)
              interactive = .true.
              cycle SCAN_CMDLINE
           case ("--library")
              library = get_option_value (i, long_option, value)
              libraries = libraries // " " // library
              cycle SCAN_CMDLINE
           case ("--localprefix")
              paths%localprefix = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--logfile")
              logfile = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--no-logfile")
              call no_option_value (long_option, value)
              logfile = ""
              cycle SCAN_CMDLINE
           case ("--model")
              model = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--rebuild")
              call no_option_value (long_option, value)
              rebuild_library = .true.
              rebuild_phs = .true.
              rebuild_grids = .true.
              rebuild_events = .true.
              cycle SCAN_CMDLINE
           case ("--rebuild-library")
              call no_option_value (long_option, value)
              rebuild_library = .true.
              cycle SCAN_CMDLINE
           case ("--rebuild-phase-space")
              call no_option_value (long_option, value)
              rebuild_phs = .true.
              cycle SCAN_CMDLINE
           case ("--rebuild-grids")
              call no_option_value (long_option, value)
              rebuild_grids = .true.
              cycle SCAN_CMDLINE
           case ("--rebuild-events")
              call no_option_value (long_option, value)
              rebuild_events = .true.
              cycle SCAN_CMDLINE
           case ("--recompile")
              call no_option_value (long_option, value)
              recompile_library = .true.
              rebuild_grids = .true.
              cycle SCAN_CMDLINE
           case ("--time-estimate")
              call no_option_value (long_option, value)
              time_estimate = .true.
              cycle SCAN_CMDLINE
           case ("--no-time-estimate")
              call no_option_value (long_option, value)
              time_estimate = .false.
              cycle SCAN_CMDLINE
           case ("--write-syntax-tables")
              call no_option_value (long_option, value)
              call init_syntax_tables ()
              call write_syntax_tables ()
              call final_syntax_tables ()
              stop
              cycle SCAN_CMDLINE
           case default
              call print_usage ()
              call msg_fatal ("Option '" // trim (arg) // "' not recognized")
           end select
        end select
        select case (arg(1:1))
        case ("-")
           j = 1
           if (len_trim (arg) == 1) then
              look_for_options = .false.
           else
              SCAN_SHORT_OPTIONS: do
                 j = j + 1
                 if (j > len_trim (arg)) exit SCAN_SHORT_OPTIONS
                 option = "-" // arg(j:j)
                 select case (option)
                 case ("-V")
                    call print_version (); stop
                 case ("-?", "-h")
                    call print_usage (); stop
                 case ("-i")
                    interactive = .true.
                    cycle SCAN_SHORT_OPTIONS
                 case ("-l")
                    if (j == len_trim (arg)) then
                       library = get_option_value (i, var_str (option))
                    else
                       library = trim (arg(j+1:))
                    end if
                    libraries = libraries // " " // library
                    cycle SCAN_CMDLINE
                 case ("-L")
                    if (j == len_trim (arg)) then
                       logfile = get_option_value (i, var_str (option))
                    else
                       logfile = trim (arg(j+1:))
                    end if
                    cycle SCAN_CMDLINE
                 case ("-m")
                    if (j < len_trim (arg))  call msg_fatal &
                         ("Option '" // option // "' needs a value")
                    model = get_option_value (i, var_str (option))
                    cycle SCAN_CMDLINE
                 case ("-r")
                    rebuild_library = .true.
                    rebuild_phs = .true.
                    rebuild_grids = .true.
                    rebuild_events = .true.
                    cycle SCAN_SHORT_OPTIONS
                 case default
                    call print_usage ()
                    call msg_fatal &
                         ("Option '" // option // "' not recognized")
                 end select
              end do SCAN_SHORT_OPTIONS
           end if
        case default
           files = files // " " // trim (arg)
        end select
     else
        files = files // " " // trim (arg)
     end if
  end do SCAN_CMDLINE

  ! Overall initialization
  if (logfile /= "")  call logfile_init (logfile)
  call msg_banner ()
  call whizard_init &
       (preload_model=model, preload_libs=libraries, default_lib=libname, &
        rebuild_library=rebuild_library, &
        rebuild_phs=rebuild_phs, &
        rebuild_grids=rebuild_grids, &
        rebuild_events=rebuild_events, &
        recompile_library=recompile_library, &
        time_estimate=time_estimate, &
        paths=paths)

  ! Run any self-checks (and no commands)
  if (checks /= "") then

     checks = trim (adjustl (checks))
     RUN_CHECKS: do while (checks /= "")
        call split (checks, check, " ")
        call whizard_check (check)
     end do RUN_CHECKS

  ! Process commands from standard input
  else if (.not. interactive .and. files == "") then
     call whizard_process_stdin (quit, quit_code)

  ! Process commands from file
  else
     files = trim (adjustl (files))
     SCAN_FILES: do while (files /= "")
        call split (files, this, " ")
        call whizard_process_file (this, quit, quit_code)
        if (quit)  exit SCAN_FILES
     end do SCAN_FILES

  end if 

  ! Enter an interactive shell if requested
  if (.not. quit .and. interactive) then
     call whizard_shell (quit_code)
  end if

  ! Overall finalization
  call whizard_final ()
  call msg_terminate (quit_code = quit_code)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains

  subroutine no_option_value (option, value)
    type(string_t), intent(in) :: option, value
    if (value /= "") then
       call msg_error (" Option '" // char (option) // "' should have no value")
    end if
  end subroutine no_option_value

  function get_option_value (i, option, value) result (string)
    type(string_t) :: string
    integer, intent(inout) :: i
    type(string_t), intent(in) :: option
    type(string_t), intent(in), optional :: value
    character(CMDLINE_ARG_LEN) :: arg
    character(CMDLINE_ARG_LEN) :: arg_value
    integer :: arg_len, arg_status
    logical :: has_value
    if (present (value)) then
       has_value = value /= ""
    else
       has_value = .false.
    end if
    if (has_value) then
       string = value
    else
       i = i + 1 
       call get_command_argument (i, arg_value, arg_len, arg_status)
       select case (arg_status)
       case (0)
       case (-1)
          call msg_error (" Option value truncated: '" // arg // "'")
       case default
          call print_usage ()
          call msg_fatal (" Option '" // char (option) // "' needs a value")
       end select
       select case (arg(1:1))
       case ("-")
          call print_usage ()
          call msg_fatal (" Option '" // char (option) // "' needs a value")
       end select
       string = trim (arg_value)
    end if
  end function get_option_value

  subroutine print_version ()
    print "(A)", "WHIZARD " // WHIZARD_VERSION 
    print "(A)", "Copyright (C) 1999-2010 Wolfgang Kilian, Thorsten Ohl, Juergen Reuter"
    print "(A)", "This is free software; see the source for copying conditions.  There is NO"
    print "(A)", "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
    print *
  end subroutine print_version

  subroutine print_usage ()
    print "(A)", "WHIZARD " // WHIZARD_VERSION
    print "(A)", "Usage: whizard [OPTIONS] [FILE]"
    print "(A)", "Run WHIZARD with the command list taken from FILE(s)"
    print "(A)", "Options for resetting default directories " &
            // "(GNU naming conventions):"
    print "(A)", "    --prefix DIR"
    print "(A)", "    --exec_prefix DIR"
    print "(A)", "    --bindir DIR"
    print "(A)", "    --libdir DIR"
    print "(A)", "    --includedir DIR"
    print "(A)", "    --datarootdir DIR"
    print "(A)", "Other options:"
    print "(A)", "-h, --help            display this help and exit"
    print "(A)", "-i, --interactive     run interactively after reading FILE(s)"
    print "(A)", "-l, --library         preload process library NAME"
    print "(A)", "    --localprefix DIR"
    print "(A)", "                      search in DIR for local models (default: ~/.whizard)"
    print "(A)", "-L, --logfile FILE    write log to FILE (default: 'whizard.log'"
    print "(A)", "-m, --model NAME      preload model NAME (default: 'SM')"
    print "(A)", "    --no-logfile      do not write a logfile"
    print "(A)", "-r, --rebuild         rebuild process code, phase space, integration grids, and events"
    print "(A)", "    --rebuild-library"
    print "(A)", "                      rebuild process code library"
    print "(A)", "    --rebuild-phase-space"
    print "(A)", "                      rebuild phase-space configuration"
    print "(A)", "    --rebuild-grids   rebuild integration grids"
    print "(A)", "    --rebuild-events  rebuild event samples"
    print "(A)", "    --recompile       recompile process code, ignoring any existing library"
    print "(A)", "-V, --version         output version information and exit"
    print "(A)", "    --write-syntax-tables"
    print "(A)", "                      write the internal syntax tables to files and exit"
    print "(A)", "-                     further options are taken as filenames"
    print *
    print "(A)", "With no FILE, read standard input."
  end subroutine print_usage

end program main
