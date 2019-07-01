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

program main

  use iso_varying_string, string_t => varying_string
  use system_dependencies
  use diagnostics
  use ifiles
  use os_interface
  use whizard

  use cmdline_options
  use features

  implicit none

  integer, parameter :: CMDLINE_ARG_LEN = 1000

!!! (WK 02/2016) Interface for the separate external routine below
  interface
     subroutine print_usage ()
     end subroutine print_usage
  end interface

! Main program variable declarations
  character(CMDLINE_ARG_LEN) :: arg
  character(2) :: option
  type(string_t) :: long_option, value
  integer :: i, j, arg_len, arg_status, area
  logical :: look_for_options
  logical :: interactive
  logical :: banner
  type(string_t) :: files, this, model, default_lib, library, libraries
  type(string_t) :: logfile
  logical :: user_code_enable = .false.
  integer :: n_user_src = 0, n_user_lib = 0
  type(string_t) :: user_src, user_lib, user_target
  type(paths_t) :: paths
  logical :: rebuild_library, rebuild_user
  logical :: rebuild_phs, rebuild_grids, rebuild_events
  logical :: recompile_library
  type(ifile_t) :: commands
  type(string_t) :: command

  type(whizard_options_t), allocatable :: options
  type(whizard_t), allocatable, target :: whizard_instance

  ! Exit status
  logical :: quit = .false.
  integer :: quit_code = 0

  ! Initial values
  look_for_options = .true.
  interactive = .false.
  files = ""
  model = "SM"
  default_lib = "default_lib"
  library = ""
  libraries = ""
  banner = .true.
  logging = .true.
  msg_level = RESULT
  logfile = "whizard.log"
  user_src = ""
  user_lib = ""
  user_target = ""
  rebuild_library = .false.
  rebuild_user = .false.
  rebuild_phs = .false.
  rebuild_grids = .false.
  rebuild_events = .false.
  recompile_library = .false.
  call paths_init (paths)

  ! Read and process options
  call init_options (print_usage)
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
              cycle scan_cmdline
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
           case ("--libtool")
              paths%libtool = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--lhapdfdir")
              paths%lhapdfdir = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--check")
              call print_usage ()
              call msg_fatal ("Option --check not supported &
                   &(for unit tests, run whizard_ut instead)")
           case ("--show-config")
              call no_option_value (long_option, value)
              call print_features (); stop
           case ("--execute")
              command = get_option_value (i, long_option, value)
              call ifile_append (commands, command)
              cycle SCAN_CMDLINE
           case ("--interactive")
              call no_option_value (long_option, value)
              interactive = .true.
              cycle SCAN_CMDLINE
           case ("--library")
              library = get_option_value (i, long_option, value)
              libraries = libraries // " " // library
              cycle SCAN_CMDLINE
           case ("--no-library")
              call no_option_value (long_option, value)
              default_lib = ""
              library = ""
              libraries = ""
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
           case ("--logging")
              call no_option_value (long_option, value)
              logging = .true.
              cycle SCAN_CMDLINE
           case ("--no-logging")
              call no_option_value (long_option, value)
              logging = .false.
              cycle SCAN_CMDLINE
           case ("--debug")
              call no_option_value (long_option, value)
              area = d_area (get_option_value (i, long_option, value))
              msg_level(area) = DEBUG
              cycle SCAN_CMDLINE
           case ("--debug2")
              call no_option_value (long_option, value)
              area = d_area (get_option_value (i, long_option, value))
              msg_level(area) = DEBUG2
              cycle SCAN_CMDLINE
           case ("--single-event")
              call no_option_value (long_option, value)
              single_event = .true.
              cycle SCAN_CMDLINE
           case ("--banner")
              call no_option_value (long_option, value)
              banner = .true.
              cycle SCAN_CMDLINE
           case ("--no-banner")
              call no_option_value (long_option, value)
              banner = .false.
              cycle SCAN_CMDLINE
           case ("--model")
              model = get_option_value (i, long_option, value)
              cycle SCAN_CMDLINE
           case ("--no-model")
              call no_option_value (long_option, value)
              model = ""
              cycle SCAN_CMDLINE
           case ("--rebuild")
              call no_option_value (long_option, value)
              rebuild_library = .true.
              rebuild_user = .true.
              rebuild_phs = .true.
              rebuild_grids = .true.
              rebuild_events = .true.
              cycle SCAN_CMDLINE
           case ("--no-rebuild")
              call no_option_value (long_option, value)
              rebuild_library = .false.
              recompile_library = .false.
              rebuild_user = .false.
              rebuild_phs = .false.
              rebuild_grids = .false.
              rebuild_events = .false.
              cycle SCAN_CMDLINE
           case ("--rebuild-library")
              call no_option_value (long_option, value)
              rebuild_library = .true.
              cycle SCAN_CMDLINE
           case ("--rebuild-user")
              call no_option_value (long_option, value)
              rebuild_user = .true.
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
           case ("--user")
              user_code_enable = .true.
              cycle SCAN_CMDLINE
           case ("--user-src")
              if (user_src == "") then
                 user_src = get_option_value (i, long_option, value)
              else
                 user_src = user_src // " " &
                      // get_option_value (i, long_option, value)
              end if
              n_user_src = n_user_src + 1
              cycle SCAN_CMDLINE
           case ("--user-lib")
              if (user_lib == "") then
                 user_lib = get_option_value (i, long_option, value)
              else
                 user_lib = user_lib // " " &
                      // get_option_value (i, long_option, value)
              end if
              n_user_lib = n_user_lib + 1
              cycle SCAN_CMDLINE
           case ("--user-target")
              user_target = get_option_value (i, long_option, value)
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
                 case ("-e")
                    command = get_option_value (i, var_str (option))
                    call ifile_append (commands, command)
                    cycle SCAN_CMDLINE
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
                    rebuild_user = .true.
                    rebuild_phs = .true.
                    rebuild_grids = .true.
                    rebuild_events = .true.
                    cycle SCAN_SHORT_OPTIONS
                 case ("-u")
                    user_code_enable = .true.
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
  if (banner)  call msg_banner ()

   allocate (options)
   allocate (whizard_instance)

   if (.not. quit) then

      ! Set options and initialize the whizard object
      options%preload_model = model
      options%default_lib = default_lib
      options%preload_libraries = libraries
      options%rebuild_library = rebuild_library
      options%recompile_library = recompile_library
      options%rebuild_user = rebuild_user
      options%rebuild_phs = rebuild_phs
      options%rebuild_grids = rebuild_grids
      options%rebuild_events = rebuild_events

      call whizard_instance%init (options, paths, logfile)

      call mask_term_signals ()

   end if

   ! Run commands given on the command line
   if (.not. quit .and. ifile_get_length (commands) > 0) then
      call whizard_instance%process_ifile (commands, quit, quit_code)
   end if

   if (.not. quit) then
      ! Process commands from standard input
      if (.not. interactive .and. files == "") then
         call whizard_instance%process_stdin (quit, quit_code)

         ! ... or process commands from file
      else
         files = trim (adjustl (files))
         SCAN_FILES: do while (files /= "")
            call split (files, this, " ")
            call whizard_instance%process_file (this, quit, quit_code)
            if (quit)  exit SCAN_FILES
         end do SCAN_FILES

      end if
  end if

  ! Enter an interactive shell if requested
  if (.not. quit .and. interactive) then
     call whizard_instance%shell (quit_code)
  end if

  ! Overall finalization
  call ifile_final (commands)

  deallocate (options)

  call whizard_instance%final ()
  deallocate (whizard_instance)

  call terminate_now_if_signal ()
  call release_term_signals ()
  call msg_terminate (quit_code = quit_code)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains

  subroutine print_version ()
    print "(A)", "WHIZARD " // WHIZARD_VERSION
    print "(A)", "Copyright (C) 1999-2017 Wolfgang Kilian, Thorsten Ohl, Juergen Reuter"
    print "(A)", "              ---------------------------------------                "
    print "(A)", "This is free software; see the source for copying conditions.  There is NO"
    print "(A)", "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
    print *
  end subroutine print_version

end program main

!!! (WK 02/2016)
!!! Separate subroutine, because this becomes a procedure pointer target
!!! Internal procedures as targets are not supported by some compilers.

  subroutine print_usage ()
    use system_dependencies, only: WHIZARD_VERSION
    print "(A)", "WHIZARD " // WHIZARD_VERSION
    print "(A)", "Usage: whizard [OPTIONS] [FILE]"
    print "(A)", "Run WHIZARD with the command list taken from FILE(s)"
    print "(A)", "Options for resetting default directories and tools" &
            // "(GNU naming conventions):"
    print "(A)", "    --prefix DIR"
    print "(A)", "    --exec_prefix DIR"
    print "(A)", "    --bindir DIR"
    print "(A)", "    --libdir DIR"
    print "(A)", "    --includedir DIR"
    print "(A)", "    --datarootdir DIR"
    print "(A)", "    --libtool LOCAL_LIBTOOL"
    print "(A)", "    --lhapdfdir DIR   (PDF sets directory)"
    print "(A)", "Other options:"
    print "(A)", "-h, --help            display this help and exit"
    print "(A)", "    --banner          display banner at startup (default)"
    print "(A)", "    --debug AREA      switch on debug output for AREA."
    print "(A)", "                      AREA can be one of Whizard's src dirs or 'all'"
    print "(A)", "    --debug2 AREA     switch on more verbose debug output for AREA."
    print "(A)", "    --single-event    only compute one phase-space point (for debugging)"
    print "(A)", "-e, --execute CMDS    execute SINDARIN CMDS before reading FILE(s)"
    print "(A)", "-i, --interactive     run interactively after reading FILE(s)"
    print "(A)", "-l, --library         preload process library NAME"
    print "(A)", "    --localprefix DIR"
    print "(A)", "                      search in DIR for local models (default: ~/.whizard)"
    print "(A)", "-L, --logfile FILE    write log to FILE (default: 'whizard.log'"
    print "(A)", "    --logging         switch on logging at startup (default)"
    print "(A)", "-m, --model NAME      preload model NAME (default: 'SM')"
    print "(A)", "    --no-banner       do not display banner at startup"
    print "(A)", "    --no-library      do not preload process library"
    print "(A)", "    --no-logfile      do not write a logfile"
    print "(A)", "    --no-logging      switch off logging at startup"
    print "(A)", "    --no-model        do not preload a model"
    print "(A)", "    --no-rebuild      do not force rebuilding"
    print "(A)", "-r, --rebuild         rebuild all (see below)"
    print "(A)", "    --rebuild-library"
    print "(A)", "                      rebuild process code library"
    print "(A)", "    --rebuild-user    rebuild user-provided code"
    print "(A)", "    --rebuild-phase-space"
    print "(A)", "                      rebuild phase-space configuration"
    print "(A)", "    --rebuild-grids   rebuild integration grids"
    print "(A)", "    --rebuild-events  rebuild event samples"
    print "(A)", "    --recompile       recompile process code"
    print "(A)", "    --show-config     show build-time configuration"
    print "(A)", "-u  --user            enable user-provided code"
    print "(A)", "    --user-src FILE   user-provided source file"
    print "(A)", "    --user-lib FILE   user-provided library file"
    print "(A)", "    --user-target BN  basename of created user library (default: user)"
    print "(A)", "-V, --version         output version information and exit"
    print "(A)", "    --write-syntax-tables"
    print "(A)", "                      write the internal syntax tables to files and exit"
    print "(A)", "-                     further options are taken as filenames"
    print *
    print "(A)", "With no FILE, read standard input."
  end subroutine print_usage

