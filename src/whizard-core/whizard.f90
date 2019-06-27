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

module whizard

  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: VERSION_STRING !NODEP!
  use limits, only: EOF, BACKSLASH !NODEP!
  use diagnostics !NODEP!
  use formats
  use md5
  use os_interface
  use lexers
  use parser
  use colors
  use state_matrices
  use analysis
  use variables
  use expressions
  use models
  use evaluators
  use phs_forests
  use hard_interactions
  use processes
  use decays
  use process_libraries
  use slha_interface
  use commands
  use vamp !NODEP!

  implicit none
  private

  public :: whizard_init
  public :: init_syntax_tables
  public :: final_syntax_tables
  public :: write_syntax_tables
  public :: whizard_final
  public :: whizard_process_stdin
  public :: whizard_process_file
  public :: whizard_shell
  public :: whizard_check

  type(rt_data_t), target :: global

  save

contains

  subroutine whizard_init &
       (preload_model, preload_libs, default_lib, &
        rebuild_library, rebuild_phs, rebuild_grids, rebuild_events, &
        recompile_library, &
        time_estimate, &
        paths)
    type(string_t), intent(in) :: preload_model, preload_libs
    type(string_t), intent(in) :: default_lib
    logical, intent(in) :: rebuild_library, rebuild_phs, rebuild_grids
    logical, intent(in) :: rebuild_events
    logical, intent(in) :: recompile_library
    logical, intent(in) :: time_estimate
    type(paths_t), intent(in), optional :: paths
    type(string_t) :: filename, libname, libs
    type(var_list_t), pointer :: model_vars
    call rt_data_global_init (global, paths)
    call var_list_append_log &
         (global%var_list, var_str ("?rebuild_library"), rebuild_library, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?rebuild_phase_space"), rebuild_phs, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?rebuild_grids"), rebuild_grids, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?rebuild_events"), rebuild_events, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?recompile_library"), recompile_library, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?time_estimate"), time_estimate, &
          intrinsic=.true.)
    call init_syntax_tables ()
    call process_library_store_load_static &
         (global%os_data, global%prc_lib, global%model, global%var_list)
    libs = adjustl (preload_libs)
    SCAN_LIBS: do while (libs /= "")
       call split (libs, libname, " ")
       call process_library_store_append &
            (libname, global%os_data, global%prc_lib)
       call process_library_load &
            (global%prc_lib, global%os_data, global%model, global%var_list, &
             ignore=.true.)
    end do SCAN_LIBS
    if (.not. associated (global%prc_lib)) then
       call process_library_store_append &
            (default_lib, global%os_data, global%prc_lib)
       if (.not. (rebuild_library .or. recompile_library)) then
          call process_library_load (global%prc_lib, &
               global%os_data, global%model, global%var_list, ignore=.true.)
       else
          call var_list_set_string (global%var_list, &
               var_str ("$library_name"), &
               process_library_get_name (global%prc_lib), is_known=.true.)
       end if
    end if
    filename = preload_model // ".mdl"
    call model_list_read_model &
         (preload_model, filename, global%os_data, global%model)
    if (associated (global%model)) then
       model_vars => model_get_var_list_ptr (global%model)
       call var_list_init_copies (global%var_list, model_vars)
       call var_list_synchronize &
            (global%var_list, model_vars, reset_pointers = .true.)
       call msg_message ("Using model: " &
            // char (model_get_name (global%model)))
       call var_list_set_string (global%var_list, var_str ("$model_name"), &
            model_get_name (global%model), is_known=.true.)
    end if
  end subroutine whizard_init

  subroutine init_syntax_tables ()
    call syntax_model_file_init ()
    call syntax_phs_forest_init ()
    call syntax_pexpr_init ()
    call syntax_slha_init ()
    call syntax_cmd_list_init ()
  end subroutine init_syntax_tables

  subroutine final_syntax_tables ()
    call syntax_model_file_final ()
    call syntax_phs_forest_final ()
    call syntax_pexpr_final ()
    call syntax_slha_final ()
    call syntax_cmd_list_final ()
  end subroutine final_syntax_tables

  subroutine write_syntax_tables ()
    integer :: unit
    character(*), parameter :: file_model = "whizard.model_file.syntax"
    character(*), parameter :: file_phs = "whizard.phase_space_file.syntax"
    character(*), parameter :: file_pexpr = "whizard.prt_expressions.syntax"
    character(*), parameter :: file_slha = "whizard.slha.syntax"
    character(*), parameter :: file_sindarin = "whizard.sindarin.syntax"
    unit = free_unit ()
    print *, "Writing file '" // file_model // "'"
    open (unit=unit, file=file_model, status="replace", action="write")
    write (unit, "(A)")  VERSION_STRING
    write (unit, "(A)")  "Syntax definition file: " // file_model
    call syntax_model_file_write (unit)
    close (unit)
    print *, "Writing file '" // file_phs // "'"
    open (unit=unit, file=file_phs, status="replace", action="write")
    write (unit, "(A)")  VERSION_STRING
    write (unit, "(A)")  "Syntax definition file: " // file_phs
    call syntax_phs_forest_write (unit)
    close (unit)
    print *, "Writing file '" // file_pexpr // "'"
    open (unit=unit, file=file_pexpr, status="replace", action="write")
    write (unit, "(A)")  VERSION_STRING
    write (unit, "(A)")  "Syntax definition file: " // file_pexpr
    call syntax_pexpr_write (unit)
    close (unit)
    print *, "Writing file '" // file_slha // "'"
    open (unit=unit, file=file_slha, status="replace", action="write")
    write (unit, "(A)")  VERSION_STRING
    write (unit, "(A)")  "Syntax definition file: " // file_slha
    call syntax_slha_write (unit)
    close (unit)
    print *, "Writing file '" // file_sindarin // "'"
    open (unit=unit, file=file_sindarin, status="replace", action="write")
    write (unit, "(A)")  VERSION_STRING
    write (unit, "(A)")  "Syntax definition file: " // file_sindarin
    call syntax_cmd_list_write (unit)
    close (unit)
  end subroutine write_syntax_tables

  subroutine whizard_final ()
    call rt_data_global_final (global)
    call decay_store_final ()
    call process_store_final ()
    call model_list_final ()
    call final_syntax_tables ()
  end subroutine whizard_final

  subroutine whizard_process_stdin (quit, quit_code)
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    call msg_message ("Reading commands from standard input")
    call lexer_init_cmd_list (lexer)
    call stream_init (stream, 5)
    call whizard_process_stream (stream, lexer, quit, quit_code)
    call stream_final (stream)
    call lexer_final (lexer)
  end subroutine whizard_process_stdin

  subroutine whizard_process_file (file, quit, quit_code)
    type(string_t), intent(in) :: file
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    integer :: u
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    logical :: exist
    call msg_message ("Reading commands from file '" // char (file) // "'")
    inquire (file=char(file), exist=exist)
    if (exist) then
       u = free_unit ()
       call lexer_init_cmd_list (lexer)
       call stream_init (stream, char (file))
       call whizard_process_stream (stream, lexer, quit, quit_code)
       call stream_final (stream)
       call lexer_final (lexer)
    else
       call msg_error ("File '" // char (file) // "' not found")
    end if
  end subroutine whizard_process_file

  subroutine whizard_process_stream (stream, lexer, quit, quit_code)
    type(stream_t), intent(inout), target :: stream
    type(lexer_t), intent(inout), target :: lexer
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(parse_tree_t) :: parse_tree
    type(command_list_t), target :: command_list
    global%lexer => lexer
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    ! call parse_tree_write (parse_tree)
    if (associated (parse_tree_get_root_ptr (parse_tree))) then
       call command_list_compile &
            (command_list, parse_tree_get_root_ptr (parse_tree), global)
    end if
    ! call command_list_write (command_list)
    ! call command_list_execute (command_list, global, print=.true.)
    call command_list_execute (command_list, global)
    call command_list_final (command_list)
    quit = global%quit
    quit_code = global%quit_code
  end subroutine whizard_process_stream

  subroutine whizard_shell (quit_code)
    integer, intent(out) :: quit_code
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    type(string_t) :: prompt1
    type(string_t) :: prompt2
    type(string_t) :: input
    type(string_t) :: extra
    integer :: last
    integer :: iostat
    logical :: mask_tmp
    logical :: quit
    call msg_message ("Launching interactive shell")
    call lexer_init_cmd_list (lexer)
    prompt1 = "whish? "
    prompt2 = "     > "
    COMMAND_LOOP: do
       call put (6, prompt1)
       call get (5, input, iostat=iostat)
       if (iostat > 0 .or. iostat == EOF) exit COMMAND_LOOP
       CONTINUE_INPUT: do
          last = len_trim (input)
          if (extract (input, last, last) /= BACKSLASH)  exit CONTINUE_INPUT
          call put (6, prompt2)
          call get (5, extra, iostat=iostat)
          if (iostat > 0) exit COMMAND_LOOP
          input = replace (input, last, extra)
       end do CONTINUE_INPUT
       call stream_init (stream, input)
       mask_tmp = mask_fatal_errors
       mask_fatal_errors = .true.
       call whizard_process_stream (stream, lexer, quit, quit_code)
       msg_count = 0
       mask_fatal_errors = mask_tmp
       call stream_final (stream)
       if (quit)  exit COMMAND_LOOP
    end do COMMAND_LOOP
    print *
    call lexer_final (lexer)
  end subroutine whizard_shell

  subroutine whizard_check (check)
    type(string_t), intent(in) :: check
    call msg_message (repeat ('=', 76), 0)
    call msg_message ("Running self-test: " // char (check), 0)
    call msg_message (repeat ('-', 76), 0)
    select case (char (check))
    case ("formats");  call format_test ()
    case ("md5");  call md5_test ()
    case ("colors");  call color_test ()
    case ("state_matrices");  call state_matrix_test ()
    case ("analysis");  call analysis_test ()
    case ("expressions");  call expressions_test ()
    case ("hard_interactions");  call hard_interaction_test (global%model)
    case ("evaluators");  call evaluator_test (global%model)
    case ("slha_interface");  call slha_test ()
    case default
       call msg_error ("Self-test '" // char (check) // "' not implemented.")
    end select
  end subroutine whizard_check


end module whizard
