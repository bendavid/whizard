! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  subroutine c_whizard_convert_string (c_string, f_string)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    character(kind=c_char), intent(in) :: c_string(*)
    type(string_t), intent(inout) :: f_string
    character(len=1) :: dummy_char
    integer :: dummy_i = 1
  
    f_string = ""
    do
       if (c_string(dummy_i) == c_null_char) then
          exit
       else if (c_string(dummy_i) == c_new_line) then
          dummy_char = CHAR(13)
          f_string = f_string // dummy_char
          dummy_char = CHAR(10)
       else
          dummy_char = c_string (dummy_i)
       end if
       f_string = f_string // dummy_char
       dummy_i = dummy_i + 1
    end do
    dummy_i = 1
  end subroutine c_whizard_convert_string
  
  subroutine c_whizard_commands (w_c_instance, cmds)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
    use commands
    use diagnostics !NODEP!
    use lexers
    use models
    use parser
    use whizard

    type(c_ptr), intent(inout) :: w_c_instance
    type(whizard_t), pointer :: whizard_instance
    type(string_t) :: cmds
    type(parse_tree_t) :: parse_tree
    type(parse_node_t), pointer :: pn_root
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    type(command_list_t), target :: cmd_list

    call c_f_pointer (w_c_instance, whizard_instance)
    call lexer_init_cmd_list (lexer)
    call syntax_cmd_list_init ()
  
    call stream_init (stream, cmds)
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    pn_root => parse_tree_get_root_ptr (parse_tree)
  
    if (associated (pn_root)) then
       call cmd_list%compile (pn_root, whizard_instance%global)
    end if
    call cmd_list%execute (whizard_instance%global)
    call cmd_list%final ()
  
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call lexer_final (lexer)
    call syntax_cmd_list_final ()
  end subroutine c_whizard_commands

  subroutine c_whizard_init (w_c_instance) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!    
    use system_dependencies !NODEP!
    use limits, only: CMDLINE_ARG_LEN !NODEP!
    use diagnostics !NODEP!
    use unit_tests
    use ifiles
    use os_interface
    use whizard
  
    implicit none
  
    type(c_ptr), intent(out) :: w_c_instance    
    character(CMDLINE_ARG_LEN) :: arg
    character(2) :: option
    type(string_t) :: long_option, value
    integer :: i, j, arg_len, arg_status
    logical :: look_for_options
    logical :: interactive
    logical :: banner
    type(string_t) :: files, this, model, default_lib, library, libraries
    type(string_t) :: check, checks, logfile
    type(test_results_t) :: test_results
    logical :: success
    logical :: user_code_enable = .false.
    integer :: n_user_src = 0, n_user_lib = 0
    type(string_t) :: user_src, user_lib
    type(paths_t) :: paths
    logical :: rebuild_library, rebuild_user
    logical :: rebuild_phs, rebuild_grids, rebuild_events
    logical :: recompile_library
    type(ifile_t) :: commands
    type(string_t) :: command

    type(whizard_options_t), allocatable :: options
    type(whizard_t), pointer :: whizard_instance
    
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
    logfile = "whizard.log"
    check = ""
    checks = ""
    user_src = ""
    user_lib = ""
    rebuild_library = .false.
    recompile_library = .false.
    rebuild_user = .false.
    rebuild_phs = .false.
    rebuild_grids = .false.
    rebuild_events = .false.
    call paths_init (paths)
  
    ! Overall initialization
    if (logfile /= "")  call logfile_init (logfile)
    call mask_term_signals ()
    if (banner)  call msg_banner ()
    
    ! Set options and initialize the whizard object
    allocate (options)
    options%preload_model = model
    options%default_lib = default_lib
    options%preload_libraries = libraries
    options%rebuild_library = rebuild_library
    options%rebuild_user = rebuild_user
    options%rebuild_phs = rebuild_phs
    options%rebuild_grids = rebuild_grids
    options%rebuild_events = rebuild_events
    
    allocate (whizard_instance)
    call whizard_instance%init (options, paths)
      
    if (checks /= "") then
       checks = trim (adjustl (checks))
       RUN_CHECKS: do while (checks /= "")
          call split (checks, check, " ")
          call whizard_check (check, test_results)
       end do RUN_CHECKS
       call test_results%wrapup (6, success)
       if (.not. success)  quit_code = 7
       quit = .true.
    end if
    
    w_c_instance = c_loc (whizard_instance)
              
  end subroutine c_whizard_init
  
  subroutine c_whizard_finalize (w_c_instance) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
    use system_dependencies !NODEP!
    use limits, only: CMDLINE_ARG_LEN !NODEP!
    use diagnostics !NODEP!
    use ifiles
    use os_interface
    use whizard
    
    type(c_ptr), intent(in) :: w_c_instance
    type(whizard_t), pointer :: whizard_instance
    logical :: quit = .false.
    integer :: quit_code = 0
      
    call c_f_pointer (w_c_instance, whizard_instance)
    call whizard_instance%final ()
    deallocate (whizard_instance)
    call terminate_now_if_signal ()
    call release_term_signals ()
    call msg_terminate (quit_code = quit_code)
  end subroutine c_whizard_finalize
  
  subroutine c_whizard_process_string (w_c_instance, c_cmds_in) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_cmds_in(*)
    type(string_t) :: f_cmds
  
    call c_whizard_convert_string (c_cmds_in, f_cmds)
    call c_whizard_commands (w_c_instance, f_cmds)
  end subroutine c_whizard_process_string

  subroutine c_whizard_model (w_c_instance, c_model) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_model(*)
    type(string_t) :: model, mdl_str
  
    call c_whizard_convert_string (c_model, model)
    mdl_str = "model = " // model
    call c_whizard_commands (w_c_instance, mdl_str)
  end subroutine c_whizard_model
  
  subroutine c_whizard_library (w_c_instance, c_library) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_library(*)
    type(string_t) :: library, lib_str
  
    call c_whizard_convert_string(c_library, library)
    lib_str = "library = " // library
    call c_whizard_commands (w_c_instance, lib_str)
  end subroutine c_whizard_library
  
  subroutine c_whizard_process (w_c_instance, c_id, c_in, c_out) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_id(*), c_in(*), c_out(*)
    type(string_t) :: proc_str, id, in, out
  
    call c_whizard_convert_string (c_id, id)
    call c_whizard_convert_string (c_in, in)
    call c_whizard_convert_string (c_out, out)
    proc_str = "process " // id // " = " // in // " => " // out
    call c_whizard_commands (w_c_instance, proc_str)
  end subroutine c_whizard_process
  
  subroutine c_whizard_compile (w_c_instance) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    type(c_ptr), intent(inout) :: w_c_instance
    type(string_t) :: cmp_str
    cmp_str = "compile"
    call c_whizard_commands (w_c_instance, cmp_str)
  end subroutine c_whizard_compile
    
  subroutine c_whizard_beams (w_c_instance, c_specs) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_specs(*)
    type(string_t) :: specs, beam_str
  
    call c_whizard_convert_string (c_specs, specs)
    beam_str = "beams = " // specs
    call c_whizard_commands (w_c_instance, beam_str)
  end subroutine c_whizard_beams
  
  subroutine c_whizard_integrate (w_c_instance, c_process) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_process(*)
    type(string_t) :: process, int_str
  
    call c_whizard_convert_string (c_process, process)
    int_str = "integrate (" // process //")"
    call c_whizard_commands (w_c_instance, int_str)
  end subroutine c_whizard_integrate
  
  subroutine c_whizard_matrix_element_test &
       (w_c_instance, c_process, n_calls) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none

    type(c_ptr), intent(inout) :: w_c_instance
    integer(kind=c_int) :: n_calls
    character(kind=c_char) :: c_process(*)
    type(string_t) :: process, me_str
    character(len=8) :: buffer
  
    call c_whizard_convert_string (c_process, process)
    write (buffer, "(I0)")  n_calls
    me_str = "integrate (" // process // ") { ?phs_only = true" // &
         "  n_calls_test = " // trim (buffer) 
    call c_whizard_commands (w_c_instance, me_str)
  end subroutine c_whizard_matrix_element_test
  
  subroutine c_whizard_simulate (w_c_instance, c_id) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none
  
    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_id(*)
    type(string_t) :: sim_str, id
  
    call c_whizard_convert_string(c_id, id)
    sim_str = "simulate (" // id // ")"
    call c_whizard_commands (w_c_instance, sim_str)
  end subroutine c_whizard_simulate
  
  subroutine c_whizard_sqrts (w_c_instance, c_value, c_unit) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
  
    implicit none

    type(c_ptr), intent(inout) :: w_c_instance
    character(kind=c_char) :: c_unit(*)
    integer(kind=c_int) :: c_value
    integer :: f_value
    character(len=8) :: f_val
    type(string_t) :: val, unit, sqrts_str
  
    f_value = c_value
    write (f_val,'(i8)') f_value
    val = f_val
    call c_whizard_convert_string (c_unit, unit)
    sqrts_str = "sqrts =" // val // unit
    call c_whizard_commands (w_c_instance, sqrts_str)
  end subroutine c_whizard_sqrts
  
  type(c_ptr) function c_whizard_hepmc_test &
       (w_c_instance, c_id, c_proc_id, c_event_id) bind(C)
    use, intrinsic :: iso_c_binding
    use iso_varying_string, string_t => varying_string !NODEP!
    use commands
    use diagnostics !NODEP!
    use events
    use hepmc_interface
    use lexers
    use models
    use parser
    use processes
    use rt_data
    use simulations
    use whizard
    use os_interface
   
    implicit none

    type(c_ptr), intent(inout) :: w_c_instance    
    type(string_t) :: sim_str
    type(parse_tree_t) :: parse_tree
    type(parse_node_t), pointer :: pn_root
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    type(command_list_t), pointer :: cmd_list
    type(whizard_t), pointer :: whizard_instance
   
    integer :: i_evt
    type(simulation_t), target :: sim
   
    character(kind=c_char), intent(in) :: c_id(*)
    type(string_t) :: id
    integer(kind=c_int), value :: c_proc_id, c_event_id
    integer :: proc_id, event_id
  
    type(hepmc_event_t), pointer :: hepmc_event
   
    type(process_t), pointer :: process
    integer :: proc
   
    integer :: factorization_mode, try
   
    call c_f_pointer (w_c_instance, whizard_instance)        
    
    call c_whizard_convert_string (c_id, id)
    sim_str = "simulate (" // id // ")" 
  
    proc_id = c_proc_id
    event_id = c_event_id
  
    allocate (hepmc_event)
    call hepmc_event_init (hepmc_event, c_proc_id, c_event_id)
  
    call syntax_cmd_list_init ()
    call lexer_init_cmd_list (lexer)
   
    call stream_init (stream, sim_str)
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    pn_root => parse_tree_get_root_ptr (parse_tree)
   
    allocate (cmd_list)
    if (associated (pn_root)) then
       call cmd_list%compile (pn_root, whizard_instance%global)
    end if   
  
    call sim%init ([id], .true., .true., whizard_instance%global)

    !!! This should generate a HepMC event as hepmc_event_t type
    call msg_message ("Not enabled for the moment.")
  
    call sim%final ()

    call cmd_list%final ()
   
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call lexer_final (lexer)
    call syntax_cmd_list_final ()
  
    c_whizard_hepmc_test = c_loc(hepmc_event)
    return
  end function c_whizard_hepmc_test

