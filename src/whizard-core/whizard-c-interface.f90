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

subroutine c_whizard_convert_string(c_string, f_string)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char), intent(in) :: c_string(*)
  type(string_t), intent(inout) :: f_string
  character(len=1) :: dummy_char
  integer :: dummy_i = 1

  f_string = ""
  do
     if(c_string(dummy_i) == c_null_char) then
        exit
     else if(c_string(dummy_i) == c_new_line) then
        dummy_char = CHAR(13)
        f_string = f_string // dummy_char
        dummy_char = CHAR(10)
     else
        dummy_char = c_string(dummy_i)
     end if
     f_string = f_string // dummy_char
     dummy_i = dummy_i + 1
  end do
  dummy_i = 1
end subroutine c_whizard_convert_string

subroutine c_whizard_commands(cmds)
  use iso_varying_string, string_t => varying_string !NODEP!
  use commands
  use diagnostics !NODEP!
  use lexers
  use models
  use parser
  use whizard

  type(string_t) :: cmds
  type(parse_tree_t) :: parse_tree
  type(parse_node_t), pointer :: pn_root
  type(stream_t), target :: stream
  type(lexer_t) :: lexer
  type(command_list_t), pointer :: cmd_list

  call lexer_init_cmd_list (lexer)

  call stream_init (stream, cmds)
  call lexer_assign_stream (lexer, stream)
  call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
  pn_root => parse_tree_get_root_ptr (parse_tree)

  allocate(cmd_list)
  call command_list_compile(cmd_list, pn_root ,global)
  call command_list_execute(cmd_list, global)
  call command_list_final(cmd_list)

  call parse_tree_final (parse_tree)
  call stream_final (stream)
  call lexer_final (lexer)
end subroutine c_whizard_commands
subroutine c_whizard_init() bind(c)
  use, intrinsic :: iso_c_binding
  use diagnostics !NODEP!
  use ifiles
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: CMDLINE_ARG_LEN !NODEP!
  use os_interface
  use system_dependencies !NODEP!
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
  logical :: user_code_enable = .false.
  integer :: n_user_src = 0, n_user_lib = 0
  type(string_t) :: user_src, user_lib
  type(paths_t) :: paths
  logical :: rebuild_library, rebuild_user
  logical :: rebuild_phs, rebuild_grids, rebuild_events
  logical :: recompile_library
  logical :: time_estimate
  type(ifile_t) :: commands
  type(string_t) :: command

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
  user_src = ""
  user_lib = ""
  rebuild_library = .false.
  rebuild_user = .false.
  rebuild_phs = .false.
  rebuild_grids = .false.
  rebuild_events = .false.
  recompile_library = .false.
  time_estimate = .true.
  call paths_init (paths)

  ! Overall initialization
  if (logfile /= "")  call logfile_init (logfile)
  call mask_term_signals ()
  call msg_banner ()
  call whizard_init &
       (preload_model=model, preload_libs=libraries, default_lib=libname, &
        rebuild_library=rebuild_library, &
        rebuild_user=rebuild_user, &
        rebuild_phs=rebuild_phs, &
        rebuild_grids=rebuild_grids, &
        rebuild_events=rebuild_events, &
        recompile_library=recompile_library, &
        time_estimate=time_estimate, &
        paths=paths, &
        user_code_enable=user_code_enable, &
        n_user_src=n_user_src, user_src=user_src, &
        n_user_lib=n_user_lib, user_lib=user_lib)

   ! Run any self-checks (and no commands)
   if (checks /= "") then
      checks = trim (adjustl (checks))
      RUN_CHECKS: do while (checks /= "")
         call split (checks, check, " ")
         call whizard_check (check, LHAPDF_AVAILABLE)
      end do RUN_CHECKS
      quit = .true.
   end if
end subroutine c_whizard_init

subroutine c_whizard_finalize() bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!
  use system_dependencies !NODEP!
  use limits, only: CMDLINE_ARG_LEN !NODEP!
  use diagnostics !NODEP!
  use ifiles
  use os_interface
  use whizard

  ! Exit status
  logical :: quit = .false.
  integer :: quit_code = 0

  ! Overall finalization
  ! call ifile_final (commands)
  call whizard_final ()
  call terminate_now_if_signal ()
  call release_term_signals ()
  call msg_terminate (quit_code = quit_code)
end subroutine c_whizard_finalize

subroutine c_whizard_process_string(c_cmds_in) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_cmds_in(*)
  type(string_t) :: f_cmds

  call c_whizard_convert_string(c_cmds_in, f_cmds)
  call c_whizard_commands(f_cmds)
end subroutine c_whizard_process_string
subroutine c_whizard_model(c_model) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_model(*)
  type(string_t) :: model, mdl_str

  call c_whizard_convert_string(c_model, model)
  mdl_str = "model = " // model
  call c_whizard_commands(mdl_str)
end subroutine c_whizard_model

subroutine c_whizard_library(c_library) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_library(*)
  type(string_t) :: library, lib_str

  call c_whizard_convert_string(c_library, library)
  lib_str = "library = " // library
  call c_whizard_commands(lib_str)
end subroutine c_whizard_library

subroutine c_whizard_process(c_id, c_in, c_out) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_id(*), c_in(*), c_out(*)
  type(string_t) :: proc_str, id, in, out

  call c_whizard_convert_string(c_id, id)
  call c_whizard_convert_string(c_in, in)
  call c_whizard_convert_string(c_out, out)
  proc_str = "process " // id // " = " // in // " => " // out
  call c_whizard_commands(proc_str)
end subroutine c_whizard_process

subroutine c_whizard_compile() bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  type(string_t) :: cmp_str
  cmp_str = "compile"
  call c_whizard_commands(cmp_str)
end subroutine c_whizard_compile

subroutine c_whizard_load(c_library) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_library(*)
  type(string_t) :: library, load_str

  call c_whizard_convert_string(c_library, library)
  load_str = "load = " // library
  call c_whizard_commands(load_str)
end subroutine c_whizard_load

subroutine c_whizard_beams(c_specs) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_specs(*)
  type(string_t) :: specs, beam_str

  call c_whizard_convert_string(c_specs, specs)
  beam_str = "beams = " // specs
  call c_whizard_commands(beam_str)
end subroutine c_whizard_beams

subroutine c_whizard_integrate(c_process) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_process(*)
  type(string_t) :: process, int_str

  call c_whizard_convert_string(c_process, process)
  int_str = "integrate (" // process //")"
  call c_whizard_commands(int_str)
end subroutine c_whizard_integrate

subroutine c_whizard_matrix_element_test(c_process) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_process(*)
  type(string_t) :: process, me_str

  call c_whizard_convert_string(c_process, process)
  me_str = "matrix_element_test (" // process // ")"
  call c_whizard_commands(me_str)
end subroutine c_whizard_matrix_element_test

subroutine c_whizard_simulate(c_id) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_id(*)
  type(string_t) :: sim_str, id

  call c_whizard_convert_string(c_id, id)
  sim_str = "simulate (" // id // ")"
  call c_whizard_commands(sim_str)
end subroutine c_whizard_simulate

subroutine c_whizard_sqrts(c_value, c_unit) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!

  implicit none

  character(kind=c_char) :: c_unit(*)
  integer(kind=c_int) :: c_value
  integer :: f_value
  character(len=8) :: f_val
  type(string_t) :: val, unit, sqrts_str

  f_value = c_value
  write(f_val,'(i8)') f_value
  val = f_val
  call c_whizard_convert_string(c_unit, unit)
  sqrts_str = "sqrts =" // val // unit
  call c_whizard_commands(sqrts_str)
end subroutine c_whizard_sqrts
type(c_ptr) function c_whizard_hepmc_test(c_id, c_proc_id, c_event_id) bind(c)
  use, intrinsic :: iso_c_binding
  use iso_varying_string, string_t => varying_string !NODEP!
  use commands
  use diagnostics !NODEP!
  use events
  use hepmc_interface
  use lexers
  use limits, only: MAX_TRIES_FOR_SINGLE_EVENT !NODEP!
  use models
  use parser
  use processes
  use rt_data
  use simulations
  use whizard
  use os_interface
 
  implicit none

  type(string_t) :: sim_str
  type(parse_tree_t) :: parse_tree
  type(parse_node_t), pointer :: pn_root
  type(stream_t), target :: stream
  type(lexer_t) :: lexer
  type(command_list_t), pointer :: cmd_list
  type(command_t), pointer :: command
  type(os_data_t) :: os_data
 
  type(cmd_simulate_t), target :: simulate
  logical :: ok !, mlm_matching
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
 
  call c_whizard_convert_string(c_id, id)
  sim_str = "simulate (" // id // ")" 

  proc_id = c_proc_id
  event_id = c_event_id

  allocate(hepmc_event)
  call hepmc_event_init (hepmc_event, c_proc_id, c_event_id)

  call lexer_init_cmd_list (lexer)
 
  call stream_init (stream, sim_str)
  call lexer_assign_stream (lexer, stream)
  call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
  pn_root => parse_tree_get_root_ptr (parse_tree)
 
  allocate(cmd_list)
  call command_list_compile(cmd_list, pn_root ,global)
 
  command => cmd_list%first
  simulate = command%simulate
 
  call rt_data_link (simulate%local, global)

  if (associated (simulate%options)) then
     call command_list_execute (simulate%options, simulate%local)
  end if

  call simulation_init (sim, simulate%process_id, simulate%local, global%var_list, ok, verbose=.false.)

  if (ok) then
     call simulation_setup_reweight &
          (sim, simulate%local%pn_reweight_expr, verbose=.false.)
     call simulation_select_process (sim, simulate%local%rng, process, proc)
     
     if (sim%allow_decays) then
        call event_init (sim%event, process, &
             sim%event_vars, sim%decay_tree(proc))
     else
        call event_init (sim%event, process, sim%event_vars)
     end if
     
     if (sim%use_num_id) then
        sim%event_vars%process_num_id = sim%num_id(proc)
     else
        sim%event_vars%process_num_id = proc
     end if
     
     if (sim%spar%polarized) then 
        factorization_mode = FM_SELECT_HELICITY 
     else 
        factorization_mode = FM_IGNORE_HELICITY 
     end if
     
     call os_data_init(os_data)
     GENERATE: do try = 1, MAX_TRIES_FOR_SINGLE_EVENT
        call event_generate &
             (sim%event, simulate%local%rng, sim%spar%unweighted, &
             factorization_mode, &
             keep_correlations=.false., &
             keep_virtual=.true., os_data=os_data, &
             shower_settings = sim%spar%shower_settings)
       if(event_is_vetoed(sim%event).and. &
            (.not.sim%n_events_set)) then
          sim%n_events = sim%n_events - 1
          if(sim%i_evt .ge. sim%n_events) then
             call event_final(sim%event)
             return
          end if
       end if
       if (event_is_valid (sim%event).and. &
            (.not.event_is_vetoed(sim%event)))  exit GENERATE
     end do GENERATE
     if (.not. event_is_valid (sim%event)) then
        write (msg_buffer, "(A,I0,A)") "Failed to generate a valid event " &
             // "after ", MAX_TRIES_FOR_SINGLE_EVENT, " tries"
        call msg_fatal ()
     end if
     
     call event_renormalize_weight (sim%event, sim%norm_weight)

     call event_write_to_hepmc(sim%event, hepmc_event)

     call simulation_handle_event (sim)
     call simulation_final_event (sim)
     call simulation_final (sim, verbose=.false.)
  end if
  call rt_data_restore (global, simulate%local)
  
  call command_list_final(cmd_list)
 
  call parse_tree_final (parse_tree)
  call stream_final (stream)
  call lexer_final (lexer)

  c_whizard_hepmc_test = c_loc(hepmc_event)
  return
end function c_whizard_hepmc_test

