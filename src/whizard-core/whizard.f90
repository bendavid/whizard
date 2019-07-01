! WHIZARD 2.2.5 Feb 27 2015
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

module whizard

  use io_units
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use system_defs, only: VERSION_STRING
  use system_defs, only: EOF, BACKSLASH
  use diagnostics
  use os_interface
  use formats
  use md5
  use sorting
  use cputime
  use sm_qcd
  use ifiles
  use lexers
  use parser
  use xml
  use colors
  use state_matrices
  use analysis
  use variables
  use model_testbed
  use user_code_interface
  use eval_trees
  use particles
  use models
  use auto_components
  use evaluators
  use phs_forests
  use beams
  use polarizations
  use sf_aux
  use sf_mappings
  use sf_base
  use sf_pdf_builtin
  use sf_lhapdf
  use sf_circe1
  use sf_circe2
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_escan
  use sf_beam_events
  use sf_user
  use phs_base
  use phs_single
  use phs_wood
  use rng_base
  use rng_tao
  use selectors
  use mci_base
  use mci_midpoint
  use mci_vamp
  use prclib_interfaces
  use particle_specifiers
  use process_libraries
  use prclib_stacks
  use hepmc_interface
  use lcio_interface
  use jets
  use pdg_arrays
  use interactions
  use slha_interface
  use cascades
  use blha_driver
  use blha_config
  use prc_core
  use prc_test
  use prc_template_me
  use prc_omega
  use subevt_expr
  use processes
  use process_stacks
  use event_transforms
  use decays
  use shower
  use events

  use hep_events
  use eio_data
  use eio_base
  use eio_raw
  use eio_checkpoints
  use eio_lhef
  use eio_hepmc
  use eio_lcio
  use eio_stdhep
  use eio_ascii
  use eio_weights

  use iterations
  use beam_structures
  use rt_data
  use dispatch
  use process_configurations
  use compilations
  use integrations
  use event_streams
  use simulations
  use nlo_data

  use expr_tests

  use commands

  implicit none
  private

  public :: whizard_options_t
  public :: whizard_t
  public :: init_syntax_tables
  public :: final_syntax_tables
  public :: write_syntax_tables
  public :: whizard_check

  type :: whizard_options_t
     type(string_t) :: preload_model
     type(string_t) :: default_lib
     type(string_t) :: preload_libraries
     logical :: rebuild_library = .false.
     logical :: recompile_library = .false.
     logical :: rebuild_user
     logical :: rebuild_phs = .false.
     logical :: rebuild_grids = .false.
     logical :: rebuild_events = .false.
  end type whizard_options_t
  
  type, extends (parse_tree_t) :: pt_entry_t
     type(pt_entry_t), pointer :: previous => null ()
  end type pt_entry_t
  
  type :: pt_stack_t
     type(pt_entry_t), pointer :: last => null ()
   contains
     procedure :: final => pt_stack_final
     procedure :: push => pt_stack_push
  end type pt_stack_t
  
  type :: whizard_t
     type(whizard_options_t) :: options
     type(rt_data_t) :: global
     type(pt_stack_t) :: pt_stack
   contains
     procedure :: init => whizard_init
     procedure :: final => whizard_final
     procedure :: init_rebuild_flags => whizard_init_rebuild_flags
     procedure :: preload_model => whizard_preload_model
     procedure :: preload_library => whizard_preload_library
     procedure :: process_ifile => whizard_process_ifile
     procedure :: process_stdin => whizard_process_stdin
     procedure :: process_file => whizard_process_file
     procedure :: process_stream => whizard_process_stream
     procedure :: shell => whizard_shell
  end type whizard_t
  

  save

contains

  subroutine pt_stack_final (pt_stack)
    class(pt_stack_t), intent(inout) :: pt_stack
    type(pt_entry_t), pointer :: current
    do while (associated (pt_stack%last))
       current => pt_stack%last
       pt_stack%last => current%previous
       call parse_tree_final (current%parse_tree_t)
       deallocate (current)
    end do
  end subroutine pt_stack_final

  subroutine pt_stack_push (pt_stack, parse_tree)
    class(pt_stack_t), intent(inout) :: pt_stack
    type(parse_tree_t), intent(out), pointer :: parse_tree
    type(pt_entry_t), pointer :: current
    allocate (current)
    parse_tree => current%parse_tree_t
    current%previous => pt_stack%last
    pt_stack%last => current
  end subroutine pt_stack_push
  
  subroutine whizard_init (whizard, options, paths, logfile)
    class(whizard_t), intent(out), target :: whizard
    type(whizard_options_t), intent(in) :: options    
    type(paths_t), intent(in), optional :: paths
    type(string_t), intent(in), optional :: logfile
    call init_syntax_tables ()
    whizard%options = options
    call whizard%global%global_init (paths, logfile)
    call whizard%init_rebuild_flags ()
    call whizard%preload_model ()
    call whizard%preload_library ()
    call whizard%global%init_fallback_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"))
    call whizard%global%init_radiation_model &
         (var_str ("SM_rad"), var_str ("SM_rad.mdl"))
  end subroutine whizard_init
  
  subroutine whizard_final (whizard)
    class(whizard_t), intent(inout), target :: whizard
    call whizard%global%final ()
    call whizard%pt_stack%final ()
!!! JRR: WK please check (#529)
    !    call user_code_final ()
    call final_syntax_tables () 
  end subroutine whizard_final
  
  subroutine whizard_init_rebuild_flags (whizard)
    class(whizard_t), intent(inout), target :: whizard
    associate (var_list => whizard%global%var_list, options => whizard%options) 
      call var_list_append_log &
           (var_list, var_str ("?rebuild_library"), options%rebuild_library, &
           intrinsic=.true.)
      call var_list_append_log &
           (var_list, var_str ("?recompile_library"), &
           options%recompile_library, &
           intrinsic=.true.)
      call var_list_append_log &
           (var_list, var_str ("?rebuild_phase_space"), options%rebuild_phs, &
           intrinsic=.true.)
      call var_list_append_log &
           (var_list, var_str ("?rebuild_grids"), options%rebuild_grids, &
           intrinsic=.true.)
      call var_list_append_log &
           (var_list, var_str ("?rebuild_events"), options%rebuild_events, &
           intrinsic=.true.)
    end associate
  end subroutine whizard_init_rebuild_flags

  subroutine whizard_preload_model (whizard)
    class(whizard_t), intent(inout), target :: whizard
    type(string_t) :: model_name
    model_name = whizard%options%preload_model
    if (model_name /= "") then
       call whizard%global%read_model (model_name, whizard%global%preload_model)
       whizard%global%model => whizard%global%preload_model
       if (associated (whizard%global%model)) then
          call whizard%global%model%link_var_list (whizard%global%var_list)
          call msg_message ("Preloaded model: " &
               // char (model_name))
       else
          call msg_fatal ("Preloading model " // char (model_name) &
               // " failed")
       end if
    else
       call msg_message ("No model preloaded")
    end if
  end subroutine whizard_preload_model
    
  subroutine whizard_preload_library (whizard)
    class(whizard_t), intent(inout), target :: whizard
    type(string_t) :: library_name, libs
    type(string_t), dimension(:), allocatable :: libname_static
    type(prclib_entry_t), pointer :: lib_entry
    integer :: i
    call get_prclib_static (libname_static)
    do i = 1, size (libname_static)
       allocate (lib_entry)
       call lib_entry%init_static (libname_static(i))
       call whizard%global%add_prclib (lib_entry)
    end do
    libs = adjustl (whizard%options%preload_libraries)
    if (libs == "" .and. whizard%options%default_lib /= "") then
          allocate (lib_entry)
          call lib_entry%init (whizard%options%default_lib)
          call whizard%global%add_prclib (lib_entry)
          call msg_message ("Preloaded library: " // &
               char (whizard%options%default_lib))    
       end if    
    SCAN_LIBS: do while (libs /= "")
       call split (libs, library_name, " ")      
       if (library_name /= "") then
          allocate (lib_entry)
          call lib_entry%init (library_name)
          call whizard%global%add_prclib (lib_entry)
          call msg_message ("Preloaded library: " // char (library_name))
       end if
    end do SCAN_LIBS    
  end subroutine whizard_preload_library
    
  subroutine init_syntax_tables ()
    call syntax_model_file_init ()
    call syntax_phs_forest_init ()
    call syntax_pexpr_init ()
    call syntax_slha_init ()
    call syntax_blha_contract_init ()    
    call syntax_cmd_list_init ()
  end subroutine init_syntax_tables

  subroutine final_syntax_tables ()
    call syntax_model_file_final ()
    call syntax_phs_forest_final ()
    call syntax_pexpr_final ()
    call syntax_slha_final ()
    call syntax_blha_contract_final ()    
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

  subroutine whizard_process_ifile (whizard, ifile, quit, quit_code)
    class(whizard_t), intent(inout), target :: whizard
    type(ifile_t), intent(in) :: ifile
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    call msg_message ("Reading commands given on the command line")
    call lexer_init_cmd_list (lexer)
    call stream_init (stream, ifile)
    call whizard%process_stream (stream, lexer, quit, quit_code)
    call stream_final (stream)
    call lexer_final (lexer)
  end subroutine whizard_process_ifile

  subroutine whizard_process_stdin (whizard, quit, quit_code)
    class(whizard_t), intent(inout), target :: whizard
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    call msg_message ("Reading commands from standard input")
    call lexer_init_cmd_list (lexer)
    call stream_init (stream, 5)
    call whizard%process_stream (stream, lexer, quit, quit_code)
    call stream_final (stream)
    call lexer_final (lexer)
  end subroutine whizard_process_stdin

  subroutine whizard_process_file (whizard, file, quit, quit_code)
    class(whizard_t), intent(inout), target :: whizard
    type(string_t), intent(in) :: file
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(lexer_t), target :: lexer
    type(stream_t), target :: stream
    logical :: exist
    call msg_message ("Reading commands from file '" // char (file) // "'")
    inquire (file=char(file), exist=exist)
    if (exist) then
       call lexer_init_cmd_list (lexer)
       call stream_init (stream, char (file))
       call whizard%process_stream (stream, lexer, quit, quit_code)
       call stream_final (stream)
       call lexer_final (lexer)
    else
       call msg_error ("File '" // char (file) // "' not found")
    end if
  end subroutine whizard_process_file

  subroutine whizard_process_stream (whizard, stream, lexer, quit, quit_code)
    class(whizard_t), intent(inout), target :: whizard
    type(stream_t), intent(inout), target :: stream
    type(lexer_t), intent(inout), target :: lexer
    logical, intent(out) :: quit
    integer, intent(out) :: quit_code
    type(parse_tree_t), pointer :: parse_tree
    type(command_list_t), target :: command_list
    call lexer_assign_stream (lexer, stream)
    call whizard%pt_stack%push (parse_tree)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    if (associated (parse_tree_get_root_ptr (parse_tree))) then
       whizard%global%lexer => lexer
       call command_list%compile (parse_tree_get_root_ptr (parse_tree), &
            whizard%global)
    end if
    call whizard%global%activate ()
    call command_list%execute (whizard%global)
    call command_list%final ()
    quit = whizard%global%quit
    quit_code = whizard%global%quit_code
  end subroutine whizard_process_stream

  subroutine whizard_shell (whizard, quit_code)
    class(whizard_t), intent(inout), target :: whizard
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
       call whizard%process_stream (stream, lexer, quit, quit_code)
       msg_count = 0
       mask_fatal_errors = mask_tmp
       call stream_final (stream)
       if (quit)  exit COMMAND_LOOP
    end do COMMAND_LOOP
    print *
    call lexer_final (lexer)
  end subroutine whizard_shell

  subroutine prepare_eio_test (event, unweighted, n_alt)
    use variables
    use model_data
    use event_base
    class(generic_event_t), intent(inout), pointer :: event
    logical, intent(in), optional :: unweighted
    integer, intent(in), optional :: n_alt
    type(model_data_t), pointer :: model
    type(var_list_t) :: var_list
    type(process_t), pointer :: process
    type(process_instance_t), pointer :: process_instance

    allocate (model)
    call model%init_test ()

    allocate (process)
    allocate (process_instance)

    call prepare_test_process (process, process_instance, model)
    call process_instance%setup_event_data ()
 
    call model%final ()
    deallocate (model)

    allocate (event_t :: event)
    select type (event)
    type is (event_t)
       if (present (unweighted)) then
          call var_list_append_log (var_list, &
               var_str ("?unweighted"), unweighted, &
               intrinsic = .true.)
       else
          call var_list_append_log (var_list, &
               var_str ("?unweighted"), .true., &
               intrinsic = .true.)
       end if
       call var_list_append_string (var_list, &
            var_str ("$sample_normalization"), &
            var_str ("auto"), intrinsic = .true.)
       call event%basic_init (var_list, n_alt)
       call event%connect (process_instance, process%get_model_ptr ())
       call var_list%final ()
    end select
  end subroutine prepare_eio_test
    
  subroutine cleanup_eio_test (event)
    use model_data
    use event_base
    class(generic_event_t), intent(inout), pointer :: event
    type(process_t), pointer :: process
    type(process_instance_t), pointer :: process_instance
    select type (event)
    type is (event_t)
       process => event%get_process_ptr ()
       process_instance => event%get_process_instance_ptr ()
       call cleanup_test_process (process, process_instance)
       deallocate (process_instance)
       deallocate (process)
       call event%final ()
    end select
    deallocate (event)
  end subroutine cleanup_eio_test
    
  subroutine prepare_whizard_model (model, name, vars)
    use iso_varying_string, string_t => varying_string
    use os_interface
    use model_data
    use var_base
    use models
    class(model_data_t), intent(inout), pointer :: model
    type(string_t), intent(in) :: name
    class(vars_t), pointer, intent(out), optional :: vars
    type(os_data_t) :: os_data
    call syntax_model_file_init ()
    call os_data_init (os_data)
    allocate (model_t :: model)
    select type (model)
    type is (model_t)
       call model%read (name // ".mdl", os_data)
       if (present (vars)) then
          vars => model%get_var_list_ptr ()
       end if
    end select
  end subroutine prepare_whizard_model
    
  subroutine cleanup_whizard_model (model)
    use model_data
    use models
    class(model_data_t), intent(inout), pointer :: model
    call model%final ()
    deallocate (model)
    call syntax_model_file_final ()
  end subroutine cleanup_whizard_model
    
  subroutine prepare_fallback_model (model)
    use model_data
    class(model_data_t), intent(inout), pointer :: model
    call prepare_whizard_model (model, var_str ("SM_hadrons"))
  end subroutine prepare_fallback_model
    
  subroutine whizard_check (check, results)
    type(string_t), intent(in) :: check
    type(test_results_t), intent(inout) :: results
    type(os_data_t) :: os_data
    integer :: u
    call os_data_init (os_data)
    u = free_unit ()
    open (u, file="whizard_check." // char (check) // ".log", &
         action="write", status="replace")
    call msg_message (repeat ('=', 76), 0)
    call msg_message ("Running self-test: " // char (check), 0)
    call msg_message (repeat ('-', 76), 0)
    eio_prepare_test => prepare_eio_test
    eio_cleanup_test => cleanup_eio_test
    prepare_model => prepare_whizard_model
    cleanup_model => cleanup_whizard_model
    eio_prepare_fallback_model => prepare_fallback_model
    eio_cleanup_fallback_model => cleanup_model
    select case (char (check))
    case ("analysis")
       call analysis_test (u, results)
    case ("beams")
       call beam_test (u, results)
    case ("cascades")
       call cascade_test (u, results)
    case ("colors")
       call color_test (u, results)
    case ("evaluators")
      call evaluator_test (u, results)
    case ("expressions")
       call expressions_test (u, results)
    case ("formats")
       call format_test (u, results)
    case ("hepmc")
       call hepmc_test (u, results)
    case ("lcio")
       call lcio_test (u, results)
    case ("jets")
       call jets_test (u, results)
    case ("pdg_arrays")
       call pdg_arrays_test (u, results)
    case ("interactions")
       call interaction_test (u, results)
    case ("lexers")
       call lexer_test (u, results)
    case ("os_interface") 
       call os_interface_test (u, results)
    case ("cputime") 
       call cputime_test (u, results)
    case ("parser")
       call parse_test (u, results)       
    case ("sorting")
       call sorting_test (u, results)
    case ("md5")       
       call md5_test (u, results)       
    case ("xml")
       call xml_test (u, results)
    case ("sm_qcd")
       call sm_qcd_test (u, results)
    case ("models")
       call models_test (u, results)
    case ("auto_components")
       call auto_components_test (u, results)
    case ("particles")
       call particles_test (u, results)
    case ("polarizations")
       call polarization_test (u, results)
    case ("sf_aux")
       call sf_aux_test (u, results)
    case ("sf_mappings")
       call sf_mappings_test (u, results)
    case ("sf_base")
       call sf_base_test (u, results)
    case ("sf_pdf_builtin")
       call sf_pdf_builtin_test (u, results)
    case ("sf_lhapdf")
       call sf_lhapdf_test (u, results)
    case ("sf_isr")
       call sf_isr_test (u, results)
    case ("sf_epa")
       call sf_epa_test (u, results)
    case ("sf_ewa")
       call sf_ewa_test (u, results)
    case ("sf_circe1")
       call sf_circe1_test (u, results)
    case ("sf_circe2")
       call sf_circe2_test (u, results)
    case ("sf_beam_events")
       call sf_beam_events_test (u, results)
    case ("sf_escan")
       call sf_escan_test (u, results)
    case ("phs_base")
       call phs_base_test (u, results)
    case ("phs_single")
       call phs_single_test (u, results)
    case ("phs_forests")
       call phs_forest_test (u, results)       
    case ("phs_wood")
       call phs_wood_test (u, results)
    case ("phs_wood_vis")
       call phs_wood_vis_test (u, results)
    case ("mci_base")
       call mci_base_test (u, results)
    case ("rng_base")
       call rng_base_test (u, results)
    case ("rng_tao")
       call rng_tao_test (u, results)
    case ("selectors")
       call selectors_test (u, results)
    case ("mci_midpoint")
       call mci_midpoint_test (u, results)
    case ("mci_vamp")
       call mci_vamp_test (u, results)
    case ("prclib_interfaces")
       call prclib_interfaces_test (u, results)
    case ("particle_specifiers")
       call particle_specifiers_test (u, results)
    case ("process_libraries")
       call process_libraries_test (u, results)
    case ("prclib_stacks")
       call prclib_stacks_test (u, results)
    case ("slha_interface")
       call slha_test (u, results)
    case ("state_matrices")
       call state_matrix_test (u, results)
    case ("prc_test")
       call prc_test_test (u, results)
    case ("subevt_expr")
       call subevt_expr_test (u, results)
    case ("processes")
       call processes_test (u, results)       
    case ("process_stacks")
       call process_stacks_test (u, results)   
    case ("event_transforms")
       call event_transforms_test (u, results)       
    case ("decays")
       call decays_test (u, results)       
    case ("shower")
       call shower_test (u, results)       
    case ("events")
       call events_test (u, results)
    case ("prc_template_me")
       call prc_template_me_test (u, results)              
    case ("prc_omega")
       call prc_omega_test (u, results)
    case ("prc_omega_diags")
       call prc_omega_diags_test (u, results)       
    case ("hep_events")
       call hep_events_test (u, results)       
    case ("eio_data")       
       call eio_data_test (u, results)
    case ("eio_base")
       call eio_base_test (u, results)
    case ("eio_raw")
       call eio_raw_test (u, results)
    case ("eio_checkpoints")
       call eio_checkpoints_test (u, results)
    case ("eio_lhef")
       call eio_lhef_test (u, results)
    case ("eio_hepmc")
       call eio_hepmc_test (u, results)
    case ("eio_lcio")
       call eio_lcio_test (u, results)
    case ("eio_stdhep")
       call eio_stdhep_test (u, results)
    case ("eio_ascii")
       call eio_ascii_test (u, results)       
    case ("eio_weights")
       call eio_weights_test (u, results)
    case ("iterations")
       call iterations_test (u, results)
    case ("beam_structures")
       call beam_structures_test (u, results)
    case ("rt_data")
       call rt_data_test (u, results)
    case ("dispatch")
       call dispatch_test (u, results)
    case ("process_configurations")
       call process_configurations_test (u, results)
    case ("compilations")
       call compilations_test (u, results)
    case ("compilations_static")
       call compilations_static_test (u, results)
    case ("integrations")
       call integrations_test (u, results)
    case ("integrations_history")
       call integrations_history_test (u, results)
    case ("event_streams")
       call event_streams_test (u, results)
    case ("simulations")
       call simulations_test (u, results)
    case ("commands")
       call commands_test (u, results)
    case ("all")
       call analysis_test (u, results)
       call beam_test (u, results)
       call md5_test (u, results)
       call lexer_test (u, results)
       call sorting_test (u, results)
       call parse_test (u, results)
       call color_test (u, results)
       call evaluator_test (u, results)
       call expressions_test (u, results)
       call format_test (u, results)
       call hepmc_test (u, results)
       call lcio_test (u, results)
       call jets_test (u, results)
       call os_interface_test (u, results)
       call cputime_test (u, results)
       call interaction_test (u, results)
       call xml_test (u, results)
       call sm_qcd_test (u, results)
       call models_test (u, results)
       call auto_components_test (u, results)
       call particles_test (u, results)
       call polarization_test (u, results)
       call sf_aux_test (u, results)
       call sf_mappings_test (u, results)
       call sf_base_test (u, results)
       call sf_pdf_builtin_test (u, results)
       call sf_lhapdf_test (u, results)
       call sf_isr_test (u, results)
       call sf_epa_test (u, results)
       call sf_ewa_test (u, results)
       call sf_circe1_test (u, results)
       call sf_circe2_test (u, results)
       call sf_beam_events_test (u, results)
       call sf_escan_test (u, results)
       call phs_base_test (u, results)
       call phs_single_test (u, results)
       call phs_forest_test (u, results)
       call phs_wood_test (u, results)
       call phs_wood_vis_test (u, results)
       call rng_base_test (u, results)
       call cascade_test (u, results)
       call rng_tao_test (u, results)
       call selectors_test (u, results)
       call mci_base_test (u, results)
       call mci_midpoint_test (u, results)
       call mci_vamp_test (u, results)
       call prclib_interfaces_test (u, results)
       call particle_specifiers_test (u, results)
       call process_libraries_test (u, results)
       call prclib_stacks_test (u, results)
       call slha_test (u, results)
       call state_matrix_test (u, results)
       call prc_test_test (u, results)
       call subevt_expr_test (u, results)
       call processes_test (u, results)
       call process_stacks_test (u, results)   
       call event_transforms_test (u, results)
       call decays_test (u, results)
       call shower_test (u, results)
       call events_test (u, results)
       call prc_omega_test (u, results)
       call prc_omega_diags_test (u, results)
       call prc_template_me_test (u, results)
       call hep_events_test (u, results)
       call eio_data_test (u, results)
       call eio_base_test (u, results)
       call eio_raw_test (u, results)
       call eio_checkpoints_test (u, results)
       call eio_lhef_test (u, results)
       call eio_hepmc_test (u, results)
       call eio_lcio_test (u, results)
       call eio_stdhep_test (u, results)
       call eio_ascii_test (u, results)
       call eio_weights_test (u, results)
       call iterations_test (u, results)
       call beam_structures_test (u, results)
       call rt_data_test (u, results)
       call dispatch_test (u, results)
       call process_configurations_test (u, results)
       call compilations_test (u, results)
       call compilations_static_test (u, results)
       call integrations_test (u, results)
       call integrations_history_test (u, results)       
       call event_streams_test (u, results)
       call simulations_test (u, results)
       call commands_test (u, results)
    case default
       call msg_fatal ("Self-test '" // char (check) // "' not implemented.")
    end select
    close (u)
  end subroutine whizard_check


end module whizard
