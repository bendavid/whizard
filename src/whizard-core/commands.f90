! WHIZARD 2.3.1 Aug 25 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
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

module commands

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use string_utils, only: lower_case
  use format_utils, only: write_indent
  use format_defs, only: FMT_14, FMT_19
  use diagnostics

  use physics_defs
  use sorting
  use sf_lhapdf, only: lhapdf_global_reset
  use os_interface
  use ifiles
  use lexers
  use syntax_rules
  use parser
  use analysis
  use pdg_arrays
  use variables
  use observables, only: var_list_check_observable
  use observables, only: var_list_check_result_var
  use eval_trees
  use models
  use auto_components
  use flavors
  use polarizations
  use particle_specifiers
  use process_libraries
  use processes
  use prclib_stacks
  use slha_interface
  use user_files
  use eio_data
  use rt_data

  use dispatch, only: dispatch_slha

  use process_configurations
  use compilations, only: compile_library, compile_executable
  use integrations, only: integrate_process
  use event_streams
  use simulations

  use radiation_generator

  implicit none
  private

  public :: get_prclib_static
  public :: command_list_t
  public :: syntax_cmd_list
  public :: syntax_cmd_list_init
  public :: syntax_cmd_list_final
  public :: syntax_cmd_list_write
  public :: lexer_init_cmd_list

  type, abstract :: command_t
     type(parse_node_t), pointer :: pn => null ()
     class(command_t), pointer :: next => null ()
     type(parse_node_t), pointer :: pn_opt => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t), pointer :: local => null ()
   contains
     procedure :: final => command_final
     procedure (command_write), deferred :: write
     procedure (command_compile), deferred :: compile
     procedure (command_execute), deferred :: execute
     procedure :: write_options => command_write_options
     procedure :: compile_options => command_compile_options
     procedure :: execute_options => cmd_execute_options
     procedure :: reset_options => cmd_reset_options
  end type command_t

  type, extends (command_t) :: cmd_model_t
     private
     type(string_t) :: name
     type(string_t) :: scheme
   contains
     procedure :: write => cmd_model_write
     procedure :: compile => cmd_model_compile
     procedure :: execute => cmd_model_execute
  end type cmd_model_t

  type, extends (command_t) :: cmd_library_t
     private
     type(string_t) :: name
   contains
     procedure :: write => cmd_library_write
     procedure :: compile => cmd_library_compile
     procedure :: execute => cmd_library_execute
  end type cmd_library_t

  type, extends (command_t) :: cmd_process_t
     private
     type(string_t) :: id
     integer :: n_in  = 0
     type(parse_node_p), dimension(:), allocatable :: pn_pdg_in
     type(parse_node_t), pointer :: pn_out => null ()
   contains
     procedure :: write => cmd_process_write
     procedure :: compile => cmd_process_compile
     procedure :: execute => cmd_process_execute  
  end type cmd_process_t

  type, extends (command_t) :: cmd_nlo_t
    private
    type(parse_node_p), dimension(3) :: pn_components
    logical, dimension(6) :: active_component = .false.
  contains
      procedure :: write => cmd_nlo_write
      procedure :: compile => cmd_nlo_compile
      procedure :: execute => cmd_nlo_execute
  end type cmd_nlo_t

  type, extends (command_t) :: cmd_compile_t
     private
     type(string_t), dimension(:), allocatable :: libname
     logical :: make_executable = .false.
     type(string_t) :: exec_name
   contains
     procedure :: write => cmd_compile_write
     procedure :: compile => cmd_compile_compile
     procedure :: execute => cmd_compile_execute
  end type cmd_compile_t

  type, extends (command_t) :: cmd_exec_t
     private
     type(parse_node_t), pointer :: pn_command => null ()
   contains
     procedure :: write => cmd_exec_write
     procedure :: compile => cmd_exec_compile
     procedure :: execute => cmd_exec_execute  
  end type cmd_exec_t

  type, extends (command_t) :: cmd_var_t
     private
     type(string_t) :: name
     integer :: type = V_NONE
     type(parse_node_t), pointer :: pn_value => null ()
     logical :: is_intrinsic = .false.
     logical :: is_model_var = .false.
   contains
     procedure :: write => cmd_var_write
     procedure :: compile => cmd_var_compile
     procedure :: execute => cmd_var_execute
     procedure :: set_value => cmd_var_set_value
  end type cmd_var_t

  type, extends (command_t) :: cmd_slha_t
     private
     type(string_t) :: file
     logical :: write_mode = .false.
   contains
     procedure :: write => cmd_slha_write
     procedure :: compile => cmd_slha_compile
     procedure :: execute => cmd_slha_execute  
  end type cmd_slha_t

  type, extends (command_t) :: cmd_show_t
     private
     type(string_t), dimension(:), allocatable :: name
   contains
     procedure :: write => cmd_show_write
     procedure :: compile => cmd_show_compile
     procedure :: execute => cmd_show_execute
  end type cmd_show_t

  type, extends (command_t) :: cmd_clear_t
     private
     type(string_t), dimension(:), allocatable :: name
   contains
     procedure :: write => cmd_clear_write
     procedure :: compile => cmd_clear_compile
     procedure :: execute => cmd_clear_execute
  end type cmd_clear_t
     
  type, extends (command_t) :: cmd_expect_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
   contains
     procedure :: write => cmd_expect_write
     procedure :: compile => cmd_expect_compile
     procedure :: execute => cmd_expect_execute  
  end type cmd_expect_t

  type, extends (command_t) :: cmd_beams_t
     private
     integer :: n_in = 0
     type(parse_node_p), dimension(:), allocatable :: pn_pdg
     integer :: n_sf_record = 0
     integer, dimension(:), allocatable :: n_entry
     type(parse_node_p), dimension(:,:), allocatable :: pn_sf_entry
   contains
     procedure :: write => cmd_beams_write
     procedure :: compile => cmd_beams_compile
     procedure :: execute => cmd_beams_execute
  end type cmd_beams_t

  type :: sentry_expr_t
     type(parse_node_p), dimension(:), allocatable :: expr
   contains
     procedure :: compile => sentry_expr_compile
     procedure :: evaluate => sentry_expr_evaluate
  end type sentry_expr_t

  type :: smatrix_expr_t
     type(sentry_expr_t), dimension(:), allocatable :: entry
   contains
     procedure :: compile => smatrix_expr_compile
     procedure :: evaluate => smatrix_expr_evaluate
  end type smatrix_expr_t
  
  type, extends (command_t) :: cmd_beams_pol_density_t
     private
     integer :: n_in = 0
     type(smatrix_expr_t), dimension(:), allocatable :: smatrix
   contains
     procedure :: write => cmd_beams_pol_density_write
     procedure :: compile => cmd_beams_pol_density_compile
     procedure :: execute => cmd_beams_pol_density_execute
  end type cmd_beams_pol_density_t

  type, extends (command_t) :: cmd_beams_pol_fraction_t
     private
     integer :: n_in = 0
     type(parse_node_p), dimension(:), allocatable :: expr
   contains
     procedure :: write => cmd_beams_pol_fraction_write
     procedure :: compile => cmd_beams_pol_fraction_compile
     procedure :: execute => cmd_beams_pol_fraction_execute
  end type cmd_beams_pol_fraction_t

  type, extends (cmd_beams_pol_fraction_t) :: cmd_beams_momentum_t
   contains
     procedure :: write => cmd_beams_momentum_write
     procedure :: execute => cmd_beams_momentum_execute
  end type cmd_beams_momentum_t

  type, extends (cmd_beams_pol_fraction_t) :: cmd_beams_theta_t
   contains
     procedure :: write => cmd_beams_theta_write
     procedure :: execute => cmd_beams_theta_execute
  end type cmd_beams_theta_t

  type, extends (cmd_beams_pol_fraction_t) :: cmd_beams_phi_t
   contains
     procedure :: write => cmd_beams_phi_write
     procedure :: execute => cmd_beams_phi_execute
  end type cmd_beams_phi_t

  type, extends (command_t) :: cmd_cuts_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
   contains
     procedure :: write => cmd_cuts_write
     procedure :: compile => cmd_cuts_compile
     procedure :: execute => cmd_cuts_execute
  end type cmd_cuts_t

  type, extends (command_t) :: cmd_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
   contains
     procedure :: write => cmd_scale_write
     procedure :: compile => cmd_scale_compile 
     procedure :: execute => cmd_scale_execute   
  end type cmd_scale_t

  type, extends (command_t) :: cmd_fac_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
   contains
     procedure :: write => cmd_fac_scale_write
     procedure :: compile => cmd_fac_scale_compile
     procedure :: execute => cmd_fac_scale_execute
  end type cmd_fac_scale_t

  type, extends (command_t) :: cmd_ren_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
   contains
     procedure :: write => cmd_ren_scale_write
     procedure :: compile => cmd_ren_scale_compile
     procedure :: execute => cmd_ren_scale_execute
  end type cmd_ren_scale_t

  type, extends (command_t) :: cmd_weight_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
   contains
     procedure :: write => cmd_weight_write
     procedure :: compile => cmd_weight_compile
     procedure :: execute => cmd_weight_execute  
  end type cmd_weight_t

  type, extends (command_t) :: cmd_selection_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
   contains
     procedure :: write => cmd_selection_write
     procedure :: compile => cmd_selection_compile
     procedure :: execute => cmd_selection_execute
  end type cmd_selection_t

  type, extends (command_t) :: cmd_reweight_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
   contains
     procedure :: write => cmd_reweight_write
     procedure :: compile => cmd_reweight_compile
     procedure :: execute => cmd_reweight_execute  
  end type cmd_reweight_t

  type, extends (command_t) :: cmd_alt_setup_t
     private
     type(parse_node_p), dimension(:), allocatable :: setup
   contains
     procedure :: write => cmd_alt_setup_write
     procedure :: compile => cmd_alt_setup_compile
     procedure :: execute => cmd_alt_setup_execute
  end type cmd_alt_setup_t

  type, extends (command_t) :: cmd_integrate_t
     private
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
   contains
     procedure :: write => cmd_integrate_write
     procedure :: compile => cmd_integrate_compile
     procedure :: execute => cmd_integrate_execute
  end type cmd_integrate_t
     
  type, extends (command_t) :: cmd_observable_t
     private
     type(string_t) :: id
   contains
     procedure :: write => cmd_observable_write
     procedure :: compile => cmd_observable_compile
     procedure :: execute => cmd_observable_execute
  end type cmd_observable_t
     
  type, extends (command_t) :: cmd_histogram_t
     private
     type(string_t) :: id
     type(parse_node_t), pointer :: pn_lower_bound => null ()
     type(parse_node_t), pointer :: pn_upper_bound => null ()
     type(parse_node_t), pointer :: pn_bin_width => null ()
   contains
     procedure :: write => cmd_histogram_write
     procedure :: compile => cmd_histogram_compile
     procedure :: execute => cmd_histogram_execute
  end type cmd_histogram_t
     
  type, extends (command_t) :: cmd_plot_t
     private
     type(string_t) :: id
   contains
     procedure :: write => cmd_plot_write
     procedure :: compile => cmd_plot_compile
     procedure :: init => cmd_plot_init
     procedure :: execute => cmd_plot_execute
  end type cmd_plot_t
     
  type, extends (command_t) :: cmd_graph_t
     private
     type(string_t) :: id
     integer :: n_elements = 0
     type(cmd_plot_t), dimension(:), allocatable :: el
     type(string_t), dimension(:), allocatable :: element_id
   contains
     procedure :: write => cmd_graph_write
     procedure :: compile => cmd_graph_compile
     procedure :: execute => cmd_graph_execute
  end type cmd_graph_t
     
  type :: analysis_id_t
    type(string_t) :: tag
    type(parse_node_t), pointer :: pn_sexpr => null ()
  end type analysis_id_t

  type, extends (command_t) :: cmd_analysis_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
   contains
     procedure :: write => cmd_analysis_write
     procedure :: compile => cmd_analysis_compile
     procedure :: execute => cmd_analysis_execute
  end type cmd_analysis_t

  type, extends (command_t) :: cmd_write_analysis_t
     private
     type(analysis_id_t), dimension(:), allocatable :: id
     type(string_t), dimension(:), allocatable :: tag
   contains
     procedure :: write => cmd_write_analysis_write
     procedure :: compile => cmd_write_analysis_compile
     procedure :: execute => cmd_write_analysis_execute
  end type cmd_write_analysis_t

  type, extends (command_t) :: cmd_compile_analysis_t
     private
     type(analysis_id_t), dimension(:), allocatable :: id
     type(string_t), dimension(:), allocatable :: tag
   contains
     procedure :: write => cmd_compile_analysis_write
     procedure :: compile => cmd_compile_analysis_compile
     procedure :: execute => cmd_compile_analysis_execute
  end type cmd_compile_analysis_t

  type, extends (command_t) :: cmd_open_out_t
     private
     type(parse_node_t), pointer :: file_expr => null ()
   contains
     procedure :: write => cmd_open_out_write
     procedure :: compile => cmd_open_out_compile
     procedure :: execute => cmd_open_out_execute
  end type cmd_open_out_t

  type, extends (cmd_open_out_t) :: cmd_close_out_t
     private
   contains
     procedure :: execute => cmd_close_out_execute
  end type cmd_close_out_t

  type, extends (command_t) :: cmd_printf_t
     private
     type(parse_node_t), pointer :: sexpr => null ()
     type(parse_node_t), pointer :: sprintf_fun => null ()
     type(parse_node_t), pointer :: sprintf_clause => null ()
     type(parse_node_t), pointer :: sprintf => null ()
   contains
     procedure :: final => cmd_printf_final
     procedure :: write => cmd_printf_write
     procedure :: compile => cmd_printf_compile
     procedure :: execute => cmd_printf_execute
  end type cmd_printf_t

  type, extends (command_t) :: cmd_record_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
   contains
     procedure :: write => cmd_record_write
     procedure :: compile => cmd_record_compile
     procedure :: execute => cmd_record_execute
  end type cmd_record_t
     
  type, extends (command_t) :: cmd_unstable_t
     private
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     type(parse_node_t), pointer :: pn_prt_in => null ()
   contains
     procedure :: write => cmd_unstable_write
     procedure :: compile => cmd_unstable_compile
     procedure :: execute => cmd_unstable_execute
  end type cmd_unstable_t
     
  type, extends (command_t) :: cmd_stable_t
     private
     type(parse_node_p), dimension(:), allocatable :: pn_pdg
   contains
     procedure :: write => cmd_stable_write
     procedure :: compile => cmd_stable_compile
     procedure :: execute => cmd_stable_execute
  end type cmd_stable_t
  
  type, extends (cmd_stable_t) :: cmd_polarized_t
   contains
     procedure :: write => cmd_polarized_write
     procedure :: execute => cmd_polarized_execute
  end type cmd_polarized_t
  
  type, extends (cmd_stable_t) :: cmd_unpolarized_t
   contains
     procedure :: write => cmd_unpolarized_write
     procedure :: execute => cmd_unpolarized_execute
  end type cmd_unpolarized_t
  
  type, extends (command_t) :: cmd_sample_format_t
     private
     type(string_t), dimension(:), allocatable :: format
   contains
     procedure :: write => cmd_sample_format_write
     procedure :: compile => cmd_sample_format_compile
     procedure :: execute => cmd_sample_format_execute
  end type cmd_sample_format_t

  type, extends (command_t) :: cmd_simulate_t
     ! not private anymore as required by the whizard-c-interface
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
   contains
     procedure :: write => cmd_simulate_write
     procedure :: compile => cmd_simulate_compile
     procedure :: execute => cmd_simulate_execute
  end type cmd_simulate_t

  type, extends (command_t) :: cmd_rescan_t
     ! private
     type(parse_node_t), pointer :: pn_filename => null ()
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
   contains
     procedure :: write => cmd_rescan_write
     procedure :: compile => cmd_rescan_compile
     procedure :: execute => cmd_rescan_execute
  end type cmd_rescan_t

  type, extends (command_t) :: cmd_iterations_t
     private
     integer :: n_pass = 0
     type(parse_node_p), dimension(:), allocatable :: pn_expr_n_it
     type(parse_node_p), dimension(:), allocatable :: pn_expr_n_calls
     type(parse_node_p), dimension(:), allocatable :: pn_sexpr_adapt
   contains
     procedure :: write => cmd_iterations_write
     procedure :: compile => cmd_iterations_compile
     procedure :: execute => cmd_iterations_execute
  end type cmd_iterations_t

  type, abstract :: range_t
     type(parse_node_t), pointer :: pn_expr => null ()
     type(parse_node_t), pointer :: pn_term => null ()
     type(parse_node_t), pointer :: pn_factor => null ()
     type(parse_node_t), pointer :: pn_value => null ()
     type(parse_node_t), pointer :: pn_literal => null ()
     type(parse_node_t), pointer :: pn_beg => null ()
     type(parse_node_t), pointer :: pn_end => null ()
     type(parse_node_t), pointer :: pn_step => null ()
     type(eval_tree_t) :: expr_beg
     type(eval_tree_t) :: expr_end
     type(eval_tree_t) :: expr_step
     integer :: step_mode = 0
     integer :: n_step = 0
   contains
     procedure :: final => range_final
     procedure (range_write), deferred :: write
     procedure :: base_write => range_write
     procedure :: init => range_init
     procedure :: create_value_node => range_create_value_node
     procedure :: compile => range_compile
     procedure (range_evaluate), deferred :: evaluate
     procedure :: get_n_iterations => range_get_n_iterations
     procedure (range_set_value), deferred :: set_value
  end type range_t
     
  type, extends (range_t) :: range_int_t
     integer :: i_beg = 0
     integer :: i_end = 0
     integer :: i_step = 0
   contains
     procedure :: write => range_int_write
     procedure :: evaluate => range_int_evaluate
     procedure :: set_value => range_int_set_value
end type range_int_t
     
  type, extends (range_t) :: range_real_t
     real(default) :: r_beg = 0
     real(default) :: r_end = 0
     real(default) :: r_step = 0
     real(default) :: lr_beg  = 0
     real(default) :: lr_end  = 0
     real(default) :: lr_step = 0
   contains
     procedure :: write => range_real_write
     procedure :: evaluate => range_real_evaluate
     procedure :: set_value => range_real_set_value
end type range_real_t
     
  type, extends (command_t) :: cmd_scan_t
     private
     type(string_t) :: name
     integer :: n_values = 0
     type(parse_node_p), dimension(:), allocatable :: scan_cmd
     !!! !!! gfortran 4.7.x memory corruption
     !!!  class(range_t), dimension(:), allocatable :: range
     type(range_int_t), dimension(:), allocatable :: range_int
     type(range_real_t), dimension(:), allocatable :: range_real
   contains
     procedure :: final => cmd_scan_final
     procedure :: write => cmd_scan_write
     procedure :: compile => cmd_scan_compile
     procedure :: execute => cmd_scan_execute  
  end type cmd_scan_t

  type, extends (command_t) :: cmd_if_t
     private
     type(parse_node_t), pointer :: pn_if_lexpr => null ()
     type(command_list_t), pointer :: if_body => null ()
     type(cmd_if_t), dimension(:), pointer :: elsif_cmd => null ()
     type(command_list_t), pointer :: else_body => null ()
   contains
     procedure :: final => cmd_if_final
     procedure :: write => cmd_if_write
     procedure :: compile => cmd_if_compile
     procedure :: execute => cmd_if_execute
  end type cmd_if_t

  type, extends (command_t) :: cmd_include_t
     private
     type(string_t) :: file
     type(command_list_t), pointer :: command_list => null ()
     type(parse_tree_t) :: parse_tree
   contains
     procedure :: final => cmd_include_final
     procedure :: write => cmd_include_write
     procedure :: compile => cmd_include_compile
     procedure :: execute => cmd_include_execute
  end type cmd_include_t

  type, extends (command_t) :: cmd_quit_t
     private
     logical :: has_code = .false.
     type(parse_node_t), pointer :: pn_code_expr => null ()
   contains
     procedure :: write => cmd_quit_write
     procedure :: compile => cmd_quit_compile
     procedure :: execute => cmd_quit_execute
  end type cmd_quit_t

  type :: command_list_t
     ! not private anymore as required by the whizard-c-interface
     class(command_t), pointer :: first => null ()
     class(command_t), pointer :: last => null ()
   contains
     procedure :: write => command_list_write
     procedure :: append => command_list_append
     procedure :: final => command_list_final
     procedure :: compile => command_list_compile
     procedure :: execute => command_list_execute
  end type command_list_t


  type(syntax_t), target, save :: syntax_cmd_list


  integer, parameter, public :: SHOW_BUFFER_SIZE = 4096
  character(*), parameter, public :: &
       DEFAULT_ANALYSIS_FILENAME = "whizard_analysis.dat"
  character(len=1), dimension(2), parameter, public :: &
       FORBIDDEN_ENDINGS1 = [ "o", "a" ]
  character(len=2), dimension(5), parameter, public :: &       
       FORBIDDEN_ENDINGS2 = [ "mp", "ps", "vg", "lo", "la" ]
  character(len=3), dimension(16), parameter, public :: &
       FORBIDDEN_ENDINGS3 = [ "aux", "dvi", "evt", "evx", "f03", "f90", &
          "f95", "log", "ltp", "mpx", "olc", "olp", "pdf", "phs", "sin", "tex" ]
       
  integer, parameter :: STEP_NONE = 0
  integer, parameter :: STEP_ADD = 1
  integer, parameter :: STEP_SUB = 2
  integer, parameter :: STEP_MUL = 3
  integer, parameter :: STEP_DIV = 4
  integer, parameter :: STEP_COMP_ADD = 11
  integer, parameter :: STEP_COMP_MUL = 13

  abstract interface
     subroutine command_write (cmd, unit, indent)
       import
       class(command_t), intent(in) :: cmd
       integer, intent(in), optional :: unit, indent
     end subroutine command_write
  end interface
  
  abstract interface
     subroutine command_compile (cmd, global)
       import
       class(command_t), intent(inout) :: cmd
       type(rt_data_t), intent(inout), target :: global
     end subroutine command_compile
  end interface

  abstract interface
     subroutine command_execute (cmd, global)
       import
       class(command_t), intent(inout) :: cmd
       type(rt_data_t), intent(inout), target :: global
     end subroutine command_execute
  end interface

  interface
     subroutine get_prclib_static (libname)
       import
       type(string_t), dimension(:), intent(inout), allocatable :: libname
     end subroutine get_prclib_static
  end interface

  abstract interface
     subroutine range_evaluate (range)
       import
       class(range_t), intent(inout) :: range
     end subroutine range_evaluate
  end interface
  
  abstract interface
     subroutine range_set_value (range, i)
       import
       class(range_t), intent(inout) :: range
       integer, intent(in) :: i
     end subroutine range_set_value
  end interface
  

contains

  recursive subroutine command_final (cmd)
    class(command_t), intent(inout) :: cmd
    if (associated (cmd%options)) then
       call cmd%options%final ()
       deallocate (cmd%options)
       call cmd%local%local_final ()
       deallocate (cmd%local)
    else
       cmd%local => null ()
    end if
  end subroutine command_final

  subroutine dispatch_command (command, pn)
    class(command_t), intent(inout), pointer :: command
    type(parse_node_t), intent(in), target :: pn
    select case (char (parse_node_get_rule_key (pn)))
    case ("cmd_model")
       allocate (cmd_model_t :: command)
    case ("cmd_library")
       allocate (cmd_library_t :: command)
    case ("cmd_process")
       allocate (cmd_process_t :: command)
    case ("cmd_nlo")
       allocate (cmd_nlo_t :: command)
    case ("cmd_compile")
       allocate (cmd_compile_t :: command)
    case ("cmd_exec")
       allocate (cmd_exec_t :: command)
     case ("cmd_num", "cmd_complex", "cmd_real", "cmd_int", &
           "cmd_log_decl", "cmd_log", "cmd_string", "cmd_string_decl", &
           "cmd_alias", "cmd_result")
       allocate (cmd_var_t :: command)
    case ("cmd_slha")
       allocate (cmd_slha_t :: command)
    case ("cmd_show")
       allocate (cmd_show_t :: command)
    case ("cmd_clear")
       allocate (cmd_clear_t :: command)
    case ("cmd_expect")
       allocate (cmd_expect_t :: command)
    case ("cmd_beams")
       allocate (cmd_beams_t :: command)
    case ("cmd_beams_pol_density")
       allocate (cmd_beams_pol_density_t :: command)
    case ("cmd_beams_pol_fraction")
       allocate (cmd_beams_pol_fraction_t :: command)
    case ("cmd_beams_momentum")
       allocate (cmd_beams_momentum_t :: command)
    case ("cmd_beams_theta")
       allocate (cmd_beams_theta_t :: command)
    case ("cmd_beams_phi")
       allocate (cmd_beams_phi_t :: command)
    case ("cmd_cuts")
       allocate (cmd_cuts_t :: command)
    case ("cmd_scale")
       allocate (cmd_scale_t :: command)
    case ("cmd_fac_scale")
       allocate (cmd_fac_scale_t :: command)
    case ("cmd_ren_scale")
       allocate (cmd_ren_scale_t :: command)
    case ("cmd_weight")
       allocate (cmd_weight_t :: command)
    case ("cmd_selection")
       allocate (cmd_selection_t :: command)
    case ("cmd_reweight")
       allocate (cmd_reweight_t :: command)
    case ("cmd_iterations")
       allocate (cmd_iterations_t :: command)
    case ("cmd_integrate")
       allocate (cmd_integrate_t :: command)
    case ("cmd_observable")
       allocate (cmd_observable_t :: command)
    case ("cmd_histogram")
       allocate (cmd_histogram_t :: command)
    case ("cmd_plot")
       allocate (cmd_plot_t :: command)
    case ("cmd_graph")
       allocate (cmd_graph_t :: command)
    case ("cmd_record")
       allocate (cmd_record_t :: command)
    case ("cmd_analysis")
       allocate (cmd_analysis_t :: command)
    case ("cmd_alt_setup")
       allocate (cmd_alt_setup_t :: command)
    case ("cmd_unstable")
       allocate (cmd_unstable_t :: command)
    case ("cmd_stable")
       allocate (cmd_stable_t :: command)
    case ("cmd_polarized")
       allocate (cmd_polarized_t :: command)
    case ("cmd_unpolarized")
       allocate (cmd_unpolarized_t :: command)
    case ("cmd_sample_format")
       allocate (cmd_sample_format_t :: command)
    case ("cmd_simulate")
       allocate (cmd_simulate_t :: command)
    case ("cmd_rescan")
       allocate (cmd_rescan_t :: command)
    case ("cmd_write_analysis")
       allocate (cmd_write_analysis_t :: command)
    case ("cmd_compile_analysis")
       allocate (cmd_compile_analysis_t :: command)
    case ("cmd_open_out")
       allocate (cmd_open_out_t :: command)
    case ("cmd_close_out")
       allocate (cmd_close_out_t :: command)
    case ("cmd_printf")
       allocate (cmd_printf_t :: command)
    case ("cmd_scan")
       allocate (cmd_scan_t :: command)
    case ("cmd_if")
       allocate (cmd_if_t :: command)
    case ("cmd_include")
       allocate (cmd_include_t :: command)
    case ("cmd_quit")
       allocate (cmd_quit_t :: command)
    case default
       print *, char (parse_node_get_rule_key (pn))
       call msg_bug ("Command not implemented")
    end select
    command%pn => pn
  end subroutine dispatch_command

  recursive subroutine command_write_options (cmd, unit, indent)
    class(command_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: ind
    ind = 1;  if (present (indent))  ind = indent + 1
    if (associated (cmd%options))  call cmd%options%write (unit, ind)
  end subroutine command_write_options
  
  recursive subroutine command_compile_options (cmd, global)
    class(command_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (associated (cmd%pn_opt)) then
       allocate (cmd%local)
       call cmd%local%local_init (global)
       call global%copy_globals (cmd%local)
       allocate (cmd%options)
       call cmd%options%compile (cmd%pn_opt, cmd%local)
       call global%restore_globals (cmd%local)
       call cmd%local%deactivate ()
    else
       cmd%local => global
    end if
  end subroutine command_compile_options
  
  recursive subroutine cmd_execute_options (cmd, global)
    class(command_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (associated (cmd%options)) then
       call cmd%local%activate ()
       call cmd%options%execute (cmd%local)
    end if
  end subroutine cmd_execute_options

  subroutine cmd_reset_options (cmd, global)
    class(command_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (associated (cmd%options)) then
       call cmd%local%deactivate (global)
    end if
  end subroutine cmd_reset_options
  
  subroutine cmd_model_write (cmd, unit, indent)
    class(cmd_model_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,'""',A,'""')")  "model =", char (cmd%name)
  end subroutine cmd_model_write

  subroutine cmd_model_compile (cmd, global)
    class(cmd_model_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_name, pn_arg, pn_scheme
    type(model_t), pointer :: model
    pn_name => cmd%pn%get_sub_ptr (3)
    pn_arg => pn_name%get_next_ptr ()
    if (associated (pn_arg)) then
       pn_scheme => pn_arg%get_sub_ptr ()
    else
       pn_scheme => null ()
    end if
    cmd%name = pn_name%get_string ()
    if (associated (pn_scheme)) then
       cmd%scheme = pn_scheme%get_string ()
       call preload_model (model, cmd%name, cmd%scheme)
    else
       cmd%scheme = ""
       call preload_model (model, cmd%name)
    end if
    global%model => model
    if (associated (global%model)) then
       call global%model%link_var_list (global%var_list)
    end if
  contains
    subroutine preload_model (model, name, scheme)
      type(model_t), pointer, intent(out) :: model
      type(string_t), intent(in) :: name
      type(string_t), intent(in), optional :: scheme
      model => null ()
      if (associated (global%model)) then
         if (global%model%matches (name, scheme)) then
            model => global%model
         end if
      end if
      if (.not. associated (model)) then
         if (global%model_list%model_exists (name, scheme)) then
            model => global%model_list%get_model_ptr (name, scheme)
         else
            call global%read_model (name, model, scheme)
         end if
      end if
    end subroutine preload_model
  end subroutine cmd_model_compile

  subroutine cmd_model_execute (cmd, global)
    class(cmd_model_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(model_t), pointer :: model
    if (cmd%scheme /= "") then
       call global%select_model (cmd%name, cmd%scheme)
    else
       call global%select_model (cmd%name)
    end if
    if (.not. associated (global%model)) &
         call msg_fatal ("Switching to model '" &
         // char (cmd%name) // "': model not found")
  end subroutine cmd_model_execute

  subroutine cmd_library_write (cmd, unit, indent)
    class(cmd_library_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit)
    call write_indent (u, indent)
    write (u, "(1x,A,1x,'""',A,'""')")  "library =", char (cmd%name)
  end subroutine cmd_library_write

  subroutine cmd_library_compile (cmd, global)
    class(cmd_library_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_name
    pn_name => parse_node_get_sub_ptr (cmd%pn, 3)
    cmd%name = parse_node_get_string (pn_name)
  end subroutine cmd_library_compile

  subroutine cmd_library_execute (cmd, global)
    class(cmd_library_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(prclib_entry_t), pointer :: lib_entry
    type(process_library_t), pointer :: lib
    logical :: rebuild_library
    lib => global%prclib_stack%get_library_ptr (cmd%name)
    rebuild_library = &
         global%var_list%get_lval (var_str ("?rebuild_library"))    
    if (.not. (associated (lib))) then
       allocate (lib_entry)
       call lib_entry%init (cmd%name)
       lib => lib_entry%process_library_t
       call global%add_prclib (lib_entry)
    else
       call global%update_prclib (lib)
    end if
    if (associated (lib) .and. .not. rebuild_library) then
       call lib%update_status (global%os_data)
    end if
  end subroutine cmd_library_execute

  subroutine cmd_process_write (cmd, unit, indent)
    class(cmd_process_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A,A,I0,A)")  "process: ", char (cmd%id), " (", &
         size (cmd%pn_pdg_in), " -> X)"
    call cmd%write_options (u, indent)
  end subroutine cmd_process_write

  subroutine cmd_process_compile (cmd, global)
    class(cmd_process_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_id, pn_in, pn_codes
    integer :: i
    pn_id => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_in  => parse_node_get_next_ptr (pn_id, 2)
    cmd%pn_out => parse_node_get_next_ptr (pn_in, 2)
    cmd%pn_opt => parse_node_get_next_ptr (cmd%pn_out)
    call cmd%compile_options (global)
    cmd%id = parse_node_get_string (pn_id)
    cmd%n_in  = parse_node_get_n_sub (pn_in)
    pn_codes => parse_node_get_sub_ptr (pn_in)
    allocate (cmd%pn_pdg_in (cmd%n_in))
    do i = 1, cmd%n_in
       cmd%pn_pdg_in(i)%ptr => pn_codes
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
  end subroutine cmd_process_compile

  subroutine cmd_process_execute (cmd, global)
    class(cmd_process_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: pdg_in, pdg_out
    type(pdg_array_t), dimension(:), allocatable :: pdg_out_tab
    type(string_t), dimension(:), allocatable :: prt_in
    type(string_t) :: prt_out, prt_out1
    type(process_configuration_t) :: prc_config
    type(prt_expr_t) :: prt_expr_out
    type(prt_spec_t), dimension(:), allocatable :: prt_spec_in
    type(prt_spec_t), dimension(:), allocatable :: prt_spec_out
    type(var_list_t), pointer :: var_list
    integer, dimension(:), allocatable :: pdg
    integer, dimension(:), allocatable :: i_term
    integer :: i, j, n_in, n_out, n_terms, n_components
    logical :: nlo_fixed_order
    type(string_t), dimension(:), allocatable :: prt_in_nlo, prt_out_nlo
    type(radiation_generator_t) :: radiation_generator
    type(pdg_list_t) :: pl_in, pl_out
    type(string_t) :: born_me_method
    type(string_t) :: real_tree_me_method
    type(string_t) :: loop_me_method
    type(string_t) :: correlation_me_method
    integer, dimension(:), allocatable :: i_list
    logical :: powheg_use_damping
    logical :: gks_active
    logical :: initial_state_colored
    logical :: has_structure_functions
    logical :: requires_pdf
    integer :: n_components_extra
    integer :: gks_multiplicity
    integer :: n_emitters
    integer, dimension(:), allocatable :: emitters
    integer :: n_components_init
    integer :: alpha_power, alphas_power
    logical :: requires_soft_mismatch
    call msg_debug (D_CORE, "cmd_process_execute")
    var_list => cmd%local%get_var_list_ptr ()

    nlo_fixed_order = cmd%local%nlo_fixed_order
    powheg_use_damping = &
       var_list%get_lval (var_str ('?powheg_use_damping'))
    gks_multiplicity = var_list%get_ival (var_str ('gks_multiplicity'))
    gks_active = gks_multiplicity > 2
    born_me_method = var_list%get_sval (var_str ("$born_me_method"))
    real_tree_me_method = var_list%get_sval (var_str ("$real_tree_me_method"))
    loop_me_method = var_list%get_sval (var_str ("$loop_me_method"))
    correlation_me_method = var_list%get_sval (var_str ("$correlation_me_method"))
    call check_nlo_options (nlo_fixed_order, var_list)
    if (any (cmd%local%active_nlo_components))  &
       call override_local_me_method (born_me_method)

    n_in = size (cmd%pn_pdg_in)
    allocate (prt_in (n_in), prt_spec_in (n_in))
    do i = 1, n_in
       pdg_in = &
            eval_pdg_array (cmd%pn_pdg_in(i)%ptr, var_list)
       prt_in(i) = make_flavor_string (pdg_in, cmd%local%model)
       prt_spec_in(i) = new_prt_spec (prt_in(i))
    end do
    call compile_prt_expr &
         (prt_expr_out, cmd%pn_out, var_list, cmd%local%model)
    call prt_expr_out%expand ()
    call scan_components ()
    call determine_needed_components ()
    call prc_config%init (cmd%id, n_in, n_components_init, cmd%local)

    alpha_power = var_list%get_ival (var_str ("alpha_power"))
    alphas_power = var_list%get_ival (var_str ("alphas_power"))
    call prc_config%set_coupling_powers (alpha_power, alphas_power)

    call setup_components ()
    call prc_config%record (cmd%local)

  contains

    subroutine override_local_me_method (me_method)
      type(string_t), intent(in) :: me_method
      call var_list%set_string (var_str ("$method"), me_method, is_known=.true.)
    end subroutine override_local_me_method

    subroutine determine_needed_components ()
      type(string_t) :: fks_method
      if (nlo_fixed_order .or. gks_active) &
           call setup_radiation_generator ()

      if (powheg_use_damping) then
         emitters = radiation_generator%get_emitter_indices()
         n_emitters = size (emitters)
      end if

      if (nlo_fixed_order) then
         fks_method = var_list%get_sval (var_str ('$fks_mapping_type'))
         requires_soft_mismatch = fks_method == var_str ('resonances')
         n_components_extra = needed_extra_components (initial_state_colored, &
              has_structure_functions, requires_soft_mismatch, &
              powheg_use_damping, n_emitters)
         allocate (i_list (n_components_extra))
      else if (gks_active) then
         call radiation_generator%generate_multiple (gks_multiplicity)
         n_components_extra = radiation_generator%get_n_gks_states ()
      end if

      if (nlo_fixed_order .and. .not. powheg_use_damping) then
         n_components_init = n_components * n_components_extra
      else if (nlo_fixed_order .and. powheg_use_damping) then
         n_components_init = n_components * n_components_extra
      else if (gks_active) then
         n_components_init = n_components * (n_components_extra + 1)
      else
         n_components_init = n_components
      end if
    end subroutine determine_needed_components

    subroutine setup_radiation_generator ()
      call split_prt (prt_spec_in, n_in, pl_in)
      call split_prt (prt_spec_out, n_out, pl_out)
      call radiation_generator%init (pl_in, pl_out, qcd = .true., qed = .false.)
      call radiation_generator%set_n (n_in, n_out, 0)
      initial_state_colored = pdg_in%has_colored_particles()
      has_structure_functions = global%beam_structure%get_n_record () > 0
      requires_pdf = initial_state_colored .and. has_structure_functions
      if (requires_pdf)  call radiation_generator%set_initial_state_emissions ()
      call radiation_generator%set_constraints (.false., .false., .true., .true.)
      call radiation_generator%set_radiation_model (cmd%local%radiation_model)
      call radiation_generator%setup_if_table ()
    end subroutine setup_radiation_generator

    subroutine scan_components ()
      n_terms = prt_expr_out%get_n_terms ()
      allocate (pdg_out_tab (n_terms))
      allocate (i_term (n_terms), source = 0)
      n_components = 0
      SCAN: do i = 1, n_terms
         if (allocated (pdg))  deallocate (pdg)
         call prt_expr_out%term_to_array (prt_spec_out, i)
         n_out = size (prt_spec_out)
         allocate (pdg (n_out))
         do j = 1, n_out
            prt_out = prt_spec_out(j)%to_string ()
            call split (prt_out, prt_out1, ":")
            pdg(j) = cmd%local%model%get_pdg (prt_out1)
         end do
         pdg_out = sort (pdg)
         do j = 1, n_components
            if (pdg_out == pdg_out_tab(j))  cycle SCAN
         end do
         n_components = n_components + 1
         i_term(n_components) = i
         pdg_out_tab(n_components) = pdg_out
      end do SCAN
    end subroutine scan_components 

    subroutine split_prt (prt, n_out, pl)
      type(prt_spec_t), intent(in), dimension(:), allocatable :: prt
      integer, intent(in) :: n_out
      type(pdg_list_t), intent(out) :: pl
      type(pdg_array_t) :: pdg
      type(string_t) :: prt_string, prt_tmp
      integer, parameter :: max_particle_number = 25
      integer, dimension(max_particle_number) :: i_particle
      integer :: i, j, n
      i_particle = 0
      call pl%init (n_out)
      do i = 1, n_out
         n = 1
         prt_string = prt(i)%to_string ()
         do
           call split (prt_string, prt_tmp, ":")
           if (prt_tmp /= "") then
             i_particle(n) = cmd%local%model%get_pdg (prt_tmp)
             n = n + 1
           else
             exit
           end if
         end do
         call pdg_array_init (pdg, n - 1)
         do j = 1, n - 1
           call pdg%set (j, i_particle(j))
         end do
         call pl%set (i, pdg)
         call pdg_array_delete (pdg)
      end do
    end subroutine split_prt

    subroutine setup_components()
      integer :: i_comp
      call msg_debug (D_CORE, "setup_components")
      do i = 1, n_components
         call prt_expr_out%term_to_array (prt_spec_out, i_term(i))
         if (nlo_fixed_order) then
            associate (active_comp => cmd%local%active_nlo_components)
               call set_component_list (i_list, i, n_components, &
                    requires_pdf, requires_soft_mismatch, &
                    powheg_use_damping, n_emitters)

               call override_local_me_method (born_me_method)
               i_comp = i
               call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
               call prc_config%setup_component (i_comp, &
                    prt_spec_in, prt_spec_out, &
                    cmd%local%model, var_list, BORN, &
                    active_in = active_comp (1))

               call radiation_generator%generate (prt_in_nlo, prt_out_nlo)
               call override_local_me_method (real_tree_me_method)
               i_comp = n_components + i
               call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
               call prc_config%setup_component (i_comp, &
                    new_prt_spec (prt_in_nlo), new_prt_spec (prt_out_nlo), &
                    cmd%local%model, var_list, NLO_REAL, &
                    active_in = active_comp (NLO_REAL + 1))

               call override_local_me_method (loop_me_method)
               i_comp = n_components * 2 + i
               call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
               call prc_config%setup_component (i_comp, &
                    prt_spec_in, prt_spec_out, &
                    cmd%local%model, var_list, NLO_VIRTUAL, &
                    active_in = active_comp (NLO_VIRTUAL + 1))

               call override_local_me_method (correlation_me_method)
               i_comp = n_components * 3 + i
               call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
               call prc_config%setup_component (i_comp, &
                    prt_spec_in, prt_spec_out, &
                    cmd%local%model, var_list, NLO_SUBTRACTION, &
                  active_in = active_comp (NLO_SUBTRACTION + 1))

               if (powheg_use_damping) then
                  call override_local_me_method (real_tree_me_method)
                  i_comp = n_components * 4 + i
                  call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
                  call prc_config%setup_component (i_comp, &
                       new_prt_spec (prt_in_nlo), new_prt_spec (prt_out_nlo), &
                       cmd%local%model, var_list, NLO_REAL, &
                       active_in = active_comp (NLO_REAL + 1))
               end if

               if (requires_pdf) then
                  ! TODO: (bcn 2016-01-26) why only omega here?
                  call override_local_me_method (var_str ("omega"))
                  i_comp = n_components * 4 + i
                  call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
                  call prc_config%setup_component (i_comp, &
                       prt_spec_in, prt_spec_out, &
                       cmd%local%model, var_list, NLO_DGLAP, &
                       active_in = active_comp (NLO_DGLAP + 1))
               end if

               if (requires_soft_mismatch) then
                  ! TODO: (bcn 2016-01-26) why only omega here?
                  call override_local_me_method (var_str ("omega"))
                  i_comp = n_components * 4 + i
                  call msg_debug (D_CORE, "Setting up this NLO component:", i_comp)
                  call prc_config%setup_component (i_comp, &
                     new_prt_spec (prt_in_nlo), new_prt_spec (prt_out_nlo), &
                     cmd%local%model, var_list, NLO_MISMATCH, &
                     active_in = active_comp (NLO_MISMATCH + 1))
               end if

               call prc_config%set_component_associations (i_list, &
                    requires_pdf, powheg_use_damping, requires_soft_mismatch)
            end associate
         else if (gks_active) then
            call override_local_me_method (var_str ("omega"))
            call prc_config%setup_component (i, prt_spec_in, prt_spec_out, &
                 cmd%local%model, var_list, BORN, active_in = .true.)
            call radiation_generator%reset_queue ()
            do j = 1, n_components_extra
               prt_out_nlo =  radiation_generator%get_next_state ()
               call prc_config%setup_component (i + j, &
                  new_prt_spec (prt_in), new_prt_spec (prt_out_nlo), &
                  cmd%local%model, var_list, GKS, active_in = .false.)
            end do
         else
            call prc_config%setup_component (i, &
                 prt_spec_in, prt_spec_out, &
                 cmd%local%model, var_list)
         end if
      end do
    end subroutine setup_components


  end subroutine cmd_process_execute

  subroutine check_nlo_options (nlo, var_list)
    logical, intent(in) :: nlo
    type(var_list_t), intent(in) :: var_list
    logical :: combined, powheg
    logical :: case_lo_but_any_other
    logical :: case_nlo_powheg_but_not_combined
    logical :: vamp_equivalences_enabled
    combined = var_list%get_lval (var_str ('?combined_nlo_integration'))
    powheg = var_list%get_lval (var_str ('?powheg_matching'))
    case_lo_but_any_other = .not. nlo .and. any ([combined, powheg])
    case_nlo_powheg_but_not_combined = &
         nlo .and. powheg .and. .not. combined
    if (case_lo_but_any_other) then
        call msg_fatal ("Option mismatch: Leading order process is selected &
                        &but either powheg_matching or combined_nlo_integration &
                        &is set to true.")
    else if (case_nlo_powheg_but_not_combined) then
        call msg_fatal ("POWHEG requires the 'combined_nlo_integration'-option &
                        &to be set to true.")
    end if
    vamp_equivalences_enabled = var_list%get_lval &
       (var_str ('?use_vamp_equivalences'))
    if (nlo .and. vamp_equivalences_enabled) &
       call msg_warning ("You have switched on VAMP equivalences. ", &
                         [var_str ("Note that they are automatically switched off "), &
                          var_str ("for NLO calculations.")])
  end subroutine check_nlo_options

  pure subroutine set_component_list (i_list, i, n_components, &
                  requires_pdf, requires_soft_mismatch, &
             powheg_use_damping, n_emitters)
    integer, dimension(:), intent(out) :: i_list
    integer, intent(in) :: i, n_components, n_emitters
    logical, intent(in) :: requires_pdf, powheg_use_damping, &
         requires_soft_mismatch
    i_list(1) = i
    i_list(2) = i + n_components
    i_list(3) = i + 2 * n_components
    i_list(4) = i + 3 * n_components
    if (requires_pdf .or. requires_soft_mismatch .or. powheg_use_damping) then
       i_list(5) = i + 4 * n_components
    end if
  end subroutine set_component_list

  pure function needed_extra_components (initial_state_colored, &
            has_structure_functions, requires_soft_mismatch, &
            powheg_use_damping, n_emitters) result (n)
    integer :: n
    logical, intent(in) :: initial_state_colored, &
         has_structure_functions, powheg_use_damping, &
         requires_soft_mismatch
    integer, intent(in) :: n_emitters
    if (initial_state_colored) then
       if (has_structure_functions) then
          n = 5
       else
          n = 4
       end if
    else if (powheg_use_damping) then
       n = 5
    else if (requires_soft_mismatch) then
       n = 5
    else
       n = 4
    end if
  end function needed_extra_components

  function make_flavor_string (aval, model) result (prt)
    type(string_t) :: prt
    type(pdg_array_t), intent(in) :: aval
    type(model_t), intent(in), target :: model
    integer, dimension(:), allocatable :: pdg
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    pdg = aval
    allocate (flv (size (pdg)))
    call flv%init (pdg, model)
    if (size (pdg) /= 0) then
       prt = flv(1)%get_name ()
       do i = 2, size (flv)
          prt = prt // ":" // flv(i)%get_name ()
       end do
    else
       prt = "?"
    end if
  end function make_flavor_string

  function make_pdg_array (prt, model) result (pdg_array)
    type(prt_spec_t), intent(in), dimension(:) :: prt
    type(model_t), intent(in) :: model
    integer, dimension(:), allocatable :: aval
    type(pdg_array_t) :: pdg_array
    type(flavor_t) :: flv
    integer :: k
    allocate (aval (size (prt)))
    do k = 1, size (prt)
      call flv%init (prt(k)%to_string (), model)
      aval (k) = flv%get_pdg ()
    end do
    pdg_array = aval
  end function make_pdg_array

  recursive subroutine compile_prt_expr (prt_expr, pn, var_list, model)
    type(prt_expr_t), intent(out) :: prt_expr
    type(parse_node_t), intent(in), target :: pn
    type(var_list_t), intent(in), target :: var_list
    type(model_t), intent(in), target :: model
    type(parse_node_t), pointer :: pn_entry, pn_term, pn_addition
    type(pdg_array_t) :: pdg
    type(string_t) :: prt_string
    integer :: n_entry, n_term, i
    select case (char (parse_node_get_rule_key (pn)))
    case ("prt_state_list")
       n_entry = parse_node_get_n_sub (pn)
       pn_entry => parse_node_get_sub_ptr (pn)
       if (n_entry == 1) then
          call compile_prt_expr (prt_expr, pn_entry, var_list, model)
       else
          call prt_expr%init_list (n_entry)
          select type (x => prt_expr%x)
          type is (prt_spec_list_t)
             do i = 1, n_entry
                call compile_prt_expr (x%expr(i), pn_entry, var_list, model)
                pn_entry => parse_node_get_next_ptr (pn_entry)
             end do
          end select
       end if
    case ("prt_state_sum")
       n_term = parse_node_get_n_sub (pn)
       pn_term => parse_node_get_sub_ptr (pn)
       pn_addition => pn_term
       if (n_term == 1) then
          call compile_prt_expr (prt_expr, pn_term, var_list, model)
       else
          call prt_expr%init_sum (n_term)
          select type (x => prt_expr%x)
          type is (prt_spec_sum_t)
             do i = 1, n_term
                call compile_prt_expr (x%expr(i), pn_term, var_list, model)
                pn_addition => parse_node_get_next_ptr (pn_addition)
                if (associated (pn_addition)) &
                     pn_term => parse_node_get_sub_ptr (pn_addition, 2)
             end do
          end select
       end if
    case ("cexpr")
       pdg = eval_pdg_array (pn, var_list)
       prt_string = make_flavor_string (pdg, model)
       call prt_expr%init_spec (new_prt_spec (prt_string))
    case default
       call parse_node_write_rec (pn)
       call msg_bug ("compile prt expr: impossible syntax rule")
    end select
  end subroutine compile_prt_expr
          
  subroutine cmd_nlo_write (cmd, unit, indent)
    class(cmd_nlo_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
  end subroutine cmd_nlo_write

  subroutine cmd_nlo_compile (cmd, global)
    class(cmd_nlo_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_comp
    integer :: i
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 3)
    cmd%pn_components(1)%ptr => parse_node_get_sub_ptr (pn_arg)
    pn_comp => parse_node_get_next_ptr (cmd%pn_components(1)%ptr)
    i = 2
    do 
      if (associated (pn_comp)) then
         cmd%pn_components(i)%ptr => pn_comp
         pn_comp => parse_node_get_next_ptr (cmd%pn_components(i)%ptr)
         i = i + 1
      else
         exit
      end if
    end do
  end subroutine cmd_nlo_compile

  subroutine cmd_nlo_execute (cmd, global)
    class(cmd_nlo_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: current_component
    type(string_t) :: component_type
    type(string_t) :: string
    integer :: i, comp, j

    cmd%active_component = .false.

    current_component => cmd%pn_components(1)%ptr
    i = 2
    do while (associated (current_component))
       component_type = eval_string (current_component, global%var_list)
       comp = component_status (component_type)
       select case (comp)
       case (BORN, NLO_VIRTUAL, NLO_MISMATCH, NLO_DGLAP)
          cmd%active_component(comp + 1) = .true.
       case (NLO_REAL)
         cmd%active_component(comp + 1) = .true.
         cmd%active_component(NLO_SUBTRACTION + 1) = .true.
       case (NLO_FULL)
          cmd%active_component = .true.
       case default
          string = var_str ("")
          do j = BORN, NLO_DGLAP
             string = string // component_status (j) // ", "
          end do
          string = string // component_status (NLO_FULL)
          call msg_fatal (char("Invalid NLO mode! Valid inputs are: " // string))
       end select
       if (i >= 5) exit
       current_component => cmd%pn_components(i)%ptr
       i = i + 1
    end do
    global%nlo_fixed_order = any (cmd%active_component(2:5))
    global%active_nlo_components = cmd%active_component
  end subroutine cmd_nlo_execute

  subroutine cmd_compile_write (cmd, unit, indent)
    class(cmd_compile_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "compile ("
    if (allocated (cmd%libname)) then
       do i = 1, size (cmd%libname)
          if (i > 1)  write (u, "(A,1x)", advance="no")  ","
          write (u, "('""',A,'""')", advance="no")  char (cmd%libname(i))
       end do
    end if
    write (u, "(A)")  ")"
  end subroutine cmd_compile_write

  subroutine cmd_compile_compile (cmd, global)
    class(cmd_compile_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_clause, pn_arg, pn_lib
    type(parse_node_t), pointer :: pn_exec_name_spec, pn_exec_name
    integer :: n_lib, i
    pn_cmd => parse_node_get_sub_ptr (cmd%pn)
    pn_clause => parse_node_get_sub_ptr (pn_cmd)
    pn_exec_name_spec => parse_node_get_sub_ptr (pn_clause, 2)
    if (associated (pn_exec_name_spec)) then
       pn_exec_name => parse_node_get_sub_ptr (pn_exec_name_spec, 2)
    else
       pn_exec_name => null ()
    end if
    pn_arg => parse_node_get_next_ptr (pn_clause)
    cmd%pn_opt => parse_node_get_next_ptr (pn_cmd)
    call cmd%compile_options (global)
    if (associated (pn_arg)) then
       n_lib = parse_node_get_n_sub (pn_arg)
    else
       n_lib = 0
    end if
    if (n_lib > 0) then
       allocate (cmd%libname (n_lib))
       pn_lib => parse_node_get_sub_ptr (pn_arg)
       do i = 1, n_lib
          cmd%libname(i) = parse_node_get_string (pn_lib)
          pn_lib => parse_node_get_next_ptr (pn_lib)
       end do
    end if
    if (associated (pn_exec_name)) then
       cmd%make_executable = .true.
       cmd%exec_name = parse_node_get_string (pn_exec_name)
    end if
  end subroutine cmd_compile_compile

  subroutine cmd_compile_execute (cmd, global)
    class(cmd_compile_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(string_t), dimension(:), allocatable :: libname, libname_static
    integer :: i
    if (allocated (cmd%libname)) then
       allocate (libname (size (cmd%libname)))
       libname = cmd%libname
    else
       call cmd%local%prclib_stack%get_names (libname)
    end if
    if (cmd%make_executable) then
       call get_prclib_static (libname_static)
       do i = 1, size (libname)
          if (any (libname_static == libname(i))) then
             call msg_fatal ("Compile: can't include static library '" &
                  // char (libname(i)) // "'")
          end if
       end do
       call compile_executable (cmd%exec_name, libname, cmd%local)
    else
       do i = 1, size (libname)
          call compile_library (libname(i), cmd%local)
       end do
    end if
  end subroutine cmd_compile_execute
    
  subroutine cmd_exec_write (cmd, unit, indent)
    class(cmd_exec_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    if (associated (cmd%pn_command)) then
       write (u, "(1x,A)")  "exec: [command associated]"
    else
       write (u, "(1x,A)")  "exec: [undefined]"       
    end if
  end subroutine cmd_exec_write

  subroutine cmd_exec_compile (cmd, global)
    class(cmd_exec_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_command
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_command => parse_node_get_sub_ptr (pn_arg)
    cmd%pn_command => pn_command
  end subroutine cmd_exec_compile

  subroutine cmd_exec_execute (cmd, global)
    class(cmd_exec_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: command
    logical :: is_known
    integer :: status
    command = eval_string (cmd%pn_command, global%var_list, is_known=is_known)
    if (is_known) then
       if (command /= "") then
          call os_system_call (command, status, verbose=.true.)
          if (status /= 0) then
             write (msg_buffer, "(A,I0)")  "Return code = ", status
             call msg_message ()
             call msg_error ("System command returned with nonzero status code")
          end if
       end if 
    end if
  end subroutine cmd_exec_execute

  subroutine cmd_var_write (cmd, unit, indent)
    class(cmd_var_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A,A)", advance="no")  "var: ", char (cmd%name), " ("
    select case (cmd%type)
    case (V_NONE)
       write (u, "(A)", advance="no")  "[unknown]"
    case (V_LOG)
       write (u, "(A)", advance="no")  "logical"
    case (V_INT)
       write (u, "(A)", advance="no")  "int"
    case (V_REAL)
       write (u, "(A)", advance="no")  "real"
    case (V_CMPLX)
       write (u, "(A)", advance="no")  "complex"
    case (V_STR)
       write (u, "(A)", advance="no")  "string"
    case (V_PDG)
       write (u, "(A)", advance="no")  "alias"
    end select
    if (cmd%is_intrinsic) then
       write (u, "(A)", advance="no")  ", intrinsic"
    end if
    if (cmd%is_model_var) then
       write (u, "(A)", advance="no")  ", model"
    end if
    write (u, "(A)")  ")"
  end subroutine cmd_var_write

  subroutine cmd_var_compile (cmd, global)
    class(cmd_var_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_var, pn_name
    type(parse_node_t), pointer :: pn_result, pn_proc
    type(string_t) :: var_name
    type(var_list_t), pointer :: model_vars
    integer :: type
    logical :: new
    pn_result => null ()
    new = .false.
    select case (char (parse_node_get_rule_key (cmd%pn)))
    case ("cmd_log_decl");    type = V_LOG
       pn_var => parse_node_get_sub_ptr (cmd%pn, 2)
       if (.not. associated (pn_var)) then   ! handle masked syntax error 
          cmd%type = V_NONE; return
       end if
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       new = .true.
    case ("cmd_log");         type = V_LOG
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
    case ("cmd_int");         type = V_INT
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
       new = .true.
    case ("cmd_real");        type = V_REAL
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
       new = .true.
    case ("cmd_complex");       type = V_CMPLX
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
       new = .true.
    case ("cmd_num");         type = V_NONE
       pn_name => parse_node_get_sub_ptr (cmd%pn)
    case ("cmd_string_decl"); type = V_STR
       pn_var => parse_node_get_sub_ptr (cmd%pn, 2)
       if (.not. associated (pn_var)) then   ! handle masked syntax error 
          cmd%type = V_NONE; return
       end if
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       new = .true.
    case ("cmd_string");      type = V_STR
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
    case ("cmd_alias");       type = V_PDG
       pn_name => parse_node_get_sub_ptr (cmd%pn, 2)
       new = .true.
    case ("cmd_result");      type = V_REAL
       pn_name => parse_node_get_sub_ptr (cmd%pn)
       pn_result => parse_node_get_sub_ptr (pn_name)
       pn_proc => parse_node_get_next_ptr (pn_result)
    case default
       call parse_node_mismatch &
            ("logical|int|real|complex|?|$|alias|var_name", cmd%pn)  ! $
    end select
    if (.not. associated (pn_name)) then   ! handle masked syntax error 
       cmd%type = V_NONE; return
    end if
    if (.not. associated (pn_result)) then
       var_name = parse_node_get_string (pn_name)
    else
       var_name = parse_node_get_key (pn_result) &
            // "(" // parse_node_get_string (pn_proc) // ")"
    end if
    select case (type)
    case (V_LOG);  var_name = "?" // var_name
    case (V_STR);  var_name = "$" // var_name    ! $
    end select
    if (associated (global%model)) then
       model_vars => global%model%get_var_list_ptr ()
    else
       model_vars => null ()
    end if
    call var_list_check_observable (global%var_list, var_name, type)
    call var_list_check_result_var (global%var_list, var_name, type)
    call var_list_check_user_var (global%var_list, var_name, type, new)
    cmd%name = var_name
    cmd%pn_value => parse_node_get_next_ptr (pn_name, 2)
    if (global%var_list%contains (cmd%name, follow_link = .false.)) then
       ! local variable
       cmd%is_intrinsic = &
            global%var_list%is_intrinsic (cmd%name, follow_link = .false.)
       cmd%type = &
            global%var_list%get_type (cmd%name, follow_link = .false.)
    else
       if (new)  cmd%type = type
       if (global%var_list%contains (cmd%name, follow_link = .true.)) then
          ! global variable
          cmd%is_intrinsic = &
               global%var_list%is_intrinsic (cmd%name, follow_link = .true.)
          if (cmd%type == V_NONE) then
             cmd%type = &
                  global%var_list%get_type (cmd%name, follow_link = .true.)
          end if
       else if (associated (model_vars)) then  ! check model variable
          cmd%is_model_var = &
               model_vars%contains (cmd%name)
          if (cmd%type == V_NONE) then
             cmd%type = &
                  model_vars%get_type (cmd%name)
          end if
       end if
       if (cmd%type == V_NONE) then
          call msg_fatal ("Variable '" // char (cmd%name) // "' " &
               // "set without declaration")
          cmd%type = V_NONE;  return
       end if
       if (cmd%is_model_var) then
          if (new) then
             call msg_fatal ("Model variable '" // char (cmd%name) // "' " &
                  // "redeclared")
          else if (model_vars%is_locked (cmd%name)) then
             call msg_fatal ("Model variable '" // char (cmd%name) // "' " &
                  // "is locked")
          end if
       else
          select case (cmd%type)
          case (V_LOG)
             call var_list_append_log (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          case (V_INT)
             call var_list_append_int (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          case (V_REAL)
             call var_list_append_real (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          case (V_CMPLX)
             call var_list_append_cmplx (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          case (V_PDG)
             call var_list_append_pdg_array (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          case (V_STR)
             call var_list_append_string (global%var_list, cmd%name, &
                  intrinsic=cmd%is_intrinsic, user=.true.)
          end select
       end if
    end if
  end subroutine cmd_var_compile

  subroutine cmd_var_execute (cmd, global)
    class(cmd_var_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default) :: rval
    logical :: is_known, pacified
    var_list => global%get_var_list_ptr ()
    if (cmd%is_model_var) then
       pacified = var_list%get_lval (var_str ("?pacify"))     
       rval = eval_real (cmd%pn_value, var_list, is_known=is_known)
       call global%model_set_real &
            (cmd%name, rval, verbose=.true., pacified=pacified)
    else if (cmd%type /= V_NONE) then
       call cmd%set_value (var_list, verbose=.true.)
    end if
  end subroutine cmd_var_execute

  subroutine cmd_var_set_value (var, var_list, verbose, model_name)
    class(cmd_var_t), intent(inout) :: var
    type(var_list_t), intent(inout), target :: var_list
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    logical :: lval, pacified
    integer :: ival
    real(default) :: rval
    complex(default) :: cval
    type(pdg_array_t) :: aval
    type(string_t) :: sval
    logical :: is_known
    pacified = var_list%get_lval (var_str ("?pacify"))     
    select case (var%type)
    case (V_LOG)
       lval = eval_log (var%pn_value, var_list, is_known=is_known)
       call var_list%set_log (var%name, &
            lval, is_known, verbose=verbose, model_name=model_name)
    case (V_INT)
       ival = eval_int (var%pn_value, var_list, is_known=is_known)
       call var_list%set_int (var%name, &
            ival, is_known, verbose=verbose, model_name=model_name)
    case (V_REAL)
       rval = eval_real (var%pn_value, var_list, is_known=is_known)
       call var_list%set_real (var%name, &
            rval, is_known, verbose=verbose, &
            model_name=model_name, pacified = pacified)
    case (V_CMPLX)
       cval = eval_cmplx (var%pn_value, var_list, is_known=is_known)
       call var_list%set_cmplx (var%name, &
            cval, is_known, verbose=verbose, &
            model_name=model_name, pacified = pacified)
    case (V_PDG)
       aval = eval_pdg_array (var%pn_value, var_list, is_known=is_known)
       call var_list%set_pdg_array (var%name, &
            aval, is_known, verbose=verbose, model_name=model_name)
    case (V_STR)
       sval = eval_string (var%pn_value, var_list, is_known=is_known)
       call var_list%set_string (var%name, &
            sval, is_known, verbose=verbose, model_name=model_name)
    end select
  end subroutine cmd_var_set_value
  
  subroutine cmd_slha_write (cmd, unit, indent)
    class(cmd_slha_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A)")  "slha: file name  = ", char (cmd%file)
    write (u, "(1x,A,L1)") "slha: write mode = ", cmd%write_mode
  end subroutine cmd_slha_write

  subroutine cmd_slha_compile (cmd, global)
    class(cmd_slha_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_key, pn_arg, pn_file
    pn_key => parse_node_get_sub_ptr (cmd%pn)
    pn_arg => parse_node_get_next_ptr (pn_key)
    pn_file => parse_node_get_sub_ptr (pn_arg)
    call cmd%compile_options (global)
    cmd%pn_opt => parse_node_get_next_ptr (pn_arg)
    select case (char (parse_node_get_key (pn_key)))
    case ("read_slha")
       cmd%write_mode = .false.
    case ("write_slha")
       cmd%write_mode = .true.
    case default
       call parse_node_mismatch ("read_slha|write_slha",  cmd%pn)
    end select
    cmd%file = parse_node_get_string (pn_file)
  end subroutine cmd_slha_compile

  subroutine cmd_slha_execute (cmd, global)
    class(cmd_slha_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    logical :: input, spectrum, decays
    if (cmd%write_mode) then
       input = .true.
       spectrum = .false.
       decays = .false.
       if (.not. associated (cmd%local%model)) then
          call msg_fatal ("SLHA: local model not associated")
          return
       end if
       call slha_write_file &
            (cmd%file, cmd%local%model, &
             input = input, spectrum = spectrum, decays = decays)
    else
       if (.not. associated (global%model)) then
          call msg_fatal ("SLHA: global model not associated")
          return
       end if
       call dispatch_slha (cmd%local, &
            input = input, spectrum = spectrum, decays = decays)
       call global%ensure_model_copy ()
       call slha_read_file &
            (cmd%file, cmd%local%os_data, global%model, &
             input = input, spectrum = spectrum, decays = decays)
    end if
  end subroutine cmd_slha_execute

  subroutine cmd_show_write (cmd, unit, indent)
    class(cmd_show_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)  
    write (u, "(1x,A)", advance="no")  "show: "  
    if (allocated (cmd%name)) then
       do i = 1, size (cmd%name)
          write (u, "(1x,A)", advance="no")  char (cmd%name(i))
       end do
       write (u, *)
    else
       write (u, "(5x,A)")  "[undefined]"
    end if
  end subroutine cmd_show_write

  subroutine cmd_show_compile (cmd, global)
    class(cmd_show_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_var, pn_prefix, pn_name
    type(string_t) :: key
    integer :: i, n_args
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    if (associated (pn_arg)) then
       select case (char (parse_node_get_rule_key (pn_arg)))
       case ("show_arg")
          cmd%pn_opt => parse_node_get_next_ptr (pn_arg)
       case default
          cmd%pn_opt => pn_arg
          pn_arg => null ()
       end select
    end if
    call cmd%compile_options (global)
    if (associated (pn_arg)) then
       n_args = parse_node_get_n_sub (pn_arg)
       allocate (cmd%name (n_args))
       pn_var => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_var))
          i = i + 1
          select case (char (parse_node_get_rule_key (pn_var)))
          case ("model", "library", "beams", "iterations", &
                "cuts", "weight", "int", "real", "complex", &
                "scale", "factorization_scale", "renormalization_scale", &
                "selection", "reweight", "analysis", "pdg", &
                "stable", "unstable", "polarized", "unpolarized", &
                "results", "expect", "intrinsic", "string", "logical")
             cmd%name(i) = parse_node_get_key (pn_var)
          case ("result_var")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             if (associated (pn_name)) then
                cmd%name(i) = parse_node_get_key (pn_prefix) &
                     // "(" // parse_node_get_string (pn_name) // ")"
             else
                cmd%name(i) = parse_node_get_key (pn_prefix)
             end if
          case ("log_var", "string_var", "alias_var")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             key = parse_node_get_key (pn_prefix)
             if (associated (pn_name)) then
                select case (char (parse_node_get_rule_key (pn_name)))
                case ("var_name")
                   select case (char (key))
                   case ("?", "$")  ! $ sign
                      cmd%name(i) = key // parse_node_get_string (pn_name)
                   case ("alias")
                      cmd%name(i) = parse_node_get_string (pn_name)
                   end select
                case default
                   call parse_node_mismatch &
                        ("var_name",  pn_name)
                end select
             else
                cmd%name(i) = key
             end if
          case default
             cmd%name(i) = parse_node_get_string (pn_var)
          end select
          pn_var => parse_node_get_next_ptr (pn_var)
       end do
    else
       allocate (cmd%name (0))
    end if
  end subroutine cmd_show_compile

  subroutine cmd_show_execute (cmd, global)
    class(cmd_show_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list, model_vars
    type(model_t), pointer :: model
    type(string_t) :: name
    integer :: n, pdg
    type(flavor_t) :: flv
    type(process_library_t), pointer :: prc_lib
    type(process_t), pointer :: process
    logical :: pacified
    character(SHOW_BUFFER_SIZE) :: buffer
    integer :: i, j, u, u_log, u_out
    u = free_unit ()
    var_list => cmd%local%var_list
    if (associated (cmd%local%model)) then
       model_vars => cmd%local%model%get_var_list_ptr ()
    else
       model_vars => null ()
    end if
    pacified = var_list%get_lval (var_str ("?pacify"))
    open (u, status = "scratch", action = "readwrite")
    if (associated (cmd%local%model)) then
       name = cmd%local%model%get_name ()
    end if
    if (size (cmd%name) == 0) then
       if (associated (model_vars)) then
          call var_list_write (model_vars, model_name = name, &
               unit = u, pacified = pacified, follow_link = .false.)
       end if
       call var_list_write (var_list, unit = u, pacified = pacified)
    else
       do i = 1, size (cmd%name)
          select case (char (cmd%name(i)))
          case ("model")
             if (associated (cmd%local%model)) then
                call cmd%local%model%show (u)
             else
                write (u, "(A)")  "Model: [undefined]"
             end if
          case ("library")
             if (associated (cmd%local%prclib)) then
                call cmd%local%prclib%show (u)
             else
                write (u, "(A)")  "Process library: [undefined]"
             end if
          case ("beams")
             call cmd%local%show_beams (u)
          case ("iterations")
             call cmd%local%it_list%write (u)
          case ("results")
             call cmd%local%process_stack%show (u)
          case ("stable")
             call cmd%local%model%show_stable (u)
          case ("polarized")
             call cmd%local%model%show_polarized (u)
          case ("unpolarized")
             call cmd%local%model%show_unpolarized (u)
          case ("unstable")
             model => cmd%local%model
             call model%show_unstable (u)
             n = model%get_n_field ()
             do j = 1, n
                pdg = model%get_pdg (j)
                call flv%init (pdg, model)
                if (.not. flv%is_stable ()) &
                     call show_unstable (cmd%local, pdg, u)
                if (flv%has_antiparticle ()) then
                   associate (anti => flv%anti ())
                     if (.not. anti%is_stable ()) &
                          call show_unstable (cmd%local, -pdg, u)
                   end associate
                end if
             end do
          case ("cuts", "weight", "scale", &
               "factorization_scale", "renormalization_scale", &
               "selection", "reweight", "analysis")
             call cmd%local%pn%show (cmd%name(i), u)
          case ("expect")
             call expect_summary (force = .true.)
          case ("intrinsic")
             call var_list_write (var_list, &
                  intrinsic=.true., unit=u, pacified = pacified)
          case ("logical")
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_LOG, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, &
                  only_type=V_LOG, unit=u, pacified = pacified)
          case ("int")
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_INT, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, only_type=V_INT, &
                  unit=u, pacified = pacified)
          case ("real")
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_REAL, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, only_type=V_REAL, &
                  unit=u, pacified = pacified)
          case ("complex")
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_CMPLX, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, only_type=V_CMPLX, &
                  unit=u, pacified = pacified)
          case ("pdg")
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_PDG, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, only_type=V_PDG, &
                  unit=u, pacified = pacified)
          case ("string") 
             if (associated (model_vars)) then
                call var_list_write (model_vars, only_type=V_STR, &
                     model_name = name, unit=u, pacified = pacified, &
                     follow_link=.false.)
             end if
             call var_list_write (var_list, only_type=V_STR, &
                  unit=u, pacified = pacified)
          case default
             if (analysis_exists (cmd%name(i))) then
                call analysis_write (cmd%name(i), u)
             else if (cmd%local%process_stack%exists (cmd%name(i))) then
                process => cmd%local%process_stack%get_process_ptr (cmd%name(i))
                call process%show (u)
             else if (associated (cmd%local%prclib_stack%get_library_ptr &
                  (cmd%name(i)))) then
                prc_lib => cmd%local%prclib_stack%get_library_ptr (cmd%name(i))
                call prc_lib%show (u)
             else if (associated (model_vars)) then
                if (model_vars%contains (cmd%name(i), follow_link=.false.)) then
                   call var_list_write_var (model_vars, cmd%name(i), &
                        unit = u, model_name = name, pacified = pacified)
                else if (var_list%contains (cmd%name(i))) then
                   call var_list_write_var (var_list, cmd%name(i), &
                        unit = u, pacified = pacified)
                else
                   call msg_error ("show: object '" // char (cmd%name(i)) &
                        // "' not found")
                end if
             else if (var_list%contains (cmd%name(i))) then
                call var_list_write_var (var_list, cmd%name(i), &
                     unit = u, pacified = pacified)
             else
                call msg_error ("show: object '" // char (cmd%name(i)) &
                     // "' not found")
             end if
          end select
       end do
    end if
    rewind (u)
    u_log = logfile_unit ()
    u_out = given_output_unit ()
    do
       read (u, "(A)", end = 1)  buffer
       if (u_log > 0)  write (u_log, "(A)")  trim (buffer)
       if (u_out > 0)  write (u_out, "(A)")  trim (buffer)
    end do
1   close (u)
    if (u_log > 0)  flush (u_log)
    if (u_out > 0)  flush (u_out)
  end subroutine cmd_show_execute

  subroutine cmd_clear_write (cmd, unit, indent)
    class(cmd_clear_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)  
    write (u, "(1x,A)", advance="no")  "clear: "  
    if (allocated (cmd%name)) then
       do i = 1, size (cmd%name)
          write (u, "(1x,A)", advance="no")  char (cmd%name(i))
       end do
       write (u, *)
    else
       write (u, "(5x,A)")  "[undefined]"
    end if
  end subroutine cmd_clear_write

  subroutine cmd_clear_compile (cmd, global)
    class(cmd_clear_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_var, pn_prefix, pn_name
    type(string_t) :: key
    integer :: i, n_args
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    if (associated (pn_arg)) then
       select case (char (parse_node_get_rule_key (pn_arg)))
       case ("clear_arg")
          cmd%pn_opt => parse_node_get_next_ptr (pn_arg)
       case default
          cmd%pn_opt => pn_arg
          pn_arg => null ()
       end select
    end if
    call cmd%compile_options (global)
    if (associated (pn_arg)) then
       n_args = parse_node_get_n_sub (pn_arg)
       allocate (cmd%name (n_args))
       pn_var => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_var))
          i = i + 1
          select case (char (parse_node_get_rule_key (pn_var)))
          case ("beams", "iterations", &
                "cuts", "weight", &
                "scale", "factorization_scale", "renormalization_scale", &
                "selection", "reweight", "analysis", &
                "unstable", "polarized", &
                "expect")
             cmd%name(i) = parse_node_get_key (pn_var)
          case ("log_var", "string_var")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             key = parse_node_get_key (pn_prefix)
             if (associated (pn_name)) then
                select case (char (parse_node_get_rule_key (pn_name)))
                case ("var_name")
                   select case (char (key))
                   case ("?", "$")  ! $ sign
                      cmd%name(i) = key // parse_node_get_string (pn_name)
                   end select
                case default
                   call parse_node_mismatch &
                        ("var_name",  pn_name)
                end select
             else
                cmd%name(i) = key
             end if
          case default
             cmd%name(i) = parse_node_get_string (pn_var)
          end select
          pn_var => parse_node_get_next_ptr (pn_var)
       end do
    else
       allocate (cmd%name (0))
    end if
  end subroutine cmd_clear_compile

  subroutine cmd_clear_execute (cmd, global)
    class(cmd_clear_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    integer :: i
    logical :: success
    type(var_list_t), pointer :: model_vars
    if (size (cmd%name) == 0) then
       call msg_warning ("clear: no object specified")
    else
       do i = 1, size (cmd%name)
          success = .true.
          select case (char (cmd%name(i)))
          case ("beams")
             call cmd%local%clear_beams ()
          case ("iterations")
             call cmd%local%it_list%clear ()
          case ("polarized")
             call cmd%local%model%clear_polarized ()
          case ("unstable")
             call cmd%local%model%clear_unstable ()
          case ("cuts", "weight", "scale", &
               "factorization_scale", "renormalization_scale", &
               "selection", "reweight", "analysis")
             call cmd%local%pn%clear (cmd%name(i))
          case ("expect")
             call expect_clear ()
          case default
             if (analysis_exists (cmd%name(i))) then
                call analysis_clear (cmd%name(i))
             else if (cmd%local%var_list%contains (cmd%name(i))) then
                if (.not. cmd%local%var_list%is_locked (cmd%name(i))) then
                   call cmd%local%var_list%unset (cmd%name(i))
                else
                   call msg_error ("clear: variable '" // char (cmd%name(i)) &
                        // "' is locked and can't be cleared")
                   success = .false.
                end if
             else if (associated (cmd%local%model)) then
                model_vars => cmd%local%model%get_var_list_ptr ()
                if (model_vars%contains (cmd%name(i), follow_link=.false.)) then
                   call msg_error ("clear: variable '" // char (cmd%name(i)) &
                        // "' is a model variable and can't be cleared")
                else
                   call msg_error ("clear: object '" // char (cmd%name(i)) &
                        // "' not found")
                end if
                success = .false.
             else
                call msg_error ("clear: object '" // char (cmd%name(i)) &
                     // "' not found")
                success = .false.
             end if
          end select
          if (success)  call msg_message ("cleared: " // char (cmd%name(i)))
       end do
    end if
  end subroutine cmd_clear_execute

  subroutine cmd_expect_write (cmd, unit, indent)
    class(cmd_expect_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    if (associated (cmd%pn_lexpr)) then
       write (u, "(1x,A)")  "expect: [expression associated]"
    else
       write (u, "(1x,A)")  "expect: [undefined]"       
    end if
  end subroutine cmd_expect_write

  subroutine cmd_expect_compile (cmd, global)
    class(cmd_expect_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_arg)
    cmd%pn_lexpr => parse_node_get_sub_ptr (pn_arg)
    call cmd%compile_options (global)
  end subroutine cmd_expect_compile

  subroutine cmd_expect_execute (cmd, global)
    class(cmd_expect_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    logical :: success, is_known
    var_list => cmd%local%get_var_list_ptr ()
    success = eval_log (cmd%pn_lexpr, var_list, is_known=is_known)
    if (is_known) then
       if (success) then
          call msg_message ("expect: success")
       else
          call msg_error ("expect: failure")
       end if
    else
       call msg_error ("expect: undefined result")
       success = .false.
    end if
    call expect_record (success)
  end subroutine cmd_expect_execute

  subroutine cmd_beams_write (cmd, unit, indent)
    class(cmd_beams_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams: [undefined]"
    end select
    if (allocated (cmd%n_entry)) then
       if (cmd%n_sf_record > 0) then
          write (u, "(1x,A,99(1x,I0))")  "structure function entries:", &
               cmd%n_entry
       end if
    end if
  end subroutine cmd_beams_write

  subroutine cmd_beams_compile (cmd, global)
    class(cmd_beams_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_beam_def, pn_beam_spec
    type(parse_node_t), pointer :: pn_beam_list
    type(parse_node_t), pointer :: pn_codes
    type(parse_node_t), pointer :: pn_strfun_seq, pn_strfun_pair
    type(parse_node_t), pointer :: pn_strfun_def
    integer :: i
    pn_beam_def => parse_node_get_sub_ptr (cmd%pn, 3)
    pn_beam_spec => parse_node_get_sub_ptr (pn_beam_def)
    pn_strfun_seq => parse_node_get_next_ptr (pn_beam_spec)
    pn_beam_list => parse_node_get_sub_ptr (pn_beam_spec)
    call cmd%compile_options (global)
    cmd%n_in = parse_node_get_n_sub (pn_beam_list)
    allocate (cmd%pn_pdg (cmd%n_in))
    pn_codes => parse_node_get_sub_ptr (pn_beam_list)
    do i = 1, cmd%n_in
       cmd%pn_pdg(i)%ptr => pn_codes
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
    if (associated (pn_strfun_seq)) then
       cmd%n_sf_record = parse_node_get_n_sub (pn_beam_def) - 1
       allocate (cmd%n_entry (cmd%n_sf_record), source = 1)
       allocate (cmd%pn_sf_entry (2, cmd%n_sf_record))
       do i = 1, cmd%n_sf_record
          pn_strfun_pair => parse_node_get_sub_ptr (pn_strfun_seq, 2)
          pn_strfun_def => parse_node_get_sub_ptr (pn_strfun_pair)
          cmd%pn_sf_entry(1,i)%ptr => pn_strfun_def
          pn_strfun_def => parse_node_get_next_ptr (pn_strfun_def)
          cmd%pn_sf_entry(2,i)%ptr => pn_strfun_def
          if (associated (pn_strfun_def))  cmd%n_entry(i) = 2
          pn_strfun_seq => parse_node_get_next_ptr (pn_strfun_seq)
       end do
    else
       allocate (cmd%n_entry (0))
       allocate (cmd%pn_sf_entry (0, 0))
    end if
  end subroutine cmd_beams_compile

  subroutine cmd_beams_execute (cmd, global)
    class(cmd_beams_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(pdg_array_t) :: pdg_array
    integer, dimension(:), allocatable :: pdg
    type(flavor_t), dimension(:), allocatable :: flv
    type(parse_node_t), pointer :: pn_key
    type(string_t) :: sf_name
    integer :: i, j
    call lhapdf_global_reset ()
    var_list => cmd%local%get_var_list_ptr ()
    allocate (flv (cmd%n_in))
    do i = 1, cmd%n_in
       pdg_array = eval_pdg_array (cmd%pn_pdg(i)%ptr, var_list)
       pdg = pdg_array
       select case (size (pdg))
       case (1)
          call flv(i)%init ( pdg(1), cmd%local%model)
       case default
          call msg_fatal ("Beams: beam particles must be unique")
       end select
    end do
    select case (cmd%n_in)
    case (1)
       if (cmd%n_sf_record > 0) then
          call msg_fatal ("Beam setup: no structure functions allowed &
               &for decay")
       end if
       call global%beam_structure%init_sf (flv%get_name ())
    case (2)
       call global%beam_structure%init_sf (flv%get_name (), cmd%n_entry)
       do i = 1, cmd%n_sf_record
          do j = 1, cmd%n_entry(i)
             pn_key => parse_node_get_sub_ptr (cmd%pn_sf_entry(j,i)%ptr)
             sf_name = parse_node_get_key (pn_key)
             call global%beam_structure%set_sf (i, j, sf_name)
          end do
       end do
    end select
  end subroutine cmd_beams_execute

  subroutine sentry_expr_compile (sentry, pn)
    class(sentry_expr_t), intent(out) :: sentry
    type(parse_node_t), intent(in), target :: pn
    type(parse_node_t), pointer :: pn_expr, pn_extra
    integer :: n_expr, i
    n_expr = parse_node_get_n_sub (pn)
    allocate (sentry%expr (n_expr))
    if (n_expr > 0) then
       i = 0
       pn_expr => parse_node_get_sub_ptr (pn)
       pn_extra => parse_node_get_next_ptr (pn_expr)
       do i = 1, n_expr
          sentry%expr(i)%ptr => pn_expr
          if (associated (pn_extra)) then
             pn_expr => parse_node_get_sub_ptr (pn_extra, 2)
             pn_extra => parse_node_get_next_ptr (pn_extra)
          end if
       end do
    end if
  end subroutine sentry_expr_compile
    
  subroutine sentry_expr_evaluate (sentry, index, value, global)
    class(sentry_expr_t), intent(inout) :: sentry
    integer, dimension(:), intent(out) :: index
    complex(default), intent(out) :: value
    type(rt_data_t), intent(in), target :: global
    type(var_list_t), pointer :: var_list
    integer :: i, n_expr, n_index
    type(eval_tree_t) :: eval_tree
    var_list => global%get_var_list_ptr ()
    n_expr = size (sentry%expr) 
    n_index = size (index)
    if (n_expr <= n_index + 1) then
       do i = 1, min (n_expr, n_index)
          associate (expr => sentry%expr(i))
            call eval_tree%init_expr (expr%ptr, var_list)
            call eval_tree%evaluate ()
            if (eval_tree%is_known ()) then
               index(i) = eval_tree%get_int ()
            else
               call msg_fatal ("Evaluating density matrix: undefined index")
            end if
          end associate
       end do
       do i = n_expr + 1, n_index
          index(i) = index(n_expr)
       end do
       if (n_expr == n_index + 1) then
          associate (expr => sentry%expr(n_expr))
            call eval_tree%init_expr (expr%ptr, var_list)
            call eval_tree%evaluate ()
            if (eval_tree%is_known ()) then
               value = eval_tree%get_cmplx ()
            else
               call msg_fatal ("Evaluating density matrix: undefined index")
            end if
            call eval_tree%final ()
          end associate
       else
          value = 1
       end if
    else
       call msg_fatal ("Evaluating density matrix: index expression too long")
    end if
  end subroutine sentry_expr_evaluate

  subroutine smatrix_expr_compile (smatrix_expr, pn)
    class(smatrix_expr_t), intent(out) :: smatrix_expr
    type(parse_node_t), intent(in), target :: pn
    type(parse_node_t), pointer :: pn_arg, pn_entry
    integer :: n_entry, i
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_arg)) then
       n_entry = parse_node_get_n_sub (pn_arg)
       allocate (smatrix_expr%entry (n_entry))
       pn_entry => parse_node_get_sub_ptr (pn_arg)
       do i = 1, n_entry
          call smatrix_expr%entry(i)%compile (pn_entry)
          pn_entry => parse_node_get_next_ptr (pn_entry)
       end do
    else
       allocate (smatrix_expr%entry (0))
    end if
  end subroutine smatrix_expr_compile

  subroutine smatrix_expr_evaluate (smatrix_expr, smatrix, global)
    class(smatrix_expr_t), intent(inout) :: smatrix_expr
    type(smatrix_t), intent(out) :: smatrix
    type(rt_data_t), intent(in), target :: global
    integer, dimension(2) :: idx
    complex(default) :: value
    integer :: i, n_entry
    n_entry = size (smatrix_expr%entry)
    call smatrix%init (2, n_entry)
    do i = 1, n_entry
       call smatrix_expr%entry(i)%evaluate (idx, value, global)
       call smatrix%set_entry (i, idx, value)
    end do
  end subroutine smatrix_expr_evaluate
    
  subroutine cmd_beams_pol_density_write (cmd, unit, indent)
    class(cmd_beams_pol_density_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams polarization setup: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams polarization setup: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams polarization setup: [undefined]"
    end select
  end subroutine cmd_beams_pol_density_write

  subroutine cmd_beams_pol_density_compile (cmd, global)
    class(cmd_beams_pol_density_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_pol_spec, pn_smatrix
    integer :: i
    pn_pol_spec => parse_node_get_sub_ptr (cmd%pn, 3)
    call cmd%compile_options (global)
    cmd%n_in = parse_node_get_n_sub (pn_pol_spec)
    allocate (cmd%smatrix (cmd%n_in))
    pn_smatrix => parse_node_get_sub_ptr (pn_pol_spec)
    do i = 1, cmd%n_in
       call cmd%smatrix(i)%compile (pn_smatrix)
       pn_smatrix => parse_node_get_next_ptr (pn_smatrix)
    end do
  end subroutine cmd_beams_pol_density_compile

  subroutine cmd_beams_pol_density_execute (cmd, global)
    class(cmd_beams_pol_density_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(smatrix_t) :: smatrix
    integer :: i
    call global%beam_structure%init_pol (cmd%n_in)
    do i = 1, cmd%n_in
       call cmd%smatrix(i)%evaluate (smatrix, global)
       call global%beam_structure%set_smatrix (i, smatrix)
    end do
  end subroutine cmd_beams_pol_density_execute

  subroutine cmd_beams_pol_fraction_write (cmd, unit, indent)
    class(cmd_beams_pol_fraction_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams polarization fraction: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams polarization fraction: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams polarization fraction: [undefined]"
    end select
  end subroutine cmd_beams_pol_fraction_write

  subroutine cmd_beams_pol_fraction_compile (cmd, global)
    class(cmd_beams_pol_fraction_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_frac_spec, pn_expr
    integer :: i
    pn_frac_spec => parse_node_get_sub_ptr (cmd%pn, 3)
    call cmd%compile_options (global)
    cmd%n_in = parse_node_get_n_sub (pn_frac_spec)
    allocate (cmd%expr (cmd%n_in))
    pn_expr => parse_node_get_sub_ptr (pn_frac_spec)
    do i = 1, cmd%n_in
       cmd%expr(i)%ptr => pn_expr
       pn_expr => parse_node_get_next_ptr (pn_expr)
    end do
  end subroutine cmd_beams_pol_fraction_compile

  subroutine cmd_beams_pol_fraction_execute (cmd, global)
    class(cmd_beams_pol_fraction_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default), dimension(:), allocatable :: pol_f
    type(eval_tree_t) :: expr
    integer :: i
    var_list => global%get_var_list_ptr ()
    allocate (pol_f (cmd%n_in))
    do i = 1, cmd%n_in
       call expr%init_expr (cmd%expr(i)%ptr, var_list)
       call expr%evaluate ()
       if (expr%is_known ()) then
          pol_f(i) = expr%get_real ()
       else
          call msg_fatal ("beams polarization fraction: undefined value")
       end if
       call expr%final ()
    end do
    call global%beam_structure%set_pol_f (pol_f)
  end subroutine cmd_beams_pol_fraction_execute

  subroutine cmd_beams_momentum_write (cmd, unit, indent)
    class(cmd_beams_momentum_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams momentum: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams momentum: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams momentum: [undefined]"
    end select
  end subroutine cmd_beams_momentum_write

  subroutine cmd_beams_momentum_execute (cmd, global)
    class(cmd_beams_momentum_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default), dimension(:), allocatable :: p
    type(eval_tree_t) :: expr
    integer :: i
    var_list => global%get_var_list_ptr ()
    allocate (p (cmd%n_in))
    do i = 1, cmd%n_in
       call expr%init_expr (cmd%expr(i)%ptr, var_list)
       call expr%evaluate ()
       if (expr%is_known ()) then
          p(i) = expr%get_real ()
       else
          call msg_fatal ("beams momentum: undefined value")
       end if
       call expr%final ()
    end do
    call global%beam_structure%set_momentum (p)
  end subroutine cmd_beams_momentum_execute

  subroutine cmd_beams_theta_write (cmd, unit, indent)
    class(cmd_beams_theta_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams theta: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams theta: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams theta: [undefined]"
    end select
  end subroutine cmd_beams_theta_write

  subroutine cmd_beams_phi_write (cmd, unit, indent)
    class(cmd_beams_phi_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_in)
    case (1)
       write (u, "(1x,A)")  "beams phi: 1 [decay]"
    case (2)
       write (u, "(1x,A)")  "beams phi: 2 [scattering]"
    case default
       write (u, "(1x,A)")  "beams phi: [undefined]"
    end select
  end subroutine cmd_beams_phi_write

  subroutine cmd_beams_theta_execute (cmd, global)
    class(cmd_beams_theta_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default), dimension(:), allocatable :: theta
    type(eval_tree_t) :: expr
    integer :: i
    var_list => global%get_var_list_ptr ()
    allocate (theta (cmd%n_in))
    do i = 1, cmd%n_in
       call expr%init_expr (cmd%expr(i)%ptr, var_list)
       call expr%evaluate ()
       if (expr%is_known ()) then
          theta(i) = expr%get_real ()
       else
          call msg_fatal ("beams theta: undefined value")
       end if
       call expr%final ()
    end do
    call global%beam_structure%set_theta (theta)
  end subroutine cmd_beams_theta_execute

  subroutine cmd_beams_phi_execute (cmd, global)
    class(cmd_beams_phi_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default), dimension(:), allocatable :: phi
    type(eval_tree_t) :: expr
    integer :: i
    var_list => global%get_var_list_ptr ()
    allocate (phi (cmd%n_in))
    do i = 1, cmd%n_in
       call expr%init_expr (cmd%expr(i)%ptr, var_list)
       call expr%evaluate ()
       if (expr%is_known ()) then
          phi(i) = expr%get_real ()
       else
          call msg_fatal ("beams phi: undefined value")
       end if
       call expr%final ()
    end do
    call global%beam_structure%set_phi (phi)
  end subroutine cmd_beams_phi_execute

  subroutine cmd_cuts_write (cmd, unit, indent)
    class(cmd_cuts_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "cuts: [defined]"
  end subroutine cmd_cuts_write
  
  subroutine cmd_cuts_compile (cmd, global)
    class(cmd_cuts_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_lexpr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_cuts_compile

  subroutine cmd_cuts_execute (cmd, global)
    class(cmd_cuts_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%cuts_lexpr => cmd%pn_lexpr
  end subroutine cmd_cuts_execute

  subroutine cmd_scale_write (cmd, unit, indent)
    class(cmd_scale_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "scale: [defined]"
  end subroutine cmd_scale_write
  
  subroutine cmd_fac_scale_write (cmd, unit, indent)
    class(cmd_fac_scale_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "factorization scale: [defined]"
  end subroutine cmd_fac_scale_write
  
  subroutine cmd_ren_scale_write (cmd, unit, indent)
    class(cmd_ren_scale_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "renormalization scale: [defined]"
  end subroutine cmd_ren_scale_write
  
  subroutine cmd_scale_compile (cmd, global)
    class(cmd_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_scale_compile

  subroutine cmd_fac_scale_compile (cmd, global)
    class(cmd_fac_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_fac_scale_compile

  subroutine cmd_ren_scale_compile (cmd, global)
    class(cmd_ren_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_ren_scale_compile

  subroutine cmd_scale_execute (cmd, global)
    class(cmd_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%scale_expr => cmd%pn_expr
  end subroutine cmd_scale_execute

  subroutine cmd_fac_scale_execute (cmd, global)
    class(cmd_fac_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%fac_scale_expr => cmd%pn_expr
  end subroutine cmd_fac_scale_execute

  subroutine cmd_ren_scale_execute (cmd, global)
    class(cmd_ren_scale_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%ren_scale_expr => cmd%pn_expr    
  end subroutine cmd_ren_scale_execute

  subroutine cmd_weight_write (cmd, unit, indent)
    class(cmd_weight_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "weight expression: [defined]"
  end subroutine cmd_weight_write
  
  subroutine cmd_weight_compile (cmd, global)
    class(cmd_weight_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_weight_compile

  subroutine cmd_weight_execute (cmd, global)
    class(cmd_weight_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%weight_expr => cmd%pn_expr
  end subroutine cmd_weight_execute

  subroutine cmd_selection_write (cmd, unit, indent)
    class(cmd_selection_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "selection expression: [defined]"
  end subroutine cmd_selection_write
  
  subroutine cmd_selection_compile (cmd, global)
    class(cmd_selection_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_selection_compile

  subroutine cmd_selection_execute (cmd, global)
    class(cmd_selection_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%selection_lexpr => cmd%pn_expr
  end subroutine cmd_selection_execute

  subroutine cmd_reweight_write (cmd, unit, indent)
    class(cmd_reweight_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "reweight expression: [defined]"
  end subroutine cmd_reweight_write
  
  subroutine cmd_reweight_compile (cmd, global)
    class(cmd_reweight_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_expr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_reweight_compile

  subroutine cmd_reweight_execute (cmd, global)
    class(cmd_reweight_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%reweight_expr => cmd%pn_expr
  end subroutine cmd_reweight_execute

  subroutine cmd_alt_setup_write (cmd, unit, indent)
    class(cmd_alt_setup_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,I0,A)")  "alt_setup: ", size (cmd%setup), " entries"
  end subroutine cmd_alt_setup_write
  
  subroutine cmd_alt_setup_compile (cmd, global)
    class(cmd_alt_setup_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_list, pn_setup
    integer :: i
    pn_list => parse_node_get_sub_ptr (cmd%pn, 3)
    if (associated (pn_list)) then
       allocate (cmd%setup (parse_node_get_n_sub (pn_list)))
       i = 1
       pn_setup => parse_node_get_sub_ptr (pn_list)
       do while (associated (pn_setup))
          cmd%setup(i)%ptr => pn_setup
          i = i + 1
          pn_setup => parse_node_get_next_ptr (pn_setup)
       end do
    else
       allocate (cmd%setup (0))
    end if
  end subroutine cmd_alt_setup_compile

  subroutine cmd_alt_setup_execute (cmd, global)
    class(cmd_alt_setup_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (allocated (global%pn%alt_setup))  deallocate (global%pn%alt_setup)
    allocate (global%pn%alt_setup (size (cmd%setup)))
    global%pn%alt_setup = cmd%setup
  end subroutine cmd_alt_setup_execute

  subroutine cmd_integrate_write (cmd, unit, indent)
    class(cmd_integrate_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "integrate ("
    do i = 1, cmd%n_proc
       if (i > 1)  write (u, "(A,1x)", advance="no")  ","
       write (u, "(A)", advance="no")  char (cmd%process_id(i))
    end do
    write (u, "(A)")  ")"
  end subroutine cmd_integrate_write

  subroutine cmd_integrate_compile (cmd, global)
    class(cmd_integrate_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_proclist, pn_proc
    integer :: i
    pn_proclist => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_proclist)
    call cmd%compile_options (global)
    cmd%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (cmd%process_id (cmd%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, cmd%n_proc
       cmd%process_id(i) = parse_node_get_string (pn_proc)
       call global%process_stack%init_result_vars (cmd%process_id(i))
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
  end subroutine cmd_integrate_compile

  subroutine cmd_integrate_execute (cmd, global)
    class(cmd_integrate_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    integer :: i
    call msg_debug (D_CORE, "cmd_integrate_execute")
    do i = 1, cmd%n_proc
       call msg_debug (D_CORE, "cmd%process_id(i) ", cmd%process_id(i))
       call integrate_process (cmd%process_id(i), cmd%local, global)
       call global%process_stack%fill_result_vars (cmd%process_id(i))
       if (signal_is_pending ())  return
    end do
  end subroutine cmd_integrate_execute

  subroutine cmd_observable_write (cmd, unit, indent)
    class(cmd_observable_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A)")  "observable: ", char (cmd%id)
  end subroutine cmd_observable_write
  
  subroutine cmd_observable_compile (cmd, global)
    class(cmd_observable_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_tag
    pn_tag => parse_node_get_sub_ptr (cmd%pn, 2)
    if (associated (pn_tag)) then
       cmd%pn_opt => parse_node_get_next_ptr (pn_tag)
    end if       
    call cmd%compile_options (global)
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       cmd%id = parse_node_get_string (pn_tag)
    case default
       call msg_bug ("observable: name expression not implemented (yet)")
    end select
  end subroutine cmd_observable_compile

  subroutine cmd_observable_execute (cmd, global)
    class(cmd_observable_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(graph_options_t) :: graph_options
    type(string_t) :: label, unit
    var_list => cmd%local%get_var_list_ptr ()
    label = var_list%get_sval (var_str ("$obs_label"))
    unit = var_list%get_sval (var_str ("$obs_unit"))
    call graph_options_init (graph_options)
    call set_graph_options (graph_options, var_list)
    call analysis_init_observable (cmd%id, label, unit, graph_options)
  end subroutine cmd_observable_execute

  subroutine cmd_histogram_write (cmd, unit, indent)
    class(cmd_histogram_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A)")  "histogram: ", char (cmd%id)
  end subroutine cmd_histogram_write
  
  subroutine cmd_histogram_compile (cmd, global)
    class(cmd_histogram_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_tag, pn_args, pn_arg1, pn_arg2, pn_arg3
    character(*), parameter :: e_illegal_use = &
       "illegal usage of 'histogram': insufficient number of arguments"
    pn_tag => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_args => parse_node_get_next_ptr (pn_tag)
    if (associated (pn_args)) then
       pn_arg1 => parse_node_get_sub_ptr (pn_args)
       if (.not. associated (pn_arg1)) call msg_fatal (e_illegal_use)
       pn_arg2 => parse_node_get_next_ptr (pn_arg1)
       if (.not. associated (pn_arg2)) call msg_fatal (e_illegal_use)
       pn_arg3 => parse_node_get_next_ptr (pn_arg2)
       cmd%pn_opt => parse_node_get_next_ptr (pn_args)
    end if       
    call cmd%compile_options (global)
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       cmd%id = parse_node_get_string (pn_tag)
    case default
       call msg_bug ("histogram: name expression not implemented (yet)")
    end select
    cmd%pn_lower_bound => pn_arg1
    cmd%pn_upper_bound => pn_arg2
    cmd%pn_bin_width => pn_arg3
  end subroutine cmd_histogram_compile

  subroutine cmd_histogram_execute (cmd, global)
    class(cmd_histogram_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    real(default) :: lower_bound, upper_bound, bin_width
    integer :: bin_number
    logical :: bin_width_is_used, normalize_bins
    type(string_t) :: obs_label, obs_unit
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options

    var_list => cmd%local%get_var_list_ptr ()
    lower_bound = eval_real (cmd%pn_lower_bound, var_list)
    upper_bound = eval_real (cmd%pn_upper_bound, var_list)
    if (associated (cmd%pn_bin_width)) then
       bin_width = eval_real (cmd%pn_bin_width, var_list)
       bin_width_is_used = .true.
    else if (var_list%is_known (var_str ("n_bins"))) then
       bin_number = &
            var_list%get_ival (var_str ("n_bins"))
       bin_width_is_used = .false.
    else
       call msg_error ("Cmd '" // char (cmd%id) // &
            "': neither bin width nor number is defined")
    end if
    normalize_bins = &
         var_list%get_lval (var_str ("?normalize_bins"))
    obs_label = &
         var_list%get_sval (var_str ("$obs_label"))
    obs_unit = &
         var_list%get_sval (var_str ("$obs_unit"))

    call graph_options_init (graph_options)
    call set_graph_options (graph_options, var_list)
    call drawing_options_init_histogram (drawing_options)
    call set_drawing_options (drawing_options, var_list)

    if (bin_width_is_used) then
       call analysis_init_histogram &
            (cmd%id, lower_bound, upper_bound, bin_width, &
             normalize_bins, &
             obs_label, obs_unit, &
             graph_options, drawing_options)
    else
       call analysis_init_histogram &
            (cmd%id, lower_bound, upper_bound, bin_number, &
             normalize_bins, &
             obs_label, obs_unit, &
             graph_options, drawing_options)
    end if
  end subroutine cmd_histogram_execute

  subroutine set_graph_options (gro, var_list)
    type(graph_options_t), intent(inout) :: gro
    type(var_list_t), intent(in) :: var_list
    call graph_options_set (gro, title = &
         var_list%get_sval (var_str ("$title")))
    call graph_options_set (gro, description = &
         var_list%get_sval (var_str ("$description")))
    call graph_options_set (gro, x_label = &
         var_list%get_sval (var_str ("$x_label")))
    call graph_options_set (gro, y_label = &
         var_list%get_sval (var_str ("$y_label")))
    call graph_options_set (gro, width_mm = &
         var_list%get_ival (var_str ("graph_width_mm")))
    call graph_options_set (gro, height_mm = &
         var_list%get_ival (var_str ("graph_height_mm")))
    call graph_options_set (gro, x_log = &
         var_list%get_lval (var_str ("?x_log")))
    call graph_options_set (gro, y_log = &
         var_list%get_lval (var_str ("?y_log")))
    if (var_list%is_known (var_str ("x_min"))) &
         call graph_options_set (gro, x_min = &
         var_list%get_rval (var_str ("x_min")))
    if (var_list%is_known (var_str ("x_max"))) &
         call graph_options_set (gro, x_max = &
         var_list%get_rval (var_str ("x_max")))
    if (var_list%is_known (var_str ("y_min"))) &
         call graph_options_set (gro, y_min = &
         var_list%get_rval (var_str ("y_min")))
    if (var_list%is_known (var_str ("y_max"))) &
         call graph_options_set (gro, y_max = &
         var_list%get_rval (var_str ("y_max")))
    call graph_options_set (gro, gmlcode_bg = &
         var_list%get_sval (var_str ("$gmlcode_bg")))
    call graph_options_set (gro, gmlcode_fg = &
         var_list%get_sval (var_str ("$gmlcode_fg")))
  end subroutine set_graph_options

  subroutine set_drawing_options (dro, var_list)
    type(drawing_options_t), intent(inout) :: dro
    type(var_list_t), intent(in) :: var_list
    if (var_list%is_known (var_str ("?draw_histogram"))) then
       if (var_list%get_lval (var_str ("?draw_histogram"))) then
          call drawing_options_set (dro, with_hbars = .true.)
       else
          call drawing_options_set (dro, with_hbars = .false., &
               with_base = .false., fill = .false., piecewise = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?draw_base"))) then
       if (var_list%get_lval (var_str ("?draw_base"))) then
          call drawing_options_set (dro, with_base = .true.)
       else
          call drawing_options_set (dro, with_base = .false., fill = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?draw_piecewise"))) then
       if (var_list%get_lval (var_str ("?draw_piecewise"))) then
          call drawing_options_set (dro, piecewise = .true.)
       else
          call drawing_options_set (dro, piecewise = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?fill_curve"))) then
       if (var_list%get_lval (var_str ("?fill_curve"))) then
          call drawing_options_set (dro, fill = .true., with_base = .true.)
       else
          call drawing_options_set (dro, fill = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?draw_curve"))) then
       if (var_list%get_lval (var_str ("?draw_curve"))) then
          call drawing_options_set (dro, draw = .true.)
       else
          call drawing_options_set (dro, draw = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?draw_errors"))) then
       if (var_list%get_lval (var_str ("?draw_errors"))) then
          call drawing_options_set (dro, err = .true.)
       else
          call drawing_options_set (dro, err = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("?draw_symbols"))) then
       if (var_list%get_lval (var_str ("?draw_symbols"))) then
          call drawing_options_set (dro, symbols = .true.)
       else
          call drawing_options_set (dro, symbols = .false.)
       end if
    end if
    if (var_list%is_known (var_str ("$fill_options"))) then
       call drawing_options_set (dro, fill_options = &
            var_list%get_sval (var_str ("$fill_options")))
    end if
    if (var_list%is_known (var_str ("$draw_options"))) then
       call drawing_options_set (dro, draw_options = &
            var_list%get_sval (var_str ("$draw_options")))
    end if
    if (var_list%is_known (var_str ("$err_options"))) then
       call drawing_options_set (dro, err_options = &
            var_list%get_sval (var_str ("$err_options")))
    end if
    if (var_list%is_known (var_str ("$symbol"))) then
       call drawing_options_set (dro, symbol = &
            var_list%get_sval (var_str ("$symbol")))
    end if
    if (var_list%is_known (var_str ("$gmlcode_bg"))) then
       call drawing_options_set (dro, gmlcode_bg = &
            var_list%get_sval (var_str ("$gmlcode_bg")))
    end if
    if (var_list%is_known (var_str ("$gmlcode_fg"))) then
       call drawing_options_set (dro, gmlcode_fg = &
            var_list%get_sval (var_str ("$gmlcode_fg")))
    end if
  end subroutine set_drawing_options

  subroutine cmd_plot_write (cmd, unit, indent)
    class(cmd_plot_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A)")  "plot: ", char (cmd%id)
  end subroutine cmd_plot_write
  
  subroutine cmd_plot_compile (cmd, global)
    class(cmd_plot_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_tag
    pn_tag => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_tag)
    call cmd%init (pn_tag, global)
  end subroutine cmd_plot_compile

  subroutine cmd_plot_init (plot, pn_tag, global)
    class(cmd_plot_t), intent(inout) :: plot
    type(parse_node_t), intent(in), pointer :: pn_tag
    type(rt_data_t), intent(inout), target :: global
    call plot%compile_options (global)
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       plot%id = parse_node_get_string (pn_tag)
    case default
       call msg_bug ("plot: name expression not implemented (yet)")
    end select
  end subroutine cmd_plot_init

  subroutine cmd_plot_execute (cmd, global)
    class(cmd_plot_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options

    var_list => cmd%local%get_var_list_ptr ()
    call graph_options_init (graph_options)
    call set_graph_options (graph_options, var_list)
    call drawing_options_init_plot (drawing_options)
    call set_drawing_options (drawing_options, var_list)

    call analysis_init_plot (cmd%id, graph_options, drawing_options)
  end subroutine cmd_plot_execute

  subroutine cmd_graph_write (cmd, unit, indent)
    class(cmd_graph_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,A,A,I0,A)")  "graph: ", char (cmd%id), &
         " (", cmd%n_elements, " entries)"
  end subroutine cmd_graph_write
  
  subroutine cmd_graph_compile (cmd, global)
    class(cmd_graph_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_term, pn_tag, pn_def, pn_app
    integer :: i

    pn_term => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_tag => parse_node_get_sub_ptr (pn_term)
    cmd%pn_opt => parse_node_get_next_ptr (pn_tag)
    call cmd%compile_options (global)
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       cmd%id = parse_node_get_string (pn_tag)
    case default
       call msg_bug ("graph: name expression not implemented (yet)")
    end select
    pn_def => parse_node_get_next_ptr (pn_term, 2)
    cmd%n_elements = parse_node_get_n_sub (pn_def)
    allocate (cmd%element_id (cmd%n_elements))
    allocate (cmd%el (cmd%n_elements))
    pn_term => parse_node_get_sub_ptr (pn_def)
    pn_tag => parse_node_get_sub_ptr (pn_term)
    cmd%el(1)%pn_opt => parse_node_get_next_ptr (pn_tag)
    call cmd%el(1)%init (pn_tag, global)
    cmd%element_id(1) = parse_node_get_string (pn_tag)
    pn_app => parse_node_get_next_ptr (pn_term)
    do i = 2, cmd%n_elements
       pn_term => parse_node_get_sub_ptr (pn_app, 2)
       pn_tag => parse_node_get_sub_ptr (pn_term)
       cmd%el(i)%pn_opt => parse_node_get_next_ptr (pn_tag)
       call cmd%el(i)%init (pn_tag, global)
       cmd%element_id(i) = parse_node_get_string (pn_tag)
       pn_app => parse_node_get_next_ptr (pn_app)
    end do

  end subroutine cmd_graph_compile

  subroutine cmd_graph_execute (cmd, global)
    class(cmd_graph_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options
    integer :: i, type

    var_list => cmd%local%get_var_list_ptr ()
    call graph_options_init (graph_options)
    call set_graph_options (graph_options, var_list)
    call analysis_init_graph (cmd%id, cmd%n_elements, graph_options)

    do i = 1, cmd%n_elements
       if (associated (cmd%el(i)%options)) then
          call cmd%el(i)%options%execute (cmd%el(i)%local)
       end if
       type = analysis_store_get_object_type (cmd%element_id(i))
       select case (type)
       case (AN_HISTOGRAM)
          call drawing_options_init_histogram (drawing_options)
       case (AN_PLOT)
          call drawing_options_init_plot (drawing_options)
       end select
       call set_drawing_options (drawing_options, var_list)
       if (associated (cmd%el(i)%options)) then
          call set_drawing_options (drawing_options, cmd%el(i)%local%var_list)
       end if
       call analysis_fill_graph (cmd%id, i, cmd%element_id(i), drawing_options)
    end do
  end subroutine cmd_graph_execute

  subroutine cmd_analysis_write (cmd, unit, indent)
    class(cmd_analysis_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "analysis: [defined]"
  end subroutine cmd_analysis_write
  
  subroutine cmd_analysis_compile (cmd, global)
    class(cmd_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%pn_lexpr => parse_node_get_sub_ptr (cmd%pn, 3)
  end subroutine cmd_analysis_compile

  subroutine cmd_analysis_execute (cmd, global)
    class(cmd_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    global%pn%analysis_lexpr => cmd%pn_lexpr
  end subroutine cmd_analysis_execute

  subroutine cmd_write_analysis_write (cmd, unit, indent)
    class(cmd_write_analysis_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "write_analysis"
  end subroutine cmd_write_analysis_write
  
  subroutine cmd_write_analysis_compile (cmd, global)
    class(cmd_write_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_clause, pn_args, pn_id
    integer :: n, i
    pn_clause => parse_node_get_sub_ptr (cmd%pn)
    pn_args => parse_node_get_sub_ptr (pn_clause, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_clause)
    call cmd%compile_options (global)
    if (associated (pn_args)) then
       n = parse_node_get_n_sub (pn_args)
       allocate (cmd%id (n))
       do i = 1, n
           pn_id => parse_node_get_sub_ptr (pn_args, i)
           if (char (parse_node_get_rule_key (pn_id)) == "analysis_id") then
              cmd%id(i)%tag = parse_node_get_string (pn_id)              
           else
              cmd%id(i)%pn_sexpr => pn_id
           end if
       end do
    else
       allocate (cmd%id (0))
    end if
  end subroutine cmd_write_analysis_compile

  subroutine cmd_write_analysis_execute (cmd, global)
    class(cmd_write_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    var_list => cmd%local%get_var_list_ptr ()
    call write_analysis_wrap (var_list, global%out_files, &
         cmd%id, tag = cmd%tag)
  end subroutine cmd_write_analysis_execute

  subroutine write_analysis_wrap (var_list, out_files, id, tag, data_file)
    type(var_list_t), intent(inout), target :: var_list
    type(file_list_t), intent(inout), target :: out_files
    type(analysis_id_t), dimension(:), intent(in), target :: id 
    type(string_t), dimension(:), allocatable, intent(out) :: tag
    type(string_t), intent(out), optional :: data_file
    type(string_t) :: defaultfile, file
    integer :: i   
    logical :: keep_open !, custom, header, columns    
    type(string_t) :: extension !, comment_prefix, separator 
!!! JRR: WK please check (#542)           
!     integer :: type
!     type(ifile_t) :: ifile
    logical :: one_file !, has_writer
!     type(analysis_iterator_t) :: iterator
!     type(rt_data_t), target :: sandbox
!     type(command_list_t) :: writer    
    defaultfile = var_list%get_sval (var_str ("$out_file"))
    if (present (data_file)) then
       if (defaultfile == "" .or. defaultfile == ".") then
          defaultfile = DEFAULT_ANALYSIS_FILENAME
       else
          if (scan (".", defaultfile) > 0) then
             call split (defaultfile, extension, ".", back=.true.)
             if (any (lower_case (char(extension)) == FORBIDDEN_ENDINGS1) .or. &
                 any (lower_case (char(extension)) == FORBIDDEN_ENDINGS2) .or. &
                 any (lower_case (char(extension)) == FORBIDDEN_ENDINGS3)) & 
                 call msg_fatal ("The ending " // char(extension) // &
                 " is internal and not allowed as data file.")
             if (extension /= "") then
                if (defaultfile /= "") then
                   defaultfile = defaultfile // "." // extension
                else
                   defaultfile = "whizard_analysis." // extension
                end if
             else
                defaultfile = defaultfile // ".dat"
             endif
          else
             defaultfile = defaultfile // ".dat"
          end if
       end if
       data_file = defaultfile
    end if
    one_file = defaultfile /= ""
    if (one_file) then
       file = defaultfile
       keep_open = file_list_is_open (out_files, file, &
            action = "write")
       if (keep_open) then
          if (present (data_file)) then
             call msg_fatal ("Compiling analysis: File '" &
                  // char (data_file) &
                   // "' can't be used, it is already open.")
          else
             call msg_message ("Appending analysis data to file '" &
                  // char (file) // "'")
          end if
       else
          call file_list_open (out_files, file, &
               action = "write", status = "replace", position = "asis")
          call msg_message ("Writing analysis data to file '" &
               // char (file) // "'")          
       end if
    end if    

!!! JRR: WK please check. Custom data output. Ticket #542
!     if (present (data_file)) then
!        custom = .false.
!     else
!        custom = var_list%get_lval (&
!            var_str ("?out_custom"))
!     end if
!     comment_prefix = var_list%get_sval (&
!          var_str ("$out_comment"))
!     header = var_list%get_lval (&
!          var_str ("?out_header"))
!     write_yerr = var_list%get_lval (&
!          var_str ("?out_yerr"))
!     write_xerr = var_list%get_lval (&
!          var_str ("?out_xerr"))

    call get_analysis_tags (tag, id, var_list)       
    do i = 1, size (tag)
       call file_list_write_analysis &
            (out_files, file, tag(i))
    end do
    if (one_file .and. .not. keep_open) then
       call file_list_close (out_files, file)
    end if       

  contains
    
    subroutine get_analysis_tags (analysis_tag, id, var_list)
      type(string_t), dimension(:), intent(out), allocatable :: analysis_tag
      type(analysis_id_t), dimension(:), intent(in) :: id
      type(var_list_t), intent(in), target :: var_list
      if (size (id) /= 0) then
         allocate (analysis_tag (size (id)))
         do i = 1, size (id)
            if (associated (id(i)%pn_sexpr)) then
               analysis_tag(i) = eval_string (id(i)%pn_sexpr, var_list)
            else
               analysis_tag(i) = id(i)%tag
            end if
         end do
      else
         call analysis_store_get_ids (tag)                
      end if
    end subroutine get_analysis_tags
    
  end subroutine write_analysis_wrap
  
  subroutine cmd_compile_analysis_write (cmd, unit, indent)
    class(cmd_compile_analysis_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "compile_analysis"
  end subroutine cmd_compile_analysis_write
  
  subroutine cmd_compile_analysis_compile (cmd, global)
    class(cmd_compile_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_clause, pn_args, pn_id
    integer :: n, i
    pn_clause => parse_node_get_sub_ptr (cmd%pn)
    pn_args => parse_node_get_sub_ptr (pn_clause, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_clause)
    call cmd%compile_options (global)
    if (associated (pn_args)) then
       n = parse_node_get_n_sub (pn_args)
       allocate (cmd%id (n))
       do i = 1, n
           pn_id => parse_node_get_sub_ptr (pn_args, i)
           if (char (parse_node_get_rule_key (pn_id)) == "analysis_id") then
              cmd%id(i)%tag = parse_node_get_string (pn_id)
           else
              cmd%id(i)%pn_sexpr => pn_id
           end if
       end do
    else
       allocate (cmd%id (0))       
    end if
  end subroutine cmd_compile_analysis_compile

  subroutine cmd_compile_analysis_execute (cmd, global)
    class(cmd_compile_analysis_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(string_t) :: file, basename, extension, driver_file, &
         makefile
    integer :: u_driver, u_makefile
    logical :: has_gmlcode, only_file
    var_list => cmd%local%get_var_list_ptr ()
    call write_analysis_wrap (var_list, &
         global%out_files, cmd%id, tag = cmd%tag, &
            data_file = file)
    basename = file    
    if (scan (".", basename) > 0) then
      call split (basename, extension, ".", back=.true.)
    else
      extension = ""
    end if
    driver_file = basename // ".tex"
    makefile = basename // "_ana.makefile"
    u_driver = free_unit ()
    open (unit=u_driver, file=char(driver_file), &
          action="write", status="replace")
    if (allocated (cmd%tag)) then
       call analysis_write_driver (file, cmd%tag, unit=u_driver)
       has_gmlcode = analysis_has_plots (cmd%tag)
    else
       call analysis_write_driver (file, unit=u_driver)
       has_gmlcode = analysis_has_plots ()
    end if
    close (u_driver)
    u_makefile = free_unit ()
    open (unit=u_makefile, file=char(makefile), &
         action="write", status="replace")
    call analysis_write_makefile (basename, u_makefile, &
         has_gmlcode, global%os_data)
    close (u_makefile)
    call msg_message ("Compiling analysis results display in '" &
         // char (driver_file) // "'")
    call msg_message ("Providing analysis steering makefile '" &
         // char (makefile) // "'")    
    only_file = global%var_list%get_lval &
         (var_str ("?analysis_file_only"))
    if (.not. only_file)  call analysis_compile_tex &
         (basename, has_gmlcode, global%os_data)
  end subroutine cmd_compile_analysis_execute

  subroutine cmd_open_out_final (object)
    class(cmd_open_out_t), intent(inout) :: object
  end subroutine cmd_open_out_final
  
  subroutine cmd_open_out_write (cmd, unit, indent)
    class(cmd_open_out_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)  
    write (u, "(1x,A)", advance="no")  "open_out: <filename>"  
  end subroutine cmd_open_out_write
  
  subroutine cmd_open_out_compile (cmd, global)
    class(cmd_open_out_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    cmd%file_expr => parse_node_get_sub_ptr (cmd%pn, 2)
    if (associated (cmd%file_expr)) then
       cmd%pn_opt => parse_node_get_next_ptr (cmd%file_expr)
    end if
    call cmd%compile_options (global)
  end subroutine cmd_open_out_compile

  subroutine cmd_open_out_execute (cmd, global)
    class(cmd_open_out_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(eval_tree_t) :: file_expr
    type(string_t) :: file
    var_list => cmd%local%get_var_list_ptr ()
    call file_expr%init_sexpr (cmd%file_expr, var_list)
    call file_expr%evaluate ()
    if (file_expr%is_known ()) then
       file = file_expr%get_string ()
       call file_list_open (global%out_files, file, &
            action = "write", status = "replace", position = "asis")
    else
       call msg_fatal ("open_out: file name argument evaluates to unknown")
    end if
    call file_expr%final ()
  end subroutine cmd_open_out_execute

  subroutine cmd_close_out_execute (cmd, global)
    class(cmd_close_out_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(eval_tree_t) :: file_expr
    type(string_t) :: file
    var_list => cmd%local%var_list
    call file_expr%init_sexpr (cmd%file_expr, var_list)
    call file_expr%evaluate ()
    if (file_expr%is_known ()) then
       file = file_expr%get_string ()
       call file_list_close (global%out_files, file)
    else
       call msg_fatal ("close_out: file name argument evaluates to unknown")
    end if
    call file_expr%final ()
  end subroutine cmd_close_out_execute

  subroutine cmd_printf_final (cmd)
    class(cmd_printf_t), intent(inout) :: cmd
    call parse_node_final (cmd%sexpr, recursive = .false.)
    deallocate (cmd%sexpr)
    call parse_node_final (cmd%sprintf_fun, recursive = .false.)
    deallocate (cmd%sprintf_fun)
    call parse_node_final (cmd%sprintf_clause, recursive = .false.)
    deallocate (cmd%sprintf_clause)
    call parse_node_final (cmd%sprintf, recursive = .false.)
    deallocate (cmd%sprintf)
  end subroutine cmd_printf_final

  subroutine cmd_printf_write (cmd, unit, indent)
    class(cmd_printf_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "printf:"
  end subroutine cmd_printf_write
  
  subroutine cmd_printf_compile (cmd, global)
    class(cmd_printf_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_clause, pn_args, pn_format
    pn_cmd => parse_node_get_sub_ptr (cmd%pn)
    pn_clause => parse_node_get_sub_ptr (pn_cmd)
    pn_format => parse_node_get_sub_ptr (pn_clause, 2)
    pn_args => parse_node_get_next_ptr (pn_clause)
    cmd%pn_opt => parse_node_get_next_ptr (pn_cmd)
    call cmd%compile_options (global)
    allocate (cmd%sexpr)
    call parse_node_create_branch (cmd%sexpr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sexpr")))
    allocate (cmd%sprintf_fun)
    call parse_node_create_branch (cmd%sprintf_fun, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sprintf_fun")))
    allocate (cmd%sprintf_clause)
    call parse_node_create_branch (cmd%sprintf_clause, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sprintf_clause")))
    allocate (cmd%sprintf)
    call parse_node_create_key (cmd%sprintf, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sprintf")))
    call parse_node_append_sub (cmd%sprintf_clause, cmd%sprintf)
    call parse_node_append_sub (cmd%sprintf_clause, pn_format)
    call parse_node_freeze_branch (cmd%sprintf_clause)
    call parse_node_append_sub (cmd%sprintf_fun, cmd%sprintf_clause)
    if (associated (pn_args)) then
       call parse_node_append_sub (cmd%sprintf_fun, pn_args)
    end if
    call parse_node_freeze_branch (cmd%sprintf_fun)
    call parse_node_append_sub (cmd%sexpr, cmd%sprintf_fun)
    call parse_node_freeze_branch (cmd%sexpr)
  end subroutine cmd_printf_compile

  subroutine cmd_printf_execute (cmd, global)
    class(cmd_printf_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(string_t) :: string, file
    type(eval_tree_t) :: sprintf_expr
    logical :: advance
    var_list => cmd%local%get_var_list_ptr ()
    advance = var_list%get_lval (&
         var_str ("?out_advance"))
    file = var_list%get_sval (&
         var_str ("$out_file"))
    call sprintf_expr%init_sexpr (cmd%sexpr, var_list)
    call sprintf_expr%evaluate ()
    if (sprintf_expr%is_known ()) then
       string = sprintf_expr%get_string ()
       if (len (file) == 0) then
          call msg_result (char (string))
       else
          call file_list_write (global%out_files, file, string, advance)
       end if
    end if
  end subroutine cmd_printf_execute

  subroutine cmd_record_write (cmd, unit, indent)
    class(cmd_record_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "record"
  end subroutine cmd_record_write
  
  subroutine cmd_record_compile (cmd, global)
    class(cmd_record_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_lexpr, pn_lsinglet, pn_lterm, pn_record
    call parse_node_create_branch (pn_lexpr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lexpr")))
    call parse_node_create_branch (pn_lsinglet, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lsinglet")))
    call parse_node_append_sub (pn_lexpr, pn_lsinglet)
    call parse_node_create_branch (pn_lterm, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lterm")))
    call parse_node_append_sub (pn_lsinglet, pn_lterm)
    pn_record => parse_node_get_sub_ptr (cmd%pn)
    call parse_node_append_sub (pn_lterm, pn_record)
    cmd%pn_lexpr => pn_lexpr
  end subroutine cmd_record_compile

  subroutine cmd_record_execute (cmd, global)
    class(cmd_record_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    logical :: lval
    var_list => global%get_var_list_ptr ()
    lval = eval_log (cmd%pn_lexpr, var_list)
  end subroutine cmd_record_execute

  subroutine cmd_unstable_write (cmd, unit, indent)
    class(cmd_unstable_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,I0,1x,A)", advance="no")  &
         "unstable:", 1, "("
    do i = 1, cmd%n_proc
       if (i > 1)  write (u, "(A,1x)", advance="no")  ","
       write (u, "(A)", advance="no")  char (cmd%process_id(i))
    end do
    write (u, "(A)")  ")"
  end subroutine cmd_unstable_write

  subroutine cmd_unstable_compile (cmd, global)
    class(cmd_unstable_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_list, pn_proc
    integer :: i
    cmd%pn_prt_in => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_list => parse_node_get_next_ptr (cmd%pn_prt_in)
    if (associated (pn_list)) then
       select case (char (parse_node_get_rule_key (pn_list)))
       case ("unstable_arg")
          cmd%n_proc = parse_node_get_n_sub (pn_list)
          cmd%pn_opt => parse_node_get_next_ptr (pn_list)
       case default
          cmd%n_proc = 0
          cmd%pn_opt => pn_list
          pn_list => null ()
       end select
    end if       
    call cmd%compile_options (global)
    if (associated (pn_list)) then
       allocate (cmd%process_id (cmd%n_proc))
       pn_proc => parse_node_get_sub_ptr (pn_list)
       do i = 1, cmd%n_proc
          cmd%process_id(i) = parse_node_get_string (pn_proc)
          call cmd%local%process_stack%init_result_vars (cmd%process_id(i))
          pn_proc => parse_node_get_next_ptr (pn_proc)
       end do
    else
       allocate (cmd%process_id (0))
    end if
  end subroutine cmd_unstable_compile

  subroutine cmd_unstable_execute (cmd, global)
    class(cmd_unstable_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    logical :: auto_decays, auto_decays_radiative
    integer :: auto_decays_multiplicity
    logical :: isotropic_decay, diagonal_decay, polarized_decay
    integer :: decay_helicity
    type(pdg_array_t) :: pa_in
    integer :: pdg_in
    type(string_t) :: libname_cur, libname_dec
    type(string_t), dimension(:), allocatable :: auto_id, tmp_id
    integer :: n_proc_user
    integer :: i, u_tmp
    character(80) :: buffer
    var_list => cmd%local%get_var_list_ptr ()
    auto_decays = &
         var_list%get_lval (var_str ("?auto_decays"))
    if (auto_decays) then
       auto_decays_multiplicity = &
            var_list%get_ival (var_str ("auto_decays_multiplicity"))
       auto_decays_radiative = &
            var_list%get_lval (var_str ("?auto_decays_radiative"))
    end if
    isotropic_decay = &
         var_list%get_lval (var_str ("?isotropic_decay"))
    if (isotropic_decay) then
       diagonal_decay = .false.
       polarized_decay = .false.
    else
       diagonal_decay = &
            var_list%get_lval (var_str ("?diagonal_decay"))
       if (diagonal_decay) then
          polarized_decay = .false.
       else
          polarized_decay = &
               var_list%is_known (var_str ("decay_helicity"))
          if (polarized_decay) then
             decay_helicity = var_list%get_ival (var_str ("decay_helicity"))
          end if
       end if
    end if
    pa_in = eval_pdg_array (cmd%pn_prt_in, var_list)
    if (pdg_array_get_length (pa_in) /= 1) &
         call msg_fatal ("Unstable: decaying particle must be unique")
    pdg_in = pdg_array_get (pa_in, 1)
    n_proc_user = cmd%n_proc
    if (auto_decays) then
       call create_auto_decays (pdg_in, &
            auto_decays_multiplicity, auto_decays_radiative, &
            libname_dec, auto_id, cmd%local)
       allocate (tmp_id (cmd%n_proc + size (auto_id)))
       tmp_id(:cmd%n_proc) = cmd%process_id
       tmp_id(cmd%n_proc+1:) = auto_id
       call move_alloc (from = tmp_id, to = cmd%process_id)
       cmd%n_proc = size (cmd%process_id)
    end if
    libname_cur = cmd%local%prclib%get_name ()
    do i = 1, cmd%n_proc
       if (i == n_proc_user + 1) then
          call cmd%local%update_prclib &
               (cmd%local%prclib_stack%get_library_ptr (libname_dec))
       end if
       if (.not. global%process_stack%exists (cmd%process_id(i))) then
          call var_list%set_log &
               (var_str ("?decay_rest_frame"), .false., is_known = .true.)
          call integrate_process (cmd%process_id(i), cmd%local, global)
          call global%process_stack%fill_result_vars (cmd%process_id(i))
       end if
    end do
    call cmd%local%update_prclib &
         (cmd%local%prclib_stack%get_library_ptr (libname_cur))
    if (cmd%n_proc > 0) then
       if (polarized_decay) then
          call global%modify_particle (pdg_in, stable = .false., &
               decay = cmd%process_id, &
               isotropic_decay = .false., &
               diagonal_decay = .false., &
               decay_helicity = decay_helicity, &
               polarized = .false.)
       else
          call global%modify_particle (pdg_in, stable = .false., &
               decay = cmd%process_id, &
               isotropic_decay = isotropic_decay, &
               diagonal_decay = diagonal_decay, &
               polarized = .false.)
       end if
       u_tmp = free_unit ()
       open (u_tmp, status = "scratch", action = "readwrite")
       call show_unstable (global, pdg_in, u_tmp)
       rewind (u_tmp)
       do
          read (u_tmp, "(A)", end = 1)  buffer
          write (msg_buffer, "(A)")  trim (buffer)
          call msg_message ()
       end do
1      continue
       close (u_tmp)
    else
       call err_unstable (global, pdg_in)
    end if
  end subroutine cmd_unstable_execute

  subroutine show_unstable (global, pdg, u)
    type(rt_data_t), intent(in), target :: global
    integer, intent(in) :: pdg, u
    type(flavor_t) :: flv
    type(string_t), dimension(:), allocatable :: decay
    real(default), dimension(:), allocatable :: br
    real(default) :: width
    type(process_t), pointer :: process
    type(process_component_def_t), pointer :: prc_def
    type(string_t), dimension(:), allocatable :: prt_out, prt_out_str
    integer :: i, j
    logical :: opened
    call flv%init (pdg, global%model)
    call flv%get_decays (decay)
    if (.not. allocated (decay))  return
    allocate (prt_out_str (size (decay)))
    allocate (br (size (decay)))
    do i = 1, size (br)
       process => global%process_stack%get_process_ptr (decay(i))
       prc_def => process%get_component_def_ptr (1)
       call prc_def%get_prt_out (prt_out)
       prt_out_str(i) = prt_out(1)
       do j = 2, size (prt_out)
          prt_out_str(i) = prt_out_str(i) // ", " // prt_out(j)
       end do
       br(i) = process%get_integral ()
    end do
    if (all (br >= 0)) then
       if (any (br > 0)) then
          width = sum (br)
          br = br / sum (br)
          write (u, "(A)") "Unstable particle " &
               // char (flv%get_name ()) &
               // ": computed branching ratios:"
          do i = 1, size (br)
             write (u, "(2x,A,':'," // FMT_14 // ",3x,A)") &
                  char (decay(i)), br(i), char (prt_out_str(i))
          end do
          write (u, "(2x,'Total width ='," // FMT_14 // ",' GeV (computed)')")  width
          write (u, "(2x,'            ='," // FMT_14 // ",' GeV (preset)')") &
               flv%get_width ()
          if (flv%decays_isotropically ()) then
             write (u, "(2x,A)")  "Decay options: isotropic"
          else if (flv%decays_diagonal ()) then
             write (u, "(2x,A)")  "Decay options: &
                  &projection on diagonal helicity states"
          else if (flv%has_decay_helicity ()) then
             write (u, "(2x,A,1x,I0)")  "Decay options: projection onto helicity =", &
                  flv%get_decay_helicity ()
          else
             write (u, "(2x,A)")  "Decay options: helicity treated exactly"
          end if
       else
          inquire (unit = u, opened = opened)
          if (opened .and. .not. mask_fatal_errors)  close (u)
          call msg_fatal ("Unstable particle " &
               // char (flv%get_name ()) &
               // ": partial width vanishes for all decay channels")
       end if
    else
       inquire (unit = u, opened = opened)       
       if (opened .and. .not. mask_fatal_errors)  close (u)
       call msg_fatal ("Unstable particle " &
               // char (flv%get_name ()) &
               // ": partial width is negative")
    end if
  end subroutine show_unstable
    
  subroutine err_unstable (global, pdg)
    type(rt_data_t), intent(in), target :: global
    integer, intent(in) :: pdg
    type(flavor_t) :: flv
    call flv%init (pdg, global%model)
    call msg_error ("Unstable: no allowed decays found for particle " &
         // char (flv%get_name ()) // ", keeping as stable")
  end subroutine err_unstable
    
  subroutine create_auto_decays &
       (pdg_in, mult, rad, libname_dec, process_id, global)
    integer, intent(in) :: pdg_in
    integer, intent(in) :: mult
    logical, intent(in) :: rad
    type(string_t), intent(out) :: libname_dec
    type(string_t), dimension(:), allocatable, intent(out) :: process_id
    type(rt_data_t), intent(inout) :: global
    type(prclib_entry_t), pointer :: lib_entry
    type(process_library_t), pointer :: lib
    type(ds_table_t) :: ds_table
    type(split_constraints_t) :: constraints
    type(pdg_array_t), dimension(:), allocatable :: pa_out
    character(80) :: buffer
    character :: p_or_a
    type(string_t) :: process_string, libname_cur
    type(flavor_t) :: flv_in, flv_out
    type(string_t) :: prt_in
    type(string_t), dimension(:), allocatable :: prt_out
    type(process_configuration_t) :: prc_config
    integer :: i, j, k
    call flv_in%init (pdg_in, global%model)
    if (rad) then
       call constraints%init (2)
    else
       call constraints%init (3)
       call constraints%set (3, constrain_radiation ())
    end if
    call constraints%set (1, constrain_n_tot (mult))
    call constraints%set (2, &
         constrain_mass_sum (flv_in%get_mass (), margin = 0._default))
    call ds_table%make (global%model, pdg_in, constraints)
    prt_in = flv_in%get_name ()
    if (pdg_in > 0) then
       p_or_a = "p"
    else
       p_or_a = "a"
    end if
    if (ds_table%get_length () == 0) then
       call msg_warning ("Auto-decays: Particle " // char (prt_in) // ": " &
            // "no decays found")
       libname_dec = ""
       allocate (process_id (0))
    else
       call msg_message ("Creating decay process library for particle " &
            // char (prt_in))
       libname_cur = global%prclib%get_name () 
       write (buffer, "(A,A,I0)")  "_d", p_or_a, abs (pdg_in)
       libname_dec = libname_cur // trim (buffer)
       lib => global%prclib_stack%get_library_ptr (libname_dec)
       if (.not. (associated (lib))) then
          allocate (lib_entry)
          call lib_entry%init (libname_dec)
          lib => lib_entry%process_library_t
          call global%add_prclib (lib_entry)
       else
          call global%update_prclib (lib)
       end if
       allocate (process_id (ds_table%get_length ()))
       do i = 1, size (process_id)
          write (buffer, "(A,'_',A,I0,'_',I0)") &
               "decay", p_or_a, abs (pdg_in), i
          process_id(i) = trim (buffer)
          process_string = process_id(i) // ": " // prt_in // " =>"
          call ds_table%get_pdg_out (i, pa_out)
          allocate (prt_out (size (pa_out)))
          do j = 1, size (pa_out)
             do k = 1, pa_out(j)%get_length ()
                call flv_out%init (pa_out(j)%get (k), global%model)
                if (k == 1) then
                   prt_out(j) = flv_out%get_name ()
                else
                   prt_out(j) = prt_out(j) // ":" // flv_out%get_name ()
                end if
             end do
             process_string = process_string // " " // prt_out(j)
          end do
          call msg_message (char (process_string))
          call prc_config%init (process_id(i), 1, 1, global)
          !!! Causes runtime error with gfortran 4.9.1 
          ! call prc_config%setup_component (1, &
          !      new_prt_spec ([prt_in]), new_prt_spec (prt_out), global%model, global%var_list)
          !!! Workaround:
          call prc_config%setup_component (1, &
               [new_prt_spec (prt_in)], new_prt_spec (prt_out), global%model, global%var_list)
          call prc_config%record (global)
          deallocate (prt_out)
          deallocate (pa_out)
       end do
       lib => global%prclib_stack%get_library_ptr (libname_cur)
       call global%update_prclib (lib)
    end if
    call ds_table%final ()
  end subroutine create_auto_decays
    
  subroutine cmd_stable_write (cmd, unit, indent)
    class(cmd_stable_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,I0)")  "stable:", size (cmd%pn_pdg)
  end subroutine cmd_stable_write

  subroutine cmd_stable_compile (cmd, global)
    class(cmd_stable_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_list, pn_prt
    integer :: n, i
    pn_list => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_list)
    call cmd%compile_options (global)
    n = parse_node_get_n_sub (pn_list)
    allocate (cmd%pn_pdg (n))
    pn_prt => parse_node_get_sub_ptr (pn_list)
    i = 1
    do while (associated (pn_prt))
       cmd%pn_pdg(i)%ptr => pn_prt
       pn_prt  => parse_node_get_next_ptr (pn_prt)
       i = i + 1
    end do
  end subroutine cmd_stable_compile

  subroutine cmd_stable_execute (cmd, global)
    class(cmd_stable_t), intent(inout) :: cmd
    type(rt_data_t), target, intent(inout) :: global
    type(var_list_t), pointer :: var_list
    type(pdg_array_t) :: pa
    integer :: pdg
    type(flavor_t) :: flv
    integer :: i
    var_list => cmd%local%get_var_list_ptr ()
    do i = 1, size (cmd%pn_pdg)
       pa = eval_pdg_array (cmd%pn_pdg(i)%ptr, var_list)
       if (pdg_array_get_length (pa) /= 1) &
            call msg_fatal ("Stable: listed particles must be unique")
       pdg = pdg_array_get (pa, 1)
       call global%modify_particle (pdg, stable = .true., &
         isotropic_decay = .false., &
         diagonal_decay = .false., &
         polarized = .false.)
       call flv%init (pdg, cmd%local%model)
       call msg_message ("Particle " &
            // char (flv%get_name ()) &
            // " declared as stable")
    end do
  end subroutine cmd_stable_execute
  
  subroutine cmd_polarized_write (cmd, unit, indent)
    class(cmd_polarized_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,I0)")  "polarized:", size (cmd%pn_pdg)
  end subroutine cmd_polarized_write

  subroutine cmd_unpolarized_write (cmd, unit, indent)
    class(cmd_unpolarized_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,I0)")  "unpolarized:", size (cmd%pn_pdg)
  end subroutine cmd_unpolarized_write

  subroutine cmd_polarized_execute (cmd, global)
    class(cmd_polarized_t), intent(inout) :: cmd
    type(rt_data_t), target, intent(inout) :: global
    type(var_list_t), pointer :: var_list
    type(pdg_array_t) :: pa
    integer :: pdg
    type(flavor_t) :: flv
    integer :: i
    var_list => cmd%local%get_var_list_ptr ()
    do i = 1, size (cmd%pn_pdg)
       pa = eval_pdg_array (cmd%pn_pdg(i)%ptr, var_list)
       if (pdg_array_get_length (pa) /= 1) &
            call msg_fatal ("Polarized: listed particles must be unique")
       pdg = pdg_array_get (pa, 1)
       call global%modify_particle (pdg, polarized = .true., &
            stable = .true., &
            isotropic_decay = .false., &
            diagonal_decay = .false.)
       call flv%init (pdg, cmd%local%model)
       call msg_message ("Particle " &
            // char (flv%get_name ()) &
            // " declared as polarized")
    end do
  end subroutine cmd_polarized_execute
  
  subroutine cmd_unpolarized_execute (cmd, global)
    class(cmd_unpolarized_t), intent(inout) :: cmd
    type(rt_data_t), target, intent(inout) :: global
    type(var_list_t), pointer :: var_list
    type(pdg_array_t) :: pa
    integer :: pdg
    type(flavor_t) :: flv
    integer :: i
    var_list => cmd%local%get_var_list_ptr ()
    do i = 1, size (cmd%pn_pdg)
       pa = eval_pdg_array (cmd%pn_pdg(i)%ptr, var_list)
       if (pdg_array_get_length (pa) /= 1) &
            call msg_fatal ("Unpolarized: listed particles must be unique")
       pdg = pdg_array_get (pa, 1)
       call global%modify_particle (pdg, polarized = .false., &
            stable = .true., &
            isotropic_decay = .false., &
            diagonal_decay = .false.)
       call flv%init (pdg, cmd%local%model)
       call msg_message ("Particle " &
            // char (flv%get_name ()) &
            // " declared as unpolarized")
    end do
  end subroutine cmd_unpolarized_execute
  
  subroutine cmd_sample_format_write (cmd, unit, indent)
    class(cmd_sample_format_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "sample_format = "
    do i = 1, size (cmd%format)
       if (i > 1)  write (u, "(A,1x)", advance="no")  ","
       write (u, "(A)", advance="no")  char (cmd%format(i))
    end do
    write (u, "(A)")
  end subroutine cmd_sample_format_write
  
  subroutine cmd_sample_format_compile (cmd, global)
    class(cmd_sample_format_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg
    type(parse_node_t), pointer :: pn_format
    integer :: i, n_format
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 3)
    if (associated (pn_arg)) then
       n_format = parse_node_get_n_sub (pn_arg)
       allocate (cmd%format (n_format))
       pn_format => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_format))
          i = i + 1
          cmd%format(i) = parse_node_get_string (pn_format)
          pn_format => parse_node_get_next_ptr (pn_format)
       end do
    else
       allocate (cmd%format (0))
    end if
  end subroutine cmd_sample_format_compile

  subroutine cmd_sample_format_execute (cmd, global)
    class(cmd_sample_format_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (allocated (global%sample_fmt))  deallocate (global%sample_fmt)
    allocate (global%sample_fmt (size (cmd%format)), source = cmd%format)
  end subroutine cmd_sample_format_execute

  subroutine cmd_simulate_write (cmd, unit, indent)
    class(cmd_simulate_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "simulate ("
    do i = 1, cmd%n_proc
       if (i > 1)  write (u, "(A,1x)", advance="no")  ","
       write (u, "(A)", advance="no")  char (cmd%process_id(i))
    end do
    write (u, "(A)")  ")"
  end subroutine cmd_simulate_write

  subroutine cmd_simulate_compile (cmd, global)
    class(cmd_simulate_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_proclist, pn_proc
    integer :: i
    pn_proclist => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_opt => parse_node_get_next_ptr (pn_proclist)
    call cmd%compile_options (global)
    cmd%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (cmd%process_id (cmd%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, cmd%n_proc
       cmd%process_id(i) = parse_node_get_string (pn_proc)
       call global%process_stack%init_result_vars (cmd%process_id(i))
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
  end subroutine cmd_simulate_compile

  subroutine cmd_simulate_execute (cmd, global)
    class(cmd_simulate_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(rt_data_t), dimension(:), allocatable, target :: alt_env
    integer :: n_events, n_fmt
    type(string_t) :: sample
    logical :: rebuild_events, read_raw, write_raw
    type(simulation_t), target :: sim
    type(string_t), dimension(:), allocatable :: sample_fmt
    type(event_stream_array_t) :: es_array
    type(event_sample_data_t) :: data
    integer :: i, checkpoint, callback
    var_list => cmd%local%var_list
    if (allocated (cmd%local%pn%alt_setup)) then
       allocate (alt_env (size (cmd%local%pn%alt_setup)))
       do i = 1, size (alt_env)
          call build_alt_setup (alt_env(i), cmd%local, &
               cmd%local%pn%alt_setup(i)%ptr)
       end do
       call sim%init (cmd%process_id, .true., .true., cmd%local, global, &
            alt_env)
    else
       call sim%init (cmd%process_id, .true., .true., cmd%local, global)
    end if
    if (signal_is_pending ())  return
    if (sim%is_valid ()) then
       call sim%init_process_selector ()    
       call openmp_set_num_threads_verbose &
            (var_list%get_ival (var_str ("openmp_num_threads")), &
            var_list%get_lval (var_str ("?openmp_logging")))
       call sim%compute_n_events (n_events, var_list)
       sample = var_list%get_sval (var_str ("$sample"))
       if (sample == "")  sample = sim%get_default_sample_name ()
       rebuild_events = &
            var_list%get_lval (var_str ("?rebuild_events"))
       read_raw = &
            var_list%get_lval (var_str ("?read_raw")) &
            .and. .not. rebuild_events
       write_raw = &
            var_list%get_lval (var_str ("?write_raw"))
       checkpoint = &
            var_list%get_ival (var_str ("checkpoint"))
       callback = &
            var_list%get_ival (var_str ("event_callback_interval"))
       if (read_raw) then
          inquire (file = char (sample) // ".evx", exist = read_raw)
       end if
       if (allocated (cmd%local%sample_fmt)) then
          n_fmt = size (cmd%local%sample_fmt)
       else
          n_fmt = 0
       end if
       data = sim%get_data ()
       data%n_evt = n_events
       data%nlo_multiplier = sim%get_n_nlo_entries (1)
       if (read_raw) then
          allocate (sample_fmt (n_fmt))
          if (n_fmt > 0)  sample_fmt = cmd%local%sample_fmt
          call es_array%init (sample, &
               sample_fmt, cmd%local, &
               data = data, &
               input = var_str ("raw"), &
               allow_switch = write_raw, &
               checkpoint = checkpoint, &
               callback = callback)
          call sim%generate (n_events, es_array)
          call es_array%final ()
       else if (write_raw) then
          allocate (sample_fmt (n_fmt + 1))
          if (n_fmt > 0)  sample_fmt(:n_fmt) = cmd%local%sample_fmt
          sample_fmt(n_fmt+1) = var_str ("raw")
          call es_array%init (sample, &
               sample_fmt, cmd%local, &
               data = data, &
               checkpoint = checkpoint, &
               callback = callback)
          call sim%generate (n_events, es_array)
          call es_array%final ()
       else if (allocated (cmd%local%sample_fmt) &
            .or. checkpoint > 0 &
            .or. callback > 0) then
          allocate (sample_fmt (n_fmt))
          if (n_fmt > 0)  sample_fmt = cmd%local%sample_fmt
          call es_array%init (sample, &
               sample_fmt, cmd%local, &
               data = data, &
               checkpoint = checkpoint, &
               callback = callback)
          call sim%generate (n_events, es_array)
          call es_array%final ()
       else
          call sim%generate (n_events)
       end if
       if (allocated (alt_env)) then
          do i = 1, size (alt_env)
             call alt_env(i)%local_final ()
          end do
       end if
    end if
    call sim%final ()
  end subroutine cmd_simulate_execute

  recursive subroutine build_alt_setup (alt_env, global, pn)
    type(rt_data_t), intent(inout), target :: alt_env
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), intent(in), target :: pn
    type(command_list_t), allocatable :: alt_options
    allocate (alt_options)
    call alt_env%local_init (global)
    call alt_env%activate ()
    call alt_options%compile (pn, alt_env)
    call alt_options%execute (alt_env)
    call alt_env%deactivate (global, keep_local = .true.)
    call alt_options%final ()
  end subroutine build_alt_setup
            
  subroutine cmd_rescan_write (cmd, unit, indent)
    class(cmd_rescan_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "rescan ("
    do i = 1, cmd%n_proc
       if (i > 1)  write (u, "(A,1x)", advance="no")  ","
       write (u, "(A)", advance="no")  char (cmd%process_id(i))
    end do
    write (u, "(A)")  ")"
  end subroutine cmd_rescan_write

  subroutine cmd_rescan_compile (cmd, global)
    class(cmd_rescan_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_filename, pn_proclist, pn_proc
    integer :: i
    pn_filename => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_proclist => parse_node_get_next_ptr (pn_filename)
    cmd%pn_opt => parse_node_get_next_ptr (pn_proclist)
    call cmd%compile_options (global)
    cmd%pn_filename => pn_filename
    cmd%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (cmd%process_id (cmd%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, cmd%n_proc
       cmd%process_id(i) = parse_node_get_string (pn_proc)
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
  end subroutine cmd_rescan_compile

  subroutine cmd_rescan_execute (cmd, global)
    class(cmd_rescan_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(rt_data_t), dimension(:), allocatable, target :: alt_env
    type(string_t) :: sample
    logical :: exist, write_raw, update_event, update_sqme, update_weight
    type(simulation_t), target :: sim
    type(event_sample_data_t) :: input_data, data
    type(string_t) :: input_sample
    integer :: n_fmt
    type(string_t), dimension(:), allocatable :: sample_fmt
    type(string_t) :: input_format, input_ext, input_file
    type(string_t) :: lhef_extension, extension_hepmc, extension_lcio
    type(event_stream_array_t) :: es_array
    integer :: i, n_events
    var_list => cmd%local%var_list
    if (allocated (cmd%local%pn%alt_setup)) then
       allocate (alt_env (size (cmd%local%pn%alt_setup)))
       do i = 1, size (alt_env)
          call build_alt_setup (alt_env(i), cmd%local, &
               cmd%local%pn%alt_setup(i)%ptr)
       end do
       call sim%init (cmd%process_id, .false., .false., cmd%local, global, &
            alt_env)
    else
       call sim%init (cmd%process_id, .false., .false., cmd%local, global)
    end if
    call sim%compute_n_events (n_events, var_list)
    input_sample = eval_string (cmd%pn_filename, var_list)
    input_format = var_list%get_sval (&
         var_str ("$rescan_input_format"))
    sample = var_list%get_sval (var_str ("$sample"))
    if (sample == "")  sample = sim%get_default_sample_name ()
    write_raw = var_list%get_lval (var_str ("?write_raw"))
    if (allocated (cmd%local%sample_fmt)) then
       n_fmt = size (cmd%local%sample_fmt)
    else
       n_fmt = 0
    end if
    if (write_raw) then
       if (sample == input_sample) then
          call msg_error ("Rescan: ?write_raw = true: " &
               // "suppressing raw event output (filename clashes with input)")
          allocate (sample_fmt (n_fmt))
          if (n_fmt > 0)  sample_fmt = cmd%local%sample_fmt
       else
          allocate (sample_fmt (n_fmt + 1))
          if (n_fmt > 0)  sample_fmt(:n_fmt) = cmd%local%sample_fmt
          sample_fmt(n_fmt+1) = var_str ("raw")
       end if
    else
       allocate (sample_fmt (n_fmt))
       if (n_fmt > 0)  sample_fmt = cmd%local%sample_fmt
    end if
    update_event = &
         var_list%get_lval (var_str ("?update_event"))
    update_sqme = &
         var_list%get_lval (var_str ("?update_sqme"))
    update_weight = &
         var_list%get_lval (var_str ("?update_weight"))
    if (update_event .or. update_sqme) then
       call msg_message ("Recalculating observables")
       if (update_sqme) then
          call msg_message ("Recalculating squared matrix elements")
       end if
    end if
    lhef_extension = &
         var_list%get_sval (var_str ("$lhef_extension"))
    extension_hepmc = &
         var_list%get_sval (var_str ("$extension_hepmc"))
    extension_lcio = &
         var_list%get_sval (var_str ("$extension_lcio"))
    select case (char (input_format))
    case ("raw");  input_ext = "evx"
       call cmd%local%set_log &
            (var_str ("?recover_beams"), .false., is_known=.true.)
    case ("lhef"); input_ext = lhef_extension
    case ("hepmc"); input_ext = extension_hepmc
    case default
       call msg_fatal ("rescan: input sample format '" // char (input_format) &
            // "' not supported")
    end select
    input_file = input_sample // "." // input_ext
    inquire (file = char (input_file), exist = exist)
    if (exist) then
       input_data = sim%get_data (alt = .false.)
       input_data%n_evt = n_events
       data = sim%get_data ()
       data%n_evt = n_events
       input_data%md5sum_cfg = ""
       call es_array%init (sample, &
            sample_fmt, cmd%local, data, &
            input = input_format, input_sample = input_sample, &
            input_data = input_data, &
            allow_switch = .false.)
       call sim%rescan (n_events, es_array, global = cmd%local)
       call es_array%final ()
    else
       call msg_fatal ("Rescan: event file '" &
            // char (input_file) // "' not found")
    end if
    if (allocated (alt_env)) then
       do i = 1, size (alt_env)
          call alt_env(i)%local_final ()
       end do
    end if
    call sim%final ()
  end subroutine cmd_rescan_execute

  subroutine cmd_iterations_write (cmd, unit, indent)
    class(cmd_iterations_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    select case (cmd%n_pass)
    case (0)
       write (u, "(1x,A)")  "iterations: [empty]"
    case (1)
       write (u, "(1x,A,I0,A)")  "iterations: ", cmd%n_pass, " pass"
    case default
       write (u, "(1x,A,I0,A)")  "iterations: ", cmd%n_pass, " passes"
    end select
  end subroutine cmd_iterations_write

  subroutine cmd_iterations_compile (cmd, global)
    class(cmd_iterations_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_n_it, pn_n_calls, pn_adapt
    type(parse_node_t), pointer :: pn_it_spec, pn_calls_spec, pn_adapt_spec
    integer :: i
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 3)
    if (associated (pn_arg)) then
       cmd%n_pass = parse_node_get_n_sub (pn_arg)
       allocate (cmd%pn_expr_n_it (cmd%n_pass))
       allocate (cmd%pn_expr_n_calls (cmd%n_pass))
       allocate (cmd%pn_sexpr_adapt (cmd%n_pass))
       pn_it_spec => parse_node_get_sub_ptr (pn_arg)
       i = 1
       do while (associated (pn_it_spec))
          pn_n_it => parse_node_get_sub_ptr (pn_it_spec)
          pn_calls_spec => parse_node_get_next_ptr (pn_n_it)
          pn_n_calls => parse_node_get_sub_ptr (pn_calls_spec, 2)
          pn_adapt_spec => parse_node_get_next_ptr (pn_calls_spec)
          if (associated (pn_adapt_spec)) then
             pn_adapt => parse_node_get_sub_ptr (pn_adapt_spec, 2)
          else
             pn_adapt => null ()
          end if
          cmd%pn_expr_n_it(i)%ptr => pn_n_it
          cmd%pn_expr_n_calls(i)%ptr => pn_n_calls
          cmd%pn_sexpr_adapt(i)%ptr => pn_adapt
          i = i + 1
          pn_it_spec => parse_node_get_next_ptr (pn_it_spec)
       end do
    else
       allocate (cmd%pn_expr_n_it (0))
       allocate (cmd%pn_expr_n_calls (0))
    end if
  end subroutine cmd_iterations_compile

  subroutine cmd_iterations_execute (cmd, global)
    class(cmd_iterations_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    integer, dimension(cmd%n_pass) :: n_it, n_calls
    logical, dimension(cmd%n_pass) :: custom_adapt
    type(string_t), dimension(cmd%n_pass) :: adapt_code
    integer :: i
    var_list => global%get_var_list_ptr ()
    do i = 1, cmd%n_pass
       n_it(i) = eval_int (cmd%pn_expr_n_it(i)%ptr, var_list)
       n_calls(i) = &
            eval_int (cmd%pn_expr_n_calls(i)%ptr, var_list)
       if (associated (cmd%pn_sexpr_adapt(i)%ptr)) then
          adapt_code(i) = &
               eval_string (cmd%pn_sexpr_adapt(i)%ptr, &
                            var_list, is_known = custom_adapt(i))
       else
          custom_adapt(i) = .false.
       end if        
    end do
    call global%it_list%init (n_it, n_calls, custom_adapt, adapt_code)
  end subroutine cmd_iterations_execute

  subroutine range_final (object)
    class(range_t), intent(inout) :: object
    if (associated (object%pn_expr)) then
       call parse_node_final (object%pn_expr, recursive = .false.)
       call parse_node_final (object%pn_term, recursive = .false.)
       call parse_node_final (object%pn_factor, recursive = .false.)
       call parse_node_final (object%pn_value, recursive = .false.)
       call parse_node_final (object%pn_literal, recursive = .false.)
       deallocate (object%pn_expr)
       deallocate (object%pn_term)
       deallocate (object%pn_factor)
       deallocate (object%pn_value)
       deallocate (object%pn_literal)
    end if
  end subroutine range_final
  
  subroutine range_write (object, unit)
    class(range_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Range specification:"
    if (associated (object%pn_expr)) then
       write (u, "(1x,A)")  "Dummy value:"
       call parse_node_write_rec (object%pn_expr, u)
    end if
    if (associated (object%pn_beg)) then
       write (u, "(1x,A)")  "Initial value:"
       call parse_node_write_rec (object%pn_beg, u)
       call object%expr_beg%write (u)
       if (associated (object%pn_end)) then
          write (u, "(1x,A)")  "Final value:"
          call parse_node_write_rec (object%pn_end, u)
          call object%expr_end%write (u)
          if (associated (object%pn_step)) then
             write (u, "(1x,A)")  "Step value:"
             call parse_node_write_rec (object%pn_step, u)
             select case (object%step_mode)
             case (STEP_ADD);   write (u, "(1x,A)")  "Step mode: +"
             case (STEP_SUB);   write (u, "(1x,A)")  "Step mode: -"
             case (STEP_MUL);   write (u, "(1x,A)")  "Step mode: *"
             case (STEP_DIV);   write (u, "(1x,A)")  "Step mode: /"
             case (STEP_COMP_ADD);  write (u, "(1x,A)")  "Division mode: +"
             case (STEP_COMP_MUL);  write (u, "(1x,A)")  "Division mode: *"
             end select
          end if
       end if
    else
       write (u, "(1x,A)")  "Expressions: [undefined]"
    end if
  end subroutine range_write

  subroutine range_int_write (object, unit)
    class(range_int_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call object%base_write (unit)
    write (u, "(1x,A)")  "Range parameters:"
    write (u, "(3x,A,I0)")  "i_beg  = ", object%i_beg
    write (u, "(3x,A,I0)")  "i_end  = ", object%i_end
    write (u, "(3x,A,I0)")  "i_step = ", object%i_step
    write (u, "(3x,A,I0)")  "n_step = ", object%n_step
  end subroutine range_int_write
  
  subroutine range_real_write (object, unit)
    class(range_real_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call object%base_write (unit)
    write (u, "(1x,A)")  "Range parameters:"
    write (u, "(3x,A," // FMT_19 // ")")  "r_beg  = ", object%r_beg
    write (u, "(3x,A," // FMT_19 // ")")  "r_end  = ", object%r_end
    write (u, "(3x,A," // FMT_19 // ")")  "r_step = ", object%r_end
    write (u, "(3x,A,I0)")  "n_step = ", object%n_step
  end subroutine range_real_write
  
  subroutine range_init (range, pn)
    class(range_t), intent(out) :: range
    type(parse_node_t), intent(in), target :: pn
    type(parse_node_t), pointer :: pn_spec, pn_end, pn_step_spec, pn_op
    select case (char (parse_node_get_rule_key (pn)))
    case ("expr")
    case ("range_expr")
       range%pn_beg => parse_node_get_sub_ptr (pn)
       pn_spec => parse_node_get_next_ptr (range%pn_beg)
       if (associated (pn_spec)) then
          pn_end => parse_node_get_sub_ptr (pn_spec, 2)
          range%pn_end => pn_end
          pn_step_spec => parse_node_get_next_ptr (pn_end)
          if (associated (pn_step_spec)) then
             pn_op => parse_node_get_sub_ptr (pn_step_spec)
             range%pn_step => parse_node_get_next_ptr (pn_op)
             select case (char (parse_node_get_rule_key (pn_op)))
             case ("/+");  range%step_mode = STEP_ADD
             case ("/-");  range%step_mode = STEP_SUB
             case ("/*");  range%step_mode = STEP_MUL
             case ("//");  range%step_mode = STEP_DIV
             case ("/+/");  range%step_mode = STEP_COMP_ADD
             case ("/*/");  range%step_mode = STEP_COMP_MUL
             case default
                call range%write ()
                call msg_bug ("Range: step mode not implemented")
             end select
          else
             range%step_mode = STEP_ADD
          end if
       else
          range%step_mode = STEP_NONE
       end if
       call range%create_value_node ()
    case default
       call msg_bug ("range expression: node type '" &
            // char (parse_node_get_rule_key (pn)) &
            // "' not implemented")
    end select
  end subroutine range_init
  
  subroutine range_create_value_node (range)
    class(range_t), intent(inout) :: range
    allocate (range%pn_literal)
    allocate (range%pn_value)
    select type (range)
    type is (range_int_t)
       call parse_node_create_value (range%pn_literal, &
            syntax_get_rule_ptr (syntax_cmd_list, var_str ("integer_literal")),&
            ival = 0)
       call parse_node_create_branch (range%pn_value, &
            syntax_get_rule_ptr (syntax_cmd_list, var_str ("integer_value")))
    type is (range_real_t)
       call parse_node_create_value (range%pn_literal, &
            syntax_get_rule_ptr (syntax_cmd_list, var_str ("real_literal")),&
            rval = 0._default)
       call parse_node_create_branch (range%pn_value, &
            syntax_get_rule_ptr (syntax_cmd_list, var_str ("real_value")))
    class default
       call msg_bug ("range: create value node: type not implemented")
    end select
    call parse_node_append_sub (range%pn_value, range%pn_literal)
    call parse_node_freeze_branch (range%pn_value)
    allocate (range%pn_factor)
    call parse_node_create_branch (range%pn_factor, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("factor")))
    call parse_node_append_sub (range%pn_factor, range%pn_value)
    call parse_node_freeze_branch (range%pn_factor)
    allocate (range%pn_term)
    call parse_node_create_branch (range%pn_term, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("term")))
    call parse_node_append_sub (range%pn_term, range%pn_factor)
    call parse_node_freeze_branch (range%pn_term)
    allocate (range%pn_expr)
    call parse_node_create_branch (range%pn_expr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("expr")))
    call parse_node_append_sub (range%pn_expr, range%pn_term)
    call parse_node_freeze_branch (range%pn_expr)
  end subroutine range_create_value_node
  
  subroutine range_compile (range, global)
    class(range_t), intent(inout) :: range
    type(rt_data_t), intent(in), target :: global
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    if (associated (range%pn_beg)) then
       call range%expr_beg%init_expr (range%pn_beg, var_list)
       if (associated (range%pn_end)) then
          call range%expr_end%init_expr (range%pn_end, var_list)
          if (associated (range%pn_step)) then
             call range%expr_step%init_expr (range%pn_step, var_list)
          end if
       end if
    end if
  end subroutine range_compile
  
  subroutine range_int_evaluate (range)
    class(range_int_t), intent(inout) :: range
    integer :: ival
    if (associated (range%pn_beg)) then
       call range%expr_beg%evaluate ()
       if (range%expr_beg%is_known ()) then
          range%i_beg = range%expr_beg%get_int ()
       else
          call range%write ()
          call msg_fatal &
               ("Range expression: initial value evaluates to unknown")
       end if
       if (associated (range%pn_end)) then
          call range%expr_end%evaluate ()
          if (range%expr_end%is_known ()) then
             range%i_end = range%expr_end%get_int ()
             if (associated (range%pn_step)) then
                call range%expr_step%evaluate ()
                if (range%expr_step%is_known ()) then
                   range%i_step = range%expr_step%get_int ()
                   select case (range%step_mode)
                   case (STEP_SUB);  range%i_step = - range%i_step
                   end select
                else
                   call range%write ()
                   call msg_fatal &
                        ("Range expression: step value evaluates to unknown")
                end if
             else
                range%i_step = 1
             end if
          else
             call range%write ()
             call msg_fatal &
                  ("Range expression: final value evaluates to unknown")
          end if
       else
          range%i_end = range%i_beg
          range%i_step = 1
       end if
       select case (range%step_mode)
       case (STEP_NONE)
          range%n_step = 1
       case (STEP_ADD, STEP_SUB)
          if (range%i_step /= 0) then
             if (range%i_beg == range%i_end) then
                range%n_step = 1
             else if (sign (1, range%i_end - range%i_beg) &
                  == sign (1, range%i_step)) then
                range%n_step = (range%i_end - range%i_beg) / range%i_step + 1
             else
                range%n_step = 0
             end if
          else
             call msg_fatal ("range evaluation (add): step value is zero")
          end if
       case (STEP_MUL)
          if (range%i_step > 1) then
             if (range%i_beg == range%i_end) then
                range%n_step = 1
             else if (range%i_beg == 0) then
                call msg_fatal ("range evaluation (mul): initial value is zero")
             else if (sign (1, range%i_beg) == sign (1, range%i_end) &
                  .and. abs (range%i_beg) < abs (range%i_end)) then
                range%n_step = 0
                ival = range%i_beg
                do while (abs (ival) <= abs (range%i_end))
                   range%n_step = range%n_step + 1
                   ival = ival * range%i_step
                end do
             else
                range%n_step = 0
             end if
          else
             call msg_fatal &
                  ("range evaluation (mult): step value is one or less")
          end if
       case (STEP_DIV)
          if (range%i_step > 1) then
             if (range%i_beg == range%i_end) then
                range%n_step = 1
             else if (sign (1, range%i_beg) == sign (1, range%i_end) &
                  .and. abs (range%i_beg) > abs (range%i_end)) then
                range%n_step = 0
                ival = range%i_beg
                do while (abs (ival) >= abs (range%i_end))
                   range%n_step = range%n_step + 1
                   if (ival == 0)  exit
                   ival = ival / range%i_step
                end do
             else
                range%n_step = 0
             end if
          else
             call msg_fatal &
                  ("range evaluation (div): step value is one or less")
          end if
       case (STEP_COMP_ADD)
          call msg_fatal ("range evaluation: &
               &step mode /+/ not allowed for integer variable")
       case (STEP_COMP_MUL)
          call msg_fatal ("range evaluation: &
               &step mode /*/ not allowed for integer variable")
       case default
          call range%write ()
          call msg_bug ("range evaluation: step mode not implemented")
       end select
    end if
  end subroutine range_int_evaluate

  subroutine range_real_evaluate (range)
    class(range_real_t), intent(inout) :: range
    if (associated (range%pn_beg)) then
       call range%expr_beg%evaluate ()
       if (range%expr_beg%is_known ()) then
          range%r_beg = range%expr_beg%get_real ()
       else
          call range%write ()
          call msg_fatal &
               ("Range expression: initial value evaluates to unknown")
       end if
       if (associated (range%pn_end)) then
          call range%expr_end%evaluate ()
          if (range%expr_end%is_known ()) then
             range%r_end = range%expr_end%get_real ()
             if (associated (range%pn_step)) then
                if (range%expr_step%is_known ()) then
                   select case (range%step_mode)
                   case (STEP_ADD, STEP_SUB, STEP_MUL, STEP_DIV)
                      call range%expr_step%evaluate ()
                      range%r_step = range%expr_step%get_real ()
                      select case (range%step_mode)
                      case (STEP_SUB);  range%r_step = - range%r_step
                      end select
                   case (STEP_COMP_ADD, STEP_COMP_MUL)
                      range%n_step = &
                           max (range%expr_step%get_int (), 0)
                   end select
                else
                   call range%write ()
                   call msg_fatal &
                        ("Range expression: step value evaluates to unknown")
                end if
             else
                call range%write ()
                call msg_fatal &
                     ("Range expression (real): step value must be provided")
             end if
          else
             call range%write ()
             call msg_fatal &
                  ("Range expression: final value evaluates to unknown")
          end if
       else
          range%r_end = range%r_beg
          range%r_step = 1
       end if
       select case (range%step_mode)
       case (STEP_NONE)
          range%n_step = 1
       case (STEP_ADD, STEP_SUB)
          if (range%r_step /= 0) then
             if (sign (1._default, range%r_end - range%r_beg) &
                  == sign (1._default, range%r_step)) then
                range%n_step = &
                     nint ((range%r_end - range%r_beg) / range%r_step + 1)
             else
                range%n_step = 0
             end if
          else
             call msg_fatal ("range evaluation (add): step value is zero")
          end if
       case (STEP_MUL)
          if (range%r_step > 1) then
             if (range%r_beg == 0 .or. range%r_end == 0) then
                call msg_fatal ("range evaluation (mul): bound is zero")
             else if (sign (1._default, range%r_beg) &
                  == sign (1._default, range%r_end) &
                  .and. abs (range%r_beg) <= abs (range%r_end)) then
                range%lr_beg = log (abs (range%r_beg))
                range%lr_end = log (abs (range%r_end))
                range%lr_step = log (range%r_step)
                range%n_step = nint &
                     (abs ((range%lr_end - range%lr_beg) / range%lr_step) + 1)
             else
                range%n_step = 0
             end if
          else
             call msg_fatal &
                  ("range evaluation (mult): step value is one or less")
          end if
       case (STEP_DIV)
          if (range%r_step > 1) then
             if (range%r_beg == 0 .or. range%r_end == 0) then
                call msg_fatal ("range evaluation (div): bound is zero")
             else if (sign (1._default, range%r_beg) &
                  == sign (1._default, range%r_end) &
                  .and. abs (range%r_beg) >= abs (range%r_end)) then
                range%lr_beg = log (abs (range%r_beg))
                range%lr_end = log (abs (range%r_end))
                range%lr_step = -log (range%r_step)
                range%n_step = nint &
                     (abs ((range%lr_end - range%lr_beg) / range%lr_step) + 1)
             else
                range%n_step = 0
             end if
          else
             call msg_fatal &
                  ("range evaluation (mult): step value is one or less")
          end if
       case (STEP_COMP_ADD)
          ! Number of steps already known
       case (STEP_COMP_MUL)
          ! Number of steps already known
          if (range%r_beg == 0 .or. range%r_end == 0) then
             call msg_fatal ("range evaluation (mul): bound is zero")
          else if (sign (1._default, range%r_beg) &
               == sign (1._default, range%r_end)) then
             range%lr_beg = log (abs (range%r_beg))
             range%lr_end = log (abs (range%r_end))
          else
             range%n_step = 0
          end if
       case default
          call range%write ()
          call msg_bug ("range evaluation: step mode not implemented")
       end select
    end if
  end subroutine range_real_evaluate

  function range_get_n_iterations (range) result (n)
    class(range_t), intent(in) :: range
    integer :: n
    n = range%n_step
  end function range_get_n_iterations
  
  subroutine range_int_set_value (range, i)
    class(range_int_t), intent(inout) :: range
    integer, intent(in) :: i
    integer :: k, ival
    select case (range%step_mode)
    case (STEP_NONE)
       ival = range%i_beg
    case (STEP_ADD, STEP_SUB)
       ival = range%i_beg + (i - 1) * range%i_step
    case (STEP_MUL)
       ival = range%i_beg
       do k = 1, i - 1
          ival = ival * range%i_step
       end do
    case (STEP_DIV)
       ival = range%i_beg
       do k = 1, i - 1
          ival = ival / range%i_step
       end do
    case default
       call range%write ()
       call msg_bug ("range iteration: step mode not implemented")
    end select
    call parse_node_set_value (range%pn_literal, ival = ival)
  end subroutine range_int_set_value
  
  subroutine range_real_set_value (range, i)
    class(range_real_t), intent(inout) :: range
    integer, intent(in) :: i
    real(default) :: rval, x
    select case (range%step_mode)
    case (STEP_NONE)
       rval = range%r_beg
    case (STEP_ADD, STEP_SUB, STEP_COMP_ADD)
       if (range%n_step > 1) then
          x = real (i - 1, default) / (range%n_step - 1)
       else
          x = 1._default / 2
       end if
       rval = x * range%r_end + (1 - x) * range%r_beg
    case (STEP_MUL, STEP_DIV, STEP_COMP_MUL)
       if (range%n_step > 1) then
          x = real (i - 1, default) / (range%n_step - 1)
       else
          x = 1._default / 2
       end if
       rval = sign &
            (exp (x * range%lr_end + (1 - x) * range%lr_beg), range%r_beg)
    case default
       call range%write ()
       call msg_bug ("range iteration: step mode not implemented")
    end select
    call parse_node_set_value (range%pn_literal, rval = rval)
  end subroutine range_real_set_value
  
  recursive subroutine cmd_scan_final (cmd)
    class(cmd_scan_t), intent(inout) :: cmd
    type(parse_node_t), pointer :: pn_var_single, pn_decl_single
    type(string_t) :: key
    integer :: i
    if (allocated (cmd%scan_cmd)) then
       do i = 1, size (cmd%scan_cmd)
          pn_var_single => parse_node_get_sub_ptr (cmd%scan_cmd(i)%ptr)
          key = parse_node_get_rule_key (pn_var_single)
          select case (char (key))
          case ("scan_string_decl", "scan_log_decl")
             pn_decl_single => parse_node_get_sub_ptr (pn_var_single, 2)
             call parse_node_final (pn_decl_single, recursive=.false.)
             deallocate (pn_decl_single)
          end select
          call parse_node_final (pn_var_single, recursive=.false.)
          deallocate (pn_var_single)
       end do
       deallocate (cmd%scan_cmd)
    end if
    !!! !!! gfortran 4.7.x memory corruption
    !!!  if (allocated (cmd%range)) then
    !!!     do i = 1, size (cmd%range)
    !!!        call cmd%range(i)%final ()
    !!!     end do
    !!!  end if
    if (allocated (cmd%range_int)) then
       do i = 1, size (cmd%range_int)
          call cmd%range_int(i)%final ()
       end do
    end if
    if (allocated (cmd%range_real)) then
       do i = 1, size (cmd%range_real)
          call cmd%range_real(i)%final ()
       end do
    end if
  end subroutine cmd_scan_final

  subroutine cmd_scan_write (cmd, unit, indent)
    class(cmd_scan_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,A,1x,'(',I0,')')")  "scan:", char (cmd%name), &
         cmd%n_values
  end subroutine cmd_scan_write

  recursive subroutine cmd_scan_compile (cmd, global)
    class(cmd_scan_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    type(parse_node_t), pointer :: pn_var, pn_body, pn_body_first
    type(parse_node_t), pointer :: pn_decl, pn_name
    type(parse_node_t), pointer :: pn_arg, pn_scan_cmd, pn_rhs
    type(parse_node_t), pointer :: pn_decl_single, pn_var_single
    type(syntax_rule_t), pointer :: var_rule_decl, var_rule
    type(string_t) :: key
    integer :: var_type
    integer :: i
    logical, parameter :: debug = .false.
    if (debug) then
       print *, "compile scan"
       call parse_node_write_rec (cmd%pn)
    end if
    pn_var => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_body => parse_node_get_next_ptr (pn_var)
    if (associated (pn_body)) then
       pn_body_first => parse_node_get_sub_ptr (pn_body)
    else
       pn_body_first => null ()
    end if
    key = parse_node_get_rule_key (pn_var)
    select case (char (key))
    case ("scan_num")
       pn_name => parse_node_get_sub_ptr (pn_var)
       cmd%name = parse_node_get_string (pn_name)
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, var_str ("cmd_num"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_int")
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, var_str ("cmd_int"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_real")
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, var_str ("cmd_real"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_complex")
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, var_str("cmd_complex"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_alias")
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, var_str ("cmd_alias"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_string_decl")
       pn_decl => parse_node_get_sub_ptr (pn_var, 2)
       pn_name => parse_node_get_sub_ptr (pn_decl, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule_decl => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_string"))
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_string_decl"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_log_decl")
       pn_decl => parse_node_get_sub_ptr (pn_var, 2)
       pn_name => parse_node_get_sub_ptr (pn_decl, 2)
       cmd%name = parse_node_get_string (pn_name)
       var_rule_decl => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_log"))
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_log_decl"))
       pn_arg => parse_node_get_next_ptr (pn_name, 2)
    case ("scan_cuts")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_cuts"))
       cmd%name = "cuts"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_weight")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_weight"))
       cmd%name = "weight"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_scale")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_scale"))
       cmd%name = "scale"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_ren_scale")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_ren_scale"))
       cmd%name = "renormalization_scale"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_fac_scale")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_fac_scale"))
       cmd%name = "factorization_scale"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_selection")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_selection"))
       cmd%name = "selection"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_reweight")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_reweight"))
       cmd%name = "reweight"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_analysis")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_analysis"))
       cmd%name = "analysis"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_model")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_model"))
       cmd%name = "model"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case ("scan_library")
       var_rule => syntax_get_rule_ptr (syntax_cmd_list, &
            var_str ("cmd_library"))
       cmd%name = "library"
       pn_arg => parse_node_get_sub_ptr (pn_var, 3)
    case default
       call msg_bug ("scan: case '" // char (key) // "' not implemented")
    end select
    if (associated (pn_arg)) then
       cmd%n_values = parse_node_get_n_sub (pn_arg)
    end if
    var_list => global%get_var_list_ptr ()
    allocate (cmd%scan_cmd (cmd%n_values))
    select case (char (key))
    case ("scan_num")
       var_type = &
            var_list%get_type (cmd%name)
       select case (var_type)
       case (V_INT)
          !!! !!! gfortran 4.7.x memory corruption
          !!!  allocate (range_int_t :: cmd%range (cmd%n_values))
          allocate (cmd%range_int (cmd%n_values))
       case (V_REAL)
          !!! !!! gfortran 4.7.x memory corruption          
          !!!  allocate (range_real_t :: cmd%range (cmd%n_values))
          allocate (cmd%range_real (cmd%n_values))
       case (V_CMPLX)
          call msg_fatal ("scan over complex variable not implemented")
       case (V_NONE)
          call msg_fatal ("scan: variable '" // char (cmd%name) //"' undefined")
       case default
          call msg_bug ("scan: impossible variable type")
       end select
    case ("scan_int")
       !!! !!! gfortran 4.7.x memory corruption       
       !!!  allocate (range_int_t :: cmd%range (cmd%n_values))
       allocate (cmd%range_int (cmd%n_values))
    case ("scan_real")
       !!! !!! gfortran 4.7.x memory corruption
       !!!  allocate (range_real_t :: cmd%range (cmd%n_values))
       allocate (cmd%range_real (cmd%n_values))
    case ("scan_complex")
       call msg_fatal ("scan over complex variable not implemented")
    end select
    i = 1
    if (associated (pn_arg)) then
       pn_rhs => parse_node_get_sub_ptr (pn_arg)
    else
       pn_rhs => null ()
    end if
    do while (associated (pn_rhs))
       allocate (pn_scan_cmd)
       call parse_node_create_branch (pn_scan_cmd, &
            syntax_get_rule_ptr (syntax_cmd_list, var_str ("command_list")))
       allocate (pn_var_single)
       pn_var_single = pn_var
       call parse_node_replace_rule (pn_var_single, var_rule)
       select case (char (key))
       case ("scan_num", "scan_int", "scan_real", &
            "scan_complex", "scan_alias", &
            "scan_cuts", "scan_weight", &
            "scan_scale", "scan_ren_scale", "scan_fac_scale", &
            "scan_selection", "scan_reweight", "scan_analysis", &
            "scan_model", "scan_library")
          if (allocated (cmd%range_int)) then
             call cmd%range_int(i)%init (pn_rhs)
             !!! !!! gfortran 4.7.x memory corruption             
             !!!  call cmd%range_int(i)%compile (global)
             call parse_node_replace_last_sub &
                  (pn_var_single, cmd%range_int(i)%pn_expr)
          else if (allocated (cmd%range_real)) then
             call cmd%range_real(i)%init (pn_rhs)
             !!! !!! gfortran 4.7.x memory corruption 
             !!!  call cmd%range_real(i)%compile (global)
             call parse_node_replace_last_sub &
                  (pn_var_single, cmd%range_real(i)%pn_expr)
          else
             call parse_node_replace_last_sub (pn_var_single, pn_rhs)
          end if
       case ("scan_string_decl", "scan_log_decl")
          allocate (pn_decl_single)
          pn_decl_single = pn_decl
          call parse_node_replace_rule (pn_decl_single, var_rule_decl)
          call parse_node_replace_last_sub (pn_decl_single, pn_rhs)
          call parse_node_freeze_branch (pn_decl_single)
          call parse_node_replace_last_sub (pn_var_single, pn_decl_single)
       case default
          call msg_bug ("scan: case '" // char (key)  &
               // "' broken")
       end select
       call parse_node_freeze_branch (pn_var_single)
       call parse_node_append_sub (pn_scan_cmd, pn_var_single)
       call parse_node_append_sub (pn_scan_cmd, pn_body_first)
       call parse_node_freeze_branch (pn_scan_cmd)
       cmd%scan_cmd(i)%ptr => pn_scan_cmd
       i = i + 1
       pn_rhs => parse_node_get_next_ptr (pn_rhs)
    end do
    if (debug) then
       do i = 1, cmd%n_values
          print *, "scan command ", i
          call parse_node_write_rec (cmd%scan_cmd(i)%ptr)
          if (allocated (cmd%range_int))  call cmd%range_int(i)%write ()
          if (allocated (cmd%range_real))  call cmd%range_real(i)%write ()
       end do
       print *, "original"
       call parse_node_write_rec (cmd%pn)
    end if
  end subroutine cmd_scan_compile

  recursive subroutine cmd_scan_execute (cmd, global)
    class(cmd_scan_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(rt_data_t), allocatable :: local
    integer :: i, j
    do i = 1, cmd%n_values
       if (allocated (cmd%range_int)) then
          call cmd%range_int(i)%compile (global)
          call cmd%range_int(i)%evaluate ()
          do j = 1, cmd%range_int(i)%get_n_iterations ()
             call cmd%range_int(i)%set_value (j)
             allocate (local)
             call build_alt_setup (local, global, cmd%scan_cmd(i)%ptr)
             call local%local_final ()
             deallocate (local)
          end do
       else if (allocated (cmd%range_real)) then
          call cmd%range_real(i)%compile (global)
          call cmd%range_real(i)%evaluate ()
          do j = 1, cmd%range_real(i)%get_n_iterations ()
             call cmd%range_real(i)%set_value (j)
             allocate (local)
             call build_alt_setup (local, global, cmd%scan_cmd(i)%ptr)
             call local%local_final ()
             deallocate (local)
          end do
       else
          allocate (local)
          call build_alt_setup (local, global, cmd%scan_cmd(i)%ptr)
          call local%local_final ()
          deallocate (local)
       end if
    end do
  end subroutine cmd_scan_execute

  recursive subroutine cmd_if_final (cmd)
    class(cmd_if_t), intent(inout) :: cmd
    integer :: i
    if (associated (cmd%if_body)) then
       call command_list_final (cmd%if_body)
       deallocate (cmd%if_body)
    end if
    if (associated (cmd%elsif_cmd)) then
       do i = 1, size (cmd%elsif_cmd)
          call cmd_if_final (cmd%elsif_cmd(i))
       end do
       deallocate (cmd%elsif_cmd)
    end if
    if (associated (cmd%else_body)) then
       call command_list_final (cmd%else_body)
       deallocate (cmd%else_body)
    end if
  end subroutine cmd_if_final

  subroutine cmd_if_write (cmd, unit, indent)
    class(cmd_if_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, ind, i
    u = given_output_unit (unit);  if (u < 0)  return
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, indent)
    write (u, "(A)")  "if <expr> then"
    if (associated (cmd%if_body)) then
       call cmd%if_body%write (unit, ind + 1)
    end if
    if (associated (cmd%elsif_cmd)) then
       do i = 1, size (cmd%elsif_cmd)
          call write_indent (u, indent)
          write (u, "(A)")  "elsif <expr> then"
          if (associated (cmd%elsif_cmd(i)%if_body)) then
             call cmd%elsif_cmd(i)%if_body%write (unit, ind + 1)
          end if
       end do
    end if
    if (associated (cmd%else_body)) then
       call write_indent (u, indent)
       write (u, "(A)")  "else"
       call cmd%else_body%write (unit, ind + 1)
    end if
  end subroutine cmd_if_write
  
  recursive subroutine cmd_if_compile (cmd, global)
    class(cmd_if_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_lexpr, pn_body
    type(parse_node_t), pointer :: pn_elsif_clauses, pn_cmd_elsif
    type(parse_node_t), pointer :: pn_else_clause, pn_cmd_else
    integer :: i, n_elsif
    pn_lexpr => parse_node_get_sub_ptr (cmd%pn, 2)
    cmd%pn_if_lexpr => pn_lexpr
    pn_body => parse_node_get_next_ptr (pn_lexpr, 2)
    select case (char (parse_node_get_rule_key (pn_body)))
    case ("command_list")
       allocate (cmd%if_body)
       call cmd%if_body%compile (pn_body, global)
       pn_elsif_clauses => parse_node_get_next_ptr (pn_body)
    case default
       pn_elsif_clauses => pn_body
    end select
    select case (char (parse_node_get_rule_key (pn_elsif_clauses)))
    case ("elsif_clauses")
       n_elsif = parse_node_get_n_sub (pn_elsif_clauses)
       allocate (cmd%elsif_cmd (n_elsif))
       pn_cmd_elsif => parse_node_get_sub_ptr (pn_elsif_clauses)
       do i = 1, n_elsif
          pn_lexpr => parse_node_get_sub_ptr (pn_cmd_elsif, 2)
          cmd%elsif_cmd(i)%pn_if_lexpr => pn_lexpr
          pn_body => parse_node_get_next_ptr (pn_lexpr, 2)
          if (associated (pn_body)) then
             allocate (cmd%elsif_cmd(i)%if_body)
             call cmd%elsif_cmd(i)%if_body%compile (pn_body, global)
          end if
          pn_cmd_elsif => parse_node_get_next_ptr (pn_cmd_elsif)
       end do
       pn_else_clause => parse_node_get_next_ptr (pn_elsif_clauses)
    case default
       pn_else_clause => pn_elsif_clauses
    end select
    select case (char (parse_node_get_rule_key (pn_else_clause)))
    case ("else_clause")
       pn_cmd_else => parse_node_get_sub_ptr (pn_else_clause)
       pn_body => parse_node_get_sub_ptr (pn_cmd_else, 2)
       if (associated (pn_body)) then
          allocate (cmd%else_body)
          call cmd%else_body%compile (pn_body, global)
       end if
    end select
  end subroutine cmd_if_compile

  recursive subroutine cmd_if_execute (cmd, global)
    class(cmd_if_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    logical :: lval, is_known
    integer :: i
    var_list => global%get_var_list_ptr ()
    lval = eval_log (cmd%pn_if_lexpr, var_list, is_known=is_known)
    if (is_known) then
       if (lval) then
          if (associated (cmd%if_body)) then
             call cmd%if_body%execute (global)
          end if
          return
       end if
    else
       call error_undecided ()
       return
    end if
    if (associated (cmd%elsif_cmd)) then
       SCAN_ELSIF: do i = 1, size (cmd%elsif_cmd)
          lval = eval_log (cmd%elsif_cmd(i)%pn_if_lexpr, var_list, &
                is_known=is_known)
          if (is_known) then
             if (lval) then
                if (associated (cmd%elsif_cmd(i)%if_body)) then
                   call cmd%elsif_cmd(i)%if_body%execute (global)
                end if
                return
             end if
          else
             call error_undecided ()
             return
          end if
       end do SCAN_ELSIF
    end if
    if (associated (cmd%else_body)) then
       call cmd%else_body%execute (global)
    end if
  contains
    subroutine error_undecided ()
      call msg_error ("Undefined result of cmditional expression: " &
           // "neither branch will be executed")
    end subroutine error_undecided
  end subroutine cmd_if_execute

  subroutine cmd_include_final (cmd)
    class(cmd_include_t), intent(inout) :: cmd
    call parse_tree_final (cmd%parse_tree)
    if (associated (cmd%command_list)) then
       call cmd%command_list%final ()
       deallocate (cmd%command_list)
    end if
  end subroutine cmd_include_final

  subroutine cmd_include_write (cmd, unit, indent)
    class(cmd_include_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, indent)
    write (u, "(A,A,A,A)")  "include ", '"', char (cmd%file), '"'
    if (associated (cmd%command_list)) then
       call cmd%command_list%write (u, ind + 1)
    end if
  end subroutine cmd_include_write
  
  subroutine cmd_include_compile (cmd, global)
    class(cmd_include_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_file
    type(string_t) :: file
    logical :: exist
    integer :: u
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    pn_file => parse_node_get_sub_ptr (pn_arg)
    file = parse_node_get_string (pn_file)
    inquire (file=char(file), exist=exist)
    if (exist) then
       cmd%file = file
    else
       cmd%file = global%os_data%whizard_cutspath // "/" // file
       inquire (file=char(cmd%file), exist=exist)
       if (.not. exist) then
          call msg_error ("Include file '" // char (file) // "' not found")
          return
       end if
    end if
    u = free_unit ()
    call lexer_init_cmd_list (lexer, global%lexer)
    call stream_init (stream, char (cmd%file))
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (cmd%parse_tree, syntax_cmd_list, lexer)
    call stream_final (stream)
    call lexer_final (lexer)
    close (u)
    allocate (cmd%command_list)
    call cmd%command_list%compile (cmd%parse_tree%get_root_ptr (), &
         global)
  end subroutine cmd_include_compile

  subroutine cmd_include_execute (cmd, global)
    class(cmd_include_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    if (associated (cmd%command_list)) then
       call msg_message &
            ("Including Sindarin from '" // char (cmd%file) // "'")
       call cmd%command_list%execute (global)
       call msg_message &
            ("End of included '" // char (cmd%file) // "'")
    end if
  end subroutine cmd_include_execute

  subroutine cmd_quit_write (cmd, unit, indent)
    class(cmd_quit_t), intent(in) :: cmd
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,L1)")  "quit: has_code = ", cmd%has_code
  end subroutine cmd_quit_write

  subroutine cmd_quit_compile (cmd, global)
    class(cmd_quit_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg
    pn_arg => parse_node_get_sub_ptr (cmd%pn, 2)
    if (associated (pn_arg)) then
       cmd%pn_code_expr => parse_node_get_sub_ptr (pn_arg)
       cmd%has_code = .true.
    end if
  end subroutine cmd_quit_compile

  subroutine cmd_quit_execute (cmd, global)
    class(cmd_quit_t), intent(inout) :: cmd
    type(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    logical :: is_known
    var_list => global%get_var_list_ptr ()
    if (cmd%has_code) then
       global%quit_code = eval_int (cmd%pn_code_expr, var_list, &
            is_known=is_known)
       if (.not. is_known) then
          call msg_error ("Undefined return code of quit/exit command")
       end if
    end if
    global%quit = .true.
  end subroutine cmd_quit_execute

  recursive subroutine command_list_write (cmd_list, unit, indent)
    class(command_list_t), intent(in) :: cmd_list
    integer, intent(in), optional :: unit, indent
    class(command_t), pointer :: cmd
    cmd => cmd_list%first
    do while (associated (cmd))
       call cmd%write (unit, indent)
       cmd => cmd%next
    end do
  end subroutine command_list_write
  
  subroutine command_list_append (cmd_list, command)
    class(command_list_t), intent(inout) :: cmd_list
    class(command_t), intent(inout), pointer :: command
    if (associated (cmd_list%last)) then
       cmd_list%last%next => command
    else
       cmd_list%first => command
    end if
    cmd_list%last => command
    command => null ()
  end subroutine command_list_append

  recursive subroutine command_list_final (cmd_list)
    class(command_list_t), intent(inout) :: cmd_list
    class(command_t), pointer :: command
    do while (associated (cmd_list%first))
       command => cmd_list%first
       cmd_list%first => cmd_list%first%next
       call command%final ()
       deallocate (command)
    end do
    cmd_list%last => null ()
  end subroutine command_list_final

  recursive subroutine command_list_compile (cmd_list, pn, global)
    class(command_list_t), intent(inout), target :: cmd_list
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_cmd
    class(command_t), pointer :: command
    integer :: i
    pn_cmd => parse_node_get_sub_ptr (pn)
    do i = 1, parse_node_get_n_sub (pn)
       call dispatch_command (command, pn_cmd)
       call command%compile (global)
       call cmd_list%append (command)
       call terminate_now_if_signal ()
       pn_cmd => parse_node_get_next_ptr (pn_cmd)
    end do
  end subroutine command_list_compile

  recursive subroutine command_list_execute (cmd_list, global)
    class(command_list_t), intent(in) :: cmd_list
    type(rt_data_t), intent(inout), target :: global
    class(command_t), pointer :: command
    command => cmd_list%first
    COMMAND_COND: do while (associated (command))
       call command%execute_options (global)
       call command%execute (global)
       call command%reset_options (global)
       call terminate_now_if_signal ()
       if (global%quit)  exit COMMAND_COND
       command => command%next
    end do COMMAND_COND
  end subroutine command_list_execute

  subroutine syntax_cmd_list_init ()
    type(ifile_t) :: ifile
    call define_cmd_list_syntax (ifile)
    call syntax_init (syntax_cmd_list, ifile)
    call ifile_final (ifile)
  end subroutine syntax_cmd_list_init

  subroutine syntax_cmd_list_final ()
    call syntax_final (syntax_cmd_list)
  end subroutine syntax_cmd_list_final

  subroutine syntax_cmd_list_write (unit)
    integer, intent(in), optional :: unit
    call syntax_write (syntax_cmd_list, unit)
  end subroutine syntax_cmd_list_write

  subroutine define_cmd_list_syntax (ifile)
    type(ifile_t), intent(inout) :: ifile
    call ifile_append (ifile, "SEQ command_list = command*")
    call ifile_append (ifile, "ALT command = " &
         // "cmd_model | cmd_library | cmd_iterations | cmd_sample_format | " &
         // "cmd_var | cmd_slha | " &
         // "cmd_show | cmd_clear | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_scale | cmd_fac_scale | cmd_ren_scale | " &
         // "cmd_weight | cmd_selection | cmd_reweight | " &
         // "cmd_beams | cmd_beams_pol_density | cmd_beams_pol_fraction | " &
         // "cmd_beams_momentum | cmd_beams_theta | cmd_beams_phi | " &
         // "cmd_integrate | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_graph | " &
         // "cmd_record | " &
         // "cmd_analysis | cmd_alt_setup | " &
         // "cmd_unstable | cmd_stable | cmd_simulate | cmd_rescan | " &
         // "cmd_process | cmd_compile | cmd_exec | " &
         // "cmd_scan | cmd_if | cmd_include | cmd_quit | " &
         // "cmd_polarized | cmd_unpolarized | " &
         // "cmd_open_out | cmd_close_out | cmd_printf | " &
         // "cmd_write_analysis | cmd_compile_analysis | cmd_nlo | cmd_components") 
    call ifile_append (ifile, "GRO options = '{' local_command_list '}'")
    call ifile_append (ifile, "SEQ local_command_list = local_command*")
    call ifile_append (ifile, "ALT local_command = " &
         // "cmd_model | cmd_library | cmd_iterations | cmd_sample_format | " &
         // "cmd_var | cmd_slha | " &
         // "cmd_show | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_scale | cmd_fac_scale | cmd_ren_scale | " &
         // "cmd_weight | cmd_selection | cmd_reweight | " &
         // "cmd_beams | cmd_beams_pol_density | cmd_beams_pol_fraction | " &
         // "cmd_beams_momentum | cmd_beams_theta | cmd_beams_phi | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_graph | " &
         // "cmd_clear | cmd_record | " &
         // "cmd_analysis | cmd_alt_setup | " &
         // "cmd_open_out | cmd_close_out | cmd_printf | " &
         // "cmd_write_analysis | cmd_compile_analysis | cmd_nlo | cmd_components")
    call ifile_append (ifile, "SEQ cmd_model = model '=' model_name model_arg?")
    call ifile_append (ifile, "KEY model")
    call ifile_append (ifile, "ALT model_name = model_id | string_literal")
    call ifile_append (ifile, "IDE model_id")
    call ifile_append (ifile, "ARG model_arg = ( model_scheme? )")
    call ifile_append (ifile, "ALT model_scheme = scheme_id | string_literal")
    call ifile_append (ifile, "IDE scheme_id")
    call ifile_append (ifile, "SEQ cmd_library = library '=' lib_name")
    call ifile_append (ifile, "KEY library")
    call ifile_append (ifile, "ALT lib_name = lib_id | string_literal")
    call ifile_append (ifile, "IDE lib_id")
    call ifile_append (ifile, "ALT cmd_var = " &
         // "cmd_log_decl | cmd_log | " &
         // "cmd_int | cmd_real | cmd_complex | cmd_num | " &
         // "cmd_string_decl | cmd_string | cmd_alias | " &
         // "cmd_result")
    call ifile_append (ifile, "SEQ cmd_log_decl = logical cmd_log")
    call ifile_append (ifile, "SEQ cmd_log = '?' var_name '=' lexpr")
    call ifile_append (ifile, "SEQ cmd_int = int var_name '=' expr")
    call ifile_append (ifile, "SEQ cmd_real = real var_name '=' expr")
    call ifile_append (ifile, "SEQ cmd_complex = complex var_name '=' expr")
    call ifile_append (ifile, "SEQ cmd_num = var_name '=' expr")
    call ifile_append (ifile, "SEQ cmd_string_decl = string cmd_string")
    call ifile_append (ifile, "SEQ cmd_string = " &
         // "'$' var_name '=' sexpr") ! $
    call ifile_append (ifile, "SEQ cmd_alias = alias var_name '=' cexpr")
    call ifile_append (ifile, "SEQ cmd_result = result '=' expr") 
    call ifile_append (ifile, "SEQ cmd_slha = slha_action slha_arg options?")
    call ifile_append (ifile, "ALT slha_action = " &
         // "read_slha | write_slha")
    call ifile_append (ifile, "KEY read_slha")
    call ifile_append (ifile, "KEY write_slha")
    call ifile_append (ifile, "ARG slha_arg = ( string_literal )")
    call ifile_append (ifile, "SEQ cmd_show = show show_arg options?")
    call ifile_append (ifile, "KEY show")
    call ifile_append (ifile, "ARG show_arg = ( showable* )")
    call ifile_append (ifile, "ALT showable = " &
         // "model | library | beams | iterations | " &
         // "cuts | weight | logical | string | pdg | " &
         // "scale | factorization_scale | renormalization_scale | " & 
         // "selection | reweight | analysis | " &
         // "stable | unstable | polarized | unpolarized | " &
         // "expect | intrinsic | int | real | complex | " &
         // "alias_var | string | results | result_var | " &
         // "log_var | string_var | var_name")
    call ifile_append (ifile, "KEY results")
    call ifile_append (ifile, "KEY intrinsic")    
    call ifile_append (ifile, "SEQ alias_var = alias var_name")
    call ifile_append (ifile, "SEQ result_var = result_key result_arg?")
    call ifile_append (ifile, "SEQ log_var = '?' var_name")
    call ifile_append (ifile, "SEQ string_var = '$' var_name")  ! $
    call ifile_append (ifile, "SEQ cmd_clear = clear clear_arg options?")    
    call ifile_append (ifile, "KEY clear")
    call ifile_append (ifile, "ARG clear_arg = ( clearable* )")
    call ifile_append (ifile, "ALT clearable = " &
         // "beams | iterations | " &
         // "cuts | weight | " &
         // "scale | factorization_scale | renormalization_scale | " & 
         // "selection | reweight | analysis | " &
         // "unstable | polarized | " &
         // "expect | " &
         // "log_var | string_var | var_name")
    call ifile_append (ifile, "SEQ cmd_expect = expect expect_arg options?")
    call ifile_append (ifile, "KEY expect")
    call ifile_append (ifile, "ARG expect_arg = ( lexpr )")
    call ifile_append (ifile, "SEQ cmd_cuts = cuts '=' lexpr")
    call ifile_append (ifile, "SEQ cmd_scale = scale '=' expr")    
    call ifile_append (ifile, "SEQ cmd_fac_scale = " &
         // "factorization_scale '=' expr")
    call ifile_append (ifile, "SEQ cmd_ren_scale = " &
         // "renormalization_scale '=' expr")
    call ifile_append (ifile, "SEQ cmd_weight = weight '=' expr")
    call ifile_append (ifile, "SEQ cmd_selection = selection '=' lexpr")
    call ifile_append (ifile, "SEQ cmd_reweight = reweight '=' expr")
    call ifile_append (ifile, "KEY cuts")
    call ifile_append (ifile, "KEY scale")    
    call ifile_append (ifile, "KEY factorization_scale")
    call ifile_append (ifile, "KEY renormalization_scale")    
    call ifile_append (ifile, "KEY weight")
    call ifile_append (ifile, "KEY selection")
    call ifile_append (ifile, "KEY reweight")
    call ifile_append (ifile, "SEQ cmd_process = process process_id '=' " &
         // "process_prt '=>' prt_state_list options?")
    call ifile_append (ifile, "KEY process")
    call ifile_append (ifile, "KEY '=>'")
    call ifile_append (ifile, "LIS process_prt = cexpr+")
    call ifile_append (ifile, "LIS prt_state_list = prt_state_sum+")
    call ifile_append (ifile, "SEQ prt_state_sum = " &
         // "prt_state prt_state_addition*")
    call ifile_append (ifile, "SEQ prt_state_addition = '+' prt_state")
    call ifile_append (ifile, "ALT prt_state = grouped_prt_state_list | cexpr")
    call ifile_append (ifile, "GRO grouped_prt_state_list = " &
         // "( prt_state_list )")
    call ifile_append (ifile, "SEQ cmd_compile = compile_cmd options?")
    call ifile_append (ifile, "SEQ compile_cmd = compile_clause compile_arg?")
    call ifile_append (ifile, "SEQ compile_clause = compile exec_name_spec?")
    call ifile_append (ifile, "KEY compile")
    call ifile_append (ifile, "SEQ exec_name_spec = as exec_name")
    call ifile_append (ifile, "KEY as")
    call ifile_append (ifile, "ALT exec_name = exec_id | string_literal")
    call ifile_append (ifile, "IDE exec_id")
    call ifile_append (ifile, "ARG compile_arg = ( lib_name* )")
    call ifile_append (ifile, "SEQ cmd_exec = exec exec_arg")
    call ifile_append (ifile, "KEY exec")
    call ifile_append (ifile, "ARG exec_arg = ( sexpr )")
    call ifile_append (ifile, "SEQ cmd_beams = beams '=' beam_def")
    call ifile_append (ifile, "KEY beams")
    call ifile_append (ifile, "SEQ beam_def = beam_spec strfun_seq*")
    call ifile_append (ifile, "SEQ beam_spec = beam_list")
    call ifile_append (ifile, "LIS beam_list = cexpr, cexpr?")
    call ifile_append (ifile, "SEQ cmd_beams_pol_density = " &
         // "beams_pol_density '=' beams_pol_spec")
    call ifile_append (ifile, "KEY beams_pol_density")
    call ifile_append (ifile, "LIS beams_pol_spec = smatrix, smatrix?")
    call ifile_append (ifile, "SEQ smatrix = '@' smatrix_arg")
    ! call ifile_append (ifile, "KEY '@'")     !!! Key already exists
    call ifile_append (ifile, "ARG smatrix_arg = ( sentry* )")
    call ifile_append (ifile, "SEQ sentry = expr extra_sentry*")
    call ifile_append (ifile, "SEQ extra_sentry = ':' expr")
    call ifile_append (ifile, "SEQ cmd_beams_pol_fraction = " &
         // "beams_pol_fraction '=' beams_par_spec")
    call ifile_append (ifile, "KEY beams_pol_fraction")
    call ifile_append (ifile, "SEQ cmd_beams_momentum = " &
         // "beams_momentum '=' beams_par_spec")
    call ifile_append (ifile, "KEY beams_momentum")
    call ifile_append (ifile, "SEQ cmd_beams_theta = " &
         // "beams_theta '=' beams_par_spec")
    call ifile_append (ifile, "KEY beams_theta")
    call ifile_append (ifile, "SEQ cmd_beams_phi = " &
         // "beams_phi '=' beams_par_spec")
    call ifile_append (ifile, "KEY beams_phi")
    call ifile_append (ifile, "LIS beams_par_spec = expr, expr?")
    call ifile_append (ifile, "SEQ strfun_seq = '=>' strfun_pair")
    call ifile_append (ifile, "LIS strfun_pair = strfun_def, strfun_def?")
    call ifile_append (ifile, "SEQ strfun_def = strfun_id")
    call ifile_append (ifile, "ALT strfun_id = " &
          // "none | lhapdf | lhapdf_photon | pdf_builtin | pdf_builtin_photon | " &
          // "isr | epa | ewa | circe1 | circe2 | energy_scan | " &
          // "gaussian | beam_events | user_sf_spec")
    call ifile_append (ifile, "KEY none")
    call ifile_append (ifile, "KEY lhapdf")
    call ifile_append (ifile, "KEY lhapdf_photon")    
    call ifile_append (ifile, "KEY pdf_builtin")    
    call ifile_append (ifile, "KEY pdf_builtin_photon")        
    call ifile_append (ifile, "KEY isr")
    call ifile_append (ifile, "KEY epa")
    call ifile_append (ifile, "KEY ewa")    
    call ifile_append (ifile, "KEY circe1")        
    call ifile_append (ifile, "KEY circe2")
    call ifile_append (ifile, "KEY energy_scan")
    call ifile_append (ifile, "KEY gaussian")
    call ifile_append (ifile, "KEY beam_events")
    call ifile_append (ifile, "SEQ user_sf_spec = user_strfun user_arg")
    call ifile_append (ifile, "KEY user_strfun")
    call ifile_append (ifile, "SEQ cmd_integrate = " &
         // "integrate proc_arg options?") 
    call ifile_append (ifile, "KEY integrate")
    call ifile_append (ifile, "ARG proc_arg = ( proc_id* )")
    call ifile_append (ifile, "IDE proc_id")
    call ifile_append (ifile, "SEQ cmd_iterations = " &
         // "iterations '=' iterations_list")
    call ifile_append (ifile, "KEY iterations")
    call ifile_append (ifile, "LIS iterations_list = iterations_spec+")
    call ifile_append (ifile, "ALT iterations_spec = it_spec")
    call ifile_append (ifile, "SEQ it_spec = expr calls_spec adapt_spec?")
    call ifile_append (ifile, "SEQ calls_spec = ':' expr")
    call ifile_append (ifile, "SEQ adapt_spec = ':' sexpr")
    call ifile_append (ifile, "SEQ cmd_components = " &
         // "active '=' component_list")
    call ifile_append (ifile, "KEY active")
    call ifile_append (ifile, "LIS component_list = sexpr+")
    call ifile_append (ifile, "SEQ cmd_sample_format = " &
         // "sample_format '=' event_format_list")
    call ifile_append (ifile, "KEY sample_format")
    call ifile_append (ifile, "LIS event_format_list = event_format+")
    call ifile_append (ifile, "IDE event_format")
    call ifile_append (ifile, "SEQ cmd_observable = " &
         // "observable analysis_tag options?")
    call ifile_append (ifile, "KEY observable")
    call ifile_append (ifile, "SEQ cmd_histogram = " &
         // "histogram analysis_tag histogram_arg " & 
         // "options?")
    call ifile_append (ifile, "KEY histogram")
    call ifile_append (ifile, "ARG histogram_arg = (expr, expr, expr?)")
    call ifile_append (ifile, "SEQ cmd_plot = plot analysis_tag options?")
    call ifile_append (ifile, "KEY plot")
    call ifile_append (ifile, "SEQ cmd_graph = graph graph_term '=' graph_def")
    call ifile_append (ifile, "KEY graph")
    call ifile_append (ifile, "SEQ graph_term = analysis_tag options?")
    call ifile_append (ifile, "SEQ graph_def = graph_term graph_append*")
    call ifile_append (ifile, "SEQ graph_append = '&' graph_term")
    call ifile_append (ifile, "SEQ cmd_analysis = analysis '=' lexpr")
    call ifile_append (ifile, "KEY analysis")
    call ifile_append (ifile, "SEQ cmd_alt_setup = " &
         // "alt_setup '=' option_list_expr")
    call ifile_append (ifile, "KEY alt_setup")
    call ifile_append (ifile, "ALT option_list_expr = " &
         // "grouped_option_list | option_list")
    call ifile_append (ifile, "GRO grouped_option_list = ( option_list_expr )")
    call ifile_append (ifile, "LIS option_list = options+")
    call ifile_append (ifile, "SEQ cmd_open_out = open_out open_arg options?")
    call ifile_append (ifile, "SEQ cmd_close_out = close_out open_arg options?")
    call ifile_append (ifile, "KEY open_out")
    call ifile_append (ifile, "KEY close_out")
    call ifile_append (ifile, "ARG open_arg = (sexpr)")
    call ifile_append (ifile, "SEQ cmd_printf = printf_cmd options?")
    call ifile_append (ifile, "SEQ printf_cmd = printf_clause sprintf_args?")
    call ifile_append (ifile, "SEQ printf_clause = printf sexpr")
    call ifile_append (ifile, "KEY printf")
    call ifile_append (ifile, "SEQ cmd_record = record_cmd")
    call ifile_append (ifile, "SEQ cmd_unstable = " &
         // "unstable cexpr unstable_arg options?")
    call ifile_append (ifile, "KEY unstable")
    call ifile_append (ifile, "ARG unstable_arg = ( proc_id* )")
    call ifile_append (ifile, "SEQ cmd_stable = stable stable_list options?")
    call ifile_append (ifile, "KEY stable")
    call ifile_append (ifile, "LIS stable_list = cexpr+")
    call ifile_append (ifile, "KEY polarized")
    call ifile_append (ifile, "SEQ cmd_polarized = polarized polarized_list options?")
    call ifile_append (ifile, "LIS polarized_list = cexpr+")
    call ifile_append (ifile, "KEY unpolarized")
    call ifile_append (ifile, "SEQ cmd_unpolarized = unpolarized unpolarized_list options?")
    call ifile_append (ifile, "LIS unpolarized_list = cexpr+")
    call ifile_append (ifile, "SEQ cmd_simulate = " &
         // "simulate proc_arg options?")
    call ifile_append (ifile, "KEY simulate")
    call ifile_append (ifile, "SEQ cmd_rescan = " &
         // "rescan sexpr proc_arg options?")
    call ifile_append (ifile, "KEY rescan")
    call ifile_append (ifile, "SEQ cmd_scan = scan scan_var scan_body?")
    call ifile_append (ifile, "KEY scan")
    call ifile_append (ifile, "ALT scan_var = " &
         // "scan_log_decl | scan_log | " &
         // "scan_int | scan_real | scan_complex | scan_num | " &
         // "scan_string_decl | scan_string | scan_alias | " &
         // "scan_cuts | scan_weight | " &
         // "scan_scale | scan_ren_scale | scan_fac_scale | " &
         // "scan_selection | scan_reweight | scan_analysis | " &
         // "scan_model | scan_library")
    call ifile_append (ifile, "SEQ scan_log_decl = logical scan_log")
    call ifile_append (ifile, "SEQ scan_log = '?' var_name '=' scan_log_arg")
    call ifile_append (ifile, "ARG scan_log_arg = ( lexpr* )")
    call ifile_append (ifile, "SEQ scan_int = int var_name '=' scan_num_arg")
    call ifile_append (ifile, "SEQ scan_real = real var_name '=' scan_num_arg")
    call ifile_append (ifile, "SEQ scan_complex = " &
         // "complex var_name '=' scan_num_arg")
    call ifile_append (ifile, "SEQ scan_num = var_name '=' scan_num_arg")
    call ifile_append (ifile, "ARG scan_num_arg = ( range* )")
    call ifile_append (ifile, "ALT range = grouped_range | range_expr")
    call ifile_append (ifile, "GRO grouped_range = ( range_expr )")
    call ifile_append (ifile, "SEQ range_expr = expr range_spec?")
    call ifile_append (ifile, "SEQ range_spec = '=>' expr step_spec?")
    call ifile_append (ifile, "SEQ step_spec = step_op expr")
    call ifile_append (ifile, "ALT step_op = " &
         // "'/+' | '/-' | '/*' | '//' | '/+/' | '/*/'")
    call ifile_append (ifile, "KEY '/+'")
    call ifile_append (ifile, "KEY '/-'")
    call ifile_append (ifile, "KEY '/*'")
    call ifile_append (ifile, "KEY '//'")
    call ifile_append (ifile, "KEY '/+/'")
    call ifile_append (ifile, "KEY '/*/'")
    call ifile_append (ifile, "SEQ scan_string_decl = string scan_string")
    call ifile_append (ifile, "SEQ scan_string = " &
         // "'$' var_name '=' scan_string_arg")
    call ifile_append (ifile, "ARG scan_string_arg = ( sexpr* )")
    call ifile_append (ifile, "SEQ scan_alias = " &
         // "alias var_name '=' scan_alias_arg")
    call ifile_append (ifile, "ARG scan_alias_arg = ( cexpr* )")
    call ifile_append (ifile, "SEQ scan_cuts = cuts '=' scan_lexpr_arg")
    call ifile_append (ifile, "ARG scan_lexpr_arg = ( lexpr* )")
    call ifile_append (ifile, "SEQ scan_scale = scale '=' scan_expr_arg")
    call ifile_append (ifile, "ARG scan_expr_arg = ( expr* )")
    call ifile_append (ifile, "SEQ scan_fac_scale = " &
         // "factorization_scale '=' scan_expr_arg")
    call ifile_append (ifile, "SEQ scan_ren_scale = " &
         // "renormalization_scale '=' scan_expr_arg")
    call ifile_append (ifile, "SEQ scan_weight = weight '=' scan_expr_arg")
    call ifile_append (ifile, "SEQ scan_selection = selection '=' scan_lexpr_arg")
    call ifile_append (ifile, "SEQ scan_reweight = reweight '=' scan_expr_arg")
    call ifile_append (ifile, "SEQ scan_analysis = analysis '=' scan_lexpr_arg")
    call ifile_append (ifile, "SEQ scan_model = model '=' scan_model_arg")
    call ifile_append (ifile, "ARG scan_model_arg = ( model_name* )")
    call ifile_append (ifile, "SEQ scan_library = library '=' scan_library_arg")
    call ifile_append (ifile, "ARG scan_library_arg = ( lib_name* )")
    call ifile_append (ifile, "GRO scan_body = '{' command_list '}'")
    call ifile_append (ifile, "SEQ cmd_if = " &
         // "if lexpr then command_list elsif_clauses else_clause endif")
    call ifile_append (ifile, "SEQ elsif_clauses = cmd_elsif*")
    call ifile_append (ifile, "SEQ cmd_elsif = elsif lexpr then command_list")
    call ifile_append (ifile, "SEQ else_clause = cmd_else?")
    call ifile_append (ifile, "SEQ cmd_else = else command_list")
    call ifile_append (ifile, "SEQ cmd_include = include include_arg")
    call ifile_append (ifile, "KEY include")
    call ifile_append (ifile, "ARG include_arg = ( string_literal )")
    call ifile_append (ifile, "SEQ cmd_quit = quit_cmd quit_arg?")
    call ifile_append (ifile, "ALT quit_cmd = quit | exit")
    call ifile_append (ifile, "KEY quit")
    call ifile_append (ifile, "KEY exit")
    call ifile_append (ifile, "ARG quit_arg = ( expr )")
    call ifile_append (ifile, "SEQ cmd_write_analysis = " &
         // "write_analysis_clause options?")
    call ifile_append (ifile, "SEQ cmd_compile_analysis = " &
         // "compile_analysis_clause options?")
    call ifile_append (ifile, "SEQ write_analysis_clause = " &
         // "write_analysis write_analysis_arg?")
    call ifile_append (ifile, "SEQ compile_analysis_clause = " &
         // "compile_analysis write_analysis_arg?")
    call ifile_append (ifile, "KEY write_analysis")
    call ifile_append (ifile, "KEY compile_analysis")
    call ifile_append (ifile, "ARG write_analysis_arg = ( analysis_tag* )")
    call ifile_append (ifile, "SEQ cmd_nlo = " &
                       // "nlo_calculation '=' nlo_calculation_list")
    call ifile_append (ifile, "KEY nlo_calculation")
    call ifile_append (ifile, "LIS nlo_calculation_list = sexpr ',' sexpr ',' sexpr")
    call define_expr_syntax (ifile, particles=.true., analysis=.true.)
  end subroutine define_cmd_list_syntax

  subroutine lexer_init_cmd_list (lexer, parent_lexer)
    type(lexer_t), intent(out) :: lexer
    type(lexer_t), intent(in), optional, target :: parent_lexer
    call lexer_init (lexer, &
         comment_chars = "#!", &
         quote_chars = '"', &
         quote_match = '"', &
         single_chars = "()[]{},;:&%?$@", &
         special_class = [ "+-*/^", "<>=~ " ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_cmd_list), &
         parent = parent_lexer)
  end subroutine lexer_init_cmd_list


end module commands
