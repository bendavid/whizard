! WHIZARD 2.0.6 Wed Dec 7 2011
! 
! Copyright (C) 1999-2011 by 
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

module commands

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
  use limits, only: HISTOGRAM_DATA_FORMAT !NODEP!
  use limits, only: DEFAULT_ANALYSIS_FILENAME !NODEP!
  use limits, only: FORBIDDEN_ENDINGS1 !NODEP!
  use limits, only: FORBIDDEN_ENDINGS2 !NODEP!
  use limits, only: FORBIDDEN_ENDINGS3 !NODEP!  
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use tao_random_numbers !NODEP!
  use md5
  use os_interface
  use ifiles
  use lexers
  use syntax_rules
  use parser
  use analysis
  use pdg_arrays
  use subevents
  use variables
  use expressions
  use models
  use state_matrices
  use flavors
  use quantum_numbers
  use polarizations
  use beams
  use strfun
  use mappings
  use phs_forests
  use cascades
  use process_libraries
  use processes
  use decays
  use events
  use slha_interface
  use cputime
  use iterations
  use beam_polarizations
  use strfun_config
  use event_files
  use user_files
  use rt_data
  use compilations
  use integrations
  use simulations

  implicit none
  private

  public :: command_list_t
  public :: command_list_final
  public :: command_list_compile
  public :: command_list_execute
  public :: syntax_cmd_list
  public :: syntax_cmd_list_init
  public :: syntax_cmd_list_final
  public :: syntax_cmd_list_write
  public :: lexer_init_cmd_list
  public :: command_test

  integer, parameter :: CMD_NONE = 0
  integer, parameter :: CMD_PROCESS = 1
  integer, parameter :: CMD_INTEGRATE = 2
  integer, parameter :: CMD_SIMULATE = 3
  integer, parameter :: CMD_RESCAN = 4
  integer, parameter :: CMD_ME_TEST = 5

  integer, parameter :: CMD_COMPILE = 10
  integer, parameter :: CMD_LOAD = 11
  integer, parameter :: CMD_EXEC = 13

  integer, parameter :: CMD_BEAMS = 21
  integer, parameter :: CMD_BEAM_POLARIZATION = 22

  integer, parameter :: CMD_MODEL = 31
  integer, parameter :: CMD_LIBRARY = 32
  integer, parameter :: CMD_CUTS = 33
  integer, parameter :: CMD_SCALE = 34
  integer, parameter :: CMD_FAC_SCALE = 35
  integer, parameter :: CMD_REN_SCALE = 36
  integer, parameter :: CMD_WEIGHT = 37
  integer, parameter :: CMD_SELECTION = 38
  integer, parameter :: CMD_REWEIGHT = 39

  integer, parameter :: CMD_VAR = 41
  integer, parameter :: CMD_SHOW = 45
  integer, parameter :: CMD_EXPECT = 46

  integer, parameter :: CMD_UNSTABLE = 51
  integer, parameter :: CMD_STABLE = 52
  integer, parameter :: CMD_POLARIZED = 53
  integer, parameter :: CMD_UNPOLARIZED = 54

  integer, parameter :: CMD_SEED = 63
  integer, parameter :: CMD_ITERATIONS = 64
  integer, parameter :: CMD_SAMPLE_FORMAT = 65

  integer, parameter :: CMD_CLEAR = 66

  integer, parameter :: CMD_ANALYSIS = 71
  integer, parameter :: CMD_OBSERVABLE = 72
  integer, parameter :: CMD_HISTOGRAM = 73
  integer, parameter :: CMD_PLOT = 74
  integer, parameter :: CMD_GRAPH = 75
  integer, parameter :: CMD_RECORD = 76
  integer, parameter :: CMD_WRITE_ANALYSIS = 77
  integer, parameter :: CMD_COMPILE_ANALYSIS = 78
  integer, parameter :: CMD_HISTOGRAM_WRITER = 79
  integer, parameter :: CMD_PLOT_WRITER = 80

  integer, parameter :: CMD_OPEN_OUT = 81
  integer, parameter :: CMD_CLOSE_OUT = 83
  integer, parameter :: CMD_PRINTD = 85
  integer, parameter :: CMD_PRINTF = 86
  
  integer, parameter :: CMD_INCLUDE = 91
  integer, parameter :: CMD_SCAN = 92
  integer, parameter :: CMD_IF = 93
  integer, parameter :: CMD_QUIT = 99

  integer, parameter :: CMD_SLHA = 101

  integer, parameter :: STEP_NONE = 0
  integer, parameter :: STEP_ADD = 1
  integer, parameter :: STEP_SUB = 2
  integer, parameter :: STEP_MUL = 3
  integer, parameter :: STEP_DIV = 4
  integer, parameter :: STEP_COMP_ADD = 11
  integer, parameter :: STEP_COMP_MUL = 13

  type :: command_t
     private
     integer :: type = CMD_NONE
     type(cmd_model_t), pointer :: model => null ()
     type(cmd_library_t), pointer :: library => null ()
     type(cmd_process_t), pointer :: process => null ()
     type(cmd_compile_t), pointer :: compile => null ()
     type(cmd_load_t), pointer :: load => null ()
     type(cmd_exec_t), pointer :: exec => null ()
     type(cmd_var_t), pointer :: var => null ()
     type(cmd_slha_t), pointer :: slha => null ()
     type(cmd_show_t), pointer :: show => null ()
     type(cmd_expect_t), pointer :: expect => null ()
     type(cmd_beams_t), pointer :: beams => null ()
     type(cmd_beam_polarization_t), pointer :: beam_polarization => null ()
     type(cmd_cuts_t), pointer :: cuts => null ()
     type(cmd_scale_t), pointer :: scale => null ()
     type(cmd_fac_scale_t), pointer :: fac_scale => null ()
     type(cmd_ren_scale_t), pointer :: ren_scale => null ()     
     type(cmd_weight_t), pointer :: weight => null ()
     type(cmd_selection_t), pointer :: selection => null ()
     type(cmd_reweight_t), pointer :: reweight => null ()
     type(cmd_seed_t), pointer :: seed => null ()
     type(cmd_iterations_t), pointer :: iterations => null ()
     type(cmd_integrate_t), pointer :: integrate => null ()
     type(cmd_me_test_t), pointer :: me_test => null ()
     type(cmd_observable_t), pointer :: observable => null ()
     type(cmd_histogram_t), pointer :: histogram => null ()
     type(cmd_plot_t), pointer :: plot => null ()
     type(cmd_graph_t), pointer :: graph => null ()
     type(cmd_clear_t), pointer :: clear => null ()
     type(cmd_record_t), pointer :: record => null ()
     type(cmd_analysis_t), pointer :: analysis => null ()
     type(cmd_write_analysis_t), pointer :: write_analysis => null ()
     type(cmd_write_analysis_t), pointer :: compile_analysis => null ()
     type(cmd_xxx_writer_t), pointer :: xxx_writer => null ()
     type(cmd_open_t), pointer :: open_out => null ()
     type(cmd_close_t), pointer :: close_out => null ()
     type(cmd_printd_t), pointer :: printd => null ()
     type(cmd_printf_t), pointer :: printf => null ()
     type(cmd_unstable_t), pointer :: unstable => null ()
     type(cmd_stable_t), pointer :: stable => null ()
     type(cmd_un_polarized_t), pointer :: un_polarized => null ()
     type(cmd_sample_format_t), pointer :: events => null ()
     type(cmd_simulate_t), pointer :: simulate => null ()
     type(cmd_rescan_t), pointer :: rescan => null ()
     type(cmd_scan_t), pointer :: loop => null ()
     type(cmd_if_t), pointer :: cond => null ()
     type(cmd_include_t), pointer :: include => null ()
     type(cmd_quit_t), pointer :: quit => null ()
     type(command_t), pointer :: next => null ()
  end type command_t

  type :: cmd_model_t
     private
     type(string_t) :: name
  end type cmd_model_t

  type :: cmd_library_t
     private
     type(string_t) :: name
  end type cmd_library_t

  type :: cmd_process_t
     private
     type(string_t) :: id
     integer :: n_in  = 0
     integer :: n_out = 0
     type(parse_node_p), dimension(:), allocatable :: pn_pdg_in
     type(parse_node_p), dimension(:), allocatable :: pn_pdg_out
     type(string_t), dimension(:), allocatable :: prt_in
     type(string_t), dimension(:), allocatable :: prt_out
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_process_t

  type :: cmd_compile_t
     private
     type(string_t), dimension(:), allocatable :: libname
     logical :: make_executable = .false.
     type(string_t) :: exec_name
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_compile_t

  type :: cmd_load_t
     private
     type(string_t), dimension(:), allocatable :: libname
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_load_t

  type :: cmd_exec_t
     private
     type(parse_node_t), pointer :: pn_command => null ()
  end type cmd_exec_t

  type :: cmd_var_t
     private
     type(string_t) :: name
     integer :: type = V_NONE
     type(parse_node_t), pointer :: pn_value => null ()
     logical :: is_intrinsic = .false.
     logical :: is_copy = .false.
  end type cmd_var_t

  type :: cmd_slha_t
     private
     type(string_t) :: file
     logical :: write = .false.
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_slha_t

  type :: cmd_show_t
     private
     type(string_t), dimension(:), allocatable :: name
     type(parse_node_p), dimension(:), allocatable :: pn_expr
     type(var_entry_t), dimension(:), allocatable :: value
  end type cmd_show_t

  type :: cmd_expect_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_expect_t

  type :: strfun_def_t
     private
     integer :: type = STRF_NONE
     type(command_list_t), pointer :: options => null ()
     type(parse_node_t), pointer :: pn_user_name => null ()
     type(rt_data_t) :: local
  end type strfun_def_t

  type :: strfun_pair_t
     private
     integer :: n
     type(strfun_def_t), dimension(2) :: def
  end type strfun_pair_t

  type :: cmd_beams_t
     private
     integer :: n_in = 0
     type(parse_node_p), dimension(:), allocatable :: pn_pdg
     type(string_t), dimension(:), allocatable :: prt
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
     integer :: n_strfun = 0
     type(strfun_pair_t), dimension(:), allocatable :: strfun_pair
  end type cmd_beams_t

  type :: bp_circ_data_t
     private
     type(parse_node_t), pointer :: pn_fraction => null ()
     real(default) :: fraction
  end type bp_circ_data_t

  type :: bp_trans_data_t
     private
     type(parse_node_t), pointer :: pn_fraction => null ()
     type(parse_node_t), pointer :: pn_phi => null ()
     real(default) :: fraction, phi
  end type bp_trans_data_t

  type :: bp_long_data_t
     private
     type(parse_node_t), pointer :: pn_fraction => null ()
     real(default) :: fraction
  end type bp_long_data_t

  type :: bp_axis_data_t
     private
     type(parse_node_t), pointer :: pn_fraction => null ()
     type(parse_node_t), pointer :: pn_theta => null ()
     type(parse_node_t), pointer :: pn_phi => null ()
     real(default) :: fraction, theta, phi
  end type bp_axis_data_t

  type :: bp_diag_data_t
     private
     type(parse_node_p), dimension(:), allocatable :: pn_hel
     type(parse_node_p), dimension(:), allocatable :: pn_fraction
     integer, dimension(:), allocatable :: hel
     real(default), dimension(:), allocatable :: fraction
  end type bp_diag_data_t

  type :: bp_density_data_t
     private
     type(parse_node_t), pointer :: pn_d => null ()
     type(parse_node_t), pointer :: pn_nd => null ()
     real(default) :: d
     complex(default) :: nd
  end type bp_density_data_t

  type :: cmd_beam_polarization_t
     private
     integer :: n = -1
     integer, dimension(2) :: type = -1
     type(bp_circ_data_t), dimension(:), pointer :: circ_data => null ()
     type(bp_trans_data_t), dimension(:), pointer :: trans_data => null ()
     type(bp_long_data_t), dimension(:), pointer :: long_data => null ()
     type(bp_axis_data_t), dimension(:), pointer :: axis_data => null ()
     type(bp_diag_data_t), dimension(:), pointer :: diag_data => null ()
     type(bp_density_data_t), dimension(:), pointer :: &
        density_data => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
     type(beam_polarization_t), dimension(:), pointer :: &
        beam_polarization => null ()
  end type cmd_beam_polarization_t

  type :: cmd_cuts_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
  end type cmd_cuts_t

  type :: cmd_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
  end type cmd_scale_t

  type :: cmd_fac_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
  end type cmd_fac_scale_t

  type :: cmd_ren_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()     
  end type cmd_ren_scale_t

  type :: cmd_weight_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_weight_t

  type :: cmd_selection_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_selection_t

  type :: cmd_reweight_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_reweight_t

  type :: cmd_integrate_t
     private
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_integrate_t
     
  type :: cmd_me_test_t
     private
     type(string_t) :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_me_test_t
     
  type :: cmd_observable_t
     private
     logical :: use_id_expr = .false.
     type(string_t) :: id
     type(parse_node_t), pointer :: pn_id => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_observable_t
     
  type :: cmd_histogram_t
     private
     type(string_t) :: id
     logical :: use_id_expr = .false.
     type(parse_node_t), pointer :: pn_id => null ()
     type(parse_node_t), pointer :: pn_lower_bound => null ()
     type(parse_node_t), pointer :: pn_upper_bound => null ()
     type(parse_node_t), pointer :: pn_bin_width => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_histogram_t
     
  type :: cmd_plot_t
     private
     type(string_t) :: id
     logical :: use_id_expr = .false.
     type(parse_node_t), pointer :: pn_id => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_plot_t
     
  type :: cmd_graph_t
     private
     type(string_t) :: id
     type(parse_node_t), pointer :: pn_id
     logical :: use_id_expr = .false.
     integer :: n_elements = 0
     type(cmd_plot_t), dimension(:), allocatable :: el
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_graph_t
     
  type :: analysis_id_t
    type(string_t) :: tag
    type(parse_node_t), pointer :: pn_sexpr => null ()
  end type analysis_id_t

  type :: cmd_analysis_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
  end type cmd_analysis_t

  type :: cmd_clear_t
     private
     integer :: n_args = 0
     type(string_t), dimension(:), allocatable :: id
     logical, dimension(:), allocatable :: use_id_expr
     type(parse_node_p), dimension(:), allocatable :: pn_id
  end type cmd_clear_t
     
  type :: cmd_write_analysis_t
    type(analysis_id_t), dimension(:), allocatable :: id
    type(string_t), dimension(:), allocatable :: tag
    type(rt_data_t) :: local
    type(command_list_t), pointer :: options => null ()
  end type cmd_write_analysis_t

  type :: cmd_xxx_writer_t
    type(parse_node_t), pointer :: macro => null ()
    integer :: type = CMD_HISTOGRAM_WRITER
  end type cmd_xxx_writer_t

  type :: cmd_open_t
     private
     type(parse_node_t), pointer :: pn_sexpr => null ()
     logical :: reading = .false.
     logical :: writing = .false.
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_open_t

  type :: cmd_close_t
     private
     type(parse_node_t), pointer :: pn_sexpr => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_close_t

  type :: cmd_printd_t
     private
     type(parse_node_t), pointer :: pn_sexpr => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_printd_t

  type :: cmd_printf_t
     private
     type(parse_node_t), pointer :: pn_sexpr => null ()
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_printf_t

  type :: cmd_record_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
  end type cmd_record_t
     
  type :: decay_properties_t
     type(parse_node_t), pointer :: pn_pdg => null ()
     type(string_t) :: prt
     type(flavor_t) :: flv
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     real(default), dimension(:), allocatable :: br
     logical :: invalid = .false.
  end type decay_properties_t

  type :: cmd_unstable_t
     private
     type(decay_properties_t), dimension(:), allocatable :: decay
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_unstable_t
     
  type :: cmd_stable_t
     private
     type(decay_properties_t), dimension(:), allocatable :: decay
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_stable_t
     
  type :: cmd_un_polarized_t
     private
     type(parse_node_p), dimension(:), allocatable :: pn_pdg
     type(string_t), dimension(:), allocatable :: name
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_un_polarized_t
  type :: cmd_sample_format_t
     private
     type(string_t), dimension(:), allocatable :: format
     integer, dimension(:), allocatable :: fmt
  end type cmd_sample_format_t

  type :: cmd_simulate_t
     private
     integer :: n_evt = 0
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_simulate_t

  type :: cmd_rescan_t
     private
     integer :: n_evt = 0
     integer :: n_proc = 0
     type(parse_node_t), pointer :: pn_filename => null ()
     type(string_t), dimension(:), allocatable :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_rescan_t

  type :: cmd_seed_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_seed_t

  type :: cmd_iterations_t
     private
     integer :: n_pass = 0
     type(parse_node_p), dimension(:), allocatable :: pn_expr_n_it
     type(parse_node_p), dimension(:), allocatable :: pn_expr_n_calls
     type(parse_node_p), dimension(:), allocatable :: pn_sexpr_adapt
  end type cmd_iterations_t

  type :: cmd_scan_t
     private
     integer :: var_type = V_NONE
     type(string_t) :: var_name
     logical :: allow_steps = .false.
     integer :: n_arg = 0
     type(command_list_t), dimension(:), pointer :: cmd_var => null ()
     logical, dimension(:), allocatable :: has_range
     type(parse_node_p), dimension(:), allocatable :: pn_beg_expr
     type(parse_node_p), dimension(:), allocatable :: pn_end_expr
     integer, dimension(:), allocatable :: step_type
     type(parse_node_p), dimension(:), allocatable :: pn_step_expr
     type(command_list_t), pointer :: body => null ()
     type(rt_data_t) :: local
  end type cmd_scan_t

  type :: cmd_if_t
     private
     type(parse_node_t), pointer :: pn_if_lexpr => null ()
     type(command_list_t), pointer :: if_body => null ()
     type(cmd_if_t), dimension(:), pointer :: elsif_cond => null ()
     type(command_list_t), pointer :: else_body => null ()
  end type cmd_if_t

  type :: cmd_include_t
     private
     type(string_t) :: file
     type(command_list_t), pointer :: command_list => null ()
     type(parse_tree_t) :: parse_tree
  end type cmd_include_t

  type :: cmd_quit_t
     private
     logical :: has_code = .false.
     type(parse_node_t), pointer :: pn_code_expr => null ()
  end type cmd_quit_t

  type :: command_list_t
     private
     type(command_t), pointer :: first => null ()
     type(command_t), pointer :: last => null ()
  end type command_list_t


  type(syntax_t), target, save :: syntax_cmd_list


contains

  recursive subroutine command_final (command)
    type(command_t), intent(inout) :: command
    select case (command%type)
    case (CMD_NONE)
    case (CMD_MODEL)
       deallocate (command%model)
    case (CMD_LIBRARY)
       deallocate (command%library)
    case (CMD_PROCESS)
       call cmd_process_final (command%process)
       deallocate (command%process)
    case (CMD_COMPILE)
       call cmd_compile_final (command%compile)
       deallocate (command%compile)
    case (CMD_LOAD)
       call cmd_load_final (command%load)
       deallocate (command%load)
    case (CMD_EXEC)
       call cmd_exec_final (command%exec)
       deallocate (command%exec)
    case (CMD_VAR)
       call cmd_var_final (command%var)
       deallocate (command%var)
    case (CMD_SLHA)
       call cmd_slha_final (command%slha)
       deallocate (command%slha)
    case (CMD_SHOW)
       call cmd_show_final (command%show)
       deallocate (command%show)
    case (CMD_EXPECT)
       call cmd_expect_final (command%expect)
       deallocate (command%expect)
    case (CMD_BEAMS)
       call cmd_beams_final (command%beams)
       deallocate (command%beams)
    case (CMD_BEAM_POLARIZATION)
       call cmd_beam_polarization_final (command%beam_polarization)
       deallocate (command%beam_polarization)
    case (CMD_CUTS)
       deallocate (command%cuts)
    case (CMD_SCALE)
       deallocate (command%scale)       
    case (CMD_FAC_SCALE)
       deallocate (command%fac_scale)
    case (CMD_REN_SCALE)
       deallocate (command%ren_scale)       
    case (CMD_WEIGHT)
       deallocate (command%weight)
    case (CMD_SELECTION)
       deallocate (command%selection)
    case (CMD_REWEIGHT)
       deallocate (command%reweight)
    case (CMD_SEED)
       call cmd_seed_final (command%seed)
       deallocate (command%seed)
    case (CMD_ITERATIONS)
       call cmd_iterations_final (command%iterations)
       deallocate (command%iterations)
    case (CMD_INTEGRATE)
       call cmd_integrate_final (command%integrate)
       deallocate (command%integrate)
    case (CMD_ME_TEST)
       call cmd_me_test_final (command%me_test)
       deallocate (command%me_test)
    case (CMD_OBSERVABLE)
       call cmd_observable_final (command%observable)
       deallocate (command%observable)
    case (CMD_HISTOGRAM)
       call cmd_histogram_final (command%histogram)
       deallocate (command%histogram)
    case (CMD_PLOT)
       call cmd_plot_final (command%plot)
       deallocate (command%plot)
    case (CMD_GRAPH)
       call cmd_graph_final (command%graph)
       deallocate (command%graph)
    case (CMD_CLEAR)
       call cmd_clear_final (command%clear)
       deallocate (command%clear)
    case (CMD_RECORD)
       call cmd_record_final (command%record)
       deallocate (command%record)
    case (CMD_ANALYSIS)
       deallocate (command%analysis)
    case (CMD_UNSTABLE)
       call cmd_unstable_final (command%unstable)
       deallocate (command%unstable)
    case (CMD_STABLE)
       call cmd_stable_final (command%stable)
       deallocate (command%stable)
    case (CMD_POLARIZED, CMD_UNPOLARIZED)
       call cmd_un_polarized_final (command%un_polarized)
       deallocate (command%un_polarized)
    case (CMD_SAMPLE_FORMAT)
       call cmd_sample_format_final (command%events)
       deallocate (command%events)
    case (CMD_SIMULATE)
       call cmd_simulate_final (command%simulate)
       deallocate (command%simulate)
    case (CMD_RESCAN)
       call cmd_rescan_final (command%rescan)
       deallocate (command%rescan)
    case (CMD_WRITE_ANALYSIS)
       call cmd_write_analysis_final (command%write_analysis)
       deallocate (command%write_analysis)
    case (CMD_COMPILE_ANALYSIS)
       call cmd_write_analysis_final (command%compile_analysis)
       deallocate (command%compile_analysis)
    case (CMD_HISTOGRAM_WRITER, CMD_PLOT_WRITER)
       call cmd_xxx_writer_final (command%xxx_writer)
    case (CMD_OPEN_OUT)
       call cmd_open_final (command%open_out)
    case (CMD_CLOSE_OUT)
       call cmd_close_final (command%close_out)
    case (CMD_PRINTD)
       call cmd_printd_final (command%printd)
       deallocate (command%printd)
    case (CMD_PRINTF)
       call cmd_printf_final (command%printf)
       deallocate (command%printf)
    case (CMD_SCAN)
       call cmd_scan_final (command%loop)
       deallocate (command%loop)
    case (CMD_IF)
       call cmd_if_final (command%cond)
       deallocate (command%cond)
    case (CMD_INCLUDE)
       call cmd_include_final (command%include)
       deallocate (command%include)
    case (CMD_QUIT)
       call cmd_quit_final (command%quit)
       deallocate (command%quit)
    end select
  end subroutine command_final

  recursive subroutine command_compile (command, pn, global)
    type(command_t), pointer :: command
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    ! call parse_node_write (pn)
    allocate (command)
    select case (char (parse_node_get_rule_key (pn)))
    case ("cmd_model")
       command%type = CMD_MODEL
       call cmd_model_compile (command%model, pn, global)
    case ("cmd_library")
       command%type = CMD_LIBRARY
       call cmd_library_compile (command%library, pn)
    case ("cmd_process")
       command%type = CMD_PROCESS
       call cmd_process_compile (command%process, pn, global)
    case ("cmd_compile")
       command%type = CMD_COMPILE
       call cmd_compile_compile (command%compile, pn, global)
    case ("cmd_load")
       command%type = CMD_LOAD
       call cmd_load_compile (command%load, pn, global)
    case ("cmd_exec")
       command%type = CMD_EXEC
       call cmd_exec_compile (command%exec, pn, global)
    case ("cmd_num", "cmd_complex", "cmd_real", "cmd_int", &
          "cmd_log_decl", "cmd_log", "cmd_string", "cmd_string_decl", &
          "cmd_alias", "cmd_result")
       command%type = CMD_VAR
       call cmd_var_compile (command%var, pn, global)
    case ("cmd_slha")
       command%type = CMD_SLHA
       call cmd_slha_compile (command%slha, pn, global)
    case ("cmd_show")
       command%type = CMD_SHOW
       call cmd_show_compile (command%show, pn, global)
    case ("cmd_expect")
       command%type = CMD_EXPECT
       call cmd_expect_compile (command%expect, pn, global)
    case ("cmd_beams")
       command%type = CMD_BEAMS
       call cmd_beams_compile (command%beams, pn, global)
    case ("cmd_beam_polarization")
       command%type = CMD_BEAM_POLARIZATION
       call cmd_beam_polarization_compile &
          (command%beam_polarization, pn, global)
    case ("cmd_cuts")
       command%type = CMD_CUTS
       call cmd_cuts_compile (command%cuts, pn)
    case ("cmd_scale")
       command%type = CMD_SCALE
       call cmd_scale_compile (command%scale, pn)       
    case ("cmd_fac_scale")
       command%type = CMD_FAC_SCALE
       call cmd_fac_scale_compile (command%fac_scale, pn)
    case ("cmd_ren_scale")
       command%type = CMD_REN_SCALE
       call cmd_ren_scale_compile (command%ren_scale, pn)       
    case ("cmd_weight")
       command%type = CMD_WEIGHT
       call cmd_weight_compile (command%weight, pn)
    case ("cmd_selection")
       command%type = CMD_SELECTION
       call cmd_selection_compile (command%selection, pn)
    case ("cmd_reweight")
       command%type = CMD_REWEIGHT
       call cmd_reweight_compile (command%reweight, pn)
    case ("cmd_seed")
       command%type = CMD_SEED
       call cmd_seed_compile (command%seed, pn, global)
    case ("cmd_iterations")
       command%type = CMD_ITERATIONS
       call cmd_iterations_compile (command%iterations, pn, global)
    case ("cmd_integrate")
       command%type = CMD_INTEGRATE
       call cmd_integrate_compile (command%integrate, pn, global)
    case ("cmd_me_test")
       command%type = CMD_ME_TEST
       call cmd_me_test_compile (command%me_test, pn, global)
    case ("cmd_observable")
       command%type = CMD_OBSERVABLE
       call cmd_observable_compile (command%observable, pn, global)
    case ("cmd_histogram")
       command%type = CMD_HISTOGRAM
       call cmd_histogram_compile (command%histogram, pn, global)
    case ("cmd_plot")
       command%type = CMD_PLOT
       call cmd_plot_compile (command%plot, pn, global)
    case ("cmd_graph")
       command%type = CMD_GRAPH
       call cmd_graph_compile (command%graph, pn, global)
    case ("cmd_clear")
       command%type = CMD_CLEAR
       call cmd_clear_compile (command%clear, pn, global)
    case ("cmd_record")
       command%type = CMD_RECORD
       call cmd_record_compile (command%record, pn, global)
    case ("cmd_analysis")
       command%type = CMD_ANALYSIS
       call cmd_analysis_compile (command%analysis, pn)
    case ("cmd_unstable")
       command%type = CMD_UNSTABLE
       call cmd_unstable_compile (command%unstable, pn, global)
    case ("cmd_stable")
       command%type = CMD_STABLE
       call cmd_stable_compile (command%stable, pn, global)
    case ("cmd_polarized")
         command%type = CMD_POLARIZED
         call cmd_un_polarized_compile (command%un_polarized, pn, global)
    case ("cmd_unpolarized")
         command%type = CMD_UNPOLARIZED
         call cmd_un_polarized_compile (command%un_polarized, pn, global)
    case ("cmd_sample_format")
       command%type = CMD_SAMPLE_FORMAT
       call cmd_sample_format_compile (command%events, pn)
    case ("cmd_simulate")
       command%type = CMD_SIMULATE
       call cmd_simulate_compile (command%simulate, pn, global)
    case ("cmd_rescan")
       command%type = CMD_RESCAN
       call cmd_rescan_compile (command%rescan, pn, global)
    case ("cmd_write_analysis")
       command%type = CMD_WRITE_ANALYSIS
       call cmd_write_analysis_compile (command%write_analysis, &
          pn, global)
    case ("cmd_compile_analysis")
       command%type = CMD_COMPILE_ANALYSIS
       call cmd_write_analysis_compile (command%compile_analysis, pn, global)
    case ("cmd_histogram_writer", "cmd_plot_writer")
       command%type = CMD_HISTOGRAM_WRITER
       call cmd_xxx_writer_compile (command%xxx_writer, pn, global)
    case ("cmd_open_out")
       command%type = CMD_OPEN_OUT
       call cmd_open_out_compile (command%open_out, pn, global)
    case ("cmd_close_out")
       command%type = CMD_CLOSE_OUT
       call cmd_close_out_compile (command%close_out, pn, global)
    case ("cmd_printd")
       command%type = CMD_PRINTD
       call cmd_printd_compile (command%printd, pn, global)
    case ("cmd_printf")
       command%type = CMD_PRINTF
       call cmd_printf_compile (command%printf, pn, global)
    case ("cmd_scan")
       command%type = CMD_SCAN
       call cmd_scan_compile (command%loop, pn, global)
    case ("cmd_if")
       command%type = CMD_IF
       call cmd_if_compile (command%cond, pn, global)
    case ("cmd_include")
       command%type = CMD_INCLUDE
       call cmd_include_compile (command%include, pn, global)
    case ("cmd_quit")
       command%type = CMD_QUIT
       call cmd_quit_compile (command%quit, pn, global)
    case default
       print *, char (parse_node_get_rule_key (pn))
       call msg_bug ("Command not implemented")
    end select
    ! call command_write (command)
  end subroutine command_compile

  recursive subroutine command_execute (command, global)
    type(command_t), intent(inout) :: command
    type(rt_data_t), intent(inout), target :: global
    select case (command%type)
    case (CMD_MODEL)
       call cmd_model_execute (command%model, global)
    case (CMD_LIBRARY)
       call cmd_library_execute (command%library, global)
    case (CMD_PROCESS)
       call cmd_process_execute (command%process, global)
    case (CMD_COMPILE)
       call cmd_compile_execute (command%compile, global)
    case (CMD_LOAD)
       call cmd_load_execute (command%load, global)
    case (CMD_EXEC)
       call cmd_exec_execute (command%exec, global)
    case (CMD_VAR)
       call cmd_var_execute (command%var, global)
    case (CMD_SLHA)
       call cmd_slha_execute (command%slha, global)
    case (CMD_SHOW)
       call cmd_show_execute (command%show, global)
    case (CMD_EXPECT)
       call cmd_expect_execute (command%expect, global)
    case (CMD_BEAMS)
       call cmd_beams_execute (command%beams, global)
    case (CMD_BEAM_POLARIZATION)
       call cmd_beam_polarization_execute (command%beam_polarization, global)
    case (CMD_CUTS)
       call cmd_cuts_execute (command%cuts, global)
    case (CMD_SCALE)
       call cmd_scale_execute (command%scale, global)       
    case (CMD_FAC_SCALE)
       call cmd_fac_scale_execute (command%fac_scale, global)
    case (CMD_REN_SCALE)
       call cmd_ren_scale_execute (command%ren_scale, global)       
    case (CMD_WEIGHT)
       call cmd_weight_execute (command%weight, global)
    case (CMD_SELECTION)
       call cmd_selection_execute (command%selection, global)
    case (CMD_REWEIGHT)
       call cmd_reweight_execute (command%reweight, global)
    case (CMD_SEED)
       call cmd_seed_execute (command%seed, global)
    case (CMD_ITERATIONS)
       call cmd_iterations_execute (command%iterations, global)
    case (CMD_INTEGRATE)
       call cmd_integrate_execute (command%integrate, global)
    case (CMD_ME_TEST)
       call cmd_me_test_execute (command%me_test, global)
    case (CMD_OBSERVABLE)
       call cmd_observable_execute (command%observable, global)
    case (CMD_HISTOGRAM)
       call cmd_histogram_execute (command%histogram, global)
    case (CMD_PLOT)
       call cmd_plot_execute (command%plot, global)
    case (CMD_GRAPH)
       call cmd_graph_execute (command%graph, global)
    case (CMD_CLEAR)
       call cmd_clear_execute (command%clear, global)
    case (CMD_RECORD)
       call cmd_record_execute (command%record, global)
    case (CMD_ANALYSIS)
       call cmd_analysis_execute (command%analysis, global)
    case (CMD_UNSTABLE)
       call cmd_unstable_execute (command%unstable, global)
    case (CMD_STABLE)
       call cmd_stable_execute (command%stable, global)
    case (CMD_POLARIZED, CMD_UNPOLARIZED)
       call cmd_un_polarized_execute &
            (command%un_polarized, command%type, global)
    case (CMD_SAMPLE_FORMAT)
       call cmd_sample_format_execute (command%events, global)
    case (CMD_SIMULATE)
       call cmd_simulate_execute (command%simulate, global)
    case (CMD_RESCAN)
       call cmd_rescan_execute (command%rescan, global)
    case (CMD_WRITE_ANALYSIS)
       call cmd_write_analysis_execute (command%write_analysis, global)
    case (CMD_COMPILE_ANALYSIS)
       call cmd_compile_analysis_execute (command%compile_analysis, global)
    case (CMD_HISTOGRAM_WRITER, CMD_PLOT_WRITER)
       call cmd_xxx_writer_execute (command%xxx_writer, global)
    case (CMD_OPEN_OUT)
       call cmd_open_execute (command%open_out, global)
    case (CMD_CLOSE_OUT)
       call cmd_close_execute (command%close_out, global)
    case (CMD_PRINTD)
       call cmd_printd_execute (command%printd, global)
    case (CMD_PRINTF)
       call cmd_printf_execute (command%printf, global)
    case (CMD_SCAN)
       call cmd_scan_execute (command%loop, global)
    case (CMD_IF)
       call cmd_if_execute (command%cond, global)
    case (CMD_INCLUDE)
       call cmd_include_execute (command%include, global)
    case (CMD_QUIT)
       call cmd_quit_execute (command%quit, global)
    end select
  end subroutine command_execute
  
  subroutine write_indent (unit, indent)
    integer, intent(in) :: unit
    integer, intent(in), optional :: indent
    if (present (indent)) then
       write (unit, "(A)", advance="no")  repeat ("  ", indent)
    end if
  end subroutine write_indent

  subroutine cmd_model_write (model, unit, indent)
    type(cmd_model_t), intent(in) :: model
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,'""',A,'""')")  "model =", char (model%name)
  end subroutine cmd_model_write

  subroutine cmd_model_compile (model, pn, global)
    type(cmd_model_t), pointer :: model
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_name
    type(model_t), pointer :: mdl
    type(string_t) :: filename
    pn_name => parse_node_get_sub_ptr (pn, 3)
    allocate (model)
    if (associated (pn_name)) then
       model%name = parse_node_get_string (pn_name)
       filename = model%name // ".mdl"
       mdl => null ()
       call model_list_read_model (model%name, filename, global%os_data, mdl)
       if (associated (mdl)) then
          call var_list_init_copies &
               (global%var_list, model_get_var_list_ptr (mdl))
       end if
    else
       model%name = ""
    end if
  end subroutine cmd_model_compile

  subroutine cmd_model_execute (model, global)
    type(cmd_model_t), intent(in) :: model
    type(rt_data_t), intent(inout), target :: global
    type(model_t), pointer :: mdl
    type(var_list_t), pointer :: model_vars
    if (model_get_name (global%model) /= model%name) then
       if (model_list_model_exists (model%name)) then
          mdl => model_list_get_model_ptr (model%name)
          global%model => mdl
          call var_list_set_string (global%var_list, var_str ("$model_name"), &
               model%name, is_known=.true.)
          call msg_message ("Switching to model '" &
               // char (model_get_name (mdl)) &
               // "': reassigning model parameters")
          model_vars => model_get_var_list_ptr (mdl)
          call var_list_synchronize &
               (global%var_list, model_vars, reset_pointers = .true.)
       end if
    else
       model_vars => model_get_var_list_ptr (global%model)
       call var_list_synchronize &
            (global%var_list, model_vars, reset_pointers = .false.)
    end if
  end subroutine cmd_model_execute

  subroutine cmd_library_write (library, unit, indent)
    type(cmd_library_t), intent(in) :: library
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,'""',A,'""')")  "library =", char (library%name)
  end subroutine cmd_library_write

  subroutine cmd_library_compile (library, pn)
    type(cmd_library_t), pointer :: library
    type(parse_node_t), intent(in), target :: pn
    type(parse_node_t), pointer :: pn_name
    pn_name => parse_node_get_sub_ptr (pn, 3)
    allocate (library)
    library%name = parse_node_get_string (pn_name)
  end subroutine cmd_library_compile

  subroutine cmd_library_execute (library, global)
    type(cmd_library_t), intent(in) :: library
    type(rt_data_t), intent(inout), target :: global
    logical :: rebuild_library, recompile_library
    rebuild_library = &
         var_list_get_lval (global%var_list, var_str ("?rebuild_library"))
    recompile_library = &
         var_list_get_lval (global%var_list, var_str ("?recompile_library"))
    if (.not. (rebuild_library .or. recompile_library .or. &
               associated (global%prc_lib))) then
       call load_library (library%name, global, global%var_list, global%prc_lib)
    else
       call process_library_store_append &
            (library%name, global%os_data, global%prc_lib)
       call var_list_set_string (global%var_list, var_str ("$library_name"), &
            process_library_get_name (global%prc_lib), is_known=.true.)
    end if
  end subroutine cmd_library_execute

  subroutine cmd_process_final (process)
    type(cmd_process_t), intent(inout) :: process
    if (allocated (process%pn_pdg_in)) then
       deallocate (process%pn_pdg_in)
    end if
    if (allocated (process%pn_pdg_out)) then
       deallocate (process%pn_pdg_out)
    end if
    if (associated (process%options)) then
       call command_list_final (process%options)
       deallocate (process%options)
    end if
  end subroutine cmd_process_final

  subroutine cmd_process_compile (process, pn, global)
    type(cmd_process_t), pointer :: process
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_id, pn_in, pn_out, pn_codes, pn_opt
    integer :: i
    pn_id => parse_node_get_sub_ptr (pn, 2)
    pn_in  => parse_node_get_next_ptr (pn_id, 2)
    pn_out => parse_node_get_next_ptr (pn_in, 2)
    pn_opt => parse_node_get_next_ptr (pn_out)
    allocate (process)
    call rt_data_local_init (process%local, global)
    if (associated (pn_opt)) then 
       allocate (process%options)
       call command_list_compile (process%options, pn_opt, process%local)
    end if
    process%id = parse_node_get_string (pn_id)
    call process_library_check_name_consistency (process%id, global%prc_lib)
    process%n_in  = parse_node_get_n_sub (pn_in)
    process%n_out = parse_node_get_n_sub (pn_out)
    pn_codes => parse_node_get_sub_ptr (pn_in)
    allocate (process%pn_pdg_in (process%n_in))
    allocate (process%prt_in (process%n_in))
    do i = 1, process%n_in
       process%pn_pdg_in(i)%ptr => pn_codes
       process%prt_in(i) = "?"
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
    pn_codes => parse_node_get_sub_ptr (pn_out)
    allocate (process%pn_pdg_out (process%n_out))
    allocate (process%prt_out (process%n_out))
    do i = 1, process%n_out
       process%pn_pdg_out(i)%ptr => pn_codes
       process%prt_out(i) = "?"
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
    call var_list_init_num_id (global%var_list, process%id)
    call rt_data_local_reset (process%local)
  end subroutine cmd_process_compile

  subroutine cmd_process_execute (process, global)
    type(cmd_process_t), intent(inout), target :: process
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: pdg_in, pdg_out
    integer :: i, method
    logical :: rebuild_library, omega_openmp
    type(string_t) :: restrictions, omega_flags, method_str    
    call rt_data_link (process%local, global)
    if (associated (process%options)) then
       call command_list_execute (process%options, process%local)
    end if
    if (process_library_is_static (global%prc_lib)) then
       call msg_error ("Process library '" &
            // char (process_library_get_name (global%prc_lib)) &
            // "' is static, no processes can be added")
       return
    end if
    do i = 1, size (process%pn_pdg_in)
       pdg_in = &
            eval_pdg_array (process%pn_pdg_in(i)%ptr, process%local%var_list)
       process%prt_in(i) = make_flavor_string (pdg_in, process%local%model)
    end do
    do i = 1, size (process%pn_pdg_out)
       pdg_out = &
            eval_pdg_array (process%pn_pdg_out(i)%ptr, process%local%var_list)
       process%prt_out(i) = make_flavor_string (pdg_out, process%local%model)
    end do
    restrictions = var_list_get_sval &
         (process%local%var_list, var_str ("$restrictions"))
    omega_flags = var_list_get_sval &
         (process%local%var_list, var_str ("$omega_flags"))
    method_str = var_list_get_sval &
         (process%local%var_list, var_str ("$method"))
    omega_openmp = var_list_get_lval &
         (process%local%var_list, var_str ("?omega_openmp"))
    method = method_of_string (method_str)
    if (all (scan (process%prt_in, "?") == 0) .and. &
         all (scan (process%prt_out, "?") == 0)) then
       rebuild_library = var_list_get_lval &
            (process%local%var_list, var_str ("?rebuild_library"))
       call process_library_append (global%prc_lib, &
            process%id, process%local%model, &
            process%prt_in, process%prt_out, &
            method = method, &
            restrictions = restrictions, omega_flags = omega_flags, &
            rebuild_library = rebuild_library, message = .true., &
            omega_openmp = omega_openmp)
    else
       call msg_error ("Broken process declaration: skipped")
    end if
    call var_list_init_num_id (global%var_list, process%id)
    call rt_data_restore (global, process%local)
  end subroutine cmd_process_execute

  function make_flavor_string (aval, model) result (prt)
    type(string_t) :: prt
    type(pdg_array_t), intent(in) :: aval
    type(model_t), intent(in), target :: model
    integer, dimension(:), allocatable :: pdg
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    pdg = aval
    allocate (flv (size (pdg)))
    call flavor_init (flv, pdg, model)
    if (size (pdg) /= 0) then
       prt = flavor_get_name (flv(1))
       do i = 2, size (flv)
          prt = prt // ":" // flavor_get_name (flv(i))
       end do
    else
       prt = "?"
    end if
  end function make_flavor_string

  function method_of_string (meth_str) result (meth_id)
    type(string_t), intent(in) :: meth_str
    integer :: meth_id
    select case (char (meth_str))
    case ("omega")
       meth_id = PRC_OMEGA
    case ("test")
       meth_id = PRC_TEST
    case ("unit")
       meth_id = PRC_UNIT
    case ("external")
       meth_id = PRC_EXTERNAL
    case ("dipole")
       meth_id = PRC_DIPOLE
    case default
       call msg_fatal ("Invalid method for matrix elements.")
    end select
  end function method_of_string
  
  subroutine cmd_compile_final (compile)
    type(cmd_compile_t), intent(inout) :: compile
    if (associated (compile%options)) then
       call command_list_final (compile%options)
       deallocate (compile%options)
    end if
  end subroutine cmd_compile_final

  subroutine cmd_compile_compile (compile, pn, global)
    type(cmd_compile_t), pointer :: compile
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_clause, pn_arg, pn_lib, pn_opt
    type(parse_node_t), pointer :: pn_exec_name_spec, pn_exec_name
    type(process_library_t), pointer :: prc_lib
    integer :: n_lib, i
    pn_cmd => parse_node_get_sub_ptr (pn)
    pn_clause => parse_node_get_sub_ptr (pn_cmd)
    pn_exec_name_spec => parse_node_get_sub_ptr (pn_clause, 2)
    if (associated (pn_exec_name_spec)) then
       pn_exec_name => parse_node_get_sub_ptr (pn_exec_name_spec, 2)
    else
       pn_exec_name => null ()
    end if
    pn_arg => parse_node_get_next_ptr (pn_clause)
    pn_opt => parse_node_get_next_ptr (pn_cmd)
    allocate (compile)
    call rt_data_local_init (compile%local, global)
    if (associated (pn_opt)) then
       allocate (compile%options)
       call command_list_compile (compile%options, pn_opt, compile%local)
    end if
    if (associated (pn_arg)) then
       n_lib = parse_node_get_n_sub (pn_arg)
    else
       n_lib = 0
    end if
    if (n_lib > 0) then
       allocate (compile%libname (n_lib))
       pn_lib => parse_node_get_sub_ptr (pn_arg)
       do i = 1, n_lib
          compile%libname(i) = parse_node_get_string (pn_lib)
          pn_lib => parse_node_get_next_ptr (pn_lib)
       end do
    else
       n_lib = 0
       prc_lib => process_library_store_get_first ()
       do while (associated (prc_lib))
          n_lib = n_lib + 1
          call process_library_advance (prc_lib)
       end do
       allocate (compile%libname (n_lib))
       i = 0
       prc_lib => process_library_store_get_first ()
       do while (associated (prc_lib))
          i = i + 1
          compile%libname(i) = process_library_get_name (prc_lib)
          call process_library_advance (prc_lib)
       end do
    end if
    if (associated (pn_exec_name)) then
       compile%make_executable = .true.
       compile%exec_name = parse_node_get_string (pn_exec_name)
    end if
    call rt_data_local_reset (compile%local)
  end subroutine cmd_compile_compile

  subroutine cmd_compile_execute (compile, global)
    type(cmd_compile_t), intent(inout), target :: compile
    type(rt_data_t), intent(inout), target :: global
    call rt_data_link (compile%local, global)
    if (associated (compile%options)) then
       call command_list_execute (compile%options, compile%local)
    end if
    if (compile%make_executable) then
       call compile_executable &
            (compile%libname, compile%exec_name, compile%local)
    else
       call compile_library &
            (compile%libname, compile%local, global%var_list, global%prc_lib)
    end if
    call rt_data_restore (global, compile%local)
  end subroutine cmd_compile_execute
    
  subroutine cmd_load_final (load)
    type(cmd_load_t), intent(inout) :: load
    if (associated (load%options)) then
       call command_list_final (load%options)
       deallocate (load%options)
    end if
  end subroutine cmd_load_final

  subroutine cmd_load_compile (load, pn, global)
    type(cmd_load_t), pointer :: load
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_lib, pn_opt
    type(process_library_t), pointer :: prc_lib
    integer :: n_lib, i
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    allocate (load)
    if (associated (pn_arg)) then
       select case (char (parse_node_get_rule_key (pn_arg)))
       case ("load_arg")
          n_lib = parse_node_get_n_sub (pn_arg)
          pn_opt => parse_node_get_next_ptr (pn_arg)
       case default
          n_lib = 0
          pn_opt => pn_arg
       end select
    else
       n_lib = 0
       pn_opt => null ()
    end if
    call rt_data_local_init (load%local, global)
    if (associated (pn_opt)) then
       allocate (load%options)
       call command_list_compile (load%options, pn_opt, load%local)
    end if
    if (n_lib > 0) then
       allocate (load%libname (n_lib))
       pn_lib => parse_node_get_sub_ptr (pn_arg)
       do i = 1, n_lib
          load%libname(i) = parse_node_get_string (pn_lib)
          pn_lib => parse_node_get_next_ptr (pn_lib)
       end do
    else
       n_lib = 0
       prc_lib => process_library_store_get_first ()
       do while (associated (prc_lib))
          n_lib = n_lib + 1
          call process_library_advance (prc_lib)
       end do
       allocate (load%libname (n_lib))
       i = 0
       prc_lib => process_library_store_get_first ()
       do while (associated (prc_lib))
          i = i + 1
          load%libname(i) = process_library_get_name (prc_lib)
          call process_library_advance (prc_lib)
       end do
    end if
    call rt_data_local_reset (load%local)
  end subroutine cmd_load_compile

  subroutine cmd_load_execute (load, global)
    type(cmd_load_t), intent(inout) :: load
    type(rt_data_t), intent(inout), target :: global
    call rt_data_link (load%local, global)
    if (associated (load%options)) then
       call command_list_execute (load%options, load%local)
    end if
    call load_library &
         (load%libname, load%local, global%var_list, global%prc_lib)
    call rt_data_restore (global, load%local)
  end subroutine cmd_load_execute

  subroutine cmd_exec_final (exec)
    type(cmd_exec_t), intent(inout) :: exec
  end subroutine cmd_exec_final

  subroutine cmd_exec_compile (exec, pn, global)
    type(cmd_exec_t), pointer :: exec
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_command
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    pn_command => parse_node_get_sub_ptr (pn_arg)
    allocate (exec)
    exec%pn_command => pn_command
  end subroutine cmd_exec_compile

  subroutine cmd_exec_execute (exec, global)
    type(cmd_exec_t), intent(inout) :: exec
    type(rt_data_t), intent(in) :: global
    type(string_t) :: command
    logical :: is_known
    integer :: status
    command = eval_string (exec%pn_command, global%var_list, is_known=is_known)
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

  subroutine cmd_var_final (var)
    type(cmd_var_t), intent(inout) :: var
  end subroutine cmd_var_final

  subroutine cmd_var_compile (var, pn, global)
    type(cmd_var_t), pointer :: var
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_var, pn_name
    type(parse_node_t), pointer :: pn_result, pn_proc
    type(string_t) :: var_name
    type(var_entry_t), pointer :: var_entry
    integer :: type
    logical :: new
    pn_result => null ()
    new = .false.
    allocate (var)
    select case (char (parse_node_get_rule_key (pn)))
    case ("cmd_log_decl");    type = V_LOG
       pn_var => parse_node_get_sub_ptr (pn, 2)
       if (.not. associated (pn_var)) then   ! handle masked syntax error 
          var%type = V_NONE; return
       end if
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       new = .true.
    case ("cmd_log");         type = V_LOG
       pn_name => parse_node_get_sub_ptr (pn, 2)
    case ("cmd_int");         type = V_INT
       pn_name => parse_node_get_sub_ptr (pn, 2)
       new = .true.
    case ("cmd_real");        type = V_REAL
       pn_name => parse_node_get_sub_ptr (pn, 2)
       new = .true.
    case ("cmd_complex");       type = V_CMPLX
       pn_name => parse_node_get_sub_ptr (pn, 2)
       new = .true.
    case ("cmd_num");         type = V_NONE
       pn_name => parse_node_get_sub_ptr (pn)
    case ("cmd_string_decl"); type = V_STR
       pn_var => parse_node_get_sub_ptr (pn, 2)
       if (.not. associated (pn_var)) then   ! handle masked syntax error 
          var%type = V_NONE; return
       end if
       pn_name => parse_node_get_sub_ptr (pn_var, 2)
       new = .true.
    case ("cmd_string");      type = V_STR
       pn_name => parse_node_get_sub_ptr (pn, 2)
    case ("cmd_alias");       type = V_PDG
       pn_name => parse_node_get_sub_ptr (pn, 2)
       new = .true.
    case ("cmd_result");      type = V_REAL
       pn_name => parse_node_get_sub_ptr (pn)
       pn_result => parse_node_get_sub_ptr (pn_name)
       pn_proc => parse_node_get_next_ptr (pn_result)
    case default
       call parse_node_mismatch &
            ("logical|int|real|complex|?|$|alias|var_name", pn)  ! $
    end select
    if (.not. associated (pn_name)) then   ! handle masked syntax error 
       var%type = V_NONE; return
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
    call var_list_check_user_var (global%var_list, var_name, type, new)
    var%type = type
    var%name = var_name
    var_entry => var_list_get_var_ptr &
       (global%var_list, var%name, var%type, follow_link=.false.)
    var%pn_value => parse_node_get_next_ptr (pn_name, 2)
    if (.not. associated (var%pn_value))  var%type = V_NONE
    if (associated (var_entry)) then
       var%is_copy = var_entry_is_copy (var_entry)
    else
       var_entry => var_list_get_var_ptr &
          (global%var_list, var%name, var%type, follow_link=.true.)
       if (associated (var_entry)) then
          var%is_intrinsic = var_entry_is_intrinsic (var_entry)
          if (var_entry_is_copy (var_entry)) then
             var%is_copy = .true.
             call var_list_init_copy (global%var_list, var_entry, user=.true.)
          end if
       end if
       if (.not. var%is_copy) then
          select case (var%type)
          case (V_LOG)
             call var_list_append_log (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          case (V_INT)
             call var_list_append_int (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          case (V_REAL)
             call var_list_append_real (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          case (V_CMPLX)
             call var_list_append_cmplx (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          case (V_PDG)
             call var_list_append_pdg_array (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          case (V_STR)
             call var_list_append_string (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic, user=.true.)
          end select
       end if
    end if
  end subroutine cmd_var_compile

  subroutine cmd_var_execute (var, global)
    type(cmd_var_t), intent(inout), target :: var
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: model_name
    type(var_list_t), pointer :: model_vars
    if (associated (global%model)) then
       model_name = model_get_name (global%model)
       model_vars => model_get_var_list_ptr (global%model)
       if (var%is_copy) then
          call var_list_set_original_pointer (global%var_list, var%name, &
               model_vars)
       end if
       call cmd_var_set_value (var, global%var_list, &
            verbose=.true., model_name=model_name)
       if (var%is_copy) then
          call model_parameters_update (global%model)
          call var_list_synchronize (global%var_list, model_vars)
       end if
    else
       call cmd_var_set_value (var, global%var_list, verbose=.true.)
    end if
  end subroutine cmd_var_execute

  subroutine cmd_var_set_value (var, var_list, verbose, model_name)
    type(cmd_var_t), intent(inout), target :: var
    type(var_list_t), intent(inout), target :: var_list
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    logical :: lval
    integer :: ival
    real(default) :: rval
    complex(default) :: cval
    type(pdg_array_t) :: aval
    type(string_t) :: sval
    logical :: is_known
    select case (var%type)
    case (V_LOG)
       lval = eval_log (var%pn_value, var_list, is_known=is_known)
       call var_list_set_log (var_list, var%name, &
            lval, is_known, verbose=verbose, model_name=model_name)
    case (V_INT)
       ival = eval_int (var%pn_value, var_list, is_known=is_known)
       call var_list_set_int (var_list, var%name, &
            ival, is_known, verbose=verbose, model_name=model_name)
    case (V_REAL)
       rval = eval_real (var%pn_value, var_list, is_known=is_known)
       call var_list_set_real (var_list, var%name, &
            rval, is_known, verbose=verbose, model_name=model_name)
    case (V_CMPLX)
       cval = eval_cmplx (var%pn_value, var_list, is_known=is_known)
       call var_list_set_cmplx (var_list, var%name, &
            cval, is_known, verbose=verbose, model_name=model_name)
    case (V_PDG)
       aval = eval_pdg_array (var%pn_value, var_list, is_known=is_known)
       call var_list_set_pdg_array (var_list, var%name, &
            aval, is_known, verbose=verbose, model_name=model_name)
    case (V_STR)
       sval = eval_string (var%pn_value, var_list, is_known=is_known)
       call var_list_set_string (var_list, var%name, &
            sval, is_known, verbose=verbose, model_name=model_name)
    end select
  end subroutine cmd_var_set_value
  
  subroutine cmd_slha_final (slha)
    type(cmd_slha_t), intent(inout) :: slha
    if (associated (slha%options)) then
       call command_list_final (slha%options)
       deallocate (slha%options)
    end if
  end subroutine cmd_slha_final

  subroutine cmd_slha_compile (slha, pn, global)
    type(cmd_slha_t), pointer :: slha
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_key, pn_arg, pn_file
    type(parse_node_t), pointer :: pn_opt
    pn_key => parse_node_get_sub_ptr (pn)
    pn_arg => parse_node_get_next_ptr (pn_key)
    pn_file => parse_node_get_sub_ptr (pn_arg)
    pn_opt => parse_node_get_next_ptr (pn_arg)
    allocate (slha)
    call rt_data_local_init (slha%local, global)
    if (associated (pn_opt)) then
       allocate (slha%options)
       call command_list_compile (slha%options, pn_opt, slha%local)
    end if
    select case (char (parse_node_get_key (pn_key)))
    case ("read_slha")
       slha%write = .false.
    case ("write_slha")
       slha%write = .true.
    case default
       call parse_node_mismatch ("read_slha|write_slha",  pn)
    end select
    slha%file = parse_node_get_string (pn_file)
    call rt_data_local_reset (slha%local)
  end subroutine cmd_slha_compile

  subroutine cmd_slha_execute (slha, global)
    type(cmd_slha_t), intent(inout), target :: slha
    type(rt_data_t), intent(inout), target :: global
    logical :: input, spectrum, decays
    call rt_data_link (slha%local, global)
    if (associated (slha%options)) then
       call command_list_execute (slha%options, slha%local)
    end if
    if (slha%write) then
       input = .true.
       spectrum = .false.
       decays = .false.
       call slha_write_file &
            (slha%file, slha%local%model, &
             input = input, spectrum = spectrum, decays = decays)
    else
       input = var_list_get_lval (slha%local%var_list, &
            var_str ("?slha_read_input"))
       spectrum = var_list_get_lval (slha%local%var_list, &
            var_str ("?slha_read_spectrum"))
       decays = var_list_get_lval (slha%local%var_list, &
            var_str ("?slha_read_decays"))
       call slha_read_file &
            (slha%file, slha%local%os_data, slha%local%model, &
             input = input, spectrum = spectrum, decays = decays)
    end if
    call rt_data_restore (global, slha%local, keep_model_vars = .true.)
  end subroutine cmd_slha_execute

  subroutine cmd_show_final (show)
    type(cmd_show_t), intent(inout) :: show
    integer :: i
    if (allocated (show%pn_expr)) then
       deallocate (show%pn_expr)
    end if
    if (allocated (show%value)) then
       do i = 1, size (show%value)
          call var_entry_final (show%value(i))
       end do
    end if
  end subroutine cmd_show_final

  subroutine cmd_show_compile (show, pn, global)
    type(cmd_show_t), pointer :: show
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_var, pn_prefix, pn_name
    type(string_t) :: key
    integer :: i, n_args
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    allocate (show)
    if (associated (pn_arg)) then
       n_args = parse_node_get_n_sub (pn_arg)
       allocate (show%name (n_args))
       allocate (show%pn_expr (n_args))
       allocate (show%value (n_args))
       pn_var => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_var))
          i = i + 1
          select case (char (parse_node_get_rule_key (pn_var)))
          case ("model", "beams", "results", "unstable", &
                "real", "int", "intrinsic", &
                "cuts", "scale", "factorization_scale", &
                "renormalization_scale", "weight", &
                "selection", "reweight", "analysis", "expect")
             show%name(i) = parse_node_get_key (pn_var)
          case ("library_spec")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             key = parse_node_get_key (pn_prefix)
             if (associated (pn_name)) then
                show%name(i) = "L " // parse_node_get_string (pn_name)
             else
                show%name(i) = key
             end if
          case ("result_var")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             if (associated (pn_name)) then
                show%name(i) = parse_node_get_key (pn_prefix) &
                     // "(" // parse_node_get_string (pn_name) // ")"
             else
                show%name(i) = parse_node_get_key (pn_prefix)
             end if
          case ("log_var", "alias_var", "string_var")
             pn_prefix => parse_node_get_sub_ptr (pn_var)
             pn_name => parse_node_get_next_ptr (pn_prefix)
             key = parse_node_get_key (pn_prefix)
             if (associated (pn_name)) then
                select case (char (parse_node_get_rule_key (pn_name)))
                case ("variable")
                   select case (char (key))
                   case ("?", "$")  ! $ sign
                      show%name(i) = key // parse_node_get_string (pn_name)
                   case ("alias")
                      show%name(i) = "A " // parse_node_get_string (pn_name)
                   case ("library")
                      show%name(i) = "L " // parse_node_get_string (pn_name)
                   end select
                case ("lexpr")
                   show%name(i) = "<lexpr>"
                   show%pn_expr(i)%ptr => pn_name
                case ("cexpr")
                   show%name(i) = "<cexpr>"
                   show%pn_expr(i)%ptr => pn_name
                case ("sexpr")
                   show%name(i) = "<sexpr>"
                   show%pn_expr(i)%ptr => pn_name
                case default
                   call parse_node_mismatch &
                        ("variable|expr|lexpr|cexpr|sexpr",  pn_name)
                end select
             else
                show%name(i) = key
             end if
          case ("num_var")
             pn_name => parse_node_get_sub_ptr (pn_var)
             if (associated (pn_name)) then
                select case (char (parse_node_get_rule_key (pn_name)))
                case ("variable")
                   show%name(i) = parse_node_get_string (pn_name)
                case ("expr")
                   show%name(i) = "<expr>"
                   show%pn_expr(i)%ptr => pn_name
                case default
                   call parse_node_mismatch &
                        ("variable|expr",  pn_name)
                end select
             else
                show%name(i) = key
             end if
          case default
             pn_prefix => null ()
             pn_name => parse_node_get_sub_ptr (pn_var)
             if (associated (pn_name)) then
                show%name(i) = parse_node_get_string (pn_name)
             else
                show%name(i) = ""
             end if
          end select
          pn_var => parse_node_get_next_ptr (pn_var)
       end do
    else
       allocate (show%name (0))
    end if
  end subroutine cmd_show_compile

  subroutine cmd_show_execute (show, global)
    type(cmd_show_t), intent(inout) :: show
    type(rt_data_t), intent(in), target :: global
    type(string_t) :: name
    type(process_library_t), pointer :: prc_lib
    logical :: lval
    integer :: ival
    real(default) :: rval
    complex(default) :: cval
    type(pdg_array_t) :: aval
    type(string_t) :: sval
    logical :: is_known
    integer :: result_type
    integer :: i, u
    u = logfile_unit ()
    if (size (show%name) == 0) then
       call var_list_write (global%var_list)
    else
       if (associated (global%model)) then
          name = model_get_name (global%model)
       else
          name = "[undefined]"
       end if
       do i = 1, size (show%name)
          select case (char (show%name(i)))
          case ("model")
             print *, "model* = ", char (name)
             write (u, *)  "model* = ", char (name)
          case ("library")
             if (associated (global%prc_lib)) then
                call process_library_write (global%prc_lib)
                call process_library_write (global%prc_lib, unit=u)
             else
                call msg_message ("Show library: no library is loaded")
             end if
          case ("beams")
             call beam_data_write (global%beam_data)
             call beam_data_write (global%beam_data, unit=u)
          case ("results")
             call process_store_write_results ()
             call process_store_write_results (unit=u)
          case ("unstable")
             call decay_store_write ()
             call decay_store_write (unit=u)
          case ("cuts")
             if (associated (global%pn_cuts_lexpr)) then
                call parse_node_write_rec (global%pn_cuts_lexpr)
                call parse_node_write_rec (global%pn_cuts_lexpr, u)
             else
                call msg_message ("No cut expression defined")
             end if
          case ("weight")
             if (associated (global%pn_weight_expr)) then
                call parse_node_write_rec (global%pn_weight_expr)
                call parse_node_write_rec (global%pn_weight_expr,u)
             else
                call msg_message ("No weight expression defined")
             end if
          case ("scale")
             if (associated (global%pn_scale_expr)) then
                call parse_node_write_rec (global%pn_scale_expr)
                call parse_node_write_rec (global%pn_scale_expr,u)
             else
                call msg_message ("No general scale expression defined")
             end if          
          case ("factorization_scale")
             if (associated (global%pn_fac_scale_expr)) then
                call parse_node_write_rec (global%pn_fac_scale_expr)
                call parse_node_write_rec (global%pn_fac_scale_expr,u)
                call msg_message ("Factorization scale expression superseding scale defintion.")
             else
                call msg_message ("No factorization scale expression defined")
             end if
          case ("renormalization_scale")
             if (associated (global%pn_ren_scale_expr)) then
                call parse_node_write_rec (global%pn_ren_scale_expr)
                call parse_node_write_rec (global%pn_ren_scale_expr,u)
                call msg_message ("Renormalization scale expression superseding scale defintion.")
             else
                call msg_message ("No renormalization scale expression defined")
             end if          
          case ("analysis")
             if (associated (global%pn_analysis_lexpr)) then
                call parse_node_write_rec (global%pn_analysis_lexpr)
                call parse_node_write_rec (global%pn_analysis_lexpr, u)
             else
                call msg_message ("No cut expression defined")
             end if
          case ("expect")
             call expect_summary ()
          case ("?")
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_LOG, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_LOG, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_LOG)
                call var_list_write (global%var_list, only_type=V_LOG, unit=u)
             end if
          case ("intrinsic")
             if (associated (global%model)) then
                call var_list_write (global%var_list, intrinsic=.true., &
                     model_name = name)
                call var_list_write (global%var_list, intrinsic=.true., &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, intrinsic=.true.)
                call var_list_write (global%var_list, intrinsic=.true., unit=u)
             end if
          case ("int")
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_INT, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_INT, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_INT)
                call var_list_write (global%var_list, only_type=V_INT, unit=u)
             end if
          case ("real")
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_REAL, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_REAL, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_REAL)
                call var_list_write (global%var_list, only_type=V_REAL, unit=u)
             end if
          case ("complex")
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_CMPLX, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_CMPLX, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_CMPLX)
                call var_list_write (global%var_list, only_type=V_CMPLX, unit=u)
             end if
          case ("alias")
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_PDG, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_PDG, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_PDG)
                call var_list_write (global%var_list, only_type=V_PDG, unit=u)
             end if
          case ("$")  !$ sign
             if (associated (global%model)) then
                call var_list_write (global%var_list, only_type=V_STR, &
                     model_name = name)
                call var_list_write (global%var_list, only_type=V_STR, &
                     model_name = name, unit=u)
             else
                call var_list_write (global%var_list, only_type=V_STR)
                call var_list_write (global%var_list, only_type=V_STR, unit=u)
             end if
          case ("n_calls", "num_id", &
                "integral", "error", "accuracy", "chi2", "efficiency")
             call var_list_write (global%var_list, prefix=char(show%name(i)))
             call var_list_write (global%var_list, prefix=char(show%name(i)), &
                  unit=u)
          case ("<lexpr>")
             lval = eval_log (show%pn_expr(i)%ptr, global%var_list, &
                  is_known=is_known)
             if (is_known) then
                call var_entry_init_log (show%value(i), &
                     var_str ("logical value"), lval)
             else
                call var_entry_init_log (show%value(i), &
                     var_str ("logical value"))
             end if
             call var_entry_write (show%value(i))
             call var_entry_write (show%value(i), unit=u)
          case ("<expr>")
             call eval_numeric (show%pn_expr(i)%ptr, global%var_list, &
                  ival=ival, rval=rval, cval=cval, &
                  is_known=is_known, result_type=result_type)
             select case (result_type)
             case (V_INT)
                if (is_known) then
                   call var_entry_init_int (show%value(i), &
                        var_str ("integer value"), ival)
                else
                   call var_entry_init_int (show%value(i), &
                        var_str ("integer value"))
                end if
             case (V_REAL)
                if (is_known) then
                   call var_entry_init_real (show%value(i), &
                        var_str ("real value"), rval)
                else
                   call var_entry_init_real (show%value(i), &
                        var_str ("real value"))
                end if
             case (V_CMPLX)
                if (is_known) then
                   call var_entry_init_cmplx (show%value(i), &
                        var_str ("complex value"), cval)
                else
                   call var_entry_init_cmplx (show%value(i), &
                        var_str ("complex value"))
                end if
             end select
             call var_entry_write (show%value(i))
             call var_entry_write (show%value(i), unit=u)
          case ("<cexpr>")
             aval = eval_pdg_array (show%pn_expr(i)%ptr, global%var_list, &
                  is_known=is_known)
             if (is_known) then
                call var_entry_init_pdg_array (show%value(i), &
                     var_str ("alias value"), aval)
             else
                call var_entry_init_pdg_array (show%value(i), &
                     var_str ("alias value"))
             end if
             call var_entry_write (show%value(i))
             call var_entry_write (show%value(i), unit=u)
          case ("<sexpr>")
             sval = eval_string (show%pn_expr(i)%ptr, global%var_list, &
                  is_known=is_known)
             if (is_known) then
                call var_entry_init_string (show%value(i), &
                     var_str ("string value"), sval)
             else
                call var_entry_init_string (show%value(i), &
                     var_str ("string value"))
             end if
             call var_entry_write (show%value(i))
             call var_entry_write (show%value(i), unit=u)
          case default
             select case (char (extract (show%name(i), 1, 2)))
             case ("L ")
                name = extract (show%name(i), 3)
                prc_lib => process_library_store_get_ptr (name)
                if (associated (prc_lib)) then
                   call process_library_write (prc_lib)
                   call process_library_write (prc_lib, unit=u)
                else
                   call msg_message ("Library '" // char (name) &
                        // "' not loaded")
                end if
             case ("A ")
                name = extract (show%name(i), 3)
                call var_list_write_var (global%var_list, name, &
                     model_name = name, type = V_PDG)
                call var_list_write_var (global%var_list, name, &
                     model_name = name, type = V_PDG, unit=u)
             case default
                if (associated (global%model)) then
                   call var_list_write_var (global%var_list, show%name(i), &
                        model_name = name)
                   call var_list_write_var (global%var_list, show%name(i), &
                        model_name = name, unit=u)
                else
                   call var_list_write_var (global%var_list, show%name(i))
                   call var_list_write_var (global%var_list, show%name(i), &
                        unit=u)
                end if
             end select
          end select
       end do
    end if
    flush (u)
  end subroutine cmd_show_execute

  subroutine cmd_expect_final (expect)
    type(cmd_expect_t), intent(inout) :: expect
    if (associated (expect%options)) then
       call command_list_final (expect%options)
       deallocate (expect%options)
    end if
  end subroutine cmd_expect_final

  subroutine cmd_expect_compile (expect, pn, global)
    type(cmd_expect_t), pointer :: expect
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_opt
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_arg)
    allocate (expect)
    call rt_data_local_init (expect%local, global)
    if (associated (pn_opt)) then
       allocate (expect%options)
       call command_list_compile (expect%options, pn_opt, expect%local)
    end if
    expect%pn_lexpr => parse_node_get_sub_ptr (pn_arg)
    call rt_data_local_reset (expect%local)
  end subroutine cmd_expect_compile

  subroutine cmd_expect_execute (expect, global)
    type(cmd_expect_t), intent(inout) :: expect
    type(rt_data_t), intent(inout), target :: global
    logical :: success, is_known
    integer :: u
    u = logfile_unit ()
    call rt_data_link (expect%local, global)
    if (associated (expect%options)) then
       call command_list_execute (expect%options, expect%local)
    end if
    success = &
         eval_log (expect%pn_lexpr, expect%local%var_list, is_known=is_known)
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
    call rt_data_restore (global, expect%local)
  end subroutine cmd_expect_execute

  subroutine cmd_beams_final (beams)
    type(cmd_beams_t), intent(inout) :: beams
    if (allocated (beams%pn_pdg)) then
       deallocate (beams%pn_pdg)
    end if
    if (associated (beams%options)) then
       call command_list_final (beams%options)
       deallocate (beams%options)
    end if
  end subroutine cmd_beams_final

  subroutine cmd_beams_compile (beams, pn, global)
    type(cmd_beams_t), pointer :: beams
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_beam_def, pn_beam_spec
    type(parse_node_t), pointer :: pn_beam_list, pn_opt
    type(parse_node_t), pointer :: pn_codes
    type(parse_node_t), pointer :: pn_strfun_seq, pn_strfun_pair
    integer :: i
    pn_beam_def => parse_node_get_sub_ptr (pn, 3)
    pn_beam_spec => parse_node_get_sub_ptr (pn_beam_def)
    pn_strfun_seq => parse_node_get_next_ptr (pn_beam_spec)
    pn_beam_list => parse_node_get_sub_ptr (pn_beam_spec)
    pn_opt => parse_node_get_next_ptr (pn_beam_list)
    allocate (beams)
    call rt_data_local_init (beams%local, global, CMD_BEAMS)
    if (associated (pn_opt)) then
       allocate (beams%options)
       call command_list_compile (beams%options, pn_opt, beams%local)
    end if
    beams%n_in = parse_node_get_n_sub (pn_beam_list)
    select case (beams%n_in)
    case (1)
       if (associated (pn_strfun_seq)) then
          call parse_node_write (pn_beam_def)
          call msg_error ("Structure functions can't be defined " &
               // "for decay processes")
          pn_strfun_seq => null ()
       end if
    end select
    allocate (beams%pn_pdg (beams%n_in))
    allocate (beams%prt (beams%n_in))
    pn_codes => parse_node_get_sub_ptr (pn_beam_list)
    do i = 1, beams%n_in
       beams%pn_pdg(i)%ptr => pn_codes
       beams%prt(i) = "?"
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
    if (associated (pn_strfun_seq)) then
       beams%n_strfun = parse_node_get_n_sub (pn_beam_def) - 1
       allocate (beams%strfun_pair (beams%n_strfun))
       do i = 1, beams%n_strfun
          pn_strfun_pair => parse_node_get_sub_ptr (pn_strfun_seq, 2)
          call strfun_pair_compile &
               (beams%strfun_pair(i), pn_strfun_pair, beams%local)
          pn_strfun_seq => parse_node_get_next_ptr (pn_strfun_seq)
       end do
    end if
    call rt_data_local_reset (beams%local)
  end subroutine cmd_beams_compile

  subroutine strfun_pair_compile (strfun_pair, pn_strfun_pair, global)
    type(strfun_pair_t), intent(out) :: strfun_pair
    type(parse_node_t), intent(in), target :: pn_strfun_pair
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_strfun_def
    integer :: i
    strfun_pair%n = parse_node_get_n_sub (pn_strfun_pair)
    pn_strfun_def => parse_node_get_sub_ptr (pn_strfun_pair)
    do i = 1, strfun_pair%n
       call strfun_def_compile (strfun_pair%def(i), pn_strfun_def, global)
       pn_strfun_def => parse_node_get_next_ptr (pn_strfun_def)
    end do
  end subroutine strfun_pair_compile

  subroutine strfun_def_compile (strfun_def, pn_strfun_def, global)
    type(strfun_def_t), intent(out) :: strfun_def
    type(parse_node_t), intent(in), target :: pn_strfun_def
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_key, pn_opt, pn_arg
    pn_key => parse_node_get_sub_ptr (pn_strfun_def)
    pn_opt => parse_node_get_next_ptr (pn_key)
    select case (char (parse_node_get_rule_key (pn_key)))
    case ("none")
       strfun_def%type = STRF_NONE
    case ("lhapdf")
       strfun_def%type = STRF_LHAPDF
    case ("pdf_builtin")
       strfun_def%type = STRF_PDF_BUILTIN
    case ("isr")
       strfun_def%type = STRF_ISR
    case ("epa")
       strfun_def%type = STRF_EPA
    case ("ewa")
       strfun_def%type = STRF_EWA
    case ("circe1")
       strfun_def%type = STRF_CIRCE1       
    case ("circe2")
       strfun_def%type = STRF_CIRCE2
    case ("energy_scan")
       strfun_def%type = STRF_ESCAN
    case ("beam_events")
       strfun_def%type = STRF_BEVT
    case ("user_sf_spec")
       strfun_def%type = STRF_USER
       pn_arg => parse_node_get_sub_ptr (pn_key, 2)
       strfun_def%pn_user_name => parse_node_get_sub_ptr (pn_arg)
    end select
    call rt_data_local_init (strfun_def%local, global)
    if (associated (pn_opt)) then
       allocate (strfun_def%options)
       call command_list_compile &
            (strfun_def%options, pn_opt, strfun_def%local)
    end if
    call rt_data_local_reset (strfun_def%local)
  end subroutine strfun_def_compile

  subroutine cmd_beams_execute (beams, global)
    type(cmd_beams_t), intent(inout), target :: beams
    type(rt_data_t), intent(inout), target :: global
    logical, dimension(2) :: p_known
    real(default), dimension(2) :: p
    logical :: sqrts_known, alpha_known, theta_known, phi_known
    real(default) :: sqrts, alpha, theta, phi
    real(default) :: beams_theta, beams_phi
    type(pdg_array_t), dimension(2) :: aval
    type(flavor_t), dimension(:), allocatable :: flv_tmp
    type(flavor_t), dimension(2) :: flv
    type(beam_polarization_t), dimension(2) :: bp
    type(polarization_t), dimension(2) :: pol
    integer :: i, u
    logical :: polarized
    u = logfile_unit ()
    call lhapdf_status_reset (global%lhapdf_status)
    call rt_data_link (beams%local, global)
    if (associated (beams%options)) then
       call command_list_execute (beams%options, beams%local)
    end if
    polarized = .true.
    do i = 1, beams%n_in
       aval(i) = eval_pdg_array (beams%pn_pdg(i)%ptr, beams%local%var_list)
       call flavor_init (flv_tmp, aval(i), beams%local%model)
       select case (size (flv_tmp))
       case (1);  flv(i) = flv_tmp(1)
       case default
          call pdg_array_write (aval(i))
          call msg_fatal &
               ("Beam expression does not evaluate to a unique particle")
          return
       end select
       beams%prt(i) = flavor_get_name (flv(i))
       if (polarized .and. associated (beams%local%beam_polarization)) then
          if (size (beams%local%beam_polarization) == beams%n_in) then
             bp(1:beams%n_in) = beams%local%beam_polarization
          else
             call msg_error ("the number of incoming particles differs " &
                // "between beam and polarization setup - ignoring polarization")
             polarized = .false.
          end if
       else
          polarized = .false.
          select case (beams%n_in)
          case (1)
             call beam_polarization_init_trivial (bp(i))
          case (2)
             call beam_polarization_init_none (bp(i))
          end select
       end if
       pol(i) = beam_polarization2polarization (bp(i), flv(i), &
            decay=(size (bp) == 1))
    end do
    sqrts_known = var_list_is_known (beams%local%var_list, "sqrts")
    p_known(1) = var_list_is_known (beams%local%var_list, "beam1_momentum")
    p_known(2) = var_list_is_known (beams%local%var_list, "beam2_momentum")
    alpha_known = var_list_is_known (beams%local%var_list, "crossing_angle")
    theta_known = var_list_is_known (beams%local%var_list, "beams_theta")
    phi_known = var_list_is_known (beams%local%var_list, "beams_phi")
    sqrts = var_list_get_rval (beams%local%var_list, "sqrts")
    p(1) = var_list_get_rval (beams%local%var_list, "beam1_momentum")
    p(2) = var_list_get_rval (beams%local%var_list, "beam2_momentum")
    alpha = var_list_get_rval (beams%local%var_list, "crossing_angle")
    theta = var_list_get_rval (beams%local%var_list, "beams_theta")
    phi = var_list_get_rval (beams%local%var_list, "beams_phi")
    select case (beams%n_in)
    case (2)
       if (sqrts_known) then
          if (any (p_known))  call msg_error &
               ("Beam setup: sqrts and beam momenta must not be set " &
               // "simultaneously; using sqrts only")
          if (alpha_known)  call msg_fatal &
               ("Beam setup: sqrts and crossing angle must not be set " &
               // "simultaneously (set beam1_momentum and beam2_momentum " &
               // "instead)")
       else
          if (.not. all (p_known))  call msg_fatal &
               ("Beam setup: either sqrts or both beam momenta must be set.")
       end if
    end select
    select case (beams%n_in)
    case (1)
       if (p_known(1) .or. theta_known .or. phi_known) then
          call beam_data_init_decay &
               (global%beam_data, flv, pol(1:1), p(1), theta, phi)
       else
          call beam_data_init_decay (global%beam_data, flv, pol(1:1))
       end if
    case (2)
       if (sqrts_known) then
          if (theta_known .or. phi_known) then
             call beam_data_init_sqrts (global%beam_data, &
                  sqrts, flv, pol, 0._default, theta=theta, phi=phi)
          else
             call beam_data_init_sqrts (global%beam_data, &
                  sqrts, flv, pol)
          end if
       else if (alpha_known) then
          if (theta_known .or. phi_known) then
             call beam_data_init_momenta (global%beam_data, &
                  p, flv, pol, alpha, theta, phi)
          else          
             call beam_data_init_momenta (global%beam_data, &
                  p, flv, pol, alpha)
          end if
       else
          if (theta_known .or. phi_known) then
             call beam_data_init_momenta (global%beam_data, &
                  p, flv, pol, theta=theta, phi=phi)
          else          
             call beam_data_init_momenta (global%beam_data, &
                  p, flv, pol)
          end if
       end if
       if (global%sf_list_allocated) then
          call sf_list_final (global%sf_list)
          deallocate (global%sf_list)
       end if
       allocate (global%sf_list)
       global%sf_list_allocated = .true.
       do i = 1, beams%n_strfun
          call strfun_pair_register (beams%strfun_pair(i), global)
       end do
       call sf_list_freeze (global%sf_list)
       call sf_list_write (global%sf_list)
       call sf_list_compute_md5sum (global%sf_list)
    end select
    call beam_data_write (global%beam_data, verbose=.false.)
    call beam_data_write (global%beam_data, verbose=.false., unit=u)
    if (polarized) then
       do i = 1, beams%n_in
          if (output_unit () >= 0) write (output_unit (), '(1x,A)') &
             "polarization of '" // char (flavor_get_name (flv(i))) // "':"
          if (u >= 0) write (u, '(1x,A)') &
             "polarization of '" // char (flavor_get_name (flv(i))) // "':"
          call beam_polarization_write (beams%local%beam_polarization(i))
          call beam_polarization_write (beams%local%beam_polarization(i), u)
       end do
    end if
    if (u >= 0) flush (u)
    call rt_data_restore (global, beams%local)
  end subroutine cmd_beams_execute

  subroutine strfun_pair_register (strfun_pair, global)
    type(strfun_pair_t), intent(inout) :: strfun_pair
    type(rt_data_t), intent(inout), target :: global
    logical, dimension(2) :: affects_beam
    select case (strfun_pair%n)
    case (1)
       affects_beam = .true.
       call strfun_def_register (strfun_pair%def(1), affects_beam, global)
    case (2)
       affects_beam = (/ .true., .false. /)
       call strfun_def_register (strfun_pair%def(1), affects_beam, global)
       affects_beam = (/ .false., .true. /)
       call strfun_def_register (strfun_pair%def(2), affects_beam, global)
    end select
  end subroutine strfun_pair_register

  subroutine strfun_def_register (strfun_def, affects_beam, global)
    type(strfun_def_t), intent(inout) :: strfun_def
    logical, dimension(2), intent(in) :: affects_beam
    type(rt_data_t), intent(inout), target :: global
    call rt_data_link (strfun_def%local, global)
    if (associated (strfun_def%options)) then
       call command_list_execute (strfun_def%options, strfun_def%local)
    end if
    select case (strfun_def%type)
    case (STRF_LHAPDF)
       call sf_list_register_lhapdf &
            (global%sf_list, affects_beam, &
             global%lhapdf_status, global%model, global%beam_data%flv, &
             strfun_def%local%var_list)
    case (STRF_PDF_BUILTIN)
       call sf_list_register_pdf_builtin &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, &
             strfun_def%local%os_data%pdf_builtin_datapath, &
             strfun_def%local%var_list)
    case (STRF_ISR)
       call sf_list_register_isr &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, global%beam_data%sqrts, &
             strfun_def%local%var_list)
    case (STRF_EPA)
       call sf_list_register_epa &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, global%beam_data%sqrts, &
             strfun_def%local%var_list)
    case (STRF_EWA)
       call msg_warning ("EWA structure function not yet fully implemented")
       call sf_list_register_ewa &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, global%beam_data%sqrts, &
             strfun_def%local%var_list)
    case (STRF_CIRCE1)
       call sf_list_register_circe1 &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, global%beam_data%sqrts, &
             global%rng, &
             strfun_def%local%var_list)
    case (STRF_CIRCE2)
       call sf_list_register_circe2 &
            (global%sf_list, affects_beam, &
             global%beam_data%flv, global%beam_data%sqrts, &
             global%rng, global%os_data%whizard_circe2path, &
             strfun_def%local%var_list)
    case (STRF_ESCAN)
       call sf_list_register_escan &
            (global%sf_list, affects_beam, &
             global%beam_data%flv, global%beam_data%sqrts, &
             strfun_def%local%var_list)
    case (STRF_BEVT)
       call sf_list_register_beam_events &
            (global%sf_list, affects_beam, &
             global%beam_data%flv, global%os_data%whizard_beamsimpath, &
             strfun_def%local%var_list)
    case (STRF_USER)
       call sf_list_register_user &
            (global%sf_list, affects_beam, &
             global%model, global%beam_data%flv, &
             eval_string (strfun_def%pn_user_name, strfun_def%local%var_list), &
             strfun_def%local%var_list)
    end select
    call rt_data_restore (global, strfun_def%local)
  end subroutine strfun_def_register

  subroutine sf_list_register_lhapdf (sf_list, affects_beam, &
       lhapdf_status, model, flv, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    type(var_list_t), intent(in) :: var_list
    type(string_t) :: lhapdf_file, lhapdf_dir
    integer :: lhapdf_member, lhapdf_photon_scheme
    type(sf_data_t), pointer :: sf_data
    integer :: i
    lhapdf_dir = &
         var_list_get_sval (var_list, var_str ("$lhapdf_dir"))  ! $
    lhapdf_file = &
         var_list_get_sval (var_list, var_str ("$lhapdf_file"))  ! $
    lhapdf_member = &
         var_list_get_ival (var_list, var_str ("lhapdf_member"))
    lhapdf_photon_scheme = &
         var_list_get_ival (var_list, var_str ("lhapdf_photon_scheme"))
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          call sf_data_init_lhapdf (sf_data, i, lhapdf_status, &
               model, flv(i), &
               lhapdf_dir, lhapdf_file, lhapdf_member, lhapdf_photon_scheme)
          call sf_list_append (sf_list, sf_data)
       end if
    end do
    if (all (affects_beam)) then
       call sf_data_setup_mapping &
            (sf_data, SFM_PDFPAIR, (/ 0, 1 /), 2._default)
    end if
  end subroutine sf_list_register_lhapdf

  subroutine sf_list_register_pdf_builtin (sf_list, affects_beam, &
       model, flv, datapath, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: datapath
    type(var_list_t), intent(in) :: var_list
    logical :: pdf_builtin_have_name
    type(string_t) :: pdf_builtin_prefix, pdf_builtin_name
    type(sf_data_t), pointer :: sf_data
    integer :: i
    pdf_builtin_have_name = &
         var_list_is_known (var_list, var_str ("$pdf_builtin_set"))
    if (pdf_builtin_have_name)  &
         pdf_builtin_name = &
         var_list_get_sval (var_list, var_str ("$pdf_builtin_set"))
    pdf_builtin_have_name = trim (pdf_builtin_name) /= ""
    pdf_builtin_prefix = ""
    if (var_list_is_known (var_list, var_str ("$pdf_builtin_path"))) &
         pdf_builtin_prefix = &
              var_list_get_sval (var_list, var_str ("$pdf_builtin_path"))
    if (trim (pdf_builtin_prefix) == "")  pdf_builtin_prefix = datapath
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          if (pdf_builtin_have_name) then
             call sf_data_init_pdf_builtin (sf_data, i, model, flv(i), &
                  name=pdf_builtin_name, path=pdf_builtin_prefix)
          else
             call sf_data_init_pdf_builtin (sf_data, i, model, flv(i), &
                  path=pdf_builtin_prefix)
          end if
          call sf_list_append (sf_list, sf_data)
       end if
    end do
    if (all (affects_beam)) then
       call sf_data_setup_mapping &
            (sf_data, SFM_PDFPAIR, (/ 0, 1 /), 2._default)
    end if
  end subroutine sf_list_register_pdf_builtin

  subroutine sf_list_register_isr (sf_list, affects_beam, &
       model, flv, sqrts, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(var_list_t), intent(in) :: var_list
    real(default) :: isr_alpha, isr_q_max, isr_mass
    integer :: isr_order
    logical :: isr_recoil
    type(sf_data_t), pointer :: sf_data
    integer :: i
    isr_alpha = var_list_get_rval (var_list, var_str ("isr_alpha"))
    if (isr_alpha == 0) then
       isr_alpha = (var_list_get_rval (var_list, var_str ("ee"))) &
                   ** 2 / (4 * pi)
    end if
    isr_q_max = var_list_get_rval (var_list, var_str ("isr_q_max"))
    if (isr_q_max == 0) then
       isr_q_max = sqrts
    end if
    isr_mass   = var_list_get_rval (var_list, var_str ("isr_mass"))
    isr_order  = var_list_get_ival (var_list, var_str ("isr_order"))
    isr_recoil = var_list_get_lval (var_list, var_str ("?isr_recoil"))
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          if (isr_mass /= 0) then
             call sf_data_init_isr (sf_data, i, &
                  model, flv(i), isr_recoil, isr_alpha, isr_q_max, isr_mass, &
                  order=isr_order)
          else
             call sf_data_init_isr (sf_data, i, &
                  model, flv(i), isr_recoil, isr_alpha, isr_q_max, &
                  order=isr_order)
          end if
          call sf_list_append (sf_list, sf_data)
       end if
    end do
    ! No pair mapping
  end subroutine sf_list_register_isr

  subroutine sf_list_register_epa (sf_list, affects_beam, &
       model, flv, sqrts, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(var_list_t), intent(in) :: var_list
    real(default) :: epa_alpha, epa_x_min, epa_q_min, epa_e_max, epa_mass
    logical :: epa_recoil
    type(sf_data_t), pointer :: sf_data
    integer :: i
    epa_alpha = var_list_get_rval (var_list, var_str ("epa_alpha"))
    if (epa_alpha == 0) then
       epa_alpha = (var_list_get_rval (var_list, var_str ("ee"))) &
                   ** 2 / (4 * pi)
    end if
    epa_x_min = var_list_get_rval (var_list, var_str ("epa_x_min"))
    epa_q_min = var_list_get_rval (var_list, var_str ("epa_q_min"))
    epa_e_max = var_list_get_rval (var_list, var_str ("epa_e_max"))
    if (epa_e_max == 0) then
       epa_e_max = sqrts
    end if
    epa_mass   = var_list_get_rval (var_list, var_str ("epa_mass"))
    epa_recoil = var_list_get_lval (var_list, var_str ("?epa_recoil"))
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          if (epa_mass /= 0) then
             call sf_data_init_epa (sf_data, i, &
                  model, flv(i), epa_recoil, &
                  epa_alpha, epa_x_min, epa_q_min, epa_e_max, epa_mass)
          else
             call sf_data_init_epa (sf_data, i, &
                  model, flv(i), epa_recoil, &
                  epa_alpha, epa_x_min, epa_q_min, epa_e_max)
          end if
          call sf_list_append (sf_list, sf_data)
       end if
    end do
    if (all (affects_beam)) then
       if (epa_recoil) then
          call sf_data_setup_mapping &
               (sf_data, SFM_EPAPAIR, (/-2, 1 /), 1._default)
       else
          call sf_data_setup_mapping &
               (sf_data, SFM_EPAPAIR, (/ 0, 1 /), 1._default)
       end if
    end if
  end subroutine sf_list_register_epa

  subroutine sf_list_register_ewa (sf_list, affects_beam, &
       model, flv, sqrts, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(var_list_t), intent(in) :: var_list
    real(default) :: ewa_x_min, ewa_q_min, ewa_pt_max, ewa_mass, ewa_sqrts    
    logical :: ewa_keep_momentum, ewa_keep_energy
    type(sf_data_t), pointer :: sf_data
    integer :: i
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          ewa_x_min  = var_list_get_rval (var_list, var_str ("ewa_x_min"))
          ewa_q_min  = var_list_get_rval (var_list, var_str ("ewa_q_min"))
          ewa_pt_max = var_list_get_rval (var_list, var_str ("ewa_pt_max"))
          if (ewa_pt_max == 0) then
             ewa_pt_max = sqrts
          end if
          ewa_mass = var_list_get_rval (var_list, var_str ("ewa_mass"))
          ewa_sqrts = sqrts
          ewa_keep_momentum = var_list_get_lval (var_list, &
                    var_str ("?ewa_keep_momentum"))
          ewa_keep_energy = var_list_get_lval (var_list, &
                    var_str ("?ewa_keep_energy"))          
          if (ewa_keep_momentum .and. ewa_keep_energy) &
               call msg_fatal (" EWA cannot violate both energy " &
                            // "and momentum conservation.") 
          if (ewa_mass /= 0) then     
             call sf_data_init_ewa (sf_data, i, &
                  model, flv(i), &
                  ewa_x_min, ewa_q_min, ewa_pt_max, ewa_sqrts, &
                  ewa_keep_momentum, ewa_keep_energy, ewa_mass)
          else         
             call sf_data_init_ewa (sf_data, i, &
                  model, flv(i), &
                  ewa_x_min, ewa_q_min, ewa_pt_max, ewa_sqrts, &
                  ewa_keep_momentum, ewa_keep_energy)          
          end if        
          call sf_list_append (sf_list, sf_data)
       end if
    end do
    if (all (affects_beam)) then
       call sf_data_setup_mapping &
            (sf_data, SFM_EWAPAIR, (/ 0, 1 /), 1._default)
    end if
  end subroutine sf_list_register_ewa

  subroutine sf_list_register_circe1 (sf_list, affects_beam, &
       model, flv, sqrts, rng, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(tao_random_state), intent(in), target :: rng
    type(var_list_t), intent(in) :: var_list
     real(default) :: circe1_sqrts
    logical, dimension(2) :: circe1_photon
    logical :: circe1_generate, circe1_map
    integer :: circe1_ver, circe1_rev, circe1_acc, circe1_chat
    type(sf_data_t), pointer :: sf_data
    if (all (affects_beam)) then
       allocate (sf_data)
       if (var_list_is_known (var_list, var_str ("circe1_sqrts"))) then
          circe1_sqrts = var_list_get_rval (var_list, var_str ("circe1_sqrts"))
       else
          circe1_sqrts = sqrts
       end if
       circe1_photon(1) = &
            var_list_get_lval (var_list, var_str ("?circe1_photon1"))
       circe1_photon(2) = &
            var_list_get_lval (var_list, var_str ("?circe1_photon2"))
       circe1_generate = &
            var_list_get_lval (var_list, var_str ("?circe1_generate"))
       circe1_map = &
            var_list_get_lval (var_list, var_str ("?circe1_map"))
       circe1_ver = &
            var_list_get_ival (var_list, var_str ("circe1_ver"))
       circe1_rev = &
            var_list_get_ival (var_list, var_str ("circe1_rev"))
       circe1_acc = &
            var_list_get_ival (var_list, var_str ("circe1_acc"))
       circe1_chat = &
            var_list_get_ival (var_list, var_str ("circe1_chat"))              
       call sf_data_init_circe1 (sf_data, &
            model, flv, circe1_sqrts, circe1_photon, &
            circe1_generate, rng, circe1_map, &
            circe1_ver, circe1_rev, circe1_acc, circe1_chat)
       call sf_list_append (sf_list, sf_data)
    else
       call msg_fatal ("CIRCE1 beamstrahlung spectrum must apply to both beams")
    end if
    ! No pair mapping
  end subroutine sf_list_register_circe1

  subroutine sf_list_register_circe2 (sf_list, affects_beam, &
       flv, sqrts, rng, path, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(tao_random_state), intent(in), target :: rng
    type(string_t), intent(in) :: path
    type(var_list_t), intent(in) :: var_list
    real(default) :: circe2_sqrts
    logical :: circe2_generate, circe2_map, circe2_polarized
    type(string_t) :: circe2_file, circe2_design
    type(sf_data_t), pointer :: sf_data
    if (all (affects_beam)) then
       allocate (sf_data)
       if (var_list_is_known (var_list, var_str ("circe2_sqrts"))) then
          circe2_sqrts = var_list_get_rval (var_list, var_str ("circe2_sqrts"))
       else
          circe2_sqrts = sqrts
       end if
       circe2_generate = &
            var_list_get_lval (var_list, var_str ("?circe2_generate"))
       circe2_map = &
            var_list_get_lval (var_list, var_str ("?circe2_map"))            
       circe2_polarized = &
            var_list_get_lval (var_list, var_str ("?circe2_polarized"))      
       circe2_file = &
            var_list_get_sval (var_list, var_str ("$circe2_file"))  ! $
       if (circe2_file == "")   call msg_fatal &
            ("CIRCE2: Data file $circe2_file must be specified") ! $
       circe2_file = path // "/" // circe2_file 
       circe2_design = &
            var_list_get_sval (var_list, var_str ("$circe2_design"))  ! $
       call sf_data_init_circe2 (sf_data, &
            flv, circe2_generate, rng, &
            circe2_map, circe2_file, circe2_design, circe2_sqrts, &
            circe2_polarized)
       call sf_list_append (sf_list, sf_data)
    else
       call msg_fatal ("CIRCE2 spectrum must apply to both beams")
    end if
    ! No pair mapping
  end subroutine sf_list_register_circe2

  subroutine sf_list_register_escan (sf_list, affects_beam, &
       flv, sqrts, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    type(var_list_t), intent(in) :: var_list
    real(default) :: escan_sqrts
    type(sf_data_t), pointer :: sf_data
    escan_sqrts = sqrts
    allocate (sf_data)
    call sf_data_init_escan (sf_data, affects_beam, flv, escan_sqrts)
    call sf_list_append (sf_list, sf_data)
    ! No pair mapping
  end subroutine sf_list_register_escan

  subroutine sf_list_register_beam_events (sf_list, affects_beam, &
       flv, path, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: path
    type(var_list_t), intent(in) :: var_list
    type(sf_data_t), pointer :: sf_data
    type(string_t) :: beam_events_file
    logical :: beam_events_warn_eof
    logical :: exist
    if (all (affects_beam)) then
       allocate (sf_data)
       beam_events_file = &
            var_list_get_sval (var_list, var_str ("$beam_events_file"))  ! $
       beam_events_warn_eof = &
            var_list_get_lval (var_list, var_str ("?beam_events_warn_eof"))
       inquire (file = char (beam_events_file), exist = exist)
       if (.not. exist) then
          beam_events_file = path // "/" // beam_events_file
          inquire (file = char (beam_events_file), exist = exist)
          if (.not. exist) then
             call msg_fatal ("Beam simulation data file '" &
                  // char (beam_events_file) // "' not found.")
          end if
       end if
       call sf_data_init_beam_events &
            (sf_data, affects_beam, flv, beam_events_file, beam_events_warn_eof)
       call sf_list_append (sf_list, sf_data)
    else
       call msg_fatal ("Beam events simulation must apply to both beams")
    end if
    ! No pair mapping
  end subroutine sf_list_register_beam_events

  subroutine sf_list_register_user (sf_list, affects_beam, &
       model, flv, user_name, var_list)
    type(sf_list_t), intent(inout) :: sf_list
    logical, dimension(2), intent(in) :: affects_beam
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: user_name
    type(var_list_t), intent(in) :: var_list
    type(sf_data_t), pointer :: sf_data
    logical :: user_strfun_mapping
    real(default) :: user_strfun_mapping_power
    integer :: i
    do i = 1, 2
       if (affects_beam(i)) then
          allocate (sf_data)
          call sf_data_init_user (sf_data, i, flv, user_name, model)
          call sf_list_append (sf_list, sf_data)
          if (all (sf_data_affects_beam (sf_data))) then
             if (.not. all (affects_beam))  call msg_fatal &
                  ("User spectrum/structure function inconsistently applied")
             exit
          end if
       end if
    end do
    user_strfun_mapping = & 
         var_list_get_lval (var_list, var_str ("?user_strfun_mapping"))
    user_strfun_mapping_power = &
         var_list_get_rval (var_list, var_str ("user_strfun_mapping_power"))
    if (all (affects_beam) .and. user_strfun_mapping) then
       if (all (sf_data_affects_beam (sf_data))) then
          call sf_data_setup_mapping &
               (sf_data, SFM_USER, &
                (/ sf_data_get_n_parameters (sf_data) - 1, &
                   sf_data_get_n_parameters (sf_data) /), &
                user_strfun_mapping_power)
       else
          call sf_data_setup_mapping &
               (sf_data, SFM_USER, &
                (/ 0, sf_data_get_n_parameters (sf_data) /), &
               user_strfun_mapping_power)
       end if
    end if
    ! No pair mapping
  end subroutine sf_list_register_user

  elemental subroutine bp_circ_data_final (d)
    type(bp_circ_data_t), intent(inout) :: d
  end subroutine bp_circ_data_final

  elemental subroutine bp_trans_data_final (d)
    type(bp_trans_data_t), intent(inout) :: d
  end subroutine bp_trans_data_final

  elemental subroutine bp_long_data_final (d)
    type(bp_long_data_t), intent(inout) :: d
  end subroutine bp_long_data_final

  elemental subroutine bp_axis_data_final (d)
    type(bp_axis_data_t), intent(inout) :: d
  end subroutine bp_axis_data_final

  elemental subroutine bp_diag_data_final (d)
    type(bp_diag_data_t), intent(inout) :: d
    deallocate (d%pn_hel)
    deallocate (d%pn_fraction)
    if (allocated (d%hel)) deallocate (d%hel)
    if (allocated (d%fraction)) deallocate (d%fraction)
  end subroutine bp_diag_data_final

  elemental subroutine bp_density_data_final (d)
    type(bp_density_data_t), intent(inout) :: d
  end subroutine bp_density_data_final

  subroutine cmd_beam_polarization_final (bp)
    type(cmd_beam_polarization_t), intent(inout) :: bp
    if (associated (bp%circ_data)) then
       call bp_circ_data_final (bp%circ_data)
       deallocate (bp%circ_data)
    end if
    if (associated (bp%trans_data)) then
       call bp_trans_data_final (bp%trans_data)
       deallocate (bp%trans_data)
    end if
    if (associated (bp%long_data)) then
       call bp_long_data_final (bp%long_data)
       deallocate (bp%long_data)
    end if
    if (associated (bp%axis_data)) then
       call bp_axis_data_final (bp%axis_data)
       deallocate (bp%axis_data)
    end if
    if (associated (bp%diag_data)) then
       call bp_diag_data_final (bp%diag_data)
       deallocate (bp%diag_data)
    end if
    if (associated (bp%density_data)) then
       call bp_density_data_final (bp%density_data)
       deallocate (bp%density_data)
    end if
    if (associated (bp%options)) then
       call command_list_final (bp%options)
       deallocate (bp%options)
    end if
    if (associated (bp%beam_polarization)) deallocate (bp%beam_polarization)
    bp%type = -1
    bp%n = -1
  end subroutine cmd_beam_polarization_final

  subroutine cmd_beam_polarization_compile (bp, pn, global)
    type(cmd_beam_polarization_t), pointer, intent(inout) :: bp
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_list, pn_opts, pn_args, pn_entry
    integer :: i, n
    pn_list => parse_node_get_sub_ptr (pn, 3)
    pn_opts => parse_node_get_sub_ptr (pn, 4)
    allocate (bp)
    call rt_data_local_init (bp%local, global)
    if (associated (pn_opts)) then
       allocate (bp%options)
       call command_list_compile (bp%options, pn_opts, bp%local)
    end if
    if (parse_node_get_rule_key (pn_list) == "off") then
       bp%n = 0
       bp%type = -1
       return
    end if
    bp%n = parse_node_get_n_sub (pn_list)
    pn_list => parse_node_get_sub_ptr (pn_list)
    do i = 1, bp%n
       pn_args => parse_node_get_sub_ptr (pn_list, 2)
       select case (char (parse_node_get_rule_key (pn_list)))
          case ("none")
             bp%type(i) = BP_NONE
          case ("bp_circ")
             if (parse_node_get_n_sub (pn_args) /= 1) then
                call cmd_beam_polarization_final (bp)
                call msg_fatal &
                   ("syntax error: expecting 'circular (fraction)'")
                return
             end if
             bp%type(i) = BP_CIRC
             if (.not. associated (bp%circ_data)) &
                  allocate (bp%circ_data(2))
             bp%circ_data(i)%pn_fraction => parse_node_get_sub_ptr (pn_args, 1)
          case ("bp_trans")
             if (parse_node_get_n_sub (pn_args) /= 2) then
                call cmd_beam_polarization_final (bp)
                call msg_fatal &
                   ("syntax error: expecting transverse (fraction, phi)'")
                return
             end if
             bp%type(i) = BP_TRANS
             if (.not. associated (bp%trans_data)) &
                  allocate (bp%trans_data(2))
             bp%trans_data(i)%pn_fraction => parse_node_get_sub_ptr (pn_args, 1)
             bp%trans_data(i)%pn_phi => parse_node_get_sub_ptr (pn_args, 2)
          case ("bp_axis")
             if (parse_node_get_n_sub (pn_args) /= 3) then
                call cmd_beam_polarization_final (bp)
                call msg_fatal &
                   ("syntax error: expecting 'axis (fraction, theta, phi)'")
                return
             end if
             bp%type(i) = BP_AXIS
             if (.not. associated (bp%axis_data)) &
                  allocate (bp%axis_data(2))
             bp%axis_data(i)%pn_fraction => parse_node_get_sub_ptr (pn_args, 1)
             bp%axis_data(i)%pn_theta => parse_node_get_sub_ptr (pn_args, 2)
             bp%axis_data(i)%pn_phi => parse_node_get_sub_ptr (pn_args, 3)
          case ("bp_long")
             if (parse_node_get_n_sub (pn_args) /= 1) then
                call cmd_beam_polarization_final (bp)
                call msg_fatal &
                   ("syntax error: expecting 'longitudinal (fraction)'")
                return
             end if
             bp%type(i) = BP_LONG
             if (.not. associated (bp%long_data)) &
                  allocate (bp%long_data(2))
             bp%long_data(i)%pn_fraction => parse_node_get_sub_ptr (pn_args, 1)
          case ("bp_dens")
             if (parse_node_get_n_sub (pn_args) /= 2) then
                call cmd_beam_polarization_final (bp)
                call msg_fatal &
                   ("syntax error: expecting 'density_matrix (a, b)'")
                return
             end if
             bp%type(i) = BP_DENSITY
             if (.not. associated (bp%density_data)) &
                  allocate (bp%density_data (2))
             bp%density_data(i)%pn_d => parse_node_get_sub_ptr (pn_args, 1)
             bp%density_data(i)%pn_nd => parse_node_get_sub_ptr (pn_args, 2)
          case ("bp_diag")
             bp%type(i) = BP_DIAG
             n = parse_node_get_n_sub (pn_args)
             if (.not. associated (bp%diag_data)) &
                allocate (bp%diag_data(2))
             allocate (bp%diag_data(i)%pn_hel (n))
             allocate (bp%diag_data(i)%pn_fraction (n))
             allocate (bp%diag_data(i)%hel (n))
             allocate (bp%diag_data(i)%fraction (n))
             pn_entry => parse_node_get_sub_ptr (pn_args)
             n = 1
             do while (associated (pn_entry))
                bp%diag_data(i)%pn_hel(n)%ptr &
                     => parse_node_get_sub_ptr (pn_entry, 1)
                bp%diag_data(i)%pn_fraction(n)%ptr &
                     => parse_node_get_sub_ptr (pn_entry, 3)
                pn_entry => parse_node_get_next_ptr (pn_entry)
                n = n + 1
             end do
          case default
             call msg_bug ("cmd_beam_polarization_compile: invalid " &
                // "polarization type")
       end select
       pn_list => parse_node_get_next_ptr (pn_list)
    end do

    call rt_data_local_reset (bp%local)
  end subroutine cmd_beam_polarization_compile

  subroutine cmd_beam_polarization_execute (bp, global)
    type(cmd_beam_polarization_t), pointer, intent(inout) :: bp
    type(rt_data_t), intent(inout), target :: global
    type(polarization_t), dimension(:), allocatable :: pol
    integer :: i, j, k, ulog
    ulog = logfile_unit ()
    call rt_data_link (bp%local, global)
    if (associated (bp%options)) &
       call command_list_execute (bp%options, bp%local)
    if (bp%n < 0) then
       call rt_data_restore (global, bp%local)
       return
    end if
    if (bp%type(1) < 0) then
       call rt_data_restore (global, bp%local)
       global%beam_polarization => null ()
       if (beam_data_are_valid (global%beam_data)) &
          call beam_data_kill_polarization (global%beam_data)
       if (global%environment /= CMD_BEAMS) call msg_message &
          ("beam polarization disabled")
       return
    end if
    if (.not. associated (bp%beam_polarization)) then
       allocate (bp%beam_polarization(bp%n))
    else
       do i = 1, bp%n
          call beam_polarization_final (bp%beam_polarization(i))
       end do
    end if
    do i = 1, bp%n
       select case (bp%type(i))
       case (BP_NONE, BP_TRIVIAL)
          if (bp%n == 2) then
             call beam_polarization_init_none (bp%beam_polarization(i))
          else
             call beam_polarization_init_trivial (bp%beam_polarization(i))
          end if
       case (BP_CIRC)
          bp%circ_data(i)%fraction = &
               eval_real (bp%circ_data(i)%pn_fraction, bp%local%var_list)
          call beam_polarization_init_circ (bp%beam_polarization(i), &
               bp%circ_data(i)%fraction)
       case (BP_TRANS)
          bp%trans_data(i)%fraction = &
               eval_real (bp%trans_data(i)%pn_fraction, bp%local%var_list)
          bp%trans_data(i)%phi = &
               eval_real (bp%trans_data(i)%pn_phi, bp%local%var_list)
          call beam_polarization_init_trans (bp%beam_polarization(i), &
               bp%trans_data(i)%fraction, bp%trans_data(i)%phi)
       case (BP_LONG)
          bp%long_data(i)%fraction = &
               eval_real (bp%long_data(i)%pn_fraction, bp%local%var_list)
          call beam_polarization_init_long (bp%beam_polarization(i), &
               bp%long_data(i)%fraction)
       case (BP_AXIS)
          bp%axis_data(i)%fraction = &
               eval_real (bp%axis_data(i)%pn_fraction, bp%local%var_list)
          bp%axis_data(i)%theta = &
               eval_real (bp%axis_data(i)%pn_theta, bp%local%var_list)
          bp%axis_data(i)%phi = &
               eval_real (bp%axis_data(i)%pn_phi, bp%local%var_list)
          call beam_polarization_init_axis (bp%beam_polarization(i), &
               bp%axis_data(i)%fraction, bp%axis_data(i)%theta, &
               bp%axis_data(i)%phi)
       case (BP_DENSITY)
          bp%density_data(i)%d = &
               eval_real (bp%density_data(i)%pn_d, bp%local%var_list)
          bp%density_data(i)%nd = &
               eval_cmplx (bp%density_data(i)%pn_nd, bp%local%var_list)
          call beam_polarization_init_density (bp%beam_polarization (i), &
               bp%density_data(i)%d, bp%density_data(i)%nd)
       case (BP_DIAG)
          do j = 1, size (bp%diag_data(i)%hel)
             bp%diag_data(i)%hel(j) = &
                  eval_int (bp%diag_data(i)%pn_hel(j)%ptr, bp%local%var_list)
             bp%diag_data(i)%fraction(j) = &
                  eval_real (bp%diag_data(i)%pn_fraction(j)%ptr, &
                  bp%local%var_list)
             if (j > 1) then
                do k = 1, j - 1 
                   if (bp%diag_data(i)%hel(j) == bp%diag_data(i)%hel(k)) then
                      call msg_error ( &
                         "'diagonal_density (h1:f1 [, h2:f2, ...])': " &
                         // "h" // int2char(j) // " and h" // int2char (k) &
                         // " must not be equal")
                      call rt_data_restore (global, bp%local)
                      return
                   end if
                end do
             end if
          end do
          call beam_polarization_init_diag (bp%beam_polarization(i), &
             bp%diag_data(i)%hel, bp%diag_data(i)%fraction)
       case default
          call msg_bug ("cmd_beam_polarization_execute: " &
             // "unknown polarization type")
       end select
       if (global%environment /= CMD_BEAMS) then
          call msg_message &
             ("polarization of incoming particle " // int2char (i) // ":")
          call beam_polarization_write (bp%beam_polarization(i))
          call beam_polarization_write (bp%beam_polarization(i), ulog)
       end if
    end do

    call rt_data_restore (global, bp%local)
    global%beam_polarization => bp%beam_polarization
    if (beam_data_are_valid (global%beam_data)) then
       if (beam_data_get_n_in (global%beam_data) /= bp%n) then
          call msg_error ("the number of incoming particles differs " &
             // "between beam and polarization setup - ignoring polarization")
       else
          allocate (pol (bp%n))
          do i = 1, bp%n
             pol(i) = beam_polarization2polarization &
                  (bp%beam_polarization(i), global%beam_data%flv(i), &
                   decay=(bp%n == 1))
          end do
          call beam_data_set_polarization (global%beam_data, pol)
       end if
    else
       if (global%environment /= CMD_BEAMS) call msg_warning ( &
          "beam_polarization only works with a beam setup")
    end if

  end subroutine cmd_beam_polarization_execute

  subroutine cmd_cuts_compile (cuts, pn)
    type(cmd_cuts_t), pointer :: cuts
    type(parse_node_t), intent(in), target :: pn
    allocate (cuts)
    cuts%pn_lexpr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_cuts_compile

  subroutine cmd_cuts_execute (cuts, global)
    type(cmd_cuts_t), intent(inout), target :: cuts
    type(rt_data_t), intent(inout), target :: global
    global%pn_cuts_lexpr => cuts%pn_lexpr
  end subroutine cmd_cuts_execute

  subroutine cmd_scale_compile (scale, pn)
    type(cmd_scale_t), pointer :: scale
    type(parse_node_t), intent(in), target :: pn
    allocate (scale)
    scale%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_scale_compile

  subroutine cmd_fac_scale_compile (scale, pn)
    type(cmd_fac_scale_t), pointer :: scale
    type(parse_node_t), intent(in), target :: pn
    allocate (scale)
    scale%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_fac_scale_compile

  subroutine cmd_ren_scale_compile (scale, pn)
    type(cmd_ren_scale_t), pointer :: scale
    type(parse_node_t), intent(in), target :: pn
    allocate (scale)
    scale%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_ren_scale_compile

  subroutine cmd_scale_execute (scale, global)
    type(cmd_scale_t), intent(inout), target :: scale
    type(rt_data_t), intent(inout), target :: global
    global%pn_scale_expr => scale%pn_expr
  end subroutine cmd_scale_execute

  subroutine cmd_fac_scale_execute (scale, global)
    type(cmd_fac_scale_t), intent(inout), target :: scale
    type(rt_data_t), intent(inout), target :: global
    global%pn_fac_scale_expr => scale%pn_expr
  end subroutine cmd_fac_scale_execute

  subroutine cmd_ren_scale_execute (scale, global)
    type(cmd_ren_scale_t), intent(inout), target :: scale
    type(rt_data_t), intent(inout), target :: global
    global%pn_ren_scale_expr => scale%pn_expr    
  end subroutine cmd_ren_scale_execute

  subroutine cmd_weight_compile (weight, pn)
    type(cmd_weight_t), pointer :: weight
    type(parse_node_t), intent(in), target :: pn
    allocate (weight)
    weight%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_weight_compile

  subroutine cmd_weight_execute (weight, global)
    type(cmd_weight_t), intent(inout), target :: weight
    type(rt_data_t), intent(inout), target :: global
    global%pn_weight_expr => weight%pn_expr
  end subroutine cmd_weight_execute

  subroutine cmd_selection_compile (selection, pn)
    type(cmd_selection_t), pointer :: selection
    type(parse_node_t), intent(in), target :: pn
    allocate (selection)
    selection%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_selection_compile

  subroutine cmd_selection_execute (selection, global)
    type(cmd_selection_t), intent(inout), target :: selection
    type(rt_data_t), intent(inout), target :: global
    global%pn_selection_lexpr => selection%pn_expr
  end subroutine cmd_selection_execute

  subroutine cmd_reweight_compile (reweight, pn)
    type(cmd_reweight_t), pointer :: reweight
    type(parse_node_t), intent(in), target :: pn
    allocate (reweight)
    reweight%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_reweight_compile

  subroutine cmd_reweight_execute (reweight, global)
    type(cmd_reweight_t), intent(inout), target :: reweight
    type(rt_data_t), intent(inout), target :: global
    global%pn_reweight_expr => reweight%pn_expr
  end subroutine cmd_reweight_execute

  subroutine cmd_integrate_final (integrate)
    type(cmd_integrate_t), intent(inout) :: integrate
    if (associated (integrate%options)) then
       call command_list_final (integrate%options)
       deallocate (integrate%options)
    end if
  end subroutine cmd_integrate_final

  subroutine cmd_integrate_compile (integrate, pn, global)
    type(cmd_integrate_t), pointer :: integrate
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_proclist, pn_proc, pn_opt
    integer :: i
    pn_proclist => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_proclist)
    allocate (integrate)
    call rt_data_local_init (integrate%local, global)
    if (associated (pn_opt)) then
       allocate (integrate%options)
       call command_list_compile (integrate%options, pn_opt, integrate%local)
    end if
    integrate%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (integrate%process_id (integrate%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, integrate%n_proc
       integrate%process_id(i) = parse_node_get_string (pn_proc)
       call var_list_init_process_results (global%var_list, &
            integrate%process_id (i))
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
    call rt_data_local_reset (integrate%local)
  end subroutine cmd_integrate_compile

  subroutine cmd_integrate_execute (integrate, global)
    type(cmd_integrate_t), intent(inout), target :: integrate
    type(rt_data_t), intent(inout), target :: global
    call rt_data_link (integrate%local, global)
    if (associated (integrate%options)) then
       call command_list_execute (integrate%options, integrate%local)
    end if
    call integrate_process &
         (integrate%process_id, integrate%local, global%var_list)
    call rt_data_restore (global, integrate%local)
  end subroutine cmd_integrate_execute

  subroutine cmd_me_test_final (me_test)
    type(cmd_me_test_t), intent(inout) :: me_test
    if (associated (me_test%options)) then
       call command_list_final (me_test%options)
       deallocate (me_test%options)
    end if
  end subroutine cmd_me_test_final

  subroutine cmd_me_test_compile (me_test, pn, global)
    type(cmd_me_test_t), pointer :: me_test
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_proclist, pn_proc, pn_opt
    integer :: i
    pn_proclist => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_proclist)
    allocate (me_test)
    call rt_data_local_init (me_test%local, global)
    if (associated (pn_opt)) then
       allocate (me_test%options)
       call command_list_compile (me_test%options, pn_opt, me_test%local)
    end if
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    me_test%process_id = parse_node_get_string (pn_proc)
    call rt_data_local_reset (me_test%local)
  end subroutine cmd_me_test_compile

  subroutine cmd_me_test_execute (me_test, global)
    type(cmd_me_test_t), intent(inout), target :: me_test
    type(rt_data_t), intent(inout), target :: global
    call rt_data_link (me_test%local, global)
    if (associated (me_test%options)) then
       call command_list_execute (me_test%options, me_test%local)
    end if
    call me_test_process &
         (me_test%process_id, me_test%local, global%var_list)
    call rt_data_restore (global, me_test%local)
  end subroutine cmd_me_test_execute

  subroutine cmd_observable_final (observable)
    type(cmd_observable_t), intent(inout) :: observable
    if (associated (observable%options)) then
       call command_list_final (observable%options)
       deallocate (observable%options)
    end if
  end subroutine cmd_observable_final

  subroutine cmd_observable_compile (observable, pn, global)
    type(cmd_observable_t), pointer :: observable
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_tag, pn_opt
    pn_tag => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_tag)) then
       pn_opt => parse_node_get_next_ptr (pn_tag)
    else
       pn_opt => null ()
    end if       
    allocate (observable)
    call rt_data_local_init (observable%local, global)
    if (associated (pn_opt)) then
       allocate (observable%options)
       call command_list_compile (observable%options, pn_opt, observable%local)
    end if
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       observable%id = parse_node_get_string (pn_tag)
    case default
       observable%use_id_expr = .true.
       observable%pn_id => pn_tag
    end select
    call rt_data_local_reset (observable%local)
  end subroutine cmd_observable_compile

  subroutine cmd_observable_execute (observable, global)
    type(cmd_observable_t), intent(inout), target :: observable
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: label, obs_unit, title
    type(graph_options_t) :: graph_options
    call rt_data_link (observable%local, global)
    if (associated (observable%options)) then
       call command_list_execute (observable%options, observable%local)
    end if
    if (observable%use_id_expr) then
       observable%id = eval_string (observable%pn_id, observable%local%var_list)
    end if
    label = var_list_get_sval &
         (observable%local%var_list, var_str ("$obs_label"))
    obs_unit = var_list_get_sval &
         (observable%local%var_list, var_str ("$obs_unit"))
    title = var_list_get_sval (observable%local%var_list, var_str ("$title")) 
    call graph_options_init (graph_options)
    call analysis_init_observable &
         (observable%id, label, obs_unit, graph_options)
    call rt_data_restore (global, observable%local)
  end subroutine cmd_observable_execute

  subroutine cmd_histogram_final (histogram)
    type(cmd_histogram_t), intent(inout) :: histogram
    if (associated (histogram%options)) then
       call command_list_final (histogram%options)
       deallocate (histogram%options)
    end if
  end subroutine cmd_histogram_final

  subroutine cmd_histogram_compile (histogram, pn, global)
    type(cmd_histogram_t), pointer :: histogram
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_tag, pn_args, pn_arg1, pn_arg2, pn_arg3
    type(parse_node_t), pointer :: pn_opt
    character(*), parameter :: e_illegal_use = &
       "illegal usage of 'histogram': insufficient number of arguments"
    pn_tag => parse_node_get_sub_ptr (pn, 2)
    pn_args => parse_node_get_next_ptr (pn_tag)
    if (associated (pn_args)) then
       pn_arg1 => parse_node_get_sub_ptr (pn_args)
       if (.not. associated (pn_arg1)) call msg_fatal (e_illegal_use)
       pn_arg2 => parse_node_get_next_ptr (pn_arg1)
       if (.not. associated (pn_arg2)) call msg_fatal (e_illegal_use)
       pn_arg3 => parse_node_get_next_ptr (pn_arg2)
       pn_opt => parse_node_get_next_ptr (pn_args)
    else
       pn_opt => null ()
    end if       
    allocate (histogram)
    call rt_data_local_init (histogram%local, global)
    if (associated (pn_opt)) then
       allocate (histogram%options)
       call command_list_compile (histogram%options, pn_opt, histogram%local)
    end if
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       histogram%id = parse_node_get_string (pn_tag)
    case default
       histogram%use_id_expr = .true.
       histogram%pn_id => pn_tag
    end select
    histogram%pn_lower_bound => pn_arg1
    histogram%pn_upper_bound => pn_arg2
    histogram%pn_bin_width => pn_arg3
    call rt_data_local_reset (histogram%local)
  end subroutine cmd_histogram_compile

  subroutine cmd_histogram_execute (histogram, global)
    type(cmd_histogram_t), intent(inout), target :: histogram
    type(rt_data_t), intent(inout), target :: global
    real(default) :: lower_bound, upper_bound, bin_width
    integer :: bin_number
    logical :: bin_width_is_used, normalize_bins
    type(string_t) :: obs_label, obs_unit
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options

    call rt_data_link (histogram%local, global)
    if (associated (histogram%options)) then
       call command_list_execute (histogram%options, histogram%local)
    end if

    if (histogram%use_id_expr) then
       histogram%id = eval_string (histogram%pn_id, histogram%local%var_list)
    end if

    lower_bound = eval_real (histogram%pn_lower_bound, histogram%local%var_list)
    upper_bound = eval_real (histogram%pn_upper_bound, histogram%local%var_list)
    if (associated (histogram%pn_bin_width)) then
       bin_width = eval_real (histogram%pn_bin_width, histogram%local%var_list)
       bin_width_is_used = .true.
    else if (var_list_is_known &
         (histogram%local%var_list, var_str ("n_bins"))) then
       bin_number = var_list_get_ival &
            (histogram%local%var_list, var_str ("n_bins"))
       bin_width_is_used = .false.
    else
       call msg_error ("Histogram '" // char (histogram%id) // &
            "': neither bin width nor number is defined")
    end if
    normalize_bins = var_list_get_lval &
         (histogram%local%var_list, var_str ("?normalize_bins"))
    obs_label = var_list_get_sval &
         (histogram%local%var_list, var_str ("$obs_label"))
    obs_unit = var_list_get_sval &
         (histogram%local%var_list, var_str ("$obs_unit"))

    call graph_options_init (graph_options)
    call set_graph_options (graph_options, histogram%local%var_list)
    call drawing_options_init_histogram (drawing_options)
    call set_drawing_options (drawing_options, histogram%local%var_list)

    if (bin_width_is_used) then
       call analysis_init_histogram &
            (histogram%id, lower_bound, upper_bound, bin_width, &
             normalize_bins, &
             obs_label, obs_unit, &
             graph_options, drawing_options)
    else
       call analysis_init_histogram &
            (histogram%id, lower_bound, upper_bound, bin_number, &
             normalize_bins, &
             obs_label, obs_unit, &
             graph_options, drawing_options)
    end if
    call rt_data_restore (global, histogram%local)

  end subroutine cmd_histogram_execute

  subroutine set_graph_options (gro, var_list)
    type(graph_options_t), intent(inout) :: gro
    type(var_list_t), intent(in) :: var_list
    call graph_options_set (gro, title = &
         var_list_get_sval (var_list, var_str ("$title")))
    call graph_options_set (gro, description = &
         var_list_get_sval (var_list, var_str ("$description")))
    call graph_options_set (gro, x_label = &
         var_list_get_sval (var_list, var_str ("$x_label")))
    call graph_options_set (gro, y_label = &
         var_list_get_sval (var_list, var_str ("$y_label")))
    call graph_options_set (gro, width_mm = &
         var_list_get_ival (var_list, var_str ("graph_width_mm")))
    call graph_options_set (gro, height_mm = &
         var_list_get_ival (var_list, var_str ("graph_height_mm")))
    call graph_options_set (gro, x_log = &
         var_list_get_lval (var_list, var_str ("?x_log")))
    call graph_options_set (gro, y_log = &
         var_list_get_lval (var_list, var_str ("?y_log")))
    if (var_list_is_known (var_list, var_str ("x_min"))) &
         call graph_options_set (gro, x_min = &
         var_list_get_rval (var_list, var_str ("x_min")))
    if (var_list_is_known (var_list, var_str ("x_max"))) &
         call graph_options_set (gro, x_max = &
         var_list_get_rval (var_list, var_str ("x_max")))
    if (var_list_is_known (var_list, var_str ("y_min"))) &
         call graph_options_set (gro, y_min = &
         var_list_get_rval (var_list, var_str ("y_min")))
    if (var_list_is_known (var_list, var_str ("y_max"))) &
         call graph_options_set (gro, y_max = &
         var_list_get_rval (var_list, var_str ("y_max")))
    call graph_options_set (gro, gmlcode_bg = &
         var_list_get_sval (var_list, var_str ("$gmlcode_bg")))
    call graph_options_set (gro, gmlcode_fg = &
         var_list_get_sval (var_list, var_str ("$gmlcode_fg")))
  end subroutine set_graph_options

  subroutine set_drawing_options (dro, var_list)
    type(drawing_options_t), intent(inout) :: dro
    type(var_list_t), intent(in) :: var_list
    if (var_list_is_known (var_list, var_str ("?draw_histogram"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_histogram"))) then
          call drawing_options_set (dro, with_hbars = .true.)
       else
          call drawing_options_set (dro, with_hbars = .false., &
               with_base = .false., fill = .false., piecewise = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?draw_base"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_base"))) then
          call drawing_options_set (dro, with_base = .true.)
       else
          call drawing_options_set (dro, with_base = .false., fill = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?draw_piecewise"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_piecewise"))) then
          call drawing_options_set (dro, piecewise = .true.)
       else
          call drawing_options_set (dro, piecewise = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?fill_curve"))) then
       if (var_list_get_lval (var_list, var_str ("?fill_curve"))) then
          call drawing_options_set (dro, fill = .true., with_base = .true.)
       else
          call drawing_options_set (dro, fill = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?draw_curve"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_curve"))) then
          call drawing_options_set (dro, draw = .true.)
       else
          call drawing_options_set (dro, draw = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?draw_errors"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_errors"))) then
          call drawing_options_set (dro, err = .true.)
       else
          call drawing_options_set (dro, err = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("?draw_symbols"))) then
       if (var_list_get_lval (var_list, var_str ("?draw_symbols"))) then
          call drawing_options_set (dro, symbols = .true.)
       else
          call drawing_options_set (dro, symbols = .false.)
       end if
    end if
    if (var_list_is_known (var_list, var_str ("$fill_options"))) then
       call drawing_options_set (dro, fill_options = &
            var_list_get_sval (var_list, var_str ("$fill_options")))
    end if
    if (var_list_is_known (var_list, var_str ("$draw_options"))) then
       call drawing_options_set (dro, draw_options = &
            var_list_get_sval (var_list, var_str ("$draw_options")))
    end if
    if (var_list_is_known (var_list, var_str ("$err_options"))) then
       call drawing_options_set (dro, err_options = &
            var_list_get_sval (var_list, var_str ("$err_options")))
    end if
    if (var_list_is_known (var_list, var_str ("$symbol"))) then
       call drawing_options_set (dro, symbol = &
            var_list_get_sval (var_list, var_str ("$symbol")))
    end if
    if (var_list_is_known (var_list, var_str ("$gmlcode_bg"))) then
       call drawing_options_set (dro, gmlcode_bg = &
            var_list_get_sval (var_list, var_str ("$gmlcode_bg")))
    end if
    if (var_list_is_known (var_list, var_str ("$gmlcode_fg"))) then
       call drawing_options_set (dro, gmlcode_fg = &
            var_list_get_sval (var_list, var_str ("$gmlcode_fg")))
    end if
  end subroutine set_drawing_options

  subroutine cmd_plot_final (plot)
    type(cmd_plot_t), intent(inout) :: plot
    if (associated (plot%options)) then
       call command_list_final (plot%options)
       deallocate (plot%options)
    end if
  end subroutine cmd_plot_final

  subroutine cmd_plot_compile (plot, pn, global)
    type(cmd_plot_t), pointer :: plot
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_tag
    type(parse_node_t), pointer :: pn_opt
    pn_tag => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_tag)
    allocate (plot)
    call cmd_plot_init (plot, pn_tag, pn_opt, global)
  end subroutine cmd_plot_compile

  subroutine cmd_plot_init (plot, pn_tag, pn_opt, global)
    type(cmd_plot_t), intent(out) :: plot
    type(parse_node_t), intent(in), pointer :: pn_tag, pn_opt
    type(rt_data_t), intent(in), target :: global
    call rt_data_local_init (plot%local, global)
    if (associated (pn_opt)) then
       allocate (plot%options)
       call command_list_compile (plot%options, pn_opt, plot%local)
    end if
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       plot%id = parse_node_get_string (pn_tag)
    case default
       plot%use_id_expr = .true.
       plot%pn_id => pn_tag
    end select
    call rt_data_local_reset (plot%local)
  end subroutine cmd_plot_init

  subroutine cmd_plot_execute (plot, global)
    type(cmd_plot_t), intent(inout), target :: plot
    type(rt_data_t), intent(inout), target :: global
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options

    call rt_data_link (plot%local, global)
    if (associated (plot%options)) then
       call command_list_execute (plot%options, plot%local)
    end if

    if (plot%use_id_expr) then
       plot%id = eval_string (plot%pn_id, plot%local%var_list)
    end if

    call graph_options_init (graph_options)
    call set_graph_options (graph_options, plot%local%var_list)
    call drawing_options_init_plot (drawing_options)
    call set_drawing_options (drawing_options, plot%local%var_list)

    call analysis_init_plot (plot%id, graph_options, drawing_options)

    call rt_data_restore (global, plot%local)

  end subroutine cmd_plot_execute

  subroutine cmd_graph_final (graph)
    type(cmd_graph_t), intent(inout) :: graph
    integer :: i
    if (allocated (graph%el)) then
       do i = 1, size (graph%el)
          call cmd_plot_final (graph%el(i))
       end do
       deallocate (graph%el)
    end if
    if (associated (graph%options)) then
       call command_list_final (graph%options)
       deallocate (graph%options)
    end if
  end subroutine cmd_graph_final

  subroutine cmd_graph_compile (graph, pn, global)
    type(cmd_graph_t), pointer :: graph
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_term, pn_tag, pn_opt, pn_def, pn_app
    integer :: i

    pn_term => parse_node_get_sub_ptr (pn, 2)
    pn_tag => parse_node_get_sub_ptr (pn_term)
    pn_opt => parse_node_get_next_ptr (pn_tag)
    allocate (graph)
    call rt_data_local_init (graph%local, global)
    if (associated (pn_opt)) then
       allocate (graph%options)
       call command_list_compile (graph%options, pn_opt, graph%local)
    end if
    select case (char (parse_node_get_rule_key (pn_tag)))
    case ("analysis_id")
       graph%id = parse_node_get_string (pn_tag)
    case default
       graph%use_id_expr = .true.
       graph%pn_id => pn_tag
    end select
    pn_def => parse_node_get_next_ptr (pn_term, 2)
    graph%n_elements = parse_node_get_n_sub (pn_def)
    call rt_data_local_reset (graph%local)

    allocate (graph%el (graph%n_elements))
    pn_term => parse_node_get_sub_ptr (pn_def)
    pn_tag => parse_node_get_sub_ptr (pn_term)
    pn_opt => parse_node_get_next_ptr (pn_tag)
    call cmd_plot_init (graph%el(1), pn_tag, pn_opt, global)
    pn_app => parse_node_get_next_ptr (pn_term)
    do i = 2, graph%n_elements
       pn_term => parse_node_get_sub_ptr (pn_app, 2)
       pn_tag => parse_node_get_sub_ptr (pn_term)
       pn_opt => parse_node_get_next_ptr (pn_tag)
       call cmd_plot_init (graph%el(i), pn_tag, pn_opt, global)
       pn_app => parse_node_get_next_ptr (pn_app)
    end do

  end subroutine cmd_graph_compile

  subroutine cmd_graph_execute (graph, global)
    type(cmd_graph_t), intent(inout), target :: graph
    type(rt_data_t), intent(inout), target :: global
    type(graph_options_t) :: graph_options
    type(drawing_options_t) :: drawing_options
    integer :: i, type

    call rt_data_link (graph%local, global)
    if (associated (graph%options)) then
       call command_list_execute (graph%options, graph%local)
    end if

    if (graph%use_id_expr) then
       graph%id = eval_string (graph%pn_id, graph%local%var_list)
    end if

    call graph_options_init (graph_options)
    call set_graph_options (graph_options, graph%local%var_list)
    call analysis_init_graph (graph%id, graph%n_elements, graph_options)

    do i = 1, graph%n_elements
       call rt_data_link (graph%el(i)%local, global)
       if (associated (graph%el(i)%options)) then
          call command_list_execute (graph%el(i)%options, graph%el(i)%local)
       end if
       if (graph%el(i)%use_id_expr) then
          graph%el(i)%id = &
               eval_string (graph%el(i)%pn_id, graph%el(i)%local%var_list)
       end if
       type = analysis_store_get_object_type (graph%el(i)%id)
       select case (type)
       case (AN_HISTOGRAM)
            call drawing_options_init_histogram (drawing_options)
       case (AN_PLOT)
            call drawing_options_init_plot (drawing_options)
       end select
       call set_drawing_options (drawing_options, graph%local%var_list)
       if (associated (graph%el(i)%options)) then
          call set_drawing_options (drawing_options, graph%el(i)%local%var_list)
       end if
       call analysis_fill_graph (graph%id, i, graph%el(i)%id, drawing_options)
       call rt_data_restore (global, graph%el(i)%local)
    end do

    call rt_data_restore (global, graph%local)

  end subroutine cmd_graph_execute

  subroutine cmd_analysis_compile (analysis, pn)
    type(cmd_analysis_t), pointer :: analysis
    type(parse_node_t), intent(in), target :: pn
    allocate (analysis)
    analysis%pn_lexpr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_analysis_compile

  subroutine cmd_analysis_execute (analysis, global)
    type(cmd_analysis_t), intent(inout), target :: analysis
    type(rt_data_t), intent(inout), target :: global
    global%pn_analysis_lexpr => analysis%pn_lexpr
  end subroutine cmd_analysis_execute

  subroutine cmd_clear_final (clear)
    type(cmd_clear_t), intent(inout) :: clear
    if (allocated (clear%pn_id))  deallocate (clear%pn_id)
  end subroutine cmd_clear_final

  subroutine cmd_clear_compile (clear, pn, global)
    type(cmd_clear_t), pointer :: clear
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_args, pn_tag
    type(string_t) :: key
    integer :: i
    pn_args => parse_node_get_sub_ptr (pn, 2)
    allocate (clear)
    if (associated (pn_args)) then
       clear%n_args = parse_node_get_n_sub (pn_args)
       allocate (clear%id (clear%n_args), clear%use_id_expr (clear%n_args))
       clear%use_id_expr = .false.
       allocate (clear%pn_id (clear%n_args))
       pn_tag => parse_node_get_sub_ptr (pn_args)
       i = 1
       do while (associated (pn_tag))
          key = parse_node_get_rule_key (pn_tag)
          select case (char (key))
          case ("iterations", "cuts", "weight", "scale", "factorization_scale", &
                "renormalization_scale", "analysis", "expect")
             clear%id(i) = key
          case ("analysis_id")
             clear%id(i) = parse_node_get_string (pn_tag)
          case default
             clear%pn_id(i)%ptr => pn_tag
          end select
          i = i + 1
          pn_tag => parse_node_get_next_ptr (pn_tag)
       end do
    end if
  end subroutine cmd_clear_compile

  subroutine cmd_clear_execute (clear, global)
    type(cmd_clear_t), intent(inout), target :: clear
    type(rt_data_t), intent(inout), target :: global
    integer :: i
    if (clear%n_args == 0) then
       call analysis_clear ()
       call msg_message ("Cleared all analysis objects")
    else
       do i = 1, clear%n_args
          select case (char (clear%id(i)))
          case ("iterations")
             call iterations_list_clear (global%it_list)
             call msg_message ("Cleared iteration list setup")
          case ("cuts")
             global%pn_cuts_lexpr => null ()
             call msg_message ("Cleared cut setup")
          case ("weight")
             global%pn_weight_expr => null ()
             call msg_message ("Cleared integration weight setup")
          case ("scale")
             global%pn_scale_expr => null ()
             call msg_message ("Cleared general scale setup")        
          case ("factorization_scale")
             global%pn_fac_scale_expr => null ()
             call msg_message ("Cleared factorization scale setup")
          case ("renormalization_scale")
             global%pn_ren_scale_expr => null ()
             call msg_message ("Cleared renormalization scale setup")        
          case ("analysis")
             global%pn_analysis_lexpr => null ()
             call msg_message ("Cleared analysis setup")
          case ("expect")
             call expect_clear ()
             call msg_message ("Cleared counters of value checks")
          case default
             if (clear%use_id_expr(i)) then
                clear%id(i) = eval_string (clear%pn_id(i)%ptr, global%var_list)
             end if
             call analysis_clear (clear%id(i))
             call msg_message ("Cleared analysis object '" &
                  // char (clear%id(i)) // "'")
          end select
       end do
    end if
  end subroutine cmd_clear_execute

  subroutine cmd_write_analysis_final (write_analysis)
    type(cmd_write_analysis_t), intent(inout) :: write_analysis
    if (allocated (write_analysis%id))  deallocate (write_analysis%id)
    if (allocated (write_analysis%tag))  deallocate (write_analysis%tag)
    if (associated (write_analysis%options)) then
       call command_list_final (write_analysis%options)
       deallocate (write_analysis%options)
    end if
  end subroutine cmd_write_analysis_final

  subroutine cmd_write_analysis_compile (write_analysis, pn, global)
    type(cmd_write_analysis_t), intent(inout), pointer :: write_analysis
    type(parse_node_t), intent(in) :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_clause, pn_opts, pn_args, pn_id
    integer :: n, i
    pn_clause => parse_node_get_sub_ptr (pn)
    pn_args => parse_node_get_sub_ptr (pn_clause, 2)
    pn_opts => parse_node_get_next_ptr (pn_clause)
    allocate (write_analysis)
    call rt_data_local_init (write_analysis%local, global)
    if (associated (pn_opts)) then
       allocate (write_analysis%options)
       call command_list_compile &
            (write_analysis%options, pn_opts, write_analysis%local)
    end if
    if (associated (pn_args)) then
       n = parse_node_get_n_sub (pn_args)
       allocate (write_analysis%id (n))
       do i = 1, n
           pn_id => parse_node_get_sub_ptr (pn_args, i)
           if (char (parse_node_get_rule_key (pn_id)) == "analysis_id") then
              write_analysis%id(i)%tag = parse_node_get_string (pn_id)
           else
              write_analysis%id(i)%pn_sexpr => pn_id
           end if
       end do
    else
       allocate (write_analysis%id (0))
    end if
    call rt_data_local_reset (write_analysis%local)
  end subroutine cmd_write_analysis_compile

  subroutine cmd_write_analysis_execute &
       (write_analysis, global, data_file, write_yerr, write_xerr)
    type(cmd_write_analysis_t), intent(inout), target :: write_analysis
    type(rt_data_t), intent(inout), target :: global
    type(string_t), intent(out), optional :: data_file
    logical, intent(out), optional :: write_yerr, write_xerr
    type(string_t) :: defaultfile, file
    logical :: keep_open, custom, header, columns
    type(string_t) :: comment_prefix, separator, extension
    integer :: i, type
    type(ifile_t) :: ifile
    logical :: one_file, has_writer
    type(analysis_iterator_t) :: iterator
    type(rt_data_t), target :: sandbox
    type(command_list_t) :: writer

    call rt_data_link (write_analysis%local, global)
    if (associated (write_analysis%options)) then
       call command_list_execute (write_analysis%options, write_analysis%local)
    end if

    defaultfile = var_list_get_sval (write_analysis%local%var_list, &
         var_str ("$out_file"))
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
       keep_open = file_list_is_open (global%out_files, file, &
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
          print *, "calling file_list_open from cmd_write_analysis_execute"
          call file_list_open (global%out_files, file, &
               action = "write", status = "replace", position = "asis")
          call msg_message ("Writing analysis data to file '" &
               // char (file) // "'")
       end if
    end if

    if (present (data_file)) then
       custom = .false.
    else
       custom = var_list_get_lval (write_analysis%local%var_list, &
           var_str ("?out_custom"))
    end if
    comment_prefix = var_list_get_sval (write_analysis%local%var_list, &
         var_str ("$out_comment"))
    separator = var_list_get_sval (write_analysis%local%var_list, &
         var_str ("$out_separator"))
    columns = var_list_get_lval (write_analysis%local%var_list, &
         var_str ("?out_columns"))
    header = var_list_get_lval (write_analysis%local%var_list, &
         var_str ("?out_header"))
    call get_analysis_tags (write_analysis%tag, write_analysis%id, &
         write_analysis%local%var_list)
    if (present (write_yerr)) then
       write_yerr = var_list_get_lval (write_analysis%local%var_list, &
            var_str ("?out_yerr"))
    end if
    if (present (write_xerr)) then
       write_xerr = var_list_get_lval (write_analysis%local%var_list, &
            var_str ("?out_xerr"))
    end if

    do i = 1, size (write_analysis%tag)
       if (custom) then
          type = analysis_store_get_object_type (write_analysis%tag(i))
          call compile_writer (writer, type, has_writer)
          if (has_writer) then
             call rt_data_link (sandbox, write_analysis%local)
          end if
          if (.not. one_file) then
             file = write_analysis%tag(i) // ".dat"
             call file_list_open (global%out_files, file, &
                  action = "write", status = "replace", position = "asis")
             call msg_message ("Writing analysis data to file '" &
                  // char (file) // "'")
          end if
          if (header) then
             call analysis_get_header &
                  (write_analysis%tag(i), ifile, comment_prefix)
             call file_list_write (global%out_files, file, ifile)
             call ifile_final (ifile)
          end if
          call analysis_init_iterator (write_analysis%tag(i), iterator)
          do while (analysis_iterator_is_valid (iterator))
             call write_data (iterator, &
                  writer, sandbox, file, columns, separator)
             call analysis_iterator_advance (iterator)
          end do
          if (.not. one_file) then
             call file_list_close (global%out_files, file)
          end if       
          if (has_writer) then
             call rt_data_restore (write_analysis%local, sandbox)
             call var_list_final (sandbox%var_list)
             call command_list_final (writer)
          end if
       else
          call file_list_write_analysis &
               (global%out_files, file, write_analysis%tag(i))
       end if
    end do

    if (one_file .and. .not. keep_open) then
       call file_list_close (global%out_files, file)
    end if       
    call rt_data_restore (global, write_analysis%local)

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
         call analysis_store_get_ids (analysis_tag)
      end if
    end subroutine get_analysis_tags

    subroutine compile_writer (writer, type, has_writer)
      type(command_list_t), intent(out) :: writer
      integer, intent(in) :: type
      logical, intent(out) :: has_writer
      select case (type)
      case (AN_HISTOGRAM)
         has_writer = associated (write_analysis%local%pn_histogram_writer)
         if (has_writer) then
            call histogram_writer_compile (writer, &
                 write_analysis%local%pn_histogram_writer, sandbox, &
                 write_analysis%local)
         end if
      case (AN_PLOT)
         has_writer = associated (write_analysis%local%pn_plot_writer)
         if (has_writer) then
            call plot_writer_compile (writer, &
                 write_analysis%local%pn_plot_writer, sandbox, &
                 write_analysis%local)
         end if
      case default
         has_writer = .false.
         call msg_error ("Custom output format " &
              // "is applicable only to elementary histograms and plots")
      end select
    end subroutine compile_writer

    subroutine write_data (iterator, writer, sandbox, file, columns, separator)
      type(analysis_iterator_t), intent(inout) :: iterator
      type(command_list_t), intent(in) :: writer
      type(rt_data_t), intent(inout), target :: sandbox
      type(string_t), intent(in) :: file
      logical, intent(in) :: columns
      type(string_t), intent(in) :: separator
      real(default) :: x, y, yerr, xerr, excess, width
      integer :: bin_index, n_bins, point_index, n_points
      character(*), parameter :: &
           OUT_REAL_FMT = '(' // HISTOGRAM_DATA_FORMAT // ')'
      select case (analysis_iterator_get_type (iterator))
      case (AN_HISTOGRAM)
         call analysis_iterator_get_data (iterator, &
            x = x, y = y, yerr = yerr, width = width, excess = excess, &
            index = bin_index, n_total = n_bins)
         if (has_writer) then
            call histogram_writer_execute (writer, sandbox, file, &
                 x, width, y, yerr, excess, bin_index, n_bins)
         else if (columns) then
            call file_list_write (global%out_files, file, &
                 real2string (x, OUT_REAL_FMT) // separator // &
                 real2string (y, OUT_REAL_FMT) // separator // &
                 real2string (yerr, OUT_REAL_FMT))
         else
            call file_list_write (global%out_files, file, &
                 real2string (x) // separator // &
                 real2string (y) // separator // &
                 real2string (yerr))
         end if
      case (AN_PLOT)
         call analysis_iterator_get_data (iterator, &
            x = x, y = y, yerr = yerr, xerr = xerr, &
            index = point_index, n_total = n_points)
         if (has_writer) then
            call plot_writer_execute (writer, sandbox, file, &
                 x, y, yerr, xerr, point_index, n_points)
         else if (columns) then
            call file_list_write (global%out_files, file, &
                 real2string (x, OUT_REAL_FMT) // separator // &
                 real2string (y, OUT_REAL_FMT) // separator // &
                 real2string (yerr, OUT_REAL_FMT) // separator // &
                 real2string (xerr, OUT_REAL_FMT))
         else
            call file_list_write (global%out_files, file, &
                 real2string (x) // separator // &
                 real2string (y) // separator // &
                 real2string (yerr) // separator // &
                 real2string (xerr))
         end if
      end select
    end subroutine write_data

  end subroutine cmd_write_analysis_execute

  subroutine histogram_writer_compile (writer, pn, local, global)
    type(command_list_t), intent(out) :: writer
    type(parse_node_t), intent(in) :: pn
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    call rt_data_local_init (local, global)
    call var_list_append_real (local%var_list, &
         var_str ("bin_center"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("bin_width"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("bin_sum"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("bin_error"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("bin_excess"), intrinsic=.true.)
    call var_list_append_int (local%var_list, &
         var_str ("bin_index"), intrinsic=.true.)
    call var_list_append_int (local%var_list, &
         var_str ("n_bins"), intrinsic=.true.)
    call var_list_append_string (local%var_list, &
         var_str ("$out_file"), intrinsic=.true.)
    call command_list_compile (writer, pn, local)
    call rt_data_local_reset (local)
  end subroutine histogram_writer_compile

  subroutine plot_writer_compile (writer, pn, local, global)
    type(command_list_t), intent(out) :: writer
    type(parse_node_t), intent(in) :: pn
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    call rt_data_local_init (local, global)
    call var_list_append_real (local%var_list, &
         var_str ("point_x"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("point_y"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("point_yerr"), intrinsic=.true.)
    call var_list_append_real (local%var_list, &
         var_str ("point_xerr"), intrinsic=.true.)
    call var_list_append_int (local%var_list, &
         var_str ("point_index"), intrinsic=.true.)
    call var_list_append_int (local%var_list, &
         var_str ("n_points"), intrinsic=.true.)
    call var_list_append_string (local%var_list, &
         var_str ("$out_file"), intrinsic=.true.)
    call command_list_compile (writer, pn, local)
    call rt_data_local_reset (local)
  end subroutine plot_writer_compile

  subroutine histogram_writer_execute (writer, local, filename, &
      x, width, y, yerr, excess, bin_index, n_bins)
    type(command_list_t), intent(in) :: writer
    type(rt_data_t), intent(inout), target :: local
    type(string_t), intent(in) :: filename
    real(default), intent(in) :: x, width, y, yerr, excess
    integer, intent(in) :: bin_index, n_bins
    call var_list_set_real (local%var_list, &
         var_str ("bin_center"), x, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("bin_width"), width, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("bin_sum"), y, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("bin_error"), yerr, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("bin_excess"), excess, is_known=.true.)
    call var_list_set_int (local%var_list, &
         var_str ("bin_index"), bin_index, is_known=.true.)
    call var_list_set_int (local%var_list, &
         var_str ("n_bins"), n_bins, is_known=.true.)
    call var_list_set_string (local%var_list, &
         var_str ("$out_file"), filename, is_known=.true.)
    call command_list_execute (writer, local)
  end subroutine histogram_writer_execute

  subroutine plot_writer_execute (writer, local, filename, &
      x, y, yerr, xerr, point_index, n_points)
    type(command_list_t), intent(in) :: writer
    type(rt_data_t), intent(inout), target :: local
    type(string_t), intent(in) :: filename
    real(default), intent(in) :: x, y, yerr, xerr
    integer, intent(in) :: point_index, n_points
    call var_list_set_real (local%var_list, &
         var_str ("point_x"), x, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("point_y"), y, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("point_yerr"), yerr, is_known=.true.)
    call var_list_set_real (local%var_list, &
         var_str ("point_xerr"), xerr, is_known=.true.)
    call var_list_set_int (local%var_list, &
         var_str ("point_index"), point_index, is_known=.true.)
    call var_list_set_int (local%var_list, &
         var_str ("n_points"), n_points, &
         is_known=.true.)
    call var_list_set_string (local%var_list, &
         var_str ("$out_file"), filename, is_known=.true.)
    call command_list_execute (writer, local)
  end subroutine plot_writer_execute

  subroutine cmd_xxx_writer_final (writer)
    type(cmd_xxx_writer_t), intent(inout) :: writer
    nullify (writer%macro)
    writer%type = CMD_HISTOGRAM_WRITER
  end subroutine cmd_xxx_writer_final

  subroutine cmd_xxx_writer_compile (writer, pn, global)
    type(cmd_xxx_writer_t), intent(inout), pointer :: writer
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in) :: global
    allocate (writer)
    select case (char (parse_node_get_rule_key (pn)))
       case ("cmd_histogram_writer")
          writer%type = CMD_HISTOGRAM_WRITER
       case ("cmd_plot_writer")
          writer%type = CMD_PLOT_WRITER
       case default
          call msg_bug ("cmd_xxx_writer_compile: invalid type")
    end select
    writer%macro => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_xxx_writer_compile

  subroutine cmd_xxx_writer_execute (writer, global)
    type(cmd_xxx_writer_t), intent(in), target :: writer
    type(rt_data_t), intent(inout), target :: global
    if (.not. associated (writer%macro)) return
    select case (writer%type)
       case (CMD_HISTOGRAM_WRITER)
          global%pn_histogram_writer => writer%macro
       case (CMD_PLOT_WRITER)
          global%pn_plot_writer => writer%macro
       case default
          call msg_bug ("cmd_xxx_writer_execute: invalid type")
    end select
  end subroutine cmd_xxx_writer_execute

  subroutine cmd_compile_analysis_execute (write, global)
    type(cmd_write_analysis_t), intent(inout), target :: write
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: data_file, basename, extension, driver_file
    integer :: u_driver, i
    logical :: has_gmlcode, write_yerr, write_xerr
    call cmd_write_analysis_execute &
         (write, global, data_file, write_yerr, write_xerr)
    basename = data_file    
    if (scan (".", basename) > 0) then
      call split (basename, extension, ".", back=.true.)
    else
      extension = ""
    end if
    driver_file = basename // ".tex"
    u_driver = free_unit ()
    open (unit=u_driver, file=char(driver_file), &
          action="write", status="replace")
    call analysis_write_driver &
         (data_file, write%tag, unit=u_driver)
    close (u_driver)
    if (size (write%tag) == 0) then
       has_gmlcode = analysis_has_plots ()
    else
       has_gmlcode = analysis_has_plots (write%tag)
    end if
    call msg_message ("Compiling analysis results display in '" &
         // char (driver_file) // "'")
    call analysis_compile_tex (basename, has_gmlcode, global%os_data)
  end subroutine cmd_compile_analysis_execute

  subroutine cmd_open_final (open)
    type(cmd_open_t), intent(inout) :: open
    if (associated (open%options)) then
       call command_list_final (open%options)
       deallocate (open%options)
    end if
  end subroutine cmd_open_final

  subroutine cmd_open_out_compile (open, pn, global)
    type(cmd_open_t), intent(out), pointer :: open
    type(parse_node_t), intent(in) :: pn
    type(rt_data_t), intent(in) :: global
    type(parse_node_t), pointer :: pn_arg, pn_opt
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_arg)) then
       pn_opt => parse_node_get_next_ptr (pn_arg)
    else
       pn_opt => null ()
    end if       
    allocate (open)
    call rt_data_local_init (open%local, global)
    if (associated (pn_opt)) then
       allocate (open%options)
       call command_list_compile (open%options, pn_opt, open%local)
    end if
    open%pn_sexpr => pn_arg
    open%writing = .true.
    call rt_data_local_reset (open%local)
  end subroutine cmd_open_out_compile

  subroutine cmd_open_execute (open, global)
    type(cmd_open_t), intent(inout) :: open
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: name
    type(string_t) :: action, status, position
    call rt_data_link (open%local, global)
    if (associated (open%options)) then
       call command_list_execute (open%options, open%local)
    end if
    name = eval_string (open%pn_sexpr, open%local%var_list)
    if (open%reading .and. open%writing) then
       action = "readwrite"
    else if (open%reading) then
       action = "read"
    else if (open%writing) then
       action = "write"
    else
       call msg_bug ("Open command: neither read nor write specified.")
    end if
    status = "replace"
    position = "asis"
    call file_list_open (global%out_files, name, &
         char (action), char (status), char (position))
    call rt_data_restore (global, open%local)
  end subroutine cmd_open_execute

  subroutine cmd_close_final (close)
    type(cmd_close_t), intent(inout) :: close
    if (associated (close%options)) then
       call command_list_final (close%options)
       deallocate (close%options)
    end if
  end subroutine cmd_close_final

  subroutine cmd_close_out_compile (close, pn, global)
    type(cmd_close_t), intent(out), pointer :: close
    type(parse_node_t), intent(in) :: pn
    type(rt_data_t), intent(in) :: global
    type(parse_node_t), pointer :: pn_arg, pn_opt
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_arg)) then
       pn_opt => parse_node_get_next_ptr (pn_arg)
    else
       pn_opt => null ()
    end if       
    allocate (close)
    call rt_data_local_init (close%local, global)
    if (associated (pn_opt)) then
       allocate (close%options)
       call command_list_compile (close%options, pn_opt, close%local)
    end if
    close%pn_sexpr => pn_arg
    call rt_data_local_reset (close%local)
  end subroutine cmd_close_out_compile

  subroutine cmd_close_execute (close, global)
    type(cmd_close_t), intent(inout) :: close
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: name
    call rt_data_link (close%local, global)
    if (associated (close%options)) then
       call command_list_execute (close%options, close%local)
    end if
    name = eval_string (close%pn_sexpr, close%local%var_list)
    call file_list_close (global%out_files, name)
    call rt_data_restore (global, close%local)
  end subroutine cmd_close_execute

  subroutine cmd_printd_final (printd)
    type(cmd_printd_t), intent(inout) :: printd
    if (associated (printd%pn_sexpr)) then
       call parse_node_final (printd%pn_sexpr)
       deallocate (printd%pn_sexpr)
    end if
    if (associated (printd%options)) then
       call command_list_final (printd%options)
       deallocate (printd%options)
    end if
  end subroutine cmd_printd_final

  subroutine cmd_printd_compile (printd, pn, global)
    type(cmd_printd_t), pointer :: printd
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_arg, pn_opt
    type(parse_node_t), pointer :: pn_sexpr, pn_sprintd
    pn_cmd => parse_node_get_sub_ptr (pn)
    pn_opt => parse_node_get_next_ptr (pn_cmd)
    pn_arg => parse_node_get_sub_ptr (pn_cmd)
    allocate (printd)
    call rt_data_local_init (printd%local, global)
    if (associated (pn_opt)) then
       allocate (printd%options)
       call command_list_compile (printd%options, pn_opt, printd%local)
    end if
    call parse_node_create_branch (pn_sexpr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sexpr")))
    call parse_node_create_branch (pn_sprintd, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sprintd_fun")))
    call parse_node_append_sub (pn_sexpr, pn_sprintd)
    call parse_node_append_sub (pn_sprintd, pn_arg)
    printd%pn_sexpr => pn_sexpr
    call rt_data_local_reset (printd%local)
  end subroutine cmd_printd_compile

  subroutine cmd_printd_execute (printd, global)
    type(cmd_printd_t), intent(inout) :: printd
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: string, file
    logical :: advance
    call rt_data_link (printd%local, global)
    if (associated (printd%options)) then
       call command_list_execute (printd%options, printd%local)
    end if
    string = eval_string (printd%pn_sexpr, printd%local%var_list)
    file = var_list_get_sval (printd%local%var_list, var_str ("$out_file"))
    advance = var_list_get_lval (printd%local%var_list, &
         var_str ("?out_advance"))
    if (len (file) == 0) then
       call msg_result (char (string))
    else
       call file_list_write (global%out_files, file, string, advance)
    end if
    call rt_data_restore (global, printd%local)
  end subroutine cmd_printd_execute

  subroutine cmd_printf_final (printf)
    type(cmd_printf_t), intent(inout) :: printf
    if (associated (printf%pn_sexpr)) then
       call parse_node_final (printf%pn_sexpr)
       deallocate (printf%pn_sexpr)
    end if
    if (associated (printf%options)) then
       call command_list_final (printf%options)
       deallocate (printf%options)
    end if
  end subroutine cmd_printf_final

  subroutine cmd_printf_compile (printf, pn, global)
    type(cmd_printf_t), pointer :: printf
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_clause, pn_opt
    type(parse_node_t), pointer :: pn_sexpr, pn_sprintf
    pn_cmd => parse_node_get_sub_ptr (pn)
    pn_opt => parse_node_get_next_ptr (pn_cmd)
    pn_clause => parse_node_get_sub_ptr (pn_cmd)
    allocate (printf)
    call rt_data_local_init (printf%local, global)
    if (associated (pn_opt)) then
       allocate (printf%options)
       call command_list_compile (printf%options, pn_opt, printf%local)
    end if
    call parse_node_create_branch (pn_sexpr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sexpr")))
    call parse_node_create_branch (pn_sprintf, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("sprintf_fun")))
    call parse_node_append_sub (pn_sexpr, pn_sprintf)
    call parse_node_append_sub (pn_sprintf, pn_clause)
    printf%pn_sexpr => pn_sexpr
    call parse_node_final (pn_sprintf, recursive = .false.)
    call parse_node_final (pn_sexpr, recursive = .false.)
    call rt_data_local_reset (printf%local)
  end subroutine cmd_printf_compile

  subroutine cmd_printf_execute (printf, global)
    type(cmd_printf_t), intent(inout) :: printf
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: string, file
    logical :: advance
    call rt_data_link (printf%local, global)
    if (associated (printf%options)) then
       call command_list_execute (printf%options, printf%local)
    end if
    string = eval_string (printf%pn_sexpr, printf%local%var_list)
    file = var_list_get_sval (printf%local%var_list, var_str ("$out_file"))
    advance = var_list_get_lval (printf%local%var_list, &
         var_str ("?out_advance"))
    if (len (file) == 0) then
       call msg_result (char (string))
    else
       call file_list_write (global%out_files, file, string, advance)
    end if
    call rt_data_restore (global, printf%local)
  end subroutine cmd_printf_execute

  subroutine cmd_record_final (record)
    type(cmd_record_t), intent(inout) :: record
  end subroutine cmd_record_final

  subroutine cmd_record_compile (record, pn, global)
    type(cmd_record_t), pointer :: record
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_lexpr, pn_lsinglet, pn_lterm, pn_record
    call parse_node_create_branch (pn_lexpr, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lexpr")))
    call parse_node_create_branch (pn_lsinglet, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lsinglet")))
    call parse_node_append_sub (pn_lexpr, pn_lsinglet)
    call parse_node_create_branch (pn_lterm, &
         syntax_get_rule_ptr (syntax_cmd_list, var_str ("lterm")))
    call parse_node_append_sub (pn_lsinglet, pn_lterm)
    pn_record => parse_node_get_sub_ptr (pn)
    call parse_node_append_sub (pn_lterm, pn_record)
    allocate (record)
    record%pn_lexpr => pn_lexpr
  end subroutine cmd_record_compile

  subroutine cmd_record_execute (record, global)
    type(cmd_record_t), intent(inout), target :: record
    type(rt_data_t), intent(inout), target :: global
    logical :: lval
    lval = eval_log (record%pn_lexpr, global%var_list)
  end subroutine cmd_record_execute

  subroutine decay_properties_final (decay)
    type(decay_properties_t), intent(inout) :: decay
  end subroutine decay_properties_final

  subroutine cmd_unstable_final (unstable)
    type(cmd_unstable_t), intent(inout) :: unstable
    integer :: d
    if (allocated (unstable%decay)) then
       do d = 1, size (unstable%decay)
          call decay_properties_final (unstable%decay(d))
       end do
       deallocate (unstable%decay)
    end if
    if (associated (unstable%options)) then
       call command_list_final (unstable%options)
       deallocate (unstable%options)
    end if
  end subroutine cmd_unstable_final

  subroutine cmd_unstable_compile (unstable, pn, global)
    type(cmd_unstable_t), pointer :: unstable
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_list, pn_decl
    type(parse_node_t), pointer :: pn_prt, pn_arg, pn_proc, pn_opt
    integer :: d, i
    pn_list => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_list)) then
       pn_opt => parse_node_get_next_ptr (pn_list)
    else
       pn_opt => null ()
    end if       
    allocate (unstable)
    call rt_data_local_init (unstable%local, global)
    if (associated (pn_opt)) then
       allocate (unstable%options)
       call command_list_compile (unstable%options, pn_opt, unstable%local)
    end if
    allocate (unstable%decay (parse_node_get_n_sub (pn_list)))
    d = 0
    pn_decl => parse_node_get_sub_ptr (pn_list)
    do while (associated (pn_decl))
       d = d + 1
       pn_prt => parse_node_get_sub_ptr (pn_decl)
       pn_arg => parse_node_get_next_ptr (pn_prt)
       unstable%decay(d)%pn_pdg => pn_prt
       unstable%decay(d)%prt = "?"
       unstable%decay(d)%n_proc = parse_node_get_n_sub (pn_arg)
       allocate (unstable%decay(d)%process_id (unstable%decay(d)%n_proc))
       allocate (unstable%decay(d)%br (unstable%decay(d)%n_proc))
       pn_proc => parse_node_get_sub_ptr (pn_arg)
       do i = 1, unstable%decay(d)%n_proc
          unstable%decay(d)%process_id(i) = parse_node_get_string (pn_proc)
          pn_proc => parse_node_get_next_ptr (pn_proc)
       end do
       pn_decl => parse_node_get_next_ptr (pn_decl)
    end do
    call rt_data_local_reset (unstable%local)
  end subroutine cmd_unstable_compile

  subroutine cmd_unstable_execute (unstable, global)
    type(cmd_unstable_t), intent(inout), target :: unstable
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: aval
    type(flavor_t), dimension(:), allocatable :: flv_tmp
    type(flavor_t), dimension(:), allocatable :: flv
    real(default), dimension(:), allocatable :: integral
    real(default) :: integral_sum
    type(process_t), pointer :: process
    type(string_t) :: process_id
    integer :: proc
    logical :: isotropic_decay, diagonal_decay
    type(particle_data_t), pointer :: prt_data
    type(decay_configuration_t), pointer :: decay_conf
    integer :: u, d
    u = logfile_unit ()
    call rt_data_link (unstable%local, global)
    if (associated (unstable%options)) then
       call command_list_execute (unstable%options, unstable%local)
    end if
    isotropic_decay = var_list_get_lval (unstable%local%var_list, &
         var_str ("?isotropic_decay"))
    diagonal_decay = var_list_get_lval (unstable%local%var_list, &
         var_str ("?diagonal_decay"))
    allocate (flv (size (unstable%decay)))
    do d = 1, size (unstable%decay)
       aval = eval_pdg_array (unstable%decay(d)%pn_pdg, unstable%local%var_list)
       call flavor_init (flv_tmp, aval, unstable%local%model)
       select case (size (flv_tmp))
       case (1);  flv(d) = flv_tmp(1)
       case default
          call pdg_array_write (aval)
          call msg_fatal ("Unstable particle expression " &
               // "does not evaluate to a unique particle")
          return
       end select
       prt_data => model_get_particle_ptr &
                       (unstable%local%model, flavor_get_pdg (flv(d)))
       if (associated (prt_data)) then
          if (flavor_is_polarized (flv(d))) then
             call msg_error ("particle '" // char (flavor_get_name (flv(d))) &
                // "' cannot be marked at unstable and polarized at the same " &
                // "time - skipping")
             unstable%decay(d)%invalid = .true.
             cycle
          end if
          if (flavor_is_antiparticle (flv(d))) then
             call particle_data_set (prt_data, &
                   a_is_stable = .false., &
                   a_decays_isotropically = isotropic_decay, &
                   a_decays_diagonal = diagonal_decay)
          else
             call particle_data_set (prt_data, &
                   p_is_stable = .false., &
                   p_decays_isotropically = isotropic_decay, &
                   p_decays_diagonal = diagonal_decay)
          end if
          unstable%decay(d)%invalid = .false.
       else
          call msg_fatal ("Particle '" // char (unstable%decay(d)%prt) &
               // "' is not contained in model '" &
               // char (model_get_name (unstable%local%model)) // "'")
          unstable%decay(d)%invalid = .true.
       end if
    end do
    do d = 1, size (unstable%decay)
       if (unstable%decay(d)%invalid) cycle
       unstable%decay(d)%prt = flavor_get_name (flv(d))
       allocate (integral (unstable%decay(d)%n_proc))
       call integrate_missing_processes &
            (unstable%decay(d)%process_id, unstable%local, global%var_list, &
             no_beams=.true.)
       LOOP_PROC: do proc = 1, unstable%decay(d)%n_proc
          process_id = unstable%decay(d)%process_id(proc)
          process => process_store_get_process_ptr (process_id)
          if (associated (process)) then
             integral(proc) = var_list_get_rval (unstable%local%var_list, &
                 var_str ("integral(") // process_id // ")")
             if (integral(proc) < 0) then
                 call msg_fatal ("Integral of process '" &
                      // char (process_id) // "' is negative")
             end if
             call process_setup_event_generation (process, qn_mask_in = &
                  new_quantum_numbers_mask (.false., .false., isotropic_decay, &
                                            mask_hd = diagonal_decay))
          else
             call msg_fatal ("Decay channel '" // char (process_id) &
                  // "' is undefined")
          end if
       end do LOOP_PROC
       integral_sum = sum (integral)
       if (integral_sum /= 0) then
          unstable%decay(d)%br = integral / integral_sum
       else
          call msg_fatal ("Unstable particle: Computed total width vanishes")
          unstable%decay(d)%br = 0
       end if
       call decay_store_append_decay &
            (flv(d), unstable%local%model, &
             integral_sum, unstable%decay(d)%n_proc, &
             isotropic_decay, diagonal_decay, &
             decay_conf)
       ASSIGN_DECAYS: do proc = 1, unstable%decay(d)%n_proc
          process => process_store_get_process_ptr &
                          (unstable%decay(d)%process_id(proc))
          if (associated (process)) then
             call decay_configuration_set_channel &
                  (decay_conf, proc, process, unstable%decay(d)%br(proc))
          end if
       end do ASSIGN_DECAYS
       deallocate (integral)
       call msg_message ("Particle '" // char (unstable%decay(d)%prt) &
            // "' declared as unstable.")
       call decay_configuration_write (decay_conf)
       call decay_configuration_write (decay_conf, u)
    end do
    call decay_store_recheck_final_state (verbose = .true.)
    call rt_data_restore (global, unstable%local)
  end subroutine cmd_unstable_execute

  subroutine cmd_stable_final (stable)
    type(cmd_stable_t), intent(inout) :: stable
    integer :: d
    if (allocated (stable%decay)) then
       do d = 1, size (stable%decay)
          call decay_properties_final (stable%decay(d))
       end do
       deallocate (stable%decay)
    end if
    if (associated (stable%options)) then
       call command_list_final (stable%options)
       deallocate (stable%options)
    end if
  end subroutine cmd_stable_final

  subroutine cmd_stable_compile (stable, pn, global)
    type(cmd_stable_t), pointer :: stable
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_list, pn_prt, pn_opt
    integer :: d
    pn_list => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_list)
    allocate (stable)
    call rt_data_local_init (stable%local, global)
    if (associated (pn_opt)) then
       allocate (stable%options)
       call command_list_compile (stable%options, pn_opt, stable%local)
    end if
    allocate (stable%decay (parse_node_get_n_sub (pn_list)))
    d = 0
    pn_prt => parse_node_get_sub_ptr (pn_list)
    do while (associated (pn_prt))
       d = d + 1
       stable%decay(d)%pn_pdg => pn_prt
       stable%decay(d)%prt = "?"
       pn_prt => parse_node_get_next_ptr (pn_prt)
    end do
    call rt_data_local_reset (stable%local)
  end subroutine cmd_stable_compile

  subroutine cmd_stable_execute (stable, global)
    type(cmd_stable_t), intent(inout), target :: stable
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: aval
    type(flavor_t), dimension(:), allocatable :: flv_tmp
    type(flavor_t) :: flv
    type(particle_data_t), pointer :: prt_data
    integer :: d
    call rt_data_link (stable%local, global)
    if (associated (stable%options)) then
       call command_list_execute (stable%options, stable%local)
    end if
    do d = 1, size (stable%decay)
       aval = eval_pdg_array (stable%decay(d)%pn_pdg, stable%local%var_list)
       call flavor_init (flv_tmp, aval, stable%local%model)
       select case (size (flv_tmp))
       case (1);  flv = flv_tmp(1)
       case default
          call pdg_array_write (aval)
          call msg_fatal ("Stable particle expression " &
               // "does not evaluate to a unique particle")
          return
       end select
       stable%decay(d)%prt = flavor_get_name (flv)
       prt_data => &
            model_get_particle_ptr (stable%local%model, flavor_get_pdg (flv))
       if (associated (prt_data)) then
          if (flavor_is_antiparticle (flv)) then
             call particle_data_set (prt_data, a_is_stable=.true.)
             call particle_data_set (prt_data, a_decays_isotropically=.false.)
             call particle_data_set (prt_data, a_decays_diagonal=.false.)
          else
             call particle_data_set (prt_data, p_is_stable=.true.)
             call particle_data_set (prt_data, p_decays_isotropically=.false.)
             call particle_data_set (prt_data, p_decays_diagonal=.false.)
          end if
       else
          call msg_fatal ("Particle '" // char (stable%decay(d)%prt) &
               // "' is not contained in model '" &
               // char (model_get_name (stable%local%model)) // "'")
       end if
       call msg_message ("Particle '" // char (stable%decay(d)%prt) &
            // "' declared as stable.")
    end do
    call decay_store_recheck_final_state (verbose = .true.)
    call rt_data_restore (global, stable%local)
  end subroutine cmd_stable_execute

  subroutine cmd_un_polarized_compile (polarized, pn, global)
    type(cmd_un_polarized_t), pointer :: polarized
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_list, pn_opt, pn_prt
    integer :: n
    allocate (polarized)
    pn_list => parse_node_get_sub_ptr (pn, 2)
    call rt_data_local_init (polarized%local, global)
    if (.not. associated (pn_list)) then
       allocate (polarized%pn_pdg (0))
       allocate (polarized%name (0))
       return
    end if
    pn_opt => parse_node_get_next_ptr (pn_list)
    if (associated (pn_opt)) then
       allocate (polarized%options)
       call command_list_compile (polarized%options, pn_opt, polarized%local)
    end if
    n = parse_node_get_n_sub (pn_list)
    allocate (polarized%pn_pdg(n), polarized%name(n))
    polarized%name = "?"
    pn_prt => parse_node_get_sub_ptr (pn_list)
    n = 1
    do while (associated (pn_prt))
       polarized%pn_pdg(n)%ptr => pn_prt
       pn_prt  => parse_node_get_next_ptr (pn_prt)
       n = n + 1
    end do
    print *, "Processed " // int2char (n - 1) // " particle definitions"
    call rt_data_local_reset (polarized%local)
  end subroutine cmd_un_polarized_compile

  subroutine cmd_un_polarized_execute (polarized, type, global)
    type(cmd_un_polarized_t), intent(inout) :: polarized
    integer, intent(in) :: type
    type(rt_data_t), target, intent(inout) :: global
    type(pdg_array_t) :: aval
    type(flavor_t), dimension(:), allocatable :: flv
    type(particle_data_t), pointer :: prt
    integer :: i, u
    logical :: anti
    call rt_data_link (polarized%local, global)
    if (associated (polarized%options)) &
       call command_list_execute (polarized%options, polarized%local)
    if (size (polarized%pn_pdg) == 0) return
    do i = 1, size (polarized%pn_pdg)
       aval = eval_pdg_array (polarized%pn_pdg(i)%ptr, polarized%local%var_list)
       call flavor_init (flv, aval, polarized%local%model)
       if (size (flv) /= 1) then
          call pdg_array_write (aval)
          u = output_unit ()
          if (u >= 0) write (u, "('')")
          call msg_error &
             ("'polarized' needs unique particles, ignoring argument")
          deallocate (flv)
          cycle
       end if
       polarized%name(i) = flavor_get_name (flv(1))
       prt => model_get_particle_ptr (polarized%local%model, &
          flavor_get_pdg (flv(1)))
       if (.not. associated (prt)) then
          call msg_error ("Model '" &
             // char (model_get_name (polarized%local%model)) &
             // "' does not contain particle '" &
             // char (polarized%name(i)) // "'")
          deallocate (flv)
          cycle
       end if
       anti = flavor_is_antiparticle (flv(1))
       if (.not. particle_data_is_stable (prt, anti) .and. &
             type == CMD_POLARIZED) then
          call msg_error ("particle '" &
             // char (polarized%name(i)) // "' cannot be marked as unstable " &
             // "and polarized at the same time - skipping")
          deallocate (flv)
          cycle
       end if
       if (anti) then
          call particle_data_set (prt, a_polarized=(type == CMD_POLARIZED))
       else
          call particle_data_set (prt, p_polarized=(type == CMD_POLARIZED))
       end if
       if (type == CMD_POLARIZED) then
          call msg_message ("Polarization of particle '" &
             // char (polarized%name(i)) // "' in model '" &
             // char (model_get_name (polarized%local%model)) &
             // "' will be retained.")
       else
          call msg_message ("Polarization of particle '" &
             // char (polarized%name(i)) // "' in model '" &
             // char (model_get_name (polarized%local%model)) &
             // "' will be discarded.")
       end if
       deallocate (flv)
    end do
    call rt_data_restore (global, polarized%local)
  end subroutine cmd_un_polarized_execute
  subroutine cmd_un_polarized_final (polarized)
    type(cmd_un_polarized_t), intent(inout) :: polarized
    if (allocated (polarized%pn_pdg)) then
       deallocate (polarized%pn_pdg)
    end if
    if (allocated (polarized%name)) deallocate (polarized%name)
    if (associated (polarized%options)) then
       call command_list_final (polarized%options)
       deallocate (polarized%options)
    end if
  end subroutine cmd_un_polarized_final
  subroutine cmd_sample_format_final (sample)
    type(cmd_sample_format_t), intent(inout) :: sample
    if (allocated (sample%format)) then
       deallocate (sample%format)
    end if
  end subroutine cmd_sample_format_final

  subroutine cmd_sample_format_compile (sample, pn)
    type(cmd_sample_format_t), pointer :: sample
    type(parse_node_t), intent(in), target :: pn
    type(parse_node_t), pointer :: pn_arg
    type(parse_node_t), pointer :: pn_format
    integer :: i, n_format
    allocate (sample)
    pn_arg => parse_node_get_sub_ptr (pn, 3)
    if (associated (pn_arg)) then
       n_format = parse_node_get_n_sub (pn_arg)
       allocate (sample%format (n_format), sample%fmt (n_format))
       pn_format => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_format))
          i = i + 1
          sample%format(i) = parse_node_get_string (pn_format)
          pn_format => parse_node_get_next_ptr (pn_format)
       end do
       sample%fmt = event_format_code (sample%format)
    else
       allocate (sample%format (0))
    end if
  end subroutine cmd_sample_format_compile

  subroutine cmd_sample_format_execute (sample, global)
    type(cmd_sample_format_t), intent(inout) :: sample
    type(rt_data_t), intent(inout) :: global
    integer, dimension(:), allocatable :: fmt_tmp
    integer :: i, j, k
    if (allocated (global%event_fmt))  deallocate (global%event_fmt)
    allocate (fmt_tmp(size (sample%format)))
    j = 0
    TRANSFER: do i = 1, size (sample%format)
       if (sample%fmt(i) == FMT_NONE) then
          call msg_error ("Undefined event-file format '" &
               // char (sample%format(i)) // "' (skipped)")
          cycle TRANSFER
       end if
       if (j > 0) then
          do k = 1, j
             if (fmt_tmp(k) == sample%fmt(i)) then
                call msg_warning ("Ignoring multiple requests for " &
                     // "event-file format '" // char (sample%format(i)) &
                     // "'" )
                cycle TRANSFER
             end if
          end do
       end if
       j = j + 1
       fmt_tmp(j) = sample%fmt(i)
    end do TRANSFER
    if (j > 0) then
       allocate (global%event_fmt(j))
       global%event_fmt = fmt_tmp(1:j)
    end if
    deallocate (fmt_tmp)
  end subroutine cmd_sample_format_execute

  subroutine cmd_simulate_final (simulate)
    type(cmd_simulate_t), intent(inout) :: simulate
    if (associated (simulate%options)) then
       call command_list_final (simulate%options)
       deallocate (simulate%options)
    end if
  end subroutine cmd_simulate_final

  subroutine cmd_simulate_compile (simulate, pn, global)
    type(cmd_simulate_t), pointer :: simulate
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_proclist, pn_proc, pn_opt
    integer :: i
    pn_proclist => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_proclist)
    allocate (simulate)
    call rt_data_local_init (simulate%local, global)
    if (associated (pn_opt)) then
       allocate (simulate%options)
       call command_list_compile (simulate%options, pn_opt, simulate%local)
    end if
    simulate%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (simulate%process_id (simulate%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, simulate%n_proc
       simulate%process_id(i) = parse_node_get_string (pn_proc)
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
    call rt_data_local_reset (simulate%local)
  end subroutine cmd_simulate_compile

  subroutine cmd_simulate_execute (simulate, global)
    type(cmd_simulate_t), intent(inout), target :: simulate
    type(rt_data_t), intent(inout), target :: global
    logical :: ok, mlm_matching
!    integer :: i_evt
    type(simulation_t), target :: sim
    call rt_data_link (simulate%local, global)
    if (associated (simulate%options)) then
       call command_list_execute (simulate%options, simulate%local)
    end if
    call simulation_init (sim, simulate%process_id, simulate%local, &
         global%var_list, ok, verbose=.true.)
    if (ok) then
       call simulation_setup_selection &
            (sim, simulate%local%pn_selection_lexpr, verbose=.true.)
       call simulation_setup_reweight &
            (sim, simulate%local%pn_reweight_expr, verbose=.true.)
       call simulation_setup_analysis &
            (sim, simulate%local%pn_analysis_lexpr, verbose=.true.)
       call openmp_set_num_threads_verbose &
            (var_list_get_ival (global%var_list, "openmp_num_threads"))
!       do i_evt = 1, simulation_get_n_events (sim)
!          call simulation_event (sim, simulate%local%rng, ok, verbose=.true.)
!          if (.not. ok)  exit
!       end do
       do while( simulation_get_i_evt(sim) .lt.simulation_get_n_events(sim))
          call simulation_event (sim, simulate%local%rng, ok, verbose=.true.)
          if (.not. ok)  exit
       end do
       call simulation_final (sim, verbose=.true.)
       mlm_matching = simulation_check_matching(sim)
       if (mlm_matching) then
            call cmd_matching_execute (simulate%local%os_data)
       end if
    end if
    call rt_data_restore (global, simulate%local)
  end subroutine cmd_simulate_execute

  subroutine cmd_matching_execute (os_data)
    type(string_t) :: cmd_string
    type(os_data_t), intent(in) :: os_data
    call msg_message ("Starting MLM matching PYTHIA interface...")
    cmd_string = os_data%prefix // "/bin/interface"
    call os_system_call (cmd_string)
    call msg_message ("PYTHIA interface finished.")
  end subroutine cmd_matching_execute

  subroutine cmd_rescan_final (rescan)
    type(cmd_rescan_t), intent(inout) :: rescan
    if (associated (rescan%options)) then
       call command_list_final (rescan%options)
       deallocate (rescan%options)
    end if
  end subroutine cmd_rescan_final

  subroutine cmd_rescan_compile (rescan, pn, global)
    type(cmd_rescan_t), pointer :: rescan
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_filename, pn_proclist, pn_proc, pn_opt
    integer :: i
    pn_filename => parse_node_get_sub_ptr (pn, 2)
    pn_proclist => parse_node_get_next_ptr (pn_filename)
    pn_opt => parse_node_get_next_ptr (pn_proclist)
    allocate (rescan)
    call rt_data_local_init (rescan%local, global)
    if (associated (pn_opt)) then
       allocate (rescan%options)
       call command_list_compile (rescan%options, pn_opt, rescan%local)
    end if
    rescan%pn_filename => pn_filename
    rescan%n_proc = parse_node_get_n_sub (pn_proclist)
    allocate (rescan%process_id (rescan%n_proc))
    pn_proc => parse_node_get_sub_ptr (pn_proclist)
    do i = 1, rescan%n_proc
       rescan%process_id(i) = parse_node_get_string (pn_proc)
       pn_proc => parse_node_get_next_ptr (pn_proc)
    end do
    call rt_data_local_reset (rescan%local)
  end subroutine cmd_rescan_compile

  subroutine cmd_rescan_execute (rescan, global)
    type(cmd_rescan_t), intent(inout), target :: rescan
    type(rt_data_t), intent(inout), target :: global
    logical :: ok
    integer :: i_evt
    type(simulation_t), target :: sim
    type(string_t) :: filename
    call rt_data_link (rescan%local, global)
    if (associated (rescan%options)) then
       call command_list_execute (rescan%options, rescan%local)
    end if
    filename = eval_string (rescan%pn_filename, rescan%local%var_list)
    call simulation_init (sim, &
         rescan%process_id, rescan%local, global%var_list, ok, &
         filename=filename, verbose=.true.)
    if (ok) then
       call simulation_setup_selection &
            (sim, rescan%local%pn_selection_lexpr, verbose=.true.)
       call simulation_setup_reweight &
            (sim, rescan%local%pn_reweight_expr, verbose=.true.)
       call simulation_setup_analysis &
            (sim, rescan%local%pn_analysis_lexpr, verbose=.true.)
       do i_evt = 1, simulation_get_n_events (sim)
          call simulation_event (sim, rescan%local%rng, ok, verbose=.true.)
          if (.not. ok)  exit
       end do
       call simulation_final (sim, verbose=.true.)
    end if
    call rt_data_restore (global, rescan%local)
  end subroutine cmd_rescan_execute

  subroutine cmd_seed_final (seed)
    type(cmd_seed_t), intent(inout) :: seed
  end subroutine cmd_seed_final

  subroutine cmd_seed_compile (seed, pn, global)
    type(cmd_seed_t), pointer :: seed
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    allocate (seed)
    seed%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_seed_compile

  subroutine cmd_seed_execute (seed, global)
    type(cmd_seed_t), intent(inout) :: seed
    type(rt_data_t), intent(inout) :: global
    integer :: seed_val
    seed_val = eval_int (seed%pn_expr, global%var_list)
    call set_rng_seed (global%rng, global%var_list, seed_val, verbose=.true.)
  end subroutine cmd_seed_execute

  subroutine set_rng_seed (rng, var_list, seed_val, verbose)
    type(tao_random_state), intent(inout) :: rng
    type(var_list_t), intent(inout), target :: var_list
    integer, intent(in) :: seed_val
    logical, intent(in) :: verbose
    character(30) :: buffer
    if (verbose) then
       write (buffer, "(I0)")  seed_val
       call msg_message ("Setting seed for random-number generator to " &
            // trim (buffer))
    end if
    call tao_random_seed (rng, seed_val)
    call var_list_set_int (var_list, var_str ("seed_value"), &
         seed_val, is_known=.true.)
  end subroutine set_rng_seed
  
  subroutine cmd_iterations_final (iterations)
    type(cmd_iterations_t), intent(inout) :: iterations
    integer :: i
    if (allocated (iterations%pn_expr_n_it)) then
       deallocate (iterations%pn_expr_n_it)
    end if
    if (allocated (iterations%pn_expr_n_calls)) then
       deallocate (iterations%pn_expr_n_calls)
    end if
  end subroutine cmd_iterations_final

  subroutine cmd_iterations_compile (iterations, pn, global)
    type(cmd_iterations_t), pointer :: iterations
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_n_it, pn_n_calls, pn_adapt
    type(parse_node_t), pointer :: pn_it_spec, pn_calls_spec, pn_adapt_spec
    integer :: i
    allocate (iterations)
    pn_arg => parse_node_get_sub_ptr (pn, 3)
    if (associated (pn_arg)) then
       iterations%n_pass = parse_node_get_n_sub (pn_arg)
       allocate (iterations%pn_expr_n_it (iterations%n_pass))
       allocate (iterations%pn_expr_n_calls (iterations%n_pass))
       allocate (iterations%pn_sexpr_adapt (iterations%n_pass))
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
          iterations%pn_expr_n_it(i)%ptr => pn_n_it
          iterations%pn_expr_n_calls(i)%ptr => pn_n_calls
          iterations%pn_sexpr_adapt(i)%ptr => pn_adapt
          i = i + 1
          pn_it_spec => parse_node_get_next_ptr (pn_it_spec)
       end do
    else
       allocate (iterations%pn_expr_n_it (0))
       allocate (iterations%pn_expr_n_calls (0))
    end if
  end subroutine cmd_iterations_compile

  subroutine cmd_iterations_execute (iterations, global)
    type(cmd_iterations_t), intent(inout) :: iterations
    type(rt_data_t), intent(inout) :: global
    integer, dimension(iterations%n_pass) :: n_it, n_calls
    logical, dimension(iterations%n_pass) :: custom_adapt
    type(string_t), dimension(iterations%n_pass) :: adapt_code
    integer :: i
    do i = 1, iterations%n_pass
       n_it(i) = eval_int (iterations%pn_expr_n_it(i)%ptr, global%var_list)
       n_calls(i) = &
            eval_int (iterations%pn_expr_n_calls(i)%ptr, global%var_list)
       if (associated (iterations%pn_sexpr_adapt(i)%ptr)) then
          adapt_code(i) = &
               eval_string (iterations%pn_sexpr_adapt(i)%ptr, &
                            global%var_list, is_known = custom_adapt(i))
       else
          custom_adapt(i) = .false.
       end if        
    end do
    call iterations_list_init &
        (global%it_list, n_it, n_calls, custom_adapt, adapt_code)
  end subroutine cmd_iterations_execute

  recursive subroutine cmd_scan_final (loop)
    type(cmd_scan_t), intent(inout) :: loop
    integer :: i
    if (associated (loop%cmd_var)) then
       do i = 1, size (loop%cmd_var)
          call command_list_final (loop%cmd_var(i))
       end do
       deallocate (loop%cmd_var)
    end if
    if (allocated (loop%has_range))  deallocate (loop%has_range)
    if (allocated (loop%pn_beg_expr)) then
       deallocate (loop%pn_beg_expr)
    end if
    if (allocated (loop%pn_end_expr)) then
       deallocate (loop%pn_end_expr)
    end if
    if (allocated (loop%step_type))  deallocate (loop%step_type)
    if (allocated (loop%pn_step_expr)) then
       deallocate (loop%pn_step_expr)
    end if
    if (associated (loop%body)) then
       call command_list_final (loop%body)
       deallocate (loop%body)
    end if
  end subroutine cmd_scan_final

  recursive subroutine cmd_scan_compile (loop, pn, global)
    type(cmd_scan_t), pointer :: loop
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_spec, pn_cmd, pn_list, pn_body
    integer :: i
    type(parse_tree_t) :: parse_tree
    type(parse_node_t), pointer :: pn_root, pn_decl, pn_init_arg, pn_arg
    type(parse_node_t), pointer :: pn_expr, pn_range, pn_range_expr
    type(parse_node_t), pointer :: pn_step, pn_step_op, pn_step_expr
    type(parse_node_t), pointer :: pn_var, pn_name
    type(string_t) :: key, str_init
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    logical :: declaration, new
    pn_spec => parse_node_get_sub_ptr (pn)
    pn_cmd => parse_node_get_sub_ptr (pn_spec, 2)
    pn_var => pn_cmd
    allocate (loop)
    call rt_data_local_init (loop%local, global)
    if (associated (pn_cmd)) then
       key = parse_node_get_rule_key (pn_cmd)
    else
       loop%var_type = V_NONE
       loop%n_arg = 0
       return
    end if
    declaration = .false.
    select case (char (key))
    case ("cmd_model_list")
       str_init = 'model = ""'
    case ("cmd_library_list")
       str_init = 'library = ""'
    case ("cmd_seed_list")
       loop%var_type = V_INT
       loop%var_name = "seed"
       str_init = 'seed = 0'
       loop%allow_steps = .true.
    case ("cmd_cuts_list")
       str_init = 'cuts = true'
    case ("cmd_scale_list")
       str_init = 'scale = 0'       
    case ("cmd_fac_scale_list")
       str_init = 'factorization_scale = 0'
    case ("cmd_ren_scale_list")
       str_init = 'renormalization_scale = 0'       
    case ("cmd_weight_list")
       str_init = 'weight = 0'
    case ("cmd_selection_list")
       str_init = 'selection = true'
    case ("cmd_reweight_list")
       str_init = 'reweight = 0'
    case ("cmd_analysis_list")
       str_init = 'analysis = true'
    case default
       new = .false.
       select case (char (key))
       case ("log_decl_list")
          loop%var_type = V_LOG
          pn_var => parse_node_get_sub_ptr (pn_cmd, 2)
          pn_name => parse_node_get_sub_ptr (pn_var, 2)
          loop%var_name = '?' // parse_node_get_string (pn_name)
          str_init = 'logical ' // loop%var_name // ' = true'
          declaration = .true.
          new = .true.
       case ("log_list")
          loop%var_type = V_LOG
          pn_name => parse_node_get_sub_ptr (pn_cmd, 2)
          loop%var_name = '?' // parse_node_get_string (pn_name)
          str_init = loop%var_name // ' = true'
       case ("int_list")
          loop%var_type = V_INT
          pn_name => parse_node_get_sub_ptr (pn_cmd, 2)
          loop%var_name = parse_node_get_string (pn_name)
          str_init = 'int ' // loop%var_name // ' = 0'
          loop%allow_steps = .true.
          new = .true.
       case ("real_list")
          loop%var_type = V_REAL
          pn_name => parse_node_get_sub_ptr (pn_cmd, 2)
          loop%var_name = parse_node_get_string (pn_name)
          str_init = 'real ' // loop%var_name // ' = 0'
          loop%allow_steps = .true.
          new = .true.
       case ("num_list")
          loop%var_type = V_REAL
          pn_name => parse_node_get_sub_ptr (pn_cmd)
          loop%var_name = parse_node_get_string (pn_name)
          str_init = loop%var_name // ' = 0'
          loop%allow_steps = .true.
       case ("string_decl_list")
          loop%var_type = V_STR
          pn_var => parse_node_get_sub_ptr (pn_cmd, 2)
          pn_name => parse_node_get_sub_ptr (pn_var, 2)
          loop%var_name = '$' // parse_node_get_string (pn_name)
          str_init = 'string ' // loop%var_name // ' = ""'
          declaration = .true.
          new = .true.
       case ("string_list")
          loop%var_type = V_STR
          pn_name => parse_node_get_sub_ptr (pn_cmd, 2)
          loop%var_name = '$' // parse_node_get_string (pn_name)
          str_init = loop%var_name // ' = ""'
       case ("alias_list")
          loop%var_type = V_PDG
          pn_name => parse_node_get_sub_ptr (pn_cmd, 2)
          loop%var_name = parse_node_get_string (pn_name)
          str_init = 'alias ' // loop%var_name // ' = PDG(0)'
          new = .true.
       case default
          call parse_node_mismatch &
               ("scan: model|library|seed|cuts|weight|" // &
                "scale|factorization_scale|renormalization_scale|analysis|" &
                // "variable",  pn_cmd)
       end select
       call var_list_check_user_var &
            (global%var_list, loop%var_name, loop%var_type, new)
       if (loop%var_type == V_NONE) then
          call msg_fatal ("Invalid scan variable declaration")
          loop%n_arg = 0
          return
       end if
    end select
    if (associated (pn_var)) then
       pn_list => parse_node_get_last_sub_ptr (pn_var)
       if (associated (pn_list)) then
          loop%n_arg = parse_node_get_n_sub (pn_list)
          allocate (loop%cmd_var (loop%n_arg))
          call lexer_init_cmd_list (lexer)
          allocate (loop%has_range (loop%n_arg)); loop%has_range = .false.
          if (loop%allow_steps) then
             allocate (loop%pn_beg_expr (loop%n_arg))
             allocate (loop%pn_end_expr (loop%n_arg))
             allocate (loop%step_type (loop%n_arg)); loop%step_type = STEP_NONE
             allocate (loop%pn_step_expr (loop%n_arg))
          end if
          call stream_init (stream, str_init)
          call lexer_assign_stream (lexer, stream)
          call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
          pn_root => parse_tree_get_root_ptr (parse_tree)
          if (declaration) then
             pn_decl => parse_node_get_sub_ptr (pn_root)
             pn_cmd => parse_node_get_sub_ptr (pn_decl, 2)
          else
             pn_cmd => parse_node_get_sub_ptr (pn_root)
          end if
          pn_init_arg => parse_node_get_last_sub_ptr (pn_cmd)
          i = 0
          pn_arg => parse_node_get_sub_ptr (pn_list)
          do while (associated (pn_arg))
             i = i + 1
             select case (char (parse_node_get_rule_key (pn_arg)))
             case ("num_steps")
                pn_expr => parse_node_get_sub_ptr (pn_arg)
                pn_range => parse_node_get_next_ptr (pn_expr)
                if (associated (pn_range)) then
                   loop%has_range(i) = .true.
                   pn_range_expr => parse_node_get_sub_ptr (pn_range, 2)
                   loop%pn_beg_expr(i)%ptr => pn_expr
                   loop%pn_end_expr(i)%ptr => pn_range_expr
                   pn_step => parse_node_get_next_ptr (pn_range_expr)
                   if (associated (pn_step)) then
                      pn_step_op => parse_node_get_sub_ptr (pn_step)
                      select case (char (parse_node_get_key (pn_step_op)))
                      case ("/+");  loop%step_type(i) = STEP_ADD
                      case ("/-");  loop%step_type(i) = STEP_SUB
                      case ("/*");  loop%step_type(i) = STEP_MUL
                      case ("//");  loop%step_type(i) = STEP_DIV
                      case ("+/+");  loop%step_type(i) = STEP_COMP_ADD
                      case ("*/*");  loop%step_type(i) = STEP_COMP_MUL
                      end select
                      pn_step_expr => parse_node_get_next_ptr (pn_step_op)
                      loop%pn_step_expr(i)%ptr => pn_step_expr
                   end if
                end if
             case default
                pn_expr => pn_arg
             end select
             call parse_node_replace_last_sub (pn_cmd, pn_expr)
             call command_list_compile (loop%cmd_var(i), pn_root, loop%local)
             pn_arg => parse_node_get_next_ptr (pn_arg)
          end do
          call parse_node_replace_last_sub (pn_cmd, pn_init_arg)
          call parse_tree_final (parse_tree)
          call stream_final (stream)
          call lexer_final (lexer)
       end if
       pn_body => parse_node_get_next_ptr (pn_spec)
       if (associated (pn_body)) then
          allocate (loop%body)
          call command_list_compile (loop%body, pn_body, loop%local)
       end if
    else
       loop%n_arg = 0
    end if
    call rt_data_local_reset (loop%local)
  end subroutine cmd_scan_compile

  recursive subroutine cmd_scan_execute (loop, global)
    type(cmd_scan_t), intent(inout), target :: loop
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: model_name
    logical :: is_seed
    integer :: i, j
    integer :: i1, i2, istep, ival, n_steps
    real(default) :: r1, r2, rstep, rval, rlog, r1log, r2log
    logical :: is_known
    type(var_entry_t), pointer :: var
    type(var_list_t), pointer :: model_vars
    call rt_data_link (loop%local, global)
    if (associated (loop%local%model)) then
       model_name = model_get_name (loop%local%model)
       model_vars => model_get_var_list_ptr (loop%local%model)
    else
       model_vars => null ()
    end if
    LOOP_LIST: do i = 1, loop%n_arg
       call command_list_execute (loop%cmd_var(i), loop%local)
       if (.not. loop%has_range(i)) then
          if (associated (loop%body)) then
             call command_list_execute (loop%body, loop%local)
             if (loop%local%quit) then
                global%quit_code = loop%local%quit_code
                global%quit = .true.
                return
             end if
          end if
       else
          call eval_numeric (loop%pn_beg_expr(i)%ptr, loop%local%var_list, &
               is_known=is_known, ival=i1, rval=r1)
          if (.not. is_known) then
             call msg_error ("Scan: undefined lower bound, skipping")
             cycle LOOP_LIST
          end if
          call eval_numeric (loop%pn_end_expr(i)%ptr, loop%local%var_list, &
               is_known=is_known, ival=i2, rval=r2)
          if (.not. is_known) then
             call msg_error ("Scan: undefined upper bound, skipping")
             cycle LOOP_LIST
          end if
          select case (loop%step_type(i))
          case (STEP_NONE)
             rstep = 1
             istep = 1
          case default
             call eval_numeric (loop%pn_step_expr(i)%ptr, loop%local%var_list, &
                  is_known=is_known, ival=istep, rval=rstep)
             if (.not. is_known) then
                call msg_error ("Scan: undefined step size, skipping")
                cycle LOOP_LIST
             end if
          end select
          if (bounds_check_fails (loop%step_type(i), loop%var_type)) &
               cycle LOOP_LIST
          if (loop%var_name == "seed") then
             is_seed = .true.
          else
             is_seed = .false.
             var => var_list_get_var_ptr (loop%local%var_list, loop%var_name)
             if (var_entry_get_type (var) /= loop%var_type) then
                call msg_fatal ("Type mismatch for loop variable '" &
                     // char (loop%var_name) // "'; skipping")
                exit LOOP_LIST
             end if
          end if
          select case (loop%var_type)
          case (V_REAL)
             n_steps = -1
             select case (loop%step_type(i))
             case (STEP_NONE, STEP_ADD)
                if (rstep /= 0)  n_steps = nint ((r2 - r1) / rstep)
             case (STEP_SUB)
                if (rstep /= 0)  n_steps = nint (- (r2 - r1) / rstep)
             case (STEP_COMP_ADD)
                if (istep /= 0)  n_steps = istep
             case (STEP_MUL, STEP_DIV, STEP_COMP_MUL)
                if (r1 * r2 > 0) then
                   if (rstep > 0 .and. rstep /= 1) then
                      r1log = log (abs (r1))
                      r2log = log (abs (r2))
                      select case (loop%step_type(i))
                      case (STEP_MUL)
                         n_steps = nint (log (r2 / r1) / log (rstep))
                      case (STEP_DIV)
                         n_steps = nint (- log (r2 / r1) / log (rstep))
                      case (STEP_COMP_MUL)
                         n_steps = istep
                      end select
                   end if
                end if
             end select
             if (n_steps == 0)  n_steps = -1
             select case (loop%step_type(i))
             case (STEP_NONE, STEP_ADD, STEP_COMP_ADD, STEP_SUB)
                do j = 0, n_steps
                   rval = (j * r2 + (n_steps-j) * r1) / n_steps
                   call set_real (rval, j/=0)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                end do
             case (STEP_MUL, STEP_COMP_MUL, STEP_DIV)
                do j = 0, n_steps
                   rlog = (j * r2log + (n_steps-j) * r1log) / n_steps
                   rval = sign (exp (rlog), r1)
                   call set_real (rval, j/=0)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                end do
             end select
          case (V_INT)
             select case (loop%step_type(i))
             case (STEP_SUB);  istep = - istep
             end select
             select case (loop%step_type(i))
             case (STEP_NONE, STEP_ADD, STEP_SUB)
                do j = i1, i2, istep
                   call set_int (j, j/=i1, is_seed)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                end do
             case (STEP_MUL, STEP_DIV)
                call msg_error ("Skipping scan: " &
                     // "Multiplicative steps not allowed " &
                     // "for integer scan variable")
             case (STEP_COMP_ADD, STEP_COMP_MUL)
                call msg_error ("Skipping scan: " &
                     // "Range division not allowed for integer scan variable")
             end select
          end select
       end if
    end do LOOP_LIST
    call rt_data_restore (global, loop%local)
  contains
    subroutine set_real (rval, verbose)
      real(default), intent(in) :: rval
      logical, intent(in) :: verbose
      call var_entry_set_real (var, rval, is_known=.true., verbose=verbose, &
           model_name=model_name)
      if (var_entry_is_copy (var)) then
         call model_parameters_update (loop%local%model)
         call var_list_synchronize (loop%local%var_list, model_vars)
      end if
    end subroutine set_real
    subroutine set_int (ival, verbose, is_seed)
      integer, intent(in) :: ival
      logical, intent(in) :: verbose, is_seed
      if (is_seed) then
         call set_rng_seed (loop%local%rng, loop%local%var_list, ival, &
              verbose=verbose)
      else
         call var_entry_set_int (var, ival, is_known=.true., verbose=verbose, &
              model_name=model_name)
         if (var_entry_is_copy (var)) then
            call model_parameters_update (loop%local%model)
            call var_list_synchronize (loop%local%var_list, model_vars)
         end if
      end if
    end subroutine set_int
    function bounds_check_fails (step_type, var_type) result (fails)
      logical :: fails
      integer, intent(in) :: step_type, var_type
      fails = .false.
      select case (var_type)
      case (V_REAL)
         if (rstep == 0) then
            call msg_error ("Scan: step size equals zero, skipping")
            fails = .true.;  return
         end if
         select case (step_type)
         case (STEP_MUL, STEP_DIV)
            if (rstep < 0) then
               call msg_error ("Scan: multiplicative step < 0, skipping")
               fails = .true.;  return
            else if (rstep == 1) then
               call msg_error ("Scan: multiplicative step = 1, skipping")
               fails = .true.;  return
            else if (r1 == 0 .or. r2 == 0) then
               call msg_error ("Scan: " &
                    // "boundary for multiplicative step is zero, skipping")
               fails = .true.;  return
            end if
         end select
      case (V_INT)
         if (istep == 0) then
            call msg_error ("Scan: step size equals zero, skipping")
            fails = .true.;  return
         end if
         select case (step_type)
         case (STEP_MUL, STEP_DIV)
            if (istep < 0) then
               call msg_error ("Scan: multiplicative step < 0, skipping")
               fails = .true.;  return
            else if (istep == 1) then
               call msg_error ("Scan: multiplicative step = 1, skipping")
               fails = .true.;  return
            else if (i1 == 0 .or. i2 == 0) then
               call msg_error ("Scan: " &
                    // "boundary for multiplicative step is zero, skipping")
               fails = .true.;  return
            end if
         end select
      end select
    end function bounds_check_fails
  end subroutine cmd_scan_execute

  recursive subroutine cmd_if_final (cond)
    type(cmd_if_t), intent(inout) :: cond
    integer :: i
    if (associated (cond%if_body)) then
       call command_list_final (cond%if_body)
       deallocate (cond%if_body)
    end if
    if (associated (cond%elsif_cond)) then
       do i = 1, size (cond%elsif_cond)
          call cmd_if_final (cond%elsif_cond(i))
       end do
       deallocate (cond%elsif_cond)
    end if
    if (associated (cond%else_body)) then
       call command_list_final (cond%else_body)
       deallocate (cond%else_body)
    end if
  end subroutine cmd_if_final

  recursive subroutine cmd_if_compile (cond, pn, global)
    type(cmd_if_t), pointer :: cond
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_lexpr, pn_body
    type(parse_node_t), pointer :: pn_elsif_clauses, pn_cmd_elsif
    type(parse_node_t), pointer :: pn_else_clause, pn_cmd_else
    integer :: i, n_elsif
    allocate (cond)
    pn_lexpr => parse_node_get_sub_ptr (pn, 2)
    cond%pn_if_lexpr => pn_lexpr
    pn_body => parse_node_get_next_ptr (pn_lexpr, 2)
    select case (char (parse_node_get_rule_key (pn_body)))
    case ("command_list")
       allocate (cond%if_body)
       call command_list_compile (cond%if_body, pn_body, global)
       pn_elsif_clauses => parse_node_get_next_ptr (pn_body)
    case default
       pn_elsif_clauses => pn_body
    end select
    select case (char (parse_node_get_rule_key (pn_elsif_clauses)))
    case ("elsif_clauses")
       n_elsif = parse_node_get_n_sub (pn_elsif_clauses)
       allocate (cond%elsif_cond (n_elsif))
       pn_cmd_elsif => parse_node_get_sub_ptr (pn_elsif_clauses)
       do i = 1, n_elsif
          pn_lexpr => parse_node_get_sub_ptr (pn_cmd_elsif, 2)
          cond%elsif_cond(i)%pn_if_lexpr => pn_lexpr
          pn_body => parse_node_get_next_ptr (pn_lexpr, 2)
          if (associated (pn_body)) then
             allocate (cond%elsif_cond(i)%if_body)
             call command_list_compile &
                  (cond%elsif_cond(i)%if_body, pn_body, global)
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
          allocate (cond%else_body)
          call command_list_compile (cond%else_body, pn_body, global)
       end if
    end select
  end subroutine cmd_if_compile

  recursive subroutine cmd_if_execute (cond, global)
    type(cmd_if_t), intent(inout), target :: cond
    type(rt_data_t), intent(inout), target :: global
    logical :: lval, is_known
    integer :: i
    lval = eval_log (cond%pn_if_lexpr, global%var_list, is_known=is_known)
    if (is_known) then
       if (lval) then
          if (associated (cond%if_body)) then
             call command_list_execute (cond%if_body, global)
          end if
          return
       end if
    else
       call error_undecided ()
       return
    end if
    if (associated (cond%elsif_cond)) then
       SCAN_ELSIF: do i = 1, size (cond%elsif_cond)
          lval = eval_log (cond%elsif_cond(i)%pn_if_lexpr, global%var_list, &
                is_known=is_known)
          if (is_known) then
             if (lval) then
                if (associated (cond%elsif_cond(i)%if_body)) then
                   call command_list_execute &
                        (cond%elsif_cond(i)%if_body, global)
                end if
                return
             end if
          else
             call error_undecided ()
             return
          end if
       end do SCAN_ELSIF
    end if
    if (associated (cond%else_body)) then
       call command_list_execute (cond%else_body, global)
    end if
  contains
    subroutine error_undecided ()
      call msg_error ("Undefined result of conditional expression: " &
           // "neither branch will be executed")
    end subroutine error_undecided
  end subroutine cmd_if_execute

  subroutine cmd_include_final (include)
    type(cmd_include_t), intent(inout) :: include
    call parse_tree_final (include%parse_tree)
    if (associated (include%command_list)) then
       call command_list_final (include%command_list)
       deallocate (include%command_list)
    end if
  end subroutine cmd_include_final

  subroutine cmd_include_compile (include, pn, global)
    type(cmd_include_t), pointer :: include
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_file
    type(string_t) :: file
    logical :: exist
    integer :: u
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    pn_file => parse_node_get_sub_ptr (pn_arg)
    allocate (include)
    file = parse_node_get_string (pn_file)
    inquire (file=char(file), exist=exist)
    if (exist) then
       include%file = file
    else
       include%file = global%os_data%whizard_cutspath // "/" // file
       inquire (file=char(include%file), exist=exist)
       if (.not. exist) then
          call msg_error ("Include file '" // char (file) // "' not found")
          return
       end if
    end if
    u = free_unit ()
    call lexer_init_cmd_list (lexer, global%lexer)
    call stream_init (stream, char (include%file))
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (include%parse_tree, syntax_cmd_list, lexer)
    call stream_final (stream)
    call lexer_final (lexer)
    close (u)
    allocate (include%command_list)
    call command_list_compile &
         (include%command_list, parse_tree_get_root_ptr (include%parse_tree), &
          global)
  end subroutine cmd_include_compile

  subroutine cmd_include_execute (include, global)
    type(cmd_include_t), intent(inout), target :: include
    type(rt_data_t), intent(inout), target :: global
    if (associated (include%command_list)) then
       call msg_message &
            ("Including script file '" // char (include%file) // "'")
       call command_list_execute (include%command_list, global)
    end if
  end subroutine cmd_include_execute

  subroutine cmd_quit_final (quit)
    type(cmd_quit_t), intent(inout) :: quit
  end subroutine cmd_quit_final

  subroutine cmd_quit_compile (quit, pn, global)
    type(cmd_quit_t), pointer :: quit
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg
    allocate (quit)
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_arg)) then
       quit%pn_code_expr => parse_node_get_sub_ptr (pn_arg)
       quit%has_code = .true.
    end if
  end subroutine cmd_quit_compile

  subroutine cmd_quit_execute (quit, global)
    type(cmd_quit_t), intent(inout), target :: quit
    type(rt_data_t), intent(inout), target :: global
    logical :: is_known
    if (quit%has_code) then
       global%quit_code = eval_int (quit%pn_code_expr, global%var_list, &
            is_known=is_known)
       if (.not. is_known) then
          call msg_error ("Undefined return code of quit/exit command")
       end if
    end if
    global%quit = .true.
  end subroutine cmd_quit_execute

  subroutine command_list_append (cmd_list, command)
    type(command_list_t), intent(inout) :: cmd_list
    type(command_t), intent(in), target :: command
    if (associated (cmd_list%last)) then
       cmd_list%last%next => command
    else
       cmd_list%first => command
    end if
    cmd_list%last => command
  end subroutine command_list_append

  recursive subroutine command_list_final (cmd_list)
    type(command_list_t), intent(inout) :: cmd_list
    type(command_t), pointer :: command
    do while (associated (cmd_list%first))
       command => cmd_list%first
       cmd_list%first => cmd_list%first%next
       call command_final (command)
       deallocate (command)
    end do
    cmd_list%last => null ()
  end subroutine command_list_final

  recursive subroutine command_list_compile (cmd_list, pn, global)
    type(command_list_t), intent(inout), target :: cmd_list
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_cmd
    type(command_t), pointer :: command
    integer :: i
    pn_cmd => parse_node_get_sub_ptr (pn)
    do i = 1, parse_node_get_n_sub (pn)
       call command_compile (command, pn_cmd, global)
       call command_list_append (cmd_list, command)
       call terminate_now_if_signal ()
       pn_cmd => parse_node_get_next_ptr (pn_cmd)
    end do
  end subroutine command_list_compile

  recursive subroutine command_list_execute (cmd_list, global)
    type(command_list_t), intent(in) :: cmd_list
    type(rt_data_t), intent(inout), target :: global
    type(command_t), pointer :: command
    command => cmd_list%first
    COMMAND_COND: do while (associated (command))
       call command_execute (command, global)
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
         // "cmd_seed | " &
         // "cmd_var | cmd_slha | " &
         // "cmd_show | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_scale | cmd_fac_scale | cmd_ren_scale | " &
         // "cmd_weight | cmd_selection | cmd_reweight | " &
         // "cmd_beams | cmd_integrate | cmd_me_test | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_graph | " &
         // "cmd_clear | cmd_record | " &
         // "cmd_analysis | " &
         // "cmd_unstable | cmd_stable | cmd_simulate | cmd_rescan| " &
         // "cmd_process | cmd_compile | cmd_load | cmd_exec | " &
         // "cmd_scan | cmd_if | cmd_include | cmd_quit | " &
         // "cmd_polarized | cmd_unpolarized | " &
         // "cmd_beam_polarization | " &
         // "cmd_open_out | cmd_close_out | cmd_printd | cmd_printf | " &
         // "cmd_write_analysis | cmd_compile_analysis | " &
         // "cmd_histogram_writer | cmd_plot_writer")
    call ifile_append (ifile, "GRO options = '{' local_command_list '}'")
    call ifile_append (ifile, "SEQ local_command_list = local_command*")
    call ifile_append (ifile, "ALT local_command = " &
         // "cmd_model | cmd_library | cmd_iterations | cmd_sample_format | " &
         // "cmd_seed | " &
         // "cmd_var | cmd_slha | " &
         // "cmd_show | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_scale | cmd_fac_scale | cmd_ren_scale | " &
         // "cmd_weight | cmd_selection | cmd_reweight | " &
         // "cmd_beams | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_graph | " &
         // "cmd_clear | cmd_record | " &
         // "cmd_analysis | " &
         // "cmd_beam_polarization | " &
         // "cmd_open_out | cmd_close_out | cmd_printd | cmd_printf | " &
         // "cmd_write_analysis | cmd_compile_analysis | " &
         // "cmd_histogram_writer | cmd_plot_writer")
    call ifile_append (ifile, "SEQ cmd_model = model '=' model_name")
    call ifile_append (ifile, "KEY model")
    call ifile_append (ifile, "ALT model_name = model_id | string_literal")
    call ifile_append (ifile, "IDE model_id")
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
    call ifile_append (ifile, "SEQ cmd_show = show show_arg?")
    call ifile_append (ifile, "KEY show")
    call ifile_append (ifile, "ARG show_arg = ( var_generic* )")
    call ifile_append (ifile, "ALT var_generic = " &
         // "model | beams | results | unstable | real | int | " &
         // "cuts | weight | scale | factorization_scale | renormalization_scale | " & 
         // "analysis | expect | " &
         // "library_spec | " &
         // "intrinsic | result_var | " &
         // "log_var | alias_var | string_var | num_var")
    call ifile_append (ifile, "KEY results")
    call ifile_append (ifile, "KEY intrinsic")
    call ifile_append (ifile, "SEQ library_spec = library lib_name?")
    call ifile_append (ifile, "SEQ result_var = result_key result_arg?")
    call ifile_append (ifile, "SEQ log_var = '?' log_var_spec?")
    call ifile_append (ifile, "ALT log_var_spec = log_val | variable")
    call ifile_append (ifile, "GRO log_val  = ( lexpr )")
    call ifile_append (ifile, "SEQ alias_var = alias alias_var_spec?")
    call ifile_append (ifile, "ALT alias_var_spec = alias_val | variable")
    call ifile_append (ifile, "GRO alias_val  = ( cexpr )")
    call ifile_append (ifile, "SEQ string_var = '$' string_var_spec?")  ! $
    call ifile_append (ifile, "ALT string_var_spec = string_val | variable")
    call ifile_append (ifile, "GRO string_val  = ( sexpr )")
    call ifile_append (ifile, "SEQ num_var = num_var_spec")
    call ifile_append (ifile, "ALT num_var_spec = num_val | variable")
    call ifile_append (ifile, "GRO num_val  = ( expr )")
    call ifile_append (ifile, "SEQ cmd_expect = expect expect_arg options?")
    call ifile_append (ifile, "KEY expect")
    call ifile_append (ifile, "ARG expect_arg = ( lexpr )")
    call ifile_append (ifile, "SEQ cmd_cuts = cuts '=' lexpr")
    call ifile_append (ifile, "SEQ cmd_scale = scale '=' expr")    
    call ifile_append (ifile, "SEQ cmd_fac_scale = factorization_scale '=' expr")
    call ifile_append (ifile, "SEQ cmd_ren_scale = renormalization_scale '=' expr")
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
         // "process_prt '=>' process_prt options?")     
    call ifile_append (ifile, "KEY process")
    call ifile_append (ifile, "KEY '=>'")
    call ifile_append (ifile, "LIS process_prt = cexpr+")
    call ifile_append (ifile, "SEQ cmd_compile = compile_cmd options?")
    call ifile_append (ifile, "SEQ compile_cmd = compile_clause compile_arg?")
    call ifile_append (ifile, "SEQ compile_clause = compile exec_name_spec?")
    call ifile_append (ifile, "KEY compile")
    call ifile_append (ifile, "SEQ exec_name_spec = as exec_name")
    call ifile_append (ifile, "KEY as")
    call ifile_append (ifile, "ALT exec_name = exec_id | string_literal")
    call ifile_append (ifile, "IDE exec_id")
    call ifile_append (ifile, "ARG compile_arg = ( lib_name* )")
    call ifile_append (ifile, "SEQ cmd_load = load load_arg?")
    call ifile_append (ifile, "KEY load")
    call ifile_append (ifile, "ARG load_arg = ( lib_name* )")
    call ifile_append (ifile, "SEQ cmd_exec = exec exec_arg")
    call ifile_append (ifile, "KEY exec")
    call ifile_append (ifile, "ARG exec_arg = ( sexpr )")
    call ifile_append (ifile, "SEQ cmd_beams = beams '=' beam_def")
    call ifile_append (ifile, "KEY beams")
    call ifile_append (ifile, "SEQ beam_def = beam_spec strfun_seq*")
    call ifile_append (ifile, "SEQ beam_spec = beam_list options?")
    call ifile_append (ifile, "LIS beam_list = cexpr, cexpr?")
    call ifile_append (ifile, "SEQ strfun_seq = '=>' strfun_pair")
    call ifile_append (ifile, "LIS strfun_pair = strfun_def, strfun_def?")
    call ifile_append (ifile, "SEQ strfun_def = strfun_id options?")
    call ifile_append (ifile, "ALT strfun_id = " &
          // "none | lhapdf | isr | epa | ewa | pdf_builtin | " &
          // "circe1 | circe2 | energy_scan | beam_events | " &
          // "user_sf_spec")
    call ifile_append (ifile, "KEY none")
    call ifile_append (ifile, "KEY lhapdf")
    call ifile_append (ifile, "KEY isr")
    call ifile_append (ifile, "KEY epa")
    call ifile_append (ifile, "KEY ewa")    
    call ifile_append (ifile, "KEY circe1")        
    call ifile_append (ifile, "KEY circe2")
    call ifile_append (ifile, "KEY energy_scan")
    call ifile_append (ifile, "KEY beam_events")
    call ifile_append (ifile, "KEY pdf_builtin")
    call ifile_append (ifile, "SEQ user_sf_spec = user_strfun user_arg")
    call ifile_append (ifile, "KEY user_strfun")
    call ifile_append (ifile, "SEQ cmd_integrate = " &
         // "integrate proc_arg options?") 
    call ifile_append (ifile, "KEY integrate")
    call ifile_append (ifile, "ARG proc_arg = ( proc_id* )")
    call ifile_append (ifile, "IDE proc_id")
    call ifile_append (ifile, "SEQ cmd_me_test = " &
         // "matrix_element_test proc_arg1 options?") 
    call ifile_append (ifile, "KEY matrix_element_test")
    call ifile_append (ifile, "ARG proc_arg1 = ( proc_id )")
    call ifile_append (ifile, "SEQ cmd_seed = seed '=' expr")
    call ifile_append (ifile, "KEY seed")
    call ifile_append (ifile, "SEQ cmd_iterations = " &
         // "iterations '=' iterations_list")
    call ifile_append (ifile, "KEY iterations")
    call ifile_append (ifile, "LIS iterations_list = iterations_spec+")
    call ifile_append (ifile, "ALT iterations_spec = it_spec")
    call ifile_append (ifile, "SEQ it_spec = expr calls_spec adapt_spec?")
    call ifile_append (ifile, "SEQ calls_spec = ':' expr")
    call ifile_append (ifile, "SEQ adapt_spec = ':' sexpr")
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
    call ifile_append (ifile, "SEQ cmd_open_out = open_out open_arg options?")
    call ifile_append (ifile, "SEQ cmd_close_out = close_out open_arg options?")
    call ifile_append (ifile, "KEY open_out")
    call ifile_append (ifile, "KEY close_out")
    call ifile_append (ifile, "ARG open_arg = (sexpr)")
    call ifile_append (ifile, "SEQ cmd_printd = printd_cmd options?")
    call ifile_append (ifile, "SEQ printd_cmd = printd sprintf_args?")
    call ifile_append (ifile, "KEY printd")
    call ifile_append (ifile, "SEQ cmd_printf = printf_cmd options?")
    call ifile_append (ifile, "SEQ printf_cmd = printf_clause sprintf_args?")
    call ifile_append (ifile, "SEQ printf_clause = printf sexpr")
    call ifile_append (ifile, "KEY printf")
    call ifile_append (ifile, "SEQ cmd_clear = clear clear_arg?")
    call ifile_append (ifile, "KEY clear")
    call ifile_append (ifile, "ARG clear_arg = ( clear_obj* )")
    call ifile_append (ifile, "ALT clear_obj = " &
         // "iterations | cuts | weight | scale | factorization_scale | " &
         // "renormalization_scale | analysis | expect | " &
         // "analysis_tag")
    call ifile_append (ifile, "SEQ cmd_record = record_cmd")
    call ifile_append (ifile, "SEQ cmd_unstable = " &
         // "unstable unstable_list options?")
    call ifile_append (ifile, "KEY unstable")
    call ifile_append (ifile, "LIS unstable_list = unstable_decl+")
    call ifile_append (ifile, "SEQ unstable_decl = cexpr unstable_arg")
    call ifile_append (ifile, "ARG unstable_arg = ( proc_id+ )")
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
    call ifile_append (ifile, "SEQ cmd_scan = scan_spec scan_body?")
    call ifile_append (ifile, "SEQ scan_spec = scan scan_command?")
    call ifile_append (ifile, "KEY scan")
    call ifile_append (ifile, "ALT scan_command = " &
         // "cmd_model_list | cmd_library_list | " &
         // "cmd_seed_list | " &
         // "cmd_cuts_list | cmd_scale_list | " &
         // "cmd_fac_scale_list | cmd_ren_scale_list | " &
         // "cmd_weight_list | cmd_selection_list | " &
         // "cmd_reweight_list | cmd_analysis_list | " &
         // "cmd_var_list")
    call ifile_append (ifile, "SEQ cmd_model_list = model = model_list_arg")
    call ifile_append (ifile, "ARG model_list_arg = ( model_name* )")
    call ifile_append (ifile, "SEQ cmd_library_list = " &
         // "library = library_list_arg")
    call ifile_append (ifile, "ARG library_list_arg = ( lib_name* )")
    call ifile_append (ifile, "SEQ cmd_seed_list = seed = num_step_list_arg")
    call ifile_append (ifile, "ARG num_step_list_arg = ( num_steps_expr* )")
    call ifile_append (ifile, "ALT num_steps_expr = grouped_num_steps | num_steps")
    call ifile_append (ifile, "GRO grouped_num_steps = ( num_steps )")
    call ifile_append (ifile, "SEQ num_steps = expr range_spec?")
    call ifile_append (ifile, "SEQ range_spec = '=>' expr step_spec?")
    call ifile_append (ifile, "SEQ step_spec = step_op expr")
    call ifile_append (ifile, "ALT step_op = " &
         // "'/+' | '/-' | '/*' | '//' | '+/+' | '*/*'")
    call ifile_append (ifile, "KEY '/+'")
    call ifile_append (ifile, "KEY '/-'")
    call ifile_append (ifile, "KEY '/*'")
    call ifile_append (ifile, "KEY '//'")
    call ifile_append (ifile, "KEY '+/+'")
    call ifile_append (ifile, "KEY '*/*'")
    call ifile_append (ifile, "ALT cmd_var_list = " &
         // "log_decl_list | log_list | " &
         // "int_list | real_list | complex_list | num_list | " &
         // "string_decl_list | string_list | alias_list")
    call ifile_append (ifile, "SEQ log_decl_list = logical log_list")
    call ifile_append (ifile, "SEQ log_list = '?' var_name '=' log_list_arg")
    call ifile_append (ifile, "ARG log_list_arg = ( lexpr* )")
    call ifile_append (ifile, "SEQ int_list = " &
         // "int var_name '=' num_step_list_arg")
    call ifile_append (ifile, "SEQ real_list = " &
         // "real var_name '=' num_step_list_arg")
    call ifile_append (ifile, "SEQ complex_list = " &
         // "complex var_name '=' num_step_list_arg")
    call ifile_append (ifile, "SEQ num_list = var_name '=' num_step_list_arg")
    call ifile_append (ifile, "SEQ string_decl_list = string string_list")
    call ifile_append (ifile, "SEQ string_list = " &
         // "'$' var_name '=' string_list_arg") !$
    call ifile_append (ifile, "ARG string_list_arg = ( sexpr* )")
    call ifile_append (ifile, "SEQ alias_list = " &
         // "alias var_name alias_list_arg")
    call ifile_append (ifile, "ARG alias_list_arg = ( cexpr* )")
    call ifile_append (ifile, "SEQ cmd_cuts_list = cuts = log_list_arg")
    call ifile_append (ifile, "SEQ cmd_scale_list = scale = num_list_arg")    
    call ifile_append (ifile, "SEQ cmd_fac_scale_list = factorization_scale = num_list_arg")
    call ifile_append (ifile, "SEQ cmd_ren_scale_list = renormalization_scale = num_list_arg")    
    call ifile_append (ifile, "SEQ cmd_weight_list = weight = num_list_arg")
    call ifile_append (ifile, "SEQ cmd_selection_list = selection = log_list_arg")
    call ifile_append (ifile, "SEQ cmd_reweight_list = reweight = num_list_arg")
    call ifile_append (ifile, "ARG num_list_arg = ( expr* )")
    call ifile_append (ifile, "SEQ cmd_analysis_list = analysis = log_list_arg")
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
    call ifile_append (ifile, "SEQ cmd_beam_polarization = " &
         // "beam_polarization '=' bp_mode options?")
    call ifile_append (ifile, "KEY beam_polarization")
    call ifile_append (ifile, "ALT bp_mode = off | bp_defs")
    call ifile_append (ifile, "KEY off")
    call ifile_append (ifile, "LIS bp_defs = bp_def, bp_def?")
    call ifile_append (ifile, "ARG arg_unary = ( expr )")
    call ifile_append (ifile, "ARG arg_binary = (expr, expr)")
    call ifile_append (ifile, "ARG arg_tenary = (expr, expr, expr)")
    call ifile_append (ifile, "ALT bp_def = " &
         // "none | bp_circ | bp_trans | bp_axis | bp_long | bp_diag | " &
         // "bp_dens")
    call ifile_append (ifile, "SEQ bp_circ = circular arg_unary")
    call ifile_append (ifile, "KEY circular")
    call ifile_append (ifile, "SEQ bp_trans = transverse arg_binary")
    call ifile_append (ifile, "KEY transverse")
    call ifile_append (ifile, "SEQ bp_axis = axis arg_tenary")
    call ifile_append (ifile, "KEY axis")
    call ifile_append (ifile, "SEQ bp_long = longitudinal arg_unary")
    call ifile_append (ifile, "KEY longitudinal")
    call ifile_append (ifile, "SEQ bp_dens = density_matrix arg_binary")
    call ifile_append (ifile, "KEY density_matrix")
    call ifile_append (ifile, "SEQ bp_diag = diagonal_density bp_diag_args")
    call ifile_append (ifile, "KEY diagonal_density")
    call ifile_append (ifile, "ARG bp_diag_args = ( bp_diag_entry+ )")
    call ifile_append (ifile, "SEQ bp_diag_entry = expr ':' expr")
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
    call ifile_append (ifile, "SEQ cmd_histogram_writer = " &
         // "histogram_writer '=' writer_macro")
    call ifile_append (ifile, "KEY histogram_writer")
    call ifile_append (ifile, "SEQ cmd_plot_writer = " &
         // "plot_writer '=' writer_macro")
    call ifile_append (ifile, "KEY plot_writer")
    call ifile_append (ifile, "GRO writer_macro = '{' command_list '}'")
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
         special_class = (/ "+-*/^", "<>=~ " /) , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_cmd_list), &
         parent = parent_lexer)
  end subroutine lexer_init_cmd_list

  subroutine command_test ()
    integer :: u
    type(stream_t), target :: stream
    type(lexer_t), target :: lexer
    type(parse_tree_t) :: parse_tree
    type(command_list_t), target :: command_list
    type(rt_data_t), target :: global
    print *, "* Initialization"
    call rt_data_global_init (global)
    allocate (global%prc_lib)
    call syntax_model_file_init ()
    call syntax_phs_forest_init ()
    call syntax_pexpr_init ()
    call syntax_cmd_list_init ()
    call lexer_init_cmd_list (lexer)
    print *, "* Open 'commands2.sin'"
    u = free_unit ()
    open (unit=u, file="commands2.sin")
    call stream_init (stream, u)
    print *, "* Parse"
    global%lexer => lexer
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    call parse_tree_write (parse_tree)
    print *
    print *, "* Compile command list"
    call var_list_append_string (global%var_list, name = "$library_name", sval = "commands2")
    call var_list_append_string (global%var_list, name = "$model_name", sval = "SM")
    call process_library_init (global%prc_lib, var_str("commands2"), global%os_data)
    call model_list_read_model (var_str("SM"), var_str("SM.mdl"), global%os_data, global%model)
    call var_list_init_copies (global%var_list, model_get_var_list_ptr (global%model))
    call var_list_synchronize (global%var_list, model_get_var_list_ptr (global%model), &
              reset_pointers = .true.)
    call var_list_set_string (global%var_list, var_str ("$model_name"), &
         model_get_name (global%model), is_known=.true.)
    if (associated (parse_tree_get_root_ptr (parse_tree))) then
       call command_list_compile &
            (command_list, parse_tree_get_root_ptr (parse_tree), global)
    end if
    print *, "* Execute command list"
    call command_list_execute (command_list, global)
    print *
    print *, "* Cleanup"
    call stream_final (stream)
    close (u)
    call lexer_final (lexer)
    call command_list_final (command_list)
    call process_store_final ()
    call model_list_final ()
    call syntax_cmd_list_final ()
    call syntax_pexpr_final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
    call rt_data_global_final (global)
  end subroutine command_test


end module commands
