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

module commands

  use kinds, only: default !NODEP!
  use kinds, only: double, i64 !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: ITERATIONS_DEFAULT_LIST_SIZE !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
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
  use prt_lists
  use variables
  use expressions
  use models
  use state_matrices
  use flavors
  use quantum_numbers
  use polarizations
  use event_formats
  use hepmc_interface
  use stdhep_interface
  use beams
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_lhapdf
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

  implicit none
  private

  public :: rt_data_global_init
  public :: rt_data_global_final
  public :: simulation_t
  public :: simulation_setup_analysis
  public :: simulation_init
  public :: command_list_t
  public :: command_list_final
  public :: command_list_write
  public :: command_list_compile
  public :: command_list_execute
  public :: syntax_cmd_list
  public :: syntax_cmd_list_init
  public :: syntax_cmd_list_final
  public :: syntax_cmd_list_write
  public :: lexer_init_cmd_list
  public :: command_test

  integer, parameter :: ST_NONE = 0
  integer, parameter :: ST_SINGLE = 1
  integer, parameter :: ST_LINEAR = 2
  integer, parameter :: ST_LOG = 3

  integer, parameter :: FMT_NONE = 0
  integer, parameter :: FMT_DEFAULT = 1
  integer, parameter :: FMT_DEBUG = 2
  integer, parameter :: FMT_HEPMC = 10
  integer, parameter :: FMT_LHEF = 20
  integer, parameter :: FMT_LHA = 21
  integer, parameter :: FMT_HEPEVT = 30
  integer, parameter :: FMT_ASCII_SHORT = 31
  integer, parameter :: FMT_ASCII_LONG = 32
  integer, parameter :: FMT_ATHENA = 33
  integer, parameter :: FMT_STDHEP = 40
  integer, parameter :: FMT_STDHEP_UP = 41
  integer, parameter :: CMD_NONE = 0
  integer, parameter :: CMD_PROCESS = 1
  integer, parameter :: CMD_INTEGRATE = 2
  integer, parameter :: CMD_SIMULATE = 3

  integer, parameter :: CMD_COMPILE = 10
  integer, parameter :: CMD_LOAD = 11
  integer, parameter :: CMD_EXEC = 13

  integer, parameter :: CMD_BEAMS = 21

  integer, parameter :: CMD_MODEL = 31
  integer, parameter :: CMD_LIBRARY = 32
  integer, parameter :: CMD_CUTS = 33
  integer, parameter :: CMD_WEIGHT = 34
  integer, parameter :: CMD_SCALE = 35

  integer, parameter :: CMD_VAR = 41
  integer, parameter :: CMD_PRINTD = 43
  integer, parameter :: CMD_PRINTF = 44
  integer, parameter :: CMD_SHOW = 45
  integer, parameter :: CMD_EXPECT = 46
  integer, parameter :: CMD_ECHO = 47

  integer, parameter :: CMD_UNSTABLE = 51
  integer, parameter :: CMD_STABLE = 52

  integer, parameter :: CMD_SEED = 63
  integer, parameter :: CMD_ITERATIONS = 64
  integer, parameter :: CMD_SAMPLE_FORMAT = 65

  integer, parameter :: CMD_ANALYSIS = 70
  integer, parameter :: CMD_WRITE_ANALYSIS = 71
  integer, parameter :: CMD_OBSERVABLE = 72
  integer, parameter :: CMD_HISTOGRAM = 73
  integer, parameter :: CMD_PLOT = 74
  integer, parameter :: CMD_CLEAR = 75
  integer, parameter :: CMD_RECORD = 76
  
  integer, parameter :: CMD_INCLUDE = 91
  integer, parameter :: CMD_SCAN = 92
  integer, parameter :: CMD_IF = 93
  integer, parameter :: CMD_QUIT = 99

  integer, parameter :: CMD_SLHA = 101

  integer, parameter :: NORM_UNDEFINED = 0
  integer, parameter :: NORM_UNIT = 1
  integer, parameter :: NORM_N_EVT = 2
  integer, parameter :: NORM_SIGMA = 3
  integer, parameter :: NORM_SIGMA_N_EVT = 4

  character(*), parameter :: &
     checkpoint_head = &
        "| % complete | events generated | events remaining | time remaining", &
     checkpoint_bar = &
        "|===================================================================|", &
     checkpoint_fmt = "('   ',F5.1,T16,I9,T35,I9,T56,A)"

  integer, parameter :: STEP_NONE = 0
  integer, parameter :: STEP_ADD = 1
  integer, parameter :: STEP_SUB = 2
  integer, parameter :: STEP_MUL = 3
  integer, parameter :: STEP_DIV = 4

  type :: step_spec_t
     private
     integer :: type = ST_NONE
     type(eval_tree_t) :: expr_beg
     type(eval_tree_t) :: expr_end
     type(eval_tree_t) :: expr_step
     type(step_spec_t), pointer :: next => null ()
  end type step_spec_t

  type :: step_list_t
     private
     type(step_spec_t), pointer :: first => null ()
     type(step_spec_t), pointer :: last => null ()
  end type step_list_t

  type :: sf_mapping_t
     private
     integer, dimension(:), allocatable :: index
     integer :: type = SFM_NONE
     real(default), dimension(:), allocatable :: par
  end type sf_mapping_t

  type :: sf_data_t
     private
     integer :: type = STRF_NONE
     logical, dimension(2) :: affects_beam = .false.
     integer :: n_parameters = 0
     type(lhapdf_data_t), dimension(2) :: lhapdf
     type(isr_data_t), dimension(2) :: isr
     type(epa_data_t), dimension(2) :: epa
     type(ewa_data_t), dimension(2) :: ewa     
     logical :: has_mapping = .false.
     type(sf_mapping_t) :: mapping
     type(sf_data_t), pointer :: next => null ()
  end type sf_data_t

  type :: sf_list_t
     private
     integer :: n_strfun = 0
     integer :: n_mapping = 0
     type(sf_data_t), pointer :: first => null ()
     type(sf_data_t), pointer :: last => null ()
     character(32) :: md5sum = ""
  end type sf_list_t

  type :: iterations_spec_t
     private
     integer :: n_it = 0
     integer :: n_calls = 0
  end type iterations_spec_t

  type :: iterations_list_t
     private
     integer :: n_pass = 0
     type(iterations_spec_t), dimension(:), allocatable :: pass
  end type iterations_list_t
     
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

  type :: file_list_t
     private
     type(file_spec_t), pointer :: first => null ()
     type(file_spec_t), pointer :: last => null ()
  end type file_list_t

  type :: process_p
     type(process_t), pointer :: ptr
  end type process_p

  public :: rt_data_t
  type :: rt_data_t
     type(lexer_t), pointer :: lexer => null ()
     type(var_list_t) :: var_list
     type(iterations_list_t) :: it_list
     type(iterations_list_t), dimension(:), pointer :: it_list_default
     integer, dimension(:), allocatable :: event_fmt
     type(os_data_t) :: os_data
     type(process_library_t), pointer :: prc_lib => null ()
     type(model_t), pointer :: model => null ()
     type(beam_data_t) :: beam_data
     type(lhapdf_status_t) :: lhapdf_status
     logical :: sf_list_allocated = .false.
     type(sf_list_t), pointer  :: sf_list => null ()
     type(parse_node_t), pointer :: pn_cuts_lexpr => null ()
     type(parse_node_t), pointer :: pn_weight_expr => null ()
     type(parse_node_t), pointer :: pn_scale_expr => null ()
     type(parse_node_t), pointer :: pn_analysis_lexpr => null ()
     type(tao_random_state), pointer :: rng => null ()
     integer :: seed
     integer :: method = PRC_UNDEFINED
     logical :: quit = .false.
     integer :: quit_code = 0
  end type rt_data_t

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
     type(cmd_printd_t), pointer :: printd => null ()
     type(cmd_printf_t), pointer :: printf => null ()
     type(cmd_show_t), pointer :: show => null ()
     type(cmd_expect_t), pointer :: expect => null ()
     type(cmd_echo_t), pointer :: echo => null ()
     type(cmd_beams_t), pointer :: beams => null ()
     type(cmd_cuts_t), pointer :: cuts => null ()
     type(cmd_weight_t), pointer :: weight => null ()
     type(cmd_scale_t), pointer :: scale => null ()
     type(cmd_seed_t), pointer :: seed => null ()
     type(cmd_iterations_t), pointer :: iterations => null ()
     type(cmd_integrate_t), pointer :: integrate => null ()
     type(cmd_observable_t), pointer :: observable => null ()
     type(cmd_histogram_t), pointer :: histogram => null ()
     type(cmd_plot_t), pointer :: plot => null ()
     type(cmd_clear_t), pointer :: clear => null ()
     type(cmd_record_t), pointer :: record => null ()
     type(cmd_analysis_t), pointer :: analysis => null ()
     type(cmd_unstable_t), pointer :: unstable => null ()
     type(cmd_stable_t), pointer :: stable => null ()
     type(cmd_sample_format_t), pointer :: events => null ()
     type(cmd_simulate_t), pointer :: simulate => null ()
     type(cmd_write_analysis_t), pointer :: write_analysis => null ()
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
     type(eval_tree_t), dimension(:), allocatable :: pdg_in
     type(eval_tree_t), dimension(:), allocatable :: pdg_out
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
     type(eval_tree_t) :: command
  end type cmd_exec_t

  type :: cmd_var_t
     private
     type(string_t) :: name
     integer :: type = V_NONE
     type(eval_tree_t) :: value
     logical, pointer :: is_known => null ()
     logical, pointer :: lval => null ()
     integer, pointer :: ival => null ()
     real(default), pointer :: rval => null ()
     complex(default), pointer :: cval => null ()
     type(string_t), pointer :: sval => null ()
     type(pdg_array_t), pointer :: aval => null ()
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

  type :: cmd_printd_t
     private
     type(eval_tree_t) :: sexpr
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_printd_t

  type :: cmd_printf_t
     private
     type(eval_tree_t) :: sexpr
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_printf_t

  type :: cmd_show_t
     private
     type(string_t), dimension(:), allocatable :: name
     type(eval_tree_t), dimension(:), allocatable :: expr
     type(var_entry_t), dimension(:), allocatable :: value
  end type cmd_show_t

  type :: cmd_expect_t
     private
     type(eval_tree_t) :: lexpr
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_expect_t

  type :: cmd_echo_t
     private
     type(eval_tree_t), dimension(:), allocatable :: expr
  end type cmd_echo_t

  type :: strfun_def_t
     private
     integer :: type = STRF_NONE
     integer :: n_parameters = 1
     type(command_list_t), pointer :: options => null ()
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
     type(eval_tree_t), dimension(:), allocatable :: pdg
     type(string_t), dimension(:), allocatable :: prt
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
     logical :: use_sqrts = .true.
     integer :: n_strfun = 0
     type(strfun_pair_t), dimension(:), allocatable :: strfun_pair
  end type cmd_beams_t

  type :: cmd_cuts_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
  end type cmd_cuts_t

  type :: cmd_weight_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_weight_t

  type :: cmd_scale_t
     private
     type(parse_node_t), pointer :: pn_expr => null ()
  end type cmd_scale_t

  type :: cmd_integrate_t
     private
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_integrate_t
     
  type :: cmd_observable_t
     private
     logical :: use_id_expr = .false.
     type(string_t) :: id
     type(eval_tree_t) :: expr_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_observable_t
     
  type :: cmd_histogram_t
     private
     type(string_t) :: id
     logical :: use_id_expr = .false.
     type(eval_tree_t) :: expr_id
     type(eval_tree_t) :: expr_lower_bound
     type(eval_tree_t) :: expr_upper_bound
     type(eval_tree_t) :: expr_bin_width
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_histogram_t
     
  type :: cmd_plot_t
     private
     type(string_t) :: id
     logical :: use_id_expr = .false.
     type(eval_tree_t) :: expr_id
     type(eval_tree_t) :: expr_lower_bound
     type(eval_tree_t) :: expr_upper_bound
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_plot_t
     
  type :: cmd_analysis_t
     private
     type(parse_node_t), pointer :: pn_lexpr => null ()
  end type cmd_analysis_t

  type :: cmd_write_analysis_t
     private
     integer :: n_args = 0
     type(string_t), dimension(:), allocatable :: id
     logical, dimension(:), allocatable :: use_id_expr
     type(eval_tree_t), dimension(:), allocatable :: expr_id
  end type cmd_write_analysis_t
     
  type :: cmd_clear_t
     private
     integer :: n_args = 0
     type(string_t), dimension(:), allocatable :: id
     logical, dimension(:), allocatable :: use_id_expr
     type(eval_tree_t), dimension(:), allocatable :: expr_id
  end type cmd_clear_t
     
  type :: cmd_record_t
     private
     type(eval_tree_t) :: lexpr
  end type cmd_record_t
     
  type :: decay_properties_t
     type(eval_tree_t) :: pdg
     type(string_t) :: prt
     type(flavor_t) :: flv
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     real(default), dimension(:), allocatable :: br
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
     
  type :: cmd_sample_format_t
     private
     type(string_t), dimension(:), allocatable :: format
     integer, dimension(:), allocatable :: fmt
  end type cmd_sample_format_t

  type :: simulation_parameters_t
    logical :: unweighted = .true.
    integer :: normalization_mode = NORM_UNDEFINED
    logical :: negative_weights = .false.
  end type simulation_parameters_t

  type :: checkpointing_t
    logical :: active = .false.
    logical :: running = .false.
    integer :: val = 0
    real(default) :: tzero = 0
  end type checkpointing_t

  type :: simulation_t
    private
    integer :: n_proc = 0
    type(string_t), dimension(:), allocatable :: process_id
    type(process_p), dimension(:), allocatable :: prc_array
    type(var_list_t) :: var_list
    logical :: rebuild_events = .false.
    integer :: n_in = 0
    type(flavor_t), dimension(:), allocatable :: beam_flv
    real(default), dimension(:), allocatable :: beam_energy
    type(string_t) :: basename
    logical :: read_raw = .false.
    logical :: write_raw = .false.
    type(string_t) :: file_raw
    type(file_list_t) :: file_list
    integer :: u_raw = -1
    type(simulation_parameters_t) :: spar
    real(default), dimension(:), allocatable :: integral
    real(default) :: integral_sum = 0
    real(default) :: norm_weight = 0
    type(md5sum_events_t) :: md5sum
    integer :: n_events = 0
    integer :: n_read = 0
    integer :: i_evt = 0
    real(default) :: luminosity = 0
    type(eval_tree_t) :: analysis_expr
    type(prt_list_t) :: prt_list
    real(default) :: event_weight = 0
    real(default) :: event_sqme = 0
    type(decay_tree_t), dimension(:), allocatable :: decay_tree
    type(checkpointing_t) :: checkpointing
    type(event_t) :: event
  end type simulation_t

  type :: cmd_simulate_t
     private
     integer :: n_evt = 0
     integer :: n_proc = 0
     type(string_t), dimension(:), allocatable :: process_id
     type(command_list_t), pointer :: options => null ()
     type(rt_data_t) :: local
  end type cmd_simulate_t

  type :: cmd_seed_t
     private
     type(eval_tree_t) :: expr
  end type cmd_seed_t

  type :: cmd_iterations_t
     private
     integer :: n_pass = 0
     type(eval_tree_t), dimension(:), allocatable :: expr_n_it
     type(eval_tree_t), dimension(:), allocatable :: expr_n_calls
  end type cmd_iterations_t

  type :: cmd_scan_t
     private
     integer :: var_type = V_NONE
     type(string_t) :: var_name
     logical :: allow_steps = .false.
     integer :: n_arg = 0
     type(command_list_t), dimension(:), pointer :: cmd_var => null ()
     logical, dimension(:), allocatable :: has_range
     type(eval_tree_t), dimension(:), allocatable :: beg_expr
     type(eval_tree_t), dimension(:), allocatable :: end_expr
     integer, dimension(:), allocatable :: step_type
     type(eval_tree_t), dimension(:), allocatable :: step_expr
     type(command_list_t), pointer :: body => null ()
     type(rt_data_t) :: local
  end type cmd_scan_t

  type :: cmd_if_t
     private
     type(eval_tree_t) :: if_lexpr
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
     type(eval_tree_t) :: code_expr
  end type cmd_quit_t

  type :: command_list_t
     private
     type(command_t), pointer :: first => null ()
     type(command_t), pointer :: last => null ()
  end type command_list_t


  type(syntax_t), target, save :: syntax_cmd_list


  interface cmd_integrate_init
     module procedure cmd_integrate_init1
     module procedure cmd_integrate_init2
  end interface


contains

  subroutine step_list_append_val (step_list, type, var_list, pn1, pn2, pn3)
    type(step_list_t), intent(inout) :: step_list
    integer, intent(in) :: type
    type(var_list_t), intent(in), target :: var_list
    type(parse_node_t), intent(in), target :: pn1
    type(parse_node_t), intent(in), optional, target :: pn2, pn3
    type(step_spec_t), pointer :: current
    allocate (current)
    current%type = type
    call eval_tree_init_expr (current%expr_beg, pn1, var_list)
    select case (type)
    case (ST_LINEAR, ST_LOG)
       call eval_tree_init_expr (current%expr_end,  pn2, var_list)
       call eval_tree_init_expr (current%expr_step, pn3, var_list)
    end select
    if (associated (step_list%last)) then
       step_list%last%next => current
    else
       step_list%first => current
    end if
    step_list%last => current
  end subroutine step_list_append_val

  subroutine step_list_final (step_list)
    type(step_list_t), intent(inout) :: step_list
    type(step_spec_t), pointer :: current
    do while (associated (step_list%first))
       current => step_list%first
       step_list%first => current%next
       call eval_tree_final (current%expr_beg)
       call eval_tree_final (current%expr_end)
       call eval_tree_final (current%expr_step)
       deallocate (current)
    end do
    step_list%last => null ()
  end subroutine step_list_final

  subroutine sf_mapping_write (sf_mapping, unit)
    type(sf_mapping_t), intent(in) :: sf_mapping
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A,I0,10(', #',I0))")  "Mapping for parameters #", &
         sf_mapping%index
    select case (sf_mapping%type)
    case (SFM_NONE);     write (u, "(3x,A)")  "[none]"
    case (SFM_PDFPAIR);  write (u, "(3x,A)")  "PDF pair mapping"
    case (SFM_ISRPAIR);  write (u, "(3x,A)")  "ISR pair mapping"
    case (SFM_EPAPAIR);  write (u, "(3x,A)")  "EPA pair mapping"
    case (SFM_EWAPAIR);  write (u, "(3x,A)")  "EWA pair mapping"    
    end select
    if (allocated (sf_mapping%par)) then
       write (u, "(3x,A)", advance="no")  "Parameters = "
       write (u, *) sf_mapping%par
    end if
  end subroutine sf_mapping_write
     
  subroutine sf_data_write (sf_data, unit)
    type(sf_data_t), intent(in) :: sf_data
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Structure function"
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          select case (sf_data%type)
          case (STRF_NONE)
             write (u, "(1x,A)") "[none]"
          case (STRF_LHAPDF)
             call lhapdf_data_write (sf_data%lhapdf(i), unit)             
          case (STRF_ISR)
             call isr_data_write (sf_data%isr(i), unit)
          case (STRF_EPA)
             call epa_data_write (sf_data%epa(i), unit)
          case (STRF_EWA)
             call ewa_data_write (sf_data%ewa(i), unit)
          end select
       end if
    end do
    write (u, *)  "affects beams = ", sf_data%affects_beam
    write (u, *)  "n_parameters  = ", sf_data%n_parameters
    if (sf_data%has_mapping) then
       call sf_mapping_write (sf_data%mapping, unit)
    end if
  end subroutine sf_data_write

  subroutine sf_data_init_lhapdf &
       (sf_data, lhapdf_status, model, flv, file, member, photon_scheme)
    type(sf_data_t), intent(inout) :: sf_data
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in), optional :: file
    integer, intent(in), optional :: member
    integer, intent(in), optional :: photon_scheme
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call lhapdf_data_init (sf_data%lhapdf(i), lhapdf_status, &
               model, flv(i), file, member, photon_scheme)
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_PDFPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 2._default
       sf_data%has_mapping = .true.
    end if
  end subroutine sf_data_init_lhapdf

  subroutine sf_data_init_isr &
       (sf_data, model, flv, alpha, q_max, mass, order)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: alpha, q_max
    real(default), intent(in), optional :: mass
    integer, intent(in), optional :: order
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call isr_data_init (sf_data%isr(i), &
               model, flv(i), alpha, q_max, mass)
          if (present (order)) &
               call isr_data_set_order (sf_data%isr(i), order)
          call isr_data_check (sf_data%isr(i))
       end if
    end do
!     if (all (sf_data%affects_beam)) then
!        allocate (sf_data%mapping%index (2))
!        sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
!        sf_data%mapping%type = SFM_ISRPAIR
!        allocate (sf_data%mapping%par (1))
!        sf_data%mapping%par = 2._default
!        sf_data%has_mapping = .true.
!     end if
  end subroutine sf_data_init_isr

  subroutine sf_data_init_epa &
       (sf_data, model, flv, alpha, x_min, q_min, E_max, mass)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: alpha, x_min, q_min, E_max
    real(default), intent(in), optional :: mass
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call epa_data_init (sf_data%epa(i), &
               model, flv(i), alpha, x_min, q_min, E_max, mass)
          call epa_data_check (sf_data%epa(i))
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_EPAPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 1._default
       sf_data%has_mapping = .true.
    end if
  end subroutine sf_data_init_epa

  subroutine sf_data_init_ewa &
       (sf_data, model, flv, x_min, q_min, pt_max, sqrts, &
        keep_momentum, keep_energy, mass)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) ::  x_min, q_min, pt_max, sqrts
    logical, intent(in) :: keep_momentum, keep_energy
    real(default), intent(in), optional :: mass
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call ewa_data_init (sf_data%ewa(i), &
               model, flv(i), x_min, q_min, pt_max, sqrts, &
               keep_momentum, keep_energy, mass)
          call ewa_data_check (sf_data%ewa(i))
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_EWAPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 1._default
       sf_data%has_mapping = .true.
    end if
    if (keep_momentum .or. keep_energy) then
       sf_data%n_parameters = 3
    else
       sf_data%n_parameters = 1
    end if 
  end subroutine sf_data_init_ewa

  subroutine sf_list_write (sf_list, unit)
    type(sf_list_t), intent(in) :: sf_list
    integer, intent(in), optional :: unit
    integer :: u
    type(sf_data_t), pointer :: current
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Structure function list"
    if (associated (sf_list%first)) then
       current => sf_list%first
       do while (associated (current))
          call sf_data_write (current, unit)
          current => current%next
       end do
    else
       write (u, "(1x,A)") "[empty]"
    end if
  end subroutine sf_list_write

  subroutine sf_list_append (sf_list, type, affects_beam, n_parameters, current)
    type(sf_list_t), intent(inout) :: sf_list
    integer, intent(in) :: type
    logical, dimension(2), intent(in) :: affects_beam
    integer, intent(in) :: n_parameters
    type(sf_data_t), pointer :: current
    allocate (current)
    current%type = type
    current%affects_beam = affects_beam
    current%n_parameters = n_parameters
    if (associated (sf_list%last)) then
       sf_list%last%next => current
    else
       sf_list%first => current
    end if
    sf_list%last => current
    sf_list%n_strfun = sf_list%n_strfun + count (affects_beam)
  end subroutine sf_list_append
       
  subroutine sf_list_freeze (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), pointer :: current
    sf_list%n_mapping = 0
    current => sf_list%first
    do while (associated (current))
       if (current%has_mapping) then
          sf_list%n_mapping = sf_list%n_mapping + 1
       end if
       current => current%next
    end do
  end subroutine sf_list_freeze

  subroutine sf_list_final (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), pointer :: current
    do while (associated (sf_list%first))
       current => sf_list%first
       sf_list%first => sf_list%first%next
       deallocate (current)
    end do
    sf_list%last => null ()
    sf_list%n_strfun = 0
  end subroutine sf_list_final

  function sf_list_get_n_strfun (sf_list) result (n)
    integer :: n
    type(sf_list_t), intent(in) :: sf_list
    n = sf_list%n_strfun
  end function sf_list_get_n_strfun

  function sf_list_get_n_mapping (sf_list) result (n)
    integer :: n
    type(sf_list_t), intent(in) :: sf_list
    n = sf_list%n_mapping
  end function sf_list_get_n_mapping

  function sf_list_get_md5sum (sf_list) result (sf_md5sum)
    character(32) :: sf_md5sum
    type(sf_list_t), intent(in) :: sf_list
    sf_md5sum = sf_list%md5sum
  end function sf_list_get_md5sum

  subroutine sf_list_compute_md5sum (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    integer :: unit
    unit = free_unit ()
    open (unit = unit, status = "scratch", action = "readwrite")
    call sf_list_write (sf_list, unit)
    rewind (unit)
    sf_list%md5sum = md5sum (unit)
    close (unit)
  end subroutine sf_list_compute_md5sum

  subroutine process_setup_strfun (process, sf_list)
    type(process_t), intent(inout), target :: process
    type(sf_list_t), intent(in) :: sf_list
    type(sf_data_t), pointer :: current
    integer :: i_sf, j, i_map, i_par
    i_sf = 0
    i_map = 0
    i_par = 0
    current => sf_list%first
    do while (associated (current))
       if (current%has_mapping) then
          i_map = i_map + 1
          call process_set_strfun_mapping &
               (process, i_map, i_par + current%mapping%index, &
                current%mapping%type, current%mapping%par)
       end if
       do j = 1, 2
          if (current%affects_beam(j)) then
             i_sf = i_sf + 1
             select case (current%type)
             case (STRF_LHAPDF)
                call process_set_strfun &
                     (process, i_sf, j, current%lhapdf(j), current%n_parameters)
             case (STRF_ISR)
                call process_set_strfun &
                     (process, i_sf, j, current%isr(j), current%n_parameters)
             case (STRF_EPA)
                call process_set_strfun &
                     (process, i_sf, j, current%epa(j), current%n_parameters)
             case (STRF_EWA)
                call process_set_strfun &
                     (process, i_sf, j, current%ewa(j), current%n_parameters)
             end select
             i_par = i_par + current%n_parameters
          end if
       end do
       current => current%next
    end do
  end subroutine process_setup_strfun
       
  subroutine iterations_list_init (it_list, n_it, n_calls)
    type(iterations_list_t), intent(inout) :: it_list
    integer, dimension(:), intent(in) :: n_it, n_calls
    it_list%n_pass = size (n_it)
    if (allocated (it_list%pass)) deallocate (it_list%pass)    
    allocate (it_list%pass (it_list%n_pass))
    it_list%pass%n_it = n_it
    it_list%pass%n_calls = n_calls
  end subroutine iterations_list_init

  subroutine iterations_list_complete (it_list, it_list_default)
    type(iterations_list_t), intent(inout) :: it_list
    type(iterations_list_t), intent(in) :: it_list_default
    if (it_list%n_pass >= 1) then
       if (it_list%pass(1)%n_it == 0)  &
            it_list%pass(1)%n_it = it_list_default%pass(1)%n_it
       if (it_list%pass(1)%n_calls == 0)  &
            it_list%pass(1)%n_calls = it_list_default%pass(1)%n_calls
    end if
    if (it_list%n_pass >= 2) then
       where (it_list%pass%n_it == 0) &
            it_list%pass%n_it = it_list_default%pass(2)%n_it
       where (it_list%pass%n_calls == 0) &
            it_list%pass%n_calls = it_list_default%pass(2)%n_calls
    end if
  end subroutine iterations_list_complete
    
  subroutine iterations_list_clear (it_list)
    type(iterations_list_t), intent(inout) :: it_list
    it_list%n_pass = 0
    deallocate (it_list%pass)
  end subroutine iterations_list_clear

  subroutine iterations_list_write (it_list, unit)
    type(iterations_list_t), intent(in) :: it_list
    integer, intent(in), optional :: unit
    type(string_t) :: buffer
    character(30) :: ibuf
    integer :: i
    buffer = "iterations = "
    if (it_list%n_pass > 0) then
       do i = 1, it_list%n_pass
          if (i > 1)  buffer = buffer // ", "
          write (ibuf, "(I0,':',I0)") &
               it_list%pass(i)%n_it, it_list%pass(i)%n_calls
          buffer = buffer // trim (ibuf)
       end do
    else
       buffer = buffer // "[undefined]"
    end if
    call msg_message (char (buffer), unit)
  end subroutine iterations_list_write

  function iterations_list_get_pass_array (it_list) result (pass)
    integer, dimension(:), allocatable :: pass
    type(iterations_list_t), intent(in) :: it_list
    integer :: it, i
    allocate (pass (sum (it_list%pass%n_it)))
    it = 0
    do i = 1, it_list%n_pass
       pass(it+1 : it+it_list%pass(i)%n_it) = i
       it = it + it_list%pass(i)%n_it
    end do
  end function iterations_list_get_pass_array

  function iterations_list_get_n_calls_array (it_list) result (n_calls)
    integer, dimension(:), allocatable :: n_calls
    type(iterations_list_t), intent(in) :: it_list
    integer :: it, i
    allocate (n_calls (sum (it_list%pass%n_it)))
    it = 0
    do i = 1, it_list%n_pass
       n_calls(it+1 : it+it_list%pass(i)%n_it) = it_list%pass(i)%n_calls
       it = it + it_list%pass(i)%n_it
    end do
  end function iterations_list_get_n_calls_array

  function iterations_list_get_n_it (it_list) result (n_it)
    integer :: n_it
    type(iterations_list_t), intent(in) :: it_list
    n_it = sum (it_list%pass%n_it)
  end function iterations_list_get_n_it

   subroutine iterations_list_adjust_n_calls (it_list, process, grid_parameters)
     type(iterations_list_t), intent(inout), target :: it_list
     type(process_t), intent(in) :: process
     type(grid_parameters_t), intent(in) :: grid_parameters
     type(iterations_spec_t), pointer :: it_spec
     integer :: n_calls, pass
     logical :: changed
     changed = .false.
     do pass = 1, it_list%n_pass
        it_spec => it_list%pass(pass)
        n_calls = max (it_spec%n_calls, &
             process_get_n_channels (process) &
             * grid_parameters%min_calls_per_channel)
        if (n_calls /= it_spec%n_calls) then
           it_spec%n_calls = n_calls
           changed = .true.
        end if
     end do
     if (changed) then
        write (msg_buffer, "(A,I0)") "Process '" &
             // char (process_get_id (process)) // "': " &
             // "resetting n_calls to ", n_calls
        call msg_warning ()
     end if
  end subroutine iterations_list_adjust_n_calls

  subroutine iterations_lists_init_default (it_list)
    type(iterations_list_t), dimension(:), pointer :: it_list
    allocate (it_list (ITERATIONS_DEFAULT_LIST_SIZE))
    call iterations_list_init (it_list(1), (/  1 /), (/ 100 /))
    call iterations_list_init (it_list(2), (/  3, 3 /), (/   1000,  10000 /))
    call iterations_list_init (it_list(3), (/  5, 3 /), (/   5000,  10000 /))
    call iterations_list_init (it_list(4), (/ 10, 5 /), (/  10000,  20000 /))
    call iterations_list_init (it_list(5), (/ 10, 5 /), (/  20000,  50000 /))
    call iterations_list_init (it_list(6), (/ 15, 5 /), (/  50000, 100000 /))
    call iterations_list_init (it_list(7), (/ 20, 5 /), (/  50000, 200000 /))
  end subroutine iterations_lists_init_default

  subroutine file_list_append_file_spec &
       (file_list, basename, var_list, format, beam_flv, beam_energy) 
       ! unweighted, negative_weights, &
    type(file_list_t), intent(inout) :: file_list
    type(string_t), intent(in) :: basename
    type(var_list_t), intent(in) :: var_list
    integer, intent(in) :: format
    type(flavor_t), dimension(:), intent(in) :: beam_flv
    real(default), dimension(:), intent(in) :: beam_energy
!     logical, intent(in) :: unweighted, negative_weights
    integer :: n_processes
    type(file_spec_t), pointer :: current
    n_processes = size (beam_flv)
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
    if (associated (file_list%last)) then
       file_list%last%next => current
    else
       file_list%first => current
    end if
    file_list%last => current
  end subroutine file_list_append_file_spec

  subroutine file_list_final (file_list)
    type(file_list_t), intent(inout) :: file_list
    type(file_spec_t), pointer :: current
    do while (associated (file_list%first))
       current => file_list%first
       file_list%first => current%next
       deallocate (current)
    end do
    file_list%last => null ()
  end subroutine file_list_final

  subroutine file_list_open (file_list, process_id, n_events)
    type(file_list_t), intent(inout), target :: file_list
    type(string_t), dimension(:), intent(in) :: process_id
    integer, intent(in) :: n_events
    real(default), dimension(:), allocatable :: integral, error
    type(process_t), pointer :: process 
    type(file_spec_t), pointer :: current
    integer :: i, n_proc
    integer(i64) :: n_events_expected    
    n_proc = size (process_id)
    current => file_list%first
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
        end select
       current => current%next
    end do
  end subroutine file_list_open

  subroutine file_list_write_event (file_list, event, i_proc, i_evt)
    type(file_list_t), intent(in), target :: file_list
    type(event_t), intent(in), target :: event
    integer, intent(in), optional :: i_proc, i_evt
    type(file_spec_t), pointer :: current
    type(hepmc_event_t) :: hepmc_event
    current => file_list%first
    do while (associated (current))
       select case (current%format)
       case (FMT_DEFAULT)
          call event_write (event, current%unit, verbose=.false.)
       case (FMT_DEBUG)
          call event_write (event, current%unit, verbose=.true.)
       case (FMT_HEPMC)
          if (hepmc_is_available ()) then
             call hepmc_event_init (hepmc_event, i_proc, i_evt)
             call event_write_to_hepmc (event, hepmc_event)
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
          call event_write_to_hepeup (event)
          call hepeup_write_lhef (current%unit)
       case (FMT_LHA)
          call event_write_to_hepeup (event)
          call hepeup_write_lha (current%unit)
       case (FMT_STDHEP)
          call event_write_to_hepevt (event, i_evt=i_evt)
          call stdhep_write (STDHEP_HEPEVT)
       case (FMT_STDHEP_UP)
          call event_write_to_hepeup (event)
          call stdhep_write (STDHEP_HEPEUP)
       end select
       current => current%next
    end do
  end subroutine file_list_write_event

  subroutine file_list_close (file_list)
    type(file_list_t), intent(inout), target :: file_list
    type(file_spec_t), pointer :: current
    current => file_list%first
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
  end subroutine file_list_close

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
    case default
       fmt = FMT_NONE
    end select
  end function event_format_code

  subroutine process_ptr_array_create (prc_array, process_id)
    type(process_p), dimension(:), intent(out), allocatable :: prc_array
    type(string_t), dimension(:), intent(in) :: process_id
    integer :: proc, n_proc
    n_proc = size (process_id)
    allocate (prc_array (n_proc))
    do proc = 1, n_proc
       prc_array(proc)%ptr => process_store_get_process_ptr (process_id(proc))
    end do
  end subroutine process_ptr_array_create

  subroutine rt_data_global_init (global, paths)
    type(rt_data_t), intent(out), target :: global
    type(paths_t), intent(in), optional :: paths
    logical, target, save :: known = .true.
    real(default), parameter :: real_specimen = 1.
    call os_data_init (global%os_data, paths)
    allocate (global%rng)
    call system_clock (global%seed)
    call tao_random_create (global%rng, global%seed)
    call var_list_append_int_ptr &
         (global%var_list, var_str ("seed_value"), global%seed, known, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("sqrts"), 0._default, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$model_name"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$restrictions"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$method"), var_str ("omega"), &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?read_color_factors"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_input"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_spectrum"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_decays"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$library_name"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("cm_momentum"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("cm_theta"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("cm_phi"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("luminosity"), 0._default, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$lhapdf_file"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_member"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_photon_scheme"), 0, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_alpha"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_q_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("isr_order"), 3, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_alpha"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_x_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_q_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_e_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_x_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_q_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_pt_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_momentum"), .false., &
          intrinsic=.false.)      
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_energy"), .false., &
          intrinsic=.false.)              
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_is_fixed"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_lhapdf"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_order"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_nf"), 5, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_mz"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("lambda_qcd"), 200.e-3_default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?fatal_beam_decay"), .true., &
          intrinsic=.true.)          
    call var_list_append_log &
         (global%var_list, var_str ("?helicity_selection_active"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("helicity_selection_threshold"), &
          1E10_default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("helicity_selection_cutoff"), 1000, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("threshold_calls"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_calls_per_channel"), 10, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_calls_per_bin"), 10, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_bins"), 3, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("max_bins"), 20, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?stratified"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?use_vamp_equivalences"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("channel_weights_power"), 0.25_default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vis_channels"), .false., &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$phs_file"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_only"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_threshold_s"), 50._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_threshold_t"), 100._default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("phs_off_shell"), 1, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("phs_t_channel"), 2, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_e_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_m_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_q_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?adapt_final_grids"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?adapt_final_weights"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?isotropic_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diagonal_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("n_events"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?unweighted"), .true., &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$event_normalization"), var_str ("auto"),&
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?negative_weights"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?keep_beams"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$sample"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?read_raw"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?write_raw"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_raw"), var_str ("evx"), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_default"), var_str ("evt"), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_debug"), var_str ("debug"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepevt"), var_str ("hepevt"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_ascii_short"), var_str ("short.evt"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_ascii_long"), var_str ("long.evt"), &
          intrinsic=.true.)      
    call var_list_append_string &
         (global%var_list, var_str ("$extension_athena"), var_str ("athena.evt"), &
          intrinsic=.true.) 
    call var_list_append_string &
         (global%var_list, var_str ("$extension_lhef"), var_str ("lhef"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_lha"), var_str ("lha"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepmc"), var_str ("hepmc"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_stdhep"), var_str ("stdhep"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_stdhep_up"), var_str ("up.stdhep"), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$analysis_filename"), &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("n_bins"), 20, &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$label"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$physical_unit"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$title"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$description"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$xlabel"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$ylabel"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?x_log"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?y_log"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_min"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_max"),  &
          intrinsic=.true.)
    call var_list_append_real (global%var_list, &
         var_str ("tolerance"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("checkpoint"), intrinsic = .true.)
    call var_list_append_int (global%var_list, var_str ("real_range"), &
         range (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, var_str ("real_precision"), &
         precision (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_epsilon"), &
         epsilon (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_tiny"), &
         tiny (real_specimen), intrinsic = .true., locked = .true.)
    call rt_data_init_pointer_variables (global)
    call iterations_lists_init_default (global%it_list_default)
  end subroutine rt_data_global_init

  subroutine rt_data_local_init (local, global)
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    call var_list_link (local%var_list, global%var_list)
    if (associated (global%model)) then
       call var_list_init_copies (local%var_list, &
            model_get_var_list_ptr (global%model), &
            derived_only = .true.)
    end if
    call rt_data_init_pointer_variables (local)
  end subroutine rt_data_local_init

  subroutine rt_data_init_pointer_variables (local)
    type(rt_data_t), intent(inout), target :: local
    logical, target, save :: known = .true.
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fc"), local%os_data%fc, known, &
          intrinsic=.true.)
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fcflags"), local%os_data%fcflags, known, &
         intrinsic=.true.)
  end subroutine rt_data_init_pointer_variables

  subroutine rt_data_link (local, global)
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    local%lexer => global%lexer
    call var_list_link (local%var_list, global%var_list)
    if (associated (global%model)) then
       call var_list_synchronize (local%var_list, &
            model_get_var_list_ptr (global%model), reset_pointers = .true.)
    end if
    local%it_list = global%it_list
    local%it_list_default => global%it_list_default
    if (allocated (global%event_fmt)) then
       allocate (local%event_fmt (size (global%event_fmt)))
       local%event_fmt = global%event_fmt
    end if
    local%os_data = global%os_data
    local%prc_lib => global%prc_lib
    local%model => global%model
    local%beam_data = global%beam_data
    local%lhapdf_status = global%lhapdf_status
    local%sf_list_allocated = .false.
    local%sf_list => global%sf_list
    local%pn_cuts_lexpr => global%pn_cuts_lexpr
    local%pn_weight_expr => global%pn_weight_expr
    local%pn_scale_expr => global%pn_scale_expr
    local%pn_analysis_lexpr => global%pn_analysis_lexpr
    local%rng => global%rng
  end subroutine rt_data_link

  subroutine rt_data_restore (global, local, keep_model_vars)
    type(rt_data_t), intent(inout) :: global
    type(rt_data_t), intent(in) :: local
    logical, intent(in), optional :: keep_model_vars
    logical :: same_model, restore
    if (associated (global%model)) then 
       same_model = &
            model_get_name (global%model) == model_get_name (local%model)
       if (present (keep_model_vars) .and. same_model) then
          restore = .not. keep_model_vars
       else
          if (.not. same_model)  call msg_message ("Restoring model '" // &
               char (model_get_name (global%model)) // "'")
          restore = .true.
       end if
       if (restore) then
          call var_list_restore (global%var_list)
       else
          call var_list_synchronize &
               (global%var_list, model_get_var_list_ptr (global%model))
       end if
    end if
  end subroutine rt_data_restore

  subroutine rt_data_global_final (global)
    type(rt_data_t), intent(inout) :: global
    call var_list_final (global%var_list)
    if (global%sf_list_allocated) then
       call sf_list_final (global%sf_list)
       deallocate (global%sf_list)
       global%sf_list_allocated = .false.
    end if
    deallocate (global%it_list_default)
  end subroutine rt_data_global_final

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
    case (CMD_PRINTD)
       call cmd_printd_final (command%printd)
       deallocate (command%printd)
    case (CMD_PRINTF)
       call cmd_printf_final (command%printf)
       deallocate (command%printf)
    case (CMD_SHOW)
       call cmd_show_final (command%show)
       deallocate (command%show)
    case (CMD_EXPECT)
       call cmd_expect_final (command%expect)
       deallocate (command%expect)
    case (CMD_ECHO)
       call cmd_echo_final (command%echo)
       deallocate (command%echo)
    case (CMD_BEAMS)
       call cmd_beams_final (command%beams)
       deallocate (command%beams)
    case (CMD_CUTS)
       deallocate (command%cuts)
    case (CMD_WEIGHT)
       deallocate (command%weight)
    case (CMD_SCALE)
       deallocate (command%scale)
    case (CMD_SEED)
       call cmd_seed_final (command%seed)
       deallocate (command%seed)
    case (CMD_ITERATIONS)
       call cmd_iterations_final (command%iterations)
       deallocate (command%iterations)
    case (CMD_INTEGRATE)
       call cmd_integrate_final (command%integrate)
       deallocate (command%integrate)
    case (CMD_OBSERVABLE)
       call cmd_observable_final (command%observable)
       deallocate (command%observable)
    case (CMD_HISTOGRAM)
       call cmd_histogram_final (command%histogram)
       deallocate (command%histogram)
    case (CMD_PLOT)
       call cmd_plot_final (command%plot)
       deallocate (command%plot)
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
    case (CMD_SAMPLE_FORMAT)
       call cmd_sample_format_final (command%events)
       deallocate (command%events)
    case (CMD_SIMULATE)
       call cmd_simulate_final (command%simulate)
       deallocate (command%simulate)
    case (CMD_WRITE_ANALYSIS)
       call cmd_write_analysis_final (command%write_analysis)
       deallocate (command%write_analysis)
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

  recursive subroutine command_write (command, unit, indent)
    type(command_t), intent(in) :: command
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    select case (command%type)
    case (CMD_NONE)
       write (u, *) "[Empty command]"
    case (CMD_MODEL)
       call cmd_model_write (command%model, unit, indent)
    case (CMD_LIBRARY)
       call cmd_library_write (command%library, unit, indent)
    case (CMD_VAR)
       call cmd_var_write (command%var, unit, indent)
    case (CMD_SLHA)
       call cmd_slha_write (command%slha, unit, indent)
    case (CMD_PROCESS)
       call cmd_process_write (command%process, unit, indent)
    case (CMD_COMPILE)
       call cmd_compile_write (command%compile, unit, indent)
    case (CMD_EXEC)
       call cmd_exec_write (command%exec, unit, indent)
    case (CMD_BEAMS)
       call cmd_beams_write (command%beams, unit, indent)
    case (CMD_CUTS)
       call cmd_cuts_write (command%cuts, unit, indent)
    case (CMD_WEIGHT)
       call cmd_weight_write (command%weight, unit, indent)
    case (CMD_SCALE)
       call cmd_scale_write (command%scale, unit, indent)
    case (CMD_SEED)
       call cmd_seed_write (command%seed, unit, indent)
    case (CMD_ITERATIONS)
       call cmd_iterations_write (command%iterations, unit, indent)
    case (CMD_INTEGRATE)
       call cmd_integrate_write (command%integrate, unit, indent)
    case (CMD_OBSERVABLE)
       call cmd_observable_write (command%observable, unit, indent)
    case (CMD_HISTOGRAM)
       call cmd_histogram_write (command%histogram, unit, indent)
    case (CMD_PLOT)
       call cmd_plot_write (command%plot, unit, indent)
    case (CMD_ANALYSIS)
       call cmd_analysis_write (command%analysis, unit, indent)
    case (CMD_UNSTABLE)
       call cmd_unstable_write (command%unstable, unit, indent)
    case (CMD_STABLE)
       call cmd_stable_write (command%stable, unit, indent)
    case (CMD_SAMPLE_FORMAT)
       call cmd_sample_format_write (command%events, unit, indent)
    case (CMD_SIMULATE)
       call cmd_simulate_write (command%simulate, unit, indent)
    case (CMD_SCAN)
       call cmd_scan_write (command%loop, unit, indent)
    case (CMD_IF)
       call cmd_if_write (command%cond, unit, indent)
    case (CMD_INCLUDE)
       call cmd_include_write (command%include, unit, indent)
    case (CMD_QUIT)
       call cmd_quit_write (command%quit, unit, indent)
    end select
  end subroutine command_write

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
          "cmd_alias")
       command%type = CMD_VAR
       call cmd_var_compile (command%var, pn, global)
    case ("cmd_slha")
       command%type = CMD_SLHA
       call cmd_slha_compile (command%slha, pn, global)
    case ("cmd_print")
       command%type = CMD_PRINTD
       call cmd_printd_compile (command%printd, pn, global)
    case ("cmd_printf")
       command%type = CMD_PRINTF
       call cmd_printf_compile (command%printf, pn, global)
    case ("cmd_show")
       command%type = CMD_SHOW
       call cmd_show_compile (command%show, pn, global)
    case ("cmd_expect")
       command%type = CMD_EXPECT
       call cmd_expect_compile (command%expect, pn, global)
    case ("cmd_echo")
       command%type = CMD_ECHO
       call cmd_echo_compile (command%echo, pn, global)
    case ("cmd_beams")
       command%type = CMD_BEAMS
       call cmd_beams_compile (command%beams, pn, global)
    case ("cmd_cuts")
       command%type = CMD_CUTS
       call cmd_cuts_compile (command%cuts, pn)
    case ("cmd_weight")
       command%type = CMD_WEIGHT
       call cmd_weight_compile (command%weight, pn)
    case ("cmd_scale")
       command%type = CMD_SCALE
       call cmd_scale_compile (command%scale, pn)
    case ("cmd_seed")
       command%type = CMD_SEED
       call cmd_seed_compile (command%seed, pn, global)
    case ("cmd_iterations")
       command%type = CMD_ITERATIONS
       call cmd_iterations_compile (command%iterations, pn, global)
    case ("cmd_integrate")
       command%type = CMD_INTEGRATE
       call cmd_integrate_compile (command%integrate, pn, global)
    case ("cmd_observable")
       command%type = CMD_OBSERVABLE
       call cmd_observable_compile (command%observable, pn, global)
    case ("cmd_histogram")
       command%type = CMD_HISTOGRAM
       call cmd_histogram_compile (command%histogram, pn, global)
    case ("cmd_plot")
       command%type = CMD_PLOT
       call cmd_plot_compile (command%plot, pn, global)
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
    case ("cmd_sample_format")
       command%type = CMD_SAMPLE_FORMAT
       call cmd_sample_format_compile (command%events, pn)
    case ("cmd_simulate")
       command%type = CMD_SIMULATE
       call cmd_simulate_compile (command%simulate, pn, global)
    case ("cmd_write_analysis")
       command%type = CMD_WRITE_ANALYSIS
       call cmd_write_analysis_compile (command%write_analysis, pn, global)
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

  recursive subroutine command_execute (command, global, print)
    type(command_t), intent(inout) :: command
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: print
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
    case (CMD_PRINTD)
       call cmd_printd_execute (command%printd, global)
    case (CMD_PRINTF)
       call cmd_printf_execute (command%printf, global)
    case (CMD_SHOW)
       call cmd_show_execute (command%show, global)
    case (CMD_EXPECT)
       call cmd_expect_execute (command%expect, global)
    case (CMD_ECHO)
       call cmd_echo_execute (command%echo, global)
    case (CMD_BEAMS)
       call cmd_beams_execute (command%beams, global)
    case (CMD_CUTS)
       call cmd_cuts_execute (command%cuts, global)
    case (CMD_WEIGHT)
       call cmd_weight_execute (command%weight, global)
    case (CMD_SCALE)
       call cmd_scale_execute (command%scale, global)
    case (CMD_SEED)
       call cmd_seed_execute (command%seed, global)
    case (CMD_ITERATIONS)
       call cmd_iterations_execute (command%iterations, global)
    case (CMD_INTEGRATE)
       call cmd_integrate_execute (command%integrate, global)
    case (CMD_OBSERVABLE)
       call cmd_observable_execute (command%observable, global)
    case (CMD_HISTOGRAM)
       call cmd_histogram_execute (command%histogram, global)
    case (CMD_PLOT)
       call cmd_plot_execute (command%plot, global)
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
    case (CMD_SAMPLE_FORMAT)
       call cmd_sample_format_execute (command%events, global)
    case (CMD_SIMULATE)
       call cmd_simulate_execute (command%simulate, global)
    case (CMD_WRITE_ANALYSIS)
       call cmd_write_analysis_execute (command%write_analysis, global)
    case (CMD_SCAN)
       call cmd_scan_execute (command%loop, global)
    case (CMD_IF)
       call cmd_if_execute (command%cond, global)
    case (CMD_INCLUDE)
       call cmd_include_execute (command%include, global)
    case (CMD_QUIT)
       call cmd_quit_execute (command%quit, global)
    end select
    if (present (print)) then
       if (print)  call command_write (command)
    end if
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
    call process_library_store_append &
         (library%name, global%os_data, global%prc_lib)
    rebuild_library = var_list_get_lval (global%var_list, var_str ("?rebuild_library"))
    recompile_library = var_list_get_lval (global%var_list, var_str ("?recompile_library"))
    if (.not. (rebuild_library .or. recompile_library)) then
       call process_library_load (global%prc_lib, &
            global%os_data, var_list=global%var_list, ignore=.true.)
    else
       call var_list_set_string (global%var_list, var_str ("$library_name"), &
            process_library_get_name (global%prc_lib), is_known=.true.)
    end if
  end subroutine cmd_library_execute

  subroutine cmd_process_final (process)
    type(cmd_process_t), intent(inout) :: process
    integer :: i
    if (allocated (process%pdg_in)) then
       do i = 1, size (process%pdg_in)
          call eval_tree_final (process%pdg_in(i))
       end do
    end if
    if (allocated (process%pdg_out)) then
       do i = 1, size (process%pdg_out)
          call eval_tree_final (process%pdg_out(i))
       end do
    end if
    if (associated (process%options)) then
       call command_list_final (process%options)
       deallocate (process%options)
    end if
  end subroutine cmd_process_final

  subroutine cmd_process_write (process, unit, indent)
    type(cmd_process_t), intent(in) :: process
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A,1x,A,1x,A,1x)", advance="no") &
         "process", char (process%id), "="
    do i = 1, process%n_in
       if (i /= 1)  write (u, "(',')", advance="no")
       write (u, "(1x,A)", advance="no") char (process%prt_in(i))
    end do
    write (u, "(1x,A,1x)", advance="no")  "=>"
    do i = 1, process%n_out
       if (i /= 1)  write (u, "(',')", advance="no")
       write (u, "(1x,A)", advance="no") char (process%prt_out(i))
    end do
    if (associated (process%options)) then
       write (u, "(1x,'{')")
       call command_list_write (process%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    else
       write (u, *)
    end if
  end subroutine cmd_process_write

  subroutine cmd_process_compile (process, pn, global)
    type(cmd_process_t), pointer :: process
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
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
    allocate (process%pdg_in (process%n_in), process%prt_in (process%n_in))
    do i = 1, process%n_in
       call eval_tree_init_cexpr &
            (process%pdg_in(i), pn_codes, process%local%var_list)
       process%prt_in(i) = "?"
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
    pn_codes => parse_node_get_sub_ptr (pn_out)
    allocate (process%pdg_out (process%n_out), process%prt_out (process%n_out))
    do i = 1, process%n_out
       call eval_tree_init_cexpr &
            (process%pdg_out(i), pn_codes, process%local%var_list)
       process%prt_out(i) = "?"
       pn_codes => parse_node_get_next_ptr (pn_codes)
    end do
  end subroutine cmd_process_compile

  subroutine cmd_process_execute (process, global)
    type(cmd_process_t), intent(inout), target :: process
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: pdg_in, pdg_out
    integer :: i, method
    logical :: rebuild_library
    type(string_t) :: restrictions, method_str    
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
    do i = 1, size (process%pdg_in)
       call eval_tree_evaluate (process%pdg_in(i))
       pdg_in = eval_tree_get_pdg_array (process%pdg_in(i))
       process%prt_in(i) = make_flavor_string (pdg_in, process%local%model)
    end do
    do i = 1, size (process%pdg_out)
       call eval_tree_evaluate (process%pdg_out(i))
       pdg_out = eval_tree_get_pdg_array (process%pdg_out(i))
       process%prt_out(i) = make_flavor_string (pdg_out, process%local%model)
    end do
    restrictions = var_list_get_sval &
         (process%local%var_list, var_str ("$restrictions"))
    method_str = var_list_get_sval &
         (process%local%var_list, var_str ("$method"))
    method = method_of_string (method_str)
    if (all (scan (process%prt_in, "?") == 0) .and. &
         all (scan (process%prt_out, "?") == 0)) then
       rebuild_library = var_list_get_lval &
            (process%local%var_list, var_str ("?rebuild_library"))
       call process_library_append (global%prc_lib, &
            process%id, process%local%model, &
            process%prt_in, process%prt_out, &
            method = method, restrictions = restrictions, &
            rebuild_library = rebuild_library, message = .true.)
    else
       call msg_error ("Broken process declaration: skipped")
    end if
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

  subroutine cmd_compile_write (compile, unit, indent)
    type(cmd_compile_t), intent(in) :: compile
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "compile "
    if (compile%make_executable) then
       write (u, "(A)", advance="no") "as " // char (compile%exec_name) // " "
    end if
    write (u, "(A)", advance="no") "("
    do i = 1, size (compile%libname)
       if (i /= 1)  write (u, "(',',1x)", advance="no")
       write (u, "(A)", advance="no") '"' // char (compile%libname(i)) // '"'
    end do
    write (u, "(A)", advance="no") ")"
    if (associated (compile%options)) then
       write (u, "(1x,'{')")
       call command_list_write (compile%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
  end subroutine cmd_compile_write

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
  end subroutine cmd_compile_compile

  subroutine cmd_compile_execute (compile, global)
    type(cmd_compile_t), intent(inout), target :: compile
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: objlist
    type(process_library_t), pointer :: prc_lib
    integer :: i
    logical :: recompile_library
    call rt_data_link (compile%local, global)
    if (associated (compile%options)) then
       call command_list_execute (compile%options, compile%local)
    end if
    do i = 1, size (compile%libname)
       prc_lib => process_library_store_get_ptr (compile%libname(i))
       if (associated (prc_lib)) then
         if (process_library_get_n_processes (prc_lib) > 0) then
            call process_library_generate_code (prc_lib, compile%local%os_data)
            call process_library_write_driver (prc_lib)
            recompile_library = var_list_get_lval &
                 (compile%local%var_list, var_str ("?recompile_library"))
            call process_library_compile &
                 (prc_lib, compile%local%os_data, recompile_library, objlist)
            call process_library_link &
                 (prc_lib, compile%local%os_data, objlist)
         end if
       else
         call msg_fatal ("Process library '" // char (compile%libname(i)) // &
                   "' has not been declared.")
       end if              
    end do
    if (compile%make_executable) then
       call write_library_manager (compile%libname)
       call compile_library_manager (compile%local%os_data)
       call link_executable &
            (compile%libname, compile%exec_name, &
               get_modellibs_flags (prc_lib, compile%local%os_data), &
               compile%local%os_data)
    else
       do i = 1, size (compile%libname)
          call process_library_load (prc_lib, compile%local%os_data, &
               var_list=compile%local%var_list)
       end do
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

  subroutine cmd_load_write (load, unit, indent)
    type(cmd_load_t), intent(in) :: load
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "load ("
    do i = 1, size (load%libname)
       if (i /= 1)  write (u, "(',',1x)", advance="no")
       write (u, "(A)", advance="no") '"' // char (load%libname(i)) // '"'
    end do
    write (u, "(A)", advance="no") ")"
  end subroutine cmd_load_write

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
  end subroutine cmd_load_compile

  subroutine cmd_load_execute (load, global)
    type(cmd_load_t), intent(inout) :: load
    type(rt_data_t), intent(inout), target :: global
    type(process_library_t), pointer :: prc_lib
    integer :: i
    call rt_data_link (load%local, global)
    if (associated (load%options)) then
       call command_list_execute (load%options, load%local)
    end if
    do i = 1, size (load%libname)
       call process_library_store_append &
            (load%libname(i), load%local%os_data, prc_lib)
       call process_library_load &
            (prc_lib, load%local%os_data, var_list=load%local%var_list)
    end do
    global%prc_lib => prc_lib
    call rt_data_restore (global, load%local)
  end subroutine cmd_load_execute

  subroutine cmd_exec_final (exec)
    type(cmd_exec_t), intent(inout) :: exec
    call eval_tree_final (exec%command)
  end subroutine cmd_exec_final

  subroutine cmd_exec_write (exec, unit, indent)
    type(cmd_exec_t), intent(in) :: exec
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "exec ("
    call eval_tree_write (exec%command, unit)
    write (u, "(A)") ")"
  end subroutine cmd_exec_write

  subroutine cmd_exec_compile (exec, pn, global)
    type(cmd_exec_t), pointer :: exec
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_command
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    pn_command => parse_node_get_sub_ptr (pn_arg)
    allocate (exec)
    call eval_tree_init_sexpr (exec%command, pn_command, global%var_list)
  end subroutine cmd_exec_compile

  subroutine cmd_exec_execute (exec, global)
    type(cmd_exec_t), intent(inout) :: exec
    type(rt_data_t), intent(in) :: global
    type(string_t) :: command
    integer :: status
    call eval_tree_evaluate (exec%command)
    if (eval_tree_result_is_known (exec%command)) then
       command = eval_tree_get_string (exec%command)
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
    call eval_tree_final (var%value)
  end subroutine cmd_var_final

  subroutine cmd_var_write (var, unit, indent)
    type(cmd_var_t), intent(in) :: var
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  char (var%name)
    write (u, "(1x, A)")  "="
    call eval_tree_write (var%value, unit)
  end subroutine cmd_var_write

  subroutine cmd_var_compile (var, pn, global)
    type(cmd_var_t), pointer :: var
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_var, pn_name, pn_expr
    type(string_t) :: var_name
    type(var_entry_t), pointer :: var_entry
    integer :: type
    logical :: new
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
    case default
       call parse_node_mismatch &
            ("logical|int|real|complex|?|$|alias|var_name", pn)  ! $
    end select
    if (.not. associated (pn_name)) then   ! handle masked syntax error 
       var%type = V_NONE; return
    end if
    var_name = parse_node_get_string (pn_name)
    select case (type)
    case (V_LOG);  var_name = "?" // var_name
    case (V_STR);  var_name = "$" // var_name    ! $
    end select
    call var_list_check_user_var (global%var_list, var_name, type, new)
    var%type = type
    var%name = var_name
    var_entry => var_list_get_var_ptr &
       (global%var_list, var%name, var%type, follow_link=.false.)
    if (associated (var_entry)) then
       var%is_copy = var_entry_is_copy (var_entry)
    else
       var_entry => var_list_get_var_ptr &
          (global%var_list, var%name, var%type, follow_link=.true.)
       if (associated (var_entry)) then
          var%is_intrinsic = var_entry_is_intrinsic (var_entry)
          if (var_entry_is_copy (var_entry)) then
             var%is_copy = .true.
             call var_list_init_copy (global%var_list, var_entry)
          end if
       end if
       if (.not. var%is_copy) then
          select case (var%type)
          case (V_LOG)
             call var_list_append_log (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          case (V_INT)
             call var_list_append_int (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          case (V_REAL)
             call var_list_append_real (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          case (V_CMPLX)
             call var_list_append_cmplx (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          case (V_PDG)
             call var_list_append_pdg_array (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          case (V_STR)
             call var_list_append_string (global%var_list, var%name, &
                   intrinsic=var%is_intrinsic)
          end select
       end if
    end if
    pn_expr => parse_node_get_next_ptr (pn_name, 2)
    if (associated (pn_expr)) then
       select case (var%type)
       case (V_LOG)
          call eval_tree_init_lexpr (var%value, pn_expr, global%var_list)
          var%lval => eval_tree_get_log_ptr (var%value)
       case (V_INT)
          call eval_tree_init_expr (var%value, pn_expr, global%var_list)
          call eval_tree_convert_result (var%value, var%type)
          var%ival => eval_tree_get_int_ptr (var%value)
       case (V_REAL)
          call eval_tree_init_expr (var%value, pn_expr, global%var_list)
          call eval_tree_convert_result (var%value, var%type)
          var%rval => eval_tree_get_real_ptr (var%value)
       case (V_CMPLX)
          call eval_tree_init_expr (var%value, pn_expr, global%var_list)
          call eval_tree_convert_result (var%value, var%type)
          var%cval => eval_tree_get_cmplx_ptr (var%value)
       case (V_PDG)
          call eval_tree_init_cexpr (var%value, pn_expr, global%var_list)
          var%aval => eval_tree_get_pdg_array_ptr (var%value)
       case (V_STR)
          call eval_tree_init_sexpr (var%value, pn_expr, global%var_list)
          var%sval => eval_tree_get_string_ptr (var%value)
       end select
       var%is_known => eval_tree_result_is_known_ptr (var%value)
    else
       var%type = V_NONE
    end if
  end subroutine cmd_var_compile

  subroutine cmd_var_execute (var, global)
    type(cmd_var_t), intent(inout), target :: var
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: model_name
    type(var_list_t), pointer :: model_vars
    type(var_entry_t) :: model_var
    if (eval_tree_is_defined (var%value)) then
       call eval_tree_evaluate (var%value)
       if (associated (global%model)) then
          model_name = model_get_name (global%model)
          model_vars => model_get_var_list_ptr (global%model)
          if (var%is_copy) then
             call var_list_set_original_pointer (global%var_list, var%name, &
                  model_vars)
          end if
          select case (var%type)
          case (V_LOG)
             call var_list_set_log (global%var_list, var%name, &
                  var%lval, var%is_known, verbose=.true., model_name=model_name)
          case (V_INT)
             call var_list_set_int (global%var_list, var%name, &
                  var%ival, var%is_known, verbose=.true., model_name=model_name)
          case (V_REAL)
             call var_list_set_real (global%var_list, var%name, &
                  var%rval, var%is_known, verbose=.true., model_name=model_name)
          case (V_CMPLX)
             call var_list_set_cmplx (global%var_list, var%name, &
                  var%cval, var%is_known, verbose=.true., model_name=model_name)
          case (V_PDG)
             call var_list_set_pdg_array (global%var_list, var%name, &
                  var%aval, var%is_known, verbose=.true., model_name=model_name)
          case (V_STR)
             call var_list_set_string (global%var_list, var%name, &
                  var%sval, var%is_known, verbose=.true., model_name=model_name)
          end select
          if (var%is_copy) then
             call model_parameters_update (global%model)
             call var_list_synchronize (global%var_list, model_vars)
          end if
       else
          select case (var%type)
          case (V_LOG)
             call var_list_set_log (global%var_list, var%name, &
                  var%lval, var%is_known, verbose=.true.)
          case (V_INT)
             call var_list_set_int (global%var_list, var%name, &
                  var%ival, var%is_known, verbose=.true.)
          case (V_REAL)
             call var_list_set_real (global%var_list, var%name, &
                  var%rval, var%is_known, verbose=.true.)
          case (V_CMPLX)
             call var_list_set_cmplx (global%var_list, var%name, &
                  var%cval, var%is_known, verbose=.true.)
          case (V_PDG)
             call var_list_set_pdg_array (global%var_list, var%name, &
                  var%aval, var%is_known, verbose=.true.)
          case (V_STR)
             call var_list_set_string (global%var_list, var%name, &
                  var%sval, var%is_known, verbose=.true.)
          end select
       end if
    else
       call msg_error ("setting variable '" // char (var%name) &
            // "': right-hand side is undefined")
    end if
  end subroutine cmd_var_execute

  subroutine cmd_slha_final (slha)
    type(cmd_slha_t), intent(inout) :: slha
    if (associated (slha%options)) then
       call command_list_final (slha%options)
       deallocate (slha%options)
    end if
  end subroutine cmd_slha_final

  subroutine cmd_slha_write (slha, unit, indent)
    type(cmd_slha_t), intent(in) :: slha
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    if (slha%write) then
       write (u, "(A)", advance="no")  "write_"
    else
       write (u, "(A)", advance="no")  "read_"
    end if
    write (u, "(A)", advance="no")  "slha"
    write (u, "(1x,A)")  "(" // char (slha%file) // ")"
    if (associated (slha%options)) then
       write (u, "(1x,'{')")
       call command_list_write (slha%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
  end subroutine cmd_slha_write

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
  end subroutine cmd_slha_compile

  subroutine cmd_slha_execute (slha, global)
    type(cmd_slha_t), intent(inout), target :: slha
    type(rt_data_t), intent(inout), target :: global
    type(model_t), pointer :: mdl
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

  subroutine cmd_printd_final (printd)
    type(cmd_printd_t), intent(inout) :: printd
    call eval_tree_final (printd%sexpr)
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
    type(string_t) :: key
    integer :: i, n_args
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
    call eval_tree_init_sexpr (printd%sexpr, pn_sexpr, printd%local%var_list)
    call parse_node_final (pn_sprintd, recursive = .false.)
    call parse_node_final (pn_sexpr, recursive = .false.)
  end subroutine cmd_printd_compile

  subroutine cmd_printd_execute (printd, global)
    type(cmd_printd_t), intent(inout) :: printd
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: name
    type(process_library_t), pointer :: prc_lib
    integer :: i
    call rt_data_link (printd%local, global)
    if (associated (printd%options)) then
       call command_list_execute (printd%options, printd%local)
    end if
    call eval_tree_evaluate (printd%sexpr)
    call msg_result (char (eval_tree_get_string (printd%sexpr)))
    call rt_data_restore (global, printd%local)
  end subroutine cmd_printd_execute

  subroutine cmd_printf_final (printf)
    type(cmd_printf_t), intent(inout) :: printf
    call eval_tree_final (printf%sexpr)
    if (associated (printf%options)) then
       call command_list_final (printf%options)
       deallocate (printf%options)
    end if
  end subroutine cmd_printf_final

  subroutine cmd_printf_compile (printf, pn, global)
    type(cmd_printf_t), pointer :: printf
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_cmd, pn_clause, pn_arg, pn_opt
    type(parse_node_t), pointer :: pn_sexpr, pn_sprintf
    type(string_t) :: key
    integer :: i, n_args
    pn_cmd => parse_node_get_sub_ptr (pn)
    pn_opt => parse_node_get_next_ptr (pn_cmd)
    pn_clause => parse_node_get_sub_ptr (pn_cmd)
    pn_arg => parse_node_get_next_ptr (pn_clause)
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
    call eval_tree_init_sexpr (printf%sexpr, pn_sexpr, printf%local%var_list)
    call parse_node_final (pn_sprintf, recursive = .false.)
    call parse_node_final (pn_sexpr, recursive = .false.)
  end subroutine cmd_printf_compile

  subroutine cmd_printf_execute (printf, global)
    type(cmd_printf_t), intent(inout) :: printf
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: name
    type(process_library_t), pointer :: prc_lib
    integer :: i
    call rt_data_link (printf%local, global)
    if (associated (printf%options)) then
       call command_list_execute (printf%options, printf%local)
    end if
    call eval_tree_evaluate (printf%sexpr)
    call msg_result (char (eval_tree_get_string (printf%sexpr)))
    call rt_data_restore (global, printf%local)
  end subroutine cmd_printf_execute

  subroutine cmd_show_final (show)
    type(cmd_show_t), intent(inout) :: show
    integer :: i
    if (allocated (show%expr)) then
       do i = 1, size (show%expr)
          call eval_tree_final (show%expr(i))
       end do
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
       allocate (show%name (n_args), show%expr (n_args), show%value (n_args))
       pn_var => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_var))
          i = i + 1
          select case (char (parse_node_get_rule_key (pn_var)))
          case ("model", "beams", "results", "unstable", &
                "real", "int", "intrinsic", &
                "cuts", "weight", "scale", "analysis", &
                "expect")
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
                   show%name(i) = "<expr>"
                   call eval_tree_init_lexpr &
                        (show%expr(i), pn_name, global%var_list)
                   call var_entry_init_log_ptr (show%value(i), &
                        var_str ("logical value"), &
                        eval_tree_get_log_ptr (show%expr(i)), &
                        eval_tree_result_is_known_ptr (show%expr(i)))
                case ("cexpr")
                   show%name(i) = "<expr>"
                   call eval_tree_init_cexpr &
                        (show%expr(i), pn_name, global%var_list)
                   call var_entry_init_pdg_array_ptr (show%value(i), &
                        var_str ("PDG-array value"), &
                        eval_tree_get_pdg_array_ptr (show%expr(i)), &
                        eval_tree_result_is_known_ptr (show%expr(i)))
                case ("sexpr")
                   show%name(i) = "<expr>"
                   call eval_tree_init_sexpr &
                        (show%expr(i), pn_name, global%var_list)
                   call var_entry_init_string_ptr (show%value(i), &
                        var_str ("string value"), &
                        eval_tree_get_string_ptr (show%expr(i)), &
                        eval_tree_result_is_known_ptr (show%expr(i)))
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
                   call eval_tree_init_expr &
                        (show%expr(i), pn_name, global%var_list)
                   select case (eval_tree_get_result_type (show%expr(i)))
                   case (V_INT)
                      call var_entry_init_int_ptr (show%value(i), &
                           var_str ("integer value"), &
                           eval_tree_get_int_ptr (show%expr(i)), &
                           eval_tree_result_is_known_ptr (show%expr(i)))
                   case (V_REAL)
                      call var_entry_init_real_ptr (show%value(i), &
                           var_str ("real value"), &
                           eval_tree_get_real_ptr (show%expr(i)), &
                           eval_tree_result_is_known_ptr (show%expr(i)))
                   case (V_CMPLX)
                      call var_entry_init_cmplx_ptr (show%value(i), &
                           var_str ("complex value"), &
                           eval_tree_get_cmplx_ptr (show%expr(i)), &
                           eval_tree_result_is_known_ptr (show%expr(i)))
                   end select
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
                call msg_message ("No scale expression defined")
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
          case ("n_calls", &
                "integral", "error", "accuracy", "chi2", "efficiency")
             call var_list_write (global%var_list, prefix=char(show%name(i)))
             call var_list_write (global%var_list, prefix=char(show%name(i)), &
                  unit=u)
          case ("<expr>")
             call eval_tree_evaluate (show%expr(i))
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
    call eval_tree_final (expect%lexpr)
    if (associated (expect%options)) then
       call command_list_final (expect%options)
       deallocate (expect%options)
    end if
  end subroutine cmd_expect_final

  subroutine cmd_expect_compile (expect, pn, global)
    type(cmd_expect_t), pointer :: expect
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_expr, pn_opt
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    pn_opt => parse_node_get_next_ptr (pn_arg)
    allocate (expect)
    call rt_data_local_init (expect%local, global)
    if (associated (pn_opt)) then
       allocate (expect%options)
       call command_list_compile (expect%options, pn_opt, expect%local)
    end if
    pn_expr => parse_node_get_sub_ptr (pn_arg)
    call eval_tree_init_lexpr (expect%lexpr, pn_expr, expect%local%var_list)
  end subroutine cmd_expect_compile

  subroutine cmd_expect_execute (expect, global)
    type(cmd_expect_t), intent(inout) :: expect
    type(rt_data_t), intent(inout), target :: global
    logical :: success
    integer :: u
    u = logfile_unit ()
    call rt_data_link (expect%local, global)
    if (associated (expect%options)) then
       call command_list_execute (expect%options, expect%local)
    end if
    call eval_tree_evaluate (expect%lexpr)
    if (eval_tree_result_is_known (expect%lexpr)) then
       success = eval_tree_get_log (expect%lexpr)
       if (success) then
          call msg_message ("expect: success")
       else
          if (u >= 0) then
             call eval_tree_write (expect%lexpr, unit=u)
             flush (u)
          end if
          call msg_error ("expect: failure")
       end if
    else
       call msg_error ("expect: undefined result")
       success = .false.
    end if
    call expect_record (success)
    call rt_data_restore (global, expect%local)
  end subroutine cmd_expect_execute

  subroutine cmd_echo_final (echo)
    type(cmd_echo_t), intent(inout) :: echo
    integer :: i
    if (allocated (echo%expr)) then
       do i = 1, size (echo%expr)
          call eval_tree_final (echo%expr(i))
       end do
    end if
  end subroutine cmd_echo_final

  subroutine cmd_echo_compile (echo, pn, global)
    type(cmd_echo_t), pointer :: echo
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_sexpr
    integer :: i, n_args
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    allocate (echo)
    if (associated (pn_arg)) then
       n_args = parse_node_get_n_sub (pn_arg)
       allocate (echo%expr (n_args))
       pn_sexpr => parse_node_get_sub_ptr (pn_arg)
       i = 0
       do while (associated (pn_sexpr))
          i = i + 1
          call eval_tree_init_sexpr &
               (echo%expr(i), pn_sexpr, global%var_list)
          pn_sexpr => parse_node_get_next_ptr (pn_sexpr)
       end do
    else
       allocate (echo%expr (0))
    end if
  end subroutine cmd_echo_compile

  subroutine cmd_echo_execute (echo, global)
    type(cmd_echo_t), intent(inout) :: echo
    type(rt_data_t), intent(in), target :: global
    type(string_t) :: string
    integer :: i, u_out, u_log
    call msg_warning &
         ("The 'echo' command is deprecated.  Please use 'print/printf' instead.")
    u_out = output_unit ()
    u_log = logfile_unit (u_out)
    do i = 1, size (echo%expr)
       call eval_tree_evaluate (echo%expr(i))
       if (eval_tree_result_is_known (echo%expr(i))) then
          string = eval_tree_get_string (echo%expr(i))
          write (u_out, "(A)")  char (string)
          if (u_log >= 0)  write (u_log, "(A)")  char (string)
       end if
    end do
    flush (u_log)
  end subroutine cmd_echo_execute

  subroutine cmd_beams_final (beams)
    type(cmd_beams_t), intent(inout) :: beams
    integer :: i
    do i = 1, beams%n_in
       call eval_tree_final (beams%pdg(i))
    end do
    if (associated (beams%options)) then
       call command_list_final (beams%options)
       deallocate (beams%options)
    end if
  end subroutine cmd_beams_final

  subroutine cmd_beams_write (beams, unit, indent)
    type(cmd_beams_t), intent(in) :: beams
    integer, intent(in), optional :: unit, indent
    integer :: u, i, ind
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "beams (" 
    do i = 1, beams%n_in
       if (i /= 1)  write (u, "(', ')", advance="no")
       write (u, "(A)", advance="no")  char (beams%prt(i))
    end do
    write (u, "(A)", advance="no")  ")" 
    if (associated (beams%options)) then
       write (u, "(1x,'{')")
       call command_list_write (beams%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
    do i = 1, beams%n_strfun
       ind = 0;  if (present (indent))  ind = indent
       call strfun_pair_write (beams%strfun_pair(i), unit, indent=ind+3)
    end do
  end subroutine cmd_beams_write

  subroutine strfun_pair_write (strfun_pair, unit, indent)
    type(strfun_pair_t), intent(in) :: strfun_pair
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "=>"
    call strfun_def_write (strfun_pair%def(1), unit, indent)
    if (strfun_pair%n == 2) then
       write (u, "(1x,A)", advance="no")  ","
       call strfun_def_write (strfun_pair%def(2), unit, indent)
    end if
    write (u, *)
  end subroutine strfun_pair_write

  subroutine strfun_def_write (strfun_def, unit, indent)
    type(strfun_def_t), intent(in) :: strfun_def
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    select case (strfun_def%type)
    case (STRF_NONE);    write (u, "(1x,A)")  "none"
    case (STRF_LHAPDF);  write (u, "(1x,A)")  "lhapdf"
    case (STRF_ISR);     write (u, "(1x,A)")  "isr"
    case (STRF_EPA);     write (u, "(1x,A)")  "epa"
    case (STRF_EWA);     write (u, "(1x,A)")  "ewa"    
    end select
    if (associated (strfun_def%options)) then
       write (u, "(1x,'{')")
       call command_list_write (strfun_def%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
  end subroutine strfun_def_write

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
    call rt_data_local_init (beams%local, global)
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
    allocate (beams%pdg (beams%n_in))
    allocate (beams%prt (beams%n_in))
    pn_codes => parse_node_get_sub_ptr (pn_beam_list)
    do i = 1, beams%n_in
       call eval_tree_init_cexpr (beams%pdg(i), pn_codes, global%var_list)
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
    type(parse_node_t), pointer :: pn_key, pn_opt
    pn_key => parse_node_get_sub_ptr (pn_strfun_def)
    pn_opt => parse_node_get_next_ptr (pn_key)
    select case (char (parse_node_get_key (pn_key)))
    case ("none")
       strfun_def%type = STRF_NONE
    case ("lhapdf")
       strfun_def%type = STRF_LHAPDF
    case ("isr")
       strfun_def%type = STRF_ISR
    case ("epa")
       strfun_def%type = STRF_EPA
    case ("ewa")
       strfun_def%type = STRF_EWA
    end select
    call rt_data_local_init (strfun_def%local, global)
    if (associated (pn_opt)) then
       allocate (strfun_def%options)
       call command_list_compile &
            (strfun_def%options, pn_opt, strfun_def%local)
    end if
  end subroutine strfun_def_compile

  subroutine cmd_beams_execute (beams, global)
    type(cmd_beams_t), intent(inout), target :: beams
    type(rt_data_t), intent(inout), target :: global
    real(default) :: sqrts, p_cm, p_cm_theta, p_cm_phi
    type(pdg_array_t), dimension(2) :: aval
    type(flavor_t), dimension(:), allocatable :: flv_tmp
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    integer :: i, u
    u = logfile_unit ()
    call lhapdf_status_reset (global%lhapdf_status)
    call rt_data_link (beams%local, global)
    if (associated (beams%options)) then
       call command_list_execute (beams%options, beams%local)
    end if
    do i = 1, beams%n_in
       call eval_tree_evaluate (beams%pdg(i))
       aval(i) = eval_tree_get_pdg_array (beams%pdg(i))
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
       select case (beams%n_in)
       case (1)
          call polarization_init_trivial (pol(i), flv(i))
       case (2)
          call polarization_init_unpolarized (pol(i), flv(i))
       end select
    end do
    p_cm = var_list_get_rval (beams%local%var_list, var_str ("cm_momentum"))
    p_cm_theta = &
         var_list_get_rval (beams%local%var_list, var_str ("cm_theta"))
    p_cm_phi = &
         var_list_get_rval (beams%local%var_list, var_str ("cm_phi"))
    select case (beams%n_in)
    case (1)
       if (p_cm == 0 .and. p_cm_theta == 0) then
          call beam_data_init_decay (global%beam_data, flv, pol)
       else
          call beam_data_init_decay &
               (global%beam_data, flv, pol, p_cm, p_cm_theta, p_cm_phi)
       end if
    case (2)
       if (beams%use_sqrts) then
          sqrts = var_list_get_rval (beams%local%var_list, var_str ("sqrts"))
          if (sqrts > 0) then
             if (p_cm == 0 .and. p_cm_theta == 0) then
                call beam_data_init_sqrts (global%beam_data, sqrts, flv, pol)
             else
                call beam_data_init_sqrts (global%beam_data, &
                     sqrts, flv, pol, p_cm, p_cm_theta, p_cm_phi)
             end if
          else
             call msg_fatal ("Beam setup: value of sqrts " &
                  // "must be set and positive")
             flush (u)
             call rt_data_restore (global, beams%local)
             return
          end if
       else
          call msg_bug ("Beam setup: individual beam setup not supported yet")
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
       call sf_list_compute_md5sum (global%sf_list)
    end select
    call beam_data_write (global%beam_data, verbose=.false.)
    call beam_data_write (global%beam_data, verbose=.false., unit=u)
    flush (u)
    call rt_data_restore (global, beams%local)
  end subroutine cmd_beams_execute
    
  subroutine strfun_pair_register (strfun_pair, global)
    type(strfun_pair_t), intent(inout) :: strfun_pair
    type(rt_data_t), intent(inout), target :: global
    logical, dimension(2) :: affects_beam
    integer :: i
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
    type(string_t) :: lhapdf_file
    type(sf_data_t), pointer :: sf_data
    integer :: lhapdf_member, lhapdf_photon_scheme
    real(default) :: isr_alpha, isr_q_max, isr_mass
    integer :: isr_order
    real(default) :: epa_alpha, epa_x_min, epa_q_min, epa_e_max, epa_mass
    real(default) :: ewa_x_min, ewa_q_min, ewa_pt_max, ewa_mass, ewa_sqrts    
    logical :: ewa_keep_momentum, ewa_keep_energy
    call rt_data_link (strfun_def%local, global)
    if (associated (strfun_def%options)) then
       call command_list_execute (strfun_def%options, strfun_def%local)
    end if
    if (strfun_def%type /= STRF_NONE) then
       call sf_list_append (global%sf_list, &
            strfun_def%type, affects_beam, strfun_def%n_parameters, sf_data)
       select case (strfun_def%type)
       case (STRF_LHAPDF)
          lhapdf_file = var_list_get_sval (strfun_def%local%var_list, &
               var_str ("$lhapdf_file"))  ! $
          lhapdf_member = var_list_get_ival (strfun_def%local%var_list, &
               var_str ("lhapdf_member"))
          lhapdf_photon_scheme = var_list_get_ival (strfun_def%local%var_list, &
               var_str ("lhapdf_photon_scheme"))
          call sf_data_init_lhapdf (sf_data, global%lhapdf_status, &
               global%model, global%beam_data%flv, &
               lhapdf_file, lhapdf_member, lhapdf_photon_scheme)
       case (STRF_ISR)
          isr_alpha = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("isr_alpha"))
          if (isr_alpha == 0) then
             isr_alpha = (var_list_get_rval (strfun_def%local%var_list, &
                  var_str ("ee"))) ** 2 / (4 * pi)
          end if
          isr_q_max = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("isr_q_max"))
          if (isr_q_max == 0) then
             isr_q_max = var_list_get_rval (strfun_def%local%var_list, &
                  var_str ("sqrts"))
          end if
          isr_mass = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("isr_mass"))
          isr_order = var_list_get_ival (strfun_def%local%var_list, &
               var_str ("isr_order"))
          if (isr_mass /= 0) then
             call sf_data_init_isr (sf_data, &
                  global%model, global%beam_data%flv, &
                  isr_alpha, isr_q_max, isr_mass, isr_order)
          else
             call sf_data_init_isr (sf_data, &
                  global%model, global%beam_data%flv, &
                  isr_alpha, isr_q_max, order = isr_order)
          end if
       case (STRF_EPA)
          epa_alpha = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("epa_alpha"))
          if (epa_alpha == 0) then
             epa_alpha = (var_list_get_rval (strfun_def%local%var_list, &
                  var_str ("ee"))) ** 2 / (4 * pi)
          end if
          epa_x_min = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("epa_x_min"))
          epa_q_min = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("epa_q_min"))
          epa_e_max = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("epa_e_max"))
          if (epa_e_max == 0) then
             epa_e_max = var_list_get_rval (strfun_def%local%var_list, &
                  var_str ("sqrts"))
          end if
          epa_mass = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("epa_mass"))
          if (epa_mass /= 0) then
             call sf_data_init_epa (sf_data, &
                  global%model, global%beam_data%flv, &
                  epa_alpha, epa_x_min, epa_q_min, epa_e_max, epa_mass)
          else
             call sf_data_init_epa (sf_data, &
                  global%model, global%beam_data%flv, &
                  epa_alpha, epa_x_min, epa_q_min, epa_e_max)
          end if
       case (STRF_EWA)
          call msg_warning ("EWA structure function not yet fully implemented")
          ewa_x_min = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("ewa_x_min"))
          ewa_q_min = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("ewa_q_min"))
          ewa_pt_max = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("ewa_pt_max"))
          if (ewa_pt_max == 0) then
             ewa_pt_max = var_list_get_rval (strfun_def%local%var_list, &
                  var_str ("sqrts"))
          end if
          ewa_mass = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("ewa_mass"))
          ewa_sqrts = var_list_get_rval (strfun_def%local%var_list, &
               var_str ("sqrts"))               
          ewa_keep_momentum = var_list_get_lval (strfun_def%local%var_list, &
               var_str ("?ewa_keep_momentum"))
          ewa_keep_energy = var_list_get_lval (strfun_def%local%var_list, &
               var_str ("?ewa_keep_energy"))           
          if (ewa_keep_momentum .and. ewa_keep_energy) &
             call msg_fatal (" EWA cannot violate both energy " &
                  // "and momentum conservation.") 
          if (ewa_mass /= 0) then     
             call sf_data_init_ewa (sf_data, &
                  global%model, global%beam_data%flv, &
                  ewa_x_min, ewa_q_min, ewa_pt_max, ewa_sqrts, &
                  ewa_keep_momentum, ewa_keep_energy, ewa_mass)
          else         
             call sf_data_init_ewa (sf_data, &
                  global%model, global%beam_data%flv, &
                  ewa_x_min, ewa_q_min, ewa_pt_max, ewa_sqrts, &
                  ewa_keep_momentum, ewa_keep_energy)          
          end if        
       end select
    end if
    call rt_data_restore (global, strfun_def%local)
  end subroutine strfun_def_register

  subroutine cmd_cuts_write (cuts, unit, indent)
    type(cmd_cuts_t), intent(in) :: cuts
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "cuts =" 
    call parse_node_write_rec (cuts%pn_lexpr, unit)
  end subroutine cmd_cuts_write

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

  subroutine cmd_weight_write (weight, unit, indent)
    type(cmd_weight_t), intent(in) :: weight
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "weight =" 
    call parse_node_write_rec (weight%pn_expr, unit)
  end subroutine cmd_weight_write

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

  subroutine cmd_scale_write (scale, unit, indent)
    type(cmd_scale_t), intent(in) :: scale
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "scale =" 
    call parse_node_write_rec (scale%pn_expr, unit)
  end subroutine cmd_scale_write

  subroutine cmd_scale_compile (scale, pn)
    type(cmd_scale_t), pointer :: scale
    type(parse_node_t), intent(in), target :: pn
    allocate (scale)
    scale%pn_expr => parse_node_get_sub_ptr (pn, 3)
  end subroutine cmd_scale_compile

  subroutine cmd_scale_execute (scale, global)
    type(cmd_scale_t), intent(inout), target :: scale
    type(rt_data_t), intent(inout), target :: global
    global%pn_scale_expr => scale%pn_expr
  end subroutine cmd_scale_execute

  subroutine cmd_integrate_final (integrate)
    type(cmd_integrate_t), intent(inout) :: integrate
    if (associated (integrate%options)) then
       call command_list_final (integrate%options)
       deallocate (integrate%options)
    end if
  end subroutine cmd_integrate_final

  subroutine cmd_integrate_write (integrate, unit, indent)
    type(cmd_integrate_t), intent(in) :: integrate
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "integrate ("
    do i = 1, integrate%n_proc
       if (i /= 1)  write (u, "(', ')", advance="no")
       write (u, "(A)", advance="no")  char (integrate%process_id(i))
    end do
    write (u, "(A)") ")"
    if (associated (integrate%options)) then
       write (u, "(1x,'{')")
       call command_list_write (integrate%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
  end subroutine cmd_integrate_write

  subroutine cmd_integrate_init1 (integrate, process_id, global)
    type(cmd_integrate_t), intent(out) :: integrate
    type(string_t), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    integer :: i
    integrate%n_proc = 1
    allocate (integrate%process_id (1))
    integrate%process_id(1) = process_id
  end subroutine cmd_integrate_init1

  subroutine cmd_integrate_init2 (integrate, process_id, global)
    type(cmd_integrate_t), intent(out) :: integrate
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    integer :: i
    integrate%n_proc = size (process_id)
    allocate (integrate%process_id (integrate%n_proc))
    integrate%process_id = process_id
  end subroutine cmd_integrate_init2

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
  end subroutine cmd_integrate_compile

  subroutine cmd_integrate_execute (integrate, global)
    type(cmd_integrate_t), intent(inout), target :: integrate
    type(rt_data_t), intent(inout), target :: global
    logical :: use_beams
    type(process_t), pointer :: process
    integer :: proc, n_in, n_out, pass, i, n_calls
    type(iterations_list_t), dimension(:), allocatable :: it_list
    type(iterations_spec_t) :: it_spec
    logical :: ok, use_default_iterations, rebuild_phs, phs_only, rebuild_grids
    type(grid_parameters_t) :: grid_parameters
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defaults
    type(string_t) :: phs_filename, grids_filename
    logical :: hs_active, vis_channels
    real(default) :: hs_threshold
    integer :: hs_cutoff
    real(default) :: alpha_s, sqrts
    character(32) :: md5sum_beams, md5sum_sf_list, md5sum_mappings
    character(32) :: md5sum_cuts, md5sum_weight, md5sum_scale
    integer :: current_pass, current_it, it
    logical :: adapt_final_grids, adapt_final_weights
    logical :: time_estimate
    integer :: u, u_tmp
    u = logfile_unit ()
    call rt_data_link (integrate%local, global)
    if (associated (integrate%options)) then
       call command_list_execute (integrate%options, integrate%local)
    end if
    call maybe_cmd_compile_execute (integrate%process_id, integrate%local)
    grid_parameters%threshold_calls = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("threshold_calls"))
    grid_parameters%min_calls_per_channel = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("min_calls_per_channel"))
    grid_parameters%min_calls_per_bin = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("min_calls_per_bin"))
    grid_parameters%min_bins = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("min_bins"))
    grid_parameters%max_bins = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("max_bins"))
    grid_parameters%stratified = &
         var_list_get_lval (integrate%local%var_list, &
                            var_str ("?stratified"))
    grid_parameters%use_vamp_equivalences = &
         var_list_get_lval (integrate%local%var_list, &
                            var_str ("?use_vamp_equivalences"))
    grid_parameters%channel_weights_power = &
         var_list_get_rval (integrate%local%var_list, &
                            var_str ("channel_weights_power"))
    phs_par%m_threshold_s = &
         var_list_get_rval (integrate%local%var_list, &
                            var_str ("phs_threshold_s"))
    phs_par%m_threshold_t = &
         var_list_get_rval (integrate%local%var_list, &
                            var_str ("phs_threshold_t"))
    phs_par%off_shell = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("phs_off_shell"))
    phs_par%t_channel = &
         var_list_get_ival (integrate%local%var_list, &
                            var_str ("phs_t_channel"))
    vis_channels = &
         var_list_get_lval (integrate%local%var_list, &
                            var_str ("?vis_channels"))                      
    use_beams = beam_data_are_valid (integrate%local%beam_data)
    if (use_beams) then
       if (.not. beam_data_masses_are_consistent &
                     (integrate%local%beam_data)) then
          call msg_warning &
               ("Masses of beam particle(s) differ from beam masses")
       end if
    end if

    allocate (it_list (integrate%n_proc))
    it_list = integrate%local%it_list

    LOOP_PROC: do proc = 1, integrate%n_proc
       call msg_message ("Integrating process '" // &
            char (integrate%process_id(proc)) // "'")
       call process_store_init_process (process, &
            integrate%local%prc_lib, &
            integrate%process_id(proc), integrate%local%model, &
            global%lhapdf_status, &
            integrate%local%var_list, use_beams=use_beams)
       if (.not. process_is_valid (process)) then
          call msg_fatal ("Integrating process '" &
               // char (integrate%process_id(proc)) // "': " &
               // "initialization failed, skipping")
          cycle LOOP_PROC
       else if (.not. process_has_matrix_element (process)) then
          call process_results_write_header (process, logfile=.false.)
          call process_results_write_header (process, unit=u)
          call process_do_dummy_integration (process)
          call process_results_write_footer (process, no_line=.true.)
          call process_results_write_footer (process, unit=u, no_line=.true.)
          flush (u)
          call process_record_integral (process, global%var_list)
          cycle LOOP_PROC
       end if
       sqrts = var_list_get_rval (integrate%local%var_list, var_str ("sqrts"))
       md5sum_beams = beam_data_get_md5sum (integrate%local%beam_data, sqrts)
       md5sum_sf_list = ""
       if (use_beams) then
          if (beam_data_get_n_in (integrate%local%beam_data) &
               /= process_get_n_in (process)) then
             call msg_fatal ("Process '" // char (integrate%process_id(proc)) &
                  // "': beam/process mismatch (collision/decay)", &
                  (/ var_str ("   --------------------------------------------"), &
                     var_str ("This possibly means that you tried to generate "), &
                     var_str ("a forbidden process, for which WHIZARD could not"), &
                     var_str ("find a valid phase space channel. Or there is a"), &
                     var_str ("mismatch between beams and hard interaction.") /) )
             return
          end if
          if (associated (integrate%local%sf_list)) then
             md5sum_sf_list = sf_list_get_md5sum (integrate%local%sf_list)
             call process_setup_beams (process, integrate%local%beam_data, &
                  sf_list_get_n_strfun (integrate%local%sf_list), &
                  sf_list_get_n_mapping (integrate%local%sf_list))
             call process_check_beam_setup (process, integrate%local%var_list)
             call process_setup_strfun (process, integrate%local%sf_list)
          else
             call process_setup_beams (process, integrate%local%beam_data, 0, 0)
          end if
       else
          call process_setup_beams (process, integrate%local%beam_data, 0, 0, &
               sqrts = sqrts)
       end if
       call process_connect_strfun (process, ok)
       if (.not. ok) then
          call msg_error ("Process '" // char (integrate%process_id(proc)) &
               // "': beam/structure function setup failed, skipped")
          cycle LOOP_PROC
       end if
       rebuild_phs = var_list_get_lval (integrate%local%var_list, &
            var_str ("?rebuild_phase_space"))
       phs_filename = var_list_get_sval (integrate%local%var_list, &
            var_str ("$phs_file"))   ! $ fool the noweb emacs mode
       phs_only = var_list_get_lval (integrate%local%var_list, &
            var_str ("?phs_only"))
       mapping_defaults%energy_scale = &
            var_list_get_rval (integrate%local%var_list, &
            var_str ("phs_e_scale"))
       mapping_defaults%invariant_mass_scale = &
            var_list_get_rval (integrate%local%var_list, &
            var_str ("phs_m_scale"))
       mapping_defaults%momentum_transfer_scale = &
            var_list_get_rval (integrate%local%var_list, &
            var_str ("phs_q_scale"))
       md5sum_mappings = &
            mapping_defaults_md5sum (mapping_defaults)
       if (phs_filename == "") then
          call process_setup_phase_space (process, rebuild_phs, &
               global%os_data, &
               phs_par, mapping_defaults, &
               filename_out = integrate%process_id(proc) // ".phs", &
               filename_vis = integrate%process_id(proc) // "_phs", &
               vis_channels = vis_channels, ok = ok)
       else
          call process_setup_phase_space (process, rebuild_phs, &
               global%os_data, &
               phs_par, mapping_defaults, &
               filename_in = phs_filename, &
               filename_out = integrate%process_id(proc) // ".phs", &
               filename_vis = integrate%process_id(proc) // "_phs", &
               vis_channels = vis_channels, ok = ok)
       end if       
       if (.not. ok) then
          call msg_error ("Process '" // char (integrate%process_id(proc)) &
               // "': phase space setup failed, skipped")
          cycle LOOP_PROC
       end if
       if (phs_only) then
          call msg_message ("Process '" // char (integrate%process_id(proc)) &
               // "': phase space setup complete.")
          cycle LOOP_PROC
       end if
       if (associated (integrate%local%pn_cuts_lexpr)) then
          call process_setup_cuts (process, integrate%local%pn_cuts_lexpr)
          md5sum_cuts = parse_node_get_md5sum (integrate%local%pn_cuts_lexpr)
          call msg_message ("Applying user-defined cuts.")
       else
          md5sum_cuts = ""
          call msg_warning ("No cuts have been defined.")
       end if
       if (associated (integrate%local%pn_weight_expr)) then
          call process_setup_weight (process, integrate%local%pn_weight_expr)
          md5sum_weight = parse_node_get_md5sum (integrate%local%pn_weight_expr)
          call msg_message ("Using user-defined integration weight.")
       else
          md5sum_weight = ""
       end if
       if (associated (integrate%local%pn_scale_expr)) then
          call process_setup_scale (process, integrate%local%pn_scale_expr)
          md5sum_scale = parse_node_get_md5sum (integrate%local%pn_scale_expr)
          call msg_message ("Using user-defined event scale setup.")
       else
          md5sum_scale = ""
          call msg_message ("Using partonic energy as event scale.")
       end if
       if (var_list_is_known (integrate%local%var_list, &
            var_str ("alphas"))) then
          alpha_s = var_list_get_rval (integrate%local%var_list, &
               var_str ("alphas"))
          call process_set_alpha_s (process, alpha_s)
       end if

       adapt_final_grids = var_list_get_lval (integrate%local%var_list, &
            var_str ("?adapt_final_grids"))
       adapt_final_weights = var_list_get_lval (integrate%local%var_list, &
            var_str ("?adapt_final_weights"))

       time_estimate = var_list_get_lval (integrate%local%var_list, &
            var_str ("?time_estimate"))

       n_in  = process_get_n_in  (process)
       n_out = process_get_n_out (process)
       use_default_iterations = it_list(proc)%n_pass == 0
       if (use_default_iterations) then
          select case (n_out)
          case (:1)
             call msg_error ("Integrate: number of outgoing particles " &
                  // "must be at least 2")
             cycle LOOP_PROC
          case (2:ITERATIONS_DEFAULT_LIST_SIZE)
             it_list(proc) = integrate%local%it_list_default(n_out + (n_in-2))
          case default
             it_list(proc) = &
                  integrate%local%it_list_default(ITERATIONS_DEFAULT_LIST_SIZE)
          end select
       else
          call iterations_list_complete (it_list(proc), &
               integrate%local%it_list_default(n_out))
       end if
       call iterations_list_adjust_n_calls (it_list(proc), &
            process, grid_parameters)
       call iterations_list_write (it_list(proc))
       call process_init_vamp_history &
            (process, iterations_list_get_n_it (it_list(proc)))

       if (it_list(proc)%n_pass > 0) then
          it_spec = it_list(proc)%pass(1)
          n_calls = it_spec%n_calls
          grids_filename = process_get_id (process) // ".vg"
          rebuild_grids = var_list_get_lval (integrate%local%var_list, &
               var_str ("?rebuild_grids"))
          if (.not. rebuild_grids) then
             call process_read_grid_file (process, &
                  grids_filename, &
                  md5sum_beams, md5sum_sf_list, md5sum_mappings, &
                  md5sum_cuts, md5sum_weight, md5sum_scale, &
                  grid_parameters, &
                  iterations_list_get_pass_array (it_list(proc)), &
                  iterations_list_get_n_calls_array (it_list(proc)),&
                  ok)
             rebuild_grids = .not. ok
          end if
          if (rebuild_grids) then
             call process_setup_grids &
                  (process, grid_parameters, calls=n_calls)
             write (msg_buffer, "(4(I0,A),A,L1)")  &
                  n_calls, " calls, ", &
                  process_get_n_channels (process), " channels, ", &
                  process_get_n_parameters (process), " dimensions, ", &
                  process_get_n_bins (process), " bins, ", &
                  "stratified = ", grid_parameters%stratified
             call msg_message ()
          else
             write (msg_buffer, "(3(I0,A),A,L1)")  &
                  n_calls, " calls, ", &
                  process_get_n_channels (process), " channels, ", &
                  process_get_n_parameters (process), " dimensions, ", &
                  "stratified = ", grid_parameters%stratified
             call msg_message ()
          end if
          current_pass = process_get_current_pass (process)
          current_it = process_get_current_it (process)
       end if

       hs_active = var_list_get_lval (integrate%local%var_list, &
            var_str ("?helicity_selection_active"))
       if (hs_active) then
          hs_threshold = var_list_get_rval (integrate%local%var_list, &
               var_str ("helicity_selection_threshold"))
          hs_cutoff = var_list_get_ival (integrate%local%var_list, &
               var_str ("helicity_selection_cutoff"))
       else
          hs_threshold = -1
          hs_cutoff = 1000
       end if
       call process_reset_helicity_selection &
            (process, hs_threshold, hs_cutoff)

       call process_results_write_header (process, logfile=.false.)
       call process_results_write_header (process, unit=u)
       flush (u)
       it = 0
       LOOP_PASS: do pass = 1, it_list(proc)%n_pass - 1
          it_spec = it_list(proc)%pass(pass)
          n_calls = it_spec%n_calls
          LOOP_IT: do i = 1, it_spec%n_it
             it = it + 1
             if (pass < current_pass &
                  .or. pass == current_pass .and. i <= current_it) then
                call process_results_write_entry (process, it)
                call process_results_write_entry (process, it, unit=u)
                flush (u)
             else
                call process_integrate (process, &
                     integrate%local%rng, grid_parameters, &
                     pass, 1, 1, n_calls, &
                     i==1, .true., i>2, .true., &
                     time_estimate, &
                     grids_filename, &
                     md5sum_beams, md5sum_sf_list, md5sum_mappings, &
                     md5sum_cuts, md5sum_weight, md5sum_scale)
             end if
          end do LOOP_IT
          call process_results_write_average (process, pass)
          call process_results_write_average (process, pass, unit=u)
          flush (u)
          call process_record_integral (process, global%var_list)
          call process_write_logfile (process)
       end do LOOP_PASS

       pass = it_list(proc)%n_pass
       if (pass > current_pass)  current_it = 0
       if (pass > 0 .and. pass >= current_pass) then
          it_spec = it_list(proc)%pass(pass)
          n_calls = it_spec%n_calls
          do i = 1, current_it
             it = it + 1
             call process_results_write_entry (process, it)
             call process_results_write_entry (process, it, unit=u)
             flush (u)
          end do
          call process_integrate (process, &
               integrate%local%rng, grid_parameters, &
               pass, current_it+1, it_spec%n_it, n_calls, &
               .true., adapt_final_grids, adapt_final_weights, .true., &
               time_estimate, &
               grids_filename, &
               md5sum_beams, md5sum_sf_list, md5sum_mappings, &
               md5sum_cuts, md5sum_weight, md5sum_scale)
          call process_record_integral (process, global%var_list)
          call process_write_logfile (process)
       end if
       call process_results_write_footer (process)
       call process_results_write_footer (process, unit=u)
       if (time_estimate .and. rebuild_grids)  call process_write_time_estimate (process)
       flush (u)
    end do LOOP_PROC

    call rt_data_restore (global, integrate%local)
  end subroutine cmd_integrate_execute

  subroutine maybe_cmd_compile_execute (process_id, global)
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    type(cmd_compile_t), pointer :: compile
    integer :: i
    if (associated (global%prc_lib)) then
       call process_library_update_status (global%prc_lib)
       if (.not. process_library_is_compiled (global%prc_lib)) then
          allocate (compile)
          allocate (compile%libname (1))
          compile%libname(1) = process_library_get_name (global%prc_lib)
          call cmd_compile_execute (compile, global)
          deallocate (compile)
       end if
    else
       call msg_bug ("Integrate: no process library active")
    end if
  end subroutine maybe_cmd_compile_execute

  subroutine cmd_observable_final (observable)
    type(cmd_observable_t), intent(inout) :: observable
    call eval_tree_final (observable%expr_id)
    if (associated (observable%options)) then
       call command_list_final (observable%options)
       deallocate (observable%options)
    end if
  end subroutine cmd_observable_final

  subroutine cmd_observable_write (observable, unit, indent)
    type(cmd_observable_t), intent(in) :: observable
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "observable"
    if (observable%use_id_expr) then
       call eval_tree_write (observable%expr_id, unit)
    else
       write (u, "(1x,A)", advance="no")  char (observable%id)
    end if
    write (u, *)
  end subroutine cmd_observable_write

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
       call eval_tree_init_sexpr &
            (observable%expr_id, pn_tag, observable%local%var_list)
    end select
  end subroutine cmd_observable_compile

  subroutine cmd_observable_execute (observable, global)
    type(cmd_observable_t), intent(inout), target :: observable
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: label, physical_unit, title
    type(plot_labels_t) :: plot_labels
    call rt_data_link (observable%local, global)
    if (associated (observable%options)) then
       call command_list_execute (observable%options, observable%local)
    end if
    if (observable%use_id_expr) then
       call eval_tree_evaluate (observable%expr_id)
       observable%id = eval_tree_get_string (observable%expr_id)
    end if
    label = var_list_get_sval (observable%local%var_list, var_str ("$label"))
    physical_unit = var_list_get_sval &
         (observable%local%var_list, var_str ("$physical_unit"))
    title = var_list_get_sval (observable%local%var_list, var_str ("$title")) 
    call plot_labels_init (plot_labels, title)
    call analysis_init_observable &
         (observable%id, label, physical_unit, plot_labels)
    call rt_data_restore (global, observable%local)
  end subroutine cmd_observable_execute

  subroutine cmd_histogram_final (histogram)
    type(cmd_histogram_t), intent(inout) :: histogram
    call eval_tree_final (histogram%expr_id)
    call eval_tree_final (histogram%expr_lower_bound)
    call eval_tree_final (histogram%expr_upper_bound)
    call eval_tree_final (histogram%expr_bin_width)
    if (associated (histogram%options)) then
       call command_list_final (histogram%options)
       deallocate (histogram%options)
    end if
  end subroutine cmd_histogram_final

  subroutine cmd_histogram_write (histogram, unit, indent)
    type(cmd_histogram_t), intent(in) :: histogram
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "histogram"
    if (histogram%use_id_expr) then
       call eval_tree_write (histogram%expr_id, unit)
    else
       write (u, "(1x,A)", advance="no")  char (histogram%id)
    end if
    write (u, "(1x,A)") "("
    call eval_tree_write (histogram%expr_lower_bound, unit)
    call write_indent (u, indent)
    write (u, "(1x,A)") ","
    call eval_tree_write (histogram%expr_upper_bound, unit)
    call write_indent (u, indent)
    write (u, "(1x,A)") ","
    call eval_tree_write (histogram%expr_bin_width, unit)
    call write_indent (u, indent)
    write (u, "(1x,A)") ")"
  end subroutine cmd_histogram_write

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
       call eval_tree_init_sexpr &
            (histogram%expr_id, pn_tag, histogram%local%var_list)
    end select
    call eval_tree_init_expr &
         (histogram%expr_lower_bound, pn_arg1, histogram%local%var_list)
    call eval_tree_init_expr &
         (histogram%expr_upper_bound, pn_arg2, histogram%local%var_list)
    if (associated (pn_arg3)) then
      call eval_tree_init_expr &
           (histogram%expr_bin_width, pn_arg3, histogram%local%var_list)
    end if
  end subroutine cmd_histogram_compile

  subroutine cmd_histogram_execute (histogram, global)
    type(cmd_histogram_t), intent(inout), target :: histogram
    type(rt_data_t), intent(inout), target :: global
    real(default) :: lower_bound, upper_bound, bin_width
    integer :: bin_number
    logical :: bounds_are_known, bin_width_is_used
    type(string_t) :: label, physical_unit
    type(string_t) :: title, description, xlabel, ylabel
    logical :: x_log, y_log
    real(default) :: y_min, y_max
    type(plot_labels_t) :: plot_labels
    call rt_data_link (histogram%local, global)
    if (associated (histogram%options)) then
       call command_list_execute (histogram%options, histogram%local)
    end if
    if (histogram%use_id_expr) then
       call eval_tree_evaluate (histogram%expr_id)
       histogram%id = eval_tree_get_string (histogram%expr_id)
    end if
    bounds_are_known = .true.
    call eval_tree_evaluate (histogram%expr_lower_bound)
    call eval_tree_evaluate (histogram%expr_upper_bound)
    call eval_tree_evaluate (histogram%expr_bin_width)
    if (eval_tree_result_is_known (histogram%expr_lower_bound)) then
       lower_bound = eval_tree_get_real (histogram%expr_lower_bound)
    else
       call msg_error ("Histogram '" &
            // char (histogram%id) // "': lower bound is undefined")
       bounds_are_known = .false.
    end if
    if (eval_tree_result_is_known (histogram%expr_upper_bound)) then
       upper_bound = eval_tree_get_real (histogram%expr_upper_bound)
    else
       call msg_error ("Histogram '" &
            // char (histogram%id) // "': upper bound is undefined")
       bounds_are_known = .false.
    end if
    if (eval_tree_result_is_known (histogram%expr_bin_width)) then
       bin_width = eval_tree_get_real (histogram%expr_bin_width)
       bin_width_is_used = .true.
    else if (var_list_is_known &
         (histogram%local%var_list, var_str ("n_bins"))) then
       bin_number = var_list_get_ival &
            (histogram%local%var_list, var_str ("n_bins"))
       bin_width_is_used = .false.
    else
       call msg_error ("Histogram '" &
            // char (histogram%id) // "': neither bin width nor number is defined")
       bounds_are_known = .false.
    end if
    label = var_list_get_sval (histogram%local%var_list, var_str ("$label"))
    physical_unit = var_list_get_sval &
         (histogram%local%var_list, var_str ("$physical_unit"))
    title = var_list_get_sval (histogram%local%var_list, var_str ("$title"))
    description = &
         var_list_get_sval (histogram%local%var_list, var_str ("$description"))
    xlabel = var_list_get_sval (histogram%local%var_list, var_str ("$xlabel"))
    ylabel = var_list_get_sval (histogram%local%var_list, var_str ("$ylabel"))
    x_log = var_list_get_lval (histogram%local%var_list, var_str ("?x_log"))
    y_log = var_list_get_lval (histogram%local%var_list, var_str ("?y_log"))    
    call plot_labels_init (plot_labels, title, &
         description, xlabel, ylabel)
    if (bounds_are_known) then
       if (bin_width_is_used) then
         if (var_list_is_known &
            (histogram%local%var_list, var_str ("y_min"))) then
            y_min = var_list_get_rval (histogram%local%var_list, &
                               var_str ("y_min"))
            if (var_list_is_known &
               (histogram%local%var_list, var_str ("y_max"))) then               
               y_max = var_list_get_rval (histogram%local%var_list, &
                               var_str ("y_max"))
               if (y_max <= y_min) then
                  call msg_fatal ("Please choose y_min smaller as y_max.")               
               else
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_width, label, physical_unit, &
                  plot_labels, x_log, y_log, y_min, y_max)
               end if   
            else
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_width, label, physical_unit, &
                  plot_labels, x_log, y_log, y_min)
            end if
         else 
            if (var_list_is_known &
               (histogram%local%var_list, var_str ("y_max"))) then   
               y_max = var_list_get_rval (histogram%local%var_list, &
                            var_str ("y_max"))
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_width, label, physical_unit, &
                  plot_labels, x_log, y_log, y_max)                
            else    
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_width, label, physical_unit, &
                  plot_labels, x_log, y_log)                                   
            end if
         end if       
       else    
          if (var_list_is_known &
            (histogram%local%var_list, var_str ("y_min"))) then
            y_min = var_list_get_rval (histogram%local%var_list, &
                               var_str ("y_min"))
            if (var_list_is_known &
               (histogram%local%var_list, var_str ("y_max"))) then               
               y_max = var_list_get_rval (histogram%local%var_list, &
                               var_str ("y_max"))
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_number, label, physical_unit, &
                  plot_labels, x_log, y_log, y_min, y_max)
            else
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_number, label, physical_unit, &
                  plot_labels, x_log, y_log, y_min)
            end if
         else 
            if (var_list_is_known &
               (histogram%local%var_list, var_str ("y_max"))) then   
               y_max = var_list_get_rval (histogram%local%var_list, &
                            var_str ("y_max"))
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_number, label, physical_unit, &
                  plot_labels, x_log, y_log, y_max)                
            else    
               call analysis_init_histogram &
                 (histogram%id, lower_bound, upper_bound, &
                  bin_number, label, physical_unit, &
                  plot_labels, x_log, y_log)                                   
            end if
         end if       
       end if         
    else
       call msg_error ("Histogram '" &
            // char (histogram%id) // "': invalid declaration, skipping")
    end if
    call rt_data_restore (global, histogram%local)
  end subroutine cmd_histogram_execute

  subroutine cmd_plot_final (plot)
    type(cmd_plot_t), intent(inout) :: plot
    call eval_tree_final (plot%expr_id)
    call eval_tree_final (plot%expr_lower_bound)
    call eval_tree_final (plot%expr_upper_bound)
    if (associated (plot%options)) then
       call command_list_final (plot%options)
       deallocate (plot%options)
    end if
  end subroutine cmd_plot_final

  subroutine cmd_plot_write (plot, unit, indent)
    type(cmd_plot_t), intent(in) :: plot
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "plot"
    if (plot%use_id_expr) then
       call eval_tree_write (plot%expr_id, unit)
    else
       write (u, "(1x,A)", advance="no")  char (plot%id)
    end if
    write (u, "(1x,A)") "("
    call eval_tree_write (plot%expr_lower_bound, unit)
    call write_indent (u, indent)
    write (u, "(1x,A)") ","
    call eval_tree_write (plot%expr_upper_bound, unit)
    call write_indent (u, indent)
    write (u, "(1x,A)") ")"
  end subroutine cmd_plot_write

  subroutine cmd_plot_compile (plot, pn, global)
    type(cmd_plot_t), pointer :: plot
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_tag, pn_args, pn_arg1, pn_arg2
    type(parse_node_t), pointer :: pn_opt
    pn_tag => parse_node_get_sub_ptr (pn, 2)
    pn_args => parse_node_get_next_ptr (pn_tag)
    pn_arg1 => parse_node_get_sub_ptr (pn_args)
    pn_arg2 => parse_node_get_next_ptr (pn_arg1)
    if (associated (pn_args)) then
       pn_opt => parse_node_get_next_ptr (pn_args)
    else
       pn_opt => null ()
    end if       
    allocate (plot)
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
       call eval_tree_init_sexpr (plot%expr_id, pn_tag, plot%local%var_list)
    end select
    call eval_tree_init_expr &
         (plot%expr_lower_bound, pn_arg1, plot%local%var_list)
    call eval_tree_init_expr &
         (plot%expr_upper_bound, pn_arg2, plot%local%var_list)
  end subroutine cmd_plot_compile

  subroutine cmd_plot_execute (plot, global)
    type(cmd_plot_t), intent(inout), target :: plot
    type(rt_data_t), intent(inout), target :: global
    real(default) :: lower_bound, upper_bound
    logical :: bounds_are_known
    type(string_t) :: title, description, xlabel, ylabel
    logical :: x_log, y_log
    real(default) :: y_min, y_max
    type(plot_labels_t) :: plot_labels
    call rt_data_link (plot%local, global)
    if (associated (plot%options)) then
       call command_list_execute (plot%options, plot%local)
    end if
    if (plot%use_id_expr) then
       call eval_tree_evaluate (plot%expr_id)
       plot%id = eval_tree_get_string (plot%expr_id)
    end if
    bounds_are_known = .true.
    call eval_tree_evaluate (plot%expr_lower_bound)
    call eval_tree_evaluate (plot%expr_upper_bound)
    if (eval_tree_result_is_known (plot%expr_lower_bound)) then
       lower_bound = eval_tree_get_real (plot%expr_lower_bound)
    else
       call msg_error ("Plot '" &
            // char (plot%id) // "': lower bound is undefined")
       bounds_are_known = .false.
    end if
    if (eval_tree_result_is_known (plot%expr_upper_bound)) then
       upper_bound = eval_tree_get_real (plot%expr_upper_bound)
    else
       call msg_error ("Plot '" &
            // char (plot%id) // "': upper bound is undefined")
       bounds_are_known = .false.
    end if
    title = var_list_get_sval (plot%local%var_list, var_str ("$title"))
    description = &
         var_list_get_sval (plot%local%var_list, var_str ("$description"))
    xlabel = var_list_get_sval (plot%local%var_list, var_str ("$xlabel"))
    ylabel = var_list_get_sval (plot%local%var_list, var_str ("$ylabel"))
    x_log = var_list_get_lval (plot%local%var_list, var_str ("?x_log"))    
    y_log = var_list_get_lval (plot%local%var_list, var_str ("?y_log"))        
    call plot_labels_init (plot_labels, title, &
         description, xlabel, ylabel)
    if (bounds_are_known) then
         if (var_list_is_known &
            (plot%local%var_list, var_str ("y_min"))) then
            y_min = var_list_get_rval (plot%local%var_list, &
                               var_str ("y_min"))
            if (var_list_is_known &
               (plot%local%var_list, var_str ("y_max"))) then               
               y_max = var_list_get_rval (plot%local%var_list, &
                               var_str ("y_max"))
               if (y_max <= y_min) then
                  call msg_fatal ("Please choose y_min smaller as y_max.")
               else
                  call analysis_init_plot &
                    (plot%id, lower_bound, upper_bound, &
                     plot_labels, x_log, y_log, y_min, y_max)
               end if      
            else
               call analysis_init_plot &
                 (plot%id, lower_bound, upper_bound, &
                  plot_labels, x_log, y_log, y_min)
            end if
         else 
            if (var_list_is_known &
               (plot%local%var_list, var_str ("y_max"))) then   
               y_max = var_list_get_rval (plot%local%var_list, &
                            var_str ("y_max"))
               call analysis_init_plot &
                 (plot%id, lower_bound, upper_bound, &
                  plot_labels, x_log, y_log, y_max)                
            else    
               call analysis_init_plot &
                 (plot%id, lower_bound, upper_bound, &
                  plot_labels, x_log, y_log)                                   
            end if    
         end if   
    else
       call msg_error ("Plot '" &
            // char (plot%id) // "': invalid declaration, skipping")
    end if
    call rt_data_restore (global, plot%local)
  end subroutine cmd_plot_execute

  subroutine cmd_analysis_write (analysis, unit, indent)
    type(cmd_analysis_t), intent(in) :: analysis
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "analysis =" 
    call parse_node_write_rec (analysis%pn_lexpr, unit)
  end subroutine cmd_analysis_write

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

  subroutine cmd_write_analysis_final (write)
    type(cmd_write_analysis_t), intent(inout) :: write
    integer :: i
    do i = 1, write%n_args
       call eval_tree_final (write%expr_id(i))
    end do
  end subroutine cmd_write_analysis_final

  subroutine cmd_write_analysis_compile (write, pn, global)
    type(cmd_write_analysis_t), pointer :: write
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_args, pn_tag
    integer :: i
    pn_args => parse_node_get_sub_ptr (pn, 2)
    allocate (write)
    if (associated (pn_args)) then
       write%n_args = parse_node_get_n_sub (pn_args)
       allocate (write%id (write%n_args), write%use_id_expr (write%n_args))
       write%use_id_expr = .false.
       allocate (write%expr_id (write%n_args))
       pn_tag => parse_node_get_sub_ptr (pn_args)
       i = 1
       do while (associated (pn_tag))
          select case (char (parse_node_get_rule_key (pn_tag)))
          case ("analysis_id")
             write%id(i) = parse_node_get_string (pn_tag)
          case default
             write%use_id_expr(i) = .true.
             call eval_tree_init_sexpr &
                  (write%expr_id(i), pn_tag, global%var_list)
          end select
          i = i + 1
          pn_tag => parse_node_get_next_ptr (pn_tag)
       end do
    end if
  end subroutine cmd_write_analysis_compile

  subroutine cmd_write_analysis_execute (write, global)
    type(cmd_write_analysis_t), intent(inout), target :: write
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: filename, data_file, driver_file
    integer :: i, u_data, u_driver, u_log
    logical :: has_gmlcode
    u_log = logfile_unit ()
    if (var_list_is_known (global%var_list, &
                           var_str ("$analysis_filename"))) then
       filename = var_list_get_sval (global%var_list, &
                                     var_str ("$analysis_filename"))
       data_file = filename // ".dat"
       driver_file = filename // ".tex"
       call msg_message ("Writing analysis results to '" &
            // char (data_file) // "'")
       u_data = free_unit ()
       open (unit=u_data, file=char(data_file), &
             action="write", status="replace")
       call msg_message ("Writing analysis results display to '" &
            // char (driver_file) // "'")
       u_driver = free_unit ()
       open (unit=u_driver, file=char(driver_file), &
             action="write", status="replace")
    else
       u_data = -1
       u_driver = -1
    end if
    if (write%n_args == 0) then
       if (u_data >= 0) then
          call analysis_write (unit=u_data)
       else
          call analysis_write ()
          call analysis_write (unit=u_log)
          flush (u_log)
       end if
       if (u_driver >= 0) then
          call analysis_write_driver (data_file, unit=u_driver)
       end if
    else
       do i = 1, write%n_args
          if (write%use_id_expr(i)) then
             call eval_tree_evaluate (write%expr_id(i))
             write%id(i) = eval_tree_get_string (write%expr_id(i))
          end if
          if (u_data >= 0) then
             call analysis_write (write%id(i), unit=u_data)
          else
             call analysis_write (write%id(i))
             call analysis_write (write%id(i), unit=u_log)
             flush (u_log)
          end if
          if (u_driver >= 0) then
             call analysis_write_driver (data_file, write%id, unit=u_driver)
          end if
       end do
    end if
    if (u_data >= 0)  close (u_data)
    if (u_driver >= 0) then
       close (u_driver)
       if (write%n_args == 0) then
          has_gmlcode = analysis_has_plots ()
       else
          has_gmlcode = analysis_has_plots (write%id)
       end if
       call msg_message ("Compiling analysis results display in '" &
            // char (driver_file) // "'")
       call analysis_compile_tex (filename, has_gmlcode, global%os_data)
    end if
  end subroutine cmd_write_analysis_execute

  subroutine cmd_clear_final (clear)
    type(cmd_clear_t), intent(inout) :: clear
    integer :: i
    do i = 1, clear%n_args
       call eval_tree_final (clear%expr_id(i))
    end do
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
       allocate (clear%expr_id (clear%n_args))
       pn_tag => parse_node_get_sub_ptr (pn_args)
       i = 1
       do while (associated (pn_tag))
          key = parse_node_get_rule_key (pn_tag)
          select case (char (key))
          case ("iterations", "cuts", "weight", "scale", "analysis", "expect")
             clear%id(i) = key
          case ("analysis_id")
             clear%id(i) = parse_node_get_string (pn_tag)
          case default
             clear%use_id_expr(i) = .true.
             call eval_tree_init_sexpr &
                  (clear%expr_id(i), pn_tag, global%var_list)
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
             call msg_message ("Cleared event scale setup")
          case ("analysis")
             global%pn_analysis_lexpr => null ()
             call msg_message ("Cleared analysis setup")
          case ("expect")
             call expect_clear ()
             call msg_message ("Cleared counters of value checks")
          case default
             if (clear%use_id_expr(i)) then
                call eval_tree_evaluate (clear%expr_id(i))
                clear%id(i) = eval_tree_get_string (clear%expr_id(i))
             end if
             call analysis_clear (clear%id(i))
             call msg_message ("Cleared analysis object '" &
                  // char (clear%id(i)) // "'")
          end select
       end do
    end if
  end subroutine cmd_clear_execute

  subroutine cmd_record_final (record)
    type(cmd_record_t), intent(inout) :: record
    call eval_tree_final (record%lexpr)
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
    call eval_tree_init_lexpr (record%lexpr, pn_lexpr, global%var_list)
  end subroutine cmd_record_compile

  subroutine cmd_record_execute (record, global)
    type(cmd_record_t), intent(inout), target :: record
    type(rt_data_t), intent(inout), target :: global
    call eval_tree_evaluate (record%lexpr)    
  end subroutine cmd_record_execute

  subroutine decay_properties_final (decay)
    type(decay_properties_t), intent(inout) :: decay
    call eval_tree_final (decay%pdg)
  end subroutine decay_properties_final

  subroutine decay_properties_write (decay, unit, indent)
    type(decay_properties_t), intent(in) :: decay
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    if (decay%n_proc /= 0) then
       write (u, "(1x,A)", advance="no") "unstable"
    else
       write (u, "(1x,A)", advance="no") "stable"
    end if
    write (u, "(1x,A)", advance="no")  char (decay%prt)
    if (decay%n_proc /= 0) then
       write (u, "(1x,A)", advance="no") "("
       do i = 1, decay%n_proc
          if (i /= 1)  write (u, "(', ')", advance="no")
          write (u, "(A)", advance="no")  char (decay%process_id(i))
       end do
       write (u, "(A)") ")"
    end if
  end subroutine decay_properties_write

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

  subroutine cmd_unstable_write (unstable, unit, indent)
    type(cmd_unstable_t), intent(in) :: unstable
    integer, intent(in), optional :: unit, indent
    integer :: d
    do d = 1, size (unstable%decay)
       call decay_properties_write (unstable%decay(d), unit, indent)
    end do
  end subroutine cmd_unstable_write

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
    if (associated (pn_opt)) then
       allocate (unstable%options)
       call command_list_compile (unstable%options, pn_opt, unstable%local)
    end if
    call rt_data_local_init (unstable%local, global)
    allocate (unstable%decay (parse_node_get_n_sub (pn_list)))
    d = 0
    pn_decl => parse_node_get_sub_ptr (pn_list)
    do while (associated (pn_decl))
       d = d + 1
       pn_prt => parse_node_get_sub_ptr (pn_decl)
       pn_arg => parse_node_get_next_ptr (pn_prt)
       call eval_tree_init_cexpr &
            (unstable%decay(d)%pdg, pn_prt, unstable%local%var_list)
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
  end subroutine cmd_unstable_compile

  subroutine cmd_unstable_execute (unstable, global)
    type(cmd_unstable_t), intent(inout), target :: unstable
    type(rt_data_t), intent(inout), target :: global
    type(pdg_array_t) :: aval
    type(flavor_t), dimension(:), allocatable :: flv_tmp
    type(flavor_t), dimension(:), allocatable :: flv
    type(cmd_integrate_t) :: integrate
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
       call eval_tree_evaluate (unstable%decay(d)%pdg)
       aval = eval_tree_get_pdg_array (unstable%decay(d)%pdg)
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
       else
          call msg_fatal ("Particle '" // char (unstable%decay(d)%prt) &
               // "' is not contained in model '" &
               // char (model_get_name (unstable%local%model)) // "'")
       end if
    end do
    do d = 1, size (unstable%decay)
    end do
    do d = 1, size (unstable%decay)
       unstable%decay(d)%prt = flavor_get_name (flv(d))
       allocate (integral (unstable%decay(d)%n_proc))
       LOOP_PROC: do proc = 1, unstable%decay(d)%n_proc
          process_id = unstable%decay(d)%process_id(proc)
          process => process_store_get_process_ptr (process_id)
          if (.not. associated (process)) then
             call msg_message ("Missing integral for decay channel: " &
                  // char (process_id))
! This version triggers a bug BOTH in nagfor and gfortran, apparently
!              call cmd_integrate_init (integrate, (/ process_id /), global)
! This is ok:
             call cmd_integrate_init (integrate, process_id, global)
!
             call cmd_integrate_execute (integrate, global)
             call cmd_integrate_final (integrate)
             call msg_message ("Integration for missing channel complete.")
             process => process_store_get_process_ptr (process_id)
          end if
          if (associated (process)) then
             integral(proc) = process_get_integral (process)
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

  subroutine cmd_stable_write (stable, unit, indent)
    type(cmd_stable_t), intent(in) :: stable
    integer, intent(in), optional :: unit, indent
    integer :: d
    do d = 1, size (stable%decay)
       call decay_properties_write (stable%decay(d), unit, indent)
    end do
  end subroutine cmd_stable_write

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
    allocate (stable%decay (parse_node_get_n_sub (pn_list)))
    d = 0
    pn_prt => parse_node_get_sub_ptr (pn_list)
    do while (associated (pn_prt))
       d = d + 1
       call eval_tree_init_cexpr &
          (stable%decay(d)%pdg, pn_prt, stable%local%var_list)
       stable%decay(d)%prt = "?"
       pn_prt => parse_node_get_next_ptr (pn_prt)
    end do
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
       call eval_tree_evaluate (stable%decay(d)%pdg)
       aval = eval_tree_get_pdg_array (stable%decay(d)%pdg)
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

  subroutine cmd_sample_format_final (sample)
    type(cmd_sample_format_t), intent(inout) :: sample
    if (allocated (sample%format)) then
       deallocate (sample%format)
    end if
  end subroutine cmd_sample_format_final

  subroutine cmd_sample_format_write (sample, unit, indent)
    type(cmd_sample_format_t), intent(in) :: sample
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no")  "sample ="
    if (allocated (sample%format)) then
       do i = 1, size (sample%format)
          write (u, "(1x,A)", advance="no")  char (sample%format(i))
       end do
       write (u, *)
    else
       write (u, "(1x,A)")  "[undefined]"
    end if
  end subroutine cmd_sample_format_write

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

  recursive subroutine simulation_parameters_init &
      (sim, unweighted, event_normalization, negative_weights)
    type(simulation_parameters_t), intent(out) :: sim
    logical, intent(in) :: unweighted
    type(string_t), intent(in) :: event_normalization
    logical, intent(in) :: negative_weights
    sim%unweighted = unweighted
    select case (char (event_normalization))
    case ("auto", "Auto", "AUTO", "automatic", "Automatic", "AUTOMATIC")
       if (unweighted) then
          sim%normalization_mode = NORM_UNIT
       else
          sim%normalization_mode = NORM_SIGMA
       end if
    case ("1", "unity", "Unity", "UNITY")
       sim%normalization_mode = NORM_UNIT
    case ("1/n", "1/N")
       sim%normalization_mode = NORM_N_EVT
    case ("sigma", "Sigma", "SIGMA")
       sim%normalization_mode = NORM_SIGMA
    case ("sigma/n", "Sigma/n", "Sigma/N", "SIGMA/N")
       sim%normalization_mode = NORM_SIGMA_N_EVT
    case default
       call msg_error ("Unknown value '" // char (event_normalization) &
            // "for $event_normalization.  I'll assume 'auto'")
       call simulation_parameters_init &
            (sim, unweighted, var_str ("auto"), negative_weights)
    end select
  end subroutine simulation_parameters_init

  subroutine simulation_parameters_write_message (sim, unit)
    type(simulation_parameters_t), intent(in) :: sim
    integer, intent(in), optional :: unit
    type(string_t) :: weight_str, norm_str, neg_str
    if (sim%unweighted) then
       weight_str = "unweighted"
    else
       weight_str = "weighted"
    end if     
    select case (sim%normalization_mode)
    case (NORM_UNIT)
       norm_str = "1"
    case (NORM_N_EVT)
       norm_str = "1/n"
    case (NORM_SIGMA)
       norm_str = "sigma"
    case (NORM_SIGMA_N_EVT)
       norm_str = "sigma/n"
    case default
       norm_str = "unknown"
    end select
    if (sim%negative_weights) then
       neg_str = ", allow negative weights"
    else
       neg_str = ""
    end if
    call msg_message ("Simulation mode = " // char (weight_str) &
         // ", event_normalization = '" // char (norm_str) &
         // "'" // char (neg_str), &
         unit)
  end subroutine simulation_parameters_write_message

  subroutine simulation_parameters_write (sim, unit)
    type(simulation_parameters_t), intent(in) :: sim
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *) "Simulation parameters:"
    write (u, *) "  unweighted         = ", sim%unweighted
    write (u, *) "  normalization_mode = ", sim%normalization_mode
    write (u, *) "  negative_weights   = ", sim%negative_weights
  end subroutine simulation_parameters_write

  function simulation_parameters_get_norm (sim, sigma, n) result (norm)
    real(default) :: norm
    type(simulation_parameters_t), intent(in) :: sim
    real(default), intent(in) :: sigma
    integer, intent(in) :: n
    select case (sim%normalization_mode)
    case (NORM_UNIT)
       norm = 1
    case (NORM_N_EVT)
       if (n /= 0) then
          norm = 1._default / n
       else
          norm = 1
       end if
    case (NORM_SIGMA)
       norm = sigma
    case (NORM_SIGMA_N_EVT)
       if (n /= 0) then
          norm = sigma / n
       else
          norm = sigma
       end if
    case default
       norm = 1
    end select
    if ((.not. sim%unweighted) .and. sigma /= 0)  norm = norm / sigma
  end function simulation_parameters_get_norm

  function simulation_parameters_get_md5sum (sim) result (md5sum_sim)
    character(32) :: md5sum_sim
    type(simulation_parameters_t), intent(in) :: sim
    integer :: u
    u = free_unit ()
    open (u, status = "scratch")
    call simulation_parameters_write (sim, u)
    rewind (u)
    md5sum_sim = md5sum (u)
    close (u)
  end function simulation_parameters_get_md5sum

  subroutine checkpointing_init (checkpointing, var_list)
    type(checkpointing_t), intent(out) :: checkpointing
    type(var_list_t), intent(in) :: var_list
    checkpointing%active = var_list_is_known (var_list, var_str ("checkpoint"))
    if (checkpointing%active) then
       checkpointing%val = &
       var_list_get_ival (var_list, var_str("checkpoint"))
       if (checkpointing%val <= 0) then
          call msg_warning ("ignoring nonpositive value of 'checkpoint'")
          checkpointing%active = .false.
       end if
    end if
  end subroutine checkpointing_init

  subroutine checkpointing_msg_start (checkpointing, n_events, i_evt)
    type(checkpointing_t), intent(inout) :: checkpointing
    integer, intent(in) :: n_events, i_evt
    if (checkpointing%active .and. n_events > i_evt) then
       call msg_message ("")
       call msg_message (checkpoint_bar)
       call msg_message (checkpoint_head)
       call msg_message (checkpoint_bar)
       write (msg_buffer, checkpoint_fmt) 0., 0, n_events - i_evt, "???"
       call msg_message ()
       checkpointing%running = .true.
       checkpointing%tzero = time_current ()
    end if
  end subroutine checkpointing_msg_start

  subroutine checkpointing_msg_event (checkpointing, n_events, n_read, i_evt)
    type(checkpointing_t), intent(in) :: checkpointing
    integer, intent(in) :: n_events, n_read, i_evt
    real(default) :: tcurrent
    type(string_t) :: tremain
    if (checkpointing%active .and. checkpointing%running &
          .and. mod (i_evt, checkpointing%val) == 0) then
       tcurrent = time_current ()
       tremain = time2string ( &
          int ((tcurrent - checkpointing%tzero) / (i_evt - n_read) &
             * (n_events - i_evt)))
       write (msg_buffer, checkpoint_fmt) &
             100 * (i_evt - n_read) / real (n_events - n_read), &
             i_evt - n_read, &
             n_events - i_evt, char (tremain)
       call msg_message ()
    end if
  end subroutine checkpointing_msg_event

  subroutine checkpointing_msg_end (checkpointing, n_read, i_evt)
    type(checkpointing_t), intent(inout) :: checkpointing
    integer, intent(in) :: n_read, i_evt
    if (checkpointing%active .and. checkpointing%running) then
       if (mod (i_evt, checkpointing%val) /= 0) then
          write (msg_buffer, checkpoint_fmt) 100., i_evt - n_read, 0, "0s"
          call msg_message ()
       end if
       call msg_message (checkpoint_bar)
       call msg_message ("")
       checkpointing%running = .false.
    end if
  end subroutine checkpointing_msg_end

  subroutine simulation_basic_init (sim, process_id, var_list, verbose)
    type(simulation_t), intent(out) :: sim
    type(string_t), dimension(:), intent(in) :: process_id
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: verbose
    type(string_t) :: process_string
    integer :: proc
    sim%n_proc = size (process_id)
    allocate (sim%process_id (sim%n_proc))
    sim%process_id = process_id
    allocate (sim%prc_array (sim%n_proc))
    do proc = 1, sim%n_proc
       sim%prc_array(proc)%ptr => &
            process_store_get_process_ptr (sim%process_id(proc))
    end do
    if (present (verbose)) then
       if (verbose) then
          process_string = ""
          do proc = 1, size (process_id)
             if (proc > 1)  process_string = process_string // ", "
             process_string = process_string // sim%process_id (proc)
          end do
          call msg_message ("Initializating simulation for processes " &
            // char (process_string) // ":")
       end if
    end if
    sim%rebuild_events = &
         var_list_get_lval (var_list, var_str ("?rebuild_events"))
    call simulation_parameters_init (sim%spar, &
         var_list_get_lval &
              (var_list, var_str ("?unweighted")), &
         var_list_get_sval &
              (var_list, var_str ("$event_normalization")), &
         var_list_get_lval &
              (var_list, var_str ("?negative_weights")))
    if (present (verbose)) then
       if (verbose)  call simulation_parameters_write_message (sim%spar)
    end if
    call var_list_init_snapshot (sim%var_list, var_list)
  end subroutine simulation_basic_init

  subroutine simulation_compute_missing_integrals (sim, global, verbose)
    type(simulation_t), intent(inout) :: sim
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: verbose
    integer :: n_missing
    type(string_t), dimension(:), allocatable :: process_id
    type(cmd_integrate_t), target :: integrate
    logical, dimension(:), allocatable :: missing
    type(string_t) :: prc_string
    integer :: proc
    logical :: verb
    verb = .false.;  if (present (verbose))  verb = verbose
    allocate (missing (sim%n_proc))
    do proc = 1, sim%n_proc
       missing(proc) = .not. associated (sim%prc_array(proc)%ptr)
    end do
    n_missing = count (missing)
    if (n_missing > 0) then
       allocate (process_id (n_missing))
       process_id = pack (sim%process_id, missing)
       if (verb) then
          prc_string = process_id(1)
          do proc = 2, n_missing
             prc_string = prc_string // ", " // process_id(proc)
          end do
          call msg_message ("Integrating missing processes: " &
               // char (prc_string))
       end if
       call cmd_integrate_init (integrate, process_id, global)
       call cmd_integrate_execute (integrate, global)
       call cmd_integrate_final (integrate)
       if (verb) then
          call msg_message ("Integration of missing processes complete, " &
               // "resuming simulation.")
       end if
    end if
    do proc = 1, sim%n_proc
       if (missing(proc)) sim%prc_array(proc)%ptr => &
            process_store_get_process_ptr (sim%process_id(proc))
    end do
  end subroutine simulation_compute_missing_integrals

  subroutine simulation_check (sim, ok)
    type(simulation_t), intent(inout) :: sim
    logical, intent(out) :: ok
    type(process_t), pointer :: process
    integer :: proc
    type(flavor_t), dimension(:), allocatable :: beam_flv
    real(default), dimension(:), allocatable :: beam_energy
    ok = .false.
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       if (.not. associated (process)) then
          call msg_fatal ("Process '" // char (sim%process_id(proc)) &
               // "' is not available for simulation.")
          return
       end if
       select case (proc)
       case (1)
          sim%n_in = process_get_n_in (process)
          allocate (beam_flv (sim%n_in), beam_energy (sim%n_in))
          beam_flv = process_get_beam_flv (process)
          beam_energy = process_get_beam_energy (process)
       case default
          if (.not. process_has_matrix_element (process))  cycle
          if (process_get_n_in (process) /= sim%n_in) then
             call msg_fatal ("Simulation: " &
                  // "Mixture of scattering and decays")
             return
          else if (any (process_get_beam_flv (process) /= beam_flv)) then
             call msg_fatal ("Simulation: Mismatch in beam particles")
             return
          else if (any (process_get_beam_energy (process) &
                        /= beam_energy))then
             call msg_fatal ("Simulation: Mismatch in beam energies")
             return
          end if
       end select
    end do
    allocate (sim%beam_flv (sim%n_in), sim%beam_energy (sim%n_in))
    sim%beam_flv = beam_flv
    sim%beam_energy = beam_energy
    ok = .true.
  end subroutine simulation_check

  subroutine simulation_setup_file_list (sim, event_fmt, basename_default)
    type(simulation_t), intent(inout) :: sim
    integer, dimension(:), intent(in), allocatable :: event_fmt
    type(string_t), intent(in) :: basename_default
    type(string_t) :: basename, extension_raw
    integer :: i
    sim%basename = var_list_get_sval (sim%var_list, var_str ("$sample"))
    if (basename == "")  sim%basename = basename_default
    sim%read_raw = var_list_get_lval (sim%var_list, var_str ("?read_raw")) &
        .and. .not. sim%rebuild_events
    sim%write_raw = var_list_get_lval (sim%var_list, var_str ("?write_raw"))
    extension_raw = var_list_get_sval (sim%var_list, var_str ("$extension_raw"))
    sim%file_raw = sim%basename // "." // extension_raw
    if (allocated (event_fmt)) then
       do i = 1, size (event_fmt)
          call file_list_append_file_spec (sim%file_list, &
               sim%basename, sim%var_list, event_fmt(i), &
               sim%beam_flv, sim%beam_energy)
       end do
    end if
  end subroutine simulation_setup_file_list

  subroutine simulation_collect_integrals (sim, ok)
    type(simulation_t), intent(inout) :: sim
    logical, intent(out) :: ok
    integer :: proc
    type(process_t), pointer :: process
    allocate (sim%integral (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       sim%integral(proc) = process_get_integral (process)
    end do
    sim%integral_sum = sum (sim%integral)
    if (sim%integral_sum > 0) then
       ok = .true.
    else
       call msg_error ("Simulation: " &
            // "sum of process integrals must be positive; skipping")
       ok = .false.
    end if
  end subroutine simulation_collect_integrals

  subroutine simulation_collect_md5sums (sim)
    type(simulation_t), intent(inout) :: sim
    integer :: proc
    type(process_t), pointer :: process
    allocate (sim%md5sum%process (sim%n_proc))
    allocate (sim%md5sum%parameters (sim%n_proc))
    allocate (sim%md5sum%results (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       sim%md5sum%process(proc) = process_get_md5sum (process)
       sim%md5sum%parameters(proc) = process_get_md5sum_parameters (process)
       sim%md5sum%results(proc) = process_get_md5sum_results (process)
    end do
    sim%md5sum%decays = decay_store_get_md5sum ()
    sim%md5sum%simulation = simulation_parameters_get_md5sum (sim%spar)
  end subroutine simulation_collect_md5sums

  subroutine simulation_setup_n_events (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    integer :: n_events
    real(default) :: luminosity
    n_events = var_list_get_ival (sim%var_list, var_str ("n_events"))
    luminosity = var_list_get_rval (sim%var_list, var_str ("luminosity"))
    sim%n_events = max (nint (luminosity * sim%integral_sum), n_events)
    sim%luminosity = max (luminosity, sim%n_events / sim%integral_sum)
    sim%norm_weight = simulation_parameters_get_norm &
         (sim%spar, sim%integral_sum, sim%n_events)
    if (present (verbose)) then
       if (verbose) then
           write (msg_buffer, "(A,1x,I0)") &
                 "Requested number of events =", sim%n_events
           call msg_message ()           
           write (msg_buffer, "(A,1x,G11.4)") &
                 "Event sample corresponds to luminosity [fb-1] = ", &
                 sim%luminosity
           call msg_message ()           
       end if
    end if
  end subroutine simulation_setup_n_events

  subroutine simulation_prepare_event_generation (sim, verbose)
    type(simulation_t), intent(inout), target :: sim
    logical, intent(in), optional :: verbose
    integer :: proc
    logical :: ok, verb
    type(process_t), pointer :: process
    verb = .false.;  if (present (verbose)) verb = verbose
    allocate (sim%decay_tree (sim%n_proc))
    do proc = 1, sim%n_proc
       process => sim%prc_array(proc)%ptr
       call process_setup_event_generation (process)
       call decay_tree_init (sim%decay_tree(proc), process)
    end do
    call file_list_open (sim%file_list, sim%process_id, sim%n_events)
    if (sim%read_raw) then
       call open_raw_event_file_for_reading &
            (sim%file_raw, sim%md5sum, sim%u_raw, ok, verbose)
       if (.not. ok)  sim%read_raw = .false.
    else
       if (verb) then
          write (msg_buffer, "(A,I0,A)") &
                "Generating ", sim%n_events, " events ..."
          call msg_message
       end if
    end if
    if (.not. sim%read_raw) then
       if (sim%write_raw) then
          call open_raw_event_file_for_writing &
               (sim%file_raw, sim%md5sum, sim%u_raw, verbose)
       end if
    end if
    call checkpointing_init (sim%checkpointing, sim%var_list)
    sim%n_read = 0
    sim%i_evt = 0
  end subroutine simulation_prepare_event_generation

  subroutine simulation_setup_analysis (sim, pn_analysis_lexpr, verbose)
    type(simulation_t), intent(inout), target :: sim
    type(parse_node_t), pointer :: pn_analysis_lexpr
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       if (associated (pn_analysis_lexpr)) then
          call msg_message ("Using user-defined analysis setup.")
       else
          call msg_message ("No analysis setup has been provided.")
       end if
    end if
    if (associated (pn_analysis_lexpr)) then
       call eval_tree_init_lexpr (sim%analysis_expr, &
            pn_analysis_lexpr, sim%var_list, sim%prt_list, &
            sim%event_weight, sim%event_sqme)
    end if
  end subroutine simulation_setup_analysis

  subroutine simulation_read_event_raw (sim, verbose)
    type(simulation_t), intent(inout), target :: sim
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: iostat
    verb = .false.;  if (present (verbose)) verb = verbose
    call event_read_raw (sim%event, sim%u_raw, &
         sim%event_weight, sim%event_sqme, iostat=iostat)
    if (iostat == 0) then
       sim%i_evt = sim%i_evt + 1
       sim%n_read = sim%n_read + 1
    else
       if (verb) then
          write (msg_buffer, "(A,1x,I0,1x,A)")  &
                "...", sim%n_read, "events read."
          call msg_message ()
       end if
       sim%read_raw = .false.
       if (verb) then
          write (msg_buffer, "(A,1x,I0,1x,A)") &
                "Generating", sim%n_events - sim%n_read, " events ..."
          call msg_message ()
       end if
       if (sim%write_raw) then
           call reopen_raw_event_file_for_writing &
                (sim%file_raw, sim%u_raw, verbose)
       else
           close (sim%u_raw)
       end if
    end if
  end subroutine simulation_read_event_raw

  subroutine simulation_select_process (sim, rng, process, proc)
    type(simulation_t), intent(in) :: sim
    type(tao_random_state), intent(inout) :: rng
    type(process_t), pointer :: process
    integer, intent(out) :: proc
    real(default) :: integral_cmp, x
    call tao_random_number (rng, x)
    integral_cmp = 0
    do proc = 1, sim%n_proc
       integral_cmp = integral_cmp + sim%integral(proc)
       if (integral_cmp > x * sim%integral_sum)  exit
    end do
    proc = min (proc, sim%n_proc)
    process => sim%prc_array(proc)%ptr
  end subroutine simulation_select_process

  subroutine simulation_generate_event (sim, rng, process, proc)
    type(simulation_t), intent(inout), target :: sim
    type(tao_random_state), intent(inout) :: rng
    type(process_t), intent(in), target :: process
    integer, intent(in) :: proc
    call event_init (sim%event, process, &
         sim%event_weight, sim%event_sqme, sim%decay_tree(proc))
    call event_generate &
         (sim%event, rng, sim%spar%unweighted, &
          FM_IGNORE_HELICITY, &
          keep_correlations=.false., &
          keep_virtual=.true.)
    sim%i_evt = sim%i_evt + 1
    sim%event%weight = sim%event%weight * sim%norm_weight
  end subroutine simulation_generate_event

  subroutine simulation_handle_event (sim)
    type(simulation_t), intent(inout), target :: sim
    call event_do_analysis (sim%event, sim%prt_list, sim%analysis_expr)
    call file_list_write_event (sim%file_list, sim%event, i_evt=sim%i_evt)
    if (sim%write_raw .and. .not. sim%read_raw) &
         call event_write_raw (sim%event, sim%u_raw)
    call checkpointing_msg_event &
         (sim%checkpointing, sim%n_events, sim%n_read, sim%i_evt)
  end subroutine simulation_handle_event

  subroutine simulation_final_event (sim)
    type(simulation_t), intent(inout), target :: sim
    call event_final (sim%event)
  end subroutine simulation_final_event

  subroutine simulation_finish_event_generation (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    integer :: proc
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    call checkpointing_msg_end &
         (sim%checkpointing, sim%n_read, sim%i_evt)
    call file_list_close (sim%file_list)
    if (sim%read_raw .or. sim%write_raw)  close (sim%u_raw)
    do proc = 1, sim%n_proc
       call decay_tree_final (sim%decay_tree(proc))
    end do
    call eval_tree_final (sim%analysis_expr)
    if (verb) then
       if (sim%read_raw) then
          write (msg_buffer, "(A,1x,I0,1x,A,1x,I0,1x,A)")  &
                "...", sim%n_read, "events read,", sim%n_events, "total."
          call msg_message ()
       else       
          write (msg_buffer, "(A,1x,I0,1x,A,1x,I0,1x,A)")  &
                "...", sim%n_events - sim%n_read, "events generated.", &
                sim%n_events, "total."
          call msg_message ()
       end if
       call msg_message ("Simulation finished.")
    end if
  end subroutine simulation_finish_event_generation

  subroutine simulation_basic_final (sim)
    type(simulation_t), intent(inout) :: sim
    call var_list_final (sim%var_list)
  end subroutine simulation_basic_final

  subroutine open_raw_event_file_for_reading &
      (file_raw, md5sum, u_raw, ok, verbose)
    type(string_t), intent(in) :: file_raw
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(out) :: u_raw
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: iostat
    verb = .false.;  if (present (verbose))  verb = verbose
    inquire (file = char (file_raw), exist = ok)
    if (ok) then
       if (verb)  call msg_message ("Reading events from file '" &
            // char (file_raw) // "' ...")
       u_raw = free_unit ()
       open (file = char (file_raw), unit = u_raw, form = "unformatted", &
             action = "read", status = "old")
       call raw_event_file_read_header (u_raw, md5sum, ok, iostat)
       if (iostat /= 0) then
          call msg_error ("Event file '" & 
               // char (file_raw) // "' is corrupt, discarding.")
          close (u_raw)
          ok = .false.
       else if (.not. ok) then
          close (u_raw)
          ok = .false.
       else
          ok = .true.
       end if
    end if
  end subroutine open_raw_event_file_for_reading

  subroutine open_raw_event_file_for_writing (file_raw, md5sum, u_raw, verbose)
    type(string_t), intent(in) :: file_raw
    type(md5sum_events_t), intent(in) :: md5sum
    integer, intent(out) :: u_raw
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       call msg_message ("Writing events in internal format to file '" &
            // char (file_raw) // "'")
    end if
    u_raw = free_unit ()
    open (file = char (file_raw), unit = u_raw, form = "unformatted", &
          action = "write", status = "replace")
    call raw_event_file_write_header (u_raw, md5sum)
  end subroutine open_raw_event_file_for_writing

  subroutine reopen_raw_event_file_for_writing (file_raw, u_raw, verbose)
    type(string_t), intent(in) :: file_raw
    integer, intent(in) :: u_raw
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .false.;  if (present (verbose)) verb = verbose
    if (verb) then
       call msg_message ("Appending events in internal format to file '" &
            // char (file_raw) // "'")
    end if
    close (u_raw)
    open (file = char (file_raw), unit = u_raw, form = "unformatted", &
          action = "write", status = "old", position = "append")
  end subroutine reopen_raw_event_file_for_writing

  subroutine simulation_init (sim, process_id, global, ok, verbose)
    type(simulation_t), intent(out) :: sim
    type(string_t), dimension(:), intent(in) :: process_id
    type(rt_data_t), intent(inout), target :: global
    logical, intent(out) :: ok
    logical, intent(in), optional :: verbose
    type(string_t) :: basename_default
    if (size (process_id) /= 0) then
       basename_default = process_id(1)
    else
       basename_default = "whizard"
    end if
    call simulation_basic_init (sim, process_id, global%var_list, verbose)
    call simulation_compute_missing_integrals (sim, global, verbose)
    call simulation_check (sim, ok)
    if (ok) then
       call simulation_collect_integrals (sim, ok)
       if (ok) then
          call simulation_setup_file_list &
               (sim, global%event_fmt, basename_default)
          call simulation_collect_md5sums (sim)
          call simulation_setup_n_events (sim, verbose)
          call simulation_prepare_event_generation (sim, verbose)
       end if
    end if
    if (.not. ok)  call simulation_basic_final (sim)
  end subroutine simulation_init

  function simulation_get_n_events (sim) result (n_events)
    integer :: n_events
    type(simulation_t), intent(in) :: sim
    n_events = sim%n_events
  end function simulation_get_n_events

  subroutine simulation_event (sim, rng, verbose)
    type(simulation_t), intent(inout), target :: sim
    type(tao_random_state), intent(inout) :: rng
    logical, intent(in), optional :: verbose
    type(process_t), pointer :: process
    integer :: proc, i
    if (sim%read_raw) then
       call simulation_read_event_raw (sim, verbose)
    end if
    if (.not. sim%read_raw) then
       if (sim%checkpointing%active .and. (.not. sim%checkpointing%running)) &
          call checkpointing_msg_start (sim%checkpointing, sim%n_events, &
             sim%i_evt)
       call simulation_select_process (sim, rng, process, proc)
       call simulation_generate_event (sim, rng, process, proc)
    end if
    call simulation_handle_event (sim)
    call simulation_final_event (sim)
  end subroutine simulation_event

  subroutine simulation_final (sim, verbose)
    type(simulation_t), intent(inout) :: sim
    logical, intent(in), optional :: verbose
    call simulation_finish_event_generation (sim, verbose)
    call simulation_basic_final (sim)
  end subroutine simulation_final

  subroutine cmd_simulate_final (simulate)
    type(cmd_simulate_t), intent(inout) :: simulate
    if (associated (simulate%options)) then
       call command_list_final (simulate%options)
       deallocate (simulate%options)
    end if
  end subroutine cmd_simulate_final

  subroutine cmd_simulate_write (simulate, unit, indent)
    type(cmd_simulate_t), intent(in) :: simulate
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)", advance="no") "simulate ("
    do i = 1, simulate%n_proc
       if (i /= 1)  write (u, "(', ')", advance="no")
       write (u, "(A)", advance="no")  char (simulate%process_id(i))
    end do
    write (u, "(A)") ")"
    if (associated (simulate%options)) then
       write (u, "(1x,'{')")
       call command_list_write (simulate%options, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    end if
  end subroutine cmd_simulate_write

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
  end subroutine cmd_simulate_compile

  subroutine cmd_simulate_execute (simulate, global)
    type(cmd_simulate_t), intent(inout), target :: simulate
    type(rt_data_t), intent(inout), target :: global
    logical :: ok
    integer :: i_evt
    type(simulation_t), target :: sim
    call rt_data_link (simulate%local, global)
    if (associated (simulate%options)) then
       call command_list_execute (simulate%options, simulate%local)
    end if
    call simulation_init (sim, simulate%process_id, simulate%local, &
         ok, verbose=.true.)
    if (ok) then
       call simulation_setup_analysis &
            (sim, simulate%local%pn_analysis_lexpr, verbose=.true.)
       do i_evt = 1, simulation_get_n_events (sim)
          call simulation_event (sim, simulate%local%rng, verbose=.true.)
       end do
       call simulation_final (sim, verbose=.true.)
    end if
    call rt_data_restore (global, simulate%local)
  end subroutine cmd_simulate_execute

  subroutine cmd_seed_final (seed)
    type(cmd_seed_t), intent(inout) :: seed
    call eval_tree_final (seed%expr)
  end subroutine cmd_seed_final

  subroutine cmd_seed_write (seed, unit, indent)
    type(cmd_seed_t), intent(in) :: seed
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "seed ="
    call eval_tree_write (seed%expr, unit)
  end subroutine cmd_seed_write

  subroutine cmd_seed_compile (seed, pn, global)
    type(cmd_seed_t), pointer :: seed
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_expr
    allocate (seed)
    pn_expr => parse_node_get_sub_ptr (pn, 3)
    call eval_tree_init_expr (seed%expr, pn_expr, global%var_list)
  end subroutine cmd_seed_compile

  subroutine cmd_seed_execute (seed, global)
    type(cmd_seed_t), intent(inout) :: seed
    type(rt_data_t), intent(inout) :: global
    integer :: seed_val
    call eval_tree_evaluate (seed%expr)
    if (eval_tree_result_is_known (seed%expr)) then
       seed_val = eval_tree_get_int (seed%expr)
       call set_rng_seed (global%rng, global%var_list, seed_val, verbose=.true.)
    else
       call msg_error ("Setting seed value: " &
            // "undefined result of integer expression evaluation")
    end if
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
    if (allocated (iterations%expr_n_it)) then
       do i = 1, size (iterations%expr_n_it)
          call eval_tree_final (iterations%expr_n_it(i))
       end do
    end if
    if (allocated (iterations%expr_n_calls)) then
       do i = 1, size (iterations%expr_n_calls)
          call eval_tree_final (iterations%expr_n_calls(i))
       end do
    end if
  end subroutine cmd_iterations_final

  subroutine cmd_iterations_write (iterations, unit, indent)
    type(cmd_iterations_t), intent(in) :: iterations
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(1x,A)")  "iterations ="
    do i = 1, iterations%n_pass
       call eval_tree_write (iterations%expr_n_it(i), unit)
       write (u, "(1x,A)") ":"
       call write_indent (u, indent)
       call eval_tree_write (iterations%expr_n_calls(i), unit)
       call write_indent (u, indent)
       if (i < iterations%n_pass)  write (u, "(1x,A)") ":"
    end do
    write (u, "(1x,A)") ")"
  end subroutine cmd_iterations_write

  subroutine cmd_iterations_compile (iterations, pn, global)
    type(cmd_iterations_t), pointer :: iterations
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(in), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_n_it, pn_n_calls
    type(parse_node_t), pointer :: pn_it_spec, pn_calls_spec
    integer :: i
    allocate (iterations)
    pn_arg => parse_node_get_sub_ptr (pn, 3)
    if (associated (pn_arg)) then
       iterations%n_pass = parse_node_get_n_sub (pn_arg)
       allocate (iterations%expr_n_it (iterations%n_pass))
       allocate (iterations%expr_n_calls (iterations%n_pass))
       pn_it_spec => parse_node_get_sub_ptr (pn_arg)
       i = 1
       do while (associated (pn_it_spec))
          select case (char (parse_node_get_rule_key (pn_it_spec)))
          case ("it_spec")
             pn_n_it => parse_node_get_sub_ptr (pn_it_spec)
             pn_calls_spec => parse_node_get_next_ptr (pn_n_it)
          case ("calls_spec")
             pn_n_it => null ()
             pn_calls_spec => pn_it_spec
          end select
          if (associated (pn_calls_spec)) then
             pn_n_calls => parse_node_get_sub_ptr (pn_calls_spec, 2)
          else
             pn_n_calls => null ()
          end if
          if (associated (pn_n_it)) then
             call eval_tree_init_expr (iterations%expr_n_it(i), &
                  pn_n_it, global%var_list)
          end if
          if (associated (pn_n_calls)) then
             call eval_tree_init_expr (iterations%expr_n_calls(i), &
                  pn_n_calls, global%var_list)
          end if
          i = i + 1
          pn_it_spec => parse_node_get_next_ptr (pn_it_spec)
       end do
    else
       allocate (iterations%expr_n_it (0), iterations%expr_n_calls (0))
    end if
  end subroutine cmd_iterations_compile

  subroutine cmd_iterations_execute (iterations, global)
    type(cmd_iterations_t), intent(inout) :: iterations
    type(rt_data_t), intent(inout) :: global
    integer, dimension(iterations%n_pass) :: n_it, n_calls
    integer :: i
    do i = 1, iterations%n_pass
       call eval_tree_evaluate (iterations%expr_n_it(i))
       call eval_tree_evaluate (iterations%expr_n_calls(i))
       if (eval_tree_result_is_known (iterations%expr_n_it(i))) then
          n_it(i) = eval_tree_get_int (iterations%expr_n_it(i))
       else
          n_it(i) = 0
       end if
       if (eval_tree_result_is_known (iterations%expr_n_calls(i))) then
          n_calls(i) = eval_tree_get_int (iterations%expr_n_calls(i))
       else
          n_calls(i) = 0
       end if
    end do
    call iterations_list_init (global%it_list, n_it, n_calls)
  end subroutine cmd_iterations_execute

  subroutine cmd_scan_final (loop)
    type(cmd_scan_t), intent(inout) :: loop
    integer :: i
    if (associated (loop%cmd_var)) then
       do i = 1, size (loop%cmd_var)
          call command_list_final (loop%cmd_var(i))
       end do
       deallocate (loop%cmd_var)
    end if
    if (allocated (loop%has_range))  deallocate (loop%has_range)
    if (allocated (loop%beg_expr)) then
       do i = 1, size (loop%beg_expr)
          call eval_tree_final (loop%beg_expr(i))
       end do
       deallocate (loop%beg_expr)
    end if
    if (allocated (loop%end_expr)) then
       do i = 1, size (loop%end_expr)
          call eval_tree_final (loop%end_expr(i))
       end do
       deallocate (loop%end_expr)
    end if
    if (allocated (loop%step_type))  deallocate (loop%step_type)
    if (allocated (loop%step_expr)) then
       do i = 1, size (loop%step_expr)
          call eval_tree_final (loop%step_expr(i))
       end do
       deallocate (loop%step_expr)
    end if
    if (associated (loop%body)) then
       call command_list_final (loop%body)
       deallocate (loop%body)
    end if
  end subroutine cmd_scan_final

  subroutine cmd_scan_write (loop, unit, indent)
    type(cmd_scan_t), intent(in) :: loop
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(A)", advance="no") "scan ("
    if (associated (loop%body)) then
       write (u, "(1x,'{')")
       call command_list_write (loop%body, unit, indent)
       call write_indent (u, indent)
       write (u, "(1x,'}')")
    else
       write (u, *)
    end if
  end subroutine cmd_scan_write

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
    case ("cmd_weight_list")
       str_init = 'weight = 0'
    case ("cmd_scale_list")
       str_init = 'scale = 0'
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
               ("scan: model|library|seed|cuts|weight|scale|analysis|" &
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
             allocate (loop%beg_expr (loop%n_arg))
             allocate (loop%end_expr (loop%n_arg))
             allocate (loop%step_type (loop%n_arg)); loop%step_type = STEP_NONE
             allocate (loop%step_expr (loop%n_arg))
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
                   call eval_tree_init_expr (loop%beg_expr(i), &
                        pn_expr, loop%local%var_list)
                   call eval_tree_init_expr (loop%end_expr(i), &
                        pn_range_expr, loop%local%var_list)
                   pn_step => parse_node_get_next_ptr (pn_range_expr)
                   if (associated (pn_step)) then
                      pn_step_op => parse_node_get_sub_ptr (pn_step)
                      select case (char (parse_node_get_key (pn_step_op)))
                      case ("/+");  loop%step_type(i) = STEP_ADD
                      case ("/-");  loop%step_type(i) = STEP_SUB
                      case ("/*");  loop%step_type(i) = STEP_MUL
                      case ("//");  loop%step_type(i) = STEP_DIV
                      end select
                      pn_step_expr => parse_node_get_next_ptr (pn_step_op)
                      call eval_tree_init_expr (loop%step_expr(i), &
                           pn_step_expr, loop%local%var_list)
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
  end subroutine cmd_scan_compile

  recursive subroutine cmd_scan_execute (loop, global)
    type(cmd_scan_t), intent(inout), target :: loop
    type(rt_data_t), intent(inout), target :: global
    type(string_t) :: model_name
    logical :: is_seed
    integer :: i, j
    integer :: i1, i2, istep, ival
    real(default) :: r1, r2, rstep, rval
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
          call eval_tree_evaluate (loop%beg_expr(i))
          if (eval_tree_result_is_known (loop%beg_expr(i))) then
             r1 = eval_tree_get_real (loop%beg_expr(i))
             i1 = eval_tree_get_int  (loop%beg_expr(i))
          else
             call msg_error ("Scan: undefined lower bound, skipping")
             cycle LOOP_LIST
          end if
          call eval_tree_evaluate (loop%end_expr(i))
          if (eval_tree_result_is_known (loop%end_expr(i))) then
             r2 = eval_tree_get_real (loop%end_expr(i))
             i2 = eval_tree_get_int  (loop%end_expr(i))
          else
             call msg_error ("Scan: undefined upper bound, skipping")
             cycle LOOP_LIST
          end if
          select case (loop%step_type(i))
          case (STEP_NONE)
             rstep = 1
             istep = 1
          case default
             call eval_tree_evaluate (loop%step_expr(i))
             if (eval_tree_result_is_known (loop%step_expr(i))) then
                rstep = eval_tree_get_real (loop%step_expr(i))
                istep = eval_tree_get_int  (loop%step_expr(i))
             else
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
             select case (loop%step_type(i))
             case (STEP_NONE, STEP_ADD)
                do j = 0, huge (0)
                   rval = r1 + j * rstep
                   if (rstep > 0) then
                      if (rval > r2) exit
                   else
                      if (rval < r2) exit
                   end if
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
             case (STEP_SUB)
                do j = 0, huge (0)
                   rval = r1 - j * rstep
                   if (rstep > 0) then
                      if (rval < r2) exit
                   else
                      if (rval > r2) exit
                   end if
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
             case (STEP_MUL)
                rval = r1
                do j = 0, huge (0)
                   if (rstep > 1) then
                      if (rval > r2)  exit
                   else 
                      if (rval < r2) exit
                   end if
                   call set_real (rval, j/=0)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                   rval = rval * rstep
                end do
             case (STEP_DIV)
                rval = r1
                do j = 0, huge (0)
                   if (rstep > 1) then
                      if (rval < r2)  exit
                   else 
                      if (rval > r2) exit
                   end if
                   call set_real (rval, j/=0)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                   rval = rval / rstep
                end do
             end select
          case (V_INT)
             select case (loop%step_type(i))
             case (STEP_NONE, STEP_ADD)
                do j = 0, huge (0)
                   ival = i1 + j * istep
                   if (istep > 0) then
                      if (ival > i2) exit
                   else
                      if (ival < i2) exit
                   end if
                   call set_int (ival, j/=0, is_seed)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                end do
             case (STEP_SUB)
                do j = 0, huge (0)
                   ival = i1 - j * istep
                   if (istep > 0) then
                      if (ival < i2) exit
                   else
                      if (ival > i2) exit
                   end if
                   call set_int (ival, j/=0, is_seed)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                end do
             case (STEP_MUL)
                ival = i1
                do j = 0, huge (0)
                   if (istep > 1) then
                      if (ival > i2)  exit
                   else 
                      if (ival < i2) exit
                   end if
                   call set_int (ival, j/=0, is_seed)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                   ival = ival * istep
                end do
             case (STEP_DIV)
                ival = i1
                do j = 0, huge (0)
                   if (istep > 1) then
                      if (ival < i2)  exit
                   else 
                      if (ival > i2) exit
                   end if
                   call set_int (ival, j/=0, is_seed)
                   if (associated (loop%body)) then
                      call command_list_execute (loop%body, loop%local)
                      if (loop%local%quit) then
                         global%quit_code = loop%local%quit_code
                         global%quit = .true.
                         return
                      end if
                   end if
                   ival = ival / istep
                end do
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
    call eval_tree_final (cond%if_lexpr)
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

  recursive subroutine cmd_if_write (cond, unit, indent)
    type(cmd_if_t), intent(in) :: cond
    integer, intent(in), optional :: unit, indent
    integer :: u, ind, i
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(A)", advance="no") "if "
    call eval_tree_write (cond%if_lexpr, unit=unit)
    call write_indent (u, indent)
    write (u, "(A)") " then"
    ind = 2;  if (present (indent))  ind = ind + indent
    if (associated (cond%if_body)) then
       call write_indent (u, ind)
       call command_list_write (cond%if_body, unit, ind)
    end if
    if (associated (cond%elsif_cond)) then
       do i = 1, size (cond%elsif_cond)
          if (associated (cond%elsif_cond(i)%if_body)) then
             call write_indent (u, indent)
             write (u, "(A)", advance="no") "elsif "
             call eval_tree_write (cond%elsif_cond(i)%if_lexpr, unit=unit)
             call write_indent (u, indent)
             write (u, "(A)") " then"
             call write_indent (u, ind)
             call command_list_write (cond%elsif_cond(i)%if_body, unit, ind)
          end if
       end do
    end if
    if (associated (cond%else_body)) then
       call write_indent (u, indent)
       write (u, "(A)", advance="no") "else"
       write (u, "(1x,A,1x)") "else"
       call write_indent (u, ind)
       call command_list_write (cond%else_body, unit, ind)
       call write_indent (u, ind)
     else
       write (u, *)
    end if
  end subroutine cmd_if_write

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
    call eval_tree_init_lexpr (cond%if_lexpr, pn_lexpr, global%var_list)
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
          call eval_tree_init_lexpr &
               (cond%elsif_cond(i)%if_lexpr, pn_lexpr, global%var_list)
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
    integer :: i
    call eval_tree_evaluate (cond%if_lexpr)
    if (eval_tree_result_is_known (cond%if_lexpr)) then
       if (eval_tree_get_log (cond%if_lexpr)) then
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
          call eval_tree_evaluate (cond%elsif_cond(i)%if_lexpr)
          if (eval_tree_result_is_known (cond%elsif_cond(i)%if_lexpr)) then
             if (eval_tree_get_log (cond%elsif_cond(i)%if_lexpr)) then
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

  subroutine cmd_include_write (include, unit, indent)
    type(cmd_include_t), intent(in) :: include
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(A)")  "! begin include file " &
         // '"' // char (include%file) // '"'
    call command_list_write (include%command_list, unit, indent)
    call write_indent (u, indent)
    write (u, "(A)")  "! end include file " &
         // '"' // char (include%file) // '"'
  end subroutine cmd_include_write

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
    call eval_tree_final (quit%code_expr)
  end subroutine cmd_quit_final

  subroutine cmd_quit_write (quit, unit, indent)
    type(cmd_quit_t), intent(in) :: quit
    integer, intent(in), optional :: unit, indent
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    call write_indent (u, indent)
    write (u, "(A)")  "quit"
  end subroutine cmd_quit_write

  subroutine cmd_quit_compile (quit, pn, global)
    type(cmd_quit_t), pointer :: quit
    type(parse_node_t), intent(in), target :: pn
    type(rt_data_t), intent(inout), target :: global
    type(parse_node_t), pointer :: pn_arg, pn_expr
    allocate (quit)
    pn_arg => parse_node_get_sub_ptr (pn, 2)
    if (associated (pn_arg)) then
       pn_expr => parse_node_get_sub_ptr (pn_arg)
       call eval_tree_init_expr (quit%code_expr, pn_expr, global%var_list)
       quit%has_code = .true.
    end if
  end subroutine cmd_quit_compile

  subroutine cmd_quit_execute (quit, global)
    type(cmd_quit_t), intent(inout), target :: quit
    type(rt_data_t), intent(inout), target :: global
    if (quit%has_code) then
       call eval_tree_evaluate  (quit%code_expr)
       if (eval_tree_result_is_known (quit%code_expr)) then
          global%quit_code = eval_tree_get_int (quit%code_expr)
       else
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

  recursive subroutine command_list_write (cmd_list, unit, indent)
    type(command_list_t), intent(in) :: cmd_list
    integer, intent(in), optional :: unit, indent
    type(command_t), pointer :: command
    command => cmd_list%first
    do while (associated (command))
       if (present (indent)) then
          call command_write (command, unit, indent + 1)
       else
          call command_write (command, unit, 1)
       end if
       command => command%next
    end do
  end subroutine command_list_write

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

  recursive subroutine command_list_execute (cmd_list, global, print)
    type(command_list_t), intent(in) :: cmd_list
    type(rt_data_t), intent(inout), target :: global
    logical, intent(in), optional :: print
    type(command_t), pointer :: command
    command => cmd_list%first
    COMMAND_COND: do while (associated (command))
       call command_execute (command, global, print)
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
         // "cmd_print | cmd_printf | cmd_show | cmd_echo | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_weight | cmd_scale | " &
         // "cmd_beams | cmd_integrate | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_clear | " &
         // "cmd_record | " &
         // "cmd_analysis | cmd_write_analysis | " &
         // "cmd_unstable | cmd_stable | cmd_simulate | " &
         // "cmd_process | cmd_compile | cmd_load | cmd_exec | " &
         // "cmd_scan | cmd_if | cmd_include | cmd_quit")
    call ifile_append (ifile, "GRO options = '{' local_command_list '}'")
    call ifile_append (ifile, "SEQ local_command_list = local_command*")
    call ifile_append (ifile, "ALT local_command = " &
         // "cmd_model | cmd_library | cmd_iterations | cmd_sample_format | " &
         // "cmd_seed | " &
         // "cmd_var | cmd_slha | " &
         // "cmd_print | cmd_printf | cmd_show | cmd_echo | " &
         // "cmd_expect | " &
         // "cmd_cuts | cmd_weight | cmd_scale | " &
         // "cmd_beams | " &
         // "cmd_observable | cmd_histogram | cmd_plot | cmd_clear | " &
         // "cmd_record | " &
         // "cmd_analysis | cmd_write_analysis")
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
         // "cmd_string_decl | cmd_string | cmd_alias")
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
    call ifile_append (ifile, "SEQ cmd_slha = slha_action slha_arg options?")
    call ifile_append (ifile, "ALT slha_action = " &
         // "read_slha | write_slha")
    call ifile_append (ifile, "KEY read_slha")
    call ifile_append (ifile, "KEY write_slha")
    call ifile_append (ifile, "ARG slha_arg = ( string_literal )")
    call ifile_append (ifile, "SEQ cmd_print = print_cmd options?")
    call ifile_append (ifile, "SEQ print_cmd = print sprintf_args?")
    call ifile_append (ifile, "KEY print")
    call ifile_append (ifile, "SEQ cmd_printf = printf_cmd options?")
    call ifile_append (ifile, "SEQ printf_cmd = printf_clause sprintf_args?")
    call ifile_append (ifile, "SEQ printf_clause = printf sexpr")
    call ifile_append (ifile, "KEY printf")
    call ifile_append (ifile, "SEQ cmd_show = show show_arg?")
    call ifile_append (ifile, "KEY show")
    call ifile_append (ifile, "ARG show_arg = ( var_generic* )")
    call ifile_append (ifile, "ALT var_generic = " &
         // "model | beams | results | unstable | real | int | " &
         // "cuts | weight | scale | analysis | expect | " &
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
    call ifile_append (ifile, "SEQ cmd_echo = echo echo_arg?")
    call ifile_append (ifile, "KEY echo")
    call ifile_append (ifile, "ARG echo_arg = ( sexpr* )")
    call ifile_append (ifile, "SEQ cmd_weight = weight '=' expr")
    call ifile_append (ifile, "SEQ cmd_scale = scale '=' expr")
    call ifile_append (ifile, "KEY cuts")
    call ifile_append (ifile, "KEY weight")
    call ifile_append (ifile, "KEY scale")
    call ifile_append (ifile, "SEQ cmd_process = process process_id '=' " &
         // "prt_list '=>' prt_list options?")   
    call ifile_append (ifile, "KEY process")
    call ifile_append (ifile, "KEY '=>'")
    call ifile_append (ifile, "LIS prt_list = cexpr+")
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
          // "none | lhapdf | isr | epa | ewa")
    call ifile_append (ifile, "KEY none")
    call ifile_append (ifile, "KEY lhapdf")
    call ifile_append (ifile, "KEY isr")
    call ifile_append (ifile, "KEY epa")
    call ifile_append (ifile, "KEY ewa")    
    call ifile_append (ifile, "SEQ cmd_integrate = " &
         // "integrate proc_arg options?") 
    call ifile_append (ifile, "KEY integrate")
    call ifile_append (ifile, "ARG proc_arg = ( proc_id* )")
    call ifile_append (ifile, "IDE proc_id")
    call ifile_append (ifile, "SEQ cmd_seed = seed '=' expr")
    call ifile_append (ifile, "KEY seed")
    call ifile_append (ifile, "SEQ cmd_iterations = " &
         // "iterations '=' iterations_list")
    call ifile_append (ifile, "KEY iterations")
    call ifile_append (ifile, "LIS iterations_list = iterations_spec+")
    call ifile_append (ifile, "ALT iterations_spec = it_spec | calls_spec")
    call ifile_append (ifile, "SEQ it_spec = expr calls_spec?")
    call ifile_append (ifile, "SEQ calls_spec = ':' expr")
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
!     call ifile_append (ifile, "ARG histogram_arg = " &
!          // "(expr, expr, bin_expr) ")
!     call ifile_append (ifile, "ALT bin_expr = w_bins | n_bins")
!     call ifile_append (ifile, "SEQ n_bins = nbins '=' expr")
!     call ifile_append (ifile, "SEQ w_bins = wbins '=' expr")    
!     call ifile_append (ifile, "KEY nbins")
!     call ifile_append (ifile, "KEY wbins")
    call ifile_append (ifile, "SEQ cmd_plot = " &
         // "plot analysis_tag plot_arg options?")
    call ifile_append (ifile, "KEY plot")
    call ifile_append (ifile, "ARG plot_arg = (expr, expr)")
    call ifile_append (ifile, "SEQ cmd_analysis = analysis '=' lexpr")
    call ifile_append (ifile, "KEY analysis")
    call ifile_append (ifile, "SEQ cmd_write_analysis = " &
         // "write_analysis write_analysis_arg?")
    call ifile_append (ifile, "KEY write_analysis")
    call ifile_append (ifile, "ARG write_analysis_arg = ( analysis_tag* )")
    call ifile_append (ifile, "SEQ cmd_clear = clear clear_arg?")
    call ifile_append (ifile, "KEY clear")
    call ifile_append (ifile, "ARG clear_arg = ( clear_obj* )")
    call ifile_append (ifile, "ALT clear_obj = " &
         // "iterations | cuts | weight | scale | analysis | expect | " &
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
    call ifile_append (ifile, "SEQ cmd_simulate = " &
         // "simulate proc_arg options?")
    call ifile_append (ifile, "KEY simulate")
    call ifile_append (ifile, "SEQ cmd_scan = scan_spec scan_body?")
    call ifile_append (ifile, "SEQ scan_spec = scan scan_command?")
    call ifile_append (ifile, "KEY scan")
    call ifile_append (ifile, "ALT scan_command = " &
         // "cmd_model_list | cmd_library_list | " &
         // "cmd_seed_list | " &
         // "cmd_cuts_list | cmd_weight_list | cmd_scale_list | " &
         // "cmd_analysis_list | " &
         // "cmd_var_list")
    call ifile_append (ifile, "SEQ cmd_model_list = model = model_list_arg")
    call ifile_append (ifile, "ARG model_list_arg = ( model_name* )")
    call ifile_append (ifile, "SEQ cmd_library_list = " &
         // "library = library_list_arg")
    call ifile_append (ifile, "ARG library_list_arg = ( lib_name* )")
    call ifile_append (ifile, "SEQ cmd_seed_list = seed = num_step_list_arg")
    call ifile_append (ifile, "ARG num_step_list_arg = ( num_steps* )")
    call ifile_append (ifile, "SEQ num_steps = expr range_spec?")
    call ifile_append (ifile, "SEQ range_spec = '=>' expr step_spec?")
    call ifile_append (ifile, "SEQ step_spec = step_op expr")
    call ifile_append (ifile, "ALT step_op = '/+' | '/-' | '/*' | '//'")
    call ifile_append (ifile, "KEY '/+'")
    call ifile_append (ifile, "KEY '/-'")
    call ifile_append (ifile, "KEY '/*'")
    call ifile_append (ifile, "KEY '//'")
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
    call ifile_append (ifile, "SEQ cmd_weight_list = weight = num_list_arg")
    call ifile_append (ifile, "SEQ cmd_scale_list = scale = num_list_arg")
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
    type(lexer_t) :: lexer
    type(parse_tree_t) :: parse_tree
    type(command_list_t), target :: command_list
    type(rt_data_t), target :: global
    print *, "* Initialization"
    call os_data_init (global%os_data)
    global%os_data%fcflags = "-gline -C=all"
    allocate (global%rng)
    call tao_random_create (global%rng, 0)
    call syntax_model_file_init ()
    call syntax_phs_forest_init ()
    call syntax_pexpr_init ()
    call syntax_cmd_list_init ()
    call lexer_init_cmd_list (lexer)
    print *, "* Open 'whizard.sin'"
    u = free_unit ()
    open (unit=u, file="whizard.sin")
    call stream_init (stream, u)
    print *, "* Parse"
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (parse_tree, syntax_cmd_list, lexer)
    call stream_final (stream)
    close (u)
    call parse_tree_write (parse_tree)
    print *
    print *, "* Compile command list"
    if (associated (parse_tree_get_root_ptr (parse_tree))) then
       call command_list_compile &
            (command_list, parse_tree_get_root_ptr (parse_tree), global)
    end if
    print *, "* Command list:"
    call command_list_write (command_list)
    print *
    print *, "* Execute command list"
    call command_list_execute (command_list, global, print=.true.)
    print *
    print *, "* Cleanup"
    call command_list_final (command_list)
    call process_store_final ()
    call model_list_final ()
    call syntax_cmd_list_final ()
    call syntax_pexpr_final ()
    call syntax_phs_forest_final ()
    call syntax_model_file_final ()
  end subroutine command_test


end module commands
