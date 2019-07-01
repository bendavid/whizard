! WHIZARD 2.2.7 Aug 11 2015
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

module rt_data

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use format_defs, only: FMT_19, FMT_12
  use system_dependencies
  use diagnostics
  use os_interface
  use lexers
  use parser
  use physics_defs, only: LAMBDA_QCD_REF
  use models
  use jets
  use subevents
  use pdg_arrays
  use variables
  use process_libraries
  use prclib_stacks
  use prc_core, only: helicity_selection_t
  use beam_structures
  use user_files
  use process_stacks
  use iterations

  implicit none
  private

  public :: rt_data_t

  type :: rt_parse_nodes_t
     type(parse_node_t), pointer :: cuts_lexpr => null ()
     type(parse_node_t), pointer :: scale_expr => null ()     
     type(parse_node_t), pointer :: fac_scale_expr => null ()
     type(parse_node_t), pointer :: ren_scale_expr => null ()     
     type(parse_node_t), pointer :: weight_expr => null ()
     type(parse_node_t), pointer :: selection_lexpr => null ()
     type(parse_node_t), pointer :: reweight_expr => null ()
     type(parse_node_t), pointer :: analysis_lexpr => null ()
     type(parse_node_p), dimension(:), allocatable :: alt_setup
   contains
     procedure :: clear => rt_parse_nodes_clear
     procedure :: write => rt_parse_nodes_write
     procedure :: show => rt_parse_nodes_show
  end type rt_parse_nodes_t
     
  type :: rt_data_t
     type(lexer_t), pointer :: lexer => null ()
     type(rt_data_t), pointer :: context => null ()
     type(var_list_t) :: var_list
     type(iterations_list_t) :: it_list
     type(os_data_t) :: os_data
     type(model_list_t) :: model_list
     type(model_t), pointer :: model => null ()
     logical :: model_is_copy = .false.
     type(model_t), pointer :: preload_model => null ()
     type(model_t), pointer :: fallback_model => null ()
     type(model_t), pointer :: radiation_model => null ()
     type(prclib_stack_t) :: prclib_stack
     type(process_library_t), pointer :: prclib => null ()
     type(beam_structure_t) :: beam_structure
     type(rt_parse_nodes_t) :: pn
     type(process_stack_t) :: process_stack
     type(string_t), dimension(:), allocatable :: sample_fmt
     type(file_list_t), pointer :: out_files => null ()
     logical :: quit = .false.
     integer :: quit_code = 0
     type(string_t) :: logfile 
     logical :: nlo_calculation = .false.
     logical, dimension(4) :: active_nlo_components
   contains
     procedure :: write => rt_data_write
     procedure :: write_vars => rt_data_write_vars
     procedure :: write_model_list => rt_data_write_model_list
     procedure :: write_libraries => rt_data_write_libraries
     procedure :: write_beams => rt_data_write_beams
     procedure :: write_expr => rt_data_write_expr
     procedure :: write_process_stack => rt_data_write_process_stack
     procedure :: clear_beams => rt_data_clear_beams
     procedure :: global_init => rt_data_global_init
     procedure :: local_init => rt_data_local_init
     procedure :: init_pointer_variables => rt_data_init_pointer_variables
     procedure :: activate => rt_data_activate
     procedure :: deactivate => rt_data_deactivate
     procedure :: copy_globals => rt_data_copy_globals
     procedure :: restore_globals => rt_data_restore_globals
     procedure :: final => rt_data_global_final
     procedure :: local_final => rt_data_local_final
     procedure :: read_model => rt_data_read_model
     procedure :: init_fallback_model => rt_data_init_fallback_model
     procedure :: init_radiation_model => rt_data_init_radiation_model
     procedure :: select_model => rt_data_select_model
     procedure :: unselect_model => rt_data_unselect_model
     procedure :: ensure_model_copy => rt_data_ensure_model_copy
     procedure :: model_set_real => rt_data_model_set_real
     procedure :: modify_particle => rt_data_modify_particle
     procedure :: get_var_list_ptr => rt_data_get_var_list_ptr
     procedure :: append_log => rt_data_append_log
     procedure :: append_int => rt_data_append_int
     procedure :: append_real => rt_data_append_real
     procedure :: append_cmplx => rt_data_append_cmplx
     procedure :: append_subevt => rt_data_append_subevt
     procedure :: append_pdg_array => rt_data_append_pdg_array
     procedure :: append_string => rt_data_append_string
     procedure :: import_values => rt_data_import_values
     procedure :: unset_values => rt_data_unset_values
     procedure :: set_log => rt_data_set_log
     procedure :: set_int => rt_data_set_int
     procedure :: set_real => rt_data_set_real
     procedure :: set_cmplx => rt_data_set_cmplx
     procedure :: set_subevt => rt_data_set_subevt
     procedure :: set_pdg_array => rt_data_set_pdg_array
     procedure :: set_string => rt_data_set_string
     procedure :: get_lval => rt_data_get_lval
     procedure :: get_ival => rt_data_get_ival
     procedure :: get_rval => rt_data_get_rval
     procedure :: get_cval => rt_data_get_cval
     procedure :: get_pval => rt_data_get_pval
     procedure :: get_aval => rt_data_get_aval
     procedure :: get_sval => rt_data_get_sval
     procedure :: contains => rt_data_contains
     procedure :: add_prclib => rt_data_add_prclib
     procedure :: update_prclib => rt_data_update_prclib
     procedure :: get_helicity_selection => rt_data_get_helicity_selection
     procedure :: show_beams => rt_data_show_beams
     procedure :: get_sqrts => rt_data_get_sqrts
     procedure :: pacify => rt_data_pacify
     procedure :: set_me_method => rt_data_set_me_method
     procedure :: get_me_method => rt_data_get_me_method
  end type rt_data_t


contains

  subroutine rt_parse_nodes_clear (rt_pn, name)
    class(rt_parse_nodes_t), intent(inout) :: rt_pn
    type(string_t), intent(in) :: name
    select case (char (name))
    case ("cuts")
       rt_pn%cuts_lexpr => null ()
    case ("scale")
       rt_pn%scale_expr => null ()
    case ("factorization_scale")
       rt_pn%fac_scale_expr => null ()
    case ("renormalization_scale")
       rt_pn%ren_scale_expr => null ()
    case ("weight")
       rt_pn%weight_expr => null ()
    case ("selection")
       rt_pn%selection_lexpr => null ()
    case ("reweight")
       rt_pn%reweight_expr => null ()
    case ("analysis")
       rt_pn%analysis_lexpr => null ()
    end select
  end subroutine rt_parse_nodes_clear
  
  subroutine rt_parse_nodes_write (object, unit)
    class(rt_parse_nodes_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    call wrt ("Cuts", object%cuts_lexpr)
    call write_separator (u)
    call wrt ("Scale", object%scale_expr)
    call write_separator (u)
    call wrt ("Factorization scale", object%fac_scale_expr)
    call write_separator (u)
    call wrt ("Renormalization scale", object%ren_scale_expr)
    call write_separator (u)
    call wrt ("Weight", object%weight_expr)
    call write_separator (u, 2)
    call wrt ("Event selection", object%selection_lexpr)
    call write_separator (u)
    call wrt ("Event reweighting factor", object%reweight_expr)
    call write_separator (u)
    call wrt ("Event analysis", object%analysis_lexpr)
    if (allocated (object%alt_setup)) then
       call write_separator (u, 2)
       write (u, "(1x,A,':')")  "Alternative setups"
       do i = 1, size (object%alt_setup)
          call write_separator (u)
          call wrt ("Commands", object%alt_setup(i)%ptr)
       end do
    end if
  contains
    subroutine wrt (title, pn)
      character(*), intent(in) :: title
      type(parse_node_t), intent(in), pointer :: pn
      if (associated (pn)) then
         write (u, "(1x,A,':')")  title
         call write_separator (u)
         call parse_node_write_rec (pn, u)
      else
         write (u, "(1x,A,':',1x,A)")  title, "[undefined]"
      end if
    end subroutine wrt
  end subroutine rt_parse_nodes_write
    
  subroutine rt_parse_nodes_show (rt_pn, name, unit)
    class(rt_parse_nodes_t), intent(in) :: rt_pn
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: unit
    type(parse_node_t), pointer :: pn
    integer :: u
    u = given_output_unit (unit)
    select case (char (name))
    case ("cuts")
       pn => rt_pn%cuts_lexpr
    case ("scale")
       pn => rt_pn%scale_expr
    case ("factorization_scale")
       pn => rt_pn%fac_scale_expr
    case ("renormalization_scale")
       pn => rt_pn%ren_scale_expr
    case ("weight")
       pn => rt_pn%weight_expr
    case ("selection")
       pn => rt_pn%selection_lexpr
    case ("reweight")
       pn => rt_pn%reweight_expr
    case ("analysis")
       pn => rt_pn%analysis_lexpr
    end select
    if (associated (pn)) then
       write (u, "(A,1x,A,1x,A)")  "Expression:", char (name), "(parse tree):"
       call parse_node_write_rec (pn, u)
    else
       write (u, "(A,1x,A,A)")  "Expression:", char (name), ": [undefined]"
    end if
  end subroutine rt_parse_nodes_show
  
  subroutine rt_data_write (object, unit, vars, pacify)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(string_t), dimension(:), intent(in), optional :: vars
    logical, intent(in), optional :: pacify
    integer :: u, i
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Runtime data:"
    if (present (vars)) then
       if (size (vars) /= 0) then
          call write_separator (u, 2)
          write (u, "(1x,A)")  "Selected variables:"
          call write_separator (u)
          call object%write_vars (u, vars)
       end if
    else
       call write_separator (u, 2)
       if (associated (object%model)) then
          call object%model%write_var_list (u, follow_link=.true.)
       else
          call var_list_write (object%var_list, u, follow_link=.true.)
       end if
    end if
    if (object%it_list%get_n_pass () > 0) then
       call write_separator (u, 2)
       write (u, "(1x)", advance="no")
       call object%it_list%write (u)
    end if
    if (associated (object%model)) then
       call write_separator (u, 2)
       call object%model%write (u)
    end if
    call object%prclib_stack%write (u)
    call object%beam_structure%write (u)
    call write_separator (u, 2)
    call object%pn%write (u)
    if (allocated (object%sample_fmt)) then
       call write_separator (u)
       write (u, "(1x,A)", advance="no")  "Event sample formats = "
       do i = 1, size (object%sample_fmt)
          if (i > 1)  write (u, "(A,1x)", advance="no")  ","
          write (u, "(A)", advance="no")  char (object%sample_fmt(i))
       end do
       write (u, "(A)")
    end if
    call object%process_stack%write (u, pacify)
    write (u, "(1x,A,1x,L1)")  "quit     :", object%quit
    write (u, "(1x,A,1x,I0)")  "quit_code:", object%quit_code
    call write_separator (u, 2)
    write (u, "(1x,A,1x,A)")   "Logfile  :", "'" // trim (char (object%logfile)) // "'"
    call write_separator (u, 2)
  end subroutine rt_data_write
  
  subroutine rt_data_write_vars (object, unit, vars)
    class(rt_data_t), intent(in), target :: object
    integer, intent(in), optional :: unit
    type(string_t), dimension(:), intent(in), optional :: vars
    type(var_list_t), pointer :: var_list
    integer :: u, i
    u = given_output_unit (unit)
    if (present (vars)) then
       var_list => object%get_var_list_ptr ()
       do i = 1, size (vars)
          associate (var => vars(i))
            if (var_list%contains (var, follow_link=.true.)) then
               call var_list_write_var (var_list, var, unit = u, &
                    follow_link = .true.)
            end if
          end associate
       end do
    end if
  end subroutine rt_data_write_vars
  
  subroutine rt_data_write_model_list (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call object%model_list%write (u)
  end subroutine rt_data_write_model_list

  subroutine rt_data_write_libraries (object, unit, libpath)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: libpath
    integer :: u
    u = given_output_unit (unit)
    call object%prclib_stack%write (u, libpath)
  end subroutine rt_data_write_libraries

  subroutine rt_data_write_beams (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    call object%beam_structure%write (u)
    call write_separator (u, 2)
  end subroutine rt_data_write_beams

  subroutine rt_data_write_expr (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    call object%pn%write (u)
    call write_separator (u, 2)
  end subroutine rt_data_write_expr
  
  subroutine rt_data_write_process_stack (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call object%process_stack%write (unit)
  end subroutine rt_data_write_process_stack
  
  subroutine rt_data_clear_beams (global)
    class(rt_data_t), intent(inout) :: global
    call global%beam_structure%final_sf ()
    call global%beam_structure%final_pol ()
    call global%beam_structure%final_mom ()
  end subroutine rt_data_clear_beams
  
  subroutine rt_data_global_init (global, paths, logfile)
    class(rt_data_t), intent(out), target :: global
    type(paths_t), intent(in), optional :: paths
    type(string_t), intent(in), optional :: logfile
    logical, target, save :: known = .true.
    integer :: seed
    real(default), parameter :: real_specimen = 1.
    call os_data_init (global%os_data, paths)
    if (present (logfile)) then
       global%logfile = logfile
    else
       global%logfile = ""
    end if
    allocate (global%out_files)
    call system_clock (seed)
    call var_list_append_log_ptr &
         (global%var_list, var_str ("?logging"), logging, known, &
         intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("seed"), seed, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$model_name"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("process_num_id"), &
         intrinsic=.true.)        
    call var_list_append_string &
         (global%var_list, var_str ("$method"), var_str ("omega"), &
         intrinsic=.true.)        
    call var_list_append_log &
         (global%var_list, var_str ("?report_progress"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$restrictions"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$omega_flags"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?read_color_factors"), .true., &
          intrinsic=.true.)
!!! JRR: WK please check (#529)    
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_cut"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_event_shape"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_obs1"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_obs2"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_sf"), var_str (""), &
!           intrinsic=.true.)     
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
         (global%var_list, var_str ("sqrts"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("luminosity"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?sf_trace"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$sf_trace_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?sf_allow_s_mapping"), .true., &
          intrinsic=.true.)
    if (present (paths)) then
       call var_list_append_string &
            (global%var_list, var_str ("$lhapdf_dir"), paths%lhapdfdir, &
             intrinsic=.true.)
    else
       call var_list_append_string &
            (global%var_list, var_str ("$lhapdf_dir"), var_str(""), &
             intrinsic=.true.)
    end if 
    call var_list_append_string &
         (global%var_list, var_str ("$lhapdf_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$lhapdf_photon_file"), var_str (""), &
          intrinsic=.true.)    
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_member"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_photon_scheme"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?hoppet_b_matching"), .false., &
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
    call var_list_append_log &
         (global%var_list, var_str ("?isr_recoil"), .false., &
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
    call var_list_append_log &
         (global%var_list, var_str ("?epa_recoil"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_x_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_pt_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_momentum"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_energy"), .false., &
          intrinsic=.true.)               
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_photon1"), .false., &
          intrinsic=.true.)     
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_photon2"), .false., &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_sqrts"), &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_generate"), .true., &
          intrinsic=.true.)               
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_map"), .true., &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_mapping_slope"), 2._default, &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_eps"), 1e-5_default, &
          intrinsic=.true.)               
    call var_list_append_int &
         (global%var_list, var_str ("circe1_ver"), 0, intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_rev"), 0, intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$circe1_acc"), var_str ("SBAND"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_chat"), 0, intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_with_radiation"), .false., &
         intrinsic=.true.)    
    call var_list_append_log &
         (global%var_list, var_str ("?circe2_polarized"), .true., &
          intrinsic=.true.)               
    call var_list_append_string &    
         (global%var_list, var_str ("$circe2_file"), &
          intrinsic=.true.)      
    call var_list_append_string &    
         (global%var_list, var_str ("$circe2_design"), var_str ("*"), &
          intrinsic=.true.)               
    call var_list_append_real &    
         (global%var_list, var_str ("gaussian_spread1"), 0._default, &
          intrinsic=.true.)      
    call var_list_append_real &    
         (global%var_list, var_str ("gaussian_spread2"), 0._default, &
          intrinsic=.true.)      
    call var_list_append_string &    
         (global%var_list, var_str ("$beam_events_file"), &
          intrinsic=.true.)      
    call var_list_append_log &
         (global%var_list, var_str ("?beam_events_warn_eof"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?energy_scan_normalize"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_is_fixed"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_lhapdf"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_pdf_builtin"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_order"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_nf"), 5, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_mz"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_lambda_qcd"), .false., &
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
    call var_list_append_string &
         (global%var_list, var_str ("$rng_method"), var_str ("tao"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$integration_method"), var_str ("vamp"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("threshold_calls"), 10, &
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
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_verbose"), .false., &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_global"), &
         .true., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_global_verbose"), &
         .false., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_channels"), &
         .false., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_channels_verbose"), &
         .false., intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("channel_weights_power"), 0.25_default, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$phs_method"), var_str ("default"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vis_channels"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?check_phs_file"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$phs_file"), var_str (""), &
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
         (global%var_list, var_str ("phs_off_shell"), 2, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("phs_t_channel"), 6, &
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
         (global%var_list, var_str ("?phs_keep_nonresonant"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_step_mapping"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_step_mapping_exp"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_s_mapping"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$run_id"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("n_calls_test"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?integration_timer"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?check_grid_file"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("accuracy_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("error_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("relative_error_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("error_threshold"), &
         0._default, intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vis_history"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?diags"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diags_color"), .false., &
          intrinsic=.true.)    
    call var_list_append_log &
         (global%var_list, var_str ("?check_event_file"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$event_file_version"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("n_events"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?unweighted"), .true., &
          intrinsic=.true.)       
    call var_list_append_real &
         (global%var_list, var_str ("safety_factor"), 1._default, &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?negative_weights"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?keep_beams"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?keep_remnants"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?recover_beams"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?update_event"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_sqme"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_weight"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?use_alpha_s_from_file"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?use_scale_from_file"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?allow_decays"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?auto_decays"), .false., &
          intrinsic=.true.)       
    call var_list_append_int &
         (global%var_list, var_str ("auto_decays_multiplicity"), 2, &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?auto_decays_radiative"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?decay_rest_frame"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?isotropic_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diagonal_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("decay_helicity"), &
          intrinsic=.true.)
    call set_eio_defaults ()
    call var_list_append_int (global%var_list, &
         var_str ("n_bins"), 20, &
         intrinsic=.true.)
    call var_list_append_log (global%var_list, &
         var_str ("?normalize_bins"), .false., &
         intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$obs_label"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$obs_unit"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$title"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$description"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$x_label"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$y_label"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("graph_width_mm"), 130, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("graph_height_mm"), 90, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?y_log"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?x_log"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("x_min"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("x_max"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_min"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_max"),  &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$gmlcode_bg"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$gmlcode_fg"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_histogram"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_base"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_piecewise"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?fill_curve"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_curve"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_errors"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_symbols"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$fill_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$draw_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$err_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$symbol"), &
          intrinsic=.true.)
    call var_list_append_log (global%var_list, &
         var_str ("?analysis_file_only"), .false., &
         intrinsic=.true.)
    call var_list_append_real (global%var_list, &
         var_str ("tolerance"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("checkpoint"), 0, &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?pacify"), .false., &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$out_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_advance"), .true., &
          intrinsic=.true.)
!!! JRR: WK please check (#542)    
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_custom"), .false., &
!           intrinsic=.true.)
!     call var_list_append_string &
!          (global%var_list, var_str ("$out_comment"), var_str ("# "), &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_header"), .true., &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_yerr"), .true., &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_xerr"), .true., &
!           intrinsic=.true.)
    call var_list_append_int (global%var_list, var_str ("real_range"), &
         range (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, var_str ("real_precision"), &
         precision (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_epsilon"), &
         epsilon (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_tiny"), &
         tiny (real_specimen), intrinsic = .true., locked = .true.)
    !!! FastJet parameters
    call var_list_append_int (global%var_list, &
         var_str ("kt_algorithm"), &
         kt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("cambridge_algorithm"), &
         cambridge_algorithm, intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("antikt_algorithm"), &
         antikt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("genkt_algorithm"), &
         genkt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("cambridge_for_passive_algorithm"), &
         cambridge_for_passive_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("genkt_for_passive_algorithm"), &
         genkt_for_passive_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("ee_kt_algorithm"), &
         ee_kt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("ee_genkt_algorithm"), &
         ee_genkt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("plugin_algorithm"), &
         plugin_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("undefined_jet_algorithm"), &
         undefined_jet_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("jet_algorithm"), undefined_jet_algorithm, &
         intrinsic = .true.)
    call var_list_append_real (global%var_list, &
         var_str ("jet_r"), 0._default, &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?polarized_events"), .false., &
            intrinsic=.true.)
    call set_shower_defaults ()
    call set_hadronization_defaults ()
    call set_mlm_matching_defaults ()
    call set_powheg_matching_defaults ()
    call var_list_append_log &
         (global%var_list, var_str ("?ckkw_matching"), .false., &
            intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$pdf_builtin_set"), var_str ("CTEQ6L"), &
         intrinsic=.true.)
    call set_openmp_defaults ()
    call var_list_append_string &
       (global%var_list, var_str ("$born_me_method"), &
        var_str ("omega"), intrinsic = .true.)
    call var_list_append_string &
       (global%var_list, var_str ("$loop_me_method"), &
        var_str ("gosam"), intrinsic = .true.)
    call var_list_append_string &
       (global%var_list, var_str ("$correlation_me_method"), &
        var_str ("omega"), intrinsic = .true.)
    call var_list_append_string &
       (global%var_list, var_str ("$real_tree_me_method"), &
        var_str ("omega"), intrinsic = .true.)
    call var_list_append_int &
       (global%var_list, var_str ("openloops_verbosity"), 1, &
        intrinsic = .true.)
    call var_list_append_real &
        (global%var_list, var_str ("fks_dij_exp1"), &
         1._default, intrinsic = .true.)
    call var_list_append_real &
        (global%var_list, var_str ("fks_dij_exp2"), &
         1._default, intrinsic = .true.)
    call var_list_append_int &
        (global%var_list, var_str ("fks_mapping_type"), &
         1, intrinsic = .true.)
    call var_list_append_log &
        (global%var_list, var_str ("?fks_count_kinematics"), &
         .false., intrinsic = .true.)
    call var_list_append_int &
        (global%var_list, var_str ("alpha_power"), &
         2, intrinsic = .true.)
    call var_list_append_int &
        (global%var_list, var_str ("alphas_power"), &
         0, intrinsic = .true.)
    call var_list_append_log &
        (global%var_list, var_str ("?combined_nlo_integration"), &
         .false., intrinsic = .true.)
    call var_list_append_log &
        (global%var_list, var_str ("?nlo_fixed_order"), &
         .false., intrinsic = .true.)
    call var_list_append_int &
        (global%var_list, var_str ("gks_multiplicity"), &
         0, intrinsic = .true.)
    call var_list_append_string &
        (global%var_list, var_str ("$gosam_filters_lo"), &
         var_str (""), intrinsic = .true.)
    call var_list_append_string &
         (global%var_list, var_str ("$gosam_filters_nlo"), &
         var_str (""), intrinsic = .true.)
    call global%init_pointer_variables ()
    call global%process_stack%init_var_list (global%var_list)

  contains

    subroutine set_eio_defaults ()
      call var_list_append_string &
           (global%var_list, var_str ("$sample"), var_str (""), &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$sample_normalization"), var_str ("auto"),&
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?sample_pacify"), .false., &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?sample_select"), .true., &
            intrinsic=.true.)
      call var_list_append_int &
           (global%var_list, var_str ("sample_max_tries"), 10000, &
           intrinsic = .true.)
      call var_list_append_int &
           (global%var_list, var_str ("sample_split_n_evt"), 0, &
           intrinsic = .true.)
      call var_list_append_int &
           (global%var_list, var_str ("sample_split_n_kbytes"), 0, &
           intrinsic = .true.)
      call var_list_append_int &
           (global%var_list, var_str ("sample_split_index"), 0, &
           intrinsic = .true.)
      call var_list_append_string &
           (global%var_list, var_str ("$rescan_input_format"), var_str ("raw"), &
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
           (global%var_list, var_str ("$debug_extension"), var_str ("debug"), &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?debug_process"), .true., &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?debug_transforms"), .true., &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?debug_decay"), .true., &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?debug_verbose"), .true., &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?hepevt_ensure_order"), .false., &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_hepevt"), var_str ("hepevt"), &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_ascii_short"), &
            var_str ("short.evt"), intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_ascii_long"), &
            var_str ("long.evt"), intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_athena"), &
            var_str ("athena.evt"), intrinsic=.true.)
      call var_list_append_string &
            (global%var_list, var_str ("$extension_mokka"), &
             var_str ("mokka.evt"), intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$lhef_version"), var_str ("2.0"), &
           intrinsic = .true.)
      call var_list_append_string &
           (global%var_list, var_str ("$lhef_extension"), var_str ("lhe"), &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?lhef_write_sqme_prc"), .true., &
           intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?lhef_write_sqme_ref"), .false., &
           intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?lhef_write_sqme_alt"), .true., &
           intrinsic = .true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_lha"), var_str ("lha"), &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_hepmc"), var_str ("hepmc"), &
            intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?hepmc_output_cross_section"), .false., &
           intrinsic = .true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_lcio"), var_str ("slcio"), &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_stdhep"), var_str ("hep"), &
            intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_stdhep_up"), &
            var_str ("up.hep"), intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_hepevt_verb"), &
            var_str ("hepevt.verb"), intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$extension_lha_verb"), &
            var_str ("lha.verb"), intrinsic=.true.)
    end subroutine set_eio_defaults
    subroutine set_shower_defaults ()
      call var_list_append_log &
           (global%var_list, var_str ("?allow_shower"), .true., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_fsr_active"), .false., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_isr_active"), .false., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?muli_active"), .false., &
              intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$shower_method"), var_str ("WHIZARD"), &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?shower_verbose"), .false., &
              intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$ps_PYTHIA_PYGIVE"), var_str (""), &
            intrinsic=.true.)
      call var_list_append_real (global%var_list, &
           var_str ("ps_mass_cutoff"), 1._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, &
           var_str ("ps_fsr_lambda"), 0.29_default, intrinsic = .true.)
      call var_list_append_real (global%var_list, &
           var_str ("ps_isr_lambda"), 0.29_default, intrinsic = .true.)
      call var_list_append_int (global%var_list, &
           var_str ("ps_max_n_flavors"), 5, intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_isr_alpha_s_running"), .true., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_fsr_alpha_s_running"), .true., &
              intrinsic=.true.)
      call var_list_append_real (global%var_list, var_str ("ps_fixed_alpha_s"), &
           0._default, intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_isr_pt_ordered"), .false., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?ps_isr_angular_ordered"), .true., &
              intrinsic=.true.)
      call var_list_append_real (global%var_list, var_str &
           ("ps_isr_primordial_kt_width"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("ps_isr_primordial_kt_cutoff"), 5._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("ps_isr_z_cutoff"), 0.999_default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("ps_isr_minenergy"), 1._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("ps_isr_tscalefactor"), 1._default, intrinsic = .true.)
      call var_list_append_log (global%var_list, var_str &
           ("?ps_isr_only_onshell_emitted_partons"), .false., intrinsic=.true.)
    end subroutine set_shower_defaults

    subroutine set_mlm_matching_defaults ()
      call var_list_append_log &
           (global%var_list, var_str ("?mlm_matching"), .false., &
              intrinsic=.true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Qcut_ME"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Qcut_PS"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_ptmin"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_etamax"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Rmin"), 0._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Emin"), 0._default, intrinsic = .true.)
      call var_list_append_int (global%var_list, var_str &
           ("mlm_nmaxMEjets"), 0, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_ETclusfactor"), 0.2_default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_ETclusminE"), 5._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_etaclusfactor"), 1._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Rclusfactor"), 1._default, intrinsic = .true.)
      call var_list_append_real (global%var_list, var_str &
           ("mlm_Eclusfactor"), 1._default, intrinsic = .true.)

    end subroutine set_mlm_matching_defaults
    subroutine set_powheg_matching_defaults ()
      call var_list_append_log &
          (global%var_list, var_str ("?powheg_matching"), &
           .false., intrinsic = .true.)
      call var_list_append_log &
          (global%var_list, var_str ("?powheg_use_singular_jacobian"), &
           .false., intrinsic = .true.)
      call var_list_append_int &
          (global%var_list, var_str ("powheg_grid_size_xi"), &
           5, intrinsic = .true.)
      call var_list_append_int &
          (global%var_list, var_str ("powheg_grid_size_y"), &
           5, intrinsic = .true.)
      call var_list_append_int &
          (global%var_list, var_str ("powheg_grid_sampling_points"), &
           500000, intrinsic = .true.)
      call var_list_append_real &
           (global%var_list, var_str ("powheg_pt_min"), &
            1._default, intrinsic = .true.)
      call var_list_append_real &
           (global%var_list, var_str ("powheg_lambda"), &
            LAMBDA_QCD_REF, intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?powheg_rebuild_grids"), &
            .false., intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?use_powheg_damping"), &
            .false., intrinsic = .true.)
      call var_list_append_log &
           (global%var_list, var_str ("?powheg_test_sudakov"), &
            .false., intrinsic = .true.)
    end subroutine set_powheg_matching_defaults

    subroutine set_hadronization_defaults ()
      call var_list_append_log &
           (global%var_list, var_str ("?allow_hadronization"), .true., &
              intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?hadronization_active"), .false., &
              intrinsic=.true.)
      call var_list_append_string &
           (global%var_list, var_str ("$hadronization_method"), &
           var_str ("PYTHIA6"), intrinsic = .true.)
    end subroutine set_hadronization_defaults

    subroutine set_openmp_defaults ()
      call var_list_append_log &
           (global%var_list, var_str ("?omega_openmp"), &
           openmp_is_active (), &
           intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?openmp_is_active"), &
           openmp_is_active (), &
           locked=.true., intrinsic=.true.)
      call var_list_append_int &
           (global%var_list, var_str ("openmp_num_threads_default"), &
           openmp_get_default_max_threads (), &
           locked=.true., intrinsic=.true.)
      call var_list_append_int &
           (global%var_list, var_str ("openmp_num_threads"), &
           openmp_get_max_threads (), &
           intrinsic=.true.)
      call var_list_append_log &
           (global%var_list, var_str ("?openmp_logging"), &
           .true., intrinsic=.true.)
    end subroutine set_openmp_defaults


  end subroutine rt_data_global_init

  subroutine rt_data_local_init (local, global, env)
    class(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    integer, intent(in), optional :: env
    local%context => global
    call local%process_stack%link (global%process_stack)
    call local%process_stack%init_var_list (local%var_list)
    call local%process_stack%link_var_list (global%var_list)
    call var_list_append_string &
         (local%var_list, var_str ("$model_name"), var_str (""), &
          intrinsic=.true.)
    call local%init_pointer_variables ()
    local%fallback_model => global%fallback_model
    local%radiation_model => global%radiation_model
    local%os_data = global%os_data
    local%logfile = global%logfile
    call local%model_list%link (global%model_list)
    local%model => global%model
    if (associated (local%model)) then
       call local%model%link_var_list (local%var_list)
    end if
  end subroutine rt_data_local_init

  subroutine rt_data_init_pointer_variables (local)
    class(rt_data_t), intent(inout), target :: local
    logical, target, save :: known = .true.
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fc"), local%os_data%fc, known, &
          intrinsic=.true.)
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fcflags"), local%os_data%fcflags, known, &
         intrinsic=.true.)
  end subroutine rt_data_init_pointer_variables

  subroutine rt_data_activate (local)
    class(rt_data_t), intent(inout), target :: local
    class(rt_data_t), pointer :: global
    global => local%context
    if (associated (global)) then
       local%lexer => global%lexer
       call global%copy_globals (local)
       local%os_data = global%os_data
       local%logfile = global%logfile
       if (associated (global%prclib)) then
          local%prclib => &
               local%prclib_stack%get_library_ptr (global%prclib%get_name ())
       end if
       call local%import_values ()
       call local%process_stack%link (global%process_stack)
       local%it_list = global%it_list
       local%beam_structure = global%beam_structure
       local%pn = global%pn
       if (allocated (local%sample_fmt))  deallocate (local%sample_fmt)
       if (allocated (global%sample_fmt)) then
          allocate (local%sample_fmt (size (global%sample_fmt)), &
               source = global%sample_fmt)
       end if
       local%out_files => global%out_files
       local%model => global%model
       local%model_is_copy = .false.
    else if (.not. associated (local%model)) then
       local%model => local%preload_model
       local%model_is_copy = .false.
    end if
    if (associated (local%model)) then
       call local%model%link_var_list (local%var_list)
       call local%var_list%set_string (var_str ("$model_name"), &
            local%model%get_name (), is_known = .true.)
    else
       call local%var_list%set_string (var_str ("$model_name"), &
            var_str (""), is_known = .false.)
    end if
  end subroutine rt_data_activate

  subroutine rt_data_deactivate (local, global, keep_local)
    class(rt_data_t), intent(inout), target :: local
    class(rt_data_t), intent(inout), optional, target :: global
    logical, intent(in), optional :: keep_local
    type(string_t) :: global_model, local_model
    logical :: same_model, delete
    delete = .true.;  if (present (keep_local))  delete = .not. keep_local
    if (present (global)) then
       if (associated (global%model) .and. associated (local%model)) then 
          global_model = global%model%get_name ()
          local_model = local%model%get_name ()
          same_model = global_model == local_model
       else
          same_model = .false.
       end if
       if (delete) then
          call local%process_stack%clear ()
          call local%unselect_model ()
          call local%unset_values ()
       else if (associated (local%model)) then
          call local%ensure_model_copy ()
       end if
       if (.not. same_model .and. global_model /= "") then
          call msg_message ("Restoring model '" // char (global_model) // "'")
       end if
       if (associated (global%model)) then
          call global%model%link_var_list (global%var_list)
       end if
       call global%restore_globals (local)
    else
       call local%unselect_model ()
    end if
  end subroutine rt_data_deactivate

  subroutine rt_data_copy_globals (global, local)
    class(rt_data_t), intent(in) :: global
    class(rt_data_t), intent(inout) :: local
    local%prclib_stack = global%prclib_stack
  end subroutine rt_data_copy_globals
 
  subroutine rt_data_restore_globals (global, local)
    class(rt_data_t), intent(inout) :: global
    class(rt_data_t), intent(in) :: local
    global%prclib_stack = local%prclib_stack
  end subroutine rt_data_restore_globals
 
  subroutine rt_data_global_final (global)
    class(rt_data_t), intent(inout) :: global
    call global%process_stack%final ()
    call global%prclib_stack%final ()
    call global%model_list%final ()
    call global%var_list%final (follow_link=.false.)
    if (associated (global%out_files)) then
       call file_list_final (global%out_files)
       deallocate (global%out_files)
    end if
  end subroutine rt_data_global_final

  subroutine rt_data_local_final (local)
    class(rt_data_t), intent(inout) :: local
    call local%process_stack%clear ()
    call local%model_list%final ()
    call local%var_list%final (follow_link=.false.)
  end subroutine rt_data_local_final

  subroutine rt_data_read_model (global, name, model)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    type(model_t), pointer, intent(out) :: model
    type(string_t) :: filename
    filename = name // ".mdl"
    call global%model_list%read_model &
         (name, filename, global%os_data, model)
  end subroutine rt_data_read_model
    
  subroutine rt_data_init_fallback_model (global, name, filename)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name, filename
    call global%model_list%read_model &
         (name, filename, global%os_data, global%fallback_model)
  end subroutine rt_data_init_fallback_model
  
  subroutine rt_data_init_radiation_model (global, name, filename)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name, filename
    call global%model_list%read_model &
         (name, filename, global%os_data, global%radiation_model)
  end subroutine rt_data_init_radiation_model
  
  subroutine rt_data_select_model (global, name)
    class(rt_data_t), intent(inout), target :: global
    type(string_t), intent(in) :: name
    logical :: same_model
    if (associated (global%model)) then
       same_model = global%model%get_name () == name
    else
       same_model = .false.
    end if
    if (.not. same_model) then
       global%model => global%model_list%get_model_ptr (name)
       if (.not. associated (global%model)) then
          call global%read_model (name, global%model)
          global%model_is_copy = .false.
       else if (associated (global%context)) then
          global%model_is_copy = &
               global%model_list%model_exists (name, follow_link=.false.)
       else
          global%model_is_copy = .false.
       end if
    end if
    if (associated (global%model)) then
       call global%model%link_var_list (global%var_list)
       call global%var_list%set_string (var_str ("$model_name"), &
            name, is_known = .true.)
       call msg_message ("Switching to model '" // char (name) // "'")
    else
       call global%var_list%set_string (var_str ("$model_name"), &
            var_str (""), is_known = .false.)
    end if
  end subroutine rt_data_select_model
  
  subroutine rt_data_unselect_model (global)
    class(rt_data_t), intent(inout), target :: global
    if (associated (global%model)) then
       global%model => null ()
       global%model_is_copy = .false.
       call global%var_list%set_string (var_str ("$model_name"), &
            var_str (""), is_known = .false.)
    end if
  end subroutine rt_data_unselect_model
  
  subroutine rt_data_ensure_model_copy (global)
    class(rt_data_t), intent(inout), target :: global
    if (associated (global%context)) then
       if (.not. global%model_is_copy) then
          call global%model_list%append_copy (global%model, global%model)
          global%model_is_copy = .true.
          call global%model%link_var_list (global%var_list)
       end if
    end if
  end subroutine rt_data_ensure_model_copy

  subroutine rt_data_model_set_real (global, name, rval, verbose, pacified)
    class(rt_data_t), intent(inout), target :: global
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in), optional :: verbose, pacified
    call global%ensure_model_copy ()
    call global%model%set_real (name, rval, verbose, pacified)
  end subroutine rt_data_model_set_real

  subroutine rt_data_modify_particle &
       (global, pdg, polarized, stable, decay, &
       isotropic_decay, diagonal_decay, decay_helicity)
    class(rt_data_t), intent(inout), target :: global
    integer, intent(in) :: pdg
    logical, intent(in), optional :: polarized, stable
    logical, intent(in), optional :: isotropic_decay, diagonal_decay
    integer, intent(in), optional :: decay_helicity
    type(string_t), dimension(:), intent(in), optional :: decay
    call global%ensure_model_copy ()
    if (present (polarized)) then
       if (polarized) then
          call global%model%set_polarized (pdg)
       else
          call global%model%set_unpolarized (pdg)
       end if
    end if
    if (present (stable)) then
       if (stable) then
          call global%model%set_stable (pdg)
       else if (present (decay)) then
          call global%model%set_unstable &
               (pdg, decay, isotropic_decay, diagonal_decay, decay_helicity)
       else
          call msg_bug ("Setting particle unstable: missing decay processes")
       end if
    end if
  end subroutine rt_data_modify_particle

  function rt_data_get_var_list_ptr (global) result (var_list)
    class(rt_data_t), intent(in), target :: global
    type(var_list_t), pointer :: var_list
    if (associated (global%model)) then
       var_list => global%model%get_var_list_ptr ()
    else
       var_list => global%var_list
    end if
  end function rt_data_get_var_list_ptr

  subroutine rt_data_append_log (local, name, lval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: lval
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_log (local%var_list, name, lval, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_log
  
  subroutine rt_data_append_int (local, name, ival, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: ival
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_int (local%var_list, name, ival, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_int
  
  subroutine rt_data_append_real (local, name, rval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    real(default), intent(in), optional :: rval
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_real (local%var_list, name, rval, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_real
  
  subroutine rt_data_append_cmplx (local, name, cval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    complex(default), intent(in), optional :: cval
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_cmplx (local%var_list, name, cval, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_cmplx
  
  subroutine rt_data_append_subevt (local, name, pval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in), optional :: pval
    logical, intent(in) :: intrinsic, user
    call var_list_append_subevt (local%var_list, name, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_subevt
  
  subroutine rt_data_append_pdg_array (local, name, aval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in), optional :: aval
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_pdg_array (local%var_list, name, aval, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_pdg_array
  
  subroutine rt_data_append_string (local, name, sval, intrinsic, user)
    class(rt_data_t), intent(inout) :: local
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: sval
    logical, intent(in), optional :: intrinsic, user
    call var_list_append_string (local%var_list, name, sval, &
         intrinsic = intrinsic, user = user)
  end subroutine rt_data_append_string
  
  subroutine rt_data_import_values (local)
    class(rt_data_t), intent(inout) :: local
    type(rt_data_t), pointer :: global
    global => local%context
    if (associated (global)) then
       call var_list_import (local%var_list, global%var_list)
    end if
  end subroutine rt_data_import_values
    
  subroutine rt_data_unset_values (global)
    class(rt_data_t), intent(inout) :: global
    call var_list_undefine (global%var_list, follow_link=.false.)
  end subroutine rt_data_unset_values
  
  subroutine rt_data_set_log (global, name, lval, is_known, verbose)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    logical, intent(in) :: lval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    call global%var_list%set_log (name, lval, is_known, &
         verbose=verbose)
  end subroutine rt_data_set_log

  subroutine rt_data_set_int (global, name, ival, is_known, verbose)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    integer, intent(in) :: ival
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    call global%var_list%set_int (name, ival, is_known, &
         verbose=verbose)
  end subroutine rt_data_set_int

  subroutine rt_data_set_real (global, name, rval, is_known, verbose, pacified)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose, pacified
    call global%var_list%set_real (name, rval, is_known, &
         verbose=verbose, pacified=pacified)
  end subroutine rt_data_set_real

  subroutine rt_data_set_cmplx (global, name, cval, is_known, verbose, pacified)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    complex(default), intent(in) :: cval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose, pacified
    call global%var_list%set_cmplx (name, cval, is_known, &
         verbose=verbose, pacified=pacified)
  end subroutine rt_data_set_cmplx

  subroutine rt_data_set_subevt (global, name, pval, is_known, verbose)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in) :: pval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    call global%var_list%set_subevt (name, pval, is_known, &
         verbose=verbose)
  end subroutine rt_data_set_subevt

  subroutine rt_data_set_pdg_array (global, name, aval, is_known, verbose)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in) :: aval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    call global%var_list%set_pdg_array (name, aval, is_known, &
         verbose=verbose)
  end subroutine rt_data_set_pdg_array

  subroutine rt_data_set_string (global, name, sval, is_known, verbose)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: sval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    call global%var_list%set_string (name, sval, is_known, &
         verbose=verbose)
  end subroutine rt_data_set_string

  function rt_data_get_lval (global, name) result (lval)
    logical :: lval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    lval = var_list%get_lval (name)
  end function rt_data_get_lval
  
  function rt_data_get_ival (global, name) result (ival)
    integer :: ival
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    ival = var_list%get_ival (name)
  end function rt_data_get_ival
  
  function rt_data_get_rval (global, name) result (rval)
    real(default) :: rval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    rval = var_list%get_rval (name)
  end function rt_data_get_rval
    
  function rt_data_get_cval (global, name) result (cval)
    complex(default) :: cval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    cval = var_list%get_cval (name)
  end function rt_data_get_cval

  function rt_data_get_aval (global, name) result (aval)
    type(pdg_array_t) :: aval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    aval = var_list%get_aval (name)
  end function rt_data_get_aval
  
  function rt_data_get_pval (global, name) result (pval)
    type(subevt_t) :: pval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    pval = var_list%get_pval (name)
  end function rt_data_get_pval
  
  function rt_data_get_sval (global, name) result (sval)
    type(string_t) :: sval
    class(rt_data_t), intent(in), target :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    sval = var_list%get_sval (name)
  end function rt_data_get_sval
  
  function rt_data_contains (global, name) result (lval)
    logical :: lval
    class(rt_data_t), intent(in) :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    lval = var_list%contains (name)
  end function rt_data_contains

  subroutine rt_data_add_prclib (global, prclib_entry)
    class(rt_data_t), intent(inout) :: global
    type(prclib_entry_t), intent(inout), pointer :: prclib_entry
    call global%prclib_stack%push (prclib_entry)
    call global%update_prclib (global%prclib_stack%get_first_ptr ())
  end subroutine rt_data_add_prclib
  
  subroutine rt_data_update_prclib (global, lib)
    class(rt_data_t), intent(inout) :: global
    type(process_library_t), intent(in), target :: lib
    global%prclib => lib
    if (global%var_list%contains (&
         var_str ("$library_name"), follow_link = .false.)) then
       call global%var_list%set_string (var_str ("$library_name"), &
            global%prclib%get_name (), is_known=.true.)
    else
       call var_list_append_string (global%var_list, &
            var_str ("$library_name"), global%prclib%get_name (), &
            intrinsic = .true.)
    end if
  end subroutine rt_data_update_prclib

  function rt_data_get_helicity_selection (rt_data) result (helicity_selection)
    class(rt_data_t), intent(in) :: rt_data
    type(helicity_selection_t) :: helicity_selection
    associate (var_list => rt_data%var_list)
      helicity_selection%active = var_list%get_lval (&
           var_str ("?helicity_selection_active"))
      if (helicity_selection%active) then
         helicity_selection%threshold = var_list%get_rval (&
              var_str ("helicity_selection_threshold"))
         helicity_selection%cutoff = var_list%get_ival (&
              var_str ("helicity_selection_cutoff"))
      end if
    end associate
  end function rt_data_get_helicity_selection

  subroutine rt_data_show_beams (rt_data, unit)
    class(rt_data_t), intent(in) :: rt_data
    integer, intent(in), optional :: unit
    type(string_t) :: s
    integer :: u
    u = given_output_unit (unit)
    associate (beams => rt_data%beam_structure, var_list => rt_data%var_list)
      call beams%write (u)
      if (.not. beams%asymmetric () .and. beams%get_n_beam () == 2) then
         write (u, "(2x,A," // FMT_19 // ",1x,'GeV')") "sqrts =", &
              var_list%get_rval (var_str ("sqrts"))         
      end if
      if (beams%contains ("pdf_builtin")) then
         s = var_list%get_sval (var_str ("$pdf_builtin_set"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "PDF set =", '"', char (s), '"'
         else
            write (u, "(2x,A,1x,A)")  "PDF set =", "[undefined]"
         end if
      end if
      if (beams%contains ("lhapdf")) then
         s = var_list%get_sval (var_str ("$lhapdf_dir"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF dir    =", '"', char (s), '"'
         end if
         s = var_list%get_sval (var_str ("$lhapdf_file"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF file   =", '"', char (s), '"'
            write (u, "(2x,A,1x,I0)") "LHAPDF member =", &
                 var_list%get_ival (var_str ("lhapdf_member"))
         else
            write (u, "(2x,A,1x,A)")  "LHAPDF file   =", "[undefined]"
         end if
      end if
      if (beams%contains ("lhapdf_photon")) then
         s = var_list%get_sval (var_str ("$lhapdf_dir"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF dir    =", '"', char (s), '"'
         end if
         s = var_list%get_sval (var_str ("$lhapdf_photon_file"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF file   =", '"', char (s), '"'
            write (u, "(2x,A,1x,I0)") "LHAPDF member =", &
                 var_list%get_ival (var_str ("lhapdf_member"))
            write (u, "(2x,A,1x,I0)") "LHAPDF scheme =", &
                 var_list%get_ival (&
                 var_str ("lhapdf_photon_scheme"))
         else
            write (u, "(2x,A,1x,A)")  "LHAPDF file   =", "[undefined]"
         end if
      end if
      if (beams%contains ("isr")) then
         write (u, "(2x,A," // FMT_19 // ")") "ISR alpha =", &
              var_list%get_rval (var_str ("isr_alpha"))
         write (u, "(2x,A," // FMT_19 // ")") "ISR Q max =", &
              var_list%get_rval (var_str ("isr_q_max"))
         write (u, "(2x,A," // FMT_19 // ")") "ISR mass  =", &
              var_list%get_rval (var_str ("isr_mass"))
         write (u, "(2x,A,1x,I0)") "ISR order  =", &
              var_list%get_ival (var_str ("isr_order"))
         write (u, "(2x,A,1x,L1)") "ISR recoil =", &
              var_list%get_lval (var_str ("?isr_recoil"))
      end if
      if (beams%contains ("epa")) then
         write (u, "(2x,A," // FMT_19 // ")") "EPA alpha  =", &
              var_list%get_rval (var_str ("epa_alpha"))
         write (u, "(2x,A," // FMT_19 // ")") "EPA x min  =", &
              var_list%get_rval (var_str ("epa_x_min"))
         write (u, "(2x,A," // FMT_19 // ")") "EPA Q min  =", &
              var_list%get_rval (var_str ("epa_q_min"))
         write (u, "(2x,A," // FMT_19 // ")") "EPA E max  =", &
              var_list%get_rval (var_str ("epa_e_max"))
         write (u, "(2x,A," // FMT_19 // ")") "EPA mass   =", &
              var_list%get_rval (var_str ("epa_mass"))
         write (u, "(2x,A,1x,L1)") "EPA recoil =", &
              var_list%get_lval (var_str ("?epa_recoil"))
      end if
      if (beams%contains ("ewa")) then
         write (u, "(2x,A," // FMT_19 // ")") "EWA x min       =", &
              var_list%get_rval (var_str ("ewa_x_min"))
         write (u, "(2x,A," // FMT_19 // ")") "EWA Pt max      =", &
              var_list%get_rval (var_str ("ewa_pt_max"))
         write (u, "(2x,A," // FMT_19 // ")") "EWA mass        =", &
              var_list%get_rval (var_str ("ewa_mass"))
         write (u, "(2x,A,1x,L1)") "EWA mom cons.   =", &
              var_list%get_lval (&
              var_str ("?ewa_keep_momentum"))
         write (u, "(2x,A,1x,L1)") "EWA energ. cons. =", &
              var_list%get_lval (&
              var_str ("ewa_keep_energy"))
      end if
      if (beams%contains ("circe1")) then
         write (u, "(2x,A,1x,I0)") "CIRCE1 version    =", &
              var_list%get_ival (var_str ("circe1_ver"))
         write (u, "(2x,A,1x,I0)") "CIRCE1 revision   =", &
              var_list%get_ival (var_str ("circe1_rev")) 
         s = var_list%get_sval (var_str ("$circe1_acc"))
         write (u, "(2x,A,1x,A)") "CIRCE1 acceler.   =", char (s)
         write (u, "(2x,A,1x,I0)") "CIRCE1 chattin.   =", &
              var_list%get_ival (var_str ("circe1_chat"))
         write (u, "(2x,A," // FMT_19 // ")") "CIRCE1 sqrts      =", &
              var_list%get_rval (var_str ("circe1_sqrts"))
         write (u, "(2x,A," // FMT_19 // ")") "CIRCE1 epsil.     =", &
              var_list%get_rval (var_str ("circe1_eps"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 phot. 1  =", &
              var_list%get_lval (var_str ("?circe1_photon1"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 phot. 2  =", &
              var_list%get_lval (var_str ("?circe1_photon2"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 generat. =", &
              var_list%get_lval (var_str ("?circe1_generate"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 mapping  =", &
              var_list%get_lval (var_str ("?circe1_map"))
         write (u, "(2x,A," // FMT_19 // ")") "CIRCE1 map. slope =", &
              var_list%get_rval (var_str ("circe1_mapping_slope"))
         write (u, "(2x,A,1x,L1)") "CIRCE recoil photon =", &
              var_list%get_lval (var_str ("?circe1_with_radiation"))
      end if
      if (beams%contains ("circe2")) then
         s = var_list%get_sval (var_str ("$circe2_design"))
         write (u, "(2x,A,1x,A)") "CIRCE2 design   =", char (s) 
         s = var_list%get_sval (var_str ("$circe2_file"))
         write (u, "(2x,A,1x,A)") "CIRCE2 file     =", char (s)
         write (u, "(2x,A,1x,L1)") "CIRCE2 polarized =", &
              var_list%get_lval (var_str ("?circe2_polarized"))
      end if
      if (beams%contains ("gaussian")) then
         write (u, "(2x,A,1x," // FMT_12 // ")") "Gaussian spread 1    =", &
              var_list%get_rval (var_str ("gaussian_spread1"))
         write (u, "(2x,A,1x," // FMT_12 // ")") "Gaussian spread 2    =", &
              var_list%get_rval (var_str ("gaussian_spread2"))
      end if
      if (beams%contains ("beam_events")) then
         s = var_list%get_sval (var_str ("$beam_events_file"))
         write (u, "(2x,A,1x,A)") "Beam events file     =", char (s)
         write (u, "(2x,A,1x,L1)") "Beam events EOF warn =", &
              var_list%get_lval (var_str ("?beam_events_warn_eof"))
      end if
    end associate
  end subroutine rt_data_show_beams
  
  function rt_data_get_sqrts (rt_data) result (sqrts)
    class(rt_data_t), intent(in) :: rt_data
    real(default) :: sqrts
    sqrts = rt_data%var_list%get_rval (var_str ("sqrts"))
  end function rt_data_get_sqrts
    
  subroutine rt_data_pacify (rt_data, efficiency_reset, error_reset)
    class(rt_data_t), intent(inout) :: rt_data
    logical, intent(in), optional :: efficiency_reset, error_reset
    type(process_entry_t), pointer :: process
    process => rt_data%process_stack%first
    do while (associated (process))
       call process%pacify (efficiency_reset, error_reset)
       process => process%next
    end do    
  end subroutine rt_data_pacify

 subroutine rt_data_set_me_method (global, me_method)
   class(rt_data_t), intent(inout) :: global
   type(string_t), intent(in) :: me_method
   logical :: success
   success = global%var_list%contains (var_str ("$method"))
   if (success) &
      call global%var_list%set_sval (var_str ("$method"), me_method)
 end subroutine rt_data_set_me_method
 
  function rt_data_get_me_method (global) result (me_method)
    type(string_t) :: me_method
    class(rt_data_t), intent(in) :: global
    me_method = global%var_list%get_sval (var_str ("$method"))
  end function rt_data_get_me_method


end module rt_data
