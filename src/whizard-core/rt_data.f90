! WHIZARD 2.0.7 Mar 19 2012
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

module rt_data

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use system_dependencies !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!
  use variables
  use os_interface
  use lexers
  use parser
  use models
  use beams
  use process_libraries
  use iterations
  use beam_polarizations
  use strfun_config
  use user_files

  implicit none
  private

  public :: rt_data_global_init
  public :: rt_data_local_init
  public :: rt_data_local_reset
  public :: rt_data_link
  public :: rt_data_restore
  public :: rt_data_global_final

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
     type(pdf_builtin_status_t) :: pdf_builtin_status
     logical :: sf_list_allocated = .false.
     type(sf_list_t), pointer  :: sf_list => null ()
     type(parse_node_t), pointer :: pn_cuts_lexpr => null ()
     type(parse_node_t), pointer :: pn_scale_expr => null ()     
     type(parse_node_t), pointer :: pn_fac_scale_expr => null ()
     type(parse_node_t), pointer :: pn_ren_scale_expr => null ()     
     type(parse_node_t), pointer :: pn_weight_expr => null ()
     type(parse_node_t), pointer :: pn_selection_lexpr => null ()
     type(parse_node_t), pointer :: pn_reweight_expr => null ()
     type(parse_node_t), pointer :: pn_analysis_lexpr => null ()
     type(parse_node_t), pointer :: pn_histogram_writer => null ()
     type(parse_node_t), pointer :: pn_plot_writer => null ()
     type(file_list_t), pointer :: out_files => null ()
     type(tao_random_state), pointer :: rng => null ()
     type(beam_polarization_t), dimension(:), pointer :: &
        beam_polarization => null ()
     integer :: seed
     integer :: method = PRC_UNDEFINED
     logical :: quit = .false.
     integer :: quit_code = 0
     integer :: environment = -1
     integer :: analysis_data_unit = -1
  end type rt_data_t


contains

  subroutine rt_data_global_init (global, paths)
    type(rt_data_t), intent(out), target :: global
    type(paths_t), intent(in), optional :: paths
    logical, target, save :: known = .true.
    real(default), parameter :: real_specimen = 1.
    call os_data_init (global%os_data, paths)
    allocate (global%out_files)
    allocate (global%rng)
    call system_clock (global%seed)
    call tao_random_create (global%rng, global%seed)
    call var_list_append_int_ptr &
         (global%var_list, var_str ("seed_value"), global%seed, known, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$model_name"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$restrictions"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$omega_flags"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$method"), var_str ("omega"), &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?read_color_factors"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$user_procs_cut"), var_str (""), &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$user_procs_event_shape"), var_str (""), &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$user_procs_obs1"), var_str (""), &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$user_procs_obs2"), var_str (""), &
          intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$user_procs_sf"), var_str (""), &
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
         (global%var_list, var_str ("sqrts"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("beam1_momentum"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("beam2_momentum"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("crossing_angle"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("beams_theta"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("beams_phi"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("luminosity"), 0._default, &
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
    call var_list_append_int &
         (global%var_list, var_str ("circe1_ver"), 0, intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_rev"), 0, intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_acc"), 1, intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_chat"), 0, intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("circe2_sqrts"), &
          intrinsic=.true.)   
    call var_list_append_log &
         (global%var_list, var_str ("?circe2_generate"), .true., &
          intrinsic=.true.)        
    call var_list_append_log &
         (global%var_list, var_str ("?circe2_map"), .true., &
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
    call var_list_append_string &    
         (global%var_list, var_str ("$beam_events_file"), &
          intrinsic=.true.)      
    call var_list_append_log &
         (global%var_list, var_str ("?beam_events_warn_eof"), .true., &
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
    call var_list_append_real &
         (global%var_list, var_str ("channel_weights_power"), 0.25_default, &
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
         (global%var_list, var_str ("?phs_step_mapping_exp"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?allow_global_mapping"), .false., &
          intrinsic=.true.)
!    call var_list_append_log &
!         (global%var_list, var_str ("?old_phs_version"), .false., &
!          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$run_id"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?use_best_grid"), .true., &
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
    call var_list_append_log &
         (global%var_list, var_str ("?vis_history"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?isotropic_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diagonal_decay"), .false., &
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
         (global%var_list, var_str ("?allow_decays"), .true., &
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
         (global%var_list, var_str ("?use_num_id"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?keep_beams"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?update_parameters"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_scale"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_alpha_s"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_sqme"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_weight"), .true., &
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
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepevt_verbose"), var_str ("hepevt.verb"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_lha_verbose"), var_str ("lha.verb"), &
          intrinsic=.true.)
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
    call var_list_append_log &
         (global%var_list, var_str ("?y_log"), .false., &
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
    call var_list_append_real (global%var_list, &
         var_str ("tolerance"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("checkpoint"), intrinsic = .true.)
    call var_list_append_string &
         (global%var_list, var_str ("$out_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_advance"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_custom"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$out_comment"), var_str ("# "), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$out_separator"), var_str (" "), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_columns"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_header"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_yerr"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_xerr"), .true., &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, var_str ("real_range"), &
         range (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, var_str ("real_precision"), &
         precision (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_epsilon"), &
         epsilon (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_tiny"), &
         tiny (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?polarized_events"), .false., &
            intrinsic=.true.)
    ! default settings for shower
    call var_list_append_log &
         (global%var_list, var_str ("?ps_fsr_active"), .false., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_use_PYTHIA_shower"), .true., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_PYTHIA_verbose"), .false., &
            intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$ps_PYTHIA_PYGIVE"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_active"), .false., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str ("ps_mass_cutoff"), &
         1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_fsr_lambda"), &
         0.29_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_lambda"), &
         0.29_default, intrinsic = .true.)
    call var_list_append_int (global%var_list, var_str ("ps_max_n_flavors"), &
         5, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_alpha_s_running"), .true., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_fsr_alpha_s_running"), .true., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str ("ps_fixed_alpha_s"), &
         0._default, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_pt_ordered"), .true., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_angular_ordered"), .true., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_primordial_kt_width"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_primordial_kt_cutoff"), &
         5._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_z_cutoff"), &
         0.999_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_minenergy"), &
         1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("ps_isr_tscalefactor"), &
         1._default, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_only_onshell_emitted_partons"), .false., &
            intrinsic=.true.)
    ! default settings for hadronization
    call var_list_append_log &
         (global%var_list, var_str ("?hadronization_active"), .false., &
            intrinsic=.true.)
    ! setting for my matching
    call var_list_append_log &
         (global%var_list, var_str ("?mlm_matching"), .false., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Qcut_ME"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Qcut_PS"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_ptmin"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_etamax"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Rmin"), &
         0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Emin"), &
         0._default, intrinsic = .true.)
    call var_list_append_int (global%var_list, var_str ("mlm_nmaxMEjets"), &
         0, intrinsic = .true.)

    call var_list_append_real (global%var_list, var_str ("mlm_ETclusfactor"), &
         0.2_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_ETclusminE"), &
         5._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_etaclusfactor"), &
         1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Rclusfactor"), &
         1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str ("mlm_Eclusfactor"), &
         1._default, intrinsic = .true.)

    call var_list_append_string (global%var_list, var_str ("$datafile"), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
          var_str ("$comment_prefix"), var_str ("#"), intrinsic=.true.)
    call var_list_append_log (global%var_list, var_str ("?write_header"), &
          .true., intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$pdf_builtin_path"), intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$pdf_builtin_set"), intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?omega_openmp"), &
         openmp_is_active (), &
         locked=.true., intrinsic=.true.)
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
    call rt_data_init_pointer_variables (global)
    call iterations_lists_init_default (global%it_list_default)
  end subroutine rt_data_global_init                            ! $

  subroutine rt_data_local_init (local, global, env)
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    integer, intent(in), optional :: env
    call var_list_link (local%var_list, global%var_list)
    if (associated (global%model)) then
       call var_list_init_copies (local%var_list, &
            model_get_var_list_ptr (global%model), &
            derived_only = .true.)
    end if
    call rt_data_init_pointer_variables (local)
    if (present (env)) local%environment = env
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

  subroutine rt_data_local_reset (local)
    type(rt_data_t), intent(inout), target :: local
  end subroutine rt_data_local_reset

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
       if (allocated (local%event_fmt)) then
         deallocate (local%event_fmt)
       end if 
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
    local%pn_fac_scale_expr => global%pn_fac_scale_expr
    local%pn_ren_scale_expr => global%pn_ren_scale_expr    
    local%pn_selection_lexpr => global%pn_selection_lexpr
    local%pn_reweight_expr => global%pn_reweight_expr
    local%pn_analysis_lexpr => global%pn_analysis_lexpr
    local%out_files => global%out_files
    local%rng => global%rng
    local%beam_polarization => global%beam_polarization
    local%pn_histogram_writer => global%pn_histogram_writer
    local%pn_plot_writer => global%pn_plot_writer
    local%analysis_data_unit = global%analysis_data_unit
  end subroutine rt_data_link

  subroutine rt_data_restore (global, local, keep_model_vars)
    type(rt_data_t), intent(inout) :: global
    type(rt_data_t), intent(inout) :: local
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
    call var_list_undefine (local%var_list, follow_link=.false.)
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
    call file_list_final (global%out_files)
    deallocate (global%out_files)
    deallocate (global%rng)
  end subroutine rt_data_global_final


end module rt_data
