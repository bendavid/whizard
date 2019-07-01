! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
module expr_tests

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_defs, only: FMT_12
  use format_utils, only: write_separator
  use unit_tests
  use os_interface
  use ifiles
  use lexers
  use parser
  use lorentz
  use sm_qcd
  use model_data
  use interactions, only: reset_interaction_counter
  use variables
  use expr_base
  use eval_trees
  use models
  use subevents
  use subevt_expr
  use phs_base
  use rng_base
  use mci_base
  use process_libraries
  use prc_test
  use prc_core
  use processes
  use events

  implicit none
  private

  public :: subevt_expr_test

contains
  
  subroutine subevt_expr_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (subevt_expr_1, "subevt_expr_1", &
         "parton-event expressions", &
         u, results)
    call test (subevt_expr_2, "subevt_expr_2", &
         "parton-event expressions", &
         u, results)
    call test (processes_5, "processes_5", &
         "handle cuts (partonic event)", &
         u, results)
    call test (processes_6, "processes_6", &
         "handle scales and weight (partonic event)", &
         u, results)
    call test (events_3, "events_3", &
         "expression evaluation", &
         u, results)
end subroutine subevt_expr_test

  subroutine subevt_expr_1 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_cuts, pt_scale, pt_fac_scale, pt_ren_scale
    type(parse_tree_t) :: pt_weight
    type(parse_node_t), pointer :: pn_cuts, pn_scale, pn_fac_scale, pn_ren_scale
    type(parse_node_t), pointer :: pn_weight
    type(eval_tree_factory_t) :: expr_factory
    type(os_data_t) :: os_data
    type(model_t), target :: model
    type(parton_expr_t), target :: expr
    real(default) :: E, Ex, m
    type(vector4_t), dimension(6) :: p
    integer :: i, pdg
    logical :: passed
    real(default) :: scale, fac_scale, ren_scale, weight
    
    write (u, "(A)")  "* Test output: subevt_expr_1"
    write (u, "(A)")  "*   Purpose: Set up a subevt and associated &
         &process-specific expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model%read (var_str ("Test.mdl"), os_data)

    write (u, "(A)")  "* Expression texts"
    write (u, "(A)")


    expr_text = "all Pt > 100 [s]"
    write (u, "(A,A)")  "cuts = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_cuts, stream, .true.)
    call stream_final (stream)
    pn_cuts => parse_tree_get_root_ptr (pt_cuts)

    expr_text = "sqrts"
    write (u, "(A,A)")  "scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_scale, stream, .true.)
    call stream_final (stream)
    pn_scale => parse_tree_get_root_ptr (pt_scale)

    expr_text = "sqrts_hat"
    write (u, "(A,A)")  "fac_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_fac_scale, stream, .true.)
    call stream_final (stream)
    pn_fac_scale => parse_tree_get_root_ptr (pt_fac_scale)

    expr_text = "100"
    write (u, "(A,A)")  "ren_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_ren_scale, stream, .true.)
    call stream_final (stream)
    pn_ren_scale => parse_tree_get_root_ptr (pt_ren_scale)

    expr_text = "n_tot - n_in - n_out"
    write (u, "(A,A)")  "weight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_weight, stream, .true.)
    call stream_final (stream)
    pn_weight => parse_tree_get_root_ptr (pt_weight)

    call ifile_final (ifile)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize process expr"
    write (u, "(A)")

    call expr%setup_vars (1000._default)
    call var_list_append_real (expr%var_list, var_str ("tolerance"), 0._default)
    call expr%link_var_list (model%get_var_list_ptr ())

    call expr_factory%init (pn_cuts)
    call expr%setup_selection (expr_factory)
    call expr_factory%init (pn_scale)
    call expr%setup_scale (expr_factory)
    call expr_factory%init (pn_fac_scale)
    call expr%setup_fac_scale (expr_factory)
    call expr_factory%init (pn_ren_scale)
    call expr%setup_ren_scale (expr_factory)
    call expr_factory%init (pn_weight)
    call expr%setup_weight (expr_factory)

    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Fill subevt and evaluate expressions"
    write (u, "(A)")

    call subevt_init (expr%subevt_t, 6)
    E = 500._default
    Ex = 400._default
    m = 125._default
    pdg = 25
    p(1) = vector4_moving (E, sqrt (E**2 - m**2), 3)
    p(2) = vector4_moving (E, -sqrt (E**2 - m**2), 3)
    p(3) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 3)
    p(4) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 3)
    p(5) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 1)
    p(6) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 1)

    call expr%reset ()
    do i = 1, 2
       call subevt_set_beam (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 3, 4
       call subevt_set_incoming (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 5, 6
       call subevt_set_outgoing (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%n_in = 2
    expr%n_out = 2
    expr%n_tot = 4
    expr%subevt_filled = .true.

    call expr%evaluate (passed, scale, fac_scale, ren_scale, weight)
    
    write (u, "(A,L1)")      "Event has passed      = ", passed
    write (u, "(A," // FMT_12 // ")")  "Scale                 = ", scale
    write (u, "(A," // FMT_12 // ")")  "Factorization scale   = ", fac_scale
    write (u, "(A," // FMT_12 // ")")  "Renormalization scale = ", ren_scale
    write (u, "(A," // FMT_12 // ")")  "Weight                = ", weight
    write (u, "(A)")
    
    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call expr%final ()

    call model%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: subevt_expr_1"
    
  end subroutine subevt_expr_1
  
  subroutine subevt_expr_2 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_selection
    type(parse_tree_t) :: pt_reweight, pt_analysis
    type(parse_node_t), pointer :: pn_selection
    type(parse_node_t), pointer :: pn_reweight, pn_analysis
    type(os_data_t) :: os_data
    type(model_t), target :: model
    type(eval_tree_factory_t) :: expr_factory
    type(event_expr_t), target :: expr
    real(default) :: E, Ex, m
    type(vector4_t), dimension(6) :: p
    integer :: i, pdg
    logical :: passed
    real(default) :: reweight
    logical :: analysis_flag
    
    write (u, "(A)")  "* Test output: subevt_expr_2"
    write (u, "(A)")  "*   Purpose: Set up a subevt and associated &
         &process-specific expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model%read (var_str ("Test.mdl"), os_data)

    write (u, "(A)")  "* Expression texts"
    write (u, "(A)")


    expr_text = "all Pt > 100 [s]"
    write (u, "(A,A)")  "selection = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_selection, stream, .true.)
    call stream_final (stream)
    pn_selection => parse_tree_get_root_ptr (pt_selection)

    expr_text = "n_tot - n_in - n_out"
    write (u, "(A,A)")  "reweight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_reweight, stream, .true.)
    call stream_final (stream)
    pn_reweight => parse_tree_get_root_ptr (pt_reweight)

    expr_text = "true"
    write (u, "(A,A)")  "analysis = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_analysis, stream, .true.)
    call stream_final (stream)
    pn_analysis => parse_tree_get_root_ptr (pt_analysis)

    call ifile_final (ifile)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize process expr"
    write (u, "(A)")

    call expr%setup_vars (1000._default)
    call expr%link_var_list (model%get_var_list_ptr ())
    call var_list_append_real (expr%var_list, var_str ("tolerance"), 0._default)

    call expr_factory%init (pn_selection)
    call expr%setup_selection (expr_factory)
    call expr_factory%init (pn_analysis)
    call expr%setup_analysis (expr_factory)
    call expr_factory%init (pn_reweight)
    call expr%setup_reweight (expr_factory)

    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Fill subevt and evaluate expressions"
    write (u, "(A)")

    call subevt_init (expr%subevt_t, 6)
    E = 500._default
    Ex = 400._default
    m = 125._default
    pdg = 25
    p(1) = vector4_moving (E, sqrt (E**2 - m**2), 3)
    p(2) = vector4_moving (E, -sqrt (E**2 - m**2), 3)
    p(3) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 3)
    p(4) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 3)
    p(5) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 1)
    p(6) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 1)

    call expr%reset ()
    do i = 1, 2
       call subevt_set_beam (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 3, 4
       call subevt_set_incoming (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 5, 6
       call subevt_set_outgoing (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%n_in = 2
    expr%n_out = 2
    expr%n_tot = 4
    expr%subevt_filled = .true.

    call expr%evaluate (passed, reweight, analysis_flag)
    
    write (u, "(A,L1)")      "Event has passed      = ", passed
    write (u, "(A," // FMT_12 // ")")  "Reweighting factor    = ", reweight
    write (u, "(A,L1)")      "Analysis flag         = ", analysis_flag
    write (u, "(A)")
    
    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call expr%final ()

    call model%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: subevt_expr_2"
    
  end subroutine subevt_expr_2
  
  subroutine processes_5 (u)
    integer, intent(in) :: u
    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    type(eval_tree_factory_t) :: expr_factory
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    type(model_t), pointer :: model_tmp
    class(model_data_t), pointer :: model
    type(var_list_t), target :: var_list
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance

    write (u, "(A)")  "* Test output: processes_5"
    write (u, "(A)")  "*   Purpose: create a process &
         &and fill a process instance"
    write (u, "(A)")

    write (u, "(A)")  "* Prepare a cut expression"
    write (u, "(A)")

    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    
    write (u, "(A)")  "* Build and initialize a test process"
    write (u, "(A)")

    libname = "processes5"
    procname = libname
    run_id = "run5"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call syntax_model_file_init ()
    allocate (model_tmp)
    call model_tmp%read (var_str ("Test.mdl"), os_data)
    call var_list_init_snapshot (var_list, model_tmp%get_var_list_ptr ())
    model => model_tmp

    call reset_interaction_counter ()

    allocate (process)
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 

    call var_list_append_real &
         (var_list, var_str ("tolerance"), 0._default)
    call process%set_var_list (var_list)
    call var_list_final (var_list)
   
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")
    
    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()

    write (u, "(A)")  "* Complete process initialization and set cuts"
    write (u, "(A)")

    call process%setup_terms ()
    call expr_factory%init (parse_tree_get_root_ptr (parse_tree))
    call process%set_cuts (expr_factory)
    call process%write (.false., u, show_var_list=.true., show_expressions=.true.)

    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)

    write (u, "(A)")
    write (u, "(A)")  "* Inject a set of random numbers"
    write (u, "(A)")
     
    call process_instance%choose_mci (1)
    call process_instance%set_mcpar ([0._default, 0._default])

    write (u, "(A)")
    write (u, "(A)")  "* Set up kinematics and subevt, check cuts (should fail)"
    write (u, "(A)")

    call process_instance%select_channel (1)
    call process_instance%compute_seed_kinematics ()
    call process_instance%compute_hard_kinematics ()
    call process_instance%compute_eff_kinematics ()
    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels ()
   
    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for another set (should succeed)"
    write (u, "(A)")
     
    call process_instance%reset ()
    call process_instance%set_mcpar ([0.5_default, 0.125_default])
    call process_instance%select_channel (1)
    call process_instance%compute_seed_kinematics ()
    call process_instance%compute_hard_kinematics ()
    call process_instance%compute_eff_kinematics ()
    call process_instance%evaluate_expressions ()
    call process_instance%compute_other_channels ()
    call process_instance%evaluate_trace ()

    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for another set using convenience procedure &
         &(failure)"
    write (u, "(A)")
     
    call process_instance%evaluate_sqme (1, [0.0_default, 0.2_default])

    call process_instance%write_header (u)

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate for another set using convenience procedure &
         &(success)"
    write (u, "(A)")
     
    call process_instance%evaluate_sqme (1, [0.1_default, 0.2_default])

    call process_instance%write_header (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call ifile_final (ifile)
    call syntax_pexpr_final ()
    
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_5"
    
  end subroutine processes_5
  
  subroutine processes_6 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_scale, pt_fac_scale, pt_ren_scale, pt_weight
    type(process_library_t), target :: lib
    type(string_t) :: libname
    type(string_t) :: procname
    type(string_t) :: run_id
    type(os_data_t) :: os_data
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    type(model_t), pointer :: model_tmp
    class(model_data_t), pointer :: model
    type(var_list_t), target :: var_list
    type(process_t), allocatable, target :: process
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts
    type(process_instance_t), allocatable, target :: process_instance
    type(eval_tree_factory_t) :: expr_factory

    write (u, "(A)")  "* Test output: processes_6"
    write (u, "(A)")  "*   Purpose: create a process &
         &and fill a process instance"
    write (u, "(A)")

    write (u, "(A)")  "* Prepare expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()

    expr_text = "sqrts - 100 GeV"
    write (u, "(A,A)")  "scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_scale, stream, .true.)
    call stream_final (stream)

    expr_text = "sqrts_hat"
    write (u, "(A,A)")  "fac_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_fac_scale, stream, .true.)
    call stream_final (stream)
    
    expr_text = "eval sqrt (M2) [collect [s]]"
    write (u, "(A,A)")  "ren_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_ren_scale, stream, .true.)
    call stream_final (stream)
    
    expr_text = "n_tot * n_in * n_out * (eval Phi / pi [s])"
    write (u, "(A,A)")  "weight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_weight, stream, .true.)
    call stream_final (stream)

    call ifile_final (ifile)
    
    write (u, "(A)")
    write (u, "(A)")  "* Build and initialize a test process"
    write (u, "(A)")

    libname = "processes4"
    procname = libname
    run_id = "run4"
    call os_data_init (os_data)
    allocate (rng_test_factory_t :: rng_factory)
    call prc_test_create_library (libname, lib)

    call syntax_model_file_init ()
    allocate (model_tmp)
    call model_tmp%read (var_str ("Test.mdl"), os_data)
    call var_list_init_snapshot (var_list, model_tmp%get_var_list_ptr ())
    model => model_tmp

    call reset_interaction_counter ()

    allocate (process)
    call process%init &
         (procname, run_id, lib, os_data, qcd, rng_factory, model) 

    call process%set_var_list (var_list)
    call var_list_final (var_list)
   
    allocate (test_t :: core_template)
    allocate (phs_test_config_t :: phs_config_template)
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    write (u, "(A)")  "* Prepare a trivial beam setup"
    write (u, "(A)")
    
    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()

    write (u, "(A)")  "* Complete process initialization and set cuts"
    write (u, "(A)")

    call process%setup_terms ()
    call expr_factory%init (parse_tree_get_root_ptr (pt_scale))
    call process%set_scale (expr_factory)
    call expr_factory%init (parse_tree_get_root_ptr (pt_fac_scale))
    call process%set_fac_scale (expr_factory)
    call expr_factory%init (parse_tree_get_root_ptr (pt_ren_scale))
    call process%set_ren_scale (expr_factory)
    call expr_factory%init (parse_tree_get_root_ptr (pt_weight))
    call process%set_weight (expr_factory)
    call process%write (.false., u, show_expressions=.true.)

    write (u, "(A)")
    write (u, "(A)")  "* Create a process instance and evaluate"
    write (u, "(A)")

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%choose_mci (1)
    call process_instance%evaluate_sqme (1, [0.5_default, 0.125_default])

    call process_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    deallocate (process)

    call parse_tree_final (pt_scale)
    call parse_tree_final (pt_fac_scale)
    call parse_tree_final (pt_ren_scale)
    call parse_tree_final (pt_weight)
    call syntax_pexpr_final ()

    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: processes_6"
    
  end subroutine processes_6
  
  subroutine events_3 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_selection, pt_reweight, pt_analysis
    type(eval_tree_factory_t) :: expr_factory
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(os_data_t) :: os_data
    type(model_t), pointer :: model
    type(var_list_t), target :: var_list

    write (u, "(A)")  "* Test output: events_3"
    write (u, "(A)")  "*   Purpose: generate an event and evaluate expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()

    write (u, "(A)")  "* Expression texts"
    write (u, "(A)")

    expr_text = "all Pt > 100 [s]"
    write (u, "(A,A)")  "selection = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_selection, stream, .true.)
    call stream_final (stream)

    expr_text = "1 + sqrts_hat / sqrts"
    write (u, "(A,A)")  "reweight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_reweight, stream, .true.)
    call stream_final (stream)
    
    expr_text = "true"
    write (u, "(A,A)")  "analysis = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_analysis, stream, .true.)
    call stream_final (stream)

    call ifile_final (ifile)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize test process event"

    call os_data_init (os_data)

    call syntax_model_file_init ()
    allocate (model)
    call model%read (var_str ("Test.mdl"), os_data)
    call var_list_init_snapshot (var_list, model%get_var_list_ptr ())

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model)

    call process%set_var_list (var_list)
    call var_list_final (var_list)

    call process_instance%setup_event_data ()

    write (u, "(A)")
    write (u, "(A)")  "* Initialize event object and set expressions"

    allocate (event)
    call event%basic_init ()

    call expr_factory%init (parse_tree_get_root_ptr (pt_selection))
    call event%set_selection (expr_factory)
    call expr_factory%init (parse_tree_get_root_ptr (pt_reweight))
    call event%set_reweight (expr_factory)
    call expr_factory%init (parse_tree_get_root_ptr (pt_analysis))
    call event%set_analysis (expr_factory)
    
    call event%connect (process_instance, process%get_model_ptr ())
    call var_list_append_real &
         (event%expr%var_list, var_str ("tolerance"), 0._default)
    call event%setup_expressions ()

    write (u, "(A)")
    write (u, "(A)")  "* Generate test process event"

    call process%generate_weighted_event (process_instance, 1)

    write (u, "(A)")
    write (u, "(A)")  "* Fill event object and evaluate expressions"
    write (u, "(A)")

    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call event%final ()
    deallocate (event)

    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: events_3"
    
  end subroutine events_3
  

end module expr_tests
