! WHIZARD 2.2.4 Feb 06 2015
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

module dispatch
  
  use kinds, only: default
  use kinds, only: i16, double
  use iso_varying_string, string_t => varying_string
  use constants, only: PI
  use system_dependencies, only: LHAPDF5_AVAILABLE
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use io_units
  use format_utils, only: write_separator
  use unit_tests
  use diagnostics
  use os_interface
  use physics_defs, only: MZ_REF, ALPHA_QCD_MZ_REF
  use physics_defs, only: PROTON, PHOTON, ELECTRON
  use sm_qcd
  use pdg_arrays
  use variables
  use model_data
  use flavors
  use interactions
  use models
  use process_constants
  use pdf_builtin !NODEP!
  use lhapdf !NODEP!  
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
  use rng_base
  use rng_tao
  use mci_base
  use mci_midpoint
  use mci_vamp
  use phs_base
  use mappings
  use phs_forests
  use phs_single
  use phs_wood
  use prc_core_def
  use prc_test
  use beams
  use prc_omega
  use prc_template_me
  use prc_core
  use processes
  use shower_base !NODEP!
  use shower
  use event_transforms
  use decays
  use beam_structures
  use eio_base
  use eio_raw
  use eio_checkpoints
  use eio_lhef
  use eio_hepmc
  use eio_lcio
  use eio_stdhep
  use eio_ascii
  use eio_weights
  use rt_data
  use prc_gosam
  use phs_fks
  use nlo_data

  implicit none
  private

  public :: dispatch_core_def
  public :: dispatch_core
  public :: dispatch_core_update
  public :: dispatch_core_restore
  public :: dispatch_mci
  public :: dispatch_phs
  public :: dispatch_fks
  public :: dispatch_rng_factory
  public :: sf_prop_t
  public :: dispatch_sf_data
  public :: dispatch_sf_config
  public :: dispatch_sf_channels
  public :: dispatch_eio
  public :: dispatch_qcd
  public :: dispatch_shower
  public :: dispatch_evt_decay
  public :: dispatch_evt_shower
  public :: dispatch_slha
  public :: dispatch_test

  type :: sf_prop_t
     real(default), dimension(2) :: isr_eps = 1
  end type sf_prop_t
  

  interface
     subroutine GetXminM (set, mem, xmin)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: xmin
     end subroutine GetXminM
  end interface

  interface
     subroutine GetXmaxM (set, mem, xmax)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: xmax
     end subroutine GetXmaxM
  end interface

  interface
     subroutine GetQ2minM (set, mem, q2min)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: q2min
     end subroutine GetQ2minM
  end interface

  interface
     subroutine GetQ2maxM (set, mem, q2max)
       integer, intent(in) :: set, mem
       double precision, intent(out) :: q2max
     end subroutine GetQ2maxM
  end interface


contains
  
  subroutine dispatch_core_def (core_def, prt_in, prt_out, &
                                global, id, nlo_type)
    class(prc_core_def_t), allocatable, intent(inout) :: core_def
    type(string_t), dimension(:), intent(in) :: prt_in
    type(string_t), dimension(:), intent(in) :: prt_out
    type(rt_data_t), intent(in) :: global
    type(string_t), intent(in), optional :: id
    type(string_t), intent(in), optional :: nlo_type
    type(string_t) :: method
    type(string_t) :: model_name
    type(string_t) :: restrictions
    logical :: openmp_support
    logical :: report_progress
    logical :: diags, diags_color
    type(string_t) :: extra_options
    type(model_t), pointer :: model
    model => global%model
    associate (var_list => global%get_var_list_ptr ())
      method = var_list%get_sval (var_str ("$method"))
      if (associated (model)) then
         model_name = model%get_name ()
      else
         model_name = ""
      end if
      select case (char (method))
      case ("unit_test")
         allocate (prc_test_def_t :: core_def)
         select type (core_def)
         type is (prc_test_def_t)
            call core_def%init (model_name, prt_in, prt_out)
         end select
      case ("template")
         allocate (template_me_def_t :: core_def)
         select type (core_def)
         type is (template_me_def_t)
            call core_def%init (model, prt_in, prt_out, unity = .false.)
         end select
      case ("template_unity")
         allocate (template_me_def_t :: core_def)
         select type (core_def)
         type is (template_me_def_t)
            call core_def%init (model, prt_in, prt_out, unity = .true.)
         end select                  
      case ("omega")
         diags = var_list%get_lval (&
              var_str ("?diags"))
         diags_color = var_list%get_lval (&
              var_str ("?diags_color"))         
         restrictions = var_list%get_sval (&
              var_str ("$restrictions"))
         openmp_support = var_list%get_lval (&
              var_str ("?omega_openmp"))
         report_progress = var_list%get_lval (&
              var_str ("?report_progress"))
         extra_options = var_list%get_sval (&
              var_str ("$omega_flags"))
         allocate (omega_omega_def_t :: core_def)
         select type (core_def)
         type is (omega_omega_def_t)
            call core_def%init (model_name, prt_in, prt_out, &
                 restrictions, openmp_support, report_progress, &
                 extra_options, diags, diags_color)
         end select
      case ("ovm")
         diags = var_list%get_lval (&
              var_str ("?diags"))
         diags_color = var_list%get_lval (&
              var_str ("?diags_color"))         
         restrictions = var_list%get_sval (&
              var_str ("$restrictions"))
         openmp_support = var_list%get_lval (&
              var_str ("?omega_openmp"))
         report_progress = var_list%get_lval (&
              var_str ("?report_progress"))
         extra_options = var_list%get_sval (&
              var_str ("$omega_flags"))
         allocate (omega_ovm_def_t :: core_def)
         select type (core_def)
         type is (omega_ovm_def_t)
            call core_def%init (model_name, prt_in, prt_out, &
                 restrictions, openmp_support, report_progress, &
                 extra_options, diags, diags_color)
         end select         
      case ("gosam")
        allocate (gosam_def_t :: core_def)
        select type (core_def)
        type is (gosam_def_t)
          if (present (id)) then
             call core_def%init (id, model_name, prt_in, &
                                 prt_out, nlo_type)
          else
             call msg_fatal ("Dispatch GoSam def: No id!")
          end if
        end select
      case default
         call msg_fatal ("Process configuration: method '" &
              // char (method) // "' not implemented")
      end select
    end associate
  end subroutine dispatch_core_def
    
  subroutine dispatch_core (core, core_def, model, &
       helicity_selection, qcd, use_color_factors)
    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_def_t), intent(in) :: core_def
    class(model_data_t), intent(in), target, optional :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    logical, intent(in), optional :: use_color_factors
    select type (core_def)
    type is (prc_test_def_t)
       allocate (test_t :: core)
    type is (template_me_def_t)
       allocate (prc_template_me_t :: core)
       select type (core)
       type is (prc_template_me_t)
          call core%set_parameters (model) 
       end select       
    class is (omega_def_t)
       if (.not. allocated (core)) allocate (prc_omega_t :: core)
       select type (core)
       type is (prc_omega_t)
          call core%set_parameters (model, & 
               helicity_selection, qcd, use_color_factors)
       end select
    type is (gosam_def_t)
      if (.not. allocated (core)) allocate (prc_gosam_t :: core)
      select type (core)
      type is (prc_gosam_t)
        call core%set_parameters (qcd, use_color_factors)
      end select
    class default
       call msg_bug ("Process core: unexpected process definition type")
    end select
  end subroutine dispatch_core

  subroutine dispatch_core_update (core, model, helicity_selection, qcd, &
       saved_core)
    class(prc_core_t), allocatable, intent(inout) :: core
    class(model_data_t), intent(in), optional, target :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    class(prc_core_t), allocatable, intent(inout), optional :: saved_core
    if (present (saved_core)) then
       allocate (saved_core, source = core)
    end if
    select type (core)
    type is (test_t)
    type is (prc_omega_t)
       call core%set_parameters (model, helicity_selection, qcd)
       call core%activate_parameters ()
    type is (prc_gosam_t)
      call msg_message ("dispatch core restore: Gosam implementation not present yet!")
    class default
       call msg_bug ("Process core update: unexpected process definition type")
    end select
  end subroutine dispatch_core_update

  subroutine dispatch_core_restore (core, saved_core)
    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_t), allocatable, intent(inout) :: saved_core
    call move_alloc (from = saved_core, to = core)
    select type (core)
    type is (test_t)
    type is (prc_omega_t)
       call core%activate_parameters ()
    class default
       call msg_bug ("Process core restore: unexpected process definition type")
    end select
  end subroutine dispatch_core_restore

  subroutine dispatch_mci (mci, global, process_id)
    class(mci_t), allocatable, intent(inout) :: mci
    type(rt_data_t), intent(in) :: global
    type(string_t), intent(in) :: process_id
    type(string_t) :: run_id
    type(string_t) :: integration_method
    type(grid_parameters_t) :: grid_par
    type(history_parameters_t) :: history_par
    logical :: rebuild_grids, check_grid_file, negative_weights, verbose
    integration_method = &
         global%var_list%get_sval (var_str ("$integration_method"))
    select case (char (integration_method))
    case ("midpoint")
       allocate (mci_midpoint_t :: mci)
    case ("vamp", "default")
       associate (var_list => global%get_var_list_ptr ())
         grid_par%threshold_calls = &
              var_list%get_ival (var_str ("threshold_calls"))
         grid_par%min_calls_per_channel = &
              var_list%get_ival (var_str ("min_calls_per_channel"))
         grid_par%min_calls_per_bin = &
              var_list%get_ival (var_str ("min_calls_per_bin"))
         grid_par%min_bins = &
              var_list%get_ival (var_str ("min_bins"))
         grid_par%max_bins = &
              var_list%get_ival (var_str ("max_bins"))
         grid_par%stratified = &
              var_list%get_lval (var_str ("?stratified"))
         grid_par%use_vamp_equivalences = &
              var_list%get_lval (var_str ("?use_vamp_equivalences"))
         grid_par%channel_weights_power = &
              var_list%get_rval (var_str ("channel_weights_power"))
         grid_par%accuracy_goal = &
              var_list%get_rval (var_str ("accuracy_goal"))
         grid_par%error_goal = &
              var_list%get_rval (var_str ("error_goal"))
         grid_par%rel_error_goal = &
              var_list%get_rval (var_str ("relative_error_goal"))
         history_par%global = &
              var_list%get_lval (var_str ("?vamp_history_global"))
         history_par%global_verbose = &
              var_list%get_lval (var_str ("?vamp_history_global_verbose"))
         history_par%channel = &
              var_list%get_lval (var_str ("?vamp_history_channels"))
         history_par%channel_verbose = &
              var_list%get_lval (var_str ("?vamp_history_channels_verbose"))
         verbose = &
              var_list%get_lval (var_str ("?vamp_verbose"))
         check_grid_file = &
              var_list%get_lval (var_str ("?check_grid_file"))
         run_id = &
              var_list%get_sval (var_str ("$run_id"))
         rebuild_grids = &
              var_list%get_lval (var_str ("?rebuild_grids"))
         negative_weights = &
              var_list%get_lval (var_str ("?negative_weights"))
       end associate
       allocate (mci_vamp_t :: mci)
       select type (mci)
       type is (mci_vamp_t)
          call mci%set_grid_parameters (grid_par)
          if (run_id /= "") then
             call mci%set_grid_filename (process_id, run_id)
          else
             call mci%set_grid_filename (process_id)
          end if
          call mci%set_history_parameters (history_par)
          call mci%set_rebuild_flag (rebuild_grids, check_grid_file)
          mci%negative_weights = negative_weights
          mci%verbose = verbose
       end select
    case default
       call msg_fatal ("Integrator '" &
            // char (integration_method) // "' not implemented")
    end select
  end subroutine dispatch_mci
  
  subroutine dispatch_phs (phs, global, process_id, mapping_defaults, phs_par, &
                           phs_method_in)
    class(phs_config_t), allocatable, intent(inout) :: phs
    type(rt_data_t), intent(in) :: global
    type(string_t), intent(in) :: process_id
    type(mapping_defaults_t), intent(in), optional :: mapping_defaults
    type(phs_parameters_t), intent(in), optional :: phs_par
    type(string_t), intent(in), optional :: phs_method_in
    type(string_t) :: phs_method, phs_file, run_id
    logical :: use_equivalences, vis_channels, fatal_beam_decay
    integer :: u_phs
    logical :: exist
    if (present (phs_method_in)) then
       phs_method = phs_method_in
    else
       phs_method = &
            global%var_list%get_sval (var_str ("$phs_method"))
    end if
    phs_file = &
         global%var_list%get_sval (var_str ("$phs_file"))
    use_equivalences = &
         global%var_list%get_lval (var_str ("?use_vamp_equivalences"))
    vis_channels = &
         global%var_list%get_lval (var_str ("?vis_channels"))
    fatal_beam_decay = &
         global%var_list%get_lval (var_str ("?fatal_beam_decay"))
    run_id = &
         global%var_list%get_sval (var_str ("$run_id"))    
    select case (char (phs_method))
    case ("single")
       allocate (phs_single_config_t :: phs)
       if (vis_channels) then
          call msg_warning ("Visualizing phase space channels not " // &
               "available for method 'single'.")
       end if
    case ("fks")
      allocate (phs_fks_config_t :: phs)
    case ("wood", "default")
       allocate (phs_wood_config_t :: phs)
       select type (phs)
       type is (phs_wood_config_t)
          if (phs_file /= "") then
             inquire (file = char (phs_file), exist = exist)
             if (exist) then
                call msg_message ("Phase space: reading configuration from '" &
                     // char (phs_file) // "'")
                u_phs = free_unit ()
                open (u_phs, file = char (phs_file), &
                     action = "read", status = "old")
                call phs%set_input (u_phs)
             else
                call msg_fatal ("Phase space: configuration file '" &
                     // char (phs_file) // "' not found")
             end if
          end if
          if (present (phs_par)) &
               call phs%set_parameters (phs_par)
          if (use_equivalences) &
               call phs%enable_equivalences ()
          if (present (mapping_defaults)) &
               call phs%set_mapping_defaults (mapping_defaults)
          phs%vis_channels = vis_channels
          phs%fatal_beam_decay = fatal_beam_decay
          phs%os_data = global%os_data
          phs%run_id = run_id
       end select
    case default
       call msg_fatal ("Phase space: parameterization method '" &
            // char (phs_method) // "' not implemented")
    end select
  end subroutine dispatch_phs
  
  subroutine dispatch_fks (fks_template, global)
    type(fks_template_t), intent(inout) :: fks_template
    type(rt_data_t), intent(in) :: global
    real(default) :: fks_dij_exp1, fks_dij_exp2
    integer :: fks_mapping_type
    
    fks_dij_exp1 = &
         global%var_list%get_rval (var_str ("fks_dij_exp1"))
    fks_dij_exp2 = &
         global%var_list%get_rval (var_str ("fks_dij_exp2")) 
    fks_mapping_type = &
         global%var_list%get_ival (var_str ("fks_mapping_type"))

    call fks_template%set_dij_exp (fks_dij_exp1, fks_dij_exp2)
    call fks_template%set_mapping_type (fks_mapping_type)
    
  end subroutine dispatch_fks

  subroutine dispatch_rng_factory (rng_factory, global, local_input)
    class(rng_factory_t), allocatable, intent(inout) :: rng_factory
    type(rt_data_t), intent(inout), target :: global
    type(rt_data_t), intent(in), target, optional :: local_input
    type(rt_data_t), pointer :: local
    type(string_t) :: rng_method
    integer :: seed
    character(30) :: buffer
    integer(i16) :: s
    if (present (local_input)) then
       local => local_input
    else
       local => global
    end if
    rng_method = &
         local%var_list%get_sval (var_str ("$rng_method"))
    seed = &
         local%var_list%get_ival (var_str ("seed"))
    s = mod (seed, 32768)
    select case (char (rng_method))
    case ("unit_test")
       allocate (rng_test_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing Test random-number generator")
    case ("tao")
       allocate (rng_tao_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing TAO random-number generator")       
    case default
       call msg_fatal ("Random-number generator '" &
            // char (rng_method) // "' not implemented")
    end select
    write (buffer, "(I0)")  s
    call msg_message ("RNG: Setting seed for random-number generator to " &
            // trim (buffer))
    call rng_factory%init (s)
    call var_list_set_int (global%var_list, var_str ("seed"), seed + 1, &
         is_known = .true.)
  end subroutine dispatch_rng_factory
  
  subroutine dispatch_sf_data (data, sf_method, i_beam, sf_prop, global, &
       pdg_in, pdg_prc)
    class(sf_data_t), allocatable, intent(inout) :: data
    type(string_t), intent(in) :: sf_method
    integer, dimension(:), intent(in) :: i_beam
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_in
    type(pdg_array_t), dimension(:,:), intent(in) :: pdg_prc
    type(sf_prop_t), intent(inout) :: sf_prop
    type(rt_data_t), intent(inout) :: global
    type(model_t), pointer :: model
    type(pdg_array_t), dimension(:), allocatable :: pdg_out
    real(default) :: sqrts, isr_alpha, isr_q_max, isr_mass
    integer :: isr_order
    logical :: isr_recoil
    real(default) :: epa_alpha, epa_x_min, epa_q_min, epa_e_max, epa_mass
    logical :: epa_recoil
    real(default) :: ewa_x_min, ewa_pt_max, ewa_mass
    logical :: ewa_keep_momentum, ewa_keep_energy   
    type(pdg_array_t), dimension(:), allocatable :: pdg_prc1
    integer :: ewa_id
    type(string_t) :: pdf_name
    type(string_t) :: lhapdf_dir, lhapdf_file
    type(string_t), dimension(13) :: lhapdf_photon_sets
    integer :: lhapdf_member, lhapdf_photon_scheme
    logical :: hoppet_b_matching
    class(rng_factory_t), allocatable :: rng_factory
    logical :: circe1_photon1, circe1_photon2, circe1_generate
    real(default) :: circe1_sqrts, circe1_eps
    integer :: circe1_version, circe1_chattiness, &
         circe1_revision
    character(6) :: circe1_accelerator
    logical :: circe2_polarized
    type(string_t) :: circe2_design, circe2_file
    logical :: beam_events_warn_eof
    type(string_t) :: beam_events_dir, beam_events_file
    logical :: escan_normalize
    lhapdf_photon_sets = [var_str ("DOG0.LHgrid"), var_str ("DOG1.LHgrid"), &
         var_str ("DGG.LHgrid"), var_str ("LACG.LHgrid"), &
         var_str ("GSG0.LHgrid"), var_str ("GSG1.LHgrid"), &
         var_str ("GSG960.LHgrid"), var_str ("GSG961.LHgrid"), &
         var_str ("GRVG0.LHgrid"), var_str ("GRVG1.LHgrid"), &
         var_str ("ACFGPG.LHgrid"), var_str ("WHITG.LHgrid"), &
         var_str ("SASG.LHgrid")]
    model => global%model
    sqrts = global%get_sqrts ()
    associate (var_list => global%get_var_list_ptr ())
      select case (char (sf_method))
      case ("sf_test_0", "sf_test_1")
         allocate (sf_test_data_t :: data)
         select type (data)
         type is (sf_test_data_t)
            select case (char (sf_method))
            case ("sf_test_0");  call data%init (model, pdg_in(i_beam(1)))
            case ("sf_test_1");  call data%init (model, pdg_in(i_beam(1)), &
                 mode = 1)
            end select
         end select
      case ("pdf_builtin")
         allocate (pdf_builtin_data_t :: data)
         select type (data)
         type is (pdf_builtin_data_t)
            pdf_name = &
                 var_list%get_sval (var_str ("$pdf_builtin_set"))
            hoppet_b_matching = &
                 var_list%get_lval (var_str ("?hoppet_b_matching"))
            call data%init ( &
                 model, pdg_in(i_beam(1)), &
                 name = pdf_name, &
                 path = global%os_data%pdf_builtin_datapath, &
                 hoppet_b_matching = hoppet_b_matching)
         end select
      case ("pdf_builtin_photon")
         call msg_fatal ("Currently, there are no photon PDFs built into WHIZARD,", &
              [var_str ("for the photon content inside a proton or neutron use"), &
               var_str ("the 'lhapdf_photon' structure function.")])
      case ("lhapdf")
         allocate (lhapdf_data_t :: data)
         if (pdg_array_get (pdg_in(i_beam(1)), 1) == PHOTON) then
            call msg_fatal ("The 'lhapdf' structure is intended only for protons and", &
                 [var_str ("pions, please use 'lhapdf_photon' for photon beams.")])
         end if         
         lhapdf_dir = &
              var_list%get_sval (var_str ("$lhapdf_dir"))  
         lhapdf_file = &
              var_list%get_sval (var_str ("$lhapdf_file")) 
         lhapdf_member = &
              var_list%get_ival (var_str ("lhapdf_member"))
         lhapdf_photon_scheme = &
              var_list%get_ival (var_str ("lhapdf_photon_scheme"))
         hoppet_b_matching = &
              var_list%get_lval (var_str ("?hoppet_b_matching"))
         select type (data)
         type is (lhapdf_data_t)
            call data%init &
                 (model, pdg_in(i_beam(1)), &
                  lhapdf_dir, lhapdf_file, lhapdf_member, &
                  lhapdf_photon_scheme, hoppet_b_matching)
         end select
      case ("lhapdf_photon")
         allocate (lhapdf_data_t :: data)
         if (pdg_array_get_length (pdg_in(i_beam(1))) /= 1 .or. &
              pdg_array_get (pdg_in(i_beam(1)), 1) /= PHOTON) then
            call msg_fatal ("The 'lhapdf_photon' structure function is exclusively for", &
                 [var_str ("photon PDFs, i.e. for photons as beam particles")])
         end if
         lhapdf_dir = &
              var_list%get_sval (var_str ("$lhapdf_dir"))  
         lhapdf_file = &
              var_list%get_sval (var_str ("$lhapdf_photon_file")) 
         lhapdf_member = &
              var_list%get_ival (var_str ("lhapdf_member"))
         lhapdf_photon_scheme = &
              var_list%get_ival (var_str ("lhapdf_photon_scheme"))
         if (.not. any (lhapdf_photon_sets == lhapdf_file)) then
            call msg_fatal ("This PDF set is not supported or not " // & 
                 "intended for photon beams.")
         end if
         select type (data)
         type is (lhapdf_data_t)
            call data%init &
                 (model, pdg_in(i_beam(1)), &
                  lhapdf_dir, lhapdf_file, lhapdf_member, &
                  lhapdf_photon_scheme)
         end select         
      case ("isr")
         allocate (isr_data_t :: data)
         isr_alpha = &
              var_list%get_rval (var_str ("isr_alpha"))
         if (isr_alpha == 0) then
            isr_alpha = (var_list%get_rval (var_str ("ee"))) &
                 ** 2 / (4 * PI)
         end if
         isr_q_max = &
              var_list%get_rval (var_str ("isr_q_max"))
         if (isr_q_max == 0) then
            isr_q_max = sqrts
         end if
         isr_mass   = var_list%get_rval (var_str ("isr_mass"))
         isr_order  = var_list%get_ival (var_str ("isr_order"))
         isr_recoil = var_list%get_lval (var_str ("?isr_recoil")) 
         select type (data)
         type is (isr_data_t)
            call data%init &
                 (model, pdg_in (i_beam(1)), isr_alpha, isr_q_max, &
                 isr_mass, isr_order, isr_recoil)
            call data%check ()
            sf_prop%isr_eps(i_beam(1)) = data%get_eps ()
         end select
      case ("epa")
         allocate (epa_data_t :: data)
         epa_alpha = var_list%get_rval (var_str ("epa_alpha"))
         if (epa_alpha == 0) then
            epa_alpha = (var_list%get_rval (var_str ("ee"))) &
                 ** 2 / (4 * PI)
         end if         
         epa_x_min = var_list%get_rval (var_str ("epa_x_min"))
         epa_q_min = var_list%get_rval (var_str ("epa_q_min"))
         epa_e_max = var_list%get_rval (var_str ("epa_e_max"))
         if (epa_e_max == 0) then
            epa_e_max = sqrts
         end if
         epa_mass   = var_list%get_rval (var_str ("epa_mass"))
         epa_recoil = var_list%get_lval (var_str ("?epa_recoil"))
         select type (data)            
         type is (epa_data_t)
            call data%init &
                 (model, pdg_in (i_beam(1)), epa_alpha, epa_x_min, &
                 epa_q_min, epa_e_max, epa_mass, epa_recoil)
            call data%check ()
         end select
      case ("ewa")
         allocate (ewa_data_t :: data)
         allocate (pdg_prc1 (size (pdg_prc, 2)))
         pdg_prc1 = pdg_prc(i_beam(1),:)
         if (any (pdg_array_get_length (pdg_prc1) /= 1) &
              .or. any (pdg_prc1 /= pdg_prc1(1))) then
            call msg_fatal &
                 ("EWA: process incoming particle (W/Z) must be unique")
         end if
         ewa_id = abs (pdg_array_get (pdg_prc1(1), 1))
         ewa_x_min = var_list%get_rval (var_str ("ewa_x_min"))
         ewa_pt_max = var_list%get_rval (var_str ("ewa_pt_max")) 
         if (ewa_pt_max == 0) then
            ewa_pt_max = sqrts
         end if
         ewa_mass = var_list%get_rval (var_str ("ewa_mass"))  
         ewa_keep_momentum = var_list%get_lval (&
              var_str ("?ewa_keep_momentum"))
         ewa_keep_energy = var_list%get_lval (&
              var_str ("?ewa_keep_energy"))                  
         if (ewa_keep_momentum .and. ewa_keep_energy) &
              call msg_fatal (" EWA cannot conserve both energy " &
                 // "and momentum.")          
         select type (data)
         type is (ewa_data_t)
            call data%init &
                 (model, pdg_in (i_beam(1)), ewa_x_min, &
                 ewa_pt_max, sqrts, ewa_keep_momentum, &
                 ewa_keep_energy, ewa_mass)
            call data%set_id (ewa_id)
            call data%check ()
         end select
      case ("circe1")
         allocate (circe1_data_t :: data)
         select type (data)
         type is (circe1_data_t)
            circe1_photon1 = &
                 var_list%get_lval (var_str ("?circe1_photon1"))        
            circe1_photon2 = &
                 var_list%get_lval (var_str ("?circe1_photon2"))        
            circe1_sqrts = &
                 var_list%get_rval (var_str ("circe1_sqrts"))
            circe1_eps = &
                 var_list%get_rval (var_str ("circe1_eps"))
            if (circe1_sqrts <= 0)  circe1_sqrts = sqrts
            circe1_generate = &
                 var_list%get_lval (var_str ("?circe1_generate"))
            circe1_version = &
                 var_list%get_ival (var_str ("circe1_ver"))
            circe1_revision = &
                 var_list%get_ival (var_str ("circe1_rev"))
            circe1_accelerator = &
                 char (var_list%get_sval (var_str ("$circe1_acc")))
            circe1_chattiness = &
                 var_list%get_ival (var_str ("circe1_chat"))
            call data%init (model, pdg_in, circe1_sqrts, circe1_eps, &
                 [circe1_photon1, circe1_photon2], &
                 circe1_version, circe1_revision, circe1_accelerator, &
                 circe1_chattiness)
            if (circe1_generate) then
               call msg_message ("Circe1: activating generator mode")
               call dispatch_rng_factory (rng_factory, global)
               call data%set_generator_mode (rng_factory)
            end if
         end select
      case ("circe2")
         allocate (circe2_data_t :: data)
         select type (data)
         type is (circe2_data_t)
            circe2_polarized = &
                 var_list%get_lval (var_str ("?circe2_polarized"))
            circe2_file = &
                 var_list%get_sval (var_str ("$circe2_file"))
            circe2_design = &
                 var_list%get_sval (var_str ("$circe2_design"))
            call data%init (global%os_data, model, pdg_in, sqrts, &
                 circe2_polarized, circe2_file, circe2_design)
            call msg_message ("Circe2: activating generator mode")
            call dispatch_rng_factory (rng_factory, global)
            call data%set_generator_mode (rng_factory)
         end select
      case ("beam_events")
         allocate (beam_events_data_t :: data)
         select type (data)
         type is (beam_events_data_t)
            beam_events_dir = global%os_data%whizard_beamsimpath
            beam_events_file = var_list%get_sval (&
                 var_str ("$beam_events_file"))
            beam_events_warn_eof = var_list%get_lval (&
                 var_str ("?beam_events_warn_eof"))
            call data%init (model, pdg_in, &
                    beam_events_dir, beam_events_file, beam_events_warn_eof)  
         end select
      case ("energy_scan")
         escan_normalize = &
              var_list%get_lval (var_str ("?energy_scan_normalize"))
         allocate (escan_data_t :: data)
         select type (data) 
         type is (escan_data_t)
            if (escan_normalize) then
               call data%init (model, pdg_in)  
            else
               call data%init (model, pdg_in, sqrts)  
            end if
         end select
      case default
         call msg_bug ("Structure function '" &
              // char (sf_method) // "' not implemented yet")
      end select
    end associate
    if (allocated (data)) then
       allocate (pdg_out (size (pdg_prc, 1)))
       call data%get_pdg_out (pdg_out)
       pdg_in(i_beam) = pdg_out
    end if
  end subroutine dispatch_sf_data
  
  function strfun_mode (name) result (n)
    type(string_t), intent(in) :: name
    integer :: n
    select case (char (name))
    case ("none")
       n = 0
    case ("sf_test_0", "sf_test_1")
       n = 1
    case ("pdf_builtin","pdf_builtin_photon", &
          "lhapdf","lhapdf_photon")
       n = 1
    case ("isr","epa","ewa")
       n = 1
    case ("circe1", "circe2")
       n = 2
    case ("beam_events")
       n = 2
    case ("energy_scan")
       n = 2
    case default
       call msg_bug ("Structure function '" // char (name) &
            // "' not supported yet")
    end select
  end function strfun_mode
    
  subroutine dispatch_sf_config (sf_config, sf_prop, global, pdg_prc)
    type(sf_config_t), dimension(:), allocatable, intent(out) :: sf_config
    type(sf_prop_t), intent(out) :: sf_prop
    type(rt_data_t), intent(inout) :: global
    type(beam_structure_t) :: beam_structure
    class(sf_data_t), allocatable :: sf_data
    type(pdg_array_t), dimension(:,:), intent(in) :: pdg_prc
    type(string_t), dimension(:), allocatable :: prt_in
    type(pdg_array_t), dimension(:), allocatable :: pdg_in
    type(flavor_t) :: flv_in
    integer :: n_beam, n_record, i
    beam_structure = global%beam_structure
    call beam_structure%expand (strfun_mode)
    n_record = beam_structure%get_n_record ()
    allocate (sf_config (n_record))
    n_beam = beam_structure%get_n_beam ()
    if (n_beam > 0) then
       allocate (prt_in (n_beam), pdg_in (n_beam))
       prt_in = beam_structure%get_prt ()
       do i = 1, n_beam
          call flavor_init (flv_in, prt_in(i), global%model)
          pdg_in(i) = flavor_get_pdg (flv_in)
       end do
    else
       n_beam = size (pdg_prc, 1)
       allocate (pdg_in (n_beam))
       pdg_in = pdg_prc(:,1)
    end if
    do i = 1, n_record
       call dispatch_sf_data (sf_data, &
            beam_structure%get_name (i), &
            beam_structure%get_i_entry (i), &
            sf_prop, global, pdg_in, pdg_prc)
       call sf_config(i)%init (beam_structure%get_i_entry (i), sf_data)
       deallocate (sf_data)
    end do
  end subroutine dispatch_sf_config
    
  subroutine dispatch_sf_channels (sf_channel, sf_string, sf_prop, coll, global)
    type(sf_channel_t), dimension(:), allocatable, intent(out) :: sf_channel
    type(string_t), intent(out) :: sf_string
    type(sf_prop_t), intent(in) :: sf_prop
    type(phs_channel_collection_t), intent(in) :: coll
    type(rt_data_t), intent(in) :: global
    type(beam_structure_t) :: beam_structure
    class(channel_prop_t), allocatable :: prop
    integer :: n_strfun, n_sf_channel, i
    logical :: sf_allow_s_mapping, circe1_map, circe1_generate
    logical :: s_mapping_enable, endpoint_mapping, power_mapping
    integer, dimension(:), allocatable :: s_mapping, single_mapping
    real(default) :: sqrts, s_mapping_power
    real(default) :: circe1_mapping_slope, endpoint_mapping_slope
    real(default) :: power_mapping_eps
    sqrts = global%get_sqrts ()
    beam_structure = global%beam_structure
    call beam_structure%expand (strfun_mode)
    n_strfun = beam_structure%get_n_record ()
    sf_string = beam_structure%to_string (sf_only = .true.)
    sf_allow_s_mapping = &
         global%var_list%get_lval (var_str ("?sf_allow_s_mapping"))
    circe1_generate = &
         global%var_list%get_lval (var_str ("?circe1_generate"))
    circe1_map = &
         global%var_list%get_lval (var_str ("?circe1_map"))
    circe1_mapping_slope = &
         global%var_list%get_rval (var_str ("circe1_mapping_slope"))
    s_mapping_enable = .false.
    s_mapping_power = 1
    endpoint_mapping = .false.
    endpoint_mapping_slope = 1
    power_mapping = .false.
    select case (char (sf_string))
    case ("", "[any particles]")
    case ("pdf_builtin, none", &
         "pdf_builtin_photon, none", &
         "none, pdf_builtin", &
         "none, pdf_builtin_photon", &
         "lhapdf, none", &
         "lhapdf_photon, none", &
         "none, lhapdf", &
         "none, lhapdf_photon")
    case ("pdf_builtin, none => none, pdf_builtin", &
          "pdf_builtin, none => none, pdf_builtin_photon", &
          "pdf_builtin_photon, none => none, pdf_builtin", &
          "pdf_builtin_photon, none => none, pdf_builtin_photon", &
          "lhapdf, none => none, lhapdf", &
          "lhapdf, none => none, lhapdf_photon", &
          "lhapdf_photon, none => none, lhapdf", &
          "lhapdf_photon, none => none, lhapdf_photon")
       allocate (s_mapping (2), source = [1, 2])
       s_mapping_enable = .true.
       s_mapping_power = 2
    case ("pdf_builtin, none => none, pdf_builtin => epa, none => none, epa", &
          "pdf_builtin, none => none, pdf_builtin => ewa, none => none, ewa", &
          "pdf_builtin, none => none, pdf_builtin => ewa, none => none, epa", &
          "pdf_builtin, none => none, pdf_builtin => epa, none => none, ewa")
       allocate (s_mapping (2), source = [1, 2])
       s_mapping_enable = .true.
       s_mapping_power = 2
    case ("isr, none", &
         "none, isr")
       allocate (single_mapping (1), source = [1])
    case ("isr, none => none, isr")
       allocate (s_mapping (2), source = [1, 2])
       power_mapping = .true.
       power_mapping_eps = minval (sf_prop%isr_eps)
    case ("isr, none => none, isr => epa, none => none, epa", &
          "isr, none => none, isr => ewa, none => none, ewa", &
          "isr, none => none, isr => ewa, none => none, epa", &
          "isr, none => none, isr => epa, none => none, ewa")
       allocate (s_mapping (2), source = [1, 2])
       power_mapping = .true.
       power_mapping_eps = minval (sf_prop%isr_eps)
    case ("circe1 => isr, none => none, isr => epa, none => none, epa", &
          "circe1 => isr, none => none, isr => ewa, none => none, ewa", &
          "circe1 => isr, none => none, isr => ewa, none => none, epa", &
          "circe1 => isr, none => none, isr => epa, none => none, ewa")
       if (circe1_generate) then
          allocate (s_mapping (2), source = [2, 3])
       else
          allocate (s_mapping (3), source = [1, 2, 3])
          endpoint_mapping = .true.
          endpoint_mapping_slope = circe1_mapping_slope
       end if
       power_mapping = .true.
       power_mapping_eps = minval (sf_prop%isr_eps)       
    case ("pdf_builtin, none => none, isr", &
         "pdf_builtin_photon, none => none, isr", &
         "lhapdf, none => none, isr", &
         "lhapdf_photon, none => none, isr")
       allocate (single_mapping (1), source = [2])
    case ("isr, none => none, pdf_builtin", &
         "isr, none => none, pdf_builtin_photon", &
         "isr, none => none, lhapdf", &
         "isr, none => none, lhapdf_photon")
       allocate (single_mapping (1), source = [1])
    case ("epa, none", &
          "none, epa")
       allocate (single_mapping (1), source = [1])
    case ("epa, none => none, epa")
       allocate (single_mapping (2), source = [1, 2])
    case ("epa, none => none, isr", &
         "isr, none => none, epa", &
         "ewa, none => none, isr", &
         "isr, none => none, ewa")
       allocate (single_mapping (2), source = [1, 2])
    case ("pdf_builtin, none => none, epa", &
         "pdf_builtin_photon, none => none, epa", &
         "lhapdf, none => none, epa", &
         "lhapdf_photon, none => none, epa")
       allocate (single_mapping (1), source = [2])
    case ("pdf_builtin, none => none, ewa", &
         "pdf_builtin_photon, none => none, ewa", &
         "lhapdf, none => none, ewa", &
         "lhapdf_photon, none => none, ewa")
       allocate (single_mapping (1), source = [2])       
    case ("epa, none => none, pdf_builtin", &
         "epa, none => none, pdf_builtin_photon", &
         "epa, none => none, lhapdf", &
         "epa, none => none, lhapdf_photon")
       allocate (single_mapping (1), source = [1])
    case ("ewa, none => none, pdf_builtin", &
         "ewa, none => none, pdf_builtin_photon", &
         "ewa, none => none, lhapdf", &
         "ewa, none => none, lhapdf_photon")
       allocate (single_mapping (1), source = [1])       
    case ("ewa, none", &
          "none, ewa")
       allocate (single_mapping (1), source = [1])
    case ("ewa, none => none, ewa")
       allocate (single_mapping (2), source = [1, 2])
    case ("energy_scan, none => none, energy_scan")
       allocate (s_mapping (2), source = [1, 2])
    case ("sf_test_1, none => none, sf_test_1")
       allocate (s_mapping (2), source = [1, 2])
    case ("circe1")
       if (circe1_generate) then
          !!! no mapping
       else if (circe1_map) then
          allocate (s_mapping (1), source = [1])
          endpoint_mapping = .true.
          endpoint_mapping_slope = circe1_mapping_slope
       else
          allocate (s_mapping (1), source = [1])
          s_mapping_enable = .true.
       end if
    case ("circe1 => isr, none => none, isr")
       if (circe1_generate) then
          allocate (s_mapping (2), source = [2, 3])
       else
          allocate (s_mapping (3), source = [1, 2, 3])
          endpoint_mapping = .true.
          endpoint_mapping_slope = circe1_mapping_slope
       end if
       power_mapping = .true.
       power_mapping_eps = minval (sf_prop%isr_eps)
    case ("circe1 => isr, none", &
         "circe1 => none, isr")
       allocate (single_mapping (1), source = [2])
    case ("circe1 => epa, none => none, epa")
       if (circe1_generate) then
          allocate (single_mapping (2), source = [2, 3])
       else
          call msg_fatal ("Circe/EPA: supported with ?circe1_generate=true &
               &only")
       end if
    case ("circe1 => ewa, none => none, ewa")
       if (circe1_generate) then
          allocate (single_mapping (2), source = [2, 3])
       else 
          call msg_fatal ("Circe/EWA: supported with ?circe1_generate=true &
               &only")
       end if
    case ("circe1 => epa, none", &
         "circe1 => none, epa")
       if (circe1_generate) then
          allocate (single_mapping (1), source = [2])
       else
          call msg_fatal ("Circe/EPA: supported with ?circe1_generate=true &
               &only")
       end if
    case ("circe1 => epa, none => none, isr", &
         "circe1 => isr, none => none, epa", &
         "circe1 => ewa, none => none, isr", &
         "circe1 => isr, none => none, ewa")
       if (circe1_generate) then
          allocate (single_mapping (2), source = [2, 3])
       else
          call msg_fatal ("Circe/EPA: supported with ?circe1_generate=true &
               &only")
       end if
    case ("circe2", &
         "beam_events")
       !!! no mapping
    case ("circe2 => isr, none => none, isr", &
       "beam_events => isr, none => none, isr")
       allocate (s_mapping (2), source = [2, 3])
       power_mapping = .true.
       power_mapping_eps = minval (sf_prop%isr_eps)
    case ("circe2 => isr, none", &
         "circe2 => none, isr", &
         "beam_events => isr, none", &
         "beam_events => none, isr")
       allocate (single_mapping (1), source = [2])
    case ("circe2 => epa, none => none, epa", &
         "beam_events => epa, none => none, epa")
       allocate (single_mapping (2), source = [2, 3])
    case ("circe2 => epa, none", &
         "circe2 => none, epa", &
         "circe2 => ewa, none", &
         "circe2 => none, ewa", &
         "beam_events => epa, none", &
         "beam_events => none, epa", &
         "beam_events => ewa, none", &
         "beam_events => none, ewa")
       allocate (single_mapping (1), source = [2])
    case ("circe2 => epa, none => none, isr", &
         "circe2 => isr, none => none, epa", &
         "circe2 => ewa, none => none, isr", &
         "circe2 => isr, none => none, ewa", &
         "beam_events => epa, none => none, isr", &
         "beam_events => isr, none => none, epa", &
         "beam_events => ewa, none => none, isr", &
         "beam_events => isr, none => none, ewa")
       allocate (single_mapping (2), source = [2, 3])
    case ("energy_scan")
    case default
       call msg_fatal ("Beam structure: " &
            // char (sf_string) // " not supported")
    end select
    if (sf_allow_s_mapping .and. coll%n > 0) then
       n_sf_channel = coll%n
       allocate (sf_channel (n_sf_channel))
       do i = 1, n_sf_channel
          call sf_channel(i)%init (n_strfun)
          if (allocated (single_mapping)) then
             call sf_channel(i)%activate_mapping (single_mapping)
          end if
          if (allocated (prop))  deallocate (prop)
          call coll%get_entry (i, prop)
          if (allocated (prop)) then
             if (endpoint_mapping .and. power_mapping) then
                select type (prop)
                type is (resonance_t)
                   call sf_channel(i)%set_eir_mapping (s_mapping, &
                        a = endpoint_mapping_slope, eps = power_mapping_eps, &
                        m = prop%mass / sqrts, w = prop%width / sqrts)
                type is (on_shell_t)
                   call sf_channel(i)%set_eio_mapping (s_mapping, &
                        a = endpoint_mapping_slope, eps = power_mapping_eps, &
                        m = prop%mass / sqrts)
                end select
             else if (endpoint_mapping) then
                select type (prop)
                type is (resonance_t)
                   call sf_channel(i)%set_epr_mapping (s_mapping, &
                        a = endpoint_mapping_slope, &
                        m = prop%mass / sqrts, w = prop%width / sqrts)
                type is (on_shell_t)
                   call sf_channel(i)%set_epo_mapping (s_mapping, &
                        a = endpoint_mapping_slope, &
                        m = prop%mass / sqrts)
                end select
             else if (power_mapping) then
                select type (prop)
                type is (resonance_t)
                   call sf_channel(i)%set_ipr_mapping (s_mapping, &
                        eps = power_mapping_eps, &
                        m = prop%mass / sqrts, w = prop%width / sqrts)
                type is (on_shell_t)
                   call sf_channel(i)%set_ipo_mapping (s_mapping, &
                        eps = power_mapping_eps, &
                        m = prop%mass / sqrts)
                end select
             else if (allocated (s_mapping)) then
                select type (prop)
                type is (resonance_t)
                   call sf_channel(i)%set_res_mapping (s_mapping, &
                        m = prop%mass / sqrts, w = prop%width / sqrts)
                type is (on_shell_t)
                   call sf_channel(i)%set_os_mapping (s_mapping, &
                        m = prop%mass / sqrts)
                end select
             else if (allocated (single_mapping)) then
                select type (prop)
                type is (resonance_t)
                   call sf_channel(i)%set_res_mapping (single_mapping, &
                        m = prop%mass / sqrts, w = prop%width / sqrts)
                type is (on_shell_t)
                   call sf_channel(i)%set_os_mapping (single_mapping, &
                        m = prop%mass / sqrts)
                end select
             end if
          else if (endpoint_mapping .and. power_mapping) then
             call sf_channel(i)%set_ei_mapping (s_mapping, &
                  a = endpoint_mapping_slope, eps = power_mapping_eps)
          else if (endpoint_mapping .and. .not. allocated (single_mapping)) then
             call sf_channel(i)%set_ep_mapping (s_mapping, &
                  a = endpoint_mapping_slope)
          else if (power_mapping .and. .not. allocated (single_mapping)) then
             call sf_channel(i)%set_ip_mapping (s_mapping, &
                  eps = power_mapping_eps)
          else if (s_mapping_enable .and. .not. allocated (single_mapping)) then
             call sf_channel(i)%set_s_mapping (s_mapping, &
                  power = s_mapping_power)
          end if
       end do
    else if (sf_allow_s_mapping) then
       allocate (sf_channel (1))
       call sf_channel(1)%init (n_strfun)
       if (allocated (single_mapping)) then
          call sf_channel(1)%activate_mapping (single_mapping)
       else if (endpoint_mapping .and. power_mapping) then
          call sf_channel(i)%set_ei_mapping (s_mapping, &
               a = endpoint_mapping_slope, eps = power_mapping_eps)
       else if (endpoint_mapping) then
          call sf_channel(1)%set_ep_mapping (s_mapping, &
                  a = endpoint_mapping_slope)
       else if (power_mapping) then
          call sf_channel(1)%set_ip_mapping (s_mapping, &
                  eps = power_mapping_eps)
       else if (s_mapping_enable) then
          call sf_channel(1)%set_s_mapping (s_mapping, &
               power = s_mapping_power)
       end if
    else
       allocate (sf_channel (1))
       call sf_channel(1)%init (n_strfun)
       if (allocated (single_mapping)) then
          call sf_channel(1)%activate_mapping (single_mapping)
       end if
    end if
  end subroutine dispatch_sf_channels
    
  subroutine dispatch_eio (eio, method, global)
    class(eio_t), intent(inout), allocatable :: eio
    type(string_t), intent(in) :: method
    type(rt_data_t), intent(in) :: global
    logical :: check, keep_beams, recover_beams
    logical :: write_sqme_prc, write_sqme_ref, write_sqme_alt
    type(string_t) :: lhef_version, lhef_extension, raw_version
    type(string_t) :: extension_default, debug_extension, extension_hepmc, &
         extension_lha, extension_hepevt, extension_ascii_short, &
         extension_ascii_long, extension_athena, extension_mokka, &
         extension_stdhep, extension_stdhep_up, extension_raw, &
         extension_hepevt_verb, extension_lha_verb, extension_lcio
    integer :: checkpoint
    logical :: show_process, show_transforms, show_decay, verbose, pacify
    select case (char (method))
    case ("raw")
       allocate (eio_raw_t :: eio)
       select type (eio)
       type is (eio_raw_t)
          check = &
               global%var_list%get_lval (var_str ("?check_event_file"))
          raw_version = &
               global%var_list%get_sval (var_str ("$event_file_version"))
          extension_raw = &
               global%var_list%get_sval (var_str ("$extension_raw"))
          call eio%set_parameters (check, raw_version, extension_raw)
       end select
    case ("checkpoint")
       allocate (eio_checkpoints_t :: eio)
       select type (eio)
       type is (eio_checkpoints_t)
          checkpoint = &
               global%var_list%get_ival (var_str ("checkpoint"))
          pacify = &
               global%var_list%get_lval (var_str ("?pacify"))
          call eio%set_parameters (checkpoint, blank = pacify)
       end select
    case ("lhef")
       allocate (eio_lhef_t :: eio)
       select type (eio)
       type is (eio_lhef_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          recover_beams = &
               global%var_list%get_lval (var_str ("?recover_beams"))
          lhef_version = &
               global%var_list%get_sval (var_str ("$lhef_version"))
          lhef_extension = &
               global%var_list%get_sval (var_str ("$lhef_extension"))
          write_sqme_prc = &
               global%var_list%get_lval (var_str ("?lhef_write_sqme_prc"))
          write_sqme_ref = &
               global%var_list%get_lval (var_str ("?lhef_write_sqme_ref"))
          write_sqme_alt = &
               global%var_list%get_lval (var_str ("?lhef_write_sqme_alt"))
          call eio%set_parameters (keep_beams, recover_beams, &
               char (lhef_version), lhef_extension, &
               write_sqme_ref, write_sqme_prc, write_sqme_alt)
       end select
    case ("hepmc")
       allocate (eio_hepmc_t :: eio)
       select type (eio)
       type is (eio_hepmc_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          recover_beams = &
               global%var_list%get_lval (var_str ("?recover_beams"))
          extension_hepmc = &
               global%var_list%get_sval (var_str ("$extension_hepmc"))          
          call eio%set_parameters (keep_beams, recover_beams, extension_hepmc)
       end select
    case ("lcio")
       allocate (eio_lcio_t :: eio)
       select type (eio)
       type is (eio_lcio_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          recover_beams = &
               global%var_list%get_lval (var_str ("?recover_beams"))
          extension_lcio = &
               global%var_list%get_sval (var_str ("$extension_lcio"))
          call eio%set_parameters (keep_beams, recover_beams, extension_lcio)
       end select       
    case ("stdhep")
       allocate (eio_stdhep_hepevt_t :: eio)
       select type (eio)
       type is (eio_stdhep_hepevt_t)                   
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_stdhep = &
               global%var_list%get_sval (var_str ("$extension_stdhep"))
          call eio%set_parameters (keep_beams, extension_stdhep)          
       end select
    case ("stdhep_up")
       allocate (eio_stdhep_hepeup_t :: eio)
       select type (eio)
       type is (eio_stdhep_hepeup_t)          
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_stdhep_up = &
               global%var_list%get_sval (var_str ("$extension_stdhep_up")) 
          call eio%set_parameters (keep_beams, extension_stdhep_up)          
       end select       
    case ("ascii")   
       allocate (eio_ascii_ascii_t :: eio)
       select type (eio)
       type is (eio_ascii_ascii_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_default = &
               global%var_list%get_sval (var_str ("$extension_default"))
          call eio%set_parameters (keep_beams, extension_default)
       end select       
    case ("athena")   
       allocate (eio_ascii_athena_t :: eio)
       select type (eio)
       type is (eio_ascii_athena_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_athena = &
               global%var_list%get_sval (var_str ("$extension_athena"))
          call eio%set_parameters (keep_beams, extension_athena)
       end select              
    case ("debug")   
       allocate (eio_ascii_debug_t :: eio)
       select type (eio)
       type is (eio_ascii_debug_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          debug_extension = &
               global%var_list%get_sval (var_str ("$debug_extension"))          
          show_process = &
               global%var_list%get_lval (var_str ("?debug_process"))
          show_transforms = &
               global%var_list%get_lval (var_str ("?debug_transforms"))
          show_decay = &
               global%var_list%get_lval (var_str ("?debug_decay"))
          verbose = &
               global%var_list%get_lval (var_str ("?debug_verbose"))
          call eio%set_parameters (keep_beams, debug_extension, &
               show_process, show_transforms, show_decay, verbose)
       end select
    case ("hepevt")   
       allocate (eio_ascii_hepevt_t :: eio)
       select type (eio)
       type is (eio_ascii_hepevt_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_hepevt = &
               global%var_list%get_sval (var_str ("$extension_hepevt"))
          call eio%set_parameters (keep_beams, extension_hepevt)
       end select              
    case ("hepevt_verb")   
       allocate (eio_ascii_hepevt_verb_t :: eio)
       select type (eio)
       type is (eio_ascii_hepevt_verb_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_hepevt_verb = &
               global%var_list%get_sval (var_str ("$extension_hepevt_verb"))
          call eio%set_parameters (keep_beams, extension_hepevt_verb)
       end select                     
    case ("lha")   
       allocate (eio_ascii_lha_t :: eio)
       select type (eio)
       type is (eio_ascii_lha_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_lha = &
               global%var_list%get_sval (var_str ("$extension_lha"))
          call eio%set_parameters (keep_beams, extension_lha)
       end select                     
    case ("lha_verb")   
       allocate (eio_ascii_lha_verb_t :: eio)
       select type (eio)
       type is (eio_ascii_lha_verb_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_lha_verb = global%var_list%get_sval ( &
               var_str ("$extension_lha_verb"))          
          call eio%set_parameters (keep_beams, extension_lha_verb)
       end select                            
    case ("long")   
       allocate (eio_ascii_long_t :: eio)
       select type (eio)
       type is (eio_ascii_long_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_ascii_long = &
               global%var_list%get_sval (var_str ("$extension_ascii_long"))
          call eio%set_parameters (keep_beams, extension_ascii_long)
       end select              
    case ("mokka")   
       allocate (eio_ascii_mokka_t :: eio)
       select type (eio)
       type is (eio_ascii_mokka_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_mokka = &
               global%var_list%get_sval (var_str ("$extension_mokka"))          
          call eio%set_parameters (keep_beams, extension_mokka)
       end select                     
    case ("short")   
       allocate (eio_ascii_short_t :: eio)
       select type (eio)
       type is (eio_ascii_short_t)
          keep_beams = &
               global%var_list%get_lval (var_str ("?keep_beams"))
          extension_ascii_short = &
               global%var_list%get_sval (var_str ("$extension_ascii_short"))
          call eio%set_parameters (keep_beams, extension_ascii_short)
       end select                     
    case ("weight_stream")
       allocate (eio_weights_t :: eio)
       select type (eio)
       type is (eio_weights_t)
          pacify = &
               global%var_list%get_lval (var_str ("?pacify"))
          call eio%set_parameters (pacify = pacify)       
       end select
    case default
       call msg_fatal ("Event I/O method '" // char (method) &
            // "' not implemented")
    end select
    call eio%set_fallback_model (global%fallback_model)
  end subroutine dispatch_eio
  
  subroutine dispatch_qcd (qcd, global)
    type(qcd_t), intent(inout) :: qcd
    type(rt_data_t), intent(in), target :: global
    type(var_list_t), pointer :: var_list
    logical :: fixed, from_mz, from_pdf_builtin, from_lhapdf, from_lambda_qcd
    real(default) :: mz, alpha_val, lambda
    integer :: nf, order, lhapdf_member
    type(string_t) :: pdfset, lhapdf_dir, lhapdf_file
    var_list => global%get_var_list_ptr ()
    fixed = &
         var_list%get_lval (var_str ("?alpha_s_is_fixed"))
    from_mz = &
         var_list%get_lval (var_str ("?alpha_s_from_mz"))
    from_pdf_builtin = &
         var_list%get_lval (var_str ("?alpha_s_from_pdf_builtin"))
    from_lhapdf = &
         var_list%get_lval (var_str ("?alpha_s_from_lhapdf"))
    from_lambda_qcd = &
         var_list%get_lval (var_str ("?alpha_s_from_lambda_qcd"))
    pdfset = &
         var_list%get_sval (var_str ("$pdf_builtin_set"))    
    lambda = &
         var_list%get_rval (var_str ("lambda_qcd"))
    nf = &
         var_list%get_ival (var_str ("alpha_s_nf"))
    order = &
         var_list%get_ival (var_str ("alpha_s_order"))
    lhapdf_dir = &
         var_list%get_sval (var_str ("$lhapdf_dir"))
    lhapdf_file = &
         var_list%get_sval (var_str ("$lhapdf_file"))
    lhapdf_member = &
         var_list%get_ival (var_str ("lhapdf_member"))         
    var_list => global%get_var_list_ptr ()
    if (var_list%contains (var_str ("mZ"))) then
       mz = var_list%get_rval (var_str ("mZ"))
    else
       mz = MZ_REF
    end if
    if (var_list%contains (var_str ("alphas"))) then
       alpha_val = var_list%get_rval (var_str ("alphas"))
    else
       alpha_val = ALPHA_QCD_MZ_REF
    end if
    if (allocated (qcd%alpha))  deallocate (qcd%alpha)
    if (from_lhapdf .and. from_pdf_builtin) then
        call msg_fatal (" Mixing alphas evolution",  &
             [var_str (" from LHAPDF and builtin PDF is not permitted")])
    end if 
    select case (count ([from_mz, from_pdf_builtin, from_lhapdf, from_lambda_qcd]))
    case (0)
       if (fixed) then 
          allocate (alpha_qcd_fixed_t :: qcd%alpha)          
       else
          call msg_fatal ("QCD alpha: no calculation mode set")
       end if
    case (2:)
       call msg_fatal ("QCD alpha: calculation mode is ambiguous")
    case (1)
       if (fixed) then
          call msg_fatal ("QCD alpha: use '?alpha_s_is_fixed = false' for " // &
               "running alphas")          
       else if (from_mz) then
          allocate (alpha_qcd_from_scale_t :: qcd%alpha)
       else if (from_pdf_builtin) then
          allocate (alpha_qcd_pdf_builtin_t :: qcd%alpha)
       else if (from_lhapdf) then
          allocate (alpha_qcd_lhapdf_t :: qcd%alpha)
       else if (from_lambda_qcd) then
          allocate (alpha_qcd_from_lambda_t :: qcd%alpha)
       end if
       call msg_message ("QCD alpha: using a running strong coupling")
    end select
    select type (alpha => qcd%alpha)
    type is (alpha_qcd_fixed_t)
       alpha%val = alpha_val
    type is (alpha_qcd_from_scale_t)
       alpha%mu_ref = mz
       alpha%ref = alpha_val
       alpha%order = order
       alpha%nf = nf
    type is (alpha_qcd_from_lambda_t)
       alpha%lambda = lambda
       alpha%order = order
       alpha%nf = nf
    type is (alpha_qcd_pdf_builtin_t)
       call alpha%init (pdfset, &
            global%os_data%pdf_builtin_datapath)
    type is (alpha_qcd_lhapdf_t)
       call alpha%init (lhapdf_file, lhapdf_member, lhapdf_dir)
    end select
  end subroutine dispatch_qcd
  
  subroutine dispatch_shower (shower_settings, global)
    class(shower_settings_t), intent(inout) :: shower_settings
    type(rt_data_t), intent(in), target :: global
    type(var_list_t), pointer :: var_list
    var_list => global%get_var_list_ptr ()
    call shower_settings%init (var_list)
  end subroutine dispatch_shower

  subroutine dispatch_evt_decay (evt, global)
    class(evt_t), intent(out), pointer :: evt
    type(rt_data_t), intent(in) :: global
    logical :: allow_decays
    allow_decays = &
         global%var_list%get_lval (var_str ("?allow_decays"))
    if (allow_decays) then
       allocate (evt_decay_t :: evt)
       call msg_message ("Simulate: activating decays")
    else
       evt => null ()
    end if
  end subroutine dispatch_evt_decay

  subroutine dispatch_evt_shower (evt, global, process)
    class(evt_t), intent(out), pointer :: evt
    type(rt_data_t), intent(in), target :: global
    type(process_t), intent(in), optional, target :: process
    logical :: allow_shower
    type(var_list_t), pointer :: var_list
    type(string_t) :: lhapdf_file, lhapdf_dir
    integer :: lhapdf_member
    double precision :: xmin, xmax, q2min, q2max
    type(shower_settings_t) :: settings
    external :: GetXminM, GetXmaxM, GetQ2minM, GetQ2maxM    
    var_list => global%get_var_list_ptr ()    
    allow_shower = &
         var_list%get_lval (var_str ("?allow_shower"))
    lhapdf_dir = &
         var_list%get_sval (var_str ("$lhapdf_dir"))    
    lhapdf_file = &
         var_list%get_sval (var_str ("$lhapdf_file"))
    lhapdf_member = &
         var_list%get_ival (var_str ("lhapdf_member"))             
    if (allow_shower) then
       allocate (evt_shower_t :: evt)
       call msg_message ("Simulate: activating parton shower")
       call dispatch_shower (settings, global)
       if (settings%mlm_matching) &
            call msg_message ("Simulate: applying MLM matching")
       if (settings%ckkw_matching) &
            call msg_warning ("Simulate: CKKW(-L) matching not yet supported")
       if (settings%hadronization_active) &
            call msg_message ("Simulate: applying hadronization")
       select type (evt)
       type is (evt_shower_t)
          call evt%init (settings, global%fallback_model, global%os_data)
          if (LHAPDF6_AVAILABLE) then
             call lhapdf_initialize &
                  (1, lhapdf_dir, lhapdf_file, lhapdf_member, evt%pdf)
          end if
          if (present (process)) &
               call evt%setup_pdf (process, global%beam_structure)
          select case (evt%pdf_type)
          case (STRF_LHAPDF6)
             evt%xmin = evt%pdf%getxmin ()
             evt%xmax = evt%pdf%getxmax ()
             evt%qmin = sqrt(evt%pdf%getq2min ())
             evt%qmax = sqrt(evt%pdf%getq2max ())
          case (STRF_LHAPDF5)
             if (LHAPDF5_AVAILABLE) then
                call GetXminM (1, lhapdf_member, xmin)
                call GetXmaxM (1, lhapdf_member, xmax)
                call GetQ2minM (1, lhapdf_member, q2min)
                call GetQ2maxM (1, lhapdf_member, q2max)
                evt%xmin = xmin
                evt%xmax = xmax
                evt%qmin = sqrt(q2min)
                evt%qmax = sqrt(q2max)
             end if
          end select
       end select
    else
       evt => null ()
    end if
  end subroutine dispatch_evt_shower

  subroutine dispatch_slha (global, input, spectrum, decays)
    type(rt_data_t), intent(inout), target :: global
    logical, intent(out) :: input, spectrum, decays
    input = &
         global%var_list%get_lval (var_str ("?slha_read_input"))
    spectrum = &
         global%var_list%get_lval (var_str ("?slha_read_spectrum"))
    decays = &
         global%var_list%get_lval (var_str ("?slha_read_decays"))    
  end subroutine dispatch_slha


  subroutine dispatch_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (dispatch_1, "dispatch_1", &
         "process configuration method", &
         u, results)
    call test (dispatch_2, "dispatch_2", &
         "process core", &
         u, results)
    call test (dispatch_3, "dispatch_3", &
         "integration method", &
         u, results)
    call test (dispatch_4, "dispatch_4", &
         "phase-space configuration", &
         u, results)
    call test (dispatch_5, "dispatch_5", &
         "random-number generator", &
         u, results)
    call test (dispatch_6, "dispatch_6", &
         "configure phase space using file", &
         u, results)
    call test (dispatch_7, "dispatch_7", &
         "structure-function data", &
         u, results)
    call test (dispatch_8, "dispatch_8", &
         "beam structure", &
         u, results)
    call test (dispatch_9, "dispatch_9", &
         "event I/O", &
         u, results)
    call test (dispatch_10, "dispatch_10", &
         "process core update", &
         u, results)
    call test (dispatch_11, "dispatch_11", &
         "QCD coupling", &
         u, results)
    call test (dispatch_12, "dispatch_12", &
         "Shower settings", &
         u, results)
    call test (dispatch_13, "dispatch_13", &
         "event transforms", &
         u, results)
    call test (dispatch_14, "dispatch_14", &
         "SLHA interface", &
         u, results)
  end subroutine dispatch_test

  subroutine dispatch_1 (u)
    integer, intent(in) :: u
    type(string_t), dimension(2) :: prt_in, prt_out
    type(rt_data_t), target :: global
    class(prc_core_def_t), allocatable :: core_def
    
    write (u, "(A)")  "* Test output: dispatch_1"
    write (u, "(A)")  "*   Purpose: select process configuration method"
    write (u, "(A)")

    call global%global_init ()
    
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    prt_in = [var_str ("a"), var_str ("b")]
    prt_out = [var_str ("c"), var_str ("d")]

    write (u, "(A)")  "* Allocate core_def as prc_test_def"

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global)
    select type (core_def)
    type is (prc_test_def_t)
       call core_def%write (u)
    end select
    
    deallocate (core_def)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate core_def as omega_def"
    write (u, "(A)")

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global)
    select type (core_def)
    type is (omega_omega_def_t)
       call core_def%write (u)
    end select
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_1"
    
  end subroutine dispatch_1
  
  subroutine dispatch_2 (u)
    integer, intent(in) :: u
    type(string_t), dimension(2) :: prt_in, prt_out
    type(rt_data_t), target :: global
    class(prc_core_def_t), allocatable :: core_def
    class(prc_core_t), allocatable :: core
    
    write (u, "(A)")  "* Test output: dispatch_2"
    write (u, "(A)")  "*   Purpose: select process configuration method"
    write (u, "(A)")  "             and allocate process core"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()

    prt_in = [var_str ("a"), var_str ("b")]
    prt_out = [var_str ("c"), var_str ("d")]

    write (u, "(A)")  "* Allocate core as test_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global)
    call dispatch_core (core, core_def)
    select type (core)
    type is (test_t)
       call core%write (u)
    end select
    
    deallocate (core)
    deallocate (core_def)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate core as prc_omega_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global)

    call global%select_model (var_str ("Test"))

    call var_list_set_log (global%var_list, &
         var_str ("?helicity_selection_active"), &
         .true., is_known = .true.)
    call var_list_set_real (global%var_list, &
         var_str ("helicity_selection_threshold"), &
         1e9_default, is_known = .true.)
    call var_list_set_int (global%var_list, &
         var_str ("helicity_selection_cutoff"), &
         10, is_known = .true.)
    
    call dispatch_core (core, core_def, &
         global%model, global%get_helicity_selection ())
    call core_def%allocate_driver (core%driver, var_str (""))

    select type (core)
    type is (prc_omega_t)
       call core%write (u)
    end select
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_2"
    
  end subroutine dispatch_2
  
  subroutine dispatch_3 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    class(mci_t), allocatable :: mci
    type(string_t) :: process_id
    
    write (u, "(A)")  "* Test output: dispatch_3"
    write (u, "(A)")  "*   Purpose: select integration method"
    write (u, "(A)")

    call global%global_init ()
    process_id = "dispatch_3"

    write (u, "(A)")  "* Allocate MCI as midpoint_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$integration_method"), &
         var_str ("midpoint"), is_known = .true.)
    call dispatch_mci (mci, global, process_id)
    select type (mci)
    type is (mci_midpoint_t)
       call mci%write (u)
    end select

    call mci%final ()
    deallocate (mci)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate MCI as vamp_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$integration_method"), &
         var_str ("vamp"), is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("threshold_calls"), &
         1, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("min_calls_per_channel"), &
         2, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("min_calls_per_bin"), &
         3, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("min_bins"), &
         4, is_known = .true.)
    call var_list_set_int (global%var_list, var_str ("max_bins"), &
         5, is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?stratified"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?use_vamp_equivalences"),&
         .false., is_known = .true.)
    call var_list_set_real (global%var_list, var_str ("channel_weights_power"),&
         4._default, is_known = .true.)
    call var_list_set_log (global%var_list, &
         var_str ("?vamp_history_global_verbose"), &
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, &
         var_str ("?vamp_history_channels"), &
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, &
         var_str ("?vamp_history_channels_verbose"), &
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?stratified"), &
         .false., is_known = .true.)

    call dispatch_mci (mci, global, process_id)
    select type (mci)
    type is (mci_vamp_t)
       call mci%write (u)
       call mci%write_history_parameters (u)
    end select

    call mci%final ()
    deallocate (mci)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate MCI as vamp_t, allow for negative weights"
    write (u, "(A)")    
    
    call var_list_set_string (global%var_list, &
         var_str ("$integration_method"), &
         var_str ("vamp"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?negative_weights"), &
         .true., is_known = .true.)
    
    call dispatch_mci (mci, global, process_id)
    select type (mci)       
    type is (mci_vamp_t)
       call mci%write (u)
       call mci%write_history_parameters (u)
    end select
    
    call mci%final ()
    deallocate (mci)
    
    call global%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_3"
    
  end subroutine dispatch_3
  
  subroutine dispatch_4 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    class(phs_config_t), allocatable :: phs
    type(phs_parameters_t) :: phs_par
    type(mapping_defaults_t) :: mapping_defs
    
    write (u, "(A)")  "* Test output: dispatch_4"
    write (u, "(A)")  "*   Purpose: select phase-space configuration method"
    write (u, "(A)")

    call global%global_init ()

    write (u, "(A)")  "* Allocate PHS as phs_single_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call dispatch_phs (phs, global, var_str ("dispatch_4"))
    call phs%write (u)

    call phs%final ()
    deallocate (phs)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate PHS as phs_wood_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call dispatch_phs (phs, global, var_str ("dispatch_4"))
    call phs%write (u)
          
    call phs%final ()
    deallocate (phs)
    
    write (u, "(A)")
    write (u, "(A)")  "* Setting parameters for phs_wood_t"
    write (u, "(A)")        

    phs_par%m_threshold_s = 123
    phs_par%m_threshold_t = 456
    phs_par%t_channel = 42
    phs_par%off_shell = 17
    phs_par%keep_nonresonant = .false.    
    mapping_defs%energy_scale = 987
    mapping_defs%invariant_mass_scale = 654
    mapping_defs%momentum_transfer_scale = 321
    mapping_defs%step_mapping = .false.   
    mapping_defs%step_mapping_exp = .false.       
    mapping_defs%enable_s_mapping = .true.       
    call dispatch_phs (phs, global, var_str ("dispatch_4"), &
         mapping_defs, phs_par)    
    call phs%write (u)    
        
    call phs%final ()

    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_4"
    
  end subroutine dispatch_4
  
  subroutine dispatch_5 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    class(rng_factory_t), allocatable :: rng_factory
    
    write (u, "(A)")  "* Test output: dispatch_5"
    write (u, "(A)")  "*   Purpose: select random-number generator"
    write (u, "(A)")

    call global%global_init ()

    write (u, "(A)")  "* Allocate RNG factory as rng_test_factory_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$rng_method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list_set_int (global%var_list, &
         var_str ("seed"), 1, is_known = .true.)
    call dispatch_rng_factory (rng_factory, global)
    call rng_factory%write (u)
    deallocate (rng_factory)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate RNG factory as rng_tao_factory_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$rng_method"), &
         var_str ("tao"), is_known = .true.)
    call dispatch_rng_factory (rng_factory, global)
    call rng_factory%write (u)
    deallocate (rng_factory)
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_5"
    
  end subroutine dispatch_5
  
  subroutine dispatch_6 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(os_data_t) :: os_data
    type(process_constants_t) :: process_data
    class(phs_config_t), allocatable :: phs
    integer :: u_phs
    
    write (u, "(A)")  "* Test output: dispatch_6"
    write (u, "(A)")  "*   Purpose: select 'wood' phase-space &
         &for a test process"
    write (u, "(A)")  "*            and read phs configuration from file"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process"
    write (u, "(A)")

    call global%global_init ()

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call global%select_model (var_str ("Test"))

    call syntax_phs_forest_init ()
    
    call init_test_process_data (var_str ("dispatch_6"), process_data)

    write (u, "(A)")  "* Write phase-space file"

    u_phs = free_unit ()
    open (u_phs, file = "dispatch_6.phs", action = "write", status = "replace")
    call write_test_phs_file (u_phs, var_str ("dispatch_6"))
    close (u_phs)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate PHS as phs_wood_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, &
         var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call var_list_set_string (global%var_list, &
         var_str ("$phs_file"), &
         var_str ("dispatch_6.phs"), is_known = .true.)
    call dispatch_phs (phs, global, var_str ("dispatch_6"))

    call phs%init (process_data, global%model)
    call phs%configure (sqrts = 1000._default)

    call phs%write (u)
    write (u, "(A)")
    select type (phs)
    type is (phs_wood_config_t)
       call phs%write_forest (u)
    end select

    call phs%final ()

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_6"
    
  end subroutine dispatch_6
  
  subroutine dispatch_7 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(os_data_t) :: os_data
    type(string_t) :: prt, sf_method
    type(sf_prop_t) :: sf_prop
    class(sf_data_t), allocatable :: data
    type(pdg_array_t), dimension(1) :: pdg_in
    type(pdg_array_t), dimension(1,1) :: pdg_prc
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    
    write (u, "(A)")  "* Test output: dispatch_7"
    write (u, "(A)")  "*   Purpose: select and configure &
         &structure function data"
    write (u, "(A)")

    call global%global_init ()
    
    call os_data_init (os_data)
    call syntax_model_file_init ()
    call global%select_model (var_str ("QCD"))
    
    call reset_interaction_counter ()
    call var_list_set_real (global%var_list, var_str ("sqrts"), &
         14000._default, is_known = .true.)
    prt = "p"
    call global%beam_structure%init_sf ([prt, prt], [1])
    pdg_in = 2212
    
    write (u, "(A)")  "* Allocate data as sf_pdf_builtin_t"
    write (u, "(A)")

    sf_method = "pdf_builtin"
    call dispatch_sf_data &
         (data, sf_method, [1], sf_prop, global, pdg_in, pdg_prc)
    call data%write (u)

    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(A)")
    write (u, "(1x,A,99(1x,I0))")  "PDG(out) = ", pdg1

    deallocate (data)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate data for different PDF set"
    write (u, "(A)")

    pdg_in = 2212
    
    call var_list_set_string (global%var_list, var_str ("$pdf_builtin_set"), &
         var_str ("CTEQ6M"), is_known = .true.)
    sf_method = "pdf_builtin"
    call dispatch_sf_data &
         (data, sf_method, [1], sf_prop, global, pdg_in, pdg_prc)
    call data%write (u)

    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(A)")
    write (u, "(1x,A,99(1x,I0))")  "PDG(out) = ", pdg1

    deallocate (data)
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_7"
    
  end subroutine dispatch_7
  
  subroutine dispatch_8 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(os_data_t) :: os_data
    type(flavor_t), dimension(2) :: flv
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_prop_t) :: sf_prop
    type(sf_channel_t), dimension(:), allocatable :: sf_channel
    type(phs_channel_collection_t) :: coll
    type(string_t) :: sf_string
    integer :: i
    type(pdg_array_t), dimension (2,1) :: pdg_prc
    
    write (u, "(A)")  "* Test output: dispatch_8"
    write (u, "(A)")  "*   Purpose: configure a structure-function chain"
    write (u, "(A)")

    call global%global_init ()
    
    call os_data_init (os_data)
    call syntax_model_file_init ()
    call global%select_model (var_str ("QCD"))
    
    write (u, "(A)")  "* Allocate LHC beams with PDF builtin"
    write (u, "(A)")

    call flavor_init (flv(1), PROTON, global%model)
    call flavor_init (flv(2), PROTON, global%model)

    call reset_interaction_counter ()
    call var_list_set_real (global%var_list, var_str ("sqrts"), &
         14000._default, is_known = .true.)
         
    call global%beam_structure%init_sf (flavor_get_name (flv), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("pdf_builtin"))
    
    call dispatch_sf_config (sf_config, sf_prop, global, pdg_prc)
    do i = 1, size (sf_config)
       call sf_config(i)%write (u)
    end do

    call dispatch_sf_channels (sf_channel, sf_string, sf_prop, coll, global)
    write (u, "(1x,A)")  "Mapping configuration:"
    do i = 1, size (sf_channel)
       write (u, "(2x)", advance = "no")
       call sf_channel(i)%write (u)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Allocate ILC beams with CIRCE1"
    write (u, "(A)")

    call global%select_model (var_str ("QED"))
    call flavor_init (flv(1), ELECTRON, global%model)
    call flavor_init (flv(2),-ELECTRON, global%model)

    call reset_interaction_counter ()
    call var_list_set_real (global%var_list, var_str ("sqrts"), &
         500._default, is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?circe1_generate"), &
         .false., is_known = .true.)
         
    call global%beam_structure%init_sf (flavor_get_name (flv), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("circe1"))
    
    call dispatch_sf_config (sf_config, sf_prop, global, pdg_prc)
    do i = 1, size (sf_config)
       call sf_config(i)%write (u)
    end do

    call dispatch_sf_channels (sf_channel, sf_string, sf_prop, coll, global)
    write (u, "(1x,A)")  "Mapping configuration:"
    do i = 1, size (sf_channel)
       write (u, "(2x)", advance = "no")
       call sf_channel(i)%write (u)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_8"
    
  end subroutine dispatch_8
  
  subroutine dispatch_9 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    class(eio_t), allocatable :: eio
    
    write (u, "(A)")  "* Test output: dispatch_9"
    write (u, "(A)")  "*   Purpose: allocate an event I/O (eio) stream"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()
    call global%init_fallback_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"))
    
    write (u, "(A)")  "* Allocate as raw"
    write (u, "(A)")
    
    call dispatch_eio (eio, var_str ("raw"), global)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as checkpoints:"
    write (u, "(A)")
    
    call dispatch_eio (eio, var_str ("checkpoint"), global)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as LHEF:"
    write (u, "(A)")
    
    call var_list_set_string (global%var_list, var_str ("$lhef_extension"), &
         var_str ("lhe_custom"), is_known = .true.)
    call dispatch_eio (eio, var_str ("lhef"), global)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as HepMC:"
    write (u, "(A)")
    
    call dispatch_eio (eio, var_str ("hepmc"), global)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as weight_stream"
    write (u, "(A)")
    
    call dispatch_eio (eio, var_str ("weight_stream"), global)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as debug format"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, var_str ("?debug_verbose"), &
         .false., is_known = .true.)
    call dispatch_eio (eio, var_str ("debug"), global)

    call eio%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call eio%final ()
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_9"
    
  end subroutine dispatch_9
  
  subroutine dispatch_10 (u)
    integer, intent(in) :: u
    type(string_t), dimension(2) :: prt_in, prt_out
    type(rt_data_t), target :: global
    class(prc_core_def_t), allocatable :: core_def
    class(prc_core_t), allocatable :: core, saved_core
    type(var_list_t), pointer :: var_list
    
    write (u, "(A)")  "* Test output: dispatch_10"
    write (u, "(A)")  "*   Purpose: select process configuration method,"
    write (u, "(A)")  "             allocate process core,"
    write (u, "(A)")  "             temporarily reset parameters"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()

    prt_in = [var_str ("a"), var_str ("b")]
    prt_out = [var_str ("c"), var_str ("d")]

    write (u, "(A)")  "* Allocate core as prc_omega_t"
    write (u, "(A)")

    call var_list_set_string (global%var_list, var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global)

    call global%select_model (var_str ("Test"))

    call dispatch_core (core, core_def, global%model)
    call core_def%allocate_driver (core%driver, var_str (""))

    select type (core)
    type is (prc_omega_t)
       call core%write (u)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Update core with modified model and helicity selection"
    write (u, "(A)")

    var_list => global%get_var_list_ptr ()
    call var_list_set_real (var_list, var_str ("gy"), 2._default, &
         is_known = .true.)
    call global%model%update_parameters ()

    call var_list_set_log (global%var_list, &
         var_str ("?helicity_selection_active"), &
         .true., is_known = .true.)
    call var_list_set_real (global%var_list, &
         var_str ("helicity_selection_threshold"), &
         2e10_default, is_known = .true.)
    call var_list_set_int (global%var_list, &
         var_str ("helicity_selection_cutoff"), &
         5, is_known = .true.)
    
    call dispatch_core_update (core, global%model, &
         global%get_helicity_selection (), &
         saved_core = saved_core)
    select type (core)
    type is (prc_omega_t)
       call core%write (u)
    end select
    
    write (u, "(A)")
    write (u, "(A)")  "* Restore core from save"
    write (u, "(A)")

    call dispatch_core_restore (core, saved_core)
    select type (core)
    type is (prc_omega_t)
       call core%write (u)
    end select
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_10"
    
  end subroutine dispatch_10
  
  subroutine dispatch_11 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(qcd_t) :: qcd
    type(var_list_t), pointer :: model_vars
    
    write (u, "(A)")  "* Test output: dispatch_11"
    write (u, "(A)")  "*   Purpose: select QCD coupling formula"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()
    call global%select_model (var_str ("SM"))
    model_vars => global%get_var_list_ptr ()

    write (u, "(A)")  "* Allocate alpha_s as fixed"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?alpha_s_is_fixed"), &
         .true., is_known = .true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (built-in)"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, var_str ("?alpha_s_is_fixed"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?alpha_s_from_mz"), &
         .true., is_known = .true.)
    call var_list_set_int &
         (global%var_list, var_str ("alpha_s_order"), 1, is_known = .true.)
    call var_list_set_real &
         (model_vars, var_str ("alphas"), 0.1234_default, &
          is_known=.true.)
    call var_list_set_real &
         (model_vars, var_str ("mZ"), 91.234_default, &
          is_known=.true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (built-in, Lambda defined)"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, var_str ("?alpha_s_from_mz"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, &
         var_str ("?alpha_s_from_lambda_qcd"), &
         .true., is_known = .true.)
    call var_list_set_real &
         (global%var_list, var_str ("lambda_qcd"), 250.e-3_default, &
          is_known=.true.)
    call var_list_set_int &
         (global%var_list, var_str ("alpha_s_order"), 2, is_known = .true.)
    call var_list_set_int &
         (global%var_list, var_str ("alpha_s_nf"), 4, is_known = .true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (using builtin PDF set)"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, &
         var_str ("?alpha_s_from_lambda_qcd"), &
         .false., is_known = .true.)
    call var_list_set_log &
         (global%var_list, var_str ("?alpha_s_from_pdf_builtin"), &
         .true., is_known = .true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_11"
    
  end subroutine dispatch_11
  
  subroutine dispatch_12 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(shower_settings_t) :: shower_settings
    
    write (u, "(A)")  "* Test output: dispatch_12"
    write (u, "(A)")  "*   Purpose: setting ISR/FSR shower"
    write (u, "(A)")

    write (u, "(A)")  "* Default settings"    
    write (u, "(A)")    
    
    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?alpha_s_is_fixed"), &
         .true., is_known = .true.)
    call dispatch_shower (shower_settings, global)
    call write_separator (u)
    call shower_settings%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Switch on ISR/FSR showers, hadronization"
    write (u, "(A)")  "      and MLM matching"
    write (u, "(A)")
    
    call var_list_set_log (global%var_list, var_str ("?ps_fsr_active"), &
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?ps_isr_active"), &
         .true., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?hadronization_active"), &
         .true., is_known = .true.)    
    call var_list_set_log (global%var_list, var_str ("?mlm_matching"), &
         .true., is_known = .true.)        
    call var_list_set_int &
         (global%var_list, var_str ("ps_max_n_flavors"), 4, is_known = .true.)
    call var_list_set_real &
         (global%var_list, var_str ("ps_isr_z_cutoff"), 0.1234_default, &
          is_known=.true.)
    call var_list_set_real (global%var_list, &
         var_str ("mlm_etamax"), 3.456_default, is_known=.true.)
    call var_list_set_string (global%var_list, &
         var_str ("$ps_PYTHIA_PYGIVE"), var_str ("abcdefgh"), is_known=.true.)    
    call dispatch_shower (shower_settings, global)
    call write_separator (u)
    call shower_settings%write (u)
    call write_separator (u)
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_12"
    
  end subroutine dispatch_12
  
  subroutine dispatch_13 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    class(evt_t), pointer :: evt
    type(var_list_t), pointer :: model_vars
    
    write (u, "(A)")  "* Test output: dispatch_13"
    write (u, "(A)")  "*   Purpose: configure event transform"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()
    call global%init_fallback_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"))
    model_vars => global%get_var_list_ptr ()    

    write (u, "(A)")  "* Partonic decays"
    write (u, "(A)")

    call dispatch_evt_decay (evt, global)
    select type (evt)
    type is (evt_decay_t)
       call evt%write (u, show_decay_tree = .true., verbose = .true.)
    end select

    call evt%final ()
    deallocate (evt)

    write (u, "(A)")
    write (u, "(A)")  "* Shower"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?allow_shower"), .true., &
         is_known = .true.)
    call dispatch_evt_shower (evt, global)
    select type (evt)
    type is (evt_shower_t)
       call evt%write (u)
       call write_separator (u, 2)
    end select

    call evt%final ()
    deallocate (evt)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_13"
    
  end subroutine dispatch_13
  
  subroutine dispatch_14 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    logical :: input, spectrum, decays
    
    write (u, "(A)")  "* Test output: dispatch_14"
    write (u, "(A)")  "*   Purpose: SLHA interface settings"
    write (u, "(A)")

    write (u, "(A)")  "* Default settings"    
    write (u, "(A)")    
    
    call global%global_init ()
    call dispatch_slha (global, &
         input = input, spectrum = spectrum, decays = decays)

    write (u, "(A,1x,L1)")  " slha_read_input     =", input
    write (u, "(A,1x,L1)")  " slha_read_spectrum  =", spectrum   
    write (u, "(A,1x,L1)")  " slha_read_decays    =", decays

    call global%final ()
    call global%global_init ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Set all entries to [false]"    
    write (u, "(A)")        
            
    call var_list_set_log (global%var_list, var_str ("?slha_read_input"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?slha_read_spectrum"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?slha_read_decays"), &
         .false., is_known = .true.)    

    call dispatch_slha (global, &
         input = input, spectrum = spectrum, decays = decays)

    write (u, "(A,1x,L1)")  " slha_read_input     =", input
    write (u, "(A,1x,L1)")  " slha_read_spectrum  =", spectrum   
    write (u, "(A,1x,L1)")  " slha_read_decays    =", decays
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_14"
    
  end subroutine dispatch_14
  

end module dispatch
