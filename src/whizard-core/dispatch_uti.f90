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

module dispatch_uti
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use io_units
  use diagnostics
  use os_interface
  use physics_defs
  use sm_qcd
  use flavors
  use interactions, only: reset_interaction_counter
  use pdg_arrays
  use process_constants
  use prc_core_def
  use prc_core
  use prc_test
  use prc_omega
  use rng_base
  use sf_mappings
  use sf_base
  use mappings
  use phs_forests
  use phs_base
  use phs_wood
  use mci_base
  use mci_midpoint
  use mci_vamp
  use processes, only: test_t
  use variables
  use models
  use eio_base
  use event_transforms
  use shower_base
  use rt_data

  use dispatch
  
  use sf_base_ut, only: sf_test_data_t
    
  implicit none
  private

  public :: dispatch_rng_factory_test
  public :: dispatch_sf_data_test

  public :: dispatch_1
  public :: dispatch_2
  public :: dispatch_3
  public :: dispatch_4
  public :: dispatch_5
  public :: dispatch_6
  public :: dispatch_7
  public :: dispatch_8
  public :: dispatch_9
  public :: dispatch_10
  public :: dispatch_11
  public :: dispatch_12
  public :: dispatch_13
  public :: dispatch_14

contains

  subroutine dispatch_1 (u)
    integer, intent(in) :: u
    type(string_t), dimension(2) :: prt_in, prt_out
    type(rt_data_t), target :: global
    class(prc_core_def_t), allocatable :: core_def
    
    write (u, "(A)")  "* Test output: dispatch_1"
    write (u, "(A)")  "*   Purpose: select process configuration method"
    write (u, "(A)")

    call global%global_init ()
    
    call global%set_log (var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    prt_in = [var_str ("a"), var_str ("b")]
    prt_out = [var_str ("c"), var_str ("d")]

    write (u, "(A)")  "* Allocate core_def as prc_test_def"

    call global%set_string (var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global%model, global%var_list)
    select type (core_def)
    type is (prc_test_def_t)
       call core_def%write (u)
    end select
    
    deallocate (core_def)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate core_def as omega_def"
    write (u, "(A)")

    call global%set_string (var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global%model, global%var_list)
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

    call global%set_string (var_str ("$method"), &
         var_str ("unit_test"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global%model, global%var_list)
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

    call global%set_string (var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global%model, global%var_list)

    call global%select_model (var_str ("Test"))

    call global%set_log (&
         var_str ("?helicity_selection_active"), &
         .true., is_known = .true.)
    call global%set_real (&
         var_str ("helicity_selection_threshold"), &
         1e9_default, is_known = .true.)
    call global%set_int (&
         var_str ("helicity_selection_cutoff"), &
         10, is_known = .true.)
    
    call dispatch_core (core, core_def, &
         global%model, &
         global%get_helicity_selection ())
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

    call global%set_string (&
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

    call global%set_string (&
         var_str ("$integration_method"), &
         var_str ("vamp"), is_known = .true.)
    call global%set_int (var_str ("threshold_calls"), &
         1, is_known = .true.)
    call global%set_int (var_str ("min_calls_per_channel"), &
         2, is_known = .true.)
    call global%set_int (var_str ("min_calls_per_bin"), &
         3, is_known = .true.)
    call global%set_int (var_str ("min_bins"), &
         4, is_known = .true.)
    call global%set_int (var_str ("max_bins"), &
         5, is_known = .true.)
    call global%set_log (var_str ("?stratified"), &
         .false., is_known = .true.)
    call global%set_log (var_str ("?use_vamp_equivalences"),&
         .false., is_known = .true.)
    call global%set_real (var_str ("channel_weights_power"),&
         4._default, is_known = .true.)
    call global%set_log (&
         var_str ("?vamp_history_global_verbose"), &
         .true., is_known = .true.)
    call global%set_log (&
         var_str ("?vamp_history_channels"), &
         .true., is_known = .true.)
    call global%set_log (&
         var_str ("?vamp_history_channels_verbose"), &
         .true., is_known = .true.)
    call global%set_log (var_str ("?stratified"), &
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
    
    call global%set_string (&
         var_str ("$integration_method"), &
         var_str ("vamp"), is_known = .true.)
    call global%set_log (var_str ("?negative_weights"), &
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

    call global%set_string (&
         var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)
    call dispatch_phs (phs, global, var_str ("dispatch_4"))
    call phs%write (u)

    call phs%final ()
    deallocate (phs)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate PHS as phs_wood_t"
    write (u, "(A)")

    call global%set_string (&
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

    call global%set_string (&
         var_str ("$rng_method"), &
         var_str ("unit_test"), is_known = .true.)
    call global%set_int (&
         var_str ("seed"), 1, is_known = .true.)
    call dispatch_rng_factory (rng_factory, global)
    call rng_factory%write (u)
    deallocate (rng_factory)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate RNG factory as rng_tao_factory_t"
    write (u, "(A)")

    call global%set_string (&
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
    use phs_base_ut, only: init_test_process_data
    use phs_wood_ut, only: write_test_phs_file
    use phs_forests
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

    call global%set_string (&
         var_str ("$phs_method"), &
         var_str ("wood"), is_known = .true.)
    call global%set_string (&
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
    call global%set_real (var_str ("sqrts"), &
         14000._default, is_known = .true.)
    prt = "p"
    call global%beam_structure%init_sf ([prt, prt], [1])
    pdg_in = 2212
    
    write (u, "(A)")  "* Allocate data as sf_pdf_builtin_t"
    write (u, "(A)")

    sf_method = "pdf_builtin"
    call dispatch_sf_data &
         (data, sf_method, [1], sf_prop, global, pdg_in, pdg_prc, .false.)
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
    
    call global%set_string (var_str ("$pdf_builtin_set"), &
         var_str ("CTEQ6M"), is_known = .true.)
    sf_method = "pdf_builtin"
    call dispatch_sf_data &
         (data, sf_method, [1], sf_prop, global, pdg_in, pdg_prc, .false.)
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

    call flv(1)%init (PROTON, global%model)
    call flv(2)%init (PROTON, global%model)

    call reset_interaction_counter ()
    call global%set_real (var_str ("sqrts"), &
         14000._default, is_known = .true.)
         
    call global%beam_structure%init_sf (flv%get_name (), [1])
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
    call flv(1)%init ( ELECTRON, global%model)
    call flv(2)%init (-ELECTRON, global%model)

    call reset_interaction_counter ()
    call global%set_real (var_str ("sqrts"), &
         500._default, is_known = .true.)
    call global%set_log (var_str ("?circe1_generate"), &
         .false., is_known = .true.)
         
    call global%beam_structure%init_sf (flv%get_name (), [1])
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
    
    call global%set_string (var_str ("$lhef_extension"), &
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
    
    call global%set_log (var_str ("?debug_verbose"), &
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
    type(var_list_t), pointer :: model_vars
    
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

    call global%set_string (var_str ("$method"), &
         var_str ("omega"), is_known = .true.)
    call dispatch_core_def (core_def, prt_in, prt_out, global%model, global%var_list)

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

    model_vars => global%model%get_var_list_ptr ()
    
    call model_vars%set_real (var_str ("gy"), 2._default, &
         is_known = .true.)
    call global%model%update_parameters ()

    call global%set_log (&
         var_str ("?helicity_selection_active"), &
         .true., is_known = .true.)
    call global%set_real (&
         var_str ("helicity_selection_threshold"), &
         2e10_default, is_known = .true.)
    call global%set_int (&
         var_str ("helicity_selection_cutoff"), &
         5, is_known = .true.)
    
    call dispatch_core_update (core, &
         global%model, &
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

    call global%set_log (var_str ("?alpha_s_is_fixed"), &
         .true., is_known = .true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (built-in)"
    write (u, "(A)")
    
    call global%set_log (var_str ("?alpha_s_is_fixed"), &
         .false., is_known = .true.)
    call global%set_log (var_str ("?alpha_s_from_mz"), &
         .true., is_known = .true.)
    call global%set_int &
         (var_str ("alpha_s_order"), 1, is_known = .true.)
    call model_vars%set_real (var_str ("alphas"), 0.1234_default, &
          is_known=.true.)
    call model_vars%set_real (var_str ("mZ"), 91.234_default, &
          is_known=.true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (built-in, Lambda defined)"
    write (u, "(A)")
    
    call global%set_log (var_str ("?alpha_s_from_mz"), &
         .false., is_known = .true.)
    call global%set_log (&
         var_str ("?alpha_s_from_lambda_qcd"), &
         .true., is_known = .true.)
    call global%set_real &
         (var_str ("lambda_qcd"), 250.e-3_default, &
          is_known=.true.)
    call global%set_int &
         (var_str ("alpha_s_order"), 2, is_known = .true.)
    call global%set_int &
         (var_str ("alpha_s_nf"), 4, is_known = .true.)
    call dispatch_qcd (qcd, global)
    call qcd%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Allocate alpha_s as running (using builtin PDF set)"
    write (u, "(A)")
    
    call global%set_log (&
         var_str ("?alpha_s_from_lambda_qcd"), &
         .false., is_known = .true.)
    call global%set_log &
         (var_str ("?alpha_s_from_pdf_builtin"), &
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
    type(var_list_t), pointer :: var_list
    type(shower_settings_t) :: shower_settings
    
    write (u, "(A)")  "* Test output: dispatch_12"
    write (u, "(A)")  "*   Purpose: setting ISR/FSR shower"
    write (u, "(A)")

    write (u, "(A)")  "* Default settings"    
    write (u, "(A)")    
    
    call global%global_init ()
    call global%set_log (var_str ("?alpha_s_is_fixed"), &
         .true., is_known = .true.)
    var_list => global%get_var_list_ptr ()
    call shower_settings%init (var_list)
    call write_separator (u)
    call shower_settings%write (u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Switch on ISR/FSR showers, hadronization"
    write (u, "(A)")  "      and MLM matching"
    write (u, "(A)")
    
    call global%set_string (var_str ("$shower_method"), &
         var_str ("PYTHIA6"), is_known = .true.)
    call global%set_log (var_str ("?ps_fsr_active"), &
         .true., is_known = .true.)
    call global%set_log (var_str ("?ps_isr_active"), &
         .true., is_known = .true.)
    call global%set_log (var_str ("?hadronization_active"), &
         .true., is_known = .true.)    
    call global%set_log (var_str ("?mlm_matching"), &
         .true., is_known = .true.)        
    call global%set_int &
         (var_str ("ps_max_n_flavors"), 4, is_known = .true.)
    call global%set_real &
         (var_str ("ps_isr_z_cutoff"), 0.1234_default, &
          is_known=.true.)
    call global%set_real (&
         var_str ("mlm_etamax"), 3.456_default, is_known=.true.)
    call global%set_string (&
         var_str ("$ps_PYTHIA_PYGIVE"), var_str ("abcdefgh"), is_known=.true.)    
    call shower_settings%init (var_list)
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
    
    write (u, "(A)")  "* Test output: dispatch_13"
    write (u, "(A)")  "*   Purpose: configure event transform"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()
    call global%init_fallback_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"))

    write (u, "(A)")  "* Partonic decays"
    write (u, "(A)")

    call dispatch_evt_decay (evt, global)
    call evt%write (u, verbose = .true., more_verbose = .true.)

    call evt%final ()
    deallocate (evt)

    write (u, "(A)")
    write (u, "(A)")  "* Shower"
    write (u, "(A)")

    call global%set_log (var_str ("?allow_shower"), .true., &
         is_known = .true.)
    call global%set_string (var_str ("$shower_method"), &
         var_str ("WHIZARD"), is_known = .true.)
    call dispatch_evt_shower (evt, global)
    call evt%write (u)
    call write_separator (u, 2)

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
            
    call global%set_log (var_str ("?slha_read_input"), &
         .false., is_known = .true.)
    call global%set_log (var_str ("?slha_read_spectrum"), &
         .false., is_known = .true.)
    call global%set_log (var_str ("?slha_read_decays"), &
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
  

  subroutine dispatch_rng_factory_test (rng_factory, global, local_input)
    use rng_base
    use rng_base_ut, only: rng_test_factory_t
    class(rng_factory_t), allocatable, intent(inout) :: rng_factory
    type(rt_data_t), intent(inout), target :: global
    type(rt_data_t), intent(in), target, optional :: local_input
    type(rt_data_t), pointer :: local
    type(string_t) :: rng_method
    if (present (local_input)) then
       local => local_input
    else
       local => global
    end if
    rng_method = &
         local%var_list%get_sval (var_str ("$rng_method"))
    select case (char (rng_method))
    case ("unit_test")
       allocate (rng_test_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing Test random-number generator")
    end select
  end subroutine dispatch_rng_factory_test
    
  subroutine dispatch_sf_data_test (data, sf_method, i_beam, sf_prop, global, &
       pdg_in, pdg_prc, polarized)
    class(sf_data_t), allocatable, intent(inout) :: data
    type(string_t), intent(in) :: sf_method
    integer, dimension(:), intent(in) :: i_beam
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_in
    type(pdg_array_t), dimension(:,:), intent(in) :: pdg_prc
    type(sf_prop_t), intent(inout) :: sf_prop
    type(rt_data_t), intent(inout) :: global
    logical, intent(in) :: polarized
    select case (char (sf_method))
    case ("sf_test_0", "sf_test_1")
       allocate (sf_test_data_t :: data)
       select type (data)
       type is (sf_test_data_t)
          select case (char (sf_method))
          case ("sf_test_0");  call data%init (global%model, pdg_in(i_beam(1)))
          case ("sf_test_1");  call data%init (global%model, pdg_in(i_beam(1)),&
               mode = 1)
          end select
       end select
    end select
  end subroutine dispatch_sf_data_test
    

end module dispatch_uti
  
